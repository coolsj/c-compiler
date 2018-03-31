#include <algorithm>
#include "CodeGen.h"

vector<CodeGenBlock*> CodeGenContext::block_stack;
map<string,LLVMValueRef> CodeGenContext::functions_;
LLVMBasicBlockRef CodeGenContext::curr_loop_cond_block_;
LLVMBasicBlockRef CodeGenContext::curr_loop_post_block_;

struct MatchPathSeparator
{
    bool operator()( char ch ) const
    {
        return ch == '\\' || ch == '/';
    }
};

string GetModuleName( string const& pathname )
{
    string filename = string( std::find_if( pathname.rbegin(), pathname.rend(), MatchPathSeparator() ).base(), pathname.end() );
	auto pivot = std::find( filename.rbegin(), filename.rend(), '.' );
    string filebasename = (pivot == filename.rend()) ? filename : string( filename.begin(), pivot.base() - 1 );
	return "sj_"+filebasename;
}

LLVMTypeRef GetLLVMType( value_type_t type )
{
	switch(type)
	{
		case value_type_t::int_type: return LLVMInt32Type();
		case value_type_t::void_type: return LLVMVoidType();
	}
}

void CreatePutsDeclaration(LLVMModuleRef module)
{
	if( CodeGenContext::GetFunctionValueRef("puts") != nullptr )
		return;
	LLVMTypeRef params[1];
	params[0] = LLVMPointerType( LLVMInt8Type(), 0 );
	LLVMTypeRef func_type = LLVMFunctionType( LLVMInt32Type(), params, 1, 0 );
	LLVMValueRef func = LLVMAddFunction(module, "puts", func_type);
	CodeGenContext::AddFunctionToContext("puts", func);
}

void CreatePrintfDeclaration(LLVMModuleRef module, size_t num_args)
{
	string printf_name = "printf"+to_string(num_args);
	if( CodeGenContext::GetFunctionValueRef(printf_name) != nullptr )
		return;
	LLVMTypeRef params[num_args];
	params[0] = LLVMPointerType( LLVMInt8Type(), 0 );
	size_t i=1;
	while(i < num_args)
		params[i++] = LLVMInt32Type();
	LLVMTypeRef func_type = LLVMFunctionType( LLVMInt32Type(), params, num_args, 0 );
	LLVMValueRef func = LLVMAddFunction(module, "printf", func_type);
	CodeGenContext::AddFunctionToContext(printf_name, func);
}

LLVMValueRef CodeGen( ASTProgramRoot* p_root )
{
	if( p_root->GetCountOfFiles() == 0 )
	{
		cerr << "no files found" << endl;
		return nullptr;
	}
	else if( p_root->GetCountOfFiles() > 1 )
	{
		cerr << "multiple files not supported" << endl;
		return nullptr;
	}
	ASTFile *p_file = *(p_root->BeginFile());
	string module_name = GetModuleName(p_file->GetFileName());

	LLVMModuleRef module = LLVMModuleCreateWithName(module_name.c_str());
    LLVMBuilderRef builder = LLVMCreateBuilder();
	LLVMValueRef ret_val = CodeGen( p_file, module, builder );

	string op_file_name = module_name+".bc";
	if( LLVMWriteBitcodeToFile(module, op_file_name.c_str()) != 0 )
	{
		cerr << "error in writing bitcode" << endl;
		return nullptr;
	}
	return ret_val;
}

LLVMValueRef CodeGen( ASTFile* p_file, LLVMModuleRef module, LLVMBuilderRef builder )
{
	LLVMValueRef file_val = nullptr;
	for(auto iter = p_file->BeginDeclaration(); 
			iter != p_file->EndDeclaration(); 
			++iter)
	{
		ASTDeclarationStatement *p_decl = *iter;
		if( p_decl->GetNodeType() == node_type_t::var_decl_stmt_nd )
		{
			cerr << "global variables not supported" << endl;
			return nullptr;
		}
		file_val = CodeGen( dynamic_cast<ASTFuncDeclarationStatement*>(p_decl), module, builder);
	}
	return file_val;
}

LLVMValueRef CodeGen( ASTStatement* p_stmt, LLVMModuleRef module, LLVMBuilderRef builder )
{
	switch( p_stmt->GetNodeType() )
	{
		case node_type_t::var_decl_stmt_nd:
			return CodeGen( dynamic_cast<ASTVarDeclarationStatement*>(p_stmt), module, builder );
		case node_type_t::func_decl_stmt_nd:
			return CodeGen( dynamic_cast<ASTFuncDeclarationStatement*>(p_stmt), module, builder );
		case node_type_t::compound_stmt_nd:
			return CodeGen( dynamic_cast<ASTCompoundStatement*>(p_stmt), module, builder );
		case node_type_t::jump_stmt_nd:
			return CodeGen( dynamic_cast<ASTJumpStatement*>(p_stmt), module, builder );
		case node_type_t::select_stmt_nd:
			return CodeGen( dynamic_cast<ASTSelectionStatement*>(p_stmt), module, builder );
		case node_type_t::expr_stmt_nd:
			return CodeGen( dynamic_cast<ASTExpressionStatement*>(p_stmt), module, builder );
		case node_type_t::iter_stmt_nd:
			return CodeGen( dynamic_cast<ASTIterationStatement*>(p_stmt), module, builder );
	}
}

LLVMValueRef CodeGen( ASTVarDeclarationStatement* p_var_decl_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	LLVMValueRef alloc_val = LLVMBuildAlloca(builder, GetLLVMType( p_var_decl_stmt->GetVarType() ), p_var_decl_stmt->GetVarName().c_str());

	ASTExpression *p_init_expr = p_var_decl_stmt->GetVarInit();
	if( p_init_expr )
	{
		LLVMValueRef init_ref_val = CodeGen(p_init_expr, module, builder);
		LLVMBuildStore(builder, init_ref_val, alloc_val);
	}
	//LLVMValueRef var_ref_val = LLVMBuildLoad(builder, alloc_val, "");
	CodeGenBlock *p_code_gen_block = CodeGenContext::GetCurrentBlock();
	p_code_gen_block->AddVar(p_var_decl_stmt->GetVarName().c_str(), alloc_val);
}

LLVMValueRef CodeGen( ASTFuncDeclarationStatement* p_func_decl_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	ASTFunctionFormalArguments *p_args = p_func_decl_stmt->GetFormalArguments();
	LLVMTypeRef func_type;
	if(p_args)
	{
		auto start_arg = p_args->BeginArgument();
		auto end_arg = p_args->EndArgument();
		size_t num_args = p_args->GetCountOfArguments();

		size_t i = 0;
		auto iter = start_arg;

		// Create Argument List
		LLVMTypeRef params[num_args];
		for(; i<num_args; ++i, ++iter)
		{
			params[i] = GetLLVMType( (*iter)->GetVarType() );
		}
		// Create function type
		func_type = LLVMFunctionType( GetLLVMType( p_func_decl_stmt->GetReturnType() ), params, num_args, 0 );
	}
	else
	{
		func_type = LLVMFunctionType( GetLLVMType( p_func_decl_stmt->GetReturnType() ), nullptr, 0, 0 );
	}
	// Create function
	LLVMValueRef func = LLVMAddFunction(module, p_func_decl_stmt->GetFuncName().c_str(), func_type);
	CodeGenContext::AddFunctionToContext(p_func_decl_stmt->GetFuncName().c_str(), func);
	LLVMSetLinkage(func, LLVMExternalLinkage);
	// Create basic block.
    LLVMBasicBlockRef block = LLVMAppendBasicBlock(func, "entry");
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(block) );
    LLVMPositionBuilderAtEnd(builder, block);
	if(p_args)
	{
		size_t i = 0;
		auto start = p_args->BeginArgument();
		auto end = p_args->EndArgument();
		for(auto iter = start; iter != end; ++iter, ++i)
		{
			LLVMValueRef alloc_val = LLVMBuildAlloca(builder, GetLLVMType( (*iter)->GetVarType() ), "");
			LLVMBuildStore(builder, LLVMGetParam(func, i), alloc_val);
			CodeGenBlock *p_code_gen_block = CodeGenContext::GetCurrentBlock();
			p_code_gen_block->AddVar((*iter)->GetVarName().c_str(), alloc_val);
		}
	}
	// Generate body.
    LLVMValueRef body = CodeGen(p_func_decl_stmt->GetFuncBody(), module, builder);
    if(body == NULL) {
        LLVMDeleteFunction(func);
        return NULL;
    }
	CodeGenContext::PopBlock();
	// Add default return
	//LLVMBuildRet( builder, body );
    // Verify function.
    if(LLVMVerifyFunction(func, LLVMPrintMessageAction) == 1) {
        fprintf(stderr, "Invalid function");
        //LLVMDeleteFunction(func);
        return NULL;
    }
    return func;
}

LLVMValueRef CodeGen( ASTCompoundStatement* p_block_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	LLVMValueRef block_val = nullptr;
	for( auto iter = p_block_stmt->BeginStatement();
			iter != p_block_stmt->EndStatement();
			++iter)
	{
		block_val = CodeGen(*iter, module, builder);
	}
	return block_val;
}

LLVMValueRef CodeGen( ASTJumpStatement* p_jump_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	LLVMValueRef jump_val = nullptr;
	switch( p_jump_stmt->GetJumpType() )
	{
		case jump_type_t::return_t:
			{
				ASTExpression* p_expr = p_jump_stmt->GetExpression();
				if( p_expr )
				{
					LLVMValueRef expr_val = CodeGen(p_expr, module, builder);
					jump_val = LLVMBuildRet(builder, expr_val);
				}
				else
				{
					jump_val = LLVMBuildRetVoid(builder);
				}
				break;
			}
		case jump_type_t::continue_t:
			{
				LLVMBasicBlockRef loop_cond_block = CodeGenContext::GetCurrentLoopCondBlock();
				LLVMBuildBr(builder, loop_cond_block);
				break;
			}
		case jump_type_t::break_t:
			{
				LLVMBasicBlockRef loop_post_block = CodeGenContext::GetCurrentLoopPostBlock();
				LLVMBuildBr(builder, loop_post_block);
				break;
			}
	}
	return jump_val;
}

LLVMValueRef CodeGen( ASTSelectionStatement* p_select_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	// New basic blocks
	LLVMValueRef curr_func_ref = LLVMGetBasicBlockParent(CodeGenContext::GetCurrentBlock()->GetBasicBlock());
    LLVMBasicBlockRef if_block = LLVMAppendBasicBlock(curr_func_ref, "if_branch");
    LLVMBasicBlockRef else_block = LLVMAppendBasicBlock(curr_func_ref, "else_branch");
    LLVMBasicBlockRef if_post_block = LLVMAppendBasicBlock(curr_func_ref, "if_post");

	// build condition
	LLVMValueRef cond_val = CodeGen( p_select_stmt->GetCondition(), module, builder );
	// build branch 
	LLVMValueRef select_val_ref = LLVMBuildCondBr(builder, cond_val, if_block, else_block);

	// build if-branch
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(if_block) );
    LLVMPositionBuilderAtEnd(builder, if_block);
	LLVMValueRef if_branch_val = CodeGen( p_select_stmt->GetTrueBranch(), module, builder );
	if( LLVMGetBasicBlockTerminator(CodeGenContext::GetCurrentBlock()->GetBasicBlock()) == NULL)
		LLVMBuildBr(builder, if_post_block);
	CodeGenContext::PopBlock();

	// build else-branch
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(else_block) );
    LLVMPositionBuilderAtEnd(builder, else_block);
	LLVMValueRef else_branch_val = nullptr;
	if( p_select_stmt->GetFalseBranch() )
	{
		else_branch_val = CodeGen( p_select_stmt->GetFalseBranch(), module, builder );
	}
	if( LLVMGetBasicBlockTerminator(CodeGenContext::GetCurrentBlock()->GetBasicBlock()) == NULL)
		LLVMBuildBr(builder, if_post_block);
	CodeGenContext::PopBlock();
	

	// build post select
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(if_post_block) );
    LLVMPositionBuilderAtEnd(builder, if_post_block);

	return select_val_ref;
}

LLVMValueRef CodeGen( ASTExpressionStatement* p_expr_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	ASTExpression* p_expr = p_expr_stmt->GetExpression();
	if( p_expr )
		return CodeGen(p_expr, module, builder);
	return nullptr;
}

LLVMValueRef CodeGen( ASTIterationStatement* p_iter_stmt, LLVMModuleRef module, LLVMBuilderRef builder)
{
	// New basic blocks
	LLVMValueRef curr_func_ref = LLVMGetBasicBlockParent(CodeGenContext::GetCurrentBlock()->GetBasicBlock());
    LLVMBasicBlockRef loop_cond_block = LLVMAppendBasicBlock(curr_func_ref, "loop_cond");
	CodeGenContext::SetCurrentLoopCondBlock(loop_cond_block);
    LLVMBasicBlockRef loop_body_block = LLVMAppendBasicBlock(curr_func_ref, "loop_body");
    LLVMBasicBlockRef loop_post_block = LLVMAppendBasicBlock(curr_func_ref, "loop_post");
	CodeGenContext::SetCurrentLoopPostBlock(loop_post_block);

	// For while and for loops, condition needs to be evaluated first
	LLVMBuildBr(builder, loop_cond_block);
	
	// build condition block
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(loop_cond_block) );
    LLVMPositionBuilderAtEnd(builder, loop_cond_block);
	LLVMValueRef cond_val = CodeGen( p_iter_stmt->GetTest(), module, builder );
	LLVMBuildCondBr(builder, cond_val, loop_body_block, loop_post_block);
	CodeGenContext::PopBlock();

	// build body block
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(loop_body_block) );
    LLVMPositionBuilderAtEnd(builder, loop_body_block);
	CodeGen( p_iter_stmt->GetBody(), module, builder );
	if( LLVMGetBasicBlockTerminator(CodeGenContext::GetCurrentBlock()->GetBasicBlock()) == NULL)
		LLVMBuildBr(builder, loop_cond_block);
	CodeGenContext::PopBlock();

	// build post block
	CodeGenContext::PushBlock( CodeGenContext::GetNewBlock(loop_post_block) );
    LLVMPositionBuilderAtEnd(builder, loop_post_block);
}

LLVMValueRef CodeGen( ASTExpression* p_expr, LLVMModuleRef module, LLVMBuilderRef builder, bool is_lhs )
{
	switch( p_expr->GetNodeType() )
	{
		case node_type_t::assign_op_expr_nd:
			return CodeGen( static_cast<ASTAssignmentOperationExpression*>(p_expr), module, builder );
		case node_type_t::binary_op_expr_nd:
			return CodeGen( static_cast<ASTBinaryOperationExpression*>(p_expr), module, builder );
		case node_type_t::scalar_id_expr_nd:
			return CodeGen( static_cast<ASTScalarIdentifierExpression*>(p_expr), module, builder, is_lhs );
		case node_type_t::constant_expr_nd:
			return CodeGen( static_cast<ASTConstantExpression*>(p_expr), module, builder );
		case node_type_t::function_call_expr_nd:
			return CodeGen( static_cast<ASTFunctionCallExpression*>(p_expr), module, builder );
	}
}

LLVMValueRef CodeGen( ASTAssignmentOperationExpression* p_assign_op_expr, LLVMModuleRef module, LLVMBuilderRef builder )
{
	LLVMValueRef left = CodeGen( p_assign_op_expr->GetLeftOperand(), module, builder, true );
	LLVMValueRef right = CodeGen( p_assign_op_expr->GetRightOperand(), module, builder );
	return LLVMBuildStore( builder, right, left );
}

LLVMValueRef CodeGen( ASTBinaryOperationExpression* p_bin_op_expr, LLVMModuleRef module, LLVMBuilderRef builder )
{
	LLVMValueRef left = CodeGen( p_bin_op_expr->GetLeftOperand(), module, builder );
	LLVMValueRef right = CodeGen( p_bin_op_expr->GetRightOperand(), module, builder );
	switch (p_bin_op_expr->GetBinaryOperationType())
	{
		case binary_op_t::add_op:
			return LLVMBuildAdd( builder, left, right, "add_temp" );
		case binary_op_t::subtract_op:
			return LLVMBuildSub( builder, left, right, "sub_temp" );
		case binary_op_t::multiply_op:
			return LLVMBuildMul( builder, left, right, "mul_temp" );
		case binary_op_t::divide_op:
			return LLVMBuildSDiv( builder, left, right, "div_temp" );
		case binary_op_t::compare_gt_op:
			return LLVMBuildICmp( builder, LLVMIntSGT, left, right, "cmp_sgt_temp" );
		case binary_op_t::compare_ge_op:
			return LLVMBuildICmp( builder, LLVMIntSGE, left, right, "cmp_sge_temp" );
		case binary_op_t::compare_lt_op:
			return LLVMBuildICmp( builder, LLVMIntSLT, left, right, "cmp_slt_temp" );
		case binary_op_t::compare_le_op:
			return LLVMBuildICmp( builder, LLVMIntSLE, left, right, "cmp_sle_temp" );
		case binary_op_t::compare_eq_op:
			return LLVMBuildICmp( builder, LLVMIntEQ, left, right, "cmp_eq_temp" );
		case binary_op_t::compare_ne_op:
			return LLVMBuildICmp( builder, LLVMIntNE, left, right, "cmp_ne_temp" );
	}
}

LLVMValueRef CodeGen( ASTScalarIdentifierExpression* p_scalar_id_expr, LLVMModuleRef module, LLVMBuilderRef builder, bool is_lhs )
{
	string var_name = p_scalar_id_expr->GetName();
	for( auto iter = CodeGenContext::BeginBlock(); iter != CodeGenContext::EndBlock(); ++iter )
	{
		CodeGenBlock* code_gen_block = *iter;
		LLVMValueRef var_val_ref = code_gen_block->GetVariableRef(var_name);
		if( var_val_ref )
		{
			if(is_lhs)
				return var_val_ref;
			else
				return LLVMBuildLoad(builder, var_val_ref, "");
		}
	}
	return nullptr;
}

LLVMValueRef CodeGen( ASTConstantExpression* p_constant_expr, LLVMModuleRef module, LLVMBuilderRef builder )
{
	switch( p_constant_expr->GetType() )
	{
		case value_type_t::int_type:
			return LLVMConstIntOfString( GetLLVMType( p_constant_expr->GetType() ), p_constant_expr->GetValue().c_str(), 10 );
		case value_type_t::string_type:
			return LLVMBuildGlobalStringPtr( builder, p_constant_expr->GetValue().c_str(), "str_tmp" );
	}
}

LLVMValueRef CodeGen( ASTFunctionCallExpression* p_func_call_expr, LLVMModuleRef module, LLVMBuilderRef builder)
{
	string func_name = p_func_call_expr->GetFuncName();
	size_t num_args = p_func_call_expr->GetCountOfActualArguments();
	string mod_func_name = func_name;
	if(func_name == "puts")
	{
		CreatePutsDeclaration(module);
	}
	else if(func_name == "printf")
	{
		CreatePrintfDeclaration(module, num_args);
		mod_func_name = func_name+to_string(num_args);
	}
	LLVMValueRef func_val = CodeGenContext::GetFunctionValueRef(mod_func_name);
	if(!func_val)
	{
		cerr << "function \'" << func_name << "\' not found" << endl;
		return nullptr;
	}
	if(num_args > 0)
	{	
		LLVMValueRef params[num_args];
		auto start_arg = p_func_call_expr->BeginActualArgument();
		auto end_arg = p_func_call_expr->EndActualArgument();
		auto iter = start_arg;
		size_t i = 0;
		for(; iter != end_arg; ++iter)
		{
			params[i++] = CodeGen(*iter, module, builder);
		}
		return LLVMBuildCall(builder, func_val, params, num_args, func_name.c_str());
	}
	else
	{
		return LLVMBuildCall(builder, func_val, nullptr, 0, func_name.c_str());
	}
}

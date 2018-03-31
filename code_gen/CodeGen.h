#include <map>
#include <vector>
#include "ASTNodes.h"
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

	class CodeGenBlock
	{
		public:
			CodeGenBlock(LLVMBasicBlockRef basic_block)
				: basic_block_(basic_block)
			{ }
			bool AddVar(string name, LLVMValueRef val_ref)
			{
				auto iter = locals_.find(name);
				if(iter != locals_.end())
				{
					cerr << "variable \'" << name << "\' already present in scope" << endl;
					return false;
				}
				locals_[name] = val_ref;
				return true;
			}
			LLVMValueRef GetVariableRef(string name)
			{
				auto iter = locals_.find(name);
				if(iter != locals_.end())
				{
					return (*iter).second;
				}
				return nullptr;
			}
			LLVMBasicBlockRef GetBasicBlock() { return basic_block_; }
		private:
			LLVMBasicBlockRef basic_block_;
			map<string,LLVMValueRef> locals_;
	};

class CodeGenContext
{
	public:
		static CodeGenBlock* GetNewBlock(LLVMBasicBlockRef basic_block) { return new CodeGenBlock(basic_block); }
		static void PushBlock(CodeGenBlock *code_gen_block) { block_stack.push_back(code_gen_block); }
		static void PopBlock() { block_stack.pop_back(); }
		static CodeGenBlock* GetCurrentBlock() { return block_stack.back(); }
		static vector<CodeGenBlock*>::reverse_iterator BeginBlock() { return block_stack.rbegin(); }
		static vector<CodeGenBlock*>::reverse_iterator EndBlock() { return block_stack.rend(); }

		static bool AddFunctionToContext(string name, LLVMValueRef func_val)
		{
			auto iter = functions_.find(name);
			if(iter != functions_.end())
			{
				cerr << "function \'" << name << "\' already present. function over-loading is not supported" << endl;
				return false;
			}
			functions_[name] = func_val;
			return true;
		}
		static LLVMValueRef GetFunctionValueRef( string name )
		{
			auto iter = functions_.find(name);
			if(iter != functions_.end())
			{
				return (*iter).second;
			}
			return nullptr;
		}
		static void SetCurrentLoopCondBlock(LLVMBasicBlockRef loop_cond_block) { curr_loop_cond_block_ = loop_cond_block; }
		static LLVMBasicBlockRef GetCurrentLoopCondBlock() { return curr_loop_cond_block_; }
		static void SetCurrentLoopPostBlock(LLVMBasicBlockRef loop_post_block) { curr_loop_post_block_ = loop_post_block; }
		static LLVMBasicBlockRef GetCurrentLoopPostBlock() { return curr_loop_post_block_; }
	private:
		static vector<CodeGenBlock*> block_stack;
		static map<string,LLVMValueRef> functions_;
		static LLVMBasicBlockRef curr_loop_cond_block_;
		static LLVMBasicBlockRef curr_loop_post_block_;
};

LLVMTypeRef GetLLVMType( value_type_t type );

LLVMValueRef CodeGen( ASTProgramRoot* p_root );
LLVMValueRef CodeGen( ASTFile* p_file, LLVMModuleRef module, LLVMBuilderRef builder );

LLVMValueRef CodeGen( ASTStatement* p_stmt, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTVarDeclarationStatement* p_func_decl_stmt, LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef CodeGen( ASTFuncDeclarationStatement* p_func_decl_stmt, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTCompoundStatement* p_block_stmt, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTJumpStatement* p_jump_stmt, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTSelectionStatement* p_select_stmt, LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef CodeGen( ASTExpressionStatement* p_expr_stmt, LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef CodeGen( ASTIterationStatement* p_iter_stmt, LLVMModuleRef module, LLVMBuilderRef builder);

LLVMValueRef CodeGen( ASTExpression* p_expr, LLVMModuleRef module, LLVMBuilderRef builder, bool is_lhs = false );
LLVMValueRef CodeGen( ASTAssignmentOperationExpression* p_assign_op_expr, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTBinaryOperationExpression* p_bin_op_expr, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTScalarIdentifierExpression* p_scalar_id_expr, LLVMModuleRef module, LLVMBuilderRef builder, bool is_lhs = false );
LLVMValueRef CodeGen( ASTConstantExpression* p_constant_expr, LLVMModuleRef module, LLVMBuilderRef builder );
LLVMValueRef CodeGen( ASTFunctionCallExpression* p_func_call_expr, LLVMModuleRef module, LLVMBuilderRef builder);

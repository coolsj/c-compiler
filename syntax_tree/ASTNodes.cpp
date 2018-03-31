#include "ASTNodes.h"
using namespace std;

string ASTFuncDeclarationStatement::GetFuncSignature()
{
	string sig = type_to_string(return_type_);
	sig += " "+name_;
	sig += "(";
	if( arguments_ )
	{
		auto start = arguments_->BeginArgument();
		auto end = arguments_->EndArgument();
		for( auto iter = start; iter != end; ++iter )
		{
			if( iter != start )
				sig += ",";
			sig += type_to_string( (*iter)->GetVarType() );
		}
	}
	sig += ")";
}

//////////////////////////
// -- Destructors --
//////////////////////////

ASTProgramRoot::~ASTProgramRoot()
{
	for( auto p_file : files_ )
		delete p_file;
}

ASTFile::~ASTFile()
{
	for( auto p_decl : declarations_ )
		delete p_decl;
}

ASTVarDeclarationStatement::~ASTVarDeclarationStatement()
{
	if(init_)
		delete init_;
}

ASTFunctionFormalArguments::~ASTFunctionFormalArguments()
{
	for( auto p_arg : arguments_ )
		delete p_arg;
}

ASTFuncDeclarationStatement::~ASTFuncDeclarationStatement()
{
	if(arguments_)
		delete arguments_;
	if(body_)
		delete body_;
}

ASTCompoundStatement::~ASTCompoundStatement()
{
	for( auto p_stmt : statements_)
		delete p_stmt;
}

ASTExpressionStatement::~ASTExpressionStatement()
{
	if(expr_)
		delete expr_;
}

ASTSelectionStatement::~ASTSelectionStatement()
{
	if(condition_) delete condition_;
	if(true_branch_) delete true_branch_;
	if(false_branch_) delete false_branch_;
}

ASTIterationStatement::~ASTIterationStatement()
{
	if(init_) delete init_;
	if(test_) delete test_;
	if(incr_) delete incr_;
	if(body_) delete body_;
}

ASTJumpStatement::~ASTJumpStatement()
{
	if(p_expr_) delete p_expr_;
}

ASTScalarIdentifierExpression::~ASTScalarIdentifierExpression()
{
	if(p_sym_) delete p_sym_;
}

ASTArrayIdentifierExpression::~ASTArrayIdentifierExpression()
{
	if(p_sym_) delete p_sym_;
	for( auto p_expr : indices_ )
		delete p_expr;
}

ASTAssignmentOperationExpression::~ASTAssignmentOperationExpression()
{
	if(left_operand_) delete left_operand_;
	if(right_operand_) delete right_operand_;
}

ASTBinaryOperationExpression::~ASTBinaryOperationExpression()
{
	if(left_operand_) delete left_operand_;
	if(right_operand_) delete right_operand_;
}

ASTUnaryOperationExpression::~ASTUnaryOperationExpression()
{
	if(operand_) delete operand_;
}

ASTFunctionCallExpression::~ASTFunctionCallExpression()
{
	for( auto p_expr : actual_args_ )
		delete p_expr;
}

//////////////////////////
// -- PrintNode methods --
//////////////////////////

void ASTProgramRoot::PrintNode()
{
	for(auto p_file: files_)
		p_file->PrintNode();
}

void ASTFile::PrintNode()
{
	cout << "// File Name: " << file_name_ << endl;
	for(auto p_decl : declarations_)
		p_decl->PrintNode();
}

void ASTVarDeclarationStatement::PrintNode()
{
	cout << type_to_string(type_) << " ";
	cout << name_ << " ";
	if(init_)
	{
		cout << "=" << " ";
		init_->PrintNode();
	}
	cout << ";" << endl;
}

void ASTFunctionFormalArguments::PrintNode()
{
	for(auto iter = arguments_.begin(); iter != arguments_.end(); ++iter)
	{
		if(iter != arguments_.begin())
			cout << ", ";
		(*iter)->PrintNode();
	}
}

void ASTFuncDeclarationStatement::PrintNode()
{
	cout << type_to_string(return_type_) << " ";
	cout << name_ << " ";
	cout << "( ";
	if(arguments_)
		arguments_->PrintNode();
	cout << " )" << endl;
	body_->PrintNode();
}

void ASTCompoundStatement::PrintNode()
{
	cout << "{" << endl;
	for( auto p_stmt : statements_ )
		p_stmt->PrintNode();
	cout << "}" << endl;
}

void ASTExpressionStatement::PrintNode()
{
	expr_->PrintNode();
	cout << ";" << endl;
}

void ASTSelectionStatement::PrintNode()
{
	cout << "if( ";
	condition_->PrintNode();
	cout << " )" << endl;
	true_branch_->PrintNode();
	if(false_branch_)
	{
		cout << "else" << endl;
		false_branch_->PrintNode();
	}
}

void ASTIterationStatement::PrintNode()
{
	cout << "while( ";
	if( init_ )
		init_->PrintNode();
	test_->PrintNode();
	if( incr_ )
		incr_->PrintNode();
	cout << " )" << endl;
	body_->PrintNode();
}

void ASTJumpStatement::PrintNode()
{
	cout << jump_type_to_string(jt_) << " ";
	if(p_expr_)
		p_expr_->PrintNode();
	cout << ";" << endl;
}

void ASTScalarIdentifierExpression::PrintNode()
{
	cout << name_ << " ";
}

void ASTArrayIdentifierExpression::PrintNode()
{
	cout << name_;
	for( auto p_expr : indices_ )
	{
		cout << "[";
		p_expr->PrintNode();
		cout << "]";
	}
	cout << " ";
}

void ASTAssignmentOperationExpression::PrintNode()
{
	left_operand_->PrintNode();
	cout << "= ";
	right_operand_->PrintNode();
	cout << " ";
}

void ASTBinaryOperationExpression::PrintNode()
{
	left_operand_->PrintNode();
	cout << binary_op_to_string(op_) << " ";
	right_operand_->PrintNode();
	cout << " ";
}

void ASTUnaryOperationExpression::PrintNode()
{
	switch( op_ )
	{
		case unary_op_t::pre_incr_op:
		case unary_op_t::pre_decr_op:
			cout << unary_op_to_string(op_);
			operand_->PrintNode();
			cout << " ";
			break;
		case unary_op_t::post_incr_op:
		case unary_op_t::post_decr_op:
			operand_->PrintNode(); 
			cout << unary_op_to_string(op_) << " ";
			break;
	}
}

void ASTConstantExpression::PrintNode()
{
	switch( type_ )
	{
		case value_type_t::int_type:
			cout << stoi(value_);
			break;
		case value_type_t::float_type:
			cout << stof(value_);
			break;
	}
}

void ASTFunctionCallExpression::PrintNode()
{
	cout << func_name_ << "(";
	for(auto iter = actual_args_.begin(); iter != actual_args_.end(); ++iter)
	{
		if( iter != actual_args_.begin() )
			cout << ", ";
		(*iter)->PrintNode();
	}
	cout << ")";
}

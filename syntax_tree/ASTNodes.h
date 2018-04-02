#ifndef ASTNODES_H_
#define ASTNODES_H_

#include <iostream>
#include <string>
#include <vector>
#include <memory>

#include "CommonTypes.h"
#include "SymbolDataBase.h"
using namespace std;

class ASTBase
{
	public:
		ASTBase( node_type_t nt )
			: nt_(nt)
		{ }
		ASTBase(ASTBase & that);
		virtual ~ASTBase() { }
		node_type_t GetNodeType() { return nt_; }
		virtual void PrintNode() = 0;
		virtual ASTBase* Clone() = 0;

	protected:
		node_type_t nt_;
};

class ASTFile;
class ASTProgramRoot : public ASTBase
{
	public:
		ASTProgramRoot()
			: ASTBase(node_type_t::program_root_nd)
		{ }
		ASTProgramRoot(ASTProgramRoot & that);
		~ASTProgramRoot();
		void AddFile(ASTFile* p_file) { files_.push_back( p_file ); }
		size_t GetCountOfFiles() { return files_.size(); }

		vector<ASTFile*>::iterator BeginFile() { return files_.begin(); }
		vector<ASTFile*>::iterator EndFile() { return files_.end(); }

		void PrintNode();
		ASTProgramRoot* Clone() { return new ASTProgramRoot(*this); }

	private:
		vector<ASTFile*> files_;
};

class ASTDeclarationStatement;
class ASTFile : public ASTBase
{
	public:
		ASTFile(string file)
			: ASTBase(node_type_t::file_nd), file_name_(file)
		{ }
		ASTFile(ASTFile & that);
		~ASTFile();
		void AddDeclaration(ASTDeclarationStatement* p_decl_stmt) { declarations_.push_back( p_decl_stmt ); }
		string GetFileName() { return file_name_; }

		vector<ASTDeclarationStatement*>::iterator BeginDeclaration() { return declarations_.begin(); }
		vector<ASTDeclarationStatement*>::iterator EndDeclaration() { return declarations_.end(); }

		void PrintNode();
		ASTFile* Clone() { return new ASTFile(*this); }

	private:
		string file_name_;
		vector<ASTDeclarationStatement*> declarations_;
};

// --- LIB : Statements

class ASTStatement : public ASTBase
{
	public:
		ASTStatement(node_type_t nt)
			: ASTBase(nt)
		{ }
		ASTStatement(ASTStatement & that);
		virtual ~ASTStatement() { }
		virtual void PrintNode() = 0;
		virtual ASTStatement* Clone() = 0;
};

class ASTDeclarationStatement : public ASTStatement
{
	public:
		ASTDeclarationStatement(node_type_t nt)
			: ASTStatement(nt)
		{ }
		ASTDeclarationStatement(ASTDeclarationStatement & that);
		virtual ~ASTDeclarationStatement() { }
		virtual void PrintNode() = 0;
		virtual ASTDeclarationStatement* Clone() = 0;
};

class ASTExpression;
class ASTVarDeclarationStatement : public ASTDeclarationStatement
{
	public:
		ASTVarDeclarationStatement(string name, ASTExpression* init)
			: ASTDeclarationStatement(node_type_t::var_decl_stmt_nd), name_(name), init_(init)
		{ }
		ASTVarDeclarationStatement(ASTVarDeclarationStatement & that);
		~ASTVarDeclarationStatement();
		string GetVarName() { return name_; }
		void SetVarType(value_type_t type) { type_ = type; }
		value_type_t GetVarType() { return type_; }
		ASTExpression* GetVarInit() { return init_; }

		void PrintNode();
		ASTVarDeclarationStatement* Clone() { return new ASTVarDeclarationStatement(*this); }

	private:
		string name_;
		value_type_t type_;
		ASTExpression* init_;
};

class ASTFunctionFormalArguments : public ASTBase
{
	public:
		ASTFunctionFormalArguments()
			: ASTBase(node_type_t::func_formal_arg_nd)
		{ }
		ASTFunctionFormalArguments(ASTFunctionFormalArguments & that);
		~ASTFunctionFormalArguments();
		void AddArgument(ASTVarDeclarationStatement* p_arg) { arguments_.push_back( p_arg ); }
		size_t GetCountOfArguments() { return arguments_.size(); }

		vector<ASTVarDeclarationStatement*>::iterator BeginArgument() { return arguments_.begin(); }
		vector<ASTVarDeclarationStatement*>::iterator EndArgument() { return arguments_.end(); }

		void PrintNode();
		ASTFunctionFormalArguments* Clone() { return new ASTFunctionFormalArguments(*this); }

	private:
		vector<ASTVarDeclarationStatement*> arguments_;
};

class ASTCompoundStatement;
class ASTFuncDeclarationStatement : public ASTDeclarationStatement
{
	public:
		ASTFuncDeclarationStatement(string name, value_type_t return_type, ASTFunctionFormalArguments* arguments, ASTCompoundStatement* body)
			: ASTDeclarationStatement(node_type_t::func_decl_stmt_nd), name_(name), return_type_(return_type), arguments_(arguments), body_(body)
		{ }
		ASTFuncDeclarationStatement(ASTFuncDeclarationStatement & that);
		~ASTFuncDeclarationStatement();
		string GetFuncName() { return name_; }
		value_type_t GetReturnType() { return return_type_; }
		ASTFunctionFormalArguments* GetFormalArguments() { return arguments_; }
		ASTCompoundStatement* GetFuncBody() { return body_; }

		string GetFuncSignature();
		void PrintNode();
		ASTFuncDeclarationStatement* Clone() { return new ASTFuncDeclarationStatement(*this); }

	private:
		string name_;
		value_type_t return_type_;
		ASTFunctionFormalArguments* arguments_;
		ASTCompoundStatement* body_;
};

class ASTCompoundStatement : public ASTStatement
{
	public:
		ASTCompoundStatement()
			: ASTStatement(node_type_t::compound_stmt_nd)
		{
			table_ = shared_ptr<SymbolTable>( new SymbolTable() );
		}
		ASTCompoundStatement(ASTCompoundStatement & that);
		~ASTCompoundStatement();
		void AddStatement(ASTStatement* p_stmt) { statements_.push_back( p_stmt ); }
		shared_ptr<SymbolTable> GetSymbolTable() { return table_; }

		vector<ASTStatement*>::iterator BeginStatement() { return statements_.begin(); }
		vector<ASTStatement*>::iterator EndStatement() { return statements_.end(); }

		void PrintNode();
		ASTCompoundStatement* Clone() { return new ASTCompoundStatement(*this); }

	private:
		vector<ASTStatement*> statements_;
		shared_ptr<SymbolTable> table_;
};


class ASTExpressionStatement : public ASTStatement
{
	public:
		ASTExpressionStatement(ASTExpression* expr)
			: ASTStatement(node_type_t::expr_stmt_nd), expr_(expr)
		{ }
		ASTExpressionStatement(ASTExpressionStatement & that);
		~ASTExpressionStatement();
		ASTExpression* GetExpression() { return expr_; }

		void PrintNode();
		ASTExpressionStatement* Clone() { return new ASTExpressionStatement(*this); }

	private:
		ASTExpression* expr_;
};

class ASTSelectionStatement : public ASTStatement
{
	public:
		ASTSelectionStatement(ASTExpression* cond, ASTStatement* true_branch, ASTStatement* false_branch)
			: ASTStatement(node_type_t::select_stmt_nd), condition_(cond), true_branch_(true_branch), false_branch_(false_branch)
		{ }
		ASTSelectionStatement(ASTSelectionStatement & that);
		~ASTSelectionStatement();
		ASTExpression* GetCondition() { return condition_; }
		ASTStatement* GetTrueBranch() { return true_branch_; }
		ASTStatement* GetFalseBranch() { return false_branch_; }

		void PrintNode();
		ASTSelectionStatement* Clone() { return new ASTSelectionStatement(*this); }

	private:
		ASTExpression* condition_;
		ASTStatement* true_branch_;
		ASTStatement* false_branch_;
};

class ASTIterationStatement : public ASTStatement
{
	public:
		ASTIterationStatement(ASTStatement* init, ASTExpression* test, ASTExpression* incr, ASTStatement* body)
			: ASTStatement(node_type_t::iter_stmt_nd), init_(init), test_(test), incr_(incr), body_(body)
		{ }
		ASTIterationStatement(ASTIterationStatement & that);
		~ASTIterationStatement();
		ASTStatement* GetInit() { return init_; }
		ASTExpression* GetTest() { return test_; }
		ASTExpression* GetIncr() { return incr_; }
		ASTStatement* GetBody() { return body_; }

		void PrintNode();
		ASTIterationStatement* Clone() { return new ASTIterationStatement(*this); }

	private:
		ASTStatement* init_;
		ASTExpression* test_;
		ASTExpression* incr_;
		ASTStatement* body_;
};

class ASTJumpStatement : public ASTStatement
{
	public:
		ASTJumpStatement(jump_type_t jt, ASTExpression* p_expr)
			: ASTStatement(node_type_t::jump_stmt_nd), jt_(jt), p_expr_(p_expr)
		{ }
		ASTJumpStatement(ASTJumpStatement & that);
		~ASTJumpStatement();
		jump_type_t GetJumpType() { return jt_; }
		ASTExpression* GetExpression() { return p_expr_; }

		void PrintNode();
		ASTJumpStatement* Clone() { return new ASTJumpStatement(*this); }

	private:
		jump_type_t jt_;
		ASTExpression* p_expr_;
};

// --- LIB : Expressions

class ASTExpression : public ASTBase
{
	public:
		ASTExpression(node_type_t nt)
			: ASTBase(nt)
		{ }
		ASTExpression(ASTExpression & that);
		virtual ~ASTExpression() { }
		virtual value_type_t GetType() { };
		virtual void PrintNode() = 0;
		virtual ASTExpression* Clone() = 0;
};

class ASTIdentifierExpression : public ASTExpression
{
	public:
		ASTIdentifierExpression(node_type_t nt, string name)
			: ASTExpression(nt), name_(name), p_sym_(nullptr)
		{ }
		ASTIdentifierExpression(ASTIdentifierExpression & that);
		virtual ~ASTIdentifierExpression() { }
		string GetName() { return name_; }
		virtual void PrintNode() = 0;
		virtual ASTIdentifierExpression* Clone() = 0;

	protected:
		string name_;
		SymbolTableEntry *p_sym_;
};

class ASTScalarIdentifierExpression : public ASTIdentifierExpression
{
	public:
		ASTScalarIdentifierExpression(string name)
			: ASTIdentifierExpression(node_type_t::scalar_id_expr_nd, name)
		{ }
		ASTScalarIdentifierExpression(ASTScalarIdentifierExpression & that);
		~ASTScalarIdentifierExpression();

		void PrintNode();
		ASTScalarIdentifierExpression* Clone() { return new ASTScalarIdentifierExpression(*this); }
};

class ASTArrayIdentifierExpression : public ASTIdentifierExpression
{
	public:
		ASTArrayIdentifierExpression(string name)
			: ASTIdentifierExpression(node_type_t::array_id_expr_nd, name)
		{ }
		ASTArrayIdentifierExpression(ASTArrayIdentifierExpression & that);
		~ASTArrayIdentifierExpression();
		void AddIndices(ASTExpression* index) {	indices_.push_back(index); }

		vector<ASTExpression*>::iterator BeginIndex() { return indices_.begin(); }
		vector<ASTExpression*>::iterator EndIndex() { return indices_.end(); }

		void PrintNode();
		ASTArrayIdentifierExpression* Clone() { return new ASTArrayIdentifierExpression(*this); }

	private:
		vector<ASTExpression*> indices_;
};

class ASTOperationExpression : public ASTExpression
{
	public:
		ASTOperationExpression(node_type_t nt)
			: ASTExpression(nt)
		{ }
		ASTOperationExpression(ASTOperationExpression & that);
		virtual ~ASTOperationExpression() { }
		virtual void PrintNode() = 0;
		virtual ASTOperationExpression* Clone() = 0;
};

class ASTAssignmentOperationExpression : public ASTOperationExpression
{
	public:
		ASTAssignmentOperationExpression(ASTExpression* left_operand, ASTExpression* right_operand)
			: ASTOperationExpression(node_type_t::assign_op_expr_nd), left_operand_(left_operand), right_operand_(right_operand)
		{ }
		ASTAssignmentOperationExpression(ASTAssignmentOperationExpression & that);
		~ASTAssignmentOperationExpression();
		ASTExpression* GetLeftOperand() { return left_operand_; }
		ASTExpression* GetRightOperand() { return right_operand_; }

		void PrintNode();
		ASTAssignmentOperationExpression* Clone() { return new ASTAssignmentOperationExpression(*this); }

	private:
		ASTExpression* left_operand_;
		ASTExpression* right_operand_;
};

class ASTBinaryOperationExpression : public ASTOperationExpression
{
	public:
		ASTBinaryOperationExpression(binary_op_t op, ASTExpression* left_operand, ASTExpression* right_operand)
			: ASTOperationExpression(node_type_t::binary_op_expr_nd), op_(op), left_operand_(left_operand), right_operand_(right_operand)
		{ }
		ASTBinaryOperationExpression(ASTBinaryOperationExpression & that);
		~ASTBinaryOperationExpression();
		binary_op_t GetBinaryOperationType() { return op_; }
		ASTExpression* GetLeftOperand() { return left_operand_; }
		ASTExpression* GetRightOperand() { return right_operand_; }

		void PrintNode();
		ASTBinaryOperationExpression* Clone() { return new ASTBinaryOperationExpression(*this); }

	private:
		binary_op_t op_;
		ASTExpression* left_operand_;
		ASTExpression* right_operand_;
};

class ASTUnaryOperationExpression: public ASTOperationExpression
{
	public:
		ASTUnaryOperationExpression(node_type_t nt, unary_op_t op, ASTExpression* operand)
			: ASTOperationExpression(nt), op_(op), operand_(operand)
		{ }
		ASTUnaryOperationExpression(ASTUnaryOperationExpression & that);
		~ASTUnaryOperationExpression();
		unary_op_t GetUnaryOperationType() { return op_; }
		ASTExpression* GetOperand() { return operand_; }

		void PrintNode();
		ASTUnaryOperationExpression* Clone() { return new ASTUnaryOperationExpression(*this); }

	private:
		unary_op_t op_;
		ASTExpression* operand_;
};

class ASTConstantExpression : public ASTExpression
{
	public:
		ASTConstantExpression(value_type_t type, string value)
			: ASTExpression(node_type_t::constant_expr_nd), type_(type), value_(value)
		{ }
		ASTConstantExpression(ASTConstantExpression & that);

		value_type_t GetType() { return type_; }
		string GetValue() { return value_; }

		void PrintNode();
		ASTConstantExpression* Clone() { return new ASTConstantExpression(*this); }

	private:
		value_type_t type_;
		string value_;
};

class ASTFunctionCallExpression : public ASTExpression
{
	public:
		ASTFunctionCallExpression(string func_name)
			: ASTExpression(node_type_t::function_call_expr_nd), func_name_(func_name)
		{ }
		ASTFunctionCallExpression(ASTFunctionCallExpression & that);
		~ASTFunctionCallExpression();
		void AddActualArgument(ASTExpression* p_arg_expr) { actual_args_.push_back(p_arg_expr); }
		size_t GetCountOfActualArguments() { return actual_args_.size(); }

		string GetFuncName() { return func_name_; }
		vector<ASTExpression*>::iterator BeginActualArgument() { return actual_args_.begin(); }
		vector<ASTExpression*>::iterator EndActualArgument() { return actual_args_.end(); }

		void PrintNode();
		ASTFunctionCallExpression* Clone() { return new ASTFunctionCallExpression(*this); }

	private:
		string func_name_;
		vector<ASTExpression*> actual_args_;
};

#endif // ASTNODES_H_

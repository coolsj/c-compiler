%{
#include <cstdio>
#include <iostream>
#include <unistd.h>
#include <cstdlib>
#include <string>
#include <cstring>
#include <memory>
using namespace std;
// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse();
extern FILE *yyin;
 
void yyerror(const char *s);
string recover_filename(FILE * f); 

#include "ASTNodes.h"
extern ASTProgramRoot* p_root;
static shared_ptr<SymbolTable> p_curr_symbol_table;
%}

%code requires {
#include "ASTNodes.h"
#include "non_terminal_types.h"
}

%token	IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token	PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token	XOR_ASSIGN OR_ASSIGN
%token	TYPEDEF_NAME ENUMERATION_CONSTANT

%token	TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token	CONST RESTRICT VOLATILE
%token	BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token	COMPLEX IMAGINARY 
%token	STRUCT UNION ENUM ELLIPSIS

%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token	ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%union
{
	char* identifier;
	int int_constant;
	float float_constant;
	assign_op_t assign_op;
	string* string_constant;
	value_type_t val_type;
	ASTFile* p_file;
	ASTDeclarationStatement* p_decl;
	ASTFuncDeclarationStatement* p_func_decl;
	ASTVarDeclarationStatement* p_var_decl;
	ASTCompoundStatement* p_compound_stmt;
	direct_declarator_t dd;
	VarDeclStmtVectorT* p_var_decl_stmt_vec;
	StmtVectorT* p_stmt_vec;
	ExprVectorT* p_expr_vec;
	ASTFunctionFormalArguments* p_func_arg;
	ASTStatement* p_stmt;
	ASTJumpStatement* p_jump_stmt;
	ASTExpression* p_expr;
	ASTBinaryOperationExpression* p_bin_op_expr;
	ASTScalarIdentifierExpression* p_scalar_id_expr;
	ASTConstantExpression* p_constant_expr;
	ASTSelectionStatement* p_select_stmt;
	ASTExpressionStatement* p_expr_stmt;
	ASTIterationStatement* p_iter_stmt;
}

%type<p_file> translation_unit
%type<p_decl> external_declaration
%type<p_func_decl> function_definition
%type<val_type> declaration_specifiers type_specifier
%type<dd> direct_declarator declarator
%type<p_var_decl_stmt_vec> init_declarator_list declaration
%type<p_compound_stmt> compound_statement block_item_list
%type<p_func_arg> parameter_type_list parameter_list
%type<p_var_decl> parameter_declaration init_declarator
%type<p_stmt_vec> block_item
%type<p_stmt> statement
%type<p_jump_stmt> jump_statement
%type<p_select_stmt> selection_statement
%type<p_iter_stmt> iteration_statement
%type<p_expr> expression assignment_expression initializer
%type<p_expr> conditional_expression logical_or_expression logical_and_expression 
%type<p_expr> inclusive_or_expression exclusive_or_expression and_expression
%type<p_expr> equality_expression relational_expression shift_expression
%type<p_expr> additive_expression multiplicative_expression
%type<p_expr> primary_expression postfix_expression unary_expression cast_expression
%type<p_constant_expr> constant string
%type<p_expr_vec> argument_expression_list
%type<p_expr_stmt> expression_statement
%type<assign_op> assignment_operator

%start translation_unit
%%

primary_expression
	: IDENTIFIER			{
								string name(yylval.identifier);
								$$ = new ASTScalarIdentifierExpression(name);
							}
	| constant				{ $$ = $1; }
	| string				{ $$ = $1; }
/*	| '(' expression ')'
	| generic_selection */
	;

constant
	: I_CONSTANT			{ $$ = new ASTConstantExpression( value_type_t::int_type, to_string(yylval.int_constant) ); }
/*	| F_CONSTANT
	| ENUMERATION_CONSTANT */
	;
/*
enumeration_constant
	: IDENTIFIER
	;
*/
string
	: STRING_LITERAL		{
								$$ = new ASTConstantExpression( value_type_t::string_type, *(yylval.string_constant) );
								delete yylval.string_constant;
							}
/*	| FUNC_NAME */
	;
/*
generic_selection
	: GENERIC '(' assignment_expression ',' generic_assoc_list ')'
	;

generic_assoc_list
	: generic_association
	| generic_assoc_list ',' generic_association
	;

generic_association
	: type_name ':' assignment_expression
	| DEFAULT ':' assignment_expression
	;
*/
postfix_expression
	: primary_expression										{ $$ = $1; }
/*	| postfix_expression '[' expression ']' */
	| postfix_expression '(' ')'								{
																	ASTIdentifierExpression* p_id_expr = dynamic_cast<ASTIdentifierExpression*>($1);
																	if( p_id_expr )
																	{
																		$$ = new ASTFunctionCallExpression(p_id_expr->GetName());
																		delete $1;
																	}
																	else
																	{
																		cerr << "unexpected token encoutered at function call" << endl;
																		$$ = nullptr;
																	}
																}
	| postfix_expression '(' argument_expression_list ')'		{
																	ASTIdentifierExpression* p_id_expr = dynamic_cast<ASTIdentifierExpression*>($1);
																	if( p_id_expr )
																	{
																		ASTFunctionCallExpression* p_func_call_expr = new ASTFunctionCallExpression(p_id_expr->GetName());
																		delete $1;
																		for( auto p_arg : *($3) )
																		{
																			p_func_call_expr->AddActualArgument(p_arg);
																		}
																		$$ = p_func_call_expr;
																	}
																	else
																	{
																		cerr << "unexpected token encoutered at function call" << endl;
																		$$ = nullptr;
																	}
																}
/*	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	| '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}' */
	;

argument_expression_list
	: assignment_expression									{
																$$ = new ExprVectorT();
																$$->push_back($1);
															}
	| argument_expression_list ',' assignment_expression	{
																$1->push_back($3);
																$$ = $1;
															}
	;

unary_expression
	: postfix_expression				{ $$ = $1; }
/*	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	| ALIGNOF '(' type_name ')' */
	;
/*
unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;
*/
cast_expression
	: unary_expression						{ $$ = $1; }
/*	| '(' type_name ')' cast_expression */
	;

multiplicative_expression
	: cast_expression									{ $$ = $1; }
	| multiplicative_expression '*' cast_expression		{ $$ = new ASTBinaryOperationExpression(binary_op_t::multiply_op, $1, $3); }
	| multiplicative_expression '/' cast_expression		{ $$ = new ASTBinaryOperationExpression(binary_op_t::divide_op, $1, $3); }
	| multiplicative_expression '%' cast_expression		{ $$ = new ASTBinaryOperationExpression(binary_op_t::modulus_op, $1, $3); }
	;

additive_expression
	: multiplicative_expression							{ $$ = $1; }
	| additive_expression '+' multiplicative_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::add_op, $1, $3); }
	| additive_expression '-' multiplicative_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::subtract_op, $1, $3); }
	;

shift_expression
	: additive_expression
/*	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression */
	;

relational_expression
	: shift_expression								{ $$ = $1; }
	| relational_expression '<' shift_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::compare_lt_op, $1, $3); }
	| relational_expression '>' shift_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::compare_gt_op, $1, $3); }
	| relational_expression LE_OP shift_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::compare_le_op, $1, $3); }
	| relational_expression GE_OP shift_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::compare_ge_op, $1, $3); }
	;

equality_expression
	: relational_expression								{ $$ = $1; }
	| equality_expression EQ_OP relational_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::compare_eq_op, $1, $3); }
	| equality_expression NE_OP relational_expression	{ $$ = new ASTBinaryOperationExpression(binary_op_t::compare_ne_op, $1, $3); }
	;

and_expression
	: equality_expression
/*	| and_expression '&' equality_expression */
	;

exclusive_or_expression
	: and_expression
/*	| exclusive_or_expression '^' and_expression */
	;

inclusive_or_expression
	: exclusive_or_expression
/*	| inclusive_or_expression '|' exclusive_or_expression */
	;

logical_and_expression
	: inclusive_or_expression
/*	| logical_and_expression AND_OP inclusive_or_expression */
	;

logical_or_expression
	: logical_and_expression
/*	| logical_or_expression OR_OP logical_and_expression */
	;

conditional_expression
	: logical_or_expression
/*	| logical_or_expression '?' expression ':' conditional_expression */
	;

assignment_expression
	: conditional_expression										{ $$ = $1; }
	| unary_expression assignment_operator assignment_expression	{
																		ASTExpression *p_rhs = $3;
																		switch($2)
																		{
																			case assign_op_t::add_assign_op:
																				p_rhs = new ASTBinaryOperationExpression(binary_op_t::add_op, $1->Clone(), $3);
																				break;
																			case assign_op_t::subtract_assign_op:
																				p_rhs = new ASTBinaryOperationExpression(binary_op_t::subtract_op, $1->Clone(), $3);
																				break;
																			case assign_op_t::multiply_assign_op:
																				p_rhs = new ASTBinaryOperationExpression(binary_op_t::multiply_op, $1->Clone(), $3);
																				break;
																			case assign_op_t::divide_assign_op:
																				p_rhs = new ASTBinaryOperationExpression(binary_op_t::divide_op, $1->Clone(), $3);
																				break;
																		}
																		$$ = new ASTAssignmentOperationExpression($1, p_rhs);
																	}
	;

assignment_operator
	: '='			{ $$ = assign_op_t::assign_op; }
	| MUL_ASSIGN	{ $$ = assign_op_t::multiply_assign_op; }
	| DIV_ASSIGN	{ $$ = assign_op_t::divide_assign_op; }
/*	| MOD_ASSIGN */
	| ADD_ASSIGN	{ $$ = assign_op_t::add_assign_op; }
	| SUB_ASSIGN	{ $$ = assign_op_t::subtract_assign_op; }
/*	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN */
	;

expression
	: assignment_expression						{ $$ = $1; }
/*	| expression ',' assignment_expression */
	;
/*
constant_expression
	: conditional_expression
	;
*/
declaration
	: declaration_specifiers init_declarator_list ';'	{
															for( auto p_var_decl_stmt : *($2) )
															{
																p_var_decl_stmt->SetVarType($1);
															}
															$$ = $2;
														}
/*	| declaration_specifiers init_declarator_list ';'
	| static_assert_declaration */
	;

declaration_specifiers
	: type_specifier	{ $$ = $1; }
/*	: storage_class_specifier declaration_specifiers
	| storage_class_specifier
	| type_specifier declaration_specifiers
	| type_specifier
	| type_qualifier declaration_specifiers
	| type_qualifier
	| function_specifier declaration_specifiers
	| function_specifier
	| alignment_specifier declaration_specifiers
	| alignment_specifier */
	;

init_declarator_list
	: init_declarator							{
													$$ = new VarDeclStmtVectorT;
													$$->push_back($1);
												}
	| init_declarator_list ',' init_declarator	{
													$1->push_back($3);
													$$ = $1;
												}
	;

init_declarator
	: declarator '=' initializer	{
										string name($1.name);
										$$ = new ASTVarDeclarationStatement(name, $3);
									}
	| declarator					{
										string name($1.name);
										$$ = new ASTVarDeclarationStatement(name, nullptr);
									}
	;
/*
storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| THREAD_LOCAL
	| AUTO
	| REGISTER
	;
*/
type_specifier
	: VOID		{ $$ = value_type_t::void_type; }
	| CHAR		{ $$ = value_type_t::char_type; }
	| SHORT		{ $$ = value_type_t::short_type; }
	| INT		{ $$ = value_type_t::int_type; }
	| LONG		{ $$ = value_type_t::long_type; }
	| FLOAT		{ $$ = value_type_t::float_type; }
	| DOUBLE	{ $$ = value_type_t::double_type; }
	| SIGNED	{ $$ = value_type_t::signed_type; }
	| UNSIGNED	{ $$ = value_type_t::unsigned_type; }
	| BOOL		{ $$ = value_type_t::bool_type; }
/*	| COMPLEX
	| IMAGINARY
	| atomic_type_specifier
	| struct_or_union_specifier
	| enum_specifier
	| TYPEDEF_NAME */
	;
/*
struct_or_union_specifier
	: struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';'
	| specifier_qualifier_list struct_declarator_list ';'
	| static_assert_declaration
	;
*/
specifier_qualifier_list
	: type_specifier
/*	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier */
	;
/*
struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: ':' constant_expression
	| declarator ':' constant_expression
	| declarator
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;

atomic_type_specifier
	: ATOMIC '(' type_name ')'
	;

type_qualifier
	: CONST
	| RESTRICT
	| VOLATILE
	| ATOMIC
	;

function_specifier
	: INLINE
	| NORETURN
	;

alignment_specifier
	: ALIGNAS '(' type_name ')'
	| ALIGNAS '(' constant_expression ')'
	;
*/
declarator
	: direct_declarator		{
								strcpy($$.name, $1.name);
								$$.p_arg = $1.p_arg;
							}
/*	: pointer direct_declarator
	| direct_declarator */
	;

direct_declarator
	: IDENTIFIER										{
															strcpy($$.name, yylval.identifier);
															$$.p_arg = nullptr;
														}
/*	| '(' declarator ')'
	| direct_declarator '[' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']' */
	| direct_declarator '(' parameter_type_list ')'		{
															$$ = $1;
															$$.p_arg = $3;
														}
	| direct_declarator '(' ')'							{
															$$ = $1;
															$$.p_arg = nullptr;
														}
/*	| direct_declarator '(' identifier_list ')' */
	;
/*
pointer
	: '*' type_qualifier_list pointer
	| '*' type_qualifier_list
	| '*' pointer
	| '*'
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;
*/

parameter_type_list
	: parameter_list	{ $$ = $1; }
/*	: parameter_list ',' ELLIPSIS
	| parameter_list */
	;

parameter_list
	: parameter_declaration							{
														$$ = new ASTFunctionFormalArguments();
														$$->AddArgument($1);
													}
	| parameter_list ',' parameter_declaration		{
														$1->AddArgument($3);
														$$ = $1;
													}
	;

parameter_declaration
	: declaration_specifiers declarator				{
														$$ = new ASTVarDeclarationStatement($2.name, nullptr);
														$$->SetVarType($1);
													}
/*	| declaration_specifiers abstract_declarator
	| declaration_specifiers */
	;
/*
identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

type_name
	: specifier_qualifier_list abstract_declarator
	| specifier_qualifier_list
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' '*' ']'
	| '[' STATIC type_qualifier_list assignment_expression ']'
	| '[' STATIC assignment_expression ']'
	| '[' type_qualifier_list STATIC assignment_expression ']'
	| '[' type_qualifier_list assignment_expression ']'
	| '[' type_qualifier_list ']'
	| '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' '*' ']'
	| direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list ']'
	| direct_abstract_declarator '[' assignment_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;
*/
initializer
	: assignment_expression
/*	: '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	| assignment_expression */
	;
/*
initializer_list
	: designation initializer
	| initializer
	| initializer_list ',' designation initializer
	| initializer_list ',' initializer
	;

designation
	: designator_list '='
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: '[' constant_expression ']'
	| '.' IDENTIFIER
	;

static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	;
*/
statement
/*	: labeled_statement */
	: compound_statement
	| expression_statement	{ $$ = $1; }
	| selection_statement	{ $$ = $1; }
	| iteration_statement	{ $$ = $1; }
	| jump_statement		{ $$ = $1; }
	;
/*
labeled_statement
	: IDENTIFIER ':' statement
	| CASE constant_expression ':' statement
	| DEFAULT ':' statement
	;
*/
compound_statement
	: '{' '}'						{ $$ = new ASTCompoundStatement(); }
	| '{'  block_item_list '}'		{ $$ = $2; }
	;

block_item_list
	: block_item					{
										$$ = new ASTCompoundStatement();
										p_curr_symbol_table = $$->GetSymbolTable();
										for( auto p_stmt : *($1) )
										{
											$$->AddStatement(p_stmt);
										}
									}
	| block_item_list block_item	{
										for( auto p_stmt : *($2) )
										{
											$1->AddStatement(p_stmt);
										}
										$$ = $1;
									}
	;

block_item
	: declaration	{
						$$ = new StmtVectorT();
						for(auto p_var_decl_stmt : *($1))
						{
							$$->push_back(p_var_decl_stmt);
						}
					}
	| statement		{
						$$ = new StmtVectorT();
						$$->push_back($1);
					}
	;

expression_statement
	: ';'				{ $$ = nullptr; }
	| expression ';'	{ $$ = new ASTExpressionStatement($1); }
	;

selection_statement
	: IF '(' expression ')' statement ELSE statement	{ $$ = new ASTSelectionStatement($3, $5, $7); }
	| IF '(' expression ')' statement					{ $$ = new ASTSelectionStatement($3, $5, nullptr); }
/*	| SWITCH '(' expression ')' statement */
	;

iteration_statement
	: WHILE '(' expression ')' statement											{ $$ = new ASTIterationStatement(nullptr, $3, nullptr, $5); }
/*	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement
	| FOR '(' declaration expression_statement expression ')' statement */
	;

jump_statement
/*	: GOTO IDENTIFIER ';' */
	: CONTINUE ';'				{ $$ = new ASTJumpStatement(jump_type_t::continue_t, nullptr); }
	| BREAK ';'					{ $$ = new ASTJumpStatement(jump_type_t::break_t, nullptr); }
	| RETURN ';'    			{ $$ = new ASTJumpStatement(jump_type_t::return_t, nullptr); }
	| RETURN expression ';'		{ $$ = new ASTJumpStatement(jump_type_t::return_t, $2); }
	;

translation_unit
	: external_declaration						{
													$$ = new ASTFile( recover_filename(yyin) );
													$$->AddDeclaration($1);
													p_root->AddFile($$);
												}
	| translation_unit external_declaration		{ $1->AddDeclaration($2); $$ = $1; }
	;

external_declaration
	: function_definition	{ $$ = $1; }
/*	| declaration */
	;

function_definition
/*	: declaration_specifiers declarator declaration_list compound_statement		{
																					$$ = new ASTFuncDeclarationStatement($2, $1, $4);
																				} */
	: declaration_specifiers declarator compound_statement						{
																					string str($2.name);
																					$$ = new ASTFuncDeclarationStatement(str, $1, $2.p_arg, $3);
																				}
	;
/*
declaration_list
	: declaration
	| declaration_list declaration
	;
*/
%%
#include <stdio.h>
void yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", s);
}

string recover_filename(FILE * f) 
{
	int fd;
	char fd_path[255];
	char * filename = (char*)malloc(255);
	ssize_t n;
	
	fd = fileno(f);
	sprintf(fd_path, "/proc/self/fd/%d", fd);  
	n = readlink(fd_path, filename, 255);
	if (n < 0)
		return NULL;
	filename[n] = '\0';
	string fname(filename);
	return fname;
}

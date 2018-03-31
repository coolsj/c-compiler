#include <vector>
#include "ASTNodes.h"

struct direct_declarator_t
{
	char* name;
	ASTFunctionFormalArguments* p_arg;
};

typedef vector<ASTVarDeclarationStatement*> VarDeclStmtVectorT;
typedef vector<ASTStatement*> StmtVectorT;
typedef vector<ASTExpression*> ExprVectorT;

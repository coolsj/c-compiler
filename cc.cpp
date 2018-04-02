#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "parser.hpp"
#include "ASTNodes.h"
#include "CodeGen.h"

extern "C" int yylex();
int yyparse();
extern FILE *yyin;
ASTProgramRoot* p_root;

static void usage()
{
	printf("Usage: cc <prog.c>\n");
}

int main(int argc, char **argv)
{
	if (argc != 2) {
		usage();
		exit(1);
	}
	char const *filename = argv[1];
	yyin = fopen(filename, "r");
	assert(yyin);
	p_root = new ASTProgramRoot;
	int ret = yyparse();
	if( p_root )
	{
		//p_root->PrintNode();
		CodeGen(p_root);
		delete p_root;
	}
	//printf("retv = %d\n", ret);
	exit(0);
}

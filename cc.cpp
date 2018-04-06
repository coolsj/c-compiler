#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <cstring>
#include "parser.hpp"
#include "ASTNodes.h"
#include "CodeGen.h"

extern "C" int yylex();
int yyparse();
extern FILE *yyin;
ASTProgramRoot* p_root;
bool optimize = false;

static void usage()
{
	printf("Usage: cc <prog.c> [-o]\n");
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		usage();
		exit(1);
	}
	char const *filename = argv[1];
	yyin = fopen(filename, "r");
	assert(yyin);
	p_root = new ASTProgramRoot;
	if( argc == 3 && strcmp( argv[2], "-o" ) != -1 )
			optimize = true;
	int ret = yyparse();
	if( p_root )
	{
		p_root->PrintNode();
		CodeGen(p_root);
		delete p_root;
	}
	//printf("retv = %d\n", ret);
	exit(0);
}

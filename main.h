#pragma once
#include "type.h"
#include "semantics.h"
#include "parser.h"
#include "codegen.h"
#include "compile.h"
#include "run_test.h"
#include "exprblock.h"


struct Option{
	char name;int clear;int set; const char* help;
};
extern Option g_Options[];
void dump_help();

enum COMPILE_FLAGS {
	B_AST=0x0001,B_DEFS=0x0002,B_GENERICS=0x0004, B_TYPES=0x0008,B_LLVM=0x0010,B_EXECUTABLE=0x0020,B_RUN=0x0040,B_VERBOSE=0x0080
};

int compile_source_file(const char* filename, int options);

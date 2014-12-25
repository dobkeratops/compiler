#pragma once
#include "codegen.h"
#include "error.h"
#include "exprblock.h"
#include "exprstructdef.h"
#include "exprfndef.h"
#include "exprflow.h"

enum{
	EMIT_FN=0x0001,EMIT_STRUCT=0x0002,EMIT_GLOBALS=0x0004,EMIT_ALL=EMIT_FN|EMIT_STRUCT|EMIT_GLOBALS
};
void output_code(FILE* ofp, Scope* scope, int depth=0,int flags=EMIT_ALL);
void name_mangle(char* dst, int size, const ExprStructDef* src);
void name_mangle(char* dst, int size, const ExprFnDef* src);
inline void dbg_mangle(const char*,...){}

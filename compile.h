#pragma once
#include "codegen.h"
#include "error.h"

void output_code(FILE* ofp, Scope* scope, int depth);
void name_mangle(char* dst, int size, const ExprStructDef* src);
void name_mangle(char* dst, int size, const ExprFnDef* src);
inline void dbg_mangle(const char*,...){}

#pragma once
#include "hack.h"

void output_code(FILE* outfile, Scope* scope);
void name_mangle(char* buffer, int size, const ExprFnDef* f);
void name_mangle(char* buffer, int size, const ExprStructDef* f);
void name_mangle(char* buffer, int size, const Type* f);
void name_mangle_append_scope(char* buffer, int size, const Scope* s);
char* name_mangle_append_name(char* buffer, int size, Name n);

class CgValue;

class CodeGen {
public:
	FILE* ofp;
	int next_reg;
};
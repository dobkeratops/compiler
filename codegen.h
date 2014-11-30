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
	CodeGen(FILE* _ofp, int nr){
		ofp=_ofp; next_reg=nr;
		comma=0;
		depth=0;
		curr_fn=0;
	}
	char comma;
	int depth;
	bool commas[32];
	vector<Node*> compile_later;
	ExprFnDef*	curr_fn;	// The current function being compiled - no nesting allowed. (defer with 'compile_later')
};
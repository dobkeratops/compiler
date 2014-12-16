#include "ast.h"
#include "compile.h"
#include "exprblock.h"
#include "exprstructdef.h"
#include "exprfndef.h"
#include "exprflow.h"

/// compile methods from various types.

// TODO: properly abstract llvm instruction generation to move to llvm api.
using std::function;

void debug_op(Name opname) {
}

Name reg_of(Node* n, ExprFnDef* owner) {
	char tmp[32];
	if (auto p=dynamic_cast<ExprIdent*>(n)){
		// TODO - check 'this',lambda locals
		return p->name;
	}
//	TODO need to verify that there are NO clashes
// todo-can we just cache this on the node itself?
	auto m=((uint32_t)
			 (size_t)n);
	sprintf(tmp,"n%x", ((m>>2) ^ (m>>18))&0xffff);
	return getStringIndex(tmp,0);
}
Scope* g_Sc;

void name_mangle_append_segment(char* dst, int size, const char* src){
	auto len=strlen(src);
	dst+=strlen(dst);
	sprintf(dst,"%lu",len);
	strcat(dst,src);
}

//UINT,SIZE_T,I8,I16,I32,I64,U8,U16,U32,U64,U128,BOOL,	// int types
//HALF,FLOAT,DOUBLE,FLOAT4,CHAR,STR,VOID,VOIDPTR,

const char* g_mangle_type[]={
	"i","u","z","c","s","l","ll","uc","us","ul","ull","","b",
	"h","f","d","6float4","c","Pc","v","Pv",
};
void name_mangle_append_type(char* dst,int size, const Type* t){
	if (!t) return;
		// todo - check how template params are suppsoed to mangle
		// we suspect the template params cover this... fn's params are mangled and this should just be struct->name
		//name_mangle_append_name(dst,size,t->struct_def->get_mangled_name());

	auto n=t->name;
	if (n==PTR){ strcat(dst,"P");}
	else if (n>=RAW_TYPES && n<=VOIDPTR) {
		strcat(dst,g_mangle_type[(int)n-RAW_TYPES]);
	}
	else if (auto sd=t->get_struct_autoderef()){
		name_mangle_append_segment(dst,size,str(sd->get_mangled_name()));
		for (auto& it:sd->instanced_types){
			dbg_mangle(" %s\n",it->name_str());
			name_mangle_append_type(dst,size,it);
		}
	}
	else {name_mangle_append_segment(dst, size, str(t->name));}
	
	for (auto ts=t->sub;ts;ts=ts->next){
		name_mangle_append_type(dst,size,ts);
	}
}
void name_mangle(char* dst, int size, const ExprFnDef* src) {
	dst[0]=0;
	// TODO - prefix scopes. Now, Overloading is the priority.
	// todo - check how template params are suppsoed to mangle
	sprintf(dst,"_Z");dst+=2;
	size_t len=strlen(dst); size--; size-=len;
	name_mangle_append_segment(dst, size, symbol_of(src->name));
	for (auto a:src->args){
		name_mangle_append_type(dst,size, a->type());
	}
}
void name_mangle(char* dst, int size, const ExprStructDef* src) {
	dst[0]=0;
	// TODO - prefix scopes. Now, Overloading is the priority.
	sprintf(dst,"_Z");dst+=2;
	size_t len=strlen(dst); size--; size-=len;
	name_mangle_append_segment(dst, size, str(src->name));

	for (auto& it:src->instanced_types){
		name_mangle_append_type(dst,size,it);
	}
}
void output_code(FILE* ofp, Scope* scope, int depth) {
	verify_all();
	auto cg=CodeGen(ofp,0);
	if (!depth)
		cg.emit_prelude();

	cg.emit_comment("from scope %s\n;",scope->name());
	// output all inner items that outer stuff depends on..
	// literals first, because we setup llvm_strlen. TODO , more solid design pls.
	for (auto l=scope->literals; l; l=l->next_of_scope) {
		if (l->type_id==T_CONST_STRING){
//			const char* name=getString(l->name);
			l->llvm_strlen=cg.emit_global_string_literal(l->name, l->as_str());
		}
	}
	for (auto n=scope->named_items;n;n=n->next) {
		for (auto s=n->structs; s;s=s->next_of_name) {
			s->compile(cg, scope);
		}
	}
	for (auto n=scope->named_items;n;n=n->next) {
		for(auto f=n->fn_defs; f; f=f->next_of_name){
			f->compile(cg,scope);
		}
	}
	// compile child items last, as they depend on me.
	for (auto sub=scope->child; sub; sub=sub->next) {
		output_code(cg.ofp,sub,depth+1);
	}
}




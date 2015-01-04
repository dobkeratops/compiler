#pragma once

#include "stringtable.h"
//#include "node.h"
#include "exprfndef.h"

struct Type;
struct TParamDef;
struct Expr;
struct Name;
// type inference
struct CaptureVars;


ResolveResult resolve_make_fn_call(Expr* receiver,ExprBlock* block/*caller*/,Scope* scope,const Type* desired,int flags) ;
ResolveResult resolve_make_fn_call(Expr* receiver,ExprBlock* block,Scope* scope,const Type* desired);
struct Call;
struct FnName;

/// 'StructDef' handles everything for struct,trait,impl,vtable class,mod/namespace,
///
/// specific types derived expose a subset as language sugar.
/// a transpiler should handle conversions eg spot a 'struct' with pure virtuals -> trait, etc.

/// TODO a Rust Enum is sugar for a struct holding constants & derived variant structs.


typedef Type TParamVal;
struct TParamXlat{
	const MyVec<TParamDef*>& tparams; const MyVec<TParamVal*>& given_types;
	TParamXlat();
	TParamXlat(	const MyVec<TParamDef*>& t, const MyVec<TParamVal*>& g):tparams(t),given_types(g){}
	bool typeparams_all_set()const{
		for (int i=0; i<given_types.size(); i++) {
			if (given_types[i]==0) return false;
		}
		return true;
	}
	int typeparam_index(const Name& n) const;
	void dump(PrinterRef depth)const;
};

struct FindFunction {
	struct Candidate{ExprFnDef* f; int score;};
	MyVec<Candidate> candidates;
	Name			name;
	const Expr* 	callsite;
	int 			flags;
	bool 			verbose=false;
	int				max_candidates=5;
	const MyVec<Expr*>& args;
	const Type* 	ret_type;
	FindFunction(Name n, const MyVec<Expr*>& a, const Type* r,int f):name(n),args(a),ret_type(r),flags(f){}
	
	void consider_candidate(ExprFnDef* f);
	void find_fn_sub(Expr* src);
	void find_fn_from_scopes(Scope* s,Scope* ex);
	void insert_candidate(ExprFnDef* f,int score);
	void dump();
};
int match_fn_tparams(MyVec<TParamVal*>& matched, const ExprFnDef* f, const MyVec<Expr*>& args, const Expr* callsite);
int match_struct_tparams(MyVec<TParamVal*>& matched, const ExprStructDef* sd, const MyVec<Expr*>& field_inits, const Expr* callsite);
int match_tparams(MyVec<TParamVal*>& matched, const MyVec<ArgDef*>& arg_fmt,const Type* ret_type,const MyVec<TParamDef*>& tparams, const MyVec<Expr*>& given_args,int first_arg_index, const Expr* callsite,bool variadic);

void fill_given_tparams(MyVec<TParamVal*>& matched, const MyVec<TParamDef*>& arg_fmt, const MyVec<TParamVal*>& given_types);









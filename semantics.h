#pragma once

#include "stringtable.h"
struct Type;
struct TParamDef;
struct Expr;
struct Name;
// type inference
ResolveResult propogate_type_refs(int flags,const Node*n, Type*& a,Type*& b);
ResolveResult propogate_type_refs(int flags, Expr *n, Type*& a,Type*& b);
ResolveResult propogate_type_fwd(int flags,const Node* n, const Type* a,Type*& b);
ResolveResult propogate_type_fwd(int flags,Expr* e, const Type*& a);
ResolveResult propogate_type_expr_ref(int flags,Expr* e, Type*& a);
ResolveResult propogate_type_refs(int flags,const Node* n, Type*& a,Type*& b,Type*& c);
ResolveResult propogate_type_fwd(int flags,const Node* n,const Type*& a,Type*& b,Type*& c);

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


struct StructInitializer{ // named initializer
	ExprBlock*		si; // struct_intializer
	Scope*			sc;
	ExprStructDef*	struct_def=0;
	ExprStructDef*	get_struct_def(){return struct_def;}
	vector<int>		field_indices;
	vector<ArgDef*> field_refs;
	vector<Expr*>	value;
	void map_fields()								{resolve(nullptr,0);}//todo..seperate out}
	StructInitializer(Scope* s,ExprBlock* block)	{si=block,sc=s;};
	ResolveResult resolve(const Type* desiredType,int flags);
};

typedef Type TParamVal;
struct TypeParamXlat{
	const vector<TParamDef*>& typeparams; const vector<TParamVal*>& given_types;
	TypeParamXlat();
	TypeParamXlat(	const vector<TParamDef*>& t, const vector<TParamVal*>& g):typeparams(t),given_types(g){}
	bool typeparams_all_set()const{
		for (int i=0; i<given_types.size(); i++) {
			if (given_types[i]==0) return false;
		}
		return true;
	}
	int typeparam_index(const Name& n) const;
	void dump(int depth)const;
};

struct FindFunction {
	struct Candidate{ExprFnDef* f; int score;};
	vector<Candidate> candidates;
	Name			name;
	const Expr* 	callsite;
	int 			flags;
	bool 			verbose=false;
	int				max_candidates=5;
	const vector<Expr*>& args;
	const Type* 	ret_type;
	FindFunction(Name n, const vector<Expr*>& a, const Type* r,int f):name(n),args(a),ret_type(r),flags(f){}
	
	void consider_candidate(ExprFnDef* f);
	void find_fn_sub(Expr* src);
	void find_fn_from_scopes(Scope* s,Scope* ex);
	void insert_candidate(ExprFnDef* f,int score);
	void dump();
};
int match_typeparams(vector<Type*>& matched, const ExprFnDef* f, const vector<Expr*>& args, const Expr* callsite);










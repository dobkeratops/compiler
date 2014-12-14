#pragma once
#include "everywhere.h"
#include "ast.h"

// type inference
ResolvedType propogate_type(int flags,const Node*n, Type*& a,Type*& b);
ResolvedType propogate_type(int flags, Expr *n, Type*& a,Type*& b);
ResolvedType propogate_type_fwd(int flags,const Node* n, const Type* a,Type*& b);
ResolvedType propogate_type_fwd(int flags,Expr* e, const Type*& a);
ResolvedType propogate_type(int flags,Expr* e, Type*& a);
ResolvedType propogate_type(int flags,const Node* n, Type*& a,Type*& b,Type*& c);
ResolvedType propogate_type_fwd(int flags,const Node* n,const Type*& a,Type*& b,Type*& c);
ResolvedType propogate_type(int flags,const Node* n, ResolvedType& a,Type*& b);
ResolvedType propogate_type(int flags,const Node* n,ResolvedType& a,Type*& b,const Type* c);

struct CaptureVars;
// load data->vtb // if this matters it would be inlined
// load vtb->fn
// when the time comes - vtb->destroy()
//                       vtb->trace

/// TODO-Anything matchable by the template engine eg Type, Constants, Ident.. (how far do we go unifying templates & macros..)

/// CaptureVars of local variables for a lambda function
/// hidden entity created in resolve. compile to 'C' might roll these manually?
struct CaptureVars : ExprDef{
	Name			tyname(){return name;};
	ExprFnDef*		capture_from=0;
	ExprFnDef*		capture_by=0;
	Variable*		vars=0;
	CaptureVars*		next_of_from=0;
	ExprStructDef*	the_struct=0;
	void 			coalesce_with(CaptureVars* other);
	ExprStructDef*	get_struct();
	CgValue			compile(CodeGen& cg, Scope* outer);
	Node* clone() const override{
		dbprintf("warning todo template instatntiation of captures\n");
		return nullptr;
	};
	Type*			get_elem_type(int i);
	Name			get_elem_name(int i);
	int				get_elem_count();
};


ResolvedType resolve_make_fn_call(Expr* receiver,ExprBlock* block/*caller*/,Scope* scope,const Type* desired,int flags) ;
ResolvedType resolve_make_fn_call(Expr* receiver,ExprBlock* block,Scope* scope,const Type* desired);
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
	vector<int>		field_indices;
	vector<ArgDef*> field_refs;
	vector<Expr*>	value;
	void map_fields()								{resolve(nullptr,0);}//todo..seperate out}
	StructInitializer(Scope* s,ExprBlock* block)	{si=block,sc=s;};
	ResolvedType resolve(const Type* desiredType,int flags);
};


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










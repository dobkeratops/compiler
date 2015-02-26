#pragma once
#include "semantics.h"
#include "exprfndef.h"
#include "type.h"


struct NamedItems {		// everything defined under a name
	Scope* owner=0;
	Name		name;
	NamedItems*		next=0;
	Type*		types=0;
	ExprFnDef*	fn_defs=0;
	ExprStructDef*	structs=0;	// also typedefs?
	ArgDef*		fields=0;		// also fields..

	ExprFnDef*	getByName(Name n);
	//	ExprFnDef* resolve(Call* site);
	NamedItems(Name n,Scope* s){  name=n; owner=s;next=0;fn_defs=0;structs=0;types=0;}
};

struct Use : public Node {	// symbol aliasing
	Vec<Name>	path;
	Vec<Name>	symbols;
	Name use_as;
	TypeDef*	clone()const;
	void	dump(PrinterRef depth) const;
	ResolveResult	resolve(Scope* sc, const Type* desired, int flags) override;
};
struct Import : public Node {	// 'use mod' to retrofit into Rust-like syntax
	Name use_as;				// 'use mod <relative_path> as <prefix>
	TypeDef*	clone()const;
	void	dump(PrinterRef depth) const;
};

/// 'Scope'-
/// scopes are created when resolving, held on some node types.
/// blocks which can add locals or named entities have them.
struct Scope {
	ExprDef*	owner_fn=0;	// TODO: eliminate this, owner might be FnDef,Struct,ExprBlock
	Expr*		m_node=0;
	Scope*		parent=0;
	Scope*		next=0;
	Scope*		child=0;
	Scope*		global=0;
	Scope*		capture_from=0;	// when resolving a local/lambda function, this is the owners' stack frame, look here for capturevars
	ExprLiteral*	literals=0;
	//Call* calls;
	Variable*	vars=0;
	NamedItems*	named_items=0;
	ExprFnDef*	templated_name_fns=0;// eg  fn FNAME[T,FNAME](x:T,y:T)->{...}  if the signature matches anything.. its' used. idea for implementing OOP & variants thru templates..
	// locals;
	// captures.
	const char* name_str()const;
private:
	Scope(){named_items=0; owner_fn=0;m_node=0;parent=0;next=0;child=0;vars=0;global=0;literals=0;}
public:
	Scope(Scope* p){ASSERT(p==0);named_items=0; owner_fn=0;m_node=0;parent=0;next=0;child=0;vars=0;global=0;literals=0;}
	void visit_calls();
	MyVec<TParamDef*>* get_tparams(){return owner_fn?owner_fn->get_typeparams():nullptr; // TODO: actually need to cascade them.
	}
	TParamDef*	get_typeparam_for(Type *t);
	bool		is_typeparam(Type* t) {return get_typeparam_for(t)!=nullptr;}
	ExprStructDef*	get_receiver();
	Variable*	try_capture_var(Name ident);	//sets up the capture block ptr.
	Variable*	find_fn_variable(Name ident,ExprFnDef* f);
	Variable*	get_fn_variable(Name name,ExprFnDef* f);
	Variable*	find_variable_rec(Name ident);
	Variable*	find_scope_variable(Name ident);
	Variable*	create_variable(Node* n, Name name,VarKind k);
	Variable*	get_or_create_scope_variable(Node* creator,Name name,VarKind k);
	ExprStructDef*	try_find_struct(const Type* t){return this->find_struct_sub(this,t);}
	ExprStructDef*	find_struct_of(const Expr* srcloc);
	Scope*			parent_within_fn(){if (!parent) return nullptr; if (parent->owner_fn!=this->owner_fn) return nullptr; return parent;}
	void			visit_named_items(Name n,std::function<void(NamedItems* n,Scope* sc)> f,Scope* ignore=0);
	ExprStructDef*	find_struct_sub_if(Scope* original,const Type* t){if (t) return find_struct_sub(original,t);else return nullptr;}
	ExprStructDef* find_inner_def(Scope* original,const Node* idn, const Type* t,int flags);
	ExprStructDef*	find_inner_def_named(Scope* original,Node* id_name,int flags);
	ExprStructDef*	find_inner_def_depth(Scope* original, const Node* nm,const Type* t,int depth);

	ExprStructDef*	find_struct_name_type_if(Scope* original,const Node* name,const Type* t){
		if (t)
			return find_struct_name_type(original,name,t);
		else
			return nullptr;
	}
	ExprStructDef*	find_struct_name_type(Scope* original,const Node* name,const Type* t);
	ExprStructDef*	find_struct_sub(Scope* original,const Type* t);
	ExprStructDef*	find_struct_named(Name name);
	ExprStructDef*	find_struct_named(const Node* node){return find_struct_named(node->as_name());}
	ExprStructDef*	find_struct(const Node* node);
	ExprStructDef*	find_struct_type(const Node* node,const Type* t);
	ExprFnDef*	find_unique_fn_named(const Node* name_node,int flags=0, const Type* fn_type=nullptr); // todo: replace with fn type.
	ExprFnDef*	find_fn(Name name,const Expr* callsite, int numrecv,const MyVec<Expr*>& args,const Type* ret_type,int flags);
	ExprFnDef*	find_fn_sub(Name name,const Expr* callsite, int numrecv,const MyVec<Expr*>& args,const Type* ret_type,int flags);
	ExprFnDef*	find_fn_for_types(Name name, int numrecv,const Type* arg0_type,const Type* arg1_type, const Type* ret_type,int flags);
	void	add_struct(ExprStructDef*);
	void	add_fn(ExprFnDef*,bool assert=false);
	void	add_fn_def(ExprFnDef*);
	void	dump(PrinterRef depth) const;
	NamedItems* find_named_items_local(Name name);
	NamedItems* get_named_items_local(Name name);
	NamedItems* find_named_items_rec(Name name);
	void		resolve();
	Expr*		current_loop(int levels);
	ExprStructDef* owner_struct() { return this->owner_fn?this->owner_fn->as_struct_def():nullptr;}
private:
	ExprFnDef*	get_owner_fn(){return this->owner_fn?this->owner_fn->as_fn_def():nullptr;}
	void push_child(Scope* sub) { sub->owner_fn=this->owner_fn; sub->next=this->child; this->child=sub;sub->parent=this; sub->global=this->global;}
public:
	void	compile_destructors(CodeGen& gc);
	void	compile_destructors_if(CodeGen& cg){if (this){
		this->compile_destructors(cg);}}
	Scope* parent_or_global()const{
		if (!this) return nullptr;
		if (parent) return this->parent; else if (global && global!=this) return this->global; else return nullptr;
	}
	Scope* make_inner_scope(Scope** pp_scope,ExprDef* owner,Expr* sub_owner);
};

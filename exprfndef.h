#pragma once
#include "type.h"
#include "ast.h"
#include "exprblock.h"
#include "scope.h"
#include "codegen.h"
#include "exprstructdef.h"

// todo.. generic instantiation: typeparam logic, and adhoc mo
struct CodeGen;
struct  ExprFnDef : ExprDef {
	ExprFnDef*	next_of_module=0; // todo: obsolete this.
	ExprFnDef*	next_of_name=0;	//link of all functions of same name...
	ExprFnDef*	instance_of=0;	// Original function, when this is a template instance
	ExprFnDef*	instances=0;		// Linklist of it's instanced functions.
	ExprFnDef*	next_instance=0;
	ExprFnDef*	next_of_capture=0;	// one capture can be shared by multiple fn
	ExprBlock*	callers=0;			// linklist of callers to here
	NamedItems*	name_ptr=0;
	Scope*		scope=0;
	CaptureVars*	captures=0;			//
	CaptureVars*	my_capture=0;			// for closures- hidden param,environment struct passed in
	ExprStructDef* m_receiver=0;
	Type*		self_t=0;			// also type of receiver.
	int8_t		vtable_index=-1;
	int8_t		vtable_param=0;		// which parameter dispatches
	int8_t		num_prefix_args=0;	// 0- foo(a,b,c) 1- a.foo(b,c)
	bool variadic;
	bool c_linkage=false;
	bool m_closure=false;
	
	Type* ret_type=0;
	Type* fn_type=0;				// eg (args)->return
	
	Name mangled_name=0;
	vector<TParamDef*> tparams;
	vector<Type*>		instanced_types;
	vector<ArgDef*> args;
	Expr* body=0;
	ExprFnDef(){};
	ExprFnDef(SrcPos sp,Name n)	{name=n;pos=sp;variadic=false;scope=0;next_of_module=0;next_of_name=0;instance_of=0;instances=0;next_instance=0;body=0;callers=0;fn_type=0;ret_type=0;name_ptr=0;}
	ExprFnDef(SrcPos sp):ExprFnDef(sp,0){};
	void			set_receiver_if_unset(ExprStructDef* sd); // not sure if it'll be arbitrary type
	ExprStructDef*	get_receiver();
	int		get_name()const {return index(name);}
	Name	get_mangled_name()const;
	bool	is_generic() const;
	bool	is_closure() const			{return my_capture!=0 || m_closure;}
	void	dump_signature() const;
	int		type_parameter_index(Name n) const;
	int		min_args();//					{for (int i=0; i<args.size();i++){if (args[i]->default_expr) return i;} return (int)args.size();}
	int 	max_args()					{return variadic?1024:(int)args.size();}
	bool	is_enough_args(int x)		{if (x<min_args()) return false; if (x> args.size() && !variadic) return false; return true;}
	bool	too_many_args(int x)		{return x>max_args();}
	const char*		kind_str()const	override{return"fn";}
	ExprFnDef* 		as_fn_def() override{return this;}
	const ExprFnDef* 		as_fn_def() const override{return this;}
	Node*			instanced_by()const{if (this->instance_of){return this->refs;}else return nullptr;}
	void			dump(PrinterRef ind) const;
	void			dump_sub(int ind,Name prefix) const;
	ResolveResult	resolve_function(Scope* definer,ExprStructDef* receiver, const Type* desired, int flags);
	ResolveResult	resolve(Scope* scope,const Type* desired,int flags);
	ResolveResult	resolve_call(Scope* scope,const Type* desired,int flags);
	CaptureVars*		get_or_create_capture(ExprFnDef* src);
	void			translate_tparams(const TParamXlat& tpx)override;
	vector<TParamDef*>* get_typeparams() override{return &this->tparams;}
	Expr*			last_expr()const;
	const Expr*		get_return_expr() const override;
	Type*				return_type()const {
		auto x=get_return_expr(); if (x){if (auto xt=x->get_type()) return xt;}
		return this->ret_type;
	}
	void	clear_return_type(){	// any change to type.. must recalc
		this->fn_type=nullptr;
		this->ret_type=nullptr;
	}
	bool	has_return_value() const;
	Node*	clone() const;
	bool	is_undefined()const	{return body==nullptr || body->is_undefined();};
	bool	is_extern()const	{return body==nullptr;}
	void	verify();
	CgValue compile(CodeGen& cg, Scope* sc, CgValue input) override;
	virtual Scope*	get_scope()				{return this->scope;}
	void		recurse(std::function<void(Node*)>&) override;
	// helpers for building functions
	ExprCompound*	convert_body_to_compound();
	void	push_body(Expr* e,bool front);
	void 	push_body_back(Expr* e)	{push_body(e,false);}
	void 	push_body_front(Expr* e){push_body(e,true);}
};
CgValue compile_function_call(CodeGen& cg, Scope* sc,CgValue recvp, const Expr* receiver, const ExprBlock* e);



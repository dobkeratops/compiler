#pragma once
// todo.. generic instantiation: typeparam logic, and adhoc mo
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
	int8_t		vtable_index=-1;
	int8_t		vtable_param=0;		// which parameter dispatches
	int8_t		num_prefix_args=0;	// 0- foo(a,b,c) 1- a.foo(b,c)
	bool variadic;
	bool c_linkage=false;
	bool m_closure=false;
	bool resolved;
	
	Type* ret_type=0;
	Type* fn_type=0;				// eg (args)->return
	
	Name mangled_name=0;
	vector<TParamDef*> typeparams;
	vector<ArgDef*> args;
	Expr* body=0;
	ExprFnDef(){};
	ExprFnDef(SrcPos sp)	{pos=sp;variadic=false;scope=0;resolved=false;next_of_module=0;next_of_name=0;instance_of=0;instances=0;next_instance=0;name=0;body=0;callers=0;fn_type=0;ret_type=0;name_ptr=0;}
	void			set_receiver_if_unset(ExprStructDef* sd); // not sure if it'll be arbitrary type
	ExprStructDef*	get_receiver();
	int		get_name()const {return index(name);}
	Name	get_mangled_name()const;
	bool	is_generic() const;
	bool	is_closure() const			{return my_capture!=0 || m_closure;}
	void	dump_signature() const;
	int		type_parameter_index(Name n) const;
	int		min_args()					{for (int i=0; i<args.size();i++){if (args[i]->default_expr) return i;} return (int)args.size();}
	int 	max_args()					{return variadic?1024:(int)args.size();}
	bool	is_enough_args(int x)		{if (x<min_args()) return false; if (x> args.size() && !variadic) return false; return true;}
	bool	too_many_args(int x)		{return x>max_args();}
	const char*		kind_str()const	override{return"fn";}
	ExprFnDef* 		as_fn_def() override{return this;}
	const ExprFnDef* 		as_fn_def() const override{return this;}
	Node*			instanced_by()const{if (this->instance_of){return this->refs;}else return nullptr;}
	void			dump(int ind) const;
	void			dump_sub(int ind,Name prefix) const;
	ResolvedType	resolve_function(Scope* definer,ExprStructDef* receiver, const Type* desired, int flags);
	ResolvedType	resolve(Scope* scope,const Type* desired,int flags);
	ResolvedType	resolve_call(Scope* scope,const Type* desired,int flags);
	CaptureVars*		get_or_create_capture(ExprFnDef* src);
	void			translate_typeparams(const TypeParamXlat& tpx)override;
	vector<TParamDef*>* get_typeparams() override{return &this->typeparams;}
	Expr*			last_expr()const{
		if (auto b=body->as_block()) return b->argls.back();
		else return body;
	}
	Expr*			get_return_value() const;
	Type*				return_type()const {
		auto x=get_return_value(); if (auto xt=x->get_type()) return xt;
		return this->ret_type;
	}
	bool	has_return_value() const{
		if (auto r=return_type()){
			return index(r->name)!=VOID;}
		else return false;
	}
	Node*	clone() const;
	bool	is_undefined()const	{return body==nullptr || body->is_undefined();};
	bool	is_extern()const	{return body==nullptr;}
	void	verify();
	CgValue compile(CodeGen& cg, Scope* sc);
	virtual Scope*	get_scope()				{return this->scope;}
};


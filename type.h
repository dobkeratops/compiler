#pragma once

#include "stringtable.h"
struct Name;
struct Type : ExprDef {
	vector<TParamDef*> typeparams;
	//ExprDef* struct_def=0;	// todo: struct_def & sub are mutually exclusive.
	Type*	sub=0;					// a type is itself a tree
	Type*	next=0;
	Node* 	m_origin=0;				// where it comes from
	bool	tparam_index=-1;
	bool	is_generic(){return tparam_index>=0;}
	bool	has_non_instanced_typeparams()const;
	
	void set_origin(Node* t){m_origin=t;}
	Node* get_origin()const {return m_origin;}
	void push_back(Type* t);
	virtual const char* kind_str()const;
	Type(Node* origin, Name outer, Type* inner):Type(origin,outer){ push_back(inner);}
	Type(Node* origin,Name a,Name b): Type(origin,a, new Type(origin,b)){}
	Type(Node* origin,Name a,Name b,Name c): Type(origin,a,new Type(origin,b,c)){}
	//		auto tc=new Type(origin,c); auto tb=new Type(origin,b); tb->push_back(tc); push_back(tb);
	//	}
	Type(ExprStructDef* sd);
	Type(Name outer, ExprStructDef* inner);
	Type(Node* origin,Name i);
	Type(Name i,SrcPos sp);
	Type() { name=0;sub=0;next=0;}
	size_t	alignment() const;
	size_t	size() const;
	int	raw_type_flags()const	{int i=index(name)-RAW_TYPES; if (i>=0&&i<NUM_RAW_TYPES){return g_raw_types[i];}else return 0;}
	bool	is_int()const		{return raw_type_flags()&RT_INTEGER;}
	bool	is_float()const		{return raw_type_flags()&RT_FLOATING;}
	bool	is_signed()const	{return raw_type_flags()&RT_SIGNED;}
	bool	is_register()const	{return !is_complex();}
	bool	is_anon_struct()const	{return !this->name && this->def;}
	bool	is_complex()const;
	bool	is_struct()const;
	bool	is_bool()const		{return name==BOOL;}
	bool	is_callable()const	{return name==FN||name==CLOSURE;}
	bool	is_qualifier()const	{return name==CONST||name==MUT;}
	bool	is_qualifier_or_ptr_or_ref()const{return is_qualifier()||is_pointer_or_ref();}
	struct FnInfo{ Type* args; Type* ret;Type* receiver;};
	FnInfo	get_fn_info(){
		if (!is_callable()) return FnInfo{nullptr,nullptr,nullptr};
		auto a=this->sub; auto ret=a->next; auto recv=ret?ret->next:nullptr;
		return FnInfo{a,ret,recv};
	}
	ExprStructDef* get_receiver()const;
	// we have a stupid OO receiver because we want C++ compatability;
	// we can use it for lambda too. We will have extention methods.
	void	set_fn_details(Type* args,Type* ret,ExprStructDef* rcv){
		ASSERT(this->is_callable());
		ASSERT(this->sub==0);
		this->push_back(args);
		this->push_back(ret);
		if (rcv) {
			ASSERT(args&&ret);
			this->push_back(new Type(rcv));
		}
	}
	
	Name array_size()const{
		ASSERT(this->sub);
		return this->sub->next->name;
	}
	//	bool	is_ptr_to(const Type* other){return ((this->type==PTR) && this->sub->eq(other));}
	//	bool	is_void_ptr()const	{if (this->type==VOIDPTR)return true;if(this->type==PTR && this->sub){if(this->type->sub==VOID) return true;}return false;};
	
	bool		has_typeparam(Scope* sc);
	bool 		is_typeparam(Scope* sc) const;
	Type*	get_elem(int index){auto r=const_cast<const Type*>(this)->get_elem(index); return const_cast<Type*>(r);}
	const Type*		get_elem(int index)const;
	const Type*		get_elem_type(int index) const{
		return get_elem(index);
	}
	int			num_pointers()const;
	int			num_pointers_and_arrays()const;
	ExprStructDef*	struct_def_noderef()const;
	ExprStructDef*	get_struct_autoderef() const; // with autoderef
	bool			is_coercible(const Type* other,Name self_t=0) const{return is_equal(other,true,self_t);};
	bool			is_equal(const Type* other,bool coerce=false,Name self_t=0) const;
	bool			is_equal(const Type* other, const TypeParamXlat& xlat ,Name self_t=0)const;
	bool			is_equal_sub(const Type* other,bool coerce=false,Name self_t=0) const;
	bool			is_equal_sub(const Type* other, const TypeParamXlat& xlat ,Name self_t=0)const;
	// todo 'is_equal' rename to iscompatible, is_equal/coercible call it with flags
	int				is_equal_or_coercible(const Type* other, Name self_t=0) const{
		if (is_equal(other,false,self_t)) return 10;
		else if (is_coercible(other,self_t)) return 1;
		else return 0;
	}
	bool			is_compatible(const Type* other,bool coerce=false) const;
	bool			is_auto(){return !this || this->name==AUTO;}
	void			dump_sub(int f
							 )const;
	void			dump(int depth)const;
	static Type*	get_auto();
	static Type*	get_bool();
	static Type*	get_void();
	static Type*	get_void_ptr();
	static Type*	get_int();
	Node*	clone() const;
	void	set_struct_def(ExprStructDef* sd);
	void	clear_struct_def();
	bool	is_array()const		{return name==ARRAY;}
	bool	is_template()const	{ return sub!=0;}
	bool	is_function() const	{ return name==FN;}
	bool	is_closure() const	{ return name==CLOSURE;}
	Type*	fn_return() const	{ if (is_callable()) return sub->next; else return nullptr;}
	Type*	fn_args_first() const		{ return sub->sub;} 	void	clear_reg()			{reg_name=0;};
	const Type*	strip_qualifiers()const{
		auto p=this;
		if(!p) return nullptr;
		while (p->is_qualifier()){p=p->sub;}
		return p;
	};
	bool	is_name(int n1)	const{return this->name==n1;}
	bool	is_name(int n1, int n2)const{return this->name==n1||this->name==n2;}
	bool	is_name(int n1, int n2,int n3)const	{return this->name==n1||this->name==n2||this->name==n3;}
	bool	is_pointer_or_ref()const		{return this->strip_qualifiers()->is_name(PTR,REF);}
	bool	is_pointer()const		{return this->strip_qualifiers()->is_pointer_or_ref();}//TODO deprecate, must be specific since pointers & references have subtle differences.
	bool 	is_pointer_not_ref()const	{if (!this) return false; return this->strip_qualifiers()->name==PTR;}
	bool	is_void()const			{return !this || this->name==VOID;}
	bool	is_void_ptr()const		{return this->is_pointer_not_ref() && this->sub && this->sub->name==VOID;}
	int		num_derefs()const		{if (!this) return 0;int num=0; auto p=this; while (p->is_pointer()){num++;p=p->sub;} return num;}
	Type*	deref_all() const		{if (!this) return nullptr;int num=0; auto p=this; while (p->is_pointer()||this->is_qualifier()){p=p->sub;}; return (Type*)p;}
	void translate_typeparams_sub(const TypeParamXlat& tpx,Type* inherit_replace);
	Name as_name()const override{
		return this->name;
	}
	ExprStructDef*	struct_def();
	ExprStructDef*	struct_def() const;
	
	
	void			translate_typeparams(const TypeParamXlat& tpx) override;
	virtual ResolvedType	resolve(Scope* s, const Type* desired,int flags);
	virtual void verify();
	CgValue	compile(CodeGen& cg, Scope* sc);
};
bool type_params_eq(const vector<Type*>& a, const Type* tp);
bool type_params_eq(const vector<Type*>& a, const vector<Type*>& b);



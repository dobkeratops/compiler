#pragma once

#include "stringtable.h"
#include "node.h"
#include "exprstructdef.h"

struct Name;

#define TC_COERCE 0x0001
#define TC_INFER_REV 0x0002
#define TC_INFER_FWD 0x0004
#define TC_INFER (TC_INFER_FWD|TC_INFER_REV)

struct Type : ExprDef {
	MyVec<TParamDef*> tparams;
	//ExprDef* struct_def=0;	// todo: struct_def & sub are mutually exclusive.
	Type*	sub=0;					// a type is itself a tree
	Type*	next=0;
	Node* 	m_origin=0;				// where it comes from
	bool	tparam_index=-1;
	bool	rvalue=false;
	bool	is_generic(){return tparam_index>=0;}
	bool	has_non_instanced_typeparams()const;
	
	void set_origin(Node* t){m_origin=t;}
	Node* get_origin()const {return m_origin;}
	void push_back(Type* t);
	virtual const char* kind_str()const;
	Type(Node* origin, Name outer, Type* inner):Type(origin,outer){ push_back(inner);}
	Type(Node* origin, Name outer, Type* sub,Type* sub2):Type(origin,outer){ push_back(sub);push_back(sub2);}
	Type(Node* origin, Name outer, Type* sub,Type* sub2,Type* sub3):Type(origin,outer){ push_back(sub);push_back(sub2); push_back(sub3);}
	Type(Node* origin, Name outer, const Type* inner):Type(origin,outer,const_cast<Type*>(inner)){}
	Type(Node* origin,Name a,Name b): Type(origin,a, new Type(origin,b)){}
	Type(Node* origin,Name a,Name b,Name c): Type(origin,a,new Type(origin,b,c)){}
	//		auto tc=new Type(origin,c); auto tb=new Type(origin,b); tb->push_back(tc); push_back(tb);
	//	}
	Type(ExprStructDef* sd);
	Type(SrcPos sp,ExprStructDef* sd):Type(sd){pos=sp;}
	Type(Name outer, ExprStructDef* inner);
	Type(Node* origin,Name i);
	Type(Name n){name=n;};
	Type(Name i,SrcPos sp);
	Type(SrcPos sp,Name i):Type(i,sp){};	///TODO swap permanently, its more logical
	Type() { name=0;sub=0;next=0;}
	size_t	alignment() const;
	size_t	size() const;
	int	raw_type_flags()const	{int i=((int)name)-RAW_TYPES; if (i>=0&&i<NUM_RAW_TYPES){return g_raw_types[i];}else return 0;}
	void	replace_auto_with(const Type* src);
	bool	is_int()const		{return raw_type_flags()&RT_INTEGER;}
	bool	is_float()const		{return raw_type_flags()&RT_FLOATING;}
	bool	is_number()const	{return raw_type_flags()&(RT_FLOATING|RT_INTEGER);}
	bool	is_signed()const	{return raw_type_flags()&RT_SIGNED;}
	bool	is_register()const	{return !is_complex();}
	bool	is_anon_struct()const	{return !this->name && this->def;}
	bool	is_complex()const;
	bool	is_struct()const;
	bool	is_bool()const		{return name==BOOL;}
	bool	is_callable()const	{return name==FN||name==CLOSURE;}
	bool	is_qualifier()const	{return name==CONST||name==MUT;}
	bool	is_qualifier_or_ptr_or_ref()const{return is_qualifier()||is_pointer_or_ref();}
	bool	has_sub_destructors()const;
	bool	has_sub_constructors()const;
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
	const Type*		get_elem_type_if(int index) const;
	Type*			get_type_sub(int index)const{ auto s=sub;for (; s && index>0; s=s->next,index--);return s;}
	bool			is_primitive()const;
	bool			is_userdefined()const;
	int			num_pointers()const;
	int			num_pointers_and_arrays()const;
	ExprStructDef*	get_common_base(Type* other) ;
	ExprStructDef*	struct_def_noderef()const;
	ExprStructDef*	get_struct_autoderef() const; // with autoderef
	bool			is_equal(const Type* other,int mode=0,Name self_t=0) const{return const_cast<Type*>(this)->is_equal_s(const_cast<Type*>(other),mode,self_t);}
	bool			is_coercible(const Type* other,Name self_t=0) const{return const_cast<Type*>(this)->is_equal_s(const_cast<Type*>(other),TC_COERCE,self_t);};
	bool			is_inferable( Type* other,Name self_t=0) {return const_cast<Type*>(this)->is_equal(const_cast<Type*>(other),TC_INFER,self_t);};
	bool			is_inferable_rev( const Type* other,Name self_t=0) {return const_cast<Type*>(this)->is_equal(const_cast<Type*>(other),TC_INFER_REV,self_t);};
	bool			is_equal(const Type* other, const TParamXlat& xlat ,Name self_t=0)const;
	
	bool			is_equal_s( Type* other,int mode=0,Name self_t=0) ;
	bool			is_equal_sub( Type* other,int mode=0,Name self_t=0) ;
	bool			is_equal_sub(const Type* other, const TParamXlat& xlat ,Name self_t=0)const;
	// todo 'is_equal' rename to iscompatible, is_equal/coercible call it with flags
	int				is_equal_or_coercible(const Type* other, Name self_t=0) const{
		if (is_equal(other,0,self_t)) return 10;
		else if (is_coercible(other,self_t)) return 1;
		else return 0;
	}
	bool			is_compatible(const Type* other,bool coerce=false) const;
	bool			is_auto(){return !this || this->name==AUTO;}
	void			dump_sub(int f
							 )const;
	void			dump(PrinterRef depth)const;
	static Type*	get_auto();
	static Type*	get_bool();
	static Type*	get_void();
	static Type*	get_float();
	static Type*	get_void_ptr();
	static Type*	get_int();
	static Type*	get_i32();
	static Type*	get_u8();
	static Type*	get_u32();
	Node*	clone() const;
	void	set_struct_def(ExprStructDef* sd);
	void	clear_struct_def();
	bool	is_rvalue()const	{if (this) return this->rvalue; return false;}
	void	set_rvalue()	{if (this) this->rvalue=true;}
	bool	is_array()const		{return name==ARRAY;}
	bool	is_template()const	{ return sub!=0;}
	bool	is_function() const	{ return name==FN;}
	bool	is_closure() const	{ return name==CLOSURE;}
	Type*	fn_return() const	{ if (is_callable()) return sub->next; else return nullptr;}
	Type*	fn_args_first() const		{ if (sub)return sub->sub;else return nullptr;} 	void	clear_reg()			{reg_name=0;};
	const Type*	strip_qualifiers()const{
		auto p=this;
		if(!p) return nullptr;
		while (p->is_qualifier()){p=p->sub;}
		return p;
	};
	bool	is_name(int n1)	const{return this->name==n1;}
	bool	is_name(int n1, int n2)const{return this->name==n1||this->name==n2;}
	bool	is_name(int n1, int n2,int n3)const	{return this->name==n1||this->name==n2||this->name==n3;}
	bool	is_pointer_or_ref()const		{return this->strip_qualifiers()->is_name(PTR,REF,RVALUE_REF);}
	bool	is_ref()const{return this->strip_qualifiers()->is_name(REF,RVALUE_REF);}
	bool	is_lvalue_ref()const{if (this){return this->strip_qualifiers()->is_name(REF);}else return false;}
	bool	is_rvalue_ref()const{if (this){return this->strip_qualifiers()->is_name(RVALUE_REF);} else return false;}
	bool	is_pointer()const		{return this->strip_qualifiers()->is_pointer_or_ref();}//TODO deprecate, must be specific since pointers & references have subtle differences.
	bool 	is_pointer_not_ref()const	{if (!this) return false; return this->strip_qualifiers()->name==PTR;}
	bool	is_void()const			{return !this || this->name==VOID;}
	bool	is_void_ptr()const		{return this->is_pointer_not_ref() && this->sub && this->sub->name==VOID;}
	int		num_derefs()const		{if (!this) return 0;int num=0; auto p=this; while (p->is_pointer()){num++;p=p->sub;} return num;}
	Type*	deref_all() const		{if (!this) return nullptr;int num=0; auto p=this; while (p->is_pointer()||this->is_qualifier()){p=p->sub;}; return (Type*)p;}
	void translate_typeparams_sub(const TParamXlat& tpx,Type* inherit_replace);
	Name as_name()const override{
		return this->name;
	}
	const Type* as_type()const{return this;}
	ExprStructDef*	struct_def();
	ExprStructDef*	struct_def() const;
	Type*	clone_or_auto();
	
	void			translate_tparams(const TParamXlat& tpx) override;
	virtual ResolveResult	resolve(Scope* s, const Type* desired,int flags);
	virtual void verify();
	CgValue	compile(CodeGen& cg, Scope* sc, CgValue input) override;
};
void dump(const Type* a,const Type* b);
void dump_tparams(const MyVec<TParamDef*>& ts, const MyVec<TParamVal*>* given) ;

bool type_params_eq(const MyVec<Type*>& a, const Type* tp);
bool type_params_eq(const MyVec<Type*>& a, const MyVec<Type*>& b);
void verify(const Type* a);
void verify(const Type* a,const Type* b);
void verify(const Type* a,const Type* b,const Type* c);
ResolveResult assert_types_eq(int flags, const Node* n, const Type* a,const Type* b);
ResolveResult infer_and_cmp_types(int flags, const Node* n,  Type*& a, Type*& b);
// coercion direction matters, so we have both fwd/reverse only versions
ResolveResult infer_and_cmp_types_rev(int flags, const Node* n,  Type*& a, const Type* b);
ResolveResult infer_and_cmp_types_fwd(int flags, const Node* n,  const Type* a, Type*& b);



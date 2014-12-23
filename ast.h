#pragma once
#include "everywhere.h"
#include "error.h"
#include "stringtable.h"
#include "node.h"

typedef Type TParamVal;
typedef char ResolveResult;

/// Pattern eg arguments or pattern matching, if-let
/// simplest must behave like 'ident'
struct Pattern : Node {
	ResolveResult	resolve(Scope* sc, const Type* desired, int flags)override;
	Pattern* next=0;
	Pattern* sub=0;
	Pattern(SrcPos _pos, Name n){pos=_pos,name=n;}
	int	get_elem_count();
	Pattern*	get_elem(int i);
	Pattern*	get_elem(int i,int ii){return get_elem(i)->get_elem(ii);}
	const Pattern*	get_elem(int i)const ;
	const Pattern*	get_elem(int i,int ii)const{return get_elem(i)->get_elem(ii);}
	void	push_back(Pattern* p);
	void	push_child(Pattern* p);
	void	dump(int indent)const;
	Node*	clone()const;
	// if-let , args, or match arms would all call this.
	ResolveResult	resolve_with_type(Scope* sc, const Type* rhs, int flags);
	CgValue	compile(CodeGen& cg, Scope* sc, CgValue input) override;
	// subroutines of pattern compile, allows seperation into if (cond){bind..}
	// brute force just uses 'compile' and hopes llvm can optimize..
	CgValue	compile_condition(CodeGen& cg,Scope* sc, CgValue input);
	CgValue compile_bind(CodeGen& cg, Scope* sc, CgValue input);
	void	recurse(std::function<void(Node*)>& f);
	void	translate_typeparams(const TypeParamXlat& xlat);
	const char* kind_str()const{return "pattern";}
	Name	as_name()const;
	bool	is_just_ident()const{return this->sub==nullptr;}
	Name	as_just_name()const{if (this){return this->sub==nullptr?this->name:0;}else return 0;}
};

// Type Parameter, actually Template Parameter as we generalize it.
struct Expr : public Node{					// anythifng yielding a value
public:
};

struct ExprDef;
struct ExprStructDef;
struct ExprScopeBlock : Expr{
	Scope*		scope=0;
};

/// any node that is a Definition, maintains list of refs
struct ExprDef :Expr{
	Node*	refs=0;
	void	remove_ref(Node* ref);
	virtual ExprDef* member_of(){return nullptr;}
};

struct TParamDef: ExprDef{
	TParamVal* bound=0;	// eg traits/concepts
	TParamVal* defaultv=0;
	TParamDef(){};
	TParamDef(SrcPos sp,Name n, TParamVal* dv,TParamVal* bound_=0, TParamVal*defaultv_=0){pos=sp;name=n;defaultv=dv;bound=bound_;defaultv=defaultv_;};
	void dump(int depth)const;
	Node* clone() const override;
	TParamDef*	as_tparam_def() override{return this;}
	const char* kind_str()const{return "TParamDef";}
};

struct TypeDef : ExprDef{ // eg type yada[T]=ptr[ptr[T]]; or C++ typedef
	const char* kind_str()const{return "typedef";}
	vector<TParamDef*> typeparams;
};

struct ExprLiteral : ExprDef {
	TypeId	type_id;
	ExprLiteral* next_of_scope=0;	// collected..
	Scope* owner_scope=0;
	int llvm_strlen;
	
	union  {int val_int; int val_uint; float val_float; void* val_ptr;bool val_bool; const char* val_str;int val_keyword;} u;
	virtual const char* kind_str()const{return"lit";}
	void dump(int depth) const;
	ExprLiteral(bool b);
	ExprLiteral(const SrcPos&s);
	ExprLiteral(const SrcPos&s,float f);
	ExprLiteral(const SrcPos&s,int i);
	ExprLiteral(const SrcPos&s,const char* start,int length);
	ExprLiteral(const SrcPos&s,const char* start);// take ownership
	~ExprLiteral();
	Node* clone() const;
	size_t strlen() const;
	bool is_string() const		{return type_id==T_CONST_STRING;}
	bool is_undefined()const	{return false;}
	const char* as_str()const	{return type_id==T_CONST_STRING?u.val_str:"";}
	ResolveResult resolve(Scope* scope, const Type* desired,int flags);
	void translate_typeparams(const TypeParamXlat& tpx);
	CgValue compile(CodeGen& cg, Scope* sc, CgValue input) override;
	ExprLiteral* as_literal() override{ return this;};
};

/// 'ArgDef' used for function arguments and struct-fields.
/// both have form ident:type=<default expr>
struct ArgDef :ExprDef{
	Scope*	owner=0;
	Pattern*	pattern=0;
	Expr*		default_expr=0;
	void set_owner(Scope* s){
		ASSERT(owner==0 ||owner==s);
		this->owner=s;}
	ExprDef* member_of()override;
	ArgDef*		next_of_name=0;
	uint32_t	size_of,offset;
	//Type* type=0;
	//Type* get_type()const {return type;}
	//void set_type(Type* t){verify(t);type=t;}
	//Type*& type_ref(){return type;}
	ArgDef(SrcPos p,Name n, Type* t=nullptr,Expr* d=nullptr){pos=p; name=n;set_type(t);default_expr=d; owner=0;}
	void dump(int depth) const;
	const char* kind_str()const override;
	~ArgDef(){}
	Node*	clone() const override;
	Name	as_name()const				{return this->name;}
	size_t	size()const;
	size_t alignment() const;	//todo, 	size_t		alignment() const			{ return type()->alignment();}//todo, eval templates/other structs, consider pointers, ..
	
	
	void	translate_typeparams(const TypeParamXlat& tpx) override;
	ResolveResult	resolve(Scope* sc, const Type* desired, int flags) override;
	ArgDef*	as_arg_def()		{return this;}
	void		recurse(std::function<void(Node*)>&) override;
};

enum VarKind{VkArg,Local,Global};
struct Variable : ExprDef{
	bool		on_stack=true;
	CaptureVars*	capture_in=0;	// todo: scope or capture could be unified? 'in this , or in capture ...'
	VarKind		kind;
	Scope*		owner=0;
	short capture_index;
	const char*	kind_str(){return "variable";}
	bool		keep_on_stack(){return on_stack||capture_in!=0;}
	Variable*	next_of_scope=0;	// TODO could these be unified, var is owned by capture or scope
	Variable*	next_of_capture=0;
	Expr*		initialize=0; // if its an argdef, we instantiate an initializer list
	Variable(SrcPos pos,Name n,VarKind k){this->pos=pos,name=n; initialize=0; owner=0;kind=k;this->set_type(0);}
	bool		is_captured()const{return this->capture_in!=nullptr;}
	Node* clone() const {
		auto v=new Variable(this->pos,name,this->kind);
		std::cout<<"clone "<<str(name)<<this<<" as "<<v<<"\n";
		v->initialize = verify_cast<Expr*>(this->initialize->clone_if());
		v->next_of_scope=0; v->set_type(this->get_type()); return v;
	}
	Variable*	as_variable() {return this;}
	const Variable*	as_variable() const {return this;}
	void dump(int depth) const;
	CgValue		compile(CodeGen&cg, Scope* sc, CgValue input) override;
	const char* kind_str()const{return "variable";}
};

struct ExprIdent :Expr{
	// TODO: definition pointer. (ptr to field,function,struct,typedef..)
	void		dump(int depth) const;
	Node*		clone() const;
	ExprIdent()	{};
	ExprIdent(const char* s,const char* e)	{name=Name(s,e);set_type(nullptr);}
	ExprIdent(Name n,SrcPos sp)				{pos=sp;name=n;set_type(nullptr);}
	ExprIdent(SrcPos sp,Name n)				{pos=sp;name=n;set_type(nullptr);}
	const char*	kind_str()const override		{return"ident";}
	Name		as_name()const override		{return name;};
	ExprIdent*	as_ident()					{return this;}
	const ExprIdent* as_ident() const		{return this;}
	bool		is_function_name()const	override;
	bool		is_variable_name()const	override;
	bool		is_placeholder()const			{return name==PLACEHOLDER;}
	bool		is_undefined()const				{return is_placeholder();}
	void		translate_typeparams(const TypeParamXlat& tpx) override;
	CgValue		compile(CodeGen&cg, Scope* sc, CgValue input) override;
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags) override;
	void		recurse(std::function<void(Node*)>&) override;
};
// Identifier with given type-parameters
struct IdentWithTParams : ExprIdent{
	ExprIdent*			ident;
	vector<TParamVal*>	given_tparams;
	void		dump(int depth)const override;
	Node*		clone()const override;
	Type*		make_type(Scope* sc) const;
	void		translate_typeparams(const TypeParamXlat& tpx) override;
	void		recurse(std::function<void(Node*)>&) override;
	Node*		get_elem_node(int index)override;
	int			get_elem_count()const override;
	IdentWithTParams(SrcPos src, ExprIdent* ident);
	const char*	kind_str()const override		{return"ident<..>";}
};


// load data->vtb // if this matters it would be inlined
// load vtb->fn
// when the time comes - vtb->destroy()
//                       vtb->trace

/// TODO-Anything matchable by the template engine eg Type, Constants, Ident.. (how far do we go unifying templates & macros..)

/// CaptureVars of local variables for a lambda function
/// hidden entity created in resolve. compile to 'C' might roll these manually?
struct CaptureVars : ExprDef{
	/// TODO - this doesn't really want to be in 'ast',
	// have just moved it to stop 'semantics' interface depending..
	//
	Name			tyname(){return name;};
	ExprFnDef*		capture_from=0;
	ExprFnDef*		capture_by=0;
	Variable*		vars=0;
	CaptureVars*		next_of_from=0;
	ExprStructDef*	the_struct=0;
	void 			coalesce_with(CaptureVars* other);
	ExprStructDef*	get_struct();
	CgValue			compile(CodeGen& cg, Scope* outer,CgValue input) override;
	Node* clone() const override{
		dbprintf("warning todo template instatntiation of captures\n");
		return nullptr;
	};
	const Type*			get_elem_type(int i)const override;
	Name			get_elem_name(int i)const override;
	int				get_elem_count() const override;
	void		recurse(std::function<void(Node*)>&) override;
};





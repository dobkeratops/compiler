#pragma once
#include "everywhere.h"
#include "error.h"
#include "stringtable.h"
#include "node.h"



typedef Type TParamVal;

// Type Parameter, actually Template Parameter as we generalize it.
struct Expr : public Node{					// anything yielding a value
public:
};


struct ExprScopeBlock : Expr{
	Scope*		scope=0;
};
/// TODO a pattern might become different to Expr
/// simplest must behave like 'ident'
struct Pattern : Node {
	ResolvedType	resolve(Scope* sc, Type* desired, int flags){ASSERT(0 && "dont resolve pattern"); return ResolvedType();}
	Pattern* next=0;
	Pattern* sub=0;
	Node*	clone()const;
	
};
/// any node that is a Definition, maintains list of refs
struct ExprDef :Expr{
	Node*	refs=0;
	void	remove_ref(Node* ref);
	virtual ExprStructDef* member_of()const{return nullptr;}
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
	ResolvedType resolve(Scope* scope, const Type* desired,int flags);
	void translate_typeparams(const TypeParamXlat& tpx);
	CgValue compile(CodeGen& cg, Scope* sc);
};

/// 'ArgDef' used for function arguments and struct-fields.
/// both have form ident:type=<default expr>
struct ArgDef :ExprDef{
	Scope*	owner=0;
	void set_owner(Scope* s){
		ASSERT(owner==0 ||owner==s);
		this->owner=s;}
	ExprStructDef* member_of();
	uint32_t	size_of,offset;
	//Type* type=0;
	Expr*		default_expr=0;
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
	ResolvedType	resolve(Scope* sc, const Type* desired, int flags) override;
	ArgDef*	as_arg_def()		{return this;}
};

enum VarKind{VkArg,Local,Global};
struct Variable : ExprDef{
	bool		on_stack=true;
	CaptureVars*	capture_in=0;	// todo: scope or capture could be unified? 'in this , or in capture ...'
	VarKind		kind;
	Scope*		owner=0;
	short capture_index;
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
	Name		as_name()const override			{return name;};
	ExprIdent*	as_ident()						{return this;}
	bool		is_function_name()const	override;
	bool		is_variable_name()const	override;
	bool		is_placeholder()const			{return name==PLACEHOLDER;}
	bool		is_undefined()const				{return is_placeholder();}
	void		translate_typeparams(const TypeParamXlat& tpx) override;
	CgValue		compile(CodeGen&cg, Scope* sc) override;
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags) override;
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
	CgValue			compile(CodeGen& cg, Scope* outer);
	Node* clone() const override{
		dbprintf("warning todo template instatntiation of captures\n");
		return nullptr;
	};
	Type*			get_elem_type(int i);
	Name			get_elem_name(int i);
	int				get_elem_count();
};




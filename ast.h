#pragma once
#include "everywhere.h"
#include "error.h"
#include "stringtable.h"


/// TODO - this sourcefile is placeholder,
/// want to move ast description in here but its' still tied up with random stuff

struct Type;
struct Expr;
struct Node {
private:Type* m_type=0;
	
	
public:
	int visited;					// anti-recursion flag.
	Node*	m_parent=0;					// for search & error messages,convenience TODO option to strip.
	Name name;
	RegisterName reg_name=0;			// temporary for llvm SSA calc. TODO: these are really in Expr, not NOde.
	bool reg_is_addr=false;
	SrcPos pos;						// where is it
	Node(){}
	ExprDef*	def=0;		// definition of the entity here. (function call, struct,type,field);
	Node*		next_of_def=0;
	void set_def(ExprDef* d);
	void clear_def();
	virtual void dump(int depth=0) const;
	virtual ResolvedType resolve(Scope* scope, const Type* desired,int flags){dbprintf("empty? %s resolve not implemented", this->kind_str());return ResolvedType(nullptr, ResolvedType::INCOMPLETE);};
	ResolvedType resolve_if(Scope* scope, const Type* desired,int flags){
		if (this) return this->resolve(scope,desired,flags);
		else return ResolvedType();
	}
	virtual const char* kind_str()const	{return"node";}
	void	replace_name_if(Name if_is,Name replacement){if (name==if_is) name=replacement;}
	virtual int get_name() const		{return 0;}
	virtual Name get_mangled_name()const {return name;}
	const char* get_name_str()const;
	const char* name_str()const			{return str(this->name);}
	//	Name ident() const					{if (this)return this->name;else return 0;}
	virtual Node* clone() const=0;
	Node* clone_if()const				{ if(this) return this->clone();else return nullptr;}
	void dump_if(int d)const			{if (this) this->dump(d);}
	virtual void clear_reg()			{reg_name=0;};
	RegisterName get_reg(CodeGen& cg, bool force_new);
	RegisterName get_reg_new(CodeGen& cg);
	RegisterName get_reg_named(Name baseName, int* new_index, bool force_new);
	RegisterName get_reg_named_new(Name baseName, int* new_index);
	RegisterName get_reg_existing();
	virtual	vector<TParamDef*>*			get_typeparams(){ return nullptr;}
	Node*	parent()					{return this->m_parent;}
	void	set_parent(Node* p)			{this->m_parent=p;}
	virtual CgValue codegen(CodeGen& cg,bool contents);
	virtual bool is_undefined()const										{if (this && name==PLACEHOLDER) return true; return false;}
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const	{return ;}
	void find_vars_written_if(Scope*s, set<Variable*>& vars) const{ if(this)this->find_vars_written(s, vars);
	}
	void translate_typeparams_if(const TypeParamXlat& tpx){if (this) this->translate_typeparams(tpx);}
	virtual void translate_typeparams(const TypeParamXlat& tpx){ error(this,"not handled for %s",this->kind_str()); };
	virtual ExprOp* as_op()const			{error(this,"expected op, found %s:%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name as_name()const {
		error(this,"expected named item at node %s kind=%s",str(this->name),this->kind_str());
		return PLACEHOLDER;
	};
	bool is_ident()const;
	virtual ExprStructDef* as_struct_def()const;
	template<typename T> T* as()const{ auto ret= const_cast<T*>(dynamic_cast<T*>(this)); if (!ret){error(this,"expected,but got %s",this->kind_str());} return ret;};
	template<typename T> T* isa()const{ return const_cast<T*>(dynamic_cast<T*>(this));};
	virtual int recurse(std::function<int(Node* f)> f){dbprintf("recurse not implemented\n");return 0;};
	
	virtual CgValue compile(CodeGen& cg, Scope* sc);
	CgValue compile_if(CodeGen& cg, Scope* sc);
	virtual Node* instanced_by()const{return nullptr;}
	virtual ExprIdent*	as_ident() {return nullptr;}
	virtual ExprFor* 	as_for() {return nullptr;}
	virtual ExprFnDef*	as_fn_def() {return nullptr;}
	virtual const ExprFnDef*	as_fn_def() const {return nullptr;}
	virtual ExprBlock*	as_block() {return nullptr;}
	virtual TParamDef*	as_tparam_def() {return nullptr;}
	ExprBlock* as_block() const{return const_cast<Node*>(this)->as_block();}
	virtual ArgDef* as_arg_def() {return nullptr;}
	virtual Variable* as_variable() {return nullptr;}
	virtual const Variable* as_variable() const {return nullptr;}
	ArgDef*			as_field() {return this->as_arg_def();}
	virtual void verify() {};
	// abstract interface to 'struct-like' entities;
	virtual Type* get_elem_type(int index){error(this,"tried to get elem on name=%s kind=%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name get_elem_name(int index){error(this,"tried to get elem on %s %s",str(this->name),this->kind_str());return nullptr;}
	virtual int get_elem_index(Name name){error(this,"tried to get elem on %s %s",str(this->name),this->kind_str());return -1;}
	virtual int get_elem_count()const{return 0;}
	virtual size_t alignment()const {return 16;} // unless you know more..
	virtual ~Node(){
		error("dont call delete, we haven't sorted out ownership of Types or nodes. compiler implementation doesn't need to free anything. Types will be owned by a manager, not the ast ");
	}
	virtual Expr*	loop_else_block()const			{return nullptr;}// for decoupling something
	LLVMType get_type_llvm() const;
	virtual Type* eval_as_type()const		{return nullptr;};
	virtual ExprBlock* is_subscript()const	{return (ExprBlock*)nullptr;}
	virtual bool is_function_name()const	{return false;}
	virtual bool is_variable_name()const	{return false;}
	virtual Scope* get_scope()				{return nullptr;}
	Type* expect_type() const;
	Type* get_type() const		{ if(this) {::verify(this->m_type);return this->m_type;}else return nullptr;}
	Type*& type()				{::verify(this->m_type);return this->m_type;}
	const Type* type()const		{::verify(this->m_type);return this->m_type;}
	void type(const Type* t)	{::verify(t);this->m_type=(Type*)t;}
	void set_type(const Type* t);
	void clear_type(){m_type=0;};
	void force_type_todo_verify(const Type* t){ m_type=const_cast<Type*>(t);}
	Type*& type_ref()			{return this->m_type;}
	void dump_top()const;
};


template<typename T>
T* expect_cast(Node* n){
	auto r=dynamic_cast<T*>(n);
	if (!r) {
		extern void error(const Node*,const char*,...);
		T t;
		error(n, "expected %s to be %s not %s", str(n->name),t.kind_str(), n->kind_str());
		n->dump(-1);
	}
	return r;
}


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




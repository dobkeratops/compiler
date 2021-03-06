#pragma once
//#include "ast.h"
#include "exprfndef.h"
#include "exprstructdef.h"
#include "exprop.h"
#include "semantics.h"
#include "codegen.h"


/// 'ExpressionBlock' - expr( expr,expr,...)
///- any group of expressions
///  eg functioncall +args, compound statement, struct-initializer, subscript expr (like fncall)
struct ExprBlock :public ExprScopeBlock{
	// used for function calls and compound statement
	// started out with lisp-like (op operands..) where a compound statement is just (do ....)
	// TODO we may split into ExprOperator, ExprFnCall, ExprBlock
	// the similarity between all is
	
	Expr*	call_expr=0;  //call_expr(argls...)  or {argsls...} call_expr[argls..] call_expr{argls}
	Vec<Expr*>	argls;
	//ExprFnDef*	call_target=0;
	ExprBlock*	next_of_call_target=0;	// to walk callers to a function
	// these are supposed to be mutually exclusive substates, this would be an enum ideally.
	ExprBlock(){};
	ExprBlock(const SrcPos& p);
	ExprBlock(const SrcPos& p,Name n){pos=p; this->name=n;};
	
	// TODO: move these into dedicated nodes, starting with 'structInitializer' which will give us ScalaDefaultConstructor.
	virtual	const char*	bracket_delim()const {return "{;}";}
	ExprFnDef*	get_fn_call()const;
	Name		get_fn_name() const;
	void		dump(PrinterRef depth) const;
	Node*		clone() const;
	ExprBlock*		clone_sub(ExprBlock* clone_into) const;
	bool		is_undefined()const;
	void		create_anon_struct_initializer();
	void			clear_reg()				{for (auto p:argls)p->clear_reg();if (call_expr)call_expr->clear_reg(); reg_name=0;};
	const char* kind_str() const  override		{return"block";}
	ExprBlock* 		as_block() override 	{return this;}
	const ExprBlock*		as_block() const override		{return this;};
	Scope*	get_scope()	override			{return this->scope;}
	void 			verify();
	CgValue 		compile(CodeGen& cg, Scope* sc, CgValue) override;
	void	translate_tparams(const TParamXlat& tpx) override;
	void	find_vars_written(Scope* s,set<Variable*>& vars )const override;
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags) override;
	ResolveResult	resolve_sub(Scope* scope, const Type* desired,int flags,Expr* receiver);
	ResolveResult	resolve_elems(Scope* scope, const Type* sub_desired, int flags);
	void push_back_if(Expr* e){if (e) argls.push_back(e);}
	void gather_symbols(Scope* sc)override;
	int	get_elem_count()const override{return this->argls.size();}
	Node*	get_elem_node(int i) override{return this->argls[i];}
	void		recurse(std::function<void(Node*)>&) override;
};

// TODO Where Block - eg 'a where{b;c;d;}' === '{b;c;d;return a}'
// add dedicated node for this
struct ExprWhere : ExprBlock {
	const char* kind_str() const  override		{return "where";}
	ResolveResult resolve(Scope* scope, const Type* desired, int flags) override;
	void	dump(PrinterRef depth) const override;
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	ExprBlock* 		as_block() override 	{return nullptr;}
	Node*		clone() const{return clone_sub(new ExprWhere());}
	virtual const Expr*	get_return_expr()const override	{return call_expr;}
};

struct StructInitializer{ // named initializer
	ExprBlock*		si; // struct_intializer
	Scope*			sc;
	ExprStructDef*	struct_def=0;
	ExprStructDef*	get_struct_def(){return struct_def;}
	MyVec<int>		field_indices;
	MyVec<ArgDef*> field_refs;
	MyVec<Expr*>	value;
	void map_fields()								{resolve(nullptr,0);}//todo..seperate out}
	StructInitializer(Scope* s,ExprBlock* block)	{si=block,sc=s;};
	ResolveResult resolve(const Type* desiredType,int flags);
};


struct ExprStructInit : ExprBlock{
	ExprStructInit(){};
	ExprStructInit(const SrcPos& p, Expr* ce):ExprBlock(p){this->call_expr=ce;};
	CgValue compile_struct_init(CodeGen& cg,Scope *sc, RegisterName force_dst);
	const char* kind_str() const  override		{
		return "struct_init";
	}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const {return (Node*)clone_sub(new ExprStructInit());}
	ResolveResult	resolve(Scope* sc, const Type* desired,int flags) override;
	ResolveResult	resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op);
	CgValue compile_operator_new(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs) override;
	virtual	const char*	bracket_delim()const override {return "{,}";}
};
struct ExprParens : ExprBlock{
	const char* kind_str() const  override		{return "expr_parens";}
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprParens());}
};
struct ExprCompound : ExprBlock{
	ExprCompound(){}
	ExprCompound(SrcPos sp){pos=sp;}
	ExprCompound(SrcPos sp,Expr* e1,Expr*e2=nullptr,Expr* e3=nullptr){pos=sp;push_back_if(e1);push_back_if(e2);push_back_if(e3);}
	const char* kind_str() const  override		{return "expr_compound";}
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprCompound(this->pos));}
	const ExprCompound*	as_compound() const override{return this;}
	ExprCompound*	as_compound() override{return this;}
	virtual const Expr*	get_return_expr()const override	{return (argls.size())?argls.back():nullptr;}

};

struct ExprTuple : ExprBlock{
	const char* kind_str() const  override		{return "tuple";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprTuple());}
	void set_tuple_component_types();
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags)override;
	CgValue compile_operator_dot(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs) override;
	ResolveResult	resolve_operator_dot(Scope *sc, const Type *desired, int flags, Expr *lhs,Type*& tref)override;
	virtual	const char*	bracket_delim()const override {return "(,)";}
};

struct ExprCall : ExprBlock{
	const char* kind_str() const  override		{return "call";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	// constructors for building from internal code generators
	ExprCall(){}
	ExprCall(SrcPos pos, Name fname);
	ExprCall(SrcPos pos, Name fname, Expr* arg);
	ExprCall(SrcPos pos, Name fname, Expr* arg1, Expr* arg2);
	ExprCall(SrcPos pos, ExprFnDef* f, Expr* arg1=nullptr, Expr* arg2=nullptr);
	ExprCall(SrcPos pos, Expr* call, Expr* arg1=nullptr, Expr* arg2=nullptr,Expr* arg3=nullptr);
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprCall());}
	ResolveResult	resolve(Scope* sc, const Type* desired,int flags) override;
	ResolveResult resolve_call_sub(Scope* sc, const Type* desired, int flags,Expr* receiver);
	CgValue compile_operator_new(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs) override;
	ResolveResult	resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op)override;
	CgValue compile_operator_dot(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs) override;
	ResolveResult	resolve_operator_dot(Scope *sc, const Type *desired, int flags, Expr *lhs,Type*& tref)override;
	virtual	const char*	bracket_delim()const override {return "(,)";}
};
			 
struct ExprArrayInit : ExprBlock{
	const char* kind_str() const  override		{return "array_init";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprArrayInit());}
	const ExprArrayInit* as_array_init()const override{return this;}
	ResolveResult	resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op)override;
};
struct ExprSubscript : ExprBlock{
	const char* kind_str() const  override		{return "subscript";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node*	clone() const override	{return (Node*)clone_sub(new ExprSubscript());}
	ResolveResult	resolve(Scope* sc, const Type* desired,int flags)override;
	const ExprSubscript* as_subscript()const	override{return this;}
	ExprSubscript* as_subscript()override{return this;}
	CgValue compile_operator_new(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs) override;
	ResolveResult	resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op)override;
	virtual	const char*	bracket_delim()const override {return "[,]";}
};

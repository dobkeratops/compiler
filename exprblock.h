#pragma once
//#include "ast.h"
#include "exprfndef.h"
#include "exprstructdef.h"
#include "exprop.h"
#include "codegen.h"


/// 'ExpressionBlock' - expr( expr,expr,...)
///- any group of expressions
///  eg functioncall +args, compound statement, struct-initializer, subscript expr (like fncall)
struct ExprBlock :public ExprScopeBlock{
	// used for function calls and compound statement
	// started out with lisp-like (op operands..) where a compound statement is just (do ....)
	// TODO we may split into ExprOperator, ExprFnCall, ExprBlock
	// the similarity between all is
	
	short	bracket_type;	//OPEN_PARENS,OPEN_BRACES,OPEN_BRACKETS,(ANGLE_BRACKETS?)
	short	delimiter=0;//COMMA, SEMICOLON,SPACES?
	
	Expr*	call_expr=0;  //call_expr(argls...)  or {argsls...} call_expr[argls..] call_expr{argls}
	Vec<Expr*>	argls;
	//ExprFnDef*	call_target=0;
	ExprBlock*	next_of_call_target=0;	// to walk callers to a function
	// these are supposed to be mutually exclusive substates, this would be an enum ideally.
	ExprBlock(){};
	ExprBlock(const SrcPos& p);
	// TODO: move these into dedicated nodes, starting with 'structInitializer' which will give us ScalaDefaultConstructor.
	bool	is_compound_expression()const	{return !call_expr && !index(name);}
	bool	is_tuple()const					{return !call_expr && this->bracket_type==OPEN_PAREN && this->delimiter==COMMA;}
	bool	is_struct_initializer()const	{return this->bracket_type==OPEN_BRACE && (this->delimiter==COMMA||this->delimiter==0);}
	bool	is_match() const				{return false;}
	bool	is_function_call()const			{return (this->call_expr!=0) && this->bracket_type==OPEN_PAREN && (this->delimiter==COMMA||this->delimiter==0);}
	bool	is_anon_struct()const			{return this->is_struct_initializer() && !this->call_expr;}
	bool	is_array_initializer()const		{return !this->call_expr && this->bracket_type==OPEN_BRACKET && this->delimiter==COMMA;}
	void	set_delim(int delim)			{delimiter=delim;}
	ExprBlock* is_subscript()const override	{
		if (this->bracket_type==OPEN_BRACKET && call_expr){
			return (ExprBlock*) this;
		}
		return (ExprBlock*)nullptr;
	}
	ExprFnDef*	get_fn_call()const;
	Name		get_fn_name() const;
	void		dump(int depth) const;
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
	void	translate_typeparams(const TypeParamXlat& tpx) override;
	void	find_vars_written(Scope* s,set<Variable*>& vars )const override;
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags);
	ResolveResult	resolve_sub(Scope* scope, const Type* desired,int flags,Expr* receiver);
	ResolveResult	resolve_elems(Scope* scope, const Type* sub_desired, int flags);
	int	get_elem_count()const override{return this->argls.size();}
	Node*	get_elem_node(int i) override{return this->argls[i];}
	void		recurse(std::function<void(Node*)>&) override;
};

// TODO Where Block - eg 'a where{b;c;d;}' === '{b;c;d;return a}'
// add dedicated node for this
struct ExprWhere : ExprBlock {
	const char* kind_str() const  override		{return "where";}
	ResolveResult resolve(Scope* scope, const Type* desired, int flags);
	void	dump(int depth) const;
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	ExprBlock* 		as_block() override 	{return nullptr;}
	Node*		clone() const;
};

struct ExprStructInit : ExprBlock{
	CgValue compile_struct_init(CodeGen& cg,Scope *sc, RegisterName force_dst);
	const char* kind_str() const  override		{return "struct_init";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const {return (Node*)clone_sub(new ExprStructInit());}
	ResolveResult	resolve(Scope* sc, const Type* desired,int flags) override;

};
struct ExprTuple : ExprBlock{
	const char* kind_str() const  override		{return "tuple";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprTuple());}
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags)override;
};
struct ExprCall : ExprBlock{
	const char* kind_str() const  override		{return "call";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprCall());}
	ResolveResult	resolve(Scope* sc, const Type* desired,int flags) override;
	ResolveResult resolve_call_sub(Scope* sc, const Type* desired, int flags,Expr* receiver);
};
struct ExprArrayInit : ExprBlock{
	const char* kind_str() const  override		{return "array_init";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node* 	clone() const override	{return (Node*)clone_sub(new ExprArrayInit());}
};
struct ExprSubscript : ExprBlock{
	const char* kind_str() const  override		{return "subscript";}
	CgValue compile(CodeGen& cg, Scope* sc, CgValue) override;
	Node*	clone() const override	{return (Node*)clone_sub(new ExprSubscript());}
	ResolveResult	resolve(Scope* sc, const Type* desired,int flags)override;
};

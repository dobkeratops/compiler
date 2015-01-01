#pragma once
#include "ast.h"

/// todo:
/// [1] operator overloading,
/// [2] custom operators
/// seperate binary,unary
///thats why this is its own file..

struct ExprOp: public Expr{
	ExprOp(){}
	Expr	*lhs=0,*rhs=0;
	Node* clone() const;
	void clear_reg()						{lhs->clear_reg(); rhs->clear_reg();}
	ExprOp(Name opname,SrcPos sp, Expr* l, Expr* r){
		pos=sp;
		lhs=l; rhs=r;
		name=opname;
	}
	void	set_fn(ExprFnDef* call)			{this->set_def((ExprDef*)call);}
	const ExprFnDef*	get_fn()const		{return def?def->as_fn_def():nullptr;}
	ExprOp(Name opname,SrcPos sp)			{name=opname; lhs=0; rhs=0;pos=sp;}
	void	dump(PrinterRef depth) const;
	int		get_operator() const			{return index(this->name);}
	int		get_op_name() const				{return index(this->name);}
	bool	is_undefined()const				{return (lhs?lhs->is_undefined():false)||(rhs?rhs->is_undefined():false);}
	ExprOp*		as_op()const override		{return const_cast<ExprOp*>(this);}
	const char* kind_str()const override		{return"operator";}
	void 		translate_tparams(const TParamXlat& tpx) override;
	ResolveResult resolve(Scope* scope, const Type* desired,int flags) override;
	void 		find_vars_written(Scope* s, set<Variable*>& vars) const override;
	void 		verify() override;
	int			num_levels(int name);
	int			get_flow_expr(int name);
	bool		find_overloads(Scope* sc,const Type* desired,int flags);
	CgValue compile(CodeGen& cg, Scope* sc,CgValue) override;
	CgValue compile_operator_overload(CodeGen& cg, Scope* sc);
	void		recurse(std::function<void(Node*)>&);
};
struct ExprDummy: public ExprOp{
	~ExprDummy() override{};
};
void dump_field_info(Node* n,Scope* sc);


#pragma once
/// todo:
/// [1] operator overloading,
/// [2] custom operators
/// seperate binary,unary
///thats why this is its own file..

struct ExprOp: public Expr{
	Expr	*lhs=0,*rhs=0;
	Node* clone() const;
	void clear_reg()						{lhs->clear_reg(); rhs->clear_reg();}
	ExprOp(Name opname,SrcPos sp, Expr* l, Expr* r){
		pos=sp;
		lhs=l; rhs=r;
		name=opname;
	}
	ExprOp(Name opname,SrcPos sp)			{name=opname; lhs=0; rhs=0;pos=sp;}
	void	dump(int depth) const;
	int		get_operator() const			{return index(this->name);}
	int		get_op_name() const				{return index(this->name);}
	bool	is_undefined()const				{return (lhs?lhs->is_undefined():false)||(rhs?rhs->is_undefined():false);}
	ExprOp*		as_op()const override		{return const_cast<ExprOp*>(this);}
	const char* kind_str()const override		{return"operator";}
	void 		translate_typeparams(const TypeParamXlat& tpx) override;
ResolvedType resolve(Scope* scope, const Type* desired,int flags) override;
	void 		find_vars_written(Scope* s, set<Variable*>& vars) const override;
	void 		verify() override;
	int			num_levels(int name);
	int			get_flow_expr(int name);
	CgValue compile(CodeGen& cg, Scope* sc);

};

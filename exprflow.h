#pragma once


#include "semantics.h"
#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "run_test.h"
#include "exprfndef.h"

struct CgValue;
struct ExprFlow:Expr{	// control flow statements
};

/// if-else expression.
struct ExprIf :  ExprFlow {
	Scope*	scope=0;
	Expr*	cond=0;
	Expr*	body=0;
	Expr*	else_block=0;
	void	dump(PrinterRef depth) const;
	ExprIf(const SrcPos& s){pos=s;name=0;cond=0;body=0;else_block=0;}
	~ExprIf(){}
	Node*	clone() const;
	bool	is_undefined()const			{
		if (cond)if (cond->is_undefined()) return true;
		if (body)if (body->is_undefined()) return true;
		if (else_block)if (else_block->is_undefined()) return true;
		return false;
	}
	const char*	kind_str()const	override	{return"if";}
	ResolveResult	resolve(Scope* scope,const Type*,int flags) ;
	void	find_vars_written(Scope* s,set<Variable*>& vars ) const override;
	void	translate_tparams(const TParamXlat& tpx) override;
	CgValue	compile(CodeGen& cg, Scope* sc,CgValue input) override;
	Scope*	get_scope()	override			{return this->scope;}
	void	recurse(std::function<void(Node*)>& f)override;
};

/// For-Else loop/expression. Currrently the lowest level loop construct
/// implement other styles of loop as specializations that omit init, incr etc.
struct ExprFor :  ExprFlow {
	ExprFor(){}
	Expr* init=0;				// clike initializer or deduced from pattern/expr
	Expr* cond=0;				// C-like condition, or deduced from pattern/expr
	Expr* incr=0;				// clike increment, or deduced from patter/expr
	Expr* body=0;
	Expr* else_block=0;			// run if the loop exits without break.
	Scope* scope=0;
	void dump(PrinterRef depth) const;
	ExprFor(const SrcPos& s)		{pos=s;name=0;init=0;cond=0;incr=0;body=0;else_block=0;scope=0;}
	//bool is_c_for()const			{return !pattern;}
	//bool is_for_in()const			{return pattern && cond==0 && incr==0;}
	~ExprFor(){}
	const char* kind_str()const	 override	{return"for";}
	bool is_undefined()const				{return /*(pattern&&pattern->is_undefined())||*/(init &&init->is_undefined())||(cond&&cond->is_undefined())||(incr&&incr->is_undefined())||(body&& body->is_undefined())||(else_block&&else_block->is_undefined());}
	Expr* find_next_break_expr(Expr* prev=0);
	Node* clone()const;
	Node* clone_for_sub(ExprFor* f)const;
	Scope*		get_scope()override			{return this->scope;}
	ExprFor*	as_for()override			{return this;}
	ResolveResult resolve(Scope* scope,const Type*,int flags) override;
	ResolveResult resolve_for_sub(Scope* scope,const Type*,int flags);
	void find_vars_written(Scope* s,set<Variable*>& vars ) const override;
	void translate_tparams(const TParamXlat& tpx) override;
	CgValue compile(CodeGen&, Scope*,CgValue) override;
	Expr*	loop_else_block()const override{return else_block;}
	void	recurse(std::function<void(Node*)>& f)override;
};
struct ExprForIn : ExprFor{
	ExprForIn(){};
	ExprForIn(SrcPos p){pos=p;};
	Pattern* pattern=nullptr;			// iterator
	Expr* expr=nullptr;
	Node* clone()const override;
	const char* kind_str()const override{return "for_in";}
	void translate_tparams(const TParamXlat& tpx) override;
	ResolveResult resolve(Scope* scope,const Type*,int flags) override;
};


/// rust match, doesn't work yet - unlikely to support everything it does
/// C++ featureset extended with 2way inference can emulate match like boost variant but improved.
struct ExprMatch : ExprFlow {
	Scope* 	scope=0;
	const char*		kind_str() const {return "match";}
	CgValue			compile(CodeGen& cg, Scope* sc,CgValue input) override;
	ResolveResult	resolve(Scope* sc, const Type* desired, int flags);
	Expr*		expr=0;
	MatchArm*	arms=0;
	Node*	clone()const{ return this->clone_into(new ExprMatch);}
	Node*	clone_into(ExprMatch* ) const;
	void	dump(PrinterRef depth)const;
	void	recurse(std::function<void(Node*)>& f)override;
};

struct MatchArm : ExprScopeBlock {
	/// if match expr satisfies the pattern,
	///  binds variables from the pattern & executes 'expr'
	ExprMatch*	match_owner;
	Scope*		scope=0;
	Pattern*	pattern=0;
	Expr*		cond=0;
	Expr*		body=0;
	MatchArm*	next=0;
	void		dump(PrinterRef depth)const;
	Node*		clone() const override;
	Scope*		get_scope()override{return this->scope;}
	void		translate_tparams(const TParamXlat& tpx){}
	CgValue		compile_condition(CodeGen& cg, Scope* sc, const Pattern* match_expr,CgValue match_val);
	// todo - as patterns exist elsewhere, so 'compile-bind might generalize'.
	CgValue		compile_bind_locals(CodeGen& cg, Scope* sc, const Pattern* match_expr,CgValue match_val);
	CgValue			compile(CodeGen& cg, Scope* sc, CgValue input) override;

//	ResolveResult	resolve(Scope* sc, const Type* desired, int flags); doesn't have resolve method because it takes 2 inputs.
	void		recurse(std::function<void(Node*)>& f) override;
	const char*		kind_str() const {return "match arm";}
};

struct ExprIfLet : ExprMatch{	// sugar for match 1arm. parses+prints different. eval the same
	void		dump(PrinterRef depth)const;
	Node*		clone() const override {return this->clone_into(new ExprIfLet);}
	const char*		kind_str() const {return "kind str";}
	ExprIfLet(){};
	ExprIfLet(SrcPos p,Pattern* ptn, Expr* _expr, Expr* _body, Expr* _else);


};
struct ExprWhileLet : ExprFor{	// from rust, alternate sugar for destructuring assignment loop.
	//void		dump(int depth)const;
	//Node*		clone() const override {return this->clone_into(new ExprIfLet);}
	const char*		kind_str() const {return "kind str";}
	ResolveResult	resolve(Scope* sc, const Type* desired, int flags);
	CgValue			compile(CodeGen& cg, Scope* sc,CgValue input);
};




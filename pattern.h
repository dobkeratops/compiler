#pragma once
#include "ast.h"
#include "type.h"
#include "scope.h"
#include "semantics.h"
#include "exprstructdef.h"
#include "codegen.h"
#include "assist.h"


/// Pattern eg arguments or pattern matching, if-let
/// simplest must behave like 'ident'
struct Pattern : Node {
	ResolveResult	resolve(Scope* sc, const Type* desired, int flags)override;
	Pattern* next=0;
	Pattern* sub=0;
	Pattern* sub_pat(){return sub?sub->as_pattern():nullptr;}
	const Pattern* sub_pat()const{return sub?sub->as_pattern():nullptr;}
	Expr* sub_expr(){return sub?sub->as_expr():nullptr;}
	const Expr* sub_expr()const{return sub?sub->as_expr():nullptr;}
	void	set_sub_expr(Expr* e){ sub=(Pattern*)e;}
	Pattern(SrcPos _pos, Name n){pos=_pos,name=n;}
	int	get_elem_count();
	Pattern*	get_elem(int i);
	Pattern*	get_elem(int i,int ii){return get_elem(i)->get_elem(ii);}
	const Pattern*	get_elem(int i)const ;
	const Pattern*	get_elem(int i,int ii)const{return get_elem(i)->get_elem(ii);}
	void	push_back(Pattern* p);
	void	push_child(Pattern* p);
	void	dump(PrinterRef indent)const;
	Node*	clone()const;
	// if-let , args, or match arms would all call this.
	ResolveResult	resolve_with_type(Scope* sc, const Type* rhs, int flags);
	CgValue	compile(CodeGen& cg, Scope* sc, CgValue input) override;
	// subroutines of pattern compile, allows seperation into if (cond){bind..}
	// brute force just uses 'compile' and hopes llvm can optimize..
	CgValue	compile_condition(CodeGen& cg,Scope* sc, CgValue input);
	CgValue compile_bind(CodeGen& cg, Scope* sc, CgValue input);
	void	recurse(std::function<void(Node*)>& f);
	void	translate_tparams(const TParamXlat& xlat);
	const char* kind_str()const{return "pattern";}
	const Pattern* as_pattern()const{return this;}
	Pattern* as_pattern(){return this;}
	Name	as_name()const;
	bool	is_just_ident()const{return this->sub==nullptr;}
	Name	as_just_name()const{if (this){return this->sub==nullptr?this->name:0;}else return 0;}
};
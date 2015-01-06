#include "ast.h"
#include "exprflow.h"

void ExprIf::find_vars_written(Scope* s, set<Variable*>& vars) const{
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}

ResolveResult	ExprFor::resolve(Scope* outer_scope,const Type* desired,int flags){
	return this->resolve_for_sub(outer_scope,desired,flags);
}
ResolveResult	ExprFor::resolve_for_sub(Scope* outer_scope,const Type* desired,int flags){
	auto sc=outer_scope->make_inner_scope(&this->scope,outer_scope->owner_fn,this);

	resolved|=init->resolve_if(sc,0,flags);
	resolved|=cond->resolve_if(sc,Type::get_bool(),flags);
	resolved|=incr->resolve_if(sc,0,flags);
	resolved|=body->resolve_if(sc,desired,flags);
	if (else_block) {
		else_block->resolve_if(sc,desired,flags);
		propogate_type_refs(flags, (Node*)this, this->type_ref(), else_block->type_ref());
	}
	//without an else bllock, we can't return
	else{
		//propogate_type_fwd(flags, (Node*)this, Type::get_void(),this->body->type_ref());
	}
	auto dbg=[&](){
		newline(0);dbprintf("debug:for: this,else: this type\n");
		this->type()->dump_if(0);newline(0);
		newline(0);
		else_block->dump_if(0);newline(0);
	};
	propogate_type_fwd(flags, desired, this->type_ref());
	
	return resolved;
}


void ExprFor::translate_tparams(const TParamXlat& tpx)
{
	
	this->init->translate_typeparams_if(tpx);
	this->cond->translate_typeparams_if(tpx);
	this->incr->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}

void ExprFor::recurse(std::function<void(Node*)>& f){
	this->cond->recurse(f);
	this->init->recurse(f);
	this->body->recurse(f);
	this->else_block->recurse(f);
	this->type()->recurse(f);
}

Node* ExprFor::clone()const{
	auto n=new ExprFor(this->pos);
	return clone_for_sub(n);
}
Node* ExprFor::clone_for_sub(ExprFor* n)const{
	n->cond=(Expr*)cond->clone_if();
	n->incr=(Expr*)cond->clone_if();
	n->body=(Expr*)cond->clone_if();
	n->else_block=(Expr*)cond->clone_if();
	return n;
}
void ExprFor::find_vars_written(Scope* s, set<Variable*>& vars) const{
	incr->find_vars_written_if(s,vars);
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}


void ExprFor::dump(PrinterRef d) const {
	newline(d);dbprintf("(%s ",this->kind_str());
	this->init->dump_if(d+1); newline(d+1);dbprintf(";");
	this->cond->dump_if(d+1); newline(d+1);dbprintf(";");
	this->incr->dump_if(d+1); newline(d+1);dbprintf(" {");
/*	} else {
		this->pattern->dump(d+1);
		newline(d);dbprintf(" in ");
		this->init->dump(d+1); newline(d);dbprintf(" {");
	}
 */
	this->body->dump_if(d+1);
	newline(d+1);dbprintf("}");
	if (this->else_block){
		newline(d+1);dbprintf("else{");
		this->else_block->dump_if(d+1);
		newline(d+1);dbprintf("}");
	}
	newline(d+1);dbprintf(")");
	if(this->type()){
		dbprintf(":");
		this->type()->dump_if(d);
	}
}

ResolveResult ExprForIn::resolve(Scope *outer_scope, const Type *desired, int flags){
	auto sc=outer_scope->make_inner_scope(&this->scope,outer_scope->owner_fn,this);

	// for pattern in expr {...}
	// for __iter=expr; if let Some(pattern)=__iter.next();_ {....}
	// aka while let
	auto p=this->pos;
//	for __iter:=$expr;true;{
//		if let Some($pat)=&__iter.next(){$body;} else {break};
//	};

	if (!(this->init || this->cond || this->incr)){
		// set the muthur up.
		this->pattern->dump(0);newline(0);
		this->expr->dump(0);newline(0);
		this->body->dump(0);newline(0);
		if (this->else_block){this->else_block->dump(0);newline(0);}
		auto body_sub=body;
		auto elseb=this->else_block;
		auto _expr=this->expr;
		this->expr=nullptr;
		this->else_block=nullptr;
		this->body=nullptr;
		this->init=new ExprOp(LET_ASSIGN,p,(Expr*)new Pattern(pos,__ITERATOR), _expr);
		this->cond=new ExprLiteral(p,true);
		this->incr=new ExprLiteral(p,VOID);
		this->body=new ExprIfLet(// invokes a pattern binding vars
				p,
				new Pattern(p,E_SOME,this->pattern),
				new ExprOp(ADDR,p, // expression itr.next()
					nullptr,
					new ExprOp(DOT,p,
						new ExprIdent(p,__ITERATOR),
						new ExprCall(p,NEXT))),
				body_sub,         // loop body
				new ExprOp(BREAK,p,nullptr,elseb?elseb:new ExprLiteral(p)));
		this->else_block=elseb?(Expr*)elseb->clone():new ExprLiteral(p);
		dbg2({this->dump(0);newline(0);})
		dbg2({this->body->dump(0);newline(0);})
	}
	return resolve_for_sub(scope,desired,flags);
}

Node* ExprForIn::clone()const{
	auto fi=new ExprForIn();
	fi->pos=this->pos;
	fi->pattern=(Pattern*)pattern->clone_if();
	fi->init=(Expr*)init->clone_if();
	
	return this->clone_for_sub(fi);
}

void ExprForIn::translate_tparams(const TParamXlat& tpx) {
	this->expr->translate_tparams(tpx);
	this->pattern->translate_tparams(tpx);
	this->ExprFor::translate_tparams(tpx);
}



CgValue ExprFor::compile(CodeGen& cg, Scope* outer_sc, CgValue input){
	return cg.emit_for(this, this->init,this->cond, this->incr, this->body, this->else_block);
}


void ExprIf::translate_tparams(const TParamXlat& tpx){
	this->cond->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}

Node* ExprIf::clone()const {
	auto p=new ExprIf(this->pos);
	p->cond=(Expr*)this->cond->clone_if();
	p->body=(Expr*)this->body->clone_if();
	p->else_block=(Expr*)this->else_block->clone_if();
	::verify(p->cond->get_type());
	return p;
}
void ExprIf::dump(PrinterRef depth) const {
	::verify(cond->get_type());
	newline(depth);dbprintf("(if\n");
	cond->dump(depth+1);
	newline(depth);dbprintf("{\n");
	body->dump(depth+1);
	if (else_block)	{
		newline(depth);dbprintf("}else{\n");
		else_block->dump(depth+1);
	}
	newline(depth);dbprintf("})");
	if (this->type()) {
		dbprintf(":");
		this->type()->dump(-1);
	}
	newline(depth);
};

ResolveResult ExprIf::resolve(Scope* outer_s,const Type* desired,int flags){
	auto sc=outer_s->make_inner_scope(&this->scope,outer_s->owner_fn,this);
	
	::verify(this->cond->get_type());
	resolved|=this->cond->resolve_if(sc,nullptr,flags); // condition can  be anything coercible to bool
	resolved|=this->body->resolve_if(sc,desired,flags);
	Type* bt=this->body->type();
	if (else_block){
		resolved|=else_block->resolve_if(sc,desired,flags);
//		propogate_type_fwd(flags,this, desired,this->body->type_ref());
//		propogate_type_fwd(flags,this, desired,this->else_block->type_ref());
		// TODO: this belongs in 'propogate_type_refs' - merge 2 alternative types.
		if (auto cb=this->body->type()->get_common_base(this->else_block->type())){
			Type* t=nullptr;
			if (this->body->type()->is_pointer()){
				t=cb->ptr_type;
			}else if (this->body->type()->is_ref()){
				t=cb->ref_type;
			} else
				t=cb->struct_type;
			return propogate_type_fwd(flags, t, this->type_ref());
		} else {
			propogate_type_expr_ref(flags,this, body->type_ref());
		
			propogate_type_refs(flags,this, this->body->type_ref(), else_block->type_ref());
			propogate_type_refs(flags,this, this->type_ref(), else_block->type_ref());
#if DEBUG >2
			this->body->type()->dump_if(0);
			this->else_block->type()->dump_if(0);
			this->type()->dump_if(0);
#endif
			return propogate_type_refs(flags,this, this->type_ref(), this->body->type_ref());
		}
	}
	else {
		// TODO: Could it actually return Body|void ? perhaps we could implicityly ask for that?
		return resolved;
	}
}
void ExprIf::recurse(std::function<void(Node*)>& f){
	this->cond->recurse(f);
	this->body->recurse(f);
	this->else_block->recurse(f);
	this->type()->recurse(f);
}

CgValue ExprIf::compile(CodeGen& cg,Scope* sc, CgValue input) {
	// todo - while etc can desugar as for(;cond;)body, for(){ body if(cond)break}
	return cg.emit_if(this->get_scope(), input, this->cond, this->body, this->else_block, this->type());
}




Node*
ExprMatch::clone_into(ExprMatch* m)const{
	m->pos=pos;
	m->expr=(Expr*)expr->clone_if();
	m->arms=(MatchArm*)arms->clone_if();
	return m;
}
Node* MatchArm::clone()const{
	auto a=new MatchArm;
	a->pos=pos;
	a->body=(Expr*)body->clone();
	a->next=(MatchArm*)next->clone_if();//TODO not recursive ffs.
	return a;
}

void ExprMatch::dump(PrinterRef depth)const{
	newline(depth); dbprintf("match "); this->expr->dump(-1000);dbprintf("{");
	for (auto ma=this->arms; ma;ma=ma->next){
		ma->dump(depth>=0?depth+1:-1);
		if (ma->next) dbprintf(",");
	}
	newline(depth);dbprintf("}");
	
}
void ExprMatch::recurse(std::function<void(Node*)>& f){
	if (!this)return;
	this->expr->recurse(f);
	for (auto a=this->arms;a;a=a->next)
		a->recurse(f);
	this->type()->recurse(f);
}


CgValue MatchArm::compile_bind_locals(CodeGen &cg, Scope *sc, const Pattern *p, CgValue val)	{
	
	//ASSERT(0&&"TODO");
	return CgValue();
}
CgValue MatchArm::compile(CodeGen& cg, Scope* sc, CgValue match_val){
	auto arm=this;
	cg.emit_comment("}Match Arm{");
	if (!this) return CgValue();
	//auto match_val=this->match_owner->match_val;
	if (!arm->next){
		// error check. we just ignore condition. TypeChecker should hack checked it's exhaustive
		arm->pattern->compile(cg,sc,match_val);
		return arm->body->compile(cg,sc);
	}
	auto armsc=arm->get_scope();
	emit_local_vars(cg,arm,nullptr, armsc);
	auto ret= cg.emit_if(arm->get_scope(), match_val, arm->pattern, arm->body, arm->next, arm->type());
	return ret;
}

CgValue
ExprMatch::compile(CodeGen& cg, Scope* sc,CgValue input){
	cg.emit_comment("{Compile Match Expression");
	auto match_val = this->expr->compile(cg,sc);
	cg.emit_comment("Match Arms {");
	auto r= this->arms->compile(cg, sc, match_val);
	cg.emit_comment("}Match Arms}");
	return r;
}

ResolveResult
ExprMatch::resolve(Scope* outer_sc, const Type* desired, int flags){
	// the whole block gets a scope
	auto match_sc=outer_sc->make_inner_scope(&this->scope,outer_sc->owner_fn,this);

	resolved|=this->expr->resolve_if(match_sc,nullptr,flags);
	propogate_type_fwd(flags, desired, this->type_ref());

	for (auto a=this->arms; a;a=a->next ) {
		
		// every arm gets a scope
		auto arm_sc=outer_sc->make_inner_scope(&a->scope,match_sc->owner_fn,this);
		a->match_owner=this;
		// setup the types: expression has same type as match patterns;
		//*dont* push the type onto the arms, because variants cast to subtypes.
		//propogate_type(flags, (Node*)this, this->expr->type_ref(), a->pattern->type_ref());
		resolved|=a->pattern->resolve_with_type(a->scope, this->expr->type(), flags);
		resolved|=a->cond->resolve_if(a->scope, Type::get_bool(),flags);
		resolved|=a->body->resolve_if(a->scope, this->type(), flags);
		
		//all arms outputs have same typeas the whole output
		propogate_type_refs(flags,this, a->body->type_ref(),this->type_ref());
		propogate_type_refs(flags,this, a->body->type_ref(),a->type_ref());
		
	}
	return propogate_type_fwd(flags, desired, this->type_ref());
}

void MatchArm::dump(PrinterRef depth)const{
	auto d1=depth>=0?depth+1:depth;
	newline(depth);
	this->pattern->dump(-1);
	if (this->cond) {dbprintf("if"); this->cond->dump(-1000);}
	dbprintf("=>");
	this->body->dump(d1);
	
}
void MatchArm::recurse(std::function<void(Node *)> &f){
	if (!this)return;
	this->body->recurse(f);
	this->cond->recurse(f);
	this->pattern->recurse(f);
	this->type()->recurse(f);
}

void ExprIfLet::dump(PrinterRef depth)const{
	newline(depth);dbprintf("if let ");
	this->arms->pattern->dump(-100);
	dbprintf("=");
	this->expr->dump(-100); dbprintf("{");
	this->arms->body->dump(depth+1);
	dbprintf("}");
	newline(depth);
	if (this->arms->next){
		newline(depth);dbprintf("else{");
		this->arms->next->body->dump(depth+1);
		newline(depth);dbprintf("}");
	}
}
ExprIfLet::ExprIfLet(SrcPos p,Pattern* ptn, Expr* _expr, Expr* _body, Expr* _else){
	auto iflet=this;
	iflet->pos=p;
	MatchArm* arm=new MatchArm(); // if-let is a single match arm with different syntax.
	iflet->arms=arm;
	iflet->expr=_expr;
	arm->pattern=ptn;
	arm->body = _body;
	// 'else' if effectively an arm with _=>{}
	if (_else){
		auto else_arm=new MatchArm();
		arm->next=else_arm;
		else_arm->pattern=new Pattern(p,PLACEHOLDER);
		else_arm->body=_else;
	}
//	this->dump(0);newline(0);
}


ResolveResult	ExprWhileLet::resolve(Scope* sc, const Type* desired, int flags){
	return COMPLETE;
}

CgValue ExprWhileLet::compile(CodeGen& cg, Scope* sc, CgValue input) {
	error("todo, while-let support");return CgValue();
}




#include "ast.h"
#include "exprflow.h"

void ExprIf::find_vars_written(Scope* s, set<Variable*>& vars) const{
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}


ResolveResult	ExprFor::resolve(Scope* outer_scope,const Type* desired,int flags){
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
		propogate_type_fwd(flags, (Node*)this, Type::get_void(),this->body->type_ref());
	}
	auto dbg=[&](){
		newline(0);dbprintf("debug:for: this,else: this type\n");
		this->type()->dump_if(0);newline(0);
		newline(0);
		else_block->dump_if(0);newline(0);
	};
	propogate_type_fwd(flags, (Node*)this, desired, this->type_ref());
	
	return resolved;
}


void ExprIf::translate_tparams(const TParamXlat& tpx){
	this->cond->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}


void ExprFor::translate_tparams(const TParamXlat& tpx)
{
	this->init->translate_typeparams_if(tpx);
	this->cond->translate_typeparams_if(tpx);
	this->incr->translate_typeparams_if(tpx);
	this->pattern->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}

void ExprFor::recurse(std::function<void(Node*)>& f){
	this->cond->recurse(f);
	this->init->recurse(f);
	this->body->recurse(f);
	this->else_block->recurse(f);
	this->pattern->recurse(f);
	this->type()->recurse(f);
}


Node* ExprFor::clone()const{
	auto n=new ExprFor(this->pos);
	n->pattern=(Expr*)pattern->clone_if();
	n->init=(Expr*)init->clone_if();
	n->cond=(Expr*)cond->clone_if();
	n->incr=(Expr*)cond->clone_if();
	n->body=(Expr*)cond->clone_if();
	n->else_block=(Expr*)cond->clone_if();
	return n;
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
	newline(depth);dbprintf("})\n");
};

ResolveResult ExprIf::resolve(Scope* outer_s,const Type* desired,int flags){
	auto sc=outer_s->make_inner_scope(&this->scope,outer_s->owner_fn,this);
	
	::verify(this->cond->get_type());
	resolved|=this->cond->resolve_if(sc,nullptr,flags); // condition can  be anything coercible to bool
	resolved|=this->body->resolve_if(sc,desired,flags);
	Type* bt=this->body->type();
	if (else_block){
		propogate_type_fwd(flags,this, desired,bt);
		propogate_type_expr_ref(flags,this, body->type_ref());
		resolved|=else_block->resolve_if(sc,bt,flags);
		propogate_type_refs(flags,this, this->body->type_ref(), else_block->type_ref());
		propogate_type_refs(flags,this, this->type_ref(), else_block->type_ref());
		
#if DEBUG >2
		this->body->type()->dump_if(0);
		this->else_block->type()->dump_if(0);
		this->type()->dump_if(0);
#endif
		return propogate_type_refs(flags,this, this->type_ref(), this->body->type_ref());
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

void ExprFor::find_vars_written(Scope* s, set<Variable*>& vars) const{
	incr->find_vars_written_if(s,vars);
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}


void ExprFor::dump(PrinterRef d) const {
	newline(d);dbprintf("(for ");
	if (this->is_c_for()) {
		this->init->dump(d+1); newline(d);dbprintf(";");
		this->cond->dump(d+1); newline(d);dbprintf(";");
		this->incr->dump(d+1); newline(d);dbprintf(" {");
	} else {
		this->pattern->dump(d+1);
		newline(d);dbprintf(" in ");
		this->init->dump(d+1); newline(d);dbprintf(" {");
	}
	this->body->dump_if(d+1);
	newline(d);dbprintf("}");
	if (this->else_block){
		newline(d);dbprintf("else{");
		this->else_block->dump_if(d+1);
		newline(d);dbprintf("}");
	}
	newline(d);dbprintf(")");
	if(this->type()){
		dbprintf(":");
		this->type()->dump_if(d);
	}
}


CgValue ExprFor::compile(CodeGen& cg, Scope* outer_sc, CgValue input){
	return cg.emit_for(this, this->init,this->cond, this->incr, this->body, this->else_block);
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
	/*
	auto ret=cg.emit_if_sub
	(
		arm,
		sc,
		[&]{return arm->pattern->compile(cg,armsc, match_val);},
		[&]{
			arm->compile_bind_locals(cg,armsc,arm->pattern,match_val);
			return arm->body->compile(cg,armsc,CgValue());},
		[&]{return arm->next->compile(cg,armsc,input);},
	 	this->type()
	);*/
	return ret;
}

// refactoring: To do this more cleanly,
// we have to add an input-value to every '::compile' method - for most its' just void.
// Perhaps there are other places where temporary variables can be eliminated?
/*
CgValue MatchArm::compile(CodeGen& cg, Scope* sc, CgValue input) {
	auto armsc=arm->get_scope();
	emit_local_vars(cg,arm,nullptr,armsc);
	auto ret= cg.emit_if_sub(arm,sc, arm->pattern, arm->body, arm->next);
}
*/
CgValue
ExprMatch::compile(CodeGen& cg, Scope* sc,CgValue input){
	// TODO - dedicated with one set of phi-nodes at the end.
	// this is RETARDED!!!
	// There are many ways match could be optimized.
	// TODO - turn some cases into binary-chop (no 'if-guards' & single value test)
	// TODO - turn some cases into vtable.
	// TODO - extract common conditions.
	cg.emit_comment("{Compile Match Expression");
	auto match_val = this->expr->compile(cg,sc);
	cg.emit_comment("Match Arms {");
	//this->match_val=match_val;	// TODO this is a bit messy. the match arms get a backpointer
								// The alternative is to add an extra value passed along expressions which most compile methods ignore.
	auto r= this->arms->compile(cg, sc, match_val);
	cg.emit_comment("}Match Arms}");
	return r;
}

ResolveResult
ExprMatch::resolve(Scope* outer_sc, const Type* desired, int flags){
	// the whole block gets a scope
	auto match_sc=outer_sc->make_inner_scope(&this->scope,outer_sc->owner_fn,this);

	resolved|=this->expr->resolve_if(match_sc,nullptr,flags);
	propogate_type_fwd(flags,this, desired, this->type_ref());

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
	return propogate_type_fwd(flags,this, desired, this->type_ref());
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
	this->arms->pattern->dump(-1);
	dbprintf("=");
	this->expr->dump(-1); dbprintf("{");
	this->arms->body->dump(depth+1);
	dbprintf("}");
	newline(depth);
	if (this->arms->next){
		newline(depth);dbprintf("else{");
		this->arms->next->body->dump(depth+1);
		newline(depth);dbprintf("}");
	}
}
CgValue ExprWhileLet::compile(CodeGen& cg, Scope* sc, CgValue input) {
	error("todo, while-let support");return CgValue();
}




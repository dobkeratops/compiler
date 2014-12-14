
#include "exprflow.h"

void ExprIf::find_vars_written(Scope* s, set<Variable*>& vars) const{
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}

ResolvedType	ExprFor::resolve(Scope* outer_scope,const Type* desired,int flags){
	auto sc=outer_scope->make_inner_scope(&this->scope,outer_scope->owner_fn,this);
	init->resolve_if(sc,0,flags);
	cond->resolve_if(sc,Type::get_bool(),flags);
	incr->resolve_if(sc,0,flags);
	body->resolve_if(sc,desired,flags);
	if (else_block) {
		else_block->resolve(sc,desired,flags);
		propogate_type(flags, (Node*)this, this->type_ref(), else_block->type_ref());
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
	
	return ResolvedType();
}


void ExprIf::translate_typeparams(const TypeParamXlat& tpx){
	this->cond->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}
void ExprFor::translate_typeparams(const TypeParamXlat& tpx)
{
	this->init->translate_typeparams_if(tpx);
	this->cond->translate_typeparams_if(tpx);
	this->incr->translate_typeparams_if(tpx);
	this->pattern->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
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
void ExprIf::dump(int depth) const {
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

ResolvedType ExprIf::resolve(Scope* outer_s,const Type* desired,int flags){
	auto sc=outer_s->make_inner_scope(&this->scope,outer_s->owner_fn,this);
	
	::verify(this->cond->get_type());
	this->cond->resolve(sc,nullptr,flags); // condition can  be anything coercible to bool
	auto body_type=this->body->resolve(sc,desired,flags);
	Type* bt=body_type.type;
	if (else_block){
		propogate_type_fwd(flags,this, desired,bt);
		propogate_type(flags,this, bt);
		else_block->resolve(sc,bt,flags);
		propogate_type(flags,this, this->body->type_ref(), else_block->type_ref());
		propogate_type(flags,this, this->type_ref(), else_block->type_ref());
		
#if DEBUG >2
		this->body->type()->dump_if(0);
		this->else_block->type()->dump_if(0);
		this->type()->dump_if(0);
#endif
		return propogate_type(flags,this, this->type_ref(), this->body->type_ref());
	}
	else {
		// TODO: Could it actually return Body|void ? perhaps we could implicityly ask for that?
		return body_type;
	}
}

void ExprFor::find_vars_written(Scope* s, set<Variable*>& vars) const{
	incr->find_vars_written_if(s,vars);
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}


void ExprFor::dump(int d) const {
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



Node*
ExprMatch::clone()const{
	auto m=new ExprMatch();
	m->pos=pos;
	m->expr=(Expr*)expr->clone_if();
	m->arms=(MatchArm*)arms->clone_if();
	return m;
}
Node* MatchArm::clone()const{
	auto a=new MatchArm();
	a->pos=pos;
	
	a->body=(Expr*)body->clone();
	a->next=(MatchArm*)next->clone_if();//TODO not recursive ffs.
	return a;
}

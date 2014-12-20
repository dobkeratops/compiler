#include "ast.h"
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
void ExprIf::recurse(std::function<void(Node*)>& f){
	this->cond->recurse(f);
	this->body->recurse(f);
	this->else_block->recurse(f);
	this->type()->recurse(f);
}

CgValue ExprIf::compile(CodeGen& cg,Scope*sc){
	// todo - while etc can desugar as for(;cond;)body, for(){ body if(cond)break}
	return cg.emit_if(this, this->cond, this->body, this->else_block, this->type());
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


CgValue ExprFor::compile(CodeGen& cg, Scope* outer_sc){
	return cg.emit_for(this, this->init,this->cond, this->incr, this->body, this->else_block);
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

void ExprMatch::dump(int depth)const{
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


CgValue MatchArm::compile_condition(CodeGen &cg, Scope *sc, const Pattern *ptn, CgValue val){
	// emit a condition to check if the runtime value  fits this pattern.
	// TODO-short-curcuiting - requires flow JumpToElse.

	// single variable bind.
	if (ptn->name==PTR ||ptn->name==REF){
		return compile_condition(cg, sc, ptn->sub, val.deref_op(cg));
	}
	if (ptn->name==PLACEHOLDER){
		return cg.emit_bool(true);
	}
	if (ptn->name==PATTERN_BIND){
		auto v=ptn->get_elem(0);
		auto p=ptn->get_elem(1);
		auto disr=cg.emit_loadelement(val, __DISCRIMINANT);
		auto ps=p->type()->is_pointer_or_ref()?p->sub:p;
		auto sd=ps->def->as_struct_def();
		auto b=cg.emit_instruction(EQ, disr, cg.emit_i32(sd->discriminant));
		auto var=v->def->as_variable();
		CgValue(var).store(cg, cg.emit_conversion((Node*)ptn, val, var->type(), sc));// coercion?

		return b;
	}
	if (ptn->name==OR || ptn->name==TUPLE ||ptn->sub){// iterate components...
		int index;
		CgValue ret;
		CgValue	val2=val;
		Name op;
		if (ptn->name==OR) {op=LOG_OR;index=0;}
		else if(ptn->name==TUPLE){op=LOG_AND;index=0;}
		else{
			auto disr=cg.emit_loadelement(val, __DISCRIMINANT);
			auto ptns=ptn->type()->is_pointer_or_ref()?ptn->sub:ptn;
			auto sd=ptns->def->as_struct_def();
			index=sd->first_user_field_index();
			ret=cg.emit_instruction(EQ, disr, cg.emit_i32(sd->discriminant));
			val2=cg.emit_conversion((Node*)ptn,val, ptn->type(),sc);
			dbg2(val2.type->dump(0));dbg2(newline(0));
		}
		//todo - this part moves to bind if not or/tople
		for (auto subp=ptn->sub; subp; subp=subp->next,index++){
			auto elem=ptn->name!=OR?cg.emit_getelementref(val2,0, index,subp->type()):val;
			auto b=this->compile_condition(cg, sc, subp, elem);
			if (op){
				if (!ret.is_valid()) ret=b;
				else ret=cg.emit_instruction(op,ret,b);
			}
		}
		return ret;
	}
	else if (auto var=ptn->def->as_variable()){
		dbg(dbprintf("bind %s :",var->name_str()));dbg(var->type()->dump_if(-1));dbg(newline(0));
		dbg(dbprintf("given val :",var->name_str()));dbg(val.type->dump_if(-1));dbg(newline(0));dbg(ptn->type()->dump_if(-1));dbg(newline(0));
		CgValue(var).store(cg, val);
		return cg.emit_bool(true);
	}else
	// single value
	if (auto lit=ptn->def->as_literal()){
		return	cg.emit_instruction(EQ, CgValue(lit), val);
	}
	ptn->dump(0);
	error(ptn,"uncompiled node");
	return CgValue();
}
CgValue MatchArm::compile_bind_locals(CodeGen &cg, Scope *sc, const Pattern *p, CgValue val)	{
	
	//ASSERT(0&&"TODO");
	return CgValue();
}
CgValue MatchArm::compile(CodeGen& cg, Scope* sc){
	cg.emit_comment("}Match Arm{");
	if (!this) return CgValue();
	auto match_val=this->match_owner->match_val;
	auto arm=this;
	if (!arm->next){
		// error check. we just ignore condition. TypeChecker should hack checked it's exhaustive
		arm->compile_bind_locals(cg,sc,arm->pattern,match_val);
		return arm->body->compile(cg,sc);
	}
	auto armsc=arm->get_scope();
	emit_local_vars(cg,arm,nullptr, armsc);
	auto ret=cg.emit_if_sub
	(
		arm,
		sc,
		[&]{return arm->compile_condition(cg,armsc, arm->pattern, match_val);},
		[&]{
			arm->compile_bind_locals(cg,armsc,arm->pattern,match_val);
			return arm->body->compile(cg,armsc);},
		arm->next,
	 	this->type()
	);
	return ret;
}

CgValue
ExprMatch::compile(CodeGen& cg, Scope* sc){
	// TODO - dedicated with one set of phi-nodes at the end.
	// this is RETARDED!!!
	// There are many ways match could be optimized.
	// TODO - turn some cases into binary-chop (no 'if-guards' & single value test)
	// TODO - turn some cases into vtable.
	// TODO - extract common conditions.
	cg.emit_comment("{Compile Match Expression");
	auto match_val = this->expr->compile(cg,sc);
	cg.emit_comment("Match Arms {");
	this->match_val=match_val;	// TODO this is a bit messy. the match arms get a backpointer
								// The alternative is to add an extra value passed along expressions which most compile methods ignore.
	auto r= this->arms->compile(cg, sc);
	cg.emit_comment("}Match Arms}");
	return r;
}

ResolvedType
ExprMatch::resolve(Scope* outer_sc, const Type* desired, int flags){
	// the whole block gets a scope
	auto match_sc=outer_sc->make_inner_scope(&this->scope,outer_sc->owner_fn,this);

	this->expr->resolve(match_sc,nullptr,flags);
	propogate_type_fwd(flags,this, desired, this->type_ref());

	for (auto a=this->arms; a;a=a->next ) {
		
		// every arm gets a scope
		auto arm_sc=outer_sc->make_inner_scope(&a->scope,match_sc->owner_fn,this);
		a->match_owner=this;
		// setup the types: expression has same type as match patterns;
		//*dont* push the type onto the arms, because variants cast to subtypes.
		//propogate_type(flags, (Node*)this, this->expr->type_ref(), a->pattern->type_ref());
		a->pattern->resolve_with_type(a->scope, this->expr->type(), flags);
		a->cond->resolve_if(a->scope, Type::get_bool(),flags);
		a->body->resolve(a->scope, this->type(), flags);
		
		//all arms outputs have same typeas the whole output
		propogate_type(flags,this, a->body->type_ref(),this->type_ref());
		propogate_type(flags,this, a->body->type_ref(),a->type_ref());
		
	}
	return propogate_type_fwd(flags,this, desired, this->type_ref());
}

void MatchArm::dump(int depth)const{
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



#include "node.h"
#include "type.h"
#include "codegen.h"

void Node::clear_def(){
	if (def)
		def->remove_ref(this);
	def=nullptr;;
}
void Node::set_def(ExprDef *d){
	if (!d && !def)
		return;
	if (!def) {
		this->next_of_def=d->refs; d->refs=this;
		def=d;
	}
	else {
		if (d==0 && this->def){ ASSERT("use clear_def(), not set_def(0)");}
		if (d!=this->def){
			dbprintf("WARNING!!-was %d %s now %d %s\n",def->pos.line,def->name_str(), d->pos.line,d->name_str());
			//			ASSERT(d==this->def);
		}
		def->remove_ref(this);
		def=d;
	}
}
void Node::set_struct_type(ExprDef* sd){
	
	if (!this->type())
		this->set_type(new Type(sd->as_struct_def()));
	if (!this->def)
		this->set_def(sd);
}
void Node::set_type(const Type* t)
{	::verify(t);
	if (this->m_type){
		if (this->m_type->is_equal(t))
			return ;
		if (!t){
			ASSERT(0 && "use clear type");
		}

#if DEBUG>=2
		dbprintf("changing type?\n");
		this->m_type->dump(-1);newline(0);
		dbprintf("to..\n");
		t->dump(-1);
		newline(0);
#endif
		//ASSERT(this->m_type==0);
	}
	this->m_type=(Type*)t;
};

ExprStructDef* Node::as_struct_def()const{
	return nullptr;
};
const char* Node::get_name_str() const{
	if (!this) return "(no_type)";
	return getString(this->name);
}



RegisterName Node::get_reg_existing(){ASSERT(reg_name); return reg_name;}
RegisterName Node::get_reg(CodeGen& cg, bool force_new){
	// variable might be in a local scope shadowing others, so it still needs a unique name
	// names are also modified by writes, for llvm SSA
	//ASSERT(!on_stack);
	if (!reg_name || force_new){
		auto old=reg_name;
		auto ret= get_reg_new(cg);
		return ret;
	} else{
		return reg_name;
	}
}
RegisterName Node::get_reg_new(CodeGen& cg) {
	return this->reg_name=cg.next_reg(name);
}

CgValue Node::compile_if(CodeGen& cg, Scope* sc){
	if (this)
		return this->compile(cg,sc,CgValue());
	else
		return CgValueVoid();
}

CgValue Node::compile(CodeGen&, Scope*,CgValue in){
	error(this,"compile not implemented for %s",this->kind_str());
	return in;
}
CgValue Node::compile(CodeGen& cg, Scope* sc){
	// most common case is no input value, input is added for 'match' & 'switch'.
	return this->compile(cg,sc,CgValue());
}
CgValue Node::compile_operator_new(CodeGen &cg, Scope *sc,const Type* t,const Expr *lhs){
	error(this,"operator new not supported for %s",this->kind_str());
	return CgValue();
}



CgValue Node::codegen(CodeGen& cg, bool just_contents) {
	dbprintf("TODO refactor codegen to use this virtual. warning codegen not implemented for %s\n",this->kind_str());
	return CgValue();
}

const Type* any_not_zero(const Type* a, const Type* b){return a?a:b;}
ResolveResult Node::propogate_type_refs(int flags,const Node*n, Type*& a,Type*& b) {
	::verify(a,b);
	if (!(a || b))
		return resolved|=ResolveResult(INCOMPLETE);
	if (!a && b) {
		a=b;
		return resolved|=ResolveResult(COMPLETE);}
	else if (!b && a) {
		b=a;
		return resolved|=ResolveResult(COMPLETE);
	}
	return resolved|=assert_types_eq(flags,n, a,b);
}
ResolveResult Node::propogate_type_refs(int flags, Expr *n, Type*& a,Type*& b) {
	::verify(a,b);
	resolved|=propogate_type_refs(flags,(const Node*)n,a,b);
	resolved|=propogate_type_refs(flags,(const Node*)n,n->type_ref(),b);
	return resolved|=propogate_type_refs(flags,(const Node*)n,n->type_ref(),a);
}
ResolveResult Node::propogate_type_fwd(int flags,const Node* n, const Type* a,Type*& b) {
	::verify(a,b);
	if (!(a || b))
		return resolved|=ResolveResult(INCOMPLETE);
	if (!a && b){
		return resolved|=ResolveResult(INCOMPLETE);
	}
	if (!b && a) {
		b=(Type*)a;
		return resolved|=ResolveResult(COMPLETE);
	}
	return resolved|=assert_types_eq(flags,n, a,b);
	
	return ResolveResult(INCOMPLETE);
}
ResolveResult Node::propogate_type_fwd(int flags,Expr* e, const Type*& a) {
	return resolved|=propogate_type_fwd(flags,e, a, e->type_ref());
}
ResolveResult Node::propogate_type_expr_ref(int flags,Expr* e, Type*& a) {
	return resolved|=propogate_type_refs(flags,e, a, e->type_ref());
}

ResolveResult Node::propogate_type_refs(int flags,const Node* n, Type*& a,Type*& b,Type*& c) {
	::verify(a,b,c);
	int ret=COMPLETE;
	ret|=propogate_type_refs(flags,n,a,b);
	ret|=(c)?propogate_type_refs(flags,n,b,c):INCOMPLETE;
	ret|=(c)?propogate_type_refs(flags,n,a,c):INCOMPLETE;
	return resolved|=ResolveResult(ret);
}
ResolveResult Node::propogate_type_fwd(int flags,const Node* n,const Type*& a,Type*& b,Type*& c) {
	::verify(a,b,c);
	int ret=COMPLETE;
	ret|=propogate_type_fwd(flags,n,a,b);
	ret|=propogate_type_fwd(flags,n,a,c);
	ret|=propogate_type_refs(flags,n,b,c);
	return resolved|=ResolveResult(ret);
}

void	ExprDef::remove_ref(Node* ref){
	Node** pp=&refs;
	Node* p;
	auto dbg=[&](){
		for (auto r=refs; r; r=r->next_of_def){
			dbprintf("ref by %p %s %s %d\n",r, r->kind_str(), str(r->name),r->	pos.line);
		}
	};
	for (p=*pp; p; pp=&p->next_of_def, p=*pp) {
		if (p==ref) *pp=p->next_of_def;
	}
	ref->def=0;
}

Node*
ExprIdent::clone() const {
	auto r=new ExprIdent(this->name,this->pos);
	r->set_type(this->get_type());	// this is where given tparams live.
	r->clear_def();	// def will need re-resolving.
	return r;
}
TypeDef* TypeDef::clone()const{
	TypeDef* td = new TypeDef(this->pos, this->name);
	// todo - clone tparams
	td->type_def = (Type*) this->type_def->clone();// actual ->type() will be translated
	return td;
}
void TypeDef::dump(PrinterRef depth) const{
	newline(depth); dbprintf("%s %s",this->kind_str() ,this->name_str()); dump_typeparams(this->tparams); dbprintf("=");
	this->type_def->dump(-1);
	if (this->type()){dbprintf(":"); this->type()->dump_if(depth);}
	dbprintf(";");
}


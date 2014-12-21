#include "everywhere.h"
#include "stringtable.h"
#include "node.h"
#include "ast.h"
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


CgValue Node::codegen(CodeGen& cg, bool just_contents) {
	dbprintf("TODO refactor codegen to use this virtual. warning codegen not implemented for %s\n",this->kind_str());
	return CgValue();
}




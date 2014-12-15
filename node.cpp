#include "everywhere.h"
#include "stringtable.h"
#include "node.h"
#include "ast.h"
#include "codegen.h"

bool Node::is_ident()const {
	return (dynamic_cast<const ExprIdent*>(this)!=0);
}


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

ExprStructDef* Node::as_struct_def()const{
	//error(this,"expect struct def");
	return nullptr;
};
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


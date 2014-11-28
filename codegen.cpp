#include "codegen.h"

// TODO: properly abstract llvm instruction generation to move to llvm api.
inline void dbprintf_mangle(const char*,...){}
extern Name getStringIndex(const char* str,const char* end) ;
extern const char* getString(const Name&);
extern bool is_operator(Name tok);
extern bool is_ident(Name tok);
extern bool is_type(Name tok);
void write_reg(CodeGen& cg, RegisterName dst );
void write_struct_name(CodeGen& cg, RegisterName dst ){write_reg(cg,dst);}
void write_type(CodeGen& cg, CgValue& lv );
void write_type(CodeGen& cg, const Type* t, bool is_ref=false);
void write_type_reg(CodeGen& cg, const Type* t,bool ref, Name reg);
void write_function_type(CodeGen& cg,ExprFnDef* fn_node);
void write_function_type(CodeGen& cg, const Type* t);
void write_global(CodeGen& cg, Name n);
CgValue alloca_type(CodeGen& cg, Expr* holder, Type* t);

Name next_reg_name(int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d",(*next_reg_index)++);
	return getStringIndex(tmp);
}
Name next_reg_name(Name prefix_name, int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d%s",(*next_reg_index)++,str(prefix_name));
	return getStringIndex(tmp);
}
void write_reg(CodeGen& cg, RegisterName reg);

void ins_begin(CodeGen& cg){fprintf(cg.ofp,"\t"); cg.comma=false;}
void ins_begin_name(CodeGen& cg,const char* txt){fprintf(cg.ofp,"\t%s ",txt);}
void write_ins_name(CodeGen& cg,const char* txt){fprintf(cg.ofp,"= %s ",txt);}
void ins_end(CodeGen& cg){fprintf(cg.ofp,"\n");}
void struct_begin(CodeGen& cg){
	ASSERT(cg.depth<32);
	cg.commas[cg.depth]=cg.comma;
	cg.depth++;
	fprintf(cg.ofp,"{");
}
void struct_end(CodeGen& cg){
	ASSERT(cg.depth);
	cg.depth--;
	cg.comma=cg.commas[cg.depth];
	fprintf(cg.ofp,"}");
}
void write_comma(CodeGen& cg){
	if (cg.comma){fprintf(cg.ofp,",");}
	cg.comma=true;
}
void ins_begin_reg_op(CodeGen& cg, Name reg, const char* op){
	ins_begin(cg);
	write_reg(cg,reg);
	write_ins_name(cg,op);
}
void write_i32_lit(CodeGen& cg,int index) {
	write_comma(cg);
	fprintf(cg.ofp,"i32 %d",index);
}
void write_i32_reg(CodeGen& cg,Name reg) {
	write_comma(cg);
	fprintf(cg.ofp,"i32 ");
	write_reg(cg,reg);
}
void write_store(CodeGen& cg, RegisterName reg, Type* type, RegisterName addr){
	ins_begin_name(cg,"store");
	write_type(cg, type,0);
	write_reg(cg, reg); fprintf(cg.ofp,",");
	write_type(cg, type,1);//fprintf(ofp,"* ");
	write_reg(cg, addr); fprintf(cg.ofp,", align 4");
	ins_end(cg);
}
void write_global(CodeGen& cg, Name n){
	fprintf(cg.ofp,"@%s ",str(n));
}
void write_fn_ptr(CodeGen& cg, Name n){
	fprintf(cg.ofp,"@%s.ptr",str(n));
}
void write_struct_ptr(CodeGen& cg, Name n){
	fprintf(cg.ofp,"%%%s*",str(n));
}
void write_txt(CodeGen& cg, const char* str,...){// catch all, just spit out given string
	char tmp[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	fprintf(cg.ofp,"%s",tmp);
}
void write_comment(CodeGen& cg, const char* str,...){// catch all, just spit out given string
	char tmp[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	fprintf(cg.ofp,"\t;%s\n",tmp);
}

struct CgValue {	// lazy-access abstraction for value-or-ref. So we can do a.m=v or v=a.m. One is a load, the other is a store. it may or may not load/store either side of the instruction. a 'variable' is included here as a form of 'adress', for var+= ...
	// TODO: this should be a tagged-union?
	// these values aren't persistent so it doesn't matter too much.
	RegisterName reg;
	int elem;     // if its a struct-in-reg
	Type* type;
	RegisterName addr;
	Node*	val;		// which AST node it corresponds to
	int ofs;
	explicit CgValue(RegisterName n,Type* t):reg(n),type(t){elem=-1;addr=0;ofs=0;val=0;}
	explicit CgValue(RegisterName v,Type* t,RegisterName address_reg,int elem_index=-1):reg(v){elem=elem_index;reg=v;addr=address_reg; type=t;ofs=0;val=0;}
	explicit CgValue(Expr* n) {
		// todo - unify with 'expr'
		addr=0; reg=0; ofs=0;elem=-1;
		val = n;
		if (auto fd=dynamic_cast<ExprFnDef*>(n)){ // variable is a function pointer?
			reg=0; // it needs to be loaded
		}
		if (auto v=dynamic_cast<Variable*>(n)){
			if (v->reg_is_addr){
				addr=v->regname;
			}
			else{
				reg=v->regname;
			};
		}
		this->type=n->type();
	}
	CgValue():reg(0),addr(0),ofs(0),val(0),type(nullptr){};
//	bool is_fn()const{return reg==0 && val==0 && type->name==FN && val!=0 && val->as_fn_def()!=0;}
	bool is_struct_elem()const{return elem>=0;}
	bool is_valid()const{return val!=0||reg!=0;}
	bool is_literal()const{return dynamic_cast<ExprLiteral*>(val)!=0;}
	bool is_reg()const { return reg!=0;}
	bool is_any()const{return is_literal()||is_reg();}
	bool is_addr() const {return reg==0 && val==0;}
	CgValue addr_op(CodeGen& cg,Type* t) { // take type calculated by sema
		ASSERT(this->type);
		if (!reg && (bool)addr) {	// we were given a *reference*, we make the vlaue the adress
			ASSERT(t->name==PTR);
			ASSERT(t->sub->eq(this->type));
			return CgValue(addr,t,0);
		} else {
			return this->to_stack(cg).addr_op(cg,t);
			// must return adress-of-adress
			// path must have been flagged as a memvar.
			// or we must have been given array-ref[i], or 'dot' member ref.
			this->type->dump_if(0);
			t->dump_if(0);
			
			ASSERT(0 && "trying to take adress of entity which is in a register. TODO comit such vars to stack");
			return CgValue();
		}
	}
	CgValue to_stack(CodeGen& cg){
		CgValue stack_val(0,this->type,alloca_type(cg,nullptr,this->type).addr,this->elem);
		stack_val.store(cg,this->reg);
		return stack_val;
	}
	CgValue deref_op(CodeGen& cg, Type* t) {
		// todo type assertion 't' is the output type.
		if (!addr) {		// the value we were given is now the *adress* - we return a reference, not a pointer.
			return CgValue(0,t,reg);
		} else {
			// it its'  a reference type, we need to load that first.
			auto ret=load(cg);
			return CgValue(0,t,ret); // ... and return it as another reference: eg given T&* p,  returning T& q=*p
		}
	}
	
	RegisterName load(CodeGen& cg,RegisterName force_regname=0) {
		auto ofp=cg.ofp;
		if (elem>=0){
			if (this->type->is_pointer()) {
				write_comment(cg,"dot reg=%s addr=%s index=%d",str(reg),str(addr),elem);
				auto sub=this->dot_sub(cg,elem);
				return sub.load(cg);
			} else{
				// elem acess
				auto newreg=(bool)force_regname?force_regname:next_reg_name(&cg.next_reg);
				ins_begin_reg_op(cg,newreg,"extractelement");
				write_type_reg(cg,this->type,false,reg);
				write_i32_lit(cg,this->elem);
				ins_end(cg);
				this->type=this->type->get_elem(elem);
				this->reg=newreg;
				return newreg;
			}
		}
		if(val) {
			if (force_regname){reg=force_regname;}
			else if (!reg) reg=next_reg_name(&cg.next_reg);
			
			if (auto lit=dynamic_cast<ExprLiteral*>(val)){
				ins_begin(cg);
				write_reg(cg,reg);
				if (lit->type()->name==INT){
					write_ins_name(cg,"or");
					write_i32_lit(cg,0);  this->write_literal(cg,lit);
				} else if(lit->type()->name==FLOAT){
					// todo, i guess we're goint to have t make a global constants table
					write_ins_name(cg,"fadd");
					fprintf(cg.ofp,"float 0.0, ");  this->write_literal(cg,lit);
				} else if (lit->type()->name==STR){
					write_ins_name(cg,"getelementptr inbounds");
					fprintf(cg.ofp,"[%d x i8]* @%s",lit->llvm_strlen, getString(lit->name));
					write_comma(cg);
					write_i32_lit(cg,0);
					write_i32_lit(cg,0);
					ASSERT(lit->llvm_strlen);
				} else {
					error(lit,"literal type not handled yet");
				}
				ins_end(cg);
				// todo: string literal, vector literals, bit formats
			}
			else if (auto fp=dynamic_cast<ExprFnDef*>(val)){
				ins_begin_reg_op(cg,reg,"load");
				// function type..
				write_type(cg,this->type,true);
				write_fn_ptr(cg,fp->get_mangled_name());
				ins_end(cg);
			}
			else if (auto v=dynamic_cast<Variable*>(val)){
//				fprintf(ofp,"\t%%%s = load ", str(reg));
				// function type..
//				write_type(ofp,this->type,false);
//				fprintf(ofp,"* %%%s\n",str(val->regname));
				if (v->reg_is_addr){reg=addr=v->regname;}else{reg=v->regname;}
//				ret=v->regname;
				return reg;
			}
		}
		if ((bool)addr) {
			if (force_regname){reg=force_regname;}
			else if (!reg) reg=next_reg_name(&cg.next_reg);
			ins_begin_reg_op(cg,reg,"load");
			write_type_reg(cg, type, addr!=0,addr);// an extra pointer level if its' a reference
			ins_end(cg);
			addr=0;
			return reg;
		}
		ASSERT(reg);
		return reg;
	}
	void write_literal(CodeGen& cg,ExprLiteral* lit){
		if (cg.comma){fprintf(cg.ofp,",");} cg.comma=true;
		auto ofp=cg.ofp;
		switch (lit->type_id) {
			case T_INT:
				fprintf(ofp," %d ",lit->u.val_int);
				break;
			case T_UINT:
				fprintf(ofp," %u ",lit->u.val_uint);
				break;
			case T_FLOAT:
				fprintf(ofp," %f ",lit->u.val_float);
				break;
				//					getelementptr inbounds ([21 x i8]* @.str3, i32 0, i32 0)
			case T_CONST_STRING:
				fprintf(ofp," getelementptr inbounds([%d x i8]* @%s, i32 0, i32 0) ", lit->llvm_strlen, getString(lit->name));
				break;
			default:
				fprintf(ofp," @TODO_LITERAL_FORMAT %s", getString(lit->name));
				break;
			}
	}
	void write_operand(CodeGen& cg){
		auto ofp=cg.ofp;
		if (reg!=0){
			write_reg(cg,reg);
			return;
		} else if (val){
			if (auto lit=dynamic_cast<ExprLiteral*>(val)) {
				write_literal(cg,lit);
			}
			return;
		} else {
			fprintf(ofp," <?CgV?> ");
			this->type->dump_if(0);
			ASSERT(this->type);
			ASSERT(0 && "missing register type");
		}
	}
	RegisterName store(CodeGen& cg,Name srcreg=0){// for read-modify-write
		ASSERT(type);
		if (!addr || !type)
			return reg;
		write_store(cg, (bool)srcreg?srcreg:reg, type, addr);
		return reg;
	}

	RegisterName store_from(CodeGen& cg,RegisterName valreg){
		auto ofp=cg.ofp;
		if (val){
			val->regname=valreg;
			reg=valreg;
		}
		if (elem>=0){
			auto srcreg=valreg;
			auto newreg=next_reg_name(&cg.next_reg);
			ins_begin_reg_op(cg,newreg,"insertelement");
			write_type_reg(cg,this->type,false,reg); write_comma(cg);
			auto elem_t=this->type->get_elem(elem);
			write_type(cg,elem_t);fprintf(ofp," ");
			write_reg(cg,valreg);
			write_i32_lit(cg, this->elem);
//			fprintf(ofp,", i32 %d", this->elem);
			ins_end(cg);
			this->reg=newreg;
			// elem remains - this value has been mutated.
			// TODO, i think this would be much more elegant to return a new CgValue.
			return newreg;
		}
		else if (addr && type) {
			if (reg!=0){dbprintf("warning %s overwrite %s?\n", str(reg),str(valreg));}
			ASSERT(reg==0);
			reg=valreg;
			write_store(cg, valreg, type, addr);
		}
		return valreg;
	}
	CgValue dot(CodeGen& cg,const Node* field_name,Scope* sc){	//calculates & returns adress
		auto ofp=cg.ofp;
		if (!type){
			write_comment(cg,"ERROR %s:%d:",__FILE__,__LINE__);return CgValue();
		}
		ASSERT(type );
		auto sd=type->deref_all()->struct_def;
		if (!sd) {type->dump(-1);error(field_name,"struct not resolved\n");}
//		auto sd=type->struct_def?type->struct_def:sc->find_struct(type);
//		if (!sd){
//			fprintf(ofp,"\t;%s.%s\n",type?getString(type->name):"???",getString(field_name));
//			type->dump(-1);
//			fprintf(ofp,";ERROR %s:%d:\n;",__FILE__,__LINE__);return CgValue();
//		}
		int index=sd->field_index(field_name);
		auto field=sd->find_field(field_name);
		return dot_sub(cg,index);
	}
	CgValue dot_sub(CodeGen& cg, int field_index){
		auto ofp=cg.ofp;
		if ((bool)reg && !addr && !(this->type->is_pointer())){			// lazy ref to field index,turn into extract/insert for load/store
			write_comment(cg,"dot reg=%s index=%d",str(reg),field_index);
			return CgValue(reg,type,0,field_index);
		}
		else {
			auto basereg=(bool)reg?reg:addr;
			ASSERT(basereg);			// TODO: detail: if it's addr, extra or lesser *?
			auto areg=next_reg_name(&cg.next_reg);
			auto ptr_t=type;if (ptr_t->name==PTR) ptr_t=ptr_t->sub;
			auto sd=ptr_t->struct_def;
			auto field=sd->fields[field_index];
			write_comment(cg,"dot reg=%s addr=%s",str(reg),str(addr));
			write_comment(cg,"%s.%s :%s",str(type->name),str(field->name),str(field->type()->name));
			ins_begin_reg_op(cg,areg, "getelementptr inbounds");
			write_struct_ptr(cg,sd->get_mangled_name());
			write_reg(cg,basereg);
			write_comma(cg);
			//cg.comma=true;
			write_i32_lit(cg,0);
			write_i32_lit(cg,field_index);
			ins_end(cg);
		
			return CgValue(0,field->type(),areg);
		}
	}
	CgValue index(RegisterName index){ // calculates & returns adress
		return CgValue();
	}
};

void debug_op(Name opname) {
}


Name reg_of(Node* n, ExprFnDef* owner) {
	char tmp[32];
	if (auto p=dynamic_cast<ExprIdent*>(n)){
		// TODO - check 'this',lambda locals
		return p->name;
	}
//	TODO need to verify that there are NO clashes
// todo-can we just cache this on the node itself?
	auto m=((uint32_t)
			 (size_t)n);
	sprintf(tmp,"n%x", ((m>>2) ^ (m>>18))&0xffff);
	return getStringIndex(tmp,0);
}
void write_reg(CodeGen& cg, RegisterName dst ) {
	fprintf(cg.ofp,"%%%s ",str(dst));
}
Scope* g_Sc;
void write_type(CodeGen& cg, CgValue& lv) {
	write_type(cg,lv.type, lv.is_addr());
}
void write_phi_reg_label(CodeGen& cg, Name reg, Name label){
	write_comma(cg);
	fprintf(cg.ofp,"[");
	write_reg(cg,reg);
	fprintf(cg.ofp,",");
	write_reg(cg,label);
	fprintf(cg.ofp,"]");
}

void write_type(CodeGen& cg, const Type* t, bool ref) {
	auto ofp=cg.ofp;
	if (!t) { fprintf(ofp,"<type_expected>");return;}
	if (t->is_pointer()){
//		dbprintf("THIS IS SUSPECT, REF ISn'T NEEDED TWICE");
		write_type(cg,t->sub,false); fprintf(ofp,"*");
	}else if (t->is_array()) {
		fprintf(ofp,"[ %s x ",str(t->array_size())); //sub->next->name));
		write_type(cg,t->sub,0); // TODO: assert its a numeric constant
		fprintf(ofp,"]"/*%s,ref?"*":""*/);
	}
	else if (t->name==TUPLE) {
		struct_begin(cg);
		for (auto s=t->sub;s;s=s->next){
			write_comma(cg);
			write_type(cg,s,false);
		}
		struct_end(cg);
	} else if (t->is_struct()){
		auto sd=t->struct_def;
		if (!sd) {
			t->m_origin->dump(0);
			t->dump(-1);
			error(t->m_origin?t->m_origin:t,"struct %s not resolved in %p\n",str(t->name),t);
		}
		if (sd->name) write_struct_name(cg, sd->get_mangled_name());
		else {
			// LLVM does allow listing an anonymous struct
			struct_begin(cg);
			//error(t,"no struct def");
			for (auto i=0; i<sd->fields.size(); i++){
				write_comma(cg);
				write_type(cg,sd->fields[i]->type(),false);
			}
			struct_end(cg);
		}
	}
	else if (t->is_function()){
		//error(t,"TODO,write function type unified ");
		write_function_type(cg, t);
	}
	else {
		if (t->is_complex()) {
			fprintf(ofp,"%%%s", str(t->name));
		}
		else
			fprintf(ofp,"%s",t?get_llvm_type_str(t->name):"???");//,t.is_pointer?"*":"");
	}
	if (ref)
		fprintf(ofp,"*");
}
void write_type_reg(CodeGen& cg, const Type* t,bool ref, Name reg){
	write_comma(cg);
	write_type(cg,t,ref);
	write_reg(cg,reg);
}

void write_instruction_sub(CodeGen& cg, Name opname,Type* type,  CgValue dst,CgValue src1){
//	ASSERT(dst.is_reg());
	const LLVMOp* op=get_op_llvm(opname,type?type->name:VOID);
	dst.write_operand(cg);
	write_ins_name(cg, op?op->op_signed:str(opname));
	if (is_comparison(opname))
		write_type(cg,src1);
	else{
		write_type(cg,type,false);
	}
	src1.write_operand(cg);
}
void write_instruction(CodeGen& cg, Name opname,Type* type,  CgValue dst,CgValue src1){
	ins_begin(cg);
	write_instruction_sub(cg,opname,type,dst,src1);
	ins_end(cg);
}
void write_instruction(CodeGen& cg, Name opname,Type* type,  CgValue dst,CgValue src1,CgValue src2){
	ASSERT(type!=0);
	ins_begin(cg);
	write_instruction_sub(cg,opname,type,dst,src1);
	fprintf(cg.ofp,",");
	src2.write_operand(cg);
	ins_end(cg);
}

void dump_locals(Scope* s){
	for (;s;s=s->parent){
		for (auto v=s->vars; v;v=v->next_of_scope){
			printf("\t;%s:",str(v->name));v->get_type()->dump(-1); printf("%%%s\n",str(v->regname));
		}
	}
}

CgValue ExprStructDef::compile(CodeGen& cg, Scope* sc) {
	auto st=this;
	if (st->is_generic()) {	// emit generic struct instances
		write_comment(cg,"instances of %s",str(st->name));
		int i=0;
		for (auto ins=st->instances; ins; ins=ins->next_instance,i++){
			write_comment(cg,"instance %d:",i);
			ins->compile(cg, sc);
		}
	} else {
		write_struct_name(cg,st->get_mangled_name());
		write_ins_name(cg,"type");
		struct_begin(cg);
		// todo: properly wrap translations to LLVM types.
		int i=0; for (auto fi: st->fields){
			write_comma(cg);
			write_type(cg,fi->type(), false);
		};
		struct_end(cg);
		ins_end(cg);
	}
	return CgValue();	// todo: could return symbol? or its' constructor-function?
}

CgValue alloca_type(CodeGen& cg, Expr* holder, Type* t) {
	RegisterName r= holder?holder->get_reg(t->name, &cg.next_reg, false):next_reg_name(&cg.next_reg);
	ins_begin_reg_op(cg,r,"alloca"); write_type(cg,t,false);
	fprintf(cg.ofp,", align %zu", t->alignment());
	ins_end(cg);
	return CgValue(0,t, r);
}

void write_local_vars(CodeGen& cg, Expr* n, ExprFnDef* fn, Scope* sc) {
	auto ofp=cg.ofp;
	for (auto v=sc->vars; v;v=v->next_of_scope){
		if (v->kind!=Local) continue;
		auto vt=v->expect_type();
		//if (!v->on_stack)
		//	continue; //reg vars experiment
		if (!vt->is_complex())
			continue;//its just a reg
		auto r= v->get_reg(v->name, &cg.next_reg, true);
		if (vt->is_struct()) {
			// alloc_struct
//			fprintf(ofp,"\t"); write_reg(ofp,r); fprintf(ofp," = alloca %%%s , align %d\n",getString(vt->name),vt->struct_def->alignment());
			alloca_type(cg, v, vt);
			v->reg_is_addr=true;
		} else if (vt->is_array()){
			auto t=vt->sub;
			if (!t || !t->next){error(v,"array type needs 2 args");}
			fprintf(cg.ofp,"\t"); write_reg(cg,r); fprintf(cg.ofp," = alloca [%s x %s] , align %zu\n",str(t->next->name),get_llvm_type_str(t->name),vt->alignment());
			v->reg_is_addr=true;
		} else	if (vt->is_pointer() || vt->is_function()){
			continue;
		} else {
			dbprintf("error:\n");
			vt->dump(-1);
			error(n,"typenot handled %s",str(vt->name));
		}
	}
}
Name gen_label(const char* s, int index){
	char tmp[256];sprintf(tmp,"%s%d",s,index); return getStringIndex(tmp);
}
void emit_label(CodeGen& cg, Name l){
	fprintf(cg.ofp,"%s:\n",str(l));
}
void emit_branch(CodeGen& cg, Name l){
	ins_begin_name(cg,"br");
	fprintf(cg.ofp,"label %%%s",str(l));
	ins_end(cg);
}
void emit_branch(CodeGen& cg, CgValue cond, Name label_then, Name label_else){
	cond.load(cg);
	ins_begin_name(cg,"br");
	fprintf(cg.ofp,"i1 %%%s, label %%%s, label %%%s",str(cond.reg), str(label_then), str(label_else));
	ins_end(cg);
}

CgValue ExprIf::compile(CodeGen& cg,Scope*sc){
	auto curr_fn=cg.curr_fn;
	auto ifn=this;
	auto ofp=cg.ofp;
	// TODO: Collect phi-nodes for anything modified inside.
	RegisterName outname=next_reg_name(&cg.next_reg);
	auto condition=ifn->cond->compile(cg,sc);
	int index=cg.next_reg++;
	auto label_if=gen_label("if",index);
	auto label_endif=gen_label("endif",index);
	if (ifn->else_block){
		auto label_else=gen_label("else",index);
		emit_branch(cg,condition,label_if,label_else);
		emit_label(cg,label_if);
		auto if_result=ifn->body->compile(cg,sc);
		if_result.load(cg,0);
		emit_branch(cg,label_endif);
		emit_label(cg,label_else);
		auto else_result=ifn->else_block->compile(cg,sc);
		else_result.load(cg,0);
		emit_branch(cg,label_endif);
		emit_label(cg,label_endif);
		// phi node picks result, conditional assignment
		if (if_result.is_valid() && else_result.is_valid()){
			ins_begin_reg_op(cg,outname,"phi");
			write_type(cg,if_result.type, if_result.is_addr());
			write_phi_reg_label(cg,if_result.reg,label_if);
			write_phi_reg_label(cg,else_result.reg,label_else);
			ins_end(cg);
		}
		auto return_type=ifn->get_type();
		return CgValue(outname,return_type);
	}
	else {
		emit_branch(cg,condition,label_if,label_endif);
		auto ifblock=ifn->body->compile(cg,sc);
		emit_label(cg,label_endif);
		// TODO phi node
		return CgValue();
	}
}

struct LoopPhiVar {
	CgValue val;//todo
	Variable*	var;
	RegisterName reg_pre;
	RegisterName reg_start;
	RegisterName reg_end;
};
void write_phi(CodeGen& cg, Scope* sc, vector<LoopPhiVar>& phi_vars,Name l_pre, Name l_end, bool extra) {
	auto ofp=cg.ofp;
	for (auto& v: phi_vars) {
		v.reg_end=v.var->regname;
		ins_begin_reg_op(cg,v.reg_start,"phi");
		write_type(cg,v.var->type(), false);//v.val.is_addr());
		write_phi_reg_label(cg,v.reg_pre,l_pre);
		write_phi_reg_label(cg,v.reg_end,l_end);
		ins_end(cg);
	}
	if (extra) for (auto i=phi_vars.size(); i>0;i--)fprintf(ofp,"       ");	// dirty hack to prevent source overun
	fprintf(ofp,"\n");
}

CgValue ExprFor::compile(CodeGen& cg, Scope* outer_sc){
//  initializer
// for:
//  test condition br else
//   body
//   (break br endfor)
//   increment
//   br loop
// break:
// else:
// endfor:
	auto nf=this;
	auto curr_fn=cg.curr_fn;
	auto sc=nf->scope;
	// write the initializer block first; it sets up variables initial state
	auto ofp=cg.ofp;
	int index=cg.next_reg++;
	auto l_init=gen_label("init",index);
	emit_branch(cg,l_init);
	emit_label(cg,l_init);
	auto init=nf->init->compile(cg,sc);
	
	set<Variable*> write_vars;
	set<Variable*> else_vars;
	// Now scan all the blocks to see which are changed.
	if (nf->cond)nf->cond->find_vars_written(sc,write_vars);
	if (nf->body)nf->body->find_vars_written(sc,write_vars);
	if (nf->incr)nf->incr->find_vars_written(sc,write_vars);
	vector<LoopPhiVar> phi_vars;
	for (auto v :write_vars){
		LoopPhiVar phi;
//		phi.val=
		phi.var=v;
		phi.reg_pre=v->regname;
		phi.reg_start=v->regname=next_reg_name(v->name,&cg.next_reg);
		phi.reg_end=0;//gen_label(str(v->name),next_index);
		//todo: can we allocate regnames in the AST? find last-write?
		phi_vars.push_back(phi);
	}
	auto l_for=gen_label("cond",index);
	auto l_body=gen_label("body",index);
	auto l_else=gen_label("else",index);
	auto l_endfor=gen_label("endfor",index);
	emit_branch(cg,l_for);
	emit_label(cg,l_for);
	auto phipos=ftell(ofp);// YUK. any way around this? eg write condition at end?
	write_phi(cg,sc,phi_vars,l_init,l_for,true);//alloc space

	auto cond_result=nf->cond?nf->cond->compile(cg,sc):CgValue();
	emit_branch(cg, cond_result, l_body, l_else);
	emit_label(cg,l_body);
	if (nf->body) nf->body->compile(cg,sc);
	if (nf->incr) nf->incr->compile(cg,sc);
	emit_branch(cg,l_for);
	emit_label(cg,l_else);
	if (nf->else_block) nf->else_block->compile(cg,sc);
	emit_branch(cg,l_endfor);
	emit_label(cg,l_endfor);
	// now write the phi-nodes.
	auto endpos=ftell(ofp);// YUK. any way around this?
	fseek(ofp, phipos,SEEK_SET);
	write_phi(cg,sc,phi_vars,l_init,l_body,false);
	fseek(ofp, 0,SEEK_END);

	//TODO: return value.
	return CgValue();
}

CgValue write_cast(CodeGen& cg,CgValue dst, CgValue&lhsv, Expr* rhse){
	// todo rename 'src', 'dst' to avoid blah as blah confusion vs C cast (type)(expr)
	auto ofp=cg.ofp;
	lhsv.load(cg);
	auto lhst=lhsv.type;
	auto rhst=rhse->type();
	const char* ins="nop";

	if (lhst->is_int() && rhst->is_float()){
		ins=rhst->is_signed()?"sitofp":"uitofp";
	}
	else if (lhst->is_float() && rhst->is_int()){
		ins=lhst->is_signed()?"fptosi":"fptoui";
	}
	else if (rhst->is_pointer()){
		ins="bitcast";
	}
	else if (lhst->size()>rhst->size()){
		if (lhst->is_int() && rhst->is_int()){
			if (lhst->is_signed()>rhst->is_signed())
				ins="sext";
			else
				ins="zext";
		} else
			ins="fpext";
	} else if (lhst->is_int() && rhst->is_int()){
		ins="trunc";
	}else{
		ins="fptrunc";
	}
	ins_begin_reg_op(cg, dst.reg, ins);
	write_type(cg,lhsv.type,0);
	write_reg(cg,lhsv.reg);
	fprintf(ofp," to ");
	write_type(cg,rhst,0);
	ins_end(cg);
	return dst;
}

CgValue ExprOp::compile(CodeGen &cg, Scope *sc) {
	auto n=this;
	auto e=this;
	auto opname = e->name;
	int opflags = operator_flags(opname);
	auto t=e->get_type();//get_type_llvm();
	
	// TODO 2operand form should copy regname for this node from the lhs.
	// TODO - multiple forms:
	//
	// generalize by lvalue being in register or memory.
	// 3-operand; assign-op; assign-op; mem-assign-op;
	if (opname==DOT || opname==ARROW){
		auto lhs=e->lhs->compile(cg,sc);
		return lhs.dot(cg,e->rhs,sc);
	}
	else if (e->lhs && e->rhs){
		auto lhs=e->lhs->compile(cg,sc);
		auto rhs=e->rhs->compile(cg,sc);
		auto lhs_v=sc->find_variable_rec(e->lhs->name);
		auto outname=lhs_v?lhs_v->name:opname;
		
		auto dst=CgValue(n->get_reg(outname,&cg.next_reg,false),n->get_type());
		
		if (opname==ASSIGN_COLON){ // do nothing-it was sema'sjob to create a variable.
			ASSERT(sc->find_scope_variable(e->lhs->name));
			if (lhs_v) dst.reg=lhs_v->regname;
				//					lhs_v->regname=dst.reg;
				return CgValue(0,lhs_v->type(),dst.reg);
				}
		else if(opname==AS) {
			// if (prim to prim) {do fpext, etc} else..
			return write_cast(cg, dst,lhs,e);
		}
		else
		if (opname==LET_ASSIGN){// Let-Assign *must* create a new variable.
			auto v=sc->find_variable_rec(e->lhs->name); WARN(v &&"semantic analysis should have created var");
			auto dst=v->get_reg(v->name, &cg.next_reg, true);
			if (rhs.is_literal()){//TODO simplify this, how does this case unify?
				v->regname=dst;
				rhs.load(cg,dst);
				return CgValue(dst,rhs.type);
			}
			if (rhs.type->is_struct()){
				v->regname=rhs.reg?rhs.reg:rhs.addr;
				v->reg_is_addr=!rhs.reg;
				return rhs;
				
			}
			else{
				dst=rhs.load(cg,dst);
				v->regname=dst; // YUK todo - reallyw wanted reg copy
				return CgValue(dst, n->get_type(), 0);
			}
		}
		else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_RHS)  && opname==ASSIGN){
			//assignment  =
			if (lhs_v) dst.reg=n->regname=lhs.reg=lhs_v->regname;
				rhs.load(cg,0);
				lhs.store_from(cg,rhs.reg);
				rhs.type=e->get_type();
				return rhs;
		}
		else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_LHS|READ_RHS) ){
			// Assign-Operators += etc
			rhs.load(cg);
			auto dstreg=lhs;
			lhs.load(cg); // turns it into a value.
			write_instruction(cg,opname,t?t:rhs.type, dst,lhs,rhs);
			auto out=dstreg.store_from(cg,dst.reg);
			return dstreg;
		}else {
			// RISClike 3operand dst=src1,src2
			lhs.load(cg); rhs.load(cg);
			write_instruction(cg,opname,t,dst,lhs,rhs);
			dst.type=e->get_type();
			return dst;
		}
	} else if (!e->lhs && e->rhs){ // prefix operator
		auto src=e->rhs->compile(cg,sc);
		if (opname==ADDR){
			if (!src.type || !n->type()) {
				n->dump(-1);
				error(n,"something wrong\n");
			}
			return src.addr_op(cg,n->type());
		}
		else if (opname==DEREF){
			return src.deref_op(cg,n->type());
		}
		else {
			if (opflags & (WRITE_LHS|WRITE_RHS)){static int once;if (once++){printf(";TODO: logic for modifications to memory");}}
			// todo: handle read/modify-writeness.
			// postincrement/preincrement etc go here..
			auto dst=CgValue(n->get_reg(opname,&cg.next_reg,false), n->get_type());
			write_instruction(cg,opname,t,dst,src);
			return dst;
		}
	} else if (e->lhs && !e->rhs) {
		error(e,"postfix operators not implemented yet.");
		return CgValue();
	} else
		return CgValue();
}


CgValue ExprBlock::compile(CodeGen& cg,Scope *sc) {
	auto n=this;
	auto e=this; auto curr_fn=cg.curr_fn;
	// [1] compound expression - last expression is the return .
	if(e->is_compound_expression()) {
		if (auto num=e->argls.size()) {
			for (int i=0; i<num-1; i++){
				e->argls[i]->compile(cg,sc);
			}
			if (e->argls.size())
				return e->argls[num-1]->compile(cg,sc);
		};
	}
	else if (e->is_struct_initializer()){
		StructInitializer si(sc,e); si.map_fields();
		auto struct_val= alloca_type(cg, e, e->type());
		e->regname=struct_val.reg; // YUK todo - reallyw wanted reg copy
		// can we trust llvm to cache the small cases in reg..
		for (int i=0; i<e->argls.size();i++) {
			auto rvalue=si.value[i]->compile(cg,sc);
			auto dst = struct_val.dot(cg,si.field_refs[i],sc);
			auto srcreg = rvalue.load(cg,0);
			dst.store_from(cg,srcreg);
			if (dst.type==struct_val.type)
				struct_val=dst; // mutate by insertion
		}
		return struct_val;
	}
	// [2] Operator
	//[3] ARRAY ACCESS
	else if (auto ar=n->is_subscript()){
		auto expr=ar->call_expr->compile(cg,sc);// expression[index]
		auto index=ar->argls[0]->compile(cg,sc);
		auto array_type=expr.type;
		auto inner_type=array_type->sub;
		auto dst=ar->get_reg(ARRAY,&cg.next_reg,false);
		expr.load(cg);
		if (!n->regname){n->regname=expr.reg;}
		
		index.load(cg,0);
		ins_begin_reg_op(cg,dst,"getelementptr inbounds");
		write_type(cg,array_type,true);//!expr.reg);
		write_reg(cg,expr.reg);
		fprintf(cg.ofp,",i32 0, i32 ");//%%%d\n",getString(expr.reg),
		index.write_operand(cg);
		ins_end(cg);
		return CgValue(0,inner_type,dst);
	}
	//[3] FUNCTION CALL
	else if (e->is_function_call()){
		auto call_fn=e->get_fn_call();
		RegisterName indirect_call=0;
		write_comment(cg,"fncall %s", call_fn?str(call_fn->name):e->call_expr->name_str());

		if (e->call_expr->is_function_name()) {
			
		} else {
			auto fptr = e->call_expr->compile(cg, sc);
			indirect_call=fptr.load(cg);
		}

		vector<CgValue> l_args;
		vector<CgValue> l_args_reg;
		// process function argumetns & load
		for (auto arg:e->argls){
			auto reg=arg->compile(cg,sc);
			if (!reg.type) {
				error_begin(arg,"arg type not resolved in call to %s\n", call_fn->name_str());
				dbprintf("arg type=");arg->dump(-1);newline(0);
				auto reg=arg->compile(cg,sc);
				error_end(arg);
				ASSERT(reg.type);
			}
			auto regval=CgValue(reg.load(cg),reg.type);
			l_args_reg.push_back(regval);
		}
		int i=0;
		for (auto reg:l_args_reg){
			if (reg.addr && !reg.reg) {
				reg.load(cg);
			}
			l_args.push_back(reg);
			i++;
		}

		auto dst=n->get_reg_new(call_fn?call_fn->name:FN, &cg.next_reg);
		//auto rt=call_fn->get_return_value();
		auto ret_type=e->type();	// semantic analysis should have done this
		if (e->type()->name!=VOID)
		{
			ins_begin_reg_op(cg,dst,"call");
		} else
			ins_begin_name(cg,"call");
		
		if (call_fn)
			write_function_type(cg, call_fn);
		else
			write_function_type(cg,e->call_expr->type());
		if (!indirect_call) {
			write_global(cg,call_fn->get_mangled_name());
		} else {
			write_reg(cg,indirect_call);
		}
		
		fprintf(cg.ofp,"(");
		for (auto i=0; i<l_args.size(); i++){
			if (i) {fprintf(cg.ofp,",");}
			auto reg=l_args_reg[i];
			write_type(cg,reg);//reg.is_addr());
			reg.write_operand(cg);
		}
		fprintf(cg.ofp,")");
		ins_end(cg);
		if (ret_type && ret_type->name!=VOID) {
			return CgValue(dst,ret_type);
		} else{
			return CgValue();
		}
	}
	return CgValue();
}

CgValue	ExprIdent::compile(CodeGen& cg, Scope* sc){
	auto n=this;
	auto var=sc->find_variable_rec(n->name);
	if (!var){
		if (n->def) {
			return CgValue(n->def);
		}
		error(n,"var not found %s\n",n->name_str());
		return CgValue();
	}
	if (var && var!=n->def){
//			error(n,"var/def out of synx %s %s\n",n->name_str(),var->name_str());
//			return CgValue();
	}
	return CgValue(var);
}

CgValue	ExprLiteral::compile(CodeGen& cg, Scope* sc) {
	return CgValue(this);
}

CgValue Type::compile(CodeGen& cg, Scope* sc){
	return CgValue(0,this,0);	// TODO - not sure this makes sense, probably not.
}

// Emit function header..
enum EmitFnMode {EmitDefinition,EmitDeclaration,EmitType};
void write_function_signature(CodeGen& cg,ExprFnDef* fn_node, EmitFnMode mode){
	auto ofp=cg.ofp;
	auto scope=fn_node->scope;
	fn_node->clear_reg();
	auto rtype=fn_node->return_type();
	fprintf(ofp,mode==EmitDefinition?"define ":mode==EmitDeclaration?"declare ":" ");
	write_type(cg,rtype,false);
	if (mode!=EmitType)
		write_global(cg,fn_node->get_mangled_name());
	fprintf(ofp,"(");
	int inter=0;
	for (auto a:fn_node->args){
		if (inter++){fprintf(ofp,",");};
		write_type(cg,a->type(),false);//was a->is_complex. confusion here over pass by ref/val. we think fn sigs should be 1:1. but raw struct type should  be pass by val? will we have to copy struct val?
		if (mode==EmitDefinition){
			auto var=scope->get_or_create_scope_variable(a,a->name, VkArg);
			var->get_reg(a->name, &cg.next_reg, false);
			write_reg(cg,var->regname);
		}
	}

	if (fn_node->variadic) {
		if (fn_node->args.size())fprintf(cg.ofp,",");fprintf(cg.ofp,"...");
	}
	fprintf(cg.ofp,")");
	if (mode==EmitType)fprintf(cg.ofp,"*");
	else fprintf(cg.ofp,"\n");
}
void write_function_type(CodeGen& cg, const Type* t) {
	auto ofp=cg.ofp;
	auto argtuple=t->sub;
	ASSERT(argtuple);
	auto retn=argtuple->next;
	write_type(cg,retn,0);
	fprintf(ofp,"(");
	for (auto arg=argtuple->sub; arg;arg=arg->next){
		write_type(cg,arg);
		if (arg->next)fprintf(ofp,",");
	}
	fprintf(ofp,")");
	fprintf(ofp,"*");
	
}
void write_function_type(CodeGen& cg,ExprFnDef* fn_node){
	write_function_signature(cg,fn_node,EmitType);
}

CgValue ExprFnDef::compile(CodeGen& cg,Scope* outer_scope){
	auto fn_node = this;
	auto ofp=cg.ofp;

	if (!fn_node){return CgValue();}
	if (fn_node->is_undefined()) {
		fprintf(ofp,";fn %s prot\n",getString(fn_node->name));
		write_function_signature(cg,fn_node,EmitDeclaration);
		return CgValue();
	}
	if (fn_node->is_generic()) {
		fprintf(ofp,";fn %s generic:-\n",getString(fn_node->get_mangled_name()));
		for (auto f=fn_node->instances; f;f=f->next_instance){
			fprintf(ofp,";fn %s generic instance\n",getString(fn_node->get_mangled_name()));
			f->compile(cg,outer_scope);
		}
		return CgValue();
	}

	if (cg.curr_fn) // we can't nest function compilation - push to CodeGen stack
	{
		cg.compile_later.push_back(this);
		return CgValue(this);
	} else{
		cg.curr_fn=this;
	}

	write_fn_ptr(cg,fn_node->get_mangled_name());
	write_ins_name(cg,"global");
	write_function_type(cg, fn_node->type());
	write_global(cg,fn_node->get_mangled_name());
	if (!fn_node->get_type() && fn_node->fn_type && fn_node->scope ){
		error(fn_node,"function name %s %s %p %p %p %p", str(fn_node->name),str(fn_node->get_mangled_name()), fn_node->instance_of, fn_node->get_type(), fn_node->fn_type, fn_node->scope);
		ASSERT(0 && "function must be resolved to compile it");
		return CgValue();
	}
	write_comment(cg,"fn %s (%p) :- ins=%p of %p ", str(fn_node->name),fn_node, fn_node->instances, fn_node->instance_of);

	auto scope=fn_node->scope;
	
	write_function_signature(cg,fn_node,EmitDefinition);
 	fprintf(ofp,"{\n");
	if (fn_node->instance_of!=nullptr){
		write_comment(cg,"compiling generic fn body");
	}
	write_local_vars(cg, fn_node->body, fn_node, scope);
	auto rtn=fn_node->get_return_value();
	auto ret=fn_node->body->compile(cg,scope);
	if (ret.is_valid() && !ret.type->is_void()) {
		ret.load(cg);
		ins_begin_name(cg,"ret");
		write_type(cg,ret);//rtn->get_type(),ret.is_addr());
		ret.write_operand(cg);
		ins_end(cg);
	} else {
		ins_begin_name(cg,"ret");
		write_txt(cg,"void");
		ins_end(cg);
	}
	fprintf(ofp,"}\n");
	cg.curr_fn=0;
	return CgValue(fn_node);
}

CgValue Node::compile(CodeGen& cg, Scope* sc){
	error(this,"compile not implemented for %s",this->kind_str());
	return CgValue();
}


// extern function; - just use the function name, and mangle arguments given at the callsite.
// this is the default assumed for unfound symbols?
// extern "C" function; - a function with C linkage.
char hexdigit(char c){if (c<10) return c+'0'; else return 'A'+(c-10);}
int translate_llvm_string_constant(char* dst, int size, const char* src){
	const char* s=src;
	char*d=dst;
	int len=0;
	for (; *s && size>2; size--){
		*d++=*s++; len++;
		// Do what C does..
		if (s[-1]!='\\') {
			continue;
		}
		char c=*s++;
		if (c=='n') c=0xa; else if (c=='t') c=0x9; else if (c=='f') c=0xc; else if(c=='r') c=0xd; else if(c=='a') c=0x7;  else if(c=='b') c=0x8;else if(c=='v') c=0xb;else c=0;
		
		*d++=hexdigit((c>>4) & 0xf);
		*d++=hexdigit(c & 0xf);
	}
	*d++=0;

	return len;
}

CgValue Node::codegen(CodeGen& cg, bool just_contents) {
	dbprintf("TODO refactor codegen to use this virtual. warning codegen not implemented for %s\n",this->kind_str());
	return CgValue();
}
void name_mangle_append_segment(char* dst, int size, const char* src){
	auto len=strlen(src);
	dst+=strlen(dst);
	sprintf(dst,"%lu",len);
	strcat(dst,src);
}

void name_mangle_append_type(char* dst,int size, const Type* t){
	if (!t) return;
		// todo - check how template params are suppsoed to mangle
		// we suspect the template params cover this... fn's params are mangled and this should just be struct->name
		//name_mangle_append_name(dst,size,t->struct_def->get_mangled_name());

	auto n=t->name;
	if (n==PTR){ strcat(dst,"P");}
	else if (n==BOOL){ strcat(dst,"b");}
	else if (n==UINT){ strcat(dst,"u");}
	else if (n==INT){ strcat(dst,"i");}
	else if (n==U32){ strcat(dst,"l");}
	else if (n==U16 || n==I16){ strcat(dst,"s");}
	else if (n==FLOAT){strcat(dst,"f");}
	else if (n==DOUBLE){strcat(dst,"d");}
	else if (n==HALF){strcat(dst,"h");}
	else if (n==CHAR || n==I8 || n==U8){strcat(dst,"c");}
	else if (auto sd=t->struct_def){
		name_mangle_append_segment(dst,size,str(sd->get_mangled_name()));
		for (auto& it:sd->instanced_types){
			dbprintf_mangle(" %s\n",it->name_str());
			name_mangle_append_type(dst,size,it);
		}
	}
	else {name_mangle_append_segment(dst, size, str(t->name));}
	
	for (auto ts=t->sub;ts;ts=ts->next){
		name_mangle_append_type(dst,size,ts);
	}
}
void name_mangle(char* dst, int size, const ExprFnDef* src) {
	dst[0]=0;
	// TODO - prefix scopes. Now, Overloading is the priority.
	// todo - check how template params are suppsoed to mangle
	sprintf(dst,"_Z");dst+=2;
	size_t len=strlen(dst); size--; size-=len;
	name_mangle_append_segment(dst, size, str(src->name));
	for (auto a:src->args){
		name_mangle_append_type(dst,size, a->type());
	}
}

void name_mangle(char* dst, int size, const ExprStructDef* src) {
	dst[0]=0;
	// TODO - prefix scopes. Now, Overloading is the priority.
	sprintf(dst,"_Z");dst+=2;
	size_t len=strlen(dst); size--; size-=len;
	name_mangle_append_segment(dst, size, str(src->name));

	for (auto& it:src->instanced_types){
		name_mangle_append_type(dst,size,it);
	}
}

void output_code(FILE* ofp, Scope* scope) {
	verify_all();
	auto cg=CodeGen(ofp,0);

	write_comment(cg,"from scope %s\n;",scope->name());
	// output all inner items that outer stuff depends on..
	// literals first, because we setup llvm_strlen. TODO , more solid design pls.
	for (auto l=scope->literals; l; l=l->next_of_scope) {
		if (l->type_id==T_CONST_STRING){
		const char* name=getString(l->name);
			char buffer[512];
			l->llvm_strlen=translate_llvm_string_constant(buffer,512, l->as_str())+1;
			write_global(cg, l->name);
			write_txt(cg,"= private unnamed_addr constant [%d x i8] c\"%s\\00\"\n", l->llvm_strlen, buffer);
		}
	}
	for (auto sub=scope->child; sub; sub=sub->next) {
		output_code(cg.ofp,sub);
	}
	for (auto n=scope->named_items;n;n=n->next) {
		for (auto s=n->structs; s;s=s->next_of_name) {
			s->compile(cg, scope);
		}
	}
	for (auto n=scope->named_items;n;n=n->next) {
		for(auto f=n->fn_defs; f; f=f->next_of_name){
			f->compile(cg,scope);
		}
	}
}




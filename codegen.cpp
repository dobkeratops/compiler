#include "codegen.h"

// TODO: properly abstract llvm instruction generation to move to llvm api.
inline void dbprintf_mangle(const char*,...){}


void CodeGen::emit_struct_name(RegisterName dst ){emit_reg(dst);}
Name next_reg_name(int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d",(*next_reg_index)++);
	return getStringIndex(tmp);
}
Name CodeGen::next_reg(){
	char tmp[64]; sprintf(tmp,"r%d",m_next_reg++);
	return getStringIndex(tmp);
}
Name next_reg_name(Name prefix_name, int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d%s",(*next_reg_index)++,str(prefix_name));
	return getStringIndex(tmp);
}
void emit_reg(RegisterName reg);

void CodeGen::emit_ins_begin_sub(){emit_txt("\t"); comma=false;}
void CodeGen::emit_undef(){emit_txt("undef");}
void CodeGen::emit_ins_name(const char* txt){emit_txt("= %s ",txt);}
void CodeGen::emit_ins_begin_name(const char* txt){emit_ins_begin_sub();emit_txt("%s ",txt);}
void CodeGen::emit_ins_end(){emit_txt("\n");}
void CodeGen::emit_txt(const char* str,...){// catch all, just spit out given string
	char tmp[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	fprintf(ofp,"%s",tmp);
}

void CodeGen::emit_nest_begin(const char* str){
	auto& cg=*this;
	ASSERT(depth<32);
	commas[depth]=comma;
	depth++;
	emit_txt(str);
	comma=0;
}
void CodeGen::emit_nest_end(const char* str){
	ASSERT(depth);
	depth--;
	comma=commas[depth];
	emit_txt(str);
}
void CodeGen::emit_args_begin()	{emit_nest_begin("(");}
void CodeGen::emit_args_end()		{emit_nest_end(")");}
void CodeGen::emit_struct_begin()	{emit_nest_begin("{");}
void CodeGen::emit_struct_end()	{emit_nest_end("}");}
void CodeGen::emit_pointer_begin()	{emit_nest_begin("");};
void CodeGen::emit_pointer_end()	{emit_nest_end("*");};

void CodeGen::emit_comma(){
	if (this->comma==1){emit_txt(",");}
	this->comma=1;
}
void CodeGen::emit_separator(const char* txt){
	emit_txt(txt);
	this->comma=0;
}
RegisterName  CodeGen::emit_ins_begin(RegisterName reg, const char* op){
	if (!reg) { emit_ins_begin_name(op);}
	else {
		emit_ins_begin_sub();
		emit_reg(reg);
		emit_ins_name(op);
	}
	return reg;
}
void CodeGen::emit_i32_lit(int index) {
	emit_comma();
	emit_txt("i32 %d",index);
}
void CodeGen::emit_i32_reg(Name reg) {
	emit_comma();
	emit_txt("i32 ");
	emit_reg(reg);
}
RegisterName	CodeGen::emit_extractvalue(RegisterName dst,Type* type,RegisterName src,int index){
	emit_ins_begin(dst,"extractvalue");
	emit_type(type);
	emit_reg(src);
	emit_comma();
	emit_txt("%d",index);
	emit_ins_end();
	return dst;
}

void CodeGen::emit_store(RegisterName reg, Type* type, RegisterName addr){
	emit_ins_begin_name("store");
	emit_type(type,0);
	emit_reg(reg);
	emit_type(type,1);
	emit_reg(addr);
	emit_txt(", align 4");
	emit_ins_end();
}
void CodeGen::emit_global(Name n){
	emit_txt("@%s ",str(n));
}
void CodeGen::emit_fn_ptr(Name n){
	emit_txt("@%s.ptr",str(n));
}
void CodeGen::emit_fn(Name n){
	emit_txt("@%s",str(n));
}
void CodeGen::emit_struct_ptr(Name n){
	emit_pointer_begin();
	emit_txt("%%%s",str(n));
	emit_pointer_end();
}
void CodeGen::emit_comment(const char* str,...){// catch all, just spit out given string
	char tmp[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	fprintf(ofp,"\t;%s\n",tmp);
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
		}
	}
	CgValue to_stack(CodeGen& cg){
		CgValue stack_val(0,this->type,cg.emit_alloca_type(nullptr,this->type).addr,this->elem);
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
			// todo: why not 'addr' aswell?
			if (this->type->is_pointer() || (addr &&!reg)) {
				cg.emit_comment("dot reg=%s addr=%s index=%d",str(reg),str(addr),elem);
				auto sub=this->get_elem_index(cg,elem);
				return sub.load(cg);
			} else{
				// elem acess
				auto newreg=(bool)force_regname?force_regname:next_reg_name(&cg.m_next_reg);
				cg.emit_ins_begin(newreg,"extractelement");
				cg.emit_type_reg(this->type,false,reg);
				cg.emit_i32_lit(this->elem);
				cg.emit_ins_end();
				this->type=this->type->get_elem(elem);
				this->reg=newreg;
				return newreg;
			}
		}
		if(val) {
			if (force_regname){reg=force_regname;}
			else if (!reg) reg=next_reg_name(&cg.m_next_reg);
			
			if (auto lit=dynamic_cast<ExprLiteral*>(val)){
				if (lit->type()->name==INT){
					cg.emit_ins_begin(reg,"or");
					cg.emit_i32_lit(0);  this->emit_literal(cg,lit);
				} else if(lit->type()->name==FLOAT){
					// todo, i guess we're goint to have t make a global constants table
					cg.emit_ins_begin(reg,"fadd");
					fprintf(cg.ofp,"float 0.0, ");  this->emit_literal(cg,lit);
				} else if (lit->type()->name==STR){
					cg.emit_ins_begin(reg,"getelementptr inbounds");
					cg.emit_comma();
					cg.emit_txt("[%d x i8]* @%s",lit->llvm_strlen, getString(lit->name));

					cg.emit_i32_lit(0);
					cg.emit_i32_lit(0);
					ASSERT(lit->llvm_strlen);
				} else {
					error(lit,"literal type not handled yet");
				}
				cg.emit_ins_end();
				// todo: string literal, vector literals, bit formats
			}
			else if (auto fp=dynamic_cast<ExprFnDef*>(val)){
				if (fp->is_closure()) {
					// Build a pair. we would also build the Capture Object here.
					// TODO: we're blocked from making a nice' emit_make_pair()' wrapper here
					// because: fp->fn_type is interpretted as {ptr,env}, but we must manually
					// tell it to build the first & second types.
					auto r=cg.emit_ins_begin(cg.next_reg(),"insertvalue");
					cg.emit_type(fp->fn_type); cg.emit_undef();
					cg.emit_function_type(fp->fn_type);
					cg.emit_fn(fp->get_mangled_name());
					cg.emit_comma();
					cg.emit_txt("0");
					cg.emit_ins_end();
					cg.emit_ins_begin(reg,"insertvalue");
					cg.emit_type(fp->fn_type); cg.emit_reg(r);
					cg.emit_comma();
					cg.emit_txt("i8* null");
					cg.emit_comma();
					cg.emit_txt("1");
					cg.emit_ins_end();
				} else {// raw function
					cg.emit_ins_begin(reg,"load");
					cg.emit_type(this->type,true);
					cg.emit_fn_ptr(fp->get_mangled_name());
					cg.emit_ins_end();
				}
				return reg;
			}
			else if (auto v=dynamic_cast<Variable*>(val)){
//				fprintf(ofp,"\t%%%s = load ", str(reg));
				// function type..
//				emit_type(ofp,this->type,false);
//				fprintf(ofp,"* %%%s\n",str(val->regname));
				if (v->reg_is_addr){reg=addr=v->regname;}else{reg=v->regname;}
//				ret=v->regname;
				return reg;
			}
		}
		if ((bool)addr) {
			ASSERT(reg==0);
			if (force_regname){reg=force_regname;}
			else if (!reg) reg=next_reg_name(&cg.m_next_reg);
			cg.emit_ins_begin(reg,"load");
			cg.emit_type_reg(type, addr!=0,addr);// an extra pointer level if its' a reference
			cg.emit_ins_end();
			addr=0;
			return reg;
		}
		ASSERT(reg);
		return reg;
	}
	void emit_literal(CodeGen& cg,const ExprLiteral* lit)const{
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
	void emit_operand(CodeGen& cg)const{
		auto ofp=cg.ofp;
		if (reg!=0){
			cg.emit_reg(reg);
			return;
		}
		else if (addr!=0){
			cg.emit_reg(addr);
			return;
		}
		else if (val){
			if (auto lit=dynamic_cast<ExprLiteral*>(val)) {
				ASSERT(addr==0 && "check case above, incorrect assumptions")
				emit_literal(cg,lit);
			}
			return;
		} else {
			fprintf(ofp," <?CgV?> ");
			this->type->dump_if(0);
			ASSERT(this->type);
			ASSERT(0 && "missing register, value wasn't found in resolving");
		}
	}
	RegisterName store(CodeGen& cg,Name srcreg=0){// for read-modify-write
		ASSERT(type);
		if (!addr || !type)
			return reg;
		cg.emit_store((bool)srcreg?srcreg:reg, type, addr);
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
			auto newreg=next_reg_name(&cg.m_next_reg);
			cg.emit_ins_begin(newreg,"insertelement");
			cg.emit_type_reg(this->type,false,reg); cg.emit_comma();
			auto elem_t=this->type->get_elem(elem);
			cg.emit_type_reg(elem_t,false,valreg);
			cg.emit_i32_lit(this->elem);
			cg.emit_ins_end();
			this->reg=newreg;
			return newreg;
		}
		else if (addr && type) {
			if (reg!=0){dbprintf("warning %s overwrite %s?\n", str(reg),str(valreg));}
			ASSERT(reg==0);
			reg=valreg;
			cg.emit_store(valreg, type, addr);
		}
		return valreg;
	}
	CgValue get_elem(CodeGen& cg,const Node* field_name,Scope* sc){	//calculates & returns adress
		ASSERT(type );
		auto sd=type->deref_all()->struct_def;
		if (!sd) {type->dump(-1);error(field_name,"struct not resolved\n");}
		int index=sd->field_index(field_name);
		auto field=sd->find_field(field_name);
		return get_elem_index(cg,index);
	}
	CgValue get_elem_index(CodeGen& cg, int field_index){
		if ((bool)reg && !addr && !(this->type->is_pointer())){
			// lazy ref to inreg field index,
			// 'load'/'store' will do 'insert'/'extract'
			cg.emit_comment("dot reg=%s index=%d",str(reg),field_index);
			return CgValue(reg,type,0,field_index);
		}
		else {
			auto sd=this->type->deref_all()->struct_def;
			auto field=sd->fields[field_index];
			auto areg=cg.next_reg();
			cg.emit_ins_begin(areg, "getelementptr inbounds");
			cg.emit_type_operand(*this);
			cg.emit_i32_lit(0);
			cg.emit_i32_lit(field_index);
			cg.emit_ins_end();
		
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
void CodeGen::emit_reg(RegisterName dst ) {
	fprintf(ofp,"%%%s ",str(dst));
}
Scope* g_Sc;

void CodeGen::emit_type(CgValue& lv) {
	emit_type(lv.type, lv.is_addr());
}
void CodeGen::emit_phi_reg_label(Name reg, Name label){
	emit_comma();
	emit_txt("[");
	emit_reg(reg);
	emit_txt(",");
	emit_reg(label);
	emit_txt("]");
}

void CodeGen::emit_type(const Type* t, bool ref) {
	emit_comma(); // type always starts new operand
	if (!t) { fprintf(ofp,"<type_expected>");return;}
	if (ref) emit_pointer_begin();
	if (t->is_pointer()){
//		dbprintf("THIS IS SUSPECT, REF ISn'T NEEDED TWICE");
		emit_pointer_begin();
		emit_type(t->sub,false);
		emit_pointer_end();
	}else if (t->is_array()) {
		emit_nest_begin("[");
		fprintf(ofp,"%s x ",str(t->array_size())); //sub->next->name));
		emit_type(t->sub,0); // TODO: assert its a numeric constant
		emit_nest_end("]");
	}
	else if (t->name==ELIPSIS){
		emit_txt("...");
	}
	else if (t->name==TUPLE) {
		emit_struct_begin();
		for (auto s=t->sub;s;s=s->next){
			emit_type(s,false);
		}
		emit_struct_end();
	} else if (t->is_struct()){
		auto sd=t->struct_def;
		if (!sd) {
			t->m_origin->dump(0);
			t->dump(-1);
			error(t->m_origin?t->m_origin:t,"struct %s not resolved in %p\n",str(t->name),t);
		}
		if (sd->name) emit_struct_name(sd->get_mangled_name());
		else {
			// LLVM does allow listing an anonymous struct
			emit_struct_begin();
			//error(t,"no struct def");
			for (auto i=0; i<sd->fields.size(); i++){
				emit_type(sd->fields[i]->type(),false);
			}
			emit_struct_end();
		}
	}
	else if (t->is_closure()){
		emit_struct_begin();
		emit_function_type(t);
		emit_comma();
		emit_txt("i8*");
		emit_struct_end();
	}
	else if (t->is_function()){
		//error(t,"TODO,write function type unified ");
		emit_separator(""); // .. because function_type also calls comma
		emit_function_type( t);
	}
	else {
		if (t->is_complex()) {
			fprintf(ofp,"%%%s", str(t->name));
		}
		else
			fprintf(ofp,"%s",t?get_llvm_type_str(t->name):"???");//,t.is_pointer?"*":"");
	}
	if (ref) emit_pointer_end();
}
void CodeGen::emit_type_reg(const Type* t,bool ref, Name reg){
	emit_type(t,ref);
	emit_reg(reg);
}
void CodeGen::emit_type_operand(const CgValue& src){
	emit_type(src.type,(src.reg==0&&src.addr));
	src.emit_operand(*this);
}

void CodeGen::emit_instruction_sub(Name opname,Type* type,  CgValue dst,CgValue src1){
//	ASSERT(dst.is_reg());
	const LLVMOp* op=get_op_llvm(opname,type?type->name:VOID);
	dst.emit_operand(*this);
	emit_ins_name( op?op->op_signed:str(opname));
	if (is_comparison(opname))
		emit_type(src1);
	else{
		emit_type(type,false);
	}
	src1.emit_operand(*this);
}
void CodeGen::emit_instruction(Name opname,Type* type,  CgValue dst,CgValue src1){
	src1.load(*this);
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dst,src1);
	emit_ins_end();
}
void CodeGen::emit_instruction(Name opname,Type* type,  CgValue dst,CgValue src1,CgValue src2){
	ASSERT(type!=0);
	src1.load(*this);
	src2.load(*this);
 
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dst,src1);
	fprintf(ofp,",");
	src2.emit_operand(*this);
	emit_ins_end();
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
		cg.emit_comment("instances of %s in %s %p",str(st->name), sc->name(),st);
		int i=0;
		for (auto ins=st->instances; ins; ins=ins->next_instance,i++){
			cg.emit_comment("instance %d: %s %s in %s %p",i,str(st->name),str(ins->name) ,sc->name(),ins);
			ins->compile(cg, sc);
		}
	} else {
		cg.emit_comment("instance %s of %s in %s %p",str(st->name),st->instance_of?st->instance_of->name_str():"none" ,sc->name(),st);
		cg.emit_struct_name(st->get_mangled_name());
		cg.emit_ins_name("type");
		cg.emit_struct_begin();
		// todo: properly wrap translations to LLVM types.
		int i=0; for (auto fi: st->fields){
			cg.emit_type(fi->type(), false);
		};
		cg.emit_struct_end();
		cg.emit_ins_end();
	}
	return CgValue();	// todo: could return symbol? or its' constructor-function?
}

CgValue CodeGen::emit_alloca_type(Expr* holder, Type* t) {
	RegisterName r= holder?holder->get_reg(t->name, &m_next_reg, false):next_reg();
	emit_ins_begin(r,"alloca"); emit_type(t,false);
	fprintf(ofp,", align %zu", t->alignment());
	emit_ins_end();
	return CgValue(0,t, r);
}

void emit_local_vars(CodeGen& cg,Expr* n, ExprFnDef* fn, Scope* sc) {
	auto ofp=cg.ofp;
	for (auto v=sc->vars; v;v=v->next_of_scope){
		if (v->kind!=Local) continue;
		auto vt=v->expect_type();
		//if (!v->on_stack)
		//	continue; //reg vars experiment
		if (!vt->is_complex())
			continue;//its just a reg
		auto r= v->get_reg(v->name, &cg.m_next_reg, true);
		if (vt->is_struct()) {
			// alloc_struct
//			fprintf(ofp,"\t"); emit_reg(ofp,r); fprintf(ofp," = alloca %%%s , align %d\n",getString(vt->name),vt->struct_def->alignment());
			cg.emit_alloca_type(v, vt);
			v->reg_is_addr=true;
		} else if (vt->is_array()){
			auto t=vt->sub;
			if (!t || !t->next){error(v,"array type needs 2 args");}
			fprintf(cg.ofp,"\t"); cg.emit_reg(r); fprintf(cg.ofp," = alloca [%s x %s] , align %zu\n",str(t->next->name),get_llvm_type_str(t->name),vt->alignment());
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
void CodeGen::emit_label(Name l){
	fprintf(ofp,"%s:\n",str(l));
}
void CodeGen::emit_branch( Name l){
	emit_ins_begin_name("br");
	fprintf(ofp,"label %%%s",str(l));
	emit_ins_end();
}
void CodeGen::emit_branch(CgValue cond, Name label_then, Name label_else){
	cond.load(*this);
	emit_ins_begin_name("br");
	fprintf(ofp,"i1 %%%s, label %%%s, label %%%s",str(cond.reg), str(label_then), str(label_else));
	emit_ins_end();
}

CgValue ExprIf::compile(CodeGen& cg,Scope*sc){
	// todo - while etc can desugar as for(;cond;)body, for(){ body if(cond)break}
	auto curr_fn=cg.curr_fn;
	auto ifn=this;
	// TODO: Collect phi-nodes for anything modified inside.
	RegisterName outname=cg.next_reg();
	auto condition=ifn->cond->compile(cg,sc);
	int index=cg.m_next_reg++;
	auto label_if=gen_label("if",index);
	auto label_endif=gen_label("endif",index);
	if (ifn->else_block){
		auto label_else=gen_label("else",index);
		cg.emit_branch(condition,label_if,label_else);
		cg.emit_label(label_if);
		auto if_result=ifn->body->compile(cg,sc);
		if_result.load(cg,0);
		cg.emit_branch(label_endif);
		cg.emit_label(label_else);
		auto else_result=ifn->else_block->compile(cg,sc);
		else_result.load(cg,0);
		cg.emit_branch(label_endif);
		cg.emit_label(label_endif);
		// phi node picks result, conditional assignment
		if (if_result.is_valid() && else_result.is_valid()){
			cg.emit_ins_begin(outname,"phi");
			cg.emit_type(if_result.type, if_result.is_addr());
			cg.emit_separator("");
			cg.emit_phi_reg_label(if_result.reg,label_if);
			cg.emit_phi_reg_label(else_result.reg,label_else);
			cg.emit_ins_end();
		}
		auto return_type=ifn->get_type();
		return CgValue(outname,return_type);
	}
	else {
		cg.emit_branch(condition,label_if,label_endif);
		auto ifblock=ifn->body->compile(cg,sc);
		cg.emit_label(label_endif);
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
void emit_phi(CodeGen& cg, Scope* sc, vector<LoopPhiVar>& phi_vars,Name l_pre, Name l_end, bool extra) {
	for (auto& v: phi_vars) {
		v.reg_end=v.var->regname;
		cg.emit_ins_begin(v.reg_start,"phi");
		cg.emit_type(v.var->type(), false);//v.val.is_addr());
		cg.emit_separator("");
		cg.emit_phi_reg_label(v.reg_pre,l_pre);
		cg.emit_phi_reg_label(v.reg_end,l_end);
		cg.emit_ins_end();
	}
	if (extra) for (auto i=phi_vars.size(); i>0;i--)fprintf(cg.ofp,"       ");	// dirty hack to prevent source overun
	cg.emit_txt("\n");
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
	int index=cg.m_next_reg++;
	auto l_init=gen_label("init",index);
	cg.emit_branch(l_init);
	cg.emit_label(l_init);
	auto init=nf->init->compile(cg,sc);
	
	set<Variable*> emit_vars;
	set<Variable*> else_vars;
	// Now scan all the blocks to see which are changed.
	if (nf->cond)nf->cond->find_vars_written(sc,emit_vars);
	if (nf->body)nf->body->find_vars_written(sc,emit_vars);
	if (nf->incr)nf->incr->find_vars_written(sc,emit_vars);
	vector<LoopPhiVar> phi_vars;
	for (auto v :emit_vars){
		LoopPhiVar phi;
//		phi.val=
		phi.var=v;
		phi.reg_pre=v->regname;
		phi.reg_start=v->regname=next_reg_name(v->name,&cg.m_next_reg);
		phi.reg_end=0;//gen_label(str(v->name),next_index);
		//todo: can we allocate regnames in the AST? find last-write?
		phi_vars.push_back(phi);
	}
	auto l_for=gen_label("cond",index);
	auto l_body=gen_label("body",index);
	auto l_else=gen_label("else",index);
	auto l_endfor=gen_label("endfor",index);
	cg.emit_branch(l_for);
	cg.emit_label(l_for);
	auto phipos=ftell(ofp);// YUK. any way around this? eg write condition at end?
	emit_phi(cg,sc,phi_vars,l_init,l_for,true);//alloc space

	auto cond_result=nf->cond?nf->cond->compile(cg,sc):CgValue();
	cg.emit_branch(cond_result, l_body, l_else);
	cg.emit_label(l_body);
	if (nf->body) nf->body->compile(cg,sc);
	if (nf->incr) nf->incr->compile(cg,sc);
	cg.emit_branch(l_for);
	cg.emit_label(l_else);
	if (nf->else_block) nf->else_block->compile(cg,sc);
	cg.emit_branch(l_endfor);
	cg.emit_label(l_endfor);
	// now write the phi-nodes.
	auto endpos=ftell(ofp);// YUK. any way around this?
	fseek(ofp, phipos,SEEK_SET);
	emit_phi(cg,sc,phi_vars,l_init,l_body,false);
	fseek(ofp, 0,SEEK_END);

	//TODO: return value.
	return CgValue();
}

CgValue CodeGen::emit_cast(CgValue dst, CgValue&lhsv, Expr* rhse){
	
	// todo rename 'src', 'dst' to avoid blah as blah confusion vs C cast (type)(expr)
	lhsv.load(*this);
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
	emit_ins_begin(dst.reg, ins);
	emit_type_reg(lhsv.type,0,lhsv.reg);
	emit_separator(" to ");
	emit_type(rhst,0);
	emit_ins_end();
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
		return lhs.get_elem(cg,e->rhs,sc);
	}
	else if (e->lhs && e->rhs){
		auto lhs=e->lhs->compile(cg,sc);
		auto rhs=e->rhs->compile(cg,sc);
		auto lhs_v=sc->find_variable_rec(e->lhs->name);
		auto outname=lhs_v?lhs_v->name:opname;
		
		auto dst=CgValue(n->get_reg(outname,&cg.m_next_reg,false),n->get_type());
		
		if (opname==ASSIGN_COLON){ // do nothing-it was sema'sjob to create a variable.
			ASSERT(sc->find_scope_variable(e->lhs->name));
			if (lhs_v) dst.reg=lhs_v->regname;
				//					lhs_v->regname=dst.reg;
				return CgValue(0,lhs_v->type(),dst.reg);
				}
		else if(opname==AS) {
			// if (prim to prim) {do fpext, etc} else..
			return cg.emit_cast(dst,lhs,e);
		}
		else
		if (opname==LET_ASSIGN){// Let-Assign *must* create a new variable.
			auto v=sc->find_variable_rec(e->lhs->name); WARN(v &&"semantic analysis should have created var");
			auto dst=v->get_reg(v->name, &cg.m_next_reg, true);
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
			auto dstreg=lhs;
			cg.emit_instruction(opname,t?t:rhs.type, dst,lhs,rhs);
			auto out=dstreg.store_from(cg,dst.reg);
			return dstreg;
		}else {
			// RISClike 3operand dst=op(src1,src2)
			cg.emit_instruction(opname,t,dst,lhs,rhs);
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
			if (opflags & (WRITE_LHS|WRITE_RHS)){static int once;if (once++){dbprintf(";TODO: logic for modifications to memory");}}
			// todo: handle read/modify-writeness.
			// postincrement/preincrement etc go here..
			auto dst=CgValue(n->get_reg(opname,&cg.m_next_reg,false), n->get_type());
			cg.emit_instruction(opname,t,dst,src);
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
		auto struct_val= cg.emit_alloca_type(e, e->type());
		e->regname=struct_val.reg; // YUK todo - reallyw wanted reg copy
		// can we trust llvm to cache the small cases in reg..
		for (int i=0; i<e->argls.size();i++) {
			auto rvalue=si.value[i]->compile(cg,sc);
			auto dst = struct_val.get_elem(cg,si.field_refs[i],sc);
			auto srcreg = rvalue.load(cg,0);
			dst.store_from(cg,srcreg);
			if (dst.type==struct_val.type)
				struct_val=dst; // mutate by insertion
		}
		return struct_val;
	}
	// [2] Operator
	//[3] ARRAY ACCESSs
	else if (auto ar=n->is_subscript()){
		auto expr=ar->call_expr->compile(cg,sc);// expression[index]
		auto index=ar->argls[0]->compile(cg,sc);
		auto array_type=expr.type;
		auto inner_type=array_type->sub;
		auto dst=ar->get_reg(ARRAY,&cg.m_next_reg,false);
		if (!n->regname){n->regname=expr.reg;}
		
		expr.load(cg);
		index.load(cg);
		cg.emit_ins_begin(dst,"getelementptr inbounds");
		cg.emit_type_reg(array_type,true,expr.reg);//!expr.reg);
		cg.emit_i32_lit(0);
		cg.emit_type_operand(index);
		return CgValue(0,inner_type,dst);
	}
	//[3] FUNCTION CALL
	else if (e->is_function_call()){
		// [3.1]evaluate arguments
		vector<CgValue> l_args;
		vector<CgValue> l_args_reg;
		// process function argumetns & load
		for (auto arg:e->argls){
			auto reg=arg->compile(cg,sc);
			if (!reg.type) {
				error_begin(arg,"arg type not resolved in call\n");
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

		//[3.2] evaluate call object..
		auto call_fn=e->get_fn_call();
		RegisterName indirect_call=0;
		cg.emit_comment("fncall %s", call_fn?str(call_fn->name):e->call_expr->name_str());

		auto ret_type=e->type();	// semantic analysis should have done this
		auto dst=(ret_type->name!=VOID)?n->get_reg_new(call_fn?call_fn->name:FN, &cg.m_next_reg):0;
		//auto rt=call_fn->get_return_value();

		auto emit_arg_list=[&](Type* rect,RegisterName receiver){
			cg.emit_args_begin();
			if(rect && receiver){
				cg.emit_type(rect);
				cg.emit_reg(receiver);
			}
			for (auto a: l_args){
				cg.emit_type_operand(a);
			}
			cg.emit_args_end();
		};

		//[3.3] make the call..
		if (e->call_expr->is_function_name()) {
			//[3.3.1] Direct Call
			cg.emit_ins_begin(dst,"call");
			cg.emit_function_type(call_fn);// NOTE we need this until we handle elipsis
			cg.emit_global(call_fn->get_mangled_name());
			emit_arg_list(0,0);
			cg.emit_ins_end();
			
		} else {
			//[3.3.2] Indirect Call... Function Pointer
			auto fn_obj = e->call_expr->compile(cg, sc);
			indirect_call=fn_obj.load(cg);
			if (fn_obj.type->is_closure()){
				//[.1] ..call Closure (function,environment*)
				indirect_call=cg.emit_extractvalue(cg.next_reg(),fn_obj.type,indirect_call,0);
				cg.emit_ins_begin(dst,"call");
				cg.emit_function_type(e->call_expr->type());
				cg.emit_reg(indirect_call);
				emit_arg_list(0,0);
				cg.emit_ins_end();
			} else{
				//[.2] ..Raw Function Pointer
				cg.emit_ins_begin(dst,"call");
				cg.emit_function_type(e->call_expr->type());
				cg.emit_reg(indirect_call);
				emit_arg_list(0,0);
				cg.emit_ins_end();
			}
		}
			// [3.3.2] Indirect Call, Closure.
	
		
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
		if (auto fi=n->def->as_field()){
			// emit Load instruction
			auto thisv=sc->find_variable_rec(THIS);
			sc->dump(0);
			ASSERT(thisv&&"attempting to find field in non method,?");
			return CgValue(thisv).get_elem(cg, this, sc);
		} else
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
void CodeGen::emit_function_signature(ExprFnDef* fn_node, EmitFnMode mode){
	auto& cg=*this;
	auto scope=fn_node->scope;
	fn_node->clear_reg();
	auto rtype=fn_node->return_type();
	cg.emit_nest_begin( "");
	cg.emit_txt(mode==EmitDefinition?"define ":mode==EmitDeclaration?"declare ":" ");
	if (mode==EmitType) cg.emit_pointer_begin();
	cg.emit_type(rtype,false);
	if (mode!=EmitType)
		cg.emit_global(fn_node->get_mangled_name());
	cg.emit_args_begin();
	int inter=0;
	if(auto r=fn_node->fn_type->get_receiver()){
		cg.emit_type(r,false);
		if (mode==EmitDefinition){
			auto var=scope->get_or_create_scope_variable(r,THIS, VkArg);
			var->get_reg(THIS, &cg.m_next_reg, false);
			cg.emit_reg(var->regname);
		}
	}
	for (auto a:fn_node->args){
		cg.emit_type(a->type(),false);//was a->is_complex. confusion here over pass by ref/val. we think fn sigs should be 1:1. but raw struct type should  be pass by val? will we have to copy struct val?
		if (mode==EmitDefinition){
			auto var=scope->get_or_create_scope_variable(a,a->name, VkArg);
			var->get_reg(a->name, &cg.m_next_reg, false);
			cg.emit_reg(var->regname);
		}
	}

	if (fn_node->variadic) {
		cg.emit_comma();
		cg.emit_txt("...");
	}
	cg.emit_args_end();
	if (mode==EmitType) cg.emit_pointer_end();
	else cg.emit_txt("\n");
	cg.emit_nest_end("");
}
void CodeGen::emit_function_type(const Type* t) {
	auto& cg=*this;
	auto ofp=cg.ofp;
	auto argtuple=t->sub;
	ASSERT(argtuple);
	auto retn=argtuple->next;
	cg.emit_comma();
	cg.emit_pointer_begin();
	cg.emit_type(retn,0);
	cg.emit_args_begin();
	if (auto rt=t->get_receiver()){
		cg.emit_type(rt);
	}
	for (auto arg=argtuple->sub; arg;arg=arg->next){
		cg.emit_type(arg);
	}
	cg.emit_args_end();
	cg.emit_pointer_end();
}
void CodeGen::emit_function_type(ExprFnDef* fn_node){
	emit_function_signature(fn_node,EmitType);
}

CgValue ExprFnDef::compile(CodeGen& cg,Scope* outer_scope){
	auto fn_node = this;
	auto ofp=cg.ofp;

	if (!fn_node){return CgValue();}
	if (fn_node->is_undefined()) {
		fprintf(ofp,";fn %s prot\n",getString(fn_node->name));
		cg.emit_function_signature(fn_node,EmitDeclaration);
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

	cg.emit_nest_begin("");
	cg.emit_fn_ptr(fn_node->get_mangled_name());
	cg.emit_ins_name("global");
	cg.emit_function_type(fn_node->type());
	cg.emit_global(fn_node->get_mangled_name());
	cg.emit_nest_end("");
	if (!fn_node->get_type() && fn_node->fn_type && fn_node->scope ){
		error(fn_node,"function name %s %s %p %p %p %p", str(fn_node->name),str(fn_node->get_mangled_name()), fn_node->instance_of, fn_node->get_type(), fn_node->fn_type, fn_node->scope);
		ASSERT(0 && "function must be resolved to compile it");
		return CgValue();
	}
	cg.emit_comment("fn %s (%p) :- ins=%p of %p ", str(fn_node->name),fn_node, fn_node->instances, fn_node->instance_of);

	auto scope=fn_node->scope;
	
	cg.emit_function_signature(fn_node,EmitDefinition);
 	fprintf(ofp,"{\n");
	if (fn_node->instance_of!=nullptr){
		cg.emit_comment("compiling generic fn body");
	}
	emit_local_vars(cg, fn_node->body, fn_node, scope);
	auto rtn=fn_node->get_return_value();
	auto ret=fn_node->body->compile(cg,scope);
	if (ret.is_valid() && !ret.type->is_void()) {
		ret.load(cg);
		cg.emit_ins_begin_name("ret");
		//emit_type(cg,ret);//rtn->get_type(),ret.is_addr());
		//ret.emit_operand(cg);
		cg.emit_type_operand(ret);
		cg.emit_ins_end();
	} else {
		cg.emit_ins_begin_name("ret");
		cg.emit_txt("void");
		cg.emit_ins_end();
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

//UINT,SIZE_T,I8,I16,I32,I64,U8,U16,U32,U64,U128,BOOL,	// int types
//HALF,FLOAT,DOUBLE,FLOAT4,CHAR,STR,VOID,VOIDPTR,

const char* g_mangle_type[]={
	"i","u","z","c","s","l","ll","uc","us","ul","ull","","b",
	"h","f","d","6float4","c","Pc","v","Pv",
};
void name_mangle_append_type(char* dst,int size, const Type* t){
	if (!t) return;
		// todo - check how template params are suppsoed to mangle
		// we suspect the template params cover this... fn's params are mangled and this should just be struct->name
		//name_mangle_append_name(dst,size,t->struct_def->get_mangled_name());

	auto n=t->name;
	if (n==PTR){ strcat(dst,"P");}
	else if (n>=RAW_TYPES && n<=VOIDPTR) {
		strcat(dst,g_mangle_type[(int)n-RAW_TYPES]);
	}
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

	cg.emit_comment("from scope %s\n;",scope->name());
	// output all inner items that outer stuff depends on..
	// literals first, because we setup llvm_strlen. TODO , more solid design pls.
	for (auto l=scope->literals; l; l=l->next_of_scope) {
		if (l->type_id==T_CONST_STRING){
		const char* name=getString(l->name);
			char buffer[512];
			l->llvm_strlen=translate_llvm_string_constant(buffer,512, l->as_str())+1;
			cg.emit_global(l->name);
			cg.emit_txt("= private unnamed_addr constant [%d x i8] c\"%s\\00\"\n", l->llvm_strlen, buffer);
		}
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
	// compile child items last, as they depend on me.
	for (auto sub=scope->child; sub; sub=sub->next) {
		output_code(cg.ofp,sub);
	}
}




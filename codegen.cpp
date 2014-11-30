#include "codegen.h"

// TODO: properly abstract llvm instruction generation to move to llvm api.
inline void dbprintf_mangle(const char*,...){}
extern Name getStringIndex(const char* str,const char* end) ;
extern const char* getString(const Name&);
extern bool is_operator(Name tok);
extern bool is_ident(Name tok);
extern bool is_type(Name tok);
void emit_reg(CodeGen& cg, RegisterName dst );
void emit_struct_name(CodeGen& cg, RegisterName dst ){emit_reg(cg,dst);}
void emit_type(CodeGen& cg, CgValue& lv );
void emit_type(CodeGen& cg, const Type* t, bool is_ref=false);
void emit_type_reg(CodeGen& cg, const Type* t,bool ref, Name reg);
void emit_function_type(CodeGen& cg,ExprFnDef* fn_node);
void emit_function_type(CodeGen& cg, const Type* t);
void emit_global(CodeGen& cg, Name n);
CgValue emit_alloca_type(CodeGen& cg, Expr* holder, Type* t);

Name next_reg_name(int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d",(*next_reg_index)++);
	return getStringIndex(tmp);
}
Name next_reg(CodeGen& cg){
	char tmp[64]; sprintf(tmp,"r%d",cg.next_reg++);
	return getStringIndex(tmp);
}
Name next_reg_name(Name prefix_name, int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d%s",(*next_reg_index)++,str(prefix_name));
	return getStringIndex(tmp);
}
void emit_reg(CodeGen& cg, RegisterName reg);

void emit_ins_begin_sub(CodeGen& cg){fprintf(cg.ofp,"\t"); cg.comma=false;}
void emit_undef(CodeGen& cg){fprintf(cg.ofp,"undef");}
void emit_ins_name(CodeGen& cg,const char* txt){fprintf(cg.ofp,"= %s ",txt);}
void emit_ins_begin_name(CodeGen& cg,const char* txt){emit_ins_begin_sub(cg);fprintf(cg.ofp,"%s ",txt);}
void emit_ins_end(CodeGen& cg){fprintf(cg.ofp,"\n");}
void emit_txt(CodeGen& cg, const char* str,...){// catch all, just spit out given string
	char tmp[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	fprintf(cg.ofp,"%s",tmp);
}

void emit_nest_begin(CodeGen& cg, const char* str){
	ASSERT(cg.depth<32);
	cg.commas[cg.depth]=cg.comma;
	cg.depth++;
	emit_txt(cg,str);
	cg.comma=0;
}
void emit_nest_end(CodeGen& cg, const char* str){
	ASSERT(cg.depth);
	cg.depth--;
	cg.comma=cg.commas[cg.depth];
	emit_txt(cg,str);
}
void emit_args_begin(CodeGen& cg)	{emit_nest_begin(cg,"(");}
void emit_args_end(CodeGen& cg)		{emit_nest_end(cg,")");}
void emit_struct_begin(CodeGen& cg)	{emit_nest_begin(cg,"{");}
void emit_struct_end(CodeGen& cg)	{emit_nest_end(cg,"}");}
void emit_pointer_begin(CodeGen&cg)	{emit_nest_begin(cg,"");};
void emit_pointer_end(CodeGen&cg)	{emit_nest_end(cg,"*");};

void emit_comma(CodeGen& cg){
	if (cg.comma==1){fprintf(cg.ofp,",");}
	cg.comma=1;
}
void emit_separator(CodeGen& cg,const char* txt){
	emit_txt(cg,txt);
	cg.comma=0;
}
RegisterName  emit_ins_begin(CodeGen& cg, RegisterName reg, const char* op){
	if (!reg) { emit_ins_begin_name(cg,op);}
	else {
		emit_ins_begin_sub(cg);
		emit_reg(cg,reg);
		emit_ins_name(cg,op);
	}
	return reg;
}
void emit_i32_lit(CodeGen& cg,int index) {
	emit_comma(cg);
	fprintf(cg.ofp,"i32 %d",index);
}
void emit_i32_reg(CodeGen& cg,Name reg) {
	emit_comma(cg);
	fprintf(cg.ofp,"i32 ");
	emit_reg(cg,reg);
}
RegisterName	emit_extractvalue(CodeGen& cg,RegisterName dst,Type* type,RegisterName src,int index){
	emit_ins_begin(cg,dst,"extractvalue");
	emit_type(cg,type);
	emit_reg(cg,src);
	emit_comma(cg);
	emit_txt(cg,"%d",index);
	emit_ins_end(cg);
	return dst;
}

void emit_store(CodeGen& cg, RegisterName reg, Type* type, RegisterName addr){
	emit_ins_begin_name(cg,"store");
	emit_type(cg, type,0);
	emit_reg(cg, reg);
	emit_type(cg, type,1);//fprintf(ofp,"* ");
	emit_reg(cg, addr); fprintf(cg.ofp,", align 4");
	emit_ins_end(cg);
}
void emit_global(CodeGen& cg, Name n){
	emit_txt(cg,"@%s ",str(n));
}
void emit_fn_ptr(CodeGen& cg, Name n){
	emit_txt(cg,"@%s.ptr",str(n));
}
void emit_fn(CodeGen& cg, Name n){
	emit_txt(cg,"@%s",str(n));
}
void emit_struct_ptr(CodeGen& cg, Name n){
	emit_pointer_begin(cg);
	emit_txt(cg,"%%%s",str(n));
	emit_pointer_end(cg);
}
void emit_comment(CodeGen& cg, const char* str,...){// catch all, just spit out given string
	char tmp[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	fprintf(cg.ofp,"\t;%s\n",tmp);
}
void emit_type_operand(CodeGen& cg, const CgValue& src);

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
		CgValue stack_val(0,this->type,emit_alloca_type(cg,nullptr,this->type).addr,this->elem);
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
				emit_comment(cg,"dot reg=%s addr=%s index=%d",str(reg),str(addr),elem);
				auto sub=this->get_elem_index(cg,elem);
				return sub.load(cg);
			} else{
				// elem acess
				auto newreg=(bool)force_regname?force_regname:next_reg_name(&cg.next_reg);
				emit_ins_begin(cg,newreg,"extractelement");
				emit_type_reg(cg,this->type,false,reg);
				emit_i32_lit(cg,this->elem);
				emit_ins_end(cg);
				this->type=this->type->get_elem(elem);
				this->reg=newreg;
				return newreg;
			}
		}
		if(val) {
			if (force_regname){reg=force_regname;}
			else if (!reg) reg=next_reg_name(&cg.next_reg);
			
			if (auto lit=dynamic_cast<ExprLiteral*>(val)){
				if (lit->type()->name==INT){
					emit_ins_begin(cg,reg,"or");
					emit_i32_lit(cg,0);  this->emit_literal(cg,lit);
				} else if(lit->type()->name==FLOAT){
					// todo, i guess we're goint to have t make a global constants table
					emit_ins_begin(cg,reg,"fadd");
					fprintf(cg.ofp,"float 0.0, ");  this->emit_literal(cg,lit);
				} else if (lit->type()->name==STR){
					emit_ins_begin(cg,reg,"getelementptr inbounds");
					emit_comma(cg);
					emit_txt(cg,"[%d x i8]* @%s",lit->llvm_strlen, getString(lit->name));

					emit_i32_lit(cg,0);
					emit_i32_lit(cg,0);
					ASSERT(lit->llvm_strlen);
				} else {
					error(lit,"literal type not handled yet");
				}
				emit_ins_end(cg);
				// todo: string literal, vector literals, bit formats
			}
			else if (auto fp=dynamic_cast<ExprFnDef*>(val)){
				if (fp->is_closure()) {
					// Build a pair. we would also build the Capture Object here.
					// TODO: we're blocked from making a nice' emit_make_pair()' wrapper here
					// because: fp->fn_type is interpretted as {ptr,env}, but we must manually
					// tell it to build the first & second types.
					auto r=emit_ins_begin(cg,next_reg(cg),"insertvalue");
					emit_type(cg, fp->fn_type); emit_undef(cg);
					emit_function_type(cg,fp->fn_type);
					emit_fn(cg,fp->get_mangled_name());
					emit_comma(cg);
					emit_txt(cg,"0");
					emit_ins_end(cg);
					emit_ins_begin(cg,reg,"insertvalue");
					emit_type(cg, fp->fn_type); emit_reg(cg,r);
					emit_comma(cg);
					emit_txt(cg,"i8* null");
					emit_comma(cg);
					emit_txt(cg,"1");
					emit_ins_end(cg);
				} else {// raw function
					emit_ins_begin(cg,reg,"load");
					emit_type(cg,this->type,true);
					emit_fn_ptr(cg,fp->get_mangled_name());
					emit_ins_end(cg);
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
			else if (!reg) reg=next_reg_name(&cg.next_reg);
			emit_ins_begin(cg,reg,"load");
			emit_type_reg(cg, type, addr!=0,addr);// an extra pointer level if its' a reference
			emit_ins_end(cg);
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
			emit_reg(cg,reg);
			return;
		}
		else if (addr!=0){
			emit_reg(cg,addr);
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
			ASSERT(0 && "missing register type");
		}
	}
	RegisterName store(CodeGen& cg,Name srcreg=0){// for read-modify-write
		ASSERT(type);
		if (!addr || !type)
			return reg;
		emit_store(cg, (bool)srcreg?srcreg:reg, type, addr);
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
			emit_ins_begin(cg,newreg,"insertelement");
			emit_type_reg(cg,this->type,false,reg); emit_comma(cg);
			auto elem_t=this->type->get_elem(elem);
			emit_type_reg(cg,elem_t,false,valreg);
			emit_i32_lit(cg, this->elem);
			emit_ins_end(cg);
			this->reg=newreg;
			return newreg;
		}
		else if (addr && type) {
			if (reg!=0){dbprintf("warning %s overwrite %s?\n", str(reg),str(valreg));}
			ASSERT(reg==0);
			reg=valreg;
			emit_store(cg, valreg, type, addr);
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
			emit_comment(cg,"dot reg=%s index=%d",str(reg),field_index);
			return CgValue(reg,type,0,field_index);
		}
		else {
			auto sd=this->type->deref_all()->struct_def;
			auto field=sd->fields[field_index];
			auto areg=next_reg(cg);
			emit_ins_begin(cg,areg, "getelementptr inbounds");
			emit_type_operand(cg, *this);
			emit_i32_lit(cg,0);
			emit_i32_lit(cg,field_index);
			emit_ins_end(cg);
		
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
void emit_reg(CodeGen& cg, RegisterName dst ) {
	fprintf(cg.ofp,"%%%s ",str(dst));
}
Scope* g_Sc;
void emit_type(CodeGen& cg, CgValue& lv) {
	emit_type(cg,lv.type, lv.is_addr());
}
void emit_phi_reg_label(CodeGen& cg, Name reg, Name label){
	emit_comma(cg);
	emit_txt(cg,"[");
	emit_reg(cg,reg);
	emit_txt(cg,",");
	emit_reg(cg,label);
	emit_txt(cg,"]");
}

void emit_type(CodeGen& cg, const Type* t, bool ref) {
	auto ofp=cg.ofp;
	emit_comma(cg); // type always starts new operand
	if (!t) { fprintf(ofp,"<type_expected>");return;}
	if (ref) emit_pointer_begin(cg);
	if (t->is_pointer()){
//		dbprintf("THIS IS SUSPECT, REF ISn'T NEEDED TWICE");
		emit_pointer_begin(cg);
		emit_type(cg,t->sub,false);
		emit_pointer_end(cg);
	}else if (t->is_array()) {
		emit_nest_begin(cg,"[");
		fprintf(ofp,"%s x ",str(t->array_size())); //sub->next->name));
		emit_type(cg,t->sub,0); // TODO: assert its a numeric constant
		emit_nest_end(cg,"]");
	}
	else if (t->name==ELIPSIS){
		emit_txt(cg,"...");
	}
	else if (t->name==TUPLE) {
		emit_struct_begin(cg);
		for (auto s=t->sub;s;s=s->next){
			emit_type(cg,s,false);
		}
		emit_struct_end(cg);
	} else if (t->is_struct()){
		auto sd=t->struct_def;
		if (!sd) {
			t->m_origin->dump(0);
			t->dump(-1);
			error(t->m_origin?t->m_origin:t,"struct %s not resolved in %p\n",str(t->name),t);
		}
		if (sd->name) emit_struct_name(cg, sd->get_mangled_name());
		else {
			// LLVM does allow listing an anonymous struct
			emit_struct_begin(cg);
			//error(t,"no struct def");
			for (auto i=0; i<sd->fields.size(); i++){
				emit_type(cg,sd->fields[i]->type(),false);
			}
			emit_struct_end(cg);
		}
	}
	else if (t->is_closure()){
		emit_struct_begin(cg);
		emit_function_type(cg,t);
		emit_comma(cg);
		emit_txt(cg,"i8*");
		emit_struct_end(cg);
	}
	else if (t->is_function()){
		//error(t,"TODO,write function type unified ");
		emit_separator(cg,""); // .. because function_type also calls comma
		emit_function_type(cg, t);
	}
	else {
		if (t->is_complex()) {
			fprintf(ofp,"%%%s", str(t->name));
		}
		else
			fprintf(ofp,"%s",t?get_llvm_type_str(t->name):"???");//,t.is_pointer?"*":"");
	}
	if (ref) emit_pointer_end(cg);
}
void emit_type_reg(CodeGen& cg, const Type* t,bool ref, Name reg){
	emit_type(cg,t,ref);
	emit_reg(cg,reg);
}
void emit_type_operand(CodeGen& cg, const CgValue& src){
	emit_type(cg,src.type,(src.reg==0&&src.addr));
	src.emit_operand(cg);
}

void emit_instruction_sub(CodeGen& cg, Name opname,Type* type,  CgValue dst,CgValue src1){
//	ASSERT(dst.is_reg());
	const LLVMOp* op=get_op_llvm(opname,type?type->name:VOID);
	dst.emit_operand(cg);
	emit_ins_name(cg, op?op->op_signed:str(opname));
	if (is_comparison(opname))
		emit_type(cg,src1);
	else{
		emit_type(cg,type,false);
	}
	src1.emit_operand(cg);
}
void emit_instruction(CodeGen& cg, Name opname,Type* type,  CgValue dst,CgValue src1){
	src1.load(cg);
	emit_ins_begin_sub(cg);
	emit_instruction_sub(cg,opname,type,dst,src1);
	emit_ins_end(cg);
}
void emit_instruction(CodeGen& cg, Name opname,Type* type,  CgValue dst,CgValue src1,CgValue src2){
	ASSERT(type!=0);
	src1.load(cg);
	src2.load(cg);
 
	emit_ins_begin_sub(cg);
	emit_instruction_sub(cg,opname,type,dst,src1);
	fprintf(cg.ofp,",");
	src2.emit_operand(cg);
	emit_ins_end(cg);
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
		emit_comment(cg,"instances of %s",str(st->name));
		int i=0;
		for (auto ins=st->instances; ins; ins=ins->next_instance,i++){
			emit_comment(cg,"instance %d:",i);
			ins->compile(cg, sc);
		}
	} else {
		emit_struct_name(cg,st->get_mangled_name());
		emit_ins_name(cg,"type");
		emit_struct_begin(cg);
		// todo: properly wrap translations to LLVM types.
		int i=0; for (auto fi: st->fields){
			emit_type(cg,fi->type(), false);
		};
		emit_struct_end(cg);
		emit_ins_end(cg);
	}
	return CgValue();	// todo: could return symbol? or its' constructor-function?
}

CgValue emit_alloca_type(CodeGen& cg, Expr* holder, Type* t) {
	RegisterName r= holder?holder->get_reg(t->name, &cg.next_reg, false):next_reg(cg);
	emit_ins_begin(cg,r,"alloca"); emit_type(cg,t,false);
	fprintf(cg.ofp,", align %zu", t->alignment());
	emit_ins_end(cg);
	return CgValue(0,t, r);
}

void emit_local_vars(CodeGen& cg, Expr* n, ExprFnDef* fn, Scope* sc) {
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
//			fprintf(ofp,"\t"); emit_reg(ofp,r); fprintf(ofp," = alloca %%%s , align %d\n",getString(vt->name),vt->struct_def->alignment());
			emit_alloca_type(cg, v, vt);
			v->reg_is_addr=true;
		} else if (vt->is_array()){
			auto t=vt->sub;
			if (!t || !t->next){error(v,"array type needs 2 args");}
			fprintf(cg.ofp,"\t"); emit_reg(cg,r); fprintf(cg.ofp," = alloca [%s x %s] , align %zu\n",str(t->next->name),get_llvm_type_str(t->name),vt->alignment());
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
	emit_ins_begin_name(cg,"br");
	fprintf(cg.ofp,"label %%%s",str(l));
	emit_ins_end(cg);
}
void emit_branch(CodeGen& cg, CgValue cond, Name label_then, Name label_else){
	cond.load(cg);
	emit_ins_begin_name(cg,"br");
	fprintf(cg.ofp,"i1 %%%s, label %%%s, label %%%s",str(cond.reg), str(label_then), str(label_else));
	emit_ins_end(cg);
}

CgValue ExprIf::compile(CodeGen& cg,Scope*sc){
	// todo - while etc can desugar as for(;cond;)body, for(){ body if(cond)break}
	auto curr_fn=cg.curr_fn;
	auto ifn=this;
	// TODO: Collect phi-nodes for anything modified inside.
	RegisterName outname=next_reg(cg);
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
			emit_ins_begin(cg,outname,"phi");
			emit_type(cg,if_result.type, if_result.is_addr());
			emit_separator(cg,"");
			emit_phi_reg_label(cg,if_result.reg,label_if);
			emit_phi_reg_label(cg,else_result.reg,label_else);
			emit_ins_end(cg);
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
void emit_phi(CodeGen& cg, Scope* sc, vector<LoopPhiVar>& phi_vars,Name l_pre, Name l_end, bool extra) {
	for (auto& v: phi_vars) {
		v.reg_end=v.var->regname;
		emit_ins_begin(cg,v.reg_start,"phi");
		emit_type(cg,v.var->type(), false);//v.val.is_addr());
		emit_separator(cg,"");
		emit_phi_reg_label(cg,v.reg_pre,l_pre);
		emit_phi_reg_label(cg,v.reg_end,l_end);
		emit_ins_end(cg);
	}
	if (extra) for (auto i=phi_vars.size(); i>0;i--)fprintf(cg.ofp,"       ");	// dirty hack to prevent source overun
	emit_txt(cg,"\n");
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
	emit_phi(cg,sc,phi_vars,l_init,l_for,true);//alloc space

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
	emit_phi(cg,sc,phi_vars,l_init,l_body,false);
	fseek(ofp, 0,SEEK_END);

	//TODO: return value.
	return CgValue();
}

CgValue emit_cast(CodeGen& cg,CgValue dst, CgValue&lhsv, Expr* rhse){
	
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
	emit_ins_begin(cg, dst.reg, ins);
	emit_type_reg(cg,lhsv.type,0,lhsv.reg);
	emit_separator(cg," to ");
	emit_type(cg,rhst,0);
	emit_ins_end(cg);
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
		
		auto dst=CgValue(n->get_reg(outname,&cg.next_reg,false),n->get_type());
		
		if (opname==ASSIGN_COLON){ // do nothing-it was sema'sjob to create a variable.
			ASSERT(sc->find_scope_variable(e->lhs->name));
			if (lhs_v) dst.reg=lhs_v->regname;
				//					lhs_v->regname=dst.reg;
				return CgValue(0,lhs_v->type(),dst.reg);
				}
		else if(opname==AS) {
			// if (prim to prim) {do fpext, etc} else..
			return emit_cast(cg, dst,lhs,e);
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
			auto dstreg=lhs;
			emit_instruction(cg,opname,t?t:rhs.type, dst,lhs,rhs);
			auto out=dstreg.store_from(cg,dst.reg);
			return dstreg;
		}else {
			// RISClike 3operand dst=op(src1,src2)
			emit_instruction(cg,opname,t,dst,lhs,rhs);
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
			auto dst=CgValue(n->get_reg(opname,&cg.next_reg,false), n->get_type());
			emit_instruction(cg,opname,t,dst,src);
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
		auto struct_val= emit_alloca_type(cg, e, e->type());
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
		auto dst=ar->get_reg(ARRAY,&cg.next_reg,false);
		if (!n->regname){n->regname=expr.reg;}
		
		expr.load(cg);
		index.load(cg);
		emit_ins_begin(cg,dst,"getelementptr inbounds");
		emit_type_reg(cg,array_type,true,expr.reg);//!expr.reg);
		emit_i32_lit(cg,0);
		emit_type_operand(cg,index);
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
		emit_comment(cg,"fncall %s", call_fn?str(call_fn->name):e->call_expr->name_str());

		auto ret_type=e->type();	// semantic analysis should have done this
		auto dst=(ret_type->name!=VOID)?n->get_reg_new(call_fn?call_fn->name:FN, &cg.next_reg):0;
		//auto rt=call_fn->get_return_value();

		auto emit_arg_list=[&](Type* rect,RegisterName receiver){
			emit_args_begin(cg);
			if(rect && receiver){
				emit_type(cg,rect);
				emit_reg(cg,receiver);
			}
			for (auto a: l_args){
				emit_type_operand(cg,a);
			}
			emit_args_end(cg);
		};

		//[3.3] make the call..
		if (e->call_expr->is_function_name()) {
			//[3.3.1] Direct Call
			emit_ins_begin(cg,dst,"call");
			emit_function_type(cg, call_fn);// NOTE we need this until we handle elipsis
			emit_global(cg,call_fn->get_mangled_name());
			emit_arg_list(0,0);
			emit_ins_end(cg);
			
		} else {
			//[3.3.2] Indirect Call... Function Pointer
			auto fn_obj = e->call_expr->compile(cg, sc);
			indirect_call=fn_obj.load(cg);
			if (fn_obj.type->is_closure()){
				//[.1] ..call Closure (function,environment*)
				indirect_call=emit_extractvalue(cg,next_reg(cg),fn_obj.type,indirect_call,0);
				emit_ins_begin(cg,dst,"call");
				emit_function_type(cg,e->call_expr->type());
				emit_reg(cg,indirect_call);
				emit_arg_list(0,0);
				emit_ins_end(cg);
			} else{
				//[.2] ..Raw Function Pointer
				emit_ins_begin(cg,dst,"call");
				emit_function_type(cg,e->call_expr->type());
				emit_reg(cg,indirect_call);
				emit_arg_list(0,0);
				emit_ins_end(cg);
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
void emit_function_signature(CodeGen& cg,ExprFnDef* fn_node, EmitFnMode mode){
	auto ofp=cg.ofp;
	auto scope=fn_node->scope;
	fn_node->clear_reg();
	auto rtype=fn_node->return_type();
	emit_nest_begin(cg, "");
	emit_txt(cg,mode==EmitDefinition?"define ":mode==EmitDeclaration?"declare ":" ");
	if (mode==EmitType) emit_pointer_begin(cg);
	emit_type(cg,rtype,false);
	if (mode!=EmitType)
		emit_global(cg,fn_node->get_mangled_name());
	emit_args_begin(cg);
	int inter=0;
	for (auto a:fn_node->args){
		emit_type(cg,a->type(),false);//was a->is_complex. confusion here over pass by ref/val. we think fn sigs should be 1:1. but raw struct type should  be pass by val? will we have to copy struct val?
		if (mode==EmitDefinition){
			auto var=scope->get_or_create_scope_variable(a,a->name, VkArg);
			var->get_reg(a->name, &cg.next_reg, false);
			emit_reg(cg,var->regname);
		}
	}

	if (fn_node->variadic) {
		emit_comma(cg);
		emit_txt(cg,"...");
	}
	emit_args_end(cg);
	if (mode==EmitType) emit_pointer_end(cg);
	else emit_txt(cg,"\n");
	emit_nest_end(cg,"");
}
void emit_function_type(CodeGen& cg, const Type* t) {
	auto ofp=cg.ofp;
	auto argtuple=t->sub;
	ASSERT(argtuple);
	auto retn=argtuple->next;
	emit_comma(cg);
	emit_pointer_begin(cg);
	emit_type(cg,retn,0);
	emit_args_begin(cg);
	for (auto arg=argtuple->sub; arg;arg=arg->next){
		emit_type(cg,arg);
	}
	emit_args_end(cg);
	emit_pointer_end(cg);
}
void emit_function_type(CodeGen& cg,ExprFnDef* fn_node){
	emit_function_signature(cg,fn_node,EmitType);
}

CgValue ExprFnDef::compile(CodeGen& cg,Scope* outer_scope){
	auto fn_node = this;
	auto ofp=cg.ofp;

	if (!fn_node){return CgValue();}
	if (fn_node->is_undefined()) {
		fprintf(ofp,";fn %s prot\n",getString(fn_node->name));
		emit_function_signature(cg,fn_node,EmitDeclaration);
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

	emit_nest_begin(cg,"");
	emit_fn_ptr(cg,fn_node->get_mangled_name());
	emit_ins_name(cg,"global");
	emit_function_type(cg, fn_node->type());
	emit_global(cg,fn_node->get_mangled_name());
	emit_nest_end(cg,"");
	if (!fn_node->get_type() && fn_node->fn_type && fn_node->scope ){
		error(fn_node,"function name %s %s %p %p %p %p", str(fn_node->name),str(fn_node->get_mangled_name()), fn_node->instance_of, fn_node->get_type(), fn_node->fn_type, fn_node->scope);
		ASSERT(0 && "function must be resolved to compile it");
		return CgValue();
	}
	emit_comment(cg,"fn %s (%p) :- ins=%p of %p ", str(fn_node->name),fn_node, fn_node->instances, fn_node->instance_of);

	auto scope=fn_node->scope;
	
	emit_function_signature(cg,fn_node,EmitDefinition);
 	fprintf(ofp,"{\n");
	if (fn_node->instance_of!=nullptr){
		emit_comment(cg,"compiling generic fn body");
	}
	emit_local_vars(cg, fn_node->body, fn_node, scope);
	auto rtn=fn_node->get_return_value();
	auto ret=fn_node->body->compile(cg,scope);
	if (ret.is_valid() && !ret.type->is_void()) {
		ret.load(cg);
		emit_ins_begin_name(cg,"ret");
		//emit_type(cg,ret);//rtn->get_type(),ret.is_addr());
		//ret.emit_operand(cg);
		emit_type_operand(cg,ret);
		emit_ins_end(cg);
	} else {
		emit_ins_begin_name(cg,"ret");
		emit_txt(cg,"void");
		emit_ins_end(cg);
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

	emit_comment(cg,"from scope %s\n;",scope->name());
	// output all inner items that outer stuff depends on..
	// literals first, because we setup llvm_strlen. TODO , more solid design pls.
	for (auto l=scope->literals; l; l=l->next_of_scope) {
		if (l->type_id==T_CONST_STRING){
		const char* name=getString(l->name);
			char buffer[512];
			l->llvm_strlen=translate_llvm_string_constant(buffer,512, l->as_str())+1;
			emit_global(cg, l->name);
			emit_txt(cg,"= private unnamed_addr constant [%d x i8] c\"%s\\00\"\n", l->llvm_strlen, buffer);
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




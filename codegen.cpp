#include "codegen.h"

// TODO: properly abstract llvm instruction generation to move to llvm api.
inline void dbprintf_mangle(const char*,...){}
#if DEBUG>=2
#define dbprintf_vcall dbprintf
#else
inline void dbprintf_vcall(const char*,...){}
#endif

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
#ifdef DEBUG2
	printf("%s",tmp);
#endif
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
	emit_txt("\t;%s\n",tmp);
}
void CodeGen::emit_prelude(){
	if (prelude_done)
		return;
	prelude_done=true;
	/// TODO - check if we can acess user-passed size versions easily, eg free(ptr,size), ..its superior
	emit_txt(";hard coded prelude\n");
	emit_txt("declare i8* @malloc(i32)\n");
	emit_txt("declare i8* @free(i8*)\n");
	emit_txt("declare i8* @realloc(i32,i8*,i32)\n");
	emit_txt(";\n");
}

struct CgValue {	// lazy-access abstraction for value-or-ref. So we can do a.m=v or v=a.m. One is a load, the other is a store. it may or may not load/store either side of the instruction. a 'variable' is included here as a form of 'adress', for var+= ...
	// TODO: this should be a tagged-union?
	// these values aren't persistent so it doesn't matter too much.
	RegisterName reg;
	int elem=-1;     // if its a struct-in-reg
	Type* type;
	RegisterName addr;
	Node*	val;		// which AST node it corresponds to
	int ofs;
	explicit CgValue(RegisterName n,Type* t):reg(n),type(t){elem=-1;addr=0;ofs=0;val=0;}
	explicit CgValue(RegisterName v,Type* t,RegisterName address_reg,int elem_index=-1):reg(v){elem=elem_index;reg=v;addr=address_reg; type=t;ofs=0;val=0;}
	explicit CgValue(Node* n) {
		// todo - unify with 'expr'
		addr=0; reg=0; ofs=0;elem=-1;
		val = n;
		if (auto fd=dynamic_cast<ExprFnDef*>(n)){ // variable is a function pointer?
			reg=0; // it needs to be loaded
		}
		if (auto v=dynamic_cast<Variable*>(n)){
			if (v->reg_is_addr){
				addr=v->reg_name;
			}
			else{
				reg=v->reg_name;
			};
		}
		this->type=n->type();
	}
	CgValue():reg(0),addr(0),ofs(0),val(0),type(nullptr){};
//	bool is_fn()const{return reg==0 && val==0 && type->name==FN && val!=0 && val->as_fn_def()!=0;}
	bool is_struct_elem()const{return elem>=0;}
	bool is_valid()const{if (val) if (val->type()->name==VOID) return false;return val!=0||reg!=0||addr!=0;}
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
		} else if (auto v=this->val->as_variable()){
			if (v->reg_is_addr){
				return CgValue(v->reg_name, new Type(0,PTR,v->type()));
			}
		}
		{
			ASSERT(0 && "tryting to take adress of register");
			return CgValue();
//			return this->to_stack(cg).addr_op(cg,t);
		}
	}
	CgValue deref_op(CodeGen& cg, Type* t) {
		ASSERT(this->type->name==PTR || this->type->name==REF);
		if (!t) { t=this->type->sub;}
		// todo type assertion 't' is the output type.
		if (!addr) {		// the value we were given is now the *adress* - we return a reference, not a pointer.
			return CgValue(0,t,reg);
		} else {
			// it its'  a reference type, we need to load that first.
			auto ret=load(cg);
			return CgValue(0,t,ret.reg); // ... and return it as another reference: eg given T&* p,  returning T& q=*p
		}
	}
	
	CgValue load(CodeGen& cg,Type* result_type=0) const{
		//if (!this->is_valid()) return CgValue();
		auto ofp=cg.ofp;
		if (elem>=0){
			// todo: why not 'addr' aswell?
			if (this->type->is_pointer() || (addr &&!reg)) {
				cg.emit_comment("dot reg=%s addr=%s index=%d",str(reg),str(addr),elem);
				auto sub=this->get_elem_index(cg,elem,result_type);
				return sub.load(cg);
			} else{
				// elem acess
				auto r=cg.next_reg();
				cg.emit_ins_begin(r,"extractelement");
				cg.emit_type_reg(this->type,false,reg);
				cg.emit_i32_lit(this->elem);
				cg.emit_ins_end();
				return CgValue(r,this->type->get_elem(elem));
			}
		}
		if(val) {
			auto outr=cg.next_reg();
			
			if (auto lit=dynamic_cast<ExprLiteral*>(val)){
				return cg.emit_literal(lit);
				// todo: string literal, vector literals, bit formats
			}
			else if (auto fp=dynamic_cast<ExprFnDef*>(val)){
				if (fp->is_closure()) {
					// Build a pair. we would also build the Capture Object here.
					// TODO: we're blocked from making a nice' emit_make_pair()' wrapper here
					// because: fp->fn_type is interpretted as {ptr,env}, but we must manually
					// tell it to build the first & second types.
					commit_capture_vars_to_stack(cg,fp->my_capture);
					auto r=cg.emit_ins_begin(cg.next_reg(),"insertvalue");
					cg.emit_type(fp->fn_type); cg.emit_undef();
					cg.emit_function_type(fp->fn_type);
					cg.emit_fn(fp->get_mangled_name());
					cg.emit_comma();
					cg.emit_txt("0");
					cg.emit_ins_end();
					if (fp->my_capture){
						auto closure_env=CgValue(fp->my_capture->reg_name,fp->my_capture->type());
						auto closure_env_ptr_i8=cg.emit_cast_raw(closure_env,cg.i8ptr());

						cg.emit_ins_begin(outr,"insertvalue");
						cg.emit_type_reg(fp->fn_type,false,r);
						cg.emit_type_operand(closure_env_ptr_i8);
						cg.emit_comma();
						cg.emit_txt("1");
						cg.emit_ins_end();
					} else {
						outr=r;
					}
					return CgValue(outr,fp->fn_type);
				} else {// raw function
					cg.emit_ins_begin(outr,"load");
					cg.emit_type(this->type,true);
					cg.emit_fn_ptr(fp->get_mangled_name());
					cg.emit_ins_end();
					return CgValue(outr,fp->fn_type);
				}
			}
			else if (auto v=dynamic_cast<Variable*>(val)){
				if (v->reg_is_addr){
					auto r= CgValue(0,v->type(),v->reg_name);
					return r.load(cg);
				} else
					return *this;	//NOP - remember its' lazy, this is a valid case for reg-reg
			}
		}
		if ((bool)addr) {
			ASSERT(reg==0);
			auto dstr=cg.next_reg();
			cg.emit_ins_begin(dstr,"load");
			cg.emit_type_reg(type, addr!=0,addr);// an extra pointer level if its' a reference
			cg.emit_ins_end();
			return CgValue(dstr,type);
		}
		ASSERT(reg && !addr);
		return *this;
	}
	void emit_literal(CodeGen& cg,const ExprLiteral* lit)const{
		if (cg.comma){cg.emit_txt(",");} cg.comma=true;
		auto ofp=cg.ofp;
		switch (lit->type_id) {
			case T_INT:
				cg.emit_txt(" %d ",lit->u.val_int);
				break;
			case T_UINT:
				cg.emit_txt(" %u ",lit->u.val_uint);
				break;
			case T_FLOAT:
				cg.emit_txt(" 0x%x00000000 ",lit->u.val_uint);
				break;
			case T_CONST_STRING:
				cg.emit_txt(" getelementptr inbounds([%d x i8]* @%s, i32 0, i32 0) ", lit->llvm_strlen, getString(lit->name));
				break;
			default:
				cg.emit_txt(" @TODO_LITERAL_FORMAT %s", getString(lit->name));
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
			cg.emit_txt(" <?CgV?> ");
			this->type->dump_if(0);
			ASSERT(this->type);
			ASSERT(0 && "missing register, value wasn't found in resolving");
		}
	}
	CgValue store(CodeGen& cg) const{// for read-modify-write
		ASSERT(type);
		if (!addr || !type)
			return *this;
		cg.emit_store(cg.next_reg(), type, addr);
		return *this;
	}
	CgValue store(CodeGen& cg,const CgValue& src) const{
		auto src_in_reg=src.load(cg);
		
		if (elem>=0){
			auto newreg=next_reg_name(&cg.m_next_reg);
			if ( reg && !addr){
				auto x=this->get_elem_index(cg, elem);
				return cg.emit_store(src_in_reg.reg,src.type, x.addr);
			}else if (addr && !reg){
				auto x=this->get_elem_index(cg, elem);
				return cg.emit_store(src_in_reg.reg,src.type, x.addr);

			}else {
				ASSERT(this->type->name!=PTR);
				cg.emit_ins_begin(newreg,"insertelement");
				cg.emit_type_reg(this->type,false,reg); cg.emit_comma();
				auto elem_t=this->type->get_elem(elem);
				cg.emit_type_reg(elem_t,false,src_in_reg.reg);
				cg.emit_i32_lit(this->elem);
				cg.emit_ins_end();
				return CgValue(newreg, this->type);
			}
		}

		
		if (addr)
			cg.emit_store(src_in_reg.reg, type, addr);
		else if (reg) {
			cg.emit_store(src_in_reg.reg, type, reg);
		} else if (val) {
			if (auto v=val->as_variable()) {
				if (v->reg_is_addr && v->reg_name) {
					cg.emit_store(src_in_reg.reg, type, v->reg_name);
					return CgValue(v);
				} else if (!v->reg_name){
					if (!v->on_stack) {
						v->reg_name=src_in_reg.reg;
					} else {
						v->reg_name=cg.emit_alloca_type(v, type).addr;
						v->reg_is_addr=true;
						cg.emit_store(src_in_reg.reg, type, v->reg_name);
					}
					return CgValue(v);
				}
			}
			val->dump(0);
			ASSERT(0 && "store case not handled, to var..");
		}
		return *this;
	}
	CgValue get_elem(CodeGen& cg,const Node* field_name,Scope* sc)const{	//calculates & returns adress
		ASSERT(type );
		auto sd=type->get_struct();
		if (!sd) {type->dump(-1);error(field_name,"struct not resolved\n");}
		int index=sd->field_index(field_name);
		auto field=sd->find_field(field_name);
		return get_elem_index(cg,index);
	}
	CgValue get_elem_index(CodeGen& cg, int field_index,Type *field_type=0) const{
		if ((bool)reg && !addr && !(this->type->is_pointer())){
			// lazy ref to inreg field index,
			// 'load'/'store' will do 'insert'/'extract'
			return CgValue(reg,type,0,field_index);
		}
		else {
			auto numptr=this->type->num_pointers()+(this->addr?1:0);
			ASSERT(numptr==1);
			return cg.emit_getelementref(*this,0,field_index);
		}
	}
	CgValue index(RegisterName index){ // calculates & returns adress
		return CgValue();
	}
};

CgValue CodeGen::emit_getelementref(const CgValue &src, Name n){
	auto i=src.type->deref_all()->struct_def->get_elem_index(n);
	return emit_getelementref(src, 0, i);
}

CgValue CodeGen::emit_getelementref(const CgValue& src, int i0, int field_index){

	auto numptr=src.type->num_pointers()+(src.addr?1:0);
	ASSERT(numptr==1);
	
	auto sd=src.type->deref_all()->struct_def;
	auto field_type=sd->get_elem_type(field_index);//fields[field_index]->type();
	auto areg=this->next_reg();
	this->emit_ins_begin(areg, "getelementptr inbounds  ");
	this->emit_type_operand(src);
	this->emit_i32_lit(0);
	
	this->emit_i32_lit(field_index);
	this->emit_ins_end();
	return CgValue(0,field_type,areg);
}
CgValue CodeGen::emit_assign(const CgValue& dst, const CgValue& src){
	return dst.store(*this, src.load(*this));
}
CgValue CodeGen::emit_literal(ExprLiteral *lit){
	auto outr=this->next_reg();
	auto ltn=lit->type()->name;
	if (ltn==INT){
		emit_ins_begin(outr,"or");
		emit_i32_lit(0);
		emit_comma();
		emit_txt("%d",lit->u.val_int);
	} else if(ltn==FLOAT){
		// todo, i guess we're goint to have t make a global constants table
		emit_ins_begin(outr,"fadd");
		emit_txt("float 0.0, ");
		emit_txt(" 0x%x00000000",lit->u.val_int);
	} else if (ltn==STR){
		emit_ins_begin(outr,"getelementptr inbounds");
		emit_comma();
		emit_txt("[%d x i8]* @%s",lit->llvm_strlen, getString(lit->name));
		
		emit_i32_lit(0);
		emit_i32_lit(0);
		ASSERT(lit->llvm_strlen);
	}
	else if (ltn==VOID){
		return CgValue();
	} else {
		lit->type()->dump_if(-1);
		error(lit,"literal type not handled yet");
		error_end(lit);
	}
	emit_ins_end();
	return CgValue(outr,lit->type());
}


CgValue CodeGen::emit_store(RegisterName reg, Type* type, RegisterName addr){
	emit_ins_begin_name("store");
	emit_type(type,0);
	emit_reg(reg);
	emit_type(type,1);
	emit_reg(addr);
	emit_txt(", align 4");
	emit_ins_end();
	return CgValue(0,type,addr);
}
CgValue CodeGen::emit_store_global(CgValue dst,Name globalvar){
	auto r=next_reg();
	ASSERT(dst.addr && "store requires a reference destination");
	emit_ins_begin_name("store");
	emit_type(dst.type,0);
	emit_global(globalvar);
	emit_type_operand(dst);
	emit_txt(", align 8");
	emit_ins_end();
	return dst;
}



CgValue Node::compile_if(CodeGen& cg, Scope* sc){
	if (this)
		return this->compile(cg,sc);
	else
		return CgValueVoid();
}



void commit_capture_vars_to_stack(CodeGen& cg, Capture* cp){
	if (!cp) return;
	return;
}
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
	ASSERT(dst);
	emit_txt("%%%s ",str(dst));
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
void CodeGen::emit_typename(Name tn, bool ref) { // should be extention-method.
	emit_comma(); // type always starts new operand
	if (ref) emit_pointer_begin();
	emit_reg(tn);
	if (ref) emit_pointer_end();
}
void CodeGen::emit_array_type(const Type* t, int count, bool ref) { // should be extention-method.
	emit_comma(); // type always starts new operand
	if (ref) emit_pointer_begin();
	emit_nest_begin("[");
	emit_txt("%d x ",count);
	emit_type(t);
	emit_nest_begin("]");
	if (ref) emit_pointer_end();
}
void CodeGen::emit_type(const Type* t, bool ref) { // should be extention-method.
	emit_comma(); // type always starts new operand
	if (!t) { emit_txt("<type_expected>");return;}
	if (ref) emit_pointer_begin();
	if (t->is_pointer()){
//		dbprintf("THIS IS SUSPECT, REF ISn'T NEEDED TWICE");
		emit_pointer_begin();
		emit_type(t->sub,false);
		emit_pointer_end();
	}else if (t->is_array()) {
		emit_nest_begin("[");
		emit_txt("%s x ",str(t->array_size())); //sub->next->name));
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
		auto sd=t->get_struct();
		if (!sd) {
			/// TODO we quieted this error because closure objs dont make a struct
			emit_reg(t->name);
		}else
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
		emit_separator(""); // .. because function_type also calls comma
		emit_function_type( t);
	}
	else {
		if (t->is_complex()) {
			emit_txt("%%%s", str(t->name));
		}
		else
			emit_txt("%s",t?get_llvm_type_str(t->name):"???");//,t.is_pointer?"*":"");
	}
	if (ref) emit_pointer_end();
}
Type* CodeGen::ptr_to(Type* other) {
	// TODO- RAII this!! -or make duplicates in a hash table.
	if (!m_ptr) { m_ptr=new Type(nullptr,PTR);}
	m_ptr->sub=other;
	return m_ptr;
}
void CodeGen::emit_free(CgValue ptr, Type* t,size_t count){
	/// TODO .. just memleak now..
}

CgValue CodeGen::emit_malloc(Type* t,size_t count){
	ASSERT(t->name==PTR &&"pass a pointer type in, it allocs *T.necasery to avoid allocating a ptr[T]");
	auto r1=next_reg();
	auto r2=next_reg();
	emit_ins_begin(r1,"call i8* @malloc");
	emit_args_begin();
	emit_i32_lit(t->sub->size()*count);
	emit_args_end();
	emit_ins_end();
	return emit_cast_reg(r1, i8ptr(), t);
}
CgValue CodeGen::emit_free(CgValue ptr,size_t count){
	ASSERT(ptr.type->is_pointer() &&"pass a pointer type in, it allocs *T.necasery to avoid allocating a ptr[T]");
	auto r=ptr.load(*this);
	auto r1=this->emit_cast_to_i8ptr(r);
	emit_ins_begin(0,"call i8* @free"); /// TODO: sized form of free.
	emit_args_begin();
	emit_type_operand(r1);
//	emit_i32_lit(t->sub->size()*count);
	emit_args_end();
	emit_ins_end();
	return CgValue();
}

CgValue CodeGen::emit_malloc_array(Type* t,CgValue count){
	auto rsizereg=next_reg();
	auto cr=count.load(*this);
	auto rsize=emit_instruction_reg_i32("mul", cr.type, 0, cr, t->sub->size());
	ASSERT(t->name==PTR &&"pass a pointer type in, it allocs *T.necasery to avoid allocating a ptr[T]");
	auto r1=next_reg();
	auto r2=next_reg();
	/// todo factor out call above.. cut-paste hell. its imm vs reg , we dont know how to put constant in reg:(
	emit_ins_begin(r1,"call i8* @malloc");
	emit_args_begin();
	emit_type_operand(rsize);
	emit_args_end();
	emit_ins_end();
	return emit_cast_reg(r1, i8ptr(), t);
}

void CodeGen::emit_type_reg(const Type* t,bool ref, Name reg){
	emit_type(t,ref);
	emit_reg(reg);
}
void CodeGen::emit_type_operand(const CgValue& src){
	/// TODO we wanted to unify 'void' here (!src.valid()) but contexts where its' called dont seem to work
	emit_type(src.type,(src.reg==0&&src.addr));
	src.emit_operand(*this);
}

void CodeGen::emit_instruction_sub(Name opname,Type* type,  RegisterName dstr,CgValue src1){
//	ASSERT(dst.is_reg());
	const LLVMOp* op=get_op_llvm(opname,type?type->name:VOID);
	this->emit_reg(dstr);
	emit_ins_name( op?op->op_signed:str(opname));
	if (is_comparison(opname))
		emit_type(src1);
	else{
		emit_type(type,false);
	}
	src1.emit_operand(*this);
}
CgValue CodeGen::emit_instruction(Name opname,Type* type,Name outname, CgValue src1){
	auto dstr=next_reg();
	auto r1=src1.load(*this);
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dstr,r1);
	emit_ins_end();
	return CgValue(dstr, type);
}
CgValue CodeGen::emit_instruction(Name opname,Type* type,Name outname,  CgValue src1,CgValue src2){
	ASSERT(type!=0);
	auto r1=src1.load(*this);
	auto r2=src2.load(*this);
	auto dstr=next_reg();
	auto dbg=[&](){
		dbprintf("src1.reg=%s/%s.%d\n",str(src1.reg),str(src1.addr),src1.elem);
		dbprintf("src1.reg=%s/%s.%d r=%s\n",str(src1.reg),str(src1.addr),src1.elem, str(r1.reg));
	};
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dstr,r1);
	emit_comma();
	r2.emit_operand(*this);
	emit_ins_end();
	return CgValue(dstr, type);
}

CgValue CodeGen::emit_instruction_reg_i32(Name opname,Type* type,  Name outname,CgValue src1,int imm_val){
	ASSERT(type!=0);
	auto r1=src1.load(*this);
	ASSERT(r1.type->is_int());
	auto dstr=next_reg();

	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dstr,src1);
	emit_comma();
	emit_txt("%d",imm_val);
	emit_ins_end();
	return CgValue(dstr,type);
}
RegisterName CodeGen::next_reg(Name name){
	char rname[256];
	const char* s=getString(name);
	sprintf(rname, "r%d%s",this->m_next_reg++,isSymbolStart(s[0])?s:"rfv");
	return getStringIndex(rname);
}

void dump_locals(Scope* s){
	for (;s;s=s->parent){
		for (auto v=s->vars; v;v=v->next_of_scope){
			printf("\t;%s:",str(v->name));v->get_type()->dump(-1); printf("%%%s\n",str(v->reg_name));
		}
	}
}
void compile_raw_vtable(CodeGen& cg, ExprStructDef* sd){
	// raw vtable looks more like what clang spits out, but we want static fields & more metadata
	cg.emit_txt("@%s = private unnamed_addr",str(sd->vtable_name));
	cg.emit_ins_begin_name("constant");
	cg.emit_array_type(cg.i8ptr(), sd->virtual_functions.size());
	/// todo: this is wrong, we should have vtable_index
	cg.emit_nest_begin("[");
	for (auto vf:sd->virtual_functions){
		cg.emit_type(cg.i8ptr());
		cg.emit_txt("bitcast");
		cg.emit_nest_begin("(");
		cg.emit_function_type(vf->fn_type);
		cg.emit_global(vf->name);
		cg.emit_separator(" to ");
		cg.emit_type(cg.i8ptr());
		cg.emit_nest_end(")");
	}
	cg.emit_nest_end("]");
	cg.emit_ins_end();
}

void CodeGen::emit_fn_cast_global(Name n,const Type* srct, const Type *dstt){
	auto&cg=*this;
	
//	cg.emit_comma();
	cg.emit_type(dstt);
	cg.emit_nest_begin("");
	cg.emit_txt("bitcast");
	cg.emit_nest_begin("(");
	cg.emit_function_type(srct);
	cg.emit_global(n);
	cg.emit_separator(" to ");
	cg.emit_function_type(dstt);
	cg.emit_nest_end(")");
	cg.emit_nest_end("");
	
}

void compile_vtable_data(CodeGen& cg, ExprStructDef* sd, Scope* sc,ExprStructDef* vtable_layout){
	// compile formatted vtable with additional data..
	if (!vtable_layout->is_compiled){
		// the vtable really is just a struct; eventually a macro system could generate
		vtable_layout->compile(cg,sc);
		vtable_layout->is_compiled=true;
	}
	cg.emit_txt("@%s = ",str(sd->vtable_name));
	cg.emit_ins_begin_name("global");
	cg.emit_typename(str(vtable_layout->mangled_name));
	cg.emit_struct_begin();

	for (auto a:vtable_layout->fields){
//		cg.emit_type(a->type(),false);
//		cg.emit_comma();
		cg.emit_txt(" ");
		auto* s=sd;
		for (;s;s=s->inherits){
			if (auto f=s->find_function_for_vtable(a->name, a->type())){
				cg.emit_fn_cast_global(f->get_mangled_name(),f->fn_type,a->type());
				break;
			}
		}
		if (!s){
			cg.emit_undef();
		}
	}
	
	cg.emit_struct_end();
	cg.emit_txt(", align 16");
	cg.emit_ins_end();
	
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

		// instantiate the vtable
		// todo: step back thru the hrc to find ov[i]errides
		if (this->vtable)
			compile_vtable_data(cg, this,sc, this->vtable);

		cg.emit_struct_name(st->get_mangled_name());
		cg.emit_ins_name("type");
		cg.emit_struct_begin();
		for (auto fi: st->fields){
			cg.emit_type(fi->type(), false);
		};
		cg.emit_struct_end();
		cg.emit_ins_end();
	}
	return CgValue();	// todo: could return symbol? or its' constructor-function?
}

CgValue CodeGen::emit_alloca_type(Expr* holder, Type* t) {
	RegisterName r= holder?holder->get_reg(*this, false):next_reg();
	emit_ins_begin(r,"alloca"); emit_type(t,false);
	emit_txt(", align %zu", t->alignment());
	emit_ins_end();
	return CgValue(0,t, r);
}

void emit_local_vars(CodeGen& cg, Expr* n, ExprFnDef* fn, Scope* sc) {
	auto ofp=cg.ofp;
	for (auto cp=fn->captures; cp;cp=cp->next_of_from){
		cp->reg_name=next_reg_name(cp->tyname(), &cg.m_next_reg);
		cg.emit_alloca_type(cp, cp->type()->deref_all());
	}
	for (auto v=sc->vars; v;v=v->next_of_scope){
		if (!v->type()) {
			cg.emit_comment("warning var %s has no type, something is wrong\n",v->name_str());
			continue;
		}
		cg.emit_comment("local %s:%t..",v->name_str(),v->type()->name_str());
		
		if (v->kind!=Local) continue;
		auto vt=v->expect_type();
		if (v->capture_in)
			continue; // no local emited if its in the capture
		auto r= v->get_reg(cg, true);
		if (vt->is_struct() || v->keep_on_stack()) {
			cg.emit_alloca_type(v, vt);
			v->reg_is_addr=true;
		} else if (vt->is_array()){
			auto t=vt->sub;
			if (!t || !t->next){error(v,"array type needs 2 args");}
			cg.emit_txt("\t"); cg.emit_reg(r); cg.emit_txt(" = alloca [%s x %s] , align %zu\n",str(t->next->name),get_llvm_type_str(t->name),vt->alignment());
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

void CodeGen::emit_label(Name l){
	emit_txt("%s:\n",str(l));
}
void CodeGen::emit_branch( Name l){
	emit_ins_begin_name("br");
	emit_txt("label %%%s",str(l));
	emit_ins_end();
}
void CodeGen::emit_branch(CgValue cond, Name label_then, Name label_else){
	cond.load(*this);
	emit_ins_begin_name("br");
	emit_txt("i1 %%%s, label %%%s, label %%%s",str(cond.reg), str(label_then), str(label_else));
	emit_ins_end();
}
CgValue CodeGen::emit_break(  CgValue v){
	
	auto i=this->flow_depth-1;
	ASSERT(i>=0);
	this->flow_result[i].store(*this, v);
	emit_branch(this->flow_break_to[i]);
	return this->flow_result[i];
}
CgValue CodeGen::emit_continue(){
	auto i=this->flow_depth-1;
	emit_branch(this->flow_continue_to[i]);
	return this->flow_result[i];
}



Name CodeGen::gen_label(const char* label,int index){
	auto i=index?index:this->m_next_reg++;
	char tmp[256];sprintf(tmp,"%s%d",label,i);
	return getStringIndex(tmp);
}

CgValue ExprIf::compile(CodeGen& cg,Scope*sc){
	// todo - while etc can desugar as for(;cond;)body, for(){ body if(cond)break}
	auto curr_fn=cg.curr_fn;
	auto ifn=this;
	// TODO: Collect phi-nodes for anything modified inside.
	RegisterName outname=cg.next_reg();
	auto condition=ifn->cond->compile(cg,sc);
	int index=cg.m_next_reg++;
	auto label_if=cg.gen_label("if",index);
	auto label_endif=cg.gen_label("endif",index);
	if (ifn->else_block){
		auto label_else=cg.gen_label("else",index);
		cg.emit_branch(condition,label_if,label_else);
		cg.emit_label(label_if);
		auto if_result=ifn->body->compile(cg,sc);
		if (if_result.is_valid())if_result=if_result.load(cg,0);
		cg.emit_branch(label_endif);
		cg.emit_label(label_else);
		auto else_result=ifn->else_block->compile(cg,sc);
		if (else_result.is_valid())else_result=else_result.load(cg,0);
		cg.emit_branch(label_endif);
		cg.emit_label(label_endif);
		// phi node picks result, conditional assignment
		if (if_result.is_valid() && else_result.is_valid()){
			cg.emit_ins_begin(outname,"phi");
			cg.emit_type(if_result);
			cg.emit_separator("");
			cg.emit_phi_reg_label(if_result.reg,label_if);
			cg.emit_phi_reg_label(else_result.reg,label_else);
			cg.emit_ins_end();
		}
		auto return_type=ifn->get_type();
		return CgValue(outname,return_type);
	}
	else {
		/// TODO: ensure  if ... else if ... typechecks ok. 
		cg.emit_branch(condition,label_if,label_endif);
		cg.emit_label(label_if);
		auto ifblock=ifn->body->compile(cg,sc);
		if (ifblock.is_valid()) ifblock=ifblock.load(cg);
		cg.emit_branch(label_endif);
		cg.emit_label(label_endif);
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
		if (v.var->on_stack) continue;
		v.reg_end=v.var->reg_name;
		cg.emit_ins_begin(v.reg_start,"phi");
		cg.emit_type(v.var->type(), false);//v.val.is_addr());
		cg.emit_separator("");
		cg.emit_phi_reg_label(v.reg_pre,l_pre);
		cg.emit_phi_reg_label(v.reg_end,l_end);
		cg.emit_ins_end();
	}
	if (extra) for (auto i=phi_vars.size(); i>0;i--)cg.emit_txt("       ");	// dirty hack to prevent source overun
	cg.emit_txt("\n");
}

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
CgValue 	CodeGen::flow_result[32]; // hack till we move stupid header

CgValue emit_for_llvm(CodeGen& cg, ExprFor* e_for, Expr* e_init,Expr* e_cond, Expr* e_incr, Expr* e_body, Expr* e_else_block){
	int index=cg.m_next_reg++;
	auto l_for=cg.gen_label("cond",index);
	auto l_body=cg.gen_label("body",index);
	auto l_else=cg.gen_label("else",index);
	auto l_endfor=cg.gen_label("endfor",index);

	auto i=	cg.flow_depth++;
	ASSERT(i<32);

	cg.flow_break_to[i]		=l_endfor;
	cg.flow_continue_to[i]	=l_for;
	auto result_ref=cg.flow_result[i]		=cg.emit_alloca_type(e_for, e_for->type());
	
	auto nf=e_for;
	auto curr_fn=cg.curr_fn;
	auto sc=nf->scope;
	// write the initializer block first; it sets up variables initial state

	auto retval=CgValue();
	auto l_init=cg.gen_label("init",index);
	cg.emit_branch(l_init);
	cg.emit_label(l_init);
	auto init=nf->init->compile(cg,sc);
	
	set<Variable*> emit_vars;
	set<Variable*> else_vars;
	// Now scan all the blocks to see which are changed.
	e_cond->find_vars_written_if(sc,emit_vars);
	e_body->find_vars_written_if(sc,emit_vars);
	e_incr->find_vars_written_if(sc,emit_vars);
	vector<LoopPhiVar> phi_vars;
	for (auto v :emit_vars){
		if (v->on_stack) continue; // no need for phi-nodes for stack vars.
		LoopPhiVar phi;
//		phi.val=
		phi.var=v;
		phi.reg_pre=v->reg_name;
		phi.reg_start=v->reg_name=next_reg_name(v->name,&cg.m_next_reg);
		phi.reg_end=0;//gen_label(str(v->name),next_index);
		//todo: can we allocate regnames in the AST? find last-write?
		phi_vars.push_back(phi);
	}
	
	cg.emit_branch(l_for);
	cg.emit_label(l_for);
	auto phipos=cg.get_pos();
	emit_phi(cg,sc,phi_vars,l_init,l_for,true);//alloc space

	auto cond_result=e_cond->compile_if(cg,sc);
	cg.emit_branch(cond_result, l_body, l_else);
	cg.emit_label(l_body);
	e_body->compile_if(cg,sc);
	e_incr->compile_if(cg,sc);
	cg.emit_branch(l_for);
	cg.emit_label(l_else);
	retval=e_else_block->compile_if(cg,sc);
	result_ref.store(cg,retval);
	cg.emit_branch(l_endfor);
	cg.emit_label(l_endfor);
	// now write the phi-nodes.
	
	cg.set_pos(phipos);
	emit_phi(cg,sc,phi_vars,l_init,l_body,false);
	cg.set_pos_end();

	cg.flow_depth--;
	//TODO: return value.
	return result_ref;
}

CgValue ExprFor::compile(CodeGen& cg, Scope* outer_sc){
	return cg.emit_for(this, this->init,this->cond, this->incr, this->body, this->else_block);
}
CgValue CodeGen::emit_for(ExprFor* e_for, Expr* e_init,Expr* e_cond, Expr* e_incr, Expr* e_body, Expr* e_else_block){
	// this is the kind of stupid bouncing we want to write a new language to avoid
	// not sure how far we want to abstract this.. codegen llvm/C or not. using ExprFor as
	// universal 'back end' loop is quite flexible
	return emit_for_llvm(*this,e_for, e_init, e_cond,e_incr,e_body,e_else_block);
}

void CodeGen::emit_return(CgValue ret){
	if (ret.is_valid() && !ret.type->is_void()) {
		auto r=ret.load(*this);
		emit_ins_begin_name("ret");
		//emit_type(cg,ret);//rtn->get_type(),ret.is_addr());
		//ret.emit_operand(cg);
		emit_type_operand(r);
		emit_ins_end();
	} else {
		emit_ins_begin_name("ret");
		emit_txt("void");
		emit_ins_end();
	}
}

CgValue CodeGen::emit_cast_to_i8ptr(CgValue& val) {
	return emit_cast_reg(val.reg, val.type, i8ptr());
}
CgValue CodeGen::emit_cast_from_i8ptr(CgValue& val, Type* totype) {
	return emit_cast_reg(val.reg, i8ptr(), totype);
}


CgValue CodeGen::emit_cast_raw(CgValue&src_val, Type* to_type){
	return emit_cast_sub(src_val,to_type);
}
CgValue CodeGen::emit_cast(CgValue&src_val, Expr* rhs_type_expr){
	return emit_cast_sub(src_val,rhs_type_expr->type());
}
CgValue CodeGen::emit_cast_sub(CgValue&lhs_val, Type* rhst){
	auto lhs_reg=lhs_val.load(*this);
//	auto lhst=lhs_val.type;
	return emit_cast_reg(lhs_reg.reg, lhs_val.type, rhst);
}
CgValue CodeGen::emit_cast_reg(RegisterName srcr, Type* lhst, Type* rhst)
{
	auto dstr=next_reg();
	const char* ins="nop";
	if (rhst->is_pointer() || rhst->is_pointer()){
		ins="bitcast";
	}else
	if (lhst->is_int() && rhst->is_float()){
		ins=rhst->is_signed()?"sitofp":"uitofp";
	}
	else if (lhst->is_float() && rhst->is_int()){
		ins=lhst->is_signed()?"fptosi":"fptoui";
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
	emit_ins_begin(dstr, ins);
	emit_type_reg(lhst,0,srcr);
	emit_separator(" to ");
	emit_type(rhst,0);
	emit_ins_end();
	return CgValue(dstr,rhst);
}
Type* CodeGen::i8ptr(){
	if (! this->m_i8ptr){
		this->m_i8ptr=new Type(nullptr,PTR,I8);
	}
	return this->m_i8ptr;
}

CgValue compile_function_call(CodeGen& cg, Scope* sc,CgValue recvp, Expr* receiver, ExprBlock* e){
	// [3.1]evaluate arguments
	vector<CgValue> l_args;
	
	// process function argumetns & load
	if (receiver){
		auto recr=receiver->compile(cg,sc);
		l_args.push_back(recr.load(cg,recr.type));
	}
	for (auto arg:e->argls){
		auto reg=arg->compile(cg,sc);
		if (!reg.type) {
			error_begin(arg,"arg type not resolved in call\n");
			dbprintf("arg type=");arg->dump(-1);newline(0);
			auto reg=arg->compile(cg,sc);
			error_end(arg);
			ASSERT(reg.type);
		}
		auto r=reg.load(cg,arg->type());
		l_args.push_back(r);
	}
	
	//[3.2] evaluate call object..
	auto call_fn=e->get_fn_call();
	RegisterName indirect_call=0;
	cg.emit_comment("fncall %s", call_fn?str(call_fn->name):e->call_expr->name_str());
	
	auto ret_type=e->type();
	auto dst=(ret_type->name!=VOID)?e->get_reg_new(cg):0;
	
	auto l_emit_arg_list=[&](Type* envt,RegisterName envr){
		cg.emit_args_begin();
		if(envt && envr){
			cg.emit_type(envt);
			cg.emit_reg(envr);
		}
		for (auto a: l_args){
			cg.emit_type_operand(a);
		}
		cg.emit_args_end();
	};
	
	//[3.3] make the call..
	if (e->call_expr->is_function_name()) {
		auto fn_name=e->call_expr->name;
		ExprStructDef* vts=nullptr;
		ArgDef* vtable_fn=nullptr;
		CgValue vtable;
		if (receiver) {
			// lookup types to see if we have a vcall.
			dbprintf_vcall("receiver: %s %s .%s\n",str(receiver->name),str(receiver->type()->name), str(e->call_expr->name));
			ASSERT(recvp.type->is_pointer() && "haven't got auto-ref yet for receiver in a.foo(b) style call\n");
			receiver->dump(-1); newline(0);
			auto vtf=recvp.type->get_struct()->try_find_field(getStringIndex("__vtable_ptr"));
			dbprintf_vcall("vtbl=%p\n",vtf);
			if (vtf) {
				vts=vtf->type()->get_struct();
				dbprintf_vcall("vtable struct=%p\n",vts);
			}
		}
		if (vts) {
			dbprintf_vcall("looks like a vcall %s\n",vts, str(vts->name));
			vtable_fn=vts->try_find_field(fn_name);
		}
		if (vtable_fn) {
			// we have a vcall, so now emit it..
			// load the vtable
			auto recvv=recvp.load(cg);
			auto vtblref=cg.emit_getelementref(recvv, getStringIndex("__vtable_ptr"));
			auto vtbl=vtblref.load(cg);
			//load the fnptr
			auto fn_ptr=cg.emit_getelementref(vtbl,fn_name).load(cg);
			cg.emit_ins_begin(dst,"call");
			cg.emit_type_operand(fn_ptr);
			l_emit_arg_list(0,0);
			cg.emit_ins_end();

		} else {
			//[3.3.1] Direct Call
			cg.emit_ins_begin(dst,"call");
			cg.emit_function_type(call_fn);/// TODO NOTE we need this until we handle elipsis
			cg.emit_global(call_fn->get_mangled_name());
			l_emit_arg_list(0,0);
			cg.emit_ins_end();
		}
	} else {
		//[3.3.2] Indirect Call... Function Object
		auto fn_obj = e->call_expr->compile(cg, sc);
		indirect_call=fn_obj.load(cg).reg;
		if (fn_obj.type->is_closure()){
			//[.1] ..call Closure (function,environment*)
			auto indirect_call_f=cg.emit_extractvalue(cg.next_reg(),fn_obj.type,indirect_call,0);
			auto envptr=cg.emit_extractvalue(cg.next_reg(),fn_obj.type,indirect_call,1);
			cg.emit_ins_begin(dst,"call");
			cg.emit_function_type(e->call_expr->type());
			cg.emit_reg(indirect_call_f);
			l_emit_arg_list(cg.i8ptr(),envptr);
			cg.emit_ins_end();
		}else{
			//[.2] ..Raw Function Pointer
			cg.emit_ins_begin(dst,"call");
			cg.emit_function_type(e->call_expr->type());
			cg.emit_reg(indirect_call);
			l_emit_arg_list(0,0);
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

CgValue	CgValueVoid(){
	return CgValue();
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
		if (rhs->as_ident()) {
			auto lhsv=e->lhs->compile(cg,sc);
			// auto-deref is part of language semantics, done here..
			while (lhsv.type->num_pointers()+(lhsv.addr?1:0) > 1){
				cg.emit_comment("dot: auto deref from level=%d",lhsv.type->num_pointers()+(lhsv.addr?1:0));
				lhsv = lhsv.deref_op(cg,0);
			}
			return lhsv.get_elem(cg,e->rhs,sc);
		}
		else{
			// compile method call
			return compile_function_call(cg,sc,e->lhs->compile(cg,sc),e->lhs,e->rhs->as_block());
		}
	}
	else if (e->lhs && e->rhs){
		auto lhs=e->lhs->compile(cg,sc);
		auto rhs=e->rhs->compile(cg,sc);
		auto lhs_v=sc->find_variable_rec(e->lhs->name);
		auto outname=lhs_v?lhs_v->name:opname;
		
		//auto dst=CgValue(n->get_reg(&cg.m_next_reg,false),n->get_type());
		
		if (opname==ASSIGN_COLON){ // do nothing-it was sema'sjob to create a variable.
			ASSERT(sc->find_scope_variable(e->lhs->name));
			return lhs_v->on_stack?CgValue(0,lhs_v->type(),lhs_v->reg_name):CgValue(lhs_v->reg_name,lhs_v->type(),0);
		}
		else if(opname==AS) {
			// if (prim to prim) {do fpext, etc} else..
			return cg.emit_cast(lhs,e);
		}
		else
		if (opname==LET_ASSIGN){// Let-Assign *must* create a new variable.
			return lhs.store(cg,rhs);
		}
		else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_RHS)  && opname==ASSIGN){
			return lhs.store(cg,rhs);
		}
		else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_LHS|READ_RHS) ){
			auto result=cg.emit_instruction(opname,t?t:rhs.type, 0,lhs,rhs);
			return lhs.store(cg,result);
		}else {
			// RISClike 3operand dst=op(src1,src2)
			auto r=cg.emit_instruction(opname,e->get_type(),0,lhs,rhs);
			return r;
		}
	} else if (!e->lhs && e->rhs){ // prefix unary operators
		if (opname==ADDR){
			auto src=e->rhs->compile(cg,sc);
			if (!src.type || !n->type()) {
				n->dump(-1);
				error(n,"something wrong\n");
			}
			return src.addr_op(cg,n->type());
		}
		else if (opname==DEREF){
			auto src=e->rhs->compile(cg,sc);
			return src.deref_op(cg,n->type());
		}
		else if (opname==NEW){
			if (auto b=rhs->as_block()){
				if (b->is_struct_initializer()){
					auto reg=cg.emit_malloc(this->type(),1);
					auto st=b->call_expr->type()->get_struct();
					if (st->vtable){
						auto vtref=cg.emit_getelementref(reg, getStringIndex("__vtable_ptr"));
						cg.emit_store_global(vtref, st->vtable_name );
					}
					return b->compile_sub(cg,sc,reg.reg);
				} else if (b->is_subscript()){ // new Foo[5] makes 5 foos; [5,6,7] is like new int[3],(fill..)
					if (b->argls.size()==1){
						auto num=b->argls[0]->compile(cg,sc);
						return cg.emit_malloc_array(this->type(),num);
					}
					else{
						// empty dynamic array ctr
					}
				}
			}
			error(e,"TODO:new only works for  new StructName{....} \n");
			return CgValue();
		}
		else if (opname==DELETE){
			auto x=rhs->compile(cg,sc);
			cg.emit_free(x,1);	///TODO array types, rustlike DST??
			
			/// TODO call destructor here.
			return CgValue();
		}
		else if (opname==BREAK){
			cg.emit_comment("BREAK EXPRESSION");
			cg.emit_break(rhs->compile(cg,sc));
			return CgValue();
		}
		else {
			auto src=e->rhs->compile(cg,sc);
			if (opflags & (WRITE_LHS|WRITE_RHS)){static int once;if (once++){dbprintf(";TODO: logic for modifications to memory");}}
			// todo: handle read/modify-writeness.
			// postincrement/preincrement etc go here..
//			auto r=cg.emit_instruction(opname,t,0,src);
//			return src.store(cg,r);
			cg.emit_comment("TODO - ++ --");
			return CgValue();
		}
	} else if (e->lhs && !e->rhs) {
		error(e,"postfix operators not implemented yet.");
		return CgValue();
	} else
		return CgValue();
}
CgValue ExprBlock::compile(CodeGen& cg,Scope *sc) {
	return compile_sub(cg,sc,0);
}

CgValue ExprBlock::compile_sub(CodeGen& cg,Scope *sc, RegisterName force_dst) {
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
		auto dbg=[&](){e->type()->dump(0);newline(0);};
		auto struct_val= force_dst?CgValue(0,e->type(),force_dst):cg.emit_alloca_type(e, e->type());
		e->reg_name=struct_val.reg; // YUK todo - reallyw wanted reg copy
		// can we trust llvm to cache the small cases in reg..
		if (e->argls.size()!=si.value.size())
			dbprintf("warning StructInitializer vs argls mismatch, %d,%d\n",e->argls.size(),si.value.size());
		for (int i=0; i<e->argls.size() && i<si.value.size();i++) {
			auto rvalue=si.value[i]->compile(cg,sc);
			auto dst = struct_val.get_elem(cg,si.field_refs[i],sc);
			auto srcreg = rvalue.load(cg);
			auto r=dst.store(cg,srcreg);
			if (r.type==struct_val.type)
				struct_val=r; // mutate by insertion
		}
		if (force_dst) { struct_val.reg=force_dst; struct_val.addr=0;}
		return struct_val;
	}
	// [2] Operator
	//[3] ARRAY ACCESSs
	else if (auto ar=n->is_subscript()){
		auto expr=ar->call_expr->compile(cg,sc);// expression[index]
		auto index=ar->argls[0]->compile(cg,sc);
		auto array_type=expr.type;
		auto inner_type=array_type->sub;
		auto dstreg=cg.next_reg();//ar->get_reg(&cg.m_next_reg,false);
		if (!n->reg_name){n->reg_name=expr.reg;}

		/// TODO , this is actually supposed to distinguish array[ n x T ] from pointer *T case
		if (expr.type->num_pointers()+(expr.addr?1:0) > 1){
			expr=expr.load(cg);
		}
		auto index_reg=index.load(cg);
		cg.emit_ins_begin(dstreg,"getelementptr inbounds");
		cg.emit_type_reg(array_type,expr.addr!=0,expr.addr?expr.addr:expr.reg);//!expr.reg);
		if (array_type->deref_all()->name==ARRAY){
			cg.emit_i32_lit(0);
		}
		cg.emit_type_operand(index_reg);
		cg.emit_ins_end();
		return CgValue(0,n->type(),dstreg);
	}
	//[3] FUNCTION CALL (no receiver)
	else if (e->is_function_call()){
		return compile_function_call(cg,sc,CgValue(),nullptr,e);
	}
	return CgValue();
}
CgValue	ExprIdent::compile(CodeGen& cg, Scope* sc){
	auto n=this;
	// Its' either a local, part of 'this', or in a capture...
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
	else if (auto cp=var->capture_in){
		return CgValue(cp->reg_name,cp->type(),0,var->capture_index);
	}
	if (var && var!=n->def){
		error(n,"var/def out of sync %s %s\n",n->name_str(),var->name_str());
		return CgValue();
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
	// add environment ptr as first param if its a closure
	if (fn_node->fn_type->name==CLOSURE){
		cg.emit_type(cg.i8ptr());
		if (mode==EmitDefinition){
			cg.emit_reg(getStringIndex("__env_i8ptr"));
		}
	}
	for (auto a:fn_node->args){
		cg.emit_type(a->type(),false);//was a->is_complex. confusion here over pass by ref/val. we think fn sigs should be 1:1. but raw struct type should  be pass by val? will we have to copy struct val?
		if (mode==EmitDefinition){
			auto var=scope->get_or_create_scope_variable(a,a->name, VkArg);
			var->get_reg(cg, false);
			cg.emit_reg(var->reg_name);
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
	if (t->name==CLOSURE){
		cg.emit_type(cg.i8ptr());
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
void compile_capture(Capture* cp, CodeGen& cg){
	cg.emit_ins_begin(cp->tyname(), "type");
	cg.emit_struct_begin();
	decltype(cp->vars->capture_index) i=0;
	for (auto v=cp->vars;v;v=v->next_of_capture,i++){
		cg.emit_type(v->type());
		v->capture_index=i;
	}
	cg.emit_struct_end();
	cg.emit_ins_end();
	cp->type() = new Type(cp->capture_by, PTR,cp->tyname());
	cp->type()->sub->struct_def = (ExprStructDef*) cp;
}
CgValue ExprFnDef::compile(CodeGen& cg,Scope* outer_scope){
	auto fn_node = this;
	auto ofp=cg.ofp;

	if (!fn_node){return CgValue();}
	if (fn_node->is_undefined()) {
		cg.emit_comment("fn %s prot",getString(fn_node->name));
		cg.emit_function_signature(fn_node,EmitDeclaration);
		return CgValue();
	}
	if (fn_node->is_generic()) {
		cg.emit_comment("fn %s generic:-",getString(fn_node->get_mangled_name()));
		for (auto f=fn_node->instances; f;f=f->next_instance){
			cg.emit_comment("fn %s generic instance",getString(fn_node->get_mangled_name()));
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
	for (auto cp=this->captures; cp;cp=cp->next_of_from){
		compile_capture(cp, cg);
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
 	cg.emit_txt("{\n");
	if (fn_node->instance_of!=nullptr){
		cg.emit_comment("compiling generic fn body");
	}
	emit_local_vars(cg, fn_node->body, fn_node, scope);
	auto rtn=fn_node->get_return_value();

	if (fn_node->fn_type->name==CLOSURE){
		auto cp=fn_node->my_capture;
		if (cp){
			cp->reg_name=cp->get_reg_new(cg);
			cp->reg_name =cg.emit_cast_reg(getStringIndex("__env_i8ptr"), cg.i8ptr(),cp->type()).reg;
		}
	}

	cg.emit_return(fn_node->body->compile(cg,scope));
	cg.emit_txt("}\n");
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
	else if (auto sd=t->get_struct()){
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

void output_code(FILE* ofp, Scope* scope, int depth) {
	verify_all();
	auto cg=CodeGen(ofp,0);
	if (!depth)
		cg.emit_prelude();

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
		output_code(cg.ofp,sub,depth+1);
	}
}




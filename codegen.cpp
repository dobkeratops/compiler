#include "codegen.h"
/// details of compiling LLVM
/// CodeGen is planned to be an interface, slot in 'CodeGenLLVM' / 'CodeGenC'
size_t g_DefaultAlignment=4;

typedef LLVMOp LLVMOp2[2];
LLVMOp2 g_llvm_ops[]= {
	{{-1,"add","add"},{-1,"fadd","fadd"}},
	{{-1,"sub","sub"},{-1,"fsub","fsub"}},
	{{-1,"mul","mul"},{-1,"fmul","fmul"}},
	{{-1,"div","div"},{-1,"fdiv","fdiv"}},
	{{-1,"and","and"},{-1,"fand","fand"}},
	{{-1,"or","or"},{-1,"fadd",""}},
	{{-1,"xor","xor"},{-1,"fadd",""}},
	{{-1,"srem","rem"},{-1,"fadd",""}},
	{{-1,"shl","shl"},{-1,"fadd",""}},
	{{-1,"ashr","shr"},{-1,"fadd",""}},
};
LLVMOp2 g_llvm_logic_ops[]= {
	{{-1,"and",""},{-1,"fadd",""}},
	{{-1,"or",""},{-1,"fadd",""}},
};
LLVMOp2 g_llvm_cmp_ops[]= {
	{{-1,"icmp slt","icmp ult"},{-1,"fcmp ult","fcmp ult"}},
	{{-1,"icmp sgt","icmp ult"},{-1,"fcmp ugt","fcmp ugt"}},
	{{-1,"icmp sle","icmp ult"},{-1,"fcmp ule","fcmp ule"}},
	{{-1,"icmp sge","icmp ult"},{-1,"fcmp uge","fcmp uge"}},
	{{-1,"icmp eq","icmp eq"},{-1,"fcmp ueq","fcmp ueq"}},
	{{-1,"icmp ne","icmp ne"},{-1,"fcmp une","fcmp une"}},
};
//const char* g_llvm_type[]={
//	"i32","i32","i1","float","i8","i8*"
//};
const char* g_llvm_type_str[]={
	"i32","u32","i64",
	"i8","i16","i32","i64","i8","i16","i32","i64","i128","i1","i1", //bool BOOL REG_BOOL
	"half","float","double","< 4 x float >", "i8", "i8*","void","void*",
	nullptr
};
const char* get_llvm_type_str(Name n_type_name){
	auto tname=index(n_type_name);
	if (tname>=INT && tname<=(VOIDPTR)){
		return g_llvm_type_str[tname-INT];
	}
	return getString(tname);
}

const LLVMOp* get_op_llvm(Name ntok,Name ntype){
	auto tok=index(ntok); auto type=index(ntype);
	int ti=(type==FLOAT||type==DOUBLE||type==FLOAT)?1:0;
	if (tok>=ADD && tok<=SHR)
		return &g_llvm_ops[tok-ADD][ti];
	if (tok>=ADD_ASSIGN && tok<=SHR_ASSIGN)
		return&g_llvm_ops[tok-ADD_ASSIGN][ti];
	if (tok>=LOG_AND && tok<=LOG_OR)
		return &g_llvm_logic_ops[tok-LOG_AND][ti];
	if (tok>=LT && tok<=NE)
		return &g_llvm_cmp_ops[tok-LT][ti];
	return 0;
}
bool CgValue::is_literal() const{
	return val->as_literal()!=0;
}

bool CgValue::is_valid()const{
	if (val) if (val->type()->name==VOID)
		return false;
	return val!=0||reg!=0||addr!=0;
}

CgValue::CgValue(Node* n) {
	// todo - unify with 'expr'
	addr=0; reg=0; ofs=0;elem=-1;
	rvalue=false;
	val = n;
	if (auto fd=n->as_fn_def()){ // variable is a function pointer?
		reg=0; // it needs to be loaded
	}
	if (auto v=n->as_variable()){
		if (v->reg_is_addr){
			addr=v->reg_name;
		}
		else{
			reg=v->reg_name;
		};
	}
	this->type=n->type();
}
CgValue CgValue::addr_op(CodeGen& cg,const Type* t)const { // take type calculated by sema
	ASSERT(this->type);
	if (!reg && (bool)addr) {	// we were given a *reference*, we make the vlaue the adress
		ASSERT(t->name==PTR);
		if (!t->sub->is_equal(this->type)){
			dbprintf("type error taking adress:-\n");
			t->dump(0);
			dbprintf("vs-\n");
			this->type->dump(0);
			dbprintf("type error\n");
			ASSERT(0);
		}
		return CgValue(addr,t,0);
	} else if (this->val){
		if (auto v=this->val->as_variable()){
			if (v->reg_is_addr){
				return CgValue(v->reg_name, new Type(0,PTR,v->type()));
			}
		}
	}
	{
//		ASSERT(0 && "bug/oversight.. tryting to take adress of register, ");
		// comit to memory.
		dbprintf("warning new untested codepath, storing a register to take its adress, probably better to have committed to stack earlier\n");
		auto regval=cg.emit_alloca(this->type);
		cg.emit_store(this->reg,this->type,regval.addr);
		return regval;
		//			return this->to_stack(cg).addr_op(cg,t);
	}
}
CgValue CgValue::deref_op(CodeGen& cg) const {
	return deref_op(cg,type->sub);
}

CgValue CgValue::ref_op(CodeGen& cg,const Type* t) const { // take type calculated earlier
	ASSERT(this->type);
#if DEBUG>=2
	this->type->dump(0);
	t->dump(0);newline(0);
#endif
	if (!reg && (bool)addr) {	// we were given a *reference*, we make the vlaue the adress
		ASSERT(t->is_ref());
		ASSERT(t->sub->is_equal(this->type));
		return CgValue(addr,t);
	}else if (reg && !addr){
		ASSERT(t->is_ref() && !this->type->is_ref());
		ASSERT(t->sub->is_equal(this->type));
		cg.emit_comment("TODO: REMOVE THE AUTO LOAD OF ARGUMENTS, DEFER THAT TO 'ARG CONVERSION- that should check if arg is a 'ref', and not load it. currently we store then load!");
		auto r=cg.emit_alloca_type((Expr*)t, this->type);
		cg.store(r,*this);
		
#if DEBUG>=2
		dbprintf("ref of stored into:-");
		r.type->dump(0);newline(0);
#endif
		return r;
	} else if (auto v=this->val->as_variable()){
		if (v->reg_is_addr){
			return CgValue(v->reg_name, new Type(0,REF,v->type()));
		}
	}
	{
		ASSERT(0 && "tryting to take adress of register");
		return CgValue();
		//			return this->to_stack(cg).addr_op(cg,t);
	}
}
CgValue CgValue::deref_for_dot(CodeGen& cg, const Type* t)const {
	if (!t) { t=this->type->sub;}
	// todo type assertion 't' is the output type.
	if (!addr) {		// the value we were given is now the *adress* - we return a reference, not a pointer.
		return CgValue(0,t,reg);
	} else {
		// it its'  a reference type, we need to load that first.
		auto ret=cg.load(*this);
		return CgValue(0,t,ret.reg); // ... and return it as another reference: eg given T&* p,  returning T& q=*p
	}
}
CgValue CgValue::deref_op(CodeGen& cg, const Type* t)const {
	/// todo: * C++ only loads a pointer.
	/// if its a ref-to-ptr it has to load the pointer first
	ASSERT(this->type->name==PTR && this->type->sub->is_equal(t));
	return deref_for_dot(cg,t);
}

CgValue CgValue::get_elem(CodeGen& cg,const Node* field_name,Scope* sc)const{	//calculates & returns adress
	ASSERT(type );
	auto sd=type->get_struct_autoderef();
	if (!sd) {
		type->dump(-1);
		error(field_name,"struct not resolved\n");
	}
	int index=sd->field_index(field_name);
	auto field=sd->find_field(field_name);
	return get_elem_index(cg,index);
}
CgValue CgValue::get_elem_index(CodeGen& cg, int field_index,const Type *field_type) const{
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
CgValue CgValue::index(RegisterName index){ // calculates & returns adress
	ASSERT(0 && "TODO");
	return CgValue();
}

CgValue	CgValueVoid(){
	return CgValue();
}



CgValue 	CodeGen::flow_result[32]; // hack till we move stupid header

using std::function;
CgValue CodeGen::load(const CgValue& v,const Type* result_type) {
	//if (!this->is_valid()) return CgValue();
	auto& cg=*this;
	auto ofp=cg.ofp;
	if (v.is_elem() ){
		// todo: why not 'addr' aswell?
		if (v.type->is_pointer() || (v.is_addr())) {
			cg.emit_comment("dot reg=%s addr=%s index=%d",str(v.reg),str(v.addr),v.elem);
			auto sub=v.get_elem_index(cg,v.elem,result_type);
			return cg.load(sub);
		} else{
			// elem acess
			auto r=cg.next_reg();
			cg.emit_ins_begin(r,"extractvalue");
			cg.emit_type_reg(v.type,false,v.reg);
			cg.emit_txt(",%d",v.elem);
			cg.emit_ins_end();
			return CgValue(r,v.type->get_elem(v.elem));
		}
	}
	if(v.val) {
		auto outr=cg.next_reg();
		
		if (auto lit=v.val->as_literal()){
			return cg.emit_make_literal(lit);
			// todo: string literal, vector literals, bit formats
		}
		else if (auto fp=v.val->as_fn_def()){
			if (fp->is_closure()) {
				// Build a pair. we would also build the CaptureVars Object here.
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
				cg.emit_type(v.type,true);
				cg.emit_fn_ptr(fp->get_mangled_name());
				cg.emit_ins_end();
				return CgValue(outr,fp->fn_type);
			}
		}
		else if (auto var=v.val->as_variable()){
			if (var->reg_is_addr){
				auto r= CgValue(0,var->type(),var->reg_name);
				return cg.load(r);
			} else
				return v;	//NOP - remember its' lazy, this is a valid case for reg-reg
		}
	}
	if ((bool)v.addr) {
		ASSERT(v.reg==0);
		auto dstr=cg.next_reg();
		cg.emit_ins_begin(dstr,"load");
		cg.emit_type_reg(v.type, v.addr!=0,v.addr);// an extra pointer level if its' a reference
		cg.emit_ins_end();
		return CgValue(dstr,v.type);
	}
	if (!(v.reg && !v.addr)){
		dbprintf("======\n");
		v.dump();
		newline(0);
		v.type->dump_if(0);
		error(0,"\n load - var broken \n");
	}
//	ASSERT(v.reg && !v.addr);
	return v;
}
void CodeGen::emit_operand_literal(const CgValue& v, const ExprLiteral* lit) {
	auto &cg=*this;
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
			error(lit,"TODO literal format");
			break;
		}
}
void CodeGen::emit_operand(const CgValue& v){
	auto &cg=*this;
	auto ofp=cg.ofp;
	if (v.reg!=0){
		cg.emit_reg(v.reg);
		return;
	}
	else if (v.addr!=0){
		cg.emit_reg(v.addr);
		return;
	}
	else if (v.val){
		if (auto lit=v.val->as_literal()) {
			ASSERT(v.addr==0 && "check case above, incorrect assumptions")
			cg.emit_operand_literal(v,lit);
		}
		if (auto fn=v.val->as_fn_def()) {
			cg.emit_global(fn->get_mangled_name());
		}
		return;
	} else {
		cg.emit_txt(" <?CgV?> ");
		v.type->dump_if(0);
		ASSERT(v.type);
		ASSERT(0 && "missing register, value wasn't found in resolving");
	}
}
CgValue CodeGen::store(const CgValue& v){// for read-modify-write
	auto &cg=*this;
	ASSERT(v.type);
	if (!v.addr || !v.type)
		return v;
	cg.emit_store(cg.next_reg(), v.type, v.addr);
	return v;
}
CgValue CodeGen::store(const CgValue& dst,const CgValue& src) {
	auto &cg=*this;
	auto src_in_reg=cg.load(src);
#if DEBUG>=3
	dbprintf("\nstore:\t");
	src.dump();
	dbprintf("\nto:\t");
	dst.dump();
	dbprintf("\nsrc in reg=\t");
	src_in_reg.dump();
	dbprintf("\n");
#endif
	if (src_in_reg.type->is_ref() && src_in_reg.type->sub->is_equal(dst.type)){
		src_in_reg=CgValue(0,src_in_reg.type->sub,src_in_reg.reg).load(*this);
#if DEBUG>=3
		dbprintf("\nsrc in reg loaded again because it was a ref=\n");
		src_in_reg.dump();
		dbprintf("\n");
#endif
	}

	if (dst.elem>=0){
		auto newreg=next_reg_name(&cg.m_next_reg);
		if ( dst.reg && !dst.addr){
			auto x=dst.get_elem_index(cg, dst.elem);
			return cg.emit_store(src_in_reg.reg,src.type, x.addr);
		}else if (dst.addr && !dst.reg){
			auto x=dst.get_elem_index(cg, dst.elem);
			return cg.emit_store(src_in_reg.reg,src.type, x.addr);

		}else {
			ASSERT(dst.type->name!=PTR);
			cg.emit_ins_begin(newreg,"insertelement");
			cg.emit_type_reg(dst.type,false,dst.reg); cg.emit_comma();
			auto elem_t=dst.type->get_elem(dst.elem);
			cg.emit_type_reg(elem_t,false,src_in_reg.reg);
			cg.emit_i32_lit(dst.elem);
			cg.emit_ins_end();
			return CgValue(newreg, dst.type);
		}
	}

	if (dst.addr){
		//ASSERT( dst.type->is_equal(src_in_reg.type));
		

		cg.emit_store(src_in_reg.reg, dst.type, dst.addr);
	}
	else if (dst.reg) {
		cg.emit_store(src_in_reg.reg, dst.type, dst.reg);
	} else if (dst.val) {
		if (auto v=dst.val->as_variable()) {
			if (v->reg_is_addr && v->reg_name) {
				cg.emit_store(src_in_reg.reg, dst.type, v->reg_name);
				return CgValue(v);
			} else if (!v->reg_name){
				if (!v->on_stack) {
					v->reg_name=src_in_reg.reg;
				} else {
					v->reg_name=cg.emit_alloca_type(v, dst.type).addr;
					v->reg_is_addr=true;
					cg.emit_store(src_in_reg.reg, dst.type, v->reg_name);
				}
				return CgValue(v);
			}
		}
		dst.val->dump(0);
		ASSERT(0 && "store case not handled, to var..");
	}
	return dst;
}
CgValue CodeGen::emit_loadelement(const CgValue &src, Name n){
	auto sd=src.type->deref_all()->struct_def();
	if (!sd){
		dbprintf("struct not found ont type %s\n",str(src.type->name));src.type->dump_if(0);newline(0);
	}
	auto i=sd->get_elem_index(n);
	return emit_getelementref(src.load(*this), 0, i, sd->get_elem_type(i));
}

CgValue CodeGen::emit_getelementref(const CgValue &src, Name n,const Type* t){
	auto i=src.type->deref_all()->struct_def()->get_elem_index(n);
	return emit_getelementref(src.load(*this), 0, i,t);
}
CgValue CodeGen::emit_getelementref(const CgValue& src, int i0, int field_index,const Type* override_type){

	auto numptr=src.type->num_pointers()+(src.addr?1:0);
	if (numptr==0){
		auto ret=next_reg();
		ASSERT(i0==0);
		return CgValue(emit_extractvalue(ret, src.type, src.reg, field_index),override_type?override_type:src.type->get_elem_type(field_index));
	}
	ASSERT(numptr==1);
	
	auto sderef=src.type->deref_all();
	auto sd=sderef->def;//struct_def();
	if (!sd && sderef->name==TUPLE) {
		sd=sderef;
	}
	if (!sd){
		dbprintf("\nsomething wrong:-\n");
		dbprintf("%s\n",src.type->name_str());
		src.dump();
		src.type->def->dump_if(0);
		ASSERT(0 && "something wrong");
	}
	auto field_type=sd->get_elem_type(field_index);//fields[field_index]->type();
	auto areg=this->next_reg();
	this->emit_ins_begin(areg, "getelementptr inbounds  ");
	this->emit_type_operand(src);
	this->emit_i32_lit(0);
	
	this->emit_i32_lit(field_index);
	this->emit_ins_end();
	auto ret= CgValue(0,field_type,areg);
	return ret;
}
CgValue CodeGen::emit_assign(const CgValue& dst, const CgValue& src){
	return dst.store(*this, this->load(src));
}
CgValue CodeGen::emit_val_i32(int v){
	auto reg=this->next_reg();
	emit_ins_begin(reg,"or");
	emit_i32_lit(0);
	emit_comma();
	emit_txt("%d",v);
	emit_ins_end();
	return	CgValue(reg,Type::get_i32());
}
CgValue CodeGen::emit_val_u32(int v){
	auto reg=this->next_reg();
	emit_ins_begin(reg,"or");
	emit_u32_lit(0);
	emit_comma();
	emit_txt("%d",v);
	emit_ins_end();
	return	CgValue(reg,Type::get_u32());
}

ExprLiteral* g_lit_bool;
CgValue CodeGen::emit_val_bool(bool v){
	if (!g_lit_bool) g_lit_bool=new ExprLiteral(v);
	return CgValue(g_lit_bool);
}

CgValue CodeGen::emit_make_literal(ExprLiteral *lit){
	auto ltn=lit->type()->name;
	auto outr=this->next_reg();

	if (ltn==PTR){ // any literal pointer is void* 0? no
		//ASSERT(lit->u.val_ptr==nullptr && "TODO non null literal pointers")
		emit_reg(outr);
		emit_txt("=getelementptr inbounds i8* null, %s %zu\n",size_t_str(),(size_t)lit->u.val_ptr);
		return CgValue(outr,lit->type());
	}
	else if (ltn==VOID){
		emit_reg(outr);
		emit_txt("=undef\n");
		return CgValue(outr,lit->type());
	}
	else if (ltn==BOOL){
		emit_ins_begin(outr,"or");
		emit_int_lit(BOOL,ltn==BOOL_TRUE?1:0);
		emit_comma();
		emit_txt("%d",lit->u.val_bool?1:0);
	}
	else if (lit->type_id==T_BOOL){
		emit_ins_begin(outr,"or");
		if (lit->u.val_bool){
			emit_txt(" i1 1,1");
		}
		else
			emit_txt(" i1 0,0");
//		emit_bool(lit->u.val_bool);
//		emit_int_lit(BOOL,ltn==BOOL_TRUE?1:0);
//		emit_comma();
//		emit_txt("%d",lit->u.val_bool?1:0);
	}
	else if (ltn==INT){
		return emit_val_i32(lit->u.val_int);
	}
	else if(ltn==FLOAT){
		// todo, i guess we're goint to have t make a global constants table
		emit_ins_begin(outr,"fadd");
		emit_txt("float 0.0, ");
		emit_txt(" 0x%x00000000",lit->u.val_int);
	}
	else if (ltn==STR){
		emit_ins_begin(outr,"getelementptr inbounds");
		emit_comma();
		emit_txt("[%d x i8]* @%s",lit->llvm_strlen, getString(lit->name));
		
		emit_i32_lit(0);
		emit_i32_lit(0);
		ASSERT(lit->llvm_strlen);
	}
	else {
		lit->type()->dump_if(-1);
		error(lit,"literal type not handled yet");
		error_end(lit);
	}
	emit_ins_end();
	return CgValue(outr,lit->type());
}

CgValue CodeGen::emit_store(RegisterName reg,const Type* type, RegisterName addr){
	emit_ins_begin_name("store");
	emit_type(type,0);
	emit_reg(reg);
	emit_type(type,1);
	emit_reg(addr);
	emit_txt(", align 4"); // todo.. align of type surely
	emit_ins_end();
	return CgValue(0,type,addr);
}
CgValue CodeGen::emit_store_i32(const CgValue& dst,int value){
	auto r=next_reg();
	ASSERT(dst.addr && "store requires a reference destination");
	emit_ins_begin_name("store");
	emit_i32_lit(value);
	emit_type_operand(dst);
	emit_txt(", align 4");
	emit_ins_end();
	return dst;
}
CgValue CodeGen::emit_store_u32(const CgValue& dst,int value){
	auto r=next_reg();
	ASSERT(dst.addr && "store requires a reference destination");
	emit_ins_begin_name("store");
	emit_u32_lit(value);
	emit_type_operand(dst);
	emit_txt(", align 4");
	emit_ins_end();
	return dst;
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

CgValue  CodeGen::emit_call(const CgValue& call_expr, const CgValue& arg1)
{

	emit_call_begin(call_expr);
	emit_args_begin();
	emit_type_operand(arg1);
	emit_args_end();
	return emit_call_end();
}

CgValue  CodeGen::emit_call(const CgValue& call_expr, const CgValue& arg1, const CgValue& arg2)
{
	emit_call_begin(call_expr);
	emit_args_begin();
	emit_type_operand(arg1);
	emit_type_operand(arg2);
	emit_args_end();
	return emit_call_end();
}

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
	emit_nest_end("]");
	if (ref) emit_pointer_end();
}
void CodeGen::emit_type(const Type* t, bool ref) { // should be extention-method.
	emit_comma(); // type always starts new operand
	if (!t) { emit_txt("<type expected>");return;}
	if (ref) emit_pointer_begin();
	if (t->name==AUTO){
		error(t->m_origin,"'auto' type not converted in instantiation \n");
	}
	else
	if (t->is_pointer()){
//		dbprintf("THIS IS SUSPECT, REF ISn'T NEEDED TWICE");
		emit_pointer_begin();
		if (t->sub->name==VOID)
			emit_txt("i8");
		else
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
		auto sd=t->get_struct_autoderef();
		if (!sd) {
			if (t->def)
			/// TODO we quieted this error because closure objs dont make a struct
				emit_reg(t->def->get_mangled_name());
			else {
				//if (strcmp("closure_51main",t->name_str())){
				//	error(t->m_origin,"unresolved type, has no definition %s",str(t->name));
				//}
				emit_reg(t->name);
			}
		}else
		if (sd->name)
			emit_struct_name(sd->get_mangled_name());
		else {
			// LLVM does allow listing an anonymous struct
			emit_struct_begin();
			//error(t,"no struct def");
			for (auto i=0; i<sd->fields.size(); i++){
				emit_type(sd->fields[i]->type(),false);
			}
			if (auto pad=sd->padding()){
				emit_array_type(Type::get_u8(), pad);//get_u32
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
		else {
			if (t->name==BOOL ||t->name==BOOL_TRUE||t->name==BOOL_FALSE||t->name==U8||t->name==I8){
				dbg(dbprintf("\n"));
			}
			emit_txt("%s",t?get_llvm_type_str(t->name):"???");//,t.is_pointer?"*":"");
		}
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

CgValue CodeGen::emit_malloc(const Type* t,size_t count){
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
	auto r=this->load(ptr);
	auto r1=this->emit_cast_to_i8ptr(r);
	emit_ins_begin(0,"call i8* @free"); /// TODO: sized form of free.
	emit_args_begin();
	emit_type_operand(r1);
//	emit_i32_lit(t->sub->size()*count);
	emit_args_end();
	emit_ins_end();
	return CgValue();
}

CgValue CodeGen::emit_malloc_array(const Type* t,CgValue count){
	auto rsizereg=next_reg();
	auto cr=this->load(count);
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
	this->emit_operand(src);
}

void CodeGen::emit_instruction_sub(Name opname,const Type* type,  RegisterName dstr,CgValue src1){
//	ASSERT(dst.is_reg());
	const LLVMOp* op=get_op_llvm(opname,type?type->name:VOID);
	this->emit_reg(dstr);
	emit_ins_name( op?op->op_signed:str(opname));
	if (is_comparison(opname)){
		dbg(dbprintf("cmopare resut=%s\n",str(type->name)));
		emit_type(src1);
	}
	else{
		emit_type(type,false);
	}
	this->emit_operand(src1);
}
CgValue CodeGen::emit_instruction(Name opname,const Type* type,Name outname, CgValue src1){
	auto dstr=next_reg();
	auto r1=load(src1);
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dstr,r1);
	emit_ins_end();
	return CgValue(dstr, type);
}
CgValue CodeGen::emit_instruction(Name opname,const Type* type,Name outname,  CgValue src1,CgValue src2){
	return emit_instruction(opname, type, src1, src2);
}
CgValue CodeGen::emit_instruction(Name opname,CgValue src1,CgValue src2){
	auto src=this->load(src1);
	return emit_instruction(opname, src.type, src, this->load(src2));
}
CgValue CodeGen::emit_instruction(Name opname,const Type* type,  CgValue src1,CgValue src2){
	ASSERT(type!=0);
	auto r1=this->load(src1);
	auto r2=this->load(src2);
	auto dstr=next_reg();
	auto dbg=[&](){
		dbprintf("src1.reg=%s/%s.%d\n",str(src1.reg),str(src1.addr),src1.elem);
		dbprintf("src1.reg=%s/%s.%d r=%s\n",str(src1.reg),str(src1.addr),src1.elem, str(r1.reg));
	};
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dstr,r1);
	emit_comma();
	this->emit_operand(r2);
	emit_ins_end();
	return CgValue(dstr, type);
}

CgValue CodeGen::emit_instruction_reg_i32(Name opname,const Type* type,  Name outname,CgValue src1,int imm_val){
	ASSERT(type!=0);
	auto r1=load(src1);
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
	sprintf(rname, "r%d%s",this->m_next_reg++,isSymbolStart(s[0],0)?s:"rfv");
	return getStringIndex(rname);
}

CgValue CodeGen::emit_get_array_elem_ref(const CgValue& src_array, const CgValue& index)
{
	auto &cg=*this;
	auto dstreg = cg.next_reg();
	auto array=src_array;
	if (src_array.type->num_pointers()+(src_array.addr?1:0) > 1){
		array=cg.load(src_array);
	}
	auto index_reg=cg.load(index);

	Type* rett=0;
	cg.emit_ins_begin(dstreg,"getelementptr inbounds");
	cg.emit_type_reg(array.type,array.addr!=0,array.addr?array.addr:array.reg);//!expr.reg);
	// TODO: pointer-to-pointer will defeat this.
	if (array.type->deref_all()->name==ARRAY){
		cg.emit_i32_lit(0);
		rett=array.type->deref_all()->sub;
	} else
		rett = array.type->sub;
	cg.emit_type_operand(index_reg);
	cg.emit_ins_end();
	return CgValue(0,rett,dstreg);
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

CgValue CodeGen::emit_alloca_type(Expr* holder, const Type* t) {
//	if (holder) {
//		if (holder->reg_name)	// dont emit 'alloca' twice if the holder already has
//			return CgValue(0,t,holder->reg_name);
//	}
	if (t->is_void()){
		if (holder)emit_comment("void value not allocated for %d",holder->pos.line);
		return CgValue();
	}
	RegisterName r= holder?holder->get_reg(*this, true):next_reg();
	//RegisterName r= next_reg();
	emit_ins_begin(r,"alloca"); emit_type(t,false);
	emit_txt(", align %zu", orelse(t->alignment(),g_DefaultAlignment));
	emit_ins_end();
	return CgValue(0,t, r);
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
	auto cr=load(cond);
	emit_ins_begin_name("br");
	emit_txt("i1 %%%s, label %%%s, label %%%s",str(cr.reg), str(label_then), str(label_else));
	emit_ins_end();
}
CgValue CodeGen::emit_break(  CgValue v, int levels){
	
	auto i=this->flow_depth-levels;
	ASSERT(i>=0);
	this->flow_result[i].store(*this, v);
	emit_branch(this->flow_break_to[i]);
	return this->flow_result[i];
}
CgValue CodeGen::emit_continue(int levels){
	auto i=this->flow_depth-levels;
	ASSERT(i>=0);
	emit_branch(this->flow_continue_to[i]);
	return this->flow_result[i];
}

Name CodeGen::gen_label(const char* label,int index){
	auto i=index?index:this->m_next_reg++;
	char tmp[256];sprintf(tmp,"%s%d",label,i);
	return getStringIndex(tmp);
}

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
	auto init=nf->init->compile(cg,sc, CgValue());
	
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
	if (retval.is_valid()){
		result_ref.store(cg,retval);
	}
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

CgValue CodeGen::emit_for(ExprFor* e_for, Expr* e_init,Expr* e_cond, Expr* e_incr, Expr* e_body, Expr* e_else_block){
	// this is the kind of stupid bouncing we want to write a new language to avoid
	// not sure how far we want to abstract this.. codegen llvm/C or not. using c-like For as
	// universal 'back end' loop is quite flexible
	return emit_for_llvm(*this,e_for, e_init, e_cond,e_incr,e_body,e_else_block);
}


void CodeGen::emit_return(CgValue ret){
	if (ret.is_valid() && !ret.type->is_void()) {
		auto r=this->load(ret);
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

CgValue CodeGen::emit_cast_to_i8ptr(const CgValue& val) {
	return emit_cast_reg(val.reg, val.type, i8ptr());
}
CgValue CodeGen::emit_cast_from_i8ptr(const CgValue& val, const Type* to_type) {
	return emit_cast_reg(val.reg, i8ptr(), to_type);
}


CgValue CodeGen::emit_cast_raw(const CgValue&src_val, const Type* to_type){
	return emit_cast_to_type(src_val,to_type);
}
CgValue CodeGen::emit_cast(const CgValue&src_val, Expr* rhs_type_expr){
	return emit_cast_to_type(src_val,rhs_type_expr->type());
}
CgValue CodeGen::emit_cast_to_type(const CgValue&lhs_val, const Type* rhst){
	auto lhs_reg=this->load(lhs_val);
//	auto lhst=lhs_val.type;
	return emit_cast_reg(lhs_reg.reg, lhs_val.type, rhst);
}

CgValue CodeGen::emit_cast_ref(CgValue src, const Type* rhst){
	ASSERT(src.addr);
	auto dstr=this->next_reg();
	emit_ins_begin(dstr, "bitcast");
	emit_type_reg(src.type,true,src.addr);
	emit_separator(" to ");
	emit_type(rhst,true);
	emit_ins_end();
	return CgValue(0,rhst,dstr);
}
CgValue CodeGen::emit_cast_reg(RegisterName srcr,const  Type* lhst, const Type* rhst)
{
	if (lhst->type()->is_equal(rhst))
		return CgValue(srcr,rhst);
	auto &cg=*this;
	auto dstr=next_reg();
	const char* ins="nop";
	if (lhst->is_pointer() && rhst->is_bool()){
		// example..
		//%15 = icmp ne i8** %14, null, !dbg !130
  		//%16 = zext i1 %15 to i8, !dbg !130

//		if (lhst->is_pointer())
		//auto cmp=emit_instruction("icmp ne",CgValue(srcr,lhst), )
		cg.emit_nest_begin("");
		cg.emit_reg(dstr);
		cg.emit_txt("=icmp ne ");
		cg.emit_type(lhst);
		cg.emit_reg(srcr);
		cg.emit_comma();
		cg.emit_txt("null;\n");
		cg.emit_nest_end("");
		// 2nd part of conversion done below.
		// oh this is retarded.
		// we need a real 'i1' bool.
		// out bools in memory are not the same at all.
		//... resolve this in load bool, store bool. bools in registers will be i1
		return CgValue(dstr,rhst);
	}else
	if (rhst->is_pointer() || rhst->is_pointer()){
		ins="bitcast";
	}else
	if (lhst->is_int() && rhst->is_float()){
		ins=rhst->is_signed()?"sitofp":"uitofp";
	}
	else if (lhst->is_float() && rhst->is_int()){
		ins=lhst->is_signed()?"fptosi":"fptoui";
	}
	else if (lhst->is_int()&&!lhst->is_bool() && rhst->is_bool()){
		emit_ins_begin(dstr, "icmp ne");
		emit_type_reg(lhst,0,srcr);
		emit_comma();
		emit_txt("0");//int_lit(lhst,0);
		emit_ins_end();
		return CgValue(dstr,rhst);
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
			cg.emit_reg(__ENV_I8_PTR);
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
void CodeGen::emit_function_type(const Type* t,bool variadic_overide) {
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
	if(variadic_overide) cg.emit_txt(",...");
	cg.emit_args_end();
	cg.emit_pointer_end();
}
void CodeGen::emit_function_type(ExprFnDef* fn_node){
	emit_function_signature(fn_node,EmitType);
}

CgValue CodeGen::emit_if(Scope* sc,CgValue input, Node* cond, Node* body, Node* else_block,Type* ret_type){

	auto &cg=*this;
	// TODO: Collect phi-nodes for anything modified inside.
	auto curr_fn=cg.curr_fn;

	auto condition=cond->compile(cg,sc,input);
	int index=cg.m_next_reg++;
	auto label_if=cg.gen_label("if",index);
	auto label_endif=cg.gen_label("endif",index);
	auto retvalref = cg.emit_alloca(ret_type);

	if (!else_block){
		cg.emit_branch(condition,label_if,label_endif);
		cg.emit_label(label_if);
		auto ifblock=body->compile(cg,sc,input);
		if (ifblock.is_valid()) {
			ifblock=cg.load(ifblock);
			retvalref.store(cg,ifblock);
		}// else default constructor for type
		cg.emit_branch(label_endif);
		cg.emit_label(label_endif);
		return retvalref;
	}

	auto label_else=cg.gen_label("else",index);

	cg.emit_branch(condition,label_if,label_else);
	cg.emit_label(label_if);
	auto if_result=body->compile(cg,sc,input);
	if (if_result.is_valid()){
		if_result=cg.load(if_result,0);
		retvalref.store(cg,if_result);
	}
	cg.emit_branch(label_endif);
	cg.emit_label(label_else);
	auto else_result=else_block?else_block->compile(cg,sc,input):CgValue();
	if (else_result.is_valid()){
		else_result=cg.load(else_result,0);
		retvalref.store(cg,else_result);
	}
	cg.emit_branch(label_endif);
	cg.emit_label(label_endif);
	return retvalref;
}



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
void CodeGen::emit_args_begin()	{emit_nest_begin("(");}
void CodeGen::emit_args_end()		{emit_nest_end(")");}
void CodeGen::emit_struct_begin(int align)	{ASSERT(!m_struct_align && "TODO nested struct alignment");emit_nest_begin("{");m_struct_align=align;}
void CodeGen::emit_struct_end()	{emit_nest_end("}"); if (m_struct_align){	this->emit_txt(", align 16");
}; m_struct_align=0;  }
void CodeGen::emit_pointer_begin()	{emit_nest_begin("");};
void CodeGen::emit_pointer_end()	{emit_nest_end("*");};

void CodeGen::emit_struct_def_begin(Name n){
	this->emit_struct_name(n);
	this->emit_ins_name("type");
	this->emit_struct_begin();
}
void CodeGen::emit_struct_def_end(){
	this->emit_struct_end();
	this->emit_ins_end();
}

void CodeGen::emit_comma(){
	if (this->comma==1){emit_txt(",");}
	this->comma=1;
}
void CodeGen::emit_separator(const char* txt){
	emit_txt(txt);
	this->comma=0;
}

void CodeGen::emit_call_begin(const CgValue& fnr){
	auto fn=fnr;
	if (fn.is_addr()){
		fn=this->load(fn);
	}
	auto &cg=*this;
	auto fnt=fn.type;
	RegisterName r=0;
	if(!fnt->fn_return()->is_void()){
		r=this->next_reg();
		emit_ins_begin(r, "call");
	} else
		emit_ins_begin_name("call");
	ASSERT(cg.call_depth<(32-1));
	return_reg[cg.call_depth++]=CgValue(r,fnt->fn_return());
	bool variadic=false;
	if (fn.val)if(fn.val->as_fn_def())variadic=fn.val->as_fn_def()->variadic;
	emit_function_type(fnt, variadic);
	emit_operand(fn);
}
CgValue CodeGen::emit_call_end(){
	auto &cg=*this;
	emit_ins_end();
	return return_reg[--cg.call_depth];
}

RegisterName  CodeGen::emit_ins_begin(RegisterName reg, const char* op){
	/// TODO: return 'CgValue' from emit_call_end(), dont take 'RegisterName' param.
	if (!reg) {
		emit_ins_begin_name(op);
	}
	else {
		emit_ins_begin_sub();
		emit_reg(reg);
		emit_ins_name(op);
	}
	return reg;
}
void CodeGen::emit_int_lit(Name type, int value) {
	emit_comma();
	emit_txt(get_llvm_type_str(type));
	emit_txt(" %d",value);
}
void CodeGen::emit_int_lit(const Type* t, int value) {
	emit_comma();
	emit_txt(get_llvm_type_str(t->name));
	emit_txt(" %d",value);
}

void CodeGen::emit_i32_lit(int index) {
	emit_comma();
	emit_txt("i32 %d",index);
}
void CodeGen::emit_u32_lit(int index) {
	emit_comma();
	emit_txt("u32 %d",index);
}

void CodeGen::emit_i32_reg(Name reg) {
	emit_comma();
	emit_txt("i32 ");
	emit_reg(reg);
}

RegisterName	CodeGen::emit_extractvalue(RegisterName dst,const Type* type,RegisterName src,int index){
	emit_ins_begin(dst,"extractvalue");
	emit_type(type);
	emit_reg(src);
	emit_comma();
	emit_txt("%d",index);
	emit_ins_end();
	return dst;
}

void
CodeGen::compile_destructor(Scope* sc, CgValue val,bool force){
	auto f=sc->find_fn_for_types(__DESTRUCTOR, 1,val.type, nullptr, nullptr, 0);
	if (f){
		// only if it was exact match. raw function search does autoref. so, T calls ~T(), but *T doesn't.
		auto f_arg0_t=f->args[0]->type();
		if (!f_arg0_t->sub->is_equal(val.type))
			return;
		if (!force){
			if (!val.rvalue){	// it only destroys rvalues. use 'force' to override for vars at end-of-scope.
				return;
			}
		}
		dbg2(printf("emit destructor for %s:",val.val?val.val->name_str():"?" ));dbg2(val.type->dump(-1)); dbg2(dbprintf(" \tdesutrctor arg0:"));dbg2(f->args[0]->type()->dump(-1));dbg2(newline(0));
		
		this->emit_call(CgValue(f), val.addr_op(*this, f_arg0_t));
	}
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

void CodeGen::emit_global_begin(Name n){
	this->emit_txt("@%s = ",str(n));
	this->emit_ins_begin_name("global");
}


int CodeGen::emit_global_string_literal(Name n, const char* s){
	char buffer[512];
	int llvm_strlen=translate_llvm_string_constant(buffer,512, s)+1;
	this->emit_global(n);
	this->emit_txt("= private unnamed_addr constant [%d x i8] c\"%s\\00\"\n", llvm_strlen, buffer);
	return llvm_strlen;
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
/*
Type* CodeGen::i8ptr(){
	if (! this->m_i8ptr){
		this->m_i8ptr=new Type(nullptr,PTR,I8);
	}
	return this->m_i8ptr;
}
*/

void CodeGen::emit_nest_begin(const char* str){
	auto& cg=*this;
	ASSERT(depth<32);
	commas[depth]=comma;
	depth++;
	emit_txt(str);
	comma=0;
}
void CodeGen::emit_reg(RegisterName dst ) {
	ASSERT(dst);
	emit_txt("%%%s ",str(dst));
}

void CodeGen::emit_nest_end(const char* str){
	ASSERT(depth);
	depth--;
	comma=commas[depth];
	emit_txt(str);
}

void CodeGen::emit_global_fn_ptr(const Type* t, Name n){
	auto &cg=*this;
	cg.emit_nest_begin("");
	cg.emit_fn_ptr(n);
	cg.emit_ins_name("global");
	cg.emit_function_type(t);
	cg.emit_global(n);
	cg.emit_nest_end("");
}

void CodeGen::emit_alloca_array_type(Name r, const Type* t, Name count,int align)
{
	auto &cg=*this;
	cg.emit_txt("\t");
	cg.emit_reg(r);
	cg.emit_txt(" = alloca [%s x %s] , align %zu\n",str(count),get_llvm_type_str(t->name),align);
}
void CgValue::dump()const {
	dbprintf("%(");
	dbprintf("reg=%s;",str(reg));
	dbprintf("val=%p %s %s;",val,val?val->name_str():"",val?val->kind_str():"");
	dbprintf("addr=%s;elem=%d;type:",str(addr),elem);
	type->dump_if(-1);
	dbprintf(" %s",rvalue?"RVal":"LVal");
	dbprintf(")");
}

CgValue CodeGen::emit_conversion(const Node*n, const CgValue& src0, const Type* to_type,const Scope* sc) {
	/// TODO: make it clear, this is a helper, not overloaded per back-end
	// no need to convert..
	dbg(dbprintf("\nconvert type:- \n"));
	dbg(src0.dump());dbg(dbprintf("\n --to--> \n"));
	dbg(to_type->dump(-1));dbg(dbprintf("\n"));
	CgValue src=src0;
	if (src0.type->is_equal(to_type,false)){
		if (src0.reg && src0.elem>=0 && !src0.addr)
			return src0.load(*this);
		if (src0.reg && src0.elem<0 && !src0.addr)
			return src0;
		// must load if turning a ref int a value
		if (src0.addr && !src0.reg){
			src=src0.load(*this);
		}
	}
	if ((src.type->is_ref() && !to_type->is_ref()) ||(src0.type->is_pointer_or_ref() && src0.addr && src0.reg==0)
		){
		src=src.load(*this);
	}
	

	dbg(dbprintf("\n after 'ref'->'value' src is"));
	dbg(src.dump());dbg(newline(0));
	dbg(dbprintf("\n"));

	// We must convert..
	// is it a trivial pointer conversion?

	if (!src.type->is_ref() && to_type->is_ref()){
		if (!src.reg && src.elem<0 && src.addr){
			return CgValue(src.addr, to_type);
		} else {
			this->emit_comment("TODO RVO, does llvm do it for us??");
			return src.ref_op(*this,to_type);
			error(n,"\ntried to take reference from value..? this is ok, but am currently trying to make the other case work\n");
		}
	}else
	if (src.type->is_pointer_not_ref() && to_type->is_pointer_not_ref()){
		if ((to_type->sub->name>=IDENT) && !to_type->sub->def){
#if DEBUG>=2
			dbprintf("\n warning type %s not defined?]\n",to_type->sub->name_str());
#endif
			newline(0);
		}
		return this->emit_cast_to_type(src,to_type);
	} else if (src.type->is_equal(to_type,false)){
		if (src.reg && !src.addr){
			return src;
		}
		return src.load(*this);
	}
	// load reference->value?
	if (src0.type->is_ref() && !to_type->is_ref()){
		if (!src0.type->sub->is_equal(to_type)){
			error(n,"\ncan't convert,bug\n");
		}
		return src.load(*this);
	}
	// have a value, want a pointer, luckily we have a reference..
	if (to_type->is_pointer_or_ref() && src.type->is_equal(to_type->sub) && src.addr){
		return CgValue(src.addr,to_type,0);
	}
	
	// lazy reference can still be casted.
	if (src.type->is_pointer()&& to_type->is_pointer()){
		return this->emit_cast_to_type(src,to_type);
	}
	if (src.addr && src.reg==0){
		return this->emit_cast_ref(src,to_type);
	}

	dbprintf("\nwhen copmiling..:-\n");
	n->dump_if(0);newline(0);
	dbprintf("\ntried to convert:-\n");
	src0.dump();
	dbprintf("\n--to-->\n");
	to_type->dump(-1);newline(0);
	error(n,"\nTODO need conversion operators\n");
	return CgValue();

}

void compile_raw_vtable(CodeGen& cg, ExprStructDef* sd){
	// raw vtable looks more like what clang spits out, but we want static fields & more metadata
	cg.emit_txt("@%s = private unnamed_addr",str(sd->vtable_name));
	cg.emit_ins_begin_name("constant");
	cg.emit_array_type(cg.i8ptr(), (int)sd->virtual_functions.size());
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


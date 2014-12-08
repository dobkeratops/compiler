 #include "codegen.h"
/// details of compiling LLVM
/// CodeGen is planned to be an interface, slot in 'CodeGenLLVM' / 'CodeGenC'
using std::function;
CgValue CodeGen::load(const CgValue& v,Type* result_type) {
	//if (!this->is_valid()) return CgValue();
	auto& cg=*this;
	auto ofp=cg.ofp;
	if (v.elem>=0){
		// todo: why not 'addr' aswell?
		if (v.type->is_pointer() || (v.addr &&!v.reg)) {
			cg.emit_comment("dot reg=%s addr=%s index=%d",str(v.reg),str(v.addr),v.elem);
			auto sub=v.get_elem_index(cg,v.elem,result_type);
			return cg.load(sub);
		} else{
			// elem acess
			auto r=cg.next_reg();
			cg.emit_ins_begin(r,"extractelement");
			cg.emit_type_reg(v.type,false,v.reg);
			cg.emit_i32_lit(v.elem);
			cg.emit_ins_end();
			return CgValue(r,v.type->get_elem(v.elem));
		}
	}
	if(v.val) {
		auto outr=cg.next_reg();
		
		if (auto lit=dynamic_cast<ExprLiteral*>(v.val)){
			return cg.emit_make_literal(lit);
			// todo: string literal, vector literals, bit formats
		}
		else if (auto fp=dynamic_cast<ExprFnDef*>(v.val)){
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
				cg.emit_type(v.type,true);
				cg.emit_fn_ptr(fp->get_mangled_name());
				cg.emit_ins_end();
				return CgValue(outr,fp->fn_type);
			}
		}
		else if (auto var=dynamic_cast<Variable*>(v.val)){
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
	ASSERT(v.reg && !v.addr);
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
			cg.emit_txt(" @TODO_LITERAL_FORMAT %s", getString(lit->name));
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
		if (auto lit=dynamic_cast<ExprLiteral*>(v.val)) {
			ASSERT(v.addr==0 && "check case above, incorrect assumptions")
			cg.emit_operand_literal(v,lit);
		}
		if (auto fn=dynamic_cast<ExprFnDef*>(v.val)) {
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
	
	if (dst.addr)
		cg.emit_store(src_in_reg.reg, dst.type, dst.addr);
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


CgValue CodeGen::emit_getelementref(const CgValue &src, Name n,const Type* t){
	auto i=src.type->deref_all()->struct_def->get_elem_index(n);
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
	return dst.store(*this, this->load(src));
}
CgValue CodeGen::emit_make_literal(ExprLiteral *lit){
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


CgValue  CodeGen::emit_call(const CgValue& call_expr, const CgValue& arg1)
{
	emit_call_begin(call_expr); emit_type_operand(arg1); return emit_call_end();
}
CgValue  CodeGen::emit_call(const CgValue& call_expr, const CgValue& arg1, const CgValue& arg2)
{
	emit_call_begin(call_expr); emit_type_operand(arg1); emit_type_operand(arg2); return emit_call_end();
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

CgValue CodeGen::emit_malloc_array(Type* t,CgValue count){
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
	this->emit_operand(src1);
}
CgValue CodeGen::emit_instruction(Name opname,Type* type,Name outname, CgValue src1){
	auto dstr=next_reg();
	auto r1=load(src1);
	emit_ins_begin_sub();
	emit_instruction_sub(opname,type,dstr,r1);
	emit_ins_end();
	return CgValue(dstr, type);
}
CgValue CodeGen::emit_instruction(Name opname,Type* type,Name outname,  CgValue src1,CgValue src2){
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

CgValue CodeGen::emit_instruction_reg_i32(Name opname,Type* type,  Name outname,CgValue src1,int imm_val){
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
	sprintf(rname, "r%d%s",this->m_next_reg++,isSymbolStart(s[0])?s:"rfv");
	return getStringIndex(rname);
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
CgValue CodeGen::emit_alloca_type(Expr* holder, Type* t) {
	if (t->is_void()){
		emit_comment("void value not allocated for %d",holder->pos.line);
		return CgValue();
	}
	RegisterName r= holder?holder->get_reg(*this, false):next_reg();
	emit_ins_begin(r,"alloca"); emit_type(t,false);
	emit_txt(", align %zu", t->alignment());
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
	// not sure how far we want to abstract this.. codegen llvm/C or not. using ExprFor as
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
	auto lhs_reg=this->load(lhs_val);
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

CgValue emit_if_llvm(CodeGen& cg, Node* ifn, Scope* sc, std::function<CgValue()> f_cond, std::function<CgValue()> f_compile_body, Expr* else_block){
	// TODO: Collect phi-nodes for anything modified inside.
	auto curr_fn=cg.curr_fn;
	//auto ifn=this;
	
	RegisterName outname=cg.next_reg();
	auto condition=f_cond();//cond->compile(cg,sc);
	int index=cg.m_next_reg++;
	auto label_if=cg.gen_label("if",index);
	auto label_endif=cg.gen_label("endif",index);
	if (else_block){
		auto label_else=cg.gen_label("else",index);
		cg.emit_branch(condition,label_if,label_else);
		cg.emit_label(label_if);
		auto if_result=f_compile_body();//body->compile(cg,sc);
		if (if_result.is_valid())if_result=cg.load(if_result,0);
		cg.emit_branch(label_endif);
		cg.emit_label(label_else);
		auto else_result=else_block->compile(cg,sc);
		if (else_result.is_valid())else_result=cg.load(else_result,0);
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
		auto ifblock=f_compile_body();//body->compile(cg,sc);
		if (ifblock.is_valid()) ifblock=cg.load(ifblock);
		cg.emit_branch(label_endif);
		cg.emit_label(label_endif);
		return CgValue();
	}
}

CgValue CodeGen::emit_if_sub(Node* if_node, Scope* s, function<CgValue()> fcond, function<CgValue()> fbody, Expr* else_block){
	return emit_if_llvm(*this,if_node,if_node->get_scope(), fcond,fbody,else_block);
}
CgValue CodeGen::emit_if(Node* if_node, Expr* cond, Expr* body, Expr* else_block){
	// this intermediate looks redundant but its' for swapping in C backend later..
	auto sc=if_node->get_scope();
	return this->emit_if_sub(if_node,sc , [&]()->CgValue{return cond->compile(*this,sc);}, [&]()->CgValue{return body->compile(*this,sc);}, else_block);
}

/*
CgValue CodeGen::emit_if_sub(Node* if_node, Scope* s, std::function<CgValue()> fcond, function<CgValue()> fbody, Expr* else_block){
	return emit_if_llvm(*this,if_node,if_node->get_scope(), fcond,fbody,else_block);
}
CgValue CodeGen::emit_if(Node* if_node, Expr* cond, Expr* body, Expr* else_block){
	// this intermediate looks redundant but its' for swapping in C backend later..
	auto sc=if_node->get_scope();
	return this->emit_if_sub(if_node,sc , [&]()->CgValue{return cond->compile(*this,sc);}, [&]()->CgValue{return body->compile(*this,sc);}, else_block);
}*/


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
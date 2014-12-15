#include "ast.h"
#include "compile.h"
#include "exprblock.h"
#include "exprstructdef.h"
#include "exprfndef.h"

/// compile methods from various types.

// TODO: properly abstract llvm instruction generation to move to llvm api.
using std::function;


CgValue Node::compile_if(CodeGen& cg, Scope* sc){
	if (this)
		return this->compile(cg,sc);
	else
		return CgValueVoid();
}


void commit_capture_vars_to_stack(CodeGen& cg, CaptureVars* cp){
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
Scope* g_Sc;


void dump_locals(Scope* s){
	for (;s;s=s->parent){
		for (auto v=s->vars; v;v=v->next_of_scope){
			printf("\t;%s:",str(v->name));v->get_type()->dump(-1); printf("%%%s\n",str(v->reg_name));
		}
	}
}

void compile_vtable_data(CodeGen& cg, ExprStructDef* sd, Scope* sc,ExprStructDef* vtable_layout){
	// compile formatted vtable with additional data..
	if (!vtable_layout->is_compiled){
		// the vtable really is just a struct; eventually a macro system could generate
		vtable_layout->compile(cg,sc);
		vtable_layout->is_compiled=true;
	}
	dbg_vtable("compiling vtable for %s\n",sd->name_str());
	cg.emit_global_begin(sd->vtable_name);
	cg.emit_typename(str(vtable_layout->mangled_name));
	cg.emit_struct_begin(16);

	for (auto a:vtable_layout->fields){
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

		for (auto fi: st->fields){
			if (!fi->type())
				return CgValue();
			if (fi->type()->is_typeparam(sc))
				return CgValue();
			if (fi->type()->name>=IDENT){
				if (!fi->type()->struct_def()){
					cg.emit_comment("not compiling %s, it shouldn't have been instanced-see issue of partially resolving generic functions for better type-inference, we're getting these bugs: phantom initiated structs. must figure out how to mark them properly",str(this->get_mangled_name()));
					return CgValue();
				}
			}
		};
		// instantiate the vtable
		// todo: step back thru the hrc to find ov[i]errides
		if (this->vtable)
			compile_vtable_data(cg, this,sc, this->vtable);

		cg.emit_struct_def_begin(st->get_mangled_name());
		for (auto fi: st->fields){
			cg.emit_type(fi->type(), false);
		};
		cg.emit_struct_def_end();
	}
	return CgValue();	// todo: could return symbol? or its' constructor-function?
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
			cg.emit_alloca_array_type(r, t, t->next->name,vt->alignment());
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

CgValue ExprIf::compile(CodeGen& cg,Scope*sc){
	// todo - while etc can desugar as for(;cond;)body, for(){ body if(cond)break}
	return cg.emit_if(this, this->cond, this->body, this->else_block);
}


CgValue ExprFor::compile(CodeGen& cg, Scope* outer_sc){
	return cg.emit_for(this, this->init,this->cond, this->incr, this->body, this->else_block);
}

CgValue compile_function_call(CodeGen& cg, Scope* sc,CgValue recvp, Expr* receiver, ExprBlock* e){
	// [3.1]evaluate arguments
	vector<CgValue> l_args;
	
	// process function argumetns & load
	if (receiver){
		auto recr=receiver->compile(cg,sc);
		l_args.push_back(  cg.load(recr,recr.type) );
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
		l_args.push_back(cg.load(reg,arg->type()));
	}
	
	//[3.2] evaluate call object..
	auto call_fn=e->get_fn_call();
	cg.emit_comment("fncall %s", call_fn?str(call_fn->name):e->call_expr->name_str());
	
	// [3.3] argument conversions..
	auto coerce_args=[&](Type* fn_type){
		// todo - should not be needed
	fn_type->resolve(sc, nullptr, 0);
	auto ai=0;
#if DEBUG>=2
	fn_type->dump(-1);newline(0);
#endif
	auto fn_arg=fn_type->fn_args_first();
	for (auto i=0; fn_arg; i++,fn_arg=fn_arg->next){
#if DEBUG>=2
		dbprintf("arg %d \n", i);fn_arg->dump(-1);newline(0);
#endif
		auto ae=i==0&&receiver?receiver:e->argls[i-(receiver?1:0)];
		auto r=cg.emit_conversion(ae,l_args[i], fn_arg,sc);
		l_args[i]=r;
	}
	};
	
	auto l_emit_arg_list=[&](CgValue env_ptr){
		cg.emit_args_begin();
		if(env_ptr.is_valid()){
			cg.emit_type_operand(env_ptr);
		}
		for (auto a: l_args){
			cg.emit_type_operand(a);
		}
		cg.emit_args_end();
	};
	
	//[3.4] make the call..
	if (e->call_expr->is_function_name()) {
		auto fn_name=e->call_expr->name;
		ExprStructDef* vts=nullptr;
		ArgDef* vtable_fn=nullptr;
		CgValue vtable;
		if (receiver) {
			// lookup types to see if we have a vcall.
			ASSERT(recvp.type->is_pointer() && "haven't got auto-ref yet for receiver in a.foo(b) style call\n");
			//receiver->dump(-1); newline(0);
			auto vtable_name=__VTABLE_PTR;
			auto structdef=recvp.type->get_struct_autoderef();
			auto vtf=structdef->try_find_field(vtable_name);
			if (vtf) {
				vts=vtf->type()->get_struct_autoderef();
			}
			dbg_vcall("receiver: %s %s .%s\n",str(receiver->name),str(receiver->type()->name), str(e->call_expr->name));
			dbg_vcall("vtbl=%p\n",vtf);
			dbg_vcall("vtable struct=%p\n",vts);
		}
		if (vts) {
			dbg_vcall("looks like a vcall %s\n",vts, str(vts->name));
			vtable_fn=vts->try_find_field(fn_name);
		}
		if (vtable_fn) {
			// we have a vcall, so now emit it..
			// load the vtable
			dbg_vcall("emit vcall %p %s.%s\n",vts, str(vts->name),str(vtable_fn->name_str()));
			auto vtbl=cg.emit_getelementref(recvp,__VTABLE_PTR);
			auto function_ptr=cg.emit_getelementval(vtbl,fn_name);
			coerce_args(function_ptr.type);
			cg.emit_call_begin(function_ptr);
			l_emit_arg_list(CgValue());
			return cg.emit_call_end();

		} else {
			//[3.3.1] Direct Call
			coerce_args(call_fn->type());
			cg.emit_call_begin(CgValue(call_fn));
			l_emit_arg_list(CgValue());
			return cg.emit_call_end();
		}
	} else {
		//[3.3.2] Indirect Call... Function Object
		auto fn_obj = e->call_expr->compile(cg, sc);
		auto call_t=e->call_expr->type();
		coerce_args(call_t);
		if (fn_obj.type->is_closure()){
			//[.1] ..call Closure (function,environment*)
			auto fn_ptr=cg.emit_getelementval(fn_obj,0,0,call_t);
			auto envptr = cg.emit_getelementval(fn_obj,0,1,cg.i8ptr());
			cg.emit_call_begin(fn_ptr);
			l_emit_arg_list(envptr);
			return cg.emit_call_end();
		}else{
			//[.2] ..Raw Function Pointer
			cg.emit_call_begin(fn_obj.load(cg));
			l_emit_arg_list(CgValue());
			return cg.emit_call_end();
		}
	}

	return CgValue();
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
		
		if (opname==DECLARE_WITH_TYPE){ // do nothing-it was sema'sjob to create a variable.
			ASSERT(sc->find_scope_variable(e->lhs->name));
			return  CgValue(lhs_v);
		}
		else if(opname==AS) {
			// if (prim to prim) {do fpext, etc} else..
			return cg.emit_cast(lhs,e);
		}
		else
		if (opname==LET_ASSIGN){// := Let-Assign *must* create a new variable,infer type.
			return lhs.store(cg,rhs);
		}
		else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_RHS)  && opname==ASSIGN){
			auto r=cg.emit_conversion(e, rhs, e->type(),sc);
			return lhs.store(cg,r);
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
					auto st=b->call_expr->type()->get_struct_autoderef();
					if (st->vtable){
						auto vtref=cg.emit_getelementref(reg,__VTABLE_PTR);
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
		if (opname==DECLARE_WITH_TYPE){ // do nothing-it was sema'sjob to create a variable.
			auto lhs_v=sc->find_scope_variable(e->lhs->name);
			return  CgValue(lhs_v);
		}

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
			auto r=dst.store(cg,rvalue.load(cg));
			if (r.type==struct_val.type)
				struct_val=r; // mutate by insertion if its 'in-reg'
		}
		// TODO: CLARIFY WHY... alloca returns 'ref' whilst struct-initializer gives a 'ptr'?
		// eliminate this, its' messy. 'force_dst' should be a CgValue.
		if (force_dst) {
			struct_val.reg=force_dst;
			struct_val.addr=0;
		}
		return struct_val;
	}
	// [2] Operator
	//[3] ARRAY ACCESSs
	else if (auto ar=n->is_subscript()){
		auto expr=ar->call_expr->compile(cg,sc);// expression[index]
		auto index=ar->argls[0]->compile(cg,sc);
		/// TODO , this is actually supposed to distinguish array[ n x T ] from pointer *T case
		// TODO: abstract this into codegen -getelementref(CgValue ptr,CgValue index);
		return cg.emit_get_array_elem_ref(expr, index);
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
	return CgValue(0,this,0);	// propogate a type into compiler interface
}
CgValue CaptureVars::compile(CodeGen& cg, Scope* outer_scope){
	auto cp=this;
	cg.emit_struct_def_begin(cp->tyname());
	decltype(cp->vars->capture_index) i=0;
	for (auto v=cp->vars;v;v=v->next_of_capture,i++){
		cg.emit_type(v->type());
		v->capture_index=i;
	}
	cg.emit_struct_def_end();
	cp->type() = new Type(cp->capture_by, PTR,cp->tyname());
	cp->type()->sub->set_struct_def((ExprStructDef*) cp);
	return CgValue(this);
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
		cp->compile(cg,outer_scope);
	}

	cg.emit_global_fn_ptr(fn_node->type(),fn_node->get_mangled_name());
	if (!fn_node->get_type() && fn_node->fn_type && fn_node->scope ){
		error(fn_node,"function name %s %s %p %p %p %p", str(fn_node->name),str(fn_node->get_mangled_name()), fn_node->instance_of, fn_node->get_type(), fn_node->fn_type, fn_node->scope);
		ASSERT(0 && "function must be resolved to compile it");
		return CgValue();
	}
	cg.emit_comment("fn %s (%p) :- ins=%p of %p ", str(fn_node->name),fn_node, fn_node->instances, fn_node->instance_of);

	auto scope=fn_node->scope;
	
	cg.emit_function_signature(fn_node,EmitDefinition);
 	cg.emit_nest_begin("{\n");
	if (fn_node->instance_of!=nullptr){
		cg.emit_comment("compiling generic fn body");
	}
	emit_local_vars(cg, fn_node->body, fn_node, scope);
	auto rtn=fn_node->get_return_value();

	if (fn_node->fn_type->name==CLOSURE){
		auto cp=fn_node->my_capture;
		if (cp){
			cp->reg_name=cp->get_reg_new(cg);
			cp->reg_name =cg.emit_cast_reg(__ENV_I8_PTR, cg.i8ptr(),cp->type()).reg;
		}
	}

	cg.emit_return(fn_node->body->compile(cg,scope));
	cg.emit_nest_end("}\n");
	cg.curr_fn=0;
	return CgValue(fn_node);
}

CgValue Node::compile(CodeGen& cg, Scope* sc){
	error(this,"compile not implemented for %s",this->kind_str());
	return CgValue();
}


CgValue EnumDef::compile(CodeGen &cg, Scope *sc){
	return CgValue();
}

CgValue compile_match_arm(CodeGen& cg, Scope* sc,Expr* match_expr, CgValue match_val, MatchArm* arm){
	if (!arm->next){
		// error check. we just ignore condition. error should assert its non exhaustive
		arm->compile_bind(cg,sc,match_expr,match_val);
		return arm->body->compile(cg,sc);
	}
	auto armsc=arm->get_scope();
	return cg.emit_if_sub(
				arm,
				sc,
				[&]{return arm->compile_check(cg,armsc, match_expr, match_val);},
				[&]{arm->compile_bind(cg,armsc,match_expr,match_val);return arm->body->compile(cg,armsc);},
				arm->next);
}

CgValue MatchArm::compile_check(CodeGen &cg, Scope *sc, Expr *match_expr, CgValue match_val){
	// emit a condition to check if the runtime value 'match_val' fits this pattern.
	ASSERT(0&&"TODO");
	return CgValue();
}
CgValue MatchArm::compile_bind(CodeGen &cg, Scope *sc, Expr *match_expr, CgValue match_val)	{
	// extract local variables from the pattern...
	ASSERT(0&&"TODO");
	return CgValue();
}

CgValue
ExprMatch::compile(CodeGen& cg, Scope* sc){
	// TODO - dedicated with one set of phi-nodes at the end.
	// this is RETARDED!!!
	// TODO - turn some cases into binary-chop (no 'if-guards' & single value test)
	// TODO - turn some cases into vtable.
	auto match_val = this->expr->compile(cg,sc);
	return compile_match_arm(cg, sc, this->expr, match_val, this->arms);
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
	else if (auto sd=t->get_struct_autoderef()){
		name_mangle_append_segment(dst,size,str(sd->get_mangled_name()));
		for (auto& it:sd->instanced_types){
			dbg_mangle(" %s\n",it->name_str());
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
//			const char* name=getString(l->name);
			l->llvm_strlen=cg.emit_global_string_literal(l->name, l->as_str());
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




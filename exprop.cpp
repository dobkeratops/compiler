#include "exprop.h"

void ExprOp::translate_tparams(const TParamXlat& tpx){
	lhs->translate_typeparams_if(tpx);
	rhs->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}

void dump_field_info(Node* n,Scope* sc){
}

void ExprOp::verify() {
	verify_expr_op(this);
	if (lhs) lhs->verify();
	if (rhs) rhs->verify();
}
void ExprOp::find_vars_written(Scope* s, set<Variable *> &vs) const{
	auto flags=operator_flags(name);
	lhs->find_vars_written_if(s,vs);
	rhs->find_vars_written_if(s,vs);
	if (flags&WRITE_LHS){
		if (auto vname=this->lhs->as_ident()){
			if (auto var=s->find_variable_rec(vname->name)){
				vs.insert(var);
			}
		}
	}
	if (flags&WRITE_RHS){
		if (auto vname=this->rhs->as_ident()){
			if (auto var=s->find_variable_rec(vname->name)){
				vs.insert(var);
			}
		}
	}
}

void ExprOp::dump(PrinterRef depth) const {
	if (!this) return;
	newline(depth);dbprintf("(");
	auto id=this->name;
	if (lhs) {lhs->dump(depth+1);}
	
	newline(depth);if (get_fn())dbprintf("fn");print_tok(id);
	
	if (rhs) {rhs->dump(depth+1);}
	newline(depth);dbprintf(")");
	if (get_type()) {dbprintf(":");get_type()->dump(-1);};
}
void ExprOp::recurse(std::function<void(Node*)>& f){
	lhs->recurse(f);
	rhs->recurse(f);
	type()->recurse(f);
}
Node* ExprOp::clone() const {
	return (Node*) new ExprOp(this->name,this->pos, (Expr*) this->lhs->clone_if(), (Expr*) this->rhs->clone_if());
}

ResolveResult ExprOp::resolve(Scope* sc, const Type* desired,int flags) {
	verify_all();
	Type* ret=0;
	auto op_ident=name;
	this->type()->resolve_if(sc,desired,flags);
	//	if (flags) {ASSERT(lhs->def) ;ASSERT(rhs->def);}

	
	// intrinsic operators
	if (op_ident==BREAK){
		if (this->rhs) {
			// break expression..
			rhs->resolve_if(sc,desired,flags);
			auto loop = sc->current_loop(this->lhs?getNumberInt(lhs->name):1);
			if (flags & R_FINAL){
				if (!loop && flags&R_FINAL) {
					error(this,"break without loop");
				}
				auto else_block=loop->loop_else_block();
				if (rhs && !else_block){
					error(this,"break <expression> requires else block with alternate return expression, same type. Use 'break break <expr>' for nesting");
				}
				if (else_block){
					propogate_type_refs(flags,(Node*)this,rhs->type_ref(),else_block->type_ref());
				}
			}
			propogate_type_refs(flags,(Node*)this, rhs->type_ref(),loop->type_ref());
			propogate_type_refs(flags,(Node*)this,this->type_ref(),this->rhs->type_ref());
		}
		return propogate_type_fwd(flags, this, desired, this->type_ref());
	}
	if (op_ident==FIELD_ASSIGN){
		error(this,"field-assign operator not handled, should only appear in struct-initializer (TODO: keyword args)");
	}
	if (op_ident==ASSIGN || op_ident==LET_ASSIGN || op_ident==DECLARE_WITH_TYPE) {
		if (op_ident==LET_ASSIGN){
			ASSERT(this->lhs && this->rhs);
			resolved|=rhs->resolve_if(sc,desired,flags);
			dbg(lhs->dump(0));dbg(printf(".let="));
			dbg(rhs->dump(0));dbg(newline(0));
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			if (desired) {
				desired->dump(-1);
			}
			auto rhs_t = rhs->get_type();
			if (lhs->as_ident()){
				auto new_var=sc->create_variable(this,vname,Local);
				lhs->set_def(new_var);
				if (rhs_t){
					lhs->set_type(rhs_t);
					this->set_type(rhs_t);
				}
			}
			
			//new_var->force_type_todo_verify(rhs_t);
			resolved|=lhs->resolve_if(sc,rhs->type(),flags);
			propogate_type_fwd(flags, this, desired, lhs->type_ref());
			return 	propogate_type_fwd(flags, this, desired, this->type_ref());
		}
		else if (op_ident==DECLARE_WITH_TYPE){ // create a var, of given type,like let lhs:rhs;
			const Type* tt=rhs?rhs->as_type():nullptr; if (!tt) tt=this->type(); if (!tt) tt=desired;
			resolved|=lhs->resolve_if(sc, tt,flags);
			if (!lhs->is_ident())
				return propogate_type_refs(flags, this, lhs->type_ref(),type_ref());
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			// todo: get this in the main parser
			auto lhsi=expect_cast<ExprIdent>(lhs);
			//			auto v=sc->find_variable_rec(this->argls[0]->name);
			auto v=sc->get_or_create_scope_variable(this,lhsi->name,Local);
			auto t=v->get_type();
			if (rhs){
				resolved|=rhs->resolve_if(sc,desired,flags);
				t=expect_cast<Type>(rhs);
				v->set_type(t);
			}
			lhs->set_def(v);
			if (t){
				if (t->name>=IDENT && !t->sub) {
					t->set_struct_def(sc->find_struct(t));
				}
			}
			if (auto t=v->get_type()) {
				sc->try_find_struct(t);// instantiate
			}

			return propogate_type_refs(flags, this, v->type_ref(),type_ref());
		}
		else if (op_ident==ASSIGN){
			ASSERT(this->lhs && this->rhs);
			dbg(::dump(this->lhs->type(),this->rhs->type()));
			resolved|=rhs->resolve_if(sc,nullptr,flags);

			resolved|=lhs->resolve_if(sc,desired,flags);
			dbg(::dump(this->lhs->type(),this->rhs->type());)

			// get coersion right..
			propogate_type_refs(flags,(Node*)this, rhs->type_ref(), lhs->type_ref());
			propogate_type_refs(flags,(Node*)this, lhs->type_ref(), this->type_ref());
			dbg(::dump(this->lhs->type(),this->rhs->type());)
			return propogate_type_fwd(flags,this, desired, type_ref());
			//propogate_type(flags,this, type_ref(),rhs->type_ref());
			//return propogate_type(flags, this, type_ref(),lhs->type_ref());
		} else{
			ASSERT(0);
			return resolved;
		}
	}
	else if (op_ident==COLON){ // TYPE ASSERTION
		// todo: get this in the main parser
		ASSERT(rhs->as_ident()); // todo- not just that
		auto tname=rhs->name;
		auto v=sc->find_variable_rec(lhs->name);
		if (!v->get_type()){
			v->set_type((const Type*)rhs);
			ASSERT(dynamic_cast<Type*>(rhs))
		}
		lhs->set_def(v);
		return propogate_type_refs(flags, this, v->type_ref(),type_ref());
	} else if (op_ident==AS){
		resolved|=this->lhs->resolve_if(sc,nullptr,flags);
		if (this->rhs->name==PLACEHOLDER) {
			this->rhs->set_type(desired);
			this->set_type(desired);
			return resolved;
		} else {
			if (auto t=this->rhs->type())
				this->set_type(t);
			return propogate_type_fwd(flags,this,desired);
		}
	}
	else if (op_ident==NEW ){
//		if (desired && !this->get_type()){
//			this->set_type(desired);
//		}
		resolved|=rhs->resolve_operator_new(sc,desired,flags, this);
		return propogate_type_fwd(flags,this, desired, this->type_ref());
	}
	else if (op_ident==DELETE ){
		return rhs->resolve_if(sc,nullptr,flags);
	}

	else if (op_ident==DOT || op_ident==ARROW) {
		resolved|=lhs->resolve_if(sc, 0,flags);//type doesn't push up- the only info we have is what field it needs
		auto t=lhs->type();
		//		dbprintf("resolve %s.%s   lhs:",getString(lhs->name),getString(rhs->name));if (t) t->dump(-1);dbprintf("\n");
		
		// TODO: assert that lhs is a pointer or struct? we could be really subtle here..
		verify_all();
		if (t) {
			t=t->deref_all();
			// now we have the elem..
			verify_expr_op(this);
			//			verify_expr_ident(rhs);
			//			ASSERT(rhs->as_ident());
			
			if (isNumStart(*str(rhs->name),0)){
				auto fi=getNumberInt(rhs->name);
				if (auto t=this->lhs->type()){
					auto elem_t = t->get_elem(fi);
					propogate_type_expr_ref(flags, this, elem_t);
					return propogate_type_fwd(flags,this, desired, this->type_ref());
				}
			}
			else if (auto field_name=rhs->as_ident()){
				if (auto st=sc->find_struct_of(lhs)){
					if (auto f=st->find_field(rhs)){
						propogate_type_refs(flags,this, f->type_ref(), this->type_ref());
						ret=f->type();
						return propogate_type_refs(flags,this, ret,this->type_ref());
					}
				}
				if (flags&R_FINAL) {
					dump_field_info(this,sc);
				}
				// no good.
				return ResolveResult();
			} else if (auto call=dynamic_cast<ExprCall*>(rhs)){
				auto method_name=call->call_expr->name;
				// TODO - should there be a dedicated MethodCall node?
				// should the parser just accumulate the receiver into the argument list already?
				//				dbprintf("method call: %s\n",str(method_name));
				call->resolve_call_sub(sc, desired, flags, lhs);
				return propogate_type_refs(flags,this,call->type(),this->type_ref());
			} else {
				if (flags & R_FINAL){
					error_begin(this,"dot operator not call or field acess %s.%s %d", t->name_str(), this->rhs->name_str(), is_number(this->rhs->name));
					error(this,"cant find struct %s", t->name_str());
					error_end(this);
				}
			}
		}
		verify_all();
		return ResolveResult(INCOMPLETE);
	}
	// remaining types are assumed overloadable.
	//look for overload - infer fowards only first like C++
	if (find_overloads(sc,desired,flags)){
		return propogate_type_fwd(flags, this, desired, this->type_ref());
	}


	if (op_ident==ADDR){  //result=&lhs
		// todo: we can assert give type is one less pointer, if given
		ASSERT(!lhs && rhs);
		Type* dt=nullptr;
		if (desired){
			if (!(desired->name==PTR)) {
				error_begin(this,"taking adress, infered output isn't a ptr\n");
				warning(desired->get_origin(),"infered from here: ");
				desired->dump(-1);error_newline();
				error_end(this);
			}
			dt=desired->sub;
		}
		auto ret=rhs->resolve_if(sc,dt,flags|R_PUT_ON_STACK);
		if (!this->get_type() && rhs->type()){
			auto ptr_type=new Type(this,PTR,(Type*)rhs->type()->clone());
			this->set_type(ptr_type);
			return propogate_type_fwd(flags,this, desired,ptr_type);
		}
		return ret;
	}
	else if (op_ident==DEREF){ //result=*rhs
		// todo: we can assert give type is one less pointer, if given
		resolved|=rhs->resolve_if(sc,0,flags);
		// todo: its' a typeparam constraint.  ptr[desired]==argls[0]
		if (!this->get_type() && rhs->type()){
			//			if (ret.type->name!=PTR) {
			//				this->dump(0);
			//				rhs->dump(0);
			//				ret.type->dump(0);
			//			}
			//			ASSERT(ret.type->name==PTR);
			if (rhs->type()->name!=PTR ){
				if (flags & R_FINAL){
					error_begin(this,"pointer expected for deref\n");
					rhs->type()->dump(-1);
					error_end(this);
				}
			} else {
				this->set_type(rhs->type()->sub);
			}
			return propogate_type_fwd(flags,this, desired, this->type_ref());
		}
		else return resolved;
	}
	
	
	
	if (is_condition(op_ident)){
		lhs->resolve_if(sc,rhs->type_ref(),flags); // comparisions take the same type on lhs/rhs
		rhs->resolve_if(sc,lhs->type_ref(),flags);
		propogate_type_refs(flags,(Node*)this, lhs->type_ref(),rhs->type_ref());
		::verify(lhs->get_type());
		::verify(rhs->get_type());
		if (!this->get_type()){
			this->set_type(new Type(this,BOOL));
		};
		// TODO: actually we want to ensure the result *converts* to bool
		// compares might not return bool, they just need an operator(bool)
		return propogate_type_fwd(flags,this,desired,this->type_ref());
	}
	else {
		// regular operator
		// TODO propogate types for pointer-arithmetic - ptr+int->ptr   int+ptr->ptr  ptr-ptr->int
		// defaults to same types all round.
		resolved|=lhs->resolve_if(sc,desired,flags);
		resolved|=rhs->resolve_if(sc,desired,flags&!R_PUT_ON_STACK);
		propogate_type_refs(flags,this, lhs->type_ref(),type_ref());
		propogate_type_refs(flags,this, rhs->type_ref(),type_ref());

		if (flags & R_FINAL){
			if (!(lhs->type()->is_number() && rhs->type()->is_number())){
				error_begin(this,"operator %s needs primitive args,given:",this->name_str());
				info(lhs,lhs->type());
				info(rhs,rhs->type());
				error_end(this);
			}
		}

		return propogate_type_fwd(flags,this, desired, type_ref());
	}
}

bool ExprOp::find_overloads(Scope *sc, const Type *desired, int flags){
	if (!name)
		return false;
	if (this->get_fn())
		return true;
	resolved|=lhs->resolve_if(sc,nullptr,flags&(~R_FINAL));
	resolved|=rhs->resolve_if(sc,nullptr,flags&(~(R_PUT_ON_STACK|R_FINAL)));
 	if (!(lhs->type_if()||rhs->type_if()||desired)){
		return false;		// no info to go on.
	}
#if DEBUG >=2
	dbprintf("considering overload for:-\n");
	dbprintf("lhs="); lhs->dump_if(-1);newline(0);
	dbprintf("rhs="); rhs->dump_if(-1);newline(0);
	dbprintf("\n");
#endif
	// at least one must be a custom type, like C++.
	int num_non_prim=0;
	if (this->lhs)if (lhs->type()->is_userdefined()) num_non_prim++;
	if (this->rhs)if (rhs->type()->is_userdefined()) num_non_prim++;
	if (!num_non_prim)
		return false;
	vector<Expr*> args;if (lhs)args.push_back(lhs);if (rhs)args.push_back(rhs);
	auto fnd=sc->find_fn(this->name, this, args,desired,flags&(~R_FINAL));
	if (fnd) {
#if DEBUG >=2
		dbprintf("\nusing overload %s( ",str(this->name));this->lhs->type()->dump_if(-1);dbprintf(" ");this->rhs->type()->dump_if(-1);dbprintf(" )\n");
#endif
		this->set_fn(fnd);
		//override any type that might have been infered by intrinsic operators.
		this->set_type(fnd->return_type());
		return true;
	}
	return false;
}

CgValue ExprOp::compile(CodeGen &cg, Scope *sc, CgValue) {
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
	if (get_fn()){
		return compile_operator_overload(cg,sc);
	}
	if (opname==DOT || opname==ARROW){
		if (auto c=rhs->as_block()){
			// compile method call
			return compile_function_call(cg,sc,CgValue(),e->lhs,c);
		}
		auto lhsv=e->lhs->compile(cg,sc);
		// auto-deref is part of language semantics, done here..
		while (lhsv.type->num_pointers()+(lhsv.addr?1:0) > 1){
			cg.emit_comment("dot: auto deref from level=%d",lhsv.type->num_pointers()+(lhsv.addr?1:0));
			lhsv = lhsv.deref_for_dot(cg,0);
		}
		if (isNumStart(*str(rhs->name),0)){
			return lhsv.get_elem_index(cg, getNumberInt(rhs->name));
		}
		if (rhs->as_ident()) {
			return lhsv.get_elem(cg,e->rhs,sc);
		}
		error(this,"unhandled case, dot operator");
		return CgValue();
	}
	else if (opname==BREAK){
		cg.emit_comment("BREAK EXPRESSION");
		
		cg.emit_break(rhs->compile(cg,sc),lhs?getNumberInt(lhs->name):1);
		return CgValue();
	}
	else if (opname==CONTINUE){
		cg.emit_comment("CONTINUE");
		
		cg.emit_continue(lhs?getNumberInt(lhs->name):1);
		return CgValue();
	}
	else if (e->lhs && e->rhs){
		auto rhs_v=e->rhs->compile(cg,sc);
		auto lhs_var=sc->find_variable_rec(e->lhs->name);
		auto outname=lhs_var?lhs_var->name:opname;
		
		if (opname==DECLARE_WITH_TYPE){ // do nothing-it was sema'sjob to create a variable.
//			auto lhs_v=e->lhs->compile(cg,sc);
//			ASSERT(sc->find_scope_variable(e->lhs->name));
//			return  CgValue(lhs_var);
			return e->lhs->compile(cg,sc,CgValue());
		}
		else if(opname==AS) {
			// if (prim to prim) {do fpext, etc} else..
			auto lhs_v=e->lhs->compile(cg,sc);
			return cg.emit_cast(lhs_v,e);
		}
		else
			if (opname==LET_ASSIGN){// := Let-Assign *must* create a new variable,infer type.
				auto lhs_v=e->lhs->compile(cg,sc,rhs_v);
				if (lhs->is_ident()){
					return lhs_v.store(cg,rhs_v);
				} else return lhs_v;
				//return e->lhs->compile(cg,sc,rhs_v);
			}
			else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_RHS)  && opname==ASSIGN){
				auto lhs_v=e->lhs->compile(cg,sc);
				auto r=cg.emit_conversion(e, rhs_v, e->type(),sc);
				return lhs_v.store(cg,r);
			}
			else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_LHS|READ_RHS) ){
				auto lhs_v=e->lhs->compile(cg,sc);
				auto result=cg.emit_instruction(opname,t?t:rhs_v.type, 0,lhs_v,rhs_v);
				return lhs_v.store(cg,result);
			}else {
				auto lhs_v=e->lhs->compile(cg,sc);
				// RISClike 3operand dst=op(src1,src2)
				auto r=cg.emit_instruction(opname,e->get_type(),0,lhs_v,rhs_v);
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
			// LHS is for 'placement-new'.
			return rhs->compile_operator_new(cg,sc,this->type(),lhs);
		}
		else if (opname==DELETE){
			auto x=rhs->compile(cg,sc);
			cg.emit_free(x,1);	///TODO array types, rustlike DST??
			
			/// TODO call destructor here.
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

CgValue ExprOp::compile_operator_overload(CodeGen& cg,Scope* sc){
	auto call_fn=const_cast<ExprFnDef*>(this->get_fn());

	CgValue lhsa,rhsa;
	if (this->lhs)
		lhsa=cg.emit_conversion(lhs, lhs->compile(cg,sc), call_fn->args[0]->type(),sc);
	if (this->rhs)
		rhsa=cg.emit_conversion(rhs, rhs->compile(cg,sc), call_fn->args[1]->type(),sc);

	cg.emit_call_begin(CgValue(call_fn));
	cg.emit_args_begin();
	if (lhs)cg.emit_type_operand(lhsa);
	if (rhs)cg.emit_type_operand(rhsa);
	cg.emit_args_end();
	cg.compile_destructor(sc,lhsa,false);
	cg.compile_destructor(sc,rhsa,false);
	auto ret_val=cg.emit_call_end();
	ret_val.rvalue=true;
	return ret_val;

}



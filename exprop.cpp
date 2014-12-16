#include "ast.h"
#include "exprop.h"
#include "scope.h"
#include "exprstructdef.h"
#include "exprblock.h"
#include "codegen.h"
#include "lexer.h"
void ExprOp::translate_typeparams(const TypeParamXlat& tpx){
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
		if (auto vname=dynamic_cast<ExprIdent*>(this->lhs)){
			if (auto var=s->find_variable_rec(vname->name)){
				vs.insert(var);
			}
		}
	}
	if (flags&WRITE_RHS){
		if (auto vname=dynamic_cast<ExprIdent*>(this->rhs)){
			if (auto var=s->find_variable_rec(vname->name)){
				vs.insert(var);
			}
		}
	}
}

void ExprOp::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("(");
	auto id=this->name;
	if (lhs) {lhs->dump(depth+1);}
	
	newline(depth);print_tok(id);
	
	if (rhs) {rhs->dump(depth+1);}
	newline(depth);dbprintf(")");
	if (get_type()) {dbprintf(":");get_type()->dump(-1);};
}

Node* ExprOp::clone() const {
	return (Node*) new ExprOp(this->name,this->pos, (Expr*) this->lhs->clone_if(), (Expr*) this->rhs->clone_if());
}

ResolvedType ExprOp::resolve(Scope* sc, const Type* desired,int flags) {
	verify_all();
	Type* ret=0;
	auto op_ident=name;
	if (this->type()) this->type()->resolve(sc,desired,flags);
	//	if (flags) {ASSERT(lhs->def) ;ASSERT(rhs->def);}
	if (op_ident==BREAK){
		if (this->rhs) {
			// break expression..
			rhs->resolve(sc,desired,flags);
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
					propogate_type(flags,(Node*)this,rhs->type_ref(),else_block->type_ref());
				}
			}
			propogate_type(flags,(Node*)this, rhs->type_ref(),loop->type_ref());
			propogate_type(flags,(Node*)this,this->type_ref(),this->rhs->type_ref());
		}
		return propogate_type_fwd(flags, this, desired, this->type_ref());
	}
	
	if (op_ident==ASSIGN || op_ident==LET_ASSIGN || op_ident==DECLARE_WITH_TYPE) {
		if (op_ident==LET_ASSIGN){
			ASSERT(this->lhs && this->rhs);
			rhs->resolve(sc,desired,flags);
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			if (desired) {
				desired->dump(-1);
			}
			auto rhs_t = rhs->get_type();
			auto new_var=sc->create_variable(this,vname,Local);
			lhs->set_def(new_var);
			new_var->force_type_todo_verify(rhs_t);
			lhs->set_type(rhs_t);
			this->set_type(rhs_t);
			propogate_type_fwd(flags, this, desired, lhs->type_ref());
			return 	propogate_type_fwd(flags, this, desired, this->type_ref());
		}
		else if (op_ident==DECLARE_WITH_TYPE){ // create a var, of given type,like let lhs:rhs;
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			// todo: get this in the main parser
			auto lhsi=expect_cast<ExprIdent>(lhs);
			//			auto v=sc->find_variable_rec(this->argls[0]->name);
			auto v=sc->get_or_create_scope_variable(this,lhsi->name,Local);
			auto t=v->get_type();
			if (rhs){
				rhs->resolve(sc,desired,flags);
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
			return propogate_type(flags, this, v->type_ref(),type_ref());
		}
		else if (op_ident==ASSIGN){
			ASSERT(this->lhs && this->rhs);
			propogate_type_fwd(flags,this, desired, type_ref());
			auto rhs_t=rhs->resolve(sc,desired,flags);
			auto lhs_t=lhs->resolve(sc,desired,flags);		// might assign to struct-field, ...
			propogate_type(flags,this, rhs->type_ref(), lhs->type_ref());
			propogate_type(flags,this, type_ref(),rhs->type_ref());
			return propogate_type(flags, this, type_ref(),lhs->type_ref());
		} else{
			ASSERT(0);
			return ResolvedType();
		}
	}
	else if (op_ident==COLON){ // TYPE ASSERTION
		// todo: get this in the main parser
		ASSERT(dynamic_cast<ExprIdent*>(rhs)); // todo- not just that
		auto tname=rhs->name;
		auto v=sc->find_variable_rec(lhs->name);
		if (!v->get_type()){
			v->set_type((const Type*)rhs);
			ASSERT(dynamic_cast<Type*>(rhs))
		}
		lhs->set_def(v);
		return propogate_type(flags, this, v->type_ref(),type_ref());
	} else if (op_ident==AS){
		this->lhs->resolve(sc,nullptr,flags);
		if (this->rhs->name==PLACEHOLDER) {
			this->rhs->set_type(desired);
			this->set_type(desired);
			return ResolvedType(this->type(),ResolvedType::COMPLETE);
		} else {
			if (auto t=this->rhs->type())
				this->set_type(t);
			return propogate_type_fwd(flags,this,desired);
		}
	}
	else if (op_ident==DOT || op_ident==ARROW) {
		auto lhs_t=lhs->resolve(sc, 0,flags);//type doesn't push up- the only info we have is what field it needs
		auto t=lhs_t.type;
		//		dbprintf("resolve %s.%s   lhs:",getString(lhs->name),getString(rhs->name));if (t) t->dump(-1);dbprintf("\n");
		
		// TODO: assert that lhs is a pointer or struct? we could be really subtle here..
		verify_all();
		if (t) {
			t=t->deref_all();
			// now we have the elem..
			verify_expr_op(this);
			//			verify_expr_ident(rhs);
			//			ASSERT(rhs->as_ident());
			
			if (isNumStart(*str(rhs->name))){
				auto fi=getNumberInt(rhs->name);
				if (auto t=this->lhs->type()){
					auto elem_t = t->get_elem(fi);
					propogate_type(flags, this, elem_t);
					return propogate_type_fwd(flags,this, desired, this->type_ref());
				}
			}
			else if (auto field_name=rhs->as_ident()){
				if (auto st=sc->find_struct_of(lhs)){
					if (auto f=st->find_field(rhs)){
						propogate_type(flags,this, f->type_ref(), this->type_ref());
						ret=f->type();
						return propogate_type(flags,this, ret,this->type_ref());
					}
				}
				if (flags&R_FINAL) {
					dump_field_info(this,sc);
				}
				// no good.
				return ResolvedType();
			} else if (auto call=rhs->as_block()){
				auto method_name=call->call_expr->name;
				// really we want to desugar this, a.foo(b) is just foo(a,b)
				// but we respect the shape of the AST?
				//				dbprintf("method call: %s\n",str(method_name));
				call->resolve_sub(sc, desired, flags, lhs);
				return propogate_type(flags,this,call->type(),this->type_ref());
			} else {
				if (flags & R_FINAL){
					error_begin(this,"dot operator not call or field acess %s.%s %d", t->name_str(), this->rhs->name_str(), is_number(this->rhs->name));
					error(this,"cant find struct %s", t->name_str());
					error_end(this);
				}
			}
		}
		verify_all();
		return ResolvedType(this->type(),ResolvedType::INCOMPLETE);
	}
	else if (op_ident==ADDR){  //result=&lhs
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
		auto ret=rhs->resolve(sc,dt,flags|R_PUT_ON_STACK);
		if (!this->get_type() && ret.type){
			auto ptr_type=new Type(this,PTR,(Type*)ret.type->clone());
			this->set_type(ptr_type);
			return propogate_type_fwd(flags,this, desired,ptr_type);
		}
		return ret;
	}
	else if (op_ident==DEREF){ //result=*rhs
		// todo: we can assert give type is one less pointer, if given
		auto ret=rhs->resolve(sc,0,flags);
		// todo: its' a typeparam constraint.  ptr[desired]==argls[0]
		if (!this->get_type() && ret.type){
			//			if (ret.type->name!=PTR) {
			//				this->dump(0);
			//				rhs->dump(0);
			//				ret.type->dump(0);
			//			}
			//			ASSERT(ret.type->name==PTR);
			if (ret.type->name!=PTR ){
				if (flags & R_FINAL){
					error_begin(this,"pointer expected for deref\n");
					rhs->type()->dump(-1);
					error_end(this);
				}
			} else {
				this->set_type(ret.type->sub);
			}
			return propogate_type_fwd(flags,this, desired, this->type_ref());
		}
		else return ResolvedType();
	}
	else if (op_ident==NEW ){
		// new struct initializer ->  malloc(sizeof(struct)); codegen struct initializer 'inplace'; ret is ptr[S]
		// can we generalize this:
		//  ident{expr,..} actually means run the init expr in there, like 'with'
		/// todo: generalize inference with wrapper , eg A vs X[B]. use for *t, &t, new t, t[i]
		auto b=rhs->as_block();
		if (!b && flags){
			error(b,"new type[n] or new type{..} expected");
		}
		if (desired && !get_type()){
			this->set_type(desired);
		}
		if (!desired && !get_type() && rhs->get_type()) {
			this->set_type( new Type(this,PTR,(Type*)b->get_type()->clone()) );
		}
		if (get_type())
			propogate_type(flags, (Node*)this, this->get_type()->sub, b->type_ref());
		
		if (rhs->is_subscript()){
			b->call_expr->resolve(sc,get_type()?get_type()->sub:nullptr,flags);
			b->set_type(b->call_expr->get_type());
		}
		else {
			b->resolve(sc, desired?desired->sub:nullptr, flags);
			
		}
		
		return propogate_type_fwd(flags,this, desired, this->type_ref());
	}
	else if (op_ident==DELETE ){
		rhs->resolve(sc,nullptr,flags);
		return ResolvedType();
	}
	else if (is_condition(op_ident)){
		auto lhst=lhs->resolve(sc,rhs->type_ref(),flags); // comparisions take the same type on lhs/rhs
		auto rhst=rhs->resolve(sc,lhs->type_ref(),flags);
		propogate_type(flags,(Node*)this, lhs->type_ref(),rhs->type_ref());
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
		auto lhst=lhs->resolve(sc,desired,flags);
		auto rhst=rhs->resolve(sc,desired,flags&!R_PUT_ON_STACK);
		propogate_type(flags,this, lhst,type_ref());
		propogate_type(flags,this, rhst,type_ref());
		return propogate_type_fwd(flags,this, desired, type_ref());
	}
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
		if (rhs->as_block()){
			// compile method call
			return compile_function_call(cg,sc,e->lhs->compile(cg,sc),e->lhs,e->rhs->as_block());
		}
		auto lhsv=e->lhs->compile(cg,sc);
		// auto-deref is part of language semantics, done here..
		while (lhsv.type->num_pointers()+(lhsv.addr?1:0) > 1){
			cg.emit_comment("dot: auto deref from level=%d",lhsv.type->num_pointers()+(lhsv.addr?1:0));
			lhsv = lhsv.deref_for_dot(cg,0);
		}
		if (isNumStart(*str(rhs->name))){
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




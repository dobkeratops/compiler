#include "ast.h"
#include "exprop.h"
#include "scope.h"
#include "exprstructdef.h"
#include "exprblock.h"

void ExprOp::translate_typeparams(const TypeParamXlat& tpx){
	lhs->translate_typeparams_if(tpx);
	rhs->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
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
					error(this,"break <expression> requires else block with alternate return expression, same type.");
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
			if (auto field_name=rhs->as_ident()){
				if (auto st=sc->find_struct_of(lhs)){
					if (auto f=st->find_field(rhs)){
						ret=f->type();
						return propogate_type(flags,this, ret,this->type_ref());
					}
				}
			} else if (auto call=rhs->as_block()){
				auto method_name=call->call_expr->name;
				// really we want to desugar this, a.foo(b) is just foo(a,b)
				// but we respect the shape of the AST?
				//				dbprintf("method call: %s\n",str(method_name));
				call->resolve_sub(sc, desired, flags, lhs);
				return propogate_type(flags,this,call->type(),this->type_ref());
			} else {
				if (flags & R_FINAL){
					error(this,"dot operator not call or field acess", t->name_str());
					error(this,"cant find struct %s", t->name_str());
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




#include "pattern.h"


void Pattern::recurse(std::function<void(Node*)>& f){
	if (!this) return;
	for (auto p=sub_pat();p;p=p->next)
		p->recurse(f);
}
Node*Pattern::clone() const {
	auto np=new Pattern(pos,name);
	np->next=(Pattern*)next->clone_if();// todo not recursive!! .. but patterns are small.
	np->sub=(Pattern*)sub->clone_if();
	return np;
}
Pattern* Pattern::get_elem(int i){
	auto s=sub_pat();
	for (; s && i>0; s=s->next,i--){}
	return s;
}
const Pattern* Pattern::get_elem(int i) const{
	auto s=sub_pat();
	for (; s && i>0; s=s->next,i--){}
	return s;
}
Name Pattern::as_name()const{
	return this->name;
}
ResolveResult
Pattern::resolve(Scope* sc, const Type* rhs, int flags){
	resolved=COMPLETE;// calls will correct it..
	return this->resolve_with_type(sc,rhs,flags);
}

ResolveResult
Pattern::resolve_with_type(Scope* sc, const Type* rhs, int flags){
	if (!this)
		return ResolveResult(COMPLETE);
	if (this->sub){
		0==0;
	}
	if (this->name==VOID){
		return ResolveResult(COMPLETE);
	}
	if (this->name==EXPRESSION){
		((Node*)(this->sub))->resolve_if(sc,rhs,flags);
		return propogate_type_refs(flags,(Node*)this, this->type_ref(), this->sub->type_ref());
	}
	if (this->name==REF){
		if (rhs&&rhs->name==REF){
			resolved|=sub_pat()->resolve_with_type(sc, rhs->sub, flags);
			this->set_type(new Type(this,REF,sub->type()));
			return resolved;
		}
	}
	if (this->name==PTR){
		if (rhs&&rhs->name==PTR){
			resolved|=sub_pat()->resolve_with_type(sc, rhs->sub, flags);
			this->set_type(new Type(this,PTR,sub->type()));
			return resolved;
		}
	}
	if (this->name==IF){// guarded.
		ASSERT(sub_pat()->next);
		sub_pat()->next->resolve_if(sc, nullptr, flags);
		sub_pat()->resolve_with_type(sc, rhs, flags);
		return propogate_type_refs(flags,(Node*)this, this->type_ref(), this->sub->type_ref());
	}
	if (this->name==OR){
		for (auto s=this->sub_pat();s;s=s->next){
			resolved|=s->resolve_with_type(sc,rhs,flags);
		}
		if (rhs)
			return propogate_type_fwd(flags, (Node*)this, rhs, this->type_ref());
		else
			return resolved;
	} else if (this->name==PATTERN_BIND){
		// get or create var here
		auto v=this->sub_pat(); auto p=v->next; ASSERT(p);
		resolved|=p->resolve_with_type(sc,rhs,flags);
		if (!v->type()&&p->type()){
			v->set_type((Type*)(p->type()->clone()));
		}
		resolved|=v->resolve_with_type(sc,p->type(),flags);
		return propogate_type_refs(flags, (Node*)this, this->sub->type_ref(), this->sub->next->type_ref());
	}
	else if (this->name==TUPLE){
		auto subt=rhs?rhs->sub:nullptr;
		for (auto subp=this->sub_pat(); subp; subp=subp->next, subt?subt=subt->next:nullptr){
			resolved|=subp->resolve_with_type(sc,subt, flags);
		}
	}
	else if(rhs && rhs->is_ref() && this->sub && this->name!=REF){// ref to value - in pattern stays a ref
		auto newsub=new Pattern(this->pos,this->name);
		newsub->sub=this->sub;
		this->sub=newsub;
		this->name=REF;
		//dbg(this->dump(0));
		//dbg(newline(0));
		return this->resolve_with_type(sc, rhs, flags);
		//		resolved|=sub_pat()->resolve_with_type(sc, rhs->sub, flags);
		//		this->set_type(new Type(this,REF,sub->type()));
		//		return resolved;
		
	}
	else if (this->name!=TUPLE && this->sub){ // Type(..,..,..) destructuring/variant
		auto sd=sc->find_struct_type(this,rhs);// todo tparams from rhs, if given
		if (sd){
			if (flags & R_FINAL) {
				if (!this->type()->is_coercible(rhs)){
					error(this,"can't match %s vs %s",str(rhs->name),str(sd->name));
				}
			}
			int i=sd->first_user_field_index(); auto subp=this->sub;
			// todo - sub types should resolve?!
			for (; i<sd->fields.size() && subp; i++,subp=subp->next){
				auto ft=sd->fields[i]->type();
				subp->set_type(ft);
				subp->resolve_with_type(sc,ft,flags);
			}
			this->set_struct_type(sd);
		}
		else if (is_range_operator(this->name)){
			for (auto subp=this->sub; subp;subp=subp->next){
				subp->resolve(sc,nullptr,flags);
			}
		} else if (flags & R_FINAL){
			error(this,"can't find %s",str(this->name));
		}
	} // else its a var of given type, or just a constant?
	else{
		if (rhs){
			propogate_type_fwd(flags, (Node*)this, rhs,this->type_ref());
		}
		dbg(dbprintf("matching pattern %s with..",str(this->name)));dbg(rhs->dump_if(-1));dbg(newline(0));
		if (auto sd=sc->find_struct_name_type_if(sc,this->name,this->type()))
		{
			this->set_struct_type(sd);
			return resolved;
		}
		if (auto sd=sc->find_struct_named(this->name)){
			this->set_struct_type(sd);
			return resolved;
		}
		if (auto sd=sc->find_inner_def_named(sc,this, 0)){
			
			this->set_struct_type(sd);
			return resolved;
		}
		
		// TODO named-constants
		// TODO boolean true/false
		// TODO nullptr
		if (is_number(this->name)){
			if (!this->type()) {this->set_type(Type::get_int());}
			return resolved;
		}
		else
			if (this->name==PLACEHOLDER){
				this->set_type(rhs);
				return resolved;
				
			} else {
				// TODO - scala style quoted variable for comparison
				auto v=sc->create_variable(this,this->name,Local);
				if (!this->def){
					dbg2(dbprintf("pattern match created var %s:",this->name_str())); dbg2(this->type()->dump_if(-1));dbg(newline(0));
					this->set_def(v);
				}
				if (rhs)
					return propogate_type_fwd(flags, this, rhs, v->type_ref());
				else
					return resolved;
			}
	}
	return resolved;
}

// TODO: we suspect this will be more complex, like Type translation (
void Pattern::translate_tparams(const TParamXlat& tpx){
	this->type()->translate_typeparams_if(tpx);
	this->def->translate_typeparams_if(tpx);
	auto i=tpx.typeparam_index(this->name);
	if (i>=0){
		this->name=tpx.given_types[i]->name;
	}
	for (auto s=this->sub_pat(); s;s=s->next){
		s->translate_tparams(tpx);
	}
}

CgValue Pattern::compile(CodeGen &cg, Scope *sc, CgValue val){
	auto ptn=this;
	// emit a condition to check if the runtime value  fits this pattern.
	// TODO-short-curcuiting - requires flow JumpToElse.
	dbg(val.dump());
//	ASSERT(val.type);
	// single variable bind.
	if (ptn->name==VOID){
		return CgValue();
	}
	if (ptn->name==EXPRESSION){
		auto rhs=ptn->sub->compile(cg,sc,CgValue());
		if (val.is_valid()){
			return cg.emit_instruction(EQ, rhs, val);
		} else
			return rhs;
	}
	if (ptn->name==PTR ||ptn->name==REF||ptn->name==RVALUE_REF){
		return ptn->sub->compile(cg, sc, val.is_valid()?val.deref_op(cg):val);
	}
	if (ptn->name==PLACEHOLDER){
		return cg.emit_val_bool(true);
	}
	if (ptn->name==PATTERN_BIND){
		auto v=ptn->get_elem(0);
		auto p=ptn->get_elem(1);
		auto disr=val.is_valid()?cg.emit_loadelement(val, __DISCRIMINANT):val;
		auto ps=p->type()->is_pointer_or_ref()?p->sub:p;
		auto sd=ps->def->as_struct_def();
		auto b=cg.emit_instruction(EQ, disr, cg.emit_val_i32(sd->discriminant));
		auto var=v->def->as_variable();
		if (val.is_valid())
			CgValue(var).store(cg, cg.emit_conversion((Node*)ptn, val, var->type(), sc));// coercion?
		
		return b;
	}
	else if (ptn->name==RANGE || ptn->name==RANGE_LT|| ptn->name==LT_RANGE|| ptn->name==LT_RANGE_LT){
		auto sp=ptn->sub;
		auto lo=sp->compile(cg,sc,CgValue());
		auto hi=sp->next->compile(cg,sc,CgValue());
		return	cg.emit_instruction(
									AND,Type::get_bool(),
									cg.emit_instruction(ptn->name!=LT_RANGE||LT_RANGE_LT?GE:GT,val,lo),
									cg.emit_instruction(ptn->name==RANGE_LT||LT_RANGE_LT?LT:LE,val,hi)
									);
	}
	if (ptn->name==IF){
		return cg.emit_instruction(AND,sub->compile(cg,sc,val),sub->next->compile(cg,sc,CgValue()));
	}
	else
		if (ptn->name==OR || ptn->name==TUPLE ||ptn->sub){// iterate components...
			int index;
			CgValue ret=CgValue();
			CgValue	val2=val;
			Name op;
			if (ptn->name==OR) {op=LOG_OR;index=0;}
			else if(ptn->name==TUPLE){op=LOG_AND;index=0;}
			else{
				dbg2({val.dump();newline(0);})
				auto disr=cg.emit_loadelement(val, __DISCRIMINANT);
				auto ptns=ptn->type()->is_pointer_or_ref()?ptn->sub:ptn;
				auto sd=ptns->def->as_struct_def();
				index=sd->first_user_field_index();
				ret=cg.emit_instruction(EQ, disr, cg.emit_val_i32(sd->discriminant));
				if (val.is_valid())
					val2=cg.emit_conversion((Node*)ptn,val, ptn->type(),sc);
				dbg2(val2.type->dump(0));dbg2(newline(0));
			}
			//todo - this part moves to bind if not or/tuple
			for (auto subp=ptn->sub; subp; subp=subp->next,index++){
				if (subp->name==VOID){	// None(void) - needs something in the brackets.
					continue;
				}
				auto elem=ptn->name!=OR?cg.emit_getelementref(val2,0, index,subp->type()):val;
				auto b=subp->compile(cg, sc, elem);
				if (ptn->name==OR || ptn->name==TUPLE){
					//ASSERT(b.type->is_bool())
				}
				if (op){
					if (!ret.is_valid())
						ret=b;
					else
						ret=cg.emit_instruction(op,Type::get_bool(),ret,b);
				}
			}
			dbg(ret.dump());
			return ret;
		}
		else if (ptn->def){
			if (auto var=ptn->def->as_variable()){
				auto varr=var->compile(cg,sc,CgValue());
				// TODO: overload assign operator as constructor would go here
				
				
				
				dbg(dbprintf("bind %s :",var->name_str()));dbg(var->type()->dump_if(-1));dbg(newline(0));
				dbg(dbprintf("given val :",var->name_str()));dbg(val.type->dump_if(-1));dbg(newline(0));dbg(ptn->type()->dump_if(-1));dbg(val.dump());dbg(newline(0));
				if (val.is_valid())
					varr.store(cg, val);
				return cg.emit_val_bool(true);
			}else
				// single value
				if (auto lit=ptn->def->as_literal()){
					return	cg.emit_instruction(EQ, CgValue(lit), val);
				}
		}
	if (is_number(this->name)){
		// its a constant or number
		auto cmpval=cg.emit_val_i32(getNumberInt(this->name));
		if (val.is_valid()){
			return	cg.emit_instruction(EQ, cmpval, val);
		}else
			return cmpval;
	}

	dbg({ptn->dump(0);newline(0);});
	dbg(dbprintf("uncompiled node %s\n",str(ptn->name)));
	return CgValue();
}

void Pattern::push_back(Pattern* newp){
	if (!newp) return;
	Pattern **pp=&sub;
	while (auto p=*pp){pp=&p->next;}
	*pp=newp;
}
void Pattern::push_child(Pattern* newp) {
	if (!newp) return;
	auto p=this;
	for (; p->sub; p=p->sub){};
	p->sub=newp;
}
void Pattern::dump(PrinterRef depth)const{
	int d2=depth>=0?depth+1:depth;
	newline(depth);
	if (name==TUPLE){
	}else
		if (name==REF){
			dbprintf("&");
		}else if (name==PTR){
			dbprintf("*");
		}else if (name==OR){
			for (auto s=this->sub;s;s=s->next){
				s->dump(-1);
				if (s->next)dbprintf("|");
			}
			return;
		} else if (name==PATTERN_BIND){
			sub->dump(-1);dbprintf("@");
			sub->next->dump_if(-1);
		} else if(name==RANGE || name==RANGE_LT||name==LT_RANGE||name==LT_RANGE_LT){
			sub->dump(-1);dbprintf("..");sub->next->dump(-1);
		}else
			dbprintf(str(name));
	if (this->sub && name!=PATTERN_BIND && name!=RANGE){
		dbprintf("(");
		for (auto s=this->sub;s;s=s->next){
			s->dump(d2);if (s->next)dbprintf(",");
		}
		dbprintf(")");
	}
	if (this->type()) {dbprintf(":"); this->type()->dump_if(-1);dbprintf(" ");}
}

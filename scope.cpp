#include "ast.h"
#include "scope.h"
#include "exprstructdef.h"

ExprFnDef* NamedItems::getByName(Name n){
	for(auto f=this->fn_defs;f;f=f->next_of_name){
		if (f->name==n)
			return f;
	}
	return nullptr;
}

ExprStructDef* Scope::find_struct_of(const Expr* srcloc)
{
	auto t=srcloc->type();
	auto sname=t->deref_all();
	if (!sname->is_struct()) error(srcloc,t,"expected struct, got %s",sname->name_str());
	auto r=try_find_struct(sname);
	//		if (!r)
	//			error(srcloc,"cant find struct %s", sname->name_str());
	if (!r){
		if (auto sd=srcloc->type()->def->as_struct_def()){
			return sd;
		}
	}
	return r;
}//original scope because typarams might use it.


Scope* Scope::make_inner_scope(Scope** pp_scope,ExprDef* owner,Expr* sub_owner)
{
	if (!*pp_scope){
		auto sc=new Scope;
#if DEBUG>=2
		if (auto ofd=owner->as_fn_def()){
			dbprintf("create scope in %p %s recv=%p\n",sc,ofd->name_str(),ofd->m_receiver);
		}else{
			dbprintf("create scope in %p %s \n",sc,owner->name_str());
		}
#endif
		push_child(sc);
		sc->owner_fn=owner;
		*pp_scope=sc;
		ASSERT(sc->node==0);
		if(!sc->node){sc->node=sub_owner;}
	}
	
	return *pp_scope;
};

TParamDef*	Scope::get_typeparam_for(Type* t) {
	if (t->def){
		if (auto tp=t->def->as_tparam_def())
			return tp;
	}
	for (auto s=this; s; s=s->parent){
		if (s->owner_fn){
			if (auto tpds=s->owner_fn->get_typeparams()){
				for (auto tpd:*tpds){
					if (tpd->name==t->name){
						t->set_def(tpd);
						return tpd;
					}
				}
			}
		}
	}
	return nullptr;
}

NamedItems* Scope::get_named_items_local(Name name){
	if (auto ni=find_named_items_local(name))
		return ni;
	NamedItems* ni=new NamedItems(name,this);
	ni->next=this->named_items;
	this->named_items=ni;
	ni->owner=this;
	return ni;
}
NamedItems* Scope::find_named_items_local(Name name){
	for (auto ni=this->named_items;ni;ni=ni->next){
		if (ni->name==name){return ni;}
	}
	return nullptr;
}

NamedItems* Scope::find_named_items_rec(Name name){
	if (auto ni=find_named_items_local(name))
		return ni;
	if (this->parent) return this->parent->find_named_items_rec(name);
	else return nullptr;
}

ExprFnDef* Scope::find_unique_fn_named(const Node* name_node,int flags, const Type* fn_type){
	auto name=name_node->as_name();
	auto sc=this;
	ExprFnDef* found=nullptr; bool ambiguous=false;
	ASSERT(fn_type==0 &&"when we have type info here, remove this hack")
	for (;sc;sc=sc->parent_or_global()){
		if(auto ni=sc->find_named_items_local(name)){
			for(auto f=ni->fn_defs;f;f=f->next_of_name){
				if (f->name==name && !f->is_generic()){
					if (found) {ambiguous=true;return nullptr;}
					found=f;
				}
			}
		}
	}
	if (flags&R_FINAL){
		//error(name_node,"can't find fn");
	}
	return ambiguous?nullptr:found;
	
}

ExprFnDef*	Scope::find_fn(Name name,const Expr* callsite, const vector<Expr*>& args,const Type* ret_type,int flags)  {
	verify_all();
	
	// TODO: rework this to take Type* fn_type fn[(args),ret] - for symetry with anything using function pointers
	// TODO: ACCELERATION:
	// make a type-code and have direct hash lookup of exact-match
	// we only need all this search logic to execute once per permutation of args.
	FindFunction ff(name,args,ret_type,flags);
	ff.callsite=callsite;
	
	Scope* prev=nullptr;
	vector<pair<ExprFnDef*,int>> candidates;
	
	Scope* rec_scope=nullptr;
	if (args.size()>0){
		if (auto rt=args[0]->type()){
			auto rec_t=rt->deref_all();
			if (auto sd=rec_t->get_struct_autoderef()){
				rec_scope=sd->scope;
				if (rec_scope){
					ff.find_fn_from_scopes(rec_scope,nullptr);
				}
			}
		}
	}
	
	for (auto src=this; src; prev=src,src=src->parent) {// go back thru scopes first;
		ff.find_fn_from_scopes(src,prev);
	}
	if (!ff.candidates.size()){
		if (flags & R_FINAL)
			error(callsite,"can't find function %s\n",str(name));
		return nullptr;
	}
	verify_all();
	if (ff.candidates.back().score<=0 || name==PLACEHOLDER) {
		if (flags & 1){
		no_match_error:
			auto best=ff.candidates.back();
			error_begin(callsite,"call: %s(",str(name));
			for (auto i=0; i<args.size(); i++){	if (i)dbprintf(",");dbprintf("",i); args[i]->type()->dump(-1); }
			dbprintf(")");
			
			// For the best match, say what you'd have to do to fix, then show all matches
			if (args.size()<best.f->min_args()){info(best.f,"maybe requires %d args, %d given",best.f->min_args(),args.size());}
			vector<Type*> callsite_tys;
			match_typeparams(callsite_tys, best.f,callsite->as_block());
			auto tpxlat=TypeParamXlat{best.f->typeparams,callsite_tys};
			for (auto i=0; i<args.size() && i<best.f->args.size(); i++){
				
				if (!args[i]->type()->is_equal(best.f->args[i]->type(),tpxlat)){
					info(best.f->args[i],"want[%d]: ",i); best.f->args[i]->type()->dump_if(-1);
					dbprintf("; given:"); args[i]->type()->dump_if(-1);newline(0);
					//					tpxlat.dump(0);
					break;
				}
			}
			//			if (candidates.size()>1)info(callsite,"other candidates:-");
			int imax=(int)ff.candidates.size()-2;
			int imin=imax-5; if (imin<0)imin=0;
			for (auto i=imin; i<=imax; i++) {
				auto &c=ff.candidates[i];
				info(c.f,"or: ",i);c.f->dump_signature();
			}
			
			error_end(callsite);
			return nullptr;
		}
		// SFINAE for caller
		return nullptr;
	}
	if (flags & R_FINAL && !ff.candidates.size())
	{
		error(callsite,";No matches found for %s\n",str(name));
		return nullptr;
	}
	if (ff.verbose)
		ff.dump();
	verify_all();
	for (int i=(int)ff.candidates.size()-1; i>=0; i--) {
		auto c=&ff.candidates[i];
		auto next_best=c->f;
		if (c->score<=0)continue;
		if (!next_best->is_generic())
			return next_best;
		if (auto new_f= instantiate_generic_function(next_best, callsite,name, args,ret_type,flags))
			return new_f;
		//TODO SFINAE here resolve it, if it doesn't resolve, try next.
	}
	if (flags&1){
		goto no_match_error;
	}
	return nullptr;
}

Variable* Scope::find_fn_variable(Name name,ExprFnDef* f){
	// todo: This Pointer?
	if (this->owner_fn!=f) return nullptr;
	for (auto v=this->vars; v; v=v->next_of_scope) {
		if (v->name==name) return v;
	}
	if (auto p=this->parent_within_fn())
		if (auto v=p->find_fn_variable(name,f))
			return v;
	if (this->global && this->global!=this){
		if (auto p=this->global->find_fn_variable(name,f))
			return	p;
	}
	return nullptr;
}
ExprStructDef* Scope::find_struct_sub(Scope* original,const Type* t){
	if (!t->has_non_instanced_typeparams())
		return nullptr;
	if (auto fn=this->find_named_items_local(t->name)){
		for (auto st=fn->structs; st;st=st->next_of_name){
			if (st->name==t->name) {
				// find with type-params...
				if (!st->is_generic())
					return st;
				return st->get_instance(original, t);
			}
		}
	}
	if (auto p=parent_or_global()) return p->find_struct_sub(original,t);
	else return nullptr;
}
ExprStructDef* Scope::find_struct_named(Name name){
	if (auto fn=this->find_named_items_local(name)){
		for (auto st=fn->structs; st;st=st->next_of_name){
			if (st->name==name) {
				return st;
			}
		}
	}
	if (auto p=parent_or_global())
		return p->find_struct_named(name);
	else return nullptr;
}

void Scope::add_fn(ExprFnDef* fnd){
	if (fnd->instance_of!=0) return; // we compile/match it by instance search.
	if (fnd->name_ptr) return;
	if (fnd->type_parameter_index(fnd->name)>=0){
		this->templated_name_fns=fnd;
		fnd->next_of_name=this->templated_name_fns;
	}
	auto ni=get_named_items_local(fnd->name);
	fnd->name_ptr=ni;
	fnd->next_of_name=ni->fn_defs;
	ni->fn_defs=fnd;
}
void Scope::add_struct(ExprStructDef* sd){
	dbg_instancing("adding struct %p %s ins of %p to %s\n",sd,sd->name_str(),sd->instance_of,this->name());
	if (sd->name_ptr)
		return;
	if (sd->instance_of){
		return add_struct(sd->instance_of);
	}
	auto ni=get_named_items_local(sd->name);
	sd->name_ptr=ni;
	sd->next_of_name=ni->structs;
	ni->structs=sd;
#if DEBUG>=2
	dbprintf("scope is now:-\n");
	this->dump(0);
#endif
}
Variable* Scope::find_scope_variable(Name name){
	for (auto v=this->vars; v;v=v->next_of_scope){
		if (v->name==name) return v;
	}
	return nullptr;
}
Variable* Scope::find_variable_rec(Name name){
	dbg_varscope("find variable %s in %s\n",str(name),this->name());
	for (auto sc=this; sc;sc=sc->parent_within_fn())
		if (auto v=sc->find_scope_variable(name))
			return v;
	
	if (this->capture_from && this->capture_from!=this){
		auto cv=this->try_capture_var(name);
		return cv;
	}
	
	return nullptr;
}

Variable* Scope::try_capture_var(Name name) {
	for (auto v=capture_from->vars;v; v=v->next_of_capture){
		if (v->name==name &&v->capture_in){
			dbg_varscope("Found CaptureVarsd Var %s in %s %s\n",str(name),this->name(),str(v->capture_in->name));
			return v;
		}
	}
	dbg_varscope("Trying to capture %s in %s\n",str(name),this->name());
	dbg_varscope(" from %s\n",this->capture_from->name());
	if (auto ofn=dynamic_cast<ExprFnDef*>(this->owner_fn)){
		auto v=capture_from->find_variable_rec(name);
		if (v) {
			dbg_varscope("capture: found %s in %s\n",str(name),this->capture_from->name());
			auto cp=ofn->get_or_create_capture(this->capture_from->owner_fn->as_fn_def());
			if (v->capture_in==0){
				v->capture_in=cp; v->next_of_capture=cp->vars; cp->vars=v;
				dbg_varscope("%s captured by %s from %s\n",str(name),this->name(),capture_from->name());
				return v;
			}
			else if (v->capture_in!=cp) {
				dbg_varscope("var %s already captured by %s, coalesce with %s\n",str(name),str(v->capture_in->capture_by->name),this->name());
				cp->coalesce_with(v->capture_in);
				return v;
				//			error(v,ofn,"can't capture variable twice yet- TODO, coalesce capture blocks");
			}
			return v;
		}
	}
	return nullptr;// we can't capture.
}

/*
 Variable* Scope::get_or_create_variable(Name name,VarKind k){
	if (auto v=this->find_variable_rec(name)) {
 return v;
	}
	return this->create_variable(name,k);
 }
 */
Variable* Scope::create_variable(Node* ast_pos, Name name,VarKind k){
	auto exv=this->find_scope_variable(name);
	if (exv) return exv;
	ASSERT(exv==0);
	auto v=new Variable(ast_pos->pos,name,k); v->next_of_scope=this->vars; this->vars=v;
	v->name=name;v->owner=this;
	return v;
}
Variable* Scope::get_or_create_scope_variable(Node* creator,Name name,VarKind k){
	auto exv=this->find_scope_variable(name);
	auto shadow_v=find_variable_rec(name);
	if (exv) return exv;
	if (shadow_v){
		warning(creator,"warning shadowing variable %s in %s\n",str(name),this->name());
	}
	auto v=this->create_variable(creator,name,k);
	return v;
}
void Scope::dump(int depth)const {
	newline(depth);dbprintf("scope: %s",this->name());
	if (this->parent)
		dbprintf("(of %s)", this->parent);
	dbprintf("{",this->name());
	for (auto v=this->vars; v; v=v->next_of_scope) {
		newline(depth+1); dbprintf("var %d %s:",index(v->name), getString(v->name));
		if (auto t=v->get_type()) t->dump(-1);
	}
	for (auto ni=this->named_items; ni;ni=ni->next){
		newline(depth+1); dbprintf("name %s:",getString(ni->name));
		for (auto fnd=ni->fn_defs; fnd;fnd=fnd->next_of_name){
			newline(depth+1);dbprintf("fn %s\n",getString(fnd->name));
		}
		for (auto fnd=ni->structs; fnd;fnd=fnd->next_of_name){
			newline(depth+1);dbprintf("struct %s\n",getString(fnd->name));
		}
	}
	for (auto s=this->child; s; s=s->next){
		s->dump(depth+1);
	}
	newline(depth);dbprintf("}");
}

Expr*	Scope::current_loop(int levels){
	int i=0;
	for (auto sc=this; sc;sc=sc->parent_within_fn()){
		if (auto n=sc->node->as_for()){
			i++;
			if (i==levels)
				return (Expr*)n;
		}
	}
	return nullptr;
}


void dump_locals(Scope* s){
	for (;s;s=s->parent){
		for (auto v=s->vars; v;v=v->next_of_scope){
			printf("\t;%s:",str(v->name));v->get_type()->dump(-1); printf("%%%s\n",str(v->reg_name));
		}
	}
}


ExprStructDef* Scope::find_struct(const Node* node) {
	if (auto sd=const_cast<ExprStructDef*>(dynamic_cast<const ExprStructDef*>(node))){return sd;} return find_struct_named(node);
}
ExprStructDef* Scope::get_receiver() {
	if (auto o=this->owner_fn)
		if (auto f=o->as_fn_def())
			return f->get_receiver();
	return nullptr;
}
const char* Scope::name() const {
	if (owner_fn) return str(owner_fn->name);
	if (!parent){
		return"<global>";
	}  else return "<anon>";
}



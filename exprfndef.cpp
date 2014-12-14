#include "semantics.h"
#include "codegen.h"
#include "repl.h"
#include "lexer.h"
#include "parser.h"
#include "run_test.h"
#include "error.h"


void ExprFnDef::verify(){
	verify_expr_fn_def(this);
	if (body) this->body->verify();
	for (auto x:args) x->verify();
	for (auto s=this->instances; s;s=s->next_instance) s->verify();
}

bool ExprFnDef::is_generic() const {
	if(instances!=nullptr)
		return true;
	if (typeparams.size())
		return true;
	for (auto i=0; i<args.size(); i++)
		if (!args[i]->type() || args[i]->type()->name==AUTO)
			return true;
	return false;
}

void ExprFnDef::set_receiver_if_unset(ExprStructDef* r){
	///TODO: ambiguity between special receiver and '1st parameter'.
	/// we started coding a 'special receiver'
	/// however UFCS means a generalized '1st parameter' makes more sense,
	/// and it's less intrusive to retrofit.
	/// we might also try 'multiple receivers' for nested classes?
	/// eg struct Scene { struct Model{  methods of model get Scene*, Model*... }}
	/// .. and we want to implement lambdas as sugar for callable objects much like c++/rust trait reform
	if (!this->m_receiver){
		this->m_receiver=r;
	}
}



void ExprFnDef::dump(int ind) const {
	dump_sub(ind,FN);
}

void ExprFnDef::dump_sub(int ind, Name prefix) const {
	if (!this) return;
	newline(ind);dbprintf("%s %s",getString(prefix),getString(name));dump_typeparams(this->typeparams);dbprintf("(");
	for (int i=0; i<args.size();i++){
		args[i]->dump(-1);
		if (i<args.size()-1) dbprintf(",");
	}
	if (variadic) dbprintf(args.size()?",...":"...");
	dbprintf(")");
	if (this->ret_type) {dbprintf("->");this->ret_type->dump(-1);};
	if (ind && this->fn_type) {
		newline(ind);dbprintf(":");this->fn_type->dump(-1);
	}
	if (this->body) {
		this->body->dump(ind);
	}
	newline(ind);
	if (auto p=this->instances){
		dbprintf(";//instantiations:");
		for (;p;p=p->next_instance){
			p->dump(ind);
		}
	}
}
ExprStructDef*	ExprFnDef::get_receiver(){ // TODO: switch to 1st-argument.
	return m_receiver;
}
Expr* ExprFnDef::get_return_value() const{
	if (this->body){
		if (auto b=dynamic_cast<ExprBlock*>(this->body)){
			if (b->argls.size()>0){
				return b->argls.back();
			}
		}
		else return this->body;
	}
	return 0;
}

Node*
ExprFnDef::clone() const{
	if (!this) return nullptr;
	auto r=new ExprFnDef(this->pos);
	r->m_closure=m_closure;
	r->name=this->name;
	r->c_linkage=false; //generic instance is not extern C.
	r->body=(ExprBlock*)(this->body?this->body->clone():nullptr);
	r->m_receiver=m_receiver;
	r->num_prefix_args=num_prefix_args;
	r->ret_type=(Type*)ret_type->clone_if();
	r->args.resize(this->args.size());
	for (int i=0; i<this->args.size(); i++) {
		r->args[i]=(ArgDef*)this->args[i]->clone();
	}
	return r;
}

Name ExprFnDef::get_mangled_name()const{
	if (!mangled_name){
		if (name==getStringIndex("main")||c_linkage){
			const_cast<ExprFnDef*>(this)->mangled_name=name;
		}else{
			char buffer[1024];
			name_mangle(buffer,1024,this);
			const_cast<ExprFnDef*>(this)->mangled_name=getStringIndex(buffer);
		}
	}
	return this->mangled_name;
}

void ExprFnDef::dump_signature()const{
	dbprintf("fn %s(",str(name));
	int i=0;for (auto a:args) {if(i)dbprintf(",");a->dump(-1);i++;}
	if (this->variadic){dbprintf("...");};
	dbprintf(")->");
	//	this->return_type()->dump_if(-1);
	this->ret_type->dump_if(-1);
}
ExprFnDef* instantiate_generic_function(ExprFnDef* srcfn,const Expr* callsite, const Name name, const vector<Expr*>& call_args, const Type* return_type,int flags) {
	verify_all();
	dbg_generic("instantiating %s %d for call %s %d\n",str(name),srcfn->pos.line, callsite->name_str(),callsite->pos.line);
	if (srcfn->type_parameter_index(srcfn->name)>=0){
		dbprintf("WARNING instantiated templated NAME function for %s, as no function of the right name was found.. experiment aimed at implementing OOP thru generics.. eg fn METHOD[OBJ,METHOD,ARG0,ARG1](o:OBJ,a0:ARG0,a1:ARG1){ o.vtable.METHOD(o,a0,a1)}", str(name));
	}
	Scope* src_fn_owner=srcfn->scope->parent_or_global();
	ExprFnDef* new_fn =(ExprFnDef*) srcfn->clone();
	// fill any args we can from the callsite.
	// TODO: translate generic-type-params
	// because we may infer return from that
	
	for (auto i=0; i<new_fn->args.size() && i<call_args.size(); i++){
		auto t=call_args[i]->get_type();
		if (t && !new_fn->args[i]->type())	{
			new_fn->args[i]->set_type((Type*)t->clone());
		}
	}
	if (return_type && !new_fn->return_type()){
		new_fn->ret_type=const_cast<Type*>(return_type);
	}
#if DEBUG >=2
	return_type->dump_if(-1);
#endif
	verify_all();
	
	auto callsiteb=dynamic_cast<const ExprBlock*>(callsite);
	ASSERT(callsiteb!=0 &&"ambiguity, when we come to do operator overloads, ExprOp & ExprBlock will call..");
	vector<Type*>	ins_typarams;
	match_typeparams(ins_typarams, srcfn,callsiteb);
	TypeParamXlat xlat(srcfn->typeparams, ins_typarams);
	new_fn->translate_typeparams(xlat);
	
	// todo: translate return type. for the minute we discard it..
	new_fn->set_def(srcfn);
	//	new_fn->ret_type=nullptr;
	new_fn->body->clear_type();// todo, inference upward..
	new_fn->next_instance = srcfn->instances;
	srcfn->instances=new_fn;
	new_fn->instance_of = srcfn;
	new_fn->resolved=false;
	new_fn->resolve(src_fn_owner,return_type,flags);//todo: we can use output type ininstantiation too
	//	new_fn->dump(0);
	new_fn->resolve(src_fn_owner,return_type,flags);//todo: we can use output type
	new_fn->fn_type->resolve(src_fn_owner,return_type,flags);//todo: we can use output type
#if DEBUG >=2
	dbg_fnmatch("%s return type=\n",new_fn->name_str());
	srcfn->type()->dump_if(-1);
	dbg_fnmatch(" from ");
	new_fn->type()->dump_if(-1000);
	dbg_fnmatch("\n");
	new_fn->fn_type->dump_if(-1);
	dbg_fnmatch("\nlast expression:");
	new_fn->last_expr()->dump_if(0);
	dbg_fnmatch("\nlast expression type:");
	new_fn->last_expr()->type()->dump_if(0);
	dbg_fnmatch("\n");
	new_fn->fn_type->resolve(src_fn_owner,return_type,flags);//todo: we can use output type
#endif
	
	verify_all();
	return new_fn;	// welcome new function!
}
//global fn:   definer_scope->capture_from =0;
//             so set 'capture_from' to its own scope.
//
//inner-function: 'definer_scope' has capture_from set - just take it.
ResolvedType ExprFnDef::resolve(Scope* definer_scope, const Type* desired,int flags) {
	verify_all();
	if (auto sd=dynamic_cast<ExprStructDef*>(definer_scope->owner_fn)){
		return resolve_function(definer_scope,sd,desired,flags);
	}
	else return resolve_function(definer_scope,nullptr,desired,flags);
}
ResolvedType ExprFnDef::resolve_function(Scope* definer_scope, ExprStructDef* recs,const Type* desired,int flags) {
	verify_all();
	
	// propogate given arguments eg polymorphic lambda..
	if (desired){
		if (!desired->is_callable()){
			error(this,desired,"creating lambda function,but trying to infer non function type\n");
		}
		// fn[(args..),ret]
		if (auto args_ret=desired->sub){
			dbg_lambdas("infering polymorphic function types");
			int i=0;
			for (auto desired_arg=args_ret->sub; desired_arg && i<this->args.size(); desired_arg=desired_arg->next,i++){
				auto arg=this->args[i];
				propogate_type_fwd(flags, arg, (const Type*)desired_arg,arg->type_ref() );
			}
			auto desired_ret=args_ret->next;
			propogate_type_fwd(flags,this,desired_ret, this->ret_type);
			// inference between the whole function type backwards is done via ret_type
		}
	}
	
	definer_scope->add_fn(this);
	this->set_receiver_if_unset(recs);
	auto sc=definer_scope->make_inner_scope(&this->scope,this,this);
	if (definer_scope->capture_from){
		sc->capture_from=definer_scope->capture_from; // this is an 'inner function' (lambda, or local)
	}
	else{
		sc->capture_from=sc; // This is a global function or class-method; nowhere to capture from.
	}
	//this->scope->parent=this->scope->global=scope->global; this->scope->owner=this;}
	
	if (this->is_generic()){	// must resolve instances too, if they relied on args that aren't resolved? TODO: dont instance until all symbols are found
		for (auto ins=this->instances; ins;ins=ins->next_instance){
			ins->resolve(scope,nullptr,flags);
		}
		//return ResolvedType();
		flags=0; // dont throw type error here
	}
	
	if (true ||!this->is_generic()){
		for (int i=0; i<this->args.size() && i<this->args.size(); i++) {
			this->args[i]->resolve(this->scope, nullptr, flags); // todo: call with defaultparams & init-expr
			auto arg=this->args[i];
			auto v=sc->find_scope_variable(arg->name);
			if (!v){v=sc->create_variable(arg,arg->name,VkArg);}
			propogate_type(flags,arg, arg->type_ref(),v->type_ref());
			if (arg->default_expr){static int warn=0; if (!warn){warn=1;
				dbprintf("error todo default expressions really need to instantiate new code- at callsite, or a shim; we need to manage caching that. type propogation requires setting those up. Possible solution is giving a variable an initializer-expression? type propogation could know about that, and its only used for input-args?");}
			}
		}
		Type* desired_ret;
		if (desired)
			desired_ret=desired->fn_return();
		else
			desired=nullptr;
		
		if (this->body){
			auto ret=this->body->resolve(sc, this->ret_type, flags);
			propogate_type(flags, (const Node*)this,this->ret_type,this->body->type_ref());
#if DEBUG>=2
			this->ret_type->dump_if(-1);
			this->body->type()->dump_if(-1);
			newline(0);
#endif
			//			this->ret_type=ret.type;
			
			propogate_type(flags, (const Node*)this, ret,this->ret_type);
		}
	}

	if (this->fn_type){
		if (!this->ret_type->is_auto() && this->fn_type->fn_return()->is_auto()){
			dbprintf("fn ret type updated\n");
			this->fn_type->sub=0;
		}
		auto a=this->fn_type->fn_args_first();
		for (int i=0; i<this->args.size(); i++,a=a->next){
			auto ad=this->args[i];
			if (a->name==AUTO && !ad->type()->is_auto()){
				dbprintf("fn type updated\n");
				this->fn_type->sub=0;
				//memleak! but we're going to keep these owned permanently-type-pools
				break;
			}
		}
	}

	if (!this->fn_type) {
		this->fn_type=new Type(this,this->is_closure()?CLOSURE:FN);
		//		if (recs){arglist->push_back(new Type(PTR,recs));}
	}
	if (!this->fn_type->sub){
		auto arglist=new Type(this,TUPLE);
		for (auto a:this->args) {
			arglist->push_back(a->type()?((Type*)a->type()->clone()):new Type(this,AUTO));
		}
		// TODO - type inference needs to know about elipsis, as 'endless auto'
		//if (this->variadic){arglist->push_back(new Type(this,ELIPSIS));}
		this->set_type(this->fn_type);

		auto ret_t=this->ret_type?(Type*)(this->ret_type->clone()):new Type(this,AUTO);
		this->fn_type->set_fn_details(arglist,ret_t,recs);
	}
	// update any 'fn_type args' that were newly resolved.. corner case we found!
	

	if (true|| !this->is_generic()){
		
		this->fn_type->resolve_if(scope,nullptr,flags);
		this->return_type()->resolve_if(scope,nullptr,flags);
	}
	return ResolvedType(fn_type,ResolvedType::COMPLETE);
}

CaptureVars* ExprFnDef::get_or_create_capture(ExprFnDef* src){
	if (!this->my_capture) {
		auto c=new CaptureVars;
		this->my_capture = c;
		c->capture_by = this;
		c->capture_from=src;
		c->next_of_from=src->captures;
		src->captures=c;
		c->pos=src->pos;
		c->name=getStringIndexConcat(this->name,str(src->name));
	}
	return my_capture;
}

int ExprFnDef::type_parameter_index(Name n) const {
	for (auto i=0; i<typeparams.size(); i++){
		if (n==typeparams[i]->name)
			return i;
	}
	return -1;
}
ResolvedType ExprFnDef::resolve_call(Scope* scope,const Type* desired,int flags) {
	if (this->is_generic()){
		for (auto ins=this->instances; ins;ins=ins->next_instance){
			ins->resolve(scope,nullptr,flags);
		}
		return ResolvedType();
	}
	
	propogate_type_fwd(flags,this, desired,this->ret_type);
	
	auto rt=this->body->resolve(scope,desired,flags);
	dbprintf("resolve %s yields type:", getString(this->as_name()));if (rt.type) rt.type->dump(-1);printf("\n");
	// awkwardness says: type error return is more like an enum that doesn't return a type?
	// if its' a type error we should favour the most significant info: types manually specified(return values,function args)
	return propogate_type(flags,this, rt,this->ret_type); // todo: hide FnDef->type. its too confusing
}
bool Type::is_typeparam(Scope* sc)const{
	return sc->get_typeparam_for(const_cast<Type*>(this))!=0;
}
void ExprFnDef::translate_typeparams(const TypeParamXlat& tpx){
	dbg_generic("translate typeparams for fn %s\n",this->name_str());
	for (auto &a:args) a->translate_typeparams(tpx);

	
	this->ret_type->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	
	if (tpx.typeparams_all_set())
	{
		this->typeparams.resize(0);
	}
}

const ExprFnDef* ExprStructDef::find_function_for_vtable(Name n, const Type* sig){
	for (auto f:this->functions){
		if (f->name==n && f->fn_type->is_coercible(sig)) /// TODO only 'this' other params should be specific should coerce
			return f;
	}
	for (auto f:this->virtual_functions){
		if (f->name==n && f->fn_type->is_coercible(sig))
			return f;
	}
	for (auto f:this->static_functions){
		if (f->name==n && f->fn_type->is_coercible(sig))
			return f;
	}
	return nullptr;
}




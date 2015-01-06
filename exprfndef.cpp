#include "exprfndef.h"
int	ExprFnDef::min_args()	{
	for (int i=0; i<args.size();i++){
		if (args[i]->default_expr)
			return i;
	}
	return (int)args.size();
}


void ExprFnDef::verify(){
	verify_expr_fn_def(this);
	if (body) this->body->verify();
	for (auto x:args) x->verify();
	for (auto s=this->instances; s;s=s->next_instance) s->verify();
}

void ExprFnDef::gather_symbols(Scope* outer_sc){
	outer_sc->add_fn(this,false);
	auto sc=outer_sc->make_inner_scope(&this->scope,this,this);
	this->body->gather_symbols_if(this->get_scope());
	// functions' inner functions are acessible too? not sure they should be.
}

bool ExprFnDef::is_generic() const {
	if(instances!=nullptr)
		return true;
	if (tparams.size())
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

bool	ExprFnDef::has_return_value() const{
	if (auto r=return_type()){
		return index(r->name)!=VOID;}
	else return false;
}


void ExprFnDef::dump(PrinterRef ind) const {
	dump_sub(ind,FN);
}

void ExprFnDef::dump_sub(int ind, Name prefix) const {
	if (!this) return;
	newline(ind);dbprintf("%s %s",getString(prefix),getString(name));dump_tparams(this->tparams,&this->instanced_types);dbprintf("(");
	for (index_t i=0; i<args.size();i++){
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
void ExprFnDef::recurse(std::function<void(Node *)>& f){
	if (!this)return;
	for (auto x:this->args)	x->recurse(f);
	if (this->body)			this->body->recurse(f);
	this->type()->recurse(f);
	
}

ExprStructDef*	ExprFnDef::get_receiver(){ // TODO: switch to 1st-argument.
	return m_receiver;
}
const Expr* ExprFnDef::get_return_expr() const{
	if (this->body){
		return this->body->get_return_expr();
	}
	return nullptr;
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
ExprFnDef* instantiate_generic_function(ExprFnDef* srcfn,const Expr* pcallsite, const Name name, const MyVec<Expr*>& call_args, const Type* return_type,int flags) {
	verify_all();
	dbg_generic("instantiating %s %d for call %s %d\n",str(name),srcfn->pos.line, pcallsite->name_str(),pcallsite->pos.line);
	dbg_generic("\t%d args %d args inc receiver\n", pcallsite->get_elem_count(), call_args.size());
	if (srcfn->type_parameter_index(srcfn->name)>=0){
		dbprintf("WARNING instantiated templated NAME function for %s, as no function of the right name was found.. experiment aimed at implementing OOP thru generics.. eg fn METHOD[OBJ,METHOD,ARG0,ARG1](o:OBJ,a0:ARG0,a1:ARG1){ o.vtable.METHOD(o,a0,a1)}", str(name));
	}
	Scope* src_fn_owner=srcfn->scope->parent_or_global();
	dbg(srcfn->dump(0));

	ExprFnDef* new_fn =(ExprFnDef*) srcfn->clone();
	dbg(new_fn->dump(0));
	// fill any args we can from the callsite.
	// TODO: translate generic-type-params
	// because we may infer return from that
	
	for (auto i=0; i<new_fn->args.size() && i<call_args.size(); i++){
		auto t=call_args[i]->get_type();
		if (t && !new_fn->args[i]->type())	{
			new_fn->args[i]->set_type((Type*)t->clone());
		}
	}
	dbg2(new_fn->dump_signature());
	if (return_type && !new_fn->return_type()){
		new_fn->ret_type=const_cast<Type*>(return_type);
	}
	dbg2(return_type->dump_if(-1));
	verify_all();
	
	auto callsiteb=pcallsite;
	ASSERT(callsiteb!=0 &&"ambiguity, when we come to do operator overloads, ExprOp & ExprBlock will call..");
	MyVec<Type*>	ins_typarams;
	match_fn_tparams(ins_typarams, srcfn,call_args, callsiteb);
	TParamXlat xlat(srcfn->tparams, ins_typarams);
	new_fn->translate_tparams(xlat);
	dbg(new_fn->dump(0));
	
	// todo: translate return type. for the minute we discard it..
	new_fn->set_def(srcfn);
	//	new_fn->ret_type=nullptr;
	new_fn->body->clear_type();// todo, inference upward..
	new_fn->next_instance = srcfn->instances;
	srcfn->instances=new_fn;
	new_fn->instance_of = srcfn;
	new_fn->resolved=INCOMPLETE;
	
	dbg2(printf("arg types after instancing\n"));
	dbg2(new_fn->dump_signature());
	
	new_fn->resolve_if(src_fn_owner,return_type,flags);//todo: we can use output type ininstantiation too
	//	new_fn->dump(0);
	new_fn->resolve_if(src_fn_owner,return_type,flags);//todo: we can use output type
	new_fn->fn_type->resolve_if(src_fn_owner,return_type,flags);//todo: we can use output type

	dbg_fnmatch("%s return type=\n",new_fn->name_str());
	dbg2(srcfn->type()->dump_if(-1));
	dbg_fnmatch(" from ");
	dbg2(new_fn->type()->dump_if(-1000));
	dbg_fnmatch("\n");
	dbg2(new_fn->fn_type->dump_if(-1));
	dbg_fnmatch("\nlast expression:");
	dbg2(new_fn->last_expr()->dump_if(0));
	dbg_fnmatch("\nlast expression type:");
	dbg2(new_fn->last_expr()->type()->dump_if(0));
	dbg_fnmatch("\n");
	dbg2(new_fn->fn_type->resolve_if(src_fn_owner,return_type,flags));//todo: we can use output type

	verify_all();
	return new_fn;	// welcome new function!
}
//global fn:   definer_scope->capture_from =0;
//             so set 'capture_from' to its own scope.
//
//inner-function: 'definer_scope' has capture_from set - just take it.
ResolveResult ExprFnDef::resolve(Scope* definer_scope, const Type* desired,int flags) {
	verify_all();
	if (auto sd=definer_scope->owner_struct()){
		return resolve_function(definer_scope,sd,desired,flags);
	}
	else return resolve_function(definer_scope,nullptr,desired,flags);
}
ResolveResult ExprFnDef::resolve_function(Scope* definer_scope, ExprStructDef* recs,const Type* desired,int flags) {
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
				arg->propogate_type_fwd(flags, (const Type*)desired_arg,arg->type_ref() );
			}
			auto desired_ret=args_ret->next;
			propogate_type_fwd(flags,desired_ret, this->ret_type);
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
			ins->resolve_if(scope,nullptr,flags);
		}
		//return ResolveResult();
		flags=0; // dont throw type error here
	}

	if (true || !this->is_generic()){
		// Need to propogate types in generic function bodies eg polylambdas
		// but dont need to terminate compiling until its instantiated properly
		auto use_flags=(this->is_generic())?(flags&~R_FINAL):flags;
			
		for (index_t i=0; i<this->args.size() && i<this->args.size(); i++) {
			this->args[i]->resolve_if(this->scope, nullptr, use_flags); // todo: call with defaultparams & init-expr
			auto arg=this->args[i];
			auto v=sc->find_scope_variable(arg->name);
			if (!v){
				v=sc->create_variable(arg,arg->name,VkArg);
			}
			propogate_type_refs(flags,arg, arg->type_ref(),v->type_ref());
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
			auto ret=this->body->resolve_if(sc, this->ret_type, flags);
			propogate_type_refs(flags, (const Node*)this,this->ret_type,this->body->type_ref());

			dbg2(this->ret_type->dump_if(-1));
			dbg2(this->body->type()->dump_if(-1));
			dbg2(newline(0));

			//			this->ret_type=ret.type;
			
			propogate_type_fwd(flags, this->body->type(),this->ret_type);
		}
	}

	if (this->fn_type){
		if (!this->ret_type->is_auto() && this->fn_type->fn_return()->is_auto()){
			dbprintf("fn ret type updated\n");
			this->fn_type->sub=0;
		}
		auto a=this->fn_type->fn_args_first();
		for (int i=0; i<this->args.size() && a; i++,a=a->next){
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
	// propogate args in patterns.. with given too?
	for (auto x:this->args){
		dbg(x->dump(0))
		;
		if (x->pattern && this->scope)
			x->pattern->resolve_if(this->scope,x->type(),flags);
	}


	if (true|| this->is_generic()){
		
		this->fn_type->resolve_if(scope,nullptr,flags);
		this->return_type()->resolve_if(scope,nullptr,flags);
	}
	return ResolveResult(COMPLETE);
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
	for (index_t i=0; i<tparams.size(); i++){
		if (n==tparams[i]->name)
			return i;
	}
	return -1;
}
ResolveResult ExprFnDef::resolve_call(Scope* scope,const Type* desired,int flags) {
	if (this->is_generic()){
		for (auto ins=this->instances; ins;ins=ins->next_instance){
			resolved|=ins->resolve_if(scope,nullptr,flags);
		}
		return resolved;
	}
	
	propogate_type_fwd(flags, desired,this->ret_type);
	
	auto rt=this->body->resolve_if(scope,desired,flags);
	dbprintf("resolve %s yields type:", getString(this->as_name()));if (auto t=this->body->type()) t->dump(-1);printf("\n");
	// awkwardness says: type error return is more like an enum that doesn't return a type?
	// if its' a type error we should favour the most significant info: types manually specified(return values,function args)
	return propogate_type_refs(flags,this, this->body->type_ref(),this->ret_type); // todo: hide FnDef->type. its too confusing
}
bool Type::is_typeparam(Scope* sc)const{
	return sc->get_typeparam_for(const_cast<Type*>(this))!=0;
}
void ExprFnDef::translate_tparams(const TParamXlat& tpx){
	this->instanced_types=tpx.given_types;

	dbg_generic("translate tparams for fn %s\n",this->name_str());
	for (auto &a:args)
		a->translate_tparams(tpx);

	
	this->ret_type->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	
	if (tpx.typeparams_all_set())
	{
		this->tparams.resize(0);
	}
}

const ExprFnDef* ExprStructDef::find_function_for_vtable(Name n, const Type* sig){
	for (auto f:this->functions){
		if (f->name!=n) continue;
		dbg_vtable("try vtable fn %s.%s\n",str(name),str(f->name));
		dbg2(f->fn_type->dump(-1));
		dbg_vtable("\n vs ");
		dbg2(sig->dump(-1));
		dbg_vtable("\n");

		if (f->fn_type->is_equal(sig,false,this->name)) /// TODO only 'this' other params should be specific should coerce
			return f;
	}
	for (auto f:this->virtual_functions){
		if (f->name==n && f->fn_type->is_equal(sig,false,this->name))
			return f;
	}
	for (auto f:this->static_functions){
		if (f->name==n && f->fn_type->is_coercible(sig))
			return f;
	}
	return nullptr;
}
Expr*			ExprFnDef::last_expr()const{
	if (auto b=body->as_block()) return b->argls.back();
	else return body;
}

void ExprFnDef::push_body(Expr* e,bool front){
	if (!body)body=e;
	else{
		convert_body_to_compound();
		ASSERT(body->as_compound());
		if (front)
			body->as_compound()->argls.push_front(e);
		else
			body->as_compound()->argls.push_back(e);
	}
}
ExprCompound* ExprFnDef::convert_body_to_compound(){
	if (!this->body){
		this->body=new ExprCompound(this->pos);
	}
	if (auto b=this->body->as_compound()){
		return b;
	}
	auto b=new ExprCompound(this->pos);
	b->argls.push_back(this->body);
	this->body=b;
	return b;
}

void emit_local_vars(CodeGen& cg, Expr* n, ExprFnDef* fn, Scope* sc) {
	auto ofp=cg.ofp;
	if (fn){
		for (auto cp=fn->captures; cp;cp=cp->next_of_from){
			cp->reg_name=next_reg_name(cp->tyname(), &cg.m_next_reg);
			cg.emit_alloca_type(cp, cp->type()->deref_all());
		}
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


CgValue ExprFnDef::compile(CodeGen& cg,Scope* outer_scope, CgValue input){
	auto fn_node = this;
	auto ofp=cg.ofp;
	
	if (!fn_node){return CgValue();}
	if (!fn_type){return CgValue();}	// hasn't been resolved - probably a trait-function decl
	if (fn_node->is_undefined()) {
		cg.emit_comment("fn %s prot",getString(fn_node->name));
		cg.emit_function_signature(fn_node,EmitDeclaration);
		return CgValue();
	}
	if (fn_node->is_generic()) {
		cg.emit_comment("fn %s generic:-",getString(fn_node->get_mangled_name()));
		for (auto f=fn_node->instances; f;f=f->next_instance){
			cg.emit_comment("fn %s generic instance",getString(fn_node->get_mangled_name()));
			f->compile(cg,outer_scope,input);
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
		cp->compile(cg,outer_scope,input);
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
	auto rtn=fn_node->get_return_expr();

	if (fn_node->fn_type->name==CLOSURE){
		auto cp=fn_node->my_capture;
		if (cp){
			cp->reg_name=cp->get_reg_new(cg);
			cp->reg_name =cg.emit_cast_reg(__ENV_I8_PTR, cg.i8ptr(),cp->type()).reg;
		}
	}
	// bind arguments to patterns
	
	for (auto arg:fn_node->args){
		if (arg->pattern && arg->name!=arg->pattern->as_just_name()){
			dbg(printf("arg %s bind to ",str(arg->name)));dbg(arg->pattern->dump_if(0));dbg(newline(0));
			arg->pattern->compile(cg,scope, scope->find_scope_variable(arg->name)->compile(cg,scope,CgValue()));
		}
	}
	auto retval=fn_node->body->compile(cg,scope);
	fn_node->get_scope()->compile_destructors(cg);
	cg.emit_return(retval);
	cg.emit_nest_end("}\n");
	cg.curr_fn=0;
	return CgValue(fn_node);
}
CgValue compile_function_call(CodeGen& cg, Scope* sc,CgValue recvp, const Expr* a_receiver, const ExprBlock* e){
	// [1.1]evaluate arguments
	Vec<CgValue> l_args;
	auto receiver=const_cast<Expr*>(a_receiver);
	
	/// TODO - why isn't recvp=receiver->compile() ??
	// process function argumetns & load
	if (receiver){	// optional 1st arg for method calls, operator new..
		recvp=receiver->compile(cg,sc);
		dbg3(receiver->dump(0));dbg3(newline(0));
		dbg3(recvp.dump());dbg3(newline(0));
	}
	if (recvp.is_valid()){
//		recvp=recr;
		l_args.push_back( recvp /*cg.load(recr,recr.type) old behaviour - autoload args - not now because we have REF args too*/ );
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
		l_args.push_back(reg /*cg.load(reg,arg->type()) Old behaviour - autoload args. not now because we have REF args*/);
	}
	auto original_args=l_args;
	
	//[1.2] evaluate call object..
	auto call_fn=e->get_fn_call();
	cg.emit_comment("fncall %s", call_fn?str(call_fn->name):e->call_expr->name_str());
	
	//[1.3] argument conversions..
	auto coerce_args=[&](const Type* fn_type){
		// todo - should not be needed
		const_cast<Type*>(fn_type)->resolve_if(sc, nullptr, 0);
		auto ai=0;
		dbg2(fn_type->dump(-1);newline(0));
		auto fn_arg=fn_type->fn_args_first();
		int i=0;
		for (; fn_arg; i++,fn_arg=fn_arg->next){
			dbg2(printf("arg %d \n", i);dbg2(fn_arg->dump(-1)));dbg2(newline(0));
			auto ae=i==0&&receiver?receiver:e->argls[i-(receiver?1:0)];
			auto r=cg.emit_conversion(ae,l_args[i], fn_arg,sc);
			l_args[i]=r;
		}
		//C-like variadic-args 'coerce to self' - needed for trivial refs->values
		for (;i<l_args.size();i++){
			auto ae=e->argls[i-(receiver?1:0)];
			auto r=cg.emit_conversion(ae, l_args[i], l_args[i].type, sc);
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
	auto ret_val=CgValue();
	//[1.4] make the call..
	if (e->call_expr->is_function_name()) {
		//[1.4.1] Calls where the fn name is a compile time symbol
		auto fn_name=e->call_expr->name;
		ExprStructDef* vts=nullptr;
		ArgDef* vtable_fn=nullptr;
		CgValue vtable;
		if (receiver) {
			// lookup types to see if we have a vcall.
			//ASSERT(recvp.type->is_pointer() && "haven't got auto-ref yet for receiver in a.foo(b) style call\n");
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
			// [1.4.1] vtable call TODO also check for __data_ptr for 'trait-objects'
			// we have a vcall, so now emit it..
			// load the vtable
			dbg_vcall("emit vcall %p %s.%s\n",vts, str(vts->name),str(vtable_fn->name_str()));
			auto vtbl=cg.emit_getelementref(recvp,__VTABLE_PTR);
			auto function_ptr=cg.emit_getelementval(vtbl,fn_name);
			coerce_args(function_ptr.type);
			cg.emit_call_begin(function_ptr);
			l_emit_arg_list(CgValue());
			ret_val= cg.emit_call_end();
			
		} else {
			if (!call_fn){
				e->dump(0);
				error(e,"call not resolved\n");
			}
			//[1.4.2] Direct Call
			coerce_args(call_fn->type());
			cg.emit_call_begin(CgValue(call_fn));
			l_emit_arg_list(CgValue());
			ret_val= cg.emit_call_end();
		}
	} else {
		//[1.4.2] Indirect Call... Function Object
		auto fn_obj = e->call_expr->compile(cg, sc);
		auto call_t=e->call_expr->type();
		coerce_args(call_t);
		if (fn_obj.type->is_closure()){
			//[.1] ..call Closure (function,environment*)
			auto fn_ptr=cg.emit_getelementval(fn_obj,0,0,call_t);
			auto envptr = cg.emit_getelementval(fn_obj,0,1,cg.i8ptr());
			cg.emit_call_begin(fn_ptr);
			l_emit_arg_list(envptr);
			ret_val= cg.emit_call_end();
		}else{
			//[.2] ..Raw Function Pointer
			cg.emit_call_begin(fn_obj.load(cg));
			l_emit_arg_list(CgValue());
			ret_val= cg.emit_call_end();
		}
	}
	// compile destructors for arguments. TODO - need to flag 'rvalues' . how? *this* is an rvalue, as is op result?
	for (auto i=0;i<original_args.size();i++){
		dbg2(original_args[i].dump());
	}
	dbg2(newline(0));
	for (auto a:original_args){
		cg.compile_destructor(sc, a,false);
	}
	ret_val.rvalue=true;
	dbg2(ret_val.dump());dbg2(newline(0));
	return ret_val;
}




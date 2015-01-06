#include "exprblock.h"

void ExprBlock::find_vars_written(Scope* s, set<Variable*>& vars) const{
	this->call_expr->find_vars_written_if(s, vars);
	for (auto a:argls)
		a->find_vars_written(s,vars);
}
Name ExprBlock::get_fn_name()const
{	// distinguishes operator &  function call
	if (call_expr){
		ASSERT(call_expr->as_ident()&& "TODO: distinguish expression with computed function name");
		return call_expr->name;
	}
	else if (get_fn_call()){return get_fn_call()->name;}
	else return 0;
}

void ExprBlock::gather_symbols(Scope* outer_sc){
	// bits declared in a functoin dont get gathered. only structs(nested classes,methods), and blocks
	// TODO - this is indiscriminate, should only make scopes if there are definitions inside.
//	outer_sc->make_inner_scope(&this->scope,outer_sc->owner_fn,this);
	auto sc=outer_sc;
	this->call_expr->gather_symbols_if(sc); // ambiguous,  where vs rest?
	for (auto x:argls){
		x->gather_symbols(sc);
	}
}
void ExprBlock::dump(PrinterRef depth) const {
	if (!this) return;
	newline(depth);
	auto b=this->bracket_delim();
//	dbprintf("%s",this->kind_str());
	this->call_expr->dump_if(-100);
	dbprintf("%c",b[0]);
	int i=0;
	for (const auto x:this->argls) {
		if(i){
			dbprintf("%c",b[1]);
		}
		dbg4(newline(depth+1);dbprintf("(%d/%d)",i,this->argls.size()););
		if (x) {x->dump(depth+2);}else{dbprintf("(none)");}
		i++;
	}
	newline(depth);dbprintf("%c",b[2]);
	if (this->get_type()){dbprintf(":");this->get_type()->dump_if(-1);}
}

void ExprBlock::recurse(std::function<void (Node *)>& f){
	if (call_expr){call_expr->recurse(f);}
	for(auto x:argls){x->recurse(f);}
	type()->recurse(f);
}

ExprBlock::ExprBlock(const SrcPos& s){ pos=s;}
ExprFnDef* ExprBlock::get_fn_call()const {
	if (!this->def)
		return nullptr;
	auto d=this->def->as_fn_def();
	if (d)
		return d;
	return nullptr;
}

void
ExprBlock::create_anon_struct_initializer(){
	// concatenate given names & argcount as the identifer
	// make it generic over types.
	char tmp[256]="anon_";
	for (auto i=0; i<argls.size();i++){
		auto p=dynamic_cast<ExprOp*>(argls[i]);
		if (!p || !(p->name==ASSIGN||p->name==COLON)){
			error(this,"anon struct initializer must have named elements {n0=expr,n1=expr,..} or{n0:expr,n1:expr,...}");
		}
		if (i) strcat(tmp,"_");
		strcat(tmp,str(p->lhs->as_name()));
	}
	// TODO - these need to be hashed somewhere, dont want each unique!
	ExprStructDef* sd=new ExprStructDef(this->pos,0);
	sd->name=getStringIndex(tmp);
	ASSERT(sd->type()==0&&"todo-struct def creates its own type");
	sd->set_type(new Type(sd));
	sd->name=getStringIndex(tmp);
	for (auto i=0; i<argls.size();i++){
		auto a=argls[i];
		auto nf=new ArgDef(a->pos, a->as_op()->lhs->as_name(),a->type());
		sd->fields.push_back(nf );
	}
	this->call_expr=sd;
	this->def=sd;
	this->set_type(sd->get_type());
}
void ExprBlock::verify(){
	verify_expr_block(this);
	if (this->call_expr) this->call_expr->verify();
	for (auto x:argls) x->verify();
}
void ExprBlock::translate_tparams(const TParamXlat& tpx){
	this->call_expr->translate_typeparams_if(tpx);
	for (auto e:argls){
		e->translate_tparams(tpx);
	}
	this->type()->translate_typeparams_if(tpx);
}

CgValue ExprBlock::compile(CodeGen& cg,Scope *sc, CgValue input) {
	if (!argls.size())
		return CgValue();

	for (int i=0; i<argls.size()-1; i++){
		this->argls[i]->compile(cg,sc);
	}

	auto ret= this->argls.back()->compile(cg,sc);

	//TODO- how to invoke destructors for all values
	dbg_raii(dbprintf("this=%s:%s; scope %p %s;  this->scope %p \n", this->name_str(),this->kind_str(),sc,sc->name_str(), this->get_scope()));
	this->get_scope()->compile_destructors_if(cg);
	
	return ret;
}



bool ExprBlock::is_undefined() const{
	if (!this) return false; //only presencence of "_" is undefined.
	for (auto x:argls){
		if (x->is_undefined())
			return true;
	}
	return false;
}
Node* ExprBlock::clone() const {
//	if (!this) return nullptr;
	return (Node*)clone_sub(new ExprBlock());
}
ExprBlock* ExprBlock::clone_sub(ExprBlock* r)const{
	r->pos=this->pos;
	if (this->call_expr) {
		r->call_expr = (Expr*) this->call_expr->clone();
	}
	r->set_type((Type*)this->get_type()->clone_if());
	r->def=nullptr;//this->def; - instantiating generic: it needs to be resolved again
	r->name=this->name;
	r->argls.resize(this->argls.size());
	for (int i=0; i<this->argls.size(); i++) {
		r->argls[i]=(Expr*)(this->argls[i]->clone());
	}
	return r;
}
ResolveResult	ExprCall::resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op){

	dbg2("desugaring operator new\n");
	op->name=DOT;
	op->lhs =new ExprOp(NEW ,op->pos, op->lhs, new ExprStructInit(op->pos, (Expr*)this->call_expr->clone()));
	//op->rhs(this) unchanged, it calls the new
	op->dump(0);
	resolved|=op->resolve(sc,desired,flags);
	op->dump(0);

	return resolved|INCOMPLETE;
}
ExprCall::ExprCall(SrcPos sp, Name fname):ExprCall(sp,new ExprIdent(sp,fname),nullptr,nullptr,nullptr){
}

ExprCall::ExprCall(SrcPos sp, Name fname, Expr* arg1):ExprCall(sp,new ExprIdent(sp,fname),arg1,nullptr,nullptr){
}
ExprCall::ExprCall(SrcPos sp, Name fname, Expr* arg1,Expr* arg2):ExprCall(sp,new ExprIdent(sp,fname),arg1,arg2,nullptr){
}

ExprCall::ExprCall(SrcPos sp, Expr* call, Expr* arg1, Expr* arg2,Expr* arg3){
	this->pos=sp;
	this->call_expr=call;
	if (arg1){
		this->argls.push_back(arg1);
		if (arg2){
			this->argls.push_back(arg2);
			if (arg3){
				this->argls.push_back(arg3);
			}
		}
	}
}
ExprCall::ExprCall(SrcPos sp, ExprFnDef* f, Expr* arg1, Expr* arg2):ExprCall(sp,new ExprIdent(sp,f->name),arg1,arg2,nullptr){
	// todo - explicit FnRef node.
	this->call_expr->set_def(f);
}

ResolveResult	ExprStructInit::resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op){
	auto b=this;
	if (!desired && !op->get_type() && op->rhs->get_type()) {
		op->set_type( new Type(op,PTR,(Type*)b->get_type()->clone()) );
	}
	if (op->get_type()){
		op->propogate_type_refs(flags, op->get_type()->sub, b->type_ref());
	}
	resolved|=b->resolve_if(sc, desired?desired->sub:nullptr, flags);
	this->type()->set_rvalue();
	return resolved;
}


ResolveResult	ExprSubscript::resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op){
	auto b=this;
	if (!desired && !op->get_type() && op->rhs->get_type()) {
		op->set_type( new Type(op,PTR,(Type*)b->get_type()->clone()) );
	}
	if (op->get_type())
		op->propogate_type_refs(flags, op->get_type()->sub, b->type_ref());
	
	resolved|=b->call_expr->resolve_if(sc,op->get_type()?op->get_type()->sub:nullptr,flags);
	resolved|=b->argls[0]->resolve_if(sc,op->get_type()?op->get_type()->sub:nullptr,flags);
	b->set_type(b->call_expr->get_type());
	return resolved;
}

ResolveResult	ExprSubscript::resolve(Scope* sc, const Type* desired,int flags){
	if (this->type()) this->type()->resolve_if(sc,nullptr,flags);
		this->def->resolve_if(sc, nullptr, flags);
	// array indexing operator TODO: check this isn't itself a Type, if we want templates anywhere.
	resolved|=this->call_expr->resolve_if(sc,nullptr,flags); // todo - it could be _[desired]. forward should give possibilities
	if (auto t=call_expr->type()){
		ASSERT(t->is_array()||t->is_pointer());
		for (auto i=0; i<argls.size(); i++)  {
			resolved|=argls[i]->resolve_if(sc,nullptr,flags&!R_PUT_ON_STACK ); // TODO any indexing type? any type extracted from 'array' ?
		}
		const Type* array_elem_type=t->sub;
		propogate_type_fwd(flags,this, array_elem_type);
		return propogate_type_fwd(flags,this, desired);
	} else
		return resolved|=INCOMPLETE;
}
CgValue ExprSubscript::compile(CodeGen& cg,Scope *sc, CgValue input) {
	auto ar=this;
	auto expr=ar->call_expr->compile(cg,sc);// expression[index]
	auto index=ar->argls[0]->compile(cg,sc);
	/// TODO , this is actually supposed to distinguish array[ n x T ] from pointer *T case
	// TODO: abstract this into codegen -getelementref(CgValue ptr,CgValue index);
	return cg.emit_get_array_elem_ref(expr, index);
}


ResolveResult	ExprTuple::resolve(Scope* sc, const Type* desired,int flags){
	if (this->type()) this->type()->resolve_if(sc,nullptr,flags);
	this->def->resolve_if(sc, nullptr, flags);

	for (index_t i=0; i<this->argls.size(); i++) {
		auto desired_sub=desired?desired->get_elem(i):nullptr;
		resolved|=this->argls[i]->resolve_if(sc,desired_sub,flags);
	}
	// todo: we need to get better at filling in the gaps.
	this->set_tuple_component_types();
	return propogate_type_fwd(flags, desired,this->type_ref());
}
void ExprTuple::set_tuple_component_types(){
	if (!this->get_type()) {
		bool typed=true;
		for (index_t i=0; i<this->argls.size();i++){
			auto ct=this->argls[i]->get_type();
			if (!ct) typed=false;
		}
		if (typed){
			auto t=new Type(this, TUPLE);
			for (index_t i=0; i<this->argls.size();i++){
				auto ct=this->argls[i]->get_type();
				if (!ct) ct=(Type*)Type::get_auto()->clone();
				t->push_back(ct);
			}
			t->set_def(t);
			this->set_type(t);
		}
	}
}
CgValue ExprTuple::compile(CodeGen& cg,Scope *sc, CgValue input) {
	auto tuple=cg.emit_alloca_type(this, this->type());
	for (index_t i=0; i<this->argls.size(); i++){
		auto val=this->argls[i]->compile(cg,sc);
		auto elem=tuple.get_elem_index(cg,i);
		elem.store(cg,val);
	}
	return tuple;
}
// these would be less verbose if they desugared to primitives, but we'd be allocating during compile passes.
// compute types -> use types to emit code seems ok,?

CgValue ExprTuple::compile_operator_dot(CodeGen& cg, Scope* sc, const Type* t, const Expr* a_lhs) {
	// TODO ensure this makes *refs* in an lvalue position so we can do expr.(x,y,z)=(..,..,..);
	auto lhs=const_cast<Expr*>(a_lhs);
	auto tuple=cg.emit_alloca_type(this, this->type());
	for (index_t i=0; i<this->argls.size(); i++){
		auto val=this->argls[i]->compile_operator_dot(cg, sc, this->argls[i]->type(), a_lhs);
		auto elem=tuple.get_elem_index(cg,i);
		elem.store(cg,val);
	}
	return tuple;

}

ResolveResult	ExprTuple::resolve_operator_dot(Scope *sc, const Type *desired, int flags, Expr *lhs,Type*& tref) {
	// tuple is ..
	auto subt=desired?desired->sub:nullptr;
	// brute force sorry. complete mess. if type was a vector tree this would be easier. inference needs a ref location to update. however types are stored as linklists, only suiting traversal.
	Vec<Type*> elem_ts; elem_ts.resize(argls.size());
	if (auto tt=this->get_type()){
		index_t i=0;
		for (auto tsubt=tt->sub;tsubt;tsubt=tsubt->next,i++){
			elem_ts[i]=tsubt;
		}
		// take ownership;
		i=0;
		for (auto tsubt=tt->sub;tsubt;tsubt=tsubt->next,i++){
			elem_ts[i]->next=nullptr;
		}
		tt->sub=nullptr;
	}
	// resolve tuple components..
	for (index_t i=0; i<argls.size(); i++,subt=subt?subt->next:nullptr){
		if (!elem_ts[i] && subt)
			elem_ts[i]=(Type*)subt->clone();// inference will write to it, fill in type gaps..
		else if (elem_ts[i] && subt){
			propogate_type_fwd(flags, subt, elem_ts[i]);
		}
		Type* tmp_tref=0;
		resolved|=argls[i]->resolve_operator_dot(sc,subt,flags,lhs, tmp_tref);
		if (tmp_tref && !elem_ts[i]){
			elem_ts[i]=(Type*)tmp_tref->clone();
		} // else fill in the gaps.
	}
	// put components back into linklist form . eugh.
	if (!this->get_type()){
		this->set_type(new Type(this, TUPLE));
	}
	for (index_t i=0; i<this->argls.size();i++){
		this->get_type()->push_back(elem_ts[i]);
	}
	return resolved|this->propogate_type_refs(flags, this->type_ref(),tref);
}

ResolveResult ExprBlock::resolve(Scope* sc, const Type* desired, int flags) {
	return this->resolve_sub(sc,desired,flags,nullptr);
}

ResolveResult ExprBlock::resolve_sub(Scope* sc, const Type* desired, int flags,Expr* receiver) {
	verify_all();
	this->get_scope()->resolve();
	if (this->type()) this->type()->resolve_if(sc,nullptr,flags);
	this->def->resolve_if(sc, nullptr, flags);
	// RVO
	
	/// loose end? if this is a method-call, we dont resolve the symbol here,
	/// in other contexts we do
	if (this->call_expr &&!receiver)
		this->call_expr->resolve_if(sc,nullptr,flags);
	::verify(this->get_type());

	ExprIdent* p=nullptr;

	if (!this->argls.size()) {
		if (!this->get_type()) this->set_type(new Type(this,VOID));
		return propogate_type_fwd(flags, desired,this->type_ref());
	}
	int	i_complete=-1;
	if (!(flags & R_REVERSE_ONLY)){
		for (auto n=0; n<(int)this->argls.size()-1; n++) {
			resolved|=this->argls[n]->resolve_if(sc,0,flags);
			if (resolved==COMPLETE)
				i_complete=n;
		}
	}
	propogate_type_fwd(flags,this, desired);
	resolved|=this->argls.back()->resolve_if(sc,desired,flags);
	if (i_complete>=this->argls.size()-1){
		dbg(printf("icomplete stuff works"));
	}
	// reverse pass too
	if (!(flags & R_FORWARD_ONLY)){
		for (auto n=(int)this->argls.size()-2;n>i_complete; n--) {
			resolved|=this->argls[n]->resolve_if(sc,0,flags);
		}
		#if DEBUG>=2
		auto resolved2=(char)COMPLETE;
		for (auto n=i_complete; n>=0; n--){
			auto a=this->argls[n];
			resolved2|=a->resolve_if(sc,0,flags);
			resolved|=resolved2;
			if (resolved2!=COMPLETE){
				error(this,"ICE,node %s in %s was falsely declared complete",a->name_str(),a->kind_str());
			}
		}
#endif
	}
#if DEBUG>=2
	if(i_complete>0 &&0==(flags&(R_FORWARD_ONLY|R_REVERSE_ONLY))){
		dbprintf("%d / %d\n", i_complete,this->argls.size());
	}
#endif
	dbg(this->type()->dump_if(-1));
	dbg(this->argls.back()->dump_if(-1));
	dbg(newline(0));

	// RVO - this is ov
	if (this->argls.size()){
		if (auto d=this->argls.back()->def){
			if (auto v=d->as_variable())
				v->return_value=true;
		}
	}
	this->type()->set_rvalue();

	return propogate_type_refs(flags,// order matters:
						this->argls.back()->type_ref(),//coerce from
						this->type_ref());			// <-coerce to
}

ResolveResult ExprCall::resolve(Scope* sc, const Type* desired, int flags) {
	this->call_expr->def->dump_if(0);
	auto r=this->resolve_call_sub(sc,desired,flags,nullptr);
	if (this->type()){
		this->type()->set_rvalue();
	}

	return r;
}
ResolveResult ExprCall::resolve_call_sub(Scope* sc, const Type* desired, int flags,Expr* receiver) {
	// TODO: distinguish 'partially resolved' from fully-resolved.
	// at the moment we only pick an fn when we know all our types.
	// But, some functions may be pure generic? -these are ok to match to nothing.
	// todo:
	//		auto n=num_known_arg_types(this->argls);
	if (call_expr->name==NAMEOF && strlen(str(this->argls[0]->name))>1) {
		auto src=this->argls[0];
		if (!this->type()){ this->set_type(new Type(this,STR));};
		char tmp[512];
		sprintf(tmp,"%s",str(src->name));
		this->call_expr=0;
		this->argls.resize(1);
		this->argls[0]=new ExprLiteral(src->pos,tmp,(int)strlen(tmp));
		resolved|=this->argls[0]->resolve_if(sc,nullptr,0);
		this->set_type(src->get_type());
		return resolved;
	}
	bool indirect_call=false;
	auto call_ident=this->call_expr->as_ident();
	if (call_ident){
		if (sc->find_fn_variable(this->call_expr->as_name(),nullptr))
			indirect_call=true;
	}else {
		indirect_call=true;
	}
	Type* fn_type=nullptr;
	if (receiver || indirect_call) {
	}
	else{
		// an ident can't be just resolved like this
		resolved|=this->call_expr->resolve_if(sc,nullptr,flags|R_CALL);
		fn_type=this->call_expr->type();
		
		//			fn_type_r=this->call_expr->resolve(sc,nullptr,flags);
		//		} else {
		//			fn_type_r=this->
	}
	//		auto fn_type=indirect_call?nullptr:fn_type_r.type;
	
	int arg_index=0;
	if (fn_type) {
		// propogate types we have into argument expressions
		for (auto a=fn_type->fn_args_first(); arg_index<argls.size() && a; arg_index++,a=a->next)  {
			if (a->name==FN){
				dbg_lambdas("resolving fn type into function argument %s\n", argls[arg_index]->name_str());
			}
			resolved|=argls[arg_index]->resolve(sc,a,flags);
		}
		for (;arg_index<argls.size(); arg_index++){ // variadic args.
			resolved|=argls[arg_index]->resolve_if(sc,nullptr,flags);
#if DEBUG >=2
			dbprintf("resolve variadic C arg[%d]\n",arg_index);
			argls[arg_index]->type()->dump_if(0);newline(0);
#endif
		}
		const Type* fr=fn_type->fn_return();
		propogate_type_fwd(flags,this, fr);
	} else
		for (auto i=0; i<argls.size(); i++)  {
			resolved|=argls[i]->resolve_if(sc,nullptr,flags );
		}
	
	if (!this->get_fn_call()){
		
		for (auto i=0; i<argls.size(); i++)  {
			resolved|=argls[i]->resolve_if(sc,nullptr ,flags);
		}
		if (this->call_expr->is_ident() && 0==dynamic_cast<Variable*>(this->call_expr->def)){

//		if (this->call_expr->is_ident() && 0==this->call_expr->def->as_variable()){
			return resolve_make_fn_call(receiver,this, sc,desired,flags);
		}
	} else if (auto fnc=this->get_fn_call()){ // static call
		int ofs=(receiver)?1:0;
		if (receiver) {
#if DEBUG>=2
			dbprintf("receiver+ %d args; call %s with %d args\n",argls.size(), fnc->name_str(), fnc->args.size());
#endif
			resolved|=receiver->resolve_if(sc,fnc->args[0]->type(),flags);
		}
		for (auto i=0; i<(argls.size()); i++)  {
			//int i=srci+ofs;
			auto ii=i+ofs;
			auto fnarg=ii<fnc->args.size()?fnc->args[ii]:nullptr;
			resolved|=argls[i]->resolve_if(sc,fnarg?fnarg->type():nullptr ,flags);
		}
		return propogate_type_fwd(flags, desired,this->get_fn_call()->ret_type);
	}
	else {
		if (flags & R_FINAL)
			if (!this->type())
				error(this,"can't call/ type check failed %s",this->call_expr->name_str());
		
		return resolved;
	}
	return resolved;
}
CgValue ExprCall::compile(CodeGen& cg,Scope *sc, CgValue input) {
	return compile_function_call(cg,sc,CgValue(),nullptr,this);
}

ResolveResult	ExprArrayInit::resolve_operator_new(Scope *sc, const Type *desired, int flags, ExprOp *op){
	error(this,"todo array initializer\n");
	return INCOMPLETE;
}

CgValue ExprArrayInit::compile(CodeGen& cg,Scope *sc, CgValue input) {
	error(this,"todo array initializer\n");
	return CgValue();
}

ResolveResult
ExprStructInit::resolve(Scope* sc,const Type* desired,int flags){
	if (this->type()) this->type()->resolve_if(sc,nullptr,flags);
	this->def->resolve_if(sc, nullptr, flags);
	this->call_expr->resolve_if(sc,nullptr,flags);

	dbg(this->type()->dump_if(-1));dbg(newline(0));
	auto si=StructInitializer(sc,this);
	return si.resolve(desired,flags);
}

ResolveResult StructInitializer::resolve(const Type* desiredType,int flags) {
	
	ExprStructDef* sd=nullptr;
#if DEBUG >=2
	dbprintf("\n===================\nstruct init: %s:",si->call_expr->name_str());
	si->call_expr->type()->dump_if(-1);
	dbprintf("\tdesired:");desiredType->dump_if(-1);newline(0);
	
	auto sdn=sc->find_struct_named(si->call_expr->name);
	sdn->dump_instances(0);
	
#endif
	if (si->call_expr->name==PLACEHOLDER && desiredType){
		si->propogate_type_fwd(flags, desiredType,si->call_expr->type_ref());
		sd=si->call_expr->type()->def->as_struct_def();
		if (!sd)
			return si->resolved|INCOMPLETE;
		dbg(sd->dump(-1));
		dbg_type("\n");
		si->call_expr->set_type(desiredType);
		if (!si)
			si->set_type(desiredType);
	}
	else {
		sd=sc->find_struct(si->call_expr);
		dbg(sd->dump_if(0));
		if (!sd){
			if (flags&R_FINAL){
				error_begin(si->call_expr,"can't find struct");
				si->call_expr->dump(-1);error_end(si->call_expr);
			}
			return si->resolved|INCOMPLETE;
		}
	}
	dbg3(printf("=====struct init & desired type..=====\n"));
	dbg3(desiredType->dump_if(0));
	dbg3(sd->dump(0));
	// if its in place..
	auto local_struct_def=si->call_expr->as_struct_def();
	if (local_struct_def){
		sc->add_struct(local_struct_def); // todo - why did we need this?
		sd=local_struct_def;
	}
	if (sd->is_generic()){
		dbprintf_tparams("matching with generic struct: %s, needs instantiating\n",sd->name_str());
		MyVec<TParamVal*> match_tparams;
		match_struct_tparams(match_tparams, sd, si->argls, si);
		if (si->type()){
			int i=0;
			for (auto subt=si->type()->sub; subt; subt=subt->next,i++){
				if (!match_tparams[i] && subt->name!=AUTO)
					match_tparams[i]=subt;
			}
		}
		dump_tparams(sd->tparams,&match_tparams);
		dbprintf_tparams("\n");
		// TODO - Type Hash.
		auto st=(Type*)sd->get_struct_type_for_tparams(match_tparams);
		dbg_tparams(st->dump(-1));
		sd=sd->get_instance(sc,st);
		dbg_tparams(sd->dump(0));
		dbg_tparams(if (sd->inherits){sd->inherits->dump(0);});
		dbprintf_tparams("\n");
		si->propogate_type_refs(flags, si->call_expr->type_ref(), st);
		si->propogate_type_refs(flags, si->call_expr->type_ref(), st);
//		si->call_expr->set_type(st);
		si->call_expr->set_def(sd);
//		si->set_def(sd);
//		si->set_type(st);
		
		
	}
	//if (!si->type()){
	//	si->set_type(new Type(sd));
	//}
	si->propogate_type_refs(flags, si->type_ref(),si->call_expr->type_ref());
	si->propogate_type_fwd(flags,si, desiredType);
	
	si->call_expr->set_def(sd);
	si->def=sd;
	this->struct_def=sd;
	// assignment forms are expected eg MyStruct{x=...,y=...,z=...} .. or can we have MyStruct{expr0,expr1..} equally?
	//int next_field_index=0;
	// todo:infer generic tparams - adapt code for functioncall. we have struct fields & struct type-params & given expressions.
	int named_field_index=-1;
	// todo encapsulate StructInitializer to reuse logic for codegen
	field_indices.reserve(si->argls.size());
	//step past the hidden automatically setup fields
	int field_index=sd->first_user_field_index();
	for (auto i=0; i<si->argls.size(); i++)  {
		auto a=si->argls[i];
		auto op=dynamic_cast<ExprOp*>(a);
		ArgDef* field=nullptr;
		Type*t = nullptr;
		if (op&&(op->name==FIELD_ASSIGN)){
			field=sd->find_field(op->lhs);
			si->resolved|=op->rhs->resolve_if(sc,field->type(),flags); // todo, need type params fwd here!
			si->propogate_type_refs(flags,op, op->lhs->type_ref(),op->rhs->type_ref());
			//				propogate_type(flags,op,op->rhs->type_ref());
			op->lhs->def=field;
			named_field_index=sd->field_index(op->lhs);
			this->value.push_back(op->rhs);
			t=op->rhs->type();
			si->propogate_type_refs(flags,op,field->type_ref(),op->rhs->type_ref());
		}else if (named_field_index==-1){
			if (field_index>=sd->fields.size()){
				error(a,sd,"too many fields");
			}
			field=sd->fields[field_index++];
			this->value.push_back(a);
			dbg3(field->dump(0));dbg(printf("\n --set_to--> \n"));dbg(a->dump(0));dbg(newline(0));
			a->resolve(sc,field->type(),flags); // todo, need generics!
			t=a->type();
			si->propogate_type_refs(flags,a,field->type_ref(),a->type_ref());
		}else{
			error(a,"named field expected");
		}
		this->field_refs.push_back(field);
		this->field_indices.push_back(field_index);
		if (local_struct_def){
			// special case :( if its' an inline def, we write the type. doing propper inference on generic structs have solved this stupidity.
			if (!local_struct_def->fields[i]->type()){
				local_struct_def->fields[i]->type()=t;
			}
		}
	}
	//	?. // if (this) return this->.... else return None.
	return si->propogate_type_fwd(flags,si, desiredType);
}


CgValue ExprStructInit::compile(CodeGen& cg,Scope *sc, CgValue input) {
	ASSERT(!input.is_valid());
	return compile_struct_init(cg,sc,0);
}

CgValue compile_struct_init_args(ExprStructInit* e,CodeGen& cg, Scope* sc, RegisterName force_dst) {
	StructInitializer si(sc,e); si.map_fields();
	auto dbg=[&](){e->type()->dump(0);newline(0);};
	auto struct_val= force_dst?CgValue(0,e->type(),force_dst):cg.emit_alloca_type(e, e->type());
	e->reg_name=struct_val.reg; // YUK todo - reallyw wanted reg copy
	// can we trust llvm to cache the small cases in reg..
	if (e->argls.size()!=si.value.size())
		dbprintf("warning StructInitializer vs argls mismatch, %d,%d\n",e->argls.size(),si.value.size());
	auto sd=si.get_struct_def();
//	auto sderef=struct_val.type->deref_all();
//	auto sd=sderef->def->as_struct_def();//struct_def();
//	sd->dump(0);dbprintf("num fields=%d %s %d %d\n", sd->fields.size(), str(sd->fields[0]->name),(int)(sd->fields[0]->name), __DISCRIMINANT);

	if (sd->m_is_variant){
		auto ni=sd->get_elem_index(__DISCRIMINANT);
		auto dis=struct_val.get_elem_index(cg,ni);
		cg.emit_store_i32(dis, sd->discriminant);
	}
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

CgValue ExprStructInit::compile_struct_init(CodeGen& cg,Scope *sc, RegisterName force_dst) {
	return compile_struct_init_args(this, cg,sc,force_dst);
}

CgValue ExprCall::compile_operator_new(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs){
	auto new_ptr=cg.emit_malloc(t,1);	// init(alloc(), args);
	
	auto r= compile_function_call(cg, sc, new_ptr, nullptr, this);

	return new_ptr;
}

CgValue ExprStructInit::compile_operator_new(CodeGen &cg, Scope *sc, const Type* t, const Expr *lhs){
//	dbg2(t->dump(0));
	auto reg=cg.emit_malloc(t,1);
	auto st=this->call_expr->type()->get_struct_autoderef();
	if (st->vtable){
		auto vtref=cg.emit_getelementref(reg,__VTABLE_PTR);
		cg.emit_store_global(vtref, st->vtable_name );
	}
	if (st->m_is_variant){
		auto dref=cg.emit_getelementref(reg,__DISCRIMINANT);
		cg.emit_store_i32(dref, st->discriminant );
	}
	this->compile_struct_init(cg,sc,reg.reg);
	return reg;
}
CgValue ExprSubscript::compile_operator_new(CodeGen &cg, Scope *sc, const Type* t,const Expr *lhs){
	// todo: multiply for multidimentional array?
	if (argls.size()==1){
		// TODO invoke constructors
		auto num=argls[0]->compile(cg,sc);
		return cg.emit_malloc_array(t,num);
	} else{
		error(this,"new [], only 1 dimension works");
		return CgValue();
	}
}

CgValue
ExprCall::compile_operator_dot(CodeGen& cg, Scope* sc, const Type* t, const Expr* lhs)
{
	return compile_function_call(cg,sc,CgValue(),lhs,this);
}

ResolveResult
ExprCall::resolve_operator_dot(Scope *sc, const Type *desired, int flags, Expr *lhs,Type*& tref){
	auto method_name=this->call_expr->name;
	this->resolve_call_sub(sc, desired, flags, lhs);
	return propogate_type_refs(flags,this->type(),tref);
	
}

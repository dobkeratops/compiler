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

void ExprBlock::dump(int depth) const {
	if (!this) return;
	newline(depth);
	auto b="{\0}\0";
	dbprintf(b+0);
	dbprintf(this->kind_str());
	this->call_expr->dump_if(-100);
	for (const auto x:this->argls) {
		if (x) {x->dump(depth+1);}else{dbprintf("(none)");}
	}
	newline(depth);dbprintf(b+2);
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
	return this->argls.back()->compile(cg,sc);
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

	for (size_t i=0; i<this->argls.size(); i++) {
		auto desired_sub=desired?desired->get_elem(i):nullptr;
		resolved|=this->argls[i]->resolve_if(sc,desired_sub,flags);
	}
	// todo: we need to get better at filling in the gaps.
	if (!this->get_type()) {
		auto t=new Type(this, TUPLE);
		for (size_t i=0; i<this->argls.size();i++){
			auto ct=this->argls[i]->get_type();
			if (!ct) ct=Type::get_auto();
			t->push_back(ct);
		}
		t->set_def(t);
		this->set_type(t);
	}
	return propogate_type_fwd(flags,(Node*)this, desired,this->type_ref());
}
CgValue ExprTuple::compile(CodeGen& cg,Scope *sc, CgValue input) {
	auto tuple=cg.emit_alloca_type(this, this->type());
	for (int i=0; i<this->argls.size(); i++){
		auto val=this->argls[i]->compile(cg,sc);
		auto elem=tuple.get_elem_index(cg,i);
		elem.store(cg,val);
	}
	return tuple;
}

ResolveResult ExprBlock::resolve(Scope* sc, const Type* desired, int flags) {
	return this->resolve_sub(sc,desired,flags,nullptr);
}

ResolveResult ExprBlock::resolve_sub(Scope* sc, const Type* desired, int flags,Expr* receiver) {
	verify_all();
	if (this->type()) this->type()->resolve_if(sc,nullptr,flags);
	this->def->resolve_if(sc, nullptr, flags);
	
	/// loose end? if this is a method-call, we dont resolve the symbol here,
	/// in other contexts we do
	if (this->call_expr &&!receiver)
		this->call_expr->resolve_if(sc,nullptr,flags);
	::verify(this->get_type());

	ExprIdent* p=nullptr;

	if (!this->argls.size()) {
		if (!this->get_type()) this->set_type(new Type(this,VOID));
		return propogate_type_fwd(flags,this, desired,this->type_ref());
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

	return propogate_type_refs(flags,(const Node*)this, this->type_ref(),this->argls.back()->type_ref());
}

ResolveResult ExprCall::resolve(Scope* sc, const Type* desired, int flags) {
	return this->resolve_call_sub(sc,desired,flags,nullptr);
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
		resolved|=this->call_expr->resolve_if(sc,nullptr,flags);
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
		return propogate_type_fwd(flags,this, desired,this->get_fn_call()->ret_type);
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
		si->propogate_type_fwd(flags,si, desiredType,si->call_expr->type_ref());
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
	//if (!si->type()){
	//	si->set_type(new Type(sd));
	//}
	si->propogate_type_refs(flags,(Node*)si, si->type_ref(),si->call_expr->type_ref());
	si->propogate_type_fwd(flags,si, desiredType);
	
	si->call_expr->def=sd;
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
			si->propogate_type_refs(flags,op,op->lhs->type_ref(),op->rhs->type_ref());
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
			dbg3(field->dump(0));dbg(printf("\n --set_to--> \n"));dbg(a->dump());dbg(newline(0));
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
CgValue ExprStructInit::compile_struct_init(CodeGen& cg,Scope *sc, RegisterName force_dst) {
	auto e=this;
	StructInitializer si(sc,e); si.map_fields();
	auto dbg=[&](){e->type()->dump(0);newline(0);};
	auto struct_val= force_dst?CgValue(0,e->type(),force_dst):cg.emit_alloca_type(e, e->type());
	e->reg_name=struct_val.reg; // YUK todo - reallyw wanted reg copy
	// can we trust llvm to cache the small cases in reg..
	if (e->argls.size()!=si.value.size())
		dbprintf("warning StructInitializer vs argls mismatch, %d,%d\n",e->argls.size(),si.value.size());
	auto sd=si.get_struct_def();
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









#include "semantics.h"

const char** g_pp,*g_p;
const char* g_filename=0;

int g_debug_get_instance=false;

struct VTablePtrs {
	void* expr_op;
	void* expr_ident;
	void* expr_fn_def;
	void* expr_block;
	void* expr_tuple;
	void* expr_array_init;
	void* expr_struct_init;
	void* expr_call;
	void* expr_subscript;
	void* expr_parens;
	void* expr_compound;
	void* type;
}
g_vtable_ptrs;
Node* g_pRoot;
bool g_verbose=true;
void lazy_cache_vtable_ptrs(){
	if (g_vtable_ptrs.expr_op)
		return;
	auto p1=new ExprOp(0,SrcPos{},0,0);
	g_vtable_ptrs.expr_op=*(void**)p1;
	
	auto p2=new ExprIdent();
	g_vtable_ptrs.expr_ident=*(void**)p2;
	
	auto p3=new ExprFnDef();
	g_vtable_ptrs.expr_fn_def=*(void**)p3;
	
	auto p4=new ExprBlock();
	g_vtable_ptrs.expr_block=*(void**)p4;
	{auto p=new ExprStructInit();
		g_vtable_ptrs.expr_struct_init=*(void**)p;}
	{auto p=new ExprArrayInit();
		g_vtable_ptrs.expr_array_init=*(void**)p;}
	{auto p=new ExprTuple();
		g_vtable_ptrs.expr_tuple=*(void**)p;}
	{auto p=new ExprCall();
		g_vtable_ptrs.expr_call=*(void**)p;}
	{auto p=new ExprSubscript();
		g_vtable_ptrs.expr_subscript=*(void**)p;}
	{auto p=new ExprCompound();
		g_vtable_ptrs.expr_compound=*(void**)p;}
	{auto p=new ExprParens();
		g_vtable_ptrs.expr_parens=*(void**)p;}

	auto p5=new Type();
	g_vtable_ptrs.type=*(void**)p5;
}
template<typename T>
void dump_ptr(T* p){ dbprintf("%p{%p %p %p %p}\n",(void*)p,0[(void**)p],1[(void**)p],2[(void**)p],3[(void**)p]);}

void verify_expr_op(const Node* p){
	lazy_cache_vtable_ptrs();
	ASSERT(g_vtable_ptrs.expr_op==*(void**)p)
}
void verify_expr_block(const Node* p){
	lazy_cache_vtable_ptrs();
	ASSERT(g_vtable_ptrs.expr_block==*(void**)p||
		   g_vtable_ptrs.expr_struct_init==*(void**)p||
		   g_vtable_ptrs.expr_array_init==*(void**)p||
		   g_vtable_ptrs.expr_tuple==*(void**)p||
		   g_vtable_ptrs.expr_call==*(void**)p||
		   g_vtable_ptrs.expr_subscript==*(void**)p||
		   g_vtable_ptrs.expr_parens==*(void**)p||
		   g_vtable_ptrs.expr_compound==*(void**)p
		   )
}
void verify_expr_fn_def(const Node* p){
	lazy_cache_vtable_ptrs();
	ASSERT(g_vtable_ptrs.expr_fn_def==*(void**)p)
}
void verify_expr_ident(const Node* p){
	lazy_cache_vtable_ptrs();
	ASSERT(g_vtable_ptrs.expr_ident==*(void**)p)
}
void verify_type(const Node* p){
	lazy_cache_vtable_ptrs();
	ASSERT(g_vtable_ptrs.type==*(void**)p)
}
void verify_all_sub(){g_pRoot->verify();}
void dbprintf(const char* str, ... )
{
#ifdef DEBUG
	char tmp[1024];
	va_list arglist;
	
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	printf("%s",tmp);
#endif
}
void dbprintf(SrcPos& pos){
	dbprintf("%s:%d:%d:",g_filename,pos.line,pos.col);
}
void dbprintf(Name& n){
	dbprintf("%s",str(n));
}
void dbprintf(Node* n){n->dump(-1);}

ResolveResult resolve_make_fn_call(Expr* receiver,ExprBlock* block/*caller*/,Scope* scope,const Type* desired,int flags);

void print_tok(Name n){
	dbprintf("%s",getString(n));
};

bool g_lisp_mode=false;

void indent(int depth) {
	for (int i=0; i<depth; i++){dbprintf("\t");};
}
void newline(int depth) {
	if (depth>=0) dbprintf("\n;"); indent(depth);
}
// Even a block is an evaluatable expression.
// it may contain outer level statements.

Type* Node::expect_type() const {
	if (this->m_type)
		return m_type;
	error((const Node*)this,"%s has no type\n", str(name));
	return nullptr;
}
void Node::dump(PrinterRef depth) const {
	if (!this) return;
	newline(depth);dbprintf("(?)");
}
void Node::dump_top() const {
	if (!this) return;
	dbprintf("%s ", getString(name));
}

int get_typeparam_index(const MyVec<TParamDef*>& tps, Name name) {
	for (int i=(int)(tps.size()-1); i>=0; i--) {
		if (tps[i]->name==name)
			return i;
	}
	return -1;
}
/*
ResolveResult propogate_type(int flags,const Node* n, ResolveResult& a,Type*& b) {
	a.combine(propogate_type(flags,n, a.type,b));
	return a;
}
ResolveResult propogate_type(int flags,Expr* e, ResolveResult& a) {
	return propogate_type(flags,e, a, e->type_ref());
}

ResolveResult propogate_type(int flags,const Node* n,ResolveResult& a,Type*& b,const Type* c) {
	a.combine(propogate_type_fwd(flags,n, c,b));
	a.combine(propogate_type(flags,n, a.type,b));
	return a;
}
 */
ExprStructDef* dump_find_struct(Scope* s, Name name){
	for (;s;s=s->parent_or_global()){
		dbprintf("find %s in in scope %s\n",getString(name),s->name_str());
		for (auto ni=s->named_items;ni;ni=ni->next){
			dbprintf("name %s\n",getString(ni->name));
			for (auto sd=ni->structs; sd;sd=sd->next_of_name) {
				dbprintf("? %s\n",getString(sd->name));
				
			}
		}
	}
	return nullptr;
}

// compile time function in type system, available when you use
// tparams.
//
// type 'grammar' is the same, its just the operators do different things
//
// type Option[x]->Some[x]|None;	// union.
//
// type WinFrame=Window&Frame;		// get all the methods
//
// type Bar=Foo[a,b];  just like typedef.
// type Mul[a,b]->Vec3[Mul[a,b]]  // like applying a function
// type Mul[float,float]->float
// type Foo[a]->tuple[a.node,a.foo] // accessors for associated types?

//todo: generic heirarchy equality test, duplicate code detection?
bool type_compare(const Type* t,int a0, int a1){
	if (t)
		if (t->name==a0)
			if (t->sub)
				if (t->sub->name==a1)
					return true;
	return false;
}
//void find_printf(const char*,...){};
#define find_printf dbprintf
index_t num_known_arg_types(MyVec<Expr*>& args) {
	index_t n=0; for (index_t i=0; i<args.size(); i++) {if (args[i]->get_type()) n++;} return n;
}

//void match_generic_type_param_sub(const MyVec<TParamDef>& tps, MyVec<Type*>& mtps, const Type* to_match, const Type* given) {
	
//}
int match_tparams_from_arg_sub(MyVec<TParamVal*>& matched_tps, const MyVec<TParamDef*>& fn_tps,  const Type* given_arg, const Type* fn_arg );

int match_tparams_from_arg(MyVec<TParamVal*>& matched_tps, const MyVec<TParamDef*>& fn_tps,  const Type* given_arg, const Type* fn_arg )
{
	if (!fn_arg) return 0;
	if (!given_arg) return 0;

	// type root coercsion rules
	if (given_arg->name!=CONST && fn_arg->name==CONST)
		return match_tparams_from_arg(matched_tps,fn_tps, given_arg,fn_arg->sub);
	if (given_arg->name==MUT && fn_arg->name!=MUT)
		return match_tparams_from_arg(matched_tps,fn_tps, given_arg->sub,fn_arg);
	if (given_arg->name!=REF && fn_arg->name==REF)
		return match_tparams_from_arg(matched_tps,fn_tps, given_arg,fn_arg->sub);
	return match_tparams_from_arg_sub(matched_tps,fn_tps, given_arg, fn_arg);
}

int match_tparams_from_arg_sub(MyVec<TParamVal*>& matched_tps, const MyVec<TParamDef*>& fn_tps, const Type* given_arg, const Type* fn_arg )
{
	int ret_score=0;
//	if (!fn_arg && !given_arg) return 0;
	// if either is unspecified.. match anything there
	// TODO:
	if (!fn_arg) return 0;
	if (!given_arg) return 0;
	
	dbg_fnmatch("%s/s ",str(fn_arg->name),str(given_arg->name));
	if (fn_arg->sub || given_arg->sub) {
		dbg_fnmatch("[");
		for (const Type* sub1=fn_arg->sub,*sub2=given_arg->sub; sub1&&sub2; sub1=sub1->	next,sub2=sub2->next) {
			ret_score+=match_tparams_from_arg_sub(matched_tps,fn_tps, sub2,sub1);
		}
		dbg_fnmatch("]");
	}
	// if either is 'AUTO' - consider it ok.
	if (fn_arg->name==AUTO) return ret_score;
	if (given_arg->name==AUTO) return ret_score;

	int ti = get_typeparam_index(fn_tps, fn_arg->name);
	if (ti>=0) {
		// Is this a generic typeparam?
		if (matched_tps[ti]==0){ // new typeparam?
			matched_tps[ti]=(Type*)given_arg;
			return ret_score+1;
		}
		else if (!(matched_tps[ti]->is_equal(given_arg))) {// or we already found it - match..
//			dbg_fnmatch("match %s !=%s\n",str(fn_arg->name),str(given_arg->name));
#if DEBUG>=3
			matched_tps[ti]->dump(-1);
			given_arg->dump(-1);
#endif
			return ret_score-1000;
		}
	} else {
		// concrete types - compare absolutely
		if (fn_arg->name != given_arg->name) {
			dbg_fnmatch("\nmatch %s !=%s\n",str(fn_arg->name),str(given_arg->name));
			return ret_score-1000;	// mismatch is instant fail for this candidate.
		}
	}
	return ret_score;
}

void fill_given_tparams(MyVec<TParamVal*>& matched, const MyVec<TParamDef*>& arg_fmt, const MyVec<TParamVal*>& given_types)
{
	// todo - you can specify types manually when you call, or infer them from args, or both..
	
}
index_t get_arg_index(const MyVec<ArgDef*>& args, Expr* e,int flags=0){
	auto nm=e->as_op()->lhs;
	for (index_t i=0; i<args.size(); i<args.size()){
		if (args[i]->name==nm->name)
			return i;
	}
	if (flags&R_FINAL){
		error_begin(e,"no field %s found\ngot:",nm->name_str());
		for (auto x:args){ dbprintf("%s\t",x->name_str());}
		error_end(e);
	}
	return -1;
}

int match_tparams(MyVec<TParamVal*>& matched, const MyVec<ArgDef*>& arg_defs,const Type* ret_type, const MyVec<TParamDef*>& tparams, const MyVec<Expr*>& given_args,int first_arg_index, const Expr* callsite,bool variadic){
	matched.resize(tparams.size());
	int score=0;
#if DEBUG>=2
	callsite->dump(0);newline(0);
#endif
	for (int i=0; i<tparams.size();i++) matched[i]=0;
	int argi=first_arg_index;
	for (int i=0; argi<arg_defs.size() && i<given_args.size(); i++,argi++) {
		// named?
		auto expr=given_args[i];
		if (expr->name==FIELD_ASSIGN){
			argi=get_arg_index(arg_defs,expr);
			if (argi<0){
				score-=1000;
				continue;
			}
			expr=expr->as_op()->rhs;
		}
		auto argdef=arg_defs[argi];
#if DEBUG>=4
		if (arg_defs[i]){
			arg_defs[i]->type()->dump_if(-1); newline(0);
			given_args[i]->dump_if(-1); newline(0);
		}
#endif
		score+=match_tparams_from_arg(matched,tparams, expr->type(), argdef->type());
	}
	if (given_args.size()>arg_defs.size() && !variadic)
		score-=1000;
	score+=match_tparams_from_arg(matched, tparams, callsite->type(), ret_type);
	dbg_fnmatch("score matching gets %d\n",score);
	return score;
}
int match_fn_tparams(MyVec<TParamVal*>& matched, const ExprFnDef* f, const MyVec<Expr*>& args,const Expr* callsite){
	// TODO: allow the types to feedback in the math
	fill_given_tparams(matched, f->tparams, f->instanced_types);
	return match_tparams(matched, f->args, f->ret_type, f->tparams, args, 0, callsite,false);
}
int match_struct_tparams(MyVec<TParamVal*>& matched, const ExprStructDef* sd, const MyVec<Expr*>& field_exprs,const Expr* callsite){
	// TODO: allow the types to feedback in the math
	fill_given_tparams(matched, sd->tparams, sd->instanced_types);
	return match_tparams(matched, sd->fields, callsite->type(), sd->tparams, field_exprs, sd->first_user_field_index()
, callsite,false);
}


void FindFunction::dump()
{
	for (int i=0; i<candidates.size();i++){
		dbprintf("candidate %d for %s: %d %p score=%d\n",i, str(name),candidates[i].f->pos.line, candidates[i].f->instance_of, candidates[i].score);
	}
}

void FindFunction::insert_candidate(ExprFnDef* f,int score){
	verify_all();
	if (candidates.size()>=max_candidates){
		for (int i=0; i<candidates.size()-1;i++){
			candidates[i]=candidates[i+1];
		}
		candidates.resize(candidates.size()-1);
	}
	verify_all();
	for(int i=0; i<candidates.size();i++){
		if (candidates[i].f==f)return;// no duplicate?!
		if (candidates[i].score>score) {
			candidates.resize(candidates.size()+1);
			verify_all();
			for (size_t j=candidates.size()-1;j>i; j--){
				candidates[j]=candidates[j-1];
			}
			verify_all();
			candidates[i]=Candidate{f,score};
			verify_all();
			return;
		}
	}
	candidates.push_back(Candidate{f,score});
	verify_all();
}

void FindFunction::consider_candidate(ExprFnDef* f) {
	dbg_fnmatch("consider candidate %d %s\n",f->pos.line,str(f->name));
	verify_all();
	for (auto& c:this->candidates){
		if (c.f==f)
			return;
	}// ? shouldn't happen.
	if (f->type_parameter_index(f->name)<0)
		if (f->name!=name && name!=PLACEHOLDER)
			return ;
	if (!f->is_enough_args((int)args.size()) || f->too_many_args((int)args.size())){
		if (0==(this->flags&R_FINAL)) return;
	}
#if DEBUG >=2
	int num_rvals=0;
	for (int i=0; i<args.size();i++) {
		if (args[i]->type()->is_rvalue()){
			dbg2(printf("got rvalue argument %s\n",f->name_str())); num_rvals++;
		}
	}
	if (!num_rvals){
		dbg2(dbprintf("no rvalue args %s",f->name_str()));
	}
#endif
	// TODO: may need to continually update the function match, eg during resolving, as more types are found, more specific peices may appear?
	// Find max number of matching arguments
	
	verify_all();
	MyVec<Type*> matched_tparams;
	for (int i=0; i<f->tparams.size(); i++){matched_tparams.push_back(nullptr);}

	int score=0;
	// no args needed or given.. score is 1..
	if (this->args.size()==0 && f->is_enough_args(0))
		score++;
	// +1 for any matching arg type regardless of placement,bonus if aprox right order
#if DEBUG>=2
	dbprintf("try to match args:-\n");
	for (auto x:this->args){
		x->dump(-1);dbprintf("\n");
	}
	dbprintf("...with:-\n");
	for (auto x:f->args){
		x->dump(-1);dbprintf("\n");
	}
#endif
	for (int i=0; i<args.size(); i++) {
		auto at=args[i]->get_type();
		if (!at) continue;
		if (i<f->args.size()){
			if (auto fa=f->args[i]) {
				if (auto ft=fa->get_type()){
					if (ft->is_rvalue_ref()){
						if (at->is_rvalue()){
							score+=1;
						}
					}
				}
			}
		}
		for (int jj=i; jj<f->args.size(); jj++) {
			auto j=jj%args.size();
			if (auto s=f->args[j]->get_type()->is_equal_or_coercible(at)){
				if (j==i) score+=(1+args.size()-i); // args in right pos score higher
				score+=s;
				break;
			}
		}
	}
	if (name==PLACEHOLDER){
//		score-=1000;
	}
	else if (!f->is_enough_args((int)args.size()) || f->too_many_args((int)args.size())){
		score-=10000;
		insert_candidate(f,score);
		return;
	}

	if (f->variadic && args.size()> f->args.size())
		score=1;	// variadic functoin can match anything?
	if (!f->tparams.size())
	for (int i=0; i<args.size() && i<f->args.size(); i++) {
		if (!f->args[i]->get_type() || (!args[i])) {
			score++; //1 point for an 'any' arg on either side
		} else{
			// both args are given:
			auto fn_arg_t=f->args[i]->get_type();
			auto given_arg_t=args[i]->get_type();
			if (auto s=given_arg_t->is_equal_or_coercible(fn_arg_t)) {
				score+=s+10;// 1 exact match worth more than any number of anys
			} else{
				//if (!is_generic_type(f->tparams,f->args[i]->get_type())
				
				{
				// instant fail for incorrect concrete arg
				//TODO consider conversion operators here.
					score-=10000;
				//	if (candidates.size()>=4)
				//		return;
				}
			}
		}
	}
	// find generic tparams..
	if (f->tparams.size()){
#if DEBUG>=2
		dbg_fnmatch("%s score=%d; before typaram match\n",str(f->name),score);
		dbg_fnmatch("callsite: %d args\n",args.size());
		for (int i=0; i<args.size();i++) {
			dbg_fnmatch("arg %s:",  str(args[i]->name));
			args[i]->type()->dump_if(-1);
			dbg_fnmatch("\tvs\t");
			f->args[i]->type()->dump_if(-1);
			dbg_fnmatch("\n");
		}
		dbg_fnmatch("\n");
#endif
		for (int i=0; i<args.size() && i<f->args.size(); i++) {
			score+=match_tparams_from_arg(matched_tparams,f->tparams, args[i]->get_type(), f->args[i]->get_type() );
		}
		score+=match_tparams_from_arg(matched_tparams, f->tparams,ret_type,f->ret_type);
		dbg_fnmatch("typaram matcher for %s\n",f->name_str());
		dbg_fnmatch("%s:%d: %s\n",g_filename,f->pos.line,str(f->name));
		dbg_fnmatch("%s score=%d; matched tparams{:-\n",str(f->name),score);
		for (auto i=0; i<f->tparams.size(); i++){
			dbg_fnmatch("[%d]%s = %s;\n", i,str(f->tparams[i]->name),matched_tparams[i]?str(matched_tparams[i]->name):"not found" );
			}
		dbg_fnmatch("}\n");
		dbg_fnmatch("\n");
		
	}
	verify_all();
	// fill any unmatched with defaults?

	// consider return type in call.
	if (ret_type)
		if (f->get_type()->is_equal(ret_type)) score+=100;

	if (f->name==name) score*=100; // 'named' functions always win over un-named forms eg F[F,X](a:X),we may use unnamed to implement OOP..
	// insert candidate
	verify_all();
	insert_candidate(f,score);
	verify_all();
}
void FindFunction::find_fn_sub(Expr* src) {
	verify_all();
	dbg_raii(src->dump(0));dbg_raii(newline(0));
	if (auto sb=dynamic_cast<ExprBlock*>(src)) {
		for (auto x:sb->argls) {
			find_fn_sub(x);
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(src)){
		if (verbose)
			dbg_fnmatch("consider %s %d for %s %d\n",f->name_str(),f->pos.line, this->callsite->name_str(),this->callsite->pos.line);
		consider_candidate(f);
		int i=0;
		for (auto ins=f->instances; ins; ins=ins->next_instance,i++) {
			dbg_fnmatch("%s ins=%d\n",f->name_str(),i);
			consider_candidate(ins);
		}
	}
	verify_all();
}
void FindFunction::find_fn_from_scopes(Scope* s,Scope* ex)
{
	if (name!=PLACEHOLDER){
		if (auto fname=s->find_named_items_local(name)){
			for (auto f=fname->fn_defs; f;f=f->next_of_name) {
				find_fn_sub((Expr*)f);
			}
		}
	} else{
		for (auto ni=s->named_items; ni; ni=ni->next){
			for (auto f=ni->fn_defs; f;f=f->next_of_name) {
				find_fn_sub((Expr*)f);
			}
		}
	}
	for (auto f=s->templated_name_fns; f;f=f->next_of_name) {
		find_fn_sub((Expr*)f);
	}
	for (auto sub=s->child; sub; sub=sub->next) {
		if (sub==ex) continue;
		find_fn_from_scopes(sub,ex);
	}
}

void dbprint_find(const MyVec<ArgDef*>& args){
	dbprintf("\n;find call with args(");
	for (int i=0; i<args.size(); i++) {dbprintf(" %d:",i);dbprintf("%p\n",args[i]);if (args[i]->get_type()) args[i]->get_type()->dump(-1);}
	dbprintf(")\n");
}

template<typename T>
void dump(MyVec<T*>& src) {
	for (int i=0; i<src.size(); i++) {
		dbprintf(src[i]->dump());
	}
}


ResolveResult resolve_make_fn_call(Expr* receiver,ExprBlock* block/*caller*/,Scope* scope,const Type* desired,int flags) {
	verify_all();

	int num_resolved_args=0;
	for (int i=0; i<block->argls.size(); i++) {
		block->resolved|=block->argls[i]->resolve_if(scope,nullptr,flags);
		if (block->argls[i]->type())
			num_resolved_args++;
	}
#if DEBUG>4
	for (int i=0; i<block->argls.size(); i++) {
		dbprintf("arg[i]=",i);
		block->argls[i]->dump_if(-1);newline(0);
	}
#endif
	if (receiver){
		block->resolved|=receiver->resolve_if(scope,nullptr,flags); if (receiver->type()) num_resolved_args++;
	}
	
	if (block->get_fn_call() && num_resolved_args==block->argls.size())
		return ResolveResult(COMPLETE);

//	11ASSERT(block->call_target==0);
	// is it just an array access.
	if (block->as_subscript()){
		block->resolved|=block->call_expr->resolve_if(scope,nullptr,flags);
		block->resolved|=block->argls[0]->resolve_if(scope,nullptr,flags);
		auto obj_t=block->call_expr->type();
		//find the method "index(t,index_t)"
		// for the minute, just assume its' an array
		// TODO: operator overload.

		if (obj_t && block->argls[0]->type()) {
			ASSERT(obj_t->is_array() || obj_t->is_pointer());
			return block->resolved;
		}
		return block->resolved;
	}
	verify_all();

	MyVec<Expr*> args_with_receiver;
	if (receiver) args_with_receiver.push_back(receiver);
	//args_with_receiver.insert(args_with_receiver.end(),block->argls.begin(),block->argls.end());
	args_with_receiver.append(block->argls);

	ExprFnDef* call_target = scope->find_fn(block->call_expr->as_name(),block,receiver?1:0,args_with_receiver, desired,flags|R_CALL);
	auto fnc=call_target;
	if (!call_target){
		return block->resolved|INCOMPLETE;
	}
	verify_all();
	if (call_target!=block->get_fn_call()) {
		block->call_expr->set_def(call_target);
		if (block->get_fn_call()) {
			error(block,"call target changed during resolving, we're not sure how to handle this yet\n");
			block->scope=0; // todo delete.
		} else {
			block->scope=0; // todo delete.
		}
		block->set_def(call_target);
		ASSERT(block->def==block->call_expr->def);
		if (call_target->resolved) {
			Type * fnr=call_target->return_type();
			return block->propogate_type_fwd(flags,block, desired, block->type_ref(),fnr);
		}
			// add this to the targets' list of calls.
		int num_known_types=(desired?1:0)+num_known_arg_types(block->argls);
		bool isg=fnc->is_generic();
		if (!(isg && num_known_types)) {
			// concrete function: we can just take return type.
			auto rt=fnc->return_type();
			return block->propogate_type_fwd(flags,block, desired, rt,block->type_ref());
		}
		{
			int once=false; if(!once++){
			dbprintf("TODO decide if we should allow genrics to instantiate earlry\n");
			dbprintf("should we propogate types or");
			dbprintf("wait till the environment propogates them");
			dbprintf("to select");
			dbprintf("restrict to forward inference for this case?");
				
			}
		}
		return block->resolved;
		// generic function, and we have some types to throw in...
		// if its' a generic function, we have to instantiate it here.
/*
		if (!block->scope) {
//			ASSERT(0 && "this is bs");
//			auto sc=new Scope;
//			block->scope=sc;
//			scope->push_child(sc);
//			sc->owner=call_target;//TODO: this is dodgy.
			// do we need to distinguish an inline instance from a global instance.
			block->next_of_call_target = call_target->callers;
			call_target->callers =block;
		}
		// create a local for each supplied argument, using its type..
		// note that vars should be able to propogate inside too???
		// TODO-inlining generic call? or what?
		Scope* fsc=block->scope;
		for (int i=0; i<block->argls.size() && i<fnc->args.size(); i++) {
			auto input_type=block->argls[i]->get_type();
			auto v=fsc->create_variable(fnc->args[i], fnc->args[i]->name,VkArg);
			auto argtype=fnc->args[i]->get_type();
			if (!v->type()){
				v->type(argtype?argtype:input_type);
			} else {
				// read the type out from the function invocation, really.
				if (block->argls[i]->get_type()) {ASSERT(input_type->eq(v->type()));}
				else block->argls[i]->type(v->type());
			}
			// and stuff a default expression in for any not called..
		}
 
		auto ret=call_target->resolve_call(fsc,desired,flags);
		verify_all();
		return propogate_type(flags,block, ret);
 */
	}
	else  {
		if (flags &R_FINAL)
			error(block,"can't resolve call\n");
		verify_all();
		return block->resolved|=INCOMPLETE;
	}
	verify_all();
}

void call_graph(Node* root,Scope* scope) {
}

void unexpected(int t){error(0,"unexpected %s\n",getString(t));}

void
gather_vtable(ExprStructDef* d) {
}
int TParamXlat::typeparam_index(const Name& n) const{
	for (int i=0; i<this->tparams.size(); i++){
		if (this->tparams[i]->name==n) return i;
	}
	return -1;
}
void TParamXlat::dump(PrinterRef depth)const{
	dbprintf("[");
	for (auto i=0; i<this->tparams.size();i++){
		if (i)dbprintf(",");
		dbprintf("%s=",str(this->tparams[i]->name));
		this->given_types[i]->dump_if(-1);
	}
	dbprintf("]");
}
bool type_params_eq(const MyVec<Type*>& a, const MyVec<Type*>& b) {
	if (a.size()!=b.size()) return false;
	for (int i=0; i<a.size(); i++) { if (!a[i]->is_equal(b[i])) return false;}
	return true;
}
bool type_params_eq(const MyVec<Type*>& a, const Type* tp){
	for (auto
		 i=0; i<a.size() && tp;i++,tp=tp->next){
		if (!a[i]->is_equal(tp))
			return false;
	}
	// TODO- defaults.
	return true;
}
bool	ExprIdent::is_function_name()const	{
	return dynamic_cast<ExprFnDef*>(this->def)!=0;
}
bool		ExprIdent::is_variable_name()const	{
	return dynamic_cast<Variable*>(this->def)!=0;
}




#include "hack.hpp"
#include "codegen.h"
#include "repl.h"

//(sourcecode "hack.cpp")
//(normalize (lerp (obj:zaxis)(normalize(sub(target(obj:pos)))) //(settings:angle_rate)) (sloc 512 40 20))

void print_tok(int i){printf("%s ",getString(i));};

bool g_lisp_mode=true;
const char* g_token_str[]={
	"",
	"int","float","str","void","auto","one","zero","voidptr","ptr","ref","tuple",
	"print","fn","struct","enum","array","union","variant","with","match",
	"let","set","var",
	"while","if","else","do","for","in","return","break",
	"(",")",
	"{","}",
	"[","]",
	"->","=>","<-","::","<->",			//arrows
	":","+","-","*","/",".",					//arithmetic
	"<",">","<=",">=","==","!=","&&","||",		//compares/logical
	"&","|","^","%","<<",">>",					//bitwise
	"=",":=","+=","-=","*=","/=","<<=",">>=","&=","|=",
	"++","--","++","--","-","*","&","!","~",
	"*?","*!","&?","[]","&[]",
	",",";",
	"_",
	NULL,	
};

#define PRECEDENCE 0xff
#define PREFIX 0x100
#define UNARY 0x200
#define ASSOC 0x400
int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,
	0,0,0,
	0,0,0,0,0,0,0,0,
	0,0,
	0,0,
	0,0,
	10,10,10,13,10,	   // arrows
	9,6,6,5,5,2,		//arithmetic
	8,8,8,8,9,9,13,14,	//compares/logical
	8,7,8,6,9,9,		//bitwise
	ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,ASSOC|16,
	PREFIX|UNARY|2,PREFIX|UNARY|2,UNARY|ASSOC|3,UNARY|ASSOC|3,UNARY|PREFIX|3,UNARY|PREFIX|3,UNARY|PREFIX|3,UNARY|PREFIX|3,UNARY|PREFIX|3, 
	UNARY|ASSOC|3, UNARY|ASSOC|3, UNARY|ASSOC|3, UNARY|ASSOC|3, UNARY|ASSOC|3,
	0,0,
	0,0,
};
bool is_ident(int tok){return tok>=IDENT;}
bool is_type(int tok){return tok<T_NUM_TYPES;}
int is_operator(int tok){ return tok>=ARROW && tok<COMMA;}
int precedence(int tok){return tok<IDENT?(g_tok_info[tok] & PRECEDENCE):0;}
int is_prefix(int tok){return tok<IDENT?(g_tok_info[tok] & (PREFIX) ):0;}
int arity(int tok){return  (tok<IDENT)?((g_tok_info[tok] & (UNARY) )?1:2):-1;}
int is_right_assoc(int tok){return (tok<IDENT)?(g_tok_info[tok]&ASSOC):0;}
int is_left_assoc(int tok){return (tok<IDENT)?(!(g_tok_info[tok]&ASSOC)):0;}
int get_prefix_operator(int tok) {
	if (tok>IDENT) return tok;
	switch (tok){
	case POST_INC: return PRE_INC;
	case POST_DEC: return PRE_DEC;
	case SUB: return NEG;
	case MUL: return DEREF;
	case AND: return ADDR;
	default: return tok;
	}
}
int get_infix_operator(int tok) {
	if (tok>IDENT) return tok;
	switch (tok){
	case PRE_INC: return POST_INC;
	case PRE_DEC: return POST_DEC;
	case NEG: return SUB;
	case DEREF: return MUL;
	case ADDR: return AND;
	default: return tok;
	}
}



StringTable::StringTable(const char** initial){
	verbose=false;
	nextId=0;
//	for (int i=0; g_token_str[i];i++) {
//		auto tmp=g_token_str[i];
//		get_index(tmp,tmp+strlen(tmp));
//	}
	nextId=IDENT;
	index_to_name.resize(IDENT);
	for (int i=0; i<index_to_name.size(); i++) {
		index_to_name[i]=std::string(g_token_str[i]);
		names.insert(std::make_pair(index_to_name[i],i));
	}
	printf("%d %s\n",COMMA,g_token_str[COMMA]);
	printf("%d %s\n",SEMICOLON,g_token_str[SEMICOLON]);
	printf("%d",nextId);
	ASSERT(nextId==IDENT);
}
int StringTable::get_index(const char* str, const char* end) {
	auto len=end-str;
	string s; s.resize(len);memcpy((char*)s.c_str(),str,len);((char*)s.c_str())[len]=0;
	auto ret=names.find(s);
	if (ret!=names.end())	return ret->second;		
	names.insert(std::make_pair(s,nextId));
	index_to_name.resize(nextId+1);
	index_to_name[nextId]=s;
//		std::cout<<s <<index_to_name[nextId];
	if (verbose)
		printf("insert[%d]%s\n",nextId,index_to_name[nextId].c_str());
	return	nextId++;
};

void StringTable::dump(){
	printf("\n");
	for (int i=0; i<this->nextId; i++) {
		printf("[%d]%s\n",i,this->index_to_name[i].c_str());
	}
};


StringTable g_Names(g_token_str);
int getStringIndex(const char* str,const char* end) {
	if (!end) end=str+strlen(str);
	return g_Names.get_index(str, end);
}
const char* getString(int index) {
	return g_Names.index_to_name[index].c_str();
}


void indent(int depth) {
	for (int i=0; i<depth; i++){printf("\t");};
}
void newline(int depth) {
	if (depth>=0) printf("\n"); indent(depth);
}
// Even a block is an evaluatable expression.
// it may contain outer level statements.

void Expr::dump(int depth) const {
	if (!this) return;
	newline(depth);printf("(?)");
}
void Expr::dump_top() const {
	if (!this) return;
	printf("%s ", getString(name));
}

Expr::Expr(){ type=0;}
Status assert_types_eq(const Type* a,const Type* b) {
	if (!a->eq(b)){
		printf("type error"); a->dump(-1);b->dump(-1);printf("\n");
		return ERROR;
	}
	return COMPLETE;
}
Status propogate_type(Type*& a,Type*& b) {
	if (!(a || b)) return INCOMPLETE;
	if (!a && b) {a=b; return INCOMPLETE;}
	else if (!b && a) {b=a; return INCOMPLETE;}
	else {ASSERT(a && b);
		return assert_types_eq(a,b);
	}
}
Status propogate_type_fwd(const Type*& a,Type*& b) {
	if (!(a || b)) return INCOMPLETE;
	if (!a && b){return INCOMPLETE;}
	if (!b && a) {b=(Type*)a;return INCOMPLETE;}
	else {ASSERT(a && b); return assert_types_eq(a,b);  }
}
Status propogate_type(Type*& a,Type*& b,Type*& c) {
	int ret=COMPLETE;
	ret|=propogate_type(a,b);
	ret|=propogate_type(b,c);
	ret|=propogate_type(a,c);
	return (Status)ret;
}
Status propogate_type_fwd(const Type*& a,Type*& b,Type*& c) {
	int ret=COMPLETE;
	ret|=propogate_type(b,c);
	ret|=propogate_type_fwd(a,b);
	ret|=propogate_type_fwd(a,c);
	return (Status)ret;
}
Status propogate_type(ResolvedType& a,Type*& b) {
	a.status=(Status)(propogate_type(a.type,b)|(int)a.status);
	return a.status;
}
Status propogate_type(ResolvedType& a,ResolvedType& b) {
	// propogate all errors between them?
	b.status=a.status=(Status)(propogate_type(a.type,b.type)|(int)a.status|(int)b.status);
	return a.status;
}


ResolvedType ExprIdent::resolve(CallScope* scope,const Type* desired) {
	// todo: not if its' a typename,argname?
	if (this->is_placeholder()) {
		//PLACEHOLDER type can be anything asked by environment, but can't be compiled out.
		this->type=(Type*)desired; return ResolvedType(this->type,COMPLETE);
	}
	propogate_type_fwd(desired,this->type);
	if (auto v=scope->get_variable(this->name)){
		propogate_type(this->type,v->type);
	}
	return ResolvedType(this->type);
}
void ExprIdent::dump(int depth) const {
	if (!this) return;
	newline(depth);printf("%s:",getString(name));
	if (this->type) {this->type->dump(-1);}
}

int ExprBlock::get_name()const{
	if (call_op) return call_op->get_name(); else return 0;
}
void ExprBlock::dump(int depth) const {
	if (!this) return;
	newline(depth);
	if (this->call_op){int id=this->call_op->ident();if (is_operator(id)) {if (is_prefix(id))printf("prefix");else if (this->argls.size()>1)printf("infix");else printf("postfix");};print_tok(id);
		if (this->type) {printf(":");this->type->dump(-1);};printf("(");} else printf("{");
	for (const auto x:this->argls) {
		if (x) {x->dump(depth+1);}else{printf("(none)");}
	}
	newline(depth);if (this->call_op)printf(")");else printf("}");
//	newline(depth);
}

ExprBlock::ExprBlock(){call_target=0;}

const char* CallScope::name() const {
	if (!parent){
		return"global";
	} 
	else return getString(outer->name);
}

// compile time function in type system, available when you use
// typeparams.
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

void Type::push_back(Type* t) {	
	if (!sub) sub=t;
	else {
		auto s=sub;
		for (; s->next!=0; s=s->next){};
		s->next =t;
	}
}
const char* Node::get_name_str() const{
	if (!this) return "(no_type)";
	return getString(this->name);
}
const char* Type::kind_str()const{return"type";}
Type::Type(Name i){ 
	struct_def=0;
	sub=0;
	next=0;
	name=i; //todo: resolve-type should happen here.
}
	//todo: generic heirarchy equality test, duplicate code detection?
bool Type::eq(const Type* other) const{
	if ((!this) && (!other)) return true;
	if (!(this && other)) return false;
	if (this->name!=other->name)return false;
//	if (!this->sub && other->sub)) return true;
	auto p=this->sub,o=other->sub;
		
	for (; p && o; p=p->next,o=o->next) {
		if (!p->eq(o)) return false;
	}
	if (o || p) return false; // didnt reach both..
	return true;
}
void Type::dump_sub()const{
	if (!this) return;
	if (this->name==TUPLE) {
		printf("(");
		for (auto t=sub; t; t=t->next){
			t->dump_sub();
			if(t->next)printf(",");
		};
		printf(")");
	} else{
		printf("%s",getString(name));
		if (sub){
			printf("[");
			for (auto t=sub; t; t=t->next){
				t->dump_sub();
				if(t->next)printf(",");
			};
			printf("]");
		}
	}
}
void Type::dump(int depth)const{
	if (!this) return;
	newline(depth);dump_sub();
}


void ExprLiteral::dump(int depth) const{
	if (!this) return;
	newline(depth);
	if (type_id==T_VOID){printf("void");}
	if (type_id==T_INT){printf("%d",u.val_int);}
	if (type_id==T_FLOAT){printf("%.7f",u.val_float);}
	if (type_id==T_CONST_STRING){
		printf("\"%s\"",u.val_str);
	}
}
// TODO : 'type==type' in our type-engine
//	then we can just make function expressions for types.

ResolvedType ExprLiteral::resolve(CallScope* , const Type* desired){
	if (this->type) {
		if (desired && this->type) {
			desired->dump(-1); this->type->dump(-1);
			ASSERT(this->type->eq(desired));
		}
		return ResolvedType(this->type);
	}
	Type* t=nullptr;
	switch (type_id) {
	case T_VOID: t=new Type(VOID); break;
	case T_INT: t=new Type(INT); break;
	case T_FLOAT: t=new Type(FLOAT); break;
	case T_CONST_STRING: t=new Type(STR); break;
	default: break;
	}
	propogate_type(t,this->type);
	propogate_type_fwd(desired,this->type);
	return ResolvedType(this->type,COMPLETE);
}

ExprLiteral::ExprLiteral(float f) {
	type=nullptr;
	type_id=T_FLOAT;
	u.val_float=f;
	printf("lit float %.3f",f);
}
ExprLiteral::ExprLiteral(int i) {
	type=nullptr;
	type_id=T_INT;
	u.val_int=i;
}
ExprLiteral::ExprLiteral(const char* start,int length) {//copy
	type=nullptr;
	type_id=T_CONST_STRING;
	auto str=( char*)malloc(length+1); ;
	u.val_str=str;memcpy(str,(void*)start,length);
	str[length]=0;
}
ExprLiteral::ExprLiteral(const char* src) {//take ownership
	u.val_str=src;
	type=nullptr;
	type_id=T_CONST_STRING;
}
ExprLiteral::~ExprLiteral(){
	if (type_id==T_CONST_STRING) {
		free((void*)u.val_str);
	}
}
void dump_typeparams(const vector<TypeParam>& ts) {
	bool a=false;
	if (ts.size()==0) return;
	printf("[");
	for (auto t:ts){
		if (a)printf(",");
		print_tok(t.name);if (t.defaultv){printf("=");print_tok(t.defaultv);}
		a=true;
	}
	printf("]");
}

void ArgDef::dump(int depth) const {
	newline(depth);printf("%s",getString(name));
	if (type) {printf(":");type->dump(-1);}
	if (default_expr) {printf("=");default_expr->dump(-1);}
}
const char* ArgDef::kind_str()const{return"arg_def";}

// the operators should all just be functioncalls, really.
// return type of function definition is of course a functoin object.
// if we make these things inline, we create Lambdas
// todo: receivers.

bool ExprFnDef::is_generic() const {
	for (auto i=0; i<args.size(); i++)
		if (!args[i]->type) return true;
	return false;
}

void ExprFnDef::dump(int ind) const {
	if (!this) return;
	newline(ind);printf("fn %s",getString(name));dump_typeparams(this->typeparams);printf("(");
	for (int i=0; i<args.size();i++){
		args[i]->dump(-1);
		if (i<args.size()-1) printf(",");
	}
	printf(")");
	if (this->type) {printf("->");this->type->dump(-1);};
	this->body->dump(ind);
	if (auto p=this->instances){
		printf("instantiations:");
		for (;p;p=p->next_instance){
			p->dump(ind);
		}
	}
}

FnName*	g_fn_names;
vector<FnName> g_functions;
FnName* getFnName(int name) {
	FnName*	n;
	for (n=g_fn_names; n; n=n->next) {
		if (n->name==name) return n;
	}
	n=new FnName(); n->name=name; n->next=g_fn_names; g_fn_names=n;
	return n;
}

// get a function's list of shared names..
FnName* ExprFnDef::get_fn_name() {
	if (this->fn_name)	return	this->fn_name;
	auto fnm=getFnName(this->name);
	this->fn_name = fnm;
	this->next_of_name=fnm->fn_defs; fnm->fn_defs = this;
	return	fnm;
}
/*
void CallScope::visit_calls() {
	for (auto call=this->calls;call;call=call->next_of_scope) {
		printf("%s --> %s\n",this->name(), getString(call->callee->name));
	}
	for (auto sub=this->child; sub; sub=sub->next)
		sub->visit_calls();
}
*/
ExprFnDef* instantiate_generic_function(CallScope* s,ExprFnDef* src, vector<Expr*>& call_args) {
	ExprFnDef* f =(ExprFnDef*) src->clone();
	if (src->fn_name && !f->fn_name) {
		f->fn_name = src->fn_name;
		f->next_of_name = src->fn_name->fn_defs;
		src->fn_name->fn_defs = f;
	}
	// fill any args we can
	for (auto i=0; i<f->args.size(); i++){
		if (!f->args[i]->type && call_args[i]->type) {f->args[i]->type=(Type*)call_args[i]->type->clone();}
	}
	f->next_instance = src->instances;
	src->instances=f;
	f->resolved=false;
	printf("generic instantiation:-\n");
	f->dump(0);
	return f;	// welcome new function!
}
Node* ExprBlock::clone() const {
	if (!this) return nullptr;
	auto r=new ExprBlock();
	if (this->call_op) {
		r->call_op = (ExprBlock*) this->call_op->clone();
	}
	r->name=this->name;
	r->argls.resize(this->argls.size());
	for (int i=0; i<this->argls.size(); i++) {
		r->argls[i]=(Expr*)(this->argls[i]->clone());
	}
	return r;
}
Node*
ExprLiteral::clone() const{
	if (!this) return nullptr;
	auto r=new ExprLiteral(0); if (this->is_string()){r->u.val_str=strdup(this->u.val_str);}else r->u=this->u;
	r->type_id=this->type_id;
	r->name=this->name;
	return r;
}
Node*
ExprFnDef::clone() const{
	if (!this) return nullptr;
	auto r=new ExprFnDef;
	r->name=this->name;
	r->body=(ExprBlock*)this->body->clone();
	r->args.resize(this->args.size());
	for (int i=0; i<this->args.size(); i++) {
		r->args[i]=(ArgDef*)this->args[i]->clone();
	}
	return r;
}
Node*
ArgDef::clone() const{
	if (!this) return nullptr;
	auto r=new ArgDef; {
		r->name=this->name;
		if (this->type)
			r->type=(Type*)this->type->clone();
		if (this->default_expr)
			r->default_expr=(Expr*)this->default_expr->clone();
	}
	return r;
}
Node*
Type::clone() const{
	if (!this) return nullptr;
	auto r= new Type(this->name);
	auto *src=this->sub;
	Type** p=&r->sub;
	while (src) {
		*p= (Type*)src->clone();
		p=&((*p)->next);
		src=src->next;
	}
	return r;
}
Node*
ExprIdent::clone() const {
	auto r=new ExprIdent(this->name);
	return r;
}

void find_printf(const char*,...){};
int num_known_arg_types(vector<Expr*>& args) {
	int n=0; for (auto i=0; i<args.size(); i++) {if (args[i]->type) n++;} return n;
}

void compare_candidate_function(ExprFnDef* f,Name name,vector<Expr*>& args,const Type* ret_type, ExprFnDef** best_fn, int* best_score,int* ambiguity) {
	if (f->name!=name)
		return ;
	// TODO: may need to continually update the function match, eg during resolving, as more types are found, more specific peices may appear?
	// Find max number of matching arguments
	if (args.size() >f->args.size())	// if not enough args, dont even try.
		return;
	if (args.size() !=f->args.size())	// TODO: bring default arguments into the picture.
		return;

	find_printf("candidate:");
	for (int i=0; i<args.size(); i++) {
		find_printf("%s ",f->args[i]->type->get_name_str());		 }
	find_printf("\n");
	int score=0;
	for (int i=0; i<args.size(); i++) {
		if (f->args[i]->type->eq(args[i]->type)) {
			find_printf("match %s %s\n",f->args[i]->type->get_name_str(), args[i]->type->get_name_str());
			score++;
		}
	}

	// consider return type in call.
	if (ret_type) if (f->type->eq(ret_type)) score++;
		
	// for any argument not matched, zero the score if its the *wrong* argument?
	// TODO: this is where we'd bring conversion operators into play.
	for (int i=0; i<args.size(); i++) {
		if ((!f->args[i]->type->eq(args[i]->type)) && f->args[i]->type!=0) score=-1;
	}
	find_printf("score is %d\n",score);
	if (score >*best_score) {
		*best_score=score;
		*best_fn=f;
		ambiguity=0;
	} else if (score==*best_score) ambiguity++;
}
void find_sub(Expr* src,Name name,vector<Expr*>& args, const Type* ret_type,ExprFnDef** best_fn, int* best_score,int* ambiguity) {
	if (auto sb=dynamic_cast<ExprBlock*>(src)) {
		find_printf("look for %s in %s\n",getString(name),src->get_name_str());
		for (auto x:sb->argls) {
			find_sub(x,name,args,ret_type,best_fn,best_score,ambiguity);
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(src)){
		compare_candidate_function(f,name,args,ret_type, best_fn,best_score,ambiguity);
	}
}

FnName* CallScope::find_fn_name(Name name){
	// search the name buckets.
	for (auto fname=this->fn_names;fname;fname=fname->next){
		if (fname->name==name) {
			return fname;
		}
	}
	return nullptr;
}

ExprFnDef*	CallScope::find_fn(Name name, vector<Expr*>& args,const Type* ret_type)  {
	find_printf("\nfind call with args(");
	for (int i=0; i<args.size(); i++) {find_printf(" %d:",i);printf("%p\n",args[i]);if (args[i]->type) args[i]->type->dump(-1);}
	find_printf(")\n");

	ExprFnDef*	best=0;
	int best_score=-1;
	int	ambiguity=0;
	for (auto src=this; src; src=src->parent) {
		auto fname=src->find_fn_name(name);
		if (!fname) {
			printf("\ncant find any function  %s\n",getString(name));
			exit(0);
			return 0;
		}
		for (auto f=fname->fn_defs; f;f=f->next_of_name) {
			find_sub(f,name, args,ret_type,&best,&best_score,&ambiguity);
		}
	}
	find_printf("match score=%d/%d\n", best_score, args.size());
	if (!best)  {
		printf("No match found\n");
		return nullptr;
	}
	if (ambiguity){
		printf("ambiguous matches for function\n");
	}
	if (best->is_generic()) {
		printf("matched generic function: instanting\n");
		return instantiate_generic_function(this, best, args);
	}
	
	return best;
}
Variable* CallScope::find_variable(Name name){
	// todo: This Pointer?
	for (auto v=this->vars; v; v=v->next) {
		if (v->name==name) return v;
	}
	if (this->parent){
		if (auto p=this->parent->get_variable(name)) 
			return	p;
	}
	if (this->global && this->global!=this){
		if (auto p=this->global->find_variable(name))
			return	p;
	}
	return nullptr;
}


Variable* CallScope::get_variable(Name name){

	if (auto v=this->find_variable(name)) {
		return v;
	}
	auto v=new Variable(); v->next=this->vars; this->vars=v; v->name=name;
	return v;
}
void CallScope::dump(int depth)const {
	newline(depth);printf("scope: %s {", this->outer?getString(this->outer->ident()):"global");
	for (auto v=this->vars; v; v=v->next) {
		newline(depth+1); printf("var %d %s:",v->name, getString(v->name)); 
		if (v->type){ v->type->dump(-1);} else {printf("not_type");}
	}
	for (auto s=this->child; s; s=s->next){
		s->dump(depth+1);
	}
	newline(depth);printf("}");
}
template<typename T>
void dump(vector<T*>& src) {
	for (int i=0; i<src.size(); i++) {
		printf(src[i]->dump());
	}
}
ResolvedType ExprBlock::resolve(CallScope* sc, const Type* desired) {

	printf("\nresolve block.. %p\n", sc);
	if (this->argls.size()<=0 && !this->call_op) {
		if (!this->type) this->type=new Type(VOID);
		return ResolvedType(this->type);
	}
	int op_ident=NONE;
	ExprIdent* p=nullptr;
	if(this->call_op){p=dynamic_cast<ExprIdent*>(this->call_op); op_ident=p->name;}
	printf("%s %s\n",getString(this->name),getString(op_ident));
	if (op_ident==NONE) {	// do executes each expr, returns last ..
		Type* ret=0;
		for (auto i=0; i<this->argls.size()-1; i++) {
			this->argls[i]->resolve(sc,0);
		}
		// last expression - type bounce
		if (auto i=this->argls.size()) {
			ret=this->argls[i-1]->resolve(sc,desired);
			propogate_type_fwd(desired,ret);
		}
		propogate_type(ret,this->type);
		return ResolvedType(ret);
	} 
	else 
	if (op_ident==ASSIGN) {
		ASSERT(this->argls.size()==2);
		auto vname=this->argls[0]->ident();	//todo: rvalue malarchy.
//			printf("set: try to create %d %s %s\n", vname, getString(vname), this->args[1]->kind_str());
		this->argls[0]->dump(0);
		printf("resolve block getvar %p %s\n", sc, getString(vname));
		auto v= sc->get_variable(vname);
		Type* t=this->argls[1]->resolve(sc,desired?desired:v->type);
		propogate_type_fwd(desired,t,v->type);
		
			// If the variable exists - assignments must match it...
			// todo: 2way inference.
			// we'd report missing types - then do a pass propogating the opposite direction
			// and bounce.
		this->type = v->type;//t?t:(Type*)desired;
		return ResolvedType(v->type);
	} else if (is_operator(op_ident)){
		// todo: &t gets adress. *t removes pointer, [i] takes index,
		// comparisons yield bool, ...
		// but for now, we just say any unset types flow thru the operator.
		Type* ret=0;
		for (auto i=0; i<this->argls.size();i++){ auto r=this->argls[i]->resolve(sc,desired);
			propogate_type(r,ret);
			propogate_type_fwd(desired,ret);
		}
		if (ret) {printf("got type");ret->dump(-1);printf("\n");}
		// now propogate types back.
		for (auto i=0; i<this->argls.size();i++) {
			if (this->argls[i]->type) {
				propogate_type(ret,this->argls[i]->type);
			} else {
				this->argls[i]->resolve(sc,ret);
			}
		}
		return ResolvedType(ret);
	} else if (op_ident==PLACEHOLDER) {
		propogate_type_fwd(desired,this->type);
		// dump given types here...
		// report candidate functions...
	}
	else {
		printf("resolve call..%s\n",getString(p->ident()));
			// TODO: distinguish 'partially resolved' from fully-resolved.
		// at the moment we only pick an fn when we know all our types.
		// But, some functions may be pure generic? -these are ok to match to nothing.
		// todo:
//		auto n=num_known_arg_types(this->argls);
		if (!this->call_target){
			
			static bool warn=false; if (!warn){warn=true;printf("TODO try re-testing every time, or waiting for max args to be ready? one way might be: no unknown args, between caller & candidate. ( eg caller (f f _) vs callee (_ f f) would infer the last ones ok\n  dont just wait for all on caller.");}
				// Todo: Process Local Vars.
				// TODO: accumulate types of arguments,
				// then use them in find_fn to resolve overloading
				// find_fn should also perform Template Instantiations.
			//we need to know some types before we call anything
//			if (n==this->argls.size())
//			 && n==this->argls.size()
			return resolve_make_fn_call(this, sc,desired);
		} else if (this->call_target){
			this->call_target->resolve(this->scope, desired);
			return ResolvedType(this->call_target->type);
		} else {
			return ResolvedType();
		}
	}
	return ResolvedType();
}

Type* resolve_make_fn_call(ExprBlock* block/*caller*/,CallScope* scope,const Type* desired) {
	for (int i=0; i<block->argls.size(); i++) {
		block->argls[i]->resolve(scope,desired);
		printf("arg %d type=",i); cout<<block->argls[i]->type<<"\n";//->dump(0); printf("\n");
	}
	ASSERT(block->call_target==0);

	ExprFnDef* call_target = scope->find_fn(block->call_op->ident(), block->argls, desired);
	auto fnc=call_target;
	if (call_target!=block->call_target) {
		if (block->call_target) {
		} else {
			printf("improving call match WARNIGN MEM LEAK ON block->scope?\n");
			block->scope=0; // todo delete.
		}
		block->call_target=call_target;
		if (call_target->resolved) {
			if (desired) ASSERT(desired->eq(call_target->type));
			return block->type=call_target->type;
		}
			// add this to the targets' list of calls.
		int num_known_types=(desired?1:0)+num_known_arg_types(block->argls);
		bool isg=fnc->is_generic();
		if (!(isg && num_known_types)) {
			if (fnc->type && desired) {
				fnc->type->dump(-1); desired->dump(-1);
				printf("%d vs %d\n",fnc->type->name,desired->name);
				ASSERT(desired->eq(fnc->type));
			};
			
			block->type = fnc->type?fnc->type:(Type*)desired;
			return ResolvedType(block->type);
			//return fnc->
		}

		// generic function, and we have some types to throw in...
		if (!block->scope) {
			auto sc=new CallScope;
			block->scope=sc;
			scope->push_child(sc);
			sc->outer=call_target;
			block->next_of_call_target = call_target->callers;
			call_target->callers =block;
		}
		// create a local for each supplied argument, using its type..
		// note that vars should be able to propogate inside too???
		CallScope* sc=block->scope;
		for (int i=0; i<block->argls.size() && i<fnc->args.size(); i++) {
			auto input_type=block->argls[i]->type;
			auto v=sc->get_variable(fnc->args[i]->name);
			auto argtype=fnc->args[i]->type;
			if (!v->type){
				v->type=argtype?argtype:input_type;
			} else {
				// read the type out from the function invocation, really.
				if (block->argls[i]->type) {ASSERT(input_type->eq(v->type));}
				else block->argls[i]->type=v->type;
			}
			// and stuff a default expression in for any not called..
		}
		
		auto ret=call_target->resolve_call(sc,desired);
		propogate_type(ret,block->type);
		return ResolvedType(block->type);
	}
	else 
		return ResolvedType();
}


ResolvedType ExprFnDef::resolve_call(CallScope* scope,const Type* desired) {
//	auto scope=new CallScope;		
//	scope->parent=parent; scope->next=parent->child; parent->child=scope;
//	scope->outer=this;
//	scope->node=this->body;
	printf("resolve fn call.. %p\n", scope);
	
	propogate_type_fwd(desired,this->type);
	
	auto rt=this->body->resolve(scope,desired);
	printf("resolve %s yields type:", getString(this->ident()));if (rt.type) rt.type->dump(-1);printf("\n");
	// awkwardness says: type error return is more like an enum that doesn't return a type?
	// if its' a type error we should favour the most significant info: types manually specified(return values,function args)
	auto s=propogate_type(rt,this->type);
	return ResolvedType(rt,s);
}
ResolvedType ExprFnDef::resolve(CallScope* scope, const Type* desired) {
// todo: makes a closure taking locals from parent scope
	if (!this->name) return new Type(FN);
	if (!this->scope){ this->scope=new CallScope; this->scope->global=scope;}
	auto sc=this->scope;
	for (int i=0; i<this->args.size() && i<this->args.size(); i++) {
		auto arg=this->args[i];
		auto v=sc->get_variable(arg->name);
		propogate_type(arg->type,v->type);
		if (arg->default_expr){static int warn=0; if (!warn){warn=1;
			printf("error todo default expressions really need to instantiate new code- at callsite, or a shim; we need to manage caching that. type propogation requires setting those up. Possible solution is giving a variable an initializer-expression? type propogation could know about that, and its only used for input-args?");}
		}
	}
	auto ret=this->body->resolve(sc,desired);
	propogate_type(ret,this->type);
	
	if (!this->fn_type)
		this->fn_type=new Type(FN);
	printf("RESOLVE FN - TODO\n");
	return ResolvedType(fn_type);
}

FnName* find_global_fn_name(CallScope* scope,Name n) {
	auto global=scope->global;
	for (auto f=global->fn_names; f;f=f->next) {
		if (f->name==n) return f;
	}
	auto f=new FnName; f->next=global->fn_names; global->fn_names=f;
	f->name=n; f->fn_defs=0;
	return	f;
}
void link_fn_name(CallScope* global,ExprFnDef* f) {
	if (f->fn_name) return;
	auto n=find_global_fn_name(global,f->name);
	f->next_of_name=n->fn_defs; n->fn_defs=f;
	f->fn_name=n;
}
void gather_functions(Node* node, CallScope* global) {
	if (auto f=dynamic_cast<ExprFnDef*>(node)) {
		// todo: local functions should only be findable inside.
		link_fn_name(global, f);
	} else if (auto b=dynamic_cast<ExprBlock*>(node)) {
		for (auto sub:b->argls) {
			gather_functions(sub,global);
		}		
	}
}
void call_graph(Node* root,CallScope* scope) {
}

struct Bar { string name="fo"; int x=0;};
struct Foo {
	Bar bar;
	vector<int> indices;
	void dump() {cout<<indices;};
};

template<typename T>
T& operator<<(T& dst, const Bar& src) { dst<<src.name<<src.x;return dst;};
template<typename T>
T& operator<<(T& dst, const Foo& src) { dst<<src.bar; dst<<src.indices;return dst;};

bool isSymbolStart(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_';}
bool isSymbolCont(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || (c>='0' && c<='9');}
bool isNumStart(char c){return (c>='0'&&c<='9');};
bool isNum(char c){return (c>='0'&&c<='9') ||c=='.';};
bool isWhitespace(char c){return  c==' '||c=='\n'||c=='\a'||c=='\t';};
bool isOperator(char c){return c=='+'||c=='-'||c=='*'||c=='/'||c=='.'||c=='='||c=='>'||c=='<'||c=='&'||c=='|'||c=='~'||c=='%'||c=='^'||c=='+';}

struct NumDenom{int num; int denom;};

struct TextInput {
	const char* buffer,*tok_start,*tok_end,*prev_start;
	int curr_tok;
	char watch_tok[64][12];

	TextInput(const char* src){
		buffer=src;
		curr_tok=-1;
		tok_start=tok_end=buffer;
		advance_tok();
	}

	void advance_sub(bool (*sub)(char c)){while ( *tok_end && sub(*tok_end)) tok_end++;}
	void advance_operator() {
		int match=0;
		int longest=0;
		for (int i=0; i<IDENT; i++) {
			int len=0;
			const char* cmp=g_token_str[i];
//			printf("%c %c\n",tok_start[0],cmp[0]);
			int j=0;
			for (; cmp[j] ; j++,len++) {
				if (tok_start[j]!=cmp[j]) break;
			}
			if (!cmp[j]) {// got all?
				if (len>longest){longest=len;match=i;}
			}
		}
//		printf("op len=%d",longest);
		tok_end=tok_start+longest;
	}
	void advance_string() {
		tok_end++;
		while (*tok_end && *tok_end!='\"') {
			if (*tok_end=='\\' && tok_end[1]) tok_end++; //skip backslashed quotes inside..
			tok_end++;
		}
		if (*tok_end)
			tok_end++; // step past last.
	}

	void advance_tok() {
		while (isWhitespace(*tok_end)&&*tok_end) tok_end++;
		tok_start=tok_end;
		if (!*tok_end) { this->curr_tok=0; return;}
		auto c=*tok_end;
		if (c=='_' && tok_end[1] && !isSymbolCont(tok_end[1])) tok_end++; //placeholder
		else if (isSymbolStart(c))	advance_sub(isSymbolCont);
		else if (isNumStart(c)) advance_sub(isNum);
		else if (c=='\"') advance_string();
		else advance_operator();
//		else tok_end++;
		this->curr_tok = getStringIndex(tok_start,tok_end);
		for (auto i=10; i>0; i--){strcpy(watch_tok[i],watch_tok[i-1]);}
		memcpy(watch_tok[0],tok_start,tok_end-tok_start); watch_tok[0][tok_end-tok_start]=0;
	}
	int eat_tok() {
		prev_start=tok_start;
		for (const char* c=tok_start; c!=tok_end;c++) {}
		int r=curr_tok;
		advance_tok();
		return r;
	}
	bool eat_if(int i) {	
		if (peek_tok()==i) {eat_tok(); return true;}
		else return false;
	}
	bool is_placeholder()const {return  ((*tok_start=='_') && !isSymbolCont(*tok_end));}
	int eat_if_placeholder(){if (is_placeholder()){advance_tok(); return PLACEHOLDER;} else return 0;}
	int eat_ident() {
		auto r=eat_tok();
		if (r<IDENT) {printf("expected ident found %s",getString(r));exit(0);}
		return r;
	}
	int eat_int() {
		auto nd=eat_number();
		return nd.num/nd.denom;
	}
	const char* eat_string() {
		auto len=(tok_end-tok_start)-2;
		ASSERT(len>=0);
		auto ret=(char*)malloc(len+1);
		memcpy((void*)ret,(void*)(tok_start+1),len+1);
		ret[len]=0;
		return ret;
	}
	NumDenom eat_number()  {
		int	val=0;
		int	frac=0;
		for (const char* p=tok_start;p<tok_end; p++) {
			val*=10; 
			frac*=10;
			if (*p=='.') { frac=1;}
			else
				val+=*p-'0';
		}
		if (frac==0) {frac=1;}
		advance_tok();
		return NumDenom{val,frac};
	}
	float eat_float() {
		auto nd=eat_number();
		return (float)nd.num/(float)nd.denom;
	}
	bool is_next_number() const {
		char c=*tok_start,c1=0;
		if (c) c1=tok_start[1];
		if ((c>='0' && c<='9')||(c=='-' && c1>='0' && c1<='9' && g_lisp_mode))
			return	true;
		else return false;
	}
	bool is_next_literal() const{
		char c=*tok_start;
		if (is_next_number() ||(c==':' && g_lisp_mode)||(c=='\"'))
			return true;
		return false;
	}
	bool is_next_string() const {
		return *tok_start=='\"';
	}

	int peek_tok(){return curr_tok;}
	void reverse(){ ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;}
	int expect(int t){ int x;if (t!=(x=eat_tok())) {printf("expected %s found %s",getString(t), getString(x));exit(0);} return x;}
};
void unexpected(int t){printf("unexpected %s\n",getString(t));exit(0);}
typedef TextInput TokenStream;

Expr* parse_lisp(TokenStream& src);
ExprFnDef* parse_fn(TokenStream&src);
ExprFor* parse_for(TokenStream&src);
ExprIf* parse_if(TokenStream&src);
TypeDef* parse_typedef(TokenStream&src);
ExprStructDef* parse_struct(TokenStream& src);

template<typename T>
T pop(std::vector<T>& v){ ASSERT(v.size()>0);auto r=v[v.size()-1];/*move?*/ v.pop_back(); return r;}
//#define pop(X) ASSERT(X.size()>0); pop_sub(X);

void dump(vector<Expr*>& v) {
	for (int i=0; i<v.size(); i++) {
		v[i]->dump_top();
	}
	printf("\n");
}

void pop_operator_call( vector<int>& operators,vector<Expr*>& operands) {
	//takes the topmost operator from the operator stack
	//creates an expression node calling it, consumes operands,
	//places result on operand stack
									 
	auto * p=new ExprBlock();
	auto op=pop(operators);
	p->call_op=new ExprIdent(op);
	if (operands.size()>=2 && (arity(op)==2)){
		auto arg1=pop(operands);
		p->argls.push_back(pop(operands));
		p->argls.push_back(arg1);
	} else if (operands.size()>=1 && arity(op)==1){
		p->argls.push_back(pop(operands));
	} else{
//						printf("\noperands:");dump(operands);
//						printf("operators");dump(operators);
		printf("\nerror: %s arity %d, %lu operands given\n",getString(op),arity(op),operands.size());
		exit(0);
	}
	operands.push_back(p);
}
//   void fn(x:(int,int),y:(int,int))
void flush_op_stack(ExprBlock* block, vector<int>& ops,vector<Expr*>& vals) {
	while (ops.size()>0) pop_operator_call(ops,vals);
	while (vals.size()) {
		block->argls.push_back(pop(vals));
	}
}
ExprBlock* parse_call(TokenStream&src,int close,int delim, Expr* op);

ExprBlock* parse_expr(TokenStream&src) {
	return parse_call(src,0,0,nullptr);
}

void another_operand_so_maybe_flush(bool& was_operand, ExprBlock* node,
					  vector<int>& operators,
					  vector<Expr*>& operands

					  ){
	if (was_operand==true) {
		printf("warning undeliminated expression parsing anyway");
		flush_op_stack(node,operators,operands);// keep going
	}
	was_operand=true;
}

ExprBlock* parse_call(TokenStream&src,int close,int delim, Expr* op) {
	// shunting yard parserfelchery
	ExprBlock *node=new ExprBlock; node->call_op=op;
	vector<int> operators;
	vector<Expr*> operands;
	bool	was_operand=false;
	int wrong_delim=delim==SEMICOLON?COMMA:SEMICOLON;
	int wrong_close=close==CLOSE_PAREN?CLOSE_BRACE:CLOSE_PAREN;

	while (true) {
		if (!src.peek_tok()) break;
		if (src.peek_tok()==IN) break;
		// parsing a single expression TODO split this into 'parse expr()', 'parse_compound'
		if (close || delim) { // compound expression mode.
			if (src.eat_if(close))
				break;
			if (src.eat_if(wrong_close)) {
				printf("unexpected %s, expected %s",getString(close),getString(wrong_close));
				exit(0);
			}
		} else { // single expression mode - we dont consume delimiter.
			auto peek=src.peek_tok();
			if (peek==CLOSE_BRACKET || peek==CLOSE_BRACE || peek==COMMA || peek==SEMICOLON)
				break;
		}
		printf(":%s\n",getString(src.peek_tok()));
		printf("parse:- %zu %zu\n",operands.size(),operators.size());
//		printf("operands:");dump(operands);
//		printf("operators:");dump(operators);printf("\n");

		print_tok(src.peek_tok());
		if (src.is_next_literal()) {
			if (src.is_next_number()) {
				auto n=src.eat_number();
				ExprLiteral* ln=0;
				if (n.denom==1) {ln=new ExprLiteral(n.num);}
				else {ln=new ExprLiteral((float)n.num/(float)n.denom);}
				operands.push_back(ln);		
				was_operand=true;
				continue;
			} else if (src.is_next_string()) {
				ExprLiteral(src.eat_string());
			} else {
				printf("error parsing literal\n");
			}
		}
		else if (src.eat_if(STRUCT)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_struct(src));
		}
		else if (src.eat_if(FN)) {
//			ASSERT(!was_operand);
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_fn(src));
		}
		else if (src.eat_if(FOR)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_for(src));
		}
		else if (src.eat_if(IF)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_if(src));
		}
		else if (src.eat_if(OPEN_PAREN)) {
			if (was_operand){
				operands.push_back(parse_call(src, CLOSE_PAREN,SEMICOLON, pop(operands)));
				// call result is operand
			}
			else {operands.push_back(parse_call(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(delim)) {
			flush_op_stack(node,operators,operands);
			was_operand=false;
		}
		else if (src.eat_if(wrong_delim) && delim){ //allows ,,;,,;,,  TODO. more precise.
//			printf("error expected %s not %s", getString(delim),getString(wrong_delim));
			flush_op_stack(node,operators,operands);// keep going
			was_operand=false;
		}
		else{
			auto tok=src.eat_tok();
			print_tok(tok);printf("<<<<\n");
			if (is_operator(tok)) {
				if (was_operand) tok=get_infix_operator(tok);
				else tok=get_prefix_operator(tok);

				while (operators.size()>0) {
					int prev_precedence=precedence(operators.back());
					int prec=precedence(tok);
					if (prev_precedence>prec
						||(is_right_assoc(tok)&&prec==prev_precedence))
						break;
					pop_operator_call(operators,operands);
				}
				operators.push_back(tok);
				was_operand=false;
			} else {
				another_operand_so_maybe_flush(was_operand,node,operators,operands);
				operands.push_back(new ExprIdent(tok));
			}
		}
		//ASSERT(sub);
		//node->argls.push_back(sub);
	};
	flush_op_stack(node,operators,operands);
	return node;
}

Type* parse_type(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	Type* ret=0;	// read the first, its the form..
	if (tok==close) return nullptr;
	if (tok==OPEN_PAREN) {
		ret=new Type(TUPLE);
		while (auto sub=parse_type(src, CLOSE_PAREN)){
			ret->push_back(sub);
			src.eat_if(COMMA);
		}
	} else {
		ret = new Type(tok);
		if (src.eat_if(OPEN_BRACKET)) {
			while (auto sub=parse_type(src, CLOSE_BRACKET)){
				ret->push_back(sub);
				src.eat_if(COMMA);
			}
		}
	}
	ret->dump(-1);
	return ret;
	
}
ArgDef* parse_arg(TokenStream& src, int close) {
	auto argname=src.eat_ident();
	if (argname==close) return nullptr;
	auto a=new ArgDef(argname);
	if (src.eat_if(COLON)) {
		a->type=parse_type(src,CLOSE_PAREN);
	}
	if (src.eat_if(ASSIGN)){
		a->default_expr=parse_expr(src);
	}
	return a;
}
void parse_typeparams(TokenStream& src,vector<TypeParam>& out) {
	while (!src.eat_if(CLOSE_BRACKET)){
//		if (src.eat_if(CLOSE_BRACKET)) break;
		auto name=src.eat_tok();
//		int d=0;
//		if (src.eat_if(ASSIGN)) {
//			int d=src.eat_tok();
//		}
		out.push_back(TypeParam{name,src.eat_if(ASSIGN)?src.eat_tok():0});
		src.eat_if(COMMA);
	}
}
ExprStructDef* parse_struct(TokenStream& src) {
	auto sd=new ExprStructDef();
	printf("parse_struct");
	auto tok=src.eat_ident();
	sd->name=tok;
	if (src.eat_if(OPEN_BRACKET)) {
		parse_typeparams(src,sd->typeparams);
	}

	if (!src.eat_if(OPEN_BRACE))
		return sd;
	// todo: type-params.
	while (NONE!=(tok=src.peek_tok())){
		if (tok==CLOSE_BRACE){src.eat_tok(); break;}
		auto arg=parse_arg(src,CLOSE_PAREN);
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
		sd->fields.push_back(arg);
	}
	return sd;
}
void ExprStructDef::dump(int depth) const{
	newline(depth);printf("struct %s",getString(this->name));dump_typeparams(this->typeparams);printf("{");
	
	for (auto f:this->fields){f->dump(depth+1);}
	newline(depth);printf("}");
}

ResolvedType ExprStructDef::resolve(CallScope* scope,const Type* desired){
	if (!this->fn_name ) {
		auto fnm=find_global_fn_name(scope, this->name);
		this->next_of_name=fnm->structs; fnm->structs=this;
	}
	if (!this->type) {
		this->type = new Type(this->name);	// name selects this struct.
	}
	auto s=propogate_type_fwd(desired,this->type);
	return ResolvedType(this->type,s);
}
// iterator protocol. value.init. increment & end test.
ExprFor* parse_for(TokenStream& src){
	auto p=new ExprFor;
	auto first=parse_expr(src);
	if (src.eat_if(IN)){
		p->pattern=first;
		p->init=parse_call(src, OPEN_BRACE, 0, 0);
//		src.expect(OPEN_BRACE);
	} else if (src.eat_if(SEMICOLON)){// cfor.  for init;condition;incr{body}
		p->pattern=0;
		p->init=first;
		p->cond=parse_expr(src);
		src.expect(SEMICOLON);
		p->incr=parse_call(src,OPEN_BRACE,0,0);
	} else {
		printf("c style for loop, expect for init;cond;incr{body}");
		exit(0);
	}
	p->body=parse_call(src, CLOSE_BRACE, SEMICOLON, nullptr);
	if (src.eat_if(ELSE)){
		src.expect(OPEN_BRACE);
		p->else_block=parse_call(src,CLOSE_BRACE, SEMICOLON, nullptr);
	}
	return p;
}

Node* ExprFor::clone()const{
	auto n=new ExprFor;
	n->pattern=(Expr*)pattern->clone_if();
	n->init=(Expr*)init->clone_if();
	n->cond=(Expr*)cond->clone_if();
	n->incr=(Expr*)cond->clone_if();
	n->body=(Expr*)cond->clone_if();
	n->else_block=(Expr*)cond->clone_if();
	return n;
}
// make a flag for c or rust mode
// exact c parser
// add := gets rid of auto noise
// add postfix : alternate functoin syntax
ExprIf* parse_if(TokenStream& src){
	auto p=new ExprIf;
	p->cond=parse_call(src, OPEN_BRACE, 0, 0);
	p->body=parse_call(src, CLOSE_BRACE,SEMICOLON,0);

	if (src.eat_if(ELSE)) {
		if (src.eat_if(IF)) {
			p->else_block= parse_if(src);
		} else if (src.eat_if(OPEN_BRACE)){
			p->else_block=parse_call(src, CLOSE_BRACE, SEMICOLON, 0);
		} else {
			printf("if { }else {} expected\n");
		}
	}
	return p;
}
Node* ExprIf::clone()const {
	auto p=new ExprIf; p->cond=(Expr*)this->cond->clone_if(); p->body=(Expr*)this->body->clone_if(); p->else_block=(Expr*)this->else_block->clone_if();
	return p;
}
void ExprIf::dump(int depth) const {
	newline(depth);printf("if\n");
	cond->dump(depth+1);
	newline(depth);printf("{\n");
	body->dump(depth+1);
	if (else_block)	{
		indent(depth);printf("} else{\n");
		else_block->dump(depth+1);
	}
	newline(depth);printf("}\n");
};


void ExprFor::dump(int d) const {
	newline(d);printf("for ");
	if (this->is_c_for()) {
		this->init->dump(d+1); newline(d);printf(";");
		this->cond->dump(d+1); newline(d);printf(";");
		this->incr->dump(d+1); newline(d);printf(" {");
	} else {
		this->pattern->dump(d+1);
		newline(d);printf(" in ");
		this->init->dump(d+1); newline(d);printf(" {");
	}
	this->body->dump_if(d+1);
	newline(d);printf("}");
	if (this->else_block){
		newline(d);printf("else{");
		this->else_block->dump_if(d+1);
		newline(d);printf("}");
	}
}

// default ++p  is {next(p); p}
// default p++ is {let r=p; next(p); r}
//
// for (x,y) in stuff {
// }
// copy how rust iteration works.
// for iter=begin(stuff); valid(iter); next(iter) { (x,y)=extract(iter);   }
//
// desugars same as c++ for (auto p:rhs)
// for (auto p=rhs.begin(); p!=rhs.end(); ++p)
// no; crap idea.
//
//

ExprFnDef* parse_fn(TokenStream&src) {
	auto *fndef=new ExprFnDef();
	printf("parse_fn");
	// read function name or blank

	auto tok=src.eat_tok(); 
	print_tok(tok);

	if (tok!=OPEN_PAREN) {
		ASSERT(is_ident(tok));
		fndef->name=tok;
		if (src.eat_if(OPEN_BRACKET)) {
			parse_typeparams(src,fndef->typeparams);
		}
		tok=src.expect(OPEN_PAREN);
	} else fndef->name=NONE;
	printf("%s",getString(fndef->name));
	// read function arguments
	while (NONE!=(tok=src.peek_tok())) {
		if (tok==CLOSE_PAREN) {src.eat_tok();break;}
		printf(" arg:%s ",getString(tok));
		auto arg=parse_arg(src,CLOSE_PAREN);
		fndef->args.push_back(arg);
		src.eat_if(COMMA);
	}
	// TODO: multiple argument blocks for currying?.
	if (src.eat_if(ARROW) || src.eat_if(COLON)) {
		fndef->type = parse_type(src, 0);
	}
	printf("fn body:");
	// implicit "progn" here..
	src.expect(OPEN_BRACE);
	fndef->body = new ExprBlock;
	fndef->body = parse_call(src, CLOSE_BRACE, SEMICOLON, nullptr);
	fndef->dump(0);
	return fndef;
}
// every file is an implicitly a function aswell taking no args
// when imported, a module inserts a call to that function.
// that sets up global stuff for it.

const char* g_TestProg=
/*
"(fn lerp((a float) (b float) (f float))(+(*(- b a)f)a))"
"(fn lerp(a b f)(mad a (sub b a) f)
"(fn lerp((a int) (b int) (f float))(+(*(- b a)f)a))"
"(fn bilerp(a b c d u v)(lerp(lerp a b )(lerp c d)v))"
"(fn invlerp(a b x)(/(- x a)(- b a)))"
"(fn madd(a b f)(+a (* b f)))"
"(fn min(a b)(if (< a b)a b))"
"(fn max(a b)(if (> a b)a b))"
"(fn addto(a b)(set a(add a b)))
"(fn madto(a b f)(set a(add a (mul b f))))
"(fn interleave((a int)(c (map string int))(b (vector float))) (return)) "
*/

// semantically we pass something, its' immutable
// doesn't matter if its' value or reference-let compiler decide
// 2 words-pass in regs. any more.. pass adress.
// however you can say pass ownership, move struct
// or pass a mutable reference - mut 
// eg...
/*
	"*++x=*--y e+r:int foo(e,r);"
	"self.pos+self.vel*dt;"
	"future.pos=self.pos+self.vel*dt;"
	"x=y=z=3; x+y+z=0;"	
	"p=&self.pos;"
	"*d++=s;"
	"q=(++10+*p);"
	"fn do_they_float(){set(tfl, 1.0); do_they_int();};"
	"fn min(a,b){if(a<b,a,b)}"
	"fn max(a,b){if(a>b,a,b)}"
	"fn clamp(a,b,f){ min(b,max(a,f)) }"
	"fn lerp(a:float,b:float,f:float){(b-a)*f+a}"
	"fn mad(a:float,b:float,f:float){a+b*f}"
	"fn main(){printf(\"lerp = %.3f ;\",lerp(0.0,10.0,0.5));}"
*/
//	"x=y; y=z; z=0.0;"
	"fn add(a,b){a+b}"
	"fn foo(x:tuple[int,int])->int{_};"
	"fn foo(x:float)->float{_};"
	"fn do_what[X=int](x:X,y:X)->X{_};"
	"fn lerp(i:int,j:int,k:int){(b-a)*f+a};"
	"fn lerp(a,b,f){(b-a)*f+a};"
	"fn render(m:Mesh){}"
	"x=1.0; y=2.0; z=3.0; w=0.5;"
	"foo=lerp(x,add(y,z),w);"
	"struct Mesh[VERTEX,MAT]{ vertices;triangles};"
	"i=20;"
	"fn Mesh()->Mesh{_}"
	"mesh = Mesh();"
	"i=10; j=20.0; k=j+i;"
	"fn itof(i:int)->float{_}"
	"fn make()->tuple[int,int]{_}"
	"obj=make();"
	"i=lerp(p,q,r);"
	"render(mesh);"
	"f1=foo(obj);"
	"f2=foo(y);"
 	"if i<10 {printf(1)} else {printf(2)}"
	"for i in foo {print(loop stuff);} else {printf(); madd(1,2,3);}"
	"for x=0; x<10; x++ { printf(x); }"
	"if i<10 {printf(1)} else {printf(2)}"
//	"	printf(\"hello world\");"
//	"}"
	"lerp(1,2,0);"
/*
"set(glob_x, 10.0);"
"fn they_int(){set(tfx, 1); tfx};"
"fn generic_fn(x y){y};"
"fn generic_fn(x y:float){y};"
	"fn generic_fn(x y:int){y};"
	"fn generic_fn(x:float y:float){ y};"
	"set(glob_y ,1);"
	"set(glob_p, generic_fn(0.0, 1.3));"
	"set(glob_q, generic_fn(0.0, 1));"
*/
	;
// when this works - it can be like SPECS
int main(int argc, const char** argv) {
	TextInput	src(g_TestProg);
	auto node=parse_call(src,0,SEMICOLON,nullptr);
	printf("%p\n",node);
	node->dump(0);
	CallScope global; global.node=(ExprBlock*)node; global.global=&global;
	gather_functions(node,&global);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->dump(0);
//	global.visit_calls();
	output_code(stdout, &global);
	global.dump(0);
}




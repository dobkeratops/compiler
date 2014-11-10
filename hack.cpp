#include "hack.hpp"
#include "codegen.h"
#include "repl.h"

//(sourcecode "hack.cpp")
//(normalize (lerp (obj:zaxis)(normalize(sub(target(obj:pos)))) //(settings:angle_rate)) (sloc 512 40 20))

void dbprintf(const char* str, ... )
{
	char tmp[1024];
	va_list arglist;
	
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	//printf("%s",tmp);
}
// for real compiler errors.
void error(Node* n, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	printf("%s\n",buffer);
}


void print_tok(int i){dbprintf("%s ",getString(i));};

bool g_lisp_mode=false;
const char* g_token_str[]={
	"",
	"int","uint","bool","float","str","void","auto","one","zero","voidptr","ptr","ref","tuple",
	"print___","fn","struct","enum","array","vector","union","variant","with","match",
	"let","set","var",
	"while","if","else","do","for","in","return","break",
	"(",")",
	"{","}",
	"[","]",
	"->",".","=>","<-","::","<->",			//arrows,accessors
	":",
	"+","-","*","/",					//arithmetic
	"&","|","^","%","<<",">>",					//bitwise
	"<",">","<=",">=","==","!=","&&","||",		//compares/logical
	"=",":=",
	"+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=", // assign-op
	"++","--","++","--", //inc/dec
	"-","*","&","!","~", // unary ops
	"*?","*!","&?","[]","&[]", // special pointers
	",",";",
	"...","..",
	"_",
	NULL,	
};


int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,0,0,			// let,set,var
	0,0,0,0,0,0,0,0,  // while,if,else,do,for,in,return,break
	0,0, //( )
	0,0, //{ }
	0,0, // [ ]
	READ|10,READ|2,READ|10,READ|10,READ|13,WRITE|10,	   // arrows
	READ|9,
	READ|6,READ|6,READ|5,READ|5,		//arithmetic
	READ|8,READ|7,READ|8,READ|6,READ|9,READ|9,		//bitwise
	READ|8,READ|8,READ|8,READ|8,READ|9,READ|9,READ|13,READ|14,	//compares/logical
	WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16, // assignment
	
	WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16, // assign-op
	
	MODIFY|PREFIX|UNARY|2,MODIFY|PREFIX|UNARY|2,MODIFY|UNARY|ASSOC|3,MODIFY|UNARY|ASSOC|3, // post/pre inc/dec
	READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3, //unary ops
	READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, /// special pointers
	0,0, // delim
	0,
	0,0,
	0, //placeholder
};
bool is_ident(int tok){return tok>=IDENT;}
bool is_type(int tok){return tok<T_NUM_TYPES;}
bool is_operator(int tok){ return tok>=ARROW && tok<COMMA;}
int operator_flags(int tok){return g_tok_info[tok];}
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

LLVMOp g_llvm_ops[]= {
	{-1,"add"},{-1,"sub"},{-1,"mul"},{-1,"div"},
	{-1,"and"},{-1,"or"},{-1,"xor"},{-1,"srem"},{-1,"shl"},{-1,"ashr"},
	{-1,"icmp slt"},
	{-1,"icmp sgt"},
	{-1,"icmp sle"},
	{-1,"icmp sge"},
	{-1,"icmp eq"},
	{-1,"icmp ne"},
	{-1,"and"},
	{-1,"or"},
};
const char* g_llvm_type[]={
	"i32","i32","i1","float"
};
const char* get_llvm_type_str(int tname){
	switch (tname){
		case INT:return "i32";
		case UINT:return "i32";
		case BOOL:return "bool";
		case FLOAT:return "float";
		case VOID:return "void";
		case STR:return "u8*";
		default: return getString(tname);
	}
}

const LLVMOp* get_op_llvm(int tok,int type){
	if (tok>=ADD && tok<=SHR) return &g_llvm_ops[tok-ADD];
	if (tok>=ADD_ASSIGN && tok<=SHR_ASSIGN) return&g_llvm_ops[tok-ADD_ASSIGN];
	if (tok>=GT && tok<=NE) return &g_llvm_ops[tok-GT];
	return 0;
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
	dbprintf("%d %s\n",COMMA,g_token_str[COMMA]);
	dbprintf("%d %s\n",SEMICOLON,g_token_str[SEMICOLON]);
	dbprintf("%d",nextId);
	ASSERT(nextId==IDENT);
}
int StringTable::get_index(const char* str, const char* end) {
	auto len=(end)?(end-str):strlen(str);
	string s; s.resize(len);memcpy((char*)s.c_str(),str,len);((char*)s.c_str())[len]=0;
	auto ret=names.find(s);
	if (ret!=names.end())	return ret->second;		
	names.insert(std::make_pair(s,nextId));
	index_to_name.resize(nextId+1);
	index_to_name[nextId]=s;
//		std::cout<<s <<index_to_name[nextId];
	if (verbose)
		dbprintf("insert[%d]%s\n",nextId,index_to_name[nextId].c_str());
	return	nextId++;
};

void StringTable::dump(){
	dbprintf("\n");
	for (int i=0; i<this->nextId; i++) {
		dbprintf("[%d]%s\n",i,this->index_to_name[i].c_str());
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
	for (int i=0; i<depth; i++){dbprintf("\t");};
}
void newline(int depth) {
	if (depth>=0) dbprintf("\n"); indent(depth);
}
// Even a block is an evaluatable expression.
// it may contain outer level statements.

void Expr::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("(?)");
}
void Expr::dump_top() const {
	if (!this) return;
	dbprintf("%s ", getString(name));
}

Expr::Expr(){ type=0;}
ResolvedType assert_types_eq(const Type* a,const Type* b) {
	ASSERT(a && b);
	if (!a->eq(b)){
		error(0,"type error:"); a->dump(-1);error(0,"!=");b->dump(-1);error(0,"\n");
		return ResolvedType(a,ResolvedType::ERROR);
	}
	return ResolvedType(a,ResolvedType::COMPLETE);
}
RegisterName Node::get_reg_existing(){ASSERT(regname); return regname;}
RegisterName Node::get_reg(Name baseName, int *new_index, bool force_new){
	// variable might be in a local scope shadowing others, so it still needs a unique name
	// names are also modified by writes, for llvm SSA
	if (!regname || force_new){
		return get_reg_new(baseName,new_index);
	} else
		return regname;
}
RegisterName Node::get_reg_new(Name baseName, int* new_index) {
	char name[256];
	sprintf(name, "r%d%s",(*new_index)++,baseName>=IDENT?getString(baseName):"");
	return this->regname=g_Names.get_index(name,0);
}


const Type* any_not_zero(const Type* a, const Type* b){return a?a:b;}
ResolvedType propogate_type(Type*& a,Type*& b) {
	if (!(a || b)) return ResolvedType(0,ResolvedType::INCOMPLETE);
	if (!a && b) {a=b; return ResolvedType(a,ResolvedType::COMPLETE);}
	else if (!b && a) {b=a; return ResolvedType(b,ResolvedType::COMPLETE);}
	else {
		return assert_types_eq(a,b);
	}
}
ResolvedType propogate_type_fwd(const Type*& a,Type*& b) {
	if (!(a || b)) return ResolvedType(0,ResolvedType::INCOMPLETE);
	if (!a && b){return ResolvedType(b,ResolvedType::INCOMPLETE);}
	if (!b && a) {b=(Type*)a;return ResolvedType(a,ResolvedType::COMPLETE);}
	else {return assert_types_eq(a,b);  }
}
ResolvedType propogate_type(Type*& a,Type*& b,Type*& c) {
	int ret=ResolvedType::COMPLETE;
	ret|=propogate_type(a,b).status;
	ret|=(c)?propogate_type(b,c).status:ResolvedType::INCOMPLETE;
	ret|=(c)?propogate_type(a,c).status:ResolvedType::INCOMPLETE;
	const Type* any=any_not_zero(a,any_not_zero(b,c));
	return ResolvedType(any,ret);
}
ResolvedType propogate_type_fwd(const Type*& a,Type*& b,Type*& c) {
	int ret=ResolvedType::COMPLETE;
	ret|=propogate_type_fwd(a,b).status;
	ret|=propogate_type_fwd(a,c).status;
	ret|=propogate_type(b,c).status;
	return ResolvedType(any_not_zero(a,any_not_zero(b,c)),ret);
}
ResolvedType propogate_type(ResolvedType& a,Type*& b) {
	a.combine(propogate_type(a.type,b));
	return a;
}
ResolvedType propogate_type(ResolvedType& a,Type*& b,const Type* c) {
	a.combine(propogate_type_fwd(c,b));
	a.combine(propogate_type(a.type,b));
	return a;
}
StructDef* dump_find_struct(Scope* s, Name name){
	for (;s;s=s->parent_or_global()){
		dbprintf("find %s in in scope %s\n",getString(name),s->name());
		for (auto ni=s->named_items;ni;ni=ni->next){
			dbprintf("name %s\n",getString(ni->name));
			for (auto sd=ni->structs; sd;sd=sd->next_of_name) {
				dbprintf("? %s\n",getString(sd->name));
				
			}
		}
	}
	return nullptr;
}


ResolvedType ExprIdent::resolve(Scope* scope,const Type* desired) {
	// todo: not if its' a typename,argname?
	if (this->is_placeholder()) {
		//PLACEHOLDER type can be anything asked by environment, but can't be compiled .
		propogate_type_fwd(desired,this->type);
		return ResolvedType(this->type,ResolvedType::COMPLETE);
	}
	propogate_type_fwd(desired,this->type);
	if (auto sd=scope->find_struct(this->name)) {
		if (!this->type){
			this->type=new Type(sd);
			return propogate_type_fwd(desired,this->type);
		}
	}
	if (auto v=scope->find_variable_rec(this->name)){
		return propogate_type(this->type,v->type);
	}
	return ResolvedType();
}
void ExprIdent::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("%s:",getString(name));
	if (this->type) {this->type->dump(-1);}
}

Name ExprBlock::get_fn_name()const
{	// distinguishes operator &  function call
	if (call_operator){
		ASSERT(dynamic_cast<ExprIdent*>(call_operator)&& "TODO: distinguish expression with computed function name");
		return call_operator->name;
	}
	else if (get_fn_call()){return get_fn_call()->name;}
	else return 0;
}


int ExprBlock::get_op_name()const{
	ASSERT(0);
	if (call_operator!=0)
		return call_operator->get_name();
	else return 0;
}
void ExprBlock::dump(int depth) const {
	if (!this) return;
	newline(depth);
	if (this->call_operator || this->get_fn_call()){
		int id=this->call_operator->ident();
		if (is_operator(id)) {
			if (is_prefix(id))dbprintf("prefix");
			else if (this->argls.size()>1)dbprintf("infix");
			else dbprintf("postfix");print_tok(id);
		}
		else if(get_fn_call()){
			dbprintf("%s",getString(get_fn_call()->name));
		};
		if (this->type) {dbprintf(":");this->type->dump(-1);};dbprintf("(");} else dbprintf("{");
	for (const auto x:this->argls) {
		if (x) {x->dump(depth+1);}else{dbprintf("(none)");}
	}
	newline(depth);if (this->call_operator)dbprintf(")");else dbprintf("}");
//	newline(depth);
}

ExprBlock::ExprBlock(){call_target=0;}

const char* Scope::name() const {
	if (!parent){
		return"global";
	} 
	else return getString(owner->name);
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
		dbprintf("(");
		for (auto t=sub; t; t=t->next){
			t->dump_sub();
			if(t->next)dbprintf(",");
		};
		dbprintf(")");
	} else{
		dbprintf("%s",getString(name));
		if (sub){
			dbprintf("[");
			for (auto t=sub; t; t=t->next){
				t->dump_sub();
				if(t->next)dbprintf(",");
			};
			dbprintf("]");
		}
	}
}
bool Type::is_struct()const{
	return struct_def!=0;
}
int Type::is_pointer() const {
	if (!this) return 0;
	if (this->name==PTR || this->name==REF)
		return 1+this->is_pointer();
	else return 0;
}
ExprStructDef* Type::get_struct()const{
	auto p=this;
	while (p && !p->is_struct()){
		p=p->sub;
	}
	return p?p->struct_def:0;
}
void Type::dump(int depth)const{
	if (!this) return;
	newline(depth);dump_sub();
}
Type::Type(ExprStructDef* sd)
{	struct_def=sd; name=sd->name;
}


void ExprLiteral::dump(int depth) const{
	if (!this) return;
	newline(depth);
	if (type_id==T_VOID){dbprintf("void");}
	if (type_id==T_INT){dbprintf("%d",u.val_int);}
	if (type_id==T_FLOAT){dbprintf("%.7f",u.val_float);}
	if (type_id==T_CONST_STRING){
		dbprintf("\"%s\"",u.val_str);
	}
}
// TODO : 'type==type' in our type-engine
//	then we can just make function expressions for types.

ResolvedType ExprLiteral::resolve(Scope* sc , const Type* desired){
	if (!this->type) {
		Type* t=nullptr;
		switch (type_id) {
		case T_VOID: t=new Type(VOID); break;
		case T_INT: t=new Type(INT); break;
		case T_FLOAT: t=new Type(FLOAT); break;
		case T_CONST_STRING: t=new Type(STR); break;
		default: break;
		}
		this->type=t; // one time resolve event.
		this->next_of_scope=sc->global->literals;
		sc->global->literals=this;
		char str[256]; if (!name){sprintf(str,"str%x",(uint32_t)(size_t)this); name=getStringIndex(str);}
	}
	return propogate_type_fwd(desired,this->type);
}
int ExprLiteral::strlen() const{
	if (type_id==T_CONST_STRING)
		return ::strlen(this->u.val_str);
	else return 0;
}

ExprLiteral::ExprLiteral(float f) {
	type=nullptr;
	type_id=T_FLOAT;
	u.val_float=f;
	dbprintf("lit float %.3f",f);
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
	dbprintf("[");
	for (auto t:ts){
		if (a)dbprintf(",");
		print_tok(t.name);if (t.defaultv){dbprintf("=");print_tok(t.defaultv);}
		a=true;
	}
	dbprintf("]");
}

void ArgDef::dump(int depth) const {
	newline(depth);dbprintf("%s",getString(name));
	if (type) {dbprintf(":");type->dump(-1);}
	if (default_expr) {dbprintf("=");default_expr->dump(-1);}
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
bool ExprBlock::is_undefined() const{
	if (!this) return false; //only presencence of "_" is undefined.
	for (auto x:argls){if (x->is_undefined()) return true;}
	return false;
}

void ExprFnDef::dump(int ind) const {
	if (!this) return;
	newline(ind);dbprintf("fn %s",getString(name));dump_typeparams(this->typeparams);dbprintf("(");
	for (int i=0; i<args.size();i++){
		args[i]->dump(-1);
		if (i<args.size()-1) dbprintf(",");
	}
	if (variadic) dbprintf(args.size()?",...":"...");
	dbprintf(")");
	if (this->ret_type) {dbprintf("->");this->ret_type->dump(-1);};
	if (this->body) {
		this->body->dump(ind);
	}
	if (auto p=this->instances){
		dbprintf("instantiations:");
		for (;p;p=p->next_instance){
			p->dump(ind);
		}
	}
}

Expr* ExprFnDef::get_return_value() const{
	if (this->body){
		if (this->body->argls.size()>0){
			return this->body->argls.back();
		}
	}
	return 0;
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

/*
void Scope::visit_calls() {
	for (auto call=this->calls;call;call=call->next_of_scope) {
		dbprintf("%s --> %s\n",this->name(), getString(call->callee->name));
	}
	for (auto sub=this->child; sub; sub=sub->next)
		sub->visit_calls();
}
*/
ExprFnDef* instantiate_generic_function(Scope* s,ExprFnDef* src, vector<Expr*>& call_args) {
	ExprFnDef* new_fn =(ExprFnDef*) src->clone();
	if (src->name_ptr && !new_fn->name_ptr) {
		s->add_fn(new_fn);
	}
	// fill any args we can
	for (auto i=0; i<new_fn->args.size(); i++){
		if (!new_fn->args[i]->type && call_args[i]->type) {new_fn->args[i]->type=(Type*)call_args[i]->type->clone();}
	}
	new_fn->next_instance = src->instances;
	src->instances=new_fn;
	new_fn->resolved=false;
	dbprintf("generic instantiation:-\n");
	new_fn->dump(0);
	return new_fn;	// welcome new function!
}
Node* ExprBlock::clone() const {
	if (!this) return nullptr;
	auto r=new ExprBlock();
	if (this->call_operator) {
		r->call_operator = (Expr*) this->call_operator->clone();
	}
	r->call_target=this->call_target;
	r->name=this->name;
	r->argls.resize(this->argls.size());
	for (int i=0; i<this->argls.size(); i++) {
		r->argls[i]=(Expr*)(this->argls[i]->clone());
	}
	return (Node*)r;
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

//void find_printf(const char*,...){};
#define find_printf dbprintf
int num_known_arg_types(vector<Expr*>& args) {
	int n=0; for (auto i=0; i<args.size(); i++) {if (args[i]->type) n++;} return n;
}

void compare_candidate_function(ExprFnDef* f,Name name,vector<Expr*>& args,const Type* ret_type, ExprFnDef** best_fn, int* best_score,int* ambiguity) {
	if (f->name!=name)
		return ;
	// TODO: may need to continually update the function match, eg during resolving, as more types are found, more specific peices may appear?
	// Find max number of matching arguments
	if (args.size() >f->args.size() && !f->variadic)	// if not enough args, dont even try.
		return;
	if (args.size() <f->args.size())	// TODO: bring default arguments into the picture.
		return;

	find_printf("candidate:");
	for (int i=0; i<args.size() && i<f->args.size(); i++) {
//		find_printf("%s ",f->args[i]->type->get_name_str());
		find_printf("arg %d %s",i,getString(f->args[i]->name));
		auto t=f->args[i]->type;
		if (t) t->dump(-1);
		find_printf("\n");
	}
	find_printf("\n");
	int score=0;
	if (f->variadic)
		score=1;	// variadic functoin can match anything?
	for (int i=0; i<args.size(); i++) {
		if (i<f->args.size()){
			if (f->args[i]->type->eq(args[i]->type)) {
				find_printf("match %s %s\n",f->args[i]->type->get_name_str(), args[i]->type->get_name_str());
				score++;
			}
		}
	}

	// consider return type in call.
	if (ret_type) if (f->type->eq(ret_type)) score++;
		
	// for any argument not matched, zero the score if its the *wrong* argument?
	// TODO: this is where we'd bring conversion operators into play.
	for (int i=0; i<args.size() && i<f->args.size(); i++) {
		if ((!f->args[i]->type->eq(args[i]->type)) && f->args[i]->type!=0) score=-1;
	}
	find_printf("score is %d\n",score);
	if (score >*best_score) {
		*best_score=score;
		*best_fn=f;
		ambiguity=0;
	} else if (score==*best_score) ambiguity++;
}
void find_fn_sub(Expr* src,Name name,vector<Expr*>& args, const Type* ret_type,ExprFnDef** best_fn, int* best_score,int* ambiguity) {
	if (auto sb=dynamic_cast<ExprBlock*>(src)) {
		find_printf("look for %s in %s\n",getString(name),src->get_name_str());
		for (auto x:sb->argls) {
			find_fn_sub(x,name,args,ret_type,best_fn,best_score,ambiguity);
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(src)){
		compare_candidate_function(f,name,args,ret_type, best_fn,best_score,ambiguity);
	}
}

ExprFnDef*	Scope::find_fn(Name name, vector<Expr*>& args,const Type* ret_type)  {
	find_printf("\nfind call with args(");
	for (int i=0; i<args.size(); i++) {find_printf(" %d:",i);find_printf("%p\n",args[i]);if (args[i]->type) args[i]->type->dump(-1);}
	find_printf(")\n");

	ExprFnDef*	best=0;
	int best_score=-1;
	int	ambiguity=0;
	for (auto src=this; src; src=src->parent) {// go back thru scopes.
		find_printf("in scope %p\n",src);
		if (auto fname=src->find_named_items_local(name)){
			for (auto f=fname->fn_defs; f;f=f->next_of_name) {
				find_fn_sub(f,name, args,ret_type,&best,&best_score,&ambiguity);
			}
		}
	}
//	if (!best) {
//		printf("\ncant find any function  %s\n",getString(name));
//		this->global->dump(0);
//		printf("\ncant find any function  %s\n",getString(name));
//		exit(0);
//		return 0;f
//	}
	find_printf("match score=%d/%z\n", best_score, args.size());
	if (!best)  {
		error(0,"No match found for %s",getString(name));
		return nullptr;
	}
	if (ambiguity){
		error(nullptr,"ambiguous matches for %s",getString(name));
	}
	if (best->is_generic()) {
		find_printf("matched generic function: instanting\n");
		return instantiate_generic_function(this, best, args);
	}
	
	return best;
}
Variable* Scope::find_fn_variable(Name name,ExprFnDef* f){
	// todo: This Pointer?
	if (this->owner!=f) return nullptr;
	for (auto v=this->vars; v; v=v->next) {
		if (v->name==name) return v;
	}
	if (this->parent){
		if (auto p=this->parent->find_fn_variable(name,f))
			return	p;
	}
	if (this->global && this->global!=this){
		if (auto p=this->global->find_fn_variable(name,f))
			return	p;
	}
	dbprintf(";fn %s no var %s in scope\n",getString(f->name),getString(name));
	return nullptr;
}
ExprStructDef* Scope::find_struct(Name name){
	if (auto fn=this->find_named_items_local(name)){
		for (auto st=fn->structs; st;st=st->next_of_name){
			if (st->name==name)
				return st;
		}
	}
	if (auto p=parent_or_global()) return p->find_struct(name);
	else return nullptr;
}
void Scope::add_fn(ExprFnDef* fnd){
	if (fnd->name_ptr) return;
	auto ni=get_named_items_local(fnd->name);
	fnd->name_ptr=ni;
	fnd->next_of_name=ni->fn_defs;
	ni->fn_defs=fnd;
}
void Scope::add_struct(ExprStructDef* sd){
	if (sd->name_ptr) return;
	auto ni=get_named_items_local(sd->name);
	sd->name_ptr=ni;
	sd->next_of_name=ni->structs;
	ni->structs=sd;
	
}
Variable* Scope::find_scope_variable(Name name){
	for (auto v=this->vars; v;v=v->next){
		if (v->name==name) return v;
	}
	return nullptr;
}
Variable* Scope::find_variable_rec(Name name){
	if (auto v=find_scope_variable(name)) return v;
	if (this->parent) return this->parent->find_variable_rec(name);
	return nullptr;
	
}
Variable* Scope::get_or_create_variable(Name name,VarKind k){
	if (auto v=this->find_variable_rec(name)) {
		return v;
	}
	return this->create_variable(name,k);
}
Variable* Scope::create_variable(Name name,VarKind k){
	auto exv=this->find_scope_variable(name);
	ASSERT(exv==0);
	auto v=new Variable(name,k); v->next=this->vars; this->vars=v;
	v->name=name;v->owner=this;
	return v;
}
Variable* Scope::get_scope_variable(Name name,VarKind k){
	auto exv=this->find_scope_variable(name);
	if (exv) return exv;
	auto v=new Variable(name,k); v->next=this->vars; this->vars=v;v->owner=this;
	v->name=name;
	return v;
}
void Scope::dump(int depth)const {
	newline(depth);dbprintf("scope: %s {", this->owner?getString(this->owner->ident()):"global");
	for (auto v=this->vars; v; v=v->next) {
		newline(depth+1); dbprintf("var %d %s:",v->name, getString(v->name));
		if (v->type){ v->type->dump(-1);} else {dbprintf("no_type");}
	}
	for (auto f=this->named_items; f;f=f->next){
		newline(depth+1); dbprintf("name %s:",getString(f->name));
	}
	for (auto s=this->child; s; s=s->next){
		s->dump(depth+1);
	}
	newline(depth);dbprintf("}");
}
template<typename T>
void dump(vector<T*>& src) {
	for (int i=0; i<src.size(); i++) {
		dbprintf(src[i]->dump());
	}
}
ResolvedType ExprBlock::resolve(Scope* sc, const Type* desired) {
	if (this->argls.size()<=0 && this->is_compound_expression() ) {
		if (!this->type) this->type=new Type(VOID);
		return propogate_type_fwd(desired,this->type);
	}
	int op_ident=NONE;
	ExprIdent* p=nullptr;
	if(this->call_operator){p=dynamic_cast<ExprIdent*>(this->call_operator); op_ident=p->name;}
	if (this->is_compound_expression()) {	// do executes each expr, returns last ..
		for (auto i=0; i<this->argls.size()-1; i++) {
			this->argls[i]->resolve(sc,0);
		}
		// last expression - type bounce
		if (auto i=this->argls.size()) {
			propogate_type_fwd(desired,this->type);
			auto ret=this->argls[i-1]->resolve(sc,desired);
			return propogate_type(ret,this->type);
		}
		else {ASSERT(0);return ResolvedType();}
	}
	else 
	if (op_ident==ASSIGN || op_ident==LET_ASSIGN) {
		ASSERT(this->argls.size()==2);
		auto vname=this->argls[0]->ident();	//todo: rvalue malarchy.
//			printf("set: try to create %d %s %s\n", vname, getString(vname), this->args[1]->kind_str());
		auto rhs_t=this->argls[1]->resolve(sc,desired);
		this->argls[0]->dump(0);
		dbprintf("resolve block getvar %p %s\n", sc, getString(vname));
		if (op_ident==LET_ASSIGN){
			auto new_var=sc->get_scope_variable(vname,Local);
			ASSERT(new_var);
			return propogate_type(rhs_t, new_var->type,desired);
		}
		else {
			propogate_type_fwd(desired, this->type);
			propogate_type(this->argls[1]->type, this->type);
			auto lhs_t=this->argls[0]->resolve(sc,this->type);
			propogate_type(this->type,this->argls[0]->type);
			return propogate_type(this->type,this->argls[0]->type);
			
//			propogate_type_fwd(desired,lhs_t);
//			return propogate_type(lhs_t,rhs_t);
		}
//			return propogate_type(
//			v=sc->find_variable_rec(name);
//			ASSERT(v);
//		}
//		propogate_type_fwd(desired,this->type);
//		if (new_var) {
//		}else{
//			printf("TODO: there's another error todo - how to resolve complex Lvalue");
//		}
//		return propogate_type(rhs_t,this->type,desired);
		
	} else if (is_operator(op_ident)){
		// todo: &t gets adress. *t removes pointer, [i] takes index,
		// comparisons yield bool, ...
		// but for now, we just say any unset types flow thru the operator.
		Type* ret=0;
		if (op_ident==COLON){
			// todo: get this in the main parser
			ASSERT(dynamic_cast<ExprIdent*>(this->argls[1]));
			int tname=this->argls[1]->name;
			auto v=sc->find_variable_rec(this->argls[0]->name);
			if (!v->type){
				v->type=new Type(tname);
			}
			propogate_type(v->type,this->type);
		}
		else
		if (op_ident==DOT || op_ident==ARROW) {
			auto lhs=this->argls[0]; auto rhs=this->argls[1];
			auto lhs_t=this->argls[0]->resolve(sc, 0);//type doesn't push up- the only info we have is what field it needs
			auto t=lhs_t.type;
			dbprintf("resolve %s.%s   lhs:",getString(lhs->name),getString(rhs->name));if (t) t->dump(-1);dbprintf("\n");

			// TODO: assert that lhs is a pointer or struct? we could be really subtle here..
			if (t) {
				t=t->deref_all();
			// now we have the elem..
				ASSERT(dynamic_cast<ExprIdent*>(rhs));
				if (auto st=sc->find_struct(t->name)){
					if (auto f=st->find_field(rhs->name)){
						f->dump(-1);
						ret=f->type;
						propogate_type(ret,this->type);
					}
				}
			}
		}
		else {
			// regular operator
			propogate_type_fwd(desired, this->type);
			for (auto i=0; i<this->argls.size();i++){
				auto r=this->argls[i]->resolve(sc,desired);
				propogate_type(r,this->type);
//				propogate_type_fwd(desired,ret);
			}
		}
		return propogate_type_fwd(desired, this->type);
	} else if (op_ident==PLACEHOLDER) {
		return propogate_type_fwd(desired,this->type);
		// dump given types here...
		// report candidate functions...
	}
	else {
		dbprintf("resolve call..%s\n",getString(p->ident()));
			// TODO: distinguish 'partially resolved' from fully-resolved.
		// at the moment we only pick an fn when we know all our types.
		// But, some functions may be pure generic? -these are ok to match to nothing.
		// todo:
//		auto n=num_known_arg_types(this->argls);
		if (!this->call_target){
			
			static bool warn=false; if (!warn){warn=true;dbprintf("TODO try re-testing every time, or waiting for max args to be ready? one way might be: no unknown args, between caller & candidate. ( eg caller (f f _) vs callee (_ f f) would infer the last ones ok\n  dont just wait for all on caller.");}
				// Todo: Process Local Vars.
				// TODO: accumulate types of arguments,
				// then use them in find_fn to resolve overloading
				// find_fn should also perform Template Instantiations.
			//we need to know some types before we call anything
//			if (n==this->argls.size())
//			 && n==this->argls.size()
			return resolve_make_fn_call(this, sc,desired);
		} else if (this->call_target){
			return this->call_target->resolve(this->scope, desired);
		} else {
			return ResolvedType();
		}
	}
	return ResolvedType();
}

ResolvedType resolve_make_fn_call(ExprBlock* block/*caller*/,Scope* scope,const Type* desired) {
	for (int i=0; i<block->argls.size(); i++) {
		block->argls[i]->resolve(scope,desired);
		dbprintf("resolve arg %d type=",i); auto t=block->argls[i]->type; if (t) t->dump(-2); printf("\n");
	}
	ASSERT(block->call_target==0);

	ExprFnDef* call_target = scope->find_fn(block->call_operator->ident(), block->argls, desired);
	auto fnc=call_target;
	if (call_target!=block->call_target) {
		if (block->call_target) {
			dbprintf("improving call match WARNIGN MEM LEAK ON block->scope?\n");
			block->scope=0; // todo delete.
		} else {
			block->scope=0; // todo delete.
		}
		block->call_target=call_target;
		if (call_target->resolved) {
			Type * fnr=call_target->return_type();
			return propogate_type_fwd(desired, block->type,fnr);
		}
			// add this to the targets' list of calls.
		int num_known_types=(desired?1:0)+num_known_arg_types(block->argls);
		bool isg=fnc->is_generic();
		if (!(isg && num_known_types)) {
			// concrete function: we can just take return type.
			auto rt=fnc->return_type();
			return propogate_type_fwd(desired, rt,block->type);
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
		return ResolvedType();
		// generic function, and we have some types to throw in...
		// if its' a generic function, we have to instantiate it here.
		
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
			auto input_type=block->argls[i]->type;
			auto v=fsc->create_variable(fnc->args[i]->name,VkArg);
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
		
		auto ret=call_target->resolve_call(fsc,desired);
		return propogate_type(ret,block->type);
	}
	else 
		return ResolvedType();
}


ResolvedType ExprFnDef::resolve_call(Scope* scope,const Type* desired) {
//	auto scope=new Scope;		
//	scope->parent=parent; scope->next=parent->child; parent->child=scope;
//	scope->outer=this;
//	scope->node=this->body;
	dbprintf("resolve fn call.. %p\n", scope);
	
	propogate_type_fwd(desired,this->ret_type);
	
	auto rt=this->body->resolve(scope,desired);
	dbprintf("resolve %s yields type:", getString(this->ident()));if (rt.type) rt.type->dump(-1);printf("\n");
	// awkwardness says: type error return is more like an enum that doesn't return a type?
	// if its' a type error we should favour the most significant info: types manually specified(return values,function args)
	return propogate_type(rt,this->ret_type);
}
ResolvedType ExprFnDef::resolve(Scope* definer_scope, const Type* desired) {
// todo: makes a closure taking locals from parent scope
//	if (!this->name) {return new Type(FN);
		
//	}
	if (!this->scope){ this->scope=new Scope; definer_scope->push_child(this->scope); scope->owner=this; };
		//this->scope->parent=this->scope->global=scope->global; this->scope->owner=this;}
	auto sc=this->scope;
	for (int i=0; i<this->args.size() && i<this->args.size(); i++) {
		auto arg=this->args[i];
		auto v=sc->find_scope_variable(arg->name);
		if (!v){v=sc->create_variable(arg->name,VkArg);}
		propogate_type(arg->type,v->type);
		if (arg->default_expr){static int warn=0; if (!warn){warn=1;
			dbprintf("error todo default expressions really need to instantiate new code- at callsite, or a shim; we need to manage caching that. type propogation requires setting those up. Possible solution is giving a variable an initializer-expression? type propogation could know about that, and its only used for input-args?");}
		}
	}
	if (this->body){
		auto ret=this->body->resolve(sc,this->ret_type);
		propogate_type(ret,this->ret_type);
	}

	if (!this->fn_type)
		this->fn_type=new Type(FN);
	{ int once;if (!once++) dbprintf("RESOLVE FN DEF TYPE PROPERLY- TODO cleanup ambiguity between fn ptr type & fn return type!\n");
	}
	return ResolvedType(fn_type,ResolvedType::COMPLETE);
}

void gather_named_items(Node* node, Scope* sc) {
	if (auto fd=dynamic_cast<ExprFnDef*>(node)) {
		// todo: local functions should only be findable inside.
		sc->add_fn(fd);
	} else if (auto sd=dynamic_cast<ExprStructDef*>(node)){
		sc->add_struct(sd);
	} else if (auto b=dynamic_cast<ExprBlock*>(node)) {
		for (auto sub:b->argls) {
			gather_named_items(sub,sc);
		}		
	}
}
void call_graph(Node* root,Scope* scope) {
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
	void advance_string(char quote) {
		tok_start=tok_end;
		tok_end++;
		while (*tok_end && *tok_end!=quote) {
			if (*tok_end=='\\' && tok_end[1]) tok_end++; //skip backslashed quotes inside..
			tok_end++;
		}
		if (*tok_end)
			tok_end++; // step past last quote.
	}

	void advance_tok() {
		while (isWhitespace(*tok_end)&&*tok_end) tok_end++;
		tok_start=tok_end;
		if (!*tok_end) { this->curr_tok=0; return;}
		auto c=*tok_end;
		if (c=='_' && tok_end[1] && !isSymbolCont(tok_end[1])) tok_end++; //placeholder
		else if (isSymbolStart(c))	advance_sub(isSymbolCont);
		else if (isNumStart(c)) advance_sub(isNum);
		else if (c=='\"')
			advance_string('\"');
		else if (c=='\'')
			advance_string('\'');
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
		if (r<IDENT) {error(0,"expected ident found %s",getString(r));exit(0);}
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
		advance_tok();
		return ret;
	}
	NumDenom eat_number()  {
		int	val=0;
		int	frac=0;
		for (const char* p=tok_start;p<tok_end; p++) {
			if (*p=='.') { frac=1;}
			else {
				val*=10;
				frac*=10;
				val+=*p-'0';
			}
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
		if (is_next_number() ||(c==':' && g_lisp_mode)|| is_next_string())
			return true;
		return false;
	}
	bool is_next_string() const {
		return *tok_start=='\"';
	}
	bool is_next_char() const {
		return *tok_start=='\'';
	}

	int peek_tok(){return curr_tok;}
	void reverse(){ ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;}
	int expect(int t){ int x;if (t!=(x=eat_tok())) {error(0,"expected %s found %s",getString(t), getString(x));exit(0);} return x;}
};
void unexpected(int t){error(0,"unexpected %s\n",getString(t));exit(0);}
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
	dbprintf("\n");
}

void pop_operator_call( vector<int>& operators,vector<Expr*>& operands) {
	//takes the topmost operator from the operator stack
	//creates an expression node calling it, consumes operands,
	//places result on operand stack
									 
	auto * p=new ExprBlock();
	auto op=pop(operators);
	p->call_operator=new ExprIdent(op);
	if (operands.size()>=2 && (arity(op)==2)){
		auto arg1=pop(operands);
		p->argls.push_back(pop(operands));
		p->argls.push_back(arg1);
	} else if (operands.size()>=1 && arity(op)==1){
		p->argls.push_back(pop(operands));
	} else{
//						printf("\noperands:");dump(operands);
//						printf("operators");dump(operators);
		error(0,"\nerror: %s arity %d, %lu operands given\n",getString(op),arity(op),operands.size());
		exit(0);
	}
	operands.push_back((Expr*)p);
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
		dbprintf("warning undeliminated expression parsing anyway");
		flush_op_stack(node,operators,operands);// keep going
	}
	was_operand=true;
}
Type* parse_type(TokenStream& src, int close);

ExprBlock* parse_call(TokenStream&src,int close,int delim, Expr* op) {
	// shunting yard parserfelchery
	ExprBlock *node=new ExprBlock; node->call_operator=op;
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
				error(0,"unexpected %s, expected %s",getString(close),getString(wrong_close));
				exit(0);
			}
		} else { // single expression mode - we dont consume delimiter.
			auto peek=src.peek_tok();
			if (peek==CLOSE_BRACKET || peek==CLOSE_BRACE || peek==COMMA || peek==SEMICOLON)
				break;
		}
		dbprintf(":%s\n",getString(src.peek_tok()));
		dbprintf("parse:- %zu %zu\n",operands.size(),operators.size());
//		printf("operands:");dump(operands);
//		printf("operators:");dump(operators);printf("\n");

		print_tok(src.peek_tok());
		if (src.is_next_literal()) {
			ExprLiteral* ln=0;
			if (src.is_next_number()) {
				auto n=src.eat_number();
				if (n.denom==1) {ln=new ExprLiteral(n.num);}
				else {ln=new ExprLiteral((float)n.num/(float)n.denom);}
			} else if (src.is_next_string()) {
				ln=new ExprLiteral(src.eat_string());
			} else {
				error(0,"error parsing literal\n");
				exit(0);
			}
			operands.push_back(ln);
			was_operand=true;
			continue;
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
			print_tok(tok);dbprintf("<<<<\n");
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
				
				if (tok==COLON){// special case: : invokes parsing type. TODO: we actually want to get rid of this? type could be read from other nodes, parsed same as rest?
					Type *t=parse_type(src,0);
					auto lhs=operands.back();
					lhs->type=t;
					was_operand=true;
				} else {
					operators.push_back(tok);
					was_operand=false;
				}
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
	// todo: pointers, adresses, arrays..
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
	dbprintf("parse_struct");
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
	newline(depth);dbprintf("struct %s",getString(this->name));dump_typeparams(this->typeparams);dbprintf("{");
	
	for (auto f:this->fields){f->dump(depth+1);}
	newline(depth);dbprintf("}");
}
bool ExprStructDef::is_generic()const{
	for (auto f:fields){if (!f->type)return true;}//TODO: is typeparam?
	return false;
}
ResolvedType ExprStructDef::resolve(Scope* scope,const Type* desired){
	scope->add_struct(this);
	if (!this->type) {
		this->type = new Type(this->name);	// name selects this struct.
	}
	return propogate_type_fwd(desired,this->type);
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
		error(0,"c style for loop, expect for init;cond;incr{body}");
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
			error(0,"if { }else {} expected\n");
		}
	}
	return p;
}
Node* ExprIf::clone()const {
	auto p=new ExprIf; p->cond=(Expr*)this->cond->clone_if(); p->body=(Expr*)this->body->clone_if(); p->else_block=(Expr*)this->else_block->clone_if();
	return p;
}
void ExprIf::dump(int depth) const {
	newline(depth);dbprintf("if\n");
	cond->dump(depth+1);
	newline(depth);dbprintf("{\n");
	body->dump(depth+1);
	if (else_block)	{
		indent(depth);dbprintf("} else{\n");
		else_block->dump(depth+1);
	}
	newline(depth);dbprintf("}\n");
};


void ExprFor::dump(int d) const {
	newline(d);printf("for ");
	if (this->is_c_for()) {
		this->init->dump(d+1); newline(d);dbprintf(";");
		this->cond->dump(d+1); newline(d);dbprintf(";");
		this->incr->dump(d+1); newline(d);dbprintf(" {");
	} else {
		this->pattern->dump(d+1);
		newline(d);dbprintf(" in ");
		this->init->dump(d+1); newline(d);dbprintf(" {");
	}
	this->body->dump_if(d+1);
	newline(d);dbprintf("}");
	if (this->else_block){
		newline(d);dbprintf("else{");
		this->else_block->dump_if(d+1);
		newline(d);dbprintf("}");
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
	dbprintf("parse_fn");
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
	dbprintf("%s",getString(fndef->name));
	// read function arguments
	while (NONE!=(tok=src.peek_tok())) {
		if (tok==ELIPSIS){
			fndef->variadic=true; src.eat_tok(); src.expect(CLOSE_PAREN); break;
		}
		if (tok==CLOSE_PAREN) {src.eat_tok();break;}
		dbprintf(" arg:%s ",getString(tok));
		auto arg=parse_arg(src,CLOSE_PAREN);
		fndef->args.push_back(arg);
		src.eat_if(COMMA);
	}
	// TODO: multiple argument blocks for currying?.
	if (src.eat_if(ARROW) || src.eat_if(COLON)) {
		fndef->ret_type = parse_type(src, 0);
	}
	dbprintf("fn body:");
	// implicit "progn" here..
	if (src.eat_if(OPEN_BRACE)){
		fndef->body = new ExprBlock;
		fndef->body = parse_call(src, CLOSE_BRACE, SEMICOLON, nullptr);
	} else {
		fndef->body=nullptr; // Its' just an extern prototype.
	}
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

/*
	"future.pos=self.pos+self.vel*dt;"
	"fn add(a,b){a+b};"
	"fn what(a:int,b:int)->int{x=a+b;other(a,b);x-=b;x+b};"
	"fn other(a:int,b:int)->int{a+b};"
	"fn foo(x:tuple[int,int])->int{_};"
	"fn foo(x:float)->float{_};"
	"fn do_what[X=int](x:X,y:X)->X{_};"
    "struct VecInt{data:*i32,num:i32,cap:i32};"
	"fn render(m:Mesh){}"
	"x=1.0; y=2.0; z=3.0; w=0.5;"
	"foo=lerp(x,add(y,z),w);"
	"struct Mesh[VERTEX,INDEX]{ vertices:VERTEX;triangles:INDEX[3]};"
	"lambda = fn(x){ print(\"foo_bar_baz\");};"
	"i=20;"
	"fn Mesh()->Mesh{_}"
	"mesh = Mesh();"
	"i=10; j=20.0; k=j+i;"
	"fn itof(i:int)->float{_}"
	"fn make()->tuple[int,int]{_}"
	"obj=make();"
	"i=lerp(p,q,r);"
	"render(mesh);"
	"my_str=\"foobar\";"
	"f1=foo(obj);"
	"f2=foo(y);"
 	"if i<10 {printf(1)} else {printf(2)}"
	"for i in foo {print(loop stuff);} else {printf(); madd(1,2,3);}"
	"for x=0; x<10; x++ { printf(x); }"
	"if i<10 {printf(1)} else {printf(2)}"
	"lerp(1,2,0);"

	"fn lerp(a:int,b:int,f:int)->int{(b-a)*f+a};"
	"fn lerp(a,b,f){(b-a)*f+a};"

	"fn printf(a:int,b:int,c:int,d:int){}"
	"fn printf(a:int,b:float,c:int,d:float){}"
	"struct Vec3{x:float,y:float,z:float};"
	"struct Mat3{ax:Vec3,ay:Vec3,az:Vec3};"

	"fn foobar(a:int,b:int)->float{"
"	m:=Mat3;v:=Vec3; vz:=m.ay.z; vw:=m.az.y; vz+=vw; q:=v.x; v.x=q;m.az.x=q;"
	"	printf(1,vz,3,vw);"
	"vz"
	"}"
*/
	"fn lerp(a:float,b:float,f:float)->float{(b-a)*f+a};"
	"fn printf(x:str,...)->int;"
	"fn main(argc:int,argv:ptr[ptr[char]]){"
	"	printf(\"hello world %.3f\", lerp(10.0,20.0,0.5));"
	"}"

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
	dbprintf("%p\n",node);
	node->dump(0);
	Scope global; global.node=(ExprBlock*)node; global.global=&global;
	gather_named_items(node,&global);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->dump(0);
//	global.visit_calls();
	global.dump(0);
	output_code(stdout, &global);
}




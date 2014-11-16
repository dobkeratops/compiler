#include "hack.hpp"
#include "codegen.h"
#include "repl.h"

const char** g_pp,*g_p;
const char* g_filename=0;
/*
 features needed:-

 VECTOR LITERALS
 
 COERSIONS
 
 REAL INLINING
 
 LAZY ARGUMENTS
 
 RAII
 
 FUNCTION TYPE PARAMETER INFERENCE (instead of *just* typeless case)
 
 NESTED ENTITY TYPE INFERENCE
 
 SIMD support - codegen - recognize appropriate structs?
 
 LAMBDAS - its not a modern langauge without them.

 UFCS

 TYPEPARAMETERS - structs, functions, bounding in inference

 HKT - type type params
 INTS IN TYPEPARAMS - buffers like C++

 C++ bindings - emit
 
 C++ bindings - generate
 
 debug information
 
 VARIANTS:
  - adhoc variants
  
 CLASSES
  - vtable generation
  - dynamic cast- as variant?
  -
 
 PATTERN MATCHING?
 
 TUPLES?
 
 DECENT ERROR MESSAGES?
 
 DUMP TYPE INFORMATION
 
 LOCAL FUNCTIONS - NAMED LAMBDAS ? fn foo()

 BREAK EXPRESSIONS
 
 RUST COMPATABILITY
  -parsing rust syntax
  -how far could we go there?
 
 C++ COMPATABILITY

if x in {
	foo=>....
 	bar=>....
	baz=>....
}

 error reporting.
 foo.q // error q not available
 foo.bar()		// bar not available.  available functions:-.....

 */

//(sourcecode "hack.cpp")
//(normalize (lerp (obj:zaxis)(normalize(sub(target(obj:pos)))) //(settings:angle_rate)) (sloc 512 40 20))

void dbprintf(const char* str, ... )
{
	char tmp[1024];
	va_list arglist;
	
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
#ifdef DEBUG
	printf("%s",tmp);
#endif
}
// for real compiler errors. todo.. codelocations on nodes
void error(const Node* n, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	printf("error:-\n");
	n->dump_if(-1);
	printf(";%s\n",buffer);
#ifdef DEBUG
	printf("compiler src: %s:%d: %s\n",__FILE__,__LINE__, __FUNCTION__);
#endif
	ASSERT(0);
}
// for real compiler errors. todo.. codelocations on nodes
void error(const Node* n,Scope* s, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	printf("\n");
	printf("%s:%d: error:",g_filename,n->pos.line);
	n->dump_if(-1);
	printf("%s\n",buffer);
	printf("in scope of %s\n",s->name());
#ifdef DEBUG
	printf("compiler src: %s:%d: %s\n",__FILE__,__LINE__, __FUNCTION__);
#endif
	CRASH(0);
}

void error(const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	printf("\n");
	printf(";%s\n",buffer);
	ASSERT(0);

}

void print_tok(int i){dbprintf("%s ",getString(i));};

bool g_lisp_mode=false;
const char* g_token_str[]={
	"",
	"int","uint","bool","float","char","str","void","auto","one","zero","voidptr","ptr","ref","tuple","__NUMBER__","__TYPE__","__IDNAME__",
	"print___","fn","struct","enum","array","vector","union","variant","with","match","sizeof","typeof","nameof","offsetof",
	"let","set","var",
	"while","if","else","do","for","in","return","break",
	"(",")",
	"{","}",
	"[","]",
	"->",".","=>","<-","::","<->",			//arrows,accessors
	":",
	"+","-","*","/",					//arithmetic
	"&","|","^","%","<<",">>",					//bitwise
	"<",">","<=",">=","==","!=",		//compares
	"&&","||",		//logical
	"=",":=","=:",
	"+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=", // assign-op
	".=",	// linklist follow
	"++","--","++","--", //inc/dec
	"-","*","&","!","~", // unary ops
	"*?","*!","&?","~[]","[]", // special pointers
	",",";",
	"...","..",
	"_",
	NULL,	
};


int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0, 0,0,0,0,
	0,0,0,			// let,set,var
	0,0,0,0,0,0,0,0,  // while,if,else,do,for,in,return,break
	0,0, //( )
	0,0, //{ }
	0,0, // [ ]
	READ|10,READ|2,READ|10,READ|10,READ|13,WRITE|10,	   // arrows
	READ|9,
	READ|6,READ|6,READ|5,READ|5,		//arithmetic
	READ|8,READ|7,READ|8,READ|6,READ|9,READ|9,		//bitwise
	READ|8,READ|8,READ|8,READ|8,READ|9,READ|9,	// COMPARES
	READ|13,READ|14,	//logical
	WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16, // assignment
	
	WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16, // assign-op
	WRITE_LHS|READ|ASSOC|16, // dot-assign
	MODIFY|PREFIX|UNARY|2,MODIFY|PREFIX|UNARY|2,MODIFY|UNARY|ASSOC|3,MODIFY|UNARY|ASSOC|3, // post/pre inc/dec
	READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3, //unary ops
	READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, /// special pointers
	0,0, // delim
	0,
	0,0,
	0, //placeholder
};
bool is_ident(Name tok){return tok>=IDENT;}
bool is_type(Name tok){return tok<T_NUM_TYPES;}
bool is_operator(Name tok){ return tok>=ARROW && tok<COMMA;}
bool is_condition(Name tok){
	return (tok>=LT && tok<=LOG_OR);
}
bool is_comparison(Name tok){
	return (tok>=LT && tok<=NE);
}
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
typedef LLVMOp LLVMOp2[2];
LLVMOp2 g_llvm_ops[]= {
	{{-1,"add","add"},{-1,"fadd","fadd"}},
	{{-1,"sub","sub"},{-1,"fsub","fsub"}},
	{{-1,"mul","mul"},{-1,"fmul","fmul"}},
	{{-1,"div","div"},{-1,"fdiv","fdiv"}},
	{{-1,"and","and"},{-1,"fand","fand"}},
	{{-1,"or","or"},{-1,"fadd",""}},
	{{-1,"xor","xor"},{-1,"fadd",""}},
	{{-1,"srem","rem"},{-1,"fadd",""}},
	{{-1,"shl","shl"},{-1,"fadd",""}},
	{{-1,"ashr","shr"},{-1,"fadd",""}},
};
LLVMOp2 g_llvm_logic_ops[]= {
	{{-1,"and",""},{-1,"fadd",""}},
	{{-1,"or",""},{-1,"fadd",""}},
};
LLVMOp2 g_llvm_cmp_ops[]= {
	{{-1,"icmp slt","icmp ult"},{-1,"fcmp ult","fcmp ult"}},
	{{-1,"icmp sgt","icmp ult"},{-1,"fcmp ugt","fcmp ugt"}},
	{{-1,"icmp sle","icmp ult"},{-1,"fcmp ule","fcmp ule"}},
	{{-1,"icmp sge","icmp ult"},{-1,"fcmp uge","fcmp uge"}},
	{{-1,"icmp eq","icmp eq"},{-1,"fcmp ueq","fcmp ueq"}},
	{{-1,"icmp ne","icmp ne"},{-1,"fcmp une","fcmp une"}},
};
const char* g_llvm_type[]={
	"i32","i32","i1","float","i8","i8*"
};
const char* get_llvm_type_str(int tname){
	switch (tname){
		case INT:return "i32";
		case UINT:return "i32";
		case BOOL:return "i1";
		case FLOAT:return "float";
		case VOID:return "void";
		case STR:return "i8*";
		case CHAR:return "i8";
		default: return getString(tname);
	}
}

const LLVMOp* get_op_llvm(int tok,int type){
	int ti=(type==FLOAT)?1:0;
	if (tok>=ADD && tok<=SHR)
		return &g_llvm_ops[tok-ADD][ti];
	if (tok>=ADD_ASSIGN && tok<=SHR_ASSIGN)
		return&g_llvm_ops[tok-ADD_ASSIGN][ti];
	if (tok>=LOG_AND && tok<=LOG_OR)
		return &g_llvm_logic_ops[tok-LOG_AND][ti];
	if (tok>=LT && tok<=NE)
		return &g_llvm_cmp_ops[tok-LT][ti];
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
int StringTable::get_index(const char* str, const char* end,char flag) {
	if (!end) end=str+strlen(str);
	auto len=(end)?(end-str):strlen(str);
	string s; s.resize(len);memcpy((char*)s.c_str(),str,len);((char*)s.c_str())[len]=0;
	auto ret=names.find(s);
	if (ret!=names.end())	return ret->second;		
	names.insert(std::make_pair(s,nextId));
	index_to_name.resize(nextId+1);
	index_to_name[nextId]=s;
	flags.resize(nextId+1); flags[nextId]=flag;
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
Name getStringIndex(const char* str,const char* end) {
	return g_Names.get_index(str, end,0);
}
const char* getString(Name index) {
	return g_Names.index_to_name[(int)index].c_str();
}
Name getNumberIndex(int num){
	char tmp[32];sprintf(tmp,"%d",num); return g_Names.get_index(tmp,0,StringTable::Number);
}

void indent(int depth) {
	for (int i=0; i<depth; i++){dbprintf("\t");};
}
void newline(int depth) {
	if (depth>=0) dbprintf("\n;"); indent(depth);
}
// Even a block is an evaluatable expression.
// it may contain outer level statements.

Type* Expr::expect_type() const {
	if (this->m_type) return m_type;
	error((const Node*)this,"%s has no type\n", str(name));
	return nullptr;
}
void Expr::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("(?)");
}
void Expr::dump_top() const {
	if (!this) return;
	dbprintf("%s ", getString(name));
}

int get_typeparam_index(const vector<TypeParam>& tps, Name name) {
	for (int i=tps.size()-1; i>=0; i--) {
		if (tps[i].name==name)
			return i;
	}
	return -1;
}

Expr::Expr(){ m_type=0;}
ResolvedType assert_types_eq(const Node* n, const Type* a,const Type* b) {
	ASSERT(a && b);
	if (!a->eq(b)){
		dbprintf("a=\n");
		a->dump(-1);
		dbprintf("b=\n");
		b->dump(-1);
		error(n,"\ntype error: %p vs %p %s %s",a,b, str(a->name),str(b->name));
		return ResolvedType(a,ResolvedType::ERROR);
	}
	return ResolvedType(a,ResolvedType::COMPLETE);
}
RegisterName Node::get_reg_existing(){ASSERT(regname); return regname;}
RegisterName Node::get_reg(Name baseName, int *new_index, bool force_new){
	// variable might be in a local scope shadowing others, so it still needs a unique name
	// names are also modified by writes, for llvm SSA
	if (!regname || force_new){
		auto old=regname;
		auto ret= get_reg_new(baseName,new_index);
		return ret;
	} else{
		return regname;
	}
}
RegisterName Node::get_reg_new(Name baseName, int* new_index) {
	char name[256];
	const char* s=getString(baseName);
	sprintf(name, "r%d%s",(*new_index)++,isSymbolStart(s[0])?s:"rfv");
	return this->regname=g_Names.get_index(name,0,0);
}
void verify(const Type* a){
	if (a){
		ASSERT(a->name>=0 && a->name<g_Names.nextId);
		verify(a->sub);
		verify(a->next);
	}
}
void verify(const Type* a,const Type* b){
	verify(a);
	verify(b);
}
void verify(const Type* a,const Type* b,const Type* c){
	verify(a);
	verify(b);
	verify(c);
}
const Type* any_not_zero(const Type* a, const Type* b){return a?a:b;}
ResolvedType propogate_type(const Node*n, Type*& a,Type*& b) {
	verify(a,b);
	if (!(a || b)) return ResolvedType(0,ResolvedType::INCOMPLETE);
	if (!a && b) {a=b; return ResolvedType(a,ResolvedType::COMPLETE);}
	else if (!b && a) {b=a; return ResolvedType(b,ResolvedType::COMPLETE);}
	else {
		return assert_types_eq(n, a,b);
	}
}
ResolvedType propogate_type_fwd(const Node* n, const Type*& a,Type*& b) {
	verify(a,b);
	if (!(a || b)) return ResolvedType(0,ResolvedType::INCOMPLETE);
	if (!a && b){return ResolvedType(b,ResolvedType::INCOMPLETE);}
	if (!b && a) {b=(Type*)a;return ResolvedType(a,ResolvedType::COMPLETE);}
	if (a && b){return assert_types_eq(n, a,b);  }
	ASSERT(0);
}
ResolvedType propogate_type(const Node* n, Type*& a,Type*& b,Type*& c) {
	verify(a,b,c);
	int ret=ResolvedType::COMPLETE;
	ret|=propogate_type(n,a,b).status;
	ret|=(c)?propogate_type(n,b,c).status:ResolvedType::INCOMPLETE;
	ret|=(c)?propogate_type(n,a,c).status:ResolvedType::INCOMPLETE;
	const Type* any=any_not_zero(a,any_not_zero(b,c));
	return ResolvedType(any,ret);
}
ResolvedType propogate_type_fwd(const Node* n,const Type*& a,Type*& b,Type*& c) {
	verify(a,b,c);
	int ret=ResolvedType::COMPLETE;
	ret|=propogate_type_fwd(n,a,b).status;
	ret|=propogate_type_fwd(n,a,c).status;
	ret|=propogate_type(n,b,c).status;
	return ResolvedType(any_not_zero(a,any_not_zero(b,c)),ret);
}
ResolvedType propogate_type(const Node* n, ResolvedType& a,Type*& b) {
	verify(a.type,b);
	a.combine(propogate_type(n, a.type,b));
	return a;
}
ResolvedType propogate_type(const Node* n,ResolvedType& a,Type*& b,const Type* c) {
	verify(a.type,b,c);
	a.combine(propogate_type_fwd(n, c,b));
	a.combine(propogate_type(n, a.type,b));
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
void ExprOp::find_vars_written(Scope* s, set<Variable *> &vs) const{
	auto flags=operator_flags(name);
	if (lhs) lhs->find_vars_written(s,vs);
	if (rhs) rhs->find_vars_written(s,vs);
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

void ExprBlock::find_vars_written(Scope* s, set<Variable*>& vars) const
{
	if (this->call_expr){
		this->call_expr->find_vars_written(s, vars);
	}
	for (auto a:argls)
		a->find_vars_written(s,vars);
}
void ExprIf::find_vars_written(Scope* s, set<Variable*>& vars) const{
	cond->find_vars_written(s,vars);
	if (body)body->find_vars_written(s,vars);
	if (else_block)else_block->find_vars_written(s,vars);
}
void ExprFor::find_vars_written(Scope* s, set<Variable*>& vars) const{
	if (incr)incr->find_vars_written(s,vars);
	if (cond)cond->find_vars_written(s,vars);
	if (body)body->find_vars_written(s,vars);
	if (else_block)else_block->find_vars_written(s,vars);
}
void Name::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &given_types)
{
	auto index=get_typeparam_index(typeparams, *this);
	if (index>=0){
		ASSERT(given_types[index]->sub==0 && "TODO type-expressions for name? concat[],...");
		*this=given_types[index]->name;
	}
}
void ExprIdent::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &given_types)
{
	// templated idents?
	// TODO - these could be expresions - "concat[T,X]"
	this->name.translate_typeparams(typeparams,given_types);

}
ResolvedType ExprIdent::resolve(Scope* scope,const Type* desired) {
	// todo: not if its' a typename,argname?
	if (this->is_placeholder()) {
		//PLACEHOLDER type can be anything asked by environment, but can't be compiled .
		propogate_type_fwd(this, desired,this->type_ref());
		return ResolvedType(this->type_ref(),ResolvedType::COMPLETE);
	}
	propogate_type_fwd(this, desired,this->type_ref());
	if (auto sd=scope->find_struct_named(this->name)) {
		if (!this->get_type()){
			this->set_type(new Type(sd));
			return propogate_type_fwd(this, desired,this->type_ref());
		}
	}
	if (auto v=scope->find_variable_rec(this->name)){
		return propogate_type(this, this->type_ref(),v->type_ref());
	} else if (!scope->find_named_items_rec(this->name)){
		error(this,scope,"can't find variable/item %s",str(this->name));
	}
	return ResolvedType();
}
void ExprIdent::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("%s:",getString(name));
	if (this->get_type()) {this->get_type()->dump(-1);}
}

Name ExprBlock::get_fn_name()const
{	// distinguishes operator &  function call
	if (call_expr){
		ASSERT(dynamic_cast<ExprIdent*>(call_expr)&& "TODO: distinguish expression with computed function name");
		return call_expr->name;
	}
	else if (get_fn_call()){return get_fn_call()->name;}
	else return 0;
}


void ExprBlock::dump(int depth) const {
	if (!this) return;
	newline(depth);
	if (this->call_expr){
		dbprintf(this->square_bracket?"subscript: ":"call: ");
		this->get_type()->dump_if(-1);
		dbprintf(" (");
//		dbprintf("%s",getString(get_fn_call()->name));
		this->call_expr->dump(depth+1);
		if (this->get_type()) {dbprintf(":");this->get_type()->dump(-1);};}
	for (const auto x:this->argls) {
		if (x) {x->dump(depth+1);}else{dbprintf("(none)");}
	}
	newline(depth);if (this->call_expr)dbprintf(")");
}
void ExprBlock::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &types){
	this->call_expr->translate_typeparams(typeparams,types);
	for (auto e:argls){
		e->translate_typeparams(typeparams,types);
	}
}

void ExprOp::dump(int depth) const {
	if (!this) return;
	newline(depth);
	int id=this->name;
	if (is_prefix(id))dbprintf("prefix");
	else if (lhs && rhs)dbprintf("infix");
	else dbprintf("postfix");print_tok(id);

	if (get_type()) {dbprintf(":");get_type()->dump(-1);};dbprintf("(");
	if (lhs) {lhs->dump(depth+1);}else{dbprintf("(none)");}
	if (rhs) {rhs->dump(depth+1);}else{dbprintf("(none)");}
	newline(depth);dbprintf(")");
}
void ExprOp::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &types){
	if (lhs) lhs->translate_typeparams(typeparams,types);
	if (rhs) rhs->translate_typeparams(typeparams,types);
}

ExprBlock::ExprBlock(){call_target=0;}

const char* Scope::name() const {
	if (owner) return str(owner->name);
	if (!parent){
		return"<global>";
	}  else return "<anon>";
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
Type::Type(Name i,SrcPos sp){
	pos=sp;
	marker=1234;
	struct_def=0;
	sub=0;
	next=0;
	name=i; //todo: resolve-type should happen here.
}
Type::Type(Name i){
	marker=1234;
	struct_def=0;
	sub=0;
	next=0;
	name=i; //todo: resolve-type should happen here.
}

	//todo: generic heirarchy equality test, duplicate code detection?
bool type_compare(const Type* t,int a0, int a1){
	if (t)
		if (t->name==a0)
			if (t->sub)
				if (t->sub->name==a1)
					return true;
	return false;
}

bool Type::eq(const Type* other) const{
	if ((!this) && (!other)) return true;
	if (!(this && other)) return false;
	if (this->name!=other->name)return false;
//	if (!this->sub && other->sub)) return true;
	if (other->name==STR && type_compare(this,PTR,CHAR)) return true;
	if (this->name==STR && type_compare(other,PTR,CHAR)) return true;
	
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
bool Type::is_complex()const{
	if (sub) return true;	// todo: we assume anything with typeparams is a struct, it might just be calculation
	for (auto a=sub; a;a=a->next)if (a->is_complex()) return true;
	if (this->is_struct()||this->name==ARRAY||this->name==VARIANT) return true;
	return false;
}
bool Type::is_struct()const{
	return struct_def!=0 || name>=IDENT; //TODO .. it might be a typedef.
}
int Type::num_pointers() const {
	if (!this) return 0;
	if (this->name==PTR || this->name==REF)
		return 1+this->sub->is_pointer();
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
{	struct_def=sd; name=sd->name;sub=0;next=0;
	marker=123456;
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
	if (!this->owner_scope){
		this->next_of_scope=sc->global->literals;
		sc->global->literals=this;
		this->owner_scope=sc->global;
	}
	if (!this->name){
		char str[256]; if (!this->name){sprintf(str,"str%x",(uint)(size_t)this); this->name=getStringIndex(str);}
	}
	if (!this->get_type()) {
		Type* t=nullptr;
		switch (type_id) {
		case T_VOID: t=new Type(VOID); break;
		case T_INT: t=new Type(INT); break;
		case T_FLOAT: t=new Type(FLOAT); break;
		case T_CONST_STRING: t=new Type(STR); break;
		default: break;
		}
		this->set_type(t); // one time resolve event.
	}
	return propogate_type_fwd(this, desired,this->type_ref());
}
size_t ExprLiteral::strlen() const{
	if (type_id==T_CONST_STRING)
		return ::strlen(this->u.val_str);
	else return 0;
}

ExprLiteral::ExprLiteral(float f) {
	this->owner_scope=0;
	set_type(new Type(FLOAT));
	type_id=T_FLOAT;
	u.val_float=f;
	dbprintf("lit float %.3f",f);
}
ExprLiteral::ExprLiteral(int i) {
	this->owner_scope=0;
	set_type(new Type(INT));
	type_id=T_INT;
	u.val_int=i;
}
ExprLiteral::ExprLiteral(const char* start,int length) {//copy
	this->owner_scope=0;
	set_type(new Type(STR));
	type_id=T_CONST_STRING;
	auto str=( char*)malloc(length+1); ;
	u.val_str=str;memcpy(str,(void*)start,length);
	str[length]=0;
}
ExprLiteral::ExprLiteral(const char* src) {//take ownership
	this->owner_scope=0;
	set_type(new Type(STR));
	u.val_str=src;
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
		print_tok(t.name);if (t.defaultv){dbprintf("=");t.defaultv->dump(-1);}
		a=true;
	}
	dbprintf("]");
}
void Variable::dump(int depth) const{
	newline(depth);dbprintf("%s",getString(name));
	if (type()) {dbprintf(":");type()->dump(-1);}
	switch (this->kind){
		case VkArg:dbprintf("(Arg)");break;
		case Local:dbprintf("(Local)");break;
		case Global:dbprintf("(Local)");break;
		default: break;
	}
}

void ArgDef::dump(int depth) const {
	newline(depth);dbprintf("%s",getString(name));
	if (type) {dbprintf(":");type->dump(-1);}
	if (default_expr) {dbprintf("=");default_expr->dump(-1);}
}
void TypeParam::dump(int depth) const {
	newline(depth);dbprintf("%s",str(name));
	if (defaultv) {dbprintf("=");defaultv->dump(-1);}
}
const char* ArgDef::kind_str()const{return"arg_def";}

// the operators should all just be functioncalls, really.
// return type of function definition is of course a functoin object.
// if we make these things inline, we create Lambdas
// todo: receivers.

bool ExprFnDef::is_generic() const {
	if(instances!=nullptr)
		return true;
	if (typeparams.size())
		return true;
	for (auto i=0; i<args.size(); i++)
		if (!args[i]->type || args[i]->type->name==AUTO)
			return true;
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
	dbprintf(" {");
	if (this->body) {
		this->body->dump(ind);
	}
	newline(ind);dbprintf("}\n");
	if (auto p=this->instances){
		dbprintf(";//instantiations:");
		for (;p;p=p->next_instance){
			p->dump(ind);
		}
	}
}
bool typeparams_all_set(const vector<TypeParam> &tformat, const vector<Type *> &given_types) {
	for (int i=0; i<given_types.size(); i++) {
		if (given_types[i]==0) return false;
	}
	return true;
}
void ExprFnDef::translate_typeparams(const vector<TypeParam> &tformat, const vector<Type *> &given_types){
	for (auto &a:args) a->translate_typeparams(tformat,given_types);
	
	if (typeparams_all_set(typeparams,given_types))
	{
		this->typeparams.resize(0);
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
void ExprFnDef::dump_signature()const{
	dbprintf("fn %s(",str(name));
	for (auto a:args) a->dump(-1);
	dbprintf(")->");
	this->return_type()->dump_if(-1);
	dbprintf("\n");
}
ExprFnDef* instantiate_generic_function(ExprFnDef* src,Name name, vector<Expr*>& call_args) {
	if (src->type_parameter_index(src->name)>=0){
		dbprintf("WARNING instantiated templated NAME function for %s, as no function of the right name was found.. experiment aimed at implementing OOP thru generics.. eg fn METHOD[OBJ,METHOD,ARG0,ARG1](o:OBJ,a0:ARG0,a1:ARG1){ o.vtable.METHOD(o,a0,a1)}", str(name));
	}
	Scope* src_fn_owner=src->scope->parent_or_global();
	ExprFnDef* new_fn =(ExprFnDef*) src->clone();
	// fill any args we can from the callsite.
	// TODO: translate generic-type-params
	// because we may infer return from that
	for (auto i=0; i<new_fn->args.size(); i++){
		if (//!new_fn->args[i]->type &&
			call_args[i]->get_type()) {
				new_fn->args[i]->set_type((Type*)call_args[i]->get_type()->clone());
		}
	}
	src->dump_signature();
	new_fn->dump_signature();
	new_fn->dump(0);
	// todo: translate return type. for the minute we discard it..
	new_fn->ret_type=nullptr;
	new_fn->body->set_type(nullptr);// todo, inference upward..
	new_fn->next_instance = src->instances;
	src->instances=new_fn;
	new_fn->instance_of = src;
	new_fn->resolved=false;
//	dbprintf("generic instantiation:-\n");
//	new_fn->dump(0);
//	new_fn->scope->dump(0);
	new_fn->resolve(src_fn_owner,nullptr);//todo: we can use output type ininstantiation too
	dbprintf("instantiated: %s \n",str(new_fn->name));
	new_fn->dump_signature();
	dbprintf("%s\n", str(new_fn->ret_type->name));
//	new_fn->dump(0);
	return new_fn;	// welcome new function!
}
Node* ExprOp::clone() const {
	return (Node*) new ExprOp(this->name,this->pos, (Expr*) this->lhs->clone_if(), (Expr*) this->rhs->clone_if());
}
Node* ExprBlock::clone() const {
	if (!this) return nullptr;
	auto r=new ExprBlock();
	if (this->call_expr) {
		r->call_expr = (Expr*) this->call_expr->clone();
	}
	r->set_type((Type*)this->get_type()->clone_if());
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
	return (Node*)this;	// TODO - ensure this doesn't get into dangling state or anything!
	if (!this) return nullptr;
	auto r=new ExprLiteral(0); if (this->is_string()){r->u.val_str=strdup(this->u.val_str);}else r->u=this->u;
	r->type_id=this->type_id;
	r->llvm_strlen=this->llvm_strlen; // TODO this should just be a reference!?
	r->name=this->name;
	return r;
}
Node*
ExprFnDef::clone() const{
	if (!this) return nullptr;
	auto r=new ExprFnDef(this->pos);
	r->name=this->name;
	r->body=(ExprBlock*)(this->body?this->body->clone():nullptr);
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
	auto r=new ExprIdent(this->name,this->pos);
	return r;
}

//void find_printf(const char*,...){};
#define find_printf dbprintf
int num_known_arg_types(vector<Expr*>& args) {
	int n=0; for (auto i=0; i<args.size(); i++) {if (args[i]->get_type()) n++;} return n;
}

//void match_generic_type_param_sub(const vector<TypeParam>& tps, vector<Type*>& mtps, const Type* to_match, const Type* given) {
	
//}

int match_generic_type_params(const vector<TypeParam>& fn_tps, vector<const Type*>& matched_tps, const Type* fn_arg, const Type* given_arg)
{
	int ret_score=0;
	if (!fn_arg && !given_arg) return 0;
	if (!fn_arg) return 0;// dont care if trying to match given with 'any'
	for (const Type* sub1=fn_arg->sub,*sub2=given_arg->sub; sub1&&sub2; sub1=sub1->next,sub2=sub2->next) {
		ret_score+=match_generic_type_params(fn_tps, matched_tps, sub1, sub2);
	}
	dbprintf("TODO: matching arguments should actually traverse the type tree , they can be complex thats' the point, eg [T](args:vector<T>,T) that sort of thing.\n");
	int ti = get_typeparam_index(fn_tps, fn_arg->name);
	if (ti>=0) {
		// Is this a generic typeparam?
		if (matched_tps[ti]==0){ // new typeparam?
			dbprintf("typeparam %s is %s\n", str(fn_tps[ti].name), str(given_arg->name));
			matched_tps[ti]=given_arg;
			return ret_score+1;
		}
		else if (!(matched_tps[ti]->eq(given_arg))) {// or we already found it - match..
			dbprintf("typeparam %s is %s, !=given %s\n", str(fn_tps[ti].name), str(matched_tps[ti]->name), str(given_arg->name));
			return ret_score-1000;
		}
	} else {
		// concrete types - compare absolutely
		if (fn_arg->name != given_arg->name)
			return ret_score-1000;	// mismatch is instant fail for this candidate.
	}
	return ret_score;
}


void compare_candidate_function(ExprFnDef* f,Name name,vector<Expr*>& args,const Type* ret_type, ExprFnDef** best_fn, int* best_score,int* ambiguity) {
	if (f->type_parameter_index(f->name)<0)
		if (f->name!=name)
			return ;
	
	// TODO: may need to continually update the function match, eg during resolving, as more types are found, more specific peices may appear?
	// Find max number of matching arguments
	if (args.size() >f->args.size() && !f->variadic)	// if not enough args, dont even try.
		return;
	if (args.size() <f->args.size())	// TODO: bring default arguments into the picture.
		return;

	find_printf("candidate:");
	
	vector<const Type*> matched_type_params;
	for (int i=0; i<f->typeparams.size(); i++){matched_type_params.push_back(nullptr);}

	for (int i=0; i<args.size() && i<f->args.size(); i++) {
//		find_printf("%s ",f->args[i]->type->get_name_str());
		find_printf("arg %d %s",i,getString(f->args[i]->name));
		auto t=f->args[i]->type;
		if (t) t->dump(-1);
		find_printf("\n");
	}

	find_printf("\n");
	int score=0;
	if (f->variadic && args.size()> f->args.size())
		score=1;	// variadic functoin can match anything?
	for (int i=0; i<args.size() && i<f->args.size(); i++) {
		
		
		if (f->args[i]->get_type()->eq(args[i]->get_type())) {
			find_printf("match %s %s\n",f->args[i]->get_type()->get_name_str(), args[i]->get_type()->get_name_str());
				score++;
		}
	}
	// find generic typeparams..
	if (f->typeparams.size()){
		for (int i=0; i<args.size() && i<f->args.size(); i++) {
				score+=match_generic_type_params(f->typeparams, matched_type_params, f->args[i]->get_type(), args[i]->get_type());
		}
#ifdef DEBUG
		dbprintf(" %s score=%d; matched typeparams{:-\n",str(f->name),score);
		for (auto i=0; i<f->typeparams.size(); i++){
			dbprintf("%s = %s;", str(f->typeparams[i].name), str(matched_type_params[i]->name) );
		}
		dbprintf("}\n");
		dbprintf("\n");
#endif
	}
	// fill any unmatched with defaults?

	// consider return type in call.
	if (ret_type) if (f->get_type()->eq(ret_type)) score++;
		
	// for any given argument not matched, zero the score if its the *wrong* argument?
	// TODO: this is where we'd bring conversion operators into play.
	if (!f->typeparams.size()) // TODO; we need to do this with translated types. for now, we skip this if the signature has typeparams.
	for (int i=0; i<args.size() && i<f->args.size(); i++) {
		if (f->args[i]->get_type() && args[i]->get_type())
			if ((!f->args[i]->get_type()->eq(args[i]->get_type())) && f->args[i]->get_type()!=0) score=-1;
	}
	if (f->name==name && score) score*=100; // 'named' functions always win over un-named forms eg F[F,X](a:X)
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
		for (auto ins=f->instances; ins; ins=ins->next_instance) {
			compare_candidate_function(ins,name,args,ret_type, best_fn,best_score,ambiguity);
		}
	}
}
void find_fn_rec(Scope* s,Scope* ex,Name name,vector<Expr*>& args, const Type* ret_type,ExprFnDef** best_fn, int* best_score,int* ambiguity)
{
	if (auto fname=s->find_named_items_local(name)){
		for (auto f=fname->fn_defs; f;f=f->next_of_name) {
			find_fn_sub((Expr*)f, name, args,ret_type,best_fn,best_score,ambiguity);
		}
	}
	for (auto f=s->templated_name_fns; f;f=f->next_of_name) {
		find_fn_sub((Expr*)f, name, args,ret_type,best_fn,best_score,ambiguity);
	}
	for (auto sub=s->child; sub; sub=sub->next) {
		if (sub==ex) continue;
		find_fn_rec(sub,ex,name,args,ret_type,best_fn,best_score,ambiguity);
	}
}

ExprFnDef*	Scope::find_fn(Name name, vector<Expr*>& args,const Type* ret_type)  {
	find_printf("\n;find call with args(");
	for (int i=0; i<args.size(); i++) {find_printf(" %d:",i);find_printf("%p\n",args[i]);if (args[i]->get_type()) args[i]->get_type()->dump(-1);}
	find_printf(")\n");

	ExprFnDef*	best=0;
	int best_score=-1;
	int	ambiguity=0;
	Scope* prev=nullptr;
	for (auto src=this; src; prev=src,src=src->parent) {// go back thru scopes first;
		find_printf(";in scope %p\n",src);
		find_fn_rec(src,prev,name, args,ret_type,&best,&best_score,&ambiguity);
	}
	// then search subscopes, excluding 'this'
	// TODO: consider "name-distance" as part of resolve algorithm??
	// only search subscopes if nothing was found here.
/*	if (!best) {
		for (auto src=this->child; src; src=src->next) {
			if (auto fname=src->find_named_items_local(name)){
				for (auto f=fname->fn_defs; f;f=f->next_of_name) {
					find_fn_sub(f,name, args,ret_type,&best,&best_score,&ambiguity);
				}
			}
		}
	}
 */
	
//	if (!best) {
//		printf("\ncant find any function  %s\n",getString(name));
//		this->global->dump(0);
//		printf("\ncant find any function  %s\n",getString(name));
//		exit(0);
//		return 0;f
//	}
	dbprintf(";match %s score=%d/%z\n", best?str(best->name):"" ,best_score, args.size());
	if (!best)  {
		for (auto i=0; i<args.size(); i++){
			dbprintf("%d :",i); args[i]->type()->dump(-1); dbprintf("\n");
		}
		dbprintf(";No match found for %s\n",str(name));
		return nullptr;
	}
	if (ambiguity){
		error(this->owner,";ambiguous matches for %s",getString(name));
	}
	if (best->is_generic()) {
		dbprintf(";matched generic function %s instanting\n",str(best->name));
		return instantiate_generic_function(best, name, args);
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
ExprStructDef* Scope::find_struct_sub(Scope* original,Type* t){
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
	if (auto p=parent_or_global()) return p->find_struct_named(name);
	else return nullptr;
}

void Scope::add_fn(ExprFnDef* fnd){
	if (fnd->instance_of!=0) return; // we compile/match it by instance search.
	dbprintf("add fn %s to %s\n", str(fnd->name), this->name());
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
/*
Variable* Scope::get_or_create_variable(Name name,VarKind k){
	if (auto v=this->find_variable_rec(name)) {
		return v;
	}
	return this->create_variable(name,k);
}
 */
Variable* Scope::create_variable(Name name,VarKind k){
	auto exv=this->find_scope_variable(name);
	if (exv) return exv;
	ASSERT(exv==0);
	dbprintf("create variable %s\n",str(name));
	auto v=new Variable(name,k); v->next=this->vars; this->vars=v;
	v->name=name;v->owner=this;
	return v;
}
Variable* Scope::get_or_create_scope_variable(Name name,VarKind k){
	auto exv=this->find_scope_variable(name);
	auto shadow_v=find_variable_rec(name);
	if (exv) return exv;
	if (shadow_v){
		dbprintf("warning shadowing variable %s in %s\n",str(name),this->name());
	}
	auto v=this->create_variable(name,k);
	return v;
}
void Scope::dump(int depth)const {
	newline(depth);dbprintf("scope: %s {", this->owner?getString(this->owner->ident()):"<global>");
	for (auto v=this->vars; v; v=v->next) {
		newline(depth+1); dbprintf("var %d %s:",(int)v->name, getString(v->name));
		if (auto t=v->get_type()) t->dump(-1);
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

// the fact is, i DO like C++.
// I just dont like header files.
template<typename T>
T* expect_cast(Node* n){
	auto r=dynamic_cast<T*>(n);
	if (!r) {
		T t;
		error(n, "expected %s to be %s not %s", str(n->name),t.kind_str(), n->kind_str());
		n->dump(-1);
	}
	return r;
}

ResolvedType ExprOp::resolve(Scope* sc, const Type* desired) {
	Type* ret=0;
	auto op_ident=name;
	if (op_ident==ASSIGN || op_ident==LET_ASSIGN || op_ident==ASSIGN_COLON) {
		ASSERT(this->lhs && this->rhs);
		auto vname=lhs->ident();	//todo: rvalue malarchy.
		//			printf("set: try to create %d %s %s\n", vname, getString(vname), this->args[1]->kind_str());
		auto rhs_t=rhs->resolve(sc,desired);
		lhs->dump(0);
		if (op_ident==LET_ASSIGN){
			if (desired) {
				desired->dump(-1);
			}
			auto rhs_t = rhs->get_type();
			auto new_var=sc->create_variable(vname,Local);
			new_var->set_type(rhs_t);
			lhs->set_type(rhs_t);
			this->set_type(rhs_t);
			propogate_type_fwd(this, desired, lhs->type_ref());
			return 	propogate_type_fwd(this, desired, this->type_ref());
		}
		else if (op_ident==ASSIGN_COLON){ // create a var, of given type.
			// todo: get this in the main parser
			auto lhsi=expect_cast<ExprIdent>(lhs);
			auto rhst=expect_cast<Type>(rhs);
			//			auto v=sc->find_variable_rec(this->argls[0]->name);
			auto v=sc->get_or_create_scope_variable(lhsi->name,Local);
			v->set_type(rhst);
			if (rhst->name>=IDENT && !rhst->sub) {
				dbprintf(";struct type? %s\n", str(rhst->name));
				rhst->struct_def = sc->find_struct(rhst);
				dbprintf("%p\n", rhst->struct_def);
			}
			if (v->get_type()) {
				dbprintf("instantiating a %s\n", str(v->get_type()->name));
				sc->find_struct(v->get_type());// instantiate
			}
			return propogate_type(this, v->type_ref(),type_ref());
		}
		else if (op_ident==ASSIGN){
			propogate_type_fwd(this, desired, type_ref());
			auto rhs_t=rhs->resolve(sc,desired);
			auto lhs_t=lhs->resolve(sc,desired);		// might assign to struct-field, ...
//			rhs->dump(0);rhs->type()->dump_if(-1);newline(0);
//			lhs->dump(0);lhs->type()->dump_if(-1);newline(0);
//			desired->dump_if(-1);newline(0);
			propogate_type(this, rhs->type_ref(), lhs->type_ref());
			propogate_type(this, type_ref(),rhs->type_ref());
//			lhs->type()->dump_if(-1);newline(0);
			return propogate_type(this, type_ref(),lhs->type_ref());
		} else{
			ASSERT(0);
			return ResolvedType();
		}
	}
	else if (op_ident==COLON){ // TYPE ASSERTION,
		// todo: get this in the main parser
		ASSERT(dynamic_cast<ExprIdent*>(rhs));
		int tname=rhs->name;
		auto v=sc->find_variable_rec(lhs->name);
		if (!v->get_type()){
			v->set_type((const Type*)rhs);
			ASSERT(dynamic_cast<Type*>(rhs))
		}
		return propogate_type(this, v->type_ref(),type_ref());
	}
	else if (op_ident==DOT || op_ident==ARROW) {
		auto lhs_t=lhs->resolve(sc, 0);//type doesn't push up- the only info we have is what field it needs
		auto t=lhs_t.type;
//		dbprintf("resolve %s.%s   lhs:",getString(lhs->name),getString(rhs->name));if (t) t->dump(-1);dbprintf("\n");
	
		// TODO: assert that lhs is a pointer or struct? we could be really subtle here..
		if (t) {
			t=t->deref_all();
			// now we have the elem..
			ASSERT(dynamic_cast<ExprIdent*>(rhs));
			if (auto st=sc->find_struct(t)){
				if (auto f=st->find_field(rhs->name)){
					f->dump(-1);
					ret=f->type;
					return propogate_type(this, ret,this->type_ref());
				} else{
					st->dump(-1);
					error(this,"\nfield not found %s in %s\n",str(rhs->name), str(st->name));
					ASSERT(0 && "field not found");
				}
			}else {
				error(this,"struct not found %s", str(t->name));
			}
		}
		return ResolvedType(this->type(),ResolvedType::INCOMPLETE);
	}
	else if (op_ident==ADDR){  //result=&lhs
		// todo: we can assert give type is one less pointer, if given
		ASSERT(!lhs && rhs);
		Type* dt=nullptr;
		if (desired){
			if (desired->name!=PTR || desired->name!=REF) {
				dbprintf("taking adr,want:\n");
				desired->dump(-1);
				newline(0);
				ASSERT(0&&"incorrect type for grabbing adress");
			}
			dt=desired->sub;
		}
		auto ret=rhs->resolve(sc,dt);
		if (!this->get_type() && ret.type){
			dbprintf("resolve pointer type\n");
			ret.type->dump(-1); newline(0);
			auto ptr_type=new Type(PTR); ptr_type->sub=(Type*)ret.type->clone();
			this->set_type(ptr_type);
			ptr_type->dump(-1); newline(0);
			return propogate_type_fwd(this, desired,ptr_type);
		}
		return ret;
	}
	
	else if (op_ident==DEREF){ //result=*rhs
		// todo: we can assert give type is one less pointer, if given
		auto ret=rhs->resolve(sc,0);
		// todo: its' a typeparam constraint.  ptr[desired]==argls[0]
		if (!this->get_type() && ret.type){
			if (ret.type->name!=PTR) {
				this->dump(0);
				rhs->dump(0);
				ret.type->dump(0);
			}
			ASSERT(ret.type->name==PTR);
			this->set_type(ret.type->sub);
			return propogate_type_fwd(this, desired, this->type_ref());
		}
		else return ResolvedType();
	}
	else if (is_condition(op_ident)){
		auto lhst=lhs->resolve(sc,rhs->type_ref()); // comparisions take the same type on lhs/rhs
		auto rhst=rhs->resolve(sc,lhs->type_ref());
		verify(lhs->get_type());
		verify(rhs->get_type());
		if (!this->get_type()){
			this->set_type(new Type(BOOL));
		};
		return rhst;
	}
	else {
		// regular operator
		// TODO propogate types for pointer-arithmetic - ptr+int->ptr   int+ptr->ptr  ptr-ptr->int
		// defaults to same types all round.
		auto lhst=lhs->resolve(sc,desired);
		auto rhst=lhs->resolve(sc,desired);
		propogate_type(this, lhst,type_ref());
		propogate_type(this, rhst,type_ref());
		return propogate_type_fwd(this, desired, type_ref());
	}
}

ResolvedType ExprBlock::resolve(Scope* sc, const Type* desired) {
	verify(this->get_type());
	if (this->argls.size()<=0 && this->is_compound_expression() ) {
		if (!this->get_type()) this->set_type(new Type(VOID));
		return propogate_type_fwd(this, desired,this->type_ref());
	}
	ExprIdent* p=nullptr;
	if (this->is_compound_expression()) {	// do executes each expr, returns last ..
		auto n=0;
		for (; n<this->argls.size()-1; n++) {
			this->argls[n]->resolve(sc,0);
		}
		// last expression - type bounce
		if (this->argls.size()) {
			propogate_type_fwd(this, desired,this->type_ref());
			auto ret=this->argls[n]->resolve(sc,desired);
			return propogate_type(this, ret,this->type_ref());
		}
		else {ASSERT(0);return ResolvedType();}
	}
	else if (this->square_bracket && this->call_expr) {
		// array indexing operator TODO: check this isn't itself a Type, if we want templates anywhere.
		auto array_type=this->call_expr->resolve(sc,nullptr); // todo - it could be _[desired]. forward should give possibilities
		if (array_type.type){
			ASSERT(array_type.type->is_array()||array_type.type->is_pointer());
			for (auto i=0; i<argls.size(); i++)  {
				argls[i]->resolve(sc,nullptr ); // TODO any indexing type? any type extracted from 'array' ?
			}
			const Type* array_elem_type=array_type.type->sub;
			propogate_type_fwd(this, array_elem_type,this->type_ref());
			return propogate_type_fwd(this, desired,this->type_ref());
		} else return ResolvedType();
	}
	else if (this->call_expr){
		dbprintf(";resolve call..%s->%s\n",sc->name(),str(p->ident()));
		// TODO: distinguish 'partially resolved' from fully-resolved.
		// at the moment we only pick an fn when we know all our types.
		// But, some functions may be pure generic? -these are ok to match to nothing.
		// todo:
//		auto n=num_known_arg_types(this->argls);
		
		auto fn_type_r=this->call_expr->resolve(sc,nullptr);
		auto fn_type=fn_type_r.type;
		int arg_index=0;
		if (fn_type) {
			// propogate types we have into argument expressions
			for (auto a=fn_type->fn_args(); arg_index<argls.size() && a; arg_index++,a=a->next)  {
				argls[arg_index]->resolve(sc,a);
			}
			for (;arg_index<argls.size(); arg_index++){ // variadic args.
				argls[arg_index]->resolve(sc,nullptr);
			}
			const Type* fr=fn_type->fn_return();
			return propogate_type_fwd(this, fr, this->type_ref());
		} else
		for (auto i=0; i<argls.size(); i++)  {
			argls[i]->resolve(sc,nullptr );
		}

		if (!this->call_target){
			
			static bool warn=false; if (!warn){warn=true;dbprintf("TODO try re-testing every time, or waiting for max args to be ready? one way might be: no unknown args, between caller & candidate. ( eg caller (f f _) vs callee (_ f f) would infer the last ones ok\n  dont just wait for all on caller.");}
			for (auto i=0; i<argls.size(); i++)  {
				argls[i]->resolve(sc,nullptr );
			}

			return resolve_make_fn_call(this, sc,desired);
		} else if (auto fnc=this->call_target){ // static call
			for (auto i=0; i<argls.size(); i++)  {
				auto fnarg=i<fnc->args.size()?fnc->args[i]:nullptr;
				argls[i]->resolve(sc,fnarg?fnarg->type:nullptr );
			}
			return propogate_type_fwd(this, desired,this->call_target->ret_type);
//			return this->call_target->resolve(this->scope, desired);
		} else {
			return ResolvedType();
		}
	}
	return ResolvedType();
}

ResolvedType resolve_make_fn_call(ExprBlock* block/*caller*/,Scope* scope,const Type* desired) {
	for (int i=0; i<block->argls.size(); i++) {
		block->argls[i]->resolve(scope,desired);
		dbprintf("resolve arg %d type=",i); auto t=block->argls[i]->get_type(); if (t) t->dump(-2); printf("\n");
	}
	ASSERT(block->call_target==0);
	// is it just an array access.
	if (block->square_bracket){
		auto object_t=block->call_expr->resolve(scope,nullptr);
		auto index_t=block->argls[0]->resolve(scope,nullptr);
		//find the method "index(t,index_t)"
		// for the minute, just assume its' an array
		// TODO: operator overload.

		if (object_t.type && index_t.type) {
			ASSERT(object_t.type->is_array() || object_t.type->is_pointer());
			return ResolvedType(object_t.type->sub,ResolvedType::COMPLETE);
		}
		return ResolvedType();
	}

	ExprFnDef* call_target = scope->find_fn(block->call_expr->ident(), block->argls, desired);
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
			return propogate_type_fwd(block, desired, block->type_ref(),fnr);
		}
			// add this to the targets' list of calls.
		int num_known_types=(desired?1:0)+num_known_arg_types(block->argls);
		bool isg=fnc->is_generic();
		if (!(isg && num_known_types)) {
			// concrete function: we can just take return type.
			auto rt=fnc->return_type();
			return propogate_type_fwd(block, desired, rt,block->type_ref());
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
			auto input_type=block->argls[i]->get_type();
			auto v=fsc->create_variable(fnc->args[i]->name,VkArg);
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
		
		auto ret=call_target->resolve_call(fsc,desired);
		return propogate_type(block, ret,block->type());
	}
	else 
		return ResolvedType();
}
int ExprFnDef::type_parameter_index(Name n) const {
	for (auto i=0; i<typeparams.size(); i++){
		if (n==typeparams[i].name)
			return i;
	}
	return -1;
}
ResolvedType ExprFnDef::resolve_call(Scope* scope,const Type* desired) {
	dbprintf("resolve fn call.. %p\n", scope);
	
	for (auto ins=this->instances; ins;ins=ins->next_instance){
		ins->resolve(scope,nullptr);
	}
	
	propogate_type_fwd(this, desired,this->ret_type);

	auto rt=this->body->resolve(scope,desired);
	dbprintf("resolve %s yields type:", getString(this->ident()));if (rt.type) rt.type->dump(-1);printf("\n");
	// awkwardness says: type error return is more like an enum that doesn't return a type?
	// if its' a type error we should favour the most significant info: types manually specified(return values,function args)
	return propogate_type(this, rt,this->ret_type);
}
ResolvedType	ExprFor::resolve(Scope* outer_scope,const Type* desired){
	auto sc=outer_scope->make_inner_scope(&this->scope);
	if (init) init->resolve(sc,0);
	if (cond) cond->resolve(sc,0);
	if (body) body->resolve(sc,0);
	if (else_block) else_block->resolve(sc,0);
	return ResolvedType();
}

ResolvedType ExprFnDef::resolve(Scope* definer_scope, const Type* desired) {

	definer_scope->add_fn(this);

	auto sc=definer_scope->make_inner_scope(&this->scope,this);
		//this->scope->parent=this->scope->global=scope->global; this->scope->owner=this;}
	if (!this->is_generic()){
	for (int i=0; i<this->args.size() && i<this->args.size(); i++) {
		auto arg=this->args[i];
		auto v=sc->find_scope_variable(arg->name);
		if (!v){v=sc->create_variable(arg->name,VkArg);}
		propogate_type(this, arg->type_ref(),v->type_ref());
		if (arg->default_expr){static int warn=0; if (!warn){warn=1;
			dbprintf("error todo default expressions really need to instantiate new code- at callsite, or a shim; we need to manage caching that. type propogation requires setting those up. Possible solution is giving a variable an initializer-expression? type propogation could know about that, and its only used for input-args?");}
		}
	}
	if (this->body){
		auto ret=this->body->resolve(sc,this->ret_type);
		propogate_type(this, ret,this->ret_type);
	}
	}

	if (!this->fn_type) {
		this->fn_type=new Type(FN);
		auto arglist=new Type(TUPLE);
		this->fn_type->push_back(arglist);
		for (auto a:this->args) {
			arglist->push_back(a->type?((Type*)a->type->clone()):new Type(AUTO));
		}
		this->fn_type->push_back(this->ret_type?(Type*)(this->ret_type->clone()):new Type(AUTO));
		
		const Type* t0=this->fn_type; Type*& tr=this->type_ref();
		propogate_type_fwd(this, t0,tr);
	}
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
	} else if (auto op=dynamic_cast<ExprOp*>(node)){
		gather_named_items(op->lhs,sc);
		gather_named_items(op->rhs,sc);
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
	char filename[512];
	SrcPos	pos;
	const char* buffer,*tok_start,*tok_end,*prev_start,*line_start;
	Name curr_tok;
#ifdef WATCH_TOK
	char watch_tok[64][12];
#endif
	bool error_newline;
	void error(const char* str,...){
		if(!error_newline){printf("\n");}
		printf("%s:%d:",filename,pos.line);
		char tmp[1024];
		va_list arglist;
		
		va_start( arglist, str );
		vsprintf(tmp, str, arglist );
		va_end( arglist );
		printf("\n"); error_newline=true;
	}
	TextInput(const char* src,const char *filename_){
		strncpy(filename,filename_,512);
		g_filename=filename;
		
		line_start=src;
		pos.set(1,0);
		buffer=src;
		curr_tok=-1;
		tok_start=tok_end=buffer;
		advance_tok();
	}

	void advance_sub(bool (*sub)(char c)){
		while ( *tok_end && sub(*tok_end)) tok_end++;
	}
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
	void skip_whitespace(){
		while (isWhitespace(*tok_end)&&*tok_end) {
			if (*tok_end=='\n') {pos.line++;line_start=tok_end;}
			pos.col=tok_end-line_start;
			tok_end++;
		}
	}

	void advance_tok() {
		skip_whitespace();
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
#ifdef WATCH_TOK
		for (auto i=10; i>0; i--){strcpy(watch_tok[i],watch_tok[i-1]);}
		memcpy(watch_tok[0],tok_start,tok_end-tok_start); watch_tok[0][tok_end-tok_start]=0;
#endif
	}
	Name eat_tok() {
		prev_start=tok_start;
		for (const char* c=tok_start; c!=tok_end;c++) {}
		auto r=curr_tok;
		advance_tok();
		return r;
	}
	bool eat_if(Name i) {
		if (peek_tok()==i) {eat_tok(); return true;}
		else return false;
	}
	bool is_placeholder()const {return  ((*tok_start=='_') && !isSymbolCont(*tok_end));}
	Name eat_if_placeholder(){if (is_placeholder()){advance_tok(); return PLACEHOLDER;} else return Name();}
	Name eat_ident() {
		auto r=eat_tok();
		if (r<IDENT) {error("expected ident found %s",getString(r));exit(0);}
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

	Name peek_tok(){return curr_tok;}
	void reverse(){ ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;}
	Name expect(Name t){ int x;if (!(t==(x=eat_tok()))) {error(0,"expected %s found %s",str(t), str(x));exit(0);} return x;}
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
struct SrcOp{ int op; SrcPos pos;};
void pop_operator_call( vector<SrcOp>& operators,vector<Expr*>& operands) {
	//takes the topmost operator from the operator stack
	//creates an expression node calling it, consumes operands,
	//places result on operand stack

	auto op=pop(operators);
	auto * p=new ExprOp(op.op,op.pos);
	if (operands.size()>=2 && (arity(op.op)==2)){
		auto arg1=pop(operands);
		p->lhs=pop(operands);
		p->rhs=arg1;
	} else if (operands.size()>=1 && arity(op.op)==1){
		p->rhs=pop(operands);
//		p->argls.push_back(pop(operands));
	} else{
//						printf("\noperands:");dump(operands);
//						printf("operators");dump(operators);
		error(0,"\nerror: %s arity %d, %lu operands given\n",str(op.op),arity(op.op),operands.size());
		exit(0);
	}
	p->pos=p->lhs?p->lhs->pos:p->rhs->pos;
	operands.push_back((Expr*)p);
}
//   void fn(x:(int,int),y:(int,int))
void flush_op_stack(ExprBlock* block, vector<SrcOp>& ops,vector<Expr*>& vals) {
	while (ops.size()>0) pop_operator_call(ops,vals);
	while (vals.size()) {
		block->argls.push_back(pop(vals));
	}
}

ExprBlock* parse_call(TokenStream&src,int close,int delim, Expr* op);

Expr* parse_expr(TokenStream&src) {
	return parse_call(src,0,0,nullptr);
}

void another_operand_so_maybe_flush(bool& was_operand, ExprBlock* node,
					  vector<SrcOp>& operators,
					  vector<Expr*>& operands

					  ){
	if (was_operand==true) {
		dbprintf("warning undeliminated expression parsing anyway");
		flush_op_stack(node,operators,operands);// keep going
	}
	was_operand=true;
}
Type* parse_type(TokenStream& src, int close);

LLVMType Expr::get_type_llvm() const
{
	if (!this) return LLVMType{VOID,0};
	if (!this->m_type) return LLVMType{VOID,0};
	auto tn=this->m_type->name;
	if (tn==VOID) return LLVMType{VOID,0};
	if (!this->m_type->sub) return LLVMType{tn,0};
	if (tn==PTR || tn==DEREF ||tn==ADDR ) return LLVMType{this->m_type->sub->name,true};
	// todo structs, etc - llvm DOES know about these.
	return LLVMType{0,0};
}

ExprBlock* parse_call(TokenStream&src,int close,int delim, Expr* op) {
	// shunting yard expression parser
	ExprBlock *node=new ExprBlock; node->call_expr=op; node->pos=src.pos;
	verify(node->type());
	vector<SrcOp> operators;
	vector<Expr*> operands;
	bool	was_operand=false;
	int wrong_delim=delim==SEMICOLON?COMMA:SEMICOLON;
	int wrong_close=close==CLOSE_PAREN?CLOSE_BRACE:CLOSE_PAREN;
	node->square_bracket=close==CLOSE_BRACKET;
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
		dbprintf(";:%s\n",getString(src.peek_tok()));
		dbprintf(";parse:- %zu %zu\n",operands.size(),operators.size());
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
			auto local_fn=parse_fn(src);
			operands.push_back(local_fn);
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
			else {
				operands.push_back(parse_call(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACKET)){
			if (was_operand){
				operands.push_back(parse_call(src,CLOSE_BRACKET,COMMA,pop(operands)));
			} else {
				//dbprintf("TODO: array literal [....]\n");
				operands.push_back(parse_call(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
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
			if (is_operator(tok)) {
				if (was_operand) tok=get_infix_operator(tok);
				else tok=get_prefix_operator(tok);

				while (operators.size()>0) {
					int prev_precedence=precedence(operators.back().op);
					int prec=precedence(tok);
					if (prev_precedence>prec
						||(is_right_assoc(tok)&&prec==prev_precedence))
						break;
					pop_operator_call(operators,operands);
				}
				if (tok==COLON){// special case: : invokes parsing type. TODO: we actually want to get rid of this? type could be read from other nodes, parsed same as rest?
					Type *t=parse_type(src,0);
					auto lhs=operands.back();
					lhs->set_type(t);
					was_operand=true;
				} else if (tok==ASSIGN_COLON){ //x=:Type  ... creates a var of 'Type'.
					Type *t=parse_type(src,0);
					operators.push_back(SrcOp{tok,src.pos});
					operands.push_back(t);
					was_operand=true;
				}

				else{
					operators.push_back(SrcOp{tok,src.pos});
					was_operand=false;
				}
			} else {
				another_operand_so_maybe_flush(was_operand,node,operators,operands);
				operands.push_back(new ExprIdent(tok,src.pos));
			}
		}
		//ASSERT(sub);
		//node->argls.push_back(sub);
	};
	flush_op_stack(node,operators,operands);
	verify(node->get_type());
	return node;
}

Type* parse_type(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	Type* ret=0;	// read the first, its the form..
	if (tok==close) return nullptr;
	if (tok==OPEN_PAREN) {
		ret=new Type(TUPLE,src.pos);
		while (auto sub=parse_type(src, CLOSE_PAREN)){
			ret->push_back(sub);
			src.eat_if(COMMA);
		}
	} else {
		// prefixes in typegrammar..
		if (tok==MUL || tok==AND) {
			ret=new Type(PTR);
			ret->sub=parse_type(src,close);
		}else {
		// main: something[typeparams]..
			ret = new Type(tok);
			if (src.eat_if(OPEN_BRACKET)) {
				while (auto sub=parse_type(src, CLOSE_BRACKET)){
					ret->push_back(sub);
					src.eat_if(COMMA);
				}
			}
		// postfixes:  eg FOO|BAR|BAZ todo  FOO*BAR*BAZ   FOO&BAR&BAZ
			if (src.peek_tok()==OR){
				Type* sub=ret; ret=new Type(VARIANT); ret->push_back(sub);
				while (src.eat_if(OR)){
					auto sub=parse_type(src,close);
					ret->push_back(sub);
				}
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
		out.push_back(TypeParam{name,src.eat_if(ASSIGN)?parse_type(src,0):0});
		src.eat_if(COMMA);
	}
}

ExprStructDef* parse_struct(TokenStream& src) {
	auto sd=new ExprStructDef(src.pos);
	dbprintf("parse_struct");
	auto tok=src.eat_ident();
	sd->name=tok;
	if (src.eat_if(OPEN_BRACKET)) {
		parse_typeparams(src,sd->typeparams);
	}
	if (src.eat_if(COLON)) {
		sd->inherits_type = parse_type(src,0); // inherited base has typeparams. only single-inheritance allowed. its essentially an anonymous field
	}

	if (!src.eat_if(OPEN_BRACE))
		return sd;
	// todo: type-params.
	while (NONE!=(tok=src.peek_tok())){
		if (tok==CLOSE_BRACE){src.eat_tok(); break;}
		if (src.eat_if(STRUCT)) {
			sd->structs.push_back(parse_struct(src));
		} else if (src.eat_if(FN)){
			sd->functions.push_back(parse_fn(src));
		} else {
			auto arg=parse_arg(src,CLOSE_PAREN);
			sd->fields.push_back(arg);
		}
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	return sd;
}
void Type::translate_typeparams(const vector<TypeParam>&param_format, const vector<Type*>& given_params){
/*
 example:
 struct vector<T,N=int> {
 	data:*T;
 	count:N;
 }

 instantiate vector<string,short>
	translate_tempalte_type( {T,N}, {string,short}, *T) -> *string
	translate_tempalte_type( {T,N}, {string,short}, N) -> short.
 
 HKT example
 struct tree<S,T>{
 	S<tree<S,T>>	sub;
 }
instantiate tree<vector,int>
 we want:
 struct tree {
    vector< tree<vector,int>> sub;
 }
 
 translate_tempalte_type( {S,T}, {vector,int}, S<tree<S,T>>)->    vector<tree<vector<int>>> //
 
 */
	// TODO: assert there is no shadowing in this types' own definitions

	Type* new_type=0;
	int param_index=get_typeparam_index(param_format, this->name);
	if (param_index>=0){
		auto src_ty=given_params[param_index];
		dbprintf("substituting %s %d/%d %d\n", str(this->name),param_index,param_format.size(), given_params.size());
		if (!src_ty){error(this,"typaram not given,partial instance?");}
		if (!src_ty->sub) {
			this->name=src_ty->name;
		} else if (!this->sub){
			this->name=src_ty->name;
			for (auto s=src_ty->sub;s;s=s->next){
				this->push_back((Type*)s->clone());
			};
		}
		else error(this,"trying to instantiate complex typeparameter into non-root of another complex typeparameter,we dont support this yet");
	}
	for (auto sub=this->sub; sub; sub=sub->next) {
		sub->translate_typeparams(param_format,given_params);
	}
}
bool type_params_eq(const vector<Type*>& a, const vector<Type*>& b) {
	if (a.size()!=b.size()) return false;
	for (int i=0; i<a.size(); i++) { if (!a[i]->eq(b[i])) return false;}
	return true;
}
ExprStructDef* ExprStructDef::get_instance(Scope* sc, Type* type) {
//	auto parent = sc->find_struct(type); //parent is this duh
	auto parent=this;
	vector<Type*> ty_params;
	// make the typeparams..
	Type* tp=type->sub;
	int i=0;
	for (i=0; i<parent->typeparams.size() && tp; i++,tp=tp->next){
		ty_params.push_back(tp);
	}
	for (;i<parent->typeparams.size(); i++) {
		ty_params.push_back(parent->typeparams[i].defaultv);
	}
	// search for existing instance
	ExprStructDef* ins=parent->instances;
	for (;ins; ins=ins->next_instance) {
		if (type_params_eq(ty_params, ins->instanced_types))
			break;
	}
	if (!ins) {
		ins = (ExprStructDef*)this->clone(); // todo: Clone could take typeparams
							// cloning is usually for template instantiation?
		ins->instanced_types=ty_params;
		ins->instance_of=this;
		ins->next_instance = this->instances; this->instances=ins;
		ins->inherits_type= this->inherits_type; // TODO: typeparams! map 'parent' within context  to make new typeparam vector, and get an instance for that too.

		ins->translate_typeparams(this->typeparams, ins->instanced_types);
		dbprintf("template instantiation of %s\n :-{", str(parent->name));
		type->dump(-1);
		ins->dump(-1);
		dbprintf("}template instantiation", str(parent->name));
	}
	if (!type->struct_def) { type->struct_def=ins;}
	else { ASSERT(type->struct_def==ins && "instantiated type should be unique")};
	return ins;
}

Node* ExprStructDef::clone() const{
	ExprStructDef* d=new ExprStructDef(this->pos);
	d->name=this->name;
	for (auto m:this->fields) {d->fields.push_back((ArgDef*)m->clone());}
//	for (auto t:this->typeparams) {d->typeparams.push_back(t->clone());}
	d->typeparams = this->typeparams;
	for (auto f:this->functions){d->functions.push_back((ExprFnDef*)f->clone());}
	for (auto s:this->structs){d->structs.push_back((ExprStructDef*)s->clone());}
	return d;
}

void ExprStructDef::translate_typeparams(const vector<TypeParam> &type_params, const vector<Type *>& given_types)
{
	for (auto a:this->fields) {
		a->translate_typeparams(/*sc,*/ type_params, given_types);
	}
	for (auto f:functions){
		f->translate_typeparams(type_params,given_types);
	}
	for (auto s:structs){
		s->translate_typeparams(type_params,given_types);
	}
	if (typeparams_all_set(type_params,given_types))
	{
		this->typeparams.resize(0);
	}
}

void ArgDef::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &types){
	this->name.translate_typeparams(typeparams,types);
	if (this->get_type()){
		this->get_type()->translate_typeparams(typeparams,types);
	}
	if (this->default_expr){
		this->default_expr->translate_typeparams(typeparams,types);
	}
}

void ExprStructDef::inherit_from(Scope * sc,Type *base_type){
	if (inherits!=0) return;// already resolved.ø
	auto base_template=sc->find_struct_named(base_type->name);
	ExprStructDef* base_instance=base_template;
	if (base_type->is_template()) {
		base_instance = base_template->get_instance(sc, base_type);
	}
	ASSERT(inherits==0); next_of_inherits=base_instance->derived; base_instance->derived=this; this->inherits=base_instance;
}

void ExprStructDef::dump(int depth) const{
	newline(depth);
	dbprintf("struct %s",getString(this->name));dump_typeparams(this->typeparams);
	dbprintf("[");
	if (this->instanced_types.size()){
		for (auto t:this->instanced_types)
		{	t->dump(depth+1);dbprintf(",");};
	}else{
		for (auto t:this->typeparams)
			{t.dump(depth+1);dbprintf(",");}
	}
	dbprintf("]");
	if (this->inherits) {dbprintf(" : %s", str(inherits->name));}
	dbprintf("{");
	for (auto m:this->fields){m->dump(depth+1);}
	for (auto s:this->structs){s->dump(depth+1);}
	for (auto f:this->functions){f->dump(depth+1);}
	newline(depth);dbprintf("}");
}
bool ExprStructDef::is_generic()const{
	if (typeparams.size())
		return true;
	for (auto f:fields){if (!f->type)return true;}//TODO: is typeparam?
	return false;
}
ResolvedType ExprStructDef::resolve(Scope* definer_scope,const Type* desired){

	definer_scope->add_struct(this);
	if (!this->get_type()) {
		this->set_type(new Type(this->name));	// name selects this struct.
	}

	auto sc=definer_scope->make_inner_scope(&this->scope,this);
	for (auto s:structs){ s->resolve(sc,nullptr);}
	for (auto f:functions){ f->resolve(sc,nullptr);}

	return propogate_type_fwd(this, desired,this->type_ref());
}
// iterator protocol. value.init. increment & end test.
ExprFor* parse_for(TokenStream& src){
	auto p=new ExprFor;
	auto first=parse_call(src,SEMICOLON,COMMA,0);
//	if (src.eat_if(IN)){
//		p->pattern=first;
//		p->init=parse_call(src, OPEN_BRACE, 0, 0);
//		src.expect(OPEN_BRACE);
//	} else if (src.eat_if(SEMICOLON)){// cfor.  for init;condition;incr{body}
	if (true) {
		p->pattern=0;
		p->init=first;
		p->cond=parse_expr(src);
		src.expect(SEMICOLON);
		p->incr=parse_call(src,OPEN_BRACE,COMMA,0);
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
	// TODO: assignments inside the 'if ..' should be in-scope
	// eg if (result,err)=do_something(),err==ok {....}  else {...}
	auto p=new ExprIf;
	p->cond=parse_call(src, OPEN_BRACE, 0, 0);
	p->body=parse_call(src, CLOSE_BRACE,SEMICOLON,0);
	verify(p->cond->get_type());

	if (src.eat_if(ELSE)) {
		if (src.eat_if(IF)) {
			p->else_block= parse_if(src);
		} else if (src.eat_if(OPEN_BRACE)){
			p->else_block=parse_call(src, CLOSE_BRACE, SEMICOLON, 0);
		} else {
			error(0,"if { }else {} expected\n");
		}
	}
	if (p->cond) verify(p->cond->get_type());
	if (p->body) verify(p->body->get_type());
	if (p->else_block) verify(p->else_block->get_type());
	return p;
}
Node* ExprIf::clone()const {
	auto p=new ExprIf;
	p->cond=(Expr*)this->cond->clone_if();
	p->body=(Expr*)this->body->clone_if();
	p->else_block=(Expr*)this->else_block->clone_if();
	verify(p->cond->get_type());
	return p;
}
void ExprIf::dump(int depth) const {
	verify(cond->get_type());
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

ResolvedType ExprIf::resolve(Scope* s,const Type* desired){
	verify(this->cond->get_type());
	this->cond->resolve(s,nullptr); // condition can  be anything coercible to bool
	auto body_type=this->body->resolve(s,desired);
	Type* bt=body_type.type;
	if (else_block){
		propogate_type_fwd(this, desired,bt);
		propogate_type(this, bt,this->type_ref());
		return else_block->resolve(s,bt);
	}
	else {
		// TODO: Could it actually return Body|void ? perhaps we could implicityly ask for that?
		return body_type;
	}
}
void ExprIf::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &types){
	this->cond->translate_typeparams(typeparams,types);
	this->body->translate_typeparams(typeparams,types);
	this->else_block->translate_typeparams(typeparams,types);
}

void ExprFor::dump(int d) const {
	newline(d);dbprintf("for ");
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
void ExprFor::translate_typeparams(const vector<TypeParam> &typeparams, const vector<Type *> &types)
{
	if (this->init) this->init->translate_typeparams(typeparams,types);
	if (this->cond) this->cond->translate_typeparams(typeparams,types);
	if (this->incr) this->incr->translate_typeparams(typeparams,types);
	if (this->pattern) this->pattern->translate_typeparams(typeparams,types);
	if (this->body) this->body->translate_typeparams(typeparams,types);
	if (this->else_block) this->else_block->translate_typeparams(typeparams,types);
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
	auto *fndef=new ExprFnDef(src.pos);
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
	"fn lerp[T,F](a:T,b:T,f:F){(b-a)*f+a};  \n"
	"fn foo(a:int)->int{printf(\"foo_int\n\");0};  \n"
	"fn printf(s:str,...)->int;  \n"
	"fn push_back[T](t:Vec[T],v:T)->int{  \n"
	"	a:=t.num;   \n"
	"	printf(\"push_back %d\n\", a);0  \n"
	"}  \n"
	"struct Vec[T]{data:*T, num:int};   \n"
	"struct Vec3[X=vx,Y=vy,Z=vz]{X:float,Y:float,Z:float};   \n"
//	"fn F[F,O,X,Y](o:O, x:X,y:Y){ o.vtable.F(o,x,y)   };"
	"fn main(argc:int,argv:**char)->int{  \n"
	"	t1:=argc<0 && argc>1;\n"
	"	test_result:=if argc==0 || argc==1 {14} else {13} ;"
	"	xs=:array[int,512];  \n"
	"	p2:=&xs[1];  \n"
	"	fs=:array[float,512];  \n"
	"	ys=:Vec[int];  \n"
	"	ys.data=&xs[3];  \n"
	"	ys.num=10;   \n"
	"	yp:=ys.data;  \n"
	"	push_back(ys,2);  \n"
	"	f:=fn local_fn(a:float,b:int)->int{printf(\"hello lambda\n\");b};  \n"
	"	my_vec=:Vec3[vecx];  my_vec.vecx=10.0;myvec.vy=5.0;  \n"
	"	r:=f(0.1,2);  \n"
	"	q:=xs[1];  p1:=&xs[1];  q:=*p1; \n"
	"	xs[1]=20;  xs[2]+=400;  xs[3]=500;  \n"
	"	*ys.data += 20;  \n"
	"	*p1=30;  \n"
	"   z:=5;  foo(z);  \n"
	"   y:=xs[1]+z+xs[2];  \n"
	"	f0:=lerp(10.0,20.0,0.5);\n"
	"   x:=if argc<2{1}else{2};  \n"
	"	for i:=0,j:=0; i<8; i+=1,j+=10 {x+=i; printf(\"i,j=%d,%d,x=%d\n\",i,j,x);}else{printf(\"loop exit fine\n\");}  \n"
	"	xs1:=xs[1]; xs2:=xs[2];  \n"
	"	printf(\"Hello From My Language %.3f %d %d %d %d \\n\", lerp(10.0,20.0,0.5) xs1,xs[3], *(ys.data),test_result); \n"
	"	0  \n"
	"}  \n\0"

/*
	"struct Foo{x:int,y:int, struct Bar{x:float}, fn method(i:int)->int{printf(\"hello from method %d\n\",i);0};};"
	"fn printf(s:str,...)->int;"
	"fn main(argc:int, argv:**char)->int{"
	"	fn local_function(i:int)->int{ printf(\"hello from local function\n\"); method(i); 0;};"
	"	method(1);"
	"	local_function(3);"
	"	0"
	"}"
 */
;
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

void filename_change_ext(char* dst,const char* src,const char* new_ext){
	strcpy(dst,src);
	char* s=dst;
	for (s=dst;*s;s++){
		if (*s=='.') break;
	}
	if (!*s) { strcat(dst, ".out");}
	else
	if (!strlen(new_ext)) { s[0]=0;}
	else strcpy(s+1,new_ext);
}
/*
// TODO literal format,
// we'd like JSON but the fly in the ointment is : vs = with : used for types.
// if '=' is used for field initializers we only have one grammar everywhere.
// could we hack a rule.. "if a block only contains ident:expr,.. it's a struct literal"
{}     // statement
{ ; ; }  // compound statement
{ , , } // ANON struct literal
[ , , , ] // 'slice' literal: *T,size. if  types differ, at compile time make a union?
(, , ) // tuple literal  assignments in brackets
 		// tuple should auto coerce to 'array(T,N)' if its (T,T,T..)
//

foo:={
	foo:[1,2,3],
 	bar:"bar node",
 	baz:{
 		foo:10.0,
 		bar:20.0,
 	}
 	yada:other
}
 
other:={

}

should roll an appropriate anonymous struct;
should reduce types on parsing them - if it finds ones with same fields
should be able to code assetless


 	for x in struct{  // that gets the record fields - its compile time
// only if you overload begin/end do you get iteration
 		.ident // gives a string of the field.
 		.typedef // gives its' type.
 	}
*/

enum COMPILE_FLAGS {
	LLVM=0x0001, TYPES=0x0002, EXECUTABLE=0x0004, RUN=0x008
};
int compile_source(const char *buffer, const char* filename, const char* outname, int flags){

	TextInput	src(buffer,filename);
	auto node=parse_call(src,0,SEMICOLON,nullptr);
	node->dump(0);
	Scope global(0); global.node=(ExprBlock*)node; global.global=&global;
	gather_named_items(node,&global);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	node->resolve(&global,nullptr);
	if (flags & TYPES) {
		global.dump(0);
		node->dump(0);
	}
	if (flags & LLVM) {
		output_code(stdout, &global);
	}
	if (outname){
		FILE* ofp=fopen(outname,"wb");
		if (ofp){
			output_code(ofp, &global);
			fprintf(ofp,"\n;end");
			fclose(ofp);
			if (flags & EXECUTABLE) {

				char exename[256];
				filename_change_ext(exename,outname,"");
				char compile_cmd[512]; sprintf(compile_cmd,"clang %s -o %s", outname, exename);
				printf("\nllvm src=%s\n executable=%s\nflags %x\n",outname,exename, flags);
				printf("\n%s\n",compile_cmd);
				auto ret= system(compile_cmd);
				if (!ret && (flags & RUN)) {
					printf("compiled ok, running executable %s \n", exename);
					char invoke[512];sprintf(invoke,"./%s",exename);
					return system(invoke);
					return 0;
				}
				return ret;
			}
		} else {
			printf("can't open output file %s\n",outname);
			return -1;
		}
	}
	return 0;
}

int compile_source_file(const char* filename, int options) {
	char outname[256];
	filename_change_ext(outname,filename,"ll");
	printf("compiling %s\n -> %s\n",filename,outname);
	auto fp=fopen(filename,"rb");
	if (fp){
		fseek(fp,0,SEEK_END); auto sz=ftell(fp); fseek(fp,0,SEEK_SET);
		char* buffer = (char*)malloc(sz+1);
		fread((void*)buffer,1,sz,fp);
		buffer[sz]=0;
		fclose(fp);
		int ret=compile_source(buffer,filename,outname,options);
		free((void*)buffer);
		return ret;
	} else{
		printf("can't open %s\n",filename);
		return -1;
	}
}
int main(int argc, const char** argv) {
//	compile_source_file("~/hack/test_hack/prog.rs",0xf);
	int options=0,given_opts=0;
	for (auto i=1; i<argc; i++) {
		const char* a=argv[i];
		if (a[0]=='-'){
			for (int j=1; a[j];j++){
				if (a[j]=='t') options|=TYPES;
				if (a[j]=='l') options|=LLVM;
				if (a[j]=='r') options|=RUN|EXECUTABLE;
				if (a[j]=='e') options|=EXECUTABLE;
			}
		}
	}
	if (!options) options=TYPES|LLVM|RUN|EXECUTABLE;
	
	for (auto i=1; i<argc; i++) {
		if (argv[i][0]!='-') {
			printf("compile src file %s with options %x\n",argv[i],options);
			compile_source_file(argv[i],options);
		}
	}
	if (argc<=1) {
		printf("no sources given so running inbuilt test.\n");
		auto ret=compile_source(g_TestProg,"g_TestProg","test.ll",options);
		if (!ret) {
			printf("\nSource we just compiled, use for reference..\n");
			printf(g_TestProg);
			printf("\noptions -l emit llvm IR only -e emit executable only -t dump AST with types -r compile & run\n");
			printf("\ndefault: it will compile & run a source file;\ngenerates srcname.ll & srcname/srcname.out executable");
		}
	}
}




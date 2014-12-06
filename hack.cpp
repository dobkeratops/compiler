
#include "hack.h"
#include "codegen.h"
#include "repl.h"

const char** g_pp,*g_p;
const char* g_filename=0;

#if DEBUG>=2
#define dbprintf_varscope dbprintf
#define dbprintf_generic dbprintf
#define dbprintf_lambdas dbprintf
#define dbprintf_instancing dbprintf
#define dbprintf_resolve dbprintf
#define dbprintf_fnmatch dbprintf
#define dbprintf_type dbprintf
#else
inline void dbprintf_fnmatch(const char*,...){}
inline void dbprintf_varscope(const char*,...){}
inline void dbprintf_generic(const char*,...){}
inline void dbprintf_lambdas(const char*,...){}
inline void dbprintf_instancing(const char*,...){}
inline void dbprintf_resolve(const char*,...){}
inline void dbprintf_type(const char*,...){}
#endif

const int g_debug_get_instance=false;
struct VTablePtrs {
	void* expr_op;
	void* expr_ident;
	void* expr_fn_def;
	void* expr_block;
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
	ASSERT(g_vtable_ptrs.expr_block==*(void**)p)
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
bool g_error_on_newline=false;
int g_num_errors=0;
void error_newline(){
	if (!g_error_on_newline){
		g_error_on_newline=true;printf("\n");}
}
void error_sub(const Node* n, const char* level, const char* txt ){
	g_num_errors++;
	error_newline();
	if (n)
		printf("%s:%d:%d:",g_filename,n->pos.line,n->pos.col);
	if (level)printf("%s-",level);
	printf("\t%s",txt);
	if (strlen(txt)){
		if (txt[strlen(txt)-1]!='\n'){g_error_on_newline=false;}
	} else{
		n->dump_if(-1);
	}
	if (n){
		if (auto x=n->instanced_by()){
			dbprintf("%p %p\n",n,x);
			printf("%s:%d:%d: referenced here",g_filename,x->pos.line,x->pos.col);
		}
	}
}
void error(const SrcPos& pos, const char* str ,...){
	char txt[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(txt, str, arglist );
	va_end( arglist );
	
	g_num_errors++;
	error_newline();
	printf("%s:%d:%d:",g_filename,pos.line,pos.col);
	printf("\t%s",txt);
	if (strlen(txt)){
		if (txt[strlen(txt)-1]!='\n'){g_error_on_newline=false;}
	}
}

void error_begin(const Node* n, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,"error",buffer);
}
void warning(const Node* n, const char* str="", ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,"warning",buffer);
}
void info(const Node* n, const char* str="", ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,"",buffer);
}
void error_end(const Node* n){
	error_newline();
	if (g_num_errors>0){
		exit(0);
	}
}

void error(const Node* n,const Scope* s, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,"error",buffer);
	error_newline();
	info(s->owner_fn, "in scope %s\n",s->name());
	error_end(n);
}
void error(const Node* n,const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,"error",buffer);
	error_end(n);
}
void error(const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(nullptr,"error",buffer);
	error_end(nullptr);
}

void error(const Node* n,const Node* n2, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,"error",buffer);
	warning(n2,"see %s",n2->name_str());
	error_end(n);
}

ResolvedType resolve_make_fn_call(Expr* receiver,ExprBlock* block/*caller*/,Scope* scope,const Type* desired,int flags);

void print_tok(Name n){
	dbprintf("%s ",getString(n));
};

bool g_lisp_mode=false;
int g_size_of[]={
	0,
	4,4,8,1,2,4,8,1,2,4,8,16,1,
	2,4,8,161,8,0,-1,-1,-1,8,8,8
};
int g_raw_types[]={
	4|RT_SIGNED|RT_INTEGER,
	4|RT_INTEGER,
	8|RT_INTEGER,
	1|RT_SIGNED|RT_INTEGER,
	2|RT_SIGNED|RT_INTEGER,
	4|RT_SIGNED|RT_INTEGER,
	8|RT_SIGNED|RT_INTEGER,
	1|RT_INTEGER,
	2|RT_INTEGER,
	4|RT_INTEGER,
	8|RT_INTEGER,
	16|RT_INTEGER,
	1|RT_INTEGER,
	2|RT_FLOATING,
	4|RT_FLOATING,
	8|RT_FLOATING,
	16|RT_FLOATING|RT_SIMD,
	1|RT_INTEGER,
	8|RT_POINTER,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0
};
const char* g_token_str[]={
	"",
	"int","uint","size_t","i8","i16","i32","i64","u8","u16","u32","u64","u128","bool",
	"half","float","double","float4","char","str","void","voidptr","one","zero","auto",
	"ptr","ref","tuple","__NUMBER__","__TYPE__","__IDNAME__",
	
	"print___","fn","struct","class","trait","virtual","static","enum","array","vector","union","variant","with","match","sizeof","typeof","nameof","offsetof", "this","self","super","vtableof","closure",
	"let","var",
	"const","mut","volatile",
	"while","if","else","do","for","in","return","break","continue",
	"(",")",
	"{","}",
	"[","]",
	"<[","]>",
	"->",".","?.","=>","<-","::","<->",			//arrows,accessors
	"|>","<|","<.>","<$>","<:",":>",			// some operators we might nick from other langs

	":","as","new","delete",
	"+","-","*","/",					//arithmetic
	"&","|","^","%","<<",">>","?:","?>","?<",					//bitwise
	"<",">","<=",">=","==","!=",		//compares
	"&&","||",		//logical
	"=",":=","=:",
	"+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=", // assign-op
	".=",	// linklist follow
	"++","--","++","--", //inc/dec
	"-","*","&","!","~", // unary ops
	"*?","*!","&?","~[]","[]","&[]", // special pointers
	",",";",";;",
	"...","..",
	"_",
	NULL,	
};

int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,// int types
	0,0,0,0,0,0,0,0,0,0,0,	// float types
	0,0,0,0,0,0,			// type modifiers
	0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0, 0,0,0,0,0, // keywords
	0,0,			// let,var
	0,0,0,			// modifiers const,mut,volatile
	0,0,0,0,0,0,0,0,0,  // while,if,else,do,for,in,return,break
	0,0, //( )
	0,0, //{ }
	0,0, // [ ]
	0,0, //<[Type]>
	READ|10,READ|2,READ|2,READ|10,READ|10,READ|13,WRITE|10,	   // dots, arrows
	READ|17,READ|17,READ|17,READ|5,READ|17,READ|17,	// unusual stuff
	READ|9,READ|9, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,
	READ|6,READ|6,READ|5,READ|5,		//arithmetic
	READ|8,READ|7,READ|8,READ|6,READ|9,READ|9,READ|9,READ|9,READ|9,	//bitwise
	READ|8,READ|8,READ|8,READ|8,READ|9,READ|9,	// COMPARES
	READ|13,READ|14,	//logical
	WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16, // assignment
	
	WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16, // assign-op
	WRITE_LHS|READ|ASSOC|16, // dot-assign
	MODIFY|PREFIX|UNARY|2,MODIFY|PREFIX|UNARY|2,MODIFY|UNARY|ASSOC|3,MODIFY|UNARY|ASSOC|3, // post/pre inc/dec
	READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3, //unary ops
	READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,READ|UNARY|ASSOC|3, /// special pointers
	0,0,17, // delim
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
bool is_callable(Name tok) { return (tok==FN || tok==CLOSURE);}
int operator_flags(Name tok){return g_tok_info[index(tok)];}
int precedence(Name ntok){auto tok=index(ntok);return tok<IDENT?(g_tok_info[tok] & PRECEDENCE):0;}
int is_prefix(Name ntok){auto tok=index(ntok);return tok<IDENT?(g_tok_info[tok] & (PREFIX) ):0;}
int arity(Name ntok){auto tok=index(ntok);return  (tok<IDENT)?((g_tok_info[tok] & (UNARY) )?1:2):-1;}
int is_right_assoc(Name ntok){auto tok=index(ntok);return (tok<IDENT)?(g_tok_info[tok]&ASSOC):0;}
int is_left_assoc(Name ntok){auto tok=index(ntok);return (tok<IDENT)?(!(g_tok_info[tok]&ASSOC)):0;}
Name get_prefix_operator(Name tok) {
	auto itok=index(tok);
	if (itok>IDENT) return tok;
	switch (itok){
	case POST_INC: return Name(PRE_INC);
	case POST_DEC: return Name(PRE_DEC);
	case SUB: return Name(NEG);
	case MUL: return Name(DEREF);
	case AND: return Name(ADDR);
	default: return tok;
	}
}
Name get_infix_operator(Name tok) {
	auto itok=index(tok);
	if (itok>IDENT) return tok;
	switch (itok){
	case PRE_INC: return Name(POST_INC);
	case PRE_DEC: return Name(POST_DEC);
	case NEG: return Name(SUB);
	case DEREF: return Name(MUL);
	case ADDR: return Name(AND);
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
//const char* g_llvm_type[]={
//	"i32","i32","i1","float","i8","i8*"
//};
const char* g_llvm_type_str[]={
	"i32","u32","i64",
	"i8","i16","i32","i64","u8","u16","u32","u64","u128","i1",
	"half","float","double","< 4 x float >", "i8", "i8*","void","void*",
	nullptr
};
const char* get_llvm_type_str(Name n_type_name){
	auto tname=index(n_type_name);
	if (tname>=INT && tname<=(VOIDPTR)){
		return g_llvm_type_str[tname-INT];
	}
	return getString(tname);
}

const LLVMOp* get_op_llvm(Name ntok,Name ntype){
	auto tok=index(ntok); auto type=index(ntype);
	int ti=(type==FLOAT||type==DOUBLE||type==FLOAT)?1:0;
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
Name strConcat(Name n1, Name n2);
inline Name strConcat(Name n1, Name n2,Name n3){ return strConcat(n1,strConcat(n2,n3));}

Name getStringIndexConcat(const char* s1, const char* s2){
	char tmp[512];
	snprintf(tmp,511,s1,s1);
	return getStringIndex(tmp);
}
Name strConcat(Name n1, Name n2){
	// todo - we could optimize the string table around concatenations
	return getStringIndexConcat(str(n1),str(n2));
}
const char* getString(const Name& n) {
	return g_Names.index_to_name[index(n)].c_str();
}
Name getNumberIndex(int num){
	char tmp[32];sprintf(tmp,"%d",num); return g_Names.get_index(tmp,0,StringTable::Number);
}
bool is_number(Name n){
	return g_Names.flags[(int)n]&StringTable::Number;
}

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
void Node::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("(?)");
}
void Node::dump_top() const {
	if (!this) return;
	dbprintf("%s ", getString(name));
}

int get_typeparam_index(const vector<TypeParam*>& tps, Name name) {
	for (int i=(int)(tps.size()-1); i>=0; i--) {
		if (tps[i]->name==name)
			return i;
	}
	return -1;
}

ResolvedType assert_types_eq(int flags, const Node* n, const Type* a,const Type* b) {
	if (!n->pos.line){
		error(n,"AST node hasn't been setup properly");
	}
	ASSERT(a && b);
	// TODO: variadic args shouldn't get here:
	if (a->name==ELIPSIS||b->name==ELIPSIS)
		return ResolvedType(a,ResolvedType::COMPLETE);
	if (!a->eq(b)){
		if (!(flags & R_FINAL))
			return ResolvedType(0,ResolvedType::INCOMPLETE);

		n->dump(0);
		error_begin(n," type mismatch\n");
		warning(a->get_origin(),"from here:");
		a->dump(-1);
		warning(b->get_origin(),"vs here");
		b->dump(-1);
		error_end(n);
		return ResolvedType(a,ResolvedType::ERROR);
	}
	return ResolvedType(a,ResolvedType::COMPLETE);
}

void	ExprDef::remove_ref(Node* ref){
	Node** pp=&refs;
	Node* p;
	auto dbg=[&](){
		for (auto r=refs; r; r=r->next_of_def){
			dbprintf("ref by %p %s %s %d\n",r, r->kind_str(), str(r->name),r->	pos.line);
		}
	};
	for (p=*pp; p; pp=&p->next_of_def, p=*pp) {
		if (p==ref) *pp=p->next_of_def;
	}
	ref->def=0;
}


void Node::set_def(ExprDef *d){
	if (!def) {
		this->next_of_def=d->refs; d->refs=this;
		def=d;
	}
	else {
		if (d!=this->def){
			dbprintf("WARNING!!-was %d %s now %d %s\n",def->pos.line,def->name_str(), d->pos.line,d->name_str());
//			ASSERT(d==this->def);
		}
		def->remove_ref(this);
		def=d;
	}
}

ExprStructDef* Node::as_struct_def()const{
	//error(this,"expect struct def");
	return nullptr;
};
RegisterName Node::get_reg_existing(){ASSERT(reg_name); return reg_name;}
RegisterName Node::get_reg(CodeGen& cg, bool force_new){
	// variable might be in a local scope shadowing others, so it still needs a unique name
	// names are also modified by writes, for llvm SSA
	//ASSERT(!on_stack);
	if (!reg_name || force_new){
		auto old=reg_name;
		auto ret= get_reg_new(cg);
		return ret;
	} else{
		return reg_name;
	}
}
RegisterName Node::get_reg_new(CodeGen& cg) {
	return this->reg_name=cg.next_reg(name);
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
ResolvedType propogate_type(int flags,const Node*n, Type*& a,Type*& b) {
	verify(a,b);
	if (!(a || b))
		return ResolvedType(0,ResolvedType::INCOMPLETE);
	if (!a && b) {
		a=b;
		return ResolvedType(a,ResolvedType::COMPLETE);}
	else if (!b && a) {
		b=a;
		return ResolvedType(b,ResolvedType::COMPLETE);
	}
	return assert_types_eq(flags,n, a,b);
}
ResolvedType propogate_type(int flags, Expr *n, Type*& a,Type*& b) {
	verify(a,b);
	propogate_type(flags,(const Node*)n,a,b);
	propogate_type(flags,(const Node*)n,n->type_ref(),b);
	return propogate_type(flags,(const Node*)n,n->type_ref(),a);
}
ResolvedType propogate_type_fwd(int flags,const Node* n, const Type* a,Type*& b) {
	verify(a,b);
	if (!(a || b))
		return ResolvedType(0,ResolvedType::INCOMPLETE);
	if (!a && b){
		return ResolvedType(b,ResolvedType::INCOMPLETE);
	}
	if (!b && a) {
		b=(Type*)a;
		return ResolvedType(a,ResolvedType::COMPLETE);
	}
	return assert_types_eq(flags,n, a,b);
	
	return ResolvedType(b,ResolvedType::INCOMPLETE);
}
ResolvedType propogate_type_fwd(int flags,Expr* e, const Type*& a) {
	return propogate_type_fwd(flags,e, a, e->type_ref());
}
ResolvedType propogate_type(int flags,Expr* e, Type*& a) {
	return propogate_type(flags,e, a, e->type_ref());
}

ResolvedType propogate_type(int flags,const Node* n, Type*& a,Type*& b,Type*& c) {
	verify(a,b,c);
	int ret=ResolvedType::COMPLETE;
	ret|=propogate_type(flags,n,a,b).status;
	ret|=(c)?propogate_type(flags,n,b,c).status:ResolvedType::INCOMPLETE;
	ret|=(c)?propogate_type(flags,n,a,c).status:ResolvedType::INCOMPLETE;
	const Type* any=any_not_zero(a,any_not_zero(b,c));
	return ResolvedType(any,ret);
}
ResolvedType propogate_type_fwd(int flags,const Node* n,const Type*& a,Type*& b,Type*& c) {
	verify(a,b,c);
	int ret=ResolvedType::COMPLETE;
	ret|=propogate_type_fwd(flags,n,a,b).status;
	ret|=propogate_type_fwd(flags,n,a,c).status;
	ret|=propogate_type(flags,n,b,c).status;
	return ResolvedType(any_not_zero(a,any_not_zero(b,c)),ret);
}
ResolvedType propogate_type(int flags,const Node* n, ResolvedType& a,Type*& b) {
	verify(a.type,b);
	a.combine(propogate_type(flags,n, a.type,b));
	return a;
}
ResolvedType propogate_type(int flags,Expr* e, ResolvedType& a) {
	return propogate_type(flags,e, a, e->type_ref());
}

ResolvedType propogate_type(int flags,const Node* n,ResolvedType& a,Type*& b,const Type* c) {
	verify(a.type,b,c);
	a.combine(propogate_type_fwd(flags,n, c,b));
	a.combine(propogate_type(flags,n, a.type,b));
	return a;
}
ExprStructDef* dump_find_struct(Scope* s, Name name){
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
	lhs->find_vars_written_if(s,vs);
	rhs->find_vars_written_if(s,vs);
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

void ExprBlock::find_vars_written(Scope* s, set<Variable*>& vars) const{
	this->call_expr->find_vars_written_if(s, vars);
	for (auto a:argls)
		a->find_vars_written(s,vars);
}
void ExprIf::find_vars_written(Scope* s, set<Variable*>& vars) const{
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}
void ExprFor::find_vars_written(Scope* s, set<Variable*>& vars) const{
	incr->find_vars_written_if(s,vars);
	cond->find_vars_written_if(s,vars);
	body->find_vars_written_if(s,vars);
	else_block->find_vars_written_if(s,vars);
}
ResolvedType ExprIdent::resolve(Scope* scope,const Type* desired,int flags) {
	// todo: not if its' a typename,argname?
	if (this->is_placeholder()) {
		//PLACEHOLDER type can be anything asked by environment, but can't be compiled .
		propogate_type_fwd(flags,this, desired,this->type_ref());
		return ResolvedType(this->type_ref(),ResolvedType::COMPLETE);
	}
	
	propogate_type_fwd(flags,this, desired,this->type_ref());
	if (this->type()) this->type()->resolve(scope,desired,flags);
	if (auto sd=scope->find_struct_named(this->name)) {
		this->set_def(sd);
		if (!this->get_type()){
			this->set_type(new Type(sd));
			return propogate_type_fwd(flags,this, desired,this->type_ref());
		}
	} else
	if (auto v=scope->find_variable_rec(this->name)){ // look for scope variable..
		v->on_stack|=flags&R_PUT_ON_STACK;
		this->set_def(v);
		return propogate_type(flags,this, this->type_ref(),v->type_ref());
	}
	if (auto sd=scope->get_receiver()) {
		if (auto fi=sd->try_find_field(this->name)){
			this->set_def(fi);
		// anonymous struct fields are possible in local anon structs..
			return propogate_type(flags,this, this->type_ref(),fi->type_ref());
		}
	}
	if (auto f=scope->find_unique_fn_named(this,flags)){ // todo: filter using function type, because we'd be storing it as a callback frequently..
		// TODO; loose end :( in case where arguments are known, this overrides the match
		//we eitehr need to pass in arguemnt informatino here *aswell*, or seperaete out the callsite case properly.
		this->set_def(f);
		this->set_type(f->fn_type);
		return propogate_type_fwd(flags,this, desired, this->type_ref());
	}
	else if (!scope->find_named_items_rec(this->name)){
		// didn't find it yet, can't do anything
		if (flags & R_FINAL){
//			if (g_verbose){
//				g_pRoot->as_block()->scope->dump(0);
//			}
			scope->find_variable_rec(this->name); // look for scope variable..
			error(this,scope,"\'%s\' undeclared",str(this->name));
		}
		return ResolvedType();
	}
	return ResolvedType();
}
void ExprIdent::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("%s ",getString(name));
	if (this->def) {dbprintf("%d",this->def->pos.line);}
	if (auto t=this->get_type()) {dbprintf(":");t->dump(-1);}
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
bool Node::is_ident()const {
	return (dynamic_cast<const ExprIdent*>(this)!=0);
}

void ExprBlock::dump(int depth) const {
	if (!this) return;
	newline(depth);
	auto b=(this->bracket_type==OPEN_PAREN)?"(\0)\0":"{\0}\0";
	if (this->call_expr){
		dbprintf(this->is_subscript()?"subscript: ":this->is_struct_initializer()?"struct_init":"call ");
		this->call_expr->dump(-100);
//
	} else{
		dbprintf(this->is_array_initializer()?"array_init ":this->is_tuple()?"tuple ":"");
	}
	dbprintf(b+0);
	for (const auto x:this->argls) {
		if (x) {x->dump(depth+1);}else{dbprintf("(none)");}
	}
	newline(depth);dbprintf(b+2);
	if (this->get_type()){dbprintf(":");this->get_type()->dump_if(-1);}

}

void ExprOp::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("(");
	auto id=this->name;
	if (lhs) {lhs->dump(depth+1);}
	
	newline(depth);print_tok(id);

	if (rhs) {rhs->dump(depth+1);}
	newline(depth);dbprintf(")");
	if (get_type()) {dbprintf(":");get_type()->dump(-1);};
}

ExprBlock::ExprBlock(const SrcPos& s){ pos=s;}
ExprFnDef* ExprBlock::get_fn_call()const {
	if (!this->def)
		return nullptr;
	auto df=this->def;
	auto d=dynamic_cast<ExprFnDef*>(df);
	if (d)
		return d;
	return nullptr;
}
const char* Scope::name() const {
	if (owner_fn) return str(owner_fn->name);
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

ResolvedType Type::resolve(Scope* sc,const Type* desired,int flags)
{
	if(!this)return ResolvedType();
	if (!this->struct_def && this->name>=IDENT && !is_number(this->name)){
		if (auto sd=sc->find_struct_named(this->name)){
			this->struct_def =sd->get_instance(sc,this);
			dbprintf_instancing("found struct %s in %s ins%p on t %p\n",this->name_str(), sc->name(),this->struct_def,this);
		}else{
			dbprintf_instancing("failed to find struct %s in %s\n",this->name_str(), sc->name());
		}
	}
	auto ds=desired?desired->sub:nullptr;
	for (auto s=this->sub;s;s=s->next,ds=ds?ds->next:nullptr)
		s->resolve(sc,ds,flags);
	
	return ResolvedType(this,ResolvedType::COMPLETE);
}
ResolvedType ArgDef::resolve(Scope* sc, const Type* desired, int flags){
	dbprintf_resolve("resolving arg %s\n",this->name_str());
	propogate_type_fwd(flags,this,desired,this->type_ref());
	if (this->type()){
		this->type()->resolve(sc,desired,flags);
	}
	if (this->default_expr){this->default_expr->resolve(sc,this->type(),flags);}
	return ResolvedType(this->type(), ResolvedType::COMPLETE);
}

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
	struct_def=0;
	sub=0;
	next=0;
	name=i; //todo: resolve-type should happen here.
}
Type::Type(Node* origin,Name i){
	this->set_origin(origin);
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
	// if its' auto[...] match contents; if its plain auto, match anything.
	if (this &&this->name==AUTO){
		if (this->sub && other) return this->sub->eq(other->sub);
		else return true;
	}
	if (other && other->name==AUTO){
		if (other->sub && this) return other->sub->eq(this->sub);
		else return true;
	}
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
bool Type::eq(const Type* other,const TypeParamXlat& xlat) const{
	if ((!this) && (!other)) return true;
	// if its' auto[...] match contents; if its plain auto, match anything.
	if (this &&this->name==AUTO){
		if (this->sub && other) return this->sub->eq(other->sub,xlat);
		else return true;
	}
	if (other && other->name==AUTO){
		if (other->sub && this) return other->sub->eq(this->sub,xlat);
		else return true;
	}
	if (!(this && other)) return false;
	// TODO: might be more subtle than this for HKT
	auto ti=xlat.typeparam_index(other->name);
	dbprintf_type("%s %s\n",str(this->name),str(other->name));
	if (ti>=0){
		return this->eq(xlat.given_types[ti],xlat);
	}
	ti=xlat.typeparam_index(this->name);
	if (ti>=0){
		return xlat.given_types[ti]->eq(other,xlat);
	}

	if (this->name!=other->name)return false;
	//	if (!this->sub && other->sub)) return true;
	if (other->name==STR && type_compare(this,PTR,CHAR)) return true;
	if (this->name==STR && type_compare(other,PTR,CHAR)) return true;
	
	auto p=this->sub,o=other->sub;
	
	for (; p && o; p=p->next,o=o->next) {
		if (!p->eq(o,xlat)) return false;
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
#if DEBUG>=2
		if (this->struct_def)
			dbprintf(" %s", str(this->struct_def->get_mangled_name()));
#endif
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
Type* g_bool,*g_void;
Type* Type::get_bool(){
	/// todo type hash on inbuilt indices
	if (g_bool)return g_bool;
	return (g_bool=new Type(nullptr,BOOL));
}
Type* Type::get_void(){
	if (g_void)return g_void;
	return (g_bool=new Type(nullptr,VOID));
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
int Type::num_pointers_and_arrays() const {
	if (!this) return 0;
	if (this->name==PTR || this->name==REF|| this->name==ARRAY)
		return 1+this->sub->is_pointer();
	else return 0;
}
size_t Type::alignment() const{
	if (this->raw_type_flags()){
		return this->size();
	}
	size_t align=0;
	for (auto s=this->sub;s;s=s->next){
		if (auto sz=s->size()>align){align=sz;};
	}
	if (this->struct_def){
		return this->struct_def->alignment();
	}
	return align?align:4;
}

size_t Type::size() const{
	auto tf=this->raw_type_flags();
	if (tf){return tf&RT_SIZEMASK};
	auto union_size=[](const Type *t){
		size_t max_elem_size=0;
		for (auto s=t->sub; s;s=s->next){
			auto sz=s->size();
			if (sz>max_elem_size)max_elem_size=sz;
		}
		return max_elem_size;
	};
	if (this->name==UNION){
		return union_size(this);
	}
	if (this->name==VARIANT){
		auto align=this->alignment();
		return align+union_size(this);
	}
	if (this->name==TUPLE){
		int size=0;
		for (auto s=this->sub; s;s=s->next){
			size+=s->size();
		}
		return size;
	}
	if (this->struct_def){
		return struct_def->as_struct_def()->size();
	}
	return 0;
}

ExprStructDef* Type::get_struct()const{
	auto p=this;
	while (p && !p->is_struct()){
		p=p->sub;
	}
	if (!p) return nullptr;
	auto sd=p->struct_def;
	if (!sd) return nullptr;
	return sd->as_struct_def();
}
ExprStructDef* Type::get_receiver()const
{
	if (this->sub)
		if (this->sub->next)
			if (this->sub->next->next)
				return this->sub->next->next->struct_def->as_struct_def();
	return nullptr;
}

size_t ExprStructDef::size()const{
	size_t sum=0;
	for (auto i=0; i<fields.size();i++){
		sum+=fields[i]->size();
	}
	return sum;
}
void Type::dump(int depth)const{
	if (!this) return;
	newline(depth);dump_sub();
}
Type::Type(ExprStructDef* sd)
{	struct_def=sd; name=sd->name; sub=0; next=0;
}
Type::Type(Name outer_name,ExprStructDef* sd)
{
	name=outer_name;
	push_back(new Type(sd));
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
	if (auto t=this->type()){
		dbprintf(":");t->dump(-1);
	}
}
// TODO : 'type==type' in our type-engine
//	then we can just make function expressions for types.

void	ExprLiteral::translate_typeparams(const TypeParamXlat& tpx){
	
}

ResolvedType ExprLiteral::resolve(Scope* sc , const Type* desired,int flags){
	if (!this->owner_scope){
		this->next_of_scope=sc->global->literals;
		sc->global->literals=this;
		this->owner_scope=sc->global;
	}
	if (this->name==0){
		char str[256]; if (this->name==0){sprintf(str,"str%x",(uint)(size_t)this); this->name=getStringIndex(str);}
	}
	if (!this->get_type()) {
		Type* t=nullptr;
		switch (type_id) {
		case T_VOID: t=new Type(this,VOID); break;
		case T_INT: t=new Type(this,INT); break;
		case T_ZERO: t=new Type(this,ZERO); break;
		case T_ONE: t=new Type(this,ONE); break;
		case T_FLOAT: t=new Type(this,FLOAT); break;
		case T_CONST_STRING: t=new Type(this,STR); break;
		default: break;
		}
		this->set_type(t); // one time resolve event.
	}
	return propogate_type_fwd(flags,this, desired,this->type_ref());
}
size_t ExprLiteral::strlen() const{
	if (type_id==T_CONST_STRING)
		return ::strlen(this->u.val_str);
	else return 0;
}
ExprLiteral::ExprLiteral(const SrcPos& s) {
	pos=s;
	this->owner_scope=0;
	set_type(new Type(this,VOID));
	type_id=T_VOID;
}

ExprLiteral::ExprLiteral(const SrcPos& s,float f) {
	pos=s;
	this->owner_scope=0;
	set_type(new Type(this,FLOAT));
	type_id=T_FLOAT;
	u.val_float=f;
}
ExprLiteral::ExprLiteral(const SrcPos& s,int i) {
	pos=s;
	this->owner_scope=0;
	set_type(new Type(this,INT));
	type_id=T_INT;
	u.val_int=i;
}
ExprLiteral::ExprLiteral(const SrcPos& s,const char* start,int length) {//copy
	pos=s;
	this->owner_scope=0;
	set_type(new Type(this,STR));
	type_id=T_CONST_STRING;
	auto str=( char*)malloc(length+1); ;
	u.val_str=str;memcpy(str,(void*)start,length);
	str[length]=0;
}
ExprLiteral::ExprLiteral(const SrcPos& s,const char* src) {//take ownership
	pos=s;
	this->owner_scope=0;
	set_type(new Type(this,STR));
	u.val_str=src;
	type_id=T_CONST_STRING;
}
ExprLiteral::~ExprLiteral(){
	if (type_id==T_CONST_STRING) {
		free((void*)u.val_str);
	}
}
void dump_typeparams(const vector<TypeParam*>& ts) {
	bool a=false;
	if (ts.size()==0) return;
	dbprintf("[");
	for (auto t:ts){
		if (a)dbprintf(",");
		print_tok(t->name);if (t->defaultv){dbprintf("=");t->defaultv->dump(-1);}
		a=true;
	}
	dbprintf("]");
}
void Variable::dump(int depth) const{
	newline(depth);dbprintf("%s",getString(name));
	if (type()) {dbprintf(":");type()->dump(-1);}
	switch (this->kind){
		case VkArg:dbprintf("(Arg)");break;
		case Local:dbprintf("(%s Local)",this->on_stack?"stack":"reg");break;
		case Global:dbprintf("(Local)");break;
		default: break;
	}
}

void ArgDef::dump(int depth) const {
	newline(depth);dbprintf("%s",getString(name));
	if (this->type()) {dbprintf(":");type()->dump(-1);}
	if (default_expr) {dbprintf("=");default_expr->dump(-1);}
}
Node*	TypeParam::clone() const
{	return new TypeParam(this->name, (Type*) (this->defaultv?this->defaultv->clone():nullptr));
}

void TypeParam::dump(int depth) const {
	newline(depth);dbprintf("%s",str(name));
	if (defaultv) {dbprintf("=");defaultv->dump(-1);}
}
const char* ArgDef::kind_str()const{return"arg_def";}

// the operators should all just be functioncalls, really.
// return type of function definition is of course a function object.
// if we make these things inline, we create Lambdas
// todo: receivers.

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
bool ExprBlock::is_undefined() const{
	if (!this) return false; //only presencence of "_" is undefined.
	for (auto x:argls){
		if (x->is_undefined())
			return true;
	}
	return false;
}

void ExprFnDef::set_receiver(ExprStructDef* r){
	///TODO: ambiguity between special receiver and '1st parameter'.
	/// we started coding a 'special receiver'
	/// however UFCS means a generalized '1st parameter' makes more sense,
	/// and it's less intrusive to retrofit.
	/// we might also try 'multiple receivers' for nested classes?
	/// eg struct Scene { struct Model{  methods of model get Scene*, Model*... }}
	/// .. and we want to implement lambdas as sugar for callable objects much like c++/rust trait reform
	this->m_receiver=r;
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

ExprFnDef* NamedItems::getByName(Name n){
	for(auto f=this->fn_defs;f;f=f->next_of_name){
		if (f->name==n)
			return f;
	}
	return nullptr;
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
	dbprintf_generic("instantiating %s %d for call %s %d\n",str(name),srcfn->pos.line, callsite->name_str(),callsite->pos.line);
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
	new_fn->body->set_type(nullptr);// todo, inference upward..
	new_fn->next_instance = srcfn->instances;
	srcfn->instances=new_fn;
	new_fn->instance_of = srcfn;
	new_fn->resolved=false;
	new_fn->resolve(src_fn_owner,return_type,flags);//todo: we can use output type ininstantiation too
//	new_fn->dump(0);
	new_fn->resolve(src_fn_owner,return_type,flags);//todo: we can use output type
#if DEBUG >=2
	dbprintf_fnmatch("%s return type=\n",new_fn->name_str());
	srcfn->type()->dump_if(-1);
	dbprintf_fnmatch(" from ");
	new_fn->type()->dump_if(-1);
	dbprintf_fnmatch("\n");
	new_fn->fn_type->dump_if(-1);
	dbprintf_fnmatch("\nlast expression:");
	new_fn->body->as_block()->argls.back()->dump(0);
	dbprintf_fnmatch("\nlast expression type:");
	new_fn->body->as_block()->argls.back()->type()->dump(0);
	dbprintf_fnmatch("\n");
#endif
	

	verify_all();
	return new_fn;	// welcome new function!
}
Node* ExprOp::clone() const {
	return (Node*) new ExprOp(this->name,this->pos, (Expr*) this->lhs->clone_if(), (Expr*) this->rhs->clone_if());
}
Node* ExprBlock::clone() const {
	if (!this) return nullptr;
	auto r=new ExprBlock(this->pos);
	r->bracket_type=this->bracket_type;
	r->delimiter=this->delimiter;
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
	return (Node*)r;
}
Node*
ExprLiteral::clone() const{
	return (Node*)this;	// TODO - ensure this doesn't get into dangling state or anything!
/*	if (!this) return nullptr;
	auto r=new ExprLiteral(0); if (this->is_string()){r->u.val_str=strdup(this->u.val_str);}else r->u=this->u;
	r->type_id=this->type_id;
	r->llvm_strlen=this->llvm_strlen; // TODO this should just be a reference!?
	r->name=this->name;
	return r;
*/
}
Node*
ExprFnDef::clone() const{
	if (!this) return nullptr;
	auto r=new ExprFnDef(this->pos);
	r->name=this->name;
	r->c_linkage=false; //generic instance is not extern C.
	r->body=(ExprBlock*)(this->body?this->body->clone():nullptr);
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
Name ExprStructDef::get_mangled_name()const{
	if (!mangled_name){
		char buffer[1024];
		name_mangle(buffer,1024,this);
		const_cast<ExprStructDef*>(this)->mangled_name=getStringIndex(buffer);
	}
	return this->mangled_name;
}

Node*
ArgDef::clone() const{
	if (!this) return nullptr;
	return new ArgDef(this->pos,this->name, (Type*)this->type()->clone_if(),(Expr*)this->default_expr->clone_if());
}
Node*
Type::clone() const{
	if (!this) return nullptr;
	auto r= new Type(this->get_origin(),this->name);
	r->struct_def=this->struct_def;
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

int match_typeparams_from_arg(vector<Type*>& matched_tps, const vector<TypeParam*>& fn_tps,  const Type* fn_arg, const Type* given_arg)
{
	int ret_score=0;
//	if (!fn_arg && !given_arg) return 0;
	// if either is unspecified.. match anything there
	// TODO:
	if (!fn_arg) return 0;
	if (!given_arg) return 0;
	
	dbprintf_fnmatch("%s/s ",str(fn_arg->name),str(given_arg->name));
	if (fn_arg->sub || given_arg->sub) {
		dbprintf_fnmatch("[");
		for (const Type* sub1=fn_arg->sub,*sub2=given_arg->sub; sub1&&sub2; sub1=sub1->	next,sub2=sub2->next) {
			ret_score+=match_typeparams_from_arg(matched_tps,fn_tps, sub1, sub2);
		}
		dbprintf_fnmatch("]");
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
		else if (!(matched_tps[ti]->eq(given_arg))) {// or we already found it - match..
//			dbprintf_fnmatch("match %s !=%s\n",str(fn_arg->name),str(given_arg->name));
#if DEBUG>=3
			matched_tps[ti]->dump(-1);
			given_arg->dump(-1);
#endif
			return ret_score-1000;
		}
	} else {
		// concrete types - compare absolutely
		if (fn_arg->name != given_arg->name) {
			dbprintf_fnmatch("\nmatch %s !=%s\n",str(fn_arg->name),str(given_arg->name));
			return ret_score-1000;	// mismatch is instant fail for this candidate.
		}
	}
	return ret_score;
}

int match_typeparams(vector<Type*>& matched, const ExprFnDef* f, const ExprBlock* callsite){
	// TODO: allow the types to feedback in the math
	matched.resize(f->typeparams.size());
	int score=0;
#if DEBUG>=2
	callsite->dump(0);newline(0);
#endif
	for (int i=0; i<f->typeparams.size();i++) matched[i]=0;
	for (int i=0; i<callsite->argls.size(); i++) {
#if DEBUG>=2
		f->args[i]->type()->dump_if(-1); newline(0);
		callsite->argls[i]->type()->dump_if(-1); newline(0);
#endif
		score+=match_typeparams_from_arg(matched,f->typeparams, f->args[i]->type(), callsite->argls[i]->type());
	}
	score+=match_typeparams_from_arg(matched, f->typeparams, f->ret_type, callsite->type());
	dbprintf_fnmatch("score matching gets %d\n",score);
	return score;
}

struct FindFunction {
	struct Candidate{ExprFnDef* f; int score;};
	vector<Candidate> candidates;
	Name			name;
	const Expr* 	callsite;
	int 			flags;
	bool 			verbose=false;
	int				max_candidates=5;
	const vector<Expr*>& args;
	const Type* 	ret_type;
	FindFunction(Name n, const vector<Expr*>& a, const Type* r,int f):name(n),args(a),ret_type(r),flags(f){}

	void consider_candidate(ExprFnDef* f);
	void find_fn_sub(Expr* src);
	void find_fn_from_scopes(Scope* s,Scope* ex);
	void insert_candidate(ExprFnDef* f,int score);
	void dump(){
		for (int i=0; i<candidates.size();i++){
			dbprintf("candidate %d for %s: %d %p score=%d\n",i, str(name),candidates[i].f->pos.line, candidates[i].f->instance_of, candidates[i].score);
		}
	}
};
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
	dbprintf_fnmatch("consider candidate %d %s\n",f->pos.line,str(f->name));
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
	// TODO: may need to continually update the function match, eg during resolving, as more types are found, more specific peices may appear?
	// Find max number of matching arguments
	
	verify_all();
	vector<Type*> matched_type_params;
	for (int i=0; i<f->typeparams.size(); i++){matched_type_params.push_back(nullptr);}

	int score=0;
	// +1 for any matching arg type regardless of placement,bonus if aprox right order
	for (int i=0; i<args.size(); i++) {
		auto at=args[i]->get_type(); if (!at) continue;
		for (int jj=i; jj<f->args.size(); jj++) {
			auto j=jj%args.size();
			if (f->args[j]->get_type()->eq(at)){
				score++;
				break;
				if (j==i) score+=10*(1+args.size()-i); // exact positional match scores higher earlier
			}
		}
	}
	if (name==PLACEHOLDER){
//		score-=1000;
	}
	else if (!f->is_enough_args((int)args.size()) || f->too_many_args((int)args.size())){
		score-=1000;
		insert_candidate(f,score);
		return;
	}

	if (f->variadic && args.size()> f->args.size())
		score=1;	// variadic functoin can match anything?
	if (!f->typeparams.size())
	for (int i=0; i<args.size() && i<f->args.size(); i++) {
		if (!f->args[i]->get_type() || (!args[i])) {
			score++; //1 point for an 'any' arg on either side
		} else{
			// both args are given:
			if (f->args[i]->get_type()->eq(args[i]->get_type())) {
				score+=10;// 1 exact match worth more than any number of anys
			} else{
				//if (!is_generic_type(f->typeparams,f->args[i]->get_type())
				
				{
				// instant fail for incorrect concrete arg
				//TODO consider conversion operators here.
					score-=1000;
				//	if (candidates.size()>=4)
				//		return;
				}
			}
		}
	}
	// find generic typeparams..
	if (f->typeparams.size()){
#if DEBUG>=2
		dbprintf_fnmatch("%s score=%d; before typaram match\n",str(f->name),score);
		dbprintf_fnmatch("callsite:\n");
		for (int i=0; i<callsite->as_block()->argls.size();i++) {
			dbprintf_fnmatch("arg %s:",  str(args[i]->name));
			callsite->as_block()->argls[i]->type()->dump_if(-1);
			dbprintf_fnmatch("\tvs\t");
			f->args[i]->type()->dump_if(-1);
			dbprintf_fnmatch("\n");
		}
		dbprintf_fnmatch("\n");
#endif
		for (int i=0; i<args.size() && i<f->args.size(); i++) {
			score+=match_typeparams_from_arg(matched_type_params,f->typeparams, f->args[i]->get_type(), args[i]->get_type());
		}
		score+=match_typeparams_from_arg(matched_type_params, f->typeparams,f->ret_type,ret_type);
		dbprintf_fnmatch("typaram matcher for %s\n",f->name_str());
		dbprintf_fnmatch("%s:%d: %s\n",g_filename,f->pos.line,str(f->name));
		dbprintf_fnmatch("%s score=%d; matched typeparams{:-\n",str(f->name),score);
		for (auto i=0; i<f->typeparams.size(); i++){
			dbprintf_fnmatch("[%d]%s = %s;\n", i,str(f->typeparams[i]->name),matched_type_params[i]?str(matched_type_params[i]->name):"" );
			}
		dbprintf_fnmatch("}\n");
		dbprintf_fnmatch("\n");
		
	}
	verify_all();
	// fill any unmatched with defaults?

	// consider return type in call.
	if (ret_type)
		if (f->get_type()->eq(ret_type)) score+=100;

	if (f->name==name) score*=100; // 'named' functions always win over un-named forms eg F[F,X](a:X),we may use unnamed to implement OOP..
	// insert candidate
	verify_all();
	insert_candidate(f,score);
	verify_all();
}
void FindFunction::find_fn_sub(Expr* src) {
	verify_all();
	if (auto sb=dynamic_cast<ExprBlock*>(src)) {
		for (auto x:sb->argls) {
			find_fn_sub(x);
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(src)){
		if (verbose)
			dbprintf_fnmatch("consider %s %d for %s %d\n",f->name_str(),f->pos.line, this->callsite->name_str(),this->callsite->pos.line);
		consider_candidate(f);
		int i=0;
		for (auto ins=f->instances; ins; ins=ins->next_instance,i++) {
			dbprintf_fnmatch("%s ins=%d\n",f->name_str(),i);
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

void dbprint_find(const vector<ArgDef*>& args){
	dbprintf("\n;find call with args(");
	for (int i=0; i<args.size(); i++) {dbprintf(" %d:",i);dbprintf("%p\n",args[i]);if (args[i]->get_type()) args[i]->get_type()->dump(-1);}
	dbprintf(")\n");
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
			if (auto sd=rec_t->get_struct()){
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
			error_begin(callsite,(name!=PLACEHOLDER)?"unmatched call %s":"possible calls for %s",str(name));
			// For the best match, say what you'd have to do to fix, then show all matches
			if (args.size()<best.f->min_args()){info(best.f,"maybe requires %d args, %d given",best.f->min_args(),args.size());}
			vector<Type*> callsite_tys;
			match_typeparams(callsite_tys, best.f,callsite->as_block());
			auto tpxlat=TypeParamXlat{best.f->typeparams,callsite_tys};
			for (auto i=0; i<args.size() && i<best.f->args.size(); i++){

				if (!args[i]->type()->eq(best.f->args[i]->type(),tpxlat)){
					info(best.f->args[i],"maybe arg %d should be ",i); best.f->args[i]->type()->dump_if(-1);
					info(args[i],"was given "); args[i]->type()->dump_if(-1);newline(0);
					tpxlat.dump();
					info(args[i],"\n");
					break;
				}
			}       
			info(callsite,"%s(",str(name));
			for (auto i=0; i<args.size(); i++){	if (i)dbprintf(",");dbprintf("",i); args[i]->type()->dump(-1); }
			dbprintf(")");
			if (candidates.size()>1)info(callsite,"see candidates:-");
			for (auto i=(int)ff.candidates.size()-1; i>=0; i--) {
				auto &c=ff.candidates[i];
				info(c.f," ");c.f->dump_signature();
				
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
	dbprintf_instancing("adding struct %p %s ins of %p to %s\n",sd,sd->name_str(),sd->instance_of,this->name());
	if (sd->name_ptr)
		return;
	if (sd->instance_of){
		return add_struct(sd->instance_of);
	}
	auto ni=get_named_items_local(sd->name);
	sd->name_ptr=ni;
	sd->next_of_name=ni->structs;
	ni->structs=sd;
}
Variable* Scope::find_scope_variable(Name name){
	for (auto v=this->vars; v;v=v->next_of_scope){
		if (v->name==name) return v;
	}
	return nullptr;
}
Variable* Scope::find_variable_rec(Name name){
	dbprintf_varscope("find variable %s in %s\n",str(name),this->name());
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
			dbprintf_varscope("Found Captured Var %s in %s %s\n",str(name),this->name(),str(v->capture_in->name));
			return v;
		}
	}
	dbprintf_varscope("Trying to capture %s in %s\n",str(name),this->name());
	dbprintf_varscope(" from %s\n",this->capture_from->name());
	if (auto ofn=dynamic_cast<ExprFnDef*>(this->owner_fn)){
		auto v=capture_from->find_variable_rec(name);
		if (v) {
			dbprintf_varscope("capture: found %s in %s\n",str(name),this->capture_from->name());
			auto cp=ofn->get_or_create_capture(this->capture_from->owner_fn->as_fn_def());
			if (v->capture_in==0){
				v->capture_in=cp; v->next_of_capture=cp->vars; cp->vars=v;
				dbprintf_varscope("%s captured by %s from %s\n",str(name),this->name(),capture_from->name());
				return v;
			}
			else if (v->capture_in!=cp) {
				dbprintf_varscope("var %s already captured by %s, coalesce with %s\n",str(name),str(v->capture_in->capture_by->name),this->name());
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
	newline(depth);dbprintf("scope: %s {",  this->name());
	for (auto v=this->vars; v; v=v->next_of_scope) {
		newline(depth+1); dbprintf("var %d %s:",index(v->name), getString(v->name));
		if (auto t=v->get_type()) t->dump(-1);
	}
	for (auto ni=this->named_items; ni;ni=ni->next){
		newline(depth+1); dbprintf("name %s:",getString(ni->name));
		for (auto fnd=ni->fn_defs; fnd;fnd=fnd->next_of_name){
			newline(depth+1);dbprintf("fn %s\n",getString(fnd->name));
		}
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

ResolvedType ExprOp::resolve(Scope* sc, const Type* desired,int flags) {
	verify_all();
	Type* ret=0;
	auto op_ident=name;
	if (this->type()) this->type()->resolve(sc,desired,flags);
//	if (flags) {ASSERT(lhs->def) ;ASSERT(rhs->def);}
	if (op_ident==BREAK){
		if (this->rhs) {
			// break expression..
			rhs->resolve(sc,desired,flags);
			auto loop = sc->current_loop();
			if (!loop && flags&R_FINAL) {
				error(this,"break without loop");
			}
			propogate_type(flags,(Node*)this, rhs->type_ref(),loop->type_ref());
			propogate_type(flags,(Node*)this,this->type_ref(),this->rhs->type_ref());
		}
		return propogate_type_fwd(flags, this, desired, this->type_ref());
	}

	if (op_ident==ASSIGN || op_ident==LET_ASSIGN || op_ident==ASSIGN_COLON) {
		ASSERT(this->lhs && this->rhs);
		auto rhs_t=rhs->resolve(sc,desired,flags);
		if (op_ident==LET_ASSIGN){
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			if (desired) {
				desired->dump(-1);
			}
			auto rhs_t = rhs->get_type();
			auto new_var=sc->create_variable(this,vname,Local);
			lhs->set_def(new_var);
			new_var->set_type(rhs_t);
			lhs->set_type(rhs_t);
			this->set_type(rhs_t);
			propogate_type_fwd(flags, this, desired, lhs->type_ref());
			return 	propogate_type_fwd(flags, this, desired, this->type_ref());
		}
		else if (op_ident==ASSIGN_COLON){ // create a var, of given type.
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			// todo: get this in the main parser
			auto lhsi=expect_cast<ExprIdent>(lhs);
			auto rhst=expect_cast<Type>(rhs);
			//			auto v=sc->find_variable_rec(this->argls[0]->name);
			auto v=sc->get_or_create_scope_variable(this,lhsi->name,Local);
			v->set_type(rhst);
			lhs->set_def(v);
			if (rhst->name>=IDENT && !rhst->sub) {
				rhst->struct_def = sc->find_struct(rhst);
			}
			if (auto t=v->get_type()) {
				sc->try_find_struct(t);// instantiate
			}
			return propogate_type(flags, this, v->type_ref(),type_ref());
		}
		else if (op_ident==ASSIGN){
			propogate_type_fwd(flags,this, desired, type_ref());
			auto rhs_t=rhs->resolve(sc,desired,flags);
			auto lhs_t=lhs->resolve(sc,desired,flags);		// might assign to struct-field, ...
			propogate_type(flags,this, rhs->type_ref(), lhs->type_ref());
			propogate_type(flags,this, type_ref(),rhs->type_ref());
			return propogate_type(flags, this, type_ref(),lhs->type_ref());
		} else{
			ASSERT(0);
			return ResolvedType();
		}
	}
	else if (op_ident==COLON){ // TYPE ASSERTION
		// todo: get this in the main parser
		ASSERT(dynamic_cast<ExprIdent*>(rhs)); // todo- not just that
		auto tname=rhs->name;
		auto v=sc->find_variable_rec(lhs->name);
		if (!v->get_type()){
			v->set_type((const Type*)rhs);
			ASSERT(dynamic_cast<Type*>(rhs))
		}
		lhs->set_def(v);
		return propogate_type(flags, this, v->type_ref(),type_ref());
	} else if (op_ident==AS){
		this->lhs->resolve(sc,nullptr,flags);
		if (this->rhs->name==PLACEHOLDER) {
			this->rhs->set_type(desired);
			this->set_type(desired);
			return ResolvedType(this->type(),ResolvedType::COMPLETE);
		} else {
			this->set_type(this->rhs->type());
			return propogate_type_fwd(flags,this,desired);
		}
	}
	else if (op_ident==DOT || op_ident==ARROW) {
		auto lhs_t=lhs->resolve(sc, 0,flags);//type doesn't push up- the only info we have is what field it needs
		auto t=lhs_t.type;
//		dbprintf("resolve %s.%s   lhs:",getString(lhs->name),getString(rhs->name));if (t) t->dump(-1);dbprintf("\n");
	
		// TODO: assert that lhs is a pointer or struct? we could be really subtle here..
		verify_all();
		if (t) {
			t=t->deref_all();
			// now we have the elem..
			verify_expr_op(this);
//			verify_expr_ident(rhs);
//			ASSERT(rhs->as_ident());
			if (auto field_name=rhs->as_ident()){
				if (auto st=sc->find_struct_of(lhs)){
					if (auto f=st->find_field(rhs)){
						ret=f->type();
						return propogate_type(flags,this, ret,this->type_ref());
					}
				}
			} else if (auto call=rhs->as_block()){
				auto method_name=call->call_expr->name;
				// really we want to desugar this, a.foo(b) is just foo(a,b)
				// but we respect the shape of the AST?
//				dbprintf("method call: %s\n",str(method_name));
				call->resolve_sub(sc, desired, flags, lhs);
				return propogate_type(flags,this,call->type(),this->type_ref());
			} else {
				if (flags & R_FINAL){
					error(this,"dot operator not call or field acess", t->name_str());
					error(this,"cant find struct %s", t->name_str());
				}
			}
		}
		verify_all();
		return ResolvedType(this->type(),ResolvedType::INCOMPLETE);
	}
	else if (op_ident==ADDR){  //result=&lhs
		// todo: we can assert give type is one less pointer, if given
		ASSERT(!lhs && rhs);
		Type* dt=nullptr;
		if (desired){
			if (!(desired->name==PTR)) {
				error_begin(this,"taking adress, infered output isn't a ptr\n");
				warning(desired->get_origin(),"infered from here: ");
				desired->dump(-1);error_newline();
				error_end(this);
			}
			dt=desired->sub;
		}
		auto ret=rhs->resolve(sc,dt,flags|R_PUT_ON_STACK);
		if (!this->get_type() && ret.type){
			auto ptr_type=new Type(this,PTR,(Type*)ret.type->clone());
			this->set_type(ptr_type);
			return propogate_type_fwd(flags,this, desired,ptr_type);
		}
		return ret;
	}
	else if (op_ident==DEREF){ //result=*rhs
		// todo: we can assert give type is one less pointer, if given
		auto ret=rhs->resolve(sc,0,flags);
		// todo: its' a typeparam constraint.  ptr[desired]==argls[0]
		if (!this->get_type() && ret.type){
//			if (ret.type->name!=PTR) {
//				this->dump(0);
//				rhs->dump(0);
//				ret.type->dump(0);
//			}
//			ASSERT(ret.type->name==PTR);
			if (ret.type->name!=PTR ){
				if (flags & R_FINAL){
					error_begin(this,"pointer expected for deref\n");
					rhs->type()->dump(-1);
					error_end(this);
				}
			} else {
				this->set_type(ret.type->sub);
			}
			return propogate_type_fwd(flags,this, desired, this->type_ref());
		}
		else return ResolvedType();
	}
	else if (op_ident==NEW ){
		// new struct initializer ->  malloc(sizeof(struct)); codegen struct initializer 'inplace'; ret is ptr[S]
		// can we generalize this:
		//  ident{expr,..} actually means run the init expr in there, like 'with'
		/// todo: generalize inference with wrapper , eg A vs X[B]. use for *t, &t, new t, t[i]
		auto b=rhs->as_block();
		if (!b && flags){
			error(b,"new type[n] or new type{..} expected");
		}
		if (desired && !get_type()){
			this->set_type(desired);
		}
		if (!desired && !get_type() && rhs->get_type()) {
			this->set_type( new Type(this,PTR,(Type*)b->get_type()->clone()) );
		}
		if (get_type())
			propogate_type(flags, (Node*)this, this->get_type()->sub, b->type_ref());
		
		if (rhs->is_subscript()){
			b->call_expr->resolve(sc,get_type()?get_type()->sub:nullptr,flags);
			b->set_type(b->call_expr->get_type());
		}
		else {
			b->resolve(sc, desired?desired->sub:nullptr, flags);
		
		}

		return propogate_type_fwd(flags,this, desired, this->type_ref());
	}
	else if (op_ident==DELETE ){
		rhs->resolve(sc,nullptr,flags);
		return ResolvedType();
	}
	else if (is_condition(op_ident)){
		auto lhst=lhs->resolve(sc,rhs->type_ref(),flags); // comparisions take the same type on lhs/rhs
		auto rhst=rhs->resolve(sc,lhs->type_ref(),flags);
		propogate_type(flags,(Node*)this, lhs->type_ref(),rhs->type_ref());
		::verify(lhs->get_type());
		::verify(rhs->get_type());
		if (!this->get_type()){
			this->set_type(new Type(this,BOOL));
		};
		// TODO: actually we want to ensure the result *converts* to bool
		// compares might not return bool, they just need an operator(bool)
		return propogate_type_fwd(flags,this,desired,this->type_ref());
	}
	else {
		// regular operator
		// TODO propogate types for pointer-arithmetic - ptr+int->ptr   int+ptr->ptr  ptr-ptr->int
		// defaults to same types all round.
		auto lhst=lhs->resolve(sc,desired,flags);
		auto rhst=rhs->resolve(sc,desired,flags&!R_PUT_ON_STACK);
		propogate_type(flags,this, lhst,type_ref());
		propogate_type(flags,this, rhst,type_ref());
		return propogate_type_fwd(flags,this, desired, type_ref());
	}
}
ResolvedType ExprBlock::resolve(Scope* sc, const Type* desired, int flags) {
	return this->resolve_sub(sc,desired,flags,nullptr);
}
ResolvedType ExprBlock::resolve_sub(Scope* sc, const Type* desired, int flags,Expr* receiver) {
	verify_all();
	if (this->type()) this->type()->resolve(sc,nullptr,flags);

	/// loose end? if this is a method-call, we dont resolve the symbol here,
	/// in other contexts we do
	if (this->call_expr &&!receiver) this->call_expr->resolve(sc,nullptr,flags);
	::verify(this->get_type());
	if (this->argls.size()<=0 && this->is_compound_expression() ) {
		if (!this->get_type()) this->set_type(new Type(this,VOID));
		return propogate_type_fwd(flags,this, desired,this->type_ref());
	}
	ExprIdent* p=nullptr;
	if (this->is_compound_expression()) {	// do executes each expr, returns last ..
		auto n=0;
		for (; n<this->argls.size()-1; n++) {
			this->argls[n]->resolve(sc,0,flags);
		}
		// last expression - type bounce. The final expression is a return value, use 'desired';
		// we then propogate backwards. some variables will have been set, eg return value accumulator..
		if (this->argls.size()) {
			propogate_type_fwd(flags,this, desired);
			auto ret=this->argls[n]->resolve(sc,desired,flags);
			// reverse pass too
			for (n=(int)this->argls.size()-1;n>=0; n--) {
				this->argls[n]->resolve(sc,0,flags);
			}
			if ((flags & R_FINAL) &&ret.type && desired  &&this->argls.back()->type()){
				if (!ret.type->eq(desired)){
					newline(0);
					dbprintf("mismattched types..\n",n);
				ret.type->dump(-1); newline(0); desired->dump(-1);newline(0);
				this->argls.back()->type()->dump(0);
				dbprintf("n=%d",n);
				this->argls.back()->dump(0);
				newline(0);
				auto ret1=this->argls.back()->resolve(sc,desired,flags);
				}
			}
			propogate_type(flags, this, this->argls.back()->type_ref());
#if DEBUG>=2
			this->type()->dump_if(-1);
			this->argls.back()->dump_if(-1);
			newline(0);
#endif
			return propogate_type(flags,this, ret);
		}
		else {ASSERT(0);return ResolvedType();}
	}
	else if (this->is_subscript()) {
		// array indexing operator TODO: check this isn't itself a Type, if we want templates anywhere.
		auto array_type=this->call_expr->resolve(sc,nullptr,flags); // todo - it could be _[desired]. forward should give possibilities
		if (array_type.type){
			ASSERT(array_type.type->is_array()||array_type.type->is_pointer());
			for (auto i=0; i<argls.size(); i++)  {
				argls[i]->resolve(sc,nullptr,flags&!R_PUT_ON_STACK ); // TODO any indexing type? any type extracted from 'array' ?
			}
			const Type* array_elem_type=array_type.type->sub;
			propogate_type_fwd(flags,this, array_elem_type);
			return propogate_type_fwd(flags,this, desired);
		} else return ResolvedType();
	} else if (this->is_struct_initializer()){
		auto si=StructInitializer(sc,this);
		return si.resolve(desired,flags);
	}
	else if (this->call_expr){
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
			this->argls[0]->resolve(sc,nullptr,0);
			this->set_type(src->get_type());
			return ResolvedType();
		}
		bool indirect_call=false;
		auto call_ident=dynamic_cast<ExprIdent*>(this->call_expr);
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
			fn_type=this->call_expr->resolve(sc,nullptr,flags).type;
//			fn_type_r=this->call_expr->resolve(sc,nullptr,flags);
//		} else {
//			fn_type_r=this->
		}
//		auto fn_type=indirect_call?nullptr:fn_type_r.type;
		
		int arg_index=0;
		if (fn_type) {
			// propogate types we have into argument expressions
			for (auto a=fn_type->fn_args(); arg_index<argls.size() && a; arg_index++,a=a->next)  {
				if (a->name==FN){
					dbprintf_lambdas("resolving fn type into function argument %s\n", argls[arg_index]->name_str());
				}
				argls[arg_index]->resolve(sc,a,flags);
			}
			for (;arg_index<argls.size(); arg_index++){ // variadic args.
				argls[arg_index]->resolve(sc,nullptr,flags);
			}
			const Type* fr=fn_type->fn_return();
			propogate_type_fwd(flags,this, fr);
		} else
		for (auto i=0; i<argls.size(); i++)  {
			argls[i]->resolve(sc,nullptr,flags );
		}

		if (!this->get_fn_call()){
			
			for (auto i=0; i<argls.size(); i++)  {
				argls[i]->resolve(sc,nullptr ,flags);
			}
			if (this->call_expr->is_ident() && 0==dynamic_cast<Variable*>(this->call_expr->def)){
				return resolve_make_fn_call(receiver,this, sc,desired,flags);
			}
		} else if (auto fnc=this->get_fn_call()){ // static call
			int ofs=(receiver)?1:0;
			if (receiver)
				receiver->resolve(sc,fnc->args[0]->type(),flags);
			for (auto srci=0; srci<argls.size(); srci++)  {
				int i=srci+ofs;
				auto fnarg=i<fnc->args.size()?fnc->args[i]:nullptr;
				argls[i]->resolve(sc,fnarg?fnarg->type():nullptr ,flags);
			}
			return propogate_type_fwd(flags,this, desired,this->get_fn_call()->ret_type);
		} else {
			if (flags & R_FINAL)
				if (!this->type())
					error(this,"can't call/ type check failed %s",this->call_expr->name_str());

			return ResolvedType();
		}
	}
	return ResolvedType();
}

ResolvedType StructInitializer::resolve(const Type* desiredType,int flags) {

	auto sd=sc->find_struct(si->call_expr);
	if (!sd){
		if (flags&R_FINAL){
			error(si->call_expr,"can't find struct %s",si->call_expr->name_str());
		}
		return ResolvedType();
	}
	auto local_struct_def=dynamic_cast<ExprStructDef*>(si->call_expr);
	if (local_struct_def)sc->add_struct(local_struct_def); // todo - why did we need this?
	if (!si->type()){
		si->set_type(new Type(sd));
	}
	si->call_expr->def=sd;
	si->def=sd;
	// assignment forms are expected eg MyStruct{x=...,y=...,z=...} .. or can we have MyStruct{expr0,expr1..} equally?
	//int next_field_index=0;
	// todo:infer generic typeparams - adapt code for functioncall. we have struct fields & struct type-params & given expressions.
	int named_field_index=-1;
	// todo encapsulate StructInitializer to reuse logic for codegen
	field_indices.reserve(si->argls.size());
	int field_index=0;
	for (auto i=0; i<si->argls.size(); i++)  {
		auto a=si->argls[i];
		auto op=dynamic_cast<ExprOp*>(a);
		ArgDef* field=nullptr;
		Type*t = nullptr;
		if (op&&(op->name==ASSIGN||op->name==COLON||op->name==LET_ASSIGN)){
			field=sd->find_field(op->lhs);
			op->rhs->resolve(sc,field->type(),flags); // todo, need type params fwd here!
			propogate_type(flags,op,op->lhs->type_ref(),op->rhs->type_ref());
			//				propogate_type(flags,op,op->rhs->type_ref());
			op->lhs->def=field;
			named_field_index=sd->field_index(op->lhs);
			this->value.push_back(op->rhs);
			t=op->rhs->type();
		}else if (named_field_index==-1){
			if (field_index>=sd->fields.size()){
				error(a,sd,"too many fields");
			}
			field=sd->fields[field_index++];
			this->value.push_back(a);
			a->resolve(sc,field->type(),flags); // todo, need generics!
			t=a->type();
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
	
	return propogate_type_fwd(flags,si, desiredType);
}


ResolvedType resolve_make_fn_call(Expr* receiver,ExprBlock* block/*caller*/,Scope* scope,const Type* desired,int flags) {
	verify_all();

	int num_resolved_args=0;
	for (int i=0; i<block->argls.size(); i++) {
		block->argls[i]->resolve(scope,nullptr,flags);
		if (block->argls[i]->type()) num_resolved_args++;
	}
	if (receiver){
		receiver->resolve(scope,nullptr,flags); if (receiver->type()) num_resolved_args++;
	}
	
	if (block->get_fn_call() && num_resolved_args==block->argls.size())
		return ResolvedType(block->get_fn_call()->ret_type,ResolvedType::COMPLETE);

//	11ASSERT(block->call_target==0);
	// is it just an array access.
	if (block->is_subscript()){
		auto object_t=block->call_expr->resolve(scope,nullptr,flags);
		auto index_t=block->argls[0]->resolve(scope,nullptr,flags);
		//find the method "index(t,index_t)"
		// for the minute, just assume its' an array
		// TODO: operator overload.

		if (object_t.type && index_t.type) {
			ASSERT(object_t.type->is_array() || object_t.type->is_pointer());
			return ResolvedType(object_t.type->sub,ResolvedType::COMPLETE);
		}
		return ResolvedType();
	}
	verify_all();

	vector<Expr*> args_with_receiver;
	if (receiver) args_with_receiver.push_back(receiver);
	args_with_receiver.insert(args_with_receiver.end(),block->argls.begin(),block->argls.end());

	ExprFnDef* call_target = scope->find_fn(block->call_expr->as_name(),block,args_with_receiver, desired,flags);
	auto fnc=call_target;
	if (!call_target){
		return ResolvedType();
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
			return propogate_type_fwd(flags,block, desired, block->type_ref(),fnr);
		}
			// add this to the targets' list of calls.
		int num_known_types=(desired?1:0)+num_known_arg_types(block->argls);
		bool isg=fnc->is_generic();
		if (!(isg && num_known_types)) {
			// concrete function: we can just take return type.
			auto rt=fnc->return_type();
			return propogate_type_fwd(flags,block, desired, rt,block->type_ref());
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
		if (flags &1) error(block,"can't resolve call\n");
		verify_all();
		return ResolvedType();
	}
	verify_all();
}

Type* Capture::get_elem_type(int i){
	auto v=vars;
	for (; v&&i>0; i--,v=v->next_of_capture);
	return v->type();
}

Name Capture::get_elem_name(int i){
	auto v=vars;
	for (; v&&i>0; i--,v=v->next_of_capture);
	return v->name;
}
int Capture::get_elem_count(){
	auto v=vars;
	int i=0;
	for (; v; i++,v=v->next_of_capture);
	return i;
}

void Capture::coalesce_with(Capture *other){
	// remap all functions that use the other to point to me
	while (other->capture_by){
		auto f=other->capture_by; // pop others' f
		other->capture_by=f->next_of_capture;

		f->next_of_capture= this->capture_by;	// push f to this' capture_by list.
		this->capture_by=f;
		f->my_capture=this;
	}
	// steal other's variables
	while (other->vars){
		auto v=other->vars;		// pop other's var
		other->vars=v->next_of_capture;

		v->next_of_capture=this->vars; // push to this
		this->vars=v;
		v->capture_in=this;
	}
}

Capture* ExprFnDef::get_or_create_capture(ExprFnDef* src){
	if (!this->my_capture) {
		auto c=new Capture;
		this->my_capture = c;
		c->capture_by = this;
		c->capture_from=src;
		c->next_of_from=src->captures;
		src->captures=c;
		c->pos=src->pos;
		c->name=getStringIndexConcat(str(this->name),str(src->name));
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
ResolvedType	ExprFor::resolve(Scope* outer_scope,const Type* desired,int flags){
	auto sc=outer_scope->make_inner_scope(&this->scope,outer_scope->owner_fn,this);
	init->resolve_if(sc,0,flags);
	cond->resolve_if(sc,Type::get_bool(),flags);
	incr->resolve_if(sc,0,flags);
	body->resolve_if(sc,desired,flags);
	if (else_block) {
		else_block->resolve(sc,desired,flags);
		propogate_type(flags, (Node*)this, this->type_ref(), else_block->type_ref());
	}
	//without an else bllock, we can't return
	else{
		propogate_type_fwd(flags, (Node*)this, Type::get_void(),this->body->type_ref());
	}
	auto dbg=[&](){
		newline(0);dbprintf("debug:for: this,else: this type\n");
		this->type()->dump_if(0);newline(0);
		newline(0);
		else_block->dump_if(0);newline(0);
	};
	propogate_type_fwd(flags, (Node*)this, desired, this->type_ref());

	return ResolvedType();
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
			dbprintf_lambdas("infering polymorphic function types");
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
	auto sc=definer_scope->make_inner_scope(&this->scope,this,this);
	this->set_receiver(recs);
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
		return ResolvedType();
	}

	if (!this->is_generic()){
		for (int i=0; i<this->args.size() && i<this->args.size(); i++) {
			this->args[i]->resolve(definer_scope, nullptr, flags); // todo: call with defaultparams & init-expr
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

	if (!this->fn_type) {
		this->fn_type=new Type(this,this->is_closure()?CLOSURE:FN);
		auto arglist=new Type(this,TUPLE);
//		if (recs){arglist->push_back(new Type(PTR,recs));}
		for (auto a:this->args) {
			arglist->push_back(a->type()?((Type*)a->type()->clone()):new Type(this,AUTO));
		}
		// TODO - type inference needs to know about elipsis, as 'endless auto'
		//if (this->variadic){arglist->push_back(new Type(this,ELIPSIS));}
		auto ret_t=this->ret_type?(Type*)(this->ret_type->clone()):new Type(this,AUTO);
		
		this->fn_type->set_fn_details(arglist,ret_t,recs);
		this->set_type(this->fn_type);
	}
	if (!this->is_generic()){
		this->fn_type->resolve_if(scope,nullptr,flags);
		this->return_type()->resolve_if(scope,nullptr,flags);
	}
	return ResolvedType(fn_type,ResolvedType::COMPLETE);
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
bool isNum(char c){return (c>='0'&&c<='9')||(c>='a'&&c<='f')||(c=='e')||(c=='x') ||c=='.';};
bool isWhitespace(char c){return  c==' '||c=='\n'||c=='\a'||c=='\t';};
bool isOperator(char c){return c=='+'||c=='-'||c=='*'||c=='/'||c=='.'||c=='='||c=='>'||c=='<'||c=='&'||c=='|'||c=='~'||c=='%'||c=='^'||c=='+';}

struct NumDenom{int num; int denom;};

struct Lexer {
	char filename[512];
	SrcPos	pos;
	enum {MAX_DEPTH=32};
	SrcPos	bracket_pos[MAX_DEPTH];
	int		bracket[MAX_DEPTH];
	const char* buffer=0,*tok_start=0,*tok_end=0,*prev_start=0,*line_start=0;
	Name curr_tok;int typaram_depth=0;
#ifdef WATCH_TOK
	char watch_tok[64][12];
#endif
	bool error_newline=false;
	int indent=0,depth=0;
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
	Lexer(const char* src,const char *filename_){
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
			int j=0;
			for (; cmp[j] ; j++,len++) {
				if (tok_start[j]!=cmp[j]) break;
			}
			if (!cmp[j]) {// got all?
				if (len>longest){longest=len;match=i;}
			}
		}
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
	bool is_comment(const char* c){
		if (*c && c[0]=='/'){if(c[1]=='/') return true;} return false;
	}
	void skip_whitespace(){
		bool newline=false;
		while ((isWhitespace(*tok_end) || is_comment(tok_end))&&*tok_end) {
			if (is_comment(tok_end)){
				// skip comment
				tok_end+=2;
				while (*tok_end!='\n' && *tok_end){
					tok_end++;
				}
			}
			if (*tok_end=='\n') {pos.line++;line_start=tok_end; typaram_depth=0;indent=0;newline=true;}
			if (newline)indent++;
			pos.col=tok_end-line_start;
			tok_end++;
		}
	}
	
	void typeparam_hack(){
		if (typaram_depth){
			if (curr_tok==GT){
				typaram_depth--;
				curr_tok=CLOSE_TYPARAM;
			}
			if (typaram_depth>=2 &&curr_tok==SHR){	//< < >>  consume one..
				typaram_depth--;
				curr_tok=CLOSE_TYPARAM;
				tok_end--;
				
			}
		}
		if (curr_tok==LT){
		// hack for reading C++ template type parameters.
		// we accept a subset of uses: when <..> appear on the same line, not seperated by close-other-pair ) ] }  or ";" or "->" - we assume it *is* a template.
		// args(a<b,c>d) is an example of a false posative. we simply throw an error requiring disambiguation with more parens
		// multiline typarams not accepted, etc.

			const char* s;
			int potential=0;
			int paren_depth=0,brace_depth=0,bracket_depth=0,ambiguity=0;
			for (s=tok_end; *s && s[1] && *s!='\n'; s++){
				char c=s[0]; char c1=s[1];
				if (c=='(') paren_depth++;
				if (c=='[') brace_depth++;
				if (c=='{') bracket_depth++;
				if (c==')') paren_depth--;
				if (c==']') brace_depth--;
				if (c=='}') bracket_depth--;
				if (c==',') ambiguity=1;
				if (c==IF || c==ELSE||c==FOR||c==RETURN||c==BREAK||c==DO||c==WHILE)
				{potential=0;break;}
				if (paren_depth<0 || brace_depth<0 || bracket_depth<0 || c==';'){
					{potential=0;break;} // we know its not a typeparam.
				}
				if ((c=='&'&&c1=='&')||(c=='|'||c1=='|'))break;//logic expressions win; no fancy TMP will parse
				if (c=='\"' || c=='\'') break;
				if (c=='<' && c1=='<') {s++; continue;}
				//if (c=='>' && c1=='>') {s++; continue;} todo.. if typaram_depth>1??
				if (c=='-' && c1=='>') {potential=0;break;}
				if (c=='>') {potential=1;break;}
				if (c=='/'&& c1=='/') break;
			}
			if (potential){
				curr_tok=OPEN_TYPARAM;
				typaram_depth++;
			}
		}
	}
	static int close_of(int tok){
		if (tok==OPEN_BRACE)return CLOSE_BRACE;
		if (tok==OPEN_BRACKET)return CLOSE_BRACKET;
		if (tok==OPEN_PAREN)return CLOSE_PAREN;
		return 0;
	}
	void advance_tok() {
		advance_tok_sub();
		typeparam_hack();
	}
	void advance_tok_sub() {
		skip_whitespace();
		tok_start=tok_end;
		pos.col=tok_start-line_start;
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
		typeparam_hack();
	}
	Name eat_tok() {
		prev_start=tok_start;
		for (const char* c=tok_start; c!=tok_end;c++) {}
		auto r=curr_tok;
		advance_tok();
		if (r==OPEN_BRACE || r==OPEN_BRACKET || r==OPEN_PAREN) {
			bracket_pos[depth]=pos;ASSERT(depth<32);bracket[depth++]=(int)r;
		}
		if (r==CLOSE_BRACE || r==CLOSE_BRACKET || r==CLOSE_PAREN) {
			auto open=bracket[--depth];
			if (depth<0)
				::error(pos, "too many close brackets");
			auto close=close_of((int)open);
			if (close!=(int)r ) {
				::error(pos,"found %s expected %s",str(r),str(close));
				::error(bracket_pos[depth+1],"from here");
				::error_end(0);
			}
		}
#ifdef DEBUG2
		if (!strcmp(getString(r),"debugme")){
			dbprintf("found debug token\n");
		}
#endif

		return r;
	}
	Name eat_if(Name a, Name b, Name c){
		auto t=peek_tok(); if (t==a || t==b || t==c) return eat_tok();
		return 0;
	}
	int eat_if(Name a, Name b){
		auto t=peek_tok(); if (t==a || t==b) return (int)t;
		return 0;
	}
	bool eat_if(Name i) {
		if (peek_tok()==i) {eat_tok(); return true;}
		else return false;
	}
	bool is_placeholder()const {return  ((*tok_start=='_') && !isSymbolCont(*tok_end));}
	Name eat_if_placeholder(){if (is_placeholder()){advance_tok(); return PLACEHOLDER;} else return Name();}
	Name eat_ident() {
		auto r=eat_tok();
		if (r<IDENT) {
			::error(pos,"expected ident found %s",getString(r));error_end(0);}
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
		const char* p=tok_start;
		int sign=1; if (*p=='-') {sign=-1;p++;}
		int base=10;
		if ((tok_start+2)<=tok_end){
			if (tok_start[0]=='0' && tok_start[1]=='x'){
				base=16;
			}
		}
		for (;p<tok_end; p++) {
			char c=*p;
			if (*p=='.') { frac=1;}
			else {
				val*=base;
				frac*=base;
				val+=(c>='0'&&c<='9')?(c-'0'):(c>='a'&&c<='f')?(10+(c-'a')):0;
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
	Name expect(Name t, const char* err=""){ decltype(t) x;if (!(t==(x=eat_tok()))) {error(0,"expected %s found %s;%s\n",str(t), str(x),err);} return x;}
	Name expect(Name a,Name b, const char* err=""){ auto x=eat_tok();if (!(a==x || b==x)) {error(0,"expected %s or %s found %s;%s\n",str(a),str(b), str(x),err);} return x;}
};

void unexpected(int t){error(0,"unexpected %s\n",getString(t));}
typedef Lexer TokenStream;

// todo: plugin arch? Node::parse(), dispatch on registered keywords?
Expr* parse_lisp(TokenStream& src);
ExprFnDef* parse_fn(TokenStream&src,ExprStructDef* owner);	// eg fn foo()
ExprFnDef* parse_closure(TokenStream&src);//eg |x|
ExprFor* parse_for(TokenStream&src);
ExprIf* parse_if(TokenStream&src);
TypeDef* parse_typedef(TokenStream&src);
Type* parse_type(TokenStream& src, int close,Node* owner);
ExprStructDef* parse_struct(TokenStream& src);
ExprStructDef* parse_enum(TokenStream& src);
ExprMatch* parse_match(TokenStream& src);
ExprLiteral* parse_literal(TokenStream& src);
ExprOp* parse_flow(TokenStream& src,Name flow);


template<typename T>
T pop(std::vector<T>& v){ ASSERT(v.size()>0);auto r=v[v.size()-1];/*move?*/ v.pop_back(); return r;}
//#define pop(X) ASSERT(X.size()>0); pop_sub(X);

void dump(vector<Expr*>& v) { 
	for (int i=0; i<v.size(); i++) {
		v[i]->dump_top();
	}
	dbprintf("\n");
}
struct SrcOp{ Name op; SrcPos pos;};
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

ExprBlock* parse_block(TokenStream&src,int close,int delim, Expr* op);

Expr* parse_expr(TokenStream&src) {
	return parse_block(src,0,0,nullptr);
}

void another_operand_so_maybe_flush(bool& was_operand, ExprBlock* node,
					  vector<SrcOp>& operators,
					  vector<Expr*>& operands

					  ){
	if (was_operand==true) {
		//error(node,"warning undeliminated expression parsing anyway");
		flush_op_stack(node,operators,operands);// keep going
	}
	was_operand=true;
}
LLVMType Node::get_type_llvm() const
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

ExprBlock* parse_block(TokenStream& src,int close,int delim, Expr* op) {
	// shunting yard expression parser+dispatch to other contexts
	ExprBlock *node=new ExprBlock(src.pos); node->call_expr=op;
	if (!g_pRoot) g_pRoot=node;
	verify(node->type());
	vector<SrcOp> operators;
	vector<Expr*> operands;
	bool	was_operand=false;
	int wrong_delim=delim==SEMICOLON?COMMA:SEMICOLON;
	int wrong_close=close==CLOSE_PAREN?CLOSE_BRACE:CLOSE_PAREN;
	node->bracket_type=(close==CLOSE_BRACKET)?OPEN_BRACKET:close==CLOSE_PAREN?OPEN_PAREN:close==CLOSE_BRACE?OPEN_BRACE:0;
	while (true) {
		if (src.peek_tok()==0) break;
		if (src.peek_tok()==IN) break;
		// parsing a single expression TODO split this into 'parse expr()', 'parse_compound'
		if (close || delim) { // compound expression mode.
			if (src.eat_if(close))
				break;
			if (src.eat_if(wrong_close)) {
				error(0,"unexpected %s, expected %s",getString(close),getString(wrong_close));
				error_end(0);
			}
		} else { // single expression mode - we dont consume delimiter.
			auto peek=src.peek_tok();
			if (peek==CLOSE_BRACKET || peek==CLOSE_BRACE || peek==COMMA || peek==SEMICOLON)
				break;
		}

		if (src.is_next_literal()) {
			auto ln=parse_literal(src);
			operands.push_back(ln);
			was_operand=true;
			continue;
		}
		else if (src.eat_if(STRUCT)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_struct(src));
		}
		else if (src.eat_if(ENUM)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_enum(src));
		}
		else if (src.eat_if(FN)) {
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			auto local_fn=parse_fn(src,nullptr);
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
		else if (auto t=src.eat_if(BREAK,RETURN,CONTINUE)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_flow(src,t));
		}
		else if (src.eat_if(OPEN_PAREN)) {
			if (was_operand){
				operands.push_back(parse_block(src, CLOSE_PAREN,SEMICOLON, pop(operands)));
				// call result is operand
			}
			else {
				operands.push_back(parse_block(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACKET)){
			if (was_operand){
				operands.push_back(parse_block(src,CLOSE_BRACKET,COMMA,pop(operands)));
			} else {
				error(operands.back()?operands.back():node,"TODO: array initializer");
				operands.push_back(parse_block(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACE)){
//			error(operands.back()?operands.back():node,"struct initializer");
			if (was_operand){// struct initializer
				operands.push_back(parse_block(src,CLOSE_BRACE,COMMA,pop(operands)));
			}
			else{//progn aka scope block with return value.
				auto sub=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
				operands.push_back(sub);
				if (sub->delimiter==COMMA)
					sub->create_anon_struct_initializer();
			}
		} else if (src.eat_if(delim)) {
			flush_op_stack(node,operators,operands);
			node->set_delim(delim);
			was_operand=false;
		}
		else if (src.eat_if(wrong_delim) && delim){ //allows ,,;,,;,,  TODO. more precise.
			node->set_delim(wrong_delim);
			flush_op_stack(node,operators,operands);// keep going
			was_operand=false;
		}
		else{
			auto tok=src.eat_tok();
			if (!was_operand && tok==OR){
				another_operand_so_maybe_flush(was_operand,node,operators,operands);
				operands.push_back(parse_closure(src));
			} else
			if (is_operator(tok)) {
				if (was_operand) tok=get_infix_operator(tok);
				else tok=get_prefix_operator(tok);


				verify_all();
				while (operators.size()>0) {
					int prev_precedence=precedence(operators.back().op);
					int prec=precedence(tok);
					if (prev_precedence>prec
						||(is_right_assoc(tok)&&prec==prev_precedence))
						break;
					pop_operator_call(operators,operands);
				}
				verify_all();
				if (tok==AS){
					Type *t=parse_type(src,0,nullptr);
					if (!was_operand) error(t,"as must follow operand");
					auto lhs=operands.back(); operands.pop_back();
					operands.push_back(new ExprOp(AS,src.pos,lhs,t));
					was_operand=true;
					t->set_type(t);
				}else
				if (tok==COLON){// special case: : invokes parsing type. TODO: we actually want to get rid of this? type could be read from other nodes, parsed same as rest?
					Type *t=parse_type(src,0,nullptr);
					auto lhs=operands.back();
					lhs->set_type(t);
					was_operand=true;
				} else if (tok==ASSIGN_COLON){ //x=:Type  ... creates a var of 'Type'.
					Type *t=parse_type(src,0,nullptr);
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
	if (operands.size()){
		// final expression is also returnvalue,
		flush_op_stack(node,operators,operands);
	} else if (node->is_compound_expression()){
		node->argls.push_back(new ExprLiteral(src.pos));
	}
	verify(node->get_type());
	node->verify();
	return node;
}

void gather_vtable(ExprStructDef* d) {
}

void
ExprBlock::create_anon_struct_initializer(){
	// concatenate given names & argcount as the identifer
	// make it generic over types.
	char tmp[256]="anon_";
	for (auto i=0; i<argls.size();i++){
		auto p=dynamic_cast<ExprOp*>(argls[i]);
		if (!p || !(p->name==ASSIGN||p->name==COLON)){
			error(this,"anon struct initializer must have named elements {n0=expr,n1=expr,..}");
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
ExprLiteral* parse_literal(TokenStream& src) {
	ExprLiteral* ln=0;
	if (src.is_next_number()) {
		auto n=src.eat_number();
		if (n.denom==1) {ln=new ExprLiteral(src.pos,n.num);}
		else {ln=new ExprLiteral(src.pos, (float)n.num/(float)n.denom);}
	} else if (src.is_next_string()) {
		ln=new ExprLiteral(src.pos,src.eat_string());
	} else {
		error(0,"error parsing literal\n");
		error_end(0);
	}
	return ln;
}

void parse_ret_val(TokenStream& src, Node* owner, Type* fn_type){
	if (src.eat_if("->")){
		fn_type->push_back(parse_type(src,0,owner));// return value
	} else{
		fn_type->push_back(new Type(owner,VOID));// return value
	}
}
Type* parse_tuple(TokenStream& src, Node* owner)
{
	auto ret= new Type(TUPLE,src.pos);
	while (auto sub=parse_type(src, CLOSE_PAREN,owner)){
		ret->push_back(sub);
		src.eat_if(COMMA);
	}
	return ret;
}
Type* parse_type(TokenStream& src, int close,Node* owner) {
	auto tok=src.eat_tok();
	Type* ret=0;	// read the first, its the form..
	if (tok==close) return nullptr;
	if (tok==FN){	// fn(arg0,arg1,...)->ret
		ret=new Type(FN,src.pos);
		src.expect(OPEN_PAREN,"eg fn() fn name()");
		ret->push_back(parse_tuple(src,owner));// args
		parse_ret_val(src,owner,ret);
	}
	else if (tok==OR){ // closure |arg0,arg1,..|->ret
		ret = new Type(owner,CLOSURE);
		auto args=new Type(owner,TUPLE); ret->push_back(args);
		while ((src.peek_tok())!=OR){
			args->push_back(parse_type(src,OR,owner));
			src.eat_if(COMMA);
		}
		parse_ret_val(src,owner,ret);
	}
	else if (tok==OPEN_PAREN) {
		ret=parse_tuple(src,owner);
		if (src.eat_if(ARROW)){
			// tuple->type  defines a function.
			auto fn_ret=parse_type(src,0,owner);
			auto fn_type=new Type(owner,CLOSURE);
			fn_type->push_back(ret);
			fn_type->push_back(fn_ret);
			return fn_type;
		}
 
	} else {
		// prefixes in typegrammar..
		if (tok==MUL || tok==AND) {
			ret=new Type(owner,PTR);
			ret->sub=parse_type(src,close,owner);
		}else {
		// main: something[typeparams]..
			ret = new Type(owner,tok);
			if (src.eat_if(OPEN_BRACKET)) {
				while (auto sub=parse_type(src, CLOSE_BRACKET,owner)){
					ret->push_back(sub);
					src.eat_if(COMMA);
				}
			}
		// postfixes:  eg FOO|BAR|BAZ todo  FOO*BAR*BAZ   FOO&BAR&BAZ
			if (src.peek_tok()==OR && close!=OR){
				Type* sub=ret; ret=new Type(owner,VARIANT); ret->push_back(sub);
				while (src.eat_if(OR)){
					auto sub=parse_type(src,close,owner);
					ret->push_back(sub);
				}
			}
		}
	}
	if (!owner) ret->set_origin(ret);	// its a type declaration, 'origin is here'.
 	// todo: pointers, adresses, arrays..
	return ret;
}
ArgDef* parse_arg(TokenStream& src, int close) {
	auto argname=src.eat_ident();
	if (argname==close) return nullptr;
	auto a=new ArgDef(src.pos,argname);
	a->pos=src.pos;
	if (src.eat_if(COLON)) {
		a->type()=parse_type(src,close,a);
	}
	if (src.eat_if(ASSIGN)){
		a->default_expr=parse_expr(src);
	}
	return a;
}
void parse_typeparams(TokenStream& src,vector<TypeParam*>& out) {
	while (!src.eat_if(CLOSE_BRACKET)){
//		if (src.eat_if(CLOSE_BRACKET)) break;
		auto name=src.eat_tok();
//		int d=0;
//		if (src.eat_if(ASSIGN)) {
//			int d=src.eat_tok();
//		}
		out.push_back(new TypeParam{name,src.eat_if(ASSIGN)?parse_type(src,0,nullptr):0});
		src.eat_if(COMMA);
	}
}

ExprStructDef* parse_struct_body(TokenStream& src,SrcPos pos,Name name);
ExprStructDef* parse_struct(TokenStream& src) {
	auto pos=src.pos;
	auto tok=src.eat_ident();
	return parse_struct_body(src,pos,tok);
}
ExprStructDef* parse_struct_body(TokenStream& src,SrcPos pos,Name name){
	auto sd=new ExprStructDef(pos,name);
	if (src.eat_if(OPEN_BRACKET)) {
		parse_typeparams(src,sd->typeparams);
	}
	if (src.eat_if(COLON)) {
		sd->inherits_type = parse_type(src,0,nullptr); // inherited base has typeparams. only single-inheritance allowed. its essentially an anonymous field
	}
	if (!src.eat_if(OPEN_BRACE))
		return sd;
	// todo: type-params.
	Name tok;
	while ((tok=src.peek_tok())!=NONE){
		if (tok==CLOSE_BRACE){src.eat_tok(); break;}
		if (src.eat_if(STRUCT)) {
			sd->structs.push_back(parse_struct(src));
		} else if (src.eat_if(FN)){
			sd->functions.push_back(parse_fn(src,sd));
		} else {
			auto arg=parse_arg(src,CLOSE_PAREN);
			sd->fields.push_back(arg);
		}
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	return sd;
}
ExprStructDef* parse_tuple_struct_body(TokenStream& src, SrcPos pos, Name name){
	Name tok;
	auto sd=new ExprStructDef(pos,name);
	if (src.eat_if(OPEN_BRACKET)) {
		parse_typeparams(src,sd->typeparams);
	}
	if (!src.eat_if(OPEN_PAREN))
		return sd;

	while ((tok=src.peek_tok())!=NONE){
		if (tok==CLOSE_PAREN){src.eat_tok(); break;}
		sd->fields.push_back(new ArgDef(pos,0,parse_type(src,0,sd)));
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	return sd;
}

ExprStructDef* parse_enum(TokenStream& src) {
	auto pos=src.pos;
	auto tok=src.eat_ident();
	auto ed=new EnumDef(src.pos,tok);
	if (src.eat_if(OPEN_BRACKET)) {
		parse_typeparams(src,ed->typeparams);
	}
	if (src.eat_if(COLON)) {
		ed->inherits_type = parse_type(src,0,ed); // inherited base has typeparams. only single-inheritance allowed. its essentially an anonymous field
	}
	if (!src.eat_if(OPEN_BRACE))
		return ed;
	// todo: type-params.
	while ((tok=src.eat_tok())!=NONE){
		auto subpos=src.pos;
		if (tok==CLOSE_BRACE){break;}
		// got an ident, now what definition follows.. =value, {fields}, (types), ..
		if (src.peek_tok()==OPEN_BRACE){
			ed->structs.push_back(parse_struct_body(src,subpos,tok));
		} else if (src.peek_tok()==OPEN_BRACKET){
			ed->structs.push_back(parse_tuple_struct_body(src,subpos,tok));
		} else if (src.eat_if(ASSIGN)){
			auto lit=parse_literal(src); lit->name=tok;
			ed->literals.push_back(lit);
		}
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	return ed;
}

void ExprIf::translate_typeparams(const TypeParamXlat& tpx){
	this->cond->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
}
void ExprFor::translate_typeparams(const TypeParamXlat& tpx)
{
	if (this->init) this->init->translate_typeparams(tpx);
	if (this->cond) this->cond->translate_typeparams(tpx);
	if (this->incr) this->incr->translate_typeparams(tpx);
	if (this->pattern) this->pattern->translate_typeparams(tpx);
	if (this->body) this->body->translate_typeparams(tpx);
	if (this->else_block) this->else_block->translate_typeparams(tpx);
}

void Name::translate_typeparams(const TypeParamXlat& tpx)
{
	auto index=tpx.typeparam_index(*this);
	if (index>=0){
		ASSERT(tpx.given_types[index]->sub==0 && "TODO type-expressions for name? concat[],...");
		*this=tpx.given_types[index]->name;
	}
}
void ExprIdent::translate_typeparams(const TypeParamXlat& tpx)
{
	// templated idents?
	// TODO - these could be expresions - "concat[T,X]"
	this->name.translate_typeparams(tpx);
	
}

void ExprBlock::translate_typeparams(const TypeParamXlat& tpx){
	if (this->call_expr)
		this->call_expr->translate_typeparams(tpx);
	for (auto e:argls){
		e->translate_typeparams(tpx);
	}
}

void ExprOp::translate_typeparams(const TypeParamXlat& tpx){
	if (lhs) lhs->translate_typeparams(tpx);
	if (rhs) rhs->translate_typeparams(tpx);
}

void ExprFnDef::translate_typeparams(const TypeParamXlat& tpx){
	for (auto &a:args) a->translate_typeparams(tpx);
	if (this->ret_type) {
		this->ret_type->translate_typeparams(tpx);
	} else {
	}
	this->body->translate_typeparams(tpx);
	
	if (tpx.typeparams_all_set())
	{
		this->typeparams.resize(0);
	}
}

ArgDef*	ExprStructDef::find_field(const Node* rhs)const{
	auto fi= this->try_find_field(rhs->as_name());
	if (!fi)
		error(rhs,this,"no field %s in ",str(name),str(this->name));
	return fi;
}
ArgDef* ExprStructDef::try_find_field(const Name name)const{
	for (auto a:fields){if (a->name==name) return a;}
	return nullptr;
}

void ExprStructDef::translate_typeparams(const TypeParamXlat& tpx)
{
	for (auto a:this->fields)a->translate_typeparams(tpx);
	for (auto f:functions)f->translate_typeparams(tpx);
	for (auto s:structs)s->translate_typeparams(tpx);
	if (tpx.typeparams_all_set())
		this->typeparams.resize(0);
}
ExprStructDef* ArgDef::member_of()	{ // todo, implement for 'Variable' aswell, unify capture & member-object.
	if (owner) return owner->get_receiver();
	return nullptr;
}

void ArgDef::translate_typeparams(const TypeParamXlat& tpx){
	this->name.translate_typeparams(tpx);
	if (this->get_type()){
		this->get_type()->struct_def=0; // needs resolving again
		this->get_type()->translate_typeparams(tpx);
	}
	if (this->default_expr){
		this->default_expr->translate_typeparams(tpx);
	}
}
int TypeParamXlat::typeparam_index(const Name& n) const{
	for (int i=0; i<this->typeparams.size(); i++){
		if (this->typeparams[i]->name==n) return i;
	}
	return -1;
}
void TypeParamXlat::dump(){
	dbprintf("[");
	for (auto i=0; i<this->typeparams.size();i++){
		if (i)dbprintf(",");
		dbprintf("%s=",str(this->typeparams[i]->name));
		this->given_types[i]->dump_if(-1);
	}
	dbprintf("]");
}

void Type::translate_typeparams(const TypeParamXlat& tpx){
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
	if (this->struct_def) this->struct_def=0;
	int param_index=tpx.typeparam_index(this->name);
	if (param_index>=0){
		auto src_ty=tpx.given_types[param_index];
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
	//this->struct_def=tpx.
	for (auto sub=this->sub; sub; sub=sub->next) {
		sub->translate_typeparams(tpx);
	}
}
bool type_params_eq(const vector<Type*>& a, const vector<Type*>& b) {
	if (a.size()!=b.size()) return false;
	for (int i=0; i<a.size(); i++) { if (!a[i]->eq(b[i])) return false;}
	return true;
}
bool type_params_eq(const vector<Type*>& a, const Type* tp){
	for (auto
		 i=0; i<a.size() && tp;i++,tp=tp->next){
		if (!a[i]->eq(tp))
			return false;
	}
	// TODO- defaults.
	return true;
}
ExprStructDef* ExprStructDef::get_instance(Scope* sc, const Type* type) {
	auto parent=this;
	if (!this->is_generic())
		return this;
	// make the typeparams..
	// search for existing instance
	ExprStructDef* ins=parent->instances;
	for (;ins; ins=ins->next_instance) {
		if (type_params_eq(ins->instanced_types,type->sub))
			break;
	}
	if (!ins) {
		dbprintf_instancing("instantiating %s with[",this->name_str());
		for (auto t=type->sub;t;t=t->next)dbprintf_instancing("%s,",t->name_str());
		dbprintf_instancing("]\n");
		// TODO: store a tree of partial instantiations eg by each type..
		vector<Type*> ty_params;
		int i=0;
		Type* tp=type->sub;
		for (i=0; i<parent->typeparams.size() && tp; i++,tp=tp->next){
			ty_params.push_back(tp);
		}
		for (;i<parent->typeparams.size(); i++) {
			ty_params.push_back(parent->typeparams[i]->defaultv);
		}
		
		ins = (ExprStructDef*)this->clone(); // todo: Clone could take typeparams
							// cloning is usually for template instantiation?
		ins->instanced_types=ty_params;
		ins->instance_of=this;
		ins->next_instance = this->instances; this->instances=ins;
		ins->inherits_type= this->inherits_type; // TODO: typeparams! map 'parent' within context  to make new typeparam vector, and get an instance for that too.
		if (g_debug_get_instance)
			for (auto i=0; i<ins->instanced_types.size();i++)
				dbprintf(ins->instanced_types[i]->name_str());
		ins->translate_typeparams(TypeParamXlat(this->typeparams, ins->instanced_types));
	}
	if (!type->struct_def) { const_cast<Type*>(type)->struct_def=ins;}
//	else { ASSERT(type->struct_def==ins && "instantiated type should be unique")};
	
	return ins;
}

Node* ExprStructDef::clone() const{
	return this->clone_sub(new ExprStructDef(this->pos,this->name));
}
Node* ExprStructDef::clone_sub(ExprStructDef* d)const {
	for (auto m:this->fields) {d->fields.push_back((ArgDef*)m->clone());}
//	for (auto t:this->typeparams) {d->typeparams.push_back(t->clone());}
	d->typeparams = this->typeparams;
	for (auto f:this->functions){d->functions.push_back((ExprFnDef*)f->clone());}
	for (auto s:this->structs){d->structs.push_back((ExprStructDef*)s->clone());}
	for (auto l:this->literals){d->literals.push_back((ExprLiteral*)l->clone());}
	return d;
}
Node* EnumDef::clone()const {
	return this->clone_sub(new EnumDef(this->pos,this->name));
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

void ExprStructDef::roll_vtable() {
	if (this->vtable){ return;} // done already
	if (this->inherits) {this->inherits->roll_vtable();}

	// todo - it should be namespaced..
	char vtn[512];sprintf(vtn,"%s__vtable",str(this->name));
	this->vtable=new ExprStructDef(this->pos,getStringIndex(vtn));
	this->vtable->scope=this->scope;

	// todo: we will create a global for the vtable
	// we want to be able to emulate rust trait-objects
	// & hotswap vtables at runtime for statemachines

	for (int i=0; i<this->functions.size();i++) {
		auto f=this->functions[i];
		// todo: static-virtual fields go here!
		this->vtable->fields.push_back(
			new ArgDef(
				this->pos,
				f->name,
				f->fn_type,//todo:  insertion of 'this'
				new ExprIdent(this->pos, f->name)
			)
		);
	}
}

void ExprStructDef::dump(int depth) const{
	newline(depth);
	dbprintf("%s %s",this->kind_str(), getString(this->name));dump_typeparams(this->typeparams);
	dbprintf("[");
	if (this->instanced_types.size()){
		for (auto t:this->instanced_types)
		{	t->dump(depth+1);dbprintf(",");};
	}else{
		for (auto t:this->typeparams)
			{t->dump(depth+1);dbprintf(",");}
	}
	dbprintf("]");
	if (this->inherits) {dbprintf(" : %s", str(inherits->name));}
	dbprintf("{");
	for (auto m:this->literals)	{m->dump(depth+1);}
	for (auto m:this->fields)	{m->dump(depth+1);}
	for (auto s:this->structs)	{s->dump(depth+1);}
	for (auto f:this->functions){f->dump(depth+1);}
	newline(depth);dbprintf("}");
}

ExprFor*	Scope::current_loop(){
	for (auto sc=this; sc;sc=sc->parent_within_fn()){
		if (auto n=sc->node->as_for())
			return n;
	}
	return nullptr;
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

bool ExprStructDef::is_generic()const{
	if (typeparams.size())
		return true;
	for (auto f:fields){if (!f->type())return true;}//TODO: is typeparam?
	return false;
}
ResolvedType ExprStructDef::resolve(Scope* definer_scope,const Type* desired,int flags){

	definer_scope->add_struct(this);
	if (!this->get_type()) {
		this->set_type(new Type(this,this->name));	// name selects this struct
	}

	if (!this->is_generic()){
		auto sc=definer_scope->make_inner_scope(&this->scope,this,this);
		for (auto m:fields){
			m->resolve(sc,nullptr,flags);
		}
		for (auto s:structs){
			s->resolve(sc,nullptr,flags);
		}
		for (auto f:functions){
			f->resolve(sc,nullptr,flags);
		}

		if (this->inherits_type && !this->inherits){
			this->inherits_type->resolve(definer_scope,desired,flags);
			this->inherits=definer_scope->find_struct(this->inherits_type);
		}
		if (this->functions.size()){// ToDO: && is-class. and differentiate virtual functions. For the minute, *all* functoins defined in struct are virtual. we want UFCS for non-virtuals.
			roll_vtable();
		}
		if (this->vtable) this->vtable->resolve(definer_scope,desired,flags);
	} else{
		for (auto ins=this->instances; ins; ins=ins->next_instance)
			ins->resolve(definer_scope,nullptr, flags);
	}

	return propogate_type_fwd(flags,this, desired);
}
// iterator protocol. value.init. increment & end test.
ExprFor* parse_for(TokenStream& src){
	auto p=new ExprFor(src.pos);
	auto first=parse_block(src,SEMICOLON,COMMA,0);
	if (src.eat_if(IN)){
		p->pattern=first;
		p->init=parse_block(src, OPEN_BRACE, 0, 0);
		src.expect(OPEN_BRACE,"eg for x..in..{}");
	} else {//if (src.eat_if(SEMICOLON)){// cfor.  for init;condition;incr{body}
		p->pattern=0;
		p->init=first;
		p->cond=parse_block(src,SEMICOLON,COMMA,0);
		//ssrc.expect(SEMICOLON,"eg for init;cond;inc{..}");
		p->incr=parse_block(src,OPEN_BRACE,COMMA,0);
	}
 //else {
//		error(p,"for..in.. or c style for loop, expect for init;cond;incr{body}");
//	}
	p->body=parse_block(src, CLOSE_BRACE, SEMICOLON, nullptr);
	if (src.eat_if(ELSE)){
		src.expect(OPEN_BRACE,"after else");
		p->else_block=parse_block(src,CLOSE_BRACE, SEMICOLON, nullptr);
	}
	return p;
}


Node* ExprFor::clone()const{
	auto n=new ExprFor(this->pos);
	n->pattern=(Expr*)pattern->clone_if();
	n->init=(Expr*)init->clone_if();
	n->cond=(Expr*)cond->clone_if();
	n->incr=(Expr*)cond->clone_if();
	n->body=(Expr*)cond->clone_if();
	n->else_block=(Expr*)cond->clone_if();
	return n;
}
ExprOp* parse_flow(TokenStream& src,Name flow_statement){
	// break is an operator. label,return.
	Expr* expr=nullptr;
	if (!(src.peek_tok()==CLOSE_BRACE || src.peek_tok()==SEMICOLON)){
		dbprintf("%s\n",str(src.peek_tok()));
		expr=parse_expr(src);
	}
	return new ExprOp(flow_statement, src.pos,nullptr,expr);
}
// make a flag for c or rust mode
// exact c parser
// add := gets rid of auto noise
// add postfix : alternate functoin syntax
ExprIf* parse_if(TokenStream& src){
	// TODO: assignments inside the 'if ..' should be in-scope
	// eg if (result,err)=do_something(),err==ok {....}  else {...}
	auto p=new ExprIf(src.pos);
//	p->cond=parse_block(src, OPEN_BRACE, 0, 0);
	p->cond=parse_block(src,OPEN_BRACE,COMMA,0);
	p->body=parse_block(src, CLOSE_BRACE,SEMICOLON,0);
	verify(p->cond->get_type());

	if (src.eat_if(ELSE)) {
		if (src.eat_if(IF)) {
			p->else_block= parse_if(src);
		} else if (src.eat_if(OPEN_BRACE)){
			p->else_block=parse_block(src, CLOSE_BRACE, SEMICOLON, 0);
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
	auto p=new ExprIf(this->pos);
	p->cond=(Expr*)this->cond->clone_if();
	p->body=(Expr*)this->body->clone_if();
	p->else_block=(Expr*)this->else_block->clone_if();
	::verify(p->cond->get_type());
	return p;
}
void ExprIf::dump(int depth) const {
	::verify(cond->get_type());
	newline(depth);dbprintf("if\n");
	cond->dump(depth+1);
	newline(depth);dbprintf("{\n");
	body->dump(depth+1);
	if (else_block)	{
		indent(depth);dbprintf("}else{\n");
		else_block->dump(depth+1);
	}
	newline(depth);dbprintf("}\n");
};

void ExprOp::verify() {
	verify_expr_op(this);
	if (lhs) lhs->verify();
	if (rhs) rhs->verify();
}
void ExprBlock::verify(){
	verify_expr_block(this);
	if (this->call_expr) this->call_expr->verify();
	for (auto x:argls) x->verify();
}
void ExprFnDef::verify(){
	verify_expr_fn_def(this);
	if (body) this->body->verify();
	for (auto x:args) x->verify();
	for (auto s=this->instances; s;s=s->next_instance) s->verify();
}
void Type::verify(){
	verify_type(this);
	for (auto x=this->sub; x;x=x->next)
		x->verify();
}

ResolvedType ExprIf::resolve(Scope* outer_s,const Type* desired,int flags){
	auto sc=outer_s->make_inner_scope(&this->scope,outer_s->owner_fn,this);
	

	::verify(this->cond->get_type());
	this->cond->resolve(sc,nullptr,flags); // condition can  be anything coercible to bool
	auto body_type=this->body->resolve(sc,desired,flags);
	Type* bt=body_type.type;
	if (else_block){
		propogate_type_fwd(flags,this, desired,bt);
		propogate_type(flags,this, bt);
		else_block->resolve(sc,bt,flags);
		propogate_type(flags,this, this->body->type_ref(), else_block->type_ref());
		propogate_type(flags,this, this->type_ref(), else_block->type_ref());

#if DEBUG >2
		this->body->type()->dump_if();
		this->else_block->type()->dump_if();
		this->type()->dump_if();
#endif
		return propogate_type(flags,this, this->type_ref(), this->body->type_ref());
	}
	else {
		// TODO: Could it actually return Body|void ? perhaps we could implicityly ask for that?
		return body_type;
	}
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
	if(this->type()){
		dbprintf(":");
		this->type()->dump_if(d);
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
void parse_fn_args_ret(ExprFnDef* fndef,TokenStream& src,int close){
	Name tok;
	while ((tok=src.peek_tok())!=NONE) {
		if (tok==ELIPSIS){
			fndef->variadic=true; src.eat_tok(); src.expect(CLOSE_PAREN); break;
		}
		if (src.eat_if(close)){break;}
		auto arg=parse_arg(src,close);
		fndef->args.push_back(arg);
		src.eat_if(COMMA);
	}
	// TODO: multiple argument blocks for currying?.
	if (src.eat_if(ARROW) || src.eat_if(COLON)) {
		fndef->ret_type = parse_type(src, 0,fndef);
	}
}
void parse_fn_body(ExprFnDef* fndef, TokenStream& src){
	// read function arguments
	// implicit "progn" here..
	auto tok=src.peek_tok();
	if (src.eat_if(OPEN_BRACE)){
		fndef->body = parse_block(src, CLOSE_BRACE, SEMICOLON, nullptr);
	} else if (tok==SEMICOLON || tok==COMMA || tok==CLOSE_BRACE ){
		fndef->body=nullptr; // Its' just an extern prototype.
		fndef->c_linkage=true;
	} else{  // its' a single-expression functoin, eg lambda.
		fndef->body=parse_expr(src);
	}
}

ExprFnDef* parse_fn(TokenStream&src, ExprStructDef* owner) {
	auto *fndef=new ExprFnDef(src.pos);
	// read function name or blank

	auto tok=src.eat_tok();
	if (tok!=OPEN_PAREN) {
		ASSERT(is_ident(tok));
		fndef->name=tok;
		if (src.eat_if(OPEN_BRACKET)) {
			parse_typeparams(src,fndef->typeparams);
		}
		tok=src.expect(OPEN_PAREN);
	} else {
		char tmp[512]; sprintf(tmp,"anon_fn_%d",src.pos.line);
		fndef->name=getStringIndex(tmp);
	}
	if (owner){
		fndef->args.push_back(new ArgDef(src.pos,THIS,new Type(PTR,owner)));
	}
	parse_fn_args_ret(fndef,src,CLOSE_PAREN);
	parse_fn_body(fndef,src);
	return fndef;
}
ExprFnDef* parse_closure(TokenStream&src) {// eg |x|x*2
	// we read | to get here
	auto *fndef=new ExprFnDef(src.pos);
	// read function name or blank

	char tmp[512]; sprintf(tmp,"closure_%d",src.pos.line);
	fndef->name=getStringIndex(tmp);
	fndef->m_closure=true;

	parse_fn_args_ret(fndef,src,OR);
	parse_fn_body(fndef,src);
	return fndef;
}

// every file is an implicitly a function aswell taking no args
// when imported, a module inserts a call to that function.
// that sets up global stuff for it.
const char* g_TestMemberFn=
/*1*/  "fn printf(s:str,...)->int;  							\n"
/*2*/  "struct Foo{												\n"
/*3*/  "	q:int,												\n"
/*4*/  "	fn method()->float{										\n"
/*5*/  "		printf(\"Foo method says q=%d this=%p\\n\",q,this);2.0	\n"
/*6*/  "	}													\n"
/*7*/  "}														\n"
/*8*/  "struct Bar{												\n"
/*9*/  "	w:int,												\n"
/*10*/ "	fn method()->float{										\n"
/*11*/ "		printf(\"Bar method says w=%d this=%p\\n\",w,this);2.0	\n"
/*12*/ "	}													\n"
/*13*/ "}														\n"
/*14*/ "fn func1(f:*Foo){printf(\" func1 says q=%d f=%p\\n\",f.q,f);}	\n"
/*14*/ "fn main(){												\n"
/*15*/ "	x:=Foo{5};	px:=&x;	y:=Bar{17}; py:=&y;\n"
/*16*/ "	printf(\"member function test.. %d\\n\",x.q);				\n"
/*17*/	"	px.func1();	\n"
/*18*/	"	px.method();	\n"
/*19*/	"	py.method();	\n"
/*20*/	"		\n"
/*20*/  "}														\n";
const char* g_TestClosure=
/*1*/ 	"fn printf(s:str,...)->int;  							\n"
/*10*/	"fn take_closure(pfunc:(int)->void){ pfunc(5);}\n"
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	y:=11;z:=12;w:=0; y+=10;w+=7;			\n"
/*51*/	"	take_closure(|x|{printf(\"closure x=%d captured y=%d z=%d\\n\",x,y,z);});\n"
"printf(\" y=%d z=%d w=%d\\n\",y+=90,z,w);\n"
/*17*/	"	0\n"
/*20*/  "}														\n";
;
const char* g_TestAlloc=
/*1*/ 	"fn printf(s:str,...)->int;  			\n"
/*2*/	"struct Foo{x:int,y:int};				\n"
/*3*/	"fn main(argc:int, argv:**char)->int{	\n"
/*4*/	"	pfoo:= new Foo{4,5};			\n"
/*5*/	"	pfoos:= new Foo[10];			\n"
/*6*/	"	pfoos[1].x=10;					\n"
/*7*/	"	printf(\"new foo %p x,y=%d,%d array alloc=%p\\n\",pfoo,pfoo.x,pfoo.y,pfoos);			\n"
/*8*/	"	delete pfoo;					\n"
/*9*/	"	0\n"
/*10*/  "}														\n";
;
const char* g_TestBasic=
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	x:=2;\n"
"y:=3;\n"
"z:=x+y;			\n"
/*17*/	"	0\n"
/*20*/  "}														\n";
;
const char* g_TestStruct=
/*54*/ "	struct FooStruct{x:int,y:int};		\n"
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	x:=FooStruct{1,2};\n"
/*17*/	"	0\n"
/*20*/  "}														\n";
;

const char* g_TestArray=
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	xs=:array[int,10];\n"
/*  */	"	xs[1]=5;\n"
/*17*/	"	0\n"
/*20*/  "}														\n";
;

const char* g_TestBasicSyntax=
/* 1*/ "*++x=*--y e+r:int foo(e,r);\n"
/* 2*/ "self.pos+self.vel*dt;\n"
/* 3*/ "future.pos=self.pos+self.vel*dt;\n"
/* 4*/ "x=y=z=3; x+y+z=0;\n"
/* 5*/ "p=&self.pos;\n"
/* 6*/ "*d++=s;\n"
/* 7*/ "q=(++10+*p);\n"
/* 8*/ "fn do_they_float(){set(tfl, 1.0); do_they_int();};\n"
/* 9*/ "fn min(a,b){if(a<b,a,b)}\n"
/*10*/ "fn max(a,b){if(a>b,a,b)}\n"
/*11*/ "fn clamp(a,b,f){ min(b,max(a,f)) }\n"
/*12*/ "fn lerp(a:float,b:float,f:float){(b-a)*f+a}\n"
/*13*/ "fn mad(a:float,b:float,f:float){a+b*f}\n"
/*14*/ "fn main(){printf(\"lerp = %.3f ;\",lerp(0.0,10.0,0.5));}\n"
;

const char* g_TestIf=
/*1*/	"fn printf(s:str,...)->int;				\n"
/*2*/	"fn main(argc:int, argv:**char)->int{	\n"
		"  x:=if argc<3{4} else{3};\n"
/*14*/	"	0									\n"
/*15*/	"}\n"
;
const char* g_TestLoop=
/*1*/	"fn printf(s:str,...)->int;				\n"
/*2*/	"fn main(argc:int, argv:**char)->int{	\n"
/*3*/	"	i:=5; b:=argc<9;						\n"
/*4*/	"	v:=for i:=0,j:=0;		\n"
/*5*/	"			i<10;			\n"
/*6*/	"			i+=1,j+=7 {	\n"
/*7*/	"		printf(\"for loop i=%d j=%d\\n\",i,j);	\n"
/*8*/	"										\n"
/*9*/	"	}									\n"
/*10*/	"	else{								\n"
/*11*/	"		printf(\"loop complete i=%d\\n\",i);0.6\n"
/*12*/	"	}									\n"
/*13*/	"	printf(\"outer scope i=%d\\n\",i);	\n"
/*14*/	"	0									\n"
/*15*/	"}\n"
;
const char* g_TestTyparamInference=
/* 1*/ "struct Union[A,B]{a:A,b:B, tag:int};		\n"
/* 2*/ "fn setv[A,B](u:&Union[A,B], v:A)->void{		\n"
/* 3*/ "	u.a=v; u.tag=0; 						\n"
/* 4*/ "}											\n"
/* 5*/ "fn setv[A,B](u:&Union[A,B], v:B)->void{		\n"
/* 6*/ "	u.b=v; u.tag=1; 						\n"
/* 7*/ "}											\n"
/* 8*/ "fn main(argc:int, argv:**char)->int{		\n"
/* 9*/ "	u=:Union[int,float];					\n"
/*10*/ "	setv(&u,10)								\n"
/*11*/ "	printf(\"u.tag=%d\\n\",u.tag);			\n"
/*12*/ "	setv(&u,10.0)	;						\n"
/*13*/ " printf(\"u.tag=%d\\n\",u.tag);				\n"
/*14*/ "	0}										\n"
/*15*/ "fn printf(s:str,...)->int;					\n"
;
const char* g_TestProg2=

/* 1*/ "enum FooBar{Foo{x:int,y:int},Bar{p:float,q:float} }	\n"
/* 2*/ "struct Union[A,B]{a:A,b:B, tag:int};\n"
/* 3*/ "fn setv[A,B](u:&Union[A,B], v:A){\n"
/* 4*/ "	u.a=v; u.tag=0; \n"
/* 5*/ "}\n"/* 1*/
/* 6*/ "fn setv[A,B](u:&Union[A,B], v:B){\n"
/* 7*/ "	u.b=v; u.tag=1; \n"
/* 8*/ "}\n"
/* 9*/ "fn take_fn(pfunc:fn(int)->void){ pfunc(5);}\n"
/*10*/ "fn take_closure(pfunc:(int)->void){ pfunc(5);}\n"
/*11*/ "fn printf(s:str,...)->int;\n"
/*12*/ "fn foo_bar(x){ printf(\"Hello From generic\\n\"); }      \n"
/*13*/ "fn foo(x:int){ printf(\"Hello From indirect 	functionpointer call %d\\n\",x); }      \n"
/*14*/ "fn bar(x:int,y:int,z:int)->int{ printf(\"bar says %d\\n\",x+y+z);0};\n"
/*15*/ "fn foo_struct(p:*FooStruct)->int{ printf(\"foostruct ptr has %d %d\\n\",p.x,p.y);0}"
/*16*/ "fn something(f:int){\n"
/*17*/ "	printf(\"somethng(int)\\n\");\n"
/*18*/ "}\n"
/*19*/ "fn something(f:float){\n"
/*20*/ "	printf(\"somethng(int)\\n\");\n"
/*21*/ "}\n"
/*22*/ "fn main(argc:int, argv:**char)->int{	\n"
/*23*/ "	xs=:array[int,512];					\n"
/*24*/ "	q:=xs[1]; p1:=&xs[1];				\n"
/*25*/ "	*p1=42;								\n"
/*26*/ "	u=:Union[int,float];				\n"
/*27*/ "	setv(&u,10)	;						\n"
/*28*/ "	printf(\"u.tag=%d\\n\",u.tag);		\n"
/*29*/ "	setv(&u,10.0)	;					\n"
/*30*/ "	printf(\"u.tag=%d\\n\",u.tag);		\n"
/*31*/ "	retval:=0;							\n"
/*32*/ "	x:= {a:=10;b:=20; a+b};				\n"
/*33*/ "	x+=10;								\n"
/*34*/ "	fp:=foo;							\n"
/*35*/ "	xs=:array[int,512];  				\n"
/*36*/ "	p2:=&xs[1];  						\n"
/*37*/ "	xs[1]+=3;							\n"
/*38*/ "	fs:=FooStruct{0xff,0x7f};			\n"
/*39*/ "	something(1);						\n"
/*40*/ "	pfs:=&fs;							\n"
/*41*/ "	foo_struct(&fs);					\n"
/*42*/ "	fn localtest(i:int)->void{			\n"
/*43*/ "		printf(\"hello from local fn %d\\n\",i);	\n"
/*44*/ "	}; 									\n"
/*43*/ "	py:=pfs as *int;					\n"
/*44*/ "	foo_bar(&fs);						\n"
/*45*/ "	printf(\"foostruct int val recast %d; foostruct raw value %d %d\n\",*py,fs.y,pfs.y);							\n"
/*46*/ "	fp(2);fp(x);fp(xs[1]);				\n"
/*47*/ "	take_fn(fp);						\n"
/*48*/ "	take_fn(localtest);					\n"
/*49*/ "	localtest(10);						\n"
/*50*/ "	take_fn(fn(x){printf(\"hello from anon function %d\\n\",x);});		\n"
/*51*/ "	take_closure(|y|{printf(\"hello from closure function x=%d y=%d\\n\",x,y);});\n"
/*52*/ "	bar(1,2,3);							\n"
/*53*/ "	retval}								\n"
/*54*/ "	struct FooStruct{x:int,y:int};		\n"
;

char g_TestPolyLambda[]= //
"fn printf(s:str,...)->int;\n"
/*1*/ "fn debugme[X,Y,R](u:&Union[X,Y], fx:(&X)->R,fy:(&Y)->R)->R{\n"
/*2*/ " if u.tag==0 { fx(&u.x)}\n"
/*3*/ " else { fy(&u.y)}\n"
/*4*/ "}\n"
/*5*/ "fn main(argc:int,argv:**char)->int{\n"
/*6*/ "fv:=Foo{vx=13,vy=14,vz=15};\n"
/*7*/ " u=:Union[int,float];\n"
/*8*/ " setv(&u,0.0);\n"
/*9*/ " setv(&u,0);\n"
/*10*/ " z:=debugme(&u,											\n"
/*11*/ "	|x:&int|{printf(\"union was set to int\\n\");10},	\n"
/*12*/ "	|x:&float|{printf(\"union was set to float\\n\");12}	\n"
/*13*/ "	);												\n"
		"printf(\"map union returns %d\\n\", z);						\n"
"	xs=:array[int,512];\n"
"q:=xs[1]; p1:=&xs[1];\n"
"	xs[2]=000;\n"
"	xs[2]+=400;\n"
"	*p1=30;\n"
"z:=5;\n"
"y:=xs[1]+z+xs[2];\n"
"x:=0;\n"
"	something_foo(&fv,&fv);\n"
"	for i:=0,j:=0; i<10; i+=1,j+=10 {\n"
"		x+=i;\n"
"		printf(\"i,j=%d,%d,x=%d\\n\",i,j,x);\n"
"	}else{\n"
"		printf(\"loop exit fine\\n\");\n"
"	}\n"
"		something_foo(&fv);\n"
"		something(&fv);\n"
"		take_closure(|x|{printf(\"closure says %d %d\\n\",x,y);})\n"
"		\n"
"		x:=if argc<2{printf(\"<2\");1}else{printf(\">2\");2};\n"
"		printf(\"yada yada yada\\n\");\n"
"		printf(\"\\nHello World %d\n\", y );\n"
"		0\n"
"		}\n"
"fn lerp(a,b,f)->float{(b-a)*f+a};\n"
"fn foo(a:*char)->void;\n"
"struct Foo {\n"
"vx:int, vy:int, vz:int\n"
"}\n"
"fn something_foo(f:&Foo){\n"
"	printf(\"f.x= %d\\n\", f.vx);\n"
"}\n"
"fn something_foo(f:&Foo,x:&Foo){\n"
"	printf(\"something_foo with 2 args overloaded\\n\");\n"
"	printf(\"f.x= %d,.y= %d,.z= %d\\n\", f.vx,f.vy,f.vz);\n"
"}\n"
"fn something(f:&Foo){\n"
"	printf(\"f.x= %d,.y= %d,.z= %d\\n\", f.vx, f.vy, f.vz);\n"
"}\n"
"fn something(f:float){\n"
"}\n"
"fn something(f:float,x){\n"
"}\n"
"fn take_closure(funcp:(int)->void){\n"
"	funcp(10);\n"
"}\n"
"struct Union[X,Y]{\n"
"tag:int,\n"
"x:X,y:Y,\n"
"};\n"
"fn setv[X,Y](u:&Union[X,Y],x:Y)->void{\n"
" printf(\"setv Y\\n\");\n"
"}\n"
"fn setv[X,Y](u:&Union[X,Y],x:X)->void{\n"
" printf(\"setv X\\n\");\n"
"}\n"
;


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

enum COMPILE_FLAGS {
	B_AST=0x0001,B_DEFS=0x0002,B_GENERICS=0x0004, B_TYPES=0x0008,B_LLVM=0x0010,B_EXECUTABLE=0x0020,B_RUN=0x0040,B_VERBOSE=0x0080
};
int compile_source(const char *buffer, const char* filename, const char* outname, int flags){

	Lexer	src(buffer,filename);

	auto node=parse_block(src,0,SEMICOLON,nullptr);
	g_pRoot=node;
	Scope global(0); global.node=(ExprBlock*)node; global.global=&global;
	if (flags & B_AST){
		node->dump(0);
	}
	
	node->verify();
	node->resolve(&global,nullptr,0);
	if (flags & B_DEFS){
		global.dump(0);
	}
	if (flags & B_TYPES) {
		node->dump(0);
	}
	node->resolve(&global,nullptr,flags&(B_EXECUTABLE|B_RUN|B_LLVM)?R_FINAL:0);// if we just want to dump/search, we dont fail for final errors.
	if (flags & B_LLVM) {
		output_code(stdout, &global);
	}
	printf("%x\n",flags);
	if (outname && (flags & (B_EXECUTABLE|B_RUN|B_LLVM))){
		FILE* ofp=fopen(outname,"wb");
		if (ofp){
			output_code(ofp, &global);
			fprintf(ofp,"\n;end");
			fclose(ofp);
			if (flags & (B_EXECUTABLE|B_RUN)) {

				char exename[256];
				filename_change_ext(exename,outname,"");
				char compile_cmd[512]; sprintf(compile_cmd,"clang %s -o %s", outname, exename);
				if (flags & B_VERBOSE)printf("\nllvm src=%s\n executable=%s\nflags %x\n",outname,exename, flags);
				if (flags & B_VERBOSE)printf("\n%s\n",compile_cmd);
				auto ret= system(compile_cmd);
				if (!ret && (flags & B_RUN)) {
					if (flags & B_VERBOSE)printf("compiled ok, running executable %s \n", exename);
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
	if (options & B_VERBOSE)printf("compiling %s\n -> %s\n",filename,outname);
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
template<typename X,typename Y>
struct Union{
	int tag;
	union {X x; Y y;};
	template<class FX,class FY,class R> R map(std::function<R(X)> fx,std::function<R(Y)> fy){
		if (tag==0) return fx(x);
		else return fy(y);
	}
};
// Union<int,float> u;
// This is the sort of thing we want our language to handle - works fine in Rust.
// C++ can't infer through to 'R' from the given 'function types', even less so with poly lambdas.
// auto x=u.map([](int x)->int{return 0;}, [](float x)->int{return 1;});
// printf("%d x\n",x);

struct Option{
	char name;int clear;int set; const char* help;
};
Option g_Options[]={
	{'a',0,B_AST,"show AST"},
	{'t',0,B_TYPES,"dump AST annotated with types"},
	{'d',0,B_DEFS,"dump definitions"},
	{'g',0,B_GENERICS,"dump generic type info"},
	{'l',0,B_LLVM,"emit LLVM source"},
	{'r',0,B_RUN|B_EXECUTABLE,"build & run"},
	{'e',0,B_EXECUTABLE,"compile executable"},
	{'v',0,B_VERBOSE,"verbose mode"},
	{'h',0,0,"help"},
	{0,0,0,0}
};
void dump_help(){
	printf("embryonic C++/Rust hybrid language\n");
	printf("(we dont even have a name yet)\n");
	printf("to run: \n");
	printf("   hack srcfile [-options]\n");
	printf("default is compile and run. -e to generate exe and not run.\n");
	printf(" \n");
	
	for (auto opt=g_Options;opt->name;opt++){
		printf("%c - %s\n",opt->name,opt->help);
	}
}

void run_tests(){
	/// TODO , actually verify these produced the right output!
	printf("no sources given so running inbuilt tests.\n");
	printf("typeparam test\n");
	auto ret9=compile_source(g_TestPolyLambda,"g_TestPolyLambda","test9.ll",B_DEFS| B_TYPES|B_RUN);
	auto ret10=compile_source(g_TestIf,"g_TestIf","test10.ll",B_TYPES|B_RUN);
	auto ret3=compile_source(g_TestLoop,"g_TestLoop","test3.ll",B_TYPES|B_RUN);

	auto ret6=compile_source(g_TestAlloc,"g_TestAlloc","test6.ll",B_TYPES|B_RUN);
	auto ret5=compile_source(g_TestProg2,"g_TestProg","test5.ll",B_TYPES|B_RUN);
	auto ret4=compile_source(g_TestClosure,"g_TestClosure","test4.ll",B_TYPES|B_RUN);
	auto ret2=compile_source(g_TestArray,"g_TestArray","test2.ll",B_TYPES|B_RUN);
	auto ret0=compile_source(g_TestBasic,"g_TestBasic","test0.ll",B_TYPES|B_RUN);
	auto ret1=compile_source(g_TestStruct,"g_TestStruct","test1.ll",B_TYPES|B_RUN);
	auto ret7=compile_source(g_TestTyparamInference,"g_TestTyparamInference","test7.ll",B_TYPES|B_RUN);
	auto ret8=compile_source(g_TestMemberFn,"g_TestMemberFn","test8.ll",B_DEFS| B_TYPES|B_RUN);
}

int main(int argc, const char** argv) {
#if DEBUG>=2
	printf("compiled with debug level=%d\n", DEBUG);
#endif
//	dbprintf("precedences: ->%d *%d +%d &%d \n", precedence(ARROW),precedence(MUL),precedence(ADD),precedence(AND));
//	compile_source_file("~/hack/test_hack/prog.rs",0xf);
	int options=0,given_opts=0;
	for (auto i=1; i<argc; i++) {
		const char* a=argv[i];
		if (a[0]=='-'){
			for (auto j=1; a[j];j++){
				if (a[j]=='h') dump_help();
				for (auto opt=g_Options;opt->name;opt++){
					if (opt->name==a[j]) {options&=~opt->clear;options|=opt->set;}
				}
			}
		}
	}
	if (!options){
		options=B_RUN|B_EXECUTABLE;
	}
	
	for (auto i=1; i<argc; i++) {
		if (argv[i][0]!='-') {
			if (options & B_VERBOSE)
				printf("compile src file %s with options %x\n",argv[i],options);
			compile_source_file(argv[i],options);
		}
	}
	if (argc<=1) {
		#if DEBUG>=2
		run_tests();
		#else
		dump_help();
		#endif
	}
}




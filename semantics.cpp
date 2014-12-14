
#include "semantics.h"
#include "codegen.h"
#include "repl.h"
#include "lexer.h"
#include "parser.h"
#include "run_test.h"
#include "error.h"

const char** g_pp,*g_p;
const char* g_filename=0;

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
	0|0,
	0|0,
};
const char* g_token_str[]={
	"",
	"int","uint","size_t","i8","i16","i32","i64","u8","u16","u32","u64","u128","bool",
	"half","float","double","float4",
	"char","str","void","voidptr","one","zero","nullptr","true","false",
	"auto","pTr","ref","Self",
	"tuple","__NUMBER__","__TYPE__","__IDNAME__",
	
	"print___","fn","struct","class","trait","virtual","static","extern", "enum","array","vector","union","variant","with","match","where","sizeof","typeof","nameof","offsetof", "this","self","super","vtableof","closure",
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
	"=",":=","=:","@",
	"+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=", // assign-op
	".=",	// linklist follow ptr.=next
	"++","--","++","--", //inc/dec
	"-","*","&","!","~", // unary ops
	"*?","*!","&?","~[]","[]","&[]", // special pointers?
	",",";",";;",
	"...","..",
	"_","",
	"\"C\"","__vtable_ptr","__env_ptr","__env_i8_ptr",
	NULL,	
};

int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,// int types
	0,0,0,0,  //floats
	0,0,0,0,0,0,0,0,0,
	0,0,0,0,
	0,0,0,0,			// type modifiers
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0, 0,0,0,0,0, // keywords
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
	WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,0, // assignment
	
	WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16, // assign-op
	WRITE_LHS|READ|ASSOC|16, // dot-assign
	MODIFY|PREFIX|UNARY|2,MODIFY|PREFIX|UNARY|2,MODIFY|UNARY|ASSOC|3,MODIFY|UNARY|ASSOC|3, // post/pre inc/dec
	READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3, //unary ops
	READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,READ|UNARY|ASSOC|3, /// special pointers
	0,0,17, // delim
	0,
	0,0,
	0, //placeholder
	0,0,0,0,0
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
	nextId=NUM_STRINGS;
	index_to_name.resize(NUM_STRINGS);
	for (int i=0; i<index_to_name.size(); i++) {
		index_to_name[i]=std::string(g_token_str[i]);
		names.insert(std::make_pair(index_to_name[i],i));
	}
	ASSERT(nextId==NUM_STRINGS);
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

Name getStringIndexConcat(Name base, const char* s2){
	char tmp[512];
	snprintf(tmp,511,"%s%s",str(base),s2);
	return getStringIndex(tmp);
}
Name strConcat(Name n1, Name n2){
	// todo - we could optimize the string table around concatenations
	return getStringIndexConcat(n1,str(n2));
}
const char* getString(const Name& n) {
	auto i=index(n);
	return i?g_Names.index_to_name[i].c_str():"";
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

int get_typeparam_index(const vector<TParamDef*>& tps, Name name) {
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
	if (!a->is_equal(b)){
		if (a->is_coercible(b)){
			return ResolvedType(0,ResolvedType::COMPLETE);
		}
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

void Node::clear_def(){
	if (def)
		def->remove_ref(this);
	def=nullptr;;
}
void Node::set_def(ExprDef *d){
	if (!d && !def)
		return;
	if (!def) {
		this->next_of_def=d->refs; d->refs=this;
		def=d;
	}
	else {
		if (d==0 && this->def){ ASSERT("use clear_def(), not set_def(0)");}
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
#if DEBUG >=2
			scope->dump(0);
#endif
			auto thisptr=scope->find_variable_rec(THIS);
			if (thisptr){
				auto recv=scope->get_receiver();
				thisptr->dump(0);
				scope->dump(0);
				if (!recv){
					dbprintf("warning 'this' but no receiver found via scope\n");
					dbprintf("warning 'this' but no receiver in s=%p owner=%p\n",scope,scope->owner_fn);
					if (scope->owner_fn){
						dbprintf("warning 'this' but no receiver in %s %s\n",scope->owner_fn->kind_str(),scope->name());
						auto fnd=scope->owner_fn->as_fn_def();
						auto p=scope->owner_fn->parent();
						dbprintf("warning 'this' but no receiver in %s %s\n",scope->owner_fn->kind_str(),scope->name());
						
						dbprintf("recv=%p\n",fnd->m_receiver);
						scope->owner_fn->dump(0);
					}
				}
			}
			error(this,scope,"\'%s\' undeclared identifier",str(this->name));
		}
		return ResolvedType();
	}
	return ResolvedType();
}
void ExprIdent::dump(int depth) const {
	if (!this) return;
	newline(depth);dbprintf("%s ",getString(name));
//	if (this->def) {dbprintf("(%s %d)",this->def->pos.line);}
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
	dbprintf(b+0);
	if (this->call_expr){
		dbprintf(this->is_subscript()?"subscript: ":this->is_struct_initializer()?"struct_init":"call ");
		this->call_expr->dump(-100);
//
	} else{
		dbprintf(this->is_array_initializer()?"array_init ":this->is_tuple()?"tuple ":"");
	}
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

bool  Type::has_typeparam(Scope* sc){
	if (this->def) {if (this->def->as_tparam_def()) return true;}
	else
	{
		if (auto tpd=sc->get_typeparam_for(this)){
			this->set_def(tpd);
			return true;
		}
	}

	for (auto s=this->sub; s; s=s->next){
		if (s->has_typeparam(sc))
			return true;
	}
	return false;
}
ResolvedType Type::resolve(Scope* sc,const Type* desired,int flags)
{
	if(!this)return ResolvedType();
	if (!this->struct_def() && this->name>=IDENT && !is_number(this->name)){
		if (!strcmp(str(name),"Union")){
			sc->owner_fn->dump_if(0);
			this->dump(-1);newline(0);
		}
		if (!this->has_typeparam(sc)){
			if (auto sd=sc->find_struct_named(this->name)){
				this->set_struct_def(sd->get_instance(sc,this));
				dbg_instancing("found struct %s in %s ins%p on t %p\n",this->name_str(), sc->name(),this->struct_def(),this);
			}else{
				dbg_instancing("failed to find struct %s in %s\n",this->name_str(), sc->name());
#if DEBUG >=2
				sd=sc->find_struct_named(this->name);
				sc->dump(0);
#endif
			}
		}
	}
#if DEBUG >=2
	dbprintf("%s structdef=%p def= %p\n",this->name_str(),this->struct_def(),this->def);
#endif
	auto ds=desired?desired->sub:nullptr;
	for (auto s=this->sub;s;s=s->next,ds=ds?ds->next:nullptr)
		s->resolve(sc,ds,flags);
	
	return ResolvedType(this,ResolvedType::COMPLETE);
}
ResolvedType ArgDef::resolve(Scope* sc, const Type* desired, int flags){
	dbg_resolve("resolving arg %s\n",this->name_str());
	propogate_type_fwd(flags,this,desired,this->type_ref());
	if (this->type()){
		this->type()->resolve(sc,desired,flags);
	}
	if (this->default_expr){this->default_expr->resolve(sc,this->type(),flags);}
	return ResolvedType(this->type(), ResolvedType::COMPLETE);
}
void Type::clear_struct_def(){
	this->clear_def();
}
void Type::set_struct_def(ExprStructDef* sd){
	this->set_def(sd);
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
	sub=0;
	next=0;
	name=i; //todo: resolve-type should happen here.
}
Type::Type(Node* origin,Name i){
	this->set_origin(origin);
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
ExprStructDef*	Type::struct_def(){
	return this->def?this->def->as_struct_def():nullptr;
}
ExprStructDef*	Type::struct_def() const{
	return const_cast<Type*>(this)->def?this->def->as_struct_def():nullptr;
}

ExprStructDef* Type::struct_def_noderef()const { // without autoderef
//	if (struct_def) return struct_def->as_struct_def();
//	else return nullptr;
	return struct_def();
};
void Node::set_type(const Type* t)
{	::verify(t);
	if (this->m_type){
		if (this->m_type->is_equal(t))
			return ;
#if DEBUG>=2
		dbprintf("changing type?\n");
		this->m_type->dump(-1);newline(0);
		dbprintf("to..\n");
		t->dump(-1);
		newline(0);
#endif
		//ASSERT(this->m_type==0);
	}
	this->m_type=(Type*)t;
};

bool type_is_coercible(const Type* from,const Type* to,bool coerce){
	// void pointers auto-coerce like they should,
	// thats what they're there for, legacy C
	// modern code just doesn't use void*
	
	if (from->is_pointer_not_ref() && to->is_pointer_not_ref() && coerce){
		if (from->is_void_ptr() || to->is_void_ptr())
			return true;
	}
	// coercible struct-pointers with inheritance
	auto s1=from->struct_def_noderef();
	auto s2= to->struct_def_noderef();
	if (s1&&s2 && coerce){
		if (!s1->has_base_class(s2))
			return true;
	}

	// TODO: 'intersection type' component coercion ?
	// int coercions
	// float

	if ((from->is_pointer_not_ref() || from->is_int()) && to->is_bool())
		return true;
	if (to->size() >= from->size()) {
		if (from->is_float() && to->is_float()){
			return true;
		}
		if (from->is_int() && to->is_int()){
			if (from->is_signed() && to->is_signed())
				return true;
			if (!(from->is_signed() || to->is_signed()))
				return true;
		}
	}
	if (to->size() > from->size()) {
		if (from->is_int() && to->is_int())
			return true;
	}
	return false;
}
bool Type::is_equal(const Type* other,bool coerce) const{
	/// TODO factor out common logic, is_coercible(),eq(),eq(,xlat)
	if ((!this) && (!other)) return true;
	// if its' auto[...] match contents; if its plain auto, match anything.
	if (this&&this->name==AUTO){
		if (this->sub && other) return this->sub->is_equal(other->sub,coerce);
		else return true;
	}
	if (other && other->name==AUTO){
		if (other->sub && this) return other->sub->is_equal(this->sub,coerce);
		else return true;
	}
	if (!(this && other)) return false;
	
	if (type_is_coercible(this,other,coerce))
		return true;
	else
		if (this->name!=other->name)return false;

//	if (!this->sub && other->sub)) return true;
	if (other->name==STR && type_compare(this,PTR,CHAR)) return true;
	if (this->name==STR && type_compare(other,PTR,CHAR)) return true;
	
	auto p=this->sub,o=other->sub;
		
	for (; p && o; p=p->next,o=o->next) {
		if (!p->is_equal(o,coerce)) return false;
	}
	if (o || p) return false; // didnt reach both..
	return true;
}
bool Type::is_equal(const Type* other,const TypeParamXlat& xlat) const{
	if ((!this) && (!other)) return true;
	// if its' auto[...] match contents; if its plain auto, match anything.
	if (this &&this->name==AUTO){
		if (this->sub && other) return this->sub->is_equal(other->sub,xlat);
		else return true;
	}
	if (other && other->name==AUTO){
		if (other->sub && this) return other->sub->is_equal(this->sub,xlat);
		else return true;
	}
	if (!(this && other))
		return false;

	// TODO: might be more subtle than this for HKT
	auto ti=xlat.typeparam_index(other->name);
	dbg_type("%s %s\n",str(this->name),str(other->name));
	if (ti>=0){
		return this->is_equal(xlat.given_types[ti],xlat);
	}
	ti=xlat.typeparam_index(this->name);
	if (ti>=0){
		return xlat.given_types[ti]->is_equal(other,xlat);
	}

	if (this->name!=other->name)return false;
	//	if (!this->sub && other->sub)) return true;
	if (other->name==STR && type_compare(this,PTR,CHAR)) return true;
	if (this->name==STR && type_compare(other,PTR,CHAR)) return true;
	
	if (type_is_coercible(this,other,true))
		return true;

	auto p=this->sub,o=other->sub;
	
	for (; p && o; p=p->next,o=o->next) {
		if (!p->is_equal(o,xlat)) return false;
	}
	if (o || p) return false; // didnt reach both..
	return true;
}

void Type::dump_sub(int flags)const{
	if (!this) return;
	if (this->name==TUPLE) {
		dbprintf("(");
		for (auto t=sub; t; t=t->next){
			t->dump_sub(flags);
			if(t->next)dbprintf(",");
		};
		dbprintf(")");
	} else{
		dbprintf("%s",getString(name));
#if DEBUG>=2
		if (this->struct_def())
			dbprintf("( struct_def=%s )", str(this->struct_def()->get_mangled_name()));
		if (this->def)
			dbprintf("( def=%s )", str(this->def->get_mangled_name()));
#endif
		if (sub){
			dbprintf("[");
			for (auto t=sub; t; t=t->next){
				t->dump_sub(flags);
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
// todo table of each 'intrinsic type', and pointer to it
Type* g_bool,*g_void,*g_void_ptr,*g_int;
Type* Type::get_bool(){
	/// todo type hash on inbuilt indices
	if (g_bool)return g_bool;
	return (g_bool=new Type(nullptr,BOOL));
}
Type* Type::get_void(){
	if (g_void)return g_void;
	return (g_void=new Type(nullptr,VOID));
}
Type* Type::get_int(){
	if (g_int)return g_int;
	return (g_int=new Type(nullptr,INT));
}
Type* Type::get_void_ptr(){
	if (g_void_ptr)return g_void_ptr;
	return (g_void_ptr=new Type(nullptr,PTR,VOID));
}

bool Type::is_struct()const{
	return struct_def()!=0 || name>=IDENT; //TODO .. it might be a typedef.
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
	if (this->struct_def()){
		return this->struct_def()->alignment();
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
	if (this->struct_def()){
		return struct_def()->size();
	}
	return 0;
}
bool
ExprStructDef::has_base_class(ExprStructDef* other)const{
	for (auto x=this; x;x=x->inherits)
		if (x==other)
			return true;
	return false;
}

ExprStructDef* Type::get_struct_autoderef()const{
	auto p=this;
//	while (p && !p->is_struct()){
	while (p->is_qualifier_or_ptr_or_ref()){
		p=p->sub;
	}
	return p->struct_def();
}
ExprStructDef* Type::get_receiver()const
{
	if (this->sub)
		if (this->sub->next)
			if (this->sub->next->next)
				return this->sub->next->next->struct_def();
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
	newline(depth);dump_sub(depth);
}
Type::Type(ExprStructDef* sd)
{	set_struct_def(sd); name=sd->name; sub=0; next=0;
}
Type::Type(Name outer_name,ExprStructDef* sd)
{
	name=outer_name;
	push_back(new Type(sd));
}

void ExprLiteral::dump(int depth) const{
	if (!this) return;
	newline(depth);
	if (type_id==T_VOIDPTR){dbprintf("%p:*void",u.val_ptr);}
	if (type_id==T_BOOL){dbprintf(u.val_bool?"true":"false");}
	if (type_id==T_VOID){dbprintf("void");}
	if (type_id==T_INT){dbprintf("%d",u.val_int);}
	if (type_id==T_FLOAT){dbprintf("%.7f",u.val_float);}
	if (type_id==T_KEYWORD){dbprintf("%s",str(u.val_keyword));}
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
void dump_typeparams(const vector<TParamDef*>& ts) {
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
Node*	TParamDef::clone() const
{	return new TParamDef(this->pos,this->name, (TParamVal*) (this->bound->clone_if()),(TParamVal*) (this->defaultv->clone_if()));
}

void TParamDef::dump(int depth) const {
	newline(depth);dbprintf("%s",str(name));
	if (defaultv) {dbprintf("=");defaultv->dump(-1);}
}
const char* ArgDef::kind_str()const{return"arg_def";}

// the operators should all just be functioncalls, really.
// return type of function definition is of course a function object.
// if we make these things inline, we create Lambdas
// todo: receivers.

bool ExprBlock::is_undefined() const{
	if (!this) return false; //only presencence of "_" is undefined.
	for (auto x:argls){
		if (x->is_undefined())
			return true;
	}
	return false;
}

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

Capture* ExprFnDef::get_or_create_capture(ExprFnDef* src){
	if (!this->my_capture) {
		auto c=new Capture;
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

ExprFnDef* NamedItems::getByName(Name n){
	for(auto f=this->fn_defs;f;f=f->next_of_name){
		if (f->name==n)
			return f;
	}
	return nullptr;
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
	r->set_struct_def(this->struct_def());
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

//void match_generic_type_param_sub(const vector<TParamDef>& tps, vector<Type*>& mtps, const Type* to_match, const Type* given) {
	
//}

int match_typeparams_from_arg(vector<TParamVal*>& matched_tps, const vector<TParamDef*>& fn_tps,  const Type* fn_arg, const Type* given_arg)
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
			ret_score+=match_typeparams_from_arg(matched_tps,fn_tps, sub1, sub2);
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

int match_typeparams(vector<TParamVal*>& matched, const ExprFnDef* f, const ExprBlock* callsite){
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
	dbg_fnmatch("score matching gets %d\n",score);
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
			if (f->args[j]->get_type()->is_equal(at)){
				if (j==i) score+=(1+args.size()-i); // args in right pos score higher
				score++;
				break;
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
			if (f->args[i]->get_type()->is_equal(args[i]->get_type())) {
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
		dbg_fnmatch("%s score=%d; before typaram match\n",str(f->name),score);
		dbg_fnmatch("callsite:\n");
		for (int i=0; i<callsite->as_block()->argls.size();i++) {
			dbg_fnmatch("arg %s:",  str(args[i]->name));
			callsite->as_block()->argls[i]->type()->dump_if(-1);
			dbg_fnmatch("\tvs\t");
			f->args[i]->type()->dump_if(-1);
			dbg_fnmatch("\n");
		}
		dbg_fnmatch("\n");
#endif
		for (int i=0; i<args.size() && i<f->args.size(); i++) {
			score+=match_typeparams_from_arg(matched_type_params,f->typeparams, f->args[i]->get_type(), args[i]->get_type());
		}
		score+=match_typeparams_from_arg(matched_type_params, f->typeparams,f->ret_type,ret_type);
		dbg_fnmatch("typaram matcher for %s\n",f->name_str());
		dbg_fnmatch("%s:%d: %s\n",g_filename,f->pos.line,str(f->name));
		dbg_fnmatch("%s score=%d; matched typeparams{:-\n",str(f->name),score);
		for (auto i=0; i<f->typeparams.size(); i++){
			dbg_fnmatch("[%d]%s = %s;\n", i,str(f->typeparams[i]->name),matched_type_params[i]?str(matched_type_params[i]->name):"" );
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
			error_begin(callsite,(name!=PLACEHOLDER)?"unmatched call %s":"possible calls for %s",str(name));
			// For the best match, say what you'd have to do to fix, then show all matches
			if (args.size()<best.f->min_args()){info(best.f,"maybe requires %d args, %d given",best.f->min_args(),args.size());}
			vector<Type*> callsite_tys;
			match_typeparams(callsite_tys, best.f,callsite->as_block());
			auto tpxlat=TypeParamXlat{best.f->typeparams,callsite_tys};
			for (auto i=0; i<args.size() && i<best.f->args.size(); i++){

				if (!args[i]->type()->is_equal(best.f->args[i]->type(),tpxlat)){
					info(best.f->args[i],"maybe arg %d should be ",i); best.f->args[i]->type()->dump_if(-1);
					info(args[i],"was given "); args[i]->type()->dump_if(-1);newline(0);
					tpxlat.dump(0);
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
bool Type::has_non_instanced_typeparams()const{ if (!def) return true; if (def->as_tparam_def()) return false; return true;}

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
			dbg_varscope("Found Captured Var %s in %s %s\n",str(name),this->name(),str(v->capture_in->name));
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
			if (flags & R_FINAL){
				if (!loop && flags&R_FINAL) {
					error(this,"break without loop");
				}
				if (rhs && !loop->else_block){
					error(this,"break <expression> requires else block with alternate return expression, same type.");
				}
				if (loop->else_block){
					propogate_type(flags,(Node*)this,rhs->type_ref(),loop->else_block->type_ref());
				}
			}
			propogate_type(flags,(Node*)this, rhs->type_ref(),loop->type_ref());
			propogate_type(flags,(Node*)this,this->type_ref(),this->rhs->type_ref());
		}
		return propogate_type_fwd(flags, this, desired, this->type_ref());
	}

	if (op_ident==ASSIGN || op_ident==LET_ASSIGN || op_ident==DECLARE_WITH_TYPE) {
		if (op_ident==LET_ASSIGN){
			ASSERT(this->lhs && this->rhs);
			rhs->resolve(sc,desired,flags);
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			if (desired) {
				desired->dump(-1);
			}
			auto rhs_t = rhs->get_type();
			auto new_var=sc->create_variable(this,vname,Local);
			lhs->set_def(new_var);
			new_var->force_type_todo_verify(rhs_t);
			lhs->set_type(rhs_t);
			this->set_type(rhs_t);
			propogate_type_fwd(flags, this, desired, lhs->type_ref());
			return 	propogate_type_fwd(flags, this, desired, this->type_ref());
		}
		else if (op_ident==DECLARE_WITH_TYPE){ // create a var, of given type,like let lhs:rhs;
			auto vname=lhs->as_name();	//todo: rvalue malarchy.
			// todo: get this in the main parser
			auto lhsi=expect_cast<ExprIdent>(lhs);
			//			auto v=sc->find_variable_rec(this->argls[0]->name);
			auto v=sc->get_or_create_scope_variable(this,lhsi->name,Local);
			auto t=v->get_type();
			if (rhs){
				rhs->resolve(sc,desired,flags);
				t=expect_cast<Type>(rhs);
				v->set_type(t);
			}
			lhs->set_def(v);
			if (t){
				if (t->name>=IDENT && !t->sub) {
					t->set_struct_def(sc->find_struct(t));
				}
			}
			if (auto t=v->get_type()) {
				sc->try_find_struct(t);// instantiate
			}
			return propogate_type(flags, this, v->type_ref(),type_ref());
		}
		else if (op_ident==ASSIGN){
			ASSERT(this->lhs && this->rhs);
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
			if (auto t=this->rhs->type())
				this->set_type(t);
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
	this->def->resolve_if(sc, nullptr, flags);
	

	/// loose end? if this is a method-call, we dont resolve the symbol here,
	/// in other contexts we do
	if (this->call_expr &&!receiver)
		this->call_expr->resolve(sc,nullptr,flags);
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
				if (!ret.type->is_equal(desired)){
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
			return propogate_type(flags,(const Node*)this, this->type_ref(),this->argls.back()->type_ref());
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
			for (auto a=fn_type->fn_args_first(); arg_index<argls.size() && a; arg_index++,a=a->next)  {
				if (a->name==FN){
					dbg_lambdas("resolving fn type into function argument %s\n", argls[arg_index]->name_str());
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
#if DEBUG>4
	for (int i=0; i<block->argls.size(); i++) {
		dbprintf("arg[i]=",i);
		block->argls[i]->dump_if(-1);newline(0);
	}
#endif
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

void call_graph(Node* root,Scope* scope) {
}

void unexpected(int t){error(0,"unexpected %s\n",getString(t));}


Node*Pattern::clone() const{
	auto np=new Pattern(); np->pos=pos;
	np->next=(Pattern*)np->clone_if();// todo not recursive!!
	np->sub=(Pattern*)np->clone_if();
	return np;
}

Node*
ExprMatch::clone()const{
	auto m=new ExprMatch();
	m->pos=pos;
	m->expr=(Expr*)expr->clone_if();
	m->arms=(MatchArm*)arms->clone_if();
	return m;
}
Node* MatchArm::clone()const{
	auto a=new MatchArm();
	a->pos=pos;
	
	a->body=(Expr*)body->clone();
	a->next=(MatchArm*)next->clone_if();//TODO not recursive ffs.
	return a;
}

void
gather_vtable(ExprStructDef* d) {
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
void ExprIf::translate_typeparams(const TypeParamXlat& tpx){
	this->cond->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
}
void ExprFor::translate_typeparams(const TypeParamXlat& tpx)
{
	this->init->translate_typeparams_if(tpx);
	this->cond->translate_typeparams_if(tpx);
	this->incr->translate_typeparams_if(tpx);
	this->pattern->translate_typeparams_if(tpx);
	this->body->translate_typeparams_if(tpx);
	this->else_block->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
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
	this->type()->translate_typeparams_if(tpx);
}

void ExprBlock::translate_typeparams(const TypeParamXlat& tpx){
	this->call_expr->translate_typeparams_if(tpx);
	for (auto e:argls){
		e->translate_typeparams(tpx);
	}
	this->type()->translate_typeparams_if(tpx);
}

void ExprOp::translate_typeparams(const TypeParamXlat& tpx){
	lhs->translate_typeparams_if(tpx);
	rhs->translate_typeparams_if(tpx);
	this->type()->translate_typeparams_if(tpx);
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
	for (auto a:this->fields)		a->translate_typeparams(tpx);
	for (auto f:functions)			f->translate_typeparams(tpx);
	for (auto f:virtual_functions)	f->translate_typeparams(tpx);
	for (auto f:static_functions)	f->translate_typeparams(tpx);
	for (auto f:static_fields)		f->translate_typeparams(tpx);
	for (auto f:static_virtual)		f->translate_typeparams(tpx);
	for (auto s:structs)			s->translate_typeparams(tpx);
	if (tpx.typeparams_all_set())
		this->typeparams.resize(0);
	this->type()->translate_typeparams_if(tpx);
}
ExprStructDef* ArgDef::member_of()	{ // todo, implement for 'Variable' aswell, unify capture & member-object.
	if (owner) return owner->get_receiver();
	return nullptr;
}

void ArgDef::translate_typeparams(const TypeParamXlat& tpx){
	this->name.translate_typeparams(tpx);
	if (this->get_type()){
		this->get_type()->set_struct_def(nullptr); // needs resolving again
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
void TypeParamXlat::dump(int depth)const{
	dbprintf("[");
	for (auto i=0; i<this->typeparams.size();i++){
		if (i)dbprintf(",");
		dbprintf("%s=",str(this->typeparams[i]->name));
		this->given_types[i]->dump_if(-1);
	}
	dbprintf("]");
}
void Type::translate_typeparams(const TypeParamXlat& tpx){
	this->translate_typeparams_sub(tpx,nullptr);
}
void Type::translate_typeparams_sub(const TypeParamXlat& tpx,Type* inherit_replace){
	// todo: replace wih 'instantiate' typparams, given complex cases
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
	this->clear_struct_def();
	int param_index=tpx.typeparam_index(this->name);
	if ((this->name==PLACEHOLDER || this->name==AUTO) && inherit_replace){
		this->name=inherit_replace->name;
		if (!this->sub && inherit_replace->sub){
			error(this,"TODO - replace an auto typeparam with complex given typeparam ");
		}
	}
	if (param_index>=0){
		auto pi=param_index;
		auto src_ty=tpx.given_types[pi];
		if (!src_ty){error(this,"typaram not given,partial instance?");}
		if (!src_ty->sub) {
			this->name=src_ty->name;
		} else if (!this->sub){
			this->name=src_ty->name;
			for (auto s=src_ty->sub;s;s=s->next){
				this->push_back((Type*)s->clone());
			};
		}
		else {
			#if DEBUG>=2
			tpx.dump(0);
			tpx.typeparams[pi]->dump(-1);
			dbg_generic(" replace with ");
			tpx.given_types[pi]->dump(-1);newline(0);
			newline(0);
			dbg_generic("substituting in:");
			this->dump(-1);
			newline(0);
			#endif
			//error_begin(this,"param index %d %s trying to instantiate complex typeparameter into non-root of another complex typeparameter,we dont support this yet\n",param_index, tpx.typeparams[pi]->name_str());
			
			//error_end(this);
			this->name=src_ty->name;
			auto inherit_sub=inherit_replace?inherit_replace->sub:nullptr;
			auto* pps=&this->sub;
			for (auto s=this->sub; s; pps=&s,s=s->next){
				s->translate_typeparams_sub(tpx,inherit_sub);
				inherit_sub=inherit_sub?inherit_sub->next:nullptr;
			}

			#if DEBUG>=2
			dbg_generic("result:");
			this->dump(-1);
			newline(0);
			#endif
		}
	}
	//this->struct_def=tpx.
	// translate child elems.
	auto inherit_sub=inherit_replace?inherit_replace->sub:nullptr;
	auto* pps=&this->sub;
	for (auto sub=this->sub; sub; pps=&sub->next,sub=*pps) {
		sub->translate_typeparams_sub(tpx,inherit_sub);
		inherit_sub=inherit_sub?inherit_sub->next:nullptr;
	}
	// replace any not give with inherit..
	while (inherit_sub){
		*pps = (Type*)inherit_sub->clone();
		inherit_sub=inherit_sub->next;
		pps=&((*pps)->next);
	}
}
bool type_params_eq(const vector<Type*>& a, const vector<Type*>& b) {
	if (a.size()!=b.size()) return false;
	for (int i=0; i<a.size(); i++) { if (!a[i]->is_equal(b[i])) return false;}
	return true;
}
bool type_params_eq(const vector<Type*>& a, const Type* tp){
	for (auto
		 i=0; i<a.size() && tp;i++,tp=tp->next){
		if (!a[i]->is_equal(tp))
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
#if DEBUG>=2
		dbg_instancing("instantiating struct %s[",this->name_str());
		for (auto t=type->sub;t;t=t->next)dbg_instancing("%s,",t->name_str());
		dbg_instancing("]\n");
#endif
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
	if (!type->struct_def()) { const_cast<Type*>(type)->set_struct_def(ins);}
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
	for (auto f:this->virtual_functions){d->virtual_functions.push_back((ExprFnDef*)f->clone());}
	for (auto f:this->static_functions){d->static_functions.push_back((ExprFnDef*)f->clone());}
	for (auto f:this->static_fields){d->static_fields.push_back((ArgDef*)f->clone());}
	for (auto f:this->static_virtual){d->static_virtual.push_back((ArgDef*)f->clone());}
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

ExprStructDef* ExprStructDef::root_class(){
	for (auto s=this; s;s=s->inherits){
		if (!s->inherits)
			return s;
	}
	return this;
}
void ExprStructDef::roll_vtable() {
	
	if (this->is_vtable_built()){// ToDO: && is-class. and differentiate virtual functions. For the
		return;
	}
	dbg_vtable("rolling vtable for %s,inherits %s\n",str(this->name),this->inherits?str(this->inherits->name):"void");
	if (this->vtable){ return;} // done already
	if (this->inherits) {this->inherits->roll_vtable();}

	this->vtable_name=getStringIndexConcat(name, "__vtable_instance");

	// todo - it should be namespaced..
	//this->vtable->scope=this->scope;

	// todo: we will create a global for the vtable
	// we want to be able to emulate rust trait-objects
	// & hotswap vtables at runtime for statemachines

	// todo: this is a simplification - only the class root describes the vtable.
	auto root=this->root_class();
	if (root!=this){
		this->vtable=root->vtable;
	}
	else{
		if (!this->virtual_functions.size())
			return;
		this->vtable=new ExprStructDef(this->pos,getStringIndexConcat(name,"__vtable_format"));
		this->vtable->vtable_name=getStringIndex("void__vtable");

		for (auto f:this->virtual_functions) {
			// todo: static-virtual fields go here!
			this->vtable->fields.push_back(
					new ArgDef(
					this->pos,
					f->name,
					f->fn_type,
					new ExprIdent(this->pos, f->name)
				)
			);
		}
		for (auto svf:static_virtual){
			this->vtable->fields.push_back(svf);
		}
	}
	// base class gets a vtable pointer
	if (this->vtable){
		this->fields.insert(
			this->fields.begin(),
			new ArgDef(pos,__VTABLE_PTR,new Type(PTR,this->vtable)));
	}

	// TODO - more metadata to come here. struct layout; pointers,message-map,'isa'??
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
	for (auto f:this->virtual_functions){f->dump(depth+1);}
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
		for (auto m:fields)			{m->resolve(sc,nullptr,flags);}
		for (auto m:static_fields)	{m->resolve(sc,nullptr,flags);}
		for (auto m:static_virtual)	{m->resolve(sc,nullptr,flags);}
		for (auto s:structs){
			s->resolve(sc,nullptr,flags);
		}
		for (auto f:functions){
			f->resolve(sc,nullptr,flags);
		}
		for (auto f:virtual_functions){
			f->resolve(sc,nullptr,flags);
		}

		if (this->inherits_type && !this->inherits){
			this->inherits_type->resolve(definer_scope,desired,flags);
			this->inherits=definer_scope->find_struct_named(this->inherits_type->name);
		}
		roll_vtable();
		
		/// TODO clarify that we dont resolve a vtable.
		//if (this->vtable) this->vtable->resolve(definer_scope,desired,flags);
	} else{
		for (auto ins=this->instances; ins; ins=ins->next_instance)
			ins->resolve(definer_scope,nullptr, flags);
	}

	return propogate_type_fwd(flags,this, desired);
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
	newline(depth);dbprintf("(if\n");
	cond->dump(depth+1);
	newline(depth);dbprintf("{\n");
	body->dump(depth+1);
	if (else_block)	{
		newline(depth);dbprintf("}else{\n");
		else_block->dump(depth+1);
	}
	newline(depth);dbprintf("})\n");
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
		this->body->type()->dump_if(0);
		this->else_block->type()->dump_if(0);
		this->type()->dump_if(0);
#endif
		return propogate_type(flags,this, this->type_ref(), this->body->type_ref());
	}
	else {
		// TODO: Could it actually return Body|void ? perhaps we could implicityly ask for that?
		return body_type;
	}
}

void ExprFor::dump(int d) const {
	newline(d);dbprintf("(for ");
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
	newline(d);dbprintf(")");
	if(this->type()){
		dbprintf(":");
		this->type()->dump_if(d);
	}
}





#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>

#ifdef DEBUG
#define ASSERT(x) if (!(auto p=(x))) {printf("error %s:%d:%d %s\n",__FILE__,__LINE__,p, #X );exit(0);}
#else
#define ASSERT(x)
#endif
/*

struct Foo {
	Vertex& vertices	//& = non-zero-pointer
	Vertex* vertices	// *= zero pointer
	Vertex~ vertices	// ~ Owning pointer.
	int[]~	indices		// Slice
	int[..]	indices		// growable array
}
// you see.. you start needing names for all these.

slice<int>	indices
vector<int>	indices;
pwn<Foo>	other_object;	// another object we refer to.		
 */

#define ilist initializer_list

using std::string;
using std::vector;
using std::cout;
using std::map;
using std::initializer_list;
template<typename T,typename S>
T& operator<<(T& dst, const vector<S>&src) { for (auto &x:src){dst<<x;};return dst;};


template<class T,class Y> T* isa(const Y& src){ return dynamic_cast<T>(src);}

enum Token {
	NONE=0,
	// top level structs & keywords
	PRINT,FN,STRUCT,LET,SET,VAR,WHILE,IF,ELSE,DO,FOR,IN,RETURN,BREAK,
	INT,FLOAT,STR,VOID,
	// delimiters
	OPEN_PAREN,CLOSE_PAREN,
	OPEN_BRACE,CLOSE_BRACE,
	OPEN_BRACKET,CLOSE_BRACKET,
	// operators
	LETASSIGN,ARROW,
	COLON,ASSIGN,
	ADD,SUB,MUL,DIV,DOT,
	LT,GT,LE,GE,EQ,NE,LOGAND,LOGOR,
	AND,OR,XOR,MOD,SHL,SHR,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,AND_ASSIGN,OR_ASSIGN,
	COMMA,SEMICOLON,
	// after these indices, comes indents
	IDENT
};
const char* g_token_str[]={
	"",
	"print","fn","struct","let","set","var","while","if","else","do","for","in","return","break",
	"int","float","str","void",
	"(",")",
	"{","}",
	"[","]",
	":=","->",
	":","=","+","-","*","/",".",
	"<",">","<=",">=","==","!=","&&","||",
	"&","|","^","%","<<",">>",
	"+=","-=","*=","/=","<<=",">>=","&=","|=",
	",",";",
	NULL,
	
};
int g_precedence[]={
    0, 1,2,2,3,3,4
};

struct StringTable {
	int	nextId= 0;
	bool verbose;
	map<string,int>	names;
	vector<string> index_to_name; //one should be index into other lol

	StringTable(const char** initial){
		verbose=false;
		nextId=0;
		for (int i=0; g_token_str[i];i++) {
			auto tmp=g_token_str[i];
			get_index(tmp,tmp+strlen(tmp));
		}
		ASSERT(nextId==IDENT);
	}
	int get_index(const char* str, const char* end) {
		int len=end-str;
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
	void dump();
};

void StringTable::dump(){
	printf("\n");
	for (int i=0; i<this->nextId; i++) {
		printf("[%d]%s\n",i,this->index_to_name[i].c_str());
	}
};


StringTable g_Names(g_token_str);
int getStringIndex(const char* str,const char* end=0) {
	if (!end) end=str+strlen(str);
	return g_Names.get_index(str, end);
}
const char* getString(int index) {
	return g_Names.index_to_name[index].c_str();
}




// (tuple x y z) // grabs these fuckers as a felcher.
//
// its not going to be so bad.
// 1000 lines: AST boiler-plate
// 1000 lines: parser
// 1000 lines: inference
// 1000 lines: convert to C++
// 1000 lines: error messages. 
// Its just a code-generator for C++
// with a syntax a little like rust.
void indent(int depth) {
	for (int i=0; i<depth; i++){printf("\t");};
}
// todo: path malarchy.
typedef int Name;

struct CallScope;
class Node {
public:
	virtual  ~Node(){}; // node ID'd by vtable.
	virtual void dump(int depth=0) const{};
	virtual void resolve_calls(CallScope* scope){printf("empty?");};
	virtual const char* kind_str(){return"node";}
	virtual int get_name() const{return 0;}
	const char* get_name_str()const{return getString(get_name());}
};
// Even a block is an evaluatable expression.
// it may contain outer level statements.

struct Expr : Node{
	void dump(int depth) const {
		indent(depth);printf("?\n");
	}
	virtual const char* kind_str()const{return"expr";}
};
// (do..) (fname ...) (operator....)
// todo: specialize those cases.
struct ExprScopeBlock : public Expr{};
struct ExprIdent;
struct Call;
struct ExprIdent :Expr{
	int get_name()const{return ident;}
	int ident;
	void dump(int depth) const {
		indent(depth);printf("%s\n",getString(ident));
	}
	virtual const char* kind_str()const{return"ident";}
	ExprIdent(int i){ident=i;}
};

struct ExprBlock :public ExprScopeBlock{
	vector<Expr*>	args;
	Call* calls;
	int get_name()const{
		if (args.size()>0) return args[0]->get_name();
		else return 0;	
	}
	void dump(int depth) const {
		indent(depth);printf("(\n",args.size());
		for (const auto x:args) { x->dump(depth+1);}
		indent(depth);printf(")\n");
	}
	void resolve_calls(CallScope* scope);
	virtual const char* kind_str()const{return"block";}
	ExprBlock(){calls=0;}
};


enum TypeId{
	T_AUTO,T_KEYWORD,T_VOID,T_INT,T_FLOAT,T_CONST_STRING,T_CHAR,T_PTR,T_STRUCT,T_FN
};
struct StructDef;
struct Type : Node{
	TypeId type;
	int ident;
	StructDef* struct_def;
	Type* sub;

	virtual const char* kind_str()const{return"type";}
	Type(TypeId tid,int i){ struct_def=0; sub=0; type=tid;ident=i; //todo: resolve-type should happen here.
	}
	void dump(int depth)const{
		indent(depth);
		printf(getString(ident));
		if (depth>=0) printf("\n");
	}
};

bool g_lisp_mode=true;
struct Module;
// module base: struct(holds fns,structs), function(local fns), raw module.
struct StructDef;
struct ExprFnDef;
struct VarDecl;
struct ModuleBase : Expr {
	Name	name;
	ModuleBase* parent;
	Module*	modules;
	StructDef* structs;
	ExprFnDef*	functions;
	VarDecl* vars;
	ModuleBase(){
		vars=0;functions=0;structs=0;modules=0;
		parent=0;
	};
	virtual ModuleBase* get_next_of_module()const{ASSERT(0);}
	virtual const char* kind_str()const{return"mod";}
};
struct Module : ModuleBase {
	Module* next_of_module;
	Module* get_next_of_module()const{return next_of_module;}
};
struct ExprLiteral : Expr {
	TypeId	type;
	union  {int val_int; float val_float; const char* val_str;} u;
	virtual const char* kind_str()const{return"lit";}
	void dump(int depth) const{
		indent(depth);
		if (type==T_VOID){printf("void");}
		if (type==T_INT){printf("%d",u.val_int);}
		if (type==T_FLOAT){printf("%.7f",u.val_float);}
		if (type==T_CONST_STRING){printf("%s",u.val_str);}
		if (depth>0)printf("\n");
	}

	ExprLiteral(float f) {
		type=T_FLOAT;
		u.val_float=f;
		printf("lit float %.3f",f);
	}
	ExprLiteral(int i) {
		type=T_INT;
		u.val_int=i;
	}
	ExprLiteral(const char* start,int length) {
		type=T_CONST_STRING;
		auto str=( char*)malloc(length+1); ;
		u.val_str=str;memcpy(str,(void*)start,length);
		str[length]=0;
	}
	~ExprLiteral(){
		if (type==T_CONST_STRING) {
			free((void*)u.val_str);
		}
	}

};

struct ArgDef :Node{
	Name name;
	Type* type;
	Expr* default_value;
	ArgDef(){type=0;default_value=0;};
	ArgDef(int i,Type* t):ArgDef(){name=i;type=t;printf("\nMAKE ARG %d %d\n", i, t?t->ident:0);}
	void dump(int depth) const {
		indent(depth);printf("%s ",getString(name));
		if (type) type->dump(-1);
		if (default_value) default_value->dump(-1);
		if (depth>0) printf("\n");
	}
	virtual const char* kind_str()const{return"arg_def";}
	~ArgDef(){}
};

struct StructDef : ModuleBase {
	vector<ArgDef> fields;
	virtual const char* kind_str()const{return"struct";}
	StructDef* next_of_module;
	ModuleBase* get_next_of_module(){return this->next_of_module;}
};




struct Ident : Expr {
	Ident(Name n) :name(n){};
	Name name;
	void dump(int depth) const {
		indent(depth);printf("ident:%s\n",getString(name));
	}
	~Ident(){}
};





// the operators should all just be functioncalls, really.
// return type of function definition is of course a functoin object.
// if we make these things inline, we create Lambdas
// todo: receivers.
struct FnInstance; struct Call;
struct ExprFnDef :public Module {
	ExprFnDef* next_of_module;
	
	Name name;
	vector<ArgDef*> args;
	ExprBlock* body;
	Call* callers;	// linklist of callers to here
	int get_name()const {return name;}
	virtual const char* kind_str()const{return"fn";}
	ExprFnDef(Name n, ilist<ArgDef> a, ExprBlock* b):name(n),body(b),instance(0),callers(0){
	}
	ExprFnDef(){next_of_module=0;name=0;body=0;callers=0;}
	void dump(int ind) const {
		indent(ind);printf("(fn %s(",getString(name));
		for (int i=0; i<args.size();i++){
			args[i]->dump(-1);
			printf(", ");
		}
		printf(")\n");
		this->body->dump(ind+1);
		indent(ind);printf(")\n");
	}
	void resolve_calls(CallScope* scope);
	FnInstance* instance;
};

struct FnName {
	ExprFnDef*	defs;
	ExprFnDef*	getByName(Name n);
	FnInstance* resolve(Call* site);
};
vector<FnName> g_functions;
/*
// todo: assoc, precedence here.
#define BIN_OP(NAME,OP) \
struct NAME : public ExprBinOp { \
	const char* name()const{return #OP;}; \
	~NAME(){}; \
	NAME(Expr*l, Expr* r):ExprBinOp(l,r){} \
};
BIN_OP(ExprAssign,=)
BIN_OP(ExprLetAssign,:=)
BIN_OP(ExprAddAssign,+=)
BIN_OP(ExprSubAssign,-=)
BIN_OP(ExprMulAssign,*=)
BIN_OP(ExprDivAssign,/=)
BIN_OP(ExprAdd,+)
BIN_OP(ExprSub,-)
BIN_OP(ExprMul,*)
BIN_OP(ExprDiv,/)
BIN_OP(ExprAsl,<<)
BIN_OP(ExprAsr,>>)
BIN_OP(ExprAnd,&)
BIN_OP(ExprOr,|)
BIN_OP(ExprXor,^)
BIN_OP(ExprFunction,->)
BIN_OP(ExprMapTo,=>)
BIN_OP(ExprSupertype,<:)
BIN_OP(ExprSubtype,:>)
BIN_OP(ExprType,:)
BIN_OP(ExprEq,==)
BIN_OP(ExprLt,<)
BIN_OP(ExprGt,>)
BIN_OP(ExprLe,<=)
BIN_OP(ExprGe,>=)
BIN_OP(ExprNe,!=)
*/

// comma operator makes tuples.
// Ident[x,y,z](..) = typeparams
// Ident[]{...}  = typeparams, struct initializer
// ident(expr) = indexing
// .[i] is indexing operator.
// Blah.[i]
// T[] is dynamic array
// T[4] is fixed array

struct ExprIf : public Expr {
	Expr* cond=0;
	Expr* if_block=0;
	Expr* else_block=0;
	void dump(int depth) const {
		indent(depth);printf("If:\n");
		cond->dump(depth+1);
		if_block->dump(depth+1);
		if (else_block)	{
			indent(depth);printf("Else:\n");
			else_block->dump(depth+1); 
		}
	};
	~ExprIf(){}
	virtual const char* kind_str()const{return"if";}
};

// struct Foo(x,y,z){ x=..., y=..., z=...} 
// rolls a default constructor.
// Could we add that to C++ ??


struct Call {
	// linked through caller->callee & scope block
	CallScope* scope;
	Call* next_of_scope;

	Expr*	caller;
	Call* next_of_caller;

	ExprFnDef*	callee;
	Call* next_of_fn;
	Call(){scope=0;next_of_scope=0;caller=0;next_of_caller=0;callee=0;next_of_fn=0;}
};

struct CallScope {
	ExprFnDef*	outer;
	ExprBlock* node;
	CallScope* parent;
	CallScope* next;
	CallScope* child;
	Call* calls;
	// locals;
	// captures.
	const char* name()const {if (!parent){return"global";} else return getString(outer->name);}
	CallScope(){outer=0;node=0;parent=0;next=0;child=0;calls=0;}
	void visit_calls();
	ExprFnDef* find_fn(Name name) const;
};

void CallScope::visit_calls() {
	for (auto call=this->calls;call;call=call->next_of_scope) {
		printf("%s --> %s\n",this->name(), getString(call->callee->name));
	}
	for (auto sub=this->child; sub; sub=sub->next)
		sub->visit_calls();
}
void find_printf(const char*,...){};
ExprFnDef* find_sub(Expr* n,Name name) {
	if (auto sb=dynamic_cast<ExprBlock*>(n)) {
		find_printf("look for %s in %s",getString(name),n->get_name_str());
		for (auto x:sb->args) {
			if (auto p=find_sub(x,name))
				return p;
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(n)){
		if (f->name==name)
			return f;
	}
	return nullptr;
}
ExprFnDef*	CallScope::find_fn(Name name) const {
	if (auto p= find_sub(this->node,name)) return p;
	else if (this->parent) return parent->find_fn(name);
	else return nullptr;
}

void ExprBlock::resolve_calls(CallScope* scope) {
	if (this->args.size()<=0) return;
	auto p=dynamic_cast<ExprIdent*>(this->args[0]);
	if (p){// printf("can't resolve non ident call yet - dynamic call, fnptr?\n");
	// TODO: Use typeparams - first compute types of arguments.
		printf("resolve call..%s\n",getString(p->ident));
		ExprFnDef* call_target = scope->find_fn(p->ident);
		if (call_target) {
			auto *call = new Call();
			call->callee=call_target; 
			call->caller=this;
		// add to lists: caller knows all calls it makes (polymorphic instantiations)
			call->next_of_caller=this->calls; this->calls=call;
		// scope knows all calls it makes
			call->next_of_scope=scope->calls; scope->calls=call;
		// function knows all its callers.
			call->next_of_fn=call_target->callers; call_target->callers=call;
		}
	}


	// Todo: Process Local Vars.
	for (int i=1; i<this->args.size(); i++) {
		this->args[i]->resolve_calls(scope);
	}
}

void ExprFnDef::resolve_calls(CallScope* parent) {
	auto scope=new CallScope;
	scope->parent=parent; scope->next=parent->child; parent->child=scope;
	scope->outer=this;
	scope->node=this->body;
	this->body->resolve_calls(scope);
}


void call_graph(Node* root,CallScope* scope) {
}

// [1] read an AST
// [2] perform Whole Program Inference:
//   generic types and implementations.
//   do we need generics?
//   we can make intrinsics:
//   T[]  slice array
//   T[..] dynamic-array
//   T[4] fixed array
//   T[4..] small-array.
//
// Why do we want to do this
// we want (a) Whole Program Type Inference
//         (b) Ad Hoc Overloading
//         (c) overloads including src->dst
/*
Node* build_test_ast() {
	return new FCall(new Ident(IDENT),{
		new Ident(IDENT+1),new Ident(IDENT+2),
			new ExprAssign(new Ident(IDENT+3),new ExprMul(new Ident(IDENT+4),new Ident(IDENT+5)))
	});
}
*/
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

/*
struct TokenStream {
	vector<Token> tokens;
	int	 pos=0;
//	struct Save{ TokStream* ts;int pos;~Save(){ts.restore(p);}}
	int	save() const{ return pos;}
	void restore(int p) { pos=p;}
	void fwd(){pos++;};
	void back(){--pos;};
	int eat(){ if (pos<tokens.size()) return tokens[pos++]; else return NONE;}
	bool eat_if(Token t){ if (tokens[pos]==t) {++pos;return true;} else return false;}
	bool eat_unless(Token t){ if (tokens[pos]!=t) {++pos;return true;} else return false;}
	int eat_ident() { if (tokens[pos]>=IDENT){ return tokens[pos++];} else return NONE;}
	bool peek_if(Token t){return tokens[pos]==t;}
	int peek(){return tokens[pos];}
	bool peek_ident(){return tokens[pos]>=IDENT;}
};
*/
bool isSymbolStart(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_';}
bool isSymbol(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || (c>='0' && c<='9');}
bool isNumStart(char c){return (c>='0'&&c<='9');};
bool isNum(char c){return (c>='0'&&c<='9') ||c=='.';};
bool isWhitespace(char c){return  c==' '||c=='\n'||c=='\a'||c=='\t';};
bool isOperator(char c){return c=='+'||c=='-'||c=='*'||c=='/'||c=='.'||c=='='||c=='>'||c=='<'||c=='&'||c=='|'||c=='~'||c=='%'||c=='^'||c=='+';}

struct NumDenom{int num; int denom;};

struct TextInput {
	const char* buffer,*tok_start,*tok_end,*prev_start;
	int curr_tok;

	TextInput(const char* src){
		buffer=src;
		curr_tok=-1;
		tok_start=tok_end=buffer;
		advance_tok();
	}

	void advance_sub(bool (*sub)(char c)){while ( *tok_end && sub(*tok_end)) tok_end++;}

	void advance_tok() {
		auto pos=tok_start;
		while (isWhitespace(*tok_end)&&*tok_end) tok_end++;
		tok_start=tok_end;
		if (!*tok_end) { this->curr_tok=0; return;}
		auto c=*tok_end;
		if (isSymbolStart(c))	advance_sub(isSymbol);
		else if (isNumStart(c)) advance_sub(isNum);
		else tok_end++;
		this->curr_tok = getStringIndex(tok_start,tok_end);
	}
	int eat_tok() {
		prev_start=tok_start;
		char tok[512]; for (const char* c=tok_start; c!=tok_end;c++) {}
		int r=curr_tok;
		advance_tok();
		return r;
	}
	int eat_ident() {
		auto r=eat_tok();
		ASSERT(r>=IDENT);
		return r;
	}
	int eat_int() {
		auto nd=eat_number();
		return nd.num/nd.denom;
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
	bool is_next_literal() const{
		char c=*tok_start;
		if ((c>='0' && c<='9')||(c==':' && g_lisp_mode)||(c=='\"'))
			return true;
		return false;
	}

	int peek_tok(){return curr_tok;}
	void reverse(){ ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;}
};
typedef TextInput TokenStream;

Expr* parse_lisp(TokenStream& src);
ExprFnDef* parse_fn(TokenStream&src);

Expr* parse_expr(TokenStream&src,int close) {
	printf("\nparse_expr\n");
	ExprBlock *node=0;
	while (true) {
		Expr* sub=nullptr;
		if (src.is_next_literal()) {
			node->args.push_back(new ExprLiteral(src.eat_float()));
			continue;
		}
		auto tok=src.eat_tok();
		if (!tok || tok==close)
			break;
//		std::cout<<tok<<getString(tok)<<"\n";
		if (tok==FN) return parse_fn(src);		
		if (!node)node =new ExprBlock;
		if (tok==OPEN_PAREN) {
			sub=parse_expr(src,CLOSE_PAREN);
		} else  {
			sub=new ExprIdent(tok);
		}
		if (sub)
			node->args.push_back(sub);
	};
	return node;
}
Type* parse_type(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	TypeId tid=T_AUTO;
	Type* ret=0;
	if (tok!=close){
		switch (tok) {
		case VOID: tid=T_VOID; break;	
		case INT: tid=T_INT; break;
		case FLOAT: tid=T_FLOAT; break;
		case STR: tid=T_CONST_STRING; break;
		}
		ret=new Type(tid,tok);
	} else ret= new Type(T_AUTO,0);
	while (close!=src.eat_tok()){};
	return ret;
	
}
ArgDef* parse_arg(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	if (tok==close) return nullptr;
	if (tok==OPEN_PAREN){
		auto id=src.eat_ident();
		return new ArgDef(id,parse_type(src,CLOSE_PAREN));
	} 
		else return new ArgDef(tok,nullptr);	
}
ExprFnDef* parse_fn(TokenStream&src) {
	auto *fndef=new ExprFnDef();
	printf("parse_fn");
	// read function name or blank
	auto tok=src.eat_tok(); 
	if (tok!=OPEN_PAREN) {
		fndef->name=tok;
		tok=src.eat_tok();
		ASSERT(tok==OPEN_PAREN);
	} else fndef->name=NONE;
	printf(getString(fndef->name));
	// read function arguments
	while (NONE!=(tok=src.peek_tok())) {
		if (tok==CLOSE_PAREN) {src.eat_tok();break;}
		printf(" arg:%s ",getString(tok));
		fndef->args.push_back(parse_arg(src,CLOSE_PAREN));
	}
	printf("fn body:");
	// implicit "progn" here..
	fndef->body = new ExprBlock;
	fndef->body->args.push_back(new ExprIdent(DO));
	while (NONE!=(tok=src.eat_tok())) {
		printf("fn body..");
		if (tok==CLOSE_PAREN)
			break;
		if (tok==OPEN_PAREN) {
			fndef->body->args.push_back(parse_expr(src,CLOSE_PAREN));
		} else {
			printf("parse_raw_ident\n");
			fndef->body->args.push_back(new ExprIdent(tok));
		}
	}

	return fndef;
}

const char* g_TestProg=
"(fn lerp((a float) (b float) (f float))(+(*(- b a)f)a))"
"(fn bilerp(a b c d u v)(lerp(lerp a b )(lerp c d)v))"
"(fn invlerp(a b x)(/(- x a)(- b a)))"
"(fn madd(a b f)(+(* b f)a))"
"(fn min(a b)(if (< a b)a b))"
"(fn max(a b)(if (> a b)a b))"
"(fn clamp(a b f)(min b (max a f)))"
"(print (lerp 10 20 0.5))"
	;
int main(int argc, const char** argv) {

	TextInput	src(g_TestProg);
	auto node=parse_expr(src,0);
//	while (auto ix=src.eat_tok()){}
//	printf("%d%s",getStringIndex("foo"),getString(IDENT+5));
//	g_Names.dump();
//	printf(getString(ADD_ASSIGN));
	printf("%x\n",node);
	node->dump(0);
	CallScope global; global.node=(ExprBlock*)node;
	node->resolve_calls(&global);
	global.visit_calls();

/*	auto ts=Parser2{{{IDENT,ASSIGN,IDENT,MUL,IDENT,ADD,IDENT,ADD,IDENT,MUL,IDENT,ADD,IDENT,MUL,IDENT,MUL,IDENT},0}};
	auto prog=ts.parse();
	prog->dump();

	Foo f{{"felcher"},{1,2,3,4,5}};
	cout << f;
	std::cout<<"hello world\n";
	auto prog2 = build_test_ast();
 	prog2->dump();
*/
}



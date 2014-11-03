#pragma once
#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>
#include <string.h>

#ifdef DEBUG
#define ASSERT(x) if (!(auto p=(x))) {printf("error %s:%d:%d %s\n",__FILE__,__LINE__,p, #X );exit(0);}
#else
#define ASSERT(x)
#endif

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
	// top level structs & keywords. one,zero are coercible types..
	INT,FLOAT,STR,VOID,AUTO,ONE,ZERO,VOIDPTR,
	PRINT,FN,STRUCT,TUPLE,VARIANT,LET,SET,VAR,WHILE,IF,ELSE,DO,FOR,IN,RETURN,BREAK,
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
	INC,DEC,
	COMMA,SEMICOLON,
	// after these indices, comes indents
	IDENT
};

extern const char* g_token_str[];
extern int g_precedence[];

struct StringTable {
	int	nextId= 0;
	bool verbose;
	map<string,int>	names;
	vector<string> index_to_name; //one should be index into other lol
	StringTable(const char** initial);
	int get_index(const char* str, const char* end);
	void dump();
};
extern StringTable g_Names;
int getStringIndex(const char* str,const char* end=0);
const char* getString(int index);
void indent(int depth);

// todo: path malarchy.
typedef int Name;
struct CallScope;
struct StructDef;
struct ExprIdent;
struct Type;

class Node {
public:
	int visited;					// anti-recursion flag.
	virtual  ~Node(){visited=0;};	// node ID'd by vtable.
	virtual void dump(int depth=0) const{};
	virtual Type* resolve(CallScope* scope){printf("empty?");return nullptr;};
	virtual const char* kind_str(){return"node";}
	virtual int get_name() const{return 0;}
	const char* get_name_str()const{return getString(get_name());}
};
struct Type : Node{
	Name	type_id;	
	StructDef* struct_def;
	Type*	sub;	// a type is itself a tree
	Type*	next;
	void push_back(Type* t);
	virtual const char* kind_str()const;
	Type(Name i);
	Type() { type_id=0;sub=0;next=0; struct_def=0;}
	bool eq(const Type* other) const;
	void dump_sub()const;
	void dump(int depth)const;
	const char* get_name_str() const;
};
struct Expr : Node{
	Type* type;
	void dump(int depth) const;
	Expr();
	virtual const char* kind_str()const{return"expr";}
	virtual Name ident() const  { return VOID;}
};
struct ExprScopeBlock : public Expr{};
struct ExprFnDef;
struct ExprBlock :public ExprScopeBlock{
	vector<Expr*>	args;
	ExprFnDef*	call_target;
	ExprBlock* next_of_call_target;	// to walk callsites to a function
	int get_name()const;
	void dump(int depth) const;
	Type* resolve(CallScope* scope);
	virtual const char* kind_str()const{return"block";}
	ExprBlock();
};
enum TypeId{
	T_AUTO,T_KEYWORD,T_VOID,T_INT,T_FLOAT,T_CONST_STRING,T_CHAR,T_PTR,T_STRUCT,T_FN
};
struct StructDef;

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
	ModuleBase(){vars=0;functions=0;structs=0;modules=0;parent=0;};
	virtual ModuleBase* get_next_of_module()const{ASSERT(0);return nullptr;}
	virtual const char* kind_str()const{return"mod";}
	Name ident() const {return this->name;}
};
struct Module : ModuleBase {
	Module* next_of_module;
	Module* get_next_of_module()const{return next_of_module;}
};
struct ExprLiteral : Expr {
	TypeId	type_id;

	union  {int val_int; float val_float; const char* val_str;int val_keyword;} u;
	virtual const char* kind_str()const{return"lit";}
	void dump(int depth) const;
	ExprLiteral(float f);
	ExprLiteral(int i);
	ExprLiteral(const char* start,int length);
	~ExprLiteral();

	Type* resolve(CallScope* scope);
};
struct ArgDef :Node{
	Name name;
	Type* type;
	Expr* default_value;
	ArgDef(){type=0;default_value=0;};
	ArgDef(int n,Type* t):ArgDef(){name=n;type=t;printf("\nMAKE ARG %s", getString(n)); if (type)type->dump(-1); printf("\n");}
	void dump(int depth) const;
	virtual const char* kind_str()const;
	~ArgDef(){}
};
struct FnName {
	Name		name;
	FnName*		next;
	ExprFnDef*	fn_defs;
	ExprFnDef*	getByName(Name n);
//	ExprFnDef* resolve(Call* site);
	FnName(){ name=0; next=0;fn_defs=0;}
};
extern FnName*	g_fn_names;
extern vector<FnName> g_functions;
FnName* getFnName(int name);
/*
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
*/
struct Variable {
	Variable* next;
	Name name;
	Type*	type;
	Type* get_type() const { if(this)return this->type;else return nullptr;}
};
struct CallScope {
	ExprFnDef*	outer;
	Expr* node;
	CallScope* parent;
	CallScope* next;
	CallScope* child;
	CallScope* global;
	//Call* calls;
	Variable* vars;
	// locals;
	// captures.
	const char* name()const;
	CallScope(){outer=0;node=0;parent=0;next=0;child=0;vars=0;global=0;}
	void visit_calls();
	Variable* get_variable(Name ident);
	Variable* create_variable(Name name,Type* t);
	ExprFnDef* find_fn(Name name,Expr** args, int num_args) ;
	void dump(int depth) const;
	void push_child(CallScope* sub) { sub->next=this->child; this->child=sub;sub->parent=this; sub->global=this->global;}
};
Type* resolve_make_fn_call(ExprBlock* block,CallScope* scope);

struct StructDef : ModuleBase {
	vector<ArgDef> fields;
	virtual const char* kind_str()const{return"struct";}
	StructDef* next_of_module;
	ModuleBase* get_next_of_module(){return this->next_of_module;}
};



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

struct Call;
struct FnName;
struct ExprFnDef :public Module {
	ExprFnDef*	next_of_module;
	ExprFnDef*	next_of_name;	//link of all functions of same name...
	ExprFnDef*	instance_of;	// Original function, when this is a template instance
	ExprFnDef*	instances;		// Linklist of it's instanced functions.
	ExprFnDef*	next_instance;
	FnName*		fn_name;
	bool resolved;
	// Partial specialization may add one specific parameter...
	// calls from un-instanced routines can partially implement?

	vector<ArgDef*> args;
	ExprBlock* body;
	ExprBlock* callers;	// linklist of callers to here
	int get_name()const {return name;}
	FnName* get_fn_name();
	bool is_generic() const;
	virtual const char* kind_str()const{return"fn";}
	ExprFnDef(){resolved=false;next_of_module=0;next_of_name=0;instance_of=0;instances=0;next_instance=0;name=0;body=0;callers=0;type=0;}
	void dump(int ind) const;
	Type* resolve(CallScope* scope);
	Type* resolve_call(CallScope* scope);
};

struct ExprIdent :Expr{
	int name;
	void dump(int depth) const {
		indent(depth);printf("%s \n",getString(name));
	}
	Name ident()const override{return name;}
	virtual const char* kind_str()const{return"ident";}
	ExprIdent(int i){name=i;}
	Type* resolve(CallScope* scope);
};









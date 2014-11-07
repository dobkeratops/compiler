#pragma once
#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>
#include <string.h>

#ifdef DEBUG
#define ASSERT(x) if (!(x)) {printf("error %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );exit(0);}
#define TRACE printf("%s:%d: %s\n",__FILE__,__LINE__,__FUNCTION__);
#else
#define ASSERT(x)
#endif

// todo: seperate Parser.

#define ilist initializer_list

using std::string;
using std::vector;
using std::cout;
using std::map;
using std::initializer_list;
template<typename T,typename S>
T& operator<<(T& dst, const vector<S>&src) { for (auto &x:src){dst<<x;};return dst;};

template<typename T,typename U>
T verify_cast(U src){auto p=dynamic_cast<T>(src);ASSERT(p);return p;}
void newline(int depth);

template<class T,class Y> T* isa(const Y& src){ return dynamic_cast<T>(src);}

#define PRECEDENCE 0xff
#define PREFIX 0x100
#define UNARY 0x200
#define ASSOC 0x400
#define WRITE_LHS 0x1000
#define WRITE_RHS 0x2000
#define WRITE (WRITE_LHS|WRITE_RHS)
#define READ_LHS 0x4000
#define READ_RHS 0x8000
#define READ (READ_LHS|READ_RHS)
#define MODIFY (READ_LHS|WRITE_LHS|READ_RHS|WRITE_RHS)
#define RWFLAGS (WRITE_LHS|READ_LHS|WRITE_RHS|READ_RHS)
extern int operator_flags(int tok);

enum Token {
	NONE=0,
	// top level structs & keywords. one,zero are coercible types..
	INT,FLOAT,STR,VOID,AUTO,ONE,ZERO,VOIDPTR,PTR,REF,TUPLE,
	PRINT,FN,STRUCT,ENUM,ARRAY,UNION,VARIANT,WITH,MATCH,
	LET,SET,VAR,
	WHILE,IF,ELSE,DO,FOR,IN,RETURN,BREAK,
	// delimiters
	OPEN_PAREN,CLOSE_PAREN,
	OPEN_BRACE,CLOSE_BRACE,
	OPEN_BRACKET,CLOSE_BRACKET,
	// operators
	ARROW,FAT_ARROW,REV_ARROW,DOUBLE_COLON,SWAP,
	COLON,ADD,SUB,MUL,DIV,DOT,
	LT,GT,LE,GE,EQ,NE,LOG_AND,LOG_OR,
	AND,OR,XOR,MOD,SHL,SHR,
	ASSIGN,LET_ASSIGN,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,AND_ASSIGN,OR_ASSIGN,
	PRE_INC,PRE_DEC,POST_INC,POST_DEC,
	NEG,DEREF,ADDR,NOT,COMPLEMENET, MAYBE_PTR,OWN_PTR,MAYBE_REF,VECTOR,SLICE,
	COMMA,SEMICOLON,
	// after these indices, comes indents
	PLACEHOLDER,
	IDENT
};

extern const char* g_token_str[];
extern int g_tok_info[];

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
typedef int32_t Name;
typedef int32_t RegisterName;
struct CallScope;
struct StructDef;
struct ExprIdent;
struct Type;
enum Status :int {COMPLETE=0,INCOMPLETE=1,ERROR=3};// complete is zero, ERROR is 3 so we can start 0 and OR the bad news in.
struct ResolvedType{
	// carries information from type propogation
	Type* type;
	Status status;
	ResolvedType(Type*t=nullptr,Status s=INCOMPLETE){type=t; status=s;}
	operator Type* ()const {return type;}
};
class Node {
public:
	Name name;						// identifier index
	RegisterName regname;			// temporary for llvm SSA calc.
	int visited;					// anti-recursion flag.
	Node(){name=0;visited=0;regname=0;}
	virtual  ~Node(){visited=0;};	// node ID'd by vtable.
	virtual void dump(int depth=0) const{};
	virtual ResolvedType resolve(CallScope* scope, const Type* desired){printf("empty?");return ResolvedType(nullptr);};
	virtual const char* kind_str(){return"node";}
	virtual int get_name() const{return 0;}
	const char* get_name_str()const;
	Name ident() const  { if (this)return this->name;else return 0;}
	virtual Node* clone() const=0;
	Node* clone_if()const { if(this) return this->clone();else return nullptr;}
	void dump_if(int d)const{if (this) this->dump(d);}
	virtual void clear_reg(){regname=0;};
	RegisterName get_reg(Name baseName, int* new_index, bool force_new);
	RegisterName get_reg_new(Name baseName, int* new_index);
	RegisterName get_reg_existing();
};

struct TypeParam{int name; int defaultv;};

struct Type : Node{
	vector<TypeParam> typeparams;
	StructDef* struct_def;
	Type*	sub;	// a type is itself a tree
	Type*	next;
	void push_back(Type* t);
	virtual const char* kind_str()const;
	Type(Name i);
	Type() { name=0;sub=0;next=0; struct_def=0;}
	bool eq(const Type* other) const;
	void dump_sub()const;
	void dump(int depth)const;
	Node* clone() const;
	void clear_reg(){regname=0;};
};
struct LLVMType {
	int name;
	bool is_pointer;
};
struct Expr : Node{
	Type* type;
	void dump(int depth) const;
	void dump_top()const;
	Expr();
	virtual const char* kind_str()const{return"expr";}
	Type* get_type() const { if(this)return this->type;else return nullptr;}
	LLVMType get_type_llvm() const {
		if (!this) return LLVMType{0,0};
		if (!this->type) return LLVMType{0,0};
		auto tn=this->type->name;
		if (!this->type->sub) return LLVMType{tn,0};
		if (tn==PTR || tn==DEREF ||tn==ADDR) return LLVMType{this->type->sub->name,true};
		// todo structs, etc - llvm DOES know about these.
		return LLVMType{0,0};
	}
};
struct ExprScopeBlock : Expr{};
struct ExprFnDef;
struct ExprBlock :public ExprScopeBlock{
	// used for operators, function calls and compound statement
	// started out with lisp-like (op operands..) where a compound statement is just (do ....)
	// TODO we may split into ExprOperator, ExprFnCall, ExprBlock
	// the similarity between all is
	Expr*	call_op;
	vector<Expr*>	argls;
	ExprFnDef*	call_target;
	CallScope* scope;
	ExprBlock* next_of_call_target;	// to walk callsites to a function
	bool is_compound_expression()const {return (!name && !call_op);}
	Name get_fn_name()const;
	int get_operator()const{if (call_op){return call_op->name;}else{return 0;}}
	int get_name()const;
	void dump(int depth) const;
	ResolvedType resolve(CallScope* scope, const Type* desired);
	virtual const char* kind_str()const{return"block";}
	ExprBlock();
	Node* clone() const;
	void clear_reg(){ for (auto p:argls)p->clear_reg();if (call_op)call_op->clear_reg(); regname=0;};
};
enum TypeId{
//	T_AUTO,T_KEYWORD,T_VOID,T_INT,T_FLOAT,T_CONST_STRING,T_CHAR,T_PTR,T_STRUCT,T_FN
	T_NONE,
	T_INT,T_FLOAT,T_CONST_STRING,T_VOID,T_AUTO,T_ONE,T_ZERO,T_VOIDPTR,
	T_WRONG_PRINT,T_FN,T_STRUCT,T_TUPLE,T_VARIANT,T_NUM_TYPES,
};
bool is_type(int tok);
struct StructDef;
extern bool g_lisp_mode;
struct Module;
// module base: struct(holds fns,structs), function(local fns), raw module.
struct StructDef;
struct ExprFnDef;
struct TypeDef;
struct ExprIf;
struct VarDecl;
struct ModuleBase : Expr {
	ModuleBase* parent;
	Module*	modules;
	StructDef* structs;
	ExprFnDef*	functions;
	VarDecl* vars;
	ModuleBase(){vars=0;functions=0;structs=0;modules=0;parent=0;};
	virtual ModuleBase* get_next_of_module()const{ASSERT(0);return nullptr;}
	virtual const char* kind_str()const{return"mod";}
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
	ExprLiteral(const char* start);// take ownership
	~ExprLiteral();
	Node* clone() const;
	bool is_string() const { return type_id==T_CONST_STRING;}
	ResolvedType resolve(CallScope* scope, const Type* desired);
	
};
struct ArgDef :Node{
	uint32_t size,offset;
	Name name;
	Type* type;
	Expr* default_expr;
	ArgDef(){type=0;default_expr=0;};
	ArgDef(int n):ArgDef(){name=n;type=0;default_expr=0;printf("\nMAKE ARG %s", getString(n)); if (type)type->dump(-1); printf("\n");}
	void dump(int depth) const;
	virtual const char* kind_str()const;
	~ArgDef(){}
	Node* clone() const;
	void render_object();
};
struct ExprStructDef;

struct FnName {		// everything defined under a name.
	Name		name;
	FnName*		next;
	Type*		types;
	ExprFnDef*	fn_defs;
	ExprStructDef*	structs; // also typedefs?
	ExprFnDef*	getByName(Name n);
//	ExprFnDef* resolve(Call* site);
	FnName(){ name=0; next=0;fn_defs=0;structs=0;types=0;}
};

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
struct Variable : Expr{
	Variable* next;
	Expr* initialize; // if its an argdef, we instantiate an initializer list
	Variable(Name n){name=n; initialize=0;}
	Node* clone() const {
		auto v=new Variable(name);
		v->initialize = verify_cast<Expr*>(this->initialize->clone_if());
		v->next=0; v->type=this->type; return v;
	}
};
// scopes are created when resolving; generic functions are evaluated
struct CallScope {
	ExprFnDef*	outer;
	Expr* node;
	CallScope* parent;
	CallScope* next;
	CallScope* child;
	CallScope* global;
	//Call* calls;
	Variable* vars;
	FnName*	fn_names;
	// locals;
	// captures.
	const char* name()const;
	CallScope(){fn_names=0; outer=0;node=0;parent=0;next=0;child=0;vars=0;global=0;}
	void visit_calls();
	Variable* find_variable(Name ident);
	Variable* get_variable(Name name);
	ExprFnDef* find_fn(Name name,vector<Expr*>& args, const Type* ret_type) ;
	FnName* find_fn_name(Name name);
	void add_fn_def(ExprFnDef*);
	void dump(int depth) const;
	void push_child(CallScope* sub) { sub->next=this->child; this->child=sub;sub->parent=this; sub->global=this->global;}
};
Type* resolve_make_fn_call(ExprBlock* block,CallScope* scope,const Type* desired);

struct StructDef : ModuleBase {
	vector<ArgDef> fields;
	virtual const char* kind_str()const{return"struct";}
	StructDef* next_of_module;
	ModuleBase* get_next_of_module(){return this->next_of_module;}
};
struct ExprIf :  Expr {
	Expr* cond=0;
	Expr* body=0;
	Expr* else_block=0;
	void dump(int depth) const;
	ExprIf(){name=0;cond=0;body=0;else_block=0;}
	~ExprIf(){}
	Node* clone() const;
	virtual const char* kind_str()const{return"if";}
	ResolvedType resolve(CallScope* scope,Type*) ;
};
struct ExprFor :  Expr {
	Expr* pattern=0;
	Expr* init=0;
	Expr* cond=0;
	Expr* incr=0;
	Expr* body=0;
	Expr* else_block=0;
	void dump(int depth) const;
	bool is_c_for()const{return !pattern;}
	bool is_for_in()const{return pattern && cond==0 && incr==0;}
	ExprFor(){name=0;pattern=0;init=0;cond=0;incr=0;body=0;else_block=0;}
	~ExprFor(){}
	virtual const char* kind_str()const{return"if";}
	ResolvedType resolve(CallScope* scope,Type*) {return ResolvedType(nullptr);};
	Expr* find_break_expr();
	Node* clone()const;
};

struct Call;
struct FnName;

struct ExprStructDef: Module {
	// lots of similarity to a function actually.
	// but its' backwards.
	// it'll want TypeParams aswell.
	uint32_t size;
	vector<TypeParam> typeparams;
	vector<ArgDef*> fields;
	ExprFnDef* constructor_fn;
	FnName* fn_name;
	ExprStructDef* next_of_name;
	ExprStructDef(){constructor_fn=0;fn_name=0;next_of_name=0;}
	void dump(int depth)const;
	ResolvedType resolve(CallScope* scope, const Type* desired);
	Node* clone()const {printf("warning,leak\n");return (Node*) this;};
};

struct ExprFnDef : Module {
	ExprFnDef*	next_of_module;
	ExprFnDef*	next_of_name;	//link of all functions of same name...
	ExprFnDef*	instance_of;	// Original function, when this is a template instance
	ExprFnDef*	instances;		// Linklist of it's instanced functions.
	ExprFnDef*	next_instance;
	FnName*		fn_name;
	CallScope*	scope;
	Type* fn_type;				// eg (args)->return
	bool resolved;
	// Partial specialization may add one specific parameter...
	// calls from un-instanced routines can partially implement?

	vector<TypeParam> typeparams;
	vector<ArgDef*> args;
	ExprBlock* body;
	ExprBlock* callers;	// linklist of callers to here
	int get_name()const {return name;}
	FnName* get_fn_name(CallScope* scope);
	bool is_generic() const;
	virtual const char* kind_str()const{return"fn";}
	ExprFnDef(){scope=0;resolved=false;next_of_module=0;next_of_name=0;instance_of=0;instances=0;next_instance=0;name=0;body=0;callers=0;type=0;fn_type=0;}
	void dump(int ind) const;
	ResolvedType resolve(CallScope* scope,const Type* desired);
	ResolvedType resolve_call(CallScope* scope,const Type* desired);
	Expr* get_return_value() const;
	Node* clone() const;
};
#define link(obj,owner,link) obj->link=owner; owner=obj;


// What details did we miss ?
// Codegen is a big issue.

struct ExprIdent :Expr{
	void dump(int depth) const;
	virtual const char* kind_str()const{return"ident";}
	ExprIdent(int i){name=i;type=0;}
	ResolvedType resolve(CallScope* scope, const Type* desired);
	Node* clone() const;
	bool is_placeholder()const{return name==PLACEHOLDER;}
};









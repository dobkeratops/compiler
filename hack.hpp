#pragma once
#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string.h>

#ifdef DEBUG
#define ASSERT(x) if (!(x)) {printf("error %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );*(volatile long*)0=0;exit(0);}
#define WARN(x) if (!(x)) {printf("warning %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );}
#define TRACE printf("%s:%d: %s\n",__FILE__,__LINE__,__FUNCTION__);
#else
#define ASSERT(x)
#define WARN(x)
#endif

struct Node;
struct Name;
extern void dbprintf(const char*,...);
extern void error(Node*,const char*,...);
extern bool is_comparison(Name n);
// todo: seperate Parser.

#define ilist initializer_list

using std::string;
using std::vector;
using std::set;
using std::cout;
using std::map;
using std::pair;
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
bool isSymbolStart(char c);
enum Token {
	NONE=0,
	// top level structs & keywords. one,zero are coercible types..
	INT,UINT,BOOL,FLOAT,CHAR,STR,VOID,AUTO,ONE,ZERO,VOIDPTR,PTR,REF,TUPLE,NUMBER,TYPE,NAME,
	PRINT,FN,STRUCT,ENUM,ARRAY,VECTOR,UNION,VARIANT,WITH,MATCH, SIZEOF, TYPEOF, NAMEOF,OFFSETOF,
	LET,SET,VAR,
	WHILE,IF,ELSE,DO,FOR,IN,RETURN,BREAK,
	// delimiters
	OPEN_PAREN,CLOSE_PAREN,
	OPEN_BRACE,CLOSE_BRACE,
	OPEN_BRACKET,CLOSE_BRACKET,
	// operators
	ARROW,DOT,FAT_ARROW,REV_ARROW,DOUBLE_COLON,SWAP,
	COLON,
	ADD,SUB,MUL,DIV,
	AND,OR,XOR,MOD,SHL,SHR,
	LT,GT,LE,GE,EQ,NE,
	LOG_AND,LOG_OR,
	ASSIGN,LET_ASSIGN,ASSIGN_COLON,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,AND_ASSIGN,OR_ASSIGN,XOR_ASSIGN,MOD_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,
	DOT_ASSIGN,
	PRE_INC,PRE_DEC,POST_INC,POST_DEC,
	NEG,DEREF,ADDR,NOT,COMPLEMENET, MAYBE_PTR,OWN_PTR,MAYBE_REF,VECTOR_OF,SLICE,
	COMMA,SEMICOLON,
	// after these indices, comes indents
	ELIPSIS,RANGE,
	PLACEHOLDER,
	IDENT
};
struct Name;
Name getStringIndex(const char* str,const char* end=0);
const char* str(int);

#ifdef DEBUG_NAMES
struct Name {
	char* str;
	int32_t index;
	Name(){index=0;str=0;}
	Name(const char* a, const char* end=0){
		if (!end) end=end+strlen(a);
		size_t len=end-a;
		str=(char*) malloc(len+1); memcpy(str,a,len); str[len]=0;
		index=getStringIndex(a,end);
	}
	Name(const Name& b){index=b.index; str=b.str;}
//	operator int32_t(){return index;}
	operator int()const {return index;}
	bool operator==(const Name& b)const{return index==b.index;}
	bool operator==(int b)const{return index==b;}
};
#else
struct Name {
	int32_t index;
	Name(){index=0;}
	Name(const char* a, const char* end=0){
		if (!end) end=end+strlen(a);
		index=getStringIndex(a,end);
	}
	Name(int i){index=i;}
	Name(const Name& b){index=b.index; }
	//	operator int32_t(){return index;}
	operator int()const {return index;}
	bool operator==(const Name& b)const{return index==b.index;}
	bool operator==(int b)const{return index==b;}
	bool operator<(int b)const{return index<b;}
	bool operator>=(int b)const{return index>=b;}
};
#endif

typedef int32_t RegisterName;

bool is_operator(Name name);
struct LLVMOp {
	int return_type;
	const char* op_signed;
	const char* op_unsigned;
};

const LLVMOp* get_op_llvm(int opname,int tyname); // for tokens with 1:1 llvm mapping
const char* get_llvm_type_str(int tname);
extern const char* g_token_str[];
extern int g_tok_info[];

struct StringTable {
	enum Flags :char {String,Number};
	int	nextId= 0;
	bool verbose;
	map<string,int>	names;
	vector<string> index_to_name; //one should be index into other lol
	vector<char>	flags;
	StringTable(const char** initial);
	int get_index(const char* str, const char* end,char flags);
	void dump();
};
extern StringTable g_Names;
Name getStringIndex(const char* str,const char* end);
Name getNumberIndex(int num);	// ints in the type system stored like so
int getNumberInt(Name n);
float getNumberFloat(Name n);
const char* getString(Name index);
void indent(int depth);
inline const char* str(Name n){return getString(n);}
inline const char* str(int i){return g_Names.index_to_name[i].c_str();}

// todo: path malarchy.
struct Scope;
struct ExprStructDef;
struct ExprIdent;
struct Type;
struct Variable;
struct ResolvedType{
	// TODO: This is a misfeature;
	// return value from Resolve should just be status
	// we require the result information to stay on the type itself.
	// we keep getting bugs from not doing that.
	enum Status:int {COMPLETE=0,INCOMPLETE=1,ERROR=3};
	// complete is zero, ERROR is 3 so we can
	// carries information from type propogation
	Type* type;
	Status status;
	void combine(const ResolvedType& other){
		status=(Status)((int)status|(int)other.status);
	}
	ResolvedType(){type=0;status=INCOMPLETE;}
	ResolvedType(const Type*t,Status s){type=const_cast<Type*>(t); status=s;}
	ResolvedType(const Type* t, int s):ResolvedType(t,(Status)s){}
	//operator Type* ()const {return type;}
	//operator bool()const { return status==COMPLETE && type!=0;}
};
class Node {
public:
	Name name;						// identifier index
	RegisterName regname;			// temporary for llvm SSA calc.
	bool reg_is_addr=false;
	int visited;					// anti-recursion flag.
	Node(){visited=0;regname=0;}
	virtual  ~Node(){visited=0;};	// node ID'd by vtable.
	virtual void dump(int depth=0) const{};
	virtual ResolvedType resolve(Scope* scope, const Type* desired){dbprintf("empty?");return ResolvedType(nullptr, ResolvedType::INCOMPLETE);};
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
	virtual bool is_undefined()const{if (this && name==PLACEHOLDER) return true; return false;}
	
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const{return ;}
};

struct TypeParam{
	Name name;
	Type* defaultv=0;
	TypeParam(){};
	TypeParam(Name n, Type* dv):name(n),defaultv(dv){};
};

struct LLVMType {
	int name;
	bool is_pointer;
};
extern void verify(const Type* t);
struct Type;
struct ExprBlock;
struct ExprOp;
struct Expr : Node{
private:Type* m_type;
public:
	void dump(int depth) const;
	void dump_top()const;
	Expr();
	virtual const char* kind_str()const{return"expr";}
	Type* expect_type() const;
	Type* get_type() const { if(this) {verify(this->m_type);return this->m_type;}else return nullptr;}
	Type*& type(){ verify(this->m_type);return this->m_type;}
	const Type* type()const{ verify(this->m_type);return this->m_type;}
	void type(const Type* t){ verify(t);this->m_type=(Type*)t;}
	void set_type(const Type* t){verify(t);this->m_type=(Type*)t;};
	Type*& type_ref(){return this->m_type;}
	LLVMType get_type_llvm() const;
	virtual Type* eval_as_type()const{return nullptr;};
	virtual ExprBlock* is_subscript(){return (ExprBlock*)nullptr;}
};
struct Type : Expr{
	int marker;
	vector<TypeParam> typeparams;
	ExprStructDef* struct_def=0;	// todo: struct_def & sub are mutually exclusive.
	Type*	sub=0;	// a type is itself a tree
	Type*	next=0;
	void push_back(Type* t);
	virtual const char* kind_str()const;
	Type(Name a,Name b): Type(a){push_back(new Type(b));}
	Type(Name a,Name b,Name c): Type(a){
		auto tc=new Type(c); auto tb=new Type(b); tb->push_back(tc); push_back(tb);
	}
	Type(ExprStructDef* sd);
	Type(Name i);
	Type() { marker=1234;name=0;sub=0;next=0; struct_def=0;}
	int alignment() const{return  4;};
	int size() const;
	bool is_register()const{return !is_complex();}
	bool is_complex()const;
	bool is_struct()const;
	bool is_array()const{return name==ARRAY;}
	bool is_template()const { return sub!=0;}
	bool is_function() const { return name==FN;}
	Type* fn_return() const { return sub->next;}
	Type* fn_args() const { return sub->sub;}
	int num_pointers()const;
	bool is_pointer()const {return (this && this->name==PTR) || (this->name==REF);}
	bool is_void()const {return !this || this->name==VOID;}
	ExprStructDef* get_struct() const; // strip away all pointers.
	int num_derefs()const {if (!this) return 0;int num=0; auto p=this; while (p->is_pointer()){num++;p=p->sub;} return num;}
	Type* deref_all() const{if (!this) return nullptr;int num=0; auto p=this; while (p->is_pointer()){p=p->sub;}; return (Type*)p;}
	bool eq(const Type* other) const;
	void dump_sub()const;
	void dump(int depth)const;
	Node* clone() const;
	void clear_reg(){regname=0;};
};

struct ExprScopeBlock : Expr{};
struct ExprFnDef;
struct Variable;
struct ExprOp: public Expr{
	Expr	*lhs=0,*rhs=0;
	int get_operator() const { return this->name;}
	int get_op_name() const { return this->name;}
	Node* clone() const;
	void clear_reg(){ lhs->clear_reg(); rhs->clear_reg();}
	ExprOp(Name opname) { name=opname; lhs=0; rhs=0;}
	ExprOp(Name opname, Expr* l, Expr* r){
		lhs=l; rhs=r;
		name=opname;
	}
	virtual const char* kind_str()const{return"operator";}
	void dump(int depth) const;
	virtual void find_vars_written(Scope* s, set<Variable*>& vars) const;
	bool is_undefined()const{return (lhs?lhs->is_undefined():false)||(rhs?rhs->is_undefined():false);}
	ResolvedType resolve(Scope* scope, const Type* desired);
};

struct ExprBlock :public ExprScopeBlock{
	// used for operators, function calls and compound statement
	// started out with lisp-like (op operands..) where a compound statement is just (do ....)
	// TODO we may split into ExprOperator, ExprFnCall, ExprBlock
	// the similarity between all is
	bool square_bracket;  // aka foo[a]  in C desugars as *(foo+a)
	bool is_compound_expression()const{return !call_expr &&!call_target && !name;}

	Expr*	call_expr=0;  //call_expr(argls...)  or {argsls...}
	vector<Expr*>	argls;
	ExprFnDef*	call_target=0;
	Scope* scope=0;
	ExprBlock* next_of_call_target=0;	// to walk callers to a function
	virtual ExprBlock* is_subscript(){if (this->square_bracket) return (ExprBlock*) this; return (ExprBlock*)nullptr;}
	ExprFnDef* get_fn_call()const {return this->call_target;}
	Name get_fn_name() const;
	void dump(int depth) const;
	ResolvedType resolve(Scope* scope, const Type* desired);
	virtual const char* kind_str()const{return"block";}
	ExprBlock();
	Node* clone() const;
	void clear_reg(){ for (auto p:argls)p->clear_reg();if (call_expr)call_expr->clear_reg(); regname=0;};
	bool is_undefined()const ;
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const;
};
enum TypeId{
//	T_AUTO,T_KEYWORD,T_VOID,T_INT,T_FLOAT,T_CONST_STRING,T_CHAR,T_PTR,T_STRUCT,T_FN
	T_NONE,
	T_INT,T_UINT,T_FLOAT,T_CONST_STRING,T_VOID,T_AUTO,T_ONE,T_ZERO,T_VOIDPTR,
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


struct ExprLiteral : Expr {
	TypeId	type_id;
	ExprLiteral* next_of_scope=0;	// collected..
	Scope* owner_scope=0;
	int llvm_strlen;

	union  {int val_int; int val_uint; float val_float; const char* val_str;int val_keyword;} u;
	virtual const char* kind_str()const{return"lit";}
	void dump(int depth) const;
	ExprLiteral(float f);
	ExprLiteral(int i);
	ExprLiteral(const char* start,int length);
	ExprLiteral(const char* start);// take ownership
	~ExprLiteral();
	Node* clone() const;
	bool is_string() const { return type_id==T_CONST_STRING;}
	ResolvedType resolve(Scope* scope, const Type* desired);
	bool is_undefined()const{return false;}
	const char* as_str()const{ return type_id==T_CONST_STRING?u.val_str:"";}
	size_t strlen() const;
};

struct ArgDef :Node{
	uint32_t size,offset;
	Type* type=0;
	Expr* default_expr=0;
	Type* get_type()const {return type;}
	void set_type(Type* t){verify(t);type=t;}
	Type*& type_ref(){return type;}
	ArgDef(){type=0;default_expr=0;};
	ArgDef(Name n){name=n;type=0;default_expr=0;}
	void dump(int depth) const;
	virtual const char* kind_str()const;
	~ArgDef(){}
	Node* clone() const;
	void render_object();
	int alignment() const{ return 4;}//todo, eval templates/other structs, consider pointers, ..
	
};

struct ExprStructDef;

struct NamedItems {		// everything defined under a name
	Scope* owner=0;
	Name		name;
	NamedItems*		next=0;
	Type*		types=0;
	ExprFnDef*	fn_defs=0;
	ExprStructDef*	structs=0; // also typedefs?

	ExprFnDef*	getByName(Name n);
//	ExprFnDef* resolve(Call* site);
	NamedItems(Name n,Scope* s){  name=n; owner=s;next=0;fn_defs=0;structs=0;types=0;}
};

/*
struct Call {
	// linked through caller->callee & scope block
	Scope* scope;
	Call* next_of_scope;

	Expr*	caller;
	Call* next_of_caller;             

	ExprFnDef*	callee;
	Call* next_of_fn;
	Call(){scope=0;next_of_scope=0;caller=0;next_of_caller=0;callee=0;next_of_fn=0;}
};
*/
enum VarKind{VkArg,Local,Global};
struct Variable : Expr{
	VarKind kind;
	Scope* owner=0;
	Variable* next=0;
	Expr* initialize=0; // if its an argdef, we instantiate an initializer list
	Variable(Name n,VarKind k){name=n; initialize=0; owner=0;kind=k;this->set_type(0);}
	Node* clone() const {
		auto v=new Variable(name,this->kind);
		std::cout<<"clone "<<str(name)<<this<<" as "<<v<<"\n";
		v->initialize = verify_cast<Expr*>(this->initialize->clone_if());
		v->next=0; v->set_type(this->get_type()); return v;
	}
	void dump(int depth) const;
};
// scopes are created when resolving; generic functions are evaluated
struct Scope {
	Expr*	owner=0;	// TODO: eliminate this, owner might be FnDef or Struct.
	Expr* node=0;
	Scope* parent=0;
	Scope* next=0;
	Scope* child=0;
	Scope* global=0;
	ExprLiteral* literals=0;
	//Call* calls;
	Variable* vars=0;
	NamedItems*	named_items=0;
	// locals;
	// captures.
	const char* name()const;
private:
	Scope(){named_items=0; owner=0;node=0;parent=0;next=0;child=0;vars=0;global=0;literals=0;}
public:
	Scope(Scope* p){ASSERT(p==0);named_items=0; owner=0;node=0;parent=0;next=0;child=0;vars=0;global=0;literals=0;}
	void visit_calls();
	Variable* find_fn_variable(Name ident,ExprFnDef* f);
	Variable* get_fn_variable(Name name,ExprFnDef* f);
	Variable* find_variable_rec(Name ident);
	Variable* find_scope_variable(Name ident);
	Variable* create_variable(Name name,VarKind k);
	Variable* get_or_create_scope_variable(Name name,VarKind k);
	ExprStructDef* find_struct(Type* t){return this->find_struct_sub(this,t);}//original scope because typarams might use it.
	ExprStructDef* find_struct_sub(Scope* original, Type* t);
	ExprStructDef* find_struct_named(Name name);
	ExprFnDef* find_fn(Name name,vector<Expr*>& args, const Type* ret_type) ;
	void add_struct(ExprStructDef*);
	void add_fn(ExprFnDef*);
	NamedItems* find_named_items_local(Name name);
	NamedItems* get_named_items_local(Name name);
	NamedItems* find_named_items_rec(Name name);
	void add_fn_def(ExprFnDef*);
	void dump(int depth) const;
private:
	void push_child(Scope* sub) { sub->owner=this->owner; sub->next=this->child; this->child=sub;sub->parent=this; sub->global=this->global;}
public:
	Scope* parent_or_global()const{
		if (parent) return this->parent; else if (global && global!=this) return this->global; else return nullptr;
	}
	Scope* make_inner_scope(Scope** pp_scope,Expr* owner=0){
		if (!*pp_scope){
			auto sc=new Scope;
			push_child(sc);
			sc->owner=owner;
			*pp_scope=sc;
		}
		return *pp_scope;
	};

};
ResolvedType resolve_make_fn_call(ExprBlock* block,Scope* scope,const Type* desired);
struct ExprIf :  Expr {
	Expr* cond=0;
	Expr* body=0;
	Expr* else_block=0;
	void dump(int depth) const;
	ExprIf(){name=0;cond=0;body=0;else_block=0;}
	~ExprIf(){}
	Node* clone() const;
	virtual const char* kind_str()const{return"if";}
	ResolvedType resolve(Scope* scope,const Type*) ;
	bool is_undefined()const{return cond->is_undefined()||body->is_undefined()||else_block->is_undefined();}
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const;
};
struct ExprFor :  Expr {
	Expr* pattern=0;
	Expr* init=0;
	Expr* cond=0;
	Expr* incr=0;
	Expr* body=0;
	Expr* else_block=0;
	Scope* scope=0;
	void dump(int depth) const;
	bool is_c_for()const{return !pattern;}
	bool is_for_in()const{return pattern && cond==0 && incr==0;}
	ExprFor(){name=0;pattern=0;init=0;cond=0;incr=0;body=0;else_block=0;scope=0;}
	~ExprFor(){}
	virtual const char* kind_str()const{return"if";}
	ResolvedType resolve(Scope* scope,const Type*);
	Expr* find_break_expr();
	Node* clone()const;
	bool is_undefined()const{return (pattern&&pattern->is_undefined())||(init &&init->is_undefined())||(cond&&cond->is_undefined())||(incr&&incr->is_undefined())||(body&& body->is_undefined())||(else_block&&else_block->is_undefined());}
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const;
};

struct Call;
struct FnName;

struct ExprStructDef: Expr {
	// lots of similarity to a function actually.
	// but its' backwards.
	// it'll want TypeParams aswell.
	uint32_t size;
	vector<TypeParam> typeparams;
	vector<ArgDef*> fields;
	vector<Type*> instanced_types;
	vector<ExprStructDef*> structs;
	vector<ExprFnDef*> functions;
	Type*	inherits_type=0;
	Scope* scope=0;
	ExprStructDef* inherits=0,*derived=0,*next_of_inherits=0; // walk the derived types of this.

	bool is_generic() const;
	ExprStructDef* instances=0, *instance_of=0,*next_instance=0;
	ExprFnDef* constructor_fn=0;
	NamedItems* name_ptr=0;
	ArgDef* find_field(Name name){ for (auto a:fields){if (a->name==name) return a;} return nullptr;}
	int field_index(Name name){for (auto i=0; i<fields.size(); i++){if(fields[i]->name==name)return i;} return -1;}
	ExprStructDef* next_of_name;
	ExprStructDef(){name_ptr=0;inherits=0;inherits_type=0;next_of_inherits=0; derived=0; constructor_fn=0;name_ptr=0;next_of_name=0; instances=0;instance_of=0;next_instance=0;}
	ExprStructDef* get_instance(Scope* sc, Type* type); // 'type' includes all the typeparams.
	void dump(int depth)const;
	ResolvedType resolve(Scope* scope, const Type* desired);
	Node* clone()const {dbprintf("warning,leak\n");return (Node*) this;};
	int alignment() const {int max_a=0; for (auto a:fields) max_a=std::max(max_a,a->alignment()); return max_a;}
	void inherit_from(Scope* sc, Type* base);
};
// todo.. generic instantiation: typeparam logic, and adhoc mo
struct ExprFnDef : Expr {
	ExprFnDef*	next_of_module=0;
	ExprFnDef*	next_of_name=0;	//link of all functions of same name...
	ExprFnDef*	instance_of=0;	// Original function, when this is a template instance
	ExprFnDef*	instances=0;		// Linklist of it's instanced functions.
	ExprFnDef*	next_instance=0;
	ExprBlock* callers=0;	// linklist of callers to here
	NamedItems*		name_ptr=0;
	Scope*	scope=0;
	
	Type* ret_type=0;
	Type* fn_type=0;				// eg (args)->return
	bool resolved;
	// Partial specialization may add one specific parameter...
	// calls from un-instanced routines can partially implement?

	vector<TypeParam> typeparams;
	vector<ArgDef*> args;
	bool variadic;
	ExprBlock* body=0;
	int get_name()const {return name;}
	bool is_generic() const;
	virtual const char* kind_str()const{return"fn";}
	ExprFnDef(){variadic=false;scope=0;resolved=false;next_of_module=0;next_of_name=0;instance_of=0;instances=0;next_instance=0;name=0;body=0;callers=0;fn_type=0;ret_type=0;name_ptr=0;}
	void dump(int ind) const;
	ResolvedType resolve(Scope* scope,const Type* desired);
	ResolvedType resolve_call(Scope* scope,const Type* desired);
	Expr* get_return_value() const;
	Type* return_type()const {
		auto x=get_return_value();
		return x?x->get_type():this->ret_type;
	}
	bool has_return_value() const{
		if (auto r=return_type()){
			return r->name!=VOID;}
		else return false;
	}
	Node* clone() const;
	bool is_undefined()const{return body==nullptr || body->is_undefined();};
	bool is_extern()const {return body==nullptr;}
};

// What details did we miss ?
// Codegen is a big issue.

struct ExprIdent :Expr{
	// TODO: definition pointer. (ptr to field,function,struct,typedef..)
	void dump(int depth) const;
	virtual const char* kind_str()const{return"ident";}
	ExprIdent(){};
	ExprIdent(const char* s,const char* e){name=Name(s,e);set_type(nullptr);}
	ExprIdent(Name n){name=n;set_type(nullptr);}
	ResolvedType resolve(Scope* scope, const Type* desired);
	Node* clone() const;
	bool is_placeholder()const{return name==PLACEHOLDER;}
	bool is_undefined()const{return is_placeholder();}
};









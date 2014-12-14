#pragma once
extern "C" char* gets(char*);
#include <cstdarg>
#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string.h>
#include <functional>
#include <cstdio>

#ifdef DEBUG
#define CRASH {*(volatile long*)0=-1;exit(-1);}
#define ASSERT(x) if (!(x)) {printf("error %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );CRASH}
#define WARN(x) if (!(x)) {printf("warning %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );}
#define TRACE printf("%s:%d: %s\n",__FILE__,__LINE__,__FUNCTION__);
#else
#define ASSERT(x)
#define WARN(x)
#define TRACE
#define CRASH
#endif

// Debug Prints
// on level 3 compiling is ultra verbose, level 4 adds line numbers

#define dbg_loc() dbprintf("%s:%d:",__FILE__,__LINE__);
#if DEBUG>=4
	#define DBLOC() dbg_loc()
#else
	#define DBLOC()
#endif
	#if DEBUG>=2
	#define dbg_varscope(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_lambdas(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_instancing(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_resolve(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_fnmatch(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_type(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_vtable(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_generic(...) {DBLOC();dbprintf(__VA_ARGS__);}
	#define dbg_strings(...) {DBLOC();dbprintf(__VA_ARGS__);}
#else
	inline void dbg_generic(const char*,...){}
	inline void dbg_fnmatch(const char*,...){}
	inline void dbg_varscope(const char*,...){}
	inline void dbg_lambdas(const char*,...){}
	inline void dbg_instancing(const char*,...){}
	inline void dbg_resolve(const char*,...){}
	inline void dbg_type(const char*,...){}
	inline void dbg_vtable(const char*,...){}
	inline void dbg_strings(const char*,...){}
#endif



typedef int32_t OneBasedIndex;

struct SrcPos {
	OneBasedIndex	line;
	int16_t col;
	void set(OneBasedIndex l,int c){line=l   ;col=c;}
	void printf(FILE* ofp){ fprintf(ofp,"%d,%d",line,col); }
	
	void print(FILE* ofp, const char* filename) {
		fprintf(ofp,"%s:%d:%d",filename,line,col);
	}
};

struct Span {
	SrcPos start,end;
};

#define R_FINAL 0x0001
#define R_REVERSE 0x0002
#define R_PUT_ON_STACK 0x8000

extern void verify_all_sub();
#define verify_all()
struct Node;
struct Name;
struct TypeParam;
struct TParamDef;
struct Expr;
struct ExprType;
struct ExprOp;
struct  ExprFnDef;
struct ExprFor;
struct ExprStructDef;
struct ExprIf;
struct ExprBlock;
struct ExprLiteral;
struct ExprIdent;
struct Match;
struct MatchArm;
struct ArgDef;
struct ExprDef;
struct Scope;
struct Type;
struct Variable;
struct ResolvedType;
struct Module;
struct TypeParamXlat;
struct VarDecl;
class CodeGen;
class CgValue;

extern  int g_debug_get_instance;


extern void dbprintf(const char*,...);
extern void error(const Node*,const Node*,const char*,...);
void error(const Node* n,const Scope* s, const char* str, ... );
extern void error(const Node*,const char*,...);
extern void error(const char*,...);
extern void error_begin(const Node* n, const char* str, ... );
extern void error_end(const Node* n);
extern void error(const SrcPos& pos, const char* str ,...);
extern bool is_comparison(Name n);
extern Name getStringIndex(const char* str,const char* end) ;
extern Name getStringIndex(Name base, const char* str) ;
extern const char* getString(const Name&);
extern bool is_operator(Name tok);
extern bool is_ident(Name tok);
extern bool is_type(Name tok);
extern void verify(const Type* t);
bool is_condition(Name tok);
bool is_comparison(Name tok);
bool is_callable(Name tok);
int operator_flags(Name tok);
int precedence(Name ntok);
int is_prefix(Name ntok);
int arity(Name ntok);
int is_right_assoc(Name ntok);
int is_left_assoc(Name ntok);
bool is_number(Name n);
Name get_infix_operator(Name tok);
Name get_prefix_operator(Name tok);

void verify_expr_op(const Node* p);
void verify_expr_block(const Node* p);
void verify_expr_fn_def(const Node* p);
void verify_expr_ident(const Node* p);
void verify_expr_block(const Node* p);
void verify_type(const Node* p);
void verify_all_sub();
void dump_typeparams(const std::vector<TParamDef*>& ts) ;
bool type_is_coercible(const Type* from,const Type* to,bool coerce);
bool type_compare(const Type* t,int a0, int a1);


int index_of(Name n);
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

template<typename T>
int get_index_in(const vector<T>& src, T& value) { int i=0; for (i=0; i<src.size(); i++) {if (src[i]==value) return i;} return -1;}


template<typename T,typename U>
T verify_cast(U src){auto p=dynamic_cast<T>(src);ASSERT(p);return p;}
void newline(int depth);

template<class T,class Y> T* isa(const Y& src){ return dynamic_cast<T>(src);}
template<class T> T& orelse(T& a, T& b){if ((bool)a)return a; else return b;}
template<class T> void next(T*& n){if (n) n=n->next;}

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
extern int operator_flags(Name n);
bool isSymbolStart(char c);
extern int g_raw_types[];
#define RT_FLOATING 0x4000
#define RT_INTEGER 0x8000
#define RT_SIGNED 0x2000
#define RT_POINTER 0x1000
#define RT_SIMD 0x1000
#define RT_SIZEMASK 0x0ff;
enum Token {
	NONE=0,
	// top level structs & keywords. one,zero are coercible types..
	RAW_TYPES,INT=RAW_TYPES,UINT,SIZE_T,I8,I16,I32,I64,U8,U16,U32,U64,U128,BOOL,	// int types
	HALF,FLOAT,DOUBLE,FLOAT4,// floats
	CHAR,STR,VOID,VOIDPTR,ONE,ZERO,NULLPTR,BOOL_TRUE,BOOL_FALSE,
	AUTO,PTR,REF,SELF_T,NUM_RAW_TYPES=SELF_T,
	TUPLE,NUMBER,TYPE,NAME,	// type modifiers
	
	PRINT,FN,STRUCT,CLASS,TRAIT,VIRTUAL,STATIC,EXTERN, ENUM,ARRAY,VECTOR,UNION,VARIANT,WITH,MATCH, WHERE, SIZEOF, TYPEOF, NAMEOF,OFFSETOF,THIS,SELF,SUPER,VTABLEOF,CLOSURE,
	LET,VAR,
	CONST,MUT,VOLATILE,
	WHILE,IF,ELSE,DO,FOR,IN,RETURN,BREAK,CONTINUE,
	// delimiters
	OPEN_PAREN,CLOSE_PAREN,
	OPEN_BRACE,CLOSE_BRACE,
	OPEN_BRACKET,CLOSE_BRACKET,
	OPEN_TYPARAM,CLOSE_TYPARAM,
	// operators
	ARROW,DOT,MAYBE_DOT,FAT_ARROW,REV_ARROW,DOUBLE_COLON,SWAP,
	// unusual
	PIPE,BACKWARD_PIPE,COMPOSE,FMAP,SUBTYPE,SUPERTYPE,
	COLON,AS,NEW,DELETE,
	ADD,SUB,MUL,DIV,
	AND,OR,XOR,MOD,SHL,SHR,OR_ELSE,MAX,MIN,
	LT,GT,LE,GE,EQ,NE,
	LOG_AND,LOG_OR,
	ASSIGN,LET_ASSIGN,DECLARE_WITH_TYPE,PATTERN_BIND,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,AND_ASSIGN,OR_ASSIGN,XOR_ASSIGN,MOD_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,
	DOT_ASSIGN,
	PRE_INC,PRE_DEC,POST_INC,POST_DEC,
	NEG,DEREF,ADDR,NOT,COMPLEMENET, MAYBE_PTR,OWN_PTR,MAYBE_REF,VECTOR_OF,SLICE,SLICE_REF,
	COMMA,SEMICOLON,DOUBLE_SEMICOLON,
	// after these indices, comes indents
	ELIPSIS,RANGE,
	PLACEHOLDER,UNDERSCORE=PLACEHOLDER,
	IDENT,
	EXTERN_C,__VTABLE_PTR,__ENV_PTR,__ENV_I8_PTR,NUM_STRINGS
};
extern CgValue CgValueVoid();

struct NumDenom{int num; int denom;};


Name getStringIndex(const char* str,const char* end=0);
Name getStringIndexConcat(Name base, const char* s2);
const char* str(int);
inline int index(Name);
#if DEBUG<2
struct Name {
	int32_t m_index;
	Name()		{m_index=0;}
	Name(int i)		{m_index=i;}
	Name(const char* a, const char* end=0){
		if (!end) end=a+strlen(a);
		size_t len=end-a;
		m_index=(int)getStringIndex(a,end);
	}
	Name(const Name& b)	{m_index=b.m_index; }
	bool operator<(int b)const	{return m_index<b;}
	bool operator>(int b)const	{return m_index>b;}
	bool operator>=(int b)const	{return m_index>=b;}
	bool operator<=(int index)const {return m_index<=index;}
	bool operator==(const Name& b)const	{return m_index==b.m_index;}
	bool operator==(int b)const			{return m_index==b;}
	bool operator!=(const Name& b)const	{return m_index!=b.m_index;}
	void translate_typeparams(const TypeParamXlat& tpx);
	bool operator!()const{return m_index==0;}
	explicit operator bool()const{return m_index!=0;}
	explicit operator int()const{return m_index;}
};
inline int index(Name n){return n.m_index;}

#else
struct Name {
	int32_t m_index;
	const char* s;
	Name()			{m_index=0;}
	Name(int i)		{m_index=i; s=str(i);}
	Name(const char* a, const char* end=0){
		if (!end)
			end=a+strlen(a);
		m_index=(int)getStringIndex(a,end);
	}
	Name(const Name& b)	{m_index=b.m_index; s=str(m_index);}
	//	operator int32_t(){return index;}
	bool operator==(int b)const	{return m_index==b;}
	bool operator<(int b)const	{return m_index<b;}
	bool operator>(int b)const	{return m_index>b;}
	bool operator>=(int b)const	{return m_index>=b;}
	bool operator<=(int index)const {return m_index<=index;}
	bool operator==(const Name& b)const	{return m_index==b.m_index;}
	bool operator!=(const Name& b)const	{return m_index!=b.m_index;}
	void translate_typeparams(const TypeParamXlat& tpx);
	bool operator!()const{return m_index==0;}
	explicit operator bool()const{return m_index!=0;}
	explicit operator int()const{return m_index;}
};
int index(Name n){return n.m_index;}
#endif

//typedef int32_t RegisterName;
typedef Name RegisterName;

bool is_operator(Name name);
struct LLVMOp {
	int return_type;
	const char* op_signed;
	const char* op_unsigned;
};

const LLVMOp* get_op_llvm(Name opname,Name tyname); // for tokens with 1:1 llvm mapping
const char* get_llvm_type_str(Name tname);
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
extern CgValue CgValueVoid();
Name getStringIndex(const char* str,const char* end);
Name getNumberIndex(int num);	// ints in the type system stored like so
int getNumberInt(Name n);
float getNumberFloat(Name n);
const char* getString(const Name& index);
void indent(int depth);
inline const char* str(const Name& n){return getString(n);}
inline const char* str(int i){return i?g_Names.index_to_name[i].c_str():"";}
int match_typeparams(vector<Type*>& matched, const ExprFnDef* f, const ExprBlock* callsite);

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
// type inference
ResolvedType propogate_type(int flags,const Node*n, Type*& a,Type*& b);
ResolvedType propogate_type(int flags, Expr *n, Type*& a,Type*& b);
ResolvedType propogate_type_fwd(int flags,const Node* n, const Type* a,Type*& b);
ResolvedType propogate_type_fwd(int flags,Expr* e, const Type*& a);
ResolvedType propogate_type(int flags,Expr* e, Type*& a);
ResolvedType propogate_type(int flags,const Node* n, Type*& a,Type*& b,Type*& c);
ResolvedType propogate_type_fwd(int flags,const Node* n,const Type*& a,Type*& b,Type*& c);
ResolvedType propogate_type(int flags,const Node* n, ResolvedType& a,Type*& b);
ResolvedType propogate_type(int flags,const Node* n,ResolvedType& a,Type*& b,const Type* c);

struct CaptureVars;
// load data->vtb // if this matters it would be inlined
// load vtb->fn
// when the time comes - vtb->destroy()
//                       vtb->trace
struct LLVMType {
	Name name;
	bool is_pointer;
};

struct Node {
private:Type* m_type=0;

public:
	Node*	m_parent=0;					// for search & error messages,convenience TODO option to strip.
	Name name;
	RegisterName reg_name=0;			// temporary for llvm SSA calc. TODO: these are really in Expr, not NOde.
	bool reg_is_addr=false;
	SrcPos pos;						// where is it
	Node(){}
	ExprDef*	def=0;		// definition of the entity here. (function call, struct,type,field);
	Node*		next_of_def=0;
	void set_def(ExprDef* d);
	void clear_def();
	virtual void dump(int depth=0) const;
	virtual ResolvedType resolve(Scope* scope, const Type* desired,int flags){dbprintf("empty? %s resolve not implemented", this->kind_str());return ResolvedType(nullptr, ResolvedType::INCOMPLETE);};
	ResolvedType resolve_if(Scope* scope, const Type* desired,int flags){
		if (this) return this->resolve(scope,desired,flags);
		else return ResolvedType();
	}
	virtual const char* kind_str()const	{return"node";}
	virtual int get_name() const		{return 0;}
	virtual Name get_mangled_name()const {return name;}
	const char* get_name_str()const;
	const char* name_str()const			{return str(this->name);}
//	Name ident() const					{if (this)return this->name;else return 0;}
	virtual Node* clone() const=0;
	Node* clone_if()const				{ if(this) return this->clone();else return nullptr;}
	void dump_if(int d)const			{if (this) this->dump(d);}
	virtual void clear_reg()			{reg_name=0;};
	RegisterName get_reg(CodeGen& cg, bool force_new);
	RegisterName get_reg_new(CodeGen& cg);
	RegisterName get_reg_named(Name baseName, int* new_index, bool force_new);
	RegisterName get_reg_named_new(Name baseName, int* new_index);
	RegisterName get_reg_existing();
	virtual	vector<TParamDef*>*			get_typeparams(){ return nullptr;}
	Node*	parent()					{return this->m_parent;}
	void	set_parent(Node* p)			{this->m_parent=p;}
	virtual CgValue codegen(CodeGen& cg,bool contents);
	virtual bool is_undefined()const										{if (this && name==PLACEHOLDER) return true; return false;}
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const	{return ;}
	void find_vars_written_if(Scope*s, set<Variable*>& vars) const{ if(this)this->find_vars_written(s, vars);
	}
	void translate_typeparams_if(const TypeParamXlat& tpx){if (this) this->translate_typeparams(tpx);}
	virtual void translate_typeparams(const TypeParamXlat& tpx){ error(this,"not handled for %s",this->kind_str()); };
	virtual ExprOp* as_op()const			{error(this,"expected op, found %s:%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name as_name()const {
		error(this,"expected named item at node %s kind=%s",str(this->name),this->kind_str());
		return PLACEHOLDER;
	};
	bool is_ident()const;
	virtual ExprStructDef* as_struct_def()const;
	template<typename T> T* as()const{ auto ret= const_cast<T*>(dynamic_cast<T*>(this)); if (!ret){error(this,"expected,but got %s",this->kind_str());} return ret;};
	template<typename T> T* isa()const{ return const_cast<T*>(dynamic_cast<T*>(this));};
	virtual int recurse(std::function<int(Node* f)> f){dbprintf("recurse not implemented\n");return 0;};

	virtual CgValue compile(CodeGen& cg, Scope* sc);
	CgValue compile_if(CodeGen& cg, Scope* sc);
	virtual Node* instanced_by()const{return nullptr;}
	virtual ExprIdent*	as_ident() {return nullptr;}
	virtual ExprFor* 	as_for() {return nullptr;}
	virtual ExprFnDef*	as_fn_def() {return nullptr;}
	virtual const ExprFnDef*	as_fn_def() const {return nullptr;}
	virtual ExprBlock*	as_block() {return nullptr;}
	virtual TParamDef*	as_tparam_def() {return nullptr;}
	ExprBlock* as_block() const{return const_cast<Node*>(this)->as_block();}
	virtual ArgDef* as_arg_def() {return nullptr;}
	virtual Variable* as_variable() {return nullptr;}
	virtual const Variable* as_variable() const {return nullptr;}
	ArgDef*			as_field() {return this->as_arg_def();}
	virtual void verify() {};
	// abstract interface to 'struct-like' entities;
	virtual Type* get_elem_type(int index){error(this,"tried to get elem on name=%s kind=%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name get_elem_name(int index){error(this,"tried to get elem on %s %s",str(this->name),this->kind_str());return nullptr;}
	virtual int get_elem_index(Name name){error(this,"tried to get elem on %s %s",str(this->name),this->kind_str());return -1;}
	virtual int get_elem_count()const{return 0;}
	virtual size_t alignment()const {return 16;} // unless you know more..
	virtual ~Node(){
		error("dont call delete, we haven't sorted out ownership of Types or nodes. compiler implementation doesn't need to free anything. Types will be owned by a manager, not the ast ");
	}
	LLVMType get_type_llvm() const;
	virtual Type* eval_as_type()const		{return nullptr;};
	virtual ExprBlock* is_subscript()const	{return (ExprBlock*)nullptr;}
	virtual bool is_function_name()const	{return false;}
	virtual bool is_variable_name()const	{return false;}
	virtual Scope* get_scope()				{return nullptr;}
	Type* expect_type() const;
	Type* get_type() const		{ if(this) {::verify(this->m_type);return this->m_type;}else return nullptr;}
	Type*& type()				{::verify(this->m_type);return this->m_type;}
	const Type* type()const		{::verify(this->m_type);return this->m_type;}
	void type(const Type* t)	{::verify(t);this->m_type=(Type*)t;}
	void set_type(const Type* t);
	void clear_type(){m_type=0;};
	void force_type_todo_verify(const Type* t){ m_type=const_cast<Type*>(t);}
	Type*& type_ref()			{return this->m_type;}
	void dump_top()const;
};

typedef Type TParamVal;
// Type Parameter, actually Template Parameter as we generalize it.
struct Expr : public Node{					// anything yielding a value
public:
	int visited;					// anti-recursion flag.
};

/// TODO-Anything matchable by the template engine eg Type, Constants, Ident.. (how far do we go unifying templates & macros..)

bool type_params_eq(const vector<Type*>& a, const Type* tp);
bool type_params_eq(const vector<Type*>& a, const vector<Type*>& b);

struct Type : Expr{
	vector<TParamDef*> typeparams;
	//ExprDef* struct_def=0;	// todo: struct_def & sub are mutually exclusive.
	Type*	sub=0;					// a type is itself a tree
	Type*	next=0;
	Node* 	m_origin=0;				// where it comes from
	bool	tparam_index=-1;
	bool	is_generic(){return tparam_index>=0;}
	bool	has_non_instanced_typeparams()const;
	
	void set_origin(Node* t){m_origin=t;}
	Node* get_origin()const {return m_origin;}
	void push_back(Type* t);
	virtual const char* kind_str()const;
	Type(Node* origin, Name outer, Type* inner):Type(origin,outer){ push_back(inner);}
	Type(Node* origin,Name a,Name b): Type(origin,a, new Type(origin,b)){}
	Type(Node* origin,Name a,Name b,Name c): Type(origin,a,new Type(origin,b,c)){}
//		auto tc=new Type(origin,c); auto tb=new Type(origin,b); tb->push_back(tc); push_back(tb);
//	}
	Type(ExprStructDef* sd);
	Type(Name outer, ExprStructDef* inner);
	Type(Node* origin,Name i);
	Type(Name i,SrcPos sp);
	Type() { name=0;sub=0;next=0;}
	size_t	alignment() const;
	size_t	size() const;
	int	raw_type_flags()const	{int i=index(name)-RAW_TYPES; if (i>=0&&i<NUM_RAW_TYPES){return g_raw_types[i];}else return 0;}
	bool	is_int()const		{return raw_type_flags()&RT_INTEGER;}
	bool	is_float()const		{return raw_type_flags()&RT_FLOATING;}
	bool	is_signed()const	{return raw_type_flags()&RT_SIGNED;}
	bool	is_register()const	{return !is_complex();}
	bool	is_complex()const;
	bool	is_struct()const;
	bool	is_bool()const		{return name==BOOL;}
	bool	is_callable()const	{return name==FN||name==CLOSURE;}
	bool	is_qualifier()const	{return name==CONST||name==MUT;}
	bool	is_qualifier_or_ptr_or_ref()const{return is_qualifier()||is_pointer_or_ref();}
	struct FnInfo{ Type* args; Type* ret;Type* receiver;};
	FnInfo	get_fn_info(){
		if (!is_callable()) return FnInfo{nullptr,nullptr,nullptr};
		auto a=this->sub; auto ret=a->next; auto recv=ret?ret->next:nullptr;
		return FnInfo{a,ret,recv};
	}
	ExprStructDef* get_receiver()const;
	// we have a stupid OO receiver because we want C++ compatability;
	// we can use it for lambda too. We will have extention methods.
	void	set_fn_details(Type* args,Type* ret,ExprStructDef* rcv){
		ASSERT(this->is_callable());
		ASSERT(this->sub==0);
		this->push_back(args);
		this->push_back(ret);
		if (rcv) {
			ASSERT(args&&ret);
			this->push_back(new Type(rcv));
		}
	}
	
	Name array_size()const{
		ASSERT(this->sub);
		return this->sub->next->name;
	}
//	bool	is_ptr_to(const Type* other){return ((this->type==PTR) && this->sub->eq(other));}
//	bool	is_void_ptr()const	{if (this->type==VOIDPTR)return true;if(this->type==PTR && this->sub){if(this->type->sub==VOID) return true;}return false;};
	
	bool		has_typeparam(Scope* sc);
	bool 		is_typeparam(Scope* sc) const;
	const Type*	get_elem(int index) const;
	Type*		get_elem(int index);
	int			num_pointers()const;
	int			num_pointers_and_arrays()const;
	ExprStructDef*	struct_def_noderef()const;
	ExprStructDef*	get_struct_autoderef() const; // with autoderef
	bool			is_coercible(const Type* other) const{return is_equal(other,true);};
	bool			is_equal(const Type* other,bool coerce=false) const;
	bool			is_equal(const Type* other, const TypeParamXlat& xlat )const;
	// todo 'is_equal' rename to iscompatible, is_equal/coercible call it with flags
	bool			is_compatible(const Type* other,bool coerce=false) const;
	bool			is_auto(){return !this || this->name==AUTO;}
	void			dump_sub(int f
							 )const;
	void			dump(int depth)const;
	static Type*	get_bool();
	static Type*	get_void();
	static Type*	get_void_ptr();
	static Type*	get_int();
	Node*	clone() const;
	void	set_struct_def(ExprStructDef* sd);
	void	clear_struct_def();
	bool	is_array()const		{return name==ARRAY;}
	bool	is_template()const	{ return sub!=0;}
	bool	is_function() const	{ return name==FN;}
	bool	is_closure() const	{ return name==CLOSURE;}
	Type*	fn_return() const	{ if (is_callable()) return sub->next; else return nullptr;}
	Type*	fn_args_first() const		{ return sub->sub;} 	void	clear_reg()			{reg_name=0;};
	const Type*	strip_qualifiers()const{
		auto p=this;
		if(!p) return nullptr;
		while (p->is_qualifier()){p=p->sub;}
		return p;
	};
	bool	is_name(int n1)	const{return this->name==n1;}
	bool	is_name(int n1, int n2)const{return this->name==n1||this->name==n2;}
	bool	is_name(int n1, int n2,int n3)const	{return this->name==n1||this->name==n2||this->name==n3;}
	bool	is_pointer_or_ref()const		{return this->strip_qualifiers()->is_name(PTR,REF);}
	bool	is_pointer()const		{return this->strip_qualifiers()->is_pointer_or_ref();}//TODO deprecate, must be specific since pointers & references have subtle differences.
	bool 	is_pointer_not_ref()const	{if (!this) return false; return this->strip_qualifiers()->name==PTR;}
	bool	is_void()const			{return !this || this->name==VOID;}
	bool	is_void_ptr()const		{return this->is_pointer_not_ref() && this->sub && this->sub->name==VOID;}
	int		num_derefs()const		{if (!this) return 0;int num=0; auto p=this; while (p->is_pointer()){num++;p=p->sub;} return num;}
	Type*	deref_all() const		{if (!this) return nullptr;int num=0; auto p=this; while (p->is_pointer()||this->is_qualifier()){p=p->sub;}; return (Type*)p;}
	void translate_typeparams_sub(const TypeParamXlat& tpx,Type* inherit_replace);
	Name as_name()const override{
		return this->name;
	}
	ExprStructDef*	struct_def();
	ExprStructDef*	struct_def() const;
	

	void			translate_typeparams(const TypeParamXlat& tpx) override;
	virtual ResolvedType	resolve(Scope* s, const Type* desired,int flags);
	virtual void verify();
	CgValue	compile(CodeGen& cg, Scope* sc);
};

struct ExprScopeBlock : Expr{
	Scope*		scope=0;
};
struct ExprOp: public Expr{
	Expr	*lhs=0,*rhs=0;
	Node* clone() const;
	void clear_reg()						{lhs->clear_reg(); rhs->clear_reg();}
	ExprOp(Name opname,SrcPos sp, Expr* l, Expr* r){
		pos=sp;
		lhs=l; rhs=r;
		name=opname;
	}
	ExprOp(Name opname,SrcPos sp)			{name=opname; lhs=0; rhs=0;pos=sp;}
	void	dump(int depth) const;
	int		get_operator() const			{return index(this->name);}
	int		get_op_name() const				{return index(this->name);}
	bool	is_undefined()const				{return (lhs?lhs->is_undefined():false)||(rhs?rhs->is_undefined():false);}
	ExprOp*		as_op()const override		{return const_cast<ExprOp*>(this);}
	const char* kind_str()const override		{return"operator";}
	void 		translate_typeparams(const TypeParamXlat& tpx) override;
	ResolvedType resolve(Scope* scope, const Type* desired,int flags) override;
	void 		find_vars_written(Scope* s, set<Variable*>& vars) const override;
	void 		verify() override;
	CgValue compile(CodeGen& cg, Scope* sc);
};

/// 'ExpressionBlock' - expr( expr,expr,...)
///- any group of expressions
///  eg functioncall +args, compound statement, struct-initializer, subscript expr (like fncall)
struct ExprBlock :public ExprScopeBlock{
	// used for function calls and compound statement
	// started out with lisp-like (op operands..) where a compound statement is just (do ....)
	// TODO we may split into ExprOperator, ExprFnCall, ExprBlock
	// the similarity between all is
	
	short	bracket_type;	//OPEN_PARENS,OPEN_BRACES,OPEN_BRACKETS,(ANGLE_BRACKETS?)
	short	delimiter=0;//COMMA, SEMICOLON,SPACES?

	Expr*	call_expr=0;  //call_expr(argls...)  or {argsls...} call_expr[argls..] call_expr{argls}
	vector<Expr*>	argls;
	//ExprFnDef*	call_target=0;
	ExprBlock*	next_of_call_target=0;	// to walk callers to a function
	// these are supposed to be mutually exclusive substates, this would be an enum ideally.
	ExprBlock(){};
	ExprBlock(const SrcPos& p);
	bool	is_compound_expression()const	{return !call_expr && !index(name);}
	bool	is_tuple()const					{return this->bracket_type==OPEN_PAREN && this->delimiter==COMMA;}
	bool	is_struct_initializer()const	{return this->bracket_type==OPEN_BRACE && (this->delimiter==COMMA||this->delimiter==0);}
	bool	is_match() const				{return false;}
	bool	is_function_call()const			{return (this->call_expr!=0) && this->bracket_type==OPEN_PAREN && (this->delimiter==COMMA||this->delimiter==0);}
	bool	is_anon_struct()const			{return this->is_struct_initializer() && !this->call_expr;}
	bool	is_array_initializer()const		{return !this->call_expr && this->bracket_type==OPEN_BRACKET && this->delimiter==COMMA;}
	void	set_delim(int delim)			{delimiter=delim;}
	ExprBlock* is_subscript()const override	{if (this->bracket_type==OPEN_BRACKET && call_expr) return (ExprBlock*) this; return (ExprBlock*)nullptr;}
	ExprFnDef*	get_fn_call()const;
	Name		get_fn_name() const;
	void		dump(int depth) const;
	Node*		clone() const;
	bool		is_undefined()const;
	void		create_anon_struct_initializer();
	void			clear_reg()				{for (auto p:argls)p->clear_reg();if (call_expr)call_expr->clear_reg(); reg_name=0;};
	const char* kind_str() const  override		{return"block";}
	ExprBlock* 		as_block()  override 	{return this;}
	Scope*	get_scope()	override			{return this->scope;}
	void 			verify();
	CgValue 		compile(CodeGen& cg, Scope* sc);
	CgValue 		compile_sub(CodeGen& cg, Scope* sc,RegisterName dst);
	void	translate_typeparams(const TypeParamXlat& tpx) override;
	void	find_vars_written(Scope* s,set<Variable*>& vars )const override;
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags);
	ResolvedType	resolve_sub(Scope* scope, const Type* desired,int flags,Expr* receiver);
};
/// TODO a pattern might become different to Expr
/// simplest must behave like 'ident'
struct Pattern : Node {
	ResolvedType	resolve(Scope* sc, Type* desired, int flags){ASSERT(0 && "dont resolve pattern"); return ResolvedType();}
	Pattern* next=0;
	Pattern* sub=0;
	Node*	clone()const;
	
};

/// rust match, doesn't work yet - unlikely to support everything it does
/// C++ featureset extended with 2way inference can emulate match like boost variant but improved.
struct ExprMatch : Expr {
	const char*		kind_str() const {return "match";}
	CgValue			compile(CodeGen& cg, Scope* sc);
	ResolvedType	resolve(Scope* sc, Type* desired, int flags);
	Expr*		expr=0;
	MatchArm*	arms=0;
	Node*	clone()const;
};
struct MatchArm : ExprScopeBlock {
	/// if match expr satisfies the pattern,
	///  binds variables from the pattern & executes 'expr'
	Pattern*	pattern=0;
	Expr*		cond=0;
	Expr*		body=0;
	MatchArm*	next=0;
	void		dump(int depth);
	Node*		clone() const;
	void		translate_typeparams(const TypeParamXlat& tpx){}
	CgValue		compile_check(CodeGen& cg, Scope* sc, Expr* match_expr,CgValue match_val);
	// todo - as patterns exist elsewhere, so 'compile-bind might generalize'.
	CgValue		compile_bind(CodeGen& cg, Scope* sc, Expr* match_expr,CgValue match_val);
	ResolvedType	resolve(Scope* sc, Type* desired, int flags);
};

enum TypeId{
	// TODO: just equate these to the master token enum. avoid future confusion
	T_NONE,
	T_BOOL,T_INT,T_UINT,T_FLOAT,T_CONST_STRING,T_VOID,T_AUTO,T_ONE,T_ZERO,T_VOIDPTR,T_NULLPTR,
	T_WRONG_PRINT,T_FN,T_STRUCT,T_TUPLE,T_VARIANT,T_KEYWORD,T_NUM_TYPES,
};
bool is_type(int tok);
extern bool g_lisp_mode;
// module base: struct(holds fns,structs), function(local fns), raw module.

struct ExprFlow:Expr{	// control flow statements
};

/// any node that is a Definition, maintains list of refs
struct ExprDef :Expr{
	Node*	refs=0;
	void	remove_ref(Node* ref);
	virtual ExprStructDef* member_of()const{return nullptr;}
};

struct TParamDef: ExprDef{
	TParamVal* bound=0;	// eg traits/concepts
	TParamVal* defaultv=0;
	TParamDef(){};
	TParamDef(SrcPos sp,Name n, TParamVal* dv,TParamVal* bound_=0, TParamVal*defaultv_=0){pos=sp;name=n;defaultv=dv;bound=bound_;defaultv=defaultv_;};
	void dump(int depth)const;
	Node* clone() const override;
	TParamDef*	as_tparam_def() override{return this;}
	const char* kind_str()const{return "TParamDef";}
};

/// CaptureVars of local variables for a lambda function
/// hidden entity created in resolve. compile to 'C' might roll these manually?
struct CaptureVars : ExprDef{
	Name			tyname(){return name;};
	ExprFnDef*		capture_from=0;
	ExprFnDef*		capture_by=0;
	Variable*		vars=0;
	CaptureVars*		next_of_from=0;
	ExprStructDef*	the_struct=0;
	void 			coalesce_with(CaptureVars* other);
	ExprStructDef*	get_struct();
	CgValue			compile(CodeGen& cg, Scope* outer);
	Node* clone() const override{
		dbprintf("warning todo template instatntiation of captures\n");
		return nullptr;
	};
	Type*			get_elem_type(int i);
	Name			get_elem_name(int i);
	int				get_elem_count();
};

struct TypeDef : ExprDef{ // eg type yada[T]=ptr[ptr[T]]; or C++ typedef
	const char* kind_str()const{return "typedef";}
	vector<TParamDef*> typeparams;
};

struct ExprLiteral : ExprDef {
	TypeId	type_id;
	ExprLiteral* next_of_scope=0;	// collected..
	Scope* owner_scope=0;
	int llvm_strlen;
	
	union  {int val_int; int val_uint; float val_float; void* val_ptr;bool val_bool; const char* val_str;int val_keyword;} u;
	virtual const char* kind_str()const{return"lit";}
	void dump(int depth) const;
	ExprLiteral(const SrcPos&s);
	ExprLiteral(const SrcPos&s,float f);
	ExprLiteral(const SrcPos&s,int i);
	ExprLiteral(const SrcPos&s,const char* start,int length);
	ExprLiteral(const SrcPos&s,const char* start);// take ownership
	~ExprLiteral();
	Node* clone() const;
	size_t strlen() const;
	bool is_string() const		{return type_id==T_CONST_STRING;}
	bool is_undefined()const	{return false;}
	const char* as_str()const	{return type_id==T_CONST_STRING?u.val_str:"";}
	ResolvedType resolve(Scope* scope, const Type* desired,int flags);
	void translate_typeparams(const TypeParamXlat& tpx);
	CgValue compile(CodeGen& cg, Scope* sc);
};

/// 'ArgDef' used for function arguments and struct-fields.
/// both have form ident:type=<default expr>
struct ArgDef :ExprDef{
	Scope*	owner=0;
	void set_owner(Scope* s){
		ASSERT(owner==0 ||owner==s);
		this->owner=s;}
	ExprStructDef* member_of();
	uint32_t	size_of,offset;
	//Type* type=0;
	Expr*		default_expr=0;
	//Type* get_type()const {return type;}
	//void set_type(Type* t){verify(t);type=t;}
	//Type*& type_ref(){return type;}
	ArgDef(SrcPos p,Name n, Type* t=nullptr,Expr* d=nullptr){pos=p; name=n;set_type(t);default_expr=d; owner=0;}
	void dump(int depth) const;
	const char* kind_str()const override;
	~ArgDef(){}
	Node*	clone() const override;
	Name	as_name()const				{return this->name;}
	size_t	size()const					{return this->type()?this->type()->size():0;}
	size_t		alignment() const			{ return type()->alignment();}//todo, eval templates/other structs, consider pointers, ..
	void	translate_typeparams(const TypeParamXlat& tpx) override;
	ResolvedType	resolve(Scope* sc, const Type* desired, int flags) override;
	ArgDef*	as_arg_def()		{return this;}
};

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

enum VarKind{VkArg,Local,Global};
struct Variable : ExprDef{
	bool		on_stack=true;
	CaptureVars*	capture_in=0;	// todo: scope or capture could be unified? 'in this , or in capture ...'
	VarKind		kind;
	Scope*		owner=0;
	short capture_index;
	bool		keep_on_stack(){return on_stack||capture_in!=0;}
	Variable*	next_of_scope=0;	// TODO could these be unified, var is owned by capture or scope
	Variable*	next_of_capture=0;
	Expr*		initialize=0; // if its an argdef, we instantiate an initializer list
	Variable(SrcPos pos,Name n,VarKind k){this->pos=pos,name=n; initialize=0; owner=0;kind=k;this->set_type(0);}
	bool		is_captured()const{return this->capture_in!=nullptr;}
	Node* clone() const {
		auto v=new Variable(this->pos,name,this->kind);
		std::cout<<"clone "<<str(name)<<this<<" as "<<v<<"\n";
		v->initialize = verify_cast<Expr*>(this->initialize->clone_if());
		v->next_of_scope=0; v->set_type(this->get_type()); return v;
	}
	Variable*	as_variable() {return this;}
	const Variable*	as_variable() const {return this;}
	void dump(int depth) const;
};

/// 'Scope'-
/// scopes are created when resolving, held on some node types.
/// blocks which can add locals or named entities have them.

struct Scope {
	ExprDef*	owner_fn=0;	// TODO: eliminate this, owner might be FnDef,Struct,ExprBlock
	Expr*		node=0;
	Scope*		parent=0;
	Scope*		next=0;
	Scope*		child=0;
	Scope*		global=0;
	Scope*		capture_from=0;	// when resolving a local/lambda function, this is the owners' stack frame, look here for capturevars
	ExprLiteral*	literals=0;
	//Call* calls;
	Variable*	vars=0;
	NamedItems*	named_items=0;
	ExprFnDef*	templated_name_fns=0;// eg  fn FNAME[T,FNAME](x:T,y:T)->{...}  if the signature matches anything.. its' used. idea for implementing OOP & variants thru templates..
	// locals;
	// captures.
	const char* name()const;
private:
	Scope(){named_items=0; owner_fn=0;node=0;parent=0;next=0;child=0;vars=0;global=0;literals=0;}
public:
	Scope(Scope* p){ASSERT(p==0);named_items=0; owner_fn=0;node=0;parent=0;next=0;child=0;vars=0;global=0;literals=0;}
	void visit_calls();
	vector<TParamDef*>* get_tparams(){return owner_fn?owner_fn->get_typeparams():nullptr; // TODO: actually need to cascade them.
	}
	TParamDef*	get_typeparam_for(Type *t);
	bool		is_typeparam(Type* t) {return get_typeparam_for(t)!=nullptr;}
	ExprStructDef*	get_receiver();
	Variable*	try_capture_var(Name ident);	//sets up the capture block ptr.
	Variable*	find_fn_variable(Name ident,ExprFnDef* f);
	Variable*	get_fn_variable(Name name,ExprFnDef* f);
	Variable*	find_variable_rec(Name ident);
	Variable*	find_scope_variable(Name ident);
	Variable*	create_variable(Node* n, Name name,VarKind k);
	Variable*	get_or_create_scope_variable(Node* creator,Name name,VarKind k);
	ExprStructDef*	try_find_struct(const Type* t){return this->find_struct_sub(this,t);}
	ExprStructDef*	find_struct_of(const Expr* srcloc){
		auto t=srcloc->type();
		auto sname=t->deref_all();
		if (!sname->is_struct()) error(srcloc,t,"expected struct, got %s",sname->name_str());
		auto r=try_find_struct(sname);
//		if (!r)
//			error(srcloc,"cant find struct %s", sname->name_str());
		return r;
	}//original scope because typarams might use it.
	Scope*			parent_within_fn(){if (!parent) return nullptr; if (parent->owner_fn!=this->owner_fn) return nullptr; return parent;}
	ExprStructDef*	find_struct_sub(Scope* original,const Type* t);
	ExprStructDef*	find_struct_named(Name name);
	ExprStructDef*	find_struct_named(const Node* node){return find_struct_named(node->as_name());}
	ExprStructDef*	find_struct(const Node* node);
	ExprFnDef*	find_unique_fn_named(const Node* name_node,int flags=0, const Type* fn_type=nullptr); // todo: replace with fn type.
	ExprFnDef*	find_fn(Name name,const Expr* callsite, const vector<Expr*>& args,const Type* ret_type,int flags);
	void	add_struct(ExprStructDef*);
	void	add_fn(ExprFnDef*);
	void	add_fn_def(ExprFnDef*);
	void	dump(int depth) const;
	NamedItems* find_named_items_local(Name name);
	NamedItems* get_named_items_local(Name name);
	NamedItems* find_named_items_rec(Name name);
	ExprFor*		current_loop();
private:
	void push_child(Scope* sub) { sub->owner_fn=this->owner_fn; sub->next=this->child; this->child=sub;sub->parent=this; sub->global=this->global;}
public:
	Scope* parent_or_global()const{
		if (parent) return this->parent; else if (global && global!=this) return this->global; else return nullptr;
	}
	Scope* make_inner_scope(Scope** pp_scope,ExprDef* owner,Expr* sub_owner);
};

ResolvedType resolve_make_fn_call(Expr* receiver,ExprBlock* block,Scope* scope,const Type* desired);
/// if-else expression.
struct ExprIf :  ExprFlow {
	Scope*	scope=0;
	Expr*	cond=0;
	Expr*	body=0;
	Expr*	else_block=0;
	void	dump(int depth) const;
	ExprIf(const SrcPos& s){pos=s;name=0;cond=0;body=0;else_block=0;}
	~ExprIf(){}
	Node*	clone() const;
	bool	is_undefined()const			{
		if (cond)if (cond->is_undefined()) return true;
		if (body)if (body->is_undefined()) return true;
		if (else_block)if (else_block->is_undefined()) return true;
		return false;
	}
	const char*	kind_str()const	override	{return"if";}
	ResolvedType	resolve(Scope* scope,const Type*,int flags) ;
	void	find_vars_written(Scope* s,set<Variable*>& vars ) const override;
	void	translate_typeparams(const TypeParamXlat& tpx) override;
	CgValue			compile(CodeGen& cg, Scope* sc);
	Scope*	get_scope()	override			{return this->scope;}
};

/// For-Else loop/expression. Currrently the lowest level loop construct
/// implement other styles of loop as specializations that omit init, incr etc.
struct ExprFor :  ExprFlow {
	Expr* pattern=0;
	Expr* init=0;
	Expr* cond=0;
	Expr* incr=0;
	Expr* body=0;
	Expr* else_block=0;
	Scope* scope=0;
	void dump(int depth) const;
	ExprFor(const SrcPos& s)		{pos=s;name=0;pattern=0;init=0;cond=0;incr=0;body=0;else_block=0;scope=0;}
	bool is_c_for()const			{return !pattern;}
	bool is_for_in()const			{return pattern && cond==0 && incr==0;}
	~ExprFor(){}
	const char* kind_str()const	 override	{return"for";}
	bool is_undefined()const				{return (pattern&&pattern->is_undefined())||(init &&init->is_undefined())||(cond&&cond->is_undefined())||(incr&&incr->is_undefined())||(body&& body->is_undefined())||(else_block&&else_block->is_undefined());}
	Expr* find_next_break_expr(Expr* prev=0);
	Node* clone()const;
	Scope*		get_scope()override			{return this->scope;}
	ExprFor*	as_for()override			{return this;}
	ResolvedType resolve(Scope* scope,const Type*,int flags);
	void find_vars_written(Scope* s,set<Variable*>& vars ) const override;
	void translate_typeparams(const TypeParamXlat& tpx) override;
	CgValue compile(CodeGen& cg, Scope* sc);
};

struct Call;
struct FnName;

/// 'StructDef' handles everything for struct,trait,impl,vtable class,mod/namespace,
///
/// specific types derived expose a subset as language sugar.
/// a transpiler should handle conversions eg spot a 'struct' with pure virtuals -> trait, etc.

/// TODO a Rust Enum is sugar for a struct holding constants & derived variant structs.


struct StructInitializer{ // named initializer
	ExprBlock*		si; // struct_intializer
	Scope*			sc;
	vector<int>		field_indices;
	vector<ArgDef*> field_refs;
	vector<Expr*>	value;
	void map_fields()								{resolve(nullptr,0);}//todo..seperate out}
	StructInitializer(Scope* s,ExprBlock* block)	{si=block,sc=s;};
	ResolvedType resolve(const Type* desiredType,int flags);
};

struct ExprIdent :Expr{
	// TODO: definition pointer. (ptr to field,function,struct,typedef..)
	void		dump(int depth) const;
	Node*		clone() const;
	ExprIdent()	{};
	ExprIdent(const char* s,const char* e)	{name=Name(s,e);set_type(nullptr);}
	ExprIdent(Name n,SrcPos sp)				{pos=sp;name=n;set_type(nullptr);}
	ExprIdent(SrcPos sp,Name n)				{pos=sp;name=n;set_type(nullptr);}
	const char*	kind_str()const override		{return"ident";}
	Name		as_name()const override			{return name;};
	ExprIdent*	as_ident()						{return this;}
	bool		is_function_name()const	override;
	bool		is_variable_name()const	override;
	bool		is_placeholder()const			{return name==PLACEHOLDER;}
	bool		is_undefined()const				{return is_placeholder();}
	void		translate_typeparams(const TypeParamXlat& tpx) override;
	CgValue		compile(CodeGen&cg, Scope* sc) override;
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags) override;
};

struct TypeParamXlat{
	const vector<TParamDef*>& typeparams; const vector<TParamVal*>& given_types;
	TypeParamXlat();
	TypeParamXlat(	const vector<TParamDef*>& t, const vector<TParamVal*>& g):typeparams(t),given_types(g){}
	bool typeparams_all_set()const{
		for (int i=0; i<given_types.size(); i++) {
			if (given_types[i]==0) return false;
		}
		return true;
	}
	int typeparam_index(const Name& n) const;
	void dump(int depth)const;
};

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
	void dump();
};










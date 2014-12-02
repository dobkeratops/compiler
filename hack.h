#pragma once
extern "C" char* gets(char*);
#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string.h>
#include <functional>
#include <cstdio>

#ifdef DEBUG
#define CRASH {*(volatile long*)0=0;exit(0);}
#define ASSERT(x) if (!(x)) {printf("error %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );CRASH}
#define WARN(x) if (!(x)) {printf("warning %s:%d: %s, %s\n",__FILE__,__LINE__, __FUNCTION__, #x );}
#define TRACE printf("%s:%d: %s\n",__FILE__,__LINE__,__FUNCTION__);
#else
#define ASSERT(x)
#define WARN(x)
#define TRACE
#define CRASH
#endif

typedef int32_t OneBasedIndex;

#define R_FINAL 0x0001
#define R_REVERSE 0x0002
#define R_PUT_ON_STACK 0x8000

struct SrcPos {
	OneBasedIndex	line; //1-based line index
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
extern void verify_all_sub();
#define verify_all()
struct Node;
struct Name;
struct Scope;
extern void dbprintf(const char*,...);
extern void error(const Node*,const Node*,const char*,...);
void error(const Node* n,const Scope* s, const char* str, ... );
extern void error(const Node*,const char*,...);
extern void error(const char*,...);
extern void error_begin(const Node* n, const char* str, ... );
extern void error_end(const Node* n);
extern bool is_comparison(Name n);
extern Name getStringIndex(const char* str,const char* end) ;
extern const char* getString(const Name&);
extern bool is_operator(Name tok);
extern bool is_ident(Name tok);
extern bool is_type(Name tok);

bool is_number(Name n);

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
	HALF,FLOAT,DOUBLE,FLOAT4,CHAR,STR,VOID,VOIDPTR,ONE,ZERO,AUTO,	// float types,ptrs
	PTR,REF,NUM_RAW_TYPES=REF,TUPLE,NUMBER,TYPE,NAME,	// type modifiers
	
	PRINT,FN,STRUCT,CLASS,TRAIT,VIRTUAL,STATIC,ENUM,ARRAY,VECTOR,UNION,VARIANT,WITH,MATCH, SIZEOF, TYPEOF, NAMEOF,OFFSETOF,THIS,SELF,SUPER,VTABLEOF,CLOSURE,
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
	ASSIGN,LET_ASSIGN,ASSIGN_COLON,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,AND_ASSIGN,OR_ASSIGN,XOR_ASSIGN,MOD_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,
	DOT_ASSIGN,
	PRE_INC,PRE_DEC,POST_INC,POST_DEC,
	NEG,DEREF,ADDR,NOT,COMPLEMENET, MAYBE_PTR,OWN_PTR,MAYBE_REF,VECTOR_OF,SLICE,SLICE_REF,
	COMMA,SEMICOLON,DOUBLE_SEMICOLON,
	// after these indices, comes indents
	ELIPSIS,RANGE,
	PLACEHOLDER,
	IDENT
};
struct Name;
struct TypeParam;
struct ExprType;
struct ExprOp;
struct  ExprFnDef;
struct ExprFor;
struct ExprStructDef;
struct ExprIf;
struct ExprBlock;
struct ExprLiteral;
struct ExprIdent;
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


Name getStringIndex(const char* str,const char* end=0);
const char* str(int);
inline int index(Name);
#ifdef DEBUG_NAMES
struct Name {
	char* str;
	int32_t index;
	Name()		{index=0;str=0;}
	Name(const char* a, const char* end=0){
		if (!end) end=end+strlen(a);
		size_t len=end-a;
		str=(char*) malloc(len+1); memcpy(str,a,len); str[len]=0;
		index=getStringIndex(a,end);
	}
	Name(const Name& b)	{index=b.index; str=b.str;}
	bool operator==(const Name& b)const	{return index==b.index;}
	bool operator==(int b)const			{return index==b;}
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
		m_index=index(getStringIndex(a,end));
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
Name getStringIndex(const char* str,const char* end);
Name getNumberIndex(int num);	// ints in the type system stored like so
int getNumberInt(Name n);
float getNumberFloat(Name n);
const char* getString(const Name& index);
void indent(int depth);
inline const char* str(const Name& n){return getString(n);}
inline const char* str(int i){return g_Names.index_to_name[i].c_str();}
void match_typeparams(vector<Type*>& matched, const ExprFnDef* f, const ExprBlock* callsite);

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

struct Capture;
// load data->vtb // if this matters it would be inlined
// load vtb->fn
// when the time comes - vtb->destroy()
//                       vtb->trace

class Node {
public:
	Node*	m_parent=0;					// for search & error messages,convenience TODO option to strip.
	Name name;
	RegisterName regname;			// temporary for llvm SSA calc. TODO: these are really in Expr, not NOde.
	bool reg_is_addr=false;
	SrcPos pos;						// where is it
	Node(){}
	ExprDef*	def=0;		// definition of the entity here. (function call, struct,type,field);
	Node*		next_of_def=0;
	void set_def(ExprDef* d);
	virtual  ~Node(){};	// node ID'd by vtable.
	virtual void dump(int depth=0) const{};
	virtual ResolvedType resolve(Scope* scope, const Type* desired,int flags){dbprintf("empty? %s resolve not implemented", this->kind_str());return ResolvedType(nullptr, ResolvedType::INCOMPLETE);};
	virtual const char* kind_str()const	{return"node";}
	virtual int get_name() const		{return 0;}
	virtual Name get_mangled_name()const {return name;}
	const char* get_name_str()const;
	const char* name_str()const			{return str(this->name);}
//	Name ident() const					{if (this)return this->name;else return 0;}
	virtual Node* clone() const=0;
	Node* clone_if()const				{ if(this) return this->clone();else return nullptr;}
	void dump_if(int d)const			{if (this) this->dump(d);}
	virtual void clear_reg()			{regname=0;};
	RegisterName get_reg(Name baseName, int* new_index, bool force_new);
	RegisterName get_reg_new(Name baseName, int* new_index);
	RegisterName get_reg_existing();
	Node*	parent()					{return this->m_parent;}
	void	set_parent(Node* p)			{this->m_parent=p;}
	virtual CgValue codegen(CodeGen& cg,bool contents);
	virtual bool is_undefined()const										{if (this && name==PLACEHOLDER) return true; return false;}
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const	{return ;}
	virtual void translate_typeparams(const TypeParamXlat& tpx){ error(this,"not handled for %s",this->kind_str()); };
	virtual ExprOp* as_op()const			{error(this,"expected op, found %s:%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name as_name()const {
		error(this,"expected ident %s",str(this->name));
		return PLACEHOLDER;
	};
	bool is_ident()const;
	virtual ExprStructDef* as_struct_def()const;
	template<typename T> T* as()const{ auto ret= const_cast<T*>(dynamic_cast<T*>(this)); if (!ret){error(this,"expected,but got %s",this->kind_str());} return ret;};
	template<typename T> T* isa()const{ return const_cast<T*>(dynamic_cast<T*>(this));};
	virtual int recurse(std::function<int(Node* f)> f){dbprintf("recurse not implemented\n");return 0;};

	virtual CgValue compile(CodeGen& cg, Scope* sc);
	virtual Node* instanced_by()const{return nullptr;}
	virtual ExprIdent* as_ident() {return nullptr;}
	virtual ExprFor* as_for() {return nullptr;}
	virtual ExprFnDef* as_fn_def() {return nullptr;}
	virtual ExprBlock* as_block() {return nullptr;}
	virtual ArgDef* as_arg_def() {return nullptr;}
	virtual Variable* as_variable() {return nullptr;}
	ArgDef*			as_field() {return this->as_arg_def();}
	virtual void verify() {};
};



struct TypeParam: Node{
	Type* bound=0;	// eg traits/concepts
	Type* defaultv=0;
	Type* value=0;
	TypeParam(){};
	TypeParam(Name n, Type* dv){name=n;defaultv=dv;};
	void dump(int depth)const;
	Node* clone() const override;
	const char* kind_str()const{return "TypeParam";}
};

struct LLVMType {
	Name name;
	bool is_pointer;
};
extern void verify(const Type* t);

struct Expr : Node{					// anything yielding a value
private:Type* m_type;
public:
	int visited;					// anti-recursion flag.

	void dump(int depth) const;
	void dump_top()const;
	Expr();
	virtual const char* kind_str()const{return"expr";}
	Type* expect_type() const;
	Type* get_type() const		{ if(this) {::verify(this->m_type);return this->m_type;}else return nullptr;}
	Type*& type()				{::verify(this->m_type);return this->m_type;}
	const Type* type()const		{::verify(this->m_type);return this->m_type;}
	void type(const Type* t)	{::verify(t);this->m_type=(Type*)t;}
	void set_type(const Type* t){::verify(t);this->m_type=(Type*)t;};
	Type*& type_ref()			{return this->m_type;}
	LLVMType get_type_llvm() const;
	virtual Type* eval_as_type()const		{return nullptr;};
	virtual ExprBlock* is_subscript()const	{return (ExprBlock*)nullptr;}
	virtual bool is_function_name()const	{return false;}
	virtual bool is_variable_name()const	{return false;}
	virtual Scope* get_scope()				{return nullptr;}
};
struct Capture : Expr{
	Name			tyname(){return name;};
//	Type*			type=0;			// a hidden type, LLVM needs it.
	ExprFnDef*		capture_from=0;
	ExprFnDef*		capture_by=0;
	Variable*		vars=0;
	Capture*		next_of_from=0;
	ExprStructDef*	the_struct=0;
	void 			coalesce_with(Capture* other);
	ExprStructDef*	get_struct();
	virtual Node* clone() const{
		dbprintf("warning todo template instatntiation of captures\n");
		return nullptr;
	};
};

struct Type : Expr{
	vector<TypeParam> typeparams;
	ExprStructDef* struct_def=0;	// todo: struct_def & sub are mutually exclusive.
	Type*	sub=0;					// a type is itself a tree
	Type*	next=0;
	Node* 	m_origin=0;				// where it comes from
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
	Type() { name=0;sub=0;next=0; struct_def=0;}
	size_t	alignment() const;
	size_t	size() const;
	int	raw_type_flags()const	{int i=index(name)-RAW_TYPES; if (i>=0&&i<NUM_RAW_TYPES){return g_raw_types[i];}else return 0;}
	bool	is_int()const		{return raw_type_flags()&RT_INTEGER;}
	bool	is_float()const		{return raw_type_flags()&RT_FLOATING;}
	bool	is_signed()const	{return raw_type_flags()&RT_SIGNED;}
	bool	is_register()const	{return !is_complex();}
	bool	is_complex()const;
	bool	is_struct()const;
	bool	is_callable()const	{return name==FN||name==CLOSURE;}
	struct FnInfo{ Type* args; Type* ret;Type* receiver;};
	FnInfo	get_fn_info(){
		if (!is_callable()) return FnInfo{nullptr,nullptr,nullptr};
		auto a=this->sub; auto ret=a->next; auto recv=ret?ret->next:nullptr;
		return FnInfo{a,ret,recv};
	}
	ExprStructDef* get_receiver()const {
		if (this->sub)
			if (this->sub->next)
				if (this->sub->next->next)
					return this->sub->next->next->struct_def;
		return nullptr;
	}
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
	
	const Type*	get_elem(int index) const;
	Type*		get_elem(int index);
	int			num_pointers()const;
	int			num_pointers_and_arrays()const;
	ExprStructDef*	get_struct() const; // strip away all pointers.
	bool			eq(const Type* other) const;
	void			dump_sub()const;
	void			dump(int depth)const;
	static Type*	get_bool() ;
	static Type*	get_void() ;
	Node*	clone() const;
	bool	is_array()const		{return name==ARRAY;}
	bool	is_template()const	{ return sub!=0;}
	bool	is_function() const	{ return name==FN;}
	bool	is_closure() const	{ return name==CLOSURE;}
	Type*	fn_return() const	{ return sub->next;}
	Type*	fn_args() const		{ return sub->sub;} 	void	clear_reg()			{regname=0;};
	bool	is_pointer()const		{return (this && this->name==PTR) || (this->name==REF);}
	bool	is_void()const			{return !this || this->name==VOID;}
	int		num_derefs()const		{if (!this) return 0;int num=0; auto p=this; while (p->is_pointer()){num++;p=p->sub;} return num;}
	Type*	deref_all() const		{if (!this) return nullptr;int num=0; auto p=this; while (p->is_pointer()){p=p->sub;}; return (Type*)p;}
	virtual void			translate_typeparams(const TypeParamXlat& tpx);
	virtual ResolvedType	resolve(Scope* s, const Type* desired,int flags);
	virtual void verify();
	CgValue	compile(CodeGen& cg, Scope* sc);
};

struct ExprScopeBlock : Expr{};
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
	virtual ExprOp*		as_op()const		{return const_cast<ExprOp*>(this);}
	virtual const char* kind_str()const		{return"operator";}
	virtual void translate_typeparams(const TypeParamXlat& tpx);
	ResolvedType resolve(Scope* scope, const Type* desired,int flags);
	virtual void find_vars_written(Scope* s, set<Variable*>& vars) const;
	virtual void verify();
	CgValue compile(CodeGen& cg, Scope* sc);
};

struct ExprBlock :public ExprScopeBlock{
	// used for operators, function calls and compound statement
	// started out with lisp-like (op operands..) where a compound statement is just (do ....)
	// TODO we may split into ExprOperator, ExprFnCall, ExprBlock
	// the similarity between all is
	
	short	bracket_type;	//OPEN_PARENS,OPEN_BRACES,OPEN_BRACKETS,(ANGLE_BRACKETS?)
	short	delimiter=0;//COMMA, SEMICOLON,SPACES?

	Expr*	call_expr=0;  //call_expr(argls...)  or {argsls...} call_expr[argls..] call_expr{argls}
	vector<Expr*>	argls;
	//ExprFnDef*	call_target=0;
	Scope*		scope=0;
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
	virtual ExprBlock* is_subscript()const	{if (this->bracket_type==OPEN_BRACKET && call_expr) return (ExprBlock*) this; return (ExprBlock*)nullptr;}
	ExprFnDef*	get_fn_call()const;
	Name		get_fn_name() const;
	void		dump(int depth) const;
	Node*		clone() const;
	bool		is_undefined()const;
	void		create_anon_struct_initializer();
	void			clear_reg()				{for (auto p:argls)p->clear_reg();if (call_expr)call_expr->clear_reg(); regname=0;};
	virtual const char* kind_str()const		{return"block";}
	ExprBlock* 		as_block() override 	{return this;}
	virtual Scope*	get_scope()				{return this->scope;}
	void 			verify();
	CgValue 		compile(CodeGen& cg, Scope* sc);
	CgValue 		compile_sub(CodeGen& cg, Scope* sc,RegisterName dst);
	virtual void	translate_typeparams(const TypeParamXlat& tpx);
	virtual void	find_vars_written(Scope* s,set<Variable*>& vars )const;
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags);
	ResolvedType	resolve_sub(Scope* scope, const Type* desired,int flags,Expr* receiver);
};
struct ExprMatch : ExprBlock {
	// todo..
};
enum TypeId{
//	T_AUTO,T_KEYWORD,T_VOID,T_INT,T_FLOAT,T_CONST_STRING,T_CHAR,T_PTR,T_STRUCT,T_FN
	T_NONE,
	T_INT,T_UINT,T_FLOAT,T_CONST_STRING,T_VOID,T_AUTO,T_ONE,T_ZERO,T_VOIDPTR,
	T_WRONG_PRINT,T_FN,T_STRUCT,T_TUPLE,T_VARIANT,T_NUM_TYPES,
};
bool is_type(int tok);
extern bool g_lisp_mode;
// module base: struct(holds fns,structs), function(local fns), raw module.


struct ExprFlow:Expr{	// control flow statements
};

struct ExprDef :Expr{	// any that is a definition
	Node*	refs=0;
	void	remove_ref(Node* ref);
	virtual ExprStructDef* member_of()const{return nullptr;}
};

struct TypeDef : ExprDef{ // eg type yada[T]=ptr[ptr[T]]; or C++ typedef
	vector<TypeParam> typeparams;
};

struct ExprLiteral : ExprDef {
	TypeId	type_id;
	ExprLiteral* next_of_scope=0;	// collected..
	Scope* owner_scope=0;
	int llvm_strlen;
	
	union  {int val_int; int val_uint; float val_float; const char* val_str;int val_keyword;} u;
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
	virtual const char* kind_str()const;
	~ArgDef(){}
	Node*	clone() const;
	Name	as_name()const				{return this->name;}
	size_t	size()const					{return this->type()?this->type()->size():0;}
	int		alignment() const			{ return 4;}//todo, eval templates/other structs, consider pointers, ..
	virtual void	translate_typeparams(const TypeParamXlat& tpx);
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
struct Variable : ExprDef{
	bool		on_stack=false;
	Capture*	capture_in=0;	// todo: scope or capture could be unified? 'in this , or in capture ...'
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
	void dump(int depth) const;
};
// scopes are created when resolving
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
	Scope* make_inner_scope(Scope** pp_scope,ExprDef* owner,Expr* sub_owner){
		if (!*pp_scope){
			auto sc=new Scope;
			push_child(sc);
			sc->owner_fn=owner;
			*pp_scope=sc;
			ASSERT(sc->node==0);
			if(!sc->node){sc->node=sub_owner;}
		}
		
		return *pp_scope;
	};
};

ResolvedType resolve_make_fn_call(Expr* receiver,ExprBlock* block,Scope* scope,const Type* desired);
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
	virtual const char*	kind_str()const		{return"if";}
	ResolvedType	resolve(Scope* scope,const Type*,int flags) ;
	virtual void	find_vars_written(Scope* s,set<Variable*>& vars ) const;
	virtual void	translate_typeparams(const TypeParamXlat& tpx);
	CgValue			compile(CodeGen& cg, Scope* sc);
	virtual Scope*	get_scope()				{return this->scope;}
};

/// For-Else loop. Currrently the lowest level loop construct
/// TODO implement 'while..break' and make 'ForElse' desugar as while.
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
	virtual const char* kind_str()const		{return"for";}
	bool is_undefined()const				{return (pattern&&pattern->is_undefined())||(init &&init->is_undefined())||(cond&&cond->is_undefined())||(incr&&incr->is_undefined())||(body&& body->is_undefined())||(else_block&&else_block->is_undefined());}
	Expr* find_next_break_expr(Expr* prev=0);
	Node* clone()const;
	virtual Scope*		get_scope()			{return this->scope;}
	virtual ExprFor*	as_for()			{return this;}
	ResolvedType resolve(Scope* scope,const Type*,int flags);
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const;
	virtual void translate_typeparams(const TypeParamXlat& tpx);
	CgValue compile(CodeGen& cg, Scope* sc);
};

struct Call;
struct FnName;

struct ExprStructDef: ExprDef {
	// lots of similarity to a function actually.
	// but its' backwards.
	// it'll want TypeParams aswell.
	Name mangled_name=0;
	bool is_enum_=false;
	bool is_enum() { return is_enum_;}
	vector<TypeParam>	typeparams;	// todo move to 'ParameterizedDef; strct,fn,typedef,mod?
	vector<Type*>		instanced_types;
	vector<ArgDef*>			fields;
	vector<ExprLiteral*>	literals;
	vector<ExprStructDef*>	structs;
	vector<ExprFnDef*>		functions;
	Type*	inherits_type=0;
	Scope* scope=0;
	ExprStructDef* inherits=0,*derived=0,*next_of_inherits=0; // walk the derived types of this.
	ExprStructDef* vtable;
	bool is_generic() const;
	ExprStructDef* instances=0, *instance_of=0,*next_instance=0;
	ExprFnDef* constructor_fn=0;
	NamedItems* name_ptr=0;
//	ArgDef* find_field(Name name){ for (auto a:fields){if (a->name==name) return a;} error(this,"no field %s",str(name));return nullptr;}
	ArgDef* find_field(const Node* rhs)const;
	ArgDef* try_find_field(const Name n)const;
	int field_index(const Node* rhs){
		auto name=rhs->as_name();
		for (auto i=0; i<fields.size(); i++){
			if(fields[i]->name==name){
				((Node*)rhs)->set_def((ExprDef*)fields[i]);
				return i;
			}
		}
		return -1;
	}
	virtual const char* kind_str()const	{return"struct";}
	ExprStructDef* next_of_name;
	Name	get_mangled_name()const;
	ExprStructDef(SrcPos sp,Name n)		{name=n;pos=sp;name_ptr=0;inherits=0;inherits_type=0;next_of_inherits=0; derived=0; constructor_fn=0;name_ptr=0;next_of_name=0; instances=0;instance_of=0;next_instance=0;}
	int		alignment() const			{int max_a=0; for (auto a:fields) max_a=std::max(max_a,a->alignment()); return max_a;}
	ExprStructDef*	as_struct_def()const	{return const_cast<ExprStructDef*>(this);}
	void			dump(int depth)const;
	size_t			size() const;
	Node*			clone()const;
	Node*			clone_sub(ExprStructDef* into) const;
	void			inherit_from(Scope* sc, Type* base);
	virtual void	translate_typeparams(const TypeParamXlat& tpx);
	ExprStructDef*	get_instance(Scope* sc, const Type* type); // 'type' includes all the typeparams.
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags);
	void			roll_vtable();
	CgValue compile(CodeGen& cg, Scope* sc);
};

inline Type* Type::get_elem(int index){
	if (this->struct_def)
		return this->struct_def->fields[index]->type();
	ASSERT(index>=0);
	auto s=sub;
	for (;s&&index>0;s=s->next,--index){};
	ASSERT(index==0);
	return s;
}

/// TODO - move to plugin-architecture, EnumDef should be there implementing Rust style enum.
struct EnumDef  : ExprStructDef {
//	void dump(int depth)const;
//	virtual void translate_typeparams(const TypeParamXlat& tpx);
	Node* clone()const;
	const char* kind_str()const{return "enum";}
	EnumDef(SrcPos sp, Name n):ExprStructDef(sp,n){}
};
// todo.. generic instantiation: typeparam logic, and adhoc mo
struct  ExprFnDef : ExprDef {
	ExprFnDef*	next_of_module=0; // todo: obsolete this.
	ExprFnDef*	next_of_name=0;	//link of all functions of same name...
	ExprFnDef*	instance_of=0;	// Original function, when this is a template instance
	ExprFnDef*	instances=0;		// Linklist of it's instanced functions.
	ExprFnDef*	next_instance=0;
	ExprFnDef*	next_of_capture=0;	// one capture can be shared by multiple fn
	ExprBlock*	callers=0;			// linklist of callers to here
	NamedItems*	name_ptr=0;
	Scope*		scope=0;
	Capture*	captures=0;			//
	Capture*	my_capture=0;			// for closures- hidden param,environment struct passed in
	ExprStructDef* m_receiver=0;
	
	Type* ret_type=0;
	Type* fn_type=0;				// eg (args)->return
	bool resolved;
	// Partial specialization may add one specific parameter...
	// calls from un-instanced routines can partially implement?

	Name mangled_name=0;
	vector<TypeParam> typeparams;
	vector<ArgDef*> args;
	bool variadic;
	bool c_linkage=false;
	bool m_closure=false;
	// num_receivers =number of prefix 'arguments'
	// eg a::foo(b,c)  has 3 args, 1 receiver.   foo(a,b,c) has 3 args, 0 receiver.
	// its possible closures can desugar as methods?
	// makes UFCS easier; in matcher, simply prioritize candidates with correct count
	int num_receivers=0;
	Expr* body=0;
	ExprFnDef(){};
	ExprFnDef(SrcPos sp)	{pos=sp;variadic=false;scope=0;resolved=false;next_of_module=0;next_of_name=0;instance_of=0;instances=0;next_instance=0;name=0;body=0;callers=0;fn_type=0;ret_type=0;name_ptr=0;}
	void			set_receiver(ExprStructDef* sd); // not sure if it'll be arbitrary type
	ExprStructDef*	get_receiver();
	int		get_name()const {return index(name);}
	Name	get_mangled_name()const;
	bool	is_generic() const;
	bool	is_closure() const			{return my_capture!=0 || m_closure;}
	void	dump_signature() const;
	int		type_parameter_index(Name n) const;
	int		min_args()					{for (int i=0; i<args.size();i++){if (args[i]->default_expr) return i;} return (int)args.size();}
	int 	max_args()					{return variadic?1024:args.size();}
	bool	is_enough_args(int x)		{if (x<min_args()) return false; if (x> args.size() && !variadic) return false; return true;}
	bool	too_many_args(int x)		{return x>max_args();}
	virtual const char*	kind_str()const	{return"fn";}
	virtual ExprFnDef* as_fn_def() {return this;}
	Node*			instanced_by()const{if (this->instance_of){return this->refs;}else return nullptr;}
	void			dump(int ind) const;
	ResolvedType	resolve_function(Scope* definer,ExprStructDef* receiver, const Type* desired, int flags);
	ResolvedType	resolve(Scope* scope,const Type* desired,int flags);
	ResolvedType	resolve_call(Scope* scope,const Type* desired,int flags);
	Capture*		get_or_create_capture(ExprFnDef* src);
	virtual void	translate_typeparams(const TypeParamXlat& tpx);
	Expr*		get_return_value() const;
	Type*		return_type()const {
		auto x=get_return_value(); if (auto xt=x->get_type()) return xt;
		return this->ret_type;
	}
	bool	has_return_value() const{
		if (auto r=return_type()){
			return index(r->name)!=VOID;}
		else return false;
	}
	Node*	clone() const;
	bool	is_undefined()const	{return body==nullptr || body->is_undefined();};
	bool	is_extern()const	{return body==nullptr;}
	void	verify();
	CgValue compile(CodeGen& cg, Scope* sc);
	virtual Scope*	get_scope()				{return this->scope;}
};

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

// What details did we miss ?
// Codegen is a big issue.

struct ExprIdent :Expr{
	// TODO: definition pointer. (ptr to field,function,struct,typedef..)
	void		dump(int depth) const;
	Node*		clone() const;
	ExprIdent()	{};
	ExprIdent(const char* s,const char* e)	{name=Name(s,e);set_type(nullptr);}
	ExprIdent(Name n,SrcPos sp)				{pos=sp;name=n;set_type(nullptr);}
	ExprIdent(SrcPos sp,Name n)				{pos=sp;name=n;set_type(nullptr);}
	virtual const char*	kind_str()const		{return"ident";}
	virtual Name	as_name()const			{return name;};
	ExprIdent*		as_ident()				{return this;}
	virtual bool	is_function_name()const	{return dynamic_cast<ExprFnDef*>(this->def)!=0;}
	virtual bool	is_variable_name()const	{return dynamic_cast<Variable*>(this->def)!=0;}
	bool			is_placeholder()const	{return name==PLACEHOLDER;}
	bool			is_undefined()const		{return is_placeholder();}
	virtual void	translate_typeparams(const TypeParamXlat& tpx);
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags);
	CgValue			compile(CodeGen&cg, Scope* sc);
};

struct TypeParamXlat{
	const vector<TypeParam>& typeparams; const vector<Type*>& given_types;
	TypeParamXlat(	const vector<TypeParam>& t, const vector<Type*>& g):typeparams(t),given_types(g){}
	bool typeparams_all_set()const{
		for (int i=0; i<given_types.size(); i++) {
			if (given_types[i]==0) return false;
		}
		return true;
	}
	int typeparam_index(const Name& n) const;
};










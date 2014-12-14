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
struct TraitDef;
struct EnumDef;
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



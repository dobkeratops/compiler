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
#include <unistd.h>


using std::move;

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


#if DEBUG>=4
#define dbg4(X) X
#define DBLOC() dbprintf("%s:%d:",__FILE__,__LINE__);
#endif

#if DEBUG>=3
#define dbg3(X) X
#define dbg_varscope(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_emitconv(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_resolve(...) {DBLOC();dbprintf(__VA_ARGS__);}
#endif

#if DEBUG>=2
#define dbg2(X) X
#define dbg_raii(X) X
#define dbg_tparams(X) X
#define dbg_instancing(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_lambdas(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_fnmatch(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_type(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_vtable(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_generic(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_strings(...) {DBLOC();dbprintf(__VA_ARGS__);}
#define dbg_vcall(...) {DBLOC();dbprintf(__VA_ARGS__);}
#endif

#if DEBUG>=2
#define dbg(X) X
#endif

#ifndef DBLOC
#define DBLOC(X)
#endif

#ifndef dbg4
#define dbg4(X)
#endif

#ifndef dbg3 
#define dbg3(X)
#endif

#ifndef dbg2
#define dbg2(X)
#endif
#ifndef dbg
#define dbg(X)
#endif
#ifndef dbg_raii
#define dbg_raii(X)
#endif

#ifndef dbg_strings
#define dbg_strings(x,...)
#endif
#ifndef dbg_vcall
#define dbg_vcall(x,...)
#endif
#ifndef dbg_vtable
#define dbg_vtable(x,...)
#endif
#ifndef dbg_resolve
#define dbg_resolve(x,...)
#endif
#ifndef dbg_lambdas
#define dbg_lambdas(x,...)
#endif
#ifndef dbg_fnmatch
#define dbg_fnmatch(x,...)
#endif
#ifndef dbg_generic
#define dbg_generic(x,...)
#endif
#ifndef dbg_type
#define dbg_type(x,...)
#endif
#ifndef dbg_instancing
#define dbg_instancing(x,...)
#endif
#ifndef dbg_emitconv
#define dbg_emitconv(x,...)
#endif
#ifndef dbg_varscope
#define dbg_varscope(x,...)
#endif
#ifndef dbg_resolve
#define dbg_resolve(x,...)
#endif
#ifndef dbg_tparams
#define dbg_tparams(x,...)
#endif

typedef size_t index_t;

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
#define R_REVERSE_ONLY 0x0002
#define R_FORWARD_ONLY 0x0004
#define R_CALL 0x0008
#define R_PUT_ON_STACK 0x8000

extern void verify_all_sub();
#define verify_all()
struct Node;
struct Name;
struct TypeParam;
struct TParamDef;
//struct Expr;
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
//struct ResolveResult;
struct Module;
struct TParamXlat;
struct VarDecl;
struct NamedItems;
struct ExprMatch;
struct CaptureVars;

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

void verify_expr_op(const Node* p);
void verify_expr_block(const Node* p);
void verify_expr_fn_def(const Node* p);
void verify_expr_ident(const Node* p);
void verify_expr_block(const Node* p);
void verify_type(const Node* p);
void verify_all_sub();
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
template<class T> const T& orelse(const T& a, const T& b){if ((bool)a)return a; else return b;}
template<class T> T& orelse(T& a, T& b){if ((bool)a)return a; else return b;}
template<class T> void next(T*& n){if (n) n=n->next;}

#define PRECEDENCE 0xff
#define PREFIX 0x100
#define UNARY 0x200
#define ASSOC 0x400
#define WRITE_LHS 0x1000
#define WRITE_RHS 0x2000
#define WRITE (WRITE_LHS|WRITE_RHS)
#define READ_LHS 		0x4000
#define READ_RHS 		0x8000
// eg a..b a..<b etc
#define RANGE_OP		0x10000
#define READ (READ_LHS|READ_RHS)
#define MODIFY (READ_LHS|WRITE_LHS|READ_RHS|WRITE_RHS)
#define RWFLAGS (WRITE_LHS|READ_LHS|WRITE_RHS|READ_RHS)
extern int operator_flags(Name n);
bool isSymbolStart(char c,char c1);
extern int g_raw_types[];
#define RT_FLOATING 0x4000
#define RT_INTEGER 	0x8000
#define RT_SIGNED 	0x2000
#define RT_POINTER 	0x1000
#define RT_SIMD 	0x10000
#define RT_SIZEMASK 0x0ff;
// todo, for consistency make <X> and ?prefix version of all operators, not just a few special cased.
enum Token {
	NO_TOK=0,
	// top level structs & keywords. one,zero are coercible types..
	RAW_TYPES,INT=RAW_TYPES,UINT,SIZE_T,I8,I16,I32,I64,U8,U16,U32,U64,U128,BOOL,BOOL_REG,	// int types
	HALF,FLOAT,DOUBLE,FLOAT4,// floats
	CHAR,STR,VOID,VOIDPTR,ONE,ZERO,NULLPTR,BOOL_TRUE,BOOL_FALSE,
	AUTO,PTR,REF,SELF_T,NUM_RAW_TYPES=SELF_T,
	TUPLE,NUMBER,TYPE,NAME,	// type modifiers
	
	PRINT,FN,STRUCT,CLASS,TRAIT,IMPL,VIRTUAL,STATIC,EXTERN, ENUM,ARRAY,VECTOR,UNION,VARIANT,WITH,MATCH, WHERE, SIZEOF, TYPEOF, NAMEOF,OFFSETOF,THIS,SELF,SUPER,VTABLEOF,CLOSURE,
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
	ASSIGN,LET_ASSIGN,DECLARE_WITH_TYPE,PATTERN_BIND,EXPRESSION,FIELD_ASSIGN,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,AND_ASSIGN,OR_ASSIGN,XOR_ASSIGN,MOD_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,
	DOT_ASSIGN,MAYBE_ASSIGN,
	PRE_INC,PRE_DEC,POST_INC,POST_DEC,
	NEG,DEREF,ADDR,NOT,COMPLEMENT,OPTION,
	RVALUE_REF,MAYBE_PTR,OWN_PTR,MAYBE_REF,VECTOR_OF,SLICE,SLICE_REF,DOUBLE_QUESTION_MARK,TAG_QUESTION_MARK,TAG_MUL,TAG_ADD,TAG_SUB,TAG_DIV, MAYBE_ARROW,
	COMMA,SEMICOLON,DOUBLE_SEMICOLON,
	// after these indices, comes indents
	
	ELIPSIS,RANGE,RANGE_FIRST=RANGE, RANGE_LT,RANGE_GT,RANGE_GE,LT_RANGE,GT_RANGE,GE_RANGE,LT_RANGE_LT,GT_RANGE_GT,RANGE_LAST=GT_RANGE_GT, // range operators
	LIFETIME,PLACEHOLDER,UNDERSCORE=PLACEHOLDER,
	IDENT,
	EXTERN_C,__VTABLE_PTR,__DATA_PTR,__ITERATOR,NEXT,BEGIN,END,E_SOME,E_NONE,__PARENT_PTR,__ENV_PTR,__DISCRIMINANT, __ENV_I8_PTR,DYNAMIC_ARRAY, STRING, UNIQUE_PTR,DICTIONARY, GC_PTR,
	__DESTRUCTOR,	// ~ClassName() c++ destructor
	DROP,		// token for Rust destructor, just translated into '__DESTRUCTOR'
	__SET_DEFAULT_VALUES,	// inserted into constructor calls, sets up given default values
	__VISIT,	// an auto-rolled function for reflection, visits all fields with given object
	__VERIFY,	// an auto-rolled function calling '__verify' on components
	NUM_STRINGS
};
extern CgValue CgValueVoid();

struct NumDenom{int num; int denom;};


extern CgValue CgValueVoid();

enum TypeId{
	// TODO: just equate these to the master token enum. avoid future confusion
	T_NONE,
	T_BOOL,T_INT,T_UINT,T_FLOAT,T_CONST_STRING,T_VOID,T_AUTO,T_ONE,T_ZERO,T_VOIDPTR,T_NULLPTR,
	T_WRONG_PRINT,T_FN,T_STRUCT,T_TUPLE,T_VARIANT,T_KEYWORD,T_NUM_TYPES,
};
extern bool g_lisp_mode;
// module base: struct(holds fns,structs), function(local fns), raw module.
extern const char* g_operator_symbol[];


// simplified vector<T> easier to debug,
// break dependance on C++stdlib for easier transpile.

template<typename T>
struct Slice{		// does not own
	T* data;
	int num;
	T& operator[](int i){ return data[i];}
	const T& operator[](int i)const {return data[i];}
	int size(){return num;}
	~Slice();
	Slice(T* d,int n):data(d),num(n){}
	template<typename F>
	void foreach(F& f){ for (int i=0; i<num; i++){ f(data[i]);};}
	T* begin(){return data;}
	T* end(){return data+num;}
	const T* begin()const{return data;}
	const T* end()const {return data+num;}
	Slice<T> slice(int s,int e){return Slice(data+s,e-s);}
};
template<typename T>
struct Vec{
	T* data=nullptr;
	int32_t	num=0;// fills a 32gb machine fine.
	int32_t	cap=0;
	T& operator[](int i){
		dbg(ASSERT(i>=0&&i<=num));return data[i];
	}
	const T& operator[](int i)const{
		dbg(ASSERT(i>=0&&i<=num));return data[i];
	}
	Vec(Vec<T>&& src){
		this->take_from(src);
	}
	Vec(const Vec<T>& src){
		resize_sub(src.num,src.num);
		for (auto i=0; i<num; i++){
			data[i]=src[i];
		}
	}
	Vec(Vec<T>&& src, const T& val){
		this->take_from(src);
		this->push_back(val);
	}
	Vec<T> clone()const{return Vec<T>(*this);}
	void take_from(Vec<T>& src){
		reserve(num+src.size());
//		data=src.data;
//		num=src.num;
//		cap=src.cap;
		num=src.size();
		for (auto i=0; i<src.size(); i++){
			data[i]=move(src[i]);
		}
		src.data=0;
		src.num=0;
		src.cap=0;
	}
	void resize_sub(int newsize,int newcap){
		if (newcap<newsize) newcap=newsize;
		int i;
		for (i=newsize;i<num;i++){
			data[i].~T();
		}
		realloc_sub(newcap);
		for (i=num;i<newsize;i++){
			new(&data[i]) T();
		}
		num=newsize;
	}
	void resize_tofit(int newsize){
		resize_sub(newsize,newsize);
	}
	void resize(int newsize){
		if (newsize!=num){
			return resize_sub(newsize,newsize>cap?(cap?cap*2:newsize):(newsize<(cap/2)?(cap/2):cap));
		}
	}
	int32_t size()const {return num;}
	T* begin(){return data;}
	T* end(){return data+num;}
	const T* begin()const {return data;}
	const T* end()const {return data+num;}
	T& back(){ASSERT(num>0);return data[num-1];}
	const T& back()const {ASSERT(num>0);return data[num-1];}
	T& front() {ASSERT(num>0);return data[0];}
	const T& front()const {ASSERT(num>0);return data[0];}
	// todo && moves
	T pop(){return pop_back();}// at most efficient place
	void push(T src){return push_back(src);}// at most efficient place
	T pop_back(){
		ASSERT(num>0);
		T ret=data[num-1];
		resize(num-1);
		return ret;
	}
	T pop_front(){
		ASSERT(num>0);
		T ret=data[0];
		for (int i=1; i<num; i++) data[i-1]=data[i];
		return ret;
	}
	void insert(int pos,T item){
		resize(num+1);
		ASSERT(data!=nullptr);
		for (int32_t i=num-1; i>pos; i--){
			this->data[i]=this->data[i-1];
		}
		ASSERT(pos<num);
		data[pos]=item;
	}
	T remove(int pos){
		ASSERT(num>0);
		auto x=data[pos];
		for (int i=pos; i<num-1; i++) data[i]=data[i+1];
		return x;
	}
	T remove_swap_back(int pos){
		ASSERT(num>0);
		auto x=data[pos];
		data[pos]=back();
		resize(num-1);
		return x;
	}
	inline void push_front(T item){
		insert(0,item);

	}
	inline void push_back(T item){
		insert(num,item);
	}
	// grr. this is why we're writing new lang. want map((A)->B)->Vec<B>,filter etc
	template<typename F>
	void foreach(F& f){ for (int i=0; i<num; i++) f(data[i]);}
	template<typename F>
	void foreach(const F& f)const { for (int i=0; i<num; i++) f(data[i]);}
	Slice<T> range(int s,int e){
		return Slice<T>(data+s,e-s);
	}
	Slice<T> as_slice(){return range(0,num);}

	void shrink_to_fit(){if (num!=cap)resize_sub(num,num);}
	void reserve(int x){if (x!=cap){resize_sub(num,x);}}
	~Vec(){
		resize_sub(0,0);
	}
	Vec(){
	}
	void realloc_sub(int newcap){
		if (cap!=newcap){
			data=(T*)realloc(data,sizeof(T)*newcap);
			cap=newcap;
		}
	}

	template<typename B>
	Vec(const Vec<B>& src){
		realloc(src.size());
		int i;
		num=src.size();
		for (i=0; i<src.size(); i++) {// invoke copy cons inplace
			new(data[i]) T(src[i]);
		}
	}
};
template<typename T>
Vec<T> operator+(Vec<T>&& a, const Vec<T>& b){
	Vec<T> ret;
	ret.take_from(a);
	ret.concat(b);
	return ret;// RVO or move?
}
template<typename T>
Vec<T> operator+(Vec<T>&& a, const T& b){
	Vec<T> ret;
	ret.resize_tofit(a.size()+1);
	for (auto i=0; i<a.size(); i++){ ret.data[i]=a[i];}
	ret.data[a.size()]=b;
	return ret;
}




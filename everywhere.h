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
struct ExprTypeDef;
struct Scope;
struct Type;
struct Variable;
struct ResolvedType;
struct Module;
struct TypeParamXlat;
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
	NONE=0,
	// top level structs & keywords. one,zero are coercible types..
	RAW_TYPES,INT=RAW_TYPES,UINT,SIZE_T,I8,I16,I32,I64,U8,U16,U32,U64,U128,BOOL,BOOL_REG,	// int types
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
	ASSIGN,LET_ASSIGN,DECLARE_WITH_TYPE,PATTERN_BIND,EXPRESSION,FIELD_ASSIGN,
	ADD_ASSIGN,SUB_ASSIGN,MUL_ASSSIGN,DIV_ASSIGN,AND_ASSIGN,OR_ASSIGN,XOR_ASSIGN,MOD_ASSIGN,SHL_ASSIGN,SHR_ASSIGN,
	DOT_ASSIGN,MAYBE_ASSIGN,
	PRE_INC,PRE_DEC,POST_INC,POST_DEC,
	NEG,DEREF,ADDR,NOT,COMPLEMENT,OPTION,
	MAYBE_PTR,OWN_PTR,MAYBE_REF,VECTOR_OF,SLICE,SLICE_REF,DOUBLE_QUESTION_MARK,TAG_QUESTION_MARK,TAG_MUL,TAG_ADD,TAG_SUB,TAG_DIV, MAYBE_ARROW,
	COMMA,SEMICOLON,DOUBLE_SEMICOLON,
	// after these indices, comes indents
	ELIPSIS,RANGE, RANGE_LT,RANGE_GT,RANGE_GE,RANGE_LE,
	PLACEHOLDER,UNDERSCORE=PLACEHOLDER,
	IDENT,
	EXTERN_C,__VTABLE_PTR,__DATA_PTR,__PARENT_PTR,__ENV_PTR,__DISCRIMINANT, __ENV_I8_PTR,DYNAMIC_ARRAY, STRING, UNIQUE_PTR,DICTIONARY, GC_PTR,  NUM_STRINGS
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



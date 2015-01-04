#pragma once
#include "everywhere.h"

/// TODO suggestion searches

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
const char* operator_symbol(Name tok);
const char* symbol_of(Name tok);
const char* symbol_of(Name tok);
int precedence(Name ntok);
int is_prefix(Name ntok);
int arity(Name ntok);
int is_right_assoc(Name ntok);
int is_left_assoc(Name ntok);
bool is_number(Name n);
Name get_infix_operator(Name tok);
Name get_prefix_operator(Name tok);

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
	void translate_tparams(const TParamXlat& tpx);
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
	void translate_tparams(const TParamXlat& tpx);
	bool operator!()const{return m_index==0;}
	explicit operator bool()const{return m_index!=0;}
	explicit operator int()const{return m_index;}
};
int index(Name n){return n.m_index;}
#endif

//typedef int32_t RegisterName;
typedef Name RegisterName;

bool is_operator(Name name);
extern const char* g_token_str[];
extern int g_tok_info[];

struct StringTable {
	enum Flags :char {String,Number};
	int	nextId= 0;
	bool verbose;
	map<string,int>	names;
	MyVec<string> index_to_name; //one should be index into other lol
	MyVec<char>	flags;
	StringTable(const char** initial);
	int get_index(const char* str, const char* end,char flags);
	void dump();
};
extern StringTable g_Names;


Name getStringIndex(const char* str,const char* end);
Name getNumberIndex(float num);	// ints in the type system stored
Name getNumberIndex(int num);	// ints in the type system stored like so
int getNumberInt(Name n);
float getNumberFloat(Name n);
const char* getString(const Name& index);
void indent(int depth);
inline const char* str(const Name& n){return getString(n);}
inline const char* str(int i){if (i<0) return "";return i?g_Names.index_to_name[i].c_str():"";}
struct Type;
void print_tok(Name n);
bool is_type(int tok);
void find_completions(Name n,std::function<void(Name n,int)> f);
bool is_range_operator(Name tok);

struct LLVMType {
	Name name;
	bool is_pointer;
};



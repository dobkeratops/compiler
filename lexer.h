#pragma once
#include "compiler.h"
extern const char* g_filename;
// STUPID HEADER FILES STUPID CLASSES
/*
template<typename T>
T& operator<<(T& dst, const Bar& src) { dst<<src.name<<src.x;return dst;};
template<typename T>
T& operator<<(T& dst, const Foo& src) { dst<<src.bar; dst<<src.indices;return dst;};
*/

bool isSymbolStart(char c);
bool isSymbolCont(char c);
bool isNumStart(char c);
bool isNum(char c);
bool isWhitespace(char c);
bool isOperator(char c);

struct Lexer {
	char filename[512];
	SrcPos	pos;
	enum {MAX_DEPTH=32};
	SrcPos	bracket_pos[MAX_DEPTH];
	int		bracket[MAX_DEPTH];
	const char* buffer=0,*tok_start=0,*tok_end=0,*prev_start=0,*line_start=0;
	Name curr_tok;int typaram_depth=0;
#ifdef WATCH_TOK
	char watch_tok[64][12];
#endif
	bool error_newline=false;
	int indent=0,depth=0;

	void error(const char* str,...);
	Lexer(const char* src,const char *filename_);
	void advance_sub(bool (*sub)(char c));
	void advance_operator();
	void advance_string(char quote);
	bool is_comment(const char* c);
	void skip_whitespace();
	
	void typeparam_hack();
	static int close_of(int tok);
	void advance_tok();
	void advance_tok_sub();
	Name eat_tok();
	Name eat_if(Name a, Name b, Name c);
	Name eat_if(Name a, Name b);
	Name eat_if_not(Name i);
	bool eat_if(Name i);
	bool is_placeholder()const;
	Name eat_if_placeholder();
	Name eat_ident();
	int eat_int();
	const char* eat_string();
	NumDenom eat_number();
	float eat_float();
	bool is_next_number() const;
	bool is_next_literal() const;
	bool is_next_string() const;
	bool is_next_char() const;
	
	Name peek_tok();
	void reverse();
	Name expect(Name t, const char* err="");
	Name expect(Name a,Name b, const char* err="");
};
typedef Lexer TokenStream;

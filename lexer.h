#pragma once
#include "stringtable.h"
#include "everywhere.h"
extern const char* g_filename;
// STUPID HEADER FILES STUPID CLASSES
/*
template<typename T>
T& operator<<(T& dst, const Bar& src) { dst<<src.name<<src.x;return dst;};
template<typename T>
T& operator<<(T& dst, const Foo& src) { dst<<src.bar; dst<<src.indices;return dst;};
*/
extern SrcPos g_srcpos;	// hack sorry,
bool isSymbolStart(char c,char c1);
bool isSymbolCont(char c,char c1);
bool isNumStart(char c,char c1);
bool isNum(char c,char c1);
bool isWhitespace(char c,char c1);
bool isOperator(char c,char c1);
int	close_of(int open);
inline int	close_of(Name open){return close_of((int)open);}

struct IndentLevel{
	int16_t spaces=0,tabs=0;
	bool is_unset()const{return spaces==0&&tabs==0;}
	void unset(){spaces=0;tabs=0;}
	int val(){return spaces+tabs*4;}// rash assumption, TODO..
	bool operator !=(const IndentLevel&other)const {
		return spaces!=other.spaces || tabs!=other.tabs;
	}
	bool operator >(const IndentLevel&other)const {
		return spaces>other.spaces || tabs>other.tabs;
	}
	bool operator <(const IndentLevel&other)const {
		return spaces<other.spaces || tabs<other.tabs;
	}
	bool confused(const IndentLevel& other)const {
		return (spaces<other.spaces && tabs>other.tabs) ||
			(spaces>other.spaces && tabs<other.tabs);
	}
};
struct Lexer {
	// todo: we could avoid repeat nonsense, encapsulating 'terminator' in lexer. 'src.push_terminator(..)'/'pop terminator()'... 'if src.is_terminator() break'. 
	char filename[512];
	SrcPos	pos;
	SrcPos	prev_pos;	// needed to correctly locate nodes after eat_tok()
	enum {MAX_DEPTH=32};
	IndentLevel m_indent[MAX_DEPTH];
	IndentLevel	curr_indent;
	SrcPos	bracket_open_pos[MAX_DEPTH];
	int		bracket_close[MAX_DEPTH];
	const char* buffer=0,*tok_start=0,*tok_end=0,*prev_start=0,*line_start=0;
	Name curr_tok;int typaram_depth=0;
#ifdef WATCH_TOK
	char watch_tok[64][12];
#endif
	bool error_newline=false;
	int indent=0,depth=0;

	void error(const char* str,...);
	Lexer(const char* src,const char *filename_);
	void advance_sub(bool (*sub)(char c,char c1));
	void advance_operator();
	void advance_string(char quote);
	bool is_comment(const char* c);
	void skip_whitespace();
	
	void typeparam_hack();
	static int close_of(int tok);
	void advance_tok();
	void advance_tok_sub();
	void begin_lambda_bar_arglist();
	Name eat_tok();
	Name eat_if(Name a, Name b, Name c,Name d){if (is_next(a,b,c,d)) return eat_tok(); else return 0;}
	Name eat_if(Name a, Name b,Name c);
	Name eat_if(Name a, Name b);
	Name eat_if_not(Name i);
	bool eat_if(Name i);
	bool eat_if_lambda_bar(){//rust syntax- bracket matching must know |...|
		if (eat_if(OR)){begin_lambda_bar_arglist();return true;}return false;
	}
	bool is_placeholder()const;
	Name eat_if_placeholder();
	Name eat_ident();
	int eat_int();
	const char* eat_string_alloc();
	Name eat_if_string();
	NumDenom eat_number();
	float eat_float();
	bool is_next_number() const;
	bool is_next_literal() const;
	bool is_next_string() const;
	bool is_next_char() const;
	bool is_next(Name n) const;
	bool is_next(Name a, Name b){return is_next(a)||is_next(b);}
	bool is_next(Name a, Name b, Name c){return is_next(a)||is_next(b,c);}
	bool is_next(Name a, Name b, Name c,Name d){return is_next(a,b)||is_next(c,d);}
	
	Name peek_tok() const;
	void reverse();
	Name expect(Name t, const char* err="");
	Name expect(Name a,Name b, const char* err="");
};
typedef Lexer TokenStream;

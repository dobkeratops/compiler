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
	const char* buffer=0,*tok_start=0,*toik_end=0,*prev_start=0,*line_start=0;
	Name curr_tok;int typaram_depth=0;
#ifdef WATCH_TOK
	char watch_tok[64][12];
#endif
	bool error_newline=false;
	int indent=0,depth=0;

	
	void error(const char* str,...){
		if(!error_newline){printf("\n");}
		printf("%s:%d:",filename,pos.line);
		char tmp[1024];
		va_list arglist;
		
		va_start( arglist, str );
		vsprintf(tmp, str, arglist );
		va_end( arglist );
		printf("\n"); error_newline=true;
	}
	Lexer(const char* src,const char *filename_){
		strncpy(filename,filename_,512);
		g_filename=filename;
		
		line_start=src;
		pos.set(1,0);
		buffer=src;
		curr_tok=-1;
		tok_start=tok_end=buffer;
		advance_tok();
	}
	
	void advance_sub(bool (*sub)(char c)){
		while ( *tok_end && sub(*tok_end)) tok_end++;
	}
	void advance_operator() {
		int match=0;
		int longest=0;
		for (int i=0; i<IDENT; i++) {
			int len=0;
			const char* cmp=g_token_str[i];
			int j=0;
			for (; cmp[j] ; j++,len++) {
				if (tok_start[j]!=cmp[j]) break;
			}
			if (!cmp[j]) {// got all?
				if (len>longest){longest=len;match=i;}
			}
		}
		tok_end=tok_start+longest;
	}
	void advance_string(char quote) {
		tok_start=tok_end;
		tok_end++;
		while (*tok_end && *tok_end!=quote) {
			if (*tok_end=='\\' && tok_end[1]) tok_end++; //skip backslashed quotes inside..
			tok_end++;
		}
		if (*tok_end)
			tok_end++; // step past last quote.
	}
	bool is_comment(const char* c){
		if (*c && c[0]=='/'){if(c[1]=='/') return true;} return false;
	}
	void skip_whitespace(){
		bool newline=false;
		while ((isWhitespace(*tok_end) || is_comment(tok_end))&&*tok_end) {
			if (is_comment(tok_end)){
				// skip comment
				tok_end+=2;
				while (*tok_end!='\n' && *tok_end){
					tok_end++;
				}
			}
			if (*tok_end=='\n') {pos.line++;line_start=tok_end; typaram_depth=0;indent=0;newline=true;}
			if (newline)indent++;
			pos.col=tok_end-line_start;
			tok_end++;
		}
	}
	
	void typeparam_hack(){
		if (typaram_depth){
			if (curr_tok==GT){
				typaram_depth--;
				curr_tok=CLOSE_TYPARAM;
			}
			if (typaram_depth>=2 &&curr_tok==SHR){	//< < >>  consume one..
				typaram_depth--;
				curr_tok=CLOSE_TYPARAM;
				tok_end--;
				
			}
		}
		if (curr_tok==LT){
			// hack for reading C++ template type parameters.
			// we accept a subset of uses: when <..> appear on the same line, not seperated by close-other-pair ) ] }  or ";" or "->" - we assume it *is* a template.
			// args(a<b,c>d) is an example of a false posative. we simply throw an error requiring disambiguation with more parens
			// multiline typarams not accepted, etc.
			
			const char* s;
			int potential=0;
			int paren_depth=0,brace_depth=0,bracket_depth=0,ambiguity=0;
			for (s=tok_end; *s && s[1] && *s!='\n'; s++){
				char c=s[0]; char c1=s[1];
				if (c=='(') paren_depth++;
				if (c=='[') brace_depth++;
				if (c=='{') bracket_depth++;
				if (c==')') paren_depth--;
				if (c==']') brace_depth--;
				if (c=='}') bracket_depth--;
				if (c==',') ambiguity=1;
				if (c==IF || c==ELSE||c==FOR||c==RETURN||c==BREAK||c==DO||c==WHILE)
				{potential=0;break;}
				if (paren_depth<0 || brace_depth<0 || bracket_depth<0 || c==';'){
					{potential=0;break;} // we know its not a typeparam.
				}
				if ((c=='&'&&c1=='&')||(c=='|'||c1=='|'))break;//logic expressions win; no fancy TMP will parse
				if (c=='\"' || c=='\'') break;
				if (c=='<' && c1=='<') {s++; continue;}
				//if (c=='>' && c1=='>') {s++; continue;} todo.. if typaram_depth>1??
				if (c=='-' && c1=='>') {potential=0;break;}
				if (c=='>') {potential=1;break;}
				if (c=='/'&& c1=='/') break;
			}
			if (potential){
				curr_tok=OPEN_TYPARAM;
				typaram_depth++;
			}
		}
	}
	static int close_of(int tok){
		if (tok==OPEN_BRACE)return CLOSE_BRACE;
		if (tok==OPEN_BRACKET)return CLOSE_BRACKET;
		if (tok==OPEN_PAREN)return CLOSE_PAREN;
		return 0;
	}
	void advance_tok() {
		advance_tok_sub();
		typeparam_hack();
	}
	void advance_tok_sub() {
		skip_whitespace();
		tok_start=tok_end;
		pos.col=tok_start-line_start;
		if (!*tok_end) { this->curr_tok=0; return;}
		auto c=*tok_end;
		if (c=='_' && tok_end[1] && !isSymbolCont(tok_end[1])) tok_end++; //placeholder
		else if (isSymbolStart(c))	advance_sub(isSymbolCont);
		else if (isNumStart(c)) advance_sub(isNum);
		else if (c=='\"')
			advance_string('\"');
		else if (c=='\'')
			advance_string('\'');
		else advance_operator();
		//		else tok_end++;
		this->curr_tok = getStringIndex(tok_start,tok_end);
#ifdef WATCH_TOK
		for (auto i=10; i>0; i--){strcpy(watch_tok[i],watch_tok[i-1]);}
		memcpy(watch_tok[0],tok_start,tok_end-tok_start); watch_tok[0][tok_end-tok_start]=0;
#endif
		typeparam_hack();
	}
	Name eat_tok() {
		prev_start=tok_start;
		for (const char* c=tok_start; c!=tok_end;c++) {}
		auto r=curr_tok;
		advance_tok();
		if (r==OPEN_BRACE || r==OPEN_BRACKET || r==OPEN_PAREN) {
			bracket_pos[depth]=pos;ASSERT(depth<32);bracket[depth++]=(int)r;
		}
		if (r==CLOSE_BRACE || r==CLOSE_BRACKET || r==CLOSE_PAREN) {
			auto open=bracket[--depth];
			if (depth<0)
				::error(pos, "too many close brackets");
			auto close=close_of((int)open);
			if (close!=(int)r ) {
				::error(pos,"found %s expected %s",str(r),str(close));
				::error(bracket_pos[depth+1],"from here");
				::error_end(0);
			}
		}
#ifdef DEBUG2
		if (!strcmp(getString(r),"debugme")){
			dbprintf("found debug token\n");
		}
#endif
		
		return r;
	}
	Name eat_if(Name a, Name b, Name c){
		auto t=peek_tok(); if (t==a || t==b || t==c) return eat_tok();
		return 0;
	}
	Name eat_if(Name a, Name b){
		auto t=peek_tok(); if (t==a || t==b) return (int)t;
		return 0;
	}
	Name eat_if_not(Name i) {
		if (peek_tok()!=i) {return eat_tok();}
		else return Name(0);
	}
	bool eat_if(Name i) {
		if (peek_tok()==i) {eat_tok(); return true;}
		else return false;
	}
	bool is_placeholder()const {return  ((*tok_start=='_') && !isSymbolCont(*tok_end));}
	Name eat_if_placeholder(){if (is_placeholder()){advance_tok(); return PLACEHOLDER;} else return Name();}
	Name eat_ident() {
		auto r=eat_tok();
		if (r<IDENT) {
			::error(pos,"expected ident found %s",getString(r));error_end(0);}
		return r;
	}
	int eat_int() {
		auto nd=eat_number();
		return nd.num/nd.denom;
	}
	const char* eat_string() {
		auto len=(tok_end-tok_start)-2;
		ASSERT(len>=0);
		auto ret=(char*)malloc(len+1);
		memcpy((void*)ret,(void*)(tok_start+1),len+1);
		ret[len]=0;
		advance_tok();
		return ret;
	}
	NumDenom eat_number()  {
		int	val=0;
		int	frac=0;
		const char* p=tok_start;
		int sign=1; if (*p=='-') {sign=-1;p++;}
		int base=10;
		if ((tok_start+2)<=tok_end){
			if (tok_start[0]=='0' && tok_start[1]=='x'){
				base=16;
			}
		}
		for (;p<tok_end; p++) {
			char c=*p;
			if (*p=='.') { frac=1;}
			else {
				val*=base;
				frac*=base;
				val+=(c>='0'&&c<='9')?(c-'0'):(c>='a'&&c<='f')?(10+(c-'a')):0;
			}
		}
		if (frac==0) {frac=1;}
		advance_tok();
		return NumDenom{val,frac};
	}
	float eat_float() {
		auto nd=eat_number();
		return (float)nd.num/(float)nd.denom;
	}
	bool is_next_number() const {
		char c=*tok_start,c1=0;
		if (c) c1=tok_start[1];
		if ((c>='0' && c<='9')||(c=='-' && c1>='0' && c1<='9' && g_lisp_mode))
			return	true;
		else return false;
	}
	bool is_next_literal() const{
		char c=*tok_start;
		if (is_next_number() ||(c==':' && g_lisp_mode)|| is_next_string())
			return true;
		return false;
	}
	bool is_next_string() const {
		return *tok_start=='\"';
	}
	bool is_next_char() const {
		return *tok_start=='\'';
	}
	
	Name peek_tok(){
		return curr_tok;
	}
	void reverse(){
		ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;
	}
	Name expect(Name t, const char* err=""){
		decltype(t) x;if (!(t==(x=eat_tok()))) {error(0,"expected %s found %s;%s\n",str(t), str(x),err);} return x;
	}
	Name expect(Name a,Name b, const char* err=""){
		auto x=eat_tok();if (!(a==x || b==x)) {error(0,"expected %s or %s found %s;%s\n",str(a),str(b), str(x),err);} return x;
	}
};
typedef Lexer TokenStream;

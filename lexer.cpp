#include "lexer.h"
#include "error.h"
SrcPos g_srcpos;	// hack sorry,

bool isSymbolStart(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_';}
bool isSymbolCont(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || (c>='0' && c<='9');}
bool isNumStart(char c){return (c>='0'&&c<='9');};
bool isNum(char c){return (c>='0'&&c<='9')||(c>='a'&&c<='f')||(c=='e')||(c=='x') ||c=='.';};
bool isWhitespace(char c){return  c==' '||c=='\n'||c=='\a'||c=='\t';};
bool isOperator(char c){return c=='+'||c=='-'||c=='*'||c=='/'||c=='.'||c=='='||c=='>'||c=='<'||c=='&'||c=='|'||c=='~'||c=='%'||c=='^'||c=='+';}

int close_of(int open){
	if (open==LT) return GT;
	else if (open==OPEN_BRACKET) return CLOSE_BRACKET;
	else if (open==OPEN_BRACE) return CLOSE_BRACE;
	else if (open==OPEN_PAREN) return CLOSE_PAREN;
	else if (open==OR) return OR;
	error(g_srcpos,"not closeable %s",str(open));return 0;
}


void Lexer::error(const char* str,...){
	if(!error_newline){printf("\n");}
	printf("%s:%d:",filename,pos.line);
	char tmp[1024];
	va_list arglist;
	
	va_start( arglist, str );
	vsprintf(tmp, str, arglist );
	va_end( arglist );
	printf("\n"); error_newline=true;
}
Lexer::Lexer(const char* src,const char *filename_){
	strncpy(filename,filename_,512);
	g_filename=filename;
	
	line_start=src;
	pos.set(1,0);
	buffer=src;
	curr_tok=-1;
	tok_start=tok_end=buffer;
	advance_tok();
}

void Lexer::advance_sub(bool (*sub)(char c)){
	while ( *tok_end && sub(*tok_end)) tok_end++;
}
void Lexer::advance_operator() {
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
void Lexer::advance_string(char quote) {
	tok_start=tok_end;
	tok_end++;
	while (*tok_end && *tok_end!=quote) {
		if (*tok_end=='\\' && tok_end[1]) tok_end++; //skip backslashed quotes inside..
		tok_end++;
	}
	if (*tok_end)
		tok_end++; // step past last quote.
}
bool Lexer::is_comment(const char* c){
	if (*c && c[0]=='/'){if(c[1]=='/') return true;}
	if (*c && c[0]=='#'){if(c[1]=='!') return true;}
	return false;
}
void Lexer::skip_whitespace(){
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

void Lexer::typeparam_hack(){
	return;
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
int Lexer::close_of(int tok){
	if (tok==OPEN_BRACE)return CLOSE_BRACE;
	if (tok==OPEN_BRACKET)return CLOSE_BRACKET;
	if (tok==OPEN_PAREN)return CLOSE_PAREN;
//	if (tok==OPEN_TYPARAM)return CLOSE_PAREN;
	if (tok==LT)return GT;
	if (tok==OR)return OR;
	return 0;
}
void Lexer::advance_tok() {
	advance_tok_sub();
	typeparam_hack();
}
void Lexer::advance_tok_sub() {
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
Name Lexer::eat_tok() {
	g_srcpos=pos;
	prev_pos=pos;
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
			::error_begin(pos,"found %s expected %s",str(r),str(close));
			::error(bracket_pos[depth+1],"from here");
			::error_end(0);
		}
	}
	if (depth>0&&bracket[depth-1]==OR && r==OR){
		depth--;
	}
#ifdef DEBUG2
	if (!strcmp(getString(r),"debugme")){
		dbprintf("found debug token\n");
	}
#endif
	
	return r;
}
void Lexer::begin_lambda_bar_arglist() {
	/// needed to handle OR like bracket for lambda eg |x,y|; allows nesting error eg |x,) '|' expected.
	bracket[depth++]=OR;ASSERT(depth<32);
}

Name Lexer::Lexer::eat_if(Name a, Name b, Name c){
	auto t=peek_tok(); if (t==a || t==b || t==c) return eat_tok();
	return 0;
}
Name Lexer::eat_if(Name a, Name b){
	auto t=peek_tok(); if (t==a || t==b) return eat_tok();
	return 0;
}
Name Lexer::eat_if_not(Name i) {
	if (peek_tok()!=i) {return eat_tok();}
	else return Name(0);
}
bool Lexer::eat_if(Name i) {
	if (peek_tok()==i) {eat_tok(); return true;}
	else return false;
}
bool Lexer::is_placeholder()const {return  ((*tok_start=='_') && !isSymbolCont(*tok_end));}
Name Lexer::eat_if_placeholder(){if (is_placeholder()){advance_tok(); return PLACEHOLDER;} else return Name();}
Name Lexer::Lexer::eat_ident() {
	auto r=eat_tok();
	if (r<IDENT) {
		::error(pos,"expected ident found %s",getString(r));}
	return r;
}
int Lexer::eat_int() {
	auto nd=eat_number();
	return nd.num/nd.denom;
}
const char* Lexer::eat_string_alloc() {
	auto len=(tok_end-tok_start)-2;
	ASSERT(len>=0);
	auto ret=(char*)malloc(len+1);
	memcpy((void*)ret,(void*)(tok_start+1),len+1);
	ret[len]=0;
	advance_tok();
	return ret;
}
Name Lexer::eat_if_string(){
	if (tok_start[0]=='\"'){
		// range of the token includes quotes. we just need contents
		auto r= getStringIndex(tok_start,tok_end);
		eat_tok();
		return r;
	}
	return 0;
}
NumDenom Lexer::eat_number()  {
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
float Lexer::eat_float() {
	auto nd=eat_number();
	return (float)nd.num/(float)nd.denom;
}
bool Lexer::is_next_number() const {
	char c=*tok_start,c1=0;
	if (c) c1=tok_start[1];
	if ((c>='0' && c<='9')||(c=='-' && c1>='0' && c1<='9' && g_lisp_mode))
		return	true;
	else return false;
}
bool Lexer::is_next_literal() const{
	char c=*tok_start;
	if (is_next_number() ||(c==':' && g_lisp_mode)|| is_next_string())
		return true;
	return false;
}
bool Lexer::is_next_string() const {
	return *tok_start=='\"';
}
bool Lexer::is_next_char() const {
	return *tok_start=='\'';
}
bool Lexer::is_next(Name n1)const{
	return peek_tok()==n1;
}
Name Lexer::peek_tok()const{
	return curr_tok;
}
void Lexer::reverse(){
	ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;
}
Name Lexer::expect(Name t, const char* err){
	decltype(t) x;
	if (!(t==(x=eat_tok()))) {
		const char*ex =str(t);
		const char*found =str(x);
		::error(pos,"expected %s found %s; %s\n",ex, found,err);
	}
	return x;
}
Name Lexer::expect(Name a,Name b, const char* err){
	auto x=eat_tok();if (!(a==x || b==x)) {error(0,"expected %s or %s found %s;%s\n",str(a),str(b), str(x),err);} return x;
}


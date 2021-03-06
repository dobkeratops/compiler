#include "stringtable.h"
// todo, sizes in bits, not bytes

const char* operator_symbol(Name ok);
bool is_ident(Name tok){return tok>=IDENT;}
bool is_type(Name tok){return tok<T_NUM_TYPES;}
bool is_operator(Name tok){ return tok>=ARROW && tok<COMMA;}
bool is_range_operator(Name tok){ return tok>=RANGE_FIRST && tok<=RANGE_LAST;}
bool is_condition(Name tok){
	return (tok>=LT && tok<=LOG_OR);
}
bool is_comparison(Name tok){
	return (tok>=LT && tok<=NE);
}
bool is_callable(Name tok) { return (tok==FN || tok==CLOSURE);}
int operator_flags(Name tok){return g_tok_info[index(tok)];}
int precedence(Name ntok){auto tok=index(ntok);return tok<IDENT?(g_tok_info[tok] & PRECEDENCE):0;}
int is_prefix(Name ntok){auto tok=index(ntok);return tok<IDENT?(g_tok_info[tok] & (PREFIX) ):0;}
int arity(Name ntok){auto tok=index(ntok);return  (tok<IDENT)?((g_tok_info[tok] & (UNARY) )?1:2):-1;}
int is_right_assoc(Name ntok){auto tok=index(ntok);return (tok<IDENT)?(g_tok_info[tok]&ASSOC):0;}
int is_left_assoc(Name ntok){auto tok=index(ntok);return (tok<IDENT)?(!(g_tok_info[tok]&ASSOC)):0;}
const char* symbol_of(Name tok)
{if(auto p=operator_symbol(tok))return p;else return str(tok);}

const char* operator_symbol(Name tok){
	if (tok>=OPEN_PAREN && tok<IDENT)
		return  g_operator_symbol[(int)tok-OPEN_PAREN];
	return nullptr;
}

Name get_prefix_operator(Name tok) {
	auto itok=index(tok);
	if (itok>IDENT) return tok;
	switch (itok){
		case POST_INC: return Name(PRE_INC);
		case POST_DEC: return Name(PRE_DEC);
		case SUB: return Name(NEG);
		case MUL: return Name(DEREF);
		case AND:
			return Name(ADDR);
		case LOG_AND:
			return Name(RVALUE_REF);
		default: return tok;
	}
}
Name get_infix_operator(Name tok) {
	auto itok=index(tok);
	if (itok>IDENT) return tok;
	switch (itok){
		case PRE_INC: return Name(POST_INC);
		case PRE_DEC: return Name(POST_DEC);
		case NEG: return Name(SUB);
		case DEREF: return Name(MUL);
		case ADDR: return Name(AND);
		case RVALUE_REF: return Name(LOG_AND);
		default: return tok;
	}
}
StringTable::StringTable(const char** initial){
	verbose=false;
	nextId=0;
	//	for (int i=0; g_token_str[i];i++) {
	//		auto tmp=g_token_str[i];
	//		get_index(tmp,tmp+strlen(tmp));
	//	}
	nextId=NUM_STRINGS;
	index_to_name.resize(NUM_STRINGS);
	for (int i=0; i<index_to_name.size(); i++) {
		index_to_name[i]=std::string(g_token_str[i]);
		names.insert(std::make_pair(index_to_name[i],i));
	}
	ASSERT(nextId==NUM_STRINGS);
}
int StringTable::get_index(const char* str, const char* end,char flag) {
	if (!end) end=str+strlen(str);
	auto len=(end)?(end-str):strlen(str);
	string s; s.resize(len);memcpy((char*)s.c_str(),str,len);((char*)s.c_str())[len]=0;
	auto ret=names.find(s);
	if (ret!=names.end())	return ret->second;
	names.insert(std::make_pair(s,nextId));
	index_to_name.resize(nextId+1);
	index_to_name[nextId]=s;
	int r;
	if (1==sscanf(str,"%d",&r)) flag|=StringTable::Number;
	flags.resize(nextId+1); flags[nextId]=flag;

	if (verbose)
		dbprintf("insert[%d]%s\n",nextId,index_to_name[nextId].c_str());
	return	nextId++;
};

void StringTable::dump(){
	dbprintf("\n");
	for (int i=0; i<this->nextId; i++) {
		dbprintf("[%d]%s\n",i,this->index_to_name[i].c_str());
	}
};

StringTable g_Names(g_token_str);
Name getStringIndex(const char* str,const char* end) {
	return g_Names.get_index(str, end,0);
}
Name strConcat(Name n1, Name n2);
inline Name strConcat(Name n1, Name n2,Name n3){ return strConcat(n1,strConcat(n2,n3));}

Name getStringIndexConcat(Name base, const char* s2){
	char tmp[512];
	snprintf(tmp,511,"%s%s",str(base),s2);
	return getStringIndex(tmp);
}
Name strConcat(Name n1, Name n2){
	// todo - we could optimize the string table around concatenations
	return getStringIndexConcat(n1,str(n2));
}
const char* getString(const Name& n) {
	auto i=index(n);
	return i?g_Names.index_to_name[i].c_str():"";
}

Name getNumberIndex(int num){
	char tmp[32];sprintf(tmp,"%d",num); return g_Names.get_index(tmp,0,StringTable::Number);
}
Name getNumberIndex(float num){
	char tmp[32];sprintf(tmp,"%f",num); return g_Names.get_index(tmp,0,StringTable::Number);
}
bool is_number(Name n){
	return g_Names.flags[(int)n]&StringTable::Number;
}
int getNumberInt(Name n){
	int r;
	sscanf(str(n),"%d",&r);
	return r;
}
float getNumberFloat(Name n){
	float r;
	sscanf(str(n),"%f",&r);
	return r;
}

void find_completions(Name src,std::function<void(Name,int)> f){
	for (int n=0; n<g_Names.index_to_name.size(); n++) {
		const char* s1=str(src);
		const char* s2=str(n);
		const char* s0=s1;
		int match;
		for (;*s1 && *s2; s1++,s2++){
			if (*s1!=*s2)
				break;
		}
		if (*s1)// didn't find s1's..
			continue;
		f(Name(n),s1-s0);
	}
}



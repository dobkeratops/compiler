#include "stringtable.h"
int g_size_of[]={
	0,
	4,4,8,1,2,4,8,1,2,4,8,16,1,
	2,4,8,161,8,0,-1,-1,-1,8,8,8
};
int g_raw_types[]={
	4|RT_SIGNED|RT_INTEGER,
	4|RT_INTEGER,
	8|RT_INTEGER,
	1|RT_SIGNED|RT_INTEGER,
	2|RT_SIGNED|RT_INTEGER,
	4|RT_SIGNED|RT_INTEGER,
	8|RT_SIGNED|RT_INTEGER,
	1|RT_INTEGER,
	2|RT_INTEGER,
	4|RT_INTEGER,
	8|RT_INTEGER,
	16|RT_INTEGER,
	1|RT_INTEGER,
	2|RT_FLOATING,
	4|RT_FLOATING,
	8|RT_FLOATING,
	16|RT_FLOATING|RT_SIMD,
	1|RT_INTEGER,
	8|RT_POINTER,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0,
	0|0,
};
const char* g_token_str[]={
	"",
	"int","uint","size_t","i8","i16","i32","i64","u8","u16","u32","u64","u128","bool",
	"half","float","double","float4",
	"char","str","void","voidptr","one","zero","nullptr","true","false",
	"auto","ptr","ref","Self",
	"tuple","__NUMBER__","__TYPE__","__IDNAME__",
	
	"print___","fn","struct","class","trait","virtual","static","extern", "enum","array","vector","union","variant","with","match","where","sizeof","typeof","nameof","offsetof", "this","self","super","vtableof","closure",
	"let","var",
	"const","mut","volatile",
	"while","if","else","do","for","in","return","break","continue",
	"(",")",
	"{","}",
	"[","]",
	"<[","]>",
	"->",".","?.","=>","<-","::","<->",			//arrows,accessors
	"|>","<|","<.>","<$>","<:",":>",			// some operators we might nick from other langs
	
	":","as","new","delete",
	"+","-","*","/",					//arithmetic
	"&","|","^","%","<<",">>","?:","?>","?<",					//bitwise
	"<",">","<=",">=","==","!=",		//compares
	"&&","||",		//logical
	"=",":=","=:","@",
	"+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=", // assign-op
	".=",	// linklist follow ptr.=next
	"++","--","++","--", //inc/dec
	"-","*","&","!","~", // unary ops
	"*?","*!","&?","~[]","[]","&[]", // special pointers?
	",",";",";;",
	"...","..",
	"_","",
	"\"C\"","__vtable_ptr","__env_ptr","__env_i8_ptr",
	NULL,
};

int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,// int types
	0,0,0,0,  //floats
	0,0,0,0,0,0,0,0,0,
	0,0,0,0,
	0,0,0,0,			// type modifiers
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0, 0,0,0,0,0, // keywords
	0,0,			// let,var
	0,0,0,			// modifiers const,mut,volatile
	0,0,0,0,0,0,0,0,0,  // while,if,else,do,for,in,return,break
	0,0, //( )
	0,0, //{ }
	0,0, // [ ]
	0,0, //<[Type]>
	READ|10,READ|2,READ|2,READ|10,READ|10,READ|13,WRITE|10,	   // dots, arrows
	READ|17,READ|17,READ|17,READ|5,READ|17,READ|17,	// unusual stuff
	READ|9,READ|9, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,
	READ|6,READ|6,READ|5,READ|5,		//arithmetic
	READ|8,READ|7,READ|8,READ|6,READ|9,READ|9,READ|9,READ|9,READ|9,	//bitwise
	READ|8,READ|8,READ|8,READ|8,READ|9,READ|9,	// COMPARES
	READ|13,READ|14,	//logical
	WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,0, // assignment
	
	WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16, // assign-op
	WRITE_LHS|READ|ASSOC|16, // dot-assign
	MODIFY|PREFIX|UNARY|2,MODIFY|PREFIX|UNARY|2,MODIFY|UNARY|ASSOC|3,MODIFY|UNARY|ASSOC|3, // post/pre inc/dec
	READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3, //unary ops
	READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,READ|UNARY|ASSOC|3, /// special pointers
	0,0,17, // delim
	0,
	0,0,
	0, //placeholder
	0,0,0,0,0
};
bool is_ident(Name tok){return tok>=IDENT;}
bool is_type(Name tok){return tok<T_NUM_TYPES;}
bool is_operator(Name tok){ return tok>=ARROW && tok<COMMA;}
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

Name get_prefix_operator(Name tok) {
	auto itok=index(tok);
	if (itok>IDENT) return tok;
	switch (itok){
		case POST_INC: return Name(PRE_INC);
		case POST_DEC: return Name(PRE_DEC);
		case SUB: return Name(NEG);
		case MUL: return Name(DEREF);
		case AND: return Name(ADDR);
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

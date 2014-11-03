#include "hack.hpp"


//(sourcecode "hack.cpp")
//(normalize (lerp (obj:zaxis)(normalize(sub(target(obj:pos)))) //(settings:angle_rate)) (sloc 512 40 20))

void print_tok(int i){printf("%s ",getString(i));};

const char* g_token_str[]={
	"",
	"int","float","str","void","auto","one","zero","voidptr",
	"print","fn","struct","tuple","variant",
	"let","set","var",
	"while","if","else","do","for","in","return","break",
	"(",")",
	"{","}",
	"[","]",
	":=","->",
	":","=","+","-","*","/",".",
	"<",">","<=",">=","==","!=","&&","||",
	"&","|","^","%","<<",">>",
	"+=","-=","*=","/=","<<=",">>=","&=","|=",
	"++","--","++","--","-","*","&","!","~",
	",",";",
	NULL,	
};

#define PRECEDENCE 0xff
#define PREFIX 0x100
#define POSTFIX 0x200
#define FIXITY (PREFIX|POSTFIX)
#define ASSOC 0x400
int g_precedence[]={
	0,
	0,0,0,0,0,0,0,0,
	0,0,0,0,0,
	0,0,0,
	0,0,0,0,0,0,0,0,
	0,0,
	0,0,
	0,0,
	0,10,	   // assignment, lambda
	9,0,4,4,6,6,12,
	3,3,3,3,3,3,2,2,
	8,7,8,6,9,9,
	ASSOC|0,ASSOC|0,ASSOC|0,ASSOC|0,ASSOC|0,ASSOC|0,ASSOC|0,ASSOC|0,
	PREFIX|10,PREFIX|10,POSTFIX|11,POSTFIX|11,PREFIX|11,PREFIX|11,PREFIX|11,PREFIX|11,PREFIX|11,
	0,0,
};
int precedence(int tok){return g_precedence[tok] & PRECEDENCE;}
int fixity(int tok){return (g_precedence[tok] & (PREFIX|POSTFIX) );}
int operands(int tok){ if (!(g_precedence[tok]&(FIXITY))) return 2; else return 1;}



StringTable::StringTable(const char** initial){
	verbose=false;
	nextId=0;
//	for (int i=0; g_token_str[i];i++) {
//		auto tmp=g_token_str[i];
//		get_index(tmp,tmp+strlen(tmp));
//	}
	nextId=IDENT;
	index_to_name.resize(IDENT);
	for (int i=0; i<index_to_name.size(); i++) {
		index_to_name[i]=std::string(g_token_str[i]);
		names.insert(std::make_pair(index_to_name[i],i));
	}
	printf("%d %s\n",COMMA,g_token_str[COMMA]);
	printf("%d %s\n",SEMICOLON,g_token_str[SEMICOLON]);
	printf("%d",nextId);
	ASSERT(nextId==IDENT);
}
int StringTable::get_index(const char* str, const char* end) {
	int len=end-str;
	string s; s.resize(len);memcpy((char*)s.c_str(),str,len);((char*)s.c_str())[len]=0;
	auto ret=names.find(s);
	if (ret!=names.end())	return ret->second;		
	names.insert(std::make_pair(s,nextId));
	index_to_name.resize(nextId+1);
	index_to_name[nextId]=s;
//		std::cout<<s <<index_to_name[nextId];
	if (verbose)
		printf("insert[%d]%s\n",nextId,index_to_name[nextId].c_str());
	return	nextId++;
};

void StringTable::dump(){
	printf("\n");
	for (int i=0; i<this->nextId; i++) {
		printf("[%d]%s\n",i,this->index_to_name[i].c_str());
	}
};


StringTable g_Names(g_token_str);
int getStringIndex(const char* str,const char* end) {
	if (!end) end=str+strlen(str);
	return g_Names.get_index(str, end);
}
const char* getString(int index) {
	return g_Names.index_to_name[index].c_str();
}



void indent(int depth) {
	for (int i=0; i<depth; i++){printf("\t");};
}
void newline(int depth) {
	if (depth>=0) printf("\n"); indent(depth);
}
// Even a block is an evaluatable expression.
// it may contain outer level statements.

void Expr::dump(int depth) const {
	newline(depth);printf("(?)");
}

Expr::Expr(){ type=0;}

Type* ExprIdent::resolve(CallScope* scope) {
	if (auto p=scope->get_variable(this->name)){ return p->get_type();}
	else return this->type;
}

int ExprBlock::get_name()const{
	if (call_op) return call_op->get_name(); else return 0;
}
void ExprBlock::dump(int depth) const {
	newline(depth); if (this->call_op){print_tok(this->call_op->ident());printf("(");}else printf("{");
	for (const auto x:this->argls) {
		if (x) {x->dump(depth+1);}else{printf("(???)");}
	}
	newline(depth);if (this->call_op)printf(")");else printf("}");
//	newline(depth);
}

ExprBlock::ExprBlock(){call_target=0;}


const char* CallScope::name() const {
	if (!parent){
		return"global";
	} 
	else return getString(outer->name);
}


void Type::push_back(Type* t) {	
	if (!sub) sub=t;
	else {
		for (auto p=sub; sub->next; sub=sub->next){};
		sub->next =t;
	}
}
const char* Type::get_name_str() const{
	if (!this) return "(no_type)";
	return getString(this->type_id);
}
const char* Type::kind_str()const{return"type";}
Type::Type(Name i){ 
	struct_def=0;
	sub=0;
	next=0;
	type_id=i; //todo: resolve-type should happen here.
}
	//todo: generic heirarchy equality test, duplicate code detection?
bool Type::eq(const Type* other) const{
	if ((!this) && (!other)) return true;
	if (!(this && other)) return false;
	if (this->type_id!=other->type_id)return false;
	return true;
//	if (!this->sub && other->sub)) return true;
	auto p=this->sub,o=other->sub;
		
	for (; p && o; p=p->next,o=o->next) {
		if (!p->eq(o)) return false;
	}
	if (o || p) return false; // didnt reach both..
	return true;
}
void Type::dump_sub()const{
	if (!this) return;
	printf("%s",getString(type_id));
	if (sub){
		printf("<");
		for (auto t=sub; t; t=t->next){t->dump_sub(); if(t->next)printf(",");}; 
		printf(">");
	}
}
void Type::dump(int depth)const{
	if (!this) return;
	newline(depth);dump_sub();
}


void ExprLiteral::dump(int depth) const{
	newline(depth);
	if (type_id==T_VOID){printf("void");}
	if (type_id==T_INT){printf("%d",u.val_int);}
	if (type_id==T_FLOAT){printf("%.7f",u.val_float);}
	if (type_id==T_CONST_STRING){printf("%s",u.val_str);}
}
// TODO : 'type==type' in our type-engine
//	then we can just make function expressions for types.

Type* ExprLiteral::resolve(CallScope* ){
	if (this->type) return this->type;
	Type* t=nullptr;
	switch (type_id) {
	case T_VOID: t=new Type(VOID); break;
	case T_INT: t=new Type(INT); break;
	case T_FLOAT: t=new Type(FLOAT); break;
	case T_CONST_STRING: t=new Type(STR); break;
	default: break;
	}
	this->type=t;
	return this->type;
}

ExprLiteral::ExprLiteral(float f) {
	type=nullptr;
	type_id=T_FLOAT;
	u.val_float=f;
	printf("lit float %.3f",f);
}
ExprLiteral::ExprLiteral(int i) {
	type=nullptr;
	type_id=T_INT;
	u.val_int=i;
}
ExprLiteral::ExprLiteral(const char* start,int length) {
	type=nullptr;
	type_id=T_CONST_STRING;
	auto str=( char*)malloc(length+1); ;
	u.val_str=str;memcpy(str,(void*)start,length);
	str[length]=0;
}
ExprLiteral::~ExprLiteral(){
	if (type_id==T_CONST_STRING) {
		free((void*)u.val_str);
	}
}


void ArgDef::dump(int depth) const {
	newline(depth);printf("%s",getString(name));
	if (type) {printf(":");type->dump(-1);}
	if (default_value) default_value->dump(-1);
}
const char* ArgDef::kind_str()const{return"arg_def";}

// the operators should all just be functioncalls, really.
// return type of function definition is of course a functoin object.
// if we make these things inline, we create Lambdas
// todo: receivers.

bool ExprFnDef::is_generic() const {
	for (auto i=0; i<args.size(); i++)
		if (!args[i]->type) return true;
	return false;
}
void ExprFnDef::dump(int ind) const {
	newline(ind);printf("fn %s(",getString(name));
	for (int i=0; i<args.size();i++){
		args[i]->dump(-1);
		printf(",");
	}
	printf(")\t{");
	this->body->dump(ind+1);
	newline(ind);printf("}");
}

FnName*	g_fn_names;
vector<FnName> g_functions;
FnName* getFnName(int name) {
	FnName*	n;
	for (n=g_fn_names; n; n=n->next) {
		if (n->name==name) return n;
	}
	n=new FnName(); n->name=name; n->next=g_fn_names; g_fn_names=n;
	return n;
}

// get a function's list of shared names..
FnName* ExprFnDef::get_fn_name() {
	if (this->fn_name)	return	this->fn_name;
	auto fnm=getFnName(this->name);
	this->fn_name = fnm;
	this->next_of_name=fnm->fn_defs; fnm->fn_defs = this;
	return	fnm;
}
/*
void CallScope::visit_calls() {
	for (auto call=this->calls;call;call=call->next_of_scope) {
		printf("%s --> %s\n",this->name(), getString(call->callee->name));
	}
	for (auto sub=this->child; sub; sub=sub->next)
		sub->visit_calls();
}
*/
ExprFnDef* instantiate_generic_function(CallScope* s,ExprFnDef* src, Expr** call_args, int num_args) {
	ExprFnDef* f = new ExprFnDef();
	f->name = src->name;
	if (src->fn_name) {
		f->fn_name = src->fn_name;
		f->next_of_name = src->fn_name->fn_defs;
		src->fn_name->fn_defs = f;
	}
	f->next_instance = src->instances;
	src->instances=f;
	f->resolved=false;
	for (auto i=0; i<src->args.size(); i++) {
		f->args.push_back(new ArgDef(src->args[i]->name, call_args[i]->type));
	}
	//f->body = src->body->clone();
	printf("todo - clone fn body");
	return f;	// welcome new function!
}


void find_printf(const char*,...){};
void find_sub(Expr* src,Name name,Expr** args, int num_args, ExprFnDef** best_fn, int* best_score,int* ambiguity) {
	if (auto sb=dynamic_cast<ExprBlock*>(src)) {
		find_printf("look for %s in %s\n",getString(name),src->get_name_str());
		for (auto x:sb->argls) {
			find_sub(x,name,args,num_args, best_fn,best_score,ambiguity);
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(src)){
		if (f->name!=name)
			return ;

		// Find max number of matching arguments
		if (num_args >f->args.size())	// if not enough args, dont even try.
			return;
		if (num_args !=f->args.size())	// TODO: bring default arguments into the picture.
			return;

		find_printf("candidate:");
		for (int i=0; i<num_args; i++) {
			find_printf("%s ",f->args[i]->type->get_name_str());		 }
		find_printf("\n");
		int score=0;
		for (int i=0; i<num_args; i++) {
			if (f->args[i]->type->eq(args[i]->type)) {
				find_printf("match %s %s\n",f->args[i]->type->get_name_str(), args[i]->type->get_name_str());
				score++;
			}
		}
		// for any argument not matched, zero the score if its the *wrong* argument?
		// TODO: this is where we'd bring conversion operators into play.
		for (int i=0; i<num_args; i++) {
			if ((!f->args[i]->type->eq(args[i]->type)) && f->args[i]->type!=0) score=-1;
		}
		find_printf("score is %d\n",score);
		if (score >*best_score) {
			*best_score=score;
			*best_fn=f;
			ambiguity=0;
		} else if (score==*best_score) ambiguity++;
	}
	return ;
}
ExprFnDef*	CallScope::find_fn(Name name, Expr** args, int num_args)  {
	find_printf("\nfind call with args(");
	for (int i=0; i<num_args; i++) {find_printf(" %d:",i);args[i]->type->dump(-1);}
	find_printf(")\n");

	ExprFnDef*	best=0;
	int best_score=-1;
	int	ambiguity=0;
	for (auto src=this; src; src=src->parent) {
		find_sub(this->node,name, args,num_args,&best,&best_score,&ambiguity);
	}
	find_printf("match score=%d/%d\n", best_score, num_args);
	if (!best)  {
		printf("No match found\n");
	}
	if (ambiguity){
		printf("ambiguous matches for function\n");
	}
	if (best->is_generic()) {
		printf("matched generic function: instanting\n");
		return instantiate_generic_function(this, best, args, num_args);
	}
	
	return best;
}
Variable* CallScope::get_variable(Name name){
	// todo: This Pointer?
	for (auto v=this->vars; v; v=v->next) {
		if (v->name==name) return v;
	}
	if (this->parent){
		if (auto p=this->parent->get_variable(name)) 
			return	p;
	}
	if (this->global && this->global!=this){
		if (auto p=this->global->get_variable(name)) 
			return	p;
	}
	return nullptr;
}


Variable* CallScope::create_variable(Name name,Type* t){

	if (auto var=this->get_variable(name)) {
		if (var->type) {ASSERT(var->type->eq(t));}
		else var->type = t;

		return var;
	}
	// create a variable;
//	printf("create var %s in %p %p\n",getString(name), this ,this->parent);
	auto v=new Variable(); v->next=this->vars; this->vars=v; v->name=name; v->type=t;
	return v;
}
void CallScope::dump(int depth)const {
	newline(depth);printf("scope: %s {", this->outer?getString(this->outer->ident()):"global");
	for (auto v=this->vars; v; v=v->next) {
		newline(depth+1); printf("var %d %s:",v->name, getString(v->name)); 
		if (v->type){ v->type->dump(-1);} else {printf("not_type");}
	}
	for (auto s=this->child; s; s=s->next){
		s->dump(depth+1);
	}
	newline(depth);printf("}");
}
template<typename T>
void dump(vector<T*>& src) {
	for (int i=0; i<src.size(); i++) {
		printf(src[i]->dump());
	}
}
void print_x(){ int x; x=0;}
Type* ExprBlock::resolve(CallScope* scope) {

	printf("\nresolve block.. %p\n", scope);
	if (this->argls.size()<=0 && !this->call_op) {
		if (!this->type) this->type=new Type();this->type->type_id=VOID;//void..
		return nullptr;
	}
	int op_ident=NONE;
	ExprIdent* p=nullptr;
	if(this->call_op){p=dynamic_cast<ExprIdent*>(this->call_op); op_ident=p->name;}
	if (op_ident==NONE) {	// do executes each expr, returns last ..
		Type* ret=0;
		for (auto i=0; i<this->argls.size(); i++) {
			ret=this->argls[i]->resolve(scope);
		}
		return ret;
	} 
	else 
	if (op_ident==SET) {
		ASSERT(this->argls.size()>=2);
		auto vname=this->argls[0]->ident();
//			printf("set: try to create %d %s %s\n", vname, getString(vname), this->args[1]->kind_str());
		this->argls[0]->dump(0);
	printf("resolve block getvar %p\n", scope);
		Type* t=this->argls[1]->resolve(scope);
		auto var= scope->create_variable(vname, t);
			// If the variable exists - assignments must match it...
			// todo: 2way inference.
			// we'd report missing types - then do a pass propogating the opposite direction
			// and bounce.
		this->type = t;
		return var->type;
	}
	else {
		printf("resolve call..%s\n",getString(p->ident()));
			// TODO: distinguish 'partially resolved' from fully-resolved.
		if (!this->call_target) {
				// Todo: Process Local Vars.
				// TODO: accumulate types of arguments,
				// then use them in find_fn to resolve overloading
				// find_fn should also perform Template Instantiations.
			return resolve_make_fn_call(this, scope);
		} else 
			return this->call_target->type;
	}
	return nullptr;
}

Type* resolve_make_fn_call(ExprBlock* block,CallScope* scope) {
	for (int i=0; i<block->argls.size(); i++) {
		block->argls[i]->resolve(scope);
		printf("arg %d type=",i); cout<<block->argls[i]->type<<"\n";//->dump(0); printf("\n");
	}
	ExprFnDef* call_target = scope->find_fn(block->call_op->ident(), &block->argls[0],block->argls.size());
	auto fnc=call_target;
	if (call_target) {
		if (call_target->resolved) {
			return block->type=call_target->type;
		}
			// add this to the targets' list of calls.
		CallScope* subscope=new CallScope;

		// create a local for each supplied argument, using its type..
		for (int i=0; i<block->argls.size() && i<fnc->args.size(); i++) {
			subscope->create_variable(fnc->args[i]->name, block->argls[i]->type);
		}
		
		// TODO: insert expressions for the default arguments
		
		scope->push_child(subscope);
		subscope->outer=call_target;
		block->next_of_call_target = call_target->callers;
		call_target->callers =block;
		block->call_target=call_target;
		block->type = call_target->resolve_call(subscope);
		call_target->resolved=true;
		return block->type;
	}
	else 
		return nullptr;
}


Type* ExprFnDef::resolve_call(CallScope* scope) {
//	auto scope=new CallScope;		
//	scope->parent=parent; scope->next=parent->child; parent->child=scope;
//	scope->outer=this;
//	scope->node=this->body;
	printf("resolve fn call.. %p\n", scope);
	auto rt=this->body->resolve(scope);
	printf("resolve %s yields type:", getString(this->ident()));if (rt)rt->dump(-1);printf("\n");
	if (this->type) {
		ASSERT(rt==this->type);	
		this->resolved=true;
	}
	this->type=rt;
	return rt;
}
Type* ExprFnDef::resolve(CallScope* scope) {
// todo: makes a closure taking locals from parent scope
	return new Type(FN);
}

void call_graph(Node* root,CallScope* scope) {
}

struct Bar { string name="fo"; int x=0;};
struct Foo {
	Bar bar;
	vector<int> indices;
	void dump() {cout<<indices;};
};

template<typename T>
T& operator<<(T& dst, const Bar& src) { dst<<src.name<<src.x;return dst;};
template<typename T>
T& operator<<(T& dst, const Foo& src) { dst<<src.bar; dst<<src.indices;return dst;};

bool isSymbolStart(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_';}
bool isSymbol(char c) { return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' || (c>='0' && c<='9');}
bool isNumStart(char c){return (c>='0'&&c<='9');};
bool isNum(char c){return (c>='0'&&c<='9') ||c=='.';};
bool isWhitespace(char c){return  c==' '||c=='\n'||c=='\a'||c=='\t';};
bool isOperator(char c){return c=='+'||c=='-'||c=='*'||c=='/'||c=='.'||c=='='||c=='>'||c=='<'||c=='&'||c=='|'||c=='~'||c=='%'||c=='^'||c=='+';}

struct NumDenom{int num; int denom;};

struct TextInput {
	const char* buffer,*tok_start,*tok_end,*prev_start;
	int curr_tok;

	TextInput(const char* src){
		buffer=src;
		curr_tok=-1;
		tok_start=tok_end=buffer;
		advance_tok();
	}

	void advance_sub(bool (*sub)(char c)){while ( *tok_end && sub(*tok_end)) tok_end++;}

	void advance_tok() {
		auto pos=tok_start;
		while (isWhitespace(*tok_end)&&*tok_end) tok_end++;
		tok_start=tok_end;
		if (!*tok_end) { this->curr_tok=0; return;}
		auto c=*tok_end;
		if (isSymbolStart(c))	advance_sub(isSymbol);
		else if (isNumStart(c)) advance_sub(isNum);
		else tok_end++;
		this->curr_tok = getStringIndex(tok_start,tok_end);
	}
	int eat_tok() {
		prev_start=tok_start;
		char tok[512]; for (const char* c=tok_start; c!=tok_end;c++) {}
		int r=curr_tok;
		advance_tok();
		return r;
	}
	bool eat_if(int i) {	
		if (peek_tok()==i) {eat_tok(); return true;}
		else return false;
	}
	int eat_ident() {
		auto r=eat_tok();
		if (r<IDENT) {printf("expected ident found %s",getString(r));exit(0);}
		return r;
	}
	int eat_int() {
		auto nd=eat_number();
		return nd.num/nd.denom;
	}
	NumDenom eat_number()  {
		int	val=0;
		int	frac=0;
		for (const char* p=tok_start;p<tok_end; p++) {
			val*=10; 
			frac*=10;
			if (*p=='.') { frac=1;}
			else
				val+=*p-'0';
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
		if (is_next_number() ||(c==':' && g_lisp_mode)||(c=='\"'))
			return true;
		return false;
	}

	int peek_tok(){return curr_tok;}
	void reverse(){ ASSERT(tok_start!=prev_start);tok_end=tok_start;tok_start=prev_start;}
	int expect(int t){ int x;if (t!=(x=eat_tok())) {printf("expected %s found %s",getString(t), getString(x));exit(0);} return x;}
};
void unexpected(int t){printf("unexpected %s\n",getString(t));exit(0);}
typedef TextInput TokenStream;

Expr* parse_lisp(TokenStream& src);
ExprFnDef* parse_fn(TokenStream&src);

template<typename T>
T pop(std::vector<T>& v){ ASSERT(v.size()>0);auto r=v[v.size()-1];/*move?*/ v.pop_back(); return r;}
//#define pop(X) ASSERT(X.size()>0); pop_sub(X);

void flush_op_stack(ExprBlock* block, vector<Expr*>& ops,vector<Expr*>& vals) {
	while (ops.size()>0) {
		ExprBlock* op=new ExprBlock;
		op->call_op=pop(ops);
		if (vals.size()>1) {
			auto arg1=pop(vals);
			op->argls.push_back(pop(vals));
			op->argls.push_back(arg1);
		} else if (vals.size()>0) {
			op->argls.push_back(pop(vals));
		}
		vals.push_back(op);
//		block->argls.push_back(op);
	}
	while (vals.size()) {
		block->argls.push_back(pop(vals));
	}
}

ExprBlock* parse_call(TokenStream&src,int delim,int close, Expr* op) {
	// shunting yard parserfelchery
	ExprBlock *node=new ExprBlock; node->call_op=op;
	vector<Expr*> operators;
	vector<Expr*> operands;
	bool	was_operand=false;
	int wrong_delim=delim==SEMICOLON?COMMA:SEMICOLON;

	while (true) {
		if (src.eat_if(close) || !src.peek_tok())
			break;
		printf("parse:- %d %d\n",operands.size(),operators.size());
		print_tok(src.peek_tok());
		Expr* sub=nullptr;
		if (src.is_next_literal()) {
			if (src.is_next_number()) {
				auto n=src.eat_number();
				ExprLiteral* ln=0;
				if (n.denom==1) {ln=new ExprLiteral(n.num);}
				else {ln=new ExprLiteral((float)n.num/(float)n.denom);}
				operands.push_back(ln);		
				was_operand=true;
				continue;
			}
		}
		if (src.eat_if(FN)) {
			ASSERT(!was_operand);
			operands.push_back(parse_fn(src)); was_operand=true;
			
		}
		else if (src.eat_if(OPEN_PAREN)) {
			if (was_operand){
				operands.push_back(parse_call(src, CLOSE_PAREN,SEMICOLON, pop(operands)));
				// call result is operand
			}
			else {operands.push_back(parse_call(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(delim)) {
				// dump all..
			//ASSERT(was_operand==true);
			flush_op_stack(node,operators,operands);
			was_operand=false;
		}else if (src.eat_if(wrong_delim)){
			printf("error expected %s not %s", getString(delim),getString(wrong_delim));
			flush_op_stack(node,operators,operands);// keep going
			was_operand=false;
		} else{
			auto tok=src.eat_tok();
			if (was_operand) {
				while (operators.size()>0) {
					int prev_precedence=precedence(operators.back()->ident());
					int prec=precedence(tok);
					if (!(prev_precedence>=prec || prec<0) ) // todo associativity
						break;
					auto * p=new ExprBlock();
					p->call_op=pop(operators);
					if (operands.size()>=2){
						auto arg1=pop(operands);
						p->argls.push_back(pop(operands));
						p->argls.push_back(arg1);
					} else if (operands.size()>=1){
						p->argls.push_back(pop(operands));
					} else{
						printf("error operator, no operands\n");
					}
					operands.push_back(p);
					was_operand=true;
				} //else {
					//operators.push_back(new ExprIdent(tok));
					//was_operand=false;
					//}
				operators.push_back(new ExprIdent(tok));
				was_operand=false;
			} else {// last was operator, we got an operand
				operands.push_back(new ExprIdent(tok));
				was_operand=true;
			}
		}
		//ASSERT(sub);
		//node->argls.push_back(sub);
	};
	flush_op_stack(node,operators,operands);
	return node;
}

Type* parse_type(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	Type* ret=0;	// read the first, its the form..
	if (tok==close) return nullptr;
	ret = new Type(tok);
	if (src.eat_if(OPEN_BRACKET)) {
		while (auto sub=parse_type(src, CLOSE_BRACKET))
			ret->push_back(sub);
	}
	printf("3\n");
	ret->dump(-1); 
	return ret;
	
}
ArgDef* parse_arg(TokenStream& src, int close) {
	auto argname=src.eat_ident();
	if (argname==close) return nullptr;
	if (src.eat_if(COLON)) {
		return new ArgDef(argname,parse_type(src,CLOSE_PAREN));
	} 
	else return new ArgDef(argname,nullptr);
}
ExprFnDef* parse_fn(TokenStream&src) {
	auto *fndef=new ExprFnDef();
	printf("parse_fn");
	// read function name or blank
	auto tok=src.eat_tok(); 
	print_tok(tok);
	if (tok!=OPEN_PAREN) {
		fndef->name=tok;
		tok=src.expect(OPEN_PAREN);
	} else fndef->name=NONE;
	printf("%s",getString(fndef->name));
	// read function arguments
	while (NONE!=(tok=src.peek_tok())) {
		if (tok==CLOSE_PAREN) {src.eat_tok();break;}
		printf(" arg:%s ",getString(tok));
		fndef->args.push_back(parse_arg(src,CLOSE_PAREN));
	}
	printf("fn body:");
	// implicit "progn" here..
	src.expect(OPEN_BRACE);
	fndef->body = new ExprBlock;
	fndef->body = parse_call(src, CLOSE_BRACE, SEMICOLON, nullptr);

	return fndef;
}
// every file is an implicitly a function aswell taking no args
// when imported, a module inserts a call to that function.
// that sets up global stuff for it.

const char* g_TestProg=
/*
"(fn lerp((a float) (b float) (f float))(+(*(- b a)f)a))"
"(fn lerp(a b f)(mad a (sub b a) f)
"(fn lerp((a int) (b int) (f float))(+(*(- b a)f)a))"
"(fn bilerp(a b c d u v)(lerp(lerp a b )(lerp c d)v))"
"(fn invlerp(a b x)(/(- x a)(- b a)))"
"(fn madd(a b f)(+a (* b f)))"
"(fn min(a b)(if (< a b)a b))"
"(fn max(a b)(if (> a b)a b))"
"(fn addto(a b)(set a(add a b)))
"(fn madto(a b f)(set a(add a (mul b f))))
"(fn interleave((a int)(c (map string int))(b (vector float))) (return)) "
"(fn clamp(a b f)(min b (max a f)))"
*/

	"future.pos=self.pos+self.vel*dt;"
	"x=y=z=3; x+y+z=0;"
//	"fn they_float(){set(tfl, 1.0); they_int()};"
//	"foo=bar(x*3,y+(self.pos+other.pos)*0.5);"
/*
"set(glob_x, 10.0);"
"fn they_int(){set(tfx, 1); tfx};"
"fn generic_fn(x y){y};"
"fn generic_fn(x y:float){y};"
	"fn generic_fn(x y:int){y};"
	"fn generic_fn(x:float y:float){ y};"
	"set(glob_y ,1);"
	"set(glob_p, generic_fn(0.0, 1.3));"
	"set(glob_q, generic_fn(0.0, 1));"
*/
	;
// when this works - it can be like SPECS
int main(int argc, const char** argv) {

	TextInput	src(g_TestProg);
	auto node=parse_call(src,0, SEMICOLON, nullptr);
//	while (auto ix=src.eat_tok()){}
//	printf("%d%s",getStringIndex("foo"),getString(IDENT+5));
//	g_Names.dump();
//	printf(getString(ADD_ASSIGN));
	printf("%p\n",node);
	node->dump(0);
	CallScope global; global.node=(ExprBlock*)node; global.global=&global;
	node->resolve(&global);
//	global.visit_calls();
	global.dump(0);

}



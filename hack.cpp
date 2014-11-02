#include "hack.hpp"



//(sourcecode "hack.cpp")
//(normalize (lerp (obj:zaxis)(normalize(sub(target(obj:pos)))) //(settings:angle_rate)) (sloc 512 40 20))


const char* g_token_str[]={
	"",
	"int","float","str","void","auto","one","zero","voidptr",
	"print","fn","struct","tuple","variant","let","set","var","while","if","else","do","for","in","return","break",
	"(",")",
	"{","}",
	"[","]",
	":=","->",
	":","=","+","-","*","/",".",
	"<",">","<=",">=","==","!=","&&","||",
	"&","|","^","%","<<",">>",
	"+=","-=","*=","/=","<<=",">>=","&=","|=",
	"++","--",
	",",";",
	NULL,	
};

int g_precedence[]={
    0, 1,2,2,3,3,4
};

StringTable::StringTable(const char** initial){
	verbose=false;
	nextId=0;
	for (int i=0; g_token_str[i];i++) {
		auto tmp=g_token_str[i];
		get_index(tmp,tmp+strlen(tmp));
	}
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
// Even a block is an evaluatable expression.
// it may contain outer level statements.

void Expr::dump(int depth) const {
	indent(depth);printf("?\n");
}

Expr::Expr(){ type=0;}

Type* ExprIdent::resolve(CallScope* scope) {
	if (auto p=scope->get_variable(this->name)){ return p->get_type();}
	else return this->type;
}

int ExprBlock::get_name()const{
	if (args.size()>0) return args[0]->get_name();
	else return 0;	
}
void ExprBlock::dump(int depth) const {
	indent(depth);printf("(\n");
	for (const auto x:args) { x->dump(depth+1);}
	indent(depth);printf(")\n");
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
const char* Type::kind_str()const{return"type";}
Type::Type(Name i){ 
	struct_def=0;
	sub=0;
	next=0;
	type_id=i; //todo: resolve-type should happen here.
}
	//todo: generic heirarchy equality test, duplicate code detection?
bool Type::eq(const Type* other) const{
	if (!(this && other)) return true;
	if (!this || !other) return false;
	if (this->type_id!=other->type_id)return false;
	if (!this->sub && other->sub) return true;
	auto p=this->sub,o=other->sub;
	for (; p && o; p=p->next,o=o->next) { 
		if (p->type_id!=o->type_id) {
			return false;
		}
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
	indent(depth);dump_sub();
	if (depth>=0) printf("\n");
}


void ExprLiteral::dump(int depth) const{
	indent(depth);
	if (type_id==T_VOID){printf("void");}
	if (type_id==T_INT){printf("%d",u.val_int);}
	if (type_id==T_FLOAT){printf("%.7f",u.val_float);}
	if (type_id==T_CONST_STRING){printf("%s",u.val_str);}
	if (depth>0)printf("\n");
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
	indent(depth);printf("%s ",getString(name));
	if (type) type->dump(-1);
	if (default_value) default_value->dump(-1);
	if (depth>0) printf("\n");
}
const char* ArgDef::kind_str()const{return"arg_def";}

// the operators should all just be functioncalls, really.
// return type of function definition is of course a functoin object.
// if we make these things inline, we create Lambdas
// todo: receivers.

void ExprFnDef::dump(int ind) const {
	indent(ind);printf("(fn %s(",getString(name));
	for (int i=0; i<args.size();i++){
		args[i]->dump(-1);
		printf(", ");
	}
	printf(")\n");
	this->body->dump(ind+1);
	indent(ind);printf(")\n");
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
void find_printf(const char*,...){};
ExprFnDef* find_sub(Expr* n,Name name) {
	if (auto sb=dynamic_cast<ExprBlock*>(n)) {
		find_printf("look for %s in %s",getString(name),n->get_name_str());
		for (auto x:sb->args) {
			if (auto p=find_sub(x,name))
				return p;
		}
	} else if (auto f=dynamic_cast<ExprFnDef*>(n)){
		if (f->name==name)
			return f;
	}
	return nullptr;
}
ExprFnDef*	CallScope::find_fn(Name name) const {
	if (auto p= find_sub(this->node,name)) return p;
	else if (this->parent) return parent->find_fn(name);
	else return nullptr;
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

Variable* CallScope::create_variable(Name name){
	if (auto p=this->get_variable(name)) return p;
	// create a variable;
//	printf("create var %s in %p %p\n",getString(name), this ,this->parent);
	auto v=new Variable(); v->next=this->vars; this->vars=v; v->name=name; v->type=0;
	return v;
}
void CallScope::dump(int depth)const {
	indent(depth);printf("scope: %s {\n", this->outer?getString(this->outer->ident()):"global");
	for (auto v=this->vars; v; v=v->next) {
		indent(depth); printf("var %d %s:",v->name, getString(v->name)); 
		if (v->type){ v->type->dump(-1);} else {printf("not_type");}
		if (depth>=0) printf("\n");
	}
	for (auto s=this->child; s; s=s->next){
		s->dump(depth+1);
	}
	indent(depth);printf("}\n");
}

Type* ExprBlock::resolve(CallScope* scope) {
	printf("resolve block.. %p\n", scope);
	if (this->args.size()<=0) {
		if (!this->type) this->type=new Type();this->type->type_id=VOID;//void..
		return nullptr;
	}
	auto p=dynamic_cast<ExprIdent*>(this->args[0]);
	if (p){
		auto ident=p->name;
	// TODO: Use typeparams - first compute types of arguments.
// special forms:-
		if (ident==DO) {	// do executes each expr, returns last ..
			Type* ret=0;
			for (auto i=1; i<this->args.size(); i++) {
				ret=this->args[i]->resolve(scope);
			}
			return ret;
		} 
		else if (ident==SET) {
			ASSERT(this->args.size()>=3);
			auto vname=this->args[1]->ident();
//			printf("set: try to create %d %s %s\n", vname, getString(vname), this->args[1]->kind_str());
			this->args[1]->dump(0);
	printf("resolve block getvar %p\n", scope);
			auto var= scope->create_variable(vname);
			Type* t=this->args[2]->resolve(scope);
			// If the variable exists - assignments must match it...
			if (var->type) ASSERT(var->type->eq(t));
			else var->type = t;
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
				for (int i=1; i<this->args.size(); i++) {
					this->args[i]->resolve(scope);
				}

				ExprFnDef* call_target = scope->find_fn(p->ident());
				if (call_target) {
					if (!call_target->resolved) {
				// add this to the targets' list of calls.
						CallScope* subscope=new CallScope;
						scope->push_child(subscope);
						subscope->outer=call_target;
						printf("%p %p\n", scope,subscope);

						this->next_of_call_target = call_target->callers;
						call_target->callers =this;
						this->call_target=call_target;
						this->type = call_target->resolve_call(subscope);
						call_target->resolved=true;
						return this->type;
					} else return call_target->type; 
				} else {
					
					return nullptr;
				}
			} else 
				return this->call_target->type;
		}
	}
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
	int eat_ident() {
		auto r=eat_tok();
		ASSERT(r>=IDENT);
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
};
typedef TextInput TokenStream;

Expr* parse_lisp(TokenStream& src);
ExprFnDef* parse_fn(TokenStream&src);

Expr* parse_expr(TokenStream&src,int close) {
	printf("\nparse_expr\n");
	ExprBlock *node=0;
	
	while (true) {
		Expr* sub=nullptr;
		if (src.is_next_literal()) {
			if (src.is_next_number()) {
				auto n=src.eat_number();
				ExprLiteral* ln=0;
				if (n.denom==1) {ln=new ExprLiteral(n.num);}
				else {ln=new ExprLiteral((float)n.num/(float)n.denom);}
				node->args.push_back(ln);		
			}
			continue;
		}
		auto tok=src.eat_tok();
		if (!tok || tok==close)
			break;
//		std::cout<<tok<<getString(tok)<<"\n";
		if (tok==FN) return parse_fn(src);		
		if (!node) {
			node =new ExprBlock;
			if (close==0) { node->args.push_back(new ExprIdent(DO));}
		}
		if (tok==OPEN_PAREN) {
			sub=parse_expr(src,CLOSE_PAREN);
		} else  {
			sub=new ExprIdent(tok);
		}
		if (sub)
			node->args.push_back(sub);
	};
	return node;
}
Type* parse_type(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	Type* ret=0;	// read the first, its the form..
	if (tok==close) return nullptr;

	if (tok!=OPEN_PAREN) {
		while (src.eat_tok()!=close){};
		return new Type(tok);
	}
	else{
		tok=src.eat_tok();
		if (tok!=close) {
			ret = new Type(tok);
			while (0!=(tok=src.eat_tok())) {
				if (tok==close) break;
				printf("1\n");
				ret->push_back(new Type(tok));
				printf("2\n");
			}
		}
	}
	while (src.eat_tok()!=close){};
	printf("3\n");
	ret->dump(-1); 
	return ret;
	
}
ArgDef* parse_arg(TokenStream& src, int close) {
	auto tok=src.eat_tok();
	if (tok==close) return nullptr;
	if (tok==OPEN_PAREN){
		auto argname=src.eat_ident();
		return new ArgDef(argname,parse_type(src,CLOSE_PAREN));
	} 
		else return new ArgDef(tok,nullptr);	
}
ExprFnDef* parse_fn(TokenStream&src) {
	auto *fndef=new ExprFnDef();
	printf("parse_fn");
	// read function name or blank
	auto tok=src.eat_tok(); 
	if (tok!=OPEN_PAREN) {
		fndef->name=tok;
		tok=src.eat_tok();
		ASSERT(tok==OPEN_PAREN);
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
	fndef->body = new ExprBlock;
	fndef->body->args.push_back(new ExprIdent(DO));
	while (NONE!=(tok=src.eat_tok())) {
		printf("fn body..");
		if (tok==CLOSE_PAREN)
			break;
		if (tok==OPEN_PAREN) {
			fndef->body->args.push_back(parse_expr(src,CLOSE_PAREN));
		} else {
			printf("parse_raw_ident\n");
			fndef->body->args.push_back(new ExprIdent(tok));
		}
	}

	return fndef;
}
// every file is an implicitly a function aswell taking no args
// when imported, a module inserts a call to that function.
// that sets up global stuff for it.

const char* g_TestProg=
/*
"(fn lerp((a float) (b float) (f float))(+(*(- b a)f)a))"
"(fn lerp((a int) (b int) (f float))(+(*(- b a)f)a))"
"(fn bilerp(a b c d u v)(lerp(lerp a b )(lerp c d)v))"
"(fn invlerp(a b x)(/(- x a)(- b a)))"
"(fn madd(a b f)(+(* b f)a))"
"(fn min(a b)(if (< a b)a b))"
"(fn max(a b)(if (> a b)a b))"
"(fn interleave((a int)(c (map string int))(b (vector float))) (return)) "
"(fn clamp(a b f)(min b (max a f)))"
*/
"(fn they_float()(set tfl 1.0) (they_int))"
"(fn they_int()(set tfx 1) tfx)"
"(set glob_x 10.0)"
"(set glob_y 1)"
"(set glob_z (they_float))"
//"(print (lerp 10 -20 0.0))"
//"(print (lerp 10.0 -20.0 0.5))"
	;
int main(int argc, const char** argv) {
	
	TextInput	src(g_TestProg);
	auto node=parse_expr(src,0);
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



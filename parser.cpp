	#include "ast.h"
#include "semantics.h"
#include "parser.h"
#include "error.h"

//#define pop(X) ASSERT(X.size()>0); pop_sub(X);

void dump(vector<Expr*>& v) {
	for (int i=0; i<v.size(); i++) {
		v[i]->dump_top();
	}
	dbprintf("\n");
}
void pop_operator_call( vector<SrcOp>& operators,vector<Expr*>& operands) {
	//takes the topmost operator from the operator stack
	//creates an expression node calling it, consumes operands,
	//places result on operand stack
	
	auto op=pop(operators);
	auto * p=new ExprOp(op.op,op.pos);
	if (operands.size()>=2 && (arity(op.op)==2)){
		auto arg1=pop(operands);
		p->lhs=pop(operands);
		p->rhs=arg1;
	} else if (operands.size()>=1 && arity(op.op)==1){
		p->rhs=pop(operands);
		//		p->argls.push_back(pop(operands));
	} else{
		//						printf("\noperands:");dump(operands);
		//						printf("operators");dump(operators);
		error(op.pos,"\nerror: %s arity %d, %lu operands given\n",str(op.op),arity(op.op),operands.size());
	}
	p->pos=p->lhs?p->lhs->pos:p->rhs->pos;
	operands.push_back((Expr*)p);
}
//   void fn(x:(int,int),y:(int,int))
void flush_op_stack(ExprBlock* block, vector<SrcOp>& ops,vector<Expr*>& vals) {
	while (ops.size()>0) pop_operator_call(ops,vals);
	while (vals.size()) {
		block->argls.push_back(pop(vals));
	}
}
void flatten_stack(vector<SrcOp>& ops,vector<Expr*>& vals){
	while (vals.size()&&ops.size()) pop_operator_call(ops,vals);
}

ExprBlock* parse_block(TokenStream&src,int close,int delim, Expr* op);

/// parse_expr - parse a single expression
/// TODO - refactor 'parse_block', this is backwards!
vector<Expr*> g_exprpool;
Expr* remove_extraneous_layers(ExprBlock* b){
	while (b->argls.size()==1 && !b->call_expr && b->is_compound_expression()){
		// silly hack to fix the fact we got expr&block backwards
		auto e=b->argls.back();
		b->argls.pop_back();
		ASSERT(b->argls.size()==0);
		g_exprpool.push_back(b);
		if (0==(b=e->as_block())){
			return e;
		}
	}
	return b;
}

Expr* parse_expr(TokenStream&src) {
	auto b= parse_block(src,0,0,nullptr);
	return remove_extraneous_layers(b);
}

void another_operand_so_maybe_flush(bool& was_operand, ExprBlock* node,
									vector<SrcOp>& operators,
									vector<Expr*>& operands
									
									){
	if (was_operand==true) {
		//error(node,"warning undeliminated expression parsing anyway");
		flush_op_stack(node,operators,operands);// keep going
	}
	was_operand=true;
}
LLVMType Node::get_type_llvm() const
{
	if (!this) return LLVMType{VOID,0};
	if (!this->m_type) return LLVMType{VOID,0};
	auto tn=this->m_type->name;
	if (tn==VOID) return LLVMType{VOID,0};
	if (!this->m_type->sub) return LLVMType{tn,0};
	if (tn==PTR || tn==DEREF ||tn==ADDR ) return LLVMType{this->m_type->sub->name,true};
	// todo structs, etc - llvm DOES know about these.
	return LLVMType{0,0};
}

ExprMatch* parse_match(TokenStream& src){
	auto m=new ExprMatch(); m->pos=src.pos;
	
	m->expr=parse_block(src,OPEN_BRACE,COMMA,0);
	// todo, copied form parse_if , but it seems to turn it into a compound block, why?
	
//	m->expr=parse_expr(src);
//	src.expect(OPEN_BRACE);
	MatchArm** pp=&m->arms;
	while (src.peek_tok()!=CLOSE_BRACE){
		auto a=new MatchArm();
		a->pos=src.pos;
		*pp=a;
		pp=&a->next;
		int close=0;
		a->pattern=parse_pattern(src,FAT_ARROW,IF,&close);
		if (close==IF){
			a->cond=parse_block(src,FAT_ARROW,COMMA,0);
		}
		a->body=parse_expr(src);
		auto tok=src.expect(COMMA,CLOSE_BRACE);
		if (tok==CLOSE_BRACE)
			break;
	}
	return	m;
}

// todo: we're not sure we need a whole other parser
// couldn't expressions,types,patterns all use the same gramar & parser,
// & just interpret the nodes differently?
Pattern* parse_pattern(TokenStream& src,int close,int close2,int* close_tok, Pattern* owner){
	// yikes, this is a total mess
	Pattern* prev=0;
	while (auto t=src.eat_tok()){
		if (t==close || t==close2) {if (close_tok){*close_tok=(int)t;}break;}
		if (t==PTR || t==REF || t==MUL || t==ADDR){ // todo: is_prefix
			if (t==MUL) t=PTR;
			if (t==ADDR) t=REF;
			if (prev){ error(src.pos,"ptr/ref operator must be prefix in pattern");}
			auto np=new Pattern(src.pos, t);
			parse_pattern(src,close,close2,close_tok,np);
			if (owner)owner->push_back(np);
			return np;
		}
		if (t==OPEN_PAREN){
			if (!prev) {
				prev=new Pattern(src.pos, TUPLE);
			}
			parse_pattern(src,CLOSE_PAREN,0,close_tok,prev);
		}
		// todo - range ".."
		// todo - slice patterns
		else if (t==LET_ASSIGN || t==DECLARE_WITH_TYPE || t== ASSIGN ||t==PATTERN_BIND){ // todo its @ in scala,rust
			auto np=new Pattern(src.prev_pos,PATTERN_BIND);
			if (owner){ error(src.pos,"pattern bind @ can only be first eg x@whatever(..)");}
			
			np->push_back(prev);
			parse_pattern(src,close,close2, close_tok, np);
			np->dump(0);newline(0);
			return np;
		}
		else if (t==COMMA){ // continue a tuple..
			if (!owner){
				error(src.pos,", in pattern must be within tuple (,,) or destructure ident(,,) ident{,,}");
			}
		} else if (t==OR){ // todo - bit more elaborate. should be ANY(....)  distinct from TUPLE(...)
			if (owner){error(src.pos,"TODO ( | | ), only  | | | works right now");}
			if (!prev){error(src.pos,"in pattern | must seperate options eg a|b|c..");}
			auto np=new Pattern(src.pos,OR);
			np->push_back(prev);
			np->push_back(parse_pattern(src,close,close2,close_tok,nullptr));
			if (owner) {
				owner->push_back(prev);prev=0;
			}
			else { return np; }
		} else{
			if (prev){
				if (owner) owner->push_back(prev);
				else {error(src.pos,"trying to seperate pattern in context where it doesn't work yet");
				}
			}
			prev=new Pattern(src.pos,t);
		}
	}
	if (owner&&prev)owner->push_back(prev);
	return (owner)?nullptr:prev;
}

// default ++p  is {next(p); p}
// default p++ is {let r=p; next(p); r}
//
// for (x,y) in stuff {
// }
// copy how rust iteration works.
// for iter=begin(stuff); valid(iter); next(iter) { (x,y)=extract(iter);   }
//
// desugars same as c++ for (auto p:rhs)
// for (auto p=rhs.begin(); p!=rhs.end(); ++p)
// no; crap idea.
//
//
ExprFnDef* parse_fn_args(TokenStream& src,int close){
	auto *fndef=new ExprFnDef(src.prev_pos);
	fndef->m_closure=true;
	Name tok;
	while (src.peek_tok()!=close) {
		auto arg=parse_arg(src,close);
		fndef->args.push_back(arg);
		src.eat_if(COMMA);
	}
	src.expect(close);
	return fndef;
}
void parse_fn_args_ret(ExprFnDef* fndef,TokenStream& src,int close){
	Name tok;
	while ((tok=src.peek_tok())!=NONE) {
		if (tok==ELIPSIS){
			fndef->variadic=true; src.eat_tok(); src.expect(close); break;
		}
		if (src.eat_if(close)){break;}
		auto arg=parse_arg(src,close);
		fndef->args.push_back(arg);
		src.eat_if(COMMA);
	}
	// TODO: multiple argument blocks for currying?.
	if (src.eat_if(ARROW) || src.eat_if(COLON)) {
		fndef->ret_type = parse_type(src, 0,fndef);
	}
}
void parse_fn_body(ExprFnDef* fndef, TokenStream& src){
	// read function arguments
	// implicit "progn" here..
	auto tok=src.peek_tok();
	if (src.eat_if(ASSIGN)){
		fndef->body=parse_expr(src);
	}else
	if (src.eat_if(OPEN_BRACE)){
		fndef->body = parse_block(src, CLOSE_BRACE, SEMICOLON, nullptr);
	} else if (tok==SEMICOLON || tok==COMMA || tok==CLOSE_BRACE ){
		fndef->body=nullptr; // Its' just an extern prototype.
	} else{  // its' a single-expression functoin, eg lambda.
		fndef->body=parse_expr(src);
	}
}

ExprFnDef* parse_fn(TokenStream&src, ExprStructDef* owner,bool is_virtual) {
	auto *fndef=new ExprFnDef(src.pos);
	// read function name or blank

	if (auto tmp=src.eat_if_string()){
		if (tmp==EXTERN_C){
			fndef->c_linkage=true;
		}else{
			error(fndef,"unknown_linkage %s",str(tmp));
		}
	}

	auto tok=src.eat_tok();
	if (tok!=OPEN_PAREN) {
		ASSERT(is_ident(tok)|| is_operator(tok));
		fndef->name=tok;
		if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
			parse_typeparams_def(src,fndef->typeparams,close_of(open));
		}
		tok=src.expect(OPEN_PAREN);
	} else {
		char tmp[512]; sprintf(tmp,"anon_fn_%d",src.pos.line);
		fndef->name=getStringIndex(tmp);
	}
	// todo: generalize: any named parameter might be 'this'.
	//.. but such functions will be outside the struct.
	if (owner){
		fndef->args.push_back(new ArgDef(src.pos,THIS,new Type(PTR,owner)));
	}
	parse_fn_args_ret(fndef,src,CLOSE_PAREN);
	parse_fn_body(fndef,src);
	return fndef;
}
ExprFnDef* parse_closure(TokenStream&src,int close) {// eg |x|x*2
	// we read | to get here
	auto *fndef=new ExprFnDef(src.pos);
	// read function name or blank
	
	char tmp[512]; sprintf(tmp,"closure_%d",src.pos.line);
	fndef->name=getStringIndex(tmp);
	fndef->m_closure=true;
	
	parse_fn_args_ret(fndef,src,close);
	parse_fn_body(fndef,src);
	return fndef;
}

ExprOp* parse_let(TokenStream& src) {
	// TODO: parse_pattern - we DO want all the tuple assignment goodness
	auto ident=src.eat_ident();
	auto id=new ExprIdent(src.prev_pos,ident);
	Type * t=nullptr;
	Expr* init=nullptr;
	if (src.eat_if(COLON)){
		t=parse_type(src,0,nullptr);
	}
	//nlet->lhs->set_type(t);// we dont need an operator, all nodes have type
	
	if (src.eat_if(ASSIGN)){
		init=parse_expr(src);
	}
// cases..
	if (!t && init){
		auto nlet=new ExprOp(LET_ASSIGN,src.prev_pos);
		nlet->lhs=id;
		nlet->rhs=init;
		return nlet;
	}
	else if (t){
		if (!init){
			auto nlet=new ExprOp(DECLARE_WITH_TYPE,src.prev_pos);
			nlet->lhs=id;
			nlet->rhs=t;
			return nlet;
		}
		else{
			auto nassign=new ExprOp(LET_ASSIGN,src.prev_pos);
			nassign->lhs=id;
			nassign->rhs=new ExprOp(AS, src.prev_pos, init, Type::get_bool());
			nassign->rhs->set_type(Type::get_bool());
			return nassign;
		}
	
	}
	else{
		auto nlet=new ExprOp(DECLARE_WITH_TYPE,src.prev_pos);
		nlet->lhs=id;
		nlet->rhs=nullptr;//new Type(AUTO);
		//error(nlet,"TODO - let <var with no type or init expr>\ntype inference should handle it but we must double check ..");
		return nlet;
	}
}

Expr* expect_pop(TokenStream& src,vector<Expr*>& ops){
	if (!ops.size()){
		error(src.pos,"expected operand first");
		return nullptr;
	}
	auto p=ops.back(); ops.pop_back();
	return p;
}
void expect(TokenStream& src,bool expr,const char* msg){
	if (!expr){
		error(src.pos,msg);
	}
}
ExprBlock* ExprBlock_alloc(SrcPos& pos){
	// todo - 'g_exprpool'=temp hack for stupid expr / block conflation
	// we free these up when parse_block() allocates extraneously for "parse_expr()"
	// parse_block & parse expr are backwards.
	
//	if (g_exprpool.size()){auto node=g_exprpool.back()->as_block(); g_exprpool.pop_back(); return node;}
//	else
		return new ExprBlock(pos);
}

ExprBlock* parse_block(TokenStream& src,int close,int delim, Expr* outer_op) {
	// shunting yard expression parser+dispatch to other contexts
	ExprBlock *node=ExprBlock_alloc(src.pos);
	node->call_expr=outer_op;
	if (!g_pRoot) g_pRoot=node;
	verify(node->type());
	vector<SrcOp> operators;
	vector<Expr*> operands;
	bool	was_operand=false;
	int wrong_delim=delim==SEMICOLON?COMMA:SEMICOLON;
	int wrong_close=close==CLOSE_PAREN?CLOSE_BRACE:CLOSE_PAREN;
	node->bracket_type=(close==CLOSE_BRACKET)?OPEN_BRACKET:close==CLOSE_PAREN?OPEN_PAREN:close==CLOSE_BRACE?OPEN_BRACE:0;
	while (true) {
		auto pos=src.pos;
		if (src.peek_tok()==0) break;
		if (src.peek_tok()==IN) break;
		// parsing a single expression TODO split this into 'parse expr()', 'parse_compound'
		if (close || delim) { // compound expression mode.
			if (src.eat_if(close))
				break;
			if (src.eat_if(wrong_close)) {
				error(0,"unexpected %s, expected %s",getString(close),getString(wrong_close));
				error_end(0);
			}
		} else { // single expression mode - we dont consume delimiter.
			auto peek=src.peek_tok();
			if (peek==CLOSE_BRACKET || peek==CLOSE_BRACE || peek==COMMA || peek==SEMICOLON)
				break;
		}
		
		if (src.is_next_literal()||src.is_next(BOOL_TRUE,BOOL_FALSE,NULLPTR,VOID)) {
			auto ln=parse_literal(src);
			operands.push_back(ln);
			was_operand=true;
			continue;
		}
		else if (src.eat_if(LET)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_let(src));
		}
		else if (src.eat_if(STRUCT)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_struct(src));
		}
		else if (src.eat_if(ENUM)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_enum(src));
		}
		else if (src.eat_if(MATCH)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_match(src));
		}
		else if (src.eat_if(EXTERN)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			/// TODO local functions should be sugar for rolling a closure bound to a variable.
			auto abi=src.eat_if_string();
			auto fnp=src.eat_if(FN);
			auto local_fn=parse_fn(src,nullptr);
			if (abi==EXTERN_C) local_fn->c_linkage=true;
			operands.push_back(local_fn);
		}
		else if (src.eat_if(FN)) {
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			/// TODO local functions should be sugar for rolling a closure bound to a variable.
			auto local_fn=parse_fn(src,nullptr);
			operands.push_back(local_fn);
		}
		else if (src.eat_if(DO)){
			// simple syntax sugar- do notation,parser hack
			flatten_stack(operators,operands);
			expect(src,operands.size()>=1,"need preceeding expression for 'do x{..}");
			auto fndef=parse_fn_args( src, OPEN_BRACE);
			fndef->body=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
			auto prev=operands.back();
			prev->as_block()->argls.push_back(fndef);
			flush_op_stack(node,operators,operands);
			was_operand=false;
		}
		else if (src.eat_if(DOUBLE_COLON)||src.peek_tok()==OPEN_TYPARAM){ // eg array::<int,5>
			auto open_tp=src.eat_if(LT,OPEN_BRACKET,OPEN_TYPARAM);
			if (open_tp && was_operand){
				auto id=pop(operands)->as_ident();
				if (!id){error(src.pos,"::<TypeParams> must follow identifier");}
				auto itw=parse_tparams_for_ident(src,id,close_of(open_tp));
				operands.push_back(itw);
			} else {
				error(src.pos,"::<TypeParams> must follow identifier");
			}
		}
		else if (src.eat_if(WHERE)){
			// simple syntax sugar,parser hack.
			flatten_stack(operators,operands);
			expect(src,operands.size()>=1,"need preceeding expression for 'where{..}");

			//another_operand_so_maybe_flush(was_operand,node,operators,operands);
			auto b2=parse_block(src,0,0,nullptr);
			ASSERT(b2->argls.size()<=1 && "remove when expr/block hack is fixed")
			b2->argls.back()->as_block()->argls.push_back(expect_pop(src,operands));
			operands.push_back(b2);
		}
		else if (src.eat_if(FOR)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_for(src));
		}
		else if (src.eat_if(IF)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_if(src));
		}
		else if (auto t=src.eat_if(BREAK,RETURN,CONTINUE)){
			another_operand_so_maybe_flush(was_operand,node,operators,operands);
			operands.push_back(parse_flow(src,t));
		}
		else if (src.eat_if(OPEN_PAREN)) {
			if (was_operand){
				operands.push_back(parse_block(src, CLOSE_PAREN,SEMICOLON, pop(operands)));
				// call result is operand
			}
			else {
				operands.push_back(parse_block(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACKET)){
			if (was_operand){
				operands.push_back(parse_block(src,CLOSE_BRACKET,COMMA,pop(operands)));
			} else {
				error(operands.back()?operands.back():node,"TODO: array initializer");
				operands.push_back(parse_block(src,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACE)){
			//			error(operands.back()?operands.back():node,"struct initializer");
			if (was_operand){// struct initializer
				auto sname=pop(operands);
				auto si=parse_block(src,CLOSE_BRACE,COMMA,sname);
				operands.push_back(si);
				si->set_type(sname->get_type()); //eg ident might have typeparams.struct init uses given type
				dbg(operands.back()->dump(0));
			}
			else{//progn aka scope block with return value.
				auto sub=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
				operands.push_back(sub);
				if (sub->delimiter==COMMA)
					sub->create_anon_struct_initializer();
			}
		} else if (src.eat_if(delim)) {
			flush_op_stack(node,operators,operands);
			node->set_delim(delim);
			was_operand=false;
		}
		else if (src.eat_if(wrong_delim) && delim){ //allows ,,;,,;,,  TODO. more precise.
			node->set_delim(wrong_delim);
			flush_op_stack(node,operators,operands);// keep going
			was_operand=false;
		}
		else{
			auto tok=src.eat_tok();
			if (!was_operand && tok==OR){
				src.begin_lambda_bar_arglist();
				another_operand_so_maybe_flush(was_operand,node,operators,operands);
				operands.push_back(parse_closure(src,OR));
			} else
				if (is_operator(tok)) {
					// struct initializer
					if (close==CLOSE_BRACE && delim==COMMA && outer_op){// todo field-init operator
						if (tok==COLON || tok==ASSIGN)
							tok=FIELD_ASSIGN;
					}
					
					if (was_operand) tok=get_infix_operator(tok);
					else tok=get_prefix_operator(tok);
					
					
					verify_all();
					while (operators.size()>0) {
						int prev_precedence=precedence(operators.back().op);
						int prec=precedence(tok);
						if (prev_precedence>prec
							||(is_right_assoc(tok)&&prec==prev_precedence))
							break;
						pop_operator_call(operators,operands);
					}
					verify_all();
					if (tok==AS){
						Type *t=parse_type(src,0,nullptr);
						if (!was_operand) error(t,"as must follow operand");
						auto lhs=operands.back(); operands.pop_back();
						operands.push_back(new ExprOp(AS,src.pos,lhs,t));
						was_operand=true;
						t->set_type(t);
					}else
						if (tok==COLON){// special case: : invokes parsing type. TODO: we actually want to get rid of this? type could be read from other nodes, parsed same as rest?
							Type *t=parse_type(src,0,nullptr);
							auto lhs=operands.back();
							lhs->set_type(t);
							was_operand=true;
						} else if (tok==DECLARE_WITH_TYPE){ //x=:Type ==let x:Type  (creates a var of 'Type').
							Type *t=parse_type(src,0,nullptr);
							operators.push_back(SrcOp{tok,pos});
							operands.push_back(t);
							was_operand=true;
						}
						else{
							operators.push_back(SrcOp{tok,pos});
							was_operand=false;
						}
				} else {
					another_operand_so_maybe_flush(was_operand,node,operators,operands);
					operands.push_back(new ExprIdent(tok,pos));
				}
		}
		//ASSERT(sub);
		//node->argls.push_back(sub);
	};
	if (operands.size()){
		// final expression is also returnvalue,
		flush_op_stack(node,operators,operands);
	} else if (node->is_compound_expression() &&
			   close!=OPEN_BRACE&& close!=FAT_ARROW)// not a if ...{  or match...{  match..if ..=>{}etc
	{
		node->argls.push_back(new ExprLiteral(src.prev_pos));
	}
	verify(node->get_type());
	node->verify();
	return node;
}
ExprOp* parse_flow(TokenStream& src,Name flow_statement){
	// eg break,continue,return. generalized return values, so they are expr.
	Expr* expr=nullptr;
	int levels=1;
	while (src.eat_if(flow_statement)){
		levels++;
	}
	if (!(src.peek_tok()==CLOSE_BRACE || src.peek_tok()==SEMICOLON)){
		dbprintf("%s\n",str(src.peek_tok()));
		expr=parse_expr(src);
	}
	return new ExprOp(flow_statement, src.pos,new ExprLiteral(src.pos,levels),expr);
}

ExprLiteral* parse_literal(TokenStream& src) {
	ExprLiteral* ln=nullptr;
	if (auto tok=src.eat_if(BOOL_TRUE,BOOL_FALSE,NULLPTR,VOID)){// special
		ln=new ExprLiteral(src.prev_pos);
		ln->name=tok;
		if (tok==VOID){
			ln->type_id=T_VOID;
			ln->set_type(Type::get_bool());
		}
		if (tok==NULLPTR){	// nullptr is a void*, autocoerce to all others.
			ln->u.val_ptr=nullptr;ln->type_id=T_NULLPTR;
			ln->set_type(Type::get_void_ptr());
		} else {
			ln->u.val_bool=tok==BOOL_TRUE?true:false;ln->type_id=T_BOOL;
			ln->set_type(Type::get_bool());
		}
	}else if (src.is_next_number()) {
		auto n=src.eat_number();
		if (n.denom==1) {ln=new ExprLiteral(src.prev_pos,n.num);}
		else {ln=new ExprLiteral(src.pos, (float)n.num/(float)n.denom);}
	} else if (src.is_next_string()) {
		ln=new ExprLiteral(src.prev_pos,src.eat_string_alloc());
	}
	else{
		error(0,"error parsing literal\n");
		error_end(0);
	}
	return ln;
}

void parse_ret_val(TokenStream& src, Node* owner, Type* fn_type){
	if (src.eat_if("->")){
		fn_type->push_back(parse_type(src,0,owner));// return value
	} else{
		fn_type->push_back(new Type(owner,VOID));// return value
	}
}
Type* parse_tuple(TokenStream& src, Node* owner)
{
	auto ret= new Type(TUPLE,src.pos);
	while (auto sub=parse_type(src, CLOSE_PAREN,owner)){
		ret->push_back(sub);
		src.eat_if(COMMA);
	}
	return ret;
}
Type* parse_type(TokenStream& src, int close,Node* owner) {
	auto tok=src.eat_tok();
	Type* ret=0;	// read the first, its the form..
	if (tok==close) return nullptr;
	if (tok==FN){	// fn(arg0,arg1,...)->ret
		ret=new Type(FN,src.pos);
		src.expect(OPEN_PAREN,"eg fn() fn name()");
		ret->push_back(parse_tuple(src,owner));// args
		parse_ret_val(src,owner,ret);
	}
	else if (tok==STRUCT){
		auto sd=parse_struct(src);
		ret=new Type(sd); ret->pos=src.pos;
		
	}
	else if (tok==OR){ // rust-style closure |arg0,arg1,..|->ret
		ret = new Type(owner,CLOSURE);
		auto args=new Type(owner,TUPLE); ret->push_back(args);
		src.begin_lambda_bar_arglist();
		while ((src.peek_tok())!=OR){
			args->push_back(parse_type(src,OR,owner));
			src.eat_if(COMMA);
		}
		src.expect(OR);
		parse_ret_val(src,owner,ret);
	}
	else if (tok==OPEN_PAREN) {
		ret=parse_tuple(src,owner);
		if (src.eat_if(ARROW)){
			// tuple->type  defines a function.
			auto fn_ret=parse_type(src,0,owner);
			auto fn_type=new Type(owner,CLOSURE);
			fn_type->push_back(ret);
			fn_type->push_back(fn_ret);
			return fn_type;
		}
	} else {
		// prefixes in typegrammar - pointers
		if (tok==MUL){
			ret=new Type(owner,PTR);
			ret->sub=parse_type(src,close,owner);
		}
		else
		if (tok==AND) {
			ret=new Type(owner,REF);
			ret->sub=parse_type(src,close,owner);
		}else {
			// main: something[typeparams]..
			ret = new Type(owner,tok);
			if (auto open=src.eat_if(OPEN_BRACKET,LT,OPEN_TYPARAM)) {
				while (auto sub=parse_type(src, close_of(open),owner)){
					ret->push_back(sub);
					src.eat_if(COMMA);
				}
			}
			// postfixes:  eg FOO|BAR|BAZ todo  FOO+BAR+BAZ  also FOO*BAR*BAZ like ml? not sure its' wise alongside pointer types.
			// might not play well with pointers, but hell we have them in
			if (src.peek_tok()==OR && close!=OR){
				Type* sub=ret; ret=new Type(owner,VARIANT); ret->push_back(sub);
				while (src.eat_if(OR)){
					auto sub=parse_type(src,close,owner);
					ret->push_back(sub);
				}
			}
		}
	}
	if (!owner) ret->set_origin(ret);	// its a type declaration, 'origin is here'.
	// todo: pointers, adresses, arrays..
	return ret;
}
ArgDef* parse_arg(TokenStream& src, int close) {
	auto argname=(src.peek_tok()==COLON)?PLACEHOLDER:src.eat_ident();
	if (argname==close) return nullptr;
	auto a=new ArgDef(src.pos,argname);
	a->pos=src.pos;
	if (src.eat_if(COLON)) {
		a->type()=parse_type(src,close,a);
	}
	if (src.eat_if(ASSIGN)){
		a->default_expr=parse_expr(src);
	}
	return a;
}
void parse_typeparams_def(TokenStream& src,vector<TParamDef*>& out,int close) {
	while (!src.eat_if(close)){
		//		if (src.eat_if(CLOSE_BRACKET)) break;
		out.push_back(
			new TParamDef(
				src.prev_pos,
				src.eat_tok(),
				src.eat_if(COLON)?parse_type(src,0,nullptr):nullptr,
				src.eat_if(ASSIGN)?parse_type(src,0,nullptr):nullptr
				));
		src.eat_if(COMMA);
	}
}
void parse_typeparams_given(TokenStream& src, Type* addto, int close){
	while (!src.eat_if(close)){
		auto tp=parse_type(src,COMMA,addto);
		addto->push_back(tp);
		src.eat_if(COMMA);
	}
}
IdentWithTParams* parse_tparams_for_ident(TokenStream& src,ExprIdent* id,int close){
	auto iwt=new IdentWithTParams(id->pos,id);
	while (!src.eat_if(close)){
		auto tp=parse_type(src,COMMA,iwt);
		iwt->given_tparams.push_back(tp);
		src.eat_if(COMMA);
	}
	return iwt;
}

ExprStructDef* parse_struct_body(TokenStream& src,SrcPos pos,Name name, Type* force_inherit);

ExprStructDef* parse_struct(TokenStream& src) {
	auto pos=src.pos;
	auto sname=src.peek_tok()==OPEN_BRACE?0:src.eat_ident();
	auto sd=parse_struct_body(src,pos,sname,nullptr);
	if (src.eat_if(WHERE)){
		src.expect(OPEN_BRACE);
		sd->body=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
	}
	return sd;
}
// TODO: are struct,trait,enum actually all the same thing with a different 'default'
ExprStructDef* parse_tuple_struct_body_sub(TokenStream& src, ExprStructDef* sd){
	Name tok;
	while ((tok=src.peek_tok())!=NONE){
		if (tok==CLOSE_PAREN){src.eat_tok(); break;}
		sd->fields.push_back(new ArgDef(src.prev_pos,0,parse_type(src,0,sd)));
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	return sd;
}

ExprStructDef* parse_struct_body(TokenStream& src,SrcPos pos,Name name, Type* force_inherit){
	auto sd=new ExprStructDef(pos,name);
	// todo, namespace it FFS.
	vector<ArgDef*> args;
	if (src.eat_if(OPEN_PAREN)){ // constructor args eg struct Foo(x,y,z){field1=x+y,..}
		while (!src.eat_if(CLOSE_PAREN)){
			args.push_back(parse_arg(src,0));
			src.eat_if(COMMA);
		}
	}
	for (auto x:args)x->dump(0);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,sd->typeparams,close_of(open));
	}
	if (src.eat_if(COLON)) {
		sd->inherits_type = parse_type(src,0,nullptr); // inherited base has typeparams. only single-inheritance allowed. its essentially an anonymous field
	} else {
		sd->inherits_type = force_inherit; // enum is sugar rolling a number of classes from base
	}
	// todo - tuple struct..
	vector<ArgDef*> default_construct_args;
	if (src.eat_if(OPEN_PAREN)){
		return parse_tuple_struct_body_sub(src,sd);
		// todo: if open brace follows, we actually have a default constructor & struct body combo.
	}
	if (!src.eat_if(OPEN_BRACE)){	// empty struct or tuple struct.
		sd->fields=args;
		return sd;
	}
	sd->args=args;
	// todo: type-params.
	Name tok;
	while ((tok=src.peek_tok())!=NONE){
		if (tok==CLOSE_BRACE){src.eat_tok(); break;}
		if (src.eat_if(STRUCT)) {
			sd->structs.push_back(parse_struct(src));
		} else if (auto cmd=src.eat_if(FN)){
			sd->functions.push_back(parse_fn(src,sd));
		} else if (auto cmd=src.eat_if(VIRTUAL)){
			if (sd->inherits_type){
				error(sd,"limited vtables - currently this can only describe the vtable layout in the base class.\nTODO - this is just a temporary simplification,  other priorities eg rust-style traits, ADTs, static-virtuals, reflection..\n");
			}
			sd->virtual_functions.push_back(parse_fn(src,sd,true));
		} else if (auto cmd=src.eat_if(STATIC)){
			auto arg=parse_arg(src,CLOSE_PAREN);
			if (src.eat_if(VIRTUAL)){
				sd->static_virtual.push_back(arg);
			} else sd->static_fields.push_back(arg);
		} else {
			auto arg=parse_arg(src,CLOSE_PAREN);
			sd->fields.push_back(arg);
		}
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	// if there's any virtual functions, stuff a vtable pointer in the start
	return sd;
}

ExprStructDef* parse_tuple_struct_body(TokenStream& src, SrcPos pos, Name name){
	Name tok;
	auto sd=new ExprStructDef(pos,name);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,sd->typeparams,close_of(open));
	}
	if (!src.eat_if(OPEN_PAREN))
		return sd;
	return parse_tuple_struct_body_sub(src,sd);
}
EnumDef* parse_enum(TokenStream& src) {
	auto pos=src.pos;
	auto tok=src.eat_ident();
	auto ed=new EnumDef(src.pos,tok);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,ed->typeparams,close_of(open));
	}
	if (src.eat_if(COLON)) {
		ed->inherits_type = parse_type(src,0,ed); // inherited base has typeparams. only single-inheritance allowed. its essentially an anonymous field
	}
	if (!src.eat_if(OPEN_BRACE))
		return ed;
	// todo: type-params.
	int index=0;		// TODO: computed discriminants; it will have to be subindex+expression
	while ((tok=src.eat_tok())!=NONE){
		auto subpos=src.pos;
		if (tok==CLOSE_BRACE){break;}
		// got an ident, now what definition follows.. =value, {fields}, (types), ..
		if (src.peek_tok()==OPEN_BRACE){
			auto sd=parse_struct_body(src,subpos,tok,nullptr);
			sd->set_variant_of(ed,index++);
			ed->structs.push_back(sd);
			
		} else if (src.peek_tok()==OPEN_BRACKET){
			auto sd=parse_tuple_struct_body(src,subpos,tok);
			sd->set_variant_of(ed,index++);
			ed->structs.push_back(sd);
		} else if (src.eat_if(ASSIGN)){
			auto lit=parse_literal(src); lit->name=tok;
			ed->literals.push_back(lit);
			index=lit->u.val_int;
		} else {
			ed->literals.push_back(new ExprLiteral(src.prev_pos,index++));
		}
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	return ed;
}

// iterator protocol. value.init. increment & end test.
ExprFor* parse_for(TokenStream& src){
	auto p=new ExprFor(src.pos);
	auto first=parse_block(src,SEMICOLON,COMMA,0);
	if (src.eat_if(IN)){
		p->pattern=first;
		p->init=parse_block(src, OPEN_BRACE, 0, 0);
		src.expect(OPEN_BRACE,"eg for x..in..{}");
	} else {//if (src.eat_if(SEMICOLON)){// cfor.  for init;condition;incr{body}
		p->pattern=0;
		p->init=first;
		p->cond=parse_block(src,SEMICOLON,COMMA,0);
		//ssrc.expect(SEMICOLON,"eg for init;cond;inc{..}");
		p->incr=parse_block(src,OPEN_BRACE,COMMA,0);
	}
 //else {
	//		error(p,"for..in.. or c style for loop, expect for init;cond;incr{body}");
	//	}
	p->body=parse_block(src, CLOSE_BRACE, SEMICOLON, nullptr);
	if (src.eat_if(ELSE)){
		src.expect(OPEN_BRACE,"after else");
		p->else_block=parse_block(src,CLOSE_BRACE, SEMICOLON, nullptr);
	}
	return p;
}

// make a flag for c or rust mode
// exact c parser
// add := gets rid of auto noise
// add postfix : alternate functoin syntax
ExprIf* parse_if(TokenStream& src){
	// TODO: assignments inside the 'if ..' should be in-scope
	// eg if (result,err)=do_something(),err==ok {....}  else {...}
	auto p=new ExprIf(src.pos);
	//	p->cond=parse_block(src, OPEN_BRACE, 0, 0);
	p->cond=parse_block(src,OPEN_BRACE,COMMA,0);
	p->body=parse_block(src, CLOSE_BRACE,SEMICOLON,0);
	verify(p->cond->get_type());
	
	if (src.eat_if(ELSE)) {
		if (src.eat_if(IF)) {
			p->else_block= parse_if(src);
		} else if (src.eat_if(OPEN_BRACE)){
			p->else_block=parse_block(src, CLOSE_BRACE, SEMICOLON, 0);
		} else {
			error(0,"if { }else {} expected\n");
		}
	}
	if (p->cond) verify(p->cond->get_type());
	if (p->body) verify(p->body->get_type());
	if (p->else_block) verify(p->else_block->get_type());
	return p;
}







#include "node.h"
#include "parser.h"

//#define pop(X) ASSERT(X.size()>0); pop_sub(X);

void dump(vector<Expr*>& v) {
	for (int i=0; i<v.size(); i++) {
		v[i]->dump_top();
	}
	dbprintf("\n");
}
void pop_operator_call( Vec<SrcOp>& operators,Vec<Expr*>& operands) {
	//takes the topmost operator from the operator stack
	//creates an expression node calling it, consumes operands,
	//places result on operand stack
	
	auto op=operators.pop();
	auto * p=new ExprOp(op.op,op.pos);
	if (operands.size()>=2 && (arity(op.op)==2)){
		auto arg1=operands.pop();
		p->lhs=operands.pop();
		p->rhs=arg1;
	} else if (operands.size()>=1 && arity(op.op)==1){
		p->rhs=operands.pop();
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
void flush_op_stack(ExprLs nodes, Vec<SrcOp>& ops,Vec<Expr*>& vals) {
	while (ops.size()>0) pop_operator_call(ops,vals);
	while (vals.size()) {
		nodes->push_back(pop(vals));
	}
}
void flatten_stack(Vec<SrcOp>& ops,Vec<Expr*>& vals){
	while (vals.size()&&ops.size()) pop_operator_call(ops,vals);
}

ExprBlock* parse_block(TokenStream&src,int close,int delim, Expr* op);

Expr* parse_expr(TokenStream&src) {
	Vec<Expr*> nodes;
	int delim;
	parse_block_nodes(&nodes,&delim, src,nullptr, 0,0);// TODO - retardation- its' allocating. Vec.num==1 could be inplace.
	ASSERT(nodes.size()==1);
	return nodes.pop_back();
}

void another_operand_so_maybe_flush(bool& was_operand, ExprLs nodes,
									Vec<SrcOp>& operators,
									Vec<Expr*>& operands
									
									){
	if (was_operand==true) {
		//error(node,"warning undeliminated expression parsing anyway");
		flush_op_stack(nodes,operators,operands);// keep going
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
// m->expr=parse_expr(src);
// src.expect(OPEN_BRACE);

	MatchArm** pp=&m->arms;
	while (src.peek_tok()!=CLOSE_BRACE){
		auto a=new MatchArm();
		a->pos=src.pos;
		*pp=a;
		pp=&a->next;
		a->pattern=parse_pattern(src,FAT_ARROW,IF,0);
		if (src.eat_if(IF)){
			// todo , slightly hacky , pattern/expr mess; would be cleaner with GuardedPattern but verbose
			auto ifpat=new Pattern(src.prev_pos,IF);
			ifpat->push_back(a->pattern);
			auto cond=(Pattern*)parse_block(src,FAT_ARROW,COMMA,0);
			ifpat->push_back(cond);
			a->pattern=ifpat;
		}
		dbg(dbprintf("%s \n",str(src.peek_tok())));
		src.eat_if(FAT_ARROW);
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
Pattern* parse_pattern(TokenStream& src,int close,int close2,int close3, Pattern* owner){
	// yikes, this is a total mess
	Pattern* prev=0;
	while (auto t=src.peek_tok()){
		dbg(dbprintf("%s\n",str(t)));
		if (t==close || t==close2||t==close3||t==ASSIGN||t==LET_ASSIGN||t==COLON) {
			break;
		}
		if (t==LIFETIME){
			error(src.pos,"lifetime params can't appear in pattern\n");
		}
		t=src.eat_tok();
		if (!prev && t==PATTERN_BIND){//backticks would be better since this can be @
			auto np=new Pattern(src.pos, EXPRESSION);
			np->set_sub_expr(parse_expr(src));
			if (owner)owner->push_back(np);
			return np;
		}
		if (t==PTR || t==REF || t==MUL || t==AND || t==ADDR || t==NOT){ // todo: is_prefix
			if (t==MUL) t=PTR;
			if (t==ADDR || t==AND) t=REF;
			if (prev){ error(src.pos,"ptr/ref operator must be prefix in pattern");}
			auto np=new Pattern(src.pos, t);
			parse_pattern(src,close,close2,close3,np);
			if (owner)owner->push_back(np);
			return np;

//			if (owner){
//				owner->push_back(np);
//				return nullptr;
//			}else
//				return np;
//		}else 
		}else
		if (t==OPEN_PAREN){
			if (!prev) {
				prev=new Pattern(src.pos, TUPLE);
			}
			auto np=parse_pattern(src,CLOSE_PAREN,0,0,prev);
			if (!prev->sub){
				prev->sub=new Pattern(src.pos,VOID);
			}
			src.expect(CLOSE_PAREN); // todo: we could avoid repeat arg encapsulated in lexer. 'push terminator'/'pop terminator'... 'src.is_terminator'
		}else
 
		if (t==RANGE){
			if(!prev){
				error(src.prev_pos,"range needs preceeding value eg a..b");
			}
			auto np=new Pattern(src.prev_pos,RANGE);
			np->push_back(prev);
			np->push_back(parse_pattern(src,close,close2,close3?close3:OR,nullptr));
	
			if (owner){
				owner->push_back(np);prev=0;
			}
			else prev=np;
		}
		// todo - range ".."
		// todo - slice patterns
		else if (t==PATTERN_BIND){ // todo its @ in scala,rust
			auto np=new Pattern(src.prev_pos,PATTERN_BIND);
			if (owner){
				error(src.pos,"pattern bind @ can only be first eg x@whatever(..)");
			}
			
			np->push_back(prev);
			parse_pattern(src,close,close2,close3, np);
			np->dump(0);newline(0);
			return np;
		}
		else if (t==COMMA){ // continue a tuple..
			if (!owner){ // ITS DELIM
				//error(src.pos,", in pattern must be within tuple (,,) or destructure ident(,,) ident{,,}");
				break;
			}
			if (prev && owner){
				owner->push_back(prev);prev=nullptr;
			}
		} else if (t==OR){ // todo - bit more elaborate. should be ANY(....)  distinct from TUPLE(...)
			//if (owner){
			//	error(src.pos,"TODO ( | | ), only  | | | works right now");
			//}
			if (!prev){
				error(src.pos,"in pattern | must seperate options eg a|b|c..");
			}
			auto np=new Pattern(src.pos,OR);
			np->push_back(prev);
			np->push_back(parse_pattern(src,close,close2,close3,nullptr));
			if (owner) {
				owner->push_back(np);prev=0;
			}
			else { return np; }
		} else{
			if (prev){
				if (owner){
					owner->push_back(prev);
				}
				else {
					error(src.pos,"trying to seperate pattern in context where it doesn't work yet");
				}
			}
			prev=new Pattern(src.pos,t);
		}
	}
	if (owner&&prev)
		owner->push_back(prev);
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
	int i=0;
	while (src.peek_tok()!=close) {
		auto arg=parse_arg(i++,src,close);
		fndef->args.push_back(arg);
		src.eat_if(COMMA);
	}
	src.expect(close);
	return fndef;
}
void parse_fn_args_ret(ExprFnDef* fndef,TokenStream& src,int close){
	Name tok;
	int i=0;
	while ((tok=src.peek_tok())!=NO_TOK) {
		if (tok==ELIPSIS){
			fndef->variadic=true; src.eat_tok(); src.expect(close); break;
		}
		if (src.eat_if(close)){break;}
		auto arg=parse_arg_or_self(i++,src,fndef->self_t, close);
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

TypeDef* parse_typedef(TokenStream& src){
	auto td=new TypeDef(src.prev_pos, src.eat_ident());
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,td->tparams,close_of(open));
	}
	src.expect(ASSIGN);
	td->type_def=parse_type(src, SEMICOLON, td);
	return td;
}

ExprFnDef* parse_fn(TokenStream&src, ExprStructDef* owner,Type* self_t, bool is_virtual) {
	auto *fndef=new ExprFnDef(src.pos);
	// read function name or blank
	fndef->self_t=(Type*)self_t->clone_if();
	if (auto tmp=src.eat_if_string()){
		if (tmp==EXTERN_C){
			fndef->c_linkage=true;
		}else{
			error(fndef,"unknown_linkage %s",str(tmp));
		}
	}

	auto tok=src.eat_tok();
	if (tok==COMPLEMENT) {
		tok=src.eat_tok();// ignore this but it should be owner->name
		if (!owner || tok!=owner->name) {
			error(src.prev_pos,"C++ style destructor must be defined in struct with same name");
		}
		// we name the destructor differently, to avoid confusion between C++/Rust
		tok=__DESTRUCTOR;
	}
	if (tok==DROP){	// we rename rust style destuctors to drop too.
		tok=__DESTRUCTOR;
	}
	if (tok!=OPEN_PAREN) {
		if (!(is_ident(tok)|| is_operator(tok))){
			dbprintf(str(tok));
			error(src.prev_pos,"expected identifer or operator in fn def\n");
		}
		fndef->name=tok;
		if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
			parse_typeparams_def(src,fndef->tparams,close_of(open));
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
	// translate 'self' into given self_t
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
//	auto ident=src.eat_ident();
//	auto id=new ExprIdent(src.prev_pos,ident);
	auto ptn=parse_pattern(src, ASSIGN,COLON,SEMICOLON);
	Type * t=nullptr;
	Expr* init=nullptr;
	if (src.eat_if(COLON)){//src.eat_if(COLON)){
		t=parse_type(src,0,nullptr);
	}
	//nlet->lhs->set_type(t);// we dont need an operator, all nodes have type
	else
	if (src.eat_if(ASSIGN)){
		init=parse_expr(src);
	}
	else
	if (src.peek_tok()!=SEMICOLON){
		error_begin(ptn,"found %s,expected let pattern[[:type][=expr]]; ",str(src.peek_tok()));
		
		error_end(ptn);
	}
// cases..
	if (!t && init){
		auto nlet=new ExprOp(LET_ASSIGN,src.prev_pos);
		nlet->lhs=(Expr*)ptn;
		nlet->rhs=init;
		return nlet;
	}
	else if (t){
		if (!init){
			auto nlet=new ExprOp(DECLARE_WITH_TYPE,src.prev_pos);
			nlet->lhs=(Expr*)ptn;
			nlet->rhs=t;
			return nlet;
		}
		else{
			auto nassign=new ExprOp(LET_ASSIGN,src.prev_pos);
			nassign->lhs=(Expr*)ptn;
			nassign->rhs=new ExprOp(AS, src.prev_pos, init, Type::get_bool());
			nassign->rhs->set_type(Type::get_bool());
			return nassign;
		}
	
	}
	else{
		auto nlet=new ExprOp(DECLARE_WITH_TYPE,src.prev_pos);
		nlet->lhs=(Expr*)ptn;
		nlet->rhs=nullptr;//new Type(AUTO);
		//error(nlet,"TODO - let <var with no type or init expr>\ntype inference should handle it but we must double check ..");
		return nlet;
	}
	//else {
	//	error(ptn,"error parsing let expression,unhandled case");
	//}
}

Expr* expect_pop(TokenStream& src,Vec<Expr*>& ops){
	if (!ops.size()){
		error(src.pos,"expected operand first");
		return nullptr;
	}
	auto p=ops.back(); ops.pop();
	return p;
}
void expect(TokenStream& src,bool expr,const char* msg){
	if (!expr){
		error(src.pos,msg);
	}
}

ExprBlock* parse_block_sub(ExprBlock* node, TokenStream& src,Expr* insert,int close,int delim, Expr* outer_node);

ExprBlock* parse_block(TokenStream& src,int close,int delim, Expr* outer_node) {
	return parse_block_sub(new ExprCompound(), src,nullptr,close,delim, outer_node);
}

ExprBlock* parse_subexpr_or_tuple(TokenStream& src) {
	// todo - parse the first expr. look for comma, insantiate ExprTuple or ExprBlock accordingly.
	auto pos=src.pos;
	Vec<Expr*> nodes;
	int delim_found=0;
	parse_block_nodes(&nodes,&delim_found,src,nullptr, CLOSE_PAREN,COMMA);
	ExprBlock* r=nullptr;
	if (nodes.size()>1 ||delim_found==COMMA){
		r=new ExprTuple();
	} else{
		r= new ExprParens();
	}
	r->pos=pos;
	r->argls.take_from(nodes);
	return r;
}

ExprBlock* parse_block_sub(ExprBlock* node, TokenStream& src,Expr* insert,int close,int delim, Expr* outer_op) {
	node->pos=src.pos;
	node->call_expr=outer_op;
	if (!g_pRoot) g_pRoot=node;
	verify(node->type());
	int delim_used=0;

	parse_block_nodes(&node->argls,&delim_used, src,insert,close,delim);
	node->call_expr=outer_op;
	
	if (delim_used==COMMA && !outer_op && close==CLOSE_BRACE)
		node->create_anon_struct_initializer();

	verify(node->type());
	return node;
}

void parse_block_nodes(ExprLs nodes, int* delim_used, TokenStream& src,Expr* insert,int close,int delim) {
	Vec<SrcOp> operators;
	Vec<Expr*> operands;
	bool	was_operand=false;
	int wrong_delim=delim==SEMICOLON?COMMA:SEMICOLON;
	int wrong_close=close==CLOSE_PAREN?CLOSE_BRACE:CLOSE_PAREN;
	if (insert){operands.push(insert);was_operand=true;}
	while (true) {
		auto pos=src.pos;
		if (src.peek_tok()==0) break;
		if (src.peek_tok()==IN) break;
		// parsing a single expression TODO split this into 'parse expr()', 'parse_compound'
		if (close || delim) { // compound expression mode.
			if (src.eat_if(close))
				break;
			if (src.eat_if(wrong_close)) {
				src.error("unexpected %s, expected %s",getString(close),getString(wrong_close));
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
		else if (src.eat_if(LIFETIME)){	// just discard lifetimes for the timebeing, but we need to cache them under ptrs.
			src.eat_ident();
			continue;
		}
		// todo ... datatable to associate ID with parse function
		else if (src.eat_if(LET)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_let(src));
		}
		else if (src.eat_if(STRUCT)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_struct(src));
		}
		else if (src.eat_if(TRAIT)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_trait(src));
		}
		else if (src.eat_if(IMPL)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_impl(src));
		}
		else if (src.eat_if(ENUM)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_enum(src));
		}
		else if (src.eat_if(MATCH)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_match(src));
		}
		else if (src.eat_if(EXTERN)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			/// TODO local functions should be sugar for rolling a closure bound to a variable.
			auto abi=src.eat_if_string();
			auto fnp=src.eat_if(FN);
			auto local_fn=parse_fn(src,nullptr);
			if (abi==EXTERN_C) local_fn->c_linkage=true;
			operands.push_back(local_fn);
		}
		else if (src.eat_if(FN)) {
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			/// TODO local functions should be sugar for rolling a closure bound to a variable.
			auto local_fn=parse_fn(src,nullptr);
			operands.push_back(local_fn);
		}
		else if (src.eat_if(DO)){
			// simple syntax sugar- do notation,parser hack
			flatten_stack(operators,operands);
			expect(src,operands.size()>=1,"need preceeding expression eg 'expr(..) do x{..}");
			auto fndef=parse_fn_args( src, OPEN_BRACE);
			fndef->body=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
			auto prev=operands.back();
			prev->as_block()->argls.push_back(fndef);
			flush_op_stack(nodes,operators,operands);
			was_operand=false;
		}
		else if (src.eat_if(DOUBLE_COLON)||src.peek_tok()==OPEN_TYPARAM){ // eg array::<int,5>
			auto open_tp=src.eat_if(LT,OPEN_BRACKET,OPEN_TYPARAM);
			if (open_tp && was_operand){
				auto id=pop(operands)->as_ident();
				if (!id){src.error("::<tparams> must follow identifier");}
				auto itw=parse_tparams_for_ident(src,id,close_of(open_tp));
				operands.push_back(itw);
			} else {
				src.error("::<tparams> must follow identifier");
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
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_for(src));
		}
		else if (src.eat_if(IF)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_if(src));
		}
		else if (auto t=src.eat_if(BREAK,RETURN,CONTINUE)){
			another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
			operands.push_back(parse_flow(src,t));
		}
		else if (src.eat_if(OPEN_PAREN)) {
			if (was_operand){
				operands.push_back(parse_block_sub(new ExprCall(), src, nullptr,CLOSE_PAREN,SEMICOLON, pop(operands)));
				// call result is operand
			}
			else {
				// subexpression-or-tuple
				operands.push_back(parse_subexpr_or_tuple(src)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACKET)){
			if (was_operand){
				operands.push_back(parse_block_sub(new ExprSubscript(), src,nullptr,CLOSE_BRACKET,COMMA,operands.pop()));
			} else {
				src.error("TODO: array initializer");
				operands.push_back(parse_block_sub(new ExprArrayInit,src,nullptr,CLOSE_PAREN,COMMA,nullptr)); // just a subexpression
				was_operand=true;
			}
		} else if (src.eat_if(OPEN_BRACE)){
			//			error(operands.back()?operands.back():node,"struct initializer");
			if (was_operand){// struct initializer
				auto sname=operands.pop_back();
				auto si=parse_block_sub(new ExprStructInit(), src,nullptr,CLOSE_BRACE,COMMA,sname);
				operands.push_back(si);
				si->set_type(sname->get_type()); //eg ident might have tparams.struct init uses given type
				dbg(operands.back()->dump(0));
			}
			else{//progn aka scope block with return value.
				auto sub=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
				operands.push_back(sub);
			}
		} else if (src.eat_if(delim)) {
			flush_op_stack(nodes,operators,operands);
			*delim_used=delim;
			was_operand=false;
		}
		else if (src.eat_if(wrong_delim) && delim){ //allows ,,;,,;,,  TODO. more precise.
			*delim_used=wrong_delim;
			flush_op_stack(nodes,operators,operands);// keep going
			was_operand=false;
		}
		else{
			auto tok=src.eat_tok();
			if (!was_operand && tok==OR){
				src.begin_lambda_bar_arglist();
				another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
				operands.push_back(parse_closure(src,OR));
			} else
				if (is_operator(tok)) {
					// struct initializer
					if (close==CLOSE_BRACE && delim==COMMA){// todo field-init operator
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
					another_operand_so_maybe_flush(was_operand,nodes,operators,operands);
					operands.push_back(new ExprIdent(tok,pos));
				}
		}
		//ASSERT(sub);
		//node->argls.push_back(sub);
	};
	if (operands.size()){
		// final expression is also returnvalue,
		flush_op_stack(nodes,operators,operands);
	} else if (((close==CLOSE_BRACE && delim==SEMICOLON)) &&
			   close!=OPEN_BRACE&& close!=FAT_ARROW)
	{
		nodes->push_back(new ExprLiteral(src.prev_pos));
	}
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
		src.error("error parsing literal\n");
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
		// prefixes in typegrammar - pointers &more
		if (tok==OPTION){
			ret=new Type(owner,OPTION);
			ret->sub=parse_type(src,close,owner);
		}
		else if (tok==COMPLEMENT){ //~str= ~[T]=vector [float*4] array [T]=slice [K:V]=dictionary
			// todo, user def types remapped according to typedefs
			auto x=parse_type(src,close,owner);
			if (x->name==SLICE){
				x->name=VECTOR;
			} else if (x->name==STR ){
				x->name=STRING;
			} else{
				x=new Type(x, UNIQUE_PTR,x);
			}
			ret=x;
		}
		else if (tok==OPEN_BRACKET){
			auto x=parse_type(src,0,owner);
			if (src.eat_if(COLON)){
				auto y=parse_type(src,0,owner);
				ret=new Type(x,DICTIONARY,x,y);
			} else if (src.eat_if(MUL)){
				auto y=parse_type(src,0,owner);
				ret=new Type(x,ARRAY,x,y);
				// calculated expression would have to be parenthesized.
			} else {
				ret=new Type(x,SLICE,x);
			}
			src.expect(CLOSE_BRACKET);
		}
		else if (tok==MUL){
			ret=new Type(owner,PTR);
			ret->sub=parse_type(src,close,owner);
		}
		else if (tok==LOG_AND) {
			ret=new Type(owner,RVALUE_REF);
			ret->sub=parse_type(src,close,owner);
		}else if (tok==AND) {
			ret=new Type(owner,REF);
			if (src.eat_if(LIFETIME)){
				src.eat_ident();
			}
			ret->sub=parse_type(src,close,owner);
		}else {
			// main: something[tparams]..
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
// example syntaxes
// (x,y,z):(int,int,int)=(1,2,3)
// (x,y,z):(int,int,int)
// foo:int
// todo:
vector<Pattern*> g_leak_hack;
ArgDef* parse_arg_or_self(int index,TokenStream& src, Type* self_t, int close) {
	// temporary , translate the self type here for rust-like behaviour,
	// until we've made the rest of it handle self; self should be like a typeparam
	if (src.peek_tok()==close){src.eat_tok();return nullptr;}
	auto ptn=parse_pattern(src,COLON,COMMA,close);
	auto argname=orelse(ptn->as_just_name(),getNumberIndex(index));
	//auto argname=(src.peek_tok()==COLON)?PLACEHOLDER:src.eat_ident();
	if (argname==close) return nullptr;
	auto a=new ArgDef(src.pos,argname);
	a->pattern=ptn;
	a->pos=src.pos;
	if (src.eat_if(COLON)) {
		a->set_type(parse_type(src,close,a));
	}
	if (src.eat_if(ASSIGN)){
		dbg(dbprintf("default expr needs cleanup- move to not consuming close"));
		a->default_expr=parse_expr(src);
	}
	if (self_t && (a->pattern->name==RVALUE_REF ||a->pattern->name==REF || a->pattern->name==PTR) && a->pattern->sub->name==SELF){
		a->name= THIS;
		g_leak_hack.push_back(a->pattern);
		a->pattern=nullptr;
		//a->pattern=new Pattern(src.prev_pos,THIS);
		a->set_type(new Type(a, PTR, (Type*)(self_t->clone())));
	}
	return a;
}
ArgDef* parse_arg(int index,TokenStream& src, int close) {
	return parse_arg_or_self(index,src,nullptr,close);
}
// for tuple struct. makes a mess of our idea for scala style constructpr
// unless we can find a way to build the type..
// need 2 token lookahead
ArgDef* parse_arg_anon(TokenStream& src, int close) {
	auto a=new ArgDef(src.pos,0);
	a->pos=src.pos;
	a->type()=parse_type(src,close,a);
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
	// where block following struct def for default constructor..
	if (src.eat_if(WHERE)){
		src.expect(OPEN_BRACE);
		sd->body=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
	}
	return sd;
}
// TODO: are struct,trait,enum actually all the same thing with a different 'default'
ExprStructDef* parse_tuple_struct_body_sub(TokenStream& src, ExprStructDef* sd){
	Name tok;
	int i=0;
	while ((tok=src.peek_tok())!=NO_TOK){
		if (tok==CLOSE_PAREN){src.eat_tok(); break;}
		sd->fields.push_back(new ArgDef(src.prev_pos,getNumberIndex(i),parse_type(src,0,sd)));
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
		i++;
	}
	dbg(printf("parsed tuple struct\n"));
	dbg(sd->dump(0));dbg(newline(0));
	return sd;
}

ExprStructDef* parse_struct_body(TokenStream& src,SrcPos pos,Name name, Type* force_inherit){
	auto sd=new ExprStructDef(pos,name);
	// todo, namespace it FFS.
	vector<ArgDef*> args;
	if (src.eat_if(OPEN_PAREN)){ // constructor args eg struct Foo(x,y,z){field1=x+y,..}
		int i=0;
		while (!src.eat_if(CLOSE_PAREN)){
			args.push_back(parse_arg(i++,src,0));
			src.eat_if(COMMA);
		}
	}
	for (auto x:args)x->dump(0);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,sd->tparams,close_of(open));
	}
	if (src.eat_if(COLON)) {
		sd->inherits_type = parse_type(src,0,nullptr); // inherited base has tparams. only single-inheritance allowed. its essentially an anonymous field
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
	int f_index=0;
	while ((tok=src.peek_tok())!=NO_TOK){
		// allow writing struct Foo{ .bar:int, .baz:int} for easy search
		if (tok==DOT){ src.eat_tok();tok=src.peek_tok();}
		if (tok==CLOSE_BRACE){src.eat_tok(); break;}
		if (src.eat_if(STRUCT)) {
			sd->structs.push_back(parse_struct(src));
		} else if (auto cmd=src.eat_if(FN)){
			sd->functions.push_back(parse_fn(src,sd));
		} else if (auto cmd=src.eat_if(VIRTUAL)){
			if (sd->inherits_type){
				error(sd,"limited vtables - currently this can only describe the vtable layout in the base class.\nTODO - this is just a temporary simplification,  other priorities eg rust-style traits, ADTs, static-virtuals, reflection..\n");
			}
			sd->virtual_functions.push_back(parse_fn(src,sd,nullptr, true));
		} else if (auto cmd=src.eat_if(STATIC)){
			auto arg=parse_arg(f_index++,src,CLOSE_BRACE);
			if (src.eat_if(VIRTUAL)){
				sd->static_virtual.push_back(arg);
			} else sd->static_fields.push_back(arg);
		} else {
			auto arg=parse_arg(f_index++,src,CLOSE_BRACE);
			sd->fields.push_back(arg);
		}
		src.eat_if(COMMA); src.eat_if(SEMICOLON);
	}
	// if there's any virtual functions, stuff a vtable pointer in the start
	return sd;
}

// trait: describes a vtable layout
TraitDef* parse_trait(TokenStream& src) {
	auto pos=src.pos;
	auto sname=src.eat_ident();

	auto td= new TraitDef(src.prev_pos, sname);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,td->tparams,close_of(open));
	}
	// todo - parse trait inheritance here
	src.expect(OPEN_BRACE);
	// where block following struct def for default constructor..
	while (src.peek_tok()!=CLOSE_BRACE){
		dbg(printf("%s\n",str(src.peek_tok())););
		if (src.eat_if(FN)){
			td->virtual_functions.push_back(parse_fn(src,nullptr));
		} else if (src.eat_if(TYPE)){
			td->typedefs.push_back(parse_typedef(src));
		}
		src.eat_if(SEMICOLON);
	}
	src.eat_tok();
	return td;
}

// impl: shortcut for declaring a bunch of functions with the same receiver.
ImplDef* parse_impl(TokenStream& src) {
	auto pos=src.pos;
	auto imp= new ImplDef(src.prev_pos,0);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)){
		parse_typeparams_def(src,imp->tparams,close_of(open));
	}
	auto t1=parse_type(src,0,0);
	if (src.eat_if(FOR)){			// impl Trait for Type
		auto t2=parse_type(src,0,0);
		imp->impl_trait=t1;
		imp->impl_for_type = t2;
	} else if (src.eat_if(COLON)){	// impl Type : Trait
		auto t2=parse_type(src,0,0);
		imp->impl_trait=t2;
		imp->impl_for_type = t1;
	} else {						// impl Type { ...}
		imp->impl_for_type=t1;
	}
	src.expect(OPEN_BRACE);
	// where block following struct def for default constructor..
	while (src.peek_tok()!=CLOSE_BRACE){
		if (src.eat_if(FN)){
			imp->functions.push_back(parse_fn(src,nullptr,imp->impl_for_type));
		} else if (src.eat_if(TYPE)){
			imp->typedefs.push_back(parse_typedef(src));
		}
		src.eat_if(SEMICOLON);
	}
	src.eat_tok();
	return imp;
}


ExprStructDef* parse_tuple_struct_body(TokenStream& src, SrcPos pos, Name name){
	Name tok;
	auto sd=new ExprStructDef(pos,name);
	if (auto open=src.eat_if(OPEN_BRACKET,LT)) {
		parse_typeparams_def(src,sd->tparams,close_of(open));
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
		parse_typeparams_def(src,ed->tparams,close_of(open));
	}
	if (src.eat_if(COLON)) {
		ed->inherits_type = parse_type(src,0,ed); // inherited base has tparams. only single-inheritance allowed. its essentially an anonymous field
	}
	if (!src.eat_if(OPEN_BRACE))
		return ed;
	// todo: type-params.
	int index=0;		// TODO: computed discriminants; it will have to be subindex+expression
	while ((tok=src.eat_tok())!=NO_TOK){
		auto subpos=src.pos;
		if (tok==CLOSE_BRACE){break;}
		// got an ident, now what definition follows.. =value, {fields}, (types), ..
		auto peektok=src.peek_tok();
		if (peektok==OPEN_BRACE){
			auto sd=parse_struct_body(src,subpos,tok,nullptr);
			sd->set_variant_of(ed,index++);
			ed->structs.push_back(sd);
			
		} else if (peektok==OPEN_PAREN){
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

// 3 cases
// for ;condition;increment{body}
// for pattern in expression {body }
// for pattern=expression,..; condition; increment {body}

ExprFor* parse_for(TokenStream& src){
	ExprFor* p=nullptr;
	Expr* first=nullptr;
	Pattern *ptn=nullptr;
	if (src.peek_tok()!=SEMICOLON){
		ptn=parse_pattern(src, IN,SEMICOLON,OPEN_BRACE);
	}
	if (src.eat_if(IN)){
		auto fi=new ExprForIn(src.pos);
		fi->pattern=ptn;
		fi->expr=parse_block(src, OPEN_BRACE, 0, 0);
//		src.expect(OPEN_BRACE,"eg for x..in..{}");
		p=fi;
	} else {//if (src.eat_if(SEMICOLON)){// cfor.  for init;condition;incr{body}
		// continue parsing initializer expression
		auto ef=new ExprFor(src.pos);
		first=parse_block_sub(new ExprBlock(src.pos),src,(Expr*)ptn,SEMICOLON,COMMA,nullptr);
		
		ef->init=first;
		ef->cond=parse_block(src,SEMICOLON,COMMA,0);
		//ssrc.expect(SEMICOLON,"eg for init;cond;inc{..}");
		ef->incr=parse_block(src,OPEN_BRACE,COMMA,0);
		p=ef;
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
Expr* parse_if(TokenStream& src){
	// TODO: assignments inside the 'if ..' should be in-scope
	// eg if (result,err)=do_something(),err==ok {....}  else {...}
	if (src.eat_if(LET)){
		return parse_if_let(src);
	}
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
	return (Expr*)p;
}
Expr* parse_if_let(TokenStream& src){
	auto iflet=new ExprIfLet;
	MatchArm* arm=new MatchArm(); // if-let is a single match arm with different syntax.
	iflet->arms=arm;
	arm->pattern=parse_pattern(src,ASSIGN,0,0);
	src.expect(ASSIGN);
	iflet->expr=parse_block(src,OPEN_BRACE,COMMA,nullptr);
	arm->body = parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
	// 'else' if effectively an arm with _=>{}
	if (src.eat_if(ELSE)){
		auto else_arm=new MatchArm();
		else_arm->pattern=new Pattern(src.pos,PLACEHOLDER);
		src.expect(OPEN_BRACE);
		else_arm->body=parse_block(src,CLOSE_BRACE,SEMICOLON,nullptr);
		arm->next=else_arm;
	}
	return (Expr*) iflet;
}




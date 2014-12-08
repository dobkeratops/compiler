#include "hack.h"
#include "parser.h"

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
		error(0,"\nerror: %s arity %d, %lu operands given\n",str(op.op),arity(op.op),operands.size());
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

ExprBlock* parse_block(TokenStream&src,int close,int delim, Expr* op);

/// parse_expr - parse a single expression
/// TODO - refactor 'parse_block', this is backwards!
Expr* parse_expr(TokenStream&src) {
	return parse_block(src,0,0,nullptr);
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
	m->expr=parse_expr(src);
	src.expect(OPEN_BRACE);
	MatchArm** pp=&m->arms;
	while (src.peek_tok()!=CLOSE_BRACE){
		auto a=new MatchArm();
		a->pos=src.pos;
		*pp=a;
		pp=&a->next;
		a->pattern=parse_pattern(src,FAT_ARROW,0);
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
Pattern* parse_pattern(TokenStream& src,int close,int close2=0){
	Pattern* p=new Pattern(); p->pos=src.pos;auto first=p;p->name=0;
	while (auto t=src.eat_tok()){
		if (t==close || t==close2) break;
		if (t==OPEN_PAREN){
			if (!p->name){
				// tuple..
				p->name=TUPLE;
			}
			p->sub=parse_pattern(src,CLOSE_PAREN,0);
		}
		// todo - range ".."
		// todo - slice patterns
		else if (t==LET_ASSIGN || t==ASSIGN_COLON || t== ASSIGN ||t==PATTERN_BIND){ // todo its @ in scala,rust
			auto np=new Pattern;
			np->name=PATTERN_BIND;
			np->sub = p;
			first=np; // assert only once
		}
		else if (t==COMMA){
			p=p->next=new Pattern();
		} else if (t==OR && close!=CLOSE_PAREN){ // todo - bit more elaborate. should be ANY(....)  distinct from TUPLE(...)
			p=p->next=new Pattern();
		} else{
			p->name=t;
		}
	}
	return first;
}


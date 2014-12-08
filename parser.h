#pragma once
#include "lexer.h"
struct SrcOp{ Name op; SrcPos pos;};
template<typename T>
T pop(std::vector<T>& v){ ASSERT(v.size()>0);auto r=v[v.size()-1];/*move?*/ v.pop_back(); return r;}
void dump(vector<Expr*>& v);

// todo: plugin arch? Node::parse(), dispatch on registered keywords?
Expr* parse_lisp(TokenStream& src);
ExprFnDef* parse_fn(TokenStream&src,ExprStructDef* owner);	// eg fn foo()
ExprFnDef* parse_closure(TokenStream&src);//eg |x|
ExprFor* parse_for(TokenStream&src);
ExprIf* parse_if(TokenStream&src);
TypeDef* parse_typedef(TokenStream&src);
Type* parse_type(TokenStream& src, int close,Node* owner);
ExprStructDef* parse_struct(TokenStream& src);
ExprStructDef* parse_enum(TokenStream& src);
ExprMatch* parse_match(TokenStream& src);
ExprLiteral* parse_literal(TokenStream& src);
ExprOp* parse_flow(TokenStream& src,Name flow);
ExprMatch* parse_match(TokenStream& src);
Pattern* parse_pattern(TokenStream& src,int close, int close2);
Expr* parse_match_arm(TokenStream& src);
Expr* parse_expr(TokenStream&src);
void another_operand_so_maybe_flush(bool& was_operand, ExprBlock* node,
									vector<SrcOp>& operators,
									vector<Expr*>& operands
									);
void flush_op_stack(ExprBlock* block, vector<SrcOp>& ops,vector<Expr*>& vals);
void pop_operator_call( vector<SrcOp>& operators,vector<Expr*>& operands);

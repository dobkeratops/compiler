#pragma once
//struct TypeDef;
struct IdentWithTParams;
struct Pattern;
#include "exprflow.h"
#include "exprblock.h"
#include "exprfndef.h"
#include "exprstructdef.h"
#include "error.h"
#include "lexer.h"

struct ImplDef;
struct TypeDef;
struct SrcOp{ Name op; SrcPos pos;};
void dump(MyVec<Expr*>& v);
extern Node* g_pRoot;	// temporary hack
// todo: plugin arch? Node::parse(), dispatch on registered keywords?
ExprBlock*	parse_block(TokenStream& src,int close,int delim, Expr* op);
Expr*		parse_expr(TokenStream&src,int close=0);
Type*		parse_type(TokenStream& src, int close,Node* owner);
ExprFnDef*	parse_fn(TokenStream&src,ExprStructDef* owner,Type* self_t=nullptr, bool is_virtual=false);	// eg fn foo()
ExprFnDef*	parse_closure(TokenStream&src,int close);//eg |x|{expr..} (x)->{}
ExprFor*	parse_for(TokenStream&src);
Expr*		parse_if(TokenStream&src);
Expr*		parse_if_let(TokenStream&src);
TypeDef*	parse_typedef(TokenStream&src);
ExprOp*		parse_flow(TokenStream& src,Name flow);
ExprOp*		parse_let(TokenStream& src);
Expr* parse_mod(TokenStream& src);
Expr* parse_use(TokenStream& src);
Expr*		parse_match_arm(TokenStream& src);
ArgDef*		parse_arg(int index,TokenStream& src, int close);
ArgDef* parse_arg_or_self(int index,TokenStream& src, Type* self_t, int close);
void		parse_typeparams_def(TokenStream& src,MyVec<TParamDef*>& out,int close);
void		parse_typeparams_given(TokenStream& src, Type* add_here, int close);
IdentWithTParams*		parse_tparams_for_ident(TokenStream& src,ExprIdent* id,int close);
ExprStructDef*	parse_struct(TokenStream& src);
TraitDef*		parse_trait(TokenStream& src);
ImplDef*		parse_impl(TokenStream& src);
ExprLiteral*	parse_literal(TokenStream& src);
/// TODO - rust features..
Pattern*	parse_pattern(TokenStream& src,int close, int close2=0,int close3=0,Pattern* owner=0);
EnumDef*	parse_enum(TokenStream& src);
TraitDef*	parse_trait(TokenStream& src);
ExprMatch*	parse_match(TokenStream& src);

typedef Vec<Expr*>* ExprLs;
void parse_block_nodes(ExprLs nodes,int* delim_used, TokenStream& src,Expr* insert,int close,int delim);

void another_operand_so_maybe_flush(bool& was_operand, ExprLs nodes,
									MyVec<SrcOp>& operators,
									MyVec<Expr*>& operands
									);
void flush_op_stack(ExprLs nodes, MyVec<SrcOp>& ops,MyVec<Expr*>& vals);
void pop_operator_call( MyVec<SrcOp>& operators,MyVec<Expr*>& operands);

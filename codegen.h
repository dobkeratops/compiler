#pragma once
// TODO : this shouldn't need the whole AST prototypes, just a few key items.
#include "compiler.h"

// Describes interface to codegen(implemented by codegen_llvm);
// codegen.cpp contains AST node 'compile' methods

void output_code(FILE* outfile, Scope* scope,int depth=0);
void name_mangle(char* buffer, int size, const ExprFnDef* f);
void name_mangle(char* buffer, int size, const ExprStructDef* f);
void name_mangle(char* buffer, int size, const Type* f);
void name_mangle_append_scope(char* buffer, int size, const Scope* s);
char* name_mangle_append_name(char* buffer, int size, Name n);
Name next_reg_name(int *next_reg_index);
Name next_reg_name(Name prefix_name, int *next_reg_index);


class CodeGen;
class CgValue;
struct ExprFnDef;
enum EmitFnMode {EmitDefinition,EmitDeclaration,EmitType};
struct Type; struct ExprFnDef;
void commit_capture_vars_to_stack(CodeGen& cg, Capture* cp);
CgValue	CgValueVoid();


struct CgValue {	// lazy-access abstraction for value-or-ref. So we can do a.m=v or v=a.m. One is a load, the other is a store. it may or may not load/store either side of the instruction. a 'variable' is included here as a form of 'adress', for var+= ...
	// TODO: this should be a tagged-union?
	// these values aren't persistent so it doesn't matter too much.
	RegisterName reg;
	int elem=-1;     // if its a struct-in-reg
	Type* type;
	RegisterName addr;
	Node*	val;		// which AST node it corresponds to
	int ofs;
	explicit CgValue(RegisterName n,const Type* t):reg(n),type(const_cast<Type*>(t)){elem=-1;addr=0;ofs=0;val=0;}
	explicit CgValue(RegisterName n,Type* t):reg(n),type(t){elem=-1;addr=0;ofs=0;val=0;}
	explicit CgValue(RegisterName v,Type* t,RegisterName address_reg,int elem_index=-1):reg(v){elem=elem_index;reg=v;addr=address_reg; type=t;ofs=0;val=0;}
	explicit CgValue(Node* n);
	CgValue():reg(0),addr(0),ofs(0),val(0),type(nullptr){};
	bool is_struct_elem()const{return elem>=0;}
	bool is_valid()const{if (val) if (val->type()->name==VOID) return false;return val!=0||reg!=0||addr!=0;}
	bool is_literal()const{return dynamic_cast<ExprLiteral*>(val)!=0;}
	bool is_reg()const { return reg!=0;}
	bool is_any()const{return is_literal()||is_reg();}
	bool is_addr() const {return reg==0 && val==0;}
	CgValue addr_op(CodeGen& cg,Type* t);
	CgValue deref_op(CodeGen& cg, Type* t);
	inline CgValue to_rvalue(CodeGen& cg)const;
	inline CgValue load(CodeGen& cg)const;
	inline CgValue store(CodeGen& cg) const;
	inline CgValue store(CodeGen& cg,const CgValue& src) const;
	CgValue get_elem(CodeGen& cg,const Node* field_name,Scope* sc)const;
	CgValue get_elem_index(CodeGen& cg, int field_index,Type *field_type=0) const;
	CgValue index(RegisterName index);
};


class CodeGen {
	/// CodeGen interface, decouples compile() methods from back end details
	/// at the same time, expressing codegen in terms of an interface
	/// keeps the ast from needing desugaring.
	/// - compile methods can deal this details of hidden pointers etc.
	///
	/// TODO adapt interface till we can handle CodeGenLLVM CodeGenCPP
	/// todo: move the external interface entirely to use wrapped CgValues,pass name hints for outputs
	/// codegen can depend on a subset of the ast concepts . Type,..
	/// TODO: we might want CodeGen to actually know about C's for & C If-Then-Else
	///   eg emit_forloop(Node* init,Node* cond,Node* incr, Node* body)// defer..
	///   this might increase boilerplate for LLVM case, but facilitate generating readable C
	/// for phi nodes we might need to rework AST to know about SSA, see the current awful hack for 'for'
public:
	typedef  Name JumpLabel;
	FILE*	ofp;
	bool	prelude_done=false;
	int		m_next_reg;
	Type*	m_i8ptr=0;
	Type*	m_ptr=0;
	int		m_struct_align=0;
	char	comma;
	int		depth;
	bool	commas[32];
	int call_depth=0;
	CgValue return_reg[32];

	int flow_depth=0;
	JumpLabel	flow_break_to[32];
	JumpLabel	flow_continue_to[32];
	static CgValue 	flow_result[32]; // hack till we move stupid header
	ExprFnDef*	curr_fn;	// The current function being compiled - no nesting allowed. (defer with 'compile_later')
	
	CodeGen(FILE* _ofp, int nr){
		ofp=_ofp; m_next_reg=nr;
		comma=0;
		depth=0;
		curr_fn=0;
	}
	vector<Node*> compile_later;
	typedef  int EmitLoc;
	RegisterName next_reg();
	RegisterName next_reg(Name name);
	/// TODO: wrapper functions for every instruction codegen needs
	/// so single calls to codegen can emit different back ends (LLVM, C, ..)
	Type* ptr_to(Type* other);
	void emit_prelude();
	void emit_struct_name(RegisterName dst );
	void emit_reg(RegisterName dst );
	void emit_ins_end();
	const char* size_t_str(){return "i64";}
	void emit_txt(const char* str,...);
	void emit_typename(Name n ,bool is_ref=false);
	void emit_array_type(const Type* t, int count, bool ref=false);
	void emit_type(CgValue& lv );
	void emit_type(const Type* t, bool is_ref=false); // these would prefer to be
	//extentions, dont want 'CodeGen' dependant on the AST.
	void emit_type_reg(const Type* t,bool ref, Name reg);
	void emit_function_type(ExprFnDef* fn_node);
	void emit_function_type(const Type* t,bool variadic=false);
	void emit_global(Name n);
	void emit_fn_cast_global(Name n,const Type* srct,const Type* dstt);
	void emit_ins_begin_sub();
	void emit_undef();
	CgValue emit_alloca_type(Expr* holder, Type* t);
	RegisterName  emit_ins_begin(RegisterName reg, const char* op);
	void emit_ins_name(const char* txt);
	void emit_ins_begin_name(const char* txt);
	void emit_comma();
	void emit_nest_begin(const char* str);
	void emit_nest_end(const char* str);
	void emit_args_begin();
	void emit_args_end();
	void emit_struct_def_begin(Name n);// eg struct n {...}
	void emit_struct_def_end();
	void emit_struct_begin(int align=0);//for use in operands
	void emit_struct_end();
	void emit_struct_ptr(Name n);
	void emit_global_begin(Name n);
	void emit_pointer_begin();
	void emit_pointer_end();
	void emit_phi_reg_label(Name reg, Name label);
	JumpLabel	gen_label(const char* name,int index=0);
	void emit_instruction_sub(Name opname,Type* type,  RegisterName dstr,CgValue src1);
	CgValue emit_instruction(Name opname,Type* type,  Name outname,CgValue src1);
	CgValue emit_instruction(Name opname,Type* type,  Name outname,CgValue src1,CgValue src2);
	CgValue emit_instruction_reg_i32(Name opname,Type* type,  Name outname,CgValue src1,int val);
	void emit_separator(const char* txt);
	void emit_i32_lit(int index);
	void emit_int_lit(Name type, int value);
	void emit_i32_reg(Name reg);
	void emit_operand_literal(const CgValue& cg,const ExprLiteral* lit);
	CgValue emit_make_literal(ExprLiteral* l);
	RegisterName	emit_extractvalue(RegisterName dst,Type* type,RegisterName src,int index);
	CgValue	emit_extractvalue(CgValue& src , int index);
	CgValue emit_store(RegisterName reg, Type* type, RegisterName addr);
	CgValue 		emit_store_global(CgValue dst, Name globalvar);
	void emit_alloca_array_type(Name n, Type* t, Name count,int align);

	// lazy load/store of abstract CgValue (ref or register)
	CgValue store(const CgValue& dst, const CgValue& src);//{return dst.store(*this,src);};
	CgValue store(const CgValue& dst);//{return dst.store(*this);};
	void	emit_operand(const CgValue& val);
	void emit_fn_ptr(Name n);
	void emit_fn(Name n);
	void emit_comment(const char* str,...);
	void emit_type_operand(const CgValue& src);
	void emit_label(Name l);
	void emit_branch( Name l);
	void emit_return(CgValue val);
	void emit_branch(CgValue cond, Name label_then, Name label_else);
	CgValue emit_cast_raw(const CgValue&lhsv, const Type* rhse);
	CgValue emit_cast(const CgValue&lhsv, Expr* rhse);
	CgValue emit_cast_to_type(const CgValue&lhs_val, const Type* rhst);
	CgValue emit_cast_to_i8ptr(const CgValue& val);
	CgValue emit_cast_from_i8ptr(const CgValue& val,const Type* totype);
	CgValue emit_cast_to_i8ptr(RegisterName src,const Type* srctype);
	CgValue emit_cast_from_i8ptr(RegisterName src, const Type* totype, RegisterName dst=0);
	CgValue emit_cast_reg(RegisterName src_reg, const Type*src_type, const Type* dst_type);
	void emit_function_signature(ExprFnDef* fn_node, EmitFnMode mode);
	Type* i8ptr();
	// API refactoring
	CgValue emit_getelementref(const CgValue& src, int i0, int field_index,const Type* elem_t=0);
	inline CgValue emit_getelementval(const CgValue& src, int i0, int field_index,const Type* elem_t=0);
	CgValue	emit_get_array_elem_ref(const CgValue& array, const CgValue& index);
	CgValue emit_getelementref(const CgValue& src, Name n,const Type* elem_t=0);
//	inline CgValue emit_getelementval(const CgValue& src, Name n);
	inline CgValue emit_getelementval(const CgValue& src, Name n,const Type* elem_t=0);
	CgValue emit_getelementref(const CgValue& src, const CgValue& index);
	CgValue emit_loadelement(const CgValue& obj, Name field);
	CgValue emit_storeelement(const CgValue& obj, Name field,const CgValue& data);
	CgValue emit_extract(CgValue src, int index);
	CgValue emit_insert(CgValue src, int index);
	CgValue emit_getelementptr(RegisterName ptr,Type* struct_t,int index, Type* elem_t);
	CgValue emit_assign(const CgValue& dst, const CgValue& src);
	CgValue emit_malloc( Type* t,size_t count);
	CgValue emit_free( CgValue ptr,size_t count);
	CgValue emit_free_array(Type* t, CgValue count);
	CgValue emit_malloc_array( Type* t,CgValue count);
	
	void emit_global_fn_ptr(const Type* t, Name n);
	int emit_global_string_literal(Name n, const char* s);

	// IF,FOR are part of the codegen interface to swap C/LLVM backends
	CgValue emit_for(ExprFor* f,Expr* init,Expr* cond, Expr* incr, Expr* body, Expr* else_block);
	CgValue emit_if(Node* n, Expr* cond, Expr* body, Expr* else_block);
	CgValue emit_if_sub(Node* n, Scope* sc,std::function<CgValue()> f_cond, std::function<CgValue()> f_body, Expr* else_block);
	CgValue emit_break(CgValue v);
	CgValue emit_continue();
	void 	emit_call_begin(const CgValue& call_expr);
	CgValue emit_call_end();
	CgValue emit_call(const CgValue& fnc, const CgValue& arg);
	CgValue emit_call(const CgValue& fnc, const CgValue& arg1,const CgValue& arg2);
	CgValue emit_conversion(const CgValue& src, const Type* to_type, const Scope* sc);

	CgValue load(const CgValue& v,Type* result_type=0);
	CgValue to_rvalue(const CgValue& lvalue_or_rvalue){
		// TODO - check its' an addr.
		return lvalue_or_rvalue.load(*this);
	}

	// helper fn for simple cases eg implementing operator overload calls
	// use this interface to allow emiting readable c

	EmitLoc get_pos(){return ftell(ofp);}
	void set_pos(EmitLoc loc){fseek(ofp,loc,SEEK_SET);}
	void set_pos_end(){fseek(ofp,0,SEEK_END);}
	void emit_free(CgValue ptr,  Type* t,size_t count);
};

struct LoopPhiVar {
	CgValue val;//todo
	Variable*	var;
	RegisterName reg_pre;
	RegisterName reg_start;
	RegisterName reg_end;
};

void emit_phi(CodeGen& cg, Scope* sc, vector<LoopPhiVar>& phi_vars,Name l_pre, Name l_end, bool extra);

inline CgValue CgValue::load(CodeGen& cg)const { return cg.load(*this);}
inline CgValue CgValue::store(CodeGen& cg)const {return cg.store(*this);}
inline CgValue CgValue::store(CodeGen& cg,const CgValue& src) const{return cg.store(*this,src);}
inline CgValue CodeGen::emit_getelementval(const CgValue& src, Name n,const Type* t){
	return emit_getelementref(src, n,t).load(*this);
}
inline CgValue CodeGen::emit_getelementval(const CgValue& src, int ar_i,int field_index,const Type* elem_t){
	return emit_getelementref(src, ar_i, field_index,elem_t).load(*this);
}
inline CgValue CgValue::to_rvalue(CodeGen& cg)const{return cg.to_rvalue(*this);}


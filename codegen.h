#pragma once
#include "hack.h"

void output_code(FILE* outfile, Scope* scope,int depth=0);
void name_mangle(char* buffer, int size, const ExprFnDef* f);
void name_mangle(char* buffer, int size, const ExprStructDef* f);
void name_mangle(char* buffer, int size, const Type* f);
void name_mangle_append_scope(char* buffer, int size, const Scope* s);
char* name_mangle_append_name(char* buffer, int size, Name n);

class CodeGen;
class CgValue;
struct ExprFnDef;
enum EmitFnMode {EmitDefinition,EmitDeclaration,EmitType};
struct Type; struct ExprFnDef;
void commit_capture_vars_to_stack(CodeGen& cg, Capture* cp);
CgValue	CgValueVoid();

class CodeGen {
	/// TODO adapt interface to handle CodeGenLLVM CodeGenC
	/// todo: move the external interface entirely to use wrapped CgValues,pass name hints for outputs
	/// codegen can depend on a subset of the ast concepts . Type,..
	/// we might need to rework AST to know about SSA, see the current awful hack for 'for'
public:
	FILE* ofp;
	bool prelude_done=false;
	int m_next_reg;
	CodeGen(FILE* _ofp, int nr){
		ofp=_ofp; m_next_reg=nr;
		comma=0;
		depth=0;
		curr_fn=0;
	}
	Type* m_i8ptr=0;
	Type* m_ptr=0;
	char comma;
	int depth;
	bool commas[32];
	vector<Node*> compile_later;
	ExprFnDef*	curr_fn;	// The current function being compiled - no nesting allowed. (defer with 'compile_later')
	typedef  int EmitLoc;
	Name next_reg();
	/// TODO: wrapper functions for every instruction codegen needs
	/// so single calls to codegen can emit different back ends (LLVM, C, ..)
	Type* ptr_to(Type* other);
	void emit_prelude();
	void emit_struct_name(RegisterName dst );
	void emit_reg(RegisterName dst );
	void emit_ins_end();
	void emit_txt(const char* str,...);
	void emit_type(CgValue& lv );
	void emit_type(const Type* t, bool is_ref=false); // these would prefer to be
	//extentions, dont want 'CodeGen' dependant on the AST.
	void emit_type_reg(const Type* t,bool ref, Name reg);
	void emit_function_type(ExprFnDef* fn_node);
	void emit_function_type(const Type* t);
	void emit_global(Name n);
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
	void emit_struct_begin();
	void emit_struct_end();
	void emit_struct_ptr(Name n);
	void emit_pointer_begin();
	void emit_pointer_end();
	void emit_phi_reg_label(Name reg, Name label);
	void emit_instruction_sub(Name opname,Type* type,  RegisterName dstr,CgValue src1);
	CgValue emit_instruction(Name opname,Type* type,  Name outname,CgValue src1);
	CgValue emit_instruction(Name opname,Type* type,  Name outname,CgValue src1,CgValue src2);
	CgValue emit_instruction_reg_i32(Name opname,Type* type,  Name outname,CgValue src1,int val);
	void emit_separator(const char* txt);
	void emit_i32_lit(int index);
	void emit_i32_reg(Name reg);
	CgValue emit_literal(ExprLiteral* l);
	RegisterName	emit_extractvalue(RegisterName dst,Type* type,RegisterName src,int index);
	CgValue emit_store(RegisterName reg, Type* type, RegisterName addr);
	void emit_fn_ptr(Name n);
	void emit_fn(Name n);
	void emit_comment(const char* str,...);
	void emit_type_operand(const CgValue& src);
	void emit_label(Name l);
	void emit_branch( Name l);
	void emit_return(CgValue val);
	void emit_branch(CgValue cond, Name label_then, Name label_else);
	CgValue emit_cast_raw(CgValue&lhsv, Type* rhse);
	CgValue emit_cast(CgValue&lhsv, Expr* rhse);
	CgValue emit_cast_sub(CgValue&lhs_val, Type* rhst);
	CgValue emit_cast_to_i8ptr(CgValue& val);
	CgValue emit_cast_from_i8ptr(CgValue& val,Type* totype);
	CgValue emit_cast_to_i8ptr(RegisterName src, Type* srctype);
	CgValue emit_cast_from_i8ptr(RegisterName src, Type* totype, RegisterName dst=0);
	CgValue emit_cast_reg(RegisterName src_reg, Type*src_type, Type* dst_type);
	void emit_function_signature(ExprFnDef* fn_node, EmitFnMode mode);
	Type* i8ptr();
	// API refactoring
	CgValue emit_extract(CgValue src, int index);
	CgValue emit_insert(CgValue src, int index);
	CgValue emit_getelementptr(RegisterName ptr,Type* struct_t,int index, Type* elem_t);
	CgValue emit_malloc( Type* t,size_t count);
	CgValue emit_free( CgValue ptr,size_t count);
	CgValue emit_malloc_array( Type* t,CgValue count);
	EmitLoc get_pos(){return ftell(ofp);}
	void set_pos(EmitLoc loc){fseek(ofp,loc,SEEK_SET);}
	void set_pos_end(){fseek(ofp,0,SEEK_END);}
	void emit_free(CgValue ptr,  Type* t,size_t count);
};
#include "codegen.h"
extern Name getStringIndex(const char* str,const char* end) ;
extern const char* getString(const Name&);
extern bool is_operator(int tok);
extern bool is_ident(int tok);
extern bool is_type(int tok);
void write_reg(FILE* ofp, RegisterName dst );
void write_type(FILE* ofp, const Type* t, bool is_ref=false);
void write_function_type(FILE* ofp,ExprFnDef* fn_node,int* regname);
void write_function_type(FILE* ofp, const Type* t);

// If you just compiled to C..
// this would all work by now.

int next_reg_name(int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d",(*next_reg_index)++);
	return getStringIndex(tmp);
}
int next_reg_name(Name prefix_name, int *next_reg_index){
	char tmp[64]; sprintf(tmp,"r%d%s",(*next_reg_index)++,str(prefix_name));
	return getStringIndex(tmp);
}
void write_reg(FILE* ofp, RegisterName reg);

void write_store(FILE* ofp, RegisterName reg, Type* type, RegisterName addr){
	fprintf(ofp, "\t store ");
	write_type(ofp, type,0);
	write_reg(ofp, reg); fprintf(ofp,",");
	write_type(ofp, type,1);//fprintf(ofp,"* ");
	write_reg(ofp, addr); fprintf(ofp,", align 4\n");
//		fprintf(ofp,"\tstore  %s %%%s, %s* %%%s, align 4\n",get_llvm_type_str(type->name), str(reg),get_llvm_type_str(type->name),str(addr));
}

struct CgValue {	// abstraction for value-or-address. So we can do a.m=v or v=a.m. One is a load, the other is a store. it may or may not load/store either side of the instruction. a 'variable' is included here as a form of 'adress', for var+= ...
	// TODO: t	his should be a tagged-union. we just can't be arsed rolling it manually, thats one of many reasons why we're writing a language
	// these values aren't persistent so it doesn't matter too much.
	ExprLiteral* lit;
	RegisterName reg;
	Type* type;
	RegisterName addr;
	Node*	var;		// TODO: rename value.could be variable(in reg?) or function pointer (just a label of functoin..) TODO unify with literal case.
	int ofs;
	// adress?
	explicit CgValue(ExprLiteral* l):lit(l){reg=0;addr=0;ofs=0;var=0;type=l->get_type();};
//	explicit CgValue(RegisterName n):reg(n){lit=0;addr=0;ofs=0;var=0;type=0;}
	explicit CgValue(RegisterName n,Type* t):reg(n),type(t){lit=0;addr=0;ofs=0;var=0;}
	explicit CgValue(RegisterName v,Type* t,RegisterName address_reg):reg(v){lit=0;reg=v;addr=address_reg; type=t;ofs=0;var=0;}
	explicit CgValue(Variable* v){
		lit=0; addr=0; reg=0; ofs=0;
		var=v;
		if (v->reg_is_addr){
			addr=v->regname;
		}
		else{
			reg=v->regname;
		};
		this->type=v->type();
	}
	explicit CgValue(Expr* n) {
		lit=0; addr=0; reg=0; ofs=0;
		var = n;
		if (auto fd=dynamic_cast<ExprFnDef*>(n)){ // variable is a function pointer?
			reg=0; // it needs to be loaded
		}
		if (auto v=dynamic_cast<Variable*>(n)){
			if (v->reg_is_addr){
				addr=v->regname;
			}
			else{
				reg=v->regname;
			};
		}
		this->type=n->type();
	}
	CgValue():lit(0),reg(0),addr(0),ofs(0),var(0),type(nullptr){};
	bool is_valid()const{return lit!=0||reg!=0;}
	bool is_literal()const{return lit!=0;}
	bool is_reg()const { return reg!=0;}
	bool is_any()const{return is_literal()||is_reg();}
	bool is_addr() const {return reg && addr;}
	CgValue addr_op(Type* t) { // take type calculated by sema
		ASSERT(this->type);
		if (!reg && addr) {	// we were given a *reference*, we make the vlaue the adress
			ASSERT(t->name==PTR);
			ASSERT(t->sub->eq(this->type));
			return CgValue(addr,t,0);
		} else {
			// must return adress-of-adress
			// path must have been flagged as a memvar.
			// or we must have been given array-ref[i], or 'dot' member ref.
			this->type->dump_if(0);
			t->dump_if(0);
			
			ASSERT(0 && "trying to take adress of entity which is in a register. TODO comit such vars to stack");
			return CgValue();
		}
	}
	CgValue deref_op(FILE* ofp, Type* t,int* next_reg_index) {
		// todo type assertion 't' is the output type.
		if (!addr) {		// the value we were given is now the *adress* - we return a reference, not a pointer.
			return CgValue(0,t,reg);
		} else {
			// it its'  a reference type, we need to load that first.
			auto ret=load(ofp,next_reg_index);
			return CgValue(0,t,ret); // ... and return it as another reference: eg given T&* p,  returning T& q=*p
		}
	}
	
	RegisterName load_literal(FILE* ofp,int *next_reg_index,RegisterName force_regname) {
		if(lit) {
			reg=force_regname?force_regname:next_reg_name(next_reg_index);
			if (lit->type()->name==INT){
				fprintf(ofp,"\t%%%s = add i32 0,",str(reg));  this->write_literal(ofp); fprintf(ofp,"\n");
			} else if(lit->type()->name==FLOAT){
				fprintf(ofp,"\t%%%s = fadd float 0.0, ",str(reg));  this->write_literal(ofp); fprintf(ofp,"\n");
			}
			return reg;
		} else return load(ofp,next_reg_index);
	}
	RegisterName load(FILE* ofp,int *next_reg_index) {
		//		return CgValue();
		if (var) {	// we have a reference to a variable - we must load it.
			// TODO - if its' a memvar we really do need to load something.
			if (!reg) {
				reg=var->get_reg(var->name,next_reg_index,false); // TODO is that false, or do we need to force new?
			}
			if (auto fp=dynamic_cast<ExprFnDef*>(var)){
				fprintf(ofp,"\t%%%s = load ", str(reg));
				// function type..
				write_type(ofp,this->type,true);
				fprintf(ofp,"@%s\n",var->name_str());
			}
			return reg;
		}
		if (addr) {
			reg=next_reg_name(next_reg_index);
			return load_into(ofp,reg);
		}
		return reg;
	}
	RegisterName load_into(FILE* ofp, RegisterName req_reg){
		if (var)
		if (auto fp=dynamic_cast<ExprFnDef*>(var)){
			reg=req_reg;
			fprintf(ofp,"\t%%pre%s =alloca ",str(reg));write_function_type(ofp,fp->type());fprintf(ofp,",align 8\n");
			fprintf(ofp,"\tstore ");write_function_type(ofp,fp->type());fprintf(ofp," @%s, \n",fp->name_str());
			write_function_type(ofp,fp->type());fprintf(ofp,"* %%pre%s,align 4",str(reg));
			fprintf(ofp,"\t%%%s = load ",str(reg)); write_function_type(ofp,fp->type()); fprintf(ofp,"* %%pre%s\n",str(reg));
			return reg;
			/*
			fprintf(ofp,"\t%%%s = load ", str(reg));
			// function type..
			write_function_type(ofp,fp->type());
			fprintf(ofp," @%s\n",fp->name_str());
			return reg;
			 */
		}

		if (!addr || !type)
			return reg; // actual reg.

		reg=req_reg;
		fprintf(ofp,"\t%%%s = load ",str(reg));
//				type->is_struct()?"%%":"", type->is_struct()?getString(type->name):get_llvm_type_str(type->name),
		write_type(ofp, type, addr!=0);// an extra pointer level if its' a reference
		fprintf(ofp," %%%s\n",str(addr));
		addr=0; // its no longer an adress.
		return reg;
	}
	void write_literal(FILE* ofp){
		switch (lit->type_id){
		case T_INT:
			fprintf(ofp," %d ",lit->u.val_int);
			break;
		case T_UINT:
			fprintf(ofp," %u ",lit->u.val_uint);
			break;
		case T_FLOAT:
			fprintf(ofp," %f ",lit->u.val_float);
			break;
			//					getelementptr inbounds ([21 x i8]* @.str3, i32 0, i32 0)
		case T_CONST_STRING:
			fprintf(ofp," getelementptr inbounds([%d x i8]* @.%s, i32 0, i32 0) ", lit->llvm_strlen, getString(lit->name));
			break;
		default:
			fprintf(ofp," @TODO_LITERAL_FORMAT %s", getString(lit->name));
			break;
		}
	}
	void write_operand(FILE* ofp){
		if (reg!=0){
			fprintf(ofp,"%%%s ",str(reg));
		} else
		if (lit){ write_literal(ofp);
		}
		else{
			this->type->dump_if(0);
			ASSERT(this->type);
			ASSERT(0 && "missing register type");
			fprintf(ofp," <?CgV?> ");
		}
	}
	RegisterName store(FILE* ofp,int* next_index){// for read-modify-write
		if (!addr || !type) return reg;
//		fprintf(ofp,"\tstore  %s %%%s, %s* %%%s, align 4\n",get_llvm_type_str(type->name), str(reg),get_llvm_type_str(type->name),str(addr));
		write_store(ofp, reg, type, addr);
		return reg;
	}


	RegisterName store_from(FILE* ofp,RegisterName valreg){
		if (var){
			var->regname=valreg;
			reg=valreg;
		}
		else if (addr && type) {
			if (reg!=0){dbprintf("warning %s overwrite %s?\n", str(reg),str(valreg));}
//			ASSERT(reg==0);
			reg=valreg;
//			fprintf(ofp,"\tstore  %s %%%s, %s* %%%s, align 4\n",get_llvm_type_str(type->name), 	str(valreg),get_llvm_type_str(type->name),str(addr));
			write_store(ofp, valreg, type, addr);
		}
		return valreg;
	}
	CgValue dot(FILE* ofp,const Node* field_name,int* next_reg_index,Scope* sc){	// calculates & returns adress
		if (!type){
			fprintf(ofp,";ERROR %s:%d:\n;",__FILE__,__LINE__);return CgValue();
		}
		ASSERT(type );
		auto sd=type->struct_def?type->struct_def:sc->find_struct(type);
//		if (!sd){
//			fprintf(ofp,"\t;%s.%s\n",type?getString(type->name):"???",getString(field_name));
//			type->dump(-1);
//			fprintf(ofp,";ERROR %s:%d:\n;",__FILE__,__LINE__);return CgValue();
//		}
		auto basereg=reg?reg:addr;
		ASSERT(basereg);			// TODO: detail: if it's addr, extra or lesser *?
		int areg=next_reg_name(next_reg_index);
		int index=sd->field_index(field_name);
		auto field=sd->find_field(field_name);
		fprintf(ofp,"\t;%s.%s :%s\n",str(type->name),str(field_name->as_ident()),str(field->type()->name));
		fprintf(ofp,"\t%%%s = getelementptr inbounds %%%s* %%%s, i32 0, i32 %d\n",
				str(areg), str(type->name), str(basereg),
				index);
		
		return CgValue(0,field?field->type():nullptr,areg);
	}
	CgValue index(RegisterName index){ // calculates & returns adress
		return CgValue();
	}
};

void debug_op(int opname) {
	printf("op index=%d; prev=%s;curr=%s;next=%s flags=%x\t",opname,str(opname-1),str(opname),str(opname+1),operator_flags(opname));
}


int reg_of(Node* n, ExprFnDef* owner) {
	char tmp[32];
	if (auto p=dynamic_cast<ExprIdent*>(n)){
		// TODO - check 'this',lambda locals
		return p->name;
	}
//	TODO need to verify that there are NO clashes
// todo-can we just cache this on the node itself?
	auto m=((uint32_t)
			 (size_t)n);
	sprintf(tmp,"n%x", ((m>>2) ^ (m>>18))&0xffff);
	return getStringIndex(tmp,0);
}
void write_reg(FILE* ofp, RegisterName dst ) {
	fprintf(ofp,"%%%s ",str(dst));
}
void write_type(FILE* ofp, const Type* t, bool ref) {
	if (!t) { fprintf(ofp,"<type_expected>");return;}
	if (t->is_pointer()){
		write_type(ofp,t->sub,ref); fprintf(ofp,"*");
	}else if (t->is_array()) {
		fprintf(ofp,"[");
		fprintf(ofp, "%s%s",str(t->sub->next->name),ref?"*":"");
		fprintf(ofp," x ");
		write_type(ofp,t->sub,0); // TODO: assert its a numeric constant
		fprintf(ofp,"]");
	}
	else if (t->is_function()){
		//error(t,"TODO,write function type unified ");
		write_function_type(ofp, t);
	}
	else {
//		dbprintf(";%s is struct %p\n", str(t->name), t->struct_def);
		if (t->is_complex()) {
			fprintf(ofp,"%%%s%s", str(t->name), ref?"*":"");
		}
		else
			fprintf(ofp,"%s%s",t?get_llvm_type_str(t->name):"???",ref?"*":"");//,t.is_pointer?"*":"");
	}
}
//void write_lit_or_reg(FILE* ofp, CgValue x ) {
//	if (x.is_reg())
//		write_reg(ofp,x.reg);
//	else if (x.is_literal()){
//	}
//	else ASSERT(0);
//}

void write_instruction_sub(FILE* ofp, Name opname,Type* type,  CgValue dst,CgValue src1){
//	ASSERT(dst.is_reg());
	const LLVMOp* op=get_op_llvm(opname,type?type->name:VOID);
	fprintf(ofp,"\t"); dst.write_operand(ofp);
	fprintf(ofp,"= %s ", op?op->op_signed:str(opname));
	if (is_comparison(opname))
		write_type(ofp,src1.type, src1.is_addr());
	else{
		write_type(ofp,type,false);
	}
	
	src1.write_operand(ofp);
}
void write_instruction(FILE* ofp, Name opname,Type* type,  CgValue dst,CgValue src1){
	write_instruction_sub(ofp,opname,type,dst,src1);
	fprintf(ofp,"\n");
}
void write_instruction(FILE* ofp, Name opname,Type* type,  CgValue dst,CgValue src1,CgValue src2){
	ASSERT(type!=0);
	write_instruction_sub(ofp,opname,type,dst,src1);
	fprintf(ofp,",");
	src2.write_operand(ofp);
	fprintf(ofp,"\n");
}

void dump_locals(Scope* s){
	for (;s;s=s->parent){
		for (auto v=s->vars; v;v=v->next){
			printf("\t;%s:",str(v->name));v->get_type()->dump(-1); printf("%%%s\n",str(v->regname));
		}
	}
}

CgValue compile_node(FILE* ofp,Expr* n, ExprFnDef* f,Scope*s,RegisterName* new_reg);


void compile_struct_def(FILE* ofp, ExprStructDef* st, Scope* sc) {
	if (st->is_generic()) {	// emit generic struct instances
		fprintf(ofp,";instances of %s\n",str(st->name));
		for (auto ins=st->instances; ins; ins=ins->next_instance){
			compile_struct_def(ofp, ins, sc);
		}
	} else {
		fprintf(ofp,"%%%s = type {", str(st->name));
		// todo: properly wrap translations to LLVM types.
		int i=0; for (auto fi: st->fields){
			if (i++)fprintf(ofp,",");
			ASSERT(fi->type());
			write_type(ofp,fi->type(), false);
		};
		fprintf(ofp," }\n");
	}
}

CgValue alloca_struct(FILE* ofp, Expr* holder, ExprStructDef* sd,int *next_index) {
	auto r= holder->get_reg(sd->name, next_index, false);
	fprintf(ofp,"\t"); write_reg(ofp,r); fprintf(ofp," = alloca %%%s , align %d\n",str(sd->name),sd->alignment());
	return CgValue(r,holder->get_type(), 0);
}

void write_local_vars(FILE* ofp,Expr* n, ExprFnDef* fn, Scope* sc, RegisterName* new_reg) {
	for (auto v=sc->vars; v;v=v->next){
		if (v->kind!=Local) continue;
		auto vt=v->expect_type();
		if (!vt->is_complex())
			continue;//its just a reg
		auto r= v->get_reg(v->name, new_reg, false);
		if (vt->is_struct()) {
			// alloc_struct
			fprintf(ofp,"\t"); write_reg(ofp,r); fprintf(ofp," = alloca %%%s , align %d\n",getString(vt->name),vt->struct_def->alignment());
			v->reg_is_addr=true;
		} else if (vt->is_array()){
			auto t=vt->sub;
			if (!t || !t->next){error(v,"array type needs 2 args");}
			fprintf(ofp,"\t"); write_reg(ofp,r); fprintf(ofp," = alloca [%s x %s] , align %d\n",str(t->next->name),get_llvm_type_str(t->name),vt->alignment());
			v->reg_is_addr=true;
		} else	if (vt->is_pointer() || vt->is_function()){
			continue;
		} else {
			dbprintf("error:\n");
			vt->dump(-1);
			error(n,"typenot handled %s",str(vt->name));
		}
	}
}
Name gen_label(const char* s, int index){
	char tmp[256];sprintf(tmp,"%s%d",s,index); return getStringIndex(tmp);
}
void emit_label(FILE* ofp, Name l){
	fprintf(ofp,"%s:\n",str(l));
}
void emit_branch(FILE* ofp, Name l){
	fprintf(ofp,"\tbr label %%%s\n",str(l));
}
void emit_branch(FILE* ofp, CgValue cond, Name label_then, Name label_else,int* next_index){
	cond.load(ofp,next_index);
	fprintf(ofp,"\tbr i1 %%%s, label %%%s, label %%%s\n",str(cond.reg), str(label_then), str(label_else));
}
CgValue compile_if(FILE* ofp,ExprIf* ifn, ExprFnDef* curr_fn,Scope*sc,RegisterName* next_index){
	// TODO: Collect phi-nodes for anything modified inside.
	RegisterName outname=next_reg_name(next_index);
	auto condition=compile_node(ofp,ifn->cond,curr_fn,sc,next_index);
	int index=(*next_index)++;
	auto label_if=gen_label("if",index);
	auto label_endif=gen_label("endif",index);
	if (ifn->else_block){
		auto label_else=gen_label("else",index);
		emit_branch(ofp,condition,label_if,label_else, next_index);
		emit_label(ofp,label_if);
		auto if_result=compile_node(ofp,ifn->body,curr_fn,sc,next_index);
		if_result.load_literal(ofp,next_index,0);
		emit_branch(ofp,label_endif);
		emit_label(ofp,label_else);
		auto else_result=compile_node(ofp,ifn->else_block,curr_fn,sc,next_index);
		else_result.load_literal(ofp,next_index,0);
		emit_branch(ofp,label_endif);
		emit_label(ofp,label_endif);
		// phi node picks result, conditional assignment
		if (if_result.is_valid() && else_result.is_valid())
			fprintf(ofp,"\t%%%s = phi ",str(outname)); write_type(ofp,if_result.type, if_result.is_addr());
			fprintf(ofp," [%%%s, %%%s], [%%%s, %%%s]\n" ,str(if_result.reg), str(label_if), str(else_result.reg),str(label_else));
		auto return_type=ifn->get_type();
		return CgValue(outname,return_type);
	}
	else {
		emit_branch(ofp,condition,label_if,label_endif, next_index);
		auto ifblock=compile_node(ofp,ifn->body,curr_fn,sc,next_index);
		emit_label(ofp,label_endif);
		// TODO phi node
		return CgValue();
	}
}

struct LoopPhiVar {
	CgValue val;//todo
	Variable*	var;
	RegisterName reg_pre;
	RegisterName reg_start;
	RegisterName reg_end;
};
void write_phi(FILE* ofp, Scope* sc, vector<LoopPhiVar>& phi_vars,Name l_pre, Name l_end, bool extra) {
	for (auto& v: phi_vars) {
		v.reg_end=v.var->regname;
		fprintf(ofp,"\t %%%s = phi ",str(v.reg_start));
		write_type(ofp,v.var->type(), false);//v.val.is_addr());
		fprintf(ofp,"[%%%s, %%%s],",str(v.reg_pre),str(l_pre));
		fprintf(ofp,"[%%%s, %%%s]",str(v.reg_end),str(l_end));
		fprintf(ofp,"\n");
	}
	if (extra) for (auto i=phi_vars.size(); i>0;i--)fprintf(ofp,"       ");	// dirty hack to prevent source overun
	fprintf(ofp,"\n");
}

CgValue compile_for(FILE* ofp, ExprFor* nf, ExprFnDef* curr_fn, Scope* outer_sc, RegisterName* next_index){
//  initializer
// for:
//  test condition br else
//   body
//   (break br endfor)
//   increment
//   br loop
// break:
// else:
// endfor:

	auto sc=nf->scope;
	// write the initializer block first; it sets up variables initial state
	int index=(*next_index)++;
	auto l_init=gen_label("init",index);
	emit_branch(ofp,l_init);
	emit_label(ofp,l_init);
	auto init=compile_node(ofp,nf->init,curr_fn,sc,next_index);
	
	set<Variable*> write_vars;
	set<Variable*> else_vars;
	// Now scan all the blocks to see which are changed.
	if (nf->cond)nf->cond->find_vars_written(sc,write_vars);
	if (nf->body)nf->body->find_vars_written(sc,write_vars);
	if (nf->incr)nf->incr->find_vars_written(sc,write_vars);
	vector<LoopPhiVar> phi_vars;
	for (auto v :write_vars){
		LoopPhiVar phi;
//		phi.val=
		phi.var=v;
		phi.reg_pre=v->regname;
		phi.reg_start=v->regname=next_reg_name(v->name,next_index);
		phi.reg_end=0;//gen_label(str(v->name),next_index);
		//todo: can we allocate regnames in the AST? find last-write?
		phi_vars.push_back(phi);
	}
	auto l_for=gen_label("cond",index);
	auto l_body=gen_label("body",index);
	auto l_else=gen_label("else",index);
	auto l_endfor=gen_label("endfor",index);
	emit_branch(ofp,l_for);
	emit_label(ofp,l_for);
	auto phipos=ftell(ofp);// YUK. any way around this? eg write condition at end?
	write_phi(ofp,sc,phi_vars,l_init,l_for,true);//alloc space

	auto cond_result=nf->cond?compile_node(ofp,nf->cond,curr_fn,sc,next_index):CgValue();
	emit_branch(ofp, cond_result, l_body, l_else, next_index);
	emit_label(ofp,l_body);
	if (nf->body) compile_node(ofp,nf->body,curr_fn,sc,next_index);
	if (nf->incr) compile_node(ofp,nf->incr,curr_fn,sc,next_index);
	emit_branch(ofp,l_for);
	emit_label(ofp,l_else);
	if (nf->else_block) compile_node(ofp,nf->else_block,curr_fn,sc,next_index);
	emit_branch(ofp,l_endfor);
	emit_label(ofp,l_endfor);
	// now write the phi-nodes.
	auto endpos=ftell(ofp);// YUK. any way around this?
	fseek(ofp, phipos,SEEK_SET);
	write_phi(ofp,sc,phi_vars,l_init,l_body,false);
	fseek(ofp, 0,SEEK_END);

	//TODO: return value.
	return CgValue();
}

/*
// Allowing partial parsing
template foo; - says foo is a template, typeparams are expected.  foo<x,y,z>.
typename foo; - says foo is a typename. allows parsing grammar.
'extern foo;' -says foo is *not* a template or typename.
// decoupling syntax from modularity.

Something::{
	void yada();
}

same as
class Something {
	void yada();
 
fn compile_node {
	(ExprFnDef* fnd){
 	},
 	(.....){
 	},
 	(....){
 	},
}
}
*/

CgValue compile_node(FILE *ofp,Expr *n, ExprFnDef *curr_fn,Scope *sc,RegisterName* next_index){
	if (auto e=dynamic_cast<ExprOp*>(n)) {
			auto opname = e->name;
			//			printf("operator? %s %p %p\n",getString(opname),e->call_operator,e->call_target);
			int opflags = operator_flags(opname);
			auto t=e->get_type();//get_type_llvm();
			
			// TODO 2operand form should copy regname for this node from the lhs.
			// TODO - multiple forms:
			//
			// generalize by lvalue being in register or memory.
			// 3-operand; assign-op; assign-op; mem-assign-op;
			if (opname==DOT || opname==ARROW){
				auto lhs=compile_node(ofp,e->lhs,curr_fn,sc,next_index);
				return lhs.dot(ofp,e->rhs,next_index,sc);
			}
			else
				if (e->lhs && e->rhs){
					auto lhs=compile_node(ofp,e->lhs,curr_fn,sc,next_index);
					auto rhs=compile_node(ofp,e->rhs,curr_fn,sc,next_index);
					auto lhs_v=sc->find_variable_rec(e->lhs->name);
					auto outname=lhs_v?lhs_v->name:opname;
					//printf_reg(lhs_v->regname);
					//if (!n->regname && lhs_v){n->regname=lhs_v->regname;} // override if its a write.
					auto dst=CgValue(n->get_reg(outname,next_index,false),n->get_type());
					// assignments :=
					//printf_reg(dst.reg);
					if (opname==ASSIGN_COLON){ // do nothing-it was sema'sjob to create a variable.
						ASSERT(sc->find_scope_variable(e->lhs->name));
						if (lhs_v) dst.reg=lhs_v->regname;
						//					lhs_v->regname=dst.reg;
						return CgValue(0,lhs_v->type(),dst.reg);
					}
					if (opname==LET_ASSIGN){// Let-Assign *must* create a new variable.
						auto v=sc->find_variable_rec(e->lhs->name); WARN(v &&"semantic analysis should have created var");
						auto dst=v->get_reg(v->name, next_index, true);
						if (rhs.is_literal()){//TODO simplify this, how does this case unify?
							fprintf(ofp,";assigning literal to local %s %%%s %%%s %%%s\n", str(v->name),str(dst), str(v->regname),str(n->regname) );
							v->regname=dst;
							rhs.load_literal(ofp,next_index,dst);
							return CgValue(dst,rhs.type);
						}
						dst=rhs.load_into(ofp,dst);
						v->regname=dst; // YUK todo - reallyw wanted reg copy
						return CgValue(dst, n->get_type(), 0);
					}
					else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_RHS)  && opname==ASSIGN){
						//assignment  =
						if (lhs_v) dst.reg=n->regname=lhs.reg=lhs_v->regname;
						rhs.load_literal(ofp,next_index,0);
						lhs.store_from(ofp,rhs.reg);
						rhs.type=e->get_type();
						return rhs;
					}
					else if ((opflags & RWFLAGS)==(WRITE_LHS|READ_LHS|READ_RHS) ){
						// Assign-Operators += etc
						rhs.load(ofp,next_index);
						auto dstreg=lhs;
						lhs.load(ofp,next_index); // turns it into a value.
						write_instruction(ofp,opname,t?t:rhs.type, dst,lhs,rhs);
						int out=dstreg.store_from(ofp,dst.reg);
						return dstreg;
					}else {
						// RISClike 3operand dst=src1,src2
						lhs.load(ofp,next_index); rhs.load(ofp,next_index);
						write_instruction(ofp,opname,t,dst,lhs,rhs);
						dst.type=e->get_type();
						return dst;
					}
				} else if (!e->lhs && e->rhs){
					auto src=compile_node(ofp,e->rhs,curr_fn,sc,next_index);
					if (opname==ADDR){
						if (!src.type || !n->type()) {
							n->dump(-1);
							error(n,"something wrong\n");
						}
						return src.addr_op(n->type());
					}
					else if (opname==DEREF){
						return src.deref_op(ofp,n->type(),next_index);
					}
					else {
						if (opflags & (WRITE_LHS|WRITE_RHS)){static int once;if (once++){printf(";TODO: logic for modifications to memory");}}
						// todo: handle read/modify-writeness.
						// postincrement/preincrement etc go here..
						auto dst=CgValue(n->get_reg(opname,next_index,false), n->get_type());
						write_instruction(ofp,opname,t,dst,src);
						return dst;
					}
				} else if (e->lhs && e->rhs) {
					error(e,"postfix operators not implemented yet.");
				} else return CgValue();
	}
	if (auto e=dynamic_cast<ExprBlock*>(n)) {
		// [1] compound expression - last expression is the return .
		if(e->is_compound_expression()) {
			if (auto num=e->argls.size()) {
				for (int i=0; i<num-1; i++){
					compile_node(ofp,e->argls[i],curr_fn,sc,next_index);
				}
			if (e->argls.size())
				return compile_node(ofp,e->argls[num-1],curr_fn,sc,next_index);
			};
		}
		else if (e->is_struct_initializer()){
//			error(n,"can't compile struct initializer  expression, TODO, desugar it\n");
			StructInitializer si(sc,e); si.map_fields();
			// '.this' is the main node, type struct
			//auto struct_val =CgValue(0,e->get_type(),*next_index++);
			auto struct_val= alloca_struct(ofp, e, e->def->as_struct_def(),next_index);

//			auto struct_ptr=
			struct_val.load(ofp,next_index);
			e->regname=struct_val.reg; // YUK todo - reallyw wanted reg copy
			// can we trust llvm to cache the small cases in reg..
			for (int i=0; i<e->argls.size();i++) {
				// struct->field=expr[i]
				auto rvalue=compile_node(ofp,si.value[i],curr_fn,sc,next_index);
				auto dst = struct_val.dot(ofp,si.field_refs[i],next_index,sc);
				auto srcreg = rvalue.load_literal(ofp,next_index,0);
				dst.store_from(ofp,srcreg);
			}
			return struct_val;
		}
		// [2] Operator
		//[3] ARRAY ACCESS
		else if (auto ar=n->is_subscript()){
			auto expr=compile_node(ofp,ar->call_expr,curr_fn,sc,next_index);// expression[index]
			auto index=compile_node(ofp,ar->argls[0],curr_fn,sc,next_index);
			auto array_type=expr.type;
			auto inner_type=array_type->sub;
			auto dst=ar->get_reg(ARRAY,next_index,false);
			expr.load(ofp,next_index);
//			expr.load_into(ofp, *next_index++);
			if (!n->regname){n->regname=expr.reg;}
			index.load_literal(ofp,next_index,0);
			fprintf(ofp,"\t%%%s = getelementptr inbounds ",str(dst));
			write_type(ofp,array_type,false);
			fprintf(ofp,"* %%%s,i32 0, i32 ",str(expr.reg));//%%%d\n",getString(expr.reg),
			index.write_operand(ofp);
			fprintf(ofp,"\n");
			return CgValue(0,inner_type,dst);
		}
		//[3] FUNCTION CALL
		else if (e->is_function_call()){
			auto call_fn=e->get_fn_call();
			RegisterName indirect_call=0;

			if (e->call_expr->is_function_name()) {
				
			} else {
				dbprintf("indirect call\n");
				auto fptr = compile_node(ofp, e->call_expr, curr_fn, sc, next_index);
				indirect_call=fptr.load(ofp, next_index);
			}

			vector<CgValue> l_args;
			vector<CgValue> l_args_reg;
			// process function argumetns & load
			for (auto arg:e->argls){
				auto reg=compile_node(ofp,arg,call_fn,sc,next_index);
				if (!reg.type) {
					fprintf(ofp,"\t; reg %s  %s\n", str(reg.reg), str(reg.addr));
					arg->dump(0);
					auto reg=compile_node(ofp,arg,call_fn,sc,next_index);
					ASSERT(reg.type);
				}
				l_args_reg.push_back(reg);
			}
			fprintf(ofp,"\t;fncall %s\n", call_fn?str(call_fn->name):e->call_expr->name_str());
			int i=0;
			for (auto reg:l_args_reg){
				if (reg.addr && !reg.reg) {
					//dbprintf("warning writing adress of entity when we want entity\n");
					//ASSERT(reg.reg==0);
//					reg.type->dump(1);
					reg.load(ofp,next_index);
				}
				l_args.push_back(reg);
				i++;
			}

			auto dst=n->get_reg_new(call_fn?call_fn->name:FN, next_index);
			//auto rt=call_fn->get_return_value();
			auto ret_type=e->type();	// semantic analysis should have done this
		
//			if (call_fn){ ASSERT(0!=call_fn->get_return_value()->type()->eq(e->type()));}
			fprintf(ofp,"\t");
			//if (call_fn->has_return_value())
			if (e->type()->name!=VOID)
			{
				write_reg(ofp,dst); fprintf(ofp," = ");
			}
			fprintf(ofp,"call ");
			if (call_fn)
				write_function_type(ofp, call_fn,next_index);
			else
				write_function_type(ofp,e->call_expr->type());
			if (!indirect_call) {
				fprintf(ofp,"@%s",getString(call_fn->name));
			} else {
				write_reg(ofp,indirect_call);
			}
			
			fprintf(ofp,"(");
			for (auto i=0; i<l_args.size(); i++){
				if (i) {fprintf(ofp," ,");}
				auto reg=l_args[i];
				write_type(ofp,/*e->argls[i]->get_type()*/ reg.type, reg.is_addr());
				reg.write_operand(ofp);
			}
			fprintf(ofp,")\n");
			if (ret_type && ret_type->name!=VOID) {
				return CgValue(dst,ret_type);
			} else{
				return CgValue();
			}
		}
	} else if (auto st=dynamic_cast<ExprStructDef*>(n)){
		// todo - were inner-structs to be allowed?
		compile_struct_def(ofp,st,sc);
	} else if (auto id=dynamic_cast<ExprIdent*>(n)){
		auto var=sc->find_variable_rec(n->name);
		if (!var){
			if (n->def) {
				dbprintf("\n place %s:%s into new lazy value\n",n->def->name_str(),n->def->kind_str());
				return CgValue(n->def);
			}
			error(n,"var not found %s\n",n->name_str());
			return CgValue();
		}
		if (var && var!=n->def){
//			error(n,"var/def out of synx %s %s\n",n->name_str(),var->name_str());
//			return CgValue();
		}
		return CgValue(var);
	} else if (auto li=dynamic_cast<ExprLiteral*>(n)){
		return CgValue(li);
	} else if (auto ifn=dynamic_cast<ExprIf*>(n)) {
		return compile_if(ofp,ifn,curr_fn,sc,next_index);
	} else if (auto ifn=dynamic_cast<ExprFor*>(n)) {
		return compile_for(ofp,ifn,curr_fn,sc,next_index);
	} else if (auto ift=dynamic_cast<Type*>(n)) {
		return CgValue(0,ift,0);
	} else{
		fprintf(ofp,"\t;TODO: node not handled %s\n",n->kind_str());
		return CgValue();
	}
	return CgValue();
}

// Emit function header..
enum EmitFnMode {EmitDefinition,EmitDeclaration,EmitType};
void write_function_signature(FILE* ofp,ExprFnDef* fn_node,int *regname, EmitFnMode mode){
	auto scope=fn_node->scope;
	fn_node->clear_reg();
	auto rtype=fn_node->return_type();
	fprintf(ofp,mode==EmitDefinition?"define ":mode==EmitDeclaration?"declare ":" ");
	write_type(ofp,rtype,false);
	if (mode!=EmitType)
		fprintf(ofp," @%s ",getString(fn_node->name));
	fprintf(ofp,"(");
	int inter=0;
	for (auto a:fn_node->args){
		if (inter++){fprintf(ofp,",");};
		write_type(ofp,a->type(),a->type()->is_complex());
		if (mode==EmitDefinition){
			auto var=scope->get_or_create_scope_variable(a,a->name, VkArg);
			var->get_reg(a->name, regname, false);
			fprintf(ofp, " %%%s", getString(var->regname));
		}
	}

	if (fn_node->variadic) {
		if (fn_node->args.size())fprintf(ofp,",");fprintf(ofp,"...");
	}
	fprintf(ofp,")");
	if (mode==EmitType)fprintf(ofp,"*");
	else fprintf(ofp,"\n");
}
void write_function_type(FILE* ofp, const Type* t) {
	auto argtuple=t->sub;
	ASSERT(argtuple);
	auto retn=argtuple->next;
	write_type(ofp,retn,0);
	fprintf(ofp,"(");
	for (auto arg=argtuple->sub; arg;arg=arg->next){
		write_type(ofp,arg);
		if (arg->next)fprintf(ofp,",");
	}
	fprintf(ofp,")");
	fprintf(ofp,"*");
	
}
void write_function_type(FILE* ofp,ExprFnDef* fn_node,int* regname){
	write_function_signature(ofp,fn_node,regname,EmitType);
}

Type* compile_function(FILE* ofp,ExprFnDef* fn_node, Scope* outer_scope){
	RegisterName regname=0;

	if (!fn_node){return nullptr;}
	if (fn_node->is_undefined()) {
		fprintf(ofp,";fn %s prot\n",getString(fn_node->name));
		write_function_signature(ofp,fn_node,&regname,EmitDeclaration);

		return nullptr;
	}
	if (fn_node->is_generic()) {
		fprintf(ofp,";fn %s generic:-\n",getString(fn_node->name));
		for (auto f=fn_node->instances; f;f=f->next_instance){
			fprintf(ofp,";fn %s generic instance\n",getString(fn_node->name));
			compile_function(ofp,f,outer_scope);
		}
		return nullptr;
	}
	if (!fn_node->get_type() && fn_node->fn_type && fn_node->scope ){
		error(fn_node,"function name %s %p %p %p %p", str(fn_node->name), fn_node->instance_of, fn_node->get_type(), fn_node->fn_type, fn_node->scope);
		ASSERT(0 && "function must be resolved to compile it");
		return nullptr;
	}
	fprintf(ofp,";fn %s (%p) :- ins=%p of %p \n", str(fn_node->name),fn_node, fn_node->instances, fn_node->instance_of);

	auto scope=fn_node->scope;
//	auto rt=fn_node->resolve(scope, nullptr);
//	if (rt.status!=ResolvedType::COMPLETE) {
//		fprintf(ofp,";can't compile function %s- not resolved\n",getString(fn_node->name));
//		return rt;
//	}
	
	write_function_signature(ofp,fn_node,&regname,EmitDefinition);
 	fprintf(ofp,"{\n");
	write_local_vars(ofp, fn_node->body, fn_node, scope, &regname);
	auto rtn=fn_node->get_return_value();
	auto ret=compile_node(ofp, fn_node->body, fn_node,scope,&regname);
//	printf("%p\n",ret.type);
	// return value
	if (ret.is_valid() && !ret.type->is_void()) {
		ret.load(ofp,&regname);
//		ASSERT(ret.reg && !ret.addr);	//TODO: returning foo.bar requires '.load'...
		fprintf(ofp,"\tret ");
		write_type(ofp,rtn->get_type(),ret.is_addr());
//		fprintf(ofp," %%%s\n", getString(ret.reg));
		ret.write_operand(ofp);
		fprintf(ofp,"\n");
		//getString(ret->get_reg(0,&regname,false)));
	} else {
		fprintf(ofp,"\tret void\n");
	}
	fprintf(ofp,"}\n");
	return fn_node->fn_type;
}

// extern function; - just use the function name, and mangle arguments given at the callsite.
// this is the default assumed for unfound symbols?
// extern "C" function; - a function with C linkage.
char hexdigit(char c){if (c<10) return c+'0'; else return 'A'+(c-10);}
int translate_llvm_string_constant(char* dst, int size, const char* src){
	const char* s=src;
	char*d=dst;
	int len=0;
	for (; *s && size>2; size--){
		*d++=*s++; len++;
		// Do what C does..
		if (s[-1]!='\\') {
			continue;
		}
		char c=*s++;
		if (c=='n') c=0xa; else if (c=='t') c=0x9; else if (c=='f') c=0xc; else if(c=='r') c=0xd; else if(c=='a') c=0x7;  else if(c=='b') c=0x8;else if(c=='v') c=0xb;else c=0;
		
		*d++=hexdigit((c>>4) & 0xf);
		*d++=hexdigit(c & 0xf);
	}
	*d++=0;
	printf("%s",dst);
	return len;
}
class CodeGen{
};

CgValue Node::codegen(CodeGen& cg, bool just_contents) {
	dbprintf("TODO refactor codegen to use this virtual. warning codegen not implemented for %s\n",this->kind_str());
	return CgValue();
}

/*
void name_mangle(char* dst, int size, ExprFnDef* src) {
	dst[0]=0;
	// TODO - prefix scopes. Now, Overloading is the priority.
	sprintf(dst,"_Z");dst+=2;
	int len-=strlen(dst); size--; size-=len;
	name_mangle_add_segment(dst, size, src->name, Name n);
	for (auto a:src->args){
		//a.
	}
	
}
char* name_mangle_append_name(char *dst,int size, Name n){
	if (n==BOOL){ strcat(dst,"b");}
	if (n==UINT){ strcat(dst,"u");}
	if (n==INT){ strcat(dst,"i");}
	if (n==LONG){ strcat(dst,"l");}
	else if (n==FLOAT){strcat(dst,"f");}
	else if (n==CHAR){strcat(dst,"c");}
	else if (n==){strcat(dst,"f");}
		auto s=str(n);
		int len=strlen(s);
		dst+=strlen(dst);
		sprintf(dst,"%d",len);
		strcat(dst,n);
	}
	return dst+strlen(dst);
}
 */



// operand bracket  means call
// operands are just a tuple
//

// operand
// lambda expression



void output_code(FILE* ofp, Scope* scope) {
	fprintf(ofp,";from scope %s\n;\n",scope->name());
	// output all inner items that outer stuff depends on..
	for (auto sub=scope->child; sub; sub=sub->next) {
		output_code(ofp,sub);
	}
	for (auto l=scope->literals; l; l=l->next_of_scope) {
		if (l->type_id==T_CONST_STRING){
		const char* name=getString(l->name);
			char buffer[512];
			l->llvm_strlen=translate_llvm_string_constant(buffer,512, l->as_str())+1;
			fprintf(ofp,"@.%s = private unnamed_addr constant [%d x i8] c\"%s\\00\"\n", getString(l->name), l->llvm_strlen, buffer);
		}
	}
	for (auto n=scope->named_items;n;n=n->next) {
		for (auto s=n->structs; s;s=s->next_of_name) {
			compile_struct_def(ofp, s, scope);
		}
	}
	for (auto n=scope->named_items;n;n=n->next) {
		for(auto f=n->fn_defs; f; f=f->next_of_name){
			compile_function(ofp,f,scope);
		}
	}
}




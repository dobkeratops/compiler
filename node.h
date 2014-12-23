	#pragma once
/// needed split this to break some circular dependancies.

typedef char ResolveResult;
enum {COMPLETE=0,INCOMPLETE=1,MISMATCH=2,RS_ERROR=INCOMPLETE|MISMATCH};

struct Type;
struct Expr;

struct Node {
private:
	Type* m_type=0;

	
public:
	int visited;					// anti-recursion flag.
	Node*	m_parent=0;					// for search & error messages,convenience TODO option to strip.
	Name name;
	RegisterName reg_name=0;			// temporary for llvm SSA calc. TODO: these are really in Expr, not NOde.
	bool 	reg_is_addr=false;
	ResolveResult	resolved=INCOMPLETE;			// true once all types are set correctly.
	SrcPos pos;						// where is it
	Node(){}
	ExprDef*	def=0;		// definition of the entity here. (function call, struct,type,field);
	Node*		next_of_def=0;
	void set_def(ExprDef* d);
	void set_struct_type(ExprDef* d);

	void clear_def();
	virtual void dump(int depth=0) const;
	virtual ResolveResult resolve(Scope* scope, const Type* desired,int flags){dbprintf("empty? %s resolve not implemented", this->kind_str());return ResolveResult(INCOMPLETE);};
	// wrapper handles 'this==nullptr', and propogation of 'resolved' flag.
	ResolveResult resolve_if(Scope* scope, const Type* desired,int flags){
		if (this) {
			bool try_improved=false;
			if (this->resolved==COMPLETE && try_improved) {
				#if DEBUG>=2
				this->resolved=COMPLETE;
				this->resolve(scope,desired,flags);
				if (this->resolved!=COMPLETE){
					this->resolved=COMPLETE;//repeat for debugger
					this->resolve(scope,desired,flags);
					error(this,"ICE,node was falsely declared complete %s:%s",this->name_str(),this->kind_str());
					return ResolveResult(this->resolved);
				}
				#endif
				return ResolveResult(COMPLETE);
			}
			else
				
			{
				this->resolved=COMPLETE;
				
				// subsequent resolve call will set to something else if not resolved.
				auto r=this->resolve(scope,desired,flags);
				resolved|=r;
				return r;
			}
		}
		else
			return ResolveResult(COMPLETE);
	}
	virtual const char* kind_str()const	{return"node";}
	void	replace_name_if(Name if_is,Name replacement){if (name==if_is) name=replacement;}
	virtual int get_name() const		{return 0;}
	virtual Name get_mangled_name()const {return name;}
	const char* get_name_str()const;
	const char* name_str()const			{return str(this->name);}
	//	Name ident() const					{if (this)return this->name;else return 0;}
	virtual Node* clone() const=0;
	Node* clone_if()const				{ if(this) return this->clone();else return nullptr;}
	void dump_if(int d)const			{if (this) this->dump(d);}
	virtual void clear_reg()			{reg_name=0;};
	RegisterName get_reg(CodeGen& cg, bool force_new);
	RegisterName get_reg_new(CodeGen& cg);
	RegisterName get_reg_named(Name baseName, int* new_index, bool force_new);
	RegisterName get_reg_named_new(Name baseName, int* new_index);
	RegisterName get_reg_existing();
	virtual	vector<TParamDef*>*			get_typeparams(){ return nullptr;}
	Node*	parent()					{return this->m_parent;}
	void	set_parent(Node* p)			{this->m_parent=p;}
	virtual CgValue codegen(CodeGen& cg,bool contents);
	virtual bool is_undefined()const										{if (this && name==PLACEHOLDER) return true; return false;}
	virtual void find_vars_written(Scope* s,set<Variable*>& vars ) const	{return ;}
	void find_vars_written_if(Scope*s, set<Variable*>& vars) const{ if(this)this->find_vars_written(s, vars);
	}
	void translate_typeparams_if(const TypeParamXlat& tpx){if (this) this->translate_typeparams(tpx);}
	virtual void translate_typeparams(const TypeParamXlat& tpx){ error(this,"not handled for %s",this->kind_str()); };
	virtual ExprOp* as_op()const			{error(this,"expected op, found %s:%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name as_name()const {
		error(this,"expected named item at node %s kind=%s",str(this->name),this->kind_str());
		return PLACEHOLDER;
	};
	bool is_ident()const{return as_ident()!=nullptr;}
	virtual ExprStructDef* as_struct_def()const;
	template<typename T> T* as()const{ auto ret= const_cast<T*>(dynamic_cast<T*>(this)); if (!ret){error(this,"expected,but got %s",this->kind_str());} return ret;};
	template<typename T> T* isa()const{ return const_cast<T*>(dynamic_cast<T*>(this));};
	virtual void recurse(std::function<void(Node* f)>& f){dbprintf("recurse not implemented for %s\n",this->kind_str());ASSERT(0&&"unimplemented recurse");};

	virtual CgValue compile(CodeGen& cg, Scope* sc, CgValue input);
	CgValue compile(CodeGen& cg, Scope* sc);
	CgValue compile_if(CodeGen& cg, Scope* sc);
	virtual Node* instanced_by()const{return nullptr;}
	virtual const Type*			as_type()const	{return nullptr;}
	virtual ExprIdent*			as_ident() 		{return nullptr;}
	virtual ExprLiteral*		as_literal() 		{return nullptr;}
	virtual const ExprIdent*	as_ident() const{return nullptr;}
	virtual ExprFor* 			as_for() 		{return nullptr;}
	virtual ExprFnDef*			as_fn_def() 	{return nullptr;}
	virtual const ExprFnDef*	as_fn_def() const {return nullptr;}
	virtual ExprBlock*			as_block()		{return nullptr;}
	virtual TParamDef*			as_tparam_def() {return nullptr;}
	virtual const ExprBlock* 	as_block() const{return nullptr;}
	virtual ArgDef* as_arg_def()				{return nullptr;}
	virtual Variable* as_variable()				{return nullptr;}
	virtual const Variable* as_variable() const {return nullptr;}
	ArgDef*			as_field() 			{return this->as_arg_def();}
	virtual void verify() {};
	// abstract interface to 'struct-like' entities;
	virtual const Type* get_elem_type(int index)const {error(this,"tried to get elem on name=%s kind=%s",str(this->name),this->kind_str());return nullptr;}
	virtual Name get_elem_name(int index)const {return const_cast<Node*>(this)->get_elem_node(index)->name;}
	virtual int get_elem_index(Name name){error(this,"tried to get elem on %s %s",str(this->name),this->kind_str());return -1;}
	int get_elem_index(Name name)const{return const_cast<Node*>(this)->get_elem_index(name);}
	virtual int get_elem_count()const{return 0;}
	virtual size_t alignment()const {return 16;} // unless you know more..
	virtual Node*	get_elem_node(int index){
		error(this,"tried to get elem %d on %s:%s, not supported",index, str(this->name),this->kind_str());
		return nullptr;
	}
	virtual Node*	get_elem_node(int index,int subindex){
		return this->get_elem_node(index)->get_elem_node(subindex);
	}
	virtual Node*	get_elem_node(int index,int sub1,int sub2){
		return this->get_elem_node(index)->get_elem_node(sub1)->get_elem_node(sub2);
	}
	virtual Node*	get_last(){
		return this->get_elem_node(this->get_elem_count()-1);
	}
	virtual Node*	get_first(){
		return this->get_elem_node(0);
	}
	virtual ~Node(){
		error("dont call delete, we haven't sorted out ownership of Types or nodes. compiler implementation doesn't need to free anything. Types will be owned by a manager, not the ast ");
	}
	virtual Expr*	loop_else_block()const			{return nullptr;}// for decoupling something
	LLVMType get_type_llvm() const;
	virtual Type* eval_as_type()const		{return nullptr;};
	virtual ExprBlock* is_subscript()const	{return (ExprBlock*)nullptr;}
	virtual bool is_function_name()const	{return false;}
	virtual bool is_variable_name()const	{return false;}
	virtual Scope* get_scope()				{return nullptr;}
	Type* expect_type() const;
	Type* get_type() const		{
#if DEBUG>=4
		if(this) {::verify(this->m_type);return this->m_type;}else return nullptr;
#else
		if (this) {return this->m_type;} else return nullptr;
#endif
	}
	const Type* type_if()const	{if (this)return this->type();else return nullptr;}
	Type*& type()				{::verify(this->m_type);return this->m_type;}
	const Type* type()const		{::verify(this->m_type);return this->m_type;}
	void type(const Type* t)	{::verify(t);this->m_type=(Type*)t;}
	void set_type(const Type* t);
	void clear_type(){m_type=0;};
	void force_type_todo_verify(const Type* t){ m_type=const_cast<Type*>(t);}
	Type*& type_ref()			{return this->m_type;}
	void dump_top()const;

	// these would be extention methods if C++ had them
	ResolveResult propogate_type_refs(int flags,const Node*n, Type*& a,Type*& b);
	ResolveResult propogate_type_refs(int flags, Expr *n, Type*& a,Type*& b);
	ResolveResult propogate_type_fwd(int flags,const Node* n, const Type* a,Type*& b);
	ResolveResult propogate_type_fwd(int flags,Expr* e, const Type*& a);
	ResolveResult propogate_type_expr_ref(int flags,Expr* e, Type*& a);
	ResolveResult propogate_type_refs(int flags,const Node* n, Type*& a,Type*& b,Type*& c);
	ResolveResult propogate_type_fwd(int flags,const Node* n,const Type*& a,Type*& b,Type*& c);
	

};


template<typename T>
T* expect_cast(Node* n){
	auto r=dynamic_cast<T*>(n);
	if (!r) {
		extern void error(const Node*,const char*,...);
		T t;
		error(n, "expected %s to be %s not %s", str(n->name),t.kind_str(), n->kind_str());
		n->dump(-1);
	}
	return r;
}

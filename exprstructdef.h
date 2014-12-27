#pragma once
#include "ast.h"
#include "scope.h"
#include "exprfndef.h"
#include "codegen.h"

struct NamedItems;
struct ImplDef;
struct TraitDef;
struct ExprStructDef: ExprDef {
	// lots of similarity to a function actually.
	// but its' backwards.
	// it'll want tparams aswell.
	Name mangled_name=0;
	Name vtable_name=0;
	int		discriminant=0;
	bool is_compiled=false;
	bool m_is_variant=false;
	bool m_is_enum=false;
	bool is_enum() { return m_is_enum;}
	vector<TParamDef*>	tparams;	// todo move to 'ParameterizedDef; strct,fn,typedef,mod?
	vector<Type*>		instanced_types;
	vector<ArgDef*>			fields;
	vector<ArgDef*>			static_virtual;
	vector<ArgDef*>			static_fields;
	vector<ExprLiteral*>	literals;
	vector<ExprStructDef*>	structs;
	vector<ExprFnDef*>		virtual_functions;
	vector<ExprFnDef*>		functions;
	vector<ExprFnDef*>		static_functions;
	vector<TypeDef*>		typedefs;
	vector<ArgDef*>			args;		// default constructor form
	ExprBlock*				body=0;		// for default constructor form.
	ImplDef*				impls=0;
	int 	first_user_field_index()const;
	Type*	inherits_type=0;
	Scope* scope=0;
	ExprStructDef* inherits=0,*derived=0,*next_of_inherits=0; // walk the derived types of this.
	ExprStructDef* vtable=0;
	bool is_generic() const;
	ExprStructDef* instances=0, *instance_of=0,*next_instance=0;
	ExprFnDef* default_constructor=0;							// TODO scala style..
	NamedItems* name_ptr=0;
	//	ArgDef* find_field(Name name){ for (auto a:fields){if (a->name==name) return a;} error(this,"no field %s",str(name));return nullptr;}
	ArgDef* find_field(const Node* rhs)const;
	ArgDef* try_find_field(const Name n)const;
	int field_index(const Node* rhs);
	const char* kind_str()const	override{return"struct";}
	ExprStructDef* next_of_name;
	Name	get_mangled_name()const;
	bool	has_vtable()const{
		return this->virtual_functions.size()!=0||(this->inherits?this->inherits->has_vtable():0);
	}
	ExprStructDef(SrcPos sp,Name n)		{
		name=n;pos=sp;name_ptr=0;inherits=0;inherits_type=0;next_of_inherits=0;
		derived=0; name_ptr=0;next_of_name=0; instances=0;instance_of=0;next_instance=0;
	}
	size_t		alignment() const;
	ExprStructDef*	as_struct_def()const	{return const_cast<ExprStructDef*>(this);}
	void			set_discriminant(int value){discriminant=value;m_is_variant=true;}
	void			set_variant_of(ExprStructDef* owner, int index){set_discriminant(index); ASSERT(inherits==0); inherits=owner;}
	void			dump(int depth)const;
	void			dump_instances(int depth)const;
	void			dump_struct_body(int depth) const;
	size_t			size() const;
	Node*			clone()const;
	ImplDef*		get_impl_for(TraitDef* t);	//optionally instantiates (like go)
	Node*			clone_sub(ExprStructDef* into) const;
	void			inherit_from(Scope* sc, Type* base);
	void	translate_tparams(const TParamXlat& tpx) override;
	ExprStructDef*	get_instance(Scope* sc, const Type* type); // 'type' includes all the tparams.
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags)override;
	
	void			roll_vtable();
	CgValue compile(CodeGen& cg, Scope* sc,CgValue input) override;
	const Type*			get_elem_type(int i)const;//{return this->fields[i]->type();}
	Name			get_elem_name(int i)const;// {return this->fields[i]->name;}
	int 			get_elem_index(Name name)const;//{int i; for (i=0; i<this->fields.size(); i++){if (this->fields[i]->name==name)return i;} return -1;}
	int				override_index(ExprFnDef* f);
	int				vtable_size();
	int				vtable_base_index();
	ExprStructDef*	root_class();
	vector<TParamDef*>*			get_typeparams() override { return &tparams;}
	int				get_elem_count(){return this->fields.size();}
	bool			is_vtable_built(){return this->vtable_name!=0;}
	const ExprFnDef*		find_function_for_vtable(Name n, const Type* fn_type);
	bool			has_base_class(ExprStructDef* other)const;
	int				num_instances()const {auto n=0;for (auto ins=instances;ins;ins=ins->next_instance){n++;} return n;}
	void		recurse(std::function<void(Node*)>&);
};

struct EnumDef  : ExprStructDef {
	//	void dump(int depth)const;
	//	virtual void translate_tparams(const TParamXlat& tpx);
	Node* clone()const;
	const char* kind_str()const{return "enum";}
	EnumDef(SrcPos sp, Name n):ExprStructDef(sp,n){m_is_enum=true;};
	//CgValue compile(CodeGen& cg, Scope* sc); // different compile behaviour: discriminant+opaque
};

/// a rust 'Trait' is a struct with only virtual functions (&typedefs)
struct TraitDef : ExprStructDef {
	ImplDef* impls=0;
	Node* clone()const;
	const char* kind_str()const{return "trait";}
	TraitDef(SrcPos sp, Name n):ExprStructDef(sp,n){};
	ResolveResult	resolve(Scope* scope, const Type* desired,int flags)override;
	CgValue compile(CodeGen& cg, Scope* sc,CgValue input) override;
};

/// a rust 'Impl' extends a struct with functions implementations
/// it could also represent a C++ namespace ... whihc would just add more definitions to a fieldless struct
struct ImplDef : ExprStructDef {
	Node* clone()const;
	Type* impl_trait=0;
	Type* impl_for_type=0;
	ExprStructDef* impl_for_struct=0;
	ImplDef*	next_of_type=0;
	ImplDef*	next_of_trait=0;
	void		add_to_struct();
	const char* kind_str()const{return "impl";}
	void dump(int depth) const;
	ImplDef(SrcPos sp, Name n):ExprStructDef(sp,n){};
};
/// a rust 'Mod' is just a struct with no fields, and just static functions
/// generalize the concepts of C++ nested structs,C++ namespaces, & Mods.
struct ModDef : ExprStructDef {
	Node* clone()const;
	const char* kind_str()const{return "mod";}
	ModDef(SrcPos sp, Name n):ExprStructDef(sp,n){};
};
ExprFnDef* instantiate_generic_function(ExprFnDef* srcfn,const Expr* callsite, const Name name, const vector<Expr*>& call_args, const Type* return_type,int flags);





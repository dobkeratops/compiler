#pragma once
#include "ast.h"
#include "scope.h"
#include "exprfndef.h"
struct NamedItems;
struct ExprStructDef: ExprDef {
	// lots of similarity to a function actually.
	// but its' backwards.
	// it'll want TypeParams aswell.
	Name mangled_name=0;
	Name vtable_name=0;
	bool is_compiled=false;
	bool is_enum_=false;
	bool is_enum() { return is_enum_;}
	vector<TParamDef*>	typeparams;	// todo move to 'ParameterizedDef; strct,fn,typedef,mod?
	vector<Type*>		instanced_types;
	vector<ArgDef*>			fields;
	vector<ArgDef*>			static_virtual;
	vector<ArgDef*>			static_fields;
	vector<ExprLiteral*>	literals;
	vector<ExprStructDef*>	structs;
	vector<ExprFnDef*>		virtual_functions;
	vector<ExprFnDef*>		functions;
	vector<ExprFnDef*>		static_functions;
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
	int field_index(const Node* rhs){
		auto name=rhs->as_name();
		for (auto i=0; i<fields.size(); i++){
			if(fields[i]->name==name){
				((Node*)rhs)->set_def((ExprDef*)fields[i]);
				return i;
			}
		}
		return -1;
	}
	const char* kind_str()const	override{return"struct";}
	ExprStructDef* next_of_name;
	Name	get_mangled_name()const;
	bool	has_vtable()const{return this->virtual_functions.size()!=0||(this->inherits?this->inherits->has_vtable():0);}
	ExprStructDef(SrcPos sp,Name n)		{name=n;pos=sp;name_ptr=0;inherits=0;inherits_type=0;next_of_inherits=0; derived=0; name_ptr=0;next_of_name=0; instances=0;instance_of=0;next_instance=0;}
	size_t		alignment() const			{size_t max_a=0; for (auto a:fields) max_a=std::max(max_a,a->alignment()); return max_a;}
	ExprStructDef*	as_struct_def()const	{return const_cast<ExprStructDef*>(this);}
	void			dump(int depth)const;
	size_t			size() const;
	Node*			clone()const;
	Node*			clone_sub(ExprStructDef* into) const;
	void			inherit_from(Scope* sc, Type* base);
	void	translate_typeparams(const TypeParamXlat& tpx) override;
	ExprStructDef*	get_instance(Scope* sc, const Type* type); // 'type' includes all the typeparams.
	ResolvedType	resolve(Scope* scope, const Type* desired,int flags);
	void			roll_vtable();
	CgValue compile(CodeGen& cg, Scope* sc);
	Type*			get_elem_type(int i){return this->fields[i]->type();}
	Name			get_elem_name(int i){return this->fields[i]->name;}
	int 			get_elem_index(Name name){int i; for (i=0; i<this->fields.size(); i++){if (this->fields[i]->name==name)return i;} return -1;}
	int				override_index(ExprFnDef* f);
	int				vtable_size();
	int				vtable_base_index();
	ExprStructDef*	root_class();
	vector<TParamDef*>*			get_typeparams() override { return &typeparams;}
	int				get_elem_count(){return this->fields.size();}
	bool			is_vtable_built(){return this->vtable_name!=0;}
	const ExprFnDef*		find_function_for_vtable(Name n, const Type* fn_type);
	bool			has_base_class(ExprStructDef* other)const;
};

struct EnumDef  : ExprStructDef {
	//	void dump(int depth)const;
	//	virtual void translate_typeparams(const TypeParamXlat& tpx);
	Node* clone()const;
	const char* kind_str()const{return "enum";}
	EnumDef(SrcPos sp, Name n):ExprStructDef(sp,n){};
	CgValue compile(CodeGen& cg, Scope* sc); // different compile behaviour: discriminant+opaque
};

/// a rust 'Trait' is a struct with only virtual functions
struct TraitDef : ExprStructDef {
	Node* clone()const;
	const char* kind_str()const{return "trait";}
	TraitDef(SrcPos sp, Name n):ExprStructDef(sp,n){};
};

/// a rust 'Impl' extends a struct with functions implementations
/// it could also represent a C++ namespace ... whihc would just add more definitions to a fieldless struct
struct ImplDef : ExprStructDef {
	Node* clone()const;
	const char* kind_str()const{return "impl";}
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





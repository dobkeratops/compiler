#pragma once
#include "semantics.h"
#include "type.h"
#include "scope.h"


struct Node;
struct Scope;
struct Type;
struct SrcPos;
/// TODO - accumulate, sort errors
/// Can we tackle the craziness of "template instantiation errors" 
/// with a smart error system?
/// seperate summary and detail, and 'callstacks'
void error_begin(const Node* n);
void error_begin(const Node* n, const char* str, ... );
void error_end(const Node* n);
void error(const Node* n, const Type* t);
void error(const Node* n, const char* str, ...);
void error_newline();
void info(const Node* n, const char* str="", ... );
void warning(const Node* n, const char* str="", ... );
void warning(const SrcPos& p, const char* str, ...);
void error(const Node* n,const Scope* s, const char* str, ... );
void error(const Node* n,const char* str, ... );
void error(const char* str, ... );
void error(const Node* n,const Node* n2, const char* str, ... );
void error(const Node* n);	// dumbest error possible -just the source location
void error_begin(const SrcPos& p,const char* str, ... );
void error(const SrcPos& p,const char* str, ... );

template<typename T>
T* expect_cast(Node* n){
	auto r=dynamic_cast<T*>(n);
	if (!r) {
		T t;
		error(n, "expected %s to be %s not %s", str(n->name),t.kind_str(), n->kind_str());
		n->dump(-1);
	}
	return r;
}



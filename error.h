#pragma once

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
void info(const Node* n, const Type*);
void warning(const Node* n, const char* str="", ... );
void warning(const SrcPos& p, const char* str, ...);
void error(const Node* n,const Scope* s, const char* str, ... );
void error(const Node* n,const char* str, ... );
void error(const char* str, ... );
void error(const Node* n,const Node* n2, const char* str, ... );
void error(const Node* n);	// dumbest error possible -just the source location
void error_begin(const SrcPos& p,const char* str, ... );
void error(const SrcPos& p,const char* str, ... );



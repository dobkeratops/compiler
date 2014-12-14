#include "error.h"
#include <stdio.h>
#include <string.h>
#include <cstdlib>

bool g_error_on_newline=false;
int g_num_errors=0;
int g_error_depth=0;
extern const char* g_filename;		// todo propper codemap
enum ErrorLevel {
	E_INFO=0,
	E_WARNING=1,
	E_ERROR=2
};
const char* g_error_level[]={
	"info","warning","error"
};

/// todo accumulate node stack to assert its' called right.
/// todo - perhaps accumulate sub messages from the error.

void error_maybe_end(const Node* n){
	if (!g_error_depth) {
		error_newline();
		if (g_num_errors>0){
			exit(-1);
		}
	}
}

void error_newline(){
	if (!g_error_on_newline){
		g_error_on_newline=true;printf("\n");}
}
void error_srcpos(const SrcPos& pos){
	printf("%s:%d:%d:",g_filename,pos.line,pos.col);
}
void error_print_line(const char* txt) {
	if (strlen(txt)){
		printf("%s",txt);
		g_error_on_newline=txt[strlen(txt)-1]=='\n';
	}
}

void error_sub(const Node* n, int level, const char* txt ){
	g_num_errors++;
	error_newline();
	if (n)
		error_srcpos(n->pos);
	if (level)printf("%s-",g_error_level[level]);
	printf("\t");
	error_print_line(txt);
	if (strlen(txt)){
	} else{
		n->dump_if(-1);
	}
	if (n){
		if (auto x=n->instanced_by()){
			char buffer[512]; sprintf(buffer,"%s:%d:%d: referenced here %s\n",g_filename,x->pos.line,x->pos.col, x->name_str());
			error_print_line(buffer);
		}
	}
}
void error(const SrcPos& pos, const char* str ,...){
	char txt[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(txt, str, arglist );
	va_end( arglist );
	
	g_num_errors++;
	error_newline();
	printf("%s:%d:%d:",g_filename,pos.line,pos.col);
	printf("\t%s",txt);
	if (strlen(txt)){
		if (txt[strlen(txt)-1]!='\n'){g_error_on_newline=false;}
	}
	error_maybe_end(nullptr);
}

void error_begin(const Node* n, const char* str, ... ){
	g_error_depth++;
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,E_ERROR,buffer);
	error_maybe_end(n);
}
void warning(const Node* n, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,E_WARNING,buffer);
	error_maybe_end(n);
}

void info(const Node* n, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,E_INFO,buffer);
	error_maybe_end(n);
}

void error_begin(const Node* n){
	g_error_depth++;
}

void error(const Node* n){
	error_sub(n,E_ERROR,"");
	error_maybe_end(n);
}

void error_end(const Node* n){
	--g_error_depth;
	error_maybe_end(n);
}

void error(const Node* n,const Scope* s, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,E_ERROR,buffer);
	error_newline();
	info(s->owner_fn, "in scope %s\n",s->name());
	error_maybe_end(n);
}
void error(const Node* n,const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,E_ERROR,buffer);
	error_maybe_end(n);
}
void error(const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(nullptr,E_ERROR,buffer);
	error_maybe_end(nullptr);
}

void error(const Node* n,const Node* n2, const char* str, ... ){
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_sub(n,E_ERROR,buffer);
	warning(n2,"see %s",n2->name_str());
	error_maybe_end(n);
}
void error(const Node* n, const Type* t) {
	error_sub(n,E_ERROR,"");
	t->dump(-1);
	error_maybe_end(n);
}

void error_srcpos(const SrcPos& p, const char* str, ...) {
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_newline();
	error_srcpos(p);
	error_print_line(str);
	error_maybe_end(nullptr);
}
void warning(const SrcPos& p, const char* str, ...) {
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_newline();
	error_srcpos(p);
	error_print_line(buffer);

}

void error_begin(const SrcPos& p, const char* str, ...) {
	g_error_depth++;
	char buffer[1024];
	va_list arglist;
	va_start( arglist, str );
	vsprintf(buffer, str, arglist );
	va_end( arglist );
	error_newline();
	error_srcpos(p);
	error_print_line(str);
	error_maybe_end(nullptr);
}







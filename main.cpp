#include "main.h"
#include "compiler.h"
#include "parser.h"
#include "codegen.h"
#include "run_test.h"

void filename_change_ext(char* dst,const char* src,const char* new_ext){
	strcpy(dst,src);
	char* s=dst;
	for (s=dst;*s;s++){
		if (*s=='.') break;
	}
	if (!*s) { strcat(dst, ".out");}
	else
		if (!strlen(new_ext)) { s[0]=0;}
		else strcpy(s+1,new_ext);
}
int compile_source(const char *buffer, const char* filename, const char* outname, int flags){
	
	Lexer	src(buffer,filename);
	
	auto node=parse_block(src,0,SEMICOLON,nullptr);
	g_pRoot=node;
	Scope global(0); global.node=(ExprBlock*)node; global.global=&global;
	if (flags & B_AST){
		node->dump(0);
	}
	
	node->verify();
	node->resolve(&global,nullptr,0);
	if (flags & B_DEFS){
		global.dump(0);
	}
	if (flags & B_TYPES) {
		node->dump(0);
	}
	node->resolve(&global,nullptr,flags&(B_EXECUTABLE|B_RUN|B_LLVM)?R_FINAL:0);// if we just want to dump/search, we dont fail for final errors.
	if (flags & B_LLVM) {
		output_code(stdout, &global);
	}
	printf("%x\n",flags);
	if (outname && (flags & (B_EXECUTABLE|B_RUN|B_LLVM))){
		FILE* ofp=fopen(outname,"wb");
		if (ofp){
			output_code(ofp, &global);
			fprintf(ofp,"\n;end");
			fclose(ofp);
			if (flags & (B_EXECUTABLE|B_RUN)) {
				
				char exename[256];
				filename_change_ext(exename,outname,"");
				char compile_cmd[512]; sprintf(compile_cmd,"clang %s -o %s", outname, exename);
				if (flags & B_VERBOSE)printf("\nllvm src=%s\n executable=%s\nflags %x\n",outname,exename, flags);
				if (flags & B_VERBOSE)printf("\n%s\n",compile_cmd);
				auto ret= system(compile_cmd);
				if (!ret && (flags & B_RUN)) {
					if (flags & B_VERBOSE)printf("compiled ok, running executable %s \n", exename);
					char invoke[512];sprintf(invoke,"./%s",exename);
					return system(invoke);
					return 0;
				}
				return ret;
			}
		} else {
			printf("can't open output file %s\n",outname);
			return -1;
		}
	}
	return 0;
}

int compile_source_file(const char* filename, int options) {
	char outname[256];
	filename_change_ext(outname,filename,"ll");
	if (options & B_VERBOSE)printf("compiling %s\n -> %s\n",filename,outname);
	auto fp=fopen(filename,"rb");
	if (fp){
		fseek(fp,0,SEEK_END); auto sz=ftell(fp); fseek(fp,0,SEEK_SET);
		char* buffer = (char*)malloc(sz+1);
		fread((void*)buffer,1,sz,fp);
		buffer[sz]=0;
		fclose(fp);
		int ret=compile_source(buffer,filename,outname,options);
		free((void*)buffer);
		return ret;
	} else{
		printf("can't open %s\n",filename);
		return -1;
	}
}
template<typename X,typename Y>
struct Union{
	int tag;
	union {X x; Y y;};
	template<class FX,class FY,class R> R map(std::function<R(X)> fx,std::function<R(Y)> fy){
		if (tag==0) return fx(x);
		else return fy(y);
	}
};
// Union<int,float> u;
// This is the sort of thing we want our language to handle - works fine in Rust.
// C++ can't infer through to 'R' from the given 'function types', even less so with poly lambdas.
// auto x=u.map([](int x)->int{return 0;}, [](float x)->int{return 1;});
// printf("%d x\n",x);

Option g_Options[]={
	{'a',0,B_AST,"show AST"},
	{'t',0,B_TYPES,"dump AST annotated with types"},
	{'d',0,B_DEFS,"dump definitions"},
	{'g',0,B_GENERICS,"dump generic type info"},
	{'l',0,B_LLVM,"emit LLVM source"},
	{'r',0,B_RUN|B_EXECUTABLE,"build & run"},
	{'e',0,B_EXECUTABLE,"compile executable"},
	{'v',0,B_VERBOSE,"verbose mode"},
	{'h',0,0,"help"},
	{0,0,0,0}
};
void dump_help(){
	printf("embryonic C++/Rust hybrid language\n");
	printf("(we dont even have a name yet)\n");
	printf("to run: \n");
	printf("   hack srcfile [-options]\n");
	printf("default is compile and run. -e to generate exe and not run.\n");
	printf(" \n");
	
	for (auto opt=g_Options;opt->name;opt++){
		printf("%c - %s\n",opt->name,opt->help);
	}
}

int main(int argc, const char** argv) {
#if DEBUG>=2
	printf("compiled with debug level=%d\n", DEBUG);
#endif
	//	dbprintf("precedences: ->%d *%d +%d &%d \n", precedence(ARROW),precedence(MUL),precedence(ADD),precedence(AND));
	//	compile_source_file("~/hack/test_hack/prog.rs",0xf);
	int options=0,given_opts=0;
	for (auto i=1; i<argc; i++) {
		const char* a=argv[i];
		if (a[0]=='-'){
			for (auto j=1; a[j];j++){
				if (a[j]=='h') dump_help();
				for (auto opt=g_Options;opt->name;opt++){
					if (opt->name==a[j]) {options&=~opt->clear;options|=opt->set;}
				}
			}
		}
	}
	if (!options){
		options=B_RUN|B_EXECUTABLE;
	}
	
	for (auto i=1; i<argc; i++) {
		if (argv[i][0]!='-') {
			if (options & B_VERBOSE)
				printf("compile src file %s with options %x\n",argv[i],options);
			compile_source_file(argv[i],options);
		}
	}
	if (argc<=1) {
#if DEBUG>=2
		run_tests();
#else
		dump_help();
#endif
	}
}
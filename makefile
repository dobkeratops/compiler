try: hack foo
	./hack
	cat test.ll
	clang test.ll -o test
	./test

hack: foo hack.cpp codegen.cpp repl.cpp hack.hpp codegen.h repl.h
	g++ hack.cpp repl.cpp codegen.cpp -o hack -std=c++1y -g3 -DDEBUG
debug: hack
	./hack 

#foo.c for investigating LLVM format
foo: foo.c
	clang foo.c -S -emit-llvm
	clang foo.ll -o foo

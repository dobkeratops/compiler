debug: hack foo
	./hack hello.rpp -t

hack: foo hack.cpp codegen.cpp repl.cpp hack.h codegen.h repl.h
	g++ hack.cpp repl.cpp codegen.cpp -o hack -std=c++1y -g3 -DDEBUG

test_llvm: hack
	./hack 
	cat test.ll

#foo.c for investigating LLVM format
foo: foo.c
	clang foo.c -S -emit-llvm
	clang foo.ll -o foo

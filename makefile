run: hack foo
	./hack hello.rpp -tr

hack: foo hack.cpp codegen.cpp repl.cpp hack.h codegen.h repl.h lexer.h parser.h lexer.cpp parser.cpp
	g++ hack.cpp repl.cpp codegen.cpp lexer.cpp parser.cpp -o hack -std=c++1y -g3 -DDEBUG

# debug: hack
# 	./hack hello.rpp -tr


# #debug mode peppered with debug prints for everything
# debug3: foo hack.cpp codegen.cpp repl.cpp hack.h codegen.h repl.h lexer.h parser.h lexer.cpp parser.cpp
	g++ hack.cpp repl.cpp codegen.cpp lexer.cpp parser.cpp -o hack -std=c++1y -g3 -DDEBUG=3
	./hack


test_llvm: hack
	./hack 
	cat test.ll

#foo.c for investigating LLVM format
foo: foo.c
	clang++ foo.cpp -S -emit-llvm -g3
	clang++ foo.ll -o foo

try: hack
	./hack > test.ll
	more test.ll
	clang test.ll

hack: hack.cpp codegen.cpp repl.cpp hack.hpp codegen.h repl.h test.c
	g++ hack.cpp repl.cpp codegen.cpp -o hack -std=c++1y -g3 -DDEBUG
	gcc test.c -std=c99 -g3
debug: hack
	./hack 


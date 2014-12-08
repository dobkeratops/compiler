run: hack
	./hack hello.rpp -tr

SRC = main.cpp ast.cpp compiler.cpp  lexer.cpp parser.cpp codegen.cpp codegen_llvm.cpp run_test.cpp  repl.cpp
HEADER = main.h ast.h compiler.h codegen.h lexer.h parser.h run_test.h  repl.h
hack: $(SRC) $(HEADER) foo
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG

debug: hack
	./hack hello.rpp -tr

# 'DEBUG=3' switches on verbose debug trace , 
# dumps of intermediate state during resolving
debug3: $(SRC) $(HEADER)
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG=3
	./hack

clean:
	-rm ./hack
	-rm *.ll
	-rm *.o
	-rm test*

test_llvm: hack
	./hack 
	cat test.ll

#foo.c for investigating LLVM format
foo: foo.cpp
	clang++ foo.cpp -S -emit-llvm -g3
	clang++ foo.ll -o foo

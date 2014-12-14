run: hack
	./hack example.rs -tr

SRC = main.cpp ast.cpp semantics.cpp exprfndef.cpp exprstructdef.cpp type.cpp scope.cpp compile.cpp  lexer.cpp parser.cpp codegen.cpp  run_test.cpp  repl.cpp error.cpp everywhere.cpp
HEADER = main.h ast.h semantics.h compile.h codegen.h lexer.h parser.h run_test.h  repl.h error.h everywhere.h exprstructdef.h exprfndef.h type.h
hack: $(SRC) $(HEADER) foo
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG

debug: hack
	./hack example.rs -tr

# 'DEBUG=4' switches on ultra verbose debug trace , 
# dumps of intermediate state during resolving 
debug4: $(SRC) $(HEADER)
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG=4
	./hack
	./hack example.rs

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
	clang++ foo.cpp -S -emit-llvm -std=c++1y -g3
	clang++ foo.ll -o foo

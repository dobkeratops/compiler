run: hack
	./hack hello.rpp -tr

SRC = main.cpp ast.cpp compiler.cpp codegen.cpp lexer.cpp parser.cpp test.cpp  resolve.cpp repl.cpp
HEADER = main.h ast.h compiler.h codegen.h lexer.h parser.h test.h resolve.h repl.h
hack: $(SRC) $(HEADER) foo
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG

debug: hack
	./hack hello.rpp -tr

debug3: $(SRC) $(HEADER)
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG=3
	./hack

clean:
	-rm ./hack
	-rm *.ll
	-rm *.o

test_llvm: hack
	./hack 
	cat test.ll

#foo.c for investigating LLVM format
foo: foo.cpp
	clang++ foo.cpp -S -emit-llvm -g3
	clang++ foo.ll -o foo

run: hack foo
	./hack hello.rpp -tr

SRC = compiler.cpp repl.cpp codegen.cpp lexer.cpp parser.cpp test.cpp  resolve.cpp ast.cpp
HEADER = compiler.h repl.h codegen.h lexer.h parser.h test.h main.h resolve.h
hack: $(SRC) $(HEADER)
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG

debug: hack
	./hack hello.rpp -tr

debug3: $(SRC) $(HEADER)
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG=3
	./hack

clean:
	rm ./hack
	rm *.o

test_llvm: hack
	./hack 
	cat test.ll

#foo.c for investigating LLVM format
foo: foo.c
	clang++ foo.cpp -S -emit-llvm -g3
	clang++ foo.ll -o foo

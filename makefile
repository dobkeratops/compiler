run: hack
	./hack example.rs -r
	./hack maths.rs -r

SRC = main.cpp node.cpp stringtable.cpp ast.cpp semantics.cpp exprfndef.cpp exprstructdef.cpp type.cpp scope.cpp exprflow.cpp compile.cpp exprop.cpp exprblock.cpp lexer.cpp parser.cpp codegen.cpp  run_test.cpp  repl.cpp error.cpp everywhere.cpp assist.cpp pattern.cpp
HEADER = main.h node.h stringtable.h ast.h semantics.h compile.h codegen.h lexer.h parser.h run_test.h  repl.h error.h everywhere.h exprstructdef.h exprop.h exprblock.h exprfndef.h type.h assist.h pattern.h
hack: $(SRC) all.cpp $(HEADER) foo
	g++ all.cpp  -o hack -std=c++1y -g3 -DDEBUG

#run internal test
test: hack
	./hack -T

# 'DEBUG=4' switches on ultra verbose debug trace , 
# dumps of intermediate state during resolving 
debug4: $(SRC) all.cpp $(HEADER)
	g++ $(SRC)  -o hack -std=c++1y -g3 -DDEBUG=4
	./hack
	./hack example.rs

hack_dbg2: $(SRC) all.cpp $(HEADER)
	g++ all.cpp -o hack_dbg2 -std=c++1y -g3 -DDEBUG=2

debug2: hack_dbg2
	./hack_dbg2 -T

debug: hack_dbg2
	./hack_dbg2 example.rs -tr

#todo - can we find a way to syntax highlight it.
diff:
	git --no-pager diff

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
	clang++ foo.cpp -S -emit-llvm -std=c++1y -g3 -DCOMPILE_FOO
	clang++ foo.ll -o foo

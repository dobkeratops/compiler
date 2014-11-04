hack: hack.cpp codegen.cpp  hack.hpp codegen.h
	g++ hack.cpp codegen.cpp -o hack -std=c++1y -g3 -DDEBUG
debug: hack
	./hack 

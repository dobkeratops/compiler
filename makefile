hack: hack.cpp
	g++ hack.cpp -o hack -std=c++1y -g3 -DDEBUG
debug: hack
	./hack 

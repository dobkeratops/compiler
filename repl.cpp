#ifdef _NO_
#include "repl.h"
void repl() {	

	Scope	global(nullptr);
	while (1) {
		char* text;size_t len;
		printf("repl>");
		getline(&text,&len,stdin);
		printf("%s",text);
		free(text);
		ASSERT(0&&"TODO");
	}
}

#endif
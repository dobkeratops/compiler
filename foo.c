#include <stdio.h>
#include <stdlib.h>

// simple C sourcefile to investigate LLVM format
float lerp(float a,float b, float f){
	return (b-a)*f+a;
}

void foo(int x){
}
struct Foo {
	float x;float y;float z;float w;
};

int main(int argc, const char** argv) {
	int tmp[512];
	tmp[2]=10;
	tmp[5]=50;
	tmp[10]=123456;
	float x=10.0f,y=20.0f,z;
	void (*fp)(int) = foo;
	double d=x;
	printf("%p",fp);
	struct Foo* ptr=malloc(sizeof(struct Foo));
	float* ptr2=(float*)ptr;
	if (argc < 2){
		printf("hello world %.3f %d\n",z,tmp[10]);
	}
		printf("<2");
	return 0;
}

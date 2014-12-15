// for friendlier 'build *.cpp in this directory' 
// this isn't part of the project, its just used to see what LLVM output is
#ifdef COMPILE_FOO
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

struct Base {
	double d;
};

struct Bar : Base{
	float x;
	virtual void foo_fn(){
		printf("hello from foo\n");
	}
	virtual void bar_fn(){
		printf("hello from foo\n");
	}
};
struct Baz : Bar{
	int y;
	void foo_fn(){
		printf("hello from baz\n");
	}
};
typedef void(*func_t)(Bar*);
void Func1(Bar*){
}
void Func2(Bar*){
}
void Func3(Bar*){
}


struct MyStruct{
	func_t f1,f2,f3;
};
MyStruct g_MyStruct={
	Func1,Func2,Func3
};

int main(int argc, const char** argv) {
	Baz* b=new Baz;
	int tmp[512];
	tmp[2]=10;
	tmp[5]=50;
	tmp[10]=123456;
	float x=10.0f,y=20.0f,z;
	void (*fp)(int) = foo;
	double d=x;
	bool b1=true;
	bool b2=argc>2;
	bool b3 = argv;
	printf("%p",fp);
	char* nullpt_test=1+(char*)nullptr;
	struct Foo* ptr=(struct Foo*) malloc(sizeof(struct Foo));
	float* ptr2=(float*)ptr;
	y+=5.2346f;
	x=0.035+0.0f;
	if (argc < 2){
		printf("hello world %.3f %.3f %d %d\n",y,100.0f,tmp[10],b2?1:2);
	}
		printf("<2");
	return 0;
}
#endif

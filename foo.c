#include <stdio.h>

float lerp(float a,float b, float f){
	return (b-a)*f+a;
}


#line 3 "test.rs"
int main(int argc, const char** argv) {
	int tmp[512];
	tmp[2]=10;
	tmp[5]=50;
	tmp[10]=123456;
	float x=10.0f,y=20.0f,z;
	if (argc < 2){
		printf("hello world %.3f %d\n",z,tmp[10]);
	}
		printf("<2");
	return 0;
}

fn lerp(a,b,f)->float{a+(b-a)*f};
fn foo1(a:*char)->void;

fn printf(s:str,...)->int;

struct Foo {
	vx:int, vy:int, vz:int
}

struct Vec4{ vx:int,vy:int,vz:int,vw:int}

struct Triangle{ i0:int,i1:int,i2:int}

struct Mesh {
	vertices:array[Vec4,20],
	triangles:array[Triangle,4]
}

fn something(f:*Foo){
	printf("f.x %d,.y %d,.z %d\n", f.vx, f.vy, f.vz)
}
fn something_int(x:int){
	printf("something overloaded for int,given %d\n",x);
}
fn call_ptr(f:fn(int)->void){
	f(5);
}


fn main(argc:int,argv:**char)->int{
	x:=0;
	call_ptr(fn(lambda_arg){printf("hello from closure %d\n",lambda_arg);});
	retval:=0;
	something_int(retval);
	acc:=retval;
	for i:=0,j:=0; i<10; i+=1,j+=10 {
		x+=i;
		printf("i,j=%d,%d,x=%d\n",i,j,x);
	}else{
		printf("loop exit fine\n");
	}
	x:=if argc<2{printf("argc is <2\n");1}else{printf("argc is>2\n");2};
	printf("yada yada yada\n");
	acc
}


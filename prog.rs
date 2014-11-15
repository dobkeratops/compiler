fn lerp(a,b,f)->float{(b-a)*f+a};
fn foo(a:*char)->void;
fn printf(s:str,...)->int;

struct Foo {
	vx:int, vy:int, vz:int
}

fn something(f){
	printf("f.x,.y,.z\n", f.vx, f.vy, f.vz);
}

fn main(argc:int,argv:**char)->int{
	xs=:array[int,512];
	q:=xs[1];
	p1:=&xs[1];
	xs[2]=000;
	xs[2]+=400;
	*p1=30;
	z:=5;
	y:=xs[1]+z+xs[2];
	x:=0;

	for i:=0,j:=0; i<10; i+=1,j+=10 {
		x+=i;
		printf("i,j=%d,%d,x=%d\n",i,j,x);
	}else{
		printf("loop exit fine\n");
	}

	x:=if argc<2{printf("<2");1}else{printf(">2");2};
	printf("yada yada yada\n");
	printf("\nhhhhello world %.3f %d\n", lerp(10.0,20.0,0.5),y );0
}


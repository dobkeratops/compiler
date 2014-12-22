#!hack -r

// Uses .rs extention for syntax highlighting, but this is NOT Rust source.

struct FILE;
extern"C"fn printf(s:str,...)->int;
extern"C"fn fopen(d:*char,z:*char)->*FILE;
extern"C"fn fclose(d:*FILE)->int;
extern"C"fn fread(d:*void,z:size_t,n:size_t,f:*FILE)->size_t;
extern"C"fn fwrite(d:*void,z:size_t,n:size_t,f:*FILE)->size_t;
extern"C"fn sqrt(f:float)->float;

// template typeparameter sugar - just omit types, and they get typeparams automatically
// eg lerp<A,B,F>(a:A,b:B,f:F)... ; also single-expression sugar.

fn lerp(a,b,f)=(b-a)*f+a;
fn invlerp(x0,x1,x)=(x-x0)/(x1-x0);

//  closure arguments declared like rust
fn take_closure(funcp:|int|){
	funcp(10);
}

// Rust style enum
enum Shape {
	Sphere(Vec3,float),
	Cuboid{min:Vec3,max:Vec3},
	Cylinder(float,float),
};
// Rust style match
fn shape_vol(s:*Shape)->float= match s{
	*Sphere(my_centre, my_radius)=>{	// destructuring like rust
		printf("match sphere vol\n");
		4.0/3.0*3.142* my_radius*my_radius*my_radius
	},
	*Cuboid(vmin, vmax)=>{	// tuple-structs/named fields are generalized here.
		printf("match cuboid vol\n");
		d:=vmax-vmin; d.vx*d.vy*d.vz 
	},
	*Cylinder(radius,height)=>3.142*radius*radius*height,
	_ =>{printf("shape error\n");0.0}
};


// 'where' expression sugar ..sort salient info on one line
fn interpolate(x,x0,y0,x1,y1)=(ofsx/dx)*dy+y0 where{
	ofsx:=x-x0;dx:=x1-x0;dy:=y1-y0;
};

// HKT-HigherKindedTypes, 'template-template parameters' 
// - adhoc synax is less verbose than C++ .. 

fn map<V,A,B>(src:&V<A>, f:|*A|->B)-> V<B>{ 
	let result=reserve(src.size());
	for index:=0; index<src.size(); index+=1 {
		push_back(&result, f(src.get(index)));
	}
	result
}

// struct declarations like Rust.  fieldname:Type,...
struct Foo {
	vx:int, vy:int, vz:int
}
struct Vec3{ vx:float,vy:float,vz:float};

// operator overloading like C++. (using fn keyword allows parsing operators as names)
fn +(a:&Vec3,b:&Vec3){ Vec3{vx=a.vx+b.vx,vy=a.vy+b.vy,vz=a.vz+b.vz} }

//single-expression syntax with struct-constructors. Infering the return type when you can see it is pleasant.
fn -(a:&Vec3,b:&Vec3)= Vec3{vx:a.vx-b.vx,vy:a.vy-b.vy,vz:a.vz-b.vz};
fn |(a:&Vec3,b:&Vec3)=a.vx*b.vx + a.vy*b.vy + a.vz*b.vz;
fn *(a:&Vec3,f:float)=Vec3{a.vx*f,a.vy*f,a.vz*f};
// eg cross product with less nesting.
fn ^(a:&Vec3,b:&Vec3)=Vec3{
	a.vy*b.vz-a.vz*b.vy,
	a.vz*b.vx-a.vx*b.vz,
	a.vx*b.vy-a.vy*b.vx
};
fn length(a:&Vec3)=sqrt(a|a);
fn normalize(a:&Vec3)=a*(1.0/length(a));

// bilerp implemented using generic 'lerp' defined above
fn bilerp(a0:&Vec3,b0:&Vec3,a1:&Vec3,b1:&Vec3,u:float,v:float)=lerp(lerp(a0,a1,u),lerp(b0,b1,u),v);

// internal vtables
// simplified -'base class' must describe entire layout.
// needed because the compiler uses C++ vtables & I want to self host by transpiling.
// I wouldn't have bothered otherwise.
// class heirachies & vtables are not the focus
// ideally i'd just have UFCS and freefunctions

struct IBaz {
	// sugar: with other qualifiers, 'fn' is optional,assumed.
	virtual foo(){}  
	virtual bar(){}  
}



// adhoc overloading like C++; most specific function is matched at callsite

fn something_foo(f:*Foo){
	printf("something_foo with 1 arg overloaded\n");
	printf("f.x= %d\n", f.vx);
}
fn something_foo(f:*Foo,x:*Foo){
	printf("something_foo with 2 args overloaded\n");
	printf("f.x= %d,.y= %d,.z= %d\n", f.vx,f.vy,f.vz);
}
fn something(f:*Foo){
	printf("something overloaded with &Foo param\n");
	printf("f.x= %d,.y= %d,.z= %d\n", f.vx, f.vy, f.vz);
}
fn something(f:float,x){
	printf("something overloaded with float param & templated param 'x'\n");
	printf("something(float, auto)\n");
}

// tagged-union implemented using templates.
// still has interesting possibilites that inbuilt enum doesn't
// perhaps the actual enum/discriminant mechanism itself could be overloadable..
struct Union[X,Y]{ // [T] and <T> both supported . want '[]' but <> is convention
	tag:int,
	x:X,y:Y,
};

fn match_with[X,Y,R]( // we will need to make overloaded permutations
	u:&Union[X,Y], // 1st param is a ref ,not pointer, so it can autoderef.
	fx:|*X|->R,
	fy:|*Y|->R)
	->R{
	if u.tag==0 { fx(&u.x)} else {fy(&u.y)}
}

fn setv[X,Y](u:&Union[X,Y],y:Y)->void{
	printf("setv Y\n");
	u.y=y;
	u.tag=1;
}

fn setv[X,Y](u:&Union[X,Y],x:X)->void{
	printf("setv X\n");
	u.x=x;
	u.tag=0;
}


fn main(argc:int,argv:**char)->int{

	printf("example program ./hello.rpp compiled & run by default makefile\n");

	// closure syntax stolen from Rust |args,..|{body...}
	let captured_y=15;
	take_closure(|x|{printf("closure1 says x=%d y=%d\n",x,captured_y);});

	// sugar for closure as last arg, foo(..)do x{...}  === foo(..,|x|{...})
	// similar to rusts' lost "DO" notation - 
	// but 'do' keyword is still free for another use as a prefix
	take_closure() do x{
		printf("closure2 says x=%d y=%d\n",x,captured_y);
	}

	let v0=Vec3{1.0,0.0,0.0};
	let v1=Vec3{0.0,1.0,0.0};
	let v2=v0+v1;

	// Demo rust-style enums
	let s1=new Sphere{Vec3{1.0,1.0,1.0},1.0};
	let s2=new Cuboid{Vec3{1.0,1.0,1.0},Vec3{2.0,2.0,2.0}};
	let sv1=shape_vol(s1 as *Shape);
	let sv2=shape_vol(s2 as *Shape);



	// let for introducing variable, rather than just ident:T
	// :T would be used as a type-assertion
	// its still possible Rust might change to ':' for 'as'?
	// this uses ":" in dumps for postfixing type info & it would be
	// useful with so much inference going on to keep that language syntax.

	let u:Union<int,float>; 
	let banana:struct{uuu:int,vvv:float}
	banana.uuu=50;
	take_banana(&banana);

	let v:Union<float,int>;

	// calls to templated functions,& using UFCS.. test'variant' template..
	u.setv(2.0);
	u.setv(5);

	let tup=tuple_test();
	let tup2=tuple_test2();
	printf("tuple components %d %d %d\n",tup.0, tup.1, tup.2);

	let foo=ret_anon_struct();
	printf("anon struct fields= %d %d\n",foo.x, foo.y);

	// rust style matching.. nesting destructuring with |, if guards
	for y:=0; y<8; y+=1{
		for x:=0; x<8; x+=1{
			match (x,y){
				(2|5,_)					=>printf("X"),
				(_,2|5)					=>printf("Y"),
				_ if x==y || (7-x)==y	=>printf("o"),
				_						=>printf(".")
			};
		};
		printf("\n");
	};


	// type inference with templates & lambdas
	// unlike C++, the output type of the lambdas infer back to  'R' here
	// in c++ you'd have  to specify the output type eg u.match_with<float>(....)

	let z=u.match_with(
		|x:*int|{printf("union was set to int %d\n",*x);15},
		|x:*float|{printf("union was set to float %.3f\n",*x);17}
	);
	printf("map union returned z=%d\n",z);

	// C-like for loops minus parens, compulsory {}
	// handles simple cases without needing a whole iterator library
	// enhanced with 'break'-'else' expressions

	acc:=0;	
	//:= from 'go', x:=y is a shortcut for let x:=y .
	// still might be useful eg pattern-based let can't have a return value, := could.
	value:=for i:=0,j:=0; i<10; i+=1,j+=10 {
		acc+=i;
		for k:=0; k<10; k+=1 {
			printf("i=%d,k=%d,acc=%d\n",i,k,acc);
			if k==5{
				printf("break from inner loop\n");
				break break 55; // read like break(break 55). 2 levels
			}
		}
		if j==6 {break 66;} // break with return expression.
	}else{
		// 'else' called if no break, needed for return value
		printf("loop exit fine\n"); 
		44
	};
	printf("loop return value = %d\n",value);
	

	let fv=Foo{vx:13,vy:14,vz:15}; // initialize struct on stack, named fields

	something_foo(&fv,&fv);
	printf("fv.vx=%d\n",fv.vx);

	let fv2=new Foo{vx:23,vy:24,vz:25}; // struct allocate & init
	something_foo(fv2);

	let fv3=new Foo{31,32,33}; // struct initializer, sequential
	something_foo(fv3);

	// Test UFCS+overloading .. see definitions of foo_bar below.
	// real member functions are for vtable layout & C++ compatability

	foo_bar(fv3,0.4);
	fv3.foo_bar(fv3,(77,88,99));
	fv3.foo_bar(fv3,fv3);

	// test arrays and ptrs work
	// 'x as *T' for raw pointer casts, like C (T*)x
	// &p[index] used for pointer offsetting, no ptr+ofs yet.

	let my_array:array<int,512>;   // like C++ array<int,512>
	let q=my_array[1];
	my_array[2]=10;
	my_array[2]+=400;
	let p1=&my_array[1];
	*p1=30;
	p1[3]=77;			// raw ptr offseting (&xs[1])[3] == xs[4]
	let z=5;
	let y=my_array[1]+z+my_array[2];
	printf("test refs to array:%d %d %d\n",my_array[2], *p1,my_array[4]);

	let pbaz1= new Qux{x=66};
	let pbaz2= new Bar{y=77};

	do_something(pbaz1 as*IBaz);//TODO autocoerce to base type
	do_something(pbaz2 as*IBaz);

	// if-else expressions like Rust.
	let x1=if argc<2{printf("argc %d <2",argc);1}else{printf("argc %d >=2",argc);2};
	printf("\n**Hello World** %d %d\n", x1, y );

	// last statement is a return value, like Rust. significant semicolons

	0
}

fn tuple_test2()->(int,float,int){	// tuples for multiple return,like Rust
	(12,23.0,34)
}
fn tuple_test(){	// rturn type inference
	(12,23,34)
}

fn do_something(p:*IBaz){
	printf("do something %p\n",p);
	p.foo();
}

// implementing vtable based 'classes', like C++
// only single-inheritance planned.

struct Qux : IBaz {
	x:int;
	fn foo(){
		printf("hello from Qux.foo this=%p this.x=%d\n",this,x);
	}
	fn bar(){
		printf("hello from Qux.bar this=%p this.x=%d\n",this,x);
	}
}

struct Bar : IBaz {
	y:int;
	fn foo(){
		printf("hello from Bar.foo this.y=%d\n",y);
	}
	fn bar(){
		printf("hello from Bar.bar this.y=%d\n",y);
	}
}
// multiple *named* return values as anonymous struct
// need to write 'struct' because type-context doesn't  include braces
// if we name it, it doesn't go into any scope at the minute.

fn ret_anon_struct()->struct{x:int, y:int}{
	let r; // infered from return value.
	r.x=88;
	r.y=99;
	r
	//could also write _{x:88,y:99}
}
fn foo_bar(i:int){
}
fn foo_bar(p:*void){
}
fn foo_bar(p:*Foo,f:float){
	printf("2arg foo_bar %p %f\n",p,f);
}
fn foo_bar(p:*Foo,q:*Foo,(i,j,k):(int,int,int)){
	printf("3arg foo_bar with tuple destructure %p %d %d %d\n",p,i,j,k);
}
fn foo_bar(p:*Foo,q:*Foo,w:*Foo){
	printf("3ptr foo_bar %p %p\n",p,w);
}

fn take_banana(x){
	printf("banana anon struct in adhoc template x=%d \n",x.uuu);
}


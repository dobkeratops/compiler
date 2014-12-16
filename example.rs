#!hack -r

// Uses .rs extention for syntax highlighting, but this is not Rust source.
// omit function body to declare prototypes for external linking,"C" linkage optional, otherwise its' a C++ name-mangle with overloaded types.
struct FILE;
extern"C"fn printf(s:str,...)->int;
extern"C"fn fopen(d:*char,z:*char)->*FILE;
extern"C"fn fclose(d:*FILE)->int;
extern"C"fn fread(d:*void,z:size_t,n:size_t,f:*FILE)->size_t;
extern"C"fn fwrite(d:*void,z:size_t,n:size_t,f:*FILE)->size_t;

// stolen from rust: function syntax 'fn <function name>(args) optional return value {body}
// however ommitting return value means infer it, not 'void' 

fn something(f:float){
}

// typeparameter sugar -omitted types get typeparams automatically,
// eg
// template<class A,class B,class F>
//    auto lerp(A a,B b, F f){return (b-a)*f+a;}
// more specific overloads are always used in preference if given

fn lerp(a,b,f)=(b-a)*f+a;

//  single expression fn sugar '='
fn invlerp(x0,x1,x)=(x-a)/(x1-x0);

//  declare a function taking a closure:
//  represented as a pair of pointers (function*, environment*)
//  raw C like functions are currently written fn(int)->void 

fn take_closure(funcp:|int|){
	funcp(10);
}

// more expression sugar ..sort salient info on one line
fn interpolate(x,x0,y0,x1,y1)=(ofsx/dx)*dy+y0 where{
	ofsx:=x-x0;dx:=x1-x0;dy:=y1-y0;
};


// HKT-HigherKindedTypes, 'template-template parameters' 
// - adhoc synax is less verbose than C++ .. 
// might want more specific declaration but
// does the use in the parameter list say enough?

fn map<V,A,B>(src:&V<A>, f:|&A|->B)-> V<B>{
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

// internal vtables
// simplified implementation - base must describe whole vtable layout
// 'inherited' types overide base class functions & can override eachother.
// this language does not focus on class-heirachies
// other mechanisms to follow (trait objects) 
// & switch-like dispatch can be done with templates.
// performance code sorts by type anyway.
//
// have implemented this for self-hosting.. the compiler in C++ uses them.

struct IBaz {
	// sugar: with other qualifiers, 'fn' is optional,assumed.
	virtual foo(){}  
	virtual bar(){}  
}
// in a 'trait', functions default to 'virtual'.
//struct IBar {
//	fn bar(selfptr){}     
//}

// open overloading like C++; most specific function is matched at callsite
// f:&Foo means parameter 'f' , reference to Foo.. 

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

// this isn't a union yet, its just  test to show the type-inference
// can handle getting a 'tag' from methods matching type X or Y
// will probably introduce propper tagged unions like Rust, 
// but want the template engine able to handle rolling pleasant custom variants
// (TODO: max[sizeof[X],sizeof[Y]] operators in template engine..)
//
// raw pointers can implement anything.. variants could be in precompiled
// datastructures with variable size, or they could be an tag+owned-pointer

struct Union[X,Y]{ // [T] and <T> both supported . want '[]' but <> is convention
	tag:int,
	x:X,y:Y,
};

fn map[X,Y,R](
	u:&Union[X,Y],
	fx:|*X|->R,
	fy:|*Y|->R)
	->R{
	if u.tag==0 { fx(&u.x)} else{fy(&u.y)}
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

	// let for introducing variable, rather than just ident:T
	// :T would be used as a type-assertion
	// its still possible Rust might change to ':' for 'as'?
	// this uses ":" in dumps for postfixing type info & it would be
	// useful with so much inference going on to keep that language syntax.

	let u:Union<int,float>; 
	let banana:struct{uuu:int,vvv:float}
	banana.uuu=50;
	take_banana(&banana);

	// alternate syntax for declaring  a new uninitialized variable of type
	v=:Union<float,int>;

	// calls to templated functions .. setting value & tag of the variant
	u.setv(2.0);
	u.setv(5);

	let tup=tuple_test();
	let tup2=tuple_test2();
	printf("tuple components %d %d %d\n",tup.0, tup.1, tup.2);

	let foo=ret_anon_struct();
	printf("anon struct fields= %d %d\n",foo.x, foo.y);
 
	// type inference with polymorphic lambdas
	// could overload 'map' to supply different combinations of types
	// C++ equivalent doesn't seem to match all template args.
	// as far as i've tried it.. you always need to specify 
	// a parameter manually

//	let pu=&u;
	let z=u.map(
		|x:*int|{printf("union was set to int %d\n",*x);15},
		|x:*float|{printf("union was set to float %.3f\n",*x);17}
	);
	printf("map union returned z=%d\n",z);

	// C-like for loops minus parens, compulsory {}
	// handles simple cases without needing a whole iterator library..
	//
	// enhanced with expression syntax: break <expr> , else {expr}
	// type of 'value' is infered from the break/else expressions
	// for-else completes rusts' "everything-is-an-expression" philosophy

	let acc=0;
	//:= from 'go', x:=y is a shortcut for let x:=y 
	value:=for i:=0,j:=0; i<10; i+=1,j+=10 {
		acc+=i;
		for k:=0; k<10; k+=1 {
			printf("i,k=%d,%d,x=%d\n",i,k,acc);
			if k==5{
				printf("break from inner loop\n");
				break break 55; // read like break(break 55). 2 levels
			}
		}
		if j==6 {break 66;} // break with return expression.
	}else{
		// for..else block called if no 'break'. 
		// else blocks needed to complete loops as expressions.
		printf("loop exit fine\n"); 
		44
	};
	printf("loop return value = %d\n",value);

	// Struct initializers...

	let fv=Foo{vx=13,vy=14,vz=15}; // initialize struct on stack, named fields
	something_foo(&fv,&fv);
	printf("fv.vx=%d\n",fv.vx);

	let fv2=new Foo{vx=23,vy=24,vz=25}; // struct allocate & init
	something_foo(fv2);

	let fv3=new Foo{31,32,33}; // struct initializer, sequential
	something_foo(fv3);

	// Test UFCS+overloading .. see definitions of foo_bar below.
	// if designing a language in a vacuum , I would have done 100% UFCS
	// there are 'member-functions' purely 
	// to ease translation/interfacing to & from C++.

	foo_bar(fv3,0.4);
	fv3.foo_bar(fv3,77);
	fv3.foo_bar(fv3,fv3);

	// test arrays and ptrs work
	// 'x as *T' for raw pointer casts, like C (T*)x
	// unsafe like C. safety can come later
	// whats most important now is the elegant mechanisms for avoiding raw ptrs

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

	// Expression syntax stolen from rust.
	// if..else.. has a return value;more flexible than ternary op
	// because it uses compound statements
	// frees up '?' for other use, eg optional types..(TODO arbitrary operators)
	// last expression in the compound blocks is return value from block

	let x1=if argc<2{printf("argc %d <2",argc);1}else{printf("argc %d >=2",argc);2};
	printf("\n**Hello World** %d %d\n", x1, y );

	// last statement is a return value. 
	// takes some getting used to but makes semicolons significant and
	// interacts very well with expression syntax generally.

	0
}

fn do_something(p:*IBaz){
	printf("do something %p\n",p);
	p.foo();
}

// implementing vtable based 'classes', like C++
// TODO handle rust/go style trait-objects to complement.
// dont need C++ multiple-inheritance.
// 

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
fn tuple_test(){
	(12,23,34)
}
fn tuple_test2()->(int,float,int){
	(12,23.0,34)
}
// multiple *named* return values as anonymous struct
// need to write 'struct' because type-context doesn't  include braces
// if we name it, it doesn't go into any scope at the minute.

fn ret_anon_struct()->struct{x:int, y:int}{
	let r; // infered from return value.
	r.x=88;
	r.y=99;
	r
	//could also write _{88,99}
}
fn foo_bar(i:int){
}
fn foo_bar(p:*void){
}
fn foo_bar(p:*Foo,f:float){
	printf("2arg foo_bar %p %f\n",p,f);
}
fn foo_bar(p:*Foo,q:*Foo,i:int){
	printf("3arg foo_bar %p %d\n",p,i);
}
fn foo_bar(p:*Foo,q:*Foo,w:*Foo){
	printf("3ptr foo_bar %p %p\n",p,w);
}

fn take_banana(x){
	printf("banana anon struct in adhoc template x=%d \n",x.uuu);
}


#include "run_test.h"

extern int compile_and_run(const char *buffer, const char* filename, const char* outname, int flags,char** capture_output);

// todo: we also need tests that *can't* compile, eg type errors
struct CompilerTest {
	const char* name;
	const char* file;int line;
	const char* source;
	const char* expected_result;
};

// every file is an implicitly a function aswell taking no args
// when imported, a module inserts a call to that function.
// that sets up global stuff for it.

CompilerTest g_Tests[]={
	{"voidptr coerce test ",__FILE__,__LINE__,
		"struct FILE;\n"
		"fn voidpf(d:*void)->int{0};\n"
		"fn voidpfr(p:**char)->*void{p as*void};\n"
		"fn main(argc:int,argv:**char)->int{\n"
		"	voidpf(argv);"
		"	x:=voidpfr(argv);"
		"  0}"
	},

	{"adhoc template 2",__FILE__,__LINE__,
		"fn lerp(a,b,f)->float{(b-a)*f+a};\n"
		"fn main(argc:int,argv:**char)->int{\n"
		"  0}"
	},
	{"multi feature test 2",__FILE__,__LINE__,
		
		"fn map<V,A,B>(src:&V<A>, f:|&A|->B)-> V<B>{\n"
		"	let result=init();\n"
		"	for index:=0; index<src.size(); index+=1 {\n"
		"		push_back(&result, f(get(&src,index)));\n"
		"	}\n"
		"	result \n"
		"}\n"
		"fn\"C\" printf(s:str,...)->int;\n"
		"fn debugme[X,Y,R](u:&Union[X,Y], fx:(&X)->R,fy:(&Y)->R)->R{\n"
		" if u.tag==0 { fx(&u.x)}\n"
		" else { fy(&u.y)}\n"
		"}\n"
		"fn main(argc:int,argv:**char)->int{\n"
		"fv:=Foo{vx=13,vy=14,vz=15};\n"
		" u=:Union[int,float];\n"
		" setv(&u,0.0);\n"
		" setv(&u,0);\n"
		" z:=debugme(&u,											\n"
		"	|x:&int|{printf(\"union was set to int\\n\");10},	\n"
		"	|x:&float|{printf(\"union was set to float\\n\");12}	\n"
		"	);												\n"
		"printf(\"map union returns %d\\n\", z);						\n"
		"	xs=:array[int,512];\n"
		"q:=xs[1]; p1:=&xs[1];\n"
		"	xs[2]=000;\n"
		"	xs[2]+=400;\n"
		"	*p1=30;\n"
		"z:=5;\n"
		"y:=xs[1]+z+xs[2];\n"
		"x:=0;\n"
		"	something_foo(&fv,&fv);\n"
		"	for i:=0,j:=0; i<10; i+=1,j+=10 {\n"
		"		x+=i;\n"
		"		printf(\"i,j=%d,%d,x=%d\\n\",i,j,x);\n"
		"	}else{\n"
		"		printf(\"loop exit fine\\n\");\n"
		"	}\n"
		"		something_foo(&fv);\n"
		"		something(&fv);\n"
		"		take_closure(|x|{printf(\"closure says %d %d\\n\",x,y);})\n"
		"		\n"
		"		x:=if argc<2{printf(\"<2\");1}else{printf(\">2\");2};\n"
		"		printf(\"yada yada yada\\n\");\n"
		"		printf(\"\\nHello World %d\n\", y );\n"
		"		0\n"
		"		}\n"
		"fn lerp(a,b,f)->float{(b-a)*f+a};\n"
		"fn foo(a:*char)->void;\n"
		"struct Foo {\n"
		"vx:int, vy:int, vz:int\n"
		"}\n"
		"fn something_foo(f:&Foo){\n"
		"	printf(\"f.x= %d\\n\", f.vx);\n"
		"}\n"
		"fn something_foo(f:&Foo,x:&Foo){\n"
		"	printf(\"something_foo with 2 args overloaded\\n\");\n"
		"	printf(\"f.x= %d,.y= %d,.z= %d\\n\", f.vx,f.vy,f.vz);\n"
		"}\n"
		"fn something(f:&Foo){\n"
		"	printf(\"f.x= %d,.y= %d,.z= %d\\n\", f.vx, f.vy, f.vz);\n"
		"}\n"
		"fn something(f:float){\n"
		"}\n"
		"fn something(f:float,x){\n"
		"}\n"
		"fn take_closure(funcp:(int)->void){\n"
		"	funcp(10);\n"
		"}\n"
		"struct Union[X,Y]{\n"
		"tag:int,\n"
		"x:X,y:Y,\n"
		"};\n"
		"fn setv[X,Y](u:&Union[X,Y],x:Y)->void{\n"
		" printf(\"setv Y\\n\");\n"
		"}\n"
		"fn setv[X,Y](u:&Union[X,Y],x:X)->void{\n"
		" printf(\"setv X\\n\");\n"
		"}\n"
		,
		nullptr
	},
	{
		"internal vtable",__FILE__,__LINE__,
		"fn\"C\" printf(s:str,...)->int;				\n"
		"struct Foo {									\n"
		"	x:int,y:int,								\n"
		"	virtual v_foo(){printf(\"Foo.foo x=%d\\n\",x);},		\n"
		"	virtual bar(){printf(\"Foo.bar\\n\");},		\n"
		"	virtual baz(){printf(\"Foo.baz\\n\");},		\n"
		"}\n"
		"struct Bar : Foo{									\n"
		"	x:int,y:int,								\n"
		"	fn v_foo(){printf(\"Bar.foo x=%d\\n\",x);},		\n"
		"	fn bar(){printf(\"Bar.bar\\n\");},		\n"
		"	fn baz(){printf(\"Bar.baz\\n\");},		\n"
		"}\n"
		"fn main(argc:int, argv:**char)->int{	\n"
		"	x1:= new Foo{x=10,y=0};						\n"
		"	x2:= new Bar{x=20,y=0};						\n"
		"	x1.v_foo();							\n"
		"	take_interface(x1);							\n"
		"	take_interface(x2);							\n"
		"	0										\n"
		"}\n"
		"fn take_interface(pf:*Foo){\n"
		"   pf.v_foo()\n"
		"}\n"
		,nullptr
	},

	{
		"multi feature test 1",__FILE__,__LINE__,
		/* 1*/ "enum FooBar{Foo{x:int,y:int},Bar{p:float,q:float} }	\n"
		/* 3*/ "fn setv[A,B](u:&Union[A,B], v:A){\n"
		/* 4*/ "	u.a=v; u.tag=0; \n"
		/* 5*/ "}\n"/* 1*/
		/* 6*/ "fn setv[A,B](u:&Union[A,B], v:B){\n"
		/* 7*/ "	u.b=v; u.tag=1; \n"
		/* 8*/ "}\n"
		/* 9*/ "fn take_fn(pfunc:fn(int)->void){ pfunc(5);}\n"
		/*10*/ "fn take_closure(pfunc:(int)->void){ pfunc(5);}\n"
		/*11*/ "fn\"C\" printf(s:str,...)->int;\n"
		/*12*/ "fn foo_bar(x){ printf(\"Hello From generic\\n\"); }      \n"
		/*13*/ "fn foo(x:int){ printf(\"Hello From indirect 	functionpointer call %d\\n\",x); }      \n"
		/*14*/ "fn bar(x:int,y:int,z:int)->int{ printf(\"bar says %d\\n\",x+y+z);0};\n"
		/*15*/ "fn foo_struct(p:*FooStruct)->int{ printf(\"foostruct ptr has %d %d\\n\",p.x,p.y);0}"
		/*16*/ "fn something(f:int){\n"
		/*17*/ "	printf(\"somethng(int)\\n\");\n"
		/*18*/ "}\n"
		/*19*/ "fn something(f:float){\n"
		/*20*/ "	printf(\"somethng(int)\\n\");\n"
		/*21*/ "}\n"
		/*22*/ "fn main(argc:int, argv:**char)->int{	\n"
		/*23*/ "	xs=:array[int,512];					\n"
		/*24*/ "	q:=xs[1]; p1:=&xs[1];				\n"
		/*25*/ "	*p1=42;								\n"
		/*26*/ "	u=:Union[int,float];				\n"
		/*27*/ "	setv(&u,10)	;						\n"
		/*28*/ "	printf(\"u.tag=%d\\n\",u.tag);		\n"
		/*29*/ "	setv(&u,10.0)	;					\n"
		/*30*/ "	printf(\"u.tag=%d\\n\",u.tag);		\n"
		/*31*/ "	retval:=0;							\n"
		/*32*/ "	x:= {a:=10;b:=20; a+b};				\n"
		/*33*/ "	x+=10;								\n"
		/*34*/ "	fp:=foo;							\n"
		/*35*/ "	xs=:array[int,512];  				\n"
		/*36*/ "	p2:=&xs[1];  						\n"
		/*37*/ "	xs[1]+=3;							\n"
		/*38*/ "	fs:=FooStruct{0xff,0x7f};			\n"
		/*39*/ "	something(1);						\n"
		/*40*/ "	pfs:=&fs;							\n"
		/*41*/ "	foo_struct(&fs);					\n"
		/*42*/ "	fn localtest(i:int)->void{			\n"
		/*43*/ "		printf(\"hello from local fn %d\\n\",i);	\n"
		/*44*/ "	}; 									\n"
		/*43*/ "	py:=pfs as *int;					\n"
		/*44*/ "	foo_bar(&fs);						\n"
		/*45*/ "	printf(\"foostruct int val recast %d; foostruct raw value %d %d\n\",*py,fs.y,pfs.y);							\n"
		/*46*/ "	fp(2);fp(x);fp(xs[1]);				\n"
		/*47*/ "	take_fn(fp);						\n"
		/*48*/ "	take_fn(localtest);					\n"
		/*49*/ "	localtest(10);						\n"
		/*50*/ "	take_fn(fn(x){printf(\"hello from anon function %d\\n\",x);});		\n"
		/*51*/ "	take_closure(|y|{printf(\"hello from closure function x=%d y=%d\\n\",x,y);});\n"
		/*52*/ "	bar(1,2,3);							\n"
		/*53*/ "	retval					\n"
			"};								\n"
		/*54*/ "struct FooStruct{x:int,y:int};		\n"
		/* 2*/ "struct Union[A,B]{a:A,b:B, tag:int};\n"
		
		,
		nullptr
	},
{

	
	
	"member functions",__FILE__,__LINE__,
	// SOURCECODE
/*1*/  "fn\"C\" printf(s:str,...)->int;  							\n"
/*2*/  "struct Foo{												\n"
/*3*/  "	q:int,												\n"
/*4*/  "	fn method()->float{										\n"
/*5*/  "		printf(\"Foo.q=%d\\n\",q);2.0		\n"
/*6*/  "	}													\n"
/*7*/  "}														\n"
/*8*/  "struct Bar{												\n"
/*9*/  "	w:int,												\n"
/*10*/ "	fn method()->float{										\n"
/*11*/ "		printf(\"Bar.w=%d\\n\",w);2.0		\n"
/*12*/ "	}													\n"
/*13*/ "}														\n"
/*14*/ "fn func1(f:*Foo){printf(\"func1 says q=%d\\n\",f.q);}	\n"
/*14*/ "fn main()->int{							\n"
/*15*/ "	x:=Foo{5};	px:=&x;	y:=Bar{17}; py:=&y;\n"
/*16*/ "	printf(\"member function test..\n\",x.q);				\n"
/*17*/	"	px.func1();	\n"
/*18*/	"	px.method();	\n"
/*19*/	"	py.method();	\n"
/*20*/	"	0\n"
/*20*/  "}														\n"
	,
	// RESULT
"member function test..\n"
"func1 says q=5\n"
"Foo.q=5\n"
"Bar.w=17\n"
},
{
		"closures",__FILE__,__LINE__,
/*1*/ 	"fn\"C\" printf(s:str,...)->int;  							\n"
/*10*/	"fn take_closure(pfunc:|int|->void){ pfunc(5);}\n"
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	y:=11;z:=12;w:=0; y+=10;w+=7;			\n"
/*51*/	"	take_closure()do x{printf(\"closure x=%d captured y=%d z=%d\\n\",x,y,z);}\n"
"printf(\"y=%d z=%d w=%d\\n\",y+=90,z,w);\n"
/*17*/	"	0\n"
/*20*/  "}														\n"
	,
// Result
"closure x=5 captured y=21 z=12\n"
"y=111 z=12 w=7\n"
},
{
	"allocation",__FILE__,__LINE__,
/*1*/ 	"fn\"C\" printf(s:str,...)->int;  			\n"
/*2*/	"struct Foo{x:int,y:int};				\n"
/*3*/	"fn main(argc:int, argv:**char)->int{	\n"
/*4*/	"	pfoo:= new Foo{4,5};			\n"
/*5*/	"	pfoos:= new Foo[10];			\n"
/*6*/	"	pfoos[1].x=10;					\n"
/*7*/	"	printf(\"new foo %p x,y=%d,%d array alloc=%p\\n\",pfoo,pfoo.x,pfoo.y,pfoos);			\n"
/*8*/	"	delete pfoo;					\n"
/*9*/	"	0\n"
/*10*/  "}														\n",
	nullptr
},
{
	"let",__FILE__,__LINE__,
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	let x=2;\n"
"y:=3;\n"
"let z=x+y;			\n"
/*17*/	"	0\n"
/*20*/  "}														\n",
	nullptr
},
{
	"struct",__FILE__,__LINE__,
/*54*/ "	struct FooStruct{x:int,y:int};		\n"
/*  */	"fn main(argc:int, argv:**char)->int{		\n"
/*  */	"	x:=FooStruct{1,2};\n"
/*17*/	"	0\n"
/*20*/  "}														\n",
	nullptr
},
{
	"if expression",__FILE__,__LINE__,
/*1*/	"extern \"C\" fn printf(s:str,...)->int;				\n"
/*2*/	"fn main(argc:int, argv:**char)->int{	\n"
"  x:=if argc<3{4} else{3};\n"
/*14*/	"	0									\n"
/*15*/	"}\n",
	nullptr
},
{
	"for  else loop",__FILE__,__LINE__,
/*1*/	"fn\"C\" printf(s:str,...)->int;				\n"
/*2*/	"fn main(argc:int, argv:**char)->int{	\n"
/*3*/	"	i:=5; b:=argc<9;						\n"
/*4*/	"	v:=for i:=0,j:=0;		\n"
/*5*/	"			i<10;			\n"
/*6*/	"			i+=1,j+=7 {	\n"
/*7*/	"		printf(\"for loop i=%d j=%d\\n\",i,j);	\n"
/*8*/	"		if i==5 {break 44;}						\n"
/*9*/	"	}									\n"
/*10*/	"	else{								\n"
/*11*/	"		printf(\"loop complete i=%d\\n\",i);55\n"
/*12*/	"	}									\n"
/*13*/	"	printf(\"loop ret=%d; outer scope i=%d\\n\",v,i);	\n"
/*14*/	"	0									\n"
/*15*/	"}\n",
	nullptr
},
{
	"type parameter inference",__FILE__,__LINE__,
/* 1*/ "struct Union<A,B>{a:A,b:B, tag:int};		\n"
/* 2*/ "fn setv[A,B](u:&Union[A,B], v:A)->void{		\n"
/* 3*/ "	u.a=v; u.tag=0; 						\n"
/* 4*/ "}											\n"
/* 5*/ "fn setv[A,B](u:&Union[A,B], v:B)->void{		\n"
/* 6*/ "	u.b=v; u.tag=1; 						\n"
/* 7*/ "}											\n"
/* 8*/ "fn main(argc:int, argv:**char)->int{		\n"
/* 9*/ "	u=:Union[int,float];					\n"
/*10*/ "	setv(&u,10)								\n"
/*11*/ "	printf(\"u.tag=%d\\n\",u.tag);			\n"
/*12*/ "	setv(&u,10.0)	;						\n"
/*13*/ " printf(\"u.tag=%d\\n\",u.tag);				\n"
/*14*/ "	0}										\n"
/*15*/ "fn\"C\" printf(s:str,...)->int;					\n"
,
// expected result
"u.tag=0\n"
"u.tag=1\n"

},
{"multi feature test 2",__FILE__,__LINE__,

"fn map<V,A,B>(src:&V<A>, f:|&A|->B)-> V<B>{\n"
"	let result=init();\n"
"	for index:=0; index<src.size(); index+=1 {\n"
"		push_back(&result, f(get(&src,index)));\n"
"	}\n"
"	result \n"
"}\n"
"fn\"C\" printf(s:str,...)->int;\n"
"fn debugme[X,Y,R](u:&Union[X,Y], fx:(&X)->R,fy:(&Y)->R)->R{\n"
" if u.tag==0 { fx(&u.x)}\n"
" else { fy(&u.y)}\n"
"}\n"
"fn main(argc:int,argv:**char)->int{\n"
"fv:=Foo{vx=13,vy=14,vz=15};\n"
" u=:Union[int,float];\n"
" setv(&u,0.0);\n"
" setv(&u,0);\n"
" z:=debugme(&u,											\n"
"	|x:&int|{printf(\"union was set to int\\n\");10},	\n"
"	|x:&float|{printf(\"union was set to float\\n\");12}	\n"
"	);												\n"
"printf(\"map union returns %d\\n\", z);						\n"
"	xs=:array[int,512];\n"
"q:=xs[1]; p1:=&xs[1];\n"
"	xs[2]=000;\n"
"	xs[2]+=400;\n"
"	*p1=30;\n"
"z:=5;\n"
"y:=xs[1]+z+xs[2];\n"
"x:=0;\n"
"	something_foo(&fv,&fv);\n"
"	for i:=0,j:=0; i<10; i+=1,j+=10 {\n"
"		x+=i;\n"
"		printf(\"i,j=%d,%d,x=%d\\n\",i,j,x);\n"
"	}else{\n"
"		printf(\"loop exit fine\\n\");\n"
"	}\n"
"		something_foo(&fv);\n"
"		something(&fv);\n"
"		take_closure(|x|{printf(\"closure says %d %d\\n\",x,y);})\n"
"		\n"
"		x:=if argc<2{printf(\"<2\");1}else{printf(\">2\");2};\n"
"		printf(\"yada yada yada\\n\");\n"
"		printf(\"\\nHello World %d\n\", y );\n"
"		0\n"
"		}\n"
"fn lerp(a,b,f)->float{(b-a)*f+a};\n"
"fn foo(a:*char)->void;\n"
"struct Foo {\n"
"vx:int, vy:int, vz:int\n"
"}\n"
"fn something_foo(f:&Foo){\n"
"	printf(\"f.x= %d\\n\", f.vx);\n"
"}\n"
"fn something_foo(f:&Foo,x:&Foo){\n"
"	printf(\"something_foo with 2 args overloaded\\n\");\n"
"	printf(\"f.x= %d,.y= %d,.z= %d\\n\", f.vx,f.vy,f.vz);\n"
"}\n"
"fn something(f:&Foo){\n"
"	printf(\"f.x= %d,.y= %d,.z= %d\\n\", f.vx, f.vy, f.vz);\n"
"}\n"
"fn something(f:float){\n"
"}\n"
"fn something(f:float,x){\n"
"}\n"
"fn take_closure(funcp:(int)->void){\n"
"	funcp(10);\n"
"}\n"
"struct Union[X,Y]{\n"
"tag:int,\n"
"x:X,y:Y,\n"
"};\n"
"fn setv[X,Y](u:&Union[X,Y],x:Y)->void{\n"
" printf(\"setv Y\\n\");\n"
"}\n"
"fn setv[X,Y](u:&Union[X,Y],x:X)->void{\n"
" printf(\"setv X\\n\");\n"
"}\n"
,
nullptr
},
{
	"let array",__FILE__,__LINE__,
"fn main(argc:int, argv:**char)->int{		\n"
"	let xs:array[int,10];\n"
"	let ptr={&xs[1]};\n"
"	ptr[1]=5;\n"
"	0\n"
"}			\n",
	nullptr
},
{
	"HKT (template template parameters)",__FILE__,__LINE__,
"fn map[C,T,Y](src:C[T],f:|T|->Y)->C[Y]{		\n"
"	let result;									\n"
"	result										\n"
"}												\n"
"												\n"
"struct Vec[T]{data:*T,num:int}"
"fn main(argc:int, argv:**char)->int{			\n"
"	let vec:Vec[int];\n"
"	let vec2=map(vec,|x|{0.0});\n"
"	0		\n"
"}			\n",
	nullptr
},
{
	nullptr,nullptr,0,nullptr,nullptr
}
};


void run_tests(){
	int index=0;
	for (auto t=g_Tests; t->name; t++,index++){
		char tmp[256]; sprintf(tmp,"test_%d.ll",index);
		printf("\nRunning Test[%d]: %s\n\n",index,t->name);
		char* output=0;
		auto ret=
		compile_and_run(t->source,t->name, tmp,B_DEFS|B_TYPES|B_RUN, t->expected_result?&output:nullptr);
		if (ret!=0) {
			printf("\n%s test %s failed\n", t->name);
			exit(-1);
		}
		if (output){
			if (strcmp(output, t->expected_result)){
				printf("\n%s:%d: Test[%d]\"%s\" gave incorrect output, expected:-\n",t->file,t->line,index,t->name);
				dbprintf("[length=%d]\n%s\n",strlen(t->expected_result),t->expected_result);
				dbprintf("[length=%d]\n%s\n",strlen(output),output);
				exit(-1);
			}
			free(output);
		}
	}
}

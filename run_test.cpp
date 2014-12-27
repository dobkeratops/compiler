#include "run_test.h"

extern int compile_and_run(const char *buffer, const char* filename, const char* outname, int flags,char** capture_output);

// todo: we also need tests that *can't* compile, eg type errors
struct CompilerTest {
	const char* name;
	const char* file;int line;
	const char* source;
	const char* expected_output;
	bool		should_fail;
};

CompilerTest g_Tests[]={

	{	"enum decl/assign",__FILE__,__LINE__,R"====(
		
		fn"C" printf(s:str,...)->int;
		enum Foo{
			Bar{x:int,y:int},
			Baz{x:float,y:float,z:int},
			Qux{x:u8},Boo
		};
		fn main(argc:int,argv:**char)->int{
			let x=Bar{};
			let f=Foo{};
			let pf:*Foo;
			f=x;
			pf=&x;	// TODO - coercion of reference doesn't work,only pointers & values, why not..
			0
		}
		)====",
		nullptr,false
	},

	{	"parse struct-trait-impl",__FILE__,__LINE__,R"====(
		extern"C" fn printf(s:str,...)->int;
		struct Foo{
			x:int,y:int
			fn a_method(){
				printf("Foo.a_method\n");
			}
		};
		trait Object {
			fn render(&self);
			fn update(&self);
		};
		impl Obj for Foo {
			fn render(&self){
				printf("Foo.render\n");
			}
			fn update(&self){
				printf("Foo.update\n");
			}
		}
		fn main(argc:int, argv:**char)->int{
			let x:Foo; let y=&x;
			y.a_method();
			y.render();
			0
		}
		)====",
		nullptr,false
	},
	
	{	"nested ,guarded patterns",__FILE__,__LINE__,R"====(
		fn"C" printf(s:str,...)->int;
		fn main(argc:int, argv:**char)->int{
			for y:=0; y<8; y+=1{
				for x:=0; x<8; x+=1{
					match (x,y){
						(2|5,_)=>printf("X"),
						(_,2|5)=>printf("Y"),
						_ if x==y || (7-x)==y =>printf("o"),
						_=>printf(".")
					};
				}
				printf("\n");
			};
			0
		},
		)====",
		nullptr
	},

	{	"match val + ranges",__FILE__,__LINE__,R"====(
		fn"C" printf(s:str,...)->int;
		fn main(argc:int, argv:**char)->int{
			for x:=0; x<100; x+=10{
				match x{
					20=>printf("one\n",x),
					30 ..40=>printf("%d inrange 2-4\n",x),
					50|70=>printf("%d inrange odd\n",x),
					_=>printf("%d out of range\n",x)
				};
			};
			0
		},
		)====",
		nullptr
	},

	{	"basic enum+match",__FILE__,__LINE__,R"====(
		
		fn"C" printf(s:str,...)->int;
		enum Foo{
			Bar{x:int,y:int},
			Baz{x:float,y:float,z:int},
			Qux,Boo
		};
		fn main(argc:int,argv:**char)->int{
			let sx=new Bar{15,25};
			let sy=new Baz{10.0,20.0,55};
			let z1=match sy {
				a@*Bar=>{printf("match with bar\n");a.x as float},
				*Baz(vx,vy,vz) =>{printf("match with baz z=%d\n",vz);vx+vy},
				//"		Qux|Boo=>0, \n"
				_=>0.0
			};
			let z2=match sx {
				a@*Bar=>{printf("match with bar x=%d y=%d\n",a.x,a.y);a.x},
				_=>0
			};
			0
		}
		)===="
		,nullptr
	},
	

	{	"elaborate match example",__FILE__,__LINE__,R"====(
		
		struct FILE;
		extern"C"fn printf(s:str,...)->int;
		extern"C"fn fopen(d:*char,z:*char)->*FILE;
		extern"C"fn fclose(d:*FILE)->int;
		extern"C"fn fread(d:*void,z:size_t,n:size_t,f:*FILE)->size_t;
		extern"C"fn fwrite(d:*void,z:size_t,n:size_t,f:*FILE)->size_t;
		extern"C"fn sqrt(f:float)->float;
		
		struct Vec3<T>{vx:T,vy:T,vz:T};
		fn + <T>(a:&Vec3<T>,b:&Vec3<T>)=Vec3::<T>{vx:a.vx+b.vx,vy:a.vy+b.vy,vz:a.vz+b.vz};
		fn - <T>(a:&Vec3<T>,b:&Vec3<T>)=Vec3::<T>{vx:a.vx-b.vx,vy:a.vy-b.vy,vz:a.vz-b.vz};
		
		fn shape_vol(s:*Shape)->float= match s{
			*Sphere(my_centre, my_radius)=>{4.0/3.0*3.142* my_radius*my_radius*my_radius},
			*Cuboid(v_min, v_max)=>{ let d=v_max-v_min; d.vx*d.vy*d.vz},
			_ =>0.0
		};
		enum Shape {
			Sphere(Vec3<float>,float),
			Cuboid{c_min:Vec3<float>,c_max:Vec3<float>}
		};
		
		
		fn main(argc:int, argv:**char)->int{
			let v0 = Vec3::<float>{1.0,2.0,3.0};
			let v1 = Vec3::<float>{1.0,2.0,3.0};
			let v2=v0+v1;
			
			let s1=new Sphere{Vec3::<float>{1.0,1.0,1.0},1.0};
			let s2=new Cuboid{Vec3::<float>{1.0,1.0,1.0},Vec3::<float>{2.0,2.0,2.0}};
			let sv1=shape_vol(s1 as *Shape);
			let sv2=shape_vol(s2 as *Shape);
			
			0
		}
		)===="
		,nullptr
	},
	

	
	{	"pattern fn args",__FILE__,__LINE__,R"====(
		fn"C" printf(s:str,...)->int;
		fn foo((x,y):(int,int)){
			printf("args %d %d\n",x,y);
			0;
		}
		fn main(argc:int, argv:**char)->int{
			foo((11,22));
			0
		},
		)====",
		nullptr
	},


	{	"return anon struct infered type",__FILE__,__LINE__,R"====(
		fn foobar()->struct {x,y}{
			_{88,99}
		}
		
		fn main(argc:int,argv:**char)->int{
			let q=foobar();
			let w=q.x;
			0	}
		)===="
		,nullptr
	},

	{	"HKT (template template parameters)",__FILE__,__LINE__, R"====(
		
		fn map[C,T,Y](src:C[T],f:|T|->Y)->C[Y]{
			let result;
			result
		}
		struct Vec[T]{data:*T,num:int}
		fn main(argc:int, argv:**char)->int{
			let vec:Vec[int];
			let vec2=map(vec,|x|{0.0});
			0
		}											,
		)===="
		,nullptr
	},

	{	"let with pattern",__FILE__,__LINE__,R"====(
		
		fn main(argc:int, argv:**char)->int{
			let x=(11,12,13);
			let (a,b,c)=x;
			0
		},
		)====",
		nullptr
	},
	

	{	"pointer to bool ",__FILE__,__LINE__, R"====(
		
		fn"C" printf(s:str,...)->int;
		fn main(argc:int,argv:**char)->int{
			let bool_from_ptr1:bool	= argv;
			printf("bool val %d\n",bool_from_ptr1);
			0
		}
		)===="
	},

	{	"let",__FILE__,__LINE__,R"====(
		
		fn"C" printf(s:str,...)->int;
		fn main(argc:int, argv:**char)->int{
			let x=2;
			let y=3;
			let z=x+y;
			printf("x,y,z=%d,%d,%d,%d\n",x,y,z);
			0
		},
		)====",
		nullptr
	},
	{	"closures",__FILE__,__LINE__,R"(
		
		fn"C" printf(s:str,...)->int;
		fn take_closure(pfunc:|int|->void){ pfunc(5);}
		fn main(argc:int, argv:**char)->int{
			let y=111; z:=333;w:=0; y+=10;w+=7;
			take_closure()do x{let yy=y;printf("closure x=%d captured y=%d z=%d yy=%d\n",x,y,z,yy);}
			printf("y=%d z=%d w=%d\n",y+=900,z,w);
			0
		}
		)",
		// Result
		"closure x=5 captured y=121 z=333 yy=121\n"
		"y=1021 z=333 w=7\n"
	},

	{	"basic operator overload",__FILE__,__LINE__,R"====(
		
		fn"C" printf(s:str,...)->int;
		struct Vec3{ x:float,y:float,z:float};
		fn +(a:&Vec3,b:&Vec3)=Vec3{a.x+b.x, a.y+b.y, a.z+b.z};
		fn main(argc:int,argv:**char)->int{
			let v0=Vec3{1.0,2.0,3.0};
			let v1=Vec3{2.0,2.0,4.0};
			let v2:Vec3;
			v2=v0+v1;
			0	}
		)===="
		,nullptr
	},
 
	{	"struct default constructor",__FILE__,__LINE__,R"====(
		
		struct Extents(min:float,max:float){
			centre:float=sum/2,
			size:float	=diff/2
		} where {
			let sum=min+max;
			let size=max-min;
		}
		fn main(argc:int,argv:**char)->int{
			0	}
		)===="
		,nullptr
	},

	{	"templated struct initializer+overload",__FILE__,__LINE__,R"====(
		
		struct Vec3<T>{ x:T,y:T,z:T};
		fn + <T>(a:&Vec3<T>,b:&Vec3<T>)=
			Vec3::<T>{x=a.x+b.x, y=a.y+b.y, z=a.z+b.z};
		fn main(argc:int,argv:**char)->int{
			let v1=Vec3::<float>{0.0,1.0,2.0};
			let v2=Vec3::<float>{2.0,1.0,0.0};
			let v3=v1+v2;
			0
		}
		)====",nullptr
	},
	
	{	"type sugar",__FILE__,__LINE__,R"====(
		struct Foo{x:int};
		fn main(argc:int,argv:**char)->int{
			// map these common types with typedefs.
			//
			let a:[int];			// __slice<int>
			let b:[int*4];			// __array<int,4>
			let c:[int:string];		// __dictionary<int,string>
			let d:~str;				// __string
			let e:~[int];			// __vector<int>
			let f:~Foo;				// __unique_ptr<Foo>
			let g:~[~Foo];			// __vector<__unique_ptr<Foo>>
			//let f:?Foo;			// __option<Foo>
			//let f:?~Foo;			// __option<__unique_ptr<Foo>>
			0	}
		)===="
		,nullptr,true
	},

	{	"WIP, overloads with mixed types",__FILE__,__LINE__,R"====(

		struct Vec3{ vx:float,vy:float,vz:float};
		extern"C"fn sqrt(f:float)->float;
		fn *(a:&Vec3,f:float)=Vec3{vx=a.vx*f,vy=a.vy*f,vz=a.vz*f};
		fn |(a:&Vec3,b:&Vec3)=a.vx*b.vx+a.vy*b.vy+a.vz*b.vz;
		fn inv_length(a:&Vec3)=1.0/sqrt(a|a);
		fn mul_vec(a:&Vec3)=a*inv_length(a);
		fn main(argc:int,argv:**char)->int{
			0
		}
		)====",nullptr
	},
	
/*
 // TODO fix this case. since working on operator overload cases, this ceased to work.
 
	{
		"references vs values",__FILE__,__LINE__,
		"fn\"C\" printf(s:str,...)->int;		\n"
		"fn main(argc:int,argv:**char)->int{\n"
		"	let apple=15;	\n"		//integer value
		"	let ref_apple:&int; \n"	// c++ reference
		"	ref_apple=apple;	\n"	// ref_apple should be the adress of apple
		"	let banana:int;	\n"		// banana, just a value
		"	banana=ref_apple;	\n"	// this should load the *value* from apple->banana
		"	0	}	\n"
		,nullptr
	},
*/

	{	"type parameter inference UFCS autoref",__FILE__,__LINE__,R"(
		
		struct Union<A,B>{a:A,b:B, tag:int};
		fn setv[A,B](u:&Union[A,B], v:A)->void{
			u.a=v; u.tag=0;
		}
		fn setv[A,B](u:&Union[A,B], v:B)->void{
			u.b=v; u.tag=1;
		}
		fn main(argc:int, argv:**char)->int{
			let u:Union[int,float];
			u.setv(10);
			printf("u.tag=%d a=%d\n",u.tag,u.a);
			setv(u,10.0)	;
			printf("u.tag=%d\n",u.tag);
			0}
		fn"C" printf(s:str,...)->int;
		)",
		// expected result
		"u.tag=0 a=10\n"
		"u.tag=1\n"
	},
	{	"if - else if -",__FILE__,__LINE__,R"(
		
		fn main(argc:int,argv:**char)->int{
			x:=if argc>1 {10}else if argc>2{20} else {30};
			0	}
		)"
		,nullptr
	},

	{	"pass anon struct to adhoc template fn",__FILE__,__LINE__,R"(
		
		fn"C" printf(s:str,...)->int;
		fn main(argc:int,argv:**char)->int{
			let q:struct{x:int};
			q.x=10;
			foobar(&q);
			0
		}
		fn foobar(p){
			printf("p.x=%d",p.x);
		}
		)"
		,nullptr
	},
	{	"anon struct infered types later..",__FILE__,__LINE__,R"====(
		
		fn main(argc:int,argv:**char)->int{
			let q=foobar();
			q.x=1.0;
			q.y=2.0;
			0
		}
		fn foobar()->struct {x,y}{
			_{}
		}
		)====",nullptr
	},
	{	"anon struct infer type",__FILE__,__LINE__,R"====(
		
		fn main(argc:int,argv:**char)->int{
			let q:struct{x,y};
			q.x=1.0;
			q.y=1.0;
			0	}
		)====",nullptr
	},
	{	"multiple return",__FILE__,__LINE__,R"====(

		fn main(argc:int,argv:**char)->int{
			let q=foobar();
			0	}
		fn foobar()->(int,float,int){
			(1,2.0,3)
		}
		)====",nullptr
	},
	{	"tuples",__FILE__,__LINE__,R"====(

		fn main(argc:int,argv:**char)->int{
			let x=(1,0.0,3);
			let q=x.1;
			0
		}
		)===="
		,nullptr
	},
	{	"for  else, nested break",__FILE__,__LINE__,R"====(

		fn"C" printf(s:str,...)->int;
		fn main(argc:int, argv:**char)->int{
			v:=for i:=0;i<10;i+=1 {
				for j:=0; j<10; j+=1 {
					if j==5 {break break 44;}
				}
			}
			else{
				printf("loop complete i=%d\n",i);55
			}
			printf("loop ret=%d\n",v);
			0
		}
		)===="
		,"loop ret=44\n"
	},

	{	"internal vtable",__FILE__,__LINE__,R"====(
		
		fn"C" printf(s:str,...)->int;
		struct Foo {
			x:int,y:int,
			virtual v_foo(){printf("Foo.foo x=%d %p\n",x,*(this as**void));},
			virtual bar(){printf("Foo.bar\n");},
			virtual baz(){printf("Foo.baz\n");},
		}
		struct Bar : Foo{
			x:int,y:int,
			fn v_foo(){printf("Bar.foo x=%d\n",x);},
			fn bar(){printf("Bar.bar\n");},
			fn baz(){printf("Bar.baz\n");},
		}
		fn main(argc:int, argv:**char)->int{
			x1:= new Foo{x=10,y=0};
			x2:= new Bar{x=20,y=0};
			take_interface(x2 as*Foo);
			x1.v_foo();
			take_interface(x1);
			0
		}
		fn take_interface(pf:*Foo){
		   pf.v_foo()
		}
		)===="
		,nullptr
	},

	{	"member function+ufcs",__FILE__,__LINE__,R"====(
		fn"C" printf(s:str,...)->int;
		struct Foo{
			q:int,
			fn my_method()->float{
				printf("Foo.q=%d\n",q);
				2.0
			}
		};
		fn main()->int{
			x:=Foo{5};
			px:=&x;
			printf("member function test..\n",x.q);
			px.func1();
			px.func1(5);
			px.my_method();
			0
		};
		fn func1(f:*Foo){printf("func1 says q=%d\n",f.q);};
		fn func1(f:*Foo,x:int){printf("func1 says q=%d x=%d\n",f.q,x);};
		)===="
		,nullptr
	},

	{	"struct new",__FILE__,__LINE__,R"====(

		struct FooStruct{x:int,y:int};
		fn main(argc:int, argv:**char)->int{
			x:=new FooStruct{1,2};
			0
		}
		)===="
		,nullptr
	},
	{	"bool values ",__FILE__,__LINE__,R"====(

		fn main(argc:int,argv:**char)->int{
			let bool_val:bool=argc>4;
			0
		}
		)===="
	},

	{	"bool coersions ",__FILE__,__LINE__,R"====(

		fn"C" printf(s:str,...)->int;
		fn main(argc:int,argv:**char)->int{
			let zeroi32:int=0;
			let vali32:int=100;
			let vali64:int=vali32;
			let valbool1:bool=vali32;
			let valbool2:bool=zeroi32;
			let valbool2:bool=argv;
			let my_ptr:*int	= nullptr;
			let bool_from_ptr1:bool	= nullptr;
			let bool_from_ptr2:bool	= &vali32;
			printf("%d %d %d %d\n",zeroi32,vali32,vali64,valbool1,valbool2,my_ptr,bool_from_ptr1,bool_from_ptr2);
			0
		}
		)===="
	},
	{	"struct",__FILE__,__LINE__,R"====(

		struct FooStruct{x:int,y:int};
			fn main(argc:int, argv:**char)->int{
			x:=FooStruct{1,2};
			0
		},
		)====",nullptr
	},
	{	"if expression",__FILE__,__LINE__,R"====(

		extern"C" fn printf(s:str,...)->int;
		fn main(argc:int, argv:**char)->int{
			x:=if argc<3{printf("if");4} else{printf("else");3};
			0
		)====",
		nullptr
	},
	{	"voidptr auto coercion ",__FILE__,__LINE__,R"====(
		
		struct FILE;
		fn voidpf(d:*void)->int{0};
		fn voidpfr(p:**char)->*void{p as*void};
		fn main(argc:int,argv:**char)->int{
			voidpf(argv);
			x:=voidpfr(argv);
		  0
		}
		)===="
	},
	{	"adhoc template",__FILE__,__LINE__,R"====(

		fn lerp(a,b,f){(b-a)*f+a};
		fn main(argc:int,argv:**char)->int{
			x:=lerp(0.0,10.0,0.5);
		  0
		}
		)===="
	},
	{	"let array",__FILE__,__LINE__, R"====(

		fn main(argc:int, argv:**char)->int{
			let xs:array[int,10];
			let ptr1={&xs[1]};
			ptr1[1]=5;
			0
		},
		)===="
		,nullptr
	},
	{	"member functions+UFCS",__FILE__,__LINE__, R"====(
		//SOURCE
		fn"C" printf(s:str,...)->int;
		struct Foo{
			q:int,
			fn method()->float{
				printf("Foo.q=%d\n",q);2.0
			}
		}
		struct Bar{
			w:int,
			fn method()->float{
				printf("Bar.w=%d\n",w);2.0
			}
		}
		fn func1(f:*Foo){printf("func1 says q=%d\n",f.q);}
		fn main()->int{
			x:=Foo{5};	px:=&x;	y:=Bar{17}; py:=&y;
			printf("member function test..\n",x.q);
			px.func1();
			px.method();
			py.method();
			0
		}

		)===="
		,// EXPECTED RESULT
		"member function test..\n"
		"func1 says q=5\n"
		"Foo.q=5\n"
		"Bar.w=17\n"
	},
	{
		"allocation",__FILE__,__LINE__,R"====(
		fn"C" printf(s:str,...)->int;
		struct Foo{x:int,y:int};
		fn main(argc:int, argv:**char)->int{
			pfoo:= new Foo{4,5};
			pfoos:= new Foo[10];
			pfoos[1].x=10;
			printf("new foo %p x,y=%d,%d array alloc=%p\n",pfoo,pfoo.x,pfoo.y,pfoos);
			delete pfoo;
			0
		},
		)===="
		,nullptr
	},
	{
		"for  else loop",__FILE__,__LINE__,R"====(
		fn"C" printf(s:str,...)->int;
		fn main(argc:int, argv:**char)->int{
			i:=5; b:=argc<9;
			v:=for i:=0,j:=0;
				i<10;
				i+=1,j+=7 {
				printf("for loop i=%d j=%d\n",i,j);
				if i==5 {break 44;}
			}
			else{
				printf("loop complete i=%d\n",i);55
			}
			printf("loop ret=%d; outer scope i=%d\n",v,i);
			0
		},
		)===="
		,nullptr
	},
	{
		"type parameter inference",__FILE__,__LINE__,R"====(
		struct Union<A,B>{a:A,b:B, tag:int};
		fn setv[A,B](u:*Union[A,B], v:A)->void{
			u.a=v; u.tag=0;
		}
		fn setv[A,B](u:*Union[A,B], v:B)->void{
			u.b=v; u.tag=1;
		}
		fn main(argc:int, argv:**char)->int{
			u=:Union[int,float];
			setv(&u,10)
			printf("u.tag=%d\n",u.tag);
			setv(&u,10.0)	;
			printf("u.tag=%d\n",u.tag);
		0}
		fn"C" printf(s:str,...)->int;
		)===="
		,// expected result
		"u.tag=0\n"
		"u.tag=1\n"
	},
	{	"multi feature test 2",__FILE__,__LINE__,R"====(
		
		fn map<V,A,B>(src:*V<A>, f:|*A|->B)-> V<B>{
			let result=init();
			for index:=0; index<src.size(); index+=1 {
				push_back(&result, f(get(src,index)));
			}
			result
		}
		fn"C" printf(s:str,...)->int;
		fn debugme[X,Y,R](u:*Union[X,Y], fx:(*X)->R,fy:(*Y)->R)->R{
			if u.tag==0 { fx(&u.x)}
			else { fy(&u.y)}
		}
		fn main(argc:int,argv:**char)->int{
		fv:=Foo{vx=13,vy=14,vz=15};
			u=:Union[int,float];
			setv(&u,0.0);
			setv(&u,0);
		z:=debugme(&u,
				   |x:*int|	{printf("union was set to int\n");10},
				   |x:*float|	{printf("union was set to float\n");12}
				   );
			printf("map union returns %d\n", z);
			xs=:array[int,512];
		q:=xs[1]; p1:=&xs[1];
			xs[2]=000;
			xs[2]+=400;
			*p1=30;
		z:=5;
		y:=xs[1]+z+xs[2];
		x:=0;
			something_foo(&fv,&fv);
			for i:=0,j:=0; i<10; i+=1,j+=10 {
				x+=i;
				printf("i,j=%d,%d,x=%d\n",i,j,x);
			}else{
				printf("loop exit fine\n");
			}
			something_foo(&fv);
			something(&fv);
			take_closure(|x|{printf("closure says %d %d\n",x,y);})
			
		x:=if argc<2{printf("<2");1}else{printf(">2");2};
			printf("yada yada yada\n");
			printf("\nHello World %d\n", y );
			0
		}
		fn lerp(a,b,f)->float{(b-a)*f+a};
		fn foo(a:*char)->void;
		struct Foo {
		vx:int, vy:int, vz:int
		}
		fn something_foo(f:*Foo){
			printf("f.x= %d\n", f.vx);
		}
		fn something_foo(f:*Foo,x:*Foo){
			printf("something_foo with 2 args overloaded\n");
			printf("f.x= %d,.y= %d,.z= %d\n", f.vx,f.vy,f.vz);
		}
		fn something(f:*Foo){
			printf("f.x= %d,.y= %d,.z= %d\n", f.vx, f.vy, f.vz);
		}
		fn something(f:float){
		}
		fn something(f:float,x){
		}
		fn take_closure(funcp:(int)->void){
			funcp(10);
		}
		struct Union[X,Y]{
		tag:int,
		x:X,y:Y,
		};
		fn setv[X,Y](u:*Union[X,Y],x:Y)->void{
			printf("setv Y\n");
		}
		fn setv[X,Y](u:*Union[X,Y],x:X)->void{
			printf("setv X\n");
		}
		
		)====",
		nullptr
	},
	

	{
		nullptr,nullptr,0,nullptr,nullptr
	}
};
CompilerTest g_todo[]={
	{	"trait objects TODO",__FILE__,__LINE__,R"====(
		// TOD.O. part of the plan but low priority.
		// C++ single-inheritance classes + Enums cover enough.
		// do want to be able to instantiate a C++ style class as a trait object for rust-interop
		//
		// trait declarations alone for bounded polymorphism would be helpful.
		fn"C" printf(s:str,...)->int;
		struct Foo{
		x:int
		};
		trait Render{
			fn render();// todo parse self
		}
		impl Render for Foo{
			fn render(){	// todo parse self
				printf("Foo render\n");
			}
		}
		fn something(f:&Foo){
			printf("hello world 2\n");
		}
		fn main(argc:int,argv:**char)->int{
			let x:Foo;
			//should desugar:
			// let pr={let x:*__trait_object<Render>;
			// x.__vtable_ptr=Foo__Render__vtable;
			// x.__data_ptr=(new Foo{..}) a i8*;
			// x}
			// need to adjust 'vcall' mechanism , if '__data_ptr' is found
			// trait-object's "&Self" could actually be an i8*, which it must recast itself?
			let pr=new Foo as *Render;
			x.something();
			x.render();
			
			0
		}
		)===="
		,nullptr,true
	},
};

void run_tests(){
	int index=0;
	for (auto t=g_Tests; t->name; t++,index++){
		char tmp[256]; sprintf(tmp,"test_%d.ll",index);
		printf("\nRunning Test[%d]: %s\n\n",index,t->name);
		char* output=0;
		auto ret=
		compile_and_run(t->source,t->name, tmp,B_AST|B_DEFS|B_TYPES|B_RUN, t->expected_output?&output:nullptr);
		if (!t->should_fail && ret!=0) {
			printf("\n test %s failed\n", t->name);
			exit(-1);
		} else if (t->should_fail && ret==0) {
			printf("\n test %s supposed to yield compiler errors\n", t->name);
			exit(-1);
		}
		if (output){
			if (strcmp(output, t->expected_output)){
				printf("\n%s:%d: Test[%d]\"%s\" gave incorrect output, expected:-\n",t->file,t->line,index,t->name);
				dbprintf("[length=%d]\n%s\n",strlen(t->expected_output),t->expected_output);
				dbprintf("[length=%d]\n%s\n",strlen(output),output);
				if (strlen(output))
					exit(-1);
				else {
					dbprintf("maybe bug with output capture so continuing..\n");
				}
			}
			free(output);
		}
	}
	compile_source_file("example.rs", B_DEFS|B_TYPES|B_RUN);

}
					

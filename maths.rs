#!hack -r

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


fn main(argc:int, argv:**char)->int{
	let v0 = Vec3::<float>{1.0,2.0,3.0};
	let v1 = Vec3::<float>{1.0,2.0,3.0};
	let v2=v0+v1;


	0
}





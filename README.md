### LLVM langauge experiment,

A subset of C/C++,+more, resyntaxed like Rust.  Implemented in C++.

'$ make' to compile & run inbuilt test

'$ ./hack hello.rpp' to compile & run example program.

see 'hello.rs' example source; (using rs extention for highlighting)

'compiler.h' descibes the ast, 'compiler.cpp' is contains bulk of the resolving

dont have a name yet hence 'hack'..

#### Currently supports:-

 * C operators, functions, structs,if-else, 
 * 'everything is an expression' syntax
 * function overloading
 * Forward+Reverse Type Inference within functions, forward between functions
 * stack-based closures
 * C for loops + break/else expressions
 * templated functions & structs
 * new/delete ,and a ordered or named struct field initializer
 * limited vtables & single inheritance
 * emits LLVM sourcecode, compiled by clang, links with C/C++ ecosystem.

Very early days, the compiler is a few weeks old.

#### Long Term Goals:-

 * significant C++ subset resyntaxed
 * open-world polymorphism
 * add Rust/functional language inspired features
 * features for parallelism, GPGPU programming
 * a subset should make a passable embedded dynamic language

Basically trying to comine everything I like from C++ & Rust, plus what i've always missed.

This could all probably be done as a fork of a C++ compiler, or as a fork of Rust. However neither community shares these specific goals and it is hard to make complex changes to existing projects (retrofitting 2way inference to C++? or retrofitting overloading to Rust? both againt the underlying design of either ..)

Rust has many inspiring features but is a departure from C++ lacking features like function overloading that prevents representing existing C++ code; I beleive C++ can be 'fixed' and improved without straying so far.

I beleive C++'s main 'curse' is the way headers & classes interact, and the asymetry between functions and methods has always been frustrating. Rust is too restrictive. Somewhere between the two is my perfect language.

This is probably all way beyond a 1man project but I'll see how far I can get..

#### Goals In detail:-

 * Resyntax a significant subset of C++, in the spirit of SPECS; 
  * should be possible to non-destructively translate a subset back & forth.
  * allow use with established C++ libraries & sourcebases
  * context free grammar
  * graph like module import: any file in a project can be the root for its own tests
  * add alternate parser that can directly read subset of C++ headers ?
     (or adapt a rust community tool for C++ -> rust translation..)
  * self host by translating own source, be mindful of c++ subset used to write this

 * Additional Features inspired by Rust & other languages:
  * 2 way inference
  * expression oriented syntax.
  * ADTs (possibly implement as sugar for dynamic_cast<>?)
  * optional trait bounds on templates?
  * rust-like trait-objects - (vtable*,data*)
  * maybe aim to compile a subset of Rust programs,
   * this is more like C++ than rust despite appearance;
   * Can we reconcile Rust ideas with C++ semantics?
    * must think about details of fatpointers, 
    * nullpointer enum-optimization
    * 'box' operator, not quite like 'new'
    * differences between modules & namesspaces 
    * anything else ?
  * maybe aim to transpile Rust aswell? (given our AST will have C++ and Rust-like elements in one place)
  * scala like sugar for default constructor
  * possibly currying, only freefunctions? or only for 'receiver or args'?

 * Additional features..  & inspired by other languages:
  * want "_" placeholder in ident position to query compiler -see haskell 'holes'..
  * 100% Open World design - free functions+UFCS/Extention methods
   * minimal syntax changes to rearrange code,
   * adhoc gather of free functions into interfaces (like go, but generalized), 
   * or sort by function into switch dispatch
  * possibly multimethods - automate rolling double-dispatch (hence 'any param' as vtable?)
  * some sort of reflection, opt in, and compile time.
  * where vtables are used, more flexibility: 
   * vtable should be upgraded to a 'class-object'
   * 'static-virtual data', 
   * eg hot-swapping,
   * auto roll classfactory& 'message-map'.
   * possibly reflection info in the vtable - walk the fields.
   * possibly generalize vcall mechanism to implement c++ classes, rust trait objects and other systems under one 'roof'

 * enhance template engine to replace macro usecases.
  * eg 'ident' parameters, more?
  * functions in the type system to stringify, concat etc.
  * perhaps 'TMP' can be made more elegent and less accidental

 * a subset should make a passable dynamic language:-
  * a single language to handle the C++/embedded Lua usecase.
  * hence the desire for whole-program inference
    * recover Rusts old ~,@ sigils for smarpointers
  * maybe want a REPL.
  * statements at root level? (perhaps root level statements outside main are tests?)

 * features aimed at data-parallel,GPGPU/shader programming 
  * eg compile the same code on GPU or CPU for debugging, streamline boilerplate (uniforms etc).
  * want intrinsic float4 type with .xyz member swizzle, and half type.
  * GPU&shader programming is important enough to hardcode language features.
  * recover rusts' lost "do" notation for pleasant internal iterators, 

 * other ideas..
  * ways of carrying more information down an object graph..
    * maybe hidden parameters on '.' overloads inlined away usually
      * eg foo.bar could return  (&Bar,this) which autocoerces to &Bar 
    * maybe nested 'this' for nested classes (eg scene->lights,scene->meshes...)
  * 'intersection types' for cutting structures up without having to butcher source
  * immutable data hint ? (eg for coalesced-allocations,local ptrs)
  * 'vtable-from-address' for pooled objects?
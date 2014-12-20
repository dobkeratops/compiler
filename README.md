### LLVM langauge experiment,

A subset of C/C++,+more, resyntaxed like Rust.  Implemented in C++.

'$ make' to compile & run inbuilt test

'$ ./hack -r example.rs' to compile & run example program.

see 'example.rs' example source; 

Dont have a name yet hence 'hack'..

#### Currently supports:-

 * most C operators, functions, structs,if-else, 
  * (todo ++/--,use &p[i] instead of ptr arithmetic)
 * 'everything is an expression' syntax
 * C for loops + break../else{..} expressions
 * function overloading+UFCS
 * limited operator overloading (no conversions yet)
 * Forward+Reverse Type Inference within functions, 
  * forward between functions -by adhoc templates
 * stack-based closures 
  * (workaround: if need escape,use a class)
 * templated functions & structs
 * new/delete ,and a ordered or named struct field initializer
 * limited C++-style internal vtables & single inheritance
 * emits LLVM sourcecode, compiled by clang, links with C/C++ ecosystem.

#### WIP
 * rust-style enum/match. Compiles without crashing,need to test cases..
 * WIP ..HKT (template-template parameters)
  * .. not extensively tested

example source..
https://github.com/dobkeratops/compiler/blob/master/example.rs

Quite early days.


#### Long Term Goals / project pillars :-

 * a systems language, 'for games'. no GC,zero overhead principle.
 * significant C++ subset resyntaxed, intended for transpiling both ways 
 * open-world polymorphism
 * add Rust/functional language inspired features
 * features for parallelism, GPGPU programming
 * a subset should make a passable embedded dynamic language
  * (think of 1 language to handle the C++-&-embedded-Lua usecase. recover @ptr..)

Basically trying to combine everything I like from C++ & Rust, dropping what I dont like, plus what i've always missed.

This could all be done as a fork of a C++ compiler, or as a fork of Rust. However neither community shares these specific goals and it is hard to make complex changes to existing projects - retrofitting 2way inference/openclasses to C++? or retrofitting adhoc-overloading to Rust? 

Rust has many inspiring features but is a departure from C++ lacking features like function overloading that prevents representing existing C++ code;

I beleive C++ can be 'fixed' and improved without straying so far,without sacrificing existing knowledge & code.

Also I value performance+productivity over compile-time safety. You need to write tests for other reasons, so IMO productivity for *tests* is what yields working code... there is still so much you still can't express in a typesystem or verify at compile time.

I beleive C++'s main 'curse' is the way "headers & classes interact", and the asymetry between functions and methods has always been frustrating.(I have worked mostly on platforms where vtables were unacceptable). 

It almost seems like a deliberate joke - how can a language run GameOfLife with compile time metaprogramming, parse ambiguous syntax with GLR, but NOT find definitions out of order?

Other C++ flaws are acceptable due to its evolutionary path and need to represent low level code.

Rust on the other hand is too restrictive; in particular I want to be able to think primarily in Functions & Structs - not classes,traits, or hierachical modules. Rust Traits are good but I'd prefer them optional & duck-typed. Rusts philosophy edges toward verbosity,'costs must be explicit' -IMO costs should be deterministic sure, but typing more for slow code doesn't make it faster- it wastes time on setup,tools,tests.. What is important is expressivity, ability to write optimal code more elegantly.

So somewhere between the two is my perfect language.

This is probably all way beyond a 1man project but I'll see how far I can get. Perhaps I can just experiment and converge on whatever mainstream option is closest.

#### project structure & info..

 * 'main.cpp' contains the driver,'parser.cpp' builds the AST,
 * 'node.h'=AST; implementations - eg'exprfndef.cpp' &'exprstructdef.cpp' 'exprOp.cpp 
 * 'semantics.cpp' & '::resolve()' methods do inference/overload resolving..
 * 'codegen.cpp' encapsulates the output, called from node '::compile()' methods 
  * this is a hacky text output at the minute but does the job, 
  * maybe replaced with mutliple back ends... LLVM, C++ transpile..

 * sorry for the range of coding styles here: 
  * I'm intending to transpile to self-host, 
  * so have been reluctant to dive into full "modern C++"
  * still might want to eliminate dependance on C++ stdlib with simplified Vec<T>



#### Goals In detail:- (a huge TODO list..)

 * Resyntax a significant subset of C++, in the spirit of SPECS; 
  * should be possible to non-destructively translate a subset back & forth.
    * to eliminate the risk of transitition
    * allow use with,& in, established C++ libraries & sourcebases
  * self host by translating own source, be mindful of c++ subset used to write this
  * 'C++ with context free grammar'

 * graph like/relative module import: 
  * any file in a project should work as the root for its own tests
  * No committing to heirachical structure
    * dont need to commit to 'crate roots', change library granularity
    * relative namespace search (to avoid any absolute root), just warn about ambiguity
    * overloading is,IMO, like non hierachical 'tagging'.
      *find more ways to leverage/improve that, eg keyword args drasticly reduce ambiguity

 * add alternate parser that can directly read subset of C++ headers ?
     (or adapt a rust community tool for C++ -> rust translation..)

 * compare with other languages, converge with whichever mainstream option is closer, take inspiration..
  * C++17,21.. of course.
  * Rust - first non-C++ language I've actually wanted to use.
  * Jonathan Blows' ".jai" "language for games"
    * most similar stated goal, but maybe different preferences
    * was inspiration to start
  * 'SugarCpp' - an interesting transpiler, looks very practical
  * D - never grabbed me for some reason, but has many features of interest eg UFCS
    * (tends to focus on gc, and doesn't have expression syntax?)
  * disqualified by CG, but still interesting:-
    * Julia - focus on overloading/multimethods & interactivity
    * Go - adhoc duck-type interface gather, simple but practical

 * Features inspired by Rust & other languages:
  * 2 way inference
  * expression oriented syntax
    * completed by adding  for-else loop,"break expressions".
  * ADTs (possibly implement as sugar for dynamic_cast<>?)
  * optional trait bounds on templates? (bounds replace SFINAE?)
  * rust-like trait-objects - (vtable*,data*) - replaces multiple-inheritance
  * maybe aim to compile a subset of Rust programs,
   * this is more like C++ than Rust despite appearance;
   * Can we reconcile Rust ideas with C++ semantics?
    * must think about details of fatpointers, 
    * nullpointer enum-optimization
    * borrowed pointers vs references? 
     * Possibly adopt '^'/'&' for C++ref vs rust borrow? 
     * will rust get 'auto-deref' in which case it'll behave a little more like C++ref?
    * Rust 'box' operator is very different to 'new'
    * differences between modules & namesspaces 
    * anything else ?
  * maybe aim to transpile to Rust aswell?
   * AST will have C++ and Rust-like elements in one place
  * scala like sugar for default constructor
  * possibly currying, only freefunctions? or only for 'receiver or args'?
  * Would like to copy Rust's macro engine identically but its not a priority right now.

 * Additional features..  & inspired by other languages:
  * want "_" placeholder in ident position to query compiler -see haskell 'holes'..
  * 100% Open World design 
   * free functions+UFCS/Extention methods
   * would prefer if methods never existed but need them for interfacing with C++
   * minimal syntax changes to rearrange code,
   * 'Expression Problem' - dont commit syntax upfront to sort by function or type
    * adhoc gather of free functions into interfaces (like go, but generalized),
     * 'duck type traits' 
    * or sort by function into switch dispatch via Join types X|Y|Z types?
  * possibly multimethods - automate rolling double-dispatch (hence 'any param' as vtable?)
  * some sort of reflection, opt in, and compile time.
  * where vtables are used, more flexibility: 
   * vtable should be upgraded to a 'class-object'
   * 'static-virtual data', 
   * eg hot-swapping (use ptr to class-object for state machines)
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
  * syntactic sugar to suit it , eg [...] could make a dynamic array whilst (T,T,T) coerces to array<T,N>
   * I think this is useful for writing tests.

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
    * maybe just autoderef on tuple member acessors so tuples do that job..
  * immutable data hint ? (eg for coalesced-allocations,local ptrs)
  * 'vtable-from-address' for pooled objects?.. 
    * generalize 'get-vtable','get-data' to do classes,trait-obj & more in 1 system?

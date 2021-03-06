### LLVM langauge experiment,

A subset of C/C++,+more, resyntaxed like Rust.  Implemented in C++.

'$ make' to compile & run inbuilt test

'$ ./hack -r example.rs' to compile & run example program.

see 'example.rs' example source; 

Dont have a name yet hence 'hack'..

#### Currently supports:-

 * most C operators, functions, structs,if-else, 
 * 'everything is an expression' syntax
 * rust-like 'enum/Match' - tagged-unions & pattern-matching
 * C for loops + break../else{..} expressions
 * RAII (destructors)
 * function overloading+UFCS
 * limited operator overloading (no conversions yet)
 * Forward+Reverse Type Inference within functions, 
  * forward between functions -by adhoc template sugar
 * stack-based closures 
 * templated functions & structs
 * new/delete ,and a ordered or named struct field initializer
 * limited C++-style internal vtables & single inheritance
 * emits LLVM sourcecode, compiled by clang, links with C/C++ ecosystem.


#### WIP
 * rust-like iterator protocol
 * details of C++ RAII
   * RValue-refs work in simple cases,more tests needed
   * ctor/dtors compose now, but TODO Foo():Bar()..{} syntax
 * Parsing rust Trait/Impl
   * not planning on getting a full implementation done soon,
   * I prefer UFCS/overloading
   * but relatively easy to get it parsed
   * might inspire getting a larger subset of Rust handled..
   * details - 'Self' type - in progress..
 * rust-style enum/match- 
   * currently does a..b a..<b, a|b|c, (a,b,c), if guards, @,nesting; 
     * no slice patterns yet
   * have tried to add scalas idea of being able to reference vars in patterns (by unary @)
   * works with  ptrs, matching C++ refs appears bugged
   * just added padding of variants,like rust.
 * HKT (template-template parameters)
   * .. not extensively tested
 * Missing operators
  * (todo ++/--;and currently &p[i] instead of ptr arithmetic)
  * deref for smartpointers!
  * [] for collections

example source..
https://github.com/dobkeratops/compiler/blob/master/example.rs

#### Long Term Goals / project pillars, Prioritized :-

 * 1 a systems language, 'for games'. no GC,zero overhead principle.
 * 2 significant C/C++ subset resyntaxed, intended for nondestructive transpiling both ways 
 * 3 open-world polymorphism (starting with UFCS)
 * 4 Add features inspired by Rust, keep syntax close
 * 5 dedicated features for parallelism, shader+GPGPU programming ?
 * 6 Aim to actually compile a subset of Rust, 
   * unless proves impossible to reconcile with goals 1,2,3
 * 7 a subset should make a passable embedded dynamic language
   * think of 1 language to handle the C++-&-embedded-Lua usecase. recover @ptr..
   * but dont' compromise goals 1-6 over this.

### Short Term Priorities

 * solidify current features set (bugs, fix any oversights, cleanup)
 * complete feature set required for compiling own transpiled own source
   * undecided-wait for a C++ -> Rust transpiler to appear and adapt it? or start one..
     * dont want to bite off more than I can chew
     * Sugar Cpp is a similar project that could use transpiling from C++.
 * solidify Rust enum/match - rusts 'coolest' feature&most complimentary to C++.
   * extend eg anon enums? overload '.discriminant()'?
 * intrinsic macros.. dump!() assert!() .. practicalities (dont need full macro system) etc
 * a simplified module system
 * low priority: gradually expand C++ and Rust features set covered (trait objects, rust macros)
   * .. but get bits done when focussed on an area of overlap
   * maybe aim to get all of Rust parsed, at least eg impl, lifetimes, even if not compiling
   * Rusts Macro System would fit perfectly but is low priority
 * Keep track of 'jonathan blows JAI' language and 
   * maybe copy features, 
   * the syntax is different but in time we could switch
  

### Rationale

Basically trying to combine everything I like from C++ & Rust, dropping what I dont like, plus what i've always missed.

This could all be done as a fork of a C++ compiler, or as a fork of Rust. however maintaining a fork againt commnity momentum (and trying to make such changes with either complex sourcebase) would be hard.

.. Anything here could be considered 'feature requests' for C++ or Rust.

I beleive C++ can be 'fixed' and improved without straying so far,without sacrificing existing knowledge & code. A C++ compiler could be 'reskinned' to fix syntactic problems?

Rust has many inspiring features but is a departure from C++ lacking features like function overloading that prevents representing existing C++ code; But its close enough to beleive it would only take very small changes to satisfy me.

Also I value performance+productivity over compile-time safety. You need to write tests for other reasons, so IMO productivity for *tests* will still yield stability... there is still so much you still can't express in a typesystem or verify at compile time.

I beleive C++'s main 'curse' is the way "headers & classes interact", and the asymetry between functions and methods has always been frustrating.(I have worked mostly on platforms where vtables were unacceptable). The standard libraries are messy but easily replaced. UFCS would be a big step forward.

Other C++ flaws are acceptable due to its evolutionary path and need to represent low level code

Rust on the other hand is a little too restrictive; in particular I want to be able to think primarily in Functions & Structs - not classes,traits, or hierachical modules. Rust Traits are good but I'd prefer them optional & duck-typed. Rusts philosophy edges toward verbosity,'costs must be explicit' -IMO costs should be deterministic sure, but typing more for slow code doesn't make it faster- it wastes time on setup,tools,tests.. What is important is expressivity, ability to write optimal code more elegantly.
Rust has to *over-estimate* safety to be sure. Some performant patterns are still safe, without being compile-time provable. empirical tests are usually good enough.

So somewhere between the two is my perfect language.

This is probably all way beyond a 1man project but I'll see how far I can get. 
Perhaps I can just experiment and converge on whatever mainstream option is closest.

If another language has every feature I want in one place... great, I'll ditch this.

#### project structure & info..

 * 'main.cpp' contains the driver,'parser.cpp' builds the AST,
 * 'node.h'=abstract AST node; implementations - eg'exprfndef.cpp' &'exprstructdef.cpp' 'exprOp.cpp (operator);  
 * 'semantics.cpp' & '::resolve()' methods do inference/overload resolving..
 * 'codegen.cpp' encapsulates the output, called from node '::compile()' methods 
  * this is a hacky text output at the minute but does the job, 
  * maybe replaced with mutliple back ends... LLVM, C++ transpile..

 * sorry for the range of coding styles here: 
  * I'm intending to transpile to self-host, 
  * so have been reluctant to dive into full "modern C++"
  * still might want to eliminate dependance on C++ stdlib with simplified Vec<T>

### Open questions...

 * inbuilt slice/vector types like .jai?
   * reasoning behind that makes sense
   * but we have templates already
   * might just give syntax sugar for declaring them 
     * [] [..] [N] == __slice<T> __vector<T> __array<T,N>
     * [K=>V] == __dictionary<K,V>
     * .. then we can switch to inbuilt later..
     * still might just want to recover old Rust syntax ~[T], ~T, ~str [T*N] .. it was nice!

 * default value passing behaviour especially * vs &, C++ vs rust..
   * C++ - value, copy by default, but autoborrow for '& ptrs' (aka references)
   * Rust - move by default, must write 'borrow' explicitely.
 * whats' more useful, whats' more consistent?
   * Move is cheap, borrow is cheap, but which is more common..
     * find oneself writing & a lot in rust.
   * Can types from both slot in one system, eg flag rust stuff as 'Move', c++ stuff as Copy
 * this language wants immutable default, like rust.
 * immutable-borrow is just like a value, so reverting to C++ like behaviour modified for immutable borrow could be nice.
 * its nice that Rust makes copy explicit (.clone()), as its' expensive
   * do we want to use C++ copy-constructors, or go that route..
 * perhaps we should just have immutable-borrow as the default/nothing extra typed
   * and write explicit move or copy operators/functions
   * and have to write something explicit for a *mutable* pointer.
     * agree with google-style guide's preference to make output params obvious at callsite
     * rust also requires you to write &mut
 * So is simply C++ like behaviour, tweaked for immutable default enough
 * can we still shoe-horn compiling Rust source into this
   * Auto-coerce *ptr to &ptr, and & makes a * like in C++.
   * does that break any C++ code?

 * What are the deatils of jonathan blows ^ptr. how does that sit.
   * would most likely want to converge on that language..

 * Could we just generalize how the * & operators act on types and use some syntax on their declaration to set that up
   * so C++ transpiled and rust originated types get the right behaviour

#### Rust Compatability ???
 * everything I want would be possible as a superset of Rust.
 * It *would* be nice to build this up into 'a superset of a subset of Rust'
   * try to overlap with Rust community, Rust tools.
 * but (i)I'm unlikely to be able to sort out every detail single handedly
 * but(ii)its' not essential for the 'Project Pillars'.
 * would anyone out there want to collaborate on building this up into a full rust implementation?
 * full rust *macros* would be a nice addition that don't complicate the internals
 * its' still possible future Rust versions might include these features..

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
    * I strongly dislike this in rust, the fact you MUST bake the whole hrc position into a symbol name
      * moving things around is just as annoying as with C++ headers.
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
    * was inspiration to start this
    * could just converge on it and keep any extra features we have?
    * as it gains momentum, we could just switch to its' syntax?
  * 'SugarCpp' - an interesting transpiler, looks very practical
    * it just accepts C++1y as starting point and just adds more.
    * differs by not having the 'rust' influence, but very similar goals
  * D - never grabbed me for some reason, but has many features of interest eg UFCS
    * (tends to focus on gc? and doesn't have expression syntax?)
  * disqualified by GarbageCollection, but still interesting:-
    * Julia
      * focus on overloading/multimethods instead of 'special parameter'
      * interactivity
    * Go - 
      * adhoc duck-type interface gather- want this;
      * interesting for doing a lot with a simple feature set
    * Nim -
      * nice features - overloading, do notation..
      * the GC does at least have realtime control
      * looks very clean

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



#### other ideas

 * Generalizing enum/match:-
   * maybe make it call an 'isa(x:*X)->*Y' test/coerce (like dynamic_cast)
   * an 'enum' just rolls a 'isa' function which tests the discriminant...
   * .. but any custom union type could supply the 'isa' methods to fit match
   * C++ vtables could slot in with 'isa() being dynamic_cast
   * eg -1 invalid index, +ve vs -ve values, etc.

 * 'intersection types'(?) for cutting structures up without having to butcher source
   * want to write the same routine with data split or merged
   * maybe just autoderef on tuple member acessors so tuples do that job..
   * writing cachefriendly code means routinely splitting things up

 * ways of carrying more information down an object graph..
   * maybe hidden parameters on '.' overloads inlined away usually
     * eg foo.bar could return  (&Bar,this) which autocoerces to &Bar 
   * maybe nested 'this' for nested classes (eg scene->lights,scene->meshes...)
     * pass an inherited 'this' into methods?
       *alternate use of 'this/self'? .. just 1 level


 * immutable data hint ? 
   * eg for coalesced-allocations,local ptrs
 * 'vtable-from-address' for pooled objects?.. 
   * generalize 'get-vtable','get-data' to do classes,trait-obj & more in 1 system?
 * random idea.. Haskell lazy eval is intruiging
   * would the haskell way of working be useful for tools? lazy file io?
   * Would there be any merit in being able to instantiate pure functions for lazy eval. x=lazy_call(..... ).
   * detect fn's that are eligable.
   * could any of that work without a GC?

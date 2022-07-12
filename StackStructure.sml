(* val fun = fn a b => a+b; *)

signature STACK = 
sig
    datatype 'a stack = Empty | cons of 'a * ('a stack)
    
    exception EmptyStack
    exception IllegalIndexAccess
    exception Error of string
    val create :'a stack 
    val push : 'a * 'a stack -> 'a stack
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val empty: 'a stack -> bool
    val poptop : 'a stack -> ('a * 'a stack) option
    (* val nth : 'a stack*int -> 'a
    val fastnth : 'a stack*int -> 'a
    val depth : 'a stack -> int *)
    
    val nth : 'a stack * int -> 'a (*Returns nth element with zero indexing *)
    val drop : 'a stack * int -> 'a stack (*return swhat is left after dropping the first i elemetns*)
    val depth : 'a stack -> int (*size*)
    val app : ('a -> unit) -> 'a stack -> unit (*applies f to the elements of l from left to right *)
    val map : ('a -> 'b) -> 'a stack -> 'b stack(*Applies f to all the elements and returns the list of results *)
    val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack (*applies f to each element of l from left to right, returning a list of results, with SOME stripped, where f was defined. f is not defined for an element of l if f applied to the element returns NONE. The above expression is equivalent to:) *)
    val find : ('a -> bool) -> 'a stack -> 'a option (*applies f to each element x of the stack l, from left to right, until f x evaluates to true. It returns SOME(x) if such an x exists; otherwise it returns NONE.*)
    val filter : ('a -> bool) -> 'a stack -> 'a stack (*applies f to each element x of l, from left to right, and returns the list of those x for which f x evaluated to true, in the same order as they occurred in the argument list.
*)
     val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b (*f init [x1, x2, ..., xn]
returns
f(x1, f(x2, ..., f(xn, init)...))
or init if the list is empty.*)
    val rev : 'a stack -> 'a stack
    (* val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b (*f init [x1, x2, ..., xn] *)
    
returns
f(xn,...,f(x2, f(x1, init))...)
or init if the list is empty.*)
    val exists : ('a -> bool) -> 'a stack -> bool(*applies f to each element x of the list l, from left to right, until f x evaluates to true; it returns true if such an x exists and false otherwise.*)
    val all : ('a -> bool) -> 'a stack -> bool(*applies f to each element x of the list l, from left to right, until f x evaluates to false; it returns false if such an x exists and true otherwise. It is equivalent to not(exists (not o f) l)).*)
    val list2stack : 'a list -> 'a stack (* Convert a list into a stack *)
    val stack2list: 'a stack -> 'a list (* Convert a stack into a list *)
    val toString: ('a -> string) -> 'a stack -> string 

end ; 

structure Mystruct : STACK = 

    struct 
        datatype 'a stack = Empty | cons of 'a * ('a stack)
        exception EmptyStack
        exception IllegalIndexAccess
        exception Error of string 
        val create= Empty
        fun push(a,x) = cons(a,x)

        fun pop(Empty) = raise EmptyStack
           |pop(cons(a,b))=b
        fun top(Empty)= raise EmptyStack
            |top(cons(a,b))=a
            (* Note that empty is also a function and also a nullary constructor *)
        fun empty(Empty) = true  
            |empty(cons(_,_))=false
        fun poptop(Empty) = NONE
            |poptop(cons(a,b)) = SOME(a,b)
        
            (* Subscript is the inbuilt error that is raised when index is negative or maore than array size *)        
        fun nth(cons(a,Empty), x) =  if (x<>0) then raise Subscript else a 
            |nth(Empty,x) = raise EmptyStack
            |nth(cons(a,b),x)= nth(b,x-1)
        fun drop(a,0) = a
            |drop(Empty,x)= if x<>0 then raise Subscript else Empty
            |drop(cons(a,b),x)=drop(b,x-1);
        
        fun depth(Empty)=0
            |depth(cons(a,b))=1+depth(b);
        fun app f =  fn Empty => ()
                     | cons(a,b)=> let val (t:unit) = f(a) in (app f) b end 
        

        fun map f = fn Empty => Empty 
                    |cons(a,b)=> cons(f(a),(map f) b)

        fun mapPartial f = fn Empty => Empty
                            |(cons(a,b)) => case f(a) of NONE => ((mapPartial f) b) 
                                                        |SOME x => cons(x, (mapPartial f) b)

        fun find f = fn Empty => NONE 
                    |   cons(a,b)=> if (f(a)) then SOME a else (find f) b
        
        fun filter f = fn Empty => Empty 
                    | cons(a,b) => if (f(a)) then cons( a , (filter f) b) else (filter f) b ; 

        fun foldr f = fn init => fn Empty =>init 
                                |cons(a,b)=>f(a,foldr f init b)

        fun rev xs= let
                fun revhelp Empty ys = ys
                    | revhelp (cons(x,xs)) ys = revhelp xs (cons(x,ys))
                        in
                    revhelp xs Empty
                        end;
        
        fun exists f = fn Empty => false
                        |cons(a,b)=> if(f(a)) then true else (exists f) b
        fun all f = fn Empty => true 
                    | cons(a,b)=> if not(f(a)) then false else (all f) b
        
        fun  list2stack nil = Empty 
             |list2stack (a::b)= cons(a,list2stack(b))

        fun stack2list x = case x of Empty => nil 
                                    |cons(a,b)=> a::stack2list(b)
        
        fun toString f = fn Empty => ""
                         | cons(a,b)=>((f(a))^"_"^(toString f b))

        
        (* fun foldl f = fn init => fn (x:'a stack) => (((foldr f) init) rev(x))
         *)

        end;
signature VMC = 
    sig 
    type stackItems ; 
    (* type valuestackItems ; *)
    type stacksNmem;
    exception IllegalStateException ; 
    
    val fileName : string; 
    val trim : string ->string  
    val tree:DataTypes.AST
    val programname:string 
    val programblock:DataTypes.block
    val programdeclarationslist : DataTypes.declaration list 
    val programcommandslist : DataTypes.command list


    val booltoint : bool -> int 
    val assignInTable : DataTypes.declaration * int *((string,int) HashTable.hash_table) -> int 

    val assignhash : (DataTypes.declaration list) *(int)*((string,int) HashTable.hash_table) -> unit 



    val Variables : (string, int) HashTable.hash_table ; 
    val myVMC :stacksNmem;

    val rules : stacksNmem ->stacksNmem;
    (*val postfix : DataTypes.AST ->stacksNmem;*)

    val execute : stacksNmem->stacksNmem ; 

    val UnwrapFactor : (DataTypes.factor * (stackItems Mystruct.stack)) ->stackItems Mystruct.stack
    val UnwrapExpression : (DataTypes.expression*(stackItems Mystruct.stack)) -> stackItems Mystruct.stack


    val writeCommandList : (DataTypes.command list * stacksNmem) -> stacksNmem

    val operation1 :unit 
    (* val operation2: stacksNmem *)
    (* val finalstack: stacksNmem *)

    val postfix : DataTypes.command list -> string 
    val postfixExpression : DataTypes.expression -> string 
    val postfixFactor : DataTypes.factor -> string 
    val postfixcomparison : DataTypes.comparison -> string 
    val MemToString : (int Array.array*int) -> string 
    val toString : stacksNmem -> string list 
    val stacktostring : stackItems Mystruct.stack ->string
    (* val depth1 :unit
    val depth2:unit 
    val depth3:unit *)
    end
 structure Vmc :VMC = 
 struct 



    fun trim s = String.substring(s,0,size(s)-1)
    
    
    val fileName = let val useless = print "Enter fileName : " in trim(valOf(TextIO.inputLine(TextIO.stdIn))) end;

    val tree = while_ast.compile fileName; 

    val DataTypes.prog(programname,programblock)=tree;

    val DataTypes.block(programdeclarationslist,programcommandslist )=programblock;

    datatype stackItems = Number of int| Variable of string | Operator of string |UNARYoperator of string | CommandSeq of DataTypes.command list  |set |ITE | WH | read | write |WrappedExpression of DataTypes.expression 

    (* and valuestackItems = numberVal of int | VariableVal of string ;  *)

    val Variables : (string, int) HashTable.hash_table= 
    HashTable.mkTable (HashString.hashString, op=) (400, Fail "not found");
    
    
    
    type stacksNmem=((stackItems Mystruct.stack) * (int array)*(stackItems Mystruct.stack))
    val myVMC= (Mystruct.Empty:stackItems Mystruct.stack,Array.array(1000,0) ,Mystruct.Empty:stackItems Mystruct.stack)
    exception IllegalStateException;
    
    fun booltoint(x) = if x then 1 else 0


    and assignInTable(a:DataTypes.declaration ,memadr : int ,Variables: (string, int) HashTable.hash_table ) = 
    let val DataTypes.declaration(names : string list, typevalue) =a in 
        case names of [] => memadr 
                    | (part1::part2)=> let val buzz=HashTable.insert Variables (part1,memadr) in assignInTable(DataTypes.declaration(part2,typevalue),memadr+1,Variables) end
        
        end 
    
    and assignhash( mydeclarations: DataTypes.declaration list , memadr : int ,Variables: (string, int) HashTable.hash_table)= 
         case mydeclarations of 
            [] => ()
            |(a::b) => let val t = assignInTable(a,memadr,Variables) in assignhash(b,t,Variables) end  


    and writeCommandList(a,machine) = 
        case a of [] => machine 
                | ((DataTypes.SET(s1,e1))::b) => let val newmachine = writeCommandList(b,machine) in 
                    (#1 newmachine , #2 newmachine , Mystruct.push(Variable(s1),Mystruct.push(WrappedExpression(e1),Mystruct.push(set , (#3 newmachine)))) ) end 

                | ((DataTypes.READ(s1))::b) => let val newmachine = writeCommandList(b,machine) in 
                    (#1 newmachine , #2 newmachine , Mystruct.push(Variable(s1),Mystruct.push(read , (#3 newmachine)))) end

                | ((DataTypes.WRITE(e1))::b) => let val newmachine = writeCommandList(b,machine) in 
                    (#1 newmachine , #2 newmachine , Mystruct.push(WrappedExpression(e1),Mystruct.push(write , (#3 newmachine)))) end

                | ((DataTypes.ite(e1,cmdl1,cmdl2))::b) => let val newmachine = writeCommandList(b,machine) in 
                    (#1 newmachine , #2 newmachine , Mystruct.push(WrappedExpression(e1),Mystruct.push(CommandSeq(cmdl1),Mystruct.push(CommandSeq(cmdl2),Mystruct.push(ITE,(#3 (newmachine))))))) end

                | ((DataTypes.whiledo(e1,cmdl1))::b) => let val newmachine = writeCommandList(b,machine) in 
                    (#1 newmachine , #2 newmachine , Mystruct.push(WrappedExpression(e1),Mystruct.push(CommandSeq(cmdl1) ,Mystruct.push(WH,(#3 newmachine))))) end

                
    and MemToString (Memory,0) ="Memory => 0 : "^Int.toString(Array.sub(Memory,0))^", "
        |MemToString(Memory,a)=  MemToString(Memory,a-1)^Int.toString(a)^" : "^Int.toString(Array.sub(Memory,a))^", "
    and postfix([]:DataTypes.command list) = "" 
        |postfix(a::b) = 
        case a of 
            DataTypes.SET(x,y) => x^"_"^postfixExpression(y)^"SET_"^postfix(b)
            |DataTypes.READ(x)=>x^"_"^"READ_"^postfix(b)
            |DataTypes.WRITE(e)=>postfixExpression(e)^"WRITE_"^postfix(b)
            |DataTypes.ite(e,cmdl1,cmdl2)=>postfixExpression(e)^postfix(cmdl1)^postfix(cmdl2)^"ITE_"^postfix(b)
            |DataTypes.whiledo(e,cmdl)=>postfixExpression(e)^postfix(cmdl)^postfix(b)
    
    and postfixExpression(e) = 
        case e of 
            DataTypes.PLUS(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"+_"
            |DataTypes.MINUS(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"-_"
            |DataTypes.OR(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"||_"
            |DataTypes.MULTIPLY(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"*_"
            |DataTypes.DIVIDE(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"/_"
            |DataTypes.MOD(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"%_"
            |DataTypes.AND(e1,e2,_)=>postfixExpression(e1)^postfixExpression(e2)^"&&_"
            |DataTypes.NEG(e,_)=>postfixExpression(e)^"~_"

            |DataTypes.NOT(e,_)=>postfixExpression(e)^"!_"
            |DataTypes.FactorToExpr(f,_) => postfixFactor(f)

    and postfixFactor(f) = 
        case f of 
            DataTypes.NUMERALVAL(n)=>Int.toString(n)^"_"
            |DataTypes.BOOLVAL(b)=>Int.toString(booltoint(b))^"_"
            |DataTypes.COMPVAL(comp)=>postfixcomparison(comp)
            |DataTypes.ExpressionToFactor(e,_)=>postfixExpression(e)
            |DataTypes.VariableToFactor(v,_)=>v^"_"
        
    and postfixcomparison(comp) =
        case comp of 
            DataTypes.LT(e1,e2)=>postfixExpression(e1)^postfixExpression(e2)^"<_"
            |DataTypes.LEQ(e1,e2)=>postfixExpression(e1)^postfixExpression(e2)^"<=_"
            |DataTypes.GT(e1,e2)=>postfixExpression(e1)^postfixExpression(e2)^">_"
            |DataTypes.GEQ(e1,e2)=>postfixExpression(e1)^postfixExpression(e2)^">=_"
            |DataTypes.EQ(e1,e2)=>postfixExpression(e1)^postfixExpression(e2)^"=_"
            |DataTypes.conNEQ(e1,e2)=>postfixExpression(e1)^postfixExpression(e2)^"<>_"

    and stacktostring (Mystruct.Empty) = ""
        |stacktostring(Mystruct.cons(x,y))= 
        case x of 
            Number(n) => Int.toString(n)^"_"^stacktostring(y)
            |Variable(v)=> v^"_"^stacktostring(y)
            |Operator(opr)=>opr^"_"^stacktostring(y)
            |UNARYoperator(opr)=>opr^"_"^stacktostring(y)
            |CommandSeq(cseq)=>postfix(cseq)
            |set => "SET_"
            |ITE => "ITE_"
            |WH => "WH_"
            |read=>"READ_"
            |write=>"WRITE_"
            |WrappedExpression(e)=>postfixExpression(e)

         

    (* and toString() *)
    and toString (V,M,C)=(stacktostring(V)::(MemToString(M: int Array.array,HashTable.numItems Variables)::(stacktostring(C)::nil)))
        

    and UnwrapFactor(fact,currstack) = case fact of 

        (DataTypes.NUMERALVAL(a)) => Mystruct.push(Number(a),currstack)
        |(DataTypes.BOOLVAL(a))=>Mystruct.push(Number(booltoint(a)),currstack) (*yaha pe kuchh problem ho sakti hai boolean ke kaaran*)
        
        |DataTypes.COMPVAL(DataTypes.LT(e1,e2)) => (UnwrapExpression(e1,(UnwrapExpression(e2,Mystruct.push(Operator("<"),currstack)))))
                |DataTypes.COMPVAL(DataTypes.LEQ(e1,e2)) => (UnwrapExpression(e1,(UnwrapExpression(e2,Mystruct.push(Operator("<="),currstack)))))
                |DataTypes.COMPVAL(DataTypes.GT(e1,e2)) => (UnwrapExpression(e1,(UnwrapExpression(e2,Mystruct.push(Operator(">"),currstack)))))
                |DataTypes.COMPVAL(DataTypes.GEQ(e1,e2)) => (UnwrapExpression(e1,(UnwrapExpression(e2,Mystruct.push(Operator(">="),currstack)))))
                |DataTypes.COMPVAL(DataTypes.EQ(e1,e2)) => (UnwrapExpression(e1,(UnwrapExpression(e2,Mystruct.push(Operator("=="),currstack)))))
                |DataTypes.COMPVAL(DataTypes.conNEQ(e1,e2)) => (UnwrapExpression(e1,(UnwrapExpression(e2,Mystruct.push(Operator("<>"),currstack)))))

        |(DataTypes.ExpressionToFactor(e,_)) => (UnwrapExpression(e,currstack))
        |(DataTypes.VariableToFactor(v,_)) => Mystruct.push(Variable(v),currstack)

        

    and UnwrapExpression (expr,currstack) = case expr of 

        (DataTypes.PLUS(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("+"),currstack)))

    |(DataTypes.MINUS(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("-"),currstack)))

    |(DataTypes.OR(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("||"),currstack)))

    |(DataTypes.MULTIPLY(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("*"),currstack)))

    |(DataTypes.DIVIDE(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("/"),currstack)))

    |(DataTypes.MOD(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("%"),currstack)))

    |(DataTypes.AND(e1,e2,_)) => UnwrapExpression(e1,UnwrapExpression(e2,Mystruct.push(Operator("&&"),currstack)))

    |(DataTypes.NEG(e1,_)) => UnwrapExpression(e1,Mystruct.push(UNARYoperator("~"),currstack))

    |(DataTypes.NOT(e1,_)) => UnwrapExpression(e1,Mystruct.push(UNARYoperator("!"),currstack))

    |(DataTypes.FactorToExpr(f,_)) => UnwrapFactor(f,currstack) 


(* ABHI UNWRAP FACTOR COMPLETE KARNA HAI *)

    
    and VariableValue (var:string) = Array.sub(#2 (myVMC), HashTable.lookup Variables var) 

    
    and rules (a:stacksNmem) = case a of 

        (V,M,Mystruct.cons(Variable(x),Mystruct.cons(read,C))) => 
            let val prompt = print("Enter value of variable "^x^": ") ;
                val userinput = valOf(TextIO.inputLine(TextIO.stdIn));
                val finalinput  = valOf(Int.fromString((trim userinput))) ; 

                in  

                let val p:(unit) = Array.update(M: int array ,HashTable.lookup Variables x, finalinput:int) in (V,M,C)  end end 
        
        |(V,M,Mystruct.cons(WrappedExpression(e),Mystruct.cons(write,C)))=> 
            (V,M,UnwrapExpression(e,Mystruct.cons(write,C)))
        
         |(V,M,Mystruct.cons(WrappedExpression(e),Mystruct.cons(CommandSeq(c1),Mystruct.cons(CommandSeq(c2),Mystruct.cons(ITE,C))))) => (V,M,UnwrapExpression(e,Mystruct.cons(CommandSeq(c1),Mystruct.cons(CommandSeq(c2),Mystruct.cons(ITE,C))))) 

         |((Mystruct.cons(Number(0),V)) , M, Mystruct.cons(CommandSeq(c1),Mystruct.cons(CommandSeq(c2),Mystruct.cons(ITE,C)))) => writeCommandList(c2, (V,M,C))

         |((Mystruct.cons(Number(1),V)) , M, Mystruct.cons(CommandSeq(c1),Mystruct.cons(CommandSeq(c2),Mystruct.cons(ITE,C)))) => writeCommandList(c1, (V,M,C))

         |(V,M,Mystruct.cons(WrappedExpression(b),Mystruct.cons(CommandSeq(cmdl),Mystruct.cons(WH,C)))) => ((Mystruct.cons(CommandSeq(cmdl),Mystruct.cons(WrappedExpression(b),V))),M,UnwrapExpression(b,Mystruct.cons(WH,C)))

         |((Mystruct.cons(Number(0),Mystruct.cons(CommandSeq(cmdl),Mystruct.cons(WrappedExpression(b),V)))) ,M ,Mystruct.cons(WH,C)  ) => (V,M,C)

         |((Mystruct.cons(Number(1),Mystruct.cons(CommandSeq(cmdl),Mystruct.cons(WrappedExpression(b),V)))) ,M ,Mystruct.cons(WH,C)  ) => writeCommandList(cmdl,(V,M,Mystruct.cons(WrappedExpression(b),Mystruct.cons(CommandSeq(cmdl),Mystruct.cons(WH,C)))))

        |((Mystruct.cons(Number(n),V)),M,Mystruct.cons(write,C))=> 
            let val output = print(Int.toString(n)^"\n")  in (V,M,C) end 



        |(V,M,Mystruct.cons(Number(m),Mystruct.cons(UNARYoperator(opr),C))) => 
            (Mystruct.cons(Number(m),V),M,Mystruct.cons(UNARYoperator(opr),C)) (* UNary Operator number*)


        | (V,M,Mystruct.cons(Variable(x),Mystruct.cons(UNARYoperator(opr),C))) => 
            (Mystruct.cons(Number(VariableValue x),V),M,Mystruct.cons(UNARYoperator(opr),C)) (*Unaryoperaor with variable*)

        |(V,M,Mystruct.cons(Number(m),Mystruct.cons(Number(n),Mystruct.cons(Operator(opr),C)) ) ) => (Mystruct.cons(Number(n),Mystruct.cons(Number(m),V)),M,Mystruct.cons(Operator(opr),C)) (*E.o00*)

        |(V,M,Mystruct.cons(Number(m),Mystruct.cons(Variable(x),Mystruct.cons(Operator(opr),C)) ) ) => (Mystruct.cons(Number(VariableValue x),Mystruct.cons(Number(m),V)),M,Mystruct.cons(Operator(opr),C)) (*E.o01*)

        |(V,M,Mystruct.cons(Variable(x),Mystruct.cons(Number(m),Mystruct.cons(Operator(opr),C)) ) ) => (Mystruct.cons(Number(m),Mystruct.cons(Number(VariableValue x),V)),M,Mystruct.cons(Operator(opr),C)) (*E.o10*)

        |(V,M,Mystruct.cons(Variable(x),Mystruct.cons(Variable(y),Mystruct.cons(Operator(opr),C)) ) ) => (Mystruct.cons(Number(VariableValue y),Mystruct.cons(Number(VariableValue x),V)),M,Mystruct.cons(Operator(opr),C)) (*E.o11*)

        |(V,M,Mystruct.cons(Number(m),C)) => (Mystruct.cons(Number(m),V),M,C) (*E.m*)
        | (V,M,Mystruct.cons(WrappedExpression(e),C))=>(V,M,UnwrapExpression(e,C))
        |(Mystruct.cons(Number(a),V),M,Mystruct.cons(UNARYoperator(opr),C)) => 
        if(opr = "~") then (Mystruct.cons(Number((~1)*a),V),M,C)
        else (Mystruct.cons(Number(1-a),V),M,C)
        

        | (Mystruct.cons(Number(n),Mystruct.cons(Number(m),V)),M,Mystruct.cons(Operator(opr),C)) => 
            if( opr = "+") then (Mystruct.cons(Number(m+n),V),M,C)
            else if(opr="-") then (Mystruct.cons(Number(m-n),V),M,C)
            else if(opr="*") then (Mystruct.cons(Number(m*n),V),M,C)
            else if(opr="/") then (Mystruct.cons(Number(m div n),V),M,C)    (*raise for division by zero *)
            else if(opr="%") then (Mystruct.cons(Number(m mod n),V),M,C)
            else if(opr="||") then  if( m=0 ) then (Mystruct.cons(Number(n),V),M,C) else (Mystruct.cons(Number(1),V),M,C)
             
            else if(opr="&&") then if (m=1) then (Mystruct.cons(Number(n),V),M,C) else (Mystruct.cons(Number(0),V),M,C)

            else if(opr="<") then (Mystruct.cons(Number(booltoint(m<n)),V),M,C) 

            else if(opr="<=") then (Mystruct.cons(Number(booltoint(m<=n)),V),M,C)
            else if(opr=">") then (Mystruct.cons(Number(booltoint(m>n)),V),M,C)
            else if(opr=">=") then (Mystruct.cons(Number(booltoint(m>=n)),V),M,C)
            else if(opr="==") then (Mystruct.cons(Number(booltoint(m=n)),V),M,C)
            else (Mystruct.cons(Number(booltoint(m<>n)),V),M,C)
        
        |(V,M,Mystruct.cons(CommandSeq([]),C)) => (V,M,C)

        |(V,M,Mystruct.cons(Variable(x), Mystruct.cons(WrappedExpression(e),Mystruct.cons(set,C) ) )) =>      (Mystruct.cons(Variable (x),V),M,UnwrapExpression(e,Mystruct.cons(set,C) ) )

        |(V,M,Mystruct.cons(Variable(x),C)) => (Mystruct.cons(Number(VariableValue x), V ),M,C) (*E.x*)
        |(Mystruct.cons(Number(m),Mystruct.cons(Variable(x),V)),M,Mystruct.cons(set,C))=> let val p:(unit) = Array.update(M: int array ,HashTable.lookup Variables x, m:int) (*; val useless = print"writing in mem" *)in (V,M,C)  end


        


    and execute (A,B,Mystruct.Empty : stackItems Mystruct.stack) =  (A,B,Mystruct.Empty : stackItems Mystruct.stack) 
       |execute (x:stacksNmem) =  (*let val useles= print "ran a rule" in *)execute(rules(x))  (*end*) 


    val operation1 = (*let val useles = print"hi" in *)assignhash(programdeclarationslist,0,Variables) (*end;*)

    val myVMC = (*let val useles = print"hi2" in *)writeCommandList(programcommandslist,myVMC)(* end;*)


  (*  val finalstack = (*let val uselses = print "executing " in *)  execute(myVMC) end ;*)


(* 
    val depth1 =Control.Print.printLength := 15; (* set printing parameters so that *)
    val depth2 =Control.Print.printDepth := 15; (* we’ll see all details of parse trees *)
    val depth3 =Control.Print.stringDepth := 15; *)
        
 end;

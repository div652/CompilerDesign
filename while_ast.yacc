open DataTypes;
open HashTable;
open HashString;
fun typewriter ([] : string list,y :int,table:(string,int) HashTable.hash_table )   = ()
    |typewriter (x::tail , y , table) = if(HashTable.insert table (x ,y) =()) then typewriter(tail, y , table ) else (); 


val typetable : (string, int) hash_table =
    HashTable.mkTable (hashString, op=) (100, Fail "not found");

%%

%name while_ast
%term PROGRAM|DOUBLECOLON| VARIABLE |COLON|SEMICOLON| INTLABEL | BOOLLABEL|COMMA|LCURL|RCURL|ASSIGN| READ | WRITE | IF|THEN |ELSE | ENDIF | WHILE | DO |ENDWH|PLUS|MINUS|OR|MULTIPLY|DIVIDE|MOD|AND|TRUE|FALSE|NEG|NOT|LPAREN|RPAREN|LT|LEQ|GT|GEQ|EQ|NEQ|NAME of string|INTEGER of int|EOF |ILLCH

 %nonterm programNT of AST | blockNT of block| declarationseqNT of declaration list|declarationNT of declaration|typeNT of int|variablelistNT of string list |commavariablelistNT of string list|commandlistNT of command list|commandseqNT of command list|commandNT of command|expressionNT of expression|FactorNT of factor|comparisonNT of comparison|variableNT of string|identifierNT of string|numeralNT of int
 
 %keyword PROGRAM DOUBLECOLON VARIABLE COLON SEMICOLON INTLABEL BOOLLABEL COMMA LCURL RCURL ASSIGN READ WRITE IF THEN ELSE ENDIF WHILE DO ENDWH LPAREN RPAREN OR AND TRUE FALSE NOT LT LEQ EQ GT GEQ NEQ PLUS MINUS MULTIPLY DIVIDE MOD 

 %pos int
 %eop EOF
 %noshift EOF
 

 %verbose
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%nonassoc LT GT LEQ GEQ

 %left MINUS PLUS 
 %left MULTIPLY DIVIDE MOD
 %right NEG
 %right NOT
 %nonassoc LPAREN RPAREN

%nodefault

 %%
 


 programNT : PROGRAM identifierNT DOUBLECOLON blockNT (prog(identifierNT,blockNT)) 

 blockNT : declarationseqNT  LCURL commandlistNT RCURL (block(declarationseqNT,commandlistNT))

declarationseqNT: declarationNT declarationseqNT (declarationNT::declarationseqNT)| ([])

 declarationNT: VARIABLE variablelistNT COLON typeNT SEMICOLON ( typewriter(variablelistNT , typeNT , typetable) ;declaration(variablelistNT , typeNT) )



typeNT  : INTLABEL (1) | BOOLLABEL(0)

 variablelistNT : variableNT commavariablelistNT (variableNT :: commavariablelistNT ) | ([])

 commavariablelistNT : COMMA variableNT commavariablelistNT (variableNT :: commavariablelistNT) | ([])

 commandlistNT : commandNT commandlistNT (commandNT :: commandlistNT) | ([])



commandNT : variableNT ASSIGN expressionNT SEMICOLON %prec ASSIGN(if( (HashTable.lookup typetable (variableNT) ) = expressiontype(expressionNT)   )then SET (variableNT,expressionNT) else raise Fail("Type mismatch occured")) 
| READ variableNT SEMICOLON(READ(variableNT)) 
| WRITE expressionNT SEMICOLON(WRITE(expressionNT)) 
| IF expressionNT THEN LCURL commandlistNT RCURL ELSE LCURL commandlistNT RCURL ENDIF SEMICOLON(if(expressiontype(expressionNT)=1) then raise Fail("Type mismatch occured") else ite(expressionNT,commandlistNT1,commandlistNT2) )
|WHILE expressionNT DO LCURL commandlistNT RCURL ENDWH SEMICOLON (whiledo(expressionNT1,commandlistNT))




expressionNT : expressionNT PLUS expressionNT  %prec PLUS(if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else PLUS(expressionNT1,expressionNT2,1))
                |expressionNT MINUS expressionNT %prec MINUS(if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else MINUS(expressionNT1,expressionNT2,1))

                |expressionNT OR expressionNT %prec OR(if (expressiontype(expressionNT1)=1 orelse expressiontype(expressionNT2)=1) then raise Fail("Type mismatch occured") else OR(expressionNT1,expressionNT2,0))

                |expressionNT MULTIPLY expressionNT %prec MULTIPLY (if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else MULTIPLY(expressionNT1,expressionNT2,1))

                |expressionNT DIVIDE expressionNT %prec DIVIDE (if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else DIVIDE(expressionNT1,expressionNT2,1))

                |expressionNT AND expressionNT %prec AND(if (expressiontype(expressionNT1)=1 orelse expressiontype(expressionNT2)=1) then raise Fail("Type mismatch occured") else AND(expressionNT1,expressionNT2,0))
                |expressionNT MOD expressionNT %prec MOD (if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else MOD(expressionNT1,expressionNT2,1))
                |NEG expressionNT %prec NEG (if(expressiontype(expressionNT)=0) then raise Fail("Type mismatch occured") else NEG(expressionNT,1))
                |NOT expressionNT %prec NOT (if(expressiontype(expressionNT)=1) then raise Fail("Type mismatch occured") else NOT(expressionNT,0))
                |FactorNT (FactorToExpr(FactorNT,factortype(FactorNT))) 


FactorNT : numeralNT (NUMERALVAL(numeralNT))
            |TRUE  (BOOLVAL true )
            |FALSE (BOOLVAL false)
            |comparisonNT (COMPVAL comparisonNT)
           
            
            |LPAREN expressionNT RPAREN (ExpressionToFactor(expressionNT,expressiontype(expressionNT)))
            |variableNT (VariableToFactor(variableNT,HashTable.lookup typetable (variableNT)))

comparisonNT :expressionNT LT expressionNT (if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then LT (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured"))

|expressionNT LEQ expressionNT (if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then LEQ (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured"))

|expressionNT GT expressionNT (if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then GT  (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured"))

|expressionNT GEQ expressionNT (if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then GEQ (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured"))

|expressionNT EQ expressionNT (if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then EQ  (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured"))

|expressionNT NEQ expressionNT (if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then conNEQ  (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured"))


variableNT : identifierNT (identifierNT)
identifierNT : NAME (NAME)
numeralNT : INTEGER (INTEGER)| PLUS INTEGER ((INTEGER)) 

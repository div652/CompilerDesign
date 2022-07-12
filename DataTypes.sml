signature DATATYPES =
sig
  datatype AST = prog of (string*block)
        and block = block of ((declaration list )*(command list))
        and declaration = declaration of ((string list) * int)
        and command  = SET of (string*expression)
          |READ of string
          |WRITE of expression
          |ite of (expression*(command list)*(command list))
          |whiledo of (expression*(command list))
        
        and expression  = PLUS of (expression*expression*int)
              |MINUS of (expression*expression*int)
              |OR of (expression*expression*int)
              |MULTIPLY of (expression*expression*int)
              |DIVIDE of (expression*expression*int)
              |MOD of (expression*expression*int)
              |AND of (expression*expression*int)
              |NEG of (expression*int)
              |NOT of (expression*int)
              |FactorToExpr of (factor*int) 

        
      

        and factor  =    NUMERALVAL of int 
            |BOOLVAL of bool
            |COMPVAL of comparison
            |ExpressionToFactor of (expression*int)
            |VariableToFactor of (string *int)

        and comparison = LT of expression*expression
                        |LEQ of expression*expression
                        |GT of expression*expression
                        |GEQ of expression*expression
                        |EQ of expression*expression
                        |conNEQ of expression*expression
        val expressiontype : expression ->int
        val factortype : factor ->int
end;




structure DataTypes :DATATYPES =
struct
  datatype AST = prog of (string*block)
        and block = block of ((declaration list )*(command list))
        and declaration = declaration of ((string list) * int)
        and command  = SET of (string*expression)
          |READ of string
          |WRITE of expression
          |ite of (expression*(command list)*(command list))
          |whiledo of (expression*(command list))
        
        and expression  = PLUS of (expression*expression*int)
              |MINUS of (expression*expression*int)
              |OR of (expression*expression*int)
              |MULTIPLY of (expression*expression*int)
              |DIVIDE of (expression*expression*int)
              |MOD of (expression*expression*int)
              |AND of (expression*expression*int)
            |NEG of (expression*int)
              |NOT of (expression*int)
              |FactorToExpr of (factor*int) 

        
      

        and factor  =    NUMERALVAL of int 
            |BOOLVAL of bool
            |COMPVAL of comparison
            |ExpressionToFactor of (expression*int)
            |VariableToFactor of (string *int)

        and comparison = LT of expression*expression
                        |LEQ of expression*expression
                        |GT of expression*expression
                        |GEQ of expression*expression
                        |EQ of expression*expression
                        |conNEQ of expression*expression

        fun expressiontype(PLUS(a,b,c)) = c
    |expressiontype(MINUS(a,b,c))=c
    |expressiontype(OR(a,b,c)) = c
    |expressiontype(MULTIPLY(a,b,c)) = c
    |expressiontype(DIVIDE(a,b,c)) = c
    |expressiontype(MOD(a,b,c)) = c
    |expressiontype(AND(a,b,c)) = c
    |expressiontype(FactorToExpr(a,b))=b 
    |expressiontype(NOT(a,b))=b
    |expressiontype(NEG(a,b))=b;

    fun factortype( NUMERALVAL(x))=1
    |factortype(BOOLVAL(x))=0
    |factortype(COMPVAL(x))=0

    |factortype(ExpressionToFactor(a,b))=b
    |factortype(VariableToFactor(a,b))=b;

                        
end;

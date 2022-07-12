open Tokens
type pos = int
type lexresult= (svalue,pos) token
fun eof() =  EOF(0,0);
fun integer(str,lexPos) =
case Int.fromString(str) of
NONE => raise Fail("Shouldn't happen: sequence of digits not recognized as integer -- " ^ str)
| SOME(n) => INTEGER(n,lexPos,lexPos)
(* For scanner initialization (not needed here) *)

fun init() = ()
%%

%header (functor while_astLexFun(structure Tokens: while_ast_TOKENS));
alpha=[a-zA-Z];
digit=[0-9];
whitespace=[\ \t\n];

%%

"program" =>(PROGRAM(yypos,yypos));
"::" =>(DOUBLECOLON(yypos,yypos));
"var" =>(VARIABLE(yypos,yypos));
":" =>(COLON(yypos,yypos));
";" =>(SEMICOLON(yypos,yypos));
"int" =>(INTLABEL(yypos,yypos));
"bool" =>(BOOLLABEL(yypos,yypos));
"," =>(COMMA(yypos,yypos));
"{" =>(LCURL(yypos,yypos));
"}" =>(RCURL(yypos,yypos));
":=" =>(ASSIGN(yypos,yypos));
"read" =>(READ(yypos,yypos));
"write" =>(WRITE(yypos,yypos));
"if" =>(IF(yypos,yypos));
"then" =>(THEN(yypos,yypos));
"else" =>(ELSE(yypos,yypos));
"endif" =>(ENDIF(yypos,yypos));
"while" =>(WHILE(yypos,yypos));
"do" =>(DO(yypos,yypos));
"endwh" =>(ENDWH(yypos,yypos));
"(" =>(LPAREN(yypos,yypos));
")" =>(RPAREN(yypos,yypos));
"||" =>(OR(yypos,yypos));
"&&" =>(AND(yypos,yypos));
"tt" =>(TRUE(yypos,yypos));
"ff" =>(FALSE(yypos,yypos));
"!" =>(NOT(yypos,yypos));
"<" =>(LT(yypos,yypos));
"<=" =>(LEQ(yypos,yypos));
"=" =>(EQ(yypos,yypos));
">" =>(GT(yypos,yypos));
">=" =>(GEQ(yypos,yypos));
"<>" =>(NEQ(yypos,yypos));
"+" =>(PLUS(yypos,yypos));
"-" =>(MINUS(yypos,yypos));
"*" =>(MULTIPLY(yypos,yypos));
"/" =>(DIVIDE(yypos,yypos));
"%" =>(MOD(yypos,yypos));
"~" =>(NEG(yypos,yypos));
{whitespace}* =>(continue()); 

{digit}+ => (integer(yytext,yypos));
{alpha}({alpha}|{digit})* => (NAME(yytext,yypos,yypos));
. => (lex());
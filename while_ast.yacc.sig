signature while_ast_TOKENS =
sig
type ('a,'b) token
type svalue
val ILLCH:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val INTEGER: (int) *  'a * 'a -> (svalue,'a) token
val NAME: (string) *  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val GEQ:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LEQ:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val MULTIPLY:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val ENDWH:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ENDIF:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WRITE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val RCURL:  'a * 'a -> (svalue,'a) token
val LCURL:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val BOOLLABEL:  'a * 'a -> (svalue,'a) token
val INTLABEL:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val VARIABLE:  'a * 'a -> (svalue,'a) token
val DOUBLECOLON:  'a * 'a -> (svalue,'a) token
val PROGRAM:  'a * 'a -> (svalue,'a) token
end
signature while_ast_LRVALS=
sig
structure Tokens : while_ast_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

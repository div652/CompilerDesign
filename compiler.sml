(* compiler.sml *)
structure while_ast :
sig val compile : string -> DataTypes.AST
end =
struct
exception while_astError;
fun compile (fileName:string)= 
let val inStream = TextIO.openIn fileName;
val grab : int -> string = fn
n => if TextIO.endOfStream inStream
then ""
else TextIO.inputN (inStream,n);
val printError : string * int * int -> unit = fn
(msg,line,col) =>
print (fileName^"["^Int.toString line^":"
^Int.toString col^"] "^msg^"\n");
val (tree,rem) = while_astParser.parse
(15,
(while_astParser.makeLexer grab ),
printError,
())
handle while_astParser.ParseError => raise while_astError;
(* Close the source program file *)
val _ = TextIO.closeIn inStream;
in tree 
end
end;
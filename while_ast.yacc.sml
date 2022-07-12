functor while_astLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : while_ast_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes;
open HashTable;
open HashString;
fun typewriter ([] : string list,y :int,table:(string,int) HashTable.hash_table )   = ()
    |typewriter (x::tail , y , table) = if(HashTable.insert table (x ,y) =()) then typewriter(tail, y , table ) else (); 


val typetable : (string, int) hash_table =
    HashTable.mkTable (hashString, op=) (100, Fail "not found");


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\003\000\000\000\
\\001\000\002\000\143\000\004\000\143\000\005\000\143\000\008\000\143\000\
\\011\000\143\000\015\000\143\000\019\000\143\000\021\000\143\000\
\\022\000\143\000\023\000\143\000\024\000\143\000\025\000\143\000\
\\026\000\143\000\027\000\143\000\033\000\143\000\034\000\143\000\
\\035\000\143\000\036\000\143\000\037\000\143\000\038\000\143\000\
\\039\000\143\000\000\000\
\\001\000\002\000\006\000\000\000\
\\001\000\003\000\106\000\009\000\106\000\000\000\
\\001\000\003\000\010\000\009\000\105\000\000\000\
\\001\000\004\000\109\000\000\000\
\\001\000\004\000\110\000\040\000\005\000\000\000\
\\001\000\004\000\111\000\000\000\
\\001\000\004\000\112\000\008\000\024\000\000\000\
\\001\000\004\000\142\000\005\000\142\000\008\000\142\000\011\000\142\000\
\\015\000\142\000\019\000\142\000\021\000\142\000\022\000\142\000\
\\023\000\142\000\024\000\142\000\025\000\142\000\026\000\142\000\
\\027\000\142\000\033\000\142\000\034\000\142\000\035\000\142\000\
\\036\000\142\000\037\000\142\000\038\000\142\000\039\000\142\000\000\000\
\\001\000\004\000\025\000\000\000\
\\001\000\005\000\107\000\000\000\
\\001\000\005\000\108\000\000\000\
\\001\000\005\000\120\000\015\000\120\000\019\000\120\000\021\000\120\000\
\\022\000\120\000\023\000\120\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\120\000\033\000\120\000\034\000\120\000\
\\035\000\120\000\036\000\120\000\037\000\120\000\038\000\120\000\
\\039\000\120\000\000\000\
\\001\000\005\000\121\000\015\000\121\000\019\000\121\000\021\000\121\000\
\\022\000\121\000\023\000\121\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\121\000\033\000\121\000\034\000\121\000\
\\035\000\121\000\036\000\121\000\037\000\121\000\038\000\121\000\
\\039\000\121\000\000\000\
\\001\000\005\000\122\000\015\000\122\000\019\000\122\000\021\000\061\000\
\\022\000\060\000\023\000\122\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\055\000\033\000\122\000\034\000\054\000\
\\035\000\053\000\036\000\052\000\037\000\051\000\038\000\050\000\
\\039\000\049\000\000\000\
\\001\000\005\000\123\000\015\000\123\000\019\000\123\000\021\000\123\000\
\\022\000\123\000\023\000\123\000\024\000\123\000\025\000\123\000\
\\026\000\123\000\027\000\123\000\033\000\123\000\034\000\123\000\
\\035\000\123\000\036\000\123\000\037\000\123\000\038\000\123\000\
\\039\000\123\000\000\000\
\\001\000\005\000\124\000\015\000\124\000\019\000\124\000\021\000\124\000\
\\022\000\124\000\023\000\124\000\024\000\124\000\025\000\124\000\
\\026\000\124\000\027\000\124\000\033\000\124\000\034\000\124\000\
\\035\000\124\000\036\000\124\000\037\000\124\000\038\000\124\000\
\\039\000\124\000\000\000\
\\001\000\005\000\125\000\015\000\125\000\019\000\125\000\021\000\061\000\
\\022\000\060\000\023\000\125\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\125\000\033\000\125\000\034\000\054\000\
\\035\000\053\000\036\000\052\000\037\000\051\000\038\000\050\000\
\\039\000\049\000\000\000\
\\001\000\005\000\126\000\015\000\126\000\019\000\126\000\021\000\126\000\
\\022\000\126\000\023\000\126\000\024\000\126\000\025\000\126\000\
\\026\000\126\000\027\000\126\000\033\000\126\000\034\000\126\000\
\\035\000\126\000\036\000\126\000\037\000\126\000\038\000\126\000\
\\039\000\126\000\000\000\
\\001\000\005\000\127\000\015\000\127\000\019\000\127\000\021\000\127\000\
\\022\000\127\000\023\000\127\000\024\000\127\000\025\000\127\000\
\\026\000\127\000\027\000\127\000\033\000\127\000\034\000\127\000\
\\035\000\127\000\036\000\127\000\037\000\127\000\038\000\127\000\
\\039\000\127\000\000\000\
\\001\000\005\000\128\000\015\000\128\000\019\000\128\000\021\000\128\000\
\\022\000\128\000\023\000\128\000\024\000\128\000\025\000\128\000\
\\026\000\128\000\027\000\128\000\033\000\128\000\034\000\128\000\
\\035\000\128\000\036\000\128\000\037\000\128\000\038\000\128\000\
\\039\000\128\000\000\000\
\\001\000\005\000\129\000\015\000\129\000\019\000\129\000\021\000\129\000\
\\022\000\129\000\023\000\129\000\024\000\129\000\025\000\129\000\
\\026\000\129\000\027\000\129\000\033\000\129\000\034\000\129\000\
\\035\000\129\000\036\000\129\000\037\000\129\000\038\000\129\000\
\\039\000\129\000\000\000\
\\001\000\005\000\130\000\015\000\130\000\019\000\130\000\021\000\130\000\
\\022\000\130\000\023\000\130\000\024\000\130\000\025\000\130\000\
\\026\000\130\000\027\000\130\000\033\000\130\000\034\000\130\000\
\\035\000\130\000\036\000\130\000\037\000\130\000\038\000\130\000\
\\039\000\130\000\000\000\
\\001\000\005\000\131\000\015\000\131\000\019\000\131\000\021\000\131\000\
\\022\000\131\000\023\000\131\000\024\000\131\000\025\000\131\000\
\\026\000\131\000\027\000\131\000\033\000\131\000\034\000\131\000\
\\035\000\131\000\036\000\131\000\037\000\131\000\038\000\131\000\
\\039\000\131\000\000\000\
\\001\000\005\000\132\000\015\000\132\000\019\000\132\000\021\000\132\000\
\\022\000\132\000\023\000\132\000\024\000\132\000\025\000\132\000\
\\026\000\132\000\027\000\132\000\033\000\132\000\034\000\132\000\
\\035\000\132\000\036\000\132\000\037\000\132\000\038\000\132\000\
\\039\000\132\000\000\000\
\\001\000\005\000\133\000\015\000\133\000\019\000\133\000\021\000\133\000\
\\022\000\133\000\023\000\133\000\024\000\133\000\025\000\133\000\
\\026\000\133\000\027\000\133\000\033\000\133\000\034\000\133\000\
\\035\000\133\000\036\000\133\000\037\000\133\000\038\000\133\000\
\\039\000\133\000\000\000\
\\001\000\005\000\134\000\015\000\134\000\019\000\134\000\021\000\134\000\
\\022\000\134\000\023\000\134\000\024\000\134\000\025\000\134\000\
\\026\000\134\000\027\000\134\000\033\000\134\000\034\000\134\000\
\\035\000\134\000\036\000\134\000\037\000\134\000\038\000\134\000\
\\039\000\134\000\000\000\
\\001\000\005\000\135\000\015\000\135\000\019\000\135\000\021\000\135\000\
\\022\000\135\000\023\000\135\000\024\000\135\000\025\000\135\000\
\\026\000\135\000\027\000\135\000\033\000\135\000\034\000\135\000\
\\035\000\135\000\036\000\135\000\037\000\135\000\038\000\135\000\
\\039\000\135\000\000\000\
\\001\000\005\000\136\000\015\000\136\000\019\000\136\000\021\000\061\000\
\\022\000\060\000\023\000\136\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\136\000\033\000\136\000\038\000\136\000\
\\039\000\136\000\000\000\
\\001\000\005\000\137\000\015\000\137\000\019\000\137\000\021\000\061\000\
\\022\000\060\000\023\000\137\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\137\000\033\000\137\000\038\000\137\000\
\\039\000\137\000\000\000\
\\001\000\005\000\138\000\015\000\138\000\019\000\138\000\021\000\061\000\
\\022\000\060\000\023\000\138\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\138\000\033\000\138\000\038\000\138\000\
\\039\000\138\000\000\000\
\\001\000\005\000\139\000\015\000\139\000\019\000\139\000\021\000\061\000\
\\022\000\060\000\023\000\139\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\139\000\033\000\139\000\038\000\139\000\
\\039\000\139\000\000\000\
\\001\000\005\000\140\000\015\000\140\000\019\000\140\000\021\000\061\000\
\\022\000\060\000\023\000\140\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\140\000\033\000\140\000\034\000\054\000\
\\035\000\053\000\036\000\052\000\037\000\051\000\038\000\140\000\
\\039\000\140\000\000\000\
\\001\000\005\000\141\000\015\000\141\000\019\000\141\000\021\000\061\000\
\\022\000\060\000\023\000\141\000\024\000\058\000\025\000\057\000\
\\026\000\056\000\027\000\141\000\033\000\141\000\034\000\054\000\
\\035\000\053\000\036\000\052\000\037\000\051\000\038\000\141\000\
\\039\000\141\000\000\000\
\\001\000\005\000\144\000\015\000\144\000\019\000\144\000\021\000\144\000\
\\022\000\144\000\023\000\144\000\024\000\144\000\025\000\144\000\
\\026\000\144\000\027\000\144\000\033\000\144\000\034\000\144\000\
\\035\000\144\000\036\000\144\000\037\000\144\000\038\000\144\000\
\\039\000\144\000\000\000\
\\001\000\005\000\145\000\015\000\145\000\019\000\145\000\021\000\145\000\
\\022\000\145\000\023\000\145\000\024\000\145\000\025\000\145\000\
\\026\000\145\000\027\000\145\000\033\000\145\000\034\000\145\000\
\\035\000\145\000\036\000\145\000\037\000\145\000\038\000\145\000\
\\039\000\145\000\000\000\
\\001\000\005\000\068\000\021\000\061\000\022\000\060\000\023\000\059\000\
\\024\000\058\000\025\000\057\000\026\000\056\000\027\000\055\000\
\\034\000\054\000\035\000\053\000\036\000\052\000\037\000\051\000\
\\038\000\050\000\039\000\049\000\000\000\
\\001\000\005\000\069\000\000\000\
\\001\000\005\000\071\000\000\000\
\\001\000\005\000\072\000\021\000\061\000\022\000\060\000\023\000\059\000\
\\024\000\058\000\025\000\057\000\026\000\056\000\027\000\055\000\
\\034\000\054\000\035\000\053\000\036\000\052\000\037\000\051\000\
\\038\000\050\000\039\000\049\000\000\000\
\\001\000\005\000\095\000\000\000\
\\001\000\005\000\100\000\000\000\
\\001\000\006\000\047\000\007\000\046\000\000\000\
\\001\000\009\000\104\000\000\000\
\\001\000\009\000\012\000\000\000\
\\001\000\009\000\086\000\000\000\
\\001\000\009\000\088\000\000\000\
\\001\000\009\000\096\000\000\000\
\\001\000\010\000\113\000\000\000\
\\001\000\010\000\114\000\012\000\022\000\013\000\021\000\014\000\020\000\
\\018\000\019\000\040\000\005\000\000\000\
\\001\000\010\000\115\000\012\000\115\000\013\000\115\000\014\000\115\000\
\\018\000\115\000\040\000\115\000\000\000\
\\001\000\010\000\116\000\012\000\116\000\013\000\116\000\014\000\116\000\
\\018\000\116\000\040\000\116\000\000\000\
\\001\000\010\000\117\000\012\000\117\000\013\000\117\000\014\000\117\000\
\\018\000\117\000\040\000\117\000\000\000\
\\001\000\010\000\118\000\012\000\118\000\013\000\118\000\014\000\118\000\
\\018\000\118\000\040\000\118\000\000\000\
\\001\000\010\000\119\000\012\000\119\000\013\000\119\000\014\000\119\000\
\\018\000\119\000\040\000\119\000\000\000\
\\001\000\010\000\028\000\000\000\
\\001\000\010\000\091\000\000\000\
\\001\000\010\000\092\000\000\000\
\\001\000\010\000\098\000\000\000\
\\001\000\011\000\026\000\000\000\
\\001\000\015\000\067\000\021\000\061\000\022\000\060\000\023\000\059\000\
\\024\000\058\000\025\000\057\000\026\000\056\000\027\000\055\000\
\\034\000\054\000\035\000\053\000\036\000\052\000\037\000\051\000\
\\038\000\050\000\039\000\049\000\000\000\
\\001\000\016\000\094\000\000\000\
\\001\000\017\000\099\000\000\000\
\\001\000\019\000\062\000\021\000\061\000\022\000\060\000\023\000\059\000\
\\024\000\058\000\025\000\057\000\026\000\056\000\027\000\055\000\
\\034\000\054\000\035\000\053\000\036\000\052\000\037\000\051\000\
\\038\000\050\000\039\000\049\000\000\000\
\\001\000\020\000\093\000\000\000\
\\001\000\021\000\040\000\028\000\039\000\029\000\038\000\030\000\037\000\
\\031\000\036\000\032\000\035\000\040\000\005\000\041\000\034\000\000\000\
\\001\000\021\000\061\000\022\000\060\000\023\000\059\000\024\000\058\000\
\\025\000\057\000\026\000\056\000\027\000\055\000\033\000\087\000\
\\034\000\054\000\035\000\053\000\036\000\052\000\037\000\051\000\
\\038\000\050\000\039\000\049\000\000\000\
\\001\000\040\000\005\000\000\000\
\\001\000\041\000\066\000\000\000\
\\001\000\042\000\000\000\000\000\
\\001\000\042\000\102\000\000\000\
\\001\000\042\000\103\000\000\000\
\"
val actionRowNumbers =
"\000\000\068\000\002\000\001\000\
\\004\000\004\000\045\000\071\000\
\\006\000\044\000\050\000\009\000\
\\008\000\010\000\060\000\050\000\
\\056\000\066\000\066\000\066\000\
\\068\000\005\000\068\000\043\000\
\\066\000\049\000\072\000\023\000\
\\028\000\026\000\022\000\064\000\
\\035\000\066\000\066\000\066\000\
\\025\000\024\000\069\000\061\000\
\\037\000\038\000\008\000\039\000\
\\012\000\011\000\040\000\066\000\
\\066\000\066\000\066\000\066\000\
\\066\000\066\000\066\000\066\000\
\\066\000\066\000\066\000\066\000\
\\046\000\067\000\021\000\020\000\
\\036\000\047\000\053\000\052\000\
\\007\000\003\000\051\000\034\000\
\\033\000\032\000\031\000\030\000\
\\029\000\018\000\019\000\017\000\
\\016\000\015\000\014\000\013\000\
\\050\000\027\000\050\000\057\000\
\\058\000\065\000\062\000\041\000\
\\048\000\055\000\050\000\059\000\
\\063\000\042\000\054\000\070\000"
val gotoT =
"\
\\001\000\099\000\000\000\
\\015\000\002\000\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\003\000\006\000\004\000\005\000\000\000\
\\003\000\009\000\004\000\005\000\000\000\
\\000\000\
\\000\000\
\\006\000\013\000\014\000\012\000\015\000\011\000\000\000\
\\000\000\
\\008\000\016\000\010\000\015\000\014\000\014\000\015\000\011\000\000\000\
\\000\000\
\\007\000\021\000\000\000\
\\000\000\
\\000\000\
\\008\000\025\000\010\000\015\000\014\000\014\000\015\000\011\000\000\000\
\\000\000\
\\011\000\031\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\039\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\040\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\014\000\041\000\015\000\011\000\000\000\
\\000\000\
\\014\000\042\000\015\000\011\000\000\000\
\\005\000\043\000\000\000\
\\011\000\046\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\061\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\062\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\063\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\071\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\072\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\073\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\074\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\075\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\076\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\077\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\078\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\079\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\080\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\081\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\082\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\011\000\083\000\012\000\030\000\013\000\029\000\014\000\028\000\
\\015\000\011\000\016\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\087\000\010\000\015\000\014\000\014\000\015\000\011\000\000\000\
\\000\000\
\\008\000\088\000\010\000\015\000\014\000\014\000\015\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\095\000\010\000\015\000\014\000\014\000\015\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 100
val numrules = 44
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INTEGER of unit ->  (int) | NAME of unit ->  (string)
 | numeralNT of unit ->  (int) | identifierNT of unit ->  (string)
 | variableNT of unit ->  (string)
 | comparisonNT of unit ->  (comparison)
 | FactorNT of unit ->  (factor)
 | expressionNT of unit ->  (expression)
 | commandNT of unit ->  (command)
 | commandseqNT of unit ->  (command list)
 | commandlistNT of unit ->  (command list)
 | commavariablelistNT of unit ->  (string list)
 | variablelistNT of unit ->  (string list) | typeNT of unit ->  (int)
 | declarationNT of unit ->  (declaration)
 | declarationseqNT of unit ->  (declaration list)
 | blockNT of unit ->  (block) | programNT of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 0) => true | (T 1) => true | (T 2) => true | (T 3) => true | (T 
4) => true | (T 5) => true | (T 6) => true | (T 7) => true | (T 8)
 => true | (T 9) => true | (T 10) => true | (T 11) => true | (T 12)
 => true | (T 13) => true | (T 14) => true | (T 15) => true | (T 16)
 => true | (T 17) => true | (T 18) => true | (T 19) => true | (T 31)
 => true | (T 32) => true | (T 22) => true | (T 26) => true | (T 27)
 => true | (T 28) => true | (T 30) => true | (T 33) => true | (T 34)
 => true | (T 37) => true | (T 35) => true | (T 36) => true | (T 38)
 => true | (T 20) => true | (T 21) => true | (T 23) => true | (T 24)
 => true | (T 25) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "PROGRAM"
  | (T 1) => "DOUBLECOLON"
  | (T 2) => "VARIABLE"
  | (T 3) => "COLON"
  | (T 4) => "SEMICOLON"
  | (T 5) => "INTLABEL"
  | (T 6) => "BOOLLABEL"
  | (T 7) => "COMMA"
  | (T 8) => "LCURL"
  | (T 9) => "RCURL"
  | (T 10) => "ASSIGN"
  | (T 11) => "READ"
  | (T 12) => "WRITE"
  | (T 13) => "IF"
  | (T 14) => "THEN"
  | (T 15) => "ELSE"
  | (T 16) => "ENDIF"
  | (T 17) => "WHILE"
  | (T 18) => "DO"
  | (T 19) => "ENDWH"
  | (T 20) => "PLUS"
  | (T 21) => "MINUS"
  | (T 22) => "OR"
  | (T 23) => "MULTIPLY"
  | (T 24) => "DIVIDE"
  | (T 25) => "MOD"
  | (T 26) => "AND"
  | (T 27) => "TRUE"
  | (T 28) => "FALSE"
  | (T 29) => "NEG"
  | (T 30) => "NOT"
  | (T 31) => "LPAREN"
  | (T 32) => "RPAREN"
  | (T 33) => "LT"
  | (T 34) => "LEQ"
  | (T 35) => "GT"
  | (T 36) => "GEQ"
  | (T 37) => "EQ"
  | (T 38) => "NEQ"
  | (T 39) => "NAME"
  | (T 40) => "INTEGER"
  | (T 41) => "EOF"
  | (T 42) => "ILLCH"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.blockNT blockNT1, _, blockNT1right)) :: _
 :: ( _, ( MlyValue.identifierNT identifierNT1, _, _)) :: ( _, ( _, 
PROGRAM1left, _)) :: rest671)) => let val  result = MlyValue.programNT
 (fn _ => let val  (identifierNT as identifierNT1) = identifierNT1 ()
 val  (blockNT as blockNT1) = blockNT1 ()
 in (prog(identifierNT,blockNT))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, blockNT1right), rest671)

end
|  ( 1, ( ( _, ( _, _, RCURL1right)) :: ( _, ( MlyValue.commandlistNT 
commandlistNT1, _, _)) :: _ :: ( _, ( MlyValue.declarationseqNT 
declarationseqNT1, declarationseqNT1left, _)) :: rest671)) => let val 
 result = MlyValue.blockNT (fn _ => let val  (declarationseqNT as 
declarationseqNT1) = declarationseqNT1 ()
 val  (commandlistNT as commandlistNT1) = commandlistNT1 ()
 in (block(declarationseqNT,commandlistNT))
end)
 in ( LrTable.NT 1, ( result, declarationseqNT1left, RCURL1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.declarationseqNT declarationseqNT1, _, 
declarationseqNT1right)) :: ( _, ( MlyValue.declarationNT 
declarationNT1, declarationNT1left, _)) :: rest671)) => let val  
result = MlyValue.declarationseqNT (fn _ => let val  (declarationNT
 as declarationNT1) = declarationNT1 ()
 val  (declarationseqNT as declarationseqNT1) = declarationseqNT1 ()
 in (declarationNT::declarationseqNT)
end)
 in ( LrTable.NT 2, ( result, declarationNT1left, 
declarationseqNT1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.declarationseqNT (fn
 _ => ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.typeNT 
typeNT1, _, _)) :: _ :: ( _, ( MlyValue.variablelistNT variablelistNT1
, _, _)) :: ( _, ( _, VARIABLE1left, _)) :: rest671)) => let val  
result = MlyValue.declarationNT (fn _ => let val  (variablelistNT as 
variablelistNT1) = variablelistNT1 ()
 val  (typeNT as typeNT1) = typeNT1 ()
 in (
 typewriter(variablelistNT , typeNT , typetable) ;declaration(variablelistNT , typeNT) 
)
end)
 in ( LrTable.NT 3, ( result, VARIABLE1left, SEMICOLON1right), rest671
)
end
|  ( 5, ( ( _, ( _, INTLABEL1left, INTLABEL1right)) :: rest671)) =>
 let val  result = MlyValue.typeNT (fn _ => (1))
 in ( LrTable.NT 4, ( result, INTLABEL1left, INTLABEL1right), rest671)

end
|  ( 6, ( ( _, ( _, BOOLLABEL1left, BOOLLABEL1right)) :: rest671)) =>
 let val  result = MlyValue.typeNT (fn _ => (0))
 in ( LrTable.NT 4, ( result, BOOLLABEL1left, BOOLLABEL1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.commavariablelistNT commavariablelistNT1, _,
 commavariablelistNT1right)) :: ( _, ( MlyValue.variableNT variableNT1
, variableNT1left, _)) :: rest671)) => let val  result = 
MlyValue.variablelistNT (fn _ => let val  (variableNT as variableNT1)
 = variableNT1 ()
 val  (commavariablelistNT as commavariablelistNT1) = 
commavariablelistNT1 ()
 in (variableNT :: commavariablelistNT )
end)
 in ( LrTable.NT 5, ( result, variableNT1left, 
commavariablelistNT1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.variablelistNT (fn _
 => ([]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( MlyValue.commavariablelistNT commavariablelistNT1, _,
 commavariablelistNT1right)) :: ( _, ( MlyValue.variableNT variableNT1
, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result
 = MlyValue.commavariablelistNT (fn _ => let val  (variableNT as 
variableNT1) = variableNT1 ()
 val  (commavariablelistNT as commavariablelistNT1) = 
commavariablelistNT1 ()
 in (variableNT :: commavariablelistNT)
end)
 in ( LrTable.NT 6, ( result, COMMA1left, commavariablelistNT1right), 
rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.commavariablelistNT
 (fn _ => ([]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.commandlistNT commandlistNT1, _, 
commandlistNT1right)) :: ( _, ( MlyValue.commandNT commandNT1, 
commandNT1left, _)) :: rest671)) => let val  result = 
MlyValue.commandlistNT (fn _ => let val  (commandNT as commandNT1) = 
commandNT1 ()
 val  (commandlistNT as commandlistNT1) = commandlistNT1 ()
 in (commandNT :: commandlistNT)
end)
 in ( LrTable.NT 7, ( result, commandNT1left, commandlistNT1right), 
rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.commandlistNT (fn _
 => ([]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.expressionNT expressionNT1, _, _)) :: _ :: ( _, ( 
MlyValue.variableNT variableNT1, variableNT1left, _)) :: rest671)) =>
 let val  result = MlyValue.commandNT (fn _ => let val  (variableNT
 as variableNT1) = variableNT1 ()
 val  (expressionNT as expressionNT1) = expressionNT1 ()
 in (
if( (HashTable.lookup typetable (variableNT) ) = expressiontype(expressionNT)   )then SET (variableNT,expressionNT) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 9, ( result, variableNT1left, SEMICOLON1right), 
rest671)
end
|  ( 14, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.variableNT variableNT1, _, _)) :: ( _, ( _, READ1left, _)) ::
 rest671)) => let val  result = MlyValue.commandNT (fn _ => let val  (
variableNT as variableNT1) = variableNT1 ()
 in (READ(variableNT))
end)
 in ( LrTable.NT 9, ( result, READ1left, SEMICOLON1right), rest671)

end
|  ( 15, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.expressionNT expressionNT1, _, _)) :: ( _, ( _, WRITE1left, _
)) :: rest671)) => let val  result = MlyValue.commandNT (fn _ => let
 val  (expressionNT as expressionNT1) = expressionNT1 ()
 in (WRITE(expressionNT))
end)
 in ( LrTable.NT 9, ( result, WRITE1left, SEMICOLON1right), rest671)

end
|  ( 16, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.commandlistNT commandlistNT2, _, _)) :: _ :: _ :: _ :: ( _, (
 MlyValue.commandlistNT commandlistNT1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.expressionNT expressionNT1, _, _)) :: ( _, ( _, IF1left, _))
 :: rest671)) => let val  result = MlyValue.commandNT (fn _ => let
 val  (expressionNT as expressionNT1) = expressionNT1 ()
 val  commandlistNT1 = commandlistNT1 ()
 val  commandlistNT2 = commandlistNT2 ()
 in (
if(expressiontype(expressionNT)=1) then raise Fail("Type mismatch occured") else ite(expressionNT,commandlistNT1,commandlistNT2) 
)
end)
 in ( LrTable.NT 9, ( result, IF1left, SEMICOLON1right), rest671)
end
|  ( 17, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.commandlistNT commandlistNT1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.expressionNT expressionNT1, _, _)) :: ( _, ( _, WHILE1left, _
)) :: rest671)) => let val  result = MlyValue.commandNT (fn _ => let
 val  expressionNT1 = expressionNT1 ()
 val  (commandlistNT as commandlistNT1) = commandlistNT1 ()
 in (whiledo(expressionNT1,commandlistNT))
end)
 in ( LrTable.NT 9, ( result, WHILE1left, SEMICOLON1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else PLUS(expressionNT1,expressionNT2,1)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 19, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else MINUS(expressionNT1,expressionNT2,1)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 20, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=1 orelse expressiontype(expressionNT2)=1) then raise Fail("Type mismatch occured") else OR(expressionNT1,expressionNT2,0)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 21, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else MULTIPLY(expressionNT1,expressionNT2,1)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 22, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else DIVIDE(expressionNT1,expressionNT2,1)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 23, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=1 orelse expressiontype(expressionNT2)=1) then raise Fail("Type mismatch occured") else AND(expressionNT1,expressionNT2,0)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 24, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.expressionNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if (expressiontype(expressionNT1)=0 orelse expressiontype(expressionNT2)=0) then raise Fail("Type mismatch occured") else MOD(expressionNT1,expressionNT2,1)
)
end)
 in ( LrTable.NT 10, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 25, ( ( _, ( MlyValue.expressionNT expressionNT1, _, 
expressionNT1right)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let
 val  result = MlyValue.expressionNT (fn _ => let val  (expressionNT
 as expressionNT1) = expressionNT1 ()
 in (
if(expressiontype(expressionNT)=0) then raise Fail("Type mismatch occured") else NEG(expressionNT,1)
)
end)
 in ( LrTable.NT 10, ( result, NEG1left, expressionNT1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.expressionNT expressionNT1, _, 
expressionNT1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let
 val  result = MlyValue.expressionNT (fn _ => let val  (expressionNT
 as expressionNT1) = expressionNT1 ()
 in (
if(expressiontype(expressionNT)=1) then raise Fail("Type mismatch occured") else NOT(expressionNT,0)
)
end)
 in ( LrTable.NT 10, ( result, NOT1left, expressionNT1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.FactorNT FactorNT1, FactorNT1left, 
FactorNT1right)) :: rest671)) => let val  result = 
MlyValue.expressionNT (fn _ => let val  (FactorNT as FactorNT1) = 
FactorNT1 ()
 in (FactorToExpr(FactorNT,factortype(FactorNT)))
end)
 in ( LrTable.NT 10, ( result, FactorNT1left, FactorNT1right), rest671
)
end
|  ( 28, ( ( _, ( MlyValue.numeralNT numeralNT1, numeralNT1left, 
numeralNT1right)) :: rest671)) => let val  result = MlyValue.FactorNT
 (fn _ => let val  (numeralNT as numeralNT1) = numeralNT1 ()
 in (NUMERALVAL(numeralNT))
end)
 in ( LrTable.NT 11, ( result, numeralNT1left, numeralNT1right), 
rest671)
end
|  ( 29, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.FactorNT (fn _ => (BOOLVAL true ))
 in ( LrTable.NT 11, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 30, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.FactorNT (fn _ => (BOOLVAL false))
 in ( LrTable.NT 11, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.comparisonNT comparisonNT1, 
comparisonNT1left, comparisonNT1right)) :: rest671)) => let val  
result = MlyValue.FactorNT (fn _ => let val  (comparisonNT as 
comparisonNT1) = comparisonNT1 ()
 in (COMPVAL comparisonNT)
end)
 in ( LrTable.NT 11, ( result, comparisonNT1left, comparisonNT1right),
 rest671)
end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expressionNT
 expressionNT1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) =>
 let val  result = MlyValue.FactorNT (fn _ => let val  (expressionNT
 as expressionNT1) = expressionNT1 ()
 in (ExpressionToFactor(expressionNT,expressiontype(expressionNT)))

end)
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.variableNT variableNT1, variableNT1left, 
variableNT1right)) :: rest671)) => let val  result = MlyValue.FactorNT
 (fn _ => let val  (variableNT as variableNT1) = variableNT1 ()
 in (
VariableToFactor(variableNT,HashTable.lookup typetable (variableNT)))

end)
 in ( LrTable.NT 11, ( result, variableNT1left, variableNT1right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.comparisonNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then LT (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 12, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 35, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.comparisonNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then LEQ (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 12, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 36, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.comparisonNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then GT  (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 12, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 37, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.comparisonNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then GEQ (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 12, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 38, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.comparisonNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then EQ  (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 12, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 39, ( ( _, ( MlyValue.expressionNT expressionNT2, _, 
expressionNT2right)) :: _ :: ( _, ( MlyValue.expressionNT 
expressionNT1, expressionNT1left, _)) :: rest671)) => let val  result
 = MlyValue.comparisonNT (fn _ => let val  expressionNT1 = 
expressionNT1 ()
 val  expressionNT2 = expressionNT2 ()
 in (
if(expressiontype(expressionNT1)=expressiontype(expressionNT2)) then conNEQ  (expressionNT1,expressionNT2) else raise Fail("Type mismatch occured")
)
end)
 in ( LrTable.NT 12, ( result, expressionNT1left, expressionNT2right),
 rest671)
end
|  ( 40, ( ( _, ( MlyValue.identifierNT identifierNT1, 
identifierNT1left, identifierNT1right)) :: rest671)) => let val  
result = MlyValue.variableNT (fn _ => let val  (identifierNT as 
identifierNT1) = identifierNT1 ()
 in (identifierNT)
end)
 in ( LrTable.NT 13, ( result, identifierNT1left, identifierNT1right),
 rest671)
end
|  ( 41, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.identifierNT (fn _ => let val 
 (NAME as NAME1) = NAME1 ()
 in (NAME)
end)
 in ( LrTable.NT 14, ( result, NAME1left, NAME1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.INTEGER INTEGER1, INTEGER1left, 
INTEGER1right)) :: rest671)) => let val  result = MlyValue.numeralNT
 (fn _ => let val  (INTEGER as INTEGER1) = INTEGER1 ()
 in (INTEGER)
end)
 in ( LrTable.NT 15, ( result, INTEGER1left, INTEGER1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.INTEGER INTEGER1, _, INTEGER1right)) :: ( _
, ( _, PLUS1left, _)) :: rest671)) => let val  result = 
MlyValue.numeralNT (fn _ => let val  (INTEGER as INTEGER1) = INTEGER1
 ()
 in ((INTEGER))
end)
 in ( LrTable.NT 15, ( result, PLUS1left, INTEGER1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.programNT x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : while_ast_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun VARIABLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun INTLABEL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLLABEL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTIPLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun INTEGER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.INTEGER (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end

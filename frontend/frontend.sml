structure SMLLrVals =
  SMLLrValsFun(structure Token = LrParser.Token)

structure SMLLex = 
  SMLLexFun(structure Tokens = SMLLrVals.Tokens)

structure SMLParser =
  Join(structure ParserData = SMLLrVals.ParserData
       structure Lex=SMLLex
       structure LrParser=LrParser)

structure Frontend = struct

  fun printError (s,i:int * int,_) = TextIO.output(TextIO.stdOut,
    "Error, line " ^ (Int.toString (#1 i)) ^ ", " ^ s ^ "\n");

  val lex = SMLParser.makeLexer

  fun readFile s = 
  let
    val fd = TextIO.openIn s
  in
    fn _ : int => (fn (SOME s) => s | NONE => "") (TextIO.inputLine fd)
  end

  fun parse file = #1 (SMLParser.parse (0,lex (readFile file),printError,()))


end

(*val test = fn _ : int => "1+22+333;";*)

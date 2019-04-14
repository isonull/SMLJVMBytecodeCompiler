structure Tokens = Tokens
open Pos

type pos = Pos.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val pos = ref (0,0)
val eof = fn () => Tokens.EOP(!pos,!pos)

val error = fn (e,l : int,_) => print "some error"

fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))

%%

%header (functor SMLLexFun(structure Tokens : SML_TOKENS));

ws    = [\ \t];
digit = [0-9];
hexdigit = [0-9a-fA-F];
letter = [A-Za-z];
num = {digit}+;
hex = {hexdigit}+;
diglet = {digit}|{letter};

int = ~?{num};
word = (0w{num})|(0w{hex});
float = (~?{num}\.{num})|(~?{num}(\.{num})?e~?{num});
char = #\"(.)\";
string = \".*\";

symbol = [!%&$#+-/:<=>?@\~'^|*];
comment = \(\*.*\*\);

id = ({letter}({diglet}|_|')*)|({symbol}+);
iddot = {id}\.;
eqvar = ''({diglet}|_)({letter}|{digit}|'|_)*;
var = '({diglet}|_)({letter}|{digit}|'|_)*;
lid = {id}(\.{id})*;

%%

{comment}   => (lex());
{ws}        => (pos := letterMove (!pos) 1; lex());
\n          => (pos := lineMove (!pos) 1; lex());
"("         => (Tokens.LRBR (!pos,!pos));
")"         => (Tokens.RRBR (!pos,!pos));
"{"         => (Tokens.LCBR (!pos,!pos));
"}"         => (Tokens.RCBR (!pos,!pos));
"["         => (Tokens.LSBR (!pos,!pos));
"]"         => (Tokens.RSBR (!pos,!pos));
"#"         => (Tokens.NS (!pos,!pos));
"="         => (Tokens.EQ (!pos,!pos));
"|"         => (Tokens.PIPE (!pos,!pos));
","         => (Tokens.COMMA (!pos,!pos));
":"         => (Tokens.COLON (!pos,!pos));
";"         => (Tokens.SEMI (!pos,!pos));
"->"        => (Tokens.RAR (!pos,!pos));
"=>"        => (Tokens.RARR (!pos,!pos));
"*"         => (Tokens.STAR (!pos,!pos));
"."         => (Tokens.DOT (!pos,!pos));
"_"         => (Tokens.WILD (!pos,!pos));
"..."       => (Tokens.RWILD (!pos,!pos));
"raise"     => (Tokens.RAISE (!pos,!pos));
"handle"    => (Tokens.HANDLE (!pos,!pos));
"if"        => (Tokens.IF (!pos,!pos));
"else"      => (Tokens.ELSE (!pos,!pos));
"then"      => (Tokens.THEN (!pos,!pos));
"andalso"   => (Tokens.ANDALSO (!pos,!pos));
"orelse"    => (Tokens.ORELSE (!pos,!pos));
"while"     => (Tokens.WHILE (!pos,!pos));
"do"        => (Tokens.DO (!pos,!pos));
"case"      => (Tokens.CASE (!pos,!pos));
"of"        => (Tokens.OF (!pos,!pos));
"fn"        => (Tokens.FN (!pos,!pos));
"rec"       => (Tokens.REC (!pos,!pos));
"val"       => (Tokens.VAL (!pos,!pos));
"fun"       => (Tokens.FUN (!pos,!pos));
"type"      => (Tokens.TYPE (!pos,!pos));
"datatype"  => (Tokens.DATATYPE (!pos,!pos));
"abstype"   => (Tokens.ABSTYPE (!pos,!pos));
"exception" => (Tokens.EXCEPTION (!pos,!pos));
"local"     => (Tokens.LOCAL (!pos,!pos));
"let"       => (Tokens.LET (!pos,!pos));
"in"        => (Tokens.IN (!pos,!pos));
"end"       => (Tokens.END (!pos,!pos));
"open"      => (Tokens.OPEN (!pos,!pos));
"with"      => (Tokens.WITH (!pos,!pos));
"withtype"  => (Tokens.WITHTYPE (!pos,!pos));
"as"        => (Tokens.AS (!pos,!pos));
"and"       => (Tokens.AND (!pos,!pos));
"infix"     => (Tokens.INFIX (!pos,!pos));
"infixr"    => (Tokens.INFIXR (!pos,!pos));
"nonfix"    => (Tokens.NONFIX (!pos,!pos));

{id}     => (Tokens.ID (yytext,!pos,!pos));
{eqvar}  => (Tokens.TYVAR ((LexUtils.eqvarLex yytext, true),!pos,!pos));
{var}    => (Tokens.TYVAR ((LexUtils.varLex yytext, false),!pos,!pos));
{iddot}  => (Tokens.IDPRE (LexUtils.iddotLex yytext,!pos,!pos));

{int}    => (Tokens.INTSCON  (LexUtils.intLex yytext,!pos,!pos));
{string} => (Tokens.STRSCON  ("",!pos,!pos));
{char}   => (Tokens.CHARSCON ("",!pos,!pos));
{float}   => (Tokens.REALSCON ("",!pos,!pos));
{word}   => (Tokens.WORDSCON ("",!pos,!pos));

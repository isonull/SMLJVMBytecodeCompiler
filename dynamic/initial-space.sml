structure InitialSpace = struct

  structure VS = ValueSpace
  structure S = Space

  datatype value = datatype Value.value

  val matchLoc = (~1, 0)
  val bindLoc = (~1, 1)

  val basNameValueListPair = [
    ("iadd"   , VAL (~1,96)),
    ("isub"   , VAL (~1,100)),
    ("imul"   , VAL (~1,104)),
    ("idiv"   , VAL (~1,108)),
    ("irem"   , VAL (~1,112)),
    ("ieq"    , VAL (~1,159)),
    ("ine"    , VAL (~1,160)),
    ("ilt"    , VAL (~1,161)),
    ("ige"    , VAL (~1,162)),
    ("igt"    , VAL (~1,163)),
    ("ile"    , VAL (~1,164)),
    ("false"  , CON ((~1,0),0)),
    ("true"   , CON ((~1,1),1)),
    ("CON"    , CON ((~1,2),0)),
    ("NIL"    , CON ((~1,1),1)),
    ("Match"  , EXC ((~1,0),0)),
    ("Bind"   , EXC ((~1,1),1)),
    ("ref"    , VAL (~1,3)),
    ("dref"   , VAL (~1,4)),
    ("aref"   , VAL (~1,5)),
    ("eq"     , VAL (~1,6)),
    ("print"  , VAL (~1,7)),
    ("tostr"  , VAL (~1,8))
    ]

  val valspa = VS.fromListPair (basNameValueListPair)

  val space = S.fromValspa valspa

end

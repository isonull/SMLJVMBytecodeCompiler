structure ArrayType = struct

  datatype atype =
    A_BOOL   |
    A_CHAR   |
    A_FLOAT  |
    A_DOUBLE |
    A_BYTE   |
    A_SHORT  |
    A_INT    |
    A_LONG

  fun toInt A_BOOL   = 4
    | toInt A_CHAR   = 5
    | toInt A_FLOAT  = 6
    | toInt A_DOUBLE = 7
    | toInt A_BYTE   = 8
    | toInt A_SHORT  = 9
    | toInt A_INT    = 10
    | toInt A_LONG   = 11

  val toWord = Word.fromInt o toInt

end

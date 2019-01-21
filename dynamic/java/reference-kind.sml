structure ReferenceKind = struct 

  datatype refkind =
    REF_GETF   |
    REF_GETS   |
    REF_PUTF   |
    REF_PUTS   |
    REF_INVVI  |
    REF_INVST  |
    REF_INVSP  |
    REF_NINVSP |
    REF_INVINT

  fun toInt REF_GETF   = 1
    | toInt REF_GETS   = 2
    | toInt REF_PUTF   = 3
    | toInt REF_PUTS   = 4
    | toInt REF_INVVI  = 5
    | toInt REF_INVST  = 6
    | toInt REF_INVSP  = 7
    | toInt REF_NINVSP = 8
    | toInt REF_INVINT = 9

  val toWord = Word.fromInt o toInt

end

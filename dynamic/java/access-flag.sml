structure AccessList = struct

  fun comb [a1, a0] [b1, b0] = [Word.orb (a1,b1), Word.orb (a0, b0)]
    | comb _ _ = raise Size

  fun combs ls = List.foldl (fn (acc, accs) => comb acc accs) [0wx00, 0wx00] ls

end

structure ClassAccess = struct

  datatype classacc =
    ACC_PUBLIC     |
    ACC_FINAL      |
    ACC_SUPER      |
    ACC_INTERFACE  |
    ACC_ABSTRACT   |
    ACC_SYNTHETIC  |
    ACC_ANNOTATION |
    ACC_ENUM       |
    ACC_MODULE

  fun aux ACC_PUBLIC     = [0wx00, 0wx01]
    | aux ACC_FINAL      = [0wx00, 0wx10]
    | aux ACC_SUPER      = [0wx00, 0wx20]
    | aux ACC_INTERFACE  = [0wx02, 0wx00]
    | aux ACC_ABSTRACT   = [0wx04, 0wx00]
    | aux ACC_SYNTHETIC  = [0wx10, 0wx00]
    | aux ACC_ANNOTATION = [0wx20, 0wx00]
    | aux ACC_ENUM       = [0wx40, 0wx00]
    | aux ACC_MODULE     = [0wx80, 0wx00]

  fun toWords ls = AccessList.combs (map aux ls)

  val toBytes = Word8List.fromWordList o toWords

end

structure MethodAccess = struct

  datatype methodacc =
    ACC_PUBLIC       |
    ACC_PRIVATE      |
    ACC_PROTECTRD    |
    ACC_STATIC       |
    ACC_FINAL        |
    ACC_SYNCHRONIZED |
    ACC_BRIDGE       |
    ACC_VARARGS      |
    ACC_NATIVE       |
    ACC_ABSTRACT     |
    ACC_STRICT       |
    ACC_SYNTHETIC

  fun aux ACC_PUBLIC           = [0wx00, 0wx01]
    | aux ACC_PRIVATE          = [0wx00, 0wx02]
    | aux ACC_PROTECTRD        = [0wx00, 0wx04]
    | aux ACC_STATIC           = [0wx00, 0wx08]
    | aux ACC_FINAL            = [0wx00, 0wx10]
    | aux ACC_SYNCHRONIZED     = [0wx00, 0wx20]
    | aux ACC_BRIDGE           = [0wx00, 0wx40]
    | aux ACC_VARARGS          = [0wx00, 0wx80]
    | aux ACC_NATIVE           = [0wx01, 0wx00]
    | aux ACC_ABSTRACT         = [0wx04, 0wx00]
    | aux ACC_STRICT           = [0wx08, 0wx00]
    | aux ACC_SYNTHETIC        = [0wx10, 0wx00]

  fun toWords ls = AccessList.combs (map aux ls)

  val toBytes = Word8List.fromWordList o toWords

end

structure FieldAccess = struct

  datatype fieldacc =
    ACC_PUBLIC       |
    ACC_PRIVATE      |
    ACC_PROTECTRD    |
    ACC_STATIC       |
    ACC_FINAL        |
    ACC_VOLATILE     |
    ACC_TRANSIENT    |
    ACC_SYNTHETIC    |
    ACC_ENUM

  fun aux ACC_PUBLIC    = [0wx00, 0wx01]
    | aux ACC_PRIVATE   = [0wx00, 0wx02]
    | aux ACC_PROTECTRD = [0wx00, 0wx04]
    | aux ACC_STATIC    = [0wx00, 0wx08]
    | aux ACC_FINAL     = [0wx00, 0wx10]
    | aux ACC_VOLATILE  = [0wx00, 0wx40]
    | aux ACC_TRANSIENT = [0wx00, 0wx80]
    | aux ACC_SYNTHETIC = [0wx10, 0wx00]
    | aux ACC_ENUM      = [0wx40, 0wx00]

  fun toWords ls = AccessList.combs (map aux ls)

  val toBytes = Word8List.fromWordList o toWords

end


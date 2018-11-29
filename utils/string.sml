structure StringAux = struct
  (* these function just consider [a-z] *)

  val succMax = #"z"
  val succMin = #"a"
  fun succ s = let
    val frontStr = String.substring (s, 0, (size s) - 1)
    val lastChar = String.sub (s, (size s) - 1)
  in if succMax > lastChar
     then frontStr ^ (Char.toString (Char.succ lastChar))
     else s ^ (Char.toString succMin) end

end

structure StringKey : ORD_KEY = struct
  type ord_key = string
  val compare = String.compare
end

structure StringBinarySet = OrdSetAuxFn (BinarySetFn (StringKey))
structure StringBinaryMap = OrdMapAuxFn (BinaryMapFn (StringKey))

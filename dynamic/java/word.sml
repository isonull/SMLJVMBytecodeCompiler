structure Word8Aux = struct

  open Word8

  val fromWord = Word8.fromLarge o Word.toLarge

end

structure Word8List = struct

  fun fromWordList ws = map Word8Aux.fromWord ws

end

structure WordList = struct

  (* Big endian *)
  (* break int into list of words smaller than 256 *)
  fun fromIntInf n (i : IntInf.int) = let
    fun aux i 0 = []
      | aux i n = let
      val h = Int.fromLarge (IntInf.mod (i, 256))
      val t = IntInf.div (i, 256) in
      (Word.fromInt h) :: (aux t (n - 1)) end in
    if n < 0 then raise Size else
      if IntInf.>= (i, 0)
      then rev (aux i n)
      else (rev o map (fn w => Word.xorb (0wxff, w)))
           (aux (IntInf.~ (IntInf.+ (i, 1))) n) end

  fun fromInt n i = let
    fun aux i 0 = []
      | aux i n = let
      val h = i mod 256
      val t = i div 256 in
      (Word.fromInt h) :: (aux t (n - 1)) end in
    if n < 0 then raise Size else
      if i >= 0
      then rev (aux i n)
      else (rev o map (fn w => Word.xorb (0wxff, w)))
           ((aux (~ (i + 1))) n) end

end

structure Word8VectorAux = struct

  open Word8Vector

  fun fromWordList ls = Vector.fromList (List.map Word8.fromLargeWord ls)

  fun fromString s = Vector.fromList (List.map (Word8.fromInt o Char.ord) (String.explode s))


end

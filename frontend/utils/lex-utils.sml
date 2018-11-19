structure LexUtils = struct
  exception LexUtilException

  fun revfold _ nil b = b
    | revfold f (x::xs) b = revfold f xs (f(x,b))

  fun splitChars ((c :: cs) : char list) (a : char) =
    if c = a
    then [] :: (splitChars cs a)
    else (fn (a, (x :: xs)) => (a :: x) :: xs
           | _ => raise LexUtilException) (c, splitChars cs a)
  | splitChars [] a = [[]]

  fun splitString s a = map implode (splitChars (explode s) a)

  fun charDigitLex c = (ord c) - (ord #"0")

  fun digitLex s = charDigitLex (String.sub (s,0))

  fun numLex s = revfold (fn (a, b) => charDigitLex a + 10 * b) (explode s) 0

  (* TODO *)
  fun hexLex s = 0

  fun iddotLex s = substring (s, 0, (size s - 1))

  fun lidLex s = splitString s #"."

  fun varLex s = substring (s, 1, (size s) - 1)

  fun isNeg s = String.sub (s, 0) = #"~"

  fun intLex s =
    if isNeg s
    then ~ (numLex (String.substring (s, 1, (size s) - 1)))
    else numLex s

  fun wordLex s =
    Word.fromInt (
    if String.sub (s, 2) = #"x"
    then hexLex (substring (s, 3, (size s) - 3))
    else numLex (substring (s, 2, (size s) - 2))
    )

  fun floatLex s = let

    fun wholeToDecimal d =
      if d < 1.0
      then d
      else wholeToDecimal (d / 10.0)

    fun aux (s1 :: s2 :: nil) = let
      val exp10 = intLex s2
      fun calExp 0 = 1.0
        | calExp n = if n < 0
                     then (calExp (n + 1)) / 10.0
                     else (calExp (n - 1)) * 10.0
      in (aux [s1]) * (calExp exp10) end

      | aux [s1] = let
      val dw = map (Real.fromInt o intLex) (splitString s1 #".")
      fun filt (d :: w :: nil) = (d, w)
        | filt _ = raise LexUtilException
      val (d, w) = filt dw
      in if d > 0.0
         then d + (wholeToDecimal w)
         else d + ~ (wholeToDecimal w) end

      | aux _ = raise LexUtilException
  in
    aux (splitString s #"e")
  end

  fun charLex s = String.sub (s, 2)

  fun stringLex s = substring (s, 1, (size s) - 2)

end

structure LexUtils = struct
  exception LexUtilException

  fun revfold _ nil b = b
    | revfold f (x::xs) b = revfold f xs (f(x,b))

  fun splitChars (c :: cs) a =
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
    then ~ (numLex s)
    else numLex s

  fun wordLex s =
    Word.fromInt (
    if String.sub (s, 2) = #"x"
    then hexLex (substring (s, 3, (size s) - 3))
    else numLex (substring (s, 2, (size s) - 2))
    )

  fun floatLex s =
  let
    fun d2w d =
      if d < 1.0
      then d
      else d2w (d / 10.0)
    fun flex1 s =
    let
      val d :: w :: nil = map (Real.fromInt o numLex) (splitString s #".")
    in
      d + (d2w w)
    end
  in
    if String.isSubstring "e" s
    then 0.0(* TODO *)
    else flex1 s
  end

  fun charLex s = String.sub (s, 2)

  fun stringLex s = substring (s, 1, (size s) - 2)

end

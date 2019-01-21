structure ModifiedUtf8 = struct

  val & = Word.andb
  val >> = Word.>>

  infix &
  infix >>

  fun fromWord w =
    if w = 0wx0 then [0wxc0, 0wx80] else
    if w <= 0wx007f then [w] else
    if w <= 0wx07ff then let
      val a = ((w >> 0wx6) & 0wx001f) + 0wxc0
      val b = (w & 0wx003f) + 0wx80 in
      [a, b] end else 
    if w <= 0wxffff then let
      val a = ((w >> 0wxc) & 0wx000f) + 0wxe0
      val b = ((w >> 0wx6) & 0wx003f) + 0wx80
      val c = (w & 0wx003f) + 0wx80 in
      [a, b, c] end
    else raise Size

  val fromChar = fromWord o Word.fromInt o Char.ord

  val fromString = List.concat o (map fromChar) o String.explode

end

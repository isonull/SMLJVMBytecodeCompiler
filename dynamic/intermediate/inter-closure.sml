structure InterClosure = struct 

  structure IS = IntBinarySetAux
  structure IMAP = IntBinaryMapAux
  structure IM = InterMethod

  type method = InterMethod.method

  type closid = int

  datatype closure =
    TOP of IS.set * method |
    FCN of closid * IS.set * method

  val i2s = Int.toString

  fun c2s (locset, (method, _)) = let
    val s2 = IS.toString locset i2s ","
    val s3 = IM.toString method in
    s2 ^ "\n" ^ s3 end

  fun prevClosid (TOP (_, _)) = NONE
    | prevClosid (FCN (cid, _, _)) = SOME cid

  fun toString (TOP (b, c)) = "TOP" ^ " " ^ (c2s (b, c))
    | toString (FCN (a, b, c)) = "FCN" ^ " " ^ (i2s a) ^ " " ^ (c2s (b, c))

end

structure InterClosure = struct 

  structure IS = IntBinarySetAux
  structure IMAP = IntBinaryMapAux
  structure IM = InterMethod

  type method = InterMethod.method

  type closid = int

  datatype closure =
    TOP of int list * int * method |
    FCN of closid * int list* int * method

  val i2s = Int.toString

  fun c2s (nexts, locmax, method) = let
    val s2 = i2s locmax
    val s3 = IM.toString method in
    "Childrens: " ^ (ListAux.toString nexts i2s ",") ^ "\n" ^ 
    "Locmax: " ^ s2 ^ "\n" ^ 
    "Codes: \n" ^ s3 end

  fun getRefs clos = case clos of
          TOP (_, _, m) => IM.getRefs m 
        | FCN (_, _, _, m) => IM.getRefs m

  fun getMethod (TOP (_, _, m)) = m
    | getMethod (FCN (_, _, _, m)) = m
    
  fun getMaxloc (TOP (_, m, _)) = m
    | getMaxloc (FCN (_, _, m, _)) = m


  fun prevClosid (TOP (_, _, _)) = ~1
    | prevClosid (FCN (cid, _, _, _)) = cid

  fun nextClosid (TOP (n, _, _)) = n
    | nextClosid (FCN (_, n, _, _)) = n

  fun removeNext (TOP (n, a, b)) r = 
    TOP (ListAux.remove n r, a, b)
    | removeNext (FCN (a, n, b, c)) r = 
    FCN (a, ListAux.remove n r, b, c)

  fun updateMethod (TOP (n, a, m)) r = 
    TOP (n, a, r)
    | updateMethod (FCN (a, n, b, m)) r = 
    FCN (a, n, b, r)
    

  fun toString (TOP (z, b, c)) = "TOP" ^ " " ^ (c2s (z, b, c))
    | toString (FCN (a, z, b, c)) = "FCN" ^ "\n" ^ 
      "Parent: " ^ (i2s a) ^ "\n " ^ (c2s (z, b, c))

end

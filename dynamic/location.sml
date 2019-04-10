structure Location = struct

  fun toString (x, y) = "(" ^ (Int.toString x) ^ "," ^(Int.toString y) ^ ")"

end

structure LocationKey = struct

  type ord_key = (int * int)

  fun compare ((x1,x2), (y1,y2)) = let
    val o1 = Int.compare (x1, y1) in
    if o1 = EQUAL then Int.compare (x2, y2) else o1 end

end

structure LocationBinaryMap = struct 

  structure LBM =  OrdMapAuxFn (BinaryMapFn (LocationKey))
  open LBM

  val tostr = toString
  fun toString m = tostr m Location.toString Location.toString "-" "\n"

end

structure LocationBinarySet = struct 

  open LocationBinaryMap.KeySet 
  val tostr = toString
  fun toString m = tostr m Location.toString ","
  

end


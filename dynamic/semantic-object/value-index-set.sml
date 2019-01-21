structure ValueIndexSet = struct
  
  structure IS = IntBinarySetAux

  open IS

  fun new set = let
    val newid = IS.getExclusion set 0 (fn x => x + 1)
    val newset = IS.add (set, newid) in
    (newid, newset) end

end

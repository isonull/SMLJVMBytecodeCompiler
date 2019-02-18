structure InterProgram = struct

  structure IC = InterClosure
  structure IM = IntBinaryMapAux

  datatype closure = datatype IC.closure

  open IntBinaryMapAux

  (* closure graph for cross closure access *)
  type program = (closure map)
  val i2s = Int.toString

  val valOf = Option.valOf

  fun distance prog x y = (if x = y then 
    SOME 0 else let
      val cx = valOf (find (prog, x)) in
      SOME ((valOf (distance prog (IC.prevClosid cx) y)) + 1) end) 
    handle Option => NONE
         | NotFound => NONE

  fun path prog x y = (if x = y then
    SOME [x] else let
      val cx = valOf (find (prog, x))
      val xp = IC.prevClosid cx in
      SOME (x :: (valOf (path prog xp y))) end)
    handle Option => NONE
         | NotFound => NONE

  fun prev prog x = IC.prevClosid (valOf (find (prog, x)))

  (*fun add prog f id closinfo = let*)
    (*val newclos = IC.new f closinfo in*)
    (*insert (prog, id, newclos) end*)

  fun toString prog = IM.toString prog (fn k => i2s k) IC.toString "" 
    "\n --------------------------- \n"
end

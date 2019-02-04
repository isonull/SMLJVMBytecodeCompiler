structure InterProgram = struct

  structure IC = InterClosure
  structure IM = IntBinaryMapAux

  datatype closure = datatype IC.closure

  open IntBinaryMapAux

  type program = closure map
  val i2s = Int.toString

  val valOf = Option.valOf

  fun distance prog x y = (if x = y then 
    SOME 0 else let
      val cx = valOf (find (prog, x)) in
      SOME ((valOf (distance prog (valOf (IC.prevClosid cx)) y)) + 1) end) 
    handle Option => NONE
         | NotFound => NONE

  fun path prog x y = (if x = y then
    SOME [] else let
      val cx = valOf (find (prog, x)) in
      SOME (cx :: (valOf (path prog (valOf (IC.prevClosid cx)) y))) end)
    handle Option => NONE
         | NotFound => NONE

  (*fun add prog f id closinfo = let*)
    (*val newclos = IC.new f closinfo in*)
    (*insert (prog, id, newclos) end*)

  fun toString prog = IM.toString prog (fn k => i2s k) IC.toString "" "\n"
end

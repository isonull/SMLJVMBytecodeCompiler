structure State = struct

  structure V = Value
  structure M = Memory
  structure ES = ExceptionNameSet

  type state = M.mem * ES.exnameset

end

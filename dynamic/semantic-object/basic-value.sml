structure BasicValue = struct

  datatype basop = ADD | SUB | MUL | DIV | NEG
  datatype basty = I
  type basval = basty * basop

end

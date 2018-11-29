structure OptionAux = struct
  open Option

  fun toString (SOME opt) tostrfn empty = tostrfn opt
    | toString NONE tostrfn empty = empty
end

signature CONTEXT = sig

  type context

  val getValstr : context -> LongValueIdentifier.lvid -> ValueStructure.valstr
  val getTystr : context -> LongTypeConstructor.ltycon -> TypeStructure.tystr

  val insenvAugment : context -> InstantiationEnvironment.insenv -> context
  val valenvAugment : context -> ValueEnvironment.valenv -> context
  val strenvAugment : context -> StructureEnvironment.strenv -> context
  val tyenvAugment : context -> TypeEnvironment.tyenv -> context
  val envAugment : context -> Environment.env -> context

  val toString : context -> string

end

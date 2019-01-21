structure Address = struct

  type addr = int

  val compare = Int.compare

end

structure AddressKey = struct

  type ord_key = Address.addr

  val compare = Address.compare

end

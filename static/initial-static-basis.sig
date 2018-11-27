signature INITIAL_STATIC_BASIS = sig

  val unitTycon : Identifier.tycon
  val boolTycon : Identifier.tycon
  val realTycon : Identifier.tycon
  val strTycon  : Identifier.tycon
  val charTycon : Identifier.tycon
  val wordTycon : Identifier.tycon
  val listTycon : Identifier.tycon
  val refTycon  : Identifier.tycon
  val exnTycon  : Identifier.tycon

  val unitTyname : Type.tyname
  val intTyname  : Type.tyname
  val boolTyname : Type.tyname
  val realTyname : Type.tyname
  val strTyname  : Type.tyname
  val charTyname : Type.tyname
  val wordTyname : Type.tyname
  val listTyname : Type.tyname
  val refTyname  : Type.tyname
  val exnTyname  : Type.tyname

  val unitTysch : Type.tysch
  val intTysch  : Type.tysch
  val boolTysch : Type.tysch
  val realTysch : Type.tysch
  val strTysch  : Type.tysch
  val charTysch : Type.tysch
  val wordTysch : Type.tysch
  val listTysch : Type.tysch
  val refTysch  : Type.tysch
  val exnTysch  : Type.tysch

  val unitTyfcn : Type.tyfcn
  val intTyfcn  : Type.tyfcn
  val boolTyfcn : Type.tyfcn
  val realTyfcn : Type.tyfcn
  val strTyfcn  : Type.tyfcn
  val charTyfcn : Type.tyfcn
  val wordTyfcn : Type.tyfcn
  val listTyfcn : Type.tyfcn
  val refTyfcn  : Type.tyfcn
  val exnTyfcn  : Type.tyfcn

  val unitValenv : StaticBasis.valenv
  val intValenv  : StaticBasis.valenv
  val boolValenv : StaticBasis.valenv
  val realValenv : StaticBasis.valenv
  val strValenv  : StaticBasis.valenv
  val charValenv : StaticBasis.valenv
  val wordValenv : StaticBasis.valenv
  val listValenv : StaticBasis.valenv
  val refValenv  : StaticBasis.valenv
  val exnValenv  : StaticBasis.valenv

  val valenv    : StaticBasis.valenv
  val tyenv     : StaticBasis.tyenv
  val strenv    : StaticBasis.strenv
  val env       : StaticBasis.env
  val tynameset : Type.tynameset
  val context   : StaticBasis.context

end

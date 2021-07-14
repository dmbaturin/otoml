include Common

module DefaultImpl = Otoml_base.Make (Default_impl.NativeInteger) (Default_impl.NativeFloat) (Default_impl.SimpleDate)

include DefaultImpl


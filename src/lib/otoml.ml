include Common

module Base = struct
  include Impl_sigs
  include Default_impl

  include Otoml_base
end

module DefaultImpl = Base.Make (Base.OCamlInteger) (Base.OCamlFloat) (Base.StringDate)

include DefaultImpl

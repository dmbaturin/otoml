include Common

module Base = struct
  include Common
  include Impl_sigs
  include Default_impl

  include Otoml_base
end

module DefaultImpl = Base.Make (Base.OCamlNumber) (Base.StringDate)

include DefaultImpl

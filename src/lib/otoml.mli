(** Default TOML implementation. *)

include Impl_sigs.TomlImplementation
  with type toml_integer = int
  and type toml_float = float
  and type toml_date = string

module Base : sig
  module type TomlInteger = sig
    include Impl_sigs.TomlInteger
  end

  module type TomlFloat = sig
    include Impl_sigs.TomlFloat
  end

  module type TomlDate = sig
    include Impl_sigs.TomlDate
  end

  module type TomlImplementation = sig
    include Impl_sigs.TomlImplementation
  end

  module OCamlInteger : TomlInteger with type t = Int.t
  module OCamlFloat : TomlFloat with type t = Float.t
  module StringDate : TomlDate with type t = string

  module Make (I : TomlInteger) (F : TomlFloat) (D : TomlDate) : 
    TomlImplementation with type toml_integer = I.t and type toml_float = F.t and type toml_date = D.t
end

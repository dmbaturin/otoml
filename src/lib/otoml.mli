(** Default TOML implementation. *)

include Impl_sigs.TomlImplementation
  with type toml_integer = int
  and type toml_float = float
  and type toml_date = string

module Base : sig
  module type TomlNumber = sig
    include Impl_sigs.TomlNumber
  end

  module type TomlDate = sig
    include Impl_sigs.TomlDate
  end

  module type TomlImplementation = sig
    include Impl_sigs.TomlImplementation
  end

  module OCamlNumber : TomlNumber
    with type int = Int.t
    and type float = Float.t

  module StringDate : TomlDate with type t = string

  module Make (N : TomlNumber) (D : TomlDate) : TomlImplementation
    with type toml_integer = N.int
    and type toml_float = N.float
    and type toml_date = D.t
end

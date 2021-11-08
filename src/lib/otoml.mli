(** OTOML is a TOML parsing, manipulation, and pretty-printing library.

    It's fully compliant with the TOML 1.0.0 specification
 *)

(** {1 Default TOML implementation}

  The default implementation is meant to cover the majority of use cases without using
  any dependencies outside of the OCaml's standard library.

  Numeric values are represented as native 31/63-bit integers and floats.

  Dates are represented as strings, with only superficial validation. For example,
  a completely implausible date like 1993-09-935 is rejected by the parser,
  but 1993-02-29 is accepted despite the fact that 1993 wasn't a leap year.

  If your use case requires big numbers or full-fledged datetime support,
  you can build a custom implementation using the functorial interface ({!Base.Make}).

 *)

include Impl_sigs.TomlImplementation
  with type toml_integer = int
  and type toml_float = float
  and type toml_date = string

(** {1 The functorial interface }

  OTOML provides a way to build your own TOML implementation by plugging your own modules
  for working with numbers and dates into the functor.
 *)

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

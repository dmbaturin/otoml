(** OTOML is a TOML parsing, manipulation, and pretty-printing library.

    - Fully compliant with the TOML 1.0.0 specification
    - Makes it easy to look up and modify deeply nested table fields
    - Provides user-friendly syntax error reporting
    - Avoids external dependencies, but provides a way for everyone to bring their own

    Usage example:

    {[
      (* Parsing a TOML document with syntax errors. *)
      utop # let t = Otoml.Parser.from_string_result "foo.bar.baz = " ;;
      val t : (Otoml.t, string) result = Error
        "Syntax error on line 1, character 15: Malformed key-value pair (missing value?)"

      (* Parsing a valid document. *)
      utop# let t = Otoml.Parser.from_string "foo.bar.baz = 42" ;;
      val t : Otoml.t = Otoml.TomlTable [("foo",
        Otoml.TomlTable [("bar",
          Otoml.TomlTable [("baz", Otoml.TomlInteger 42)])])]

      (* Retrieving a nested field, exact type match is expected by detault. *)
      utop # Otoml.find_result t (Otoml.get_string) ["foo"; "bar"; "baz"] ;;
      - : (string, string) result =
      Error "Unexpected TOML value type at key foo.bar.baz: value must be a string, found integer"

      (* Retrieving a field in non-strict mode (automatic type conversion). *)
      utop # Otoml.find_result t (Otoml.get_string ~strict:false) ["foo"; "bar"; "baz"] ;;
      - : (string, string) result = Ok "42"

      (* Updating a field. *)
      utop # Otoml.update_result t ["foo"; "bar"; "baz"] (Some (Otoml.string "quux")) ;;
      - : (Otoml.t, string) result = Ok
       (Otoml.TomlTable [("foo",
         Otoml.TomlTable [("bar", Otoml.TomlTable [("baz", Otoml.TomlString "quux")])])])

      (* Deleting a field. *)
      utop # Otoml.update_result t ["foo"; "bar"; "baz"] None ;;
      - : (Otoml.t, string) result =
      Ok (Otoml.TomlTable [("foo", Otoml.TomlTable [("bar", Otoml.TomlTable [])])])

     ]}
 *)

(** {1 Default TOML implementation}

  The default implementation is meant to cover the majority of use cases without using
  any dependencies outside of the OCaml standard library.

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

# OTOML

![maintenance-status](https://img.shields.io/badge/maintenance-actively--developed-brightgreen.svg)
![CI](https://github.com/dmbaturin/otoml/actions/workflows/main.yml/badge.svg)

A TOML parsing and manipulation library for OCaml.

In short:

* TOML 1.0-compliant.
* Transparent (no abstract types).
* Uses immutable data structures.
* Easy access to deeply nested values.
* Preserves the order of fields in tables (even though the spec doesn't require it).
* Preserves the original syntax variant (e.g. inline vs normal table) when parsing and printing.
* Flexible pretty-printing options.
* Does not force a calendar or bignum library dependency on you (you can plug your own into the functor).

## Goals

The main goal for writing another TOML library is to provide a library for _manipulating_ TOML files, not just _reading_ them.

TOML is designed as a configuration file format.
It's not just a serialization format for machines to talk to one another.
A lot of time it's written, edited, and read by humans.

That is why TOML supports comments and multiple ways to write the same data. 

Ideally, when a program reads a TOML file and writes it back, it should be able to echo it back and respect
user's choice of using inline records vs sections (i.e. `section = {...}` vs `[section]`) and so on.

OTOML preserves that information and makes it available to the user.

It also offers a convenient interface for accessing and modifying values in deeply nested tables.

## Example

```ocaml
utop # #require "otoml";;

(* Parse a TOML string. *)

utop # let t = Otoml.Parser.from_string "
[settings]
  [settings.basic]
    crash_randomly = true
" ;;
val t : Otoml.t =
  Otoml.TomlTable
   [("settings",
     Otoml.TomlTable
      [("basic", Otoml.TomlTable [("crash_randomly", Otoml.TomlBoolean true)])])]

(* Look up a deeply nested value with a known type. *)
utop # Otoml.find t Otoml.get_boolean ["settings"; "basic"; "crash_randomly"] ;;
- : bool = true

(* Update a deeply nested value. *)
utop # let t = Otoml.update t ["settings"; "basic"; "crash_randomly"] (Some (Otoml.integer 0)) ;;
val t : Otoml.t =
  Otoml.TomlTable
   [("settings",
     Otoml.TomlTable
      [("basic", Otoml.TomlTable [("crash_randomly", Otoml.TomlInteger 0)])])]

(* Look up a value and convert it to desired type (if possible). *)
utop # Otoml.find t (Otoml.get_boolean ~strict:false) ["settings"; "basic"; "crash_randomly"] ;;
- : bool = false

(* There's a pretty-printer, too! *)
utop # let t = Otoml.Parser.from_string "[foo] \n [foo.bar] \n baz = {quux = false} \n xyzzy = [ ] \n" |>
  Otoml.Printer.to_channel ~indent_width:4 ~indent_subtables:true ~collapse_tables:true stdout ;;

[foo.bar]
    baz = {quux = false}
    xyzzy = []

val t : unit = ()
```

## Bring your own dependencies

TOML specification requires support for datetime values and arbitrarily large numbers.

For a language that uses machine types and doesn't have datetime support in the standrad library
it means that implementations have to make a choice whether to be light on dependencies and easy to use
or be standard-compliant.

OTOML solves that problem with OCaml functors (parameterized modules).

A default implementation is provided for convenience. It only depends on the OCaml standard library.

* Numbers are represented with OCaml's native `int` and `float` types.
* Date and time values are validated but not parsed, represented as strings.

If your application doesn't need large numbers and you either don't use datetime values
or are ready to parse them yourself, it may be all you need.

But if you do, you can bring your own dependencies because the default implementation is not hardcoded,
it's just a predefined instance of a functor.

This is how you could assemble the default implementation yourself:

```ocaml
module DefaultToml = Otoml.Base.Make (Otoml.Base.OCamlNumber) (Otoml.Base.StringDate)
```

Thus you can replace any of the modules or all of them with your own.
For example, this is how you can use [zarith](https://opam.ocaml.org/packages/zarith/)
and [decimal](https://opam.ocaml.org/packages/decimal/) for big numbers,
but keep simple string dates:

```ocaml
module BigNumber = struct
  type int = Z.t
  type float = Decimal.t

  let int_of_string = Z.of_string
  let int_to_string = Z.to_string
  let int_of_boolean b = if b then Z.one else Z.zero
  let int_to_boolean n = (n <> Z.zero)

  (* Can't just reuse Decimal.to/of_string because their optional arguments
     would cause a signature mismatch. *)
  let float_of_string s = Decimal.of_string s

  (* Decimal.to_string uses "NaN" spelling
     while TOML requires all special float values to be lowercase. *)
  let float_to_string x = Decimal.to_string x |> String.lowercase_ascii
  let float_of_boolean b = if b then Decimal.one else Decimal.zero
  let float_to_boolean x = Decimal.(x <> zero)

  let float_of_int = Decimal.of_bigint
  let int_of_float = Decimal.to_bigint
end

module MyToml = Otoml.Base.Make (BigNumber) (Otoml.Base.StringDate)
```

## Deviations from the TOML 1.0 specification

The default implementation is not fully compliant with the standard. These are the deviations:

>Arbitrary 64-bit signed integers (from −2^63 to 2^63−1) should be accepted and handled losslessly.
>If an integer cannot be represented losslessly, an error must be thrown.

The default implementation uses OCaml's native integer type, which is 63-bit on 64-bit architectures and 31-bit on 32-bit ones.

Numbers greater than maximum representable values will cause generic parse errors
(`string ... does not represent a valid integer/floating point number`).
More precise error message for that case may be added in the future.

>To unambiguously represent a specific instant in time, you may use an RFC 3339 formatted date-time with offset.
>Millisecond precision is required. Further precision of fractional seconds is implementation-specific.

The default implementation does not interpret datetime values at all,
only checks them for superficial validity and returns as strings.

For example, "1993-09-947" is considered invalid (as expected), but 1993-02-29 is valid despite the fact that 1993 wasn't a leap year.

Thus, actual precision depends on the library you use to parse those date strings or plug into the functor.

## Users

* [soupault](https://www.soupault.app) (static website framework based on HTML element tree rewriting)
* [fromager](https://github.com/mimoo/fromager) (ocamlformat frontend)
* [camyll](https://alan-j-hu.github.io/camyll) (static website generator with literate Agda support)
* [dirsift](https://github.com/darrenldl/dirsift) (directory search utility)
* [lab](https://github.com/tmcgilchrist/ocaml-gitlab) (GitLab CLI)

# Changelog

## 1.0.5

* Carriage return characters now work correctly in multi-line strings,
  so files produced on Windows with CRLF newlines are parsed correctly
  (report by Bohdan Kolesnikov)
* Files that consists of a single comment without a newline are handled correctly now.
* Correct use of custom inequality functions in the example from the README (patch by Yawar Amin) 

## 1.0.4

* `Parser.from_string_result` now handles `Duplicate_key` exceptions rather than lets them escape (#3, report by Ryan Moore).
* `Duplicate_key` exception is now correctly exposed in the module interface.

## 1.0.3

* Fix for a possible open file descriptor leak on errors (patch by Vincent Bernardoff).

## 1.0.2

* Correct logic for non-strict boolean retrieval (`[]` is no longer considered true).

## 1.0.1

* Fixed value lookup inside inline tables.

## 1.0.0

* String representations of special floating-point values (not a number, infinity) are compliant with the TOML spec.
* Support for date to string conversions in accessors.
* Multiple new helpers for easy lookup of values of various types.

## 0.9.3

### Breaking changes

The functor now takes a single `TomlNumber` module instead of independent `TomlInteger` and `TomlFloat`
modules.
The reason for that change is that for integer/float conversions to work, the module needs to know
both int and float types and provide conversion functions that involve both types.
That change has no effect on the default implementation interface.

### New features

* `get_opt` and `get_result` combinators.
* `get_table_values` for quickly retrieving unboxed values from homogemous tables.
* `string_of_path : `
* New `*_opt` and `*_result` versions of the high level interface functions and `*_exn` aliases.

### Bug fixes

* Accessors: fixed incorrect float to boolean conversion and added int/float conversions.
* Exceptions are now correctly exposed in the functorial interface.
* `path_exists` is correctly exposed now as well.
* Integer and float implicit conversions in non-strict mode are now supported.

### Internal changes

* The lexer no longer uses internal mutable state so it's now re-entrant and (hopefully) thread-safe.
* Modules signatures now use `include` to avoid copying large chunks, so the library is much easier to contribute to.

## 0.9.2

### Breaking changes

`Otoml.get_array` now has type `?strict:bool -> (t -> 'a) -> t -> 'a list`,
that is, it requires an accessor function that will be applied to every item of the array.

For example, you can use `Otoml.find t (Otoml.get_array Otoml.get_string) ["foo"]` to retrieve
an array of strings from a TOML document's key `foo`.

The motivation for the change is that it allows retrieving arrays of unwrapped OCaml values in one step.
The old behaviour can still be emulated using an identify function for the accessor,
for example the built-in `Otoml.get_value : 'a -> 'a`.

### New features

New `Otoml.path_exists t ["some"; "table"; "key"]` allows checking if a key path exists in a TOML document.

`Otoml.Printer.to_string/to_channel` functions now provide `~force_table_array` option. When set to true,
it forces every array that contains nothing but tables to be rendered using the `[[...]]`` table array syntax.

### Bug fixes

Unicode escape sequences are now printed correctly.

If a table has subtables and non-table items, the non-table items are forcibly moved before the first subtable
for printing. This way the output parses correctly, otherwise the non-table items would be mistakenly treated
as subtable members. This way hand-constructed TOML tables are always formatted correctly even if the user
inserts non-table items after a subtable.

### Testing

Added a minimal test suite for the read-write interface, and a TOML test suite client executable for encoder testing.

## 0.9.1

Default implementation modules correctly expose their `type t` so that custom implementations
built using the functorial interface don't end up with unintentionally abstract types
for integers, floats, and dates.

## 0.9.0

Initial release.

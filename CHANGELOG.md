# Changelog

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

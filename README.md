# eztoml

A TOML parsing and manipulation library for OCaml

## Goals

The main goal for writing another TOML library is to provide a library for _manipulating_ TOML files, not just _reading_ them.

TOML is designed as a configuration file format.
It's not just a serialization format for machines to talk to one another.
A lot of time it's written, edited, and read by humans.

That is why TOML supports comments and multiple ways to write the same data. 

Ideally, when a program reads a TOML file and writes it back, it should be able to preserve
comments, user's choice of using inline records vs sections (i.e. `section = { ...}` vs `[section]`) and so on.

eztoml preserves that information and makes it available to the user.

## Example

Mockup:

```ocaml
# let toml = "

# There are many like it, but this one is mine
[my_section]
  inline_record = {
    my_option = false
  }
"

# EzToml.from_string toml;;

- : [> EzToml.t ] = 
`Table [("my_section", `Table [("inline_record", `Table [("my_option", `Bool false)])])]

# EzToml.Ast.from_string toml ;;
- : EzToml.Ast.t = [
`Comment "There are many like it, but this one is mine";
`Table ("my_section", [`InlineTable ...
]

```

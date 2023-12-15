# Notes

Streaming vs complete parser: Cannot check for "end" with a streaming parser, must be complete

# TODO

- Test with relevant .core files, if possible with many files
All of above will be hard work, but most likely possible

- CI tests:
  - Cargo test for all files
  - erlc to check that core files can be compiled

Split parser into sub-modules:
- Lexical definitions
- Terminals
- Non-terminals (productions?)

- Nice to have, better error messages:
  - https://github.com/rust-bakery/nom/blob/main/doc/error_management.md
  - https://docs.rs/nom_locate/latest/nom_locate/
  - https://docs.rs/nom-supreme/latest/nom_supreme/
  - https://www.youtube.com/watch?v=Ph7xHhBfH0w
  - https://docs.rs/miette/latest/miette/

/home/ek/code/otp/lib/compiler/src/core_parse.yrl

https://github.com/erlang/otp/blob/master/lib/compiler/src/core_scan.erl

Nice links:
- https://naiveai.hashnode.dev/practical-parsing-nom#heading-putting-it-all-together

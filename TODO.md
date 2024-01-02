# Notes

Streaming vs complete parser: Cannot check for "end" (without taking all input anyways) with a streaming parser, therefore it makes sense to use a complete parser (one that takes all input).

Core Erlang is compiled only. No interactive way to run code like in normal non-core Erlang.

# TODO

- Test with relevant .core files, if possible with many files
All of above will be hard work, but most likely possible

- CI tests:
  - Cargo test for all files
  - erlc to check that core files can be compiled

General TODO: Maybe avoid manual OK((i, sth)) return use map instead?

Split parser into sub-modules:
- Lexical definitions
- Terminals
- Non-terminals (productions?)

- move parser to create and separate git repo
  - Before this can be done annotations needs to be parsed as well.

- Nice to have, better error messages:
  - https://github.com/rust-bakery/nom/blob/main/doc/error_management.md
  - https://docs.rs/nom_locate/latest/nom_locate/
  - https://docs.rs/nom-supreme/latest/nom_supreme/
  - https://www.youtube.com/watch?v=Ph7xHhBfH0w
  - https://docs.rs/miette/latest/miette/

- Test suite use OTP Lib:
  - Inspired by https://github.com/hamler-lang/CoreErlang/tree/master/test/data
  - The idea is to convert any erl file from otp to .core and check them
  - https://github.com/erlang/otp/tree/master/lib
  - Instead of keeping all files there could be a script to get all these files and flatten the file-names or at the very least the paths.

- Additional test suite ideas:
  - https://github.com/erlang/otp/
  - https://github.com/erlang/otp/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22
  - https://github.com/drobakowski/awesome-erlang

Consider supporting older OTP versions. Right now I only accept the translation from erl to core as defined in OTP26. 
- According to my testing version 23-26 use the same translation to .core and anything older is different.

/home/ek/code/otp/lib/compiler/src/cerl.erl
/home/ek/code/otp/lib/compiler/src/core_parse.yrl

https://github.com/erlang/otp/blob/master/lib/compiler/src/core_scan.erl

Nice links:
- https://naiveai.hashnode.dev/practical-parsing-nom#heading-putting-it-all-together

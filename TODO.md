# TODO

List of elements to improve upon for this project sorted by category.

## General improvements

- better erlang integration. Get cerl direct not via filesystem
- move parser to create and separate git repo
  - Before this can be done annotations needs to be parsed as well.
- avoid manual OK((i, sth)) return use map instead?

## Code quality

- better error message style:
  - <https://github.com/rust-bakery/nom/blob/main/doc/error_management.md>
  - <https://docs.rs/nom_locate/latest/nom_locate/>
  - <https://docs.rs/nom-supreme/latest/nom_supreme/>
  - <https://www.youtube.com/watch?v=Ph7xHhBfH0w>
  - <https://docs.rs/miette/latest/miette/>
- Cargo runs all tests twice
  - Once for lib.rs and once for main.rs
  - What to do about it? Not a problem right now though.

## Well-formedness

- End must be the end. E.g. (!int. end. ?int) is not ok.
- Consistency sanity check:
  - Is `-session` defined for a function that is used or is it defined without
    any function body? If so give error/warning. Also require spec for function.

## Test suite improvements

- Parser test suite use OTP Lib:
  - Inspired by <https://github.com/hamler-lang/CoreErlang/tree/master/test/data>
  - The idea is to convert any erl file from otp to .core and check them
  - <https://github.com/erlang/otp/tree/master/lib>
- Additional test suite ideas:
  - <https://github.com/erlang/otp/>
  - <https://github.com/erlang/otp/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22>
  - <https://github.com/drobakowski/awesome-erlang>
- Good list of examples
  - <https://github.com/gertab/ElixirST/tree/master/lib/elixirst/examples>

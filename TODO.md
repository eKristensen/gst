# Notes

Streaming vs complete parser: Cannot check for "end" (without taking all input anyways) with a streaming parser, therefore it makes sense to use a complete parser (one that takes all input).

Core Erlang is compiled only. No interactive way to run code like in normal non-core Erlang.

Be inspired by https://staff.um.edu.mt/afra1/papers/Agere21.pdf
- Use tag "-session" on client side and describe sessions as a string to parse later. The same idea as I had
- Since someone else did the same it is properly not all bad.

# TODO

- Keep the Erlang preprossor (epp) in mind. Also remember that we are in Core Erlang, which is too far in the compile process for details in EPP to really matter. Still good to remember that it exists. It could be usefull.

- TODO IMPORTANT: Being able to write (!int. end. ?int) should be a syntax error (will be added as a well-formedness constrain)

Improve code quality:
- Resolve Rust warnings and make clippy happy
- Handle all panic and todo: In other words: There should not be anywhere errors are not handled.
- Error messages are currently mostly useless, e.g. There is no hint that to say you might have forgotten to add "." after a session types 
- Limit use of clone()

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

Good list of examples:
https://github.com/gertab/ElixirST/tree/master/lib/elixirst/examples

Native types supported in Elixir ST:
https://github.com/gertab/ElixirST/blob/75d098f51df40b5ff1022c7dc56a695b0f3da9d9/lib/elixirst/session_type.ex#L122

`-spec` documentation: https://www.erlang.org/doc/reference_manual/typespec#specifications-for-functions
- Simple singular
- Cases `;`
  - Requires spec to have alternatives
- Conditional `when`
  - Conditional alternatives? Something like `Option<Condition>` ?
- Optional return information?
  - Part of Simple singular case. Properly not important for this analysis anyways.
- I am fairly certain that the format of the spec is tagged tuples like the Erlang Abstract Format. When Erlang translates code to core it properly internally represents the erlang program in erlang abstract format. The -spec does not have anything specific to core erlang, and therefore the spec is added directly as tagged tuples as this is compatible with core erlang.
  - Important "documentation" source: https://github.com/zuiderkwast/erlang_abstract_format

TODO: Consistency sanity check:
- Is `-session` defined for a function that is used or is it defined without any function body? If so give error/warning. Also require spec for function.

# Type inference

Potentially a problem. How to now what type it has.

# Choices

Core erlang allows reassignment to same variable name. This type checker will halt if a variable is reassigned to a different type than it already has. This is in order to avoid problems related to renaming and/or reusing variable names.

TODO: Maybe reconsider to allow variables to be renames as core erlang allows anyways.

# Cargo runs all tests twice

Once for lib.rs and once for main.rs

What to do about it? Not a problem right now though.

# Change session type format

After meeting we got the following as a starting point for the client example that I have

```
-type st_return() :: {}.


-service ("ServerPid1/travel: !string. ?int. +(book: ... ; reject: ...)").
-service ("ServerPid2/calculator: +(neg: !int. ?int. ; add: !int !int ?int)").
-session (" SessionID: +neg: !int ?int  ").



%                       That name does not matter, what matters is that it matches the function argument
% Client scripts         ↓↓
-session ("'negation'(fresh(!number. ?number. !string),_)").
-spec negation(fresh(),number()) -> st_return().
```

# Opening n new sessions in a function

Note 25. jan 2025: If the number of sessions that a function opens is not static the typing system should reject that function.

However it might be relevant to consider later

Maybe we will need to work with dependent types.

# Erlang no native session types

Erlang does not have native session types or the ability to create a new type

I could use any() and then somehow use the type name for something, but that is not transparent/easy to understand
Therefore it is a bad idea

I decide not to use the built in -spec and type everything myself.

Maybe the best option is somewhere in the middle

The session id is going to be a ref, and that is enough for dialyzer/eqwalizer

We need more to check sessions, but the checks are only "symbolic", not enforced by erlang.
Channel types does not exist natively in erlang, so anything that needs to describe it must be "fake"/a string



How would a client hand over a session to another client i erlang? aka what would a session type look like?
Just a ref sent along for further use?
Something more?

Session-channel = {ServerID, SessionID} = {ref(),atom()}

    new      session = {ref(), new}         <-- "constructor"
fwd/existing session = {ref(), atom()}      <-- "ongoing"

ref() needs to be known.


# Notes

Various notes related to this project.

## Goals

The goal is only to support OTP26 or newer, nothing older.

## Implementation notes, choices and inspiration

Streaming vs complete parser: Cannot check for "end" (without taking all input
anyways) with a streaming parser, therefore it makes sense to use a complete
parser (one that takes all input).

Core Erlang is compiled only. No interactive way to run code like in normal
non-core Erlang.

This work is similar to <https://staff.um.edu.mt/afra1/papers/Agere21.pdf>

Keep the Erlang preprocessor (epp) in mind. Also remember that we are in Core
Erlang, which is too far in the compile process for details in EPP to really
matter. Still good to remember that it exists. It could be useful.

According to my testing version 23-26 use the same translation to .core and
anything older is different.

### Parser whitespace strategy

* Remove whitespace as early as possible.
* Remove whitespace before starting
* Remove whitespace after each component
* Avoid excess calls to whitespace parser.
* Each component takes care of internal whitespace.

Skipping whitespace is fast and cannot result in rollback. While fast there is no need to call it more than needed. Therefore at start and then after each compoment.

### Extending the Erlang type system

Erlang does not have native session types or the ability to create a new
completely type

It is possible to use any() and then somehow use the type name for something,
but that is not transparent/easy to understand. Therefore it is a bad idea

The best option is somewhere in the middle. We extend the type system and reuse
all prior work in Dialyzer/eqwalizer etc. The session id is going to be a ref,
that is enough for compatibility with existing tools.

Everything related to sessions are going to be in a new `-session` spec.
The content of this spec will only make sense to the type checker, not Erlang.

Furthermore, we use the behaviors, specifically the gen server to model
"session channels". This is important for the type system and to ensure
we know how the system will behave at runtime.

## Core Erlang references

[otp git repo]/lib/compiler/src/cerl.erl
[otp git repo]/lib/compiler/src/core_parse.yrl

<https://github.com/erlang/otp/blob/master/lib/compiler/src/core_scan.erl>

Native types supported in Elixir ST:
<https://github.com/gertab/ElixirST/blob/75d098f51df40b5ff1022c7dc56a695b0f3da9d9/lib/elixirst/session_type.ex#L122>

## `-spec` documentation

<https://www.erlang.org/doc/reference_manual/typespec#specifications-for-functions>

<https://github.com/zuiderkwast/erlang_abstract_format>

## Nom Links

<https://naiveai.hashnode.dev/practical-parsing-nom#heading-putting-it-all-together>

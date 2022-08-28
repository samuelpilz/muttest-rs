# Muttest Architecture

## Concepts

* **mutable**: a snippet of code that can be mutated, decided at compile-time
* **mutation**: an alternative behavior for a mutable, decided at run-time

### Mutable

A snippet of code that can be mutated is called a *mutable*.
Only code that is executed at run-time can be mutated.

**Code that can be mutated**

* non-const expressions, blocks and statements

**Code that cannot be mutated**

* const expressions and functions
* function names
* what type impl which traits

Mutables are identified by numeric id and their crate name.


<!-- TODO: more
* what can be a mutable
* mutable definition
* mutable-details
 -->

## Components

* library `muttest-core`
* proc-macro `mutate`
* crate `muttest`
* executable `cargo-muttest`

<!-- TODO: explain in detail -->

### Muttest files

The proc-macro and muttest runner communicate via files in the `target/muttest` folder.

* `mutable-definitions-<crate>.csv`: information gathered by syntax analysis
* `mutable-details.csv`: static information gathered by the compiler
* `cover-<x>.csv`: run-time information

## Structure of this repository

There are several crates here:

* `muttest-core` in the root folder.
* `muttest-codegen` in `codegen`
* `muttest` in `api` that exports
* `cargo-muttest` in `app`

### Reasons for separate crates

1. The proc-macro has to be defined in a separate crate. However, users should only have use `muttest` as dependency.
2. The dependencies for the runner should not be included when depending on in the `muttest` crate.
3. The proc-macro related dependencies are only included in the proc-macro crate
4. Extra dependencies might pull trait impls, which might break type inference for crates using `muttest`

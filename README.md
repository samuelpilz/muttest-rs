# Rust Mutation Testing

*Are your tests any good?*

Tests have one main purpose: to find bugs!
How good are your tests at finding these?

We can simulate bugs by changing (mutating) the code.
A good test suite should catch these bugs by failing, *killing* the mutant.
A large number of surviving mutants hint at test suite defects.

## How To Use

1. Install cargo plugin `cargo-muttest`
2. Add `muttest` as dev-dependency for your crate
3. Annotate your code (e.g. `impl` blocks) with `#[cfg_attr(test, muttest::mutate)]`.
4. Annotate the modules of your tests with `#[cfg_attr(test, muttest::tests)]`
5. run `cargo muttest --lib`
6. analyze the report. The file `target/muttest/report.json` contains the report in json format.

If you want to include the integration tests in mutation analysis, the setup needs to be a bit different:

- `muttest` is an optional `dependency` instead of a `dev-dependency`.
- The attributes are guarded with `#[cfg_attr(feature = "muttest", ...)]`
- Run `cargo muttest` without `--lib`

Whatever setup you choose, make sure that `muttest` does not affect release-code. 

Using the environment variable `RUST_LOG=debug`, you can see the commands executed during mutation analysis.

### Limitations

`muttest` can only mutate expressions that are executed at run-time.
It is not possible to mutate:

* const & static definitions
* `const fn`s, even when called at run-time
* macros & their arguments
* patterns
* types

For technical reasons, only top-level items in modules can be annotated with `#[mutate]`.

### Performance Improvements

If the run-time of mutation analysis is dominated by mutant evaluation, you can instruct the compiler to optimize the test suite by adding this profile and adding the option `--profile muttest` when calling `cargo muttest`.

```toml
[profile.muttest]
inherits = "dev"
opt-level = 3
lto = true
```

## How `muttest` Works

In a nutshell, the macro `#[mutate]` transforms certain patterns of source code and inserts hooks that can enable mutations at run-time.
This way, the code only needs to be compiled once.

### Crate Structure

Most of the functionality is implemented in the crate at the root of this repository, `muttest-core`.
The core library implements source code transformations, mutation activation, and code for program analysis.

This repository contains several other crates

- `muttest-codegen` in directory `codegen`
- `muttest` in directory `api` re-exports the relevant types from the proc-macro crate and the core crate 
- `muttest-selftest` in the directory `selftest` for mutation testing of `muttest` itself.

### Acknowledgements and Prior Work

* Roland H. Untch, Andrew J. Offutt, and Mary J. Harrold: [Mutation Analysis Using Mutant Schemata](https://doi.org/10.1145/174146.154265)
  * Mutant Schema Generation (MSG): using program transformation for mutation testing on Fortran programs
* llogiq: [`mutagen`](https://github.com/llogiq/mutagen)
  * Using proc-macros for MSG in Rust
  * Using dead branches to help the compiler with type inference ([in this blog post](https://llogiq.github.io/2018/04/11/shift.html))
  * [opportunistic mutations](https://llogiq.github.io/2018/03/03/opportune.html)
* Autoderef-based specialization
  * [Autoref-based stable specialization](https://github.com/dtolnay/case-studies/blob/master/autoref-specialization/README.md) by dtolnay
  * [Generalized Autoref-based specialization](http://lukaskalbertodt.github.io/2019/12/05/generalized-autoref-based-specialization.html) by Lukas introduces Auto*de*ref-based specialization.

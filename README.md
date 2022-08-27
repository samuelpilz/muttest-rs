# Rust Mutation Testing

*Are your tests any good?*

Tests have one main purpose: to find bugs!
How good are your tests at finding these?

We can simulate bugs by changing (mutating) the code.
A good test suite should catch these bugs by failing, "killing" the mutant.
A large number of surviving mutants hint at test suite defects.

## Using `muttest`

**tl;dr:**

1. Install cargo plugin `cargo-muttest`
2. Add `muttest` as dependency for your crate
3. Annotate your code blocks with `#[muttest::mutate]`. 
4. run `cargo muttest`
5. analyze the report

<!-- TODO: extended tutorial -->
<!-- TODO: dev-dependency & test-guard or feature-guard? -->

## Limitations

* cannot mutate consts
* we skip unsafe

## Acknowledgements and related Work

* Roland H. Untch *et.al.*: (Mutation Analysis Using Mutant Schemata)[https://doi.org/10.1145/174146.154265]
  * Mutant Schema Generation (MSG): using program transformation for mutation testing on Fortran programs
* dtolnay: (Autoref-based stable specialization)[https://github.com/dtolnay/case-studies/blob/master/autoref-specialization/README.md]
* llogiq: [`mutagen`](https://github.com/llogiq/mutagen)
  * Using proc-macros for MSG in Rust
  * Using dead branches to help the compiler with type inference ((in this blog post)[https://llogiq.github.io/2018/04/11/shift.html])
  * (opportunistic mutations)[https://llogiq.github.io/2018/03/03/opportune.html]
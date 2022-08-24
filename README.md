# Rust Mutation Testing

*How good are your tests at finding bugs?*

We can simulate bugs by changing (mutating) of the code.
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


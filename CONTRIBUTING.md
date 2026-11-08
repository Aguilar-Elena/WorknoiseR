# Contributing to WorknoiseR

Contributions to WorknoiseR are welcome, particularly those that improve reproducibility, occupational-hygiene methodology, testing, documentation, bilingual support and spatial noise modelling.

## Before contributing

Please open an issue before making substantial changes so that the proposed scope and methodological approach can be discussed.

For bug reports, include:

- the WorknoiseR version;
- R version and operating system;
- a minimal reproducible example;
- expected behaviour;
- observed behaviour;
- relevant warnings or errors.

## Development workflow

1. Fork the repository.
2. Create a feature or fix branch.
3. Make focused changes.
4. Add or update tests where appropriate.
5. Run:

```r
devtools::test()
devtools::check()
```

6. Confirm that no existing numerical behaviour has changed unintentionally.
7. Submit a pull request explaining the change and its methodological implications.

## Occupational-noise methodology

Changes affecting calculations, regulatory interpretation, hearing-protector methods or spatial modelling should include a clear technical rationale and, where appropriate, references to the applicable legislation, standards or validated methodology.

## Bilingual behaviour

The Spanish and English interfaces share the same numerical core. Contributions must not introduce language-dependent numerical results.

## Code style

Keep functions focused, documented and independently testable. Avoid unnecessary dependencies and preserve backward compatibility where reasonably possible.

## Testing

New functionality should include tests covering expected results, edge cases and, where relevant, Spanish/English equivalence.

## License

By contributing, you agree that your contributions will be distributed under the project's MIT License.

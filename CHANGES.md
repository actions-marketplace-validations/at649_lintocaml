# Changes

## 0.1.1

- Fixes analysis inside exception handlers and distinguishes guarded, deferred,
  and explicitly handled partial calls.
- Improves rule accuracy, messages, profiles, and comparison-result coverage.
- Hardens source resolution, malformed-location handling, SARIF output, and
  source fixes; symbolic links are never replaced by `--fix`.
- Makes the GitHub Action ready for Marketplace use with input validation,
  reliable failure propagation, optional SARIF generation, and CI smoke tests.
- Refreshes installation, configuration, rule, and release documentation.

## 0.1.0

Initial release.

- Reads `.cmt` and `.cmti` artifacts from OCaml 5.2 through 5.5.
- Includes 51 correctness, performance, and idiom rules.
- Supports text, JSON, and SARIF output, path-based configuration, audited
  suppressions, and conservative source fixes.
- Provides a pre-commit hook and a composite GitHub Action.

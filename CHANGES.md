# Changes

## 0.1.5

- The fixture suite no longer needs `perl`. It was used to edit one fixture,
  and the opam builders are not guaranteed to ship it.

## 0.1.4

- Corrected the sample output in the README: a run prints "0 hints across 12
  files", never "0 hint across 12 file(s)".

## 0.1.3

- The fixture suite no longer shells out to `python3`. The opam builders do
  not ship it, so every assertion there failed. The checks moved to a small
  OCaml helper built alongside the tests.

## 0.1.2

- SARIF now reports the analyser version, so code scanning can tell one
  lintml build's findings from another's.
- Dropped the `lintml.conf` fallback. It named a file that never shipped, so
  a stray one could quietly become config.
- Fixed the action reference in the README, which pointed at a tag that does
  not exist.

## 0.1.1

Initial release.

- Reads `.cmt` and `.cmti` artifacts from OCaml 5.2 through 5.5.
- Includes 51 correctness, performance, and idiom rules.
- Supports text, JSON, and SARIF output, path-based configuration, audited
  suppressions, and conservative source fixes.
- Provides a pre-commit hook and a composite GitHub Action.

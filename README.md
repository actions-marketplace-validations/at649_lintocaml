# lintml

An OCaml linter that works from the typed AST. It reads the `.cmt` files your
build produces, so rules see resolved names and real types: `a == b` on two
strings is reported, `a == b` on two ints is not.

```console
$ dune build @check && lintml
src/parse.ml:8:45: error: physical equality `==` on a boxed value compares allocations, not contents
  | let same_string (a : string) (b : string) = a == b
  |                                             ^^^^^^
  hint: use `=` for structural equality
  rule: physical-eq-on-boxed

src/scan.ml:6:34: warning: List.length is O(n); comparing it to 0 walks the whole list to answer an O(1) question
  | let empty_check (l : int list) = List.length l = 0
  |                                  ^^^^^^^^^^^^^^^^^
  hint: compare against the empty list: `l = []`
  rule: length-compare-zero

summary: 1 error, 1 warning, 0 hints across 12 files
```

Requires OCaml 5.2 to 5.5.

## Profiles

The default profile is bugs and expensive mistakes. Style advice lives in
`--profile idiomatic`, and `--profile pedantic` turns on everything.

Rules stay quiet when they cannot prove what they would claim. If a type is a
variable rather than a known one, the rule says nothing rather than guess.

## Install

Until the opam release lands, pin the tag:

```sh
opam pin add lintml https://github.com/at649/lintml.git#v0.1.5
```

From a clone instead:

```sh
opam install . --deps-only --with-test
opam exec -- dune build @install
opam pin add --yes lintml.dev .
```

To work on lintml without installing it, use
`opam exec -- dune exec lintml -- --help`.

## Getting started

lintml reads compiler artifacts, so build the project before linting it. Exit
code 3 means no artifacts were found.

```sh
dune build @check        # produces .cmt files
lintml                   # lints _build
```

| Command | |
|---|---|
| `lintml lint PATH` | lint a specific directory |
| `lintml list-rules` | rules active in a profile |
| `lintml explain RULE` | why a rule exists and what to do instead |
| `lintml --fix` | apply the mechanically safe fixes |
| `lintml --version` | print the installed version |

| Flag | |
|---|---|
| `--profile default\|idiomatic\|pedantic` | which rules run |
| `--fail-on error\|warning\|hint\|never` | exit-code threshold |
| `--format text\|json\|sarif` | `sarif` feeds GitHub code scanning |
| `--report-suppressed` | include suppressed findings and their reasons |

## What it catches

51 rules. `lintml list-rules --profile pedantic` prints them all; a
representative sample:

**Correctness** — on by default

| Rule | |
|---|---|
| `physical-eq-on-boxed` | `==` on strings, lists or tuples compares allocations, not contents |
| `poly-compare-on-function` | structural comparison on a function raises at runtime |
| `compare-result-equality` | `compare a b = 1` is false for most inputs; only the sign is specified |
| `swallowed-exception` | `try ... with _ ->` also eats `Out_of_memory` and cancellation |
| `partial-function` | `List.hd`, `Option.get`, `Hashtbl.find` where a total version exists |
| `ignored-promise` | a discarded `Lwt.t` or `Eio` promise loses its work and its failure |
| `division-by-zero` | literal `/ 0` or `mod 0` |
| `mutable-hashtable-key` | a key that can change under the table |

**Performance** — on by default

| Rule | |
|---|---|
| `length-compare-zero` | `List.length l = 0` walks the list to answer an O(1) question |
| `quadratic-concat` | building a string with `^` in a fold |
| `list-append-in-loop` | `acc @ [x]` in a fold |
| `head-of-traversal` | `List.hd (List.sort c l)` is O(n log n) for an O(n) question |
| `exists-via-filter` | filtering and counting to answer yes or no |
| `hashtbl-mem-then-find` | two lookups where one would do |

**Idiom** — `--profile idiomatic`

`redundant-if-bool`, `match-bool`, `double-negation`, `negated-condition`,
`identity-map`, `redundant-fun-wrapper`, `option-match-to-combinator` and
others. Useful on a codebase you are cleaning up, noise on one you are not.

## Configuration

`lintml.toml`, found by searching upward from the current directory:

```toml
profile = "default"

[rules]
physical-eq-on-boxed = "error"
partial-function = "hint"
length-compare-zero = "off"

[[overrides]]
paths = ["test/**", "vendor/**", "**/parser.ml"]
profile = "off"
```

Turn generated code off first. A menhir parser is tens of thousands of lines of
`Obj.magic` and will bury every finding you care about; nobody is going to edit
it anyway. The same goes for anything else a build step writes.

Paths are relative to the config file, so it means the same thing wherever you
run from. Later overrides win. An unknown rule ID is an error rather than a
silent no-op, so a typo cannot quietly disable a rule.

The parser accepts the TOML forms shown above: double-quoted strings, string
arrays, `[rules]`, `[[overrides]]`, and `[overrides.rules]`. It rejects unknown
sections and malformed values instead of guessing.

To suppress one finding, say so where it happens:

```ocaml
(left == right)
[@lintml.allow ("physical-eq-on-boxed", "identity is intentional")]
```

## Fixes

`--fix` applies only replacements whose equivalence is mechanical, currently
`constant-condition`, `redundant-list-append` and `double-negation`.
Performance suggestions are never applied automatically: several of them change
physical identity, and a linter should not make that decision for you.

Overlapping edits are skipped rather than applied in sequence, and a fix is
skipped when the source no longer matches what the rule saw, so a stale `.cmt`
cannot corrupt a file. Files are replaced atomically. Rebuild before linting
again.

## CI

```yaml
permissions:
  contents: read
  security-events: write

steps:
  - uses: actions/checkout@v6
  - uses: ocaml/setup-ocaml@v3
    with:
      ocaml-compiler: "5.5.x"
  - uses: at649/lintml@v0.1.5
    with:
      profile: default
      fail-on: error
```

The action installs lintml in the opam switch created by `setup-ocaml`, builds
the project, and uploads SARIF so findings appear in code scanning. Set
`upload-sarif: "false"` if the repository cannot grant `security-events: write`.
For several scan roots, put one path on each line:

```yaml
    with:
      paths: |
        _build/default/lib
        _build/default/bin
```

Exit codes:

| Code | |
|---:|---|
| 0 | nothing reached `--fail-on` |
| 1 | findings reached `--fail-on` |
| 2 | bad arguments or configuration |
| 3 | no `.cmt` files — build with `-bin-annot` first (`dune build @check`) |
| 4 | every artifact came from an unsupported compiler |
| 10 | no artifact could be read, or an internal error |

An artifact that cannot be read is a warning while other artifacts remain
usable. Codes 4 and 10 mean nothing was analysable.

## Limitations

The project has to build first. Any build that passes `-bin-annot` works: dune,
`ocamlfind ocamlc -bin-annot`, a Makefile. Builds that never emit `.cmt` give
lintml nothing to read.

It does not track values, so `List.nth l (Random.int (List.length l))` is
reported even though it cannot fail. It does understand a `[]` match arm, a
`try` catching the exception, and `if List.length l > n`.

It never sees surface syntax. `a.(i)` and `Array.get a i` are the same typed
node, so rules about formatting or spelling cannot be written. See CONTRIBUTING.

## Contributing

Adding a rule is the easiest way in: each one is a single module plus a fixture
pair, and needs no knowledge of the engine. See [CONTRIBUTING.md](CONTRIBUTING.md).

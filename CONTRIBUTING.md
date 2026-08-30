# Contributing

The easiest useful contribution is a rule: one module, one fixture pair, no
engine knowledge needed.

## Adding a rule

Create `lib/rules/<rule_id>.ml`, with underscores where the id has hyphens —
`test/run_fixtures.sh` enforces that. Match on `Expr_view.t` and never open
`Typedtree`. If the view is missing a node kind you need, extend `Expr_view`
and `Tast_iface` together; they are the only two files that know the compiler's
AST exists.

Register it in `lib/rules/registry.ml`.

Add fixtures to `test/fixtures/sample/sample.ml`—at least one case that must be
reported and one that must not—and assert both in `test/run_fixtures.sh`.
The negative case is the one that matters. Append to the end of the fixture
file and do not reflow it, because the assertions are keyed on line numbers.

Then run `opam exec -- dune runtest` and check your rule actually fired. A rule
that matches nothing still builds, still registers, and still appears in
`list-rules` as though it works. That has happened here.

## What makes a rule acceptable

Three things, all of them:

It is not already a compiler warning. If `ocamlc -w` catches it, the rule is
noise.

It stays quiet when it cannot establish a fact. `physical-eq-on-boxed` says
nothing about `a == b` when the type is a variable, because a guess there is
indistinguishable from a false positive to whoever reads the output.

It can justify itself in three sentences. Those sentences go in `docs`, which
is what `lintml explain` prints. If you cannot write them, the rule is an
opinion.

Rules that catch bugs go in the `Default` profile; rules that express taste go
in `Idiomatic`. When unsure, pick `Idiomatic`. Linters get uninstalled over
correct-but-worthless findings far more often than over wrong ones.

## Things that will surprise you

Alphabetic operators are escaped in resolved paths. `mod` arrives as
`Stdlib.\#mod`, and the same goes for `land`, `lor`, `lxor`, `lsl`, `lsr` and
`asr`. `Expr_view.path_is` strips the escape so you can write the plain name,
but comparing paths by hand will silently never match. `division-by-zero`
ignored `mod 0` for a while for exactly this reason.

Warning 9 is fatal here, so a record pattern must bind every field. On
`Expr_view.Function` add `; _` unless you actually mean to constrain
`recursive` and `simple_params`.

Those two fields exist because eta-reduction advice can break code. A function
with labelled or optional parameters cannot be eta-reduced at all, and reducing
a `let rec` gives `let rec f = g`, which the compiler rejects. Any rule
correlating parameters with arguments needs `simple_params = true`; any rule
proposing eta-reduction also needs `recursive = false`.

`Tpat_construct` is typed for value patterns. A general pattern may expose a
constructor directly or beneath `Tpat_value`, depending on the typed-tree
context. Code that examines constructors must handle both shapes.

When a general rule and a specific one both match an expression, the general
one should decline — otherwise one line draws two findings. See
`covered_elsewhere` in `length_compare_zero.ml`.

## What the typed AST cannot tell you

`.cmt` files record the typed tree, and the compiler has already normalised
some distinctions away. `fun x -> match x with` and `function` are the same
node. So are `a.(i)` and `Array.get a i`. Parenthesisation, comments and
formatting are gone entirely.

Rules that depend on any of that need a parsetree pass, which does not exist.
Do not write one against the typed AST hoping it will work.

## False positives found in the field

Neither of these is a rule bug. In both the code is genuinely what the rule
describes and the author meant it.

QCheck properties like `fun l -> List.rev (List.rev l) = l` are exactly the
redundant operation `redundant-reverse` reports, and removing it deletes the
test. Exclude test directories with a path override.

`List.nth l (Random.int (List.length l))` cannot go out of range, but proving
it needs value tracking that lintml does not do.

Three guards in `tast_iface.ml` do suppress provably safe partial calls, and
each checks the specific fact rather than spotting something nearby: an
enclosing match with a `[]` arm, an enclosing `try` catching the exception that
call raises, and an enclosing `if` comparing the list's length with a literal.
Catching the wrong exception does not suppress, and `if List.length l > 1` does
not suppress `List.nth l 5`. A fourth guard would follow the same shape —
collect the fact in the walker, key it by location, inject a suppression with a
reason.

## Compiler support

The supported range is declared in `dune-project`, currently 5.2 to 5.5.
Version-specific AST differences live in `lib/engine/tast_iface.ml` behind
`cppo` and nowhere else. CI builds and tests every supported compiler version,
with an additional macOS run on the newest version.

## Before opening a pull request

```sh
opam exec -- dune build @fmt --auto-promote
opam exec -- dune runtest
opam exec -- dune build @check
opam exec -- dune exec -- lintml --fail-on hint _build/default/lib _build/default/bin
```

The last one is lintml linting itself. It has to be clean.

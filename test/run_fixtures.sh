#!/usr/bin/env bash
# End-to-end fixture test. Every rule is asserted both ways: the case it must
# report, and the case it must not.
set -uo pipefail
cd "$(dirname "$0")/.."
ROOT=$(pwd)
if [ -x "$ROOT/_build/default/bin/main.exe" ]; then
  LINTML="$ROOT/_build/default/bin/main.exe"
else
  LINTML="$ROOT/bin/main.exe"
fi
FIXTURE="$ROOT/test/fixtures/sample"
if [ -x "$ROOT/_build/default/test/jsonq.exe" ]; then
  JSONQ="$ROOT/_build/default/test/jsonq.exe"
else
  JSONQ="$ROOT/test/jsonq.exe"
fi
TEST_CONFIG="$ROOT/test/lintml-test.toml"
TEMP_DIRS=()

cleanup() {
  for directory in "${TEMP_DIRS[@]}"; do
    rm -rf -- "$directory"
  done
}
trap cleanup EXIT

[ -x "$LINTML" ] || { echo "FAIL: build lintml first (dune build)"; exit 1; }
( cd "$FIXTURE" && dune clean && dune build @check 2>/dev/null ) ||
  { echo "FAIL: fixture build"; exit 1; }

OUT=$("$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic --format json "$FIXTURE/_build")
OUT_DEFAULT=$("$LINTML" --no-color --config "$TEST_CONFIG" --profile default --format json "$FIXTURE/_build")
fails=0

if echo "$OUT" | "$JSONQ" replacement-metadata; then
  echo "  ok       JSON distinguishes safe replacements from advice"
else
  echo "  WRONG    JSON replacement metadata is incomplete"; fails=$((fails+1))
fi

expect_exit() {
  expected=$1
  shift
  "$@" >/dev/null 2>&1
  actual=$?
  if [ "$actual" -eq "$expected" ]; then
    echo "  ok       exit $expected"
  else
    echo "  WRONG    expected exit $expected, got $actual"; fails=$((fails+1))
  fi
}

expect() {
  if echo "$OUT" | "$JSONQ" has "$1" "$2"; then
    echo "  ok       $1 @ $2"
  else
    echo "  MISSING  $1 @ $2"; fails=$((fails+1))
  fi
}

expect_clean() {
  if echo "$OUT_DEFAULT" | "$JSONQ" line-clean "$1"; then
    echo "  ok       $1 clean  ($2)"
  else
    echo "  FALSE+   $1 should be clean  ($2)"; fails=$((fails+1))
  fi
}

expect_rule_clean() {
  if echo "$OUT" | "$JSONQ" rule-line-clean "$1" "$2"; then
    echo "  ok       $1 @ $2 clean  ($3)"
  else
    echo "  FALSE+   $1 should be clean @ $2  ($3)"; fails=$((fails+1))
  fi
}

expect_hint() {
  if echo "$OUT" | "$JSONQ" suggests "$1" "$2"; then
    echo "  ok       $1 suggests '$2'"
  else
    echo "  WRONG    $1 should suggest '$2'"; fails=$((fails+1))
  fi
}

echo "positive:"
expect length-compare-zero    6
expect physical-eq-on-boxed   8
expect partial-function      10
expect swallowed-exception   12
expect redundant-if-bool     14
expect length-compare-zero   16
expect length-compare-zero   18

echo
echo "suggestion correctness:"
expect_hint  6 'l = []'
expect_hint 16 'l <> []'
expect_hint 18 'l <> []'

echo
echo "negative:"
expect_clean 22 "l = [] is already correct"
expect_clean 24 "== on int is legitimate"
expect_clean 26 "== on functions: = would raise, so == is the only option"
expect_clean 28 "List.nth_opt is the safe variant"
expect_clean 30 "handler names a specific exception"
expect_clean 32 "type variable: must not guess"
expect_clean 34 "catch-all that re-raises is correct"
expect_clean 36 "a.(i) and s.[i] must not report Array.get/String.get"
expect_clean 38 "String.length is O(1)"
expect_clean 40 "Array.length is O(1)"
expect_clean 42 "0 > List.length is always false, not an emptiness test"
expect_clean 44 "a guarded catch-all propagates exceptions that fail the guard"
expect swallowed-exception 46
expect swallowed-exception 49
expect_clean 52 "a tail re-raise of the caught exception is correct"
expect_clean 56 "List.hd on a visibly non-empty literal is safe"
expect_clean 57 "Option.get (Some _) is safe"
expect_clean 58 "List.nth with an in-bounds literal index is safe"
expect physical-eq-on-boxed 65
expect physical-eq-on-boxed 66
expect physical-eq-on-boxed 67
expect_clean 68 "constant variants are immediate"
expect_clean 69 "mixed variants are not always boxed"
expect poly-compare-on-function 71
expect poly-compare-on-float-nan 72
expect length-compare-n 73
expect unsafe-string-index 74
expect ignored-result 78
expect ignored-promise 86
expect poly-compare-on-abstract 88
expect_clean 90 "Float.equal makes NaN semantics explicit"
expect_clean 91 "length minus one is not provably unsafe in a guarded branch"
expect_clean 92 "a matched result is handled"
expect quadratic-concat 94
expect list-append-in-loop 95
expect repeated-list-nth 96
expect_clean 98 "String.concat over the completed list is linear"
expect_clean 99 "cons followed by one reversal is linear"
expect_rule_clean repeated-list-nth 100 "List.nth targets a different list"
expect_clean 101 "a suppression with an audit reason is respected"
expect_clean 102 "float literals cannot be NaN"
expect_clean 103 "an explicit Ok value cannot hide an error"
expect_clean 106 "both branches re-raise the caught exception"
expect physical-eq-on-boxed 109
expect_clean 114 "a structure suppression applies only after its declaration"
expect_rule_clean repeated-list-nth 116 "a fixed List.nth index is bounded work"
expect poly-compare-on-abstract 117
expect_clean 118 "polymorphic hashing is safe for an immediate type"
expect division-by-zero 120
expect_rule_clean division-by-zero 121 "a dynamic divisor may be non-zero"
expect negative-size 122
expect_rule_clean negative-size 123 "a dynamic collection size may be valid"
expect boolean-comparison 124
expect_rule_clean boolean-comparison 125 "a bare boolean needs no simplification"
expect double-negation 126
expect_rule_clean double-negation 127 "one negation is meaningful"
expect redundant-string-concat 128
expect_rule_clean redundant-string-concat 129 "non-empty concatenation is meaningful"
expect_rule_clean list-fusion 130 "map fusion can reorder callback effects"
expect_rule_clean list-fusion 131 "one map allocates no intermediate map result"
expect list-fusion 132
expect_rule_clean list-fusion 133 "List.concat_map is already fused"
expect list-fusion 134
expect_rule_clean list-fusion 135 "List.rev_map is already fused"
expect redundant-reverse 136
expect_rule_clean redundant-reverse 137 "one reversal is meaningful"
expect generic-failure 138
expect_rule_clean generic-failure 139 "a specific exception is intentional"
expect redundant-fun-wrapper 141
expect_rule_clean redundant-fun-wrapper 142 "a transformed argument is not forwarded"
expect_rule_clean redundant-fun-wrapper 144 "a labelled call changes the wrapper type"
expect match-bool 145
expect_rule_clean match-bool 146 "a non-boolean match is meaningful"
expect division-by-zero 147
expect division-by-zero 148
expect negative-size 149
expect redundant-string-concat 150
expect boolean-comparison 151
expect list-fusion 152
expect disabled-all-warnings 153
expect_rule_clean disabled-all-warnings 155 "a specific warning suppression is scoped"
expect mutable-hashtable-key 158
expect_rule_clean mutable-hashtable-key 160 "an immutable hashtable key is stable"
expect_rule_clean physical-eq-on-boxed 24 "physical equality is valid for immediate values"
expect_rule_clean poly-compare-on-abstract 118 "hashing an immediate value is safe"
expect_rule_clean poly-compare-on-float-nan 102 "literal floats cannot be NaN"
expect_rule_clean ignored-result 92 "matching a result consumes it"
expect_rule_clean ignored-promise 161 "returning a promise keeps the computation"
expect_rule_clean partial-function 56 "a visibly non-empty list has a head"
expect_rule_clean swallowed-exception 30 "a specific exception handler is narrow"
expect_rule_clean unsafe-string-index 91 "a guarded last-index access is not provably unsafe"
expect_rule_clean length-compare-zero 22 "direct list emptiness is constant time"
expect_rule_clean length-compare-n 42 "the reversed comparison is not the targeted performance pattern"
expect_rule_clean quadratic-concat 98 "one String.concat is linear"
expect_rule_clean list-append-in-loop 99 "cons and one reverse is linear"
expect_rule_clean redundant-if-bool 162 "the branches are not equivalent to the condition"
expect option-match-to-combinator 165
expect_rule_clean option-match-to-combinator 167 "the bound value is transformed before mapping"
expect option-match-to-combinator 170
expect constant-condition 171
expect_rule_clean constant-condition 172 "a dynamic condition selects a branch"
expect redundant-list-append 173
expect_rule_clean redundant-list-append 174 "a non-empty prefix contributes values"
expect left-nested-list-append 175
expect_rule_clean left-nested-list-append 176 "right nesting copies each list spine once"
expect invalid-string-bounds 177
expect_rule_clean invalid-string-bounds 178 "the final literal index is valid"
expect invalid-string-bounds 179
expect_rule_clean invalid-string-bounds 180 "dynamic slice bounds may be valid"
expect_rule_clean poly-compare-on-float-nan 181 "ordinary IEEE float equality may be intentional"

echo
echo "new rules:"
expect hashtbl-mem-then-find 223
expect_rule_clean hashtbl-mem-then-find 224 "find_opt answers membership and value in one lookup"
expect_rule_clean hashtbl-mem-then-find 227 "a lookup of a different table is a distinct operation"
expect empty-filter-equality 231
expect empty-filter-equality 232
expect_rule_clean empty-filter-equality 233 "an ordinary filter result is meaningful"
expect_rule_clean empty-filter-equality 234 "comparing against a non-empty list is a real comparison"
expect exists-equals-mem 237
expect_rule_clean exists-equals-mem 238 "a transformed element is not a membership test"
expect_rule_clean exists-equals-mem 239 "List.mem is already the named operation"
expect string-length-compare-empty 242
expect string-length-compare-empty 243
expect_rule_clean string-length-compare-empty 244 "a bound against 1 is not the literal-0 pattern"
expect_rule_clean string-length-compare-empty 245 "a length bound above zero is a real comparison"
expect_rule_clean string-length-compare-empty 246 "direct comparison with the empty string is idiomatic"

expect_rule_clean partial-function 250 "an empty-list arm proves the list is non-empty"
expect partial-function 251

echo
echo "safe fixes:"
FIX_PROJECT=$(mktemp -d)
TEMP_DIRS+=("$FIX_PROJECT")
cp -R "$ROOT/test/fixtures/fix/." "$FIX_PROJECT/"
( cd "$FIX_PROJECT" && dune build @check 2>/dev/null ) ||
  { echo "FAIL: fix fixture build"; exit 1; }
"$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic \
  --fail-on never --fix "$FIX_PROJECT/_build" >/dev/null
EXPECTED_FIX='let choose value = value
let append values = values
let nested value = (not (not value))
let keep condition value = if condition then value else 0'
ACTUAL_FIX=$(cat "$FIX_PROJECT/fix_fixture.ml")
if [ "$ACTUAL_FIX" = "$EXPECTED_FIX" ]; then
  echo "  ok       applies safe fixes atomically and skips nested overlaps"
else
  echo "  WRONG    safe fix output differs"; fails=$((fails+1))
  diff -u <(printf '%s\n' "$EXPECTED_FIX") <(printf '%s\n' "$ACTUAL_FIX") || true
fi

STALE_PROJECT=$(mktemp -d)
TEMP_DIRS+=("$STALE_PROJECT")
cp -R "$ROOT/test/fixtures/fix/." "$STALE_PROJECT/"
( cd "$STALE_PROJECT" && dune build @check 2>/dev/null ) ||
  { echo "FAIL: stale fix fixture build"; exit 1; }
perl -0pi -e 's/if true then value/if flag then value/' \
  "$STALE_PROJECT/fix_fixture.ml"
"$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic \
  --fail-on never --fix "$STALE_PROJECT/_build" >/dev/null 2>/dev/null
STALE_EXPECTED='let choose value = if flag then value else 0
let append values = [] @ values
let nested value = if true then not (not value) else false
let keep condition value = if condition then value else 0'
STALE_ACTUAL=$(cat "$STALE_PROJECT/fix_fixture.ml")
if [ "$STALE_ACTUAL" = "$STALE_EXPECTED" ]; then
  echo "  ok       skips fixes when source and compiler artifact differ"
else
  echo "  WRONG    changed source using stale compiler locations"; fails=$((fails+1))
fi

if echo "$OUT" | "$JSONQ" has-in-file disabled-all-warnings sample.mli 1; then
  echo "  ok       analyses .cmti interface attributes"
else
  echo "  MISSING  disabled-all-warnings in sample.mli"; fails=$((fails+1))
fi

echo
echo "command contract:"
expect_exit 2 "$LINTML" --format yaml does-not-exist
expect_exit 2 "$LINTML" --fail-on loud does-not-exist
expect_exit 2 "$LINTML" does-not-exist
expect_exit 3 "$LINTML" lint does-not-exist
expect_exit 0 "$LINTML" explain ignored-result
expect_exit 2 "$LINTML" explain does-not-exist
expect_exit 2 "$LINTML" list-rules --profile loud
expect_exit 1 "$LINTML" --no-color --config "$TEST_CONFIG" "$FIXTURE/_build"
expect_exit 0 "$LINTML" --no-color --fail-on never "$FIXTURE/_build"

DEFAULT_RULES=$("$LINTML" list-rules)
IDIOMATIC_RULES=$("$LINTML" list-rules --profile idiomatic)
PEDANTIC_RULES=$("$LINTML" list-rules --profile pedantic)
if ! echo "$DEFAULT_RULES" | grep -q redundant-if-bool \
   && echo "$IDIOMATIC_RULES" | grep -q redundant-if-bool \
   && ! echo "$IDIOMATIC_RULES" | grep -q generic-failure \
   && echo "$PEDANTIC_RULES" | grep -q generic-failure; then
  echo "  ok       list-rules filters by profile"
else
  echo "  WRONG    list-rules profile filtering failed"; fails=$((fails+1))
fi

DISCOVERED=$(
  cd "$FIXTURE" &&
  "$LINTML" --no-color --profile pedantic --format json _build
)
if echo "$DISCOVERED" | "$JSONQ" no-diagnostics; then
  echo "  ok       discovers config upward and applies path override"
else
  echo "  WRONG    discovered config did not suppress fixture"; fails=$((fails+1))
fi

EXPLICIT_RELATIVE=$(
  cd "$FIXTURE" &&
  "$LINTML" --no-color --config ./lintml.toml --profile pedantic --format json _build
)
if echo "$EXPLICIT_RELATIVE" | "$JSONQ" no-diagnostics; then
  echo "  ok       anchors an explicit relative config to its real directory"
else
  echo "  WRONG    relative config path broke path overrides"; fails=$((fails+1))
fi

DISCOVERED_FROM_ROOT=$(
  cd "${TMPDIR:-/tmp}" &&
  "$LINTML" --no-color --profile pedantic --format json "$FIXTURE/_build"
)
if echo "$DISCOVERED_FROM_ROOT" | "$JSONQ" no-diagnostics; then
  echo "  ok       discovers config from an explicit scan root"
else
  echo "  WRONG    scan-root config discovery failed"; fails=$((fails+1))
fi

SUPPRESSED=$(
  "$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic \
    --report-suppressed --format json \
    "$FIXTURE/_build"
)
if echo "$SUPPRESSED" | "$JSONQ" suppressed-reason 101 "allocation identity is intended" &&
   echo "$SUPPRESSED" | "$JSONQ" suppressed-reason 114 "generated module intentionally checks identity"; then
  echo "  ok       reports expression and structure suppression reasons"
else
  echo "  WRONG    suppression audit output is incomplete"; fails=$((fails+1))
fi

BAD_CMT_DIR=$(mktemp -d)
TEMP_DIRS+=("$BAD_CMT_DIR")
printf 'not a cmt\n' > "$BAD_CMT_DIR/broken.cmt"
expect_exit 10 "$LINTML" lint "$BAD_CMT_DIR"

OUT_AGAIN=$("$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic --format json "$FIXTURE/_build")
if [ "$OUT" = "$OUT_AGAIN" ]; then
  echo "  ok       JSON output is byte-identical"
else
  echo "  WRONG    JSON output is not deterministic"; fails=$((fails+1))
fi

SARIF=$("$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic --format sarif "$FIXTURE/_build")
# The expected rule count comes from the registry rather than a literal, so
# adding a rule does not require editing this assertion.
RULE_COUNT=$("$LINTML" list-rules --profile pedantic | grep -c .)
if echo "$SARIF" | "$JSONQ" sarif-structure "$RULE_COUNT"; then
  echo "  ok       SARIF structure and rule metadata"
else
  echo "  WRONG    invalid SARIF output"; fails=$((fails+1))
fi

SARIF_SUPPRESSED=$(
  "$LINTML" --no-color --config "$TEST_CONFIG" --profile pedantic \
    --report-suppressed --format sarif \
    "$FIXTURE/_build"
)
if echo "$SARIF_SUPPRESSED" | "$JSONQ" sarif-suppressions; then
  echo "  ok       SARIF identifies in-source suppressions"
else
  echo "  WRONG    invalid SARIF suppression metadata"; fails=$((fails+1))
fi

echo
echo "new rules:"
expect compare-result-equality 184
expect_rule_clean compare-result-equality 185 "testing the sign is the correct form"
expect_rule_clean compare-result-equality 186 "= 0 is the one exact guarantee compare makes"

expect negative-list-index 189
expect_rule_clean negative-list-index 190 "a non-negative index is fine"

expect length-of-mapped-list 193
expect_rule_clean length-of-mapped-list 194 "length of the list itself is not wasteful"
expect_rule_clean length-of-mapped-list 195 "filter does not preserve length"

expect head-of-traversal 198
expect head-of-traversal 199
expect_rule_clean head-of-traversal 200 "plain List.hd is a different rule's business"

expect negated-condition 203
expect_rule_clean negated-condition 204 "no else branch to exchange"
expect_rule_clean negated-condition 205 "an unnegated condition is fine"

expect identity-map 208
expect_rule_clean identity-map 209 "a real transformation must not be reported"

expect redundant-string-sub 212
expect_rule_clean redundant-string-sub 213 "a real slice is not a whole-string copy"

expect exists-via-filter 216
expect_rule_clean exists-via-filter 217 "counting matches is a different question"
expect_rule_clean exists-via-filter 218 "List.exists is already the right call"
# A general rule must defer to the specific one so a single expression draws a
# single finding.
expect_rule_clean length-compare-zero 216 "exists-via-filter covers this expression"

echo
echo "rule overlap:"
# Every reported location must be attributable to exactly one rule.
if echo "$OUT" | "$JSONQ" no-overlap; then
  echo "  ok       no two rules fire on the same location"
else
  echo "  WRONG    overlapping rules produce duplicate findings"; fails=$((fails+1))
fi

echo
echo "physical/arithmetic rules:"
expect physical-assoc-lookup 254
expect_rule_clean physical-assoc-lookup 255 "List.assoc compares structurally"
expect_rule_clean physical-assoc-lookup 256 "an int key is immediate, so assq is sound"

expect truncated-int-division 259
expect_rule_clean truncated-int-division 260 "converting before dividing is correct"

expect length-of-append 263
expect_rule_clean length-of-append 264 "adding the lengths is the suggested form"

expect discarding-extractor 267
expect_rule_clean discarding-extractor 268 "Result.value ~default does not raise"
# Option.get belongs to partial-function; one expression, one finding.
expect_rule_clean discarding-extractor 255 "Option.get is covered by partial-function"

echo "safety rules:"
expect unsafe-cast 271
expect_rule_clean unsafe-cast 272 "an ordinary conversion is not a cast"
expect head-of-filter 275
expect_rule_clean head-of-filter 276 "returning all matches is a different question"
# head-of-filter explains the wasted traversal; partial-function must defer.
expect_rule_clean partial-function 275 "head-of-filter owns this expression"

echo "handled partiality:"
expect_rule_clean partial-function 279 "Not_found is caught, so the partiality is handled"
expect_rule_clean partial-function 280 "Failure is caught for List.hd"
expect partial-function 281
# Catching the wrong exception must not suppress: Hashtbl.find raises
# Not_found, not Failure.
expect partial-function 282

echo "length guards:"
expect_rule_clean partial-function 285 "length > 0 proves List.hd is in range"
expect_rule_clean partial-function 286 "length > 2 proves index 1 is in range"
expect_rule_clean partial-function 287 "0 < length reads the same as length > 0"
expect_rule_clean partial-function 288 "length <> 0 proves non-empty"
# The guard must check the arithmetic, not merely notice a nearby List.length:
# length > 1 does not prove index 5 is valid.
expect partial-function 289
expect partial-function 290
# Wrong direction and dynamic bounds prove nothing.
expect partial-function 291
expect partial-function 292

echo "match guards:"
expect partial-function 296
expect partial-function 299
expect partial-function 301
expect partial-function 303
expect_rule_clean partial-function 305 "a cons pattern proves the matched list is non-empty"

echo "comparison direction and cast boundaries:"
expect string-length-compare-empty 308
expect string-length-compare-empty 309
expect_hint 308 's <> ""'
expect_hint 309 's <> ""'
expect_rule_clean string-length-compare-empty 310 "a string length cannot be negative"
expect_rule_clean string-length-compare-empty 311 "zero cannot exceed a string length"
expect_rule_clean unsafe-cast 314 "Obj.repr safely erases a value to Obj.t"
expect partial-function 318
expect partial-function 320
expect_rule_clean discarding-extractor 320 "Option.get belongs to partial-function"
expect_rule_clean exists-equals-mem 324 "the target is recomputed for each item"
expect_rule_clean exists-equals-mem 325 "the comparison target is the bound item itself"
expect exists-via-filter 329
expect exists-via-filter 331
expect_hint 329 'not (List.exists'
expect_hint 331 'List.exists'
expect_rule_clean exists-via-filter 333 "zero cannot exceed a filtered-list length"
expect_rule_clean hashtbl-mem-then-find 337 "the second lookup is deferred"
expect_rule_clean ignored-result 344 "an unrelated type named result is not Stdlib.result"
expect_rule_clean option-match-to-combinator 346 "an unrelated type named option is not Stdlib.option"
expect trivial-sprintf 351
expect trivial-sprintf 352
expect trivial-sprintf 353
expect_rule_clean trivial-sprintf 354 "%f has different output from string_of_float"
expect_rule_clean trivial-sprintf 355 "a format with surrounding text is not a direct conversion"
expect_rule_clean trivial-sprintf 356 "a format with two arguments is not a direct conversion"
expect_rule_clean partial-function 362 "the nested module suppression applies inside the module"
expect partial-function 365
expect_rule_clean disabled-all-warnings 7 "the nested signature suppression applies inside the signature"
expect disabled-all-warnings 10

echo "guarded catch-all:"
expect_rule_clean swallowed-exception 368 "a fatal re-raise precedes the catch-all"
expect_rule_clean swallowed-exception 371 "the protected catch-all itself stays clean"
expect swallowed-exception 373
expect swallowed-exception 378

echo "degraded inputs:"
# A single unreadable artifact must not suppress the findings from every other
# file: it is a warning, and the run continues.
MIXED=$(mktemp -d)
cp -R "$FIXTURE/_build" "$MIXED/"
echo "not a cmt file" > "$MIXED/_build/broken.cmt"
MIXED_OUT=$("$LINTML" --no-color --fail-on never --format json "$MIXED/_build" 2>"$MIXED/err")
if echo "$MIXED_OUT" | "$JSONQ" has-diagnostics; then
  echo "  ok       findings survive an unreadable artifact"
else
  echo "  WRONG    an unreadable artifact suppressed all findings"; fails=$((fails+1))
fi
if echo "$MIXED_OUT" | "$JSONQ" load-error broken.cmt; then
  echo "  ok       JSON reports the unreadable artifact readably"
else
  echo "  WRONG    load_errors missing or unhelpful in JSON"; fails=$((fails+1))
fi
if grep -q 'warning: cannot read' "$MIXED/err"; then
  echo "  ok       the unreadable artifact is reported as a warning"
else
  echo "  WRONG    no warning for the unreadable artifact"; fails=$((fails+1))
fi
rm -rf "$MIXED"

# When nothing at all could be analysed, that is an error rather than silence.
ONLYBAD=$(mktemp -d)
echo "junk" > "$ONLYBAD/x.cmt"
"$LINTML" --no-color "$ONLYBAD" >/dev/null 2>&1
if [ "$?" -eq 10 ]; then
  echo "  ok       exit 10 when no artifact could be read"
else
  echo "  WRONG    expected exit 10 when nothing is analysable"; fails=$((fails+1))
fi
rm -rf "$ONLYBAD"

# The example config is shipped for users to copy, so it has to parse.
if "$LINTML" --config "$ROOT/lintml.toml.example" --fail-on never "$FIXTURE/_build" >/dev/null 2>&1; then
  echo "  ok       lintml.toml.example is valid configuration"
else
  echo "  WRONG    lintml.toml.example does not parse"; fails=$((fails+1))
fi

echo "repository invariants:"
# Each rule's module name must be its id with hyphens as underscores, so the
# file a reader opens matches the id lintml printed.
mismatches=0
for file in lib/rules/*.ml; do
  base=$(basename "$file" .ml)
  [ "$base" = registry ] && continue
  id=$(grep -oE 'id = "[a-z0-9-]+"' "$file" | head -1 | sed 's/id = "//;s/"//')
  [ -z "$id" ] && continue
  expected=$(echo "$id" | tr '-' '_')
  if [ "$base" != "$expected" ]; then
    echo "  WRONG    $base.ml declares rule '$id' (expected $expected.ml)"
    mismatches=$((mismatches+1))
  fi
done
if [ "$mismatches" -eq 0 ]; then
  echo "  ok       every rule module is named after its id"
else
  fails=$((fails+mismatches))
fi

# Every registered rule must have a fixture assertion, or it ships untested.
untested=0
for file in lib/rules/*.ml; do
  base=$(basename "$file" .ml)
  [ "$base" = registry ] && continue
  id=$(grep -oE 'id = "[a-z0-9-]+"' "$file" | head -1 | sed 's/id = "//;s/"//')
  [ -z "$id" ] && continue
  if ! grep -q "expect $id " "$0"; then
    echo "  WRONG    rule '$id' has no positive fixture assertion"
    untested=$((untested+1))
  fi
done
if [ "$untested" -eq 0 ]; then
  echo "  ok       every rule has a positive fixture assertion"
else
  fails=$((fails+untested))
fi

echo
if [ "$fails" -eq 0 ]; then echo "fixtures: PASS"; else echo "fixtures: $fails FAILURE(S)"; fi
exit $fails

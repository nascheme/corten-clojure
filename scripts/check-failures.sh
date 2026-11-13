#!/bin/sh
# Compare the failing assertions of a test run with a list of expected
# failures (used by cljs-tests.sh and clojure-test-suite.sh).
#
# usage: scripts/check-failures.sh EXPECTED OUTDIR NAME...
#   OUTDIR/NAME.out is the output of test file NAME (cljs.test of
#   clj/cljs/test.clj). Each failure is one line:
#     NAME | FAIL in <test> <contexts> | <expected form>
#   with ERROR for errors, LOAD for load errors, SKIP for skipped tests
#   or libs, and NO SUMMARY when the run did not finish.
#   Only the lines of the NAMEs given are compared.
#   Prints the new failures and the fixed ones; exit status 1 if there
#   are new failures.
#   UPDATE=1  replace the lines of the NAMEs in EXPECTED by this run's.
set -e
expected=$1
outdir=$2
shift 2

extract() {
    for n in "$@"; do
        f="$outdir/$n.out"
        if [ ! -f "$f" ] || ! grep -q '^Ran ' "$f"; then
            echo "$n | NO SUMMARY"
            continue
        fi
        awk -v N="$n" '
            function norm(s) {
                gsub(/__[0-9]+/, "__N", s)       # gensyms
                gsub(/[ \t]+/, " ", s)
                if (length(s) > 240) s = substr(s, 1, 240) "..."
                return s
            }
            /^(FAIL|ERROR) in/ {
                h = $0
                while ((getline l) > 0 && l !~ /^expected:/) {}
                sub(/^expected: */, "", l)
                print norm(N " | " h " | " l)
                next
            }
            /^LOAD ERROR in/ { print norm(N " | " $0) }
            # when-var-exists of the clojure-test-suite, and libs or
            # imports of the ns form that run-file skipped.
            /^SKIP - / { print norm(N " | " $0) }
            /^SKIPPED / { print norm(N " | " $0) }
        ' "$f"
    done
}

cur=$(mktemp)
exp=$(mktemp)
other=$(mktemp)
trap 'rm -f "$cur" "$exp" "$other"' EXIT
extract "$@" | LC_ALL=C sort > "$cur"
touch "$expected"
# Lines of EXPECTED for the NAMEs run ($exp) and for others ($other).
names=$(printf '%s ' "$@")
awk -v names="$names" -v other="$other" '
    BEGIN { n = split(names, a, " "); for (i = 1; i <= n; i++) run[a[i]] = 1 }
    { split($0, f, " [|] "); if (f[1] in run) print; else print > other }
' "$expected" | LC_ALL=C sort > "$exp"

if [ -n "$UPDATE" ]; then
    cat "$other" "$cur" | LC_ALL=C sort > "$expected"
    echo "updated $expected: $(wc -l < "$cur") expected failures for these files"
    exit 0
fi

new=$(LC_ALL=C comm -13 "$exp" "$cur")
fixed=$(LC_ALL=C comm -23 "$exp" "$cur")
if [ -n "$fixed" ]; then
    echo "FIXED (expected failures that now pass; UPDATE=1 to record):"
    echo "$fixed" | sed 's/^/  /'
fi
if [ -n "$new" ]; then
    echo "NEW FAILURES:"
    echo "$new" | sed 's/^/  /'
    exit 1
fi
echo "no new failures ($(wc -l < "$cur") expected failures)"

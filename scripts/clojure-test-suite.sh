#!/bin/sh
# Run jank's clojure-test-suite (portable .cljc tests of clojure.core,
# clojure.string, clojure.edn) with the small cljs.test of
# clj/cljs/test.clj, one process per file, and report the counts.
#
# usage: scripts/clojure-test-suite.sh [file-pattern...]
#   e.g. scripts/clojure-test-suite.sh abs assoc 'string_test/*'
#   (patterns are matched against paths under test/clojure, no extension)
#   SUITE  clojure-test-suite checkout
#          (default /home/nas/agent-home/src/clojure-test-suite)
#   KEEP   directory to keep the per-file output in (default: temporary)
#   UPDATE=1  record this run's failures as the expected ones
# The failing assertions are compared with tests/clojure-test-suite.expected
# (see scripts/check-failures.sh): the exit status is 1 if there are new
# failures.
set -e
here=$(cd "$(dirname "$0")/.." && pwd)
suite=${SUITE:-/home/nas/agent-home/src/clojure-test-suite}/test
work=${KEEP:-$(mktemp -d)}
mkdir -p "$work"

cargo build -q --release --manifest-path "$here/Cargo.toml"
lisp="$here/target/release/corten"
(cd "$here" && "$lisp" --boot clj/init.clj --save "$work/clj.img" > /dev/null)

files=$(cd "$suite/clojure" && find . -name '*.cljc' | sed 's|^\./||; s|\.cljc$||' | sort |
    grep -v -e '^core_test/portability$' -e '^core_test/number_range$')
if [ $# -gt 0 ]; then
    sel=
    for f in $files; do
        for p in "$@"; do
            case $f in $p|*/$p) sel="$sel $f"; break ;; esac
        done
    done
    files=$sel
fi

tests=0; pass=0; fail=0; error=0; lerr=0; crash=0
for f in $files; do
    out="$work/$f.out"
    mkdir -p "$(dirname "$out")"
    drv="$work/$f-driver.clj"
    cat > "$drv" <<EOF
(ns cts-driver (:require cljs.test))
(corten.kernel/add-load-path "$suite")
(cljs.test/run-file "$suite/clojure/$f.cljc")
(cljs.test/run-tests)
EOF
    st=0
    timeout 120 "$lisp" --image "$work/clj.img" --load "$drv" > "$out" 2>&1 || st=$?
    line=$(grep -A1 '^Ran ' "$out" | tr '\n' ' ')
    if [ -z "$line" ]; then
        crash=$((crash + 1))
        echo "$f: no summary (exit status $st)"
        continue
    fi
    set -- $line
    # Ran T tests containing A assertions. F failures, E errors, L load errors.
    t=$2; fl=$7; e=$9; l=${11}
    a=${5}
    p=$((a - fl - e))
    tests=$((tests + t)); pass=$((pass + p)); fail=$((fail + fl))
    error=$((error + e)); lerr=$((lerr + l))
    if [ "$fl" != 0 ] || [ "$e" != 0 ] || [ "$l" != 0 ] || [ "$t" = 0 ]; then
        echo "$f: $t tests, $p pass, $fl fail, $e error, $l load errors"
    fi
done
echo "total: $tests tests, $pass pass, $fail fail, $error error, $lerr load errors, $crash no summary"
st=0
"$here/scripts/check-failures.sh" "$here/tests/clojure-test-suite.expected" "$work" $files || st=$?
[ -n "$KEEP" ] || rm -rf "$work"
exit $st

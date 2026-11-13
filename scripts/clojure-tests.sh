#!/bin/sh
# Run Clojure's own tests (test/clojure/test_clojure/*.clj of a Clojure
# checkout) with the small cljs.test of clj/cljs/test.clj, one process
# per file, in parallel, and report the counts. Files that only test the
# JVM host (Java interop, gen-class, compilation, ...) are left out.
# tests/test_clojure is put first on the load path: a port of
# clojure.test-helper and jvm_names.clj, which names the JVM exception
# classes the tests catch after corten.lang's error types.
#
# usage: scripts/clojure-tests.sh [file-pattern...]   e.g. sequences 'pr*'
#   CLOJURE  Clojure checkout (default /home/nas/pkg/clojure)
#   TEST_CHECK  test.check checkout, put on the load path if present
#            (default: test.check in this repo)
#   KEEP     directory to keep the per-file output in (default: temporary)
#   JOBS     parallel processes (default: number of CPUs)
#   UPDATE=1 record this run's failures as the expected ones
# The failing assertions are compared with tests/clojure-tests.expected
# (see scripts/check-failures.sh): the exit status is 1 if there are new
# failures.
set -e
here=$(cd "$(dirname "$0")/.." && pwd)
test=${CLOJURE:-/home/nas/pkg/clojure}/test
tcheck=${TEST_CHECK:-$here/test.check}
work=${KEEP:-$(mktemp -d)}
jobs=${JOBS:-$(nproc)}
mkdir -p "$work"

cargo build -q --release --manifest-path "$here/Cargo.toml"
lisp="$here/target/release/corten"
(cd "$here" && "$lisp" --boot clj/init.clj --save "$work/clj.img" > /dev/null)

# JVM-only: Java interop and classes, the compiler, reflection,
# serialization, the socket server and main; run_single_test and
# ns_libs_load_later are helpers of other files.
files=$(cd "$test/clojure/test_clojure" && ls *.clj *.cljc | sed 's/\.cljc*$//' | grep -v -x \
    -e annotations -e array_symbols -e clearing -e compilation \
    -e data_structures_interop -e genclass -e 'generated_.*' \
    -e java_interop -e main -e method_thunks -e ns_libs_load_later \
    -e param_tags -e reflect -e run_single_test -e serialization \
    -e server -e streams)
if [ $# -gt 0 ]; then
    sel=
    for f in $files; do
        for p in "$@"; do
            case $f in $p) sel="$sel $f"; break ;; esac
        done
    done
    files=$sel
fi

for f in $files; do
    src=$(ls "$test/clojure/test_clojure/$f".clj* | head -1)
    cat > "$work/$f-driver.clj" <<DRV
(ns ct-driver (:require cljs.test))
(corten.kernel/add-load-path "$here/tests/test_clojure")
(corten.kernel/add-load-path "$test")
$([ -d "$tcheck/src/main/clojure" ] && echo "(corten.kernel/add-load-path \"$tcheck/src/main/clojure\")")
(load-file "$here/tests/test_clojure/jvm_names.clj")
(in-ns 'ct-driver)
(cljs.test/run-file "$src")
(cljs.test/run-tests)
DRV
done
echo $files | tr ' ' '\n' | xargs -P "$jobs" -I{} sh -c \
    'timeout 120 "$1" --image "$2/clj.img" --load "$2/$3-driver.clj" > "$2/$3.out" 2>&1 || true' \
    sh "$lisp" "$work" {}

tests=0; pass=0; fail=0; error=0; lerr=0; crash=0
for f in $files; do
    out="$work/$f.out"
    line=$(grep -A1 '^Ran ' "$out" | tr '\n' ' ')
    if [ -z "$line" ]; then
        crash=$((crash + 1))
        echo "$f: no summary"
        continue
    fi
    set -- $line
    # Ran T tests containing A assertions. F failures, E errors, L load errors.
    t=$2; a=$5; fl=$7; e=$9; l=${11}
    p=$((a - fl - e))
    tests=$((tests + t)); pass=$((pass + p)); fail=$((fail + fl))
    error=$((error + e)); lerr=$((lerr + l))
    echo "$f: $t tests, $p pass, $fl fail, $e error, $l load errors"
done
echo "total: $tests tests, $pass pass, $fail fail, $error error, $lerr load errors, $crash no summary"
st=0
"$here/scripts/check-failures.sh" "$here/tests/clojure-tests.expected" "$work" $files || st=$?
[ -n "$KEEP" ] || rm -rf "$work"
exit $st

#!/bin/sh
# Run ClojureScript test files (default: core_test) with the small
# cljs.test of clj/cljs/test.clj and report the counts. The
# top-level forms are evaluated one by one (cljs.test/run-file), so a
# form that fails to compile is counted as a load error and the run goes
# on; libs in the ns form that do not exist here are skipped. The
# reader takes the :cljs branches of .cljc files here (as well as
# :corten), unlike in user code.
#
# usage: scripts/cljs-tests.sh [name...]     e.g. core_test collections_test
#   (name.cljs, or name.cljc if there is no .cljs)
#   CLJS   ClojureScript checkout (default /home/nas/pkg/clojurescript)
#   TEST_CHECK  test.check checkout, put on the load path and loaded if
#          present, as in a cljs test build (default: test.check in this repo)
#   KEEP   directory to keep the per-file output in (default: temporary)
#   UPDATE=1  record this run's failures as the expected ones
# The failing assertions are compared with tests/cljs-tests.expected
# (see scripts/check-failures.sh): the exit status is 1 if there are new
# failures.
set -e
here=$(cd "$(dirname "$0")/.." && pwd)
cljs=${CLJS:-/home/nas/pkg/clojurescript}
tcheck=${TEST_CHECK:-$here/test.check}
names=${*:-core_test}
work=${KEEP:-$(mktemp -d)}
mkdir -p "$work"

cargo build -q --release --manifest-path "$here/Cargo.toml"
lisp="$here/target/release/corten"
(cd "$here" && "$lisp" --boot clj/init.clj --save "$work/clj.img" > /dev/null)

for n in $names; do
    src="$cljs/src/test/cljs/cljs/$n.cljs"
    [ -f "$src" ] || src="$cljs/src/test/cljs/cljs/$n.cljc"
    out="$work/$n.out"
    mkdir -p "$(dirname "$out")"
    cat > "$work/$n-driver.clj" <<EOF
(ns cljs-test-driver (:require cljs.test))
(corten.kernel/set-reader-features :corten :cljs :default)
(corten.kernel/add-load-path "$cljs/src/test/cljs")
$([ -d "$tcheck/src/main/clojure" ] && echo "(corten.kernel/add-load-path \"$tcheck/src/main/clojure\")
(require 'clojure.test.check 'clojure.test.check.properties)")
(cljs.test/run-file "$src")
(cljs.test/run-tests)
EOF
    timeout 600 "$lisp" --image "$work/clj.img" --load "$work/$n-driver.clj" \
        > "$out" 2>&1 || echo "$n: exit status $?"
    echo "$n: $(grep '^Ran ' "$out" || echo 'no summary')"
    grep -A1 '^Ran ' "$out" | tail -1
done
st=0
"$here/scripts/check-failures.sh" "$here/tests/cljs-tests.expected" "$work" $names || st=$?
[ -n "$KEEP" ] || rm -rf "$work"
exit $st

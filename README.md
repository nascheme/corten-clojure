# Corten Clojure

An experimental **Clojure dialect compiled to native x86-64 code on Linux**,
with a runtime and compiler written in Rust. No JVM, JavaScript engine, or
Common Lisp installation is required.

This branch started with a Rust port of the
[Corman Lisp](https://github.com/nascheme/cormanlisp-rs) C++ kernel. The Common
Lisp layer has since been removed. Corten Clojure builds on that kernel ancestry
as a native Clojure system; its executable is `corten`.

The core library is adapted from
[ClojureScript's self-hosted `cljs/core.cljs`](https://github.com/clojure/clojurescript):
persistent collections, sequences, and protocols are implemented in Clojure,
with Rust primitives replacing the JavaScript host. Other library code comes
from Clojure, ClojureScript, and SCI. This is not the official Clojure or
ClojureScript implementation, and it is not a drop-in replacement for either.

**Status:** a working, single-threaded system with a REPL, native compiler,
garbage collector, and saved images. It runs substantial portions of the
Clojure and ClojureScript test suites, but compatibility and host integration
are incomplete. Expect bugs and incompatible changes.

## Build and run

Requirements:

- x86-64 Linux; other architectures and operating systems are unsupported.
- A current stable Rust toolchain (edition 2024), Cargo, and a system C linker.

From the repository root:

```sh
cargo build --release

# Bootstrap the Clojure library and save the resulting heap image.
target/release/corten --boot clj/init.clj --save clj.img

# Start the Clojure REPL.
target/release/corten --image clj.img

# Evaluate forms without entering the REPL.
target/release/corten --image clj.img -e '(+ 1 2)'
# => 3

target/release/corten --image clj.img \
  -e '(mapv #(* % %) (range 5))'
# => [0 1 4 9 16]

# Load a program or an included benchmark.
target/release/corten --image clj.img --load bench/grail/float.cljc
```

All library sources needed for bootstrap are included. A separate Clojure or
ClojureScript checkout is needed only for the external test suites, not to
build or run the system. Keep the `clj/` sources available for loading libraries;
images do not embed all library sources. Run the examples from the repository
root.

### Command-line options

```text
corten [--image IMG] [--disasm] [--boot FILE] [--load FILE]
           [-e FORMS]... [--repl] [--save IMG]
```

- `--image IMG` restores an image and must be the first option.
- `--boot FILE` loads with the file's directory as the working directory;
  `clj/init.clj` builds the full system from the minimal kernel.
- `--load FILE` loads a source file without the bootstrap directory change.
- `-e FORMS` evaluates forms and prints the last value.
- `--repl` enters the REPL after loading or evaluating forms.
- `--save IMG` writes a heap image.
- `--disasm` dumps generated machine code.

Options after `--image` are processed in order. Without an image or bootstrap,
only the minimal kernel is available, not the full Clojure library. The REPL
supports `*1`, `*2`, `*3`, and `*e`.

Images contain implementation-specific compiled code and heap objects, not
portable bytecode. Rebuild `clj.img` after kernel/compiler changes; incompatible
kernel identifiers are rejected. File handles are not restored as open files.
Only load trusted source and images.

## Implementation and language direction

The compiler expands Clojure macros into an intermediate representation and
emits x86-64 machine code using `iced-x86`. Functions support multiple arities,
variadic arguments, closures, `loop`/`recur`, and exceptions. Vars are mutable
indirections, so redefining a function changes subsequent calls through its Var.

The runtime uses tagged 64-bit values and a mostly-copying garbage collector
with conservative stack roots. ClojureScript's collection types and protocol
implementations form the library base; the kernel also knows the collection
layouts needed by the reader and bootstrap.

The goal is **Clojure on its own native host**, with reader feature `:corten`.
Language semantics aim to follow JVM Clojure where they do not depend on Java;
host facilities live under `corten.*`, rather than promising JVM interop.
JavaScript/Closure compatibility remains internally to support the ported
ClojureScript library and is intended to shrink over time.

The project and executable are Corten Clojure and `corten`, respectively.

### Available functionality

- Persistent lists, vectors, maps and sets; lazy sequences, transients,
  higher-order sequence operations, and reducers.
- Macros, destructuring, namespaces, Vars, dynamic `binding`, metadata,
  `require`, `use`, `refer`, `alias`, and source loading.
- Protocols, `deftype`, records, `reify`, and multimethods.
- Longs, arbitrary-precision BigInts, exact ratios, doubles, and single floats.
  Long arithmetic checks overflow; promoting operations such as `+'` produce
  BigInts, and `unchecked-*` operations wrap. Bit operations use 64-bit Longs.
- Atoms, delays, volatiles, synchronous promises, and exception handling with
  `try`/`catch`/`finally` and `ex-info`.
- Clojure characters, typed arrays, regexes, string/reader/writer facilities,
  console I/O, and file input.
- Ported libraries including `clojure.string`, `set`, `walk`, `data`, `zip`,
  `template`, `edn`, `math`, and REPL helpers, plus `cljs.pprint` and portions
  of `cljs.spec.*`. Availability does not imply complete compatibility.

## Status and remaining work

Major gaps and known differences include:

- **BigDecimal:** not implemented. `M` literals currently read as doubles;
  `bigdec` and `with-precision` still need proper decimal support.
- **Identity and host semantics:** `identical?` still follows JavaScript-style
  value identity for strings and doubles, rather than pointer identity.
  Some string methods retain one-character-string conversions. JavaScript
  object/method emulation and `goog.*` compatibility need replacement with
  direct native host facilities.
- **Concurrency:** only one Lisp thread. No `future`, refs/STM, or agents.
  `tap>` invokes taps synchronously. Dereferencing an undelivered promise
  throws unless a timeout value is supplied.
- **Macros and protocols:** macros currently receive nil for `&env` and live
  in self-hosted ClojureScript-style `$macros` namespaces. Protocol reflection
  and `:extend-via-metadata` need work. `set!` of a Var outside `binding` can
  change its root, unlike JVM Clojure's binding restriction.
- **Diagnostics:** proper Clojure stack traces and compiler source-location
  reporting are missing. `clojure.stacktrace` prints errors and causes, not
  frames. Some kernel errors still have overly general types.
- **Reader and compatibility controls:** `*data-readers*` is missing;
  `*read-eval*`, `*warn-on-reflection*`, and `*unchecked-math*` do not currently
  control behavior. Java APIs and the JVM ecosystem are not provided.
- **Spec and generative testing:** some test.check generators assume JavaScript
  numbers, causing failures or hangs. Instrumenting core `next` with spec can
  recurse indefinitely. The spec port is not complete.
- **I/O and deployment:** the host API remains limited; library-source
  embedding in images is not implemented. This is not yet a standalone
  application packaging tool.
- **Memory management and performance:** no weak pointers, finalization, or
  return of free heap blocks to the OS. The identity-hash table currently
  retains hashed objects. Intermediate floating-point values are boxed;
  unboxing and a more efficient collector are future optimization work.
- **Printing:** Clojure-style `print-method` multimethod integration remains
  to be implemented.

The checked-in `tests/*.expected` files record known external-suite failures,
load errors, and skipped tests/libraries. They are the detailed regression
baseline, not evidence of full Clojure compatibility. JVM Clojure is the main
semantic reference; ClojureScript tests also exercise the implementation base,
but JavaScript-specific behavior is not always a compatibility goal.

## Tests

The self-contained Rust and integration tests require no upstream checkout:

```sh
cargo test

# Stress GC and verify heap pointers after every collection.
CORTEN_GC_THRESHOLD=4k CORTEN_GC_VERIFY=1 cargo test
```

External suites require upstream source checkouts, a POSIX shell, and common
Linux utilities including `timeout` (`nproc` and `xargs` for parallel runs).
Override the scripts' developer-local default paths:

```sh
# Clojure's tests, excluding JVM-only files.
CLOJURE=/absolute/path/to/clojure scripts/clojure-tests.sh
CLOJURE=/absolute/path/to/clojure scripts/clojure-tests.sh sequences 'pr*'

# ClojureScript tests (core_test by default).
CLJS=/absolute/path/to/clojurescript scripts/cljs-tests.sh core_test

# jank-lang/clojure-test-suite: portable Clojure tests.
SUITE=/absolute/path/to/clojure-test-suite scripts/clojure-test-suite.sh
```

Sources: [Clojure](https://github.com/clojure/clojure),
[ClojureScript](https://github.com/clojure/clojurescript), and
[clojure-test-suite](https://github.com/jank-lang/clojure-test-suite).
The portable-suite baseline requires a locally adapted checkout with `:corten`
reader branches where the default branches use Java classes; an unmodified
upstream checkout may produce additional failures.

Clojure and ClojureScript suite runners use an optional
[test.check](https://github.com/clojure/test.check) checkout at `test.check/`,
or the path in `TEST_CHECK`. External tests are not vendored. Upstream revisions
can change results.

The runners build a release image and compare results with `tests/*.expected`.
They exit nonzero for failures not in the baseline; a successful run does not
mean every assertion passed. `KEEP=/absolute/path` retains output.
`UPDATE=1` rewrites the baseline and should be used only after reviewing changes.

## License

This is a **mixed-license project**. Original independent work is MIT-licensed;
Corman Lisp's MIT notice is retained for kernel-derived material. Clojure,
ClojureScript, SCI-derived code, and the test.check RNG port retain EPL 1.0;
`clojure.repl` retains CPL 1.0. Closure-derived code carries Apache 2.0 notices,
and benchmark translations and Rust dependencies retain their own notices.

See [LICENSE](LICENSE.txt) for full license texts, attribution, and scope.
Modified upstream material is not relicensed as MIT. Distributing binaries or
saved images containing EPL/CPL-covered code also requires making the
corresponding covered source available under its license and telling recipients
how to obtain it; retain the license and copyright notices with
redistributions.

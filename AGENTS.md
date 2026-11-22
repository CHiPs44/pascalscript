# AGENTS.md

## Preliminary statement about LLM / AI usage

This project tries to keep LLM / AI usage "reasonable", i.e. no part of it should be "vibe coded" autonomously by an agent.

Main use of LLM / AI is to refactor code, develop tedious simple tasks, give advice, plans and code reviews.

## Project overview

PascalScript is a C23 implementation of an interpreted Turbo Pascal-inspired dialect.

Source files are lexed and parsed into an AST, then interpreted. The public entry
point is `pascalscript.c`; core implementation is in `src/`, declarations are in
`include/`, C tests are in `test/`, Pascal examples are in `examples/`, and design
notes are in `doc/`.

This repository is LGPL-3.0-or-later. Preserve existing SPDX headers in C and Pascal
files and add them to new source files.

## Read before changing code

- Start with `README.md`, `STATUS.md`, and `TODO.md` for project direction.
- Read `doc/AST.md` for the compilation and interpretation model.
- Read `doc/CLI.md` for CLI behavior.
- Keep parser, AST, interpreter, and public declaration changes consistent across
  `src/`, `include/`, and relevant tests.
- Search for existing helpers before adding new allocation, string, symbol, AST, or
  error-handling logic. This codebase has dedicated `ps_*` modules for these areas.

## Build and test

Use CMake out-of-tree. GCC is the default; separate GCC and Clang builds are useful
for checking both supported toolchains.

```sh
cmake -S . -B build -DCMAKE_C_COMPILER=gcc -DCMAKE_BUILD_TYPE=Debug
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

For Clang:

```sh
cmake -S . -B build-clang -DCMAKE_C_COMPILER=clang -DCMAKE_BUILD_TYPE=Debug
cmake --build build-clang --parallel
ctest --test-dir build-clang --output-on-failure
```

Run one example directly:

```sh
./build/pascalscript examples/410-circle.pas
```

Useful CLI diagnostics include `-n` for parse-only, `-t` for trace, `-d` for debug,
`-s` for symbols, `-u` for source-buffer dumping, and `-m` for memory usage.

For the full CMake/CTest suite, including Pascal example executables, use:

```sh
ctest --test-dir build --output-on-failure
```

CMake registers only Pascal example files that are executable. When adding or restoring
an example in CMake tests, preserve or set the executable bit rather than relying on
a build-generated wrapper.

Optional memory checks:

```sh
ctest --test-dir build -T memcheck --output-on-failure
cmake -S . -B build-asan -DCMAKE_BUILD_TYPE=Debug -DENABLE_ASAN=ON
cmake --build build-asan --parallel
ASAN_OPTIONS=detect_leaks=1 ctest --test-dir build-asan --output-on-failure
```

The legacy `test-tests.sh` and `test-examples.sh` scripts require additional local
toolchains and 32-bit support; prefer CMake/CTest for the normal validation loop.

## C coding conventions

- The project requires C23 and builds with `-Wall -Wextra -Wpedantic`.
- Format C with `.clang-format`; use Allman braces, four-space indentation, 120-column
  lines, and right-aligned pointers.
- Preserve the module naming pattern: public names begin with `ps_` and source/header
  pairs live in `src/` and `include/`.
- Check allocation and I/O failure paths; do not silently discard errors.
- Keep AST common fields, kind/group checks, casts, and corresponding free functions
  consistent.
- Use assertions for invariant violations, but return explicit errors for invalid
  user source or runtime input.
- Keep changes in `src/` and `include/` synchronized. Do not use casts or unrelated
  refactoring to suppress warnings.

## Pascal and example conventions

- Pascal sources use four-space indentation and LF line endings.
- Keep `Program ...;` and the required final `End.` syntax.
- Curly-brace `{}` comments, `(* *)` comments, and `//` comments are supported; use
  SPDX comments only where license metadata is required.
- Examples should remain compilable with Free Pascal in default FPC mode:
  `fpc -MFPC source.pas`.
- Example filenames and tests are discovered automatically by CMake. Do not change
  filename numbering or executable bits unless the change intentionally affects test
  discovery.
- Test behavior through both the interpreter and, where relevant, FPC compatibility.

## Tests and validation

- Add or update C tests for lexer, parser, AST, value, stack, symbol-table, and
  memory-manager behavior in `test/test_*.c`.
- Add focused Pascal example programs or executable CTest cases when syntax or runtime
  behavior changes. Prefer a small reproducer over a broad fixture.
- CTest failures may be detected by a `FAILED!` marker in test output, as configured
  by CMake.
- At minimum, run the normal CMake build and CTest suite after code changes.
- Run AddressSanitizer or Valgrind for memory ownership, lifetime, or allocation
  changes.
- Do not include build directories or generated reports in source changes. `build/`
  and `build-*` are ignored.

## Change hygiene

- Make surgical changes and preserve existing behavior outside the requested scope.
- Add tests for behavior introduced or fixed; do not use tests that merely restate
  constants or implementation details.
- Inspect `git --no-pager diff` before finalizing; do not revert unrelated work.
- Keep generated `build/include/ps_version.h` out of commits; edit `VERSION` or
  `include/ps_version.h.in` instead.

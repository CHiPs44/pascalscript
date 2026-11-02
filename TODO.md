# TODO

## Work in progress:

- Documentation: README, STATUS, ...
- Bug fixes

## More advanced features:

- **String**
  - Individual character access with `[]`
  - Memory management handling with garbage collection or reference counting
  - Perhaps handle "unlimited" strings and make classic Pascal usage transparent
- **Statement** - `Case` ... `Of` ... `Else` / `Otherwise`
- **Records** - Record/struct type definitions
- **Pointers and Heap** - Pointer type and operations:
  - `Pointer`, `New`, `Dispose`, `^`
  - Maybe we need to handle `Forward` declarations?
- **Sets** - Set type and operations
- **Files** - File I/O operations
  - `File`, `Text`, `Assign`, `Rewrite`, `Reset`, `Close`, `Read`, `Write`, `Seek`, `Eof`, `Eoln`
- **Units** - Module/unit system
  - this time `Forward` declarations will be mandatory as `Interface` is essentially a bunch of forward declarations for `Implementation`
- Additional system procedures:
  - `Read`, `ReadLn`, `ReadStr`, `Delay`, `Exit`, `Halt`, `FillChar`, `Move`
- Memory access arrays:
  - `Mem`, `MemW`

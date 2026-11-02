# Command-line options

The command-line options are defined in `pascalscript.c`. To see the options
supported by the current build, run:

```sh
pascalscript --help
```

As of 2026-11-02, this outputs:

```text
PascalScript v0.1.2.3 (32 bits) - License: LGPL 3.0 or later, see LICENSE file
Usage: ./build-clang/pascalscript [-t] [-d] [-s] [-b] [-v] [program_file]
  program_file : path to the Pascal source file to run
Runtime options:
 -b : flips short circuit boolean evaluation (default: false, {$B-})
 -i : flips I/O error checking (default: true, ${I+})
 -r : flips range checking (default: true, {$R+})
Other options:
 -a : launch AST tests
 -c : display configuration and exit
 -d : debug (more verbose trace)
 -h : display this help message and exit
 -m : display memory usage at end
 -n : do not execute program, just parse source code
 -s : dump symbols at initialization and termination
 -t : trace execution
 -u : dump source buffer after loading
 -v : verbose (display banner and other infos)
 ```

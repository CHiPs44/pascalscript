#!/bin/sh

bison -rall -o pascalscript.tab.c pascalscript.y && \
flex  -v    -o pascalscript.lex.c pascalscript.l && \
LANG=C gcc -o pascalscript error.c lexer.c operator.c pascalscript.c pascalscript.lex.c pascalscript.tab.c runtime_error.c symbol.c symbol_stack.c symbol_table.c vm.c

# EOF

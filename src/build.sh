#!/bin/sh

make

# rm -f pascalscript pascalscript.tab.* pascalscript.lex.* pascalscript.output
# bison       -rall -d -o pascalscript.tab.c pascalscript.y
# flex                 -o pascalscript.lex.c pascalscript.l
# LANG=C gcc  -Wall    -o pascalscript error.c lexer.c operator.c pascalscript.c pascalscript.lex.c pascalscript.tab.c runtime_error.c symbol.c symbol_stack.c symbol_table.c vm.c

# EOF

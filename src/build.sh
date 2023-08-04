#!/bin/sh

flex  -v    -o pascalscript.lex.c pascalscript.l && \
bison -rall -o pascalscript.tab.c pascalscript.y && \
gcc -o pascalscript *.c
#pascalscript.lex.c pascalscript.tab.c lexer.c error.c

# EOF

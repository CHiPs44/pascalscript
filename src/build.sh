#!/bin/sh

bison -rall -o pascalscript.tab.c pascalscript.y && \
flex  -v    -o pascalscript.lex.c pascalscript.l && \
LANG=C gcc -o pascalscript *.c
#pascalscript.lex.c pascalscript.tab.c lexer.c error.c

# EOF

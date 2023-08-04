CFLAGS=-W -Wall -ansi -pedantic -std=c17 -g

all: pascalscript

pascalscript: pascalscript.lex.c pascalscript.tab.c lexer.c error.c
	LANG=C gcc $(CFLAGS) -o pascalscript pascalscript.lex.c pascalscript.tab.c lexer.c error.c

pascalscript.tab.c,pascalscript.tab.h: pascalscript.y
	bison -rall -opascalscript.tab.c -d pascalscript.y

pascalscript.lex.c: pascalscript.l pascalscript.tab.h
	flex -opascalscript.lex.c pascalscript.l

# EOF

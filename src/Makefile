# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

CFLAGS=-W -Wall -ansi -pedantic -std=c17 -g

all: pascalscript

pascalscript: pascalscript.lex.c pascalscript.tab.c lexer.c error.c
	LANG=C gcc $(CFLAGS) -o pascalscript pascalscript.lex.c pascalscript.tab.c lexer.c error.c

pascalscript.tab.c,pascalscript.tab.h: pascalscript.y
	bison -rall -opascalscript.tab.c -d pascalscript.y

pascalscript.lex.c: pascalscript.l pascalscript.tab.h
	flex -opascalscript.lex.c pascalscript.l

# EOF

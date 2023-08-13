# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

CC = LANG=C gcc
# CFLAGS = -W -Wall -ansi -pedantic -std=c17 -g
CFLAGS = -Wall
PROJECT = pascalscript
SOURCES = \
error.c lexer.c operator.c pascalscript.c runtime_error.c \
symbol.c symbol_stack.c symbol_table.c vm.c \
pascalscript.lex.c pascalscript.tab.c

all: $(PROJECT)

$(PROJECT): $(SOURCES)
	echo $(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES)

# TODO seems *.y is built by a default rule as a call to yacc that replace pascalscript.c

pascalscript.tab.c: pascalscript.y
	echo bison -rall --output=pascalscript.tab.c --header=pascalscript.tab.h pascalscript.y

pascalscript.lex.c: pascalscript.l pascalscript.tab.c
	echo flex --outfile=pascalscript.lex.c pascalscript.l

clean:
	echo rm -f $(PROJECT) pascalscript.lex.c pascalscript.tab.c pascalscript.tab.h pascalscript.output

# EOF

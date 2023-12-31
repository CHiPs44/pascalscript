# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2023 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

CC = LANG=C gcc
# CFLAGS = -W -Wall -ansi -pedantic -std=c17 -g
CFLAGS = -Wall
PROJECT = pascalscript
SOURCES = \
error.c lexer.c operator.c pascalscript.c \
symbol.c symbol_stack.c symbol_table.c vm.c \
pascalscript.lex.c pascalscript.tab.c
INCLUDES = \
error.h lexer.h operator.h pascalscript.h \
symbol.h symbol_stack.h symbol_table.h vm.h \
pascalscript.tab.h

all: $(PROJECT)

$(PROJECT): $(SOURCES) $(INCLUDES)
	$(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES)

# *.y is built by a default rule as a call to yacc/bison that replace pascalscript.c...
%.c: %.y
# same for flex
%.c: %.l

pascalscript.tab.c: pascalscript.y
	# bison -rall --output=pascalscript.tab.c --header=pascalscript.tab.h pascalscript.y
	bison -rall --output=pascalscript.tab.c --defines=pascalscript.tab.h pascalscript.y

pascalscript.lex.c: pascalscript.l pascalscript.tab.c
	flex --outfile=pascalscript.lex.c pascalscript.l

clean:
	rm -f $(PROJECT) pascalscript.lex.c pascalscript.tab.c pascalscript.tab.h pascalscript.output

# EOF

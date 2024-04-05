# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

CC = LANG=C gcc
# CFLAGS = -W -Wall -ansi -pedantic -std=c17 -g
CFLAGS = -Wall
PROJECT = pascalscript
SOURCES = \
error.c lexer.c operator.c pascalscript.c \
source.c symbol.c symbol_stack.c symbol_table.c vm.c
INCLUDES = \
error.h lexer.h operator.h pascalscript.h \
symbol.h symbol_stack.h symbol_table.h vm.h

all: $(PROJECT)

$(PROJECT): $(SOURCES) $(INCLUDES)
	$(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES)

# clean:
# 	rm -f $(PROJECT) pascalscript.lex.c pascalscript.tab.c pascalscript.tab.h pascalscript.output

# EOF

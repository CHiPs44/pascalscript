# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

CC = LANG=C gcc
# CFLAGS = -W -Wall -ansi -pedantic -std=c17 -g
CFLAGS   = -Wall -Iinclude
PROJECT  = pascalscript
SOURCES  = \
	src/pascalscript.c src/ps_error.c src/ps_lexer.c src/ps_operator.c \
	src/ps_parser.c src/ps_readall.c src/ps_source.c src/ps_symbol.c src/ps_symbol_stack.c \
	src/ps_symbol_table.c src/ps_token.c src/ps_vm.c
INCLUDES = \
	include/ps_config.h include/ps_error.h include/ps_lexer.h include/ps_operator.h \
	include/ps_parser.h include/ps_readall.h include/ps_buffer.h include/ps_symbol.h include/ps_symbol_stack.h \
	include/ps_symbol_table.h include/ps_token.h include/ps_vm.h

all: $(PROJECT)

$(PROJECT): $(SOURCES) $(INCLUDES)
	$(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES)

clean:
	rm -f $(PROJECT)

# EOF

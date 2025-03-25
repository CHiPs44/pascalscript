# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

CC = LANG=C gcc
# CFLAGS = -W -Wall -ansi -pedantic -std=c17 -g
CFLAGS   = -Wall -Iinclude -ggdb
PROJECT  = pascalscript
SOURCES  = $(wildcard src/*.c)
INCLUDES = $(wildcard include/*.h)

all: $(PROJECT)

$(PROJECT): $(SOURCES) $(INCLUDES)
	$(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES)

clean:
	rm -f $(PROJECT)

# EOF

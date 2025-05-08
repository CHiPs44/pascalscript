# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: LGPL-3.0-or-later

CC = LANG=C gcc
# CFLAGS   = -std=c17 -Wall -Iinclude -ggdb
CFLAGS   = -std=c17 -Wall -Iinclude -ggdb -m32
CLIBS    = -lm
PROJECT  = pascalscript
SOURCES  = $(wildcard src/*.c)
INCLUDES = $(wildcard include/*.h)

all: $(PROJECT)

$(PROJECT): $(SOURCES) $(INCLUDES)
	$(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES) $(CLIBS)

clean:
	rm -f $(PROJECT)

# EOF

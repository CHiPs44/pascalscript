# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: LGPL-3.0-or-later

# x86 32 bits:
# (requires sudo apt install gcc-multilib g++-multilib) 
CC		= LANG=C gcc
#CFLAGS	= -std=c17 -Wall -Iinclude -ggdb
CFLAGS	= -std=c17 -Wall -Iinclude -ggdb -m32

# ARM 32 bits:
# (requires apt install qemu-user gcc-arm-linux-gnueabi)
# (and export QEMU_LD_PREFIX=/usr/arm-linux-gnueabi or -static)
# (incompatible with gcc-multilib / g++-multilib)
# CC		= arm-linux-gnueabi-gcc
#CC		= arm-none-eabi-gcc
# CFLAGS	= -static -std=c17 -Wall -Iinclude -ggdb

CLIBS    = -lm
PROJECT  = pascalscript
SOURCES  = $(wildcard src/*.c)
INCLUDES = $(wildcard include/*.h)

all: $(PROJECT)

$(PROJECT): $(SOURCES) $(INCLUDES)
	/bin/echo "Compiling $(PROJECT) with $(CC) using flags: $(CFLAGS)"
	/bin/echo "Source files: $(SOURCES)"
	/bin/echo "Include files: $(INCLUDES)"
	$(CC) $(CFLAGS) -o $(PROJECT) $(SOURCES) $(CLIBS)

clean:
	rm -f $(PROJECT)

# EOF

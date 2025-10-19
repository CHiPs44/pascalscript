# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
# SPDX-License-Identifier: LGPL-3.0-or-later

# x86 32 bits:
# (requires sudo apt install gcc-multilib g++-multilib)
CC		= LANG=C gcc
# CFLAGS	= -std=c17 -Wall -Iinclude -ggdb
# CFLAGS	= -m32 -std=c17 -Wall -Iinclude -ggdb
CFLAGS	= -m32 -std=c17 -Wall -Iinclude -ggdb -O3
# CFLAGS	= -m32 -std=c17 -Wall -Iinclude -ggdb -O3 -fsanitize=address -fsanitize=leak -static-libasan 

# ARM 32 bits:
# (requires apt install qemu-user gcc-arm-linux-gnueabi)
# (and export QEMU_LD_PREFIX=/usr/arm-linux-gnueabi or -static)
# (incompatible with gcc-multilib / g++-multilib)
#CC		= arm-linux-gnueabi-gcc
#CC		= arm-none-eabi-gcc
#CFLAGS	= -static -std=c17 -Wall -Iinclude -ggdb

CLIBS    = -lm
PROJECT  = pascalscript
SOURCES  = $(wildcard src/*.c)
INCLUDES = $(wildcard include/*.h)

DEPDIR := .deps
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d
DEPFILES := $(SOURCES:src/%.c=$(DEPDIR)/%.d)
#$(shell echo DEPFILES is ${DEPFILES})

$(DEPFILES):
	include $(wildcard $(DEPFILES))

$(PROJECT): obj/%.o
	echo $(CC) $(CFLAGS) -o $@ -c $< $(CLIBS)

%.o : %.c
%.o : %.c $(DEPDIR)/%.d | $(DEPDIR)
# 	$(COMPILE.c) $(OUTPUT_OPTION) $<
	$(CC) $(CFLAGS) -c $< -o $@

# src/%.d: src/%.c $(INCLUDES)
# 	$(CC) -MM $(CFLAGS) $(SOURCES) > $@

$(DEPDIR): ; @mkdir -p $@

all: $(PROJECT)

clean:
	rm -f $(PROJECT)
	rm -f obj/*.o
	rm -f ${DEPDIR}/*.d

obj/%.o: src/%.c ${DEPDIR}/%.d
	$(CC) $(CFLAGS) -c $< -o $@

# EOF

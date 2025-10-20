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

CLIBS    = -lm

PROJECT  = pascalscript
SRCDIR   = src
INCDIR   = include
OBJDIR   = .obj
DEPDIR   = .deps

SOURCES  = $(wildcard $(SRCDIR)/*.c)
OBJECTS  = $(SOURCES:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
DEPFILES = $(SOURCES:$(SRCDIR)/%.c=$(DEPDIR)/%.d)

# dependency flags: generate .d files in $(DEPDIR) and set proper target
DEPFLAGS = -MMD -MP -MF $(DEPDIR)/$*.d -MT $@

# include dependency files (skip when running clean)
ifneq ($(MAKECMDGOALS),clean)
-include $(wildcard $(DEPFILES))
endif

.PHONY: all clean test dirs

all: $(PROJECT)

# link
$(PROJECT): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(CLIBS)

# compile: create object and dependency file
$(OBJDIR)/%.o: $(SRCDIR)/%.c | dirs
	$(CC) $(CFLAGS) $(DEPFLAGS) -c $< -o $@

# fallback generic rule
%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

dirs:
	@mkdir -p $(OBJDIR) $(DEPDIR)

test:
	@echo "SOURCES: $(SOURCES)"
	@echo "OBJECTS: $(OBJECTS)"
	@echo "DEPFILES: $(DEPFILES)"

clean:
	-rm -f $(PROJECT)
	-rm -f $(OBJDIR)/*.o
	-rm -f $(DEPDIR)/*.d

# EOF

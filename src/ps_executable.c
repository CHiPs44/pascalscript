/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_executable.h"
#include "ps_memory.h"

ps_executable *ps_executable_alloc(ps_formal_signature *signature, uint16_t line, uint16_t column)
{
    ps_executable *executable = ps_memory_malloc(sizeof(ps_executable));
    if (executable == NULL)
        return NULL;
    executable->address = NULL;
    executable->formal_signature = signature;
    executable->line = line;
    executable->column = column;
    return executable;
}

ps_executable *ps_executable_free(ps_executable *executable)
{
    if (executable == NULL)
        return NULL;
    if (executable->formal_signature != NULL)
        executable->formal_signature = ps_formal_signature_free(executable->formal_signature);
    ps_memory_free(executable);
    return NULL;
}

void ps_executable_debug(FILE *output, char *message, ps_executable *executable)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "%s: Executable at %p:\n", message, (void *)executable);
    if (executable == NULL)
        return;
    fprintf(output, "\tSignature: %p\n", (void *)executable->formal_signature);
    fprintf(output, "\tLine: %u, Column: %u\n", executable->line, executable->column);
}

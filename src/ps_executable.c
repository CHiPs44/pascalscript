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

bool ps_executable_is_user_defined(ps_executable_kind kind)
{
    return kind == PS_EXECUTABLE_PROC_USER || kind == PS_EXECUTABLE_FUNC_USER;
}

bool ps_executable_is_system(ps_executable_kind kind)
{
    return !ps_executable_is_user_defined(kind);
}

ps_executable *ps_executable_alloc(ps_executable_kind kind, ps_ast_block *block)
{
    ps_executable *executable = ps_memory_malloc(PS_MEMORY_EXECUTABLE, sizeof(ps_executable));
    if (executable == NULL)
        return NULL;
    executable->kind = kind;
    executable->block = block;
    return executable;
}

ps_executable *ps_executable_free(ps_executable *executable)
{
    if (executable == NULL)
        return NULL;
    if (ps_executable_is_system(executable->kind))
        return NULL;
    executable->block = ps_ast_free_block(executable->block);
    ps_memory_free(PS_MEMORY_EXECUTABLE, executable);
    return NULL;
}

char *ps_executable_get_kind_name(ps_executable_kind kind)
{
    static char name[16];

    switch (kind)
    {
    case PS_EXECUTABLE_FUNC_1ARG:
        return "FUNC_1ARG";
    case PS_EXECUTABLE_FUNC_1ARG_S:
        return "FUNC_1ARG_S";
    case PS_EXECUTABLE_FUNC_2ARGS:
        return "FUNC_2ARGS";
    case PS_EXECUTABLE_PROC_1ARG:
        return "PROC_1ARG";
    case PS_EXECUTABLE_PROC_FILE_WRITE:
        return "FILE_WRITE";
    case PS_EXECUTABLE_PROC_FILE_READ:
        return "FILE_READ";
    case PS_EXECUTABLE_FUNC_USER:
        return "FUNCTION";
    case PS_EXECUTABLE_PROC_USER:
        return "PROCEDURE";
    default:
        snprintf(name, 15, "???%d???", kind);
    }
}

void ps_executable_debug(FILE *output, char *message, ps_executable *executable)
{
    if (output == NULL)
        output = stderr;
    fprintf(output, "%s: Executable at %p:\n", message, (void *)executable);
    if (executable == NULL)
        return;
    fprintf(output, "\tKind: %s\n", ps_executable_get_kind_name(executable->kind));
    if (ps_executable_is_user_defined(executable->kind))
        fprintf(output, "\tName: %s\n", executable->block->name);
}

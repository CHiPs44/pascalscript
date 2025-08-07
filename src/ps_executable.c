/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_executable.h"

ps_executable *ps_executable_alloc(ps_signature *signature, ps_type_definition *return_type, uint16_t line,
                                   uint16_t column)
{
    ps_executable *executable = malloc(sizeof(ps_executable));
    if (executable == NULL)
        return NULL;
    executable->signature = signature;
    executable->return_type = return_type;
    executable->line = line;
    executable->column = column;
    return executable;
}

ps_executable *ps_executable_free(ps_executable *executable)
{
    if (executable == NULL)
        return NULL;
    if (executable->signature != NULL)
        ps_signature_done(executable->signature);
    // if (executable->return_type != NULL)
    //     ps_type_definition_free(executable->return_type);
    free(executable);
    return NULL;
}

void ps_executable_debug(FILE *output, char *message, ps_executable *executable)
{
    if (output == NULL)
        output= stderr;
    fprintf(output, "%s: Executable at %p:\n", message, (void *)executable);
    fprintf(output, "  Signature: %p\n", (void *)executable->signature);
    // if (executable->signature != NULL)
    //     ps_signature_debug(output, "  Signature", executable->signature);
    fprintf(output, "  Return type: %p\n", (void *)executable->return_type);
    if (executable->return_type != NULL)
        ps_type_definition_debug(output, "  Return type", executable->return_type);
    fprintf(output, "  Line: %u, Column: %u\n", executable->line, executable->column);
}

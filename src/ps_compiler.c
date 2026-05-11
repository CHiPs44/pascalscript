/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_compiler.h"
#include "ps_environment.h"
#include "ps_error.h"
#include "ps_memory.h"
#include "ps_parser.h"
#include "ps_string_heap.h"
#include "ps_value.h"

#include "ps_compiler.h"

ps_compiler *ps_compiler_alloc(bool range_check, bool bool_eval, bool io_check)
{
    ps_compiler *compiler = ps_memory_malloc(PS_MEMORY_INTERPRETER, sizeof(ps_compiler));
    compiler->parser = NULL;
    compiler->string_heap = NULL;
    compiler->level = 0;
    compiler->error = PS_ERROR_NONE;
    compiler->debug = COMPILER_DEBUG_NONE;
    compiler->range_check = range_check;
    compiler->bool_eval = bool_eval;
    compiler->io_check = io_check;
    for (size_t i = 0; i < PS_COMPILER_ENVIRONMENTS; i++)
        compiler->environments[i] = NULL;
    return compiler;
}

ps_compiler *ps_compiler_free(ps_compiler *compiler)
{
    ps_memory_free(PS_MEMORY_INTERPRETER, compiler);
    return NULL;
}

bool ps_compiler_return_false(ps_compiler *compiler, ps_error error)
{
    compiler->error = error;
    return false;
}

void *ps_compiler_return_null(ps_compiler *compiler, ps_error error)
{
    compiler->error = error;
    return NULL;
}

bool ps_compiler_set_message(ps_compiler *compiler, char *format, ...) // NOSONAR
{
    va_list args;
    va_start(args, format);
    vsnprintf(compiler->message, sizeof(compiler->message), format, args); // NOSONAR
    va_end(args);
    return false;
}

bool ps_compiler_enter_environment(ps_compiler *compiler, ps_identifier name)
{
    (void)compiler;
    (void)name;
    return false;
}

bool ps_compiler_exit_environment(ps_compiler *compiler)
{
    (void)compiler;
    return false;
}

ps_environment *ps_compiler_get_environment(ps_compiler *compiler)
{
    (void)compiler;
    return NULL;
}

ps_symbol *ps_compiler_find_symbol(ps_compiler *compiler, const char *name, bool local)
{
    (void)compiler;
    (void)name;
    (void)local;
    return NULL;
}

bool ps_compiler_add_symbol(ps_compiler *compiler, ps_symbol *symbol)
{
    (void)compiler;
    (void)symbol;
    return false;
}

bool ps_compiler_add_variable(ps_compiler *compiler, const ps_identifier identifier, ps_symbol *type_symbol)
{
    (void)compiler;
    (void)identifier;
    (void)type_symbol;
    return false;
}

bool ps_compiler_is_number(ps_compiler *compiler, ps_value *value)
{
    (void)compiler;
    (void)value;
    return false;
}

bool ps_compiler_is_ordinal(ps_compiler *compiler, ps_value *value)
{
    (void)compiler;
    (void)value;
    return false;
}

bool ps_compiler_copy_value(ps_compiler *compiler, ps_value *from, ps_value *to)
{
    (void)compiler;
    (void)from;
    (void)to;
    return false;
}

bool ps_compiler_load_string(ps_compiler *compiler, char *source, size_t length)
{
    (void)compiler;
    (void)source;
    (void)length;
    return false;
}

bool ps_compiler_load_file(ps_compiler *compiler, const char *filename)
{
    (void)compiler;
    (void)filename;
    return false;
}

bool ps_compiler_run(ps_compiler *compiler)
{
    (void)compiler;
    return false;
}

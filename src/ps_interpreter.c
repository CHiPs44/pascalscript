/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_parser.h"
#include "ps_system.h"
#include "ps_value.h"

#include "ps_interpreter.h"
#include "ps_visitor.h"

ps_interpreter *ps_interpreter_init()
{
    ps_interpreter *interpreter = calloc(1, sizeof(ps_interpreter));
    if (interpreter == NULL)
        return NULL;
    // Allocate string heap
    interpreter->string_heap = ps_string_heap_init(PS_STRING_HEAP_SIZE);
    if (interpreter->string_heap == NULL)
        return ps_interpreter_done(interpreter);
    // Allocate parser
    interpreter->parser = ps_parser_init();
    if (interpreter->parser == NULL)
        return ps_interpreter_done(interpreter);
    // Allocate and initialize system environment
    if (!ps_system_init(interpreter))
        return ps_interpreter_done(interpreter);
    // Set default state & options
    interpreter->level = PS_INTERPRETER_ENVIRONMENT_SYSTEM;
    interpreter->error = PS_ERROR_NONE;
    // flags
    interpreter->debug = false;
    interpreter->trace = false;
    interpreter->dump = false;
    // options
    interpreter->range_check = true;
    interpreter->bool_eval = false;
    return interpreter;
}

ps_interpreter *ps_interpreter_done(ps_interpreter *interpreter)
{
    if (interpreter->string_heap != NULL)
    {
        ps_string_heap_done(interpreter->string_heap);
        interpreter->string_heap = NULL;
    }
    if (interpreter->parser != NULL)
    {
        ps_parser_done(interpreter->parser);
        interpreter->parser = NULL;
    }
    for (size_t i = 0; i < PS_INTERPRETER_ENVIRONMENTS; i++)
    {
        if (interpreter->environments[i] != NULL)
        {
            ps_environment_done(interpreter->environments[i]);
            interpreter->environments[i] = NULL;
        }
    }
    free(interpreter);
    return NULL;
}

bool ps_interpreter_return_error(ps_interpreter *interpreter, ps_error error)
{
    interpreter->error = error;
    return false;
}

bool ps_interpreter_enter_environment(ps_interpreter *interpreter, ps_identifier *name)
{
    if (interpreter->level >= PS_INTERPRETER_ENVIRONMENTS - 1)
        return ps_interpreter_return_error(interpreter, PS_ERROR_ENVIRONMENT_OVERFLOW);
    ps_environment *parent = interpreter->environments[interpreter->level];
    ps_environment *environment = ps_environment_init(parent, name, PS_SYMBOL_TABLE_DEFAULT_SIZE);
    if (environment == NULL)
        return ps_interpreter_return_error(interpreter, PS_ERROR_OUT_OF_MEMORY);
    interpreter->level += 1;
    interpreter->environments[interpreter->level] = environment;
    fprintf(stderr, "--------------------------------------------------------------------------------\n");
    fprintf(stderr, "=> ENTER %d: %s\n", interpreter->level, (char *)name);
    fprintf(stderr, "--------------------------------------------------------------------------------\n");
    return true;
}

bool ps_interpreter_exit_environment(ps_interpreter *interpreter)
{
    if (interpreter->level <= PS_INTERPRETER_ENVIRONMENT_SYSTEM)
        return ps_interpreter_return_error(interpreter, PS_ERROR_ENVIRONMENT_UNDERFLOW);
    ps_environment *environment = interpreter->environments[interpreter->level];
    ps_environment_done(environment);
    interpreter->environments[interpreter->level] = NULL;
    fprintf(stderr, "--------------------------------------------------------------------------------\n");
    fprintf(stderr, "=> EXIT %d\n", interpreter->level);
    fprintf(stderr, "--------------------------------------------------------------------------------\n");
    interpreter->level -= 1;
    return true;
}

ps_environment *ps_interpreter_get_environment(ps_interpreter *interpreter)
{
    if (interpreter->level <= PS_INTERPRETER_ENVIRONMENT_SYSTEM || interpreter->level >= PS_INTERPRETER_ENVIRONMENTS)
    {
        // interpreter->error = PS_ERROR_INVALID_ENVIRONMENT;
        return NULL; // Invalid environment level
    }
    return interpreter->environments[interpreter->level];
}

ps_symbol *ps_interpreter_find_symbol(ps_interpreter *interpreter, ps_identifier *name, bool local)
{
    // char tmp[128];
    int level = interpreter->level;
    do
    {
        // snprintf(tmp, sizeof(tmp), "ENVIRONMENT %d: %s", level, interpreter->environments[level]->name);
        // ps_symbol_table_dump(interpreter->environments[level]->symbols, tmp, stderr);
        ps_symbol *symbol = ps_environment_find_symbol(interpreter->environments[level], name);
        if (symbol != NULL)
            return symbol;
        if (local)
            break;
        level -= 1;
    } while (level >= PS_INTERPRETER_ENVIRONMENT_SYSTEM);
    return NULL; // Symbol not found in any environment
}

bool ps_interpreter_add_symbol(ps_interpreter *interpreter, ps_symbol *symbol)
{
    ps_environment *environment = ps_interpreter_get_environment(interpreter);
    if (!ps_environment_add_symbol(environment, symbol))
        return ps_interpreter_return_error(interpreter, PS_ERROR_SYMBOL_NOT_ADDED);
    return true;
}

bool ps_interpreter_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to)
{
    // ps_value_debug(stderr, "FROM\t", from);
    // ps_value_debug(stderr, "TO\t", to);
    if (to->type == NULL || to->type->base == PS_TYPE_NONE)
        to->type = from->type;
    if (from->type == to->type)
    {
        to->data = from->data;
        return true;
    }
    // Integer => Unsigned?
    if (from->type->base == PS_TYPE_INTEGER && to->type->base == PS_TYPE_UNSIGNED)
    {
        if (interpreter->range_check && from->data.i < 0)
            return ps_interpreter_return_error(interpreter, PS_ERROR_OUT_OF_RANGE);
        to->data.u = from->data.i;
        return true;
    }
    // Unsigned => Integer?
    if (from->type->base == PS_TYPE_UNSIGNED && to->type->base == PS_TYPE_INTEGER)
    {
        if (interpreter->range_check && from->data.u > PS_INTEGER_MAX)
            return ps_interpreter_return_error(interpreter, PS_ERROR_OUT_OF_RANGE);
        to->data.i = from->data.u;
        return true;
    }
    // Integer => Real?
    if (from->type->base == PS_TYPE_INTEGER && to->type->base == PS_TYPE_REAL)
    {
        // NB: no range check needed, as real can hold all integer values
        to->data.r = (ps_real)from->data.i;
        return true;
    }
    // Unsigned => Real?
    if (from->type->base == PS_TYPE_UNSIGNED && to->type->base == PS_TYPE_REAL)
    {
        // NB: no range check needed, as real can hold all unsigned values
        to->data.r = (ps_real)from->data.u;
        return true;
    }
    return ps_interpreter_return_error(interpreter, PS_ERROR_TYPE_MISMATCH);
}

bool ps_interpreter_load_string(ps_interpreter *interpreter, char *source, size_t length)
{
    ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
    bool ok = ps_buffer_load_string(lexer->buffer, source, length);
    return ok;
}

bool ps_interpreter_load_file(ps_interpreter *interpreter, char *filename)
{
    ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
    return ps_buffer_load_file(lexer->buffer, filename);
}

bool ps_interpreter_run(ps_interpreter *interpreter, bool exec)
{
    ps_parser *parser = interpreter->parser;
    ps_lexer *lexer = ps_parser_get_lexer(parser);
    ps_lexer_reset(lexer);
    if (!ps_buffer_read_next_char(lexer->buffer))
    {
        fprintf(stderr, "ERROR: %s\n", ps_lexer_show_error(lexer));
        return false;
    }
    // parser->debug = true;
    if (!ps_parse_start(interpreter, exec ? MODE_EXEC : MODE_SKIP))
    {
        uint16_t start = lexer->buffer->current_line > 1 ? lexer->buffer->current_line - 2 : 0;
        ps_buffer_dump(lexer->buffer, start, 5);
        fprintf(stderr, "%*s\n", lexer->buffer->current_column + 14, "^");
        fprintf(stderr, "ERROR line %d column %d: interpreter=%d %s parser=%d %s lexer=%d %s\n",
                lexer->buffer->current_line + 1, lexer->buffer->current_column + 1, interpreter->error,
                ps_error_get_message(interpreter->error), parser->error, ps_error_get_message(parser->error),
                lexer->error, ps_error_get_message(lexer->error));
        ps_token_debug(stderr, "TOKEN: ", &lexer->current_token);
        return false;
    }
    return true;
}

// /** @brief Allocate new value */
// ps_value *ps_interpreter_alloc_value(ps_interpreter *interpreter)
// {
//     ps_value *value = (ps_value *)calloc(1, sizeof(ps_value));
//     if (value == NULL)
//     {
//         interpreter->error = PS_ERROR_OUT_OF_MEMORY;
//         return NULL;
//     }
//     return value;
// }

// void ps_interpreter_free_value(ps_interpreter *interpreter, ps_value *value)
// {
//     free(value);
// }

// ps_symbol *ps_interpreter_add_auto_value(ps_interpreter *interpreter, ps_value *value)
// {
//     ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_AUTO, NULL, value);
//     if (symbol == NULL)
//     {
//         interpreter->error = PS_ERROR_OUT_OF_MEMORY;
//         return NULL;
//     }
//     return ps_interpreter_add_symbol(interpreter, symbol);
// }

// ps_symbol *ps_interpreter_add_string_constant(ps_interpreter *interpreter, char *z)
// {
//     ps_symbol *symbol = ps_symbol_table_add_string_constant(interpreter->parser->symbols, z);
//     if (symbol == NULL)
//     {
//         interpreter->error = PS_ERROR_SYMBOL_NOT_ADDED;
//         return NULL;
//     }
//     return symbol;
// }

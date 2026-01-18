/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <string.h>

#include "ps_error.h"
#include "ps_interpreter.h"
#include "ps_memory.h"
#include "ps_parser.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_visit.h"

ps_interpreter *ps_interpreter_alloc()
{
    ps_interpreter *interpreter = ps_memory_malloc(sizeof(ps_interpreter));
    // fprintf(stderr, "ALLOC\tINTERPRETER: %p\n", interpreter);
    if (interpreter == NULL)
        return NULL;
    // Set default state
    for (size_t i = 0; i < PS_INTERPRETER_ENVIRONMENTS; i++)
        interpreter->environments[i] = NULL;
    interpreter->level = PS_INTERPRETER_ENVIRONMENT_SYSTEM;
    interpreter->error = PS_ERROR_NONE;
    interpreter->debug = DEBUG_NONE;
    interpreter->range_check = true;
    interpreter->bool_eval = false;
    // Allocate string heap
    interpreter->string_heap = ps_string_heap_alloc(PS_STRING_HEAP_SIZE);
    if (interpreter->string_heap == NULL)
        return ps_interpreter_free(interpreter);
    // Allocate parser
    interpreter->parser = ps_parser_alloc();
    if (interpreter->parser == NULL)
        return ps_interpreter_free(interpreter);
    // Allocate system environment
    ps_identifier system = "SYSTEM";
    interpreter->environments[0] = ps_environment_alloc(NULL, &system, PS_SYSTEM_SYMBOL_TABLE_SIZE);
    if (interpreter->environments[0] == NULL)
        return ps_interpreter_free(interpreter);
    // Initialize system environment
    if (!ps_system_init(interpreter->environments[0]))
        return ps_interpreter_free(interpreter);
    return interpreter;
}

ps_interpreter *ps_interpreter_free(ps_interpreter *interpreter)
{
    if (interpreter != NULL)
    {
        // fprintf(stderr, "FREE\tINTERPRETER: %p\n", interpreter);
        if (interpreter->string_heap != NULL)
            interpreter->string_heap = ps_string_heap_free(interpreter->string_heap);
        if (interpreter->parser != NULL)
            interpreter->parser = ps_parser_free(interpreter->parser);
        ps_system_done(interpreter->environments[0]);
        for (size_t i = 0; i < PS_INTERPRETER_ENVIRONMENTS; i++)
        {
            if (interpreter->environments[i] != NULL)
                interpreter->environments[i] = ps_environment_free(interpreter->environments[i]);
        }
        ps_memory_free(interpreter);
    }
    return NULL;
}

bool ps_interpreter_return_false(ps_interpreter *interpreter, ps_error error)
{
    interpreter->error = error;
    return false;
}

void *ps_interpreter_return_null(ps_interpreter *interpreter, ps_error error)
{
    interpreter->error = error;
    return NULL;
}

bool ps_interpreter_set_message(ps_interpreter *interpreter, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    int ret = vsnprintf(interpreter->message, sizeof(interpreter->message), format, args);
    va_end(args);
    if (ret < 0 || ret >= (int)sizeof(interpreter->message))
        return ps_interpreter_return_false(interpreter, PS_ERROR_OVERFLOW);
    return true;
}

bool ps_interpreter_enter_environment(ps_interpreter *interpreter, ps_identifier *name)
{
    if (interpreter->level >= PS_INTERPRETER_ENVIRONMENTS - 1)
        return ps_interpreter_return_false(interpreter, PS_ERROR_ENVIRONMENT_OVERFLOW);
    ps_environment *parent = interpreter->environments[interpreter->level];
    ps_environment *environment = ps_environment_alloc(parent, name, PS_SYMBOL_TABLE_DEFAULT_SIZE);
    if (environment == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_MEMORY);
    interpreter->level += 1;
    interpreter->environments[interpreter->level] = environment;
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
        fprintf(stderr, "=> ENTER ENVIRONMENT %d/%d '%s'\n", interpreter->level, PS_INTERPRETER_ENVIRONMENTS,
                (char *)name);
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
    }
    return true;
}

bool ps_interpreter_exit_environment(ps_interpreter *interpreter)
{
    ps_environment *environment = interpreter->environments[interpreter->level];
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
        fprintf(stderr, "=> EXIT ENVIRONMENT %d/%d '%s'\n", interpreter->level, PS_INTERPRETER_ENVIRONMENTS,
                environment->name);
        ps_symbol_table_dump(NULL, "EXIT ENVIRONMENT", environment->symbols);
        fprintf(stderr, "--------------------------------------------------------------------------------\n");
    }
    if (interpreter->level <= PS_INTERPRETER_ENVIRONMENT_SYSTEM)
        return ps_interpreter_return_false(interpreter, PS_ERROR_ENVIRONMENT_UNDERFLOW);
    interpreter->environments[interpreter->level] = ps_environment_free(environment);
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
    ps_symbol *symbol;
    do
    {
        // snprintf(tmp, sizeof(tmp), "ENVIRONMENT %d: %s", level, interpreter->environments[level]->name);
        // ps_symbol_table_dump(NULL, tmp, interpreter->environments[level]->symbols);
        symbol = ps_environment_find_symbol(interpreter->environments[level], name, local);
        if (symbol != NULL)
            return symbol;
        if (local)
            break;
        level -= 1;
    } while (level != PS_INTERPRETER_ENVIRONMENT_SYSTEM);
    return NULL; // Symbol not found in any environment
}

bool ps_interpreter_add_symbol(ps_interpreter *interpreter, ps_symbol *symbol)
{
    ps_environment *environment = ps_interpreter_get_environment(interpreter);
    if (interpreter->debug >= DEBUG_TRACE)
        fprintf(stderr, "ADD %*s SYMBOL '%*s' TO ENVIRONMENT '%*s' with value %p: '%s'\n", -10,
                ps_symbol_get_kind_name(symbol->kind), -(int)PS_IDENTIFIER_LEN, symbol->name, -(int)PS_IDENTIFIER_LEN,
                environment->name, (void *)symbol->value,
                symbol->value == NULL ? "NULL" : ps_value_get_debug_string(symbol->value));
    if (interpreter->debug >= DEBUG_VERBOSE)
        ps_symbol_table_dump(NULL, "BEFORE ADD", environment->symbols);
    if (!ps_environment_add_symbol(environment, symbol))
        return ps_interpreter_return_false(interpreter, PS_ERROR_SYMBOL_NOT_ADDED);
    if (interpreter->debug >= DEBUG_VERBOSE)
        ps_symbol_table_dump(NULL, "AFTER ADD", environment->symbols);
    return true;
}

bool ps_interpreter_copy_value(ps_interpreter *interpreter, ps_value *from, ps_value *to)
{
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fputc('*', stderr);
        ps_value_debug(stderr, "FROM\t", from);
    }
    // If destination type is NONE, set it to source type
    if (to->type == NULL || to->type == &ps_system_none || to->type->value->data.t->base == PS_TYPE_NONE)
        to->type = from->type;
    // Same type, just copy value
    if (from->type == to->type)
    {
        to->data = from->data;
        goto OK;
    }
    // Integer => Unsigned?
    if (from->type->value->data.t->base == PS_TYPE_INTEGER && to->type->value->data.t->base == PS_TYPE_UNSIGNED)
    {
        if (interpreter->range_check && from->data.i < 0)
            return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_RANGE);
        to->data.u = (ps_unsigned)from->data.i;
        goto OK;
    }
    // Unsigned => Integer?
    if (from->type->value->data.t->base == PS_TYPE_UNSIGNED && to->type->value->data.t->base == PS_TYPE_INTEGER)
    {
        if (interpreter->range_check && from->data.u > PS_INTEGER_MAX)
            return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_RANGE);
        to->data.i = (ps_integer)from->data.u;
        goto OK;
    }
    // Integer => Real?
    if (from->type->value->data.t->base == PS_TYPE_INTEGER && to->type->value->data.t->base == PS_TYPE_REAL)
    {
        // NB: no range check needed, as real can hold all integer values
        to->data.r = (ps_real)from->data.i;
        goto OK;
    }
    // Unsigned => Real?
    if (from->type->value->data.t->base == PS_TYPE_UNSIGNED && to->type->value->data.t->base == PS_TYPE_REAL)
    {
        // NB: no range check needed, as real can hold all unsigned values
        to->data.r = (ps_real)from->data.u;
        goto OK;
    }
    ps_interpreter_set_message(interpreter, "Cannot convert value from type '%s' to type '%s'",
                               ps_value_type_get_name(from->type->value->data.t->base),
                               ps_value_type_get_name(to->type->value->data.t->base));
    return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
OK:
    if (interpreter->debug >= DEBUG_VERBOSE)
    {
        fputc('*', stderr);
        ps_value_debug(stderr, "TO\t", to);
    }
    return true;
}

bool ps_interpreter_load_string(ps_interpreter *interpreter, char *source, size_t length)
{
    ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser);
    return ps_buffer_load_string(lexer->buffer, source, length);
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

    memset(interpreter->message, 0, sizeof(interpreter->message));
    if (!ps_buffer_read_next_char(lexer->buffer))
    {
        fprintf(stderr, "ERROR: %s\n", ps_lexer_get_debug_value(lexer));
        return false;
    }
    if (!ps_visit_start(interpreter, exec ? MODE_EXEC : MODE_SKIP))
    {
        // Display up to 3 lines around the error
        uint16_t start = lexer->buffer->current_line > 1 ? lexer->buffer->current_line - 2 : 0;
        int margin = ps_buffer_dump(stderr, lexer->buffer, start, 4);
        // Try to align the '^' under the right column
        fprintf(stderr, "%*s\n", margin + lexer->buffer->current_column, "^");
        // Make line and column 1-based
        fprintf(stderr, "ERROR line %d column %d: interpreter=%d %s parser=%d %s lexer=%d %s\n",
                lexer->buffer->current_line + 1, lexer->buffer->current_column + 1, interpreter->error,
                ps_error_get_message(interpreter->error), parser->error, ps_error_get_message(parser->error),
                lexer->error, ps_error_get_message(lexer->error));
        if (interpreter->message[0] != '\0')
            fprintf(stderr, "ERROR %s\n", interpreter->message);
        ps_token_debug(stderr, "TOKEN: ", &lexer->current_token);
        return false;
    }

    return true;
}

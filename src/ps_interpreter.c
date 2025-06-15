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
    interpreter->parser = ps_parser_init(NULL, NULL);
    if (interpreter->parser == NULL)
    {
        ps_interpreter_done(interpreter);
        return NULL;
    }
    if (!ps_system_init(interpreter))
    {
        ps_interpreter_done(interpreter);
        return NULL;
    }
    interpreter->error = PS_RUNTIME_ERROR_NONE;
    interpreter->debug = false;
    interpreter->trace = false;
    interpreter->range_check = true;
    interpreter->bool_eval = false;
    interpreter->from_string = false;
    interpreter->environment = ps_system_init(interpreter);
    return interpreter;
}

void ps_interpreter_done(ps_interpreter *interpreter)
{
    if (interpreter->parser != NULL)
    {
        ps_parser_done(interpreter->parser);
        interpreter->parser = NULL;
    }
    free(interpreter);
}

/** @brief Allocate new value */
ps_value *ps_interpreter_alloc_value(ps_interpreter *interpreter)
{
    ps_value *value = (ps_value *)calloc(1, sizeof(ps_value));
    if (value == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
        return NULL;
    }
    return value;
}

void ps_interpreter_free_value(ps_interpreter *interpreter, ps_value *value)
{
    free(value);
}

ps_symbol *ps_interpreter_add_auto_value(ps_interpreter *interpreter, ps_value *value)
{
    ps_symbol *symbol = ps_symbol_alloc(PS_SYMBOL_KIND_AUTO, NULL, value);
    if (symbol == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_OUT_OF_MEMORY;
        return NULL;
    }
    return ps_interpreter_add_symbol(interpreter, symbol);
}

ps_symbol *ps_interpreter_add_symbol(ps_interpreter *interpreter, ps_value *symbol)
{
    ps_symbol_table *symbols = interpreter->parser->symbols;
    if (!ps_symbol_table_add(symbols, symbol) == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED;
        return NULL;
    }
    return symbol;
}

ps_symbol *ps_interpreter_add_string_constant(ps_interpreter *interpreter, char *z)
{
    ps_symbol *symbol = ps_symbol_table_add_string_constant(interpreter->parser->symbols, z);
    if (symbol == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED;
        return NULL;
    }
    return symbol;
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

bool ps_interpreter_enter_scope(ps_interpreter *interpreter)
{
    // TODO: implement scope management
    return false;
}

bool ps_interpreter_exit_scope(ps_interpreter *interpreter)
{
    // TODO: implement scope management
    return false;
}

bool ps_interpreter_run(ps_interpreter *interpreter, bool exec)
{
    ps_parser *parser = interpreter->parser;
    ps_lexer *lexer = ps_parser_get_lexer(parser);
    // parser->debug = true;
    if (!ps_visit_start(interpreter, exec))
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

/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>

#include "ps_ast.h"
#include "ps_error.h"
#include "ps_executable.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_tools.h"
#include "ps_type_definition.h"
#include "ps_value.h"

/**********************************************************************************************************************/
/* STANDARD PROCEDURES                                                                                                */
/**********************************************************************************************************************/

/* clang-format off */
PS_SYSTEM_PROCEDURE(dec      , "DEC"      , PS_EXECUTABLE_PROC_1ARG_S      , .proc_1arg_s      , &ps_procedure_dec      )
PS_SYSTEM_PROCEDURE(inc      , "INC"      , PS_EXECUTABLE_PROC_1ARG_S      , .proc_1arg_s      , &ps_procedure_inc      )
PS_SYSTEM_PROCEDURE(randomize, "RANDOMIZE", PS_EXECUTABLE_PROC_1ARG_V      , .proc_1arg_v      , &ps_procedure_randomize)
PS_SYSTEM_PROCEDURE(read     , "READ"     , PS_EXECUTABLE_PROC_FILE_READ   , .proc_file_read   , &ps_procedure_read     )
PS_SYSTEM_PROCEDURE(readln   , "READLN"   , PS_EXECUTABLE_PROC_FILE_READ   , .proc_file_read   , &ps_procedure_readln   )
PS_SYSTEM_PROCEDURE(readstr  , "READSTR"  , PS_EXECUTABLE_PROC_STRING_READ , .proc_string_read , &ps_procedure_readstr  )
PS_SYSTEM_PROCEDURE(write    , "WRITE"    , PS_EXECUTABLE_PROC_FILE_WRITE  , .proc_file_write  , &ps_procedure_write    )
PS_SYSTEM_PROCEDURE(writeln  , "WRITELN"  , PS_EXECUTABLE_PROC_FILE_WRITE  , .proc_file_write  , &ps_procedure_writeln  )
PS_SYSTEM_PROCEDURE(writestr , "WRITESTR" , PS_EXECUTABLE_PROC_STRING_WRITE, .proc_string_write, &ps_procedure_writestr )
/* clang-format on */

bool ps_procedures_init(ps_ast_block *system)
{
    (void)system;
    ADD_SYSTEM_SYMBOL(ps_system_procedure_dec)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_inc)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_randomize)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_read)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_readln)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_readstr)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_write)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_writeln)
    ADD_SYSTEM_SYMBOL(ps_system_procedure_writestr)
    return true;
error:
    return false;
}

bool ps_procedure_inc_or_dec(ps_interpreter *interpreter, ps_symbol *variable, bool is_inc)
{
    (void)variable;
    (void)is_inc;
    return ps_interpreter_set_error_message(interpreter, PS_ERROR_NOT_IMPLEMENTED, "%s", is_inc ? "INC" : "DEC");
    /*
    ps_value value = {0};
    if (!ps_interpreter_get_variable_value(interpreter, variable, &value))
        return false;
    if (!ps_value_is_ordinal(&value))
        return ps_interpreter_return_false(interpreter, PS_ERROR_UNEXPECTED_TYPE);
    ps_value new_value = {.allocated = false, .type = value.type, .data = {0}};
    ps_value_type base = ps_value_get_base(&value);
    switch (base)
    {
    case PS_TYPE_CHAR:
    case PS_TYPE_INTEGER:
    case PS_TYPE_UNSIGNED:
        ps_error error = is_inc ? ps_function_succ(interpreter, &value, &new_value)
                                : ps_function_pred(interpreter, &value, &new_value);
        if (error != PS_ERROR_NONE)
            return ps_interpreter_return_false(interpreter, error);
        break;
    default:
        return ps_interpreter_return_false(interpreter, PS_ERROR_UNEXPECTED_TYPE);
    }
    ps_error error = ps_value_copy(&new_value, &value, interpreter->range_check);
    if (error != PS_ERROR_NONE)
        return ps_interpreter_return_false(interpreter, error);
    if (!ps_interpreter_set_variable_value(interpreter, variable, &value))
        return false;
    return true;
    */
}

bool ps_procedure_dec(ps_interpreter *interpreter, ps_symbol *variable)
{
    return ps_procedure_inc_or_dec(interpreter, variable, false);
}

bool ps_procedure_inc(ps_interpreter *interpreter, ps_symbol *variable)
{
    return ps_procedure_inc_or_dec(interpreter, variable, true);
}

/**
 * Execute system procedure Randomize / Randomize(Seed)
 */
bool ps_procedure_randomize(ps_interpreter *interpreter, const ps_value *value)
{
    unsigned int seed = 0;
    // No argument: use current time as seed
    if (value != NULL && value->type != NULL && value->type->value != NULL)
    {
        // Argument: use its value as seed
        switch (value->type->value->data.t->base)
        {
        case PS_TYPE_INTEGER:
            seed = (unsigned int)(value->data.i);
            break;
        case PS_TYPE_UNSIGNED:
            seed = (unsigned int)(value->data.u); // NOSONAR
            break;
        default:
            return ps_interpreter_return_false(interpreter, PS_ERROR_UNEXPECTED_TYPE);
        }
        srand(seed);
        if (interpreter->logger->debug_level >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "RANDOMIZE(%u)\n", seed);
        return true;
    }
    srand((unsigned int)time(NULL));
    if (interpreter->logger->debug_level >= PS_DEBUG_VERBOSE)
        fprintf(stderr, "RANDOMIZE\n");
    return true;
}

/**
 * Execute system procedure Read(F, Value)
 */
bool ps_procedure_read(ps_interpreter *interpreter, FILE *f, ps_value *value) // NOSONAR
{
    assert(interpreter != NULL);
    assert(value != NULL);

    FILE *input = f == NULL ? stdin : f;
    ps_value_type type = ps_value_get_base(value);
    ps_value_type visible_type = ps_value_get_type(value);

    if (type == PS_TYPE_INTEGER)
    {
        char token[64];
        if (fscanf(input, "%63s", token) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read integer");
        errno = 0;
        char *end = NULL;
        ps_integer number = (ps_integer)strtoll(token, &end, 10);
        if (errno == ERANGE || end == token || *end != '\0')
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read integer from '%s'", token);
        ps_value_set_integer(value, number);
        return true;
    }
    if (type == PS_TYPE_UNSIGNED)
    {
        char token[64];
        if (fscanf(input, "%63s", token) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read unsigned integer");
        errno = 0;
        char *end = NULL;
        ps_unsigned number = (ps_unsigned)strtoull(token, &end, 10);
        if (errno == ERANGE || end == token || *end != '\0')
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read unsigned integer from '%s'", token);
        ps_value_set_unsigned(value, number);
        return true;
    }
    if (type == PS_TYPE_REAL)
    {
        char token[64];
        if (fscanf(input, "%63s", token) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read real");
        errno = 0;
        char *end = NULL;
        ps_real number = strtod(token, &end);
        if (errno == ERANGE || end == token || *end != '\0')
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read real from '%s'", token);
        ps_value_set_real(value, number);
        return true;
    }
    if (type == PS_TYPE_CHAR)
    {
        ps_char c = '\0';
        if (fscanf(input, " %c", &c) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read character");
        ps_value_set_char(value, c);
        return true;
    }
    if (visible_type == PS_TYPE_STRING)
    {
        char token[PS_STRING_MAX_LEN + 1];
        if (fscanf(input, "%255s", token) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read string");
        ps_string *string = ps_string_create(token);
        if (string == NULL)
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                    "Could not allocate read string");
        ps_value_set_string(value, string);
        return true;
    }
    if (visible_type == PS_TYPE_BOOLEAN)
    {
        char token[16];
        if (fscanf(input, "%15s", token) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read boolean");
        if (!strcasecmp(token, "TRUE"))
            ps_value_set_boolean(value, true);
        else if (!strcasecmp(token, "FALSE"))
            ps_value_set_boolean(value, false);
        else
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read boolean from '%s'", token);
        return true;
    }
    if (visible_type == PS_TYPE_ENUM || type == PS_TYPE_ENUM)
    {
        char token[64];
        if (fscanf(input, "%63s", token) != 1)
            return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                    "Failed to read enum");
        ps_symbol *enum_type = value->type;
        ps_type_definition *type_def = enum_type->value->data.t;
        if (ps_value_get_type(value) == PS_TYPE_SUBRANGE)
            type_def = type_def->def.g.e.symbol_enum->value->data.t;
        for (ps_unsigned i = 0; i < type_def->def.e.count; i += 1)
        {
            if (!strcasecmp((char *)type_def->def.e.values[i]->name, token))
            {
                value->data.u = i;
                return true;
            }
        }
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read enum value '%s'", token);
    }

    return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                            "Cannot read value of type '%s'", ps_value_get_type_name(value));
}

/**
 * Execute system procedure ReadLn(F, Value)
 */
bool ps_procedure_readln(ps_interpreter *interpreter, FILE *f, ps_value *value) // NOSONAR
{
    if (!ps_procedure_read(interpreter, f, value))
        return false;

    FILE *input = f == NULL ? stdin : f;
    int c;
    do
    {
        c = fgetc(input);
    } while (c != EOF && c != '\n');
    return true;
}

/**
 * Execute system procedure ReadStr(S, Value)
 */
bool ps_procedure_readstr(ps_interpreter *interpreter, ps_string *s, ps_value *value) // NOSONAR
{
    assert(interpreter != NULL);
    assert(s != NULL);
    assert(value != NULL);

    ps_string_len pos = 0;
    while (pos < s->len && isspace((unsigned char)s->str[pos]))
        pos += 1;
    if (pos >= s->len)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_EOF, "No value remaining in ReadStr string");

    ps_value_type type = ps_value_get_base(value);
    ps_value_type visible_type = ps_value_get_type(value);

    if (type == PS_TYPE_INTEGER)
    {
        errno = 0;
        char *end = NULL;
        ps_integer number = (ps_integer)strtoll((char *)&s->str[pos], &end, 10);
        if (errno == ERANGE || end == (char *)&s->str[pos])
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read integer from ReadStr string");
        ps_value_set_integer(value, number);
        pos = (ps_string_len)((char *)end - (char *)s->str);
    }
    else if (type == PS_TYPE_UNSIGNED)
    {
        errno = 0;
        char *end = NULL;
        ps_unsigned number = (ps_unsigned)strtoull((char *)&s->str[pos], &end, 10);
        if (errno == ERANGE || end == (char *)&s->str[pos])
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read unsigned integer from ReadStr string");
        ps_value_set_unsigned(value, number);
        pos = (ps_string_len)((char *)end - (char *)s->str);
    }
    else if (type == PS_TYPE_REAL)
    {
        errno = 0;
        char *end = NULL;
        ps_real number = strtod((char *)&s->str[pos], &end);
        if (errno == ERANGE || end == (char *)&s->str[pos])
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read real from ReadStr string");
        ps_value_set_real(value, number);
        pos = (ps_string_len)((char *)end - (char *)s->str);
    }
    else if (type == PS_TYPE_CHAR)
    {
        ps_value_set_char(value, s->str[pos]);
        pos += 1;
    }
    else if (visible_type == PS_TYPE_STRING)
    {
        ps_string_len end = pos;
        while (end < s->len && !isspace((unsigned char)s->str[end]))
            end += 1;
        ps_string *string = ps_string_create((char *)&s->str[pos]);
        if (string == NULL)
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                    "Could not allocate ReadStr string");
        string->str[end - pos] = '\0';
        string->len = end - pos;
        ps_value_set_string(value, string);
        pos = end;
    }
    else if (visible_type == PS_TYPE_BOOLEAN)
    {
        ps_string_len end = pos;
        while (end < s->len && !isspace((unsigned char)s->str[end]))
            end += 1;
        char token[16];
        ps_string_len length = end - pos < 15 ? end - pos : 15;
        memcpy(token, &s->str[pos], length);
        token[length] = '\0';
        if (!strcasecmp(token, "TRUE"))
            ps_value_set_boolean(value, true);
        else if (!strcasecmp(token, "FALSE"))
            ps_value_set_boolean(value, false);
        else
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read boolean from '%s'", token);
        pos = end;
    }
    else if (visible_type == PS_TYPE_ENUM || type == PS_TYPE_ENUM)
    {
        ps_string_len end = pos;
        while (end < s->len && !isspace((unsigned char)s->str[end]))
            end += 1;
        ps_string_len length = end - pos;
        ps_symbol *enum_type = value->type;
        ps_type_definition *type_def = enum_type->value->data.t;
        if (ps_value_get_type(value) == PS_TYPE_SUBRANGE)
            type_def = type_def->def.g.e.symbol_enum->value->data.t;
        for (ps_unsigned i = 0; i < type_def->def.e.count; i += 1)
        {
            ps_char *name = type_def->def.e.values[i]->name;
            if ((ps_string_len)strlen((char *)name) == length &&
                !strncasecmp((char *)name, (char *)&s->str[pos], length))
            {
                value->data.u = i;
                pos = end;
                goto enum_read_done;
            }
        }
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read enum value from ReadStr");
enum_read_done:
        ;
    }
    else
    {
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read value of type '%s' from ReadStr",
                                                ps_value_get_type_name(value));
    }

    while (pos < s->len && isspace((unsigned char)s->str[pos]))
        pos += 1;
    ps_string_len remaining = s->len - pos;
    if (pos > 0)
        memmove(s->str, &s->str[pos], remaining);
    s->len = remaining;
    s->str[remaining] = '\0';
    return true;
}

bool ps_procedure_write_or_writeln(ps_interpreter *interpreter, FILE *f, const ps_value *value, int16_t width,
                                   int16_t precision)
{
    char *display_value = ps_value_get_display_string(value, width, precision);
    if (display_value == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_EXPECTED_STRING);
    if (interpreter->logger->debug_level >= PS_DEBUG_VERBOSE)
        fprintf(f, "WRITE('%s')\n", display_value);
    else
        fprintf(f, "%s", display_value);
    return true;
}

bool ps_procedure_write(ps_interpreter *interpreter, FILE *f, const ps_value *value, int16_t width, int16_t precision)
{
    return ps_procedure_write_or_writeln(interpreter, f, value, width, precision);
}

bool ps_procedure_writeln(ps_interpreter *interpreter, FILE *f, const ps_value *value, int16_t width, int16_t precision)
{
    return ps_procedure_write_or_writeln(interpreter, f, value, width, precision);
}

bool ps_procedure_writestr(ps_interpreter *interpreter, ps_string *s, const ps_value *value, int16_t width,
                           int16_t precision)
{
    static char buffer[PS_STRING_MAX_LEN + 1];
    memset(buffer, 0, sizeof(buffer));
    char *display_value = ps_value_get_display_string(value, width, precision);
    if (display_value == NULL)
        return ps_interpreter_return_false(interpreter, PS_ERROR_EXPECTED_STRING);
    snprintf(buffer, sizeof(buffer), "%s%s", s->str, display_value);
    (void)ps_strscpy((char *)s->str, buffer, s->max + 1);
    s->len = (ps_string_len)ps_strnlen((char *)s->str, s->max);
    return true;
}

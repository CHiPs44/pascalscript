/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
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
    int c;

    do
    {
        c = fgetc(input);
    } while (c != EOF && isspace((unsigned char)c));
    if (c == EOF)
        return ps_interpreter_set_error_message(interpreter, feof(input) ? PS_ERROR_EOF : PS_ERROR_READING_FILE,
                                                "Failed to read value");

    ps_string *buffer = ps_string_alloc(0);
    if (buffer == NULL)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY, "Could not allocate read buffer");

    ps_string *appended = ps_string_append_char(buffer, (ps_char)c);
    if (appended == NULL)
    {
        ps_string_free(buffer);
        return ps_interpreter_set_error_message(interpreter,
                                                errno == EINVAL ? PS_ERROR_STRING_TOO_LONG : PS_ERROR_OUT_OF_MEMORY,
                                                "Could not append to read buffer");
    }
    buffer = appended;

    if (ps_value_get_base(value) != PS_TYPE_CHAR)
    {
        while (c != EOF && !isspace((unsigned char)c))
        {
            c = fgetc(input);
            if (c == EOF || isspace((unsigned char)c))
                break;

            appended = ps_string_append_char(buffer, (ps_char)c);
            if (appended == NULL)
            {
                ps_string_free(buffer);
                return ps_interpreter_set_error_message(
                        interpreter, errno == EINVAL ? PS_ERROR_STRING_TOO_LONG : PS_ERROR_OUT_OF_MEMORY,
                        "Could not append to read buffer");
            }
            buffer = appended;
        }
        if (c != EOF)
            (void)ungetc(c, input);
    }

    bool ok = ps_procedure_readstr(interpreter, buffer, value);
    ps_string_free(buffer);
    return ok;
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

static bool ps_procedure_readstr_integer(ps_interpreter *interpreter, ps_string *s, ps_value *value, ps_string_len pos,
                                         ps_string_len *consumed)
{
    errno = 0;
    char *end = NULL;
    long long number = strtoll((char *)&s->str[pos], &end, 10);
    if (errno == ERANGE || end == (char *)&s->str[pos] || number < PS_INTEGER_MIN || number > PS_INTEGER_MAX)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read integer from ReadStr string");
    ps_value_set_integer(value, (ps_integer)number);
    *consumed = (ps_string_len)(end - (char *)s->str);
    return true;
}

static bool ps_procedure_readstr_unsigned(ps_interpreter *interpreter, ps_string *s, ps_value *value, ps_string_len pos,
                                          ps_string_len *consumed)
{
    errno = 0;
    char *end = NULL;
    unsigned long long number = strtoull((char *)&s->str[pos], &end, 10);
    if (errno == ERANGE || end == (char *)&s->str[pos] || number > PS_UNSIGNED_MAX)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read unsigned integer from ReadStr string");
    ps_value_set_unsigned(value, (ps_unsigned)number);
    *consumed = (ps_string_len)(end - (char *)s->str);
    return true;
}

static bool ps_procedure_readstr_real(ps_interpreter *interpreter, ps_string *s, ps_value *value, ps_string_len pos,
                                      ps_string_len *consumed)
{
    errno = 0;
    char *end = NULL;
    double d = strtod((char *)&s->str[pos], &end);
    if (errno == ERANGE || end == (char *)&s->str[pos])
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read real from ReadStr string");
    ps_real number = (ps_real)d;
    ps_value_set_real(value, number);
    *consumed = (ps_string_len)(end - (char *)s->str);
    return true;
}

static bool ps_procedure_readstr_char(ps_value *value, const ps_string *s, ps_string_len pos, ps_string_len *consumed)
{
    ps_value_set_char(value, s->str[pos]);
    *consumed = pos + 1;
    return true;
}

static ps_string_len ps_procedure_readstr_token_end(const ps_string *s, ps_string_len pos)
{
    ps_string_len end = pos;
    while (end < s->len && !isspace((unsigned char)s->str[end]))
        end += 1;
    return end;
}

static bool ps_procedure_readstr_string(ps_interpreter *interpreter, ps_string *s, ps_value *value, ps_string_len pos,
                                        ps_string_len *consumed)
{
    ps_string_len end = ps_procedure_readstr_token_end(s, pos);
    ps_string_len length = end - pos;
    ps_string *string = ps_string_alloc(length);
    if (string == NULL)
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_OUT_OF_MEMORY,
                                                "Could not allocate ReadStr string");
    if (!ps_string_set(string, length, (char *)&s->str[pos]))
    {
        ps_string_free(string);
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_STRING_TOO_LONG,
                                                "ReadStr string value is too long");
    }
    ps_value_set_string(value, string);
    *consumed = end;
    return true;
}

static bool ps_procedure_readstr_boolean(ps_interpreter *interpreter, ps_string *s, ps_value *value, ps_string_len pos,
                                         ps_string_len *consumed)
{
    ps_string_len end = ps_procedure_readstr_token_end(s, pos);
    ps_string_len length = end - pos;
    char *token = (char *)&s->str[pos];
    if (length == 4 && !strncasecmp(token, "TRUE", length))
        ps_value_set_boolean(value, true);
    else if (length == 5 && !strncasecmp(token, "FALSE", length))
        ps_value_set_boolean(value, false);
    else
    {
        const char *token_end = (char *)&s->str[end];
        char *parsed_end = NULL;
        errno = 0;
        long long signed_number = strtoll(token, &parsed_end, 10);
        if (parsed_end == token_end && parsed_end != token && errno != ERANGE)
            ps_value_set_boolean(value, signed_number != 0);
        else if (token[0] != '-')
        {
            errno = 0;
            unsigned long long unsigned_number = strtoull(token, &parsed_end, 10);
            if (parsed_end == token_end && parsed_end != token && errno != ERANGE)
                ps_value_set_boolean(value, unsigned_number != 0);
            else
                return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                        "Cannot read boolean from '%.*s'", (int)length, token);
        }
        else
            return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                    "Cannot read boolean from '%.*s'", (int)length, token);
    }
    *consumed = end;
    return true;
}

static bool ps_procedure_readstr_enum(ps_interpreter *interpreter, ps_string *s, ps_value *value, ps_string_len pos,
                                      ps_string_len *consumed)
{
    ps_string_len end = ps_procedure_readstr_token_end(s, pos);
    ps_string_len length = end - pos;
    ps_symbol *enum_type = value->type;
    ps_type_definition *type_def = enum_type->value->data.t;
    if (ps_value_get_type(value) == PS_TYPE_SUBRANGE)
        type_def = type_def->def.g.e.symbol_enum->value->data.t;
    for (ps_unsigned i = 0; i < type_def->def.e.count; i += 1)
    {
        ps_char *name = type_def->def.e.values[i]->name;
        if ((ps_string_len)ps_strnlen((char *)name, PS_IDENTIFIER_SIZE + 1) == length &&
            !strncasecmp((char *)name, (char *)&s->str[pos], length))
        {
            value->data.u = i;
            *consumed = end;
            return true;
        }
    }
    return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH, "Cannot read enum value from ReadStr");
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
    ps_string_len consumed = pos;
    bool ok = false;

    if (type == PS_TYPE_INTEGER)
        ok = ps_procedure_readstr_integer(interpreter, s, value, pos, &consumed);
    else if (type == PS_TYPE_UNSIGNED)
        ok = ps_procedure_readstr_unsigned(interpreter, s, value, pos, &consumed);
    else if (type == PS_TYPE_REAL)
        ok = ps_procedure_readstr_real(interpreter, s, value, pos, &consumed);
    else if (type == PS_TYPE_CHAR)
        ok = ps_procedure_readstr_char(value, s, pos, &consumed);
    else if (visible_type == PS_TYPE_STRING)
        ok = ps_procedure_readstr_string(interpreter, s, value, pos, &consumed);
    else if (visible_type == PS_TYPE_BOOLEAN)
        ok = ps_procedure_readstr_boolean(interpreter, s, value, pos, &consumed);
    else if (visible_type == PS_TYPE_ENUM || type == PS_TYPE_ENUM)
        ok = ps_procedure_readstr_enum(interpreter, s, value, pos, &consumed);
    else
        return ps_interpreter_set_error_message(interpreter, PS_ERROR_TYPE_MISMATCH,
                                                "Cannot read value of type '%s' from ReadStr",
                                                ps_value_get_type_name(value));
    if (!ok)
        return false;

    while (consumed < s->len && isspace((unsigned char)s->str[consumed]))
        consumed += 1;
    ps_string_len remaining = s->len - consumed;
    if (consumed > 0)
        memmove(s->str, &s->str[consumed], remaining);
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

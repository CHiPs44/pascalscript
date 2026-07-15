/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_ast.h"
#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_string.h"
#include "ps_symbol_table.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_value.h"

/******************************************************************************/
/* STANDARD FUNCTIONS                                                         */
/******************************************************************************/

/* clang-format off */
// Math
PS_SYSTEM_FUNCTION (function, abs           , "ABS"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_abs           );
PS_SYSTEM_FUNCTION (function, arctan        , "ARCTAN"      , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_arctan        );
PS_SYSTEM_FUNCTION (function, cos           , "COS"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_cos           );
PS_SYSTEM_FUNCTION (function, even          , "EVEN"        , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_even          );
PS_SYSTEM_FUNCTION (function, exp           , "EXP"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_exp           );
PS_SYSTEM_FUNCTION (function, frac          , "FRAC"        , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_frac          );
PS_SYSTEM_FUNCTION (function, int           , "INT"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_int           );
PS_SYSTEM_FUNCTION (function, ln            , "LN"          , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_ln            );
PS_SYSTEM_FUNCTION (function, log           , "LOG"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_log           );
PS_SYSTEM_FUNCTION (function, odd           , "ODD"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_odd           );
PS_SYSTEM_FUNCTION (function, power         , "POWER"       , PS_EXECUTABLE_FUNC_1ARG_S, .func_2args , &ps_function_power         );
PS_SYSTEM_FUNCTION (function, random        , "RANDOM"      , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_random        );
PS_SYSTEM_FUNCTION (function, round         , "ROUND"       , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_round         );
PS_SYSTEM_FUNCTION (function, sin           , "SIN"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_sin           );
PS_SYSTEM_FUNCTION (function, sqr           , "SQR"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_sqr           );
PS_SYSTEM_FUNCTION (function, sqrt          , "SQRT"        , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_sqrt          );
PS_SYSTEM_FUNCTION (function, succ          , "SUCC"        , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_succ          );
PS_SYSTEM_FUNCTION (function, tan           , "TAN"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_tan           );
PS_SYSTEM_FUNCTION (function, trunc         , "TRUNC"       , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_trunc         );
// Ordinal
PS_SYSTEM_FUNCTION (function, chr           , "CHR"         , PS_EXECUTABLE_FUNC_1ARG,   .func_1arg  , &ps_function_chr           );
PS_SYSTEM_FUNCTION (function, high          , "HIGH"        , PS_EXECUTABLE_FUNC_1ARG_S, .func_1arg_s, &ps_function_high          );
PS_SYSTEM_FUNCTION (function, low           , "LOW"         , PS_EXECUTABLE_FUNC_1ARG_S, .func_1arg_s, &ps_function_low           );
PS_SYSTEM_FUNCTION (function, ord           , "ORD"         , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_ord           );
PS_SYSTEM_FUNCTION (function, pred          , "PRED"        , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_pred          );
// String
PS_SYSTEM_FUNCTION (function, length        , "LENGTH"      , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_length        );
PS_SYSTEM_FUNCTION (function, lowercase     , "LOWERCASE"   , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_lowercase     );
PS_SYSTEM_FUNCTION (function, uppercase     , "UPPERCASE"   , PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_uppercase     );
// System
PS_SYSTEM_FUNCTION (function, get_tick_count, "GETTICKCOUNT", PS_EXECUTABLE_FUNC_1ARG  , .func_1arg  , &ps_function_get_tick_count);
/* clang-format on */

bool ps_functions_init(ps_ast_block *system)
{
    // Math
    ADD_SYSTEM_SYMBOL(ps_system_function_abs)
    ADD_SYSTEM_SYMBOL(ps_system_function_arctan)
    ADD_SYSTEM_SYMBOL(ps_system_function_cos)
    ADD_SYSTEM_SYMBOL(ps_system_function_even)
    ADD_SYSTEM_SYMBOL(ps_system_function_exp)
    ADD_SYSTEM_SYMBOL(ps_system_function_frac)
    ADD_SYSTEM_SYMBOL(ps_system_function_int)
    ADD_SYSTEM_SYMBOL(ps_system_function_ln)
    ADD_SYSTEM_SYMBOL(ps_system_function_log)
    ADD_SYSTEM_SYMBOL(ps_system_function_odd)
    ADD_SYSTEM_SYMBOL(ps_system_function_power)
    ADD_SYSTEM_SYMBOL(ps_system_function_random)
    ADD_SYSTEM_SYMBOL(ps_system_function_round)
    ADD_SYSTEM_SYMBOL(ps_system_function_sin)
    ADD_SYSTEM_SYMBOL(ps_system_function_sqr)
    ADD_SYSTEM_SYMBOL(ps_system_function_sqrt)
    ADD_SYSTEM_SYMBOL(ps_system_function_succ)
    ADD_SYSTEM_SYMBOL(ps_system_function_tan)
    ADD_SYSTEM_SYMBOL(ps_system_function_trunc)
    // Ordinal
    ADD_SYSTEM_SYMBOL(ps_system_function_chr)
    ADD_SYSTEM_SYMBOL(ps_system_function_high)
    ADD_SYSTEM_SYMBOL(ps_system_function_low)
    ADD_SYSTEM_SYMBOL(ps_system_function_ord)
    ADD_SYSTEM_SYMBOL(ps_system_function_pred)
    // String
    ADD_SYSTEM_SYMBOL(ps_system_function_length)
    ADD_SYSTEM_SYMBOL(ps_system_function_lowercase)
    ADD_SYSTEM_SYMBOL(ps_system_function_uppercase)
    // System
    ADD_SYSTEM_SYMBOL(ps_system_function_get_tick_count)
    return true;
error:
    return false;
}

ps_error ps_function_exec_1arg(ps_interpreter *interpreter, const ps_symbol *symbol, const ps_value *value,
                               ps_value *result)
{
    assert(interpreter != NULL);
    assert(symbol != NULL);
    assert(symbol->value != NULL);
    assert(symbol->value->data.x != NULL);
    ps_function_1arg function = symbol->value->data.x->func_1arg;
    if (function != &ps_function_random && function != &ps_function_get_tick_count)
        assert(value != NULL);
    assert(result != NULL);
    if (function == NULL)
    {
        ps_interpreter_set_message(interpreter, "Function '%s' not implemented", symbol->name);
        return PS_ERROR_NOT_IMPLEMENTED;
    }
    return function(interpreter, value, result);
}

ps_error ps_function_exec_1arg_s(ps_interpreter *interpreter, const ps_symbol *symbol, ps_symbol *type,
                                 ps_value *result)
{
    assert(interpreter != NULL);
    assert(symbol != NULL);
    assert(symbol->value != NULL);
    assert(result != NULL);
    ps_function_1arg_s function = (ps_function_1arg_s)(symbol->value->data.x->func_1arg_s);
    if (function == NULL)
    {
        ps_interpreter_set_message(interpreter, "Function '%s' not implemented", symbol->name);
        return PS_ERROR_NOT_IMPLEMENTED;
    }
    return function(interpreter, type, result);
}

ps_error ps_function_exec_2args(ps_interpreter *interpreter, const ps_symbol *symbol, const ps_value *a,
                                const ps_value *b, ps_value *result)
{
    assert(interpreter != NULL);
    assert(symbol != NULL);
    assert(symbol->value != NULL);
    assert(a != NULL);
    assert(b != NULL);
    assert(result != NULL);
    ps_function_2args function = (ps_function_2args)(symbol->value->data.x->func_2args);
    if (function == NULL)
    {
        ps_interpreter_set_message(interpreter, "Function '%s' not implemented", symbol->name);
        return PS_ERROR_NOT_IMPLEMENTED;
    }
    return function(interpreter, a, b, result);
}

ps_error ps_function_return_error_with_message(ps_interpreter *interpreter, ps_error error, // NOSONAR
                                               const char *format, ...)
{
    va_list args;
    va_start(args, format);
    vsnprintf(interpreter->message, sizeof(interpreter->message), format, args); // NOSONAR
    va_end(args);
    return error;
}

/******************************************************************************/
/* ORDINAL                                                                    */
/******************************************************************************/

ps_error ps_function_odd(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_ordinal(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_ORDINAL,
                                                     "Odd: Ordinal expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_boolean;
    switch (ps_value_get_base(value))
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)((value->data.u & 1) != 0);
        break;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)((value->data.i & 1) != 0);
        break;
    default:
        break;
    }
    return PS_ERROR_NONE;
}

ps_error ps_function_even(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_ordinal(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_ORDINAL,
                                                     "Even: Ordinal expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_boolean;
    switch (ps_value_get_base(value))
    {
    case PS_TYPE_UNSIGNED:
        result->data.b = (ps_boolean)((value->data.u & 1) == 0);
        break;
    case PS_TYPE_INTEGER:
        result->data.b = (ps_boolean)((value->data.i & 1) == 0);
        break;
    default:
        break;
    }
    return PS_ERROR_NONE;
}

ps_error ps_function_ord(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_ordinal(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Ord: Ordinal expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    switch (ps_value_get_base(value))
    {
    case PS_TYPE_BOOLEAN:
        // ord(false) => 0 / ord(true) => 1
        result->data.u = value->data.b ? 1 : 0;
        break;
    case PS_TYPE_CHAR:
        // ord('0') => 48 / ord('A') => 65 / ...
        result->data.u = (ps_unsigned)(value->data.c);
        break;
    case PS_TYPE_UNSIGNED:
        // enum has unsigned as base type
        result->data.u = value->data.u;
        break;
    default:
        break;
    }
    result->type = &ps_system_unsigned;
    return PS_ERROR_NONE;
}

ps_error ps_function_chr(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_ordinal(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_ORDINAL,
                                                     "Chr: Ordinal expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_char;
    switch (ps_value_get_base(value))
    {
    case PS_TYPE_UNSIGNED:
        if (interpreter->range_check && value->data.u > PS_CHAR_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = (ps_char)(value->data.u);
        break;
    case PS_TYPE_INTEGER:
        if (interpreter->range_check && (value->data.i < 0 || value->data.i > PS_CHAR_MAX))
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = (ps_char)(value->data.i);
        break;
    default:
        break;
    }
    return PS_ERROR_NONE;
}

ps_error ps_function_low_or_high_subrange(const ps_symbol *type, ps_value *result, bool low)
{
    const ps_type_definition *type_def = type->value->data.t;
    if (type_def->type != PS_TYPE_SUBRANGE)
        return PS_ERROR_UNEXPECTED_TYPE;
    switch (type_def->base)
    {
    case PS_TYPE_CHAR:
        result->type = &ps_system_char;
        result->data.c = low ? type_def->def.g.c.min : type_def->def.g.c.max;
        break;
    case PS_TYPE_INTEGER:
        result->type = &ps_system_integer;
        result->data.i = low ? type_def->def.g.i.min : type_def->def.g.i.max;
        break;
    case PS_TYPE_UNSIGNED:
        result->type = &ps_system_unsigned;
        result->data.u = low ? type_def->def.g.u.min : type_def->def.g.u.max;
        break;
    case PS_TYPE_ENUM:
        result->type = (ps_symbol *)type;
        result->data.u = low ? type_def->def.g.e.min : type_def->def.g.e.max;
        break;
    default:
        return PS_ERROR_UNEXPECTED_TYPE;
    }
    return PS_ERROR_NONE;
}

ps_error ps_function_low_or_high_type(ps_interpreter *interpreter, ps_symbol *type, ps_value *result, bool low)
{
    const ps_type_definition *type_def = type->value->data.t;
    switch (type_def->type)
    {
    case PS_TYPE_CHAR:
        result->type = &ps_system_char;
        result->data.c = low ? (ps_char)'\0' : PS_CHAR_MAX;
        break;
    case PS_TYPE_INTEGER:
        result->type = &ps_system_integer;
        result->data.i = low ? PS_INTEGER_MIN : PS_INTEGER_MAX;
        break;
    case PS_TYPE_UNSIGNED:
        result->type = &ps_system_unsigned;
        result->data.u = low ? 0 : PS_UNSIGNED_MAX;
        break;
    case PS_TYPE_ENUM:
        result->type = type;
        result->data.u = low ? 0 : type_def->def.e.count - 1;
        break;
    case PS_TYPE_BOOLEAN:
        result->type = &ps_system_boolean;
        result->data.b =
            low ? ps_system_constant_boolean_true.value->data.b : ps_system_constant_boolean_false.value->data.b;
        break;
    case PS_TYPE_ARRAY:
        ps_symbol *subrange = type_def->def.a.subrange;
        return ps_function_low_or_high_type(interpreter, subrange, result, low);
    case PS_TYPE_SUBRANGE:
        return ps_function_low_or_high_subrange(type, result, low);
    default:
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "%s: (2) Type or Variable expected, got %s", low ? "Low" : "High",
                                                     ps_type_definition_get_name(type->value->data.t));
    }
    return PS_ERROR_NONE;
}

ps_error ps_function_low_or_high(ps_interpreter *interpreter, ps_symbol *type_or_var, ps_value *result, bool low)
{
    ps_symbol *type = NULL;
    if (type_or_var->kind == PS_SYMBOL_KIND_TYPE_DEFINITION)
        type = type_or_var;
    else if (type_or_var->kind == PS_SYMBOL_KIND_VARIABLE)
        type = type_or_var->value->type;
    else
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "%s: Type or Variable expected, got %s", low ? "Low" : "High",
                                                     ps_symbol_get_kind_name(type_or_var->kind));
    return ps_function_low_or_high_type(interpreter, type, result, low);
}

ps_error ps_function_low(ps_interpreter *interpreter, ps_symbol *type, ps_value *result)
{
    return ps_function_low_or_high(interpreter, type, result, true);
}

ps_error ps_function_high(ps_interpreter *interpreter, ps_symbol *type, ps_value *result)
{
    return ps_function_low_or_high(interpreter, type, result, false);
}

ps_error ps_function_pred_IUS(const ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    switch (ps_value_get_type(value))
    {
    case PS_TYPE_INTEGER:
        // pred(min) => error / pred(i) => i - 1
        if (interpreter->range_check && value->data.i == PS_INTEGER_MIN)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.i = value->data.i - 1;
        break;
    case PS_TYPE_UNSIGNED:
        // pred(0) => error / pred(u) => u - 1
        if (interpreter->range_check && value->data.u == 0)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.u = value->data.u - 1;
        break;
    case PS_TYPE_SUBRANGE:
        switch (ps_value_get_base(value))
        {
        case PS_TYPE_CHAR:
            if (interpreter->range_check && value->data.c <= value->type->value->data.t->def.g.c.min)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.c = value->data.c - 1;
            break;
        case PS_TYPE_INTEGER:
            if (interpreter->range_check && value->data.i <= value->type->value->data.t->def.g.i.min)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.i = value->data.i - 1;
            break;
        case PS_TYPE_UNSIGNED:
            if (interpreter->range_check && value->data.u <= value->type->value->data.t->def.g.u.min)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.u = value->data.u - 1;
            break;
        case PS_TYPE_ENUM:
            if (interpreter->range_check && value->data.u <= value->type->value->data.t->def.g.e.min)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.u = value->data.u - 1;
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

ps_error ps_function_pred_ordinal(const ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    switch (ps_value_get_type(value))
    {
    case PS_TYPE_CHAR:
        // pred(NUL) => error / pred(c) => c - 1
        if (interpreter->range_check && value->data.c == 0)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = (ps_char)(value->data.c - 1);
        break;
    case PS_TYPE_ENUM:
        // pred(0) => error / pred(u) => u - 1
        if (interpreter->range_check && value->data.u == 0)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.u = value->data.u - 1;
        break;
    case PS_TYPE_BOOLEAN:
        // pred(true) => false / pred(false) => error
        if (interpreter->range_check && !value->data.b)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.b = ps_system_constant_boolean_false.value->data.b;
        break;
    default:
        break;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

ps_error ps_function_pred(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_valid(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_VALUE, "Pred: Invalid value");
    if (ps_value_is_integer(value) || ps_value_is_unsigned(value) || ps_value_is_subrange(value))
        return ps_function_pred_IUS(interpreter, value, result);
    if (ps_value_is_ordinal(value))
        return ps_function_pred_ordinal(interpreter, value, result);
    return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                 "Pred: Ordinal expected, got %s",
                                                 ps_type_definition_get_name(value->type->value->data.t));
}

ps_error ps_function_succ_ius(const ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    switch (ps_value_get_type(value))
    {
    case PS_TYPE_INTEGER:
        // succ(max) => error / succ(i) => i + 1
        if (interpreter->range_check && value->data.i >= PS_INTEGER_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.i = value->data.i + 1;
        break;
    case PS_TYPE_UNSIGNED:
        // succ(max) => error / succ(u) => u + 1
        if (interpreter->range_check && value->data.u >= PS_UNSIGNED_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.u = value->data.u + 1;
        break;
    case PS_TYPE_SUBRANGE:
        switch (ps_value_get_base(value))
        {
        case PS_TYPE_CHAR:
            if (interpreter->range_check && value->data.c >= value->type->value->data.t->def.g.c.max)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.c = value->data.c + 1;
            break;
        case PS_TYPE_INTEGER:
            if (interpreter->range_check && value->data.i >= value->type->value->data.t->def.g.i.max)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.i = value->data.i + 1;
            break;
        case PS_TYPE_UNSIGNED:
            if (interpreter->range_check && value->data.u >= value->type->value->data.t->def.g.u.max)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.u = value->data.u + 1;
            break;
        case PS_TYPE_ENUM:
            if (interpreter->range_check && value->data.u >= value->type->value->data.t->def.g.e.max)
                return PS_ERROR_OUT_OF_RANGE;
            result->data.u = value->data.u + 1;
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

ps_error ps_function_succ_CEB(const ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    result->type = value->type;
    switch (ps_value_get_type(value))
    {
    case PS_TYPE_CHAR:
        // succ(max) => error / succ(c) => c + 1
        if (interpreter->range_check && value->data.c == PS_CHAR_MAX)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.c = (ps_char)(value->data.c + 1);
        break;
    case PS_TYPE_ENUM:
        // succ(max) => error / succ(u) => u + 1
        if (interpreter->range_check && value->data.u > (value->type->value->data.t->def.e.count))
            return PS_ERROR_OUT_OF_RANGE;
        result->data.u = value->data.u + 1;
        break;
    case PS_TYPE_BOOLEAN:
        // succ(true) => error / succ(false) => true
        if (interpreter->range_check && value->data.b)
            return PS_ERROR_OUT_OF_RANGE;
        result->data.b = ps_system_constant_boolean_true.value->data.b;
        break;
    default:
        break;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

ps_error ps_function_succ(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_valid(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_VALUE, "Succ: Invalid value");
    if (ps_value_is_integer(value) || ps_value_is_unsigned(value) || ps_value_is_subrange(value))
        return ps_function_succ_ius(interpreter, value, result);
    if (ps_value_is_ordinal(value))
        return ps_function_succ_CEB(interpreter, value, result);
    return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                 "Pred: Ordinal expected, got %s",
                                                 ps_type_definition_get_name(value->type->value->data.t));
}

/******************************************************************************/
/* MATH                                                                       */
/******************************************************************************/

ps_error ps_function_abs(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_number(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Abs: Number expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_UNSIGNED:
        // |u| => u
        result->data.u = value->data.u;
        break;
    case PS_TYPE_INTEGER:
        // |i| => abs(i)
        result->data.i = (ps_integer)abs(value->data.i); // NOSONAR
        break;
    case PS_TYPE_REAL:
        // |r| => fabs(r)
        result->data.r = (ps_real)fabs(value->data.r);
        break;
    default:
        break;
    }
    result->type = value->type;
    return PS_ERROR_NONE;
}

ps_error ps_function_trunc(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Trunc: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    double r = trunc(value->data.r);
    if (interpreter->range_check && (r < (ps_real)PS_INTEGER_MIN || r > (ps_real)PS_INTEGER_MAX))
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_integer;
    result->data.i = (ps_integer)r;
    return PS_ERROR_NONE;
}

ps_error ps_function_round(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Round: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    double r = round(value->data.r);
    if (interpreter->range_check && (r < PS_INTEGER_MIN || r > PS_INTEGER_MAX))
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_integer;
    result->data.i = (ps_integer)r;
    return PS_ERROR_NONE;
}

ps_error ps_function_int(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Int: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    if (ps_value_get_base(value) != PS_TYPE_REAL)
        return PS_ERROR_EXPECTED_REAL;
    result->type = &ps_system_real;
    double r;
    modf(value->data.r, &r);
    result->data.r = (ps_real)r;
    return PS_ERROR_NONE;
}

ps_error ps_function_frac(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Frac: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    double i;
    result->data.r = (ps_real)modf(value->data.r, &i);
    result->type = &ps_system_real;
    return PS_ERROR_NONE;
}

ps_error ps_function_sin(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Sin: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_real;
    result->data.r = (ps_real)sin(value->data.r);
    return PS_ERROR_NONE;
}

ps_error ps_function_cos(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Cos: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_real;
    result->data.r = (ps_real)cos(value->data.r);
    return PS_ERROR_NONE;
}

ps_error ps_function_tan(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Tan: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    double c = cos(value->data.r);
    if (c == 0.0)
        return PS_ERROR_DIVISION_BY_ZERO;
    double s = sin(value->data.r);
    double r = s / c;
    if (interpreter->range_check && (r < PS_REAL_MIN || r > PS_REAL_MAX))
        return PS_ERROR_OUT_OF_RANGE;
    result->data.r = (ps_real)r;
    result->type = &ps_system_real;
    return PS_ERROR_NONE;
}

ps_error ps_function_arctan(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "ArcTan: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    double r = atan(value->data.r);
    if (errno != 0 || isnan(r) || isinf(r))
        return PS_ERROR_MATH_NAN_INF;
    if (interpreter->range_check && (r < PS_REAL_MIN || r > PS_REAL_MAX))
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_real;
    result->data.r = (ps_real)r;
    return PS_ERROR_NONE;
}

ps_error ps_function_sqr(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Sqr: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    double r = value->data.r * value->data.r;
    if (interpreter->range_check && (r < PS_REAL_MIN || r > PS_REAL_MAX))
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_real;
    result->data.r = (ps_real)r;
    return PS_ERROR_NONE;
}

ps_error ps_function_sqrt(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Sqrt: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    if (value->data.r < 0.0)
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_real;
    result->data.r = (ps_real)sqrt(value->data.r);
    return PS_ERROR_NONE;
}

ps_error ps_function_exp(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Exp: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_real;
    double r = exp(value->data.r);
    if (interpreter->range_check && r > PS_REAL_MAX)
        return PS_ERROR_OUT_OF_RANGE;
    result->data.r = (ps_real)r;
    return PS_ERROR_NONE;
}

ps_error ps_function_ln(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE, "Ln: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    if (value->data.r <= 0.0)
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_real;
    result->data.r = (ps_real)log(value->data.r);
    return PS_ERROR_NONE;
}

ps_error ps_function_log(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_real(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_UNEXPECTED_TYPE,
                                                     "Log: Real expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    if (value->data.r <= 0.0)
        return PS_ERROR_OUT_OF_RANGE;
    result->type = &ps_system_real;
    result->data.r = (ps_real)log10(value->data.r);
    return PS_ERROR_NONE;
}

ps_error ps_function_power(ps_interpreter *interpreter, const ps_value *a, const ps_value *b, ps_value *result)
{
    if (!ps_value_is_real(a) || !ps_value_is_real(b))
        return ps_function_return_error_with_message(
            interpreter, PS_ERROR_EXPECTED_REAL, "Power: Reals expected, got %s and %s",
            ps_type_definition_get_name(a->type->value->data.t), ps_type_definition_get_name(b->type->value->data.t));
    result->type = &ps_system_real;
    double r = pow(a->data.r, b->data.r);
    if (interpreter->range_check && (r < PS_REAL_MIN || r > PS_REAL_MAX))
        return PS_ERROR_OUT_OF_RANGE;
    result->data.r = (ps_real)r;
    return PS_ERROR_NONE;
}

/******************************************************************************/
/* STRINGS                                                                    */
/******************************************************************************/

ps_error ps_function_length(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_string(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_STRING,
                                                     "Length: String expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_unsigned;
    result->data.u = value->data.s->len;
    return PS_ERROR_NONE;
}

ps_error ps_function_lowercase(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_string(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_STRING,
                                                     "LowerCase: String expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    ps_string *s = ps_string_lowercase(value->data.s);
    if (s == NULL)
        return PS_ERROR_OUT_OF_MEMORY;
    result->type = &ps_system_string;
    result->data.s = s;
    return PS_ERROR_NONE;
}

ps_error ps_function_uppercase(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (!ps_value_is_string(value))
        return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_STRING,
                                                     "UpperCase: String expected, got %s",
                                                     ps_type_definition_get_name(value->type->value->data.t));
    result->type = &ps_system_string;
    result->data.s = ps_string_uppercase(value->data.s);
    if (result->data.s == NULL)
        return PS_ERROR_OUT_OF_MEMORY;
    return PS_ERROR_NONE;
}

/******************************************************************************/
/* OTHER                                                                      */
/******************************************************************************/

ps_error ps_function_get_tick_count(ps_interpreter *interpreter, const ps_value *value, ps_value *result) // NOSONAR
{
    // NB: interprter & value parameters are not used
    ((void)(interpreter));
    ((void)value);
    result->type = &ps_system_unsigned;
    clock_t c = clock();
    result->data.u = (ps_unsigned)((c * 1000) / CLOCKS_PER_SEC);
    return PS_ERROR_NONE;
}

// from https://www.cs.yale.edu/homes/aspnes/pinewiki/C(2f)Randomization.html
//   => correct "bias" introduced by modulus operator
int rand_range_integer(int n)
{
    int r;
    int limit;

    limit = RAND_MAX - (RAND_MAX % n);
    while ((r = rand()) >= limit)
        ;

    return r % n;
}

unsigned int rand_range_unsigned(unsigned int n)
{
    // use rand_range_integer() for small values
    if (n <= RAND_MAX)
    {
        return (unsigned int)rand_range_integer((int)n);
    }
    // use double for larger values
    return (unsigned int)((double)rand() * (double)n / (double)RAND_MAX);
}

ps_error ps_function_random(ps_interpreter *interpreter, const ps_value *value, ps_value *result)
{
    if (value == NULL)
    {
        // no argument, return random real between 0.0 included and 1.0 excluded
        result->type = &ps_system_real;
        do
        {
            double r = (double)rand() / (double)RAND_MAX;
            result->data.r = (ps_real)r;
        } while (result->data.r >= 1.0);
    }
    else
    {
        // one argument, return random integer / unsigned
        if (!(ps_value_is_number(value) && !ps_value_is_real(value)))
            return ps_function_return_error_with_message(interpreter, PS_ERROR_EXPECTED_SCALAR,
                                                         "Random: Integer or Unsigned expected, got %s",
                                                         ps_type_definition_get_name(value->type->value->data.t));
        switch (ps_value_get_base(value))
        {
        case PS_TYPE_INTEGER:
            result->type = &ps_system_integer;
            result->data.i = (ps_integer)rand_range_integer(value->data.i); // NOSONAR
            break;
        case PS_TYPE_UNSIGNED:
            result->type = &ps_system_unsigned;
            result->data.u = (ps_unsigned)rand_range_unsigned(value->data.u); // NOSONAR
            break;
        default:
            return PS_ERROR_UNEXPECTED_TYPE;
        }
    }
    return PS_ERROR_NONE;
}

/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_token.h"
#include "ps_value.h"

#define RETURN_ERROR(__PS_ERROR__)                                                                                     \
    {                                                                                                                  \
        interpreter->error = __PS_ERROR__;                                                                             \
        return false;                                                                                                  \
    }

// typedef bool (*ps_binary_func)(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result);

/* clang-format off */
#define II (PS_TYPE_INTEGER  << 4 | PS_TYPE_INTEGER )
#define IU (PS_TYPE_INTEGER  << 4 | PS_TYPE_UNSIGNED)
#define UI (PS_TYPE_UNSIGNED << 4 | PS_TYPE_INTEGER )
#define UU (PS_TYPE_UNSIGNED << 4 | PS_TYPE_UNSIGNED)
#define RR (PS_TYPE_REAL     << 4 | PS_TYPE_REAL    )
#define RI (PS_TYPE_REAL     << 4 | PS_TYPE_INTEGER )
#define RU (PS_TYPE_REAL     << 4 | PS_TYPE_UNSIGNED)
#define IR (PS_TYPE_INTEGER  << 4 | PS_TYPE_REAL    )
#define UR (PS_TYPE_UNSIGNED << 4 | PS_TYPE_REAL    )
#define BB (PS_TYPE_BOOLEAN  << 4 | PS_TYPE_BOOLEAN )
#define CC (PS_TYPE_CHAR     << 4 | PS_TYPE_CHAR    )
#define CS (PS_TYPE_CHAR     << 4 | PS_TYPE_STRING  )
#define SC (PS_TYPE_STRING   << 4 | PS_TYPE_CHAR    )
#define SS (PS_TYPE_STRING   << 4 | PS_TYPE_STRING  )
/* clang-format on */

#define NUMBER_CASE(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__)                                                    \
    case (__KEY__):                                                                                                    \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
        break;

#define NUMBER_CASE_DIV_MOD(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__)                                            \
    case (__KEY__):                                                                                                    \
        if (b->data.__B__ == 0)                                                                                        \
            RETURN_ERROR(PS_RUNTIME_ERROR_DIVISION_BY_ZERO)                                                            \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
        break;

#define NUMBER_CASE_DIV_REAL(__KEY__, __A__, __B__)                                                                    \
    case (__KEY__):                                                                                                    \
        if ((ps_real)(b->data.__B__) == 0.0)                                                                           \
            RETURN_ERROR(PS_RUNTIME_ERROR_DIVISION_BY_ZERO)                                                            \
        result->data.r = (ps_real)(a->data.__A__) / (ps_real)(b->data.__B__);                                          \
        r = PS_TYPE_REAL;                                                                                              \
        break;

#define STRING_CASE_CS(__KEY__, __OP__)                                                                                \
    case (__KEY__):                                                                                                    \
        s = ps_string_alloc(1);                                                                                        \
        if (s == NULL)                                                                                                 \
            return false;                                                                                              \
        s->str[0] = a->data.c;                                                                                         \
        s->len = 1;                                                                                                    \
        result->data.b = __OP__(s, b->data.s);                                                                         \
        ps_string_free(s);                                                                                             \
        r = PS_TYPE_BOOLEAN;                                                                                           \
        break;

#define STRING_CASE_SC(__KEY__, __OP__)                                                                                \
    case (__KEY__):                                                                                                    \
        s = ps_string_alloc(1);                                                                                        \
        if (s == NULL)                                                                                                 \
            return false;                                                                                              \
        s->str[0] = a->data.c;                                                                                         \
        s->len = 1;                                                                                                    \
        result->data.b = __OP__(a->data.s, s);                                                                         \
        ps_string_free(s);                                                                                             \
        r = PS_TYPE_BOOLEAN;                                                                                           \
        break;

#define STRING_CASE_SS(__KEY__, __OP__)                                                                                \
    case (__KEY__):                                                                                                    \
        result->data.b = __OP__(a->data.s, b->data.s);                                                                 \
        r = PS_TYPE_BOOLEAN;                                                                                           \
        break;

bool ps_string_eq(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) == 0;
}

bool ps_string_ne(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) != 0;
}

bool ps_string_lt(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) < 0;
}

bool ps_string_le(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) <= 0;
}

bool ps_string_gt(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) > 0;
}

bool ps_string_ge(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) >= 0;
}

bool ps_function_binary_op(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result,
                           ps_token_type token_type)
{
    // //           TTTTTTTTTT      | AAAA                 | BBBB
    // //           ----------------+----------------------+--------------
    uint16_t key = token_type << 8 | (a->type->base << 4) | b->type->base;
    // ps_operator_binary_entry entry = {0, PS_TYPE_NONE, NULL};
    // for (size_t i = 0; i < PS_OPERATOR_BINARY_TABLE_COUNT; i++)
    // {
    //     if (ps_operator_binary_table[i].k == key)
    //     {
    //         entry = ps_operator_binary_table[i];
    //         break;
    //     }
    // }
    // if (entry.k == 0)
    //     RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE)
    // if (!((*entry.f)(interpreter, a, b, result)))
    //     return false;
    ps_value_type r = PS_TYPE_NONE;
    ps_string *s = NULL;
    switch (key)
    {
        NUMBER_CASE(PS_TOKEN_AND << 8 | II, i, i, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_AND << 8 | IU, i, u, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_AND << 8 | UI, u, i, &, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_AND << 8 | UU, u, u, &, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_AND << 8 | BB, b, b, &&, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_OR << 8 | II, i, i, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_OR << 8 | IU, i, u, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_OR << 8 | UI, u, i, &, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_OR << 8 | UU, u, u, &, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_OR << 8 | BB, b, b, ||, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | II, i, i, ^, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | IU, i, u, ^, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | UI, u, i, ^, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | UU, u, u, ^, u, PS_TYPE_UNSIGNED)
    case PS_TOKEN_XOR << 8 | BB:
        result->data.b = !(a->data.b) != !(b->data.b);
        r = PS_TYPE_BOOLEAN;
        break;
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | II, i, i, +, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | IU, i, u, +, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | UI, u, i, +, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | UU, u, u, +, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | RR, r, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | RI, r, i, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | RU, r, u, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | IR, i, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | UR, u, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | II, i, i, -, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | IU, i, u, -, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | UI, u, i, -, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | UU, u, u, -, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | RR, r, r, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | RI, r, i, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | RU, r, u, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | IR, i, r, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | UR, u, r, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | II, i, i, *, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | IU, i, u, *, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | UI, u, i, *, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | UU, u, u, *, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | RR, r, r, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | RI, r, i, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | RU, r, u, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | IR, i, r, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | UR, u, r, *, r, PS_TYPE_REAL)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | II, i, i, /, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | IU, i, u, /, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | UI, u, i, /, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | UU, u, u, /, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | II, i, i, %, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | IU, i, u, %, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | UI, u, i, %, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | UU, u, u, %, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | II, i, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | IU, i, u)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | UI, u, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | UU, u, u)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | RR, r, r)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | RI, r, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | RU, r, u)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | IR, i, r)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | UR, u, r)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | II, i, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | IU, i, u, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | UI, u, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | UU, u, u, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | RR, r, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | RI, r, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | RU, r, u, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | IR, i, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | UR, u, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQUAL << 8 | CC, c, c, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | II, i, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | IU, i, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | UI, u, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | UU, u, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | RR, r, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | RI, r, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | RU, r, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | IR, i, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | UR, u, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NOT_EQUAL << 8 | CC, c, c, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | II, i, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | IU, i, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | UI, u, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | UU, u, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | RR, r, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | RI, r, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | RU, r, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | IR, i, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | UR, u, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_THAN << 8 | CC, c, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | II, i, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | IU, i, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | UI, u, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | UU, u, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | RR, r, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | RI, r, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | RU, r, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | IR, i, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | UR, u, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LESS_OR_EQUAL << 8 | CC, c, c, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | II, i, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | IU, i, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | UI, u, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | UU, u, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | RR, r, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | RI, r, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | RU, r, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | IR, i, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | UR, u, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_THAN << 8 | CC, c, c, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | II, i, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | IU, i, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | UI, u, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | UU, u, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | RR, r, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | RI, r, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | RU, r, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | IR, i, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | UR, u, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GREATER_OR_EQUAL << 8 | CC, c, c, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | II, i, i, <<, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | IU, i, u, <<, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | UI, u, i, <<, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | UU, u, u, <<, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | II, i, i, >>, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | IU, i, u, >>, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | UI, u, i, >>, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | UU, u, u, >>, u, PS_TYPE_UNSIGNED)
    case PS_TOKEN_PLUS << 8 | CC:
        s = ps_string_alloc(2);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->str[1] = b->data.c;
        s->len = 2;
        result->data.s = s;
        break;
    case PS_TOKEN_PLUS << 8 | CS:
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(s, b->data.s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        break;
    case PS_TOKEN_PLUS << 8 | SC:
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = b->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(a->data.s, s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        break;

    case PS_TOKEN_PLUS << 8 | SS:
        s = ps_string_concat(a->data.s, b->data.s, PS_STRING_MAX_LEN);
        if (s == NULL)
            return false;
        result->data.s = s;
        break;
        STRING_CASE_CS(PS_TOKEN_EQUAL << 8 | CS, ps_string_eq)
        STRING_CASE_SC(PS_TOKEN_EQUAL << 8 | SC, ps_string_eq)
        STRING_CASE_SS(PS_TOKEN_EQUAL << 8 | SS, ps_string_eq)
        STRING_CASE_CS(PS_TOKEN_NOT_EQUAL << 8 | CS, ps_string_ne)
        STRING_CASE_SC(PS_TOKEN_NOT_EQUAL << 8 | SC, ps_string_ne)
        STRING_CASE_SS(PS_TOKEN_NOT_EQUAL << 8 | SS, ps_string_ne)
        STRING_CASE_CS(PS_TOKEN_LESS_THAN << 8 | CS, ps_string_lt)
        STRING_CASE_SC(PS_TOKEN_LESS_THAN << 8 | SC, ps_string_lt)
        STRING_CASE_SS(PS_TOKEN_LESS_THAN << 8 | SS, ps_string_lt)
        STRING_CASE_CS(PS_TOKEN_LESS_OR_EQUAL << 8 | CS, ps_string_le)
        STRING_CASE_SC(PS_TOKEN_LESS_OR_EQUAL << 8 | SC, ps_string_le)
        STRING_CASE_SS(PS_TOKEN_LESS_OR_EQUAL << 8 | SS, ps_string_le)
        STRING_CASE_CS(PS_TOKEN_GREATER_THAN << 8 | CS, ps_string_gt)
        STRING_CASE_SC(PS_TOKEN_GREATER_THAN << 8 | SC, ps_string_gt)
        STRING_CASE_SS(PS_TOKEN_GREATER_THAN << 8 | SS, ps_string_gt)
        STRING_CASE_CS(PS_TOKEN_GREATER_OR_EQUAL << 8 | CS, ps_string_ge)
        STRING_CASE_SC(PS_TOKEN_GREATER_OR_EQUAL << 8 | SC, ps_string_ge)
        STRING_CASE_SS(PS_TOKEN_GREATER_OR_EQUAL << 8 | SS, ps_string_ge)
    default:
        if (interpreter->debug)
        {
            fprintf(stderr, "*** ERROR: [1] Unknown binary operator %s for types %s and %s\n",
                    ps_token_get_keyword(token_type), ps_value_get_type_name(a->type->base),
                    ps_value_get_type_name(b->type->base));
        }
        RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE);
    }
    if (result->type != NULL && result->type->base != PS_TYPE_NONE)
    {
        // check if the result type is compatible with the entry's result type
        if (result->type->base != r)
        {
            if (interpreter->debug)
            {
                fprintf(stderr, "*** ERROR: [2] Unknown binary operator %s for types %s and %s\n",
                        ps_token_get_keyword(token_type), ps_value_get_type_name(a->type->base),
                        ps_value_get_type_name(b->type->base));
            }
            RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE)
        }
    }
    else
    {
        // if we don't know what the result type is, we take entry's result type converting base type to the system type
        switch (r)
        {
        case PS_TYPE_REAL:
            result->type = ps_system_real.value->data.t;
            break;
        case PS_TYPE_INTEGER:
            result->type = ps_system_integer.value->data.t;
            break;
        case PS_TYPE_UNSIGNED:
            result->type = ps_system_unsigned.value->data.t;
            break;
        case PS_TYPE_BOOLEAN:
            result->type = ps_system_boolean.value->data.t;
            break;
        case PS_TYPE_CHAR:
            result->type = ps_system_char.value->data.t;
            break;
        case PS_TYPE_STRING:
            result->type = ps_system_string.value->data.t;
            break;
        default:
            if (interpreter->debug)
            {
                fprintf(stderr, "*** ERROR: [3] Unknown binary operator %s for types %s and %s\n",
                        ps_token_get_keyword(token_type), ps_value_get_type_name(a->type->base),
                        ps_value_get_type_name(b->type->base));
            }
            RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE)
        }
    }
    return true;
}

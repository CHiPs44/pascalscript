/*
    This file is part of the PascalScript Pascal vm.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <string.h>
#include <time.h>

#include "ps_error.h"
#include "ps_functions.h"
#include "ps_interpreter.h"
#include "ps_operator.h"
#include "ps_string.h"
#include "ps_system.h"
#include "ps_value.h"

/**
 * @brief Compute
 *  - bitwise not for integer / unsigned,
 *  - logical not for boolean,
 *  - negative for integer / real
 *  and return true
 *  otherwise return false and set PS_ERROR_OPERATOR_NOT_APPLICABLE
 */
bool ps_operator_unary_eval(ps_interpreter *interpreter, const ps_value *value, ps_value *result,
                            ps_operator_unary operator)
{
    result->type = value->type;
    // NB: with FPC, not(subrange) or not(enum) yields integer result without range checking
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_INTEGER:
        switch (operator)
        {
        case PS_OP_NOT:
            result->data.i = ~value->data.i;
            break;
        case PS_OP_NEG:
            result->data.i = -value->data.i;
            break;
        default:
            ps_interpreter_set_message(interpreter, "Unexpected operator %d for INTEGER unary operation", operator);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_UNSIGNED:
        if (operator == PS_OP_NOT)
        {
            result->data.u = ~value->data.u;
        }
        else if (operator == PS_OP_NEG)
        {
            if (interpreter->range_check && value->data.u > PS_INTEGER_MAX)
                return ps_interpreter_return_false(interpreter, PS_ERROR_OUT_OF_RANGE);
            result->type = &ps_system_integer;
            result->data.i = -((ps_integer)(value->data.u));
        }
        else
        {
            ps_interpreter_set_message(interpreter, "Unexpected operator %d for UNSIGNED unary operation", operator);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_REAL:
        if (operator == PS_OP_NEG)
        {
            result->data.r = -value->data.r;
        }
        else
        {
            ps_interpreter_set_message(interpreter, "Unexpected operator %d for REAL unary operation", operator);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_BOOLEAN:
        if (operator == PS_OP_NOT)
        {
            result->data.b = !value->data.b;
        }
        else
        {

            ps_interpreter_set_message(interpreter, "Unexpected operator %d for BOOLEAN unary operation", operator);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    default:
        return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
    }
    return true;
}

/* All sensible base types combinations in Pascal */

/* clang-format off */
#define BB (PS_TYPE_BOOLEAN  << 4 | PS_TYPE_BOOLEAN )
#define II (PS_TYPE_INTEGER  << 4 | PS_TYPE_INTEGER )
#define IU (PS_TYPE_INTEGER  << 4 | PS_TYPE_UNSIGNED)
#define UI (PS_TYPE_UNSIGNED << 4 | PS_TYPE_INTEGER )
#define UU (PS_TYPE_UNSIGNED << 4 | PS_TYPE_UNSIGNED)
#define IR (PS_TYPE_INTEGER  << 4 | PS_TYPE_REAL    )
#define RI (PS_TYPE_REAL     << 4 | PS_TYPE_INTEGER )
#define RR (PS_TYPE_REAL     << 4 | PS_TYPE_REAL    )
#define RU (PS_TYPE_REAL     << 4 | PS_TYPE_UNSIGNED)
#define UR (PS_TYPE_UNSIGNED << 4 | PS_TYPE_REAL    )
#define CC (PS_TYPE_CHAR     << 4 | PS_TYPE_CHAR    )
#define CS (PS_TYPE_CHAR     << 4 | PS_TYPE_STRING  )
#define SC (PS_TYPE_STRING   << 4 | PS_TYPE_CHAR    )
#define SS (PS_TYPE_STRING   << 4 | PS_TYPE_STRING  )
/* clang-format on */

#define NUMBER_CASE(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__)                                                    \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
    }                                                                                                                  \
    else

#define NUMBER_CASE_SIGNED(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__, __CAST__)                                   \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        result->data.__R__ = a->data.__A__ __OP__(__CAST__) b->data.__B__;                                             \
        r = __TYPE__;                                                                                                  \
    }                                                                                                                  \
    else

#define NUMBER_CASE_DIV_MOD(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__)                                            \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        if (b->data.__B__ == 0)                                                                                        \
            return ps_interpreter_return_false(interpreter, PS_ERROR_DIVISION_BY_ZERO);                                \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
    }                                                                                                                  \
    else

#define NUMBER_CASE_DIV_REAL(__KEY__, __A__, __B__)                                                                    \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        if ((ps_real)(b->data.__B__) == 0.0)                                                                           \
            return ps_interpreter_return_false(interpreter, PS_ERROR_DIVISION_BY_ZERO);                                \
        result->data.r = (ps_real)(a->data.__A__) / (ps_real)(b->data.__B__);                                          \
        r = PS_TYPE_REAL;                                                                                              \
    }                                                                                                                  \
    else

#define STRING_CASE_CS(__KEY__, __OP__)                                                                                \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        s = ps_string_alloc(1);                                                                                        \
        if (s == NULL)                                                                                                 \
            return false;                                                                                              \
        s->str[0] = a->data.c;                                                                                         \
        s->len = 1;                                                                                                    \
        result->data.b = __OP__(s, b->data.s);                                                                         \
        ps_string_free(s);                                                                                             \
        r = PS_TYPE_BOOLEAN;                                                                                           \
    }                                                                                                                  \
    else

#define STRING_CASE_SC(__KEY__, __OP__)                                                                                \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        s = ps_string_alloc(1);                                                                                        \
        if (s == NULL)                                                                                                 \
            return false;                                                                                              \
        s->str[0] = a->data.c;                                                                                         \
        s->len = 1;                                                                                                    \
        result->data.b = __OP__(a->data.s, s);                                                                         \
        ps_string_free(s);                                                                                             \
        r = PS_TYPE_BOOLEAN;                                                                                           \
    }                                                                                                                  \
    else

#define STRING_CASE_SS(__KEY__, __OP__)                                                                                \
    if (key == (__KEY__))                                                                                              \
    {                                                                                                                  \
        result->data.b = __OP__(a->data.s, b->data.s);                                                                 \
        r = PS_TYPE_BOOLEAN;                                                                                           \
    }                                                                                                                  \
    else

static inline bool ps_string_eq(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) == 0;
}
static inline bool ps_string_ne(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) != 0;
}
static inline bool ps_string_lt(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) < 0;
}
static inline bool ps_string_le(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) <= 0;
}
static inline bool ps_string_gt(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) > 0;
}
static inline bool ps_string_ge(ps_string *a, ps_string *b)
{
    return ps_string_compare(a, b) >= 0;
}

bool ps_operator_eval_binary(ps_interpreter *interpreter, const ps_value *a, // NOSONAR
                             const ps_value *b, ps_value *result, ps_operator_binary operator)
{
    //                               OOOOOOOO | AAAA                                | BBBB
    //                        ----------------+-------------------------------------+-----------------------------
    uint16_t key = (uint16_t)(operator << 8 | (a->type->value->data.t->base << 4) | b->type->value->data.t->base);
    ps_value_type r = PS_TYPE_NONE;
    const ps_symbol *expected_type = result->type;
    ps_string *s = NULL;

    // AND.U/I/B
    NUMBER_CASE(PS_OP_AND << 8 | BB, b, b, &&, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_AND << 8 | II, i, i, &, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_AND << 8 | IU, i, u, &, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_AND << 8 | UI, u, i, &, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_AND << 8 | UU, u, u, &, u, PS_TYPE_UNSIGNED)
    // OR.U/I/B
    NUMBER_CASE(PS_OP_OR << 8 | BB, b, b, ||, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_OR << 8 | II, i, i, |, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_OR << 8 | IU, i, u, |, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_OR << 8 | UI, u, i, |, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_OR << 8 | UU, u, u, |, u, PS_TYPE_UNSIGNED)
    // XOR.U/I/B
    if (key == (PS_OP_XOR << 8 | BB))
    {
        // C does not have a logical exclusive or operator, so we use a xor b = !a != !b
        result->data.b = !(a->data.b) != !(b->data.b);
        r = PS_TYPE_BOOLEAN;
    }
    else
        NUMBER_CASE(PS_OP_XOR << 8 | II, i, i, ^, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_XOR << 8 | IU, i, u, ^, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_XOR << 8 | UI, u, i, ^, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_XOR << 8 | UU, u, u, ^, u, PS_TYPE_UNSIGNED)
    // ADD.I/U/R/C/S
    if (key == (PS_OP_ADD << 8 | CC)) // char + char => string
    {
        s = ps_string_alloc(2);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->str[1] = b->data.c;
        s->len = 2;
        result->data.s = s;
        r = PS_TYPE_STRING;
    }
    else if (key == (PS_OP_ADD << 8 | CS)) // char + string => string
    {
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(s, b->data.s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        if (result->data.s == NULL)
            return false;
        r = PS_TYPE_STRING;
    }
    else if (key == (PS_OP_ADD << 8 | SC)) // string + char => string
    {
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = b->data.c;
        s->str[1] = '\0';
        s->len = 1;
        result->data.s = ps_string_concat(a->data.s, s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        if (result->data.s == NULL)
            return false;
        r = PS_TYPE_STRING;
    }
    else if (key == (PS_OP_ADD << 8 | SS)) // string + string => string
    {
        s = ps_string_concat(a->data.s, b->data.s, PS_STRING_MAX_LEN);
        if (s == NULL)
            return false;
        result->data.s = s;
        r = PS_TYPE_STRING;
    }
    else
        NUMBER_CASE(PS_OP_ADD << 8 | II, i, i, +, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_ADD << 8 | IR, i, r, +, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_ADD << 8 | IU, i, u, +, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_ADD << 8 | RI, r, i, +, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_ADD << 8 | RR, r, r, +, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_ADD << 8 | RU, r, u, +, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_ADD << 8 | UI, u, i, +, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_ADD << 8 | UR, u, r, +, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_ADD << 8 | UU, u, u, +, u, PS_TYPE_UNSIGNED)
    // SUB.I/U/R
    NUMBER_CASE(PS_OP_SUB << 8 | II, i, i, -, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_SUB << 8 | IR, i, r, -, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_SUB << 8 | IU, i, u, -, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_SUB << 8 | RI, r, i, -, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_SUB << 8 | RR, r, r, -, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_SUB << 8 | RU, r, u, -, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_SUB << 8 | UI, u, i, -, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_SUB << 8 | UR, u, r, -, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_SUB << 8 | UU, u, u, -, u, PS_TYPE_UNSIGNED)
    // MUL.I/U/R
    NUMBER_CASE(PS_OP_MUL << 8 | II, i, i, *, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_MUL << 8 | IR, i, r, *, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_MUL << 8 | IU, i, u, *, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_MUL << 8 | RI, r, i, *, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_MUL << 8 | RR, r, r, *, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_MUL << 8 | RU, r, u, *, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_MUL << 8 | UI, u, i, *, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_MUL << 8 | UR, u, r, *, r, PS_TYPE_REAL)
    NUMBER_CASE(PS_OP_MUL << 8 | UU, u, u, *, u, PS_TYPE_UNSIGNED)
    // DIV.I/U
    NUMBER_CASE_DIV_MOD(PS_OP_DIV << 8 | II, i, i, /, i, PS_TYPE_INTEGER)
    NUMBER_CASE_DIV_MOD(PS_OP_DIV << 8 | IU, i, u, /, i, PS_TYPE_INTEGER)
    NUMBER_CASE_DIV_MOD(PS_OP_DIV << 8 | UI, u, i, /, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE_DIV_MOD(PS_OP_DIV << 8 | UU, u, u, /, u, PS_TYPE_UNSIGNED)
    // DIV.R
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | II, i, i)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | IR, i, i)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | IU, i, u)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | RI, r, i)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | RR, r, r)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | RU, r, u)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | UI, u, i)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | UR, u, r)
    NUMBER_CASE_DIV_REAL(PS_OP_DIV_REAL << 8 | UU, u, u)
    // MOD.I/U
    NUMBER_CASE_DIV_MOD(PS_OP_MOD << 8 | II, i, i, %, i, PS_TYPE_INTEGER)
    NUMBER_CASE_DIV_MOD(PS_OP_MOD << 8 | IU, i, u, %, i, PS_TYPE_INTEGER)
    NUMBER_CASE_DIV_MOD(PS_OP_MOD << 8 | UI, u, i, %, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE_DIV_MOD(PS_OP_MOD << 8 | UU, u, u, %, u, PS_TYPE_UNSIGNED)
    // EQ.I/U/R/C/S
    NUMBER_CASE_SIGNED(PS_OP_EQ << 8 | IU, i, u, ==, b, PS_TYPE_BOOLEAN, ps_integer)
    NUMBER_CASE_SIGNED(PS_OP_EQ << 8 | UI, u, i, ==, b, PS_TYPE_BOOLEAN, ps_unsigned)
    NUMBER_CASE(PS_OP_EQ << 8 | CC, c, c, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | II, i, i, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | IR, i, r, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | RI, r, i, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | RR, r, r, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | RU, r, u, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | UR, u, r, ==, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_EQ << 8 | UU, u, u, ==, b, PS_TYPE_BOOLEAN)
    STRING_CASE_CS(PS_OP_EQ << 8 | CS, ps_string_eq)
    STRING_CASE_SC(PS_OP_EQ << 8 | SC, ps_string_eq)
    STRING_CASE_SS(PS_OP_EQ << 8 | SS, ps_string_eq)
    // NE.I/U/R/C/S
    NUMBER_CASE_SIGNED(PS_OP_NE << 8 | IU, i, u, !=, b, PS_TYPE_BOOLEAN, ps_integer)
    NUMBER_CASE_SIGNED(PS_OP_NE << 8 | UI, u, i, !=, b, PS_TYPE_BOOLEAN, ps_unsigned)
    NUMBER_CASE(PS_OP_NE << 8 | CC, c, c, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | II, i, i, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | IR, i, r, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | RI, r, i, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | RR, r, r, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | RU, r, u, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | UR, u, r, !=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_NE << 8 | UU, u, u, !=, b, PS_TYPE_BOOLEAN)
    STRING_CASE_CS(PS_OP_NE << 8 | CS, ps_string_ne)
    STRING_CASE_SC(PS_OP_NE << 8 | SC, ps_string_ne)
    STRING_CASE_SS(PS_OP_NE << 8 | SS, ps_string_ne)
    // LT.I/U/R/C/S
    NUMBER_CASE_SIGNED(PS_OP_LT << 8 | IU, i, u, <, b, PS_TYPE_BOOLEAN, ps_integer)
    NUMBER_CASE_SIGNED(PS_OP_LT << 8 | UI, u, i, <, b, PS_TYPE_BOOLEAN, ps_unsigned)
    NUMBER_CASE(PS_OP_LT << 8 | CC, c, c, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | II, i, i, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | IR, i, r, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | RI, r, i, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | RR, r, r, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | RU, r, u, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | UR, u, r, <, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LT << 8 | UU, u, u, <, b, PS_TYPE_BOOLEAN)
    STRING_CASE_CS(PS_OP_LT << 8 | CS, ps_string_lt)
    STRING_CASE_SC(PS_OP_LT << 8 | SC, ps_string_lt)
    STRING_CASE_SS(PS_OP_LT << 8 | SS, ps_string_lt)
    // LE.I/U/R/C/S
    NUMBER_CASE_SIGNED(PS_OP_LE << 8 | IU, i, u, <=, b, PS_TYPE_BOOLEAN, ps_integer)
    NUMBER_CASE_SIGNED(PS_OP_LE << 8 | UI, u, i, <=, b, PS_TYPE_BOOLEAN, ps_unsigned)
    NUMBER_CASE(PS_OP_LE << 8 | CC, c, c, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | II, i, i, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | IR, i, r, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | RI, r, i, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | RR, r, r, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | RU, r, u, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | UR, u, r, <=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_LE << 8 | UU, u, u, <=, b, PS_TYPE_BOOLEAN)
    STRING_CASE_CS(PS_OP_LE << 8 | CS, ps_string_le)
    STRING_CASE_SC(PS_OP_LE << 8 | SC, ps_string_le)
    STRING_CASE_SS(PS_OP_LE << 8 | SS, ps_string_le)
    // GT.I/U/R/C/S
    NUMBER_CASE_SIGNED(PS_OP_GT << 8 | IU, i, u, >, b, PS_TYPE_BOOLEAN, ps_integer)
    NUMBER_CASE_SIGNED(PS_OP_GT << 8 | UI, u, i, >, b, PS_TYPE_BOOLEAN, ps_unsigned)
    NUMBER_CASE(PS_OP_GT << 8 | CC, c, c, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | II, i, i, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | IR, i, r, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | RI, r, i, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | RR, r, r, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | RU, r, u, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | UR, u, r, >, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GT << 8 | UU, u, u, >, b, PS_TYPE_BOOLEAN)
    STRING_CASE_CS(PS_OP_GT << 8 | CS, ps_string_gt)
    STRING_CASE_SC(PS_OP_GT << 8 | SC, ps_string_gt)
    STRING_CASE_SS(PS_OP_GT << 8 | SS, ps_string_gt)
    // GE.I/U/R/C/S
    NUMBER_CASE_SIGNED(PS_OP_GE << 8 | IU, i, u, >=, b, PS_TYPE_BOOLEAN, ps_integer)
    NUMBER_CASE_SIGNED(PS_OP_GE << 8 | UI, u, i, >=, b, PS_TYPE_BOOLEAN, ps_unsigned)
    NUMBER_CASE(PS_OP_GE << 8 | CC, c, c, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | II, i, i, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | IR, i, r, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | RI, r, i, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | RR, r, r, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | RU, r, u, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | UR, u, r, >=, b, PS_TYPE_BOOLEAN)
    NUMBER_CASE(PS_OP_GE << 8 | UU, u, u, >=, b, PS_TYPE_BOOLEAN)
    STRING_CASE_CS(PS_OP_GE << 8 | CS, ps_string_ge)
    STRING_CASE_SC(PS_OP_GE << 8 | SC, ps_string_ge)
    STRING_CASE_SS(PS_OP_GE << 8 | SS, ps_string_ge)
    // SHL.I/U
    NUMBER_CASE(PS_OP_SHL << 8 | II, i, i, <<, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_SHL << 8 | IU, i, u, <<, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_SHL << 8 | UI, u, i, <<, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_SHL << 8 | UU, u, u, <<, u, PS_TYPE_UNSIGNED)
    // SHR.I/U
    NUMBER_CASE(PS_OP_SHR << 8 | II, i, i, >>, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_SHR << 8 | IU, i, u, >>, i, PS_TYPE_INTEGER)
    NUMBER_CASE(PS_OP_SHR << 8 | UI, u, i, >>, u, PS_TYPE_UNSIGNED)
    NUMBER_CASE(PS_OP_SHR << 8 | UU, u, u, >>, u, PS_TYPE_UNSIGNED)
    {
        ps_interpreter_set_message(interpreter, "Binary operator %s (%d) is not applicable for types %s and %s\n",
                                   ps_operator_binary_get_name(operator), operator,
                                   ps_value_type_get_name(a->type->value->data.t->base),
                                   ps_value_type_get_name(b->type->value->data.t->base));
        return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
    }
    // Convert type to symbolic type
    switch (r)
    {
    case PS_TYPE_REAL:
        result->type = &ps_system_real;
        break;
    case PS_TYPE_INTEGER:
        result->type = &ps_system_integer;
        break;
    case PS_TYPE_UNSIGNED:
        result->type = &ps_system_unsigned;
        break;
    case PS_TYPE_BOOLEAN:
        result->type = &ps_system_boolean;
        break;
    case PS_TYPE_CHAR:
        result->type = &ps_system_char;
        break;
    case PS_TYPE_STRING:
        result->type = &ps_system_string;
        break;
    default:
        ps_interpreter_set_message(
            interpreter, "Unknown binary operator %s (%d) result type %s (%) for types %s and %s\n",
            ps_operator_binary_get_name(operator), operator, ps_value_type_get_name(r), r,
            ps_value_type_get_name(a->type->value->data.t->base), ps_value_type_get_name(b->type->value->data.t->base));
        return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
    }
    if (expected_type != &ps_system_none && result->type != expected_type)
    {
        ps_interpreter_set_message(
            interpreter, "Binary operator %s (%d) result type %s does not match expected type %s\n",
            ps_operator_binary_get_name(operator), operator, ps_value_type_get_name(result->type->value->data.t->base),
            ps_value_type_get_name(expected_type->value->data.t->base));
        return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
    }
    return true;
}

ps_operator_unary ps_operator_unary_from_token(ps_token_type token)
{
    switch (token)
    {
    case PS_TOKEN_MINUS:
        return PS_OP_NEG;
    case PS_TOKEN_NOT:
        return PS_OP_NOT;
    default:
        return PS_OP_UNARY_INVALID;
    }
}

ps_operator_binary ps_operator_binary_from_token(ps_token_type token)
{
    switch (token)
    {
    case PS_TOKEN_PLUS:
        return PS_OP_ADD;
    case PS_TOKEN_MINUS:
        return PS_OP_SUB;
    case PS_TOKEN_STAR:
        return PS_OP_MUL;
    case PS_TOKEN_SLASH:
        return PS_OP_DIV;
    case PS_TOKEN_EQ:
        return PS_OP_EQ;
    case PS_TOKEN_NE:
        return PS_OP_NE;
    case PS_TOKEN_LT:
        return PS_OP_LT;
    case PS_TOKEN_LE:
        return PS_OP_LE;
    case PS_TOKEN_GT:
        return PS_OP_GT;
    case PS_TOKEN_GE:
        return PS_OP_GE;
    default:
        return PS_OP_BINARY_INVALID;
    }
}

char *ps_operator_unary_get_name(ps_operator_unary operator)
{
    static char unknown[32];
    switch (operator)
    {
    case PS_OP_UNARY_INVALID:
        return "INVALID";
    case PS_OP_NEG:
        return "NEG";
    case PS_OP_NOT:
        return "NOT";
    default:
        snprintf(unknown, sizeof(unknown), "?UNARY_OPERATOR(%d)?", operator);
        return unknown;
    }
}

char *ps_operator_binary_get_name(ps_operator_binary operator)
{
    static char unknown[32];
    switch (operator)
    {
    case PS_OP_BINARY_INVALID:
        return "INVALID";
    case PS_OP_ADD:
        return "ADD";
    case PS_OP_SUB:
        return "SUB";
    case PS_OP_OR:
        return "OR";
    case PS_OP_XOR:
        return "XOR";
    case PS_OP_MUL:
        return "MUL";
    case PS_OP_DIV:
        return "DIV";
    case PS_OP_DIV_REAL:
        return "DIV_REAL";
    case PS_OP_MOD:
        return "MOD";
    case PS_OP_AND:
        return "AND";
    case PS_OP_SHL:
        return "SHL";
    case PS_OP_SHR:
        return "SHR";
    case PS_OP_EQ:
        return "EQ";
    case PS_OP_GE:
        return "GE";
    case PS_OP_GT:
        return "GT";
    case PS_OP_LE:
        return "LE";
    case PS_OP_LT:
        return "LT";
    case PS_OP_NE:
        return "NE";
    default:
        snprintf(unknown, sizeof(unknown), "?BINARY_OPERATOR(%d)?", operator);
        return unknown;
    }
}

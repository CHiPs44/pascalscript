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
bool ps_function_unary_op(ps_interpreter *interpreter, ps_value *value, ps_value *result, ps_token_type token_type)
{
    result->type = value->type;
    // NB: with FPC, not(subrange) or not(enum) yields integer result without range checking
    switch (value->type->value->data.t->base)
    {
    case PS_TYPE_INTEGER:
        switch (token_type)
        {
        case PS_TOKEN_NOT:
            result->data.i = ~value->data.i;
            break;
        case PS_TOKEN_MINUS:
            result->data.i = -value->data.i;
            break;
        default:
            ps_interpreter_set_message(interpreter, "Unexpected token type %s (%d) for INTEGER unary operation",
                                       ps_token_get_keyword(token_type), token_type);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_UNSIGNED:
        switch (token_type)
        {
        case PS_TOKEN_NOT:
            result->data.u = ~value->data.u;
            break;
        default:
            ps_interpreter_set_message(interpreter, "Unexpected token type %s (%d) for UNSIGNED unary operation",
                                       ps_token_get_keyword(token_type), token_type);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_REAL:
        switch (token_type)
        {
        case PS_TOKEN_MINUS:
            result->data.r = -value->data.r;
            break;
        default:
            ps_interpreter_set_message(interpreter, "Unexpected token type %s (%d) for REAL unary operation",
                                       ps_token_get_keyword(token_type), token_type);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_BOOLEAN:
        switch (token_type)
        {
        case PS_TOKEN_NOT:
            result->data.b = !value->data.b;
            break;
        default:
            ps_interpreter_set_message(interpreter, "Unexpected token type %s (%d) for BOOLEAN unary operation",
                                       ps_token_get_keyword(token_type), token_type);
            return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    default:
        return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
    }
    return true;
}

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
    case (__KEY__):                                                                                                    \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
        break;

#define NUMBER_CASE_SIGNED(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__, __CAST__)                                   \
    case (__KEY__):                                                                                                    \
        result->data.__R__ = a->data.__A__ __OP__(__CAST__) b->data.__B__;                                             \
        r = __TYPE__;                                                                                                  \
        break;

#define NUMBER_CASE_DIV_MOD(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__)                                            \
    case (__KEY__):                                                                                                    \
        if (b->data.__B__ == 0)                                                                                        \
            return ps_interpreter_return_false(interpreter, PS_ERROR_DIVISION_BY_ZERO);                                \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
        break;

#define NUMBER_CASE_DIV_REAL(__KEY__, __A__, __B__)                                                                    \
    case (__KEY__):                                                                                                    \
        if ((ps_real)(b->data.__B__) == 0.0)                                                                           \
            return ps_interpreter_return_false(interpreter, PS_ERROR_DIVISION_BY_ZERO);                                \
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

/* clang-format off */
bool ps_string_eq(ps_string *a, ps_string *b) { return ps_string_compare(a, b) == 0; }
bool ps_string_ne(ps_string *a, ps_string *b) { return ps_string_compare(a, b) != 0; }
bool ps_string_lt(ps_string *a, ps_string *b) { return ps_string_compare(a, b) <  0; }
bool ps_string_le(ps_string *a, ps_string *b) { return ps_string_compare(a, b) <= 0; }
bool ps_string_gt(ps_string *a, ps_string *b) { return ps_string_compare(a, b) >  0; }
bool ps_string_ge(ps_string *a, ps_string *b) { return ps_string_compare(a, b) >= 0; }
/* clang-format on */

bool ps_function_binary_op(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result,
                           ps_token_type token_type)
{
    //                    OOOOOOOO | AAAA                                | BBBB
    //             ----------------+-------------------------------------+-----------------------------
    uint16_t key = token_type << 8 | (a->type->value->data.t->base << 4) | b->type->value->data.t->base;
    ps_value_type r = PS_TYPE_NONE;
    ps_symbol *expected_type = result->type;
    ps_string *s = NULL;

    switch (key)
    {
        // AND.U/I/B
        NUMBER_CASE(PS_TOKEN_AND << 8 | BB, b, b, &&, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_AND << 8 | II, i, i, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_AND << 8 | IU, i, u, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_AND << 8 | UI, u, i, &, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_AND << 8 | UU, u, u, &, u, PS_TYPE_UNSIGNED)
        // OR.U/I/B
        NUMBER_CASE(PS_TOKEN_OR << 8 | BB, b, b, ||, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_OR << 8 | II, i, i, |, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_OR << 8 | IU, i, u, |, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_OR << 8 | UI, u, i, |, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_OR << 8 | UU, u, u, |, u, PS_TYPE_UNSIGNED)
        // XOR.U/I/B
    case PS_TOKEN_XOR << 8 | BB:
        result->data.b = !(a->data.b) != !(b->data.b);
        r = PS_TYPE_BOOLEAN;
        break;
        NUMBER_CASE(PS_TOKEN_XOR << 8 | II, i, i, ^, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | IU, i, u, ^, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | UI, u, i, ^, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_XOR << 8 | UU, u, u, ^, u, PS_TYPE_UNSIGNED)
        // ADD.I/U/R/C/S
    case PS_TOKEN_PLUS << 8 | CC: // char + char => string
        s = ps_string_alloc(2);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->str[1] = b->data.c;
        s->len = 2;
        result->data.s = s;
        r = PS_TYPE_STRING;
        break;
    case PS_TOKEN_PLUS << 8 | CS: // char + string => string
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(s, b->data.s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        r = PS_TYPE_STRING;
        break;
    case PS_TOKEN_PLUS << 8 | SC: // string + char => string
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = b->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(a->data.s, s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        r = PS_TYPE_STRING;
        break;
    case PS_TOKEN_PLUS << 8 | SS: // string + string => string
        s = ps_string_concat(a->data.s, b->data.s, PS_STRING_MAX_LEN);
        if (s == NULL)
            return false;
        result->data.s = s;
        r = PS_TYPE_STRING;
        break;
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | II, i, i, +, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | IU, i, u, +, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | UI, u, i, +, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | UU, u, u, +, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | IR, i, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | RI, r, i, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | RR, r, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_PLUS << 8 | RU, r, u, +, r, PS_TYPE_REAL)
        // SUB.I/U/R
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | II, i, i, -, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | IU, i, u, -, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | UI, u, i, -, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | UU, u, u, -, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | RR, r, r, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | RI, r, i, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | RU, r, u, -, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_MINUS << 8 | IR, i, r, -, r, PS_TYPE_REAL)
        // MUL.I/U/R
        NUMBER_CASE(PS_TOKEN_STAR << 8 | II, i, i, *, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | IU, i, u, *, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | UI, u, i, *, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | UU, u, u, *, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | RR, r, r, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | RI, r, i, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | RU, r, u, *, r, PS_TYPE_REAL)
        NUMBER_CASE(PS_TOKEN_STAR << 8 | IR, i, r, *, r, PS_TYPE_REAL)
        // DIV.I/U
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | II, i, i, /, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | IU, i, u, /, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | UI, u, i, /, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_DIV << 8 | UU, u, u, /, u, PS_TYPE_UNSIGNED)
        // DIV.R
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | II, i, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | IU, i, u)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | UI, u, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | UU, u, u)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | IR, i, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | RI, r, i)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | RR, r, r)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | RU, r, u)
        NUMBER_CASE_DIV_REAL(PS_TOKEN_SLASH << 8 | UR, u, r)
        // MOD.I/U
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | II, i, i, %, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | IU, i, u, %, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | UI, u, i, %, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(PS_TOKEN_MOD << 8 | UU, u, u, %, u, PS_TYPE_UNSIGNED)
        // EQ.I/U/R/C/S
        NUMBER_CASE_SIGNED(PS_TOKEN_EQ << 8 | IU, i, u, ==, b, PS_TYPE_BOOLEAN, ps_integer)
        NUMBER_CASE_SIGNED(PS_TOKEN_EQ << 8 | UI, u, i, ==, b, PS_TYPE_BOOLEAN, ps_unsigned)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | CC, c, c, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | II, i, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | IR, i, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | RI, r, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | RR, r, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | RU, r, u, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | UR, u, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_EQ << 8 | UU, u, u, ==, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(PS_TOKEN_EQ << 8 | CS, ps_string_eq)
        STRING_CASE_SC(PS_TOKEN_EQ << 8 | SC, ps_string_eq)
        STRING_CASE_SS(PS_TOKEN_EQ << 8 | SS, ps_string_eq)
        // NE.I/U/R/C/S
        NUMBER_CASE_SIGNED(PS_TOKEN_NE << 8 | IU, i, u, !=, b, PS_TYPE_BOOLEAN, ps_integer)
        NUMBER_CASE_SIGNED(PS_TOKEN_NE << 8 | UI, u, i, !=, b, PS_TYPE_BOOLEAN, ps_unsigned)
        NUMBER_CASE(PS_TOKEN_NE << 8 | CC, c, c, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | II, i, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | IR, i, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | RI, r, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | RR, r, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | RU, r, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | UR, u, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_NE << 8 | UU, u, u, !=, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(PS_TOKEN_NE << 8 | CS, ps_string_ne)
        STRING_CASE_SC(PS_TOKEN_NE << 8 | SC, ps_string_ne)
        STRING_CASE_SS(PS_TOKEN_NE << 8 | SS, ps_string_ne)
        // LT.I/U/R/C/S
        NUMBER_CASE_SIGNED(PS_TOKEN_LT << 8 | IU, i, u, <, b, PS_TYPE_BOOLEAN, ps_integer)
        NUMBER_CASE_SIGNED(PS_TOKEN_LT << 8 | UI, u, i, <, b, PS_TYPE_BOOLEAN, ps_unsigned)
        NUMBER_CASE(PS_TOKEN_LT << 8 | CC, c, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | II, i, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | IR, i, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | RI, r, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | RR, r, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | RU, r, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | UR, u, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LT << 8 | UU, u, u, <, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(PS_TOKEN_LT << 8 | CS, ps_string_lt)
        STRING_CASE_SC(PS_TOKEN_LT << 8 | SC, ps_string_lt)
        STRING_CASE_SS(PS_TOKEN_LT << 8 | SS, ps_string_lt)
        // LE.I/U/R/C/S
        NUMBER_CASE_SIGNED(PS_TOKEN_LE << 8 | IU, i, u, <=, b, PS_TYPE_BOOLEAN, ps_integer)
        NUMBER_CASE_SIGNED(PS_TOKEN_LE << 8 | UI, u, i, <=, b, PS_TYPE_BOOLEAN, ps_unsigned)
        NUMBER_CASE(PS_TOKEN_LE << 8 | CC, c, c, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | II, i, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | IR, i, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | RI, r, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | RR, r, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | RU, r, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | UR, u, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_LE << 8 | UU, u, u, <=, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(PS_TOKEN_LE << 8 | CS, ps_string_le)
        STRING_CASE_SC(PS_TOKEN_LE << 8 | SC, ps_string_le)
        STRING_CASE_SS(PS_TOKEN_LE << 8 | SS, ps_string_le)
        // GT.I/U/R/C/S
        NUMBER_CASE_SIGNED(PS_TOKEN_GT << 8 | IU, i, u, >, b, PS_TYPE_BOOLEAN, ps_integer)
        NUMBER_CASE_SIGNED(PS_TOKEN_GT << 8 | UI, u, i, >, b, PS_TYPE_BOOLEAN, ps_unsigned)
        NUMBER_CASE(PS_TOKEN_GT << 8 | CC, c, c, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | II, i, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | IR, i, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | RI, r, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | RR, r, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | RU, r, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | UR, u, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GT << 8 | UU, u, u, >, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(PS_TOKEN_GT << 8 | CS, ps_string_gt)
        STRING_CASE_SC(PS_TOKEN_GT << 8 | SC, ps_string_gt)
        STRING_CASE_SS(PS_TOKEN_GT << 8 | SS, ps_string_gt)
        // GE.I/U/R/C/S
        NUMBER_CASE_SIGNED(PS_TOKEN_GE << 8 | IU, i, u, >=, b, PS_TYPE_BOOLEAN, ps_integer)
        NUMBER_CASE_SIGNED(PS_TOKEN_GE << 8 | UI, u, i, >=, b, PS_TYPE_BOOLEAN, ps_unsigned)
        NUMBER_CASE(PS_TOKEN_GE << 8 | CC, c, c, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | II, i, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | IR, i, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | RI, r, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | RR, r, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | RU, r, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | UR, u, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(PS_TOKEN_GE << 8 | UU, u, u, >=, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(PS_TOKEN_GE << 8 | CS, ps_string_ge)
        STRING_CASE_SC(PS_TOKEN_GE << 8 | SC, ps_string_ge)
        STRING_CASE_SS(PS_TOKEN_GE << 8 | SS, ps_string_ge)
        // SHL.I/U
        NUMBER_CASE(PS_TOKEN_SHL << 8 | II, i, i, <<, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | IU, i, u, <<, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | UI, u, i, <<, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_SHL << 8 | UU, u, u, <<, u, PS_TYPE_UNSIGNED)
        // SHR.I/U
        NUMBER_CASE(PS_TOKEN_SHR << 8 | II, i, i, >>, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | IU, i, u, >>, i, PS_TYPE_INTEGER)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | UI, u, i, >>, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(PS_TOKEN_SHR << 8 | UU, u, u, >>, u, PS_TYPE_UNSIGNED)
    default:
        ps_interpreter_set_message(interpreter, "Binary operator %d/'%s' is not applicable for types '%s' and '%s'\n",
                                   token_type, ps_token_get_keyword(token_type),
                                   ps_value_type_get_name(a->type->value->data.t->base),
                                   ps_value_type_get_name(b->type->value->data.t->base));
        return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
    }
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
        ps_interpreter_set_message(interpreter, "Unknown binary operator %d/'%s' for types '%s' and '%s'\n", token_type,
                                   ps_token_get_keyword(token_type),
                                   ps_value_type_get_name(a->type->value->data.t->base),
                                   ps_value_type_get_name(b->type->value->data.t->base));
        return ps_interpreter_return_false(interpreter, PS_ERROR_OPERATOR_NOT_APPLICABLE);
    }
    if (expected_type != &ps_system_none && result->type != expected_type)
    {
        ps_interpreter_set_message(
            interpreter, "Binary operator '%s' (%d) result type '%s' does not match expected type '%s'\n",
            ps_token_get_keyword(token_type), token_type, ps_value_type_get_name(result->type->value->data.t->base),
            ps_value_type_get_name(expected_type->value->data.t->base));
        return ps_interpreter_return_false(interpreter, PS_ERROR_TYPE_MISMATCH);
    }
    return true;
}

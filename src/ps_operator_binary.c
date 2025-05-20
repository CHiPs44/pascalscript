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

typedef bool (*ps_binary_func)(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result);

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

#define NUMBER_FUNC(__NAME__, __A__, __B__, __OP__, __R__)                                                             \
    bool ps_##__NAME__(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)                        \
    {                                                                                                                  \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        return true;                                                                                                   \
    }

#define NUMBER_FUNC_DIV(__NAME__, __A__, __B__, __OP__, __R__)                                                         \
    bool ps_##__NAME__(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)                        \
    {                                                                                                                  \
        if (b->data.__B__ == 0)                                                                                        \
            RETURN_ERROR(PS_RUNTIME_ERROR_DIVISION_BY_ZERO)                                                            \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        return true;                                                                                                   \
    }

#define NUMBER_FUNC_DIVR(__NAME__, __A__, __B__, __OP__)                                                               \
    bool ps_##__NAME__(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)                        \
    {                                                                                                                  \
        if ((ps_real)(b->data.__B__) == 0.0)                                                                           \
            RETURN_ERROR(PS_RUNTIME_ERROR_DIVISION_BY_ZERO)                                                            \
        result->data.r = (ps_real)(a->data.__A__)__OP__(ps_real)(b->data.__B__);                                       \
        return true;                                                                                                   \
    }

NUMBER_FUNC(and_ii, i, i, &, i)
NUMBER_FUNC(and_iu, i, i, &, i)
NUMBER_FUNC(and_ui, u, i, &, u)
NUMBER_FUNC(and_uu, u, u, &, u)
NUMBER_FUNC(and_bb, b, b, &&, b)

NUMBER_FUNC(or_ii, i, i, &, i)
NUMBER_FUNC(or_iu, i, i, &, i)
NUMBER_FUNC(or_ui, u, i, &, u)
NUMBER_FUNC(or_uu, u, u, &, u)
NUMBER_FUNC(or_bb, b, b, ||, b)

NUMBER_FUNC(xor_ii, i, i, ^, i)
NUMBER_FUNC(xor_iu, i, i, ^, i)
NUMBER_FUNC(xor_ui, u, i, ^, u)
NUMBER_FUNC(xor_uu, u, u, ^, u)

bool ps_xor_bb(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    // no ^^ operator in C, replace with:
    //  xor(f, f) => f ( = !f != !f )
    //  xor(f, t) => t ( = !f != !t )
    //  xor(t, f) => t ( = !t != !f )
    //  xor(t, t) => f ( = !t != !t )
    result->data.b = !(a->data.b) != !(b->data.b);
    return true;
}

NUMBER_FUNC(add_ii, i, i, &, i)
NUMBER_FUNC(add_iu, i, i, &, i)
NUMBER_FUNC(add_ui, u, i, &, u)
NUMBER_FUNC(add_uu, u, u, &, u)

NUMBER_FUNC(add_rr, r, r, +, r)
NUMBER_FUNC(add_ri, r, i, +, r)
NUMBER_FUNC(add_ru, r, u, +, r)
NUMBER_FUNC(add_ir, i, r, +, r)
NUMBER_FUNC(add_ur, u, r, +, r)

NUMBER_FUNC(sub_ii, i, i, -, i)
NUMBER_FUNC(sub_iu, i, i, -, i)
NUMBER_FUNC(sub_ui, u, i, -, u)
NUMBER_FUNC(sub_uu, u, u, -, u)

NUMBER_FUNC(sub_rr, r, r, -, r)
NUMBER_FUNC(sub_ri, r, i, -, r)
NUMBER_FUNC(sub_ru, r, u, -, r)
NUMBER_FUNC(sub_ir, i, r, -, r)
NUMBER_FUNC(sub_ur, u, r, -, r)

NUMBER_FUNC(mul_ii, i, i, *, i)
NUMBER_FUNC(mul_iu, i, i, *, i)
NUMBER_FUNC(mul_ui, u, i, *, u)
NUMBER_FUNC(mul_uu, u, u, *, u)

NUMBER_FUNC(mul_rr, r, r, *, r)
NUMBER_FUNC(mul_ri, r, i, *, r)
NUMBER_FUNC(mul_ru, r, u, *, r)
NUMBER_FUNC(mul_ir, i, r, *, r)
NUMBER_FUNC(mul_ur, u, r, *, r)

NUMBER_FUNC_DIV(div_ii, i, i, /, i)
NUMBER_FUNC_DIV(div_iu, i, i, /, i)
NUMBER_FUNC_DIV(div_ui, u, i, /, u)
NUMBER_FUNC_DIV(div_uu, u, u, /, u)

NUMBER_FUNC_DIV(mod_ii, i, i, %, i)
NUMBER_FUNC_DIV(mod_iu, i, i, %, i)
NUMBER_FUNC_DIV(mod_ui, u, i, %, u)
NUMBER_FUNC_DIV(mod_uu, u, u, %, u)

NUMBER_FUNC_DIVR(divr_ii, i, i, /)
NUMBER_FUNC_DIVR(divr_iu, i, i, /)
NUMBER_FUNC_DIVR(divr_ui, u, i, /)
NUMBER_FUNC_DIVR(divr_uu, u, u, /)
NUMBER_FUNC_DIVR(divr_rr, r, r, /)
NUMBER_FUNC_DIVR(divr_ri, r, i, /)
NUMBER_FUNC_DIVR(divr_ru, r, u, /)
NUMBER_FUNC_DIVR(divr_ir, i, r, /)
NUMBER_FUNC_DIVR(divr_ur, u, r, /)

NUMBER_FUNC(eq_ii, i, i, ==, b)
NUMBER_FUNC(eq_iu, i, i, ==, b)
NUMBER_FUNC(eq_ui, u, i, ==, b)
NUMBER_FUNC(eq_uu, u, u, ==, b)
NUMBER_FUNC(eq_rr, r, r, ==, b)
NUMBER_FUNC(eq_ri, r, i, ==, b)
NUMBER_FUNC(eq_ru, r, u, ==, b)
NUMBER_FUNC(eq_ir, i, r, ==, b)
NUMBER_FUNC(eq_ur, u, r, ==, b)
NUMBER_FUNC(eq_cc, c, c, ==, b)

NUMBER_FUNC(ne_ii, i, i, !=, b)
NUMBER_FUNC(ne_iu, i, i, !=, b)
NUMBER_FUNC(ne_ui, u, i, !=, b)
NUMBER_FUNC(ne_uu, u, u, !=, b)
NUMBER_FUNC(ne_rr, r, r, !=, b)
NUMBER_FUNC(ne_ri, r, i, !=, b)
NUMBER_FUNC(ne_ru, r, u, !=, b)
NUMBER_FUNC(ne_ir, i, r, !=, b)
NUMBER_FUNC(ne_ur, u, r, !=, b)
NUMBER_FUNC(ne_cc, c, c, !=, b)

NUMBER_FUNC(lt_ii, i, i, <, b)
NUMBER_FUNC(lt_iu, i, i, <, b)
NUMBER_FUNC(lt_ui, u, i, <, b)
NUMBER_FUNC(lt_uu, u, u, <, b)
NUMBER_FUNC(lt_rr, r, r, <, b)
NUMBER_FUNC(lt_ri, r, i, <, b)
NUMBER_FUNC(lt_ru, r, u, <, b)
NUMBER_FUNC(lt_ir, i, r, <, b)
NUMBER_FUNC(lt_ur, u, r, <, b)
NUMBER_FUNC(lt_cc, c, r, <, b)

NUMBER_FUNC(le_ii, i, i, <=, b)
NUMBER_FUNC(le_iu, i, i, <=, b)
NUMBER_FUNC(le_ui, u, i, <=, b)
NUMBER_FUNC(le_uu, u, u, <=, b)
NUMBER_FUNC(le_rr, r, r, <=, b)
NUMBER_FUNC(le_ri, r, i, <=, b)
NUMBER_FUNC(le_ru, r, u, <=, b)
NUMBER_FUNC(le_ir, i, r, <=, b)
NUMBER_FUNC(le_ur, u, r, <=, b)
NUMBER_FUNC(le_cc, c, c, <=, b)

NUMBER_FUNC(gt_ii, i, i, >, b)
NUMBER_FUNC(gt_iu, i, i, >, b)
NUMBER_FUNC(gt_ui, u, i, >, b)
NUMBER_FUNC(gt_uu, u, u, >, b)
NUMBER_FUNC(gt_rr, r, r, >, b)
NUMBER_FUNC(gt_ri, r, i, >, b)
NUMBER_FUNC(gt_ru, r, u, >, b)
NUMBER_FUNC(gt_ir, i, r, >, b)
NUMBER_FUNC(gt_ur, u, r, >, b)
NUMBER_FUNC(gt_cc, c, c, >, b)

NUMBER_FUNC(ge_ii, i, i, >=, b)
NUMBER_FUNC(ge_iu, i, i, >=, b)
NUMBER_FUNC(ge_ui, u, i, >=, b)
NUMBER_FUNC(ge_uu, u, u, >=, b)
NUMBER_FUNC(ge_rr, r, r, >=, b)
NUMBER_FUNC(ge_ri, r, i, >=, b)
NUMBER_FUNC(ge_ru, r, u, >=, b)
NUMBER_FUNC(ge_ir, i, r, >=, b)
NUMBER_FUNC(ge_ur, u, r, >=, b)
NUMBER_FUNC(ge_cc, c, c, >=, b)

NUMBER_FUNC(shl_ii, i, i, <<, i)
NUMBER_FUNC(shl_iu, i, i, <<, i)
NUMBER_FUNC(shl_ui, u, i, <<, u)
NUMBER_FUNC(shl_uu, u, u, <<, u)

NUMBER_FUNC(shr_ii, i, i, >>, i)
NUMBER_FUNC(shr_iu, i, i, >>, i)
NUMBER_FUNC(shr_ui, u, i, >>, u)
NUMBER_FUNC(shr_uu, u, u, >>, u)

/**
 * @brief Concatenate char + char into string
 */
bool ps_add_cc(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    ps_string *s = ps_string_alloc(2);
    if (s == NULL)
        return false;
    s->str[0] = a->data.c;
    s->str[1] = b->data.c;
    s->len = 2;
    result->data.s = s;
    return true;
}

/**
 * @brief Concatenate char + string into string
 */
bool ps_add_cs(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    ps_string *s = ps_string_alloc(1);
    if (s == NULL)
        return false;
    s->str[0] = a->data.c;
    s->len = 1;
    result->data.s = ps_string_concat(s, b->data.s, PS_STRING_MAX_LEN);
    ps_string_free(s);
    return true;
}

/**
 * @brief Concatenate string + char into string
 */
bool ps_add_sc(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    ps_string *s = ps_string_alloc(1);
    if (s == NULL)
        return false;
    s->str[0] = b->data.c;
    s->len = 1;
    result->data.s = ps_string_concat(a->data.s, s, PS_STRING_MAX_LEN);
    ps_string_free(s);
    return true;
}

/**
 * @brief Concatenate string + string into string
 */
bool ps_add_ss(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)
{
    ps_string *s = ps_string_concat(a->data.s, b->data.s, PS_STRING_MAX_LEN);
    if (s == NULL)
        return false;
    result->data.s = s;
    return true;
}

#define STRING_FUNC_CS(__NAME__, __OP__, __R__)                                                                        \
    bool ps_##__NAME__(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)                        \
    {                                                                                                                  \
        ps_string *s = ps_string_alloc(1);                                                                             \
        if (s == NULL)                                                                                                 \
            return false;                                                                                              \
        s->str[0] = a->data.c;                                                                                         \
        s->len = 1;                                                                                                    \
        result->data.__R__ = __OP__(s, b->data.s);                                                                     \
        ps_string_free(s);                                                                                             \
        return true;                                                                                                   \
    }

#define STRING_FUNC_SC(__NAME__, __OP__, __R__)                                                                        \
    bool ps_##__NAME__(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)                        \
    {                                                                                                                  \
        ps_string *s = ps_string_alloc(1);                                                                             \
        if (s == NULL)                                                                                                 \
            return false;                                                                                              \
        s->str[0] = a->data.c;                                                                                         \
        s->len = 1;                                                                                                    \
        result->data.__R__ = __OP__(a->data.s, s);                                                                     \
        ps_string_free(s);                                                                                             \
        return true;                                                                                                   \
    }

#define STRING_FUNC_SS(__NAME__, __OP__, __R__)                                                                        \
    bool ps_##__NAME__(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result)                        \
    {                                                                                                                  \
        result->data.__R__ = __OP__(a->data.s, b->data.s);                                                             \
        return true;                                                                                                   \
    }

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

STRING_FUNC_CS(eq_cs, ps_string_eq, b)
STRING_FUNC_SC(eq_sc, ps_string_eq, b)
STRING_FUNC_SS(eq_ss, ps_string_eq, b)
STRING_FUNC_CS(ne_cs, ps_string_ne, b)
STRING_FUNC_SC(ne_sc, ps_string_ne, b)
STRING_FUNC_SS(ne_ss, ps_string_ne, b)
STRING_FUNC_CS(lt_cs, ps_string_lt, b)
STRING_FUNC_SC(lt_sc, ps_string_lt, b)
STRING_FUNC_SS(lt_ss, ps_string_lt, b)
STRING_FUNC_CS(le_cs, ps_string_le, b)
STRING_FUNC_SC(le_sc, ps_string_le, b)
STRING_FUNC_SS(le_ss, ps_string_le, b)
STRING_FUNC_CS(gt_cs, ps_string_gt, b)
STRING_FUNC_SC(gt_sc, ps_string_gt, b)
STRING_FUNC_SS(gt_ss, ps_string_gt, b)
STRING_FUNC_CS(ge_cs, ps_string_ge, b)
STRING_FUNC_SC(ge_sc, ps_string_ge, b)
STRING_FUNC_SS(ge_ss, ps_string_ge, b)

typedef struct s_ps_operator_binary_entry
{
    uint16_t k;        /** @brief TTTTTTTTAAAABBBB: operator token + left & right operand base types */
    ps_value_type r;   /** @brief result base type */
    ps_binary_func *f; /** @brief function to call for the operator */
} /*__attribute__((__packed__))*/ ps_operator_binary_entry;

// clang-format off

ps_operator_binary_entry ps_operator_binary_table[] = {

    // ==================== BITWISE OPERATORS ====================

    {PS_TOKEN_AND              | II, PS_TYPE_INTEGER , &ps_and_ii},
    {PS_TOKEN_AND              | IU, PS_TYPE_INTEGER , &ps_and_iu},
    {PS_TOKEN_AND              | UI, PS_TYPE_UNSIGNED, &ps_and_ui},
    {PS_TOKEN_AND              | UU, PS_TYPE_UNSIGNED, &ps_and_uu},
    {PS_TOKEN_AND              | BB, PS_TYPE_BOOLEAN , &ps_and_bb},

    {PS_TOKEN_OR               | II, PS_TYPE_INTEGER , &ps_or_ii },
    {PS_TOKEN_OR               | IU, PS_TYPE_INTEGER , &ps_or_iu },
    {PS_TOKEN_OR               | UI, PS_TYPE_UNSIGNED, &ps_or_ui },
    {PS_TOKEN_OR               | UU, PS_TYPE_UNSIGNED, &ps_or_uu },
    {PS_TOKEN_OR               | BB, PS_TYPE_BOOLEAN , &ps_or_bb },

    {PS_TOKEN_XOR              | II, PS_TYPE_INTEGER , &ps_xor_ii},
    {PS_TOKEN_XOR              | IU, PS_TYPE_INTEGER , &ps_xor_iu},
    {PS_TOKEN_XOR              | UI, PS_TYPE_UNSIGNED, &ps_xor_ui},
    {PS_TOKEN_XOR              | UU, PS_TYPE_UNSIGNED, &ps_xor_uu},
    {PS_TOKEN_XOR              | BB, PS_TYPE_BOOLEAN , &ps_xor_bb},

    // ==================== SHIFT OPERATORS ====================

    {PS_TOKEN_SHL              | II, PS_TYPE_INTEGER , &ps_shl_ii},
    {PS_TOKEN_SHL              | IU, PS_TYPE_INTEGER , &ps_shl_iu},
    {PS_TOKEN_SHL              | UI, PS_TYPE_UNSIGNED, &ps_shl_ui},
    {PS_TOKEN_SHL              | UU, PS_TYPE_UNSIGNED, &ps_shl_uu},
    {PS_TOKEN_SHR              | II, PS_TYPE_UNSIGNED, &ps_shr_ii},
    {PS_TOKEN_SHR              | IU, PS_TYPE_UNSIGNED, &ps_shr_iu},
    {PS_TOKEN_SHR              | UI, PS_TYPE_UNSIGNED, &ps_shr_ui},
    {PS_TOKEN_SHR              | UU, PS_TYPE_UNSIGNED, &ps_shr_uu},

    // ==================== ARITHMETIC OPERATORS ====================

    {PS_TOKEN_PLUS             | II, PS_TYPE_INTEGER , &ps_add_ii},
    {PS_TOKEN_PLUS             | IU, PS_TYPE_INTEGER , &ps_add_iu},
    {PS_TOKEN_PLUS             | UI, PS_TYPE_UNSIGNED, &ps_add_ui},
    {PS_TOKEN_PLUS             | UU, PS_TYPE_UNSIGNED, &ps_add_uu},
    {PS_TOKEN_PLUS             | RR, PS_TYPE_REAL    , &ps_add_rr},
    {PS_TOKEN_PLUS             | RI, PS_TYPE_REAL    , &ps_add_ri},
    {PS_TOKEN_PLUS             | RU, PS_TYPE_REAL    , &ps_add_ru},
    {PS_TOKEN_PLUS             | IR, PS_TYPE_REAL    , &ps_add_ir},
    {PS_TOKEN_PLUS             | UR, PS_TYPE_REAL    , &ps_add_ur},
    {PS_TOKEN_PLUS             | CC, PS_TYPE_STRING  , &ps_add_cc},
    {PS_TOKEN_PLUS             | CS, PS_TYPE_STRING  , &ps_add_cs},
    {PS_TOKEN_PLUS             | SC, PS_TYPE_STRING  , &ps_add_sc},
    {PS_TOKEN_PLUS             | SS, PS_TYPE_STRING  , &ps_add_ss},
        
    {PS_TOKEN_MINUS            | II, PS_TYPE_INTEGER , &ps_sub_ii},
    {PS_TOKEN_MINUS            | IU, PS_TYPE_INTEGER , &ps_sub_iu},
    {PS_TOKEN_MINUS            | UI, PS_TYPE_UNSIGNED, &ps_sub_ui},
    {PS_TOKEN_MINUS            | UU, PS_TYPE_UNSIGNED, &ps_sub_uu},
    {PS_TOKEN_MINUS            | RR, PS_TYPE_REAL    , &ps_sub_rr},
    {PS_TOKEN_MINUS            | RI, PS_TYPE_REAL    , &ps_sub_ri},
    {PS_TOKEN_MINUS            | RU, PS_TYPE_REAL    , &ps_sub_ru},
    {PS_TOKEN_MINUS            | IR, PS_TYPE_REAL    , &ps_sub_ir},
    {PS_TOKEN_MINUS            | UR, PS_TYPE_REAL    , &ps_sub_ur},

    {PS_TOKEN_STAR             | II, PS_TYPE_INTEGER , &ps_mul_ii},
    {PS_TOKEN_STAR             | IU, PS_TYPE_INTEGER , &ps_mul_iu},
    {PS_TOKEN_STAR             | UI, PS_TYPE_UNSIGNED, &ps_mul_ui},
    {PS_TOKEN_STAR             | UU, PS_TYPE_UNSIGNED, &ps_mul_uu},
    {PS_TOKEN_STAR             | RR, PS_TYPE_REAL    , &ps_mul_rr}             ,
    {PS_TOKEN_STAR             | RI, PS_TYPE_REAL    , &ps_mul_ri}             ,
    {PS_TOKEN_STAR             | RU, PS_TYPE_REAL    , &ps_mul_ru}             ,
    {PS_TOKEN_STAR             | IR, PS_TYPE_REAL    , &ps_mul_ir},
    {PS_TOKEN_STAR             | UR, PS_TYPE_REAL    , &ps_mul_ur},

    {PS_TOKEN_DIV              | II, PS_TYPE_INTEGER , &ps_div_ii},
    {PS_TOKEN_DIV              | IU, PS_TYPE_INTEGER , &ps_div_iu},
    {PS_TOKEN_DIV              | UI, PS_TYPE_UNSIGNED, &ps_div_ui},
    {PS_TOKEN_DIV              | UU, PS_TYPE_UNSIGNED, &ps_div_uu},

    {PS_TOKEN_MOD              | II, PS_TYPE_INTEGER , &ps_mod_ii},
    {PS_TOKEN_MOD              | IU, PS_TYPE_INTEGER , &ps_mod_iu},
    {PS_TOKEN_MOD              | UI, PS_TYPE_UNSIGNED, &ps_mod_ui},
    {PS_TOKEN_MOD              | UU, PS_TYPE_UNSIGNED, &ps_mod_uu},

    {PS_TOKEN_SLASH            | II, PS_TYPE_REAL    , &ps_divr_ii},
    {PS_TOKEN_SLASH            | IU, PS_TYPE_REAL    , &ps_divr_iu},
    {PS_TOKEN_SLASH            | UI, PS_TYPE_REAL    , &ps_divr_ui},
    {PS_TOKEN_SLASH            | UU, PS_TYPE_REAL    , &ps_divr_uu},
    {PS_TOKEN_SLASH            | RR, PS_TYPE_REAL    , &ps_divr_rr},
    {PS_TOKEN_SLASH            | RI, PS_TYPE_REAL    , &ps_divr_ri},
    {PS_TOKEN_SLASH            | RU, PS_TYPE_REAL    , &ps_divr_ru},
    {PS_TOKEN_SLASH            | IR, PS_TYPE_REAL    , &ps_divr_ir},
    {PS_TOKEN_SLASH            | UR, PS_TYPE_REAL    , &ps_divr_ur},

    // ==================== RELATIONAL OPERATORS ====================

    {PS_TOKEN_EQUAL            | II, PS_TYPE_BOOLEAN , &ps_eq_ii},
    {PS_TOKEN_EQUAL            | IU, PS_TYPE_BOOLEAN , &ps_eq_iu},
    {PS_TOKEN_EQUAL            | UI, PS_TYPE_BOOLEAN , &ps_eq_ui},
    {PS_TOKEN_EQUAL            | UU, PS_TYPE_BOOLEAN , &ps_eq_uu},
    {PS_TOKEN_EQUAL            | RR, PS_TYPE_BOOLEAN , &ps_eq_rr},
    {PS_TOKEN_EQUAL            | RI, PS_TYPE_BOOLEAN , &ps_eq_ri},
    {PS_TOKEN_EQUAL            | RU, PS_TYPE_BOOLEAN , &ps_eq_ru},
    {PS_TOKEN_EQUAL            | IR, PS_TYPE_BOOLEAN , &ps_eq_ir},
    {PS_TOKEN_EQUAL            | UR, PS_TYPE_BOOLEAN , &ps_eq_ur},
    {PS_TOKEN_EQUAL            | CC, PS_TYPE_BOOLEAN , &ps_eq_cc},
    {PS_TOKEN_EQUAL            | CS, PS_TYPE_BOOLEAN , &ps_eq_cs},
    {PS_TOKEN_EQUAL            | SC, PS_TYPE_BOOLEAN , &ps_eq_sc},
    {PS_TOKEN_EQUAL            | SS, PS_TYPE_BOOLEAN , &ps_eq_ss},

    {PS_TOKEN_NOT_EQUAL        | II, PS_TYPE_BOOLEAN , &ps_ne_ii},
    {PS_TOKEN_NOT_EQUAL        | IU, PS_TYPE_BOOLEAN , &ps_ne_iu},
    {PS_TOKEN_NOT_EQUAL        | UI, PS_TYPE_BOOLEAN , &ps_ne_ui},
    {PS_TOKEN_NOT_EQUAL        | UU, PS_TYPE_BOOLEAN , &ps_ne_uu},
    {PS_TOKEN_NOT_EQUAL        | RR, PS_TYPE_BOOLEAN , &ps_ne_rr},
    {PS_TOKEN_NOT_EQUAL        | RI, PS_TYPE_BOOLEAN , &ps_ne_ri},
    {PS_TOKEN_NOT_EQUAL        | RU, PS_TYPE_BOOLEAN , &ps_ne_ru},
    {PS_TOKEN_NOT_EQUAL        | IR, PS_TYPE_BOOLEAN , &ps_ne_ir},
    {PS_TOKEN_NOT_EQUAL        | UR, PS_TYPE_BOOLEAN , &ps_ne_ur},
    {PS_TOKEN_NOT_EQUAL        | CC, PS_TYPE_BOOLEAN , &ps_ne_cc},
    {PS_TOKEN_NOT_EQUAL        | CS, PS_TYPE_BOOLEAN , &ps_ne_cs},
    {PS_TOKEN_NOT_EQUAL        | SC, PS_TYPE_BOOLEAN , &ps_ne_sc},
    {PS_TOKEN_NOT_EQUAL        | SS, PS_TYPE_BOOLEAN , &ps_ne_ss},

    {PS_TOKEN_LESS_THAN        | II, PS_TYPE_BOOLEAN , &ps_lt_ii},
    {PS_TOKEN_LESS_THAN        | IU, PS_TYPE_BOOLEAN , &ps_lt_iu},
    {PS_TOKEN_LESS_THAN        | UI, PS_TYPE_BOOLEAN , &ps_lt_ui},
    {PS_TOKEN_LESS_THAN        | UU, PS_TYPE_BOOLEAN , &ps_lt_uu},
    {PS_TOKEN_LESS_THAN        | RR, PS_TYPE_BOOLEAN , &ps_lt_rr},
    {PS_TOKEN_LESS_THAN        | RI, PS_TYPE_BOOLEAN , &ps_lt_ri},
    {PS_TOKEN_LESS_THAN        | RU, PS_TYPE_BOOLEAN , &ps_lt_ru},
    {PS_TOKEN_LESS_THAN        | IR, PS_TYPE_BOOLEAN , &ps_lt_ir},
    {PS_TOKEN_LESS_THAN        | UR, PS_TYPE_BOOLEAN , &ps_lt_ur},
    {PS_TOKEN_LESS_THAN        | CC, PS_TYPE_BOOLEAN , &ps_lt_cc},
    {PS_TOKEN_LESS_THAN        | CS, PS_TYPE_BOOLEAN , &ps_lt_cs},
    {PS_TOKEN_LESS_THAN        | SC, PS_TYPE_BOOLEAN , &ps_lt_sc},
    {PS_TOKEN_LESS_THAN        | SS, PS_TYPE_BOOLEAN , &ps_lt_ss},

    {PS_TOKEN_LESS_OR_EQUAL    | II, PS_TYPE_BOOLEAN , &ps_le_ii},
    {PS_TOKEN_LESS_OR_EQUAL    | IU, PS_TYPE_BOOLEAN , &ps_le_iu},
    {PS_TOKEN_LESS_OR_EQUAL    | UI, PS_TYPE_BOOLEAN , &ps_le_ui},
    {PS_TOKEN_LESS_OR_EQUAL    | UU, PS_TYPE_BOOLEAN , &ps_le_uu},
    {PS_TOKEN_LESS_OR_EQUAL    | RR, PS_TYPE_BOOLEAN , &ps_le_rr},
    {PS_TOKEN_LESS_OR_EQUAL    | RI, PS_TYPE_BOOLEAN , &ps_le_ri},
    {PS_TOKEN_LESS_OR_EQUAL    | RU, PS_TYPE_BOOLEAN , &ps_le_ru},
    {PS_TOKEN_LESS_OR_EQUAL    | IR, PS_TYPE_BOOLEAN , &ps_le_ir},
    {PS_TOKEN_LESS_OR_EQUAL    | UR, PS_TYPE_BOOLEAN , &ps_le_ur},
    {PS_TOKEN_LESS_OR_EQUAL    | CC, PS_TYPE_BOOLEAN , &ps_le_cc},
    {PS_TOKEN_LESS_OR_EQUAL    | CS, PS_TYPE_BOOLEAN , &ps_le_cs},
    {PS_TOKEN_LESS_OR_EQUAL    | SC, PS_TYPE_BOOLEAN , &ps_le_sc},
    {PS_TOKEN_LESS_OR_EQUAL    | SS, PS_TYPE_BOOLEAN , &ps_le_ss},

    {PS_TOKEN_GREATER_THAN     | II, PS_TYPE_BOOLEAN , &ps_gt_ii},
    {PS_TOKEN_GREATER_THAN     | IU, PS_TYPE_BOOLEAN , &ps_gt_iu},
    {PS_TOKEN_GREATER_THAN     | UI, PS_TYPE_BOOLEAN , &ps_gt_ui},
    {PS_TOKEN_GREATER_THAN     | UU, PS_TYPE_BOOLEAN , &ps_gt_uu},
    {PS_TOKEN_GREATER_THAN     | RR, PS_TYPE_BOOLEAN , &ps_gt_rr},
    {PS_TOKEN_GREATER_THAN     | RI, PS_TYPE_BOOLEAN , &ps_gt_ri},
    {PS_TOKEN_GREATER_THAN     | RU, PS_TYPE_BOOLEAN , &ps_gt_ru},
    {PS_TOKEN_GREATER_THAN     | IR, PS_TYPE_BOOLEAN , &ps_gt_ir},
    {PS_TOKEN_GREATER_THAN     | UR, PS_TYPE_BOOLEAN , &ps_gt_ur},
    {PS_TOKEN_GREATER_THAN     | CC, PS_TYPE_BOOLEAN , &ps_gt_cc},
    {PS_TOKEN_GREATER_THAN     | CS, PS_TYPE_BOOLEAN , &ps_gt_cs},
    {PS_TOKEN_GREATER_THAN     | SC, PS_TYPE_BOOLEAN , &ps_gt_sc},
    {PS_TOKEN_GREATER_THAN     | SS, PS_TYPE_BOOLEAN , &ps_gt_ss},

    {PS_TOKEN_GREATER_OR_EQUAL | II, PS_TYPE_BOOLEAN , &ps_ge_ii},
    {PS_TOKEN_GREATER_OR_EQUAL | IU, PS_TYPE_BOOLEAN , &ps_ge_iu},
    {PS_TOKEN_GREATER_OR_EQUAL | UI, PS_TYPE_BOOLEAN , &ps_ge_ui},
    {PS_TOKEN_GREATER_OR_EQUAL | UU, PS_TYPE_BOOLEAN , &ps_ge_uu},
    {PS_TOKEN_GREATER_OR_EQUAL | RR, PS_TYPE_BOOLEAN , &ps_ge_rr},
    {PS_TOKEN_GREATER_OR_EQUAL | RI, PS_TYPE_BOOLEAN , &ps_ge_ri},
    {PS_TOKEN_GREATER_OR_EQUAL | RU, PS_TYPE_BOOLEAN , &ps_ge_ru},
    {PS_TOKEN_GREATER_OR_EQUAL | IR, PS_TYPE_BOOLEAN , &ps_ge_ir},
    {PS_TOKEN_GREATER_OR_EQUAL | UR, PS_TYPE_BOOLEAN , &ps_ge_ur},
    {PS_TOKEN_GREATER_OR_EQUAL | CC, PS_TYPE_BOOLEAN , &ps_ge_cc},
    {PS_TOKEN_GREATER_OR_EQUAL | CS, PS_TYPE_BOOLEAN , &ps_ge_cs},
    {PS_TOKEN_GREATER_OR_EQUAL | SC, PS_TYPE_BOOLEAN , &ps_ge_sc},
    {PS_TOKEN_GREATER_OR_EQUAL | SS, PS_TYPE_BOOLEAN , &ps_ge_ss},

};

// clang-format on

#define PS_OPERATOR_BINARY_TABLE_COUNT (sizeof(ps_operator_binary_table) / sizeof(ps_operator_binary_entry))

void ps_operator_binary_dump()
{
    for (size_t i = 0; i < PS_OPERATOR_BINARY_TABLE_COUNT; i++)
    {
        ps_operator_binary_entry *e = &ps_operator_binary_table[i];
        fprintf(stderr, "0x%04X\t%s\t%s\t%s\n", e->k, ps_value_get_type_name((e->k >> 8) & 0xf),
                ps_value_get_type_name(e->k & 0x0F), ps_value_get_type_name(e->r));
    }
}

bool ps_operator_binary_exec(ps_interpreter *interpreter, ps_value *a, ps_value *b, ps_value *result,
                             ps_token_type token_type)
{
    //           TTTTTTTTTT | AAAA                 | BBBB
    //           -----------+----------------------+--------------
    uint16_t k = token_type | (a->type->base << 4) | b->type->base;
    ps_operator_binary_entry entry = {0, PS_TYPE_NONE, NULL};
    for (size_t i = 0; i < PS_OPERATOR_BINARY_TABLE_COUNT; i++)
    {
        if (ps_operator_binary_table[i].k == k)
        {
            entry = ps_operator_binary_table[i];
            break;
        }
    }
    if (entry.k != k)
        RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE)
    if (!((*entry.f)(interpreter, a, b, result)))
        return false;
    ps_value_type r = entry.r;
    if (result->type != NULL && result->type->base != PS_TYPE_NONE)
    {
        // check if the result type is compatible with the entry's result type
        if (result->type->base != entry.r)
            RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE)
    }
    else
    {
    // if we don't know what the result type is, we take entry's result type
        switch (entry.r)
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
            RETURN_ERROR(PS_RUNTIME_ERROR_OPERATOR_NOT_APPLICABLE)
        }
    }
}

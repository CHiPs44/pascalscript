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
#include "ps_string.h"
#include "ps_system.h"
#include "ps_value.h"
#include "ps_vm.h"

/**
 * @brief Compute
 *  - bitwise not for integer / unsigned,
 *  - logical not for boolean,
 *  - negative for integer / real
 *  and return true
 *  otherwise return false and set PS_ERROR_OPERATOR_NOT_APPLICABLE
 */
bool ps_function_unary_op(ps_vm *vm, ps_value *value, ps_value *result, ps_vm_opcode opcode)
{
    result->type = value->type;
    // NB: with FPC, not(subrange) or not(enum) yields integer result without range checking
    switch (value->type->base)
    {
    case PS_TYPE_INTEGER:
        switch (opcode)
        {
        case OP_NOT:
            result->data.i = ~value->data.i;
            break;
        case OP_NEG:
            result->data.i = -value->data.i;
            break;
        default:
            return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_UNSIGNED:
        switch (opcode)
        {
        case OP_NOT:
            result->data.u = ~value->data.u;
            break;
        default:
            return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_REAL:
        switch (opcode)
        {
        case OP_NEG:
            result->data.r = -value->data.r;
            break;
        default:
            return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    case PS_TYPE_BOOLEAN:
        switch (opcode)
        {
        case OP_NOT:
            result->data.b = !value->data.b;
            break;
        default:
            return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
        break;
    default:
        return ps_vm_return_false(vm, PS_ERROR_TYPE_MISMATCH);
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

#define NUMBER_CASE_DIV_MOD(__KEY__, __A__, __B__, __OP__, __R__, __TYPE__)                                            \
    case (__KEY__):                                                                                                    \
        if (b->data.__B__ == 0)                                                                                        \
            return ps_vm_return_false(vm, PS_ERROR_DIVISION_BY_ZERO);                                                  \
        result->data.__R__ = a->data.__A__ __OP__ b->data.__B__;                                                       \
        r = __TYPE__;                                                                                                  \
        break;

#define NUMBER_CASE_DIV_REAL(__KEY__, __A__, __B__)                                                                    \
    case (__KEY__):                                                                                                    \
        if ((ps_real)(b->data.__B__) == 0.0)                                                                           \
            return ps_vm_return_false(vm, PS_ERROR_DIVISION_BY_ZERO);                                                  \
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

bool ps_function_binary_op(ps_vm *vm, ps_value *a, ps_value *b, ps_value *result, ps_vm_opcode opcode)
{
    //             OOOOOOOOOOO | AAAA                 | BBBB
    //             ------------+----------------------+--------------
    uint16_t key = opcode << 8 | (a->type->base << 4) | b->type->base;
    ps_value_type r = PS_TYPE_NONE;
    ps_string *s = NULL;

    switch (key)
    {
        // AND.U/I/B
        NUMBER_CASE(OP_AND << 8 | BB, b, b, &&, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_AND << 8 | II, i, i, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_AND << 8 | IU, i, u, &, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_AND << 8 | UI, u, i, &, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_AND << 8 | UU, u, u, &, u, PS_TYPE_UNSIGNED)
        // OR.U/I/B
        NUMBER_CASE(OP_OR << 8 | BB, b, b, ||, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_OR << 8 | II, i, i, |, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_OR << 8 | IU, i, u, |, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_OR << 8 | UI, u, i, |, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_OR << 8 | UU, u, u, |, u, PS_TYPE_UNSIGNED)
        // XOR.U/I/B
    case OP_XOR << 8 | BB:
        result->data.b = !(a->data.b) != !(b->data.b);
        r = PS_TYPE_BOOLEAN;
        break;
        NUMBER_CASE(OP_XOR << 8 | II, i, i, ^, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_XOR << 8 | IU, i, u, ^, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_XOR << 8 | UI, u, i, ^, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_XOR << 8 | UU, u, u, ^, u, PS_TYPE_UNSIGNED)
        // ADD.I/U/R/C/S
    case OP_ADD << 8 | CC: // char + char => string
        s = ps_string_alloc(2);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->str[1] = b->data.c;
        s->len = 2;
        result->data.s = s;
        r = PS_TYPE_STRING;
        break;
    case OP_ADD << 8 | CS: // char + string => string
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = a->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(s, b->data.s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        r = PS_TYPE_STRING;
        break;
    case OP_ADD << 8 | SC: // string + char => string
        s = ps_string_alloc(1);
        if (s == NULL)
            return false;
        s->str[0] = b->data.c;
        s->len = 1;
        result->data.s = ps_string_concat(a->data.s, s, PS_STRING_MAX_LEN);
        ps_string_free(s);
        r = PS_TYPE_STRING;
        break;
    case OP_ADD << 8 | SS: // string + string => string
        s = ps_string_concat(a->data.s, b->data.s, PS_STRING_MAX_LEN);
        if (s == NULL)
            return false;
        result->data.s = s;
        r = PS_TYPE_STRING;
        break;
        NUMBER_CASE(OP_ADD << 8 | II, i, i, +, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_ADD << 8 | IU, i, u, +, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_ADD << 8 | UI, u, i, +, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_ADD << 8 | UU, u, u, +, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_ADD << 8 | IR, i, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_ADD << 8 | RI, r, i, +, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_ADD << 8 | RR, r, r, +, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_ADD << 8 | RU, r, u, +, r, PS_TYPE_REAL)
        // SUB.I/U/R
        NUMBER_CASE(OP_SUB << 8 | II, i, i, -, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_SUB << 8 | IU, i, u, -, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_SUB << 8 | UI, u, i, -, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_SUB << 8 | UU, u, u, -, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_SUB << 8 | RR, r, r, -, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_SUB << 8 | RI, r, i, -, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_SUB << 8 | RU, r, u, -, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_SUB << 8 | IR, i, r, -, r, PS_TYPE_REAL)
        // MUL.I/U/R
        NUMBER_CASE(OP_MUL << 8 | II, i, i, *, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_MUL << 8 | IU, i, u, *, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_MUL << 8 | UI, u, i, *, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_MUL << 8 | UU, u, u, *, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_MUL << 8 | RR, r, r, *, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_MUL << 8 | RI, r, i, *, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_MUL << 8 | RU, r, u, *, r, PS_TYPE_REAL)
        NUMBER_CASE(OP_MUL << 8 | IR, i, r, *, r, PS_TYPE_REAL)
        // DIV.I/U
        NUMBER_CASE_DIV_MOD(OP_DIV << 8 | II, i, i, /, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(OP_DIV << 8 | IU, i, u, /, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(OP_DIV << 8 | UI, u, i, /, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(OP_DIV << 8 | UU, u, u, /, u, PS_TYPE_UNSIGNED)
        // DIV.R
        NUMBER_CASE_DIV_REAL(OP_DIV << 8 | IR, i, r)
        NUMBER_CASE_DIV_REAL(OP_DIV << 8 | RI, r, i)
        NUMBER_CASE_DIV_REAL(OP_DIV << 8 | RR, r, r)
        NUMBER_CASE_DIV_REAL(OP_DIV << 8 | RU, r, u)
        NUMBER_CASE_DIV_REAL(OP_DIV << 8 | UR, u, r)
        // MOD.I/U
        NUMBER_CASE_DIV_MOD(OP_MOD << 8 | II, i, i, %, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(OP_MOD << 8 | IU, i, u, %, i, PS_TYPE_INTEGER)
        NUMBER_CASE_DIV_MOD(OP_MOD << 8 | UI, u, i, %, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE_DIV_MOD(OP_MOD << 8 | UU, u, u, %, u, PS_TYPE_UNSIGNED)
        // EQ.I/U/R/C/S
        NUMBER_CASE(OP_CEQ << 8 | CC, c, c, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | II, i, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | IR, i, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | IU, i, u, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | RI, r, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | RR, r, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | RU, r, u, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | UI, u, i, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | UR, u, r, ==, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CEQ << 8 | UU, u, u, ==, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(OP_CEQ << 8 | CS, ps_string_eq)
        STRING_CASE_SC(OP_CEQ << 8 | SC, ps_string_eq)
        STRING_CASE_SS(OP_CEQ << 8 | SS, ps_string_eq)
        // NE.I/U/R/C/S
        NUMBER_CASE(OP_CNE << 8 | II, i, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | IU, i, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | UI, u, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | UU, u, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | RR, r, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | RI, r, i, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | RU, r, u, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | IR, i, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | UR, u, r, !=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CNE << 8 | CC, c, c, !=, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(OP_CNE << 8 | CS, ps_string_ne)
        STRING_CASE_SC(OP_CNE << 8 | SC, ps_string_ne)
        STRING_CASE_SS(OP_CNE << 8 | SS, ps_string_ne)
        // LT.I/U/R/C/S
        NUMBER_CASE(OP_CLT << 8 | II, i, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | IU, i, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | UI, u, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | UU, u, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | RR, r, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | RI, r, i, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | RU, r, u, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | IR, i, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | UR, u, r, <, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLT << 8 | CC, c, r, <, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(OP_CLT << 8 | CS, ps_string_lt)
        STRING_CASE_SC(OP_CLT << 8 | SC, ps_string_lt)
        STRING_CASE_SS(OP_CLT << 8 | SS, ps_string_lt)
        // LE.I/U/R/C/S
        NUMBER_CASE(OP_CLE << 8 | II, i, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | IU, i, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | UI, u, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | UU, u, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | RR, r, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | RI, r, i, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | RU, r, u, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | IR, i, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | UR, u, r, <=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CLE << 8 | CC, c, c, <=, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(OP_CLE << 8 | CS, ps_string_le)
        STRING_CASE_SC(OP_CLE << 8 | SC, ps_string_le)
        STRING_CASE_SS(OP_CLE << 8 | SS, ps_string_le)
        // GT.I/U/R/C/S
        NUMBER_CASE(OP_CGT << 8 | II, i, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | IU, i, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | UI, u, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | UU, u, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | RR, r, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | RI, r, i, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | RU, r, u, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | IR, i, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | UR, u, r, >, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGT << 8 | CC, c, c, >, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(OP_CGT << 8 | CS, ps_string_gt)
        STRING_CASE_SC(OP_CGT << 8 | SC, ps_string_gt)
        STRING_CASE_SS(OP_CGT << 8 | SS, ps_string_gt)
        // GE.I/U/R/C/S
        NUMBER_CASE(OP_CGE << 8 | II, i, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | IU, i, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | UI, u, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | UU, u, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | RR, r, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | RI, r, i, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | RU, r, u, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | IR, i, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | UR, u, r, >=, b, PS_TYPE_BOOLEAN)
        NUMBER_CASE(OP_CGE << 8 | CC, c, c, >=, b, PS_TYPE_BOOLEAN)
        STRING_CASE_CS(OP_CGE << 8 | CS, ps_string_ge)
        STRING_CASE_SC(OP_CGE << 8 | SC, ps_string_ge)
        STRING_CASE_SS(OP_CGE << 8 | SS, ps_string_ge)
        // SHL.I/U
        NUMBER_CASE(OP_SHL << 8 | II, i, i, <<, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_SHL << 8 | IU, i, u, <<, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_SHL << 8 | UI, u, i, <<, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_SHL << 8 | UU, u, u, <<, u, PS_TYPE_UNSIGNED)
        // SHR.I/U
        NUMBER_CASE(OP_SHR << 8 | II, i, i, >>, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_SHR << 8 | IU, i, u, >>, i, PS_TYPE_INTEGER)
        NUMBER_CASE(OP_SHR << 8 | UI, u, i, >>, u, PS_TYPE_UNSIGNED)
        NUMBER_CASE(OP_SHR << 8 | UU, u, u, >>, u, PS_TYPE_UNSIGNED)
    default:
        if (vm->debug)
        {
            fprintf(stderr, "*** ERROR: [1] binary operator %s not applicable for types %s and %s\n",
                    ps_vm_get_opcode_name(opcode), ps_value_type_get_name(a->type->base),
                    ps_value_type_get_name(b->type->base));
        }
        return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
    }
    if (result->type != NULL && result->type->base != PS_TYPE_NONE)
    {
        // check if the expected result type is compatible with the result type
        if (result->type->base != r)
        {
            if (vm->debug)
            {
                fprintf(stderr, "*** ERROR: [2] binary operator %s not applicable for types %s and %s\n",
                        ps_vm_get_opcode_name(opcode), ps_value_type_get_name(a->type->base),
                        ps_value_type_get_name(b->type->base));
            }
            return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
    }
    else
    {
        // if the expected result type is unknown, we take result type converting base type to the system type
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
            if (vm->debug)
            {
                fprintf(stderr, "*** ERROR: [3] Unknown binary operator %s for types %s and %s\n",
                        ps_token_get_keyword(opcode), ps_value_type_get_name(a->type->base),
                        ps_value_type_get_name(b->type->base));
            }
            return ps_vm_return_false(vm, PS_ERROR_OPERATOR_NOT_APPLICABLE);
        }
    }
    return true;
}

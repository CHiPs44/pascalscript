/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_H
#define _PS_SYSTEM_H

#include "ps_interpreter.h"
#include "ps_symbol.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Type definition type defintion (!) */
    extern ps_symbol ps_system_type_def;

    /* System types (with type==base) */
    extern ps_symbol ps_system_none;
    extern ps_symbol ps_system_boolean;
    extern ps_symbol ps_system_char;
    extern ps_symbol ps_system_integer;
    extern ps_symbol ps_system_unsigned;
    extern ps_symbol ps_system_real;
    extern ps_symbol ps_system_string;
    extern ps_symbol ps_system_procedure;
    extern ps_symbol ps_system_function;

    /* Derived types */
    extern ps_symbol ps_system_subrange;
    extern ps_symbol ps_system_enum;

    /* System procedures & functions */
    extern ps_symbol ps_system_function_abs;
    extern ps_symbol ps_system_function_abs;
    extern ps_symbol ps_system_function_arctan;
    extern ps_symbol ps_system_function_chr;
    extern ps_symbol ps_system_function_cos;
    extern ps_symbol ps_system_function_even;
    extern ps_symbol ps_system_function_exp;
    extern ps_symbol ps_system_function_frac;
    extern ps_symbol ps_system_function_get_tick_count;
    extern ps_symbol ps_system_function_int;
    extern ps_symbol ps_system_function_ln;
    extern ps_symbol ps_system_function_log;
    extern ps_symbol ps_system_function_odd;
    extern ps_symbol ps_system_function_ord;
    extern ps_symbol ps_system_function_pred;
    extern ps_symbol ps_system_function_random;
    extern ps_symbol ps_system_function_round;
    extern ps_symbol ps_system_function_sin;
    extern ps_symbol ps_system_function_sqr;
    extern ps_symbol ps_system_function_sqrt;
    extern ps_symbol ps_system_function_succ;
    extern ps_symbol ps_system_function_tan;
    extern ps_symbol ps_system_function_trunc;
    extern ps_symbol ps_system_procedure_randomize;
    extern ps_symbol ps_system_procedure_read;
    extern ps_symbol ps_system_procedure_readln;
    extern ps_symbol ps_system_procedure_write;
    extern ps_symbol ps_system_procedure_writeln;

    /* System constants */
    extern ps_symbol ps_system_constant_boolean_false;
    extern ps_symbol ps_system_constant_boolean_true;
    extern ps_symbol ps_system_constant_integer_maxint;
    extern ps_symbol ps_system_constant_integer_minint;
    extern ps_symbol ps_system_constant_unsigned_maxuint;
    extern ps_symbol ps_system_constant_real_maxreal;
    extern ps_symbol ps_system_constant_real_minreal;
    extern ps_symbol ps_system_constant_real_epsreal;
    extern ps_symbol ps_system_constant_real_pi;
    extern ps_symbol ps_system_constant_unsigned_ps_bitness;
    extern ps_symbol ps_system_constant_string_ps_version;
    extern ps_symbol ps_system_constant_unsigned_ps_version_index;
    extern ps_symbol ps_system_constant_unsigned_ps_version_major;
    extern ps_symbol ps_system_constant_unsigned_ps_version_minor;
    extern ps_symbol ps_system_constant_unsigned_ps_version_patch;

    bool ps_system_init(ps_interpreter *interpreter);
    void ps_system_done(ps_interpreter *interpreter);

#ifdef __cplusplus
}
#endif

#endif

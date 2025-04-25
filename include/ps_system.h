/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_H
#define _PS_SYSTEM_H

#include "ps_symbol.h"
#include "ps_interpreter.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /* System types (with type==base) */
    extern ps_symbol ps_system_none;
    extern ps_symbol ps_system_boolean;
    extern ps_symbol ps_system_char;
    extern ps_symbol ps_system_integer;
    extern ps_symbol ps_system_real;
    extern ps_symbol ps_system_unsigned;

    /* System procedures & functions */
    extern ps_symbol ps_system_procedure_read;
    extern ps_symbol ps_system_procedure_readln;
    extern ps_symbol ps_system_procedure_write;
    extern ps_symbol ps_system_procedure_writeln;
    extern ps_symbol ps_system_procedure_randomize;
    extern ps_symbol ps_system_function_odd;
    extern ps_symbol ps_system_function_even;
    extern ps_symbol ps_system_function_abs;
    extern ps_symbol ps_system_function_chr;
    extern ps_symbol ps_system_function_ord;
    extern ps_symbol ps_system_function_succ;
    extern ps_symbol ps_system_function_pred;

    /* System constants */
    extern ps_symbol ps_system_version_major;
    extern ps_symbol ps_system_version_minor;
    extern ps_symbol ps_system_version_patch;
    extern ps_symbol ps_system_version_index;

    extern bool ps_system_init(ps_interpreter *interpreter);
    extern void ps_system_done(ps_interpreter *interpreter);

#ifdef __cplusplus
}
#endif

#endif

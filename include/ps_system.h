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

#ifndef PS_SYSTEM_SYMBOL_TABLE_SIZE
#define PS_SYSTEM_SYMBOL_TABLE_SIZE 64
#endif

#define PS_SYSTEM_FUNCTION(TYPE, VALUE, NAME, CALLABLE_FIELD, CALLABLE)                                                \
    ps_executable ps_executable_##TYPE##_##VALUE = {CALLABLE_FIELD = CALLABLE, .formal_signature = NULL, .line = 0,    \
                                                    .column = 0};                                                      \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.x = &ps_executable_##TYPE##_##VALUE}};  \
    ps_symbol ps_system_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_FUNCTION,                                           \
                                            .name = NAME,                                                              \
                                            .value = &ps_value_##TYPE##_##VALUE,                                       \
                                            .system = true,                                                            \
                                            .allocated = false}
#define PS_SYSTEM_PROCEDURE(TYPE, VALUE, NAME, CALLABLE_FIELD, CALLABLE)                                               \
    ps_executable ps_executable_##TYPE##_##VALUE = {CALLABLE_FIELD = CALLABLE, .formal_signature = NULL, .line = 0,    \
                                                    .column = 0};                                                      \
    ps_value ps_value_##TYPE##_##VALUE = {.type = &ps_system_##TYPE, .data = {.x = &ps_executable_##TYPE##_##VALUE}};  \
    ps_symbol ps_system_##TYPE##_##VALUE = {.kind = PS_SYMBOL_KIND_PROCEDURE,                                          \
                                            .name = NAME,                                                              \
                                            .value = &ps_value_##TYPE##_##VALUE,                                       \
                                            .system = true,                                                            \
                                            .allocated = false}

#define ADD_SYSTEM_SYMBOL(__SYMBOL__)                                                                                  \
    if (!ps_environment_add_symbol(system, &__SYMBOL__))                                                               \
        goto error;

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
    extern ps_symbol ps_system_enum;
    extern ps_symbol ps_system_subrange_char;
    extern ps_symbol ps_system_subrange_integer;
    extern ps_symbol ps_system_subrange_unsigned;
    extern ps_symbol ps_system_subrange_enum;

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

    bool ps_system_init(ps_environment *system);
    void ps_system_done(const ps_environment *system);

#ifdef __cplusplus
}
#endif

#endif

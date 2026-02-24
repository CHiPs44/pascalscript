/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_PROCEDURES_H
#define _PS_PROCEDURES_H

#include <stdio.h>

#include "ps_interpreter.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_procedures_init(ps_environment *system);

    extern ps_symbol ps_system_procedure_read;
    extern ps_symbol ps_system_procedure_readln;
    extern ps_symbol ps_system_procedure_write;
    extern ps_symbol ps_system_procedure_writeln;
    extern ps_symbol ps_system_procedure_randomize;

    /* clang-format off */
    bool ps_procedure_read     (const ps_interpreter *interpreter, const FILE *f, ps_value *value);
    bool ps_procedure_readln   (const ps_interpreter *interpreter, const FILE *f, ps_value *value);
    bool ps_procedure_write    (const ps_interpreter *interpreter, const FILE *f, const ps_value *value, int16_t width, int16_t precision);
    bool ps_procedure_writeln  (const ps_interpreter *interpreter, const FILE *f, const ps_value *value, int16_t width, int16_t precision);
    bool ps_procedure_randomize(const ps_interpreter *interpreter, const ps_value *value);
    /* clang-format on */

#ifdef __cplusplus
}
#endif

#endif /* _PS_PROCEDURES_H */

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

    bool ps_procedure_read(ps_interpreter *interpreter, FILE *f, ps_value *value);
    bool ps_procedure_readln(ps_interpreter *interpreter, FILE *f, ps_value *value);
    bool ps_procedure_write(ps_interpreter *interpreter, FILE *f, ps_value *value);
    bool ps_procedure_writeln(ps_interpreter *interpreter, FILE *f, ps_value *value);
    bool ps_procedure_randomize(ps_interpreter *interpreter, FILE *f, ps_value *value);

#ifdef __cplusplus
}
#endif

#endif /* _PS_PROCEDURES_H */

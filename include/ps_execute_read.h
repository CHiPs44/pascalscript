/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_EXECUTE_READ_H
#define _PS_EXECUTE_READ_H

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Execute Read, ReadLn, or ReadStr procedure call */
    bool ps_execute_procedure_read_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTE_READ_H */

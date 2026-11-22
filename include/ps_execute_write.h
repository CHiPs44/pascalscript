/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_EXECUTE_WRITE_H
#define _PS_EXECUTE_WRITE_H

#include "ps_ast.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Execute Write, WriteLn, or WriteStr procedure call */
    bool ps_execute_procedure_write_call(ps_interpreter *interpreter, const ps_ast_call *procedure_call);

#ifdef __cplusplus
}
#endif

#endif /* _PS_EXECUTE_WRITE_H */

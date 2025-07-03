/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_COMPILE_H
#define _PS_COMPILE_H

#include <stdint.h>

#include "ps_interpreter.h"

#ifdef __cplusplus
extern "C"
{
#endif

    bool ps_parse_start(ps_interpreter *interpreter, bool exec);

#ifdef __cplusplus
}
#endif

#endif /* _PS_COMPILE_H */

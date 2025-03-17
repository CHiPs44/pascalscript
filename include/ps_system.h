/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_H
#define _PS_SYSTEM_H

#include "ps_runtime.h"

#ifdef __cplusplus
extern "C"
{
#endif

    void ps_system_init(ps_runtime *runtime);
    void ps_system_done(ps_runtime *runtime);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYSTEM_H */

/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_SYSTEM_H
#define _PS_SYSTEM_H

#include "ps_symbol.h"
#include "ps_interpreter.h"

#ifdef __cplusplus
extern "C"
{
#endif

    extern ps_symbol ps_symbol_version_major;
    extern ps_symbol ps_symbol_version_minor;
    extern ps_symbol ps_symbol_version_patch;
    extern ps_symbol ps_symbol_version_index;

    extern void ps_system_init(ps_interpreter *interpreter);
    extern void ps_system_done(ps_interpreter *interpreter);

#ifdef __cplusplus
}
#endif

#endif /* _PS_SYSTEM_H */

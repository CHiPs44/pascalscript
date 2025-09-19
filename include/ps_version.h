/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_VERSION_H
#define _PS_VERSION_H

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef PS_VERSION_MAJOR
#define PS_VERSION_MAJOR 0
#endif
#ifndef PS_VERSION_MINOR
#define PS_VERSION_MINOR 1
#endif
#ifndef PS_VERSION_PATCH
#define PS_VERSION_PATCH 2
#endif
#ifndef PS_VERSION_INDEX
#define PS_VERSION_INDEX 3
#endif

#define STR0(x) #x
#define STR(x) STR0(x)
#define PS_VERSION STR(PS_VERSION_MAJOR) "." STR(PS_VERSION_MINOR) "." STR(PS_VERSION_PATCH) "." STR(PS_VERSION_INDEX)

#ifdef __cplusplus
}
#endif

#endif /* _PS_VERSION_H */

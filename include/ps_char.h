/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe 'CHiPs' Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_CHAR_H
#define _PS_CHAR_H

#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /// @brief Converts a character to lowercase
    /// @param c
    /// @return The lowercase version of the character
    ps_char ps_char_lowercase(ps_char c);

    /// @brief Converts a character to uppercase
    /// @param c
    /// @return The uppercase version of the character
    ps_char ps_char_uppercase(ps_char c);

#ifdef __cplusplus
}
#endif

#endif /* _PS_CHAR_H */

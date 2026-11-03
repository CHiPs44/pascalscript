/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_char.h"

ps_char ps_char_lowercase(ps_char c)
{
    if (c >= 'A' && c <= 'Z')
        return c + ('a' - 'A'); // PLUS!
    return c;
}

ps_char ps_char_uppercase(ps_char c)
{
    if (c >= 'a' && c <= 'z')
        return c - ('a' - 'A'); // MINUS!
    return c;
}

/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _KEYWORDS_H
#define _KEYWORDS_H

#include "lexer.h"

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct _keyword_t
    {
        int token;
        char *keyword;
        bool symbolic;
    } keyword_t;

    extern keyword_t keywords[];

#ifdef __cplusplus
}
#endif

#endif /* _KEYWORDS_H */

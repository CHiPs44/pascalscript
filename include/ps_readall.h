/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_READALL_H
#define _PS_READALL_H

#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

/* Size of each input chunk to be read and allocate for. */
#ifndef PS_READALL_CHUNK
#define PS_READALL_CHUNK 1024
#endif

#define PS_READALL_OK 0       /* Success */
#define PS_READALL_INVALID -1 /* Invalid parameters */
#define PS_READALL_ERROR -2   /* Stream error */
#define PS_READALL_TOOMUCH -3 /* Too much input */
#define PS_READALL_NOMEM -4   /* Out of memory */

    int ps_readall(FILE *in, char **dataptr, size_t *sizeptr);

#ifdef __cplusplus
}
#endif

#endif /* _PS_READALL_H */

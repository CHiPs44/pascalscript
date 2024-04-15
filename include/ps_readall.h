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
#ifndef READALL_CHUNK
#define READALL_CHUNK 1024
#endif

#define  READALL_OK          0  /* Success */
#define  READALL_INVALID    -1  /* Invalid parameters */
#define  READALL_ERROR      -2  /* Stream error */
#define  READALL_TOOMUCH    -3  /* Too much input */
#define  READALL_NOMEM      -4  /* Out of memory */

int readall(FILE *in, char **dataptr, size_t *sizeptr);

#ifdef __cplusplus
}
#endif

#endif /* _PS_READALL_H */

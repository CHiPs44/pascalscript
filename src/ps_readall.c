/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

// From https://stackoverflow.com/questions/14002954/c-programming-how-to-read-the-whole-file-contents-into-a-buffer

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "ps_readall.h"

/* This function returns one of the READALL_ constants above.
   If the return value is zero == PS_READALL_OK, then:
     (*dataptr) points to a dynamically allocated buffer, with
     (*sizeptr) chars read from the file.
     The buffer is allocated for one extra char, which is NUL,
     and automatically appended after the data.
   Initial values of (*dataptr) and (*sizeptr) are ignored.
*/
int ps_readall(FILE *in, char **dataptr, size_t *sizeptr)
{
    char *data = NULL, *temp;
    size_t size = 0;
    size_t used = 0;
    size_t n;

    /* None of the parameters can be NULL. */
    if (in == NULL || dataptr == NULL || sizeptr == NULL)
        return PS_READALL_INVALID;
    /* A read error already occurred? */
    if (ferror(in))
        return PS_READALL_ERROR;
    while (1)
    {
        if (used + PS_READALL_CHUNK + 1 > size)
        {
            size = used + PS_READALL_CHUNK + 1;
            /* Overflow check. Some ANSI C compilers may optimize this away, though. */
            if (size <= used)
            {
                free(data);
                return PS_READALL_TOOMUCH;
            }
            temp = realloc(data, size);
            if (temp == NULL)
            {
                free(data);
                return PS_READALL_NOMEM;
            }
            data = temp;
        }
        n = fread(data + used, 1, PS_READALL_CHUNK, in);
        if (n == 0)
            break;
        used += n;
    }
    if (ferror(in))
    {
        free(data);
        return PS_READALL_ERROR;
    }
    temp = realloc(data, used + 1);
    if (temp == NULL)
    {
        free(data);
        return PS_READALL_NOMEM;
    }
    data = temp;
    data[used] = '\0';
    *dataptr = data;
    *sizeptr = used;

    return PS_READALL_OK;
}

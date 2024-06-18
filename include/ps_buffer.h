/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#ifndef _PS_BUFFER_H
#define _PS_BUFFER_H

#include <stdbool.h>
#include <stdint.h>

#include "ps_error.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef BUFFER_MAX_LINES
#define BUFFER_MAX_LINES 1000
#endif

#ifndef BUFFER_MAX_COLUMNS
#define BUFFER_MAX_COLUMNS 255
#endif

    typedef struct _buffer_t
    {
        // clang-format off
        char       *text;
        size_t      length;
        uint16_t    line_count;
        char      **line_starts;
        uint8_t    *line_lengths;
        error_t     error;
        // clang-format off
    } buffer_t;

    /**
     * @brief Init / reset buffer 
     */
    void ps_buffer_init(buffer_t *buffer);

    /**
     * @brief Scan source for line starts & lengths
     */
    bool ps_buffer_scan_text(buffer_t *buffer);

    /**
     * @brief Load file into source buffer
     */
    bool ps_buffer_load_file(buffer_t *buffer, char *filename);

    /**
     * @brief Set source code from memory buffer
     */
    bool ps_buffer_set_text(buffer_t *buffer, char *source, size_t length);

    void ps_buffer_dump(buffer_t *buffer, uint16_t from_line, uint16_t to_line);

#ifdef __cplusplus
}
#endif

#endif /* _PS_BUFFER_H */

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
#define BUFFER_MAX_LINES 1024
#endif

#ifndef BUFFER_MAX_COLUMNS
#define BUFFER_MAX_COLUMNS 256
#endif

#if BUFFER_MAX_COLUMNS <= 256
#define BUFFER_LINE_LENGTH uint8_t
#else
#define BUFFER_LINE_LENGTH uint16_t
#endif

    typedef struct _buffer_t
    {
        error_t error;
        char *text;
        size_t length;
        unsigned int line_count;
        char **line_starts;
        BUFFER_LINE_LENGTH *line_lengths;
    } buffer_t;

    /**
     * @brief Init / reset buffer 
     */
    void buffer_init(buffer_t *buffer);

    /**
     * @brief Scan source for line starts & lengths
     */
    bool buffer_scan_text(buffer_t *buffer);

    /**
     * @brief Load file into source buffer
     */
    bool buffer_load_file(buffer_t *buffer, char *filename);

    /**
     * @brief Set source code from memory buffer
     */
    bool buffer_set_text(buffer_t *buffer, char *source, size_t length);

    void buffer_list_text(buffer_t *buffer, int from_line, int to_line);

    void lexer_reset_cursor(buffer_t *buffer);

    char buffer_reset_cursor(buffer_t *buffer);

    char buffer_read_next_char(buffer_t *buffer);

    char buffer_peek_next_char(buffer_t *buffer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_BUFFER_H */

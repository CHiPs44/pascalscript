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

#ifndef PS_BUFFER_MAX_LINES
#define PS_BUFFER_MAX_LINES 1000
#endif

#ifndef PS_BUFFER_MAX_COLUMNS
#define PS_BUFFER_MAX_COLUMNS 255
#endif

    typedef struct _ps_buffer_t
    {
        // clang-format off
        char       *text;
        uint32_t    length;
        uint16_t    line_count;
        char      **line_starts;
        uint8_t    *line_lengths;
        uint16_t    current_line;
        uint8_t     current_column;
        char        current_char;
        char        next_char;
        ps_error_t  error;
        // clang-format off
    } ps_buffer_t;

    /**
     * @brief Init / reset buffer
     */
    bool ps_buffer_init(ps_buffer_t *buffer);

    /**
     * @brief Release buffer
     */
    bool ps_buffer_done(ps_buffer_t *buffer);

    /**
     * @brief Reset buffer
     */
    void ps_buffer_reset(ps_buffer_t *buffer);

    /**
     * @brief Scan source for line starts & lengths
     */
    bool ps_buffer_scan_text(ps_buffer_t *buffer);

    /**
     * @brief Load file into source buffer
     */
    bool ps_buffer_load_file(ps_buffer_t *buffer, char *filename);

    /**
     * @brief Set source code from memory buffer
     */
    bool ps_buffer_set_text(ps_buffer_t *buffer, char *source, size_t length);

    /**
     * @brief Dump content from one line for one "page"
     */
    void ps_buffer_dump(ps_buffer_t *buffer, uint16_t from_line, uint16_t page_size);

    /**
     * @brief Read next char of buffer
     */
    bool ps_buffer_read_next_char(ps_buffer_t *buffer);

    /**
     * @brief Get current char of buffer
     */
    char ps_buffer_peek_char(ps_buffer_t *buffer);

    /**
     * @brief Peek next char of buffer
     */
    char ps_buffer_peek_next_char(ps_buffer_t *buffer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_BUFFER_H */

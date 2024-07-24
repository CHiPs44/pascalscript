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

    typedef struct s_ps_buffer
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
        ps_error    error;
        int         file_errno;
        uint8_t     debug; // 0=OFF, 1=debug, 2=verbose
        // clang-format on
    } ps_buffer;

    /** @brief Init / reset buffer */
    bool ps_buffer_init(ps_buffer *buffer);

    /** @brief Release buffer */
    bool ps_buffer_done(ps_buffer *buffer);

    /** @brief Reset buffer */
    void ps_buffer_reset(ps_buffer *buffer);

    /** @brief Get error message */
    char *ps_buffer_show_error(ps_buffer *buffer);

    /** @brief Scan source for line starts & lengths */
    bool ps_buffer_scan_text(ps_buffer *buffer);

    /** @brief Load file into source buffer */
    bool ps_buffer_load_file(ps_buffer *buffer, char *filename);

    /** @brief Set source code from memory buffer */
    bool ps_buffer_set_text(ps_buffer *buffer, char *source, size_t length);

    /** @brief Dump content from one line for one "page" */
    void ps_buffer_dump(ps_buffer *buffer, uint16_t from_line, uint16_t page_size);

    /** @brief Read next char of buffer */
    bool ps_buffer_read_next_char(ps_buffer *buffer);

    /** @brief Get current char of buffer */
    char ps_buffer_peek_char(ps_buffer *buffer);

    /** @brief Peek next char of buffer */
    char ps_buffer_peek_next_char(ps_buffer *buffer);

#ifdef __cplusplus
}
#endif

#endif /* _PS_BUFFER_H */

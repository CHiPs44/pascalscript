/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "ps_error.h"
#include "ps_readall.h"
#include "ps_buffer.h"

void buffer_init(buffer_t *buffer)
{
    buffer->error = BUFFER_ERROR_NONE;
    buffer->length = 0;
    buffer->line_count = 0;
    buffer->line_lengths = NULL;
    buffer->line_starts = NULL;
    buffer->text = NULL;
}

bool buffer_scan_text(buffer_t *buffer)
{
    int line = 0;
    int column = 0;
    char *text = buffer->text;
    char *start = text;
    char c;
    // Count lines
    buffer->line_count = 0;
    c = *text++;
    while (c)
    {
        if (c == '\n')
            buffer->line_count += 1;
        c = *text++;
    }
    // Allocate arrays for line starts and lengths
    if (buffer->line_starts != NULL)
        free(buffer->line_starts);
    buffer->line_starts = calloc(buffer->line_count, sizeof(char *));
    if (buffer->line_starts == NULL)
    {
        buffer->error = BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    }
    if (buffer->line_lengths != NULL)
        free(buffer->line_lengths);
    buffer->line_lengths = calloc(buffer->line_count, sizeof(BUFFER_LINE_LENGTH));
    if (buffer->line_lengths == NULL)
    {
        buffer->error = BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    }
    // Find and memorize line starts
    text = buffer->text;
    bool stop = false;
    while (!stop)
    {
        c = *text;
        switch (c)
        {
        case '\n':
            if (line == BUFFER_MAX_LINES)
            {
                buffer->error = LEXER_ERROR_BUFFER_OVERFLOW;
                return false;
            }
            else
            {
                buffer->line_starts[line] = start;
                buffer->line_lengths[line] = text - start;
                line += 1;
                column = 0;
                text += 1;
                start = text;
            }
            break;
        case '\0':
            stop = true;
            break;
        default:
            column += 1;
            text += 1;
            break;
        }
    }
    buffer->line_count = line;
    buffer->error = BUFFER_ERROR_NONE;
    return true;
}

bool buffer_load_file(buffer_t *buffer, char *filename)
{
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        buffer->error = BUFFER_ERROR_OPENING_FILE;
        return false;
    }
    int result = readall(input, &(buffer->text), &(buffer->length));
    fclose(input);
    switch (result)
    {
    case READALL_ERROR:
        buffer->error = BUFFER_ERROR_READING_FILE;
        return false;
    case READALL_NOMEM:
        buffer->error = BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    case READALL_OK:
        buffer->error = BUFFER_ERROR_NONE;
        return buffer_scan_text(buffer);
    default:
        buffer->error = BUFFER_ERROR_READING_FILE;
        return false;
    }
}

bool buffer_set_text(buffer_t *buffer, char *text, size_t length)
{
    buffer->text = text;
    buffer->length = length;
    if (buffer->line_starts != NULL)
    {
        free(buffer->line_starts);
        buffer->line_starts = NULL;
    }
    if (buffer->line_lengths != NULL)
    {
        free(buffer->line_lengths);
        buffer->line_lengths = NULL;
    }
    return buffer_scan_text(buffer);
}

void buffer_list_text(buffer_t *buffer, int from_line, int to_line)
{
    char line_[BUFFER_MAX_COLUMNS + 1];

    from_line =
        from_line < 0 ? 0 : from_line > buffer->line_count ? buffer->line_count
                                                           : from_line;
    to_line =
        to_line < 0 ? 0 : to_line > buffer->line_count ? buffer->line_count
                                                       : to_line;
    if (from_line > to_line)
    {
        int temp = from_line;
        from_line = to_line;
        to_line = temp;
    }
    printf("12345 (12345) |1234567890123456789012345678901234567890|\n");
    for (int line = from_line; line < to_line; line += 1)
    {
        strncpy(line_, buffer->line_starts[line], buffer->line_lengths[line]);
        line_[buffer->line_lengths[line]] = '\0';
        printf("%05d (%05d) |%-40s|\n", line + 1, buffer->line_lengths[line], line_);
    }
}

/* EOF */

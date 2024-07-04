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

bool ps_buffer_init(ps_buffer_t *buffer)
{
    buffer->text = NULL;
    buffer->length = 0;
    buffer->line_count = 0;
    if (buffer->line_lengths != NULL)
        free(buffer->line_lengths);
    buffer->line_lengths = NULL;
    if (buffer->line_starts != NULL)
        free(buffer->line_starts);
    buffer->line_starts = NULL;
    buffer->error = BUFFER_ERROR_NONE;
    return true;
}

bool ps_buffer_done(ps_buffer_t *buffer)
{
    if (buffer->line_lengths != NULL)
        free(buffer->line_lengths);
    buffer->line_lengths = NULL;
    if (buffer->line_starts != NULL)
        free(buffer->line_starts);
    memset(buffer, 0, sizeof(ps_buffer_t));
    return true;
}

void ps_buffer_reset(ps_buffer_t *buffer)
{
    buffer->current_line = 0;
    buffer->current_column = 0;
    buffer->current_char = '\0';
}

bool ps_buffer_scan_text(ps_buffer_t *buffer)
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
    {
        free(buffer->line_starts);
        buffer->line_starts = NULL;
    }
    if (buffer->line_lengths != NULL)
    {
        free(buffer->line_lengths);
        buffer->line_lengths = NULL;
    }
    if (buffer->line_count == 0)
    {
        buffer->error = BUFFER_ERROR_NONE;
        return true;
    }
    buffer->line_starts = calloc(buffer->line_count, sizeof(char *));
    if (buffer->line_starts == NULL)
    {
        buffer->error = BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    }
    buffer->line_lengths = calloc(buffer->line_count, sizeof(uint16_t));
    if (buffer->line_lengths == NULL)
    {
        free(buffer->line_starts);
        buffer->line_starts = NULL;
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
            if (line == PS_BUFFER_MAX_LINES)
            {
                buffer->error = BUFFER_ERROR_OVERFLOW_LINES;
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
    buffer->error = BUFFER_ERROR_NONE;
    return true;
}

bool ps_buffer_load_file(ps_buffer_t *buffer, char *filename)
{
    size_t length;
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        buffer->error = BUFFER_ERROR_OPENING_FILE;
        return false;
    }
    int result = ps_readall(input, &(buffer->text), &length);
    fclose(input);
    switch (result)
    {
    case PS_READALL_ERROR:
        buffer->error = BUFFER_ERROR_READING_FILE;
        return false;
    case PS_READALL_NOMEM:
        buffer->error = BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    case PS_READALL_OK:
        if (length > UINT16_MAX)
        {
            free(buffer->text);
            buffer->error = BUFFER_ERROR_OVERFLOW;
            return false;
        }
        buffer->length = length;
        buffer->error = BUFFER_ERROR_NONE;
        return ps_buffer_scan_text(buffer);
    default:
        buffer->error = BUFFER_ERROR_READING_FILE;
        return false;
    }
}

bool ps_buffer_set_text(ps_buffer_t *buffer, char *text, size_t length)
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
    return ps_buffer_scan_text(buffer);
}

void ps_buffer_dump(ps_buffer_t *buffer, uint16_t from_line, uint16_t page_size)
{
    char line[PS_BUFFER_MAX_COLUMNS + 1];

    if (buffer->line_count == 0)
    {
        printf("EMPTY!\n");
        return;
    }
    printf("%d => %d for %d lines\n", buffer->line_count, from_line, page_size);
    printf("            |         1         2         3         4         5         6         7         8|\n");
    printf("12345 (123) |12345678901234567890123456789012345678901234567890123456789012345678901234567890|\n");
    for (int line_number = from_line; line_number < from_line + page_size - 1; line_number += 1)
    {
        if (line_number >= buffer->line_count)
            break;
        strncpy(line, buffer->line_starts[line_number], PS_BUFFER_MAX_COLUMNS);
        line[buffer->line_lengths[line_number]] = '\0';
        printf("%05d (%03d) |%-80s|\n", line_number + 1, buffer->line_lengths[line_number], line);
    }
    printf("\n");
}

bool ps_buffer_peek_char(ps_buffer_t *buffer, char *current_char)
{
    // printf("buffer_peek_char: char=%c alpha=%d, alnum=%d, digit=%d\n", buffer->current_char, isalpha(buffer->current_char), isalnum(buffer->current_char), isdigit(buffer->current_char));
    if (buffer->error != BUFFER_ERROR_NONE)
    {
        *current_char = '\0';
        return false;
    }
    *current_char = buffer->current_char;
    return true;
}

bool ps_buffer_peek_next_char(ps_buffer_t *buffer, char *next_char)
{
    if (buffer->current_line <= buffer->line_count)
    {
        if (buffer->current_column < buffer->line_lengths[buffer->current_line])
        {
            char c = buffer->line_starts[buffer->current_line][buffer->current_column];
            // printf("ps_buffer_peek_next_char: c=%d\n", c);
            *next_char = c;
            return true;
        }
        /* else if (buffer->current_line+1<=buffer->line_count) {
            *next_char = buffer->line_starts[buffer->current_line][buffer->current_column + 1];
            return true;
        }*/
    }
    *next_char = '\0';
    return true;
}

bool ps_buffer_read_next_char(ps_buffer_t *buffer)
{
    // printf("ps_buffer_read_next_char: line=%d, col=%d err=%d\n", buffer->current_line, buffer->current_column, buffer->error);
    buffer->current_char = '\0';
    if (buffer->current_line < buffer->line_count)
    {
        if (buffer->current_column <= buffer->line_lengths[buffer->current_line])
        {
            buffer->current_char = buffer->line_starts[buffer->current_line][buffer->current_column];
            // Advance to next char
            buffer->current_column += 1;
            if (buffer->current_column > buffer->line_lengths[buffer->current_line])
            {
                buffer->current_line += 1;
                buffer->current_column = 0;
                if (buffer->current_line > buffer->line_count)
                {
                    buffer->error = BUFFER_ERROR_UNEXPECTED_EOF;
                    buffer->current_char = '\0';
                    // printf("ps_buffer_read_next_char: line=%d/%d, col=%d err=%d\n",
                    //        buffer->current_line, buffer->line_count, buffer->current_column, buffer->error);
                    return false;
                }
            }
        }
    }
    // printf("ps_buffer_read_next_char: line=%05d col=%03d char=%c\n", buffer->current_line, buffer->current_column, buffer->current_char);
    return true;
}

/* EOF */

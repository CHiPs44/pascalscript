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

void ps_buffer_init(ps_buffer_t *buffer)
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
    buffer->error = BUFFER_ERROR_NONE;
    return true;
}

bool ps_buffer_load_file(ps_buffer_t *buffer, char *filename)
{
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        buffer->error = BUFFER_ERROR_OPENING_FILE;
        return false;
    }
    int result = ps_readall(input, &(buffer->text), &(buffer->length));
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
        return;
    // printf("%d => %d for %d lines\n", buffer->line_count, from_line, page_size);
    printf("            |         1         2         3         4         5         6         7         8|\n");
    printf("12345 (123) |12345678901234567890123456789012345678901234567890123456789012345678901234567890|\n");
    for (int line_number = from_line; line_number < from_line + page_size - 1; line_number += 1)
    {
        if (line_number >= buffer->line_count)
            return;
        strncpy(line, buffer->line_starts[line_number], PS_BUFFER_MAX_COLUMNS);
        line[buffer->line_lengths[line_number]] = '\0';
        printf("%05d (%03d) |%-80s|\n", line_number + 1, buffer->line_lengths[line_number], line);
    }
    printf("\n");
}

/* EOF */

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

void ps_buffer_free(ps_buffer_t *buffer)
{
    if (buffer->line_lengths != NULL)
        free(buffer->line_lengths);
    buffer->line_lengths = NULL;
    if (buffer->line_starts != NULL)
        free(buffer->line_starts);
    buffer->line_starts = NULL;
}

bool ps_buffer_init(ps_buffer_t *buffer)
{
    ps_buffer_free(buffer);
    buffer->text = NULL;
    buffer->length = 0;
    buffer->line_count = 0;
    buffer->current_char = '\0';
    buffer->next_char = '\0';
    buffer->error = BUFFER_ERROR_NONE;
    buffer->debug = 0;
    return true;
}

bool ps_buffer_done(ps_buffer_t *buffer)
{
    ps_buffer_free(buffer);
    memset(buffer, 0, sizeof(ps_buffer_t));
    return true;
}

void ps_buffer_reset(ps_buffer_t *buffer)
{
    buffer->current_line = 0;
    buffer->current_column = 0;
    buffer->current_char = '\0';
    buffer->next_char = '\0';
}

char *ps_buffer_show_error(ps_buffer_t *buffer)
{
    static char ps_buffer_error_message[256];
    snprintf(ps_buffer_error_message, 255, "LEXER: %d %s, line %d, column %d",
             buffer->error, ps_error_get_message(buffer->error),
             buffer->current_line, buffer->current_column);
    return ps_buffer_error_message;
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
    if (buffer->line_count > PS_BUFFER_MAX_LINES)
    {
        buffer->error = BUFFER_ERROR_OVERFLOW_LINES;
        return false;
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
    buffer->line_lengths = calloc(buffer->line_count, sizeof(uint8_t));
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
            buffer->line_starts[line] = start;
            buffer->line_lengths[line] = text - start;
            line += 1;
            column = 0;
            text += 1;
            start = text;
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

    printf("\n");
    printf("%d => %d for %d lines\n", buffer->line_count, from_line, page_size);
    if (buffer->line_count == 0)
    {
        printf("Buffer is EMPTY!\n");
        return;
    }
    printf("            |         1         2         3         4         5         6         7         8|\n");
    printf("Line  (Len) |12345678901234567890123456789012345678901234567890123456789012345678901234567890|\n");
    for (int line_number = from_line; line_number < from_line + page_size - 1; line_number += 1)
    {
        if (line_number >= buffer->line_count)
            break;
        strncpy(line, buffer->line_starts[line_number], buffer->line_lengths[line_number]);
        line[buffer->line_lengths[line_number]] = '\0';
        printf("%05d (%03d) |%-80s|\n", line_number + 1, buffer->line_lengths[line_number], line);
    }
    printf("\n");
}

char ps_buffer_peek_char(ps_buffer_t *buffer)
{
    return buffer->error == BUFFER_ERROR_NONE ? buffer->current_char : '\0';
}

char ps_buffer_peek_next_char(ps_buffer_t *buffer)
{
    return buffer->error == BUFFER_ERROR_NONE ? buffer->next_char : '\0';
}

char *ps_buffer_debug_char(char c)
{
    static char tmp[16];
    static char *ctrl[32] = {
        "NUL", "SOH", "STX", "ETX",
        "EOT", "ENQ", "ACK", "BEL",
        "BS ", "TAB", "LF ", "VT ",
        "FF ", "CR ", "SO ", "SI ",
        "DLE", "DC1", "DC2", "DC3",
        "DC4", "NAK", "SYN", "ETB",
        "CAN", "EM ", "SUB", "ESC",
        "FS ", "GS ", "RS ", "US "};
    if (c >= ' ')
        sprintf(tmp, " '%c' (0x%02x)", c, c);
    else
        sprintf(tmp, " %s (0x%02x)", ctrl[c + 0], c);
    return tmp;
}

static uint32_t ps_buffer_counter = 0;

bool ps_buffer_read_next_char(ps_buffer_t *buffer)
{
    ps_buffer_counter += 1;
    if (buffer->debug >= 2)
        printf("ps_buffer_read_next_char: BEGIN counter=%06d line=%05d col=%03d char=%s, next=%s\n",
               ps_buffer_counter,
               buffer->current_line, buffer->current_column,
               ps_buffer_debug_char(buffer->current_char),
               ps_buffer_debug_char(buffer->next_char));
    buffer->current_char = '\0';
    buffer->next_char = '\0';
    // already at end of buffer?
    if (buffer->error == BUFFER_ERROR_EOF)
        return false;
    // already after end of buffer?
    if (buffer->current_line >= buffer->line_count)
    {
        // printf("EOF!\n");
        buffer->error = BUFFER_ERROR_EOF;
        return false;
    }
    // we point to current char already
    buffer->current_char = buffer->line_starts[buffer->current_line][buffer->current_column];
    // advance to next char
    buffer->current_column += 1;
    // at end of line?
    if (buffer->current_column > buffer->line_lengths[buffer->current_line])
    {
        // go to next line
        buffer->current_line += 1;
        buffer->current_column = 0;
    }
    if (buffer->current_line > buffer->line_count)
    {
        buffer->next_char = '\0';
    }
    else
    {
        // printf("NEXT: Line %d/%d\n", buffer->current_line, buffer->line_count);
        // printf("NEXT: Col  %d/%d\n", buffer->current_column, buffer->line_lengths[buffer->current_line]);
        if (buffer->line_lengths[buffer->current_line] == 0)
            buffer->next_char = '\0';
        else
            buffer->next_char = buffer->line_starts[buffer->current_line][buffer->current_column];
        // printf("NEXT: End %s\n", ps_buffer_debug_char(buffer->next_char));
    }
    if (buffer->debug > 0)
        printf("ps_buffer_read_next_char: END   counter=%06d line=%05d col=%03d char=%s, next=%s\n",
               ps_buffer_counter,
               buffer->current_line, buffer->current_column,
               ps_buffer_debug_char(buffer->current_char),
               ps_buffer_debug_char(buffer->next_char));
    return true;
}

/* EOF */

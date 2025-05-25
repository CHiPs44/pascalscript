/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "ps_error.h"
#include "ps_readall.h"
#include "ps_buffer.h"

ps_buffer *ps_buffer_init()
{
    ps_buffer *buffer = calloc(1, sizeof(ps_buffer));
    if (buffer == NULL)
        return NULL;
    buffer->file_errno = 0;
    buffer->text = NULL;
    buffer->length = 0;
    buffer->line_count = 0;
    buffer->line_starts = NULL;
    buffer->line_lengths = NULL;
    ps_buffer_reset(buffer);
    return buffer;
}

void ps_buffer_done(ps_buffer *buffer)
{
    if (buffer->from_file)
        free(buffer->text);
    free(buffer->line_starts);
    free(buffer->line_lengths);
    free(buffer);
}

void ps_buffer_reset(ps_buffer *buffer)
{
    buffer->current_line = 0;
    buffer->current_column = 0;
    buffer->current_char = '\0';
    buffer->next_char = '\0';
    buffer->error = PS_BUFFER_ERROR_NONE;
}

char *ps_buffer_show_error(ps_buffer *buffer)
{
    static char error_message[128];
    snprintf(error_message, sizeof(error_message) - 1,
             "BUFFER: %d %s, line %d, column %d",
             buffer->error, ps_error_get_message(buffer->error),
             buffer->current_line, buffer->current_column);
    return error_message;
}

char *ps_buffer_debug_char(char c)
{
    static char tmp[8];
    // static char *ctrl[32] = {
    //     "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    //     "BS ", "TAB", "LF ", "VT ", "FF ", "CR ", "SO ", "SI ",
    //     "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    //     "CAN", "EM ", "SUB", "ESC", "FS ", "GS ", "RS ", "US "};
    if (c < ' ')
        // control characters
        snprintf(tmp, sizeof(tmp) - 1, "^%c/%02x", c + 'A' /*ctrl[c + 0]*/, c);
    else if (c < 0x7f)
        // ascii printable characters (DEL excluded)
        snprintf(tmp, sizeof(tmp) - 1, "%c/%02x", c, c);
    else
        // extended characters
        snprintf(tmp, sizeof(tmp) - 1, "%02x", c);
    return tmp;
}

void ps_buffer_debug(ps_buffer *buffer, char *message, FILE *f)
{
    if (f == NULL)
        f = stderr;
    fprintf(f,
            "%s line=%05d col=%03d current=%s, next=%s, error=%d %s\n",
            message,
            buffer->current_line, buffer->current_column,
            ps_buffer_debug_char(buffer->current_char),
            ps_buffer_debug_char(buffer->next_char),
            buffer->error, ps_error_get_message(buffer->error));
}

bool ps_buffer_scan_text(ps_buffer *buffer)
{
    int line = 0;
    int column = 0;
    char *text = buffer->text;
    char *start = text;
    char current_char, previous_char;
    // Count lines
    buffer->line_count = 0;
    current_char = *text++;
    previous_char = '\0';
    while (current_char)
    {
        if (current_char == '\r' || current_char == '\n')
            buffer->line_count += 1;
        previous_char = current_char;
        current_char = *text++;
        // Skip LF if CR encountered
        if (previous_char == '\r' && current_char == '\n')
        {
            previous_char = current_char;
            current_char = *text++;
        }
    }
    if (buffer->line_count > PS_BUFFER_MAX_LINES)
    {
        buffer->error = PS_BUFFER_ERROR_OVERFLOW_LINES;
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
        buffer->error = PS_BUFFER_ERROR_NONE;
        return true;
    }
    buffer->line_starts = calloc(buffer->line_count, sizeof(char *));
    if (buffer->line_starts == NULL)
    {
        buffer->error = PS_BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    }
    buffer->line_lengths = calloc(buffer->line_count, sizeof(uint8_t));
    if (buffer->line_lengths == NULL)
    {
        free(buffer->line_starts);
        buffer->line_starts = NULL;
        buffer->error = PS_BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    }
    // Find and memorize line starts
    // should work for Macintosh CR only files and DOS/Windows/Internet CR+LF files.
    text = buffer->text;
    int line_length;
    bool stop = false;
    while (!stop)
    {
        current_char = *text;
        switch (current_char)
        {
        case '\r':
        case '\n':
            line_length = text - start;
            if (line_length > PS_BUFFER_MAX_COLUMNS)
            {
                free(buffer->line_starts);
                buffer->line_starts = NULL;
                free(buffer->line_lengths);
                buffer->line_lengths = NULL;
                buffer->error = PS_BUFFER_ERROR_OVERFLOW_COLUMNS;
                return false;
            }
            buffer->line_starts[line] = start;
            buffer->line_lengths[line] = line_length;
            line += 1;
            column = 0;
            text += 1;
            // Skip LF if CR encountered
            if (current_char == '\r' && *text == '\n')
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
    buffer->error = PS_BUFFER_ERROR_NONE;
    return true;
}

bool ps_buffer_load_file(ps_buffer *buffer, char *filename)
{
    int result;
    size_t length;
    // Open file
    FILE *input = fopen(filename, "r");
    if (input == NULL)
    {
        buffer->error = PS_BUFFER_ERROR_OPENING_FILE;
        buffer->file_errno = errno;
        return false;
    }
    // Check length of file
    if (fseek(input, 0, SEEK_END))
    {
        buffer->error = PS_BUFFER_ERROR_READING_FILE;
        buffer->file_errno = errno;
        fclose(input);
        return false;
    }
    length = ftell(input);
    if (length > PS_BUFFER_MAX_SIZE)
    {
        buffer->error = PS_BUFFER_ERROR_OVERFLOW;
        fclose(input);
        return false;
    }
    if (fseek(input, 0, 0))
    {
        buffer->file_errno = errno;
        buffer->error = PS_BUFFER_ERROR_READING_FILE;
        fclose(input);
        return false;
    }
    // Read file
    result = ps_readall(input, &(buffer->text), &length);
    buffer->file_errno = errno;
    fclose(input);
    switch (result)
    {
    case PS_READALL_OK:
        buffer->from_file = true;
        buffer->length = (uint16_t)length;
        buffer->error = PS_BUFFER_ERROR_NONE;
        return ps_buffer_scan_text(buffer);
    case PS_READALL_NOMEM:
        buffer->error = PS_BUFFER_ERROR_OUT_OF_MEMORY;
        return false;
    // case PS_READALL_ERROR:
    //     buffer->error = PS_BUFFER_ERROR_READING_FILE;
    //     return false;
    default:
        buffer->error = PS_BUFFER_ERROR_READING_FILE;
        return false;
    }
}

bool ps_buffer_load_string(ps_buffer *buffer, char *text, size_t length)
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
    buffer->from_file = false;
    return ps_buffer_scan_text(buffer);
}

void ps_buffer_dump(ps_buffer *buffer, uint16_t from_line, uint16_t page_size)
{
    char line[PS_BUFFER_MAX_COLUMNS + 1];

    fprintf(stderr, "\n");
    // fprintf(stderr, "%d => %d for %d lines\n", buffer->line_count, from_line, page_size);
    if (buffer->line_count == 0)
    {
        fprintf(stderr, "Buffer is EMPTY!\n");
        return;
    }
    fprintf(stderr, "            |         1         2         3         4         5         6         7         8|\n");
    fprintf(stderr, "Line  (Len) |12345678901234567890123456789012345678901234567890123456789012345678901234567890|\n");
    for (int line_number = from_line; line_number < from_line + page_size - 1; line_number += 1)
    {
        if (line_number >= buffer->line_count)
            break;
        strncpy(line, buffer->line_starts[line_number], buffer->line_lengths[line_number]);
        line[buffer->line_lengths[line_number]] = '\0';
        fprintf(stderr, "%05d (%03d) |%-80s|\n", line_number + 1, buffer->line_lengths[line_number], line);
    }
    // fprintf(stderr, "\n");
}

char ps_buffer_peek_char(ps_buffer *buffer)
{
    return buffer->error == PS_BUFFER_ERROR_NONE ? buffer->current_char : '\0';
}

char ps_buffer_peek_next_char(ps_buffer *buffer)
{
    return buffer->error == PS_BUFFER_ERROR_NONE ? buffer->next_char : '\0';
}

bool ps_buffer_read_next_char(ps_buffer *buffer)
{
    // ps_buffer_debug(buffer, "ps_buffer_read_next_char: BEGIN");
    buffer->current_char = '\0';
    buffer->next_char = '\0';
    // already at end of buffer?
    if (buffer->error == PS_BUFFER_ERROR_EOF)
        return false;
    // already after end of buffer?
    if (buffer->current_line >= buffer->line_count)
    {
        buffer->error = PS_BUFFER_ERROR_EOF;
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
        if (buffer->line_lengths[buffer->current_line] == 0)
            buffer->next_char = '\0';
        else if (buffer->current_line < buffer->line_count)
            buffer->next_char = buffer->line_starts[buffer->current_line][buffer->current_column];
        else
            buffer->next_char = '\0';
    }
    // ps_buffer_debug(buffer, "ps_buffer_read_next_char: END  ");
    return true;
}

/* EOF */

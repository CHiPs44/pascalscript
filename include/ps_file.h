/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_FILE_H
#define _PS_FILE_H

#include "ps_config.h"
#include "ps_type_definition.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief  */
    typedef struct s_ps_file
    {
        FILE *f;                  /** @brief "C" file pointer */
        ps_string *filename;      /** @brief File name */
        ps_type_definition *type; /** @brief Type definition of the file : File, Text, File Of Type */
        ps_unsigned record_size;  /** @brief Record size for binary files */
        bool is_text;             /** @brief true if text file, false if binary */
        bool is_open;             /** @brief true if file is open */

    } __attribute__((__packed__)) ps_file;

    /** @brief Allocate new file with specified type */
    /** @returns New ps_file struct */
    ps_file *ps_file_alloc(ps_type_definition *type);
    /** @brief Free existing file */
    /** @returns NULL */
    ps_file *ps_file_free(ps_file *file);

    /** @brief Assign file name to file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/assign.html */
    bool ps_file_assign(ps_file *file, ps_string *filename);
    /** @brief Reads block(s) from file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/blockread.html */
    bool ps_file_blockread(ps_file *file, ps_pointer buffer, ps_unsigned count, ps_unsigned *result);
    /** @brief Writes block(s) to file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/blockwrite.html */
    bool ps_file_blockwrite(ps_file *file, ps_pointer buffer, ps_unsigned count, ps_unsigned *result);
    /** @brief Close file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/close.html */
    bool ps_file_close(ps_file *file);
    /** @brief Checks if text file is at end of file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/eof.html */
    bool ps_file_eof(ps_file *file, ps_boolean *result);
    /** @brief Checks if text file is at end of line */
    /** @see https://www.freepascal.org/docs-html/rtl/system/eoln.html */
    bool ps_file_eoln(ps_file *file, ps_boolean *result);
    /** @brief Delete file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/erase.html */
    bool ps_file_erase(ps_file *file);
    /** @brief Get file position */
    /** @see https://www.freepascal.org/docs-html/rtl/system/filepos.html */
    bool ps_file_filepos(ps_file *file, ps_unsigned *result);
    /** @brief Get file size */
    /** @see https://www.freepascal.org/docs-html/rtl/system/filesize.html */
    bool ps_file_filesize(ps_file *file, ps_unsigned *result);
    /** @brief Flush file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/flush.html */
    bool ps_file_flush(ps_file *file);
    /** @brief Read data from text file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/read.html */
    bool ps_file_read(ps_file *file, uint8_t count, ps_value *args);
    /** @brief Read data from text file, then skip end of line */
    /** @see https://www.freepascal.org/docs-html/rtl/system/readln.html */
    bool ps_file_readln(ps_file *file, uint8_t count, ps_value *args);
    /** @brief Rename assigned file to new name */
    /** @see https://www.freepascal.org/docs-html/rtl/system/rename.html */
    bool ps_file_rename(ps_file *file, ps_string *new_name);
    /** @brief Open file for reading */
    /** @see https://www.freepascal.org/docs-html/rtl/system/reset.html */
    bool ps_file_reset(ps_file *file, ps_unsigned record_size);
    /** @brief Open file for writing */
    /** @see https://www.freepascal.org/docs-html/rtl/system/rewrite.html */
    bool ps_file_rewrite(ps_file *file, ps_unsigned record_size);
    /** @brief Seek position into binary file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/seek.html */
    bool ps_file_seek(ps_file *file, ps_unsigned position);
    /** @brief Seek end of text file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/seekeof.html */
    bool ps_file_seekeof(ps_file *file);
    /** @brief Seek end of line in text file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/seekeoln.html */
    bool ps_file_seekeoln(ps_file *file);
    /** @brief Truncate file at its current position */
    /** @see https://www.freepascal.org/docs-html/rtl/system/truncate.html */
    bool ps_file_truncate(ps_file *file);
    /** @brief Write data to text file */
    /** @see https://www.freepascal.org/docs-html/rtl/system/write.html */
    bool ps_file_write(ps_file *file, uint8_t count, ps_value *args, ps_unsigned *num_chars, ps_unsigned *decimals);
    /** @brief Write data to text file, then output end of line */
    /** @see https://www.freepascal.org/docs-html/rtl/system/writeln.html */
    bool ps_file_writeln(ps_file *file, uint8_t count, ps_value *args, ps_unsigned *num_chars, ps_unsigned *decimals);

#define PS_FILE_SIZE sizeof(ps_file)

#ifdef __cplusplus
}
#endif

#endif /* _PS_FILE_H */

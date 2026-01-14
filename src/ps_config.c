/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>

#include "ps_config.h"
#include "ps_symbol.h"
#include "ps_system_types.h"
#include "ps_value.h"
#include "ps_value_data.h"
#include "ps_version.h"

#define FMT_SIZE "zu"

#define STRING(__STRING__) #__STRING__

#define REPORT_FORMAT(__MACRO__, __FORMAT__) fprintf(output, "┃ %-36s ┃ %-37" __FORMAT__ " ┃\n", #__MACRO__, __MACRO__)

#define REPORT_STRING(__MACRO__) fprintf(output, "┃ %-36s ┃ %-37s ┃\n", #__MACRO__, STRING(__MACRO__))

#define REPORT_SECTION(__TITLE__)                                                                                      \
    fprintf(output, "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");             \
    fprintf(output, "┃ %-76s ┃\n", __TITLE__);                                                                         \
    fprintf(output, "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n")

#define PS_SIZE_T_SIZE sizeof(size_t)

void ps_config_report(FILE *output)
{
    if (output == NULL)
        output = stderr;
    //               1         2         3         4         5         6         7         8
    //      12345678901234567890123456789012345678901234567890123456789012345678901234567890
    //                 1         2         3         4         5         6         7
    //      ┃ 1234567890123456789012345678901234567890123456789012345678901234567890123456 ┃
    //      ┃ 123456789012345678901234567890123456 ┃ 1234567890123456789012345678901234567 ┃
    // fprintf(output, "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    // fprintf(output, "┃ KEY                                  ┃ VALUE                                 ┃\n");
    // fprintf(output, "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    // REPORT_SECTION("*** CONFIGURATION ***");
    fprintf(output, "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    fprintf(output, "┃ *** PASCALSCRIPT CONFIGURATION ***                                           ┃\n");
    fprintf(output, "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    REPORT_FORMAT(PS_VERSION, "s");
    // REPORT_FORMAT(PS_VERSION_MAJOR, "d");
    // REPORT_FORMAT(PS_VERSION_MINOR, "d");
    // REPORT_FORMAT(PS_VERSION_PATCH, "d");
    // REPORT_FORMAT(PS_VERSION_INDEX, "d");
    REPORT_FORMAT(PS_BITNESS, "d");
    REPORT_SECTION("*** INTEGER TYPE ***");
    REPORT_STRING(PS_INTEGER);
    REPORT_FORMAT(PS_INTEGER_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_INTEGER_MIN, PS_INTEGER_FMT_10);
    REPORT_FORMAT(PS_INTEGER_MIN, PS_INTEGER_FMT_16);
    REPORT_FORMAT(PS_INTEGER_MAX, PS_INTEGER_FMT_10);
    REPORT_FORMAT(PS_INTEGER_MAX, PS_INTEGER_FMT_16);
    REPORT_FORMAT(PS_INTEGER_FMT_10, "s");
    REPORT_FORMAT(PS_INTEGER_FMT_16, "s");
    REPORT_SECTION("*** UNSIGNED TYPE ***");
    REPORT_STRING(PS_UNSIGNED);
    REPORT_FORMAT(PS_UNSIGNED_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_UNSIGNED_MAX, PS_UNSIGNED_FMT_10);
    REPORT_FORMAT(PS_UNSIGNED_FMT_10, "s");
    REPORT_FORMAT(PS_UNSIGNED_FMT_16, "s");
    REPORT_SECTION("*** BOOLEAN TYPE ***");
    REPORT_STRING(PS_BOOLEAN);
    REPORT_FORMAT(PS_BOOLEAN_SIZE, FMT_SIZE);
    REPORT_SECTION("*** REAL TYPE ***");
    REPORT_STRING(PS_REAL);
    REPORT_FORMAT(PS_REAL_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_REAL_MIN, "G");
    REPORT_FORMAT(PS_REAL_MAX, "G");
    REPORT_FORMAT(PS_REAL_EPSILON, "G");
    REPORT_FORMAT(PS_REAL_FMT, "s");
    REPORT_SECTION("*** CHARACTER TYPE ***");
    REPORT_STRING(PS_CHAR);
    REPORT_FORMAT(PS_CHAR_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_CHAR_MAX, "u");
    REPORT_SECTION("*** STRING ***");
    REPORT_STRING(PS_STRING_LEN_TYPE);
    REPORT_FORMAT(PS_STRING_MAX_LEN, "u");
    REPORT_FORMAT(PS_STRING_SIZE, FMT_SIZE);
    // REPORT_STRING(PS_STRING_REF_TYPE);
    REPORT_SECTION("*** IDENTIFIER ***");
    REPORT_FORMAT(PS_IDENTIFIER_LEN, "u");
    REPORT_FORMAT(PS_IDENTIFIER_SIZE, FMT_SIZE);
    REPORT_SECTION("*** SIZES ***");
    // REPORT_FORMAT(PS_FILE_SIZE, FMT_SIZE);
    // REPORT_FORMAT(PS_POINTER_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_VALUE_DATA_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_VALUE_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_SYMBOL_SIZE, FMT_SIZE);
    REPORT_FORMAT(PS_SIZE_T_SIZE, FMT_SIZE);
    fprintf(output, "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}

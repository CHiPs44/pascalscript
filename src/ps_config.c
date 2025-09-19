
/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>

#include "ps_config.h"
#include "ps_system_types.h"
#include "ps_version.h"

#define STRING(Z) #Z

#define REPORT_FORMAT(__MACRO__, __FORMAT__) \
    printf("┃ %-36s ┃ %-37" __FORMAT__ " ┃\n", #__MACRO__, __MACRO__)

#define REPORT_STRING(__MACRO__) \
    printf("┃ %-36s ┃ %-37s ┃\n", #__MACRO__, STRING(__MACRO__))

#define REPORT_SECTION(__TITLE__) \
    printf("┃ %-36s ┃ %-37s ┃\n", __TITLE__, "")

void ps_config_report()
{
    //               1         2         3         4         5         6         7         8
    //      12345678901234567890123456789012345678901234567890123456789012345678901234567890
    //      ┃ 123456789012345678901234567890123456 ┃ 1234567890123456789012345678901234567 ┃
    printf("┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
    printf("┃ KEY                                  ┃ VALUE                                 ┃\n");
    printf("┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
    REPORT_SECTION("*** Configuration");
    REPORT_FORMAT(PS_VERSION, "s");
    REPORT_FORMAT(PS_BITNESS, "d");
    REPORT_SECTION("*** Integer type");
    REPORT_STRING(PS_INTEGER);
    REPORT_FORMAT(PS_INTEGER_MIN, PS_INTEGER_FMT_10);
    REPORT_FORMAT(PS_INTEGER_MAX, PS_INTEGER_FMT_10);
    REPORT_FORMAT(PS_INTEGER_FMT_10, "s");
    REPORT_FORMAT(PS_INTEGER_FMT_16, "s");
    REPORT_SECTION("*** Unsigned type");
    REPORT_STRING(PS_UNSIGNED);
    REPORT_FORMAT(PS_UNSIGNED_MAX, PS_UNSIGNED_FMT_10);
    REPORT_FORMAT(PS_UNSIGNED_FMT_10, "s");
    REPORT_FORMAT(PS_UNSIGNED_FMT_16, "s");
    REPORT_SECTION("*** Boolean type");
    REPORT_STRING(PS_BOOLEAN);
    REPORT_SECTION("*** Real type");
    REPORT_STRING(PS_REAL);
    REPORT_FORMAT(PS_REAL_MIN, PS_REAL_FMT);
    REPORT_FORMAT(PS_REAL_MAX, PS_REAL_FMT);
    REPORT_FORMAT(PS_REAL_EPSILON, PS_REAL_FMT);
    REPORT_FORMAT(PS_REAL_FMT, "s");
    REPORT_SECTION("*** Character type");
    REPORT_STRING(PS_CHAR);
    REPORT_FORMAT(PS_CHAR_MAX, "u");
    REPORT_SECTION("*** String type");
    REPORT_STRING(PS_STRING_LEN_TYPE);
    REPORT_FORMAT(PS_STRING_MAX_LEN, "u");
    REPORT_STRING(PS_STRING_REF_TYPE);
    REPORT_SECTION("*** Identifier type");
    REPORT_FORMAT(PS_IDENTIFIER_LEN, "u");
    REPORT_FORMAT(PS_IDENTIFIER_SIZE, "u");
    REPORT_SECTION("*** Sizes");
    REPORT_FORMAT(PS_INTEGER_SIZE, "u");
    REPORT_FORMAT(PS_UNSIGNED_SIZE, "u");
    REPORT_FORMAT(PS_REAL_SIZE, "u");
    REPORT_FORMAT(PS_BOOLEAN_SIZE, "u");
    REPORT_FORMAT(PS_CHAR_SIZE, "u");
    REPORT_FORMAT(PS_STRING_SIZE, "u");
    REPORT_FORMAT(PS_FILE_SIZE, "u");
    REPORT_FORMAT(PS_POINTER_SIZE, "u");
    printf("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
}

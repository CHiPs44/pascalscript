/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_memory.h"
#include "ps_string.h"
#include "ps_tools.h"

int main(void)
{
    printf("TEST STRINGS: BEGIN\n\n");

    bool ok = false;
    bool failed = false;

    printf("TEST STRINGS: ALLOC\n");
    ps_string *s = ps_string_alloc(PS_STRING_MAX_LEN);
    ps_string_debug(stdout, "s  ", s);
    ok = s != NULL && s->max == PS_STRING_MAX_LEN;
    printf("TEST STRINGS: ALLOC %s\n\n", ok ? "OK" : "FAILED!");
    failed = failed || !ok;

    printf("TEST STRINGS: SET\n");
    //                    12345678901234567890123456789012345678901234
    ps_string_set(s, 44, "The Quick Brown Fox Jumps Over The Lazy Dog!");
    ps_string_debug(stdout, "s  ", s);
    ok = s != NULL && s->len == 44;
    printf("TEST STRINGS: SET %s\n\n", ok ? "OK" : "FAILED!");
    failed = failed || !ok;

    printf("TEST STRINGS: CONCAT\n");
    //                                123456789012345678901234s
    ps_string *s1 = ps_string_create("The Quick Brown Fox ", 30);
    ps_string_debug(stdout, "s1 ", s1);
    ps_string *s2 = ps_string_create("Jumps Over The Lazy Dog!", 30);
    ps_string_debug(stdout, "s2 ", s2);
    ps_string *s3 = ps_string_concat(s1, s2, 60);
    ps_string_debug(stdout, "s3 ", s3);
    ok = s3 != NULL && s3->len == 44;
    printf("TEST STRINGS: CONCAT %s\n\n", ok ? "OK" : "FAILED!");
    failed = failed || !ok;

    printf("TEST STRINGS: SUBSTRING\n");
    ps_string *s4 = ps_string_copy(s, 11, 5);
    ps_string_debug(stdout, "s4 ", s4);
    ok = s4 != NULL && 0 == strcmp((char *)s4->str, "Brown");
    printf("TEST STRINGS: SUBSTRING %s\n\n", ok ? "OK" : "FAILED!");
    failed = failed || !ok;

    printf("TEST STRINGS: COMPARE DIFFERENT\n");
    int test = ps_string_compare(s1, s2);
    ps_string_debug(stdout, "s1 ", s1);
    ps_string_debug(stdout, "s2 ", s2);
    printf("TEST STRINGS: COMPARE s1 vs s2: %d\n", test);
    ok = test > 0;
    printf("TEST STRINGS: COMPARE %s (%d)\n\n", ok ? "OK" : "FAILED!", test);
    failed = failed || !ok;

    printf("TEST STRINGS: COMPARE SAME\n");
    test = ps_string_compare(s1, s1);
    ps_string_debug(stdout, "s1 ", s1);
    ps_string_debug(stdout, "s1 ", s1);
    printf("TEST STRINGS: COMPARE s1 vs s1: %d\n", test);
    ok = test == 0;
    printf("TEST STRINGS: COMPARE %s (%d)\n\n", ok ? "OK" : "FAILED!", test);
    failed = failed || !ok;

    if (failed)
        printf("TEST STRINGS: FAILED!\n");
    else
        printf("TEST STRINGS: END\n");

    ps_string_free(s4);
    ps_string_free(s3);
    ps_string_free(s2);
    ps_string_free(s1);
    ps_string_free(s);
    return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}

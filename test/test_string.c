/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
*/

#include <stdio.h>
#include <sys/resource.h>

#include "../include/ps_string.h"

#include "../src/ps_string.c"

int main(void)
{
    struct rlimit rl = {256 * 1024 * 12, 256 * 1024 * 12};
    setrlimit(RLIMIT_AS, &rl);

    printf("TEST STRINGS: BEGIN\n\n");

    bool ok;

    printf("TEST STRINGS: ALLOC\n");
    ps_string *s = ps_string_new(ps_string_max);
    ps_string_debug(s, "s  ");
    ok = s != NULL && s->max == ps_string_max;
    printf("TEST STRINGS: ALLOC %s\n\n", ok ? "OK" : "KO");

    printf("TEST STRINGS: SET\n");
    //                12345678901234567890123456789012345678901234
    ps_string_set(s, "The Quick Brown Fox Jumps Over The Lazy Dog!");
    ps_string_debug(s, "s ");
    ok = s != NULL && s->len == 44;
    printf("TEST STRINGS: SET %s\n\n", ok ? "OK" : "KO");

    printf("TEST STRINGS: CONCAT\n");
    //                                    123456789012345678901234s
    ps_string *s1 = ps_string_create(30, "The Quick Brown Fox ");
    ps_string_debug(s1, "s1 ");
    ps_string *s2 = ps_string_create(30, "Jumps Over The Lazy Dog!");
    ps_string_debug(s2, "s2 ");
    ps_string *s3 = ps_string_concat(s1, s2);
    ps_string_debug(s3, "s3 ");
    ok = s3 != NULL && s3->len == 44;
    printf("TEST STRINGS: CONCAT %s\n\n", ok ? "OK" : "KO");

    printf("TEST STRINGS: SUBSTRING\n");
    ps_string *s4 = ps_string_substring(s, 11, 5);
    ps_string_debug(s4, "s4 ");
    ok = s4 != NULL && 0 == strcmp(s4->str, "Brown");
    printf("TEST STRINGS: SUBSTRING %s\n\n", ok ? "OK" : "KO");

    printf("TEST STRINGS: COMPARE\n");
    int test = ps_string_compare(s1, s2);
    ps_string_debug(s1, "s1 ");
    ps_string_debug(s2, "s2 ");
    ok = test > 0;
    printf("TEST STRINGS: COMPARE %s (%d)\n\n", ok ? "OK" : "KO", test);

    printf("TEST STRINGS: END\n");
}

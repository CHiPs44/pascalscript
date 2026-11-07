/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2026 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ps_ast.h"
#include "ps_functions.h"
#include "ps_procedures.h"
#include "ps_string.h"
#include "ps_string_heap.h"
#include "ps_system.h"
#include "ps_type_definition.h"

static ps_ast_block *system_block;
static ps_string_heap *string_heap;
static ps_interpreter *interpreter;
static int failures;

#define CHECK(expression)                                                                                              \
    do                                                                                                                 \
    {                                                                                                                  \
        if (!(expression))                                                                                             \
        {                                                                                                              \
            fprintf(stderr, "FAILED! %s:%d: %s\n", __FILE__, __LINE__, #expression);                                   \
            failures++;                                                                                                \
        }                                                                                                              \
    } while (0)

#define CHECK_REAL(expression, expected) CHECK(fabs((double)(expression) - (double)(expected)) < 0.0001)

static ps_value integer_value(ps_integer value) { return (ps_value){.type = &ps_system_integer, .data.i = value}; }

static ps_value unsigned_value(ps_unsigned value) { return (ps_value){.type = &ps_system_unsigned, .data.u = value}; }

static ps_value real_value(ps_real value) { return (ps_value){.type = &ps_system_real, .data.r = value}; }

static ps_value boolean_value(ps_boolean value) { return (ps_value){.type = &ps_system_boolean, .data.b = value}; }

static ps_value char_value(ps_char value) { return (ps_value){.type = &ps_system_char, .data.c = value}; }

static void test_ps_functions_init(void)
{
    ps_ast_block *block = ps_ast_create_block(0, 0, NULL, PS_AST_PROGRAM, "FUNCTIONS", ps_symbol_table_alloc(64, 4));
    CHECK(block != NULL);
    if (block == NULL)
        return;
    CHECK(ps_functions_init(block));
    CHECK(ps_symbol_table_find(block->symbols, "ABS") == &ps_system_function_abs);
    CHECK(ps_symbol_table_find(block->symbols, "GETTICKCOUNT") == &ps_system_function_get_tick_count);
    ps_ast_free_block(block);
}

static void test_ps_function_exec_1arg(void)
{
    ps_value input = integer_value(-12);
    ps_value result = {0};
    CHECK(ps_function_exec_1arg(interpreter, &ps_system_function_abs, &input, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == 12);
    CHECK(ps_function_exec_1arg(interpreter, &ps_system_function_random, NULL, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r >= 0.0 && result.data.r < 1.0);
}

static void test_ps_function_exec_1arg_s(void)
{
    ps_value result = {0};
    CHECK(ps_function_exec_1arg_s(interpreter, &ps_system_function_low, &ps_system_integer, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == PS_INTEGER_MIN);
}

static void test_ps_function_exec_2args(void)
{
    ps_value base = real_value(2.0);
    ps_value exponent = real_value(3.0);
    ps_value result = {0};
    CHECK(ps_function_exec_2args(interpreter, &ps_system_function_power, &base, &exponent, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 8.0);
}

static void test_ps_function_abs(void)
{
    ps_value value = integer_value(-12);
    ps_value result = {0};
    CHECK(ps_function_abs(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == 12);
    value = unsigned_value(7);
    CHECK(ps_function_abs(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned && result.data.u == 7);
    value = real_value(-2.5);
    CHECK(ps_function_abs(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 2.5);
}

static void test_ps_function_arctan(void)
{
    ps_value value = real_value(1.0);
    ps_value result = {0};
    CHECK(ps_function_arctan(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real);
    CHECK_REAL(result.data.r, atan(1.0));
}

static void test_ps_function_cos(void)
{
    ps_value value = real_value(0.0);
    ps_value result = {0};
    CHECK(ps_function_cos(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 1.0);
}

static void test_ps_function_even(void)
{
    ps_value value = integer_value(-4);
    ps_value result = {0};
    CHECK(ps_function_even(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_boolean && result.data.b);
    value = integer_value(3);
    CHECK(ps_function_even(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(!result.data.b);
}

static void test_ps_function_exp(void)
{
    ps_value value = real_value(0.0);
    ps_value result = {0};
    CHECK(ps_function_exp(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 1.0);
    value = real_value(-1.0);
    CHECK(ps_function_exp(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK_REAL(result.data.r, exp(-1.0));
}

static void test_ps_function_frac(void)
{
    ps_value value = real_value(-3.25);
    ps_value result = {0};
    CHECK(ps_function_frac(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == -0.25);
    value = real_value(4.0);
    CHECK(ps_function_frac(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.data.r == 0.0);
}

static void test_ps_function_int(void)
{
    ps_value value = real_value(-3.75);
    ps_value result = {0};
    CHECK(ps_function_int(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == -3.0);
}

static void test_ps_function_ln(void)
{
    ps_value value = real_value(1.0);
    ps_value result = {0};
    CHECK(ps_function_ln(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 0.0);
    value = real_value(0.0);
    CHECK(ps_function_ln(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
}

static void test_ps_function_log10(void)
{
    ps_value value = real_value(100.0);
    ps_value result = {0};
    CHECK(ps_function_log10(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK_REAL(result.data.r, 2.0);
    value = real_value(-1.0);
    CHECK(ps_function_log10(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
}

static void test_ps_function_log2(void)
{
    ps_value value = real_value(8.0);
    ps_value result = {0};
    CHECK(ps_function_log2(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK_REAL(result.data.r, 3.0);
    value = real_value(0.0);
    CHECK(ps_function_log2(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
}

static void test_ps_function_logn(void)
{
    ps_value base = real_value(2.0);
    ps_value value = real_value(8.0);
    ps_value result = {0};
    CHECK(ps_function_logn(interpreter, &base, &value, &result) == PS_ERROR_NONE);
    CHECK_REAL(result.data.r, 3.0);
    base = real_value(1.0);
    CHECK(ps_function_logn(interpreter, &base, &value, &result) == PS_ERROR_OUT_OF_RANGE);
}

static void test_ps_function_odd(void)
{
    ps_value value = integer_value(-3);
    ps_value result = {0};
    CHECK(ps_function_odd(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_boolean && result.data.b);
    value = integer_value(4);
    CHECK(ps_function_odd(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(!result.data.b);
}

static void test_ps_function_random(void)
{
    ps_value limit = integer_value(5);
    ps_value result = {0};
    ps_value seed = integer_value(1);
    CHECK(ps_procedure_randomize(interpreter, &seed));
    CHECK(ps_function_random(interpreter, &limit, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i >= 0 && result.data.i < 5);
    limit = integer_value(1);
    CHECK(ps_function_random(interpreter, &limit, &result) == PS_ERROR_NONE);
    CHECK(result.data.i == 0);
    limit = unsigned_value(3);
    CHECK(ps_function_random(interpreter, &limit, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned && result.data.u < 3);
    CHECK(ps_function_random(interpreter, NULL, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r >= 0.0 && result.data.r < 1.0);
}

static void test_ps_function_round(void)
{
    ps_value value = real_value(2.5);
    ps_value result = {0};
    CHECK(ps_function_round(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == 3);
    value = real_value(-2.5);
    CHECK(ps_function_round(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.data.i == -3);
}

static void test_ps_function_sin(void)
{
    ps_value value = real_value(0.0);
    ps_value result = {0};
    CHECK(ps_function_sin(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 0.0);
}

static void test_ps_function_sqr(void)
{
    ps_value value = real_value(-3.0);
    ps_value result = {0};
    CHECK(ps_function_sqr(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 9.0);
}

static void test_ps_function_sqrt(void)
{
    ps_value value = real_value(9.0);
    ps_value result = {0};
    CHECK(ps_function_sqrt(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 3.0);
    value = real_value(-1.0);
    CHECK(ps_function_sqrt(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
}

static void test_ps_function_tan(void)
{
    ps_value value = real_value((ps_real)(PS_REAL_PI / 4.0));
    ps_value result = {0};
    CHECK(ps_function_tan(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK_REAL(result.data.r, 1.0);
}

static void test_ps_function_trunc(void)
{
    ps_value value = real_value(-3.75);
    ps_value result = {0};
    CHECK(ps_function_trunc(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == -3);
}

static void test_ps_function_power(void)
{
    ps_value base = real_value(2.0);
    ps_value exponent = real_value(3.0);
    ps_value result = {0};
    CHECK(ps_function_power(interpreter, &base, &exponent, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_real && result.data.r == 8.0);
    exponent = real_value(0.0);
    CHECK(ps_function_power(interpreter, &base, &exponent, &result) == PS_ERROR_NONE);
    CHECK(result.data.r == 1.0);
}

static void test_ps_function_chr(void)
{
    ps_value value = integer_value(65);
    ps_value result = {0};
    CHECK(ps_function_chr(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_char && result.data.c == 'A');
    value = integer_value(256);
    CHECK(ps_function_chr(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
}

static void test_ps_function_high(void)
{
    ps_value result = {0};
    CHECK(ps_function_high(interpreter, &ps_system_integer, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == PS_INTEGER_MAX);
    CHECK(ps_function_high(interpreter, &ps_system_unsigned, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned && result.data.u == PS_UNSIGNED_MAX);
    CHECK(ps_function_high(interpreter, &ps_system_char, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_char && result.data.c == PS_CHAR_MAX);
}

static void test_ps_function_low(void)
{
    ps_value result = {0};
    CHECK(ps_function_low(interpreter, &ps_system_integer, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == PS_INTEGER_MIN);
    CHECK(ps_function_low(interpreter, &ps_system_unsigned, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned && result.data.u == 0);
    CHECK(ps_function_low(interpreter, &ps_system_char, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_char && result.data.c == 0);
}

static void test_ps_function_ord(void)
{
    ps_value value = char_value('A');
    ps_value result = {0};
    CHECK(ps_function_ord(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned && result.data.u == 'A');
    value = boolean_value(true);
    CHECK(ps_function_ord(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.data.u == 1);
    value = boolean_value(false);
    CHECK(ps_function_ord(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.data.u == 0);
}

static void test_ps_function_pred(void)
{
    ps_value value = integer_value(10);
    ps_value result = {0};
    CHECK(ps_function_pred(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == 9);
    value = unsigned_value(0);
    CHECK(ps_function_pred(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
    value = boolean_value(true);
    CHECK(ps_function_pred(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_boolean && !result.data.b);
}

static void test_ps_function_succ(void)
{
    ps_value value = integer_value(-1);
    ps_value result = {0};
    CHECK(ps_function_succ(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_integer && result.data.i == 0);
    value = integer_value(PS_INTEGER_MAX);
    CHECK(ps_function_succ(interpreter, &value, &result) == PS_ERROR_OUT_OF_RANGE);
    value = boolean_value(false);
    CHECK(ps_function_succ(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_boolean && result.data.b);
}

static void test_ps_function_length(void)
{
    ps_string *text = ps_string_create("Hello", 5);
    CHECK(text != NULL);
    if (text == NULL)
        return;
    ps_value value = {.type = &ps_system_string, .data.s = text};
    ps_value result = {0};
    CHECK(ps_function_length(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned && result.data.u == 5);
    value = char_value('x');
    CHECK(ps_function_length(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.data.u == 1);
    ps_string_free(text);
}

static void test_ps_function_lowercase(void)
{
    ps_string *text = ps_string_create("HeLLo", 5);
    CHECK(text != NULL);
    if (text == NULL)
        return;
    ps_value value = {.type = &ps_system_string, .data.s = text};
    ps_value result = {0};
    CHECK(ps_function_lowercase(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_string && strcmp((char *)result.data.s->str, "hello") == 0);
    value = char_value('Q');
    CHECK(ps_function_lowercase(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_char && result.data.c == 'q');
    ps_string_free(text);
}

static void test_ps_function_uppercase(void)
{
    ps_string *text = ps_string_create("HeLLo", 5);
    CHECK(text != NULL);
    if (text == NULL)
        return;
    ps_value value = {.type = &ps_system_string, .data.s = text};
    ps_value result = {0};
    CHECK(ps_function_uppercase(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_string && strcmp((char *)result.data.s->str, "HELLO") == 0);
    value = char_value('q');
    CHECK(ps_function_uppercase(interpreter, &value, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_char && result.data.c == 'Q');
    ps_string_free(text);
}

static void test_ps_function_get_tick_count(void)
{
    ps_value result = {0};
    CHECK(ps_function_get_tick_count(interpreter, NULL, &result) == PS_ERROR_NONE);
    CHECK(result.type == &ps_system_unsigned);
}

static void test_ps_procedures_init(void)
{
    ps_ast_block *block = ps_ast_create_block(0, 0, NULL, PS_AST_PROGRAM, "PROCEDURES", ps_symbol_table_alloc(32, 4));
    CHECK(block != NULL);
    if (block == NULL)
        return;
    CHECK(ps_procedures_init(block));
    CHECK(ps_symbol_table_find(block->symbols, "WRITELN") == &ps_system_procedure_writeln);
    ps_ast_free_block(block);
}

static void test_ps_procedure_dec(void)
{
    ps_symbol variable = {.name = "VALUE", .kind = PS_SYMBOL_KIND_VARIABLE};
    CHECK(!ps_procedure_dec(interpreter, &variable));
    CHECK(interpreter->error == PS_ERROR_NOT_IMPLEMENTED);
}

static void test_ps_procedure_inc(void)
{
    ps_symbol variable = {.name = "VALUE", .kind = PS_SYMBOL_KIND_VARIABLE};
    CHECK(!ps_procedure_inc(interpreter, &variable));
    CHECK(interpreter->error == PS_ERROR_NOT_IMPLEMENTED);
}

static void test_ps_procedure_randomize(void)
{
    ps_value seed = integer_value(42);
    CHECK(ps_procedure_randomize(interpreter, &seed));
    int expected = rand(); // NOSONAR
    CHECK(ps_procedure_randomize(interpreter, &seed));
    CHECK(rand() == expected);
    seed = real_value(1.0);
    CHECK(!ps_procedure_randomize(interpreter, &seed));
    CHECK(interpreter->error == PS_ERROR_UNEXPECTED_TYPE);
    CHECK(ps_procedure_randomize(interpreter, NULL));
}

static void test_ps_procedure_read(void)
{
    ps_value value = integer_value(0);
    FILE *input = tmpfile();
    CHECK(input != NULL);
    if (input != NULL)
    {
        CHECK(!ps_procedure_read(interpreter, input, &value));
        CHECK(interpreter->error == PS_ERROR_NOT_IMPLEMENTED);
        fclose(input);
    }
}

static void test_ps_procedure_readln(void)
{
    ps_value value = integer_value(0);
    FILE *input = tmpfile();
    CHECK(input != NULL);
    if (input != NULL)
    {
        CHECK(!ps_procedure_readln(interpreter, input, &value));
        CHECK(interpreter->error == PS_ERROR_NOT_IMPLEMENTED);
        fclose(input);
    }
}

static void test_ps_procedure_readstr(void)
{
    ps_string *input = ps_string_create("123", 3);
    CHECK(input != NULL);
    if (input == NULL)
        return;
    ps_value value = integer_value(0);
    CHECK(!ps_procedure_readstr(interpreter, input, &value));
    CHECK(interpreter->error == PS_ERROR_NOT_IMPLEMENTED);
    ps_string_free(input);
}

static void test_ps_procedure_write(void)
{
    ps_value value = integer_value(123);
    FILE *output = tmpfile();
    CHECK(output != NULL);
    if (output == NULL)
        return;
    CHECK(ps_procedure_write(interpreter, output, &value, 0, 0));
    rewind(output);
    char buffer[16] = {0};
    CHECK(fread(buffer, 1, sizeof(buffer) - 1, output) == 3);
    CHECK(strcmp(buffer, "123") == 0);
    fclose(output);
}

static void test_ps_procedure_writeln(void)
{
    ps_value value = integer_value(7);
    FILE *output = tmpfile();
    CHECK(output != NULL);
    if (output == NULL)
        return;
    CHECK(ps_procedure_writeln(interpreter, output, &value, 3, 0));
    rewind(output);
    char buffer[16] = {0};
    CHECK(fread(buffer, 1, sizeof(buffer) - 1, output) == 3);
    CHECK(strcmp(buffer, "  7") == 0);
    fclose(output);
}

static void test_ps_procedure_writestr(void)
{
    ps_string *buffer = ps_string_create("Value=", PS_STRING_MAX_LEN);
    CHECK(buffer != NULL);
    if (buffer == NULL)
        return;
    ps_value value = integer_value(12);
    CHECK(ps_procedure_writestr(interpreter, buffer, &value, 0, 0));
    CHECK(strcmp((char *)buffer->str, "Value=12") == 0);
    ps_string_free(buffer);
}

#define RUN_TEST(test)                                                                                                 \
    do                                                                                                                 \
    {                                                                                                                  \
        printf("TEST SYSTEM LIB: %s\n", #test);                                                                        \
        test();                                                                                                        \
    } while (0)

int main(void)
{
    system_block = ps_system_alloc();
    string_heap = ps_string_heap_alloc(8, 4);
    CHECK(system_block != NULL && string_heap != NULL);
    if (system_block == NULL || string_heap == NULL)
        goto cleanup;
    interpreter = ps_interpreter_alloc(system_block, string_heap, true, false, false);
    CHECK(interpreter != NULL);
    if (interpreter == NULL)
        goto cleanup;

    RUN_TEST(test_ps_functions_init);
    RUN_TEST(test_ps_function_exec_1arg);
    RUN_TEST(test_ps_function_exec_1arg_s);
    RUN_TEST(test_ps_function_exec_2args);
    RUN_TEST(test_ps_function_abs);
    RUN_TEST(test_ps_function_arctan);
    RUN_TEST(test_ps_function_cos);
    RUN_TEST(test_ps_function_even);
    RUN_TEST(test_ps_function_exp);
    RUN_TEST(test_ps_function_frac);
    RUN_TEST(test_ps_function_int);
    RUN_TEST(test_ps_function_ln);
    RUN_TEST(test_ps_function_log10);
    RUN_TEST(test_ps_function_log2);
    RUN_TEST(test_ps_function_logn);
    RUN_TEST(test_ps_function_odd);
    RUN_TEST(test_ps_function_random);
    RUN_TEST(test_ps_function_round);
    RUN_TEST(test_ps_function_sin);
    RUN_TEST(test_ps_function_sqr);
    RUN_TEST(test_ps_function_sqrt);
    RUN_TEST(test_ps_function_tan);
    RUN_TEST(test_ps_function_trunc);
    RUN_TEST(test_ps_function_power);
    RUN_TEST(test_ps_function_chr);
    RUN_TEST(test_ps_function_high);
    RUN_TEST(test_ps_function_low);
    RUN_TEST(test_ps_function_ord);
    RUN_TEST(test_ps_function_pred);
    RUN_TEST(test_ps_function_succ);
    RUN_TEST(test_ps_function_length);
    RUN_TEST(test_ps_function_lowercase);
    RUN_TEST(test_ps_function_uppercase);
    RUN_TEST(test_ps_function_get_tick_count);
    RUN_TEST(test_ps_procedures_init);
    RUN_TEST(test_ps_procedure_dec);
    RUN_TEST(test_ps_procedure_inc);
    RUN_TEST(test_ps_procedure_randomize);
    RUN_TEST(test_ps_procedure_read);
    RUN_TEST(test_ps_procedure_readln);
    RUN_TEST(test_ps_procedure_readstr);
    RUN_TEST(test_ps_procedure_write);
    RUN_TEST(test_ps_procedure_writeln);
    RUN_TEST(test_ps_procedure_writestr);

cleanup:
    interpreter = ps_interpreter_free(interpreter);
    string_heap = ps_string_heap_free(string_heap);
    system_block = ps_system_free(system_block);
    printf("TEST SYSTEM LIB: %s\n", failures == 0 ? "END" : "FAILED!");
    return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

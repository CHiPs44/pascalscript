/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_TOKEN_H
#define _PS_TOKEN_H

#include "ps_config.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef MAX_IDENTIFIER
#define MAX_IDENTIFIER 31
#endif

    /*
        1. Mimimalistic
            program         begin           end
            const           var
            integer         UNSIGNED        real
            char            string
        2. Expressions
            div             mod
        3. Decision making
            boolean         true            false
            if              then            else
            and             not             or
        4. Loops
            repeat          until           while       do
        5. User types
            type            set
        6. Arrays
            array
        6. "Modularity"
            function        procedure
        7. More control
            case            of
            for             downto          to          in
        8. Mother of all evil?
            goto            label
        9. More operators
            shl             shr             xor

        -file           -packed         -record             -nil        -with
        -absolute       -asm            -inline             -operator   -reintroduce
        -unit           -interface      -implementation     -uses
        -constructor    -destructor     -inherited          -object     -self

        forward
    */

    /* THESE ARE NOT TOKENS
        Comments
            PS_TOKEN_LEFT_COMMENT,        (*
            PS_TOKEN_LEFT_CURLY_BRACKET,  {
            PS_TOKEN_RIGHT_COMMENT,       *)
            PS_TOKEN_RIGHT_CURLY_BRACKET, }
        Numerical base prefixes
            PS_TOKEN_AMPERSAND,         & => Octal
            PS_TOKEN_DOLLAR,            $ => Hexadecimal
            PS_TOKEN_PERCENT,           % => Binary
     */

    typedef enum _token_type_t
    {
        PS_TOKEN_NONE = 0,
        PS_TOKEN_END_OF_FILE,
        // Numeric values
        PS_TOKEN_INTEGER_VALUE,
        PS_TOKEN_UNSIGNED_VALUE,
        PS_TOKEN_REAL_VALUE,
        // Other value types
        PS_TOKEN_BOOLEAN_VALUE,
        PS_TOKEN_CHAR_VALUE,
        PS_TOKEN_STRING_VALUE,
        // Identifier
        PS_TOKEN_IDENTIFIER,
        // Reserved words
        PS_TOKEN_RESERVED_WORDS = (UINT8_MAX / 2) + 1,
        PS_TOKEN_PROGRAM = PS_TOKEN_RESERVED_WORDS,
        PS_TOKEN_CONST,
        PS_TOKEN_TYPE,
        PS_TOKEN_VAR,
        PS_TOKEN_FUNCTION,
        PS_TOKEN_PROCEDURE,
        PS_TOKEN_BEGIN,
        PS_TOKEN_END,
        PS_TOKEN_INTEGER,
        PS_TOKEN_UNSIGNED,
        PS_TOKEN_BOOLEAN,
        PS_TOKEN_CHAR,
        PS_TOKEN_REAL,
        PS_TOKEN_STRING,
        PS_TOKEN_NIL,
        PS_TOKEN_ARRAY,
        PS_TOKEN_OF,
        PS_TOKEN_SET,
        PS_TOKEN_RECORD,
        PS_TOKEN_WITH,
        PS_TOKEN_FILE,
        PS_TOKEN_IF,
        PS_TOKEN_THEN,
        PS_TOKEN_ELSE,
        PS_TOKEN_DO,
        PS_TOKEN_WHILE,
        PS_TOKEN_REPEAT,
        PS_TOKEN_UNTIL,
        PS_TOKEN_FOR,
        PS_TOKEN_TO,
        PS_TOKEN_DOWNTO,
        PS_TOKEN_CASE,
        PS_TOKEN_OTHERWISE,
        PS_TOKEN_GOTO,
        PS_TOKEN_LABEL,
        PS_TOKEN_UNIT,
        PS_TOKEN_USES,
        PS_TOKEN_INTERFACE,
        PS_TOKEN_IMPLEMENTATION,
        // Operators
        PS_TOKEN_DIV,
        PS_TOKEN_MOD,
        PS_TOKEN_AND,
        PS_TOKEN_OR,
        PS_TOKEN_XOR,
        PS_TOKEN_NOT,
        PS_TOKEN_SHL,
        PS_TOKEN_SHR,
        PS_TOKEN_IN,
        // PS_TOKEN_IS,
        // PS_TOKEN_AS,
        // Symbols
        PS_TOKEN_ASSIGN,            // :=  assign
        PS_TOKEN_AT_SIGN,           // @   address of
        PS_TOKEN_CARET,             // ^   pointer to
        PS_TOKEN_COLON,             // :   various uses
        PS_TOKEN_COMMA,             // ,   various uses
        PS_TOKEN_RANGE,             // ..  ranges
        PS_TOKEN_DOT,               // .   various uses
        PS_TOKEN_LEFT_BRACKET,      // [   array access
        PS_TOKEN_LEFT_PARENTHESIS,  // (   various uses
        PS_TOKEN_RIGHT_BRACKET,     // ]   array access
        PS_TOKEN_RIGHT_PARENTHESIS, // )   various uses
        PS_TOKEN_SEMI_COLON,        // ;   various uses
        // Arithmetic operators
        PS_TOKEN_PLUS,  // +   addition
        PS_TOKEN_MINUS, // -   substraction / negation
        PS_TOKEN_STAR,  // *   multiplication
        PS_TOKEN_SLASH, // /   division (real)
        PS_TOKEN_POWER, // **  exponentiation
        // Comparison operators
        PS_TOKEN_EQUAL,            // =
        PS_TOKEN_NOT_EQUAL,        // <>
        PS_TOKEN_LESS_THAN,        // <
        PS_TOKEN_LESS_OR_EQUAL,    // <=
        PS_TOKEN_GREATER_THAN,     // >
        PS_TOKEN_GREATER_OR_EQUAL, // >=
        // Make token value fit in one byte
        PS_TOKEN_MAX = UINT8_MAX
    } __attribute__((__packed__)) ps_token_type;

    typedef struct s_ps_token
    {
        ps_token_type type;
        union {
            char identifier[MAX_IDENTIFIER + 1];
            ps_integer i;
            ps_unsigned u;
            ps_real r;
            ps_char c;
            ps_boolean b;
            char s[PS_STRING_MAX_LEN + 1];
        } value;
    } ps_token;

#define PS_TOKEN_TYPE_SIZE sizeof(ps_token_type)
#define PS_TOKEN_SIZE sizeof(ps_token)

    void ps_token_debug(FILE *output, char *message, ps_token *token);
    char *ps_token_dump_value(ps_token *token);

    ps_token_type ps_token_is_keyword(char *identifier);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOKEN_H */

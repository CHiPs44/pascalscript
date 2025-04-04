/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: GPL-3.0-or-later
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
            TOKEN_LEFT_COMMENT,        (*
            TOKEN_LEFT_CURLY_BRACKET,  {
            TOKEN_RIGHT_COMMENT,       *)
            TOKEN_RIGHT_CURLY_BRACKET, }
        Numerical base prefixes
            TOKEN_AMPERSAND,         & => Octal
            TOKEN_DOLLAR,            $ => Hexadecimal
            TOKEN_PERCENT,           % => Binary
     */

    typedef enum _token_type_t
    {
        TOKEN_NONE = 0,
        TOKEN_END_OF_FILE,
        // Numeric values
        TOKEN_INTEGER_VALUE,
        TOKEN_UNSIGNED_VALUE,
        TOKEN_REAL_VALUE,
        // Other value types
        TOKEN_BOOLEAN_VALUE,
        TOKEN_CHAR_VALUE,
        TOKEN_STRING_VALUE,
        // Identifier
        TOKEN_IDENTIFIER,
        // Reserved words
        TOKEN_RESERVED_WORDS = (UINT8_MAX / 2) + 1,
        TOKEN_PROGRAM = TOKEN_RESERVED_WORDS,
        TOKEN_CONST,
        TOKEN_TYPE,
        TOKEN_VAR,
        TOKEN_FUNCTION,
        TOKEN_PROCEDURE,
        TOKEN_BEGIN,
        TOKEN_END,
        TOKEN_INTEGER,
        TOKEN_UNSIGNED,
        TOKEN_BOOLEAN,
        TOKEN_CHAR,
        TOKEN_REAL,
        TOKEN_STRING,
        TOKEN_FALSE,
        TOKEN_TRUE,
        TOKEN_NIL,
        TOKEN_ARRAY,
        TOKEN_OF,
        TOKEN_SET,
        TOKEN_RECORD,
        TOKEN_WITH,
        TOKEN_FILE,
        TOKEN_IF,
        TOKEN_THEN,
        TOKEN_ELSE,
        TOKEN_DO,
        TOKEN_WHILE,
        TOKEN_REPEAT,
        TOKEN_UNTIL,
        TOKEN_FOR,
        TOKEN_TO,
        TOKEN_DOWNTO,
        TOKEN_IN,
        TOKEN_CASE,
        TOKEN_GOTO,
        TOKEN_LABEL,
        TOKEN_UNIT,
        TOKEN_USES,
        TOKEN_INTERFACE,
        TOKEN_IMPLEMENTATION,
        // Operators
        TOKEN_DIV,
        TOKEN_MOD,
        TOKEN_AND,
        TOKEN_OR,
        TOKEN_XOR,
        TOKEN_NOT,
        TOKEN_SHL,
        TOKEN_SHR,
        // Symbols
        TOKEN_ASSIGN,            // :=  assign
        TOKEN_AT_SIGN,           // @   address of
        TOKEN_CARET,             // ^   pointer to
        TOKEN_COLON,             // :   various uses
        TOKEN_COMMA,             // ,   various uses
        TOKEN_RANGE,             // ..  ranges
        TOKEN_DOT,               // .   various uses
        TOKEN_LEFT_BRACKET,      // [   array access
        TOKEN_LEFT_PARENTHESIS,  // (   various uses
        TOKEN_RIGHT_BRACKET,     // ]   array access
        TOKEN_RIGHT_PARENTHESIS, // )   various uses
        TOKEN_SEMI_COLON,        // ;   various uses
        // Arithmetic operators
        TOKEN_PLUS,  // +   addition
        TOKEN_MINUS, // -   substraction / negation
        TOKEN_STAR,  // *   multiplication
        TOKEN_SLASH, // /   division (real)
        // Comparison operators
        TOKEN_EQUAL,            // =
        TOKEN_NOT_EQUAL,        // <>
        TOKEN_LESS_THAN,        // <
        TOKEN_LESS_OR_EQUAL,    // <=
        TOKEN_GREATER_THAN,     // >
        TOKEN_GREATER_OR_EQUAL, // >=
        // These should not be tokens
        TOKEN_WRITE,
        TOKEN_WRITELN,
        // Make token value fit in one byte
        TOKEN_MAX = UINT8_MAX
    } __attribute__((__packed__)) ps_token_type;

    typedef struct s_ps_token
    {
        ps_token_type type;
        union
        {
            char identifier[MAX_IDENTIFIER + 1];
            ps_integer i;
            ps_unsigned u;
            ps_real r;
            ps_char c;
            ps_boolean b;
            ps_char s[PS_STRING_MAX_LEN + 1];
        } value;
    } ps_token;

#define PS_TOKEN_TYPE_SIZE sizeof(ps_token_type)
#define PS_TOKEN_SIZE sizeof(ps_token)

    void ps_token_dump(ps_token *token);

    ps_token_type ps_token_is_keyword(char *text);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOKEN_H */

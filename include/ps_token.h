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
            integer         cardinal        string
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
        // Numeric values         size  mini / maxi
        //                       ----- ---------------------------------------------
        TOKEN_INTEGER_VALUE,  // 2 / 4 -32768..32767 / -2,147,483,648..2,147,483,647
        TOKEN_CARDINAL_VALUE, // 2 / 4 0..65,535 / 0..4,294,967,295
        TOKEN_REAL_VALUE,     // 4 / 8 +/- 1.5E-45 .. 3.4E38 / 5.0E-324..1.7E308
        // Other value types
        TOKEN_CHAR_VALUE,
        TOKEN_STRING_VALUE,
        // Identifier
        TOKEN_IDENTIFIER,
        // Reserved words
        // ==============================
        TOKEN_RESERVED_WORDS = 127,
        TOKEN_PROGRAM,
        TOKEN_CONST,
        TOKEN_TYPE,
        TOKEN_VAR,
        TOKEN_FUNCTION,
        TOKEN_PROCEDURE,
        TOKEN_BEGIN,
        TOKEN_END,
        TOKEN_INTEGER,
        TOKEN_CARDINAL,
        TOKEN_BOOLEAN,
        TOKEN_CHAR,
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
        TOKEN_OP_DIV_INT,
        TOKEN_OP_MOD,
        TOKEN_OP_AND,
        TOKEN_OP_OR,
        TOKEN_OP_XOR,
        TOKEN_OP_NOT,
        TOKEN_OP_SHL,
        TOKEN_OP_SHR,
        // ===========================================================
        // Symbols
        TOKEN_DOT_COLON,         // :=  assign
        TOKEN_AT_SIGN,           // @   address of
        TOKEN_CARET,             // ^   pointer to
        TOKEN_COLON,             // :   various uses
        TOKEN_COMMA,             // ,   various uses
        TOKEN_DOT_DOT,           // ..  ranges
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
    } token_type_t;

    typedef struct _token_t
    {
        token_type_t type;
        union
        {
            char identifier[MAX_IDENTIFIER + 1];
            ps_integer_t i;
            ps_unsigned_t u;
            ps_real_t r;
            ps_char_t c;
            ps_char_t s[PS_STRING_MAX + 1];
        } value;
    } token_t;

    typedef struct _keyword_t
    {
        int token;
        char *keyword;
    } keyword_t;

    void ps_token_dump(token_t *token);

    extern keyword_t keywords[];
    token_type_t ps_token_is_keyword(char *text);

#ifdef __cplusplus
}
#endif

#endif /* _PS_TOKEN_H */

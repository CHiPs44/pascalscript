#ifndef ERROR_H
#define ERROR_H

typedef enum _error_code_t {
    ERROR_NONE,
    /* lexer */
    ERROR_IDENTIFIER_TOO_LONG,
    ERROR_OVERFLOW,
    /* parser */
    ERROR_SYNTAX,
    ERROR_UNEXPECTED,
    ERROR_UNKOWN_IDENTIFIER,
    ERROR_CONSTANT_VALUE,
    /* ...*/
} error_code_t;

extern char *error_get_message(error_code_t code);

#endif 
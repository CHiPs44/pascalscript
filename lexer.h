#ifndef LEXER_H
#define LEXER_H

#define MAX_IDENTIFIER 31

typedef enum _token_type_t {
    IDENTIFIER,
    INT_VAL
} token_type_t;

typedef struct _token_t {
    token_type_t type;
    union {
        int int_val;
        char identifier[MAX_IDENTIFIER + 1];
    } value;
} token_t;

extern int copy_identifier(const char *buffer);

extern int copy_integer_value(const char *buffer);

#endif

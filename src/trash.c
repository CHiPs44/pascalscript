/**
 * @brief Copy current identifier into current token
 *
 * @param text
 * @return int ERROR_ZERO | LEXER_ERROR_IDENTIFIER_TOO_LONG
 */
error_t lexer_copy_identifier(char *text, token_t *token)
{
    char identifier[MAX_IDENTIFIER + 1];
    size_t length = strlen(text);
    if (length > MAX_IDENTIFIER)
    {
        return LEXER_ERROR_IDENTIFIER_TOO_LONG;
    }
    token->type = TOKEN_IDENTIFIER;
    strcpy(identifier, text);
    symbol_normalize_name(identifier);
    strcpy(token->value.identifier, identifier);
    fprintf(stderr, "\tlexer_copy_identifier: %s\n", token->value.identifier);
    return ERROR_ZERO;
}

/**
 * @brief Parse current integer value into current token
 *
 * @return error_t ERROR_ZERO | LEXER_ERROR_OVERFLOW
 */
error_t lexer_copy_integer_value(char *text, token_t *token)
{
    long val = strtoul(text, 0, 10);
    fprintf(stderr, " [lexer_copy_integer_value %s %ld %d %d]", text, val, errno, INT_MAX);
    if (errno == ERANGE || val > INT_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %ld", text, val);
        return LEXER_ERROR_OVERFLOW;
    }
    token->type = TOKEN_INTEGER_VALUE;
    token->value.i = (int)val;
    return ERROR_ZERO;
}

/**
 * @brief Parse current real value into current token
 *
 * @return error_t ERROR_ZERO | LEXER_ERROR_OVERFLOW
 */
error_t lexer_copy_real_value(char *text, token_t *token)
{
    PS_REAL val = strtod(text, NULL);
    fprintf(stderr, " [lexer_copy_real_value %s %f %d]", text, val, errno);
    if (errno == ERANGE)
    {
        fprintf(stderr, "LEXER_ERROR_OVERFLOW %s %f", text, val);
        return LEXER_ERROR_OVERFLOW;
    }
    token->type = TOKEN_REAL_VALUE;
    token->value.r = val;
    return ERROR_ZERO;
}

/**
 * @brief Parse current char value into current token
 *
 * @return error_t ERROR_ZERO
 */
error_t lexer_copy_char_value(char *text, token_t *token)
{
    // TODO? "'X'" or "''''"
    PS_CHAR val = text[1];
    // fprintf(stderr, " [lexer_copy_char_value %s %c]", text, val);
    token->type = TOKEN_CHAR_VALUE;
    token->value.c = val;
    return ERROR_ZERO;
}

/**
 * @brief Parse current string value into current token
 *
 * @return error_t ERROR_ZERO | LEXER_ERROR_STRING_TOO_LONG
 */
error_t lexer_copy_string_value(char *text, token_t *token)
{
    // TODO replace "''" with "'"
    size_t len = strlen(text) - 2;
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", len, text);
    // fprintf(stderr, " [lexer_copy_string_value]\n");
    if (len > PS_STRING_MAX)
    {
        fprintf(stderr, "LEXER_ERROR_STRING_TOO_LONG %ld |%s|", len, text);
        return LEXER_ERROR_STRING_TOO_LONG;
    }
    token->type = TOKEN_STRING_VALUE;
    if (len == 0)
        strcpy(token->value.s, "");
    else
        strncpy(token->value.s, &text[1], len);
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", strlen(token->value.s), token->value.s);
    return ERROR_ZERO;
}


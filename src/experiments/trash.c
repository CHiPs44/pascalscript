/**
 * @brief Copy current identifier into current token
 *
 * @param text
 * @return int PS_ERROR_ZERO | PS_LEXER_ERROR_IDENTIFIER_TOO_LONG
 */
ps_error lexer_copy_identifier(char *text, ps_token *token)
{
    char identifier[PS_IDENTIFIER_MAX + 1];
    size_t length = strlen(text);
    if (length > PS_IDENTIFIER_MAX)
    {
        return PS_LEXER_ERROR_IDENTIFIER_TOO_LONG;
    }
    token->type = PS_TOKEN_IDENTIFIER;
    strcpy(identifier, text);
    ps_symbol_normalize_name(identifier);
    strcpy(token->value.identifier, identifier);
    fprintf(stderr, "\tlexer_copy_identifier: %s\n", token->value.identifier);
    return PS_ERROR_ZERO;
}

/**
 * @brief Parse current integer value into current token
 *
 * @return ps_error PS_ERROR_ZERO | PS_LEXER_ERROR_OVERFLOW
 */
ps_error lexer_copy_integer_value(char *text, ps_token *token)
{
    long val = strtoul(text, 0, 10);
    fprintf(stderr, " [lexer_copy_integer_value %s %ld %d %d]", text, val, errno, INT_MAX);
    if (errno == ERANGE || val > INT_MAX)
    {
        fprintf(stderr, "PS_LEXER_ERROR_OVERFLOW %s %ld", text, val);
        return PS_LEXER_ERROR_OVERFLOW;
    }
    token->type = PS_TOKEN_INTEGER_VALUE;
    token->value.i = (int)val;
    return PS_ERROR_ZERO;
}

/**
 * @brief Parse current real value into current token
 *
 * @return ps_error PS_ERROR_ZERO | PS_LEXER_ERROR_OVERFLOW
 */
ps_error lexer_copy_real_value(char *text, ps_token *token)
{
    ps_real val = strtod(text, NULL);
    fprintf(stderr, " [lexer_copy_real_value %s %f %d]", text, val, errno);
    if (errno == ERANGE)
    {
        fprintf(stderr, "PS_LEXER_ERROR_OVERFLOW %s %f", text, val);
        return PS_LEXER_ERROR_OVERFLOW;
    }
    token->type = PS_TOKEN_REAL_VALUE;
    token->value.r = val;
    return PS_ERROR_ZERO;
}

/**
 * @brief Parse current char value into current token
 *
 * @return ps_error PS_ERROR_ZERO
 */
ps_error lexer_copy_char_value(char *text, ps_token *token)
{
    // TODO? "'X'" or "''''"
    ps_char val = text[1];
    // fprintf(stderr, " [lexer_copy_char_value %s %c]", text, val);
    token->type = PS_TOKEN_CHAR_VALUE;
    token->value.c = val;
    return PS_ERROR_ZERO;
}

/**
 * @brief Parse current string value into current token
 *
 * @return ps_error PS_ERROR_ZERO | PS_LEXER_ERROR_STRING_TOO_LONG
 */
ps_error lexer_copy_string_value(char *text, ps_token *token)
{
    // TODO replace "''" with "'"
    size_t len = strlen(text) - 2;
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", len, text);
    // fprintf(stderr, " [lexer_copy_string_value]\n");
    if (len > ps_string_max)
    {
        fprintf(stderr, "PS_LEXER_ERROR_STRING_TOO_LONG %ld |%s|", len, text);
        return PS_LEXER_ERROR_STRING_TOO_LONG;
    }
    token->type = PS_TOKEN_STRING_VALUE;
    if (len == 0)
        strcpy(token->value.s, "");
    else
        strncpy(token->value.s, &text[1], len);
    // fprintf(stderr, " [lexer_copy_string_value l=%ld s=|%s|]\n", strlen(token->value.s), token->value.s);
    return PS_ERROR_ZERO;
}


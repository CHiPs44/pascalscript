#include <string.h>

#include "ps_functions.h"
#include "ps_system.h"
#include "ps_parser.h"
// #include "ps_visitor.h"

// clang-format off
#define USE_LEXER                       ps_lexer *lexer = ps_parser_get_lexer(interpreter->parser)
#define SET_VISITOR(__VISITOR__)        static char *visitor = __VISITOR__
#define READ_NEXT_TOKEN                 { \
                                            if (!ps_lexer_read_next_token(lexer)) \
                                                return false; \
                                            if (interpreter->debug) \
                                            { \
                                                fprintf(stderr, "TOKEN\t%-32s %-8s ", "", ""); \
                                                ps_token_dump(&lexer->current_token); \
                                            } \
                                        }
#define EXPECT_TOKEN(__PS_TOKEN_TYPE__) if (!ps_parser_expect_token_type(interpreter->parser, __PS_TOKEN_TYPE__)) return false
#define RETURN_ERROR(__PS_ERROR__)      { \
                                            interpreter->error = __PS_ERROR__; \
                                            if (interpreter->debug) { \
                                                fprintf(stderr, "RETURN\t%-32s %-8d ", visitor, __PS_ERROR__); \
                                                ps_token_dump(&lexer->current_token); \
                                            } \
                                            return false; \
                                        }
#define COPY_IDENTIFIER(__IDENTIFIER__) strncpy(__IDENTIFIER__, lexer->current_token.value.identifier, PS_IDENTIFIER_LEN)
#define TRACE_BEGIN(__PLUS__)           if (interpreter->debug) { \
                                            fprintf(stderr, "BEGIN\t%-32s %-8s ", visitor, __PLUS__); \
                                            ps_token_dump(&lexer->current_token); \
                                        }
#define TRACE_END(__PLUS__)             if (interpreter->debug) { \
                                            fprintf(stderr, "END\t%-32s %-8s ", visitor, __PLUS__); \
                                            ps_token_dump(&lexer->current_token); \
                                        }
#define TRACE_ERROR(__PLUS__)           { \
                                            if (interpreter->debug) { \
                                                fprintf(stderr, "ERROR\t%-32s %-8s ", visitor, __PLUS__); \
                                                ps_token_dump(&lexer->current_token); \
                                            } \
                                            return false; \
                                        }
// clang-format on

bool ps_visit_expression(ps_interpreter *interpreter, ps_value *result);

bool ps_visit_factor(ps_interpreter *interpreter, ps_value *result)
{
    USE_LEXER;
    SET_VISITOR("FACTOR");
    TRACE_BEGIN("");
    ps_value factor;
    ps_identifier identifier;
    ps_symbol *symbol;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_LEFT_PARENTHESIS:
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, result))
            TRACE_ERROR("");
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_IDENTIFIER:
        // TODO: variable, const, function, ...
        COPY_IDENTIFIER(identifier);
        symbol = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
        if (symbol == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
        if (symbol->kind != PS_SYMBOL_KIND_AUTO &&
            symbol->kind != PS_SYMBOL_KIND_CONSTANT &&
            symbol->kind != PS_SYMBOL_KIND_VARIABLE)
            RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE);
        result->type = symbol->value->type;
        result->data = symbol->value->data;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_CHAR_VALUE:
        result->type = ps_symbol_char.value->data.t;
        result->data.c = lexer->current_token.value.c;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_INTEGER_VALUE:
        result->type = ps_symbol_integer.value->data.t;
        result->data.i = lexer->current_token.value.i;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_UNSIGNED_VALUE:
        result->type = ps_symbol_unsigned.value->data.t;
        result->data.u = lexer->current_token.value.u;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_REAL_VALUE:
        result->type = ps_symbol_real.value->data.t;
        result->data.r = lexer->current_token.value.r;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_BOOLEAN_VALUE:
        result->type = ps_symbol_boolean.value->data.t;
        result->data.b = lexer->current_token.value.b;
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_MINUS:
    case PS_TOKEN_NOT:
        READ_NEXT_TOKEN;
        if (!ps_visit_factor(interpreter, &factor))
            TRACE_ERROR("");
        if (!ps_function_unary_op(interpreter, &factor, result, PS_TOKEN_MINUS))
            TRACE_ERROR("");
        READ_NEXT_TOKEN;
        break;
    case PS_TOKEN_STRING_VALUE:
    case PS_TOKEN_NIL:
        interpreter->error = PS_ERROR_NOT_IMPLEMENTED;
        TRACE_ERROR("");
    default:
        interpreter->error = PS_PARSER_ERROR_UNEXPECTED_TOKEN;
        TRACE_ERROR("");
    }

    TRACE_END("OK");
    return true;
}

bool ps_visit_term(ps_interpreter *interpreter, ps_value *result)
{
    // '*' | '/' | 'DIV' | 'MOD' | 'AND' | 'SHL' | 'SHR' | 'AS' ;
    static ps_token_type multiplicative_operators[] = {
        PS_TOKEN_STAR, PS_TOKEN_SLASH, PS_TOKEN_DIV, PS_TOKEN_MOD, PS_TOKEN_AND, // PS_TOKEN_SHL, PS_TOKEN_SHR,
    };
    USE_LEXER;
    SET_VISITOR("TERM");
    TRACE_BEGIN("");
    ps_value left = {0}, right = {0};
    ps_token_type multiplicative_operator = PS_TOKEN_NONE;
    if (!ps_visit_factor(interpreter, &left))
        TRACE_ERROR("");
    multiplicative_operator = ps_parser_expect_token_types(
        interpreter->parser,
        sizeof(multiplicative_operators) / sizeof(ps_token_type),
        multiplicative_operators);
    if (multiplicative_operator == PS_TOKEN_NONE)
    {
        result->type = left.type;
        result->data = left.data;
        return true;
    }
    READ_NEXT_TOKEN;
    if (!ps_visit_factor(interpreter, &right))
        TRACE_ERROR("");
    if (!ps_function_binary_op(interpreter, &left, &right, result, multiplicative_operator))
        TRACE_ERROR("");
    TRACE_END("OK");
    return true;
}

bool ps_visit_simple_expression(ps_interpreter *interpreter, ps_value *result)
{
    // '+' | '-' | 'OR' | 'XOR'
    static ps_token_type additive_operators[] = {PS_TOKEN_PLUS, PS_TOKEN_MINUS, PS_TOKEN_OR, PS_TOKEN_XOR};
    USE_LEXER;
    SET_VISITOR("SIMPLE_EXPRESSION");
    TRACE_BEGIN("");
    ps_value left = {0}, right = {0};
    ps_token_type additive_operator = PS_TOKEN_NONE;
    if (!ps_visit_term(interpreter, &left))
        TRACE_ERROR("");
    additive_operator = ps_parser_expect_token_types(
        interpreter->parser,
        sizeof(additive_operators) / sizeof(ps_token_type),
        additive_operators);
    if (additive_operator == PS_TOKEN_NONE)
    {
        result->type = left.type;
        result->data = left.data;
        TRACE_END("1");
        return true;
    }
    READ_NEXT_TOKEN;
    if (!ps_visit_factor(interpreter, &right))
        TRACE_ERROR("");
    if (!ps_function_binary_op(interpreter, &left, &right, result, additive_operator))
        TRACE_ERROR("");
    TRACE_END("2");
    return true;
}

/**
 * SIMPLE_EXPRESSION [ RELATIONAL_OPERATOR SIMPLE_EXPRESSION ]
 */
bool ps_visit_expression(ps_interpreter *interpreter, ps_value *result)
{
    // '<' | '<=' | '>' | '>=' | '=' | '<>' | 'IN' | 'IS'
    static ps_token_type relational_operators[] = {
        PS_TOKEN_LESS_THAN, PS_TOKEN_LESS_OR_EQUAL,
        PS_TOKEN_GREATER_THAN, PS_TOKEN_GREATER_OR_EQUAL,
        PS_TOKEN_EQUAL, PS_TOKEN_NOT_EQUAL, PS_TOKEN_IN, // PS_TOKEN_IS,
    };
    USE_LEXER;
    SET_VISITOR("EXPRESSION");
    TRACE_BEGIN("");
    ps_value left = {0}, right = {0};
    ps_token_type relational_operator = PS_TOKEN_NONE;
    if (!ps_visit_simple_expression(interpreter, &left))
        TRACE_ERROR("");
    relational_operator = ps_parser_expect_token_types(
        interpreter->parser,
        sizeof(relational_operators) / sizeof(ps_token_type),
        relational_operators);
    if (relational_operator == PS_TOKEN_NONE)
    {
        result->type = left.type;
        result->data = left.data;
        TRACE_END("2");
        return true;
    }
    READ_NEXT_TOKEN;
    if (!ps_visit_simple_expression(interpreter, &right))
        TRACE_ERROR("");
    result->type = &ps_symbol_boolean;
    if (!ps_function_binary_op(interpreter, &left, &right, result, relational_operator))
        TRACE_ERROR("");
    TRACE_END("2");
    return true;
}

/**
 * PROGRAM IDENTIFIER ;
 */
bool ps_visit_program(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("PROGRAM");
    TRACE_BEGIN("");
    ps_identifier identifier;
    EXPECT_TOKEN(PS_TOKEN_PROGRAM);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
    READ_NEXT_TOKEN;
    ps_symbol *program = ps_symbol_init(
        PS_SYMBOL_SCOPE_GLOBAL,
        PS_SYMBOL_KIND_PROGRAM,
        &identifier,
        NULL);
    if (ps_symbol_table_add(interpreter->parser->symbols, program) == NULL)
        RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    TRACE_END("OK");
    return true;
}

/**
 * Visit CONST IDENTIFIER = VALUE;
 *             IDENTIFIER = VALUE;
 *             ...
 * Next steps:
 *       IDENTIFIER = IDENTIFIER | VALUE ;
 *       IDENTIFIER = CONSTANT_EXPRESSION ;
 */
bool ps_visit_const(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("CONST");
    TRACE_BEGIN("");
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *constant;
    EXPECT_TOKEN(PS_TOKEN_CONST);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_EQUAL);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_IDENTIFIER:
            constant = ps_symbol_table_get(interpreter->parser->symbols, &lexer->current_token.value.identifier);
            if (constant == NULL)
                RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND);
            if (constant->kind != PS_SYMBOL_KIND_CONSTANT)
                RETURN_ERROR(PS_RUNTIME_ERROR_EXPECTED_CONSTANT);
            type = constant->value->type;
            data = constant->value->data;
            break;
        case PS_TOKEN_INTEGER_VALUE:
            type = ps_symbol_integer.value->data.t;
            data.i = lexer->current_token.value.i;
            break;
        case PS_TOKEN_REAL_VALUE:
            type = ps_symbol_real.value->data.t;
            data.r = lexer->current_token.value.r;
            break;
        case PS_TOKEN_UNSIGNED_VALUE:
            type = ps_symbol_unsigned.value->data.t;
            data.u = lexer->current_token.value.u;
            break;
        case PS_TOKEN_CHAR_VALUE:
            type = ps_symbol_char.value->data.t;
            data.c = lexer->current_token.value.c;
            break;
        case PS_TOKEN_BOOLEAN_VALUE:
            type = ps_symbol_boolean.value->data.t;
            data.b = lexer->current_token.value.b;
            break;
        // Not yet!
        // case PS_TOKEN_STRING_VALUE:
        //     type = ps_symbol_string.value->data.t;
        //     strncpy(data.s + 1, lexer->current_token.value.s, PS_STRING_MAX_LEN);
        //     break;
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        value = ps_value_init(type, data);
        constant = ps_symbol_init(
            PS_SYMBOL_SCOPE_GLOBAL,
            PS_SYMBOL_KIND_CONSTANT,
            &identifier,
            value);
        if (constant == NULL)
        {
            RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
        }
        if (ps_symbol_table_add(interpreter->parser->symbols, constant) == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);
    TRACE_END("OK");
    return true;
}

/**
 * Visit    VAR IDENTIFIER : TYPE;
 *              IDENTIFIER : TYPE;
 *          ...
 * Next step: allow identifier list with commas
 *              IDENTIFIER, IDENTIFIER, ... : TYPE ;
 */
bool ps_visit_var(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("VAR");
    TRACE_BEGIN("");
    ps_identifier identifier;
    ps_type_definition *type;
    ps_value *value;
    ps_value_data data;
    ps_symbol *variable;
    EXPECT_TOKEN(PS_TOKEN_VAR);
    READ_NEXT_TOKEN;
    do
    {
        EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
        COPY_IDENTIFIER(identifier);
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_COLON);
        READ_NEXT_TOKEN;
        switch (lexer->current_token.type)
        {
        case PS_TOKEN_BOOLEAN:
            type = ps_symbol_boolean.value->data.t;
            data.b = (ps_boolean) false;
            break;
        case PS_TOKEN_CHAR:
            type = ps_symbol_char.value->data.t;
            data.c = '\0';
            break;
        case PS_TOKEN_INTEGER:
            type = ps_symbol_integer.value->data.t;
            data.i = 0;
            break;
        case PS_TOKEN_UNSIGNED:
            type = ps_symbol_unsigned.value->data.t;
            data.u = 0;
            break;
        case PS_TOKEN_REAL:
            type = ps_symbol_real.value->data.t;
            data.r = 0.0;
            break;
        default:
            RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
        }
        READ_NEXT_TOKEN;
        EXPECT_TOKEN(PS_TOKEN_SEMI_COLON);
        READ_NEXT_TOKEN;
        value = ps_value_init(type, data);
        variable = ps_symbol_init(
            PS_SYMBOL_SCOPE_GLOBAL,
            PS_SYMBOL_KIND_VARIABLE,
            &identifier,
            value);
        if (variable == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_OUT_OF_MEMORY);
        if (ps_symbol_table_add(interpreter->parser->symbols, variable) == NULL)
            RETURN_ERROR(PS_RUNTIME_ERROR_SYMBOL_NOT_ADDED);
    } while (lexer->current_token.type == PS_TOKEN_IDENTIFIER);
    TRACE_END("OK");
    return true;
}

/**
 * Visit IDENTIFIER := EXPRESSION
 */
bool ps_visit_assignment(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("ASSIGNMENT");
    TRACE_BEGIN("");
    ps_identifier identifier;
    ps_symbol *variable;
    ps_value result = {0};

    COPY_IDENTIFIER(identifier);
    READ_NEXT_TOKEN;
    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN;
    if (!ps_visit_expression(interpreter, &result))
        TRACE_ERROR("");

    // start "code" execution
    variable = ps_symbol_table_get(interpreter->parser->symbols, &identifier);
    if (variable == NULL)
    {
        interpreter->error = PS_RUNTIME_ERROR_SYMBOL_NOT_FOUND;
        TRACE_ERROR("");
    }
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
    {
        interpreter->error = PS_RUNTIME_ERROR_EXPECTED_VARIABLE;
        TRACE_ERROR("");
    }
    if (interpreter->debug)
        ps_value_debug(stderr, "ASSIGN => ", &result);
    if (!ps_function_copy_value(interpreter, &result, variable->value))
        TRACE_ERROR("");
    variable->value->data = result.data;
    // end "code" execution

    TRACE_END("OK");
    return true;
}

/**
 * Visit
 *      WRITE | WRITELN ( EXPRESSION , EXPRESSION ... ) ;
 * Next step:
 *      WRITE | WRITELN ( EXPRESSION [ : WIDTH : PRECISION ] , EXPRESSION [ : WIDTH : PRECISION ] ... ) ;
 */
bool ps_visit_write_or_writeln(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("WRITE_OR_WRITELN");
    TRACE_BEGIN("");
    ps_value result = {0};
    bool newline = lexer->current_token.type == PS_TOKEN_WRITELN;
    bool loop = true;

    READ_NEXT_TOKEN;
    // "Write[Ln];" or "Write[Ln] Else"?
    if (lexer->current_token.type == PS_TOKEN_SEMI_COLON ||
        lexer->current_token.type == PS_TOKEN_ELSE)
    {
        if (newline)
            fprintf(stdout, "\n");
        TRACE_END("EMPTY1");
        return true;
    }
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
    READ_NEXT_TOKEN;
    // "Write[Ln]()"?
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        READ_NEXT_TOKEN;
        loop = false;
    }

    while (loop)
    {
        if (!ps_visit_expression(interpreter, &result))
            TRACE_ERROR("");
        // start "code" execution
        if (!ps_function_write(interpreter, stdout, &result))
            TRACE_ERROR("DISPLAY");
        // end "code" execution
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN;
            continue;
        }
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN;
        loop = false;
    }

    if (newline)
        fprintf(stdout, "\n");

    TRACE_END("OK");
    return true;
}

bool ps_visit_statement_list(ps_interpreter *interpreter);

/**
 * Visit BEGIN
 *         [ STATEMENT ... ] [ ; ]
 *       END
 * NB: ; or . or whatever after END is analyzed in the caller
 */
bool ps_visit_compound_statement(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("COMPOUND_STATEMENT");
    TRACE_BEGIN("");

    EXPECT_TOKEN(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN;
    if (lexer->current_token.type != PS_TOKEN_END)
    {
        if (!ps_visit_statement_list(interpreter))
            TRACE_ERROR("");
    }
    EXPECT_TOKEN(PS_TOKEN_END);
    READ_NEXT_TOKEN;

    TRACE_END("OK");
    return true;
}

/**
 * Visit statement
 *      IDENTIFIER := EXPRESSION
 *      WRITE | WRITELN ( EXPRESSION )
 * Next steps:
 *      IF EXPRESSION THEN STATEMENT ;
 *      IF EXPRESSION THEN STATEMENT ELSE STATEMENT ;
 *      IF EXPRESSION THEN BEGIN STATEMENTS END ELSE STATEMENT ;
 *      IF EXPRESSION THEN BEGIN STATEMENTS END ELSE BEGIN STATEMENTS END ;
 *      IF EXPRESSION THEN STATEMENT ELSE BEGIN STATEMENTS END ;
 *      WRITE | WRITELN ( EXPRESSION , EXPRESSION ... ) ;
 */
bool ps_visit_statement(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("STATEMENT");
    // for assignement & write[ln]
    ps_value result = {0};
    TRACE_BEGIN("");

    if (lexer->current_token.type == PS_TOKEN_BEGIN)
    {
        if (!ps_visit_compound_statement(interpreter))
            TRACE_ERROR("");
        TRACE_END("OK");
        return true;
    }

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_IDENTIFIER:
        if (!ps_visit_assignment(interpreter))
            TRACE_ERROR("");
        break;
    case PS_TOKEN_IF:
        /* IF EXPRESSION THEN STATEMENT [ ELSE STATEMENT ] ; */
        READ_NEXT_TOKEN;
        if (!ps_visit_expression(interpreter, &result))
            TRACE_ERROR("");
        if (result.type != ps_symbol_boolean.value->data.t)
            RETURN_ERROR(PS_RUNTIME_ERROR_UNEXPECTED_TYPE);
        EXPECT_TOKEN(PS_TOKEN_THEN);
        READ_NEXT_TOKEN;
        if (result.data.b == true)
        {
            if (!ps_visit_statement(interpreter))
                TRACE_ERROR("");
        }
        else
        {
        }
        break;
    case PS_TOKEN_WRITE:
    case PS_TOKEN_WRITELN:
        if (!ps_visit_write_or_writeln(interpreter))
            TRACE_ERROR("");
        break;
    default:
        RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
    }

    TRACE_END("OK");
    return true;
}

/**
 * Visit statement sequence
 */
bool ps_visit_statement_list(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("STATEMENT_LIST");
    TRACE_BEGIN("");

    if (lexer->current_token.type == PS_TOKEN_END)
    {
        READ_NEXT_TOKEN;
    }
    else
    {
        // let's go!
        bool loop = true;
        do
        {
            if (!ps_visit_statement(interpreter))
                TRACE_ERROR("");
            if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            {
                READ_NEXT_TOKEN;
                if (lexer->current_token.type == PS_TOKEN_END)
                {
                    loop = false;
                }
            }
            else if (lexer->current_token.type == PS_TOKEN_END)
            {
                loop = false;
            }
            else
            {
                RETURN_ERROR(PS_PARSER_ERROR_UNEXPECTED_TOKEN);
            }
        } while (loop);
    }
    TRACE_END("OK");
    return true;
}

bool ps_visit_statement_or_compound_statement(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("STATEMENT");
    TRACE_BEGIN("");
    if (lexer->current_token.type == PS_TOKEN_BEGIN)
    {
        READ_NEXT_TOKEN;
        if (!ps_visit_compound_statement(interpreter))
            TRACE_ERROR("");
        return true;
    }
    if (!ps_visit_statement(interpreter))
        TRACE_ERROR("");
    TRACE_END("OK");
    return true;
}

/**
 * Visit [ CONST ... TYPE ... VAR ... ]*
 *       COMPOUND_STATEMENT
 * NB: ; or . or whatever after END is analyzed in the caller
 */
bool ps_visit_block(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("BLOCK");
    TRACE_BEGIN("");

    do
    {
        if (lexer->current_token.type == PS_TOKEN_CONST && !ps_visit_const(interpreter))
            TRACE_ERROR("");
        // if (lexer->current_token.type == PS_TOKEN_TYPE && !ps_visit_type(interpreter))
        //     TRACE_ERROR("");
        if (lexer->current_token.type == PS_TOKEN_VAR && !ps_visit_var(interpreter))
            TRACE_ERROR("");
    } while (lexer->current_token.type != PS_TOKEN_BEGIN);

    if (!ps_visit_compound_statement(interpreter))
        TRACE_ERROR("");

    TRACE_END("OK");
    return true;
}

/**
 * Visit PROGRAM...
 *       BLOCK
 *       .
 */
bool ps_visit_start(ps_interpreter *interpreter)
{
    USE_LEXER;
    SET_VISITOR("START");
    TRACE_BEGIN("");
    READ_NEXT_TOKEN;
    if (!ps_visit_program(interpreter))
        TRACE_ERROR("");
    if (!ps_visit_block(interpreter))
        TRACE_ERROR("");
    EXPECT_TOKEN(PS_TOKEN_DOT);
    // NB: source code after '.' is not analyzed and has not to be
    TRACE_END("OK");
    return true;
}

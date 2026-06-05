/*
    This file is part of the PascalScript Pascal compiler.
    SPDX-FileCopyrightText: 2025 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#include "ps_parse_statement.h"
#include "ps_array.h"
#include "ps_ast.h"
#include "ps_functions.h"
#include "ps_parse.h"
#include "ps_parse_executable.h"
#include "ps_parse_expression.h"
#include "ps_procedures.h"
#include "ps_symbol.h"
#include "ps_system.h"
#include "ps_token.h"

/**
 * Parse statement:
 *      compound_statement
 *      assignment_statement
 *      procedure_call_statement
 *      if_then_else_statement
 *      repeat_until_statement
 *      while_do_statement
 *      for_to_downto_do_statement
 */
bool ps_parse_statement(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **statement)
{
    PARSE_BEGIN("STATEMENT", "");
    (void)start_line;
    (void)start_column;

    switch (lexer->current_token.type)
    {
    case PS_TOKEN_BEGIN:
        ps_ast_statement_list *statement_list = NULL;
        if (!ps_parse_compound_statement(compiler, block, &statement_list))
            TRACE_ERROR("COMPOUND")
        *statement = (ps_ast_node *)statement_list;
        break;
    case PS_TOKEN_IDENTIFIER:
        if (!ps_parse_assignment_or_procedure_call(compiler, block, statement))
            TRACE_ERROR("ASSIGNMENT/PROCEDURE")
        break;
    case PS_TOKEN_IF:
        ps_ast_if *if_statement = NULL;
        if (!ps_parse_if_then_else(compiler, block, &if_statement))
            TRACE_ERROR("IF")
        *statement = (ps_ast_node *)if_statement;
        break;
    case PS_TOKEN_REPEAT:
        ps_ast_repeat *repeat_statement = NULL;
        if (!ps_parse_repeat_until(compiler, block, &repeat_statement))
            TRACE_ERROR("REPEAT")
        *statement = (ps_ast_node *)repeat_statement;
        break;
    case PS_TOKEN_WHILE:
        ps_ast_while *while_statement = NULL;
        if (!ps_parse_while_do(compiler, block, &while_statement))
            TRACE_ERROR("WHILE")
        *statement = (ps_ast_node *)while_statement;
        break;
    case PS_TOKEN_FOR:
        ps_ast_for *for_statement = NULL;
        if (!ps_parse_for_do(compiler, block, &for_statement))
            TRACE_ERROR("FOR")
        *statement = (ps_ast_node *)for_statement;
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * Parse compound statement:
 *      'BEGIN'
 *          [ STATEMENT [ ';' STATEMENT ]* ] [ ';' ]
 *      'END'
 * NB: ';' or '.' or whatever after END is analyzed in the caller
 */
bool ps_parse_compound_statement(ps_compiler *compiler, ps_ast_block *block, ps_ast_statement_list **statement_list)
{
    PARSE_BEGIN("STATEMENT", "COMPOUND");

    // 'BEGIN'
    EXPECT_TOKEN(PS_TOKEN_BEGIN);
    READ_NEXT_TOKEN

    // [ STATEMENT [ ';' STATEMENT ]* ] [ ';' ]
    if (!ps_parse_statement_list(compiler, block, statement_list, PS_TOKEN_END))
        TRACE_ERROR("STATEMENT_LIST")

    // 'END'
    EXPECT_TOKEN(PS_TOKEN_END)
    READ_NEXT_TOKEN

    // Fix statement list position to BEGIN token, not first statement (if any)
    ps_ast_statement_list *statement_list2 = *statement_list;
    statement_list2->line = start_line;
    statement_list2->column = start_column;

    PARSE_END("OK")
}

// bool ps_parse_assignment_array(ps_compiler *compiler, ps_ast_block *block, ps_ast_node *statement, ps_symbol
// *variable)
// {
//     PARSE_BEGIN("ASSIGNMENT", "ARRAY")

//     ps_value result = {.allocated = false, .type = &ps_system_none, .data.v = NULL};
//     ps_symbol *item_type = ps_array_get_subrange(variable);
//     u_int8_t dimensions = ps_array_get_dimensions(variable);
//     if (dimensions > 8)
//         RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
//     ps_value indexes[8] = {0};
//     int dimension = 0;
//     // Initialize index types for each dimension
//     bool loop = true;
//     do
//     {
//         indexes[dimension].allocated = false;
//         indexes[dimension].type = item_type;
//         indexes[dimension].data.v = NULL;
//         if (!loop)
//             break;
//         dimension += 1;
//         item_type = item_type->value->type->value->data.t->def.a.subrange;
//         if (item_type->kind != PS_TYPE_ARRAY)
//             loop = false;
//     } while (true);

//     EXPECT_TOKEN(PS_TOKEN_LEFT_BRACKET)
//     READ_NEXT_TOKEN
//     do
//     {
//         // At least one index
//         if (!ps_parse_expression(compiler, &indexes[dimension]))
//             TRACE_ERROR("INDEX")
//         dimension += 1;
//         // ',' begins another index
//         if (lexer->current_token.type == PS_TOKEN_COMMA)
//         {
//             // Too many indexes?
//             if (dimension == dimensions)
//                 RETURN_ERROR(PS_ERROR_TOO_MANY_DIMENSIONS)
//             READ_NEXT_TOKEN
//             continue;
//         }
//         // ']' ends indexes (and loop)
//         if (lexer->current_token.type == PS_TOKEN_RIGHT_BRACKET)
//         {
//             // Not enough indexes?
//             if (dimension != dimensions)
//                 RETURN_ERROR(PS_ERROR_NOT_ENOUGH_DIMENSIONS)
//             READ_NEXT_TOKEN
//             break;
//         }
//         RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
//     } while (true);
//     // Check for ':='
//     EXPECT_TOKEN(PS_TOKEN_ASSIGN)
//     READ_NEXT_TOKEN
//     // Parse expression for value, expected type is item type
//     result.type = item_type;
//     if (!ps_parse_expression(compiler, &result))
//         TRACE_ERROR("EXPRESSION1")
//     // if (mode == MODE_EXEC)
//     // {
//     //     ps_error error = ps_array_set_value(variable, &indexes, &result, compiler->range_check);
//     //     if (error != PS_ERROR_NONE)
//     //     {
//     //         compiler->error = error;
//     //         TRACE_ERROR("ARRAY_ASSIGN")
//     //     }
//     // }

//     PARSE_END("OK")
// }

/**
 * Parse assignment:
 *  Simple:
 *      IDENTIFIER := EXPRESSION
 * Next steps:
 *  Array access:
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' := EXPRESSION
 *  Pointer dereference:
 *      IDENTIFIER '^' = EXPRESSION
 *      IDENTIFIER '[' EXPRESSION [ ',' EXPRESSION ]* ']' '^' := EXPRESSION
 */
bool ps_parse_assignment(ps_compiler *compiler, ps_ast_block *block, ps_ast_assignment **assignment,
                         ps_symbol *variable)
{
    PARSE_BEGIN("STATEMENT", "ASSIGNMENT")
    ps_ast_node *expression = NULL;
    ps_ast_node *lvalue = NULL;

    // IDENTIFIER
    if (variable->kind == PS_SYMBOL_KIND_CONSTANT)
    {
        compiler->error = PS_ERROR_ASSIGN_TO_CONST;
        ps_compiler_set_message(compiler, "Constant '%s' cannot be assigned", variable->name);
        TRACE_ERROR("CONSTANT");
    }
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
    {
        compiler->error = PS_ERROR_EXPECTED_VARIABLE;
        ps_compiler_set_message(compiler, "Symbol '%s' is not a variable", variable->name);
        TRACE_ERROR("VARIABLE");
    }

    if (compiler->debug >= PS_DEBUG_VERBOSE)
        fprintf(stderr, "\nINFO\tASSIGNMENT: #1 variable '%s' type is '%s'\n", variable->name,
                ps_type_definition_get_name(variable->value->type->value->data.t));
    if (ps_value_get_type(variable->value) == PS_TYPE_ARRAY)
    {
        // => array[index] := expression
        RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
        // if (!ps_parse_assignment_array(compiler, variable))
        //     TRACE_ERROR("ARRAY")
    }
    else
    {
        // ':='
        EXPECT_TOKEN(PS_TOKEN_ASSIGN);
        READ_NEXT_TOKEN
        // EXPRESSION
        if (!ps_parse_expression(compiler, block, &expression))
            TRACE_ERROR("EXPRESSION1");
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "\nINFO\tASSIGNMENT: #2 variable '%s' type is '%s'\n", variable->name,
                    ps_type_definition_get_name(variable->value->type->value->data.t));
        // AST NODE => ASSIGNMENT(LVALUE, EXPRESSION)
        lvalue = (ps_ast_node *)ps_ast_create_variable_simple(start_line, start_column, PS_AST_LVALUE_SIMPLE, variable);
        if (lvalue == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }

    *assignment = ps_ast_create_assignment(start_line, start_column, lvalue, expression);
    if (assignment == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse
 *      'READ'  | 'READLN' [ '('
 *          variable [ ',' variable ] ]*
 *      ')' ] ;
 * Next steps:
 *   Read from text file:
 *      'READ' | 'READLN' [ '('
 *          [ file_variable ',' ]
 *          variable [ ',' variable ]*
 *      ')' ] ;
 *   Read from binary file:
 *      'READ' '('
 *          file_variable ',' variable
 *      ')' ;
 */
bool ps_parse_read_or_readln(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call, bool newline)
{
    (void)compiler;
    (void)block;
    (void)call;
    (void)newline;
    PARSE_BEGIN("STATEMENT", "READ_OR_READLN")
    (void)start_line;
    (void)start_column;
    RETURN_ERROR(PS_ERROR_NOT_IMPLEMENTED)
}

/**
 * Parse
 *      'WRITE' | 'WRITELN' [ '('
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 * Next steps:
 *   Write to text file:
 *      'WRITE' | 'WRITELN' [ '('
 *          [ file_variable ',' ]
 *          expression [ ':' width [ ':' precision ] ]
 *          [ ',' expression [ ':' width [ ':' precision ] ] ]*
 *      ')' ] ;
 *   Write to binary file:
 *      'WRITE' '('
 *          file_variable ',' expression
 *      ')' ;
 */
bool ps_parse_write_or_writeln(ps_compiler *compiler, ps_ast_block *block, ps_ast_call **call, bool newline)
{
    PARSE_BEGIN("STATEMENT", "WRITE_OR_WRITELN");

    size_t n_args = 0;
    ps_ast_node *args[8] = {0};
    int16_t widths[8] = {0};
    int16_t precisions[8] = {0};
    bool loop = true;
    int16_t width = 0;
    int16_t precision = 0;
    ps_ast_node *expression = NULL;

    // "Write[Ln];" or "Write[Ln] Else|End|Until"?
    // (Write without parameters is legal but is a no-op)
    if (PS_TOKEN_NONE != ps_parser_expect_statement_end_token(compiler->parser))
    {
        if (newline)
            fprintf(stdout, "\n");
        PARSE_END("EMPTY1");
    }
    EXPECT_TOKEN(PS_TOKEN_LEFT_PARENTHESIS);
    READ_NEXT_TOKEN
    // "Write[Ln]()"?
    if (lexer->current_token.type == PS_TOKEN_RIGHT_PARENTHESIS)
    {
        // if (mode == MODE_EXEC && newline)
        //     fprintf(stdout, "\n");
        READ_NEXT_TOKEN
        loop = false;
    }

    while (loop)
    {
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "\nINFO\tWRITE_OR_WRITELN: expecting expression of type 'ANY'\n");
        if (!ps_parse_expression(compiler, block, &expression))
            TRACE_ERROR("EXPRESSION")
        // retrieve string/numeric format
        width = 0;
        precision = 0;
        if (lexer->current_token.type == PS_TOKEN_COLON)
        {
            READ_NEXT_TOKEN
            EXPECT_TOKEN(PS_TOKEN_UNSIGNED_VALUE);
            width = (int16_t)(lexer->current_token.value.u);
            READ_NEXT_TOKEN
            if (lexer->current_token.type == PS_TOKEN_COLON)
            {
                READ_NEXT_TOKEN
                EXPECT_TOKEN(PS_TOKEN_UNSIGNED_VALUE);
                precision = (int16_t)(lexer->current_token.value.u);
                READ_NEXT_TOKEN
            }
        }
        // if (mode == MODE_EXEC && !ps_procedure_write(compiler, stdout, &result, width, precision))
        //     TRACE_ERROR(newline ? "WRITELN" : "WRITE");
        if (n_args >= 8)
            RETURN_ERROR(PS_ERROR_TOO_MANY_ARGUMENTS)
        args[n_args] = expression;
        widths[n_args] = width;
        precisions[n_args] = precision;
        n_args += 1;
        if (lexer->current_token.type == PS_TOKEN_COMMA)
        {
            READ_NEXT_TOKEN
            continue;
        }
        EXPECT_TOKEN(PS_TOKEN_RIGHT_PARENTHESIS);
        READ_NEXT_TOKEN
        loop = false;
    }

    *call = ps_ast_create_call(start_line, start_column, PS_AST_PROCEDURE_CALL,
                               newline ? &ps_system_procedure_writeln : &ps_system_procedure_write, n_args,
                               n_args > 0 ? args : NULL, widths, precisions);
    if (*call == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse assignment or procedure call:
 *  this is determined by the symbol kind:
 *      - variable: assignment
 *      - procedure: procedure call
 */
bool ps_parse_assignment_or_procedure_call(ps_compiler *compiler, ps_ast_block *block, ps_ast_node **statement)
{
    PARSE_BEGIN("STATEMENT", "ASSIGNMENT OR PROCEDURE CALL");
    (void)start_line;
    (void)start_column;

    ps_identifier identifier;
    ps_symbol *symbol;
    ps_ast_assignment **assignement = NULL;

    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN

    // First, check if this is an assignment to the current function name
    symbol = ps_symbol_table_get(block->parent->symbols, identifier);
    if (symbol != NULL && symbol->kind == PS_SYMBOL_KIND_FUNCTION && strcmp((char *)identifier, block->name) == 0)
    {
        if (compiler->debug >= PS_DEBUG_VERBOSE)
            fprintf(stderr, "INFO\tAssignment to current function '%s' as Result\n", (char *)identifier);
        // Assign to the not so implicit "Result" local variable
        symbol = ps_compiler_find_symbol(compiler, block, "RESULT", false);
    }
    else
    {
        // Normal lookup - can be variable, constant, procedure, or function
        symbol = ps_compiler_find_symbol(compiler, block, identifier, false);
    }
    if (symbol == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    switch (symbol->kind)
    {
    case PS_SYMBOL_KIND_CONSTANT:
        ps_compiler_set_message(compiler, "Constant '%s' cannot be assigned", symbol->name);
        RETURN_ERROR(PS_ERROR_ASSIGN_TO_CONST)
    case PS_SYMBOL_KIND_VARIABLE:
        if (!ps_parse_assignment(compiler, block, assignement, symbol))
            TRACE_ERROR("ASSIGNMENT")
        statement = (ps_ast_node **)assignement;
        break;
    case PS_SYMBOL_KIND_PROCEDURE:
        ps_ast_call **procedure = NULL;
        if (!ps_parse_procedure_or_function_call(compiler, block, procedure, symbol))
            TRACE_ERROR("PROCEDURE_CALL")
        *statement = (ps_ast_node *)(*procedure);
        break;
    case PS_SYMBOL_KIND_FUNCTION:
        // Assignment to function name => assignment to Result
        if (strcmp(block->name, identifier) != 0)
        {
            ps_compiler_set_message(compiler, "Cannot assign to %s from %s", identifier, block->name);
            RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN);
        }
        symbol = ps_compiler_find_symbol(compiler, block, "RESULT", true);
        if (symbol == NULL)
            RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND)
        if (!ps_parse_assignment(compiler, block, assignement, symbol))
            TRACE_ERROR("ASSIGNMENT")
        *statement = (ps_ast_node *)(*assignement);
        break;
    default:
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    }

    PARSE_END("OK")
}

/**
 * Parse
 *      'IF' expression 'THEN' statement [ 'ELSE' statement ]
 */
bool ps_parse_if_then_else(ps_compiler *compiler, ps_ast_block *block, ps_ast_if **if_statement)
{
    PARSE_BEGIN("STATEMENT", "IF_THEN_ELSE")

    ps_ast_node **condition = NULL;
    ps_ast_statement_list *then_branch = NULL;
    ps_ast_statement_list *else_branch = NULL;
    ps_ast_statement_list *statement_list = NULL;

    // IF
    EXPECT_TOKEN(PS_TOKEN_IF)
    READ_NEXT_TOKEN

    // Condition
    if (!ps_parse_expression(compiler, block, condition))
        TRACE_ERROR("CONDITION")
    // if (result.type != &ps_system_boolean)
    //     RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);

    // THEN
    EXPECT_TOKEN(PS_TOKEN_THEN)
    READ_NEXT_TOKEN

    // Statement
    ps_ast_node **then_node = NULL;
    if (!ps_parse_statement(compiler, block, then_node))
        TRACE_ERROR("THEN")
    if ((*then_node)->kind == PS_AST_STATEMENT_LIST)
        then_branch = (ps_ast_statement_list *)(*then_node);
    else
    {
        statement_list = ps_ast_create_statement_list((*then_node)->line, (*then_node)->column, 1);
        if (statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        statement_list->statements[0] = *then_node;
        then_branch = statement_list;
    }

    // ELSE?
    if (lexer->current_token.type == PS_TOKEN_ELSE)
    {
        READ_NEXT_TOKEN
        // Statement
        ps_ast_node **else_node = NULL;
        if (!ps_parse_statement(compiler, block, else_node))
            TRACE_ERROR("ELSE")
        if ((*else_node)->kind == PS_AST_STATEMENT_LIST)
            else_branch = (ps_ast_statement_list *)(*else_node);
        else
        {
            ps_ast_statement_list *statement_list = NULL;
            statement_list = ps_ast_create_statement_list((*else_node)->line, (*else_node)->column, 1);
            if (statement_list == NULL)
                RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
            statement_list->statements[0] = *else_node;
            then_branch = statement_list;
        }
    }

    *if_statement = ps_ast_create_if(start_line, start_column, *condition, then_branch, else_branch);
    if (*if_statement == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse
 *      'REPEAT' statement_list [ ';' ] 'UNTIL' expression ;
 */
bool ps_parse_repeat_until(ps_compiler *compiler, ps_ast_block *block, ps_ast_repeat **repeat_statement)
{
    PARSE_BEGIN("STATEMENT", "REPEAT_UNTIL");

    ps_ast_statement_list **body = NULL;
    ps_ast_node **condition = NULL;

    // REPEAT
    EXPECT_TOKEN(PS_TOKEN_REPEAT)
    READ_NEXT_TOKEN

    // STATEMENT LIST
    if (!ps_parse_statement_list(compiler, block, body, PS_TOKEN_UNTIL))
        TRACE_ERROR("STATEMENTS");

    // UNTIL
    EXPECT_TOKEN(PS_TOKEN_UNTIL);
    READ_NEXT_TOKEN

    // CONDITION
    if (!ps_parse_expression(compiler, block, condition))
        TRACE_ERROR("EXPRESSION");
    // if (result.type != &ps_system_boolean)
    //     RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);
    READ_NEXT_TOKEN

    // AST NODE => REPEAT(BODY, CONDITION)
    *repeat_statement = ps_ast_create_repeat(start_line, start_column, *body, *condition);
    if (*repeat_statement == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse
 *      'WHILE' expression 'DO' statement
 */
bool ps_parse_while_do(ps_compiler *compiler, ps_ast_block *block, ps_ast_while **while_statement)
{
    PARSE_BEGIN("STATEMENT", "WHILE_DO");

    ps_ast_node **condition = NULL;
    ps_ast_statement_list *body = NULL;

    // WHILE
    EXPECT_TOKEN(PS_TOKEN_WHILE)
    READ_NEXT_TOKEN

    // CONDITION
    if (!ps_parse_expression(compiler, block, condition))
        TRACE_ERROR("EXPRESSION");
    // if (result.type != &ps_system_boolean)
    //     RETURN_ERROR(PS_ERROR_UNEXPECTED_TYPE);

    // DO
    EXPECT_TOKEN(PS_TOKEN_DO);
    READ_NEXT_TOKEN

    ps_ast_node **body_node = NULL;
    if (!ps_parse_statement(compiler, block, body_node))
        TRACE_ERROR("STATEMENT");
    if ((*body_node)->kind == PS_AST_STATEMENT_LIST)
        body = (ps_ast_statement_list *)body_node;
    else
    {
        ps_ast_statement_list *statement_list =
            ps_ast_create_statement_list((*body_node)->line, (*body_node)->column, 1);
        if (statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        statement_list->statements[0] = *body_node;
        body = statement_list;
    }

    *while_statement = ps_ast_create_while(start_line, start_column, *condition, body);
    if (*while_statement == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse
 *      'FOR' control_variable ':=' expression ( 'TO' | 'DOWNTO' ) expression 'DO' statement ;
 */
bool ps_parse_for_do(ps_compiler *compiler, ps_ast_block *block, ps_ast_for **for_statement)
{
    PARSE_BEGIN("STATEMENT", "FOR_DO");

    ps_symbol *variable = NULL;
    ps_ast_node *start = NULL;
    ps_ast_node *finish = NULL;
    bool downto = false;
    ps_ast_statement_list *body = NULL;
    ps_identifier identifier = {0};

    // FOR
    EXPECT_TOKEN(PS_TOKEN_FOR)
    READ_NEXT_TOKEN

    // CONTROL_VARIABLE
    EXPECT_TOKEN(PS_TOKEN_IDENTIFIER);
    COPY_IDENTIFIER(identifier)
    READ_NEXT_TOKEN
    variable = ps_compiler_find_symbol(compiler, block, identifier, true);
    if (variable == NULL)
        RETURN_ERROR(PS_ERROR_SYMBOL_NOT_FOUND);
    if (variable->kind != PS_SYMBOL_KIND_VARIABLE)
        RETURN_ERROR(PS_ERROR_EXPECTED_VARIABLE);
    if (!ps_value_is_ordinal(variable->value))
        RETURN_ERROR(PS_ERROR_EXPECTED_ORDINAL)
    // start.type = variable->value->type;
    // finish.type = variable->value->type;

    // :=
    EXPECT_TOKEN(PS_TOKEN_ASSIGN);
    READ_NEXT_TOKEN

    // START VALUE
    if (!ps_parse_expression(compiler, block, &start))
        TRACE_ERROR("START");

    // TO | DOWNTO
    if (lexer->current_token.type == PS_TOKEN_TO)
        downto = false;
    else if (lexer->current_token.type == PS_TOKEN_DOWNTO)
        downto = true;
    else
        RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
    READ_NEXT_TOKEN

    // FINISH VALUE
    if (!ps_parse_expression(compiler, block, &finish))
        TRACE_ERROR("FINISH");

    // DO
    EXPECT_TOKEN(PS_TOKEN_DO);
    READ_NEXT_TOKEN

    if (!ps_parse_statement_or_compound_statement(compiler, block, &body))
        TRACE_ERROR("STATEMENT_OR_COMPOUND")

    ps_ast_variable_simple *variable_node =
        ps_ast_create_variable_simple(start_line, start_column, PS_AST_LVALUE_SIMPLE, variable);
    if (variable_node == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    *for_statement = ps_ast_create_for(start_line, start_column, variable_node, start, finish, downto ? -1 : 1, body);
    if (*for_statement == NULL)
        RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)

    PARSE_END("OK")
}

/**
 * Parse statement sequence, stopping at "stop" token (END or UNTIL)
 *  skip/ignore ';' before end
 */
bool ps_parse_statement_list(ps_compiler *compiler, ps_ast_block *block, ps_ast_statement_list **statement_list,
                             ps_token_type stop)
{
    PARSE_BEGIN("STATEMENT", "STATEMENT_LIST");

    // Empty block?
    if (lexer->current_token.type == stop)
    {
        *statement_list = ps_ast_create_statement_list(start_line, start_column, 0);
        if (*statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
    }
    else
    {
        // Let's go!
        ps_ast_node **statement = NULL;
        // Up to 256 statements for now
        size_t count = 0;
        ps_ast_node *statements[256];
        bool loop = true;
        do
        {
            if (!ps_parse_statement(compiler, block, statement))
                TRACE_ERROR("STATEMENT");
            count += 1;
            if (count > 255)
                RETURN_ERROR(PS_ERROR_TOO_MANY_ARGUMENTS) // should be PS_ERROR_TOO_MANY_STATEMENTS
            statements[count - 1] = (ps_ast_node *)(*statement);
            // NB: semi-colon at statement list end is optional
            if (lexer->current_token.type == PS_TOKEN_SEMI_COLON)
            {
                READ_NEXT_TOKEN
                if (lexer->current_token.type == stop) // NOSONAR
                    loop = false;
            }
            else if (lexer->current_token.type == stop)
                loop = false;
            else
                RETURN_ERROR(PS_ERROR_UNEXPECTED_TOKEN)
        } while (loop);
        // Create statement list and copy statements into it
        *statement_list = ps_ast_create_statement_list(start_line, start_column, count);
        if (*statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        for (size_t i = 0; i < count; i += 1)
            (*statement_list)->statements[i] = statements[i];
    }

    PARSE_END("OK")
}

/**
 * Parse statement or compound statement:
 *      statement_or_compound_statement = statement | compound_statement
 */
bool ps_parse_statement_or_compound_statement(ps_compiler *compiler, ps_ast_block *block,
                                              ps_ast_statement_list **statement_list)
{
    PARSE_BEGIN("STATEMENT", "STATEMENT_OR_COMPOUND_STATEMENT");

    if (lexer->current_token.type == PS_TOKEN_BEGIN)
    {
        if (!ps_parse_compound_statement(compiler, block, statement_list))
            TRACE_ERROR("COMPOUND");
    }
    else
    {
        ps_ast_node **statement = NULL;
        if (!ps_parse_statement(compiler, block, statement))
            TRACE_ERROR("STATEMENT");
        *statement_list = ps_ast_create_statement_list(start_line, start_column, 1);
        if (*statement_list == NULL)
            RETURN_ERROR(PS_ERROR_OUT_OF_MEMORY)
        (*statement_list)->statements[0] = *statement;
    }

    PARSE_END("OK")
}

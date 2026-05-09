/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_H
#define _PS_AST_H

#include "ps_operator.h"
#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Abstract Syntax Tree node group */
    typedef enum e_ps_ast_node_group
    {
        PS_AST_GROUP_BLOCK,
        PS_AST_GROUP_STATEMENT,
        PS_AST_GROUP_EXPRESSION,
        PS_AST_GROUP_LVALUE
    } __attribute__((__packed__)) ps_ast_node_group;

    /** @brief Abstract Syntax Tree node kind */
    typedef enum enum_ps_ast_node_kind
    {
        PS_AST_PROGRAM,          /** @brief BLOCK:      PROGRAM                                      */
        PS_AST_PROCEDURE,        /** @brief BLOCK:      PROCEDURE                                    */
        PS_AST_FUNCTION,         /** @brief BLOCK:      FUNCTION                                     */
        PS_AST_UNIT,             /** @brief BLOCK:      UNIT                                         */
        PS_AST_STATEMENT_LIST,   /** @brief STATEMENT:  List of statements                           */
        PS_AST_ASSIGNMENT,       /** @brief STATEMENT:  Assignment                                   */
        PS_AST_IF,               /** @brief STATEMENT:  IF                                           */
        PS_AST_WHILE,            /** @brief STATEMENT:  WHILE                                        */
        PS_AST_REPEAT,           /** @brief STATEMENT:  REPEAT                                       */
        PS_AST_FOR,              /** @brief STATEMENT:  FOR                                          */
        PS_AST_PROCEDURE_CALL,   /** @brief STATEMENT:  PROCEDURE call                               */
        PS_AST_UNARY_OPERATION,  /** @brief EXPRESSION: Unary operation                              */
        PS_AST_BINARY_OPERATION, /** @brief EXPRESSION: Binary operation                             */
        PS_AST_FUNCTION_CALL,    /** @brief EXPRESSION: FUNCTION call                                */
        PS_AST_VALUE,            /** @brief EXPRESSION: Value: integer, real, string, boolean, ...   */
        PS_AST_VARIABLE_SIMPLE,  /** @brief EXPRESSION: Simple variable (or constant) being accessed */
        PS_AST_VARIABLE_ARRAY,   /** @brief EXPRESSION: Array element being accessed                 */
        PS_AST_LVALUE_SIMPLE,    /** @brief LVALUE:     Simple variable being written to             */
        PS_AST_LVALUE_ARRAY,     /** @brief LVALUE:     Array element being written to               */
    } __attribute__((__packed__)) ps_ast_node_kind;

#define PS_AST_NODE_COMMON                                                                                             \
    ps_ast_node_group group; /** @brief Node group                                                  */                 \
    ps_ast_node_kind kind;   /** @brief Node kind                                                   */                 \
    uint16_t line;           /** @brief Source code line number for error reporting, 0 if unknown   */                 \
    uint16_t column;         /** @brief Source code column number for error reporting, 0 if unknown */

    /** @brief Abstract Syntax Tree node */
    typedef struct s_ps_ast_node
    {
        PS_AST_NODE_COMMON
    } ps_ast_node;

    /** @brief Block is a program, procedure, function or unit */
    /** @details Units may be separated as they are special cases with interface and implementation */
    typedef struct s_ps_ast_block
    {
        PS_AST_NODE_COMMON
        ps_identifier name;                    /** @brief Every block has a name                                    */
        size_t n_vars;                         /** @brief Number of variables to allocate at startup                */
        ps_symbol_table *symbols;              /** @brief Constants, types, variables, procedures and functions     */
        size_t n_executables;                  /** @brief exactly 1 for procedure and function, 0 or more otherwise */
        ps_ast_node **executables;             /** @brief declarations of procedures and functions                  */
        ps_ast_statement_list *statement_list; /** @brief Statements in this block                                  */
        ps_formal_signature *signature;        /** @brief Only for procedures and functions, empty otherwise        */
        ps_symbol *result_type;                /** @brief Only for functions, NULL otherwise                        */
    } ps_ast_block;

    typedef struct s_ps_ast_statement_list
    {
        PS_AST_NODE_COMMON
        size_t count;             /** @brief Number of statements  */
        ps_ast_node **statements; /** @brief NULL if no statements */
    } ps_ast_statement_list;

    typedef struct s_ps_ast_if
    {
        PS_AST_NODE_COMMON
        ps_ast_node *condition;             /** @brief If condition, must be a boolean expression                */
        ps_ast_statement_list *then_branch; /** @brief Statements to execute if condition is true, can be empty  */
        ps_ast_statement_list *else_branch; /** @brief Statements to execute if condition is false, can be empty */
    } ps_ast_if;

    typedef struct s_ps_ast_while
    {
        PS_AST_NODE_COMMON
        ps_ast_node *condition;      /** @brief Loop while condition is true                  */
        ps_ast_statement_list *body; /** @brief Statements to execute while condition is true */
    } ps_ast_while;

    typedef struct s_ps_ast_repeat
    {
        PS_AST_NODE_COMMON
        ps_ast_statement_list *body; /** @brief Statements to execute at least once */
        ps_ast_node *condition;      /** @brief Loop until condition is true        */
    } ps_ast_repeat;

    typedef struct s_ps_ast_for
    {
        PS_AST_NODE_COMMON
        ps_ast_node *variable;       /** @brief Loop variable                                         */
        ps_ast_node *start;          /** @brief Start value                                           */
        ps_ast_node *end;            /** @brief End value                                             */
        int step;                    /** @brief Step value: 1 for "TO", -1 for "DOWNTO"               */
        ps_ast_statement_list *body; /** @brief Statements to execute for each value of loop variable */
    } ps_ast_for;

    typedef struct s_ps_ast_call
    {
        PS_AST_NODE_COMMON
        ps_symbol *executable;      /** @brief procedure of function being called     */
        size_t n_args;              /** @brief number of arguments, 0 if no arguments */
        ps_ast_node_argument *args; /** @brief arguments, NULL if no arguments        */
    } ps_ast_call;

    typedef struct s_ps_ast_argument
    {
        bool is_reference; /** @brief true if argument is passed by reference */
        union {
            ps_ast_node *expression;       /** @brief expression argument (by value)               */
            ps_ast_node *symbol_reference; /** @brief variable argument (by value or by reference) */
        };
    } ps_ast_node_argument;

    typedef struct s_ps_ast_assignment
    {
        PS_AST_NODE_COMMON
        ps_ast_node *lvalue;     /** @brief variable being assigned to       */
        ps_ast_node *expression; /** @brief expression to assign to variable */
    } ps_ast_assignment;

    /** @brief Expression: literal value */
    typedef struct s_ps_ast_value
    {
        PS_AST_NODE_COMMON
        ps_value value;
    } ps_ast_value;

    /** @brief Variable or Lvalue: simple variable */
    /** @example I, Total, ...  */
    typedef struct s_ps_ast_variable_simple
    {
        PS_AST_NODE_COMMON
        ps_symbol *variable; /** @brief Symbol being referenced */
    } ps_ast_variable_simple;

    /** @brief Variable or Lvalue: array           */
    /** @example A[I], A[I, J, K], ... */
    typedef struct s_ps_ast_variable_array
    {
        PS_AST_NODE_COMMON
        ps_symbol *variable;   /** @brief Symbol being referenced                */
        size_t n_indexes;      /** @brief For array access, 0 if not an array    */
        ps_ast_node **indexes; /** @brief For array access, NULL if not an array */
    } ps_ast_variable_array;

    typedef struct s_ps_ast_unary_operation
    {
        PS_AST_NODE_COMMON
        ps_operator_unary operator;
        ps_ast_node *operand;
    } ps_ast_unary_operation;

    typedef struct s_ps_ast_binary_operation
    {
        PS_AST_NODE_COMMON
        ps_operator_binary operator; /** @brief Binary operator */
        ps_ast_node *left;           /** @brief Left operand */
        ps_ast_node *right;          /** @brief Right operand */
    } ps_ast_binary_operation;

    // clang-format off
    #define PS_AST_NODE_SIZE                   sizeof(ps_ast_node)
    #define PS_AST_NODE_BLOCK_SIZE             sizeof(ps_ast_block)
    #define PS_AST_NODE_STATEMENT_LIST_SIZE    sizeof(ps_ast_statement_list)
    #define PS_AST_NODE_IF_SIZE                sizeof(ps_ast_if)
    #define PS_AST_NODE_WHILE_SIZE             sizeof(ps_ast_while);
    #define PS_AST_NODE_REPEAT_SIZE            sizeof(ps_ast_repeat)
    #define PS_AST_NODE_FOR_SIZE               sizeof(ps_ast_for)
    #define PS_AST_NODE_CALL_SIZE              sizeof(ps_ast_call)
    #define PS_AST_NODE_ARGUMENT_SIZE          sizeof(ps_ast_node_argument)
    #define PS_AST_NODE_ASSIGNMENT_SIZE        sizeof(ps_ast_assignment)
    #define PS_AST_NODE_VALUE_SIZE             sizeof(ps_ast_value)
    #define PS_AST_NODE_UNARY_OPERATION_SIZE   sizeof(ps_ast_unary_operation)
    #define PS_AST_NODE_BINARY_OPERATION_SIZE  sizeof(ps_ast_binary_operation)
    #define PS_AST_NODE_VARIABLE_SIMPLE_SIZE   sizeof(ps_ast_variable_simple)
    #define PS_AST_NODE_VARIABLE_ARRAY_SIZE    sizeof(ps_ast_variable_array)
    // clang-format on

    /** @brief Create a new AST node of the given kind */
    ps_ast_node *ps_ast_create_node(ps_ast_node_group group, ps_ast_node_kind kind, uint16_t line, uint16_t column, size_t size);

    ps_ast_node *ps_ast_create_block(uint16_t line, uint16_t column, ps_ast_node_kind kind, char *name);
    ps_ast_node *ps_ast_create_statement_list(uint16_t line, uint16_t column, size_t count);
    ps_ast_node *ps_ast_create_assignment(uint16_t line, uint16_t column, ps_ast_node *variable,
                                          ps_ast_node *expression);
    ps_ast_node *ps_ast_create_if(uint16_t line, uint16_t column, ps_ast_node *condition,
                                  ps_ast_statement_list *then_branch, ps_ast_statement_list *else_branch);
    ps_ast_node *ps_ast_create_while(uint16_t line, uint16_t column, ps_ast_node *condition,
                                     ps_ast_statement_list *body);
    ps_ast_node *ps_ast_create_repeat(uint16_t line, uint16_t column, ps_ast_statement_list *body,
                                      ps_ast_node *condition);
    ps_ast_node *ps_ast_create_for(uint16_t line, uint16_t column, ps_ast_node *variable, ps_ast_node *start,
                                   ps_ast_node *end, int step, ps_ast_statement_list *body);
    ps_ast_node *ps_ast_create_procedure_call(uint16_t line, uint16_t column, ps_symbol *executable, size_t n_args,
                                              ps_ast_node_argument *args);
    ps_ast_node *ps_ast_create_function_call(uint16_t line, uint16_t column, ps_symbol *executable, size_t n_args,
                                             ps_ast_node_argument *args);
    ps_ast_node *ps_ast_create_unary_operation(uint16_t line, uint16_t column, ps_operator_unary operator,
                                               ps_ast_node * operand);
    ps_ast_node *ps_ast_create_binary_operation(uint16_t line, uint16_t column, ps_operator_binary operator,
                                                ps_ast_node * left, ps_ast_node *right);
    ps_ast_node *ps_ast_create_value(uint16_t line, uint16_t column, ps_value value);
    ps_ast_node *ps_ast_create_variable_simple(uint16_t line, uint16_t column, ps_symbol *variable);
    ps_ast_node *ps_ast_create_variable_array(uint16_t line, uint16_t column, ps_symbol *symbol, size_t n_indexes,
                                              ps_ast_node *indexes);
    ps_ast_node *ps_ast_create_lvalue_simple(uint16_t line, uint16_t column, ps_symbol *variable);
    ps_ast_node *ps_ast_create_lvalue_array(uint16_t line, uint16_t column, ps_symbol *symbol, size_t n_indexes,
                                            ps_ast_node *indexes);

    /** @brief Free an AST node and all its children */
    ps_ast_node *ps_ast_free_node(ps_ast_node *node);

    ps_ast_node *ps_ast_free_block(ps_ast_node *node);
    ps_ast_node *ps_ast_free_statement_list(ps_ast_node *node);
    ps_ast_node *ps_ast_free_assignment(ps_ast_node *node);
    ps_ast_node *ps_ast_free_if(ps_ast_node *node);
    ps_ast_node *ps_ast_free_while(ps_ast_node *node);
    ps_ast_node *ps_ast_free_repeat(ps_ast_node *node);
    ps_ast_node *ps_ast_free_for(ps_ast_node *node);
    ps_ast_node *ps_ast_free_procedure_call(ps_ast_node *node);
    ps_ast_node *ps_ast_free_unary_operation(ps_ast_node *node);
    ps_ast_node *ps_ast_free_binary_operation(ps_ast_node *node);
    ps_ast_node *ps_ast_free_function_call(ps_ast_node *node);
    ps_ast_node *ps_ast_free_value(ps_ast_node *node);
    ps_ast_node *ps_ast_free_variable_simple(ps_ast_node *node);
    ps_ast_node *ps_ast_free_variable_array(ps_ast_node *node);
    ps_ast_node *ps_ast_free_lvalue_simple(ps_ast_node *node);
    ps_ast_node *ps_ast_free_lvalue_array(ps_ast_node *node);

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_H */

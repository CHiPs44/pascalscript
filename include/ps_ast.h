/*
    This file is part of the PascalScript Pascal interpreter.
    SPDX-FileCopyrightText: 2024 Christophe "CHiPs" Petit <chips44@gmail.com>
    SPDX-License-Identifier: LGPL-3.0-or-later
*/

#ifndef _PS_AST_H
#define _PS_AST_H

#include "ps_signature.h"
#include "ps_symbol.h"
#include "ps_symbol_table.h"
#include "ps_value.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /** @brief Abstract Syntax Tree node kind */
    typedef enum enum_ps_ast_node_kind
    {
        PS_AST_PROGRAM,          /** @brief Block: PROGRAM                                  */
        PS_AST_PROCEDURE,        /** @brief Block: PROCEDURE                                */
        PS_AST_FUNCTION,         /** @brief Block: FUNCTION                                 */
        PS_AST_UNIT,             /** @brief Block: UNIT                                     */
        PS_AST_STATEMENT_LIST,   /** @brief List of statements                              */
        PS_AST_ASSIGNMENT,       /** @brief Assignment (:=) statement                       */
        PS_AST_IF,               /** @brief IF statement                                    */
        PS_AST_WHILE,            /** @brief WHILE statement                                 */
        PS_AST_REPEAT,           /** @brief REPEAT statement                                */
        PS_AST_FOR,              /** @brief FOR statement                                   */
        PS_AST_PROCEDURE_CALL,   /** @brief PROCEDURE call                                  */
        PS_AST_EXPRESSION,       /** @brief Expression                                      */
        PS_AST_UNARY_OPERATION,  /** @brief Unary operation                                 */
        PS_AST_BINARY_OPERATION, /** @brief Binary operation                                */
        PS_AST_FUNCTION_CALL,    /** @brief FUNCTION call                                   */
        PS_AST_VALUE,            /** @brief Value node: integer, real, string, boolean, ... */
        PS_AST_SYMBOL_REFERENCE, /** @brief Symbol reference: variable or constant          */
    } ps_ast_node_kind;

    /** @brief Abstract Syntax Tree node */
    typedef struct s_ps_ast_node
    {
        ps_ast_node_kind kind;
        union {
            ps_ast_node_block *block;
            ps_ast_node_statement_list *statement_list;
            ps_ast_node_assignment *assignment;
            ps_ast_node_if *if_statement;
            ps_ast_node_while *while_statement;
            ps_ast_node_repeat *repeat_statement;
            ps_ast_node_for *for_statement;
            ps_ast_node_call *procedure_call;
            ps_ast_node_unary_operation *unary_operation;
            ps_ast_node_binary_operation *binary_operation;
            ps_ast_node_expression *expression;
            ps_ast_node_call *function_call;
            ps_ast_node_value *value;
            ps_ast_node_symbol_reference *symbol_reference;
        };
    } ps_ast_node;

    /** @brief Block is a program, procedure, function or unit */
    /** @details Units may be separated as they are special cases with interface and implementation */
    typedef struct s_ps_ast_node_block
    {
        ps_identifier name;                     /** @brief Every block has a name                                */
        ps_formal_signature *signature;         /** @brief Only for procedures and functions, empty otherwise    */
        ps_symbol *result_type;                 /** @brief Only for functions, NULL otherwise                    */
        size_t n_vars;                          /** @brief Number of variables to allocate at startup            */
        ps_symbol_table *symbols;               /** @brief Constants, types, variables, procedures and functions */
        ps_ast_node_statement_list *statements; /** @brief Statements in this block                              */
        size_t n_executables;                   /** @brief Number of declared procedures and functions           */
        ps_ast_node **executables;              /** @brief declarations of procedures and functions              */
    } ps_ast_node_block;

    typedef struct ps_ast_node_statement_list
    {
        size_t count;             /** @brief Number of statements  */
        ps_ast_node **statements; /** @brief NULL if no statements */
    } ps_ast_node_statement_list;

    typedef struct s_ps_ast_node_if
    {
        ps_ast_node *condition;   /** @brief If condition                                              */
        ps_ast_node *then_branch; /** @brief Statements to execute if condition is true, can be empty  */
        ps_ast_node *else_branch; /** @brief Statements to execute if condition is false, can be empty */
    } ps_ast_node_if;

    typedef struct s_ps_ast_node_while
    {
        ps_ast_node *condition; /** @brief Repeat while condition is true                */
        ps_ast_node *body;      /** @brief Statements to execute while condition is true */
    } ps_ast_node_while;

    typedef struct s_ps_ast_node_repeat
    {
        ps_ast_node *body;      /** @brief Statements to execute at least once */
        ps_ast_node *condition; /** @brief Repeat until condition is true      */
    } ps_ast_node_repeat;

    typedef struct s_ps_ast_node_for
    {
        ps_ast_node_symbol_reference *variable; /** @brief Loop variable                                         */
        ps_ast_node_expression *start;          /** @brief Start value                                           */
        ps_ast_node_expression *end;            /** @brief End value                                             */
        int step;                               /** @brief Step value: 1 for "TO", -1 for "DOWNTO"               */
        ps_ast_node *body;                      /** @brief Statements to execute for each value of loop variable */
    } ps_ast_node_for;

    typedef struct s_ps_ast_node_call
    {
        ps_symbol *executable;      /** @brief procedure of function being called     */
        size_t n_args;              /** @brief number of arguments, 0 if no arguments */
        ps_ast_node_argument *args; /** @brief arguments, NULL if no arguments        */
    } ps_ast_node_call;

    typedef struct s_ps_ast_node_argument
    {
        bool is_reference; /** @brief true if argument is passed by reference */
        union {
            ps_ast_node_expression *expression;             /** @brief expression argument (by value)               */
            ps_ast_node_symbol_reference *symbol_reference; /** @brief variable argument (by value or by reference) */
        };
    } ps_ast_node_argument;

    typedef struct s_ps_ast_node_assignment
    {
        ps_ast_node *variable;   /** @brief variable being assigned to       */
        ps_ast_node *expression; /** @brief expression to assign to variable */
    } ps_ast_node_assignment;

    typedef struct s_ps_ast_node_expression
    {
        ps_ast_node_kind kind; /** @brief value, variable reference, constant reference or function call */
        union {
            ps_ast_node_value *value;                       /** @brief for literal value    */
            ps_ast_node_symbol_reference *symbol;           /** @brief for symbol reference */
            ps_ast_node_unary_operation *unary_operation;   /** @brief for unary operation  */
            ps_ast_node_binary_operation *binary_operation; /** @brief for binary operation */
            ps_ast_node_call *function_call;                /** @brief for function call    */
        };
    } ps_ast_node_expression;

    /** @brief literal value */
    typedef struct s_ps_ast_node_value
    {
        ps_value value;
    } ps_ast_node_value;

    /** @brief Symbol reference: variable or constant */
    /** @example I, A[I], A[I, J, K], ...             */
    typedef struct s_ps_ast_node_symbol_reference
    {
        ps_symbol *symbol;               /** @brief Symbol being referenced                */
        size_t n_indexes;                /** @brief For array access, 0 if not an array    */
        ps_ast_node_expression *indexes; /** @brief For array access, NULL if not an array */
    } ps_ast_node_symbol_reference;

    typedef enum e_ps_ast_node_unary_operator
    {
        PS_OP_NEG,
        PS_OP_NOT,
    } ps_ast_node_unary_operator;

    typedef struct s_ps_ast_node_unary_operation
    {
        ps_ast_node_unary_operator operator;
        ps_ast_node_expression *operand;
    } ps_ast_node_unary_operation;

    // clang-format off
    typedef enum e_ps_ast_node_binary_operator
    {
        // additive / terms
        PS_OP_ADD, PS_OP_SUB, PS_OP_OR, PS_OP_XOR,
        // multiply / factors
        PS_OP_MUL, PS_OP_DIV, PS_OP_DIV_REAL, PS_OP_MOD, PS_OP_AND, PS_OP_SHL, PS_OP_SHR,
        // comparison
        PS_OP_EQ, PS_OP_GE, PS_OP_GT, PS_OP_LE, PS_OP_LT, PS_OP_NE,
    } ps_ast_node_binary_operator;
    // clang-format on

    typedef struct s_ps_ast_node_binary_operation
    {
        ps_ast_node_expression *left;
        ps_ast_node_binary_operator operator;
        ps_ast_node_expression *right;
    } ps_ast_node_binary_operation;

    // clang-format off
    #define PS_AST_NODE_SIZE                  sizeof(ps_ast_node)
    #define PS_AST_NODE_BLOCK_SIZE            sizeof(ps_ast_node_block)
    #define PS_AST_NODE_STATEMENT_LIST_SIZE   sizeof(ps_ast_node_statement_list)
    #define PS_AST_NODE_IF_SIZE               sizeof(ps_ast_node_if)
    #define PS_AST_NODE_WHILE_SIZE            sizeof(ps_ast_node_while);
    #define PS_AST_NODE_REPEAT_SIZE           sizeof(ps_ast_node_repeat)
    #define PS_AST_NODE_FOR_SIZE              sizeof(ps_ast_node_for)
    #define PS_AST_NODE_CALL_SIZE             sizeof(ps_ast_node_call)
    #define PS_AST_NODE_ARGUMENT_SIZE         sizeof(ps_ast_node_argument)
    #define PS_AST_NODE_ASSIGNMENT_SIZE       sizeof(ps_ast_node_assignment)
    #define PS_AST_NODE_EXPRESSION_SIZE       sizeof(ps_ast_node_expression)
    #define PS_AST_NODE_VALUE_SIZE            sizeof(ps_ast_node_value)
    #define PS_AST_NODE_SYMBOL_REFERENCE_SIZE sizeof(ps_ast_node_symbol_reference)
    #define PS_AST_NODE_UNARY_OPERATION_SIZE  sizeof(ps_ast_node_unary_operation)
    #define PS_AST_NODE_BINARY_OPERATION_SIZE sizeof(ps_ast_node_binary_operation)
    // clang-format on

    /** @brief Create a new AST node of the given kind */
    ps_ast_node *ps_ast_create_node(ps_ast_node_kind kind);

    /** @brief Free an AST node and all its children */
    bool ps_ast_free_node(ps_ast_node *node);

    /** @brief Visit an AST node and execute it if it's a statement or evaluate it if it's an expression */
    bool ps_ast_visit_node(ps_ast_node *node);

#ifdef __cplusplus
}
#endif

#endif /* _PS_AST_H */

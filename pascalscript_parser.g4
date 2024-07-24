/* ******************** PascalScript ANTLR4 ******************** */

// npm i -g --save-dev antlr-format-cli
//  antlr-format -v pascalscript_parser.g4
// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar pascalscript_parser;

options {
    // needs antlr4 >= 4.10
    caseInsensitive = true;
    tokenVocab = pascalscript_lexer;
}

/* ******************** PROGRAM ******************** */

pascalFile
    : pascalProgram
    // | pascalUnit
    ;

pascalProgram
    : PROGRAM IDENTIFIER SEMI_COLON
    //   usesBlock?
    pascalHeader instructionBlock DOT EOF
    ;

/* NB: units disabled for now
pascalUnit
    : UNIT IDENTIFIER SEMI_COLON
      INTERFACE
        usesBlock?
        interfaceBlock*
      IMPLEMENTATION
        usesBlock?
        implementationBlock
      END DOT
    ;
usesBlock
    : USES identifierList SEMI_COLON
    ;
interfaceBlock
    : constBlock 
    | typeBlock 
    | varBlock 
    | procedureDeclarationHeader 
    | functionDeclarationHeader
    ;
implementationBlock
    : pascalBlock
      initializationBlock?
      implementationBlock?
    ;
initializationBlock
    : INITIALIZATION 
    | (instruction | instructionBlock SEMI_COLON)
    ;
finalizationBlock
    : FINALIZATION 
    | (instruction | instructionBlock SEMI_COLON)
    ;
*/
pascalHeader
    : pascalBlock*
    ;

pascalBlock
    : constBlock
    // | typeBlock
    | varBlock
    // | procedureFunctionBlock
    ;

instructionBlock
    : BEGIN (instruction)* END
    ;

instruction
    : assignment
    // | ifBlock
    // | repeatBlock
    // | whileBlock
    // | forBlock
    | procedureCall
    ;

assignment
    : variableReference DOT_COLON expression SEMI_COLON
    ;

// ifBlock
//     :  IF expression THEN
//         ( instruction | instructionBlock ) SEMI_COLON
//         ( ELSE ( instruction | instructionBlock ) SEMI_COLON )?
//     ;
// repeatBlock
//     : REPEAT ( instruction )* UNTIL expression SEMI_COLON
//     ;
// whileBlock
//     : WHILE expression DO (instruction | instructionBlock SEMI_COLON)
//     ;
// forBlock
//     : FOR variableReference DOT_COLON expression ( TO | DOWNTO ) expression
//         instruction | instructionBlock SEMI_COLON
//     ;
procedureCall
    : ( /*IDENTIFIER | */ WRITE | WRITELN) (LEFT_PARENTHESIS parameterList RIGHT_PARENTHESIS)? SEMI_COLON
    ;

// functionCall
//     : IDENTIFIER ( LEFT_PARENTHESIS parameterList RIGHT_PARENTHESIS )?
//     ;
parameterList
    : parameter (COMMA parameter)*
    ;

parameter
    : expression
    | variableReference
    | constantReference
    ;

/* ******************** LABELS & GOTO ******************** */

/*
label
    : DECIMAL_DIGIT_SEQUENCE 
    | IDENTIFIER
    ;
labelList
    : label ( COMMA label )*
    ;
labelDeclaration
    : LABEL labelList SEMI_COLON
    ;
gotoBlock
    : GOTO label SEMI_COLON
    ;
*/

/* ******************** CONSTANTS ******************** */

/*
    NB: no expressions in constants, only values
        no typed constants, only numbers, chars & strings

    Const
        One = 1;
        KibiByte = 1024;
        MaxWord = $FFFF;
        Pi = 3.141592653589793;
        Star = '*';
        HelloWorld = 'Hello, world!';
*/

constBlock
    : CONST constantDeclaration+
    ;

constantDeclaration
    // : IDENTIFIER EQUAL constantValue SEMI_COLON
    : IDENTIFIER EQUAL expression SEMI_COLON
    ;

/*
constantValue
    : SIGN UNSIGNED_INTEGER_VALUE
    | UNSIGNED_INTEGER_VALUE
    | SIGN UNSIGNED_REAL_VALUE
    | UNSIGNED_REAL_VALUE
    | CHARACTER_VALUE
    | COMPOSED_STRING_VALUE
    | IDENTIFIER
    ;
*/

/* ******************** TYPES ******************** */

/*
    Type
        TextLine = String[80];
*/

/*
typeBlock
    : TYPE typeDeclaration+
    ;
typeDeclaration
    : IDENTIFIER EQUAL typeDefinition SEMI_COLON
    ;
*/
typeReference
    : scalarType
    // | stringType
    // : IDENTIFIER
    // | typeDefinition
    ;

/*
typeDefinition
    : REAL
    | scalarType
    | stringType
    // | arrayType
    // | recordType
    ;
*/
scalarType
    : INTEGER
    | CARDINAL
    | BOOLEAN
    | CHAR
    // | enumType
    // | subrangeType
    ;

/*
enumType
    : LEFT_PARENTHESIS IDENTIFIER ( COMMA IDENTIFIER )* RIGHT_PARENTHESIS
    ;
subrangeType
    : subrangeLimit DOT_DOT subrangeLimit
    ;
subrangeLimit
    : SIGN? UNSIGNED_INTEGER
    | SIGN? IDENTIFIER
    ;
*/
/*
stringSize
    : UNSIGNED_INTEGER | IDENTIFIER
    ;
*/
// stringType
//     : STRING
//     // | STRING ( LEFT_BRACKET stringSize RIGHT_BRACKET )?
//     ;

/* ******************** ARRAYS ******************** */

/*
arrayType
    : ARRAY LEFT_BRACKET arrayLimits RIGHT_BRACKET OF typeDefinition
    ;
arrayLimits
    : arrayLimit ( COMMA arrayLimit )*
    ;
arrayLimit
    : scalarType
    | IDENTIFIER
    ;
recordType
    : RECORD ( identifierList COLON typeDefinition SEMI_COLON )* END
    ;
*/

/* ******************** VARIABLES ******************** */

varBlock
    : VAR variableDeclaration+
    ;

variableDeclaration
    : identifierList COLON typeReference SEMI_COLON
    ;

identifierList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

/* ******************** PROCEDURES & FUNCTIONS ******************** */

/*
procedureFunctionBlock
    : ( procedureDeclaration | functionDeclaration )+
    ;
procedureDeclaration
    : procedureDeclarationHeader
      procedureOrFunctionBody SEMI_COLON
    ;
procedureDeclarationHeader
    : PROCEDURE IDENTIFIER ( LEFT_PARENTHESIS parameterDeclarationList RIGHT_PARENTHESIS )? SEMI_COLON
    ;
functionDeclaration
    : functionDeclarationHeader
    procedureOrFunctionBody SEMI_COLON
    ;
functionDeclarationHeader
    : FUNCTION IDENTIFIER ( LEFT_PARENTHESIS parameterDeclarationList RIGHT_PARENTHESIS )? COLON typeReference SEMI_COLON
    ;
parameterDeclaration
    : VAR? identifierList COLON typeReference
    ;
parameterDeclarationList
    : ( parameterDeclaration ( COMMA parameterDeclaration )* )?
    ;
procedureOrFunctionBody
    : pascalHeader instructionBlock
    ;
*/

/* ******************** EXPRESSIONS ******************** */

expression
    : unaryOperator expression
    // : expression relationalOperator expression
    | expression multiplicativeOperator expression
    | expression additiveOperator expression
    | expression relationalOperator expression
    | UNSIGNED_INTEGER_VALUE
    | UNSIGNED_REAL_VALUE
    | CHARACTER_VALUE
    | BOOLEAN_VALUE
    | COMPOSED_STRING_VALUE
    | variableReference
    | constantReference
    // | functionCall
    // | unaryOperator expression
    | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
    ;

unaryOperator
    : PLUS
    | MINUS
    | NOT
    ;

relationalOperator
    : EQUAL
    | NOT_EQUAL
    | LESS_THAN
    | LESS_OR_EQUAL
    | GREATER_THAN
    | GREATER_OR_EQUAL
    ;

additiveOperator
    : PLUS
    | MINUS
    | OR
    | XOR
    ;

multiplicativeOperator
    : STAR
    | SLASH
    | DIV
    | MOD
    | AND
    | SHL
    | SHR
    ;

variableReference
    : IDENTIFIER
    // | IDENTIFIER LEFT_BRACKET expression ( COMMA expression )* RIGHT_BRACKET
    ;

constantReference
    : IDENTIFIER
    ;

// EOF

/* ******************** PascalScript ANTLR4 ******************** */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar pascalscript;

options {
    // needs antlr4 >= 4.10, Unbuntu 22.04 has 4.7
    // caseInsensitive = true;
}

/* ******************** PROGRAM ******************** */

/* NB: units disabled for now
pascalFile
    : pascalProgram
    | pascalUnit
    ;
*/
pascalProgram
    : PROGRAM IDENTIFIER SEMI_COLON
    //   usesBlock?
      pascalHeader
      instructionBlock DOT
    ;
/*
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
pascalBlock
    : constBlock
    | typeBlock
    | varBlock
    | procedureFunctionBlock
    ;
pascalHeader
    : pascalBlock*
    ;
instructionBlock
    : BEGIN ( instruction )* END
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
/*
ifBlock
    :  IF expression THEN
        ( instruction | instructionBlock ) SEMI_COLON
        ( ELSE ( instruction | instructionBlock ) SEMI_COLON )?
    ;
repeatBlock
    : REPEAT ( instruction )* UNTIL expression SEMI_COLON
    ;
whileBlock
    : WHILE expression DO (instruction | instructionBlock SEMI_COLON)
    ;
forBlock
    : FOR variableReference DOT_COLON expression ( TO | DOWNTO ) expression
        instruction | instructionBlock SEMI_COLON
    ;
*/
procedureCall
    : ( IDENTIFIER | WRITE | WRITELN ) ( LEFT_PARENTHESIS parametersList RIGHT_PARENTHESIS )? SEMI_COLON
    ;
/*
functionCall
    : IDENTIFIER ( LEFT_PARENTHESIS parametersList RIGHT_PARENTHESIS )?
    ;
*/
parametersList
    : parameter ( COMMA parameter )*
    ;
parameter
    : expression
    ;

/* ******************** NUMBERS ******************** */

BINARY_DIGIT_SEQUENCE
    : (BINARY_DIGIT )+
    ;

OCTAL_DIGIT_SEQUENCE
    : ( OCTAL_DIGIT )+
    ;

DECIMAL_DIGIT_SEQUENCE
    : ( DECIMAL_DIGIT )+
    ;

HEXADECIMAL_DIGIT_SEQUENCE
    : ( HEXADECIMAL_DIGIT )+
    ;

UNSIGNED_INTEGER
    : DECIMAL_DIGIT_SEQUENCE 
    | PERCENT BINARY_DIGIT_SEQUENCE 
    | AMPERSAND OCTAL_DIGIT_SEQUENCE 
    | DOLLAR HEXADECIMAL_DIGIT_SEQUENCE
    ;

SIGN
    : PLUS 
    | MINUS
    ;

SIGNED_INTEGER
    : SIGN? UNSIGNED_INTEGER
    ;

SCALE_FACTOR
    : ( 'E' | 'e' ) SIGN? DECIMAL_DIGIT_SEQUENCE
    ;

UNSIGNED_REAL
    : DECIMAL_DIGIT_SEQUENCE ( DOT DECIMAL_DIGIT_SEQUENCE )? SCALE_FACTOR?
    ;

SIGNED_REAL
    : SIGN? UNSIGNED_REAL
    ;

/* ******************** IDENTIFIERS ******************** */

UPPERCASE_LETTER
    : 'A'..'Z'
    ;
LOWERCASE_LETTER
    : 'a'..'z'
    ;
LETTER
    : LOWERCASE_LETTER 
    | UPPERCASE_LETTER 
    ;
IDENTIFIER
    : LETTER | UNDERSCORE ( LETTER | DECIMAL_DIGIT | UNDERSCORE )*
    ;
identifierList
    : IDENTIFIER ( COMMA IDENTIFIER )*
    ;

/* ******************** BOOLEANS ******************** */
BOOLEAN_VALUE
    : FALSE
    | TRUE
    ;

/* ******************** LABEL & GOTO ******************** */
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
    : CONST ( constantDeclaration SEMI_COLON )+
    ;
constantDeclaration
    : IDENTIFIER EQUAL constantValue SEMI_COLON
    ;
constantValue
    : UNSIGNED_INTEGER
    | SIGNED_INTEGER
    | UNSIGNED_REAL 
    | SIGNED_REAL
    | QUOTED_CHAR
    | QUOTED_STRING
    ;

/* ******************** TYPES ******************** */

/*
    Const
        MaxLineLength = 80;
    Type
        TextLine = String[MaxLineLength];
*/

typeBlock
    : TYPE typeDeclaration+
    ;
typeDeclaration
    : IDENTIFIER EQUAL typeDefinition SEMI_COLON
    ;
typeReference
    : IDENTIFIER
    | typeDefinition
    ;
typeDefinition
    : REAL
    | scalarType
    | stringType
    // | arrayType
    // | recordType
    ;
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
stringSize
    : UNSIGNED_INTEGER | IDENTIFIER
    ;
stringType
    : STRING ( LEFT_BRACKET stringSize RIGHT_BRACKET )?
    ;
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
    : VAR ( variableDeclaration SEMI_COLON )+
    ;
variableDeclaration
    : identifierList COLON typeReference
    ;

/* ******************** PROCEDURES & FUNCTIONS ******************** */

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

/* ******************** EXPRESSIONS ******************** */
unaryOperator
    : PLUS | MINUS | NOT
    ;
additiveOperator
    : PLUS | MINUS | OR | XOR
    ;
multiplicativeOperator
    : STAR | SLASH | DIV | MOD | AND | SHL | SHR
    ;
relationalOperator
    : EQUAL | NOT_EQUAL | LESS_THAN | LESS_OR_EQUAL | GREATER_THAN | GREATER_OR_EQUAL;
variableReference
    : IDENTIFIER
    | IDENTIFIER LEFT_BRACKET expression (  COMMA expression )* RIGHT_BRACKET
    ;
constantReference
    : IDENTIFIER
    ;
expression
    : unaryOperator expression
    | expression relationalOperator     expression
    | expression additiveOperator       expression
    | expression multiplicativeOperator expression
    | term
    | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
    ;
term
    : UNSIGNED_INTEGER
    | UNSIGNED_REAL
    | CHARACTER_VALUE
    | BOOLEAN_VALUE
    | COMPOSED_STRING
    | variableReference 
    | IDENTIFIER
    | SIGN expression
    // | functionCall
    ;

/* ******************** LEXEMES ******************** */

// NB: lexer rules begin with / are uppercase

// Keywords
PROGRAM:        'PROGRAM';
/*
UNIT:           'UNIT';
USES:           'USES';
INTERFACE:      'INTERFACE';
IMPLEMENTATION: 'IMPLEMENTATION';
INITIALIZATION: 'INITIALIZATION';
FINALIZATION:   'FINALIZATION';
*/
BEGIN:          'BEGIN';
END:            'END';
IF:             'IF';
THEN:           'THEN';
ELSE:           'ELSE';
REPEAT:         'REPEAT';
UNTIL:          'UNTIL';
WHILE:          'WHILE';
DO:             'DO';
FOR:            'FOR';
TO:             'TO';
DOWNTO:         'DOWNTO';
WRITE:          'WRITE';
WRITELN:        'WRITELN';
FALSE:          'FALSE';
TRUE:           'TRUE';
LABEL:          'LABEL';
GOTO:           'GOTO';
INTEGER:        'INTEGER';
CARDINAL:       'CARDINAL';
BOOLEAN:        'BOOLEAN';
CHAR:           'CHAR';
STRING:         'STRING';
REAL:           'REAL';
ARRAY:          'ARRAY';
OF:             'OF';
RECORD:         'RECORD';
CONST:          'CONST';
TYPE:           'TYPE';
VAR:            'VAR';
PROCEDURE:      'PROCEDURE';
FUNCTION:       'FUNCTION';
// Operators
NOT:        'NOT';
OR:         'OR';
XOR:        'XOR';
DIV:        'DIV';
MOD:        'MOD';
AND:        'AND';
SHL:        'SHL';
SHR:        'SHR';
// Symbols
DOT_COLON:          ':=';   // assignment
AT_SIGN:            '@';    // address of
CARET:              '^';    // pointer to
COLON:              ':';    // various uses
COMMA:              ',';    // various uses
DOT_DOT:            '..';   // ranges
DOT:                '.';    // various uses
LEFT_BRACKET:       '[';    // array access
LEFT_PARENTHESIS:   '(';    // various uses
RIGHT_BRACKET:      ']';    // array access
RIGHT_PARENTHESIS:  ')';    // various uses
SEMI_COLON:         ';';    // various uses
UNDERSCORE:         '_';    // identifier part
QUOTE:              '\'';   // char/string delimiter
PERCENT:            '%';    // binary prefix
AMPERSAND:          '&';    // octal prefix
DOLLAR:             '$';    // hexadecimal prefix
// Arithmetic operators
PLUS:               '+';    // addition
MINUS:              '-';    // substraction / negation
STAR:               '*';    // multiplication
SLASH:              '/';    // division (real)
// Comparison operators
EQUAL:              '=';
NOT_EQUAL:          '<>';
LESS_THAN:          '<';
LESS_OR_EQUAL:      '<=';
GREATER_THAN:       '>';
GREATER_OR_EQUAL:   '>=';
// Numbers
BINARY_DIGIT:       '0'..'1';
OCTAL_DIGIT:        '0'..'7';
DECIMAL_DIGIT:      '0'..'9';
HEXADECIMAL_DIGIT:  DECIMAL_DIGIT | 'a'..'f' | 'A'..'F';

/* ******************** CHARS & STRINGS ******************** */

// Any printable character from C0 & C1 Unicode blocks except ' (\u0027)
CHAR_VALUE
    : '\u0020'..'\u0026'
    | '\u0028'..'\u007e'
    | '\u00a0'..'\u00ff'
    | '\'\''
    ;
// #13 as CR, #10 as LF, #$1B as ESC, ...
CONTROL_CHAR
    : '#' UNSIGNED_INTEGER
    ;
// Single character
QUOTED_CHAR
    : QUOTE CHAR_VALUE QUOTE
    ;
CHARACTER_VALUE
    : QUOTED_CHAR
    | CONTROL_CHAR
    ;
QUOTED_STRING
    : QUOTE ( CHAR_VALUE )* QUOTE
    ;
COMPOSED_STRING
    : ( QUOTED_STRING | CONTROL_CHAR )+
    ;

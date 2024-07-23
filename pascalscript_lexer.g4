/* ******************** PascalScript ANTLR4 ******************** */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

lexer grammar pascalscript_lexer;

options {
    // needs antlr4 >= 4.10
    caseInsensitive = true;
}

/* ******************** WHITESPACE ******************** */

WS: [ \n\t\r]+ -> skip;

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
DIV:        'DIV'; // division (integer)
MOD:        'MOD';
AND:        'AND';
SHL:        'SHL';
SHR:        'SHR';

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

/* ******************** IDENTIFIER ******************** */

LETTER
    : 'A'..'Z'
    ;
IDENTIFIER
    : ( LETTER | UNDERSCORE ) ( LETTER | DECIMAL_DIGIT | UNDERSCORE )*
    ;

/* ******************** NUMBERS ******************** */

UNSIGNED_INTEGER_VALUE
    : DECIMAL_DIGIT_SEQUENCE 
    | PERCENT BINARY_DIGIT_SEQUENCE 
    | AMPERSAND OCTAL_DIGIT_SEQUENCE 
    | DOLLAR HEXADECIMAL_DIGIT_SEQUENCE
    ;
UNSIGNED_REAL_VALUE
    : DECIMAL_DIGIT_SEQUENCE DOT DECIMAL_DIGIT_SEQUENCE ( 'E'? SIGN? DECIMAL_DIGIT_SEQUENCE )?
    ;
DECIMAL_DIGIT_SEQUENCE
    : DECIMAL_DIGIT+
    ;
BINARY_DIGIT_SEQUENCE
    : BINARY_DIGIT+
    ;
OCTAL_DIGIT_SEQUENCE
    : OCTAL_DIGIT+
    ;
HEXADECIMAL_DIGIT_SEQUENCE
    : HEXADECIMAL_DIGIT+
    ;
SIGN
    : PLUS 
    | MINUS
    ;
DECIMAL_DIGIT:      '0'..'9';
BINARY_DIGIT:       '0'..'1';
OCTAL_DIGIT:        '0'..'7';
HEXADECIMAL_DIGIT:  '0'..'9' | 'A'..'F';

/* ******************** CHARS & STRINGS ******************** */

CHARACTER_VALUE
    : QUOTED_CHAR
    | CONTROL_CHAR
    ;
COMPOSED_STRING_VALUE
    : ( QUOTED_STRING | CONTROL_CHAR )+
    ;
// Any printable character from C0 & C1 Unicode blocks except ' (#39 / \u0027)
CHAR_VALUE
    // C0 / ASCII
    : '\u0020'..'\u0026'
    | '\u0028'..'\u007e'
    // C1 / LATIN1
    | '\u00a0'..'\u00ff'
    | '\'\''
    ;
// #13 as CR, #10 as LF, #$1B as ESC, ...
CONTROL_CHAR
    : '#' UNSIGNED_INTEGER_VALUE
    ;
// Single character
QUOTED_CHAR
    : QUOTE CHAR_VALUE QUOTE
    ;
QUOTED_STRING
    : QUOTE ( CHAR_VALUE )* QUOTE
    ;
/* ******************** BOOLEANS ******************** */

BOOLEAN_VALUE
    : FALSE
    | TRUE
    ;

/* ******************** WHITESPACE / COMMENTS ******************** */

ANY_CHAR: .;
COMMENT1: '(*' ANY_CHAR* '*)' -> skip;
COMMENT2: '{' ANY_CHAR* '}' -> skip;
COMMENT3: '//' ANY_CHAR* '\n' -> skip;

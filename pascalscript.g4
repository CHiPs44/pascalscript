/* ******************** PascalScript ANTLR4 ******************** */

grammar pascalscript;

options {
    // caseInsensitive = true;
}

/* Lexer rules begin with / are uppercase */

// Keywords

PROGRAM:    'PROGRAM';
BEGIN:      'BEGIN';
END:        'END';

IF:         'IF';
THEN:       'THEN';
ELSE:       'ELSE';

REPEAT:     'REPEAT';
UNTIL:      'UNTIL';

WHILE:      'WHILE';
DO:         'DO';

FOR:        'FOR';
TO:         'TO';
DOWNTO:     'DOWNTO';

WRITE:      'WRITE';
WRITELN:    'WRITELN';

FALSE:      'FALSE';
TRUE:       'TRUE';

LABEL:      'LABEL';
GOTO:       'GOTO';

INTEGER:    'INTEGER';
CARDINAL:   'CARDINAL';
BOOLEAN:    'BOOLEAN';
CHAR:       'CHAR';
STRING:     'STRING';
REAL:       'REAL';

ARRAY:      'ARRAY';
OF:         'OF';

RECORD:     'RECORD';

CONST:      'CONST';
TYPE:       'TYPE';
VAR:        'VAR';

PROCEDURE:  'PROCEDURE';
FUNCTION:   'FUNCTION';

NOT:        'NOT';
OR:         'OR';
XOR:        'XOR';
DIV:        'DIV';
MOD:        'MOD';
AND:        'AND';
SHL:        'SHL';
SHR:        'SHR';

// Symbols
DOT_COLON: ':='; // assign
AT_SIGN: '@'; //   address of
CARET: '^'; // pointer to
COLON: ':'; // various uses
COMMA: ','; // various uses
DOT_DOT: '..'; // ranges
DOT: '.'; // various uses
LEFT_BRACKET: '['; // array access
LEFT_PARENTHESIS: '('; // various uses
RIGHT_BRACKET: ']'; // array access
RIGHT_PARENTHESIS: ')'; // various uses
SEMI_COLON: ';'; // various uses
UNDERSCORE: '_';
QUOTE: '\'';

// Arithmetic operators
PLUS: '+'; // addition
MINUS: '-'; // substraction / negation
STAR: '*'; // multiplication
SLASH: '/'; // division (real)

// Comparison operators
EQUAL: '=';
NOT_EQUAL: '<>';
LESS_THAN: '<';
LESS_OR_EQUAL: '<=';
GREATER_THAN: '>';
GREATER_OR_EQUAL: '>=';

/* ******************** PROGRAM ******************** */

pascalProgram
    : PROGRAM identifier SEMI_COLON 
      pascalHeader
      instructionBlock DOT
    ;

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
    | ifBlock
    | repeatBlock
    | whileBlock
    | forBlock
    | procedureCall
    ;

assignment
    : variableReference DOT_COLON expression SEMI_COLON
    ;

ifBlock
    :  IF expression THEN
        ( instruction | instructionBlock ) SEMI_COLON
        (: ELSE ( instruction | instructionBlock ) SEMI_COLON )?
    ;
repeatBlock
    : REPEAT ( instruction )* UNTIL expression SEMI_COLON
    ;
whileBlock
    : WHILE expression DO instruction | instructionBlock SEMI_COLON
    ;
forBlock
    : FOR variableReference DOT_COLON expression ( TO | DOWNTO ) expression
        instruction | instructionBlock SEMI_COLON
    ;
procedureCall
    : ( identifier | WRITE | WRITELN ) (: LEFT_PARENTHESIS parametersList RIGHT_PARENTHESIS )? SEMI_COLON
    ;
functionCall
    : identifier (: LEFT_PARENTHESIS parametersList RIGHT_PARENTHESIS )?
    ;
parametersList
    : parameter ( COMMA parameter )*
    ;
parameter
    : expression
    ;

/* ******************** NUMBERS ******************** */

BINARY_DIGIT
    : '0' | '1'
    ;

OCTAL_DIGIT
    : '0'..'7'
    ;

DECIMAL_DIGIT
    : '0' | '9'
    ;

HEXADECIMAL_DIGIT
    : DECIMAL_DIGIT | 'a'..'f' | 'A'..'F'
    ;

binaryDigitSequence
    : (BINARY_DIGIT )+
    ;

octalDigitSequence
    : ( OCTAL_DIGIT )+
    ;

decimalDigitSequence
    : ( DECIMAL_DIGIT )+
    ;

hexadecimalDigitSequence
    : ( HEXADECIMAL_DIGIT )+
    ;

unsignedInteger
    : decimalDigitSequence 
    | '%' binaryDigitSequence 
    | '&' octalDigitSequence 
    | '$' hexadecimalDigitSequence
    ;

sign
    : PLUS 
    | MINUS
    ;

signedInteger
    : (: sign )? unsignedInteger
    ;

scaleFactor
    : ( 'E' | 'e' ) (: sign )? decimalDigitSequence
    ;

unsignedReal
    : decimalDigitSequence (: DOT decimalDigitSequence )? (: scaleFactor )?
    ;

signedReal
    : (: sign )? unsignedReal
    ;

/* ******************** IDENTIFIERS ******************** */

UPPERCASE_LETTER
    : 'A'..'Z'
    ;
LOWERCASE_LETTER
    : 'a'..'z'
    ;
letter
    : LOWERCASE_LETTER 
    | UPPERCASE_LETTER 
    ;
identifier
    : letter | UNDERSCORE ( letter | DECIMAL_DIGIT | UNDERSCORE )*
    ;
identifierList
    : identifier ( COMMA identifier )*
    ;

/* ******************** CHARS & STRINGS ******************** */

charValue
    : QUOTE 
    // Any character except ', CR or LF
    | UPPERCASE_LETTER
    | LOWERCASE_LETTER
    | DECIMAL_DIGIT
    // TODO other chars
    ;
controlChar
    : '#' unsignedInteger
    ;
quotedChar
    : QUOTE charValue QUOTE
    ;
characterValue
    : quotedChar
    | controlChar
    ;
characterString
    : quotedString | controlChar ( quotedString | controlChar )*
    ;
quotedString
    : QUOTE ( charValue )* QUOTE
    ;

/* ******************** BOOLEANS ******************** */
booleanValue
    : FALSE
    | TRUE
    ;

/* ******************** LABEL & GOTO ******************** */
label
    : decimalDigitSequence 
    | identifier
    ;
labelList
    : label (: COMMA label )*
    ;
labelDeclaration
    : LABEL labelList SEMI_COLON
    ;
gotoBlock
    : GOTO label SEMI_COLON
    ;

/* ******************** CONSTANTS ******************** */

constantIdentifier
    : identifier
    ;
constantValue
    : unsignedInteger
    | signedInteger
    | unsignedReal 
    | signedReal
    | quotedChar
    | quotedString
    ;
constBlock
    : CONST ( constantDeclaration SEMI_COLON )+
    ;
constantDeclaration
    : constantIdentifier EQUAL constantValue SEMI_COLON
    ;

/* ******************** TYPES ******************** */

typeBlock
    : TYPE typeDeclaration+
    ;
typeDeclaration
    : typeIdentifier EQUAL typeDefinition SEMI_COLON
    ;
typeIdentifier
    : identifier
    ;
typeReference
    : typeIdentifier
    | typeDefinition
    ;
typeDefinition
    : scalarType
    | stringType
    | realType
    | arrayType
    | recordType
    ;
scalarType
    : ordinalType
    | enumType
    | subrangeType
    ;
ordinalType
    : INTEGER | CARDINAL | BOOLEAN | CHAR
    ;
enumType
    : LEFT_PARENTHESIS identifier ( COMMA identifier )* RIGHT_PARENTHESIS
    ;
subrangeLimit
    : (: sign )? unsignedInteger
    | (: sign )? constantIdentifier
    ;
subrangeType
    : subrangeLimit DOT_DOT subrangeLimit
    ;
stringSize
    : unsignedInteger | constantIdentifier
    ;
stringType
    : STRING (: LEFT_BRACKET stringSize RIGHT_BRACKET )?
    ;
realType
    : REAL
    ;
arrayLimit
    : scalarType
    | typeIdentifier
    ;
arrayLimits
    : arrayLimit (: COMMA arrayLimit )*
    ;
arrayType
    : ARRAY LEFT_BRACKET arrayLimits RIGHT_BRACKET OF typeDefinition
    ;
recordType
    : RECORD ( identifierList COLON typeDefinition SEMI_COLON )* END
    ;

/* ******************** VARIABLES ******************** */

varBlock
    : VAR (: variableDeclaration SEMI_COLON )+
    ;
variableDeclaration
    : identifierList COLON typeReference
    ;

/* ******************** PROCEDURES & FUNCTIONS ******************** */

procedureFunctionBlock
    : (: procedureDeclaration | functionDeclaration )+
    ;
procedureDeclaration
    : PROCEDURE identifier (: LEFT_PARENTHESIS parameterDeclarationList RIGHT_PARENTHESIS )? SEMI_COLON
      procedureOrFunctionBody SEMI_COLON
    ;
functionDeclaration
    : FUNCTION identifier (: LEFT_PARENTHESIS parameterDeclarationList RIGHT_PARENTHESIS )? COLON typeReference SEMI_COLON
      procedureOrFunctionBody SEMI_COLON
    ;
parameterDeclaration
    : VAR? identifierList COLON typeReference
    ;
parameterDeclarationList
    : (: parameterDeclaration (: COMMA parameterDeclaration )* )?
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
    : identifier
    | identifier LEFT_BRACKET expression (:  COMMA expression )* RIGHT_BRACKET
    ;
constantReference
    : identifier
    ;
expression
    : unaryOperator expression
    | expression relationalOperator expression
    | expression additiveOperator expression
    | expression multiplicativeOperator expression
    | term
    | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
    ;
term
    : unsignedInteger
    | unsignedReal
    | characterValue
    | booleanValue
    | characterString
    | variableReference 
    | constantIdentifier
    | sign expression
    | functionCall
    ;

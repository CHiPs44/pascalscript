/* ******************** PascalScript ANTLR4 ******************** */

grammar pascalscript;

options {
    // caseInsensitive = true;
}

/* Lexer rules begin with / are uppercase */

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
CONST:      'CONST';
INTEGER:    'INTEGER';
CARDINAL:   'CARDINAL';
BOOLEAN:    'BOOLEAN';
CHAR:       'CHAR';
REAL:       'REAL';
ARRAY:      'ARRAY';
OF:         'OF';
STRING:     'STRING';
RECORD:     'RECORD';
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
PROGRAM:    'PROGRAM';
BEGIN:      'BEGIN';
END:        'END';

EQUALS:         '=';
DOT_DOT:        '..';
LEFT_BRACKET:   '[';
RIGHT_BRACKET:  '[';
// ':'
// '*'
// '/'
// '<>';
// '<';
// '<=';
// '>';
// '>=';
/*
		"LEFT_PARENTHESIS", "RIGHT_PARENTHESIS", "COMMA", "'0'", "'1'", "'2'", "'3'", "'4'", "'5'", "'6'", 
		"'7'", "'8'", "'9'", "'a'", "'b'", "'c'", "'d'", "'e'", "'f'", "'A'", 
		"'B'", "'C'", "'D'", "'E'", "'F'", "'%'", "'&'", "'$'", "'+'", "'-'", 
		"'G'", "'H'", "'I'", "'J'", "'K'", "'L'", "'M'", "'N'", "'O'", "'P'", 
		"'Q'", "'R'", "'S'", "'T'", "'U'", "'V'", "'W'", "'X'", "'Y'", "'Z'", 
		"'g'", "'h'", "'i'", "'j'", "'k'", "'l'", "'m'", "'n'", "'o'", "'p'", 
		"'q'", "'r'", "'s'", "'t'", "'u'", "'v'", "'w'", "'x'", "'y'", "'z'", 
		"UNDERSCORE", "''''", "'#'", "'''", "
*/
ASSIGN:     ':=';

/* ******************** PROGRAM ******************** */

pascalProgram
    : PROGRAM identifier SEMI_COLON programHeader instructionBlock '.'
    ;

programHeader
    : ( constBlock typeBlock varBlock )
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
    : variableReference ASSIGN expression SEMI_COLON
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
    : FOR variableReference ASSIGN expression ( TO | DOWNTO ) expression
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

binaryDigit
    : '0' | '1'
    ;

octalDigit
    : binaryDigit | ['2'..'7']
    ;

decimalDigit
    : octalDigit | '8' | '9'
    ;

hexadecimalDigit
    : decimalDigit | ['a'..'f'] | ['A'..'F']
    ;

binaryDigitSequence
    : binaryDigit ( binaryDigit )*
    ;

octalDigitSequence
    : octalDigit ( octalDigit )*
    ;

decimalDigitSequence
    : decimalDigit ( decimalDigit )*
    ;

hexadecimalDigitSequence
    : hexadecimalDigit ( hexadecimalDigit )*
    ;

unsignedInteger
    : decimalDigitSequence 
    | '%' binaryDigitSequence 
    | '&' octalDigitSequence 
    | '$' hexadecimalDigitSequence
    ;

sign
    : '+' 
    | '-'
    ;

signedInteger
    : (: sign )? unsignedInteger
    ;

scaleFactor
    : ( 'E' | 'e' ) (: sign )? decimalDigitSequence
    ;

unsignedReal
    : decimalDigitSequence (: '.' decimalDigitSequence )? (: scaleFactor )?
    ;

signedReal
    : (: sign )? unsignedReal
    ;

/* ******************** IDENTIFIERS ******************** */

uppercaseLetter
    : 'A'..'Z'
    ;

lowercaseLetter
    : 'a'..'z'
    ;
letter
    : lowercaseLetter 
    | uppercaseLetter 
    ;

identifier
    : letter | UNDERSCORE ( letter | decimalDigit | UNDERSCORE )*
    ;

identifierList
    : identifier ( COMMA identifier )*
    ;

/* ******************** CHARS & STRINGS ******************** */

charValue
    : '\'\'' 
    // Any character except ', CR or LF
    | uppercaseLetter
    | lowercaseLetter
    | decimalDigit
    // TODO other chars
    ;
controlChar
    : '#' unsignedInteger
    ;
quotedChar
    : '\'' charValue '\''
    ;
characterValue
    : quotedChar
    | controlChar
    ;
characterString
    : quotedString | controlChar ( quotedString | controlChar )*
    ;
quotedString
    : '\'' ( charValue )* '\''
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
constantDeclaration
    : constantIdentifier EQUALS constantValue SEMI_COLON
    ;
constBlock
    : CONST constantDeclaration SEMI_COLON ( constantDeclaration SEMI_COLON )*
    ;

/* ******************** TYPES ******************** */

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
scalarType
    : ordinalType
    | enumType
    | subrangeType
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
stringSize
    : unsignedInteger | constantIdentifier
    ;
stringType
    : STRING (: LEFT_BRACKET stringSize RIGHT_BRACKET )?
    ;
recordType
    : RECORD ( identifierList ':' typeDefinition SEMI_COLON )* END
    ;
typeDefinition
    : scalarType
    | realType
    | arrayType
    | stringType
    | recordType
    ;
typeIdentifier
    : identifier
    ;
typeDeclaration
    : typeIdentifier EQUALS typeDefinition
    ;
typeBlock
    : TYPE typeDeclaration SEMI_COLON (: typeDeclaration SEMI_COLON )*
    ;
typeReference
    : typeIdentifier
    | typeDefinition
    ;

/* ******************** VARIABLES ******************** */
variableDeclaration
    : identifier (: COMMA identifier )* ':' typeReference
    ;
varBlock
    : VAR variableDeclaration SEMI_COLON (: variableDeclaration SEMI_COLON )*
    ;

/* ******************** PROCEDURES & FUNCTIONS ******************** */
parameterDeclaration
    : identifierList ':' typeReference
    ;
parameterDeclarationList
    : (: parameterDeclaration (: COMMA parameterDeclaration )* )?
    ;
procedureOrFunctionBody
    : (: constBlock )? (: typeBlock )? (: varBlock )? (: procedureFunctionBlock )? instructionBlock
    ;
procedureDeclaration
    : PROCEDURE identifier (: LEFT_PARENTHESIS parameterDeclarationList RIGHT_PARENTHESIS )? SEMI_COLON
      procedureOrFunctionBody SEMI_COLON
    ;
functionDeclaration
    : FUNCTION identifier (: LEFT_PARENTHESIS parameterDeclarationList RIGHT_PARENTHESIS )? ':' typeReference SEMI_COLON
      procedureOrFunctionBody SEMI_COLON
    ;
procedureFunctionBlock
    : (: procedureDeclaration | functionDeclaration )*
    ;

/* ******************** EXPRESSIONS ******************** */
unaryOperator
    : '+' | '-' | NOT
    ;
additiveOperator
    : '+' | '-' | OR | XOR
    ;
multiplicativeOperator
    : '*' | '/' | DIV | MOD | AND | SHL | SHR
    ;
relationalOperator
    : EQUALS | '<>' | '<' | '<=' | '>' | '>=';
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

# Pascal BNF

From <http://downloads.freepascal.org/fpc/docs-pdf/ref.pdf>.

```text
letter                  =   'A'..'Z' | 'a'..'z'
digit                   =   '0'..'9'
hex_digit               =   digit | 'A'..'F' | 'a'..'f'
octal_digit             =   '0'..'7'
binary_digit            =   '0' | '1'

' + - * / = < > [ ] . , ( ) : ^ @ { } $ # & %
<< >> ** <> >< <= >= := += -= *= /= (* *) (. .) //

identifier              =   letter | '_' , [ letter | digit | '_' ]*

hex_digit_sequence      =   hex_digit, [ hex_digit ]*
octal_digit_sequence    =   octal_digit, [ octal_digit ]*
binary_digit_sequence   =   hex_digit, [ hex_digit ]*
digit_sequence          =   binary_digit , [ binary_digit ]*
unsigned_integer        =   digit_sequence | '$' hex_digit_sequence | '&' octal_digit_sequence | '%' binary_digit_sequence
sign                    =   '+' | '-'
scale_factor            =   'E' | 'e' , [ sign ] , digit_sequence
unsigned_real           =   digit_sequence [ '.' digit_sequence ] [ scale_factor ]
unsigned_number         =   unsigned_real | unsigned integer
signed_number           =   sign unsigned_number

label                   =   'LABEL' digit_sequence | identifier

character_string        =   quoted_string | control_string
quoted_string           =   "'" [ string_character ]* "’"
string_character        =   "''" | Any character except ’ or CR
control_string          =   '#' unsigned_integer

constant_declaration    =   identifier = expression hintdirectives ; 
IGNORED typed_constant_declaration =   ..

type_declaration        =   identifier = type hint_directives ;
type                    =   simple_type | string_type | structured_type | pointer_type | 
                            procedural_type | generic_type | specialized type | type alias 
simple_type             =   ordinal_type | real_type
real_type               =   real_type_identifier
ordinal_type            =   'INTEGER' | 'SHORTINT' | 'SMALLINT' | 'LONGINT' | 'LONGWORD' | 'INT64' | 
                            'BYTE' | 'WORD' | 'CARDINAL' | 'QWORD' | 'BYTEBOOL' | 'WORDBOOL' | 'LONGBOOL' | 'QWORDBOOL'
boolean_type            =   'BOOLEAN' | 'BOOLEAN16' | 'BOOLEAN32' | 'BOOLEAN64'
enumerated type         =   ( identifier_list )
identifier_list         =   [ identifier ][ , identifier , ]*
IGNORED assigned_enum_list


```

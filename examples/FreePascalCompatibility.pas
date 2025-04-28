Unit FreePascalCompatibility;

{
    This unit provides compatibility definitions to ensure that code written 
    for 32 bits PascalScript can be compiled with Free Pascal
    Use
      fpc -FaFreePascalCompatibility <yourfile>.pas
    to compile from examples directory
}

Interface

Const MaxUint = $FFFFFFFF; // 4294967295

{ Could be Cardinal, too, but MaXUint is already defined }
Type Unsigned = 0..MaxUint;

{ Supplemental function }
Function Even(N: Integer): Boolean;

Implementation

Function Even(N: Integer): Boolean;
Begin
  Even := Not Odd(N);
End;

End.

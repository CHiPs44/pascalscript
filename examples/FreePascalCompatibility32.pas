Unit FreePascalCompatibility32;

{
    This unit provides compatibility definitions to ensure that code written
    for 32 bits PascalScript can be compiled with Free Pascal
    Use
      fpc -FaFreePascalCompatibility32 <yourfile>.pas
    to compile from examples directory
}

Interface

Const
  { PascalScript version }
  PS_VERSION = '0.1.2.3';
  PS_BITNESS = 32;
  { Maximum unsigned integer }
  MaxUint    = $FFFFFFFF;
  { Real constants }
  MinReal    = -1.17549435082228750796873653722224568e-38;
  MaxReal    = 3.40282346638528859811704183484516925e38;
  EpsReal    = 1.19209289550781250000000000000000000e-7;

Type
  { Could be Cardinal, too, but MaXUint is already defined }
  Unsigned = 0..MaxUint;

{ Supplemental functions }

Function Even(N: Integer): Boolean;
Function Even(N: Unsigned): Boolean;
Function Tan(X: Real): Real;

Implementation

Function Even(N: Integer): Boolean;
Begin
  Even := Not Odd(N);
End;

Function Even(N: Unsigned): Boolean;
Begin
  Even := Not Odd(N);
End;

Function Tan(X: Real): Real;
Begin
  Tan := Sin(X) / Cos(X);
End;

End.

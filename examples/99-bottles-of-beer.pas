{ from https://rosettacode.org/wiki/99_Bottles_of_Beer/Pascal }
{ see also https://www.99-bottles-of-beer.net/language-pascal-1067.html }

Program NinetyNineBottlesOfBeer;

Var 
  I: Integer;

Begin
  For I := 99 Downto 1 Do
    If I <> 1 Then
      Begin
        WriteLn(I, ' bottles of beer on the wall');
        WriteLn(I, ' bottles of beer');
        WriteLn('Take one down, pass it around');
        If I = 2 Then
          WriteLn('One bottle of beer on the wall')
        Else
          WriteLn(I - 1, ' bottles of beer on the wall');
        WriteLn;
      End
    Else
      Begin
        WriteLn('One bottle of beer on the wall');
        WriteLn('One bottle of beer');
        WriteLn('Take one down, pass it around');
        WriteLn('No more bottles of beer on the wall');
      End
End.

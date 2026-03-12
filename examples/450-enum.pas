Program Enums;

Type
  TGender = (Male, Female, Other);
  TAbilities = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma);
  TCharacterClass = (Fighter, Wizard, Cleric, Rogue);
  TCharacterRace = (Human, Elf, Dwarf, Halfling, Gnome, HalfOrc);
  TDie = (D4, D6, D8, D10, D12, D20, D100);

Var
  Gender: TGender;
  Ability: TAbilities;
  CharacterClass: TCharacterClass;
  Race: TCharacterRace;
  Die: TDie;
  I: Integer;
  U: Unsigned;

Procedure TestLowHighPredSucc();
Begin
  // should not work! (and does not with FPC) because enumeration values are not implicitly convertible to integer/unsigned
  // I := Female;
  // WriteLn('I = ', I);
  // should work! and does with FPC because Ord() is used to explicitly convert the enumeration value to an integer/unsigned
  I := Ord('A');
  WriteLn('I = ', I, ' (=', 65, ')');
  I := Ord(Succ('A'));
  WriteLn('I = ', I, ' (=', 66, ')');
  I := Ord(Pred('A'));
  WriteLn('I = ', I, ' (=', 64, ')');
  I := Succ(I);
  WriteLn('I = ', I, ' (=', 65, ')');
  I := Pred(I);
  WriteLn('I = ', I, ' (=', 64, ')');
  I := High(Integer);
  WriteLn('I = ', I, ' (=', MaxInt, ')');
  I := Succ(I - 1);
  WriteLn('I = ', I, ' (=', MaxInt, ')');
  I := Low(Integer);
  WriteLn('I = ', I, ' (=', MinInt, ')');
  I := Pred(I + 1);
  WriteLn('I = ', I, ' (=', MinInt, ')');
  I := Ord(Female);
  WriteLn('I = ', I, ' (=', 1, ')');
End;

Procedure DisplayCharacter();
Begin
  WriteLn('Character details:');
  WriteLn('  Gender: ', Gender);
  WriteLn(' Ability: ', Ability);
  WriteLn('   Class: ', CharacterClass);
  WriteLn('    Race: ', Race);
  WriteLn('     Die: ', Die);
End;

Procedure DisplayDays();
Type
  Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
Var
  Day, Min, Max: Days;
Begin
  WriteLn('Days of the week:');
  Min := Low(Days);
  WriteLn('  Min=', Ord(Min), ' ', Min);
  Max := High(Days);
  WriteLn('  Max=', Ord(Max), ' ', Max);
  Write('  ');
  For Day := Min To Max Do
  Begin
    Write(Day);
    If Day <> Max then
      Write(', ');
  End;
  WriteLn;
End;

Begin
  TestLowHighPredSucc;
  // RPG Character
  Gender := Other;
  Ability := Wisdom;
  CharacterClass := Cleric;
  Race := Gnome;
  Die := D12;
  DisplayCharacter;
  // Days of the week
  DisplayDays;
End.

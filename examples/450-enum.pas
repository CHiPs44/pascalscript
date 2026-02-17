Program Enums;
Type
  TGender = (Male, Female, Other);
  TAbilities = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma);
  TCharacterClass = (Fighter, Wizard, Cleric, Rogue);
  TCharacterRace = (Human, Elf, Dwarf, Halfling, Gnome, HalfOrc);
  TDie = (D4, D6, D8, D10, D12, D20, D100);
  Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
Var
  Gender: TGender;
  Ability: TAbilities;
  CharacterClass: TCharacterClass;
  Race: TCharacterRace;
  Die: TDie;
  Day: Days;
  I: Integer;
Begin
  I := Female;
  Gender := Other;
  Ability := Wisdom;
  CharacterClass := Cleric;
  Race := HalfOrc;
  Die := D12;
  WriteLn('Character details:');
  WriteLn(' Gender: ', Gender);
  WriteLn('Ability: ', Ability);
  WriteLn('  Class: ', CharacterClass);
  WriteLn('   Race: ', Race);
  WriteLn('    Die: ', Die);
  WriteLn('Days of the week:');
  For Day := Monday to Sunday do
  Begin
    Write(Day);
    If Day <> Sunday then
      Write(', ');
  End;
  WriteLn();
End.
Program Enums;

Type
  TGender = (Male, Female, Other);
  TAbilities = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma);
  TCharacterClass = (Fighter, Wizard, Cleric, Rogue);
  TCharacterRace = (Human, Elf, Dwarf, Halfling, Gnome, Half_Orc);
  TDie = (D4, D6, D8, D10, D12, D20, D100);
  Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

Var
  Gender: TGender;
  Ability: TAbilities;
  CharacterClass: TCharacterClass;
  Race: TCharacterRace;
  Die: TDie;
  I: Integer;

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
// Type
//   Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
Var
  Day, Min, Max: Days;
Begin
  WriteLn('Days of the week:');
  Min := Low(Days);
  Max := High(Days);
  For Day := Min To Max Do
  Begin
    Write(Day);
    If Day <> Max then
      Write(', ');
  End;
  WriteLn;
End;

Begin
  // should not work!
  I := Female;
  // RPG Character
  Gender := Other;
  Ability := Wisdom;
  CharacterClass := Cleric;
  Race := Gnome;
  Die := D12;
  DisplayCharacter;
  WriteLn;
  // Days of the week
  DisplayDays;
End.

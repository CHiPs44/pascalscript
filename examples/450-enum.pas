Program Enums;
Type
  TGender = (Male, Female, Other);
  TAbilities = (Strength, Intelligence, Wisdom, Dexterity, Constitution, Charisma);
  TCharacterClass = (Fighter, Wizard, Cleric, Rogue);
  TCharacterRace = (Human, Elf, Dwarf, Halfling, Gnome, HalfOrc);
  TDie = (D4, D6, D8, D10, D12, D20, D100);
  Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
Var
  MyGender: TGender;
  MyAbility: TAbilities;
  MyClass: TCharacterClass;
  MyRace: TCharacterRace;
  MyDie: TDie;
  MyDay: Days;
Begin
  MyGender := Other;
  MyAbility := Wisdom;
  MyClass := Cleric;
  MyRace := HalfOrc;
  MyDie := D12;
  WriteLn('My Gender: ', MyGender);
  WriteLn('My Ability: ', MyAbility);
  WriteLn('My Class: ', MyClass);
  WriteLn('My Race: ', MyRace);
  WriteLn('My Die: ', MyDie);
  WriteLn('Days of the week:');
  For MyDay := Monday to Sunday do
  Begin
    Write(MyDay);
    If MyDay <> Sunday then
      Write(', ');
  End;
  WriteLn();
End.
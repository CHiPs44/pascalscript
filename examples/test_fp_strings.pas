Program TestFpStrings;

{$R+}

Var
    S1: String[100];
    S2: String;

Begin
    S1 := '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890';
    WriteLn('Length(S1)=', Length(S1), ' High(S1)=', High(S1));
    WriteLn('S1=''', S1, '''');
    S2 := '';
    WriteLn('S2=''', S2, '''');
    WriteLn('Length(S2)=', Length(S2), ' High(S2)=', High(S2));
    S2 := S2 + S1;
    WriteLn('Length(S2)=', Length(S2), ' High(S2)=', High(S2));
    WriteLn('S2=''', S2, '''');
    S2 := S2 + S1;
    WriteLn('Length(S2)=', Length(S2), ' High(S2)=', High(S2));
    WriteLn('S2=''', S2, '''');
    S2 := S2 + S1;
    // S2 will be truncated to 255 chars without error, even when range checking is on
    // cf. https://www.freepascal.org/docs-html/current/ref/refsu47.html:
    // Note that if all strings in a string expressions are short strings, 
    // the resulting string is also a short string. Thus, a truncation may occur: 
    // there is no automatic upscaling to ansistring. 
    WriteLn('Length(S2)=', Length(S2), ' High(S2)=', High(S2));
    WriteLn('S2=''', S2, '''');
End.

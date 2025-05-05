Program Example06ForDo;
Const
    Limit = 30;
Var
    I: Integer;
    C: Char; 
Begin
    For I := -Limit To Limit Do
    Begin
        Write(I);
        // If I >= 0 And I Mod 10 = 0 Then
        If (I >= 0) And (I Mod 10 = 0) Then
            WriteLn
        Else
            Write(' ');
    End;
    WriteLn('OK!');
    I := 0;
    For C := 'A' To 'Z' Do
    Begin
        Write('''', C, '''=#', Ord(C));
        I := I + 1;
        If I Mod 13 = 0 Then
            WriteLn
        Else
            Write(' ');
    End;
    WriteLn('OK!');
End.

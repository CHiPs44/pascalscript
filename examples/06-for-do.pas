Program Example05WhileDo;
Const
    Limit = 30;
Var
    I : Integer;
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
End.

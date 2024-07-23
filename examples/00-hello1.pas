(* Won't work with first versions as they lack strings *)
Program HelloWorld;
Const 
    HELLO_WORLD = '''Hello, ''world''!''';
Begin
    WriteLn(HELLO_WORLD);
End.

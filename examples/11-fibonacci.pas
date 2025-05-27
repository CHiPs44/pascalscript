{ From https://wiki.freepascal.org/Basic_Pascal_Tutorial/Chapter_3/Solution }

(* Author:    Tao Yue
   Date:      19 July 1997
   Description:
      Find the first 10 Fibonacci numbers
   Version:
      1.0 - original version
*)

Program Fibonacci;

Var 
  Fibonacci1, Fibonacci2 : integer;
  temp : integer;
  count : integer;

Begin
(* Main *)
  writeln ('First ten Fibonacci numbers are:');
  count := 0;
  Fibonacci1 := 0;
  Fibonacci2 := 1;
  Repeat
    write (Fibonacci2(*:7*), ' ');
    temp := Fibonacci2;
    Fibonacci2 := Fibonacci1 + Fibonacci2;
    Fibonacci1 := Temp;
    count := count + 1
  Until count = 42;
  writeln;
  (* Of course, you could use a FOR loop or a WHILE loop
  to solve this problem. *)
End.
(* Main *)

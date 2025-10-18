{ From https://wiki.freepascal.org/Basic_Pascal_Tutorial/Chapter_3/Solution}


(* Author:    Tao Yue
   Date:      13 July 2000
   Description:
      Display all powers of two up to 20000, five per line
   Version:
      1.0 - original version
*)

Program PowersofTwo;

Const 
  numperline = 5;
  maxnum = 20000;
  base = 2;

Var 
  number : integer;
(*longint;*)
  linecount : integer;

Begin
  (* Main *)
  writeln ('Powers of ', base, ', 1 <= x <= ', maxnum, ':');
   (* Set up for loop *)
  number := 1;
  linecount := 0;
   (* Loop *)
  While number <= maxnum Do
    Begin
      linecount := linecount + 1;

(* Print a comma and space unless this is the first
         number on the line *)
      If linecount > 1 Then
        write (', ');
         (* Display the number *)
      write (number);

(* Print a comma and go to the next line if this is
         the last number on the line UNLESS it is the
         last number of the series *)
      If (linecount = numperline) And Not (number * 2 > maxnum) Then
        Begin
          writeln (',');
          linecount := 0
        End;
      (* Increment number *)
      number := number * base;
      // writeln('test', number <= maxnum);
    End;
  (* while *)
  writeln;
   (* This program can also be written using a
      REPEAT..UNTIL loop. *)
End.
(* Main *)
program const_circle(input,output);

{from https://www.tutorialspoint.com/pascal/pascal_constants.htm}

const PI = 3.141592654;

var r, d, c : real;   {variable declaration: radius, diameter, circumference}

begin
   writeln('Enter the radius of the circle');
   readln(r);
   d := 2 * r;
   c :=  PI * d;
   writeln('The circumference of the circle is ', c:7:2);
end.

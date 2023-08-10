program hello;
{ Won't work with first versions }
const
  HelloWorld = 12345678;
  // HelloWorld2 = 123456789023457890123456789023457890;
  // HelloWorld3 = 4294967295;
  HelloWorld3 = 2147483648;
begin
  (*WriteLn(
    'Hello, world!'
  );*)
  WriteLn(HelloWorld);
  // Test line commment
end.

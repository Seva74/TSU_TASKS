Type
  pel = ^elem;
  elem = Record
       Data : integer;
       Next : pel ;
  End;

Var
  u : pel;
  i,n:integer;

Procedure Init(Var u : pel);
Var
  p : pel;
begin
  u := Nil;
  for i:=1 to n do
  begin
    New(p);
    p^.Next := Nil;
    p^.Data := random(1998)-1000 ;
    writeln(p^.data);
    u := p;
  end;
//Writeln(u^.data,' - the last elemnt of the list');
end;

begin
 writeln('how many random numbers do you want?');
 readln(n);
 Init(u);
 readln();
end.


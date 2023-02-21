program abc;
var f:text;
  a,b,l:integer;
  s:string;
  d:char;

begin

  assign(f,'123.txt');
  reset(f);
  read(f,a,d);
  while(l<>1) do
  begin
       read(f,d);
       if(d<>chr(32))then s:=s+d
       else l:=1;
  end;
  l:=0;

  writeln(a*2,' ',s,' ', b*3);
  readln;


end.

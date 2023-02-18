program test;
var f:text;
  k,k1,i,n:integer;
  s:string;
  d:char;
  a:array[1..100] of string[20];

begin
  n:=1;
  assign(f,'test1.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
      //write(d);
      if(d<>chr(32))then s:=s+d
      else begin
        if(k=1) then
        begin
          for i:=0 to n do if a[i]=s then k1:=1;
          if(k1=0) then
          begin
            a[n]:=s;
            n:=n+1;
          end;
          k1:=0;
          k:=0;
        end;
        if(s='Россия') then k:=1;
        s:='';
      end;
    end;
    read(f,d);
  end;
  close(f);
  //repeating for another one file
  assign(f,'test2.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
      //write(d);
      if(d<>chr(32))then s:=s+d
      else begin
        if(k=1) then
        begin
          for i:=0 to n do if a[i]=s then k1:=1;
          if(k1=0) then
          begin
            a[n]:=s;
            n:=n+1;
          end;
          k1:=0;
          k:=0;
        end;
        if(s='Россия') then k:=1;
        s:='';
      end;
    end;
    read(f,d);
  end;
  close(f);
  assign(f,'result.txt');
  rewrite(f);
  for i:=1 to n do write(f,a[i],' ');
  close(f);
  //readln;

end.

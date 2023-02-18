program test;
var f:text;
  k,k1,i,j,l:integer;
  s,ru:string;
  d:char;
  a:array[1..100,1..100] of string[20];
  b:array[1..100,1..100] of string[20];

begin
  k:=2;
  assign(f,'file1.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
      //write(d);
      if(d<>chr(32))then
      begin
        s:=s+d;
        l:=1;
      end
      else if( (d=chr(32)) and (l=1)) then
      begin
        k:=k+1;
        a[k div 3, k mod 3]:=s;
        s:='';
        l:=0;
      end
      else l:=0;
    end;
    read(f,d);
  end;
  close(f);

  assign(f,'file2.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
      //write(d);
      if(d<>chr(32))then
      begin
        s:=s+d;
        l:=1;
      end
      else if( (d=chr(32)) and (l=1)) then
      begin
        k:=k+1;
        a[k div 3, k mod 3]:=s;
        s:='';
        l:=0;
      end
      else l:=0;
    end;
    read(f,d);
  end;
  close(f);

  k1:=2;
  assign(f,'file3.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
      //write(d);
      if(d<>chr(32))then
      begin
        s:=s+d;
        l:=1;
      end
      else if( (d=chr(32)) and (l=1)) then
      begin
        k1:=k1+1;
        b[k1 div 3, k1 mod 3]:=s;
        s:='';
        l:=0;
      end
      else l:=0;
    end;
    read(f,d);
  end;
  close(f);

  assign(f,'file4.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
      //write(d);
      if(d<>chr(32))then
      begin
        s:=s+d;
        l:=1;
      end
      else if( (d=chr(32)) and (l=1)) then
      begin
        k1:=k1+1;
        b[k1 div 3, k1 mod 3]:=s;
        s:='';
        l:=0;
      end
      else l:=0;
    end;
    read(f,d);
  end;
  close(f);

  for j:=1 to (k div 3) do
    begin
      if(a[j,1]<>'Россия')then
      begin
        for i:=1 to (k1 div 3) do
        begin
          if(a[j,0]=b[i,0]) then
          begin
            writeln(a[j,0],' ',a[j,1],' ',a[j,2],' ',b[i,1],' ',b[i,2]);
          end;
        end;
      end;
    end;
  readln;
end.

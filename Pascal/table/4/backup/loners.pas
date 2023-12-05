program loners;
var f:text;
  k,k1,i,j,z,l,count,min,max:integer;
  s,compare:string;
  d:char;
  a:array[1..100,1..100] of string[20];
  b:array[1..100,1..100] of string[20];
  c:array[1..200] of string[20];
  e:array[1..200] of string[20];
  g:array[1..200] of integer;

begin
  k:=2;
  assign(f,'file1.txt');
  reset(f);
  while not eof(f) do
  begin
    while not eoln(f) do
    begin
      read(f,d);
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

  l:=0;
  for j:=1 to (k div 3) do
    begin
      if(a[j,1]='Россия')then
      begin
        compare:=a[j,2];
        for i:=1 to (k div 3) do if(a[i,2]=compare) then l:=l+1;
        if (l=1) then
        begin
          count:=count+1;
          c[count]:=compare;
          for z:=1 to (k1 div 3) do
            begin
              if(a[j,0] = b[z,0]) then e[count]:=b[z,2];
            end;
        end;
        l:=0;
      end;
    end;
  for j:=1 to (k div 3) do
    begin
      compare:=a[j,1];
      for i:=1 to (k div 3) do
        begin
          if(a[i,1]=compare) then l:=l+1;
        end;
      if (l=1) then
        begin
          count:=count+1;
          c[count]:=compare;
          for z:=1 to (k1 div 3) do
            begin
              if(a[j,0] = b[z,0]) then e[count]:=b[z,2];
            end;
        end;
      l:=0;
    end;
   writeln('список городов России и стран, из которых приехало только по одному человеку:');
   for i:=1 to count do writeln('студент из ',c[i],' родившийся в ',e[i],' году');
   for i:=1 to count do val(e[i],g[i]);
   min:=g[1];
   max:=g[1];
   for i:=2 to count do
     begin
       if (g[i]<min) then min:=g[i];
       if (g[i]>max) then max:=g[i];
     end;
   writeln('а их возрастной диапазон: от ', 2023-max, ' лет до ', 2023-min, ' лет');
  readln;
end.

program Sort;
uses dos;
var
  h1,m1,s1,t1:word;
  h2,m2,s2,t2:word;
  d:longint;
var
  i,a,b,m,n: integer;
  Aux,Mass,Unsort: array of integer;

procedure SimpleSort(var Mass: array of integer; length: integer);
  var i,a: integer;
  begin
       i:= 0;
       while i <> length do begin
             i+= 1;
             if Mass[i] < Mass[i-1] then begin
                a := Mass[i];
                Mass[i]:= Mass[i-1];
                Mass[i-1]:= a;
                i:= 0;
             end;
       end;
  end;

procedure insert(var Mass: array of integer; length: integer);
  var i,j: integer;
begin
  for j:= 1 to length-1 do begin
    for i:= 1 to j+1 do begin
      if Mass[j+1] < Mass[i] then begin
        a:= Mass[i];
        Mass[i]:= Mass[j+1];
        Mass[j+1]:= a;
      end;
    end;
  end;
end;

procedure S(var Mass: array of integer; first, last: integer);
  var middle, start, final , i: integer;
      Aux: array [1..1000000] of integer;
begin
     middle:= (first + last) div 2;
     start:= first;
     final:= middle+1;
     for i:=first to last do
         if (start <= middle) and ((final > last) or (Mass[start] < Mass[final])) then begin
            Aux[i]:= Mass[start];
            start+= 1;
         end
         else begin
              Aux[i]:=Mass[final];
              final+=1;
         end;
for i:=first to last do Mass[i]:=Aux[i];
end;

procedure MergeSort(var Mass: array of integer; first, last: integer);
begin
     if first < last then begin
        MergeSort(Mass, first, (first + last) div 2);
        MergeSort(Mass, (first + last) div 2 + 1, last);
        S(Mass, first, last);
     end;
end;

function Test(var Unsort, Mass: array of integer; b: integer): boolean;
         var unsortcount, masscount: array [-999..999] of longint;
             i: longint;
begin
     i:= 1;
     while (i < b) and (Mass[i] <= Mass[i+1]) do i+= 1;
     if i < b-1 then Test := False
     else begin
        for i:= -999 to 999 do begin
              Unsortcount[i] := 0;
              Masscount[i] := 0;
        end;
        for i:= 1 to b do begin
              Unsortcount[unsort[i]] += 1;
              Masscount[Mass[i]] += 1;
        end;
        i:= -999;
        while (i <= 999) and (unsortcount[i] = masscount[i]) do i+= 1;
        if i = 1000 then Test:= True
        else Test:= False;
     end;
end;

procedure first ();
begin
  gettime(h1,m1,s1,t1);
  SimpleSort(Mass, m);
  gettime(h2,m2,s2,t2);
  d:=(longint(h2)*360000+longint(m2)*6000+s2*100+t2)-
     (longint(h1)*360000+longint(m1)*6000+s1*100+t1);
  writeln('SimpleSort time is ',d/100:0:2,' seconds');
end;

procedure second ();
var start, stop: TDateTime;
begin
  gettime(h1,m1,s1,t1);
  insert(Mass, m);
  gettime(h2,m2,s2,t2);
  d:=(longint(h2)*360000+longint(m2)*6000+s2*100+t2)-
     (longint(h1)*360000+longint(m1)*6000+s1*100+t1);
  writeln('insert time is ',d/100:0:2,' seconds');
end;

procedure third ();
var start, stop: TDateTime;
begin
  gettime(h1,m1,s1,t1);
  mergesort(Mass, 1, m);
  gettime(h2,m2,s2,t2);
  d:=(longint(h2)*360000+longint(m2)*6000+s2*100+t2)-
     (longint(h1)*360000+longint(m1)*6000+s1*100+t1);
  writeln('mergesort time is ',d/100:0:2,' seconds');
end;

begin
  Randomize;
  Write('Input array length: ');
  Readln(b);
  if b>=1000 then exit;
  Setlength(Mass,1000000);
  Setlength(Aux,1000000);
  Setlength(Unsort,1000000);

  for i:= 1 to b do begin
    Mass[i]:= random(1000);
    Unsort[i]:= Mass[i];
  end;


  SimpleSort(Mass, b);
  Write('Result for Simple Sort: ');
  for i:= 1 to b do write(Mass[i], ' ');
  if Test(Unsort, Mass, b) = False then exit;

  writeLn();

  for i:= 1 to b do begin
    Mass[i]:= random(1000);
    Unsort[i]:= Mass[i];
  end;

  insert(Mass, b);
  Write('Result for Insert Sort: ');
  for i:= 1 to b do write(Mass[i], ' ');
  if Test(Unsort, Mass, b) = False then exit;

  writeLn();

  for i:= 1 to b do begin
    Mass[i]:= random(1000);
    Unsort[i]:= Mass[i];
  end;

  mergesort(Mass, 1, b);
  Write('Result for Merge sort: ');
  for i:= 1 to b do write(Mass[i], ' ');
  if Test(Unsort, Mass, b) = False then exit;

   m:= 10;
   n:= 0;
   writeln;
   while m <= 1000 do begin
     for i:= 1 to m do begin
    Mass[i]:= random(1000);
    Unsort[i]:= Mass[i];
  end;


        write('for ', m,' elements ');
        first;



          n+= 1;
          m*= 10;
   end;
   m:= 10;
   while m <= 10000 do begin
     for i:= 1 to m do begin
    Mass[i]:= random(1000);
    Unsort[i]:= Mass[i];
  end;


        write('for ', m,' elements ');
        second;


          n+= 1;
          m*= 10;
   end;
   m:= 10;
   while m <= 1000000 do begin
     for i:= 1 to m do begin
    Mass[i]:= random(1000);
    Unsort[i]:= Mass[i];
  end;


        write('for ', m,' elements ');
        third;



          n+= 1;
          m*= 10;
   end;

  readLn(b);
end.


program LabSS;
var T10,Max64,NSS,ss1,ss2,n,i,kn,SS,Max64N: int64;
var D,Z,S:array[1..63] of integer; // maxint64 = 9 2 2 3 3 7 2 0 3 6 8 5 4 7 7 5 8 0 7 in 2 SS = 63*'1'
  procedure wait();
begin
    readln();
    readln();
end;
begin
  Max64N:=0;
  Max64:= 9223372036854775807;
  write('Enter the current base of the number system ');
  read(ss1);
  if (ss1<2) or (ss1>10000) then begin
    writeln('Error, invalid number system');
    wait;
    exit;
  end;
  write('Enter the number of digits in the number ');
  read(n);
  repeat
  inc(Max64N);
  Z[Max64N]:= Max64 mod ss1;
  Max64:= Max64 div ss1;

  until Max64 = 0;
  if (n<1) or (n>63) or(n>Max64N) then begin
    writeln('Error, invalid number');
    wait;
    exit;
  end;
  write('Enter the number in digits separated by a space ');
  for i:=1 to n do begin
    read(D[i]);
    if D[i]> ss1 then begin
      writeln('Error, the number is not contained in the original number system');
      wait;
      exit;
    end;
  end;
  if n=max64n then begin
    for i:=1 to max64n do begin
     if D[i]>Z[max64n-i+1] then begin
     writeln('Error, invalid number');
     end;
     end;
    end;
  write('Enter the desired number system ');
  read(ss2);
  kn:=n;
  T10:=0;
  SS:=1;
  i:=1;
  for  kn:=1 to n do begin
      T10:= T10*ss1+D[kn];
  end;
  NSS:=0;
  repeat
  S[i]:= T10 mod ss2;
  inc(i);
  T10:= T10 div ss2;
  inc(NSS);
  until T10 = 0;
  while NSS>0 do begin
  write(S[NSS],' ');
  dec(NSS);
  end;
  wait;
end.


program sorting;
const
n=16;
var
Name:array [0..n-1] of string = ('scene','breakfast','judgment','glass','society','condition','flight','courage','trust','homework','cash','possibility','weather','service','extent','damage');
Indexation:array [0..n-1] of integer = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
i:integer;
procedure Sort(var index:array of integer;SortM:array of string);
var i,j,k,x:integer;

begin
  for j:=0 to n-2 do begin
  k:=j;
  for i:=j+1 to n-1 do
      if SortM[index[k]] > SortM[index[i]] then
         k:=i;
  x:=index[j];
  index[j]:=index[k];
  index[k]:=x;
end;

end;
begin
  writeln('Source array');
  for i:=0 to n-1 do writeln (Indexation[i], ' ',Name[Indexation[i]]);
  writeln;

  Sort(indexation,Name);
  writeln('Sorted array');
  for i:=0 to n-1 do begin
      writeln (Indexation[i], ' ',Name[Indexation[i]]);
  end;
  readln;
end.

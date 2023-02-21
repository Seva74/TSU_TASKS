uses crt,Math;
var x1,x2,x,dx,eps,s,t,a,sum,answer:Extended;
  it,k,k1:int64;
function factorial(var it:int64):int64;
var j,s: int64;
  begin
    s := 1;
    for j := 1 to (it*2) do
      s := s * j;
    factorial:=s
  end;

begin
clrscr;
x1:=-Pi;
x2:=Pi;
dx:=(x2-x1)/9;
writeln(' function is cos(x) ');
for k:=0 to 9 do
  begin
    x:=x1+(dx*k);
    a:=1000;
    it:=0;
    eps:=0.1;
    sum:=0;
    for k1:=0 to 5 do
      begin
        while (abs(a)>eps)do
        begin
        it:=it+1;
        a:= ( (power(-1,it)*Power(x, 2*it) )/ (factorial(it)));
        sum:=sum+a;
        end;
        answer:=sum+1;
        writeln(' x = ' ,x:5:2,'. sum = ', answer:8:4,'. eps = ',eps:8:6,'. number of iterction = ',it);
        eps:=eps/10
      end;
  end;
readln
end.


unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    LinedButton: TButton;
    FileOpenDialog: TOpenDialog;
    FileSaveDialog: TSaveDialog;
    SortButton: TButton;
    MergeButton: TButton;
    FileSaveButton: TButton;
    FileOpenButton: TButton;
    ExitButton: TButton;
    Input: TEdit;
    Label1: TLabel;
    ListLined: TListBox;
    ListSorted: TListBox;
    ListMerge: TListBox;
    FileMemo: TMemo;
    procedure ExitButtonClick(Sender: TObject);
    procedure FileOpenButtonClick(Sender: TObject);
    procedure FileSaveButtonClick(Sender: TObject);
    procedure LinedButtonClick(Sender: TObject);
    procedure MergeButtonClick(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);

  private

  public

  end;

  type pel=^elem;
     elem=record s:integer; p:pel end;
var
  Form1: TForm1;
  M, Y: array of integer;
  n, i, S0: integer;
  st: string;
  p1,p2,p3,p4,p5:pel;
implementation

Procedure generate(n:integer);
begin
  p1:=nil; p2:=nil;
  for i:=0 to n-1 do
  begin
    new(p3);
    if p1=nil then p1:=p3
    else p2^.p:=p3;
    p2:=p3; p2^.s:=random(1998)-1000; p2^.p:=nil;
  end;

end;

procedure slist(var p1,p2,p3:pel);
  var p4:pel;
begin
  if p1^.s<=p2^.s then
    begin p3:=p1; p4:=p1; p1:=p1^.p end
  else begin p3:=p2; p4:=p2; p2:=p2^.p end;
  while (p1<>nil)and(p2<>nil) do
   if p1^.s<=p2^.s then
   begin p4^.p:=p1; p4:=p1; p1:=p1^.p end
   else begin p4^.p:=p2; p4:=p2; p2:=p2^.p end;
   if p1<>nil then p4^.p:=p1  else p4^.p:=p2;
   p1:=nil; p2:=nil
end;

procedure sortlist(var p:pel;n:integer);
  var p1,p2:pel;
      k,i:integer;
begin
  if n>1 then begin
    k:=n div 2; p1:=p;
    for i:=1 to k-1 do p1:=p1^.p;
    p2:=p1^.p; p1^.p:=nil; p1:=p;
    sortlist(p1,k);
    sortlist(p2,n-k);
    slist(p1,p2,p)
  end
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  Form1.close;
end;

procedure TForm1.FileSaveButtonClick(Sender: TObject);
var outf: textfile;
    i: integer;
begin
  Form1.FileMemo.Lines.Clear;
  Form1.FileMemo.Lines.Add('Укажите файл для сохранения списка');
  if Form1.FileSaveDialog.Execute then
  begin
    assignfile (outf, Form1.FileSaveDialog.FileName);
    rewrite(outf);

    //for i:=0 to (n-1) do
    //begin
    //  write (outf, IntToStr(M[i]) + ' ');
    //end;
   p3:=p1;
   while p3<>nil do begin
    write(outf, IntToStr(p3^.s) + ' ');
   p3:=p3^.p
   end;
    closefile (outf);
    Form1.FileMemo.Lines.Clear;
    Form1.FileMemo.Lines.Add ('Cписок записан в файл '+ Form1.FileSaveDialog. FileName);
  end
  else; //файл не выбран
end;

procedure TForm1.FileOpenButtonClick(Sender: TObject);
begin
  FileMemo.Lines.Clear;
  FileMemo.Lines.Add('Укажите файл');
  if FileOpenDialog.Execute then
  begin
    if FileExists (FileOpenDialog.FileName) then
    begin
      FileMemo.Lines.Add('Oткрыт файл '+FileOpenDialog.FileName);
      //...
    end
    else FileMemo.Lines.Add('Фaйл '+FileOpenDialog.FileName+ ' не найден' );
  end
  else; //файл не выбран
end;

procedure TForm1.LinedButtonClick(Sender: TObject);
begin
  Form1.ListLined.Clear;
  n:= StrToInt(Input.text);
  generate(n);

  if n > 40 then Exit;
   p3:=p1;
   while p3<>nil do begin
     st:= IntToStr(p3^.s);
     Form1.ListLined.Items.add(st);
     p3:=p3^.p
   end;
end;

procedure TForm1.MergeButtonClick(Sender: TObject);
begin
    Form1.ListMerge.Clear;
  n:= StrToInt(Input.text);
  if n > 40 then Exit;
  generate(n);
  sortlist(p1,n);
  p3:=p1;
  while p3<>nil do begin
    st:= IntToStr(p3^.s);
    Form1.ListMerge.Items.add(st);
    p3:=p3^.p
  end;
end;

procedure TForm1.SortButtonClick(Sender: TObject);
begin
  Form1.ListSorted.Clear;
  if n > 40 then Exit;
  n:= StrToInt(Input.text);
  generate(1);
  for i:=1 to n-1 do
  begin
      S0:=random(1998)-1000;
      new(p3); p3^.s:=S0;
      p4:=nil; p5:=p1;
      while (p5<>nil)and(p5^.s<S0) do
          begin p4:=p5; p5:=p5^.p end;
      if p4=nil then p1:=p3{вставка в начало списка}
      else p4^.p:=p3;{вставка в список между p4 и p5}
      p3^.p:=p5;
      if p5=nil then p2:=p3;{вставка в конец списка}
  end;

   p3:=p1;
   while p3<>nil do begin
     st:= IntToStr(p3^.s);
     Form1.ListSorted.Items.add(st);
     p3:=p3^.p
   end;
end;
end.

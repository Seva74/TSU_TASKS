unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    MergeButton: TButton;
    FileSaveButton: TButton;
    FileOpenButton: TButton;
    FileOpenDialog: TOpenDialog;
    FileSaveDialog: TSaveDialog;
    FileMemo: TMemo;
    ListMerge: TListBox;
    SortButton: TButton;
    LinedButton: TButton;
    ExitButton: TButton;
    ListSorted: TListBox;
    ListLined: TListBox;
    NInput: TEdit;
    Label1: TLabel;
    procedure FileMemoChange(Sender: TObject);
    procedure FileOpenButtonClick(Sender: TObject);
    procedure FileSaveButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure LinedButtonClick(Sender: TObject);
    procedure MergeButtonClick(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);
  private

  public

  end;

Type Mass= array [1..100] of integer;
   TMass= ^Mass;

var
  Form1: TForm1;
  M, Y: array of integer;
  n, i: integer;
  st: string;

implementation

procedure generate(n: integer);
begin
  SetLength(M, n);
  for i:=0 to n-1 do
  begin
    M[i]:= random(1998)-999
  end;
end;

procedure sort(n: integer);
var j,q,x,a: integer;
begin
  generate(n);
  for j:=0 to n-1 do begin
    a:=j;
    for q:=j+1 to n do
      if M[a]>M[q] then a:=q;
    x:=M[j];
    M[j]:=M[a];
    M[a]:=x
  end;
end;

//procedure S(b1,e1,e2:integer);
//  var i1, i2, j:longint;
//begin i1:=b1; i2:=e1+1; j:=b1;
//  while (i1<=e1)and(i2<=e2) do begin
//    if M[i1]<=M[i2] then
//      begin Y[j]:=M[i1]; i1:=i1+1 end
//    else begin Y[j]:=M[i2]; i2:=i2+1 end;
//    j:=j+1
//  end;
//  while i1<=e1 do begin
//    Y[j]:=M[i1]; i1:=i1+1; j:=j+1
//  end;
//  while i2<=e2 do begin
//    Y[j]:=M[i2]; i2:=i2+1; j:=j+1
//  end
//end;
//
//procedure Merge(b,e:integer);
//  var c,i:integer;
//  begin
//    generate(n);
//    if b<e then begin
//      c:=(b+e)div 2;
//      Merge(b,c);
//      Merge(c+1,e);
//      S(b,c,e);
//      for i:=b to e do M[i]:=Y[i]
//    end
//  end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  Halt;
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

    for i:=0 to n do
    begin
      write (outf, IntToStr(M[i]) + ' ');
    end;

    closefile (outf);
    Form1.FileMemo.Lines.Clear;
    Form1.FileMemo.Lines.Add ('Cписок записан в дайл '+ Form1.FileSaveDialog. FileName);
  end
  else; //файл не выбран
end;

procedure TForm1.FileMemoChange(Sender: TObject);
begin

end;

procedure TForm1.FileOpenButtonClick(Sender: TObject);
begin
  FileMemo.Lines.Clear;
  FileMemo.Lines.Add('Укажите файл');
  if FileOpenDialog.Execute then
  begin
    if FileExists (FileOpenDialog.FileName) then
    begin
      FileMemo.Lines.Add('Oткрыт дайл'+FileOpenDialog.FileName);
      //...
    end
    else FileMemo.Lines.Add('Фaйл'+FileOpenDialog.FileName+ ' не найден' );
  end
  else; //файл не выбран
end;

procedure TForm1.LinedButtonClick(Sender: TObject);
begin
  Form1.ListLined.Clear;
  n:= StrToInt(NInput.text);
  generate(n);

  if n > 40 then Exit;
  for i:=0 to n-1 do
  begin
    st:= IntToStr(M[i]);
    Form1.ListLined.Items.add(st);
  end;
end;

procedure TForm1.MergeButtonClick(Sender: TObject);
begin
    Form1.ListMerge.Clear;
  n:= StrToInt(NInput.text);
  sort(n);

  if n > 40 then Exit;
  for i:=0 to n-1 do
  begin
    st:= IntToStr(M[i]);
    Form1.ListMerge.Items.add(st);
  end;
end;

procedure TForm1.SortButtonClick(Sender: TObject);
begin
  Form1.ListSorted.Clear;
  n:= StrToInt(NInput.text);
  sort(n);

  if n > 40 then Exit;
  for i:=0 to n-1 do
  begin
    st:= IntToStr(M[i]);
    Form1.ListSorted.Items.add(st);
  end;
end;




end.


unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, ColorBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.Button1Click(Sender: TObject);
begin

  paintbox1.Canvas.Pen.Color:=clGreen;
  paintbox1.Canvas.Pen.Width:=3;
  paintbox1.Canvas.Brush.Color:=clRed;
  paintbox1.Canvas.Ellipsec(75, 75, 25, 25);

  paintbox1.Canvas.Pen.Color:=clred;
  paintbox1.Canvas.brush.Color:=clGreen;
  paintbox1.Canvas.brush.style:=bscross;
  paintbox1.Canvas.Rectangle(110, 50, 210, 100);
  paintbox1.Canvas.Pen.Color:=clBlack;
  paintbox1.Canvas.Pen.Width:=10;
  paintbox1.Canvas.Line(10, 120, 260, 120);

  //brush.color:=clform;
  //font.color:=clblue;
  //font.name:='courier';
  //font.Size:=30;
  //font.style:=[fsbold];
  //TextOut(60,150,'рисунок');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  form1.close;
end;

end.


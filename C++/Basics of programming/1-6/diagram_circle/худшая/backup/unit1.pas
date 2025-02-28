unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, TASources, TAGraph, TASeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    //procedure Shape1ChangeBounds(Sender: TObject);
  private
  //Form1.Canvas.Pie(120,120,520,520,100,400,520,520);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DrawGrid1Click(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  begin
  paintbox1.canvas.pen.width:=3;
  paintbox1.canvas.pen.color:=clred;
  paintbox1.Canvas.ellipse(0,0,400,400);
  paintbox1.Canvas.Pie(0,0,400,400,200,200,400,400);
end;



end.


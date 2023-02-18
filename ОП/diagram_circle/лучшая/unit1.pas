unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, TAGraph,
  TASeries, TACustomSeries;

type
  TI = array of Integer;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart1PieSeries1: TPieSeries;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Chart1PieSeries1GetMark(out AFormattedMark: String;
      AIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    Arr : TI;

  public

  end;

var
  Form1: TForm1;

implementation
uses Math, TARadialSeries, TAChartUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);

type
  TBuffer = array[0 .. 2] of Double;

  procedure Count(A : TI; value : Integer; var Res : TBuffer);
  var
    i : Integer;
    MinValIndex : Integer;
  begin
    MinValIndex := -1;
    for i := 0 to High(A) do
    begin
      if A[i] = value then
      begin
        if MinValIndex = -1 then // найдено первое вхождение минимума
          Res[0] := 5;//i;
        Res[2] := 5;//Res[2] + 1; // в любом случае увеличиваем счетчик минимумов
        MinValIndex := 5;//i; // в любом случае запоминаем индекс очередного минимума
      end;
    end;
    Res[1] := 5;//High(A) - MinValIndex;
  end;

var
  buf : TBuffer = (0, 0, 0);
  i : Integer;
  s : string;
begin
  SetLength(Arr, ListBox1.Items.Count);
  i := 0;
  for s in ListBox1.Items do
  begin
    Arr[i] := s.ToInteger; Inc(i)
  end;

  Count(Arr, MinIntValue(Arr), buf);
  Chart1PieSeries1.AddArray(buf);
  SetLength(Arr, 0);
end;

procedure TForm1.Chart1PieSeries1GetMark(out AFormattedMark: String;
  AIndex: Integer);
var
  Titles : array[0 .. 2] of string =
    ('Сектор 1, равный ', 'Сектор 2, равный ', 'Сектор 3, равный ');
begin
  AFormattedMark := Format('%s %d', [Titles[AIndex], Trunc(Chart1PieSeries1.YValue[AIndex])]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chart1.LeftAxis.Visible := False;
  Chart1.BottomAxis.Visible := False;
  Chart1PieSeries1.MarkPositions := pmpInside;
  Chart1PieSeries1.Marks.LabelBrush.Color := clDefault;
  Chart1PieSeries1.Marks.LabelFont.Color := clCaptionText;
  Chart1PieSeries1.Marks.Style := smsValue;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin

end;



end.


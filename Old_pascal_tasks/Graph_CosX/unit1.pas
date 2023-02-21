unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, TAGraph,
  TASeries, TARadialSeries, TAFuncSeries, TAMultiSeries, TAPolygonSeries,
  TAExpressionSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1FuncSeries1: TFuncSeries;
    Label1: TLabel;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
procedure TForm1.Chart1FuncSeries1calculate(const AX: Double; out AY: Double);
begin
  AY := cos(AX)
end;

end.


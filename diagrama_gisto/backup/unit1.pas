unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, TAGraph,
  TASources, TASeries, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Label1: TLabel;
    RedChartSource: TRandomChartSource;
    procedure Chart1BarSeries1BeforeDrawBar(ASender: TBarSeries;
      ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Chart1BarSeries1BeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
begin

end;

end.


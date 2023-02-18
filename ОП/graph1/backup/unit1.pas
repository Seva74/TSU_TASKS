unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Out_value: TMemo;
    Out_iteraction: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.Button1Click(Sender: TObject);
var a,b,x,c,it:real;
begin
    a:=StrToFloat(edit1.Text);
    b:=StrToFloat(edit2.Text);
    x:=round(Cos(a)/b)*b;
    it:=1;
    while(b<1)do
      begin
      it:=it+1.8;
      b:=b*6;
      end;
    Out_value.Text:=FloatToStr(x);
    Out_iteraction.Text:=FloatToStr(round(it));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    Form1.Close;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin

end;

procedure TForm1.FormClick(Sender: TObject);
begin

end;

procedure TForm1.Label5Click(Sender: TObject);
begin

end;

{procedure TForm1.Button2Click(Sender: TObject);
begin

end; }

end.


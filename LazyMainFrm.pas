unit LazyMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LazyInitLib, Vcl.StdCtrls;

type
  TForm30 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    MeinIntWert: ILazyInit<Integer>;
  public
    { Public-Deklarationen }
  end;

var
  Form30: TForm30;

implementation

{$R *.dfm}

procedure TForm30.Button1Click(Sender: TObject);
begin
  Label1.Caption := IntToStr(MeinIntWert.Value);
end;

procedure TForm30.Button2Click(Sender: TObject);
var
  group: ILazyGroup;
  value1: TLazyInit<String>;
  value2: TLazyInit<String>;
begin
  Memo1.Lines.Add('START');
  group := LazyFactory.NewGroup;
  value1 := TLazyInit<String>.Create(
    function: String
    begin
      Memo1.Lines.Add('Init Value 1');
      Result := 'ABC';
    end, group);

  value2 := TLazyInit<String>.Create(
    function: String
    begin
      Memo1.Lines.Add('Init Value 2');
      Result := 'DEF';
    end, group);

  Memo1.Lines.Add('Set Label Caption');
  Label1.Caption := value1.Value;
  Memo1.Lines.Add('Did Set Label Caption');

  Memo1.Lines.Add('Set Button Caption');
  Button2.Caption := value2.Value;
  Memo1.Lines.Add('Did Set Button Caption');

  Memo1.Lines.Add('FINISH');
end;

procedure TForm30.FormCreate(Sender: TObject);
begin
  MeinIntWert := TLazyInit<Integer>.Create(
    function: Integer
    begin
      Result := 4711;
    end);
end;

end.

unit LazyMainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LazyInitLib, Vcl.StdCtrls;

type
  TKundeInitRec = record
    Name: String;
    VName: String;
    function GetName: String;
  end;
  TKunde = class(TObject)
  private
    FKundenDaten: TKundeInitRec;
    FName: ILazyInit<String>;
    FKndNr: Integer;
  public
    constructor Create;
    property KndNr: Integer read FKndNr;
    property Name: string read FKundenDaten.Name;
  end;

  TForm30 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
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
var
  kunde: TKunde;
begin
  kunde := TKunde.Create;
  kunde.FKndNr:= 10029;
//  label1.Caption := kunde.Name.Value;
//  Label1.Caption := IntToStr(MeinIntWert.Value);
end;

procedure TForm30.FormCreate(Sender: TObject);
begin
  MeinIntWert := TLazyInit<Integer>.Create(
    function: Integer
    begin
      Result := 4711;
    end);
end;

{ TKunde }

constructor TKunde.Create;
begin
  FName := TLazyInit<String>.Create(
    function: String
    begin
      Result := 'Kunde: '+IntTOStr(FKndNr);
    end);
end;

{ TKundeInitRec }

function TKundeInitRec.GetName: String;
begin
  Result := 'a';
end;

end.

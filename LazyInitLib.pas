unit LazyInitLib;

interface

uses
  System.SysUtils;

type
  ILazyInit<T> = interface
  ['{C2ECA0DA-B938-4015-813D-8D8709BF2D64}']
    function Value: T;
  end;

  TLazyInit<T> = class(TInterfacedObject, ILazyInit<T>)
  private
    FIsInit: Boolean;
    FValue: T;
    FGetValueFunction: TFunc<T>;
  public
    constructor Create(GetValueFunc: TFunc<T>);
    function Value: T;
  end;

implementation

{ TLazyInit<T> }

constructor TLazyInit<T>.Create(GetValueFunc: TFunc<T>);
begin
  FGetValueFunction := GetValueFunc;
  FIsInit := false;
end;

function TLazyInit<T>.Value: T;
begin
  if not FIsInit then
  begin
    FValue := FGetValueFunction;
    FIsInit := true;
  end;
  Result := FValue;
end;

end.

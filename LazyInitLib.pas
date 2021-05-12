unit LazyInitLib;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  ILazyInit<T> = interface
  ['{C2ECA0DA-B938-4015-813D-8D8709BF2D64}']
    function Value: T;
  end;

  ILazyInitializing = interface
  ['{CE406453-7C8F-4DB8-85CF-F8E2A3634AFC}']
    procedure DoInitialize;
  end;

  ILazyGroup = interface
  ['{5BA3B0DD-39AD-4623-B939-CAD25BAEC49F}']
    procedure DoInitialize;
    procedure RegisterLazy(lazy: ILazyInitializing);
  end;

  TLazyInit<T> = class(TInterfacedObject, ILazyInit<T>, ILazyInitializing)
  private
    FIsInit: Boolean;
    FValue: T;
    FGetValueFunction: TFunc<T>;
    FGroup: ILazyGroup;
    procedure DoInitialize;
  public
    constructor Create(GetValueFunc: TFunc<T>);overload;
    constructor Create(GetValueFunc: TFunc<T>; group: ILazyGroup);overload;
    function Value: T;
  end;

  LazyFactory = record
    class function NewGroup: ILazyGroup; static;
  end;

implementation

type
  TLazyGroup = class(TInterfacedObject, ILazyGroup)
  private
    FLazys: TList<ILazyInitializing>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoInitialize;
    procedure RegisterLazy(lazy: ILazyInitializing);
  end;

{ TLazyInit<T> }

constructor TLazyInit<T>.Create(GetValueFunc: TFunc<T>);
begin
  FGetValueFunction := GetValueFunc;
  FIsInit := false;
  FGroup := nil;
end;

constructor TLazyInit<T>.Create(GetValueFunc: TFunc<T>; group: ILazyGroup);
begin
  Create(GetValueFunc);
  FGroup := group;
  FGroup.RegisterLazy(Self);
end;

procedure TLazyInit<T>.DoInitialize;
begin
  if not FIsInit then
  begin
    FValue := FGetValueFunction;
    FIsInit := true;
    if FGroup <> nil then FGroup.DoInitialize;
  end;
end;

function TLazyInit<T>.Value: T;
begin
  DoInitialize;
  Result := FValue;
end;

{ TLazyGroup }

constructor TLazyGroup.Create;
begin
  FLazys := TList<ILazyInitializing>.Create;
end;

destructor TLazyGroup.Destroy;
begin
  FLazys.Free;
  inherited;
end;

procedure TLazyGroup.DoInitialize;
var
  lazy: ILazyInitializing;
begin
  for lazy in FLazys do
  begin
    lazy.DoInitialize;
  end;
end;

procedure TLazyGroup.RegisterLazy(lazy: ILazyInitializing);
begin
  FLazys.Add(lazy);
end;

{ LazyFactory }

class function LazyFactory.NewGroup: ILazyGroup;
begin
  Result := TLazyGroup.Create;
end;

end.

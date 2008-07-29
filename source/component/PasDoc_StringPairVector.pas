unit PasDoc_StringPairVector;
(* Redesign, based on TStrings.
  Reason: make doc generators independent from item/tag types.

  A proper solution would use an interface, to access the usual properties
  of all known items:
  Name = Names[i]
  Description = Values[i]
  Text(NameValueSep, ItemSep)

  Here properties are used for this purpose, but this doesn't help
  when the lists are assumed to have an specific type.
*)

interface

uses
  Classes,
  PasDoc_ObjectVector;

type
  TStringPair = class
    Name: string;
    Value: string;
    Data: Pointer;

    { Init Name and Value by @link(ExtractFirstWord) from S. }
    constructor CreateExtractFirstWord(const S: string);

    constructor Create(const AName, AValue: string; AData: Pointer = nil);
  end;

  TOwner = TObjectVector; //for now!

  { This is a list of string pairs.
    This class contains only non-nil objects of class TStringPair.

    Using this class instead of TStringList (with it's Name and Value
    properties) is often better, because this allows both Name and Value
    of each pair to safely contain any special characters (including '='
    and newline markers). It's also faster, since it doesn't try to
    encode Name and Value into one string. }
{$IFDEF old}
  TStringPairVector = class(TObjectVector)
{$ELSE}
  TStringPairVector = class(TStrings)
{$ENDIF}
  protected
    lst: TObjectVector;
  //implement abstracts
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const S: string); override;
    function  GetObject(index: integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  //our properties
    function  GetItems(i: Integer): TStringPair;
    procedure SetItems(i: Integer; Item: TStringPair);
    function  GetNames(i: integer): string;
    function  GetValues(i: integer): string;
  public
    constructor Create; virtual;  //reintroduce;
    constructor CreateIn(AOwner: TOwner);
    destructor Destroy; override;

    function  AddExtractFirstWord(const s: string): TStringPair;
    procedure AddPair(APair: TStringPair);

    { Returns all items Names and Values glued together.
      For every item, string Name + NameValueSepapator + Value is
      constructed. Then all such strings for every items all
      concatenated with ItemSeparator.

      Remember that the very idea of @link(TStringPair) and
      @link(TStringPairVector) is that Name and Value strings
      may contain any special characters, including things you
      give here as NameValueSepapator and ItemSeparator.
      So it's practically impossible to later convert such Text
      back to items and Names/Value pairs. }
    function Text(const NameValueSeparator, ItemSeparator: string): string;

    { Finds a string pair with given Name.
      Returns -1 if not found. }
    function FindName(const Name: string; IgnoreCase: boolean = true): Integer;

    { Removes first string pair with given Name.
      Returns if some pair was removed. }
    function DeleteName(const Name: string; IgnoreCase: boolean = true): boolean;

  //TStrings compatibility
    property Strings[i: integer]: string read GetNames;
  //TStringList overrides
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    property Items[i: Integer]: TStringPair read GetItems write SetItems; default;
    property Names[i: integer]: string read GetNames;
    property Values[i: integer]: string read GetValues;
    //property Objects[i: integer]:
  end;

function IsEmpty(const AOV: TStringPairVector): boolean; overload;
function IsEmpty(const AOV: TStrings): boolean; overload;

implementation

uses
  SysUtils { For LowerCase under Kylix 3 },
  PasDoc_Utils;

function IsEmpty(const AOV: TStringPairVector): boolean; overload;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

function IsEmpty(const AOV: TStrings): boolean; overload;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

{ TStringPair ---------------------------------------------------------------- }

constructor TStringPair.CreateExtractFirstWord(const S: string);
var
  FirstWord, Rest: string;
begin
  ExtractFirstWord(S, FirstWord, Rest);
  Create(FirstWord, Rest);
end;

constructor TStringPair.Create(const AName, AValue: string; AData: Pointer);
begin
  inherited Create;
  Name := AName;
  Value := AValue;
  Data := AData;
end;

{ TStringPairVector ---------------------------------------------------------- }

constructor TStringPairVector.Create;
begin
  inherited;
  lst := TObjectVector.Create(True);
end;

constructor TStringPairVector.CreateIn(AOwner: TOwner);
begin
  Create;
  if AOwner <> nil then
    AOwner.Add(self);
end;

destructor TStringPairVector.Destroy;
begin
  lst.Free;
  inherited;
end;

function TStringPairVector.GetItems(i: Integer): TStringPair;
begin
{$IFDEF old}
  Result := TStringPair(inherited Items[i]);
{$ELSE}
  Result := TStringPair(lst.Items[i]);
{$ENDIF}
end;

procedure TStringPairVector.SetItems(i: Integer; Item: TStringPair);
begin
{$IFDEF old}
  inherited Items[i] := Item;
{$ELSE}
  lst.Items[i] := Item;
{$ENDIF}
end;

function TStringPairVector.GetNames(i: integer): string;
begin
  Result := Items[i].Name;
end;

function TStringPairVector.GetValues(i: integer): string;
begin
  Result := Items[i].Value;
end;

function TStringPairVector.AddExtractFirstWord(
  const s: string): TStringPair;
begin
  Result := TStringPair.CreateExtractFirstWord(s);
  if Result.Name = '' then
    FreeAndNil(Result)  //this is considered an error, to be handled by caller
  else
    lst.Add(Result);
end;

procedure TStringPairVector.Clear;
begin
  lst.Clear;
end;

procedure TStringPairVector.Delete(Index: Integer);
begin
  lst.Delete(Index);
end;

function TStringPairVector.Get(Index: Integer): string;
begin
//we must construct an string, as expected by TStrings.GetValue/GetValueFromIndex
  Result := Items[index].Name + NameValueSeparator + Items[index].Value;
end;

function TStringPairVector.GetCount: Integer;
begin
  Result := lst.Count;
end;

function TStringPairVector.GetObject(index: integer): TObject;
begin
  Result := nil;  //Data: pointer???
end;

procedure TStringPairVector.Insert(Index: Integer; const S: string);
begin
  //nop
end;

procedure TStringPairVector.Put(Index: Integer; const S: string);
begin
  Items[index].Name := s;
end;

procedure TStringPairVector.PutObject(Index: Integer; AObject: TObject);
begin
  Items[index].Data := AObject; //?
end;

function TStringPairVector.Text(
  const NameValueSeparator, ItemSeparator: string): string;
var
  i: Integer;
  item: TStringPair;
begin
  if Count > 0 then begin
    item :=  Items[0];
    Result := item.Name + NameValueSeparator + item.Value;
    for i := 1 to Count - 1 do
      item := Items[i];
      Result := Result + ItemSeparator +
        item.Name + NameValueSeparator + item.Value;
  end;
end;

function TStringPairVector.FindName(const Name: string;
  IgnoreCase: boolean): Integer;
var
  LowerCasedName: string;
begin
  if IgnoreCase then
  begin
    LowerCasedName := LowerCase(Name);
    for Result := 0 to Count - 1 do
      if LowerCase(Items[Result].Name) = LowerCasedName then
        Exit;
    Result := -1;
  end else
  begin
    for Result := 0 to Count - 1 do
      if Items[Result].Name = Name then
        Exit;
    Result := -1;
  end;
end;

function TStringPairVector.DeleteName(const Name: string;
  IgnoreCase: boolean): boolean;
var
  i: Integer;
begin
  i := FindName(Name, IgnoreCase);
  Result := i <> -1;
  if Result then
    Delete(i);
end;

procedure TStringPairVector.AddPair(APair: TStringPair);
begin
  lst.Add(APair);
end;

end.

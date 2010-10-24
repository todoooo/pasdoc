{
  @cvs($Date$)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  a simple object vector
}
unit PasDoc_ObjectVector;

interface
uses
  Contnrs,
  Classes;

type
  TObjectVector = class(TObjectList)
  public
    { This is only to make constructor virtual, while original
      TObjectList has a static constructor. }
    constructor Create(const AOwnsObject: boolean); virtual;
{$IFDEF fpc}
{$ELSE}
  //fix bug in D7 TList.Sort
    procedure Sort(Compare: TListSortCompare); reintroduce;
{$ENDIF}
  end;

{$IFDEF old}
function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
{$ELSE}
function IsEmpty(const AOV: TObjectVector): boolean; overload;
{$ENDIF}

implementation

{$IFDEF old}
function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
{$ELSE}
function IsEmpty(const AOV: TObjectVector): boolean; overload;
{$ENDIF}
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

{ TObjectVector }

constructor TObjectVector.Create(const AOwnsObject: boolean);
begin
  inherited Create(AOwnsObject);
end;

{$IFDEF fpc}
{$ELSE}

(* Two bugs:
  Delphi QuickSort also compares arrays with 1 member,
    leading to an invalid pointer in the compare function (hard!)
  The compare function must return zero for equal items,
    otherwise an endless loop will result. (should have been cured)
*)
procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TObjectVector.Sort(Compare: TListSortCompare);
begin
  if Count <= 1 then
    exit;
{$IFnDEF new}
  inherited;
{$ELSE}
  //if (FList <> nil) and (Count > 1) then  //bug: should be: > 1
  if (List <> nil) and (Count > 1) then  //bug: should be: > 1
    QuickSort(List, 0, Count - 1, Compare);
{$ENDIF}
end;
{$ENDIF}

end.

unit fEditor;
(*< @abstract(Editor for external description files.)
  The editor allows to create and update external description files,
  residing in the Descriptions directory.
  Navigation between the items is implemented in various ways:

  - select from the combo box of all items

  - dbl click into the source, at or past the item to select

  - use the arrow buttons the select the next/previous item.
  The hollow arrows select items without any description.
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  PasDoc_Items;

type
  TEditBox = class(TForm)
    gbEdit: TGroupBox;
    edSrc: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    edRem: TMemo;
    buSave: TButton;
    cbItem: TComboBox;
    edDesc: TMemo;
    buPrev: TButton;
    buNext: TButton;
    buPrevEmpty: TButton;
    buNextEmpty: TButton;
    buPrevParent: TButton;
    buNextParent: TButton;
    procedure buSaveClick(Sender: TObject);
    procedure cbItemChange(Sender: TObject);
    procedure edRemChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure udAllChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure cbItemSelect(Sender: TObject);
    procedure udEmptyClick(Sender: TObject; Button: TUDBtnType);
    procedure udAllClick(Sender: TObject; Button: TUDBtnType);
    procedure buPrevClick(Sender: TObject);
    procedure buNextClick(Sender: TObject);
    procedure buPrevEmptyClick(Sender: TObject);
    procedure buNextEmptyClick(Sender: TObject);
    procedure edSrcDblClick(Sender: TObject);
    procedure buPrevParentClick(Sender: TObject);
    procedure buNextParentClick(Sender: TObject);
  private
    CurUnit: TPasUnit;
    CurItem: TPasItem; //intro/conclusion???
    RemChanged: boolean;
    FileChanged: boolean;
    DescFile: string;
    procedure InitItems;
    procedure LoadDescFile(const fn: string);
    procedure SaveRem;
    procedure SaveFile;
    procedure ItemChange(i: integer);
    function  DescribedItem(item: TPasItem): TPasItem;
  public
    DescDir: string;
    procedure LoadUnit(U: TPasUnit);
    procedure SelectItem(item: TPasItem);
  end;

var
  EditBox: TEditBox;

implementation

{$R *.dfm}

uses
  PasDoc_Languages, PasDoc_Utils;

{ TEditBox }

procedure TEditBox.LoadUnit(U: TPasUnit);
begin
//save old work?
//load unit
  CurUnit := U;
  gbEdit.Caption := u.SourceFileName;
  edSrc.Lines.LoadFromFile(u.SourceFileName);
  edRem.Clear;
  InitItems;
  if DescDir <> '' then begin
    LoadDescFile(DescDir + ChangeFileExt(ExtractFileName(u.SourceFileName), '.txt'));
  end else
    DescFile := ''; //prevent save
end;

procedure TEditBox.InitItems;

  procedure AddItem(item: TPasItem);
  var
    del: TPasDelegate;
  begin
  //for now: init whole file, with (top level) items, recursively
    //edRem.Lines.Add('#' + item.QualifiedName);
    del := TPasDelegate.Create(item.QualifiedName);
    del.PasItem := item;
    cbItem.AddItem(item.QualifiedName, del);
  end;

  procedure AddMembers(scope: TPasScope);
  var
    i: integer;
    item: TPasItem;
  begin
    if scope = nil then
      exit;
    for i := 0 to scope.Members.Count - 1 do begin
      item := scope.Members.PasItemAt[i];
    //prevent recursion for used units
      if item.ID <> trUnit then begin
        AddItem(item);
        if item is TPasScope then
          AddMembers(TPasScope(item));
      end;
    end;
  end;

var
  i: integer;
begin
//clear item list
  for i := 0 to cbItem.Items.Count - 1 do begin
    cbItem.Items.Objects[i].Free;
  end;
  cbItem.Clear;
//init item list
  AddItem(CurUnit);
  AddMembers(CurUnit);
end;

procedure TEditBox.LoadDescFile(const fn: string);
var
  lst: TStringList;
  i, iAnchor: integer;
  a, s, d: string;
  item: TPasDelegate;

  procedure SaveItem;
  begin
    if (a <> '') and (d <> '') then begin
      iAnchor := cbItem.Items.IndexOf(a);
      if iAnchor >= 0 then begin
        TObject(item) := cbItem.Items.Objects[iAnchor];
        item.Value := d; //collected description
      end; //else try partial match? Exclude unit name?
    end;
  end;

begin //LoadDescFile
//save file?
  SaveFile;

  DescFile := fn;

  if FileExists(fn) then begin
    lst := TStringList.Create;
    try
      lst.LoadFromFile(fn);
      iAnchor := -1;
      d := '';
      a := '';
      for i := 0 to lst.Count - 1 do begin
        s := lst[i];
        if (s <> '') and (s[1] = '#') then begin //found anchor
        //save preceding item
          SaveItem;
        //init next anchor
          a := copy(s, 2, Length(s));
          d := '';
        end else //if d <> '' then
          d := d + s + LineEnding; //text always ends with EOL
      end;
      SaveItem; //the last one
    finally
      lst.Free;
    end;
  end else begin
  //what?
  end;
end;

procedure TEditBox.SelectItem(item: TPasItem);
begin
  if item = nil then
    exit;
  if item.MyUnit <> CurUnit then
    exit; //for now, may load unit file?
//try find item
  cbItem.ItemIndex := -1; //force change notification
  cbItem.Text := item.QualifiedName; //select it?
end;

procedure TEditBox.SaveFile;
var
  strm: TFileStream;
  i: integer;
  item: TPasDelegate;
  s: string;
begin
  if DescFile = '' then
    exit;
  SaveRem; //in case it was changed
  if not FileChanged then
    exit;
  strm := TFileStream.Create(DescFile, fmCreate); //???
  try
    for i := 0 to cbItem.Items.Count - 1 do begin
      TObject(item) := cbItem.Items.Objects[i];
      if item.Value <> '' then begin
        s := '#' + item.Name + LineEnding;
        strm.Write(s[1], Length(s));
        strm.Write(item.Value[1], Length(item.Value));
      end;
    end;
  finally
    strm.Free;
  end;
  FileChanged := False;
end;

procedure TEditBox.SaveRem;
var
  s: string;
begin
  if RemChanged then begin
    if assigned(CurItem) then begin
      s := edRem.Text;
    //force EOL at the end of the rem
      if (s <> '') and (s[Length(s)] >= ' ') then
        s := s + LineEnding;
      CurItem.Value := s;
    end;
    RemChanged := False;
  end;
end;

//----------------------- GUI ---------------------------------

procedure TEditBox.cbItemSelect(Sender: TObject);
begin
  ItemChange(cbItem.ItemIndex);
end;

procedure TEditBox.cbItemChange(Sender: TObject);
begin
  ItemChange(cbItem.ItemIndex);
end;

function TEditBox.DescribedItem(item: TPasItem): TPasItem;
begin
(* Find an item with a description.
  If docs have been created, the PasItem also contains inherited descriptions,
  else the ancestry must be inspected.
  Further calls try FirstAncestorItem, for inherited descriptions.
*)
  Result := item.PasItem; //resolve delegates, from editor or class ancestry
  while (Result <> nil) and not Result.HasDescription and (Result.RawDescription = '') do
    Result := Result.FirstAncestorItem.PasItem;
end;

procedure TEditBox.ItemChange(i: integer);
var
  ifp: integer;
  PasItem: TPasItem;
begin
  SaveRem;
  edRem.Clear;
  edDesc.Clear;
  CurItem := nil;
{$IFDEF old}
  i := cbItem.ItemIndex;
{$ELSE}
{$ENDIF}
  if i < 0 then
    exit;
  cbItem.ItemIndex := i;
  TObject(CurItem) := cbItem.Items.Objects[i];
//in source
  PasItem := CurItem.PasItem;
  ifp := PasItem.NamePosition;
  if ifp >= 0 then begin
  //hilite declaration
    edSrc.SelLength := 0;
    edSrc.SelStart := ifp;
    edSrc.SelLength := Length(PasItem.Name);
  end else
    edSrc.SelLength := 0;
//item
  edRem.Text := CurItem.Value;
  PasItem := DescribedItem(CurItem.PasItem); //resolve delegate, search for inherited description
  if PasItem <> nil then begin
    edDesc.Text := PasItem.RawDescription; //else no description at all
    if edDesc.Text = '' then //also try inherited description
      edDesc.Text := PasItem.AbstractDescription + PasItem.DetailedDescription; //valid in analysis?
  end else
    edDesc.Text := '';
end;

procedure TEditBox.edRemChange(Sender: TObject);
begin
  RemChanged := True;
  FileChanged := True;
end;

procedure TEditBox.buSaveClick(Sender: TObject);
begin
  SaveFile;
end;

procedure TEditBox.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FileChanged then begin
    case MessageBox(self.WindowHandle, 'Save changes?', 'PasDoc2 Editor', MB_YESNOCANCEL) of
    mrYes: SaveFile;
    //mrNo,
    mrCancel:
      begin
        CanClose := False;
        exit;
      end;
    end;
  end;
  CanClose := True;
end;

procedure TEditBox.udAllChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
(* select the prev/next item.
  The new position should depend on the currently selected item,
  the up/down position should be ignored/adjusted accordingly.
  Wrap around?
*)
  case Direction of
  updUp:  //select previous item
    AllowChange := NewValue >= 0;
  updDown:  //select next item
    AllowChange := NewValue < cbItem.Items.Count;
  end;
  if AllowChange then
    ItemChange(NewValue);
end;

(* The Button property is absolutely unreliable :-(
  That's why I use distinct buttons for the UpDown controls.
*)

procedure TEditBox.buPrevParentClick(Sender: TObject);
var
  i: integer;
  item: TDescriptionItem;
  par: TPasItem;
begin
  i := cbItem.ItemIndex;
  if i < 1 then
    exit;
  TObject(item) := cbItem.Items.Objects[i];
  par := item.PasItem.MyOwner;
  if par = nil then
    exit;
  repeat
    dec(i);
    TObject(item) := cbItem.Items.Objects[i];
    if item.PasItem = par then
      break;
  until i < 1;
  ItemChange(i);
end;

procedure TEditBox.buNextParentClick(Sender: TObject);
var
  i, n: integer;
  item: TDescriptionItem;
  par: TPasItem;
begin
  i := cbItem.ItemIndex;
  if i < 1 then
    exit;
  TObject(item) := cbItem.Items.Objects[i];
  par := item.PasItem.MyOwner;
  if par = nil then
    exit;
  n := cbItem.Items.Count - 1;
  while i < n do begin
    inc(i);
    TObject(item) := cbItem.Items.Objects[i];
    if item.PasItem.MyOwner <> par then
      break;
  end;
  ItemChange(i);
end;

procedure TEditBox.udAllClick(Sender: TObject; Button: TUDBtnType);
var
  i: integer;
begin
(* Move to the next/prev item
*)
  i := cbItem.ItemIndex;
  case Button of
  btNext: inc(i);
  btPrev: dec(i);
  end;
  if cardinal(i) < cardinal(cbItem.Items.count) then begin
    //udEmpty.Position := i;
    //udAll.Position := i;
    ItemChange(i);
  end;
end;

procedure TEditBox.udEmptyClick(Sender: TObject; Button: TUDBtnType);
var
  i, d: integer;
  item: TDescriptionItem;
  described: boolean;
begin
(* Move to the next/prev item without a description.
  The items in the combo box are delegates, so
  descriptions must be searched in the PasItem.
  If no docs are created, search for inherited descriptions.
*)
  i := cbItem.ItemIndex;
  case Button of
  btNext: d := 1;
  //btPrev:
  else d := -1;
  end;
  i := i+d;
  while cardinal(i) < cardinal(cbItem.Items.count) do begin
    TObject(item) := cbItem.Items.Objects[i];
    described := (item.Value <> '') //external description?
      or (DescribedItem(item.PasItem) <> nil);
    if not described then begin //no description available
      ItemChange(i);
      break;
    end;
    i := i+d; //try next item
  end;
end;

procedure TEditBox.buPrevClick(Sender: TObject);
begin
  udAllClick(Sender, btPrev);
end;

procedure TEditBox.buNextClick(Sender: TObject);
begin
  udAllClick(Sender, btNext);
end;

procedure TEditBox.buPrevEmptyClick(Sender: TObject);
begin
  udEmptyClick(Sender, btPrev);
end;

procedure TEditBox.buNextEmptyClick(Sender: TObject);
begin
  udEmptyClick(Sender, btNext);
end;

procedure TEditBox.edSrcDblClick(Sender: TObject);
var
  i, p: integer;
  item: TDescriptionItem;
begin
(* select the item at or before the current position.
*)
  p := edSrc.SelStart; //point of click?
  for i := cbItem.Items.Count - 1 downto 0 do begin
    TObject(item) := cbItem.Items.Objects[i];
    if item.PasItem.NamePosition <= p then begin
      ItemChange(i);
      break;
    end;
  end;
end;

end.



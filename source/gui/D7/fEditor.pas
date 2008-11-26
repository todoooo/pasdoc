unit fEditor;

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
    buInit: TButton;
    buSave: TButton;
    cbItem: TComboBox;
    edDesc: TMemo;
    procedure buSaveClick(Sender: TObject);
    procedure cbItemChange(Sender: TObject);
    procedure edRemChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
        if s[1] = '#' then begin //found anchor
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

procedure TEditBox.cbItemChange(Sender: TObject);
var
  i, ifp: integer;
begin
  SaveRem;
  edRem.Clear;
  CurItem := nil;
  i := cbItem.ItemIndex;
  if i < 0 then
    exit;
  TObject(CurItem) := cbItem.Items.Objects[i];
  edRem.Text := CurItem.Value;
  //edDesc.Text := CurItem.PasItem.DetailedDescription; //valid in analysis?
  //if edDesc.Text = '' then
    edDesc.Text := CurItem.PasItem.RawDescription;
//in source
  //CurItem := item;
  //edLink.Text := item.QualifiedName;
  //cbItem.Text := item.QualifiedName; //select it?
  ifp := CurItem.PasItem.NamePosition;
  if ifp >= 0 then begin
  //hilite declaration
    edSrc.SelLength := 0;
    edSrc.SelStart := ifp;
    edSrc.SelLength := Length(CurItem.PasItem.Name);
    //edSrc.Refresh;
  //search description
  end else
    edSrc.SelLength := 0;
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

end.


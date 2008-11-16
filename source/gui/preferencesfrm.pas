{ This unit implements TPreferences form (run by TPreferences.Execute).

  @author(Michalis Kamburelis) }

unit PreferencesFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TPreferences }

  TPreferences = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnResetDefaults: TButton;
    EditWWWBrowserCommand: TEdit;
    EditWWWHelpServer: TEdit;
    LabelWWWBrowserCommand: TLabel;
    LabelWWWHelpServer: TLabel;
    procedure BtnResetDefaultsClick(Sender: TObject);
  private
    { private declarations }
  public
    class procedure Execute;
  end; 

implementation

uses
{$IFDEF WIN32}
{$ELSE}
    WWWBrowserRunnerDM,
{$ENDIF}
    PasDocGuiSettings;

procedure TPreferences.BtnResetDefaultsClick(Sender: TObject);
begin
{$IFDEF WIN32}
{ TODO : Browser? }
{$ELSE}
  EditWWWBrowserCommand.Text := DefaultWWWBrowserCommand;
  EditWWWHelpServer.Text := DefaultWWWHelpServer;
{$ENDIF}
end;

class procedure TPreferences.Execute;
var
  F: TPreferences;
begin
{$IFDEF WIN32}
{ TODO : Preferences?
 }
{$ELSE}
  F := TPreferences.Create(nil);
  try
    F.EditWWWBrowserCommand.Text := WWWBrowserRunner.BrowserCommand;
    F.EditWWWHelpServer.Text := WWWHelpServer;
    if F.ShowModal = mrOK then
    begin
      WWWBrowserRunner.BrowserCommand := F.EditWWWBrowserCommand.Text;
      WWWHelpServer := F.EditWWWHelpServer.Text;
    end;
  finally
    F.Free;
  end;
{$ENDIF}
end;

initialization
  {$I preferencesfrm.lrs}
end.


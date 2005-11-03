{ This unit implements TPreferences form (run by TPreferences.Execute)
  and declares some global settings (that didn't fit anywhere else)
  like WWWHelpServer. }

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
    EditWWWBrowserCommand: TEdit;
    EditWWWHelpServer: TEdit;
    LabelWWWBrowserCommand: TLabel;
    LabelWWWHelpServer: TLabel;
  private
    { private declarations }
  public
    class procedure Execute;
  end; 

var
  WWWHelpServer: string;

implementation

uses WWWBrowserRunnerDM;

class procedure TPreferences.Execute;
var
  F: TPreferences;
begin
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
end;

initialization
  {$I preferencesfrm.lrs}
  
  { Assign default value for WWWHelpServer }
  WWWHelpServer := 'http://pasdoc.sipsolutions.net/';
end.


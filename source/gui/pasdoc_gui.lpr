program pasdoc_gui;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, frmAboutUnit, frmHelpGeneratorUnit, pasdoc_package,
  HelpProcessor,
{$IFDEF WIN32}
{$ELSE}
  WWWBrowserRunnerDM,
{$ENDIF}
  PreferencesFrm, PasDocGuiSettings;

begin
  Application.Initialize;
  Application.CreateForm(TfrmHelpGenerator, frmHelpGenerator);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.


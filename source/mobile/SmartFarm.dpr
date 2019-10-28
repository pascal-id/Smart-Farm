program SmartFarm;

uses
  System.Classes,
  System.StartUpCopy,
  FMX.Forms,
  iSolution.lib.Logger in 'Source\iSolution.lib.Logger.pas',
  iSolution.lib.Configuration in 'Source\iSolution.lib.Configuration.pas',
  iSolution.lib.Path in 'Source\iSolution.lib.Path.pas',
  iSolutions.lib.ListItemControls in 'Source\iSolutions.lib.ListItemControls.pas',
  iSolutions.lib.VKKeyboardHelper in 'Source\iSolutions.lib.VKKeyboardHelper.pas',
  iSolution.lib.StringHelper in 'Source\iSolution.lib.StringHelper.pas',
  SF.dlg.Login in 'Source\SF.dlg.Login.pas' {frmLogin},
  SF.vwr.Account in 'Source\SF.vwr.Account.pas' {frmUserAccount},
  SF.vwr.Home in 'Source\SF.vwr.Home.pas' {frmHome},
  SF.vwr.Schedule in 'Source\SF.vwr.Schedule.pas' {frmSchedule},
  SF.vwr.ScheduleAdd in 'Source\SF.vwr.ScheduleAdd.pas' {frmScheduleAdd},
  SF.vwr.Settings in 'Source\SF.vwr.Settings.pas' {frmSettings},
  SF.vwr.SettingsGeneral in 'Source\SF.vwr.SettingsGeneral.pas' {frmSettingsGeneral},
  SF.vwr.SettingsGeneralHumidity in 'Source\SF.vwr.SettingsGeneralHumidity.pas' {frmSettingsGeneralHumidity},
  SF.vwr.SettingsStation in 'Source\SF.vwr.SettingsStation.pas' {frmSettingsStation},
  SF.lib.SmartFarmHelpoer in 'Source\SF.lib.SmartFarmHelpoer.pas',
  SF.lib.SmartFarmAPI in 'Source\SF.lib.SmartFarmAPI.pas',
  SF.bsx.SmartFarm in 'Source\SF.bsx.SmartFarm.pas' {frmBase},
  SF.bsx.SmartFarmChild in 'Source\SF.bsx.SmartFarmChild.pas' {frmBaseChild},
  SF.bsx.SmartFarmMain in 'Source\SF.bsx.SmartFarmMain.pas' {frmBaseMain};

{$R *.res}

begin
  Application.Initialize;
  //  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmUserAccount, frmUserAccount);
  Application.CreateForm(TfrmHome, frmHome);
  Application.CreateForm(TfrmSchedule, frmSchedule);
  Application.CreateForm(TfrmScheduleAdd, frmScheduleAdd);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmSettingsGeneral, frmSettingsGeneral);
  Application.CreateForm(TfrmSettingsGeneralHumidity, frmSettingsGeneralHumidity);
  Application.CreateForm(TfrmSettingsStation, frmSettingsStation);
  Application.CreateForm(TfrmBase, frmBase);
  Application.CreateForm(TfrmBaseChild, frmBaseChild);
  Application.CreateForm(TfrmBaseMain, frmBaseMain);
  Application.Run;
end.

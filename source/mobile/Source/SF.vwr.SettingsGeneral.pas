unit SF.vwr.SettingsGeneral;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  SF.bsx.SmartFarmChild;

type
  TfrmSettingsGeneral = class(TfrmBaseChild)
    Layout1: TLayout;
    lytSettingHumidity: TLayout;
    lblSettingHumidity: TLabel;
    lblSettingHumidityCaption: TLabel;
    btnMore: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnMoreClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSettingsGeneral: TfrmSettingsGeneral;

implementation

uses SF.vwr.SettingsGeneralHumidity,
     SF.lib.SmartFarmHelpoer;

{$R *.fmx}

procedure TfrmSettingsGeneral.FormCreate(Sender: TObject);
begin
  inherited;
  tbcDevices.TabIndex := 2;
end;

procedure TfrmSettingsGeneral.btnMoreClick(Sender: TObject);
var
  frmSettingGeneral : TCustomForm;
begin
  frmSettingGeneral := TCSClass_SmartFarmHelper.IsFormLoaded('frmSettingGeneralHumidity');
  if frmSettingGeneral = nil then
  begin
    frmSettingGeneral := TfrmSettingsGeneralHumidity.Create(Self);
  end;
  frmSettingGeneral.Show;
end;

end.

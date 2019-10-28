unit SF.vwr.SettingsGeneralHumidity;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  SF.bsx.SmartFarmChild, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Gestures, System.ImageList, FMX.ImgList, FMX.Controls.Presentation,
  FMX.Layouts, FMX.Edit;

type
  TfrmSettingsGeneralHumidity = class(TfrmBaseChild)
    lytHumidity: TLayout;
    lblHumidityCaption: TLabel;
    edtHumidity: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSettingsGeneralHumidity: TfrmSettingsGeneralHumidity;

implementation

{$R *.fmx}

{-----------------------------------------------------------------------------
  Procedure : FormCreate
  Author    : iwancs
  Date      : 24 Oct 2019
  Arguments : Sender : TObject;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmSettingsGeneralHumidity.FormCreate(Sender: TObject);
begin
  inherited;
  tbcDevices.TabIndex := 2;
end;

end.

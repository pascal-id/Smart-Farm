unit SF.vwr.Account;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Objects,
  SF.bsx.SmartFarmChild, SF.bsx.SmartFarm;

type
  TfrmUserAccount = class(TfrmBaseChild)
    lytProfilePicture: TLayout;
    elpsProfilePicture: TEllipse;
    lblUsername: TLabel;
    lblUserDescription: TLabel;
    btnLogout: TCornerButton;
    lytButtonLogout: TLayout;
    procedure FormCreate(Sender: TObject);
  end;

var
  frmUserAccount: TfrmUserAccount;

implementation

{$R *.fmx}

procedure TfrmUserAccount.FormCreate(Sender: TObject);
begin
  inherited;
  tbcDevices.TabIndex := 2;
end;

end.

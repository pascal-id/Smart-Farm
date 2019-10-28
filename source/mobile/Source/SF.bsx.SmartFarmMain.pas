unit SF.bsx.SmartFarmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  SF.bsx.SmartFarm, FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts;

type
  TfrmBaseMain = class(TfrmBase)
    lblTitle: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift:
        TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBaseMain: TfrmBaseMain;

implementation

uses FMX.DialogService, iSolutions.lib.VKKeyboardHelper;

{$R *.fmx}

procedure TfrmBaseMain.FormKeyDown(
  Sender: TObject;
  var Key: Word;
  var KeyChar: Char;
  Shift: TShiftState
);

begin
  TCSClass_BackButtonHelper.Handle_HardwareBack(
    Key,
    function : Boolean
    var
      bResult : Boolean;
    begin
      bResult := False;
      MessageDlg(
        'Yakin akan keluar?',
        TMsgDlgType.mtConfirmation,
        mbYesNo,
        0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrYes then
          begin
            bResult := True;
            Application.Terminate;
          end;
        end
      );
      Result := bResult;
    end
  );
  begin

  end;
end;

end.

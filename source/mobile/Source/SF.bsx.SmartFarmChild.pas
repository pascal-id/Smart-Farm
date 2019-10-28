unit SF.bsx.SmartFarmChild;

interface

uses System.Classes, System.Actions, System.ImageList, System.Types,
     FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
     FMX.TabControl, FMX.ActnList, FMX.Types,
     FMX.Gestures, FMX.ImgList, FMX.StdCtrls, FMX.ListView,
     FMX.Layouts, FMX.Controls, FMX.Controls.Presentation,
     FMX.Forms, FMX.Graphics,
     SF.bsx.SmartFarm;

type
  TfrmBaseChild = class(TfrmBase)
    lblTitle: TLabel;
    btnBack: TButton;
    lblBackCaption: TLabel;
    procedure btnBackClick(Sender: TObject);
  end;

var
  frmBaseChild: TfrmBaseChild;

implementation

{$R *.fmx}

{-----------------------------------------------------------------------------
  Procedure : btnBackClick
  Author    : iwancs
  Date      : 24 Oct 2019
  Arguments : Sender : TObject;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmBaseChild.btnBackClick(Sender: TObject);
begin
  Close;
end;

end.

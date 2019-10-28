unit SF.bsx.SmartFarm;

interface

uses System.Classes, System.Actions, System.ImageList, System.Types,
     FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
     FMX.TabControl, FMX.ActnList, FMX.Types,
     FMX.Gestures, FMX.ImgList, FMX.StdCtrls, FMX.ListView,
     FMX.Layouts, FMX.Controls, FMX.Controls.Presentation,
     FMX.Forms, FMX.Graphics;

type
  TfrmBase = class(TForm)
    ilButtons: TImageList;
    tbcDevices: TTabControl;
    gstrmngrNavigation: TGestureManager;
    actlstNavigation: TActionList;
    actNextPanel: TNextTabAction;
    actPreviousPanel: TPreviousTabAction;
    lytHeader: TLayout;
    tbtmHome: TTabItem;
    tbtmSchedule: TTabItem;
    tbtmSettings: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure tbcDevicesChange(Sender: TObject);

    procedure tbtmHomeClick(Sender: TObject);
    procedure tbtmSettingsClick(Sender: TObject);
    procedure tbtmScheduleClick(Sender: TObject);
  private
    FPainted : Boolean;

    FOnFullyLoaded: TNotifyEvent;

  public
    property OnFullyLoaded : TNotifyEvent
      read FOnFullyLoaded write FOnFullyLoaded;
  end;

var
  frmBase: TfrmBase;

implementation

uses SF.vwr.Home, SF.vwr.Schedule, SF.vwr.Settings,
     SF.lib.SmartFarmHelpoer ;

{$R *.fmx}

procedure TfrmBase.FormCreate(Sender: TObject);
begin
  FPainted := False;
end;

procedure TfrmBase.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if not FPainted then
  begin
    FPainted := True;
    if Assigned(FOnFullyLoaded) then
    begin
      FOnFullyLoaded(Sender);
    end;
  end;
end;

procedure TfrmBase.tbcDevicesChange(Sender: TObject);
begin
  if tbcDevices.ActiveTab = tbtmHome then
  begin
    tbtmHomeClick(tbtmHome);
  end
  else
  if tbcDevices.ActiveTab = tbtmSchedule then
  begin
    tbtmScheduleClick(tbtmSchedule);
  end
  else
  if tbcDevices.ActiveTab = tbtmSettings then
  begin
    tbtmSettingsClick(tbtmSettings);
  end;
end;

procedure TfrmBase.tbtmHomeClick(Sender: TObject);
var
  frmTemp : TCustomForm;
begin
  frmTemp := TCSClass_SmartFarmHelper.IsFormLoaded('frmHome');
  if frmTemp = nil then
  begin
    frmTemp := TfrmHome.Create(Self);
  end;
  TfrmBase(frmTemp).tbcDevices.ActiveTab := TfrmBase(frmTemp).tbtmHome;
  frmTemp.Show;
end;

procedure TfrmBase.tbtmSettingsClick(Sender: TObject);
var
  frmTemp : TCustomForm;
begin
  frmTemp := TCSClass_SmartFarmHelper.IsFormLoaded('frmSettings');
  if frmTemp = nil then
  begin
    frmTemp := TfrmSettings.Create(Self);
  end;
  TfrmBase(frmTemp).tbcDevices.ActiveTab := TfrmBase(frmTemp).tbtmSettings;
  frmTemp.Show;
end;

procedure TfrmBase.tbtmScheduleClick(Sender: TObject);
var
  frmTemp : TCustomForm;
begin
  frmTemp := TCSClass_SmartFarmHelper.IsFormLoaded('frmSchedule');
  if frmTemp = nil then
  begin
    frmTemp := TfrmSchedule.Create(Self);
  end;
  TfrmBase(frmTemp).tbcDevices.ActiveTab := TfrmBase(frmTemp).tbtmSchedule;
  frmTemp.Show;
end;

end.

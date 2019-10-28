unit SF.vwr.Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Edit, FMX.ComboEdit, FMX.Objects, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  SF.bsx.SmartFarmMain, SF.bsx.SmartFarm;

type
  TfrmSettings = class(TfrmBaseMain)
    lytStation: TLayout;
    imgProfile: TImage;
    lblAccountName: TLabel;
    lblDescription: TLabel;
    lvSettingList: TListView;
    ilListViewImages: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure lvSettingListItemClickEx(const Sender: TObject; ItemIndex: Integer;
        const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
  private
    procedure FillSettingList;
    procedure Event_FullyLoaded(Sender : TObject);
  public
    { Public declarations }
  end;

var
  frmSettings: TfrmSettings;

implementation

uses SF.vwr.SettingsGeneral, SF.vwr.SettingsStation,
     SF.lib.SmartFarmHelpoer;

{$R *.fmx}

procedure TfrmSettings.FillSettingList;
var
  itmRecord : TListViewItem;
  itmColumn : TListItemDrawable;
begin
  itmRecord := lvSettingList.Items.Add;
  itmRecord.Text := 'Station Controls';
  itmRecord.ImageIndex := 0;


  itmRecord := lvSettingList.Items.Add;
  itmRecord.Text := 'General Settings';
  itmRecord.ImageIndex := 1;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  inherited;
  OnFullyLoaded := Event_FullyLoaded;
end;

{ TfrmSettings }

procedure TfrmSettings.Event_FullyLoaded(Sender: TObject);
begin
  tbcDevices.TabIndex := 2;
  FillSettingList;
end;

procedure TfrmSettings.lvSettingListItemClickEx(
  const Sender: TObject;
  ItemIndex: Integer;
  const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
var
  frmSetting : TCustomForm;
begin
  if ItemObject is TListItemAccessory then
  begin
    if ItemIndex = 0 then
    begin
      frmSetting := TCSClass_SmartFarmHelper.IsFormLoaded('frmSettingsStation');
      if frmSetting = nil then
      begin
        frmSetting := TfrmSettingsStation.Create(Self);
      end;
      frmSetting.Show;
    end
    else
    if ItemIndex = 1 then
    begin
      frmSetting := TCSClass_SmartFarmHelper.IsFormLoaded('frmSettingsGeneral');
      if frmSetting = nil then
      begin
        frmSetting := TfrmSettingsGeneral.Create(Self);
      end;
      frmSetting.Show;
    end;
  end;
end;

end.

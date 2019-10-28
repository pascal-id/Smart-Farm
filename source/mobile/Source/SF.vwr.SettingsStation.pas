unit SF.vwr.SettingsStation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  SF.bsx.SmartFarmChild, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox,
  SF.lib.SmartFarmAPI,iSolutions.lib.ListItemControls;

type
  TfrmSettingsStation = class(TfrmBaseChild)
    lvSettingList: TListView;
    lytStation: TLayout;
    cbbStationList: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure lvSettingListItemClickEx(const Sender: TObject; ItemIndex: Integer;
        const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure lvSettingListUpdateObjects(const Sender: TObject; const AItem:
        TListViewItem);
  private
    FStationList : TCSRec_Stations;

    procedure OnSwitch_Switch(Sender : TObject);

    procedure AddScheduleList(
      AIndex : string;
      ASubject : string;
      ADescription : string;
      AStatus : Boolean
    );
    procedure FillSettingList;
    procedure Event_FullyLoaded(Sender : TObject);
  end;

var
  frmSettingsStation: TfrmSettingsStation;

implementation

uses SF.lib.SmartFarmHelpoer;

{$R *.fmx}

procedure TfrmSettingsStation.FormCreate(Sender: TObject);
begin
  inherited;
  OnFullyLoaded := Event_FullyLoaded;
end;

{ TfrmSettingsStation }

{-----------------------------------------------------------------------------
  Procedure : Event_FullyLoaded
  Author    : iwancs
  Date      : 23 Oct 2019
  Arguments : Sender : TObject;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmSettingsStation.AddScheduleList(
  AIndex : string;
  ASubject : string;
  ADescription: string;
  AStatus: Boolean);
var
  itmRecord : TListViewItem;
begin
  if ASubject <> '' then
  begin
    itmRecord := lvSettingList.Items.Add;
    itmRecord.Text := ASubject;
    itmRecord.Detail := ADescription;
    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').IsChecked := AStatus;
    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').TagString := AIndex;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').SwitchRound := 8;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').OnColor := $FFB7D3FF;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').OffColor := $FFCBCBCB;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').ThumbOnColor := $FF5599FF;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').ThumbOffColor := $FF939393;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').ThumbHeight :=
//      itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').Height + 8;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').ThumbWidth :=
//      itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').Height + 8;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').ThumbRound := 15;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').IsChecked := AStatus;
//    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').OnSwitch :=
//      OnSwitch_Switch;
  end;
end;

procedure TfrmSettingsStation.Event_FullyLoaded(Sender: TObject);
begin
  tbcDevices.TabIndex := 2;
  FStationList := TCSClass_SmartFarmHelper.FillStationList(cbbStationList,False);
  FillSettingList;
end;

{-----------------------------------------------------------------------------
  Procedure : FillSettingList
  Author    : iwancs
  Date      : 23 Oct 2019
  Arguments : ;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmSettingsStation.FillSettingList;
//var
//  itmRecord : TListViewItem;
//  itmColumn : TListItemDrawable;
begin
  AddScheduleList(
    '1',
    'Sprinkle',
    'Will affect on all nodes and not automatically off',
    False
  );
  AddScheduleList(
    '2',
    'Temperature Sensor',
    'Will affect on all nodes',
    False
  );
  AddScheduleList(
    '3',
    'Humidity Sensor',
    'Will affect on all nodes',
    False
  );

//  itmRecord := lvSettingList.Items.Add;
//  itmRecord.Text := 'Sprinkle';
//  itmRecord.Detail := 'Will affect on all nodes and not automatically off';
//
//
//  itmRecord := lvSettingList.Items.Add;
//  itmRecord.Text := 'Temperature Sensor';
//  itmRecord.Detail := 'Will affect on all nodes';
//
//  itmRecord := lvSettingList.Items.Add;
//  itmRecord.Text := 'Humidity Sensor';
//  itmRecord.Detail := 'Will affect on all nodes';

end;

procedure TfrmSettingsStation.lvSettingListItemClickEx(
  const Sender: TObject;
  ItemIndex: Integer;
  const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable
);
begin
  inherited;
  if ItemObject is TListItemAccessory then
  begin
    if ItemIndex = 0 then
    begin

    end;
  end;
//
end;

procedure TfrmSettingsStation.lvSettingListUpdateObjects(
  const Sender: TObject;
  const AItem: TListViewItem);
var
  S: TListItemSwitch;
begin
  S:= AItem.Objects.FindObject('Switch') as TListItemSwitch;
  if S = nil then
  begin
    S:= TListItemSwitch.Create(AItem);
    S.Name:= 'Switch';
    S.Align:= TListItemAlign.Trailing;
    S.VertAlign:= TListItemAlign.Center;
    S.SwitchRound := 8;
    S.OnColor := $FFB7D3FF;
    S.OffColor := $FFCBCBCB;
    S.ThumbOnColor := $FF5599FF;
    S.ThumbOffColor := $FF939393;
    S.ThumbHeight := S.Height + 8;
    S.ThumbWidth := S.Height + 8;
    S.ThumbRound := 15;
    S.OnSwitch := OnSwitch_Switch;
  end;
end;

procedure TfrmSettingsStation.OnSwitch_Switch(Sender: TObject);
begin
  TCSClass_SmartFarmAPI.SetStationSprinkle(
    FStationList[cbbStationList.ItemIndex].slug,
    '',
    (Sender as TListItemSwitch).IsChecked
  )
end;

end.

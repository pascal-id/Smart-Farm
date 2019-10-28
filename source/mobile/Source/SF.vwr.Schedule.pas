unit SF.vwr.Schedule;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Objects, FMX.Edit, FMX.ComboEdit, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.ListBox,
  iSolutions.lib.ListItemControls,
  SF.bsx.SmartFarmMain, SF.lib.SmartFarmAPI, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TfrmSchedule = class(TfrmBaseMain)
    lytStation: TLayout;
    btnEditList: TButton;
    imgFloatButton: TImage;
    lvSchedule: TListView;
    cbbStationList: TComboBox;
    fdmtblSchedules: TFDMemTable;
    strngfldSchedulesSTATION_ID: TStringField;
    strngfldSchedulesNODE_ID: TStringField;
    intgrfldSchedulesTYPE: TIntegerField;
    intgrfldSchedulesMODE: TIntegerField;
    strngfldSchedulesDAYS: TStringField;
    strngfldSchedulesVALUE: TStringField;
    lytAddSchedule: TLayout;
    strngfldSchedulesACTIVE: TStringField;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgFloatButtonClick(Sender: TObject);
    procedure AddScheduleList(
      ASubject : string;
      ADescription : string;
      AStatus : Boolean
    );
    procedure btnEditListClick(Sender: TObject);
    procedure cbbStationListChange(Sender: TObject);
    procedure lvScheduleButtonClick(const Sender: TObject; const AItem: TListItem;
        const AObject: TListItemSimpleControl);
    procedure lvSchedulePullRefresh(Sender: TObject);
    procedure lvScheduleUpdateObjects(const Sender: TObject; const AItem:
        TListViewItem);
  private
    FStationList : TCSRec_Stations;
    FCheckList : TStringList;

    procedure Event_FullyLoaded(Sender : TObject);
    procedure OnSwitch_Switch(Sender : TObject);

    procedure FillScheduleList;
  public
    { Public declarations }
  end;

var
  frmSchedule: TfrmSchedule;

implementation

uses System.DateUtils,
     iSolution.lib.StringHelper,
     SF.lib.SmartFarmHelpoer,
     SF.vwr.ScheduleAdd;

{$R *.fmx}

procedure TfrmSchedule.FormDestroy(Sender: TObject);
begin
  inherited;
  FCheckList.Free;
end;

procedure TfrmSchedule.FormCreate(Sender: TObject);
begin
  inherited;
  FCheckList := TStringList.Create;
  FCheckList.Sorted := True;
  FCheckList.Duplicates := TDuplicates.dupIgnore;
  OnFullyLoaded := Event_FullyLoaded;
end;

procedure TfrmSchedule.AddScheduleList(
  ASubject : string;
  ADescription: string;
  AStatus: Boolean);
var
  itmRecord : TListViewItem;
begin
  if ASubject <> '' then
  begin
    itmRecord := lvSchedule.Items.Add;
    itmRecord.Data['Item'] := ASubject;
    itmRecord.Data['Description'] := ADescription;
    itmRecord.Objects.FindObjectT<TListItemSwitch>('Switch').IsChecked := AStatus;
  end;
end;

procedure TfrmSchedule.btnEditListClick(Sender: TObject);
var
  arrChecked : TArray<Integer>;
begin
  if btnEditList.Tag = 0 then
  begin
    lvSchedule.EditMode := True;
    btnEditList.Tag := 1;
    btnEditList.Text := 'Done';
  end
  else
  begin
    lvSchedule.EditMode := False;
    btnEditList.Tag := 0;
    btnEditList.Text := 'Edit';

    //Delete selected item
    if FCheckList.Count > 0 then
    begin
      try
        for var i := FCheckList.Count - 1 downto 0 do
        begin
          lvSchedule.Items.Delete(FCheckList[i].ToInteger);
        end;
      finally
        FCheckList.Clear;
      end;
    end;

    FillScheduleList;
  end;
end;

procedure TfrmSchedule.cbbStationListChange(Sender: TObject);
begin
  FillScheduleList;
end;

procedure TfrmSchedule.Event_FullyLoaded(Sender: TObject);
begin
  tbcDevices.TabIndex := 1;
  FStationList := TCSClass_SmartFarmHelper.FillStationList(cbbStationList);
  FillScheduleList;
end;

procedure TfrmSchedule.FillScheduleList;
var
  sDays : string;
  iDaysCount : Integer;
  sDayValue : string;
  sDayName : string;
  recNodes : TCSRec_Nodes;
  sNodeList : string;

  function GetActiveStatus(AValue : Boolean) : string;
  begin
    if AValue  then
    begin
      Result := 'Turn On';
    end
    else
    begin
      Result := 'Turn Off';
    end;
  end;

begin
  lvSchedule.Items.Clear;

  TCSClass_SmartFarmHelper.LoadScheduleData(
    fdmtblSchedules,
    FStationList,
    cbbStationList.ItemIndex
  );

//  if not fdmtblSchedules.Locate(
//    'STATION_ID;NODE_ID;TYPE;MODE;VALUE;DAYS',
//    VarArrayOf(
//      [FStationList[cbbStationList.ItemIndex].slug,
//       sNodeID,
//       FSchedule._Type,
//       FSchedule.Mode,
//       FSchedule.Value,
//       FSchedule.DayToString
//      ]
//    )
//  ) then
  fdmtblSchedules.IndexFieldNames := 'STATION_ID;NODE_ID;TYPE;MODE;VALUE;DAYS';
    
  fdmtblSchedules.First;
  while not fdmtblSchedules.Eof do
  begin
    recNodes := TCSClass_SmartFarmAPI.GetNodeList(
      fdmtblSchedules.FieldByName('STATION_ID').AsString,
      fdmtblSchedules.FieldByName('NODE_ID').AsString
    );
    sNodeList := recNodes[0].name;
//  AddScheduleList(
//    '23:00',
//    'Node 1, Everyday Turn On',
//    False
//  );
//    fdmtblSchedules.FieldByName('NODE_ID');
    case fdmtblSchedules.FieldByName('TYPE').AsInteger of
      1:  //By Time
      begin
        case fdmtblSchedules.FieldByName('MODE').AsInteger of
          0:  //By Days
          begin
            sDays := fdmtblSchedules.FieldByName('DAYS').AsString;
            iDaysCount := TCSRec_StringHelper.PartCount(
              sDays,
              ','
            );
            sDayName := '';
            for var i := 0 to iDaysCount - 1 do
            begin
              sDayValue := TCSRec_StringHelper.PartByIndex(
                sDays,
                i,
                ','
              );
              if sDayValue <> '' then
              begin
                sDayName := sDayName +
                  FormatSettings.LongDayNames[
                    sDayValue.ToInteger + 1
                  ] + ', ';
              end;
            end;
            AddScheduleList(
              fdmtblSchedules.FieldByName('VALUE').AsString,
              Format(
                'Node %s - Every %s, %s',
                [sNodeList,
                 sDayName,
                 GetActiveStatus(
                  fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
                 )
                ]
              ),
              fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
            );
          end;
          1:  //By One Day
          begin
            AddScheduleList(
              fdmtblSchedules.FieldByName('VALUE').AsString,
              Format(
                'Node %s - Run Once at Specified Date, %s',
                [sNodeList,
                 GetActiveStatus(
                  fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
                 )
                ]
              ),
              fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
            );
          end;
        end
      end;
      0:  //By Trigger
      begin
        case fdmtblSchedules.FieldByName('MODE').AsInteger of
          0:  //By Humidity
          begin
            AddScheduleList(
              fdmtblSchedules.FieldByName('VALUE').AsString,
              Format(
                'Node %s - When Hunidity Reached the Value, %s',
                [sNodeList,
                 GetActiveStatus(
                  fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
                 )
                ]
              ),
              fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
            );
          end;
          1:  //By Temperature
          begin
            AddScheduleList(
              fdmtblSchedules.FieldByName('VALUE').AsString,
              Format(
                'Node %s - When Temperature Reached The Value, %s',
                [sNodeList,
                 GetActiveStatus(
                  fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
                 )
                ]
              ),
              fdmtblSchedules.FieldByName('ACTIVE').AsBoolean
            );
          end;
        end
      end;
    end;

    fdmtblSchedules.Next;
  end;
//
//
//    for var i := 0 to Length(arrSchedules) - 1 do
//    begin
//      if Length(arrSchedules[i].Schedules) > 0 then
//      begin
//        for var j := 0 to Length(arrSchedules[i].Schedules) - 1 do
//        begin
////          case arrSchedules[i].Schedules[j]._Type of
////
////          end;
////          AddScheduleList(
////
////          );
//        end;
//      end;
//    end;
//  end;

end;

procedure TfrmSchedule.imgFloatButtonClick(Sender: TObject);
var
  frmTemp : TCustomForm;
begin
  frmTemp := TCSClass_SmartFarmHelper.IsFormLoaded('frmScheduleAdd');
  if frmTemp = nil then
  begin
    frmTemp := TfrmScheduleAdd.Create(Self);
  end;
  frmTemp.ShowModal(
    procedure(AResult : TModalResult)
    begin
      FillScheduleList;
    end
  );
end;

procedure TfrmSchedule.lvScheduleButtonClick(
  const Sender: TObject;
  const AItem: TListItem;
  const AObject: TListItemSimpleControl
);
begin
  if AObject.Name = 'Delete' then
  begin
    (AObject as TListItemGlyphButton).Checked :=
      not (AObject as TListItemGlyphButton).Checked;
    if (AObject as TListItemGlyphButton).Checked then
    begin
      FCheckList.Add(FormatFloat('000',AItem.Index)); 
    end
    else
    begin
      FCheckList.Delete(
        FCheckList.IndexOf(
          FormatFloat('000',AItem.Index)
        )
      );
    end;
  end;

end;

procedure TfrmSchedule.lvSchedulePullRefresh(Sender: TObject);
begin
  inherited;
  FillScheduleList;
end;

procedure TfrmSchedule.lvScheduleUpdateObjects(
  const Sender: TObject;
  const AItem: TListViewItem);
var
  liSwitch: TListItemSwitch;
  liDelete: TListItemGlyphButton;
  liText : TListItemText;
  liDetail : TListItemText;
begin
  liSwitch:= AItem.Objects.FindObject('Switch') as TListItemSwitch;
  if liSwitch = nil then
  begin
    liSwitch:= TListItemSwitch.Create(AItem);
    liSwitch.Name:= 'Switch';
    liSwitch.Align:= TListItemAlign.Trailing;
    liSwitch.VertAlign:= TListItemAlign.Center;
    liSwitch.SwitchRound := 8;
    liSwitch.OnColor := $FFB7D3FF;
    liSwitch.OffColor := $FFCBCBCB;
    liSwitch.ThumbOnColor := $FF5599FF;
    liSwitch.ThumbOffColor := $FF939393;
    liSwitch.ThumbHeight := liSwitch.Height + 8;
    liSwitch.ThumbWidth := liSwitch.Height + 8;
    liSwitch.ThumbRound := 15;
    liSwitch.OnSwitch := OnSwitch_Switch;
  end;

  liText:= AItem.Objects.FindObject('Item') as TListItemText;
  if liText <> nil then
  begin
    liText.Width := ClientWidth - (liSwitch.Width * 2);
    if lvSchedule.EditMode then
    begin
      liDelete:= AItem.Objects.FindObject('Delete') as TListItemGlyphButton;
      if liDelete <> nil then
      begin
        liText.Width := ClientWidth - (liSwitch.Width * 2) - liDelete.Width;
      end;
    end;
  end;

  liDetail:= AItem.Objects.FindObject('Description') as TListItemText;
  if liDetail <> nil then
  begin
    liDetail.Width := liText.Width;
  end;
end;

procedure TfrmSchedule.OnSwitch_Switch(Sender: TObject);
begin
  if cbbStationList.ItemIndex > 0 then
  begin
    TCSClass_SmartFarmAPI.SetStationSprinkle(
      FStationList[cbbStationList.ItemIndex - 1].slug,
      '',
      (Sender as TListItemSwitch).IsChecked
    );
  end;
end;

end.

unit SF.lib.SmartFarmHelpoer;

interface

uses FMX.ListBox, FMX.Types, FMX.Forms,
     FireDAC.Comp.Client,
     SF.lib.SmartFarmAPI;

type
  TCSClass_SmartFarmHelper = class
    class function FillStationList(
      AComboBox : TComboBox;
      AAllData : Boolean = True
    ) : TCSRec_Stations;
    class procedure LoadScheduleData(
      ADataSet : TFDMemTable;
      AStationList : TCSRec_Stations;
      AStationIndex : Integer
    );
    class function IsFormLoaded(
      AFormName: string
    ): TCustomForm;
  end;

implementation

uses System.SysUtils;

{ TCSClass_SmartFarmHelper }

{-----------------------------------------------------------------------------
  Procedure : FillStationList
  Author    : iwancs
  Date      : 23 Oct 2019
  Arguments : AComboBox : TComboBoxAStationList : TCSRec_Stations;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_SmartFarmHelper.FillStationList(
  AComboBox: TComboBox;
  AAllData : Boolean
) : TCSRec_Stations;
var
  itmListBox : TListBoxItem;
begin
  if AAllData then
  begin
    itmListBox := TListBoxItem.Create(AComboBox);
    itmListBox.Parent := AComboBox;
    AComboBox.AddObject(itmListBox);
    itmListBox.TextSettings.Font.Family := 'Ubuntu';
    itmListBox.TextSettings.Font.Size := 11;
    itmListBox.TextSettings.FontColor := $FF5599FF;
    itmListBox.TextSettings.HorzAlign := TTextAlign.Center;
    itmListBox.Text := 'All Stations';
  end;

  Result := TCSClass_SmartFarmAPI.GetStationList;
  if length(Result) > 0 then
  begin
    for var i := 0 to Length(Result) - 1 do
    begin
      itmListBox := TListBoxItem.Create(AComboBox);
      itmListBox.Parent := AComboBox;
      AComboBox.AddObject(itmListBox);
      itmListBox.TextSettings.Font.Family := 'Ubuntu';
      itmListBox.TextSettings.Font.Size := 11;
      itmListBox.TextSettings.FontColor := $FF5599FF;
      itmListBox.TextSettings.HorzAlign := TTextAlign.Center;
      itmListBox.Text := Result[i].name;
    end;
  end;
  AComboBox.ItemIndex := 0;
end;

class function TCSClass_SmartFarmHelper.IsFormLoaded(
  AFormName: string): TCustomForm;
begin
  Result := nil;
  for var i := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.Forms[i] is TCustomForm) and
       (CompareText(Screen.Forms[i].Name, AFormName) = 0) then
    begin
      Result := TCustomForm(Screen.Forms[i]);
      Break;
    end;
  end;
end;

class procedure TCSClass_SmartFarmHelper.LoadScheduleData(
  ADataSet: TFDMemTable;
  AStationList : TCSRec_Stations;
  AStationIndex : Integer
);
var
  arrSchedules : TCSRec_NodeSchedules;
  iCount : integer;
  sStationID : string;
  sDays : string;
begin
  ADataSet.Close;
  ADataSet.CreateDataSet;

  iCount := 1;
  sStationID := '';
  if AStationIndex = 0 then
  begin
    //All Station
    iCount := Length(AStationList);
  end
  else
  begin
    sStationID := AStationList[AStationIndex - 1].slug;
  end;

  for var h := 0 to iCount - 1 do
  begin
    if (sStationID = '') or
       (h > 0) then
    begin
      sStationID := AStationList[h].slug;
    end;
    arrSchedules := TCSClass_SmartFarmAPI.GetSchedule(sStationID);
    if length(arrSchedules) > 0 then
    begin
      for var i := 0 to Length(arrSchedules) - 1 do
      begin
        for var j := 0 to Length(arrSchedules[i].Schedules) - 1 do
        begin
          ADataSet.Append;
          ADataSet.FieldByName('STATION_ID').AsString := sStationID;
          ADataSet.FieldByName('NODE_ID').AsString := arrSchedules[i].Slug;

          ADataSet.FieldByName('TYPE').AsInteger :=
            arrSchedules[i].Schedules[j]._Type;
          ADataSet.FieldByName('MODE').AsInteger :=
            arrSchedules[i].Schedules[j].Mode;
          ADataSet.FieldByName('VALUE').Value :=
            arrSchedules[i].Schedules[j].Value;
          ADataSet.FieldByName('ACTIVE').AsBoolean :=
            arrSchedules[i].Schedules[j].Active;

          sDays := '';
          for var k := 0 to Length(arrSchedules[i].Schedules[j].Days) - 1 do
          begin
            sDays := sDays + arrSchedules[i].Schedules[j].Days[k].ToString + ',';
          end;
          ADataSet.FieldByName('DAYS').AsString := sDays;
          ADataSet.Post;
        end;
      end;
    end;
  end;
end;

end.

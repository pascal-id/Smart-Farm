unit SmartFarmServer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TValueState = record
    Value: Integer;
    State: Integer;
  end;

  TDaySet = set of 0 .. 6;

  TSchedule = record
    Type_: Integer;
    Mode: Integer;
    IsActive: Boolean;
    Value: String;
    Days: TDaySet;
  end;

  TScheduleList = array of TSchedule;

  TUpdateData = record
    Temperature: TValueState;
    Humidity: TValueState;
    Sprinkle: TValueState;
    Schedules: TScheduleList;
  end;

var
  Token,
  StationID,
  NodeID,
  GetUpdateURL,
  GetScheduleURL,
  PostUpdateURL: String;

function GetUpdateData: TUpdateData;
procedure UpdateEnvCondData(const ATemperature: Double; AHumidity: Integer; const AIsSprinkleOn: Boolean);

implementation

uses
  httpdefs,
  fpJSON,
  utils;

function GetUpdateData: TUpdateData;
var
  LRootNode,LStateNode,LScheduleNode,LDaysNode: TJSONData;
  LSchedule: TSchedule;
  i,j: Integer;
begin
  Result := Default(TUpdateData);

  LRootNode := GetJSONResponse(hmGET, IncludeHTTPPathDelimiter(GetUpdateURL) + '?' + URLEncodeParams([KVP('stationId',StationID),KVP('id',NodeID)]),[KVP('Token',Token)]);
  try
    Result.Sprinkle.State    := NodeValueToIntDef(LRootNode,'data[0].options.devices.sprinkle.state',-1);
    Result.Temperature.State := NodeValueToIntDef(LRootNode,'data[0].options.devices.suhu.state',-1);
    Result.Temperature.Value := NodeValueToIntDef(LRootNode,'data[0].options.devices.suhu.value',-1);
    Result.Humidity.State    := NodeValueToIntDef(LRootNode,'data[0].options.devices.kelembaban.state',-1);
    Result.Humidity.Value    := NodeValueToIntDef(LRootNode,'data[0].options.devices.kelembaban.value',-1);
  finally
    LRootNode.Free;
  end;

  LRootNode := GetJSONResponse(hmGET, IncludeHTTPPathDelimiter(GetScheduleURL) + '?' + URLEncodeParams([KVP('stationId',StationID),KVP('nodeId',NodeID)]),[KVP('Token',Token)]);
  try
    LStateNode := LRootNode.FindPath('data[0].schedules');
    if Assigned(LStateNode) then begin
      for i := 0 to LStateNode.Count - 1 do begin
        LScheduleNode := LStateNode.Items[i];

        LSchedule.Type_    := NodeValueToIntDef(LScheduleNode,'type',-1);
        LSchedule.Mode     := NodeValueToIntDef(LScheduleNode,'mode',-1);
        LSchedule.IsActive := NodeValueToIntDef(LScheduleNode,'active',-1) = 1;
        LSchedule.Value    := NodeValueToStrDef(LScheduleNode,'value','');

        LDaysNode := LScheduleNode.FindPath('days');
        if Assigned(LDaysNode) then
          for j := 0 to LDaysNode.Count - 1 do
            Include(LSchedule.Days,LDaysNode.Items[j].AsInteger);

        SetLength(Result.Schedules,Length(Result.Schedules) + 1);
        Result.Schedules[High(Result.Schedules)] := LSchedule;
      end;
    end;
  finally
    LRootNode.Free;
  end;
end;

procedure UpdateEnvCondData(const ATemperature: Double; AHumidity: Integer; const AIsSprinkleOn: Boolean);
var
  AIsSprinkleOnIntVal: Integer;
  OptionsJSON: String;
begin
  if AIsSprinkleOn then AIsSprinkleOnIntVal := 0 else AIsSprinkleOnIntVal := 1;
  OptionsJSON := Format('{"devices":{"suhu":{"value":%02f,"state":0},"kelembaban":{"value":%d,"state":0},"sprinkle":{"state":%d}}}',[ATemperature,AHumidity,AIsSprinkleOnIntVal]);
  GetJSONResponse(hmPOST,IncludeHTTPPathDelimiter(PostUpdateURL),[KVP('Token',Token)],URLEncodeParams([KVP('id',NodeID),KVP('options',OptionsJSON)]));
end;

end.

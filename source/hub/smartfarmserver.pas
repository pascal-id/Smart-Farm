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
  StationID,
  NodeID,
  GetUpdateURL,
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
  LStateNode,LScheduleNode: TJSONData;
  LSchedule: TSchedule;
  i,j: Integer;
begin
  with GetJSONResponse(hmGET, IncludeHTTPPathDelimiter(GetUpdateURL) + '?' + URLEncodeParams([KVP('stationId',StationID),KVP('id',NodeID)])) do
    try
      LStateNode := FindPath('data.options.sprinkle.state');
      if Assigned(LStateNode) then
        Result.Sprinkle.State := LStateNode.AsInteger;

      LStateNode := FindPath('data.options.suhu.state');
      if Assigned(LStateNode) then
        Result.Temperature.State := LStateNode.AsInteger;

      LStateNode := FindPath('data.options.suhu.value');
      if Assigned(LStateNode) then
        Result.Temperature.Value := LStateNode.AsInteger;

      LStateNode := FindPath('data.options.kelembaban.state');
      if Assigned(LStateNode) then
        Result.Humidity.State := LStateNode.AsInteger;

      LStateNode := FindPath('data.options.kelembaban.value');
      if Assigned(LStateNode) then
        Result.Humidity.Value := LStateNode.AsInteger;

      LStateNode := FindPath('data.schedules');
      if Assigned(LStateNode) then begin
        for i := 0 to LStateNode.Count - 1 do begin
          LScheduleNode := LStateNode.Items[i];

          // safe values for invalid schedule
          LSchedule.Type_ := -1;
          LSchedule.Mode := -1;

          LStateNode := LScheduleNode.FindPath('type');
          if Assigned(LStateNode) then
            LSchedule.Type_ := LStateNode.AsInteger;

          LStateNode := LScheduleNode.FindPath('mode');
          if Assigned(LStateNode) then
            LSchedule.Mode := LStateNode.AsInteger;

          LStateNode := LScheduleNode.FindPath('active');
          if Assigned(LStateNode) then
            LSchedule.IsActive := LStateNode.AsInteger = 1;

          LStateNode := LScheduleNode.FindPath('value');
          if Assigned(LStateNode) then
            LSchedule.Value := LStateNode.AsString;

          LStateNode := LScheduleNode.FindPath('days');
          if Assigned(LStateNode) then
            for j := 0 to LStateNode.Count - 1 do
              Include(LSchedule.Days,LStateNode.Items[j].AsInteger);

          SetLength(Result.Schedules,Length(Result.Schedules) + 1);
          Result.Schedules[High(Result.Schedules)] := LSchedule;
        end;
      end;
    finally
      Free;
    end;
end;

procedure UpdateEnvCondData(const ATemperature: Double; AHumidity: Integer; const AIsSprinkleOn: Boolean);
var
  AIsSprinkleOnIntVal: Integer;
  OptionsJSON: String;
begin
  if AIsSprinkleOn then AIsSprinkleOnIntVal := 0 else AIsSprinkleOnIntVal := 1;
  OptionsJSON := Format('{"suhu":{"value":%02f,"state":0"},"kelembaban":{"value":%d,"state":0},"sprinkle":{"state":%d}}',[ATemperature,AHumidity,AIsSprinkleOnIntVal]);
  GetJSONResponse(hmPOST,IncludeHTTPPathDelimiter(PostUpdateURL),URLEncodeParams([KVP('id',NodeID),KVP('options',OptionsJSON)]));
end;

end.

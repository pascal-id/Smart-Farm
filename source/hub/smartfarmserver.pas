unit SmartFarmServer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  generics.collections;

type
  TScheduleSprinkleRequestMap = specialize TDictionary<String,String>;
  TScheduleSprinkleRequest = specialize TPair<String,String>;

  TUpdateData = record
    TurnSprinkleOn: Boolean;
    ScheduleSprinkleRequestMap: TScheduleSprinkleRequestMap;
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
  fphttpclient,
  fpJSON,
  utils;

var
  UpdateDataResp: TUpdateData;

function GetUpdateData: TUpdateData;
var
  StateNode: TJSONData;
begin
  try
    with GetJSONResponse(hmGET, IncludeHTTPPathDelimiter(GetUpdateURL) + Format('?stationId=%s&id=%s',[StationID,NodeID])) do
      try
        StateNode := FindPath('data.options.sprinkle_state');
        UpdateDataResp.TurnSprinkleOn := Assigned(StateNode) and (StateNode.AsInteger = 0);
        Result := UpdateDataResp;
      finally
        Free;
      end
  except
    on e: Exception do begin
      WriteLn(e.Message);
    end
  end;
end;

procedure UpdateEnvCondData(const ATemperature: Double; AHumidity: Integer; const AIsSprinkleOn: Boolean);
var
  OptionsJSON: String;
begin
  try
    OptionsJSON := Format('{"suhu":%02f,"kelembaban":%d}',[ATemperature,AHumidity]);
    GetJSONResponse(hmPOST,IncludeHTTPPathDelimiter(PostUpdateURL),Format('id=%s&options=%s',[EncodeURLElement(NodeID),EncodeURLElement(OptionsJSON)]));
  except
    on e: Exception do begin
      WriteLn(e.Message);
    end;
  end;
end;

initialization
  UpdateDataResp.ScheduleSprinkleRequestMap := TScheduleSprinkleRequestMap.Create;

finalization
  UpdateDataResp.ScheduleSprinkleRequestMap.Free;

end.

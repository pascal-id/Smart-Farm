unit node_controller;
{
  USAGE:

  [x] Get Device Info
  curl "smartfarm.pascal-id.test/node/?stationId=tgr123&id=qsw345sxP"

  [x] Set Device Info
  curl -X POST "smartfarm.pascal-id.test/node/" -d 'id=qsw345sxP&value=3'

  curl -X POST "smartfarm.pascal-id.test/node/" \
    -d 'id=qsw345sxP&state=1&value=3&options={"devices":{"suhu":{"value":28.75,"state":0},"kelembaban":{"value":888,"state":0},"sprinkle":{"state":1}}}'

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers, json_helpers,
  array_helpers;

{$include ../common/smartfarm.inc}

type
  TNodeModule = class(TMyCustomWebModule)
  private
    FStationID, FID: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common, node_model, node_history_model, auth_model;

constructor TNodeModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TNodeModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TNodeModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_AUTH_UNAUTHORIZED);
end;

// GET Method Handler
procedure TNodeModule.Get;
var
  i, indexOptions, indexSchedules: integer;
  deviceOptions, schedulesAsString: string;
  whereAsArray: TStringArray;
  deviceAsJsonArray: TJSONArray;
  json: TJSONUtil;
begin
  FStationID := _GET['stationId'];
  FID := _GET['id'];
  if FStationID.IsEmpty then
  begin
    OutputJson(404, ERR_INVALID_PARAMETER);
  end;

  // Prepare data selection
  whereAsArray.Add('stations.status_id=0');
  whereAsArray.Add('nodes.status_id=0');
  whereAsArray.Add('stations.slug="' + FStationID + '"');
  if not FID.IsEmpty then
  begin
    whereAsArray.Add('nodes.slug="' + FID + '"');
  end;

  DataBaseInit;
  json := TJSONUtil.Create;
  with TNodeModel.Create do
  begin
    AddJoin('stations', 'sid', 'station_id', []);
    if not Find(whereAsArray) then
    begin
      OutputJson(404, ERR_DATA_NOT_FOUND);
    end;

    deviceAsJsonArray := TJSONArray.Create;
    if not DataToJSON(Data, deviceAsJsonArray, False) then
    begin
      OutputJson(404, ERR_INVALID_QUERY);
    end;

    //convert options string value to json string formatted
    for i := 0 to deviceAsJsonArray.Count - 1 do
    begin

      // remove unused field
      deviceAsJsonArray.Items[i].Delete('nid');
      deviceAsJsonArray.Items[i].Delete('station_id');
      deviceAsJsonArray.Items[i].Delete('status_id');

      indexOptions := deviceAsJsonArray.Items[i].IndexOfName('options');
      if indexOptions <> -1 then
      begin
        try
          deviceOptions := deviceAsJsonArray.Items[i].Items[indexOptions].AsString;
          if deviceOptions.Trim.IsEmpty then
            deviceOptions := '{}';
          if IsJsonValid(deviceOptions) then
            deviceAsJsonArray.Items[i].Items[indexOptions] := GetJSON(deviceOptions);
        except
        end;
      end;

      indexSchedules := deviceAsJsonArray.Items[i].IndexOfName('schedules');
      if indexSchedules <> -1 then
      begin
        schedulesAsString :=
          deviceAsJsonArray.Items[i].Items[indexSchedules].AsString;
        if schedulesAsString.Trim.IsEmpty then
          schedulesAsString := '{}';
        if IsJsonValid(schedulesAsString) then
          deviceAsJsonArray.Items[i].Items[indexSchedules] :=
            GetJSON(schedulesAsString);
      end;

    end;

    json['code'] := 0;
    json['count'] := deviceAsJsonArray.Count;
    json.ValueArray['data'] := deviceAsJsonArray;
    Free;
  end;

  //TODO: for test only, remove it
  if not authToken.IsEmpty then
  begin
    json['auth/token'] := authToken;
    json['auth/slug'] := authSlug;
  end;

  Response.Content := json.AsJSON;
  json.Free;
end;

// POST Method Handler
procedure TNodeModule.Post;
var
  stationId: integer;
  sDid, sMessage: string;
  device: TNodeModel;
  history: TNodeHistoryModel;
  deviceState, deviceValue, deviceOptions, activity, description: string;
  deviceOptionsAsJson: TJSONObject;
  temperatureAverageExisting, humidityAverageExisting,
    temperatureUpdate, humadityUpdate: Double;
begin
  activity := '';
  description := '';
  FID := _POST['id'];
  if FID.IsEmpty then
  begin
    OutputJson(404, ERR_INVALID_PARAMETER);
  end;

  //TODO: check is related to user or not

  deviceState := _POST['state'];
  deviceValue := _POST['value'];
  deviceOptions := _POST['options'];
  //deviceOptions := UrlDecode(deviceOptions);
  deviceOptions := deviceOptions.Replace('\n', #10);
  deviceOptions := deviceOptions.Replace('\"', '"');
  if not IsJsonValid(deviceOptions) then
    OutputJson(400, ERR_INVALID_PARAMETER_VALUE);

  if (deviceState.IsEmpty and deviceValue.IsEmpty and deviceOptions.IsEmpty) then
    OutputJson(404, ERR_INVALID_PARAMETER);

  DataBaseInit;
  device := TNodeModel.Create;
  if not device.Find(['slug="' + FID + '"']) then
  begin
    OutputJson(404, ERR_NODE_NOT_FOUND);
  end;

  sDid := device['nid'];
  stationId := device['station_id'];

  sMessage := INFO_DEVICE_CHECKID;
  if not deviceState.IsEmpty then
    device['state'] := deviceState;
  if not deviceValue.IsEmpty then
    device['value'] := deviceValue;
  if not deviceOptions.IsEmpty then
  begin
    //rebuild options json
    if IsJsonValid(deviceOptions) then
    begin
      deviceOptionsAsJson := TJSONObject( GetJSON(deviceOptions));
      temperatureUpdate := s2f(jsonGetData(deviceOptionsAsJson, 'devices/' + TEMPERATURE_KEY + '/value'));
      humadityUpdate := s2f(jsonGetData(deviceOptionsAsJson, 'devices/' + HUMIDITY_KEY + '/value'));

      // set temperature average
      if temperatureUpdate > 0 then
      begin
        temperatureAverageExisting := device.Data.FieldByName('temperature_average').AsFloat;
        if temperatureAverageExisting > 0 then
        begin
          temperatureUpdate := (temperatureUpdate + temperatureAverageExisting) / 2;
          description := 'temperature,';
        end;
        device['temperature_average'] := temperatureUpdate;
      end;

      // set humidity average
      if humadityUpdate > 0 then
      begin
        humidityAverageExisting := device.Data.FieldByName('humidity_average').AsFloat;
        if humidityAverageExisting > 0 then
        begin
          humadityUpdate := (humadityUpdate + humidityAverageExisting) / 2;
          description:= description + 'humadity,';
        end;
        device['humidity_average'] := humadityUpdate;
      end;

      {
      if IsJsonValid(existingDeviceOptions) then
      begin
        deviceOptionsAsJson := TJSONObject( GetJSON(deviceOptions));
        existingDeviceOptionsAsJson := TJSONObject( GetJSON(existingDeviceOptions));
        for i:=0 to deviceOptionsAsJson.Count-1 do
        begin
          sKey := deviceOptionsAsJson.Names[i];
          existingDeviceOptionsAsJson.Elements[sKey] := deviceOptionsAsJson.Items[i];
        end;
        deviceOptions := existingDeviceOptionsAsJson.AsJSON;
        existingDeviceOptionsAsJson.Free;
        //deviceOptionsAsJson.Free;
      end;
      }
      activity := 'bulk';
      device['options'] := deviceOptions;
    end;
  end;
  if device.Save('slug="' + FID + '"') then
  begin
    history := TNodeHistoryModel.Create;
    history['date'] := Now.Format();
    history['slug'] := FID;
    history['node_id'] := sDID;
    history['station_id'] := stationId;
    history['activity'] := activity;
    history['description'] := description;
    history['status_id'] := 0;
    if not deviceState.IsEmpty then
      history['state'] := deviceState;
    if not deviceValue.IsEmpty then
      history['value'] := deviceValue;
    if not deviceOptions.IsEmpty then
      history['options'] := deviceOptions;
    history.Save;
    history.Free;
    sMessage := OK;
  end;
  device.Free;

  OutputJson(200, sMessage);
end;

end.


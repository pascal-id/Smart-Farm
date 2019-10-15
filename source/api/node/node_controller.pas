unit node_controller;
{
  USAGE:

  [x] Get Device Info
  curl "smartfarm.pascal-id.test/node/?stationId=tgr123&id=qsw345sxP"

  [x] Set Device Info
  curl -X POST "smartfarm.pascal-id.test/node/" \
    -d 'id=qsw345sxP&state=1&value=3&options={"suhu":34,"kelembaban":45,"time":{"08:30": 5000,"16:00": 10000}}'

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
    OutputJson(401, ERR_NOT_PERMITTED);
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

  Response.Content := json.AsJSON;
  json.Free;
end;

// POST Method Handler
// CURL example:
//   curl -X POST -H "Authorization: Basic dW5hbWU6cGFzc3dvcmQ=" "yourtargeturl"
procedure TNodeModule.Post;
var
  i: integer;
  sKey, sDid, sMessage: string;
  device: TNodeModel;
  history: TNodeHistoryModel;
  deviceState, deviceValue, deviceOptions, existingDeviceOptions: string;
  deviceOptionsAsJson, existingDeviceOptionsAsJson: TJSONObject;
begin
  FID := _POST['id'];
  if FID.IsEmpty then
  begin
    OutputJson(404, ERR_INVALID_PARAMETER);
  end;

  //TODO: check is related to user or not

  deviceState := _POST['state'];
  deviceValue := _POST['value'];
  deviceOptions := _POST['options'];
  deviceOptions := deviceOptions.Replace('\"', '"');

  if (deviceState.IsEmpty and deviceValue.IsEmpty and deviceOptions.IsEmpty) then
    OutputJson(404, ERR_INVALID_PARAMETER);

  DataBaseInit;
  device := TNodeModel.Create;
  if not device.Find(['slug="' + FID + '"']) then
  begin
    OutputJson(404, ERR_NODE_NOT_FOUND);
  end;

  sDid := device['did'];

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
      existingDeviceOptions := device['options'];
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
      device['options'] := deviceOptions;
    end;
  end;
  if device.Save('slug="' + FID + '"') then
  begin
    history := TNodeHistoryModel.Create;
    history['date'] := Now.Format();
    history['slug'] := FID;
    history['devices_id'] := sDID;
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


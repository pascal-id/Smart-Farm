unit node_controller;
{
  USAGE:

  [x] Get Device List
  curl "smartfarm.pascal-id.test/node/?stationId=tgr123"

  [x] Get Device Info
  curl "smartfarm.pascal-id.test/node/?stationId=tgr123&id=qsw345sxP"

  [x] Set Device Info
  curl -X POST "smartfarm.pascal-id.test/node/" -d 'id=qsw345sxP&value=3'

  curl -X POST "smartfarm.pascal-id.test/node/" \
    -d 'id=qsw345sxP&state=1&value=3&options={"devices":{"suhu":{"value":28.75,"state":0},"kelembaban":{"value":77,"state":0},"sprinkle":{"state":1}}}'

  [x] Set Specific Device condition
  curl -X POST "smartfarm.pascal-id.test/node/" -d 'id=qsw345sxP&device=suhu&value=33'
  curl -X POST "smartfarm.pascal-id.test/node/" -d 'id=qsw345sxP&device=springkler&state=1'

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers, json_helpers,
  array_helpers;

{$include ../common/smartfarm.inc}

type

  { TNodeModule }

  TNodeModule = class(TMyCustomWebModule)
  private
    FStationID, FID: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    procedure Options; override;
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
  deviceAsJsonArray: TJSONArray;
  json: TJSONUtil;
begin
  FStationID := _GET['stationId'];
  FID := _GET['id'];
  if FStationID.IsEmpty then
  begin
    OutputJson(404, ERR_INVALID_PARAMETER);
  end;

  DataBaseInit;
  json := TJSONUtil.Create;
  with TNodeModel.Create do
  begin
    if not IsNodeExists(FStationID, FID) then
      OutputJson(404, ERR_DATA_NOT_FOUND);

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
    json['auth/token'] := authToken; //todo: remove
    json['auth/slug'] := authSlug;
    json['auth/time_elapsed'] := authElapsed;
  end;

  Response.Content := json.AsJSON;
  json.Free;
end;

// POST Method Handler
procedure TNodeModule.Post;
var
  i, indexDevice: Integer;
  stationId: integer;
  sDid, sMessage, sKey: string;
  device: TNodeModel;
  history: TNodeHistoryModel;
  deviceName,
  deviceState, deviceValue, deviceOptions, activity, description: string;
  nodeOptionsExisting: string;
  nodeOptionsExistingAsJson: TJSONUtil;
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
  deviceName := _POST['device'];
  //deviceOptions := UrlDecode(deviceOptions);
  deviceOptions := deviceOptions.Replace('\n', #10);
  deviceOptions := deviceOptions.Replace('\"', '"');
  //if not IsJsonValid(deviceOptions) then
  //  OutputJson(400, ERR_INVALID_PARAMETER_VALUE);

  if deviceState.IsEqualTo('off') then deviceState := '1';
  if deviceState.IsEqualTo('on') then deviceState := '0';

  if (deviceState.IsEmpty and deviceValue.IsEmpty and deviceOptions.IsEmpty) then
    OutputJson(404, ERR_INVALID_PARAMETER);

  DataBaseInit;
  device := TNodeModel.Create;
  if not device.Find(['slug="' + FID + '"']) then
  begin
    OutputJson(404, ERR_NODE_NOT_FOUND);
  end;
  nodeOptionsExisting := device['options'];

  sDid := device['nid'];
  stationId := device['station_id'];

  sMessage := INFO_DEVICE_CHECKID;
  if deviceName.IsEmpty and (not deviceState.IsEmpty) then
    device['state'] := deviceState;
  if deviceName.IsEmpty and (not deviceValue.IsEmpty) then
    device['value'] := deviceValue;
  if not deviceOptions.IsEmpty then
  begin
    //rebuild options json
    if IsJsonValid(deviceOptions) then
    begin
      deviceOptionsAsJson := TJSONObject( GetJSON(deviceOptions));
      temperatureUpdate := s2f(jsonGetData(deviceOptionsAsJson, 'devices/' + TEMPERATURE_KEY + '/value'));
      humadityUpdate := s2f(jsonGetData(deviceOptionsAsJson, 'devices/' + HUMIDITY_KEY + '/value'));

      indexDevice := deviceOptionsAsJson.IndexOfName('devices');
      if indexDevice <> -1 then
      begin
        for i := 0 to deviceOptionsAsJson.Items[indexDevice].Count-1 do
        begin
          TJSONObject(deviceOptionsAsJson.Items[indexDevice].Items[i]).Add('date',Now.AsString);
        end;
      end;

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

      // rebuild options json
      if IsJsonValid(nodeOptionsExisting) then
      begin
        //nodeOptionsExistingAsJson := GetJSON(nodeOptionsExisting);

        //TODO: rebuild options json


      end;

      activity := 'bulk';
      device['options'] := deviceOptionsAsJson.AsJSON;
    end;
  end else
  begin
    // for specific device
    if not deviceName.IsEmpty then
    begin
      activity := 'manual';
      description := '';
      nodeOptionsExistingAsJson := TJSONUtil.Create;
      nodeOptionsExistingAsJson.LoadFromJsonString(nodeOptionsExisting);
      sMessage := _POST['state'];
      if sMessage.IsEqualTo('off') then sMessage := '1';
      if sMessage.IsEqualTo('on') then sMessage := '0';
      if not sMessage.IsEmpty then
      begin
        nodeOptionsExistingAsJson['devices/'+deviceName+'/state'] := sMessage.AsInteger;
        nodeOptionsExistingAsJson['devices/'+deviceName+'/date'] := Now.AsString;
        description := description + deviceName + ' state set to ' + sMessage + ',';
        deviceState := '';
      end;
      sMessage := _POST['value'];
      if not sMessage.IsEmpty then
      begin
        nodeOptionsExistingAsJson['devices/'+deviceName+'/value'] := sMessage.AsInteger;
        nodeOptionsExistingAsJson['devices/'+deviceName+'/date'] := Now.AsString;
        description := description + deviceName + ' value set to ' + sMessage + ',';
        deviceValue := '';
      end;
      device['options'] := nodeOptionsExistingAsJson.AsJSON;
      deviceOptions := nodeOptionsExisting;
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

procedure TNodeModule.Options;
begin
  Response.Code := 204;
  Response.Content := '';
end;

end.


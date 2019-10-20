unit node_history_controller;
{
  USAGE:

  [x] Get Device Info
  curl "smartfarm.pascal-id.test/node/history/?stationId=tgr123&id=qsw345sxP"

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers, json_helpers,
  array_helpers;

{$include ../common/smartfarm.inc}

type

  { TNodeHistoryModule }

  TNodeHistoryModule = class(TMyCustomWebModule)
  private
    FID: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    procedure Options; override;
  end;

implementation

uses common, node_history_model, auth_model;

procedure TNodeHistoryModule.BeforeRequestHandler(Sender: TObject;
  ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_NOT_PERMITTED);
end;

constructor TNodeHistoryModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TNodeHistoryModule.Destroy;
begin
  inherited Destroy;
end;

// GET Method Handler
procedure TNodeHistoryModule.Get;
var
  i, indexOptions, limitQuery: integer;
  s, nodeOptions, field: string;
  deviceOptionAsJson: TJSONData;
  nodeHistory: TNodeHistoryModel;
  whereAsArray: TStringArray;
  historyAsArray: TJSONArray;
  json: TJSONUtil;
  temperatureAverage, humidityAverage: Double;
  temperatureCount, humidityCount: Integer;
begin
  temperatureAverage := 0;
  humidityAverage := 0;
  temperatureCount := 0;
  humidityCount := 0;
  FID := _GET['id'];
  field := _GET['field'];
  limitQuery := s2i(_GET['limit']);

  //TODO: check is related to user or not

  DataBaseInit;
  nodeHistory := TNodeHistoryModel.Create;
  if not FID.IsEmpty then
    whereAsArray.Add('node_history.slug="' + FID + '"');
  if not field.IsEmpty then
    whereAsArray.Add(field + ' IS NOT NULL');
  if limitQuery = 0 then
    whereAsArray.Add('date >= CURDATE()');
  nodeHistory.AddJoin('nodes', 'nid', 'node_id', ['name']);
  if not nodeHistory.Find(whereAsArray, 'date desc', limitQuery) then
  begin
    nodeHistory.Free;
    OutputJson(404, ERR_HISTORY_EMPTY);
  end;

  historyAsArray := TJSONArray.Create;
  if not DataToJSON(nodeHistory.Data, historyAsArray, False) then
  begin
    OutputJson(404, ERR_INVALID_QUERY);
  end;

  // remove unused field
  for i := 0 to historyAsArray.Count - 1 do
  begin
    historyAsArray.Items[i].Delete('nhid');
    historyAsArray.Items[i].Delete('status_id');
    historyAsArray.Items[i].Delete('node_id');

    //convert options string value to json string formatted
    indexOptions := historyAsArray.Items[i].IndexOfName('options');
    if indexOptions <> -1 then
    begin
      try
        nodeOptions := historyAsArray.Items[i].Items[indexOptions].AsString;
        if nodeOptions.Trim.IsEmpty then
          nodeOptions := '{}';
        if IsJsonValid(nodeOptions) then
        begin
          deviceOptionAsJson := GetJSON(nodeOptions);

          // get average temperature
          s := jsonGetData(deviceOptionAsJson, 'devices/' + TEMPERATURE_KEY + '/value');
          if not s.IsEmpty then
          begin
            temperatureAverage:= temperatureAverage + s2f(s);
            temperatureCount:= temperatureCount+1;
          end;

          // get humidity temperature
          s := jsonGetData(deviceOptionAsJson, 'devices/' + HUMIDITY_KEY + '/value');
          if not s.IsEmpty then
          begin
            humidityAverage:= humidityAverage + s2f(s);
            humidityCount:= humidityCount+1;
          end;

          historyAsArray.Items[i].Items[indexOptions] := GetJSON(nodeOptions);
        end;
      except
      end;
    end;

  end;

  if temperatureAverage > 0 then
    temperatureAverage := temperatureAverage / temperatureCount;
  if humidityAverage > 0 then
    humidityAverage := humidityAverage / humidityCount;

  json := TJSONUtil.Create;
  json['code'] := 0;
  json['count'] := historyAsArray.Count;
  json['info/temperature_average'] := temperatureAverage;
  json['info/humidity_average'] := humidityAverage;
  json.ValueArray['data'] := historyAsArray;
  Response.Content := json.AsJSON;
  nodeHistory.Free;
  json.Free;
end;

// POST Method Handler
procedure TNodeHistoryModule.Post;
begin
  Response.Content := '';
end;

// OPTIONS Method Handler
procedure TNodeHistoryModule.Options;
begin
  Response.Code := 204;
  Response.Content := '';
end;


end.




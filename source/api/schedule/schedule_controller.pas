unit schedule_controller;

{
  USAGE:

  [x] Get Schedule
  curl "smartfarm.pascal-id.test/schedule/?stationId=tgr123&nodeId=qsw345sxP"

  [x] Set Schedule
  curl -X POST "smartfarm.pascal-id.test/schedule/" \
    -d 'stationId=tgr123&nodeId=qsw345sxP&schedule="{yourScheduleAsJsonString}"'

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, json_helpers, datetime_helpers, array_helpers;

{$include ../common/smartfarm.inc}

type
  TScheduleModule = class(TMyCustomWebModule)
  private
    FStationID: string;
    FNodeID: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common, auth_model, station_model, node_model;

constructor TScheduleModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TScheduleModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TScheduleModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_NOT_PERMITTED);
end;

// GET Method Handler
procedure TScheduleModule.Get;
var
  i, indexSchedules: integer;
  whereAsArray: TStringArray;
  json: TJSONUtil;
  schedulesAsJsonArray: TJSONArray;
  schedulesAsString: string;
begin
  FStationID := _GET['stationId'];
  FNodeID := _GET['nodeId'];
  if FStationID.IsEmpty or FNodeID.IsEmpty then
    OutputJson(401, ERR_INVALID_PARAMETER);

  // Prepare data selection
  whereAsArray.Add('stations.status_id=0');
  whereAsArray.Add('nodes.status_id=0');
  whereAsArray.Add('stations.slug="' + FStationID + '"');

  if not FNodeID.IsEmpty then
  begin
    whereAsArray.Add('nodes.slug="' + FNodeID + '"');
  end;

  DataBaseInit();
  json := TJSONUtil.Create;
  with TNodeModel.Create() do
  begin
    AddJoin('stations', 'sid', 'station_id', []);
    if not Find(whereAsArray, '', 0,
      'nid, nodes.slug, nodes.name, nodes.schedules') then
    begin
      OutputJson(404, ERR_DATA_NOT_FOUND);
    end;

    schedulesAsJsonArray := TJSONArray.Create;
    if not DataToJSON(Data, schedulesAsJsonArray, False) then
    begin
      OutputJson(404, ERR_INVALID_QUERY);
    end;

    //convert schedule string value to json string formatted
    for i := 0 to schedulesAsJsonArray.Count - 1 do
    begin
      // remove unused field
      schedulesAsJsonArray.Items[i].Delete('nid');

      indexSchedules := schedulesAsJsonArray.Items[i].IndexOfName('schedules');
      if indexSchedules <> -1 then
      begin
        schedulesAsString :=
          schedulesAsJsonArray.Items[i].Items[indexSchedules].AsString;
        if schedulesAsString.Trim.IsEmpty then
          schedulesAsString := '{}';
        if IsJsonValid(schedulesAsString) then
          schedulesAsJsonArray.Items[i].Items[indexSchedules] :=
            GetJSON(schedulesAsString);
      end;
    end;

    json['code'] := 0;
    json['count'] := schedulesAsJsonArray.Count;
    json.ValueArray['data'] := schedulesAsJsonArray;
    Free;
  end;

  Response.Content := json.AsJSON;
  json.Free;
end;

// POST Method Handler
procedure TScheduleModule.Post;
var
  scheduleAsString: String;
  node: TNodeModel;
begin
  FStationID := _POST['stationId'];
  if FStationID.IsEmpty then
    FStationID := _GET['stationId'];
  FNodeID := _POST['nodeId'];
  if FNodeID.IsEmpty then
    FNodeID := _GET['nodeId'];
  scheduleAsString := _POST['schedule'];
  scheduleAsString := scheduleAsString.Replace('\"', '"');

  if FStationID.IsEmpty or FNodeID.IsEmpty or scheduleAsString.IsEmpty then
    OutputJson(400, ERR_INVALID_PARAMETER);

  if not IsJsonValid(scheduleAsString) then
    OutputJson(400, ERR_INVALID_PARAMETER);

  //TODO: check is related to station or not

  DataBaseInit();
  node := TNodeModel.Create();
  if not node.Find(['slug="' + FNodeID + '"']) then
  begin
    OutputJson(404, ERR_NODE_NOT_FOUND);
  end;

  node['schedules'] := scheduleAsString;
  if node.Save('slug="' + FNodeID + '"') then
  begin
    node.Free;
    OutputJson(200, OK);
  end else begin
    node.Free;
    OutputJson(400, ERR_STATION_SCHEDULE_FAILED);
  end;
end;

end.



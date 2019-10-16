unit issue_controller;
{
  USAGE:

  [x] Get Issue
  curl "smartfarm.pascal-id.test/issue/?stationId=tgr123&nodeId=qsw345sxP"

  [x] Set Schedule
  curl -X POST "smartfarm.pascal-id.test/issue/" \
  -d 'stationId=tgr123&nodeId=qsw345sxP&type=1&level=2&activity=[activity]&description=[description]'

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers, array_helpers;

{$include ../common/smartfarm.inc}

type
  TIssuesModule = class(TMyCustomWebModule)
  private
    FStationID, FNodeID: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common, auth_model, issue_model, station_model, node_model;

constructor TIssuesModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TIssuesModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TIssuesModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_NOT_PERMITTED);
end;

// GET Method Handler
procedure TIssuesModule.Get;
var
  json: TJSONUtil;
  queryToSelectIssues: TStringList;
  issue: TIssuesModel;
  issueArray: TJSONArray;
  nodeWhere: string;
begin
  FStationID := _GET['stationId'];
  FNodeID := _GET['nodeId'];
  if FStationID.IsEmpty then
    OutputJson(401, ERR_INVALID_PARAMETER);

  DataBaseInit();
  queryToSelectIssues := TStringList.Create;
  json := TJSONUtil.Create;

  if not FNodeID.IsEmpty then
    nodeWhere := ' AND n.slug="' + FNodeID + '"';
  queryToSelectIssues.Text :=
    'select issues.date, s.name station_name, s.slug station_slug, ' +
    #10'n.name node_name, n.slug node_slug, issues.activity, ' +
    #10'issues.type_id type, issues.level_id level, issues.description ' +
    #10'FROM issues' + #10'JOIN stations s ON issues.station_id=s.sid AND s.status_id=0 AND s.slug="' + FStationID + '"' +
    #10'JOIN nodes n ON issues.node_id=n.nid AND n.status_id=0' + nodeWhere +
    #10'WHERE issues.status_id=0 ORDER BY issues.date DESC' +
    #10'LIMIT 20';

  issueArray := TJSONArray.Create;
  if not QueryOpenToJson(queryToSelectIssues.Text, issueArray, false) then
  begin
    OutputJson(404, ERR_INVALID_QUERY);
  end;

  json['code'] := 0;
  json['count'] := issueArray.Count;
  //json['fieldName'] := fieldsName;
  json.ValueArray['data'] := issueArray;
  Response.Content := json.AsJSON;

  //---
  queryToSelectIssues.Free;
  json.Free;
end;

// POST Method Handler
procedure TIssuesModule.Post;
var
  stationId, nodeId, responseCode: Integer;
  typeId, levelId: Integer;
  msg, activity, descriptionIssue: String;
  issue: TIssuesModel;
  station: TStationModel;
  node: TNodeModel;
begin
  FStationID := _POST['stationId'];
  FNodeID := _POST['nodeId'];
  activity := _POST['activity'];
  typeId := s2i(_POST['type']);
  levelId := s2i(_POST['level']);
  descriptionIssue := _POST['description'];

  if FStationID.IsEmpty or FNodeID.IsEmpty or activity.IsEmpty or descriptionIssue.IsEmpty then
    OutputJson(400, ERR_INVALID_PARAMETER);

  DataBaseInit();

  station := TStationModel.Create();
  if not station.Find(['slug="' + FStationID + '"']) then
  begin
    station.Free;
    OutputJson(404, ERR_STATION_NOT_FOUND);
  end;
  stationId := station['sid'];

  node := TNodeModel.Create();
  if not node.Find(['slug="' + FNodeID + '"']) then
  begin
    station.Free;
    node.Free;
    OutputJson(404, ERR_NODE_NOT_FOUND);
  end;
  nodeId := node['nid'];

  station.Free;
  node.Free;

  issue := TIssuesModel.Create();
  issue['date'] := Now;
  issue['station_id'] := stationId;
  issue['node_id'] := nodeId;

  issue['activity'] := activity;
  issue['type_id'] := typeId;
  issue['level_id'] := levelId;
  issue['description'] := descriptionIssue;
  issue['status_id'] := 0;
  msg := OK;
  responseCode := 200;
  if not issue.Save() then
  begin
    msg := FAILED;
    responseCode := 400;
  end;
  issue.Free;
  OutputJson(responseCode, msg);
end;

end.


unit issue_controller;
{
  USAGE:

  [x] Get Issue
  curl "smartfarm.pascal-id.test/issue/?stationId=tgr123&nodeId=qsw345sxP"

  [x] Set Schedule
  curl -X POST "smartfarm.pascal-id.test/issue/" \
    -d 'stationId=tgr123&nodeId=qsw345sxP&....'

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

uses common, auth_model;

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
begin
  //---
  Response.Content := '{}';
end;



end.


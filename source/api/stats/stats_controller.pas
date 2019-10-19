unit stats_controller;

{
  USAGE:

  [x] Get Stats
  curl "smartfarm.pascal-id.test/stats/"

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers;

{$include ../common/smartfarm.inc}

type
  TStatsModule = class(TMyCustomWebModule)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common, auth_model, station_model, node_model, issue_model;

constructor TStatsModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TStatsModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TStatsModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_NOT_PERMITTED);
end;

// GET Method Handler
procedure TStatsModule.Get;
var
  json: TJSONUtil;
begin
  json := TJSONUtil.Create;
  json['code'] := 0;

  DataBaseInit;
  with TStationModel.Create do
  begin
    if Find(['status_id=0']) then
    begin
      json['station/count'] := RecordCount;
    end;
    Free;
  end;

  with TNodeModel.Create do
  begin
    if Find(['status_id=0']) then
    begin
      json['node/count'] := RecordCount;
    end;
    Free;
  end;

  with TIssuesModel.Create do
  begin
    if Find(['status_id=0']) then
    begin
      json['issue/count'] := RecordCount;
    end;
    Free;
  end;

  Response.Content := json.AsJSON;
  json.Free;
end;

// POST Method Handler
procedure TStatsModule.Post;
begin
end;



end.


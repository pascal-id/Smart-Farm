unit station_controller;

{
  USAGE:

  [x] Get Station Info
  curl "smartfarm.pascal-id.test/station/?id=tgr123"

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers;

{$include ../common/smartfarm.inc}

type
  TStationModule = class(TMyCustomWebModule)
  private
    FID: string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common, auth_model;

constructor TStationModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TStationModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TStationModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_NOT_PERMITTED);
end;

// GET Method Handler
procedure TStationModule.Get;
var
  queryToSelectUser, fieldsName: string;
  userArray: TJSONArray;
  json: TJSONUtil;
  bWithoutFieldName: boolean;
begin

  FID := _GET['id'];
  bWithoutFieldName := FID.IsEmpty;
  DataBaseInit();

  // Example with Direct Query
  fieldsName := 'slug, name';
  queryToSelectUser := 'SELECT ' + fieldsName + ' FROM stations WHERE status_id=0';
  if not FID.IsEmpty then
    queryToSelectUser := queryToSelectUser + #10'AND slug="' + FID + '"';

  userArray := TJSONArray.Create;
  json := TJSONUtil.Create;
  if not QueryOpenToJson(queryToSelectUser, userArray, bWithoutFieldName) then
  begin
    OutputJson(404, ERR_INVALID_QUERY);
  end;

  json['code'] := 0;
  json['count'] := userArray.Count;
  json['fieldName'] := fieldsName;
  json.ValueArray['data'] := userArray;
  Response.Content := json.AsJSONFormated;

  json.Free;
end;

// POST Method Handler
procedure TStationModule.Post;
begin
  OutputJson(404, ERR_UNSUPPORTED_METHOD);
end;



end.


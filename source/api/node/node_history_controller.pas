unit node_history_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler,
  database_lib, string_helpers, dateutils, datetime_helpers, json_helpers;

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
  nodeOptions: string;
  nodeHistory: TNodeHistoryModel;
  historyAsArray: TJSONArray;
  json: TJSONUtil;
begin
  FID := _GET['id'];
  limitQuery := s2i(_GET['limit']);
  if limitQuery = 0 then
    limitQuery := 10;
  if FID.IsEmpty then
  begin
    OutputJson(404, ERR_INVALID_PARAMETER);
  end;

  //TODO: check is related to user or not

  DataBaseInit;
  nodeHistory := TNodeHistoryModel.Create;
  if not nodeHistory.Find(['slug="' + FID + '"'], 'date desc', limitQuery) then
  begin
    nodeHistory.Free;
    OutputJson(404, ERR_NODE_NOT_FOUND);
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
    historyAsArray.Items[i].Delete('slug');
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
          historyAsArray.Items[i].Items[indexOptions] := GetJSON(nodeOptions);
      except
      end;
    end;

  end;

  json := TJSONUtil.Create;
  json['code'] := 0;
  json['count'] := historyAsArray.Count;
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




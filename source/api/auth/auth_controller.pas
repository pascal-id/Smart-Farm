unit auth_controller;
{
  USAGE:

  [x] Login
  curl "http://smartfarm.pascal-id.test/auth/" -d 'username=tigor&password=123456'

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler, 
    database_lib, string_helpers, dateutils, datetime_helpers;

{$include ../common/smartfarm.inc}

type
  TAuthModule = class(TMyCustomWebModule)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common, auth_model;

constructor TAuthModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TAuthModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TAuthModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

// GET Method Handler
procedure TAuthModule.Get;
begin
  Response.Content := '{}';
end;

// POST Method Handler
procedure TAuthModule.Post;
var
  username, password: String;
  json: TJSONUtil;
begin
  username := _POST['username'];
  password := _POST['password'];
  if username.IsEmpty or password.IsEmpty then
    OutputJson( 400, ERR_INVALID_PARAMETER);

  DataBaseInit();
  json := TJSONUtil.Create;
  json['code'] := 404;
  json['msg'] := ERR_AUTH_INVALID;
  with TAuthModel.Create() do
  begin
    if Login(username, password) then
    begin
      json['code'] := 0;
      json['msg'] := OK;
      json['user_email'] := Email;
      json['token'] := Token;
      json['expired'] := ExpiredDate.AsString;
    end;
    Free;
  end;

  Response.Content:= json.AsJSON;
  json.Free;
end;



end.


unit auth_model;

{$mode objfpc}{$H+}

interface

uses
  hash_tools, common, fpjson, fastplaz_handler,
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers;

{$include ../../common/smartfarm.inc}

type

  { TAuthModel }

  TAuthModel = class(TSimpleModel)
  private
    FSlug: string;
    FToken: string;
  public
    constructor Create(const DefaultTableName: string = '');

    function Login(AUsername, APassword: string):Boolean;
    property Token: string read FToken;
    property Slug: string read FSlug;
  end;

function isAuthenticated: boolean;

implementation

constructor TAuthModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table name = devices
  FToken := '';
end;

function TAuthModel.Login(AUsername, APassword: string): Boolean;
var
  shaString, selectUser: string;
  userArray: TJSONArray;
begin
  Result := False;
  shaString := SHA256(AUsername + '|' + APassword);
  selectUser:= 'SELECT username, email, name, token FROM users WHERE status_id=0 ' +
    #10'AND username='''+AUsername+'''' +
    #10'AND token='''+shaString+'''';
  userArray := TJSONArray.Create;
  if not QueryOpenToJson(selectUser, userArray, False) then
  begin
    userArray.Free;
    Exit;
  end;

  if userArray.Count = 0 then
  begin
    userArray.Free;
    Exit;
  end;

  FSlug := userArray.Items[0].Items[0].AsString;
  FToken := userArray.Items[0].Items[3].AsString;
  userArray.Free;
  Result := True;
end;

function isAuthenticated: boolean;
var
  authstring, token: string;
begin
  Result := False;
  //authstring := Header['Authorization'];
  authstring := GetEnvironmentVariable('Authorization');
  token := GetEnvironmentVariable('Token');

  //TODO: check authentication

  //if token.isEmpty then
  //  Exit;



  Result := True;
end;


end.


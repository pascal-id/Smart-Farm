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
    FClientID: string;
    FEmail: string;
    FExpiredDate: TDateTime;
    FSlug: string;
    FToken: string;
  public
    constructor Create(const DefaultTableName: string = '');

    function Login(AUsername, APassword: string): Boolean;
    function IsTokenValid(AToken: string): Boolean;

    property ClientID: string read FClientID;
    property Token: string read FToken;
    property Slug: string read FSlug;
    property Email: string read FEmail;
    property ExpiredDate: TDateTime read FExpiredDate;
  end;

function isAuthenticated: boolean;

var
  authToken, authSlug: String;

implementation

constructor TAuthModel.Create(const DefaultTableName: string = '');
begin
  inherited Create('auth');
  primaryKey := 'aid';
  FToken := '';
end;

function TAuthModel.Login(AUsername, APassword: string): Boolean;
var
  shaString, selectUser: string;
  userArray: TJSONArray;
begin
  Result := False;
  shaString := SHA256(AUsername + '|' + APassword);
  selectUser:= 'SELECT uid, username, email, name, token FROM users WHERE status_id=0 ' +
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

  FToken := SHA256(AUsername + Now.AsString + RandomString(10,15));
  FExpiredDate := Now.IncHour(24);
  Value['ref_id'] := 0;
  Value['user_id'] := userArray.Items[0].Items[0].AsInteger;
  Value['token'] := FToken;
  Value['expired'] := FExpiredDate;
  if not Save() then
  begin
    userArray.Free;
    Exit;
  end;

  FSlug := userArray.Items[0].Items[1].AsString;
  FEmail := userArray.Items[0].Items[2].AsString;
  userArray.Free;
  Result := True;
end;

function TAuthModel.IsTokenValid(AToken: string): Boolean;
begin
  Result := False;
  if AToken.IsEmpty then Exit;

  AddJoin('users', 'uid', 'auth.user_id', ['username', 'email']);
  if not FindFirst(['auth.token='''+AToken+'''', 'expired > '''+ Now.AsString +''''], '', 'aid, auth.user_id, expired') then
    Exit;

  FExpiredDate := Value['expired'];
  FEmail := Value['email'];
  FSlug := Value['username'];
  Result := True;
end;

function isAuthenticated: boolean;
var
  clientID: string;
begin
  Result := True; //TODO: make it False
  clientID := GetEnvironmentVariable('client-id');
  authToken := GetEnvironmentVariable('Token');

  if authToken.IsEmpty then
    Exit;

  DataBaseInit();

  with TAuthModel.Create() do
  begin
    if not IsTokenValid(authToken) then
    begin
      Free;
      Exit;
    end;

    authSlug := FSlug;
    Free;
  end;

  Result := True;
end;


end.


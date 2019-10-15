unit auth_model;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler,
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TAuthModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

function isAuthenticated: boolean;

implementation

constructor TAuthModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table name = devices
end;

function isAuthenticated: boolean;
var
  authstring: string;
begin
  Result := False;
  //authstring := Header['Authorization'];
  authstring := GetEnvironmentVariable('Authorization');


  //TODO: check authentication


  Result := True;
end;


end.


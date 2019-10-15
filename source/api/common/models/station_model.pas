unit station_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TStationModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

constructor TStationModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( DefaultTableName); // table name = stations
  primaryKey:= 'sid';
end;

end.


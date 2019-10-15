unit node_history_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TNodeHistoryModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

constructor TNodeHistoryModel.Create(
  const DefaultTableName: string = '');
begin
  inherited Create('node_history'); // table name = device_historys
  primaryKey := 'nhid';
end;

end.


unit node_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TNodeModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

constructor TNodeModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table name = devices
  //primaryKey:= 'nid';
end;

end.


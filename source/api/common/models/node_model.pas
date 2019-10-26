unit node_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers,
  array_helpers;

type

  { TNodeModel }

  TNodeModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');

    function IsNodeExists( AStationID: String; AID: String): Boolean;
  end;

implementation

uses common, auth_model;

constructor TNodeModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table name = devices
  //primaryKey:= 'nid';
end;

function TNodeModel.IsNodeExists(AStationID: String; AID: String): Boolean;
var
  whereAsArray: TStringArray;
begin
  Result := False;

  // Prepare data selection
  whereAsArray.Add('stations.user_id=' + i2s(authUserId));
  whereAsArray.Add('stations.status_id=0');
  whereAsArray.Add('nodes.status_id=0');
  whereAsArray.Add('stations.slug="' + AStationID + '"');
  if not AID.IsEmpty then
  begin
    whereAsArray.Add('nodes.slug="' + AID + '"');
  end;
  AddJoin('stations', 'sid', 'station_id', []);
  Result := Find(whereAsArray);
end;

end.


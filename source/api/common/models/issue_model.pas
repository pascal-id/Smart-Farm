unit issue_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TIssuesModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

constructor TIssuesModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( 'issues');
  primaryKey := 'iid';
end;

end.


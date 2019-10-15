unit issue_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler, 
    database_lib, string_helpers, dateutils, datetime_helpers;

{$include ../common/smartfarm.inc}

type
  TIssuesModule = class(TMyCustomWebModule)
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

constructor TIssuesModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TIssuesModule.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TIssuesModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest
  );
begin
  Response.ContentType := 'application/json';
  if not isAuthenticated then
    OutputJson(401, ERR_NOT_PERMITTED);
end;

// GET Method Handler
procedure TIssuesModule.Get;
begin
  //---
  Response.Content := '{}';
end;

// POST Method Handler
procedure TIssuesModule.Post;
begin
  //---
  Response.Content := '{}';
end;



end.


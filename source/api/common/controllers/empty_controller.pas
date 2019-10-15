unit empty_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, fpjson, HTTPDefs, fastplaz_handler, database_lib, 
    string_helpers, dateutils, datetime_helpers;

type

  { TEmptyModule }

  TEmptyModule = class(TMyCustomWebModule)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
    procedure Options; override;
  end;

implementation

uses common, json_lib;

procedure TEmptyModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest
  );
begin
  Response.ContentType := 'application/json';
end;

constructor TEmptyModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TEmptyModule.Destroy;
begin
  inherited Destroy;
end;

// GET Method Handler
procedure TEmptyModule.Get;
begin
  Response.Content := '{"state":"empty"}';
end;

// POST Method Handler
procedure TEmptyModule.Post;
begin
  Response.Content := '{"state":"empty"}';
end;

// OPTIONS Method Handler
procedure TEmptyModule.Options;
begin
  Response.Code := 204;
  Response.Content := '';
end;




initialization

end.


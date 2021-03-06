unit utils;

{$mode objfpc}{$H+}

interface

uses
  fpHTTPClient,
  fpJSON,
  jsonparser;

type
  TKeyValuePair = record
    Key,Value: String;
  end;

  THTTPMethod = (hmGET,hmPOST);

{$ifdef enablelogging}
var
  IsLoggingEnabled: Boolean;
{$endif}

procedure Log(const AMessage: String = ''); inline;
function KeyValuePair(const AKey,AValue: String): TKeyValuePair; inline;
// lazy typer version og KeyValuePair
function KVP(const AKey,AValue: String): TKeyValuePair; inline;
function URLEncodeParams(const AParams: array of TKeyValuePair): String;
// ABody is for POST only and must be URL encoded (use KeyValuePair + URLEncodeParams above to make one)!!!
function GetJSONResponse(const AMethod: THTTPMethod; const AURL: String; const AHeaders: array of TKeyValuePair; const ABody: String = ''): TJSONData;
function NodeValueToIntDef(const AJSONData: TJSONData; const APath: String; const ADefault: Integer): Integer;
function NodeValueToStrDef(const AJSONData: TJSONData; const APath: String; const ADefault: String): String;

implementation

uses
  SysUtils;

procedure Log(const AMessage: String = ''); inline;
begin
  {$ifdef enablelogging}
  if IsLoggingEnabled then WriteLn(AMessage);
  {$endif}
end;

function KeyValuePair(const AKey,AValue: String): TKeyValuePair; inline;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

function KVP(const AKey,AValue: String): TKeyValuePair; inline;
begin
  Result := KeyValuePair(AKey, AValue);
end;

function URLEncodeParams(const AParams: array of TKeyValuePair): String;
var
  LParam: TKeyValuePair;
begin
  Result := '';
  for LParam in AParams do
    Result := Result + '&' + EncodeURLElement(LParam.Key) + '=' + EncodeURLElement(LParam.Value);
  Delete(Result,1,1); // Remove first '&'
end;

function GetJSONResponse(const AMethod: THTTPMethod; const AURL: String; const AHeaders: array of TKeyValuePair; const ABody: String = ''): TJSONData;
var
  LHeader: TKeyValuePair;
  LResponseBody: String;
begin
  Result := nil;
  try
    LResponseBody := '';
    case AMethod of
      hmGet : begin
        Log('GET ' + AURL);
        with TFPHTTPClient.Create(nil) do
          try
            for LHeader in AHeaders do
              RequestHeaders.Values[LHeader.Key] := LHeader.Value;
            LResponseBody := Get(AURL);
          finally
            Free;
          end;
        Log(LResponseBody);
      end;
      hmPost: begin
        Log('POST ' + AURL);
        Log(ABody);
        with TFPHTTPClient.Create(nil) do
          try
            for LHeader in AHeaders do
              RequestHeaders.Values[LHeader.Key] := LHeader.Value;
            LResponseBody := FormPost(AURL, ABody);
          finally
            Free;
          end;
        Log(LResponseBody);
      end;
    end;
    Log;
    Result := GetJSON(LResponseBody);
  finally
    if not Assigned(Result) then Result := TJSONObject.Create;
  end;
end;

function NodeValueToIntDef(const AJSONData: TJSONData; const APath: String; const ADefault: Integer): Integer;
var
  LNode: TJSONData;
begin
  LNode := AJSONData.FindPath(APath);
  if (Assigned(LNode)) then
    Result := LNode.AsInteger
  else
    Result := ADefault;
end;

function NodeValueToStrDef(const AJSONData: TJSONData; const APath: String; const ADefault: String): String;
var
  LNode: TJSONData;
begin
  LNode := AJSONData.FindPath(APath);
  if (Assigned(LNode)) then
    Result := LNode.AsString
  else
    Result := ADefault;
end;

{$ifdef enablelogging}
initialization
  IsLoggingEnabled := true;
{$endif}

end.

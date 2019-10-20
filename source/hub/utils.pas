unit utils;

{$mode objfpc}{$H+}

interface

uses
  fpHTTPClient,
  fpJSON,
  jsonparser;

type
  THTTPMethod = (hmGET,hmPOST);
  TKeyValuePair = record
    Key,Value: String;
  end;

function KeyValuePair(const AKey,AValue: String): TKeyValuePair; inline;
// lazy typer version og KeyValuePair
function KVP(const AKey,AValue: String): TKeyValuePair; inline;
function URLEncodeParams(const AParams: array of TKeyValuePair): String;
// ABody is for POST only and must be URL encoded (use KeyValuePair + URLEncodeParams above to make one)!!!
function GetJSONResponse(const AMethod: THTTPMethod; const AURL: String; const ABody: String = ''): TJSONData;

implementation

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

function GetJSONResponse(const AMethod: THTTPMethod; const AURL: String; const ABody: String = ''): TJSONData;
var
  LResponseBody: String;
begin
  Result := nil;
  try
    LResponseBody := '';
    case AMethod of
      hmGet : begin
        WriteLn('GET ' + AURL);
        LResponseBody := TFPHTTPClient.SimpleGet(AURL);
        WriteLn(LResponseBody);
      end;
      hmPost: begin
        WriteLn('POST ' + AURL);
        WriteLn(ABody);
        LResponseBody := TFPHTTPClient.SimpleFormPost(AURL, ABody);
        WriteLn(LResponseBody);
      end;
    end;
    WriteLn;
    Result := GetJSON(LResponseBody);
  finally
    if not Assigned(Result) then Result := TJSONObject.Create;
  end;
end;

end.

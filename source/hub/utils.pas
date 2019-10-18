unit utils;

{$mode objfpc}{$H+}

interface

uses
  fpHTTPClient,
  fpJSON,
  jsonparser;

type
  THTTPMethod = (hmGET,hmPOST);

function GetJSONResponse(const AMethod: THTTPMethod; const AURL: String; const ABody: String = ''): TJSONData;

implementation

// ABody is for POST only and must be URL encoded!!!
function GetJSONResponse(const AMethod: THTTPMethod; const AURL: String; const ABody: String = ''): TJSONData;
var
  LResponseBody: String;
begin
  case AMethod of
    hmGet : LResponseBody := TFPHTTPClient.SimpleGet(AURL);
    hmPost: LResponseBody := TFPHTTPClient.SimpleFormPost(AURL, ABody);
  end;

  // debugging
  case AMethod of
    hmGet: begin
      WriteLn('GET ' + AURL);
      WriteLn(LResponseBody);
    end;
    hmPost: begin
      WriteLn('POST ' + AURL);
      WriteLn(ABody);
      WriteLn(LResponseBody);
    end;
  end;
  WriteLn;

  Result := GetJSON(LResponseBody);
end;

end.

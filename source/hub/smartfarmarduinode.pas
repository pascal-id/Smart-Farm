unit SmartFarmArduinode;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DateUtils;

type
  TUpdateData = record
    Temperature: Double;
    Humidity: Integer;
    IsSprinkleOn: Boolean;
  end;

var
  GetTemperatureURL,
  GetHumidityURL,
  TurnSprinkleOnURL,
  TurnSprinkleOffURL,
  GetSprinkleStatusURL: String;

function GetUpdateData: TUpdateData;
procedure ToggleSprinkle(const AIsOn: Boolean);
procedure TurnSprinkleOnWithTimeout(const ATimeout: Cardinal);

implementation

uses
  fpJSON,
  utils;

function GetUpdateData: TUpdateData;
var
  LStateNode: TJSONData;
begin
  try
    with GetJSONResponse(hmGET, GetTemperatureURL) do
      try
        LStateNode := FindPath('data.nilai');
        if Assigned(LStateNode) then Result.Temperature := LStateNode.AsFloat;
      finally
        Free;
      end;

    with GetJSONResponse(hmGET, GetHumidityURL) do
      try
        LStateNode := FindPath('data.nilai');
        if Assigned(LStateNode) then Result.Humidity := LStateNode.AsInteger;
      finally
        Free;
      end;

    with GetJSONResponse(hmGET, GetSprinkleStatusURL) do
      try
        LStateNode := FindPath('data.status');
        Result.IsSprinkleOn := Assigned(LStateNode) and (LStateNode.AsString = 'nyala');
      finally
        Free;
      end;
  except
    on e: Exception do begin
      WriteLn(e.Message);
    end
  end;
end;

procedure ToggleSprinkle(const AIsOn: Boolean);
var
  LURL: String;
  LResponseBody: String;
begin
  try
    if AIsOn then
      LURL := TurnSprinkleOnURL
    else
      LURL := TurnSprinkleOffURL;
    with GetJSONResponse(hmGET, LURL) do
      try
        // cek gagal atau berhasil?
      finally
        Free;
      end
  except
    on e: Exception do begin
      WriteLn(e.Message);
    end;
  end;
end;

type
  TSprinkleTimeoutThread = class(TThread)
  private
    FTimeout: Cardinal;
  public
    constructor Create(const ATimeout: Cardinal);
    procedure Execute; override;
  end;

constructor TSprinkleTimeoutThread.Create(const ATimeout: Cardinal);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FTimeout := ATimeout;
  Resume;
end;

procedure TSprinkleTimeoutThread.Execute;
var
  StartTime: TTime;
begin
  ToggleSprinkle(true);
  StartTime := Time;
  while IncMilliSecond(StartTime,FTimeout) >= Time do begin
    Sleep(1);
  end;
  ToggleSprinkle(false);
end;

procedure TurnSprinkleOnWithTimeout(const ATimeout: Cardinal);
begin
  TSprinkleTimeoutThread.Create(ATimeout);
end;

end.

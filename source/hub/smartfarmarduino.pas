unit SmartFarmArduino;

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
  DeviceID,
  GetTemperatureURL,
  GetHumidityURL,
  TurnSprinkleOnURL,
  TurnSprinkleOffURL,
  GetSprinkleStatusURL: String;

function GetUpdateData: TUpdateData;
procedure ToggleSprinkle(const AIsOn: Boolean);
procedure TurnSprinkleOnWithTimeout(const ATimeout: Cardinal; const AUseThreads: Boolean = false);

implementation

uses
  fpJSON,
  utils;

function GetURLParam(const AURL: String): String;
var
  QuestionMarkPos: Integer;
begin
  Result := '';
  QuestionMarkPos := Pos('?',AURL);
  if QuestionMarkPos > 0 then begin
    Result := Copy(AURL,QuestionMarkPos + 1, Length(AURL) - QuestionMarkPos);
  end
end;

function GetUpdateData: TUpdateData;
type
  TReturnType = (rtFloat,rtInteger,rtString);
  TBombardResult = record
    Assigned: Boolean;
    case TReturnType of
      rtFloat  : (FloatValue  : Double);
      rtInteger: (IntegerValue: Integer);
      rtString : (StringValue : ShortString);
  end;

  // due to Arduino's inconsistent HTTP response, we need this function so that it will keep hitting the same
  // endpoint until the returned param value in the response is the same as the one we sent
  function BombardUntilCorrect(const AURL,APath: String; const AReturnType: TReturnType): TBombardResult;
  var
    LStateNode: TJSONData;
    Continue: Boolean;
  begin
    Continue := true;
    repeat
      with GetJSONResponse(hmGET, AURL, []) do
        try
          LStateNode := FindPath('param');
          if Assigned(LStateNode) and (LStateNode.AsString = GetURLParam(AURL)) then begin
            LStateNode := FindPath(APath);
            Result.Assigned := Assigned(LStateNode);
            if Result.Assigned then
              case AReturnType of
                rtFloat  : Result.FloatValue   := LStateNode.AsFloat;
                rtInteger: Result.IntegerValue := LStateNode.AsInteger;
                rtString : Result.StringValue  := LStateNode.AsString;
              end;
            Continue := false;
          end;
        finally
          Free;
        end;
    until not Continue;
  end;

begin
  Result.Temperature  := BombardUntilCorrect(GetTemperatureURL, 'data.nilai', rtFloat).FloatValue;
  Result.Humidity     := BombardUntilCorrect(GetHumidityURL, 'data.nilai', rtInteger).IntegerValue;
  Result.IsSprinkleOn := BombardUntilCorrect(GetSprinkleStatusURL, 'data.status', rtString).StringValue = 'nyala';
end;

procedure ToggleSprinkle(const AIsOn: Boolean);
var
  LURL: String;
  Continue: Boolean;
  LStateNode: TJSONData;
begin
  if AIsOn then
    LURL := TurnSprinkleOnURL
  else
    LURL := TurnSprinkleOffURL;

  Continue := true;
  repeat
    with GetJSONResponse(hmGET, LURL, []) do
      try
        LStateNode := FindPath('param');
        if Assigned(LStateNode) and (LStateNode.AsString = GetURLParam(LURL)) then Continue := false;
      finally
        Free;
      end;
  until not Continue;
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
  Start;
end;

procedure TSprinkleTimeoutThread.Execute;
begin
  ToggleSprinkle(true);
  Sleep(FTimeout);
  ToggleSprinkle(false);
end;

procedure TurnSprinkleOnWithTimeout(const ATimeout: Cardinal; const AUseThreads: Boolean = false);
begin
  if AUseThreads then
    TSprinkleTimeoutThread.Create(ATimeout)
  else begin
    ToggleSprinkle(true);
    Sleep(ATimeout);
    ToggleSprinkle(false);
  end;
end;

end.

program SmartFarmHub;

{$mode objfpc}{$H+}

uses
  cthreads,
  SysUtils,
  DateUtils,
  ssockets,
  fphttpclient,
  IniFiles,
  SmartFarmServer,
  SmartFarmArduino;

procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
begin
  if E <> nil then begin
    WriteLn(StdErr,E.ClassName + ': ' + E.Message);
  end;
  WriteLn(BackTraceStrFunc(ExceptAddr));
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    WriteLn(BackTraceStrFunc(Frames[I]));
end;

procedure ReadAndPopulateConfig;
var
  Config: TIniFile;
begin
  Config := TIniFile.Create('config.ini');
  try
    SmartFarmServer.StationID := Config.ReadString('Server','StationID','');
    SmartFarmServer.GetUpdateURL := Config.ReadString('Server','GetUpdateURL','');
    SmartFarmServer.PostUpdateURL := Config.ReadString('Server','PostUpdateURL','');

    SmartFarmServer.NodeID := Config.ReadString('RaspberryPi','NodeID','');

    SmartFarmArduino.GetTemperatureURL := Config.ReadString('Arduino','GetTemperatureURL','');
    SmartFarmArduino.GetHumidityURL := Config.ReadString('Arduino','GetHumidityURL','');
    SmartFarmArduino.TurnSprinkleOnURL := Config.ReadString('Arduino','TurnSprinkleOnURL','');
    SmartFarmArduino.TurnSprinkleOffURL := Config.ReadString('Arduino','TurnSprinkleOffURL','');
    SmartFarmArduino.GetSprinkleStatusURL := Config.ReadString('Arduino','GetSprinkleStatusURL','');
  finally
    Config.Free;
  end;
end;

type
  TSprinkleState = (ssKeep,ssOff,ssOn);

function GetNextSprinkleState(const AServerUpdateData: SmartFarmServer.TUpdateData; const AArduinoUpdateData: SmartFarmArduino.TUpdateData): TSprinkleState;
var
  LSchedule: TSchedule;
  LScheduleDateTime: TDateTime;
begin
  Result := ssKeep;

  // manual springkle state change request
  if not AArduinoUpdateData.IsSprinkleOn and (AServerUpdateData.Sprinkle.State = 0) then Result := ssOn;
  if     AArduinoUpdateData.IsSprinkleOn and (AServerUpdateData.Sprinkle.State = 1) then Result := ssOff;

  // handle schedules only when there's no manual request
  if Result = ssKeep then begin
    for LSchedule in AServerUpdateData.Schedules do
      if LSchedule.IsActive then begin
        // turn sprinkle on automatically...

        // if humidity reaches or drops below this point
        if (LSchedule.Type_ = 0) and (LSchedule.Mode = 0) and (AArduinoUpdateData.Humidity <= StrToIntDef(LSchedule.Value,0))  then Result := ssOn;
        // if temperature reaches or drops below this point
        if (LSchedule.Type_ = 0) and (LSchedule.Mode = 1) and (AArduinoUpdateData.Temperature >= StrToIntDef(LSchedule.Value,0)) then Result := ssOn;
        // if daily schedule and time matches now
        if (LSchedule.Type_ = 1) and (LSchedule.Mode = 0)
          and (Byte(DayOfTheWeek(Now) - 1) in LSchedule.Days)
          and (Format('%02d:%02d',[HourOfTheDay(Now),MinuteOfTheDay(Now)]) = LSchedule.Value)
        then Result := ssOn;
        // if one time schedule matches now
        if (LSchedule.Type_ = 1) and (LSchedule.Mode = 1) then begin
          LScheduleDateTime := StrToDateTime(LSchedule.Value);
          if (Now >= LScheduleDateTime) and (SecondsBetween(Now, LScheduleDateTime) < 60) then Result := ssOn;
        end;
      end;
  end;
end;

var
  ServerUpdateData: SmartFarmServer.TUpdateData;
  ArduinoUpdateData: SmartFarmArduino.TUpdateData;
begin
  ReadAndPopulateConfig;
  while true do begin
    try
      ArduinoUpdateData := SmartFarmArduino.GetUpdateData;
      ServerUpdateData := SmartFarmServer.GetUpdateData;

      case GetNextSprinkleState(ServerUpdateData, ArduinoUpdateData) of
        ssOn : begin
          SmartFarmArduino.ToggleSprinkle(true);
          ArduinoUpdateData.IsSprinkleOn := true;
        end;
        ssOff: begin
          SmartFarmArduino.ToggleSprinkle(false);
          ArduinoUpdateData.IsSprinkleOn := false;
        end;
        ssKeep: ; // intentionally do nothing
      end;

      SmartFarmServer.UpdateEnvCondData(ArduinoUpdateData.Temperature,ArduinoUpdateData.Humidity,ArduinoUpdateData.IsSprinkleOn);
    except
      on e: ESocketError do
        DumpExceptionCallStack(e);
      on e: EHTTPClient do
        DumpExceptionCallStack(e);
    end;
  end;
end.

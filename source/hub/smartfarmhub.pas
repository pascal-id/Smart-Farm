program SmartFarmHub;

{$mode objfpc}{$H+}

uses
  cthreads,
  SysUtils,
  DateUtils,
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

var
  SprinkleTurnOnTimeoutMilliseconds: Integer;

procedure ReadAndPopulateConfig;
var
  Config: TIniFile;
begin
  Config := TIniFile.Create('config.ini');
  try
    SmartFarmServer.Token          := Config.ReadString('Server','Token','');
    SmartFarmServer.StationID      := Config.ReadString('Server','StationID','');
    SmartFarmServer.NodeID         := Config.ReadString('Server','NodeID','');
    SmartFarmServer.GetUpdateURL   := Config.ReadString('Server','GetUpdateURL','');
    SmartFarmServer.GetScheduleURL := Config.ReadString('Server','GetScheduleURL','');
    SmartFarmServer.PostUpdateURL  := Config.ReadString('Server','PostUpdateURL','');

    SprinkleTurnOnTimeoutMilliseconds := StrToIntDef(Config.ReadString('RaspberryPi','SprinkleTurnOnTimeoutMilliseconds',''),5000);

    SmartFarmArduino.GetTemperatureURL    := Config.ReadString('Arduino','GetTemperatureURL','');
    SmartFarmArduino.GetHumidityURL       := Config.ReadString('Arduino','GetHumidityURL','');
    SmartFarmArduino.TurnSprinkleOnURL    := Config.ReadString('Arduino','TurnSprinkleOnURL','');
    SmartFarmArduino.TurnSprinkleOffURL   := Config.ReadString('Arduino','TurnSprinkleOffURL','');
    SmartFarmArduino.GetSprinkleStatusURL := Config.ReadString('Arduino','GetSprinkleStatusURL','');
  finally
    Config.Free;
  end;
end;

type
  TSprinkleState = (ssKeep,ssOff,ssOn,ssOnTimeout);

function GetNextSprinkleState(const AServerUpdateData: SmartFarmServer.TUpdateData; const AArduinoUpdateData: SmartFarmArduino.TUpdateData): TSprinkleState;
var
  LSchedule: TSchedule;
  LScheduleValue,LToday,LNowHour,LNowMinute: Integer;
  LIsSchedulePastNow,LIsOvertimeWithinTolerance: Boolean;
  LScheduleDateTime: TDateTime;
begin
  Result := ssKeep;

  // manual sprinkle state change request
  if not AArduinoUpdateData.IsSprinkleOn and (AServerUpdateData.Sprinkle.State = 0) then begin
    Result := ssOn;
    WriteLn('CASE 1: Manual springkle on request');
  end;

  if AArduinoUpdateData.IsSprinkleOn and (AServerUpdateData.Sprinkle.State = 1) then begin
    Result := ssOff;
    WriteLn('CASE 2: Manual springkle off request');
  end;

  // handle schedules only when there's no manual request
  if Result = ssKeep then begin
    for LSchedule in AServerUpdateData.Schedules do
      if LSchedule.IsActive then begin
        // type: 0 & mode: 0 => trigger by humidity
        if (LSchedule.Type_ = 0) and (LSchedule.Mode = 0) then begin
          LScheduleValue := StrToIntDef(LSchedule.Value,0);

          if (AArduinoUpdateData.Humidity >= LScheduleValue) and not AArduinoUpdateData.IsSprinkleOn then begin
            Result := ssOnTimeout;
            writeln('CASE 3: Humidity(',AArduinoUpdateData.Humidity,') >= Schedule(',LScheduleValue,'), springkle off');
          end;
          // else if (AArduinoUpdateData.Humidity < LScheduleValue) and AArduinoUpdateData.IsSprinkleOn then begin
          //   Result := ssOff;
          //   writeln('CASE 4: Humidity(',AArduinoUpdateData.Humidity,') < Schedule(',LScheduleValue,'), springkle on');
          // end;
        end;

        if Result = ssKeep then
          // type: 0 & mode: 1 => trigger by temperature
          if (LSchedule.Type_ = 0) and (LSchedule.Mode = 1) then begin
            LScheduleValue := StrToIntDef(LSchedule.Value,0);

            if (AArduinoUpdateData.Temperature >= LScheduleValue) and not AArduinoUpdateData.IsSprinkleOn then begin
              Result := ssOnTimeout;
              WriteLn('CASE 5: Temperature(',AArduinoUpdateData.Temperature,') >= Schedule(',LScheduleValue,'), springkle off');
            end;
            // else if (AArduinoUpdateData.Temperature < LScheduleValue) and AArduinoUpdateData.IsSprinkleOn then begin
            //   Result := ssOff;
            //   WriteLn('CASE 6: Temperature(',AArduinoUpdateData.Temperature,') < Schedule(',LScheduleValue,'), springkle on');
            // end;
          end;

        if Result = ssKeep then
          // type: 1 & mode: 0 => trigger by daily schedule
          if (LSchedule.Type_ = 1) and (LSchedule.Mode = 0) then begin
            LToday     := DayOfTheWeek(Now);
            LNowHour   := HourOfTheDay(Now);
            LNowMinute := MinuteOfTheDay(Now);

            if (Byte(LToday - 1) in LSchedule.Days)
              and (Format('%02d:%02d',[LNowHour,LNowMinute]) = LSchedule.Value)
              and not AArduinoUpdateData.IsSprinkleOn
            then begin
              Result := ssOnTimeout;
              WriteLn('CASE 7: Daily schedule on day ',DefaultFormatSettings.LongDayNames[DayOfTheWeek(Now)],' at ',LNowHour,':',LNowMinute,', springkle off');
            end;
          // else if AArduinoUpdateData.IsSprinkleOn then begin
          //     Result := ssOff;
          //     WriteLn('CASE 8: No matching daily schedule, springkle on');
          //   end;
          end;

        if Result = ssKeep then
          // type: 1 & mode: 1 => trigger by one time schedule
          if (LSchedule.Type_ = 1) and (LSchedule.Mode = 1) then begin
            LScheduleDateTime          := StrToDateTime(LSchedule.Value);
            LIsSchedulePastNow         := Now >= LScheduleDateTime;
            LIsOvertimeWithinTolerance := SecondsBetween(Now, LScheduleDateTime) < 60; // any second within the same minute will trigger

            if LIsSchedulePastNow and LIsOvertimeWithinTolerance and not AArduinoUpdateData.IsSprinkleOn then begin
              Result := ssOnTimeout;
              WriteLn('CASE 9: ScheduleStart(',DateToStr(LScheduleDateTime),') <= Now(',DateToStr(Now),') <= ScheduleEnd(',DateToStr(LScheduleDateTime + OneMinute),'), springkle off');
            end;
            // else if AArduinoUpdateData.IsSprinkleOn then begin
            //   Result := ssOff;
            //   WriteLn('CASE 10: No matching one time schedule, springkle on');
            // end;
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
        ssOnTimeout : SmartFarmArduino.TurnSprinkleOnWithTimeout(SprinkleTurnOnTimeoutMilliseconds);
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

      SmartFarmServer.UpdateEnvCondData(ArduinoUpdateData.Temperature,ArduinoUpdateData.Humidity,ServerUpdateData.Sprinkle.State = 0);
    except
      on e: Exception do
        DumpExceptionCallStack(e);
    end;
  end;
end.

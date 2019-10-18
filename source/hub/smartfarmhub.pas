program SmartFarmHub;

{$mode objfpc}{$H+}

uses
  cthreads,
  SysUtils,
  IniFiles,
  SmartFarmServer,
  SmartFarmArduiNode;

var
  Config: TIniFile;
  ServerUpdateData: SmartFarmServer.TUpdateData;
  NodeUpdateData: SmartFarmArduiNode.TUpdateData;
  RestingTimeMS: Integer;
  ScheduleSprinkleRequestMap: TScheduleSprinkleRequestMap;
  ScheduleSprinkleRequest: TScheduleSprinkleRequest;
begin
  Config := TIniFile.Create('config.ini');
  try
    SmartFarmServer.StationID := Config.ReadString('Config','SmartFarmServerStationID','');
    SmartFarmServer.NodeID := Config.ReadString('Config','SmartFarmServerNodeID','');
    SmartFarmServer.GetUpdateURL := Config.ReadString('Config','SmartFarmServerGetUpdateURL','');
    SmartFarmServer.PostUpdateURL := Config.ReadString('Config','SmartFarmServerPostUpdateURL','');
    SmartFarmArduiNode.GetTemperatureURL := Config.ReadString('Config','SmartFarmArduiNodeGetTemperatureURL','');
    SmartFarmArduiNode.GetHumidityURL := Config.ReadString('Config','SmartFarmArduiNodeGetHumidityURL','');
    SmartFarmArduiNode.TurnSprinkleOnURL := Config.ReadString('Config','SmartFarmArduiNodeTurnSprinkleOnURL','');
    SmartFarmArduiNode.TurnSprinkleOffURL := Config.ReadString('Config','SmartFarmArduiNodeTurnSprinkleOffURL','');
    SmartFarmArduiNode.GetSprinkleStatusURL := Config.ReadString('Config','SmartFarmArduiNodeGetSprinkleStatusURL','');
    RestingTimeMS := Config.ReadInteger('Config','RestingTimeMS',10);

    while true do begin
      NodeUpdateData := SmartFarmArduiNode.GetUpdateData;
      SmartFarmServer.UpdateEnvCondData(NodeUpdateData.Temperature,NodeUpdateData.Humidity,NodeUpdateData.IsSprinkleOn);
      WriteLn(Format('Temp = %02f, Hum = %d, SplOn = %s',[NodeUpdateData.Temperature,NodeUpdateData.Humidity,BoolToStr(NodeUpdateData.IsSprinkleOn)]));

      ServerUpdateData := SmartFarmServer.GetUpdateData;

      // Kirim request buka kran kalau belum nyala
      // if not IsSprinkleOn and ServerUpdateData.TurnSprinkleOn then SmartFarmArduiNode.ToggleSprinkle(true);
      // Kirim request tutup kran kalau belum mati
      // if IsSprinkleOn and not ServerUpdateData.TurnSprinkleOn then SmartFarmArduiNode.ToggleSprinkle(false);

      // ScheduleSprinkleRequestMap := ServerUpdateData.ScheduleSprinkleRequestMap;
      // for ScheduleSprinkleRequest in ScheduleSprinkleRequestMap do begin
      //   WriteLn(ScheduleSprinkleRequest.Key,' ',ScheduleSprinkleRequest.Value);
      // end;

      Sleep(RestingTimeMS);
    end;
  finally
    Config.Free;
  end;
end.

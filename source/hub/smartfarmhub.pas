program SmartFarmHub;

{$mode objfpc}{$H+}

uses
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
		SmartFarmServer.GetUpdateURL := Config.ReadString('Config','SmartFarmServerGetUpdateURL','');
		SmartFarmServer.PostUpdateURL := Config.ReadString('Config','SmartFarmServerPostUpdateURL','');
		SmartFarmArduiNode.GetUpdateURL := Config.ReadString('Config','SmartFarmArduiNodeGetUpdateURL','');
		SmartFarmArduiNode.TurnSprinkleOnURL := Config.ReadString('Config','SmartFarmArduiNodeTurnSprinkleOnURL','');
		RestingTimeMS := Config.ReadInteger('Config','RestingTimeMS',10);
		while true do begin
			ServerUpdateData := SmartFarmServer.GetUpdateData;
			if ServerUpdateData.ManualSprinkleRequest then SmartFarmArduiNode.TurnSprinkleOn;
			ScheduleSprinkleRequestMap := ServerUpdateData.ScheduleSprinkleRequestMap;
			for ScheduleSprinkleRequest in ScheduleSprinkleRequestMap do begin
				WriteLn(ScheduleSprinkleRequest.Key,' ',ScheduleSprinkleRequest.Value);
			end;

			NodeUpdateData := SmartFarmArduiNode.GetUpdateData;
			SmartFarmServer.UpdateEnvCondData(NodeUpdateData.Temperature,NodeUpdateData.Humidity);

			Sleep(RestingTimeMS);
		end;
	finally
		Config.Free;
	end;
end.

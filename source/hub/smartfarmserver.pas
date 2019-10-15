unit SmartFarmServer;

{$mode objfpc}{$H+}

interface

uses
	SysUtils,
	generics.collections,
	fpHTTPClient,
  fpJSON;

type
	TScheduleSprinkleRequestMap = specialize TDictionary<String,String>;
	TScheduleSprinkleRequest = specialize TPair<String,String>;

	TUpdateData = record
		ManualSprinkleRequest: Boolean;
		ScheduleSprinkleRequestMap: TScheduleSprinkleRequestMap;
	end;

var
	GetUpdateURL,
	PostUpdateURL: String;

function GetUpdateData: TUpdateData;
procedure UpdateEnvCondData(const ATemperature,AHumidity: Integer);

implementation

var
	UpdateDataResp: TUpdateData;

function GetUpdateData: TUpdateData;
begin
	try
		with GetJSON(TFPHTTPClient.SimpleGet(GetUpdateURL)) do
			try
				// baca field2 dari server
				Result := UpdateDataResp;
			finally
				Free;
			end
	except
		on e: Exception do begin
			WriteLn(e.Message);
		end
	end;
end;

procedure UpdateEnvCondData(const ATemperature,AHumidity: Integer);
begin
	try
		TFPHTTPClient.SimplePost(PostUpdateURL);
	except
		on e: Exception do begin
			WriteLn(e.Message);
		end;
	end;
end;

initialization
	UpdateDataResp.ScheduleSprinkleRequestMap := TScheduleSprinkleRequestMap.Create;

finalization
	UpdateDataResp.ScheduleSprinkleRequestMap.Free;

end.

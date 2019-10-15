unit SmartFarmArduinode;

{$mode objfpc}{$H+}

interface

uses
	SysUtils,
	generics.collections,
	fpHTTPClient,
  fpJSON;

type
	TUpdateData = record
		Temperature: Integer;
		Humidity: Integer;
	end;

var
	GetUpdateURL,
	TurnSprinkleOnURL: String;

function GetUpdateData: TUpdateData;
procedure TurnSprinkleOn;

implementation

function GetUpdateData: TUpdateData;
begin
	try
		with GetJSON(TFPHTTPClient.SimpleGet(GetUpdateURL)) do
			try
				// baca field2 dari server
			finally
				Free;
			end
	except
		on e: Exception do begin
			WriteLn(e.Message);
		end
	end;
end;

procedure TurnSprinkleOn;
begin
	try
		TFPHTTPClient.SimplePost(TurnSprinkleOnURL);
	except
		on e: Exception do begin
			WriteLn(e.Message);
		end;
	end;
end;

end.

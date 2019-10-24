program ArduinoDummyServer;

{$mode objfpc}

uses
  Classes,
  SysUtils,
  fphttpapp,
  httpdefs,
  httproute;

var
  IsSprinkleOn: Boolean;
  Temperature: Double;
  Humidity: Integer;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  LModule,LTmpString: String;
begin
  LModule := ARequest.QueryFields.Values['module'];
  case LModule of
    'suhu'       : begin
      WriteStr(LTmpString,'{"data":{"nilai":',Temperature:1:2,'},"param":"module='+LModule+'"}');
      AResponse.Content := LTmpString;
    end;
    'kelembaban' : begin
      WriteStr(LTmpString,'{"data":{"nilai":',Humidity,'},"param":"module='+LModule+'"}');
      AResponse.Content := LTmpString;
    end;
    'status-kran': begin
      if IsSprinkleOn then LTmpString := 'nyala' else LTmpString := 'mati';
      AResponse.Content := '{"data":{"status":"'+LTmpString+'"},"param":"module='+LModule+'"}';
    end;
    'buka-kran'     : IsSprinkleOn   := true;
    'tutup-kran'    : IsSprinkleOn   := false;
    'set-suhu'      : Temperature    := StrToFloatDef(ARequest.QueryFields.Values['value'],0);
    'set-kelembaban': Humidity       := StrToIntDef(ARequest.QueryFields.Values['value'],0);
    otherwise         AResponse.Code := 400;
  end;
  if AResponse.Content = '' then
    AResponse.Content := '{"param":"module='+LModule+'"}';
end;

begin
  IsSprinkleOn := false;
  HTTPRouter.RegisterRoute('/',@HandleRequest);
  Application.Port := 9000;
  Application.Initialize;
  Application.Run;
end.

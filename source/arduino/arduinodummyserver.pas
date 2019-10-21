program ArduinoDummyServer;

{$mode objfpc}

uses
  classes,
  fphttpapp,
  httpdefs,
  httproute;

var
  IsSprinkleOn: Boolean;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  LSprinkleStatus: String;
begin
  case ARequest.QueryFields.Values['module'] of
    'suhu'       : AResponse.Content := '{"data":{"nilai":28.75},"param":"module=suhu"}';
    'kelembaban' : AResponse.Content := '{"data":{"nilai":888},"param":"module=kelembaban"}';
    'status-kran': begin
      if IsSprinkleOn then LSprinkleStatus := 'nyala' else LSprinkleStatus := 'mati';
      AResponse.Content := '{"data":{"status":"'+LSprinkleStatus+'"},"param":"module=status-kran"}';
    end;
    'buka-kran'  : begin
      IsSprinkleOn := true;
      AResponse.Content := '{"param":"module=buka-kran"}';
    end;
    'tutup-kran' : begin
      IsSprinkleOn := false;
      AResponse.Content := '{"param":"module=tutup-kran"}';
    end;
    otherwise      AResponse.Code    := 400;
  end;
end;

begin
  IsSprinkleOn := false;
  HTTPRouter.RegisterRoute('/',@HandleRequest);
  Application.Port := 9000;
  Application.Initialize;
  Application.Run;
end.

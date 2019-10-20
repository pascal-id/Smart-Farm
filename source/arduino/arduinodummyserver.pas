program ArduinoDummyServer;

{$mode objfpc}

uses
  classes,
  fphttpapp,
  httpdefs,
  httproute;

procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  case ARequest.QueryFields.Values['module'] of
    'suhu'       : AResponse.Content := '{"data":{"nilai":28.75},"param":"module=suhu"}';
    'kelembaban' : AResponse.Content := '{"data":{"nilai":888},"param":"module=kelembaban"}';
    'status-kran': AResponse.Content := '{"data":{"status":"nyala"},"param":"module=status-kran"}';
    'buka-kran'  : AResponse.Content := '{"param":"module=buka-kran"}';
    'tutup-kran' : AResponse.Content := '{"param":"module=tutup-kran"}';
    otherwise      AResponse.Code    := 400;
  end;
end;

begin
  HTTPRouter.RegisterRoute('/',@HandleRequest);
  Application.Port := 9000;
  Application.Initialize;
  Application.Run;
end.

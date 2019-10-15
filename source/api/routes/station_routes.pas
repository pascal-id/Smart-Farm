unit station_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses station_controller;

initialization
  Route[ '/(.*)'] := TStationModule;

end.


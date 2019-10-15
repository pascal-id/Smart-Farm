unit schedule_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses schedule_controller;

initialization
  Route[ '/'] := TScheduleModule; // Main Module

end.


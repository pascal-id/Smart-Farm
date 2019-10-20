unit stats_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses stats_controller;

initialization
  Route[ '/'] := TStatsModule; // Main Module

end.


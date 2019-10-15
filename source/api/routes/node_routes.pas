unit node_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses node_controller, node_history_controller;

initialization
  Route[ '/history/(.*)'] := TNodeHistoryModule;
  Route[ '/(.*)'] := TNodeModule;
end.


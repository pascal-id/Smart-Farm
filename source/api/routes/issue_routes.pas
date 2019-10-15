unit issue_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, issue_controller;

initialization
  Route[ 'info'] := TInfoModule;
  Route[ '/'] := TIssuesModule; // Main Module

end.


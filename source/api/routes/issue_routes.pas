unit issue_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses issue_controller;

initialization
  Route[ '/'] := TIssuesModule; // Main Module

end.


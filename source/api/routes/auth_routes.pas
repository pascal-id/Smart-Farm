unit auth_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses auth_controller;

initialization
  Route[ '/'] := TAuthModule;

end.


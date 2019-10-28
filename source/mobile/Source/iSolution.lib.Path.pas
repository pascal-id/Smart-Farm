unit iSolution.lib.Path;

interface

type
  TCSClass_Path = class
  public
    class function Application : string;
    class function Database : string;
    class function Report : string;
    class function Document(AFileName : string): string;
  end;

implementation

uses System.IOUtils;

{-----------------------------------------------------------------------------
  Procedure : Application
  Author    : VMUser
  Date      : 12 Mar 2014 - 25 Oct 2016
  Arguments :
  Result    : string
  Desc      : This method will try to get the data path where the supporting
              file exists. This is primaryly to get the data path on mobile
              device
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_Path.Application: string;
begin
  {$IFDEF CPUARM}
  Result := TPath.GetDocumentsPath;
  {$ELSE}
  Result := TPath.GetDirectoryName(ParamStr(0));
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Procedure : Database
  Author    : iwancs
  Date      : 30 Mar 2018 - 30 Mar 2018
  Arguments :
  Result    : string
  Desc      : This method will try to get the database path where the supporting
              file exists.
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_Path.Database: string;
begin
  Result := TPath.Combine(TCSClass_Path.Application, 'Database');
end;

{-----------------------------------------------------------------------------
  Procedure : Report
  Author    : VMUser
  Date      : 25 Oct 2016 - 25 Oct 2016
  Arguments :
  Result    : string
  Desc      : This method will try to get the data path where the supporting
              file exists. This is primaryly to get the data path on mobile
              device
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_Path.Report : string;
begin
  Result := TPath.Combine(TCSClass_Path.Application, 'Report');
end;

{-----------------------------------------------------------------------------
  Procedure : Document
  Author    : VMUser
  Date      : 06 Sep 2014 - 06 Sep 2014
  Arguments :
  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_Path.Document(AFileName : string) : string;
begin
  Result := TPath.Combine(TCSClass_Path.Application, AFileName);
end;

end.

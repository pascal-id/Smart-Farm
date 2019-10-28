unit iSolution.lib.Configuration;

interface

uses System.Classes;

type
  TCSClass_Configuration = class
  public
    class procedure ReadSections(
      AFileName: string;
      out ASectionsList : TStringList);
    class procedure ReadConfigs(
      AFileName: string;
      ASectionName: string;
      out AConfigs: TStringList);
    class function ReadConfig(
      AFileName: string;
      ASectionName : string;
      AConfigName : string;
      ADefaultValue : string = '') : string;
    class procedure WriteConfig(
      AFileName : string;
      ASectionName : string;
      AConfigName : string;
      AValue: string);
    class procedure DeleteConfig(
      AFileName : string;
      ASectionName : string;
      AConfigName : string);
    class procedure DeleteSection(
      AFileName : string;
      ASectionName : string);
    class function IsSectionExists(
      AFileName: string;
      ASectionName: string): Boolean;
  end;

implementation

uses System.SysUtils, System.IniFiles;

{ Config File Routine}
{-----------------------------------------------------------------------------
  Procedure : ReadSections
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AFileName: string;	->
              ASectionName: string;	->
              ASections: TStringList;	->

  Result    :
  Desc      :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Configuration.ReadSections(
  AFileName: string;
  out ASectionsList: TStringList);
var
  iniConfig : TMemIniFile;
begin
  if ASectionsList <> nil then
  begin
    if FileExists(AFileName) then
    begin
      iniConfig := TMemIniFile.Create(AFileName);
      try
        iniConfig.ReadSections(ASectionsList);
      finally
        iniConfig.Free;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : ReadConfigs
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AFileName: string;	->
              ASectionName: string;	->
              AConfigs: TStringList;	->

  Result    :
  Desc      :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Configuration.ReadConfigs(
  AFileName : string;
  ASectionName: string;
  out AConfigs: TStringList);
var
  iniConfig : TMemIniFile;
begin
  if AConfigs <> nil then
  begin
    if FileExists(AFileName) then
    begin
      iniConfig := TMemIniFile.Create(AFileName);
      try
        iniConfig.ReadSectionValues(ASectionName,AConfigs);
      finally
        iniConfig.Free;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : ReadConfig
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AFileName: string;	->
              ASectionName: string;	->
              AConfigName: string;	->

  Result    : string
  Desc      :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_Configuration.ReadConfig(
  AFileName : string;
  ASectionName : string;
  AConfigName: string;
  ADefaultValue : string): string;
var
  iniConfig : TMemIniFile;
begin
  Result := ADefaultValue;
  if FileExists(AFileName) then
  begin
    iniConfig := TMemIniFile.Create(AFileName);
    try
      Result := iniConfig.ReadString(ASectionName,AConfigName,ADefaultValue);
    finally
      iniConfig.Free;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : WriteConfig
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AFileName: string;	->
              ASectionName: string;	->
              AValue: string;	->

  Result    :
  Desc      :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Configuration.WriteConfig(
  AFileName : string;
  ASectionName : string;
  AConfigName : string;
  AValue: string);
var
  iniConfig : TMemIniFile;
begin
  iniConfig := TMemIniFile.Create(AFileName);
  try
    iniConfig.WriteString(ASectionName, AConfigName, AValue);
  finally
    iniConfig.UpdateFile;
    iniConfig.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : DeleteConfig
  Author    : iwancs
  Date      : 22 Jul 2018 - 22 Jul 2018
  Arguments : AFileName: string;	->
              ASectionName: string;	->
              AConfigName: string;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Configuration.DeleteConfig(
  AFileName : string;
  ASectionName : string;
  AConfigName: string);
var
  iniConfig : TMemIniFile;
begin
  iniConfig := TMemIniFile.Create(AFileName);
  try
    iniConfig.EraseSection(ASectionName);
  finally
    iniConfig.UpdateFile;
    iniConfig.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : DeleteSection
  Author    : iwancs
  Date      : 22 Jul 2018 - 22 Jul 2018
  Arguments : AFileName: string;	->
              ASectionName: string;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Configuration.DeleteSection
  (AFileName : string;
  ASectionName: string);
begin

end;

{-----------------------------------------------------------------------------
  Procedure : IsSectionExists
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AFileName: string;	->
              ASectionName: string;	->

  Result    : Boolean
  Desc      : This function will check if the section specified is exist in
              the file specified. Return true if exists, false otherwise
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_Configuration.IsSectionExists(
  AFileName : string;
  ASectionName: string): Boolean;
var
  slSections : TStringList;
begin
  slSections := TStringList.Create;
  try
    TCSClass_Configuration.ReadSections(AFileName,slSections);
    Result := slSections.IndexOf(ASectionName) > -1;
  finally
    slSections.Free;
  end;

end;

end.

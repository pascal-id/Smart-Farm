unit iSolution.lib.Logger;

interface

uses System.Classes;

type
  TCSClass_Logger = class
  private
    class procedure WriteLog(
      AMsg : string;
      AFileName: string = '');
  public
    class procedure LogMessage(
      AFileName : string;
      AMessage : string;
      AArgument : array of const);overload;
    class procedure LogMessage(
      AMessage : string;
      AArgument : array of const);overload;
    class procedure LogMessage(AMessage : string);overload;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ Tfunc_Logger }
{-----------------------------------------------------------------------------
  Procedure : WriteLog
  Author    : iwancs
  Date      : 28 May 2018 - 28 May 2018
  Arguments : AMsg: string;	->
              AFileName: string;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Logger.WriteLog(
  AMsg : string;
  AFileName : string
);
var
  sDocumentFileName : string;
  sLogMessage : string;
begin
  sLogMessage := Format(
    '%s : %s' + #13#10,
    [FormatDateTime('dd MMM yyyy HH:nn:ss:zzz',Now),
     AMsg]
  );

  sDocumentFileName := TPath.Combine(
    TPath.GetDocumentsPath,
    Format(
      'Debug_%s.log',
      [FormatDateTime('yyyyMMDD',Now)]
    )
  );
//  sDocumentFileName :=
//    Format(
//      'D:\Debug_%s.log',
//      [FormatDateTime('yyyyMMDD',Now)]
//    );

  if AFileName <> '' then
  begin
    sDocumentFileName := AFileName;
  end;

  try
    TFile.AppendAllText(
      sDocumentFileName,
      sLogMessage,
      TEncoding.Default
    );
  except
  end;
end;

(*
procedure WriteLog(
  AMsg : string;
  AFileName : string = ''
);
var
  sDocumentFileName : string;
  sLogMessage : string;
begin
  sLogMessage := Format(
    '%s : %s' + #13#10,
    [FormatDateTime('dd MMM yyyy HH:nn:ss:zzz',Now),
     AMsg]
  );

  {$IFNDEF MSWINDOWS}
  sDocumentFileName := TPath.GetDocumentsPath + PathDelim +
    Format(
      'Debug_%s.log',
      [FormatDateTime('yyyyMMDD',Now)]
    );
  {$ELSE}
  sDocumentFileName :=
    Format(
      'Debug_%s.log',
      [FormatDateTime('yyyyMMDD',Now)]
    );
  {$ENDIF}
  if AFileName <> '' then
  begin
    sDocumentFileName := AFileName;
  end;

  TFile.AppendAllText(
    sDocumentFileName,
    sLogMessage,
    TEncoding.Default
  );
end;
*)

{-----------------------------------------------------------------------------
  Procedure : LogMessage
  Author    : iwancs
  Date      : 11 Apr 2018 - 11 Apr 2018
  Arguments : AFileName: string;	->
              AMessage: string;	->
              AArgument: array of const;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Logger.LogMessage(
  AFileName : string;
  AMessage: string;
  AArgument: array of const);
var
  sMessage : string;
  sFilename : string;
begin
  sMessage := Format(AMessage,AArgument);
  sFilename := AFileName;
  TThread.CreateAnonymousThread(
    procedure
    begin
      TCSClass_Logger.WriteLog(sMessage, sFileName);
    end
  ).Start;

//  if TThread.CurrentThread.ThreadID = MainThreadID then
//  begin
//    TCSClass_Logger.WriteLog(sMessage, AFileName);
//  end
//  else
//  begin
//    TThread.Synchronize(
//      nil,
//      procedure
//      begin
//        TCSClass_Logger.WriteLog(sMessage, AFileName);
//      end
//    );
//  end;
end;

{-----------------------------------------------------------------------------
  Procedure : LogMessage
  Author    : Win7
  Date      : 05 Feb 2017 - 05 Feb 2017
  Arguments : AMessage: string;	->
              AArgument: array of const;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Logger.LogMessage(
  AMessage: string;
  AArgument: array of const);
begin
  LogMessage('',AMessage,AArgument);
end;

{-----------------------------------------------------------------------------
  Procedure : LogMessage
  Author    : Win7
  Date      : 05 Feb 2017 - 05 Feb 2017
  Arguments : AMessage: string;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_Logger.LogMessage(AMessage: string);
begin
  LogMessage(AMessage,[]);
end;

end.

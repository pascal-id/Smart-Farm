(*-----------------------------------------------------------------------------
 Project     : ApplicationTester
 Unit Name   : iSolution.lib.StringHelper.pas
 Author      : CopyRight® 2013 - Iwan Cahyadi Sugeng - Indonesia
 Email       : iwan.c.sugeng@gmail.com
 Date        : 04 May 2017
 Definitions :
   Types:
   - [NAMES] : [TYPE]
     DESC    : --> Complete description of the property
     USG     : --> Who use them and who set them
   Variables:
   - [NAMES] : [TYPE]
     DESC    : --> Complete description of the property
     SCOP    :
     Ex      :
     USG     : --> Who use them and who set them
   Constants :
   - [NAMES] = [VALUE]
     DESC    : --> Complete description of the property
     USG     : --> Who use them and who set them
   ResourceStrings:
   - [NAMES] = [VALUES]
     DESC    : --> Complete description of the property
     USG     : --> Who use them and who set them
   Properties:
   - [NAMES] : [TYPE]
     DESC    : --> Complete description of the property
     FLD     : --> Field Name that store properties value
     MTD     : --> Method if exist that return or set field value
     VAL     : --> Possible value
     USG     : --> Who use them and who set them
   Rules     :
   - [NAMES]
     DESC    : --> Description of what the rule done
     MODULE  : --> Name of the module containing the rule
     TABLES  : --> The Tablename list for the rule to execute on
   Dataset   :
   - [NAMES]
     DESC    : --> Complete description of the dataset usage and parameter
     METHOD  : --> Method to call
     ARG     : --> Argument for the method
     PARAMS  : --> Parameter needed by the query
     SCRIPT  : --> Script File Name that store the query
     VERSION : --> Script Version used in this unit
 Description :
 How To Use  :
 Todo	     :
 History     :
 Used By     :
-----------------------------------------------------------------------------*)
unit iSolution.lib.StringHelper;

interface

uses System.Classes, System.SysUtils;

type
  TCSRec_Delimeter = record
  public
    class function GetText(AText: String; AIndex: Integer; ADelimeter: Char): string;static;
    class function GetTextQuoted(
      AText: String;
      AIndex: Integer;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function GetName(AText: String; AIndex: Integer; ADelimeter: Char): string;static;
    class function GetValue(AText: String; AName: string; ADelimeter: Char): string;static;
    class function GetValueFromIndex(AText: String; AIndex: Integer; ADelimeter: Char): string;static;
    class function GetCount(AText: string; ADelimeter: Char): Integer;static;
    class function GetCountQuoted(
      AText: string;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): Integer;static;
    class function FindText(AText: string; AName: string;ADelimeter : Char): Integer;static;
    class function FindName(AText: string; AName: string;ADelimeter : Char): Integer;static;
    class function RemoveText(AText: string; AIndex: integer;ADelimeter: Char): string;static;
    class function RemoveName(AText: string; AName: string;ADelimeter: Char): string;static;
  end;

  TCSRec_Trim = record
    class function Left(AText : String; ACount : Integer): String;overload;static;
    class function Right(AText : String; ACount : Integer): String;overload;static;
    class function LeftStr(AText : String; ASubText : String): String;overload;static;
    class function RightStr(AText : String; ASubText : String): String;overload;static;
  end;

  TCSRec_StringHelper = record
  private
    class procedure ParseString(
      AStringList : TStringList;
      AText : string;
      ADelimeter: Char;
      AQuotedStr : Char = '"'
    );static;
    class procedure ParseNameValueWithSpace(
      AStringList : TStringList;
      AText : string;
      ANameValueSeperator : Char = '=';
      AQuotedStr : Char = '"'
    );static;
  public
    class function PartCount(
      AText: string;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): Integer;static;
    class function PartByIndex(
      AText: String;
      AIndex: Integer;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function PartNameByIndex(
      AText: String;
      AIndex: Integer;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function PartValueByName(
      AText: String;
      AName: string;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function PartValueByNameX(
      AText: String;
      AName: string;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function PartValueByIndex(
      AText: String;
      AIndex: Integer;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function PartFind(
      AText: string;
      AValue: string;
      ADelimeter : Char;
      AQuotedStr : Char = '"'): Integer;static;
    class function PartFindByName(
      AText: string;
      AName: string;
      ADelimeter : Char;
      AQuotedStr : Char = '"'): Integer;static;
    class function PartRemoveByIndex(
      AText: string;
      AIndex: integer;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function PartRemoveByName(
      AText: string;
      AName: string;
      ADelimeter: Char;
      AQuotedStr : Char = '"'): string;static;
    class function GetTextBetweenDelimeter(
      AText : string;
      AStartDelimeter : Char;
      AEndDelimeter : Char;
      AIndex : Integer
    ) : string;static;
    class function SpaceToCamelCase(
      AText : string
    ) : string;static;
  end;

  TCSClass_DateHelper = class
    class function StringFLoatToDateTime(AValue : string) : TDateTime;
    class function DateTimeToStringFloat(AValue : TDateTime) : string;
    class function MilliSecondsToTimeWords(AValue : Integer) : string;
  end;

  TCSClass_ByteHelper = class
    class function StrToByte(AValue : string) : Byte;
    class function BytesToStr(AValue : TBytes) : string;
  end;

implementation

{String Manipulation Routine}
{-----------------------------------------------------------------------------
  Procedure : GetText
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	  -> The source delimited text to search from
              AIndex: Integer;	-> The index of the text to find
              ADelimeter: Char;	-> The delimeter char
  Result    : string
  Desc      : Get text at specified index from delimeted list
  Reference : http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetText(
  AText: String;
  AIndex: Integer;
  ADelimeter: Char): string;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := '';
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      if AIndex < slText.Count then
        Result := slText[AIndex];
    finally
      slText.Free;
    end;
  except
    Result := '';
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : GetTextQuoted
  Author    : Win7
  Date      : 05 Apr 2017 - 05 Apr 2017
  Arguments : AText: String;	->
              AIndex: Integer;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetTextQuoted(
  AText: String;
  AIndex: Integer;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      slText.Delimiter := ADelimeter;
      slText.QuoteChar := AQuotedStr;
      slText.DelimitedText := AText;
      if AIndex < slText.Count then
        Result := slText[AIndex];
    finally
      slText.Free;
    end;
  except
    Result := '';
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : GetName
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	  -> The source delimited text to search from
              AIndex: Integer;	-> The index of the text to find
              ADelimeter: Char;	-> The delimeter char
  Result    : string
  Desc      : Get name part of a name value delimeted list
  Reference : http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetName(AText: String; AIndex: Integer; ADelimeter: Char): string;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := '';
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      Result := slText.Names[AIndex];
    finally
      slText.Free;
    end;
  except
    Result := '';
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : GetValue
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	  -> The source delimited text to search from
              AIndex: Integer;	-> The index of the text to find
              ADelimeter: Char;	-> The delimeter char
  Result    : string
  Desc      : Get value part of a name value delimeted list
  Reference : http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetValue(AText: String;AName: string; ADelimeter: Char): string;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := '';
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      Result := slText.Values[AName];
    finally
      slText.Free;
    end;
  except
    Result := '';
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : GetValueFromIndex
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	->
              AIndex: Integer;	->
              ADelimeter: Char;	->

  Result    : string
  Desc      :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetValueFromIndex(AText: String; AIndex: Integer;
  ADelimeter: Char): string;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := '';
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      Result := slText.ValueFromIndex[AIndex];
    finally
      slText.Free;
    end;
  except
    Result := '';
  end;

end;

{-----------------------------------------------------------------------------
  Procedure : GetCount
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	  -> The source delimited text
              ADelimeter: Char;	-> The delimeter char
  Result    : Integer
  Desc      : To get the delimeted text count
  Reference : http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetCount(AText: string;ADelimeter: Char): Integer;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := 0;
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      Result := slText.Count;
    finally
      slText.Free;
    end;
  except
    Result := 0;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : GetCountQuoted
  Author    : Win7
  Date      : 09 Apr 2017 - 09 Apr 2017
  Arguments : AText: string;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : Integer
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.GetCountQuoted(
  AText: string;
  ADelimeter : Char;
  AQuotedStr: Char): Integer;
var
  slText : TStringList;
begin
  Result := 0;
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      slText.Delimiter := ADelimeter;
      slText.QuoteChar := AQuotedStr;
      slText.DelimitedText := AText;
      Result := slText.Count;
    finally
      slText.Free;
    end;
  except
    Result := 0;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : FindText
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: string;	->
              AName: string;	->
              ADelimeter: Char;	->

  Result    : string
  Desc      : This will find AName in AText that seperated with ADelimeter
              It will return the index of AName in AText
              It will return -1 if not found
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.FindText(
  AText : string;
  AName: string;
  ADelimeter: Char): Integer;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := -1;
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      Result := slText.IndexOf(AName);
    finally
      slText.Free;
    end;
  except
    Result := -1;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : FindName
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: string;	->
              AName: string;	->
              ADelimeter: Char;	->

  Result    : Integer
  Desc      : This will find AName from the name part of AText
              It will return the index
              It will return -1 if not found
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.FindName(
  AText: string;
  AName: string;
  ADelimeter: Char): Integer;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := -1;
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      Result := slText.IndexOfName(AName);
    finally
      slText.Free;
    end;
  except
    Result := -1;
  end;

end;

{-----------------------------------------------------------------------------
  Procedure : RemoveText
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: string;	->
              AIndex: integer;	->
              ADelimeter: Char;	->

  Result    : string
  Desc      : This will remove a text in the specified index and return the new
              text
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.RemoveText(
  AText: string;
  AIndex: integer;
  ADelimeter: Char): string;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := AText;
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      slText.Delete(AIndex);
      Result := '';
      for sTemp in slText do
      begin
        Result := Result + sTemp + ADelimeter;
      end;
      if Result <> '' then
      begin
        Result := TCSRec_Trim.LeftStr(Result,ADelimeter);
      end;
    finally
      slText.Free;
    end;
  except
    Result := AText;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : RemoveName
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: string;	->
              AName: string;	->
              ADelimeter: Char;	->

  Result    : string
  Desc      : This will remove a text with the name part specified by AName and
              return the new text
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Delimeter.RemoveName(
  AText : string;
  AName: string;
  ADelimeter: Char): string;
var
  slText : TStringList;
  iDelimeterLength : integer;
  iPos : integer;
  sTemp : string;
  sParsed : string;
begin
  Result := AText;
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      iDelimeterLength := length(ADelimeter);
      sTemp := AText + ADelimeter;
      while Length(sTemp) > 0 do
      begin
        iPos := Pos(ADelimeter,sTemp);
        sParsed := copy(sTemp,0,iPos-1);
        slText.Add(sParsed);
        sTemp := Copy(sTemp,iPos+iDelimeterLength,Length(sTemp));
      end;
      slText.Delete(slText.IndexOfName(AName));
      Result := '';
      for sTemp in slText do
      begin
        Result := Result + sTemp + ';';
      end;
    finally
      slText.Free;
    end;
  except
    Result := AText;
  end;

end;

{ TCSFTrim }
{-----------------------------------------------------------------------------
  Procedure : Left
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	->
              ACount: Integer;	->

  Result    : String
  Desc      : This will remove left string for ACount character
              Eq:
                sTemp := TCSFTrim.Left('cdsData',3);
              --> Result:
                'Data'
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Trim.Left(AText: String; ACount: Integer): String;
begin
  Result := Copy(AText, ACount + 1, length(AText));
end;

{-----------------------------------------------------------------------------
  Procedure : Right
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	->
              ACount: Integer;	->

  Result    : String
  Desc      : This will remove left
              Eq:
                sTemp := TCSFTrim.Right('Data;',1);
              --> Result:
                'Data'
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Trim.Right(AText: String; ACount: Integer): String;
begin
  Result := Copy(AText, 1, Length(AText) - ACount)
end;

{-----------------------------------------------------------------------------
  Procedure : LeftStr
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: String;	->
              ASubText: String;	->

  Result    : String
  Desc      : This will remove left string for as long as the text specified
              by ASubText
              Eq:
                sTemp := TCSFTrim.LeftStr('cdsData','cds');
              --> Result:
                'Data'
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Trim.LeftStr(AText : string; ASubText: String): String;
var
  sTemp : string;
  sText : string;
begin
  sText := Trim(AText);
  Result := sText;
  sTemp := Copy(sText,1, length(ASubText));
  if CompareText(ASubText,sTemp) = 0 then
    Result := Copy(AText, Length(ASubText) + 1, length(AText) - Length(ASubText));
end;

{-----------------------------------------------------------------------------
  Procedure : RightStr
  Author    : Iwan
  Date      : 12 Apr 2016 - 12 Apr 2016
  Arguments : AText: AnsiString;	->
              ASubText: AnsiString;	->

  Result    : AnsiString
  Desc      : This will remove right string for as long as the text specified
              by ASubText
              Eq:
                sTemp := TCSFTrim.RightStr('Data;',';');
              --> Result:
                'Data'
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_Trim.RightStr(AText : string; ASubText: String): String;
var
  sTemp : String;
  sText : string;
begin
  sText := Trim(AText);
  Result := sText;
  sTemp := Copy(sText,Length(sText), Length(ASubText));
  if CompareText(ASubText,sTemp) = 0 then
    Result := Copy(sText, 1, Length(sText) - Length(ASubText));
end;

{-----------------------------------------------------------------------------
  Procedure : ParseString
  Author    : iwancs
  Date      : 27 Aug 2018 - 27 Aug 2018
  Arguments : AStringList: TStringList;	->
              AText: string;	->
              ADelimeter: char;	->
              AQuotedStr: Char;	->

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSRec_StringHelper.ParseString(
  AStringList: TStringList;
  AText: string;
  ADelimeter : char;
  AQuotedStr: Char);
var
  cTemp : Char;
  sTemp : string;
begin
  if AStringList <> nil then
  begin
    for cTemp in AText do
    begin
      if cTemp <> ADelimeter then
      begin
        sTemp := sTemp + cTemp;
      end
      else
      begin
        AStringList.Add(sTemp);
        sTemp := '';
      end;
    end;

    //If sTemp is not empty, then it store last index value but have not saved yet
    if sTemp <> '' then
    begin
      AStringList.Add(sTemp);
    end;
  end;
//  if (AStringList <> nil) then
//  try
//    AStringList.Delimiter := ADelimeter;
//    AStringList.QuoteChar := AQuotedStr;
//    AStringList.DelimitedText := AText;
//  except
//  end;
end;

{-----------------------------------------------------------------------------
  Procedure : ParseNameValue
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AStringList: TStringList;	->
              AText: string;	->
              ADelimeter: string;	->
              AQuotedStr: Char;	->

  Result    :
  Desc      :
    Parse String to name value list by detecting NAME="VALUE"
    This method is to parse name value pair seperated with space, mostly used
    in web parsing
    Example:
      id="Username" name="username_text"
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSRec_StringHelper.ParseNameValueWithSpace(
  AStringList: TStringList;
  AText: string;
  ANameValueSeperator : Char;
  AQuotedStr: Char);
var
  I: Integer;
  sTemp : string;
  sNameValue : string;
  cTemp : Char;
  bQuotedStr : Boolean;
begin
  if (AStringList <> nil) then
  try
    //Remove all space before or after ANameValueSeperator
    sTemp := StringReplace(
      AText,' ' + ANameValueSeperator + ' ',ANameValueSeperator,
      [rfReplaceAll]
    );
    sTemp := StringReplace(
      AText,' ' + ANameValueSeperator,ANameValueSeperator,
      [rfReplaceAll]
    );
    sTemp := StringReplace(
      AText,ANameValueSeperator + ' ',ANameValueSeperator,
      [rfReplaceAll]
    );

    bQuotedStr := False;
    sNameValue := '';
    for cTemp in sTemp do
    begin
      //Handle QuotedStr
      if cTemp = AQuotedStr then
      begin
        bQuotedStr := not bQuotedStr;
      end;

      if (cTemp <> ' ') or bQuotedStr then
      begin
        //Store the char if not QuatedStr and not space
        if cTemp <> AQuotedStr then
        begin
          sNameValue := sNameValue + cTemp;
        end;
      end
      else
      //If space then consider it as a name value pair
      if cTemp = ' ' then
      begin
        AStringList.Add(sNameValue);
        sNameValue := '';
      end;
    end;

    //Store the last pair
    if sNameValue <> '' then
    begin
      AStringList.Add(sNameValue);
      sNameValue := '';
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartCount
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: string;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : Integer
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartCount(
  AText: string;
  ADelimeter : Char;
  AQuotedStr: Char): Integer;
var
  slText : TStringList;
begin
  Result := -1;
  if not AText.IsEmpty then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.Count;
    finally
      slText.Free;
    end;
  except

  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartByIndex
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: String;	->
              AIndex: Integer;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartByIndex(
  AText: String;
  AIndex: Integer;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if AText <> '' then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      if AIndex < slText.Count then
        Result := slText[AIndex];
    finally
      slText.Free;
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartNameByIndex
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: String;	->
              AIndex: Integer;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartNameByIndex(
  AText: String;
  AIndex: Integer;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if not AText.IsEmpty then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.Names[AIndex];
    finally
      slText.Free;
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartValueByName
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: string;	->
              AName: string;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartValueByName(
  AText : string;
  AName : string;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if not String.IsNullOrWhiteSpace(AText) and
     not String.IsNullOrWhiteSpace(AName) then
  try
    slText := TStringList.Create;
    try
      ParseNameValueWithSpace(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.Values[AName];
    finally
      slText.Free;
    end;
  except
  end;
end;

class function TCSRec_StringHelper.PartValueByNameX(
  AText : string;
  AName: string;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if not String.IsNullOrWhiteSpace(AText) and
     not String.IsNullOrWhiteSpace(AName) then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.Values[AName];
    finally
      slText.Free;
    end;
  except
  end;
end;

class function TCSRec_StringHelper.SpaceToCamelCase(AText: string): string;
var
  sTemp : string;
  cCamelCase : Char;
begin
  sTemp := AText;
  for var i := 1 to sTemp.Length do
  begin
    if Ord(sTemp[i]) = 20 then
    begin
      sTemp[i + 1] := UpCase(sTemp[i + 1]);
    end;
  end;
  Result := StringReplace(sTemp,' ','',[rfReplaceAll]);
end;

{-----------------------------------------------------------------------------
  Procedure : PartValueByIndex
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: String;	->
              AIndex: Integer;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartValueByIndex(
  AText: String;
  AIndex: Integer;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if not AText.IsEmpty then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.ValueFromIndex[AIndex];
    finally
      slText.Free;
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartFind
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: string;	->
              AValue: string;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : Integer
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartFind(
  AText : string;
  AValue: string;
  ADelimeter : Char;
  AQuotedStr: Char): Integer;
var
  slText : TStringList;
begin
  Result := -1;
  if not String.IsNullOrWhiteSpace(AText) and
     not String.IsNullOrWhiteSpace(AValue) then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.IndexOf(AValue);
    finally
      slText.Free;
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartFindByName
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: string;	->
              AName: string;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : Integer
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartFindByName(
  AText : string;
  AName : string;
  ADelimeter : Char;
  AQuotedStr: Char): Integer;
var
  slText : TStringList;
begin
  Result := -1;
  if not String.IsNullOrWhiteSpace(AText) and
     not String.IsNullOrWhiteSpace(AName) then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      Result := slText.IndexOfName(AName);
    finally
      slText.Free;
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartRemoveByIndex
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: string;	->
              AIndex: integer;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartRemoveByIndex(
  AText: string;
  AIndex: integer;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
  sTemp : string;
begin
  Result := '';
  if not AText.IsEmpty then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);

      slText.Delete(AIndex);
      Result := slText.DelimitedText;
    finally
      slText.Free;
    end;
  except
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : PartRemoveByName
  Author    : Win7
  Date      : 04 May 2017 - 04 May 2017
  Arguments : AText: string;	->
              AName: string;	->
              ADelimeter: Char;	->
              AQuotedStr: Char;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSRec_StringHelper.PartRemoveByName(
  AText : string;
  AName: string;
  ADelimeter : Char;
  AQuotedStr: Char): string;
var
  slText : TStringList;
begin
  Result := '';
  if not String.IsNullOrWhiteSpace(AText) and
     not String.IsNullOrWhiteSpace(AName) then
  try
    slText := TStringList.Create;
    try
      ParseString(slText, AText, ADelimeter, AQuotedStr);
      slText.Delete(slText.IndexOfName(AName));
      Result := slText.DelimitedText;
    finally
      slText.Free;
    end;
  except
  end;

end;

{ TCSRec_StringHelper }

class function TCSRec_StringHelper.GetTextBetweenDelimeter(
  AText: string;
  AStartDelimeter : Char;
  AEndDelimeter: Char;
  AIndex : Integer
): string;
var
  slText : TStringList;
  cTemp : char;
  sTemp : string;
  bStart : Boolean;
begin
  Result := '';
  sTemp := '';
  bStart := False;
  slText := TStringList.Create;
  try
    for cTemp in AText do
    begin
      if (cTemp = AStartDelimeter) then
      begin
        bStart := True;
      end;

      if cTemp = AEndDelimeter then
      begin
        bStart := False;
        slText.Add(sTemp);
        sTemp := '';
      end;

      if bStart then
      begin
        if (cTemp <> AStartDelimeter) and
           (cTemp <> AEndDelimeter)  then
        begin
          sTemp := sTemp + cTemp;
        end;
      end;
    end;
    if slText.Count > 0 then
    begin
      Result := slText[AIndex];

    end;
  finally
    slText.Free;
  end;

end;

{ TCSClass_DateHelper }

{-----------------------------------------------------------------------------
  Procedure : DateTimeToStringFloat
  Author    : iwancs
  Date      : 09 Apr 2018 - 09 Apr 2018
  Arguments : AValue: TDateTime;	->

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_DateHelper.DateTimeToStringFloat(
  AValue: TDateTime): string;
//var
//  sTemp : string;
begin
  Result := FloatToStr(AValue);
//  if Pos(FormatSettings.DecimalSeparator, AValue) = 0 then
//  begin
//    sTemp := StringReplace(
//              AValue,
//              FormatSettings.ThousandSeparator,
//              FormatSettings.DecimalSeparator,
//              [rfReplaceAll]
//            );
//  end;
end;

{-----------------------------------------------------------------------------
  Procedure : StringFLoatToDateTime
  Author    : iwancs
  Date      : 09 Apr 2018 - 09 Apr 2018
  Arguments : AValue: string;	->

  Result    : TDateTime
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_DateHelper.StringFLoatToDateTime(
  AValue: string
): TDateTime;
var
  sTemp : string;
begin
  sTemp := AValue;
  if Pos(FormatSettings.DecimalSeparator, AValue) = 0 then
  begin
    sTemp := StringReplace(
              AValue,
              FormatSettings.ThousandSeparator,
              FormatSettings.DecimalSeparator,
              [rfReplaceAll]
            );
  end;
  Result := FloatToDateTime(StrToFloatDef(sTemp, 0));
end;

{-----------------------------------------------------------------------------
  Procedure : MilliSecondsToWord
  Author    : iwancs
  Date      : 10 Sep 2019
  Arguments : AValue : Integer;

  Result    : string
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_DateHelper.MilliSecondsToTimeWords(AValue: Integer): string;
var
  sHours : string;
  sMinutes : string;
  sSeconds : string;
  iSeconds : Integer;
begin
  Result := '0 Hours 0 Minutes 0 Seconds ';
  if AValue > 0 then
  begin
    iSeconds := AValue div 1000;
    sHours := ((iSeconds div 60) div 60).ToString;
    sMinutes := ((iSeconds div 60) mod 60).ToString;
    sSeconds := (iSeconds mod 60).ToString;
    Result := Format(
      '%s Hours %s Minutes %s Seconds ',
      [sHours,
       sMinutes,
       sSeconds
      ]
    )
  end;
end;


{ TCSClass_ByteHelper }

class function TCSClass_ByteHelper.BytesToStr(AValue: TBytes): string;
var
  i : Integer;
  sTemp : string;
begin
  for i := 0 to Length(AValue) - 1 do
  begin
    Result := Result + '$' + AValue[i].ToHexString + ' ';
  end;
end;

class function TCSClass_ByteHelper.StrToByte(AValue: string): Byte;
var
  iResult : Integer;
  sTemp : string;
begin
  Result := 0;
  if AValue.StartsWith('$') then
  begin
    sTemp := AValue;
  end
  else
  begin
    sTemp := '$' + AValue;
  end;
  if TryStrToInt(sTemp,iResult) then
  begin
    Result := iResult;
  end;
end;

end.

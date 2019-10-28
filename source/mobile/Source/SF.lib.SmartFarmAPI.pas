unit SF.lib.SmartFarmAPI;

interface

uses System.SysUtils, System.JSON, System.DateUtils, System.Generics.Collections,
     System.Net.Mime, System.Net.HttpClientComponent, System.Net.HttpClient,
     FireDAC.Comp.Client;

type
  TProc_RequestParams = reference to procedure (
    ARequestParams: TMultipartFormData
  );

  TCSRec_User = record
    user_email : string;
    token : string;
    expired : string;
    procedure Clear;
    function isEmpty : Boolean;
  end;

  TCSRec_Station = record
    slug : string;
    name : string;
    procedure Clear;
    function isEmpty : Boolean;
  end;

  TCSRec_Stations = TArray<TCSRec_Station>;

  TCSRec_DeviceTemperature = record
  strict private
    FValue : integer;
    FState : Boolean;
    FDate : TDateTime;
  private
    function GetDate: variant;
    function GetState: variant;
    function GetValue: variant;
    procedure SetDate(const Value: variant);
    procedure SetState(const Value: variant);
    procedure SetValue(const Value: variant);
  public
    property State : variant read GetState write SetState;
    property Date : variant read GetDate write SetDate;
    property Value : variant read GetValue write SetValue;
  end;

  TCSRec_DeviceHumidity = record
  strict Private
    FValue : integer;
    FState : Boolean;
    FDate : TDateTime;
  private
    function GetDate: variant;
    function GetState: variant;
    function GetValue: variant;
    procedure SetDate(const Value: variant);
    procedure SetState(const Value: variant);
    procedure SetValue(const Value: variant);
  public
    property State : variant read GetState write SetState;
    property Date : variant read GetDate write SetDate;
    property Value : variant read GetValue write SetValue;
  end;

  TCSRec_DeviceSprinkle = record
    FState : Boolean;
    FDate : TDateTime;
  private
    function GetDate: variant;
    function GetState: variant;
    procedure SetDate(const Value: variant);
    procedure SetState(const Value: variant);
  public
    property State : variant read GetState write SetState;
    property Date : variant read GetDate write SetDate;
  end;

  TCSRec_Devices = record
    suhu : TCSRec_DeviceTemperature;
    kelembapan : TCSRec_DeviceHumidity;
    sprinkle : TCSRec_DeviceSprinkle;
    function ToJSOn : string;
  end;

  TCSRec_Node = record
    slug : string;
    name : string;
    devtype : string;
    location : string;
    state : string;
    value : string;
    options : TCSRec_Devices;
  end;

  TCSRec_Nodes = TArray<TCSRec_Node>;

  TCSRec_History = record
    date : TDateTime;
    stationID : string;
    stationName : string;
    nodeID : string;
    nodeName : string;
    activity : string;
    description : string;
    suhu_state : string;
    suhu_value : string;
    kelembaban_state : string;
    kelembaban_value : string;
    sprinkler_state : string;
  end;
  TCSRec_Histories = TArray<TCSRec_History>;

  TCSRec_NodeSummary = record
    station : string;
    node : string;
    temperature_average : string;
    humidity_average : string;
  end;

  TCSRec_Issue = record
    date : TDateTime;
    station_name : string;
    station_slug : string;
    node_name : string;
    node_slug : string;
    activity : string;
    sType : string;
    level : Integer;
    description : string;
  end;
  TCSRec_Issues = TArray<TCSRec_Issue>;


  TCSRec_Schedule = record
    _Type : SmallInt;
    Mode : SmallInt;
    Value : string;
    Active : Boolean;
    Days : TArray<Smallint>;
    procedure Clear;
    function isEmpty : Boolean;
    function DayToString : string;
    function ActiveToString : string;
  end;

  TCSRec_NodeSchedule = record
    Slug : string;
    Name : string;
    Schedules : TArray<TCSRec_Schedule>;
  end;

  TCSRec_NodeSchedules = TArray<TCSRec_NodeSchedule>;

  TCSClass_SmartFarmAPI = class
  private
    class var FResponse : IHTTPResponse;
    class var FToken : string;
    class procedure OnRequest_Completed(
      const Sender: TObject;
      const AResponse: IHTTPResponse
    );

    class function ParseDateTime(AValue : string) : TDateTime;

    class function Execute(
      const ACommand : string;
      AParamProc : TProc_RequestParams = nil;
      ADataPath : string = 'data'
    ) : TJSONValue;

    class procedure CheckResponseOk(JSON: TJSONValue);
    class function parseUser(AData : TJSONValue) : TCSRec_User;
    class function parseStation(AData : TJSONValue) : TCSRec_Station;
    class function parseStation_blank(AData : TJSONValue) : TCSRec_Station;
    class function parseNode(AData : TJSONValue) : TCSRec_Node;
    class function parseNodeSummary(AData : TJSONValue) : TCSRec_NodeSummary;
    class function parseHistory(AData : TJSONValue) : TCSRec_History;
    class function parseIssue(AData : TJSONValue) : TCSRec_Issue;
    class function parseSchedule(AData : TJSONValue) : TCSRec_NodeSchedules;

  public
    class function Login(
      AUserName : string;
      APassword : string
    ) : TCSRec_User;
    class function GetStationList(AStationID : string = '') : TCSRec_Stations;
    class function GetStationDetail(AStationID : string) : TCSRec_Station;
    class function GetNodeList(
      AStationID : string;
      ANodeID : string = ''
    ) : TCSRec_Nodes;
    class function GetNodeSummery(
      AStationID : string;
      ANodeID : string = ''
    ): TCSRec_NodeSummary;
    class function GetNodeHistory(
      AStationID : string;
      ANodeID : string
    ) : TCSRec_Histories;
    class function GetNodeIssue(
      AStationID : string;
      ANodeID : string = ''
    ) : TCSRec_Issues;
    class function GetSchedule(
      AStationID : string;
      ANodeID : string = ''
    ) : TCSRec_NodeSchedules;
    class function SetSchedule(
      AStationID : string;
      ANodeID : string;
      const AScheduleDataset : TFDMemTable
    ) : Boolean;
    class function SetStationSprinkle(
      AStationID : string;
      ANodeID : string;
      AState : Boolean
    ) : Boolean;
  end;

implementation

uses System.StrUtils, System.Variants, System.JSON.Builders,
     System.Net.URLClient,
     FMX.Dialogs, FMX.Forms,
     iSolution.lib.Logger, iSolution.lib.StringHelper;

class procedure TCSClass_SmartFarmAPI.OnRequest_Completed(
  const Sender: TObject;
  const AResponse: IHTTPResponse
);
begin
  FResponse := AResponse;
end;

class function TCSClass_SmartFarmAPI.ParseDateTime(AValue: string): TDateTime;
begin
//  2019-10-21 11:07:09
  Result := EncodeDateTime(
    LeftStr(AValue,4).ToInteger,
    MidStr(AValue,6,2).ToInteger,
    MidStr(AValue,9,2).ToInteger,
    MidStr(AValue,12,2).ToInteger,
    MidStr(AValue,15,2).ToInteger,
    MidStr(AValue,18,2).ToInteger,
    0
  );
end;

{-----------------------------------------------------------------------------
  Procedure : Execute
  Author    : iwancs
  Date      : 21 Oct 2019
  Arguments : ACommand : stringAParamProc : TProc_RequestParamsADataPath : string;

  Result    : TJSONValue
  Desc      :
  Version   :
  Reference : https://flixengineering.com/archives/200
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_SmartFarmAPI.Execute(
  const ACommand: string;
  AParamProc: TProc_RequestParams;
  ADataPath : string
): TJSONValue;
const
  StrBaseUrl = 'https://smartfarm.pascal-id.org/%s';
var
  Client: TNetHTTPClient;
  Request: TNetHTTPRequest;
  Response: IHTTPResponse;
  Header : TNetHeaders;
  jsvResult : TJSONValue;
  mpRequestParams : TMultipartFormData;
begin
  Result := nil;
  FResponse := nil;
  Client := TNetHTTPClient.Create(nil);
  Request := TNetHTTPRequest.Create(nil);
  mpRequestParams := nil;
  try
    Request.OnRequestCompleted :=
      TCSClass_SmartFarmAPI.OnRequest_Completed;
    Client.HandleRedirects:= True;
    Request.Client := Client;
    if FToken <> '' then
    begin
      Request.CustomHeaders['Token'] := FToken;
    end;
//    Request.Accept := 'application/json';
    Request.AcceptCharset := 'UTF-8, *;q=0.8';
    if Assigned(AParamProc) then
    begin
      mpRequestParams := TMultipartFormData.Create;
      AParamProc(mpRequestParams);
    end;
    Request.Asynchronous := True;
    try
      if mpRequestParams <> nil then
      begin
        Response := Request.Post(
          Format(StrBaseUrl,[ACommand]) ,
          mpRequestParams
        );
      end
      else
      begin
        Response := Request.Get(
          Format(StrBaseUrl,[ACommand])
        );
      end;
    except
      raise;
    end;

    while FResponse = nil do
    begin
      Application.ProcessMessages;
    end;

    if (FResponse.ContentAsString <> '') then
    begin
      jsvResult := TJSONObject.ParseJSONValue(FResponse.ContentAsString);
      CheckResponseOk(jsvResult);
      jsvResult.TryGetValue<TJSONValue>(ADataPath,Result);
    end;
  finally
    mpRequestParams.Free;
    Request.Free;
    Client.Free;
  end;
end;

class procedure TCSClass_SmartFarmAPI.CheckResponseOk(JSON: TJSONValue);
var
  iErrorCode : Integer;
  sErrorDescription : string;
begin
  try
    if  not (JSON.GetValue<Integer>('code') in [0, 200]) then
    begin
      iErrorCode := JSON.GetValue<Integer>('code');
      sErrorDescription := Format(
        'Unknown Response Error : %s',
        [JSON.GetValue<string>('msg') ]
      );
//      JSON.TryGetValue<Integer>('error_code',iErrorCode);
//      JSON.TryGetValue<string>('description',sErrorDescription);
      raise Exception.CreateFmt(
        'Response Error Code : %d - %s',
        [iErrorCode, sErrorDescription]
      );
    end;
  except
    TCSClass_Logger.LogMessage(
      'Failed To CheckResponseOK : %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseUser(AData: TJSONValue): TCSRec_User;
begin
  try
    Result.user_email := AData.GetValue<string>('user_email');
    Result.token := AData.GetValue<string>('token');
    Result.expired := AData.GetValue<string>('expired');
  except
    TCSClass_Logger.LogMessage(
      'Failed To parseUser: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;


end;

class function TCSClass_SmartFarmAPI.parseStation(AData: TJSONValue): TCSRec_Station;
var
  Data: TJSONArray;
begin
  try
    Data := AData as TJSONArray;
    Result.Clear;
    Result.slug := Data.Items[0].GetValue<string>('slug');
    Result.name := Data.Items[0].GetValue<string>('name');
  except
    TCSClass_Logger.LogMessage(
      'Failed To ParseStation: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseStation_blank(
  AData: TJSONValue): TCSRec_Station;
begin
  try
    Result.slug := AData.A[0].Value;
    Result.name := AData.A[1].Value;
  except
    TCSClass_Logger.LogMessage(
      'Failed To ParseStation: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseNode(AData: TJSONValue): TCSRec_Node;
var
  sOptions : string;
  jsvDevices : TJSONValue;
begin
  try
    Result.slug := AData.GetValue<string>('slug');
    Result.name := AData.GetValue<string>('name');
    Result.devtype := AData.GetValue<string>('type');
    Result.location := AData.GetValue<string>('location');
    Result.state := AData.GetValue<string>('state');
    Result.value := AData.GetValue<string>('value');
    if not AData.TryGetValue<string>('options',sOptions) then
    begin
      jsvDevices := AData.GetValue<TJSONValue>('options');
      jsvDevices := jsvDevices.GetValue<TJSONValue>('devices');
      if jsvDevices <> nil then
      begin
        Result.options.suhu.Value :=
          jsvDevices.GetValue<TJSONValue>('suhu').GetValue<Integer>('value');
        Result.options.suhu.state :=
          jsvDevices.GetValue<TJSONValue>('suhu').GetValue<Integer>('state');
        Result.options.suhu.date :=
          jsvDevices.GetValue<TJSONValue>('suhu').GetValue<string>('date');

        Result.options.kelembapan.value :=
          jsvDevices.GetValue<TJSONValue>('kelembapan').GetValue<Integer>('value');
        Result.options.kelembapan.state :=
          jsvDevices.GetValue<TJSONValue>('kelembapan').GetValue<Integer>('state');
        Result.options.kelembapan.date :=
          jsvDevices.GetValue<TJSONValue>('kelembapan').GetValue<string>('date');

        Result.options.sprinkle.state :=
          jsvDevices.GetValue<TJSONValue>('sprinkle').GetValue<Integer>('state');
        Result.options.sprinkle.date :=
          jsvDevices.GetValue<TJSONValue>('sprinkle').GetValue<string>('date');
      end;
    end;
  except
    TCSClass_Logger.LogMessage(
      'Failed To ParseDevice: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseNodeSummary(
  AData: TJSONValue
): TCSRec_NodeSummary;
begin
  try
    Result.temperature_average := AData.GetValue<string>('temperature_average');
    Result.humidity_average := AData.GetValue<string>('humidity_average');
  except
    TCSClass_Logger.LogMessage(
      'Failed To parseNodeSummary: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseHistory(
  AData: TJSONValue
): TCSRec_History;
var
  jsvOptions : TJSONValue;
begin
  try
    Result.date := ParseDateTime(AData.GetValue<string>('date'));
    Result.stationID := AData.GetValue<string>('station_slug');
    Result.stationName := AData.GetValue<string>('station_name');
    Result.nodeID := AData.GetValue<string>('slug');
    Result.nodeName := AData.GetValue<string>('name');
    Result.activity := AData.GetValue<string>('activity');
    Result.description := AData.GetValue<string>('description');

    jsvOptions := AData.GetValue<TJSONValue>('options');
    Result.suhu_state := jsvOptions.GetValue<TJSONValue>('suhu').GetValue<string>('state');
    Result.suhu_value := jsvOptions.GetValue<TJSONValue>('suhu').GetValue<string>('value');
    Result.kelembaban_state := jsvOptions.GetValue<TJSONValue>('kelembaban').GetValue<string>('state');
    Result.kelembaban_value := jsvOptions.GetValue<TJSONValue>('kelembaban').GetValue<string>('value');
    Result.sprinkler_state := jsvOptions.GetValue<TJSONValue>('sprinkler').GetValue<string>('state');
  except
    TCSClass_Logger.LogMessage(
      'Failed To parseHistory: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseIssue(
  AData: TJSONValue
): TCSRec_Issue;
begin
  try
    Result.date := ParseDateTime(AData.GetValue<string>('date'));
    Result.station_name := AData.GetValue<string>('station_name');
    Result.station_slug := AData.GetValue<string>('station_slug');
    Result.node_name := AData.GetValue<string>('node_name');
    Result.node_slug := AData.GetValue<string>('node_slug');
    Result.activity := AData.GetValue<string>('activity');
    Result.sType := AData.GetValue<string>('type');
    Result.level := AData.GetValue<string>('level').ToInteger;
    Result.description := AData.GetValue<string>('description');
  except
    TCSClass_Logger.LogMessage(
      'Failed To parseIssue: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

class function TCSClass_SmartFarmAPI.parseSchedule(
  AData: TJSONValue
): TCSRec_NodeSchedules;
var
  Data: TJSONArray;
  jsvSchedules : TJSONValue;
  sSchedule : string;
  jsaSchedule : TJSONArray;
  jsaDays : TJSONArray;
  sActive : string;
begin
  try
    Data := AData as TJSONArray;
    SetLength(Result, Data.Count);
    for var i := 0 to Data.Count - 1 do
    begin
      Result[i].Slug := Data.Items[i].GetValue<string>('slug');
      Result[i].Name := Data.Items[i].GetValue<string>('name');

      jsvSchedules := Data.Items[i].GetValue<TJSONValue>('schedules');
      if jsvSchedules.ClassNameIs(TJSONArray.ClassName) then
      begin
        jsaSchedule := TJSONArray(jsvSchedules);
      end;
      if jsaSchedule <> nil then
      begin
        SetLength(Result[i].Schedules, jsaSchedule.Count);
        for var j := 0 to jsaSchedule.Count - 1 do
        begin
          jsaSchedule.Items[j].TryGetValue<SmallInt>('type',Result[i].Schedules[j]._Type);
          jsaSchedule.Items[j].TryGetValue<SmallInt>('mode',Result[i].Schedules[j].Mode);
          jsaSchedule.Items[j].TryGetValue<string>('value',Result[i].Schedules[j].Value);
          jsaSchedule.Items[j].TryGetValue<string>('active',sActive);

          Result[i].Schedules[j].Active := sActive = '0';

          if jsaSchedule.Items[j].TryGetValue<TJSONArray>('days',jsaDays) then
          begin
            SetLength(Result[i].Schedules[j].Days, jsaDays.Count);
            for var k := 0 to jsaDays.Count - 1 do
            begin
              Result[i].Schedules[j].Days[k] := jsaDays.Items[k].AsType<SmallInt>;
            end;
          end;
        end;
      end;
    end;
  except
    TCSClass_Logger.LogMessage(
      'Failed To ParseStation: %s (%s)',
      [Exception(ExceptObject).ToString,
       Exception(ExceptObject).ClassName
      ]
    );
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : Login
  Author    : iwancs
  Date      : 23 Oct 2019
  Arguments : AUserName : stringAPassword : string;

  Result    : TCSRec_User
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_SmartFarmAPI.Login(AUserName,
  APassword: string): TCSRec_User;
var
  JSON: TJSONValue;
  I: Integer;

begin
  JSON := Execute(
            'auth/',
            procedure(ARequestParams: TMultipartFormData)
            begin
              ARequestParams.AddField('username',AUserName);
              ARequestParams.AddField('password',APassword);
            end,
            ''
          );
  if JSON <> nil then
  begin
    try
      Result.Clear;
      Result := parseUser(JSON);
      FToken := Result.token;
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseChatMembers: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;

end;

{ TCSClass_SmartFarmAPI }

class function TCSClass_SmartFarmAPI.GetStationList(
  AStationID : string
): TCSRec_Stations;
var
  JSON: TJSONValue;
  Data: TJSONArray;
  I: Integer;

begin
  if AStationID = '' then
  begin
    JSON := Execute('station/');
  end
  else
  begin
    JSON := Execute('station/?id=' + AStationID);
  end;
  if JSON <> nil then
  begin
    try
      Data := JSON as TJSONArray;
      SetLength(Result, Data.Count);
      for I := 0 to Data.Count - 1 do
      begin
        if AStationID = '' then
        begin
          Result[i] := parseStation_blank(Data.Items[I]);
        end
        else
        begin
          Result[i] := parseStation(Data.Items[I]);
        end;
      end;
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseChatMembers: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;
end;

class function TCSClass_SmartFarmAPI.GetStationDetail(
  AStationID: string
): TCSRec_Station;
var
  JSON: TJSONValue;
  I: Integer;

begin
  JSON := Execute('station/?id=' + AStationID);
  if JSON <> nil then
  begin
    try
      Result := parseStation(JSON);
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseChatMembers: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;
end;

class function TCSClass_SmartFarmAPI.GetNodeList(
  AStationID: string;
  ANodeID : string = ''
): TCSRec_Nodes;
var
  JSON: TJSONValue;
  Data: TJSONArray;
  I: Integer;
begin
//  JSON := Execute('node/?stationid=' + AStationID);
  JSON := Execute(Format(
      'node/?stationid=%s&id=%s',
      [AStationID, ANodeID]
    )
  );
  if JSON <> nil then
  begin
    try
      Data := JSON as TJSONArray;
      SetLength(Result, Data.Count);
      for I := 0 to Data.Count - 1 do
      begin
        Result[i] := parseNode(Data.Items[I]);
      end;
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseChatMembers: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;
end;

class function TCSClass_SmartFarmAPI.GetNodeSummery(
  AStationID : string;
  ANodeID: string
): TCSRec_NodeSummary;
var
  JSON: TJSONValue;
  I: Integer;
begin
  JSON := Execute(Format('node/history/?id=%s&limit=20',[ANodeID]),nil,'info');
  if JSON <> nil then
  begin
    try
      Result := parseNodeSummary(JSON);
    except
      TCSClass_Logger.LogMessage(
        'Failed To GetNodeSummery: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;

end;

class function TCSClass_SmartFarmAPI.GetNodeHistory(
  AStationID : string;
  ANodeID: string
): TCSRec_Histories;
var
  JSON: TJSONValue;
  Data: TJSONArray;
  I: Integer;
begin
  JSON := Execute(Format('node/history/?id=%s',[ANodeID]));
//  JSON := Execute('node/history/?id=' + AStationID + '&nodeid=' + ANodeID);
  if JSON <> nil then
  begin
    try
      Data := JSON as TJSONArray;
      SetLength(Result, Data.Count);
      for I := 0 to Data.Count - 1 do
      begin
        Result[i] := parseHistory(Data.Items[I]);
      end;
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseHistory: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;
end;

class function TCSClass_SmartFarmAPI.GetNodeIssue(
  AStationID : string;
  ANodeID: string
): TCSRec_Issues;
var
  JSON: TJSONValue;
  Data: TJSONArray;
  iDataCount : Integer;
begin
  JSON := Execute(Format('issue/?stationId=%s&nodeId=%s',[AStationID,ANodeID]));
//  JSON := Execute('issue/?stationId=' + AStationID +
//                  '&nodeId=' + ANodeID);
  if JSON <> nil then
  begin
    try
      Data := JSON as TJSONArray;
      iDataCount := Data.Count;
      if iDataCount > 12 then
      begin
        iDataCount := 12;
      end;
      SetLength(Result, iDataCount);
      for var I := 0 to iDataCount - 1 do
      begin
        Result[i] := parseIssue(Data.Items[I]);
      end;
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseIssue: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : GetSchedule
  Author    : iwancs
  Date      : 21 Oct 2019
  Arguments : AStationID : stringANodeID : string;

  Result    : TCSRec_Schedule
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class function TCSClass_SmartFarmAPI.GetSchedule(
  AStationID : string;
  ANodeID: string
): TCSRec_NodeSchedules;
var
  JSON: TJSONValue;
begin
  JSON := Execute(
    Format(
      'schedule/?stationId=%s&nodeId=%s',
      [AStationID, ANodeID]
    )
  );
  if JSON <> nil then
  begin
    try
      Result := parseSchedule(JSON);
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseSchedule: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;

end;

class function TCSClass_SmartFarmAPI.SetSchedule(
  AStationID : string;
  ANodeID: string;
  const AScheduleDataset : TFDMemTable
): Boolean;
var
  JSON: TJSONValue;
  jsaSchedules : TJSONArray;
  jsoSchedule : TJSONObject;
  jsaDays : TJSONArray;
  sSchedule : string;
  iCount : Integer;
  sDays : string;
begin
  jsaSchedules := TJSONArray.Create;
  try
//    if (ANodeID = '') or
//       (ANodeID = '0')  then
//    begin
//      AScheduleDataset.Filter := Format(
//        'STATION_ID=%s',
//        [QuotedStr(AStationID)
//        ]
//      );
//    end
//    else
//    begin
//      AScheduleDataset.Filter := Format(
//        'STATION_ID=%s AND NODE_ID=%s',
//        [QuotedStr(AStationID),
//         QuotedStr(ANodeID)
//        ]
//      );
//    end;
//    AScheduleDataset.Filtered := True;

    AScheduleDataset.First;
    while not AScheduleDataset.Eof do
    begin
      jsoSchedule := TJSONObject.Create;
      jsoSchedule.AddPair(
        'type',
        AScheduleDataset.FieldByName('TYPE').AsString
      );
      jsoSchedule.AddPair(
        'mode',
        AScheduleDataset.FieldByName('MODE').AsString
      );
      jsoSchedule.AddPair(
        'value',
        AScheduleDataset.FieldByName('VALUE').AsString
      );
      jsoSchedule.AddPair(
        'active',
        AScheduleDataset.FieldByName('ACTIVE').AsString
      );
      iCount := TCSRec_StringHelper.PartCount(
        AScheduleDataset.FieldByName('DAYS').AsString,
        ','
      );
      if iCount > 0 then
      begin
        jsaDays := TJSONArray.Create;
        for var i := 0 to iCount - 1 do
        begin
          jsaDays.Add(
            TCSRec_StringHelper.PartByIndex(
              AScheduleDataset.FieldByName('DAYS').AsString,
              i,
              ','
            )
          );
        end;
        jsoSchedule.AddPair('days',jsaDays);
      end;

      jsaSchedules.Add(jsoSchedule);

      AScheduleDataset.Next;
    end;

    sSchedule := jsaSchedules.ToJSON;
  finally
    jsaSchedules.Free;
  end;

  JSON := Execute(
            'schedule/',
            procedure(ARequestParams: TMultipartFormData)
            begin
              ARequestParams.AddField('stationId',AStationID);
              ARequestParams.AddField('nodeId',ANodeID);
              ARequestParams.AddField('schedule',sSchedule);
            end,
            ''
          );
  if JSON <> nil then
  begin
    try
      Result := True;
    except
      TCSClass_Logger.LogMessage(
        'Failed To ParseChatMembers: %s (%s)',
        [Exception(ExceptObject).ToString,
         Exception(ExceptObject).ClassName
        ]
      );
    end;
  end;
end;

class function TCSClass_SmartFarmAPI.SetStationSprinkle(
  AStationID : string;
  ANodeID: string;
  AState: Boolean
): Boolean;
var
  JSON: TJSONValue;
  recNodes : TCSRec_Nodes;
begin
  recNodes := GetNodeList(
    AStationID,
    ANodeID
  );

  for var i := 0 to Length(recNodes) - 1 do
  begin
    recNodes[i].options.sprinkle.state := AState;

    JSON := Execute(
              'node/',
              procedure(ARequestParams: TMultipartFormData)
              begin
                ARequestParams.AddField('id',recNodes[i].slug);
                ARequestParams.AddField('state',recNodes[i].state);
                ARequestParams.AddField('value',recNodes[i].value);
                ARequestParams.AddField(
                  'options',
                  recNodes[i].options.ToJSOn
                );
              end
            );
    if JSON <> nil then
    begin
      try
        Result := True;
      except
        TCSClass_Logger.LogMessage(
          'Failed To ParseChatMembers: %s (%s)',
          [Exception(ExceptObject).ToString,
           Exception(ExceptObject).ClassName
          ]
        );
      end;
    end;
  end;
end;

{ TCSRec_User }

procedure TCSRec_Station.Clear;
begin
  slug := '';
  name := '';
end;

function TCSRec_Station.isEmpty: Boolean;
begin
  Result := slug = '';
end;

{ TCSRec_User }

procedure TCSRec_User.Clear;
begin
  user_email := '';
  token := '';
  expired := '';
end;

function TCSRec_User.isEmpty: Boolean;
begin
  Result := token = '';
end;

{ TCSRec_Schedule }

procedure TCSRec_Schedule.Clear;
begin
  _Type := 0;
  Mode := 0;
  Value := Null;
  Active := False;
  Days := [];
end;

function TCSRec_Schedule.DayToString: string;
begin
  Result := '';
  for var j := 0 to Length(Days) - 1 do
  begin
    Result := Result + Days[j].ToString + ',';
  end;
end;

function TCSRec_Schedule.ActiveToString: string;
begin
  Result := '1';
  if Active then
  begin
    Result := '0';
  end;
end;

function TCSRec_Schedule.isEmpty: Boolean;
begin

end;

{ TCSRec_Devices }

function TCSRec_Devices.ToJSOn: string;
var
  jsoDevices : TJSONObject;
  jsoSuhu : TJSONObject;
  jsoKelembapan : TJSONObject;
  jsoSprinkle : TJSONObject;
begin
  jsoDevices := TJSONObject.Create;
  jsoSuhu := TJSONObject.Create;
  jsoKelembapan := TJSONObject.Create;
  jsoSprinkle := TJSONObject.Create;

  jsoSuhu.AddPair('value',suhu.Value);
  jsoSuhu.AddPair('state',suhu.State);
  jsoDevices.AddPair(
    'suhu',
    jsoSuhu
  );

  jsoKelembapan.AddPair('value',kelembapan.value);
  jsoKelembapan.AddPair('state',kelembapan.state);
  jsoDevices.AddPair(
    'kelembapan',
    jsoKelembapan
  );

  jsoSprinkle.AddPair('state',sprinkle.state);
  jsoDevices.AddPair(
    'sprinkle',
    jsoSprinkle
  );

  Result := TJSONObject.Create.AddPair(
    'devices',
    jsoDevices
  ).ToJSON;
end;

{ TCSRec_DeviceTemperature }

function TCSRec_DeviceTemperature.GetDate: variant;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:nn:ss',FDate);
end;

function TCSRec_DeviceTemperature.GetState: variant;
begin
  Result := 1;
  if FState then
  begin
    Result := 0;
  end;
end;

function TCSRec_DeviceTemperature.GetValue: variant;
begin
  Result := FValue;
end;

procedure TCSRec_DeviceTemperature.SetDate(const Value: variant);
begin
  FDate := TCSClass_SmartFarmAPI.ParseDateTime(Value);
end;

procedure TCSRec_DeviceTemperature.SetState(const Value: variant);
begin
  if VarIsType(Value, varBoolean) then
  begin
    FState := Value;
  end
  else
  begin
    FState := Value = 0;
  end;
end;

procedure TCSRec_DeviceTemperature.SetValue(const Value: variant);
begin
  FValue := Value;
end;

{ TCSRec_DeviceHumidity }

function TCSRec_DeviceHumidity.GetDate: variant;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:nn:ss',FDate);
end;

function TCSRec_DeviceHumidity.GetState: variant;
begin
  Result := 1;
  if FState then
  begin
    Result := 0;
  end;
end;

function TCSRec_DeviceHumidity.GetValue: variant;
begin
  Result := FValue;
end;

procedure TCSRec_DeviceHumidity.SetDate(const Value: variant);
begin
  FDate := TCSClass_SmartFarmAPI.ParseDateTime(Value);
end;

procedure TCSRec_DeviceHumidity.SetState(const Value: variant);
begin
  if VarIsType(Value, varBoolean) then
  begin
    FState := Value;
  end
  else
  begin
    FState := Value = 0;
  end;
end;

procedure TCSRec_DeviceHumidity.SetValue(const Value: variant);
begin
  FValue := Value;
end;

{ TCSRec_DeviceSprinkle }

function TCSRec_DeviceSprinkle.GetDate: variant;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:nn:ss',FDate);
end;

function TCSRec_DeviceSprinkle.GetState: variant;
begin
  Result := 1;
  if FState then
  begin
    Result := 0;
  end;
end;

procedure TCSRec_DeviceSprinkle.SetDate(const Value: variant);
begin
  FDate := TCSClass_SmartFarmAPI.ParseDateTime(Value);
end;

procedure TCSRec_DeviceSprinkle.SetState(const Value: variant);
begin
  if VarIsType(Value, varBoolean) then
  begin
    FState := Value;
  end
  else
  begin
    FState := Value = 0;
  end;
end;

{ TCSRec_Days }

end.

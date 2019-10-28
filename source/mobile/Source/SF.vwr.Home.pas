unit SF.vwr.Home;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Gestures,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Edit, FMX.ComboEdit, FMXTee.Engine, FMXTee.Series, FMXTee.Procs,
  FMXTee.Chart, FMX.Objects, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid,
  SF.bsx.SmartFarmMain, FMX.ListBox,
  SF.lib.SmartFarmAPI, FMX.Effects;

type
  TfrmHome = class(TfrmBaseMain)
    lytStation: TLayout;
    chtHumidity: TChart;
    lnsrsHumidity: TLineSeries;
    lblHumidityTitle: TLabel;
    lytStatus: TLayout;
    lytTemperature: TLayout;
    lytActivity: TLayout;
    lblTemperatureCaption: TLabel;
    lblActivity: TLabel;
    lblTemperatureValue: TLabel;
    imgTemperature: TImage;
    lvSprinkleActivity: TListView;
    lytIssue: TLayout;
    lblIssueCaption: TLabel;
    lytGraph: TLayout;
    lytHistory: TLayout;
    lblHistoryCaption: TLabel;
    cbbStationList: TComboBox;
    lytHumidityValue: TLayout;
    lblHumidityValue: TLabel;
    strngrdHistory: TStringGrid;
    andctrLoading: TAniIndicator;
    lblIssueEmptyData: TLabel;
    lblHistoryEmptyData: TLabel;
    shdwfctWidgetTemperature: TShadowEffect;
    rctnglWidgetTemperature: TRectangle;
    rctnglWidgetActivity: TRectangle;
    rctnglWidgetIssue: TRectangle;
    rctnglWidgetHistory: TRectangle;
    rctnglWidgetGraph: TRectangle;
    strngrdIssue: TStringGrid;
    shdwfctWidgetActivity: TShadowEffect;
    shdwfctWidgetIssue: TShadowEffect;
    shdwfctWidgetHistory: TShadowEffect;
    shdwfctWidgetGraph: TShadowEffect;
    lytTemperatureImage: TLayout;
    procedure cbbStationListChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    FStationList : TCSRec_Stations;
    FRowHeight : Integer;

    procedure Event_FullyLoaded(Sender : TObject);

    procedure ClearGrid(AGrid : TStringGrid);
    procedure CreateIssueColumns;
    procedure CreateHistoryColumns;
    procedure ResizeStatus;
    procedure ResizeIssueColumns;
    procedure ResizeHistoryColumns;

    procedure LoadWidget(
      AStationID : string;
      ANodeID : string = ''
    );
    procedure LoadIssue(
      AStationID : string;
      ANodeID : string = ''
    );
    procedure LoadHistory(
      AStationID : string;
      ANodeID : string = ''
    );
    function MinWidth(ACurrentValue, AMinValue: Double): Double;
  public
    { Public declarations }
  end;

  TColumnAccess = class( TColumn )
  end;

var
  frmHome: TfrmHome;

implementation

uses System.DateUtils,
     SF.lib.SmartFarmHelpoer;

{$R *.fmx}

{-----------------------------------------------------------------------------
  Procedure : cbbStationListChange
  Author    : iwancs
  Date      : 21 Oct 2019
  Arguments : Sender : TObject;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmHome.cbbStationListChange(Sender: TObject);
var
  sStationID : string;
begin
  sStationID := '';
  if cbbStationList.ItemIndex > 0 then
  begin
    sStationID := FStationList[cbbStationList.ItemIndex - 1].slug;
  end;
  andctrLoading.Enabled := True;
  andctrLoading.Visible := True;
  try
    LoadWidget(sStationID);
    LoadIssue(sStationID);
    LoadHistory(sStationID);
  finally
    andctrLoading.Visible := False;
    andctrLoading.Enabled := False;
  end;
end;

procedure TfrmHome.FormCreate(Sender: TObject);
begin
  inherited;
  OnFullyLoaded := Event_FullyLoaded;
  FRowHeight := 24;
end;

procedure TfrmHome.LoadWidget(
  AStationID : string;
  ANodeID : string = ''
);
var
  recNodeSummary : TCSRec_NodeSummary;
  recNodes : TCSRec_Nodes;
  itmRecord : TListViewItem;
  dtCurrent : TDateTime;
  dtNew : TDateTime;
begin
  recNodeSummary := TCSClass_SmartFarmAPI.GetNodeSummery(
    AStationID,
    ANodeID
  );
  lblTemperatureValue.Text := FormatFloat(
    '#,##0',
    StrToFloatDef(
      StringReplace(
        recNodeSummary.temperature_average,
        '.',
        FormatSettings.DecimalSeparator,
        []
      ),
      0
    )
  ) + 'º';
  lblHumidityValue.Text := FormatFloat(
    '#,##0.0',
    StrToFloatDef(
      StringReplace(
        recNodeSummary.humidity_average,
        '.',
        FormatSettings.DecimalSeparator,
        []
      ),
      0
    )
  );

  lvSprinkleActivity.Items.Clear;

  recNodes := TCSClass_SmartFarmAPI.GetNodeList(
    AStationID
  );

//  try
//    if Length(recNodes) > 0 then
//    begin
//      dtCurrent := 0;
//      for var i := 0 to Length(recNodes) - 1 do
//      begin
//        dtNew := VarToDateTime(recNodes[i].options.sprinkle.Date);
//        if CompareDateTime(dtCurrent,dtNew) < 0 then
//        begin
//          dtCurrent := dtNew;
//        end;
//      end;
//    end;
//  except
//
//  end;

  itmRecord := lvSprinkleActivity.Items.Add;
  itmRecord.Text:= 'Last Activity';
  dtCurrent := Now;
  if dtCurrent = 0 then
  begin
    itmRecord.Detail:= 'NA'
  end
  else
  begin
    itmRecord.Detail:= FormatDateTime(
      'dd MMMM yyy HH:nn',
      dtCurrent
    );
  end;


  itmRecord := lvSprinkleActivity.Items.Add;
  itmRecord.Text:= 'Next Activity';
  itmRecord.Detail:= FormatDateTime(
    'dd MMMM yyy HH:nn',
    IncHour(Now,1)
  );

end;

{-----------------------------------------------------------------------------
  Procedure : CreateHistoryColumns
  Author    : iwancs
  Date      : 20 Oct 2019
  Arguments : ;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmHome.ClearGrid(AGrid: TStringGrid);
var
  iCol : Integer;
  iRow : Integer;
begin
  for iRow := 0 to AGrid.RowCount - 1 do
  begin
    for iCol := 0 to AGrid.ColumnCount - 1 do
    begin
      AGrid.Cells[iCol, iRow] := '';
    end;
  end
end;

procedure TfrmHome.CreateHistoryColumns;
var
  colItem : TStringColumn;
  dblTotalColWidth : Double;
  dblColWidth : Double;

begin
  dblColWidth := ClientWidth / 5;

  dblTotalColWidth := 0;
  colItem := TStringColumn.Create(strngrdHistory);
  colItem.Header := 'Time';
  colItem.Parent := strngrdHistory;
  strngrdHistory.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 30);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdHistory);
  colItem.Header := 'Station';
  colItem.Parent := strngrdHistory;
  strngrdHistory.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 50);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdHistory);
  colItem.Header := 'Node';
  colItem.Parent := strngrdHistory;
  strngrdHistory.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 50);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdHistory);
  colItem.Header := 'Activity';
  colItem.Parent := strngrdHistory;
  strngrdHistory.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 70);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdHistory);
  colItem.Header := 'Description';
  colItem.Parent := strngrdHistory;
  strngrdHistory.AddObject(colItem);
  colItem.Width := strngrdHistory.Width - dblTotalColWidth;
end;

{-----------------------------------------------------------------------------
  Procedure : CreateIssueColumns
  Author    : iwancs
  Date      : 20 Oct 2019
  Arguments : ;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmHome.CreateIssueColumns;
var
  colItem : TStringColumn;
  CellCtrl: TStyledControl;
  dblTotalColWidth : Double;
  dblColWidth : Double;
begin
  dblTotalColWidth := 0;
  dblColWidth := ClientWidth / 4;

  colItem := TStringColumn.Create(strngrdIssue);
  colItem.Header := 'Station';
  colItem.Parent := strngrdIssue;
  strngrdIssue.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 50);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdIssue);
  colItem.Header := 'Node';
  colItem.Parent := strngrdIssue;
  strngrdIssue.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 50);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdIssue);
  colItem.Header := 'Activity';
  colItem.Parent := strngrdIssue;
  strngrdIssue.AddObject(colItem);
  colItem.Width := MinWidth(dblColWidth, 70);
  dblTotalColWidth := dblTotalColWidth + colItem.Width;

  colItem := TStringColumn.Create(strngrdIssue);
  colItem.Header := 'Description';
  colItem.Parent := strngrdIssue;
  strngrdIssue.AddObject(colItem);
  colItem.Width := strngrdIssue.Width - dblTotalColWidth;
end;

procedure TfrmHome.Event_FullyLoaded(Sender: TObject);
begin
  strngrdHistory.RowHeight := strngrdHistory.Canvas.TextHeight('A') *
                              Canvas.Scale;
  strngrdIssue.RowHeight := strngrdIssue.Canvas.TextHeight('A') *
                              Canvas.Scale;
  CreateIssueColumns;
  CreateHistoryColumns;
  FStationList := TCSClass_SmartFarmHelper.FillStationList(cbbStationList);
  tbcDevices.TabIndex := 0;
  ResizeStatus;
end;

function TfrmHome.MinWidth(
  ACurrentValue : Double;
  AMinValue : Double
) : Double;
begin
  Result := ACurrentValue;
  if ACurrentValue < AMinValue then
  begin
    Result := AMinValue;
  end;

end;

procedure TfrmHome.ResizeHistoryColumns;
var
  dblTotalColWidth : Double;
  dblColWidth : Double;
begin
  if strngrdHistory.ColumnCount > 0 then
  begin
    dblColWidth := ClientWidth / 5;

    strngrdHistory.Columns[0].Width := MinWidth(dblColWidth, 30);
    dblTotalColWidth := dblTotalColWidth + strngrdHistory.Columns[0].Width;

    strngrdHistory.Columns[1].Width := MinWidth(dblColWidth, 50);
    dblTotalColWidth := dblTotalColWidth + strngrdHistory.Columns[1].Width;

    strngrdHistory.Columns[2].Width := MinWidth(dblColWidth, 50);
    dblTotalColWidth := dblTotalColWidth + strngrdHistory.Columns[2].Width;

    strngrdHistory.Columns[3].Width := MinWidth(dblColWidth, 70);
    dblTotalColWidth := dblTotalColWidth + strngrdHistory.Columns[3].Width;

    strngrdHistory.Columns[4].Width := strngrdHistory.Width - dblTotalColWidth;
  end;
end;
{-----------------------------------------------------------------------------
  Procedure : ResizeIssueColumns
  Author    : iwancs
  Date      : 24 Oct 2019
  Arguments : ;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmHome.ResizeIssueColumns;
var
  dblTotalColWidth : Double;
  dblColWidth : Double;
begin
  if strngrdIssue.ColumnCount > 0 then
  begin
    dblColWidth := ClientWidth / 4;

    strngrdIssue.Columns[0].Width := MinWidth(dblColWidth, 30);
    dblTotalColWidth := dblTotalColWidth + strngrdIssue.Columns[0].Width;

    strngrdIssue.Columns[1].Width := MinWidth(dblColWidth, 50);
    dblTotalColWidth := dblTotalColWidth + strngrdIssue.Columns[1].Width;

    strngrdIssue.Columns[2].Width := MinWidth(dblColWidth, 70);
    dblTotalColWidth := dblTotalColWidth + strngrdIssue.Columns[2].Width;

    strngrdIssue.Columns[3].Width := strngrdIssue.Width - dblTotalColWidth;
  end;
end;

procedure TfrmHome.ResizeStatus;
begin
  lytTemperature.Width := ClientWidth / 2;
  lytActivity.Width := ClientWidth / 2;
//  lvSprinkleActivity.ItemAppearanceObjects.ItemObjects.Text.Width :=
//    lvSprinkleActivity.Width;
//  lvSprinkleActivity.ItemAppearanceObjects.ItemObjects.Detail.Width :=
//    lvSprinkleActivity.Width;
end;

{-----------------------------------------------------------------------------
  Procedure : LoadHistory
  Author    : iwancs
  Date      : 21 Oct 2019
  Arguments : AStationID : stringANodeID : string;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmHome.LoadHistory(
  AStationID : string;
  ANodeID: string
);
var
  arrHistory : TCSRec_Histories;
  iHistoryCount : Integer;
begin
  //Reset Values
  ClearGrid(strngrdHistory);
  chtHumidity.Series[0].Clear;

  //Get Data
  arrHistory := TCSClass_SmartFarmAPI.GetNodeHistory(
    AStationID,
    ANodeID
  );

  //Load History Graph
  for var iRow := 0 to Length(arrHistory) - 1 do
  begin
    if (AStationID = '') or
       ((AStationID <> '') and
        (arrHistory[iRow].stationID = AStationID)
       ) then
    begin
      chtHumidity.Series[0].AddXY(
        iHistoryCount,
        StrToFloatDef(arrHistory[iRow].kelembaban_value, 0)
      );
      Application.ProcessMessages;
    end;
  end;

  //Load History Tbale
  strngrdHistory.RowCount := 5;

  iHistoryCount := 0;
  // Updates data in the grid cells
  for var iRow := 0 to Length(arrHistory) - 1 do
  begin
    if (AStationID = '') or
       ((AStationID <> '') and
        (arrHistory[iRow].stationID = AStationID)
       ) then
    begin
      //Load to Grid
      strngrdHistory.Cells[0,iHistoryCount] :=
        FormatDateTime('HH:nn', arrHistory[iRow].date);
      strngrdHistory.Cells[1,iHistoryCount] := arrHistory[iRow].stationName;
      strngrdHistory.Cells[2,iHistoryCount] := arrHistory[iRow].nodeName;
      strngrdHistory.Cells[3,iHistoryCount] := arrHistory[iRow].activity;
      strngrdHistory.Cells[4,iHistoryCount] := arrHistory[iRow].description;

      Inc(iHistoryCount);
      Application.ProcessMessages;
    end;
    if iHistoryCount = 5 then
    begin
      Break;
    end;
  end;

  if (Length(arrHistory) = 0) or
     (iHistoryCount = 0) then
  begin
    for var i := 0 to 50 do
    begin
      chtHumidity.Series[0].AddXY(
        i,
        i * cos(i)
      );
    end;

    strngrdHistory.RowCount := 7;
    lytHistory.Height :=
      lblHistoryCaption.Height +
      (strngrdHistory.RowHeight *
       strngrdHistory.RowCount
      ) + 10;
    lblHistoryEmptyData.Visible := True;
  end
  else
  begin
    if iHistoryCount < 7 then
    begin
      strngrdHistory.RowCount := 7;
    end
    else
    begin
      strngrdHistory.RowCount := iHistoryCount;
    end;
    lytHistory.Height :=
      lblHistoryCaption.Height +
      (strngrdHistory.RowHeight *
       strngrdHistory.RowCount
      ) + 10;
    lblHistoryEmptyData.Visible := False;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : LoadIssue
  Author    : iwancs
  Date      : 21 Oct 2019
  Arguments : AStationID : stringANodeID : string;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmHome.LoadIssue(
  AStationID : string;
  ANodeID : string
);
var
  arrIssues : TCSRec_Issues;
  iIssueCount : Integer;
begin
  ClearGrid(strngrdIssue);
  arrIssues := TCSClass_SmartFarmAPI.GetNodeIssue(
    AStationID,
    ANodeID
  );
  strngrdIssue.RowCount := 7;

  iIssueCount := 7;
  if Length(arrIssues) < 7 then
  begin
    iIssueCount := Length(arrIssues);
  end;

  // Updates data in the grid cells
  for var iRow := 0 to iIssueCount - 1 do
  begin
    strngrdIssue.Cells[0,iRow] := arrIssues[iRow].station_name;
    strngrdIssue.Cells[1,iRow] := arrIssues[iRow].node_name;
    strngrdIssue.Cells[2,iRow] := arrIssues[iRow].activity;
    strngrdIssue.Cells[3,iRow] := arrIssues[iRow].description;
    Inc(iIssueCount);
  end;

  if Length(arrIssues) = 0 then
  begin
    lytIssue.Height :=
      lblIssueCaption.Height +
      (strngrdIssue.RowHeight *
       strngrdIssue.RowCount
      ) + 10;
    lblIssueEmptyData.Visible := True;
  end
  else
  begin
    lytIssue.Height :=
      lblIssueCaption.Height +
      (strngrdIssue.RowHeight *
       strngrdIssue.RowCount
      ) + 10;
    lblIssueEmptyData.Visible := False;
  end;
end;

procedure TfrmHome.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Application.Terminate;
end;

procedure TfrmHome.FormResize(Sender: TObject);
begin
  ResizeStatus;
  ResizeIssueColumns;
  ResizeHistoryColumns;
end;

end.

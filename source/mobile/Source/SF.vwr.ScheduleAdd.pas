unit SF.vwr.ScheduleAdd;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  SF.bsx.SmartFarmMain, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Gestures, System.ImageList, FMX.ImgList, FMX.Controls.Presentation,
  FMX.Layouts, FMX.Edit, FMX.ComboEdit, FMX.Menus, FMX.ListBox, FMX.MultiView,
  FMX.Objects, FMX.Ani, FMX.EditBox, FMX.SpinBox, FMX.DateTimeCtrls,
  FMX.Calendar, FMX.Platform, FMX.Pickers,
  SF.lib.SmartFarmAPI, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TRectangle = class(FMX.Objects.TRectangle)
  private
    FID: string;
    FText: string;
  public
    property ID : string read FID write FID;
    property Text : string read FText write FTExt;
  end;

  TfrmScheduleAdd = class(TfrmBaseMain)
    lblSave: TLabel;
    lytStation: TLayout;
    lytSearchNode: TLayout;
    lblSearchNodeCaption: TLabel;
    lytSearchSchedule: TLayout;
    lblSearchScheduleCaption: TLabel;
    btnSearchSchedule: TButton;
    swtchStatus: TSwitch;
    lytStatus: TLayout;
    rctnglBackground: TRectangle;
    rctnglScheduleAddNodeList: TRectangle;
    rctnglScheduleAddNodeCancel: TRectangle;
    lytScheduleAddNode: TLayout;
    fltnmtnScheduleAddNode: TFloatAnimation;
    fltnmtnFadeIn: TFloatAnimation;
    lblCancel: TLabel;
    btnSearchNode: TButton;
    lytSearchScheduleValue: TLayout;
    lytSearchNodeValue: TLayout;
    lytScheduleAddTime: TLayout;
    rbHumidity: TRadioButton;
    lytScheduleAddTimeModeTriggerHunidity: TLayout;
    spnbxHumidity: TSpinBox;
    lytScheduleAddTimeModeTriggerTemperature: TLayout;
    rbTemperature: TRadioButton;
    spnbxTemperature: TSpinBox;
    lytScheduleAddTimeDone: TLayout;
    lblScheduleAddTimeDone: TLabel;
    tbcScheduleAddTimeMode: TTabControl;
    tbtmScheduleAddTimeModeTime: TTabItem;
    tbtmScheduleAddTimeModeTrigger: TTabItem;
    lytScheduleAddTimeSelector: TLayout;
    rctnglScheduleAddTimeSelectorTrigger: TRectangle;
    rctnglScheduleAddTimeSelectorTime: TRectangle;
    lblScheduleAddTimeSelectorTime: TLabel;
    lblScheduleAddTimeSelectorTrigger: TLabel;
    lytScheduleAddTimeSelectorButtons: TLayout;
    rbScheduleAddTimeDay: TRadioButton;
    rbScheduleAddTimeOneTime: TRadioButton;
    tmdtScheduleAddTimeDayTime: TTimeEdit;
    chkScheduleAddTimeDayMonday: TCheckBox;
    chkScheduleAddTimeDayTuesday: TCheckBox;
    chkScheduleAddTimeDayWednesday: TCheckBox;
    chkScheduleAddTimeDaySaturday: TCheckBox;
    chkScheduleAddTimeDayFriday: TCheckBox;
    chkScheduleAddTimeDayThursday: TCheckBox;
    chkScheduleAddTimeDaySunday: TCheckBox;
    dtdtScheduleAddTimeOneTimeDate: TDateEdit;
    rctngScheduleAddTimeBackground: TRectangle;
    fltnmtnScheduleAddTime: TFloatAnimation;
    cbbStationList: TComboBox;
    vrtscrlbxNodeList: TVertScrollBox;
    lytScheduleAddTimeModeDays: TLayout;
    lytScheduleAddTimeModeHours: TLayout;
    lblSearchNode: TLabel;
    rctnglSearchNode: TRectangle;
    rctnglSearchSchedule: TRectangle;
    lblSearchSchedule: TLabel;
    fdmtblSchedules: TFDMemTable;
    strngfldSchedulesSTATION_ID: TStringField;
    strngfldSchedulesNODE_ID: TStringField;
    intgrfldSchedulesTYPE: TIntegerField;
    intgrfldSchedulesMODE: TIntegerField;
    strngfldSchedulesVALUE: TStringField;
    strngfldSchedulesDAYS: TStringField;
    strngfldSchedulesACTIVE: TStringField;
    procedure FormDestroy(Sender: TObject);
    procedure btnNodeIconClick(Sender: TObject);
    procedure btnSearchNodeClick(Sender: TObject);
    procedure btnSearchScheduleClick(Sender: TObject);
    procedure cbbStationListChange(Sender: TObject);
    procedure edtSearchNodeClick(Sender: TObject);
    procedure edtSearchScheduleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblSaveClick(Sender: TObject);
    procedure lblScheduleAddTimeDoneClick(Sender: TObject);
    procedure lblTitleClick(Sender: TObject);
    procedure OnScheduleAddTimeSelector_Click(Sender: TObject);
    procedure OnNode_Click(Sender: TObject);
    procedure lstNodeListItemClick(const Sender: TCustomListBox; const Item:
        TListBoxItem);
    procedure rctnglBackgroundClick(Sender: TObject);
    procedure rctnglScheduleAddNodeCancelClick(Sender: TObject);
  private
    FForm : TCustomPopupForm;
    FDateTimePicker: TCustomDateTimePicker;
    FStationList : TCSRec_Stations;
    FNodeList : TCSRec_Nodes;
    FSchedule : TCSRec_Schedule;

    procedure Event_FullyLoaded(Sender : TObject);

    procedure AddItemListBox(
      AID : string;
      AText : string;
      AFontColor : TAlphaColor = TAlphaColorRec.Black
    );
    procedure SetDaysLayout;
    procedure FillNodeList;

    function UploadSchedule : Boolean;
    function SaveSchedule : Boolean;
    procedure SetScheduleAddNode(AShow : Boolean);
    procedure SetScheduleAddTime(AShow : Boolean);
    { Private declarations }
  public
    { Public declarations }
    class procedure ShowWait(AProc : TProc);
  end;

var
  frmScheduleAdd: TfrmScheduleAdd;

implementation

uses System.Math,
  SF.lib.SmartFarmHelpoer;

{$R *.fmx}

procedure TfrmScheduleAdd.FormDestroy(Sender: TObject);
begin
end;

{-----------------------------------------------------------------------------
  Procedure : AddItemListBox
  Author    : iwancs
  Date      : 19 Oct 2019
  Arguments : AID : stringAText : string;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmScheduleAdd.AddItemListBox(
  AID : string;
  AText: string;
  AFontColor : TAlphaColor
);
var
  rctnglNode: TRectangle;
  lblNode: TLabel;
begin
  rctnglNode := TRectangle.Create(Self);
  lblNode := TLabel.Create(Self);

  vrtscrlbxNodeList.Padding.Top := 5;
  vrtscrlbxNodeList.Padding.Bottom := 5;
  vrtscrlbxNodeList.Padding.Left := 10;
  vrtscrlbxNodeList.Padding.Right := 10;
  rctnglNode.Parent := vrtscrlbxNodeList;
  rctnglNode.Name := 'rctnglNode' + AID;
  rctnglNode.Sides := [TSide.Bottom];
  rctnglNode.Fill.Kind := TBrushKind.None;
  rctnglNode.Stroke.Color := $FFBFBFBF;
  rctnglNode.ID := AID;
  rctnglNode.Text := AText;
  rctnglNode.OnClick := OnNode_Click;
  rctnglNode.Visible := True;
  rctnglNode.Margins.Left := 5;
  rctnglNode.Margins.Right := 5;
  rctnglNode.Top := rctnglNode.Height *
    vrtscrlbxNodeList.Content.ChildrenCount;
  rctnglNode.Align := TAlignLayout.Top;

  lblNode.Parent := rctnglNode;
  lblNode.Name := 'lblNode' + AID;
  lblNode.Align := TAlignLayout.Client;
  lblNode.HitTest := False;
  lblNode.StyledSettings := [];
  lblNode.TextSettings.Font.Family := 'Ubuntu';
  lblNode.TextSettings.Font.Size := 20;
  lblNode.TextSettings.FontColor := AFontColor;
  lblNode.TextSettings.HorzAlign := TTextAlign.Center;
  lblNode.Text := AText;

  if vrtscrlbxNodeList.Content.ChildrenCount <= 5 then
  begin
    lytScheduleAddNode.Height :=
      (rctnglNode.Height * vrtscrlbxNodeList.Content.ChildrenCount) + 10;
  end;
end;

procedure TfrmScheduleAdd.btnNodeIconClick(Sender: TObject);
begin
  SetScheduleAddNode(True);
end;

procedure TfrmScheduleAdd.btnSearchNodeClick(Sender: TObject);
begin
  inherited;
  SetScheduleAddNode(True);
end;

procedure TfrmScheduleAdd.btnSearchScheduleClick(Sender: TObject);
begin
  inherited;
  SetScheduleAddTime(True);
end;

procedure TfrmScheduleAdd.cbbStationListChange(Sender: TObject);
begin
  inherited;
  if Length(FStationList) > 0  then
  begin
    FillNodeList;
  end;
end;

procedure TfrmScheduleAdd.edtSearchNodeClick(Sender: TObject);
begin
  SetScheduleAddNode(True);
end;

procedure TfrmScheduleAdd.edtSearchScheduleClick(Sender: TObject);
begin
  inherited;
  SetScheduleAddTime(True);
end;

procedure TfrmScheduleAdd.Event_FullyLoaded(Sender: TObject);
begin
  tbcDevices.TabIndex := 1;
  lytScheduleAddNode.Visible := False;
  lytScheduleAddTime.Visible := False;
  FStationList := TCSClass_SmartFarmHelper.FillStationList(
    cbbStationList,
    False
  );
  FillNodeList;
  fdmtblSchedules.Close;
  fdmtblSchedules.CreateDataSet;
//  TCSClass_SmartFarmHelper.LoadScheduleData(
//    fdmtblSchedules,
//    FStationList,
//    cbbStationList.ItemIndex
//  );
  SetDaysLayout;
  tbcScheduleAddTimeMode.TabIndex := 0;
  rbHumidity.IsChecked := True;
  rbScheduleAddTimeDay.IsChecked := True;
end;

procedure TfrmScheduleAdd.FormCreate(Sender: TObject);
begin
  inherited;
  OnFullyLoaded := Event_FullyLoaded;
end;

procedure TfrmScheduleAdd.lstNodeListItemClick(
  const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  inherited;
  ShowMessage(Item.Text);
end;

procedure TfrmScheduleAdd.FillNodeList;
var
  arrNodeList : TCSRec_Nodes;
begin
  SetLength(FNodeList,0);
//  if cbbStationList.ItemIndex > 0 then
//  begin
//    FNodeList := TCSClass_SmartFarmAPI.GetNodeList(
//      FStationList[cbbStationList.ItemIndex - 1].slug
//    );
//  end
//  else
//  begin
//    for var i := 0 to Length(FStationList) - 1 do
//    begin
//      SetLength(arrNodeList,0);
//      arrNodeList := TCSClass_SmartFarmAPI.GetNodeList(
//        FStationList[i].slug
//      );
//      FNodeList := FNodeList + arrNodeList;
//    end;
//  end;

  FNodeList := TCSClass_SmartFarmAPI.GetNodeList(
    FStationList[cbbStationList.ItemIndex].slug
  );

  AddItemListBox(
    '0',
    'All Nodes'
  );
  for var i := 0 to Length(FNodeList) - 1 do
  begin
    AddItemListBox(
      FNodeList[i].slug,
      FNodeList[i].name,
      $FF5599FF
    );
  end;
end;

procedure TfrmScheduleAdd.lblSaveClick(Sender: TObject);
begin
  if UploadSchedule then
  begin
    Close;
  end;
end;

procedure TfrmScheduleAdd.lblScheduleAddTimeDoneClick(Sender: TObject);
begin
  if SaveSchedule then
  begin
    SetScheduleAddTime(False);
  end;
end;

procedure TfrmScheduleAdd.lblTitleClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScheduleAdd.OnScheduleAddTimeSelector_Click(Sender: TObject);
begin
  if TLabel(Sender).Tag <> lytScheduleAddTimeSelectorButtons.Tag then
  begin
    case TLabel(Sender).Tag of
      1:  //Time
      begin
        rctnglScheduleAddTimeSelectorTime.Fill.Color := $FF5599FF;
        lblScheduleAddTimeSelectorTime.FontColor := TAlphaColorRec.White;

        rctnglScheduleAddTimeSelectorTrigger.Fill.Color := TAlphaColorRec.White;
        lblScheduleAddTimeSelectorTrigger.FontColor := $FF5599FF;

      end;
      2:  //Trigger
      begin
        rctnglScheduleAddTimeSelectorTrigger.Fill.Color := $FF5599FF;
        lblScheduleAddTimeSelectorTrigger.FontColor := TAlphaColorRec.White;

        rctnglScheduleAddTimeSelectorTime.Fill.Color := TAlphaColorRec.White;
        lblScheduleAddTimeSelectorTime.FontColor := $FF5599FF;
      end;
    end;

    lytScheduleAddTimeSelectorButtons.Tag := TLabel(Sender).Tag;
    tbcScheduleAddTimeMode.TabIndex := TLabel(Sender).Tag - 1;
  end;
end;

procedure TfrmScheduleAdd.SetDaysLayout;
var
  dblTotalWidth : Double;
  dblWidth : Double;
begin
  dblTotalWidth := chkScheduleAddTimeDayThursday.Width +
                   chkScheduleAddTimeDayFriday.Width;

  dblWidth := (ClientWidth -
               chkScheduleAddTimeDayMonday.Position.X -
               dblTotalWidth
              ) / 2;
  if dblWidth < 0 then
  begin
    dblWidth := 2;
  end;
  dblWidth := (dblTotalWidth / 2) + dblWidth;
  chkScheduleAddTimeDayMonday.Position.X := 24;
  chkScheduleAddTimeDayMonday.Position.y := 8;

  chkScheduleAddTimeDayTuesday.Position.X :=
    chkScheduleAddTimeDayMonday.Position.X +
    dblWidth;
  chkScheduleAddTimeDayTuesday.Position.y := 8;

  chkScheduleAddTimeDayWednesday.Position.X := 24;
  chkScheduleAddTimeDayWednesday.Position.y := 40;

  chkScheduleAddTimeDayThursday.Position.X :=
    chkScheduleAddTimeDayWednesday.Position.X +
    dblWidth;
  chkScheduleAddTimeDayThursday.Position.y := 40;

  chkScheduleAddTimeDayFriday.Position.X := 24;
  chkScheduleAddTimeDayFriday.Position.y := 72;

  chkScheduleAddTimeDaySaturday.Position.X :=
    chkScheduleAddTimeDayFriday.Position.X +
    dblWidth;
  chkScheduleAddTimeDaySaturday.Position.y := 72;

  chkScheduleAddTimeDaySunday.Position.X := 24;
  chkScheduleAddTimeDaySunday.Position.y :=104;
end;

procedure TfrmScheduleAdd.SetScheduleAddNode(AShow : Boolean);
var
  dWidth : Single;
  dHeight : Single;
begin
  rctnglBackground.Visible := AShow;
  lytScheduleAddNode.Visible := AShow;
  if AShow then
  begin
    dWidth := ClientWidth - 20;
    dHeight := lytScheduleAddNode.Height;
    lytScheduleAddNode.Width := ClientWidth - 20;
    lytScheduleAddNode.Height := rctnglScheduleAddNodeList.Height +
                            rctnglScheduleAddNodeCancel.Height +
                            rctnglScheduleAddNodeList.Margins.Bottom;
    lytScheduleAddNode.Position.X := (ClientWidth - dWidth) / 2;
    fltnmtnFadeIn.StartValue := 0;
    fltnmtnFadeIn.StopValue := 0.5;
    fltnmtnFadeIn.Start;
    fltnmtnScheduleAddNode.StartValue := ClientHeight;
    fltnmtnScheduleAddNode.StopValue :=(ClientHeight - dHeight);
    fltnmtnScheduleAddNode.Start;
  end;
end;

procedure TfrmScheduleAdd.SetScheduleAddTime(AShow: Boolean);
begin
  rctnglBackground.Visible := AShow;
  lytScheduleAddTime.Visible := AShow;
  if AShow then
  begin
    lytScheduleAddTime.Width := ClientWidth;
    lytScheduleAddTime.Position.X := 0;
    fltnmtnFadeIn.StartValue := 0;
    fltnmtnFadeIn.StopValue := 0.5;
    fltnmtnFadeIn.Start;
    fltnmtnScheduleAddTime.StartValue := ClientHeight;
    fltnmtnScheduleAddTime.StopValue :=(
      ClientHeight - lytScheduleAddTime.Height);
    fltnmtnScheduleAddTime.Start;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure : ShowWait
  Author    : iwancs
  Date      : 26 Oct 2019
  Arguments : AProc : TProc;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TfrmScheduleAdd.ShowWait(AProc: TProc);
//var
//  frmTemp : TCustomForm;
begin
//  frmTemp := TCSClass_Forms.IsFormLoaded('frmScheduleAdd');
//  if frmTemp = nil then
//  begin
//    frmTemp := TfrmScheduleAdd.Create(Self);
//  end;
//  frmTemp.ShowModal(;
end;

function TfrmScheduleAdd.UploadSchedule : Boolean;
var
  sStationID : string;
  sNodeID : string;
  sDays : string;
  iCount : Integer;
begin
  Result := False;
  FSchedule.Active := swtchStatus.IsChecked;

  iCount := 1;
  if lblSearchNode.TagString = '0' then
  begin
    iCount := Length(FNodeList);
  end
  else
  begin
    //For single node
    sNodeID := lblSearchNode.TagString;
  end;

  for var i := 0 to iCount - 1 do
  begin
    if (sNodeID = '') or
       (i > 0) then
    begin
      sNodeID := FNodeList[i].slug;
    end;

    if not fdmtblSchedules.Locate(
      'STATION_ID;NODE_ID;TYPE;MODE;VALUE;DAYS',
      VarArrayOf(
        [FStationList[cbbStationList.ItemIndex].slug,
         sNodeID,
         FSchedule._Type,
         FSchedule.Mode,
         FSchedule.Value,
         FSchedule.DayToString
        ]
      )
    ) then
    begin
      fdmtblSchedules.Append;
      fdmtblSchedules.FieldByName('STATION_ID').AsString :=
        FStationList[cbbStationList.ItemIndex].slug;
      fdmtblSchedules.FieldByName('NODE_ID').AsString := sNodeID;
      fdmtblSchedules.FieldByName('TYPE').AsInteger :=
        FSchedule._Type;
      fdmtblSchedules.FieldByName('MODE').AsInteger :=
        FSchedule.Mode;
      fdmtblSchedules.FieldByName('VALUE').AsString :=
        FSchedule.Value;
      fdmtblSchedules.FieldByName('DAYS').AsString :=
        FSchedule.DayToString;
      fdmtblSchedules.FieldByName('ACTIVE').AsString :=
        FSchedule.ActiveToString;
      fdmtblSchedules.Post;
    end;
  end;


  iCount := 1;
  if lblSearchNode.TagString = '0' then
  begin
    iCount := Length(FNodeList);
  end
  else
  begin
    //For single node
    sNodeID := lblSearchNode.TagString;
  end;

  for var i := 0 to iCount - 1 do
  begin
    if (sNodeID = '') or
       (i > 0) then
    begin
      sNodeID := FNodeList[i].slug;
    end;
    fdmtblSchedules.Filter := Format(
      'STATION_ID=%s AND NODE_ID=%s',
      [QuotedStr(FStationList[cbbStationList.ItemIndex].slug),
       QuotedStr(sNodeID)
      ]
    );
    fdmtblSchedules.Filtered := True;
    TCSClass_SmartFarmAPI.SetSchedule(
      FStationList[cbbStationList.ItemIndex].slug,
      sNodeID,
      fdmtblSchedules
    );
  end;
  Result := True;
//  end
//  else
//  begin
//    ShowMessage('Must Select a Station');
//  end;
end;

{-----------------------------------------------------------------------------
  Procedure : SaveSchedule
  Author    : iwancs
  Date      : 23 Oct 2019
  Arguments : ;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
function TfrmScheduleAdd.SaveSchedule : Boolean;
var
  bAllDay : Boolean;
  sDays : string;
begin
  Result := False;
  case tbcScheduleAddTimeMode.TabIndex of
    0:  //By Time
    begin
      if rbScheduleAddTimeDay.IsChecked then  //Specific Day
      begin
        bAllDay := True;
        sDays := '';
        FSchedule.Days := [];
        if chkScheduleAddTimeDayMonday.IsChecked then
        begin
          sDays := sDays + 'Monday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDayMonday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if chkScheduleAddTimeDayTuesday.IsChecked then
        begin
          sDays := sDays + 'Tuesday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDayTuesday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if chkScheduleAddTimeDayWednesday.IsChecked then
        begin
          sDays := sDays + 'Wednesday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDayWednesday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if chkScheduleAddTimeDayThursday.IsChecked then
        begin
          sDays := sDays + 'Thursday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDayThursday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if chkScheduleAddTimeDayFriday.IsChecked then
        begin
          sDays := sDays + 'Friday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDayFriday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if chkScheduleAddTimeDaySaturday.IsChecked then
        begin
          sDays := sDays + 'Saturday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDaySaturday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if chkScheduleAddTimeDaySunday.IsChecked then
        begin
          sDays := sDays + 'Sunday, ';
          FSchedule.Days := FSchedule.Days +
                            [chkScheduleAddTimeDaySunday.Tag]
        end
        else
        begin
          bAllDay := False;
        end;

        if sDays = '' then
        begin
          ShowMessage('Must Select At Least A Day');
        end;
        if bAllDay then
        begin
          lblSearchSchedule.Text := Format(
            'Every Day, %s',
            [FormatDateTime('HH:nn',tmdtScheduleAddTimeDayTime.Time)]
          );
          FSchedule._Type := 1;
          FSchedule.Mode := 0;
          FSchedule.Value := FormatDateTime(
            'HH:nn',
            tmdtScheduleAddTimeDayTime.Time
          );
          Result := True;
        end
        else
        begin
          lblSearchSchedule.Text := Format(
            'Every %s, %s ',
            [sDays,
             FormatDateTime('HH:nn',tmdtScheduleAddTimeDayTime.Time)
            ]
          );
          FSchedule._Type := 1;
          FSchedule.Mode := 0;
          FSchedule.Value := FormatDateTime(
            'HH:nn',
            tmdtScheduleAddTimeDayTime.Time
          );
          Result := True;
        end;
      end
      else
      begin
        //One Time
        lblSearchSchedule.Text := Format(
          'On %s',
          [FormatDateTime('dd MMMM yyy',dtdtScheduleAddTimeOneTimeDate.DateTime)]
        );
        FSchedule._Type := 1;
        FSchedule.Mode := 1;
        FSchedule.Value := FormatDateTime(
            'yyyy-MM-DD HH:nn',
            dtdtScheduleAddTimeOneTimeDate.Date +
            tmdtScheduleAddTimeDayTime.Time
          );
        Result := True;
      end;
    end;
    1:  //By Trigger
    begin
      if rbHumidity.IsChecked then
      begin
        //Trigger by Humidity Value
        lblSearchSchedule.Text := Format(
          'When Humidity is %s',
          [FormatFloat('#,##0.##', spnbxHumidity.Value)]
        );
        FSchedule._Type := 0;
        FSchedule.Mode := 0;
        FSchedule.Value := spnbxHumidity.Value.ToString;
        Result := True;
      end;
      if rbTemperature.IsChecked then
      begin
        //Trigger by Humidity Value
        lblSearchSchedule.Text := Format(
          'When Temperature is %s',
          [FormatFloat('#,##0', spnbxTemperature.Value)]
        );
        FSchedule._Type := 1;
        FSchedule.Mode := 1;
        FSchedule.Value := spnbxTemperature.Value.ToString;
        Result := True;
      end;
    end;
  end;
end;

procedure TfrmScheduleAdd.OnNode_Click(Sender: TObject);
begin
//  ShowMessage((Sender as TRectangle).ID);
  lblSearchNode.Text := (Sender as TRectangle).Text;
  lblSearchNode.TagString := (Sender as TRectangle).ID;
  SetScheduleAddNode(False);
end;

procedure TfrmScheduleAdd.rctnglBackgroundClick(Sender: TObject);
begin
  inherited;
  SetScheduleAddNode(False);
  SetScheduleAddTime(False);
end;

procedure TfrmScheduleAdd.rctnglScheduleAddNodeCancelClick(Sender: TObject);
begin
  inherited;
  SetScheduleAddNode(False);
end;

end.

{-----------------------------------------------------------------------------
 Project     : iSolutions MKD Printing Service  Library for SmartFarm
 Unit Name   : iSolutions.lib.ListItemControls.pas
 Author      : CopyRight® 2019 - PT Interaktif Cipta Lestari - Indonesia
 Email       : iwan.c.sugeng@gmail.com
 Date        : 22 Oct 2019
 Description : https://pastebin.com/U47Bnibn
               https://stackoverflow.com/questions/37076388/add-tswitch-to-every-tlistview-item
               https://stackoverflow.com/questions/37083809/click-events-being-caught-by-list-view-parent-item
 How To Use  :
 Todo	     :
 History     :
-----------------------------------------------------------------------------}
unit iSolutions.lib.ListItemControls;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView;

type
  TJDListViewControl = class;
  TListItemSwitch = class;
  TListItemProgressBar = class;

  TLISwitchThumbStyle = (tsRect, tsRoundRect, tsElipse);

  TJDListViewControlClass = class of TJDListViewControl;

  { TJDListViewControl }

  TJDListViewControl = class(TListItemSimpleControl)
  private

  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
  end;

  { TListItemSwitch }

  TListItemSwitch = class(TJDListViewControl)
  private
    FIsChecked: Boolean;
    FOnSwitch: TNotifyEvent;
    FThumbStyle: TLISwitchThumbStyle;
    FThumbWidth: Single;
    FThumbHeight: Single;
    FThumbRound: Single;
    FSwitchRound: Single;
    FOffColor: TAlphaColor;
    FOnColor: TAlphaColor;
    FThumbColor: TAlphaColor;

    FThumbOnColor: TAlphaColor;       //23 Oct 2019 - Added By ICS
    FThumbOffColor: TAlphaColor;      //23 Oct 2019 - Added By ICS

    FOnText: String;
    FOffText: String;
    function GetOffText: String;
    function GetOnText: String;
    procedure SetOffText(const Value: String);
    procedure SetOnText(const Value: String);
    function GetThumbOffColor: TAlphaColor;
    function GetThumbOnColor: TAlphaColor;
    procedure SetThumbOffColor(const Value: TAlphaColor);
    procedure SetThumbOnColor(const Value: TAlphaColor);
  protected
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
      override;
    procedure DoSwitch; virtual;
    procedure Render(
      const Canvas: TCanvas;
      const DrawItemIndex: Integer;
      const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources;
      const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0
    ); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
  public
    function GetIsChecked: Boolean;
    function GetOffColor: TAlphaColor;
    function GetOnColor: TAlphaColor;
    function GetOnSwitch: TNotifyEvent;
    function GetSwitchRound: Single;
    function GetThumbColor: TAlphaColor;
    function GetThumbHeight: Single;
    function GetThumbRound: Single;
    function GetThumbStyle: TLISwitchThumbStyle;
    function GetThumbWidth: Single;

    procedure SetIsChecked(const AValue: Boolean);
    procedure SetThumbStyle(const Value: TLISwitchThumbStyle);
    procedure SetThumbWidth(const Value: Single);
    procedure SetThumbHeight(const Value: Single);
    procedure SetThumbRound(const Value: Single);
    procedure SetSwitchRound(const Value: Single);
    procedure SetOffColor(const Value: TAlphaColor);
    procedure SetOnColor(const Value: TAlphaColor);
    procedure SetThumbColor(const Value: TAlphaColor);
    procedure SetOnSwitch(const Value: TNotifyEvent);

    property IsChecked: Boolean read GetIsChecked write SetIsChecked;
    property OnColor: TAlphaColor read GetOnColor write SetOnColor;
    property OffColor: TAlphaColor read GetOffColor write SetOffColor;
    property OnText: String read GetOnText write SetOnText;
    property OffText: String read GetOffText write SetOffText;
    property SwitchRound: Single read GetSwitchRound write SetSwitchRound;
    property ThumbColor: TAlphaColor read GetThumbColor write SetThumbColor;
    property ThumbOnColor: TAlphaColor read GetThumbOnColor write SetThumbOnColor;
    property ThumbOffColor: TAlphaColor read GetThumbOffColor write SetThumbOffColor;
    property ThumbHeight: Single read GetThumbHeight write SetThumbHeight;
    property ThumbStyle: TLISwitchThumbStyle read GetThumbStyle write SetThumbStyle;
    property ThumbWidth: Single read GetThumbWidth write SetThumbWidth;
    property ThumbRound: Single read GetThumbRound write SetThumbRound;
    property OnSwitch: TNotifyEvent read GetOnSwitch write SetOnSwitch;
  end;

  { TListItemProgressBar }

  TLIProgressBarStyle = (psNormal, psFill);

  TListItemProgressBar = class(TJDListViewControl)
  private
    FMin: Double;
    FMax: Double;
    FPosition: Double;
  protected
    function MouseDown(const Button: TMouseButton; const Shift: TShiftState; const MousePos: TPointF): Boolean;
      override;
    procedure Render(
      const Canvas: TCanvas;
      const DrawItemIndex: Integer;
      const DrawStates: TListItemDrawStates;
      const Resources: TListItemStyleResources;
      const Params: TListItemDrawable.TParams;
      const SubPassNo: Integer = 0
    ); override;
  public
    constructor Create(const AOwner: TListItem); override;
    destructor Destroy; override;
  public

  end;


implementation

{ TJDListViewControl }

constructor TJDListViewControl.Create(const AOwner: TListItem);
begin
  inherited;

end;

destructor TJDListViewControl.Destroy;
begin

  inherited;
end;

{ TListItemSwitch }

constructor TListItemSwitch.Create(const AOwner: TListItem);
begin
  inherited;
  Width:= 50;
  Height:= 20;
  FIsChecked:= False;
  FOnColor:= TAlphaColorRec.Lime;
  FOffColor:= TAlphaColorRec.SkyBlue;
  FSwitchRound:= 5;
  FThumbColor:= TAlphaColorRec.Black;

  FThumbOnColor:= TAlphaColorRec.Black;   //23 Oct 2019 - Added By ICS
  FThumbOffColor:= TAlphaColorRec.Grey;   //23 Oct 2019 - Added By ICS

  FThumbWidth:= 14;
  FThumbHeight:= 14;
  FThumbRound:= 5;
end;

destructor TListItemSwitch.Destroy;
begin

  inherited;
end;

function TListItemSwitch.MouseDown(const Button: TMouseButton;
  const Shift: TShiftState; const MousePos: TPointF): Boolean;
begin
  if (Button = TMouseButton.mbLeft) and Enabled then begin
    Result := inherited;
    if Result then begin
      DoSwitch;
    end;
  end;
end;

procedure TListItemSwitch.DoSwitch;
begin
  FIsChecked:= not FIsChecked;
  if Assigned(OnSwitch) then
    OnSwitch(Self);
  Invalidate;
end;

function TListItemSwitch.GetIsChecked: Boolean;
begin
  Result:= FIsChecked;
end;

function TListItemSwitch.GetOffColor: TAlphaColor;
begin
  Result:= FOffColor;
end;

function TListItemSwitch.GetOffText: String;
begin
  Result:= FOffText;
end;

function TListItemSwitch.GetOnColor: TAlphaColor;
begin
  Result:= FOnColor;
end;

function TListItemSwitch.GetOnSwitch: TNotifyEvent;
begin
  Result := FOnSwitch;
end;

function TListItemSwitch.GetOnText: String;
begin
  Result:= FOnText;
end;

function TListItemSwitch.GetSwitchRound: Single;
begin
  Result:= FSwitchRound;
end;

function TListItemSwitch.GetThumbColor: TAlphaColor;
begin
  Result:= FThumbColor;
end;

function TListItemSwitch.GetThumbHeight: Single;
begin
  Result:= FThumbHeight;
end;

function TListItemSwitch.GetThumbOffColor: TAlphaColor;
begin
  //23 Oct 2019 - Added By ICS
  Result := FThumbOffColor;
end;

function TListItemSwitch.GetThumbOnColor: TAlphaColor;
begin
  //23 Oct 2019 - Added By ICS
  Result := FThumbOnColor;
end;

function TListItemSwitch.GetThumbRound: Single;
begin
  Result:= FThumbRound;
end;

function TListItemSwitch.GetThumbStyle: TLISwitchThumbStyle;
begin
  Result:= FThumbStyle;
end;

function TListItemSwitch.GetThumbWidth: Single;
begin
  Result:= FThumbWidth;
end;

procedure TListItemSwitch.SetIsChecked(const AValue: Boolean);
begin
  FIsChecked:= AValue;
  Invalidate;
end;

procedure TListItemSwitch.SetOffColor(const Value: TAlphaColor);
begin
  FOffColor := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetOffText(const Value: String);
begin
  FOffText:= Value;
  Invalidate;
end;

procedure TListItemSwitch.SetOnColor(const Value: TAlphaColor);
begin
  FOnColor := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetOnSwitch(const Value: TNotifyEvent);
begin
  FOnSwitch:= Value;
  Invalidate;
end;

procedure TListItemSwitch.SetOnText(const Value: String);
begin
  FOnText:= Value;
  Invalidate;
end;

procedure TListItemSwitch.SetSwitchRound(const Value: Single);
begin
  FSwitchRound := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbWidth(const Value: Single);
begin
  FThumbWidth := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbColor(const Value: TAlphaColor);
begin
  FThumbColor := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbHeight(const Value: Single);
begin
  FThumbHeight := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbOffColor(const Value: TAlphaColor);
begin
  //23 Oct 2019 - Added By ICS
  FThumbOffColor := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbOnColor(const Value: TAlphaColor);
begin
  //23 Oct 2019 - Added By ICS
  FThumbOnColor := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbRound(const Value: Single);
begin
  FThumbRound := Value;
  Invalidate;
end;

procedure TListItemSwitch.SetThumbStyle(const Value: TLISwitchThumbStyle);
begin
  FThumbStyle := Value;
  Invalidate;
end;

procedure TListItemSwitch.Render(
  const Canvas: TCanvas;
  const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates;
  const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams;
  const SubPassNo: Integer = 0
);
var
  R, R2: TRectF;
  D: Single;
begin
  inherited;
  R:= Self.LocalRect;
  R2:= R;
  Canvas.BeginScene;
  try
    Canvas.Stroke.Kind:= TBrushKind.None;
    Canvas.Fill.Kind:= TBrushKind.Solid;
    if IsChecked then begin
      Canvas.Fill.Color:= FOnColor;
    end else begin
      Canvas.Fill.Color:= FOffColor;
    end;
    Canvas.FillRect(R, FSwitchRound, FSwitchRound,
      [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight],
      1.0, TCornerType.Round);
    R2.Top:= R.Top + (R.Height / 2) - (FThumbHeight / 2);
    R2.Height:= FThumbHeight;
    D:= R2.Top - R.Top;
    if IsChecked then begin
      R2.Left:= R.Right - FThumbWidth - D;
    end else begin
      R2.Left:= R.Left + D;
    end;
    R2.Width:= FThumbWidth;

    //23 Oct 2019 - Added By ICS
    if IsChecked then begin
      Canvas.Fill.Color:= FThumbOnColor;
    end else begin
      Canvas.Fill.Color:= FThumbOffColor;
    end;
//    Canvas.Fill.Color:= FThumbColor;

    Canvas.FillRect(R2, FThumbRound, FThumbRound,
      [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight],
      1.0, TCornerType.Round);
  finally
    Canvas.EndScene;
  end;
end;

{ TListItemProgressBar }

constructor TListItemProgressBar.Create(const AOwner: TListItem);
begin
  inherited;

end;

destructor TListItemProgressBar.Destroy;
begin

  inherited;
end;

function TListItemProgressBar.MouseDown(const Button: TMouseButton;
  const Shift: TShiftState; const MousePos: TPointF): Boolean;
begin

end;

procedure TListItemProgressBar.Render(
  const Canvas: TCanvas;
  const DrawItemIndex: Integer;
  const DrawStates: TListItemDrawStates;
  const Resources: TListItemStyleResources;
  const Params: TListItemDrawable.TParams;
  const SubPassNo: Integer
);
begin
  inherited;

end;

end.

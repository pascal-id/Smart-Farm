unit iSolutions.lib.VKKeyboardHelper;

interface

uses System.Classes, System.Types, System.SysUtils,
     FMX.Forms, FMX.Layouts, FMX.Types, FMX.Controls;

type
  TCSClass_VKKeyboardHelper = class

    procedure FormVirtualKeyboardHidden(
      Sender: TObject;
      KeyboardVisible: Boolean;
      const Bounds: TRect);
    procedure FormVirtualKeyboardShown(
      Sender: TObject;
      KeyboardVisible: Boolean;
      const Bounds: TRect);
    procedure FormFocusChanged(Sender: TObject);
  private
    FOwner : TForm;

    FContentLayout: TLayout;
    FContentScrollBox: TVertScrollBox;

    //Scrollable editing support
    FKBBounds: TRectF;
    FNeedOffset: Boolean;

    procedure CalcContentBoundsProc(
      Sender: TObject;
      var ContentBounds: TRectF);
    procedure RestorePosition;
    procedure UpdateKBBounds;
  public
    constructor Create(AOwner : TForm);
    destructor Destroy; override;

    property ContentScrollBox : TVertScrollBox
      read FContentScrollBox write FContentScrollBox;
    property ContentLayout : TLayout
      read FContentLayout write FContentLayout;
  end;

  TCSClass_BackButtonHelper = class
    class procedure Handle_HardwareBack(
      var AKey : Word;
      AFunc_HandleClose : TFunc<Boolean>
    );
  end;

implementation

uses System.Math, System.UITypes,
     FMX.Platform, FMX.VirtualKeyboard;


{ TCSClass_VKKeyboardHelper }

procedure TCSClass_VKKeyboardHelper.CalcContentBoundsProc(
  Sender: TObject;
  var ContentBounds: TRectF);
begin
  if FNeedOffset and (FKBBounds.Top > 0) then
  begin
    ContentBounds.Bottom := Max(
      ContentBounds.Bottom,
      2 * FOwner.ClientHeight - FKBBounds.Top
    );
  end;
end;

constructor TCSClass_VKKeyboardHelper.Create(AOwner: TForm);
begin
  FOwner := AOwner;
  FOwner.OnVirtualKeyboardShown := FormVirtualKeyboardShown;
  FOwner.OnVirtualKeyboardHidden := FormVirtualKeyboardHidden;
  FOwner.OnFocusChanged := FormFocusChanged;
end;

destructor TCSClass_VKKeyboardHelper.Destroy;
begin

  inherited;
end;

procedure TCSClass_VKKeyboardHelper.FormFocusChanged(Sender: TObject);
begin
  UpdateKBBounds;
end;

procedure TCSClass_VKKeyboardHelper.FormVirtualKeyboardHidden(
  Sender: TObject;
  KeyboardVisible: Boolean;
  const Bounds: TRect
);
begin
  FKBBounds.Create(0, 0, 0, 0);
  FNeedOffset := False;
  RestorePosition;
end;

procedure TCSClass_VKKeyboardHelper.FormVirtualKeyboardShown(
  Sender: TObject;
  KeyboardVisible: Boolean;
  const Bounds: TRect);
begin
  FKBBounds := TRectF.Create(Bounds);
  FKBBounds.TopLeft := FOwner.ScreenToClient(FKBBounds.TopLeft);
  FKBBounds.BottomRight := FOwner.ScreenToClient(FKBBounds.BottomRight);
  UpdateKBBounds;
end;

procedure TCSClass_VKKeyboardHelper.RestorePosition;
begin
  FContentScrollBox.ViewportPosition := PointF(
    FContentScrollBox.ViewportPosition.X,
    0
  );
  FContentLayout.Align := TAlignLayout.alClient;
  FContentScrollBox.RealignContent;
end;

procedure TCSClass_VKKeyboardHelper.UpdateKBBounds;
var
  LFocused : TControl;
  LFocusRect: TRectF;
begin
  FNeedOffset := False;
  if Assigned(FOwner.Focused) then
  begin
    LFocused := TControl(FOwner.Focused.GetObject);
    LFocusRect := LFocused.AbsoluteRect;
    LFocusRect.Offset(FContentScrollBox.ViewportPosition);
    if (LFocusRect.IntersectsWith(TRectF.Create(FKBBounds))) and
       (LFocusRect.Bottom > FKBBounds.Top) then
    begin
      FNeedOffset := True;
      FContentLayout.Align := TAlignLayout.alHorizontal;
      FContentScrollBox.RealignContent;
      Application.ProcessMessages;
      FContentScrollBox.ViewportPosition :=
        PointF(FContentScrollBox.ViewportPosition.X,
               LFocusRect.Bottom - FKBBounds.Top);
    end;
  end;
  if not FNeedOffset then
    RestorePosition;
end;

{ TCSClass_BackButtonHelper }
{-----------------------------------------------------------------------------
  Procedure : Handle_HardwareBack
  Author    : iwancs
  Date      : 20 Oct 2019
  Arguments : AKey : WordAFunc_HandleClose : TFunc<Boolean>;

  Result    :
  Desc      :
  Version   :
  Reference : https://stackoverflow.com/questions/18774408/how-do-i-handle-a-back-button-press-in-a-delphi-android-app
  CallBy    :
-----------------------------------------------------------------------------}
class procedure TCSClass_BackButtonHelper.Handle_HardwareBack(
  var AKey : Word;
  AFunc_HandleClose : TFunc<Boolean>
);
var
  FService : IFMXVirtualKeyboardService;
begin
  if AKey = vkHardwareBack then
  begin
    if TPlatformServices.Current.SupportsPlatformService(
      IFMXVirtualKeyboardService,
      IInterface(FService)
    ) then
    begin
      if (FService <> nil) and
         (TVirtualKeyboardState.Visible in FService.VirtualKeyBoardState) then
      begin
        // Back button pressed, keyboard visible, so do nothing...
      end
      else
      begin
        if Assigned(AFunc_HandleClose) then
        begin
          if not AFunc_HandleClose then
          begin
            AKey := 0;
          end;
        end;
//        // Back button pressed, keyboard not visible or not supported on this platform, lets exit the app...
//        if MessageDlg('Exit Application?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], -1) = mrOK then
//        begin
//          // Exit application here...
//        end else
//        begin
//          // They changed their mind, so ignore the Back button press...
//          AKey := 0;
//        end;
      end;
    end;
  end;
end;

end.

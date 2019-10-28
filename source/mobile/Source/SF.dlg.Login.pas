unit SF.dlg.Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  SF.lib.SmartFarmAPI, iSolutions.lib.VKKeyboardHelper;

type
  TfrmLogin = class(TForm)
    lytUsername: TLayout;
    edtUsername: TEdit;
    stylbkLogin: TStyleBook;
    vrtscrlbxContent: TVertScrollBox;
    lytContent: TLayout;
    lblAccountLogin: TLabel;
    edtPassword: TEdit;
    rctnglBackground: TRectangle;
    rctnglButtonLogin: TRectangle;
    lblLogin: TLabel;
    imgShowPassword: TImage;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnShowPasswordClick(Sender: TObject);
    procedure edtPasswordKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
        Shift: TShiftState);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FPainted : Boolean;
    FKeyboardHelper : TCSClass_VKKeyboardHelper;
    procedure CheckCredentials;
    function CheckUser(AValue : string) : TCSRec_Station;
    function LoadUser(ASlug : string) : TCSRec_Station;
  public

  end;

var
  frmLogin: TfrmLogin;

implementation

uses
  SF.vwr.Home, iSolution.lib.Configuration, iSolution.lib.Path;

{$R *.fmx}

procedure TfrmLogin.FormDestroy(Sender: TObject);
begin
  FKeyboardHelper.Free;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  FPainted := False;
  FKeyboardHelper := TCSClass_VKKeyboardHelper.Create(Self);
  FKeyboardHelper.ContentScrollBox := vrtscrlbxContent;
  FKeyboardHelper.ContentLayout := lytContent;
end;

{-----------------------------------------------------------------------------
  Procedure : btnLoginClick
  Author    : iwancs
  Date      : 04 Oct 2019
  Arguments : Sender : TObject;

  Result    :
  Desc      :
  Version   :
  Reference :
  CallBy    :
-----------------------------------------------------------------------------}
procedure TfrmLogin.btnLoginClick(Sender: TObject);
var
  usrInfo : TCSRec_User;
begin
//  TCSClass_SmartFarmAPI.Login(
//    'iwan',
//    '123456'
//  );
//  TCSClass_SmartFarmAPI.GetNodeDetail(
//    'tgr123',
//    'qsw345sxP'
//  );

  usrInfo := TCSClass_SmartFarmAPI.Login(
    edtUsername.Text,
    edtPassword.Text
  );

//  usrInfo := CheckUser(edtUsername.Text);
  if not usrInfo.isEmpty then
  begin
    frmHome := TfrmHome.Create(Application);
    frmHome.Show;
    Hide;
  end
  else
  begin
    ShowMessage('User Not Found');
  end;
end;

procedure TfrmLogin.btnShowPasswordClick(Sender: TObject);
begin
  if imgShowPassword.Tag = 0 then
  begin
    edtPassword.Password := False;
    imgShowPassword.Tag := 1;
  end
  else
  begin
    edtPassword.Password := True;
    imgShowPassword.Tag := 0;
  end;
end;

procedure TfrmLogin.CheckCredentials;
var
  usrInfo : TCSRec_Station;
  bShowLogin : Boolean;
  sSlug : string;
begin
  bShowLogin := False;
  sSlug := TCSClass_Configuration.ReadConfig(
    TCSClass_Path.Document('config.sys'),
    'Credentials',
    'slug'
  );
  if sSlug <> '' then
  begin
    usrInfo.Clear;
    usrInfo := LoadUser(sSlug);
    if not usrInfo.isEmpty then
    begin
      frmHome := TfrmHome.Create(Application);
//      frmHome.UserInfo := usrInfo;
      frmHome.Show;
      Hide;
    end
    else
    begin
      bShowLogin := True;
    end;
  end
  else
  begin
    bShowLogin := True;
  end;

  if bShowLogin then
  begin
    TCSClass_Configuration.WriteConfig(
      TCSClass_Path.Document('config.sys'),
      'Credentials',
      'slug',
      ''
    );
    frmLogin.Show;
  end;
end;

function TfrmLogin.CheckUser(AValue: string): TCSRec_Station;
var
  arrStations : TCSRec_Stations;
begin
  Result.Clear;
  arrStations := TCSClass_SmartFarmAPI.GetStationList;
  if length(arrStations) > 0 then
  begin
    for var i := 0 to Length(arrStations) - 1 do
    begin
      if CompareText(arrStations[i].slug, AValue) = 0 then
      begin
        Result := arrStations[i];
        Break;
      end;
    end;
  end;
end;

procedure TfrmLogin.edtPasswordKeyDown(Sender: TObject; var Key: Word; var
    KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    btnLoginClick(lblLogin);
  end;
end;

procedure TfrmLogin.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect:
    TRectF);
begin
  if not FPainted then
  begin
    FPainted := True;
//    CheckCredentials
  end;
end;

function TfrmLogin.LoadUser(ASlug: string): TCSRec_Station;
var
  statInfo : TCSRec_Station;
begin
  statInfo.Clear;
  statInfo := TCSClass_SmartFarmAPI.GetStationDetail(ASlug);
  if not statInfo.isEmpty then
  begin
    Result := statInfo;
  end;
end;

end.

unit cl.Main.Form;

interface
 {$I cl.Calc.Define.inc}
uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  System.TypInfo,
  Vcl.Graphics,
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,  
  Vcl.StdCtrls, 
  Vcl.ComCtrls, 
  Vcl.ExtCtrls,
  Math,
  cl.Calc.Main,
  cl.Main.Types,
  cl.Calc.Scanner,
  cl.Calc.Syntax;

type
  TFormCalc = class(TForm)
    PanelInput: TPanel;
    PanelBottom: TPanel;
    Label1: TLabel;
    MemoResult: TRichEdit;
    PanelExpression: TPanel;
    MemoExpression: TRichEdit;
    PanelExpressionTop: TPanel;
    PanelScannerRun: TPanel;
    PanelRef: TPanel;
    PanelExpressionBottom: TPanel;
    PanelExecute: TPanel;
    PanelSettingOpen: TPanel;
    TimerScanner: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure PanelInputAlignPosition(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
    procedure PanelExecuteClick(Sender: TObject);
    procedure PanelSettingOpenClick(Sender: TObject);
    procedure PanelExecuteMouseEnter(Sender: TObject);
    procedure PanelExecuteMouseLeave(Sender: TObject);
    procedure PanelRefClick(Sender: TObject);
    procedure PanelScannerRunClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerScannerTimer(Sender: TObject);
  private
    { Private declarations }
     FScannerAtimate:TclScanner;
     procedure MemoResultUpdate(ReturnOk:boolean);
     procedure MemoResultSetText(Value:string;ReturnOk:boolean);
     procedure Log(Sender:TObject;Msg:string;E:Exception=nil);
  public
    { Public declarations }
  end;

var
  FormCalc: TFormCalc;

implementation
uses cl.Main.Setting.Form,
     cl.Main.Ref.Form;

{$R *.dfm}


procedure TFormCalc.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(FScannerAtimate);
end;

procedure TFormCalc.FormCreate(Sender: TObject);
begin
   reportmemoryleaksonshutdown:= true;
   MemoResult.Text:='';
   FScannerAtimate:=TclScanner.Create;
   FScannerAtimate.Braskets:=['(','{','[',')','}',']'];
end;

procedure TFormCalc.PanelExecuteClick(Sender: TObject);
var Calc:TclCalc;
 r:double;
begin
    r :=
     (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - - - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    )
     -
    (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - - - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    )

     *
    (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - - - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    )

     - + -
    (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - -( - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    ));

    Calc:= TclCalc.Create;
    Calc.Rule.CanShowError:=  FormCalcSetting.P_CanShowError.Checked;
    Calc.OnLog:= Log;
    try
      if Calc.Execute(MemoExpression.Text) then
       MemoResultSetText(Math.RoundTo(Calc.Return,-8).Tostring,true)
      else
      begin
        MemoExpression.SelStart := Calc.ErrorPos;
        MemoExpression.SelLength := Calc.ErrorLen;
        MemoResultSetText(Calc.ErrorMessage.Replace(#13,' ').Replace(#10,''),false);
      end;
    finally
      Calc.Free;
    end;
end;

procedure TFormCalc.PanelExecuteMouseEnter(Sender: TObject);
begin
   (Sender as TPanel).Color:= $0072842B;
end;

procedure TFormCalc.PanelExecuteMouseLeave(Sender: TObject);
begin

    if Sender = PanelExecute then
    (Sender as TPanel).Color:= $006C7D28
    else
    (Sender as TPanel).Color:= $00412C2E;
end;

procedure TFormCalc.Log(Sender:TObject;Msg:string;E:Exception=nil);
begin
end;



procedure TFormCalc.MemoResultSetText(Value:string;ReturnOk:boolean);
var i:integer;
    ColorFrac:TColor;
begin
   MemoResult.Text:= '';
   MemoResultUpdate(ReturnOk);
   MemoResult.Text:= Value;
   if (FormCalcSetting <> nil)
   and (  FormCalcSetting.P_CanColorReturn.Checked)
   and  ReturnOk then
   begin
      ColorFrac:= ColorConvert.RandomColorLS(180,125);
      Value:= MemoResult.Text;
      if length(Value) > 0 then
      begin
        I:=1;
        while I <= length(Value)  do
        begin
           if CharInSet( Value[i],[',','.']) then
           break;
           MemoResult.SelStart := I-1;
           MemoResult.SelLength := 1;
           MemoResult.SelAttributes.Color := ColorConvert.RandomColorLS(180,125);
           inc(i);
        end;
        MemoResult.SelStart := I;
        MemoResult.SelLength := length(Value)  - I;
        MemoResult.SelAttributes.Color := ColorFrac;
      end;
   end;
   
end;

procedure TFormCalc.MemoResultUpdate(ReturnOk:boolean);
var R:TRect;
    AClientRect:TRect;
    AHeightFont:integer;
    Canvas:TCanvas;
begin
    if ReturnOk then
    begin
     MemoResult.Font.Size:= 20;
     MemoResult.ScrollBars:= ssNone;
     MemoResult.Alignment:= taCenter;
    end
    else
    begin
     MemoResult.Font.Size:= 12;
     MemoResult.ScrollBars:= ssBoth;
     MemoResult.Alignment:= taLeftJustify;
    end;

    Canvas:=TCanvas.Create;
    try
      Canvas.Handle:= GetDC(MemoResult.Handle);
      Canvas.Font:= MemoResult.Font;
      AHeightFont:=Canvas.TextHeight('Yy');
    finally
      Canvas.Free;
    end;
    SendMessage(MemoResult.Handle,EM_GETRECT,0,LParam(@R));
    AClientRect:=  MemoResult.ClientRect;

    if ReturnOk then
    begin
      R.Left:= AClientRect.Left + 30;
      R.Right:= AClientRect.Right - 30;
      R.Top:=  AClientRect.Height div 2 - AHeightFont div 2;
    end
    else
    begin
      R.Left:= AClientRect.Left + 5;
      R.Right:= AClientRect.Right - 5;
      R.Top:=  5;
    end;
    SendMessage(MemoResult.Handle,EM_SETRECT,0,LParam(@R));
end;

procedure TFormCalc.PanelInputAlignPosition(Sender: TWinControl; Control: TControl;
  var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
begin
   if Control <> PanelExpression then
    exit;   
   NewLeft   :=  AlignRect.Width * 5 div 100 ;
   NewTop    :=  AlignRect.Height * 5 div 100 ;
   NewWidth  :=  AlignRect.Width -  NewLeft * 2;
   NewHeight :=  AlignRect.Height -  NewTop * 2;
   
end;

procedure TFormCalc.PanelRefClick(Sender: TObject);
begin
    FormCaclRef.ShowModal;
end;



procedure TFormCalc.PanelSettingOpenClick(Sender: TObject);
begin
   FormCalcSetting.ShowModal;
end;


procedure TFormCalc.PanelScannerRunClick(Sender: TObject);
begin
 if TimerScanner.Enabled  then
 begin
   FScannerAtimate.Reset;
   TimerScanner.Enabled:=false;
   PanelScannerRun.Font.Color:=clwhite;
   PanelScannerRun.Caption:='8';
   MemoResultSetText('',false);
 end
 else
 begin
   FScannerAtimate.Reset;
   FScannerAtimate.Buffer:=MemoExpression.Text;
   TimerScanner.Enabled:=true;
   PanelScannerRun.Font.Color:=$008080FF;
   PanelScannerRun.Caption:='r';
 end;


end;

procedure TFormCalc.TimerScannerTimer(Sender: TObject);
var APos,ALen:integer;
begin
  try
     if not FScannerAtimate.Read then
     begin
       PanelScannerRunClick(TimerScanner);
       exit;
     end;

     if PanelScannerRun.Font.Color <> $0080FF80 then
     PanelScannerRun.Font.Color:= $0080FF80
     else
     PanelScannerRun.Font.Color:= $008080FF;

     MemoResultSetText(FScannerAtimate.CurText,false);
     MemoResult.Lines.Add(GetEnumName(TypeInfo(Syntax.TEnumToken), Ord(FScannerAtimate.Caret.TokenClass)));
     FScannerAtimate.PosToRich(APos,ALen);
     MemoExpression.SelStart:= APos;
     MemoExpression.SelLength:= ALen;
  except
    on e: Exception do
    begin
        FScannerAtimate.PosToRich(APos,ALen);
        PanelScannerRunClick(TimerScanner);
        MemoExpression.SelStart := APos;
        MemoExpression.SelLength := ALen;
        MemoResultSetText(e.Message.Replace(#13,' ').Replace(#10,''),false);
    end;
  end;
end;


    {
    s:= cos(PI/3);
    d:= Power(2,(5+2));

    d:= s + min( 10 , 5.5) * ((sin(Pi/6)+ -+ 42)* -d-- - (3 shr 1) ) ;






    r:= ((30) or -1)+2;
    r:= (30) or -1+2;
     r:= (30) or -((1+2));
     r:= (30) or (-1+2);
    //r:= ((12-5)*3 * 40/(2 + (3-(12+( (22-4)*2)/2)) / (7+44.32)));
    r:=  -  00-- ((-12- - + - 5)* - + 3 * ++40/ ---(2 + (-3-(-12+( (22-4)*2)/2)) / (7+- 44.32)));
    r:=  -- 24 /  - (5+1) * 2;;
    //r:=strtofloat('--2');

    r :=
     (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - - - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    )
     -
    (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - - - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    )

     *
    (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - - - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    )

     - + -
    (
      2 + 3+ 4  +5 + 1.2+ 11.5+ 0.77+ - 1 -2 - 3 - 4.5 - 6.6 - + - 2 + - 3 - + 4 +
      2 + (3+ 4)  +(5 + 1.2)+ (11.5+ (0.77+ - 1)) -2 - (3 - (4.5 - 6.6) - + - 2) + - 3 - + 4 -
      2 /  - -( - (3 -  4)  +(5 / 1.2)+ (11.5+ -(0.77* - 1)) -2 - (3*2 - (4.5 - 6.6) -2 * - 2) + - (3 / + 4)
    ));


    r:= ---  -  +  - 93-  2+4*+3/-24 - 52 * 53 - - 3/ - ( 123 + 88 / 33);

    }

end.

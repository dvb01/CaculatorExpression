unit cl.Calc.Exception;

interface
{$I cl.Calc.Define.inc}
uses
  System.SysUtils,
  System.Classes,
  cl.Calc.Types,
  cl.Calc.Res;

  type



  TclErrorPrmItem = record
    Msg: PResStringRec;
    Args:  array of string;
    procedure Clear;
    procedure SetterArgs(Value:array of string);
  end;
  PclErrorPrmItem = ^TclErrorPrmItem;



  TclErrorPrm = record
    IsBeginStr:boolean;
    SingleLn:boolean;
    Place: string;
    Rec:TArray<TclErrorPrmItem>;
    Str:TArray<string>;
    function ToString:string;
    procedure Clear;
  end;
  PclErrorPrm = ^TclErrorPrm;

  ExceptionCaclClass = class of ExceptionCaclCustom;

  ExceptionCaclCustom = class (Exception)
  public
    constructor CreateCalc(Msg:string);virtual;
  end;
  ExceptionCaclCompiler = class (ExceptionCaclCustom);
  ExceptionCaclParser = class (ExceptionCaclCompiler);
  ExceptionCaclRunner = class (ExceptionCaclCompiler);
  ExceptionCaclScanner = class (ExceptionCaclCustom);
  ExceptionCaclCalc = class (ExceptionCaclCustom);

  clErrorFactory  = class
  private
      class procedure CustomCreate(AClass:ExceptionCaclClass;Prm:PclErrorPrm);static;
      class function SenderToExceptionClass(Sender:TObject): ExceptionCaclClass;static;
      class procedure CustomSenderNil(Place,FerstMsg:string);
      class procedure ScannerToArgs(Scanner:TObject;out ATextWindow,ATextCur:string;out APos,ALen:integer); static;
      class procedure Common(Sender:TObject;const Place,FerstMsg:string;AMsg : PResStringRec; const Args : array of string); static;
  public
      class procedure Compiler(Sender:TObject;const Place,FerstMsg:string; AMsg : PResStringRec;const Args : array of string);  static;
      class procedure Scanner(Sender:TObject;const Place,FerstMsg:string;AMsg : PResStringRec;const Args : array of string); static;
  end;


implementation
 uses
   cl.Calc.Compiler.Types,
   cl.Calc.Compiler.Parser,
   cl.Calc.Compiler.Runner,
   cl.Calc.Scanner,
   cl.Calc.Main;

   type
    TLocCompiler = class (TclCompilerCustom);

{ TclErrorPrm }

procedure TclErrorPrm.Clear;
begin
  RecHlp.Clear(self);
end;

function TclErrorPrm.ToString: string;
var S:TStringBuilder;
   procedure  LocAppend(P:PResStringRec;Arguments:array of string);
   var Arr: array of TVarRec;
       I:integer;
   begin
      if P = nil  then
        exit;
       Setlength(Arr,length(Arguments));
       for I := 0 to length(Arguments)-1 do
       begin
         Arr[i].VType := vtUnicodeString;
         Arr[i].VUnicodeString := Pointer(Arguments[i]);
       end;
      if SingleLn then
        S.Append(' ').Append(Format(LoadResString(P), Arr))
      else
        S.Append(#13#10).Append(Format(LoadResString(P), Arr))
   end;
   procedure LocRec;
   var i:integer;
   begin
      for I := 0 to length(Rec)-1 do
      if SingleLn then
        LocAppend(Rec[i].Msg, Rec[i].Args)
      else LocAppend(Rec[i].Msg, Rec[i].Args)
   end;
   procedure LocStr;
   var i:integer;
   begin
      for I := 0 to length(Str)-1 do
      if SingleLn then
      S.Append(' ').Append(Str[i])
      else
      S.Append(#13#10).Append(Str[i])
   end;
begin
    S:=TStringBuilder.Create;
    try
       S.Append('Error');
       if Place <> '' then
       S.Append(' '+Place);

       if IsBeginStr then
       begin
           LocStr;
           LocRec;
       end
       else
       begin
          LocRec;
          LocStr;
       end;
       Result:= S.ToString;
    finally
      S.Free;
    end;
end;

{ TclErrorPrmItem }

procedure TclErrorPrmItem.Clear;
begin
    RecHlp.Clear(self);
end;

procedure TclErrorPrmItem.SetterArgs(Value: array of string);
var i:integer;
begin
    SetLength(Args,length(Value));
    for I := 0 to length(Value)-1 do
        Args[i]:= Value[i];
end;

{ ExceptionCaclCustom }

constructor ExceptionCaclCustom.CreateCalc(Msg:string);
begin
    if Msg <>'' then inherited Create(Msg)
    else inherited CreateResFmt(@Rs_CaclDefault,['unknown']);
end;

   { clErrorFactory }
class procedure clErrorFactory.CustomCreate(AClass:ExceptionCaclClass;Prm:PclErrorPrm);
begin
  if AClass = nil then
  AClass:=  ExceptionCaclCustom;
  if Prm = nil then
      raise AClass.CreateCalc('')
  else
  raise AClass.CreateCalc(Prm.ToString);
end;

class procedure clErrorFactory.CustomSenderNil(Place,FerstMsg:string);
var Prm:TclErrorPrm;
begin
    Prm.Clear;
    Prm.IsBeginStr:=true;
    Prm.SingleLn:=true;
    Prm.Place:=Place;
    Prm.Str:=[FerstMsg];
    CustomCreate(nil,@Prm);
end;

class function clErrorFactory.SenderToExceptionClass(Sender: TObject): ExceptionCaclClass;
begin
    if Sender = nil then
        Result:= nil
    else if Sender is TclParser then
        Result:= ExceptionCaclParser
    else if Sender is TclRunner then
        Result:= ExceptionCaclRunner
    else if Sender is TclScanner then
        Result:= ExceptionCaclScanner
    else if Sender is TclCalcCustom then
        Result:= ExceptionCaclCalc
    else
       Result:=nil
end;


class procedure clErrorFactory.ScannerToArgs(Scanner:TObject;out ATextWindow,ATextCur:string;out APos,ALen:integer);
begin
     ATextWindow:='';
     ATextCur:='';
     APos:=0;
     ALen:=0;
     if (Scanner <> nil) and (Scanner is TclScanner) then
      TclScanner(Scanner).GetPrmForError(ATextWindow,ATextCur,APos,ALen);
end;

class procedure clErrorFactory.Common(Sender:TObject;const Place,FerstMsg:string;
                                     AMsg : PResStringRec;const Args : array of string);
var Prm:TclErrorPrm;
    Item1,Item2:TclErrorPrmItem;
    function LocGetPlaceFull:string;
    var PlaceType:string;
    begin
      if Sender = nil then
       PlaceType:= 'Unknown'
      else
        PlaceType:= Sender.ClassName;

      if Place <> '' then
       Result:= PlaceType + '.' + Place
     else
       Result:= PlaceType;

    end;

    procedure LocItem2Setter;
    var
      ATextWindow,ATextCur:string;
      APos,ALen:integer;
      Scan:TclScanner;
    begin
      if Sender is TclCompilerCustom then
        Scan:= TLocCompiler(Sender).Scanner
      else if Sender is TclScanner then
        Scan:= TclScanner(Sender)
      else
       exit;
      ScannerToArgs(Scan,ATextWindow,ATextCur,APos,ALen);
      Item2.Msg:= @Rs_clInvalidToken;
      Item2.Args:=[ATextWindow,ATextCur,APos.ToString,ALen.ToString];
    end;
begin

  Prm.Clear;
  Prm.IsBeginStr:=true;
  Prm.SingleLn:=true;
  Item1.Clear;
  Item2.Clear;

  if Sender = nil then
  begin
     CustomSenderNil(Place,FerstMsg);
     exit;
  end;

  Item1.Msg:= AMsg;
  Item1.SetterArgs(Args);
  LocItem2Setter;
  Prm.Place:= LocGetPlaceFull;
  Prm.Rec:= [Item1,Item2];
  Prm.Str:=[FerstMsg];
  CustomCreate(SenderToExceptionClass(Sender),@Prm);
end;



class procedure clErrorFactory.Compiler(Sender:TObject;const Place,FerstMsg:string;AMsg : PResStringRec;const Args : array of string );
begin
    Common(Sender,Place,FerstMsg,AMsg,Args);
end;

class procedure clErrorFactory.Scanner(Sender: TObject;const Place, FerstMsg: string; AMsg: PResStringRec;const Args: array of string );
begin
    Common(Sender,Place,FerstMsg,AMsg,Args);
end;




end.

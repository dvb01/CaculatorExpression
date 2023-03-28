unit cl.Calc.Scanner;

interface

{$I cl.Calc.Define.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Character,
  Math,
  System.Generics.Collections,
  cl.Calc.Types,
  cl.Calc.Utils,
  cl.Calc.Syntax,
  cl.Calc.Res,
  cl.Calc.Exception;

type

  PclCaret = ^TclCaret;

  TclCaret = record
  public
    WindowPosStart, WindowPosStop: integer;
    TokenPosStart: integer;
    TokenPosStop: integer;
    TokenLength: integer;
    TokenClass: Syntax.TEnumToken;
    procedure Clear;
    procedure Reset;
    procedure Select;
    procedure AddCaret(P: PclCaret; ReplaceClass: boolean = false);
    procedure AddCaretStep(P: PclCaret);
    procedure JmpToEnd;
    procedure JmpToBegin;
    function IsValidWindow(Len: integer = -2): boolean;
    function IsValidToken: boolean;
    function IsValid(Len: integer = -2): boolean;
    procedure CreateError;
    procedure CheckError(Len: integer = -2);
  end;

  TclScanner = class(TclObject)
  private
    FBuffer: string;
    FCaret: TclCaret;
    FStack: TStack<TclCaret>;
    FBraskets: TSetOfChar;

    function CurCharGet: Char;
    function CurTextFullGet: string;
    function CurTextGet: string;
    function IsEofGet: boolean;
    function CaretGet: PclCaret;
    procedure BufferSet(const Value: string);
    function CurTextWindowGet: string;
  protected
    function CaretIsValid: boolean;
    procedure CaretCheckError;

    function Sh(Index: integer): Char;
    procedure ErrorScanner();
  public
    function ReadInc: boolean;
    function ReadDec: boolean;
    procedure ReadJmpToEnd;
    function Read: boolean;
    procedure Push;
    procedure Pop;
    procedure Reset;
    procedure Match(const Value: string);

    property Buffer: string read FBuffer write BufferSet;

    property Braskets: TSetOfChar read FBraskets write FBraskets;

    function Eof: boolean;
    property IsEof: boolean read IsEofGet;

    property Caret: PclCaret read CaretGet;
    property CurText: string read CurTextGet;
    property CurTextFull: string read CurTextFullGet;
    // вернет область окна ограниченную 15 - 20 символами в каждую сторону
    property CurTextWindow: string read CurTextWindowGet;
    function GetCurTextWindow(CountCharLeftRigth: integer = 10): string;

    property CurChar: Char read CurCharGet;
    procedure CurReset;
    procedure CurSelect;
    procedure CurUpdate(WindowPosStart, WindowPosStop: integer);
    procedure CurUpdateSelect;

    function IsCurToken(AChar: Char): boolean; overload;
    function IsCurToken(AChars: TSetOfChar): boolean; overload;
    function IsCurToken(const Str: string): boolean; overload;
    function IsCurToken(const Strs: array of string; Index: PInteger)
      : boolean; overload;
    function IsCurToken(AChars: TCharArray; Index: PInteger): boolean; overload;

    procedure PosToRich(out APos, ALen: integer);
    procedure GetPrmForError(var ATextWindow, ATextCur: string;
      var APos, ALen: integer);

    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TclCaret }

procedure TclCaret.AddCaret(P: PclCaret; ReplaceClass: boolean = false);
begin
  if ReplaceClass then
    TokenClass := P.TokenClass;
  WindowPosStart := max(1, min(TokenPosStart, P.WindowPosStart));
  WindowPosStop := max(1, max(TokenPosStop, P.WindowPosStop));
  Select;
  CheckError;
end;

procedure TclCaret.AddCaretStep(P: PclCaret);
begin
  WindowPosStart := max(1, min(WindowPosStart, P.TokenPosStart));
  WindowPosStop := max(1, max(WindowPosStop, P.TokenPosStop));
  Select;
  CheckError;
end;

procedure TclCaret.Clear;
begin
  fillchar(self, sizeof(self), 0);
end;

procedure TclCaret.CheckError(Len: integer = -2);
begin
  if not IsValid(Len) then
    CreateError;
end;

procedure TclCaret.CreateError;
begin
  raise Exception.CreateResFmt(@Rs_TclCaret_Invalid, []);
end;

function TclCaret.IsValid(Len: integer = -2): boolean;
begin
  Result := IsValidWindow(Len) and IsValidToken;
end;

function TclCaret.IsValidToken: boolean;
begin
  Result := (TokenPosStart >= WindowPosStart) and
    (TokenPosStart <= WindowPosStop) and
    (TokenPosStart + TokenLength - 1 = TokenPosStop) and
    (TokenPosStart + TokenLength - 1 <= WindowPosStop);
end;

function TclCaret.IsValidWindow(Len: integer = -2): boolean;
begin
  if Len = -2 then
  begin
    Result := (WindowPosStart <= WindowPosStop);
  end
  else
  begin
    Result := (Len > 0) and (WindowPosStart > 0) and (WindowPosStop <= Len) and
      (WindowPosStart <= WindowPosStop);
  end;

end;

procedure TclCaret.JmpToEnd;
begin
  TokenPosStop := WindowPosStop;
  TokenPosStart := TokenPosStop;
  TokenLength := 1;
  CheckError;
end;

procedure TclCaret.JmpToBegin;
begin
  TokenPosStop := WindowPosStart;
  TokenPosStart := TokenPosStop;
  TokenLength := 1;
  CheckError;
end;

procedure TclCaret.Reset;
begin
  TokenPosStop := WindowPosStart - 1;
  TokenPosStart := WindowPosStart - 1;
  TokenLength := 0;
end;

procedure TclCaret.Select;
begin
  TokenPosStart := WindowPosStart;
  TokenPosStop := WindowPosStop;
  TokenLength := WindowPosStop - WindowPosStart + 1;
end;

{ TclScanner }

constructor TclScanner.Create;
begin
  inherited;
  FBraskets := ['(', ')'];
  FBuffer := '';
  FStack := TStack<TclCaret>.Create;
  FStack.Capacity := 100;
  FCaret.Clear;
  CurUpdate(0, 0);

end;

destructor TclScanner.Destroy;
begin
  FCaret.Clear;
  FreeAndNil(FStack);
  FBuffer := '';
  inherited;
end;

procedure TclScanner.ErrorScanner();
begin
  clErrorFactory.Scanner(self, '', '', nil, []);
end;

procedure TclScanner.BufferSet(const Value: string);
begin
  if FBuffer <> Value then
  begin
    if FStack.Count > 0 then
      raise Exception.CreateResFmt(@Rs_TclScanner_BufferSet, []);
    FBuffer := Value;
    CurUpdate(1, Length(FBuffer));
  end;
end;

function TclScanner.CaretGet: PclCaret;
begin
  Result := @FCaret;
end;

function TclScanner.CaretIsValid: boolean;
begin
  Result := Caret.IsValid(Length(FBuffer));
end;

procedure TclScanner.CaretCheckError;
begin
  Caret.CheckError(Length(FBuffer));
end;

procedure TclScanner.CurReset;
begin
  FCaret.Reset;
end;

procedure TclScanner.CurUpdate(WindowPosStart, WindowPosStop: integer);
begin
  if Length(FBuffer) <= 0 then
  begin
    FCaret.Clear;
    exit;
  end;
  FCaret.WindowPosStart := WindowPosStart;
  FCaret.WindowPosStop := WindowPosStop;
  if not Caret.IsValidWindow(Length(FBuffer)) then
    raise Exception.CreateResFmt(@Rs_TclScanner_CurUpdate, []);
  CurReset;
end;

procedure TclScanner.CurUpdateSelect;
begin
  CurUpdate(FCaret.TokenPosStart, FCaret.TokenPosStop);
  CurSelect;
end;

procedure TclScanner.CurSelect;
begin
  FCaret.Select;
end;

function TclScanner.CurCharGet: Char;
begin
  Result := Sh(0);
end;

function TclScanner.CurTextFullGet: string;
begin
  CaretCheckError;
  Result := Copy(FBuffer, FCaret.WindowPosStart, FCaret.WindowPosStop -
    FCaret.WindowPosStart + 1);
end;

function TclScanner.CurTextGet: string;
begin
  CaretCheckError;
  Result := Copy(FBuffer, FCaret.TokenPosStart, FCaret.TokenLength);
end;

function TclScanner.CurTextWindowGet: string;
begin
  Result := GetCurTextWindow(10);
end;

function TclScanner.GetCurTextWindow(CountCharLeftRigth: integer = 10): string;
var
  i: integer;
begin
  i := CountCharLeftRigth;
  if i < 1 then
    i := 1;
  if i > 200 then
    i := 200;
  Push;
  try
    Caret.WindowPosStart := max(Caret.WindowPosStart, Caret.TokenPosStart - i);
    Caret.WindowPosStop := min(Caret.WindowPosStart + i * 2,
      Caret.WindowPosStop);
    Caret.Select;
    Result := CurText;
  finally
    Pop;
  end;
end;

procedure TclScanner.GetPrmForError(var ATextWindow, ATextCur: string;
  var APos, ALen: integer);
var
  ef: boolean;
  procedure Loc;
  begin
    ATextWindow := GetCurTextWindow(10);
    ATextCur := CurText;
    APos := Caret.TokenPosStart;
    ALen := Caret.TokenLength;
  end;

begin
  ef := Eof;
  if ef then
  begin
    Push;
    try
      ReadJmpToEnd;
      Loc;
    finally
      Pop;
    end;
  end
  else
    Loc;
end;

function TclScanner.IsEofGet: boolean;
begin
  CaretCheckError;
  Result := Eof;
end;

procedure TclScanner.Pop;
begin
  FCaret := FStack.Pop;
end;

procedure TclScanner.Push;
begin
  FStack.Push(FCaret);
end;

procedure TclScanner.Reset;
begin
  FStack.Clear;
  BufferSet('');
end;

function TclScanner.Sh(Index: integer): Char;
begin
  Result := FBuffer[FCaret.TokenPosStop + Index];
end;

function TclScanner.Eof: boolean;
begin
  Result := not((FCaret.TokenPosStop <= FCaret.WindowPosStop) and
    (FCaret.TokenPosStop > 0));
end;

function TclScanner.ReadInc: boolean;
begin
  inc(FCaret.TokenPosStop);
  Result := not Eof;
end;

procedure TclScanner.ReadJmpToEnd;
begin
  FCaret.JmpToEnd;
end;

function TclScanner.ReadDec: boolean;
begin
  Dec(FCaret.TokenPosStop);
  Result := not Eof;
end;

procedure TclScanner.Match(const Value: string);
begin
  if not IsCurToken(Value) then
    self.ErrorScanner;
end;

function TclScanner.IsCurToken(AChar: Char): boolean;
var
  S: TSetOfChar;
begin
  S := [AChar];
  Result := IsCurToken(S);
end;

function TclScanner.IsCurToken(AChars: TSetOfChar): boolean;
begin
  Result := IsInChars(CurChar, AChars);
end;

function TclScanner.IsCurToken(const Str: string): boolean;
begin
  Result := CmpStr(CurText, Str);
end;

function TclScanner.IsCurToken(const Strs: array of string;
  Index: PInteger): boolean;
var
  i: integer;
begin
  for i := Low(Strs) to High(Strs) do
    if IsCurToken(Strs[i]) then
    begin
      if Assigned(Index) then
        Index^ := i;
      Result := true;
      exit;
    end;
  if Assigned(Index) then
    Index^ := -1;
  Result := false;
end;

function TclScanner.IsCurToken(AChars: TCharArray; Index: PInteger): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to Length(AChars) - 1 do
    if AChars[i] = CurChar then
    begin
      Result := true;
      if Assigned(Index) then
        Index^ := i;
      exit;
    end;
  if Assigned(Index) then
    Index^ := -1;
end;

procedure TclScanner.PosToRich(out APos, ALen: integer);
begin
  if Length(FBuffer) <= 0 then
  begin
    APos := 0;
    ALen := 0;
    exit();
  end;

  self.Push;
  try
    APos := Caret.TokenPosStart;
    ALen := Caret.TokenLength;
    CurUpdate(1, Length(FBuffer));
    Caret.TokenPosStop := APos;
    Dec(APos);
    repeat
      if CharInSet(CurChar, [#13]) then
        Dec(APos);
    until not ReadDec;
  finally
    self.Pop;
  end;
end;

function TclScanner.Read: boolean;
var
  c: Char;
  procedure LocUpdateLen;
  begin
    FCaret.TokenLength := FCaret.TokenPosStop - FCaret.TokenPosStart + 1;
  end;

  procedure LocParsTokenLiteral;
  begin
    while IsLiteral(Sh(1)) or IsNumber(Sh(1)) do
      ReadInc;
    LocUpdateLen;
    FCaret.TokenClass := tcLiteral;
  end;

  procedure LocParsTokenNumberWhole;
  begin
    while IsNumber(Sh(1)) do
      ReadInc;
  end;

  procedure LocParsTokenHexNumberWhole;
  begin
    while IsHexNumber(Sh(1)) do
      ReadInc;
  end;

  procedure LocTokenNumber;
  begin
    LocParsTokenNumberWhole;
    FCaret.TokenClass := tcNumber;

    if (Sh(1) = '.') and (Sh(2) <> '.') and (not IsLiteralEx(Sh(2))) then
    begin
      ReadInc;

      if IsNumber(Sh(1)) then
        LocParsTokenNumberWhole;

      if IsInChars(Sh(1), ['e', 'E']) then
      begin
        ReadInc;
        if IsInChars(Sh(1), ['+', '-']) then
          ReadInc;
        LocParsTokenNumberWhole();

      end;
      // double

    end
    else if IsInChars(Sh(1), ['e', 'E']) and
      IsInChars(Sh(2), ['0' .. '9', '+', '-']) then
    begin
      ReadInc;
      if IsInChars(Sh(1), ['+', '-']) then
        ReadInc;
      LocParsTokenNumberWhole;
      // double
    end;
    LocUpdateLen;
  end;

  procedure LocTokenHexNumber;
  begin

    ReadInc;
    LocParsTokenHexNumberWhole;
    FCaret.TokenClass := tcHexNumber;
    LocUpdateLen;
  end;

  procedure LocParsTokenSpecifical;
  begin
    FCaret.TokenClass := tcSpecifical;
  end;

  procedure LocParsTokenOperator;
  begin
    FCaret.TokenClass := tcOperator;
  end;

begin
  FCaret.TokenClass := tcNone;
  FCaret.TokenPosStart := FCaret.TokenPosStop;
  repeat
    FCaret.TokenLength := 0;
    Result := ReadInc;
    if not Result then
      exit;
    FCaret.TokenPosStart := FCaret.TokenPosStop;
    LocUpdateLen;
    c := Sh(0);
    CurText;

    if IsInChars(c, Syntax.Spaces) then
      continue
    else if c = #13 then
    begin
      if Sh(1) = #10 then
      begin
        Result := ReadInc;
        if not Result then
          exit;
      end;
    end
    else if c = #0 then
    begin
      exit(false);
    end
    else if IsLiteral(c) then
    begin
      LocParsTokenLiteral;

      if IsCurToken(Syntax.sopMatOperators, nil) then
      begin
        LocParsTokenOperator;
      end;
    end
    else if IsNumber(c) then
    begin
      LocTokenNumber
    end
    else if c = '$' then
    begin
      LocTokenHexNumber
    end
    else if (c = ':') and (Sh(1) = '=') then
    begin
      ReadInc;
      LocUpdateLen;
      FCaret.TokenClass := tcAssing;
    end
    else if IsCurToken(Syntax.sopMatOperators, nil) then
    begin
      LocParsTokenOperator;
    end
    else if IsInChars(c, Syntax.Separators) then
    begin
      FCaret.TokenClass := tcSeparator;
    end
    else if IsInChars(c, FBraskets) or IsInChars(c, Syntax.FuncBrasket) then
    begin
      FCaret.TokenClass := tcBrasket;
    end
    else
    begin
      ErrorScanner();
    end;

  until FCaret.TokenClass <> tcNone;
end;

end.

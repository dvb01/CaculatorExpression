unit cl.Calc.Token_old;

interface
uses
  System.SysUtils,
  System.Classes,
  System.Character,
  System.Generics.Collections,
  cl.Calc.Types,
  cl.Calc.Utils,
  cl.Calc.Syntax,
  cl.Calc.Res;

  type
  // токен. текущее положение каретки в тексте выражения
  // имеет поведение стека, можно пушить положение каретки
  // текст хранится в 1 экземпляле
  // от класса к классу менятся только положение каретки и передается эксемпляр TclToken
  TclToken = class(TclObject)
  type
    TPrm = record
      PosBegin: integer;
      PosEnd: integer;
      Pos: integer;
      Length: integer;
      function GetPosEndAtLength:integer;
      procedure Clear;
      procedure SelectAll;
    end;
    PPrm = ^TPrm;
  private
  var
    FText: string;
    FCur: TPrm;
    FStack: TStack<TPrm>;
    function CurTextFullGet: string;
    function CurCharGet: Char; inline;
    function CurGet: PPrm; inline;
    function TextLengthGet: integer; inline;
    procedure TextSet(const Value: string);
    function UpperStr(Value:string):string;
    function StrEql(const S1, S2: String): Boolean;
    function CharEql(const S1, S2: Char): Boolean;
    function CurTextGet: string;
  protected
    procedure Reset;
//    function ReadCustom: Boolean;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Push;
    procedure Pop;
    function ReadInc: Boolean;
    function ReadDec: Boolean;
    function ReadNext: Boolean;overload;
    function ReadPred: Boolean;overload;
    function ReadNext(AChars: TSetOfChar): Boolean;overload;
    function ReadPred(AChars: TSetOfChar): Boolean;overload;




    function IsEof: Boolean;
    function IsCurToken(AChar: Char): Boolean; overload;
    function IsCurToken(AChars: TSetOfChar): Boolean; overload;
    function IsCurToken(AChars: TCharArray; Index: PInteger) : Boolean; overload;
    function IsCurToken(const Str:string;IsRevers:boolean) : Boolean;  overload;
    function IsCurToken(const Strs: array of string;IsRevers:boolean; Index: PInteger) : Boolean;  overload;

    // следующий токен не содердит переданные
    function IsNextTokenNot(AChars: TSetOfChar):boolean;
    function IsPredTokenNot(AChars: TSetOfChar):boolean;

    procedure CurReset;
    procedure CurSelectAll;
    procedure CurUpdate(PosBegin, PosEnd: integer);
    procedure CurUpdateAtSelect;
    property Cur: PPrm read CurGet;
    property CurTextFull: string read CurTextFullGet;
    property CurText: string read CurTextGet;
    property CurChar: Char read CurCharGet;
    property TextLength: integer read TextLengthGet;
    property Text: string read FText write TextSet;
    procedure CurPosToRich(out APos, ALen: integer);
  end;

implementation


{ TclToken }

constructor TclToken.Create();
begin
  inherited Create;
  FStack := TStack<TPrm>.Create;
  FStack.Capacity := 100;
  FText := '';
  CurUpdate(0, 0);
end;

destructor TclToken.Destroy;
begin
  FreeAndNil(FStack);
  FText := '';
  inherited;
end;

procedure TclToken.Reset;
begin
  FStack.Clear;
  TextSet('');
end;

function TclToken.CurCharGet: Char;
begin
  Result := FText[FCur.Pos];
end;

function TclToken.CurGet: PPrm;
begin
  Result := @FCur;
end;

function TclToken.CurTextFullGet: string;
begin
  Result := Copy(FText, FCur.PosBegin, FCur.PosEnd - FCur.PosBegin + 1);
end;

function TclToken.CurTextGet: string;
begin
    Result := Copy(FText, FCur.Pos, FCur.Length);
end;

procedure TclToken.Pop;
begin
  FCur := FStack.Pop;
end;

procedure TclToken.Push;
begin
  FStack.Push(FCur);
end;

procedure TclToken.CurReset;
begin
  FCur.Pos := FCur.PosBegin - 1;
  FCur.Length := 1;
end;

procedure TclToken.CurSelectAll;
begin
  FCur.SelectAll;
end;

procedure TclToken.CurUpdate(PosBegin, PosEnd: integer);
begin
  if Length(FText) <= 0 then
  begin
    FCur.PosBegin := 0;
    FCur.PosEnd := 0;
    CurReset;
    FCur.Length := 0;
    exit;
  end;
  if (PosBegin < 0) or (PosBegin > Length(FText)) or (PosBegin > PosEnd) then
    raise Exception.CreateResFmt(@Rs_TclToken_CurUpdate,
      ['PosBegin', PosBegin.ToString]);

  if (PosEnd < 0) or (PosEnd > Length(FText)) then
    raise Exception.CreateResFmt(@Rs_TclToken_CurUpdate,
      ['PosEnd', PosEnd.ToString]);

  FCur.PosBegin := PosBegin;
  FCur.PosEnd := PosEnd;
  CurReset;
end;

procedure TclToken.CurUpdateAtSelect;
begin
 CurUpdate(Cur.Pos, Cur.Pos + Cur.Length-1);
end;

function TclToken.ReadInc: Boolean;
begin
  inc(FCur.Pos);
  Result := (FCur.Pos <= FCur.PosEnd) and (FCur.Pos > 0);
end;

function TclToken.ReadDec: Boolean;
begin
  Dec(FCur.Pos);
  Result := (FCur.Pos <= FCur.PosEnd) and (FCur.Pos > 0);
end;

function TclToken.TextLengthGet: integer;
begin
  Result := Length(FText);
end;

procedure TclToken.TextSet(const Value: string);
begin
  if FText <> Value then
  begin
    if FStack.Count > 0 then
      raise Exception.CreateResFmt(@Rs_TclToken_TextSet, []);
    FText := Value;
    CurUpdate(1, Length(FText));
  end;
end;

function TclToken.IsEof: Boolean;
begin
  Result := not((FCur.Pos <= FCur.PosEnd) and (FCur.Pos > 0));
end;

function TclToken.UpperStr(Value:string):string;
begin
   Result:= AnsiUpperCase(Value);
end;

function TclToken.StrEql(const S1, S2: String): Boolean;
begin
  result := CompareText(S1, S2) = 0;
end;

function TclToken.CharEql(const S1, S2: Char): Boolean;
begin
   Result:=  S1.ToUpper =  S2.ToUpper;
end;

{
  procedure TclToken.Match(AChar: Char);
  begin
  Match([AChar]);
  end;

  procedure TclToken.Match(AChars: TSetOfChar);
  begin
  while ReadNextChar do
  begin
  if CharInSet(CurChar, AChars) then
  exit
  else if not CharInSet(CurChar, [#32, #13, #10]) then
  raise Exception.Create('Error TclToken.Match не валидное выражение');
  end;
  end;
}



function TclToken.ReadNext(AChars: TSetOfChar): Boolean;
begin
  Result := false;
  while ReadNext do
    if CharInSet(CurChar, AChars) then
      exit(true);
end;

function TclToken.ReadPred(AChars: TSetOfChar): Boolean;
begin
  Result := false;
  while ReadPred do
    if CharInSet(CurChar, AChars) then
      exit(true);
end;

function TclToken.ReadNext: Boolean;
begin
  Result := false;
  while ReadInc do
    if not CharInSet(CurChar, [#32, #13, #10]) then
      exit(true);
end;

function TclToken.ReadPred: Boolean;
begin
  Result := false;
  while ReadDec do
    if not CharInSet(CurChar, [#32, #13, #10]) then
      exit(true);
end;

function TclToken.IsCurToken(AChar: Char): Boolean;
var S:TSetOfChar;
begin
  S:= [AChar];
  Result := IsCurToken(S);
end;

function TclToken.IsCurToken(AChars: TSetOfChar): Boolean;
begin
  Result := CharInSet(CurChar, AChars);
end;

function TclToken.IsCurToken(AChars: TCharArray; Index: PInteger)
  : Boolean;
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

function TclToken.IsCurToken(const Str:string;IsRevers:boolean) : Boolean;
var i,c,PosEnd:integer;
begin
   Result:=false;
   PosEnd:=-1;
   if  (FCur.Pos < 0) or (FCur.Pos <  FCur.PosBegin) or IsEof then
    raise Exception.Create('Error TclToken.IsCurToken string не инициализирован Token');
   try
     Push;
     try
          if not IsRevers then
          begin
                i:=1;
                c:=  length(Str);
                if c <=0 then
                 exit;
                repeat
                  if  not  CharEql(CurChar,Str[i])   then
                   exit
                  else if  ( i >= c) then
                  begin
                    Result:= IsNextTokenNot(['a'..'z','0'..'9','а'..'я','A'..'Z','А'..'Я']);
                    if Result  then
                    PosEnd:= Cur.Pos;
                    exit();
                  end;
                  inc(I);
                until not ReadInc;
          end
          else
          begin
                i:=length(Str);
                c:=  1;
                if i <=0 then
                 exit;
                repeat
                  if  not  CharEql(CurChar,Str[i])   then
                   exit
                  else if  ( i <= c) then
                  begin
                    Result:= IsPredTokenNot(['a'..'z','а'..'я','A'..'Z','А'..'Я']);
                    if Result  then
                    PosEnd:= Cur.Pos;
                    exit();
                  end;
                  dec(I);
                until not ReadDec;
          end;
     finally
        Pop;
     end;
   finally
     if Result  then
      Cur.Length:= PosEnd - Cur.Pos +1;
   end;
end;

function TclToken.IsCurToken(const Strs: array of string;IsRevers:boolean;  Index: PInteger) : Boolean;
var i:integer;
begin
    Result:=false;
    for I := Low(Strs) to High(Strs) do
    if IsCurToken(Strs[i],IsRevers) then
    begin
       if Assigned(Index) then
       Index^:= i;
       Result:=true;
       exit;
    end;
    if Assigned(Index) then
    Index^:= -1;
    Result:=false;
end;

function TclToken.IsNextTokenNot(AChars: TSetOfChar):boolean;
begin
   Push;
   try
    Result := true;
    if  ReadInc and CharInSet(CurChar, AChars) then
        exit(false);
   finally
     Pop;
   end;
end;

function TclToken.IsPredTokenNot(AChars: TSetOfChar):boolean;
begin
   Push;
   try
    Result := true;
    if  ReadDec and CharInSet(CurChar, AChars) then
        exit(false);
   finally
     Pop;
   end;
end;

procedure TclToken.CurPosToRich(out APos, ALen: integer);
begin
  if self.TextLength <=0 then
  begin
   APos:=0;
   ALen:=0;
   exit();
  end;

  self.Push;
  try
    APos := Cur.Pos;
    ALen := Cur.Length;
    CurUpdate(1, Length(FText));
    FCur.Pos := APos;
    Dec(APos);
    repeat
      if CharInSet(CurChar, [#13]) then
        Dec(APos);
    until not ReadDec;
  finally
    self.Pop;
  end;
end;

{ TclToken.TPrm }

procedure TclToken.TPrm.Clear;
begin
  fillchar(self, sizeof(self), 0);
end;


function TclToken.TPrm.GetPosEndAtLength: integer;
begin
   Result:= Pos + Length-1;
end;

procedure TclToken.TPrm.SelectAll;
begin
  Pos := PosBegin;
  Length := PosEnd - PosBegin + 1;
end;

end.

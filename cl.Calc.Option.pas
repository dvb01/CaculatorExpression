unit cl.Calc.Option;

interface
{$I cl.Calc.Define.inc}
uses
  System.SysUtils,
  System.Classes,
  System.Character,
  System.Generics.Collections,
  cl.Calc.Types,
  cl.Calc.Res;

  type

  PclRuleBracketsItem = ^TclRuleBracketsItem;

  TclRuleBracketsItem = record
    CharBegin, CharEnd: Char;
  end;

  // какие скобки можно использовать
  TclRuleBrackets = class sealed(TclObject)
  type
    TEnumerator = class
    private
      FList: TListEnumerator;
      function GetCurrent: PclRuleBracketsItem; inline;
      function CountGet: integer;
    public
      constructor Create(AList: TList);
      destructor Destroy; override;
      function MoveNext: Boolean; inline;
      property Current: PclRuleBracketsItem read GetCurrent;
      procedure Reset;
      property Count: integer read CountGet;
    end;

    TRecChars = record
      Open, Close: TSetOfChar;
      OpenClose: TSetOfChar;
      ArrOpen, ArrClose: TCharArray;
      procedure Clear;
    end;
    PRecChars = ^TRecChars;
  private
    FList: TList;
    FRecCharsSession:TRecChars;
    FLock:integer;
    function IndexofCharBegin(B: Char): integer;
    function IndexofCharEnd(E: Char): integer;
    function RecCharsGet: PRecChars;
    function LockedGet: boolean;
  public
    procedure Registred(CharBegin, CharEnd: Char);
    procedure UnRegistred(CharBegin, CharEnd: Char);
    procedure UnRegistredAll;
    function GetNewEnumerator: TEnumerator; inline;
    function GetNewSetOfChar: TRecChars;
    property RecChars: PRecChars read RecCharsGet;
    property Locked: boolean read LockedGet;
    procedure ParsBefore;
    procedure ParsAfter;
    constructor Create;
    destructor Destroy; override;
  end;

  // правила парсинга и вычисления результата
  TclRule = class(TclObject)
  private
    FBrackets: TclRuleBrackets;
  //  FCanUseDoubleNumberInExpression: Boolean;
    FCanShowError: Boolean;
  public
  //  property CanUseDoubleNumberInExpression: Boolean read FCanUseDoubleNumberInExpression write FCanUseDoubleNumberInExpression;
    property Brackets: TclRuleBrackets read FBrackets;
    property CanShowError: Boolean read FCanShowError write FCanShowError;
    procedure ParsBefore;
    procedure ParsAfter;
    constructor Create;
    destructor Destroy; override;
  end;


implementation



{ TclRuleBrackets }

constructor TclRuleBrackets.Create;
begin
  inherited;
  FLock:=0;
  FList := TList.Create;
  FRecCharsSession.Clear;
end;

destructor TclRuleBrackets.Destroy;
begin
  UnRegistredAll;
  FreeAndNil(FList);
  inherited;
end;

function TclRuleBrackets.GetNewEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FList);
end;

function TclRuleBrackets.GetNewSetOfChar: TRecChars;
var
  L: TEnumerator;
  i: integer;
begin
  Result.Open := [];
  Result.Close := [];
  i := 0;
  L := GetNewEnumerator;
  try
    Setlength(Result.ArrOpen, L.Count);
    Setlength(Result.ArrClose, L.Count);
    while L.MoveNext do
    begin
      {$IFNDEF NEXTGEN}
      Include(Result.Open, AnsiChar(L.Current.CharBegin));
      Include(Result.Close, AnsiChar(L.Current.CharEnd));
      {$ELSE}
      Include(Open, Char(L.Current.CharBegin));
      Include(Close, Char(L.Current.CharEnd));
      {$ENDIF}
      Result.ArrOpen[i] := L.Current.CharBegin;
      Result.ArrClose[i] := L.Current.CharEnd;
      inc(i);
    end;
  finally
    L.Free;
  end;
  Result.OpenClose := Result.Open + Result.Close;
end;

function TclRuleBrackets.IndexofCharBegin(B: Char): integer;
begin
  for Result := 0 to FList.Count - 1 do
    if PclRuleBracketsItem(FList.List[Result]).CharBegin = B then
      exit;
  Result := -1;
end;

function TclRuleBrackets.IndexofCharEnd(E: Char): integer;
begin
  for Result := 0 to FList.Count - 1 do
    if PclRuleBracketsItem(FList.List[Result]).CharEnd = E then
      exit;
  Result := -1;
end;

function TclRuleBrackets.LockedGet: boolean;
begin
   Result:= FLock>0;
end;

procedure TclRuleBrackets.ParsAfter;
begin
   dec(FLock);
   if  FLock = 0 then
        FRecCharsSession.Clear;
end;

procedure TclRuleBrackets.ParsBefore;
begin
  inc(FLock);
  if FLock = 1 then   
  FRecCharsSession:= GetNewSetOfChar;
end;

function TclRuleBrackets.RecCharsGet: PRecChars;
begin
   Result:=@FRecCharsSession;
end;

procedure TclRuleBrackets.Registred(CharBegin, CharEnd: Char);
var
  Item: PclRuleBracketsItem;
begin
  if Locked  then
    raise Exception.CreateResFmt(@Rs_TclRuleBrackets_Locked, []);
  if (IndexofCharBegin(CharBegin) >= 0) or (IndexofCharEnd(CharEnd) >= 0) then
    raise Exception.CreateResFmt(@Rs_TclRuleBrackets_Registred, []);
  New(Item);
  Item.CharBegin := CharBegin;
  Item.CharEnd := CharEnd;
  FList.Add(Item);
end;

procedure TclRuleBrackets.UnRegistred(CharBegin, CharEnd: Char);
var
  i1, i2: integer;
begin
  if Locked  then
    raise Exception.CreateResFmt(@Rs_TclRuleBrackets_Locked, []);

  i1 := IndexofCharBegin(CharBegin);
  i2 := IndexofCharEnd(CharEnd);
  if (i1 >= 0) and (i1 = i2) then
  begin
    Dispose(PclRuleBracketsItem(FList.List[i1]));
    FList.Delete(i1);
  end;
end;

procedure TclRuleBrackets.UnRegistredAll;
var
  i: integer;
begin
  if Locked then
    raise Exception.CreateResFmt(@Rs_TclRuleBrackets_Locked, []);

  for i := FList.Count - 1 downto 0 do
    Dispose(PclRuleBracketsItem(FList.List[i]));
  FList.Clear;
end;

{ TclRuleBrackets.TEnumerator }

function TclRuleBrackets.TEnumerator.CountGet: integer;
begin
  Result := FList.Count;
end;

constructor TclRuleBrackets.TEnumerator.Create(AList: TList);
begin
  inherited Create;
  FList := TListEnumerator.Create(AList);
end;

destructor TclRuleBrackets.TEnumerator.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TclRuleBrackets.TEnumerator.GetCurrent: PclRuleBracketsItem;
begin
  Result := PclRuleBracketsItem(FList.Current);
end;

function TclRuleBrackets.TEnumerator.MoveNext: Boolean;
begin
  Result := FList.MoveNext;
end;

procedure TclRuleBrackets.TEnumerator.Reset;
begin
  FList.Reset;
end;

{ TclRuleBrackets.TRecChars }

procedure TclRuleBrackets.TRecChars.Clear;
begin
   SetLength(ArrOpen,0);
   SetLength(ArrClose,0);
   fillchar(self,sizeof(self),0);
end;



{ TclRule }

constructor TclRule.Create;
begin
  inherited;
  FBrackets := TclRuleBrackets.Create;
 // FCanUseDoubleNumberInExpression := false;
  FCanShowError := false;
end;

destructor TclRule.Destroy;
begin
  FreeAndNil(FBrackets);
  inherited;
end;

procedure TclRule.ParsAfter;
begin
   FBrackets.ParsAfter;
end;

procedure TclRule.ParsBefore;
begin
  FBrackets.ParsBefore;
end;



end.

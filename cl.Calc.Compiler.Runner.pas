unit cl.Calc.Compiler.Runner;

interface

{$I cl.Calc.Define.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  Math,
  cl.Calc.Types,
  cl.Calc.Syntax,
  cl.Calc.Scanner,
  cl.Calc.Option,
  cl.Calc.Classes,
  cl.Calc.Res,
  cl.Calc.Compiler.Types;

type

  TclRunner = class(TclCompilerCustom)
  private
    FReturn: double;
    function GetValueMain(Ctx: TccEval): double;
    function GetValueSymbol(Ctx: TccSymbol): double;
    function GetValueNumber(Ctx: TccNumber): double;
    function GetValueFunction(Ctx: TccFunction): double;
    function GetArgumetsFunction(Ctx: TccFunction):TArray<double>;
    function GetValueFunctionSys(Ctx: TccFunctionSys; const Args: TArray<double>): double;
    function GetValueFunctionAddr(Ctx: TccFunctionAddr;const Args: TArray<double>): double;
    function GetValueCombo(Ctx: TccCombo): double;
    function GetValueSymbolAssing(Ctx: TccSymbolAssing): double;
    function GetValueExpression(Ctx: TccExpression): double;

    function Exp(Ctx: TccExpression): double;
    procedure Exp_CheckSubsequence(Ctx: TccExpression; IndexStart: integer);
    function Exp_Сalculation(Ctx: TccExpression; Ops: Syntax.TEnumOperators; var AReturn: double): boolean;
    function Exp_IndexOperator(Ctx: TccExpression; StartIndex: integer; Op: Syntax.TEnumOperators): integer;
  protected
    procedure Start; override;
    procedure DoStart; override;

    procedure ErrorRunner(Ctx: TccBase; const Place, FerstMsg: string; AMsg: PResStringRec; Args: array of string); overload;
    procedure ErrorRunner(Ctx: TccBase; AMsg: PResStringRec; Args: array of string); overload;
    procedure ErrorRunner(Ctx: TccBase); overload;

  public
    procedure Reset; override;
    constructor Create();
    destructor Destroy; override;
    property Return: double read FReturn write FReturn;
  end;

implementation

{ TclRunner }

constructor TclRunner.Create;
begin
  inherited Create;
  FReturn := 0;
end;

destructor TclRunner.Destroy;
begin
  inherited;
end;

procedure TclRunner.ErrorRunner(Ctx: TccBase; const Place, FerstMsg: string;
  AMsg: PResStringRec; Args: array of string);
begin
  if Ctx <> nil then
    Ctx.CaretPush(Scanner);
  inherited ErrorCompiler(Place, FerstMsg, AMsg, Args);
  if Ctx <> nil then
    Ctx.CaretPop(Scanner);
end;

procedure TclRunner.ErrorRunner(Ctx: TccBase; AMsg: PResStringRec;
  Args: array of string);
begin
  if Ctx <> nil then
    Ctx.CaretPush(Scanner);
  inherited ErrorCompiler('', '', AMsg, Args);
  if Ctx <> nil then
    Ctx.CaretPop(Scanner);
end;

procedure TclRunner.ErrorRunner(Ctx: TccBase);
begin
  if Ctx <> nil then
    Ctx.CaretPush(Scanner);
  inherited ErrorCompiler('', '', nil, []);
  if Ctx <> nil then
    Ctx.CaretPop(Scanner);
end;

procedure TclRunner.Start;
begin
  FReturn := GetValueMain(ContextRoot);
end;

procedure TclRunner.DoStart;
begin
  Start;
end;

procedure TclRunner.Reset;
begin
  inherited Reset;
  FReturn := 0;
end;

function TclRunner.GetValueMain(Ctx: TccEval): double;
begin
  Result := 0;
  // TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator

  // TccEval =  TccNumber  TccFunction  TccCombo  TccSymbolAssing

  if Ctx is TccSymbolAssing then
    ErrorRunner(Ctx {$IFDEF CalcDebug},'GetValueMain', ' Ctx is TccSymbolAssing', nil, []{$ENDIF})

  else if Ctx is TccFunction then
    Result := GetValueFunction(TccFunction(Ctx))

  else if Ctx is TccCombo then
    Result := GetValueCombo(TccCombo(Ctx))

  else if Ctx is TccNumber then
    Result := GetValueNumber(TccNumber(Ctx))

  else if Ctx is TccSymbol then
    Result := GetValueSymbol(TccSymbol(Ctx))
  else
    ErrorRunner(Ctx {$IFDEF CalcDebug},'GetValueMain', '2', nil, []{$ENDIF})
end;

function TclRunner.GetValueSymbol(Ctx: TccSymbol): double;
begin
  if Ctx.Variable = nil then
    ErrorRunner(Ctx {$IFDEF CalcDebug},'GetValueSymbol', '', nil, []{$ENDIF});
  Result := Ctx.Variable.Value;
end;

function TclRunner.GetValueNumber(Ctx: TccNumber): double;
begin
  Result := Ctx.Value;
end;

function TclRunner.GetValueFunction(Ctx: TccFunction): double;
begin
  Result:=0;
  if Ctx is TccFunctionAddr then
  begin
     Result:= GetValueFunctionAddr(TccFunctionAddr(Ctx),GetArgumetsFunction(Ctx));
  end
  else if (Ctx is TccFunctionSys) then
  begin
     Result := GetValueFunctionSys(TccFunctionSys(Ctx), GetArgumetsFunction(Ctx));
  end
  else
    ErrorRunner(Ctx{$IFDEF CalcDebug},'GetValueFunction', '', nil, []{$ENDIF});
end;

function TclRunner.GetArgumetsFunction(Ctx: TccFunction):TArray<double>;
var c,i:integer;

  function LocGetCountArgs: integer;
  var
    Expected: Syntax.TEnumExpected;
    Item: TccBase;
    i: integer;
  begin
    Result := 0;
    Expected := opeEval; // opeEval, opeSepa
    for i := 0 to Ctx.Childs.Count - 1 do
    begin
      Item := Ctx.Childs.Items[i];
      if Item is TccEval then
      begin
        if Expected <> opeEval then
          ErrorRunner(Item{$IFDEF CalcDebug},'GetFunctionArgumets.LocGetCountArgs', '1', nil,[]{$ENDIF});
        Expected := opeSepa;
        inc(Result);
      end
      else if Item is TccSeparator then
      begin
        if Expected <> opeSepa then
          ErrorRunner(Item{$IFDEF CalcDebug},'GetFunctionArgumets.LocGetCountArgs', '2', nil,[]{$ENDIF});
        Expected := opeEval;
      end
      else
        ErrorRunner(Item{$IFDEF CalcDebug},'GetFunctionArgumets.LocGetCountArgs', '3', nil,[]{$ENDIF});
    end;
    if (Ctx.Childs.Count>0) and (Expected <> opeSepa) then
      ErrorRunner(Ctx{$IFDEF CalcDebug},'GetFunctionArgumets.LocGetCountArgs', '4', nil,[]{$ENDIF});

    if Ctx.Childs.Count = 1 then
    begin
      // TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
       Item:=  Ctx.Childs.Items[0];
       if not (Item is TccCombo) then
        ErrorRunner(Ctx{$IFDEF CalcDebug},'GetFunctionArgumets.LocGetCountArgs', '5', nil,[]{$ENDIF});
        if TccCombo(Item).Childs.Count = 0 then
         Result:=0;
    end;
    
  end;
  var
  Item: TccBase;
begin
    c:=LocGetCountArgs;
    if (c < 0) or (c > 20) then
    ErrorRunner(Ctx{$IFDEF CalcDebug},'GetFunctionArgumets', '1', nil,[]{$ENDIF});
    Setlength(Result, c);
    if c > 0 then
    begin
     // когда с = 0 Ctx.Childs.Count  может быть больше 0  и содержать пустой combo
     // это происходит когда  пустые скобки Func()
      c:=0;
      for i := 0 to Ctx.Childs.Count - 1 do
      begin
        if c >= length(Result) then
        ErrorRunner(Ctx{$IFDEF CalcDebug},'GetFunctionArgumets', '2', nil,[]{$ENDIF});
        Item := Ctx.Childs.Items[i];
        if Item is TccEval then
        begin
          Result[c] := GetValueMain(TccEval(Item));
          inc(c);
        end;
      end;
    end;
end;

function TclRunner.GetValueFunctionAddr(Ctx: TccFunctionAddr;const Args: TArray<double>): double;
var Values: TArray<TValue>;
C,I:integer;
begin
   C:= Ctx.CountArgs;
   if C <> length(Args) then
    ErrorRunner(Ctx{$IFDEF CalcDebug},'GetValueFunctionAddr', 'Кол-во агрументов переданные в функцию должно быть равно '+Ctx.CountArgs.ToString, nil, []{$ENDIF});

   Setlength(Values,C);
   for I := 0 to C-1 do
    Values[i]:= Args[i];
   Result:=System.Rtti.Invoke(Ctx.Call,Values,System.TypInfo.ccReg,typeinfo(Double),true,false).AsExtended;
end;

function TclRunner.GetValueFunctionSys(Ctx: TccFunctionSys; const Args: TArray<double>): double;
var
  CountArgs: integer;
  procedure LocErrorN(c: integer);
  begin
    if length(Args) <> c then
      ErrorRunner(Ctx{$IFDEF CalcDebug},'GetValueFunctionSys.LocErrorN', 'Кол-во агрументов переданные в функцию должно быть равно '+c.ToString, nil, []{$ENDIF});
  end;
  procedure LocError1;
  begin
    LocErrorN(1);
  end;

begin
  CountArgs := Syntax.sFuncArg[Ctx.NameFunc];
  CountArgs := min(max(CountArgs, 0), 20);
  if CountArgs < length(Args) then
    ErrorRunner(Ctx{$IFDEF CalcDebug},'GetValueFunctionSys', 'Кол-во агрументов переданные в функцию должно быть равно '+CountArgs.ToString, nil, []{$ENDIF});
  Result := 0;

  case Ctx.NameFunc of
    fnArcSin:
      begin
        LocError1;
        Result := Math.ArcSin(Args[0]);
      end;
    fnArcCos:
      begin
        LocError1;
        Result := Math.ArcCos(Args[0]);
      end;
    fnTan:
      begin
        LocError1;
        Result := Math.Tan(Args[0]);
      end;
    fnCotan:
      begin
        LocError1;
        Result := Math.Cotan(Args[0]);
      end;
    fnSinCos:
      begin
        Result := 0;
      end;
    fnSin:
      begin
        LocError1;
        Result := Sin(Args[0]);
      end;
    fnCos:
      begin
        LocError1;
        Result := Cos(Args[0]);
      end;
    fnLog10:
      begin
        LocError1;
        Result := Math.Log10(Args[0]);
      end;
    fnLog2:
      begin
        LocError1;
        Result := Math.Log2(Args[0]);
      end;
    fnLogN:
      begin
        LocErrorN(2);
        Result := Math.LogN(Args[0], Args[1]);
      end;
    fnMax:
      begin
        LocErrorN(2);
        Result := Math.max(Args[0], Args[1]);
      end;
    fnMin:
      begin
        LocErrorN(2);
        Result := Math.min(Args[0], Args[1]);
      end;
    fnPower:
      begin
        LocErrorN(2);
        Result := Math.Power(Args[0], Args[1]);
      end;
    fnPi:
      begin
        LocErrorN(0);
        Result := Pi;
      end;
    fnRand:
      begin
        LocErrorN(1);
        Result := Random(Round(Args[0]));
      end;
  else
    ErrorRunner(Ctx{$IFDEF CalcDebug},'GetValueFunctionSys', '2', nil, []{$ENDIF});
  end;
end;

function TclRunner.GetValueCombo(Ctx: TccCombo): double;
begin
  Result := GetValueExpression(Ctx);
end;

function TclRunner.GetValueSymbolAssing(Ctx: TccSymbolAssing): double;
begin
  if Ctx.Variable = nil then
    ErrorRunner(Ctx{$IFDEF CalcDebug},'GetValueSymbolAssing', '', nil, []{$ENDIF});
  Result := GetValueExpression(Ctx);
  Ctx.Variable.Value := Result;
end;

function TclRunner.GetValueExpression(Ctx: TccExpression): double;
begin
  Result := Exp(Ctx);
end;

function TclRunner.Exp(Ctx: TccExpression): double;
var
  ACount, i: integer;
  Item: TccBase;
  R, m: boolean;
begin
  Result := 0;
  ACount := Ctx.Childs.Count;
  if ACount = 1 then
  begin
    Item := Ctx.Childs.Items[0];
    if Item is TccEval then
    begin
      if Item is TccSymbolAssing then
        ErrorRunner(Ctx {$IFDEF CalcDebug},'Exp', '1', nil, []{$ENDIF});
      // т.к даже если это ContextRoot то  ACount дожен быть > 1 т.к ";"

      Result := GetValueMain(TccEval(Item));
    end
    else
      ErrorRunner(Ctx {$IFDEF CalcDebug},'Exp', '2', nil, []{$ENDIF});
    exit;
  end;

  Exp_CheckSubsequence(Ctx, 0);

  // для запуска строк с присвоением d:= 1;
  if ContextRoot = Ctx then
  begin
    for i := 0 to Ctx.Childs.Count - 1 do
    begin
      Item := Ctx.Childs.Items[i];
      if Item is TccSymbolAssing then
        Result := GetValueSymbolAssing(TccSymbolAssing(Item));
    end;
  end;

  R := Exp_Сalculation(Ctx, [opPower], Result);
  m := Exp_Сalculation(Ctx, [opMult, opDiv, opDivWhole, opMod], Result);
  R := R or m;
  m := Exp_Сalculation(Ctx, [opShl, opShr, opAnd, opOr, opXor], Result);
  R := R or m;
  m := Exp_Сalculation(Ctx, [opPlus, opMinus], Result);
  R := R or m;


  // для получения результата если в гл контексте есть TccSymbolAssing и нет активных операторов
  // "d; s; d:=1; s:=d+1; d"
  // в данном примере результатом будет  d , т.е 1  но если  "d; s; d:=1; s:=d+1;" то результат будет s т.е 2
  if (ContextRoot = Ctx) and not R and (Ctx.Childs.Count > 0) then
  begin
    Item := Ctx.Childs.Items[-1];
    if Item is TccEval then
      Result := GetValueMain(TccEval(Item));
  end;
end;

function TclRunner.Exp_Сalculation(Ctx: TccExpression;
  Ops: Syntax.TEnumOperators; var AReturn: double): boolean;
type
  TOp = Syntax.TEnumOperator;
var
  C: TOp;
  L, R: double;
  i: integer;

  function LocGetValueLeftRigth(AOp: TOp; AL, AR: double): double;
  begin

    case AOp of

      opPlus:
        Result := AL + AR;
      opMinus:
        Result := AL - AR;
      opMult:
        Result := AL * AR;
      opDiv:
        Result := AL / AR;
      opDivWhole:
        Result := Round(AL) div Round(AR);
      opPower:
        Result := Power(AL, AR);
      opMod:
        Result := Round(AL) mod Round(AR);
      opShl:
        Result := Round(AL) Shl Round(AR);
      opShr:
        Result := Round(AL) Shr Round(AR);
      opAnd:
        Result := Round(AL) And Round(AR);
      opOr:
        Result := Round(AL) Or Round(AR);
      opXor:
        Result := Round(AL) Xor Round(AR);
    else
      raise Exception.CreateResFmt
        (@Rs_TclRunner_ExpСalculation_InvalidOperator, []);

    end;
  end;

  function LocGetCenter(Index: integer): TOp;
  var
    AOperator: TccOperator;
  begin

    AOperator := Ctx.Childs.Items[Index] as TccOperator;

    if (Index = Ctx.Childs.Count - 1) or
      ((Index = 0) and not(AOperator.Value in [opPlus, opMinus])) then
      ErrorRunner(AOperator{$IFDEF CalcDebug},'Exp_Сalculation.LocGetCenter', '', nil, []{$ENDIF});

    Result := AOperator.Value;
    AOperator.Free;
  end;

  function LocGetLeft(Index: integer): double;
  var
    Item: TccBase;
  begin
    Result := 0;
    if Index < 0 then
      exit();
    Item := Ctx.Childs.Items[Index];

    if not(Item is TccEval) then
      ErrorRunner(Item {$IFDEF CalcDebug},'Exp_Сalculation.LocGetLeft', '', nil, []{$ENDIF})
    else
      Result := GetValueMain(TccEval(Item));
    Item.Free;
  end;

  function LocGetRigth(var Index: integer): double;
  var
    Item: TccBase;
    AOp, p: TOp;
  begin
    if Index > 0 then
      dec(Index);

    AOp := opPlus;
    while True do
    begin
      Item := Ctx.Childs.Items[Index];

      /// //////////////////////////
      if Item is TccEval then
      begin
        Result := GetValueMain(TccEval(Item));

        if AOp = opMinus then
          Result := -Result;
        Item.Free;
        exit;
      end;
      /// ////////////////////////////////

      if not(Item is TccOperator) then
        ErrorRunner(Item{$IFDEF CalcDebug},'Exp_Сalculation.LocGetRigth', '1', nil,[]{$ENDIF});

      p := TccOperator(Item).Value;

      if not(p in [opPlus, opMinus]) then
        ErrorRunner(Item {$IFDEF CalcDebug},'Exp_Сalculation.LocGetRigth', '2', nil,[]{$ENDIF});
      if p = opMinus then
      begin
        if AOp = opMinus then
          AOp := opPlus
        else if AOp = opPlus then
          AOp := opMinus
        else
          ErrorRunner(Item {$IFDEF CalcDebug},'Exp_Сalculation.LocGetRigth', '3', nil, []{$ENDIF});
      end;
      Item.Free;

      if Index >= Ctx.Childs.Count then
        raise Exception.CreateResFmt
          (@Rs_TclRunner_ExpСalculation_OperatorBeginOrEnd, []);

    end;
  end;

begin
  i := 0;
  Result := false;

  while i < Ctx.Childs.Count do
  begin
    i := Exp_IndexOperator(Ctx, i, Ops);
    if i < 0 then
      break;
    if not Result then
      Result := True;
    c := LocGetCenter(i);
    L := LocGetLeft(i - 1);
    R := LocGetRigth(i);
    AReturn := LocGetValueLeftRigth(c, L, R);
    Ctx.AddNumberOuter(i, AReturn);
  end;
end;

function TclRunner.Exp_IndexOperator(Ctx: TccExpression; StartIndex: integer;
  Op: Syntax.TEnumOperators): integer;
begin
  for Result := StartIndex to Ctx.Childs.Count - 1 do
    if (Ctx.Childs.Items[Result] is TccOperator) and
      (TccOperator(Ctx.Childs.Items[Result]).Value in Op) then
      exit();
  Result := -1;
end;

procedure TclRunner.Exp_CheckSubsequence(Ctx: TccExpression;
  IndexStart: integer);
var
  i: integer;
  Expected: Syntax.TEnumExpected;
  Item: TccBase;
  IsCurAssing: boolean;
  IsNeedError: boolean;
  IsLastSepAssign: boolean;
  b:boolean;
begin
  Expected := opeEval; // opeAny, opeEval, opeSepa
  i := IndexStart;
  IsCurAssing := false;
  IsNeedError := false;
  IsLastSepAssign := false;


  while i < Ctx.Childs.Count do
  begin
    IsLastSepAssign := false;
    Item := Ctx.Childs.Items[i];
    // TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
    // TccEval =  TccNumber  TccFunction  TccCombo  TccSymbolAssing
    if Item is TccEval then
    begin
      if Item is TccSymbolAssing then
      begin
        if (self.ContextRoot <> Ctx) or IsNeedError then
          ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '1', nil, []{$ENDIF});
        IsCurAssing := True;
      end
      else
        IsNeedError := True;

      if Expected <> opeEval then
        ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '2', nil, []{$ENDIF});
      Expected := opeSepa;
    end
    else if Item is TccOperator then
    begin
      if IsCurAssing then
        ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '3', nil, []{$ENDIF});

      if Expected <> opeSepa then
      begin
        if not(TccOperator(Item).Value in [opPlus, opMinus]) then
          ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '4', nil, []{$ENDIF});
      end;
      Expected := opeEval;
    end
    else if Item is TccSeparator then
    begin
      if IsCurAssing then
      begin
        if TccSeparator(Item).Value <> opsDotWithComma then
          ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '5', nil, []{$ENDIF});
        IsCurAssing := false;
      end
      else
        ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '6', nil, []{$ENDIF});

      if Expected <> opeSepa then
        ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '7', nil, []{$ENDIF});
      IsLastSepAssign := True;
      Expected := opeEval;
    end
    else
      ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '8', nil, []{$ENDIF});
    inc(i);
  end;

  if (not IsLastSepAssign and (Expected = opeEval)) or
    (IsLastSepAssign and (Expected = opeSepa)) then
  begin
    Item := Ctx.Childs.ItemsTry[-1];
    b:= (Item  <> nil) and (Ctx.Parent <> nil) and (Ctx.Parent is TccFunction);
    if not b then
    ErrorRunner(Item{$IFDEF CalcDebug},'Exp_CheckSubsequence', '9', nil, []{$ENDIF});
  end;
end;

end.

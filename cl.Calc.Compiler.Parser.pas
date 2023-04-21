unit cl.Calc.Compiler.Parser;

interface

{$I cl.Calc.Define.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Math,
  cl.Calc.Types,
  cl.Calc.Syntax,
  cl.Calc.Scanner,
  cl.Calc.Option,
  cl.Calc.Classes,
  cl.Calc.Res,
  cl.Calc.Compiler.Types;

type

  TclParser = class(TclCompilerCustom)
  private
    FBrasketsStack: TStack<ShortInt>;
    // ShortInt это индекс на FRule.Brackets.RecChars ArrOpen, ArrClose: TCharArray; если -1 то это стандартная скобка ( )
    FCtxStack: TclContextStack;
    FContext: TccExpression;
    procedure ContextUpdateCaret();
    function CtxCountGet: integer;
    function CtxPeekGet: TccExpression;
    procedure CtxPush(New: TccExpression);
    function CtxPop: TccExpression;
    property CtxCount: integer read CtxCountGet;
    property CtxPeek: TccExpression read CtxPeekGet;

    function BrasketsPeekGet: integer;
    function BrasketsCountGet: integer;
    procedure BrasketsPush(New: integer);
    function BrasketsPop: integer;
    property BrasketsPeek: integer read BrasketsPeekGet;
    property BrasketsCount: integer read BrasketsCountGet;

    procedure ParseMain();

    procedure TokenClassSeparator;
    procedure TokenSeparatorComma;
    procedure TokenSeparatorDotWithComma;
    procedure TokenSeparator_CheckError;
    procedure TokenSeparator_Create(Ops: Syntax.TEnumSeparator);

    procedure TokenClassLiteral;

    /// //////////////////////////
    // Symbol Variable
    procedure TokenSymbol_Case;
    procedure TokenSymbol_VariableCreate;
    procedure TokenSymbol_SymbolCreate;
    procedure TokenSymbol_SymbolAssingCreate;
    /// //////////////////////////

    /// //////////////////////////
    // Number
    procedure TokenClassNumber;
    procedure TokenClassHexNumber;
    procedure TokenNumber_Update(Value: Double);
    procedure TokenNumber_Create(Value: Double);
    /// //////////////////////////

    /// //////////////////////////
    // Operator
    procedure TokenClassOperator;
    procedure TokenOperator_Update(Op: Syntax.TEnumOperator);
    procedure TokenOperator_Create(Op: Syntax.TEnumOperator);
    /// ///////////////////////

//    procedure TokenClassSpecifical;

    /// //////////////////////////
    // Brasket
    procedure TokenClassBrasket;
    /// //////////////////////////

    // Function
    procedure TokenFunction_BrasketBegin; // Brasket
    procedure TokenFunction_BrasketEnd; // Brasket
    procedure TokenFunction_SeparatorComma;
    procedure TokenFunctionSys_LiteralCase;
    procedure TokenFunctionSys_Create(AName: Syntax.TEnumFunc);
    procedure TokenFunctionAddr_Create(CountArgs:integer;Call:Pointer);
    // Combo
    procedure TokenCombo_BrasketBegin();
    procedure TokenCombo_BrasketEnd;

  protected
    procedure Start; override;
    procedure DoStart; override;
    procedure ErrorParser(const Place, FerstMsg: string; AMsg: PResStringRec; Args: array of string); overload;
    procedure ErrorParser(AMsg: PResStringRec; Args: array of string); overload;
    procedure ErrorParser(); overload;
    property Context: TccExpression read FContext;
  public
    procedure Reset; override;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TclParser }

constructor TclParser.Create();
begin
  inherited Create;
  FBrasketsStack := TStack<ShortInt>.Create;
  FContext := nil;
  FCtxStack := TclContextStack.Create;
end;

destructor TclParser.Destroy;
begin
  FContext := nil;
  FreeAndNil(FCtxStack);
  FreeAndNil(FBrasketsStack);
  inherited;
end;

procedure TclParser.Reset;
begin
  inherited Reset;
  FBrasketsStack.Clear;
  FCtxStack.Clear;
  FContext := nil;
end;

function TclParser.CtxCountGet: integer;
begin
  Result := FCtxStack.Count;
end;

function TclParser.CtxPeekGet: TccExpression;
begin
  Result := FCtxStack.Peek;
end;

function TclParser.CtxPop: TccExpression;
begin
  FContext := FCtxStack.Pop;
  Result := FContext;
end;

procedure TclParser.CtxPush(New: TccExpression);
begin
  FCtxStack.Push(FContext);
  FContext := New;
end;

procedure TclParser.ErrorParser();
begin
  inherited ErrorCompiler('', '', nil, []);
end;

procedure TclParser.ErrorParser(AMsg: PResStringRec; Args: array of string);
begin
  inherited ErrorCompiler('', '', AMsg, Args);
end;

procedure TclParser.ErrorParser(const Place, FerstMsg: string;
  AMsg: PResStringRec; Args: array of string);
begin
  inherited ErrorCompiler(Place, FerstMsg, AMsg, Args);
end;

function TclParser.BrasketsPeekGet: integer;
begin
  Result := FBrasketsStack.Peek;
end;

function TclParser.BrasketsCountGet: integer;
begin
  Result := FBrasketsStack.Count;
end;

procedure TclParser.BrasketsPush(New: integer);
begin
  if ShortInt(New) <> New then
    raise Exception.CreateResFmt(@Rs_TclParser_BrasketsPush, []);
  FBrasketsStack.Push(ShortInt(New));
end;

function TclParser.BrasketsPop: integer;
begin
  Result := FBrasketsStack.Pop;
end;

procedure TclParser.DoStart;
begin
  CtxPush(ContextRoot);
  Start;
  CtxPop;
end;

procedure TclParser.Start;
begin
  ParseMain();
end;

procedure TclParser.ParseMain();
begin
  try
    while Scanner.Read do
    begin

{$IFDEF CalcDebug}
      Scanner.CurText;
      Scanner.CurTextFull;
      Scanner.CurChar;
{$ENDIF}
      ContextUpdateCaret;

{$IFDEF CalcDebug}
      Context.CaretText[Scanner];
{$ENDIF}

      case Scanner.Caret.TokenClass of
        tcAssing:
          ErrorParser({$IFDEF CalcDebug}'ParseMain', ' TokenClass = tcAssing', nil, []{$ENDIF});
          // tcAssing может быть только в  TokenClassLiteral_Symb
        tcSeparator:
          TokenClassSeparator;
        tcLiteral:
          TokenClassLiteral;
        tcNumber:
          TokenClassNumber;
        tcHexNumber:
          TokenClassHexNumber;
        tcOperator:
          TokenClassOperator;
        tcSpecifical:
          ErrorParser({$IFDEF CalcDebug}'ParseMain', ' TokenClass = tcSpecifical', nil, []{$ENDIF});
        tcBrasket:
          TokenClassBrasket;
      else
        ErrorParser({$IFDEF CalcDebug}'ParseMain', ' TokenClass = tcNone', nil, []{$ENDIF});
      end;
    end;

  finally
    Scanner.Push;
    Scanner.ReadJmpToEnd;
    ContextUpdateCaret;
    Scanner.Pop;
  end;

  if self.BrasketsCount > 0 then
    ErrorParser({$IFDEF CalcDebug}'ParseMain', ' не закрытая скобка в конце',nil, []{$ENDIF});
  if self.CtxCount > 1 then
    ErrorParser({$IFDEF CalcDebug}'ParseMain', 'CtxCount > 1', nil, []{$ENDIF});
end;

procedure TclParser.TokenClassSeparator;
begin
  if Scanner.IsCurToken(Syntax.Comma) then
    TokenSeparatorComma
  else if Scanner.IsCurToken(Syntax.DotWithComma) then
    TokenSeparatorDotWithComma
  else
    ErrorParser({$IFDEF CalcDebug}'TokenClassSeparator', '', nil, []{$ENDIF});
end;

procedure TclParser.TokenSeparator_CheckError;
var
  Last: TccBase;
begin
  Last := Context.Childs.ItemsTry[-1];
  if Last <> nil then
  begin
    // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
    if (Last is TccOperator) or (Last is TccSeparator) or
      (Last is TccSymbolAssing) then
      ErrorParser({$IFDEF CalcDebug}'TokenSeparator_CheckError',
        'предыдущий символ не может быть [' + Last.ClassName + ']', nil,[]{$ENDIF});
  end;
end;

procedure TclParser.TokenSeparatorComma;
begin
  Scanner.Match(Syntax.Comma);
  TokenSeparator_CheckError;

  if not((CtxPeek is TccFunction) and (Context is TccCombo)) then
    ErrorParser({$IFDEF CalcDebug}'TokenSeparatorComma', '', nil, []{$ENDIF});
  TokenFunction_SeparatorComma;
end;

procedure TclParser.TokenSeparatorDotWithComma;
var
  Ops: Syntax.TEnumSeparator;
begin
  Scanner.Match(Syntax.DotWithComma);
  TokenSeparator_CheckError;

  if not(self.Context is TccSymbolAssing) then
    ErrorParser({$IFDEF CalcDebug}'TokenSeparatorDotWithComma', '1', nil,[]{$ENDIF});

  if (self.CtxPeek <> self.ContextRoot) then
    ErrorParser({$IFDEF CalcDebug}'TokenSeparatorDotWithComma', '2', nil,[]{$ENDIF});

  Ops := Syntax.OpsToEnum(Scanner.CurChar);
  if Ops <> opsDotWithComma then
    ErrorParser({$IFDEF CalcDebug}'TokenSeparatorDotWithComma', '3', nil,[]{$ENDIF});

  self.CtxPop;
  TokenSeparator_Create(Ops);
end;

procedure TclParser.TokenSeparator_Create(Ops: Syntax.TEnumSeparator);
begin
  Scanner.Push;
  Scanner.CurUpdateSelect;
  Context.AddSeparator(Ops).CaretUpdate(Scanner);
  Scanner.Pop;
end;

procedure TclParser.TokenClassLiteral;
var FnAddr:Pointer;
var FnCountArgs:integer;
begin
  FnAddr:=nil;
  FnCountArgs:=0;
  if Scanner.IsCurToken(Syntax.sFunc, nil) then
    TokenFunctionSys_LiteralCase
  else if Syntax.FuncGet(Scanner.CurText,FnAddr,FnCountArgs) and (FnAddr<>nil) then
    TokenFunctionAddr_Create(FnCountArgs,FnAddr)
  else
    TokenSymbol_Case;
end;

procedure TclParser.TokenSymbol_Case;
var
  b: boolean;

  function LocIsNextAssing: boolean;
  begin
    Result := false;
    Scanner.Push;
    try
      if not Scanner.Read then
        exit;
      Result := Scanner.Caret.TokenClass = tcAssing;
    finally
      Scanner.Pop;
    end;
  end;

  function LocIsNextDotWithComma: boolean;
  begin
    Result := false;
    Scanner.Push;
    try
      if not Scanner.Read then
        exit;
      Result := Scanner.Caret.TokenClass = tcSeparator;
      Result := Result and Scanner.IsCurToken(Syntax.DotWithComma);
    finally
      Scanner.Pop;
    end;
  end;

  function LocCheckLastDotWithComma(isError: boolean): boolean;
  var
    Last: TccBase;
  begin
    Result := true;
    Last := Context.Childs.ItemsTry[-1];
    if Last <> nil then
    begin
      // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
      if not((Last is TccSeparator) and
        (TccSeparator(Last).Value = opsDotWithComma)) then
      begin
        Result := false;
        if isError then
          ErrorParser ({$IFDEF CalcDebug}'TokenSymbol_Case.LocCheckLastDotWithComma', '', nil, []{$ENDIF});
      end;

    end;
  end;

  procedure LocChildsCheckWholeExpression;
  var
    I: integer;
    P: TccBase;
  begin
    // в root блоке не может быть по среди i:=1; i:=2; (гололое выражение 2 + 3) i:=3; тут можно 9+10+i
    // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
    for I := 0 to Context.Childs.Count - 1 do
    begin
      P := Context.Childs.Items[I];
      if not((P is TccSymbolAssing) or (P Is TccSeparator)) then
        ErrorParser ({$IFDEF CalcDebug}'TokenSymbol_Case.LocChildsCheckWholeExpression','', nil, []{$ENDIF});
    end;
  end;

  procedure LocCheckCanCreateSymbol;
  begin
    if self.Context <> self.ContextRoot then
      ErrorParser({$IFDEF CalcDebug}'TokenSymbol_Case.LocCheckCanCreateSymbol', '', nil, []{$ENDIF});
    LocCheckLastDotWithComma(true);
    LocChildsCheckWholeExpression;
  end;

  function LocGetVariable(): TcgVariable;
  begin
    Result := ContextRoot.VariableTabl.Get(Scanner.CurText);
  end;

  procedure LocCreateVariable;
  begin
    if LocGetVariable = nil then
      TokenSymbol_VariableCreate;
  end;

begin
  /// правила создания памяти переменной схожи с delphi  + не нужно декларировать имя если в него что то присваевается
  /// будет ошибка если встится имя которое выполняет get а не set
  /// i:= b+1; ошибка в b
  /// b; i:= b+1; нет ошибки b = 0 ответ i т.е 1
  /// локальные перенные в под выражениях не делал

  b := LocIsNextAssing;
  if b then
  begin
    // создание новый памяти переменной
    LocCheckCanCreateSymbol;
    LocCreateVariable;
    TokenSymbol_SymbolAssingCreate;
    if not Scanner.Read then
      ErrorParser({$IFDEF CalcDebug}'TokenSymbol_Case', '1', nil, []{$ENDIF});
  end
  else
  begin

    if (Context = self.ContextRoot) and LocIsNextDotWithComma and
      LocCheckLastDotWithComma(false) then
    begin
      // создание новый памяти переменной
      LocCheckCanCreateSymbol;
      LocCreateVariable;
      if not Scanner.Read then
        ErrorParser({$IFDEF CalcDebug}'TokenSymbol_Case', '2', nil, []{$ENDIF});
      exit;
    end;

    // если память под переменную не выделена то error
    if LocGetVariable = nil then
      ErrorParser({$IFDEF CalcDebug}'TokenSymbol_Case', '3', nil, []{$ENDIF});

    // символ который ссылается на память
    TokenSymbol_SymbolCreate;

  end;
end;

procedure TclParser.TokenSymbol_VariableCreate;
begin
  ContextRoot.VariableTabl.AddVariable(Scanner.CurText);
end;

procedure TclParser.TokenSymbol_SymbolCreate;
var
  S: string;
  V: TcgVariable;
  Last: TccBase;
begin
  S := Scanner.CurText;
  V := ContextRoot.VariableTabl.Get(S);
  if V = nil then
    ErrorParser({$IFDEF CalcDebug}'TokenSymbol_SymbolCreate', '1', nil, []{$ENDIF});

  // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
  Last := Context.Childs.ItemsTry[-1];
  if (Last <> nil) then
  begin
    if not((Last is TccOperator) or (Last is TccSeparator)) then
      ErrorParser({$IFDEF CalcDebug}'TokenSymbol_SymbolCreate', '2', nil,[]{$ENDIF});
    // если условие прошло то все остальные ошибки в runner
  end;

  Scanner.Push;
  Scanner.CurUpdateSelect;
  Context.AddSymbol(V).CaretUpdate(Scanner);
  Scanner.Pop;
end;

procedure TclParser.TokenSymbol_SymbolAssingCreate;
var
  V: TcgVariable;
  S: TccSymbolAssing;
begin
  V := ContextRoot.VariableTabl.Get(Scanner.CurText);
  if V = nil then
    ErrorParser({$IFDEF CalcDebug}'TokenSymbol_SymbolAssingCreate', '', nil, []{$ENDIF});
  Scanner.Push;
  Scanner.CurUpdateSelect;
  S := Context.AddSymbolAssing(V);
  S.CaretUpdate(Scanner);
  CtxPush(S);
  Scanner.Pop;
end;

procedure TclParser.TokenClassNumber;
var
  AValue: string;
  ADouble: Double;
begin
  AValue := Scanner.CurText;
  AValue := trim(AValue.Replace('.', FormatSettings.DecimalSeparator));
  //
  if not TryStrToFloat(AValue, ADouble) then
    ErrorParser({$IFDEF CalcDebug}'TokenClassNumber', '', nil, []{$ENDIF});
  TokenNumber_Update(ADouble);
end;

procedure TclParser.TokenClassHexNumber;
var
  AValue: string;
  AInteger: integer;
begin
  AValue := Scanner.CurText;
  if not TryStrToInt(AValue, AInteger) then
    ErrorParser({$IFDEF CalcDebug}'TokenClassHexNumber', '', nil, []{$ENDIF});
  TokenNumber_Update(AInteger);
end;

procedure TclParser.TokenNumber_Update(Value: Double);
var
  Last, Last2: TccBase;
  IsMinus: boolean;
begin
  Scanner.Push;
  IsMinus := false;
  Last := Context.Childs.ItemsTry[-1];
  if Last <> nil then
  begin
    // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing  TccSeparator
    if Last is TccOperator then
    begin
      Last2 := Context.Childs.ItemsTry[-2];
      if (Last2 = nil) or (Last2 is TccOperator) then
      begin
        if not(TccOperator(Last).Value in [opPlus, opMinus]) then
          ErrorParser({$IFDEF CalcDebug}'TokenNumber_Update', '1', nil,[]{$ENDIF});
        IsMinus := TccOperator(Last).Value = opMinus;

        Scanner.Caret.AddCaret(Last.Caret);
        FreeAndNil(Last);
      end;
    end
    else if not(Last is TccSeparator) then
      ErrorParser({$IFDEF CalcDebug}'TokenNumber_Update', '2', nil, []{$ENDIF});
  end;
  if IsMinus then
    Value := -Value;
  TokenNumber_Create(Value);
  Scanner.Pop;
end;

procedure TclParser.TokenNumber_Create(Value: Double);
begin
  Scanner.Push;
  Scanner.CurUpdateSelect;
  Context.AddNumber(Value).CaretUpdate(Scanner);
  Scanner.Pop;
end;

procedure TclParser.TokenClassOperator;
var
  I: integer;
  Op: Syntax.TEnumOperator;
begin
  if not Scanner.IsCurToken(Syntax.sopMatOperators, @I) then
    ErrorParser({$IFDEF CalcDebug}'TokenClassOperator', '1', nil, []{$ENDIF});
  Op := Syntax.OpToEnum(Syntax.sopMatOperators[I]);
  if Op = opNone then
    ErrorParser({$IFDEF CalcDebug}'TokenClassOperator', '2', nil, []{$ENDIF});
  TokenOperator_Update(Op);
end;

procedure TclParser.TokenOperator_Update(Op: Syntax.TEnumOperator);
var
  Last: TccBase;
begin
  Last := Context.Childs.ItemsTry[-1];
  if Last <> nil then
  begin
    // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
    if Last is TccOperator then
    begin
      if not(Op in [opPlus, opMinus]) then
        ErrorParser({$IFDEF CalcDebug}'TokenOperator_Update', '1', nil, []{$ENDIF})

      else if not(TccOperator(Last).Value in [opPlus, opMinus]) then
        TokenOperator_Create(Op)
      else
      begin
        if Op = opMinus then
        begin
          if TccOperator(Last).Value = opMinus then
            TccOperator(Last).Value := opPlus
          else
            TccOperator(Last).Value := opMinus
        end;
        Last.Caret.WindowPosStop := Scanner.Caret.TokenPosStop;
        Last.Caret.Select;
      end;
    end
    else if (Last is TccSeparator) and not(Op in [opPlus, opMinus]) then
    begin
      ErrorParser({$IFDEF CalcDebug}'TokenOperator_Update', '2', nil,[]{$ENDIF})
    end
    else
      TokenOperator_Create(Op)

  end
  else if not(Op in [opPlus, opMinus]) then
    ErrorParser({$IFDEF CalcDebug}'TokenOperator_Update', '3', nil, []{$ENDIF})
  else
    TokenOperator_Create(Op);

end;

procedure TclParser.TokenOperator_Create(Op: Syntax.TEnumOperator);
begin
  Scanner.Push;
  Scanner.CurUpdateSelect;
  Context.AddOperator(Op).CaretUpdate(Scanner);
  Scanner.Pop;
end;

{
procedure TclParser.TokenClassSpecifical;
begin
end;
}

procedure TclParser.TokenClassBrasket;
var
  Last: TccBase;
begin
  if Scanner.IsCurToken(Syntax.FuncBrasketBegin) then
  begin
    Last := Context.Childs.ItemsTry[-1];
    if Last <> nil then
    begin
      // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
      if Last is TccFunction then
      begin
        TokenFunction_BrasketBegin;
        exit;
      end
    end;
    TokenCombo_BrasketBegin;
    exit;
  end
  else if Scanner.IsCurToken(Syntax.FuncBrasketEnd) then
  begin

    if CtxCount <= 1 then
      ErrorParser({$IFDEF CalcDebug}'TokenClassBrasket', '1', nil, []{$ENDIF});

    // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
    if (CtxPeek is TccFunction) and (Context is TccCombo) then
    begin
      TokenFunction_BrasketEnd;
      exit;
    end
    else if (Context is TccCombo) or (Context is TccSymbolAssing) then
    begin
      TokenCombo_BrasketEnd;
      exit;
    end
    else
      ErrorParser({$IFDEF CalcDebug}'TokenClassBrasket', '2', nil, []{$ENDIF});
  end
  else if Scanner.IsCurToken(Rule.Brackets.RecChars.ArrOpen, nil) then
  begin
    TokenCombo_BrasketBegin;
  end
  else if Scanner.IsCurToken(Rule.Brackets.RecChars.ArrClose, nil) then
  begin
    if (CtxPeek is TccCombo) or (CtxPeek is TccSymbolAssing) then
    begin
      TokenCombo_BrasketEnd;
      exit;
    end
    else
      ErrorParser({$IFDEF CalcDebug}'TokenClassBrasket', '3', nil, []{$ENDIF})
  end
  else
    ErrorParser({$IFDEF CalcDebug}'TokenClassBrasket', '4', nil, []{$ENDIF});
end;

procedure TclParser.TokenFunctionSys_LiteralCase;
var
  I: integer;
begin
  if not Scanner.IsCurToken(Syntax.sFunc, @I) then
    ErrorParser({$IFDEF CalcDebug}'TokenFunctionSys_LiteralCase', '1', nil,[]{$ENDIF});
  if Syntax.TEnumFunc(I) = fnNone then
    ErrorParser({$IFDEF CalcDebug}'TokenFunctionSys_LiteralCase', '2', nil,[]{$ENDIF});
  TokenFunctionSys_Create(Syntax.TEnumFunc(I));
end;

procedure TclParser.TokenFunctionSys_Create(AName: Syntax.TEnumFunc);
var
  Func: TccFunctionSys;
begin
  // при создании любой вызываемой функции, контекст парсинга меняется когда будет получена скобка
  // это произойдет на следующей итерации парсинга
  // если функция не имеет агрументов то и не будет смены контекста
  // мы узнаем верно ли были переданы агрументы только при вычислении значения в  runner
  Scanner.Push;
  Scanner.CurUpdateSelect;
  Func := Context.AddFunctionSys(AName);
  Func.CaretUpdate(Scanner);
  Scanner.Pop;
end;

procedure TclParser.TokenFunctionAddr_Create(CountArgs:integer;Call:Pointer);
var Func: TccFunctionAddr;
begin
  Scanner.Push;
  Scanner.CurUpdateSelect;
  Func := Context.AddFunctionAddr(Scanner.CurText,CountArgs,Call);
  Func.CaretUpdate(Scanner);
  Scanner.Pop;
end;

procedure TclParser.TokenFunction_BrasketBegin;
var
  Last: TccBase;
  C: TccCombo;
begin
  Last := self.Context.Childs.ItemsTry[-1];
  if (Last = nil) or not(Last is TccFunction) then
    ErrorParser({$IFDEF CalcDebug}'TokenFunction_BrasketBegin', '', nil,[]{$ENDIF});

  Scanner.Match(Syntax.FuncBrasketBegin);
  CtxPush(TccFunction(Last));
  // пушим новый контекст функции которую создали шаг назад
  ContextUpdateCaret;

  Scanner.Push;
  Scanner.CurUpdateSelect;
  C := Context.AddCombo;
  C.CaretUpdate(Scanner);
  CtxPush(C);
  // пушим комбо для агрумента тогда синтаксис будет func(1,3.7+6) где запятая разделитель агрументов
  Scanner.Pop;

  // пробовать пример max(min(100/20,15^2),max(2+4,8.67))

end;

procedure TclParser.TokenFunction_BrasketEnd;
begin
  Scanner.Match(Syntax.FuncBrasketEnd);
  if not((CtxPeek is TccFunction) and (Context is TccCombo)) then
    ErrorParser({$IFDEF CalcDebug}'TokenFunction_BrasketEnd', '', nil,[]{$ENDIF});
  self.CtxPop; // TccCombo
  ContextUpdateCaret;
  self.CtxPop; // TccFunction
  ContextUpdateCaret;
end;

procedure TclParser.TokenFunction_SeparatorComma;
var
  Ops: Syntax.TEnumSeparator;
  C: TccCombo;
begin
  Ops := Syntax.OpsToEnum(Scanner.CurChar);
  if Ops <> opsComma then
    ErrorParser({$IFDEF CalcDebug}'TokenFunction_SeparatorComma', '', nil,[]{$ENDIF});

  // вернем контекс на функцию
  self.CtxPop; // TccCombo
  ContextUpdateCaret;

  // в функции добавим сепаратор
  TokenSeparator_Create(Ops);

  // создадим новый combo для следующего агрумента
  Scanner.Push;
  Scanner.CurUpdateSelect;
  C := Context.AddCombo;
  C.CaretUpdate(Scanner);
  CtxPush(C); // 2
  Scanner.Pop;

  // возможно тогда ситаксис не оч удобный будет  max(1,3+5)
  // не понятно 1.3 +5 или  max( (1) ,(3+5) )
  // короч 1 й вариант остается как в delphi
end;

procedure TclParser.TokenCombo_BrasketBegin();
var
  Last: TccBase;
  Index: integer;
  C: TccCombo;
begin
  Index := -1;
  if not Scanner.IsCurToken(Syntax.FuncBrasketBegin)
  and not Scanner.IsCurToken(Rule.Brackets.RecChars.ArrOpen, @Index) then
    ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketBegin', '1', nil,[]{$ENDIF});

  Last := Context.Childs.ItemsTry[-1];
  if Last <> nil then
  begin
    // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator
    if not(Last is TccOperator) and not(Last is TccSeparator) then
      ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketBegin', '2', nil,[]{$ENDIF});
  end;
  BrasketsPush(Index);
  Scanner.Push;
  Scanner.CurUpdateSelect;
  C := Context.AddCombo;
  C.CaretUpdate(Scanner);
  CtxPush(C);
  Scanner.Pop;
end;

procedure TclParser.TokenCombo_BrasketEnd;
var
  Last: TccBase;
  Index1: integer;
begin
  Index1 := -1;
  if not Scanner.IsCurToken(Syntax.FuncBrasketEnd) and
    not Scanner.IsCurToken(Rule.Brackets.RecChars.ArrClose, @Index1) then
    ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketEnd', '1', nil,[]{$ENDIF});

  Last := Context.Childs.ItemsTry[-1];
  if Last = nil then
    ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketEnd', '2', nil,[]{$ENDIF});

  // *CLASSES* TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing  TccSeparator
  if (Last is TccOperator)
  or (Last is TccSymbolAssing)
   or (Last is TccSeparator)  then
    ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketEnd', '3', nil,[]{$ENDIF});

  if self.BrasketsCount <= 0 then
    ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketEnd', '4', nil,[]{$ENDIF});
  if self.BrasketsPeek <> Index1 then
    ErrorParser({$IFDEF CalcDebug}'TokenCombo_BrasketEnd', '5', nil,[]{$ENDIF});
  self.BrasketsPop;
  self.CtxPop;
end;

procedure TclParser.ContextUpdateCaret();
begin
  Context.Caret.AddCaretStep(Scanner.Caret);
end;

end.

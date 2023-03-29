unit cl.Calc.DeCompiler;

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
  TclDeCompiler = class(TclObject)
  strict private[weak]
    FRule: TclRule;
    [weak]
    FContextRoot: TccComboRoot;
    [weak]
    FScanner: TclScanner;
    Return: TStringBuilder;
    FStackTab: integer;
    FNotError: boolean;
  protected
    function Add(S: string): TclDeCompiler;
    function EndLn: TclDeCompiler;
    function TabInteranal(Count: integer): TclDeCompiler;
    function TabPush: TclDeCompiler;
    function TabPop: TclDeCompiler;
    function Tab: TclDeCompiler;
    function Space: TclDeCompiler;

    property Scanner: TclScanner read FScanner;
    property Rule: TclRule read FRule write FRule;
    property ContextRoot: TccComboRoot read FContextRoot write FContextRoot;

    procedure ErrorDeCompiler(Ctx: TccBase);

    procedure ctxBase(Ctx: TccBase);
    procedure ctxSymbolAssing(Ctx: TccSymbolAssing);
    procedure ctxFunction(Ctx: TccFunction);
    procedure ctxComboRoot(Ctx: TccComboRoot);
    procedure ctxCombo(Ctx: TccCombo);
    procedure ctxNumber(Ctx: TccNumber);
    procedure ctxSymbol(Ctx: TccSymbol);
    procedure ctxOperator(Ctx: TccOperator);
    procedure ctxSeparator(Ctx: TccSeparator);
  public
    procedure Reset;
    property NotError: boolean read FNotError write FNotError;
    function Execute(AContext: TccComboRoot; AScanner: TclScanner;
      ARule: TclRule): string;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TclDeCompiler }

constructor TclDeCompiler.Create;
begin
  inherited;
  FNotError := false;
  FRule := nil;
  FContextRoot := nil;
  Return := TStringBuilder.Create;
end;

destructor TclDeCompiler.Destroy;
begin
  FRule := nil;
  FContextRoot := nil;
  FreeAndNil(Return);
  inherited;
end;

function TclDeCompiler.Add(S: string): TclDeCompiler;
begin
  Result := self;
  Return.Append(S);
end;

function TclDeCompiler.EndLn: TclDeCompiler;
begin
  Result := self;
  Add(#13#10);
end;

function TclDeCompiler.Space: TclDeCompiler;
begin
  Result := self;
  Add(' ');
end;

function TclDeCompiler.TabInteranal(Count: integer): TclDeCompiler;
var
  i: integer;
begin
  Result := self;
  for i := 0 to Count - 1 do
    Add('    ');
end;

function TclDeCompiler.Tab: TclDeCompiler;
begin
  Result := self;
  TabInteranal(FStackTab);
end;

function TclDeCompiler.TabPush: TclDeCompiler;
begin
  Result := self;
  if FStackTab < 0 then
    ErrorDeCompiler(nil);
  inc(FStackTab);
end;

function TclDeCompiler.TabPop: TclDeCompiler;
begin
  Result := self;
  dec(FStackTab);
  if FStackTab < 0 then
    ErrorDeCompiler(nil);
end;

procedure TclDeCompiler.Reset;
begin
  FStackTab := 0;
  ContextRoot := nil;
  FRule := nil;
  Return.Clear;
end;

function TclDeCompiler.Execute(AContext: TccComboRoot; AScanner: TclScanner;
  ARule: TclRule): string;
var
  i: integer;
begin
  Reset;
  Result := '';
  ContextRoot := AContext;
  FScanner := AScanner;
  try
    ctxComboRoot(ContextRoot);
  finally
    ContextRoot := nil;
    FScanner := nil;
  end;
  Result := Return.ToString;
end;

procedure TclDeCompiler.ErrorDeCompiler(Ctx: TccBase);
begin
  if not FNotError then
    exit;

  if Ctx = nil then
    raise Exception.Create('Error cl.Calc.DeCompiler.ErrorDeCompiler no class')
  else
    raise Exception.Create('Error ErrorDeCompiler ' + Ctx.ClassName);
end;

procedure TclDeCompiler.ctxBase(Ctx: TccBase);
begin

  // TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator

  // TccEval =  TccNumber  TccFunction  TccCombo  TccSymbolAssing
  if Ctx is TccComboRoot then
    ErrorDeCompiler(Ctx)

  else if Ctx is TccSymbolAssing then
    ctxSymbolAssing(TccSymbolAssing(Ctx))

  else if Ctx is TccFunction then
    ctxFunction(TccFunction(Ctx))

  else if Ctx is TccCombo then
    ctxCombo(TccCombo(Ctx))

  else if Ctx is TccNumber then
    ctxNumber(TccNumber(Ctx))

  else if Ctx is TccSymbol then
    ctxSymbol(TccSymbol(Ctx))

  else if Ctx is TccOperator then
    ctxOperator(TccOperator(Ctx))

  else if Ctx is TccSeparator then
    ctxSeparator(TccSeparator(Ctx))

  else
    ErrorDeCompiler(Ctx)

end;

procedure TclDeCompiler.ctxSymbolAssing(Ctx: TccSymbolAssing);
var
  i: integer;
begin
  Add(Ctx.Variable.Name).Add(Syntax.Assing).Space;
  for i := 0 to Ctx.Childs.Count - 1 do
    ctxBase(Ctx.Childs[i]);
end;

procedure TclDeCompiler.ctxFunction(Ctx: TccFunction);
var
  i: integer;
begin
  if Ctx is TccFunctionSys then
    Space.Add(Syntax.sFunc[TccFunctionSys(Ctx).NameFunc])
  else if Ctx is TccFunctionAddr then
    Space.Add(TccFunctionAddr(Ctx).NameFunc)
  else
    ErrorDeCompiler(Ctx);

  if Ctx.Childs.Count <= 0 then
   exit;

  if not(Ctx.Childs.Items[0] is TccCombo) then
    ErrorDeCompiler(Ctx);

  if TccCombo(Ctx.Childs.Items[0]).Childs.Count > 0 then
  begin
    Add(Syntax.FuncBrasketBegin);
    for i := 0 to Ctx.Childs.Count - 1 do
      ctxBase(Ctx.Childs[i]);
    Space.Add(Syntax.FuncBrasketEnd);
  end;


end;

procedure TclDeCompiler.ctxComboRoot(Ctx: TccComboRoot);
var
  i: integer;
begin
  for i := 0 to Ctx.VariableTabl.Count - 1 do
    Add(Ctx.VariableTabl.Item(i).Name).Add(Syntax.DotWithComma);
  EndLn;
  ctxCombo(Ctx);
end;

procedure TclDeCompiler.ctxCombo(Ctx: TccCombo);
var i: integer;
begin

  Ctx.CaretText[FScanner];
  if Ctx <> ContextRoot then
  begin
    // s:= Ctx.Parent.ClassName;
    if (Ctx.Parent <> nil) and ((Ctx.Parent is TccFunction)
      // or (Ctx.Parent is TccSymbolAssing)
      ) then
      // Add(Syntax.FuncBrasketBegin)
    else
      Space.Add(Syntax.FuncBrasketBegin);
  end;

  for i := 0 to Ctx.Childs.Count - 1 do
    ctxBase(Ctx.Childs[i]);

  if Ctx <> ContextRoot then
  begin
    if (Ctx.Parent <> nil) and ((Ctx.Parent is TccFunction)
      // or (Ctx.Parent is TccSymbolAssing)
      ) then
      // Add(Syntax.FuncBrasketEnd)
    else
      Space.Add(Syntax.FuncBrasketEnd);
  end;

end;

procedure TclDeCompiler.ctxNumber(Ctx: TccNumber);
begin
  Space.Add(Ctx.Value.ToString.Replace(',', '.'));
end;

procedure TclDeCompiler.ctxSymbol(Ctx: TccSymbol);
begin
  Space.Add(Ctx.Variable.Name);
end;

procedure TclDeCompiler.ctxOperator(Ctx: TccOperator);
begin
  Space.Add(Syntax.OpEnumToStr(Ctx.Value));
end;

procedure TclDeCompiler.ctxSeparator(Ctx: TccSeparator);
begin
  if Ctx.Value = opsDotWithComma then
    Space.Add(Syntax.DotWithComma).EndLn

  else if (Ctx.Value = opsComma) and (Ctx.Parent <> nil) and
    (Ctx.Parent is TccFunction) then
    Space.Add(Syntax.Comma)

  else
    ErrorDeCompiler(Ctx)
end;

end.

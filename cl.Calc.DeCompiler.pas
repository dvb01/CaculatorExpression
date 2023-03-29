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
  TclDeCompiler = class (TclObject)
  strict private
    [weak]FRule: TclRule;
    [weak]FContextRoot: TccComboRoot;
    FReturn:TStringBuilder;
  protected
    procedure ErrorDeCompiler(Ctx:TccBase);
    property Rule: TclRule read FRule write FRule;
    property ContextRoot: TccComboRoot read FContextRoot write FContextRoot;
    procedure ctxBase(Ctx:TccBase);
    procedure ctxSymbolAssing(Ctx:TccSymbolAssing);
    procedure ctxFunction(Ctx:TccFunction);
    procedure ctxComboRoot(Ctx:TccComboRoot);
    procedure ctxCombo(Ctx:TccCombo);
    procedure ctxNumber(Ctx:TccNumber);
    procedure ctxSymbol(Ctx:TccSymbol);
    procedure ctxOperator(Ctx:TccOperator);
    procedure ctxSeparator(Ctx:TccSeparator);
  public
    function Execute(AContext: TccComboRoot;ARule: TclRule):string;
    constructor Create();
    destructor Destroy; override;
  end;


implementation

{ TclDeCompiler }

constructor TclDeCompiler.Create;
begin
   inherited;
   FRule:=nil;
   FContextRoot:=nil;
   FReturn:=TStringBuilder.Create;
end;

destructor TclDeCompiler.Destroy;
begin
   FRule:=nil;
   FContextRoot:=nil;
   FreeAndNil(FReturn);
  inherited;
end;

function TclDeCompiler.Execute(AContext: TccComboRoot; ARule: TclRule): string;
var
  I: Integer;
begin
    Result:='';
    ErrorDeCompiler(nil);
    for I := 0 to AContext.Childs.Count-1 do
    begin

    end;

end;

procedure TclDeCompiler.ErrorDeCompiler(Ctx:TccBase);
begin
  raise Exception.Create('Error cl.Calc.DeCompiler.ErrorDeCompiler no code');

  raise Exception.Create('Error ErrorDeCompiler '+Ctx.ClassName);
end;

procedure TclDeCompiler.ctxBase(Ctx:TccBase);
begin

  // TccOperator  TccNumber TccFunction TccCombo  TccSymbol TccSymbolAssing TccSeparator

  // TccEval =  TccNumber  TccFunction  TccCombo  TccSymbolAssing

  if Ctx is TccSymbolAssing then
    ctxSymbolAssing(TccSymbolAssing(Ctx))

  else if Ctx is TccFunction then
    ctxFunction(TccFunction(Ctx))

  else if Ctx is TccComboRoot then
    ctxComboRoot(TccComboRoot(Ctx))

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

procedure TclDeCompiler.ctxSymbolAssing(Ctx:TccSymbolAssing);
begin

end;

procedure TclDeCompiler.ctxFunction(Ctx:TccFunction);
begin

end;

procedure TclDeCompiler.ctxComboRoot(Ctx:TccComboRoot);
begin

end;

procedure TclDeCompiler.ctxCombo(Ctx:TccCombo);
begin

end;

procedure TclDeCompiler.ctxNumber(Ctx:TccNumber);
begin

end;

procedure TclDeCompiler.ctxSymbol(Ctx:TccSymbol);
begin

end;

procedure TclDeCompiler.ctxOperator(Ctx:TccOperator);
begin

end;

procedure TclDeCompiler.ctxSeparator(Ctx:TccSeparator);
begin

end;



end.

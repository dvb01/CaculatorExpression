unit cl.Calc.Compiler.Types;

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
  cl.Calc.Exception,
  cl.Calc.Res;

type

  TclContextStack = class(TStack<TccExpression>)
  end;

  TclCompilerCustom = class abstract(TclObject)
  strict private
    [weak]FRule: TclRule;
    [weak]FContextRoot: TccComboRoot;
    [weak]FScanner: TclScanner;
    procedure RuleSet(const Value: TclRule);
    procedure ContextRootSet(const Value: TccComboRoot);
    procedure ScannerSet(const Value: TclScanner);
  protected
    procedure ErrorCompiler(); overload;
    procedure ErrorCompiler(const Place, FerstMsg: string; AMsg: PResStringRec;  Args: array of string); overload;
    procedure Start; virtual; abstract;
    procedure DoStart; virtual;
    property Scanner: TclScanner read FScanner write ScannerSet;
    property Rule: TclRule read FRule write RuleSet;
    property ContextRoot: TccComboRoot read FContextRoot write ContextRootSet;
  public
    procedure Execute(AContext: TccComboRoot; AScanner: TclScanner; ARule: TclRule); virtual;
    procedure Reset; virtual;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TclCompilerCustom }

constructor TclCompilerCustom.Create;
begin
  inherited;
  FRule := nil;
  FContextRoot := nil;
  FScanner := nil;
end;

destructor TclCompilerCustom.Destroy;
begin
  FRule := nil;
  FContextRoot := nil;
  FScanner := nil;
  inherited;
end;

procedure TclCompilerCustom.ContextRootSet(const Value: TccComboRoot);
begin
  FContextRoot := Value;
end;

procedure TclCompilerCustom.ErrorCompiler();
begin
  clErrorFactory.Compiler(self, '', '', nil, []);
end;

procedure TclCompilerCustom.ErrorCompiler(const Place, FerstMsg: string;
  AMsg: PResStringRec; Args: array of string);
begin
  clErrorFactory.Compiler(self, Place, FerstMsg, AMsg, Args);
end;

procedure TclCompilerCustom.Execute(AContext: TccComboRoot;
  AScanner: TclScanner; ARule: TclRule);
begin
  if (AContext = nil) or (AScanner = nil) or (ARule = nil) then
    raise Exception.CreateResFmt(@Rs_TclCompilerCustom_Execute, []);

  Rule := ARule;
  ContextRoot := AContext;
  Scanner := AScanner;
  try
    Scanner.Push;
    DoStart();
    Scanner.Pop;
  finally
    Rule := nil;
    ContextRoot := nil;
    Scanner := nil;
  end;
end;

procedure TclCompilerCustom.DoStart;
begin
  Start();
end;

procedure TclCompilerCustom.Reset;
begin
  FRule := nil;
  FContextRoot := nil;
  FScanner := nil;
end;

procedure TclCompilerCustom.RuleSet(const Value: TclRule);
begin
  FRule := Value;
end;

procedure TclCompilerCustom.ScannerSet(const Value: TclScanner);
begin
  FScanner := Value;
end;

end.

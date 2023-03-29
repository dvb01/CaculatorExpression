unit cl.Calc.Main;

interface

{$I cl.Calc.Define.inc}

uses
  System.SysUtils,
  System.Classes,
  cl.Calc.Scanner,
  cl.Calc.Option,
  cl.Calc.Types,
  cl.Calc.Classes,
  cl.Calc.Compiler.Parser,
  cl.Calc.Compiler.Runner,
  cl.Calc.Res,
  cl.Calc.DeCompiler;

type
  TclCalcCustom = class abstract(TclObjectLog)
  private
    FRule: TclRule;
    FParser: TclParser;
    FScanner: TclScanner;
    FRunner: TclRunner;
    [weak]FContextRoot: TccComboRoot;
    FReturn: double;
    FErrorMessage: string;
    FErrorPos: integer;
    FErrorLen: integer;
    FLockExecute: integer;
  protected
    function Run:boolean;virtual;
    function RunParser:boolean;
    function RunRunner:boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Rule: TclRule read FRule;
    procedure Reset;
    function Execute(TextExpression: string): boolean;
    property Return: double read FReturn;
    property Scanner: TclScanner read FScanner;
    property ErrorMessage: string read FErrorMessage;
    property ErrorPos: integer read FErrorPos;
    property ErrorLen: integer read FErrorLen;
  end;

  TclCalc = class(TclCalcCustom)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TclCalcDeComp  = class (TclCalc)
  private
    FReturnTextExpression:string;
  protected
    function Run:boolean;override;
   public
     property ReturnTextExpression: string read FReturnTextExpression;
  end;

implementation

{ TclCalc }

constructor TclCalcCustom.Create;
begin
  inherited;
  FRule := TclRule.Create;
  FParser := TclParser.Create;
  FReturn := 0;
  FLockExecute := 0;
  FContextRoot:=nil;
  FScanner := TclScanner.Create;
  FRunner := TclRunner.Create;
end;

destructor TclCalcCustom.Destroy;
begin
  FreeAndNil(FRunner);
  FreeAndNil(FParser);
  FreeAndNil(FRule);
  FreeAndNil(FScanner);
  inherited;
end;

procedure TclCalcCustom.Reset;
begin
  if FLockExecute > 0 then
    raise Exception.CreateResFmt(@Rs_TclCalcCustom_Reset, []);
  FParser.Reset;
  FRunner.Reset;
  FScanner.Reset;
  FReturn := 0;
  FErrorMessage := '';
  FErrorPos := 0;
  FErrorLen := 0;
  FContextRoot:=nil;
end;

function TclCalcCustom.Run: boolean;
begin
  Result:= RunParser;
  if not Result then
    exit;
  Result := RunRunner;
end;

function TclCalcCustom.RunParser:boolean;
begin
    Result:=false;
    try
      FParser.Execute(FContextRoot, FScanner, Rule);
      Result := true;
    except
      on e: Exception do
      begin
        FErrorMessage := e.Message;
        FScanner.PosToRich(FErrorPos, FErrorLen);
        Log('Error TclCalcCustom.RunParser ' + FErrorMessage, e);
        if Rule.CanShowError then
          raise;
      end;
    end;
end;

function TclCalcCustom.RunRunner:boolean;
begin
    Result:=false;
    try
      FRunner.Execute(FContextRoot, FScanner, Rule);
      FReturn := FRunner.Return;
      Result := true;
    except
      on e: Exception do
      begin
        FErrorMessage := e.Message;
        FScanner.PosToRich(FErrorPos, FErrorLen);
        Log('Error TclCalcCustom.RunRunner ' + FErrorMessage, e);
        if Rule.CanShowError then
          raise;
      end;
    end;
end;

function TclCalcCustom.Execute(TextExpression: string): boolean;

begin
  if FLockExecute > 0 then
    raise Exception.CreateResFmt(@Rs_TclCalcCustom_Execute, []);
  Reset;
  if length(TextExpression) <= 0 then
    exit(true);
  inc(FLockExecute);
  try
    Rule.ParsBefore;
    FScanner.Braskets := Rule.Brackets.RecChars.Open + Rule.Brackets.RecChars.Close;
    try
      FContextRoot := TccComboRoot.Create();
      try
         FScanner.Buffer := TextExpression;
         Result:=Run;
      finally
        FreeAndNil(FContextRoot);
      end;
    finally
      Rule.ParsAfter;
    end;
  finally
    dec(FLockExecute);
  end;

end;

{ TclCalc }

constructor TclCalc.Create;
begin
  inherited;
  Rule.Brackets.Registred('(', ')');
  Rule.Brackets.Registred('[', ']');
  Rule.Brackets.Registred('{', '}');
  Rule.CanShowError := false;
end;

destructor TclCalc.Destroy;
begin

  inherited;
end;

{ TclCalcDeComp }

function TclCalcDeComp.Run: boolean;
var D:TclDeCompiler;
begin
  Result:= RunParser;
  if not Result then
    exit;
  D :=TclDeCompiler.Create;
  try

    FReturnTextExpression:=  D.Execute(FContextRoot,FScanner,Rule);
    Result:=true;
  finally
    D.Free;
  end;
end;

end.

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
  cl.Calc.Res;

type
  TclCalcCustom = class(TclObjectLog)
  private
    FRule: TclRule;
    FParser: TclParser;
    FScanner: TclScanner;
    FRunner: TclRunner;
    FReturn: double;
    FErrorMessage: string;
    FErrorPos: integer;
    FErrorLen: integer;
    FLockExecute: integer;
  protected
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

implementation

{ TclCalc }

constructor TclCalcCustom.Create;
begin
  inherited;
  FRule := TclRule.Create;
  FParser := TclParser.Create;
  FReturn := 0;
  FLockExecute := 0;
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
end;

function TclCalcCustom.Execute(TextExpression: string): boolean;
var
  Context: TccComboRoot;
begin
  if FLockExecute > 0 then
    raise Exception.CreateResFmt(@Rs_TclCalcCustom_Execute, []);
  Result := false;
  Reset;
  if length(TextExpression) <= 0 then
    exit(true);

  inc(FLockExecute);
  try
    Rule.ParsBefore;
    FScanner.Braskets := Rule.Brackets.RecChars.Open + Rule.Brackets.RecChars.Close;
    try
      Context := TccComboRoot.Create();
      try
        FScanner.Buffer := TextExpression;
        try
          FParser.Execute(Context, FScanner, Rule);
          Result := true;
        except
          on e: Exception do
          begin
            FErrorMessage := e.Message;
            FScanner.PosToRich(FErrorPos, FErrorLen);
            self.Log('Error TclCalcCustom.Execute.Parser ' + FErrorMessage, e);
            if Rule.CanShowError then
              raise;
          end;
        end;

        if not Result then
          exit;
        Result := false;

        try
          FRunner.Execute(Context, FScanner, Rule);
          FReturn := FRunner.Return;
          Result := true;
        except
          on e: Exception do
          begin
            FErrorMessage := e.Message;
            FScanner.PosToRich(FErrorPos, FErrorLen);
            self.Log('Error TclCalcCustom.Execute.Runner ' + FErrorMessage, e);
            if Rule.CanShowError then
              raise;
          end;
        end;

      finally
        Context.Free;
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

end.

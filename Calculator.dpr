program Calculator;

uses
  Vcl.Forms,
  cl.Main.Form in 'cl.Main.Form.pas' {FormCalc},
  cl.Calc.Types in 'cl.Calc.Types.pas',
  cl.Main.Setting.Form in 'cl.Main.Setting.Form.pas' {FormCalcSetting},
  cl.Calc.Res in 'cl.Calc.Res.pas',
  cl.Calc.Classes in 'cl.Calc.Classes.pas',
  cl.Calc.Main in 'cl.Calc.Main.pas',
  cl.Main.Types in 'cl.Main.Types.pas',
  cl.Calc.Syntax in 'cl.Calc.Syntax.pas',
  cl.Calc.Utils in 'cl.Calc.Utils.pas',
  cl.Calc.Option in 'cl.Calc.Option.pas',
  cl.Calc.Compiler.Parser in 'cl.Calc.Compiler.Parser.pas',
  cl.Calc.Scanner in 'cl.Calc.Scanner.pas',
  cl.Calc.Compiler.Runner in 'cl.Calc.Compiler.Runner.pas',
  cl.Calc.Compiler.Types in 'cl.Calc.Compiler.Types.pas',
  cl.Calc.Exception in 'cl.Calc.Exception.pas',
  cl.Main.Ref.Form in 'cl.Main.Ref.Form.pas' {FormCaclRef},
  cl.Calc.GeneratorExpression in 'cl.Calc.GeneratorExpression.pas',
  cl.Calc.DeCompiler in 'cl.Calc.DeCompiler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormCalc, FormCalc);
  Application.CreateForm(TFormCalcSetting, FormCalcSetting);
  Application.CreateForm(TFormCaclRef, FormCaclRef);
  Application.Run;
end.

unit cl.Calc.GeneratorExpression;

interface
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
    TctItemTest = record
     IsValidText:boolean;
     Text:string;
     CorrectAnswer:double;// Math.RoundTo(CorrectAnswer,-8)
    end;
    TctPrm= record
      CountVariable:integer;

    end;


    // получить выражения после чего можно их тестировать

    TctGenRoot = class (TclObject)
    private
     // FContextRoot: TccComboRoot;
    public
      procedure New(var ArrTest:TArray<TctItemTest>;Count:integer);
    end;


   TctGenItem =  class (TclObject)
     //FContextRoot: TccComboRoot;
   end;


    {

       0..9 a..z braskets operators

    }

implementation

procedure TctGenRoot.New(var ArrTest:TArray<TctItemTest>;Count:integer);
begin
    raise Exception.Create('Error cl.Calc.GeneratorExpression.TctGen.New no code');
    // ????
end;

end.

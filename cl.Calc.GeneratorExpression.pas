unit cl.Calc.GeneratorExpression;

interface
uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo;

  type
    TctItemTest = record
     Text:string;
     CorrectAnswer:double;// Math.RoundTo(CorrectAnswer,-8)
    end;


    // получить валидные выражения после чего можно их тестировать

    TctGen = class

    public
      procedure New(var ArrTest:TArray<TctItemTest>;Count:integer);
    end;

    {

       1 + 2

    }

implementation

procedure TctGen.New(var ArrTest:TArray<TctItemTest>;Count:integer);
begin
    // ????
end;

end.

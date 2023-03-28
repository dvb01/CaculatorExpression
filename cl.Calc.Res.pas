unit cl.Calc.Res;

interface
{$I cl.Calc.Define.inc}

resourcestring
  Rs_CaclDefault = 'Ошибка в калькуляторе в классе ["%s"] ';
  Rs_TclScanner_BufferSet = 'Error TclScanner.BufferSet Нельзя изменять текст выражения, если он уже парсится';
  Rs_TclScanner_CurUpdate = 'Error TclScanner.CurUpdate Параметры обновления каретки не валидны';
  Rs_TclCaret_Invalid = 'Error TclCaret.CreateError Пераметры каретки не валидны';


  Rs_TclRuleBrackets_Registred  = 'Error TclRuleBrackets.Registred попытка добавления дубликата скобочного символа';
  Rs_TclRuleBrackets_Locked  = 'Error TclRuleBrackets заблокирован для добавления и удаления элементов';
  Rs_TclCompilerCustom_Execute = 'Error TclCompilerCustom.Execute не валидные параметры на входе';

  Rs_TclParser_BrasketsPush = 'Error TclParser.BrasketsPush Выход за пределы диапозона ShortInt. Кол-во разновидностей скобок превыщено';
  Rs_TclRunner_ExpСalculation_InvalidOperator = 'Error TclRunner.ExpСalculation в выражении есть не валидный оператор';
  Rs_TclRunner_ExpСalculation_OperatorBeginOrEnd = 'Error TclRunner.ExpСalculation оператор находится в начале или в конце выражания';

  Rs_TclCalcCustom_Reset = 'Error TclCalcCustom.Reset запрещено выполнять Reset, т.к выпоняется вычисление';
  Rs_TclCalcCustom_Execute = 'Error TclCalcCustom.Execute уже было запущено одно вычисление';

  Rs_clInvalidToken = 'неожиданный токен в выражении ["%s"] ошибка в ["%s"] позиция:[%s] длинна:[%s]';

implementation

end.

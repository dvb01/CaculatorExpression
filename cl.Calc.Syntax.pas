unit cl.Calc.Syntax;

interface
{$I cl.Calc.Define.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Character,
  System.SyncObjs,
  System.Generics.Collections,
  cl.Calc.Res,
  cl.Calc.Types,
  cl.Calc.Utils;

  type



  Syntax = record
  type

    TEnumToken = (tcNone,tcAssing, tcSeparator, tcLiteral,
                 tcNumber,tcHexNumber,tcOperator,tcSpecifical,
                 tcBrasket);


    TEnumOperator = (opNone, opPlus, opMinus, opMult, opDiv,opDivWhole,opPower,opMod,
                     opShl, opShr, opAnd, opOr, opXor );
    TEnumOperators = set of TEnumOperator;

    TEnumSeparator = (opsNone, opsDotWithComma, opsComma);
    TEnumExpected = (opeEval, opeSepa);



    // при парсинге выражения. классы ожидаемого токена
   // TEmumExpectedToken = (opeAny, opeOperator, opeNumber, opeDot, opeComma);
   // TExpectedTokens = set of TEmumExpectedToken;
  private
   const
    symbolPlus1 = '+';
    symbolMinus1 = '-';
    symbolMult1 = '*';
    symbolMult2 = 'x';
    symbolDiv1 = '/';
    symbolDiv2 = ':';
    symbolDiv3 = 'div';
    symbolMod1 = 'mod';
    symbolMod2 = '%';
    symbolPower1 = '^';

    symbol_shl = 'shl';
    symbol_shr = 'shr';
    symbol_and = 'and';
    symbol_or = 'or';
    symbol_xor = 'xor';
  public

  const


    sopPlus          : array [0..0] of string = ( symbolPlus1 );
    sopMinus         : array [0..0] of string = ( symbolMinus1 );

    sopMult          : array [0..1] of string = ( symbolMult1,symbolMult2 );
    sopDiv           : array [0..1] of string = ( symbolDiv1,symbolDiv2 );
    sopDivWhole      : array [0..0] of string = ( symbolDiv3 );

    sopMod           : array [0..1] of string = ( symbolMod1,symbolMod2 );
    sopPower         : array [0..0] of string = ( symbolPower1 );

    sopShl         : array [0..0] of string = ( symbol_shl );
    sopShr         : array [0..0] of string = ( symbol_shr );
    sopAnd         : array [0..0] of string = ( symbol_and );
    sopOr         : array [0..0] of string = ( symbol_or );
    sopXor         : array [0..0] of string = ( symbol_xor );


    sopMatOperators  : array [0..14] of string =   (
                                                   symbolPlus1,
                                                   symbolMinus1,
                                                   symbolMult1,
                                                   symbolMult2,
                                                   symbolDiv1,
                                                   symbolDiv2,
                                                   symbolDiv3,
                                                   symbolMod1,
                                                   symbolMod2,
                                                   symbolPower1,
                                                   symbol_shl,
                                                   symbol_shr,
                                                   symbol_and,
                                                   symbol_or,
                                                   symbol_xor
                                                   );



    sopPlusMinus: array [0..1]  of string  = (symbolPlus1,symbolMinus1);



    Numbers: TSetOfChar = ['0'..'9'];
    Eng: TSetOfChar = ['A'..'z'];
    Ru: TSetOfChar = ['А'..'я'];
    Separators: TSetOfChar = [',',';'];
    Comma =',';
    DotWithComma =';';
    Assing = ':=';
    EndValue = [#32, #13, #10, #0];
    Spaces = [#32, #8, #9, #11, #12];
    LnBreak =  #13;
    LnCaret =  #10;
    FuncBrasket = ['(',')'];
    FuncBrasketBegin ='(';
    FuncBrasketEnd =')';

    class function OpToEnum(const OpValue: string): TEnumOperator; static;
    class function OpEnumToStr(const Op: TEnumOperator): string; static;
    class function OpsToEnum(const OpsValue: Char): TEnumSeparator; static;
    class procedure OpToList(L:TStrings); static;
    //функции

   type
    TEnumFunc = (fnNone, fnArcSin, fnArcCos, fnTan, fnCotan,fnSinCos,fnSin,fnCos,
                fnLog10,fnLog2,fnLogN,fnMax,fnMin,fnPower,fnPi,fnRand);
    const

     sfnArcSin ='ArcSin';
     sfnArcCos ='ArcCos';
     sfnTan ='Tan';
     sfnCotan ='Cotan';
     sfnSinCos ='SinCos';
     sfnSin ='Sin';
     sfnCos ='Cos';
     sfnLog10 ='Log10';
     sfnLog2 ='Log2';
     sfnLogN ='LogN';
     sfnMax  ='Max';
     sfnMin  ='Min';
     sfnPower  ='Power';
     sfnPi ='Pi';
     sfRand = 'Rand';

     sFunc: array [TEnumFunc]  of string  = (
                                        '',
                                        sfnArcSin,
                                        sfnArcCos,
                                        sfnTan,
                                        sfnCotan,
                                        sfnSinCos,
                                        sfnSin,
                                        sfnCos,
                                        sfnLog10,
                                        sfnLog2,
                                        sfnLogN,
                                        sfnMax,
                                        sfnMin,
                                        sfnPower,
                                        sfnPi,
                                        sfRand
                                       );
     sFuncArg: array [TEnumFunc]  of integer  = (
                                        0,
                                        1,
                                        1,
                                        1,
                                        1,
                                        3,
                                        1,
                                        1,
                                        1,
                                        1,
                                        2,
                                        2,
                                        2,
                                        2,
                                        0,
                                        1
                                       );

     class function FuncToEnum(const FuncValue: string): TEnumFunc; static;

     // как добавить новую функцию
     ///
     ///  1.добавить конастанту в Syntax с именем sfn....
     ///  2. добавить эту константу в sFunc и в sFuncArg кол-во агрументов double
     ///  3. добавить в конец TEnumFunc
     ///  4. перейти в модуль cl.Calc.Compiler.Runner
     ///  5 function TclRunner.GetValueFunctionSys  +- 215 ln
     ///  6. в конце case добавить вызов самой функции
     ///

     ///  добавить новую функцию без изменения кода
     type
      TclSyntaxCallFunc = function ():double;
      TclSyntaxCallFunc1 = function (x1:double):double;
      TclSyntaxCallFunc2 = function (x1,x2:double):double;
      TclSyntaxCallFunc3 = function (x1,x2,x3:double):double;
      TclSyntaxCallFunc4 = function (x1,x2,x3,x4:double):double;
      TclSyntaxCallFunc5 = function (x1,x2,x3,x4,x5:double):double;
      TclSyntaxCallFunc6 = function (x1,x2,x3,x4,x5,x6:double):double;
      TclSyntaxCallFunc7 = function (x1,x2,x3,x4,x5,x6,x7:double):double;

      class function FuncReg(const FuncName: string;AddrCall:TclSyntaxCallFunc): boolean; static;
      class function FuncReg1(const FuncName: string;AddrCall:TclSyntaxCallFunc1): boolean; static;
      class function FuncReg2(const FuncName: string;AddrCall:TclSyntaxCallFunc2): boolean; static;
      class function FuncReg3(const FuncName: string;AddrCall:TclSyntaxCallFunc3): boolean; static;
      class function FuncReg4(const FuncName: string;AddrCall:TclSyntaxCallFunc4): boolean; static;
      class function FuncReg5(const FuncName: string;AddrCall:TclSyntaxCallFunc5): boolean; static;
      class function FuncReg6(const FuncName: string;AddrCall:TclSyntaxCallFunc6): boolean; static;
      class function FuncReg7(const FuncName: string;AddrCall:TclSyntaxCallFunc7): boolean; static;
      class function FuncRegCustom(const FuncName: string;AddrCall:Pointer;CountArg:integer): boolean; static;
      class function FuncUnReg(const FuncName: string): boolean; overload;static;
      class function FuncUnReg(AddrCall:Pointer): boolean;overload; static;
      class function FuncGet (const Name:string;out Addr:Pointer;out CountArgs:integer):boolean;static;

     // получить список функций
      class procedure FuncAllToList (L:TStrings);static;
      class function FuncToString(const Name:string;CountArgs:integer):string; static;
  end;

implementation
 type
   TGlobalFunc = class
     type
      TItem = record
        Name:string;
        Call:Pointer;
        Args:integer;
      end;
      PItem = ^TItem;
    strict private
     class var FGlobalFunc:TGlobalFunc;
     var
     List:TList;// в идеале hash таблицу делать нужно но кальк же демка поэтому пока так
     Cs:TCriticalSection;
     function pGetIndex(Index:integer):PItem;
     procedure  pFreeIndex(Index:integer);
     procedure  pClear;
     function pIndexofName(const Name:string):integer;
     function pIndexofAddr(Addr:Pointer):integer;
     function pGet(const Name:string;out Addr:Pointer; out CountArgs:integer):boolean;
     function pAdd(const Name:string;Addr:Pointer;CountArgs:integer):boolean;
     function pRemoveName(const Name:string):boolean;  overload;
     function pRemoveAddr(Addr:Pointer):boolean; overload;
     procedure pFuncAddToList (L:TStrings);
    private
     class procedure GlobalListDestroy;
    public
     function Get(const Name:string;out Addr:Pointer;out CountArgs:integer):boolean;
     function Add(const Name:string;Addr:Pointer;CountArgs:integer):boolean;
     function Remove(const Name:string):boolean;  overload;
     function Remove(Addr:Pointer):boolean; overload;
     procedure FuncAddToList (L:TStrings);
     constructor Create;
     destructor Destroy; override;
     class function GlobalList: TGlobalFunc;
   end;




{ TGlobalFunc }

constructor TGlobalFunc.Create;
begin
    inherited;
    FGlobalFunc:=self;
    List:=TList.Create;
    Cs:=TCriticalSection.Create;
end;

destructor TGlobalFunc.Destroy;
begin
    pClear;
    FreeAndNil(List);
    FreeAndNil(Cs);
    inherited;
end;

class procedure TGlobalFunc.GlobalListDestroy;
begin
    FGlobalFunc.Free;
    FGlobalFunc:=nil;
end;

class function TGlobalFunc.GlobalList: TGlobalFunc;
begin
 Result:=   TInterlocked.CompareExchange<TGlobalFunc>(FGlobalFunc,nil,nil);
end;

procedure TGlobalFunc.FuncAddToList(L:TStrings);
begin
   Cs.Enter;
   try
      pFuncAddToList(L);
   finally
      Cs.Leave;
   end;
end;

function TGlobalFunc.Add(const Name: string; Addr: Pointer; CountArgs: integer):boolean;
begin
   Cs.Enter;
   try
       Result:= pAdd(Name,Addr,CountArgs);
   finally
      Cs.Leave;
   end;
end;
function TGlobalFunc.Get(const Name: string; out Addr: Pointer;
  out CountArgs: integer): boolean;
begin
   Cs.Enter;
   try
       Result:= pGet(Name,Addr,CountArgs);
   finally
      Cs.Leave;
   end;
end;

function TGlobalFunc.Remove(const Name: string):boolean;
begin
   Cs.Enter;
   try
       Result:= pRemoveName(Name);
   finally
      Cs.Leave;
   end;
end;

function TGlobalFunc.Remove(Addr: Pointer):boolean;
begin
   Cs.Enter;
   try
       Result:= pRemoveAddr(Addr);
   finally
      Cs.Leave;
   end;
end;

function TGlobalFunc.pGetIndex(Index:integer):PItem;
begin
   Result:= PItem(List.List[Index]);
end;
procedure TGlobalFunc.pFreeIndex(Index:integer);
begin
    dispose(PItem(List.List[Index]));
    List.Delete(Index);
end;

procedure TGlobalFunc.pFuncAddToList(L: TStrings);
var
  I: Integer;
begin
    for I := List.Count-1 downto 0 do
    L.Add( Syntax.FuncToString(PItem(List.List[I]).Name,PItem(List.List[I]).Args) );
end;

procedure  TGlobalFunc.pClear;
var
  I: Integer;
begin
    for I := 0 to List.Count-1 do
    dispose(PItem(List.List[I]));
    List.Clear;
end;

function TGlobalFunc.pIndexofName(const Name:string):integer;
begin
    for Result := 0 to List.Count-1 do
    if CmpStr(Name,pGetIndex(Result).Name) then
      exit();
    Result:= -1;
end;
function TGlobalFunc.pIndexofAddr(Addr:Pointer):integer;
begin
    for Result := 0 to List.Count-1 do
    if Addr = pGetIndex(Result).Call then
      exit();
    Result:= -1;
end;
function TGlobalFunc.pGet(const Name:string;out Addr:Pointer; out CountArgs:integer):boolean;
var i:integer;
 Item:PItem;
begin
    Addr:=nil;
    CountArgs:=0;
    Result:=false;
    i:= pIndexofName(Name);
    if i < 0 then
    exit;
    Item:=self.pGetIndex(i);
    CountArgs:=  Item.Args;
    Addr:=       Item.Call;
    Result:=true;
end;
function TGlobalFunc.pAdd(const Name:string;Addr:Pointer;CountArgs:integer):boolean;
var Item:PItem;
begin
    Result:= Assigned(Addr)
    and (CountArgs >= 0)
    and (CountArgs < 20)
    and IsValidName(Name)
    and  (pIndexofName(Name) < 0)
    and (pIndexofAddr(Addr) < 0);
    if not Result then
    exit;
    New(Item);
    Item.Name:=  Name;
    Item.Call:=  Addr;
    Item.Args:=  CountArgs;
    List.Add(Item);

end;
function TGlobalFunc.pRemoveName(const Name:string):boolean;
var i:integer;
begin
  i:=pIndexofName(Name);
  Result:= i>=0;
  if not Result then
    exit;
  pFreeIndex(i);
end;
function TGlobalFunc.pRemoveAddr(Addr:Pointer):boolean;
var i:integer;
begin
  i:=pIndexofAddr(Addr);
  Result:= i>=0;
  if not Result then
    exit;
  pFreeIndex(i);
end;


       { Syntax }

class function Syntax.FuncReg(const FuncName: string;AddrCall:TclSyntaxCallFunc): boolean;
begin
  Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),0);
end;



class function Syntax.FuncReg1(const FuncName: string;
  AddrCall: TclSyntaxCallFunc1): boolean;
begin
   Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),1);
end;

class function Syntax.FuncReg2(const FuncName: string;
  AddrCall: TclSyntaxCallFunc2): boolean;
begin
   Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),2);
end;

class function Syntax.FuncReg3(const FuncName: string;
  AddrCall: TclSyntaxCallFunc3): boolean;
begin
  Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),3);
end;

class function Syntax.FuncReg4(const FuncName: string;
  AddrCall: TclSyntaxCallFunc4): boolean;
begin
   Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),4);
end;

class function Syntax.FuncReg5(const FuncName: string;
  AddrCall: TclSyntaxCallFunc5): boolean;
begin
   Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),5);
end;

class function Syntax.FuncReg6(const FuncName: string;
  AddrCall: TclSyntaxCallFunc6): boolean;
begin
   Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),6);
end;

class function Syntax.FuncReg7(const FuncName: string;
  AddrCall: TclSyntaxCallFunc7): boolean;
begin
   Result:= TGlobalFunc.GlobalList.Add(FuncName,Pointer(@AddrCall),7);
end;

class function Syntax.FuncRegCustom(const FuncName: string;AddrCall:Pointer;CountArg:integer): boolean;
begin
    Result:= TGlobalFunc.GlobalList.Add(FuncName,AddrCall,CountArg);
end;

class function Syntax.FuncUnReg(const FuncName: string): boolean;
begin
  Result:= TGlobalFunc.GlobalList.Remove(FuncName);
end;

class function Syntax.FuncUnReg(AddrCall:Pointer): boolean;
begin
  Result:= TGlobalFunc.GlobalList.Remove(AddrCall);
end;


class function Syntax.FuncToString(const Name:string;CountArgs:integer):string;
var s:string;
    function ArgsToString(CountArgs:integer):string;
    var i:integer;
    begin
      Result:='(';
      try
        if CountArgs  = 1 then
        begin
            Result:= Result +'Value:Double';
            exit;
        end;
        for I := 0 to CountArgs-1 do
        begin
          if I > 0 then
           Result:= Result +', ';
           Result:= Result +'Value'+integer(I+1).ToString;
        end;
        if CountArgs  > 0 then
         Result:= Result +': Double';
      finally
          Result:=Result+')';
      end;
    end;
begin
  s:= ArgsToString(CountArgs);
  Result:= 'function '+Name+s+':Double';
end;

class procedure Syntax.FuncAllToList(L: TStrings);
var i:TEnumFunc;
begin
   L.BeginUpdate;
   try
     L.Clear;
    L.Add('---Встроенные функции---');
    TGlobalFunc.GlobalList.FuncAddToList(L);
    L.Add('---Системные функции---');
    for I := low(sFunc)  to High(sFunc) do
    if (I <>TEnumFunc(0))  then
     L.Add(  FuncToString( sFunc[TEnumFunc(i)] , sFuncArg[TEnumFunc(i)] ));
   finally
     L.EndUpdate;
   end;

end;

class function Syntax.FuncGet(const Name:string;out Addr:Pointer;out CountArgs:integer):boolean;
begin
  Result:= TGlobalFunc.GlobalList.Get(Name,Addr,CountArgs);
end;

class function Syntax.FuncToEnum(const FuncValue: string): TEnumFunc;
var i:TEnumFunc;
begin
    for I := low(sFunc)  to High(sFunc) do
    if (I <>TEnumFunc(0)) and CmpStr(FuncValue,sFunc[i]) then
    exit(TEnumFunc(i));
   Result:= TEnumFunc(0);
end;

class function Syntax.OpToEnum(const OpValue: string) : TEnumOperator;
begin
  if ArrayStringIsValue(OpValue,sopPlus) then
      Result := opPlus
  else if ArrayStringIsValue(OpValue,sopMinus) then
      Result := opMinus
  else if ArrayStringIsValue(OpValue,sopMult) then
      Result := opMult
  else if ArrayStringIsValue(OpValue,sopDiv) then
      Result := opDiv
  else if ArrayStringIsValue(OpValue,sopDivWhole) then
      Result := opDivWhole
  else if ArrayStringIsValue(OpValue,sopMod) then
      Result := opMod
  else if ArrayStringIsValue(OpValue,sopPower) then
      Result := opPower
  else if ArrayStringIsValue(OpValue,sopShl) then
      Result := opShl
  else if ArrayStringIsValue(OpValue,sopShr) then
      Result := opShr
  else if ArrayStringIsValue(OpValue,sopAnd) then
      Result := opAnd
  else if ArrayStringIsValue(OpValue,sopOr) then
      Result := opOr
  else if ArrayStringIsValue(OpValue,sopXor) then
      Result := opXor



  else
    Result := opNone;

end;

class function Syntax.OpEnumToStr(const Op: TEnumOperator): string;
begin
    case Op of
       opPlus: Result:=  sopPlus[0];
       opMinus: Result:=  sopMinus[0];
       opMult: Result:=  sopMult[0];
       opDiv: Result:=  sopDiv[0];
       opDivWhole: Result:=  sopDivWhole[0];
       opPower: Result:=  sopPower[0];
       opMod: Result:=  sopMod[0];
       opShl: Result:=  sopShl[0];
       opShr: Result:=  sopShr[0];
       opAnd: Result:=  sopAnd[0];
       opOr: Result:=  sopOr[0];
       opXor: Result:=  sopXor[0];
    else Result:='opNone';

    end;
end;

class function Syntax.OpsToEnum(const OpsValue: Char): TEnumSeparator;
begin
   if OpsValue = Comma   then
    Result:=opsComma
   else if OpsValue = DotWithComma then
    Result:= opsDotWithComma
   else
     Result:= opsNone;
end;

class procedure Syntax.OpToList(L:TStrings);
var i:integer;
begin
   L.BeginUpdate;
   try
     L.Clear;
    for I := low(sopMatOperators)  to High(sopMatOperators) do
     L.Add(sopMatOperators[i]);
   finally
     L.EndUpdate;
   end;
end;

{
    while c <>0 do
    begin
      ss:= (c and 1).Tostring +ss;
      c:=c shr 1;
    end;
    c:=0;

    for I := 1  to length(ss) do
    begin
     c:= c shl c;
     c:= c or strtoint(ss[i]);
    end;

}


function Test ():double;
begin
  Result:=12345; // тестовая функция для вызова с калькулятора
end;

function TestSum4 (x1,x2,x3,x4:double):double;
begin
  Result:=x1+x2+x3+x4; // тестовая функция для вызова с калькулятора
end;

function TestSum3 (x1,x2,x3:double):double;
begin
  Result:=x1+x2+x3; // тестовая функция для вызова с калькулятора
end;

initialization
begin
    TGlobalFunc.Create;
    Syntax.FuncReg('Test',Test);
    Syntax.FuncReg4('TestSum4',TestSum4);
    Syntax.FuncRegCustom('TestSum3',@TestSum3,3);
end;
finalization
begin
    TGlobalFunc.GlobalListDestroy;
end;

end.

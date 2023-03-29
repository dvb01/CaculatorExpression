unit cl.Calc.Classes;

interface
{$I cl.Calc.Define.inc}
uses
  System.SysUtils,
  System.Classes,
  System.Types,
  cl.Calc.Types,
  cl.Calc.Scanner,
  cl.Calc.Syntax,
  cl.Calc.Option,
  cl.Calc.Utils,
  cl.Calc.Res;

type

  TccBase = class;
  TccCombo = class;
  TccFunctionSys = class;
  TccBaseClass = class of TccBase;
  TcgVariableTabl =class;
  TccExpression = class;
  TccSymbol =class;
  TcgVariable =class;
  TccSymbolAssing=class;
  TccFunctionAddr = class;

  // TclListChildrens список детей подвыражений
  // source (12+((3-2)*2))
  // index 0 =  12
  // index 1 =  +
  // index 2 =  (3-2)*2
      // index 0 = 3-2
          // index 0 = 3
          // index 1 = -
          // index 2 = 2
      // index 1 = *
      // index 2 = 2
  TclListChildrens = class(TclObject)
  strict private
  var
    [weak]FOwner:TccExpression;
    FList: TList;
    function CountGet: integer;
    function ItemsGet(index: integer): TccBase;
    function ItemsTryGet(Index: integer): TccBase;
  protected
    function RemoveIntertal(Child:TccBase):boolean;
    procedure DeleteIntertal(Index:Integer);
    procedure ClearIntertal;
  public
    property Items[index: integer]: TccBase read ItemsGet; default;
    property ItemsTry[Index:integer]: TccBase read ItemsTryGet;
    property Count: integer read CountGet;
    function Add(Value: TccBase): integer;
    procedure Insert(index: integer; Value: TccBase);
    function Extract(index: integer):TccBase;
    constructor Create(AOwner:TccExpression);
    destructor Destroy; override;
  end;

  // абстрактная сущность
  // любая возможная строка с выражения
  // все будующие значения с текста выражения должны быть унаследованы от TccBase
  // TccBase условно  разделен на 3 наследника
  // 1. operator
  // 2. separator
  // 3. eval
  TccBase = class abstract(TclObject)
  private
    [weak]FParent: TccExpression;
    FCaret: TclCaret;
    function CaretGet: PclCaret;
    function CaretTextGet(const Scaner: TclScanner): string;
  protected
    procedure SetParent(AParent:TccExpression); virtual;
  public
    property Caret: PclCaret read CaretGet;
    procedure CaretPush(Scaner:TclScanner);
    procedure CaretPop(Scaner:TclScanner);
    procedure CaretUpdate(Scaner:TclScanner); virtual;
    property CaretText[const Scaner:TclScanner]: string read CaretTextGet;
    property Parent: TccExpression read FParent;
    constructor Create(); virtual;
    destructor Destroy; override;
  end;

  // оператор  +-*/ или любой другой с доспупного синтаксиса Syntax.sopMatOperators
  // запечатанный класс чтобы случайно не унаследоватся
  TccOperator = class sealed (TccBase)
  private
    FValue: Syntax.TEnumOperator;
  protected
  public
    property Value: Syntax.TEnumOperator read FValue write FValue;
    constructor Create(); override;
    destructor Destroy; override;
  end;

  // любой разделитель актуальные это , ;
  // не факт что этим классом буду пользоватся
  TccSeparatorCustom = class abstract(TccBase)
  end;

  TccSeparator = class (TccSeparatorCustom)
  private
    FValue: Syntax.TEnumSeparator;
  protected
  public
    property Value: Syntax.TEnumSeparator read FValue write FValue;
    constructor Create(); override;
    destructor Destroy; override;
  end;

  // абстрактное выражение или число или комбинация операторов или комбинация чисел или комбинация скобок
  // над этой сушностью можно производить операции
  TccEval = class abstract(TccBase)
  end;

  // только число
  TccNumber = class(TccEval)
  private
    FValue: double;
  protected
    procedure SetMinus;
    procedure SetValue(AValue: double);
  public
    property Value: double read FValue;
    constructor Create(); override;
    destructor Destroy; override;
  end;



  // посчитанный промежуточный результат
  TccNumberOuter = class sealed(TccNumber);


  // признаки что нужно наследоватся от  TccExpression
  // 1. должен хранить лист детей
  // 2. должна быть поддержка смены контекса для под выражений
  TccExpression = class abstract (TccEval)
  strict private
    FChilds: TclListChildrens;
  protected
    procedure RemoveFromChilds(Child:TccBase);
  public
    property Childs: TclListChildrens read FChilds;
    function AddChild(AClassCreate:TccBaseClass):TccBase;
    function AddOperator(Op:Syntax.TEnumOperator):TccOperator;
    function AddNumber(Value:Double):TccNumber;
    function AddNumberOuter(Index:integer;Value:Double):TccNumberOuter;
    function AddFunctionSys(NameFunc:Syntax.TEnumFunc):TccFunctionSys;
    function AddFunctionAddr(const NameFunc:string;CountArgs:integer;Call:Pointer):TccFunctionAddr;

    function AddCombo:TccCombo;
    function AddSymbol(const Reffer:TcgVariable):TccSymbol;
    function AddSymbolAssing(const Reffer:TcgVariable):TccSymbolAssing;
    function AddSeparator(const Ops:Syntax.TEnumSeparator):TccSeparator;

    constructor Create(); override;
    destructor Destroy; override;
  end;


  // синтаксис func(1,2,3) 3 агрумента

  // Childs.Items[i mod 2] as TccCombo
  // Childs.Items[i mod 1] as TccSeparator
  // TccCombo Count = кол-ву агрументов
  // если функция забронирована системом (TClass = TccFunctionSys)
  //то кол-во агрументов =  Syntax.sFuncArg[xEmunFunc]
  TccFunction = class abstract (TccExpression)
    private
    protected
    public
     constructor Create(); override;
     destructor Destroy; override;
  end;

  TccFunctionSys = class sealed (TccFunction)
    private
     FNameFunc:Syntax.TEnumFunc;
    protected
    public
     property NameFunc: Syntax.TEnumFunc read FNameFunc write FNameFunc;
     constructor Create(); override;
     destructor Destroy; override;
  end;

  TccFunctionAddr = class sealed (TccFunction)
    private
     FCountArgs:integer;
     FCall:Pointer;
     FNameFunc:string;
    protected
    public
     property CountArgs: integer read FCountArgs write FCountArgs;
     property Call: Pointer read FCall write FCall;
     property NameFunc: string read FNameFunc write FNameFunc;
     constructor Create(); override;
     destructor Destroy; override;
  end;



  {
  TccFunctionUser = class sealed (TccFunction)

  end;}


  // комбинационное выражение например  (12+((3-2)*2))
  TccCombo = class(TccExpression)
  private
  protected
  public
    constructor Create(); override;
    destructor Destroy; override;
  end;

  // символьная перемнная в выражении My:=10; My2:= (3+ My +5);   My2 это TccSymbolAssing  My это  TccSymbol
  // в выражение происходит получения значения с переменной
  TccSymbol = class(TccEval)
  private
    [weak]FVariable:TcgVariable;// ссылка на зареганную переменную
  protected
  public
    property Variable: TcgVariable read FVariable write FVariable;
    constructor Create(); override;
    destructor Destroy; override;
  end;

  // в выражение происходит присвоение значения в переменную
  TccSymbolAssing =  class (TccExpression)
  private
    [weak]FVariable:TcgVariable;// ссылка на зареганную переменную
  protected
  public
    property Variable: TcgVariable read FVariable write FVariable;
    constructor Create(); override;
    destructor Destroy; override;
  end;

  // память хранит имя и значение переменной
  // на эту память ссылается или TccSymbolAssing или TccSymbol
  TcgVariable = class (TclObject)
    private
      FValue:double;
      FName:string;
      FListRef:TList;
    protected
    public
      property Name: string read FName write FName;
      property Value: double read FValue write FValue;
      procedure SymbolAddRef(Value:TccSymbol); overload;
      procedure SymbolRelease(Value:TccSymbol); overload;
      procedure SymbolAddRef(Value:TccSymbolAssing); overload;
      procedure SymbolRelease(Value:TccSymbolAssing);overload;
      constructor Create;
      destructor Destroy; override;
  end;



  // таблица символов
  // сделаю по простому, без хещей, т.к переменных не много будет
  TcgVariableTabl = class (TclObject)
    private
      FList:TList;
     public
      function Get(const Name:string):TcgVariable;
      function AddVariable(const Name:string):TcgVariable;
      procedure Add(Value:TcgVariable);
      procedure Delete(Value:TcgVariable);
      function Count:integer;
      function Item(Index:integer): TcgVariable;
      constructor Create;
      destructor Destroy; override;
  end;

  // директор который подается на вход пасинга
  TccComboRoot = class (TccCombo)
    private
     FVariableTabl: TcgVariableTabl;
    protected
    public
     property VariableTabl: TcgVariableTabl read FVariableTabl;
     constructor Create(); override;
     destructor Destroy; override;
  end;



  TclClassesFactory  = class
    class function New(Context:TccExpression; AClassCreate:TccBaseClass):TccBase;
  end;
implementation

{ TclListChildrens.TclList }



{ TclListChildrens }

constructor TclListChildrens.Create(AOwner:TccExpression);
begin
  inherited Create;
  FOwner:=AOwner;
  FList := TList.Create;
end;

procedure TclListChildrens.ClearIntertal;
var
  I: integer;
begin
  for I := Count - 1 downto 0 do
    TObject(FList.List[i]).Free;
end;

destructor TclListChildrens.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TclListChildrens.Extract(index: integer):TccBase;
begin
  if Index<0 then
     Index:= Count+Index;
  Result:= Items[index];
  DeleteIntertal(Index);
end;

function TclListChildrens.Add(Value: TccBase): integer;
begin
  if Value.Parent = FOwner then
   begin
      Value.SetParent(nil);
      RemoveIntertal(Value);
   end;
  Result := FList.Add(Value);
   Value.SetParent(FOwner);
end;

procedure TclListChildrens.Insert(index: integer; Value: TccBase);
begin
  if Value.Parent = FOwner then
   begin
      Value.SetParent(nil);
      RemoveIntertal(Value);
   end;
  FList.Insert(Index, Value);
  Value.SetParent(FOwner);
end;

function TclListChildrens.CountGet: integer;
begin
  Result := FList.Count;
end;

function TclListChildrens.ItemsGet(index: integer): TccBase;
begin
  if Index<0 then
     Index:= Count+Index;

  Result := TccBase(FList.List[Index]);
end;

function TclListChildrens.ItemsTryGet(Index: integer): TccBase;
begin
  if Index<0 then
     Index:= Count+Index;
  if (Index<Count) and (Index>=0)  then
    Result:= Items[Index]
  else
   Result:=nil;
end;

function TclListChildrens.RemoveIntertal(Child: TccBase): boolean;
var i:integer;
begin
    if Child<>nil then
    begin
      Result:= FList.Last = Child;
      if Result  then
        DeleteIntertal(Count-1)
        else
        begin
          i:=FList.IndexOf(Child) ;
          Result:= I>=0;
          if Result then
             DeleteIntertal(I);
        end;

    end
    else Result:=false;
end;

procedure TclListChildrens.DeleteIntertal(Index:Integer);
var B:TccBase;
begin
   B:=Items[index];
   FList.Delete(index);
   B.SetParent(nil);    
end;

{ TccExpression }

constructor TccExpression.Create();
begin
  inherited Create();
  FChilds := TclListChildrens.Create(self);
end;

destructor TccExpression.Destroy;
begin
   FChilds.ClearIntertal;
   FreeAndNil(FChilds);
  inherited;
end;

procedure TccExpression.RemoveFromChilds(Child:TccBase);
begin
    Childs.RemoveIntertal(Child);
end;



function TccExpression.AddChild(AClassCreate:TccBaseClass):TccBase;
begin
    Result:= TclClassesFactory.New(self,AClassCreate);
    Childs.Add(Result);
end;

function TccExpression.AddOperator(Op:Syntax.TEnumOperator):TccOperator;
begin
  Result:=  AddChild(TccOperator) as TccOperator;
  Result.Value:= Op;
end;

function TccExpression.AddNumber(Value:Double):TccNumber;
begin
  Result:=  AddChild(TccNumber) as TccNumber;
  Result.SetValue(Value);
end;

function TccExpression.AddNumberOuter(Index:integer;Value:Double):TccNumberOuter;
begin
  Result:= TclClassesFactory.New(self,TccNumberOuter) as TccNumberOuter;
  Childs.Insert(Index,Result);
  Result.SetValue(Value);
end;

function TccExpression.AddFunctionSys(NameFunc:Syntax.TEnumFunc):TccFunctionSys;
begin
  Result:=  AddChild(TccFunctionSys) as TccFunctionSys;
  Result.NameFunc:=NameFunc;
end;

function TccExpression.AddFunctionAddr(const NameFunc:string;CountArgs:integer;Call:Pointer):TccFunctionAddr;
begin
  Result:=  AddChild(TccFunctionAddr) as TccFunctionAddr;
  Result.NameFunc:=NameFunc;
  Result.Call:=  Call;
  Result.CountArgs:= CountArgs;
end;

function TccExpression.AddCombo:TccCombo;
begin
  Result:=  AddChild(TccCombo) as TccCombo;
end;


function TccExpression.AddSymbol(const Reffer:TcgVariable):TccSymbol;
begin
   Result:=  AddChild(TccSymbol) as TccSymbol;
   if Reffer <> nil then     
   Reffer.SymbolAddRef(Result);
end;

function TccExpression.AddSymbolAssing(const Reffer:TcgVariable):TccSymbolAssing;
begin
   Result:=  AddChild(TccSymbolAssing) as TccSymbolAssing;
   if Reffer <> nil then
   Reffer.SymbolAddRef(Result);
end;

function TccExpression.AddSeparator(const Ops:Syntax.TEnumSeparator):TccSeparator;
begin
   Result:=  AddChild(TccSeparator) as TccSeparator;
   Result.Value:= Ops;
end;


    { TccCombo }

constructor TccCombo.Create();
begin
  inherited  Create();
end;

destructor TccCombo.Destroy;
begin
  inherited;
end;



{ TccFunction }

constructor TccFunction.Create();
begin
  inherited;
end;

destructor TccFunction.Destroy;
begin
  inherited;
end;


{ TccFunctionSys }

constructor TccFunctionSys.Create();
begin
  inherited;
   FNameFunc:=fnNone;
end;

destructor TccFunctionSys.Destroy;
begin
  FNameFunc:=fnNone;
  inherited;
end;

{ TccFunctionAddr }

constructor TccFunctionAddr.Create();
begin
  inherited;
  FCountArgs:=0;
  FCall:=nil;
  FNameFunc:='';
end;

destructor TccFunctionAddr.Destroy;
begin
  FCountArgs:=0;
  FCall:=nil;
  inherited;
end;




{ TccNumber }

constructor TccNumber.Create();
begin
  inherited;
  FValue := 0;
end;

destructor TccNumber.Destroy;
begin
  FValue := 0;
  inherited;
end;

procedure TccNumber.SetMinus;
begin
   FValue:= - FValue;
end;

procedure TccNumber.SetValue(AValue: double);
begin
  FValue := AValue;
end;



{ TccBase }

constructor TccBase.Create();
begin
  inherited Create;
  FParent:=nil;
  FCaret.Clear;
end;

destructor TccBase.Destroy;
begin
  if FParent<>nil then
  FParent.RemoveFromChilds(self);
  FCaret.Clear;
  inherited;
end;

procedure TccBase.SetParent(AParent: TccExpression);
begin
    FParent:= AParent;
end;

function TccBase.CaretTextGet(const Scaner: TclScanner): string;
begin
   Scaner.Push;
   try
     Scaner.Caret^:=FCaret;
     Result:= Scaner.CurText;
   finally
    Scaner.Pop;
   end;
end;

function TccBase.CaretGet: PclCaret;
begin
  Result:= @FCaret;
end;

procedure TccBase.CaretPush(Scaner:TclScanner);
begin
   Scaner.Push;
   Scaner.Caret^:= FCaret;
end;

procedure TccBase.CaretPop(Scaner:TclScanner);
begin
    Scaner.Pop;
end;

procedure TccBase.CaretUpdate(Scaner:TclScanner);
begin
    FCaret:= Scaner.Caret^;
end;



{ TccEval }


{ TccOperator }

constructor TccOperator.Create();
begin
  inherited;
  FValue := opNone;
end;

destructor TccOperator.Destroy;
begin
  inherited;
end;



{ TclClassesFactory }

class function TclClassesFactory.New(Context: TccExpression; AClassCreate: TccBaseClass): TccBase;
begin
   Result:=  AClassCreate.Create();
end;





{ TcgVariableTabl }


function TcgVariableTabl.Count: integer;
begin
   Result:= FList.Count;
end;

constructor TcgVariableTabl.Create;
begin
   inherited;
   FList:=  TList.Create;
end;

destructor TcgVariableTabl.Destroy;
var
  I: Integer;
begin
  for I := FList.Count-1 downto 0 do
   TcgVariable(FList.List[i]).Free;
   FreeAndNil(FList);
  inherited;
end;

function TcgVariableTabl.AddVariable(const Name:string):TcgVariable;
begin
   Result:=  TcgVariable.Create;
   Result.Name:= Name;
   Add(Result);
end;

procedure TcgVariableTabl.Add(Value: TcgVariable);
begin
    FList.Add(Value);
end;

procedure TcgVariableTabl.Delete(Value: TcgVariable);
begin
  FList.Remove(Value);
end;

function TcgVariableTabl.Get(const Name: string): TcgVariable;
var i:integer;
begin
   if length(Name) <=0 then
    exit(nil);
   for I := 0 to FList.Count-1 do
   if CmpStr(Name,TcgVariable(FList.List[i]).FName) then
   exit(TcgVariable(FList.List[i]));
    Result:=nil;
end;

function TcgVariableTabl.Item(Index: integer): TcgVariable;
begin
   Result:= TcgVariable(FList[Index]);
end;

{ TcgVariable }

constructor TcgVariable.Create;
begin
  inherited;
  FValue:=0;
  FName:='';
  FListRef:=TList.Create;
end;

destructor TcgVariable.Destroy;
var
  I: Integer;
begin
  for I := 0 to FListRef.Count-1 do
  if TObject(FListRef.List[i]) is TccSymbolAssing then
  TccSymbolAssing(FListRef.List[i]).Variable:=nil
  else if TObject(FListRef.List[i]) is TccSymbol then
   TccSymbol(FListRef.List[i]).Variable:=nil;
  FreeAndNil(FListRef);
  inherited;
end;


procedure TcgVariable.SymbolAddRef(Value: TccSymbol);
begin
  if Value.Variable <> self then
  begin
    Value.Variable:=self;
    FListRef.Add(Value);
  end;
end;

procedure TcgVariable.SymbolRelease(Value: TccSymbol);
begin
  if Value.Variable = self then
  begin
    FListRef.Remove(Value);
    Value.Variable:=nil;
  end;
end;

procedure TcgVariable.SymbolAddRef(Value:TccSymbolAssing);
begin
  if Value.Variable <> self then
  begin
    Value.Variable:=self;
    FListRef.Add(Value);
  end;
end;

procedure TcgVariable.SymbolRelease(Value:TccSymbolAssing);
begin
  if Value.Variable = self then
  begin
    FListRef.Remove(Value);
    Value.Variable:=nil;
  end;
end;

{ TccComboRoot }

constructor TccComboRoot.Create();
begin
  inherited Create();
  FVariableTabl:= TcgVariableTabl.Create;
end;

destructor TccComboRoot.Destroy;
begin
  FreeAndNil(FVariableTabl);
  inherited;
end;

{ TccSymbol }

constructor TccSymbol.Create();
begin
  inherited;
    FVariable:=nil;
end;

destructor TccSymbol.Destroy;
begin
   if FVariable<>nil then
   FVariable.SymbolRelease(self);
  inherited;
end;

{ TccSymbolAssing }

constructor TccSymbolAssing.Create();
begin
  inherited;
   FVariable:=nil;
end;

destructor TccSymbolAssing.Destroy;
begin
     if FVariable<>nil then
   FVariable.SymbolRelease(self);
  inherited;
end;



{ TccSeparator }

constructor TccSeparator.Create();
begin
  inherited;
  FValue:= opsNone;
end;

destructor TccSeparator.Destroy;
begin
  FValue:= opsNone;
  inherited;
end;



end.

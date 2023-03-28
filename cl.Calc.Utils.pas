unit cl.Calc.Utils;

interface
{$I cl.Calc.Define.inc}
uses
  System.SysUtils,
  System.Classes,
  System.Character,
  cl.Calc.Types;




  function ArrayStringIndex(const Value:string;const A:array of string):integer;
  function ArrayStringIsValue(const Value:string;const A:array of string):boolean;
  function CmpText(const S1, S2: string): Integer;
  function CmpStr(const S1, S2: string): boolean;
  function IsNumber(C: Char): Boolean;
  function IsLiteral(C: Char): Boolean;
  function IsLiteralEx(c: Char): Boolean;
  function IsInChars(c:Char;AChars: TSetOfChar):boolean;
  function IsInByte(c:Char;ABytes: TByteOfSet):boolean;
  function IsHexNumber(c: Char): Boolean;
  function IsValidName(const S: string): Boolean;
implementation

function IsInByte(c:Char;ABytes: TByteOfSet):boolean;
begin
    Result:= Ord(c) in ABytes;
end;

function IsHexNumber(c: Char): Boolean;
begin
  result := IsInByte(c, [Ord('0')..Ord('9'),Ord('A')..Ord('F'),Ord('a')..Ord('f')]);
end;

function  IsInChars(c:Char;AChars: TSetOfChar):boolean;
begin
    Result:= CharInSet(c, AChars);
end;

function IsNumber(C: Char): Boolean;
begin
  result := (C >= '0') and (C <='9');
end;


function IsLiteralEx(c: Char): Boolean;
begin
  if IsInChars(c, ['e','E']) then
    result := false
  else
    result := IsLiteral(c);
end;

function IsLiteral(C: Char): Boolean;
begin
  result := ((C >= 'a') and (C <='z')) or
            ((C >= 'A') and (C <='Z')) or
             (C = '_');
end;

function IsValidName(const S: string): Boolean;
var
  I: Integer;
begin
  result := false;
  if S = '' then
    exit;
  if not IsLiteral(S[1]) then
    exit;
  for I := Low(S) to Low(S) do
    if not (IsLiteral(S[I]) or IsNumber(S[I])) then
      exit;
  result := true;
end;


function ArrayStringIsValue(const Value:string;const A:array of string):boolean;
begin
    Result:= ArrayStringIndex(Value,A) >=0;
end;

function ArrayStringIndex(const Value:string;const A:array of string):integer;
begin
    for Result := Low(A) to High(A) do
    if CmpStr(Value, A[Result])  then
      exit;
    Result:=-1;
end;

function CmpText(const S1, S2: string): Integer;
begin
   Result:=CompareText(S1, S2);
end;

function CmpStr(const S1, S2: string): boolean;
begin
   Result:= CmpText(S1, S2) = 0;
end;

end.

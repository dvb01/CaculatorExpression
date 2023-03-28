unit cl.Calc.Types;

interface

{$I cl.Calc.Define.inc}

uses
  System.SysUtils,
  System.Classes,
  cl.Calc.Res;

type

  TSetOfChar = TSysCharSet;
  TCharArray = array of Char;
  TByteOfSet = set of Byte;

  TclEventLog = procedure(Sender: TObject; Msg: string; E: Exception = nil) of object;

  TListEnumeratorHelper = class helper for TListEnumerator
  strict private
    function CountGet: integer;
  public
    procedure Reset;
    property Count: integer read CountGet;
  end;

  RecHlp = record
  public
    class procedure Clear<T>(var Instance: T); static;
  end;

  // базовый объект
  TclObject = class

  end;

  // логирование
  TclObjectLog = class(TclObject)
  private
    FOnLog: TclEventLog;
  protected
    procedure Log(Msg: string; E: Exception = nil); virtual;
    procedure LogProc(Sender: TObject; Msg: string; E: Exception = nil);
  public
    property OnLog: TclEventLog read FOnLog write FOnLog;
  end;

implementation

{ TListEnumeratorHelper }

function TListEnumeratorHelper.CountGet: integer;
begin
  with self do
    Result := FList.Count;
end;

procedure TListEnumeratorHelper.Reset;
begin
  with self do
    FIndex := -1;

end;

{ TclObject }

{ TclObjectErrorLog }

procedure TclObjectLog.Log(Msg: string; E: Exception);
begin
  if Assigned(FOnLog) then
    FOnLog(self, Msg, E);
end;

procedure TclObjectLog.LogProc(Sender: TObject; Msg: string; E: Exception);
begin
  Log(Msg, E);
end;

class procedure RecHlp.Clear<T>(var Instance: T);
begin
  System.Finalize(Instance);
  System.FillChar(Instance, sizeof(Instance), 0);
end;

end.

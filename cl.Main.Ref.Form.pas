unit cl.Main.Ref.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,cl.Calc.Syntax, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TFormCaclRef = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label4: TLabel;
    ListOperators: TListBox;
    Label1: TLabel;
    ListFunc: TListBox;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
     procedure LoadListFunc;
     procedure LoadListOperators;
  public
    { Public declarations }
  end;

var
  FormCaclRef: TFormCaclRef;

implementation

{$R *.dfm}

{ TForm1 }

procedure TFormCaclRef.FormShow(Sender: TObject);
begin
    LoadListFunc;
    LoadListOperators;
end;

procedure TFormCaclRef.LoadListFunc;
begin
    Syntax.FuncAllToList(ListFunc.Items);
end;

procedure TFormCaclRef.LoadListOperators;
begin
  Syntax.OpToList(ListOperators.Items);
end;

end.

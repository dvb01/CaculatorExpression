unit cl.Main.Setting.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormCalcSetting = class(TForm)
    PanelInput: TPanel;
    P_CanShowError: TCheckBox;
    Label2: TLabel;
    P_CanColorReturn: TCheckBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCalcSetting: TFormCalcSetting;

implementation

{$R *.dfm}

procedure TFormCalcSetting.FormCreate(Sender: TObject);
begin
     P_CanShowError.Checked:=false;
     P_CanColorReturn.Checked:=true;
end;

end.

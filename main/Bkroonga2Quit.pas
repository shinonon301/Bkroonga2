unit Bkroonga2Quit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TBkroonga2QuitForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    LabelSec: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
    FormResult: TModalResult;
  end;

var
  Bkroonga2QuitForm: TBkroonga2QuitForm;

implementation

{$R *.dfm}

procedure TBkroonga2QuitForm.Button1Click(Sender: TObject);
begin
  FormResult := Button1.ModalResult;
  Close;
end;

procedure TBkroonga2QuitForm.Button2Click(Sender: TObject);
begin
  FormResult := Button2.ModalResult;
  Close;
end;

procedure TBkroonga2QuitForm.FormCreate(Sender: TObject);
begin
  FormResult := mrCancel;
end;

end.

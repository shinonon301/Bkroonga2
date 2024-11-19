object Bkroonga2QuitForm: TBkroonga2QuitForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Bkroonga2'
  ClientHeight = 118
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 28
    Top = 16
    Width = 202
    Height = 15
    Caption = 'Groonga'#12398#32066#20102#12395#26178#38291#12364#12363#12363#12387#12390#12356#12414#12377
  end
  object LabelSec: TLabel
    Left = 120
    Top = 37
    Width = 19
    Height = 15
    Caption = '0'#31186
  end
  object Button1: TButton
    Left = 8
    Top = 71
    Width = 113
    Height = 33
    Caption = #24375#21046#32066#20102'(&A)'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 137
    Top = 71
    Width = 113
    Height = 33
    Cancel = True
    Caption = #32066#20102#12414#12391#24453#12388'(&W)'
    Default = True
    ModalResult = 2
    TabOrder = 1
    OnClick = Button2Click
  end
end

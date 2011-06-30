object VerDocumentoForm: TVerDocumentoForm
  Left = 503
  Top = 264
  Width = 405
  Height = 354
  Caption = 'Ver documento encontrado'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    397
    327)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 73
    Width = 79
    Height = 13
    Caption = 'Buscar palabras:'
    Transparent = True
  end
  object Label2: TLabel
    Left = 9
    Top = 197
    Width = 85
    Height = 13
    Caption = 'Buscar sin'#243'nimos:'
    Transparent = True
  end
  object SpeedButton1: TSpeedButton
    Left = 86
    Top = 176
    Width = 59
    Height = 18
    Caption = 'Siguiente'
    Flat = True
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 86
    Top = 301
    Width = 59
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Siguiente'
    Flat = True
    OnClick = SpeedButton2Click
  end
  object Bevel1: TBevel
    Left = 152
    Top = 24
    Width = 9
    Height = 297
    Anchors = [akLeft, akTop, akBottom]
    Shape = bsLeftLine
  end
  object Label3: TLabel
    Left = 161
    Top = 8
    Width = 76
    Height = 13
    Caption = 'Texto completo:'
    Transparent = True
  end
  object Label4: TLabel
    Left = 9
    Top = 8
    Width = 120
    Height = 13
    Caption = 'Buscar cualquier palabra:'
    Transparent = True
  end
  object SpeedButton3: TSpeedButton
    Left = 86
    Top = 48
    Width = 59
    Height = 18
    Caption = 'Siguiente'
    Flat = True
    OnClick = SpeedButton3Click
  end
  object lv_palabras: TListBox
    Left = 9
    Top = 91
    Width = 136
    Height = 81
    ItemHeight = 13
    TabOrder = 0
    OnClick = lv_palabrasClick
  end
  object lv_sinonimos: TListBox
    Left = 9
    Top = 216
    Width = 136
    Height = 81
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = lv_palabrasClick
  end
  object Memo1: TMemo
    Left = 162
    Top = 24
    Width = 230
    Height = 298
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WantReturns = False
  end
  object Edit1: TEdit
    Left = 9
    Top = 24
    Width = 136
    Height = 21
    TabOrder = 3
    OnKeyDown = Edit1KeyDown
  end
end

object MainForm: TMainForm
  Left = 396
  Top = 270
  Width = 567
  Height = 434
  Caption = 'Indexador de Toopo'
  Color = 15922418
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object p_main: TPanel
    Left = 0
    Top = 0
    Width = 559
    Height = 386
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      559
      386)
    object p_central: TPanel
      Left = -2
      Top = -2
      Width = 299
      Height = 389
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        299
        389)
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 109
        Height = 13
        Caption = 'Archivos para indexar:'
      end
      object Bevel1: TBevel
        Left = 120
        Top = 7
        Width = 177
        Height = 10
        Anchors = [akLeft, akTop, akRight]
        Shape = bsBottomLine
      end
      object lista: TListView
        Left = 8
        Top = 28
        Width = 289
        Height = 329
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Nombre'
            Width = 100
          end
          item
            Caption = 'Carpeta'
            Width = 150
          end>
        MultiSelect = True
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object b_aniadir: TButton
        Left = 170
        Top = 363
        Width = 60
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = 'A'#241'adir'
        TabOrder = 1
        OnClick = b_aniadirClick
      end
      object b_eliminar: TButton
        Left = 238
        Top = 363
        Width = 60
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = 'Eliminar'
        TabOrder = 2
        OnClick = b_eliminarClick
      end
      object Button1: TButton
        Left = 8
        Top = 364
        Width = 21
        Height = 19
        Anchors = [akLeft, akBottom]
        Caption = '?'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
    object Panel1: TPanel
      Left = 299
      Top = -2
      Width = 258
      Height = 129
      Anchors = [akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        258
        129)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 60
        Height = 13
        Caption = 'Estad'#237'sticas:'
      end
      object Bevel2: TBevel
        Left = 75
        Top = 7
        Width = 177
        Height = 10
        Anchors = [akLeft, akTop, akRight]
        Shape = bsBottomLine
      end
      object lb_estadisticas: TListBox
        Left = 6
        Top = 28
        Width = 249
        Height = 101
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object Panel2: TPanel
      Left = 299
      Top = 134
      Width = 256
      Height = 250
      Anchors = [akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        256
        250)
      object Label4: TLabel
        Left = 8
        Top = 101
        Width = 172
        Height = 13
        Caption = 'Archivo de diccionario de sin'#243'nimos:'
      end
      object Label3: TLabel
        Left = 8
        Top = 57
        Width = 132
        Height = 13
        Caption = 'Archivo de palabras vac'#237'as:'
      end
      object Label5: TLabel
        Left = 8
        Top = 144
        Width = 81
        Height = 13
        Caption = 'Carpeta destino:'
      end
      object Label6: TLabel
        Left = 8
        Top = 193
        Width = 134
        Height = 13
        Caption = 'Longitud m'#237'nima de palabra:'
      end
      object Label7: TLabel
        Left = 8
        Top = 11
        Width = 146
        Height = 13
        Caption = 'Nombre de la biblioteca digital:'
      end
      object Bevel3: TBevel
        Left = 8
        Top = 0
        Width = 250
        Height = 9
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object pb: TProgressBar
        Left = 8
        Top = 231
        Width = 248
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        Min = 0
        Max = 100
        Smooth = True
        TabOrder = 0
        Visible = False
      end
      object b_indexar: TButton
        Left = 84
        Top = 224
        Width = 97
        Height = 22
        Caption = 'Iniciar indexaci'#243'n'
        TabOrder = 1
        OnClick = b_indexarClick
      end
      object Panel3: TPanel
        Left = 8
        Top = 117
        Width = 248
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        BorderWidth = 1
        BorderStyle = bsSingle
        Caption = 'Panel3'
        Color = clScrollBar
        TabOrder = 2
        DesignSize = (
          244
          17)
        object SpeedButton1: TSpeedButton
          Left = 229
          Top = 0
          Width = 19
          Height = 17
          Anchors = [akTop, akRight]
          Caption = '...'
          OnClick = SpeedButton1Click
        end
        object e_diccionario: TEdit
          Left = 0
          Top = 0
          Width = 228
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          TabOrder = 0
        end
      end
      object Panel4: TPanel
        Left = 8
        Top = 73
        Width = 248
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        BorderWidth = 1
        BorderStyle = bsSingle
        Caption = 'Panel3'
        Color = clScrollBar
        TabOrder = 3
        DesignSize = (
          244
          17)
        object SpeedButton2: TSpeedButton
          Left = 230
          Top = 0
          Width = 18
          Height = 17
          Anchors = [akTop, akRight]
          Caption = '...'
          OnClick = SpeedButton2Click
        end
        object e_vacias: TEdit
          Left = 0
          Top = 0
          Width = 229
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          TabOrder = 0
        end
      end
      object Panel5: TPanel
        Left = 8
        Top = 160
        Width = 248
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        BorderWidth = 1
        BorderStyle = bsSingle
        Caption = 'Panel3'
        Color = clScrollBar
        TabOrder = 4
        DesignSize = (
          244
          17)
        object SpeedButton3: TSpeedButton
          Left = 229
          Top = 0
          Width = 19
          Height = 17
          Anchors = [akTop, akRight]
          Caption = '...'
          OnClick = SpeedButton3Click
        end
        object e_carpeta: TEdit
          Left = 0
          Top = 0
          Width = 228
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          TabOrder = 0
        end
      end
      object e_lenPalabra: TEdit
        Left = 215
        Top = 190
        Width = 41
        Height = 19
        Anchors = [akTop, akRight]
        AutoSize = False
        TabOrder = 5
        Text = '3'
      end
      object e_nombre: TEdit
        Left = 8
        Top = 27
        Width = 248
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        Text = 'Mi biblioteca digital'
      end
    end
  end
  object sb: TStatusBar
    Left = 0
    Top = 386
    Width = 559
    Height = 21
    Panels = <>
    SimplePanel = True
  end
  object od: TOpenDialog
    DefaultExt = '*.txt'
    Filter = 'Archivos de texto (*.txt)|*.txt|Todos los archivos (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'A'#241'adir archivos a la indexaci'#243'n'
    Left = 232
    Top = 264
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.txt'
    Filter = 'Archivos de texto (*.txt)|*.txt|Todos los archivos (*.*)|*.*'
    Left = 464
    Top = 56
  end
end

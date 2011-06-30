object MainForm: TMainForm
  Left = 347
  Top = 256
  Width = 449
  Height = 254
  Caption = 'Buscador Toopo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    441
    227)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 441
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      441
      229)
    object Label2: TLabel
      Left = 8
      Top = 94
      Width = 93
      Height = 13
      Caption = 'Palabras buscadas:'
    end
    object lv_resultados: TListBox
      Left = 6
      Top = 130
      Width = 428
      Height = 61
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBtnFace
      ItemHeight = 13
      TabOrder = 2
      OnClick = lv_resultadosClick
      OnDblClick = b_abrirClick
    end
    object b_buscar: TButton
      Left = 372
      Top = 61
      Width = 62
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Buscar'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = b_buscarClick
    end
    object b_abrir: TButton
      Left = 281
      Top = 199
      Width = 153
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = 'Ver contenido del documento'
      Enabled = False
      TabOrder = 4
      OnClick = b_abrirClick
    end
    object e_buscado: TEdit
      Left = 107
      Top = 90
      Width = 259
      Height = 21
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      ParentColor = True
      ReadOnly = True
      TabOrder = 6
    end
    object e_consulta: TComboBox
      Left = 8
      Top = 60
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'ni'#241'o'
        'ni'#241'a'
        'migas'
        'zapato de cristal'
        'el lobo y la abuela: abuelita, que ojos tan grandes tienes')
    end
    object b_cargar: TButton
      Left = 34
      Top = 199
      Width = 144
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = 'Abrir otra biblioteca digital'
      TabOrder = 5
      OnClick = b_cargarClick
    end
    object Button1: TButton
      Left = 6
      Top = 199
      Width = 22
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = '?'
      TabOrder = 3
      OnClick = Button1Click
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 441
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 7
      object Bevel1: TBevel
        Left = 0
        Top = 43
        Width = 441
        Height = 2
        Align = alBottom
        Shape = bsTopLine
      end
      object Image1: TImage
        Left = 8
        Top = 7
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          07544269746D617076020000424D760200000000000076000000280000002000
          000020000000010004000000000000020000C40E0000C40E0000100000000000
          0000000000000000800000800000008080008000000080008000808000008080
          8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
          FF00DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
          DDDDD000DDDDDDDDDDDDDDDDDDDDDDDD000D007000DDDDDDDDDDDDDDDDDDDD00
          070070707000000000000000000000070707808080F7700EE0878700EE070708
          080808880D0F70EE087878700EE070D08880D000DDD0F0EE007777070EE00DDD
          000DDDDDDDDD0000000000000000DDDDDDDDDD7DDDDDDDD00F7F87800DDDDDDD
          DDDDDDD77DDDD707F7800878707DDDDDD7DDDDDDD77D0F8F780000878700DD77
          7DDDDDDDDDD7F87870000008787007DDDDDD777DDD0F878707777770877700DD
          777DDDD77777F0807878878708070077DDDDDDDDDD0F7F0787811878707800DD
          DDDD777777F0F8F8F81F91F8F8870F777777DDDDDD77777F8F99998F887777DD
          DDDDDDD000F0F8F70008800078800F0007DDDD0888007F80F400004F08000088
          807DD7887F8807F0F440044F087088F78807D08707787F70FFF00FFF07877770
          780008807777F7808FF00FF8087877770780080777777F770008800077877777
          708008707777F7F088877888087877770780080707087F7F0008800087878770
          708000807008F0F7F7F8F878780F800708077F00008F0D0F7F7F878780D0F800
          000DD7F008FF7DD0F7F778780DD7FF800F7DDD0FFFF7DDDD0F8F87870DDD7FFF
          F0DDDDD7007DDDDDD0087870DDDDD0007DDDDDDDDDDDDDDDDDD000DDDDDDDDDD
          DDDD}
        Transparent = True
      end
      object Label3: TLabel
        Left = 49
        Top = 6
        Width = 388
        Height = 28
        AutoSize = False
        Caption = 
          'Desde esta ventana podr'#225's buscar cualquier conjunto de palabras ' +
          'a trav'#233's del buscador Toopo. Tan solo tienes que escribir las pa' +
          'labras y hacer clic en "Buscar"'
        WordWrap = True
      end
    end
  end
end

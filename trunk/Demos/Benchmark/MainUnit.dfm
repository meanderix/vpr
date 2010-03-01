object Form1: TForm1
  Left = 616
  Top = 132
  Caption = 'GR32 / AggPas / GDI+ benchmark tool'
  ClientHeight = 743
  ClientWidth = 791
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMinimized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 496
    Width = 791
    Height = 247
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 152
      Top = 75
      Width = 61
      Height = 11
      Caption = 'Random Seed:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnBenchmark: TButton
      Left = 151
      Top = 8
      Width = 186
      Height = 25
      Caption = 'Benchmark'
      TabOrder = 0
      OnClick = btnBenchmarkClick
    end
    object rgRasterizer: TRadioGroup
      Left = 8
      Top = 112
      Width = 329
      Height = 129
      Caption = 'Polygon Rasterizer'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'VPR'
        'VPR (LCD)'
        'AggPas / Agg2D'
        'AGG Lite'
        'LibArt'
        'GDI+ (high speed)'
        'GDI+ (high quality)'
        'Cairo'
        'Direct2D'
        'GR32_Polygons (am16times)'
        'GR32_Polygons (am8times)'
        'GR32_Polygons (am4times)'
        'GR32_Polygons (am2times)'
        'GR32_Polygons (amNone)')
      TabOrder = 1
      OnClick = rgRasterizerClick
    end
    object rgTest: TRadioGroup
      Left = 8
      Top = 8
      Width = 137
      Height = 97
      Caption = 'Benchmark Test'
      ItemIndex = 0
      Items.Strings = (
        '5k ellipses'
        '20k thick lines'
        '20k thin lines'
        '5k splines'
        '2k text output')
      TabOrder = 2
    end
    object GroupBox1: TGroupBox
      Left = 344
      Top = 8
      Width = 433
      Height = 233
      Caption = 'Benchmark Results'
      TabOrder = 3
      object Memo1: TMemo
        Left = 8
        Top = 16
        Width = 417
        Height = 209
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object cbStroke: TCheckBox
      Left = 151
      Top = 38
      Width = 97
      Height = 17
      Caption = 'Stroke outlines'
      TabOrder = 4
    end
    object cbOpaque: TCheckBox
      Left = 151
      Top = 55
      Width = 97
      Height = 17
      Caption = 'Opaque'
      TabOrder = 5
    end
    object cbAllTests: TCheckBox
      Left = 248
      Top = 38
      Width = 89
      Height = 17
      Caption = 'All tests'
      TabOrder = 6
    end
    object cbAllRasterizers: TCheckBox
      Left = 248
      Top = 55
      Width = 89
      Height = 17
      Caption = 'All rasterizers'
      TabOrder = 7
    end
    object gbSeed: TGaugeBar
      Left = 152
      Top = 88
      Width = 153
      Height = 16
      Backgnd = bgPattern
      HandleSize = 20
      Max = 999
      ShowArrows = False
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = gbSeedChange
    end
    object pnlSeed: TPanel
      Left = 312
      Top = 88
      Width = 25
      Height = 17
      Alignment = taRightJustify
      BevelInner = bvLowered
      BevelOuter = bvNone
      Caption = '0'
      TabOrder = 9
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 791
    Height = 496
    Align = alClient
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 1
    object Img: TImage32
      Left = 2
      Top = 2
      Width = 787
      Height = 492
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCenter
      Color = clWhite
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnPaintStage = ImgPaintStage
    end
  end
end

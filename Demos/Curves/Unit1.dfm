object Form3: TForm3
  Left = 833
  Top = 165
  Caption = 'Curves Example'
  ClientHeight = 560
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TImage32
    Left = 8
    Top = 8
    Width = 512
    Height = 512
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Color = 3355443
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object Button2: TButton
    Left = 8
    Top = 528
    Width = 75
    Height = 25
    Caption = 'Draw Curve'
    TabOrder = 1
    OnClick = Button2Click
  end
end

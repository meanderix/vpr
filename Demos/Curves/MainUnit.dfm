object MainForm: TMainForm
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
  DesignSize = (
    527
    560)
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
  object btnDrawCurve: TButton
    Left = 8
    Top = 528
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Draw Curve'
    TabOrder = 1
    OnClick = btnDrawCurveClick
  end
  object cbxUpdate: TCheckBox
    Left = 473
    Top = 532
    Width = 47
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Timer'
    TabOrder = 2
    OnClick = cbxUpdateClick
  end
  object tmrUpdate: TTimer
    Enabled = False
    Interval = 30
    OnTimer = btnDrawCurveClick
    Left = 376
    Top = 496
  end
end

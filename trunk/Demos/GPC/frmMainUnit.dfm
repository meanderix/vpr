object frmMain: TfrmMain
  Left = 530
  Top = 126
  Caption = 'GPC Test'
  ClientHeight = 392
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    497
    392)
  PixelsPerInch = 96
  TextHeight = 13
  object rgClipOperation: TRadioGroup
    Left = 8
    Top = 8
    Width = 129
    Height = 121
    Caption = 'Operation'
    ItemIndex = 0
    Items.Strings = (
      'Difference'
      'Intersection'
      'Exclusive'
      'Union')
    TabOrder = 0
    OnClick = rgClipOperationClick
  end
  object rgOutputType: TRadioGroup
    Left = 8
    Top = 135
    Width = 129
    Height = 66
    Caption = 'Output Type'
    ItemIndex = 0
    Items.Strings = (
      'Polygon'
      'Triangle strips')
    TabOrder = 1
    OnClick = rgOutputTypeClick
  end
  object Image32: TImage32
    Left = 143
    Top = 8
    Width = 346
    Height = 376
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Color = clWhite
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    TabOrder = 2
    OnPaintStage = Image32PaintStage
    OnResize = Image32Resize
  end
end

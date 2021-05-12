object Form30: TForm30
  Left = 0
  Top = 0
  Caption = 'Form30'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 35
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 32
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Simple Init'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 32
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Group Init'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 304
    Top = 32
    Width = 201
    Height = 209
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button3: TButton
    Left = 32
    Top = 126
    Width = 75
    Height = 25
    Caption = 'Thread Init'
    TabOrder = 3
    OnClick = Button3Click
  end
end

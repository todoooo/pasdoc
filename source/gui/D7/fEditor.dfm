object EditBox: TEditBox
  Left = 19
  Top = 135
  Width = 1007
  Height = 507
  Caption = 'Description Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object gbEdit: TGroupBox
    Left = 0
    Top = 0
    Width = 999
    Height = 480
    Align = alClient
    Caption = 'gbEdit'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 473
      Top = 15
      Height = 463
    end
    object edSrc: TMemo
      Left = 2
      Top = 15
      Width = 471
      Height = 463
      Align = alLeft
      HideSelection = False
      Lines.Strings = (
        'edSrc')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Panel1: TPanel
      Left = 476
      Top = 15
      Width = 521
      Height = 463
      Align = alClient
      TabOrder = 1
      object Panel2: TPanel
        Left = 1
        Top = 1
        Width = 519
        Height = 108
        Align = alTop
        TabOrder = 0
        DesignSize = (
          519
          108)
        object buInit: TButton
          Left = 356
          Top = 4
          Width = 45
          Height = 25
          Caption = 'Init'
          TabOrder = 0
        end
        object buSave: TButton
          Left = 456
          Top = 4
          Width = 57
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Save'
          TabOrder = 1
          OnClick = buSaveClick
        end
        object cbItem: TComboBox
          Left = 8
          Top = 8
          Width = 313
          Height = 21
          ItemHeight = 13
          TabOrder = 2
          Text = 'cbItem'
          OnChange = cbItemChange
        end
        object edDesc: TMemo
          Left = 8
          Top = 36
          Width = 505
          Height = 65
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'edDesc')
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 3
        end
      end
      object edRem: TMemo
        Left = 1
        Top = 109
        Width = 519
        Height = 353
        Align = alClient
        HideSelection = False
        Lines.Strings = (
          'edRem')
        TabOrder = 1
        OnChange = edRemChange
      end
    end
  end
end

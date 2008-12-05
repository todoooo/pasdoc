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
      OnDblClick = edSrcDblClick
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
        object buSave: TButton
          Left = 456
          Top = 4
          Width = 57
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Save'
          TabOrder = 0
          OnClick = buSaveClick
        end
        object cbItem: TComboBox
          Left = 132
          Top = 8
          Width = 293
          Height = 21
          Hint = 'Select item to view its description'
          ItemHeight = 13
          TabOrder = 1
          Text = 'cbItem'
          OnChange = cbItemChange
          OnSelect = cbItemSelect
        end
        object edDesc: TMemo
          Left = 8
          Top = 44
          Width = 505
          Height = 57
          Hint = 'Description in source code'
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'edDesc')
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 2
        end
        object buPrev: TButton
          Left = 8
          Top = 4
          Width = 21
          Height = 17
          Caption = #233
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Wingdings'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = buPrevClick
        end
        object buNext: TButton
          Left = 8
          Top = 20
          Width = 21
          Height = 17
          Caption = #234
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Wingdings'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = buNextClick
        end
        object buPrevParent: TButton
          Left = 66
          Top = 4
          Width = 21
          Height = 17
          Caption = #245
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Wingdings'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = buPrevParentClick
        end
      end
      object edRem: TMemo
        Left = 1
        Top = 109
        Width = 519
        Height = 353
        Hint = 'Enter or edit external description here'
        Align = alClient
        HideSelection = False
        Lines.Strings = (
          'edRem')
        TabOrder = 1
        OnChange = edRemChange
      end
    end
  end
  object buPrevEmpty: TButton
    Left = 512
    Top = 20
    Width = 21
    Height = 17
    Caption = #241
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = buPrevEmptyClick
  end
  object buNextEmpty: TButton
    Left = 512
    Top = 36
    Width = 21
    Height = 17
    Caption = #242
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = buNextEmptyClick
  end
  object buNextParent: TButton
    Left = 544
    Top = 36
    Width = 21
    Height = 17
    Caption = #247
    Font.Charset = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Wingdings'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = buNextParentClick
  end
end

object FormCalc: TFormCalc
  Left = 0
  Top = 0
  ClientHeight = 628
  ClientWidth = 798
  Color = 4271150
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelInput: TPanel
    Left = 0
    Top = 0
    Width = 798
    Height = 471
    Align = alClient
    BevelOuter = bvNone
    Color = 4271150
    ParentBackground = False
    TabOrder = 0
    OnAlignPosition = PanelInputAlignPosition
    ExplicitLeft = -5
    ExplicitTop = 2
    object PanelExpression: TPanel
      Left = 75
      Top = 29
      Width = 641
      Height = 407
      Align = alCustom
      BevelOuter = bvNone
      TabOrder = 0
      object MemoExpression: TRichEdit
        AlignWithMargins = True
        Left = 3
        Top = 42
        Width = 635
        Height = 323
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = 10485760
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'd;s;'
          '    s:= cos(PI/3);'
          '    d:= 2^(5+2);    '
          
            '    a:=-  00-- ((-1.22- - + - 25.09)* - + Rand(20) * ++0.40/ ---' +
            '(2 +'
          '    { -3 - (-12+  [   [2*2-4]*2    ] / 2 )} / (7+- 44.32)));'
          ''
          '    b:=max(12^2 - a,cos(pi div 3));'
          '    p:=Rand(100);'
          '    i :=Rand(100000 +- 44.32);'
          '    b:= i xor Rand(100000) + I xor P mod 128;'
          '    Result:= (i shr 8) xor (p +b) mod 128;'
          
            '    Result + s + min( 10 , 5.5) * ((sin(Pi/6)+ -+ 42) * -d-i - -' +
            ' (3 shr 1) ) ')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        Zoom = 100
        ExplicitTop = 45
      end
      object PanelExpressionTop: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 635
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        Color = clBlack
        TabOrder = 1
        ExplicitTop = 26
        ExplicitWidth = 648
        object PanelScannerRun: TPanel
          AlignWithMargins = True
          Left = 68
          Top = 5
          Width = 34
          Height = 28
          Hint = #1040#1085#1080#1084#1072#1094#1080#1103' '#1089#1082#1072#1085#1080#1088#1086#1074#1072#1085#1080#1103
          Margins.Left = 0
          Margins.Top = 5
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alLeft
          BevelOuter = bvNone
          Caption = '8'
          Color = 4271150
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWhite
          Font.Height = -19
          Font.Name = 'Webdings'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = PanelScannerRunClick
          OnMouseEnter = PanelExecuteMouseEnter
          OnMouseLeave = PanelExecuteMouseLeave
          ExplicitLeft = 98
          ExplicitTop = 0
        end
        object PanelRef: TPanel
          AlignWithMargins = True
          Left = 34
          Top = 5
          Width = 34
          Height = 28
          Margins.Left = 0
          Margins.Top = 5
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alLeft
          BevelOuter = bvNone
          Caption = '?'
          Color = 4271150
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -19
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          OnClick = PanelRefClick
          OnMouseEnter = PanelExecuteMouseEnter
          OnMouseLeave = PanelExecuteMouseLeave
          ExplicitTop = 8
        end
        object PanelSettingOpen: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 5
          Width = 34
          Height = 28
          Margins.Left = 0
          Margins.Top = 5
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alLeft
          BevelOuter = bvNone
          Caption = '@'
          Color = 4271150
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWhite
          Font.Height = -19
          Font.Name = 'Webdings'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          OnClick = PanelSettingOpenClick
          OnMouseEnter = PanelExecuteMouseEnter
          OnMouseLeave = PanelExecuteMouseLeave
          ExplicitLeft = 8
          ExplicitTop = 8
        end
      end
      object PanelExpressionBottom: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 371
        Width = 635
        Height = 33
        Align = alBottom
        BevelOuter = bvNone
        Color = 6628393
        ParentBackground = False
        TabOrder = 2
        ExplicitTop = 387
        ExplicitWidth = 648
        object PanelExecute: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 635
          Height = 33
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          BevelOuter = bvNone
          Caption = '4'
          Color = 7109928
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWhite
          Font.Height = -21
          Font.Name = 'Webdings'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          OnClick = PanelExecuteClick
          OnMouseEnter = PanelExecuteMouseEnter
          OnMouseLeave = PanelExecuteMouseLeave
          ExplicitTop = -3
        end
      end
    end
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 476
    Width = 788
    Height = 147
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    Color = 5980985
    ParentBackground = False
    TabOrder = 1
    ExplicitLeft = -19
    ExplicitTop = 540
    object Label1: TLabel
      Left = 24
      Top = 16
      Width = 55
      Height = 19
      Caption = #1054#1090#1074#1077#1090' :'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object MemoResult: TRichEdit
      AlignWithMargins = True
      Left = 20
      Top = 40
      Width = 748
      Height = 87
      Margins.Left = 20
      Margins.Top = 40
      Margins.Right = 20
      Margins.Bottom = 20
      Align = alClient
      Alignment = taCenter
      BorderStyle = bsNone
      Color = 4271150
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'RichEdit1')
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Zoom = 100
      ExplicitLeft = 24
      ExplicitTop = 38
    end
  end
  object TimerScanner: TTimer
    Enabled = False
    Interval = 150
    OnTimer = TimerScannerTimer
    Left = 608
    Top = 168
  end
end

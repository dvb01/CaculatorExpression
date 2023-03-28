object FormCalcSetting: TFormCalcSetting
  Left = 0
  Top = 0
  ClientHeight = 85
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelInput: TPanel
    Left = 0
    Top = 0
    Width = 419
    Height = 85
    Align = alClient
    BevelOuter = bvNone
    Color = 6628393
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 627
    ExplicitHeight = 145
    object Label2: TLabel
      Left = 44
      Top = 21
      Width = 149
      Height = 16
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1086#1082#1085#1086' '#1086#1096#1080#1073#1082#1080
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 44
      Top = 44
      Width = 305
      Height = 16
      Caption = #1054#1082#1088#1072#1096#1080#1074#1072#1090#1100' '#1088#1072#1079#1088#1103#1076#1099' '#1095#1080#1089#1083#1072' '#1086#1090#1074#1077#1090#1072' '#1074' '#1088#1072#1079#1085#1099#1077' '#1094#1074#1077#1090#1072
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object P_CanShowError: TCheckBox
      Left = 21
      Top = 21
      Width = 17
      Height = 17
      TabOrder = 0
    end
    object P_CanColorReturn: TCheckBox
      Left = 21
      Top = 44
      Width = 17
      Height = 17
      TabOrder = 1
    end
  end
end

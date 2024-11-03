unit dlMeasSettingsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Spin;

type

  { TMeasSettingsForm }

  TMeasSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CbCondition2: TComboBox;
    CbCondition2LimitUnits: TComboBox;
    CbIntervalUnits: TComboBox;
    CbCondition3: TComboBox;
    CbCondition3LimitUnits: TComboBox;
    CbTransformation: TComboBox;
    EdCondition2Limit: TFloatSpinEdit;
    EdCondition3Limit: TFloatSpinEdit;
    EdInterval1: TFloatSpinEdit;
    EdInterval2: TFloatSpinEdit;
    EdInterval3: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    InfoCondition1: TLabel;
    CbUseCondition2: TCheckBox;
    CbUseCondition1: TCheckBox;
    CbUseCondition3: TCheckBox;
    LblInterval2Unit: TLabel;
    LblInterval3Unit: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure CbConditionSelect(Sender: TObject);
    procedure CbIntervalUnitsSelect(Sender: TObject);
    procedure CbUseConditionChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FActivated: Boolean;
    procedure ControlsToSettings;
    procedure ReadTransformations;
    procedure SettingsToControls;
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
  public
    { public declarations }
  end;

var
  MeasSettingsForm: TMeasSettingsForm;


implementation

{$R *.lfm}

uses
  dlGlobal, dlUtils, dlTransformation;

procedure TMeasSettingsForm.ButtonOKClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
begin
  if ValidData(msg, C) then
    ControlsToSettings
  else begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOk], 0);
    ModalResult := mrNone;
  end;
end;


procedure TMeasSettingsForm.CbConditionSelect(Sender: TObject);
begin
  CbCondition2LimitUnits.Visible := CbUseCondition2.Checked and (CbCondition2.ItemIndex = 0);
  CbCondition3LimitUnits.Visible := CbUseCondition3.Checked and (CbCondition3.ItemIndex = 0);
end;


procedure TMeasSettingsForm.CbIntervalUnitsSelect(Sender: TObject);
begin
  LblInterval2Unit.Caption := CbIntervalUnits.Items[CbIntervalUnits.ItemIndex];
  LblInterval3Unit.Caption := CbIntervalUnits.Items[CbIntervalUnits.ItemIndex];
end;


procedure TMeasSettingsForm.CbUseConditionChange(Sender: TObject);
begin
  EdInterval1.Visible := CbUseCondition1.Checked;

  EdInterval2.Visible := CbUseCondition2.Checked;
  CbCondition2.Visible := CbUseCondition2.Checked;
  EdCondition2Limit.Visible := CbUseCondition2.Checked;
  CbCondition2LimitUnits.Visible := CbUseCondition2.Checked;

  EdInterval3.Visible := CbUseCondition3.Checked;
  CbCondition3.Visible := CbUseCondition3.Checked;
  EdCondition3Limit.Visible := CbUseCondition3.Checked;
  CbCondition3LimitUnits.Visible := CbUseCondition3.Checked;

  if CbUseCondition1.Checked then begin
    CbIntervalUnits.Top := EdInterval1.Top;
    LblInterval2Unit.Visible := CbUseCondition2.Checked;
    LblInterval3Unit.Visible := CbUseCondition3.Checked;
  end else
  if CbUseCondition2.Checked then begin
    CbIntervalUnits.Top := EdInterval2.Top;
    LblInterval2Unit.Visible := false;
    LblInterval3Unit.Visible := CbUseCondition3.Checked;;
  end else
  if CbUseCondition3.Checked then begin
    CbIntervalUnits.Top := EdInterval3.Top;
    LblInterval2Unit.Visible := false;
    LblInterval3Unit.Visible := false;
  end;
end;

procedure TMeasSettingsForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    Constraints.MinHeight := GroupBox2.Top + GroupBox2.Height + GroupBox2.BorderSpacing.Around +
      ButtonPanel1.Height + ButtonPanel1.BorderSpacing.Around * 2;
    Constraints.MaxHeight := Constraints.MinHeight;
    Constraints.MinWidth := GroupBox1.Left + GroupBox1.Width + GroupBox1.BorderSpacing.Around -
     (CbCondition2.Width - InfoCondition1.Width);
    FActivated := true;
  end;
end;


procedure TMeasSettingsForm.FormCreate(Sender: TObject);
begin
  SettingsToControls;
end;


procedure TMeasSettingsForm.ControlsToSettings;
begin
  with MeasSettings do begin
    TimeUnits := TTimeUnits(CbIntervalUnits.ItemIndex);
    with IntervalSettings[0] do begin
      Active := CbUseCondition1.Checked;
      Interval := EdInterval1.Value;
    end;
    with IntervalSettings[1] do begin
      Active := CbUseCondition2.Checked;
      Interval := EdInterval2.Value;
      Condition := TMeasIntervalCondition(CbCondition2.ItemIndex+1);
      ConditionLimit := EdCondition2Limit.Value;
      ConditionLimitUnits := TTimeUnits(CbCondition2LimitUnits.ItemIndex);
    end;
    with IntervalSettings[2] do begin
      Active := CbUseCondition3.Checked;
      Interval := EdInterval3.Value;
      Condition := TMeasIntervalCondition(CbCondition3.ItemIndex+1);
      ConditionLimit := EdCondition3Limit.Value;
      ConditionLimitUnits := TTimeUnits(CbCondition3LimitUnits.ItemIndex);
    end;
    if CbTransformation.ItemIndex < 1 then
      Transformation := ''
    else
      Transformation := CbTransformation.Items[CbTransformation.ItemIndex];
  end;
end;


procedure TMeasSettingsForm.ReadTransformations;
var
  i: Integer;
begin
  CbTransformation.Items.BeginUpdate;
  try
    CbTransformation.Items.Clear;
    CbTransformation.Items.Add('(none)');
    for i := 0 to TransformationList.Count-1 do
      CbTransformation.Items.Add(TransformationList[i].TransformationName);
    CbTransformation.ItemIndex := TransformationList.IndexOf(MeasSettings.Transformation) + 1;
  finally
    CbTransformation.Items.EndUpdate;
  end;
end;


procedure TMeasSettingsForm.SettingsToControls;
begin
  CbIntervalUnits.ItemIndex := ord(MeasSettings.TimeUnits);
  CbIntervalUnitsSelect(nil);

  with MeasSettings do begin
    with IntervalSettings[0] do begin
      CbUseCondition1.Checked := Active;
      EdInterval1.Value := Interval;
    end;
    with IntervalSettings[1] do begin
      CbUseCondition2.Checked := Active;
      EdInterval2.Value := Interval;
      CbCondition2.ItemIndex := ord(Condition) - 1;
      EdCondition2Limit.Value := ConditionLimit;
      CbCondition2LimitUnits.ItemIndex := ord(ConditionLimitUnits);
    end;
    with IntervalSettings[2] do begin
      CbUseCondition3.Checked := Active;
      EdInterval3.Value := Interval;
      CbCondition3.ItemIndex := ord(Condition) - 1;
      EdCondition3Limit.Value := ConditionLimit;
      CbCondition3LimitUnits.ItemIndex := ord(ConditionLimitUnits);
    end;
  end;
  CbUseConditionChange(nil);
  CbConditionSelect(nil);

  ReadTransformations;
end;


function TMeasSettingsForm.ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
begin
  Result := false;
  if (not CbUseCondition1.Checked) and (not CbUseCondition2.Checked) and (not CbUseCondition3.Checked) then
  begin
    AMsg := 'At least one condition must be checked.';
    AControl := CbUseCondition1;
    exit;
  end;

  Result := true;
end;

end.


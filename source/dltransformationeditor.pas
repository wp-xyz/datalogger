unit dlTransformationEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, dlTransformation;

type
  TEditMode = (emNew, emEdit);

  { TTransformationEditor }

  TTransformationEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    CbLogarithmic: TCheckBox;
    EdExpression: TEdit;
    EdMaxIn: TEdit;
    EdMaxOut: TEdit;
    EdMinIn: TEdit;
    EdMinOut: TEdit;
    EdName: TEdit;
    EdTransformationName: TEdit;
    EdUnits: TEdit;
    GbDiagram: TGroupBox;
    GbExpression: TGroupBox;
    GbInputRange: TGroupBox;
    GbInputRange1: TGroupBox;
    GbTransfQuant: TGroupBox;
    GroupBox1: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FActivated: Boolean;
    FEditMode: TEditMode;
    procedure SetEditMode(AValue: TEditMode);
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
  public
    { public declarations }
    function GetTransformation: TTransformation;
    procedure SetTransformation(ATransformation: TTransformation);
    property EditMode: TEditMode read FEditMode write SetEditMode;
  end;

var
  TransformationEditor: TTransformationEditor;

  
implementation

{$R *.lfm}

uses
  Math,
  dlGlobal, dlUtils;


{ TTransformationEditor }

function TTransformationEditor.GetTransformation: TTransformation;
var
  mx, mn: Double;
begin
  Result := TTransformation.Create;

  Result.TransformationName := EdTransformationName.Text;
  Result.Expression := EdExpression.Text;
  Result.MeasName := EdName.Text;
  Result.MeasUnits := EdUnits.Text;
  Result.Logarithmic := CbLogarithmic.Checked;
  
  if EdMinIn.Text = '' then mn := NaN else mn := StrToFloat(edMinIn.Text);
  if EdMaxIn.Text = '' then mx := NaN else mx := StrToFloat(edMaxIn.Text);
  PutInOrder(mn, mx);
  Result.MinIn := mn;
  Result.MaxIn := mx;
  
  if EdMinOut.Text = '' then mn := NaN else mn := StrToFloat(edMinOut.Text);
  if EdMaxOut.Text = '' then mx := NaN else mx := StrToFloat(edMaxOut.Text);
  PutInOrder(mn, mx);
  Result.MinOut := mn;
  Result.MaxOut := mx;
end;


procedure TTransformationEditor.OKButtonClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
begin
  if not ValidData(msg, C) then
    ModalResult := mrNone;
end;

procedure TTransformationEditor.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinHeight := GbDiagram.Top + GbDiagram.Height +
      GbDiagram.BorderSpacing.Around + GbDiagram.BorderSpacing.Bottom +
      ButtonPanel1.Height;
  end;
end;


procedure TTransformationEditor.SetEditMode(AValue: TEditMode);
begin
  FEditMode := AValue;
  case FEditMode of
    emNew  : Caption := 'New transformation';
    emEdit : Caption := 'Edit transformation';
  end;
end;


procedure TTransformationEditor.SetTransformation(ATransformation: TTransformation);
begin
  if ATransformation = nil then begin
    EdTransformationName.Clear;
    EdExpression.Clear;
    EdName.Clear;
    EdUnits.Clear;
    EdMinIn.Clear;
    EdMaxIn.Clear;
    EdMinOut.Clear;
    EdMaxOut.Clear;
    CbLogarithmic.Checked := false;
  end else begin
    EdTransformationName.Text := ATransformation.TransformationName;
    EdExpression.Text := ATransformation.Expression;
    EdName.Text := ATransformation.MeasName;
    EdUnits.Text := ATransformation.MeasUnits;
    if IsEmptyNumber(ATransformation.MinIn)
      then EdMinIn.Clear
      else EdMinIn.Text := Format('%g', [ATransformation.MinIn]);
    if IsEmptyNumber(ATransformation.MaxIn)
      then EdMaxIn.Clear
      else EdMaxIn.Text := Format('%g', [ATransformation.MaxIn]);
    if IsEmptyNumber(ATransformation.MinOut)
      then EdMinOut.Clear
      else EdMinOut.Text := Format('%g', [ATransformation.MinOut]);
    if IsEmptyNumber(ATransformation.MaxOut)
      then EdMaxOut.Clear
      else EdMaxOut.Text := Format('%g', [ATransformation.MaxOut]);
    CbLogarithmic.Checked := ATransformation.Logarithmic;
  end;
end;


function TTransformationEditor.ValidData(out AMsg: String;
  out AControl: TWinControl): Boolean;
var
  x: Double;
begin
  Result := false;
  if EdTransformationName.Text = '' then begin
    AControl := EdTransformationName;
    AMsg := 'This field must not be empty';
    exit;
  end;
  if EdExpression.Text = '' then begin
    AControl := EdExpression;
    AMsg := 'This field must not be empty.';
    exit;
  end;
  if EdName.Text = '' then begin
    AControl := EdName;
    AMsg := 'This field must not be empty.';
    exit;
  end;
  if EdUnits.Text = '' then begin
    AControl := EdUnits;
    AMsg := 'This field must not be empty.';
    exit;
  end;
  if (EdMinIn.Text <> '') and not TryStrToFloat(EdMinIn.Text, x) then begin
    AControl := EdMinIn;
    AMsg := 'Input must be a number.';
    exit;
  end;
  if (EdMaxIn.Text <> '') and not TryStrToFloat(EdMaxIn.Text, x) then begin
    AControl := EdMaxIn;
    AMsg := 'Input must be a number.';
    exit;
  end;
  if (EdMinOut.Text <> '') and not TryStrToFloat(EdMinOut.Text, x) then begin
    AControl := EdMinOut;
    AMsg := 'Input must be a number.';
    exit;
  end;
  if (EdMaxOut.Text <> '') and not TryStrToFloat(EdMaxOut.Text, x) then begin
    AControl := EdMaxOut;
    AMsg := 'Input must be a number.';
    exit;
  end;
  Result := true;
end;


end.


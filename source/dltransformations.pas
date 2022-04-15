unit dlTransformations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  Buttons, ComCtrls, ExtCtrls; 

type

  { TTransformationForm }

  TTransformationForm = class(TForm)
    Bevel1: TBevel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnAdd: TBitBtn;
    BtnEdit: TBitBtn;
    BtnDelete: TBitBtn;
    Panel1: TPanel;
    TransformationsListView: TListView;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TransformationsListViewSelectItem(Sender: TObject; {%H-}Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    function  FindTransformationIndex(AName: String): Integer;
    procedure LoadTransformations;
    procedure SaveTransformations;
    procedure UpdateCmdStates;
  public
    { public declarations }
  end;

var
  TransformationForm: TTransformationForm;

implementation

{$R *.lfm}

uses
  IniFiles,
  dlGlobal, dlUtils, dlTransformation, dlTransformationEditor;


{ TTransformationForm }

procedure TTransformationForm.BtnAddClick(Sender: TObject);
var
  item: TListItem;
  F: TTransformationEditor;
  T: TTransformation;
begin
  F := TTransformationEditor.Create(nil);
  try
    F.EditMode := emNew;
    F.SetTransformation(nil);
    if F.ShowModal = mrOK then begin;
      T := F.GetTransformation;
      TransformationList.Add(T);
      item := TransformationsListView.Items.Add;
      with item do begin
        Data := T;
        Caption := T.TransformationName;
        SubItems.Add(T.Expression);
        SubItems.Add(T.MeasName);
        SubItems.Add(T.MeasUnits);
      end;
      TransformationsListView.Selected := item;
      UpdateCmdStates;
    end;
  finally
    F.Free;
  end;
end;


procedure TTransformationForm.BtnDeleteClick(Sender : TObject);
var
  res : integer;
  item : TListItem;
  idx : integer;
  nextIdx : integer;
begin
  item := TransformationsListView.Selected;
  if item = nil then
    exit;

  res := MessageDlg('Do you really want to delete this transformation?',
    mtConfirmation, [mbYes, mbNo], 0);
  if res = mrYes then begin
    idx := item.Index;
    // Delete transformation from global transformation list
    TransformationList.Delete(idx);
    // Delete item from listview
    TransformationsListView.Items.Delete(idx);
    // Select next item
    if idx >= TransformationsListView.Items.Count-1 then
      nextIdx := TransformationsListView.Items.Count - 1
    else
      nextIdx := idx;
    TransformationsListView.Selected := TransformationsListView.Items[nextIdx];
  end;

  UpdateCmdStates;
end;


procedure TTransformationForm.BtnEditClick(Sender: TObject);
var
  F: TTransformationEditor;
begin
  F := TTransformationEditor.Create(nil);
  try
    F.EditMode := emEdit;
    F.SetTransformation(TTransformation(TransformationsListView.Selected.Data));
    if F.ShowModal = mrOK then begin
      with TransformationsListView.Selected do begin
        TTransformation(Data).Free;
        Data := F.GetTransformation;
        Caption := TTransformation(Data).TransformationName;
        SubItems[0] := TTransformation(Data).Expression;
        SubItems[1] := TTransformation(Data).MeasName;
        SubItems[2] := TTransformation(Data).MeasUnits;
      end;
      UpdateCmdStates;
    end;
  finally
    F.Free;
  end;
end;


procedure TTransformationForm.BtnOKClick(Sender: TObject);
begin
  SaveTransformations;
end;


function TTransformationForm.FindTransformationIndex(AName: String): Integer;
var
  T: TTransformation;
begin
  for Result := 0 to TransformationsListView.Items.Count-1 do begin
    T := TTransformation(TransformationsListView.Items[Result].Data);
    if SameText(T.TransformationName, AName) then
      exit;
  end;
  Result := -1;
end;


procedure TTransformationForm.FormCreate(Sender: TObject);
begin
  LoadTransformations;
  TransformationsListview.ItemIndex := FindTransformationIndex(MeasSettings.Transformation);
  ActiveControl := TransformationsListView;
  UpdateCmdStates;
end;


procedure TTransformationForm.TransformationsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateCmdStates;
end;


procedure TTransformationForm.LoadTransformations;
var
  i: Integer;
  T: TTransformation;
begin
  TransformationsListView.Items.BeginUpdate;
  try
    TransformationsListView.Items.Clear;
    for i := 0 to TransformationList.Count-1 do
    begin
      T := TransformationList[i];
      with TransformationsListView.Items.Add do begin
        Caption := T.TransformationName;
        SubItems.Add(T.Expression);
        SubItems.Add(T.MeasName);
        SubItems.Add(T.MeasUnits);
        Data := T;
      end;
    end;
  finally
    TransformationsListView.Items.EndUpdate;
  end;
end;


procedure TTransformationForm.SaveTransformations;
var
  ini: TCustomIniFile;
begin
  ini := CreateGlobalIni;
  try
    TransformationList.WriteToIni(ini);
  finally
    ini.Free;
  end;
end;


procedure TTransformationForm.UpdateCmdStates;
begin
  BtnAdd.Enabled := true;
  BtnEdit.Enabled := (TransformationsListView.Selected <> nil)
    and (TransformationsListView.Items.Count > 0);
  BtnDelete.Enabled := BtnEdit.Enabled;
end;

end.


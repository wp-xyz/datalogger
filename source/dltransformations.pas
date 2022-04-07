unit dlTransformations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  Buttons, StdCtrls, ComCtrls, ExtCtrls, fpexprpars;

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
    procedure FormDestroy(Sender: TObject);
    procedure TransformationsListViewSelectItem(Sender: TObject; Item: TListItem;
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
  IniFiles, Math,
  dlGlobal, dlUtils, dlTransformationEditor;


{ TTransformationForm }

procedure TTransformationForm.BtnAddClick(Sender: TObject);
var
  item : TListItem;
  F: TTransformationEditor;
begin
  F := TTransformationEditor.Create(nil);
  try
    F.EditMode := emNew;
    F.SetTransformation(nil);
    if F.ShowModal = mrOK then begin;
      item := TransformationsListView.Items.Add;
      with item do begin
        Data := F.GetTransformation;
        Caption := TTransformation(Data).TransformationName;
        SubItems.Add(TTransformation(Data).Expression);
        SubItems.Add(TTransformation(Data).MeasName);
        SubItems.Add(TTransformation(Data).MeasUnits);
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
  indx : integer;
  nextIndx : integer;
  T: TTransformation;
begin
  item := TransformationsListView.Selected;
  if item = nil then
    exit;

  res := MessageDlg('Do you really want to delete this transformation?',
    mtConfirmation, [mbYes, mbNo], 0);
  if res = mrYes then begin
    indx := item.Index;
    T := TTransformation(item.Data);
    T.Free;
    TransformationsListView.Items.Delete(indx);
    if indx >= TransformationsListView.Items.Count-1 then
      nextIndx := TransformationsListView.Items.Count - 1
    else
      nextIndx := indx;
    TransformationsListView.Selected := TransformationsListView.Items[nextIndx];
  end;

  UpdateCmdStates;
end;


procedure TTransformationForm.BtnEditClick(Sender: TObject);
var
  item: TListItem;
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


procedure TTransformationForm.FormDestroy(Sender: TObject);
var
  i: Integer;
  T: TTransformation;
begin
  for i:=TransformationsListView.Items.Count-1 downto 0 do begin
    T := TTransformation(TransformationsListView.Items[i].Data);
    T.Free;
  end;
end;


procedure TTransformationForm.TransformationsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateCmdStates;
end;


procedure TTransformationForm.LoadTransformations;
var
  ini : TCustomIniFile;
  List : TStringList;
  L : TStringList;
  i : integer;
  T : TTransformation;
  s : string;
  sf : string;
  mx, mn : double;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini := CreateGlobalIni;
  List := TStringList.Create;
  L := TStringList.Create;
  try
    L.Delimiter := ';';
    ini.ReadSection('Transformations', List);
    TransformationsListView.Items.Clear;
    for i:=0 to List.Count-1 do begin
      s := trim(ini.ReadString('Transformations', List[i], ''));
      if (s <> '') and (s[1] = '(') then Delete(s, 1, 1);
      if (s <> '') and (s[Length(s)] = ')') then Delete(s, Length(s), 1);
      if s <> '' then begin
        L.DelimitedText := trim(s);
        T := TTransformation.Create;
        T.TransformationName := List[i];
        T.MeasName := L.Values['MeasName'];
        T.MeasUnits := L.Values['MeasUnits'];
        T.Expression := L.Values['Expression'];
        T.Logarithmic := SameText(L.Values['Logarithmic'], 'true');
        sf := L.Values['MaxIn'];
        if sf <> '' then mx := StrToFloat(sf, fs) else mx := nan;
        sf := L.Values['MinIn'];
        if sf <> '' then mn := StrToFloat(sf, fs) else mn := nan;
        PutInOrder(mn, mx);
        T.MaxIn := mx;
        T.MinIn := mn;
        sf := L.Values['MaxOut'];
        if sf <> '' then mx := StrToFloat(sf, fs) else mx := nan;
        sf := L.Values['MinOut'];
        if sf <> '' then mn := StrToFloat(sf, fs) else mn := nan;
        PutInOrder(mn, mx);
        T.MaxOut := mx;
        T.MinOut := mn;
        with TransformationsListView.Items.Add do begin
          Caption := T.TransformationName;
          SubItems.Add(T.Expression);
          SubItems.Add(T.MeasName);
          SubItems.Add(T.MeasUnits);
          Data := T;
        end;
      end;
    end;
  finally
    L.Free;
    List.Free;
    ini.Free;
  end;
end;


procedure TTransformationForm.SaveTransformations;
var
  ini : TCustomIniFile;
  i : integer;
  T : TTransformation;
  minIn, maxIn : string;
  minOut, maxOut : string;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini := CreateGlobalIni;
  try
    ini.EraseSection('Transformations');
    for i:=0 to TransformationsListView.Items.Count-1 do begin
      T := TTransformation(TransformationsListView.Items[i].Data);
      if T <> nil then begin
        if IsEmptyNumber(T.MinIn) then
          minIn := ''
        else
          minIn := Format('%0.4e', [T.MinIn]);
        if IsEmptyNumber(T.MaxIn) then
          maxIn := ''
        else
          maxIn := Format('%0.4e', [T.MaxIn]);
        if IsEmptyNumber(T.MinOut) then
          minOut:= ''
        else
          minOut := Format('%0.4e', [T.MinOut]);
        if IsEmptyNumber(T.MaxOut) then
          maxOut := ''
        else
          maxOut := Format('%0.4e', [T.MaxOut]);
        ini.WriteString(
          'Transformations',
          T.TransformationName,
          Format(
            '("MeasName=%s"; "MeasUnits=%s"; "Expression=%s"; '+
              '"Logarithmic=%s"; ' +
              'MinIn=%s; MaxIn=%s; MinOut=%s; MaxOut=%s)',
            [T.MeasName, T.MeasUnits, T.Expression,
             BoolToStr(T.Logarithmic, true),
             minIn, maxIn, minOut, maxOut],
            fs
          )
        );
      end;
    end;
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


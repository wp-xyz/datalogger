unit dlSerialPortSettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ButtonPanel, Spin, contnrs, inifiles,
  dlGlobal;

type
  { TSerPresetItem }
  TSerPresetItem = class
    Name : string;
    Baudrate : integer;
    Databits : integer;
    StopBits : double;
    Parity : char;
    Handshake : integer;  // see hXXXX constants
    ReadOut : integer;    // see roXXXX constants - not implemented
    Editable : integer;   // see eXXXX constants
    Digits: Integer;
    procedure Assign(AItem:TSerPresetItem);
  end;

  { TSerPresetList }
  TSerPresetList = class(TFPObjectList)
  private
    function GetItem(AIndex:integer) : TSerPresetItem;
    procedure SetItem(AIndex:integer; Value:TSerPresetItem);
  protected
  public
    procedure AddDefault(const AName:string; const Presets:TDevicePresets);
    function  GetItemByName(const AName:string) : TSerPresetItem;
    procedure ReadFromIni(ini:TCustomIniFile; const Section:string);
    procedure WriteToIni(ini:TCustomIniFile; const Section:string);
    property Items[AIndex:integer] : TSerPresetItem read Getitem write SetItem; default;
  end;

  { TSerPortForm }
  TSerPortForm = class(TForm)
    PortDeviceBevel : TBevel;
    ButtonPanel: TButtonPanel;
    CbBaudRate : TComboBox;
    CbPort : TComboBox;
    CbDevice: TComboBox;
    CbStopBits : TComboBox;
    CbParity : TComboBox;
    CbDatabits : TComboBox;
    CbReadout: TComboBox;
    LblBaudRate : TLabel;
    LblDisplayDigits: TLabel;
    LblParity : TLabel;
    LblDatabits : TLabel;
    LblDevice: TLabel;
    LblStopBits : TLabel;
    LblPort : TLabel;
    LblReadout: TLabel;
    MainPanel : TPanel;
    RgHandshake: TRadioGroup;
    EdDigits: TSpinEdit;
    procedure CbDeviceChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FActivated: Boolean;
    FPresets: TSerPresetList;
    FPrevPresetItem: TSerPresetItem;
    FPrevIndex: integer;
    function  GetBaudrate: integer;
    function  GetDatabits: integer;
    function  GetDeviceName: string;
    function  GetDigits: Integer;
    function  GetHandshake: integer;
    function  GetParity: char;
    function  GetPort: string;
    function  GetReadout: Integer;
    function  GetStopBits: double;
    procedure SetBaudrate(value: integer);
    procedure SetDatabits(Value: integer);
    procedure SetDeviceName(Value: string);
    procedure SetDigits(Value: Integer);
    procedure SetHandshake(Value: integer);
    procedure SetParity(Value: char);
    procedure SetPort(Value: string);
    procedure SetReadout(Value: Integer);
    procedure SetStopBits(Value: double);
  public
    { public declarations }
    property DeviceName: string read GetDeviceName write SetDeviceName;
    property Baudrate: integer read GetBaudrate write SetBaudrate;
    property Databits: integer read GetDatabits write SetDatabits;
    property Handshake: integer read GetHandshake write SetHandshake;
    property Parity: char read GetParity write SetParity;
    property Port: string read GetPort write SetPort;
    property Stopbits: double read GetStopBits write SetStopBits;
    property Readout: Integer read GetReadout write SetReadout;
    property Digits: Integer read GetDigits write SetDigits;
  end;

var
  SerPortForm : TSerPortForm;


implementation

{$R *.lfm}

uses
  synaser, dlUtils;

{ TSerPresetItem }

procedure TSerPresetItem.Assign(AItem: TSerPresetItem);
begin
  Name := AItem.Name;
  BaudRate := AItem.BaudRate;
  DataBits := AItem.DataBits;
  StopBits := AItem.StopBits;
  Parity := AItem.Parity;
  Handshake := AItem.Handshake;
  ReadOut := AItem.ReadOut;
  Digits := AItem.Digits;
  Editable := AItem.Editable;
end;


{ TSerPresetList }

procedure TSerPresetList.AddDefault(const AName: string;
  const Presets: TDevicePresets);
var
  item: TSerPresetItem;
begin
  item := TSerPresetItem.Create;
  item.Name := AName;
  item.BaudRate := Presets.BaudRate;
  item.DataBits := Presets.DataBits;
  item.StopBits := Presets.StopBits;
  item.Parity := Presets.Parity;
  item.Handshake := Presets.Handshake;
  item.ReadOut := Presets.ReadOut;
  item.Digits := Presets.Digits;
  item.Editable := Presets.Editable;
  Add(item);
end;

function TSerPresetList.GetItem(AIndex:integer) : TSerPresetItem;
begin
  result := TSerPresetItem(inherited Items[AIndex]);
end;

function TSerPresetList.GetItemByName(const AName:string) : TSerPresetItem;
var
  i : integer;
begin
  for i:=0 to Count-1 do
    if SameText(Items[i].Name, AName) then begin
      result := Items[i];
      exit;
    end;
  result := nil;
end;

procedure TSerPresetList.ReadFromIni(ini:TCustomIniFile; const Section:string);
var
  List, L : TStringList;
  item : TSerPresetItem;
  s : string;
  i : integer;
begin
  Assert(ini <> nil);
  List := TStringList.Create;
  L := TStringList.Create;
  try
    L.Delimiter := ';';
    ini.ReadSection(Section, List);

    AddDefault('Conrad VC630', VC630Presets);
    AddDefault('Conrad VC820', VC820_840Presets);
    AddDefault('Conrad VC830', VC830_850Presets);
    AddDefault('Conrad VC840', VC820_840Presets);
    AddDefault('Conrad VC850', VC830_850Presets);
    AddDefault('Mercury MTTR01', MTTR01Presets);

    for i:=0 to List.Count-1 do begin
      s := ini.ReadString(Section, List[i], '');
      if (s <> '') and (s[1] = '(') then
        System.Delete(s, 1, 1);
      if (s <> '') and (s[Length(s)] = ')') then
        System.Delete(s, Length(s), 1);
      s := trim(s);
      if (s <> '') then begin
        L.Clear;
        L.DelimitedText := s;
        item := GetItemByName(List[i]);
        if item = nil then begin
          item := TSerPresetItem.Create;
          item.Name := List[i];
          Add(item);
        end;
        item.Baudrate := StrToInt(L.Values['Baudrate']);
        item.Databits := StrToInt(L.Values['DataBits']);
        item.StopBits := StrToFloat(L.Values['StopBits']);
        s := L.Values['Parity'];
        if s <> '' then item.Parity := s[1] else item.Parity := 'N';
        item.HandShake := StrToInt(L.Values['Handshake']);
        item.ReadOut := StrToInt(L.Values['ReadOut']);
        item.Digits := StrToInt(L.Values['Digits']);
        item.Editable := StrToInt(L.Values['Editable']);
      end;
    end;
    AddDefault('user-defined', OtherPresets);
  finally
    L.Free;
    List.Free;
  end;
end;

procedure TSerPresetList.SetItem(AIndex:integer; Value:TSerPresetItem);
begin
  inherited Items[AIndex] := Value;
end;

procedure TSerPresetList.WriteToIni(ini:TCustomIniFile; const Section:string);
var
  item : TSerPresetItem;
  i : integer;
  s : string;
begin
  Assert(ini <> nil);
  for i:=0 to Count-2 do begin  // -2, da user-defined nicht gespeichert werden soll
    item := Items[i];
    s := Format(
      '(Baudrate=%d; Databits=%d; StopBits=%0.1f; Parity=%s; '+
      'Handshake=%d; ReadOut=%d; Digits=%d; Editable=%d)',
      [item.Baudrate, item.Databits, item.StopBits, item.Parity,
       item.Handshake, item.ReadOut, item.Digits, item.Editable]
    );
    ini.WriteString(section, item.Name, s);
  end;
end;


{ TSerPortForm }

procedure TSerPortForm.CbDeviceChange(Sender: TObject);
var
  item : TSerPresetItem;
begin
  if FPrevIndex <> -1 then begin
    FPresets[FPrevIndex].Assign(FPrevPresetItem);
  end;

  item := FPresets[CbDevice.ItemIndex];
  SetBaudrate(item.BaudRate);
  SetDataBits(item.DataBits);
  SetStopBits(item.StopBits);
  SetParity(item.Parity);
  SetHandshake(item.Handshake);
  SetReadout(item.Readout);
  SetDigits(item.Digits);

  CbBaudrate.Enabled := (item.Editable and eBaudrate <> 0);
  CbDatabits.Enabled := (item.Editable and eDatabits <> 0);
  CbStopBits.Enabled := (item.Editable and eStopBits <> 0);
  CbParity.Enabled := (item.Editable and eParity <> 0);
  CbReadout.Enabled := (item.Editable and eReadout <> 0);
  EdDigits.Enabled := (item.Editable and eDigits <> 0);
  RgHandshake.Enabled := (item.Editable and eHandshake <> 0);

  FPrevPresetItem.Assign(item);
  FPrevIndex := CbDevice.ItemIndex;
end;

procedure TSerPortForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinHeight := MainPanel.Height + ButtonPanel.Height;
  end;
end;

procedure TSerPortForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ini: TCustomIniFile;
begin
  if CanClose then begin
    ini := CreateGlobalIni;
    try
      FPresets.WriteToIni(ini, DEVICES_SCT);
    finally
      ini.Free;
    end;
  end;
end;

procedure TSerPortForm.FormCreate(Sender: TObject);
var
  ini: TCustomIniFile;
  i: integer;
  L: TStrings;
begin
  FPresets := TSerPresetList.Create;

  ini := CreateGlobalIni;
  try
    FPresets.ReadFromIni(ini, DEVICES_SCT);
  finally
    ini.Free;
  end;

  if FPresets.Count = 0 then begin
    FPresets.AddDefault('Conrad VC630', VC630Presets);
    FPresets.AddDefault('Conrad VC820', VC820_840Presets);
    FPresets.AddDefault('Conrad VC830', VC830_850Presets);
    FPresets.AddDefault('Conrad VC840', VC820_840Presets);
    FPresets.AddDefault('Conrad VC850', VC830_850Presets);
    FPresets.AddDefault('Mercury MTTR01', MTTR01Presets);
    FPresets.AddDefault('user-defined', OtherPresets);
  end;

  FPrevPresetItem := TSerPresetItem.Create;
  FPrevIndex := -1;

  CbDevice.Items.Clear;
  for i:=0 to FPresets.Count-1 do
    CbDevice.Items.Add(FPresets[i].Name);

  L := TStringList.Create;
  try
    L.StrictDelimiter := true;
    L.CommaText := GetSerialPortNames;
    CbPort.Items.Assign(L);
    if CbPort.Items.Count = 1 then
      CbPort.ItemIndex := GetPortNo(CbPort.Items[0]);
  finally
    L.Free;
  end;
end;

procedure TSerPortForm.FormDestroy(Sender: TObject);
begin
  FPrevPresetItem.Free;
  FPresets.Free;
end;

function TSerPortForm.GetBaudrate: integer;
begin
  result := StrToInt(CbBaudrate.Text);
end;

function TSerPortForm.GetDatabits: integer;
begin
  result := StrToInt(CbDatabits.Text);
end;

function TSerPortForm.GetDigits: Integer;
begin
  if EdDigits.Text = '' then
    Result := 0
  else
    Result := EdDigits.Value;
end;

function TSerPortForm.GetHandshake: integer;
begin
  result := RgHandshake.ItemIndex;
end;

function TSerPortForm.GetParity: char;
var
  s : string;
begin
  s := Uppercase(CbParity.Items[CbParity.ItemIndex]);
  result := s[1];
end;

function TSerPortForm.GetPort: string;
begin
  if CbPort.ItemIndex = -1 then
    result := ''
  else
    result := CbPort.Items[CbPort.ItemIndex];
end;

function TSerPortForm.GetDeviceName: string;
begin
  if CbDevice.ItemIndex < CbDevice.Items.Count-1 then  // Count-1 due to "user-defined"
    result := cbDevice.Items[CbDevice.ItemIndex]
  else
    result := '';
end;

function TSerPortForm.GetReadout: Integer;
begin
  Result := CbReadout.ItemIndex;
end;

function TSerPortForm.GetStopBits: double;
begin
  result := StrToFloat(CbStopBits.Text);
end;

procedure TSerPortForm.SetBaudrate(Value: integer);
begin
  CbBaudrate.ItemIndex := CbBaudrate.Items.IndexOf(IntToStr(Value));
  if CbBaudrate.ItemIndex = -1 then
    raise Exception.CreateFmt('%d is not a valid baud rate.', [Value]);
end;

procedure TSerPortForm.SetDataBits(Value: integer);
begin
  CbDatabits.ItemIndex := CbDataBits.Items.IndexOf(IntToStr(Value));
  if CbDatabits.ItemIndex = -1 then
    raise Exception.Create('Illegal databits value');
end;

procedure TSerPortForm.SetDigits(Value: Integer);
begin
  if Value <= 0 then
    EdDigits.Clear
  else
    EdDigits.Value := Value;
end;

procedure TSerPortForm.SetHandshake(Value: integer);
begin
  RgHandshake.ItemIndex := Value;
end;

procedure TSerPortForm.SetParity(value: Char);
var
  i: integer;
  s: string;
begin
  for i:=0 to CbParity.Items.Count-1 do begin
    s := Uppercase(CbParity.Items[i]);
    if s[1] = Value then begin
      CbParity.ItemIndex := i;
      exit;
    end;
  end;
  raise Exception.Create('Invalid parity value');
end;

procedure TSerPortForm.SetPort(Value: string);
var
  i : integer;
begin
  i := CbPort.Items.IndexOf(Uppercase(Value));
  if i = -1 then
    MessageDlg(
      Format('The serial port %s is not available on this PC.', [Value]),
      mtError, [mbOK], 0)
  else
    CbPort.ItemIndex := i;
end;

procedure TSerPortForm.SetDeviceName(Value: string);
begin
  if Value = '' then
    CbDevice.ItemIndex := CbDevice.Items.Count - 1
  else
    CbDevice.ItemIndex := CbDevice.Items.IndexOf(Value);
  CbDeviceChange(nil);
end;

procedure TSerPortForm.SetReadout(Value: Integer);
begin
  CbReadout.ItemIndex := Value;
end;

procedure TSerPortForm.SetStopBits(Value: double);
begin
  CbStopBits.ItemIndex := dlUtils.GetStopBits(Value);
end;


end.


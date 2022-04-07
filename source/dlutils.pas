unit dlUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  dlGlobal;

function  CreateIni : TCustomIniFile;
function  CreateGlobalIni : TCustomIniFile;

function  GetPortName(APortNo: Integer): string;
function  GetPortNo(const APortName: string): integer;

function  GetStopBits(ACode: Integer): double;
function  GetStopBits(AValue: double): Integer;

function  GetHandshakeString(ACode: Integer): String;

function  IsEmptyNumber(AValue: Double): Boolean;
procedure PutInOrder(var x,y:double);

function  ConvertFromDateTime(AValue: TDateTime; AUnits: TTimeUnits): Double; inline;
function  ConvertToDateTime(AValue: Double; AUnits: TTimeUnits): TDateTime; inline;
function  ConvertTimeUnits(AValue: Double; FromUnits, ToUnits: TTimeUnits): Double;


implementation

uses
  Math, synaser;

type
  TMyMemIniFile = class(TMemIniFile)
  private
    FPointFormatSettings: TFormatSettings;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; override;
    procedure WriteFloat(const Section, Ident: string; Value: Double); override;
  end;

constructor TMyMemIniFile.Create(const AFileName: string;
  AEscapeLineFeeds : Boolean = False);
begin
  inherited Create(AFileName, AEscapeLineFeeds);
  FPointFormatSettings := DefaultFormatSettings;
  FPointFormatSettings.DecimalSeparator := '.';
  FPointFormatSettings.ThousandSeparator := ',';
end;

function TMyMemIniFile.ReadFloat(const Section, Ident: string;
  Default: Double): Double;
begin
  Result := StrToFloatDef(ReadString(Section, Ident, ''), Default, FPointFormatSettings);
end;

procedure TMyMemIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value, FPointFormatSettings));
end;


function CreateIni : TCustomIniFile;
var
  cfg : string;
begin
  cfg := GetAppConfigDir(false);
  if not DirectoryExists(cfg) then
    CreateDir(cfg);
  result := TMyMemIniFile.Create(GetAppConfigFile(false));
end;

function CreateGlobalIni : TCustomIniFile;
var
  cfg : string;
begin
  try
    cfg := ChangeFileExt(ParamStr(0), '.cfg');
    result := TMyMemIniFile.Create(cfg);
  except   // in case of lacking write permission to exe directory...
    cfg := GetAppConfigDir(true);
    if not DirectoryExists(cfg) then
      CreateDir(cfg);
    result := TMyMemIniFile.Create(GetAppConfigFile(true));
  end;
end;

function GetPortName(APortNo: Integer): string;
begin
  result := Format('COM%d', [APortNo]);
end;

function GetPortNo(const APortName: String): Integer;
var
  com : string;
begin
  com := Uppercase(trim(copy(APortName, 1, 3)));
  if (com = 'COM') then
    result := StrToInt(trim(copy(APortName, 4, 255)))
  else
    result := -1;
end;

function GetStopBits(ACode:Integer): double;
begin
  case ACode of
    SB1        : result := 1.0;
    SB1andHalf : result := 1.5;
    SB2        : result := 2.0;
  end;
end;

function GetStopBits(AValue:double): Integer;
const
  eps = 1e-6;
begin
  if SameValue(AValue, 1.0, eps) then
    result := SB1
  else
  if SameValue(AValue, 1.5, eps) then
    result := SB1andHalf
  else
  if SameValue(AValue, 2.0, eps) then
    result := SB2
  else
    raise Exception.Create('There can only be 1, 1.5, or 2 stop bits');
end;

function GetHandshakeString(ACode: Integer): String;
begin
  case ACode of
    hNone: Result := 'none';
    hHardware: Result := 'hardware';
    hSoftware: Result := 'software';
  end;
end;

function IsEmptyNumber(AValue: Double): Boolean;
begin
  Result := IsNaN(AValue);
//  result := abs(AValue - EMPTYNUMBER) < 1e-9;
end;

procedure PutInOrder(var x,y:double);
// Stellt sicher, dass x < y ist.
var
  z : double;
begin
  if not IsEmptyNumber(x) and not IsEmptyNumber(y) then begin
    if x > y then begin
      z := x;
      x := y;
      y := z;
    end;
  end;
end;

function ConvertFromDateTime(AValue: TDateTime; AUnits: TTimeUnits): Double;
begin
  Result := AValue * TIME_MULTIPLIER[AUnits];
end;

function ConvertToDateTime(AValue: Double; AUnits: TTimeUnits): TDateTime;
begin
  Result := AValue / TIME_MULTIPLIER[AUnits];
end;

function ConvertTimeUnits(AValue: Double; FromUnits,ToUnits: TTimeUnits): Double;
begin
  if FromUnits = ToUnits then
    Result := AValue
  else
    Result := ConvertFromDateTime(ConvertToDateTime(AValue, FromUnits), ToUnits);
end;

end.


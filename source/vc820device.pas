unit VC820device;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dlSerialDevice;

type
  TVC820Thread = class(TSerialThread)
  protected
    procedure Execute; override;
    function GetByteNumber(AByte: byte): Integer;
  end;

  TVC820Decoder = class(TSerialDecoder)
  private
    FPrevDecimals: Integer;
  protected
    procedure Decode(ABuffer: TSerialDataArray); override;
  public
    constructor Create; override;
  end;


implementation

uses
  Math;


{ TVC820Thread }

procedure TVC820Thread.Execute;
var
  buf: array[0..13] of byte;
begin
  BufferSize := 14;

  FSerial.RTS := false;
  FSerial.DTR := true;

  while not Terminated do begin
    // in case that the data are out of sync: look for the first data byte
    repeat
      FBuffer[0] := FSerial.RecvByte(TimeOut);
      FStatus := FSerial.LastError;
    until (GetByteNumber(FBuffer[0]) = 1) or (FStatus <> 0);
    if FStatus = 0 then begin
      FSerial.RecvBufferEx(@FBuffer[1], BufferSize-1, TimeOut);
      FStatus := FSerial.LastError;
      Synchronize(@DoDataAvail)
    end else
      Synchronize(@DoError);
  end;
end;

function TVC820Thread.GetByteNumber(AByte: byte): Integer;
// Each byte of the data stream contains the byte number in the higher
// nibble of the byte.
begin
  result := (AByte and $F0) shr 4;
end;


{ TVC820Decoder }

constructor TVC820Decoder.Create;
begin
  inherited Create;
  FDisplayFlags := [dfAC, dfAuto, dfDC, dfDelta, dfDiode, dfHold,
    dfBatt, dfUnits];
  FPrevDecimals := -1;
  FNumDigits := 4;
end;

procedure TVC820Decoder.Decode(ABuffer: TSerialDataArray);

  function CalcDigit(index1,index2:integer) : string;
  var
    code : byte;
  begin
    code := (ABuffer[index1] and $07) shl 4 + ABuffer[index2] and $0F;
    case code of
      $7D : result := '0';
      $05 : result := '1';
      $5B : result := '2';
      $1F : result := '3';
      $27 : result := '4';
      $3E : result := '5';
      $7E : result := '6';
      $15 : result := '7';
      $7F : result := '8';
      $3F : result := '9';
      $68 : result := 'L';
      else  result := '';
    end;
    if (ABuffer[index1] and $08 <> 0) then begin
      if index1 = 1 then
        result := '-' + result
      else
        result := '.' + result;
    end;
  end;

const
  BUFSIZE = 14;
var
  digitA, digitB, digitC, digitD: ansistring;
  rangechg: Boolean;
  i: Integer;

begin
  inherited Decode(ABuffer);

  if (ABuffer = nil) or (Length(ABuffer) <> BUFSIZE) then begin
    FStatus := sdrFormatError;
    exit;
  end;

  // The bytes are numbered from 1 to E. This number is in the higher nibble.
  // If not, then there are wrong data.
  for i:=0 to BUFSIZE-1 do
    if (ABuffer[i] and $F0) shr 4 <> i+1 then begin
      FStatus := sdrFormatError;
      exit;
    end;

  digitA := CalcDigit(1,2);
  digitB := CalcDigit(3,4);
  digitC := CalcDigit(5,6);
  digitD := CalcDigit(7,8);

  if (digitA = '') and (digitB = '0') and (digitC = '.L') and (digitD = '') then begin
    FValue := Infinity;
    FStatus := sdrOK; //verload;
  end else begin
    if (digitA = '') or (digitB = '') or (digitC = '') or (digitD = '') then begin
      FStatus := sdrFormatError;
      exit;
    end;

    if digitB[1] = '.' then begin
      FDecimals := 3;
      digitB[1] := FormatSettings.DecimalSeparator;
    end else
    if digitC[1] = '.' then begin
      FDecimals := 2;
      digitC[1] := FormatSettings.DecimalSeparator;
    end else
    if digitD[1] = '.' then begin
      FDecimals := 1;
      digitD[1] := FormatSettings.DecimalSeparator;
    end else
      FDecimals := 0;

    // Omit the first value after switching range, it would be a spike
    rangechg := (FPrevDecimals <> -1) and (FDecimals <> FPrevDecimals);
    FPrevDecimals := FDecimals;
    if rangechg then begin
      FStatus := sdrIgnore;
      exit;
    end;

    if TryStrToFloat(digitA + digitB + digitC + digitD, FValue) then
      FStatus := sdrOK
    else begin
      FValue := NaN;
      FStatus := sdrNumError;
    end;
  end;

  // Units
  FMultiplierExponent := 0;
  if ABuffer[9] and $02 <> 0 then
    FMultiplierExponent := 3;
  if ABuffer[9] and $04 <> 0 then
    FMultiplierExponent := -9;
  if ABuffer[9] and $08 <> 0 then
    FMultiplierExponent := -6;
  if ABuffer[10] and $02 <> 0 then
    FMultiplierExponent := 6;
  if ABuffer[10] and $08 <> 0 then
    FMultiplierExponent := -3;
  if ABuffer[10] and $04 <> 0 then
    FMultiplierExponent := -2;

  FQuantity := qNone;
  if ABuffer[11] and $04 <> 0 then
    FQuantity := qResistance;
  if ABuffer[11] and $08 <> 0 then
    FQuantity := qCapacitance;
  if ABuffer[12] and $02 <> 0 then
    FQuantity := qFrequency;
  if ABuffer[12] and $04 <> 0 then
    FQuantity := qVoltage;
  if ABuffer[12] and $08 <> 0 then
    FQuantity := qCurrent;
  if ABuffer[13] and $01 <> 0 then begin
    FQuantity := qTemperature;
    FFahrenheit := false;
  end;

  // Status display
  FDisplayFlagValues[dfDiode] := (ABuffer[9] and $01 <> 0);
  FDisplayFlagValues[dfAC] := (ABuffer[0] and $08 <> 0);
  FDisplayFlagValues[dfDC] := (ABuffer[0] and $04 <> 0);
  FDisplayFlagValues[dfAuto] := (ABuffer[0] and $02 <> 0);
  FDisplayFlagValues[dfHold] := (ABuffer[11] and $01 <> 0);
  FDisplayFlagValues[dfDelta] := (ABuffer[11] and $02 <> 0);
  FDisplayFlagValues[dfBatt] := (ABuffer[12] and $01 <> 0);
  FDisplayFlagValues[dfUnits] := true;
end;


end.


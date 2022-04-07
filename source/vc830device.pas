unit VC830Device;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dlSerialDevice;

type
  TVC830Thread = class(TSerialThread)
  protected
    procedure Execute; override;
  end;

  TVC830Decoder = class(TSerialDecoder)
  protected
    procedure Decode(ABuffer: TSerialDataArray); override;
  public
    constructor Create; override;
  end;


implementation

uses
  Math;

{ TVC830Thread }

procedure TVC830Thread.Execute;
const
  BUFSIZE = 12;  // eigentlich 14, aber #13#10 sind am Ende schon entfernt
var
  s: AnsiString;
  i: Integer;
  ok: Boolean;
begin
  FSerial.RTS := false;
  FSerial.DTR := true;

  while not Terminated do begin
//    SetLength(s, 128);
    s := FSerial.RecvTerminated(TimeOut, #13#10);
    FStatus := FSerial.LastError;
    if FStatus = 0 then begin
      SetLength(FBuffer, BUFSIZE);
      for i:=1 to BUFSIZE do
        FBuffer[i-1] := ord(s[i]);
      Synchronize(@DoDataAvail);
    end else
      Synchronize(@DoError);
  end;
end;


{ TVC830Decoder }

constructor TVC830Decoder.Create;
begin
  inherited Create;
  FNumDigits := 4;
  FDisplayFlags := [dfAC, dfAuto, dfBeep, dfDC, dfDiode, dfHold,
    dfBatt, dfRel, dfUnits, dfMax, dfMin];
end;

procedure TVC830Decoder.Decode(ABuffer: TSerialDataArray);
const
  DECPT : array['1'..'4'] of integer = (2, 3, -1, 4);      // '3' not used
var
  val: String;
  i: Integer;
begin
  inherited Decode(ABuffer);

  if (ABuffer = nil) or (Length(ABuffer) <> 12) then begin // CRLF already removed!
    FStatus := sdrFormatError;
    exit;
  end;

  if not (ansichar(ABuffer[0]) in ['+', '-']) then begin
    FStatus := sdrFormatError;
    exit;
  end;

  SetLength(val, 4);
  Move(ABuffer[1], val[1], 4);
  if val = '?0:?' then begin
    FValue := Infinity;
    FStatus := sdrOK;
  end else begin
    if ansichar(ABuffer[6]) <> '0' then begin
      FDecimals := FNumDigits - DECPT[ansichar(ABuffer[6])] + 1; //StrToInt(ansichar(ABuffer[6])) + 1;
      Insert(FormatSettings.DecimalSeparator, val, DECPT[ansichar(ABuffer[6])]);
    end else
      FDecimals := 0;
    if TryStrToFloat(val, FValue) then begin
      if ansichar(ABuffer[0]) = '-' then
        FValue := -FValue;
      FStatus := sdrOK;
    end else begin
      FValue := NaN;
      FStatus := sdrNumError;
    end;
  end;

  if ABuffer[8] and $02 <> 0 then
    FMultiplierExponent := -9                 // n
  else
  if ABuffer[9] and $80 <> 0 then
    FMultiplierExponent := -6                 // µ
  else
  if ABuffer[9] and $40 <> 0 then
    FMultiplierExponent := -3                 // m
  else
  if ABuffer[9] and $20 <> 0 then
    FMultiplierExponent := 3                  // k
  else
  if ABuffer[9] and $10 <> 0 then
    FMultiplierExponent := 6                  // M
  else
  if ABuffer[9] and $02 <> 0 then
    FMultiplierExponent := -2;                // %

  if ABuffer[10] and $80 <> 0 then
    FQuantity := qVoltage
  else
  if ABuffer[10] and $40 <> 0 then
    FQuantity := qCurrent
  else
  if ABuffer[10] and $20 <> 0 then
    FQuantity := qResistance
  else
  if ABuffer[10] and $08 <> 0 then
    FQuantity := qFrequency
  else
  if ABuffer[10] and $04 <> 0 then
    FQuantity := qCapacitance
  else
  if ABuffer[10] and $03 <> 0 then begin
    FQuantity := qTemperature;
    FFahrenheit := (ABuffer[10] and $01 <> 0);
  end else
    FQuantity := qUnknown;

  FDisplayFlagValues[dfAuto] := ABuffer[7] and $20 <> 0;
  FDisplayFlagValues[dfDC] := ABuffer[7] and $10 <> 0;
  FDisplayFlagValues[dfAC] := ABuffer[7] and $08 <> 0;
  FDisplayFlagValues[dfRel] := ABuffer[7] and $04 <> 0;
  FDisplayFlagValues[dfHold] := ABuffer[7] and $02 <> 0;
  FDisplayFlagValues[dfMax] := ABuffer[8] and $20 <> 0;
  FDisplayFlagValues[dfMin] := ABuffer[8] and $10 <> 0;
  FDisplayFlagValues[dfBatt] := ABuffer[8] and $04 <> 0;
  FDisplayFlagValues[dfBeep] := ABuffer[8] and $08 <> 0;
  FDisplayFlagValues[dfDiode] := ABuffer[8] and $04 <> 0;
  FDisplayFlagValues[dfUnits] := true;
end;


end.


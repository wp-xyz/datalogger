unit dlSerialDevice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synaser;

{ SerialThread }

type
  TSerialThread = class;
  TSerialDataArray = array of byte;

  TSerialThreadDataAvailEvent = procedure(Sender:TSerialThread;
    AData: TSerialDataArray) of object;

  TSerialThreadErrorEvent = procedure(Sender: TSerialThread;
    AStatus: Integer) of object;

  TSerialThread = class(TThread)
  private
    FTimeOut: Integer;
    FOnDataAvail: TSerialThreadDataAvailEvent;
    FOnError: TSerialThreadErrorEvent;
    function GetBufferSize: Integer;
    procedure SetBufferSize(AValue: Integer);

  protected
    FBuffer: TSerialDataArray;
    FSerial: TBlockSerial;
    FStatus: Integer;
    procedure DoDataAvail;
    procedure DoError;
    property BufferSize: integer read GetBufferSize write SetBufferSize;
    property Serial: TBlockSerial read FSerial;

  public
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    function SetPortParams(const APort: String; ABaudRate, ADataBits: Integer;
      AParity: char; AStopBits: Integer; ASoftFlow, AHardFlow: Boolean): Integer;

    property Terminated;
    property TimeOut: Integer read FTimeOut write FTimeOut default 500;
    property OnDataAvail: TSerialThreadDataAvailEvent read FOnDataAvail write FOnDataAvail;
    property OnError: TSerialThreadErrorEvent read FOnError write FOnError;
  end;

  TSerialThreadClass = class of TSerialThread;


{ SerialDecoder }

const
  // Decoder status values
  sdrOK = 0;
  sdrOverload = 1;
  sdrNumError = 2;
  sdrFormatError = 3;
  sdrIgnore = 4;

type
  TQuantity = (qUnknown, qNone, qVoltage, qCurrent, qResistance, qCapacitance,
    qFrequency, qTemperature);

  TDisplayFlag = (dfAC, dfAuto, dfBatt, dfBeep, dfDC, dfDelta, dfDiode, dfHold,
    dfMax, dfMin, dfRel, dfUnits);

  TDisplayFlags = set of TDisplayFlag;

  TSerialDecoder = class
  private
    function GetMultiplier: double;
    function GetQuantityText: String;
    function GetUnits(ARawData: Boolean): String;
    function GetValue(ARawData: Boolean): Double;
    function GetValueText(ARawData, WithUnits: Boolean): String;

  protected
    FBuffer: TSerialDataArray;
    FValue: Double;
    FDisplayFlags : TDisplayFlags;
    FDisplayFlagValues: Array[TDisplayFlag] of boolean;

    FAC: boolean;
    FAuto: boolean;
    FBeep: Boolean;
    FDC: boolean;
    FDelta: Boolean;
    FDiode: Boolean;
    FHold: boolean;
    FLowBatt: boolean;
    FQuantity: TQuantity;
    FRel: boolean;
    FFahrenheit: boolean;
    FMultiplierExponent: integer;
    FNumDigits: Integer;
    FDecimals: Integer;
    FStatus: Integer;

  public
    constructor Create; virtual;
    procedure Clear; virtual;
    function BufferToString(AsHex: Boolean): String;
    procedure Decode(ABuffer:TSerialDataArray); virtual;
    function DisplayFlagIsSet(AFlag:TDisplayFlag): Boolean;
    function DisplayFlagAsText(AFlag:TDisplayFlag): String;

    property Buffer: TSerialDataArray read FBuffer;
    property Multiplier: double read GetMultiplier;
    property NumDigits: Integer read FNumDigits write FNumDigits;
    property Quantity: TQuantity read FQuantity;
    property QuantityText: String read GetQuantityText;
    property DisplayedDecimals: Integer read FDecimals;
    property Status: Integer read FStatus write FStatus;
    property Units[ARawData:boolean]: string read GetUnits;
    property Value[ARawData:boolean]: double read GetValue;
    property ValueText[ARawData:boolean; WithUnits: Boolean]: String read GetValueText;
  end;

  TSerialDecoderClass = class of TSerialDecoder;


{ TSerialDevice }

type
  TSerialDevice = class;

  TSerialDataEvent = procedure(Sender: TObject; Decoder: TSerialDecoder) of object;
  TSerialErrorEvent = procedure(Sender: TObject; APortStatus: Integer) of object;

  TSerialDevice = class
  private
    FThreadClass: TSerialThreadClass;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnData: TSerialDataEvent;
    FOnError: TSerialErrorEvent;
  protected
    FThread: TSerialThread;
    FDecoder: TSerialDecoder;
    procedure ThreadDataAvailHandler(Sender: TSerialThread; ABuffer: TSerialDataArray);
    procedure ThreadErrorHandler(Sender: TSerialThread; AStatus: Integer);
    procedure ThreadTerminateHandler(Sender: TObject);
  public
    constructor Create(AThreadClass: TSerialThreadClass; ADecoderClass: TSerialDecoderClass);
    destructor Destroy; override;
    procedure Connect(APort:String; ABaudRate, ADataBits: Integer;
      AParity: Char; AStopBits: Integer; ASoftFlow, AHardFlow: Boolean; ATimeOut:Integer);
    function Connected: Boolean;
    procedure Disconnect;
    property Decoder: TSerialDecoder read FDecoder;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnData: TSerialDataEvent read FOnData write FOnData;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnError: TSerialErrorEvent read FOnError write FOnError;
  end;

function GetSerialDeviceErrorText(AStatus: Integer): String;


implementation

uses
  Math;

const
  QUANTITY_NAMES : array[TQuantity] of string = (
    '', '', 'Voltage', 'Current', 'Resistance', 'Capacitance', 'Frequency', 'Temperature'
  );
  QUANTITY_UNITS: array[TQuantity] of string = (
    '', '', 'V', 'A', 'Ohms', 'F', 'Hz', '');

  DISPLAYFLAG_NAMES : array[TDisplayFlag] of string = (
    'AC', 'Auto', 'Batt', 'Beep', 'DC', 'Delta', 'Diode', 'Hold',
    'Max', 'Min', 'Rel', '');

function GetSerialDeviceErrorText(AStatus: Integer): String;
begin
  case AStatus of
    0                  : Result := '';
    sdrOverload        : Result := 'Measurement result out of range.';
    sdrNumError        : Result := 'Measurement result is not a number.';
    sdrFormatError     : Result := 'Received data are not in the correct format.';
    sdrIgnore          : Result := 'Measurement is not complete. Ignore.';
    ErrAlreadyOwned    : Result := 'Already owned';
    ErrAlreadyInUse    : Result := 'Already in use';
    ErrWrongParameter  : Result := 'Wrong parameter';
    ErrPortNotOpen     : Result := 'Port not open';
    ErrNoDeviceAnswer  : Result := 'Device does not respond';
    ErrMaxBuffer       : Result := 'End of buffer reached';
    ErrTimeout         : Result := 'Timeout';
    ErrNotRead         : Result := 'Data not read';
    ErrFrame           : Result := 'The hardware detected a framing error.';
    ErrOverrun         : Result := 'A character-buffer overrun has occurred. The next character is lost.';
    ErrRxOver          : Result := 'An input buffer overflow has occurred. There is either no room in the input buffer, or a character was received after the end-of-file (EOF) character.';
    ErrRxParity        : Result := 'The hardware detected a parity error.';
    ErrTxFull          : Result := 'Tx full';
    else                 Result := 'Unspecified communication error.';
  end;
end;



{ TSerialThread }

constructor TSerialThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  FTimeout := 500;
  FSerial := TBlockSerial.Create;
end;

destructor TSerialThread.Destroy;
begin
  FSerial.Free;
  BufferSize := 0;
  inherited Destroy;
end;

procedure TSerialThread.DoDataAvail;
begin
  if Assigned(FOnDataAvail) then
    FOnDataAvail(self, FBuffer);
end;

procedure TSerialThread.DoError;
begin
  if Assigned(FOnError) then
    FOnError(self, FStatus);
end;

function TSerialThread.GetBufferSize: Integer;
begin
  result := Length(FBuffer);
end;

procedure TSerialThread.SetBufferSize(AValue: Integer);
begin
  SetLength(FBuffer, AValue);
end;

function TSerialThread.SetPortParams(const APort:String; ABaudRate, ADataBits: Integer;
  AParity:char; AStopBits:Integer; ASoftFlow, AHardFlow: Boolean): Integer;
begin
  with FSerial do begin
  //  CloseSocket;
    Connect(APort);
    Config(ABaudRate, ADataBits, AParity, AStopBits, ASoftFlow, AHardFlow);
    Result := LastError;
  end;
end;


{ TSerialDecoder }

constructor TSerialDecoder.Create;
begin
  inherited Create;
  FDisplayFlags := [];
  FDecimals := -1;  // no fixed count of decimals
  Clear;
end;

procedure TSerialDecoder.Clear;
var
  f: TDisplayFlag;
begin
  SetLength(FBuffer, 0);
  FValue := 0.0;
  FMultiplierExponent := 0;
  FQuantity := qUnknown;
  FFahrenheit := false;
  for f := Low(TDisplayFlag) to High(TDisplayFlag) do
    FDisplayFlagValues[f] := false;
end;

function TSerialDecoder.BufferToString(AsHex: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  if AsHex then begin
    if Length(FBuffer) > 1 then
      Result := Format('%2x', [FBuffer[0]]);
    for i:=1 to High(FBuffer) do
      Result := Format('%s %2x', [Result, FBuffer[i]]);
  end else begin
    SetLength(Result, Length(FBuffer));
    for i:=0 to High(FBuffer) do
      if FBuffer[i] = 0 then
        Result[i+1] := '.'
      else
        Result[i+1] := char(FBuffer[i]);
  end;
end;

procedure TSerialDecoder.Decode(ABuffer: TSerialDataArray);
var
  n: Integer;
begin
  Clear;
  n := Length(ABuffer);
  SetLength(FBuffer, n);
  Move(ABuffer[0], FBuffer[0], n);
end;

function TSerialDecoder.DisplayFlagIsSet(AFlag: TDisplayFlag): Boolean;
begin
  Result := FDisplayFlagValues[AFlag];
end;

function TSerialDecoder.DisplayFlagAsText(AFlag: TDisplayFlag): String;
begin
  if (AFlag in FDisplayFlags) and FDisplayFlagValues[AFlag] then begin
    if AFlag = dfUnits then
      result := GetUnits(true)
    else
      result := DISPLAYFLAG_NAMES[AFlag]
  end else
    result := '';
end;

function TSerialDecoder.GetMultiplier: Double;
begin
  Result := IntPower(10, FMultiplierExponent);
end;

function TSerialDecoder.GetQuantityText: String;
begin
  result := QUANTITY_NAMES[FQuantity];
end;

function TSerialDecoder.GetUnits(ARawData:boolean): string;
begin
  Result := '';
  case FQuantity of
    qUnknown, qNone :
      exit;
    qTemperature :
      if FFahrenheit then
        Result := 'F'
      else
        Result := '°C';
    else
      if ARawData then begin
        if FMultiplierExponent = -9 then
          Result := 'n'
        else if FMultiplierExponent = -6 then
          Result := 'µ'
        else if FMultiplierExponent = -3 then
          Result := 'm'
        else if FMultiplierExponent = 3 then
          Result := 'k'
        else if FMultiplierExponent = 6 then
          Result := 'M'
        else if FMultiplierExponent = -2 then
          Result := '%';
      end;
      Result := Result + QUANTITY_UNITS[FQuantity];
  end;
end;

function TSerialDecoder.GetValue(ARawData: Boolean): double;
begin
  Result := FValue;
  if not ARawData and not IsInfinite(FValue) and not IsNaN(FValue) then
    Result := FValue * GetMultiplier;
end;

function TSerialDecoder.GetValueText(ARawData, WithUnits: Boolean): String;
var
  v: double;
  us: String;
begin
  v := GetValue(ARawData);

  if ARawData then begin
    if FDecimals = -1 then
      Result := Format('%.12g', [v])
    else
      Result := Format('%.*f', [FDecimals, v])
  end else begin
    if (abs(v) < 1E-7) and (abs(v) > 1E7) then
      Result := Format('%.4e', [v])
    else
      Result := Format('%.12g', [v]);
  end;

  if WithUnits then begin
    us := GetUnits(ARawData);
    if us <> '' then
      Result := Format('%s %s', [Result, us]);
  end;
end;


{ TSerialDevice }

constructor TSerialDevice.Create(AThreadClass: TSerialThreadClass;
  ADecoderClass: TSerialDecoderClass);
begin
  inherited Create;
  FThreadClass := AThreadClass;
  FDecoder := ADecoderClass.Create;
end;

destructor TSerialDevice.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  FDecoder.Free;
  inherited Destroy;
end;

procedure TSerialDevice.Connect(APort:String; ABaudRate, ADataBits: Integer;
  AParity: Char; AStopBits: Integer; ASoftFlow, AHardFlow: Boolean;
  ATimeOut: Integer);
var
  status: Integer;
begin
  if FThread <> nil then
    FThread.Terminate;

  FThread := FThreadClass.Create(true);
  FThread.FreeOnTerminate := true;
  FThread.OnDataAvail := @ThreadDataAvailHandler;
  FThread.OnError := @ThreadErrorHandler;
  FThread.OnTerminate := @ThreadTerminateHandler;
  FThread.TimeOut := ATimeOut;
  status := FThread.SetPortParams(APort, ABaudRate, ADataBits, AParity, AStopBits,
    ASoftFlow, AHardFlow);
  if status = 0 then begin
    FThread.Start;
    if Assigned(FOnConnect) then
      FOnConnect(self);
  end else
    FOnError(self, status);
end;

function TSerialDevice.Connected: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TSerialDevice.Disconnect;
begin
  if FThread <> nil then begin
    FThread.Terminate;
    FThread.OnDataAvail := nil;
    FThread.OnError := nil;
    if Assigned(FOnDisconnect) then
      FOnDisconnect(self);
  end;
end;

procedure TSerialDevice.ThreadDataAvailHandler(Sender: TSerialThread;
  ABuffer: TSerialDataArray);
begin
  if Assigned(FOnData) then begin
    FDecoder.Decode(ABuffer);
    FOnData(Self, FDecoder);
    if FDecoder.Status <> 0 then
      ThreadErrorHandler(nil, FDecoder.Status);
  end;
end;

procedure TSerialDevice.ThreadErrorHandler(Sender: TSerialThread;
  AStatus: Integer);
begin
  if Assigned(FOnError) then
    FOnError(Self, AStatus);
end;

procedure TSerialDevice.ThreadTerminateHandler(Sender: TObject);
begin
  FThread := nil;
end;

end.


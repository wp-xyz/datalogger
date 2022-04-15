unit dlGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TATypes;

const
  //EMPTYNUMBER = -9.999;
  OVRLOAD = 99999;
  UNDEFINED_NUMBER = 1E308;
  UNDEFINED_DATE = -1;

  DEVICES_SCT = 'Devices';

  HOURS_PER_DAY = 24;
  MINUTES_PER_DAY = HOURS_PER_DAY*60;
  SECONDS_PER_DAY = MINUTES_PER_DAY*60;

  TAB = #9;

  IMGINDEX_START = 4;
  IMGINDEX_STOP  = 5;

  DEFAULT_SERIES_COUNT = 12;

  eNone      = $00;
  eBaudrate  = $01;
  eDatabits  = $02;
  eStopBits  = $04;
  eParity    = $08;
  eHandshake = $10;
  eReadOut   = $20;
  eDigits    = $40;
  eAny       = $FF;

  hNone      = 0;
  hSoftware  = 1;
  hHardware  = 2;

  roUnknown  = 0;
  roSegments = 1;
  roAsciiDigits = 2;
     // Byte      1 2 3 4 5 6 7 8 9 10 11 12 13 14
     // Bsp. 1    D C   - 3 , 9 9 9    V        CR
     // Bsp. 2    O H M   3 , 9 9 9 M  o  h  m  CR

type
  TTimeUnits = (tuSeconds, tuMinutes, tuHours);
  TTimeDisplay = (tdNumber, tdDateTime);

  TMeasIntervalCondition = (micStart, micTimeAfter, micValueAbove, micValueBelow);

  TSeriesStyle = (ssLines, ssSymbols, ssBoth);

  TDataMode = (dmRaw, dmTransformed);

  TTransformFlag = (tfOK, tfTooSmallIn, tfTooLargeIn,
    tfTooSmallOut, tfTooLargeOut, tfError);

  TDevicePresets = record
    Baudrate: integer;
    Databits: integer;
    StopBits: double;
    Parity: char;
    Handshake: integer;  // see hXXXX constants
    ReadOut: Integer;     // see roXXXX constants
    Digits: Integer;
    Editable: integer;   // see eXXXX constants
  end;

  TDeviceSettings = record
    Name: String;
    Readout: Integer;
    Digits: Integer;
    Port: String;
    Baudrate: Integer;
    Databits: Integer;
    StopBits: Double;
    Parity: Char;
    Handshake: Integer;  // see hXXXX constants
  end;

  TMeasIntervalSettings = record
    Active: Boolean;
    Interval: Double;
    Condition: TMeasIntervalCondition;
    ConditionLimit: Double;
    ConditionLimitUnits: TTimeUnits;
  end;

  TMeasSettings = record
    IntervalSettings: Array[0..2] of TMeasIntervalSettings;
    Interval: Double;
    Transformation: String;
    TimeUnits: TTimeUnits;
  end;

  TSeriesSettings = record
    Style: TSeriesStyle;
    LineWidth: Integer;
    LineStyle: TPenStyle;
    LineColor: TColor;
    Symbol: TSeriesPointerStyle;
    SymbolColor: TColor;
    SymbolBorderColor: TColor;
  end;

  TDiagramSettings = record
    TimeDisplay: TTimeDisplay;
    Logarithmic: Boolean;
    FullGrids: Boolean;
    SeriesSettings: array[0..DEFAULT_SERIES_COUNT-1] of TSeriesSettings;
  end;


var
  VC630Presets: TDevicePresets = (
    BaudRate: 4800;
    DataBits: 7;
    StopBits: 2.0;
    Parity: 'N';
    Handshake: hNone;
    ReadOut: roAsciiDigits;
    Digits: 4;
    Editable: eNone;
  );
  VC820_840Presets: TDevicePresets = (
    BaudRate: 2400;
    DataBits: 8;
    StopBits: 2;
    Parity: 'N';
    Handshake: hNone;
    ReadOut: roSegments;
    Digits: 4;
    Editable: eNone;
  );
  VC830_850Presets: TDevicePresets = (
    BaudRate: 2400;
    DataBits: 8;
    StopBits: 2.0;
    Parity: 'N';
    Handshake: hNone;
    ReadOut: roAsciiDigits;
    Digits: 4;
    Editable: eNone;
  );
  OtherPresets: TDevicePresets = (
    Baudrate: 0;
    Databits: 8;
    StopBits: 2.0;
    Parity: 'N';
    Handshake: hNone;
    ReadOut: roSegments;
    Digits: -1;
    Editable: eAny;
  );

  DeviceSettings: TDeviceSettings = (
    Name: '';
    Readout: roUnknown;
    Digits: 1;
    Port: 'COM1';
    BaudRate: 2400;
    DataBits: 8;
    StopBits: 2;
    Parity: 'N';
    Handshake: hNone;
  );

  MeasSettings: TMeasSettings = (
    IntervalSettings : (
      (Active: true;  Interval:0.5; Condition: micStart;     ConditionLimit:  0.0; ConditionLimitUnits: tuSeconds),
      (Active: false; Interval: 10; Condition: micTimeAfter; ConditionLimit: 60.0; ConditionLimitUnits: tuSeconds),
      (Active: false; Interval: 60; Condition: micTimeAfter; ConditionLimit: 3600; ConditionLimitUnits: tuSeconds)
    );
    Interval: 0.5;
    Transformation: '';
    TimeUnits: tuSeconds;
  );

  DiagramSettings: TDiagramSettings = (
    TimeDisplay: tdNumber;
    Logarithmic: false;
    FullGrids: false;
    SeriesSettings: (
      (Style: ssLines; LineWidth: 3; LineStyle: psSolid; LineColor: clBlue;
        Symbol: psCircle; SymbolColor: clBlue; SymbolBorderColor: clBlue),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clRed;
        Symbol: psCircle; SymbolColor: clRed; SymbolBorderColor: clRed),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clGreen;
        Symbol: psCircle; SymbolColor: clGreen; SymbolBorderColor: clGreen),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clFuchsia;
        Symbol: psCircle; SymbolColor: clFuchsia; SymbolBorderColor: clFuchsia),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clOlive;
        Symbol: psCircle; SymbolColor: clOlive; SymbolBorderColor: clOlive),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clMaroon;
        Symbol: psCircle; SymbolColor: clMaroon; SymbolBorderColor: clMaroon),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clNavy;
        Symbol: psCircle; SymbolColor: clNavy; SymbolBorderColor: clNavy),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clTeal;
        Symbol: psCircle; SymbolColor: clTeal; SymbolBorderColor: clTeal),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clPurple;
        Symbol: psCircle; SymbolColor: clPurple; SymbolBorderColor: clPurple),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clYellow;
        Symbol: psCircle; SymbolColor: clYellow; SymbolBorderColor: clYellow),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clLime;
        Symbol: psCircle; SymbolColor: clLime; SymbolBorderColor: clLime),
      (Style: ssLines; LineWidth: 1; LineStyle: psSolid; LineColor: clAqua;
        Symbol: psCircle; SymbolColor: clAqua; SymbolBorderColor: clAqua)
    );
  );


const
  TIME_UNITS : array[TTimeUnits] of String = (
    'seconds', 'minutes', 'hours'
  );
  TIME_CAPTION : array[TTimeUnits] of String = (
    'Time, s', 'Time, min', 'Time, h'
  );
  TIME_MULTIPLIER : array[TTimeUnits] of Double = (
    SECONDS_PER_DAY, MINUTES_PER_DAY, HOURS_PER_DAY
  );


implementation

end.


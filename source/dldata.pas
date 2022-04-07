unit dlData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, laz2_xmlread, laz2_DOM, fpexprpars,
  fpstypes, fpspreadsheet, fpsallformats,
  TACustomSeries,
  dlGlobal;

type
  TDataParser = class(TFPExpressionParser)
  public
    constructor Create(AOwner:TComponent); override;
  end;

  TDataItem = class
    Time : double;
    Value: double;
    Comment: String;
  end;

  TDataList = class(TFPObjectList)
  private
    FTimeUnits: TTimeUnits;
    FRawQuantName: String;
    FRawUnits: String;
    FRawMultiplier: Double;  // Multiplies raw value to get to SI units.
    FSeries: TChartSeries;
    FFileName: string;
    FTimeOffset: double;
    FMaxTime: double;
    FMinValue: Double;
    FMaxValue: Double;
    FTransQuantName: String;
    FTransUnits: String;
    FTransMaxIn: Double;
    FTransMinIn: Double;
    FTransMaxOut: Double;
    FTransMinOut: Double;
    FTransLog: Boolean;
    FParser: TFPExpressionParser;
    FParserVariable: TFPExprIdentifierDef;
    FMeasDate: TDateTime;
    function GetCaption(ATransformedData: Boolean): String;
    function GetItem(AIndex: Integer): TDataItem;
    function GetTransExpression: String;
    procedure SetItem(AIndex: Integer; AValue: TDataitem);
    procedure SetTimeUnits(AValue: TTimeUnits);
    procedure SetTransExpression(const AValue: String);

  protected
    procedure ConvertTimeUnits(ANewUnits: TTimeUnits);

  public
    constructor Create;
    destructor Destroy; override;
    function AddValue(ATime, AValue: Double; const AComment: String = ''): TDataItem;
    function CanSetLogarithmic: boolean;
    procedure Clear;
    procedure CopyTransformationFrom(AData: TDataList);
    function HasComments: Boolean;
    function HasTransformation: Boolean;
    procedure LoadFromTextFile(const AFileName: String);
    procedure LoadFromXMLFile(const AFileName: String);
    procedure SaveAsSpreadsheetFile(const AFileName: String;
      AFormat: TsSpreadsheetFormat; ACommentLinesOnly: Boolean = false);
    procedure SaveAsTextFile(const AFileName: String; ACommentLinesOnly: Boolean = false);
    procedure SaveAsXMLFile(const AFileName: String);
    function Transform(AValue: Double; out AFlag: TTransformFlag): double;
    property Items[AIndex: Integer]: TDataItem read GetItem write SetItem;
    property Series: TChartSeries read FSeries write FSeries;
    property FileName: string read FFileName write FFilename;
    property MeasDate: TDateTime read FMeasDate write FMeasDate;
    property Caption[ATransformedData: Boolean]: String read GetCaption;
    property RawQuantName: String read FRawQuantName write FRawQuantName;
    property RawUnits: String read FRawUnits write FRawUnits;
    property RawMultiplier: Double read FRawMultiplier write FRawMultiplier;
    property TimeOffset: double read FTimeOffset write FTimeOffset;
    property TimeUnits: TTimeUnits read FTimeUnits write SetTimeUnits;
    property TransExpression: String read GetTransExpression write SetTransExpression;
    property TransQuantName: String read FTransQuantName write FTransQuantName;
    property TransUnits: String read FTransUnits write FTransUnits;
    property TransMaxIn: Double read FTransMaxIn write FTransMaxIn;
    property TransMaxOut: Double read FTransMaxOut write FTransMaxOut;
    property TransMinIn: Double read FTransMinIn write FTransMinIn;
    property TransMinOut: Double read FTransMinOut write FTransMinOut;
    property TransLog: Boolean read FTransLog write FTransLog;
  end;


implementation

uses
  Math, StrUtils,
  fpsUtils, fpsXMLCommon,
  dlUtils, dlTransformations;


{ Utilities }

function GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if ANode = nil then
    exit;

  Found := false;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then begin
      Found := true;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;


{ TDataParser }

procedure dp_power(var Result:TFPExpressionResult; const Args:TExprParameterArray);
begin
  Result.resFloat := power(Args[0].resFloat, Args[1].resFloat);
end;

constructor TDataParser.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  BuiltIns := [bcMath, bcBoolean];
  Identifiers.AddFunction('power', 'F', 'F', @dp_power);
  Identifiers.AddFloatVariable('x', 1.0);
end;


{ TDataList }

constructor TDataList.Create;
begin
  inherited Create;
  FRawMultiplier := 1.0;
  FTimeOffset := 0.0;
  FParser := TDataParser.Create(nil);
  FParserVariable := TDataParser(FParser).IdentifierByName('x');
  Clear;
end;

destructor TDataList.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TDataList.AddValue(ATime, AValue: Double;
  const AComment: String = ''): TDataItem;
begin
  result := TDataItem.Create;
  result.Time := ATime;
  result.Value := AValue;
  result.Comment := AComment;
  Add(result);
  FMaxTime := Max(FMaxTime, ATime);
  FMaxValue := Max(FMaxValue, AValue);
  FMinValue := Min(FMinValue, AValue);
end;

function TDataList.CanSetLogarithmic: boolean;
begin
  Result := FMinValue > 0;
end;

procedure TDataList.Clear;
begin
  inherited;
  FMaxTime := 0;
  FMinValue := 1E308;
  FMaxValue := -FMinValue;
end;

procedure TDataList.ConvertTimeUnits(ANewUnits: TTimeUnits);
var
  t: TDateTime;
  i: Integer;
begin
  if ANewUnits = FTimeUnits then
    exit;
  for i:=0 to Count-1 do
    Items[i].Time := dlUtils.ConvertTimeUnits(Items[i].Time, FTimeUnits, ANewUnits);
  FTimeUnits := ANewUnits;
end;

procedure TDataList.CopyTransformationFrom(AData: TDataList);
begin
  if AData = nil then begin
    TransExpression := '';
    TransQuantName := '';
    TransUnits := '';
    TransMaxIn := NaN;
    TransMaxOut := NaN;
    TransMinIn := NaN;
    TransMinOut := NaN;
    TransLog := false;
  end else begin
    TransExpression := AData.TransExpression;
    TransQuantName := AData.TransQuantName;
    TransUnits := AData.TransUnits;
    TransMaxIn := AData.TransMaxIn;
    TransMaxOut := AData.TransMaxOut;
    TransMinIn := AData.TransMinIn;
    TransMinOut := AData.TransMinOut;
    TransLog := AData.TransLog;
  end;
end;

function TDataList.GetCaption(ATransformedData: Boolean): String;
begin
  if ATransformedData then
    Result := IfThen(FTransUnits = '', FTransQuantName, FTransQuantName + ', ' + FTransUnits)
  else
    Result := IfThen(FRawUnits = '', FRawQuantname, FRawQuantName + ', ' + FRawUnits);
end;

function TDataList.GetItem(AIndex: Integer): TDataItem;
begin
  result := TDataItem(inherited Items[AIndex]);
end;

function TDataList.GetTransExpression: String;
begin
  Result := FParser.Expression;
end;

function TDataList.HasComments: Boolean;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if Items[i].Comment <> '' then exit(true);
  Result := false;
end;

function TDataList.HasTransformation: Boolean;
begin
  Result := TransExpression <> '';
end;

procedure TDataList.LoadFromTextFile(const AFileName: String);
var
  F : TextFile;
  s, sx, sy : string;
  p : integer;
  x, y : double;
  res : integer;
  L: TStringList;
  ok: Boolean;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  AssignFile(F, AFileName);
  Reset(F);

  Clear;

  L := TStringList.Create;
  try
    L.Delimiter:= TAB;
    L.StrictDelimiter := true;
    while not EoF(F) do begin
      ReadLn(F, s);
      if s = '' then
        Continue;
      if (s[1] <> '#') then begin
        L.DelimitedText := s;
        ok := false;
        if (L.Count > 1) then
          ok := TryStrToFloat(L[0], x, fs) and TryStrToFloat(L[1], y, fs);
        if ok then begin
          if (L.Count > 2) and (L[2] <> '') then
            AddValue(x, y, L[2])
          else
            AddValue(x, y);
        end;
      end else
      if LeftStr(s, 5) = '#Time' then begin
        L.DelimitedText := s;
        s := L[1];
        p := pos(',', L[1]);
        FTransQuantName := Copy(L[1], 1, p-1);
        FTransUnits := trim(Copy(L[1], p+1, MaxInt));
      end;
    end;

    FFileName := AFileName;
    FTimeUnits := tuSeconds;
  finally
    CloseFile(F);
  end;
end;


procedure TDataList.LoadFromXMLFile(const AFileName: String);
var
  measnode: TDOMNode;
  node: TDOMNode;
  transnode, itemnode, valuenode: TDOMNode;
  doc: TXMLDocument;
  nodename: String;
  s, sComment: String;
  fs: TFormatSettings;
  t, x: Double;
  tu: TTimeUnits;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  FFileName := AFileName;
  FTimeUnits := tuSeconds;

  ReadXMLFile(doc, AFileName);
  node := doc.FirstChild;
  while (node <> nil) do begin
    nodeName := node.NodeName;
    if nodeName <> 'measurement' then begin
      node := node.NextSibling;
      continue;
    end;
    measnode := node;
    break;
  end;

  node := measnode.FirstChild;
  while node <> nil do begin
    nodename := node.NodeName;

    // Read transformation
    if nodename = 'transformation' then begin
      TransExpression := GetAttrValue(node, 'expression');
      FTransQuantName := GetAttrValue(node, 'quantity-name');
      FTransUnits := GetAttrValue(node, 'units');

      s := GetAttrValue(node, 'min-in');
      FTransMinIn := NaN;
      if (s <> '') and TryStrToFloat(s, x, fs) then FTransMinIn := x;

      s := GetAttrValue(node, 'max-in');
      FTransMaxIn := NaN;
      if (s <> '') and TryStrToFloat(s, x, fs) then FTransMaxIn := x;

      s := GetAttrValue(node, 'min-out');
      FTransMinOut := NaN;
      if (s <> '') and TryStrToFloat(s, x, fs) then FTransMinOut := x;

      s := GetAttrValue(node, 'max-out');
      FTransMaxOut := NaN;
      if (s <> '') and TryStrToFloat(s, x, fs) then FTransMaxOut := x;

      s := GetAttrValue(node, 'logarithmic-plot');
      if s = 'true' then FTransLog := true else FTransLog := false;
    end
    else
    // Read values
    if nodename = 'raw-data' then begin
      FRawQuantName := GetAttrValue(node, 'name');
      FRawUnits := GetAttrValue(node, 'units');
      s := GetAttrValue(node, 'multiplier');
      if (s <> '') and TryStrToFloat(s, x, fs)
        then FRawMultiplier := x else FRawMultiplier := 1.0;
      s := GetAttrValue(measnode, 'start-datetime');
      if s <> '' then
        FMeasDate := StrToDate(s);
      s := GetAttrValue(measnode, 'time-units');
      if s <> '' then
        for tu in TTimeUnits do
          if TIME_UNITS[tu] = s then begin
            FTimeUnits := tu;
            break;
          end;

      itemnode := node.Firstchild;
      while itemnode <> nil do begin
        nodename := itemnode.NodeName;
        if nodename = 'item' then begin
          valuenode := itemnode.FirstChild;
          t := NaN;
          x := NaN;
          sComment := '';
          while valuenode <> nil do begin
            nodename := valuenode.NodeName;
            if nodename = 'time' then begin
              s := GetAttrValue(valuenode, 'value');
              if (s <> '') and TryStrToFloat(s, t, fs) then else t := NaN;
            end;
            if nodename = 'value' then begin
              s := GetAttrValue(valuenode, 'value');
              if (s <> '') and TryStrToFloat(s, x, fs) then else x := NaN;
              //sTrans := GetAttrValue(valuenode, 'transformed-value');
            end;
            if nodename = 'comment' then
              sComment := valuenode.NodeValue;
            valuenode := valuenode.NextSibling;
          end;
          if not IsNaN(t) then
            AddValue(t, x, sComment);
        end;
        itemnode := itemnode.NextSibling;
      end;
    end;
    node := node.NextSibling;
  end;
end;


procedure TDataList.SaveAsSpreadsheetFile(const AFileName: String;
  AFormat: TsSpreadsheetFormat; ACommentLinesOnly: Boolean = false);
var
  r,c: Cardinal;
  workbook: TsWorkbook;
  worksheet: TsWorksheet;
  i: Integer;
  flag: TTransformFlag;
  cTime, cRaw, cTrans, cComment: LongInt;
  x, y, yt: Double;
begin
  cTime := 0;
  cRaw := 1;
  if TransExpression <> '' then begin
    cTrans := cRaw + 1;
    cComment := cTrans + 1;
  end else begin
    cTrans := -1;
    cComment := cRaw + 1;
  end;
  if not HasComments then
    cComment := -1;

  workbook := TsWorkbook.Create;
  try
    worksheet := workbook.AddWorksheet('Data');

    // Write header row
    r := 0;
    worksheet.WriteUTF8Text(r, cTime, TIME_CAPTION[FTimeUnits]);
    worksheet.WriteUTF8Text(r, cRaw, Caption[false]);
    if cTrans <> -1 then
      worksheet.WriteUTF8Text(r, cTrans, Caption[true]);
    if cComment <> -1 then
      worksheet.WriteUTF8Text(r, cComment, 'Comments');

    // Format header row in bold
    for i := 0 to worksheet.GetLastColIndex do
      worksheet.WriteFontStyle(r, i, [fssBold]);

    // Write data cells
    for i := 0 to Count-1 do begin
      if ACommentLinesOnly and (Items[i].Comment = '') then
        Continue;
      x := Items[i].Time;
      y := Items[i].Value;
      yt := Transform(y, flag);
      inc(r);
      if DiagramSettings.TimeDisplay = tdDateTime then
        worksheet.WriteDateTime(r, cTime, x, nfLongTime)
      else
        worksheet.WriteNumber(r, cTime, x);
      worksheet.WriteNumber(r, cRaw, y);
      if (cTrans <> -1) and not IsNaN(yt) then
        worksheet.WriteNumber(r, cTrans, yt);
      if cComment <> -1 then
        worksheet.WriteUTF8Text(r, cComment, Items[i].Comment);
    end;

    // Save to file
    workbook.WriteToFile(AFileName, AFormat, true)
  finally
    workbook.Free;
  end;
end;


procedure TDataList.SaveAsTextFile(const AFilename:string;
  ACommentLinesOnly: Boolean = false);
var
  F: TextFile;
  i: integer;
  x, y, yt: double;
  sx: String;
  flag: TTransformFlag;
  item: TDataItem;
  hasTrans: Boolean;
  rawTitle, transTitle: String;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  hasTrans := TransExpression <> '';
  rawTitle := GetCaption(false);
  transTitle := GetCaption(true);

  AssignFile(F, AFileName);
  Rewrite(F);
  try
    // add "#" for compatibility with gnuplot
    WriteLn(F, '#Measurement data');
    WriteLn(F, '#', DateToStr(FMeasDate));
    WriteLn(F);
    Write(F, '#', TIME_CAPTION[FTimeUnits], TAB, rawTitle);
    if hasTrans then
      Write(F, TAB, transTitle);
    if HasComments then
      Write(F, TAB, 'Comments');
    WriteLn(F);
    for i:=0 to Count-1 do begin
      item := Items[i];
      if ACommentLinesOnly and (item.Comment = '') then
        Continue;
      x := item.Time;
      if DiagramSettings.TimeDisplay = tdDateTime then
        sx := FormatDateTime('c', x)
      else
        sx := Format('%g', [x], fs);
      y := item.Value;
      if hasTrans then begin
        yt := Transform(y, flag);
        if IsEmptyNumber(yt) then
          WriteLn(F, sx, TAB, y, TAB, '', TAB, item.Comment)
        else
          WriteLn(F, sx, TAB, y, TAB, yt, TAB, item.Comment);
      end else
        WriteLn(F, sx, TAB, y, TAB, item.Comment);
    end;

    FFileName := AFileName;
  finally
    CloseFile(F);
  end;
end;


procedure TDataList.SaveAsXMLFile(const AFileName: String);
var
  stream: TMemoryStream;
  i: Integer;
  item: TDataItem;
  sName, sUnits: String;
  sMinIn, sMaxIn, sMinOut, sMaxOut, sMultiplier: String;
  sTrans: String;
  valTrans: Double;
  hasTrans: Boolean;
  flags: TTransformFlag;
  fs: TFormatSettings;
begin
  stream := TMemoryStream.Create;
  try
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';

    AppendToStream(stream,
      '<?xml version="1.0" encoding="UTF-8" ?>');

    AppendToStream(stream,
      '<measurement>');

    hasTrans := TransExpression <> '';
    if hasTrans then begin
      sName := IfThen(FTransQuantName = '', '', Format('quantity-name="%s" ', [FTransQuantName]));
      sUnits := IfThen(FTransUnits = '', '', Format('units="%s" ', [FTransUnits]));
      sMinIn := IfThen(IsNan(FTransMinIn), '', Format('min-in="%g" ', [FTransMinIn]));
      sMaxIn := IfThen(IsNan(FTransMaxIn), '', Format('max-in="%g" ', [FTransMaxIn]));
      sMinOut := IfThen(IsNan(FTransMinOut), '', Format('min-out="%g" ', [FTransMinOut]));
      sMaxOut := IfThen(IsNan(FTransMaxOut), '', Format('max-out="%g" ', [FTransMaxOut]));
      AppendToStream(stream,
        Format(
        '<transformation expression="%s" ' +
          sMinIn + sMaxIn + sMinOut + sMaxOut +
          'logarithmic-plot="%s" ' +
          sName + sUnits + '/>', [
        TransExpression,
        BoolToStr(FTransLog, true)
        ], fs) );
    end;

    sName := IfThen(FRawQuantName = '', '', Format('name="%s" ', [FRawQuantName]));
    sUnits := IfThen(FRawUnits = '', '', Format('units="%s" ', [FRawUnits]));
    AppendToStream(stream,
      Format(
        '<raw-data '+
          sName + sUnits + 'multiplier="%g" count="%d" start-datetime="%s" time-units="%s">', [
        FRawMultiplier, Count, DateToStr(FMeasDate), TIME_UNITS[FTimeUnits]
      ], fs));

    for i := 0 to Count-1 do begin
      item := Items[i];
      sTrans := '';
      if hasTrans then begin
        valTrans := Transform(item.Value, flags);
        sTrans := IfThen(flags = tfOK, Format('transformed-value="%g" ', [valTrans], fs));
      end;
      AppendToStream(stream,
          '<item>');
      AppendToStream(stream, Format(
            '<time value="%g" />',
            [item.Time], fs ));
      AppendToStream(stream, Format(
            '<value value="%g" ' + sTrans + '/>',
            [item.Value], fs ));
      if item.Comment <> '' then
        AppendToStream(stream,
            '<comment>' +
              UTF8TextToXMLText(item.Comment) +
            '</comment>' );
      AppendToStream(stream,
          '</item>');
    end;

    AppendToStream(stream,
        '</raw-data>');
    AppendToStream(stream,
      '</measurement>');

    stream.Position := 0;
    stream.SaveToFile(AFileName);

    FFileName := AFileName;
  finally
    stream.Free;
  end;
end;


procedure TDataList.SetItem(AIndex: Integer; AValue: TDataItem);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TDataList.SetTimeUnits(AValue: TTimeUnits);
begin
  ConvertTimeUnits(AValue);
end;

procedure TDataList.SetTransExpression(const AValue: String);
begin
  FParser.Expression := AValue;
end;

function TDataList.Transform(AValue: Double; out AFlag: TTransformFlag): Double;
var
  res : TFPExpressionResult;
begin
  AFlag := tfOK;
  if IsEmptyNumber(AValue) then
    result := AValue
  else
  if (TransExpression = '') then
    result := AValue
  else
  begin
    if not IsEmptyNumber(FTransMaxIn) and (AValue > FTransMaxIn) then begin
      AFlag := tfTooLargeIn;
      AValue := FTransMaxIn;
    end
    else
    if not IsEmptyNumber(FTransMinIn) and (AValue < FTransMinIn) then begin
      AFlag := tfTooSmallIn;
      AValue := FTransMinIn;
    end;
    try
      FParserVariable.AsFloat := AValue;
      res := FParser.Evaluate;
      result := res.ResFloat;
      if not IsEmptyNumber(FTransMaxOut) and (Result > FTransMaxOut) then begin
        Result := FTransMaxOut;
        AFlag := tfTooLargeOut;
      end else
      if not IsEmptyNumber(FTransMinOut) and (Result < FTransMinOut) then begin
        Result := FTransMinOut;
        AFlag := tfTooSmallOut;
      end;
    except
      Result := NaN;
      AFlag := tfError;
    end;
  end;
end;

end.


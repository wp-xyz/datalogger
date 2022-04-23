unit dlData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, laz2_xmlread, laz2_DOM, fpexprpars,
  fpstypes, fpspreadsheet, {%H-}fpsallformats,
  TACustomSeries,
  dlGlobal, dlTransformation;

type
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
    FTransformation: TTransformation;
    FMeasDate: TDateTime;
    function GetCaption(ATransformedData: Boolean): String;
    function GetItem(AIndex: Integer): TDataItem;
    function GetTransQuantName: String;
    function GetTransUnits: String;
    procedure SetItem(AIndex: Integer; AValue: TDataitem);
    procedure SetTimeUnits(AValue: TTimeUnits);

  protected
    procedure ConvertTimeUnits(ANewUnits: TTimeUnits);

  public
    constructor Create;
    function AddValue(ATime, AValue: Double; const AComment: String = ''): TDataItem;
    function CanSetLogarithmic: boolean;
    procedure Clear;
//    procedure CopyTransformationFrom(AData: TDataList);
    function HasComments: Boolean;
    function HasTransformation: Boolean;
    procedure LoadFromTextFile(const AFileName: String);
    procedure LoadFromXMLFile(const AFileName: String);
    procedure SaveAsSpreadsheetFile(const AFileName: String;
      AFormat: TsSpreadsheetFormat; ACommentLinesOnly: Boolean = false);
    procedure SaveAsTextFile(const AFileName: String; ACommentLinesOnly: Boolean = false);
    procedure SaveAsXMLFile(const AFileName: String);
    procedure SelectTransformation(ATransformationName: String);
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
    property Transformation: TTransformation read FTransformation;
    property TransQuantName: String read GetTransQuantName;
    property TransUnits: String read GetTransUnits;
  end;


implementation

uses
  Math, StrUtils,
  fpsUtils, fpsXMLCommon,
  dlUtils;


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


{ TDataList }

constructor TDataList.Create;
begin
  inherited Create;
  FRawMultiplier := 1.0;
  FTimeOffset := 0.0;
  Clear;
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
  i: Integer;
begin
  if ANewUnits = FTimeUnits then
    exit;
  for i:=0 to Count-1 do
    Items[i].Time := dlUtils.ConvertTimeUnits(Items[i].Time, FTimeUnits, ANewUnits);
  FTimeUnits := ANewUnits;
end;

function TDataList.GetCaption(ATransformedData: Boolean): String;
begin
  if ATransformedData and (FTransformation <> nil) then
  begin
    Result := FTransformation.MeasName;
    if FTransformation.MeasUnits <> '' then
      Result := Result + ', ' + FTransformation.MeasUnits;
  end else
  begin
    Result := FRawQuantName;
    if FRawUnits <> '' then
      Result := Result + ', ' + FRawUnits;
  end;
end;

function TDataList.GetItem(AIndex: Integer): TDataItem;
begin
  result := TDataItem(inherited Items[AIndex]);
end;

function TDataList.GetTransQuantName: String;
begin
  if HasTransformation then
    Result := FTransformation.MeasName
  else
    Result := '';
end;

function TDataList.GetTransUnits: String;
begin
  if HasTransformation then
    Result := FTransformation.MeasUnits
  else
    Result := '';
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
  Result := (FTransformation <> nil) and (FTransformation.Expression <> '');
end;

{ Reads the raw data in a saved text file. Transformed data, if available,
  will be ignored - they will be recalculated by the application. }
procedure TDataList.LoadFromTextFile(const AFileName: String);
var
  F: TextFile;
  s: string;
  p: integer;
  x, y: double;
  L: TStringList;
  ok: Boolean;
  hasComment: Boolean = false;
  commentCol: Integer = -1;
begin
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
        ok := TryStrToFloat(L[0], x, UniversalFormatSettings) and 
              TryStrToFloat(L[1], y, UniversalFormatSettings);
        if ok then begin
          if hasComment then
            AddValue(x, y, L[commentCol])
          else
            AddValue(x, y);
        end;
      end else
      if Lowercase(LeftStr(s, 5)) = '#time' then begin
        L.DelimitedText := s;
        if L.Count < 2 then
          raise Exception.Create('Not enough columns');
        
        hasComment := L.Count > 2;
        commentCol := L.Count-1;

        s := L[1];
        p := pos(',', s);
        if p > 0 then
        begin
          FRawQuantName := trim(Copy(s, 1, p-1));
          FRawUnits := trim(Copy(s, p+1, MaxInt));
        end else
        begin
          FRawQuantName := s;
          FRawUnits := '';
        end;
      end;
    end;

    FFileName := AFileName;
    FTimeUnits := tuSeconds;
    
    SelectTransformation(MeasSettings.Transformation); 
  finally
    L.Free;
    CloseFile(F);
  end;
end;


{ Reads the raw data in a saved xml file. Transformed data, if available,
  will be ignored - they will be recalculated by the application. }
procedure TDataList.LoadFromXMLFile(const AFileName: String);
var
  measnode: TDOMNode;
  node: TDOMNode;
  itemnode, valuenode: TDOMNode;
  doc: TXMLDocument = nil;
  nodename: String;
  s, sComment: String;
  t, x: Double;
  tu: TTimeUnits;
begin
  FFileName := AFileName;
  FTimeUnits := tuSeconds;
  FMeasDate := UNDEFINED_DATE;
  FRawMultiplier := 1.0;

  try
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

      // Read values
      if (nodename = 'raw-data') or (nodename = 'values') then begin
        FRawQuantName := GetAttrValue(node, 'name');
        FRawUnits := GetAttrValue(node, 'units');
        s := GetAttrValue(node, 'multiplier');
        if (s <> '') and TryStrToFloat(s, x, UniversalFormatSettings) then
          FRawMultiplier := x;
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
                if (s = '') or not TryStrToFloat(s, t, UniversalFormatSettings) then
                  t := NaN;
              end;
              if (nodename = 'value') or (nodename = 'raw-value') then begin
                s := GetAttrValue(valuenode, 'value');
                if (s = '') or not TryStrToFloat(s, x, UniversalFormatSettings) then
                  x := NaN;
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

    SelectTransformation(MeasSettings.Transformation);

  finally
    doc.Free;
  end;
end;


{ Writes raw and transformed data (if available) to a spreadsheet file. 
  if ACommentLinesOnly is true only lines with comments will be written in order
  to facilitate finding comment lines. }
procedure TDataList.SaveAsSpreadsheetFile(const AFileName: String;
  AFormat: TsSpreadsheetFormat; ACommentLinesOnly: Boolean = false);
var
  i: Integer;
  r, c: Cardinal;
  workbook: TsWorkbook;
  worksheet: TsWorksheet;
  flag: TTransformFlag;
  cTime, cRaw, cTrans, cComment: LongInt;
  x, y, yt: Double;
begin
  cTime := 0;
  cRaw := 1;
  if HasTransformation then begin
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
    worksheet.WriteText(r, cTime, TIME_CAPTION[FTimeUnits]);
    worksheet.WriteText(r, cRaw, Caption[false]);
    if cTrans <> -1 then
      worksheet.WriteText(r, cTrans, Caption[true]);
    if cComment <> -1 then
      worksheet.WriteText(r, cComment, 'Comments');

    // Format header row in bold
    for c := 0 to worksheet.GetLastColIndex do
      worksheet.WriteFontStyle(r, c, [fssBold]);

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
        worksheet.WriteText(r, cComment, Items[i].Comment);
    end;

    // Save to file
    workbook.WriteToFile(AFileName, AFormat, true)
  finally
    workbook.Free;
  end;
end;


{ Writes raw and transformed data (if available) to a text file. 
  if ACommentLinesOnly is true only lines with comments will be written in order
  to facilitate finding comment lines. }
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
begin
  hasTrans := HasTransformation;
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
        sx := Format('%g', [x], UniversalFormatSettings);
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


{ Writes raw and transformed data (if available) to an xml file. 
  The transformation itself is not written.
  If ACommentLinesOnly is true only lines with comments will be written in order
  to facilitate finding comment lines. }
procedure TDataList.SaveAsXMLFile(const AFileName: String);
const
  LE = LineEnding;
var
  stream: TMemoryStream;
  i: Integer;
  item: TDataItem;
  sName, sUnits, sDate: String;
  valTrans: Double;
  hasTrans: Boolean;
  flags: TTransformFlag;
begin
  hasTrans := HasTransformation;
  
  stream := TMemoryStream.Create;
  try
    AppendToStream(stream,
      '<?xml version="1.0" encoding="UTF-8" ?>' + LE);

    AppendToStream(stream,
      '<measurement>' + LE);

    sName := IfThen(FRawQuantName = '', '', Format('name="%s" ', [FRawQuantName]));
    sUnits := IfThen(FRawUnits = '', '', Format('units="%s" ', [FRawUnits]));
    sDate :=  DateToStr(FMeasDate, UniversalFormatSettings);
    AppendToStream(stream, Format(
      '  <values %s %s multiplier="%g" count="%d" start-datetime="%s" time-units="%s">' + LE, [
        sName, sUnits, FRawMultiplier, Count, sDate, TIME_UNITS[FTimeUnits]
      ], UniversalFormatSettings));

    for i := 0 to Count-1 do begin
      item := Items[i];
      AppendToStream(stream,
        '    <item>' + LE);
      AppendToStream(stream, Format(
        '      <time value="%g" />' + LE, [item.Time], UniversalFormatSettings ));
      AppendToStream(stream, Format(
        '      <raw-value value="%g" />' + LE, [item.Value], UniversalFormatSettings));
      if hasTrans then
      begin
        valTrans := Transform(item.Value, flags);
        if flags = tfOK then
          AppendToStream(stream, Format(
            '      <transformed-value value="%g" />' + LE, [valTrans], UniversalFormatSettings ));
      end;
      if item.Comment <> '' then
        AppendToStream(stream,
          '      <comment>' + LE +
                   UTF8TextToXMLText(item.Comment) + LE +
          '      </comment>' + LE);
      AppendToStream(stream,
          '    </item>' + LE);
    end;

    AppendToStream(stream,
        '  </values>' + LE);
    AppendToStream(stream,
      '</measurement>');

    stream.Position := 0;
    stream.SaveToFile(AFileName);

    FFileName := AFileName;
  finally
    stream.Free;
  end;
end;


procedure TDataList.SelectTransformation(ATransformationName: String);
begin
  FTransformation := TransformationList.Find(ATransformationName);
end;


procedure TDataList.SetItem(AIndex: Integer; AValue: TDataItem);
begin
  inherited Items[AIndex] := AValue;
end;


procedure TDataList.SetTimeUnits(AValue: TTimeUnits);
begin
  ConvertTimeUnits(AValue);
end;


function TDataList.Transform(AValue: Double; out AFlag: TTransformFlag): Double;
begin
  if HasTransformation then
    Result := FTransformation.Transform(AValue, AFlag)
  else
    Result := NaN;
end;


end.


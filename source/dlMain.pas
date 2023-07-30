unit dlMain;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  // FCL + LCL
  Classes, SysUtils, contnrs, FileUtil, IniFiles, Graphics, Controls, Types,
  StdCtrls, ExtCtrls, ComCtrls, Forms, Dialogs, Menus, ActnList, StdActns,
  Grids, Buttons,
  // TAChart
  TAGraph, TACustomSeries, TASeries, TASources, TACustomSource,
  TAIntervalSources, TATypes, TAChartImageList, TATransformations,
  TAChartListbox, TALegend, TATools,
  // other
  synaser, MRUManager,
  // project
  dlGlobal, dlData, dlSerialDevice, dlLEDCtrl, dlTransformation;

type
  { TMainForm }

  TMainForm = class(TForm)
    AcSerialPort: TAction;
    AcDiagramRawData: TAction;
    AcStartStopMeas: TAction;
    AcDiagramTransformedData: TAction;
    AcAddComment: TAction;
    AcSaveSettings: TAction;
    AcTimeOffset: TAction;
    AcDiagramRemoveCurve: TAction;
    AcAutoSaveSettings: TAction;
    AcMeasurementSettings: TAction;
    AcCopyDiagramToClipboard: TAction;
    AcSeriesLinesOnly: TAction;
    AcSeriesSymbolsOnly: TAction;
    AcSeriesLinesAndSymbols: TAction;
    AcDiagramRemoveAll: TAction;
    AcDiagramLogarithmicY: TAction;
    AcApplyTransformation: TAction;
    AcDiagramFullGrids: TAction;
    AcTransformations: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    BitBtn1: TBitBtn;
    Chart: TChart;
    ChartAxisTransformations: TChartAxisTransformations;
    LogAxisTransform: TLogarithmAxisTransform;
    ChartImageList: TChartImageList;
    ChartListbox: TChartListbox;
    ChartToolset: TChartToolset;
    CbFiles: TComboBox;
    DataGrid: TDrawGrid;
    LogSource: TListChartSource;
    MDiagramSeparator1: TMenuItem;
    MnuDiagramTimeOffset: TMenuItem;
    MnuDiagramRemoveCurve: TMenuItem;
    MnuConfigAutoSave: TMenuItem;
    MnuConfigMeasSettings: TMenuItem;
    MnuDiagramRemoveAllCurves: TMenuItem;
    MnuDiagramSeparator2: TMenuItem;
    MnuDiagramCopyToClipboard: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem5: TMenuItem;
    MnuConfigSaveNow: TMenuItem;
    MnuDiagram: TMenuItem;
    MnuDiagramRawData: TMenuItem;
    MnuDiagramTransformedData: TMenuItem;
    BtnDataModeRaw: TToolButton;
    BtnDataModeTransf: TToolButton;
    LinesSymbolsPopupMenu: TPopupMenu;
    Panel1: TPanel;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    TbSeriesLinesAndSymbols: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ZoomMouseWheelTool: TZoomMouseWheelTool;
    PanDragTool: TPanDragTool;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    ZoomDragTool: TZoomDragTool;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    AcFileSaveAs: TFileSaveAs;
    AcFileOpen: TFileOpen;
    MeasSeries: TLineSeries;
    CheckBox1:TCheckBox;
    AcFileExit: TFileExit;
    CoolBar: TCoolBar;
    ImageList: TImageList;
    LEDInfoQuant: TLabel;
    LEDInfoBatt: TLabel;
    LEDInfoAuto: TLabel;
    MainMenu: TMainMenu;
    Memo:TMemo;
    MnuConfigTransformations: TMenuItem;
    MnuFileSaveAs: TMenuItem;
    MnuFileOpen: TMenuItem;
    MnuFileSeparator: TMenuItem;
    MnuFileReOpen: TMenuItem;
    MnuConfigSerialPort: TMenuItem;
    MnuConfiguration: TMenuItem;
    MnuFileExit: TMenuItem;
    MnuFile: TMenuItem;
    PageControl: TPageControl;
    PanelValueInfos: TPanel;
    PanelValue: TPanel;
    RecentFilesPopupMenu: TPopupMenu;
    ChartListboxSplitter: TSplitter;
    StatusBar:TStatusBar;
    PgDiagram: TTabSheet;
    PgTable: TTabSheet;
    PgCommunication: TTabSheet;
    MainToolBar: TToolBar;
    ToolButton1: TToolButton;
    BtnStartStopMeas: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    MeasChartSource: TUserDefinedChartSource;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure AcAddCommentExecute(Sender: TObject);
    procedure AcApplyTransformationExecute(Sender: TObject);
    procedure AcAutoSaveSettingsExecute(Sender: TObject);
    procedure AcCopyDiagramToClipboardExecute(Sender: TObject);
    procedure AcDiagramFullGridsExecute(Sender: TObject);
    procedure AcDiagramLogarithmicYExecute(Sender: TObject);
    procedure AcDiagramRawDataExecute(Sender: TObject);
    procedure AcDiagramRemoveAllExecute(Sender: TObject);
    procedure AcDiagramRemoveCurveExecute(Sender: TObject);
    procedure AcDiagramTransformedDataExecute(Sender: TObject);
    procedure AcFileOpenAccept(Sender: TObject);
    procedure AcFileSaveAsAccept(Sender: TObject);
    procedure AcMeasurementSettingsExecute(Sender: TObject);
    procedure AcSaveSettingsExecute(Sender: TObject);
    procedure AcSerialPortExecute(Sender: TObject);
    procedure AcSeriesLinesAndSymbolsExecute(Sender: TObject);
    procedure AcStartStopMeasExecute(Sender: TObject);
    procedure AcTimeOffsetExecute(Sender: TObject);
    procedure AcTransformationsExecute(Sender: TObject);
    procedure ApplicationPropertiesHint(Sender: TObject);
    procedure CbFilesSelect(Sender: TObject);
    procedure ChartListboxAddSeries({%H-}ASender: TChartListbox;
      ASeries: TCustomChartSeries; {%H-}AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxSeriesIconDblClick({%H-}ASender: TObject; AIndex: Integer);
    procedure ChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure CoolBarResize(Sender: TObject);
    procedure DataGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; {%H-}AState: TGridDrawState);
    procedure DataGridPrepareCanvas(sender: TObject; ACol, {%H-}ARow: Integer;
      {%H-}AState: TGridDrawState);
    procedure DataPointCrosshairToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender:TObject; var CanClose:boolean);
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
    procedure LinesSymbolsPopupMenuClose(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ZoomDragToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);

  private
    { Measurement }
    FMeasData : TDataList;
    FSerialDevice: TSerialDevice;
    FStartTime: TDateTime;
    FPrevTime: TDateTime;
    FRunning: Boolean;
    FMissing: Integer;
    FActivated: Boolean;
    function Connect: Boolean;
    procedure Disconnect;
    function GetMeasInterval(ATime, AValue: Double): Double;
    procedure OnDataHandler(Sender: TObject; Decoder: TSerialDecoder);
    procedure OnDisconnectHandler(Sender: TObject);
    procedure OnErrorHandler(Sender: TObject; APortStatus: Integer);
    procedure ProcessValue(ATime: TDateTime; AValue: Double);
    procedure SetSerialParams;
    procedure StartMeas;
    procedure StopMeas;

  private
    { Transformations }
    FShowTransformedValues: Boolean;
    function GetDataMode: TDataMode;
    procedure SelectTransformation(const AName: String);
    procedure SetDataMode(AValue: TDataMode);
    procedure UpdateDataMode;

  private
    { Display & Chart }
    FLEDDisplay : TLEDDisplay;
    procedure ApplySeriesSettings(ASeries: TLineseries; AIndex: Integer);
    procedure ApplyTimeOffsetHandler(Sender: TObject; {%H-}ASeries: TChartSeries);
    function CanSetLogarithmic: Boolean;
    procedure GetDataInfo(ASeries: TChartSeries; out AQuantName, AUnitName: String);
    procedure PopulateLogSource;
    procedure SaveChart(const AFileName: String);
    procedure SetFullGrids(AEnable: Boolean);
    procedure SetLedDisplayNumDigits(AValue: Integer);
    procedure SetLogarithmic(AValue: Boolean);
    procedure SetTimeUnits(AValue: TTimeUnits);
    procedure ShowChartListbox(Enable: Boolean);
    procedure UpdateChart;
    procedure UpdateChartBottomAxis;
    procedure UpdateChartSources;
    procedure UpdateChartTics;

  private
    { Grid }
    function GetCellText(ACol, ARow: Integer): String;
    procedure SetupGrid;

  private
    { Files & most recently used files }
    FDataList: TObjectList;
    MRUMenuManager: TMRUMenuManager;
    function GetDefaultExt(AFilterIndex: Integer): String;
    function GetDefaultFilterIndex(Ext: String): Integer;
    procedure MRUMenuManagerRecentFile(Sender: TObject; const AFileName: string);
    procedure OpenFile(const AFileName: String);
    procedure PopulateFilesCombo;

  private
    { Configuration }
    procedure ReadFromIni;
    procedure WriteToIni;

  private
    { Misc }
    procedure UpdateCmdStates;

  protected

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math,
  fpsTypes,
  TAChartUtils, TADrawerSVG, TADrawUtils, TADrawerCanvas, TAChartAxis,
  VC820Device, VC830Device,
  dlUtils, dlSerialPortSettings, dlTransformations,
  dlRemoveCurveDialog, dlTimeOffsetDialog, dlMeasSettingsDialog,
  dlSeriesStyleEditor;

resourcestring
  SMouseInfo = 'L-drag/MWheel = zoom, Shift-LDrag = pan, LClick = unzoom, RDrag = read values';

const
  X_AXIS_GROWTH: Array[TTimeUnits] of Double = (10, 5, 5, 1);

  COUNT_PANEL = 0;
  VALUE_PANEL = 1;
  MOUSE_PANEL = 2;
  HINT_PANEL  = MOUSE_PANEL;

  XML_FILTERINDEX  = 1;
  TXT_FILTERINDEX  = 2;
  CSV_FILTERINDEX  = 3;
  XLS_FILTERINDEX  = 4;
  XLSX_FILTERINDEX = 5;
  ODS_FILTERINDEX  = 6;
  BMP_FILTERINDEX  = 7;
  JPG_FILTERINDEX  = 8;
  PNG_FILTERINDEX  = 9;
  SVG_FILTERINDEX  = 10;

var
  NextPaletteIndex : integer = 1;


{ TMainForm }

procedure TMainForm.AcAddCommentExecute(Sender: TObject);
var
  currIdx: Integer;
  item: TDataItem;
  x, xt: Double;
  s: String;
  ttl: String;
  flag: TTransformFlag;
begin
  currIdx := FMeasData.Count - 1;
  item := FMeasData.Items[currIdx];

  x := item.Value;
  s := Format('%s = %.6g %s', [FMeasData.RawQuantName, x, FMeasData.RawUnits]);
  if GetDataMode = dmTransformed then begin
    xt := FMeasData.Transform(x, flag);
    if (flag = tfOK) and not IsNaN(xt) then
      s := Format('%s, %s = %.6g %s', [s, FMeasData.TransQuantName, xt, FMeasData.TransUnits]);
  end;
  ttl := Format('Enter comment on data point #%d: t = %.6g s, %s', [
    currIdx + 1, item.Time, s]
  );

  s := '';
  if InputQuery(ttl, 'Comment', s) then begin
    item.Comment := s;
    MeasSeries.Marks.Style := smsLabel;
    MeasSeries.Marks.Visible := true;
    Application.ProcessMessages;
  end;
end;


procedure TMainForm.AcApplyTransformationExecute(Sender: TObject);
var
  P: TDataList;
begin
  SelectTransformation(MeasSettings.Transformation);
  
  P := TDataList(CbFiles.Items.Objects[CbFiles.ItemIndex]);
  if P <> nil then begin
    SetupGrid;
    (P.Series.Source as TUserDefinedChartSource).Reset;
  end;
end;


procedure TMainForm.AcAutoSaveSettingsExecute(Sender: TObject);
begin
  // Nothing to do. We just need the Checked property which is set by AutoCheck...
end;


procedure TMainForm.AcCopyDiagramToClipboardExecute(Sender: TObject);
begin
  Chart.CopyToClipboardBitmap;
end;


procedure TMainForm.AcDiagramFullGridsExecute(Sender: TObject);
begin
  SetFullGrids(AcDiagramFullGrids.Checked);
end;


procedure TMainForm.AcDiagramLogarithmicYExecute(Sender: TObject);
begin
  SetLogarithmic(AcDiagramLogarithmicY.Checked);
end;


procedure TMainForm.AcDiagramRawDataExecute(Sender: TObject);
begin
  SetDataMode(dmRaw);
end;


procedure TMainForm.AcDiagramRemoveAllExecute(Sender: TObject);
var
  i : integer;
  ser : TChartSeries;
  data : TDataList;
begin
  // clear and free loaded data
  for i:=FDataList.Count-1 downto 1 do begin
    data := TDataList(FDataList.Items[i]);
    ser := data.Series as TChartSeries;
    if ser.Source <> nil then ser.Source.Free;
    Chart.DeleteSeries(ser);
    ser.Free;
    FDataList.Delete(i);
  end;
  // clear currently measured data
  FMeasData.Clear;
  MeasChartSource.PointsNumber := 0;
  // Update gui
  ShowChartListbox(false);
  NextPaletteIndex := 1;
  UpdateCmdStates;
end;


procedure TMainForm.AcDiagramRemoveCurveExecute(Sender: TObject);
var
  Dlg : TRemoveCurvesForm;
  i,j : integer;
  item : TDataList;
  ser : TChartSeries;
  source: TCustomChartSource = nil;

  procedure AddToDlg(AItem:TDataList);
  begin
    if (AItem = FMeasData) and (AItem.Count = 0) then
      exit;
    with Dlg.Listview.Items.Add do begin
      if AItem = FMeasData then
        Caption := 'Measurement'
      else
        Caption := AItem.FileName;
      Data := AItem;
      ImageIndex := AItem.Series.Index;
    end;
  end;

begin
  if FDataList.Count = 0 then exit;

  Dlg := TRemoveCurvesForm.Create(nil);
  try
    ChartImageList.Chart := Chart;
    Dlg.ImageList.Assign(ChartImageList);
//    j := IfThen(MeasSeries.Count = 0, 1, 0);
    for i:=0 to FDataList.Count-1 do
      AddToDlg(TDataList(FDataList.Items[i]));

    Dlg.Caption := 'Remove curves';
    Dlg.Position := poMainFormCenter;
    Dlg.ListView.Selected := Dlg.ListView.Items[0];
    Dlg.Listview.MultiSelect := true;
    Dlg.CbReassignLineColors.Checked := false;
    if Dlg.ShowModal = mrOK then begin
      for i:=Dlg.ListView.Items.Count-1 downto 0 do begin
        if Dlg.Listview.Items[i].Selected then begin
          item := TDataList(Dlg.Listview.Items[i].Data);
          if item = FMeasData then begin
            FMeasData.Clear;
            MeasChartSource.PointsNumber := 0;
          end else
          if item <> nil then begin
            j := FDataList.IndexOf(item);
            ser := item.Series;
            if (ser.Source <> nil) then
              source.Free;
            Chart.DeleteSeries(ser);
            ser.Free;
            FDataList.Delete(j);
          end;
        end;
      end;
      if Dlg.CbReassignLineColors.Checked then
        for i:=1 to Chart.SeriesCount-1 do begin
          ser := TChartSeries(Chart.Series[i]);
          if ser is TLineSeries then
            with TLineSeries(ser) do begin
              LinePen.Color := DiagramSettings.SeriesSettings[i mod DEFAULT_SERIES_COUNT].LineColor;
              Pointer.Pen.Color := DiagramSettings.SeriesSettings[i mod DEFAULT_SERIES_COUNT].LineColor;
              Pointer.Brush.Color := DiagramSettings.SeriesSettings[i mod DEFAULT_SERIES_COUNT].LineColor;
            end;
        end;
      ShowChartListbox(FDataList.Count > 1);
      if FDataList.Count = 1 then NextPaletteIndex := 1;
      Chart.Invalidate;
      UpdateChartTics;
      UpdateCmdStates;
    end;

  finally
    Dlg.Free;
    ChartImageList.Chart := nil;
  end;
end;


procedure TMainForm.AcDiagramTransformedDataExecute(Sender: TObject);
begin
  SetDataMode(dmTransformed);
end;


procedure TMainForm.AcFileOpenAccept(Sender: TObject);
begin
  OpenFile(AcFileOpen.Dialog.FileName);
end;


procedure TMainForm.AcFileSaveAsAccept(Sender: TObject);
var
  data: TDataList;
begin
  if PageControl.ActivePage = PgDiagram then
    data := FMeasData
  else
  if PageControl.ActivePage = PgTable then
    data := TDataList(CbFiles.Items.Objects[CbFiles.ItemIndex])
  else
    exit;

  if (data = nil) or (data.Count = 0) then
  begin
    MessageDlg('No data to save.', mtInformation, [mbOK], 0);
    exit;
  end;
  
  case AcFileSaveAs.Dialog.FilterIndex of
    XML_FILTERINDEX:
      data.SaveAsXMLFile(AcFileSaveAs.Dialog.FileName);
    TXT_FILTERINDEX, CSV_FILTERINDEX:
      data.SaveAsTextFile(AcFileSaveAs.Dialog.FileName);
    XLS_FILTERINDEX:
      data.SaveAsSpreadsheetFile(AcFileSaveAs.Dialog.FileName, sfExcel8);
    XLSX_FILTERINDEX:
      data.SaveAsSpreadsheetFile(AcFileSaveAs.Dialog.FileName, sfOOXML);
    ODS_FILTERINDEX:
      data.SaveAsSpreadsheetFile(AcFileSaveAs.Dialog.FileName, sfOpenDocument);
    {
    CSV_FILTERINDEX:
      data.SaveAsSpreadsheetFile(AcFileSaveAs.Dialog.FileName, sfCSV);
    }
    BMP_FILTERINDEX, JPG_FILTERINDEX, PNG_FILTERINDEX, SVG_FILTERINDEX:
      if PageControl.ActivePage = PgDiagram then
        SaveChart(AcFileSaveAs.Dialog.FileName);
  end;
end;


procedure TMainForm.AcMeasurementSettingsExecute(Sender: TObject);
var
  F: TMeasSettingsForm;
begin
  F := TMeasSettingsForm.Create(nil);
  try
    if F.ShowModal = mrOK then
      SelectTransformation(MeasSettings.Transformation);
  finally
    F.Free;
  end;
end;


procedure TMainForm.AcSaveSettingsExecute(Sender: TObject);
begin
  WriteToIni;
end;


procedure TMainForm.AcSerialPortExecute(Sender: TObject);
var
  F : TSerPortForm;
begin
  if GetSerialPortNames = '' then begin
    MessageDlg(
      'This PC is not equipped with a serial port. '+
      'Sorry, this program cannot be executed.',
      mtError, [mbOK], 0);
    exit;
  end;

  F := TSerPortForm.Create(nil);
  try
    F.Port := DeviceSettings.Port;
    F.DeviceName := DeviceSettings.Name;
    F.Readout := DeviceSettings.Readout;
    F.Digits := DeviceSettings.Digits;
    if SameText(DeviceSettings.Name, 'user-defined') then begin
      F.Baudrate := DeviceSettings.BaudRate;
      F.Databits := DeviceSettings.Databits;
      F.StopBits := DeviceSettings.StopBits;
      F.Parity := DeviceSettings.Parity;
      F.Handshake := DeviceSettings.Handshake;
    end;
    if F.ShowModal = mrOK then begin
      DeviceSettings.Port := F.Port;
      DeviceSettings.Name := F.DeviceName;
      DeviceSettings.BaudRate := F.BaudRate;
      DeviceSettings.Databits := F.Databits;
      DeviceSettings.StopBits := F.StopBits;
      DeviceSettings.Parity := F.Parity;
      DeviceSettings.Handshake := F.Handshake;
      DeviceSettings.Readout := F.Readout;
      DeviceSettings.Digits := F.Digits;
      SetLedDisplayNumDigits(F.Digits);
    end;
  finally
    F.Free;
  end;
end;


procedure TMainForm.AcSeriesLinesAndSymbolsExecute(Sender: TObject);
var
  i: Integer;
begin               
  if AcSeriesLinesOnly.Checked then
    DiagramSettings.SeriesSettings[0].Style := ssLines
  else
  if AcSeriesSymbolsOnly.Checked then
    DiagramSettings.SeriesSettings[0].Style := ssSymbols
  else
  if AcSeriesLinesAndSymbols.Checked then
    DiagramSettings.SeriesSettings[0].Style := ssBoth;

  with MeasSeries do begin
    ShowLines := DiagramSettings.SeriesSettings[0].Style in [ssLines, ssBoth];
    ShowPoints := DiagramSettings.SeriesSettings[0].Style in [ssSymbols, ssBoth];
  end;               
end;


procedure TMainForm.AcStartStopMeasExecute(Sender: TObject);
begin
  if FRunning then
    StopMeas
  else
    StartMeas;
end;


procedure TMainForm.AcTimeOffsetExecute(Sender:TObject);
var
  Dlg: TTimeOffsetForm;
  i: integer;
  oldoffs: array of double = nil;

  procedure AddToDlg(AItem:TDataList);
  begin
    if (AItem = FMeasData) and (AItem.Count = 0) then
      exit;
    with Dlg.Listview.Items.Add do begin
      if AItem = FMeasData then
        Caption := 'Measurement'
      else
        Caption := AItem.FileName;
      Data := AItem;
      SubItems.Add(Format('%.2f', [AItem.TimeOffset]));
      ImageIndex := AItem.Series.Index;
    end;
  end;

begin
  Dlg := TTimeOffsetForm.Create(nil);
  try
    ChartImageList.Chart := Chart;
    Dlg.ImageList.Assign(ChartImageList);
    Dlg.Caption := 'Time offset';
    Dlg.Position := poMainFormCenter;
    Dlg.OnApplyOffsetToSeries := @ApplyTimeOffsetHandler;
    SetLength(oldoffs, FDataList.Count);
    for i:=0 to FDataList.Count-1 do begin
      oldoffs[i] := TDataList(FDataList.Items[i]).TimeOffset;
      AddToDlg(TDataList(FDataList.Items[i]));
    end;
    if Dlg.ListView.Items.Count = 0 then
      exit;
    Dlg.ListView.Selected := Dlg.ListView.Items[0];
    if Dlg.ShowModal <> mrOK then begin     // aborted --> restore old offsets
      for i:=0 to FDataList.Count-1 do
        TDataList(FDataList.Items[i]).TimeOffset := oldoffs[i];
      Chart.Invalidate;
    end;
  finally
    Dlg.Free;
    ChartImageList.Chart := nil;
  end;
end;


procedure TMainForm.AcTransformationsExecute(Sender : TObject);
var
  F : TTransformationForm;
begin
  F := TTransformationForm.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;


procedure TMainForm.ApplicationPropertiesHint(Sender: TObject);
begin
  Statusbar.Panels[HINT_PANEL].Text := Application.Hint;
end;


procedure TMainForm.CbFilesSelect(Sender: TObject);
begin
  SetupGrid;
end;


procedure TMainForm.ApplySeriesSettings(ASeries: TLineSeries; AIndex: Integer);
begin
  AIndex := AIndex mod DEFAULT_SERIES_COUNT;
  with ASeries do begin
    ShowLines := DiagramSettings.SeriesSettings[AIndex].Style in [ssLines, ssBoth];
    ShowPoints := DiagramSettings.SeriesSettings[AIndex].Style in [ssSymbols, ssBoth];
    LinePen.Color := DiagramSettings.SeriesSettings[AIndex].LineColor;
    LinePen.Style := DiagramSettings.SeriesSettings[AIndex].LineStyle;
    LinePen.Width := DiagramSettings.SeriesSettings[AIndex].LineWidth;
    Pointer.Style := DiagramSettings.SeriesSettings[AIndex].Symbol;
    Pointer.Brush.Color := DiagramSettings.SeriesSettings[AIndex].SymbolColor;
    Pointer.Pen.Color := DiagramSettings.SeriesSettings[AIndex].SymbolColor;
  end;
end;


procedure TMainForm.ApplyTimeOffsetHandler(Sender:TObject; ASeries:TChartSeries);
begin
  UpdateChartTics;
  UpdateCmdStates;
//  Chart.Invalidate;
end;


function TMainForm.CanSetLogarithmic : boolean;
var
  j : integer;
  data : TDataList;
begin
  result := true;
  for j:=0 to FDataList.Count-1 do begin
    data := TDataList(FDataList[j]);
    result := data.CanSetLogarithmic;
    if result = false then
      exit;
  end;
end;


procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
begin
  if (ASeries = MeasSeries) and (FMeasData <> nil) then begin
    if ((FMeasData.Count = 0)) and not (FRunning and (Chart.SeriesCount > 1))
//       (FDataList <> nil) and (FDataList.Count = 1)
    then
      ASkip := true;
  end;
end;


procedure TMainForm.ChartListboxSeriesIconDblClick(ASender: TObject;
  AIndex: Integer);
var
  F: TSeriesStyleEditor;
  series: TLineSeries;
begin
  F := TSeriesStyleEditor.Create(nil);
  try
    series := ChartListbox.Series[AIndex] as TLineSeries;
    F.ShowLines := series.ShowLines;
    F.ShowSymbols := series.ShowPoints;
    F.LineColor := series.LinePen.Color;
    F.LineStyle := series.LinePen.Style;
    F.LineWidth := series.LinePen.Width;
    F.Symbol := series.Pointer.Style;
    F.SymbolColor := series.Pointer.Brush.Color;
    F.SymbolBorderColor := series.Pointer.Pen.Color;
    if F.ShowModal = mrOK then begin
      series.ShowPoints := F.ShowSymbols;
      series.ShowLines := F.ShowLines;
      series.LinePen.Color := F.LineColor;
      series.LinePen.Style := F.Linestyle;
      series.LinePen.Width := F.LineWidth;
      series.Pointer.Style := F.Symbol;
      series.Pointer.Brush.Color := F.SymbolColor;
      series.Pointer.Pen.Color := F.SymbolBorderColor;
    end;
  finally
    F.Free;
  end;
end;


function TMainForm.Connect : boolean;
const
  TIMEOUT = 1000;
begin
  Result := false;

  if pos(DeviceSettings.Port, GetSerialPortNames) = 0 then begin
    MessageDlg(
      Format('Port %s does not exist on this PC.', [DeviceSettings.Port]),
      mtError, [mbOK], 0
    );
    exit;
  end;

  Memo.Lines.Add('Connecting...');

  FreeAndNil(FSerialDevice);
  case DeviceSettings.Readout of
    roUnknown:
      begin
        Memo.Lines.Add('ERROR: Unknown device readout method.');
        MessageDlg('Unknown device readout method.', mtError, [mbOK], 0);
        exit;
      end;
    roSegments:
      FSerialDevice := TSerialDevice.Create(TVC820Thread, TVC820Decoder);
    roAsciiDigits:
      FSerialDevice := TSerialDevice.Create(TVC830Thread, TVC830Decoder);
  end;
  FSerialDevice.Connect(
    DeviceSettings.Port,
    DeviceSettings.Baudrate,
    DeviceSettings.Databits,
    DeviceSettings.Parity,
    GetStopBits(DeviceSettings.StopBits),
    DeviceSettings.Handshake = hSoftware,
    DeviceSettings.Handshake = hHardware,
    TIMEOUT
  );
  FSerialDevice.OnData := @OnDataHandler;
  FSerialDevice.OnError := @OnErrorHandler;
  FSerialDevice.OnDisconnect := @OnDisconnectHandler;

  Result := FSerialDevice.Connected;
  if Result then
  begin
    with Memo.Lines do begin
      Add('Connected');
      Add('Serial port parameters:');
      Add(Format('  Port: %s', [DeviceSettings.Port]));
      Add(Format('  Baudrate: %d', [DeviceSettings.Baudrate]));
      Add(Format('  Parity: %s', [''+DeviceSettings.Parity]));
      Add(Format('  Data bits: %d', [DeviceSettings.Databits]));
      Add(Format('  Stop bits: %s', [FloatToStr(Devicesettings.Stopbits)]));
      Add(Format('  Handshake: %s', [GetHandshakeString(DeviceSettings.Handshake)]));
      Add('');
    end;
  end else
  begin
    Memo.Lines.Add('ERROR: Could not connect to serial port.');
    MessageDlg('Could not connect to serial port.', mtError, [mbOK], 0);
  end;
end;


procedure TMainForm.ChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  data: TDataList;
  flag: TTransformFlag;
begin
  if ASource = nil then begin
    AItem.X := NaN;
    AItem.Y := NaN;
    exit;
  end;

  data := TDataList(ASource.Tag);
  AItem.X := data.Items[AIndex].Time + data.TimeOffset;
  if DiagramSettings.TimeDisplay = tdDateTime then
    AItem.X := ConvertToDateTime(AItem.X, MeasSettings.TimeUnits);
  AItem.Y := data.Items[AIndex].Value;
  if FShowTransformedValues then
    AItem.Y := data.Transform(AItem.Y, flag);
  AItem.Text := data.Items[AIndex].Comment;
end;


procedure TMainForm.DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: String;
  grid: TDrawGrid;
begin
  s := GetCellText(ACol, ARow);
  if s = '' then
    exit;

  grid := TDrawGrid(Sender);
  grid.Canvas.TextRect(ARect, ARect.Left+constCellpadding, ARect.Top+constCellpadding, s);
end;


procedure TMainForm.DataPointCrosshairToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
begin
  Unused(ATool);
  Unused(APoint);
  DataPointCrosshairTool.Hide;
  Statusbar.Panels[MOUSE_PANEL].Text := SMouseInfo;
end;


procedure TMainForm.DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
var
  x,y : double;
  ttl, txt : string;
  ser : TChartSeries;
  quantName, unitStr: String;
begin
  if (ASender <> nil) and (ASender.Series is TChartSeries) then begin
    ser := (ASender.Series as TChartSeries);
    x := ser.Source.Item[ASender.PointIndex]^.X;
    y := ser.Source.Item[ASender.PointIndex]^.Y;
    txt := ser.Source.Item[ASender.PointIndex]^.Text;
    ttl := ser.Title;
    GetDataInfo(ser, quantName, unitStr);
    if txt <> '' then
      Statusbar.Panels[MOUSE_PANEL].Text := Format(
        '"%s": Time=%.6g s, %s=%.6g %s, Comment: %s',
        [ttl, x, quantName, y, unitStr, txt]
      )
    else
      Statusbar.Panels[MOUSE_PANEL].Text := Format(
        '"%s": Time=%.6g s, %s=%.6g %s',
        [ttl, x, quantName, y, unitStr]
      );
    //Statusbar.Refresh;
  end;
end;

procedure TMainForm.DataGridPrepareCanvas(sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
var
  ts: TTextStyle;
  grid: TDrawGrid;
begin
  grid := TDrawGrid(Sender);
  ts := grid.Canvas.TextStyle;
  if ACol = grid.ColCount-1
    then ts.Alignment := taLeftJustify
    else ts.Alignment := taCenter;
  grid.Canvas.TextStyle := ts;
end;


procedure TMainForm.CoolBarResize(Sender: TObject);
begin
  SetLedDisplayNumDigits(DeviceSettings.Digits);
end;


procedure TMainForm.Disconnect;
begin
  if (FSerialDevice <> nil) and FSerialDevice.Connected then
  begin
    Memo.Lines.Add('Disconnecting...');
    FSerialDevice.Disconnect;
    Memo.Lines.Add('Disconnected');
  end;
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    ReadFromIni;
    SetLedDisplayNumDigits(DeviceSettings.Digits);
  end;
end;

procedure TMainForm.FormCloseQuery(Sender:TObject; var CanClose:boolean);
var
  res: Integer;
begin
  res := MessageDlg('Do your really want to close this program?',
    mtConfirmation, [mbYes, mbNo], 0);
  CanClose := (res = mrYes);

  if CanClose then
  begin
    if (FSerialDevice <> nil) and FSerialDevice.Connected then 
    begin
      FSerialDevice.Disconnect;
      repeat
        Application.ProcessMessages;
        Sleep(100);
      until not FSerialDevice.Connected;
    end;
    if AcAutoSaveSettings.Checked then
      WriteToIni;
  end;
end;

procedure TMainForm.FormCreate(Sender:TObject);
begin
  FRunning := false;

  TransformationList := TTransformationList.Create;
  
  FMeasData := TDataList.Create;
  FMeasData.Series := MeasSeries;
  FMeasData.TimeOffset := 0;
  MeasSeries.Tag := PtrInt(FMeasData);
  ApplySeriesSettings(MeasSeries, 0);
  MeasChartSource.Tag := PtrInt(FMeasData);

  FDataList := TObjectList.Create;
  FDataList.Add(FMeasData);

  PopulateFilesCombo;
  CbFiles.ItemIndex := 0;

  PopulateLogSource;
  Chart.Hint := '|' + SMouseInfo;

  DataGrid.ColWidths[1] := 100;
  DataGrid.ColWidths[2] := 100;
  DataGrid.ColWidths[3] := 100;

  FLEDDisplay := TLEDDisplay.Create(self);
  with FLEDDisplay do begin
    Parent := PanelValue;
    Align := alClient;
    NumDigits := 5;  // 4 für Ziffern + 1 für Vorzeichen
    LeadingZeros := false;
    LEDContrast := 7;
    DigitLineWidth := 4;
    DigitHeight := Height;
    DigitWidth := Height * 2 div 3 - 1;
    Borderstyle := bsNone;
    BevelStyle := bvNone;
    Angle := 8;
    Clear;
    LEDInfoQuant.Font.Color := SegmentOffColor;
    LEDInfoAuto.Font.Color := SegmentOffColor;
    LEDInfoBatt.Font.Color := SegmentOffColor;
  end;

  MRUMenuManager := TMRUMenuManager.Create(self);
  with MRUMenuManager do begin
    Name := 'MRUMenuManager';
    IniFileName := GetAppConfigFile(false);
    IniSection := 'RecentFiles';
    MaxRecent := 16;
    MenuCaptionMask := '&%x - %s';    // & --> create hotkey
    MenuItem := MnuFileReopen;
    PopupMenu := RecentFilesPopupMenu;
    OnRecentFile := @MRUMenuManagerRecentFile;
  end;

//  Coolbar.Bands[2].Text := 'Transformations';
  Coolbar.AutosizeBands;
  
  UpdateCmdStates;
end;


procedure TMainForm.FormDestroy(Sender:TObject);
begin
  FreeAndNil(FSerialDevice);
  FreeAndNil(FDataList);  // destroying also FMeasData
  FreeAndNil(TransformationList);
end;


function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
  P: TDataList;
  item: TDataItem;
  yt: Double;
  flag: TTransformFlag;
begin
  Result := '';
  
  if CbFiles.ItemIndex = -1 then
    exit;
  P := TDataList(CbFiles.Items.Objects[CbFiles.ItemIndex]);
  if (P = nil) or (P.Count = 0) then
    exit;

  if ARow = 0 then
    case ACol of
      1: Result := TIME_CAPTION[P.TimeUnits];
      2: Result := P.Caption[false];
      3: if P.HasTransformation and (GetDataMode = dmTransformed)
           then Result := P.Caption[true]
           else Result :='Comments';
      4: Result := 'Comments';
    end
  else begin
    item := P.Items[ARow-1];
    case ACol of
      1: if DiagramSettings.TimeDisplay = tdDateTime then
           Result := DateToStr(ConvertToDateTime(item.Time, MeasSettings.TimeUnits))
         else
           Result := Format('%.3f', [item.Time]);
      2: Result := Format('%.3f', [item.Value]);
      3: if P.HasTransformation and (GetDataMode = dmTransformed) then begin
           yt := P.Transform(item.Value, flag);
           case flag of
             tfOK          : Result := Format('%.3f',  [yt]);
             tfTooLargeOut : Result := Format('>%.3f', [yt]);
             tfTooSmallOut : Result := Format('<%.3f', [yt]);
             tfError       : Result := 'Error';
           end;
         end else
           Result := item.Comment;
      4: Result := item.Comment;
    end;
  end;
end;


procedure TMainForm.GetDataInfo(ASeries: TChartSeries;
  out AQuantName, AUnitName: String);
var
  source: TUserDefinedChartSource;
  data: TDataList;
begin
  AQuantName := '';
  AUnitName := '';
  if ASeries = nil then
    exit;
  if not (ASeries.Source is TUserDefinedChartSource) then
    exit;
  source := TUserDefinedChartSource(ASeries.Source);
  data := TDataList(source.Tag);
  if data = nil then
    exit;
  AQuantName := data.TransQuantName;
  if AQuantName = '' then AQuantName := data.RawQuantName;
  AUnitName := data.TransUnits;
  if AUnitName = '' then AUnitName := data.RawUnits;
end;


function TMainForm.GetDataMode : TDataMode;
begin
  if (AcDiagramTransformedData.Checked) and (MeasSettings.Transformation <> '') then
    result := dmTransformed
  else
    result := dmRaw;
end;


function TMainForm.GetDefaultExt(AFilterIndex: Integer) : string;
begin
  case AFilterIndex of
    XML_FILTERINDEX : Result := '.xml';
    TXT_FILTERINDEX : Result := '.txt';
    CSV_FILTERINDEX : Result := '.csv';
    XLS_FILTERINDEX : Result := '.xls';
    XLSX_FILTERINDEX: Result := '.xlsx';
    ODS_FILTERINDEX : Result := '.ods';
    BMP_FILTERINDEX : Result := '.bmp';
    JPG_FILTERINDEX : Result := '.jpg';
    PNG_FILTERINDEX : Result := '.png';
    SVG_FILTERINDEX : Result := '.svg';
    else raise Exception.Create('GetDefaultExt: Invalid FilterIndex');
  end;
end;


function TMainForm.GetDefaultFilterIndex(Ext: string): Integer;
begin
  if (Ext = '') then
    Result := XML_FILTERINDEX
  else begin
    if (Ext[1] <> '.') then Ext := '.' + Ext;
    case Lowercase(Ext) of
      '.xml' : Result := XML_FILTERINDEX;
      '.txt' : Result := TXT_FILTERINDEX;
      '.csv' : Result := CSV_FILTERINDEX;
      '.xls' : Result := XLS_FILTERINDEX;
      '.xlsx': Result := XLSX_FILTERINDEX;
      '.ods' : Result := ODS_FILTERINDEX;
      '.bmp' : Result := BMP_FILTERINDEX;
      '.jpg',
      '.jpeg': Result := JPG_FILTERINDEX;
      '.png' : Result := PNG_FILTERINDEX;
      '.svg' : Result := SVG_FILTERINDEX;
      else     Raise Exception.CreateFmt('Unknown file extension "%s".', [Ext]);
    end;
  end;
end;


function TMainForm.GetMeasInterval(ATime, AValue: Double): Double;
var
  t: TDateTime;
  i: Integer;
begin
  Result := 0;
  with MeasSettings do begin
    if IntervalSettings[0].Active then
      Result := IntervalSettings[0].Interval;
    for i:=1 to 2 do
      if IntervalSettings[i].Active then
        case IntervalSettings[i].Condition of
          micTimeAfter:
            begin
              t := ConvertTimeUnits(
                IntervalSettings[i].ConditionLimit,
                IntervalSettings[i].ConditionLimitUnits,
                TimeUnits
              );
              if ATime > t then
                Result := IntervalSettings[i].Interval;
            end;
          micValueAbove:
            if not IsEmptyNumber(IntervalSettings[i].ConditionLimit) then
              if (AValue > IntervalSettings[i].ConditionLimit) then begin
                Result := IntervalSettings[i].Interval;
                exit;
              end;
          micValueBelow:
            if not IsEmptyNumber(IntervalSettings[i].ConditionLimit) then
              if (AValue < IntervalSettings[i].ConditionLimit) then begin
                Result := IntervalSettings[i].Interval;
                exit;
              end;
        end;
  end;
end;


procedure TMainForm.LinesSymbolsPopupMenuClose(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to LinesSymbolsPopupMenu.Items.Count-1 do
    if LinesSymbolsPopupMenu.Items[i].Checked then begin
      TbSeriesLinesAndSymbols.Action := LinesSymbolsPopupMenu.Items[i].Action;
      exit;
    end;
end;


procedure TMainForm.MRUMenuManagerRecentFile(Sender: TObject;
  const AFileName: string);
begin
  OpenFile(AFileName);
end;


procedure TMainForm.OnDataHandler(Sender: TObject; Decoder: TSerialDecoder);
var
  t, x: TDateTime;
  txt: string;
  value: String;
  flagstr: String;

  procedure AddDisplayFlag(AFlag:TDisplayFlag; var AResult: String);
  var
    s: String;
  begin
    s := Decoder.DisplayFlagAsText(AFlag);
    if s <> '' then
    begin
      if AResult = '' then
        AResult := s
      else
        AResult := AResult + ' ' + s;
    end;
  end;

begin
  if not FRunning then
    exit;
  
  if Decoder.Status = sdrOK then begin
    t := now();
    if (t - FPrevTime < MeasSettings.Interval/SECONDS_PER_DAY) then
      exit;
    x := Decoder.Value[true];  // raw value
    ProcessValue(t, x);

    // Update LED display
    FLEDDisplay.FractionDigits := Decoder.DisplayedDecimals;
    FLEDDisplay.Value := x;

    txt := Decoder.Units[true];
    if Decoder.Quantity in [qVoltage, qCurrent] then
    begin
      if Decoder.DisplayFlagIsSet(dfAC) then
        txt := txt + '/ ' + Decoder.DisplayFlagAsText(dfAC);
      if Decoder.DisplayFlagIsSet(dfDC) then
        txt := txt + '/ ' + Decoder.DisplayFlagAsText(dfDC);
    end;
    LEDInfoQuant.Caption := txt;
    LEDInfoQuant.Font.Color := FLEDDisplay.ColorLED;
    LEDInfoAuto.Font.Color := IfThen(Decoder.DisplayFlagIsSet(dfAuto), FLEDDisplay.ColorLED, FLEDDisplay.SegmentOffColor);
    LEDInfoBatt.Font.Color := IfThen(Decoder.DisplayFlagIsSet(dfBatt), FLEDDisplay.ColorLED, FLEDDisplay.SegmentOffColor);

    flagstr := '';
    AddDisplayFlag(dfAuto, flagStr);
    AddDisplayFlag(dfHold, flagStr);
    AddDisplayFlag(dfDelta, flagStr);
    AddDisplayFlag(dfBatt, flagStr);
    if flagStr <> '' then txt := Format('%s [%s]', [txt, flagStr]);
    //LEDInfoPanel.Caption := txt;

    if Decoder.Quantity <> qUnknown then
      value := Decoder.QuantityText + ' = '
    else
      value := '';
    value := value + Decoder.ValueText[true, true] + ' = ' + Decoder.ValueText[false, true];

    AddDisplayFlag(dfDC, value);
    AddDisplayFlag(dfAC, value);
    AddDisplayFlag(dfRel, value);
    AddDisplayFlag(dfAuto, value);
    AddDisplayFlag(dfHold, value);
    AddDisplayFlag(dfBatt, value);
    AddDisplayFlag(dfDiode, value);
    AddDisplayFlag(dfMin, value);
    AddDisplayFlag(dfMax, value);
    AddDisplayFlag(dfBeep, value);
    AddDisplayFlag(dfDelta, value);

    txt := Decoder.BufferToString(Checkbox1.Checked);
    Memo.Lines.Add(Format('Received: "%s" --> "%s"', [txt, value]));
    StatusBar.SimpleText := Format('Current value: %s', [txt]);

    FPrevTime := t;
  end else
    StatusBar.SimpleText := Format('Decoder error %d', [Decoder.Status]);
end;


procedure TMainForm.OnDisconnectHandler(Sender: TObject);
begin
  FRunning := false;
end;


procedure TMainForm.OnErrorHandler(Sender: TObject; APortStatus: Integer);
var
  s: String;
begin
  if FRunning then inc(FMissing);
  StatusBar.Panels[COUNT_PANEL].Text := Format('%d values (%d missed)', [FMeasData.Count, FMissing]);

  s := Format('Port error %d (%s)', [APortStatus, GetSerialDeviceErrorText(APortStatus)]);
  Memo.Lines.Add(s);
  StatusBar.Panels[HINT_PANEL].Text := s;
end;


procedure TMainForm.OpenFile(const AFileName: String);
var
  data: TDataList;
  crs: TCursor;
  ser: TLineSeries;
  chartsource: TUserDefinedChartSource;
  ext: String;
begin
  crs := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    data := TDataList.Create;
    try
      ext := ExtractFileExt(AFileName);
      case Lowercase(ext) of
        '.xml': data.LoadFromXMLFile(AFileName);
        '.txt', 
        '.csv': data.LoadFromTextFile(AFileName);
      end;

      if (data.Count > 0) then begin
        data.TimeUnits := MeasSettings.TimeUnits;  // Convert to global time units
        
        if FMeasData.RawQuantName = '' then
        begin
          FMeasData.RawQuantName := data.RawQuantName;
          FMeasData.RawUnits := data.RawUnits;
        end;

        chartsource := TUserDefinedChartSource.Create(self);
        chartsource.OnGetChartDataItem := @ChartSourceGetChartDataItem;
        chartsource.Tag := PtrInt(data);
        chartsource.PointsNumber := data.Count;

        ser := TLineSeries.Create(self);
        ser.Title := ExtractFileName(AFileName);
        ser.Source := chartsource;
        ApplySeriesSettings(ser, NextPaletteIndex);
        inc(NextPaletteIndex);
        ser.AxisIndexX := 0;  // x has been moved to position 0 to have contiguous y axes.
        ser.AxisIndexY := 1;
        if data.HasComments then begin
          ser.Marks.LinkPen.Color := clGray;
          ser.Marks.Style := smsLabel;
          ser.Marks.Visible := true;
        end;
        Chart.AddSeries(ser);
        Chart.Extent.UseXMax := false;
        Chart.Extent.UseXMin := false;
        Chart.Extent.XMax := 0;
        Chart.ZoomFull;
        Chart.AxisList[ser.AxisIndexY].Title.Caption := data.Caption[FShowTransformedValues];
        UpdateChartBottomAxis;

        data.Series := ser;
        FDataList.Add(data);
       {$IFDEF DEBUG}
        saveall;
       {$ENDIF}
        ShowChartListbox(FDataList.Count > 1);

        PopulateFilesCombo;
        CbFiles.ItemIndex := FDataList.Count-1;
        CbFilesSelect(nil);

        SelectTransformation(MeasSettings.Transformation);

        UpdateCmdStates;
        Statusbar.Panels[MOUSE_PANEL].Text := SMouseInfo;
        MRUMenuManager.AddToRecent(AFileName);
      end else
      begin
        data.Free;
        Screen.Cursor := crs;
        MessageDlg(Format('No data recognized in "%s".', [AFileName]),
          mtError, [mbOK], 0);
      end;
    except
      on E:Exception do begin
        data.Free;
        Screen.Cursor := crs;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    Screen.Cursor := crs;
  end;
end;


procedure TMainForm.PageControlChange(Sender: TObject);
begin
  UpdateCmdStates;
end;


procedure TMainForm.PopulateFilesCombo;
var
  i: Integer;
  item: TDataList;
begin
  CbFiles.Items.Clear;
  CbFiles.Items.AddObject('Current measurement', FMeasData);
  for i:=1 to FDataList.Count-1 do begin
    item := TDataList(FDataList.Items[i]);
    CbFiles.Items.AddObject(item.FileName, item);
  end;
end;


procedure TMainForm.PopulateLogSource;
const
  n = 40;
var
  i : integer;
begin
  LogSource.Clear;
  LogSource.YCount := 2;
  for i := -n to +n do
    LogSource.DataPoints.Add(Format('1E%d|1E%d|1E%d||', [i,i,i]));
end;


procedure TMainForm.ProcessValue(ATime: TDateTime; AValue: Double);
var
  t: TDateTime;
  transfValue: Double;
  flag: TTransformFlag;
  indicator, s, txt: String;
begin
  if FMeasData.Count = 0 then begin
    // The first value has arrived - we store globally available metadata.
    FMeasData.RawQuantName := FSerialDevice.Decoder.QuantityText;
    FMeasData.RawUnits := FSerialDevice.Decoder.Units[true];
    FMeasData.RawMultiplier := FSerialDevice.Decoder.Multiplier;
    FMeasData.MeasDate := FStartTime;
    FMeasData.TimeUnits := MeasSettings.TimeUnits;
    UpdateDatamode;
    UpdateChartBottomAxis;
  end;

  // Convert current time to requested units
  t := ConvertFromDateTime(ATime - FStartTime, MeasSettings.TimeUnits);

  // Update chart
  if t > Chart.Extent.XMax then
    Chart.Extent.XMax := Chart.Extent.XMax + X_AXIS_GROWTH[MeasSettings.TimeUnits];
  FMeasData.AddValue(t, AValue);
  MeasChartSource.PointsNumber := FMeasData.Count;
  if MeasSeries.Active then
    UpdateChartTics;

  // Update grid
  if (PageControl.ActivePage = PgTable) and (CbFiles.ItemIndex = 0) then begin
    DataGrid.RowCount := FMeasData.Count + DataGrid.FixedRows;
    DataGrid.ColCount := IfThen(MeasSettings.Transformation = '', 4, 5);
    if DataGrid.ColCount = 5 then DataGrid.ColWidths[4] := DataGrid.ColWidths[3];
    DataGrid.Invalidate;
  end;

  // Update status bar
  if FMissing = 0 then
    Statusbar.Panels[COUNT_PANEL].Text := Format('%d values', [FMeasData.Count])
  else
    StatusBar.Panels[COUNT_PANEL].Text := Format('%d values (%d missed)', [FMeasData.Count, FMissing]);

  // Update value display in statusbar
  txt := FSerialDevice.Decoder.ValueText[true, true];  // raw value + units
  if FShowTransformedValues then begin
    transfValue := FMeasData.Transform(AValue, flag);
    case flag of
      tfTooSmallOut : indicator := '<';
      tfTooLargeOut : indicator := '>';
      else            indicator := '';
    end;
    if not IsEmptyNumber(transfValue) then begin

      if (abs(transfValue) > 1e7) or (abs(transfValue) < 1e-7) then
        s := Format('%s%.4e', [indicator, transfValue])
      else
        s := Format('%s%.6g', [indicator, transfValue]);
      if FMeasData.TransUnits <> '' then
        s := s + ' ' + FMeasData.TransUnits;
      txt := Format('%s (%s)', [txt, s]);
    end;
  end;
  Statusbar.Panels[VALUE_PANEL].Text := txt;

  // Update measurement interval
  MeasSettings.Interval := GetMeasInterval(t, AValue);
end;


procedure TMainForm.ReadFromIni;
var
  ini : TCustomIniFile;
  key, s : string;
  L, T, W, H : integer;
  i: Integer;
  IsMax : boolean;
  PresetList : TSerPresetList;
  item : TSerPresetItem;
begin
  PresetList := TSerPresetList.Create;
  try
    ini := CreateGlobalIni;
    try
      PresetList.ReadFromIni(ini, DEVICES_SCT);
      TransformationList.ReadFromIni(ini);
    finally
      ini.Free;
    end;

    ini := CreateIni;
    try
      key := 'MainForm';
      L := ini.ReadInteger(key, 'Left', Left);
      T := Ini.ReadInteger(key, 'Top', Top);
      W := ini.ReadInteger(key, 'Width', Width);
      H := ini.ReadInteger(key, 'Height', Height);
      IsMax := ini.ReadBool(key, 'Maximized', WindowState = wsMaximized);
      if W > Screen.Width then W := Screen.Width;
      if H > Screen.Height then H := Screen.Height;
      if L < 0 then L := 0;
      if T < 0 then T := 0;
      if L + W > Screen.Width then L := Screen.Width - W;
      if T + H > Screen.Height then T := Screen.Height - H;
      Left := L;
      Top := T;
      Width := W;
      Height := H;
      if IsMax then
        WindowState := wsMaximized
      else
        WindowState := wsNormal;
      PageControl.ActivePageIndex := ini.ReadInteger(key, 'PageIndex', 0);

      key := 'Settings';
      AcAutoSaveSettings.Checked := ini.ReadBool(key, 'AutoSaveSettings',
        AcAutoSaveSettings.Checked);
      s := Lowercase(ini.ReadString(key, 'SaveExt', '.txt'));
      AcFileSaveAs.Dialog.FilterIndex := GetDefaultFilterIndex(s);

      key := 'DeviceSettings';
      with DeviceSettings do begin
        Port := ini.ReadString(key, 'Port', Port);
        name := ini.ReadString(key, 'DeviceName', Name);
        item := nil;
        if (Name <> '') then begin
          item := PresetList.GetItemByName(Name);
          if item <> nil then begin
            Baudrate := item.BaudRate;
            DataBits := item.DataBits;
            StopBits := item.StopBits;
            Parity := item.Parity;
            Handshake := item.Handshake;
            Readout := item.Readout;
            Digits := item.Digits;
            SetLedDisplayNumDigits(Digits);
          end;
        end;
        if item = nil then begin
          Baudrate := ini.ReadInteger(key, 'BaudRate', Baudrate);
          Databits := ini.ReadInteger(key, 'Databits', Databits);
          StopBits := ini.ReadFloat(key, 'StopBits', StopBits);
          s := ini.ReadString(key, 'Parity', Parity);
          if s <> '' then
            Parity := s[1]
          else
            Parity := 'N';
          Handshake := ini.ReadInteger(key, 'Handshake', Handshake);
          Readout := roUnknown;
          Digits := 1;
        end;
      end;

      key := 'MeasSettings';
      with MeasSettings do begin
        Interval := ini.ReadFloat(key, 'Interval', Interval);
        Transformation := ini.ReadString(key, 'Transformation', Transformation);
        SelectTransformation(Transformation);
        SetDataMode(TDataMode(ini.ReadInteger(key, 'DataMode', ord(dmRaw))));
        SetTimeUnits(TTimeUnits(ini.ReadInteger(key, 'TimeUnits', ord(tuSeconds))));
        for i:=0 to 2 do
          with IntervalSettings[i] do begin
            s := 'IntervalSettings[' + IntToStr(i) + '].';
            Active := ini.ReadBool(key, s+'Active', Active);
            Interval := ini.ReadFloat(key, s+'Interval', Interval);
            Condition := TMeasIntervalCondition(ini.ReadInteger(key, s+'Condition', ord(Condition)));
            ConditionLimit := ini.ReadFloat(key, s+'ConditionLimit', ConditionLimit);
            ConditionLimitUnits := TTimeUnits(ini.ReadInteger(key, s+'ConditionLimitUnits', ord(ConditionLimitUnits)));
          end;
      end;

      key := 'DiagramSettings';
      with DiagramSettings do begin
        TimeDisplay := TTimeDisplay(ini.ReadInteger(key, 'TimeDisplay', ord(TimeDisplay)));
        if MeasSettings.Transformation = '' then
          SetLogarithmic(ini.Readbool(key, 'Logarithmic', Logarithmic));
          // Logarithmic setting of transformation is stored in transformation record.
        SetFullGrids(ini.ReadBool(key, 'FullGrids', FullGrids));
        for i:=0 to DEFAULT_SERIES_COUNT-1 do
          with SeriesSettings[i] do begin
            Style := TSeriesStyle(ini.ReadInteger(key,
              Format('Series%d_Style', [i+1]), ord(Style)));
            LineWidth := ini.ReadInteger(key,
              Format('Series%d_LineWidth', [i+1]), LineWidth);
            LineStyle := TPenStyle(ini.ReadInteger(key,
              Format('Series%d_LineStyle', [i+1]), ord(LineStyle)));
            LineColor := TColor(ini.ReadInteger(key,
              Format('Series%d_LineColor', [i+1]), ord(LineColor)));
            Symbol := TSeriesPointerStyle(ini.ReadInteger(key, Format('Series%d_Symbol', [i+1]),
              ord(Symbol)));
            SymbolBorderColor := TColor(ini.ReadInteger(key,
              Format('Series%d_SymbolBorderColor', [i+1]), ord(SymbolBorderColor)));
            SymbolColor := TColor(ini.ReadInteger(key,
              Format('Series%d_SymbolColor', [i+1]), ord(SymbolColor)));
          end;
      end;
    finally
      ini.Free;
    end;

  finally
    PresetList.Free;
  end;
end;


procedure TMainForm.SaveChart(const AFileName: String);
var
  ext: String;
  fs: TFileStream;
  id: IChartDrawer;
begin
  Chart.Legend.Visible := true;
  ext := Lowercase(ExtractFileExt(AFileName));
  case ext of
    '.bmp':
      Chart.SaveToBitmapFile(AFileName);
    '.jpg', '.jpeg':
      Chart.SaveToFile(TJPEGImage, AFileName);
    '.png':
      Chart.SaveToFile(TPortableNetworkGraphic, AFileName);
    '.svg':
      begin
        fs := TFileStream.Create(AFileName, fmCreate + fmShareDenyNone);
        try
          id := TSVGDrawer.Create(fs, true);
          id.DoChartColorToFPColor := @ChartColorSysToFPColor;
          with Chart do
            Draw(id, Rect(0, 0, Width, Height));
        finally
          fs.Free;
        end;
      end;
  end;
  Chart.Legend.Visible := false;
end;


procedure TMainForm.SelectTransformation(const AName: String);
var
  i: Integer;
  ser: TChartSeries;
begin
  MeasSettings.Transformation := AName;
  FMeasData.SelectTransformation(AName);
  SetupGrid;
  for i := 0 to Chart.SeriesCount-1 do begin
    if (Chart.Series[i] is TChartSeries) then
    begin
      ser := TChartSeries(Chart.Series[i]);
      if ser.Source is TUserDefinedChartSource then
        TUserDefinedChartSource(ser.Source).Reset;
    end;
  end;
  UpdateDataMode;
end;


procedure TMainForm.SetDataMode(AValue: TDataMode);
var
  s: string;
  u: string;
  T: TTransformation;
begin
  FShowTransformedValues := (AValue = dmTransformed) and (MeasSettings.Transformation <> '');
  
  AcDiagramRawData.Checked := (AValue = dmRaw);
  AcDiagramRawData.Enabled := (MeasSettings.Transformation <> '');
  AcDiagramTransformedData.Checked := (AValue = dmTransformed);
  AcDiagramTransformedData.Enabled := (MeasSettings.Transformation <> '');

  if FShowTransformedValues then
  begin
    T := FMeasData.Transformation;
    if T = nil then
      T := TransformationList.Find(MeasSettings.Transformation);
    if T = nil then
      raise Exception.Create('Transformation not found.');
    s := FMeasData.TransQuantName;
    u := FMeasData.TransUnits;
    SetLogarithmic(T.Logarithmic);
    if not IsNaN(T.MaxOut) then
    begin
      Chart.LeftAxis.Range.Max := T.MaxOut;
      Chart.LeftAxis.Range.UseMax := true;
    end;
    if not IsNaN(T.MinOut) then
    begin
      Chart.LeftAxis.Range.Min := T.MinOut;
      Chart.LeftAxis.Range.UseMin := true;
    end;
  end else
  begin
    s := FMeasData.RawQuantName;
    u := FMeasData.RawUnits;
    SetLogarithmic(false);
    Chart.LeftAxis.Range.UseMin := false;
    Chart.LeftAxis.Range.UseMax := false;
  end;

  if u = '' then
    Chart.LeftAxis.Title.Caption := s
  else
    Chart.LeftAxis.Title.Caption := Format('%s, %s', [s, u]);

  UpdateChart;
  SetupGrid;
end;


procedure TMainForm.SetFullGrids(AEnable:boolean);

  procedure SetAxisGrid(Axis:TChartAxis; Enable:boolean);
  begin
    with Axis do begin
      if Enable then begin
        with Grid do begin
          Style := psSolid;
          Color := clSilver;
          Visible := true;
        end;
        if Minors.Count > 0 then
          with Minors[0].Grid do begin
            Style := psDot;
            Color := $00D4D4D4; //clSilver;
            Visible := true;
          end;
      end else begin
        with Grid do begin
          Style := psDot;
          Color := clSilver;
          Visible := true;
        end;
        if Minors.Count > 0 then
          with Minors[0].Grid do
            Visible := false;
      end;
    end;
  end;

begin
  AcDiagramFullGrids.Checked := AEnable;
  DiagramSettings.FullGrids := AEnable;
  SetAxisGrid(Chart.LeftAxis, AEnable);
  SetAxisGrid(Chart.BottomAxis, AEnable);
end;


procedure TMainForm.SetLedDisplayNumDigits(AValue: Integer);
var
  delta: Integer;
begin
  FLedDisplay.NumDigits := AValue + 1;
  delta := FLedDisplay.DigitWidth + 16 + PanelValueInfos.Width;
  PanelValue.Width := FLedDisplay.NumDigits * FLedDisplay.DigitWidth + delta;
end;


procedure TMainForm.SetLogarithmic(AValue:boolean);
begin
  DiagramSettings.Logarithmic := AValue;
  if AValue then
    if not CanSetLogarithmic then begin
      MessageDlg('No logarithmic display due to non-positive data.', mtError, [mbOK], 0);
      AValue := false;
    end;
  AcDiagramLogarithmicY.Checked := AValue;
  LogAxisTransform.Enabled := AValue;
  UpdateChartTics;
  {
  with Chart.LeftAxis.Intervals do
    if AValue then begin
      Options := Options + [aipGraphCoords]; // - [aipUseMaxLength, aipUseMinLength];
      MinLength := 50;
      MaxLength := 90;
      NiceSteps := Format('%g|%g|%g', [Log10(0.2), Log10(0.5), Log10(1.0)]);
      Tolerance := 5;
    end else begin
      Options := Options - [aipGraphCoords]; // + [aipUseMaxLength, aipUseMinLength];
      MinLength := 10;
      MaxLength := 50;
      NiceSteps := '0.2|0.5|1.0';
      Tolerance := 2;
    end;
    }
end;


procedure TMainForm.SetSerialParams;
const
  TIMEOUT = 1000;
begin
  if FSerialDevice <> nil then
  begin
    FSerialDevice.Connect(
      DeviceSettings.Port,
      DeviceSettings.Baudrate,
      DeviceSettings.Databits,
      DeviceSettings.Parity,
      GetStopBits(DeviceSettings.StopBits),
      DeviceSettings.Handshake = hSoftware,
      DeviceSettings.Handshake = hHardware,
      TIMEOUT
    );
    with Memo.Lines do begin
      Add('Serial port parameters:');
      Add(Format('  Port: %s', [DeviceSettings.Port]));
      Add(Format('  Baudrate: %d', [DeviceSettings.Baudrate]));
      Add(Format('  Parity: %s', [''+DeviceSettings.Parity]));
      Add(Format('  Data bits: %d', [DeviceSettings.Databits]));
      Add(Format('  Stop bits: %s', [FloatToStr(Devicesettings.Stopbits)]));
      Add(Format('  Handshake: %s', [GetHandshakeString(DeviceSettings.Handshake)]));
    end;
  end;
end;


procedure TMainForm.SetTimeUnits(AValue: TTimeUnits);
begin
  MeasSettings.TimeUnits := AValue;
  UpdateChartBottomAxis;
  Chart.Invalidate;
end;


procedure TMainForm.SetupGrid;
var
  P: TDataList;
begin
  P := TDataList(CbFiles.Items.Objects[CbFiles.ItemIndex]);
  DataGrid.RowCount := DataGrid.FixedRows + P.Count;
  if P.HasTransformation and AcDiagramTransformedData.Checked then begin
    DataGrid.ColCount := 5;
    DataGrid.ColWidths[4] := DataGrid.ColWidths[3];
  end else
    DataGrid.ColCount := 4;
  DataGrid.Invalidate;
end;


procedure TMainForm.ShowChartListbox(Enable: Boolean);
begin
  ChartListbox.Visible := Enable;
  ChartListboxSplitter.Visible := ChartListbox.Visible;
  ChartListboxSplitter.Left := 0;
  if Enable then begin    // Re-build the chart listbox items.
    ChartListbox.Chart := nil;
    ChartListbox.Chart := Chart;
  end;
end;


procedure TMainForm.StartMeas;
begin
  if not FRunning then begin
    Memo.Lines.Clear;
    (*
    if FLogStream <> nil then
      WriteStringToStream(FLogStream, 'started'#13);
    *)
    FRunning := Connect;
    FMissing := 0;
    FMeasData.Clear;
    MeasSeries.Legend.Visible := true;
    MeasChartSource.PointsNumber := 0;
    Chart.Extent.XMax := X_AXIS_GROWTH[MeasSettings.TimeUnits];
    Chart.Extent.XMin := 0;
    Chart.Extent.UseXMax := true;
    Chart.Extent.UseXMin := true;
    ShowChartListbox(FDataList.Count > 1);
    MeasSettings.Interval := GetMeasInterval(0, NaN);
    (*
    FPrevDecimals := -1;
    Chart.ZoomFull;
    Chart.Show;
    *)
    FStartTime := now();
    FPrevTime := FStartTime;
    Statusbar.Panels[MOUSE_PANEL].Text := SMouseInfo;
    if FRunning then begin
      AcStartStopMeas.Caption := 'Stop';
      AcStartStopMeas.ImageIndex := IMGINDEX_STOP;
      BtnStartStopMeas.Caption := AcStartStopMeas.Caption;
      BtnStartStopMeas.ImageIndex := IMGINDEX_STOP;
      UpdateCmdStates;
      Application.ProcessMessages;
      Sleep(100);
    end;
  end;
end;


procedure TMainForm.StopMeas;
begin
  if FRunning then begin
    Disconnect;
    FRunning := false;
    AcStartStopMeas.Caption := 'Start';
    AcStartStopMeas.ImageIndex := IMGINDEX_START;
    FLEDDisplay.Clear;
    Chart.Extent.UseXMin := false;
    Chart.Extent.UseXMax := false;
    Chart.ZoomFull;
    Statusbar.Panels[COUNT_PANEL].Text := Format('%d values', [FMeasData.Count]);
    Statusbar.Panels[VALUE_PANEL].Text := '';
    UpdateCmdStates;

    (*
    if FLogStream <> nil then
      WriteStringToStream(FLogStream, 'stopped'#13);
    *)
  end;
end;


procedure TMainForm.UpdateCmdStates;
begin
  AcSerialPort.Enabled := not FRunning;
  AcAddComment.Enabled := FRunning;
  AcFileSaveAs.Enabled := (not FRunning) and ((FMeasData.Count > 0) or (FDataList.Count > 1));
  AcDiagramRawData.Enabled := (MeasSettings.Transformation <> '');
  AcDiagramTransformedData.Enabled := (MeasSettings.Transformation <> '');
  AcDiagramRemoveCurve.Enabled := (not FRunning) and (FDataList.Count > 1);
  AcTimeOffset.Enabled := (not FRunning) and (FDataList.Count > 1);
  AcCopyDiagramToClipboard.Enabled := (PageControl.ActivePage = PgDiagram);
end;


procedure TMainForm.UpdateChart;
begin
  UpdateChartSources;
  Chart.Invalidate;
end;


procedure TMainForm.UpdateChartBottomAxis;
begin
  with Chart.BottomAxis do begin
    case DiagramSettings.TimeDisplay of
      tdNumber:
        begin
          Marks.Source := nil;
          Marks.Style := smsValue;
          Intervals.MinLength := 40;
          Intervals.maxLength := 100;
        end;
      tdDateTime:
        begin
          Marks.Source := DateTimeIntervalChartSource;
          Marks.Style := smsLabel;
        end;
    end;
    Title.Caption := TIME_CAPTION[MeasSettings.TimeUnits];
  end;
end;


procedure TMainForm.UpdateChartSources;
var
  i: Integer;
  P: TDataList;
  series: TChartSeries;
begin
  for i:=0 to FDataList.Count-1 do begin
    P := TDataList(FDataList.Items[i]);
    series := P.Series;
    (series.Source as TUserDefinedChartSource).Reset;
  end;
end;


procedure TMainForm.UpdateChartTics;
var
  ymn, ymx : double;
  ex : TDoubleRect;
begin
  if Chart.IsZoomed then
    ex := Chart.LogicalExtent
  else
    ex := Chart.GetFullExtent;
  ymn := ex.a.y;
  ymx := ex.b.y;

  if SameValue(ymx, -UNDEFINED_NUMBER, UNDEFINED_NUMBER*1e-10) then
    exit;

  with Chart.LeftAxis.Range do
    if (ymn = ymx) then begin
      if AcDiagramLogarithmicY.Checked then begin
        Max := ymx * 10;
        Min := ymx * 0.1;
      end else begin
        Max := ymx + 0.5;
        Min := ymx - 0.5;
      end;
      UseMax := true;
      UseMin := true;
    end else begin
      UseMin := false;
      UseMax := false;
    end;

  with Chart.LeftAxis do begin
    if AcDiagramLogarithmicY.Checked then begin
      if (ymx - ymn > 2) then begin
        Marks.Source := LogSource;
        if Minors.Count > 0 then
          Minors[0].Intervals.Count := 9;
      end else begin
        Marks.Source := nil;
   //     Intervals.Options := Intervals.Options - [aipGraphCoords]; //, aipUseMaxLength];
        Intervals.Options := Intervals.Options + [aipGraphCoords]; // - [aipUseMaxLength];
        Intervals.MaxLength := 120;
        Intervals.MinLength := 20;
//        Intervals.NiceSteps := Format('%g|%g|%g', [log10(0.2), log10(0.5), log10(1.0)]);
        if Minors.Count > 0 then
          Minors[0].Intervals.Count := 4;
        Intervals.Tolerance := 30;
      end;
    end else begin
      Marks.Source := nil;
      Intervals.Options := Intervals.Options - [aipGraphCoords, aipUseCount] + [aipUseMaxLength];
      Intervals.MaxLength := 50;
      Intervals.MinLength := 10;
      Intervals.NiceSteps := '0.2|0.5|1.0';
      if Minors.Count > 0 then begin
        Minors[0].Intervals.Count := 4;
        {
        d := Value[1].FValue - Value[0].FValue;
        Minors[0].Intervals.Count := Round(Log10(Value[1].FValue - Value[0].FValue)) - 1;
        }
      end;
      Intervals.Tolerance := 2;
    end;
  end;
end;


procedure TMainForm.UpdateDataMode;
begin
  SetDataMode(GetDataMode);
end;


procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  key, s: String;
  i: Integer;
begin
  ini := CreateIni;
  try
    key := 'MainForm';
    ini.WriteBool(key, 'Maximized', (WindowState = wsMaximized));
    if WindowState = wsNormal then begin
      ini.WriteInteger(key, 'Left', Left);
      ini.WriteInteger(key, 'Top', Top);
      ini.WriteInteger(key, 'Width', Width);
      ini.WriteInteger(key, 'Height', Height);
    end;
    ini.WriteInteger(key, 'PageIndex', PageControl.ActivePageIndex);

    key := 'Settings';
    ini.EraseSection(key);
    ini.WriteBool(key, 'AutoSaveSettings', AcAutoSaveSettings.Checked);
    ini.WriteString(key, 'SaveExt', GetDefaultExt(AcFileSaveAs.Dialog.FilterIndex));
    (*
    ini.WriteBool('Settings', 'ShowRawData', AcShowRawData.Checked);
    if OpenDialog.FileName <> '' then
      ini.WriteString('Settings', 'OpenInitialDir', ExtractFileDir(OpenDialog.FileName));
    if SaveDialog.FileName <> '' then
      ini.WriteString('Settings', 'SaveInitialDir', ExtractFileDir(SaveDialog.FileName));
    *)

    key := 'DeviceSettings';
    ini.EraseSection(key);
    with DeviceSettings do begin
      ini.WriteString(key, 'DeviceName', Name);
      ini.WriteString(key, 'Port', Port);
      ini.WriteInteger(key, 'Baudrate', Baudrate);
      ini.WriteInteger(key, 'Databits', Databits);
      ini.WriteFloat(key, 'StopBits', StopBits);
      ini.WriteString(key, 'Parity', ''+Parity);
      ini.WriteInteger(key, 'Handshake', Handshake);
      ini.WriteInteger(key, 'Readout', Readout);
      ini.WriteInteger(key, 'Digits', Digits);
    end;

    key := 'DiagramSettings';
    ini.EraseSection(key);
    with DiagramSettings do begin
      ini.WriteInteger(key, 'TimeDisplay', ord(TimeDisplay));
      ini.WriteBool(key, 'Logarithmic', Logarithmic);
      ini.WriteBool(key, 'FullGrids', FullGrids);
      for i:=0 to DEFAULT_SERIES_COUNT-1 do
        with SeriesSettings[i] do begin
          ini.WriteInteger(key, Format('Series%d_Style', [i+1]), ord(Style));
          ini.WriteInteger(key, Format('Series%d_LineWidth', [i+1]), LineWidth);
          ini.WriteInteger(key, Format('Series%d_LineStyle', [i+1]), ord(LineStyle));
          ini.WriteInteger(key, Format('Series%d_LineColor', [i+1]), ord(LineColor));
          ini.WriteInteger(key, Format('Series%d_Symbol', [i+1]), ord(Symbol));
          ini.WriteInteger(key, Format('Series%d_SymbolBorderColor', [i+1]), ord(SymbolBorderColor));
          ini.WriteInteger(key, Format('Series%d_SymbolColor', [i+1]), ord(SymbolColor));
        end;
    end;

    key := 'MeasSettings';
    ini.EraseSection(key);
    with MeasSettings do begin
      ini.WriteFloat(key, 'Interval', Interval);
      ini.WriteString(key, 'Transformation', Transformation);
      ini.WriteInteger(key, 'DataMode', ord(GetDataMode));
      ini.WriteInteger(key, 'TimeUnits', ord(TimeUnits));
      for i:=0 to 2 do
        with IntervalSettings[i] do begin
          s := 'IntervalSettings[' + IntToStr(i) + '].';
          ini.WriteBool(key, s+'Active', Active);
          ini.WriteFloat(key, s+'Interval', Interval);
          ini.WriteInteger(key, s+'Condition', ord(Condition));
          ini.WriteFloat(key, s+'ConditionLimit', ConditionLimit);
          ini.WriteInteger(key, s+'ConditionLimitUnits', ord(ConditionLimitUnits));
        end;
    end;

  finally
    ini.Free;
  end;
end;


procedure TMainForm.ZoomDragToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
begin
  Unused(ATool);
  Unused(APoint);
  UpdateChartTics;
end;


end.


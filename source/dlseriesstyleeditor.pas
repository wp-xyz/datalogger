unit dlSeriesStyleEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ColorBox, Spin, ButtonPanel, types, TATypes, TAChartCombos;

type

  { TSeriesStyleEditor }

  TSeriesStyleEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    CbLineColor: TColorBox;
    CbShowSymbols: TCheckBox;
    CbSymbolColor: TColorBox;
    CbSymbolBorderColor: TColorBox;
    CbShowLines: TCheckBox;
    CbLineStyle: TChartComboBox;
    CbSymbol: TChartComboBox;
    GbLines: TGroupBox;
    GbSymbols: TGroupBox;
    LblLineColor: TLabel;
    LblSymbolColor: TLabel;
    LblSymbol: TLabel;
    LblLineWidth: TLabel;
    LblLineStyle: TLabel;
    LblBorderColor: TLabel;
    Panel1: TPanel;
    EdLineWidth: TSpinEdit;
    procedure CbLineStyleDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CbShowLinesChange(Sender: TObject);
    procedure CbShowSymbolsChange(Sender: TObject);
  private
    function GetLineColor: TColor;
    function GetLineStyle: TPenStyle;
    function GetLineWidth: Integer;
    function GetShowLines: Boolean;
    function GetShowSymbols: Boolean;
    function GetSymbol: TSeriesPointerStyle;
    function GetSymbolBorderColor: TColor;
    function GetSymbolColor: TColor;
    function IndexToSymbol(AIndex: Integer): TSeriesPointerStyle;
    procedure SetLineColor(AValue: TColor);
    procedure SetLineStyle(AValue: TPenStyle);
    procedure SetLineWidth(AValue: Integer);
    procedure SetShowLines(AValue: Boolean);
    procedure SetShowSymbols(AValue: Boolean);
    procedure SetSymbol(AValue: TSeriesPointerStyle);
    procedure SetSymbolBorderColor(AValue: TColor);
    procedure SetSymbolColor(AValue: TColor);

  public
    property LineColor: TColor read GetLineColor write SetLineColor;
    property LineStyle: TPenStyle read GetLineStyle write SetLineStyle;
    property Linewidth: Integer read GetLineWidth write SetLineWidth;
    property ShowLines: Boolean read GetShowLines write SetShowLines;
    property ShowSymbols: Boolean read GetShowSymbols write SetShowSymbols;
    property Symbol: TSeriesPointerStyle read GetSymbol write SetSymbol;
    property SymbolBorderColor: TColor read GetSymbolBorderColor write SetSymbolBorderColor;
    property SymbolColor: TColor read GetSymbolColor write SetSymbolColor;
  end;

var
  SeriesStyleEditor: TSeriesStyleEditor;


implementation

{$R *.lfm}

uses
  LCLType,
  TAGeometry;

{ TSeriesStyleEditor }

procedure TSeriesStyleEditor.CbLineStyleDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  SYMBOLWIDTH = 48;
var
  y: Integer;
begin
  y := (ARect.Top + ARect.Bottom) div 2;
  with CbLineStyle do begin
    if (ItemIndex <> -1) and (odSelected in State) then begin
      Canvas.Brush.Color := clHighlight;
//      Canvas.Font.Color := clHighlightText;
    end else begin
      Canvas.Brush.Color := Color;
  //    Canvas.Font.Color := clWindowText;
    end;
    Canvas.FillRect(ARect);
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Style := TPenStyle(PtrInt(Items.Objects[Index]));
    Canvas.Line(2, y, SYMBOLWIDTH, y);
    ARect.Left := SYMBOLWIDTH + 6;
    Canvas.TextOut(ARect.Left, y - Canvas.TextHeight('M') div 2, Items[Index]);
  end;
end;


procedure TSeriesStyleEditor.CbShowLinesChange(Sender: TObject);
begin
  CbLineStyle.Visible := CbShowLines.Checked;
  LblLineStyle.Visible := CbLineStyle.Visible;

  CbLineColor.Visible := CbShowLines.Checked;
  LblLineColor.Visible := CbLineColor.Visible;

  EdLineWidth.Visible := CbShowLines.Checked;
  LblLineWidth.Visible := EdLineWidth.Visible;
end;


procedure TSeriesStyleEditor.CbShowSymbolsChange(Sender: TObject);
begin
  CbSymbol.Visible := CbShowSymbols.Checked;
  LblSymbol.Visible := CbSymbol.Visible;

  CbSymbolColor.Visible := CbShowSymbols.Checked;
  LblSymbolColor.Visible := CbSymbolColor.Visible;

  CbSymbolBorderColor.Visible := CbShowSymbols.Checked;
  LblBorderColor.Visible := CbSymbolBorderColor.Visible;
end;


function TSeriesStyleEditor.GetLineColor: TColor;
begin
  result := CbLineColor.Selected;
end;


function TSeriesStyleEditor.GetLineStyle: TPenStyle;
begin
  Result := TPenStyle(PtrInt(CbLineStyle.Items.Objects[CbLineStyle.ItemIndex]));
end;


function TSeriesStyleEditor.GetLineWidth: Integer;
begin
  result := EdLineWidth.Value;
end;

function TSeriesStyleEditor.GetShowLines: Boolean;
begin
  Result := CbShowLines.Checked;
end;


function TSeriesStyleEditor.GetShowSymbols: Boolean;
begin
  Result := CbShowSymbols.Checked;
end;


function TSeriesStyleEditor.GetSymbol: TSeriesPointerStyle;
begin
  Result := IndexToSymbol(CbSymbol.ItemIndex);
end;


function TSeriesStyleEditor.GetSymbolColor: TColor;
begin
  result := CbSymbolColor.Selected;
end;


function TSeriesStyleEditor.GetSymbolBorderColor: TColor;
begin
  result := CbSymbolBorderColor.Selected;
end;


function TSeriesStyleEditor.IndexToSymbol(AIndex: Integer): TSeriesPointerStyle;
begin
  Result := TSeriesPointerStyle(PtrInt(CbSymbol.Items.Objects[AIndex]));
end;


procedure TSeriesStyleEditor.SetLineColor(AValue: TColor);
begin
  CbLineColor.Selected := AValue;
end;


procedure TSeriesStyleEditor.SetLineStyle(AValue: TPenStyle);
begin
  CbLineStyle.ItemIndex := CbLineStyle.Items.IndexOfObject(TObject(PtrInt(AValue)));
end;


procedure TSeriesStyleEditor.SetLineWidth(AValue: Integer);
begin
  EdLineWidth.Value := AValue;
end;


procedure TSeriesStyleEditor.SetShowLines(AValue: Boolean);
begin
  CbShowLines.Checked := AValue;
  CbShowLinesChange(nil);
end;


procedure TSeriesStyleEditor.SetShowSymbols(AValue: Boolean);
begin
  CbShowSymbols.Checked := AValue;
  CbShowSymbolsChange(nil);
end;


procedure TSeriesStyleEditor.SetSymbol(AValue: TSeriesPointerStyle);
begin
  CbSymbol.ItemIndex := CbSymbol.Items.IndexOfObject(TObject(PtrInt(AValue)));
end;


procedure TSeriesStyleEditor.SetSymbolBorderColor(AValue: TColor);
begin
  CbSymbolBorderColor.Selected := AValue;
end;


procedure TSeriesStyleEditor.SetSymbolColor(AValue: TColor);
begin
  CbSymbolColor.Selected := AValue;
end;


end.


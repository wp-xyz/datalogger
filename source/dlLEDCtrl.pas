unit dlLEDCtrl;

{ RackControls:
  TLEDButton, TButtonPanel, TScrewPanel, TLEDDisplay, TLEDMeter

  (C)opyright 2004 Version 1.20
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  RackControls ist eine Komponentensammlung zur Erstellung von
  Audiorack-ähnlichen Oberflächen. Diese Komponenten sind
  Public Domain, das Urheberrecht liegt aber beim Autor.

  Die Komponente TLEDDisplay ist eine Weiterentwicklung
  der Komponente TLCDDisplay von Luis Iglesias:
  luis.iglesias@vigo.org

  Änderungen, die bei LEDDisplay nachfolgende Nullen bei LeadingZeros=False doch zeichnet
  Ergänzt von Wolfgang Kleinrath

  Eigenschaft FSingleLED ergänzt von U. Conrad

  Lazarus adaptation: Luca Olivetti <luca@ventoso.org>

  wp : extension to neg numbers
       improved painting of digits
}

interface

{.$DEFINE BGRASupport}

uses
  LCLType, LCLIntf, LResources, LMessages, Buttons,
  Classes, Graphics, Controls, ExtCtrls, SysUtils,
 {$IFDEF BGRASupport}
  BGRABitmap, BGRABitmapTypes,
 {$ENDIF}
  Forms;

const
  EMPTY_VALUE = 1E308;

type
  TContrast        = 0..9;
  TDecSeparator    = (dsApostrophe, dsComma, dsColon, dsHyphen, dsNone, dsPoint, dsSemicolon);
  TSegmentStyle    = (ssRectangular, ssBeveled);

  TLEDDisplay = class(TGraphicControl)
  private
    FBevelStyle      : TPanelBevel;
    FBorderStyle     : TBorderStyle;
    FColorBackGround,
    FColorLED        : TColor;
    FDecSeparator    : TDecSeparator;
   {$IFDEF BGRASupport}
    FDigit           : array [-2..10] of TBGRABitmap;  // -1: '-', -2: ' ', 10: 'L'
   {$ELSE}
    FDigit           : array [-2..10] of TBitmap;      // -1: '-', -2: ' ', 10: 'L'
   {$ENDIF}
    FDigitHeight     : integer;
    FDigitShapeColor : TColor;
    FDigitWidth      : integer;
    FDrawDigitShapes : boolean;
    FFractionDigits  : integer;
    FLEDContrast     : TContrast;
    FSegmentOffColor : TColor;
    FLineWidth,
    FNumDigits       : integer;
    FLeadingZeros    : boolean;
    FSegCl           : array [-2..10, 1..7] of TColor;
    FSegmentStyle    : TSegmentStyle;
    FValue           : extended;
    FDisplayText     : string;
    FAngle           : integer;
    FOnChange        : TNotifyEvent;

    procedure SetAngle(newValue:integer);
    procedure SetBevelStyle(newValue: TPanelBevel);
    procedure SetBorderStyle(newValue: TBorderStyle);
    procedure SetColorBackGround(newValue: TColor);
    procedure SetColorLED(newValue: TColor);
    procedure SetDecSeparator(newValue: TDecSeparator);
    procedure SetDigitHeight(newValue: integer);
    procedure SetDigitWidth(newValue: integer);
    procedure SetDrawDigitShapes(newValue: boolean);
    procedure SetFractionDigits(newValue: integer);
    procedure SetLeadingZeros(newValue: boolean);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetLineWidth(newValue: integer);
    procedure SetNumDigits(newValue: integer);
    procedure SetSegmentStyle(newValue: TSegmentStyle);
    procedure SetText(const newValue:string);
    procedure SetValue(newValue: extended);

  protected
    procedure AssignColors(seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);
    procedure Change; dynamic;
    procedure CreateDigitBitmaps;
    procedure MakeDisplayText;
    procedure Paint; override;
    procedure PaintSep(DigitLeft,DigitTop,DigitSpace:integer);

  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  Clear;
    property SegmentOffColor: TColor read FSegmentOffColor;

  published
    property Angle : integer read FAngle write SetAngle default 5;
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ColorBackGround: TColor read FColorBackGround write setColorBackGround default clOlive;
    property ColorLED: TColor read FColorLED write setColorLED default clLime;
    property DecSeparator: TDecSeparator read FDecSeparator write setDecSeparator default dsPoint;
    property DigitHeight: integer read FDigitHeight write setDigitHeight default 30;
    property DigitWidth: integer read FDigitWidth write setDigitWidth default 20;
    property DigitLineWidth: integer read FLineWidth write setLineWidth default 3;
    property DrawDigitShapes: boolean read FDrawDigitShapes write SetDrawDigitShapes default true;
    property FractionDigits: integer read FFractionDigits write setFractionDigits default 0;
    property Height default 36;
    property LeadingZeros: boolean read FLeadingZeros write setLeadingZeros default true;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast default 6;
    property NumDigits: integer read FNumDigits write setNumDigits default 6;
    property SegmentStyle: TSegmentStyle read FSegmentStyle write setSegmentStyle default ssBeveled;
    property Text:string read FDisplayText write SetText;
    property Value: extended read FValue write SetValue;
    property Visible;
    property Width default 168;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  EColorConv = class(Exception);


implementation

uses
  math;

const
  SHAPECONTRAST = 3;

type
  TColorVector = record
    case Integer of
      0: (Coord: array[0..2] of Double);
      1: (R, G, B: Double);
      2: (H, L, S: Double);
  end;

function HLS2RGB(const HLS: TColorVector): TColorVector;
const
  Hue: array[0..5, 0..2] of Integer = (
    (1, -1, -1),  // red
    (1, 1, -1),   // yellow
    (-1, 1, -1),  // green
    (-1, 1, 1),   // cyan
    (-1, -1, 1),	// blue
    (1, -1, 1)    // magenta
  );
  Components: array[0..2] of string = ('Hue', 'Luminance', 'Saturation');
var
  i, j, k: Integer;
  x: Double;
begin
  for i := Low(HLS.Coord) to High(HLS.Coord) do
    if (HLS.Coord[i] < 0) or (HLS.Coord[i] > 1) then
      raise EColorConv.CreateFmt('HLS2RGB: 0 <= %s value <= 1 required', [Components[i]]);

  j := Trunc(HLS.H * 6) mod 6;
  k := (j+1) mod 6;
  x := Frac(HLS.H * 6);
  for i := 0 to 2 do
    Result.Coord[i] := Hue[j, i] + x*(Hue[k, i]-Hue[j, i]);
  for i := 0 to 2 do Result.Coord[i] := Result.Coord[i] * HLS.S;
  if HLS.L <= 0.5 then
    for i := 0 to 2 do Result.Coord[i] := HLS.L * (Result.Coord[i]+ 1)
  else
    for i := 0 to 2 do Result.Coord[i] := HLS.L + Result.Coord[i] * (1-HLS.L);

  for i := 0 to 2 do
    if Result.Coord[i] < 0 then Result.Coord[i] := 0 else
    if Result.Coord[i] > 1 then Result.Coord[i] := 1;
end;

function RGB2HLS(const RGB: TColorVector): TColorVector;
const
  Epsilon = 1E-8;
  Components: array[0..2] of string = ('Red', 'Green', 'Blue');
var
  i, k: Integer;
  x: Double;
  V: TColorVector;
  W: TColorVector absolute Result;
  Hue: Double;
  Sat: Double;
  Lum: Double;

  function GetHue: Double;
  begin
    case k of
      0: if W.G > W.B then
           Result := 2+(W.B+1)/2
         else
           Result := 4-(W.G+1)/2;
      1: if W.B > W.R then
           Result := 4+(W.R+1)/2
         else
           Result := 6-(W.B+1)/2;
      2: if W.R > W.G then
           Result := (W.G+1)/2
         else
           Result := 2-(W.R+1)/2;
      else Result := 0;
    end;
    Result := Result/6;
  end;

begin
  for i := Low(RGB.Coord) to High(RGB.Coord) do
    if (RGB.Coord[i] < 0) or (RGB.Coord[i] > 1) then
      raise EColorConv.CreateFmt('RGB2HLS: 0 <= %s value <= 1 required', [Components[i]]);

  x := 0;
  for i := 0 to 2 do begin
    V.Coord[i] := 2*RGB.Coord[i]-1; // [0, 1] -> [-1, 1]
    if Abs(V.Coord[i]) > x then begin
      x := Abs(V.Coord[i]);
      k := i;	// index of RGB coordinate most different from 0.5
    end;
  end;
  if x < Epsilon then	begin // middle grey
    Result.H := 0;
    Result.L := 0.5; // could be RGB.G or RGB.B as well
    Result.S := 0;
    Exit;
  end else
    x := 1/x;
  for i := 0 to 2 do
    W.Coord[i] := V.Coord[i] * x;
  x := 0;
  if V.Coord[k] <= 0 then begin
    for i := 0 to 2 do
      if (W.Coord[i]+1) > x then
        x := W.Coord[i] +1;
    if x < Epsilon then begin // R = G = B: location on grey axis
      Result.H := 0;
      Result.L := RGB.R; // could be RGB.G or RGB.B as well
      Result.S := 0;
      Exit;
    end else
      x := 2/x;
    for i := 0 to 2 do W.Coord[i] := x*(W.Coord[i]+1)-1;
    Hue := GetHue;
    // compute saturation
    if Abs(V.G-V.R) > Epsilon then
      Sat := (V.G-V.R)/(W.Coord[1]*(V.R+1)-W.Coord[0]*(V.G+1))
    else if Abs(V.B-V.G) > Epsilon then
      Sat := (V.B-V.G)/(W.Coord[2]*(V.G+1)-W.Coord[1]*(V.B+1))
    else if Abs(V.B-V.R) > Epsilon then
      Sat := (V.B-V.R)/(W.Coord[2]*(V.R+1)-W.Coord[0]*(V.B+1))
    else Sat := 0;
    // compute luminance
    if Abs(W.Coord[1]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[1]*(V.R+1)-W.Coord[0]*(V.G+1))/(W.Coord[1]-W.Coord[0])
    else if Abs(W.Coord[2]-W.Coord[1]) > Epsilon then
      Lum := (W.Coord[2]*(V.G+1)-W.Coord[1]*(V.B+1))/(W.Coord[2]-W.Coord[1])
    else if Abs(W.Coord[2]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[2]*(V.R+1)-W.Coord[0]*(V.B+1))/(W.Coord[2]-W.Coord[0])
    else Lum := V.R+1;
    Lum := Lum * 0.5;
  end else begin
    for i := 0 to 2 do
      if (1-W.Coord[i]) > x then
        x := 1-W.Coord[i];
    if x < Epsilon then begin // R = G = B: location on grey axis
      Result.H := 0;
      Result.L := RGB.R; // could be RGB.G or RGB.B as well
      Result.S := 0;
      Exit;
    end else
      x := 2/x;
    for i := 0 to 2 do
      W.Coord[i] := x*(W.Coord[i]-1)+1;
    x := 1;
    for i := 0 to 2 do
      if W.Coord[i] < x then begin
        x := W.Coord[i];
        k := i;
      end;
    Hue := GetHue;
    // compute saturation
    if Abs(V.G-V.R) > Epsilon then
      Sat := (V.G-V.R)/(W.Coord[0]*(V.G-1)-W.Coord[1]*(V.R-1))
    else if Abs(V.B-V.G) > Epsilon then
      Sat := (V.B-V.G)/(W.Coord[1]*(V.B-1)-W.Coord[2]*(V.G-1))
    else if Abs(V.B-V.R) > Epsilon then
      Sat := (V.B-V.R)/(W.Coord[0]*(V.B-1)-W.Coord[2]*(V.R-1))
    else
      Sat := 0;
    // compute luminance
    if Abs(W.Coord[1]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[1]*(V.R-1)-W.Coord[0]*(V.G-1))/(W.Coord[1]-W.Coord[0])
    else if Abs(W.Coord[2]-W.Coord[1]) > Epsilon then
      Lum := (W.Coord[2]*(V.G-1)-W.Coord[1]*(V.B-1))/(W.Coord[2]-W.Coord[1])
    else if Abs(W.Coord[2]-W.Coord[0]) > Epsilon then
      Lum := (W.Coord[2]*(V.R-1)-W.Coord[0]*(V.B-1))/(W.Coord[2]-W.Coord[0])
    else
      Lum := V.R-1;
    Lum := 1 + Lum * 0.5;
  end;
  W.H := Hue;
  W.L := Lum;
  W.S := Sat;

  for i := 0 to 2 do
    if W.Coord[i] < 0 then W.Coord[i] := 0 else
    if W.Coord[i] > 1 then W.Coord[i] := 1;
end;


procedure Get3DColors(FaceColor: TColor; var HighLightColor, ShadowColor: TColor; HLFactor, ShFactor: single);
var
  V,HLS : TColorVector;
  R,G,B : Byte;
begin
  FaceColor := ColorToRGB(FaceColor);
  R := Red(FaceColor);
  G := Green(FaceColor);
  B := Blue(FaceColor);
  HighLightColor := RGBToColor(
 	  255-round((256-R) * HLFactor),
	  255-round((256-G) * HLFactor),
 	  255-round((256-B) * HLFactor)
  );
  V.R := R/255;
  V.G := G/255;
  V.B := B/255;
  HLS := RGB2HLS(V);
  HLS.L := HLS.L * ShFactor;	// Luminance := Luminance * Shadowfactor
  V := HLS2RGB(HLS);
  ShadowColor := RGBToColor(
    Round(V.R*255),
    Round(V.G*255),
    Round(V.B*255)
  );
end;

function GetIntermediateColor(Color1,Color2:TColor;AContrast:integer):TColor;
var
  YR,YG,YB,SR,
  SG,SB,DR,DG,DB,
  StartClr,EndClr : integer;
begin
  StartClr:=ColorToRGB(Color1);
  YR := Red(StartClr);
  YG := Green(StartClr);
  YB := Blue(StartClr);
  SR := YR;
  SG := YG;
  SB := YB;
  EndClr := ColorToRGB(Color2);
  DR := Red(EndClr)-SR;
  DG := Green(EndClr)-SG;
  DB := Blue(EndClr)-SB;
  YR := SR + round(DR / 9 * AContrast);
  YG := SG + round(DG / 9 * AContrast);
  YB := SB + round(DB / 9 * AContrast);
  Result := RGBToColor( YR, YG, YB);
end; {GetIntermediateColor}

function GetOffColor(const OnColor:TColor;const AContrast:TContrast):TColor;
var
  Dummy : TCOlor;
begin
  Get3DColors(OnColor, Dummy, Result,(10-AContrast)/10,(10-AContrast)/10);
end; {CalculateOffColor}

procedure AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
begin
  Get3DColors(FaceColor,HighlightColor,ShadowColor,(10-HLContrast)/10,(10-ShContrast)/10);
end; {AssignBevelColors}


{ Komponente TLEDDisplay }
constructor TLEDDisplay.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FAngle := 5;
  FBevelStyle := bvLowered;
  FBorderStyle := bsSingle;
  FColorBackGround := clBlack;
  FColorLED := clLime;
  FLEDContrast := 6;
  FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
  FDigitShapeColor := GetIntermediateColor(FColorBackground, clBlack, SHAPECONTRAST);
  FDrawDigitShapes := true;
  FDecSeparator := dsPoint;
  FDigitHeight := 30;
  FDigitWidth := 20;
  FLineWidth := 3;
  FLeadingZeros := true;
  FNumDigits := 6;
  FSegmentStyle := ssBeveled;
  FValue := 0;
  Height := 36;
  Width := 168;
  CreateDigitBitmaps;
  MakeDisplayText;
end;

destructor TLEDDisplay.Destroy;
var c:integer;
begin
  for c := -2 to 10 do
    FDigit[c].free;
  inherited destroy;
end;

procedure TLEDDisplay.AssignColors(seg:integer; s1,s2,s3,s4,s5,s6,s7:Boolean);
begin
  if s1 then
    FSegCl[seg, 1] := FColorLED
  else
    FSegCl[seg, 1] := FSegmentOffColor;
  if s2 then
    FSegCl[seg, 2] := FColorLED
  else
    FSegCl[seg, 2] := FSegmentOffColor;
  if s3 then
    FSegCl[seg, 3] := FColorLED
  else
    FSegCl[seg, 3] := FSegmentOffColor;
  if s4 then
    FSegCl[seg, 4] := FColorLED
  else
    FSegCl[seg, 4] := FSegmentOffColor;
  if s5 then
    FSegCl[seg, 5] := FColorLED
  else
    FSegCl[seg, 5] := FSegmentOffColor;
  if s6 then
    FSegCl[seg, 6] := FColorLED
  else
    FSegCl[seg, 6] := FSegmentOffColor;
  if s7 then
    FSegCl[seg, 7] := FColorLED
  else
    FSegCl[seg, 7] := FSegmentOffColor;
end;

procedure TLEDDisplay.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLEDDisplay.Clear;
begin
  Setvalue(EMPTY_VALUE);
end;

procedure TLEDDisplay.CreateDigitBitmaps;
var
  TL, TR, TBL, TBR,
  ML, MTL, MTR, MR,
  MBL, MBR, BL, BTL,
  BTR, BR, P1, P2    : TPoint;
  c, wAlt, LineW     : integer;
  alpha, tan_a       : double;

 {$IFDEF BGRASupport}
  function CreatePoly(const Points: array of TPoint) : ArrayOfTPointF;
  var
    i : integer;
  begin
    SetLength(result, High(Points) + 1);
    for i:=0 to High(Points) do
      result[i] := PointF(Points[i].X, Points[i].Y);
  end;
 {$ENDIF}

  procedure Slant(var P:TPoint);
  begin
    P.x := P.x + trunc(tan_a * (FDigitHeight - P.y));
  end;

begin
  LineW := FLineWidth+2;
  wAlt := FDigitHeight;
  alpha := pi() / 180.0 * FAngle;
  tan_a := tan(alpha);

  { Polygonpunkte zuweisen }
  TL.x := 0;
  TL.y := 0;
  TR.x := FDigitWidth - 1;
  TR.y := 0;
  TBL.x := LineW - 1;
  TBL.y := LineW -1;
  TBR.x := FDigitWidth - LineW;
  TBR.y := TBL.y;
  ML.x := 0;
  ML.y := wAlt div 2;
  MTL.x := TBL.x;
  MTL.y := ML.y - (LineW div 2);
  MTR.x := TBR.x;
  MTR.y := MTL.y;
  MR.x := TR.x;
  MR.y := ML.y;
  MBL.x := TBL.x;
  MBL.y := ML.y + (LineW div 2);
  MBR.x := MTR.x;
  MBR.y := MBL.y;
  BL.x := 0;
  BL.y := wAlt - 1;
  BR.x := TR.x;
  BR.y := BL.y;
  BTL.x := TBL.x;
  BTL.y := wAlt - LineW;
  BTR.x := TBR.x;
  BTR.y := BTL.y;

  Slant(TL);
  Slant(TR);
  Slant(TBL);
  Slant(TBR);
  Slant(ML);
  Slant(MTL);
  Slant(MTR);
  Slant(MR);
  Slant(MBL);
  Slant(MBR);
  Slant(BL);
  Slant(BR);
  Slant(BTL);
  Slant(BTR);

  { Segmentfarben zuweisen }
  {     --1--
        2   3
        --4--
        5   6
        --7--
  }

  AssignColors(-2,false,false,false,false,false,false,false);  // all off
  AssignColors(-1,false,false,false,true,false,false,false);   // '-'
  AssignColors (0,true,true,true,false,true,true,true);
  AssignColors (1,false,false,true,false,false,true,false);
  AssignColors (2,true,false,true,true,true,false,true);
  AssignColors (3,true,false,true,true,false,true,true);
  AssignColors (4,false,true,true,true,false,true,false);
  AssignColors (5,true,true,false,true,false,true,true);
  AssignColors (6,false,true,false,true,true,true,true);
  AssignColors (7,true,false,true,false,false,true,false);
  AssignColors (8,true,true,true,true,true,true,true);
  AssignColors (9,true,true,true,true,false,true,true);
  AssignColors(10,false,true,false,false,true,false,true);

  { Bitmap erstellen }
  for c := -2 to 10 do begin
    FDigit[c].free;
   {$IFDEF BGRASupport}
    FDigit[c] := TBGRABitmap.Create(FDigitWidth + round(tan_a*FDigitHeight), wAlt, FColorBackground);
    with FDigit[c] do begin
   {$ELSE}
    FDigit[c] := TBitmap.Create;
    FDigit[c].width := FDigitWidth + round(tan_a*FDigitHeight);
    FDigit[c].height := wAlt;
    with FDigit[c].canvas do begin
      if FDrawDigitShapes then
        Pen.Color := FDigitShapeColor
      else
        Pen.Color := FColorBackGround;
      Brush.Color := FColorBackGround;
      Brush.Style := bsSolid;
      Pen.Width := 1;
      Rectangle (TL.x, TL.y, BR.x+1, BR.y+1);
      if FSegmentStyle=ssRectangular then
        Pen.Width:=FLineWidth;
   {$ENDIF}

   { Segment 1 }
     {$IFDEF BGRASupport}
      if FSegmentStyle = ssRectangular then begin
        P1 := Point(FLineWidth, FLineWidth div 2);
        P2 := Point(FDigitWidth-FLineWidth-1, FLineWidth div 2);
        Slant(P1);
        Slant(P2);
        DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 1]), false);
      end else
        FillPolyAntiAlias(CreatePoly([TL, TR, TBR, TBL]), ColorToBGRA(FSegCl[c, 1]));
     {$ELSE}
      Brush.Color := FSegCl[c, 1];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 1];
        P1 := Point(FLineWidth, FLineWidth div 2);
        P2 := Point(FDigitWidth-FLineWidth-1, FLineWidth div 2);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon ([TL, TR, TBR, TBL]);
     {$ENDIF}

     { Segment 2 }
     {$IFDEF BGRASupport}
      if FSegmentStyle = ssRectangular then begin
        P1 := Point(FLineWidth div 2, FLineWidth*3 div 2);
        P2 := Point(P1.x, FDigitHeight div 2 - FlineWidth);
        Slant(P1);
        Slant(P2);
        DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 2]), false);
      end else
        FillPolyAntiAlias(CreatePoly([TL, TBL, MTL, ML]), ColorToBGRA(FSegCl[c, 2]));
     {$ELSE}
      Brush.Color := FSegCl[c, 2];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 2];
        P1 := Point(FLineWidth div 2, FLineWidth*3 div 2);
        P2 := Point(P1.x, FDigitHeight div 2 - FlineWidth);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon ([TL, TBL, MTL, ML]);
     {$ENDIF}

     { Segment 3 }
     {$IFDEF BGRASupport}
      if FSegmentStyle = ssRectangular then begin
        P1 := Point(FDigitWidth - FlineWidth div 2 - 1, FLineWidth*3 div 2);
        P2 := Point(P1.x, FDigitHeight div 2 - FLineWidth);
        Slant(P1);
        Slant(P2);
        DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 3]), false);
      end else
        FillPolyAntiAlias(CreatePoly([TR, MR, MTR, TBR]), ColorToBGRA(FSegCl[c,3]));
     {$ELSE}
      Brush.Color := FSegCl[c, 3];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 3];
        P1 := Point(FDigitWidth - FlineWidth div 2 - 1, FLineWidth*3 div 2);
        P2 := Point(P1.x, FDigitHeight div 2 - FLineWidth);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon ([TR, MR, MTR, TBR]);
     {$ENDIF}

     { Segment 4 }
     {$IFDEF BGRASupport}
      if FSegmentStyle = ssRectangular then begin
        P1 := Point(FLineWidth, FDigitHeight div 2);
        P2 := Point(FDigitWidth - FLineWidth, FDigitHeight div 2);
        Slant(P1);
        Slant(P2);
        DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 4]), false);
      end else
        FillPolyAntiAlias(CreatePoly([ML, MTL, MTR, MR, MBR, MBL]), ColorToBGRA(FSegCl[c, 4]));
     {$ELSE}
      Brush.Color := FSegCl[c, 4];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 4];
        P1 := Point(FLineWidth, FDigitHeight div 2);
        P2 := Point(FDigitWidth - FLineWidth, FDigitHeight div 2);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon ([ML, MTL, MTR, MR, MBR, MBL]);
     {$ENDIF}

     { Segment 5 }
     {$IFDEF BGRASupport}
      if FSegmentStyle = ssRectangular then begin
        P1 := Point(FlineWidth div 2, FDigitHeight div 2 + FLineWidth);
        P2 := Point(P1.x, FDigitHeight - FLineWidth*3 div 2);
        Slant(P1);
        Slant(P2);
        DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 5]), false);
      end else
        FillPolyAntiAlias(CreatePoly([ML, MBL, BTL, BL]), ColorToBGRA(FSegCl[c, 5]));
     {$ELSE}
      Brush.Color := FSegCl[c, 5];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 5];
        P1 := Point(FlineWidth div 2, FDigitHeight div 2 + FLineWidth);
        P2 := Point(P1.x, FDigitHeight - FLineWidth*3 div 2);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon ([ML, MBL, BTL, BL]);
     {$ENDIF}

     { Segment 6 }
     {$IFDEF BGRASupport}
      if FSegmentStyle = ssRectangular then begin
        P1 := Point(FDigitWidth - FLineWidth div 2 - 1, FDigitHeight div 2 + FLineWidth);
        P2 := Point(P1.x, FDigitHeight - FLineWidth*3 div 2);
        Slant(P1);
        Slant(P2);
        DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 6]), false);
      end else
        FillPolyAntiAlias(CreatePoly([MR, BR, BTR, MBR]), ColorToBGRA(FSegCl[c, 6]));
     {$ELSE}
      Brush.Color := FSegCl[c, 6];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 6];
        P1 := Point(FDigitWidth - FLineWidth div 2 - 1, FDigitHeight div 2 + FLineWidth);
        P2 := Point(P1.x, FDigitHeight - FLineWidth*3 div 2);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon ([MR, BR, BTR, MBR]);
     {$ENDIF}

      { Segment 7 }
     {$IFDEF BGRASupport}
       if FSegmentStyle = ssRectangular then begin
         P1 := Point(FLineWidth, FDigitHeight - FlineWidth div 2 - 1);
         P2 := Point(FDigitWidth - FLineWidth, FDigitHeight - FLineWidth div 2 - 1);
         Slant(P1);
         Slant(P2);
         DrawLineAntiAlias(P1.X, P1.Y, P2.X, P2.Y, ColorToBGRA(FSegCl[c, 7]), false);
       end else
         FillPolyAntiAlias(CreatePoly([BL, BTL, BTR, BR]), ColorToBGRA(FSegCl[c, 7]));
     {$ELSE}
      Brush.Color := FSegCl[c, 7];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 7];
        P1 := Point(FLineWidth, FDigitHeight - FlineWidth div 2 - 1);
        P2 := Point(FDigitWidth - FLineWidth, FDigitHeight - FLineWidth div 2 - 1);
        Slant(P1);
        Slant(P2);
        MoveTo(P1.x, P1.y);
        LineTo(P2.x, P2.y);
      end
      else
        Polygon([BL, BTL, BTR, BR]);
     {$ENDIF}
    end;
  end;
end;

procedure TLEDDisplay.MakeDisplayText;
begin
  FDisplayText := '';

  if IsInfinite(FValue) then begin
    FDisplayText := ' .0L';
    while true do begin
      if Length(FDisplayText) <= FNumDigits then
        FDisplayText := FDisplayText + ' '
      else
        exit;
      if Length(FDisplayText) <= FNumdigits then
        FDisplayText := ' ' + FDisplayText
      else
       exit;
    end;
  end;

  if not SameValue(FValue, EMPTY_VALUE, 1e-8) then begin
    try
      FDisplayText := FloatToStrF(FValue, ffFixed, 18, FFractionDigits);
    except
    end;
  end;

  // make sure to have a decimal separator in the string
  if pos(DecimalSeparator, FDisplayText) = 0 then
    FDisplayText := FDisplayText + DecimalSeparator;
  if FLeadingZeros then begin
    if FValue < 0 then
      while Length(FDisplayText) < FNumDigits+1 do
        System.Insert('0', FDisplayText, 2)
    else
      while Length(FDisplayText) < FNumDigits+1 do
        FDisplaytext := '0' + FDisplayText;
  end else
    while Length(FDisplayText) < FNumDigits+1 do
      FDisplayText := ' ' + FDisplayText;

  // remove aux decimals separator added above.
  if FDisplayText[Length(FDisplayText)] = DecimalSeparator then
    System.Delete(FDisplayText, Length(FDisplayText), 1);
end;

procedure TLEDDisplay.Paint;
var
  i,j : integer;
  Area : TRect;
  DigitSpace,
  DigitLeft,
  DigitTop : integer;
begin
  Area := getClientRect;
  Digitspace := Width div FNumDigits;
  DigitTop := (Height - DigitHeight) div 2;
  DigitLeft := (DigitSpace - FDigitWidth) div 2;
  with Canvas do begin
    Brush.Color := FColorBackground;
    FillRect(Area);
    for i:=1 to Length(FDisplaytext) do begin
      if (FDisplaytext[i] = DecimalSeparator) or (FDisplaytext[i] = '.') then
        PaintSep(DigitLeft, DigitTop, DigitSpace)
      else begin
        case FDisplayText[i] of
          ' ' : j := -2;
          '-' : j := -1;
          'L' : j := 10;
          else  j := StrToInt(FDisplayText[i]);
        end;
       {$IFDEF BGRASupport}
        FDigit[j].Draw(Canvas, DigitLeft, DigitTop);
       {$ELSE}
        Draw(DigitLeft, DigitTop, FDigit[j]);
       {$ENDIF}
        inc(DigitLeft, DigitSpace);
      end;
    end;
    { Bevel zeichnen }
    if BevelStyle <> bvNone then begin
      if BevelStyle = bvRaised then
        Pen.Color := clBtnHighlight
      else
        Pen.Color := clBtnShadow;
      MoveTo(Area.Right-1,Area.Top);
      LineTo(Area.Left,Area.Top);
      LineTo(Area.Left,Area.Bottom-1);
      if BevelStyle = bvRaised then
        Pen.Color := clBtnShadow
      else
        Pen.Color := clBtnHighlight;
      MoveTo(Area.Left,Area.Bottom-1);
      LineTo(Area.Right-1,Area.Bottom-1);
      LineTo(Area.Right-1,Area.Top);
      InflateRect(Area,-1,-1);
    end;
    { Border zeichnen }
    if BorderStyle <> bsNone then begin
      Brush.Color := clWindowFrame;
      FrameRect(Area);
    end;
  end;
end;

procedure TLEDDisplay.PaintSep(DigitLeft,DigitTop,DigitSpace:integer);
var
  SepLeft, SepTop, SepWidth : integer;
begin
  with Canvas do begin
    Brush.Color := FColorLED;
    Pen.Width := 1;
    if FDrawDigitShapes then
      Pen.Color := FDigitShapeColor
    else
      Pen.Color := FColorBackground;

    SepLeft := DigitLeft - (DigitSpace - FDigitWidth) div 2 - FLineWidth + 1; //6;

    if (FDecSeparator = dsColon) or (FDecSeparator = dsSemicolon) then begin
      SepTop := DigitTop + (Self.Height-4) div 3 - 2;
      Ellipse(SepLeft, SepTop, SepLeft+FLineWidth+1, SepTop+FLineWidth+1);
    end;

    case FDecSeparator of
      dsSemicolon   : SepTop := DigitTop + (Self.Height-4)*2 div 3 - 2;
      dsColon,
      dsPoint       : SepTop := DigitTop + FDigitHeight - FLineWidth - 1;
      dsComma       : SepTop := DigitTop + FDigitHeight - FLineWidth*2 - 1;
      dsApostrophe  : SepTop := DigitTop;
      else            SepTop := (Self.Height-FLineWidth) div 2;
    end;

    if (FDecSeparator = dsSemicolon) or
       (FDecSeparator = dsComma) or
       (FDecSeparator = dsApostrophe)
    then
      Polygon([
        Point(SepLeft+(FLineWidth div 2), SepTop+(FLineWidth div 2)),
        Point(SepLeft+FLineWidth, SepTop+(FLineWidth div 2)),
        Point(SepLeft+(FLineWidth div 2), SepTop+(FLineWidth * 2))
      ]);

    if (FDecSeparator = dsColon) or
       (FDecSeparator = dsPoint) or
       (FDecSeparator = dsSemicolon) or
       (FDecSeparator = dsComma) or
       (FDecSeparator = dsApostrophe)
    then
      Ellipse(SepLeft, SepTop, SepLeft+FLineWidth+1, SepTop+FLineWidth+1);

    if FDecSeparator = dsHyphen then begin
      SepWidth := round((DigitSpace-FDigitWidth)/3*2);
      SepLeft := DigitLeft-(DigitSpace-FDigitWidth)+((DigitSpace-FDigitWidth-SepWidth) div 2);
      Rectangle(SepLeft, SepTop, SepLeft+SepWidth, SepTop+FLineWidth+1);
    end;
  end;
end;

procedure TLEDDisplay.SetAngle(newValue:integer);
begin
  if FAngle <> newValue then begin
    FAngle := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetBevelStyle(newValue: TPanelBevel);
begin
  if FBevelStyle <> newValue then begin
    FBevelStyle := newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetBorderStyle(newValue: TBorderStyle);
begin
  if FBorderStyle <> newValue then begin
    FBorderStyle := newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetColorBackGround(newValue: TColor);
begin
  if FColorBackGround <> NewValue then begin
    FColorBackGround := NewValue;
    FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetColorLED(newValue: TColor);
begin
  if FColorLED <> newValue then begin
    FColorLED := newValue;
    FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDecSeparator(newValue: TDecSeparator);
begin
  if FDecSeparator <> newValue then begin
    FDecSeparator := newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDigitHeight(newValue: integer);
begin
  if FDigitHeight <> newValue then begin
    FDigitHeight := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDigitWidth(newValue: integer);
begin
  if FDigitWidth <> newValue then begin
    FDigitWidth := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDrawDigitShapes(newValue: boolean);
begin
  if newValue<>FDrawDigitShapes then begin
    FDrawDigitShapes := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetFractionDigits(newValue: integer);
begin
  if FFractionDigits <> newValue then begin
    FFractionDigits := newValue;
    if FFractionDigits > FNumDigits-1 then
      FFractionDigits := FNumDigits-1;
    MakeDisplayText;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLeadingZeros(newValue: boolean);
begin
  if FLeadingZeros <> newValue then begin
    FLeadingZeros := newValue;
    MakeDisplayText;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLEDContrast(newContrast: TContrast);
begin
  if (FLEDContrast <> newContrast) and (newContrast >= 0) and (newContrast < 10)
  then begin
    FLEDContrast := newContrast;
    FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLineWidth(newValue: integer);
begin
  if FLineWidth <> newValue then begin
    FLineWidth := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetNumDigits(newValue: integer);
begin
  if FNumDigits <> newValue then begin
    FNumDigits := newValue;
    MakeDisplayText;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetSegmentStyle(newValue: TSegmentStyle);
begin
  if FSegmentStyle <> newValue then begin
    FSegmentStyle := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetText(const newValue:string);
var
  i : integer;
begin
  SetLength(FDisplayText, FNumDigits);
  i := 1;
  while (i <= Length(newValue)) and (i <= FNumDigits) do begin
    if i <= Length(newValue) then begin
      if newValue[i] in ['0'..'9', '-', 'L', 'O', ' '] then
        FDisplayText[i] := newValue[i]
      else
        FDisplaytext[i] := '-';
    end else
      FDisplayText[i] := ' ';
    inc(i);
  end;

{
  FDisplayText := copy(newValue, 1, FNumDigits);
  for i:=1 to Length(FDisplayText) do
    if not (FDisplayText[i] in ['0'..'9', '-', 'L', 'O', ' ']) then
      FDisplayText[i] := '-'
      }
  while Length(FDisplayText) < FNumDigits do
    FDisplaytext := FDisplaytext + ' ';
  CreateDigitBitmaps;
  Invalidate;
  Change;
end;

procedure TLEDDisplay.SetValue(newValue: extended);
begin
  if (FValue <> NewValue) then begin
    FValue := NewValue;
    CreateDigitBitmaps;
    MakeDisplayText;
    Invalidate;
    Change;
  end;
end;

end.

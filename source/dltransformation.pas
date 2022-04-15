unit dlTransformation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, IniFiles, fpexprpars,
  dlGlobal, dlUtils;

type
  TTransformationParser = class(TFPExpressionParser)
  private
    FVariable: TFPExprIdentifierDef;
  public
    constructor Create(AOwner:TComponent); override;
    property Variable: TFPExprIdentifierDef read FVariable;
  end;
  
  TTransformation = class
  private
    FTransformationName: String;
    FMeasName: String;
    FMeasUnits: String;
    FMaxIn: Double;
    FMinIn: Double;
    FMaxOut: Double;
    FMinOut: Double;
    FLogarithmic: Boolean;
    FParser: TTransformationParser;
    FParserVariable: String;
    function GetExpression: String;
    procedure SetExpression(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    function Transform(AValue: Double; out AFlag: TTransformFlag): Double;
    property TransformationName: String read FTransformationName write FTransformationName;
    property Expression: String read GetExpression write SetExpression;
    property MeasName: String read FMeasName write FMeasName;
    property MeasUnits: String read FMeasUnits write FMeasUnits;
    property MaxIn: Double read FMaxIn write FMaxIn;
    property MaxOut: Double read FMaxOut write FMaxOut;
    property MinIn: Double read FMinIn write FMinIn;
    property MinOut: Double read FMinOut write FMinOut;
    property Logarithmic: Boolean read FLogarithmic write FLogarithmic;
  end;
      
  TTransformationList = class(TFPObjectList)
  private
    function GetItem(AIndex: Integer): TTransformation;
    procedure SetItem(AIndex: Integer; AValue: TTransformation);
  public
    function Find(const ATransformationName: String): TTransformation;
    function IndexOf(const ATransformationName: String): integer;
    procedure ReadFromIni(ini: TCustomIniFile);
    procedure WriteToIni(ini: TCustomIniFile);
    property Items[AIndex: Integer]: TTransformation read GetItem write SetItem; default;
  end;
  

var
  TransformationList: TTransformationList = nil;
  
  
implementation

uses
  Math;


{ TTransformationParser }

procedure tp_power(var Result:TFPExpressionResult; const Args:TExprParameterArray);
begin
  Result.resFloat := power(Args[0].resFloat, Args[1].resFloat);
end;

constructor TTransformationParser.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  BuiltIns := [bcMath, bcBoolean];
  Identifiers.AddFunction('power', 'F', 'F', @tp_power);
  FVariable := Identifiers.AddFloatVariable('x', 1.0);
end;


{ TTransformation }

constructor TTransformation.Create;
begin
  inherited;
  FMaxIn := NaN;
  FMinIn := NaN;
  FMaxOut := NaN;
  FMinOut := NaN;
  FParser := TTransformationParser.Create(nil);
end;

destructor TTransformation.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TTransformation.GetExpression: String;
begin
  Result := FParser.Expression;
end;

function TTransformation.Transform(AValue: Double; out AFlag: TTransformFlag): Double;
var
  res : TFPExpressionResult;
begin
  AFlag := tfOK;
  if IsEmptyNumber(AValue) then
    result := AValue
  else
  if (GetExpression = '') then
    result := AValue
  else
  begin
    if not IsEmptyNumber(FMaxIn) and (AValue > FMaxIn) then begin
      AFlag := tfTooLargeIn;
      AValue := FMaxIn;
    end
    else
    if not IsEmptyNumber(FMinIn) and (AValue < FMinIn) then begin
      AFlag := tfTooSmallIn;
      AValue := FMinIn;
    end;
    
    try
      FParser.Variable.AsFloat := AValue;
      res := FParser.Evaluate;
      result := res.ResFloat;
      if not IsEmptyNumber(FMaxOut) and (Result > FMaxOut) then begin
        Result := FMaxOut;
        AFlag := tfTooLargeOut;
      end else
      if not IsEmptyNumber(FMinOut) and (Result < FMinOut) then begin
        Result := FMinOut;
        AFlag := tfTooSmallOut;
      end;
    except
      Result := NaN;
      AFlag := tfError;
    end;
  end;
end;

procedure TTransformation.SetExpression(const AValue: String);
begin
  if AValue = GetExpression then 
    exit;
  FParser.Expression := AValue;
end;


{ TTransformationList }

function TTransformationList.Find(const ATransformationName: String): TTransformation;
var
  idx: Integer;
begin
  idx := IndexOf(ATransformationName);
  if idx > -1 then
    Result := Items[idx]
  else
    Result := nil;
end;

function TTransformationList.GetItem(AIndex: Integer): TTransformation;
begin
  Result := TTransformation(inherited Items[AIndex]);
end;

function TTransformationList.IndexOf(const ATransformationName: String): Integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result].TransformationName = ATransformationName then
      exit;
  Result := -1;
end;

procedure TTransformationList.ReadFromIni(ini: TCustomIniFile);
var
  L, LTrans: TStringList;
  i : integer;
  T : TTransformation;
  s : string;
  sf : string;
  mx, mn : double;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  Clear;
  
  L := TStringList.Create;
  LTrans := TStringList.Create;
  try
    LTrans.Delimiter := ';';
    ini.ReadSection('Transformations', L);
    for i:=0 to L.Count-1 do begin
      s := TrimBrackets(Trim(ini.ReadString('Transformations', L[i], '')));
      if s <> '' then begin
        LTrans.DelimitedText := trim(s);
        T := TTransformation.Create;
        T.TransformationName := L[i];
        T.MeasName := LTrans.Values['MeasName'];
        T.MeasUnits := LTrans.Values['MeasUnits'];
        T.Expression := LTrans.Values['Expression'];
        T.Logarithmic := SameText(LTrans.Values['Logarithmic'], 'true');
        sf := LTrans.Values['MaxIn'];
        if sf <> '' then mx := StrToFloat(sf, fs) else mx := nan;
        sf := LTrans.Values['MinIn'];
        if sf <> '' then mn := StrToFloat(sf, fs) else mn := nan;
        PutInOrder(mn, mx);
        T.MaxIn := mx;
        T.MinIn := mn;
        sf := LTrans.Values['MaxOut'];
        if sf <> '' then mx := StrToFloat(sf, fs) else mx := nan;
        sf := LTrans.Values['MinOut'];
        if sf <> '' then mn := StrToFloat(sf, fs) else mn := nan;
        PutInOrder(mn, mx);
        T.MaxOut := mx;
        T.MinOut := mn;
        
        Add(T);
      end;
    end;
  finally
    Ltrans.Free;
    L.Free;
  end;
end;

procedure TTransformationList.WriteToIni(ini: TCustomIniFile);
var
  i: integer;
  T: TTransformation;
  minIn, maxIn: string;
  minOut, maxOut: string;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini.EraseSection('Transformations');
  for i:=0 to Count-1 do begin
    T := Items[i];
    if T <> nil then begin
      if IsEmptyNumber(T.MinIn) then
        minIn := ''
      else
        minIn := Format('%0.4e', [T.MinIn], fs);
      if IsEmptyNumber(T.MaxIn) then
        maxIn := ''
      else
        maxIn := Format('%0.4e', [T.MaxIn], fs);
      if IsEmptyNumber(T.MinOut) then
        minOut:= ''
      else
        minOut := Format('%0.4e', [T.MinOut], fs);
      if IsEmptyNumber(T.MaxOut) then
        maxOut := ''
      else
        maxOut := Format('%0.4e', [T.MaxOut], fs);
      ini.WriteString(
        'Transformations',
        T.TransformationName,
        Format(
          '("MeasName=%s"; "MeasUnits=%s"; "Expression=%s"; '+
            '"Logarithmic=%s"; ' +
            'MinIn=%s; MaxIn=%s; MinOut=%s; MaxOut=%s)',
          [T.MeasName, T.MeasUnits, T.Expression,
           BoolToStr(T.Logarithmic, true),
           minIn, maxIn, minOut, maxOut],
          fs
        )
      );
    end;
  end;
end;

procedure TTransformationList.SetItem(AIndex: Integer; AValue: TTransformation);
begin
  inherited Items[AIndex] := AValue;
end;


end.


unit dlTimeOffsetDialog;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Spin, ComCtrls, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, TACustomSeries,
  dlListViewDialog;

type
  TSeriesEvent = procedure (Sender:TObject; Series:TChartSeries) of object;

  { TTimeOffsetForm }
  TTimeOffsetForm = class(TListViewForm)
    EdOffset:TFloatSpinEdit;
    LblTimeOffset:TLabel;
    LblTimeOffsetUnits:TLabel;
    procedure EdOffsetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewSelectItem(Sender:TObject; Item:TListItem; Selected:Boolean);

  private
    { private declarations }
    FApplyOffsetEvent : TSeriesEvent;
    procedure UpdateSeries;

  public
    { public declarations }
    property OnApplyOffsetToSeries : TSeriesEvent read FApplyOffsetEvent write FApplyOffsetEvent;
  end;

var
  TimeOffsetForm: TTimeOffsetForm;

implementation

{$R *.lfm}

uses
  dlGlobal, dlData;


{ TTimeOffsetForm }

procedure TTimeOffsetForm.EdOffsetChange(Sender: TObject);
begin
  UpdateSeries;
end;


procedure TTimeOffsetForm.FormCreate(Sender: TObject);
begin
  case MeasSettings.TimeUnits of
    tuSeconds  : LblTimeOffsetUnits.Caption := 'sec';
    tuMinutes  : LblTimeOffsetUnits.Caption := 'min';
    tuHours    : LblTimeOffsetUnits.Caption := 'hours';
    else         raise Exception.Create('Unsupported time units.');
  end;
end;


procedure TTimeOffsetForm.FormShow(Sender: TObject);
// workaround for a range check error if assigned at design-time
begin
  ListView.SmallImages := ImageList;
end;


procedure TTimeOffsetForm.ListViewSelectItem(Sender:TObject; Item:TListItem;
  Selected:Boolean);
begin
  if Selected then begin
    EdOffset.Value := TDataList(Item.Data).TimeOffset;
    UpdateSeries;
  end;
end;


procedure TTimeOffsetForm.UpdateSeries;
var
  item : TDataList;
begin
  item := TDataList(ListView.Selected.Data);
  if (item <> nil) and (FApplyOffsetEvent <> nil) then begin
    item.TimeOffset := EdOffset.Value;
    ListView.Selected.SubItems[0] := EdOffset.Text;
    FApplyOffsetEvent(self, item.Series);
  end;
end;


end.


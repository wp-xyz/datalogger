unit dlRemoveCurveDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, StdCtrls, dlListViewDialog;

type

  { TRemoveCurvesForm }

  TRemoveCurvesForm = class(TListviewForm)
    CbReassignLineColors: TCheckBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  RemoveCurvesForm: TRemoveCurvesForm;

implementation

{$R *.lfm}

end.


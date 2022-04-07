program datalogger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, laz_fpspreadsheet, dlMain, dlSerialDevice,
  VC820device, VC830Device, dlGlobal, dlUtils, dlData, dlTransformations,
dlTransformationEditor, mrumanager, dlTimeOffsetDialog, dlListViewDialog,
dlRemoveCurveDialog, dlMeasSettingsDialog, dlseriesstyleeditor
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.


unit dlListViewDialog;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ComCtrls, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  { TListViewForm }
  TListViewForm = class(TForm)
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    ImageList: TImageList;
    ListView: TListView;
    Panel_ListView: TPanel;
    Panel_Buttons: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ListViewForm: TListViewForm;

implementation

{$R *.lfm}

{ TListViewForm }

procedure TListViewForm.FormShow(Sender: TObject);
// workaround for a range check error if assigned at design-time
begin
  Listview.SmallImages := ImageList;
end;


end.


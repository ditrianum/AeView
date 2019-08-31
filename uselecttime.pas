unit uSelectTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Math,
  DateUtils, uAeView;

type
  EDateException = Class(Exception);

  { TfrmSelectTime }

  TfrmSelectTime = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    cbTimeFrame: TComboBox;
    edEndTime: TEdit;
    edStartTime: TEdit;
    lbTimeFrame: TLabel;
    lbEndTime: TLabel;
    lbStartTime: TLabel;
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure cbTimeFrameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    StartTime: TDateTime;
    EndTime: TDateTime;
    procedure UpdateTimeFrameStatus;
  end;

var
  frmSelectTime: TfrmSelectTime;
  AbortClose: boolean;
  DateException: EDateException;

implementation

{$R *.lfm}

{ TfrmSelectTime }

procedure TfrmSelectTime.UpdateTimeFrameStatus;
// adjust interface to user selection
var
  custom: boolean;
begin
  custom := false;

  edStartTime.Caption := FormatDateTime('YYYY-MM-DD', StartTime);
  if EndTime <> 0 then
    edEndTime.Caption := FormatDateTime('YYYY-MM-DD', EndTime)
  else
    edEndTime.Caption := '';

  if cbTimeFrame.ItemIndex = 7 then
    custom := true;

  lbStartTime.Enabled := custom;
  lbEndTime.Enabled := custom;
  edStartTime.Enabled := custom;
  edEndTime.Enabled := custom;
end;

procedure TfrmSelectTime.FormCreate(Sender: TObject);
begin
  DateException := EDateException.Create('Invalid date.');

  cbTimeFrame.Items.Add('Last 24 hours');
  cbTimeFrame.Items.Add('Last 7 days');
  cbTimeFrame.Items.Add('Last 15 days');
  cbTimeFrame.Items.Add('Last 30 days');
  cbTimeFrame.Items.Add('Last 3 months');
  cbTimeFrame.Items.Add('Last 6 months');
  cbTimeFrame.Items.Add('Last 12 months');
  cbTimeFrame.Items.Add('Custom...');
  cbTimeFrame.ItemIndex := 0;
end;

procedure TfrmSelectTime.FormShow(Sender: TObject);
begin
  UpdateTimeFrameStatus;
end;

procedure TfrmSelectTime.cbTimeFrameChange(Sender: TObject);
begin
  if cbTimeFrame.ItemIndex < 7 then
    GetPredefinedTime(cbTimeFrame.ItemIndex, StartTime, EndTime);

  UpdateTimeFrameStatus;
end;

procedure TfrmSelectTime.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not AbortClose;
  AbortClose := false;
end;

procedure TfrmSelectTime.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSelectTime.btOKClick(Sender: TObject);
var
  vr: TValueRelationship;
begin
  if cbTimeFrame.ItemIndex = 7 then
    // check start / end date of custom time frame
    begin
      // check start date
      try
        StartTime := ScanDateTime('yyyy-mm-dd', edStartTime.Text);
        vr := CompareDate(StartTime, Epoch);
        if vr < 0 then
          Raise DateException;
        vr := CompareDate(StartTime, LiveTime(1));
        if vr > 0 then
          Raise DateException;
      except
        ShowMessage('Invalid start date.');
        AbortClose := true;
        exit;
      end;
      // check end date
      try
        EndTime := ScanDateTime('yyyy-mm-dd', edEndTime.Text);
        EndTime := IncHour(EndTime, 23);
        EndTime := IncMinute(EndTime, 59);
        EndTime := IncSecond(EndTime, 59);
        vr := CompareDate(EndTime, StartTime);
        if vr < 0 then
          Raise DateException;
        vr := CompareDate(EndTime, LiveTime);
        if vr > 0 then
          Raise DateException;
      except
        ShowMessage('Invalid end date.');
        AbortClose := true;
        exit;
      end;
    end;
end;

end.


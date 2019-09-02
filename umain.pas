{
 	Atmospheric Electricity Viewer

	by Frank Hoogerbeets 2019-08-27 <frank@ditrianum.org>

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston,
	MA  02111-1307  USA
}

unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  TAGraph, TASeries, TAIntervalSources, DateUtils, TATools,
  TAChartAxisUtils, TAChartUtils, LCLIntf, LCLType, Types,
  uSettings, uSelectTime, uAeview;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    btClose: TButton;
    btRefresh: TButton;
    btSettings: TButton;
    btScreenshot: TButton;
    btSelectTime: TButton;
    Chart1: TChart;
    Chart1ConstantLine1383: TConstantLine;
    Chart1ConstantLine1432: TConstantLine;
    Chart1ConstantLine1388: TConstantLine;
    Chart1ConstantLine1427: TConstantLine;
    Chart1ConstantLine1393: TConstantLine;
    Chart1ConstantLine1422: TConstantLine;
    Chart1ConstantLine1398: TConstantLine;
    Chart1ConstantLine1403: TConstantLine;
    Chart1ConstantLine1408: TConstantLine;
    Chart1ConstantLine1413: TConstantLine;
    Chart1ConstantLine1417: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartZoomOutWheel: TZoomMouseWheelTool;
    ChartZoomInWheel: TZoomMouseWheelTool;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    lbSelectedTimeValue: TLabel;
    lbSelectedTime: TLabel;
    lbSourceName: TLabel;
    SaveDialog1: TSaveDialog;
    procedure btCloseClick(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure btScreenshotClick(Sender: TObject);
    procedure btSelectTimeClick(Sender: TObject);
    procedure btSettingsClick(Sender: TObject);
    procedure Chart1ExtentChanged(ASender: TChart);
    procedure FormCreate(Sender: TObject);
  private
    procedure GetXAxisRange(Chart: TChart; out FirstX, LastX: double);
    procedure LoadChart();
  public
    DateTimeViewStart: Double;
    DateTimeViewEnd: Double;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Epoch := EncodeDate(2010, 01, 01);

  DefaultFormatSettings.DecimalSeparator := '.';
  Application.UpdateFormatSettings := false;

  SaveDialog1.Options := [ofOverwritePrompt];

  Settings.ConfigFile := 'aeview.cfg';
  LoadSettings();

  {
    If the recorded data show obvious 4 and 5-step millivolt levels then the
    following constant lines can be used to mark them, but their position
    may be different depending on the recording device.
  }
  //Chart1ConstantLine1383.Active := false;
  //Chart1ConstantLine1388.Active := false;
  //Chart1ConstantLine1393.Active := false;
  //Chart1ConstantLine1398.Active := false;
  //Chart1ConstantLine1403.Active := false;
  //Chart1ConstantLine1408.Active := false;
  //Chart1ConstantLine1413.Active := false;
  //Chart1ConstantLine1417.Active := false;
  //Chart1ConstantLine1422.Active := false;
  //Chart1ConstantLine1427.Active := false;
  //Chart1ConstantLine1432.Active := false;

  // last 24 hours LiveTime
  SelectedTime.TimeFrame := 1;

  LoadData();
  LoadChart();
end;

procedure TfrmMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btRefreshClick(Sender: TObject);
begin
  LoadData;
  LoadChart;
  Chart1.Refresh;
end;

procedure TfrmMain.btScreenshotClick(Sender: TObject);
var
  b1, b2: TBitmap;
  r1, r2: TRect;
  dleft, dtop, dwidth, dheight: integer;
begin
  dleft := (Left + 10) + Settings.ScreenshotLeft;
  dtop := (Top + 35) + Settings.ScreenshotTop;
  dwidth := (Width + 5) + Settings.ScreenshotWidth;
  dheight := (Height - 15) + Settings.ScreenshotHeight;

  r1 := Rect(dleft, dtop, Left + dwidth, Top + dheight);
  b1 := GetScreenshot;


  b2 := TBitmap.Create;
  b2.SetSize(dwidth, dheight);

  r2 := Rect(0, 0, dwidth, dheight);
  b2.Canvas.CopyRect(r2, b1.Canvas, r1);

  SaveDialog1.InitialDir := ExtractFilePath(ScreenShotFile);
  SaveDialog1.Filename := 'aeview-img-' + TimeStamp + '.bmp';
  if SaveDialog1.Execute then
    begin
      ScreenshotFile := SaveDialog1.Filename;
      b2.SaveToFile(ScreenShotFile);
      SaveAsJPEG();
    end;

  b1.Free;
  b2.Free;
end;

procedure TfrmMain.btSelectTimeClick(Sender: TObject);
var
  f: TfrmSelectTime;
begin
  f := TfrmSelectTime.Create(frmMain);
  f.StartTime := SelectedTime.StartTime;
  f.EndTime := SelectedTime.EndTime;
  f.cbTimeFrame.ItemIndex := SelectedTime.TimeFrame;
  if f.ShowModal = mrOK then
    begin
      SelectedTime.TimeFrame := f.cbTimeFrame.ItemIndex;
      if SelectedTime.TimeFrame < 3 then
        // LiveTime
        SelectedTime.CanRefresh := true
      else
        begin
          SelectedTime.StartTime := f.StartTime;
          SelectedTime.EndTime := f.EndTime;
          SelectedTime.CanRefresh := false;
        end;
      LoadData();
      LoadChart();
      btRefresh.Enabled := SelectedTime.CanRefresh;
    end;
  f.Free;
end;

procedure TfrmMain.btSettingsClick(Sender: TObject);
var
  f: TfrmSettings;
  //s: string;
  //reload: boolean;
begin
  //reload := false;
  f := TfrmSettings.Create(frmMain);
  //f.edDataSource.Text := Settings.DataSource;
  f.edStationID.Text := Settings.StationID;
  f.edSourceID.Text := Settings.SourceID;
  f.edLineColor.Text := ColorToString(Settings.LineColor);
  f.edScreenshotLeft.Text := IntToStr(Settings.ScreenshotLeft);
  f.edScreenshotTop.Text := IntToStr(Settings.ScreenshotTop);
  f.edScreenshotWidth.Text := IntToStr(Settings.ScreenshotWidth);
  f.edScreenshotHeight.Text := IntToStr(Settings.ScreenshotHeight);
  if f.ShowModal = mrOK then
    begin
      //s := trim(f.edDataSource.Text);
      //if s <> Settings.DataSource then
      //  begin
      //    Settings.DataSource := s;
      //    reload := true;
      //  end;
      Settings.StationID := trim(f.edStationID.Text);
      Settings.SourceID := trim(f.edSourceID.Text);
      Settings.LineColor := StringToColor(f.edLineColor.Text);
      Settings.ScreenshotLeft := StrToInt(f.edScreenshotLeft.Text);
      Settings.ScreenshotTop := StrToInt(f.edScreenshotTop.Text);
      Settings.ScreenshotWidth := StrToInt(f.edScreenshotWidth.Text);
      Settings.ScreenshotHeight := StrToInt(f.edScreenshotHeight.Text);
      if SaveSettings() then
        begin
          // (re)load
          // if reload then
            // LoadFile();
          LoadChart();
          Chart1.Refresh;
        end;
    end;
  f.Free;
end;

procedure TfrmMain.Chart1ExtentChanged(ASender: TChart);
begin
  GetXAxisRange(ASender, DateTimeViewStart, DateTimeViewEnd);
  lbSelectedTimeValue.Caption :=
    FormatDateTime('YYYY-MM-DD, hh:nn', DateTimeViewStart)
    + ' - '
    + FormatDateTime('YYYY-MM-DD, hh:nn', DateTimeViewEnd);
end;

procedure TfrmMain.GetXAxisRange(Chart: TChart; out FirstX, LastX: double);
var
  ex: TDoubleRect;  // requires TAChartUtils in "uses"
begin
  ex := Chart.CurrentExtent;
  FirstX := ex.a.x;
  LastX := ex.b.x;
end;

procedure TfrmMain.LoadChart();
var
  i: integer;
begin
  Chart1LineSeries1.Clear;
  Chart1.Title.Text.Clear;

  Chart1LineSeries1.SeriesColor := Settings.LineColor;

  for i := 0 to DataCount do
    begin
      Chart1LineSeries1.AddXY(AeData[i].Serial, AeData[i].Voltage);
      // prevent double memory usage
      AeData[i] := Default(TAeData);
    end;

  ResetDataCount;

  Chart1.Title.Text.AddText(Settings.StationID);
  lbSourceName.Caption := Settings.SourceID;
end;

end.


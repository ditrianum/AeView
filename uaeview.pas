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

unit uAeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, StrUtils, DateUtils, LazSysUtils,
  LCLIntf, LCLType, Math;

const
  DefaultLineColor = $00FF9F2F;
  ArchiveFolder = 'archive';
  kMaxLines = 1319999;

type
  TAeData = record
    Date: string;
    Time: string;
    Serial: TDatetime;
    Value: string;
    Voltage: extended;
    IsValid: boolean;
  end;

  TSelectedTime = record
    TimeFrame: integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    CanRefresh: boolean;
  end;

  TSettings = record
    ConfigFile: string;
    //DataSource: string;
    StationID: string;
    SourceID: string;
    LineColor: TColor;
    ScreenshotLeft: integer;
    ScreenshotTop: integer;
    ScreenshotWidth: integer;
    ScreenshotHeight: integer;
  end;

var
  AeData: array[0..kMaxLines] of TAeData;
  Epoch: TDateTime;
  DataCount: integer;  // 0-based
  LiveTimeFile: string;
  ScreenshotFile: string;
  SelectedTime: TSelectedTime;
  Settings: TSettings;

function FileID(dt: TDateTime): string;
function GetAeData(const data: string): TAeData;
procedure GetPredefinedTime(const index: integer; var stime, etime: TDateTime);
function GetScreenshot: TBitmap;
function LiveTime(days: word = 0): TDateTime;
procedure LoadData;
procedure LoadSettings;
procedure ResetDataCount;
procedure SaveAsJPEG;
function SaveSettings: boolean;
function TimeStamp: string;
procedure UpdateLiveTime;

implementation

{ private }

function ArchivedFile(const aFile: string): string;
begin
  result := ArchiveFolder + DirectorySeparator + aFile;
end;

function IncDataFile(const aFile: string; const increment: integer = 1): string;
var
  y, m: word;
  dt: TDateTime;
begin
  result := RightStr(aFile, 11);
  y := StrToInt(LeftStr(result, 4));
  m := StrToInt(MidStr(result, 6, 2));
  dt := IncMonth(EncodeDate(y, m, 1), increment);
  result := FileID(dt);
end;

function NextDataFile(var aFile: string): boolean;
var
  cFile: string;
begin
  result := false;
  cFile := RightStr(aFile, 21);

  repeat
    cFile := IncDataFile(cFile);
    if cFile < LiveTimeFile then
      aFile := ArchivedFile(cFile)
    else if cFile = LiveTimeFile then
      aFile := cFile
    else
      break;
    result := FileExists(aFile);
  until (result = true);
end;

function ReadDataFile(aFile: string): boolean;
{ 
	returns false if an error occurred
	or no (more) matching dates were found
}
var
  tfin: TextFile;
  buffer: string;
  tmpAeData: TAeData;
  tmpAeDataBefore: TAeData;
  vr: TValueRelationship;
  lines: longword;
begin
  result := false;
  if aFile <> LiveTimeFile then
    result := true;

  lines := 0;
  tmpAeDataBefore := default(TAeData);

  AssignFile(tfin, aFile);
  try
    Reset(tfin);
    try
      try
        while not eof(tfin) do
          begin
            readln(tfin, buffer);
            inc(lines);
            if Length(Trim(buffer)) > 0 then
              begin
                tmpAeData := GetAeData(buffer);
                // check data valifity
                if not tmpAeData.IsValid then
                  begin
                    buffer := 'Warning, invalid data found in '
                      + aFile
                      + ' at line '
                      + IntToStr(lines);
                    ShowMessage(buffer);
                    result := false;
                    break;
                  end;
                // check date/time overlap, but skip equal date/time if nan
                vr := CompareDateTime(tmpAeDataBefore.Serial, tmpAeData.Serial);
                if (vr > 0) or ((vr = 0) and (IsNan(tmpAeData.Voltage) = false)) then
                  begin
                    buffer := 'Warning, date/time overlap in '
                      + aFile
                      + ' at line '
                      + IntToStr(lines);
                    ShowMessage(buffer);
                    result := false;
                    break;
                  end;
                tmpAeDataBefore := tmpAeData;
                // check date/time match
                vr := CompareDateTime(tmpAeData.Serial, SelectedTime.StartTime);
                if vr >= 0 then
                  begin
                    vr := CompareDateTime(tmpAeData.Serial, SelectedTime.EndTime);
                    if vr > 0 then
                      // reached the end
                      begin
                        result := false;
                        break;
                      end;
                    DataCount := DataCount + 1;
                    AeData[DataCount] := tmpAeData;
                    if DataCount = kMaxLines then
                      // reached maximum
                      begin
                        buffer := 'Warning, reached data limit. Loading file(s) aborted.';
                        ShowMessage(buffer);
                        result := false;
                        break;
                      end;
                  end;
              end;
          end;
      except
        on E: EInOutError do
          begin
            buffer := E.Message; // + LineEnding + LineEnding + Settings.DataSource;
            ShowMessage(buffer);
            result := false;
          end;
      end;
    finally
      CloseFile(tfin);
    end;
  except
    ShowMessage('Error opening data file ' + aFile);
    result := false;
  end;
end;

procedure ReadSettings(var tf: TextFile);
var
  buffer: string;
begin
  //readln(tf, Settings.DataSource);
  readln(tf, Settings.StationID);
  readln(tf, Settings.SourceID);

  readln(tf, buffer);
  buffer := trim(buffer);
  if length(buffer) = 0 then
    buffer := ColorToString(DefaultLineColor);
  Settings.LineColor := StringToColor(buffer);

  readln(tf, buffer);
  buffer := trim(buffer);
  if length(buffer) = 0 then
    buffer := '0';
  Settings.ScreenshotLeft := StrToInt(buffer);

  readln(tf, buffer);
  buffer := trim(buffer);
  if length(buffer) = 0 then
    buffer := '0';
  Settings.ScreenshotTop := StrToInt(buffer);

  readln(tf, buffer);
  buffer := trim(buffer);
  if length(buffer) = 0 then
    buffer := '0';
  Settings.ScreenshotWidth := StrToInt(buffer);

  readln(tf, buffer);
  buffer := trim(buffer);
  if length(buffer) = 0 then
    buffer := '0';
  Settings.ScreenshotHeight := StrToInt(buffer);
end;

procedure WriteSettings(var tf: TextFile);
begin
  //writeln(tf, Settings.DataSource);
  writeln(tf, Settings.StationID);
  writeln(tf, Settings.SourceID);
  writeln(tf, ColorToString(Settings.LineColor));
  writeln(tf, IntToStr(Settings.ScreenshotLeft));
  writeln(tf, IntToStr(Settings.ScreenshotTop));
  writeln(tf, IntToStr(Settings.ScreenshotWidth));
  writeln(tf, IntToStr(Settings.ScreenshotHeight));
end;

{ public }

function FileID(dt: TDateTime): string;
begin
  result := 'aelog-utc-'
    + FormatDateTime('YYYY-MM', dt)
    + '.csv'
end;

function GetAeData(const data: string): TAeData;
var
  y,m,d,h,n,s: word;
  value: string;
begin
  result := Default(TAeData);
  result.IsValid := false; // assume

  value := Trim(MidStr(data, 21, 12));
  if Length(value) = 0 then
    value := 'Nan';

  result.Value := value;

  try
    result.Voltage := StrToFloat(value);
  except
    exit;
  end;

  result.Date := LeftStr(data, 10);
  result.Time := MidStr(data, 12, 8);

  y := StrToInt(LeftStr(result.Date, 4));
  m := StrToInt(MidStr(result.Date, 6, 2));
  d := StrToInt(MidStr(result.Date, 9, 2));
  h := StrToInt(LeftStr(result.Time, 2));
  n := StrToInt(MidStr(result.Time, 4, 2));
  s := StrToInt(MidStr(result.Time, 7, 2));

  if y < YearOf(Epoch) then
    exit;

  if not IsValidDate(y, m, d) then
    exit;

  if not IsValidTime(h, n, s, 0) then
    exit;

  result.Serial := EncodeDateTime(y, m, d, h, n, s, 0);
  result.IsValid := true;
end;

procedure GetPredefinedTime(const index: integer; var stime, etime: TDateTime);
var
  lt: TDateTime;
begin
  lt := LiveTime;
  case index of
    0: // 24 hours
      begin
        stime := LiveTime(1);
        etime := lt;
      end;
    1: // 7 days
      begin
        stime := LiveTime(7);
        etime := lt;
      end;
    2: // 15 days
      begin
        stime := LiveTime(15);
        etime := lt;
      end;
    3: // last 30 days
      begin
        etime := lt;
        stime := etime - 30;
      end;
    4: // last three months
      begin
        etime := lt;
        stime := etime - 92;
      end;
    5: // last six months
      begin
        etime := lt;
        stime := etime - 183;
      end;
    6: // last 12 months
      begin
        etime := lt;
        stime := etime - 365;
      end;
  end;
end;

function GetScreenshot(): TBitmap;
var
  screenDC: HDC;
begin
  screenDC := GetDC(0);
  try
    result := TBitmap.Create;
    result.LoadFromDevice(screenDC);
  finally
    ReleaseDC(0, screenDC);
  end;
end;

function LiveTime(days: word = 0): TDateTime;
begin
  result := NowUTC - days;
end;

procedure LoadData();
var
  aFile, cFile: string;
  continue: boolean;
begin
  ResetDataCount;

  if SelectedTime.TimeFrame < 3 then
    UpdateLiveTime;

  {
    This should be the current file. In a worst case scenario there will be
    up to 26 seconds delay after midnight of the new month before AeLog has
    updated the archive.
  }

  LiveTimeFile := FileID(LiveTime);
  cFile := FileID(SelectedTime.StartTime);

  if cFile = LiveTimeFile then
    begin
      if FileExists(cFile) then
        ReadDataFile(cFile);
      exit;
    end;

  aFile := IncDataFile(cFile, -1);

  repeat
    continue := NextDataFile(aFile);
    if continue then
      continue := ReadDataFile(aFile);
  until (continue = false);
end;

procedure LoadSettings();
var
  tfile: TextFile;
  message: string;
  error: boolean;
begin
  error := true;

  // fname := GetAppConfigFile(false);
  AssignFile(tfile, Settings.ConfigFile);
  if FileExists(Settings.ConfigFile) then
    begin
      // read settings
      try
        Reset(tfile);
        try
          try
            ReadSettings(tfile);
            error := false;
          except
            on E: EInOutError do
              begin
                message := E.Message + LineEnding + LineEnding
                  + Settings.ConfigFile;
                ShowMessage(message);
              end;
          end;
        finally
          CloseFile(tfile);
        end;
      except
        ShowMessage('Error opening settings file.' + LineEnding
          + LineEnding + 'Loading default settings instead.');
      end;
    end;

  if not error then
    exit;

  // default settings
  with Settings do
    begin
      //DataSource := 'aelog.csv';
      StationID := 'Your Station';
      SourceID := 'source: your source';
      LineColor := DefaultLineColor;
      ScreenshotLeft := 0;
      ScreenshotTop := 0;
      ScreenshotWidth := 0;
      ScreenshotHeight := 0;
    end;

  // write settings
  try
    Rewrite(tfile);
    try
      try
        WriteSettings(tfile);
      except
        on E: EInOutError do
          begin
            message := E.Message + LineEnding + LineEnding
              + Settings.ConfigFile;
            ShowMessage(message);
          end;
      end;
    finally
      CloseFile(tfile);
    end;
  except
    message := 'Error creating settings file.' + LineEnding
      + LineEnding + 'Default settings are not saved.';
    ShowMessage(message);
  end;
end;

procedure ResetDataCount;
begin
  DataCount := -1;
end;

procedure SaveAsJPEG();
var
  pic: TPicture;
  jpg: TJPEGImage;
begin
  pic := TPicture.Create;
  jpg := TJPEGImage.Create;

  try
    pic.LoadFromFile(ScreenshotFile);
    jpg.Assign(pic.Bitmap);
    jpg.CompressionQuality := 90;
    jpg.SaveToFile(ChangeFileExt(ScreenshotFile, '.jpg'));
  finally
    FreeAndNil(jpg);
    FreeAndNil(pic);
  end;
end;

function SaveSettings(): boolean;
var
  tfile: TextFile;
  buffer: string;
begin
  result := false;
  // fname := GetAppConfigFile(false);
  AssignFile(tfile, Settings.ConfigFile);
  try
    Rewrite(tfile);
    try
      try
        WriteSettings(tfile);
        result := true;
      except
        on E: EInOutError do
          begin
            buffer := E.Message + LineEnding + LineEnding
              + Settings.ConfigFile;
            ShowMessage(buffer);
          end;
      end;
    finally
      CloseFile(tfile);
    end;
  except
    ShowMessage('Error opening settings file.' + LineEnding + LineEnding +
      'Settings are not saved.');
  end;
end;

function TimeStamp(): string;
begin
  result := FormatDateTime('YYYY-MM-DD-hh-nn-ss', LiveTime);
end;

procedure UpdateLiveTime;
begin
  with SelectedTime do
    begin
      GetPredefinedTime(TimeFrame, StartTime, EndTime);
    end;
end;

end.


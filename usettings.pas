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

unit uSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    btOK: TButton;
    btCancel: TButton;
    btChooseLineColor: TButton;
    ColorDialog1: TColorDialog;
    edScreenshotHeight: TEdit;
    edScreenshotWidth: TEdit;
    edScreenshotTop: TEdit;
    edScreenshotLeft: TEdit;
    edSourceID: TEdit;
    edLineColor: TEdit;
    edStationID: TEdit;
    lbScreenshotHeight: TLabel;
    lbScreenshotWidth: TLabel;
    lbScreenshotTop: TLabel;
    lbScreenshotInfo: TLabel;
    lbScreenshotLeft: TLabel;
    lbSourceID: TLabel;
    lbLineColor: TLabel;
    lbStationID: TLabel;
    procedure btChooseLineColorClick(Sender: TObject);
  private

  public

  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.btChooseLineColorClick(Sender: TObject);
begin
  ColorDialog1.Color := StringToColor(edLineColor.Text);
  if ColorDialog1.Execute then
    edLineColor.Text := ColorToString(ColorDialog1.Color);
end;

end.


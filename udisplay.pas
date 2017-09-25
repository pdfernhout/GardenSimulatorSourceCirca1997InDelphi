unit Udisplay;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udisplay: Display options form. Allows user to change various options related to
drawing in garden window and browser. Simple dialog, no interactions, just filling
at start (from domain) and setting when OK is clicked. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ugsform;

type
  TDisplayOptionsForm = class(GsForm)
    OK: TButton;
    Cancel: TButton;
    selectionWidthPanel: TPanel;
    label1: TLabel;
    plantProximityNeeded: TEdit;
    Label2: TLabel;
    helpButton: TBitBtn;
    gardenScalePanel: TPanel;
    scaleOverride: TRadioButton;
    scaleMultiply: TRadioButton;
    Label4: TLabel;
    scale: TEdit;
    scaleIgnore: TRadioButton;
    Label9: TLabel;
    gardenDrawPanel: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    sortPolygonsMainWindow: TCheckBox;
    drawLinesMainWindow: TCheckBox;
    browserDrawPanel: TPanel;
    Label7: TLabel;
    sortPolygonsBrowser: TCheckBox;
    drawLinesBrowser: TCheckBox;
    simpleLeavesMainWindow: TCheckBox;
    Label8: TLabel;
    straightLinesMainWindow: TCheckBox;
    wireFrameMainWindow: TCheckBox;
    Label10: TLabel;
    simpleLeavesBrowser: TCheckBox;
    straightLinesBrowser: TCheckBox;
    wireFrameBrowser: TCheckBox;
    undoLevelsPanel: TPanel;
    Label11: TLabel;
    undoLevelsEdit: TEdit;
    idNumberPanel: TPanel;
    Label12: TLabel;
    nextPlantIDNumber: TEdit;
    Label13: TLabel;
    nextSoilPatchIDNumber: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    redrawNeeded: boolean;
    redrawBrowserPlantNeeded: boolean;
  end;

var
  DisplayOptionsForm: TDisplayOptionsForm;

implementation

uses udomain, uegarden, ugsim;

{$R *.DFM}

procedure TDisplayOptionsForm.FormActivate(Sender: TObject);
	begin
	plantProximityNeeded.text := IntToStr(Domain.menuOptions.plantProximityNeeded);
	undoLevelsEdit.text := IntToStr(Domain.menuOptions.undoLimit);
	scale.text := FloatToStrF(Domain.garden.gardenScale, ffGeneral, 7, 0);
  nextSoilPatchIDNumber.text := IntToStr(Domain.garden.numSoilPatchesCreated + 1);
  nextPlantIDNumber.text := IntToStr(Domain.garden.numPlantsCreated + 1);
  if Domain.garden.gardenScaleEffect = kGardenScaleOverridesPatchScale then
    scaleOverride.checked := true
  else if Domain.garden.gardenScaleEffect = kGardenScaleIgnore then
    scaleIgnore.checked := true
  else
    scaleMultiply.checked := true;
  with Domain.menuOptions.mainWindowPlantDrawOptions do
    begin
    sortPolygonsMainWindow.checked := sortPolygons;
    drawLinesMainWindow.checked := drawLines;
    simpleLeavesMainWindow.checked := simpleLeavesOnly;
    straightLinesMainWindow.checked := straightLinesOnly;
    wireFrameMainWindow.checked := wireFrame;
   end;
  with Domain.menuOptions.browserPlantDrawOptions do
    begin
    sortPolygonsBrowser.checked := sortPolygons;
    drawLinesBrowser.checked := drawLines;
    simpleLeavesBrowser.checked := simpleLeavesOnly;
    straightLinesBrowser.checked := straightLinesOnly;
    wireFrameBrowser.checked := wireFrame;
   end;
  redrawNeeded := false;
  redrawBrowserPlantNeeded := false;
	end;

procedure TDisplayOptionsForm.OKClick(Sender: TObject);
  var
  	newProximity: longint;
    newScale: single;
    oldEffect: smallint;
    newUndoLimit: longint;
    newNextSoilPatchIDNumber: longint;
    newNextPlantIDNumber: longint;
	begin
  { undo levels }
  try
    newUndoLimit := StrToInt(undoLevelsEdit.text);
    if (newUndoLimit < 1) or (newUndoLimit > 1000) then
      begin
      ShowMessage('The number of undo levels must be between 1 and 1000.');
  	  modalResult := 0;
      exit;
      end;
  except
    ShowMessage('Improper value entered for the number of undo levels.');
    modalResult := 0;
    exit;
  end;
  { plant proximity }
  try
    newProximity := StrToInt(plantProximityNeeded.text);
    if (newProximity < 0) or (newProximity > 30) then
      begin
      ShowMessage('Size of selection rectangle at plant base must be between 0 and 30.');
  	  modalResult := 0;
      exit;
      end;
  except
    ShowMessage('Improper value entered for size of selection rectangle at plant base.');
    modalResult := 0;
    exit;
  end;
  { scale }
  try
    newScale := StrToFloat(scale.text);
    if (newScale <= 0.0) or (newScale > 3000) then
      begin
      ShowMessage('Garden scale must be greater than 0 and less than or equal 3000.');
  	  modalResult := 0;
      exit;
      end
  except
    ShowMessage('Improper value entered for garden scale');
    modalResult := 0;
    exit;
  end;
  { newNextSoilPatchIDNumber }
  try
    newNextSoilPatchIDNumber := StrToInt(nextSoilPatchIDNumber.text);
    if (newNextSoilPatchIDNumber < 1) or (newNextSoilPatchIDNumber > 2100000000) then
      begin
      ShowMessage('The next soil patch ID number must be between 1 and 2100000000.');
  	  modalResult := 0;
      exit;
      end;
  except
    ShowMessage('Improper value entered for the next soil patch ID number.');
    modalResult := 0;
    exit;
  end;
  { newNextPlantIDNumber }
  try
    newNextPlantIDNumber := StrToInt(nextPlantIDNumber.text);
    if (newNextPlantIDNumber < 1) or (newNextPlantIDNumber > 2100000000) then
      begin
      ShowMessage('The next plant ID number must be between 1 and 2100000000.');
  	  modalResult := 0;
      exit;
      end;
  except
    ShowMessage('Improper value entered for the next plant ID number.');
    modalResult := 0;
    exit;
  end;
  { if these all came through all right, set }
  Domain.menuOptions.undoLimit := newUndoLimit;
  GardenForm.commandList.setNewUndoLimit(newUndoLimit);
  Domain.menuOptions.plantProximityNeeded := newProximity;
  Domain.garden.numSoilPatchesCreated := newNextSoilPatchIDNumber - 1;
  Domain.garden.numPlantsCreated := newNextPlantIDNumber - 1;
  { decide if redraw is needed }
  if Domain.garden.gardenScale <> newScale then
  	redrawNeeded := true;
  Domain.garden.gardenScale := newScale;
  oldEffect := Domain.garden.gardenScaleEffect;
  if scaleMultiply.checked then
  	Domain.garden.gardenScaleEffect := kGardenScaleMultipliesPatchScale
  else if scaleIgnore.checked then
  	Domain.garden.gardenScaleEffect := kGardenScaleIgnore
  else
  	Domain.garden.gardenScaleEffect := kGardenScaleOverridesPatchScale;
  if Domain.garden.gardenScaleEffect <> oldEffect then
  	redrawNeeded := true;
  with Domain.menuOptions.mainWindowPlantDrawOptions do
    begin
    if sortPolygons <> sortPolygonsMainWindow.checked then
      begin
      redrawNeeded := true;
      sortPolygons := sortPolygonsMainWindow.checked;
      end;
    if drawLines <> drawLinesMainWindow.checked then
      begin
      redrawNeeded := true;
      drawLines := drawLinesMainWindow.checked;
      end;
    if simpleLeavesOnly <> simpleLeavesMainWindow.checked then
      begin
      redrawNeeded := true;
      simpleLeavesOnly := simpleLeavesMainWindow.checked;
      end;
    if straightLinesOnly <> straightLinesMainWindow.checked then
      begin
      redrawNeeded := true;
      straightLinesOnly := straightLinesMainWindow.checked;
      end;
    if wireFrame <> wireFrameMainWindow.checked then
      begin
      redrawNeeded := true;
      wireFrame := wireFrameMainWindow.checked;
      end;
    end;
  with Domain.menuOptions.browserPlantDrawOptions do
    begin
    if sortPolygons <> sortPolygonsBrowser.checked then
      begin
      redrawBrowserPlantNeeded := true;
      sortPolygons := sortPolygonsBrowser.checked;
      end;
    if drawLines <> drawLinesBrowser.checked then
      begin
      redrawBrowserPlantNeeded := true;
      drawLines := drawLinesBrowser.checked;
      end;
    if simpleLeavesOnly <> simpleLeavesBrowser.checked then
      begin
      redrawBrowserPlantNeeded := true;
      simpleLeavesOnly := simpleLeavesBrowser.checked;
      end;
    if straightLinesOnly <> straightLinesBrowser.checked then
      begin
      redrawBrowserPlantNeeded := true;
      straightLinesOnly := straightLinesBrowser.checked;
      end;
    if wireFrame <> wireFrameBrowser.checked then
      begin
      redrawBrowserPlantNeeded := true;
      wireFrame := wireFrameBrowser.checked;
      end;
    end;
  end;

procedure TDisplayOptionsForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Display_options')
  end;

end.

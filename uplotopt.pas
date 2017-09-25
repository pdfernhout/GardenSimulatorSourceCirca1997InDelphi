unit Uplotopt;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uplotopt: Plot options form. This form appears in two circumstances: 1) the user
graphed an aspect from the browser and the domain option to show the options window
at the time of graphing was turned on; and 2) the user clicked on one of several
columns in the graph list box on the graph window. The window shows several options
that can be changed for a loggedVar (see ulogvar for details). The window works
by making a copy of the loggedVar and updating the original if OK is pressed. The
aspect pointer is used only for reference and nothing there is changed.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Buttons, Dialogs, uaspects, ugroups, ulogvar, ugsform;

type
  TPlotOptionsForm = class(GsForm)
    ColorDialog: TColorDialog;
    scalePanel: TPanel;
    Label2: TLabel;
    scaleChoices: TComboBox;
    unitPanel: TPanel;
    Label5: TLabel;
    objectPanel: TPanel;
    ObjectLabel: TLabel;
    objectChoices: TComboBox;
    buttonsPanel: TPanel;
    showDialogAtChoice: TCheckBox;
    axisPanel: TPanel;
    Label4: TLabel;
    leftAxis: TRadioButton;
    rightAxis: TRadioButton;
    linePanel: TPanel;
    Label6: TLabel;
    Label1: TLabel;
    colorSwatch: TShape;
    Style: TComboBox;
    drawBars: TCheckBox;
    previewPanel: TPanel;
    lineImage: TImage;
    OK: TButton;
    Cancel: TButton;
    help: TBitBtn;
    aspectUnit: TEdit;
    aspectNameLabel: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure StyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure StyleChange(Sender: TObject);
    procedure ColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure colorSwatchMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CancelClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure leftAxisClick(Sender: TObject);
    procedure rightAxisClick(Sender: TObject);
    procedure scaleChoicesChange(Sender: TObject);
    procedure objectChoicesChange(Sender: TObject);
    procedure drawBarsClick(Sender: TObject);
    procedure aspectUnitMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure helpClick(Sender: TObject);
    procedure aspectUnitKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    loggedVarOriginal: GsLoggedVar;
    loggedVar: GsLoggedVar;
    aspect: GsAspect;
    procedure drawLineImage;
    procedure initialize(aLoggedVar: GsLoggedVar; anAspect: GsAspect; showAtChoice: boolean);
    procedure closeWithException(exceptionString: string);
    end;

implementation

uses uunits, udomain, umodel, usupport, ueutils;

const kBetweenGap = 4;

{$R *.DFM}

procedure TPlotOptionsForm.FormCreate(Sender: TObject);
  var i: smallint;
  begin
  { note: don't need copy of aspect because can't change anything in it here }
  for i := 1 to 8 do style.items.add(intToStr(i));
  loggedVar := GsLoggedVar.create;
  if loggedVar = nil then closeWithException('Could not create loggedVar');
  end;

procedure TPlotOptionsForm.OKClick(Sender: TObject);
  begin
  loggedVar.copyOptionsTo(loggedVarOriginal);
  modalResult := mrOK;
  end;

procedure TPlotOptionsForm.CancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure TPlotOptionsForm.FormDeactivate(Sender: TObject);
  begin
  loggedVar.free;
  loggedVar := nil;
  loggedVarOriginal := nil;
  aspect := nil;
  end;

procedure TPlotOptionsForm.initialize(aLoggedVar: GsLoggedVar; anAspect: GsAspect; showAtChoice: boolean);
  var i: smallint;
  begin
  { make copy of loggedVar for cancel; copy only pointer to aspect }
  if aLoggedVar = nil then closeWithException('Plot options form initialized with nil loggedVar');
  loggedVarOriginal := aLoggedVar;
  loggedVarOriginal.copyOptionsTo(loggedVar);
  if anAspect = nil then closeWithException('Plot options form initialized with nil aspect');
  aspect := anAspect;
  { fill info from current loggedVar and aspect }
  showDialogAtChoice.checked := showAtChoice;
  { must use loggedVarOriginal, not loggedVar, for aspectNameLabel.text because it uses aspect pointer to get name,
    and copy loggedVar has no aspect pointer }
  aspectNameLabel.text := removeUnitSuffix(loggedVarOriginal.displayName(kShowLayerInfo));
  Domain.garden.loadObjectNamesIntoComboBoxForObjectType(objectChoices, aspect.objectType);
  if loggedVar.model <> nil then
    objectChoices.itemIndex := objectChoices.items.indexOfObject(loggedVar.model)
  else
    objectChoices.itemIndex := -1;
  colorSwatch.brush.color := loggedVar.lineColor;
  colorSwatch.invalidate;
  style.itemIndex := smallint(loggedVar.lineStyle);
  style.invalidate;
  drawBars.checked := loggedVar.drawBars; 
  if drawBars.checked then
    style.enabled := false
  else
    style.enabled := true;
  self.drawLineImage;
  if loggedVar.scaleBy >= 1.0 then
    scaleChoices.text := floatToStrF(loggedVar.scaleBy, ffFixed, 7, 1)
  else
    scaleChoices.text := floatToStr(loggedVar.scaleBy);
  aspectUnit.text := unitStringForEnum(aspect.unitSet(loggedVar.groupItemDerivedIndex), loggedVar.currentUnit);
  if loggedVar.axisToPlotOn = kLeftAxis then
    leftAxis.checked := true
  else
    rightAxis.checked := true;
  end;

procedure TPlotOptionsForm.closeWithException(exceptionString: string);
  begin
  raise Exception.create(exceptionString);
  modalResult := mrCancel;
  end;

procedure TPlotOptionsForm.StyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var lineWidth: smallint;
	begin
  if Application.terminated then exit;
  if (style.items.count <= 0) or (index < 0) or (index > style.items.count - 1) then exit;
  style.canvas.fillRect(rect);
  style.canvas.pen.color := clBlack;
  if index < 5 then
  	begin
  	style.canvas.pen.width := 1;
  	style.canvas.pen.style := TPenStyle(index);
  	style.canvas.moveTo(rect.left, rect.top + 8);
  	style.canvas.lineTo(rect.right, rect.top + 8);
    end
  else
  	begin
  	lineWidth := index - 3;
		style.canvas.brush.color := clBlack;
  	style.canvas.brush.style := bsSolid;
  	style.canvas.fillRect(classes.Rect(rect.left, rect.top + 8 - (lineWidth div 2),
  	rect.right, rect.top + 8 + (lineWidth div 2) + (lineWidth mod 2)));
    end
	end;

procedure TPlotOptionsForm.drawLineImage;
	begin
  with lineImage.canvas do
    begin
    fillRect(lineImage.clientRect);
    pen.color := loggedVar.lineColor;
    if drawBars.checked then
      begin
      pen.width := 5;
      pen.style := psSolid;
      moveTo(lineImage.width div 3, lineImage.height - 1);
      lineTo(lineImage.width div 3, lineImage.height div 3);
      moveTo(2 * lineImage.width div 3, lineImage.height - 1);
      lineTo(2 * lineImage.width div 3, 3 * lineImage.height div 4);
      end
    else
      begin
      pen.width := loggedVar.lineWidth;
      pen.style := TPenStyle(loggedVar.lineStyle);
      moveTo(0, lineImage.height);
      lineTo(lineImage.width div 3, lineImage.height div 3);
      lineTo(2 * lineImage.width div 3, 3 * lineImage.height div 4);
      lineTo(lineImage.width, 0);
      end;
    end;
  end;

procedure TPlotOptionsForm.StyleChange(Sender: TObject);
	begin
  if style.itemIndex < 5 then
  	begin
  	loggedVar.lineWidth := 1;
    loggedVar.lineStyle := TPenStyle(style.itemIndex);
    end
  else
  	begin
  	loggedVar.lineWidth := style.itemIndex - 3;
    loggedVar.lineStyle := psSolid;
    end;
  self.drawLineImage;
	end;

procedure TPlotOptionsForm.ColorClick(Sender: TObject);
  begin
  ColorDialog.color := loggedVar.lineColor;
  if ColorDialog.execute then
	  begin
    loggedVar.lineColor := ColorDialog.color;
    self.drawLineImage;
    end;
  end;

procedure TPlotOptionsForm.FormShow(Sender: TObject);
  begin
  if loggedVar <> nil then
	begin
	self.drawLineImage;
  if loggedVar.lineWidth = 1 then
		style.itemIndex := smallint(loggedVar.lineStyle)
  else
		style.itemIndex := loggedVar.lineWidth + 3;
  end;
  end;

procedure TPlotOptionsForm.colorSwatchMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  ColorDialog.color := loggedVar.lineColor;
  if ColorDialog.execute then
	  begin
    loggedVar.lineColor := ColorDialog.color;
    colorSwatch.brush.color := colorDialog.color;
    colorSwatch.invalidate;
    self.drawLineImage;
    end;
  end;

procedure TPlotOptionsForm.aspectUnitKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  if (key = VK_DOWN) then
    self.aspectUnitMouseUp(sender, mbLeft, [], 0, 0)
  else if (key = VK_UP) then
    self.aspectUnitMouseUp(sender, mbLeft, [ssShift], 0, 0);
  end;

procedure TPlotOptionsForm.aspectUnitMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if not (ssShift in shift) then
    loggedVar.currentUnit := GetNextUnitEnumInUnitSet(aspect.unitSet(loggedVar.groupItemDerivedIndex),
        loggedVar.currentUnit)
  else
    loggedVar.currentUnit := GetPreviousUnitEnumInUnitSet(aspect.unitSet(loggedVar.groupItemDerivedIndex),
        loggedVar.currentUnit);
  aspectUnit.text := unitStringForEnum(aspect.unitSet(loggedVar.groupItemDerivedIndex), loggedVar.currentUnit);
  end;

procedure TPlotOptionsForm.leftAxisClick(Sender: TObject);
  begin
  loggedVar.axisToPlotOn := kLeftAxis;
  end;

procedure TPlotOptionsForm.rightAxisClick(Sender: TObject);
  begin
  loggedVar.axisToPlotOn := kRightAxis;
  end;

procedure TPlotOptionsForm.scaleChoicesChange(Sender: TObject);
  var oldValue, newValue: single;
  begin
  oldValue := loggedVar.scaleBy;
  try
    newValue := strToFloat(scaleChoices.text);
    newValue := min(10000000.0, max(0.00000001, newValue));  { arbitrary bounds }
    loggedVar.scaleBy := newValue;
  except
    loggedVar.scaleBy := oldValue;
  end;
  end;

procedure TPlotOptionsForm.objectChoicesChange(Sender: TObject);
  begin
  if objectChoices.itemIndex <> -1 then
    loggedVar.model := GsModel(objectChoices.items.objects[objectChoices.itemIndex]);
  end;

procedure TPlotOptionsForm.drawBarsClick(Sender: TObject);
  begin
  loggedVar.drawBars := drawBars.checked;
  if drawBars.checked then
    style.enabled := false
  else
    style.enabled := true;
  self.drawLineImage;
  end;

procedure TPlotOptionsForm.helpClick(Sender: TObject);
  begin
  application.helpJump('windows_Graph_item_options_window')
  end;

procedure TPlotOptionsForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  if (ssAlt in shift) and (chr(key) in ['L', 'l']) then
    self.colorSwatchMouseUp(sender, mbLeft, shift, 0, 0);
  end;

end.

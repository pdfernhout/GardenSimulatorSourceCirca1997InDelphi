unit Ubrowcol;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowcol: Browser component for color. Displays three sliders for R, G, and B, and
square of color. User can drag sliders to change color or click on square to get standard
Windows color selector. }

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom, usliders;

const
  kItemRedSlider = 1;
  kItemGreenSlider = 2;
  kItemBlueSlider = 3;
  kItemColorRect = 4;
  kColorRectSize = 100;
  kMinSliderLength = 50;

type KfColorBrowserComponent = class(KfBrowserComponent)
	public
  currentColor: TColorRef;
  redSlider: KfSlider;
  greenSlider: KfSlider;
  blueSlider: KfSlider;
  colorRect: TRect;
  procedure initialize; override;
  procedure initSlider(slider: KfSlider);
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  procedure sliderMouseDown(Sender: TObject);
  procedure sliderMouseMove(Sender: TObject);
  procedure sliderMouseUp(Sender: TObject);
  procedure sliderKeyDown(sender: TObject);
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  function maxSelectedItemIndex: integer; override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  destructor destroy; override;
  procedure disableSliders;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct;

procedure KfColorBrowserComponent.initialize;
	begin
	{assuming sliders will be deleted automatically by owner - self - otherwise would have destructor}
  redSlider := KfSlider.create(self);
  self.initSlider(redSlider);
  greenSlider := KfSlider.create(self);
  self.initSlider(greenSlider);
  blueSlider := KfSlider.create(self);
  self.initSlider(blueSlider);
  self.onMouseUp := self.doMouseUp;
  end;

procedure KfColorBrowserComponent.initSlider(slider: KfSlider);
  begin
  with slider do
    begin
    parent := self;
    FOnMouseDown := self.sliderMouseDown;
    FOnMouseMove := self.sliderMouseMove;
    FOnMouseUp := self.sliderMouseUp;
    FOnKeyDown := self.sliderKeyDown;
    readOnly := aspect.readOnly;
    useDefaultSizeAndDraggerSize;
    minValue := 0;
    maxValue := 255;
    end;
  end;

destructor KfColorBrowserComponent.destroy;
  begin
  { do not free sliders because their parent (self) will free them }
  redSlider := nil;
  greenSlider := nil;
  blueSlider := nil;
  inherited destroy;
  end;

procedure KfColorBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  { process slider arrow keys first }
  if (model <> nil) and (not self.readOnly) then
    case key of
    VK_HOME, VK_END, VK_DOWN, VK_LEFT, VK_UP, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_RETURN:
      case self.selectedItemIndex of
        kItemRedSlider:
          begin
          redSlider.doKeyDown(sender, key, shift);
          exit;
          end;
        kItemGreenSlider:
          begin
          greenSlider.doKeyDown(sender, key, shift);
          exit;
          end;
        kItemBlueSlider:
          begin
          blueSlider.doKeyDown(sender, key, shift);
          exit;
          end;
        kItemColorRect:
          begin
          self.doMouseUp(self, mbLeft, [], colorRect.left + 1, colorRect.top + 1);
          end;
        end;
      end;
  inherited doKeyDown(sender, key, shift);
  end;

procedure KfColorBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
    colorDialog: TColorDialog;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  if (model = nil) or (self.readOnly) then exit;
  thePoint := point(x,y);
  if ptInRect(colorRect, thePoint) then
    begin
    self.selectedItemIndex := kItemColorRect;
    colorDialog := TColorDialog.create(owner);
    try
    if colorDialog.execute then
      begin
      currentColor := colorDialog.color;
		  GardenForm.doCommand(
        GsChangeDomainColorCommand.createCommand(model, currentColor, aspect.fieldNumber));
      self.updateDisplay;
      self.invalidate;
      end;
    finally
      colorDialog.free;
      colorDialog := nil;
    end;
    end;
  end;

procedure KfColorBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  redSlider.enabled := model <> nil;
  greenSlider.enabled := model <> nil;
  blueSlider.enabled := model <> nil;
  redSlider.readOnly := self.readOnly;
  greenSlider.readOnly := self.readOnly;
  blueSlider.readOnly := self.readOnly;
  end;

procedure KfColorBrowserComponent.updateModelValues;
  var oldColor: TColorRef;
  begin
  if model = nil then exit;
  if self.collapsed then exit;
  oldColor := currentColor;
  self.updateCurrentValue(-1);
  if oldColor <> currentColor then
    begin
    self.updateDisplay;
    self.invalidate;
    end;
  end;

procedure KfColorBrowserComponent.updateCurrentValue(aFieldIndex: integer);
	begin
  if (model <> nil) and (aspect.fieldType = kFieldColor) then
    begin
    model.transferField(kGetField, currentColor, aspect.fieldNumber, kFieldColor, kNotArray,
      aspect.deriveMethod(groupItem.derivedIndex), nil);
    end
  else
    self.disableSliders;
  end;

procedure KfColorBrowserComponent.disableSliders;
  begin
  redSlider.currentValue := 0;
  redSlider.enabled := false;
  greenSlider.currentValue := 0;
  greenSlider.enabled := false;
  blueSlider.currentValue := 0;
  blueSlider.enabled := false;
  end;

procedure KfColorBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldColor) then
    begin
    redSlider.currentValue := getRValue(currentColor);
    greenSlider.currentValue := getGValue(currentColor);
    blueSlider.currentValue := getBValue(currentColor);
    end
  else
    self.disableSliders;
  end;

procedure KfColorBrowserComponent.sliderMouseDown(Sender: TObject);
  begin
  if model = nil then exit;
  if sender = redSlider then
    self.selectedItemIndex := kItemRedSlider
  else if sender = greenSlider then
    self.selectedItemIndex := kItemGreenSlider
  else if sender = blueSlider then
    self.selectedItemIndex := kItemBlueSlider;
  if not self.focused then
    self.setFocus
  else
    self.invalidate;
  { paint method updates unofficial focus flags in sliders }
  end;

procedure KfColorBrowserComponent.sliderMouseMove(Sender: TObject);
	begin
  if model <> nil then
    begin
    currentColor := support_rgb(redSlider.currentValue, greenSlider.currentValue, blueSlider.currentValue);
    { don't make change permanent yet, not until mouse up }
    self.updateDisplay;
    self.invalidate;
    end
  else
    self.disableSliders;
  end;

procedure KfColorBrowserComponent.sliderMouseUp(Sender: TObject);
	begin
  if model <> nil then
    begin
    currentColor := support_rgb(redSlider.currentValue, greenSlider.currentValue, blueSlider.currentValue);
		GardenForm.doCommand(
      GsChangeDomainColorCommand.createCommand(model, currentColor, aspect.fieldNumber));
    self.updateDisplay;
    self.invalidate;
    end
  else
    self.disableSliders;
  end;

procedure KfColorBrowserComponent.sliderKeyDown(sender: TOBject);
  begin
  self.sliderMouseUp(sender);
  end;

function KfColorBrowserComponent.maxWidth: integer;
  begin
  result := kLeftRightGap * 2 + intMax(self.labelWidth, kEditMaxWidth);
  end;

function KfColorBrowserComponent.minWidth(requestedWidth: integer): integer;
  var minAllowed: integer;
  begin
  minAllowed := kLeftRightGap * 2 + intMax(self.longestLabelWordWidth,
    kColorRectSize + kMinSliderLength + kBetweenGap);
  if requestedWidth > minAllowed then
    result := -1
  else
    result := minAllowed;
  end;

function KfColorBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + self.textHeight * 3 + kBetweenGap * 2 + kTopBottomGap * 2;
  end;

procedure KfColorBrowserComponent.resizeElements;
  begin
	redSlider.left := kLeftRightGap + self.formTextWidth('W') + kBetweenGap;
  redSlider.width := self.width - kColorRectSize - kLeftRightGap * 2;
	redSlider.top := self.collapsedHeight + kTopBottomGap;
	greenSlider.left := redSlider.left;
  greenSlider.width := redSlider.width;
	greenSlider.top := redSlider.top + self.textHeight + kBetweenGap;
	blueSlider.left := redSlider.left;
  blueSlider.width := redSlider.width;
	blueSlider.top := greenSlider.top + self.textHeight + kBetweenGap;
  end;

function KfColorBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (redSlider.enabled) then
    result := kItemColorRect
  else
    result := kItemLabel;
  end;

procedure KfColorBrowserComponent.Paint;
var
  Rect, rTextRect, gTextRect, bTextRect: TRect;
  oldPalette: HPalette;
begin
  { ask sliders to update themselves based on whether they are selected }
  redSlider.hasUnofficialFocus := (self.selectedItemIndex = kItemRedSlider);
  greenSlider.hasUnofficialFocus := (self.selectedItemIndex = kItemGreenSlider);
  blueSlider.hasUnofficialFocus := (self.selectedItemIndex = kItemBlueSlider);
  inherited paint;
  if self.collapsed then exit;
  rect := getClientRect;
  with rTextRect do
    begin
    left := rect.left + kLeftRightGap;
    right := left + self.canvas.textWidth('W');
    top := self.collapsedHeight + kTopBottomGap;
    bottom := top + self.textHeight;
    end;
  gTextRect := rTextRect;
  with gTextRect do
    begin
    top := rTextRect.bottom + kBetweenGap;
    bottom := top + self.textHeight;
    end;
  bTextRect := rTextRect;
  with bTextRect do
    begin
    top := gTextRect.bottom + kBetweenGap;
    bottom := top + self.textHeight;
    end;
  with colorRect do
    begin
    left := rTextRect.right + redSlider.width + kBetweenGap * 2;
    right := self.width - kLeftRightGap;
    top := self.collapsedHeight + kTopBottomGap;
    bottom := self.uncollapsedHeight - kTopBottomGap;
    end;
  with Canvas do
    begin
    self.drawText('r', rTextRect, false, false);
    self.drawText('g', gTextRect, false, false);
    self.drawText('b', bTextRect, false, false);
    oldPalette := 0;
    if Domain.paletteBitmapLoaded then
      begin
    	oldPalette := selectPalette(handle, domain.paletteBitmap.palette, false);
      realizePalette(handle);
      end;
    brush.color := currentColor;
    fillRect(colorRect);
    if Domain.paletteBitmapLoaded then
      begin
    	selectPalette(handle, oldPalette, true);
      realizePalette(handle);
      end;
    pen.color := clBtnShadow;
    if (self.selectedItemIndex <> kItemColorRect) then
      pen.color := clBtnShadow
    else
      pen.color := clBtnText;
    brush.style := bsClear;
    rectangle(colorRect.left - 1, colorRect.top - 1, colorRect.right + 1, colorRect.bottom + 1);
    end;
end;

end.


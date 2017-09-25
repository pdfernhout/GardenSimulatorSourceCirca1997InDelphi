unit Ubrowint;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowint: Browser component for integer. There are no integer arrays allowed, so far.
Displays slider with upper and lower bounds, and value beneath. Does NOT allow user
to change the unit.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom, usliders;

const
  kItemSlider = 1;
  kItemValueText = 2;
  kItemUnitText = 3;

type KfIntegerSliderBrowserComponent = class(KfBrowserComponent)
	public
  currentValue: integer;
  minValue: integer;
  maxValue: integer;
  slider: KfSlider;
  unitIndex: integer;
  procedure initialize; override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  procedure sliderMouseDown(Sender: TObject);
  procedure sliderMouseMove(Sender: TObject);
  procedure sliderMouseUp(Sender: TObject);
  procedure sliderKeyDown(sender: TOBject);
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function minScaleWidthWithBounds: integer;
  function minScaleWidthWithoutBounds: integer;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  function maxSelectedItemIndex: integer; override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  function checkCurrentValueAgainstSoftBounds(aFieldIndex: integer): boolean;
  destructor destroy; override;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct;

procedure KfIntegerSliderBrowserComponent.initialize;
	begin
	{assuming slider will be deleted automatically by owner - self - otherwise would have destructor}
	slider := KfSlider.create(self);
  slider.parent := self;
  slider.FOnMouseDown := self.sliderMouseDown;
  slider.FOnMouseMove := self.sliderMouseMove;
  slider.FOnMouseUp := self.sliderMouseUp;
  slider.FOnKeyDown := self.sliderKeyDown;
  slider.readOnly := aspect.readOnly;
  slider.useDefaultSizeAndDraggerSize;
  if groupItem.currentUnit <> 0 then
    unitIndex := groupItem.currentUnit
  else
    if GsDomain.default.menuOptions.showMetricUnits then
      unitIndex := aspect.unitDefaultMetric(groupItem.derivedIndex)
    else
      unitIndex := aspect.unitDefaultEnglish(groupItem.derivedIndex);
  minValue := trunc(aspect.boundSoftLower(groupItem.derivedIndex));
  maxValue := trunc(aspect.boundSoftUpper(groupItem.derivedIndex));
  slider.minValue := minValue;
  slider.maxValue := maxValue;
  end;

destructor KfIntegerSliderBrowserComponent.destroy;
  begin
  { do not free slider because owner (self) will free it }
  slider := nil;
  inherited destroy;
  end;

function KfIntegerSliderBrowserComponent.checkCurrentValueAgainstSoftBounds(aFieldIndex: integer): boolean;
  var oldValue: integer;
  begin
  oldValue := currentValue;
  if currentValue < minValue then currentValue := minValue;
  if currentValue > maxValue then currentValue := maxValue;
  result := (currentValue <> oldValue);
  end;

procedure KfIntegerSliderBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  { process slider arrow keys first }
  if slider.enabled then
    case key of
      VK_HOME, VK_END, VK_DOWN, VK_LEFT, VK_UP, VK_RIGHT, VK_NEXT, VK_PRIOR:
        if (self.selectedItemIndex = kItemSlider) then
          begin
          slider.doKeyDown(sender, key, shift);
          exit;
          end;
        end;
  inherited doKeyDown(sender, key, shift);
  if slider.enabled then if (key = VK_RETURN) and (self.selectedItemIndex = kItemSlider) then
    slider.doKeyDown(sender, key, shift);
  end;

procedure KfIntegerSliderBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  slider.enabled := model <> nil;
  slider.readOnly := self.readOnly;
  end;

procedure KfIntegerSliderBrowserComponent.updateModelValues;
  var oldValue: integer;
  begin
  if model = nil then exit;
  if self.collapsed then exit;
  oldValue := currentValue;
  self.updateCurrentValue(-1);
  if oldValue <> currentValue then
    begin
    updateDisplay;
    invalidate;
    end;
  end;

procedure KfIntegerSliderBrowserComponent.updateCurrentValue(aFieldIndex: integer);
	begin
  if (model <> nil) and (aspect.fieldType = kFieldInt) then
    begin
    model.transferField(kGetField, currentValue, aspect.fieldNumber, kFieldInt, kNotArray,
      aspect.deriveMethod(groupItem.derivedIndex), nil);
    if currentValue < minValue then
      begin
      minValue := currentValue;
  		slider.minValue := minValue;
      end;
    if currentValue > maxValue then
    	begin
      maxValue := currentValue;
  		slider.maxValue := maxValue;
      end;
    end
  else
    currentValue := 0;
  end;

procedure KfIntegerSliderBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldInt) then
    begin
    slider.currentValue := currentValue;
    end
  else
    begin
    slider.currentValue := 0;
    slider.enabled := false;
    end;
  end;

procedure KfIntegerSliderBrowserComponent.sliderMouseDown(Sender: TObject);
  begin
  if model = nil then exit;
  self.selectedItemIndex := kItemSlider;
  slider.hasUnofficialFocus := true;
  slider.invalidate;
  if not self.focused then self.setFocus;
  end;

procedure KfIntegerSliderBrowserComponent.sliderMouseMove(Sender: TObject);
	begin
  if model <> nil then
    begin
  	currentValue := slider.currentValue;
    { don't make change permanent yet, not until mouse up }
    self.updateDisplay;
    self.invalidate;
    end
  else
    begin
    slider.currentValue := 0;
    slider.enabled := false;
    end;
  end;

procedure KfIntegerSliderBrowserComponent.sliderMouseUp(Sender: TObject);
	begin
  if model <> nil then
    begin
  	currentValue := slider.currentValue;
		GardenForm.doCommand(
  		GsChangeDomainIntegerCommand.createCommand(model, currentValue, aspect.fieldNumber, kNotArray));
    self.updateDisplay;
    self.invalidate;
    end
  else
    begin
    slider.currentValue := 0;
    slider.enabled := false;
    end;
  end;

procedure KfIntegerSliderBrowserComponent.sliderKeyDown(sender: TOBject);
  begin
  self.sliderMouseUp(sender);
  end;

function KfIntegerSliderBrowserComponent.maxWidth: integer;
  begin
  result := kLeftRightGap * 2 + intMax(self.labelWidth, minScaleWidthWithBounds);
  end;

function KfIntegerSliderBrowserComponent.minWidth(requestedWidth: integer): integer;
  var minAllowed: integer;
  begin
  minAllowed := kLeftRightGap * 2 + intMax(self.longestLabelWordWidth, minScaleWidthWithoutBounds);
  if requestedWidth > minAllowed then
    result := -1
  else
    result := minAllowed;
  end;

function KfIntegerSliderBrowserComponent.minScaleWidthWithBounds: integer;
  var
    minString, valueString, unitString, maxString: string[30];
  begin
  minString := intToStr(minValue);
  valueString := intToStr(currentValue) + '  ';
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), unitIndex);
  maxString := intToStr(maxValue);
  result := self.formTextWidth(minString + valueString + unitString + maxString)
    + kBetweenGap * 3;
  end;

function KfIntegerSliderBrowserComponent.minScaleWidthWithoutBounds: integer;
  var
    valueString, unitString: string[30];
  begin
  valueString := intToStr(currentValue) + '  ';
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), unitIndex);
  result := self.formTextWidth(valueString + unitString) + kBetweenGap;
  end;

function KfIntegerSliderBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + self.editHeight + kTopBottomGap * 2;
  end;

procedure KfIntegerSliderBrowserComponent.resizeElements;
  begin
	slider.left := kLeftRightGap;
	slider.top := self.collapsedHeight;
  slider.width := self.width - kLeftRightGap * 2;
  end;

function KfIntegerSliderBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (slider.enabled) then
    result := kItemSlider
  else
    result := kItemLabel;
  end;

procedure KfIntegerSliderBrowserComponent.Paint;
var
  Rect, minTextRect, maxTextRect, valueTextRect, unitTextRect: TRect;
  minString, valueString, maxString, unitString: string[30];
begin
  { ask slider to update itself based on whether it is selected }
  if (self.selectedItemIndex = kItemSlider) then
    slider.hasUnofficialFocus := true
  else
    slider.hasUnofficialFocus := false;
  inherited paint;
  if self.collapsed then exit;
  minString := intToStr(minValue);
  valueString := intToStr(currentValue);
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), unitIndex);
  maxString := intToStr(maxValue);
  rect := getClientRect;
  with minTextRect do
    begin
    left := rect.left + kLeftRightGap;
    right := left + self.canvas.textWidth(minString);
    top := slider.top + slider.height + 1;
    bottom := top + self.textHeight;
    end;
  with maxTextRect do
    begin
    right := rect.right - kLeftRightGap;
    left := right - self.canvas.textWidth(maxString);
    top := minTextRect.top;
    bottom := top + self.textHeight;
    end;
  with valueTextRect do
    begin
    right := rect.left + (rect.right - rect.left) div 2;
    left := right - self.canvas.textWidth(valueString);
    top := minTextRect.top;
    bottom := top + self.textHeight;
    end;
  with unitTextRect do
    begin
    left := valueTextRect.right + 5;
    right := left + self.canvas.textWidth(unitString);
    top := minTextRect.top;
    bottom := top + self.textHeight;
    end;
  with Canvas do
    begin
    self.drawText(minString, minTextRect, false, false);
    self.drawText(valueString, valueTextRect, false, false);
    self.drawText(unitString, unitTextRect, false, false);
    self.drawText(maxString, maxTextRect, false, false);
  end;
end;

end.

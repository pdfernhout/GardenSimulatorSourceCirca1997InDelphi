unit ubrowrl;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowrl: Browser component for one real value (single). Displays one slider with bounds,
a value, and a unit. User can slide slider, click on value to change in dialog, or click
on unit. Clicking on bound brings up bound change dialog (uboundch).
There are two types of bounds: recommended and absolute. Recommended bounds are
our guesses for what we expect these things to be. Absolute bounds matter only for
editable aspects and they cannot be overridden. For read-only aspects, bounds are always
overridden if the values range outside the bounds.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom, usliders;

const
  kItemSlider = 1;
  kItemMinBound = 2;
  kItemValueText = 3;
  kItemUnitText = 4;
  kItemMaxBound = 5;

type KfRealSliderBrowserComponent = class(KfBrowserComponent)
	public
  currentValue: single;
  softMin: single;
  softMax: single;
  hardMin: single;
  hardMax: single;
  slider: KfSlider;
  currentUnitIndex: integer;
  valueTextRect: TRect;
  unitTextRect: TRect;
  minTextRect: TRect;
  maxTextRect: TRect;
  drawWithoutBounds: boolean;
  arrayIndex: smallint;
  procedure initialize; override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  procedure sliderMouseDown(Sender: TObject);
  procedure sliderMouseMove(Sender: TObject);
  procedure sliderMouseUp(Sender: TObject);
  procedure sliderKeyDown(sender: TOBject);
  function sliderPositionFromValue(value: single): integer;
  function valueFromSliderPosition: single;
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  function maxSelectedItemIndex: integer; override;
  function nextSelectedItemIndex(goForward: boolean): integer; override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure changeSoftBounds;
  procedure adjustValue;
  procedure selectNextUnitInSet(shift, ctrl: boolean);
  procedure calculateBoundsForCurrentUnit;
  function checkModelValueAgainstSoftBounds: boolean;
  function checkModelValueAgainstHardBounds: boolean;
  function toModelUnit(value: single): single;
  function toCurrentUnit(value: single): single;
  function boundWarning(hard: boolean; value: single): string;
  destructor destroy; override;
  procedure updateAspectInfo; override;
  procedure selectDerivationMethod;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct,
  uboundch, uesoil, ueplant, ugroups, uderopt;

procedure KfRealSliderBrowserComponent.initialize;
	begin
	{assuming slider will be deleted automatically by owner - self - otherwise would have destructor}
	slider := KfSlider.create(self);
  slider.parent := self;
  slider.FOnMouseDown := self.sliderMouseDown;
  slider.FOnMouseMove := self.sliderMouseMove;
  slider.FOnMouseUp := self.sliderMouseUp;
  slider.FOnKeyDown := self.sliderKeyDown;
  slider.maxValue := 100; { make it a percentage move }
  slider.minValue := 0;
  slider.readOnly := aspect.readOnly;
  slider.useDefaultSizeAndDraggerSize;
  arrayIndex := kNotArray;
  if groupItem.currentUnit <> 0 then
    currentUnitIndex := groupItem.currentUnit
  else
    begin
    if GsDomain.default.menuOptions.showMetricUnits then
      currentUnitIndex := aspect.unitDefaultMetric(groupItem.derivedIndex)
    else
      currentUnitIndex := aspect.unitDefaultEnglish(groupItem.derivedIndex);
    groupItem.setUnit(currentUnitIndex); 
    end;
  self.calculateBoundsForCurrentUnit;
  end;

destructor KfRealSliderBrowserComponent.destroy;
  begin
  { do not free slider because owner (self) will free it }
  slider := nil;
  inherited destroy;
  end;

procedure KfRealSliderBrowserComponent.adjustValue;
  var
{$IFDEF WINDOWS}
    newString: string;
{$ELSE}
    newString: ansistring;
{$ENDIF}
    oldString, nameString, prompt, unitString, minString, maxString: string;
    newValue, valueInCurrentUnit, valueBeforeBoundsCheck: single;
    outOfRange: boolean;
  begin
  valueInCurrentUnit := toCurrentUnit(currentValue);
  newString := digitValueString(valueInCurrentUnit);
  oldString := newString;
  nameString := copy(self.caption, 1, 30);
  if length(nameString) = 30 then nameString := nameString + '...';
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  minString :=  digitValueString(toCurrentUnit(softMin));
  maxString := digitValueString(toCurrentUnit(softMax));
  prompt := 'Enter a new value for '
    + chr(13) + chr(13) + nameString + chr(13) + chr(13)
    + 'between ' + minString + ' and ' + maxString + ' ' + unitString + '.';
  if inputQuery('Enter precise value', prompt, newString) then
    if (newString <> oldString) and boundForString(newString, aspect.fieldType, newValue) then
      begin
      currentValue := toModelUnit(newValue);
      valueBeforeBoundsCheck := currentValue;
      outOfRange := self.checkModelValueAgainstSoftBounds;
      if outOfRange then
        begin
        if messageDlg(boundWarning(kSoft, valueBeforeBoundsCheck), mtWarning, [mbYes, mbNo], 0) = mrYes then
          begin
          currentValue := valueBeforeBoundsCheck;
          if self.checkModelValueAgainstHardBounds then
            begin
            showMessage(boundWarning(kHard, valueBeforeBoundsCheck));
            { if value they entered is outside hard bounds, reset to original }
            if boundForString(oldString, aspect.fieldType, newValue) then
              currentValue := toModelUnit(newValue);
            end
          else { new value is outside soft bounds but inside hard bounds }
            begin
            { reset bounds to overridden value }
            if currentValue < softMin then softMin := currentValue
            else if currentValue > softMax then softMax := currentValue;
            end;
          end
        else
          { if they clicked No, reset to original }
          if boundForString(oldString, aspect.fieldType, newValue) then
            currentValue := toModelUnit(newValue);
        end;
		  GardenForm.doCommand(GsChangeDomainRealCommand.createCommand(model, currentValue,
        aspect.fieldNumber, arrayIndex, aspect.deriveMethod(groupItem.derivedIndex)));
      self.updateDisplay;
      self.invalidate;
      end;
  end;

function KfRealSliderBrowserComponent.boundWarning(hard: boolean; value: single): string;
  var
    valueInCurrentUnit, hardMinInCurrentUnit, hardMaxInCurrentUnit,
      minValueInCurrentUnit, maxValueInCurrentUnit: single;
    minString, maxString, recAbsString, unitString: string[50];
    endString: string;
  begin
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  if hard then
    begin
    minValueInCurrentUnit := toCurrentUnit(hardMin);
    maxValueInCurrentUnit := toCurrentUnit(hardMax);
    recAbsString := 'absolute';
    endString := 'Cannot override.';
    end
  else
    begin
    recAbsString := 'recommended';
    minValueInCurrentUnit := toCurrentUnit(softMin);
    maxValueInCurrentUnit := toCurrentUnit(softMax);
    hardMinInCurrentUnit := toCurrentUnit(hardMin);
    hardMaxInCurrentUnit := toCurrentUnit(hardMax);
    minString :=  digitValueString(hardMinInCurrentUnit);
    maxString :=  digitValueString(hardMaxInCurrentUnit);
    endString := 'Do you want to override the recommended range?' + chr(13) + '(The absolute range is '
      + minString + ' to ' + maxString + ' ' + unitString + '.)';
    end;
  minString :=  digitValueString(minValueInCurrentUnit);
  maxString :=  digitValueString(maxValueInCurrentUnit);
  valueInCurrentUnit := toCurrentUnit(value);
  result := 'That value ('
    + digitValueString(valueInCurrentUnit)
    + ') is outside the ' + recAbsString + ' range for this variable' + chr(13)
    + '(' + minString + ' to ' + maxString + ' ' + unitString + '). ' + chr(13) + endString;
  end;

function KfRealSliderBrowserComponent.toModelUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex, aspect.unitModel(groupItem.derivedIndex), value);
  end;

function KfRealSliderBrowserComponent.toCurrentUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), aspect.unitModel(groupItem.derivedIndex), currentUnitIndex, value);
  end;

function KfRealSliderBrowserComponent.checkModelValueAgainstSoftBounds: boolean;
  var oldValue: single;
  begin
  oldValue := currentValue;
  if currentValue < softMin then currentValue := softMin;
  if currentValue > softMax then currentValue := softMax;
  result := (currentValue <> oldValue);
  end;

function KfRealSliderBrowserComponent.checkModelValueAgainstHardBounds: boolean;
  var
    oldValue: single;
  begin
  oldValue := currentValue;
  if currentValue < hardMin then currentValue := hardMin;
  if currentValue > hardMax then currentValue := hardMax;
  result := (currentValue <> oldValue);
  end;

procedure KfRealSliderBrowserComponent.selectNextUnitInSet(shift, ctrl: boolean);
  begin
  if ctrl then
    self.selectDerivationMethod
  else
    begin
    if shift then
      currentUnitIndex := GetPreviousUnitEnumInUnitSet(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex)
    else
      currentUnitIndex := GetNextUnitEnumInUnitSet(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
    groupItem.setUnit(currentUnitIndex);
    self.calculateBoundsForCurrentUnit;
    self.updateCurrentValue(-1);
    self.updateDisplay;
    self.invalidate;
    groupsFileMightHaveChanged := true;
    end;
  end;

procedure KfRealSliderBrowserComponent.selectDerivationMethod;
  var
    derivationForm: TAspectDerivationForm;
    response: longint;
	begin
  if not aspect.canBeDerived then exit;
  derivationForm := TAspectDerivationForm.create(self);
  if derivationForm = nil then
    raise Exception.create('Could not create aspect derivation window');
  try
    derivationForm.initialize(
        DisplayUnitSetString(aspect.unitSet(0)),
        'This is the simulation variable without calculation.',
        GsGroupItem.deriveMethodName(aspect.deriveMethod(1)),
        GsGroupItem.deriveMethodDescription(aspect.deriveMethod(1)),
        GsGroupItem.deriveMethodName(aspect.deriveMethod(2)),
        GsGroupItem.deriveMethodDescription(aspect.deriveMethod(2)),
        GsGroupItem.deriveMethodName(aspect.deriveMethod(3)),
        GsGroupItem.deriveMethodDescription(aspect.deriveMethod(3)),
        groupItem.derivedIndex);
    response := derivationForm.showModal;
    if response = mrOK then
      begin
      groupItem.derivedIndex := derivationForm.deriveMethod;
      self.caption := firstLetterForObjectTypeIndex(aspect.objectType) + ': ' + groupItem.displayName;
      self.calculateBoundsForCurrentUnit;
      self.updateCurrentValue(-1);
      self.updateDisplay;
      self.invalidate;
      groupsFileMightHaveChanged := true;
      end;
  finally
    derivationForm.free;
    derivationForm := nil;
  end;
  end;

procedure KfRealSliderBrowserComponent.calculateBoundsForCurrentUnit;
  begin
  softMin := groupItem.boundSoftLower;
  softMax := groupItem.boundSoftUpper;
  hardMin := aspect.boundHardLower(groupItem.derivedIndex);
  hardMax := aspect.boundHardUpper(groupItem.derivedIndex);
  end;

procedure KfRealSliderBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  thePoint := Point(x, y);
  if ptInRect(valueTextRect, thePoint) then
    begin
    if slider.enabled and not slider.readOnly then
      begin
      self.selectedItemIndex := kItemValueText;
      self.invalidate;
      self.adjustValue;
      end;
    end
  else if ptInRect(unitTextRect, thePoint) then
    begin
    self.selectedItemIndex := kItemUnitText;
    self.invalidate;
    self.selectNextUnitInSet(ssShift in shift, ssCtrl in shift);
    end
  else if ptInRect(minTextRect, thePoint) then
    begin
    self.selectedItemIndex := kItemMinBound;
    self.invalidate;
    self.changeSoftBounds;
    end
  else if ptInRect(maxTextRect, thePoint) then
    begin
    self.selectedItemIndex := kItemMaxBound;
    self.invalidate;
    self.changeSoftBounds;
    end;
  end;

procedure KfRealSliderBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  { process slider arrow keys first }
  if slider.enabled and not slider.readOnly then
    case key of
      VK_HOME, VK_END, VK_DOWN, VK_LEFT, VK_UP, VK_RIGHT, VK_NEXT, VK_PRIOR:
        if (self.selectedItemIndex = kItemSlider) then
          begin
          slider.doKeyDown(sender, key, shift);
          exit;
          end;
        end;
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) then
    case self.selectedItemIndex of
      kItemSlider: if slider.enabled and not slider.readOnly then slider.doKeyDown(sender, key, shift);
      kItemValueText: if slider.enabled and not slider.readOnly then self.adjustValue;
      kItemUnitText:  self.selectNextUnitInSet(ssShift in shift, ssCtrl in shift);
      kItemMinBound, kItemMaxBound: self.changeSoftBounds;
      end;
  end;

procedure KfRealSliderBrowserComponent.changeSoftBounds;
  var
    boundsForm: TBoundsChangeForm;
    tryValue: single;
    response: integer;
  begin
  boundsForm := TBoundsChangeForm.create(owner);
  try
    boundsForm.initialize(aspect, groupItem);
    response := boundsForm.showModal;
    if response = mrOK then
      begin
      if (boundForString(boundsForm.lowerBoundEdit.text, aspect.fieldType, tryValue)) then
        softMin := Convert(aspect.unitSet(groupItem.derivedIndex), boundsForm.currentUnit,
          aspect.unitModel(groupItem.derivedIndex), tryValue);
      if (boundForString(boundsForm.upperBoundEdit.text, aspect.fieldType, tryValue)) then
        softMax := Convert(aspect.unitSet(groupItem.derivedIndex), boundsForm.currentUnit,
          aspect.unitModel(groupItem.derivedIndex), tryValue);
      end;
  finally
    boundsForm.free;
    boundsForm := nil;
  end;
  if response = mrOK then
    begin
    groupItem.boundSoftLower := softMin;
    groupItem.boundSoftUpper := softMax;
    self.calculateBoundsForCurrentUnit;
    self.updateCurrentValue(-1);
    self.updateDisplay;
    TBrowserForm(owner).repositionComponents;
    end;
  end;

function KfRealSliderBrowserComponent.sliderPositionFromValue(value: single): integer;
  begin
  if softMax - softMin = 0.0 then
    result := 0
  else if value <= softMin then
    result := 0
  else if value >= softMax then
    result := 100
  else result := round(100.0 * (value - softMin) / (softMax - softMin));
  end;

function KfRealSliderBrowserComponent.valueFromSliderPosition: single;
  var
    sliderPosition: single;
  begin
  sliderPosition := slider.currentValue;
  result := softMin
    + sliderPosition / 100.0 * (softMax - softMin);
  end;

procedure KfRealSliderBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  slider.enabled := model <> nil;
  slider.readOnly := self.readOnly;
  end;

procedure KfRealSliderBrowserComponent.updateModelValues;
  var oldValue: single;
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

procedure KfRealSliderBrowserComponent.updateCurrentValue(aFieldIndex: integer);
  var
    changed: boolean;
    soilPatch: GsSoilPatch;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldFloat) then
    begin
    if (aspect.indexCount > 1) and (aspect.indexType = kIndexTypeLayer) then
      begin
      if (groupItem.arrayShowType = kArrayShowMeanWeightedBySoilLayerThickness)
        or (groupItem.arrayShowType = kArrayShowMeanWeightedBySoilLayerWeight) then
        begin
        soilPatch := nil;
        { dependent on soil patch }
        case aspect.objectType of
          kObjectTypeSoil: soilPatch := GsSoilPatch(model);
          kObjectTypePlant, kObjectTypeDrawingPlant:
            { template plants have no soil patch, so any components that do summaries will have zero values }
            if model.isTemplate then
              soilPatch := nil
            else
              soilPatch := GsPlant(model).soil;
          end;
        if soilPatch <> nil then with groupItem do
          currentValue := soilPatch.valueForLayerArray(model, arrayShowType, aspect.fieldNumber, arraySelected,
            aspect.deriveMethod(groupItem.derivedIndex))
        else
          currentValue := 0.0;
        end
      else with groupItem do
        { not dependent on soil patch }
        currentValue := GsGroupItem.valueForLayerArray(model, arrayShowType, aspect.fieldNumber, arraySelected,
          aspect.deriveMethod(groupItem.derivedIndex));
      end
    else
      model.transferField(kGetField, currentValue, aspect.fieldNumber, kFieldFloat, arrayIndex,
        aspect.deriveMethod(groupItem.derivedIndex), nil);
    if currentValue < softMin then softMin := currentValue;
    if currentValue > softMax then softMax := currentValue;
    end
  else
    currentValue := 0.0;
  end;

procedure KfRealSliderBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldFloat) then
    slider.currentValue := self.sliderPositionFromValue(currentValue)
  else
    begin
    slider.currentValue := 0;
    slider.enabled := false;
    end;
  end;

procedure KfRealSliderBrowserComponent.sliderMouseDown(Sender: TObject);
  begin
  if model = nil then exit;
  self.selectedItemIndex := kItemSlider;
  slider.hasUnofficialFocus := true;
  slider.invalidate;
  if not self.focused then self.setFocus;
  end;

procedure KfRealSliderBrowserComponent.sliderMouseMove(Sender: TObject);
	begin
  if model = nil then exit;
  currentValue := self.valueFromSliderPosition;
  self.invalidate;
  end;

procedure KfRealSliderBrowserComponent.sliderMouseUp(Sender: TObject);
	begin
  if model = nil then exit;
  currentValue := self.valueFromSliderPosition;
  GardenForm.doCommand(GsChangeDomainRealCommand.createCommand(model, currentValue,
    aspect.fieldNumber, arrayIndex, aspect.deriveMethod(groupItem.derivedIndex)));
  self.invalidate;
  end;

procedure KfRealSliderBrowserComponent.sliderKeyDown(Sender: TObject);
  begin
  self.sliderMouseUp(sender);
  end;

function KfRealSliderBrowserComponent.maxWidth: integer;
  begin
  result := intMax(kLeftRightGap * 2 + self.labelWidth,
    self.minScaleWidthWithBounds(aspect.unitSet(groupItem.derivedIndex), aspect.unitModel(groupItem.derivedIndex),
      softMin, softMax));
  end;

function KfRealSliderBrowserComponent.minWidth(requestedWidth: integer): integer;
  var
    widthForLongestWord, widthWithBounds, widthWithoutBounds: integer;
  begin
  { 1. test if width given is less than labelWidth.
       if not, move on
       if so, test if width given is less than width of longest full word in labelWidth.
         if not, move on
         if so, give back width of longest full word in labelWidth as minWidth.
    2. test if width given is less than minScaleWidthWithBounds.
       if not, move on (set flag to draw bounds)
       if so, test if width given is less than minScaleWidthWithoutBounds.
         if not, move on (set flag to not draw bounds)
         if so, give back minScaleWidthWithoutBounds as minWidth.
  }
  widthForLongestWord := kLeftRightGap * 2 + self.longestLabelWordWidth;
  widthWithBounds := self.minScaleWidthWithBounds(aspect.unitSet(groupItem.derivedIndex),
      aspect.unitModel(groupItem.derivedIndex), softMin, softMax);
  widthWithoutBounds := self.minScaleWidthWithoutBounds(aspect.unitSet(groupItem.derivedIndex),
      aspect.unitModel(groupItem.derivedIndex));
  result := -1;
  if requestedWidth < widthForLongestWord then
    result := widthForLongestWord;
  if requestedWidth < widthWithBounds then
    begin
    drawWithoutBounds := true;
    if requestedWidth < widthWithoutBounds then
      result := intMax(result, widthWithoutBounds);
    end
  else
    drawWithoutBounds := false;
  end;

function KfRealSliderBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + slider.height + self.textHeight + kTopBottomGap;
  end;

procedure KfRealSliderBrowserComponent.resizeElements;
  begin
	slider.left := kLeftRightGap;
	slider.top := self.collapsedHeight;
  slider.width := self.width - kLeftRightGap * 2;
  end;

function KfRealSliderBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (model <> nil) then
    result := kItemMaxBound
  else
    result := kItemLabel;
  end;

function KfRealSliderBrowserComponent.nextSelectedItemIndex(goForward: boolean): integer;
  begin
  result := kItemNone;
  case self.selectedItemIndex of
    kItemNone: result := self.selectedItemIndex;
    kItemLabel:
      if goForward then
        begin
        if readOnly then result := kItemMinBound else result := kItemSlider;
        end
      else result := kItemNone;
    kItemSlider:
      if goForward then result := kItemMinBound else result := kItemLabel;
    kItemMinBound:
      if goForward then
        begin
        if readOnly then result := kItemUnitText else result := kItemValueText;
        end
      else
        begin
        if readOnly then result := kItemLabel else result := kItemSlider;
        end;
    kItemValueText:
      if goForward then result := kItemUnitText else result := kItemMinBound;
    kItemUnitText:
      if goForward then result := kItemMaxBound
      else
        begin
        if readOnly then result := kItemMinBound else result := kItemValueText;
        end;
    kItemMaxBound:
      if goForward then result := self.maxSelectedItemIndex + 1 else result := kItemUnitText;
    end;
  end;

procedure KfRealSliderBrowserComponent.updateAspectInfo;
  begin
  self.calculateBoundsForCurrentUnit;
  self.updateCurrentValue(-1);
  self.updateDisplay;
  end;

procedure KfRealSliderBrowserComponent.Paint;
var
  Rect: TRect;
  softMinInCurrentUnit, softMaxInCurrentUnit, valueInCurrentUnit: single;
  minString, valueString, maxString, unitString: string[30];
begin
  { ask slider to update itself based on whether it is selected }
  if (self.selectedItemIndex = kItemSlider) then
    slider.hasUnofficialFocus := true
  else
    slider.hasUnofficialFocus := false;
  inherited paint;
  if self.collapsed then exit;
  rect := getClientRect;
  softMinInCurrentUnit := toCurrentUnit(softMin);
  softMaxInCurrentUnit := toCurrentUnit(softMax);
  valueInCurrentUnit := toCurrentUnit(currentValue);
  minString := digitValueString(softMinInCurrentUnit);
  maxString := digitValueString(softMaxInCurrentUnit);
  valueString := digitValueString(valueInCurrentUnit);
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  with minTextRect do
    begin
    left := rect.left + kLeftRightGap;
    right := left + self.canvas.textWidth(minString);
    top := slider.top;
   { top := slider.top + slider.height + 1; }
    bottom := top + self.textHeight;
    end;
  with maxTextRect do
    begin
    right := rect.right - kLeftRightGap;
    left := right - self.canvas.textWidth(maxString);
    top := minTextRect.top;
    bottom := top + self.textHeight;
    end;
  slider.left := minTextRect.right + 1;
  slider.width := maxTextRect.left - slider.left - 2;
  with valueTextRect do
    begin
    right := rect.left + (rect.right - rect.left) div 2;
    left := right - self.canvas.textWidth(valueString);
    top := slider.top + slider.height + 1;
    {top := minTextRect.top; }
    bottom := top + self.textHeight;
    end;
  with unitTextRect do
    begin
    left := valueTextRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth(unitString);
    top := valueTextRect.top;
    bottom := top + self.textHeight;
    end;
  with Canvas do
    begin
    self.drawText(minString, minTextRect, true, (self.selectedItemIndex = kItemMinBound));
    self.drawText(valueString, valueTextRect, not self.readOnly, (self.selectedItemIndex = kItemValueText));
    self.drawText(unitString, unitTextRect, true, (self.selectedItemIndex = kItemUnitText));
    self.drawText(maxString, maxTextRect, true, (self.selectedItemIndex = kItemMaxBound));
    end;
  end;

end.

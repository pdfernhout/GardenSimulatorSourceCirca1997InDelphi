unit Ubrowrla;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowrla: Browser component for multiple real numbers (singles). Actually, there is no
reason whatsoever for this component to be separate from ubrowrl, which shows only one
single value. It's just that we started out thinking it was necessary to separate them for speed,
and we never took the time to put them together. So every change to one must usually result
in a change to the other. It displays from 1 (though we never use it for one) to 12 sliders
(12 is for months), one set of bounds, one value (for the selected slider), and one unit
(for all sliders). The user can change any value (by clicking on its slider or clicking
on the value when it is selected). Can select a slider by clicking on it or by tabbing to it.
Note that you CANNOT just say how many array elements you want; it works off the indexCount
field in the aspect.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom, usliders;

type
  valuesArray = array[0..11] of single;

type KfRealSliderArrayBrowserComponent = class(KfBrowserComponent)
	public
  arraySize: integer;
  currentIndex: integer;
  currentValues: valuesArray;
  softMin: single;
  softMax: single;
  hardMin: single;
  hardMax: single;
  sliders: array[0..11] of KfSlider;
  currentUnitIndex: integer;
  valueTextRect: TRect;
  valueTextItemIndex: integer;
  unitTextRect: TRect;
  unitTextItemIndex: integer;
  minTextRect: TRect;
  minBoundItemIndex: integer;
  maxTextRect: TRect;
  maxBoundItemIndex: integer;
  drawWithoutBounds: boolean;
  procedure initialize; override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  procedure sliderMouseDown(Sender: TObject);
  procedure sliderMouseMove(Sender: TObject);
  procedure sliderMouseUp(Sender: TObject);
  procedure sliderKeyDown(sender: TObject);
  function sliderPositionFromValue(value: single): integer;
  function valueFromSliderPosition(position: integer): single;
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  function maxSelectedItemIndex: integer; override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure adjustValue;
  procedure selectNextUnitInSet(shift, ctrl: boolean);
  procedure calculateBoundsForCurrentUnit;
  function checkModelValueAgainstBounds(hard: boolean; aFieldIndex: integer): boolean;
  function checkOneValueAgainstSoftBounds(i: integer): boolean;
  function checkOneValueAgainstHardBounds(i: integer): boolean;
  function aSliderIsSelected: boolean;
  function toModelUnit(value: single): single;
  function toCurrentUnit(value: single): single;
  function boundWarning(hard: boolean; value: single): string;
  destructor destroy; override;
  procedure updateAspectInfo; override;
  function nextSelectedItemIndex(goForward: boolean): integer; override;
  procedure changeSoftBounds;
  procedure selectDerivationMethod;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, umconsts, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct, uboundch,
  uderopt, ulayers, ugroups;

procedure KfRealSliderArrayBrowserComponent.initialize;
  var i: integer;
	begin
  arraySize := aspect.indexCount;
  minBoundItemIndex := arraySize + 1;
  unitTextItemIndex := minBoundItemIndex + 1;
  valueTextItemIndex := unitTextItemIndex + 1;
  maxBoundItemIndex := valueTextItemIndex + 1;
	{assuming slider will be deleted automatically by owner - self - otherwise would have destructor}
  for i := 0 to arraySize - 1 do
    begin
    sliders[i] := KfSlider.create(self);
    sliders[i].parent := self;
    sliders[i].FOnMouseDown := self.sliderMouseDown;
    sliders[i].FOnMouseMove := self.sliderMouseMove;
    sliders[i].FOnMouseUp := self.sliderMouseUp;
    sliders[i].FOnKeyDown := self.sliderKeyDown;
    sliders[i].minValue := 0;
    sliders[i].maxValue := 100; { make it a percentage move }
    sliders[i].readOnly := aspect.readOnly;
    sliders[i].tag := i;
    sliders[i].useDefaultSizeAndDraggerSize;
    end;
  currentIndex := 0;
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

destructor KfRealSliderArrayBrowserComponent.destroy;
  var i: integer;
  begin
  for i := 0 to arraySize - 1 do
    begin
    { do not free sliders because owner (self) will free them }
    sliders[i] := nil;
    end;
  inherited destroy;
  end;

procedure KfRealSliderArrayBrowserComponent.adjustValue;
  var
{$IFDEF WINDOWS}
    newString: string;
{$ELSE}
    newString: ansistring;
{$ENDIF}
    oldString, nameString, prompt, unitString, minString, maxString: string;
    newValue, valueInCurrentUnit, valueBeforeBoundsCheck: single;
    softMinInCurrentUnit, softMaxInCurrentUnit: single;
    outOfRange: boolean;
  begin
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  valueInCurrentUnit := toCurrentUnit(currentValues[currentIndex]);
  softMinInCurrentUnit := toCurrentUnit(softMin);
  softMaxInCurrentUnit := toCurrentUnit(softMax);
  newString := digitValueString(valueInCurrentUnit);
  oldString := newString;
  nameString := copy(self.caption, 1, 30);
  if length(nameString) = 30 then nameString := nameString + '...';
  nameString := nameString + ' [' + intToStr(currentIndex + 1) + ']'; {make better}
  minString := digitValueString(softMinInCurrentUnit);
  maxString := digitValueString(softMaxInCurrentUnit);
  prompt := 'Enter a new value for ' + chr(13) + chr(13)
    + nameString + chr(13) + chr(13)
    + 'between ' + minString + ' and ' + maxString + ' ' + unitString + '.';
  if inputQuery('Enter precise value', prompt, newString) then
    if (newString <> oldString) and boundForString(newString, aspect.fieldType, newValue) then
      begin
      currentValues[currentIndex] := toModelUnit(newValue);
      valueBeforeBoundsCheck := currentValues[currentIndex];
      outOfRange := self.checkModelValueAgainstBounds(kSoft, currentIndex);
      if outOfRange then
        begin
        if messageDlg(boundWarning(kSoft, valueBeforeBoundsCheck), mtWarning, [mbYes, mbNo], 0) = mrYes then
          begin
          currentValues[currentIndex] := valueBeforeBoundsCheck;
          if self.checkModelValueAgainstBounds(kHard, currentIndex) then
            begin
            showMessage(boundWarning(kHard, valueBeforeBoundsCheck));
            { if value they entered is outside hard bounds, reset to original }
            if boundForString(oldString, aspect.fieldType, newValue) then
              currentValues[currentIndex] := toModelUnit(newValue);
            end
          else { new value is outside soft bounds but inside hard bounds }
            begin
            { reset bounds to overridden value }
            if currentValues[currentIndex] < softMin then softMin := currentValues[currentIndex]
            else if currentValues[currentIndex] > softMax then softMax := currentValues[currentIndex];
            end;
          end
        else
          { if they clicked No, reset to original }
          if boundForString(oldString, aspect.fieldType, newValue) then
            currentValues[currentIndex] := toModelUnit(newValue);
        end;
		  GardenForm.doCommand(
  		  GsChangeDomainRealCommand.createCommand(model, currentValues[currentIndex],
        aspect.fieldNumber, currentIndex, aspect.deriveMethod(groupItem.derivedIndex)));
      self.updateDisplay;
      self.invalidate;
      end;
  end;

function KfRealSliderArrayBrowserComponent.toModelUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex,
      aspect.unitModel(groupItem.derivedIndex), value);
  end;

function KfRealSliderArrayBrowserComponent.toCurrentUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), aspect.unitModel(groupItem.derivedIndex),
      currentUnitIndex, value);
  end;

function KfRealSliderArrayBrowserComponent.checkModelValueAgainstBounds(hard: boolean;
  aFieldIndex: integer): boolean;
  var i: integer;
  begin
  result := false;
  if aFieldIndex <> -1 then
    begin
    if hard then
      result := self.checkOneValueAgainstHardBounds(aFieldIndex)
    else
      result := self.checkOneValueAgainstSoftBounds(aFieldIndex);
    end
  else
    begin
    if hard then
      begin
      for i := 0 to arraySize - 1 do
          if self.checkOneValueAgainstHardBounds(i) then result := true;
      end
    else
      begin
      for i := 0 to arraySize - 1 do
          if self.checkOneValueAgainstSoftBounds(i) then result := true;
      end;
    end;
  end;

function KfRealSliderArrayBrowserComponent.checkOneValueAgainstSoftBounds(i: integer): boolean;
  var oldValue: single;
  begin
  oldValue := currentValues[i];
  if currentValues[i] < softMin then currentValues[i] := softMin;
  if currentValues[i] > softMax then currentValues[i] := softMax;
  result := (currentValues[i] <> oldValue);
  end;

function KfRealSliderArrayBrowserComponent.checkOneValueAgainstHardBounds(i: integer): boolean;
  var oldValue: single;
  begin
  oldValue := currentValues[i];
  if currentValues[i] < hardMin then currentValues[i] := hardMin;
  if currentValues[i] > hardMax then currentValues[i] := hardMax;
  result := (currentValues[i] <> oldValue);
  end;

procedure KfRealSliderArrayBrowserComponent.selectNextUnitInSet(shift, ctrl: boolean);
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
    groupsFileMightHaveChanged := true;
    end;
  end;

procedure KfRealSliderArrayBrowserComponent.selectDerivationMethod;
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

function KfRealSliderArrayBrowserComponent.boundWarning(hard: boolean; value: single): string;
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

procedure KfRealSliderArrayBrowserComponent.calculateBoundsForCurrentUnit;
  begin
  softMin := groupItem.boundSoftLower;
  softMax := groupItem.boundSoftUpper;
  hardMin := aspect.boundHardLower(groupItem.derivedIndex);
  hardMax := aspect.boundHardUpper(groupItem.derivedIndex);
  end;

procedure KfRealSliderArrayBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  thePoint := Point(x, y);
  if valueTextItemIndex <> 0 then if ptInRect(valueTextRect, thePoint) then
    begin
    if sliders[0].enabled and not sliders[0].readOnly then
      begin
      self.selectedItemIndex := valueTextItemIndex;
      self.invalidate;
      self.adjustValue;
      end;
    end;
  if ptInRect(unitTextRect, thePoint) then
    begin
    self.selectedItemIndex := unitTextItemIndex;
    self.invalidate;
    self.selectNextUnitInSet(ssShift in shift, ssCtrl in shift);
    end
  else if ptInRect(minTextRect, thePoint) then
    begin
    self.selectedItemIndex := minBoundItemIndex;
    self.invalidate;
    self.changeSoftBounds;
    end
  else if ptInRect(maxTextRect, thePoint) then
    begin
    self.selectedItemIndex := maxBoundItemIndex;
    self.invalidate;
    self.changeSoftBounds; 
    end;
  end;

procedure KfRealSliderArrayBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  { process slider arrow keys first }
  if sliders[0].enabled and not sliders[0].readOnly then
    case key of
      VK_HOME, VK_END, VK_DOWN, VK_LEFT, VK_UP, VK_RIGHT, VK_NEXT, VK_PRIOR:
        if self.aSliderIsSelected then
          sliders[self.selectedItemIndex - 1].doKeyDown(sender, key, shift);
      end;
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) then
    if (valueTextItemIndex <> 0) and (self.selectedItemIndex = valueTextItemIndex) then
      begin
      if sliders[0].enabled and not sliders[0].readOnly then self.adjustValue;
      end
    else if (self.selectedItemIndex = unitTextItemIndex) then
      self.selectNextUnitInSet(ssShift in shift, ssCtrl in shift)
    else if (self.selectedItemIndex = minBoundItemIndex) or (self.selectedItemIndex = maxBoundItemIndex) then
      self.changeSoftBounds
    else if self.aSliderIsSelected then
      begin
      if sliders[0].enabled and not sliders[0].readOnly then self.adjustValue;
      end;
  end;

procedure KfRealSliderArrayBrowserComponent.changeSoftBounds;
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

function KfRealSliderArrayBrowserComponent.aSliderIsSelected: boolean;
  begin
  result := (self.selectedItemIndex > kItemLabel) and (self.selectedItemIndex <= arraySize);
  end;

function KfRealSliderArrayBrowserComponent.sliderPositionFromValue(value: single): integer;
  begin
  if softMax - softMin = 0.0 then
    result := 0
  else if value <= softMin then
    result := 0
  else if value >= softMax then
    result := 100
  else result := round(100.0 * (value - softMin) / (softMax - softMin));
  end;

function KfRealSliderArrayBrowserComponent.valueFromSliderPosition(position: integer): single;
  begin
  result := softMin + position / 100.0 * (softMax - softMin);
  end;

procedure KfRealSliderArrayBrowserComponent.updateEnabling;
  var i: integer;
	begin
  inherited updateEnabling;
  for i := 0 to arraySize - 1 do
    begin
    sliders[i].enabled := model <> nil;
    sliders[i].readOnly := self.readOnly;
    end;
  end;

procedure KfRealSliderArrayBrowserComponent.updateModelValues;
  var
    i: integer;
    oldValues: valuesArray;
    changed: boolean;
  begin
  if model = nil then exit;
  if self.collapsed then exit;
  oldValues := currentValues;
  self.updateCurrentValue(-1);
  changed := false;
  for i := 0 to 11 do if oldValues[i] <> currentValues[i] then
    begin
    changed := true;
    break;
    end;
  if changed then
    begin
    updateDisplay;
    invalidate;
    end;
  end;

procedure KfRealSliderArrayBrowserComponent.updateCurrentValue(aFieldIndex: integer);
  var
    i: integer;
    changed: boolean;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldFloat) then
    begin
    if aFieldIndex <> -1 then
      model.transferField(kGetField, currentValues[aFieldIndex], aspect.fieldNumber, kFieldFloat,
        aFieldIndex, aspect.deriveMethod(groupItem.derivedIndex), nil)
    else for i := 0 to arraySize - 1 do
      model.transferField(kGetField, currentValues[i], aspect.fieldNumber, kFieldFloat,
        i, aspect.deriveMethod(groupItem.derivedIndex), nil);
    if aFieldIndex <> -1 then
      begin
      if currentValues[aFieldIndex] < softMin then softMin := currentValues[aFieldIndex];
      if currentValues[aFieldIndex] > softMax then softMax := currentValues[aFieldIndex];
      end
    else for i := 0 to arraySize - 1 do
      begin
      if currentValues[i] < softMin then softMin := currentValues[i];
      if currentValues[i] > softMax then softMax := currentValues[i];
      end;
    end
  else
    for i := 0 to arraySize - 1 do currentValues[i] := 0.0;
  end;

procedure KfRealSliderArrayBrowserComponent.updateDisplay;
  var i: integer;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldFloat) then
    for i := 0 to arraySize - 1 do
      sliders[i].currentValue := self.sliderPositionFromValue(currentValues[i])
  else
    begin
    for i := 0 to arraySize - 1 do sliders[i].currentValue := 0;
    for i := 0 to arraySize - 1 do sliders[i].enabled := false;
    end;
  end;

procedure KfRealSliderArrayBrowserComponent.updateAspectInfo;
  begin
  self.calculateBoundsForCurrentUnit;
  self.updateCurrentValue(-1);
  self.updateDisplay;
  end;

procedure KfRealSliderArrayBrowserComponent.sliderMouseDown(Sender: TObject);
  begin
  if model = nil then exit;
  if (sliders[currentIndex] <> sender) then
    begin
    sliders[currentIndex].hasUnofficialFocus := false;
    sliders[currentIndex].repaint;
    end;
  currentIndex := KfSlider(sender).tag;
  self.selectedItemIndex := currentIndex + 1;
  sliders[currentIndex].hasUnofficialFocus := true;
  sliders[currentIndex].invalidate;
  if not self.focused then
    self.setFocus
  else
    self.invalidate;
  end;

procedure KfRealSliderArrayBrowserComponent.sliderMouseMove(Sender: TObject);
	begin
  if model = nil then exit;
  currentValues[currentIndex] := self.valueFromSliderPosition(sliders[currentIndex].currentValue);
  self.invalidate;
  end;

procedure KfRealSliderArrayBrowserComponent.sliderMouseUp(Sender: TObject);
 	begin
  if model = nil then exit;
  currentValues[currentIndex] := self.valueFromSliderPosition(sliders[currentIndex].currentValue);
  GardenForm.doCommand(GsChangeDomainRealCommand.createCommand(model, currentValues[currentIndex],
    aspect.fieldNumber, currentIndex, aspect.deriveMethod(groupItem.derivedIndex)));
  self.invalidate;
  end;

procedure KfRealSliderArrayBrowserComponent.sliderKeyDown(Sender: TObject);
  begin
  self.sliderMouseUp(sender);
  end;

function KfRealSliderArrayBrowserComponent.maxWidth: integer;
  begin
  result := intMax(kLeftRightGap * 2 + self.labelWidth,
    self.minScaleWidthWithBounds(aspect.unitSet(groupItem.derivedIndex), aspect.unitModel(groupItem.derivedIndex),
        softMin, softMax));
  end;

function KfRealSliderArrayBrowserComponent.minWidth(requestedWidth: integer): integer;
  var
    widthForLongestWord, widthWithBounds, widthWithoutBounds: integer;
  begin
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

function KfRealSliderArrayBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + sliders[0].height * arraySize
    + self.textHeight + kTopBottomGap * 3;
  end;

procedure KfRealSliderArrayBrowserComponent.resizeElements;
  var i, y: integer;
  begin
  y := self.collapsedHeight + kTopBottomGap;
  for i := 0 to arraySize - 1 do
    begin
	  sliders[i].left := kLeftRightGap;
    sliders[i].width := self.width - kLeftRightGap * 2;
	  sliders[i].top := y;
    y := y + sliders[i].height;
    end;
  end;

function KfRealSliderArrayBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (model <> nil) then
    result := maxBoundItemIndex
  else
    result := kItemLabel;
  end;

function KfRealSliderArrayBrowserComponent.nextSelectedItemIndex(goForward: boolean): integer;
  begin
  result := self.selectedItemIndex;
  if aSliderIsSelected then
    begin
    if goForward then
      begin
      if self.selectedItemIndex = arraySize then result := minBoundItemIndex
      else result := self.selectedItemIndex + 1;
      end
    else
      begin
      if self.selectedItemIndex = kItemLabel + 1 then result := kItemLabel
      else result := self.selectedItemIndex - 1;
      end;
    end
  else if self.selectedItemIndex = kItemNone then
    begin
    result := self.selectedItemIndex;
    end
  else if self.selectedItemIndex = kItemLabel then
    begin
    if goForward then
      begin
      result := kItemLabel + 1;
      end
    else result := kItemNone;
    end
  else if self.selectedItemIndex = minBoundItemIndex then
    begin
    if goForward then
      begin
      if readOnly then result := unitTextItemIndex else result := valueTextItemIndex;
      end
    else
      begin
      result := arraySize;
      end;
    end
  else if self.selectedItemIndex = valueTextItemIndex then
    begin
    if goForward then result := unitTextItemIndex else result := minBoundItemIndex;
    end
  else if self.selectedItemIndex = unitTextItemIndex then
    begin
    if goForward then result := maxBoundItemIndex
    else
      begin
      if readOnly then result := minBoundItemIndex else result := valueTextItemIndex;
      end;
    end
  else if self.selectedItemIndex = maxBoundItemIndex then
    begin
    if goForward then result := self.maxSelectedItemIndex + 1 else result := unitTextItemIndex;
    end;
  end;

procedure KfRealSliderArrayBrowserComponent.Paint;
var
  softMinInCurrentUnit, softMaxInCurrentUnit: single;
  currentValueInCurrentUnit: array[0..12] of single;
  Rect: TRect;
  minString, valueString, maxString, unitString: string;
  i: integer;
begin
  { ask sliders to update themselves based on whether they are selected }
  if self.aSliderIsSelected then self.currentIndex := self.selectedItemIndex - 1;
  for i := 0 to arraySize - 1 do
    begin
    sliders[i].hasUnofficialFocus := (self.selectedItemIndex = i + 1);
    end;
  inherited paint;
  if self.collapsed then exit;
  rect := getClientRect;
  softMinInCurrentUnit := toCurrentUnit(softMin);
  softMaxInCurrentUnit := toCurrentUnit(softMax);
  if drawWithoutBounds then
    begin
    minString := '';
    maxString := '';
    end
  else
    begin
    minString := digitValueString(softMinInCurrentUnit);
    maxString := digitValueString(softMaxInCurrentUnit);
    end;
  for i := 0 to arraySize - 1 do
    currentValueInCurrentUnit[i] := toCurrentUnit(currentValues[i]);
  valueString := '[' + intToStr(self.currentIndex + 1) + ']  ';
  valueString := valueString + digitValueString(currentValueInCurrentUnit[currentIndex]);
  unitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  with minTextRect do
    begin
    left := rect.left + kLeftRightGap;
    right := left + self.canvas.textWidth(minString);
    top := sliders[arraySize-1].top + sliders[arraySize-1].height;
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
    left := valueTextRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth(unitString);
    top := minTextRect.top;
    bottom := top + self.textHeight;
    end;
  with Canvas do
    begin
    self.drawText(minString, minTextRect, true, (self.selectedItemIndex = minBoundItemIndex));
    self.drawText(valueString, valueTextRect, not self.readOnly,
      (self.selectedItemIndex = valueTextItemIndex) and (sliders[0].enabled));
    self.drawText(unitString, unitTextRect, true,
      (self.selectedItemIndex = unitTextItemIndex) and (sliders[0].enabled));
    self.drawText(maxString, maxTextRect, true, (self.selectedItemIndex = maxBoundItemIndex));
    end;
end;

end.

unit Ubrowscv;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowscv: Browser component for s curve. Displays little graph of curve, xy coordinates
of two points that define curve, units. Curve is calculated by giving parameters
to s curve function (ueutils) to generate smooth function. User can drag each of the
two points that define the curve or click on any of the four numbers to change it
by itself. User can click on bound to change for X direction only -- y axis is always
from zero to one. Because curve is being drawn during paint method and therefore
s curve function is being calculated during paint method, uses special s curve function
that returns whether or not an exception occurred during the function. If so, the paint
method stops without finishing the curve. }

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom;

const
  kItemPoint1 = 1;
  kItemPoint2 = 2;
  kItemX1 = 3;
  kItemY1 = 4;
  kItemX2 = 5;
  kItemY2 = 6;
  kItemSCurveUnitText = 7;

type KfSCurveBrowserComponent = class(KfBrowserComponent)
	public
  editEnabled: boolean;
  draggingPoint: boolean;
  currentValues: array[0..3] of single;
  minXValue: single;
  maxXValue: single;
  belowZero: boolean;
  currentUnitIndex: integer;
  graphRect: TRect;
  pointRects: array[0..1] of TRect;
  valueRects: array[0..3] of TRect;
  unitTextRect: TRect;
  procedure initialize; override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure paint; override;
  function maxSelectedItemIndex: integer; override;
  procedure doMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  function checkModelValuesAgainstSoftBounds(aFieldIndex: integer): boolean;
  function checkOneValueAgainstSoftBounds(i: integer): boolean;
  procedure adjustValue;
  procedure adjustValueByPixels(index, xChange, yChange: integer);
  procedure selectNextUnitInSet(shift: boolean);
  function stringForIndex(index: integer): string;
  function boundForIndex(index: integer; isMinimum: boolean): single;
  function xValueFromPointPosition(aPoint: TPoint): single;
  function yValueFromPointPosition(aPoint: TPoint): single;
  function pointFromXY(x, y: single): TPoint;
  procedure setPointAfterMouseUp(index: integer; x, y: integer);
  procedure drawSCurve(draw: boolean; var failed: boolean);
  procedure keepPointInGraphRect(var point: TPoint);
  procedure keepPointsFromCrossing(var point0: TPoint; var point1: TPoint; movingPoint: integer);
  function toModelUnit(value: single): single;
  function toCurrentUnit(value: single): single;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, umconsts, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct, usliders;

{ ----------------------------------------------------------------------------- initialize }
procedure KfSCurveBrowserComponent.initialize;
  var
    temp: single;
	begin
  self.onMouseDown := self.doMouseDown;
  self.onMouseMove := self.doMouseMove;
  self.onMouseUp := self.doMouseUp;
  self.onKeyDown := self.doKeyDown;
  self.draggingPoint := false;
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
  minXValue := groupItem.boundSoftLower;
  maxXValue := groupItem.boundSoftUpper;
  self.belowZero := false;
  if (minXValue < 0.0) then
    begin
    temp := abs(maxXValue);
    maxXValue := abs(minXValue);
    minXValue := temp;
    self.belowZero := true;
    end;
  end;

{ ------------------------------------------------------------------------ mouse and key }
procedure KfSCurveBrowserComponent.doMouseDown(sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
    i: integer;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseDown(sender, button, shift, x, y);
  if not self.editEnabled then exit;
  thePoint := Point(x, y);
  for i := 0 to 1 do
    if ptInRect(pointRects[i], thePoint) then
      begin
      self.selectedItemIndex := kItemPoint1 + i;
      self.draggingPoint := true;
      self.invalidate; { should be only point rect }
      end;
  if not self.focused then self.setFocus;
  end;

procedure KfSCurveBrowserComponent.doMouseMove(sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    i, index: integer;
    saveValues: array[0..3] of single;
    failed: boolean;
    points: array[0..1] of TPoint;
	begin
  if not self.draggingPoint then exit;
  if not self.editEnabled then exit;
  for i := 0 to 3 do saveValues[i] := currentValues[i];
  index := selectedItemIndex - kItemPoint1;
  try
  points[index] := point(x, y);
  if index = 0 then  { set other point }
    points[1] := pointFromXY(currentValues[2], currentValues[3])
  else
    points[0] := pointFromXY(currentValues[0], currentValues[1]);
  self.keepPointInGraphRect(points[index]);
  self.keepPointsFromCrossing(points[0], points[1], index);
  currentValues[index * 2] := self.xValueFromPointPosition(points[index]);
  currentValues[index * 2 + 1] := self.yValueFromPointPosition(points[index]);
  self.drawSCurve(false, failed);
  if failed then
    for i := 0 to 3 do currentValues[i] := saveValues[i];
  self.invalidate;  { whole }
  except
    { do nothing - to prevent repetitive messages if problem }
  end;
  end;

procedure KfSCurveBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    thePoint: TPoint;
    i: integer;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  thePoint := Point(x, y);
  for i := 0 to 1 do
    if (ptInRect(pointRects[i], thePoint)) and (self.editEnabled) then
      begin
      if self.draggingPoint then
        begin
        self.draggingPoint := false;
        self.setPointAfterMouseUp(i, x, y);
        self.invalidate;
        exit;
        end; 
      end;
  if self.draggingPoint then self.draggingPoint := false;
  for i := 0 to 3 do
    if (ptInRect(valueRects[i], thePoint)) and (self.editEnabled) then
      begin
      self.selectedItemIndex := i + kItemX1;
      self.invalidate;
      self.adjustValue;
      exit;
      end;
  if ptInRect(unitTextRect, thePoint) then { can still do this if disabled }
    begin
    self.selectedItemIndex := kItemSCurveUnitText;
    self.invalidate;
    self.selectNextUnitInSet(ssShift in shift);
    end;
  end;

procedure KfSCurveBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  var
    increment, index: integer;
  begin
  inherited doKeyDown(sender, key, shift);
  increment := 2;
  if key = VK_RETURN then
    case self.selectedItemIndex of
      kItemX1, kItemY1, kItemX2, kItemY2: self.adjustValue;
      kItemSCurveUnitText:  self.selectNextUnitInSet(ssShift in shift);
      end
  else if (self.selectedItemIndex = kItemPoint1) or (self.selectedItemIndex = kItemPoint2) then
    begin
    index := self.selectedItemIndex - kItemPoint1;
    case key of
      VK_LEFT: self.adjustValueByPixels(index, -increment, 0);
      VK_RIGHT: self.adjustValueByPixels(index, increment, 0);
      VK_DOWN: self.adjustValueByPixels(index, 0, increment);
      VK_UP: self.adjustValueByPixels(index, 0, -increment);
      end;
    end;
  end;

{ --------------------------------------------------------- functions called by mouse and key }
procedure KfSCurveBrowserComponent.adjustValue;
  var
{$IFDEF WINDOWS}
    newString: string;
{$ELSE}
    newString: ansistring;
{$ENDIF}
    oldString, nameString, prompt, currentUnitString: string;
    newValue, oldValue, valueInCurrentUnit: single;
    index: integer;
    failed: boolean;
    points: array[0..1] of TPoint;
  begin
  if self.selectedItemIndex < kItemX1 then exit;
  index := self.selectedItemIndex - kItemX1;
  oldValue := currentValues[index];
  nameString := copy(self.caption, 1, 30);
  currentUnitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  if length(nameString) = 30 then nameString := nameString + '...';
  if not odd(index) then  { x }
    begin
    valueInCurrentUnit := toCurrentUnit(currentValues[index]);
    newString := digitValueString(valueInCurrentUnit);  
    end
  else  { y }  { don't convert y value - is always 0-1 }
    begin
    valueInCurrentUnit := currentValues[index];
    newString := floatToStrF(valueInCurrentUnit, ffFixed, 7, 2);
    end;
  oldString := newString;
  prompt := 'Enter a new value for ' + chr(13) + chr(13) + nameString + chr(13) + chr(13)
    + ' ' + stringForIndex(index) + ' (in ' + currentUnitString + ').';
  if inputQuery('Enter precise value', prompt, newString) then
    if (newString <> oldString) and (boundForString(newString, aspect.fieldType, newValue)) then
      begin
      if not odd(index) then
        currentValues[index] := toModelUnit(newValue)
      else
        currentValues[index] := newValue;
      points[0] := self.pointFromXY(currentValues[0], currentValues[1]);
      points[1] := self.pointFromXY(currentValues[2], currentValues[3]);
      self.keepPointInGraphRect(points[(index - 1) div 2]);
      self.keepPointsFromCrossing(points[0], points[1], (index - 1) div 2);
      self.drawSCurve(false, failed);
      if failed then
        begin
        currentValues[index] := oldValue;
        showMessage('Invalid value.');
        end
      else
        begin
        { don't create change point command here, because only one float is changed }
        GardenForm.doCommand(GsChangeDomainRealCommand.createCommand(model, currentValues[index],
            aspect.fieldNumber, index, aspect.deriveMethod(groupItem.derivedIndex)));
        self.invalidate;
        end;
      end;
  end;

procedure KfSCurveBrowserComponent.adjustValueByPixels(index, xChange, yChange: integer);
  var
    points: array[0..1] of TPoint;
    oldX, oldY: single;
    failed: boolean;
    xIndex, yIndex: integer;
  begin
  xIndex := index * 2;
  yIndex := index * 2 + 1;
  oldX := currentValues[xIndex];
  oldY := currentValues[yIndex];
  points[0] := pointFromXY(currentValues[0], currentValues[1]);
  points[1] := pointFromXY(currentValues[2], currentValues[3]);
  points[index].x := points[index].x + xChange;
  points[index].y := points[index].y + yChange;
  self.keepPointInGraphRect(points[index]);
  self.keepPointsFromCrossing(points[0], points[1], index);
  self.drawSCurve(false, failed);
  if failed then
    begin
    currentValues[xIndex] := oldX;
    currentValues[yIndex] := oldY;
    end
  else
    begin
    currentValues[xIndex] := self.xValueFromPointPosition(points[index]);
    currentValues[yIndex] := self.yValueFromPointPosition(points[index]);
    GardenForm.doCommand(GsChangeDomainSCurvePointCommand.createCommand(model, currentValues[xIndex],
      currentValues[yIndex], aspect.fieldNumber, index));
    self.invalidate;
    end;
  end;

procedure KfSCurveBrowserComponent.selectNextUnitInSet(shift: boolean);
  begin
  { no layer or derivation for this - s curve cannot be layer or absolute/relative }
  if shift then
    currentUnitIndex := GetPreviousUnitEnumInUnitSet(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex)
  else
    currentUnitIndex := GetNextUnitEnumInUnitSet(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  groupItem.setUnit(currentUnitIndex); 
  self.updateDisplay;
  end;

procedure KfSCurveBrowserComponent.setPointAfterMouseUp(index: integer; x, y: integer);
  var
    xIndex, yIndex: integer;
    thePoint: TPoint;
  begin
  self.selectedItemIndex := kItemPoint1 + index;
  thePoint := point(x,y);
  xIndex := index * 2;
  yIndex := index * 2 + 1;
  currentValues[xIndex] := self.xValueFromPointPosition(thePoint);
  currentValues[yIndex] := self.yValueFromPointPosition(thePoint);
  { change x and y values }
  GardenForm.doCommand(GsChangeDomainSCurvePointCommand.createCommand(model, currentValues[xIndex],
    currentValues[yIndex], aspect.fieldNumber, index));
  end;

procedure KfSCurveBrowserComponent.keepPointInGraphRect(var point: TPoint);
  begin
  if point.x < graphRect.left then point.x := graphRect.left;
  if point.x > graphRect.right then point.x := graphRect.right;
  if point.y < graphRect.top then point.y := graphRect.top;
  if point.y > graphRect.bottom then point.y := graphRect.bottom;
  end;

procedure KfSCurveBrowserComponent.keepPointsFromCrossing(var point0: TPoint; var point1: TPoint;
  movingPoint: integer);
  var
    closestPoint: TPoint;
    proportion, minDifference: single;
  begin
  proportion := 0.01;
  minDifference := (maxXValue - minXValue) * proportion;
  closestPoint := pointFromXY(minDifference, 0);
  closestPoint.x := intMax(1, closestPoint.x - graphRect.left);
  if movingPoint = 0 then
    begin
    if point0.x > point1.x - closestPoint.x then
      begin
      {debugPrint('md ' + floatToStr(minDifference) + ' movingPoint ' + intToStr(movingPoint) + ' p0 ' + intToStr(point0.x)
        + ' p1 ' + intToStr(point1.x) + ' cp ' + intToStr(closestPoint.x));}
      point0.x := point1.x - closestPoint.x;
      end;
    end
  else
    begin
    if point1.x < point0.x + closestPoint.x then
      begin
      {debugPrint('p0 ' + intToStr(point0.x) + ' p1 ' + intToStr(point1.x) + ' cp '
        + intToStr(closestPoint.x));}
      point1.x := point0.x + closestPoint.x;
      end;
    end;
  end;

{ ------------------------------------------------------------------------------- updating }
procedure KfSCurveBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  if (model = nil) or (self.readOnly) then
    self.editEnabled := false
  else
    self.editEnabled := true;
  end;

procedure KfSCurveBrowserComponent.updateModelValues;
  var
    i: integer;
    oldValues: array[0..3] of single;
    changed: boolean;
  begin
  if model = nil then exit;
  if self.collapsed then exit;
  for i := 0 to 3 do oldValues[i] := currentValues[i];
  self.updateCurrentValue(-1);
  changed := false;
  for i := 0 to 3 do if oldValues[i] <> currentValues[i] then
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

procedure KfSCurveBrowserComponent.updateCurrentValue(aFieldIndex: integer);
  var
    i: integer;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldFloat) then
    begin
    if aFieldIndex <> -1 then
      begin
      model.transferField(kGetField, currentValues[aFieldIndex],
        aspect.fieldNumber, kFieldFloat, aFieldIndex, aspect.deriveMethod(groupItem.derivedIndex), nil);
      end
    else for i := 0 to 3 do
      begin
      model.transferField(kGetField, currentValues[i],
        aspect.fieldNumber, kFieldFloat, i, aspect.deriveMethod(groupItem.derivedIndex), nil);
      end;
    if currentValues[0] < minXValue then minXValue := currentValues[0];
    if currentValues[2] > maxXValue then maxXValue := currentValues[2];
    end
  else
    self.editEnabled := false;
  end;

procedure KfSCurveBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldFloat) then
    self.invalidate
  else
    self.editEnabled := false;
  end;

{ ----------------------------------------------------------------------------------------- painting }
var gRecursing: boolean;

procedure KfSCurveBrowserComponent.Paint;
var
  minYTextRect, maxYTextRect, minXTextRect, maxXTextRect, pointTextRect: TRect;
  maxXValueInCurrentUnit, minXValueInCurrentUnit: single;
  minXString, maxXString, minYString, maxYString, currentUnitString: string;
  currentValuesInCurrentUnit: array[0..3] of single;
  valueStrings: array[0..3] of string;
  i, xPos, totalWidth: integer;
  thePoint: TPoint;
  failed: boolean;
begin
  try
  inherited paint;
  if self.collapsed then exit;
  if gRecursing then
    begin
   { errorMessage('KfSCurveBrowserComponent.Paint: recursion in paint method'); }
    gRecursing := false;
    exit;
    end;
  gRecursing := true;
  { convert bounds and values from model to current unit ( don't convert Y value) }
  { this is only for writing strings; use model unit for drawing points }
  minXValueInCurrentUnit := toCurrentUnit(minXValue);
  if not self.belowZero then
    begin
    maxXValueInCurrentUnit := toCurrentUnit(maxXValue);
    currentValuesInCurrentUnit[0] := toCurrentUnit(currentValues[0]);
    currentValuesInCurrentUnit[2] := toCurrentUnit(currentValues[2]);
    end
  else
    begin
    maxXValueInCurrentUnit := toCurrentUnit(-maxXValue);
    currentValuesInCurrentUnit[0] := toCurrentUnit(-currentValues[0]);
    currentValuesInCurrentUnit[2] := toCurrentUnit(-currentValues[2]);
    end;
  currentValuesInCurrentUnit[1] := currentValues[1];
  currentValuesInCurrentUnit[3] := currentValues[3];
  { strings to write for number }
  minXString := digitValueString(minXValueInCurrentUnit);
  maxXString := digitValueString(maxXValueInCurrentUnit);
  minYString := '0.0';
  maxYString := '1.0';
  currentUnitString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex);
  for i := 0 to 3 do
    if (i = 0) or (i = 2) then
      valueStrings[i] := stringForIndex(i) + ' ' +
        digitValueString(currentValuesInCurrentUnit[i])
    else
      valueStrings[i] := stringForIndex(i) + ' ' +
        floatToStrF(currentValuesInCurrentUnit[i], ffFixed, 7, 2);
  { calculate all rectangles to draw text, points }
  with graphRect do
    begin
	  left := kLeftRightGap + self.canvas.textWidth(minYString);
    right := self.width - kLeftRightGap;
	  top := self.collapsedHeight;
    bottom := self.height - self.textHeight * 2 - kBetweenGap - kTopBottomGap * 2;
    end;
  with minXTextRect do
    begin
    left := graphRect.left;
    right := left + self.canvas.textWidth(minXString);
    top := graphRect.bottom + 1;
    bottom := top + self.textHeight;
    end;
  with maxXTextRect do
    begin
    right := graphRect.right;
    left := right - self.canvas.textWidth(maxXString);
    top := minXTextRect.top;
    bottom := top + self.textHeight;
    end;
  with minYTextRect do
    begin
    left := graphRect.left - self.canvas.textWidth(maxYString) - 1;
    right := graphRect.left - 1;
    top := graphRect.bottom - self.textHeight;
    bottom := graphRect.bottom;
    end;
  with maxYTextRect do
    begin
    left := graphRect.left - self.canvas.textWidth(maxYString) - 1;
    right := graphRect.left - 1;
    top := graphRect.top;
    bottom := top + self.textHeight;
    end;
  for i := 0 to 1 do
    with pointRects[i] do
      begin
      thePoint := self.pointFromXY(currentValues[i * 2], currentValues[i * 2 + 1]);
      left := thePoint.x - kSliderDraggerHeight div 2;
      right := left + kSliderDraggerHeight;
      top := thePoint.y - kSliderDraggerHeight div 2;
      bottom := top + kSliderDraggerHeight;
      end;
  totalWidth := 0;   { center point value texts }
  for i := 0 to 3 do totalWidth := totalWidth + canvas.textWidth(valueStrings[i]);
  xPos := self.left + self.width div 2 - totalWidth div 2;
  for i := 0 to 3 do
    with valueRects[i] do
      begin
      left := xPos;
      right := left + canvas.textWidth(valueStrings[i]);
      top := minXTextRect.bottom + kLeftRightGap div 2;
      bottom := top + self.textHeight;
      xPos := xPos + (right - left) + kLeftRightGap * 2;
      end;
  with unitTextRect do { center unit text }
    begin
    left := graphRect.left + (graphRect.right - graphRect.left) div 2
       - canvas.textWidth(currentUnitString) div 2;
    right := left + canvas.textWidth(currentUnitString);
    top := minXTextRect.top;
    bottom := top + self.textHeight;
    end;
  { finally, draw all text }
  with Canvas do
    begin
    { draw lines black if enabled, else gray }
    if self.editEnabled then
      pen.color := clBtnText
    else
      pen.color := clBtnShadow;
    { draw axes }
    with graphRect do
      begin
      moveTo(left, top);
      lineTo(left, bottom);
      lineTo(right, bottom);
      end;
    { draw axis labels }
    self.drawText(minYString, minYTextRect, false, false);
    self.drawText(maxYString, maxYTextRect, false, false);
    self.drawText(minXString, minXTextRect, false, false);
    self.drawText(maxXString, maxXTextRect, false, false);
    { draw value texts }
    for i := 0 to 3 do
      self.drawText(valueStrings[i], valueRects[i], true,
        (self.editEnabled) and (i = self.selectedItemIndex - kItemX1));
    { draw unit string }
    self.drawText(currentUnitString, unitTextRect, true,
      (self.editEnabled) and (self.selectedItemIndex = kItemSCurveUnitText));
    { draw point rectangles and labels (labels first because brush color will affect them }
    for i := 0 to 1 do
      begin
      { first put text at lower right of point }
      pointTextRect := rect(pointRects[i].right + 5, pointRects[i].top,
        pointRects[i].right + 5 + textWidth(intToStr(i+1)), pointRects[i].top + self.textHeight);
      { adjust point text rectangle if could not be seen }
      if pointTextRect.right >= graphRect.right then offsetRect(pointTextRect, -20, 0);
      if pointTextRect.bottom >= graphRect.bottom then offsetRect(pointTextRect, 0, -10);
      self.drawText(intToStr(i + 1), pointTextRect, false, false);
      end;
    { draw curve }
    drawSCurve(true, failed);
    if not failed then { draw points }
      for i := 0 to 1 do
        KfDrawButton(self.canvas, pointRects[i], (self.selectedItemIndex = kItemPoint1 + i), self.editEnabled);
  end;
finally
  gRecursing := false;
end;
end;

procedure KfSCurveBrowserComponent.drawSCurve(draw: boolean; var failed: boolean);
  var
    i: integer;
    x, y: single;
    thisPoint: TPoint;
    curve: SCurveStructure;
    numPoints: integer;
  begin
  failed := true;
  if model = nil then exit;
  numPoints := 100;
  { assumes that zero does NOT fall between x1 and x2, though one of these could be zero }
  curve.x1 := currentValues[0];
  curve.y1 := currentValues[1];
  curve.x2 := currentValues[2];
  curve.y2 := currentValues[3];
  if draw then canvas.pen.color := clBtnShadow;
  try
    Utils_CalcSCurveCoeffsWithResult(curve, failed);
    if failed then exit;
    for i := 0 to numPoints - 1 do
      begin
      x := 0.0; { delphi 2.0 wants this }
      y := 0.0;
      try
        x := minXValue + (1.0 * i / numPoints) * (maxXValue - minXValue);
      except
        exit;
      end;
      y := scurveWithResult(x, curve.c1, curve.c2, failed);
      if failed then exit;
      if draw then
        begin
        thisPoint := pointFromXY(x, y);
        if i = 0 then
          canvas.moveTo(thisPoint.x, thisPoint.y)
        else
          canvas.lineTo(thisPoint.x, thisPoint.y);
        end;
      end;
  except
    exit;
  end;
  failed := false;
  end;

function KfSCurveBrowserComponent.maxWidth: integer;
  begin
  result := kLeftRightGap * 2 + intMax(self.labelWidth, kEditMaxWidth);
  end;

function KfSCurveBrowserComponent.minWidth(requestedWidth: integer): integer;
  begin
  { CFK FIX - look at this to figure best max and min }
  { this is not a very good approximation }
  result := kLeftRightGap * 2 + intMax(self.longestLabelWordWidth,
    self.formTextWidth('0.0') + (graphRect.right - graphRect.left));
  end;

function KfSCurveBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + kGraphHeight + self.textHeight * 3
    + kBetweenGap * 2 + kTopBottomGap;
  end;

function KfSCurveBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (self.editEnabled) then
    result := kItemSCurveUnitText
  else
    result := kItemLabel;
  end;

{ ---------------------------------------------------------------------------- utilities }
function KfSCurveBrowserComponent.toModelUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), currentUnitIndex,
      aspect.unitModel(groupItem.derivedIndex), value);
  end;

function KfSCurveBrowserComponent.toCurrentUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex),
      aspect.unitModel(groupItem.derivedIndex), currentUnitIndex, value);
  end;

function KfSCurveBrowserComponent.xValueFromPointPosition(aPoint: TPoint): single;
  begin
  result := minXValue + (maxXValue - minXValue)
    * (aPoint.x - graphRect.left) / (graphRect.right - graphRect.left);
  end;

function KfSCurveBrowserComponent.yValueFromPointPosition(aPoint: TPoint): single;
  begin
  result := 1.0 - (aPoint.y - graphRect.top) / (graphRect.bottom - graphRect.top);
  end;

function KfSCurveBrowserComponent.pointFromXY(x, y: single): TPoint;
  begin
  result.x := graphRect.left +
    round(((x - minXValue) / (maxXValue - minXValue))
      * (graphRect.right - graphRect.left));
  result.y := graphRect.top + round((1.0 - y) * (graphRect.bottom - graphRect.top));
  end;

function KfSCurveBrowserComponent.stringForIndex(index: integer): string;
  begin
  case index of
    0: result := 'x1';
    1: result := 'y1';
    2: result := 'x2';
    3: result := 'y2';
    end;
  end;

function KfSCurveBrowserComponent.checkModelValuesAgainstSoftBounds(aFieldIndex: integer): boolean;
  var i: integer;
  begin
  result := false;
  if aFieldIndex <> -1 then
    result := self.checkOneValueAgainstSoftBounds(aFieldIndex)
  else
    for i := 0 to 3 do if self.checkOneValueAgainstSoftBounds(i) then result := true;
  end;

function KfSCurveBrowserComponent.checkOneValueAgainstSoftBounds(i: integer): boolean;
  var
   oldValue, bound: single;
  begin
  oldValue := currentValues[i];
  bound := boundForIndex(i, true);
  if currentValues[i] < bound then bound := currentValues[i];
  bound := boundForIndex(i, false);
  if currentValues[i] > bound then bound := currentValues[i];
  result := (currentValues[i] <> oldValue);
  end;

function KfSCurveBrowserComponent.boundForIndex(index: integer; isMinimum: boolean): single;
  begin
  result := 0.0;
  case index of
    0, 2: { x }
      if isMinimum then
        result := minXValue
      else
        result := maxXValue;
    1, 3: { y }
      if isMinimum then
        result := 0.0
      else
        result := 1.0;
    end;
  end;

begin
gRecursing := false;
end.
(* not using
procedure KfSCurveBrowserComponent.checkPointsForFailure(var movingPoint: TPoint; var nonMovingPoint: TPoint);
  var
    failed: boolean;
    distanceFromLeft, distanceFromRight, distanceFromTop, distanceFromBottom: integer;
    distanceFromOtherPointX, smallestDistance, increment: integer;
  begin
  failed := true;
  increment := 5;
  while failed do
    begin
    failed := self.drawSCurve(false);
    if failed then
      begin
      distanceFromLeft := movingPoint.x - graphRect.left;
      distanceFromRight := graphRect.right - movingPoint.x;
      distanceFromTop := movingPoint.y - graphRect.top;
      distanceFromBottom := graphRect.bottom - movingPoint.y;
      distanceFromOtherPointX := movingPoint.x - nonMovingPoint.x;
      smallestDistance := intMin(distanceFromLeft, intMin(distanceFromRight,
        intMin(distanceFromTop, intMin(distanceFromBottom, distanceFromOtherPointX))));
      if smallestDistance = distanceFromLeft then movingPoint.x := movingPoint.x + increment
      else if smallestDistance = distanceFromRight then movingPoint.x := movingPoint.x - increment
      else if smallestDistance = distanceFromTop then movingPoint.y := movingPoint.y + increment
      else if smallestDistance = distanceFromBottom then movingPoint.y := movingPoint.y - increment
      else if smallestDistance = distanceFromOtherPointX then
        if distanceFromOtherPointX > 0 then
          movingPoint.x := movingPoint.x + increment
        else
          movingPoint.x := movingPoint.x - increment;
      end;
    end;
  end;
*)


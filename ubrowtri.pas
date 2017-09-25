unit Ubrowtri;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowtri: Browser component for soil texture triangle. This used to be used on the
left side of the browser (numbers side), but now it is used on the right side. Because
it is not a "normal" browser component (BC), it has to have several weird things to make sure
it acts differently for being on the right side. There are a few other right-side
BCs like this. To be clean, there really should have been a right-side
BC class instead and they could have been subclassed from it (but they
weren't planned, they just grew). The system for tabbing is the same as for the left-side
BCs, but this has a pointer to a soil patch and NOT an aspect or groupItem. Because the
aspects for the soil texture triangle (sand, silt, clay) don't change, they are hard-coded
here (note this means you can't change them in the aspects.tab file without changing this file).
The values for divisions between soil-texture classes are hard-coded and taken from Figure
3.1 on p. 39 in Troeh & Thompson, 'Soils and Soil Fertility' (5th ed.) which is based on USDA guidelines.
User can click on layer number to change the layer shown, or move the point around to change
the percentages, or change each percentage separately by clicking on it. The method for making
sure the percentages add up to 100 is not very good, and we had always wanted to have time
to make all the layers show at once somehow.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom, usliders, uesoil, ucollect;

const
  kItemLayerText = 1;
  kItemPoint = 2;
  kItemClay = 3;
  kItemSilt = 4;
  kItemSand = 5;

  kTriangleMaxWidth = 200;
  kTriangleMinWidth = 150;

type

KfSoilTextureTriangleDrawComponent = class(KfBrowserComponent)
	public
  soil: GsSoilPatch;
  editEnabled: boolean;
  claySiltSand: array[0..9] of array[0..2] of single;
  layerIndex: integer;
  draggingPoint: boolean;
  layerTextRect: TRect;
  percentTextRects: array[0..2] of TRect;
  pointRect: TRect;
  triangleRect: TRect;
  constructor create(anOwner: TComponent); override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  function maxSelectedItemIndex: integer; override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  function pointFromClaySand(clay, sand: single): TPoint;
  procedure claySandFromPoint(point: TPoint; var clay: single; var sand: single);
  procedure makeSurePercentsAddUpTo100;
  procedure changePercent(amount: single; index: integer);
  procedure drawSoilTextureTriangle(recalculatePoint: boolean);
  procedure drawTextLabel(text: string; drawRect: TRect; drawBox: boolean);
  function textureString: string;
  procedure adjustTriangleSoItIsEquilateral;
  procedure changeLayer(shift: boolean);
  procedure adjustValue;
  procedure adjustValueByPixels(x, y: integer);
  procedure getClaySiltSand(aFieldIndex: integer);
  procedure drawTextureLine(clay1, sand1, clay2, sand2: single);
  procedure setPercentsAfterMouseUp(x, y: integer);
  procedure doMouseDown(sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer); override;
  procedure doMouseMove(sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
   Integer); override;
  procedure updateFromUpdateEventList(updateList: TListCollection); override;
  function collapsedHeight: integer; override;
  procedure collapseOrExpand(y: integer); override;
  procedure updateSoilPatch(newPatch: GsSoilPatch);
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct, umconsts;

function pctBound(value: single): single;
  begin
  result := min(value, 100.0);
  result := max(result, 0.0);
  end;

function decWrap(index: integer): integer;
  begin
  result := 0;
  case index of
    0: result := 2;
    1: result := 0;
    2: result := 1;
    end;
  end;

constructor KfSoilTextureTriangleDrawComponent.create(anOwner: TComponent);
  begin
  inherited create(anOwner);
  self.model := nil;
  self.soil := nil;
  self.parentFont := true;
  self.tabStop := true;
  self.enabled := true;
  self.selectedItemIndex := kItemNone;
  self.onMouseDown := self.doMouseDown;
  self.onMouseMove := self.doMouseMove;
  self.onMouseUp := self.doMouseUp;
  self.onKeyDown := self.doKeyDown;
  self.draggingPoint := false;
  self.layerIndex := 0;
  self.editEnabled := false;
  self.readOnly := false;
  end;

function KfSoilTextureTriangleDrawComponent.collapsedHeight: integer;
  begin
  result := self.textHeight + kTopBottomGap;
  end;

procedure KfSoilTextureTriangleDrawComponent.collapseOrExpand(y: integer);
  begin
  { do nothing }
  end;

procedure KfSoilTextureTriangleDrawComponent.updateSoilPatch(newPatch: GsSoilPatch);
  begin
  if soil <> newPatch then
  	begin
    soil := newPatch;
    { set model to soil because drawText checks to see if model is nil }
    model := soil;
    self.updateCurrentValue(-1);
    self.updateEnabling;
    end;
  end;

{ -------------------------------------------------------------------------------------- mouse and key }
procedure KfSoilTextureTriangleDrawComponent.doMouseDown(sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
  begin
  if not self.editEnabled then exit;
  thePoint := Point(x, y);
  if ptInRect(pointRect, thePoint) then
    begin
    self.selectedItemIndex := kItemPoint;
    self.draggingPoint := true;
    self.invalidate;
    end;
  if not self.focused then self.setFocus;
  end;

procedure KfSoilTextureTriangleDrawComponent.doMouseMove(sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
	begin
  if not self.draggingPoint then exit;
  if not self.editEnabled then exit;
  thePoint := point(x, y);
  self.claySandFromPoint(thePoint, claySiltSand[layerIndex][0], claySiltSand[layerIndex][2]);
  self.makeSurePercentsAddUpTo100;
  self.invalidate;  { whole }
  end;

procedure KfSoilTextureTriangleDrawComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    thePoint: TPoint;
    i: integer;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  thePoint := Point(x, y);
  if (self.editEnabled) and (self.draggingPoint) then
    begin
    self.draggingPoint := false;
    self.setPercentsAfterMouseUp(x, y);
    exit;
    end;
  if self.draggingPoint then self.draggingPoint := false;
  for i := 0 to 2 do
    if (ptInRect(percentTextRects[i], thePoint)) and (self.editEnabled) then
      begin
      self.selectedItemIndex := i + kItemClay;
      self.invalidate;
      self.adjustValue;
      exit;
      end;
  if ptInRect(layerTextRect, thePoint) and (soil <> nil) then self.changeLayer(ssShift in shift);
  end;

procedure KfSoilTextureTriangleDrawComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  var
    increment: integer;
  begin
  inherited doKeyDown(sender, key, shift);
  increment := 2;
  if key = VK_RETURN then
    case self.selectedItemIndex of
      kItemLayerText: self.changeLayer(ssShift in shift);
      kItemClay, kItemSilt, kItemSand: self.adjustValue;
      end
  else if (self.selectedItemIndex = kItemPoint) then
    begin
    case key of
      VK_LEFT: self.adjustValueByPixels(-increment, 0);
      VK_RIGHT: self.adjustValueByPixels(increment, 0);
      VK_DOWN: self.adjustValueByPixels(0, increment);
      VK_UP: self.adjustValueByPixels(0, -increment);
      end;
    end;
  end;

{ ---------------------------------------------------------------- called by mouse & key functions }
procedure KfSoilTextureTriangleDrawComponent.setPercentsAfterMouseUp(x, y: integer);
  var
    thePoint: TPoint;
  begin
  self.selectedItemIndex := kItemPoint;
  thePoint := point(x,y);
  self.claySandFromPoint(thePoint, claySiltSand[layerIndex][0], claySiltSand[layerIndex][2]);
  self.makeSurePercentsAddUpTo100;
  GardenForm.doCommand(GsChangeDomainSoilTextureCommand.createCommand(soil, claySiltSand[layerIndex][0],
    claySiltSand[layerIndex][1], claySiltSand[layerIndex][2], layerIndex));
  end;

procedure KfSoilTextureTriangleDrawComponent.changeLayer(shift: boolean);
  begin
  if soil = nil then exit;
  self.selectedItemIndex := kItemLayerText;
  if shift then
    layerIndex := layerIndex - 1
  else
    layerIndex := layerIndex + 1;
  if layerIndex > soil.state.numLayers - 1 then layerIndex := 0;
  if layerIndex < 0 then layerIndex := soil.state.numLayers - 1;
  self.invalidate;
  end;

procedure KfSoilTextureTriangleDrawComponent.adjustValue;
  var
{$IFDEF WINDOWS}
    newString: string;
{$ELSE}
    newString: ansistring;
{$ENDIF}
    oldString, nameString, prompt: string;
    newValue: single;
    index: integer;
  begin
  if self.selectedItemIndex < kItemClay then exit;
  index := self.selectedItemIndex - kItemClay;
  case index of
    0: nameString := 'clay';
    1: nameString := 'silt';
    2: nameString := 'sand';
    end;
  newString := floatToStrF(claySiltSand[layerIndex][index], ffFixed, 7, 0);
  oldString := newString;
  prompt := 'Enter a new percentage of ' + chr(13) + chr(13) + nameString + '.';
  if inputQuery('Enter precise value', prompt, newString) then
    if (newString <> oldString) and (boundForString(newString, kFieldFloat, newValue)) then
      begin
      self.changePercent(newValue - claySiltSand[layerIndex][index], index);
      { change clay, silt and sand values }
      GardenForm.doCommand(GsChangeDomainSoilTextureCommand.createCommand(soil, claySiltSand[layerIndex][0],
        claySiltSand[layerIndex][1], claySiltSand[layerIndex][2], layerIndex));
      self.invalidate;
      end;
  end;

procedure KfSoilTextureTriangleDrawComponent.adjustValueByPixels(x, y: integer);
  var thePoint: TPoint;
  begin
  thePoint := self.pointFromClaySand(claySiltSand[layerIndex][0], claySiltSand[layerIndex][2]);
  thePoint.x := thePoint.x + x;
  thePoint.y := thePoint.y + y;
  self.claySandFromPoint(thePoint, claySiltSand[layerIndex][0], claySiltSand[layerIndex][2]);
  self.makeSurePercentsAddUpTo100;
  self.invalidate;
  end;

const
  kClay = 0;
  kSilt = 1;
  kSand = 2;

procedure KfSoilTextureTriangleDrawComponent.makeSurePercentsAddUpTo100;
  var
    clay, silt, sand: single;
  begin
  clay := claySiltSand[layerIndex][0];
  silt := claySiltSand[layerIndex][1];
  sand := claySiltSand[layerIndex][2];

  clay := pctBound(clay);
  silt := pctBound(silt);
  sand := pctBound(sand);

  if clay + sand > 100.0 then sand := pctBound(100.0 - clay);
  silt := 100.0 - clay - sand;
  silt := round(silt);

  claySiltSand[layerIndex][0] := clay;
  claySiltSand[layerIndex][1] := silt;
  claySiltSand[layerIndex][2] := sand;
  end;

procedure KfSoilTextureTriangleDrawComponent.changePercent(amount: single; index: integer);
  var
    items: array[0..2] of single;
  begin
  items[0] := claySiltSand[layerIndex][index];
  items[1] := claySiltSand[layerIndex][decWrap(index)];
  items[2] := claySiltSand[layerIndex][decWrap(decWrap(index))];

  if ((items[0] = 0.0) and (amount < 0)) or ((items[0] = 100.0) and (amount > 0)) then exit;
  items[0] := round(pctBound(items[0] + amount));
  items[1] := round(pctBound(items[1] - amount / 2.0));
  if (items[0] + items[1] > 100.0) then items[1] := 100.0 - items[0];
  items[2] := 100.0 - items[0] - items[1];
  items[2] := round(items[2]);

  claySiltSand[layerIndex][index] := items[0];
  claySiltSand[layerIndex][decWrap(index)] := items[1];
  claySiltSand[layerIndex][decWrap(decWrap(index))] := items[2];
  end;

{ ----------------------------------------------------------------------------------- updating }
procedure KfSoilTextureTriangleDrawComponent.updateEnabling;
	begin
  inherited updateEnabling;
  if (soil = nil) or (self.readOnly) then
    begin
    self.editEnabled := false;
    self.invalidate;
    end
  else
    self.editEnabled := true;
  end;

procedure KfSoilTextureTriangleDrawComponent.updateModelValues;
  var
    i: integer;
    oldValues: array[0..9] of array[0..2] of single;
    changed: boolean;
  begin
  if soil = nil then exit;
  if self.collapsed then exit;
  for i := 0 to 9 do
    begin
    oldValues[i][0] := claySiltSand[i][0];
    oldValues[i][1] := claySiltSand[i][1];
    oldValues[i][2] := claySiltSand[i][2];
    end;
  self.updateCurrentValue(-1);
  changed := false;
  for i := 0 to 9 do
    begin
    if oldValues[i][0] <> claySiltSand[i][0] then begin changed := true; break; end;
    if oldValues[i][1] <> claySiltSand[i][1] then begin changed := true; break; end;
    if oldValues[i][2] <> claySiltSand[i][2] then begin changed := true; break; end;
    end;
  if changed then
    begin
    updateDisplay;
    invalidate;
    end;
  end;

procedure KfSoilTextureTriangleDrawComponent.updateCurrentValue(aFieldIndex: integer);
  var
    i: integer;
	begin
  if soil <> nil then
    begin
    if aFieldIndex <> -1 then self.getClaySiltSand(aFieldIndex)
    else for i := 0 to 9 do self.getClaySiltSand(i);
    self.makeSurePercentsAddUpTo100;
    end
  else
    self.editEnabled := false;
  end;

procedure KfSoilTextureTriangleDrawComponent.getClaySiltSand(aFieldIndex: integer);
  begin
  soil.transferField(kGetField, claySiltSand[aFieldIndex][0], kSoilPatchLayerClayContent_pct,
      kFieldFloat, aFieldIndex, 0, nil);
  soil.transferField(kGetField, claySiltSand[aFieldIndex][1], kSoilPatchLayerSiltContent_pct,
      kFieldFloat, aFieldIndex, 0, nil);
  soil.transferField(kGetField, claySiltSand[aFieldIndex][2], kSoilPatchLayerSandContent_pct,
      kFieldFloat, aFieldIndex, 0, nil);
  end;

procedure KfSoilTextureTriangleDrawComponent.updateFromUpdateEventList(updateList: TListCollection);
  { this fxn is almost identical to that of the browser component, but it checks against specific fields
    and has no aspect }
  var
    i: longint;
    updateEvent: GsModelUpdateEvent;
    shouldUpdate: boolean;
  begin
  if updateList.count <= 0 then exit;
  if self.model = nil then exit;
  shouldUpdate := false;
  updateEvent := nil;
  for i := 0 to updateList.count - 1 do
    begin
    updateEvent := GsModelUpdateEvent(updateList.items[i]);
    if updateEvent = nil then continue;
    if updateEvent.model = nil then continue;
    if (updateEvent.model = self.soil)
      and ((updateEvent.fieldID = kSoilPatchLayerClayContent_pct)
        or (updateEvent.fieldID = kSoilPatchLayerSiltContent_pct)
        or (updateEvent.fieldID = kSoilPatchLayerSandContent_pct)) then
      begin
      shouldUpdate := true;
      break;
      end;
    end;
  if shouldUpdate and (updateEvent <> nil) then
    begin
    { could put check in descendents to only update if value is different from current }
    self.updateCurrentValue(updateEvent.fieldIndex);
    self.updateDisplay;
    end;
  end;

procedure KfSoilTextureTriangleDrawComponent.updateDisplay;
	begin
  if soil <> nil then
    begin
    self.editEnabled := true;
    self.invalidate;
    end
  else
    self.editEnabled := false;
  end;

procedure KfSoilTextureTriangleDrawComponent.Paint;
var
  i: integer;
  thePoint: TPoint;
  percentStrings: array[0..2] of string;
  theTextureString, layerString: string;
  fullRect, textureTextRect, clay0Rect, clay100Rect, silt0Rect, silt100Rect,
    sand0Rect, sand100Rect: TRect;
begin
  fullRect := getClientRect;
  with canvas do
    begin
    font := self.font;
    if self.textHeight = 0 then self.calculateTextDimensions;
    brush.color := clBtnFace;
    brush.style := bsSolid;
    pen.color := clBtnText;
    pen.style := psSolid;
    rectangle(fullRect.left, fullRect.top, fullRect.right, fullRect.bottom);
    end;
  inflateRect(fullRect, -(fullRect.right - fullRect.left) div 15, -(fullRect.bottom - fullRect.top) div 20);
  percentStrings[0] := 'clay ' + floatToStrF(claySiltSand[layerIndex][0], ffFixed, 7, 0) + '%';
  percentStrings[1] := 'silt ' + floatToStrF(claySiltSand[layerIndex][1], ffFixed, 7, 0) + '%';
  percentStrings[2] := 'sand ' + floatToStrF(claySiltSand[layerIndex][2], ffFixed, 7, 0) + '%';
  theTextureString := self.textureString;
  layerString := 'layer ' + intToStr(layerIndex + 1);
   with layerTextRect do
    begin
    left := (fullRect.right - fullRect.left) div 2
      - (canvas.textWidth(layerString) + canvas.textWidth('Sandy clay loam') + kLeftRightGap) div 2;
    right := left + self.canvas.textWidth(layerString);
    top := fullRect.top + kTopBottomGap;
    bottom := top + self.textHeight;
    end;
  with textureTextRect do
    begin
    left := layerTextRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth(theTextureString);
    top := layerTextRect.top;
    bottom := top + self.textHeight;
    end;
  with triangleRect do
    begin
    left := fullRect.left + kLeftRightGap;
    right := fullRect.right - kLeftRightGap;
    top := layerTextRect.bottom + kLeftRightGap;
    bottom := fullRect.bottom - self.textHeight - kTopBottomGap - kLeftRightGap;
    end;
  self.adjustTriangleSoItIsEquilateral;
  thePoint := self.pointFromClaySand(claySiltSand[layerIndex][0], claySiltSand[layerIndex][2]);
  pointRect := rect(thePoint.x - kSliderDraggerHeight div 2, thePoint.y - kSliderDraggerHeight div 2,
    thePoint.x + kSliderDraggerHeight div 2, thePoint.y + kSliderDraggerHeight div 2);
  with clay0Rect do
    begin
    right := triangleRect.left;
    left := right - self.canvas.textWidth('0 ');
    bottom := triangleRect.bottom;
    top := bottom - self.textHeight;
    end;
  with clay100Rect do
    begin
    right := triangleRect.left + (triangleRect.right - triangleRect.left) div 2 - kLeftRightGap;
    left := right - self.canvas.textWidth('100 ');
    top := triangleRect.top;
    bottom := top + self.textHeight;
    end;
  with silt0Rect do
    begin
    left := triangleRect.left + (triangleRect.right - triangleRect.left) div 2 + kLeftRightGap;
    right := left + self.canvas.textWidth('0 ');
    top := triangleRect.top;
    bottom := top + self.textHeight;
    end;
  with silt100Rect do
    begin
    left := triangleRect.right;
    right := left + self.canvas.textWidth('100 ');
    bottom := triangleRect.bottom;
    top := bottom - self.textHeight;
    end;
  with sand0Rect do
    begin
    right := triangleRect.right;
    left := right - self.canvas.textWidth('0 ');
    top := triangleRect.bottom + kBetweenGap;
    bottom := top + self.textHeight;
    end;
  with sand100Rect do
    begin
    left := triangleRect.left;
    right := left + self.canvas.textWidth('100 ');
    top := triangleRect.bottom + kBetweenGap;
    bottom := top + self.textHeight;
    end;
  with percentTextRects[0] do { clay }
    begin
    left := fullRect.left + kLeftRightGap;
    right := left + self.canvas.textWidth(percentStrings[0]);
    top := triangleRect.top + (triangleRect.bottom - triangleRect.top) div 3;
    bottom := top + self.textHeight;
    end;
  with percentTextRects[1] do { silt }
    begin
    right := fullRect.right - kLeftRightGap;
    left := right - self.canvas.textWidth(percentStrings[1]);
    top := triangleRect.top + (triangleRect.bottom - triangleRect.top) div 3;
    bottom := top + self.textHeight;
    end;
  with percentTextRects[2] do { sand }
    begin
    left := triangleRect.left + (triangleRect.right - triangleRect.left) div 2
      - self.canvas.textWidth(percentStrings[2]) div 2;
    right := left + self.canvas.textWidth(percentStrings[2]);
    top := triangleRect.bottom + kLeftRightGap;
    bottom := top + self.textHeight;
    end;
  with Canvas do
    begin
    self.drawSoilTextureTriangle(false);
    KfDrawButton(self.canvas, pointRect, (self.selectedItemIndex = kItemPoint), self.editEnabled);
    font.style := [fsItalic];
    brush.style := bsClear;
    self.drawTextLabel('0', clay0Rect, false);
    self.drawTextLabel('100', clay100Rect, false);
    self.drawTextLabel('0', silt0Rect, false);
    self.drawTextLabel('100', silt100Rect, false);
    self.drawTextLabel('0', sand0Rect, false);
    self.drawTextLabel('100', sand100Rect, false);
    font.style := [];
    self.drawText(layerString, layerTextRect, self.editEnabled, (self.selectedItemIndex = kItemLayerText));
    self.drawTextLabel(theTextureString, textureTextRect, false);
    for i := 0 to 2 do
      self.drawText(percentStrings[i], percentTextRects[i], self.editEnabled,
        (self.selectedItemIndex = kItemClay + i));
    end;
end;

procedure KfSoilTextureTriangleDrawComponent.drawTextLabel(text: string; drawRect: TRect; drawBox: boolean);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with self.canvas do
    begin
    font := self.font;
    font.color := clBtnText;
    brush.style := bsClear;
    strPCopy(cText, text);
    winprocs.drawText(handle, cText, strLen(cText), drawRect, DT_LEFT);
    end;
  if drawBox then with self.canvas do
    begin
    pen.color := clBtnShadow;
    moveTo(drawRect.right + 2, drawRect.top);
    lineTo(drawRect.left - 2, drawRect.top);
    lineTo(drawRect.left - 2, drawRect.bottom - 2);
    lineTo(drawRect.right + 2, drawRect.bottom - 2);
    lineTo(drawRect.right + 2, drawRect.top);
    end;
  end;

procedure KfSoilTextureTriangleDrawComponent.adjustTriangleSoItIsEquilateral;
  var
    triangleHeight, aspectRatio, newWidth, centerX: single;
  begin
  triangleHeight := round((triangleRect.right - triangleRect.left) * sqrt(3.0) / 2.0);
  if (triangleRect.bottom - triangleRect.top) < triangleHeight then
    begin
    aspectRatio := (triangleRect.right - triangleRect.left) / triangleHeight;
    newWidth := aspectRatio * (triangleRect.bottom - triangleRect.top);
    centerX := (triangleRect.right - triangleRect.left) / 2.0;
    triangleRect.left := round(centerX - newWidth / 2.0);
    triangleRect.right := round(centerX + newWidth / 2.0);
    end;
  end;

procedure KfSoilTextureTriangleDrawComponent.drawSoilTextureTriangle(recalculatePoint: boolean);
  begin
  { draw triangle }
  canvas.polygon([
    point(triangleRect.left, triangleRect.bottom),
    point(triangleRect.right, triangleRect.bottom),
    point(round(triangleRect.left + (triangleRect.right - triangleRect.left) / 2),
      triangleRect.bottom - round((triangleRect.right - triangleRect.left) * sqrt(3.0) / 2.0))]);
  { draw scale lines }
  canvas.pen.color := clBtnShadow;
  { moving up sand axis }
  drawTextureLine(20, 80, 0, 80);
  drawTextureLine(40, 60, 0, 60);
  drawTextureLine(60, 40, 0, 40);
  drawTextureLine(80, 20, 0, 20);
  { moving up clay axis }
  drawTextureLine(20, 0, 20, 80);
  drawTextureLine(40, 0, 40, 60);
  drawTextureLine(60, 0, 60, 40);
  drawTextureLine(80, 0, 80, 20);
  { moving up silt axis }
  drawTextureLine(80, 0, 0, 80);
  drawTextureLine(60, 0, 0, 60);
  drawTextureLine(40, 0, 0, 40);
  drawTextureLine(20, 0, 0, 20);
  { draw texture lines }
  canvas.pen.color := clBtnText;
  drawTextureLine(10, 90, 0, 85);
  drawTextureLine(15, 85, 0, 70);
  drawTextureLine(20, 80, 20, 52);
  drawTextureLine(20, 52, 7, 52);
  drawTextureLine(7, 52, 7, 43);
  drawTextureLine(0, 50, 27, 22);
  drawTextureLine(20, 52, 27, 45);
  drawTextureLine(27, 45, 27, 0);
  drawTextureLine(27, 45, 55, 45);
  drawTextureLine(20, 80, 20, 52);
  drawTextureLine(40, 45, 40, 0);
  drawTextureLine(40, 20, 60, 0);
  drawTextureLine(40, 20, 27, 20);
  drawTextureLine(0, 20, 12, 8);
  drawTextureLine(12, 8, 12, 0);
  end;

procedure KfSoilTextureTriangleDrawComponent.drawTextureLine(clay1, sand1, clay2, sand2: single);
  var point1, point2: TPoint;
  begin
  point1 := pointFromClaySand(clay1, sand1);
  point2 := pointFromClaySand(clay2, sand2);
  canvas.moveTo(point1.x, point1.y);
  canvas.lineTo(point2.x, point2.y);
  end;

function KfSoilTextureTriangleDrawComponent.maxWidth: integer;
  begin
  result := kLeftRightGap * 2 + intMax(self.labelWidth, kTriangleMaxWidth);
  end;

function KfSoilTextureTriangleDrawComponent.minWidth(requestedWidth: integer): integer;
  var minAllowed: integer;
  begin
  minAllowed := kLeftRightGap * 2 + intMax(self.longestLabelWordWidth, kTriangleMinWidth);
  if requestedWidth > minAllowed then
    result := -1
  else
    result := minAllowed;
  end;

function KfSoilTextureTriangleDrawComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + round((kTriangleMinWidth * sqrt(3.0) / 2.0))
    + self.textHeight * 2 + kLeftRightGap * 3;
  end;

procedure KfSoilTextureTriangleDrawComponent.resizeElements;
  begin
  end;

function KfSoilTextureTriangleDrawComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (self.editEnabled) then
    result := kItemSand
  else
    result := kItemLabel;
  end;

{ ----------------------------------------------------------------------------------- conversion }
function KfSoilTextureTriangleDrawComponent.pointFromClaySand(clay, sand: single): TPoint;
  var
    s, c, sp, cp: single;
  begin
  s := triangleRect.right - triangleRect.left;
  c := s * sqrt(3.0) / 2.0;
  sp := sand / 100.0;
  cp := clay / 100.0;
  result.x := round(s - s * sp - 0.5 * s * cp);
  result.y := round(c * cp);
  {adjust for inset of triangle in image and y being flipped on screen}
  result.x := result.x + triangleRect.left;
  result.y := triangleRect.bottom - result.y;
  end;

procedure KfSoilTextureTriangleDrawComponent.claySandFromPoint(point: TPoint; var clay: single; var sand: single);
  var
    s, c: single;
    realPoint: TPoint;
  begin
  {adjust for triangle inset and y reversal}
  realPoint.x := point.x - triangleRect.left;
  realPoint.y := triangleRect.bottom - point.y;
  s := triangleRect.right - triangleRect.left;
  c := s * sqrt(3.0) / 2.0;
  clay := 100.0 * realPoint.y / c;
  sand := 100.0 * (1.0 - realPoint.x / s - 0.5 * realPoint.y / c);
  end;

function KfSoilTextureTriangleDrawComponent.textureString: string;
  var
    clay, sand, silt: single;
  begin
  clay := claySiltSand[layerIndex][0];
  silt := claySiltSand[layerIndex][1];
  sand := claySiltSand[layerIndex][2];
  {clay}
  if (clay >= 40) and (sand <= 45) and (silt <= 40) then result := 'Clay'
  {silty clay}
  else if (clay >= 40) and (silt > 40) then result := 'Silty clay'
  {silty clay loam}
  else if (clay >= 27) and (clay < 40) and (sand <= 20) then result := 'Silty clay loam'
  {clay loam - must be before sandy clay for test to be correct}
  else if (clay >= 27) and (clay < 40) and (sand > 20) and (sand <= 45) then result := 'Clay loam'
  {sandy clay}
  else if (clay >= 35) and (sand > 45) then result := 'Sandy clay'
  {silt - must come before silt loam}
  else if (clay <= 12) and (silt >= 80) then result := 'Silt'
  {silt loam}
  else if (clay <= 27) and (silt >= 50) then result := 'Silt loam'
  {loam}
  else if (clay >= 7) and (clay < 27) and (sand <= 52) and (silt >= 28) and (silt < 50)
    then result := 'Loam'
  {sandy clay loam}
  else if (clay >= 20) and (clay < 35) and (sand > 45) and (silt < 28)
    then result := 'Sandy clay loam'
  {what is left must be sandy loam, loamy sand, or sand}
  {move out from corner, testing against lines}
  {sand}
  else if ((-2.0 / 3.0 * (100.0 - sand) + 10.0 - clay) > 0) then result := 'Sand'
  {loamy sand}
  else if ((-0.5 * (100.0 - sand) + 15.0 - clay) > 0) then result := 'Loamy sand'
  else result := 'Sandy loam';
  end;

end.

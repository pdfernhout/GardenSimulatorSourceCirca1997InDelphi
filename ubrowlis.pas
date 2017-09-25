unit Ubrowlis;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowlis: Browser component for list of enumerated items. Items are displayed as a list
of radio buttons. Strings for items are got from object in superclass, using fillEnumStringList
method of each object. This represents a case where you can't just add an aspect to aspects.tab.
If you add an aspect of this type, you must change the fillEnumStringList for the object to add
this list.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom;

const
  kItemFirstChoice = 1;
  kMaxNumChoices = 20; 

type KfEnumChoiceBrowserComponent = class(KfBrowserComponent)
	public
  hasRadioButtons: boolean;
  currentChoice: integer;
  choiceStrings: TStringList;
  choiceRects: array[0..kMaxNumChoices] of TRect;
  choiceChecks: array[0..kMaxNumChoices] of boolean;
  editEnabled: boolean;
  procedure initialize; override;
  destructor destroy; override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  procedure drawRadioButton(value: boolean; enabled: boolean; rect: TRect);
  procedure drawCheckBox(value: boolean; enabled: boolean; rect: TRect);
  function maxSelectedItemIndex: integer; override;
  procedure clearChecks;
  procedure setNewValue(index: integer);
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, umconsts, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct,
  uesoil, ueplant, uegarden;

procedure KfEnumChoiceBrowserComponent.initialize;
	begin
  currentChoice := 0;
  editEnabled := false;
  choiceStrings := TStringList.create;
  case aspect.objectType of
    kObjectTypeSoil: GsSoilPatch.fillEnumStringList(choiceStrings, aspect.fieldNumber, hasRadioButtons);
    kObjectTypePlant, kObjectTypeDrawingPlant:
      GsPlant.fillEnumStringList(choiceStrings, aspect.fieldNumber, hasRadioButtons);
    kObjectTypeGarden: GsGarden.fillEnumStringList(choiceStrings, aspect.fieldNumber, hasRadioButtons);
    { no instances of these yet. if any added, include above }
   { kObjectTypeWeather: GsWeather.fillEnumStringList(choiceStrings, aspect.fieldNumber);
    kObjectTypeBag: GsBag.fillEnumStringList(choiceStrings, aspect.fieldNumber); }
  else
    raise Exception.create('enumerated list unsupported for object type ' + intToStr(aspect.objectType));
  end;
  end;

destructor KfEnumChoiceBrowserComponent.destroy;
  begin
  choiceStrings.free;
  choiceStrings := nil;
  inherited destroy;
  end;

procedure KfEnumChoiceBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  editEnabled := not ((model = nil) or (self.readOnly));
  end;

procedure KfEnumChoiceBrowserComponent.updateModelValues;
  var oldChoice: integer;
  begin
  if model = nil then exit;
  if self.collapsed then exit;
  oldChoice := currentChoice;
  self.updateCurrentValue(-1);
  if not hasRadioButtons or (oldChoice <> currentChoice) then
    begin
    updateDisplay;
    invalidate;
    end;
  end;

procedure KfEnumChoiceBrowserComponent.updateCurrentValue(aFieldIndex: integer);
  var
    i: smallint;
	begin
  if (model = nil) or (aspect.fieldType <> kFieldEnumeratedList) then
    begin
    currentChoice := 0;
    self.clearChecks;
    editEnabled := false;
    end
  else
    begin
    if hasRadioButtons then
      model.transferField(kGetField, currentChoice, aspect.fieldNumber, kFieldInt, kNotArray,
        aspect.deriveMethod(groupItem.derivedIndex), nil)
    else
      if choiceStrings.count > 0 then
        for i := 0 to choiceStrings.count - 1 do
          model.transferField(kGetField, choiceChecks[i], aspect.fieldNumber, kFieldBoolean, i,
            aspect.deriveMethod(groupItem.derivedIndex), nil);
    end;
  end;

procedure KfEnumChoiceBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldEnumeratedList) then
    self.invalidate
  else
    editEnabled := false;
  end;

procedure KfEnumChoiceBrowserComponent.clearChecks;
  var i: smallint;
  begin
  for i := 0 to kMaxNumChoices do choiceChecks[i] := false;
  end;

procedure KfEnumChoiceBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    i: integer;
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  if (editEnabled) then
    begin
    thePoint := Point(x, y);
    if choiceStrings.count > 0 then for i := 0 to choiceStrings.count - 1 do
      if ptInRect(choiceRects[i], thePoint) then
        begin
        self.setNewValue(i);
        self.selectedItemIndex := kItemFirstChoice + currentChoice;
        break;
        end;
    self.invalidate;
    end;
  end;

procedure KfEnumChoiceBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  inherited doKeyDown(sender, key, shift);
  if (editEnabled) and (key = VK_RETURN) and (self.selectedItemIndex >= kItemFirstChoice) then
    begin
    self.setNewValue(self.selectedItemIndex - kItemFirstChoice);
    self.invalidate;
    end;
  end;

procedure KfEnumChoiceBrowserComponent.setNewValue(index: integer);
  begin
  if hasRadioButtons then
    begin
    currentChoice := index;
    GardenForm.doCommand(GsChangeDomainIntegerCommand.createCommand(model, currentChoice,
        aspect.fieldNumber, kNotArray));
    end
  else
    begin
    choiceChecks[index] := not choiceChecks[index];
    GardenForm.doCommand(GsChangeDomainBooleanCommand.createCommand(model, choiceChecks[index],
        aspect.fieldNumber, index));
    end;
  end;

function KfEnumChoiceBrowserComponent.maxWidth: integer;
  var
    i, maxWidth: integer;
  begin
  maxWidth := 0;
  with TBrowserForm(owner).canvas do for i := 0 to choiceStrings.count - 1 do
    maxWidth := intMax(maxWidth, textWidth(choiceStrings.strings[i]));
  result := intMax(kLeftRightGap * 2 + self.labelWidth,
    + kRadioButtonLeftGap + kRadioButtonWidth + kRadioButtonBeforeTextGap + maxWidth + kLeftRightGap);
  end;

function KfEnumChoiceBrowserComponent.minWidth(requestedWidth: integer): integer;
  var
    i, longestWidth, minAllowed: integer;
  begin
  longestWidth := 0;
  with TBrowserForm(owner).canvas do for i := 0 to choiceStrings.count - 1 do
    longestWidth := intMax(longestWidth, textWidth(choiceStrings.strings[i]));
  minAllowed := intMax(kLeftRightGap * 2 + self.longestLabelWordWidth,
    + kRadioButtonLeftGap + kRadioButtonWidth + kRadioButtonBeforeTextGap + longestWidth + kLeftRightGap);
  if requestedWidth > minAllowed then
    result := -1
  else
    result := minAllowed;
  end;

function KfEnumChoiceBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + self.textHeight * choiceStrings.count + kTopBottomGap;
  end;

function KfEnumChoiceBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (self.editEnabled) then
    result := kItemFirstChoice + choiceStrings.count - 1
  else
    result := kItemLabel;
  end;

procedure KfEnumChoiceBrowserComponent.resizeElements;
  begin
  { do nothing }
  end;

procedure KfEnumChoiceBrowserComponent.paint;
  var
    i: integer;
    rect: TRect;
    circleRects: array[0..kMaxNumChoices] of TRect;
    textRects: array[0..kMaxNumChoices] of TRect;
  begin
  inherited paint;
  if self.collapsed then exit;
  rect := GetClientRect;
  if choiceStrings.count > 0 then for i := 0 to choiceStrings.count - 1 do
    begin
    with circleRects[i] do
      begin
      left := rect.left + kRadioButtonLeftGap;
      right := left + kRadioButtonWidth;
      top := rect.top + self.collapsedHeight + self.textHeight * i
        + (self.textHeight div 2 - kRadioButtonWidth div 2);
      bottom := top + kRadioButtonWidth;
      end;
    with textRects[i] do
      begin
      left := rect.left + kRadioButtonLeftGap + kRadioButtonWidth + kRadioButtonBeforeTextGap;
      right := left + self.canvas.textWidth(choiceStrings.strings[i]);
      top := rect.top + self.collapsedHeight + self.textHeight * i;
      bottom := top + self.textHeight;
      end;
    if hasRadioButtons then
      self.drawRadioButton((currentChoice = i), editEnabled, circleRects[i])
    else
      self.drawCheckBox(choiceChecks[i], editEnabled, circleRects[i]);
    self.drawText(choiceStrings.strings[i], textRects[i], true,
      (self.selectedItemIndex = kItemFirstChoice + i) and (self.editEnabled));
    unionRect(choiceRects[i], circleRects[i], textRects[i]);
    end;
  end;

procedure KfEnumChoiceBrowserComponent.drawRadioButton(value: boolean; enabled: boolean; rect: TRect);
  var
    brushColor: TColor;
  begin
  with self.canvas do
    begin
    brushColor := brush.color;
    if value then
      if enabled then
        brush.color := clBtnText
      else
        brush.color := clBtnShadow
    else
      brush.color := clBtnFace;
    if enabled then
      pen.color := clBtnText
    else
      pen.color := clBtnShadow;
    with rect do ellipse(left, top, right, bottom);
    brush.color := brushColor;
    brush.style := bsClear;
    end;
  end;

procedure KfEnumChoiceBrowserComponent.drawCheckBox(value: boolean; enabled: boolean; rect: TRect);
  begin
  with self.canvas do
    begin
    brush.style := bsClear;
    if enabled then
      pen.color := clBtnText
    else
      pen.color := clBtnShadow;
    with rect do rectangle(left, top, right, bottom);
    if value then with rect do
      begin
      moveTo(left, top);
      lineTo(right, bottom);
      moveTo(left, bottom);
      lineTo(right, top);
      end;
    end;
  end;

end.

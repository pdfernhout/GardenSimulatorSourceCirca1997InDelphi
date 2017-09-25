unit Ubrowcom;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowcom: Base class for browser components. Has pointers to aspect (info on representation
of variable data), object (plant, soil patch, etc), and groupItem (info on bounds, unit, etc).
Sets defaults for displaying. Methods for drawing and accessing data are used by subclasses.
In all browser components, the user can use the tab key to move between interface
items on the component. This is important because to save resources we replaced
normal window-handle sliders and other components with no-resource sliders that have
no handles and therefore can have no focus. So there is an internal "unofficial" focus
system used to know how to interpret keystrokes based on the index number of the
item selected. The browser components are therefore (sort of) able to handle keyboard-only
input. A lot of the other code in this file has to do with resizing and determining the
smallest/largest size the browser component can take. Also notice that the invalidation
system for ALL the browser components is very bad; they just invalidate their whole selves
when that is often not necessary. But on a fast enough computer you can't tell anyway.
The internal tab system must include
  a. overriding the maxSelectedItemIndex function if there is more than one tab stop
  b. handling the enter keypress (override the doKeyPress method) at each tab stop
     (make it a tab stop only if something happens when you click on it) }

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ugroups, ucollect;


const
  kTopBottomGap = 4;
  kLeftRightGap = 8;
  kRadioButtonLeftGap = 30;
  kRadioButtonWidth = 10;
  kRadioButtonBetweenGap = 10;
  kRadioButtonBeforeTextGap = 5;
  kBetweenGap = 4;
  kEditMaxWidth = 200;
  kEditMinWidth = 150;
  kNotSelectedString = '(not selected)';
  kNotArray = -1;
  kMaxArraySize = 12;
  kGraphHeight = 150;
  kDefaultHardMin = 0.0;
  kDefaultHardMax = 1000.0;
  kMaxDisplayDigitsToLeftOfDecimal = 7;
  kMaxDisplayDigitsToRightOfDecimal = 6;
  kMaxDisplayDigits = 9;
  kSoft = false;
  kHard = true;

  kItemNone = -1;
  kItemLabel = 0;

type KfBrowserComponent = class(TPanel)
	public
  model: GsModel;
  aspect: GsAspect;
  groupItem: GsGroupItem;
  collapsed: boolean;
  readOnly: boolean;
  labelWidth: integer;
  textHeight: integer;
  labelRect: TRect;
  wrappedLabelHeight: integer;
  longestLabelWordWidth: integer;
  selectedItemIndex: integer;
  draggable: boolean;
  saveButtonDuringDrag: TMouseButton;
  saveShiftDuringDrag: TShiftState;
  constructor createField(AOwner: TComponent; anAspect: GsAspect; aGroupItem: GsGroupItem);
  procedure initialize; virtual;
  procedure updateModel(newModel: GsModel); virtual;
  procedure updateModelValues; virtual;
	procedure updateEnabling; virtual;
  procedure updateFromUpdateEventList(updateList: TListCollection); virtual;
  procedure updateCurrentValue(aFieldIndex: integer); virtual;
  procedure updateDisplay; virtual;
  procedure collapseOrExpand(y: integer); virtual;
  procedure collapse;
  procedure expand;
  procedure paint; override;
  procedure fillWithBevels;
  function collapsedHeight: integer; virtual;
  function minWidth(requestedWidth: integer): integer; virtual;
  function maxWidth: integer; virtual;
  function uncollapsedHeight: integer; virtual;
  procedure resizeElements; virtual;
  function editHeight: integer;
  procedure calculateTextDimensions; virtual;
  procedure calculateHeight;
  procedure doOnEnter(sender: TObject);
  procedure doOnExit(sender: TObject);
  procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  procedure doDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  procedure doDragDrop(Sender, Source: TObject; X, Y: Integer);
  procedure doMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); virtual;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  procedure forceMouseUpAfterDrag(x, y: integer);
  procedure tabThroughItems(goForward: boolean);
  function nextSelectedItemIndex(goForward: boolean): integer; virtual;
  function maxSelectedItemIndex: integer; virtual;
	function measureText(text: string; drawRect: TRect): integer;
  function drawText(text: string; drawRect: TRect; isSelectable: boolean; isSelected: boolean): integer;
  procedure drawTextSelectedBox(rect: TRect);
  procedure drawTextLabel(text: string; drawRect: TRect; drawBox: boolean);
  procedure updateAspectInfo; virtual;
  function minScaleWidthWithBounds(unitSet, unitModel: integer; softMin, softMax: single): integer;
  function minScaleWidthWithoutBounds(unitSet, unitModel: integer): integer;
  function calculateLongestLabelWordWidth: integer;
  function formTextHeight: longint;
  function formTextWidth(aString: string): longint;
  function canBeGraphed: boolean;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, umconsts, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct;

constructor KfBrowserComponent.createField(AOwner: TComponent; anAspect: GsAspect; aGroupItem: GsGroupItem);
  var objectText: string[10];
	begin
  self.create(AOwner);
	aspect := anAspect;
  groupItem := aGroupItem;
  self.readOnly := anAspect.readOnly;
  self.model := nil;
  self.parentFont := true;
  self.collapsed := true; { start collapsed }
  self.caption := removeUnitSuffix(groupItem.displayName);
  objectText := firstLetterForObjectTypeIndex(aspect.objectType) + ': ';
  self.caption := objectText + self.caption;
  self.onDragOver := self.doDragOver;
  self.onDragDrop := self.doDragDrop;
  self.onMouseDown := self.doMouseDown;
  self.onMouseUp := self.doMouseUp;
  self.onKeyDown := self.doKeyDown;
  self.onEnter := self.doOnEnter;
  self.onExit := self.doOnExit;
  self.tabStop := true;
  self.enabled := true;
  self.initialize;
  self.updateDisplay;
  self.updateEnabling;
  self.selectedItemIndex := -1;
  self.bevelInner := bvRaised;
  self.bevelOuter := bvNone;
  self.draggable := true;
  end;

function KfBrowserComponent.formTextHeight: longint;
  var theForm: TForm;
  begin
  theForm := getParentForm(self);
  if theForm = nil then
    result := 16 {default}
  else if theForm.canvas = nil then
    result := 16 {default}
  else
    begin
    result := 16;
    theForm.canvas.font := theForm.font;
    result := theForm.canvas.textHeight('W');
    end;
  end;

function KfBrowserComponent.formTextWidth(aString: string): longint;
  var theForm: TForm;
  begin
  theForm := getParentForm(self);
  if theForm = nil then
    result := 50 {default}
  else if theForm.canvas = nil then
    result := 50 {default}
  else
    begin
    result := length(aString) * 8;
    theForm.canvas.font := theForm.font;
    result := theForm.canvas.textWidth(aString);
    end;
  end;

procedure KfBrowserComponent.calculateTextDimensions;
  begin
  { use form canvas for this because when component is first created, doesn't know what font it has }
  self.textHeight := self.formTextHeight;
  self.labelWidth := self.formTextWidth(trimLeftAndRight(self.caption));
  self.longestLabelWordWidth := self.calculateLongestLabelWordWidth;
  end;

procedure KfBrowserComponent.doMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.saveButtonDuringDrag := button;
  self.saveShiftDuringDrag := shift;
  if (button = mbRight) or ((button = mbLeft) and (ssShift in shift)) then
    begin
    if not self.focused then self.setFocus;
    self.collapseOrExpand(y);
    end
  else if button = mbLeft then
    begin
    { tell control panel about mouse down for dragging components }
    if draggable then
      if y < self.collapsedHeight then
        if parent <> nil then (parent as TPanel).onMouseDown(sender, button, shift, x, self.top + y);
    if not self.focused then self.setFocus;
    end;
  if self.collapsed then self.selectedItemIndex := kItemLabel;
  end;

procedure KfBrowserComponent.doDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
  begin
  { tell control panel about drag over for dragging components }
  accept := draggable and (source = parent);
  end;

procedure KfBrowserComponent.doDragDrop(Sender, Source: TObject; X, Y: Integer);
  begin
  { tell control panel about drag drop for dragging components }
  if draggable then
    if parent <> nil then (parent as TPanel).onDragDrop(sender, source, x, self.top + y);
  end;

procedure KfBrowserComponent.forceMouseUpAfterDrag(x, y: integer);
  begin
  self.onMouseUp(self, saveButtonDuringDrag, saveShiftDuringDrag, x, y);
  end;

procedure KfBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
 { subclasses should override }
  end;

procedure KfBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
{ subclasses should override and respond to enter events when different items are selected }
{ subclasses should call inherited }
  var
    Form: TForm;
  begin
  if (key = VK_TAB) then
    begin
    if shift = [ssShift] then
      self.tabThroughItems(false)
    else
      self.tabThroughItems(true);
    if self.selectedItemIndex = kItemNone then
      begin
      form := getParentForm(self);
      if form <> nil then
        if shift = [ssShift] then
          begin
          TBrowserForm(form).backTabbing := true;
          form.perform(wm_NextDlgCtl, 1, 0); {previous}
          end
        else
          form.perform(wm_NextDlgCtl, 0, 0);
      end;
    end
  else
    if (key = VK_RETURN) and (self.selectedItemIndex = kItemLabel) then
      self.collapseOrExpand(0); {give it a y as if the label was clicked on}
  end;

function KfBrowserComponent.maxSelectedItemIndex: integer;
  begin
  { subclasses should override }
  result := 0;
  end;

procedure KfBrowserComponent.tabThroughItems(goForward: boolean);
  begin
  { subclasses need not override this if they override maxSelectedItemIndex }
  self.selectedItemIndex := self.nextSelectedItemIndex(goForward);
  if self.selectedItemIndex > self.maxSelectedItemIndex then
    self.selectedItemIndex := kItemNone;
  self.invalidate;
  end;

function KfBrowserComponent.nextSelectedItemIndex(goForward: boolean): integer;
  begin
  if goForward then
    result := self.selectedItemIndex + 1
  else
    result := self.selectedItemIndex - 1;
  end;

procedure KfBrowserComponent.WMGetDlgCode(var Message: TWMGetDlgCode);
  begin
  inherited;
  { want arrows allows us to pass arrow keys on to slider or combo box }
  Message.Result := Message.Result or DLGC_WANTTAB or DLGC_WANTARROWS;
  end;

procedure KfBrowserComponent.initialize;
	begin
  { sublasses chould override }
  end;

procedure KfBrowserComponent.doOnEnter(sender: TObject);
  begin
  TBrowserForm(owner).enteringComponent(self);
  if TBrowserForm(owner).backTabbing then
    begin
    self.selectedItemIndex := self.maxSelectedItemIndex;
    TBrowserForm(owner).backTabbing := false;
    end;
  if self.selectedItemIndex = kItemNone then self.selectedItemIndex := kItemLabel;
  self.invalidate;
  end;

procedure KfBrowserComponent.doOnExit(sender: TObject);
  begin
  TBrowserForm(owner).leavingComponent(self);
  self.selectedItemIndex := kItemNone;
  self.invalidate;
  end;

function KfBrowserComponent.canBeGraphed: boolean;
  begin
  result := false;
  if not self.enabled then exit;
  if aspect = nil then exit;
  if model = nil then exit;
  if (aspect.fieldType <> kFieldFloat)
    and (aspect.fieldType <> kFieldInt) then
    exit;
  if (aspect.indexType <> kIndexTypeUndefined)
    and (aspect.indexType <> kIndexTypeNone)
    and (aspect.indexType <> kIndexTypeLayer) then
    exit;
  result := true;
  end;

{model has changed}
procedure KfBrowserComponent.updateModel(newModel: GsModel);
	begin
  if model <> newModel then
  	begin
    model := newModel;
    self.updateEnabling;
    self.updateCurrentValue(-1);  { -1 means update all }
    self.updateDisplay;
    self.invalidate;
    end
  end;

procedure KfBrowserComponent.updateModelValues;
  begin
  { subclasses should override }
  end;

{ aspect has changed }
procedure KfBrowserComponent.updateAspectInfo;
  begin
  { sublasses may override to respond to particular changes such as bounds}
  end;

procedure KfBrowserComponent.updateEnabling;
	begin
  { subclasses should call this when they do their enabling }
  self.enabled := (self.model <> nil);
  end;

procedure KfBrowserComponent.updateCurrentValue(aFieldIndex: integer);
	begin
  { subclasses may override }
  end;

procedure KfBrowserComponent.updateFromUpdateEventList(updateList: TListCollection);
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
    if (updateEvent.model = self.model) and (updateEvent.fieldID = aspect.fieldNumber) then
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
    self.invalidate;
    end;
  end;

procedure KfBrowserComponent.updateDisplay;
	begin
  end;

function KfBrowserComponent.minWidth(requestedWidth: integer): integer;
  begin
  { subclasses must override }
  result := 0;
  end;

function KfBrowserComponent.maxWidth: integer;
  begin
  { subclasses must override }
  result := 0;
  end;

function KfBrowserComponent.uncollapsedHeight: integer;
  begin
  { subclasses must override }
  result := 0;
  end;

procedure KfBrowserComponent.resizeElements;
  begin
  { subclasses must override }
  end;

function KfBrowserComponent.editHeight: integer;
  begin
  result := self.textHeight + 8;
  end;

function KFBrowserComponent.calculateLongestLabelWordWidth: integer;
  var
    i, lastSpace, maxLength: integer;
  begin
  { o: xxxxx xxxxxxxxx xx }
  lastSpace := 3;
  maxLength := 0;
  for i := 4 to length(self.caption) do
    begin
    if (caption[i] = ' ') or (i = length(self.caption)) then
      begin
      if i - lastSpace > maxLength then maxLength := i - lastSpace;
      lastSpace := i;
      end;
    end;
  result := self.formTextWidth('w') * maxLength;
  end;

procedure KfBrowserComponent.calculateHeight;
  var
    fullRect: TRect;
  begin
  fullRect := getClientRect;
  with labelRect do
    begin
    left := fullRect.left + kLeftRightGap;
    right := fullRect.right - kLeftRightGap;
    top := fullRect.top + 2;
    bottom := top + self.textHeight;
    end;
  self.wrappedLabelHeight := self.measureText(self.caption, labelRect);
  if self.collapsed then
    begin
    self.height := self.collapsedHeight;
    end
  else
    self.height := self.uncollapsedHeight;
  end;

procedure KfBrowserComponent.collapseOrExpand(y: integer);
  var
    wasFocused: boolean;
  begin
  if y > self.collapsedHeight then exit;
  if not self.collapsed then
    self.collapse
  else
    self.expand;
  if owner <> nil then
    TBrowserForm(owner).repositionComponents;
  end;

procedure KfBrowserComponent.collapse;
  begin
  self.height := self.collapsedHeight;
  self.collapsed := true;
  end;

procedure KfBrowserComponent.expand;
  begin
  self.height := self.uncollapsedHeight;
  self.collapsed := false;
  updateCurrentValue(-1);
  end;

procedure KfBrowserComponent.Paint;
  var
    fullRect: TRect;
  begin
  { copied from TCustomPanel.paint and location of caption text changed }
  { can't call inherited paint because it wants to put the caption in the middle }
  self.fillWithBevels;
  fullRect := getClientRect;
  with canvas do
    begin
    font := self.font;
    brush.style := bsClear;
    with labelRect do
      begin
      left := fullRect.left + kLeftRightGap;
      right := left + self.width - kLeftRightGap * 2;
      top := fullRect.top + 2;
      bottom := top + self.textHeight;
      end;
    self.wrappedLabelHeight := self.drawText(self.caption, labelRect,
      (not self.collapsed), (self.selectedItemIndex = kItemLabel));
    end;
  end;

procedure KfBrowserComponent.fillWithBevels;
  var
    fullRect: TRect;
    TopColor, BottomColor: TColor;
    theForm: TForm;

  procedure AdjustColors(Bevel: TPanelBevel);
    begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
    end;

  begin
  { can't call inherited paint because it wants to put the caption in the middle }
  fullRect := getClientRect;
  with canvas do
    begin
    { if component is on left side of browser (and is therefore draggable), show that it is selected }
    brush.color := clBtnFace;
    if (self.draggable) then
      begin
      theForm := getParentForm(self);
      if (theForm <> nil) and (theForm is TBrowserForm) and ((theForm as TBrowserForm).selectedComponent = self) then
        brush.color := clWindow;
      end;
    fillRect(fullRect);
    brush.style := bsClear;
    end;
  if BevelOuter <> bvNone then
    begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, fullRect, TopColor, BottomColor, BevelWidth);
    end;
  Frame3D(Canvas, fullRect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
    begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, fullRect, TopColor, BottomColor, BevelWidth);
    end;   
  end;

function KfBrowserComponent.measureText(text: string; drawRect: TRect): integer;
  var
    cText: array[0..255] of Char;
    theForm: TForm;
    newHeight: longint;
    useRect: TRect;
  begin
  theForm := getParentForm(self);
  if theForm = nil then
    result := 16 {default}
  else
    begin
    if theForm.canvas = nil then
      result := 16 {default}
    else
      with theForm.canvas do
        begin
        font := theForm.font;
        useRect := drawRect;
        strPCopy(cText, text);
        { returns height of rectangle }
        newHeight := winprocs.drawText(handle, cText, strLen(cText), useRect,
          DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
        if newHeight > self.textHeight then
          result := newHeight;
        result := newHeight;
        end;
    end;
  end;

function KfBrowserComponent.drawText(text: string; drawRect: TRect;
  isSelectable: boolean; isSelected: boolean): integer;
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with self.canvas do
    begin
    font := self.font;
    if self.model <> nil then
      font.color := clBtnText
    else
      font.color := clBtnShadow;
    strPCopy(cText, text);
    result := winprocs.drawText(handle, cText, strLen(cText), drawRect,
      DT_LEFT or DT_WORDBREAK or DT_NOCLIP);
    drawRect.bottom := drawRect.top + result;
    if (isSelected) then
      self.drawTextSelectedBox(drawRect)
    else if (isSelectable) then
      begin
      pen.color := clBtnShadow;
      moveTo(drawRect.left, drawRect.bottom - 3);
      lineTo(drawRect.right, drawRect.bottom - 3);
      end;
    end;
  end;

procedure KfBrowserComponent.drawTextSelectedBox(rect: TRect);
  begin
  with canvas do
    begin
    pen.color := clBtnShadow;
    moveTo(rect.right + 2, rect.top);
    lineTo(rect.left - 2, rect.top);
    lineTo(rect.left - 2, rect.bottom - 2);
    lineTo(rect.right + 2, rect.bottom - 2);
    lineTo(rect.right + 2, rect.top);
    end;
  end;

procedure KfBrowserComponent.drawTextLabel(text: string; drawRect: TRect; drawBox: boolean);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with self.canvas do
    begin
    font := self.font;
    font.color := clBtnText;
    strPCopy(cText, text);
    winprocs.drawText(handle, cText, strLen(cText), drawRect, DT_LEFT or DT_WORDBREAK);
    end;
  if drawBox then self.drawTextSelectedBox(drawRect);
  end;

function KfBrowserComponent.collapsedHeight: integer;
  begin
  result := self.wrappedLabelHeight + kTopBottomGap;
  end;

function KfBrowserComponent.minScaleWidthWithBounds(unitSet, unitModel: integer; softMin, softMax: single): integer;
  var
    i, numeralChars, letterChars, maxLengthSoftMin, maxLengthSoftMax: integer;
    minValue, maxValue: single;
  begin
  { value number of chars (always use max) }
  numeralChars := kMaxDisplayDigitsToLeftOfDecimal {+ kMaxDisplayDigitsToRightOfDecimal + 1};
  { unit string number of chars (get max for set) }
  letterChars := maxUnitStringLengthForSet(unitSet);
  { bounds number of chars (use actual bounds, but calculate max for set) }
  maxLengthSoftMin := 0;
  maxLengthSoftMax := 0;
  for i := 1 to GetLastUnitEnumInUnitSet(unitSet) - 1 do
    begin
    minValue := Convert(unitSet, unitModel, i, softMin);
    maxValue := Convert(unitSet, unitModel, i, softMax);
    maxLengthSoftMin := intMax(maxLengthSoftMin, length(digitValueString(minValue)));
    maxLengthSoftMax := intMax(maxLengthSoftMax, length(digitValueString(maxValue)));
    end;
  numeralChars := numeralChars + maxLengthSoftMin + maxLengthSoftMax;
  { calc min scale width from number of chars }
  result := self.formTextWidth('i') * letterChars
    + self.formTextWidth('1') * numeralChars
    + kBetweenGap * 3 + kLeftRightGap * 2;
  end;

function KfBrowserComponent.minScaleWidthWithoutBounds(unitSet, unitModel: integer): integer;
  var
    numeralChars, letterChars: integer;
  begin
  { value number of chars (always use max) }
  numeralChars := kMaxDisplayDigitsToLeftOfDecimal {+ kMaxDisplayDigitsToRightOfDecimal + 1};
  { unit string number of chars (get max for set) }
  letterChars := maxUnitStringLengthForSet(unitSet);
  { calc min scale width from number of chars }
  result := self.formTextWidth('i') * letterChars
    + self.formTextWidth('1') * numeralChars
    + kBetweenGap + kLeftRightGap * 2;
  end;

end.
(* not using
  procedure drawTextMouseDownBox(textRect: TRect);
  procedure drawTextSelectableBox(textRect: TRect);
  procedure drawButtonHighlightAndShadow(rect: TRect);

procedure KfBrowserComponent.drawTextSelectableBox(textRect: TRect);
  var
    rect: TRect;
    penStyle: TPenStyle;
  begin
  rect := textRect;
  with canvas do
    begin
    brush.style := bsClear;
    penStyle := pen.style;
    pen.style := psSolid;
    inflateRect(rect, 2, -1);
    {self.drawButtonHighlightAndShadow(rect); }
    moveto(rect.left, rect.bottom);
    lineTo(rect.right, rect.bottom);
    pen.style := penStyle;
    end;
  end;

procedure KfBrowserComponent.drawTextMouseDownBox(textRect: TRect);
  var
    rect: TRect;
    penStyle: TPenStyle;
  begin
  rect := textRect;
  with canvas do
    begin
    brush.style := bsClear;
    penStyle := pen.style;
    pen.style := psSolid;
    pen.color := clBtnShadow;
    inflateRect(rect, 2, -1);
    rectangle(rect.left, rect.top, rect.right, rect.bottom);
    pen.style := penStyle;
    end;
  end;

procedure KfBrowserComponent.drawButtonHighlightAndShadow(rect: TRect);
  begin
  with canvas do
    begin
    pen.color := clBtnHighlight;
    moveTo(rect.right, rect.top);
    lineTo(rect.left, rect.top);
    lineTo(rect.left, rect.bottom);
    pen.color := clBtnShadow;
    lineTo(rect.right, rect.bottom);
    lineTo(rect.right, rect.top);
    end;
  end;
*)



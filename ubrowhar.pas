unit Ubrowhar;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowhar: Browser component for harvest item template (uharvprt). Displays name of icon and picture of icon.
Clicking on name or icon brings up harvest item editor (uharved) to change harvest
item template. Harvest item templates are objects that hold information (nutritional and otherwise)
about things that can be harvested. NOTE that this is a little confusing: in the code the
objects that contain information about what can be harvested are called "harvest item TEMPLATES"
and the objects that contain information about particular fruits, leaves, etc are called "harvest items".
BUT in the help system we just call the harvest item templates (HITs) "harvest items" because
the whole term is too difficult to grasp. In the help system we just call the fruits, leaves,
etc "harvested items". It seemed like a good idea at the time.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, Graphics, WinProcs, Messages,
  umodel, uaspects, ubrowcom, uicon, uharvprt;

const
  kItemFileName = 1;
  kItemIcon = 2;
  kIconSize = 32;

type KfHarvestItemTemplateBrowserComponent = class(KfBrowserComponent)
	public
  harvestItemTemplate: GsHarvestItemTemplate;
  fileNameRect: TRect;
  iconRect: TRect;
  editEnabled: boolean;
  procedure initialize; override;
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
  function maxSelectedItemIndex: integer; override;
  procedure chooseNewOrEdit;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, umconsts, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct, uharved;

procedure KfHarvestItemTemplateBrowserComponent.initialize;
	begin
  editEnabled := false;
  harvestItemTemplate := nil;
  end;

procedure KfHarvestItemTemplateBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  if (model = nil) or (self.readOnly) then
    editEnabled := false
  else
    editEnabled := true;
  end;

procedure KfHarvestItemTemplateBrowserComponent.updateModelValues;
  begin
  self.updateCurrentValue(-1);
  end;

procedure KfHarvestItemTemplateBrowserComponent.updateCurrentValue(aFieldIndex: integer);
	begin
  if (model <> nil) and (aspect.fieldType = kFieldHarvestItemTemplate) then
     begin
     model.transferField(kGetField, harvestItemTemplate, aspect.fieldNumber, kFieldHarvestItemTemplate,
       kNotArray, aspect.deriveMethod(groupItem.derivedIndex), nil);
     self.updateDisplay;
     end
  else
    editEnabled := false;
  end;

procedure KfHarvestItemTemplateBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldHarvestItemTemplate) then
    self.invalidate
  else
    editEnabled := false;
  end;

procedure KfHarvestItemTemplateBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  if (editEnabled) then
    begin
    thePoint := Point(x, y);
    if ptInRect(fileNameRect, thePoint) then
      begin
      self.selectedItemIndex := kItemFileName;
      self.chooseNewOrEdit;
      end
    else if ptInRect(iconRect, thePoint) then
      begin
      self.selectedItemIndex := kItemIcon;
      self.chooseNewOrEdit;
      end;
    self.invalidate;
    end;
  end;

procedure KfHarvestItemTemplateBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) and (editEnabled) then
    begin
    case self.selectedItemIndex of
      kItemFileName: self.chooseNewOrEdit;
      kItemIcon: self.chooseNewOrEdit;
      end;
    end;
  { undo command is specifically to handle if they DELETE the harvest item template }
  if (key = VK_DELETE) and (editEnabled) then
    if (self.selectedItemIndex = kItemFileName) or (self.selectedItemIndex = kItemIcon) then
      begin
      harvestItemTemplate := nil;
      GardenForm.doCommand(
        GsChangeDomainHarvestItemTemplateCommand.createCommand(model, harvestItemTemplate, aspect.fieldNumber));
      self.updateDisplay;
      self.invalidate;
      end;
  end;

procedure KfHarvestItemTemplateBrowserComponent.chooseNewOrEdit;
  var
    harvestItemTemplateEditorForm: THarvestItemTemplateEditorForm;
  begin
  harvestItemTemplateEditorForm := THarvestItemTemplateEditorForm.create(self);
  if harvestItemTemplateEditorForm = nil then
    raise Exception.create('Could not create harvest item template editor');
  try
    harvestItemTemplateEditorForm.setSelectedHarvestItemTemplate(self.harvestItemTemplate);
    if harvestItemTemplateEditorForm.showModal = mrOK then
      begin
      { when user clicks OK on form, it copies correct pointer to harvestItemTemplate }
      { we just deal with the change by making an undo command }
      GardenForm.doCommand(
        GsChangeDomainHarvestItemTemplateCommand.createCommand(model, harvestItemTemplate,
          aspect.fieldNumber));
      self.invalidate;
      end;
  finally
    harvestItemTemplateEditorForm.free;
    harvestItemTemplateEditorForm := nil;
  end;
  end;

function KfHarvestItemTemplateBrowserComponent.maxWidth: integer;
  begin
  result := intMax(kLeftRightGap * 2 + self.labelWidth,
    kLeftRightGap + self.formTextWidth('WWWWWWWW.WWW') + kLeftRightGap + kIconSize
      + kLeftRightGap);
  end;

function KfHarvestItemTemplateBrowserComponent.minWidth(requestedWidth: integer): integer;
  var minAllowed: integer;
  begin
  minAllowed := intMax(kLeftRightGap * 2 + self.longestLabelWordWidth,
    kLeftRightGap + self.formTextWidth('WWWWWWWW.WWW') + kLeftRightGap + kIconSize
      + kLeftRightGap);
  if requestedWidth > minAllowed then
    result := -1
  else
    result := minAllowed;
  end;

function KfHarvestItemTemplateBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + kIconSize + kTopBottomGap * 2;
  end;

function KfHarvestItemTemplateBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (self.editEnabled) then
    result := kItemIcon
  else
    result := kItemLabel;
  end;

procedure KfHarvestItemTemplateBrowserComponent.resizeElements;
  begin
  { do nothing }
  end;

procedure KfHarvestItemTemplateBrowserComponent.paint;
  var
    rect: TRect;
    showText: string;
  begin
  inherited paint;
  if self.collapsed then exit;
  rect := GetClientRect;
  if (harvestItemTemplate <> nil) and (length(harvestItemTemplate.name) > 0) then
    showText := harvestItemTemplate.name
  else
    showText := '(none selected)';
  with fileNameRect do
    begin
    left := rect.left + kLeftRightGap;
    right := left + self.canvas.textWidth(showText);
    top := self.collapsedHeight + kTopBottomGap;
    bottom := top + self.textHeight;
    end;
  with iconRect do
    begin
    right := rect.right - kLeftRightGap;
    left := right - kIconSize;
    top := fileNameRect.top;
    bottom := top + kIconSize;
    end;
  with Canvas do
    begin
    self.drawText(showText, fileNameRect, true, (self.selectedItemIndex = kItemFileName) and (self.editEnabled));
    if (harvestItemTemplate <> nil) and (harvestItemTemplate.icon <> nil)
      and (harvestItemTemplate.icon.icon <> nil) then
    	draw(iconRect.left, iconRect.top, harvestItemTemplate.icon.icon);
    pen.color := clBtnShadow;
    if (self.selectedItemIndex <> kItemIcon) then
      pen.color := clBtnShadow
    else
      pen.color := clBtnText;
    brush.style := bsClear;
    rectangle(iconRect.left - 1, iconRect.top - 1, iconRect.right + 1, iconRect.bottom + 1);
    end;
end;

end.

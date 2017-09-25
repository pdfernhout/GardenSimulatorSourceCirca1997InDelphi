unit Uharved;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uharved: Harvest item template (HIT) editor. There is a naming discrepancy here; in the help
system, HITs are called simply 'harvest items' because it is shorter. But in the code
harvest items are not HITs, they are objects that represent real fruits or leaves or
plants that were harvested. The HITs just contain information about what is in a 'typical'
harvested item for a cultivar. For example, there might be a HIT called 'Tomato fruit',
and a particular harvested item might be of the TYPE 'Tomato fruit', but it was harvested
from a particular tomato on a particular date. The HIT editor presents a list box of HITs
and allows the user to change the nutritional information in a columned list box,
the icon by clicking on the icon (or on 'Set icon'), and the note by typing in a memo.
The group editor supports canceling the edit by making a copy of the whole group manager
and replacing it when the user clicks OK, but here there is no HIT manager; the HIT list
is in the template manager along with a lot of other things. So the HIT editor makes
a copy of the list of HITs, then compares the two lists after the OK button is pressed.
The HIT editor can be called from the browser (from a HIT browser component - ubrowhar)
or from the main menu.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, uharvprt, ugsform;

type
  THarvestItemTemplateEditorForm = class(GsForm)
    Save: TButton;
    Cancel: TButton;
    helpButton: TBitBtn;
    copyTemplate: TButton;
    deleteTemplate: TButton;
    GroupBox1: TGroupBox;
    itemName: TEdit;
    iconImage: TImage;
    header: THeader;
    variablesList: TListBox;
    changeIconButton: TButton;
    harvestItemsGroupBox: TGroupBox;
    harvestItemTemplatesList: TListBox;
    importHarvestItemNutritionInfo: TButton;
    exportHarvestItemNutritionInfo: TButton;
    Label1: TLabel;
    noteMemo: TMemo;
    Label2: TLabel;
    procedure variablesListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure iconImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure deleteTemplateClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure headerSized(Sender: TObject; ASection, AWidth: Integer);
    procedure variablesListDblClick(Sender: TObject);
    procedure harvestItemTemplatesListMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure itemNameExit(Sender: TObject);
    procedure harvestItemTemplatesListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure copyTemplateClick(Sender: TObject);
    procedure harvestItemTemplatesListDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure helpButtonClick(Sender: TObject);
    procedure changeIconButtonClick(Sender: TObject);
    procedure variablesListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure importHarvestItemNutritionInfoClick(Sender: TObject);
    procedure exportHarvestItemNutritionInfoClick(Sender: TObject);
    procedure noteMemoExit(Sender: TObject);
    procedure noteMemoChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    noteHasChanged: boolean;
    destructor destroy; override;
    procedure loadHarvestItemTemplatesFromTemplateManager;
    procedure updateInfoForCurrentHarvestItemTemplate;
    function currentHarvestItemTemplate: GsHarvestItemTemplate;
    function variableDescription(index: smallint): string;
    function variableUnitString(index: smallint): string;
    procedure drawTextInVariablesListBox(text: string; drawRect: TRect);
    procedure setSelectedHarvestItemTemplate(aHarvestItemTemplate: GsHarvestItemTemplate);
    function saveHarvestItemTemplatesToTemplateManager: boolean;
    procedure checkDependencyBetweenWetAndDryBiomassAndWater(template: GsHarvestItemTemplate; indexChanged: integer);
    function templateForName(aName: string): GsHarvestItemTemplate;
  end;

implementation

{$R *.DFM}

uses udomain, usupport, ubrowhar, uiconch, ucursor, ufilertx;

(*this function gives a weird warning in 2.0 that '|' is undefined on line after {***} *)
procedure THarvestItemTemplateEditorForm.variablesListDblClick(Sender: TObject);
  var
    theIndex: longint;
    newValue: single;
    theTemplate: GsHarvestItemTemplate;
{$IFDEF WINDOWS}
    valueString, unitString, promptString: string;
{$ELSE}
    valueString, unitString, promptString: ansistring;
{$ENDIF}
  begin
  if self.currentHarvestItemTemplate = nil then exit;
  theTemplate := self.currentHarvestItemTemplate;
  theIndex := variablesList.itemIndex;
  unitString := self.variableUnitString(theIndex);
  promptString := 'Enter a new value for ' +  self.variableDescription(theIndex);
  if unitString <> ' - ' then
    promptString := promptString + ' in ' + unitString + '.'
  else
    promptString := promptString + '.';
  promptString := promptString + chr(13) + '(Value corresponds to wet biomass.)';
  valueString := floatToStr(theTemplate.infoArray[theIndex]);
  if inputQuery('Enter value', promptString, valueString) then
    begin
    newValue := 0.0;
    try
      newValue := strToFloat(valueString);
    except
      newValue := theTemplate.infoArray[theIndex];
    end;
    if newValue < 0.0 then newValue := 0.0;
    {***} {weird warning occurs next in 2.0}
    theTemplate.infoArray[theIndex] := newValue;
    self.checkDependencyBetweenWetAndDryBiomassAndWater(self.currentHarvestItemTemplate, theIndex);
    variablesList.invalidate;
    templatesFileMightHaveChanged := true;
    end;
  end;

procedure THarvestItemTemplateEditorForm.FormCreate(Sender: TObject);
  var i: longint;
  begin
  for i := 0 to kHarvestReportShowLastChoice do
    variablesList.items.add(Domain.harvestManager.harvestItemTemplateInfoArrayDescription(i));
  self.loadHarvestItemTemplatesFromTemplateManager;
  self.updateInfoForCurrentHarvestItemTemplate;
  variablesList.itemIndex := 0;
  end;

destructor THarvestItemTemplateEditorForm.destroy;
  var
    i: longint;
    templateInList: GsHarvestItemTemplate;
  begin
  { delete all copies of HITs in list box }
  if harvestItemTemplatesList.items.count > 0 then
    for i := 0 to harvestItemTemplatesList.items.count - 1 do
    begin
    templateInList := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[i]);
    templateInList.free;
    templateInList := nil;
    end;
  inherited destroy;
  end;

procedure THarvestItemTemplateEditorForm.loadHarvestItemTemplatesFromTemplateManager;
  var
    i: longint;
    harvestItemTemplate: GsHarvestItemTemplate;
    newHarvestItemTemplate: GsHarvestItemTemplate;
  begin
  { ask template manager to set in use flags to use in deletion }
  Domain.templateManager.setInUseFlagsForHarvestItemTemplateList(true);
  harvestItemTemplatesList.clear;
  if Domain.templateManager.harvestItemTemplateList.count > 0 then
    begin
    for i := 0 to Domain.templateManager.harvestItemTemplateList.count - 1 do
      begin
      newHarvestItemTemplate := GsHarvestItemTemplate.create;
      harvestItemTemplate := GsHarvestItemTemplate(Domain.templateManager.harvestItemTemplateList.items[i]);
      harvestItemTemplate.copyTo(newHarvestItemTemplate);
      newHarvestItemTemplate.originalIfCopy := harvestItemTemplate;
      harvestItemTemplatesList.items.addObject(newHarvestItemTemplate.getName, newHarvestItemTemplate);
      end;
    harvestItemTemplatesList.itemIndex := 0;
    end;
  end;

function THarvestItemTemplateEditorForm.saveHarvestItemTemplatesToTemplateManager: boolean;
  var
    i: longint;
    toRemoveList: TList;
    templateInList, templateInManager: GsHarvestItemTemplate;
  begin
  result := true;
  { if called from the browser, don't let them select a HIT if it has no icon }
  if owner is KfHarvestItemTemplateBrowserComponent then
    if self.currentHarvestItemTemplate.icon = nil then
      begin
      result := false;
      messageDlg('You have selected a harvest item that has no icon.' + chr(13)
        + 'You must either choose another harvest item' + chr(13)
        + 'or select an icon for this harvest item.', mtError, [mbOk], 0);
      exit;
      end;
  { check if any templates have been deleted from list box;
    if so, delete them from template manager. To do this,
    use HIT inUse flags to say they are present in list box;
    those whose flags are not set should be removed }
  toRemoveList := TList.create;
  Domain.templateManager.setAllHarvestItemTemplateInUseFlagsToFalse;
  if harvestItemTemplatesList.items.count > 0 then
    for i := 0 to harvestItemTemplatesList.items.count - 1 do
      begin
      templateInList := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[i]);
      if templateInList = nil then continue;
      templateInManager := GsHarvestItemTemplate(templateInList.originalIfCopy);
      if templateInManager = nil then continue;
      templateInManager.inUse := true;
      end;
  { two step process to avoid changing list as traverse it }
  if Domain.templateManager.harvestItemTemplateList.count > 0 then
    for i := 0 to Domain.templateManager.harvestItemTemplateList.count - 1 do
      begin
      templateInManager := GsHarvestItemTemplate(Domain.templateManager.harvestItemTemplateList.items[i]);
      if not templateInManager.inUse then toRemoveList.add(templateInManager);
      end;
  { now remove unused (deleted) harvest item templates from template manager }
  if toRemoveList.count > 0 then
    for i := 0 to toRemoveList.count - 1 do
      Domain.templateManager.harvestItemTemplateList.remove(toRemoveList.items[i]);
  toRemoveList.free;
  toRemoveList := nil;
  { now copy the contents of all HITs in list box to template manager versions }
  if harvestItemTemplatesList.items.count > 0 then
    for i := 0 to harvestItemTemplatesList.items.count - 1 do
      begin
      templateInList := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[i]);
      if templateInList = nil then continue;
      templateInManager := GsHarvestItemTemplate(templateInList.originalIfCopy);
      { if new template, make new one in template manager }
      if templateInManager = nil then
        begin
        templateInManager := GsHarvestItemTemplate.create;
        templateInList.originalIfCopy := templateInManager;
        Domain.templateManager.harvestItemTemplateList.add(templateInManager);
        end;
      templateInList.copyTo(templateInManager);
      end;
  { if owner is a browser component with field harvestItemTemplate, tell owner what
    selected pointer is in template manager. Note that this must be done last so we know what the
    HIT's counterpart is in the template manager even if it was created just now. }
  { This could also be called from the garden window, which updates the browser itself. }
  if owner is KfHarvestItemTemplateBrowserComponent then
    (owner as KfHarvestItemTemplateBrowserComponent).harvestItemTemplate :=
      GsHarvestItemTemplate(self.currentHarvestItemTemplate.originalIfCopy);
  end;

function THarvestItemTemplateEditorForm.currentHarvestItemTemplate: GsHarvestItemTemplate;
  begin
  if (harvestItemTemplatesList.items.count <= 0) or (harvestItemTemplatesList.itemIndex < 0) then
    result := nil
  else
    result := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[harvestItemTemplatesList.itemIndex]);
  end;

procedure THarvestItemTemplateEditorForm.updateInfoForCurrentHarvestItemTemplate;
  var
    theTemplate: GsHarvestItemTemplate;
  begin
  theTemplate := self.currentHarvestItemTemplate;
  if theTemplate = nil then
    begin
    itemName.text := '(none selected)';
    iconImage.picture.icon := nil;
    deleteTemplate.enabled := false;
    exit;
    end;
  itemName.text := theTemplate.getName;
  if (theTemplate.icon <> nil) and (theTemplate.icon.icon <> nil) then
    iconImage.picture.icon := theTemplate.icon.icon
  else
    iconImage.picture.icon := nil;
  if theTemplate.inUse then
    deleteTemplate.enabled := false
  else
    deleteTemplate.enabled := true;
  variablesList.invalidate;
  harvestItemTemplatesList.invalidate;
  transferNoteAndMemo(theTemplate.note, noteMemo, kGetField);
  self.noteHasChanged := false;
  end;

procedure THarvestItemTemplateEditorForm.setSelectedHarvestItemTemplate(aHarvestItemTemplate: GsHarvestItemTemplate);
  var
    i: longint;
    templateInList, templateInManager: GsHarvestItemTemplate;
  begin
  if aHarvestItemTemplate = nil then exit;
  if harvestItemTemplatesList.items.count <= 0 then exit;
  for i := 0 to harvestItemTemplatesList.items.count - 1 do
    begin
    templateInList := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[i]);
    if templateInList = nil then continue;
    templateInManager := GsHarvestItemTemplate(templateInList.originalIfCopy);
    if templateInManager = nil then continue;
    if templateInManager = aHarvestItemTemplate then
      harvestItemTemplatesList.itemIndex := harvestItemTemplatesList.items.indexOfObject(templateInList);
    end;
  self.updateInfoForCurrentHarvestItemTemplate;
  end;

procedure THarvestItemTemplateEditorForm.copyTemplateClick(Sender: TObject);
  var
    newHarvestItemTemplate: GsHarvestItemTemplate;
{$IFDEF WINDOWS}
    nameString: string;
{$ELSE}
    nameString: ansistring;
{$ENDIF}
  begin
  if self.currentHarvestItemTemplate = nil then exit;
  nameString := self.currentHarvestItemTemplate.getName + ' copy';
  if inputQuery('New harvest item', 'Enter a name for the new harvest item', nameString) then
    begin
    newHarvestItemTemplate := GsHarvestItemTemplate.create;
    self.currentHarvestItemTemplate.copyTo(newHarvestItemTemplate);
    newHarvestItemTemplate.originalIfCopy := nil;
    newHarvestItemTemplate.setName(nameString);
    newHarvestItemTemplate.inUse := false;
    harvestItemTemplatesList.items.addObject(newHarvestItemTemplate.getName, newHarvestItemTemplate);
    harvestItemTemplatesList.itemIndex := harvestItemTemplatesList.items.count - 1;
    harvestItemTemplatesList.invalidate;
    self.updateInfoForCurrentHarvestItemTemplate;  
    templatesFileMightHaveChanged := true;
    end;
  end;

const
  kSectionVariable = 0;
  kSectionValue = 1;
  kSectionUnit = 2;

procedure THarvestItemTemplateEditorForm.variablesListDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    i, leftPos: integer;
    drawRects: array[kSectionVariable..kSectionUnit] of TRect;
    selected: boolean;
    valueText: string;
  begin
  if Application.terminated then exit;
  if (variablesList.items.count <= 0) or (index < 0) or (index > variablesList.items.count - 1) then exit;
  if self.currentHarvestItemTemplate = nil then exit;
  selected := odSelected in state;
  { set up drawing rects using header }
  leftPos := 0;
  for i := kSectionVariable to kSectionUnit do
    begin
    drawRects[i].top := rect.top;
    drawRects[i].bottom := rect.bottom;
    drawRects[i].left := leftPos;
    drawRects[i].right := leftPos + header.sectionWidth[i];
    leftPos := drawRects[i].right;
    end;
  { fill checkmark and line rects with white, others with clHighlight if selected }
  for i := kSectionVariable to kSectionUnit do with variablesList.canvas do
    begin
    brush.style := bsSolid;
    if selected then
      brush.color := clHighlight
    else
      brush.color := clWindow;
    fillRect(drawRects[i]);
    brush.style := bsClear;
    end;
  self.drawTextInVariablesListBox(self.variableDescription(index), drawRects[kSectionVariable]);
  if index = kHarvestReportShowCount then
    { show count as integer }
    valueText := floatToStrF(self.currentHarvestItemTemplate.infoArray[index], ffFixed, 7, 0)
  else
    valueText := digitValueString(self.currentHarvestItemTemplate.infoArray[index]);
  self.drawTextInVariablesListBox(valueText, drawRects[kSectionValue]);
  self.drawTextInVariablesListBox(self.variableUnitString(index), drawRects[kSectionUnit]);
  if selected then variablesList.canvas.drawFocusRect(rect);
  end;

procedure THarvestItemTemplateEditorForm.drawTextInVariablesListBox(text: string; drawRect: TRect);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with variablesList.canvas do
    begin
    if length(text) > 0 then
      begin
      font := variablesList.font;
      font.color := clBtnText;
      strPCopy(cText, text);
      drawRect.left := drawRect.left + 5; { margin for text }
      drawText(handle, cText, strLen(cText), drawRect, DT_LEFT);
      drawRect.left := drawRect.left - 5;
      end;
    pen.color := clBtnShadow;
    moveTo(drawRect.right, drawRect.top);
    lineTo(drawRect.right, drawRect.bottom);
    end;
  end;

procedure THarvestItemTemplateEditorForm.harvestItemTemplatesListDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    selected: boolean;
    useText: string;
    cText: array[0..255] of Char;
  begin
  if Application.terminated then exit;
  if (harvestItemTemplatesList.items.count <= 0) or (index < 0) or (index > harvestItemTemplatesList.items.count - 1) then
    exit;
  selected := odSelected in state;
  with harvestItemTemplatesList.canvas do
    begin
    brush.style := bsSolid;
    if selected then
      brush.color := clHighlight
    else
      brush.color := clWindow;
    fillRect(rect);
    brush.style := bsClear;  
    font := harvestItemTemplatesList.font;
    font.color := clBtnText;
    if GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[index]).inUse then
      useText := harvestItemTemplatesList.items[index]
    else
      useText := harvestItemTemplatesList.items[index] + ' (unused)';
    strPCopy(cText, useText);
    rect.left := rect.left + 3; { margin for text }
    drawText(handle, cText, strLen(cText), rect, DT_LEFT);
    rect.left := rect.left - 3;
    if selected then drawFocusRect(rect);
    end;
  end;

function THarvestItemTemplateEditorForm.variableDescription(index: smallint): string;
  var
    leftParen: smallint;
  begin
  if (index < 0) or (index > variablesList.items.count - 1) then
    result := ''
  else
    begin
    leftParen := pos('(', variablesList.items[index]);
    if leftParen <> 0 then
      result := copy(variablesList.items[index], 1, leftParen - 2) { lose space before parenthesis }
    else
      result := variablesList.items[index];
    end;
  end;

function THarvestItemTemplateEditorForm.variableUnitString(index: smallint): string;
  var
    leftParen: smallint;
  begin
  if (index < 0) or (index > variablesList.items.count - 1) then
    result := ''
  else
    begin
    leftParen := pos('(', variablesList.items[index]);
    if leftParen <> 0 then
      { lose right parenthesis }
      result := copy(variablesList.items[index], leftParen + 1, length(variablesList.items[index]) - leftParen - 1)
    else
      result := ' - ';
    end;
  end;

procedure THarvestItemTemplateEditorForm.itemNameExit(Sender: TObject);
  var saveItemIndex: longint;
  begin
  if self.currentHarvestItemTemplate = nil then exit;
  self.currentHarvestItemTemplate.setName(itemName.text);
  { if you change the text for a selection in a list box, you have to reset the itemIndex because
    the list box will lose it - remember this - took a while to figure out }
  saveItemIndex := harvestItemTemplatesList.itemIndex;
  harvestItemTemplatesList.items[harvestItemTemplatesList.itemIndex] := itemName.text;
  harvestItemTemplatesList.itemIndex := saveItemIndex;
  end;

procedure THarvestItemTemplateEditorForm.changeIconButtonClick(
  Sender: TObject);
  begin
  self.iconImageClick(self);
  end;

procedure THarvestItemTemplateEditorForm.iconImageClick(Sender: TObject);
  var
    iconChooserForm: TIconChooserForm;
  begin
  if self.currentHarvestItemTemplate = nil then exit;
  iconChooserForm := TIconChooserForm.create(self);
  if iconChooserForm = nil then
    raise Exception.create('Could not create icon chooser window');
  try
    iconChooserForm.setSelectedIcon(self.currentHarvestItemTemplate.icon);
    if iconChooserForm.showModal = mrOK then
      begin
      { when user clicks OK on form, it copies correct pointer to currentHarvestItemTemplate.icon }
      { we just deal with the change by invalidating (cannot undo) }
      self.updateInfoForCurrentHarvestItemTemplate;
      templatesFileMightHaveChanged := true;
      end;
  finally
    iconChooserForm.free;
    iconChooserForm := nil;
  end;
  end;

procedure THarvestItemTemplateEditorForm.deleteTemplateClick(Sender: TObject);
  var
    harvestItemTemplate: GsHarvestItemTemplate;
  begin
  { can only delete harvest item template if it has no references }
  { this button should be disabled if a HIT has any references (is in use) }
  { free, but delete only from list box; when close dialog with OK, will update template manager }
  if (harvestItemTemplatesList.itemIndex >= 0) then
    begin
    if messageDlg('Delete this harvest item?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
    harvestItemTemplate := GsHarvestItemTemplate(
        harvestItemTemplatesList.items.objects[harvestItemTemplatesList.itemIndex]);
    if harvestItemTemplate = nil then exit;
    harvestItemTemplatesList.items.delete(harvestItemTemplatesList.itemIndex);
    harvestItemTemplate.free;
    harvestItemTemplate := nil;
    templatesFileMightHaveChanged := true;
    end;
  end;

procedure THarvestItemTemplateEditorForm.SaveClick(Sender: TObject);
  begin
  if self.saveHarvestItemTemplatesToTemplateManager then
    modalResult := mrOK
  else
    modalResult := mrNone;
  end;

procedure THarvestItemTemplateEditorForm.CancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure THarvestItemTemplateEditorForm.headerSized(Sender: TObject;
  ASection, AWidth: Integer);
  begin
  variablesList.invalidate;
  end;

procedure THarvestItemTemplateEditorForm.checkDependencyBetweenWetAndDryBiomassAndWater(template: GsHarvestItemTemplate;
    indexChanged: integer);
  var
    water_frn, wet_g, dry_g: single;
  begin
  if (indexChanged <> kHarvestReportShowWater_pct) and
    (indexChanged <> kHarvestReportShowBiomassWet_g) and
    (indexChanged <> kHarvestReportShowBiomassDry_g) then exit;
  water_frn := template.infoArray[kHarvestReportShowWater_pct] / 100.0;
  wet_g := template.infoArray[kHarvestReportShowBiomassWet_g];
  dry_g := template.infoArray[kHarvestReportShowBiomassDry_g];
  case indexChanged of
    kHarvestReportShowWater_pct, kHarvestReportShowBiomassWet_g:
      { recalculate dry biomass from wet biomass and water }
      dry_g := wet_g * (1.0 - water_frn);
    kHarvestReportShowBiomassDry_g:
      { recalculate percent water if have wet, or wet if have percent water }
      if wet_g <> 0.0 then
        water_frn := 1.0 - (dry_g / wet_g)
      else { wet = 0 }
        begin
        if water_frn <> 0.0 then
          wet_g := dry_g / (1.0 - water_frn)
        else
          begin
          wet_g := 0.0;
          water_frn := 0.0;
          end;
        end;
    end;
  template.infoArray[kHarvestReportShowWater_pct] := water_frn * 100.0;
  template.infoArray[kHarvestReportShowBiomassWet_g] := wet_g;
  template.infoArray[kHarvestReportShowBiomassDry_g] := dry_g;
  end;

procedure THarvestItemTemplateEditorForm.harvestItemTemplatesListMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.updateInfoForCurrentHarvestItemTemplate;
  end;

procedure THarvestItemTemplateEditorForm.harvestItemTemplatesListKeyUp(
  Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
  self.updateInfoForCurrentHarvestItemTemplate;
  end;

procedure THarvestItemTemplateEditorForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Harvest_item_editor')
  end;

procedure THarvestItemTemplateEditorForm.variablesListKeyUp(
  Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
  if (key = VK_SPACE) then
    begin
    self.variablesListDblClick(self);
    if variablesList.itemIndex < variablesList.items.count - 1 then
      variablesList.itemIndex := variablesList.itemIndex + 1;
    end;
  end;

function THarvestItemTemplateEditorForm.templateForName(aName: string): GsHarvestItemTemplate;
  var
    template: GsHarvestItemTemplate;
    i: longint;
  begin
  result := nil;
  if harvestItemTemplatesList.items.count > 0 then
    for i := 0 to harvestItemTemplatesList.items.count - 1 do
    begin
    template := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[i]);
    if template.getName = aName then
      begin
      result := template;
      exit;
      end;
    end;
  end;


procedure THarvestItemTemplateEditorForm.importHarvestItemNutritionInfoClick(Sender: TObject);
  var
    fileNameWithPath, templateName, valueString: string;
    template: GsHarvestItemTemplate;
    i: smallint;
    textFiler: GsTextFiler;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTabbedText, 'harvitem.tab');
  if fileNameWithPath = '' then exit;
  textFiler := GsTextFiler.createWithFileNameForReading(fileNameWithPath);
  try
    textFiler.skipRestOfLine; { skip over labels line  }
    while not textFiler.atEndOfFile do
      begin
      textFiler.streamString(templateName, '');
      template := self.templateForName(templateName);
      if template = nil then
        begin
        template := GsHarvestItemTemplate.create;
        if template = nil then
          raise Exception.create('Could not create new harvest item');
        template.setName(templateName);
        template.originalIfCopy := nil;
        template.inUse := false;
        harvestItemTemplatesList.items.addObject(template.getName, template);
        end;
      for i := 0 to variablesList.items.count - 1 do
        textFiler.streamSingle(template.infoArray[i], '');
      self.checkDependencyBetweenWetAndDryBiomassAndWater(template, kHarvestReportShowWater_pct);
      textFiler.streamPChar(template.note, 'note');
      textFiler.streamEndOfLine;
      end;
  finally
    textFiler.free;
    textFiler := nil;
    templatesFileMightHaveChanged := true;
  end;
  variablesList.invalidate;
  end;

procedure THarvestItemTemplateEditorForm.exportHarvestItemNutritionInfoClick(
  Sender: TObject);
  var
    i, j: integer;
    template: GsHarvestItemTemplate;
    fileInfo: SaveFileNamesStructure;
    textFiler: GsTextFiler;
    valueText: string;
  begin
  if harvestItemTemplatesList.items.count <= 0 then exit;
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, 'harvitem.tab', fileInfo) then exit;
  textFiler := GsTextFiler.createWithFileNameForWriting(fileInfo.tempFile);
  try
  	cursor_startWait;
    { labels line }
    textFiler.writeLiteralString('name');
    for i := 0 to variablesList.items.count - 1 do
      textFiler.writeLiteralString(self.variableDescription(i));
    textFiler.writeLiteralString('note');
    textFiler.streamEndOfLine;
    for i := 0 to harvestItemTemplatesList.items.count - 1 do
      begin
      template := GsHarvestItemTemplate(harvestItemTemplatesList.items.objects[i]);
      if template <> nil then
        begin
        textFiler.writeLiteralString(template.getName);
        for j := 0 to variablesList.items.count - 1 do
          begin
          if j = kHarvestReportShowCount then
            { show count as integer }
            valueText := floatToStrF(template.infoArray[j], ffFixed, 7, 0)
          else
            valueText := digitValueString(template.infoArray[j]);
          textFiler.writeLiteralString(valueText);
          end;
        textFiler.streamPChar(template.note, '');
        textFiler.streamEndOfLine;
        end;
      end;
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    textFiler.free;
    textFiler := nil;
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure THarvestItemTemplateEditorForm.noteMemoExit(Sender: TObject);
  begin
  if (self.currentHarvestItemTemplate <> nil) and (self.noteHasChanged) then
    transferNoteAndMemo(self.currentHarvestItemTemplate.note, noteMemo, kSetField);
  end;

procedure THarvestItemTemplateEditorForm.noteMemoChange(Sender: TObject);
  begin
  self.noteHasChanged := true;
  end;

end.

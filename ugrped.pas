unit Ugrped;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugrped: Group editor form. Used to edit groups in the groups file and to change some
characteristics of each group item. Groups are defined in ugroups and controlled
by the group manager (which the domain has). List boxes of group items ('choices')
and aspects ('variables') have pointers to the correct group items or aspects. A
NEW group manager is made inside the form, and if the user clicks OK the new
group manager is made current and the old one is freed. If the user clicks Cancel the
new group manager is freed and nothing is done to the current group manager. The user
can also save or load a group file from here, or import/export a tabbed text groups
file. Note that the tab import/export here works differently than in the templates
window. In fact, this way of doing tabbed text i/o is the norm, and the templates
way (with ufilertx) is the exception.}

interface

uses SysUtils, WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Grids, Outline, UmConsts, UAspects, UGroups, Dialogs, umodel,
  ExtCtrls, ugsform;

type
  TGroupEditorForm = class(GsForm)
    Cancel: TButton;
    Save: TButton;
    chooseFont: TBitBtn;
    FontDialog1: TFontDialog;
    helpButton: TBitBtn;
    importGroups: TButton;
    exportGroups: TButton;
    filePanel: TPanel;
    groupFileLabel: TLabel;
    groupFileName: TEdit;
    loadGroupFile: TButton;
    SaveGroupsAs: TButton;
    saveGroups: TButton;
    aspectsPanel: TPanel;
    variables: TListBox;
    aspectsShowPanel: TPanel;
    Label3: TLabel;
    showParams: TCheckBox;
    showVars: TCheckBox;
    showSingleNumbers: TCheckBox;
    showArrays: TCheckBox;
    showChoices: TCheckBox;
    show3DObjects: TCheckBox;
    showColors: TCheckBox;
    showHarvestItems: TCheckBox;
    objects: TComboBox;
    Label2: TLabel;
    groupPanel: TPanel;
    add: TBitBtn;
    remove: TBitBtn;
    up: TBitBtn;
    down: TBitBtn;
    groupLabel: TLabel;
    group: TComboBox;
    newGroup: TButton;
    RenameGroup: TButton;
    copyGroup: TButton;
    deleteGroup: TButton;
    Choices: TListBox;
    splitter: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    layerOptions: TBitBtn;
    findAspect: TButton;
    findAspectAgain: TButton;
    aspectDerivation: TBitBtn;
    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure upClick(Sender: TObject);
    procedure downClick(Sender: TObject);
    procedure newGroupClick(Sender: TObject);
    procedure deleteGroupClick(Sender: TObject);
    procedure groupChange(Sender: TObject);
    procedure SaveGroupsAsClick(Sender: TObject);
    procedure RenameGroupClick(Sender: TObject);
    procedure objectsChange(Sender: TObject);
    procedure importGroupsClick(Sender: TObject);
    procedure exportGroupsClick(Sender: TObject);
    procedure SaveGroupsClick(Sender: TObject);
    procedure chooseFontClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure loadGroupFileClick(Sender: TObject);
    procedure copyGroupClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure showParamsClick(Sender: TObject);
    procedure showVarsClick(Sender: TObject);
    procedure showDailyVarsClick(Sender: TObject);
    procedure showSingleNumbersClick(Sender: TObject);
    procedure showArraysClick(Sender: TObject);
    procedure showChoicesClick(Sender: TObject);
    procedure showColorsClick(Sender: TObject);
    procedure show3DObjectsClick(Sender: TObject);
    procedure showHarvestItemsClick(Sender: TObject);
    procedure splitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure splitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure splitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure ChoicesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure variablesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ChoicesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure variablesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChoicesEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure variablesEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure layerOptionsClick(Sender: TObject);
    procedure findAspectClick(Sender: TObject);
    procedure findAspectAgainClick(Sender: TObject);
    procedure aspectDerivationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    currentGroupManager: GsGroupManager;
    splitterDragging : boolean;
    splitterNeedToRedraw : boolean;
    splitterStartPos : integer;
    splitterLastDrawPos : integer;
    lastHeight: integer;
    dragItemsY: integer;
    searchNextString: string;
    lastIndexFound: longint;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
		procedure loadObjectTypes;
		procedure addVariablesForObjectType(objectType: integer);
    procedure updateGroupDisplay;
    procedure updateChoicesForCurrentGroup;
    procedure initializeAspects;
    procedure initializeGroups;
    procedure selectGroup(newGroup: GsGroup);
    procedure setObjectTypeForModel(model: GsModel);
    procedure groupsSaveOrSaveAs(askForFileName: boolean);
    function currentGroup: GsGroup;
    function objectTypeForItemIndex(index: smallint): smallint;
    function itemIndexForObjectType(objectType: smallint): smallint;
    procedure resizeFilePanel;
    procedure resizeGroupPanel;
    procedure resizeAspectsPanel;
		procedure resizePanelsToSplitter;
    function drawSplitterLine(pos: integer): integer;
    procedure undrawSplitterLine;
    function searchForStringInAspectListAndSelectIfFound(aString: string): boolean;
    function hintForAspectInListBox(component: TComponent; cursorPos: TPoint): string;
  end;

const
    kSplitterDragMinPixels = 50;

implementation

uses UFiler, UGsim, Messages, ubrowser, udomain, udebug, usupport, ucursor, ulayers, uunits, uderopt;

{$R *.DFM}

constructor TGroupEditorForm.create(AOwner: TComponent);
  begin
  inherited create(AOwner);
  self.dragItemsY := 0;
	currentGroupManager := GsGroupManager.create;
  if (currentGroupManager = nil) or (Domain.groupManager = nil) then
    begin
    raise Exception.create('Could not create copy of group manager');
    modalResult := mrCancel;
    exit;
    end;
 Domain.groupManager.copyTo(currentGroupManager);
 self.initializeAspects;
 self.initializeGroups;
 layerOptions.enabled := false;
 aspectDerivation.enabled := false;
 end;

procedure TGroupEditorForm.FormActivate(Sender: TObject);
  begin
  groupFileName.text := lowerCase(Domain.fileOptions.groupsFileName);
  splitter.top := filePanel.top + filePanel.height + (self.clientHeight - filePanel.height) div 2;
  self.resizePanelsToSplitter;
  lastHeight := self.clientHeight;
  { this will make list boxes create horizontal scroll bars }
  {  sendMessage(variables.handle, LB_SETHORIZONTALEXTENT, 1000, longint(0));
     sendMessage(choices.handle, LB_SETHORIZONTALEXTENT, 1000, longint(0)); }
  self.canvas.font := self.font;
  showParams.checked := true;
  showSingleNumbers.checked := true;
  end;

procedure TGroupEditorForm.FormDeactivate(Sender: TObject);
  begin
  currentGroupManager.free;
  currentGroupManager := nil;
  end;

procedure TGroupEditorForm.SaveClick(Sender: TObject);
  begin
  groupsFileMightHaveChanged := true;
  Domain.groupManager.free;
  Domain.groupManager := nil;
  Domain.groupManager := currentGroupManager;
  currentGroupManager := nil;
  modalResult := mrOK;
  end;

procedure TGroupEditorForm.CancelClick(Sender: TObject);
  begin
  currentGroupManager.free;
  currentGroupManager := nil;
  modalResult := mrCancel;
  end;

procedure TGroupEditorForm.initializeAspects;
  begin
  self.loadObjectTypes;
  objects.itemIndex := 0;
  self.objectsChange(self);
  variables.itemIndex := -1;
  end;

procedure TGroupEditorForm.initializeGroups;
  begin
	self.updateGroupDisplay;
  end;

destructor TGroupEditorForm.destroy;
	begin
  inherited destroy;
  end;

procedure TGroupEditorForm.objectsChange(Sender: TObject);
  begin
  if (objects.itemIndex <> -1) then
    begin
    self.addVariablesForObjectType(self.objectTypeForItemIndex(objects.itemIndex));
    end
  else
    begin
    variables.clear;
    end;
  end;

function TGroupEditorForm.objectTypeForItemIndex(index: smallint): smallint;
  begin
  result := -1;
  case index of
    0: result := kObjectTypeWeather;
    1: result := kObjectTypeSoil;
    2: result := kObjectTypePlant;
    3: result := kObjectTypeDrawingPlant;
    4: result := kObjectTypeBag;
    end;
  end;

function TGroupEditorForm.itemIndexForObjectType(objectType: smallint): smallint;
  begin
  result := -1;
  case objectType of
    kObjectTypeWeather: result := 0;
    kObjectTypeSoil: result := 1;
    kObjectTypePlant: result := 2;
    kObjectTypeDrawingPlant: result := 3;
    kObjectTypeBag: result := 4;
    end;
  end;

procedure TGroupEditorForm.setObjectTypeForModel(model: GsModel);
  begin
  if model = nil then exit;
  if model.objectType = kObjectUndefined then exit;
  objects.itemIndex := self.itemIndexForObjectType(model.objectType);
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.loadObjectTypes;
  var
    i: integer;
    aName: string;
  begin
  objects.clear;
  variables.clear;
  for i := 0 to 4 do
    begin
    aName := GsAspectManager.objectTypeName(self.objectTypeForItemIndex(i));
    objects.items.add(aName);
    end;
  end;

function isUpper(ch: char): boolean;
  begin
  if ch in ['A'..'Z'] then
    result := true
  else
    result := false;
  end;

procedure TGroupEditorForm.addVariablesForObjectType(objectType: integer);
	var
    name: string;
    i: longint;
    aspect: GsAspect;
	begin
  variables.clear;
  if Domain.aspectManager.aspects.count > 0 then
  	for i := 0 to Domain.aspectManager.aspects.count - 1 do
    	begin
      aspect := GsAspect(Domain.aspectManager.aspects.items[i]);
      if aspect.objectType = objectType then
      	begin
        if (not showParams.checked) and (aspect.aspectType = kAspectTypeParameter) then continue;
        if (not showVars.checked) and (aspect.aspectType <> kAspectTypeParameter) then continue;
        if (not showSingleNumbers.checked) then
          if ((aspect.fieldType = kFieldFloat) or (aspect.fieldType = kFieldInt))
            and (aspect.indexType <= kIndexTypeNone) then continue;
        if (not showArrays.checked) and (aspect.indexType > kIndexTypeNone) then continue;
        if (not showChoices.checked) then
          if ((aspect.fieldType = kFieldBoolean) or (aspect.fieldType = kFieldEnumeratedList)) then continue;
        if (not showColors.checked) and (aspect.fieldType = kFieldColor) then continue;
        if (not show3DObjects.checked) and (aspect.fieldType = kFieldThreeDObject) then continue;
        if (not showHarvestItems.checked) and (aspect.fieldType = kFieldHarvestItemTemplate) then continue;
        name := removeUnitSuffix(aspect.aspectName);
        variables.items.addObject(name, aspect);
        end;
      end;
  self.searchNextString := '';
  self.lastIndexFound := -1;
  findAspectAgain.enabled := false;
  end;

function TGroupEditorForm.hintForAspectInListBox(component: TComponent; cursorPos: TPoint): string;
  var
    aspect: GsAspect;
    listBox: TListBox;
    itemIndex: longint;
  begin
  result := '';
  if not (component is TListBox) then exit;
  listBox := component as TListBox;
  if (listBox <> variables) and (listBox <> choices) then exit;
  itemIndex := listBox.itemAtPos(cursorPos, true); {true = don't show anthing if beyond last item}
  if itemIndex >= 0 then
    begin
    aspect := nil;
    if listBox = variables then
      aspect := GsAspect(listBox.items.objects[itemIndex])
    else if listBox = choices then
      aspect := Domain.aspectManager.aspectForFieldID(GsGroupItem(listBox.items.objects[itemIndex]).fieldID);
    if (aspect <> nil) and (aspect.hint <> nil) then
      result := strPas(aspect.hint);
    end;
  end;

procedure writeGroupLabels(var outputFile: TextFile);
  var i: smallint;
  begin
  write(outputFile, 'group'); write(outputFile, chr(9));
  write(outputFile, 'fieldID'); write(outputFile, chr(9));
  write(outputFile, 'lowerBound'); write(outputFile, chr(9));
  write(outputFile, 'upperBound'); write(outputFile, chr(9));
  write(outputFile, 'unit'); write(outputFile, chr(9));
  write(outputFile, 'derivedIndex'); write(outputFile, chr(9));
  write(outputFile, 'isLayers');  write(outputFile, chr(9));
  write(outputFile, 'arrayShow');  write(outputFile, chr(9));
  for i := 0 to 9 do
    begin
    write(outputFile, 'showLayer' + intToStr(i+1));
    write(outputFile, chr(9));
    end;
  writeln(outputFile);
  end;

procedure writeOneGroupItemToFile(aGroup: GsGroup; groupItem: GsGroupItem; var outputFile: TextFile);
  var
    i: smallint;
    aspect: GsAspect;
    writeString: string;
  begin
  write(outputFile, aGroup.name); write(outputFile, chr(9));
  write(outputFile, groupItem.fieldID); write(outputFile, chr(9));
  write(outputFile, floatToStr(groupItem.boundSoftLower)); write(outputFile, chr(9));
  write(outputFile, floatToStr(groupItem.boundSoftUpper)); write(outputFile, chr(9));
  { for unit, try to write out unit name for set in aspect }
  write(outputFile, intToStr(groupItem.currentUnit));
  if (Domain <> nil) and (Domain.aspectManager <> nil) then
    begin
    aspect := nil;
    aspect := Domain.aspectManager.aspectForFieldID(groupItem.fieldID);
    if aspect <> nil then
      begin
      writeString := UnitStringForEnum(aspect.unitSet(groupItem.derivedIndex), groupItem.currentUnit);
      if writeString = '%' then writeString := 'percent';
      write(outputFile, ' ' + writeString);
      end;
    end;
  write(outputFile, chr(9));
  write(outputFile, intToStr(groupItem.derivedIndex)); write(outputFile, chr(9));
  write(outputFile, boolToStr(groupItem.isLayerArray)); write(outputFile, chr(9));
  write(outputFile, intToStr(groupItem.arrayShowType)); write(outputFile, chr(9));
  for i := 0 to kMaxArraySize - 1 do
    begin
    write(outputFile, boolToStr(groupItem.arraySelected[i]));
    if i < kMaxArraySize - 1 then write(outputFile, chr(9));
    end;
  writeln(outputFile);
  end;

procedure TGroupEditorForm.exportGroupsClick(Sender: TObject);
  var
    i, j: integer;
    aGroup: GsGroup;
    groupItem: GsGroupItem;
    fileInfo: SaveFileNamesStructure;
    outputFile: TextFile;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, 'groups.tab', fileInfo) then exit;
  try
  	cursor_startWait;
    assignFile(outputFile, fileInfo.tempFile);
    rewrite(outputFile);
    writeGroupLabels(outputFile); { write labels at top }
    if currentGroupManager.groups.count > 0 then
      for i := 0 to currentGroupManager.groups.count - 1 do
        begin
        aGroup := currentGroupManager.groupForIndex(i);
        if aGroup.groupItems.count > 0 then
          for j := 0 to aGroup.groupItems.count - 1 do
            begin
            groupItem := aGroup.groupItemForIndex(j);
            if groupItem <> nil then writeOneGroupItemToFile(aGroup, groupItem, outputFile);
            end;
        end;
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    closeFile(outputFile);
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure TGroupEditorForm.importGroupsClick(Sender: TObject);
  var
    fileNameWithPath, groupName, lastGroupName, ignore, boundString: string;
    inputFile: TextFile;
    aGroup: GsGroup;
    groupItem: GsGroupItem;
    i: smallint;
  begin
  aGroup := nil;
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTabbedText, 'groups.tab');
  if fileNameWithPath <> '' then
	  begin
	  assignFile(inputFile, fileNameWithPath);
    try
	    reset(inputFile);
    currentGroupManager.groups.clear;
    lastGroupName := 'NONE';
    readln(inputFile, ignore); { skip over labels line  }
    while not eof(inputFile) do
      begin
      groupName := readUntilTab(inputFile);
      {if first name = NONE - will have a group for this}
      if (groupName <> lastGroupName) or (aGroup = nil) then
        begin
        aGroup := currentGroupManager.newGroup;
        aGroup.name := copy(groupName, 1, 80);
        lastGroupName := groupName;
        end;
      groupItem := aGroup.newGroupItem;
      groupItem.fieldID := readUntilTab(inputFile);
      boundString := readUntilTab(inputFile);
      boundForString(boundString, kFieldFloat, groupItem.boundSoftLower);
      boundString := readUntilTab(inputFile);
      boundForString(boundString, kFieldFloat, groupItem.boundSoftUpper);
      read(inputFile, groupItem.currentUnit);
      ignore := readUntilTab(inputFile);
      read(inputFile, groupItem.derivedIndex);
      ignore := readUntilTab(inputFile);
      readBooleanToTab(inputFile, groupItem.isLayerArray);
      read(inputFile, groupItem.arrayShowType);
      ignore := readUntilTab(inputFile);
      for i := 0 to kMaxArraySize - 1 do
        readBooleanToTab(inputFile, groupItem.arraySelected[i]);
      end;
    finally
      closeFile(inputFile);
    end;
    self.initializeGroups;
    end;
  end;

procedure TGroupEditorForm.AddClick(Sender: TObject);
  var
    i, oldCount: integer;
    newGroupItem: GsGroupItem;
  begin
  if self.currentGroup = nil then exit;
  if variables.items.count > 0 then
    begin
    oldCount := variables.items.count;
    i := 0;
    while i <= variables.items.count - 1 do
      begin
      if variables.selected[i] then
        begin
        { can add same aspect more than once }
        newGroupItem := self.currentGroup.addAspect(GsAspect(variables.items.objects[i]));
        choices.items.addObject(newGroupItem.displayName, newGroupItem);
        end;
      inc(i);
      end;
    { if anything added, make last new item selected item }
    if variables.items.count > oldCount then
      variables.itemIndex := variables.items.count - 1;
    end;
  end;

procedure TGroupEditorForm.RemoveClick(Sender: TObject);
  var
    i: integer;
  begin
  if self.currentGroup = nil then exit;
  if choices.items.count > 0 then
    begin
    i := 0;
    while i <= choices.items.count - 1 do
      begin
      if choices.selected[i] then
        begin
        self.currentGroup.groupItems.delete(i);
        choices.items.delete(i);
        end
      else { else because if you delete one, the next one becomes that index }
        inc(i);
      end;
    end;
  end;

procedure TGroupEditorForm.upClick(Sender: TObject);
  var
    i: integer;
  begin
  if self.currentGroup = nil then exit;
  if choices.items.count > 1 then
    begin
    i := 1;  { start at 1 because you can't move first one up any more }
    while i <= choices.items.count - 1 do
      begin
      if choices.selected[i] then
        begin
        self.currentGroup.groupItems.move(i, i - 1);
        choices.items.move(i, i - 1);
        choices.selected[i-1] := true;
        end;
      inc(i);
      end;
    end;
  end;

procedure TGroupEditorForm.downClick(Sender: TObject);
  var
    i: integer;
  begin
  if self.currentGroup = nil then exit;
  if choices.items.count > 1 then
    begin
    i := choices.items.count - 2;  { start at next to last one because you can't move last one down any more }
    while i >= 0 do
      begin
      if choices.selected[i] then
        begin
        self.currentGroup.groupItems.move(i, i + 1);
        choices.items.move(i, i + 1);
        choices.selected[i+1] := true;
        end;
      dec(i);
      end;
    end;
  end;

procedure TGroupEditorForm.newGroupClick(Sender: TObject);
var
{$IFDEF WINDOWS}
  newString: string;
{$ELSE}
  newString: ansistring;
{$ENDIF}
  clickedOK: Boolean;
  newGroupIndex: longint;
  newGroup: GsGroup;
begin
  newString := 'group' + IntToStr(currentGroupManager.groups.count);
  clickedOK := InputQuery('New group', 'Enter the name of the new group', newString);
  if clickedOK then
  	begin
		newGroupIndex := currentGroupManager.addGroup(newString);
    newGroup := currentGroupManager.groupForIndex(newGroupIndex);
    group.items.addObject(newGroup.name, newGroup);
    group.itemIndex := group.items.indexOfObject(newGroup);
    self.updateChoicesForCurrentGroup;
    end;
end;

procedure TGroupEditorForm.deleteGroupClick(Sender: TObject);
	begin
	if self.currentGroup <> nil then
		begin
 	  currentGroupManager.groups.remove(self.currentGroup);
    self.updateGroupDisplay;
  	end;
	end;

procedure TGroupEditorForm.updateGroupDisplay;
	var
	  aGroup: GsGroup;
    i, saveItemIndex: longint;
	begin
  saveItemIndex := group.itemIndex;
  group.items.clear;
  if currentGroupManager.groups.count > 0 then
    for i := 0 to currentGroupManager.groups.count - 1 do
     	begin
    	aGroup := currentGroupManager.groupForIndex(i);
    	group.items.addObject(aGroup.name, aGroup);
      end;
  if saveItemIndex <> -1 then
    group.itemIndex := saveItemIndex
  else if group.items.count > 0 then
    group.itemIndex := 0;
  self.updateChoicesForCurrentGroup;
  end;

procedure TGroupEditorForm.updateChoicesForCurrentGroup;
	var
    i: longint;
    groupItem: GsGroupItem;
  begin
  choices.clear;
  if self.currentGroup <> nil then
  	begin
    if self.currentGroup.groupItems.count > 0 then
    	for i := 0 to self.currentGroup.groupItems.count - 1 do
        begin
        groupItem := GsGroupItem(self.currentGroup.groupItems.items[i]);
        choices.items.addObject(groupItem.displayName, groupItem);
        end;
    end;
  end;

function TGroupEditorForm.currentGroup: GsGroup;
  begin
  if group.items.count = 0 then
    result := nil
  else if group.itemIndex <> -1 then
    result := GsGroup(group.items.objects[group.itemIndex])
  else
    result := nil;
  end;

procedure TGroupEditorForm.groupChange(Sender: TObject);
  begin
  self.updateGroupDisplay;
  end;

procedure TGroupEditorForm.selectGroup(newGroup: GsGroup);
  var
    selection: longint;
  begin
  if currentGroupManager = nil then exit;
  { since this is only called just when the form has been created, we can assume the name
    has not been changed }
  group.ItemIndex := group.items.indexOf(newGroup.name);
  self.updateChoicesForCurrentGroup;
  end;

procedure TGroupEditorForm.loadGroupFileClick(Sender: TObject);
  var
    fileNameWithPath: string;
    fileNameIsDifferent: boolean;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeGroups, Domain.fileOptions.groupsFileName);
  if fileNameWithPath <> '' then
    begin
    try
  	  cursor_startWait;
      fileNameIsDifferent := (fileNameWithPath <> Domain.fileOptions.groupsFileName);
      GsFiler.load(fileNameWithPath, currentGroupManager);
      Domain.fileOptions.groupsFileName := lowerCase(fileNameWithPath);
      groupFileName.text := lowerCase(Domain.fileOptions.groupsFileName);
      if fileNameIsDifferent then iniFileMightHaveChanged := true;
    finally
  	  cursor_stopWait;
    end;
    end;
  end;

procedure TGroupEditorForm.SaveGroupsAsClick(Sender: TObject);
  begin
  self.groupsSaveOrSaveAs(kAskForFileName);
  end;

procedure TGroupEditorForm.SaveGroupsClick(Sender: TObject);
  begin
  self.groupsSaveOrSaveAs(kDontAskForFileName);
  end;

procedure TGroupEditorForm.groupsSaveOrSaveAs(askForFileName: boolean);
  var
    fileInfo: SaveFileNamesStructure;
    fileNameIsDifferent: boolean;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeGroups, askForFileName, Domain.fileOptions.groupsFileName, fileInfo) then exit;
  fileNameIsDifferent := fileInfo.newFile <> Domain.fileOptions.groupsFileName;
  try
  	cursor_startWait;
    GsFiler.save(fileInfo.tempFile, currentGroupManager);
    Domain.fileOptions.groupsFileName := lowerCase(fileInfo.newFile);
    groupFileName.text := lowerCase(Domain.fileOptions.groupsFileName);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  if fileNameIsDifferent then iniFileMightHaveChanged := true;
  groupsFileMightHaveChanged := false;
  end;

procedure TGroupEditorForm.RenameGroupClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
  newString: string;
{$ELSE}
  newString: ansistring;
{$ENDIF}
    clickedOK: Boolean;
  begin
  if self.currentGroup <> nil then
  	begin
    newString := self.currentGroup.name;
    clickedOK := InputQuery('Rename group ' + newString, 'Please enter the new name for the group', newString);
    if clickedOK then
     	begin
		  self.currentGroup.name := newString;
      self.updateGroupDisplay;
      end;
    end;
  end;

procedure TGroupEditorForm.copyGroupClick(Sender: TObject);
  var
    newGroup: GsGroup;
{$IFDEF WINDOWS}
    nameString: string;
{$ELSE}
    nameString: ansistring;
{$ENDIF}
  begin
  if self.currentGroup = nil then exit;
  nameString := self.currentGroup.name + ' copy';
  if inputQuery('Copy group', 'Enter a name for the new group', nameString) then
    begin
    newGroup := GsGroup.create;
    self.currentGroup.copyTo(newGroup);
    newGroup.name := nameString;
		currentGroupManager.groups.add(newGroup);
    group.items.addObject(newGroup.name, newGroup);
    group.itemIndex := group.items.indexOfObject(newGroup);
    self.updateChoicesForCurrentGroup;
    end;
  end;

procedure TGroupEditorForm.chooseFontClick(Sender: TObject);
  begin
  fontDialog1.font := variables.font;
  if fontDialog1.Execute then
    begin
    variables.font := fontDialog1.font;
    choices.font := fontDialog1.font;
    application.processMessages;
    end;
  end;

procedure TGroupEditorForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Group_editor')
  end;

const
  kBetweenGap = 4;

procedure TGroupEditorForm.FormResize(Sender: TObject);
  var
    mainWidth, newTop: integer;
  begin
  filePanel.visible := false;
  groupPanel.visible := false;
  aspectsPanel.visible := false;
  splitter.visible := false;
  save.left := self.clientWidth - save.width - kBetweenGap;
  cancel.left := save.left;
  importGroups.left := save.left;
  exportGroups.left := save.left;
  findAspect.left := save.left;
  findAspectAgain.left := save.left;
  chooseFont.left := save.left;
  helpButton.left := save.left;
  mainWidth := self.clientWidth - save.width - kBetweenGap * 3;
  with filePanel do setBounds(kBetweenGap, kBetweenGap, mainWidth, groupFileName.height + kBetweenGap * 2);
  self.resizeFilePanel;
  if lastHeight <= 0 then lastHeight := self.clientHeight;
  newTop := round(1.0 * splitter.top * self.clientHeight / lastHeight);
  with splitter do setBounds(kBetweenGap, newTop, mainWidth, height);
  self.resizePanelsToSplitter;
  lastHeight := self.clientHeight;
  filePanel.visible := true;
  groupPanel.visible := true;
  aspectsPanel.visible := true;
  splitter.visible := true;
  end;

procedure TGroupEditorForm.resizePanelsToSplitter;
  var newTop: longint;
	begin
  newTop := filePanel.top + filePanel.height + kBetweenGap;
  with groupPanel do setBounds(kBetweenGap, newTop, filePanel.width, splitter.top - newTop);
  newTop := splitter.top + splitter.height;
  with aspectsPanel do setBounds(kBetweenGap, newTop, filePanel.width, self.clientHeight - newTop - kBetweenGap);
  self.resizeGroupPanel;
  self.resizeAspectsPanel;
  end;

procedure TGroupEditorForm.resizeFilePanel;
  var newLeft, newWidth: integer;
  begin
  with groupFileLabel do setBounds(kBetweenGap, kBetweenGap, width, height);
  with saveGroupsAs do setBounds(filePanel.width - width - kBetweenGap, kBetweenGap, width, height);
  with saveGroups do setBounds(saveGroupsAs.left - width, kBetweenGap, width, height);
  with loadGroupFile do setBounds(saveGroups.left - width, kBetweenGap, width, height);
  newLeft := groupFileLabel.left + groupFileLabel.width + kBetweenGap;
  newWidth := loadGroupFile.left - newLeft - kBetweenGap;
  if newWidth < 0 then newWidth := 0;
  with groupFileName do setBounds(newLeft, kBetweenGap, newWidth, height);
  end;

procedure TGroupEditorForm.resizeGroupPanel;
  var newLeft, newWidth, newTop: integer;
  begin
  with groupLabel do setBounds(kBetweenGap, kBetweenGap, width, height);
  with deleteGroup do setBounds(groupPanel.width - width - kBetweenGap, kBetweenGap, width, height);
  with copyGroup do setBounds(deleteGroup.left - width, kBetweenGap, width, height);
  with renameGroup do setBounds(copyGroup.left - width, kBetweenGap, width, height);
  with newGroup do setBounds(renameGroup.left - width, kBetweenGap, width, height);
  newLeft := groupLabel.left + groupLabel.width + kBetweenGap;
  newWidth := newGroup.left - newLeft - kBetweenGap;
  if newWidth < 0 then newWidth := 0;
  with group do setBounds(newLeft, kBetweenGap, newWidth, height);
  newTop := groupPanel.height - down.height - kBetweenGap;
  with layerOptions do setBounds(groupPanel.width - width - kBetweenGap, newTop, width, height);
  with aspectDerivation do setBounds(layerOptions.left - width, newTop, width, height);
  with down do setBounds(aspectDerivation.left - width, newTop, width, height);
  with up do setBounds(down.left - width, newTop, width, height);
  with remove do setBounds(up.left - width, newTop, width, height);
  with add do setBounds(remove.left - width, newTop, width, height);
  newTop := group.top + group.height + kBetweenGap;
  with choices do setBounds(kBetweenGap, newTop,
      groupPanel.width - kBetweenGap * 2, add.top - newTop - kBetweenGap);
  end;

procedure TGroupEditorForm.resizeAspectsPanel;
  begin
  with aspectsShowPanel do setBounds(0, 0, aspectsPanel.width, height);
  with variables do setBounds(kBetweenGap, aspectsShowPanel.height + kBetweenGap,
      aspectsPanel.width - kBetweenGap * 2, aspectsPanel.height - aspectsShowPanel.height - kBetweenGap * 2);
  end;

procedure TGroupEditorForm.splitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.splitterDragging := true;
  self.splitterStartPos := y;
  self.splitterLastDrawPos := -1;
  self.splitterNeedToRedraw := true;
  inherited mouseDown(button, shift, x, y);
  end;

procedure TGroupEditorForm.splitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  var splitRegionTop, minTop, minBottom: integer;
  begin
  splitRegionTop := filePanel.top + filePanel.height + kBetweenGap;
  minTop := splitRegionTop + group.height * 2 + add.height + kBetweenGap * 4;
  minBottom := aspectsShowPanel.height + group.height + kBetweenGap * 2;
  if self.splitterDragging and
    (splitter.top + y >= minTop)
      and (splitter.top + y < self.clientHeight - splitRegionTop - minBottom) then
      begin
      self.undrawSplitterLine;
      self.splitterLastDrawPos := self.drawSplitterLine(y);
      end;
  inherited mouseMove(shift, x, y);
  end;

procedure TGroupEditorForm.splitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 var splitRegionTop, minTop, minBottom: integer;
  begin
  if self.splitterDragging then
    begin
    self.undrawSplitterLine;
    splitRegionTop := filePanel.top + filePanel.height + kBetweenGap;
    minTop := splitRegionTop + group.height * 2 + add.height + kBetweenGap * 4;
    minBottom := aspectsShowPanel.height + group.height + kBetweenGap * 2;
    splitter.top := splitter.top - (splitterStartPos - y);
    if splitter.top < minTop then splitter.top := minTop;
    if splitter.top > self.clientHeight - splitRegionTop - minBottom then
      splitter.top := self.clientHeight - splitRegionTop - minBottom;
  	self.resizePanelsToSplitter;
    self.resizeGroupPanel;
    self.resizeAspectsPanel;
    self.splitterDragging := false;
    end;
  inherited mouseUp(button, shift, x, y);
  end;

function TGroupEditorForm.drawSplitterLine(pos: integer): integer;
  var
    theDC: HDC;
  begin
  theDC := getDC(0);
  result := self.clientOrigin.y + splitter.top + pos + 2;
  patBlt(theDC, self.clientOrigin.x + splitter.left, result, splitter.width, 1, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := true;
  end;

procedure TGroupEditorForm.undrawSplitterLine;
  var theDC: HDC;
  begin
  if not self.splitterNeedToRedraw then exit;
  theDC := getDC(0);
  patBlt(theDC, self.clientOrigin.x + splitter.left, self.splitterLastDrawPos, splitter.width, 1, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := false;
  end;

procedure TGroupEditorForm.showParamsClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showVarsClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showDailyVarsClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showSingleNumbersClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showArraysClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showChoicesClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showColorsClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.show3DObjectsClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

procedure TGroupEditorForm.showHarvestItemsClick(Sender: TObject);
  begin
  self.objectsChange(self);
  end;

{ mouse down - start drag, or if right mouse button, open layer options window,
  or if shift-right-mouse, open derivation window }
procedure TGroupEditorForm.ChoicesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    groupItem: GsGroupItem;
    aspect: GsAspect;
  begin
  if (choices.items.count > 0) and (choices.itemIndex >= 0) then
    begin
    groupItem := GsGroupItem(choices.items.objects[choices.itemIndex]);
    if groupItem <> nil then
      begin
      layerOptions.enabled := groupItem.isLayerArray;
      aspect := Domain.aspectManager.aspectForFieldID(groupItem.fieldID);
      if aspect <> nil then
        aspectDerivation.enabled := aspect.canBeDerived
      else
        aspectDerivation.enabled := false;
      end
    else
      layerOptions.enabled := false;
    end
  else
    layerOptions.enabled := false;
  if button = mbRight then
    begin
    if (ssShift in shift) then
      begin
      if aspectDerivation.enabled then self.aspectDerivationClick(self);
      end
    else if layerOptions.enabled then
      self.layerOptionsClick(self);
    end
  else
    begin
    self.dragItemsY := y;
    choices.beginDrag(true); { drag mode should be manual }
    end;
  end;

procedure TGroupEditorForm.variablesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  variables.beginDrag(true); { drag mode should be manual }
  end;

{ drag over - accept drag }
procedure TGroupEditorForm.ChoicesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
 begin
 { accept coming from either itself or vars list }
 accept := (Source is TListBox);
 end;

procedure TGroupEditorForm.variablesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
 begin
 { accept only coming from choices list }
 accept := (Source is TListBox) and ((Source as TListBox).name = 'Choices');
 end;

{ end drag - finish drag }
{ choices - could be dragged to choices or variables }
procedure TGroupEditorForm.ChoicesEndDrag(Sender, Target: TObject; X, Y: Integer);
  var rows, i: integer;
  begin
  if target = nil then exit;
  if not (target is TListBox) then exit;
  if (target as TListBox).name = 'variables' then  { dragged to variables }
    self.removeClick(self)
  else if (sender = target) then  { dragged to itself }
    begin
    { move items up or down }
    if self.dragItemsY <> 0 then
      begin
      rows := abs(y - self.dragItemsY) div choices.itemHeight;
      if rows > 0 then
        begin
        if (y - self.dragItemsY) > 0 then
          for i := 0 to rows - 1 do self.downClick(self)
        else
          for i := 0 to rows - 1 do self.upClick(self);
        end;
      self.dragItemsY := 0;
      end;
    end;
  end;

{ variables - could only be dragged to choices }
procedure TGroupEditorForm.variablesEndDrag(Sender, Target: TObject; X, Y: Integer);
  begin
  if target = nil then exit;
  { dragged to choices }
  if (target is TListBox) and ((target as TListBox).name = 'Choices') then
    begin
    self.addClick(self);
    exit;
    end;
  end;

procedure TGroupEditorForm.layerOptionsClick(Sender: TObject);
  var
    layersForm: TlayerOptionsForm;
    groupItem: GsGroupItem;
    response, saveItemIndex: longint;
	begin
  if (choices.items.count > 0) and (choices.itemIndex >= 0) then
    begin
    groupItem := GsGroupItem(choices.items.objects[choices.itemIndex]);
    if groupItem = nil then exit;
    layersForm := TlayerOptionsForm.create(self);
    if layersForm = nil then
      raise Exception.create('Could not create layer options window');
    try
      layersForm.initialize(groupItem.displayName, groupItem.arrayShowType, groupItem.arraySelected,
          kLayersFormIsNotBeingCalledFromGraphForm);
		  response := layersForm.showModal;
      if response = mrOK then layersForm.getResult(groupItem.arrayShowType, groupItem.arraySelected);
    finally
      layersForm.free;
      layersForm := nil;
    end;
    if response = mrOK then
      begin
      saveItemIndex := choices.itemIndex;
      choices.items[saveItemIndex] := groupItem.displayName;
      choices.itemIndex := saveItemIndex;
      choices.selected[saveItemIndex] := true;
      end;
    end;
  end;

procedure TGroupEditorForm.aspectDerivationClick(Sender: TObject);
  var
    derivationForm: TAspectDerivationForm;
    groupItem: GsGroupItem;
    aspect: GsAspect;
    response, saveItemIndex: longint;
	begin
  if (choices.items.count > 0) and (choices.itemIndex >= 0) then
    begin
    groupItem := GsGroupItem(choices.items.objects[choices.itemIndex]);
    if groupItem = nil then exit;
    aspect := Domain.aspectManager.aspectForFieldID(groupItem.fieldID);
    if aspect = nil then exit;
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
        groupItem.derivedIndex := derivationForm.deriveMethod;
    finally
      derivationForm.free;
      derivationForm := nil;
    end;
    if response = mrOK then
      begin
      saveItemIndex := choices.itemIndex;
      choices.items[saveItemIndex] := groupItem.displayName;
      choices.itemIndex := saveItemIndex;
      choices.selected[saveItemIndex] := true;
      end;
    end;
  end;

procedure TGroupEditorForm.findAspectClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
    searchString: string;
{$ELSE}
    searchString: ansistring;
{$ENDIF}
  begin
  searchString := '';
  if InputQuery('Find', 'Text to find in aspects list:', searchString) then
  	findAspectAgain.enabled := self.searchForStringInAspectListAndSelectIfFound(searchString);
  end;

procedure TGroupEditorForm.findAspectAgainClick(Sender: TObject);
  begin
  if (searchNextString <> '')
      and (lastIndexFound >= 0) and (lastIndexFound <= variables.items.count - 1) then
    self.searchForStringInAspectListAndSelectIfFound(self.searchNextString);
  end;

function TGroupEditorForm.searchForStringInAspectListAndSelectIfFound(aString: string): boolean;
  var
    i, j: longint;
    stringInList: string;
  begin
  { returns true if string found }
  result := false;
  if variables.items.count <= 1 then exit; 
  if aString = '' then exit;
  { if list box is selected and itemIndex is valid, they clicked, so go from there }
  if (variables.focused) and (variables.itemIndex >= 0) then
    lastIndexFound := variables.itemIndex;
  { if lastIndexFound is invalid, try to use currently selected item; if that is invalid, use last item }
  if (lastIndexFound < 0) or (lastIndexFound > variables.items.count - 1) then
    begin
    if (variables.itemIndex >= 0) then
      lastIndexFound := variables.itemIndex
    else
      lastIndexFound := variables.items.count - 1;
    end;
  { if lastIndexFound is last item in list, start at 0, else start at next item }
  if lastIndexFound = variables.items.count - 1 then
    i := 0
  else
    i := lastIndexFound + 1;
  while i <> lastIndexFound do
    begin
    stringInList := lowerCase(variables.items[i]);
    if pos(lowerCase(aString), stringInList) <> 0 then
      begin
      { unselect everything, then select found index }
      variables.itemIndex := -1;
      for j := 0 to variables.items.count - 1 do variables.selected[j] := false;
      variables.itemIndex := i;
      variables.selected[i] := true;
      lastIndexFound := i;
      searchNextString := aString;
      result := true;
      exit;
      end;
    inc(i);
    if i > variables.items.count - 1 then i := 0; { loop around }
    end;
  messageDlg('Text "' + aString + '" not found.', mtInformation, [mbOK], 0);
  end;

end.
(* had this for OnKeyUp, but was conflicting with selecting an item in the list boxes, so took out,
  but might want to rememeber how I did it. To use this you must set the Form's KeyPreview property to True.
  if (lowerCase(chr(key)) = 'f') and (ssCtrl in shift) then
    self.findAspectClick(self)
  else if (lowerCase(chr(key)) = 'g') and (ssCtrl in shift) then
    self.findAspectAgainClick(self);
*)

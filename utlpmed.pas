unit Utlpmed;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utlpmed: Tool parameter list editor. Allows user to change lists of choices available
when doing a tool action. These are the lists that show up in the combo boxes in the
garden window toolbar when the user picks up a tool. What we are editing here is
GsToolRateOrAmountParam objects (utools), which know their type, units, and bounds. Bounds are
hard-coded with hardCodeBoundsForModelUnit (in utools). The user can also export/import
the information for use in other gardens. The list of GsToolRateOrAmountParams is
in the toolParamListManager (in the domain), and the dialog makes a copy of
that manager when it starts up and overwrites the original if OK is pressed.
The lists themselves cannot be created or deleted or renamed. The five lists are:
* carrying amount (for carrying soil or mulch - t/ha or t),
* adding amount for solids (for applying amendments - t/ha or t),
* adding amount for liquids (not used),
* planting depth (not used),
* mixing depth (for mixing soil - meters),
* tool area (size of tool - m2).
The 'amount per second' option is invisible and code that deals with it commented out,
because we have not implemented the code the implements that type of tool action
(it would be in ugscom).}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, utools, ucollect, ugsform;

type
  TToolParamListEditorForm = class(GsForm)
    Label1: TLabel;
    OK: TButton;
    Cancel: TButton;
    helpButton: TBitBtn;
    itemsList: TListBox;
    Label2: TLabel;
    selectedItemPanel: TPanel;
    valueEdit: TEdit;
    absoluteOrRelative: TRadioGroup;
    moveItemUp: TBitBtn;
    moveItemDown: TBitBtn;
    newItem: TButton;
    copyItem: TButton;
    deleteItem: TButton;
    paramLists: TListBox;
    unitsGroupBox: TGroupBox;
    showAllItems: TRadioButton;
    showSelectedUnitSystemItems: TRadioButton;
    Label6: TLabel;
    boundsText: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    importTabbed: TButton;
    exportTabbed: TButton;
    itemUnit: TEdit;
    perClickOrSecond: TRadioGroup;
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure itemsListClick(Sender: TObject);
    procedure newItemClick(Sender: TObject);
    procedure perClickOrSecondClick(Sender: TObject);
    procedure absoluteOrRelativeClick(Sender: TObject);
    procedure valueEditExit(Sender: TObject);
    procedure copyItemClick(Sender: TObject);
    procedure deleteItemClick(Sender: TObject);
    procedure moveItemUpClick(Sender: TObject);
    procedure moveItemDownClick(Sender: TObject);
    procedure paramListsClick(Sender: TObject);
    procedure showAllItemsClick(Sender: TObject);
    procedure showSelectedUnitSystemItemsClick(Sender: TObject);
    procedure itemUnitMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure importTabbedClick(Sender: TObject);
    procedure exportTabbedClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure itemUnitKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    currentToolParamListManager: GsToolParamListManager;
    constructor create(AOwner: TComponent); override;
    function currentList: TListCollection;
    function currentItem: GsToolRateOrAmountParam;
    procedure updateForCurrentItem;
    procedure writeLabelsToOutputFile(var outputFile: textFile);
    procedure writeOneListToOutputFile(theList: TListCollection; var outputFile: textFile);
  end;

implementation

{$R *.DFM}

uses udomain, usupport, uunits;

const
  kRadioAbsolute = 0;
  kRadioRelative = 1;
  kRadioPerClick = 0;
  kRadioPerSecond = 1;

var
  updateLock: boolean;
  clickedOnList: boolean;

constructor TToolParamListEditorForm.create(AOwner: TComponent);
  begin
  inherited create(AOwner);
  updateLock := false;
  clickedOnList := false;
	currentToolParamListManager := GsToolParamListManager.create;
  if (currentToolParamListManager = nil) or (Domain.toolParamListManager = nil) then
    begin
    raise Exception.create('Could not create copy of tool param list manager');
    modalResult := mrCancel;
    exit;
    end;
 Domain.toolParamListManager.copyTo(currentToolParamListManager);
 end;

procedure TToolParamListEditorForm.FormActivate(Sender: TObject);
  begin
  paramLists.itemIndex := 0;
  showAllItems.checked := currentToolParamListManager.showParamsWithUnselectedUnitSystem;
  { shouldn't have to do this }
  showSelectedUnitSystemItems.checked := not showAllItems.checked;
  self.paramListsClick(self);
  end;

procedure TToolParamListEditorForm.FormDeactivate(Sender: TObject);
  begin
  currentToolParamListManager.free;
  currentToolParamListManager := nil;
  end;

procedure TToolParamListEditorForm.OKClick(Sender: TObject);
  begin
  Domain.toolParamListManager.free;
  Domain.toolParamListManager := nil;
  Domain.toolParamListManager := currentToolParamListManager;
  currentToolParamListManager := nil;
  modalResult := mrOK;
  end;

procedure TToolParamListEditorForm.CancelClick(Sender: TObject);
  begin
  currentToolParamListManager.free;
  currentToolParamListManager := nil;
  modalResult := mrCancel;
  end;

function TToolParamListEditorForm.currentList: TListCollection;
  begin
  result := currentToolParamListManager.listForListType(paramLists.itemIndex + 1);
  end;

function TToolParamListEditorForm.currentItem: GsToolRateOrAmountParam;
  begin
  result := currentObjectInListBox(itemsList) as GsToolRateOrAmountParam;
  end;

procedure TToolParamListEditorForm.paramListsClick(Sender: TObject);
  var
    theList: TListCollection;
    theRateOrAmountParam: GsToolRateOrAmountParam;
    i: longint;
  begin
  itemsList.clear;
  theList := currentToolParamListManager.listForListType(paramLists.itemIndex + 1);
  if theList.count > 0 then
    for i := 0 to theList.count - 1 do
      begin
      theRateOrAmountParam := GsToolRateOrAmountParam(theList.items[i]);
      if theRateOrAmountParam = nil then continue;
      if theRateOrAmountParam.valueType <> kAmountAuto then
        itemsList.items.addObject(theRateOrAmountParam.name(false), theRateOrAmountParam);
      end;
  if itemsList.items.count > 0 then
    begin
    itemsList.itemIndex := 0;
    self.itemsListClick(self);
    end;
  end;

procedure TToolParamListEditorForm.itemsListClick(Sender: TObject);
  begin
  try
    clickedOnList := true;
    if not updateLock then self.updateForCurrentItem;
  finally
    clickedOnList := false;
  end;
  end;

procedure TToolParamListEditorForm.updateForCurrentItem;
  var
    item: GsToolRateOrAmountParam;
    canBeRelativeAndPerSecond: boolean;
    saveItemIndex: longint;
  begin
  item := self.currentItem; 
  if item = nil then exit;
  try
    updateLock := true;
    valueEdit.text := digitValueString(item.toCurrentUnit(item.value));
    itemUnit.text := UnitStringForEnum(item.unitSet, item.displayUnit);
    boundsText.text := digitValueString(item.toCurrentUnit(item.lowerBound))
      + ' - ' + digitValueString(item.toCurrentUnit(item.upperBound)) + ' ' + itemUnit.text;
    if (item.valueType = kAmountPerClick) or (item.valueType = kAmountPerSecond) then
      absoluteOrRelative.itemIndex := kRadioAbsolute
    else
      absoluteOrRelative.itemIndex := kRadioRelative;
{    if (item.valueType = kAmountPerClick) or (item.valueType = kAmountPerAreaPerClick) then
      perClickOrSecond.itemIndex := kRadioPerClick
    else
      perClickOrSecond.itemIndex := kRadioPerSecond;  }
    if (paramLists.itemIndex + 1 = kListTypeAmountSolid)
        or (paramLists.itemIndex + 1 = kListTypeAmountLiquid) then
      canBeRelativeAndPerSecond := true
    else
      canBeRelativeAndPerSecond := false;
    absoluteOrRelative.enabled := canBeRelativeAndPerSecond;
  {  perClickOrSecond.enabled := canBeRelativeAndPerSecond;  }
    if not clickedOnList then
      begin
      saveItemIndex := itemsList.itemIndex;
      itemsList.items.strings[saveItemIndex] := item.name(false);
      itemsList.itemIndex := saveItemIndex;
      end;
  finally
    updateLock := false;
  end;
  end;

procedure TToolParamListEditorForm.itemUnitKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
  if (key = VK_DOWN) then
    self.itemUnitMouseUp(sender, mbLeft, [], 0, 0)
  else if (key = VK_UP) then
    self.itemUnitMouseUp(sender, mbLeft, [ssShift], 0, 0);
  end;

procedure TToolParamListEditorForm.itemUnitMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    item: GsToolRateOrAmountParam;
    lastUnit: smallint;
  begin
  if updateLock then exit;
  item := self.currentItem;
  if item = nil then exit;
  lastUnit := item.displayUnit;
  { change to next or previous unit }
  if ssShift in shift then
    item.displayUnit := GetPreviousUnitEnumInUnitSet(item.unitSet, item.displayUnit)
  else
    item.displayUnit := GetNextUnitEnumInUnitSet(item.unitSet, item.displayUnit);
  { update display }
  self.updateForCurrentItem;
  end;

procedure TToolParamListEditorForm.absoluteOrRelativeClick(Sender: TObject);
  var
    item: GsToolRateOrAmountParam;
  begin
  item := self.currentItem;
  if item = nil then exit;
  if absoluteOrRelative.itemIndex = kRadioAbsolute then
    begin
    if item.valueType = kAmountPerAreaPerClick then
      item.valueType := kAmountPerClick
    else if item.valueType = kAmountPerAreaPerSecond then
      item.valueType := kAmountPerSecond;
    end
  else
    begin
    if item.valueType = kAmountPerClick then
      item.valueType := kAmountPerAreaPerClick
    else if item.valueType = kAmountPerSecond then
      item.valueType := kAmountPerAreaPerSecond;
    end;
  item.resolveUnitSetForMaterialTypeAndValueType;
  item.switchDisplayUnitBetweenAbsoluteAndRelative;
  self.updateForCurrentItem;
  end;

procedure TToolParamListEditorForm.perClickOrSecondClick(Sender: TObject);
  var
    item: GsToolRateOrAmountParam;
  begin
  item := self.currentItem;
  if item = nil then exit;
  if perClickOrSecond.itemIndex = kRadioPerClick then
    begin
    if item.valueType = kAmountPerSecond then item.valueType := kAmountPerClick;
    if item.valueType = kAmountPerAreaPerSecond then item.valueType := kAmountPerAreaPerClick;
    end
  else
    begin
    showMessage('Tool use per second is unfinished (see help).');
    if item.valueType = kAmountPerClick then item.valueType := kAmountPerSecond;
    if item.valueType = kAmountPerAreaPerClick then item.valueType := kAmountPerAreaPerSecond;
    end;
  item.resolveUnitSetForMaterialTypeAndValueType;
  self.updateForCurrentItem;
  end;

procedure TToolParamListEditorForm.valueEditExit(Sender: TObject);
  var
    item: GsToolRateOrAmountParam;
    tryValue, itemValueInCurrentUnit, tryValueInModelUnit: single;
    lowerBoundInCurrentUnit, upperBoundInCurrentUnit: single;
  begin
  item := self.currentItem;
  if item = nil then exit;
  itemValueInCurrentUnit := item.toCurrentUnit(item.value);
  try
    tryValue := strToFloat(valueEdit.text);
  except
    tryValue := itemValueInCurrentUnit;
  end;
  if tryValue <> itemValueInCurrentUnit then
    begin
    tryValueInModelUnit := item.toModelUnit(tryValue);
    lowerBoundInCurrentUnit := item.toCurrentUnit(item.lowerBound);
    upperBoundInCurrentUnit := item.toCurrentUnit(item.upperBound);
    if (tryValueInModelUnit < item.lowerBound) or (tryValueInModelUnit > item.upperBound) then
      begin
      showMessage('The number entered (' + valueEdit.text + ') is out of bounds ('
        + digitValueString(lowerBoundInCurrentUnit) + ' - '
        + digitValueString(upperBoundInCurrentUnit) + ' ' + itemUnit.text + ').');
      end
    else
      begin
      item.value := tryValueInModelUnit;
      self.updateForCurrentItem;
      end;
    end;
  end;

procedure TToolParamListEditorForm.newItemClick(Sender: TObject);
  var
    newItem: GsToolRateOrAmountParam;
  begin
  if paramLists.itemIndex < 0 then exit;
  newItem := GsToolRateOrAmountParam.createAsMemberOfList(paramLists.itemIndex + 1);
  currentToolParamListManager.listForListType(paramLists.itemIndex + 1).add(newItem);
  itemsList.items.addObject(newItem.name(false), newItem);
  itemsList.itemIndex := itemsList.items.count - 1;
  self.updateForCurrentItem;
  end;

procedure TToolParamListEditorForm.copyItemClick(Sender: TObject);
  var
    item: GsToolRateOrAmountParam;
    newItem: GsToolRateOrAmountParam;
  begin
  if paramLists.itemIndex < 0 then exit;
  item := self.currentItem;
  if item = nil then exit;
  newItem := GsToolRateOrAmountParam.create;
  item.copyTo(newItem);
  currentToolParamListManager.listForListType(paramLists.itemIndex + 1).add(newItem);
  itemsList.items.addObject(newItem.name(false), newItem);
  itemsList.itemIndex := itemsList.items.count - 1;
  self.updateForCurrentItem;
  end;

procedure TToolParamListEditorForm.deleteItemClick(Sender: TObject);
  var
    item: GsToolRateOrAmountParam;
    index: longint;
  begin
  if paramLists.itemIndex < 0 then exit;
  item := self.currentItem;
  if item = nil then exit;
  currentToolParamListManager.listForListType(paramLists.itemIndex + 1).remove(item);
  item.free;
  item := nil;
  index := itemsList.itemIndex;
  itemsList.items.delete(itemsList.itemIndex);
  if index < itemsList.items.count then
    itemsList.itemIndex := index
  else
    itemsList.itemIndex := index - 1;
  end;

procedure TToolParamListEditorForm.moveItemUpClick(Sender: TObject);
  var
    index: longint;
  begin
  if paramLists.itemIndex < 0 then exit;
  index := itemsList.itemIndex;
  if index < 1 then exit;
  currentToolParamListManager.listForListType(paramLists.itemIndex + 1).move(index, index - 1);
  itemsList.items.move(index, index - 1);
  itemsList.itemIndex := index - 1;
  end;

procedure TToolParamListEditorForm.moveItemDownClick(Sender: TObject);
  var
    index: longint;
  begin
  if paramLists.itemIndex < 0 then exit;
  index := itemsList.itemIndex;
  if index > itemsList.items.count - 2 then exit;
  currentToolParamListManager.listForListType(paramLists.itemIndex + 1).move(index, index + 1);
  itemsList.items.move(index, index + 1);
  itemsList.itemIndex := index + 1;
  end;

procedure TToolParamListEditorForm.showAllItemsClick(Sender: TObject);
  begin
  currentToolParamListManager.showParamsWithUnselectedUnitSystem := showAllItems.checked;
  end;

procedure TToolParamListEditorForm.showSelectedUnitSystemItemsClick(
  Sender: TObject);
  begin
  currentToolParamListManager.showParamsWithUnselectedUnitSystem := showAllItems.checked;
  end;

procedure TToolParamListEditorForm.exportTabbedClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
    i: longint;
    outputFile: TextFile;
    item: GsToolRateOrAmountParam;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, 'params.tab', fileInfo) then exit;
  try
    assignFile(outputFile, fileInfo.tempFile);
    rewrite(outputFile);
    self.writeLabelsToOutputFile(outputFile);
    self.writeOneListToOutputFile(currentToolParamListManager.amountCarryingList, outputFile);
    self.writeOneListToOutputFile(currentToolParamListManager.amountSolidList, outputFile);
    self.writeOneListToOutputFile(currentToolParamListManager.amountLiquidList, outputFile);
    self.writeOneListToOutputFile(currentToolParamListManager.depthPlantingList, outputFile);
    self.writeOneListToOutputFile(currentToolParamListManager.depthMixingList, outputFile);
    self.writeOneListToOutputFile(currentToolParamListManager.toolAreaList, outputFile);
    fileInfo.writingWasSuccessful := true;
  finally
    closeFile(outputFile);
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure TToolParamListEditorForm.writeLabelsToOutputFile(var outputFile: textFile);
  begin
  write(outputFile, 'list' + chr(9));
  write(outputFile, 'material' + chr(9));
  write(outputFile, 'type' +  chr(9));
  write(outputFile, 'value' +  chr(9));
  write(outputFile, 'unit');
  writeln(outputFile);
  end;

procedure TToolParamListEditorForm.writeOneListToOutputFile(
    theList: TListCollection; var outputFile: textFile);
  var
    i: longint;
    item: GsToolRateOrAmountParam;
  begin
  if theList.count > 0 then
    for i := 0 to theList.count - 1 do
      begin
      item := GsToolRateOrAmountParam(theList.items[i]);
      if item <> nil then with item do
        begin
        write(outputFile, intToStr(listType) + ' ' + listTypeName + chr(9));
        write(outputFile, intToStr(materialType) + ' ' + materialTypeName + chr(9));
        write(outputFile, intToStr(valueType) + ' ' + valueTypeName + chr(9));
        write(outputFile, floatToStr(toCurrentUnit(value))); write(outputFile, chr(9));
        if (displayUnit <> 0) and (unitSet <> 0) then
          write(outputFile, intToStr(displayUnit) + ' ' + unitStringForEnum(unitSet, displayUnit))
        else
          write(outputFile, intToStr(displayUnit));
        writeln(outputFile);
        end;
      end;
  end;

procedure TToolParamListEditorForm.importTabbedClick(Sender: TObject);
  var
    fileNameWithPath, ignore, toFirstTab: string;
    inputFile: TextFile;
    item: GsToolRateOrAmountParam;
    listType, materialType, valueType, displayUnit: smallint;
    value: single;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTabbedText, 'params.tab');
  if fileNameWithPath = '' then exit;
  try
    assignFile(inputFile, fileNameWithPath);
    reset(inputFile);
    currentToolParamListManager.clearAllLists;
    readln(inputFile, ignore); { skip over labels line  }
    while not eof(inputFile) do
      begin
      read(inputFile, listType);
      ignore := readUntilTab(inputFile);
      read(inputFile, materialType);
      ignore := readUntilTab(inputFile);
      read(inputFile, valueType);
      ignore := readUntilTab(inputFile);
      read(inputFile, value);
      ignore := readUntilTab(inputFile);
      read(inputFile, displayUnit);
      ignore := readUntilTab(inputFile);
      item := GsToolRateOrAmountParam.createWithInfo(listType, materialType, valueType, value, displayUnit);
      case listType of
        kListTypeAmountCarrying: currentToolParamListManager.amountCarryingList.add(item);
        kListTypeAmountSolid: currentToolParamListManager.amountSolidList.add(item);
        kListTypeAmountLiquid: currentToolParamListManager.amountLiquidList.add(item);
        kListTypeDepthPlanting: currentToolParamListManager.depthPlantingList.add(item);
        kListTypeDepthMixing: currentToolParamListManager.depthMixingList.add(item);
        kListTypeToolArea: currentToolParamListManager.toolAreaList.add(item);
        else
          raise Exception.create('TToolParamListEditorForm.importTabbedClick: Error reading parameter list');
        end;
      end;
  finally
    closeFile(inputFile);
  end;
  end;

procedure TToolParamListEditorForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Tool_parameter_list_editor')
  end;

end.

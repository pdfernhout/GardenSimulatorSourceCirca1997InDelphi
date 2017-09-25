unit Ugroups;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugroups: The group manager manages groups. Each group has some number of group items.
A group item has the text ID of an aspect and some display information for it (unit,
bounds, derivation, layer options). There are several class functions at the end
of the file that are used for accessing and displaying information in common to all
group items. Since the groups can be input and output in tab-delimited form, the options
stored here can be changed by the user (as opposed to the aspects, which can only be
changed in the source code).}

interface

uses UFiler, classes, ucollect, UAspects, StdCtrls;

const
  kArrayShowOneIndex = 0;
  kArrayShowMin = 1;
  kArrayShowMax = 2;
  kArrayShowMean = 3;
  kArrayShowMeanWeightedBySoilLayerThickness = 4;
  kArrayShowMeanWeightedBySoilLayerWeight = 5;
  kArrayShowSum = 6;
  kArrayShowAllValues = 7;
  kMaxArraySize = 10;

const
  kNotDerived = 0;
  kDerived1 = 1;
  kDerived2 = 2;
  kDerived3 = 3;

type selectionArray = array[0..kMaxArraySize - 1] of boolean;

type

GsGroupItem = class(GsStreamableObject)
  public
  fieldID: string[80];
  boundSoftLower: single;
  boundSoftUpper: single;
  currentUnit: smallint;
  derivedIndex: smallint;
  { these two only apply if the item is by soil layer }
  isLayerArray: boolean;
  arrayShowType: smallint;
  arraySelected: selectionArray;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
	constructor createWithInfo(aFieldID: string; aLowBound, aHighBound: single; aUnitIndex, aDerivedIndex: smallint);
  procedure setUnit(aUnit: smallint);
  function displayName: string;
  { class functions for dealing with layers }
  class function firstArrayIndexSelected(arraySelected: selectionArray): smallint;
  class function numberOfArrayIndexesSelected(arraySelected: selectionArray): smallint;
  class function valueForLayerArray(modelProxy: GsStreamableObject; arrayShowType, fieldNumber: smallint;
      arraySelected: selectionArray; deriveMethod: smallint): single;
  class function arrayShowTypeName(arrayShowType: smallint; arraySelected: selectionArray): string;
  class function arrayShowTypeLongName(arrayShowType: smallint): string;
  class function layersListString(arraySelected: selectionArray): string;
  class function nameForGroupItemInfo(aspectName: string; isLayerArray: boolean;
      arrayShowType: smallint; arraySelected: selectionArray; deriveMethod, unitSet: smallint): string;
  class function deriveMethodName(deriveMethod: smallint): string;
  class function deriveMethodDescription(deriveMethod: smallint): string;
  end;

GsGroup = class(GsStreamableObject)
  public
  groupItems: TListCollection;
  name: string[80];
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
	constructor create;  override;
  destructor destroy; override;
	function aspectForIndex(index: longint): GsAspect;
  function groupItemForIndex(index: longint): GsGroupItem;
  function addAspect(aspect: GsAspect): GsGroupItem;
  function newGroupItem: GsGroupItem;
  end;

GsGroupManager = class(GsStreamableObject)
 	public
  groups: TListCollection;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
	constructor create;  override;
  destructor destroy; override;
	function addGroup(name: string): integer;
	procedure deleteGroup(groupIndex: longint);
	function groupForIndex(groupIndex: longint): GsGroup;
  procedure loadGroupsIntoComboBox(groupBox: TComboBox);
  function newGroup: GsGroup;
  procedure setAllGroupItemsToDefaultUnitsForUnitSystem(showMetricUnits: boolean);
  end;

implementation

uses SysUtils, udomain, udebug, uclasses, usupport, umodel, uunits;

{GsGroupItem}
constructor GsGroupItem.createWithInfo(aFieldID: string; aLowBound, aHighBound: single;
    aUnitIndex, aDerivedIndex: smallint);
  var i: smallint;
  begin
  self.create;
  fieldID := copy(aFieldID, 1, 80);
  boundSoftLower := aLowBound;
  boundSoftUpper := aHighBound;
  currentUnit := aUnitIndex;
  isLayerArray := false;
  derivedIndex := aDerivedIndex;
  { default group item to show all layers selected (not used if not layer) }
  arrayShowType := kArrayShowAllValues;
  for i := 0 to kMaxArraySize - 1 do arraySelected[i] := true;
  end;

procedure GsGroupItem.setUnit(aUnit: smallint);
  begin
  currentUnit := aUnit;
  groupsFileMightHaveChanged := true;
  end;

function GsGroupItem.displayName: string;
  var
    aspect: GsAspect;
  begin
  try
    aspect := Domain.aspectManager.aspectForFieldID(fieldID);
    result := GsGroupItem.nameForGroupItemInfo(aspect.aspectName, isLayerArray, arrayShowType, arraySelected,
      aspect.deriveMethod(derivedIndex), aspect.unitSet(derivedIndex))
  except
      result := '*** Aspect undefined for ' + fieldID;
  end;
  end;

procedure GsGroupItem.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsGroupItem;
  cvir.versionNumber := 1;
  cvir.additionNumber := 2;
  end;

procedure GsGroupItem.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    i: smallint;
    aspect: GsAspect;
  begin
  filer.streamShortString(fieldID);
  filer.streamSingle(boundSoftLower);
  filer.streamSingle(boundSoftUpper);
  filer.streamSmallint(currentUnit);
  if cvir.additionNumber >= 2 then
    filer.streamSmallint(derivedIndex);
  if cvir.additionNumber >= 1 then
    filer.streamSmallint(arrayShowType)
  else
    if filer.isReading then arrayShowType := kArrayShowAllValues;
  if cvir.additionNumber >= 1 then
    filer.streamBytes(arraySelected, sizeOf(arraySelected))
  else
    if filer.isReading then for i := 0 to kMaxArraySize - 1 do arraySelected[i] := true;
  if cvir.additionNumber >= 1 then
    filer.streamBoolean(isLayerArray)
  else
    if filer.isReading then
      begin
      aspect := Domain.aspectManager.aspectForFieldID(fieldID);
      if aspect = nil then
        isLayerArray := false
      else
        isLayerArray := (aspect.indexCount > 0) and (aspect.indexType = kIndexTypeLayer);
      end;
  end;

{GsGroup}
constructor GsGroup.create;
	begin
  inherited create;
  groupItems := TListCollection.create;
  end;

destructor GsGroup.destroy;
	begin
  groupItems.free;
  inherited destroy;
  end;

procedure GsGroup.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsGroup;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsGroup.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  filer.streamShortString(name);
  groupItems.streamUsingFiler(filer, GsGroupItem);
  end;

function GsGroup.aspectForIndex(index: longint): GsAspect;
	begin
  result := Domain.aspectManager.aspectForFieldID(GsGroupItem(groupItems.items[index]).fieldID);
  end;

function GsGroup.groupItemForIndex(index: longint): GsGroupItem;
  begin
  result := nil;
  if groupItems.count > 0 then
    begin
    if index < groupItems.count then
      result := GsGroupItem(groupItems.items[index])
    else
      raise Exception.create('Group item index ' + intToStr(index) + ' out of bounds');
    end;
  end;

function GsGroup.newGroupItem: GsGroupItem;
  var
    theGroupItem: GsGroupItem;
  begin
  theGroupItem := GsGroupItem.create;
  groupItems.add(theGroupItem);
  result := theGroupItem;
  end;

const
  kDefaultSoftMin: single = 0.0;
  kDefaultSoftMax: single = 100.0;

function GsGroup.addAspect(aspect: GsAspect): GsGroupItem;
  var
    tryValue, softMin, softMax: single;
    currentUnitIndex, derivedIndex: smallint;
    groupItem: GsGroupItem;
  begin
  result := nil;
  if aspect = nil then
    raise exception.create(' nil aspect ');
  { if aspect has no derivations, derivedIndex is 0; if aspect has derivations, choose first by default }
  if aspect.canBeDerived then
    derivedIndex := 1
  else
    derivedIndex := 0;
  if aspect.fieldType = kFieldFloat then
    begin
    softMin := aspect.boundSoftLower(derivedIndex);
    softMax := aspect.boundSoftUpper(derivedIndex);
    if Domain.menuOptions.showMetricUnits then
      currentUnitIndex := aspect.unitDefaultMetric(derivedIndex)
    else
      currentUnitIndex := aspect.unitDefaultEnglish(derivedIndex);
    groupItem := GsGroupItem.createWithInfo(aspect.fieldID, softMin, softMax, currentUnitIndex, derivedIndex);
    groupItem.isLayerArray := ((aspect.indexCount > 0) and (aspect.indexType = kIndexTypeLayer));
    end
  else  { not float }
    groupItem := GsGroupItem.createWithInfo(aspect.fieldID, 0.0, 0.0, 0, 0);
  groupItems.add(groupItem);
  result := groupItem;
  end;

{GsGroupManager}
constructor GsGroupManager.create;
	begin
  inherited create;
  groups := TListCollection.create;
  end;

destructor GsGroupManager.destroy;
	begin
  groups.free;
  inherited destroy;
  end;

procedure GsGroupManager.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsGroupManager;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsGroupManager.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  groups.streamUsingFiler(filer, GsGroup);
  end;

function GsGroupManager.addGroup(name: string): integer;
	var
		group: GsGroup;
	begin
  group := GsGroup.create;
  group.name := name;
  result := groups.add(group)
  end;

function GsGroupManager.newGroup: GsGroup;
  var
    theGroup: GsGroup;
  begin
  theGroup := GsGroup.create;
  groups.add(theGroup);
  result := theGroup;
  end;

procedure GsGroupManager.deleteGroup(groupIndex: longint);
	begin
  groups.delete(groupIndex);
  end;

function GsGroupManager.groupForIndex(groupIndex: longint): GsGroup;
	begin
  result := GsGroup(groups.items[groupIndex]);
  end;

procedure GsGroupManager.loadGroupsIntoComboBox(groupBox: TComboBox);
  var
	  aGroup: GsGroup;
    i: integer;
  begin
  groupBox.items.clear;
  if self.groups.count > 0 then
	  begin
    for i := 0 to self.groups.count - 1 do
      begin
      aGroup := self.groupForIndex(i);
      groupBox.items.addObject(aGroup.name, aGroup);
      end;
	  groupBox.itemIndex := 0;
    end
  else
	  groupBox.itemIndex := -1;
  end;

procedure GsGroupManager.setAllGroupItemsToDefaultUnitsForUnitSystem(showMetricUnits: boolean);
  var
	  group: GsGroup;
    groupItem: GsGroupItem;
    aspect: GsAspect;
    i, j: integer;
  begin
  if groups.count > 0 then
    for i := 0 to self.groups.count - 1 do
      begin
      group := self.groupForIndex(i);
      if group = nil then
        raise Exception.create('GsGroupManager.setAllGroupItemsToDefaultUnitsForUnitSystem: nil group');
      if group.groupItems.count > 0 then
        for j := 0 to group.groupItems.count - 1 do
          begin
          groupItem := group.groupItemForIndex(j);
          if groupItem = nil then
            raise Exception.create('GsGroupManager.setAllGroupItemsToDefaultUnitsForSystem: nil group item');
          aspect := Domain.aspectManager.aspectForFieldID(groupItem.fieldID);
          if aspect = nil then continue;
          if showMetricUnits then
            groupItem.setUnit(aspect.unitDefaultMetric(groupItem.derivedIndex))
          else
            groupItem.setUnit(aspect.unitDefaultEnglish(groupItem.derivedIndex));
          end;
      end;
  end;

{ group item class functions for array management }
class function GsGroupItem.firstArrayIndexSelected(arraySelected: selectionArray): smallint;
  var i: integer;
  begin
  result := 0;
  for i := 0 to kMaxArraySize - 1 do
    if arraySelected[i] then
      begin
      result := i;
      break;
      end;
  end;

class function GsGroupItem.numberOfArrayIndexesSelected(arraySelected: selectionArray): smallint;
  var i: integer;
  begin
  result := 0;
  for i := 0 to kMaxArraySize - 1 do if arraySelected[i] then inc(result);
  end;

class function GsGroupItem.arrayShowTypeName(arrayShowType: smallint; arraySelected: selectionArray): string;
  begin
  result := '';
  case arrayShowType of
    kArrayShowOneIndex: result := 'layer ' + intToStr(GsGroupItem.firstArrayIndexSelected(arraySelected) + 1);
    kArrayShowMin: result := 'min';
    kArrayShowMax: result := 'max';
    kArrayShowMean: result := 'avg';
    kArrayShowMeanWeightedBySoilLayerThickness: result := 'avg-thickness';
    kArrayShowMeanWeightedBySoilLayerWeight: result := 'avg-mass';
    kArrayShowSum: result := 'sum';
    kArrayShowAllValues: result := 'all layers';
    end;
  end;

class function GsGroupItem.arrayShowTypeLongName(arrayShowType: smallint): string;
  begin
  result := '';
  case arrayShowType of
    kArrayShowOneIndex: result := 'one layer value only';
    kArrayShowMin: result := 'minimum';
    kArrayShowMax: result := 'maximum';
    kArrayShowMean: result := 'average (not weighted)';
    kArrayShowMeanWeightedBySoilLayerThickness: result := 'average weighted by layer thickness';
    kArrayShowMeanWeightedBySoilLayerWeight: result := 'average weighted by layer mass';
    kArrayShowSum: result := 'total';
    kArrayShowAllValues: result := 'all layers together';
    end;
  end;

class function GsGroupItem.layersListString(arraySelected: selectionArray): string;
  var
    i, numSelected: smallint;
    firstDone: boolean;
  begin
  result := '';
  numSelected := GsGroupItem.numberOfArrayIndexesSelected(arraySelected);
  if numSelected = kMaxArraySize then
    result := result + 'all layers'
  else
    begin
    if numSelected = 1 then
      result := result + 'layer '
    else
      result := result + 'layers ';
    firstDone := false;
    for i := 0 to kMaxArraySize - 1 do
      if arraySelected[i] then
        begin
        if firstDone then
          result := result + ', '
        else
          firstDone := true;
        result := result + intToStr(i+1);
        end;
    end;
  end;

class function GsGroupItem.deriveMethodName(deriveMethod: smallint): string;
  begin
  result := '';
  case deriveMethod of
    kDeriveTypeUndefined: result := '';
    kDeriveTypeDepth: result := 'depth fraction';
    kDeriveTypeConcentration: result := 'conc. by weight';
    kDeriveTypeArea: result := 'absolute';
    kDeriveTypeConcentrationFromPercent: result := 'conc. from percent by weight';
    else
      raise Exception.create('GsGroupItem.deriveMethodName: bad index');
    end;
  end;

class function GsGroupItem.deriveMethodDescription(deriveMethod: smallint): string;
  begin
  result := '';
  case deriveMethod of
    kDeriveTypeUndefined: result := '';
    kDeriveTypeDepth: result := 'To derive a depth fraction,'
      + ' depth of liquid (mm) is divided by the thickness of the soil layer or of all the soil layers (mm),'
      + ' giving a fraction (m/m). The fraction of the soil volume taken up by rocks is ignored in the'
      + ' calculation.';
    kDeriveTypeConcentration: result := 'To derive a concentration by weight,'
      + ' relative mass (kg/ha) is divided by the relative mass of the soil layer or of all the soil layers (kg/ha)'
      + ' giving a fraction (kg/kg) which is converted to g/kg.';
    kDeriveTypeArea: result := 'To derive an absolute quantity,'
      + ' relative mass (kg/ha) is divided by the area of the soil patch (ha)'
      + ' giving an absolute mass (kg).';
    kDeriveTypeConcentrationFromPercent: result := 'To derive a concentration from percent by weight,'
      + ' percent by weight is converted to a fraction (kg/kg) which is converted to g/kg.';
    else
      raise Exception.create('GsGroupItem.deriveMethodName: bad index');
    end;
  end;

class function GsGroupItem.nameForGroupItemInfo(aspectName: string; isLayerArray: boolean;
    arrayShowType: smallint; arraySelected: selectionArray; deriveMethod, unitSet: smallint): string;
  var
    aspect: GsAspect;
  begin
  result := '';
  result := removeUnitSuffix(aspectName);
  result := trimLeftAndRight(result);
  if deriveMethod = kDeriveTypeUndefined then
    begin
    { for masses relative to area, label non-derived }
    if unitSet = kMassOverArea then
      result := result + ', relative';
    { for percent, label non-derived (this is to make bag contents clear) }
    if unitSet = kNonChangingUnitPercent then
      result := result + ', percent';
    end
  else
    begin
    result := result + ', ' + GsGroupItem.deriveMethodName(deriveMethod);
    end;
  if isLayerArray then
    begin
    result := result + ' [' + GsGroupItem.arrayShowTypeName(arrayShowType, arraySelected);
    if (arrayShowType <> kArrayShowAllValues) and (arrayShowType <> kArrayShowOneIndex) then
      result := result + ': ' + GsGroupItem.layersListString(arraySelected);
    result := result + ']';
    end;
  end;

class function GsGroupItem.valueForLayerArray(modelProxy: GsStreamableObject; arrayShowType, fieldNumber: smallint;
    arraySelected: selectionArray; deriveMethod: smallint): single;
  var
    i, arraySelectedIndex, numArrayIndexesSelected: smallint;
  	valueSingle, minimum, maximum, total: single;
    minimumSet, maximumSet: boolean;
    model: GsModel;
  begin
  result := 0.0;
  model := GsModel(modelProxy);
  numArrayIndexesSelected := GsGroupItem.numberOfArrayIndexesSelected(arraySelected);
  case arrayShowType of
    kArrayShowOneIndex:
      begin
      arraySelectedIndex := GsGroupItem.firstArrayIndexSelected(arraySelected);
      model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, arraySelectedIndex, deriveMethod, nil);
      result := valueSingle;
      end;
    kArrayShowMin:
      begin
      minimumSet := false;
      minimum := 0.0;
      for i := 0 to kMaxArraySize - 1 do if arraySelected[i] then
        begin
        model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, i, deriveMethod, nil);
        if not minimumSet then
          begin
          minimum := valueSingle;
          minimumSet := true;
          end
        else if valueSingle < minimum then
          minimum := valueSingle;
        end;
      result := minimum;
      end;
    kArrayShowMax:
      begin
      maximumSet := false;
      maximum := 0.0;
      for i := 0 to kMaxArraySize - 1 do if arraySelected[i] then
        begin
        model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, i, deriveMethod, nil);
        if not maximumSet then
          begin
          maximum := valueSingle;
          maximumSet := true;
          end
        else if valueSingle > maximum then
          maximum := valueSingle;
        end;
      result := maximum;
      end;
    kArrayShowMean:
      begin
      total := 0.0;
      for i := 0 to kMaxArraySize - 1 do if arraySelected[i] then
        begin
        model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, i, deriveMethod, nil);
        total := total + valueSingle;
        end;
      if (numArrayIndexesSelected > 0) then
        result := total / numArrayIndexesSelected
      else
        result := valueSingle;
      end;
    kArrayShowSum:
      begin
      total := 0.0;
      for i := 0 to kMaxArraySize - 1 do if arraySelected[i] then
        begin
        model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, i, deriveMethod, nil);
        total := total + valueSingle;
        end;
      result := total;
      end;
    end;
  end;

end.

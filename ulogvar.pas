unit Ulogvar;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ulogvar: A logged variable, or an object to hold information about a simulation aspect
for graphing. The 'loggedVar' as we call it has its data in a long array of singles
in memory, which is expanded and contracted as needed. A loggedVar can only hold
singles (not integers). The loggedVar keeps track of many things about how to
display itself, including which axis it is on (left or right - see ugrafcom),
if it is an array and if so how to display it, its model in the domain (to get the data),
its aspect (to get the name, hint, etc), information taken from a group item for it
(doesn't have a group item pointer because of possible changes by the group editor),
information on how to draw the line on the graph (which is edited in the graph window),
and the start and end dates of its data. Since data is linked to simulation dates,
the data is looked up that way.}

interface

uses WinTypes, WinProcs, Graphics, ufiler, umodel, uaspects, ugroups, udate;

const
  kLeftAxis = 0;
  kRightAxis = 1;

  kShowLayerInfo = true;
  kDontShowLayerInfo = false;

type
  BigSingleArray = array [0..16000] of single;
  PBigSingleArray = ^BigSingleArray;

GsLoggedVar = class(GsStreamableObject)
  public
  loggedVarType: smallint;
  model: GsModel;
  aspect: GsAspect;
  currentUnit: smallint;
  groupItemBoundSoftLower: single;
  groupItemBoundSoftUpper: single;
  groupItemDerivedIndex: smallint;
  arraySize: smallint;
  isLayerArray: boolean;
  arraySelected: selectionArray;
  arrayShowType: smallint;
  scaleBy: single;
  axisToPlotOn: smallint;
  plotFlag: boolean;
  drawBars: boolean;
  lineStyle: TPenStyle;
  lineColor: TColorRef;
  lineWidth: smallint;
  dataPointsArrayPtr: PBigSingleArray;
  numDataPointsAllocated: longint;
  numDataPointsUsed: longint;
  numDataPointsLost: longint;
  firstAvailableDataPointArrayIndex: longint;
  creationDate: GsDate;
  hasValidPointers: boolean;
  constructor create; override;
  constructor createWithInfo(aModel: GsModel; anAspect: GsAspect;
    aGroupItem: GsGroupItem; aCreationDate: GsDate; daysToLog: longint);
  destructor destroy; override;
  procedure reset(dateToday: GsDate);
  procedure changeDaysToLog(daysToLog: longint; dateToday: GsDate);
  procedure addTolog;
  function dataForIndexFromCreation(index: longint; var value: single): boolean;
  function dataForIndexFromCreationUnscaled(index: longint; var value: single): boolean;
  function dataForDate(date: GsDate; var value: single): boolean;
  function toCurrentUnit(value: single): single;
  function axisString: string;
  function unitString: string;
  function scaleString: string;
  function modelString: string;
  function listString: string;
  procedure switchAxisToPlotOn;
  procedure switchToNextUnitInSet(shift, ctrl: boolean);
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure copyOptionsTo(copy: GsLoggedVar);
  function hasEarlierStartDateThan(aDate: GsDate): boolean;
	function computeStartDate: GsDate;
  function isAnyDataAvailable: boolean;
  function firstAvailableDataPointIndex: longint;
  function lastAvailableDataPointIndex: longint;
	function dataPointIndexFromCreationForDate(date: GsDate): longint;
  function displayName(includeLayerStuff: boolean): string;
  procedure selectDerivationMethod;
  end;

implementation

uses SysUtils, Forms, Controls, uesoil, ueplant, ueutils, uunits, usupport, uexcept, uclasses, udomain, uderopt;

constructor GsLoggedVar.create;
  begin
  inherited create;
  hasValidPointers := true;
  end;

constructor GsLoggedVar.createWithInfo(aModel: GsModel; anAspect: GsAspect;
  aGroupItem: GsGroupItem; aCreationDate: GsDate; daysToLog: longint);
  var i: integer;
	begin
  self.create;
  model := aModel;
  aspect := anAspect;
  currentUnit := aGroupItem.currentUnit;
  groupItemBoundSoftLower := aGroupItem.boundSoftLower;
  groupItemBoundSoftUpper := aGroupItem.boundSoftUpper;
  groupItemDerivedIndex := aGroupItem.derivedIndex;
  creationDate := aCreationDate;
  numDataPointsAllocated := 0;
  numDataPointsUsed := 0;
  numDataPointsLost := 0;
  firstAvailableDataPointArrayIndex := 0;
  axisToPlotOn := kLeftAxis; { default to left side }
  { CFK FIX - sometime in future, figure scale using groupItem bounds and current scale bounds,
    but give user choice whether this is auto or not }
  scaleBy := 1.0;
  arraySize := aspect.indexCount;
  if arraySize = 0 then arraySize := 1;
  for i := 0 to kMaxArraySize - 1 do arraySelected[i] := false; { set all }
  arrayShowType := kArrayShowOneIndex;
  if (arraySize > 1) and (aspect.indexType = kIndexTypeLayer) then
    begin
    isLayerArray := true;
    if aGroupItem.arrayShowType = kArrayShowAllValues then
      arrayShowType := kArrayShowMeanWeightedBySoilLayerThickness
    else
      arrayShowType := aGroupItem.arrayShowType;
    arraySelected := aGroupItem.arraySelected;
    end
  else
    isLayerArray := false;
  dataPointsArrayPtr := nil;
  GetMem(dataPointsArrayPtr, daysToLog * sizeOf(single));
  numDataPointsAllocated := daysToLog;
  plotFlag := true;
  lineStyle := psSolid;
  lineWidth := 1;
  lineColor := clNavy;
  drawBars := false;
  end;

destructor GsLoggedVar.destroy;
	begin
  if dataPointsArrayPtr <> nil then
  	FreeMem(dataPointsArrayPtr, numDataPointsAllocated * sizeOf(single));
  dataPointsArrayPtr := nil;
  numDataPointsAllocated := 0;
  numDataPointsUsed := 0;
  numDataPointsLost := 0;
  firstAvailableDataPointArrayIndex := 0;
  aspect := nil;
  model := nil;
  inherited destroy;
  end;

function GsLoggedVar.displayName(includeLayerStuff: boolean): string;
  begin
  result := '';
  if aspect = nil then exit;
{  if not hasValidPointers then exit; }
  if includeLayerStuff then
    result := GsGroupItem.nameForGroupItemInfo(aspect.aspectName, isLayerArray, arrayShowType, arraySelected,
      aspect.deriveMethod(groupItemDerivedIndex), aspect.unitSet(groupItemDerivedIndex))
  else
    result := GsGroupItem.nameForGroupItemInfo(aspect.aspectName, false, arrayShowType, arraySelected,
      aspect.deriveMethod(groupItemDerivedIndex), aspect.unitSet(groupItemDerivedIndex));
  end;

procedure GsLoggedVar.reset(dateToday: GsDate);
  begin
  creationDate := dateToday;
  numDataPointsUsed := 0;
  numDataPointsLost := 0;
  firstAvailableDataPointArrayIndex := 0;
  end;

procedure GsLoggedVar.changeDaysToLog(daysToLog: longint; dateToday: GsDate);
  var
    newDataArrayPtr: PBigSingleArray;
    pointsToCopy: longint;
    i: longint;
    dataValue: single;
	begin
  newDataArrayPtr := nil;
  GetMem(newDataArrayPtr, daysToLog * sizeOf(single));
  FailNilPtr(newDataArrayPtr);
  pointsToCopy := numDataPointsUsed;
  if pointsToCopy > daysToLog then pointsToCopy := daysToLog;
  {copy tail end of data}
  for i := 0 to pointsToCopy - 1 do
    begin
  	self.dataForIndexFromCreationUnscaled(numDataPointsUsed - 1 - i + numDataPointsLost, dataValue);
    newDataArrayPtr^[pointsToCopy - 1 - i] := dataValue;
    end;
  FreeMem(dataPointsArrayPtr, numDataPointsAllocated * sizeOf(single));
  dataPointsArrayPtr := newDataArrayPtr;
  numDataPointsAllocated := daysToLog;
  numDataPointsUsed := pointsToCopy;
  numDataPointsLost := 0;
  creationDate := dateToday;
  {backdate the creation date for data currently available}
  GsDate_addDays(creationDate, -numDataPointsUsed);
  firstAvailableDataPointArrayIndex := 0;
  end;

function GsLoggedVar.dataForIndexFromCreationUnscaled(index: longint; var value: single): boolean;
	var
    dataIndex: longint;
	begin
  result := false;
  if (numDataPointsLost <= index) and (index < numDataPointsLost + numDataPointsUsed) then
  	begin
    result := true;
    dataIndex := index - numDataPointsLost + firstAvailableDataPointArrayIndex;
    if dataIndex >= numDataPointsAllocated then
    	dataIndex := dataIndex - numDataPointsAllocated;
    value := dataPointsArrayPtr^[dataIndex];
    end
  else
    value := 0.0;
  end;

function GsLoggedVar.dataForIndexFromCreation(index: longint; var value: single): boolean;
	var
    dataIndex: longint;
	begin
  result := false;
  if (numDataPointsLost <= index) and (index < numDataPointsLost + numDataPointsUsed) then
  	begin
    result := true;
    dataIndex := index - numDataPointsLost + firstAvailableDataPointArrayIndex;
    if dataIndex >= numDataPointsAllocated then
    	dataIndex := dataIndex - numDataPointsAllocated;
    value := dataPointsArrayPtr^[dataIndex];
    try
      begin
      value := toCurrentUnit(value);
      value := value * scaleBy;
      end;
    except
      begin
      result := false;
      value := 0.0;
      end;
    end;
    end
  else
    value := 0.0;
  end;

function GsLoggedVar.dataForDate(date: GsDate; var value: single): boolean;
  var
    daysFromStart: longint;
    inRange: boolean;
  begin
  daysFromStart := GsDate_daysBetween(creationDate, date);
  result := self.dataForIndexFromCreation(daysFromStart, value);
  end;

procedure GsLoggedVar.addTolog;
  var
    dataIndex: longint;
    soilPatch: GsSoilPatch;
  	valueInteger: integer;
    valueSingle: single;
    arraySelectedIndex, deriveMethod: smallint;
	begin
 { if not hasValidPointers then exit; }
  try
  {get data from model and store it}
  if model = nil then exit;
  if aspect = nil then exit;
  deriveMethod := aspect.deriveMethod(groupItemDerivedIndex);
  { compute place to put point in wrapping data buffer }
  if numDataPointsUsed >= numDataPointsAllocated then
  	begin
    { lose a data point }
    inc(firstAvailableDataPointArrayIndex);
    dec(numDataPointsUsed);
    inc(numDataPointsLost);
    if firstAvailableDataPointArrayIndex >= numDataPointsAllocated then
    	firstAvailableDataPointArrayIndex := 0;
    end;
  {compute where to put next point and handle possible wrapping}
  dataIndex := firstAvailableDataPointArrayIndex + numDataPointsUsed;
  if dataIndex >= numDataPointsAllocated then
  	dataIndex := dataIndex - numDataPointsAllocated;
  inc(numDataPointsUsed);
  case aspect.fieldType of
    kFieldFloat:
  	  begin
      if (arraySize = 1) or (aspect.indexType <> kIndexTypeLayer) then
        begin
        model.TransferField(kGetField, valueSingle, aspect.fieldNumber, kFieldFloat, 0, deriveMethod, nil);
        dataPointsArrayPtr^[dataIndex] := valueSingle;
        end
      else
        begin
        if (arrayShowType = kArrayShowMeanWeightedBySoilLayerThickness)
          or (arrayShowType = kArrayShowMeanWeightedBySoilLayerWeight) then
          begin
          soilPatch := nil;
          { dependent on soil patch }
          case aspect.objectType of
            kObjectTypeSoil: soilPatch := GsSoilPatch(model);
            kObjectTypePlant, kObjectTypeDrawingPlant: soilPatch := GsPlant(model).soil;
            end;
          if soilPatch <> nil then
            dataPointsArrayPtr^[dataIndex] := soilPatch.valueForLayerArray(model, arrayShowType,
                aspect.fieldNumber, arraySelected, deriveMethod)
          else
            dataPointsArrayPtr^[dataIndex] := 0.0;
          end
        else
          { not dependent on soil patch }
          dataPointsArrayPtr^[dataIndex] := GsGroupItem.valueForLayerArray(model, arrayShowType,
              aspect.fieldNumber, arraySelected, deriveMethod);
        end;
      end;
    kFieldInt:  { assuming no arrays of ints }
  	  begin
      arraySelectedIndex := 0;
  	  model.TransferField(kGetField, valueInteger, aspect.fieldNumber, kFieldInt, arraySelectedIndex, deriveMethod, nil);
  	  dataPointsArrayPtr^[dataIndex] := valueInteger;
      end;
    else
      begin
      errorMessage('Unknown field type for field number ' + intToStr(aspect.fieldNumber));
  	  dataPointsArrayPtr^[dataIndex] := 0.0;
      end;
    end;
  except
    errorMessage('Problem in GsLoggedVar.addTolog');
  end;
  end;

function GsLoggedVar.toCurrentUnit(value: single): single;
  begin
  result := 0.0;
 { if not hasValidPointers then exit; }
  result := Convert(aspect.unitSet(groupItemDerivedIndex), aspect.unitModel(groupItemDerivedIndex),
    self.currentUnit, value);
  end;

function GsLoggedVar.axisString: string;
  begin
  if self.axisToPlotOn = kLeftAxis then
    result := 'L'
  else
    result := 'R';
  end;

function GsLoggedVar.unitString: string;
  begin
  result := '';
 { if not hasValidPointers then exit; }
  if (aspect = nil) then exit;
  result := UnitStringForEnum(aspect.unitSet(groupItemDerivedIndex), self.currentUnit);
  end;

function GsLoggedVar.scaleString: string;
  begin
  result := digitValueString(scaleBy);
  end;

function GsLoggedVar.modelString: string;
  begin
  result := '';
  if model = nil then exit;
 { if not hasValidPointers then exit; }
    result := model.getName;
  end;

function GsLoggedVar.listString: string;
  begin
 { if not hasValidPointers then exit; }
  result := '';
  if aspect = nil then exit;
  if isLayerArray then
    begin
    result := GsGroupItem.arrayShowTypeName(arrayShowType, arraySelected);
    if (arrayShowType <> kArrayShowAllValues) and (arrayShowType <> kArrayShowOneIndex) then
      result := result + ': ' + GsGroupItem.layersListString(arraySelected);
    end
  else
    result := '---'
  end;

procedure GsLoggedVar.switchAxisToPlotOn;
  begin
  if self.axisToPlotOn = kLeftAxis then
    self.axisToPlotOn := kRightAxis
  else
    self.axisToPlotOn := kLeftAxis;
  end;

procedure GsLoggedVar.switchToNextUnitInSet(shift, ctrl: boolean);
  begin
 { if not hasValidPointers then exit; }
  if ctrl then
    self.selectDerivationMethod
  else if shift then
    self.currentUnit := GetPreviousUnitEnumInUnitSet(aspect.unitSet(groupItemDerivedIndex), self.currentUnit)
  else
    self.currentUnit := GetNextUnitEnumInUnitSet(aspect.unitSet(groupItemDerivedIndex), self.currentUnit);
  end;

procedure GsLoggedVar.selectDerivationMethod;
  var
    derivationForm: TAspectDerivationForm;
    response: longint;
	begin
  if not aspect.canBeDerived then exit;
  derivationForm := TAspectDerivationForm.create(Application);
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
        groupItemDerivedIndex);
    response := derivationForm.showModal;
    if response = mrOK then
      groupItemDerivedIndex := derivationForm.deriveMethod;
  finally
    derivationForm.free;
    derivationForm := nil;
  end;
  end;

procedure GsLoggedVar.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsLoggedVar;
  cvir.versionNumber := 0;
  cvir.additionNumber := 1;
  end;

var
  globalAspectFieldID, globalModelName: string;

procedure GsLoggedVar.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    existingNumDataPointsAllocated: longint;
    i: smallint;
  begin
  if filer.isReading then hasValidPointers := false;
  existingNumDataPointsAllocated := numDataPointsAllocated;
  filer.streamSmallint(loggedVarType);
  filer.streamSmallint(currentUnit);
  filer.streamSingle(groupItemBoundSoftLower);
  filer.streamSingle(groupItemBoundSoftUpper);
  if cvir.additionNumber >= 1 then
    filer.streamSmallint(groupItemDerivedIndex);
  filer.streamSmallint(arraySize);
  filer.streamBoolean(isLayerArray);
  for i := 0 to kMaxArraySize - 1 do filer.streamBoolean(arraySelected[i]);
  filer.streamSmallint(arrayShowType);
  filer.streamSingle(scaleBy);
  filer.streamSmallint(axisToPlotOn);
  filer.streamBoolean(plotFlag);
  filer.streamBoolean(drawBars);
  filer.streamPenStyle(lineStyle);
  filer.streamColorRef(lineColor);
  filer.streamSmallint(lineWidth);
  filer.streamLongint(numDataPointsAllocated);
  filer.streamLongint(numDataPointsUsed);
  filer.streamLongint(numDataPointsLost);
  filer.streamLongint(firstAvailableDataPointArrayIndex);
  filer.streamDate(creationDate);
  { stream aspect fieldID, when reading use fieldID to get pointer }
  if filer.isReading then
    begin
    filer.streamShortString(globalAspectFieldID);
    aspect := nil;
    aspect := Domain.aspectManager.aspectForFieldID(globalAspectFieldID);
    if aspect = nil then exit;
    end
  else
    filer.streamShortString(aspect.fieldID);
  { stream model name, when reading use name to get pointer}
  if filer.isReading then
    begin
    filer.streamShortString(globalModelName);
    model := nil;
    model := Domain.garden.modelForName(globalModelName);
    if model = nil then exit;
    end
  else
    begin
    globalModelName := model.getName;
    filer.streamShortString(globalModelName);
    end;
  { stream data }
  if filer.isReading then
    begin
    if dataPointsArrayPtr <> nil then
  	  FreeMem(dataPointsArrayPtr, existingNumDataPointsAllocated * sizeOf(single));
    dataPointsArrayPtr := nil;
    try
      GetMem(dataPointsArrayPtr, numDataPointsAllocated * sizeOf(single));
      FailNilPtr(dataPointsArrayPtr);
    except
      numDataPointsAllocated := 0;
      raise;
    end;
    end;
  filer.streamBytes(dataPointsArrayPtr^, numDataPointsAllocated * sizeOf(single));
  hasValidPointers := true;
  end;

procedure GsLoggedVar.copyOptionsTo(copy: GsLoggedVar);
  begin
 { if not hasValidPointers then exit; }
  copy.model := self.model;
  copy.currentUnit := self.currentUnit;
  copy.scaleBy := self.scaleBy;
  copy.axisToPlotOn := self.axisToPlotOn;
  copy.arraySize := self.arraySize;
  copy.arraySelected := self.arraySelected;
  copy.arrayShowType := self.arrayShowType;
  copy.drawBars := self.drawBars;
  copy.lineStyle := self.lineStyle;
  copy.lineColor := self.lineColor;
  copy.lineWidth := self.lineWidth;
  end;

function GsLoggedVar.hasEarlierStartDateThan(aDate: GsDate): boolean;
  begin
  result := GsDate_firstDateIsEarlierThanSecond(self.computeStartDate, aDate);
  end;

function GsLoggedVar.computeStartDate: GsDate;
  begin
  result := creationDate;
  GsDate_addDays(result, numDataPointsLost)
  end;

function GsLoggedVar.isAnyDataAvailable: boolean;
  begin
  result := numDataPointsUsed > 0;
  end;

function GsLoggedVar.firstAvailableDataPointIndex: longint;
  begin
  result := numDataPointsLost;
  end;

function GsLoggedVar.lastAvailableDataPointIndex: longint;
  begin
  result := numDataPointsLost + numDataPointsUsed - 1;
  end;

function GsLoggedVar.dataPointIndexFromCreationForDate(date: GsDate): longint;
  var
  	startDate: GsDate;
  begin
  result := GsDate_daysBetween(creationDate, date);
  end;

end.
 
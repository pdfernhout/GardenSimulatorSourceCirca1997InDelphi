unit Ugscom;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugscom: All the commands that can be undone. See ucommand for the base class of undoable
commands and the command list manager. Each mouse down/move/up in the garden window
can potentially create a command. Depending on the tool action (as defined in utools),
a command is created and put into the command list, where it can be done, undone,
and redone. In some of these subclasses redo is the same as do; if so, redoCommand is
not overridden and it calls doCommand. Each command must override at least trackMouse,
doCommand, undoCommand, and description. There are two subclasses that have several
related subclasses hanging off them: GsSoilOperationCommand (in which one soil
patch is modified, and a copy is kept of the original), and GsTwoSoilPatchOperationCommand
(in which two soil patches are modified at once, and copies are kept of both originals).
For many soil operations it is not really necessary to copy the whole soil patch, but
that is the simplest and safest way though it might be slower and more expensive in
memory terms. Several of the plant changing methods also keep a copy of a plant.
A few of these commands are not created from the garden window but from other windows
(such as resetHarvestListCommand), and some can be done both from the garden window
and from the other windows (reseedPlantCommand can be done from the browser). They
act in the same way as the mouse-created commands in the command list. }

interface

uses SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms,
			Dialogs, StdCtrls, ExtCtrls, ucollect, umodel,
      UeSoil, UePlant, UCommand, UAspects, udplant, uturt3d,
      uicon, uharvprt, ueorgmat, udate, utools, uebag, ulogvar, uestruct;

type
GsNewSoilPatchCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure redoCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsRemoveSoilPatchCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  constructor createCommand(aPatch: GsSoilPatch);
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsModifySoilPatchCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  cornerBeingDragged: smallint;
  oldPoint: TPoint;
  newPoint: TPoint;
  offset: TPoint;
  deletedPlantList: TList;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure redoCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsDragSoilPatchCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  offset: TPoint;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure redoCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsChangeAreaSoilPatchCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  oldInfo: SoilPatchBasicInfoStructure;
  newInfo: SoilPatchBasicInfoStructure;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsPlantSeedCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  plant: GsPlant;
  cultivar: GsPlant;
  paramObject: GsToolRateOrAmountParam;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
	procedure redoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsZapPlantCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  plant: GsPlant;
  addedOrganicMatter: GsOrganicMatter;
  constructor create;
  constructor createCommand(aPlant: GsPlant);
  destructor destroy; override;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
  procedure redoCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsZapAllPlantsCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  zappedPlantList: TList;
  addedOrganicMatterList: TList;
  constructor create;
  destructor destroy; override;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
  procedure redoCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsReseedPlantCommand = class(KfCommand)
	public
  plant: GsPlant;
  plantCopy: GsPlant;
  drawingPlant: GsDrawingPlant;
  drawingPlantCopy: GsDrawingPlant;
  soilPatch: GsSoilPatch;
  addedOrganicMatter: GsOrganicMatter;
  startGrowingFlag: boolean;
  constructor create;
  constructor createWithPlant(thePlant: GsPlant; aStartGrowingFlag: boolean);
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
	procedure redoCommand; override;
  function description: string; override;
  end;

GsGrowPlantCommand = class(KfCommand)
  public
  numDaysToGrow: smallint;
  plant: GsPlant;
  plantCopy: GsPlant;
  drawingPlant: GsDrawingPlant;
  drawingPlantCopy: GsDrawingPlant;
  soilPatch: GsSoilPatch;
  constructor createWithPlant(thePlant: GsPlant);
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
	procedure redoCommand; override;
  function description: string; override;
  procedure setDaysToGrowWithItemIndex(index: smallint);
  class procedure fillComboBoxItemsWithDaysToGrowChoices(comboBoxItems: TStrings);
	end;

GsTransplantCommand = class(KfCommand)
	public
  oldSoilPatch, newSoilPatch: GsSoilPatch;
  oldPoint, newPoint: TPoint;
  grabOffset: TPoint;
  plant: GsPlant;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  procedure updateTotalsAndBrowserForTransplant;
  function description: string; override;
  end;

GsRemoveGraphLoggedVarCommand = class(KfCommand)
	public
  loggedVar: GsLoggedVar;
  loggedVarIndex: longint;
  constructor createCommand(aLoggedVar: GsLoggedVar; anIndex: longint);
  destructor destroy; override;
  procedure doCommand; override;
  procedure undoCommand; override;
  function description: string; override;
  end;

GsRemovePlantPartsCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  plant: GsPlant;
  removedPlantPartsList: TList;
  addedHarvestItemList: TList;
  addedOrganicMatter: GsOrganicMatter;
  pullPlant: boolean;
  soilPatchColorChanges: boolean;
  constructor create;
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  procedure redoCommand; override;
  function description: string; override;
  procedure handleOrganicMatter;
  end;

GsHarvestPlantCommand = class(GsRemovePlantPartsCommand)
	public
  userWantsToPullPlant: boolean;
  bundledPlantPartsList: TList;
  plantIsKeptForReseeding: boolean;
  constructor create;
  destructor destroy; override;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  procedure redoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsPickUpToolCommand = class(KfCommand)
  public
  carriedToolObject: TObject;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	end;

GsMoveMulchCommand = class(KfCommand)
  public
  originalPoint: TPoint;
  movedToPoint: TPoint;
  carriedMulchObject: TObject;
  originalSoilPatchObject: TObject;
  movedToSoilPatchObject: TObject;
  originalSoilPatchColorChanges, movedToSoilPatchColorChanges: boolean;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure undoCommand; override;
	procedure redoCommand; override;
	procedure updateGardenAndBrowser;
  function description: string; override;
	end;

GsSetBrowserChoiceLeftCommand = class(KfCommand)
  public
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	end;

GsSetBrowserChoiceRightCommand = class(KfCommand)
  public
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	end;

GsSetBrowserChoiceBothCommand = class(KfCommand)
  public
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	end;

GsRenameCommand = class(KfCommand)
  public
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	end;

GsChangeModelNameCommand = class(KfCommand)
  public
  model: GsModel;
  oldValue: string;
 	newValue: string;
 	constructor createCommand(aModel: GsModel; aNewValue: string);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsDeleteTemplateCommand = class(KfCommand)
	public
  template: GsModel;
  constructor createCommand(aTemplate: GsModel);
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsScanCommand = class(KfCommand)
  public
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
  procedure updateScanAt(point: TPoint);
	end;

GsDuplicateCommand = class(KfCommand)
	public
  soilPatch: GsSoilPatch;
  soilPatchCopy: GsSoilPatch;
  plant: GsPlant;
  plantCopy: GsPlant;
  plantOffset: TPoint;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
  destructor destroy; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	procedure addPlantsInUseNotInGarden(inUseList: TList); override;
  end;

GsSoilOperationCommand = class(KfCommand)
  public
  soilPatch: GsSoilPatch;
  originalSoilPatchCopyWithoutPlants: GsSoilPatch;
  changedSoilPatchCopyWithoutPlants: GsSoilPatch;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure redoCommand; override;
	procedure undoCommand; override;
	procedure updateGardenAndBrowser;
  function description: string; override;
  destructor destroy; override;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); virtual;
	end;

GsCultivateCommand = class(GsSoilOperationCommand)
  public
  paramObject1, paramObject2: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsMixSoilCommand = class(GsSoilOperationCommand)
  public
  paramObject1, paramObject2: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsDoubleDigCommand = class(GsSoilOperationCommand)
  public
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsAerateCommand = class(GsSoilOperationCommand)
  public
  paramObject1, paramObject2: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsWaterCommand = class(GsSoilOperationCommand)
  public
  paramObject: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsApplyAmendmentCommand = class(GsSoilOperationCommand)
  public
  bag: GsBag;
  paramObject: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsTwoSoilPatchOperationCommand = class(GsSoilOperationCommand)
  public
  secondSoilPatch: GsSoilPatch;
  originalSecondSoilPatchCopyWithoutPlants: GsSoilPatch;
  changedSecondSoilPatchCopyWithoutPlants: GsSoilPatch;
	function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure redoCommand; override;
	procedure undoCommand; override;
	procedure updateGardenAndBrowser;
  function description: string; override;
  destructor destroy; override;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
	end;

GsCarrySoilCommand = class(GsTwoSoilPatchOperationCommand)
  public
  paramObject1, paramObject2: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsCarryMulchCommand = class(GsTwoSoilPatchOperationCommand)
  public
  paramObject: GsToolRateOrAmountParam;
  procedure doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean); override;
  function description: string; override;
	end;

GsSupportCommand = class(KfCommand)
  public
  plant: GsPlant;
  wasSupportedBeforeDoing: boolean;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	end;

GsStopOrStartPlantCommand = class(KfCommand)
  public
  plant: GsPlant;
  wasStoppedBeforeDoing: boolean;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
    mouseDidMove: boolean): KfCommand; override;
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
	end;

GsResetHarvestListCommand = class(KfCommand)
	public
  oldHarvestList: TListCollection;
  newHarvestList: TListCollection;
  saveLastReportResetDate: GsDate;
  constructor createCommand(aHarvestList: TListCollection);
	procedure doCommand; override;
	procedure undoCommand; override;
	procedure redoCommand; override;
  function description: string; override;
  destructor destroy; override;
  end;

{ domain changes - called from browser components }
GsChangeDomainCommand = class(KfCommand)
  public
  model: GsModel;
  fieldId: smallint;
  constructor createCommand(aModel: GsModel; aFieldID: smallint);
  procedure updatePlantBoundsIfNeeded;
  function description: string; override;
  end;

GsChangeDomainRealCommand = class(GsChangeDomainCommand)
	public
  oldValue: single;
 	newValue: single;
  index: smallint;
  deriveMethod: smallint;
  constructor createCommand(aModel: GsModel; aNewValue: single; aFieldID, aIndex, aDeriveMethod: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsChangeDomainSCurvePointCommand = class(GsChangeDomainCommand)
	public
  oldX: single;
  oldY: single;
 	newX: single;
  newY: single;
  pointIndex: smallint;
  constructor createCommand(aModel: GsModel; aNewX: single; aNewY: single; aFieldID, aPointIndex: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsChangeDomainColorCommand = class(GsChangeDomainCommand)
	public
  oldColor: TColorRef;
  newColor: TColorRef;
  constructor createCommand(aModel: GsModel; aNewColor: TColorRef; aFieldID: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsChangeDomainIconCommand = class(GsChangeDomainCommand)
	public
  oldIcon: GsIcon;
  newIcon: GsIcon;
  constructor createCommand(aModel: GsModel; aNewIcon: GsIcon; aFieldID: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  destructor destroy; override;
  end;

GsChangeDomainHarvestItemTemplateCommand = class(GsChangeDomainCommand)
	public
  oldHarvestItemTemplate: GsHarvestItemTemplate;
  newHarvestItemTemplate: GsHarvestItemTemplate;
  constructor createCommand(aModel: GsModel; aNewHarvestItemTemplate: GsHarvestItemTemplate; aFieldID: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  destructor destroy; override;
  end;

GsChangeDomainObject3DCommand = class(GsChangeDomainCommand)
	public
  oldTdo: KfObject3D;
  newTdo: KfObject3D;
  constructor createCommand(aModel: GsModel; aNewTdo: KfObject3D; aFieldID: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  destructor destroy; override;
  end;

GsChangeDomainSoilTextureCommand = class(GsChangeDomainCommand)
	public
  oldClay: single;
  oldSilt: single;
  oldSand: single;
 	newClay: single;
  newSilt: single;
  newSand: single;
  index: smallint;
  constructor createCommand(aModel: GsModel; aClay: single; aSilt: single; aSand: single;
    aFieldIndex: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsChangeDomainIntegerCommand = class(GsChangeDomainCommand)
	public
  oldValue: integer;
 	newValue: integer;
  index: smallint;
 	constructor createCommand(aModel: GsModel; aNewValue: integer; aFieldID: smallint; aIndex: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

GsChangeDomainStringCommand = class(GsChangeDomainCommand)
	public
  oldValue: string;
 	newValue: string;
  index: smallint;
 	constructor createCommand(aModel: GsModel; aNewValue: string; aFieldID: smallint; aIndex: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;       

GsChangeDomainBooleanCommand = class(GsChangeDomainCommand)
	public
  oldValue: boolean;
 	newValue: boolean;
  index: smallint;
 	constructor createCommand(aModel: GsModel; aNewValue: boolean; aFieldID: smallint; aIndex: smallint);
	procedure doCommand; override;
	procedure undoCommand; override;
  function description: string; override;
  end;

implementation

uses ugsim, udomain, ubrowser, umconsts, upatarea, udppart, uunits,
  uharvest, ueutils, uesoilop, ugraph, ueweath, usupport;

{ ------------------------------------------------------------ common functions }
const
  kNewSoilPatch = true;
  kNotNewSoilPatch = false;

  kMaxDescriptionLength = 60;

function limitDescription(description: string): string;
  begin
  result := description;
  if length(description) > kMaxDescriptionLength then
    result := copy(description, 1, kMaxDescriptionLength - 4) + ' ...';
  end;

function newSoilPatchNamePrefix: string;
  begin
  { assumes Domain and garden not nil }
  result := 's' + intToStr(Domain.garden.numSoilPatchesCreated) + ': ';
  end;

procedure giveNewSoilPatchUniqueNameBasedOn(soilPatch: GsSoilPatch; newName: string);
  var
    done: boolean;
    tries: smallint;
  begin
  tries := 0;
  soilPatch.setName(newSoilPatchNamePrefix + newName);
  done := not Domain.garden.soilPatchPrefixIsInUseByAnotherPatch(soilPatch);
  while not done do
    begin
    { use count to stop infinite loop from happening in the case of some unexpected problem }
    if tries < 100000 then
      begin
      inc(Domain.garden.numSoilPatchesCreated);
      soilPatch.setName(newSoilPatchNamePrefix + newName);
      done := not Domain.garden.soilPatchPrefixIsInUseByAnotherPatch(soilPatch);
      inc(tries);
      end
    else
      raise Exception.create('problem - too many soil patches to make unique name');
    end;
  end;

function newPlantNamePrefix: string;
  begin
  { assumes Domain and garden not nil }
  result := 'p' + intToStr(Domain.garden.numPlantsCreated) + ': ';
  end;

procedure giveNewPlantUniqueNameBasedOn(plant: GsPlant; newName: string);
  var
    done: boolean;
    tries: smallint;
  begin
  tries := 0;
  plant.setName(newPlantNamePrefix + newName);
  done := not Domain.garden.plantPrefixIsInUseByAnotherPlant(plant);
  while not done do
    begin
    { use tries count to stop infinite loop from happening in the case of some unexpected problem }
    if tries < 1000000 then
      begin
      inc(Domain.garden.numPlantsCreated);
      plant.setName(newPlantNamePrefix + newName);
      done := not Domain.garden.plantPrefixIsInUseByAnotherPlant(plant);
      inc(tries);
      end
    else
      raise Exception.create('problem - too many plants to make unique name');
    end;
  end;

{ ------------------------------------------------------------ GsNewSoilPatchCommand }
function GsNewSoilPatchCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
  	soilType: GsSoilPatch;
    newIndex: smallint;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := GsSoilPatch.createWithGardenAndPoint(Domain.garden, nextPoint);
    { PDF FIX - need to add something to template window to update default if delete soil type }
    try
      soilType := GsSoilPatch(Domain.templateManager.defaultSoilType);
    except
      soilType := nil;
    end;
    if soilType <> nil then
      begin
      soilPatch.copyFromSoilType(Domain.garden, soilType);
      soilPatch.isTemplate := false;
  		inc(Domain.garden.numSoilPatchesCreated);
      giveNewSoilPatchUniqueNameBasedOn(soilPatch, soilType.getName);
      end
    else
      begin
      showMessage('There is no default soil type.' + chr(13)
          + 'Before you can make a new soil patch,' + chr(13)
          + 'you must create or load a soil type template.');
      soilPatch.free;
      soilPatch := nil;
      result := nil;
      self.free;
      exit;
      end;
  	Domain.garden.addSoilPatch(soilPatch);
  	GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    GardenForm.modelAddedOrRemoved;
    end
  else if aTrackPhase = trackMove then
  	begin
  	GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  	soilPatch.resizeTo(nextPoint);
  	GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  	end
  else if aTrackPhase = trackRelease then
  	begin
    if soilPatch.enforceMinimumStartingSize then
      GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    newIndex := GardenForm.toolActionChoices.items.indexOf('shape soil patch');
    if newIndex <> -1 then
      begin
      GardenForm.currentTool.currentActionName := 'shape soil patch';
      GardenForm.toolActionChoices.itemIndex := newIndex;
  	  GardenForm.updateToolParamChoicesForCurrentToolAction;
      end;
  	end;
  end;

destructor GsNewSoilPatchCommand.destroy;
	begin
  if not done then
  	soilPatch.free;
  inherited destroy;
  end;

{need to request soil patch area}
procedure GsNewSoilPatchCommand.doCommand;
  var
    newInfo: SoilPatchBasicInfoStructure;
    selectedSoilType: GsSoilPatch;
  begin
  inherited doCommand;
  {trying this line - should not be needed for enforcing size}
  GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  if soilPatchAreaForm = nil then
    raise Exception.create('Could not create soil patch area window');
  try
    { could use more sophisticated algorithm to calculate area based on ratio of
    number of pixels to previous patch size times its size; if so, change area here }
    { otherwise, soil patch has default params set up when copying from the default soil type }
    soilPatch.fillBasicInfoStructure(newInfo);
    soilPatchAreaForm.initializeWithSoilPatchBasicInfo(newInfo, kNewSoilPatch, ''); 
    soilPatchAreaForm.showModal;
    { cancel button on soilPatchAreaForm is invisible in this case, so don't check modalResult }
    soilPatchAreaForm.fillBasicInfoStructure(newInfo);
    selectedSoilType := soilPatchAreaForm.selectedSoilType;
  finally
    soilPatchAreaForm.hide;
  end;
  { cancel button on soilPatchAreaForm is invisible in this case, so don't check modalResult }
  { if different soil type chosen than default, make soil patch that type }
  if (selectedSoilType <> nil)
    and (selectedSoilType <> GsSoilPatch(Domain.templateManager.defaultSoilType)) then
    begin
    soilPatch.copyFromSoilType(Domain.garden, selectedSoilType);
    soilPatch.isTemplate := false;
    giveNewSoilPatchUniqueNameBasedOn(soilPatch, selectedSoilType.getName);
    soilPatch.computeBoundsRect;
    { browser has to update new soil patch name }
    BrowserForm.updateObjectChoices;
    end;
  soilPatch.useBasicInfoStructure(newInfo);
  end;

procedure GsNewSoilPatchCommand.redoCommand;
  begin
  inherited doCommand;  {not inherited redo!}
  Domain.garden.addSoilPatch(soilPatch);
  GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  GardenForm.modelUndeleted(soilPatch);
  end;

procedure GsNewSoilPatchCommand.undoCommand;
  begin
  inherited undoCommand;
  Domain.garden.removeSoilPatch(soilPatch);
  GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  GardenForm.modelDeleted(soilPatch);
  end;

function GsNewSoilPatchCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'create soil patch'
  else
    result := 'create ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsRemoveSoilPatchCommand }
constructor GsRemoveSoilPatchCommand.createCommand(aPatch: GsSoilPatch);
  begin
  { this constructor is for deleting from the browser }
  self.create;
  soilPatch := aPatch;
  end;

function GsRemoveSoilPatchCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      end;
    end;
	end;

destructor GsRemoveSoilPatchCommand.destroy;
	begin
  if done then
  	soilPatch.free;
  inherited destroy;
  end;

procedure GsRemoveSoilPatchCommand.doCommand;
  begin
  inherited doCommand;
  Domain.garden.removeSoilPatch(soilPatch);
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  GardenForm.modelDeleted(soilPatch);
  end;

procedure GsRemoveSoilPatchCommand.undoCommand;
  begin
  inherited undoCommand;
  Domain.garden.addSoilPatch(soilPatch);
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  GardenForm.modelUndeleted(soilPatch);
  end;

function GsRemoveSoilPatchCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'remove soil patch'
  else
    result := 'remove ' + soilPatch.getName;
  result := limitDescription(result);
  end;

procedure GsRemoveSoilPatchCommand.addPlantsInUseNotInGarden(inUseList: TList);
  var
    i: longint;
	begin
  if (done) and (soilPatch <> nil) then
    if (soilPatch.plantList.count > 0) then
    	for i := 0 to soilPatch.plantList.count - 1 do
      	addToListIfAbsent(inUseList, soilPatch.plantList[i]);
  end;

{ ------------------------------------------------------------ GsModifySoilPatchCommand }
function GsModifySoilPatchCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    computedPoint: TPoint;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	cornerBeingDragged := -1;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
    	cornerBeingDragged := soilPatch.findNearestCornerTo(nextPoint);
			oldPoint := soilPatch.corners[cornerBeingDragged];
    	offset.x := nextPoint.x - soilPatch.corners[cornerBeingDragged].x;
    	offset.y := nextPoint.y - soilPatch.corners[cornerBeingDragged].y;
    	end
    else
      begin
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	if cornerBeingDragged >= 0 then
  		begin
  		GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    	computedPoint.x := nextPoint.x - offset.x;
    	computedPoint.y := nextPoint.y - offset.y;
  		soilPatch.moveCornerTo(cornerBeingDragged, computedPoint);
  		GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    	end;
  	end
  else if aTrackPhase = trackRelease then
  	begin
  	if cornerBeingDragged >= 0 then
      begin
      newPoint := soilPatch.corners[cornerBeingDragged];
      end
    else
      begin
      result := nil;
      self.free;
      end;
  	end;
	end;

destructor GsModifySoilPatchCommand.destroy;
  var
    i: longint;
    plant: GsPlant;
	begin
  if (done) and (deletedPlantList <> nil) and (deletedPlantList.count > 0) then
    for i := 0 to deletedPlantList.count - 1 do
    	begin
    	plant := GsPlant(deletedPlantList.items[i]);
    	plant.free;
    	end;
  deletedPlantList.free;
  deletedPlantList := nil;
  inherited destroy;
  end;

procedure GsModifySoilPatchCommand.doCommand;
  var
  	i: longint;
    plant: GsPlant;
  begin
  inherited doCommand;
  {deal with deleting plants}
  deletedPlantList := TList.create;
  if soilPatch.plantList.count > 0 then
    begin
    for i := 0 to soilPatch.plantList.count - 1 do
      begin
      plant := GsPlant(soilPatch.plantList.items[i]);
      if not soilPatch.preciseIncludesPointTest(plant.basePoint) then
        deletedPlantList.add(plant);
      end;
    end;
  if deletedPlantList.count > 0 then
    begin
    ShowMessage('One or more plants or seeds are no longer in the patch and will be deleted. Use Undo to undelete them');
 	 	GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);  {using old rect}
    for i := 0 to deletedPlantList.count - 1 do
      begin
      soilPatch.removePlant(GsPlant(deletedPlantList.items[i]));
      end;
 	 	soilPatch.computeExtendedBoundsRect; {compute new smaller rect for future}
    end;
  end;

procedure GsModifySoilPatchCommand.redoCommand;
  var i: longint;
  begin
  inherited doCommand; {not redo}
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  {remove plants again}
  for i := 0 to deletedPlantList.count - 1 do
    soilPatch.removePlant(GsPlant(deletedPlantList.items[i]));
  soilPatch.moveCornerTo(cornerBeingDragged, newPoint);
  GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  {extended bounds rect recomputed when move corner}
  end;

procedure GsModifySoilPatchCommand.undoCommand;
  var i: longint;
  begin
  inherited undoCommand;
  GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  {put back plants}
  for i := 0 to deletedPlantList.count - 1 do
    soilPatch.addPlant(GsPlant(deletedPlantList.items[i]));
  soilPatch.moveCornerTo(cornerBeingDragged, oldPoint);
  {extended rect recomputed when move corner}
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect); {use bigger rect with plants}
  end;

function GsModifySoilPatchCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'shape soil patch'
  else
    result := 'shape ' + soilPatch.getName;
  result := limitDescription(result);
  end;

procedure GsModifySoilPatchCommand.addPlantsInUseNotInGarden(inUseList: TList);
  var
    i: longint;
    plant: GsPlant;
	begin
  if (done) and (deletedPlantList <> nil) and (deletedPlantList.count > 0) then
    for i := 0 to deletedPlantList.count - 1 do
      begin
      plant := GsPlant(deletedPlantList.items[i]);
      addToListIfAbsent(inUseList, plant);
      end;
  end;

{ ------------------------------------------------------------ GsDragSoilPatchCommand }
function GsDragSoilPatchCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    delta: TPoint;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
    	offset := Point(0,0);
    	end
    else
      begin
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	if mouseDidMove then
      begin
    	GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
      delta := Point(nextPoint.x - previousPoint.x, nextPoint.y - previousPoint.y);
      offset.x := offset.x + delta.x;
      offset.y := offset.y + delta.y;
  		soilPatch.moveBy(delta);
  		GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
      end;
  	end
  else if aTrackPhase = trackRelease then
  	begin
  	end;
	end;

destructor GsDragSoilPatchCommand.destroy;
	begin
  inherited destroy;
  end;

  { ------------------------------------------------------------ GsDragSoilPatchCommand.doCommand should do nothing}

procedure GsDragSoilPatchCommand.redoCommand;
  begin
  inherited doCommand; {not redo}
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  soilPatch.moveBy(offset);
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  end;

procedure GsDragSoilPatchCommand.undoCommand;
  var minusOffset: TPoint;
  begin
  inherited undoCommand;
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  minusOffset := Point(-(offset.x), -(offset.y));
  soilPatch.moveBy(minusOffset);
  GardenForm.invalidateGardenRect(soilPatch.extendedBoundsRect);
  end;

function GsDragSoilPatchCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'drag soil patch'
  else
    result := 'drag ' + soilPatch.getName;
  result := limitDescription(result);
  end;


{ ------------------------------------------------------------ GsChangeAreaSoilPatchCommand }
function GsChangeAreaSoilPatchCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    formResult: smallint;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    soilPatch.fillBasicInfoStructure(oldInfo);
    newInfo := oldInfo;
    if soilPatchAreaForm = nil then
      raise Exception.create('Could not create soil patch area window');
    try
      soilPatchAreaForm.initializeWithSoilPatchBasicInfo(newInfo, kNotNewSoilPatch,
        soilPatch.soilTypeName); 
      formResult := soilPatchAreaForm.showModal;
  	  if formResult = mrOK then
        soilPatchAreaForm.fillBasicInfoStructure(newInfo);
    finally
      soilPatchAreaForm.hide;
    end;
    if formResult = mrCancel then
      begin
    	result := nil;
    	self.free;
    	end;
    end;
  end;

procedure GsChangeAreaSoilPatchCommand.doCommand;
  begin
  inherited doCommand;
  soilPatch.useBasicInfoStructure(newInfo);
  if (newInfo.scale <> oldInfo.scale) or (newInfo.viewingAngle_deg <> oldInfo.viewingAngle_deg) then
    GardenForm.invalidateEntireGarden; {can't be sure how much has changed}
  { more things than these might have changed, due to the effects of changing the area }
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

procedure GsChangeAreaSoilPatchCommand.undoCommand;
  begin
  inherited undoCommand;
  soilPatch.useBasicInfoStructure(oldInfo);
  if (newInfo.scale <> oldInfo.scale) or (newInfo.viewingAngle_deg <> oldInfo.viewingAngle_deg) then
    GardenForm.invalidateEntireGarden; {can't be sure how much has changed}
  { more things than these might have changed, due to the effects of changing the area }
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

function GsChangeAreaSoilPatchCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'change soil patch'
  else
    result := 'change ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsPlantSeedCommand }
function GsPlantSeedCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
  		plant := GsPlant.createWithSoilPatchAndPoint(soilPatch, nextPoint);
      { eventually will get index for template manager list from tool selected contents }
      if cultivar = nil then
        begin
        ShowMessage('There is no cultivar to plant.' + chr(13)
          + 'If no cultivars are loaded, create or load a cultivar template. ' + chr(13)
          + 'Then select a cultivar from the list of cultivars when you plant.');
        result := nil;
        self.free;
        exit;
        end
      else
        begin
        plant.plantFromCultivar(cultivar);
  			inc(Domain.garden.numPlantsCreated);
        giveNewPlantUniqueNameBasedOn(plant, cultivar.getName);
   			soilPatch.addPlant(plant);
        GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
        GardenForm.invalidateGardenRect(plant.boundsRect);
        end;
    	end
    else { soil patch is nil }
      begin
      {cancel Command }
      result := nil;
      self.free;
      end;
    end;
  end;

destructor GsPlantSeedCommand.destroy;
	begin
  if not done then
  	plant.free;
  inherited destroy;
  end;

procedure GsPlantSeedCommand.doCommand;
  begin
  inherited doCommand;
  GardenForm.modelAddedOrRemoved;
  end;

procedure GsPlantSeedCommand.undoCommand;
  begin
  inherited undoCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  soilPatch.removePlant(plant);
  GardenForm.modelDeleted(plant);
  end;

procedure GsPlantSeedCommand.redoCommand;
  begin
  inherited doCommand;
  soilPatch.addPlant(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  GardenForm.modelUndeleted(plant);
  end;

function GsPlantSeedCommand.description: string;
	begin
  if plant = nil then
    result := 'plant seed'
  else
    result := 'plant ' + plant.getName;
  result := limitDescription(result);
  end;

procedure GsPlantSeedCommand.addPlantsInUseNotInGarden(inUseList: TList);
	begin
  if (not done) then
    addToListIfAbsent(inUseList, plant);
  end;

{ ------------------------------------------------------------ GsZapPlantCommand }
constructor GsZapPlantCommand.create;
  begin
  inherited create;
  addedOrganicMatter := GsOrganicMatter.create;
  end;

constructor GsZapPlantCommand.createCommand(aPlant: GsPlant);
  begin
  self.create;
  plant := aPlant;
  soilPatch := plant.soil;
  end;

destructor GsZapPlantCommand.destroy;
	begin
  if done and (plant <> nil) then
  	plant.free;
  if not done then
    addedOrganicMatter.free;
  addedOrganicMatter := nil;
  inherited destroy;
  end;

function GsZapPlantCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    plant := nil;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
  		plant := soilPatch.findPlant(nextPoint) as GsPlant;
    	if plant = nil then
      	begin
        {no plant}
      	result := nil;
      	self.free;
      	end;
    	end
    else
      begin
      {no soil patch}
      result := nil;
      self.free;
      end;
    end
	end;

procedure GsZapPlantCommand.doCommand;
  begin
  inherited doCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  soilPatch.removePlant(plant);
  GardenForm.modelDeleted(plant);
  { the 0.0 says that nothing was harvested from the plant; the false says the roots were not harvested }
  addedOrganicMatter.addWholePlant(plant, 0.0, false);
  if not addedOrganicMatter.hasContents then
    begin
    addedOrganicMatter.free;
    addedOrganicMatter := nil;
    end
  else
    begin
    addedOrganicMatter.setPosition(plant.basePoint);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    soilPatch.looseOrganicMatterList.add(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kToSoil);
    { since the OM gets added to the mulch, invalidate soil patch after adding because it might change color }
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    end;
  end;

procedure GsZapPlantCommand.redoCommand;
  begin
  inherited doCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  soilPatch.removePlant(plant);
  GardenForm.modelDeleted(plant);
  if addedOrganicMatter <> nil then
    begin
    soilPatch.looseOrganicMatterList.add(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kToSoil);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    { since the OM gets added to the mulch, invalidate soil patch after adding because it might change color }
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    end;
  end;

procedure GsZapPlantCommand.undoCommand;
  begin
  inherited undoCommand;
  soilPatch.addPlant(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  GardenForm.modelUndeleted(plant);
  if addedOrganicMatter <> nil then
    begin
    soilPatch.looseOrganicMatterList.remove(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kFromSoil);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    { since the OM gets taken from the mulch, invalidate soil patch after removing because it might change color }
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    end;
  end;

function GsZapPlantCommand.description: string;
	begin
  if plant = nil then
    result := 'pull plant'
  else
    result := 'pull ' + plant.getName;
  result := limitDescription(result);
  end;

procedure GsZapPlantCommand.addPlantsInUseNotInGarden(inUseList: TList);
	begin
  if done then
    addToListIfAbsent(inUseList, plant);
  end;

{ ------------------------------------------------------------ GsZapAllPlantsCommand }
constructor GsZapAllPlantsCommand.create;
  begin
  inherited create;
  zappedPlantList := TList.create;
  addedOrganicMatterList := TList.create;
  end;

destructor GsZapAllPlantsCommand.destroy;
  var
    plant: GsPlant;
    organicMatter: GsOrganicMatter;
    i: longint;
	begin
  { free plants if they were zapped }
  if (done) and (zappedPlantList <> nil) and (zappedPlantList.count > 0) then
    for i := 0 to zappedPlantList.count - 1 do
    	begin
    	plant := GsPlant(zappedPlantList.items[i]);
    	plant.free;
    	end;
  zappedPlantList.free;
  zappedPlantList := nil;
  { free OM blobs if plants were NOT zapped }
  if (not done) and (addedOrganicMatterList <> nil) and (addedOrganicMatterList.count > 0) then
    for i := 0 to addedOrganicMatterList.count - 1 do
    	begin
    	organicMatter := GsOrganicMatter(addedOrganicMatterList.items[i]);
    	organicMatter.free;
    	end;
  addedOrganicMatterList.free;
  addedOrganicMatterList := nil;
  inherited destroy;
  end;

function GsZapAllPlantsCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      end;
    end;
	end;

procedure GsZapAllPlantsCommand.doCommand;
  var
    plant: GsPlant;
    OM: GsOrganicMatter;
    i: longint;
  begin
  inherited doCommand;
  if soilPatch = nil then
    raise Exception.create('GsZapAllPlantsCommand.undoCommand: nil soil patch');
  if soilPatch.plantList.count > 0 then
    for i := 0 to soilPatch.plantList.count - 1 do
      begin
      plant := GsPlant(soilPatch.plantList.items[i]);
      zappedPlantList.add(plant);
      GardenForm.invalidateGardenRect(plant.boundsRect);
      GardenForm.modelDeleted(plant);
      OM := GsOrganicMatter.create;
      { the 0.0 says that nothing was harvested from the plant; the false says the roots were not harvested }
      OM.addWholePlant(plant, 0.0, false);
      { only keep the OM if it has contents }
      if OM.hasContents then
        begin
        OM.setPosition(plant.basePoint);
        GardenForm.invalidateToolRect(OM.boundsRect);
        { add to soil patch list, and add also to command list, for undo/redo }
        soilPatch.looseOrganicMatterList.add(OM);
        addedOrganicMatterList.add(OM);
        OM.transferToOrFromSoil(soilPatch, kToSoil);
        end
      else
        begin
        OM.free;
        OM := nil;
        end;
      end;
  { wait until end to clear pointers in soil patch plant list - because otherwise for loop might be messed up }
  soilPatch.plantList.clearPointersWithoutDeletingObjects;
  end;

procedure GsZapAllPlantsCommand.redoCommand;
  var
    plant: GsPlant;
    OM: GsOrganicMatter;
    i: longint;
  begin
  inherited doCommand;
  if soilPatch = nil then
    raise Exception.create('GsZapAllPlantsCommand.undoCommand: nil soil patch');
  if soilPatch.plantList.count > 0 then
    for i := 0 to soilPatch.plantList.count - 1 do
      begin
      plant := GsPlant(soilPatch.plantList.items[i]);
      zappedPlantList.add(plant);
      GardenForm.invalidateGardenRect(plant.boundsRect);
      GardenForm.modelDeleted(plant);
      end;
  { wait until end to clear pointers in soil patch plant list - because otherwise for loop might be messed up }
  soilPatch.plantList.clearPointersWithoutDeletingObjects;
  { add OMs in list to soil patch organic matter and garden invalidation, but do not remove from command list }
  if addedOrganicMatterList.count > 0 then
    for i := 0 to addedOrganicMatterList.count - 1 do
      begin
      OM := GsOrganicMatter(addedOrganicMatterList.items[i]);
      GardenForm.invalidateToolRect(OM.boundsRect);
      soilPatch.looseOrganicMatterList.add(OM);
      OM.transferToOrFromSoil(soilPatch, kToSoil);
      end;
  end;

procedure GsZapAllPlantsCommand.undoCommand;
  var
    plant: GsPlant;
    OM: GsOrganicMatter;
    i: longint;
  begin
  inherited undoCommand;
  if soilPatch = nil then
    raise Exception.create('GsZapAllPlantsCommand.undoCommand: nil soil patch');
  if zappedPlantList.count > 0 then
    for i := 0 to zappedPlantList.count - 1 do
      begin
      plant := GsPlant(zappedPlantList.items[i]);
      soilPatch.addPlant(plant);
      GardenForm.invalidateGardenRect(plant.boundsRect);
      GardenForm.modelUnDeleted(plant);
      end;
  { wait until end to clear pointers in soil patch plant list - because otherwise for loop might be messed up }
  zappedPlantList.clear;
  { remove organic matter blobs from soil patch - but only those created by this command }
  if addedOrganicMatterList.count > 0 then
    for i := 0 to addedOrganicMatterList.count - 1 do
      begin
      OM := GsOrganicMatter(addedOrganicMatterList.items[i]);
      soilPatch.looseOrganicMatterList.remove(OM);
      OM.transferToOrFromSoil(soilPatch, kFromSoil);
      GardenForm.invalidateToolRect(OM.boundsRect);
      end;
  end;

function GsZapAllPlantsCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'pull plants in soil patch'
  else
    result := 'pull plants in ' + soilPatch.getName;
  result := limitDescription(result);
  end;

procedure GsZapAllPlantsCommand.addPlantsInUseNotInGarden(inUseList: TList);
  var
    plant: GsPlant;
    i: longint;
	begin
  if done then
    if (zappedPlantList <> nil) and (zappedPlantList.count > 0) then
      for i := 0 to zappedPlantList.count - 1 do
        begin
        plant := GsPlant(zappedPlantList.items[i]);
        addToListIfAbsent(inUseList, plant);
        end;
  end;

{ ------------------------------------------------------------ GsReseedPlantCommand }
constructor GsReseedPlantCommand.create;
  begin
  inherited create;
  startGrowingFlag := kBecomeSeedAndStartGrowingNow;
  addedOrganicMatter := GsOrganicMatter.create;
  end;

constructor GsReseedPlantCommand.createWithPlant(thePlant: GsPlant; aStartGrowingFlag: boolean);
  begin
  self.create;
  startGrowingFlag := aStartGrowingFlag;
  plant := thePlant;
  soilPatch := plant.soil;
  if soilPatch = nil then
    raise Exception.create('GsReseedPlantCommand.createWithPlant: nil soil patch');
  end;

function GsReseedPlantCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    plant := nil;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
  		plant := soilPatch.findPlant(nextPoint) as GsPlant;
    	if plant = nil then
      	begin
        {no plant}
      	result := nil;
      	self.free;
      	end;
    	end
    else
      begin
      {no soil patch}
      result := nil;
      self.free;
      end;
    end
	end;

destructor GsReseedPlantCommand.destroy;
	begin
  plantCopy.free;
  if done then
  	drawingPlant.free
  else
    drawingPlantCopy.free;
  if not done then
    addedOrganicMatter.free;
  addedOrganicMatter := nil;
  inherited destroy;
  end;

procedure GsReseedPlantCommand.doCommand;
  var
    biomassLost: BiomassLostInReseedingStructure;
  begin
  inherited doCommand;
  { create copy of plant }
  plantCopy := GsPlant.create;
  plant.copyTo(plantCopy);
  { save pointer to drawing plant (so removed plant parts pointers are valid) }
  drawingPlant := plant.drawingPlant;
  { create new copy of drawing plant in plant }
  drawingPlantCopy := GsDrawingPlant.create;
  plant.drawingPlant := drawingPlantCopy;
  drawingPlant.copyTo(plant.drawingPlant);
  { this clears out drawing plant objects }
  plant.drawingPlant.model := plant;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plant.becomeSeed(startGrowingFlag, biomassLost);
  { take the plant biomass lost and make an OM blob }
  addedOrganicMatter.addReseededPlantBiomass(biomassLost);
  if not addedOrganicMatter.hasContents then
    begin
    addedOrganicMatter.free;
    addedOrganicMatter := nil;
    end
  else
    begin
    addedOrganicMatter.setPosition(plant.basePoint);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    soilPatch.looseOrganicMatterList.add(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kToSoil);
    end;
  { update soil patch plant means because root weights changed }
  soilPatch.updatePlantMeans;
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  { must redraw plant before soil patch }
  { since the OM gets added to the mulch, invalidate soil patch after adding because it might change color }
  if addedOrganicMatter <> nil then
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

procedure GsReseedPlantCommand.undoCommand;
  begin
  inherited undoCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plantCopy.copyTo(plant);
  plant.drawingPlant := drawingPlant;
  plant.drawingPlant.model := plant;
  if addedOrganicMatter <> nil then
    begin
    soilPatch.looseOrganicMatterList.remove(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kFromSoil);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    end;
  { update soil patch plant means because root weights changed }
  soilPatch.updatePlantMeans;
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  { must redraw plant before soil patch }
  { since the OM gets added to the mulch, invalidate soil patch after adding because it might change color }
  if addedOrganicMatter <> nil then
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

procedure GsReseedPlantCommand.redoCommand;
  var
    fakeBiomassLost: BiomassLostInReseedingStructure;
  begin
  inherited doCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plant.drawingPlant := drawingPlantCopy;
  plant.drawingPlant.model := plant;
  plant.becomeSeed(startGrowingFlag, fakeBiomassLost);
  if addedOrganicMatter <> nil then
    begin
    soilPatch.looseOrganicMatterList.add(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kToSoil);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    end;
  { update soil patch plant means because root weights changed }
  soilPatch.updatePlantMeans;
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  { must redraw plant before soil patch }
  { since the OM gets added to the mulch, invalidate soil patch after adding because it might change color }
  if addedOrganicMatter <> nil then
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

function GsReseedPlantCommand.description: string;
	begin
  if plant = nil then
    result := 'reseed plant'
  else
    result := 'reseed ' + plant.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsGrowPlantCommand }
constructor GsGrowPlantCommand.createWithPlant(thePlant: GsPlant);
  begin
  self.create;
  plant := thePlant;
  soilPatch := plant.soil;
  end;

function GsGrowPlantCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    plant := nil;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
  		plant := soilPatch.findPlant(nextPoint) as GsPlant;
    	if plant = nil then
      	begin
        {no plant}
      	result := nil;
      	self.free;
      	end;
    	end
    else
      begin
      {no soil patch}
      result := nil;
      self.free;
      end;
    end
	end;

destructor GsGrowPlantCommand.destroy;
	begin
  plantCopy.free;
  if done then
  	drawingPlant.free
  else
    drawingPlantCopy.free;
  inherited destroy;
  end;

{ the days to grow for the grow command are hard-coded (to avoid the more complicated tool param list
  manager, which has units and allows users to edit the lists). These two functions are the only ones
  that know what is in the list. }
class procedure GsGrowPlantCommand.fillComboBoxItemsWithDaysToGrowChoices(comboBoxItems: TStrings);
  begin
  comboBoxItems.add('5');
  comboBoxItems.add('10');
  comboBoxItems.add('15');
  comboBoxItems.add('20');
  comboBoxItems.add('30');
  comboBoxItems.add('40');
  comboBoxItems.add('50');
  comboBoxItems.add('60');
  end;

procedure GsGrowPlantCommand.setDaysToGrowWithItemIndex(index: smallint);
  begin
  numDaysToGrow := 0;
  case index of
    0: numDaysToGrow := 5;
    1: numDaysToGrow := 10;
    2: numDaysToGrow := 15;
    3: numDaysToGrow := 20;
    4: numDaysToGrow := 30;
    5: numDaysToGrow := 40;
    6: numDaysToGrow := 50;
    7: numDaysToGrow := 60;
    end;
  end;

procedure GsGrowPlantCommand.doCommand;
  begin
  inherited doCommand;
  { create copy of plant }
  plantCopy := GsPlant.create;
  plant.copyTo(plantCopy);
  { save pointer to drawing plant (so removed plant parts pointers are valid) }
  drawingPlant := plant.drawingPlant;
  { create new copy of drawing plant in plant }
  drawingPlantCopy := GsDrawingPlant.create;
  plant.drawingPlant := drawingPlantCopy;
  drawingPlant.copyTo(plant.drawingPlant);
  plant.drawingPlant.model := plant;
  { this clears out drawing plant objects }
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plant.grow(numDaysToGrow);
  { update soil patch plant means because root weights changed }
  soilPatch.updatePlantMeans;
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

procedure GsGrowPlantCommand.undoCommand;
  begin
  inherited undoCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plantCopy.copyTo(plant);
  plant.drawingPlant := drawingPlant;
  plant.drawingPlant.model := plant;
  { update soil patch plant means because root weights changed }
  soilPatch.updatePlantMeans;
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

procedure GsGrowPlantCommand.redoCommand;
  begin
  inherited doCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plant.drawingPlant := drawingPlantCopy;
  plant.drawingPlant.model := plant;
  plant.grow(numDaysToGrow);
  { update soil patch plant means because root weights changed }
  soilPatch.updatePlantMeans;
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  end;

function GsGrowPlantCommand.description: string;
	begin
  if plant = nil then
    result := 'grow plant'
  else
    result := 'grow ' + plant.getName + ' for ' + intToStr(numDaysToGrow) + ' days';
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsTransplantCommand }
function GsTransplantCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    plant := nil;
  	oldSoilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if oldSoilPatch <> nil then
  		begin
  		plant := oldSoilPatch.findPlant(nextPoint) as GsPlant;
    	if plant = nil then
      	begin
        {no plant}
      	result := nil;
      	self.free;
      	end
      else
        begin
        newSoilPatch := oldSoilPatch;
        grabOffset.x := plant.basePoint.x - nextPoint.x;
        grabOffset.y := plant.basePoint.y - nextPoint.y;
        oldPoint := nextPoint;
        newPoint := nextPoint;
        end;
    	end
    else
      begin
      {no soil patch}
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
    if mouseDidMove then
      begin
      newPoint.x := nextPoint.x + grabOffset.x;
      newPoint.y := nextPoint.y + grabOffset.y;
  		GardenForm.invalidateGardenRect(plant.boundsRect);
 		 	plant.moveTo(newPoint);
  		GardenForm.invalidateGardenRect(plant.boundsRect);
      end;
  	end
  else if aTrackPhase = trackRelease then
  	begin
    newPoint.x := nextPoint.x + grabOffset.x;
    newPoint.y := nextPoint.y + grabOffset.y;
    newSoilPatch := Domain.garden.findSoilPatch(newPoint) as GsSoilPatch;
    {don't do if plant didn't really move}
    if (oldPoint.x = newPoint.x) and (oldPoint.y = newPoint.y) then
      begin
  		GardenForm.invalidateGardenRect(plant.boundsRect);
      result := nil;
      self.free;
      end
    else if (newSoilPatch = nil) then
      begin
      { put back if is outside of a patch}
  		GardenForm.invalidateGardenRect(plant.boundsRect);
      plant.moveTo(oldPoint);
  		GardenForm.invalidateGardenRect(plant.boundsRect);
      result := nil;
      self.free;
      end;
    end;
	end;

procedure GsTransplantCommand.doCommand;
  begin
  inherited doCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plant.transplant(oldPoint, oldSoilPatch, newPoint, newSoilPatch);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  self.updateTotalsAndBrowserForTransplant;
  end;

procedure GsTransplantCommand.undoCommand;
  begin
  inherited undoCommand;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  plant.transplant(newPoint, newSoilPatch, oldPoint, oldSoilPatch);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  self.updateTotalsAndBrowserForTransplant;
  end;

procedure GsTransplantCommand.updateTotalsAndBrowserForTransplant;
  begin
  if oldSoilPatch <> newSoilPatch then
    begin
    { update soil patch plant means because root weights changed }
    oldSoilPatch.updatePlantMeans;
    newSoilPatch.updatePlantMeans;
    { tell browser that plant is now under new soil patch and that some values for plant and patches might have changed }
    BrowserForm.updateObjectChoices;
    BrowserForm.updateModelValuesForModel(plant);
    BrowserForm.updateModelValuesForModel(oldSoilPatch);
    BrowserForm.updateModelValuesForModel(newSoilPatch);
    end;
  end;

function GsTransplantCommand.description: string;
	begin
  if plant = nil then
    result := 'transplant plant'
  else
    begin
    if (oldSoilPatch = newSoilPatch) then
      result := 'transplant ' + plant.getName + ' within ' + oldSoilPatch.getName
    else
      result := 'transplant ' + plant.getName + ' from ' + oldSoilPatch.getName + ' to ' + newSoilPatch.getName;
    end;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsSetBrowserChoiceLeftCommand }
{does other stuff - not undoable}
function GsSetBrowserChoiceLeftCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand;
  var
    aSoilPatch: GsSoilPatch;
    aPlant: GsPlant;
	begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    aPlant := nil;
    aSoilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
    if aSoilPatch <> nil then
      aPlant := aSoilPatch.findPlant(nextPoint) as GsPlant;
    if aPlant <> nil then
      BrowserForm.setSelectedModel(aPlant)
    else if aSoilPatch <> nil then
      BrowserForm.setSelectedModel(aSoilPatch)
    else
      BrowserForm.setSelectedModel(Domain.garden.weather);
    {configure browser form here - show left, right, both}
    BrowserForm.showMode(kBrowserShowLeft);
    end
  else
    begin
    result := nil;
    self.free;
    end;
  end;

{ ------------------------------------------------------------ GsSetBrowserChoiceRightCommand }
{does other stuff - not undoable}
function GsSetBrowserChoiceRightCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand;
  var
    aSoilPatch: GsSoilPatch;
    aPlant: GsPlant;
	begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    aPlant := nil;
    aSoilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
    if aSoilPatch <> nil then
      aPlant := aSoilPatch.findPlant(nextPoint) as GsPlant;
    if aPlant <> nil then
      BrowserForm.setSelectedModel(aPlant)
    else if aSoilPatch <> nil then
      BrowserForm.setSelectedModel(aSoilPatch)
    else
      BrowserForm.setSelectedModel(Domain.garden.weather);
    {configure browser form here - show left, right, both}
    BrowserForm.showMode(kBrowserShowRight);
    end
  else
    begin
    result := nil;
    self.free;
    end;
  end;

{ ------------------------------------------------------------ GsSetBrowserChoiceBothCommand }
{does other stuff - not undoable}
function GsSetBrowserChoiceBothCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand;
  var
    aSoilPatch: GsSoilPatch;
    aPlant: GsPlant;
	begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    aPlant := nil;
    aSoilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
    if aSoilPatch <> nil then
      aPlant := aSoilPatch.findPlant(nextPoint) as GsPlant;
    if aPlant <> nil then
      BrowserForm.setSelectedModel(aPlant)
    else if aSoilPatch <> nil then
      BrowserForm.setSelectedModel(aSoilPatch)
    else
      BrowserForm.setSelectedModel(Domain.garden.weather);
    {configure browser form here - show left, right, both}
    BrowserForm.showMode(kBrowserShowBoth);
    end
  else
    begin
    result := nil;
    self.free;
    end;
  end;

{ ------------------------------------------------------------ GsRenameCommand  }
function GsRenameCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
{$IFDEF WINDOWS}
    afterPrefixAnsii, newNameAnsii: string;
{$ELSE}
    afterPrefixAnsii, newNameAnsii: ansistring;
{$ENDIF}
		newName, prefix, afterPrefix: string;
    soilPatch: GsSoilPatch;
    plant: GsPlant;
  begin
  result := self;
  { if do pop up in trackPress, then get error later - need to wait till released }
  if aTrackPhase = trackRelease then
  	begin
    result := nil;
    plant := nil;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
  		plant := soilPatch.findPlant(nextPoint) as GsPlant;
    	if plant = nil then
      	begin
        { rename soil }
        newName := soilPatch.getName;
    		separateNameIntoPrefixAndAfterPrefix(newName, prefix, afterPrefix);
    		afterPrefixAnsii := afterPrefix;
        if inputQuery('Rename soil patch', 'Please enter a new name for soil patch ' + prefix, afterPrefixAnsii) then
          result := GsChangeModelNameCommand.createCommand(soilPatch, 's' + prefix + ': ' + afterPrefixAnsii);
      	end
      else
        begin
        { rename plant }
        newName := plant.getName;
    		separateNameIntoPrefixAndAfterPrefix(newName, prefix, afterPrefix);
    		afterPrefixAnsii := afterPrefix;
        if inputQuery('Rename plant', 'Please enter a new name for plant ' + prefix, afterPrefixAnsii) then
          result := GsChangeModelNameCommand.createCommand(plant, 'p' + prefix + ': ' + afterPrefixAnsii);
        end;
      afterPrefix := afterPrefixAnsii;
    	end;
    self.free;
    end;
	end;

{ ------------------------------------------------------------ GsRemoveGraphLoggedVarCommand }
constructor GsRemoveGraphLoggedVarCommand.createCommand(aLoggedVar: GsLoggedVar; anIndex: longint);
  begin
  inherited create;
  loggedVar := aLoggedVar;
  loggedVarIndex := anIndex;
  end;

destructor GsRemoveGraphLoggedVarCommand.destroy;
	begin
  if done and (loggedVar <> nil) then
  	loggedVar.free;
  loggedVar := nil;
  inherited destroy;
  end;

procedure GsRemoveGraphLoggedVarCommand.doCommand;
  begin
  inherited doCommand;
  GraphForm.removeLoggedVarFromList(loggedVarIndex);
  end;

procedure GsRemoveGraphLoggedVarCommand.undoCommand;
  begin
  inherited undoCommand;
  GraphForm.addLoggedVarToList(loggedVar, loggedVarIndex);
  end;

function GsRemoveGraphLoggedVarCommand.description: string;
	begin
  if loggedVar = nil then
    result := 'remove aspect from graph '
  else
    result := 'remove from graph ' + loggedVar.displayName(kDontShowLayerInfo);
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsRemovePlantPartsCommand }
constructor GsRemovePlantPartsCommand.create;
  begin
  inherited create;
  removedPlantPartsList := TList.create;
  addedHarvestItemList := TList.create;
  addedOrganicMatter := GsOrganicMatter.create;
  pullPlant := false;
  soilPatchColorChanges := false; {used to tell when to redraw entire patch}
  { after calling create, user will add items to be removed to list }
  end;

destructor GsRemovePlantPartsCommand.destroy;
  var i: longint;
  begin
  removedPlantPartsList.free;
  removedPlantPartsList := nil;
  if (not self.done) and (addedHarvestItemList.count > 0) then
    for i := 0 to addedHarvestItemList.count - 1 do
      begin
      GsHarvestItem(addedHarvestItemList.items[i]).free;
      end;
  addedHarvestItemList.free;
  addedHarvestItemList := nil;
  if (not self.done) then
    addedOrganicMatter.free;
  addedOrganicMatter := nil;
  inherited destroy;
  end;

procedure GsRemovePlantPartsCommand.doCommand;
  var
    plantPart: GsDrawingPlantPart;
    newHarvestItem: GsHarvestItem;
    i: longint;
    oldSoilColor: TColorRef;
  begin
  inherited doCommand;
  if (plant = nil) or (soilPatch = nil ) then
    raise Exception.create('GsRemovePlantPartsCommand.doCommand: need to set plant and soil patch');
  oldSoilColor := soilPatch.surfaceColor;
  if removedPlantPartsList.count > 0 then
    begin
    for i := 0 to removedPlantPartsList.count - 1 do
      begin
      plantPart := GsDrawingPlantPart(removedPlantPartsList.items[i]);
      if plantPart.canBeHarvested then
        begin
        newHarvestItem := GsHarvestItem.create;
        with newHarvestItem do
          begin
          count := 1;
          biomass_g := plantPart.liveBiomassForHarvest_kg * kg_to_g;
          dateHarvested := Domain.garden.date;
          plantName := plant.getName;
          soilPatchName := soilPatch.getName;
          harvestItemTemplate := plantPart.harvestItemTemplate;
          if harvestItemTemplate = nil then
            raise Exception.create('GsRemovePlantPartsCommand.doCommand: nil harvestItemTemplate');
          end;
        addedHarvestItemList.add(newHarvestItem);
        Domain.harvestManager.harvestItemList.add(newHarvestItem);
        if not pullPlant and (plantPart.deadBiomassForHarvest_kg > 0.0) then
          addedOrganicMatter.addPlantPartBiomass(plantPart, false);
        end
      else
        begin
        if not pullPlant and (plantPart.totalBiomassForHarvest_kg > 0.0) then
          addedOrganicMatter.addPlantPartBiomass(plantPart, true);
        end;
      if not pullPlant then plantPart.addOrRemove(kRemovingBiomassFromPlant);
      end;
    end;
  if (addedHarvestItemList.count > 0) then
    begin
    HarvestForm.handlePossibleHarvestListChange;
    GardenForm.updateHarvestPanel := true;
    end;
  if not pullPlant then
    begin
    self.handleOrganicMatter;
    BrowserForm.updateModelValuesForModel(plant);
    end;
  BrowserForm.updateModelValuesForModel(soilPatch);
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  if oldSoilColor <> soilPatch.surfaceColor then
    soilPatchColorChanges := true;
  if soilPatchColorChanges then
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  end;

procedure GsRemovePlantPartsCommand.handleOrganicMatter;
  begin
  if not addedOrganicMatter.hasContents then
    begin
    addedOrganicMatter.free;
    addedOrganicMatter := nil;
    end
  else
    begin
    addedOrganicMatter.setPosition(plant.basePoint);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    soilPatch.looseOrganicMatterList.add(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kToSoil);
    end;
  end;

procedure GsRemovePlantPartsCommand.undoCommand;
  var
    plantPart: GsDrawingPlantPart;
    newHarvestItem: GsHarvestItem;
    i: longint;
  begin
  inherited undoCommand;
  if not pullPlant and (removedPlantPartsList.count > 0) then
    begin
    { go through list backwards on undo so fruit biomass will be correct to add biomass
      back into plant before flag (for inclusion in inflorescence) is reset by inflorescence }
    for i := removedPlantPartsList.count - 1 downto 0 do
      begin
      plantPart := GsDrawingPlantPart(removedPlantPartsList.items[i]);
      plantPart.addOrRemove(kAddingBiomassToPlant);
      end;
    end;
  if addedOrganicMatter <> nil then
    begin
    soilPatch.looseOrganicMatterList.remove(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kFromSoil);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    end;
  if addedHarvestItemList.count > 0 then
    begin
    for i := 0 to addedHarvestItemList.count - 1 do
      begin
      newHarvestItem := addedHarvestItemList.items[i];
      Domain.harvestManager.harvestItemList.remove(newHarvestItem);
      end;
    HarvestForm.handlePossibleHarvestListChange;
    GardenForm.updateHarvestPanel := true;
    end;
  if not pullPlant then BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  if soilPatchColorChanges then
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  end;

procedure GsRemovePlantPartsCommand.redoCommand;
  var
    plantPart: GsDrawingPlantPart;
    newHarvestItem: GsHarvestItem;
    i: longint;
  begin
  inherited doCommand;
  if not pullPlant and (removedPlantPartsList.count > 0) then
    begin
    for i := 0 to removedPlantPartsList.count - 1 do
      begin
      plantPart := GsDrawingPlantPart(removedPlantPartsList.items[i]);
      plantPart.addOrRemove(kRemovingBiomassFromPlant);
      end;
    end;
  if addedOrganicMatter <> nil then
    begin
    soilPatch.looseOrganicMatterList.add(addedOrganicMatter);
    addedOrganicMatter.transferToOrFromSoil(soilPatch, kToSoil);
    GardenForm.invalidateToolRect(addedOrganicMatter.boundsRect);
    end;
  if addedHarvestItemList.count > 0 then
    begin
    for i := 0 to addedHarvestItemList.count - 1 do
      begin
      newHarvestItem := addedHarvestItemList.items[i];
      Domain.harvestManager.harvestItemList.add(newHarvestItem);
      end;
    HarvestForm.handlePossibleHarvestListChange;
    GardenForm.updateHarvestPanel := true;
    end;
  if not pullPlant then BrowserForm.updateModelValuesForModel(plant);
  BrowserForm.updateModelValuesForModel(soilPatch);
  GardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant);
  GardenForm.invalidateGardenRect(plant.boundsRect);
  if soilPatchColorChanges then
    GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  end;

function GsRemovePlantPartsCommand.description: string;
	begin
  if plant = nil then
    result := 'harvest plant part(s) from plant'
  else
    result := 'harvest plant part(s) from ' + plant.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsHarvestPlantCommand }
constructor GsHarvestPlantCommand.create;
  begin
  inherited create;
  bundledPlantPartsList := TList.create;
  end;

destructor GsHarvestPlantCommand.destroy;
  begin
  bundledPlantPartsList.free;
  bundledPlantPartsList := nil;
  if self.done and pullPlant and not plantIsKeptForReseeding then
    begin
    plant.free;
    plant := nil;
    end;
  inherited destroy;
  end;

function GsHarvestPlantCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; 
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    plant := nil;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch <> nil then
  		begin
  		plant := soilPatch.findPlant(nextPoint) as GsPlant;
    	if plant = nil then
      	begin
        {no plant}
      	result := nil;
      	self.free;
      	end
      else if (not userWantsToPullPlant) and (plant.drawingPlant.iconForHarvesting(false) = nil) then
        begin
        {nothing to harvest}
      	result := nil;
      	self.free;
        end;
    	end
    else
      begin
      {no soil patch}
      result := nil;
      self.free;
      end;
    end
	end;

procedure GsHarvestPlantCommand.doCommand;
  var
    i: longint;
    drawingPlant: GsDrawingPlant;
    plantPart: GsDrawingPlantPart;
    newHarvestItem: GsHarvestItem;
    statistics: GsDrawingPlantStatistics;
    totalBiomassHarvested_g, tPha_to_g: single;
    harvestedRoots: boolean;
    harvestItem, bundledHarvestItem: GsHarvestItem;
    oldSoilColor: TColorRef;
    rootIsBundled,storageIsLumpedWithWhole, stemIsBundled: boolean;
  begin
  bundledHarvestItem := nil;
  statistics := nil;
  harvestedRoots := false;
  drawingPlant := plant.drawingPlant;
  tPha_to_g := t_to_kg * kg_to_g * soilPatch.params.area_ha;
  oldSoilColor := soilPatch.surfaceColor;

  if userWantsToPullPlant then
    pullPlant := true
  else
    { figure out if plant should be pulled because parts are lumped together }
    pullPlant := drawingPlant.mustBePulledToHarvest;

  if pullPlant and not userWantsToPullPlant then
    plantIsKeptForReseeding :=
    	Domain.menuOptions.reseedingOption <> kReseedingOptionNever
  else
    plantIsKeptForReseeding := false;

  {put plant parts for bundling in list}
  if pullPlant and drawingPlant.hasHarvestItemTemplateFor(kHarvestPartTypeWholePlant) then
    drawingPlant.addAllBundledPlantPartsToList(bundledPlantPartsList);

  { decide whether to create a bundling harvest item }
  bundledHarvestItem := nil;
  rootIsBundled := drawingPlant.isBundled(kHarvestPartTypeRoot);
  storageIsLumpedWithWhole := drawingPlant.storageLumpedWith(kHarvestPartTypeWholePlant);
  stemIsBundled := drawingPlant.isBundled(kHarvestPartTypeStem);
  if pullPlant and
     ((bundledPlantPartsList.count > 0) or
     drawingPlant.isBundled(kHarvestPartTypeRoot) or
     drawingPlant.storageLumpedWith(kHarvestPartTypeWholePlant) or
     drawingPlant.isBundled(kHarvestPartTypeStem)) then
    begin
    bundledHarvestItem := GsHarvestItem.create;
    with bundledHarvestItem do
      begin
      count := 1;
      biomass_g := 0.0;
      dateHarvested := Domain.garden.date;
      plantName := plant.getName;
      soilPatchName := soilPatch.getName;
      harvestItemTemplate := drawingPlant.harvestItemTemplateFor(kHarvestPartTypeWholePlant);
      if harvestItemTemplate = nil then
        raise Exception.create('GsHarvestPlantCommand.doCommand: nil harvestItemTemplate');
      end;
    end;

  { go through bundled list, total biomass, fill item }
  if pullPlant and (bundledPlantPartsList.count > 0)
      and (bundledHarvestItem <> nil) then
    for i := 0 to bundledPlantPartsList.count - 1 do
      begin
      plantPart := GsDrawingPlantPart(bundledPlantPartsList.items[i]);
      { set bundled flags on all parts in bundled list - undo should set them to false }
      plantPart.isBundled := true;
      with bundledHarvestItem do
        biomass_g := biomass_g + plantPart.liveBiomassForHarvest_kg * kg_to_g;
      { if doing bundling, are always pulling plant, so don't need to
        add to OM or remove from plant since these things are done at the end of this method }
      end;
  { check if bundling root (include storage organ if lumped into root }
  if pullPlant and drawingPlant.isBundled(kHarvestPartTypeRoot)
      and (bundledHarvestItem <> nil) then
    begin
    harvestedRoots := true;
    { should be a yield parameter for roots }
    with bundledHarvestItem do
      begin
      biomass_g := biomass_g + plant.biomass.totalRootWeight_tPha * tPha_to_g;
      if drawingPlant.storageLumpedWith(kHarvestPartTypeRoot) then
        biomass_g := biomass_g + plant.biomass.storageOrgan_tPha * tPha_to_g;
      end;
    end;
  { check if bundling storage organ into stems that have been bundled }
  if pullPlant and drawingPlant.storageLumpedWith(kHarvestPartTypeStem)
      and drawingPlant.isBundled(kHarvestPartTypeStem)
      and (bundledHarvestItem <> nil) then
    begin
    with bundledHarvestItem do
      biomass_g := biomass_g + plant.biomass.storageOrgan_tPha * tPha_to_g;
    end;
  { check if bundling storage organ into whole plant }
  if pullPlant and drawingPlant.storageLumpedWith(kHarvestPartTypeWholePlant) 
      and (bundledHarvestItem <> nil) then
    begin
    with bundledHarvestItem do
      biomass_g := biomass_g + plant.biomass.storageOrgan_tPha * tPha_to_g;
    end;

  if bundledHarvestItem <> nil then
    begin
    addedHarvestItemList.add(bundledHarvestItem);
    Domain.harvestManager.harvestItemList.add(bundledHarvestItem);
    end;

  { add mature plant parts to removed plant parts list, whether pull plant or not }
  plant.drawingPlant.addAllMaturePlantPartsToList(removedPlantPartsList);

  { if pulling plant, add invisible parts (root, storage organ) and lumped stems if harvested
    and not already bundled. storage organ can only be lumped in with root, stem or whole plant }

  { create root item if necessary (add storage organ if necessary) }
  if pullPlant
      and drawingPlant.hasHarvestItemTemplateFor(kHarvestPartTypeRoot)
      and not drawingPlant.isBundled(kHarvestPartTypeRoot) then
    begin
    harvestedRoots := true;
    newHarvestItem := GsHarvestItem.create;
    with newHarvestItem do
      begin
      count := 1;
      biomass_g := plant.biomass.totalRootWeight_tPha * tPha_to_g;
      {root weight should be reduced by a 'useable' parameter?}
      if drawingPlant.storageLumpedWith(kHarvestPartTypeRoot) and
          not drawingPlant.isBundled(kHarvestPartTypeStorageOrgan) then
        biomass_g := biomass_g + plant.biomass.storageOrgan_tPha * tPha_to_g;
      dateHarvested := Domain.garden.date;
      plantName := plant.getName;
      soilPatchName := soilPatch.getName;
      harvestItemTemplate := drawingPlant.harvestItemTemplateFor(kHarvestPartTypeRoot);
      if harvestItemTemplate = nil then
        raise Exception.create('GsHarvestPlantCommand.doCommand: nil harvestItemTemplate');
      end;
    addedHarvestItemList.add(newHarvestItem);
    Domain.harvestManager.harvestItemList.add(newHarvestItem);
    end;

  { create stem item if necessary (add storage organ if necessary) }
  if pullPlant
      and drawingPlant.hasHarvestItemTemplateFor(kHarvestPartTypeStem)
      and not drawingPlant.isBundled(kHarvestPartTypeStem) then
    begin
    { gather plant statistics to get weight of all stems }
    statistics := GsDrawingPlantStatistics.create;
    drawingPlant.getDrawingPlantStatistics(statistics);
    newHarvestItem := GsHarvestItem.create;
    with newHarvestItem do
      begin
      count := 1;
      biomass_g := statistics.liveBiomass_kg[kHarvestPartTypeStem] * kg_to_g;
      if drawingPlant.storageLumpedWith(kHarvestPartTypeStem) and
        not drawingPlant.isBundled(kHarvestPartTypeStorageOrgan) then
        biomass_g := biomass_g + plant.biomass.storageOrgan_tPha * tPha_to_g;
      dateHarvested := Domain.garden.date;
      plantName := plant.getName;
      soilPatchName := soilPatch.getName;
      harvestItemTemplate := drawingPlant.harvestItemTemplateFor(kHarvestPartTypeStem);
      if harvestItemTemplate = nil then
        raise Exception.create('GsHarvestPlantCommand.doCommand: nil harvestItemTemplate');
      end;
    addedHarvestItemList.add(newHarvestItem);
    Domain.harvestManager.harvestItemList.add(newHarvestItem);
    statistics.free;
    statistics := nil;
    end;

  { create storage organ item if necessary }
  if pullPlant
      and drawingPlant.hasHarvestItemTemplateFor(kHarvestPartTypeStorageOrgan)
      and not drawingPlant.isBundled(kHarvestPartTypeStorageOrgan)
      and drawingPlant.storageLumpedWith(kHarvestPartTypeStorageOrgan) then
    begin
    newHarvestItem := GsHarvestItem.create;
    with newHarvestItem do
      begin
      count := 1;
      biomass_g := plant.biomass.storageOrgan_tPha * tPha_to_g;
      dateHarvested := Domain.garden.date;
      plantName := plant.getName;
      soilPatchName := soilPatch.getName;
      harvestItemTemplate := drawingPlant.harvestItemTemplateFor(kHarvestPartTypeStorageOrgan);
      if harvestItemTemplate = nil then
        raise Exception.create('GsHarvestPlantCommand.doCommand: nil harvestItemTemplate');
      end;
    addedHarvestItemList.add(newHarvestItem);
    Domain.harvestManager.harvestItemList.add(newHarvestItem);
    end;

  inherited doCommand;

  { add remainder of plant to OM if plant is pulled up }
  if pullPlant then
    begin
    { total up all biomass in harvest items }
    totalBiomassHarvested_g := 0.0;
    if addedHarvestItemList.count > 0 then
      for i := 0 to addedHarvestItemList.count - 1 do
        begin
        harvestItem := GsHarvestItem(addedHarvestItemList.items[i]);
        totalBiomassHarvested_g := totalBiomassHarvested_g + harvestItem.biomass_g;
        end;
    addedOrganicMatter.addWholePlant(plant, totalBiomassHarvested_g, harvestedRoots);
    self.handleOrganicMatter;
    end;

  if oldSoilColor <> soilPatch.surfaceColor then
    soilPatchColorChanges := true;

  { if plant was pulled, remove from soil patch and garden }
  if pullPlant  then
    begin
    if plantIsKeptForReseeding then
      begin
      plant.awaitingReseeding := true;
      soilPatch.updatePlantMeans;
      BrowserForm.modelDeleted(plant);
      end
    else
      begin
    	soilPatch.removePlant(plant);
    	GardenForm.modelDeleted(plant);
      end;
    if soilPatchColorChanges then
      GardenForm.invalidateGardenRect(soilPatch.boundsRect);
    end;
  end;

procedure GsHarvestPlantCommand.undoCommand;
  var
    plantPart: GsDrawingPlantPart;
  	i: longint;
  begin
  if pullPlant then
    begin
    if plantIsKeptForReseeding then
      begin
      plant.awaitingReseeding := false;
      soilPatch.updatePlantMeans;
      BrowserForm.modelUndeleted(plant);
      end
    else
      begin
    	if (bundledPlantPartsList.count > 0) then
      	for i := 0 to bundledPlantPartsList.count - 1 do
        	begin
        	plantPart := GsDrawingPlantPart(bundledPlantPartsList.items[i]);
        	plantPart.isBundled := false;
        	end;
    	soilPatch.addPlant(plant);
    	{do soil patch model values change when add/remove plant?}
    	GardenForm.modelUndeleted(plant);
      end;
    end;
  inherited undoCommand;
  end;

procedure GsHarvestPlantCommand.redoCommand;
  var
    plantPart: GsDrawingPlantPart;
		i: longint;
  begin
  if pullPlant then
    begin
    if plantIsKeptForReseeding then
      begin
      plant.awaitingReseeding := true;
      soilPatch.updatePlantMeans;
      BrowserForm.modelDeleted(plant);
      end
    else
      begin
    	if (bundledPlantPartsList.count > 0) then
      	for i := 0 to bundledPlantPartsList.count - 1 do
        	begin
        	plantPart := GsDrawingPlantPart(bundledPlantPartsList.items[i]);
        	plantPart.isBundled := true;
        	end;
    	{do soil patch model values change when add/remove plant?}
    	soilPatch.removePlant(plant);
    	GardenForm.modelDeleted(plant);
      end;
    end;
  inherited redoCommand;  {redo in this case as inherited will call inherited do}
  end;

function GsHarvestPlantCommand.description: string;
	begin
  if plant = nil then
    result := 'harvest plant'
  else
    result := 'harvest ' + plant.getName;
  result := limitDescription(result);
  end;

procedure GsHarvestPlantCommand.addPlantsInUseNotInGarden(inUseList: TList);
	begin
  if done and pullPlant then
    addToListIfAbsent(inUseList, plant);
  end;

{ ------------------------------------------------------------ GsChangeModelNameCommand }
constructor GsChangeModelNameCommand.createCommand(aModel: GsModel; aNewValue: string);
	begin
  create;
  model := aModel;
  newValue := aNewValue;
  oldValue := model.getName;
  end;

procedure GsChangeModelNameCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  model.setName(newValue);
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  BrowserForm.updateObjectChoicesWithPointer; { usually uses name }
  inherited doCommand;
  end;

procedure GsChangeModelNameCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  model.setName(oldValue);
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  BrowserForm.updateObjectChoicesWithPointer; { usually uses name }
  inherited undoCommand;
  end;

function GsChangeModelNameCommand.description: string;
	begin
  result := 'rename ' + oldValue + ' to ' + newValue;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsDeleteTemplateCommand }
constructor GsDeleteTemplateCommand.createCommand(aTemplate: GsModel);
	begin
  create;
  template := aTemplate;
  end;

destructor GsDeleteTemplateCommand.destroy;
  begin
  if done then
    template.free;
  template := nil;
  end;

procedure GsDeleteTemplateCommand.doCommand;
  begin
  if (Domain = nil) or (Domain.templateManager = nil) then exit;
  Domain.templateManager.removeTemplate(template);
  BrowserForm.updateObjectChoicesForDeletedObject;
  inherited doCommand;
  end;

procedure GsDeleteTemplateCommand.undoCommand;
  begin
  if (Domain = nil) or (Domain.templateManager = nil) then exit;
  if template is GsWeather then
    begin
    if Domain.templateManager.climateList <> nil then
      Domain.templateManager.climateList.add(template);
    end
  else if template is GsSoilPatch then
    begin
    if Domain.templateManager.soilTypeList <> nil then
      Domain.templateManager.soilTypeList.add(template);
    end
  else if template is GsPlant then
    begin
    if Domain.templateManager.cultivarList <> nil then
      Domain.templateManager.cultivarList.add(template);
    end
  else if template is GsBag then
    begin
    if Domain.templateManager.bagList <> nil then
      Domain.templateManager.bagList.add(template);
    end;
  BrowserForm.updateObjectChoicesWithPointer; 
  BrowserForm.selectTemplate(template);
  inherited undoCommand;
  end;

function GsDeleteTemplateCommand.description: string;
	begin
  if template = nil then
    result := 'delete template'
  else
    result := 'delete template ' + template.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsScanCommand }
{probably no longer needed}
function GsScanCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    self.updateScanAt(nextPoint);
    end
  else if aTrackPhase = trackMove then
  	begin
    self.updateScanAt(nextPoint);
  	end
  else if aTrackPhase = trackRelease then
  	begin
    {GardenForm.status.caption := '';}
    result := nil;
    self.free;
  	end;
	end;

procedure GsScanCommand.updateScanAt(point: TPoint);
	begin
{  soilPatch := Domain.garden.findSoilPatch(point) as GsSoilPatch;
  if soilPatch <> nil then plant := soilPatch.findPlant(point) as GsPlant
  else plant := nil;
  if plant <> nil then GardenForm.status.caption := 'plant actual size: ' + IntToStr(plant.actualSize)
  else if soilPatch <> nil then GardenForm.status.caption := 'soil patch size: ' + 'as yet unknown yet'
  else GardenForm.status.caption := 'weather status: ' + 'fine';
  }
  end;

{ ------------------------------------------------------------ GsPickUpToolCommand }
function GsPickUpToolCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    toolRect : TRect;
    carriedTool: GsTool;
    moveMulchCommand: GsMoveMulchCommand;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	carriedTool := Domain.toolManager.findToolAtXY(nextPoint.x, nextPoint.y, nil);
    carriedToolObject := carriedTool;
  	if (carriedTool <> nil) and (Domain.menuOptions.showTools) then
   		begin
    	Domain.toolManager.bringToolToFront(carriedTool);
    	toolRect := carriedTool.boundsRect;
    	GardenForm.invalidateToolRect(toolRect);
    	end;
    if carriedTool = nil then
      begin
    	result := nil;
    	self.free;
    	moveMulchCommand := GsMoveMulchCommand.create;
      result := moveMulchCommand.TrackMouse(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	carriedTool := carriedToolObject as GsTool;
    if (carriedTool <> nil)  and (Domain.menuOptions.showTools) and
      ((previousPoint.x <> nextPoint.x) or (previousPoint.y <> nextPoint.y)) then
  		begin
    	toolRect := carriedTool.boundsRect;
    	GardenForm.invalidateToolRect(toolRect);
    	carriedTool.x := carriedTool.x + nextPoint.x - previousPoint.x;
    	carriedTool.y := carriedTool.y + nextPoint.y - previousPoint.y;
    	toolRect := carriedTool.boundsRect;
    	GardenForm.invalidateToolRect(toolRect);
   	 	end;
  	end
  else if aTrackPhase = trackRelease then
  	begin
    result := nil;
    self.free;
  	end;
	end;

{ ------------------------------------------------------------ GsMoveMulchCommand }
function GsMoveMulchCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    theRect : TRect;
    carriedMulch: GsOrganicMatter;
    originalSoilPatch: GsSoilPatch;
    movedToSoilPatch: GsSoilPatch;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	carriedMulch := Domain.garden.findMulchAtXY(nextPoint.x, nextPoint.y) as GsOrganicMatter;
    originalSoilPatch := Domain.garden.findSoilPatchForMulchAtXY(nextPoint.x, nextPoint.y) as GsSoilPatch;
    carriedMulchObject := carriedMulch;
    originalSoilPatchObject := originalSoilPatch;
  	if (carriedMulch <> nil) then
      begin
      theRect := carriedMulch.boundsRect;
    	GardenForm.invalidateToolRect(theRect);
      originalPoint := carriedMulch.basePoint;
      end
    else
      begin
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	carriedMulch := carriedMulchObject as GsOrganicMatter;
    if (carriedMulch <> nil) and
      ((previousPoint.x <> nextPoint.x) or (previousPoint.y <> nextPoint.y)) then
  		begin
    	theRect := carriedMulch.boundsRect;
    	GardenForm.invalidateToolRect(theRect);
      carriedMulch.setPosition(Point(
      		carriedMulch.basePoint.x + nextPoint.x - previousPoint.x,
      		carriedMulch.basePoint.y + nextPoint.y - previousPoint.y));
    	theRect := carriedMulch.boundsRect;
    	GardenForm.invalidateToolRect(theRect);
   	 	end;
  	end
  else if aTrackPhase = trackRelease then
  	begin
  	carriedMulch := carriedMulchObject as GsOrganicMatter;
    originalSoilPatch := originalSoilPatchObject as GsSoilPatch;
    movedToSoilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
    if movedToSoilPatch = nil then
      begin
    	theRect := carriedMulch.boundsRect;
    	GardenForm.invalidateToolRect(theRect);
      carriedMulch.setPosition(originalPoint);
    	theRect := carriedMulch.boundsRect;
    	GardenForm.invalidateToolRect(theRect);
      result := nil;
      self.free;
      end
    else if movedToSoilPatch = originalSoilPatch then {can't undo move...}
      begin
      result := nil;
      self.free;
      end
    else {different patches...}
      begin
      movedToPoint := carriedMulch.basePoint;
      movedToSoilPatchObject := movedToSoilPatch;
      end;
  	end;
	end;

procedure GsMoveMulchCommand.doCommand;
  var
    carriedMulch: GsOrganicMatter;
    originalSoilPatch: GsSoilPatch;
    movedToSoilPatch: GsSoilPatch;
    originalSoilColor, movedToSoilColor: TColorRef;
  begin
  inherited doCommand;
  carriedMulch := carriedMulchObject as GsOrganicMatter;
  originalSoilPatch := originalSoilPatchObject as GsSoilPatch;
  movedToSoilPatch := movedToSoilPatchObject as GsSoilPatch;
  originalSoilColor := originalSoilPatch.surfaceColor;
  movedToSoilColor :=  movedToSoilPatch.surfaceColor;
  carriedMulch.transferToOrFromSoil(originalSoilPatch, kFromSoil);
  carriedMulch.transferToOrFromSoil(movedToSoilPatch, kToSoil);
  originalSoilPatch.looseOrganicMatterList.remove(carriedMulch);
  movedToSoilPatch.looseOrganicMatterList.add(carriedMulch);
  originalSoilPatchColorChanges := originalSoilColor <> originalSoilPatch.surfaceColor;
  movedToSoilPatchColorChanges := movedToSoilColor <> movedToSoilPatch.surfaceColor;
  self.updateGardenAndBrowser;
  end;

procedure GsMoveMulchCommand.undoCommand;
  var
    theRect : TRect;
    carriedMulch: GsOrganicMatter;
    originalSoilPatch: GsSoilPatch;
    movedToSoilPatch: GsSoilPatch;
  begin
  inherited undoCommand;
  carriedMulch := carriedMulchObject as GsOrganicMatter;
  originalSoilPatch := originalSoilPatchObject as GsSoilPatch;
  movedToSoilPatch := movedToSoilPatchObject as GsSoilPatch;
  carriedMulch.transferToOrFromSoil(movedToSoilPatch, kFromSoil);
  carriedMulch.transferToOrFromSoil(originalSoilPatch, kToSoil);
  theRect := carriedMulch.boundsRect;
  GardenForm.invalidateToolRect(theRect);
  carriedMulch.setPosition(originalPoint);
  theRect := carriedMulch.boundsRect;
  movedToSoilPatch.looseOrganicMatterList.remove(carriedMulch);
  originalSoilPatch.looseOrganicMatterList.add(carriedMulch);
  GardenForm.invalidateToolRect(theRect);
  self.updateGardenAndBrowser;
  end;

procedure GsMoveMulchCommand.redoCommand;
  var
    theRect : TRect;
    carriedMulch: GsOrganicMatter;
    originalSoilPatch: GsSoilPatch;
    movedToSoilPatch: GsSoilPatch;
  begin
  inherited doCommand; {not inherited redo!}
  carriedMulch := carriedMulchObject as GsOrganicMatter;
  originalSoilPatch := originalSoilPatchObject as GsSoilPatch;
  movedToSoilPatch := movedToSoilPatchObject as GsSoilPatch;
  carriedMulch.transferToOrFromSoil(originalSoilPatch, kFromSoil);
  carriedMulch.transferToOrFromSoil(movedToSoilPatch, kToSoil);
  theRect := carriedMulch.boundsRect;
  GardenForm.invalidateToolRect(theRect);
  carriedMulch.setPosition(movedToPoint);
  theRect := carriedMulch.boundsRect;
  originalSoilPatch.looseOrganicMatterList.remove(carriedMulch);
  movedToSoilPatch.looseOrganicMatterList.add(carriedMulch);
  GardenForm.invalidateToolRect(theRect);
  self.updateGardenAndBrowser;
  end;

procedure GsMoveMulchCommand.updateGardenAndBrowser;
  begin
	BrowserForm.updateModelValuesForModel(originalSoilPatchObject as GsSoilPatch);
	BrowserForm.updateModelValuesForModel(movedToSoilPatchObject as GsSoilPatch);
  {should really do these - but will slow things down when have plants..}
  if originalSoilPatchColorChanges then
  	GardenForm.invalidateGardenRect((originalSoilPatchObject as GsSoilPatch).boundsRect);
  if movedToSoilPatchColorChanges then
  	GardenForm.invalidateGardenRect((movedToSoilPatchObject as GsSoilPatch).boundsRect);
  end;

function GsMoveMulchCommand.description: string;
	begin
  result := 'drag organic matter';
  if originalSoilPatchObject <> nil then
    result := result + ' from ' + (originalSoilPatchObject as GsSoilPatch).getName;
  if movedToSoilPatchObject <> nil then
    result := result + ' to ' + (movedToSoilPatchObject as GsSoilPatch).getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsDuplicateCommand }
destructor GsDuplicateCommand.destroy;
  begin
  if not done then
    begin
    if soilPatchCopy <> nil then
      begin
      soilPatchCopy.free;
      soilPatchCopy := nil;
      end;
    if plantCopy <> nil then
      begin
      plantCopy.free;
      plantCopy := nil;
      end;
    end;
  inherited destroy;
  end;

function GsDuplicateCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    newSoilPatch: GsSoilPatch;
    delta, newPoint: TPoint;
    i: longint;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
    plant := nil;
    plantCopy := nil;
    soilPatch := nil;
    soilPatchCopy := nil;
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      exit;
      end;
    plant := soilPatch.findPlant(nextPoint) as GsPlant;
    if plant = nil then
      begin
      { duplicate soil patch }
      soilPatchCopy := GsSoilPatch.create;
      { soil patch streaming does plant pointer fixing up - so don't have to here }
      soilPatch.copyTo(soilPatchCopy);
      { copyTo does not stream garden pointer, so must fix up }
      soilPatchCopy.garden := Domain.garden;
      self.doCommand;
      end
    else
      begin
      { duplicate plant }
      plantCopy := GsPlant.create;
      plant.copyTo(plantCopy);
      { soil pointer is not streamed - must fix up }
      plantCopy.soil := plant.soil;
      plantOffset.x := plantCopy.basePoint.x - nextPoint.x;
      plantOffset.y := plantCopy.basePoint.y - nextPoint.y;
      self.doCommand;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
    if not done then exit;
  	if mouseDidMove then
      begin
      if soilPatchCopy <> nil then
        begin
    	  GardenForm.invalidateGardenRect(soilPatchCopy.extendedBoundsRect);
        delta := Point(nextPoint.x - previousPoint.x, nextPoint.y - previousPoint.y);
  		  soilPatchCopy.moveBy(delta);
  		  GardenForm.invalidateGardenRect(soilPatchCopy.extendedBoundsRect);
        end
      else if plantCopy <> nil then
        begin
        newPoint.x := nextPoint.x + plantOffset.x;
        newPoint.y := nextPoint.y + plantOffset.y;
        newSoilPatch := Domain.garden.findSoilPatch(newPoint) as GsSoilPatch;
        { want to keep plant inside soil patch - too complicated to transplant here }
        if newSoilPatch = soilPatch then
          begin
  		    GardenForm.invalidateGardenRect(plantCopy.boundsRect);
 		 	    plantCopy.moveTo(newPoint);
  		    GardenForm.invalidateGardenRect(plantCopy.boundsRect);
          end;
        end;
      end;
  	end
  else if aTrackPhase = trackRelease then
  	begin
    if not done then exit;
    { enforce minimum movement distance so copy can be seen }
    if soilPatchCopy <> nil then
      begin
      { since soil patch cannot be shaped during drag, only need check one corner }
      if (soilPatchCopy.corners[0].x = soilPatch.corners[0].x)
          and (soilPatchCopy.corners[0].y = soilPatch.corners[0].y) then
        begin
  		  GardenForm.invalidateGardenRect(soilPatchCopy.extendedBoundsRect);
        soilPatchCopy.moveBy(point(10, 10));
  		  GardenForm.invalidateGardenRect(soilPatchCopy.extendedBoundsRect);
        end;
      end
    else if plantCopy <> nil then
      begin
      if (plantCopy.basePoint.x = plant.basePoint.x)
          and (plantCopy.basePoint.y = plant.basePoint.y) then
        begin
        GardenForm.invalidateGardenRect(plantCopy.boundsRect);
        plantCopy.moveTo(point(plant.basePoint.x + 10, plant.basePoint.y + 10));
        GardenForm.invalidateGardenRect(plantCopy.boundsRect);
        end;
      end;
  	end;
	end;

procedure GsDuplicateCommand.doCommand;
  var
    i: longint;
    originalName: string;
  begin
  { may have been done in the mouse down }
  if self.done then exit;
  inherited doCommand;
  { tell garden about copy }
  if soilPatchCopy <> nil then
    begin
    inc(Domain.garden.numSoilPatchesCreated);
    { set names here, because garden number created is incremented here }
    giveNewSoilPatchUniqueNameBasedOn(soilPatchCopy, ' (Copy of ' + soilPatch.getName + ')');
    if soilPatchCopy.plantList.count > 0 then
      for i := 0 to soilPatchCopy.plantList.count - 1 do
        begin
        plant := GsPlant(soilPatchCopy.plantList.items[i]);
        inc(Domain.garden.numPlantsCreated);
        originalName := plant.getName;
        giveNewPlantUniqueNameBasedOn(plant, ' (Copy of ' + originalName + ')');
        end;
    Domain.garden.addSoilPatch(soilPatchCopy);
    GardenForm.invalidateGardenRect(soilPatchCopy.extendedBoundsRect);
    GardenForm.modelAddedOrRemoved;
    end
  else if plantCopy <> nil then
    begin
    inc(Domain.garden.numPlantsCreated);
    originalName := plant.getName;
    giveNewPlantUniqueNameBasedOn(plant, ' (Copy of ' + originalName + ')');
    soilPatch.addPlant(plantCopy);
    GardenForm.invalidateGardenRect(plantCopy.boundsRect);
    GardenForm.modelAddedOrRemoved;
    end;
  end;

procedure GsDuplicateCommand.undoCommand;
  begin
  inherited undoCommand;
  { tell garden about to forget about copy }
  if soilPatchCopy <> nil then
    begin
    dec(Domain.garden.numSoilPatchesCreated);
    Domain.garden.removeSoilPatch(soilPatchCopy);
    GardenForm.invalidateGardenRect(soilPatchCopy.extendedBoundsRect);
    GardenForm.modelAddedOrRemoved;
    end
  else if plantCopy <> nil then
    begin
    dec(Domain.garden.numPlantsCreated);
    soilPatch.removePlant(plantCopy);
    GardenForm.invalidateGardenRect(plantCopy.boundsRect);
    GardenForm.modelAddedOrRemoved;
    end;
  end;

function GsDuplicateCommand.description: string;
	begin
  result := 'duplicate';
  if soilPatchCopy <> nil then
    begin
    if soilPatch <> nil then
      result := 'duplicate ' + soilPatch.getName;
    end
  else if plantCopy <> nil then
    begin
    if plant <> nil then
      result := 'duplicate ' + plant.getName;
    end;
  result := limitDescription(result);
  end;

procedure GsDuplicateCommand.addPlantsInUseNotInGarden(inUseList: TList);
  var
    i: longint;
	begin
  if (not done) then
    begin
    if (soilPatchCopy <> nil) then
      begin
      if (soilPatchCopy.plantList.count > 0) then
    	  for i := 0 to soilPatchCopy.plantList.count - 1 do
      	  addToListIfAbsent(inUseList, soilPatchCopy.plantList[i]);
      end
    else if plantCopy <> nil then
      addToListIfAbsent(inUseList, plantCopy);
    end;
  end;

{ ------------------------------------------------------------ GsSoilOperationCommand }
function GsSoilOperationCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  try
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      end
    else
      begin
  		originalSoilPatchCopyWithoutPlants := GsSoilPatch.create;
   		changedSoilPatchCopyWithoutPlants := GsSoilPatch.create;
      soilPatch.copyToWithoutSubobjects(originalSoilPatchCopyWithoutPlants);
      soilPatch.copyToWithoutSubobjects(changedSoilPatchCopyWithoutPlants);
      {do something to soil}
    	self.doSoilOperation(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
    self.doSoilOperation(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
  	end
  else if aTrackPhase = trackRelease then
  	begin
    self.doSoilOperation(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
    end;
  except
  	result := nil;
  	self.free;
  	ShowMessage('Problem doing that command');
  end;
  end;

procedure GsSoilOperationCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  begin
  {subclasses should override}
  end;

procedure GsSoilOperationCommand.doCommand;
  begin
  inherited doCommand;
  changedSoilPatchCopyWithoutPlants.copyToWithoutSubobjects(soilPatch);
  self.updateGardenAndBrowser;
  end;

procedure GsSoilOperationCommand.redoCommand;
  begin
  inherited doCommand; {not inherited redo!}
  changedSoilPatchCopyWithoutPlants.copyToWithoutSubobjects(soilPatch);
  self.updateGardenAndBrowser;
  end;

procedure GsSoilOperationCommand.undoCommand;
  begin
  inherited undoCommand;
  originalSoilPatchCopyWithoutPlants.copyToWithoutSubobjects(soilPatch);
  self.updateGardenAndBrowser;
  end;

procedure GsSoilOperationCommand.updateGardenAndBrowser;
  var
  	i: longint;
  begin
	BrowserForm.updateModelValuesForModel(soilPatch);
  GardenForm.invalidateGardenRect(soilPatch.boundsRect);
  end;

function GsSoilOperationCommand.description: string;
	begin
  { subclasses should override }
  result := 'soil operation - description not overridden';
  end;

destructor GsSoilOperationCommand.destroy;
  begin
  originalSoilPatchCopyWithoutPlants.free;
  originalSoilPatchCopyWithoutPlants := nil;
  changedSoilPatchCopyWithoutPlants.free;
  changedSoilPatchCopyWithoutPlants := nil;
  inherited destroy;
  end;

{ ------------------------------------------------------------ GsCultivateCommand }
const
	kCultivateDefaultArea_m2 = 0.1;
  kCultivateDefaultDepth_m = 0.1;
  kCultivateMaxEfficiency_frn = 1.0;

procedure GsCultivateCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    area_ha, depth_m: single;
    efficiency_frn, randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m: single;
 	begin
  if aTrackPhase = trackPress then
  	begin
    if paramObject1 <> nil then
      depth_m := paramObject1.value
    else
      depth_m := kCultivateDefaultDepth_m;
    if paramObject2 <> nil then
      area_ha := paramObject2.value * m2_to_ha
    else
      area_ha := kCultivateDefaultArea_m2 * m2_to_ha;
    efficiency_frn := safedivExcept(area_ha, changedSoilPatchCopyWithoutPlants.params.area_ha, 0.0)
        * kCultivateMaxEfficiency_frn;
    randomRoughness_mm := 23.0 * efficiency_frn + 8.0;
    ridgeHeight_mm := 0.24 * depth_m * m_to_mm + 16.0;
    ridgeInterval_m := ridgeHeight_mm * 0.0075;
		SoilOp.mixSoil(changedSoilPatchCopyWithoutPlants, area_ha, depth_m, kCultivateMaxEfficiency_frn,
    	randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m, false);
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  end;

function GsCultivateCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'cultivate soil patch'
  else
    result := 'cultivate ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsTwoSoilPatchOperationCommand }
function GsTwoSoilPatchOperationCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  begin
  result := self;
  try
  if aTrackPhase = trackPress then
  	begin
    result := inherited trackMouse(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
    end
  else if aTrackPhase = trackMove then
  	begin
    self.doSoilOperation(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
  	end
  else if aTrackPhase = trackRelease then
  	begin
    secondSoilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if secondSoilPatch = nil then
      begin
      result := nil;
      self.free;
      end
    else
      begin
  		originalSecondSoilPatchCopyWithoutPlants := GsSoilPatch.create;
   		changedSecondSoilPatchCopyWithoutPlants := GsSoilPatch.create;
      secondSoilPatch.copyToWithoutSubobjects(originalSecondSoilPatchCopyWithoutPlants);
      secondSoilPatch.copyToWithoutSubobjects(changedSecondSoilPatchCopyWithoutPlants);
      {move something between two soil patches }
    	self.doSoilOperation(aTrackPhase, anchorPoint, previousPoint, nextPoint, mouseDidMove);
      end;
    end;
  except
  	result := nil;
  	self.free;
  	ShowMessage('Problem doing that command');
  end;
  end;

procedure GsTwoSoilPatchOperationCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint,
    nextPoint: TPoint; mouseDidMove: boolean);
  begin
  {subclasses should override}
  end;

procedure GsTwoSoilPatchOperationCommand.doCommand;
  begin
  inherited doCommand;   { copies first soil patch }
  changedSecondSoilPatchCopyWithoutPlants.copyToWithoutSubobjects(secondSoilPatch);
  self.updateGardenAndBrowser;
  end;

procedure GsTwoSoilPatchOperationCommand.redoCommand;
  begin
  inherited doCommand; {not inherited redo!} { copies first soil patch }
  changedSecondSoilPatchCopyWithoutPlants.copyToWithoutSubobjects(secondSoilPatch);
  self.updateGardenAndBrowser;
  end;

procedure GsTwoSoilPatchOperationCommand.undoCommand;
  begin
  inherited undoCommand; { copies first soil patch }
  originalSecondSoilPatchCopyWithoutPlants.copyToWithoutSubobjects(secondSoilPatch);
  self.updateGardenAndBrowser;
  end;

procedure GsTwoSoilPatchOperationCommand.updateGardenAndBrowser;
  begin
	BrowserForm.updateModelValuesForModel(secondSoilPatch);
  GardenForm.invalidateGardenRect(secondSoilPatch.boundsRect);
  end;

function GsTwoSoilPatchOperationCommand.description: string;
	begin
  { subclasses should override }
  result := 'soil operation - description not overriden';
  end;

destructor GsTwoSoilPatchOperationCommand.destroy;
  begin
  if (not application.terminated) and (GardenForm <> nil)
  		and (GardenForm.currentTool <> nil) then
    begin
  	if GardenForm.currentTool.state = kToolStateFull then
    	begin
  		GardenForm.invalidateCurrentToolRect;
    	GardenForm.currentTool.setState(kToolStateUp);
  		GardenForm.invalidateCurrentToolRect;
      end;
    end;
  originalSecondSoilPatchCopyWithoutPlants.free;
  originalSecondSoilPatchCopyWithoutPlants := nil;
  changedSecondSoilPatchCopyWithoutPlants.free;
  changedSecondSoilPatchCopyWithoutPlants := nil;
  inherited destroy;
  end;

{ ------------------------------------------------------------ GsCarrySoilCommand }
const
  kCarrySoilDefaultAmount_tPha = 1.0;
  kCarrySoilDefaultDepth_m = 0.3;

procedure GsCarrySoilCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    depth_m, amount_tPha: single;
    carriedSoil: LayerStructure;
 	begin
  try
  if aTrackPhase = trackPress then
    begin
  	GardenForm.invalidateCurrentToolRect;
    GardenForm.currentTool.setState(kToolStateFull);
  	GardenForm.invalidateCurrentToolRect;
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    { don't do anything if released mouse on same patch }
    if soilPatch <> secondSoilPatch then
      begin
      amount_tPha := 0.0;
      if paramObject1 <> nil then
        begin
        if paramObject1.unitSet = kMass then
          begin
          if changedSoilPatchCopyWithoutPlants.params.area_ha <> 0 then
            amount_tPha := safediv(paramObject1.value,
                changedSoilPatchCopyWithoutPlants.params.area_ha) * kg_to_t
          else
            amount_tPha := 0.0;
          end
        else if paramObject1.unitSet = kMassOverArea then
          amount_tPha := paramObject1.value * kg_to_t;
        end;
      if amount_tPha = 0 then amount_tPha := kCarrySoilDefaultAmount_tPha;
      if paramObject2 <> nil then
        carriedSoil.depth_m := paramObject2.value
      else
        carriedSoil.depth_m := kCarrySoilDefaultDepth_m;
      SoilOp.removeSoil(changedSoilPatchCopyWithoutPlants, amount_tPha, carriedSoil);
      SoilOp.addSoil(changedSecondSoilPatchCopyWithoutPlants, carriedSoil.weight_tPha, carriedSoil);
      end;
  	GardenForm.invalidateCurrentToolRect;
    GardenForm.currentTool.setState(kToolStateUp);
  	GardenForm.invalidateCurrentToolRect;
    end;
  except
    errorMessage('Problem in GsCarrySoilCommand.doSoilOperation');
  end;
  end;

function GsCarrySoilCommand.description: string;
	begin
  result := 'carry soil';
  if soilPatch <> nil then
    result := result + ' from ' + soilPatch.getName;
  if secondSoilPatch <> nil then
    result := result + ' to ' + secondSoilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsCarryMulchCommand }
const
  kCarryMulchDefaultAmount_tPha = 0.5;

procedure GsCarryMulchCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    amount_tPha: single;
    carriedMulch: MulchStructure;
 	begin
  try
  if aTrackPhase = trackPress then
    begin
  	GardenForm.invalidateCurrentToolRect;
    GardenForm.currentTool.setState(kToolStateFull);
  	GardenForm.invalidateCurrentToolRect;
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    { don't do anything if released mouse on same patch }
    if soilPatch <> secondSoilPatch then
      begin
      amount_tPha := 0.0;
      carriedMulch.flatCropResidue_tPha := 0.0;
      carriedMulch.organicNFresh_kgPha := 0.0;
      carriedMulch.organicPFresh_kgPha := 0.0;
      if paramObject <> nil then
        begin
        if paramObject.unitSet = kMass then
          begin
          if changedSoilPatchCopyWithoutPlants.params.area_ha <> 0 then
            amount_tPha := safediv(paramObject.value,
                changedSoilPatchCopyWithoutPlants.params.area_ha) * kg_to_t
          else
            amount_tPha := 0.0;
          end
        else if paramObject.unitSet = kMassOverArea then
          amount_tPha := paramObject.value * kg_to_t;
        end;
      if amount_tPha = 0 then amount_tPha := kCarryMulchDefaultAmount_tPha;
      SoilOp.removeMulch(changedSoilPatchCopyWithoutPlants, amount_tPha, carriedMulch);
      SoilOp.addMulch(changedSecondSoilPatchCopyWithoutPlants, carriedMulch);
      end;
  	GardenForm.invalidateCurrentToolRect;
    GardenForm.currentTool.setState(kToolStateUp);
  	GardenForm.invalidateCurrentToolRect;
    end;
  except
    errorMessage('Problem in GsCarryMulchCommand.doSoilOperation');
  end;
  end;

function GsCarryMulchCommand.description: string;
	begin
  result := 'carry mulch';
  if soilPatch <> nil then
    result := result + ' from ' + soilPatch.getName;
  if secondSoilPatch <> nil then
    result := result + ' to ' + secondSoilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsMixSoilCommand }
const
	kMixSoilDefaultArea_m2 = 0.5;
  kMixSoilDefaultDepth_m = 0.3;
  kMixSoilMaxEfficiency_frn = 1.0;

procedure GsMixSoilCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    area_ha, depth_m: single;
    efficiency_frn, randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m: single;
 	begin
  if aTrackPhase = trackPress then
  	begin
    if paramObject1 <> nil then
      depth_m := paramObject1.value
    else
      depth_m := kMixSoilDefaultDepth_m;
    if paramObject2 <> nil then
      area_ha := paramObject2.value * m2_to_ha
    else
      area_ha := kMixSoilDefaultArea_m2 * m2_to_ha;
    efficiency_frn := safedivExcept(area_ha, changedSoilPatchCopyWithoutPlants.params.area_ha, 0.0)
        * kCultivateMaxEfficiency_frn;
    randomRoughness_mm := 23.0 * efficiency_frn + 8.0;
    ridgeHeight_mm := 0.24 * depth_m * m_to_mm + 16.0;
    ridgeInterval_m := ridgeHeight_mm * 0.0075;
		SoilOp.mixSoil(changedSoilPatchCopyWithoutPlants, area_ha, depth_m, kMixSoilMaxEfficiency_frn,
    	randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m, false);
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  end;

function GsMixSoilCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'mix soil in soil patch'
  else
    result := 'mix soil in ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsDoubleDigCommand }
const
  kDoubleDigDepth_m = 0.6;
  kDoubleDigMaxEfficiency_frn = 0.9;

procedure GsDoubleDigCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m: single;
 	begin
  if aTrackPhase = trackPress then
  	begin
    randomRoughness_mm := 23.0 * kDoubleDigMaxEfficiency_frn + 8.0;
    ridgeHeight_mm := 0.24 * kDoubleDigDepth_m * m_to_mm + 16.0;
    ridgeInterval_m := ridgeHeight_mm * 0.0075;
		SoilOp.mixSoil(changedSoilPatchCopyWithoutPlants, soilPatch.params.area_ha,
        kDoubleDigDepth_m, kDoubleDigMaxEfficiency_frn,
    	  randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m, false);
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  end;

function GsDoubleDigCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'double-dig soil patch'
  else
    result := 'double-dig ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsAerateCommand }
const
  kAerateSoilDefaultDepth_m = 0.1;
  kAerateSoilDefaultArea_m2 = 0.1;

procedure GsAerateCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    depth_m, area_ha: single;
 	begin
  if aTrackPhase = trackPress then
  	begin
    depth_m := 0.0;
    if paramObject1 <> nil then
      depth_m := paramObject1.value
    else
      depth_m := kAerateSoilDefaultDepth_m;
    if paramObject2 <> nil then
      area_ha := paramObject2.value * m2_to_ha
    else
      area_ha := kAerateSoilDefaultArea_m2 * m2_to_ha;
		SoilOp.aerateSoil(changedSoilPatchCopyWithoutPlants, depth_m, area_ha);
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  end;

function GsAerateCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'aerate soil patch'
  else
    result := 'aerate ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsWaterCommand }
const
  kWaterDefaultAmount_mm = 20.0;
  liters_to_m3 = 0.001;

procedure GsWaterCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    amountAbsolute_liters, amountAbsolute_m3, amountRelative_mm: single;
  {could simplify to just store amount added - not entire patch}
 	begin
  try
  if aTrackPhase = trackPress then
  	begin
    amountRelative_mm := 0.0;
    if paramObject <> nil then
      begin
      if paramObject.unitSet = kVolume then
        begin
        amountAbsolute_liters := paramObject.value;
        amountAbsolute_m3 := amountAbsolute_liters * liters_to_m3;
        if changedSoilPatchCopyWithoutPlants.params.area_ha <> 0 then
          amountRelative_mm := safediv(amountAbsolute_m3,
              changedSoilPatchCopyWithoutPlants.params.area_ha * ha_to_m2) * m_to_mm
        else
          amountRelative_mm := 0.0;
        end
      else if paramObject.unitSet = kDepthOfWater then
        begin
        amountRelative_mm := paramObject.value;
        end;
      end;
    if amountRelative_mm = 0.0 then amountRelative_mm := kWaterDefaultAmount_mm;
    { bound at zero and max possible water per auto irrigation event; stops garbled number from causing trouble }
    amountRelative_mm := max(0.0,
        min(changedSoilPatchCopyWithoutPlants.params.maxApplicVolumeAutoIrr_mm, amountRelative_mm));
		SoilOp.irrigate(changedSoilPatchCopyWithoutPlants, amountRelative_mm, false);
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  except
    errorMessage('Problem in GsWaterCommand.doSoilOperation');
  end;
  end;

function GsWaterCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'water soil patch'
  else
    result := 'water ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsApplyAmendmentCommand }
const
  kBagDefaultAmountSolid_tPha = 1.0;
  kBagDefaultAmountLiquid_mm = 1.0;

procedure GsApplyAmendmentCommand.doSoilOperation(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean);
  var
    materialType: smallint;
    amountAbsoluteLiquid_liters, amountAbsoluteLiquid_m3, amountRelativeLiquid_mm: single;
    amountAbsoluteSolid_kg, amountRelativeSolid_tPha: single;
 	begin
  try
  if aTrackPhase = trackPress then
  	begin
    if (bag = nil) or (not (bag is GsBag)) then
      begin
      raise Exception.create('GsApplyAmendmentCommand.doSoilOperation: no bag');
      exit;
      end;
    materialType := kSolid;
    amountRelativeLiquid_mm := 0.0;
    amountRelativeSolid_tPha := 0.0;
    if paramObject <> nil then
      begin
      materialType := paramObject.materialType;
      if materialType = kSolid then
        begin
        if paramObject.unitSet = kMass then
          begin
          amountAbsoluteSolid_kg := paramObject.value;
          if changedSoilPatchCopyWithoutPlants.params.area_ha <> 0 then
            amountRelativeSolid_tPha := safediv(amountAbsoluteSolid_kg,
                changedSoilPatchCopyWithoutPlants.params.area_ha) * kg_to_t
          else
            amountRelativeSolid_tPha := 0.0;
          end
        else if paramObject.unitSet = kMassOverArea then
          amountRelativeSolid_tPha := paramObject.value * kg_to_t;
        end
      else
        begin
        if paramObject.unitSet = kVolume then
          begin
          amountAbsoluteLiquid_liters := paramObject.value;
          amountAbsoluteLiquid_m3 := amountAbsoluteLiquid_liters * liters_to_m3;
          if changedSoilPatchCopyWithoutPlants.params.area_ha <> 0 then
            amountRelativeLiquid_mm := safediv(amountAbsoluteLiquid_m3,
                changedSoilPatchCopyWithoutPlants.params.area_ha * ha_to_m2) * m_to_mm
          else
            amountRelativeLiquid_mm := 0.0;
          end
        else if paramObject.unitSet = kDepthOfWater then
          amountRelativeLiquid_mm := paramObject.value;
        end;
      end;
    if materialType = kSolid then
      begin
      if amountRelativeSolid_tPha = 0.0 then amountRelativeSolid_tPha := kBagDefaultAmountSolid_tPha;
		  SoilOp.applyBag(changedSoilPatchCopyWithoutPlants, amountRelativeSolid_tPha, bag.contents);
      end
    else
      begin
      if amountRelativeLiquid_mm = 0.0 then amountRelativeLiquid_mm := kBagDefaultAmountLiquid_mm;
     { SoilOp.applyBagAsLiquid(changedSoilPatchCopyWithoutPlants, amountRelativeLiquid_mm, bag.contents); }
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  except
    errorMessage('Problem in GsApplyAmendmentCommand.doSoilOperation');
  end;
  end;

function GsApplyAmendmentCommand.description: string;
	begin
  if soilPatch = nil then
    result := 'apply amendment to soil patch'
  else
    result := 'apply amendment to ' + soilPatch.getName;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsSupportCommand }
function GsSupportCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    soilPatch: GsSoilPatch;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      exit;
      end;
    plant := soilPatch.findPlant(nextPoint) as GsPlant;
    if plant = nil then
      begin
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  end;

procedure GsSupportCommand.doCommand;
  begin
  inherited doCommand;
  if plant = nil then exit;
  wasSupportedBeforeDoing := plant.development.isSupported;
  plant.development.isSupported := not plant.development.isSupported;
  end;

procedure GsSupportCommand.undoCommand;
  begin
  inherited undoCommand;
  if plant = nil then exit;
  plant.development.isSupported := not plant.development.isSupported;
  end;

function GsSupportCommand.description: string;
	begin
  if wasSupportedBeforeDoing then
    begin
    if plant = nil then
      result := 'remove support from plant'
    else
      result := 'remove support from ' + plant.getName;
    end
  else
    begin
    if plant = nil then
      result := 'support plant'
    else
      result := 'support ' + plant.getName;
    end;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsStopOrStartPlantCommand }
function GsStopOrStartPlantCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  mouseDidMove: boolean): KfCommand;
  var
    soilPatch: GsSoilPatch;
  begin
  result := self;
  if aTrackPhase = trackPress then
  	begin
  	soilPatch := Domain.garden.findSoilPatch(nextPoint) as GsSoilPatch;
  	if soilPatch = nil then
      begin
      result := nil;
      self.free;
      exit;
      end;
    plant := soilPatch.findPlant(nextPoint) as GsPlant;
    if plant = nil then
      begin
      result := nil;
      self.free;
      end;
    end
  else if aTrackPhase = trackMove then
  	begin
  	end
  else if aTrackPhase = trackRelease then
  	begin
    end;
  end;

procedure GsStopOrStartPlantCommand.doCommand;
  begin
  inherited doCommand;
  if plant = nil then exit;
  wasStoppedBeforeDoing := plant.isInStasisBox;
  plant.isInStasisBox := not plant.isInStasisBox;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  end;

procedure GsStopOrStartPlantCommand.undoCommand;
  begin
  inherited undoCommand;
  if plant = nil then exit;
  plant.isInStasisBox := not plant.isInStasisBox;
  GardenForm.invalidateGardenRect(plant.boundsRect);
  end;

function GsStopOrStartPlantCommand.description: string;
  begin
  if not wasStoppedBeforeDoing then
    begin
    if plant = nil then
      result := 'place plant in stasis box'
    else
      result := 'place ' + plant.getName + ' in stasis box';
    end
  else
    begin
    if plant = nil then
      result := 'take plant out of stasis box'
    else
      result := 'take ' + plant.getName + ' out of stasis box';
    end;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsResetHarvestListCommand }
constructor GsResetHarvestListCommand.createCommand(aHarvestList: TListCollection);
	begin
  create;
  oldHarvestList := aHarvestList;
  end;

destructor GsResetHarvestListCommand.destroy;
  begin
  if done then
    begin
    oldHarvestList.free;
    oldHarvestList := nil;
    end
  else
    begin
    newHarvestList.free;
    newHarvestList := nil;
    end;
  inherited destroy;
  end;

procedure GsResetHarvestListCommand.doCommand;
  begin
  newHarvestList := TListCollection.create;
  Domain.harvestManager.harvestItemList := newHarvestList;
  { OK to do this with date because command list is cleared out when date changes }
  saveLastReportResetDate := Domain.harvestManager.lastReportResetDate;
  Domain.harvestManager.setLastReportResetDate(Domain.garden.date);
  GardenForm.updateHarvestPanel := true;
  BrowserForm.drawPlantHarvestItems;
  HarvestForm.handlePossibleHarvestListChange;
  inherited doCommand;
  end;

procedure GsResetHarvestListCommand.undoCommand;
  begin
  Domain.harvestManager.harvestItemList := oldHarvestList;
  Domain.harvestManager.setLastReportResetDate(saveLastReportResetDate);
  GardenForm.updateHarvestPanel := true;
  BrowserForm.drawPlantHarvestItems;
  HarvestForm.handlePossibleHarvestListChange;
  inherited undoCommand;
  end;

procedure GsResetHarvestListCommand.redoCommand;
  begin
  inherited doCommand; {not inherited redo!}
  Domain.harvestManager.harvestItemList := newHarvestList;
  Domain.harvestManager.setLastReportResetDate(Domain.garden.date);
  GardenForm.updateHarvestPanel := true;
  BrowserForm.drawPlantHarvestItems;
  HarvestForm.handlePossibleHarvestListChange;
  end;

function GsResetHarvestListCommand.description: string;
	begin
  result := 'reset all harvest reports';
  end;

{ ------------------------------------------------------------ GsChangeDomainCommand }
constructor GsChangeDomainCommand.createCommand(aModel: GsModel; aFieldID: smallint);
  begin
  self.create;
  model := aModel;
  fieldId := aFieldId;
  end;

function GsChangeDomainCommand.description: string;
  var
    aspect: GsAspect;
  begin
  result := '';
  { subclasses may want to call }
  if (Domain <> nil) and (Domain.aspectManager <> nil) then
    begin
    aspect := Domain.aspectManager.aspectForFieldNumber(fieldID);
    if (aspect <> nil) then
      result := aspect.aspectName;
    end;
  end;

procedure GsChangeDomainCommand.updatePlantBoundsIfNeeded;
  var
    plant: GsPlant;
    aspect: GsAspect;
  begin
  if model = nil then exit;
  if (model is GsPlant) then
    begin
    plant := model as GsPlant;
    aspect := nil;
    if (Domain <> nil) and (Domain.aspectManager <> nil) then
      aspect := Domain.aspectManager.aspectForFieldNumber(fieldID);
    if (not plant.isTemplate)
        and (aspect <> nil) and (aspect.objectType = kObjectTypeDrawingPlant)
        and (Domain.plantBitmap <> nil) and (Domain.plantBitmap.canvas <> nil) then
      begin
      plant.computeBounds := true;
      plant.drawOn(Domain.plantBitmap.canvas);
      plant.computeBounds := false;
      end;
    end;
  end;

{ ------------------------------------------------------------ GsChangeDomainRealCommand }
constructor GsChangeDomainRealCommand.createCommand(aModel: GsModel; aNewValue: single;
    aFieldID, aIndex, aDeriveMethod: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newValue := aNewValue;
  index := aIndex;
  deriveMethod := aDeriveMethod;
  {store old value}
  Domain.modelTransferField(model, oldValue, kGetField, fieldId, kFieldFloat, index, deriveMethod);
  end;

procedure GsChangeDomainRealCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newValue, kSetField, fieldId, kFieldFloat, index, deriveMethod);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainRealCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldValue, kSetField, fieldId, kFieldFloat, index, deriveMethod);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainRealCommand.description: string;
	begin
  result := inherited description;
  result := 'change ' + result;
  if index <> -1 then
    result := result + ' (' + intToStr(index + 1) + ')';
  result := result + ' from ' + digitValueString(oldValue) + ' to ' + digitValueString(newValue);
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsChangeDomainSCurvePointCommand }
constructor GsChangeDomainSCurvePointCommand.createCommand(aModel: GsModel;
		aNewX: single; aNewY: single; aFieldID: smallint; aPointIndex: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newX := aNewX;
  newY := aNewY;
  pointIndex := aPointIndex;
  {store old value}
  Domain.modelTransferField(model, oldX, kGetField, fieldId, kFieldFloat, pointIndex * 2, 0);
  Domain.modelTransferField(model, oldY, kGetField, fieldId, kFieldFloat, pointIndex * 2 + 1, 0);
  end;

procedure GsChangeDomainSCurvePointCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newX, kSetField, fieldId, kFieldFloat, pointIndex * 2, 0);
  Domain.modelTransferField(model, newY, kSetField, fieldId, kFieldFloat, pointIndex * 2 + 1, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainSCurvePointCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldX, kSetField, fieldId, kFieldFloat, pointIndex * 2, 0);
  Domain.modelTransferField(model, oldY, kSetField, fieldId, kFieldFloat, pointIndex * 2 + 1, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainSCurvePointCommand.description: string;
	begin
  result := inherited description;
  result := 'change ' + result + ' point ' + intToStr(pointIndex + 1)
    + ' from (' + digitValueString(oldX) + ', ' + digitValueString(oldY) + ')'
    + ' to (' + digitValueString(newX) + ', ' + digitValueString(newY) + ')';
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsChangeDomainColorCommand }    
constructor GsChangeDomainColorCommand.createCommand(aModel: GsModel; aNewColor: TColorRef;
  aFieldID: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newColor := aNewColor;
  {store old value}
  Domain.modelTransferField(model, oldColor, kGetField, fieldId, kFieldColor, 0, 0);
  end;

procedure GsChangeDomainColorCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newColor, kSetField, fieldId, kFieldColor, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainColorCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldColor, kSetField, fieldId, kFieldColor, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainColorCommand.description: string;
	begin
  result := inherited description;
  result := 'change ' + result + ' from '
    + '(R ' + intToStr(getRValue(oldColor))
    + ', G ' + intToStr(getGValue(oldColor))
    + ', B ' + intToStr(getBValue(oldColor))
    + ') to (R ' + intToStr(getRValue(newColor))
    + ', G ' + intToStr(getGValue(newColor))
    + ', B ' + intToStr(getBValue(newColor)) + ')';
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsChangeDomainIconCommand }
constructor GsChangeDomainIconCommand.createCommand(aModel: GsModel; aNewIcon: GsIcon; aFieldID: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newIcon := aNewIcon;
  {store old value}
  oldIcon := nil;
  Domain.modelTransferField(model, oldIcon, kGetField, fieldId, kFieldIcon, 0, 0);
  end;

procedure GsChangeDomainIconCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newIcon, kSetField, fieldId, kFieldIcon, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainIconCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldIcon, kSetField, fieldId, kFieldIcon, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainIconCommand.description: string;
  var oldName, newName: string;
	begin
  result := inherited description;
  oldName := '(none)';
  if (oldIcon <> nil) and (length(oldIcon.name) > 0) then oldName := oldIcon.name;
  newName := '(none)';
  if (newIcon <> nil) and (length(newIcon.name) > 0) then newName := newIcon.name;
  result := 'change ' + result + ' from ' + oldName + ' to ' + newName;
  result := limitDescription(result);
  end;

destructor GsChangeDomainIconCommand.destroy;
  begin
  {don't free icons because they are shared with template manager}
  inherited destroy;
  end;

{ ------------------------------------------------------------ GsChangeDomainHarvestItemTemplateCommand }
constructor GsChangeDomainHarvestItemTemplateCommand.createCommand(aModel: GsModel;
  aNewHarvestItemTemplate: GsHarvestItemTemplate; aFieldID: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newHarvestItemTemplate := aNewHarvestItemTemplate;
  {store old value}
  oldHarvestItemTemplate := nil;
  Domain.modelTransferField(model, oldHarvestItemTemplate, kGetField, fieldId, kFieldHarvestItemTemplate, 0, 0);
  end;

procedure GsChangeDomainHarvestItemTemplateCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newHarvestItemTemplate, kSetField, fieldId, kFieldHarvestItemTemplate, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  BrowserForm.updateModelValuesForModel(model);
  inherited doCommand;
  end;

procedure GsChangeDomainHarvestItemTemplateCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldHarvestItemTemplate, kSetField, fieldId, kFieldHarvestItemTemplate, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  BrowserForm.updateModelValuesForModel(model);
  inherited undoCommand;
  end;

function GsChangeDomainHarvestItemTemplateCommand.description: string;
  var oldName, newName: string;
	begin
  result := inherited description;
  if (oldHarvestItemTemplate <> nil) and (length(oldHarvestItemTemplate.name) > 0) then
    oldName := oldHarvestItemTemplate.name
  else
    oldName := '(none)';
  if (newHarvestItemTemplate <> nil) and (length(newHarvestItemTemplate.name) > 0) then
    newName := newHarvestItemTemplate.name
  else
    newName := '(none)';
  result := 'change ' + result + ' from ' + oldName + ' to ' + newName;
  result := limitDescription(result);
  end;

destructor GsChangeDomainHarvestItemTemplateCommand.destroy;
  begin
  {don't free harvest item templates because they are shared with template manager}
  inherited destroy;
  end;

{ ------------------------------------------------------------ GsChangeDomainObject3DCommand }
constructor GsChangeDomainObject3DCommand.createCommand(aModel: GsModel; aNewTdo: KfObject3D; aFieldID: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newTdo := aNewTdo;
  {store old value}
  oldTdo := nil;
  Domain.modelTransferField(model, oldTdo, kGetField, fieldId, kFieldThreeDObject, 0, 0);
  end;

procedure GsChangeDomainObject3DCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newTdo, kSetField, fieldId, kFieldThreeDObject, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainObject3DCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldTdo, kSetField, fieldId, kFieldThreeDObject, 0, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainObject3DCommand.description: string;
  var oldName, newName: string;
	begin
  result := inherited description;
  oldName := '(none)';
  if (oldTdo <> nil) and (length(oldTdo.name) > 0) then oldName := lowerCase(extractFileName(oldTdo.name));
  newName := '(none)';
  if (newTdo <> nil) and (length(newTdo.name) > 0) then newName := lowerCase(extractFileName(newTdo.name));
  result := 'change ' + result + ' from ' + oldName + ' to ' + newName;
  result := limitDescription(result);
  end;

destructor GsChangeDomainObject3DCommand.destroy;
  begin
  {do not free tdo's because they are shared with template manager}
  inherited destroy;
  end;

{ ------------------------------------------------------------ GsChangeDomainSoilTextureCommand }
constructor GsChangeDomainSoilTextureCommand.createCommand(aModel: GsModel;
		aClay: single; aSilt: single; aSand: single; aFieldIndex: smallint);
	begin
  { not using fieldID field in this case }
  inherited createCommand(aModel, 0);
  newClay := aClay;
  newSilt := aSilt;
  newSand := aSand;
  index := aFieldIndex;
  {store old value}
  Domain.modelTransferField(model, oldClay, kGetField, kSoilPatchLayerClayContent_pct, kFieldFloat, index, 0);
  Domain.modelTransferField(model, oldSilt, kGetField, kSoilPatchLayerSiltContent_pct, kFieldFloat, index, 0);
  Domain.modelTransferField(model, oldSand, kGetField, kSoilPatchLayerSandContent_pct, kFieldFloat, index, 0);
  end;

procedure GsChangeDomainSoilTextureCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newClay, kSetField, kSoilPatchLayerClayContent_pct, kFieldFloat, index, 0);
  Domain.modelTransferField(model, newSilt, kSetField, kSoilPatchLayerSiltContent_pct, kFieldFloat, index, 0);
  Domain.modelTransferField(model, newSand, kSetField, kSoilPatchLayerSandContent_pct, kFieldFloat, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainSoilTextureCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldClay, kSetField, kSoilPatchLayerClayContent_pct, kFieldFloat, index, 0);
  Domain.modelTransferField(model, oldSilt, kSetField, kSoilPatchLayerSiltContent_pct, kFieldFloat, index, 0);
  Domain.modelTransferField(model, oldSand, kSetField, kSoilPatchLayerSandContent_pct, kFieldFloat, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainSoilTextureCommand.description: string;
	begin
  result := 'change soil texture from ('
    + floatToStrF(oldClay, ffFixed, 7, 0) + ', '
    + floatToStrF(oldSilt, ffFixed, 7, 0)
    + ', ' + floatToStrF(oldSand, ffFixed, 7, 0)
    + ') to (' + floatToStrF(newClay, ffFixed, 7, 0) + ', '
    + floatToStrF(newSilt, ffFixed, 7, 0) + ', '
    + floatToStrF(newSand, ffFixed, 7, 0) + ')';
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsChangeDomainIntegerCommand }
constructor GsChangeDomainIntegerCommand.createCommand(aModel: GsModel;
		aNewValue: integer; aFieldID: smallint; aIndex: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newValue := aNewValue;
  index := aIndex;
  {store old value}
  Domain.modelTransferField(model, oldValue, kGetField, fieldId, kFieldInt, index, 0);
  end;

procedure GsChangeDomainIntegerCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newValue, kSetField, fieldId, kFieldInt, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainIntegerCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldValue, kSetField, fieldId, kFieldInt, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainIntegerCommand.description: string;
	begin
  result := inherited description;
  result := 'change ' + result + ' from ' + IntToStr(oldValue) + ' to ' + IntToStr(newValue);
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsChangeDomainStringCommand }
constructor GsChangeDomainStringCommand.createCommand(aModel: GsModel;
		aNewValue: string; aFieldID: smallint; aIndex: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newValue := aNewValue;
  index := aIndex;
  {store old value}
  Domain.modelTransferField(model, oldValue, kGetField, fieldId, kFieldString, index, 0);
  end;

procedure GsChangeDomainStringCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newValue, kSetField, fieldId, kFieldString, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainStringCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldValue, kSetField, fieldId, kFieldString, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainStringCommand.description: string;
	begin
  result := inherited description;
  result := 'change ' + result + ' from ' + oldValue + ' to ' + newValue;
  result := limitDescription(result);
  end;

{ ------------------------------------------------------------ GsChangeDomainBooleanCommand }
constructor GsChangeDomainBooleanCommand.createCommand(aModel: GsModel;
		aNewValue: boolean; aFieldID: smallint; aIndex: smallint);
	begin
  inherited createCommand(aModel, aFieldID);
  newValue := aNewValue;
  index := aIndex;
  {store old value}
  Domain.modelTransferField(model, oldValue, kGetField, fieldId, kFieldBoolean, index, 0);
  end;

procedure GsChangeDomainBooleanCommand.doCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, newValue, kSetField, fieldId, kFieldBoolean, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited doCommand;
  end;

procedure GsChangeDomainBooleanCommand.undoCommand;
  begin
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  Domain.modelTransferField(model, oldValue, kSetField, fieldId, kFieldBoolean, index, 0);
  self.updatePlantBoundsIfNeeded;
  GardenForm.invalidateGardenRect((model as GsGraphicalModel).boundsRect);
  inherited undoCommand;
  end;

function GsChangeDomainBooleanCommand.description: string;
  var
    oldValueString, newValueString: string[10];
	begin
  result := inherited description;
  if oldValue then
    oldValueString := 'yes'
  else
    oldValueString := 'no';
  if newValue then
    newValueString := 'yes'
  else
    newValueString := 'no';
  result := 'change ' + result + ' from ' + oldValueString + ' to ' + newValueString;
  result := limitDescription(result);
  end;

end.

unit Utools;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utools: Several classes for tool use.
* GsToolRateOrAmountParam: Holds information on one parameter for a tool action, such
as how deep to dig or how much material to apply. When the user picks up a tool
and choices appear in the one or two combo boxes after the tool list in the garden
window toolbar, each one of the choices is one of these objects. Bounds are hard-coded
in this file.
* GsToolParamListManager: In the domain; keeps several lists of GsToolRateOrAmountParams
for use when displaying tool action options and carrying out tool actions. The only
lists that are used at this point are adding amount for solids (for applying an amendment),
carrying amount (for carrying soil or mulch), mixing depth, and tool area. Tool area
is used with all the mixing tools to decide how much of the soil patch is affected by
the tool action (tool area/soil patch area). The tool params list editor (utlpmed)
is used to edit these items. The tool param list manager is saved in the garden
file with the domain.
* GsToolAction: The tool action object has the job of creating the command to place
in the undo list and actually carry out what should happen after the mouse was
clicked. Each tool has a list of tool actions (which can be changed in the tool
editor - utooledt). The tool actions use case statements to decide several things
about the tool action that are necessary to carry it out - how many parameters
it needs, the string to put in the combo box, what command to create, etc. The
types of tool actions are a hard-coded list of constants.
* GsToolSettings: This object stores some information about a tool in the
garden file separate from the information stored in the tool file. This is so
we can save the position of each tool in the garden picture and whether the
tool is visible or not. When the garden file is opened this information is
written over the information from the tool file in the tool manager. There
is actually no list of tool settings saved in the domain, since this information
is in the tool manager. On streaming in, the information is copied into the
current tool manager; on streaming out, the information in the current tool
manager is used to create a temporary tool settings list which is streamed out.
Look at the streamDataWithFiler and readDataWithFiler functions in udomain
for how this works.
* GsTool: A tool object, which carries out tool actions on soil patches
and plants. The tool object has some number of tool actions (a TListCollection),
a name, a note (PChar), several bitmaps (up,down,full, and masks for all these),
and sounds to play at various stages of mouse use. Tools are read from a tool
file and stored in the tool manager in the domain. Tools encapsulate the
behavior of transparent bitmaps as well as action handling.
* GsToolManager: Keeps and handles all the tools. Held onto by the domain.
Has code for locating tools at a cursor position, streaming tools, etc.
Treats glove tool specially, because it is 'up' when all the other tools
are down.}

interface
uses WinTypes, WinProcs, SysUtils, Messages, Classes, Graphics, Controls, Forms,
			Dialogs, StdCtrls, ExtCtrls, ucollect, 
      uesoil, ucommand, ufiler, usound, ubitmap, uestruct;

const
  kToolStateDown = 1;
  kToolStateUp = 2;
  kToolStateFull = 3;

  kOverPlant = 1;
  kOverPatch = 2;
  kOverWeather = 3;

  kAllowBitmapSubstitution = true;
  kNoBitmapSubstitution = false;

  kToolActionFirstAction = 1;
  kToolActionPullPlant = 1;
  kToolActionHarvestWholePlant = 2;
  kToolActionHarvestMatureParts = 3;
  kToolActionRename = 4;
  kToolActionTransplant = 5;
  kToolActionApply = 6;
  kToolActionPlant = 7;
  kToolActionScan = 8;
  kToolActionMagnify = 9;
  kToolActionScanAndMagnify = 10;
  kToolActionNewSoilPatch = 11;
  kToolActionChangeSoilPatch = 12;
  kToolActionShapeSoilPatch = 13;
  kToolActionDragSoilPatch = 14;
 	kToolActionMixSoil = 15;
  kToolActionDoubleDigSoil = 16;
  kToolActionCarrySoil = 17;
  kToolActionRemoveSoilPatch = 18;
  kToolActionCultivateSoil = 19;
  kToolActionWater = 20;
  kToolActionGrowPlant = 21;
  kToolActionZapPlant = 22; { is now zap all plants in patch }
  kToolActionAerateSoil = 23;
  kToolActionCarryMulch = 24;
  kToolActionSupport = 25;
  { new }
  kToolActionReseed = 26;
  kToolActionDuplicate = 27;
  kToolActionStopOrStart = 28;
  kToolActionLastAction = 28;

  kListTypeAmountCarrying = 1;
  kListTypeAmountSolid = 2;
  kListTypeAmountLiquid = 3;
  kListTypeDepthPlanting = 4;
  kListTypeDepthMixing = 5;
  kListTypeToolArea = 6;

  kAmountPerClick = 1;
  kAmountPerAreaPerClick = 2;
  kAmountPerSecond = 3;
  kAmountPerAreaPerSecond = 4;
  kAmountAuto = 5;

  kSolid = 0;
  kLiquid = 1;
  kNone = 2;

type

  GsTool = class;
  GsToolManager = class;

  GsToolRateOrAmountParam = class(GsStreamableObject)
    public
    materialType: smallint;
    valueType: smallint;
    listType: smallint;
    value: single;
    unitSet: smallint;
    displayUnit: smallint;
    modelUnit: smallint;
    lowerBound: single;
    upperBound: single;
    constructor createWithInfo(aListType, aMaterialType, aValueType: smallint; aValue: single; aDisplayUnit: smallint);
    constructor createAsMemberOfList(aListType: smallint);
    procedure resolveUnitSetForMaterialTypeAndValueType;
    procedure switchDisplayUnitBetweenAbsoluteAndRelative;
    procedure hardCodeBoundsForModelUnit;
    procedure setModelBounds(lower, upper: single);
    function name(isInGardenWindow: boolean): string;
    function toModelUnit(value: single): single;
    function toCurrentUnit(value: single): single;
    procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
    procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    function listTypeName: string;
    function materialTypeName: string;
    function valueTypeName: string;
    end;

  GsToolParamListManager = class(GsStreamableObject)
    public
    amountCarryingList: TListCollection;
    amountSolidList: TListCollection;
    amountLiquidList: TListCollection;
    depthPlantingList: TListCollection;
    depthMixingList: TListCollection;
    toolAreaList: TListCollection;
    showParamsWithUnselectedUnitSystem: boolean;
    constructor create; override;
    destructor destroy; override;
    procedure fillComboBoxWithSavedList(comboBox: TComboBox; listType: smallint);
    function listForListType(listType: smallint): TListCollection;
    procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
    procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    procedure hardCodeLists;
    procedure clearAllLists;
    end;

  GsToolAction = class(GsStreamableObject)
    public
    actionType: smallint;
    param1, param2: string;
    constructor createWithActionType(aType: smallint);
    function createCommand: KfCommand;
    procedure setToolParam1FromComboBox(comboBox: TComboBox);
    procedure setToolParam2FromComboBox(comboBox: TComboBox);
    function name: string;
    procedure fillComboBoxesWithParams(toolParam1Choices, toolParam2Choices: TComboBox);
    procedure fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices: TComboBox);
    procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
    procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    function paramCount: smallint;
    function getHint(overWhat: smallint;
        gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants: boolean): string;
    end;

  GsToolSettings = class(GsStreamableObject)
    public
    name: string;
    x, y : smallint;
    visible: boolean;
    constructor createWithTool(aTool: GsTool);
    procedure setToolValuesInToolManager(toolManager: GsToolManager);
		procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
		procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
    end;

  GsTool = class(GsStreamableObject)
    private
    { Private declarations }
    public
      upBitmap, upMask, downBitmap, downMask, fullBitmap, fullMask : GsBitmap;
      upHotSpot, downHotSpot, fullHotSpot: TPoint;
      soundDown, soundUsing, soundUp, soundPickUp, soundMoving, soundPutDown: GsSound;
      x, y : smallint;
      state : smallint;
      visible : boolean;
      cursor : TCursor;
      name : string;
      note: PChar;
      actionList: TListCollection;
      currentActionName: string;
      destructor destroy; override;
			function verifyClassAndVersionInformation(
				filer: GsFiler; size: longint; const cvirRead, cvirClass: GsClassAndVersionInformationRecord): boolean; override;
  		procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
 		  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
			procedure makeMasks;
      function boundsRect : TRect;
			function imageRect : TRect;
			procedure drawOn(aBitmap: TBitmap);
      procedure loadBitmaps(const upFile: string; const downFile: string; const fullFile: string);
      procedure moveOver(point: TPoint);
      procedure setState(aState: smallint);
      function bitmapForCurrentState(substituteIfAbsent: boolean): GsBitmap;
      function maskForBitmap(bitmap: GsBitmap): GsBitmap;
      function hotSpotForCurrentState: TPoint;
      function currentAction: GsToolAction;
      function isPointOnTool(point: TPoint): boolean;
			function isPointOnDownMask(point: TPoint): boolean;
      function createNewCommand(point: TPoint): KfCommand;
      procedure fillComboBoxWithActions(toolActionChoices: TComboBox);
      constructor create; override;
      procedure constrainToRectangle(aRect: TRect);
      function actionForActionType(anActionType: smallint): GsToolAction;
      function getHintIfOver(longHint: boolean): string;
      function getHowToHintIfUsing(overWhat: smallint;
          gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants: boolean): string;
 	end;

  GsToolManager = class(GsStreamableObject)
    public
    tools: TListCollection;
    gloveTool: GsTool;
    constructor create; override;
    procedure loadDefaultTools;
    destructor destroy; override;
  	procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
    procedure putDownAllToolsExceptGlove;
    function specialCopyTo(newCopy: GsStreamableObject): boolean;
 		procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    procedure constrainToolsToRectangle(aRect: TRect);
    function findGlove: GsTool;
    procedure bringToolToFront(tool: GsTool);
    function findToolAtXY(x: integer; y: integer; currentTool: GsTool): GsTool;
    function findToolForName(name: string): GsTool;
    function newListCollectionOfToolSettings: TListCollection;
    procedure updateToolSettings(aListCollection: TListCollection);
    procedure createDefaultGlove;
    end;

function toolAtXY(each: TObject; data: TObject) : Boolean;

implementation

uses
	UGsim, ueplant, UGsCom, Uclasses, udomain, uunits, usupport, uebag;

{GsToolRateOrAmountParam}
constructor GsToolRateOrAmountParam.createWithInfo(
    aListType, aMaterialType, aValueType: smallint; aValue: single; aDisplayUnit: smallint);
  begin
  listType := aListType;
  materialType := aMaterialType;
  valueType := aValueType;
  displayUnit := aDisplayUnit;
  if valueType <> kAmountAuto then
    begin
    self.resolveUnitSetForMaterialTypeAndValueType;
    value := toModelUnit(aValue);
    end
  else
    value := 0.0;
  end;

constructor GsToolRateOrAmountParam.createAsMemberOfList(aListType: smallint);
  var
    isMetric: boolean;
    materialType, displayUnit: smallint;
  begin
  isMetric := Domain.menuOptions.showMetricUnits;
  case aListType of
    kListTypeAmountCarrying, kListTypeAmountSolid:
      begin
      materialType := kSolid;
      if isMetric then displayUnit := kMassKilograms else displayUnit := kMassPounds;
      end;
    kListTypeAmountLiquid:
      begin
      materialType := kLiquid;
      if isMetric then displayUnit := kVolumeLiters else displayUnit := kVolumeGallons;
      end;
    kListTypeDepthPlanting:
      begin
      materialType := kNone;
      if isMetric then displayUnit := kLengthCentimeters else displayUnit := kLengthInches;
      end;
    kListTypeDepthMixing:
      begin
      materialType := kNone;
      if isMetric then displayUnit := kLengthMeters else displayUnit := kLengthFeet;
      end;
    kListTypeToolArea:
      begin
      materialType := kNone;
      if isMetric then displayUnit := kAreaSquareMeters else displayUnit := kAreaSquareFeet;
      end;
    else
      raise Exception.create('GsToolRateOrAmountParam.createAsMemberOfList: unsupported list type');
    end;
  self.createWithInfo(aListType, materialType, kAmountPerClick, 1.0, displayUnit);
  end;

procedure GsToolRateOrAmountParam.resolveUnitSetForMaterialTypeAndValueType;
  begin
  case materialType of
    kSolid:
      case valueType of
        kAmountPerClick, kAmountPerSecond:
          begin
          unitSet := kMass;
          modelUnit := kMassKilograms;
          end;
        kAmountPerAreaPerClick, kAmountPerAreaPerSecond:
          begin
          unitSet := kMassOverArea;
          modelUnit := kMassOverAreaKilogramsPHectare;
          end;
        end;
    kLiquid:
      case valueType of
        kAmountPerClick, kAmountPerSecond:
          begin
          unitSet := kVolume;
          modelUnit := kVolumeLiters;
          end;
        kAmountPerAreaPerClick, kAmountPerAreaPerSecond:
          begin
          unitSet := kDepthOfWater;
          modelUnit := kDepthOfWaterMillimeters;
          end;
        end;
    kNone:
      begin
      case listType of
        kListTypeDepthMixing:
          begin
          unitSet := kLength;
          modelUnit := kLengthMeters;
          end;
        kListTypeDepthPlanting:
          begin
          unitSet := kLength;
          modelUnit := kLengthMillimeters;
          end;
        kListTypeToolArea:
          begin
          unitSet := kArea;
          modelUnit := kAreaSquareMeters;
          end;
        end;
      end;
    end;
  self.hardCodeBoundsForModelUnit;
  end;

procedure GsToolRateOrAmountParam.hardCodeBoundsForModelUnit;
  begin
  case unitSet of
    kMass: self.setModelBounds(0, 10); {kg}
    kMassOverArea: self.setModelBounds(0, 100); {kg/ha}
    kVolume: self.setModelBounds(0, 10); {liters}
    kDepthOfWater: self.setModelBounds(0, 100); {mm}
    kLength:
      if modelUnit = kLengthMeters then
        self.setModelBounds(0, 3) {m}
      else
        self.setModelBounds(0, 100);  {mm}
    kArea: self.setModelBounds(0, 100);
    else raise Exception.create('GsToolRateOrAmountParam.hardCodeBoundsForModelUnit: unsupported unit set');
    end;
  end;

procedure GsToolRateOrAmountParam.setModelBounds(lower, upper: single);
  begin
  lowerBound := lower;
  upperBound := upper;
  end;

procedure GsToolRateOrAmountParam.switchDisplayUnitBetweenAbsoluteAndRelative;
  var newDisplayUnit: smallint;
  begin
  { this is called after the unit set is switched }
  case unitSet of
    kMassOverArea: { switching from kMass }
      case displayUnit of
        kMassMilligrams: newDisplayUnit := kMassOverAreaGramsPSquareMeter;
        kMassGrams: newDisplayUnit := kMassOverAreaGramsPSquareMeter;
        kMassKilograms: newDisplayUnit := kMassOverAreaKilogramsPSquareMeter;
        kMassMetricTons: newDisplayUnit := kMassOverAreaMetricTonsPHectare;
        kMassOunces: newDisplayUnit := kMassOverAreaOuncesPSquareFoot;
        kMassPounds: newDisplayUnit := kMassOverAreaPoundsPSquareFoot;
        kMassEnglishTons: newDisplayUnit := kMassOverAreaEnglishTonsPAcre;
    		else raise Exception.create(
    			'GsToolRateOrAmountParam.switchDisplayUnitBetweenAbsoluteAndRelative: unsupported case');
        end;
    kMass: { switching from kMassOverArea }
      case displayUnit of
        kMassOverAreaKilogramsPHectare: newDisplayUnit := kMassKilograms;
        kMassOverAreaMetricTonsPHectare: newDisplayUnit := kMassMetricTons;
        kMassOverAreaKilogramsPSquareMeter: newDisplayUnit := kMassKilograms;
        kMassOverAreaGramsPSquareMeter: newDisplayUnit := kMassGrams;
        kMassOverAreaGramsPSquareCentimeter: newDisplayUnit := kMassGrams;
        kMassOverAreaPoundsPAcre: newDisplayUnit := kMassPounds;
        kMassOverAreaEnglishTonsPAcre: newDisplayUnit := kMassEnglishTons;
        kMassOverAreaPoundsPSquareYard: newDisplayUnit := kMassPounds;
        kMassOverAreaPoundsPSquareFoot: newDisplayUnit := kMassPounds;
        kMassOverAreaOuncesPSquareFoot: newDisplayUnit := kMassOunces;
    		else raise Exception.create(
    			'GsToolRateOrAmountParam.switchDisplayUnitBetweenAbsoluteAndRelative: unsupported case');
        end;
    kVolume: { switching from kDepthOfWater}
      case displayUnit of
        kDepthOfWaterMillimeters: newDisplayUnit := kVolumeLiters;
        kDepthOfWaterInches: newDisplayUnit := kVolumeGallons;
    		else raise Exception.create(
    			'GsToolRateOrAmountParam.switchDisplayUnitBetweenAbsoluteAndRelative: unsupported case');
        end;
    kDepthOfWater: { switching from kVolume }
      case displayUnit of
        kVolumeMilliliters: newDisplayUnit := kDepthOfWaterMillimeters;
        kVolumeLiters: newDisplayUnit := kDepthOfWaterMillimeters;
        kVolumeCubicMeters: newDisplayUnit := kDepthOfWaterMillimeters;
        kVolumeQuarts: newDisplayUnit := kDepthOfWaterInches;
        kVolumeGallons: newDisplayUnit := kDepthOfWaterInches;
        kVolumeOunces: newDisplayUnit := kDepthOfWaterInches;
    		else raise Exception.create(
    			'GsToolRateOrAmountParam.switchDisplayUnitBetweenAbsoluteAndRelative: unsupported case');
        end;
    else raise Exception.create(
    	'GsToolRateOrAmountParam.switchDisplayUnitBetweenAbsoluteAndRelative: unsupported case');
    end;
  displayUnit := newDisplayUnit;
  end;

function GsToolRateOrAmountParam.toModelUnit(value: single): single;
  begin
  result := Convert(unitSet, displayUnit, modelUnit, value);
  end;

function GsToolRateOrAmountParam.toCurrentUnit(value: single): single;
  begin
  result := Convert(unitSet, modelUnit, displayUnit, value);
  end;

function nameForListType(aListType: smallint): string;
  begin
  result := '';
  case aListType of
    kListTypeAmountCarrying: result := 'amount';
    kListTypeAmountSolid: result := 'amount';
    kListTypeAmountLiquid: result := 'amount';
    kListTypeDepthMixing: result := 'depth';
    kListTypeDepthPlanting: result := 'depth';
    kListTypeToolArea: result := 'area';
    end;
  end;

function GsToolRateOrAmountParam.name(isInGardenWindow: boolean): string;
  var valueString: string;
  begin
  if isInGardenWindow then
    result := nameForListType(listType) + ': '
  else
    result := '';
  if valueType = kAmountAuto then
    result := result + 'auto'
  else
    begin
    try
      valueString := digitValueString(toCurrentUnit(value));
    except
      valueString := '(error)';
    end;
    result := result + valueString;
    result := result + ' ' + UnitStringForEnum(unitSet, displayUnit);
    if (valueType = kAmountPerSecond) or (valueType = kAmountPerAreaPerSecond) then
      result := result + '/sec';
    end;
  end;

function GsToolRateOrAmountParam.listTypeName: string;
  begin
  result := '';
  case listType of
    kListTypeAmountCarrying: result := 'amount carrying';
    kListTypeAmountSolid: result := 'amount solid';
    kListTypeAmountLiquid: result := 'amount liquid';
    kListTypeDepthPlanting: result := 'depth planting';
    kListTypeDepthMixing: result := 'depth mixing';
    kListTypeToolArea: result := 'area';
    else
      raise Exception.create('GsToolRateOrAmountParam.listTypeName: Invalid tool parameter list type');
    end;
  end;

function GsToolRateOrAmountParam.materialTypeName: string;
  begin
  result := '';
  case materialType of
    kLiquid: result := 'liquid';
    kSolid: result := 'solid';
    kNone: result := 'none';
    else
      raise Exception.create('GsToolRateOrAmountParam.materialTypeName: Invalid tool parameter material type');
    end;
  end;

function GsToolRateOrAmountParam.valueTypeName: string;
  begin
  result := '';
  case valueType of
    kAmountPerClick: result := 'amount per click';
    kAmountPerAreaPerClick: result := 'amount per area per click';
    kAmountPerSecond: result := 'amount per second';
    kAmountPerAreaPerSecond: result := 'amount per area per second';
    kAmountAuto: result := 'auto amount';
    else
      raise Exception.create('GsToolRateOrAmountParam.valueTypeName: Invalid tool parameter value type');
    end;
  end;

procedure GsToolRateOrAmountParam.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsToolRateOrAmountValue;
  cvir.versionNumber := 1;
  cvir.additionNumber := 0;
  end;

procedure GsToolRateOrAmountParam.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamSmallint(materialType);
  filer.streamSmallint(valueType);
  filer.streamSmallint(listType);
  filer.streamSingle(value);
  filer.streamSmallint(unitSet);
  filer.streamSmallint(displayUnit);
  filer.streamSmallint(modelUnit);
  filer.streamSingle(lowerBound);
  filer.streamSingle(upperBound);
  end;

{GsToolParamListManager}
constructor GsToolParamListManager.create;
  begin
  inherited create;
  amountCarryingList := TListCollection.create;
  amountSolidList := TListCollection.create;
  amountLiquidList := TListCollection.create;
  depthPlantingList := TListCollection.create;
  depthMixingList := TListCollection.create;
  toolAreaList := TListCollection.create;
  showParamsWithUnselectedUnitSystem := true;
  end;

destructor GsToolParamListManager.destroy;
  begin
  amountCarryingList.free;
  amountCarryingList := nil;
  amountSolidList.free;
  amountSolidList := nil;
  amountLiquidList.free;
  amountLiquidList := nil;
  depthPlantingList.free;
  depthPlantingList := nil;
  depthMixingList.free;
  depthMixingList := nil;
  toolAreaList.free;
  toolAreaList := nil;
  inherited destroy;
  end;

procedure GsToolParamListManager.fillComboBoxWithSavedList(comboBox: TComboBox; listType: smallint);
  var
    theList: TListCollection;
    theRateOrAmountParam: GsToolRateOrAmountParam;
    i: longint;
    shouldAddItem: boolean;
  begin
  { this is used to fill the combo boxes on the GardenForm. it should not be used in the list editor }
  comboBox.clear;
  comboBox.sorted := false; { for number choices, don't want sorting }
  theList := self.listForListType(listType);
  if theList.count > 0 then
    for i := 0 to theList.count - 1 do
      begin
      theRateOrAmountParam := GsToolRateOrAmountParam(theList.items[i]);
      if self.showParamsWithUnselectedUnitSystem then
        shouldAddItem := true
      else
        begin
        with theRateOrAmountParam do
          if unitIsMetric(unitSet, displayUnit) = Domain.menuOptions.showMetricUnits then
            shouldAddItem := true
          else
            shouldAddItem := false;
        end;
      if shouldAddItem then
        comboBox.items.addObject(theRateOrAmountParam.name(true), theRateOrAmountParam);
      end;
  comboBox.hint := nameForListType(listType) + ' choices';
  end;

function GsToolParamListManager.listForListType(listType: smallint): TListCollection;
  begin
  case listType of
    kListTypeAmountCarrying: result := amountCarryingList;
    kListTypeAmountSolid: result := amountSolidList;
    kListTypeAmountLiquid: result := amountLiquidList;
    kListTypeDepthPlanting: result := depthPlantingList;
    kListTypeDepthMixing: result := depthMixingList;
    kListTypeToolArea: result := toolAreaList;
    else
      raise Exception.create('GsToolParamListManager.listForListType: Unknown tool param list type');
    end;
  end;

procedure GsToolParamListManager.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsToolParamListManager;
  cvir.versionNumber := 2;
  cvir.additionNumber := 0;
  end;

procedure GsToolParamListManager.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  amountCarryingList.streamUsingFiler(filer, GsToolRateOrAmountParam);
  amountSolidList.streamUsingFiler(filer, GsToolRateOrAmountParam);
  amountLiquidList.streamUsingFiler(filer, GsToolRateOrAmountParam);
  depthPlantingList.streamUsingFiler(filer, GsToolRateOrAmountParam);
  depthMixingList.streamUsingFiler(filer, GsToolRateOrAmountParam);
  toolAreaList.streamUsingFiler(filer, GsToolRateOrAmountParam);
  filer.streamBoolean(showParamsWithUnselectedUnitSystem);
  end;

procedure GsToolParamListManager.clearAllLists;
  begin
  amountCarryingList.clear;
  amountSolidList.clear;
  amountLiquidList.clear;
  depthPlantingList.clear;
  depthMixingList.clear;
  toolAreaList.clear;
  end;

{ GsToolAction }
constructor GsToolAction.createWithActionType(aType: smallint);
  begin
  self.create;
  actionType := aType;
  end;

function GsToolAction.paramCount: smallint;
  begin
  result := 0;
  case actionType of
    kToolActionPullPlant: result := 0;
    kToolActionHarvestWholePlant: result := 0;
    kToolActionHarvestMatureParts: result := 0;
    kToolActionRename: result := 0;
    kToolActionTransplant: result := 0;
    kToolActionApply: result := 2;
    kToolActionPlant: result := 2;
    kToolActionScan: result := 0;
    kToolActionMagnify: result := 0;
    kToolActionScanAndMagnify: result := 0;
    kToolActionNewSoilPatch: result := 0;
    kToolActionChangeSoilPatch: result := 0;
    kToolActionShapeSoilPatch: result := 0;
    kToolActionDragSoilPatch: result := 0;
 	  kToolActionMixSoil: result := 2;
    kToolActionDoubleDigSoil: result := 0;
    kToolActionCarrySoil: result := 2;
    kToolActionRemoveSoilPatch: result := 0;
    kToolActionCultivateSoil: result := 2;
    kToolActionWater: result := 1;
    kToolActionGrowPlant: result := 1;
    kToolActionZapPlant: result := 0;
    kToolActionAerateSoil: result := 2;
    kToolActionCarryMulch: result := 1;
    kToolActionSupport: result := 0;
    kToolActionReseed: result := 0;
    kToolActionDuplicate: result := 0;
    kToolActionStopOrStart: result := 0;
  else
    raise Exception.create('Unimplemented tool action');
  end;
  end;

function GsToolAction.name: string;
  begin
  result := '';
  case actionType of
    kToolActionPullPlant: result := 'pull plant';
    kToolActionHarvestWholePlant: result := 'harvest whole';
    kToolActionHarvestMatureParts: result := 'harvest parts';
    kToolActionRename: result := 'rename';
    kToolActionTransplant: result := 'transplant';
    kToolActionApply: result := 'apply';
    kToolActionPlant: result := 'plant';
    kToolActionScan: result := 'scan';
    kToolActionMagnify: result := 'magnify';
    kToolActionScanAndMagnify: result := 'scan and magnify';
    kToolActionNewSoilPatch: result := 'new soil patch';
    kToolActionChangeSoilPatch: result := 'change soil patch';
    kToolActionShapeSoilPatch: result := 'shape soil patch';
    kToolActionDragSoilPatch: result := 'drag soil patch';
 	  kToolActionMixSoil: result := 'mix soil';
    kToolActionDoubleDigSoil: result := 'double-dig soil';
    kToolActionCarrySoil: result := 'carry soil';
    kToolActionRemoveSoilPatch: result := 'remove soil patch';
    kToolActionCultivateSoil: result := 'cultivate soil';
    kToolActionWater: result := 'water soil';
    kToolActionGrowPlant: result := 'grow plant';
    kToolActionZapPlant: result := 'pull all plants';
    kToolActionAerateSoil: result := 'aerate soil';
    kToolActionCarryMulch: result := 'carry mulch';
    kToolActionSupport: result := 'support plant';
    kToolActionReseed: result := 'reseed plant';
    kToolActionDuplicate: result := 'duplicate';
    kToolActionStopOrStart: result := 'stasis box on/off';
  else
    raise Exception.create('Unimplemented tool action');
  end;
  end;

{ function GsToolAction.getHint is at bottom of file because it is long }

function GsToolAction.createCommand: KfCommand;
  var
    param1Object, param2Object: GsToolRateOrAmountParam;
  begin
  result := nil;
  param1Object := GsToolRateOrAmountParam(GardenForm.currentToolParam1Object);
  param2Object := GsToolRateOrAmountParam(GardenForm.currentToolParam2Object);
  case actionType of
    kToolActionPullPlant: result := GsZapPlantCommand.create;
    kToolActionHarvestWholePlant:
      begin
      result := GsHarvestPlantCommand.create;
      (result as GsHarvestPlantCommand).userWantsToPullPlant := true;
      end;
    kToolActionHarvestMatureParts:
      begin
      result := GsHarvestPlantCommand.create;
      (result as GsHarvestPlantCommand).userWantsToPullPlant := false;
      end;
    kToolActionRename: result := GsRenameCommand.create;
    kToolActionTransplant: result := GsTransplantCommand.create;
    kToolActionApply:
      begin
      result := GsApplyAmendmentCommand.create;
      (result as GsApplyAmendmentCommand).bag := GsBag(GardenForm.currentToolParam1Object);
      (result as GsApplyAmendmentCommand).paramObject := param2Object;
      end;
    kToolActionPlant:
      begin
      result := GsPlantSeedCommand.create;
      (result as GsPlantSeedCommand).cultivar := GsPlant(GardenForm.currentToolParam1Object);
      (result as GsPlantSeedCommand).paramObject := param2Object;
      end;
    kToolActionScan: result := GsSetBrowserChoiceLeftCommand.create;
    kToolActionMagnify: result := GsSetBrowserChoiceRightCommand.create;
    kToolActionScanAndMagnify: result := GsSetBrowserChoiceBothCommand.create;
    kToolActionNewSoilPatch: result := GsNewSoilPatchCommand.create;
    kToolActionChangeSoilPatch: result := GsChangeAreaSoilPatchCommand.create;
    kToolActionShapeSoilPatch: result := GsModifySoilPatchCommand.create;
    kToolActionDragSoilPatch: result := GsDragSoilPatchCommand.create;
 	  kToolActionMixSoil:
      begin
      result := GsMixSoilCommand.create;
      (result as GsMixSoilCommand).paramObject1 := param1Object;
      (result as GsMixSoilCommand).paramObject2 := param2Object;
      end;
    kToolActionDoubleDigSoil: result := GsDoubleDigCommand.create;
    kToolActionCarrySoil:
      begin
      result := GsCarrySoilCommand.create;
      (result as GsCarrySoilCommand).paramObject1 := param1Object;
      (result as GsCarrySoilCommand).paramObject2 := param2Object;
      end;
    kToolActionRemoveSoilPatch: result := GsRemoveSoilPatchCommand.create;
    kToolActionCultivateSoil:
      begin
      result := GsCultivateCommand.create;
      (result as GsCultivateCommand).paramObject1 := param1Object;
      (result as GsCultivateCommand).paramObject2 := param2Object;
      end;
    kToolActionWater:
      begin
      result := GsWaterCommand.create;
      (result as GsWaterCommand).paramObject := param1Object;
      end;
    kToolActionGrowPlant:
      begin
      result := GsGrowPlantCommand.create;
      { special hardcoded case - don't use toolParamListManager }
      if GardenForm.toolParam1Choices.itemIndex <> -1 then
        (result as GsGrowPlantCommand).setDaysToGrowWithItemIndex(GardenForm.toolParam1Choices.itemIndex)
      else
        (result as GsGrowPlantCommand).setDaysToGrowWithItemIndex(0); 
      end;
    kToolActionZapPlant: result := GsZapAllPlantsCommand.create;
    kToolActionAerateSoil:
      begin
      result := GsAerateCommand.create;
      (result as GsAerateCommand).paramObject1 := param1Object;
      (result as GsAerateCommand).paramObject2 := param2Object;
      end;
    kToolActionCarryMulch:
      begin
      result := GsCarryMulchCommand.create;
      (result as GsCarryMulchCommand).paramObject := param1Object;
      end;
    kToolActionSupport: result := GsSupportCommand.create;
    kToolActionReseed: result := GsReseedPlantCommand.create;
    kToolActionDuplicate: result := GsDuplicateCommand.create;
    kToolActionStopOrStart: result := GsStopOrStartPlantCommand.create;
  else
    begin
    raise Exception.create('Unimplemented tool action');
    result := nil;
    end;
  end;
  end;

procedure GsToolAction.fillComboBoxesWithParams(toolParam1Choices, toolParam2Choices: TComboBox);
  var
    setSecond: boolean;
  begin
  setSecond := false;
  toolParam1Choices.clear;
  case actionType of
    kToolActionPullPlant: ;
    kToolActionHarvestWholePlant: ;
    kToolActionHarvestMatureParts: ;
    kToolActionRename: ;
    kToolActionTransplant: ;
    kToolActionApply:
      begin
      Domain.templateManager.fillComboBoxWithBagsForTools(toolParam1Choices);
      setSecond := true;
      end;
    kToolActionPlant:
      begin
      Domain.templateManager.fillComboBoxWithCultivarsForTools(toolParam1Choices);
      setSecond := true;
      end;
    kToolActionScan: ;
    kToolActionMagnify: ;
    kToolActionScanAndMagnify: ;
    kToolActionNewSoilPatch: ;
    kToolActionChangeSoilPatch: ;
    kToolActionShapeSoilPatch: ;
    kToolActionDragSoilPatch: ;
 	  kToolActionMixSoil:
      begin
      Domain.toolParamListManager.fillComboBoxWithSavedList(toolParam1Choices, kListTypeDepthMixing);
      self.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices);
      end;
    kToolActionDoubleDigSoil: ;
    kToolActionCarrySoil:
      begin
      Domain.toolParamListManager.fillComboBoxWithSavedList(toolParam1Choices, kListTypeAmountCarrying);
      self.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices);
      end;
    kToolActionRemoveSoilPatch: ;
    kToolActionCultivateSoil:
      begin
      Domain.toolParamListManager.fillComboBoxWithSavedList(toolParam1Choices, kListTypeDepthMixing);
      self.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices);
      end;
    kToolActionWater:
      Domain.toolParamListManager.fillComboBoxWithSavedList(toolParam1Choices, kListTypeAmountLiquid);
    kToolActionGrowPlant:
      begin
      { don't want these numbers sorted }
      toolParam1Choices.sorted := false;
      { special hard-coded case - don't use toolParamListManager - use class procedure }
      GsGrowPlantCommand.fillComboBoxItemsWithDaysToGrowChoices(toolParam1Choices.items);
      { default choice is 5 days }
      toolParam1Choices.itemIndex := 0;
      end;
    kToolActionZapPlant: ;
    kToolActionAerateSoil:
      begin
      Domain.toolParamListManager.fillComboBoxWithSavedList(toolParam1Choices, kListTypeDepthMixing);
      self.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices);
      end;
    kToolActionCarryMulch:
      Domain.toolParamListManager.fillComboBoxWithSavedList(toolParam1Choices, kListTypeAmountCarrying);
    kToolActionSupport: ;
    kToolActionReseed: ;
    kToolActionDuplicate: ;
    kToolActionStopOrStart: ;
  else
    raise Exception.create('Unimplemented tool action');
  end;
  self.setToolParam1FromComboBox(toolParam1Choices);
  if setSecond then self.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices);
  end;

procedure GsToolAction.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices: TComboBox);
  var
    bag: GsBag;
  begin
  toolParam2Choices.clear;
  with Domain.toolParamListManager do
  case actionType of
    kToolActionPullPlant: ;
    kToolActionHarvestWholePlant: ;
    kToolActionHarvestMatureParts: ;
    kToolActionRename: ;
    kToolActionTransplant: ;
    kToolActionApply:
      begin
      bag := currentObjectInComboBox(toolParam1Choices) as GsBag;
      if bag <> nil then
        begin
        if bag.materialType = kSolid then
          fillComboBoxWithSavedList(toolParam2Choices, kListTypeAmountSolid)
        else
          fillComboBoxWithSavedList(toolParam2Choices, kListTypeAmountLiquid);
        end;
      end;
    kToolActionPlant: fillComboBoxWithSavedList(toolParam2Choices, kListTypeDepthPlanting);
    kToolActionScan: ;
    kToolActionMagnify: ;
    kToolActionScanAndMagnify: ;
    kToolActionNewSoilPatch: ;
    kToolActionChangeSoilPatch: ;
    kToolActionShapeSoilPatch: ;
    kToolActionDragSoilPatch: ;
    kToolActionMixSoil: fillComboBoxWithSavedList(toolParam2Choices, kListTypeToolArea);
    kToolActionDoubleDigSoil: ;
    kToolActionCarrySoil: fillComboBoxWithSavedList(toolParam2Choices, kListTypeDepthMixing);
    kToolActionRemoveSoilPatch: ;
    kToolActionCultivateSoil: fillComboBoxWithSavedList(toolParam2Choices, kListTypeToolArea);
    kToolActionWater: ;
    kToolActionGrowPlant: ;
    kToolActionZapPlant: ;
    kToolActionCarryMulch: ;
    kToolActionAerateSoil: fillComboBoxWithSavedList(toolParam2Choices, kListTypeToolArea);
    kToolActionSupport: ;
    kToolActionReseed: ;
    kToolActionDuplicate: ;
    kToolActionStopOrStart: ;
  else
    raise Exception.create('Unimplemented tool action');
  end;
  self.setToolParam2FromComboBox(toolParam2Choices);
  end;

procedure GsToolAction.setToolParam1FromComboBox(comboBox: TComboBox);
  begin
  comboBox.itemIndex := comboBox.items.indexOf(self.param1);
  if (comboBox.itemIndex = -1) and (comboBox.items.count > 0) then
    begin
    comboBox.itemIndex := 0;
    self.param1 := comboBox.items[comboBox.itemIndex];
    end;
  end;

procedure GsToolAction.setToolParam2FromComboBox(comboBox: TComboBox);
  begin
  comboBox.itemIndex := comboBox.items.indexOf(self.param2);
  if (comboBox.itemIndex = -1) and (comboBox.items.count > 0) then
    begin
    comboBox.itemIndex := 0;
    self.param2 := comboBox.items[comboBox.itemIndex];
    end;
  end;

procedure GsToolAction.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsToolAction;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsToolAction.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamSmallint(actionType);
  filer.streamShortString(param1);
  filer.streamShortString(param2);
  end;

{GsToolSettings}
constructor GsToolSettings.createWithTool(aTool: GsTool);
  begin
  self.create;
  name := aTool.name;
  x := aTool.x;
  y := aTool.y;
  visible := aTool.visible;
  end;

procedure GsToolSettings.setToolValuesInToolManager(toolManager: GsToolManager);
  var
    aTool: GsTool;
  begin
  aTool := toolManager.findToolForName(name);
  if aTool = nil then exit;
  aTool.name := name;
  aTool.x := x;
  aTool.y := y;
  aTool.visible := visible;
  toolManager.bringToolToFront(aTool);
  end;

procedure GsToolSettings.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsToolSettings;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsToolSettings.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  filer.streamShortString(name);
  filer.streamSmallint(x);
  filer.streamSmallint(y);
  filer.streamBoolean(visible);
  end;

{GsTool}
constructor GsTool.create;
  begin
  inherited create;
  {these are GsBitmaps so they can be streamed}
  upBitmap := GsBitmap.create;
  downBitmap := GsBitmap.create;
  upMask := GsBitmap.create;
  downMask := GsBitmap.create;
  fullBitmap := GsBitmap.create;
  fullMask := GsBitmap.create;
  actionList := TListCollection.create;
  soundDown := GsSound.create;
  soundUsing := GsSound.create;
  soundUp := GsSound.create;
  soundPickUp := GsSound.create;
  soundMoving := GsSound.create;
  soundPutDown := GsSound.create;
  state := kToolStateDown;
  note := nil;
  end;

destructor GsTool.destroy;
	begin;
  {cursor - do not free! it is a smallint}
  soundDown.free;
  soundDown := nil;
  soundUsing.free;
  soundUsing := nil;
  soundUp.free;
  soundUp := nil;
  soundPickUp.free;
  soundPickUp := nil;
  soundMoving.free;
  soundMoving := nil; 
  soundPutDown.free;
  soundPutDown := nil; 

  upBitmap.Free;
  upBitmap := nil;
  upMask.Free;
  upMask := nil;
  downBitmap.Free;
  downBitmap := nil;
  downMask.Free;
  downMask := nil;
  fullBitmap.free;
  fullBitmap := nil;
  fullMask.free;
  fullMask := nil;
  actionList.free;
  actionList := nil;
  if note <> nil then
  	StrDispose(note);
  note := nil;
  inherited destroy;
	end;

procedure GsTool.makeMasks;
	begin;
  with upMask do
  	begin;
		monochrome := true;
  	width := upBitmap.width;
  	height := upBitmap.height;
  	canvas.copyMode := cmWhiteness;
  	canvas.copyRect(Rect(0,0,width,height), upBitmap.canvas, Rect(0,0,width,height));
  	canvas.copyMode := cmSrcInvert;
  	canvas.copyRect(Rect(0,0,width,height), upBitmap.canvas, Rect(0,0,width,height));
  	end;
  with downMask do
  	begin;
		monochrome := true;
  	width := downBitmap.width;
  	height := downBitmap.height;
  	canvas.copyMode := cmWhiteness;
  	canvas.copyRect(Rect(0,0,width,height), downBitmap.canvas, Rect(0,0,width,height));
  	canvas.copyMode := cmSrcInvert;
  	canvas.copyRect(Rect(0,0,width,height), downBitmap.canvas, Rect(0,0,width,height));
    end;
  if fullBitmap.width = 0 then exit;
  with fullMask do
  	begin;
		monochrome := true;
  	width := fullBitmap.width;
  	height := fullBitmap.height;
  	canvas.copyMode := cmWhiteness;
  	canvas.copyRect(Rect(0,0,width,height), fullBitmap.canvas, Rect(0,0,width,height));
  	canvas.copyMode := cmSrcInvert;
  	canvas.copyRect(Rect(0,0,width,height), fullBitmap.canvas, Rect(0,0,width,height));
    end;
  end;

function GsTool.bitmapForCurrentState(substituteIfAbsent: boolean): GsBitmap;
  begin
  result := nil;
  case state of
    kToolStateDown:
      result := downBitmap;
    kToolStateUp:
      if (upBitmap.width <> 0) or (not substituteIfAbsent) then
        result := upBitmap
      else
        result := downBitmap;
    kToolStateFull:
      if (fullBitmap.width <> 0) or (not substituteIfAbsent) then
        result := fullBitmap
      else
        begin
        if upBitmap.width <> 0 then
          result := upBitmap
        else if downBitmap.width <> 0 then
          result := downBitmap
        else
          raise Exception.create('GsTool.bitmapForCurrentState: Tool has no bitmaps');
        end;
    end;
  end;

function GsTool.maskForBitmap(bitmap: GsBitmap): GsBitmap;
  begin
  result := nil;
  if bitmap = upBitmap then
    result := upMask
  else if bitmap = downBitmap then
    result := downMask
  else if bitmap = fullBitmap then
    result := fullMask
  else
    raise Exception.create('GsTool.maskForBitmap: no mask for this bitmap');
  end;

function GsTool.hotSpotForCurrentState: TPoint;
  begin
  result := point(0,0);
  case state of
    kToolStateDown: result := downHotSpot;
    kToolStateUp: result := upHotSpot;
    kToolStateFull: result := fullHotSpot;
    end;
  end;

function GSTool.boundsRect : TRect;
  var
    theHotSpot: TPoint;
    theBitmap: GsBitmap;
	begin
  theHotSpot := self.hotSpotForCurrentState;
  theBitmap := self.bitmapForCurrentState(true);
  result := rect(x - theHotSpot.x, y - theHotSpot.y,
    	x + theBitmap.width - theHotSpot.x, y + theBitmap.height - theHotSpot.y);
  end;

function GSTool.imageRect : TRect;
  var
    theBitmap: GsBitmap;
	begin
  theBitmap := self.bitmapForCurrentState(kAllowBitmapSubstitution);
  result := Rect(0, 0, theBitmap.width, theBitmap.height);
  end;

procedure GsTool.drawOn(aBitmap: TBitmap);
  var
    theBitmap, theMask: GsBitmap;
	begin
  theBitmap := self.bitmapForCurrentState(kAllowBitmapSubstitution);
  theMask := self.maskForBitmap(theBitmap);
  with aBitmap.canvas do
    begin
    copyMode := cmSrcPaint;
    copyRect(self.boundsRect, theMask.canvas, self.imageRect);
    copyMode := cmSrcAnd;
    copyRect(self.boundsRect, theBitmap.canvas, self.imageRect);
    end;
  end;

procedure GsTool.loadBitmaps(const upFile: string; const downFile: string; const fullFile: string);
	begin;
  if Domain.paletteBitmapLoaded then
    begin
    LoadDibFromFileAndPutInBitmap(upFile, upBitmap, Domain.paletteBitmap);
    LoadDibFromFileAndPutInBitmap(downFile, downBitmap, Domain.paletteBitmap);
    if fullFile <> '' then
     LoadDibFromFileAndPutInBitmap(fullFile, fullBitmap, Domain.paletteBitmap);
    end
  else
    begin
    upBitmap.loadFromFile(upFile);
    downBitmap.loadFromFile(downFile);
    if fullFile <> '' then fullBitmap.loadFromFile(fullFile);
    end;
 	self.makeMasks;
  end;

function GsTool.isPointOnTool(point: TPoint): boolean;
	begin
  result := false;
  if PtInRect(boundsRect, point) then
  	begin
    Result := self.isPointOnDownMask(point);
    end;
  end;

function GsTool.isPointOnDownMask(point: TPoint): boolean;
	var
  testPixel, backgroundPixel: TColor;
  xPixel, yPixel: longint;
  begin
  xPixel := point.x - (x - downHotSpot.x);
  yPixel := point.y - (y - downHotSpot.y);
  backgroundPixel := downMask.canvas.Pixels[0, 0];
  testPixel := downMask.canvas.Pixels[xPixel, yPixel];
  result := backgroundPixel <> testPixel;
	end;

procedure GsTool.moveOver(point: TPoint);
	begin
  end;

procedure GsTool.setState(aState: smallint);
  begin
  state := aState;
  {GardenForm.status.caption := self.name + ' pick up'; }
  end;

function GsTool.currentAction: GsToolAction;
  var
    anAction: GsToolAction;
    i: longint;
  begin
  result := nil;
  if actionList.count > 0 then
    for i := 0 to actionList.count - 1 do
      begin
      anAction := GsToolAction(actionList.items[i]);
      if anAction.name = self.currentActionName then
        begin
        result := anAction;
        exit;
        end;
      end;
  end;

function GsTool.getHintIfOver(longHint: boolean): string;
  var
    anAction: GsToolAction;
    i: longint;
  begin
  result := self.name;
  if not longHint then exit;
  if actionList.count > 0 then
    for i := 0 to actionList.count - 1 do
      begin
      anAction := GsToolAction(actionList.items[i]);
      if anAction = nil then continue;
      if i = 0 then
        result := result + '. Actions: ' + anAction.name
      else
        result := result + ', ' + anAction.name;
      end;
  if actionList.count > 0 then
    result := result + '.';
  result := result + ' Click and drag to move; right-click to pick up or put down; click while holding to use.';
  end;

function GsTool.getHowToHintIfUsing(overWhat: smallint;
    gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants: boolean): string;
  var
    anAction: GsToolAction;
  begin
  anAction := self.currentAction;
  if anAction = nil then exit;
  result := self.name + ': ' + anAction.name + '. '
      + anAction.getHint(overWhat, gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants);
  result := upperCase(copy(result, 1, 1)) + copy(result, 2, length(result));
  end;

function GsTool.actionForActionType(anActionType: smallint): GsToolAction;
  var
    anAction: GsToolAction;
    i: longint;
  begin
  result := nil;
  if actionList.count > 0 then
    for i := 0 to actionList.count - 1 do
      begin
      anAction := GsToolAction(actionList.items[i]);
      if anAction.actionType = anActionType then
        begin
        result := anAction;
        exit;
        end;
      end;
  end;

function GsTool.createNewCommand(point: TPoint): KfCommand;
  var theAction: GsToolAction;
  begin
  result := nil;
  theAction := self.currentAction;
  {check if picking up tool - always do this in preference to action}
  if self = Domain.toolManager.gloveTool then
    begin
    if Domain.toolManager.findToolAtXY(x, y, nil) <> nil then
      begin
      result := GsPickUpToolCommand.create;
      exit;
      end;
    if Domain.garden.findMulchAtXY(x, y) <> nil then
      begin
      result := GsMoveMulchCommand.create;
      exit;
      end;
    end;
  if theAction <> nil then
    result := theAction.createCommand;
  end;

procedure GsTool.fillComboBoxWithActions(toolActionChoices: TComboBox);
  var
    i: longint;
    theAction: GsToolAction;
  begin
  toolActionChoices.clear;
  if actionList.count > 0 then
    begin
    for i := 0 to actionList.count - 1 do
      begin
      theAction := GsToolAction(actionList[i]);
      toolActionChoices.items.addObject(theAction.name, theAction);
      end;
    toolActionChoices.itemIndex := toolActionChoices.items.indexOf(self.currentActionName);
    if (toolActionChoices.itemIndex = -1) and (toolActionChoices.items.count > 0) then
      begin
      toolActionChoices.itemIndex := 0;
      self.currentActionName := toolActionChoices.items[toolActionChoices.itemIndex];
      end;
    end;
  end;

{PDF FIX - can remove this in production version - or keep for expansion}
{will produce multiple errors when loading collection -
could elimiate dialog or set some flag to show once per load}
{handle reading older version with masks}
function GsTool.verifyClassAndVersionInformation(
	filer: GsFiler; size: longint; const cvirRead, cvirClass: GsClassAndVersionInformationRecord): boolean;
  begin
  result := false;
  try
  result := inherited verifyClassAndVersionInformation(filer, size, cvirRead, cvirClass);
  except
  	on GsExceptionFilerUnexpectedVersionNumber do
   		begin
    	if cvirRead.versionNumber = 2 then
        begin
        ShowMessage('Reading obsolete version 2 tool file - should resave as version 3');
      	result := true;
        end;
    	if cvirRead.versionNumber = 3 then
        begin
        ShowMessage('Reading obsolete version 3 tool file - should resave as version 4');
      	result := true;
        end;
    	end;
  end;
  end;

procedure GsTool.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsTool;
  cvir.versionNumber := 4;
  cvir.additionNumber := 0;
  end;

procedure GsTool.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  filer.streamBitmap(upBitmap);
  if cvir.versionNumber = 2 then filer.streamBitmap(upMask);
  filer.streamBitmap(downBitmap);
  if cvir.versionNumber = 2 then filer.streamBitmap(downMask); 
  filer.streamBitmap(fullBitmap);
  if cvir.versionNumber = 2 then filer.streamBitmap(fullMask);
  if filer.isReading then self.makeMasks;
  filer.streamSmallint(state);
  filer.streamSmallint(x);
  filer.streamSmallint(y);
  filer.streamBoolean(visible);
  filer.streamCursor(cursor);
  filer.streamShortString(name);
  filer.streamPoint(upHotSpot);
  filer.streamPoint(downHotSpot);
  filer.streamPoint(fullHotSpot);
  actionList.streamUsingFiler(filer, GsToolAction);
  filer.streamShortString(currentActionName);
  soundDown.streamUsingFiler(filer);
  soundUsing.streamUsingFiler(filer);
  soundUp.streamUsingFiler(filer);
  soundPickUp.streamUsingFiler(filer);
  soundMoving.streamUsingFiler(filer);
  soundPutDown.streamUsingFiler(filer);
  filer.streamPChar(note);
  end;

procedure GsTool.constrainToRectangle(aRect: TRect);
  begin
  if x >= aRect.right then x := aRect.right - 1;
  if x < aRect.left then x := aRect.left;
  if y >= aRect.bottom then y := aRect.bottom - 1;
  if y < aRect.top then y := aRect.top;
  end;

{GsToolManager}
constructor GsToolManager.create;
  begin
  inherited create;
  tools := TListCollection.create;
  end;

destructor GsToolManager.destroy;
  begin
  {remember - glove is also in list! so do not free it seperately}
  tools.free;
  tools := nil;
  inherited destroy;
  end;

procedure GsToolManager.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsToolManager;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsToolManager.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  tools.streamUsingFiler(filer, GsTool);
  if filer.isReading then
    begin
    gloveTool := self.findGlove;
    self.putDownAllToolsExceptGlove;
    end;
  end;

procedure GsToolManager.putDownAllToolsExceptGlove;
  var
    i: integer;
    theTool: GsTool;
  begin
  { assumes gloveTool pointer set before this function called }
  if tools.count > 0 then for i := 0 to tools.count - 1 do
    begin
    theTool := GsTool(tools.items[i]);
    if (theTool <> self.gloveTool) then theTool.setState(kToolStateDown);
    end;
  end;

{overriden to reduce demand for a continuous chunk of memory for stream}
{ returns true if successful copy }
{pdf - renamed from copyTo to avoid confusion with regular copyTo
and to prevent delphi 2.0 warning}
function GsToolManager.specialCopyTo(newCopy: GsStreamableObject): boolean;
  var
  	i: longint;
    newToolManager: GsToolManager;
    oldTool, newTool: GsTool;
  begin
  result := false;
  newToolManager := newCopy as GsToolManager;
  newToolManager.tools.clear;
  if tools.count > 0 then
    for i := 0 to tools.count - 1 do
      begin
      oldTool := GsTool(tools[i]);
      try
        newTool := GsTool.create;
        oldTool.copyTo(newTool);
        newToolManager.tools.add(newTool);
      except
        exit;
      end;
      end;
  newToolManager.gloveTool := newToolManager.findGlove;
  result := true;
  end;

procedure GsToolManager.constrainToolsToRectangle(aRect: TRect);
  var
    i: integer;
    theTool: GsTool;
  begin
  if tools.count > 0 then for i := 0 to tools.count - 1 do
    begin
    theTool := GsTool(tools.items[i]);
    theTool.constrainToRectangle(aRect);
    end;
  end;

function GsToolManager.findGlove: GsTool;
  var
    i: integer;
    theTool: GsTool;
  begin
  result := nil;
  if tools.count > 0 then for i := 0 to tools.count - 1 do
    begin
    theTool := GsTool(tools.items[i]);
    if theTool.name = 'glove' then
      begin
		  result := theTool;
      exit;
      end;
    end;
  raise Exception.create('Glove tool not found!');
  end;

procedure GsToolManager.bringToolToFront(tool: GsTool);
	begin
  {put tool at end of list}
  Domain.toolManager.tools.remove(tool);
  Domain.toolManager.tools.add(tool);
  end;

var
  gPoint: TPoint;

function toolAtXY(each: TObject; data: TObject) : Boolean;
  begin
  Result := false;
  if (each <> GardenForm.currentTool) and (each <> Domain.toolManager.gloveTool) then
    if (each as GsTool).isPointOnTool(gPoint) then Result := true;
  end;

{currentTool can be nil - it is a tool to exclude from check beside glove tool}
function GsToolManager.findToolAtXY(x: integer; y: integer; currentTool: GsTool): GsTool;
  var
    i: integer;
    theTool: GsTool;
    aPoint: TPoint;
  begin
  result := nil;
  aPoint := Point(x, y);
  if tools.count > 0 then for i := tools.count - 1 downto 0 do
    begin
    theTool := GsTool(tools.items[i]);
    if (theTool <> currentTool) and (theTool <> gloveTool) then
      if theTool.isPointOnTool(aPoint) then
        begin
        Result := theTool;
        exit;
        end;
     end;
  end;

function GsToolManager.findToolForName(name: string): GsTool;
  var
    i: integer;
    theTool: GsTool;
  begin
  result := nil;
  if tools.count > 0 then
  	for i := tools.count - 1 downto 0 do
    	begin
    	theTool := GsTool(tools.items[i]);
    	if (theTool.name = name) then
        begin
        Result := theTool;
        exit;
        end;
     end;
  end;

function GsToolManager.newListCollectionOfToolSettings: TListCollection;
  var
    i: integer;
    theTool: GsTool;
    toolSettings: GsToolSettings;
  begin
  result := TListCollection.create;
  try
  if tools.count > 0 then
  	for i := 0 to tools.count - 1  do
    	begin
    	theTool := GsTool(tools.items[i]);
      toolSettings := GsToolSettings.createWithTool(theTool);
      result.add(toolSettings);
      end;
  except
  result.free;
  result := nil;
  raise;
  end;
  end;

procedure GsToolManager.updateToolSettings(aListCollection: TListCollection);
  var
    i: integer;
    toolSettings: GsToolSettings;
  begin
  if aListCollection.count > 0 then
  	for i := 0 to aListCollection.count - 1 do
    	begin
    	toolSettings := GsToolSettings(aListCollection.items[i]);
      toolSettings.setToolValuesInToolManager(self);
      end;
  end;

{ tool action how-to hints }
function GsToolAction.getHint(overWhat: smallint;
    gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants: boolean): string;
  { note for next version: could change these based on: gardenHasPatches, gardenHasPlants, patchHasPlants: boolean}
  begin
  result := '';
  case actionType of
    kToolActionPullPlant:
      case overWhat of
        kOverPlant:   result := 'To pull this plant, click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To pull a plant in this soil patch, click at its base (where it comes out of the ground).'
          else
            result := '(There are no plants in this soil patch to pull. You must plant a seed here first.)';
        kOverWeather:
          if gardenHasPlants then
            result := 'To pull a plant, click at its base (where it comes out of the ground).'
          else
            result := '(There are no plants in the garden to pull. You must plant a seed first.)';
        end;
    kToolActionHarvestWholePlant:
      case overWhat of
        kOverPlant:   result := 'To harvest this plant''s edible parts *and* pull it up, click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To harvest the edible parts from a plant in this soil patch *and* pull the plant up, '
                + 'click at its base.'
          else
            result := '(There are no plants in this soil patch to harvest. You must plant a seed here first.)';
        kOverWeather:
          if gardenHasPlants then
            result := 'To harvest the edible parts from a plant *and* pull the plant up, click at its base.'
          else
            result := '(There are no plants in the garden to harvest. You must plant a seed first.)';
        end;
    kToolActionHarvestMatureParts:
      case overWhat of
        kOverPlant: result := 'To harvest this plant''s mature edible parts *without* pulling it up, click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To harvest mature edible parts from a plant in this soil patch *without* pulling it up, '
                + 'click at its base.'
          else
            result := '(There are no plants in this soil patch to harvest. You must plant a seed here first.)';
        kOverWeather:
          if gardenHasPlants then
            result := 'To harvest mature edible parts from a plant *without* pulling it up, click at its base.'
          else
            result := '(There are no plants in the garden to harvest. You must plant a seed first.)';
        end;
    kToolActionRename:
      case overWhat of
        kOverPlant:   result := 'To rename this plant, click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To rename this soil patch, click here. To rename a plant in this patch, click at its base.'
          else
            result := 'To rename this soil patch, click here.';
        kOverWeather:
          if gardenHasPlants then
            result := 'To rename a soil patch, click inside it. To rename a plant, click at its base.'
          else if gardenHasPatches then
            result := 'To rename a soil patch, click inside it.'
          else
            result := '(There are no soil patches or plants to rename. You must create a soil patch first.)';
        end;
    kToolActionTransplant:
      case overWhat of
        kOverPlant:
          if atLeastTwoPatches then
            result := 'To transplant this plant within this patch or to another patch, click here and drag.'
          else
            result := 'To move this plant around within this patch, click here and drag.';
        kOverPatch:
          if patchHasPlants then
            begin
            if atLeastTwoPatches then
              result := 'To transplant a plant within this patch or to another patch, click at its base and drag.'
            else
              result := 'To move a plant around within this patch, click at its base and drag.';
            end
          else
            result := '(There are no plants in this soil patch to transplant. You must plant a seed here first.)';
        kOverWeather:
          if gardenHasPlants then
            result := 'To transplant a plant within its soil patch or to another patch, click at its base and drag.'
          else
            result := '(There are no plants in the garden to transplant. You must plant a seed first.)';
        end;
    kToolActionApply:
      case overWhat of
        kOverPlant,
        kOverPatch:   result := 'To apply ' + param1 + ' to this soil patch, click here.';
        kOverWeather:
          if gardenHasPatches then
            result := 'To apply ' + param1 + ' to a soil patch, click inside it.'
          else
            result := '(There are no soil patches to apply a soil amendment to. You must create a soil patch first.)';
        end;
    kToolActionPlant:
      case overWhat of
        kOverPlant:
          result := 'There is already a plant here! To plant a ' + param1
              + ', click somewhere else inside this soil patch.';
        kOverPatch:   result := 'To plant a ' + param1 + ', click anywhere inside this soil patch.';
        kOverWeather:
          if gardenHasPatches then
            result := 'To plant a ' + param1 + ', click inside a soil patch.'
          else
            result := '(There are no soil patches to plant in. You must create a soil patch first.)';
        end;
    kToolActionScan:
      case overWhat of
        kOverPlant:   result := 'To show this plant in the browser (numbers side), click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To show this soil patch in the browser (numbers side), click here. '
                + 'To show a plant, click at its base.'
          else
            result := 'To show this soil patch in the browser (numbers side), click here.';
        kOverWeather:
          if gardenHasPlants then
            result := 'To show a plant in the browser (numbers side), click at its base. '
                + 'To show a soil patch, click inside it.'
          else if gardenHasPatches then
            result := 'To show a soil patch in the browser (numbers side), click inside it.'
          else
            result := '(There are no soil patches or plants to scan. You must create a soil patch first.)';
        end;
    kToolActionMagnify:
      case overWhat of
        kOverPlant:   result := 'To show this plant in the browser (pictures side), click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To show this soil patch in the browser (pictures side), click here. '
                + 'To show a plant, click at its base.'
          else
            result := 'To show this soil patch in the browser (pictures side), click here.';
        kOverWeather:
          if gardenHasPlants then
            result := 'To show a plant in the browser (pictures side), click at its base. '
                + 'To show a soil patch, click inside it.'
          else if gardenHasPatches then
            result := 'To show a soil patch in the browser (pictures side), click inside it.'
          else
            result := '(There are no soil patches or plants to magnify. You must create a soil patch first.)';
        end;
    kToolActionScanAndMagnify:
      case overWhat of
        kOverPlant:   result := 'To show this plant in the browser (both sides), click here.';
        kOverPatch:   
          if patchHasPlants then
            result := 'To show this soil patch in the browser (both sides), click here. '
                + 'To show a plant, click at its base.'
          else
            result := 'To show this soil patch in the browser (both sides), click here.';
        kOverWeather:
          if gardenHasPlants then
            result := 'To show a plant in the browser (both sides), click at its base. '
                + 'To show a soil patch, click inside it.'
          else if gardenHasPatches then
            result := 'To show a soil patch in the browser (both sides), click inside it.'
          else
            result := '(There are no soil patches or plants to scan and magnify. You must create a soil patch first.)';
        end;
    kToolActionNewSoilPatch:
      case overWhat of
        kOverPlant,
        kOverPatch:   result := 'To create a new soil patch, move outside of this soil patch, then click and drag.';
        kOverWeather: result := 'To create a new soil patch, click and drag outside of any existing soil patches.';
        end;
    kToolActionChangeSoilPatch:
       case overWhat of
        kOverPlant,
        kOverPatch:   result := 'To change important soil patch parameters, click here.';
        kOverWeather:
          if gardenHasPatches then
            result := 'To change important soil patch parameters, click inside a soil patch.'
          else
            result := '(There are no soil patches to change parameters for. You must create a soil patch first.)';
        end;
    kToolActionShapeSoilPatch:
       case overWhat of
         kOverPlant,
         kOverPatch:   result := 'To reshape this soil patch, click just inside a corner and drag the corner.';
         kOverWeather:
           if gardenHasPatches then
             result := 'To reshape a soil patch, click just inside a corner and drag the corner.'
           else
             result := '(There are no soil patches to reshape. You must create a soil patch first.)';
         end;
    kToolActionDragSoilPatch:
       case overWhat of
         kOverPlant,
         kOverPatch:
           if patchHasPlants then
             result := 'To move this soil patch and its plants, click here and drag.'
           else
             result := 'To move this soil patch, click here and drag.';
         kOverWeather:
           if gardenHasPatches then
             result := 'To move a soil patch and any plants on it, click inside the soil patch and drag.'
           else
             result := '(There are no soil patches to drag. You must create a soil patch first.)';
         end;
 	  kToolActionMixSoil:
       case overWhat of
         kOverPlant,
         kOverPatch:   result := 'To mix the soil, click here.';
         kOverWeather:
           if gardenHasPatches then
             result := 'To mix the soil in a soil patch, click inside it.'
           else
             result := '(There are no soil patches to mix soil in. You must create a soil patch first.)';
         end;
    kToolActionDoubleDigSoil:
       case overWhat of
         kOverPlant,
         kOverPatch:   result := 'To double-dig the soil (mix it well and deeply), click here.';
         kOverWeather:
           if gardenHasPatches then
             result := 'To double-dig the soil (mix it well and deeply) in a soil patch, click inside it.'
           else
             result := '(There are no soil patches to double-dig soil in. You must create a soil patch first.)';
         end;
    kToolActionCarrySoil:
       case overWhat of
         kOverPlant,
         kOverPatch:
           if atLeastTwoPatches then
             result := 'To move some soil to another soil patch, click here and drag to another patch.'
           else
             result := '(There is no other soil patch to carry soil to. You must create another soil patch first.)';
         kOverWeather:
           if atLeastTwoPatches then
             result := 'To move some soil from one soil patch to another, click in one patch and drag to another.'
           else
             result := '(There aren''t enough soil patches to move soil between. '
                 + 'You must create at least two soil patches first.)';
         end;
    kToolActionRemoveSoilPatch:
       case overWhat of
         kOverPlant,
         kOverPatch:
           if patchHasPlants then
             result := 'To remove this soil patch and all its plants from the simulation, click here.'
           else
             result := 'To remove this soil patch from the simulation, click here.';
         kOverWeather:
           if gardenHasPatches then
             result := 'To remove a soil patch and any plants on it from the simulation, click inside it.'
           else
             result := '(There are no soil patches to remove. You must create a soil patch first.)';
         end;
    kToolActionCultivateSoil:
       case overWhat of
         kOverPlant,
         kOverPatch:   result := 'To cultivate the soil (mix it shallowly), click here.';
         kOverWeather: 
           if gardenHasPatches then
             result := 'To cultivate a soil patch (mix it shallowly), click inside it.'
           else
             result := '(There are no soil patches to cultivate soil in. You must create a soil patch first.)';
         end;
    kToolActionWater:
       case overWhat of
         kOverPlant,
         kOverPatch:   result := 'To water the soil in this soil patch, click here. You cannot water individual plants.';
         kOverWeather: 
           if gardenHasPatches then
             result := 'To water the soil in a soil patch, click inside it.'
           else
             result := '(There are no soil patches to water. You must create a soil patch first.)';
         end;
    kToolActionGrowPlant:
      case overWhat of
        kOverPlant:   result := 'To grow this plant instantly for the number of days shown, click here.';
        kOverPatch:
          if patchHasPlants then
            result := 'To grow a plant in this soil patch instantly for the number of days shown, click at its base.'
          else
            result := '(There are no plants in this soil patch to grow. You must plant a seed here first.)';
         kOverWeather:
          if gardenHasPlants then
            result := 'To grow a plant instantly for the number of days shown, click at its base.'
          else
            result := '(There are no plants in the garden to grow. You must plant a seed first.)';
         end;
    kToolActionZapPlant: {pull all plants in patch}
      case overWhat of
        kOverPlant,
        kOverPatch:
          if patchHasPlants then
            result := 'To pull up all the plants in this patch, click here. '
              + '(To pull up one plant, use the ''pull'' tool action.)'
          else
            result := '(There are no plants in this soil patch to pull. You must plant a seed here first.)';
         kOverWeather: 
          if gardenHasPlants then
            result := 'To pull up all the plants in a soil patch, click inside it.'
          else
            result := '(There are no plants in the garden to pull. You must plant a seed first.)';
         end;
    kToolActionAerateSoil:
       case overWhat of
         kOverPlant,
         kOverPatch:   result := 'To aerate the soil in this soil patch, click here.';
         kOverWeather:
           if gardenHasPatches then
             result := 'To aerate the soil in a soil patch, click inside it.'
           else
             result := '(There are no soil patches to aerate. You must create a soil patch first.)';
         end;
    kToolActionCarryMulch:
       case overWhat of
         kOverPlant,
         kOverPatch:
           if atLeastTwoPatches then
             result := 'To move some mulch to another soil patch, click here and drag to another patch.'
           else
             result := '(There is no other soil patch to carry mulch to. You must create another soil patch first.)';
         kOverWeather:
           if atLeastTwoPatches then
             result := 'To move some mulch from one soil patch to another, click in one patch and drag to another.'
           else
             result := '(There aren''t enough soil patches to move mulch between. '
                 + 'You must create at least two soil patches first.)';
         end;
    kToolActionSupport:
    	result := 'This action is not supported in this version.';
    kToolActionReseed:
       case overWhat of
         kOverPlant:   result := 'To start this plant over from a seed or seedling, click here.';
         kOverPatch:
          if patchHasPlants then
            result := 'To start a plant in this soil patch over from a seed or seedling, click at its base.'
          else
            result := '(There are no plants in this soil patch to reseed. You must plant a seed here first.)';
         kOverWeather:
          if gardenHasPlants then
            result := 'To start a plant over from a seed or seedling, click at its base.'
          else
            result := '(There are no plants in the garden to reseed. You must plant a seed first.)';
         end;
    kToolActionDuplicate:
       case overWhat of
         kOverPlant:   result := 'To make an identical copy of this plant, click here.';
         kOverPatch:
           if patchHasPlants then
             result := 'To make an identical copy of this soil patch and its plants, click here. '
                 + 'To copy one plant, click at its base.'
           else
             result := 'To make an identical copy of this soil patch, click here.';
         kOverWeather:
           if gardenHasPlants then
             result := 'To copy a soil patch and any plants on it, click inside it. To copy a plant, click at its base.'
           else if gardenHasPatches then
             result := 'To copy a soil patch, click inside it.'
           else
            result := '(There are no soil patches or plants to copy. You must create a soil patch first.)';
         end;
    kToolActionStopOrStart:
       case overWhat of
         kOverPlant:   result := 'To place this plant in stasis or take it out again, click here.';
         kOverPatch:
          if patchHasPlants then
            result := 'To place a plant in this soil patch in stasis or take it out again, click at its base.'
          else
            result := '(There are no plants in this soil patch to place in stasis. You must plant a seed here first.)';
         kOverWeather:
          if gardenHasPlants then
            result := 'To place a plant in stasis or take it out again, click at its base.'
          else
            result := '(There are no plants in the garden to place in stasis. You must plant a seed first.)';
         end;
  else
    raise Exception.create('Unimplemented tool action');
  end;
  end;

{ defaulting }
procedure GsToolManager.createDefaultGlove;
	var
  tool: GsTool;
  begin
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'glove';
    state := kToolStateUp;
    upHotSpot := Point(0,0);
    downHotSpot := Point(0, 0);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
    {loadBitmaps('c:\GARDNSIM\BMP256\glove.bmp', 'c:\GARDNSIM\BMP256\glove.bmp', ''); }
    actionList.add(GsToolAction.createWithActionType(kToolActionPullPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionHarvestWholePlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionHarvestMatureParts));
    actionList.add(GsToolAction.createWithActionType(kToolActionRename));
    actionList.add(GsToolAction.createWithActionType(kToolActionTransplant));
    actionList.add(GsToolAction.createWithActionType(kToolActionNewSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionChangeSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionShapeSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionDragSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionMixSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionDoubleDigSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionCarrySoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionRemoveSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionCultivateSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionAerateSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionCarryMulch));
    actionList.add(GsToolAction.createWithActionType(kToolActionGrowPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionZapPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionDuplicate));
    actionList.add(GsToolAction.createWithActionType(kToolActionReseed));
    actionList.add(GsToolAction.createWithActionType(kToolActionStopOrStart));
    actionList.add(GsToolAction.createWithActionType(kToolActionSupport));
    actionList.add(GsToolAction.createWithActionType(kToolActionWater));
    actionList.add(GsToolAction.createWithActionType(kToolActionApply));
    actionList.add(GsToolAction.createWithActionType(kToolActionPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionScan));
    actionList.add(GsToolAction.createWithActionType(kToolActionMagnify));
    actionList.add(GsToolAction.createWithActionType(kToolActionScanAndMagnify));
    currentActionName := 'new soil patch';
    end;
  gloveTool := self.findGlove;
  end;

procedure GsToolManager.loadDefaultTools;
	var
  tool: GsTool;
  begin
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'glove';
    state := kToolStateUp;
    upHotSpot := Point(60, 18);
    downHotSpot := Point(60, 18);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
    loadBitmaps('c:\GARDNSIM\BMP256\glove.bmp', 'c:\GARDNSIM\BMP256\glove.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionPullPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionHarvestWholePlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionHarvestMatureParts));
    actionList.add(GsToolAction.createWithActionType(kToolActionRename));
    actionList.add(GsToolAction.createWithActionType(kToolActionTransplant));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'shovel';
    upHotSpot := Point(42, 215);
    downHotSpot := Point(27, 218);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\shovel_u.bmp', 'c:\GARDNSIM\BMP256\shovel_d.bmp',
        'c:\gardnsim\bmp256\shovel_f.bmp');
    actionList.add(GsToolAction.createWithActionType(kToolActionNewSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionChangeSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionShapeSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionDragSoilPatch));
    actionList.add(GsToolAction.createWithActionType(kToolActionMixSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionDoubleDigSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionCarrySoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionRemoveSoilPatch));
    currentActionName := 'new soil patch';
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'hoe';
    upHotSpot := Point(137, 121);
    downHotSpot := Point(4, 168);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\hoe_u.bmp', 'c:\GARDNSIM\BMP256\hoe_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionCultivateSoil));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'pitchfork';
    upHotSpot := Point(0,0);
    downHotSpot := Point(0,0);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\pitch_u.bmp', 'c:\GARDNSIM\BMP256\pitch_d.bmp',
        'c:\gardnsim\bmp256\pitch_f.bmp');
    actionList.add(GsToolAction.createWithActionType(kToolActionAerateSoil));
    actionList.add(GsToolAction.createWithActionType(kToolActionCarryMulch));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'magic wand';
    upHotSpot := Point(128, 44);
    downHotSpot := Point(73, 18);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\wand_u.bmp', 'c:\GARDNSIM\BMP256\wand_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionGrowPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionZapPlant));
    actionList.add(GsToolAction.createWithActionType(kToolActionRename));
    actionList.add(GsToolAction.createWithActionType(kToolActionDuplicate));
    actionList.add(GsToolAction.createWithActionType(kToolActionReseed));
    actionList.add(GsToolAction.createWithActionType(kToolActionStopOrStart));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'stake';
    upHotSpot := Point(0,0);
    downHotSpot := Point(0,0);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\stake_u.bmp', 'c:\GARDNSIM\BMP256\stake_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionSupport));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'watering can';
    upHotSpot := Point(128, 56);
    downHotSpot := Point(99, 40);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
 	  loadBitmaps('c:\GARDNSIM\BMP256\water_u.bmp', 'c:\GARDNSIM\BMP256\water_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionWater));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'bag';
    upHotSpot := Point(101, 76);
    downHotSpot := Point(78, 6);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
    loadBitmaps('c:\GARDNSIM\BMP256\bag_u.bmp', 'c:\GARDNSIM\BMP256\bag_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionApply));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'seed packet';
    upHotSpot := Point(75, 53);
    downHotSpot := Point(34, 2);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\seedpt_u.bmp', 'c:\GARDNSIM\BMP256\seedpt_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionPlant));
    end;
	tool := GsTool.create;
	tools.add(tool);
  with tool do
    begin
    name := 'growcorder';
    upHotSpot := Point(29, 96);
    downHotSpot := Point(90, 55);
    x := x + downHotSpot.x;
    y := y + downHotSpot.y;
  	loadBitmaps('c:\GARDNSIM\BMP256\gcord_u.bmp', 'c:\GARDNSIM\BMP256\gcord_d.bmp', '');
    actionList.add(GsToolAction.createWithActionType(kToolActionScan));
    actionList.add(GsToolAction.createWithActionType(kToolActionMagnify));
    actionList.add(GsToolAction.createWithActionType(kToolActionScanAndMagnify));
    end;
  gloveTool := self.findGlove;
  end;

procedure GsToolParamListManager.hardCodeLists;
  begin
  with depthPlantingList do
    begin
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountAuto, 0.0, kLengthCentimeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 1.0, kLengthCentimeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 2.0, kLengthCentimeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 3.0, kLengthCentimeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 4.0, kLengthCentimeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 5.0, kLengthCentimeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 0.25, kLengthInches));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 0.5, kLengthInches));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 0.75, kLengthInches));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 1.0, kLengthInches));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 1.5, kLengthInches));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthPlanting, kNone, kAmountPerClick, 2.0, kLengthInches));
    end;
  with depthMixingList do
    begin
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 0.5, kLengthMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 1.0, kLengthMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 1.5, kLengthMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 2.0, kLengthMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 1.0, kLengthFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 2.0, kLengthFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 3.0, kLengthFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 4.0, kLengthFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeDepthMixing, kNone, kAmountPerClick, 5.0, kLengthFeet));
    end;
  with toolAreaList do
    begin
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 0.25, kAreaSquareMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 0.5, kAreaSquareMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 1.0, kAreaSquareMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 2.0, kAreaSquareMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 3.0, kAreaSquareMeters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 0.5, kAreaSquareFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 1.0, kAreaSquareFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 2.0, kAreaSquareFeet));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeToolArea, kNone, kAmountPerClick, 3.0, kAreaSquareFeet));
    end;
  with amountCarryingList do
    begin
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 0.25, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 0.5, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 0.75, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 1.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 1.5, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 2.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 0.25, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 0.5, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 0.75, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 1.0, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 1.5, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 2.0, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountCarrying, kSolid, kAmountPerClick, 3.0, kMassPounds));
    end;
  with amountSolidList do
    begin
    { amount per click }
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerClick, 1.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerClick, 2.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerClick, 3.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerClick, 1.0, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerClick, 2.0, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerClick, 3.0, kMassPounds));
    { amount per second }
    { hiding amount per second for now }
  {  add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerSecond, 1.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerSecond, 2.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerSecond, 3.0, kMassKilograms));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerSecond, 1.0, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerSecond, 2.0, kMassPounds));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountSolid, kSolid, kAmountPerSecond, 3.0, kMassPounds));  }
    { amount per area }
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerClick, 1.0, kMassOverAreaKilogramsPSquareMeter));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerClick, 2.0, kMassOverAreaKilogramsPSquareMeter));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerClick, 3.0, kMassOverAreaKilogramsPSquareMeter));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerClick, 1.0, kMassOverAreaPoundsPSquareFoot));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerClick, 2.0, kMassOverAreaPoundsPSquareFoot));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerClick, 3.0, kMassOverAreaPoundsPSquareFoot));
    { amount per area per second }
 {   add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerSecond, 1.0, kMassOverAreaKilogramsPSquareMeter));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerSecond, 2.0, kMassOverAreaKilogramsPSquareMeter));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerSecond, 3.0, kMassOverAreaKilogramsPSquareMeter));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerSecond, 1.0, kMassOverAreaPoundsPSquareFoot));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerSecond, 2.0, kMassOverAreaPoundsPSquareFoot));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountSolid, kSolid, kAmountPerAreaPerSecond, 3.0, kMassOverAreaPoundsPSquareFoot)); }
    end;
  with amountLiquidList do
    begin
    { amount per click }
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerClick, 1.0, kVolumeLiters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerClick, 2.0, kVolumeLiters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerClick, 3.0, kVolumeLiters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerClick, 1.0, kVolumeGallons));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerClick, 2.0, kVolumeGallons));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerClick, 3.0, kVolumeGallons));
    { amount per second }
 {   add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerSecond, 1.0, kVolumeLiters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerSecond, 2.0, kVolumeLiters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerSecond, 3.0, kVolumeLiters));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerSecond, 1.0, kVolumeGallons));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerSecond, 2.0, kVolumeGallons));
    add(GsToolRateOrAmountParam.createWithInfo(kListTypeAmountLiquid, kLiquid, kAmountPerSecond, 3.0, kVolumeGallons)); }
    { amount per area }
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerClick, 1.0, kDepthOfWaterMillimeters));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerClick, 2.0, kDepthOfWaterMillimeters));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerClick, 3.0, kDepthOfWaterMillimeters));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerClick, 1.0, kDepthOfWaterInches));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerClick, 2.0, kDepthOfWaterInches));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerClick, 3.0, kDepthOfWaterInches));
    { amount per area per second }
 {   add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerSecond, 1.0, kDepthOfWaterMillimeters));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerSecond, 2.0, kDepthOfWaterMillimeters));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerSecond, 3.0, kDepthOfWaterMillimeters));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerSecond, 1.0, kDepthOfWaterInches));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerSecond, 2.0, kDepthOfWaterInches));
    add(GsToolRateOrAmountParam.createWithInfo(
      kListTypeAmountLiquid, kLiquid, kAmountPerAreaPerSecond, 3.0, kDepthOfWaterInches));  }
    end;
  end;

end.
{
	Copyright (c) 1996 Paul D. Fernhout and Cynthia F. Kurtz
  								 All rights reserved

  Notice: This program contains trade secrets that are the property of
  Paul D. Fernhout and Cynthia F. Kurtz. The contents may not be used or disclosed in part or
  in whole without express written permission of the owners.
  Internet: pdfernhout @ bix.com, cfkurtz @ igc.apc.org, kfsoft @ netins.net
}


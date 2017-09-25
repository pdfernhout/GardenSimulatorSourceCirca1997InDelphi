unit UDPlant;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udplant: Drawing plant. Draws plant at any time, contains information on how to draw
plant, models partitioning of plant biomass into individual plant parts (meristems,
internodes, leaves, inflorescences, flowers, fruits). Drawing plant model has NO feedback
to rest of plant model and is for display only. This is because it is not very accurate
in terms of partitioning and size. The main idea here is give users an idea of how the
plant is doing visually so they can see how well it is growing and what they need to
do to keep it healthy. Also in this file is the traverser object, which moves through
the drawing plant structure calling specific functions of all the drawing plant parts
with several activities such as demanding photosynthate, growing, drawing, etc.
The traverser was created to get around stack size limits in Delphi 1.0; otherwise it would
not have been needed and these functions could have been recursive. The access-> comments are
for the tab2asp program to read when it checks for correspondence between these variables and aspects.}

interface

uses WinProcs, WinTypes, Graphics, Classes, UTurt3d, UCollect, UDebug, Dialogs, uestruct, umodel,
  ufiler, ufilertx, urandom, uharvprt, uicon;

const
	kGenderMale = 0; kGenderFemale = 1; kGenderHermaphroditic = 2; kGenderUnassigned = 3;
	kCompoundLeafPinnate = 0; kCompoundLeafPalmate = 1;
  kStageFlowerBud = 1; kStageOpenFlower = 2; kStageUnripeFruit = 3; kStageRipeFruit = 4;
    kStageFallenFruit = 5; kStageFallenFlower = 6;
  kPartTypeNone = 0; kPartTypeFlowerFruit = 1; kPartTypeInflorescence = 2; kPartTypeMeristem = 3;
    kPartTypePhytomer = 4; kPartTypeLeaf = 5;
  kDirectionLeft = 0; kDirectionRight = 1;
  kTraverseNone = 0; kTraverseLeft = 1; kTraverseRight = 2; kTraverseNext = 3; kTraverseDone = 4;
  kActivityNone = 0;
  kActivityNextDay = 1;
  kActivityDemandVegetative_kg = 2;
  kActivityDemandReproductive_kg = 3;
  kActivityGrowVegetative = 4;
  kActivityGrowReproductive = 5;
  kActivityStartReproduction = 6;
  kActivityFindPlantPartAtPosition = 7;
  kActivityFindAgeOfYoungestPhytomer = 8;
  kActivityDraw = 9;
  kActivityReport = 10;
  kActivityStream = 11;
  kActivityFree = 12;
  kActivityVegetativeBiomassThatCanBeRemoved_kg = 13;
  kActivityRemoveVegetativeBiomass = 14;
  kActivityReproductiveBiomassThatCanBeRemoved_kg = 15;
  kActivityRemoveReproductiveBiomass = 16;
  kActivityAddMatureHarvestedPartsToList = 17;
  kActivityAddBundledPartsToList = 18;
  kActivityGatherStatistics = 19;
  kActivityStandingDeadBiomassThatCanBeRemoved_kg = 20;
  kActivityRemoveStandingDeadBiomass = 21;

type
  SinglePoint = record
    x: single;
    y: single;
    reservedZ: single;
    end;

{ aspects->start }
{ object->drawingPlant }
{ access->pGeneral }
DrawingParamsGeneral = record
  harvestItemTemplatesArray: array[0..kHarvestPartTypeLast] of GsHarvestItemTemplate;
  harvestItemIncludeInWholePlantHarvestBiomass: array[0..kHarvestPartTypeLast] of boolean;
  storageOrganIsLumpedInWith: smallint;
  randomSway: single;
  numApicalInflors: smallint;
  numAxillaryInflors: smallint;
  gender: smallint{ENUM};
  lineDivisions: smallint;
  isDicot: boolean;
  maleFlowersAreSeparate: boolean;
  reservedArray: array [1..160] of byte;
  end;

{ access->pSeedlingLeaf }
DrawingParamsSeedlingLeaf = record
  object3D: KfObject3D;
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  scale: single;
  nodesOnStemWhenFallsOff: smallint;
  reservedArray: array [1..40] of byte;
  end;

{ access->pLeaf }
DrawingParamsLeaf = record
  object3D: KfObject3D;
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  petioleColor: TColorRef;
  optimalBiomass_kg: single;
  optimalFractionOfOptimalBiomassAtCreation_kg: single;
  minDaysToGrow: smallint;
  maxDaysToGrow: smallint;
  scaleAtOptimalBiomass: single;
  petioleLengthAtOptimalBiomass_mm: single;
  petioleWidthAtOptimalBiomass_mm: single;
  petioleAngle: single;
  compoundPetioleToRachisRatio: single;
  compoundPinnateOrPalmate: smallint{ENUM};
  compoundNumLeaflets: smallint;
  fractionOfLiveBiomassWhenAbscisses_frn: single;
  reservedArray: array [1..36] of byte; {was 40, added one single (4), now is 36}
  end;

{ access->pInternode }
DrawingParamsInternode = record
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  meanderIndex: single;
  flexibilityIndex: single;
  { creation }
  minFractionOfOptimalInitialBiomassToCreateInternode_frn: single;
  minDaysToCreateInternode: smallint;
  maxDaysToCreateInternodeIfOverMinFraction: smallint;
  { growth and expansion }
  optimalFinalBiomass_kg: single;
  canRecoverFromStuntingDuringCreation: boolean;
  minDaysToAccumulateBiomass: smallint;
  maxDaysToAccumulateBiomass: smallint;
  minDaysToExpand: smallint;
  maxDaysToExpand: smallint;
  lengthAtOptimalFinalBiomassAndExpansion_mm: single;
  lengthMultiplierDueToBiomassAccretion: single;
  lengthMultiplierDueToExpansion: single;
  widthAtOptimalFinalBiomassAndExpansion_mm: single;
  widthMultiplierDueToBiomassAccretion: single;
  widthMultiplierDueToExpansion: single;
  { bolting }
  lengthMultiplierDueToBolting: single;
  minDaysToBolt: smallint;
  maxDaysToBolt: smallint;
  reservedArray: array [1..40] of byte;
  end;

{ access->pFlower }
DrawingParamsFlower = record
  object3D: KfObject3D;
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  optimalBiomass_kg: single;
  minFractionOfOptimalInitialBiomassToCreateFlower_frn: single;
  minDaysToCreateFlowerBud: smallint;
  maxDaysToCreateFlowerBudIfOverMinFraction: smallint;
  minFractionOfOptimalBiomassToOpenFlower_frn: single;
  minDaysToGrow: smallint;
  maxDaysToGrowIfOverMinFraction: smallint;
  daysBeforeDrop: smallint;
  scaleAtFullSize: single;
  numPetals: smallint;
  petalsAreRadiallyArranged: boolean;
  budObject3D: KfObject3D;
  budFaceColor: TColorRef;
  budBackfaceColor: TColorRef;
  budScaleAtFullSize: single;
  budNumPetals: smallint;
  budPetalsAreRadiallyArranged: boolean;
  minFractionOfOptimalBiomassToCreateFruit_frn: single; {new}
  reservedArray: array [1..36] of byte; {was 40, added 1 single=4, is 36}
  end;

{ access->pInflor }
DrawingParamsInflorescence = record
  optimalBiomass_kg: single;
  minFractionOfOptimalBiomassToCreateInflorescence_frn: single;
  minDaysToCreateInflorescence: smallint;
  maxDaysToCreateInflorescenceIfOverMinFraction: smallint;
  minFractionOfOptimalBiomassToMakeFlowers_frn: single;
  minDaysToGrow: smallint;
  maxDaysToGrow: smallint;
  peduncleLength_mm: single;
  internodeLength_mm: single;
  internodeWidth_mm: single;
  pedicelLength_mm: single;
  pedicelAngle: single;
  branchAngle: single;
  terminalStalkLength_mm: single;
  numFlowers: smallint;
  daysToAllFlowersCreated: smallint;
  numBranches: smallint;
  branchesAreAlternate: boolean;
  isHead: boolean;
  isTerminal: boolean;
  flowersDrawTopToBottom: boolean;
  flowersSpiralOnStem: boolean;
  stalkColor: TColorRef;
  reservedArray: array [1..40] of byte;
  end;

{ access->pAxillaryBud }
DrawingParamsAxillaryBud = record
  object3D: KfObject3D;
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  scale: single;
  reservedArray: array [1..40] of byte;
  end;

{ access->pMeristem }
DrawingParamsMeristem = record
  branchingIndex: single;
  branchingDistance: single;
  determinateProbability: single;
  branchingIsSympodial: boolean;
  branchingIsAlternate: boolean;
  reservedArray: array [1..40] of byte;
  end;

{ access->pFruit }
DrawingParamsFruit = record
  object3D: KfObject3D;
  sCurveParams: SCurveStructure;
  unripeFaceColor: TColorRef;
  unripeBackfaceColor: TColorRef;
  ripeFaceColor: TColorRef;
  ripeBackfaceColor: TColorRef;
  scaleAtFullSize: single;
  optimalBiomass_kg: single;
  unused1{minFractionOfOptimalInitialBiomassToCreateFruit_frn}: single;
  unused2{minDaysToCreateFruit}: smallint;
  unused3{maxDaysToCreateFruitIfOverMinFraction}: smallint;
  minDaysToGrow: smallint;
  maxDaysToGrow: smallint;
  stalkStrengthIndex: single;
  daysToRipen: smallint;
  numSections: smallint;
  sectionsAreRadiallyArranged: boolean;
  reservedArray: array [1..40] of byte;
  end;

{ access->pRoot }
DrawingParamsRoot = record
  object3D: KfObject3D;
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  scaleAtFullSize: single;
  showsAboveGround: boolean;
  reservedArray: array [1..40] of byte;
  end;
{ aspects->stop }

  GsDrawingPlant = class;
  GsTraverser = class;

GsDrawingPlantStatistics = class(TObject)
  public
  count: array[0..kStatisticsPartTypeLast] of longint;
  liveBiomass_kg: array[0..kStatisticsPartTypeLast] of single;
  deadBiomass_kg: array[0..kStatisticsPartTypeLast] of single;
  procedure zeroAllFields;
  function totalBiomass_kg: single;
  end;

GsDrawingPlant = class(GsModel)
  public
  model: GsModel;
  turtle: KfTurtle;
  firstPhytomer: GsStreamableObject;
  randomNumberGenerator: GsRandom;
  pGeneral: DrawingParamsGeneral;
  pMeristem: DrawingParamsMeristem;
  pInternode: DrawingParamsInternode;
  pLeaf: DrawingParamsLeaf;
  pSeedlingLeaf: DrawingParamsSeedlingLeaf;
  pAxillaryBud: DrawingParamsAxillaryBud;
  pInflor: array[0..1] of DrawingParamsInflorescence;
  pFlower: array[0..1] of DrawingParamsFlower;
  pFruit: DrawingParamsFruit;
  pRoot: DrawingParamsRoot;
  unallocatedNewVegetativeBiomass_kg: single;
  unremovedDeadVegetativeBiomass_kg: single;
  unallocatedNewReproductiveBiomass_kg: single;
  unremovedDeadReproductiveBiomass_kg: single;
  unremovedStandingDeadBiomass_kg: single;
  ageOfYoungestPhytomer: smallint;
  wiltingPercent: smallint;
  numApicalActiveReproductiveMeristemsOrInflorescences: longint;
  numAxillaryActiveReproductiveMeristemsOrInflorescences: longint;
  numApicalInactiveReproductiveMeristems: longint;
  numAxillaryInactiveReproductiveMeristems: longint;
  ageAtWhichFloweringStarted: smallint;
  initialRotationAngle: single;
  swaySeedToday: longint;
  swayRandomNumberGenerator: GsRandom;
  floweringHasStarted: boolean;
  needToRecalculateColors: boolean;
  constructor create; override;
  constructor createWithModelPlant(aPlant: GsModel);
	function maximumLineWidth: single;
	procedure randomizeFromTime;
  procedure drawWithTurtle(aTurtle: KfTurtle; rotateAngle, topOrSideAngle: single);
	procedure initialize;
  destructor destroy; override;
	procedure nextDay;
  procedure reduceStandingDeadFromTransferToResidue;
	procedure plantWideDrawingVarsWithTraverser(traverser: GsTraverser);
  procedure plantWideDrawingVars;
	function findPlantPartAtPosition(point: TPoint): GsStreamableObject;
  procedure addAllMaturePlantPartsToList(aList: TList);
  procedure addAllBundledPlantPartsToList(aList: TList);
  procedure getDrawingPlantStatistics(statistics: GsDrawingPlantStatistics);
	procedure report;
  procedure directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure transferHarvestItemTemplate(direction: integer;
    var myHarvestItemTemplate: GsHarvestItemTemplate; var otherHarvestItemTemplate: GsHarvestItemTemplate);
  procedure transferObject3D(direction: integer; var myTdo: KfObject3D; var otherTdo: KfObject3D);
  procedure transferStorageOrganLumping(direction: smallint; var value: smallint);
  procedure transferHarvestBundling(direction: integer; var value: boolean; fieldIndex: integer);
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure streamDataWithoutParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  procedure streamParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  procedure becomeGenericPlant;
  procedure freeAllDrawingPlantParts;
  procedure becomeSeed;
  procedure allocateOrRemoveBiomassWithTraverser(traverser: GsTraverser);
  procedure allocateOrRemoveParticularBiomass(biomassToAddOrRemove_kg: single;
    var undistributedBiomass_kg: single; askingMode, tellingMode: integer; traverser: GsTraverser);
  function firstAvailableIcon: TIcon;
  function iconForHarvesting(harvestWholePlant: boolean): TIcon;
  function mustBePulledToHarvest: boolean;
  procedure setTdoInUseFlags;
  function modelPlantStandingLive_kg: single;
  function numHarvestItemTemplatesNotNil: smallint;
	function hasHarvestItemTemplateFor(itemType: smallint): boolean;
	function harvestItemTemplateFor(itemType: smallint): GsHarvestItemTemplate;
	function isBundled(itemType: smallint): boolean;
	function storageLumpedWith(itemType: smallint): boolean;
	function harvestIconForPartType(statistics: GsDrawingPlantStatistics;
			harvestItemType: smallint; var anIcon: TIcon): boolean;
  end;

GsTraverser = class(TObject)
  public
  plant: GsDrawingPlant;
  currentPhytomer: GsStreamableObject;
  point: TPoint;
  total: single;
  fractionOfPotentialBiomass: single;
  count: longint;
  mode: smallint;
  finished: boolean;
  filer: GsFiler;
  foundPlantPart: GsStreamableObject;
  foundList: TList;
  statistics: GsDrawingPlantStatistics;
  constructor createWithPlant(thePlant: GsDrawingPlant);
  procedure traversePlant(traversalCount: longint);
  procedure beginTraversal(aMode: integer);
  procedure traverseWholePlant(aMode: integer);
  end;

var
  cancelDrawing: boolean;
  worstNStressColor: TColorRef;
  worstPStressColor: TColorRef;
  worstDeadColor: TColorRef;

function blendColors(firstColor: TColorRef; secondColor: TColorRef; aStrength: single): TColorRef;
function darkerColor(aColor: TColorRef): TColorRef;
function lighterColor(aColor: TColorRef): TColorRef;
function linearGrowthWithFactorResult(current, optimal: single; minDays: integer; growthFactor: single): single;
procedure linearGrowthWithFactor(var current: single; optimal: single; minDays: integer; growthFactor: single);
function linearGrowthResult(current, optimal: single; minDays: integer): single;
procedure linearGrowth(var current: single; optimal: single; minDays: integer);
function pointsAreCloseEnough(point1: TPoint; point2: SinglePoint): boolean;

implementation

uses Forms, ueutils, uaspects, umconsts, ueplant, sysutils, utempman, udomain, uunits,
  udpinter, udpmeris, uclasses, usupport, ugsim;


function pointsAreCloseEnough(point1: TPoint; point2: SinglePoint): boolean;
  begin
  result := (abs(point1.x - point2.x) < Domain.menuOptions.plantProximityNeeded)
    and (abs(point1.y - point2.y) < Domain.menuOptions.plantProximityNeeded);
  end;

{ -------------------------------------------------------------------------- linear growth functions }
function linearGrowthWithFactorResult(current, optimal: single; minDays: integer; growthFactor: single): single;
  var
    amountNeeded, maxPossible: single;
  begin
  try
  amountNeeded := optimal - current;
  maxPossible := safedivExcept(optimal, minDays, optimal);
  amountNeeded := max(0.0, min(amountNeeded, maxPossible));
  result := amountNeeded * growthFactor;
  except result := errorMessage('Problem in linearGrowthWithFactorResult'); end;
  end;

procedure linearGrowthWithFactor(var current: single; optimal: single; minDays: integer; growthFactor: single);
  var
    amountNeeded, maxPossible: single;
  begin
  try
  amountNeeded := optimal - current;
  maxPossible := safedivExcept(optimal, minDays, optimal);
  amountNeeded := max(0.0, min(amountNeeded, maxPossible));
  current := current + amountNeeded * growthFactor;
  except errorMessage('Problem in linearGrowthWithFactor'); end;
  end;

function linearGrowthResult(current, optimal: single; minDays: integer): single;
  var
    amountNeeded, maxPossible: single;
  begin
  try
  amountNeeded := optimal - current;
  maxPossible := safedivExcept(optimal, minDays, optimal);
  amountNeeded := max(0.0, min(amountNeeded, maxPossible));
  result := amountNeeded;
  except result := errorMessage('Problem in linearGrowthResult'); end;
  end;

procedure linearGrowth(var current: single; optimal: single; minDays: integer);
  var
    amountNeeded, maxPossible: single;
  begin
  try
  amountNeeded := optimal - current;
  maxPossible := safedivExcept(optimal, minDays, optimal);
  amountNeeded := max(0.0, min(amountNeeded, maxPossible));
  current := current + amountNeeded;
  except errorMessage('Problem in linearGrowth'); end;
  end;

{ ---------------------------------------------------------------------------------- color functions }
function blendColors(firstColor: TColorRef; secondColor: TColorRef; aStrength: single): TColorRef;
  begin
  {blend first color with second color,
   weighting the second color by aStrength (0-1) and first color by (1 - aStrength).}
  if (aStrength < 0.0) or (aStrength > 1.0) then debugPrint('color blend strength outside of range');
  result := support_rgb(
    round((GetRValue(firstColor) * (1.0 - aStrength)) + (GetRValue(secondColor) * aStrength)),
    round((GetGValue(firstColor) * (1.0 - aStrength)) + (GetGValue(secondColor) * aStrength)),
    round((GetBValue(firstColor) * (1.0 - aStrength)) + (GetBValue(secondColor) * aStrength)));
  end;

function darkerColor(aColor: TColorRef): TColorRef;
  var r, g, b: word;
  begin
  r := intMax(0, GetRValue(aColor) - 15);
  g := intMax(0, GetGValue(aColor) - 15);
  b := intMax(0, GetBValue(aColor) - 15);
  result := support_rgb(r, g, b);
  {result := support_rgb(GetRValue(aColor) div 2, GetGValue(aColor) div 2, GetBValue(aColor) div 2); }
  end;

function lighterColor(aColor: TColorRef): TColorRef;
  var r, g, b: word;
  begin
  r := intMin(255, round(GetRValue(aColor) * 1.5));
  g := intMin(255, round(GetGValue(aColor) * 1.5));
  b := intMin(255, round(GetBValue(aColor) * 1.5));
  result := support_rgb(r, g, b);
  {result := support_rgb(GetRValue(aColor) div 2, GetGValue(aColor) div 2, GetBValue(aColor) div 2); }
  end;

{ ---------------------------------------------------------------------------------- traversing object }
constructor GsTraverser.createWithPlant(thePlant: GsDrawingPlant);
  begin
  self.create;
  plant := thePlant;
  end;

procedure GsTraverser.beginTraversal(aMode: integer);
  begin
  mode := aMode;
  currentPhytomer := plant.firstPhytomer;
  self.total := 0.0;
  self.count := 0;
  self.finished := false;
  foundPlantPart := nil;
  if currentPhytomer <> nil then
    begin
    GsInternode(currentPhytomer).traversingDirection := kTraverseLeft;
    GsInternode(currentPhytomer).traverseActivity(mode, self);
    {reset afterwards in case read in differently}
    GsInternode(currentPhytomer).traversingDirection := kTraverseLeft;
    end;
  end;

{ for kActivityAddMatureHarvestedPartsToList, the user of the traverser should set the
  foundList to a TList }
procedure GsTraverser.traverseWholePlant(aMode: integer);
  begin
  if (aMode = kActivityAddMatureHarvestedPartsToList) and (foundList = nil) then
    raise Exception.create('GsTraverser.traverseWholePlant: nil foundList');
  self.beginTraversal(aMode);
  self.traversePlant(0);
  end;

procedure GsTraverser.traversePlant(traversalCount: longint);
  var i: longint;
      nodesDrawn: longint;
      lastPhytomer: GsInternode;
      phytomerTraversing: GsInternode;
  begin
  if (currentPhytomer = nil) then exit;
  phytomerTraversing := GsInternode(currentPhytomer);
  i := 0;
  nodesDrawn := 0;
  while i <= traversalCount do
    begin
    if (mode = kActivityDraw) then
      i := i;
    if self.finished then exit;
    case phytomerTraversing.traversingDirection of
      kTraverseLeft:
        begin
        inc(phytomerTraversing.traversingDirection);
        if phytomerTraversing.leftBranchPlantPart <> nil then
          begin
          if (mode = kActivityDraw) then
            begin
    				plant.turtle.push;
    				plant.turtle.RotateZ(plant.pLeaf.petioleAngle * 2.0  / 3.0);
            end;
          phytomerTraversing.leftBranchPlantPart.traverseActivity(mode, self);
          if (phytomerTraversing.leftBranchPlantPart.isPhytomer) then
            begin
            phytomerTraversing := GsInternode(phytomerTraversing.leftBranchPlantPart);
            phytomerTraversing.traversingDirection := kTraverseLeft;
            end
          else
            if (mode = kActivityDraw) then plant.turtle.pop;
          end;
        end;
      kTraverseRight:
        begin
        inc(phytomerTraversing.traversingDirection);
        if phytomerTraversing.rightBranchPlantPart <> nil then
          begin
          if (mode = kActivityDraw) then
            begin
    				plant.turtle.push;
    				plant.turtle.RotateX(128);
    				plant.turtle.RotateZ(plant.pLeaf.petioleAngle * 2.0  / 3.0);
            end;
          phytomerTraversing.rightBranchPlantPart.traverseActivity(mode, self);
          if (phytomerTraversing.rightBranchPlantPart.isPhytomer) then
            begin
            phytomerTraversing := GsInternode(phytomerTraversing.rightBranchPlantPart);
            phytomerTraversing.traversingDirection := kTraverseLeft;
            end
          else
            if (mode = kActivityDraw) then plant.turtle.pop;
          end;
        end;
      kTraverseNext:
        begin
        inc(phytomerTraversing.traversingDirection);
        if phytomerTraversing.nextPlantPart <> nil then
          begin
          if (mode = kActivityDraw) then
            begin
            plant.turtle.push;
            plant.turtle.RotateX(98);  {phyllotactic rotation; 98 / 256 <> 137.5 / 360}
            end;
          phytomerTraversing.nextPlantPart.traverseActivity(mode, self);
          if (phytomerTraversing.nextPlantPart.isPhytomer) then
            begin
            phytomerTraversing := GsInternode(phytomerTraversing.nextPlantPart);
            phytomerTraversing.traversingDirection := kTraverseLeft;
            end
          else
            if (mode = kActivityDraw) then plant.turtle.pop;
          end;
        end;
      kTraverseDone:
        begin
        phytomerTraversing.traversingDirection := kTraverseNone;
        lastPhytomer := phytomerTraversing;
        phytomerTraversing := phytomerTraversing.phytomerAttachedTo;
        if (mode = kActivityFree) then
          begin
          lastPhytomer.free;
          if phytomerTraversing <> nil then
            begin
        	  if (phytomerTraversing.traversingDirection = kTraverseLeft + 1) then
              phytomerTraversing.leftBranchPlantPart := nil
            else if (phytomerTraversing.traversingDirection =  kTraverseRight + 1)  then
              phytomerTraversing.rightBranchPlantPart := nil
            else if (phytomerTraversing.traversingDirection =  kTraverseNext + 1)  then
              phytomerTraversing.nextPlantPart := nil;
            end;
          end;
        if phytomerTraversing = nil then break;
        {special drawing stuff - if returning from left or right branch draw, pop turtle}
        if (mode = kActivityDraw) then
          begin
         {	if (phytomerTraversing.traversingDirection = kTraverseLeft + 1)
           or (phytomerTraversing.traversingDirection =  kTraverseRight + 1)  then }
           plant.turtle.pop;
          end;
        end;
      kTraverseNone:
        raise Exception.create('GsTraverser.traversePlant: kTraverseNone encountered');
      end;
    if (traversalCount <> 0) then inc(i);
    (*if (mode = kActivityDraw) then
      begin
      inc(nodesDrawn);
      {check for messages every 256 nodes}
      if (nodesDrawn mod 256) = 0 then
        begin
        Application.ProcessMessages;
        {need to watch out for tool actions affecting current plant by deleting it - should
        prohibit tool actions while drawing}
        if cancelDrawing then exit;
        end;
      end;
    *)
    end;
  {if (mode = kActivityDraw) then debugPrint(intToSTr(plant.turtle.numMatrixesUsed)); }
  end;

{ ---------------------------------------------------------------------------------- statistics object }
procedure GsDrawingPlantStatistics.zeroAllFields;
  var i: smallint;
  begin
  for i := 0 to kStatisticsPartTypeLast do
    begin
    count[i] := 0;
    liveBiomass_kg[i] := 0.0;
    deadBiomass_kg[i] := 0.0;
    end;
  end;

function GsDrawingPlantStatistics.totalBiomass_kg: single;
  var i: smallint;
  begin
  result := 0.0;
  for i := 0 to kStatisticsPartTypeLast do
    begin
    result := result + liveBiomass_kg[i];
    result := result + deadBiomass_kg[i];
    end;
  end;

{ ---------------------------------------------------------------------------------- drawing plant }
constructor GsDrawingPlant.create;
  begin
  inherited create;
  RandomNumberGenerator := GsRandom.create;
  swayRandomNumberGenerator := GsRandom.create;
	{self.randomizeFromTime; - not needed here as always copy from cultivar and then randomize from time}
  firstPhytomer := nil;
  turtle := nil;
  end;

constructor GsDrawingPlant.createWithModelPlant(aPlant: GsModel);
  begin
  self.create;
  model := aPlant;
  self.initialize;
  end;

function GsDrawingPlant.hasHarvestItemTemplateFor(itemType: smallint): boolean;
  begin
  result := pGeneral.harvestItemTemplatesArray[itemType] <> nil;
  end;

function GsDrawingPlant.numHarvestItemTemplatesNotNil: smallint;
  var i: smallint;
  begin
  result := 0;
  for i := 0 to kHarvestPartTypeLast do
    if pGeneral.harvestItemTemplatesArray[i] <> nil then
      inc(result);
  end;

function GsDrawingPlant.harvestItemTemplateFor(itemType: smallint): GsHarvestItemTemplate;
  begin
  result := pGeneral.harvestItemTemplatesArray[itemType];
  end;

function GsDrawingPlant.isBundled(itemType: smallint): boolean;
  begin
  result := pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[itemType];
  end;

function GsDrawingPlant.storageLumpedWith(itemType: smallint): boolean;
  begin
  result := false;
  case itemType of
    kHarvestPartTypeRoot: result := pGeneral.storageOrganIsLumpedInWith = 0;
    kHarvestPartTypeStem: result := pGeneral.storageOrganIsLumpedInWith = 1;
    kHarvestPartTypeWholePlant: result := pGeneral.storageOrganIsLumpedInWith = 2;
    kHarvestPartTypeStorageOrgan: result := pGeneral.storageOrganIsLumpedInWith = 3;
    else
      raise Exception.create('GsDrawingPlant.storageLumpedWith: invalid value');
    end;
  end;

function GsDrawingPlant.maximumLineWidth: single;
  var i: integer;
  begin
  result := 0.0;
  if result < pLeaf.petioleWidthAtOptimalBiomass_mm  then
    result := pLeaf.petioleWidthAtOptimalBiomass_mm;
  if result < pInternode.widthAtOptimalFinalBiomassAndExpansion_mm  then
    result := pInternode.widthAtOptimalFinalBiomassAndExpansion_mm;
  for i := 0 to 1 do
    begin
  	if result < pInflor[i].internodeWidth_mm  then
    	result := pInflor[i].internodeWidth_mm;
    end;
  end;

procedure GsDrawingPlant.Initialize;
  begin
  { initialize parameters? or leave at zero? }
  { initialize state variables }
  AgeOfYoungestPhytomer := 0;
  floweringHasStarted := false;
  ageAtWhichFloweringStarted := 0;
  { these will be maintained as meristems or inflorescences are created or change state }
  {numApical (Axillary) ActiveReproductiveMeristemsOrInflorescences is:
  number of apical (axillary) meristems on plant that are either are working on producing an inflorescence
  or already created an inflorescence and vanished. These numbers are used only once
  for each apical (axillary) meristem as it decides whether it will produce an inflorescence. It reconciles
  the number of inflorescences produced with the number expected on the plant. }
  numApicalActiveReproductiveMeristemsOrInflorescences := 0;
  numAxillaryActiveReproductiveMeristemsOrInflorescences := 0;
  {numInactiveApical (Axillary) Meristems is:
  used by apical (axillary) meristems in reproductive mode to decide if they will produce
  an inflorescence. It is a measure of how many apical (axillary) meristems remain that are accessible for
  producing inflorescences. It reconciles the number of inflorescences produced with the number
  expected on the plant. }
  numApicalInactiveReproductiveMeristems := 0;
  numAxillaryInactiveReproductiveMeristems := 0;
  unallocatedNewVegetativeBiomass_kg := 0.0;
  unremovedDeadVegetativeBiomass_kg := 0.0;
  unallocatedNewReproductiveBiomass_kg := 0.0;
  unremovedDeadReproductiveBiomass_kg := 0.0;
  unremovedStandingDeadBiomass_kg := 0.0;
  {self.becomeGenericPlant;} { defaults params and creates 3D objects and harvest item templates }
  end;

destructor GsDrawingPlant.destroy;
	begin
  RandomNumberGenerator.free;
  RandomNumberGenerator := nil;
  swayRandomNumberGenerator.free;
  swayRandomNumberGenerator := nil;
  { do not free 3D objects becuase they are shared - template manager does it}
  { do not free harvest item templates because they are shared - template manager does it}
  { free plant parts }
  self.freeAllDrawingPlantParts;
  inherited destroy;
  end;

procedure GsDrawingPlant.freeAllDrawingPlantParts;
  var
    traverser: GsTraverser;
  begin
  if (firstPhytomer <> nil) then
    begin
    traverser := GsTraverser.createWithPlant(self);
    traverser.traverseWholePlant(kActivityFree);
    traverser.free;
    firstPhytomer := nil;
    end;
  end;

function GsDrawingPlant.mustBePulledToHarvest: boolean;
  var
    harvestingKillsPlant, hasOtherPart: boolean;
  begin
  result := false;
  with pGeneral do
    begin
    harvestingKillsPlant :=
      ((harvestItemTemplatesArray[kHarvestPartTypeRoot] <> nil) or
      (harvestItemTemplatesArray[kHarvestPartTypeStem] <> nil) or
      (harvestItemTemplatesArray[kHarvestPartTypeStorageOrgan] <> nil) or
      (harvestItemTemplatesArray[kHarvestPartTypeWholePlant] <> nil));
    if harvestingKillsPlant then
      begin
      hasOtherPart :=
      ((harvestItemTemplatesArray[kHarvestPartTypeSeedlingLeaf] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeLeaf] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeFemaleInflorescence] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeMaleInflorescence] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeFemaleFlower] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeFemaleFlowerBud] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeMaleFlower] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeMaleFlowerBud] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeAxillaryBud] <> nil) or
        (harvestItemTemplatesArray[kHarvestPartTypeFruit] <> nil));
      result := not hasOtherPart;
      end;
    end;
  end;

procedure GsDrawingPlant.randomizeFromTime;
  var
  	i: longint;
    sum: single;
  begin
  randomNumberGenerator.randomizeFromTime;
  {warm up the generator to diverge}
  for i := 0 to 100 do
    randomNumberGenerator.zeroToOne;
  {generate a related random number}
  try
    swayRandomNumberGenerator.initialize(randomNumberGenerator.seed + 1023);
  except
    swayRandomNumberGenerator.randomizeFromTime;
  end;
  {warm up the generator to diverge}
  for i := 0 to 100 do
    swayRandomNumberGenerator.zeroToOne;
  swaySeedToday := swayRandomNumberGenerator.seed;
  initialRotationAngle := RandomNumberGenerator.zeroToOne * 256.0;
  end;

procedure GsDrawingPlant.becomeSeed;
  begin
  self.freeAllDrawingPlantParts;
  ageOfYoungestPhytomer := 0;
  wiltingPercent := 0;
  numApicalActiveReproductiveMeristemsOrInflorescences := 0;
  numAxillaryActiveReproductiveMeristemsOrInflorescences := 0;
  numApicalInactiveReproductiveMeristems := 0;
  numAxillaryInactiveReproductiveMeristems := 0;
  floweringHasStarted := false;
  ageAtWhichFloweringStarted := 0;
  unallocatedNewVegetativeBiomass_kg := 0.0;
  unremovedDeadVegetativeBiomass_kg := 0.0;
  unallocatedNewReproductiveBiomass_kg := 0.0;
  unremovedDeadReproductiveBiomass_kg := 0.0;
  unremovedStandingDeadBiomass_kg := 0.0;
  end;

procedure GsDrawingPlant.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsDrawingPlant;
  cvir.versionNumber := 1;
  cvir.additionNumber := 1;
  end;

procedure GsDrawingPlant.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  self.streamDataWithoutParamsWithFiler(filer, cvir);
  self.streamParamsWithFiler(filer, cvir);
  if filer.isReading then self.needToRecalculateColors := true;
  end;

procedure GsDrawingPlant.streamDataWithoutParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    traverser: GsTraverser;
    hasFirstPhytomer: boolean;
  begin
  traverser := nil;
  randomNumberGenerator.streamUsingFiler(filer);
  swayRandomNumberGenerator.streamUsingFiler(filer);
  filer.streamLongint(swaySeedToday);
  filer.streamSingle(initialRotationAngle);
  filer.streamSmallint(ageOfYoungestPhytomer);
  filer.streamSmallint(wiltingPercent);
  filer.streamBoolean(floweringHasStarted);
  filer.streamSmallint(ageAtWhichFloweringStarted);
  filer.streamSingle(unallocatedNewVegetativeBiomass_kg);
  filer.streamSingle(unremovedDeadVegetativeBiomass_kg);
  filer.streamSingle(unallocatedNewReproductiveBiomass_kg);
  filer.streamSingle(unremovedDeadReproductiveBiomass_kg);
  if cvir.additionNumber >= 1 then
    filer.streamSingle(unremovedStandingDeadBiomass_kg);
  { drawing model objects }
  if filer.isReading then
    begin
    filer.streamBoolean(hasFirstPhytomer);
    {don't want to create if not written}
    if firstPhytomer <> nil then
      self.freeAllDrawingPlantParts;
    if hasFirstPhytomer then
      begin
      firstPhytomer := nil;
    	firstPhytomer := GsInternode.create;
      if firstPhytomer <> nil then
        begin
        GsInternode(firstPhytomer).plant := self;
        GsInternode(firstPhytomer).phytomerAttachedTo := nil;
        end
      else
        raise Exception.create('Could not create first phytomer for plant');
      end;
    end
  else if filer.isWriting then
    begin
    hasFirstPhytomer := self.firstPhytomer <> nil;
    filer.streamBoolean(hasFirstPhytomer);
    end;
  if hasFirstPhytomer then
    begin
    traverser := GsTraverser.createWithPlant(self);
    traverser.filer := filer;
  	traverser.traverseWholePlant(kActivityStream);
  	traverser.free;
    end;
  end;

procedure GsDrawingPlant.streamParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    i: smallint;
    templateManager: GsTemplateManager;
   begin
  { this should never be called outside of streamDataWithFiler because it does not call inherited }
  filer.streamBytes(pGeneral, sizeOf(pGeneral));
  filer.streamBytes(pMeristem, sizeOf(pMeristem));
  filer.streamBytes(pInternode, sizeOf(pInternode));
  filer.streamBytes(pLeaf, sizeOf(pLeaf));
  filer.streamBytes(pSeedlingLeaf, sizeOf(pSeedlingLeaf));
  filer.streamBytes(pAxillaryBud, sizeOf(pAxillaryBud));
  filer.streamBytes(pInflor, sizeOf(pInflor));
  filer.streamBytes(pFlower, sizeOf(pFlower));
  filer.streamBytes(pFruit, sizeOf(pFruit));
  filer.streamBytes(pRoot, sizeOf(pRoot));
  templateManager := nil;
  templateManager := filer.resourceProvider as GsTemplateManager;
  {copyTo doesn't set the resource system}
  {if it is used on objects in another template manager, the results for these field
  will be wrong - the template manager dialog adjusts for this after the copy}
  if templateManager = nil then
    templateManager := Domain.templateManager;
  if templateManager <> nil then
    begin
  	{ tell 3D objects to stream themselves as references}
  	templateManager.streamReferenceToObject3d(filer, pSeedlingLeaf.object3D);
 		templateManager.streamReferenceToObject3d(filer, pLeaf.object3D);
  	templateManager.streamReferenceToObject3d(filer, pFlower[kGenderFemale].object3D);
  	templateManager.streamReferenceToObject3d(filer, pFlower[kGenderMale].object3D);
  	templateManager.streamReferenceToObject3d(filer, pFlower[kGenderFemale].budObject3D);
  	templateManager.streamReferenceToObject3d(filer, pFlower[kGenderMale].budObject3D);
  	templateManager.streamReferenceToObject3d(filer, pAxillaryBud.object3D);
  	templateManager.streamReferenceToObject3d(filer, pFruit.object3D);
  	templateManager.streamReferenceToObject3d(filer, pRoot.object3D);
  	{ tell harvest item templates to stream themselves as references}
    for i := 0 to kHarvestPartTypeLast do
  	  templateManager.streamReferenceToHarvestItemTemplate(filer, pGeneral.harvestItemTemplatesArray[i]);
    end
  else
    begin
  	pSeedlingLeaf.object3D := nil;
 		pLeaf.object3D := nil;
  	pFlower[kGenderFemale].object3D := nil;
  	pFlower[kGenderMale].object3D := nil;
  	pFlower[kGenderFemale].budObject3D := nil;
 	 	pFlower[kGenderMale].budObject3D := nil;
  	pAxillaryBud.object3D := nil;
  	pFruit.object3D := nil;
  	pRoot.object3D := nil;
    for i := 0 to kHarvestPartTypeLast do pGeneral.harvestItemTemplatesArray[i] := nil;
    end;
  end;

procedure GsDrawingPlant.drawWithTurtle(aTurtle: KfTurtle; rotateAngle, topOrSideAngle: single);
  var
    traverser: GsTraverser;
  begin
  {reset sway generator so get same swayed plant for today}
  swayRandomNumberGenerator.initialize(swaySeedToday);
  cancelDrawing := false;
  turtle := aTurtle;
  if (turtle = nil) then
    raise exception.create('Turtle is nil');
  turtle.drawingSurface.pane.brush.style := bsSolid;
  turtle.setLineColor(clGreen);
  turtle.push;
  turtle.RotateZ(64);
  turtle.RotateY(topOrSideAngle);
  turtle.RotateX(initialRotationAngle + rotateAngle);
  turtle.drawingSurface.lineColor := clGreen;
  {turtle.drawInMillimeters(100 / turtle.scale);}
  if (firstPhytomer <> nil) then
    begin
    traverser := GsTraverser.createWithPlant(self);
    traverser.traverseWholePlant(kActivityDraw);
    traverser.free;
    traverser := nil;
    end;
  turtle.pop;
  turtle := nil;
  { recalculating colors occurs during drawing of leaves and internodes if necessary }
  self.needToRecalculateColors := false;
  end;

procedure GsDrawingPlant.nextDay;
  var traverser: GsTraverser;
  begin
  try
  traverser := GsTraverser.createWithPlant(self);
  if (firstPhytomer = nil) and (GsPlant(model).development.lifeHistoryStage <> kSeed) then
    begin
    {If first day, create first phytomer. Assume seed reserves are placed into this phytomer.}
    FirstPhytomer := (GsMeristem.NewWithPlant(self)).createFirstPhytomer;
    {Must tell firstPhytomer to recalculate internode angle because it did not know it was the
    first phytomer until after drawing plant got this pointer back.}
    if firstPhytomer = nil then exit;
    GsInternode(firstPhytomer).setAsFirstPhytomer;
    end;
  {If day to start reproductive mode, tell all meristems to switch over. They decide if they
  will produce inflorescences, and if so, what gender.}
  if (not floweringHasStarted)
    and (GsPlant(model).development.lifeHistoryStage = kReproductiveAllocationPeriod)
    and (firstPhytomer <> nil)  then
    begin
    traverser.traverseWholePlant(kActivityStartReproduction);
    floweringHasStarted := true;
    ageAtWhichFloweringStarted := GsPlant(model).development.age;
    end;
  {Figure wilting angle based on water stress}
  if not GsPlant(model).development.isDead then
    self.wiltingPercent := round((1.0 - GsPlant(model).constraints.waterStressFactor_frn) * 100.0);
  self.allocateOrRemoveBiomassWithTraverser(traverser);
  self.plantWideDrawingVarsWithTraverser(traverser);
  traverser.traverseWholePlant(kActivityNextDay);
  traverser.free;
  traverser := nil;
  {if have drawn at all today - use the last computed seed next time draw}
  swaySeedToday := swayRandomNumberGenerator.seed;
  self.needToRecalculateColors := true;
  except errorMessage('Problem in drawing plant NextDay'); end;
  end;

procedure GsDrawingPlant.reduceStandingDeadFromTransferToResidue;
  var
    traverser: GsTraverser;
    reductionInStandingDeadIntoPatch_t, reductionInStandingDeadIntoPatch_kg: single;
    plant: GsPlant;
  begin
  if firstPhytomer = nil then exit;
  plant := model as GsPlant;
  traverser := GsTraverser.createWithPlant(self);
  { get standing dead moved into soil patch }
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionInStandingDeadFromTransferToResidue_tPha,
      reductionInStandingDeadIntoPatch_t);
  reductionInStandingDeadIntoPatch_kg := reductionInStandingDeadIntoPatch_t * t_to_kg;
  reductionInStandingDeadIntoPatch_kg := reductionInStandingDeadIntoPatch_kg + unremovedStandingDeadBiomass_kg;
  unremovedStandingDeadBiomass_kg := 0.0;
  { removed standing dead }
  if reductionInStandingDeadIntoPatch_kg > 0.0 then
    self.allocateOrRemoveParticularBiomass(reductionInStandingDeadIntoPatch_kg, unremovedStandingDeadBiomass_kg,
    kActivityStandingDeadBiomassThatCanBeRemoved_kg, kActivityRemoveStandingDeadBiomass, traverser);
  self.needToRecalculateColors := true;
  traverser.free;
  traverser := nil;  
  end;

procedure cancelOutOppositeAmounts(var amountAdded_kg: single; var amountTakenAway_kg: single);
  begin
  if (amountAdded_kg > 0.0) and (amountTakenAway_kg > 0.0) then
    begin
    if amountAdded_kg = amountTakenAway_kg then
      begin
      amountAdded_kg := 0.0;
      amountTakenAway_kg := 0.0;
      end
    else if amountAdded_kg > amountTakenAway_kg then
      begin
      amountAdded_kg := amountAdded_kg - amountTakenAway_kg;
      amountTakenAway_kg := 0.0;
      end
    else
      begin
      amountTakenAway_kg := amountTakenAway_kg - amountAdded_kg;
      amountAdded_kg := 0.0;
      end;
    end;
  end;

procedure GsDrawingPlant.allocateOrRemoveBiomassWithTraverser(traverser: GsTraverser);
  var
    shootAllocation_t, shootAllocation_kg, reproAllocation_t, reproAllocation_kg,
      reductionFromFrostAndDaylength_t, reductionFromFallingLeaves_t, reductionFromDryness_t,
      reductionFromAboveGroundVegetativeDeath_t, reductionInShootBiomass_kg,
      reductionInReproBiomassFromDecay_t, reductionInReproBiomassFromDeath_t, reductionInReproBiomass_kg,
      reductionFromMobilizationOutOfShoots_t: single;
    plant: GsPlant;
  begin
  if firstPhytomer = nil then exit;
  plant := model as GsPlant;
  { get new vegetative and reproductive allocations from plant }
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.shootAllocation_tPha, shootAllocation_t);
  shootAllocation_kg := shootAllocation_t * t_to_kg;
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reproductiveAllocation_tPha, reproAllocation_t);
  reproAllocation_kg := reproAllocation_t * t_to_kg;
  { get vegetative and reproductive biomass reductions from plant }
  { vegetative }
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionFromFrostAndDaylength_tPha,
      reductionFromFrostAndDaylength_t);
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionFromDryness_tPha, reductionFromDryness_t);
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionFromFallingLeaves_tPha, reductionFromFallingLeaves_t);
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionFromAboveGroundVegetativeDeath_tPha,
      reductionFromAboveGroundVegetativeDeath_t);
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.mobilizationOutOfShoots_tPha,
      reductionFromMobilizationOutOfShoots_t);
  reductionInShootBiomass_kg := (reductionFromFrostAndDaylength_t + reductionFromDryness_t
      + reductionFromFallingLeaves_t + reductionFromAboveGroundVegetativeDeath_t
      + reductionFromMobilizationOutOfShoots_t) * t_to_kg;
  { reproductive }
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionFromReproductiveDecay_tPha,
      reductionInReproBiomassFromDecay_t);
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.reductionFromReproductiveDeath_tPha,
      reductionInReproBiomassFromDeath_t);
  reductionInReproBiomass_kg := (reductionInReproBiomassFromDecay_t + reductionInReproBiomassFromDeath_t) * t_to_kg;
  { add in amounts rolled over from yesterday }
  shootAllocation_kg := shootAllocation_kg + unallocatedNewVegetativeBiomass_kg;
  unallocatedNewVegetativeBiomass_kg := 0.0;
  reproAllocation_kg := reproAllocation_kg + unallocatedNewReproductiveBiomass_kg;
  unallocatedNewReproductiveBiomass_kg := 0.0;
  reductionInShootBiomass_kg := reductionInShootBiomass_kg + unremovedDeadVegetativeBiomass_kg;
  unremovedDeadVegetativeBiomass_kg := 0.0;
  reductionInReproBiomass_kg := reductionInReproBiomass_kg + unremovedDeadReproductiveBiomass_kg;
  unremovedDeadReproductiveBiomass_kg := 0.0;
  { see if amounts can cancel out to any extent }
  cancelOutOppositeAmounts(shootAllocation_kg, reductionInShootBiomass_kg);
  cancelOutOppositeAmounts(reproAllocation_kg, reductionInReproBiomass_kg);
  { allocate new shoot biomass }
  if shootAllocation_kg > 0.0 then
    self.allocateOrRemoveParticularBiomass(shootAllocation_kg, unallocatedNewVegetativeBiomass_kg,
    kActivityDemandVegetative_kg, kActivityGrowVegetative, traverser);
  { remove dead shoot biomass }
  if reductionInShootBiomass_kg > 0.0 then
    self.allocateOrRemoveParticularBiomass(reductionInShootBiomass_kg, unremovedDeadVegetativeBiomass_kg,
    kActivityVegetativeBiomassThatCanBeRemoved_kg, kActivityRemoveVegetativeBiomass, traverser);
  { allocate new fruit biomass }
  if reproAllocation_kg > 0.0 then
    self.allocateOrRemoveParticularBiomass(reproAllocation_kg, unallocatedNewReproductiveBiomass_kg,
    kActivityDemandReproductive_kg, kActivityGrowReproductive, traverser);
  { remove dead fruit biomass }
  if reductionInReproBiomass_kg > 0.0 then
    self.allocateOrRemoveParticularBiomass(reductionInReproBiomass_kg, unremovedDeadReproductiveBiomass_kg,
    kActivityReproductiveBiomassThatCanBeRemoved_kg, kActivityRemoveReproductiveBiomass, traverser);
  end;

procedure GsDrawingPlant.allocateOrRemoveParticularBiomass(biomassToAddOrRemove_kg: single;
  var undistributedBiomass_kg: single; askingMode, tellingMode: integer; traverser: GsTraverser);
  var
    totalDemandOrAvailableBiomass_kg: single;
  begin
  try
  traverser.traverseWholePlant(askingMode);
  totalDemandOrAvailableBiomass_kg := traverser.total;
  if totalDemandOrAvailableBiomass_kg > 0.0 then
    begin
    if biomassToAddOrRemove_kg > totalDemandOrAvailableBiomass_kg then
      begin
      undistributedBiomass_kg := biomassToAddOrRemove_kg - totalDemandOrAvailableBiomass_kg;
      biomassToAddOrRemove_kg := totalDemandOrAvailableBiomass_kg;
      traverser.fractionOfPotentialBiomass := 1.0;
      end
    else
      traverser.fractionOfPotentialBiomass := safediv(biomassToAddOrRemove_kg, totalDemandOrAvailableBiomass_kg);
    traverser.traverseWholePlant(tellingMode);
    end
  else
    begin  { no demand }
    undistributedBiomass_kg := undistributedBiomass_kg + biomassToAddOrRemove_kg;
    end;
  except
    errorMessage('Problem in GsDrawingPlant.allocateOrRemoveParticularBiomass');
  end;
  end;

function GsDrawingPlant.modelPlantStandingLive_kg: single;
  var
    plant: GsPlant;
    standingLive_t, newShootBiomass_t: single;
  begin
  plant := model as GsPlant;
  { get new vegetative and reproductive allocations from plant }
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.standingLive_tPha, standingLive_t);
  plant.ConvertRelativeAndAbsolute(kGetField, plant.biomass.shootAllocation_tPha, newShootBiomass_t);
  result := (standingLive_t - newShootBiomass_t) * t_to_kg;
  end;

procedure GsDrawingPlant.plantWideDrawingVarsWithTraverser(traverser: GsTraverser);
  begin
  if (firstPhytomer <> nil) then
    begin
    traverser.count := maxLongInt;
    traverser.traverseWholePlant(kActivityFindAgeOfYoungestPhytomer);
    ageOfYoungestPhytomer := traverser.count;
    end
  else
    ageOfYoungestPhytomer := 0;
  end;

procedure GsDrawingPlant.plantWideDrawingVars;
  var traverser: GsTraverser;
  begin
  traverser := GsTraverser.createWithPlant(self);
  self.plantWideDrawingVarsWithTraverser(traverser);
  traverser.free;
  end;

function GsDrawingPlant.findPlantPartAtPosition(point: TPoint): GsStreamableObject;
  var traverser: GsTraverser;
  begin
  traverser := GsTraverser.createWithPlant(self);
  traverser.point := point;
  traverser.traverseWholePlant(kActivityFindPlantPartAtPosition);
  result := traverser.foundPlantPart;
  traverser.free;
  end;

procedure GsDrawingPlant.addAllMaturePlantPartsToList(aList: TList);
  var
    traverser: GsTraverser;
  begin
  traverser := GsTraverser.createWithPlant(self);
  traverser.foundList := aList;
  traverser.traverseWholePlant(kActivityAddMatureHarvestedPartsToList);
  traverser.free;
  end;

procedure GsDrawingPlant.addAllBundledPlantPartsToList(aList: TList);
  var
    traverser: GsTraverser;
  begin
  traverser := GsTraverser.createWithPlant(self);
  traverser.foundList := aList;
  traverser.traverseWholePlant(kActivityAddBundledPartsToList);
  traverser.free;
  end;

procedure GsDrawingPlant.getDrawingPlantStatistics(statistics: GsDrawingPlantStatistics);
  var
    traverser: GsTraverser;
  begin
  traverser := GsTraverser.createWithPlant(self);
  traverser.statistics := statistics;
  traverser.traverseWholePlant(kActivityGatherStatistics);
  traverser.free;
  { set undistributed amounts in statistics }
  statistics.liveBiomass_kg[kStatisticsPartTypeUnallocatedNewVegetativeBiomass_kg] :=
      self.unallocatedNewVegetativeBiomass_kg;
  statistics.deadBiomass_kg[kStatisticsPartTypeUnremovedDeadVegetativeBiomass_kg] :=
      self.unremovedDeadVegetativeBiomass_kg;
  statistics.liveBiomass_kg[kStatisticsPartTypeUnallocatedNewReproductiveBiomass_kg] :=
      self.unallocatedNewReproductiveBiomass_kg;
  statistics.deadBiomass_kg[kStatisticsPartTypeUnremovedDeadReproductiveBiomass_kg] :=
      self.unremovedDeadReproductiveBiomass_kg;
  end;

procedure GsDrawingPlant.report;
  var traverser: GsTraverser;
  begin
  if (firstPhytomer <> nil) then
    begin
    DebugPrint('---------------------- Start plant report');
    traverser := GsTraverser.createWithPlant(self);
    traverser.traverseWholePlant(kActivityReport);
    traverser.free;
    DebugPrint('---------------------- End plant report');
    end;
  end;

procedure GsDrawingPlant.transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  self.directTransferField(d, v, fieldID, ft, index, deriveMethod, updateList);
  if (d = kSetField) and (ft = kFieldColor) then
    self.needToRecalculateColors := true;
  end;

procedure GsDrawingPlant.directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
  begin
  DrawingPlant_directTransferField(self, v, d, fieldID, ft, index, deriveMethod, updateList);
  { tell MODEL plant to add the field to the update list - not self - browser has model plant }
  model.addToUpdateList(fieldID, index, updateList);
  end;

{harvest item templates are shared with template manager who will free them}
procedure GsDrawingPlant.transferHarvestItemTemplate(direction: integer;
    var myHarvestItemTemplate: GsHarvestItemTemplate; var otherHarvestItemTemplate: GsHarvestItemTemplate);
  begin
  if direction = kSetField then
    myHarvestItemTemplate := otherHarvestItemTemplate
  else
    otherHarvestItemTemplate := myHarvestItemTemplate;
  end;

{tdo's are shared with template manager who will free them}
procedure GsDrawingPlant.transferObject3D(direction: integer; var myTdo: KfObject3D; var otherTdo: KfObject3D);
  begin
  if direction = kSetField then
    myTdo := otherTdo
  else
    otherTdo := myTdo;
  end;

procedure GsDrawingPlant.transferStorageOrganLumping(direction: smallint; var value: smallint);
  begin
  if direction = kSetField then
    begin
    case value of
      0: pGeneral.storageOrganIsLumpedInWith := kHarvestPartTypeRoot;
      1: pGeneral.storageOrganIsLumpedInWith := kHarvestPartTypeStem;
      2: pGeneral.storageOrganIsLumpedInWith := kHarvestPartTypeWholePlant;
      3: pGeneral.storageOrganIsLumpedInWith := kHarvestPartTypeStorageOrgan;
      else
        raise Exception.create('GsDrawingPlant.transferStorageOrganLumping: invalid value');
      end;
    end
  else
    begin
    case pGeneral.storageOrganIsLumpedInWith of
      kHarvestPartTypeRoot: value := 0;
      kHarvestPartTypeStem: value := 1;
      kHarvestPartTypeWholePlant: value := 2;
      kHarvestPartTypeStorageOrgan: value := 3;
      else
        raise Exception.create('GsDrawingPlant.transferStorageOrganLumping: invalid value');
      end;
    end;
  end;

procedure GsDrawingPlant.transferHarvestBundling(direction: integer; var value: boolean; fieldIndex: integer);
  var
    realIndex: integer;
  begin
  case fieldIndex of
    0:  realIndex := kHarvestPartTypeSeedlingLeaf;
    1:  realIndex := kHarvestPartTypeLeaf;
    2:  realIndex := kHarvestPartTypeFemaleInflorescence;
    3:  realIndex := kHarvestPartTypeMaleInflorescence;
    4:  realIndex := kHarvestPartTypeFemaleFlower;
    5:  realIndex := kHarvestPartTypeFemaleFlowerBud;
    6:  realIndex := kHarvestPartTypeMaleFlower;
    7:  realIndex := kHarvestPartTypeMaleFlowerBud;
    8:  realIndex := kHarvestPartTypeAxillaryBud;
    9:  realIndex := kHarvestPartTypeFruit;
    10: realIndex := kHarvestPartTypeRoot;
    11: realIndex := kHarvestPartTypeStem;
  else
    raise Exception.create('GsDrawingPlant.transferHarvestBundling: invalid index');
  end;
  if direction = kSetField then
    pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[realIndex] := value
  else
    value := pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[realIndex];
  end;

function GsDrawingPlant.firstAvailableIcon: TIcon;
  var i: smallint;
  begin
  result := nil;
  for i := 0 to kHarvestPartTypeLast do
    if pGeneral.harvestItemTemplatesArray[i] <> nil then
    begin
    result := pGeneral.harvestItemTemplatesArray[i].icon.icon;
    exit;
    end;
  end;

function GsDrawingPlant.iconForHarvesting(harvestWholePlant: boolean): TIcon;
  var
    statistics: GsDrawingPlantStatistics;
    pullPlant: boolean;
    plant: GsPlant;
  begin
  result := nil;
  plant := GsPlant(model);
  if plant.development.lifeHistoryStage = kSeed then exit;
  statistics := nil;
  pullPlant := harvestWholePlant or self.mustBePulledToHarvest;
  {do whole, root, stem, storage organ first}
  if pullPlant then
    begin
    if self.hasHarvestItemTemplateFor(kHarvestPartTypeWholePlant) and (plant.biomass.totalLive_tPha > 0.0) then
      result := self.harvestItemTemplateFor(kHarvestPartTypeWholePlant).icon.icon
    else if self.hasHarvestItemTemplateFor(kHarvestPartTypeRoot) and (plant.biomass.totalRootWeight_tPha > 0.0) then
      result := self.harvestItemTemplateFor(kHarvestPartTypeRoot).icon.icon
    else if self.hasHarvestItemTemplateFor(kHarvestPartTypeStem)and (plant.biomass.totalShootWeight_tPha > 0.0) then
      result := self.harvestItemTemplateFor(kHarvestPartTypeStem).icon.icon
    else if self.hasHarvestItemTemplateFor(kHarvestPartTypeStorageOrgan) and (plant.biomass.storageOrgan_tPha > 0.0) then
      result := harvestItemTemplateFor(kHarvestPartTypeStorageOrgan).icon.icon;
    end;
  {otherwise look for rest}
  if result = nil then
    begin
  	statistics := GsDrawingPlantStatistics.create;
  	self.getDrawingPlantStatistics(statistics);
    if not harvestIconForPartType(statistics, kHarvestPartTypeFruit, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeLeaf, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeFemaleFlower, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeMaleFlower, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeFemaleFlowerBud, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeMaleFlowerBud, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeFemaleInflorescence, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeMaleInflorescence, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeAxillaryBud, result) then
    if not harvestIconForPartType(statistics, kHarvestPartTypeSeedlingLeaf, result) then
      result := nil;
    end;
  if (result = nil) and harvestWholePlant then
    result := GardenForm.harvestIconDefault.picture.icon;
  statistics.free;
  statistics := nil;
  end;

function GsDrawingPlant.harvestIconForPartType(statistics: GsDrawingPlantStatistics;
		harvestItemType: smallint; var anIcon: TIcon): boolean;
  begin
	result := self.hasHarvestItemTemplateFor(harvestItemType) and
  	(statistics.liveBiomass_kg[harvestItemType] > 0.0);
  if result then
      anIcon := self.harvestItemTemplateFor(harvestItemType).icon.icon;
  end;

procedure GsDrawingPlant.setTdoInUseFlags;
  begin
  if pSeedlingLeaf.object3D <> nil then pSeedlingLeaf.object3D.inUse := true;
  if pLeaf.object3D <> nil then pLeaf.object3D.inUse := true;
  if pFlower[kGenderFemale].object3D <> nil then pFlower[kGenderFemale].object3D.inUse := true;
  if pFlower[kGenderMale].object3D <> nil then pFlower[kGenderMale].object3D.inUse := true;
  if pFlower[kGenderFemale].budObject3D <> nil then pFlower[kGenderFemale].budObject3D.inUse := true;
  if pFlower[kGenderMale].budObject3D <> nil then pFlower[kGenderMale].budObject3D.inUse := true;
  if pAxillaryBud.object3d <> nil then pAxillaryBud.object3d.inUse := true;
  if pFruit.object3d <> nil then pFruit.object3d.inUse := true;
  if pRoot.object3d <> nil then pRoot.object3d.inUse := true;
  end;

procedure GsDrawingPlant.becomeGenericPlant;
  begin
  with pGeneral do
    begin
    randomSway := 16.0;
    numApicalInflors := 0;
    numAxillaryInflors := 5;
    gender := kGenderHermaphroditic;
    lineDivisions := 3;
    isDicot := true;
    maleFlowersAreSeparate := false;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\sl_1.tdo');
    faceColor := support_rgb(0, 150, 0);
    backfaceColor := support_rgb(0, 220, 0);
    scale := 50; nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_sunfl.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalBiomass_kg := 0.000001;
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 50;
    petioleLengthAtOptimalBiomass_mm := 30;
    petioleWidthAtOptimalBiomass_mm := 2;
    petioleAngle := 24;
    petioleColor := faceColor;
    compoundPetioleToRachisRatio := 1.0;
    compoundPinnateOrPalmate := kCompoundLeafPinnate;
    compoundNumLeaflets := 1;
    fractionOfLiveBiomassWhenAbscisses_frn := 0.05;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20); backfaceColor := support_rgb(0, 255, 20);
    meanderIndex := 10;
    flexibilityIndex := 30;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    optimalFinalBiomass_kg := 0.1;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 100;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 2;
    widthMultiplierDueToBiomassAccretion := 3;
    widthMultiplierDueToExpansion := 10;
    lengthMultiplierDueToBolting := 1;
    minDaysToBolt := 1;
    maxDaysToBolt := 1;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(200, 200, 0);
    backfaceColor := support_rgb(255, 255, 0);
    scaleAtFullSize := 10;
    numPetals := 5;
    petalsAreRadiallyArranged := true;
    optimalBiomass_kg := 0.000001;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minFractionOfOptimalBiomassToCreateFruit_frn := 0.5;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 100;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 10;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
  end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(200, 200, 0);
    backfaceColor := support_rgb(255, 255, 0);
    scaleAtFullSize := 0;
    numPetals := 0;
    petalsAreRadiallyArranged := false;
    optimalBiomass_kg := 0.000001;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 100;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 10;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 0);
    optimalBiomass_kg := 0.0000005;
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 15;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 20;
    peduncleLength_mm := 5;
    internodeLength_mm := 5;
    internodeWidth_mm := 1;
    pedicelLength_mm := 5;
    pedicelAngle := 32;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 4;
    daysToAllFlowersCreated := 30;
    numBranches := 0;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    stalkColor := support_rgb(0, 128, 0);
    optimalBiomass_kg := 0.002;
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 15;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 20;
    peduncleLength_mm := 5;
    internodeLength_mm := 5;
    internodeWidth_mm := 1;
    pedicelLength_mm := 5;
    pedicelAngle := 32;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 4;
    daysToAllFlowersCreated := 30;
    numBranches := 0;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    faceColor := support_rgb(0, 200, 0);
    backfaceColor := support_rgb(0, 150, 0);
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 10;
    branchingDistance := 3;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    unripeFaceColor := support_rgb(0, 150, 0);
    unripeBackfaceColor := support_rgb(0, 100, 0);
    ripeFaceColor := support_rgb(150, 0, 0);
    ripeBackfaceColor := support_rgb(100, 0, 0);
    scaleAtFullSize := 100;
    optimalBiomass_kg := 0.001;
    stalkStrengthIndex := 100;
 	  daysToRipen := 20;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    { x1 = 1 day, x2 = 5% of total biomass, x2 = minDaysToGrow, y2 = 95% of total biomass}
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    faceColor := support_rgb(150, 150, 0);
    backfaceColor := support_rgb(150, 150, 0);
    scaleAtFullSize := 100;
    showsAboveGround := false;
    end;
  end;

function genderString(index: smallint): string;
  begin
  result := '';
  case index of
    kGenderMale: result := 'male';
    kGenderFemale: result := 'female';
    end;
  end;

procedure GsDrawingPlant.streamUsingTextFiler(textFiler: GsTextFiler);
  var
    template: GsHarvestItemTemplate;
    i, j, numTemplates: smallint;
    templateName, writeName, ws, newString, tdoName: string;
    provider: GsTemplateManager;
  begin
  { this is for streaming as a cultivar, so only need to stream parameters }
  { and the cultivar CANNOT be a seedling }
  provider := nil;
  with textFiler do
    begin
    if isReading then
      begin
      provider := GsTemplateManager(textFiler.resourceProvider);
      if provider = nil then provider := Domain.templateManager;
      if provider = nil then exit;
      end;
    {pGeneral}
    for i := 0 to kHarvestPartTypeLast do
      begin
      if isWriting then
        begin
        template := self.harvestItemTemplateFor(i);
        if template <> nil then templateName := template.getName else templateName := 'none';
        if i = 0 then
          writeName := 'harvest item: ' + Domain.harvestManager.harvestItemTypeDescription(i)
        else
          writeName := Domain.harvestManager.harvestItemTypeDescription(i);
        end;
      streamStringWithNonTabDelimiter(templateName, writeName);
      if isReading and (provider <> nil) then
        pGeneral.harvestItemTemplatesArray[i] := provider.findHarvestItemTemplate(templateName);
      end;
    streamTab;
    for i := 0 to kHarvestPartTypeLast do
      begin
      if i = 0 then
        writeName := 'bundled: ' + Domain.harvestManager.harvestItemTypeDescription(i)
      else
        writeName := Domain.harvestManager.harvestItemTypeDescription(i);
      streamBooleanWithNonTabDelimiter(pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[i], writeName);
      end;
    streamTab;
    streamSmallintWithLabel(pGeneral.storageOrganIsLumpedInWith,
        Domain.harvestManager.harvestItemTypeDescription(pGeneral.storageOrganIsLumpedInWith),
        'storageOrganIsLumpedInWith');
    streamSingle(pGeneral.randomSway, 'randomSway');
    streamSmallint(pGeneral.numApicalInflors, 'numApicalInflors');
    streamSmallint(pGeneral.numAxillaryInflors, 'numAxillaryInflors');
    streamSmallint(pGeneral.gender, 'gender');
    streamSmallint(pGeneral.lineDivisions, 'lineDivisions');
    streamBoolean(pGeneral.isDicot, 'isDicot');
    streamBoolean(pGeneral.maleFlowersAreSeparate, 'maleFlowersAreSeparate');
    {pSeedlingLeaf}
    if isWriting then
      if pSeedlingLeaf.object3D <> nil then tdoName := pSeedlingLeaf.object3D.getName else tdoName := 'none';
    streamString(tdoName, 'seedlingLeaf_tdo');
    if isReading and (provider <> nil) then
      pSeedlingLeaf.object3D := provider.findMatchForObject3dByName(tdoName);
    streamColorRef(pSeedlingLeaf.faceColor, 'seedlingLeaf_faceColor');
    streamColorRef(pSeedlingLeaf.backfaceColor, 'seedlingLeaf_backfaceColor');
    streamSingle(pSeedlingLeaf.scale, 'seedlingLeaf_scale');
    streamSmallint(pSeedlingLeaf.nodesOnStemWhenFallsOff, 'seedlingLeaf_nodesOnStemWhenFallsOff');
    {pLeaf}
    if isWriting then if pLeaf.object3D <> nil then tdoName := pLeaf.object3D.getName else tdoName := 'none';
    streamString(tdoName, 'leaf_tdo');
    if isReading and (provider <> nil) then
      pLeaf.object3D := provider.findMatchForObject3dByName(tdoName);
    streamColorRef(pLeaf.faceColor, 'leaf_faceColor');
    streamColorRef(pLeaf.backfaceColor, 'leaf_backfaceColor');
    streamColorRef(pLeaf.petioleColor, 'leaf_petioleColor');
    streamSingle(pLeaf.optimalBiomass_kg, 'leaf_optimalBiomass_kg');
    streamSingle(pLeaf.optimalFractionOfOptimalBiomassAtCreation_kg, 'leaf_optimalFractionOfOptimalBiomassAtCreation_kg');
    streamSmallint(pLeaf.minDaysToGrow, 'leaf_minDaysToGrow');
    streamSmallint(pLeaf.maxDaysToGrow, 'leaf_maxDaysToGrow');
    streamSingle(pLeaf.scaleAtOptimalBiomass, 'leaf_scaleAtOptimalBiomass');
    streamSingle(pLeaf.petioleLengthAtOptimalBiomass_mm, 'leaf_petioleLengthAtOptimalBiomass_mm');
    streamSingle(pLeaf.petioleWidthAtOptimalBiomass_mm, 'leaf_petioleWidthAtOptimalBiomass_mm');
    streamSingle(pLeaf.petioleAngle, 'leaf_petioleAngle');
    streamSingle(pLeaf.compoundPetioleToRachisRatio, 'leaf_compoundPetioleToRachisRatio');
    streamSmallint(pLeaf.compoundPinnateOrPalmate, 'leaf_compoundPinnateOrPalmate');
    streamSmallint(pLeaf.compoundNumLeaflets, 'leaf_compoundNumLeaflets');
    pLeaf.fractionOfLiveBiomassWhenAbscisses_frn := 0.0; {cfk fix later?}
    {pInternode}
    streamColorRef(pInternode.faceColor, 'internode_faceColor');
    streamColorRef(pInternode.backfaceColor, 'internode_backfaceColor');
    streamSingle(pInternode.meanderIndex, 'internode_meanderIndex');
    streamSingle(pInternode.flexibilityIndex, 'internode_flexibilityIndex');
    streamSingle(pInternode.minFractionOfOptimalInitialBiomassToCreateInternode_frn,
        'internode_minFractionOfOptimalInitialBiomassToCreateInternode_frn');
    streamSmallint(pInternode.minDaysToCreateInternode, 'internode_minDaysToCreateInternode');
    streamSmallint(pInternode.maxDaysToCreateInternodeIfOverMinFraction,
        'internode_maxDaysToCreateInternodeIfOverMinFraction');
    streamSingle(pInternode.optimalFinalBiomass_kg, 'internode_optimalFinalBiomass_kg');
    streamBoolean(pInternode.canRecoverFromStuntingDuringCreation, 'canRecoverFromStuntingDuringCreation');
    streamSmallint(pInternode.minDaysToAccumulateBiomass, 'internode_minDaysToAccumulateBiomass');
    streamSmallint(pInternode.maxDaysToAccumulateBiomass, 'internode_maxDaysToAccumulateBiomass');
    streamSmallint(pInternode.minDaysToExpand, 'internode_minDaysToExpand');
    streamSmallint(pInternode.maxDaysToExpand, 'internode_maxDaysToExpand');
    streamSingle(pInternode.lengthAtOptimalFinalBiomassAndExpansion_mm,
        'internode_lengthAtOptimalFinalBiomassAndExpansion_mm');
    streamSingle(pInternode.lengthMultiplierDueToBiomassAccretion, 'internode_lengthMultiplierDueToBiomassAccretion');
    streamSingle(pInternode.lengthMultiplierDueToExpansion, 'internode_lengthMultiplierDueToExpansion');
    streamSingle(pInternode.widthAtOptimalFinalBiomassAndExpansion_mm, 'internodewidthAtOptimalFinalBiomassAndExpansion_mm');
    streamSingle(pInternode.widthMultiplierDueToBiomassAccretion, 'internode_widthMultiplierDueToBiomassAccretion');
    streamSingle(pInternode.widthMultiplierDueToExpansion, 'internode_widthMultiplierDueToExpansion');
    streamSingle(pInternode.lengthMultiplierDueToBolting, 'internode_lengthMultiplierDueToBolting');
    streamSmallint(pInternode.minDaysToBolt, 'internode_minDaysToBolt');
    streamSmallint(pInternode.maxDaysToBolt, 'internode_maxDaysToBolt');
    {pFlower - male and female}
    for i := kGenderMale to kGenderFemale do
      begin
      ws := genderString(i) + 'Flower' + '_';
      if isWriting then
        if pFlower[i].object3D <> nil then tdoName := pFlower[i].object3D.getName else tdoName := 'none';
      streamString(tdoName, ws + 'tdo');
      if isReading and (provider <> nil) then
        pFlower[i].object3D := provider.findMatchForObject3dByName(tdoName);
      streamColorRef(pFlower[i].faceColor, ws + 'faceColor');
      streamColorRef(pFlower[i].backfaceColor, ws + 'backfaceColor');
      streamSingle(pFlower[i].optimalBiomass_kg, ws + 'optimalBiomass_kg');
      streamSingle(pFlower[i].minFractionOfOptimalInitialBiomassToCreateFlower_frn,
          ws + 'minFractionOfOptimalInitialBiomassToCreateFlower_frn');
      streamSmallint(pFlower[i].minDaysToCreateFlowerBud, ws + 'minDaysToCreateFlowerBud');
      streamSmallint(pFlower[i].maxDaysToCreateFlowerBudIfOverMinFraction,
          ws + 'maxDaysToCreateFlowerBudIfOverMinFraction');
      streamSingle(pFlower[i].minFractionOfOptimalBiomassToOpenFlower_frn,
          ws + 'minFractionOfOptimalBiomassToOpenFlower_frn');
      streamSmallint(pFlower[i].minDaysToGrow, ws + 'minDaysToGrow');
      streamSmallint(pFlower[i].maxDaysToGrowIfOverMinFraction, ws + 'maxDaysToGrowIfOverMinFraction');
      streamSmallint(pFlower[i].daysBeforeDrop, ws + 'daysBeforeDrop');
      streamSingle(pFlower[i].scaleAtFullSize, ws + 'scaleAtFullSize');
      streamSmallint(pFlower[i].numPetals, ws + 'numPetals');
      streamBoolean(pFlower[i].petalsAreRadiallyArranged, ws + 'petalsAreRadiallyArranged');
      if isWriting then
        if pFlower[i].budObject3D <> nil then tdoName := pFlower[i].budObject3D.getName else tdoName := 'none';
      streamString(tdoName, ws + 'tdo');
      if isReading and (provider <> nil) then
        pFlower[i].budObject3D := provider.findMatchForObject3dByName(tdoName);
      streamColorRef(pFlower[i].budFaceColor, ws + 'budFaceColor');
      streamColorRef(pFlower[i].budBackfaceColor, ws + 'budBackfaceColor');
      streamSingle(pFlower[i].budScaleAtFullSize, ws + 'budScaleAtFullSize');
      streamSmallint(pFlower[i].budNumPetals, ws + 'budNumPetals');
      streamBoolean(pFlower[i].budPetalsAreRadiallyArranged, ws + 'budPetalsAreRadiallyArranged');
      streamSingle(pFlower[i].minFractionOfOptimalBiomassToCreateFruit_frn,
          ws + 'minFractionOfOptimalBiomassToCreateFruit_frn');
      end;
    {pInflorescence - male and female}
    for i := kGenderMale to kGenderFemale do
      begin
      ws := genderString(i) + 'Inflor' + '_';
      streamSingle(pInflor[i].optimalBiomass_kg, ws + 'optimalBiomass_kg');
      streamSingle(pInflor[i].minFractionOfOptimalBiomassToCreateInflorescence_frn,
          ws + 'minFractionOfOptimalBiomassToCreateInflorescence_frn');
      streamSmallint(pInflor[i].minDaysToCreateInflorescence, ws + 'minDaysToCreateInflorescence');
      streamSmallint(pInflor[i].maxDaysToCreateInflorescenceIfOverMinFraction,
          ws + 'maxDaysToCreateInflorescenceIfOverMinFraction');
      streamSingle(pInflor[i].minFractionOfOptimalBiomassToMakeFlowers_frn,
          ws + 'minFractionOfOptimalBiomassToMakeFlowers_frn');
      streamSmallint(pInflor[i].minDaysToGrow, ws + 'minDaysToGrow');
      streamSmallint(pInflor[i].maxDaysToGrow, ws + 'maxDaysToGrow');
      streamSingle(pInflor[i].peduncleLength_mm, ws + 'peduncleLength_mm');
      streamSingle(pInflor[i].internodeLength_mm, ws + 'internodeLength_mm');
      streamSingle(pInflor[i].internodeWidth_mm, ws + 'internodeWidth_mm');
      streamSingle(pInflor[i].pedicelLength_mm, ws + 'pedicelLength_mm');
      streamSingle(pInflor[i].pedicelAngle, ws + 'pedicelAngle');
      streamSingle(pInflor[i].branchAngle, ws + 'branchAngle');
      streamSingle(pInflor[i].terminalStalkLength_mm, ws + 'terminalStalkLength_mm');
      streamSmallint(pInflor[i].numFlowers, ws + 'numFlowers');
      streamSmallint(pInflor[i].daysToAllFlowersCreated, ws + 'daysToAllFlowersCreated');
      streamSmallint(pInflor[i].numBranches, ws + 'numBranches');
      streamBoolean(pInflor[i].branchesAreAlternate, ws + 'branchesAreAlternate');
      streamBoolean(pInflor[i].isHead, ws + 'isHead');
      streamBoolean(pInflor[i].isTerminal, ws + 'isTerminal');
      streamBoolean(pInflor[i].flowersDrawTopToBottom, ws + 'flowersDrawTopToBottom');
      streamBoolean(pInflor[i].flowersSpiralOnStem, ws + 'flowersSpiralOnStem');
      streamColorRef(pInflor[i].stalkColor, ws + 'stalkColor');
      end;
      {pAxillaryBud}
      if isWriting then
        if pAxillaryBud.object3d <> nil then tdoName := pAxillaryBud.object3d.getName else tdoName := 'none';
      streamString(tdoName, 'axil_tdo');
      if isReading and (provider <> nil) then
        pAxillaryBud.object3d := provider.findMatchForObject3dByName(tdoName);
      streamColorRef(pAxillaryBud.faceColor, 'axil_faceColor');
      streamColorRef(pAxillaryBud.backfaceColor, 'axil_backfaceColor');
      streamSingle(pAxillaryBud.scale, 'axil_scale');
      {pMeristem}
      streamSingle(pMeristem.branchingIndex, 'meristem_branchingIndex');
      streamSingle(pMeristem.branchingDistance, 'meristem_branchingDistance');
      streamSingle(pMeristem.determinateProbability, 'meristem_determinateProbability');
      streamBoolean(pMeristem.branchingIsSympodial, 'meristem_branchingIsSympodial');
      streamBoolean(pMeristem.branchingIsAlternate, 'meristem_branchingIsAlternate');
      {pFruit}
      if isWriting then
        if pFruit.object3d <> nil then tdoName := pFruit.object3d.getName else tdoName := 'none';
      streamString(tdoName, 'fruit_tdo');
      if isReading and (provider <> nil) then
        pFruit.object3d := provider.findMatchForObject3dByName(tdoName);
      streamColorRef(pFruit.unripeFaceColor, 'fruit_unripeFaceColor');
      streamColorRef(pFruit.unripeBackfaceColor, 'fruit_unripeBackfaceColor');
      streamColorRef(pFruit.ripeFaceColor, 'fruit_ripeFaceColor');
      streamColorRef(pFruit.ripeBackfaceColor, 'fruit_ripeBackfaceColor');
      streamSCurve(pFruit.sCurveParams, 'fruit_scurve');
      streamSingle(pFruit.scaleAtFullSize, 'fruit_scaleAtFullSize');
      streamSingle(pFruit.optimalBiomass_kg, 'fruit_optimalBiomass_kg');
      streamSmallint(pFruit.minDaysToGrow, 'fruit_minDaysToGrow');
      streamSmallint(pFruit.maxDaysToGrow, 'fruit_maxDaysToGrow');
      streamSingle(pFruit.stalkStrengthIndex, 'fruit_stalkStrengthIndex');
      streamSmallint(pFruit.daysToRipen, 'fruit_daysToRipen');
      streamSmallint(pFruit.numSections, 'fruit_numSections');
      streamBoolean(pFruit.sectionsAreRadiallyArranged, 'fruit_sectionsAreRadiallyArranged');
      {pRoot}
      if isWriting then
        if pRoot.object3d <> nil then tdoName := pRoot.object3d.getName else tdoName := 'none';
      streamString(tdoName, 'root_tdo');
      if isReading and (provider <> nil) then
        pRoot.object3d := provider.findMatchForObject3dByName(tdoName);
      streamColorRef(pRoot.faceColor, 'root_faceColor');
      streamColorRef(pRoot.backfaceColor, 'root_backfaceColor');
      streamSingle(pRoot.scaleAtFullSize, 'root_scaleAtFullSize');
      streamBoolean(pRoot.showsAboveGround, 'root_showsAboveGround');
    { DO NOT do an end of line because plant does it }
    end;
 end;

begin
{These worst stress condition colors are constant over all plants.
This makes sense because chlorosis (yellowing) is always yellow;
age is always brown;
and P stress is always dark green.}
worstNStressColor := support_rgb(200, 200, 0);
worstPStressColor := support_rgb(20, 50, 20);
worstDeadColor := support_rgb(100, 100, 0);
end.

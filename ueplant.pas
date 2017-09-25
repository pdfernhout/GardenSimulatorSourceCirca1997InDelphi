unit ueplant;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ueplant: Plant object. Information in the plant is in several record structures
as specified in uestruct. Always make sure to recalculate biomass relationships
when changing any of the biomass components. And tell the drawing plant when
doing this by setting a var for the daily change in X, then changing the drawing
plant code to look at the change in X when it does its next day routine.
Read uestruct to understand the structures here before reading this code.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses SysUtils, Graphics, Classes, WinTypes, ExtCtrls, ufiler, ufilertx, uestruct, uesoil, umodel, umconsts,
  udplant, ucollect, udppart, urandom;

type
GsPlant = class(GsGraphicalModel)
  public
  {model vars}
  soil: GsSoilPatch;
  drawingPlant: GsDrawingPlant;
  options: plantOptionsArray;
  params: PlantParamsStructure;
  development: DevelopmentStructure;
  water: PlantWaterStructure;
  nutrients: NutrientsStructure;
  biomass: BiomassStructure;
  constraints: ConstraintsStructure;
  randomNumberGenerator: GsRandom;
  {graphical vars}
  basePoint: TPoint;
  possibleSize: smallint;
  computeBounds: boolean;  {used to know when drawing in garden to get turtle bounds}
  isGrowingOptimally: boolean; {temporary, doesn't need to be streamed}
  {constructor/destructor}
  awaitingReseeding: boolean; {plant is no longer in model; but is stored to be used for resseding later}
  cultivarName: string[kGraphicalModelNameLength];
  isInStasisBox: boolean; 
  constructor createWithSoilPatchAndPoint(aSoilPatch: GsSoilPatch; point: TPoint);
  constructor create; override;
  destructor destroy; override;
  procedure randomizeFromTime;
  {graphical functions}
  function includesPoint(const aPoint: TPoint): boolean;  override;
  function preciseIncludesPointTest(const aPoint: TPoint): boolean; override;
  procedure moveBy(delta: TPoint);
  procedure moveTo(newPoint: TPoint);
  procedure transplant(oldPoint: TPoint; oldSoilPatch: GsSoilPatch; newPoint: TPoint; newSoilPatch: GsSoilPatch);
  procedure adjustForNewSoilPatchArea(oldArea_ha, newArea_ha: single);
  function actualSize: smallint;
	function determineScale: single;
  procedure drawOn(destCanvas: TCanvas); override;
  procedure drawOnAsSymbol(destCanvas: TCanvas);
  procedure drawOnAsPlant(destCanvas: TCanvas);
	procedure expandBoundsRectForLineWidth;
	procedure enforceMinimumBoundsRect;
  procedure computeBoundsRect;
  function shouldDrawFromTop: boolean;
  {i/o functions}
  procedure setCultivarName(aString: string);
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure streamDataWithoutParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord;
      streamDrawingPlant: boolean);
  procedure streamParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord;
      streamDrawingPlant: boolean);
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure copyFromCultivar(cultivar: GsPlant; copyParams: boolean);
  procedure copyToWithoutParams(newCopy: GsPlant);
  procedure ConvertRelativeAndAbsolute(direction: integer; var modelValue: single; var value: single);
  procedure BDConvert(var modelValue: single; var value;
    fieldID, deriveMethod, fieldType, direction, index: integer; updateList: TListCollection);
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  procedure ConvertLeafConductanceParams;
  procedure ConvertOptimalPParams;
  procedure ConvertOptimalNParams;
  procedure calculatePotentialHeatUnits(beingPlantedNow: boolean);
  procedure calculatePotentialHeatUnitsAtPlanting;
  { next day functions }
  procedure PlantNextDayBeforeWaterAndNutrientAllocation;
  procedure PlantNextDayBetweenWaterAndNutrientAllocation;
  procedure PlantNextDayAfterWaterAndNutrientAllocation;
  { the functions called by next day }
  procedure CheckForAnnualOrBiennialDeath;
  function isSeed: boolean;
  function isNotSeed: boolean;
  function isAnnual: boolean;
  function isBiennial: boolean;
  function awaitingReseedingOrInStasisBox: boolean;
  function optionIsTrue(index: smallint): boolean;
  procedure SetWaterUseAtOptimalIfOptionOn;
  procedure SetNitrogenUseAtOptimalIfOptionOn;
  procedure SetPhosphorusUseAtOptimalIfOptionOn;
  function CheckForGerminationIfSeed(theDay: integer): integer;
  procedure ZeroPlantDailyVars;
  procedure SetPlantDailyDefaults;
  procedure AccumulateVernalizationUnits;
  procedure AccumulateHeatUnits;
  procedure CheckForDormancy;
  procedure BiomassTempStressFactor;
  procedure LeafAreaIndex;
  procedure PlantHeight;
  procedure RootDepth;
  procedure CalcNumLayersWithRootsInThem;
  procedure RootSystemDiameter;
  procedure FrostAndShortDaylengthDamage;
  procedure FrostDeath;
  procedure RootGrowthConstraintByLayer;
  procedure WaterDemand_mm;
  procedure AerationStress;
  procedure PotentialBiomassIncrease;
  procedure NDemandAndFixation;
  procedure FinishNUptake;
  procedure PDemand;
  procedure FinishPUptake;
  procedure CombinedBiomassConstraint;
  procedure AccumulateWaterUse;
  procedure IncreaseBiomassWithStress;
  procedure AllocateNewBiomassAmongComponents;
  procedure FallingLeavesIfTree(theMonth: integer);
  procedure DecreaseBiomassFromDryness;
  procedure ReproductiveBiomassDecay;
  procedure ZeroHeatUnitsIfOldAndSmall;
  procedure LifeHistoryStage;
  procedure Die;
	function dayLengthIsAboveThresholdForGrowth: boolean;
  function minDayLengthForWinterGrowth_hr: single;
  procedure transferPropOfStandingDeadToFlatCropResidue(proportion: single);
  function nConcInLiveBiomass_kgPt: single;
  function pConcInLiveBiomass_kgPt: single;
  { initializing functions }
  procedure ZeroPlantCumulativeVars;
  procedure SetDefaultsForSeed;
  { tool functions }
  procedure plantFromCultivar(cultivar: GsPlant);
  procedure addOrRemovePlantPartBiomass(plantPart: GsDrawingPlantPart; addOrRemoveFlag: boolean);
  procedure becomeSeedNoReturns(startGrowingNow: boolean);
  procedure becomeSeed(startGrowingNow: boolean; var lostBiomass: BiomassLostInReseedingStructure);
  function isDisplayedAsSymbol: boolean;
	function objectType: integer; override;
  class procedure fillEnumStringList(var list: TStringList; fieldID: Integer; var hasRadioButtons: boolean);
  procedure grow(numDays: longint);
  function getHint(longHint: boolean): string;
  { side effect functions }
  procedure sideEffects_optimalTemp_degC(updateList: TListCollection);
  procedure sideEffects_baseTemp_degC(updateList: TListCollection);
  end;

implementation

{PDF FIX - CFK FIX - would be better if domain was not in here.  It is only used
for flags of whether to draw plant as symbol or not and whether to call drawing
plant next day.  Yet difficult to pass them through functions...}

uses
  uegarden, ueweath, ueutils, ueq, uep, ueg, ueorgmat, uunits, udebug, uaspects,
  uturt3d, dialogs, udomain, forms, udate, uclasses, udefault, uhardcd, usupport, utstream;

const
  kBeingPlantedNow = true;
  kNotBeingPlantedNow = false;

{ ------------------------------------------------------------------------------------------ next day }
procedure GsPlant.PlantNextDayBeforeWaterAndNutrientAllocation;
  var
    germinationResult: integer;
  begin
  try
  { if in stasis box, leave immediately }
  if self.isInStasisBox then exit;
  { if awaiting reseeding, check if should become seed }
  if awaitingReseeding then
    begin
    if isNotSeed or (development.cumHeatUnits <> 0.0) then
    	self.becomeSeedNoReturns(kBecomeSeedAndStartGrowingNow);
    exit;
    end;
  ZeroPlantDailyVars;
  SetPlantDailyDefaults;
  if development.isDead then exit;
  CheckForAnnualOrBiennialDeath;
  if development.isDead then exit;
  if isSeed then
    begin
    germinationResult := CheckForGerminationIfSeed(soil.garden.julianDayToday);
    if germinationResult = kPlantHasNotGerminated then exit;
    end;
  AccumulateHeatUnits;
  if params.vernalizationRequirement <> kNoVernalization then
    AccumulateVernalizationUnits;
  CheckForDormancy;
  BiomassTempStressFactor;
  LeafAreaIndex;
  PlantHeight;
  RootDepth;
  CalcNumLayersWithRootsInThem;
  RootSystemDiameter;
  FrostAndShortDaylengthDamage;
  FrostDeath;
  if development.isDead then exit;
  if constraints.biomassTempStressFactor_frn > 0.0 then
    begin
    if development.isDormant then
      constraints.biomassGrowthConstraint_frn := 1.0
    else
      RootGrowthConstraintByLayer;
    end;
  except
    on e: Exception do errorMessage('Exception in PlantNextDayBeforeWaterAndNutrientAllocation: ' + e.message); end;
  end;

procedure GsPlant.PlantNextDayBetweenWaterAndNutrientAllocation;
  begin
  try
  if self.awaitingReseedingOrInStasisBox then exit;
  if isSeed or (development.isDead) or (constraints.biomassTempStressFactor_frn <= 0.0) or (development.isDormant) then
    exit;
  SetWaterUseAtOptimalIfOptionOn;
  AerationStress;
  PotentialBiomassIncrease;
  except on e: Exception do
    errorMessage('Exception in plant PlantNextDayBetweenWaterAndNutrientAllocation: ' + e.message); end;
  end;

procedure GsPlant.PlantNextDayAfterWaterAndNutrientAllocation;
  begin
  try
  if self.awaitingReseedingOrInStasisBox then exit;
  if isNotSeed and (not development.isDead) then
    begin
    if (constraints.biomassTempStressFactor_frn > 0.0) then
      begin
      if development.isDormant then
        constraints.biomassGrowthConstraint_frn := 1.0
      else
        begin
        CombinedBiomassConstraint;
        AccumulateWaterUse;
        LifeHistoryStage;
        IncreaseBiomassWithStress;
        AllocateNewBiomassAmongComponents;
        ReproductiveBiomassDecay;
        end;
      FallingLeavesIfTree(GsDate_monthFromDate(soil.garden.date));
      DecreaseBiomassFromDryness;
      ZeroHeatUnitsIfOldAndSmall;
      end
    else { biomassTempStressFactor_frn = 0 }
      constraints.biomassGrowthConstraint_frn := 0.0;
    inc(development.age);
    if development.age mod 364 = 0 then
      inc(development.yearsOld);
    end;
  { call drawing plant next day even if dead, because it has to deal with standing dead deallocation }
  if isNotSeed and (drawingPlant <> nil) then
    drawingPlant.nextDay;
  except
    on e: Exception do errorMessage('Exception in plant PlantNextDayAfterWaterAndNutrientAllocation: ' + e.message); end;
  end;

{ ------------------------------------------------------------------------------------------ create/destroy }
constructor GsPlant.create;
  var i: smallint;
	begin
  inherited create;
  drawingPlant := GsDrawingPlant.createWithModelPlant(self);
  RandomNumberGenerator := GsRandom.create;
  computeBounds := false;
  awaitingReseeding := false;
  isInStasisBox := false;
  for i := 0 to kPlantOptionsLastOption do options[i] := true;
  isGrowingOptimally := false;
  self.ZeroPlantCumulativeVars;
  possibleSize := 20;
  constraints.biomassGrowthConstraint_frn := 1.0;
  end;

procedure GsPlant.randomizeFromTime;
  var i: smallint;
  begin
  if drawingPlant <> nil then
    drawingPlant.randomizeFromTime;
  randomNumberGenerator.randomizeFromTime;
  for i := 0 to 100 do randomNumberGenerator.zeroToOne;
  end;

constructor GsPlant.createWithSoilPatchAndPoint(aSoilPatch: GsSoilPatch; point: TPoint);
	begin
  self.create;
  soil := aSoilPatch;
  basePoint := point;
  self.computeBoundsRect;
  end;

destructor GsPlant.destroy;
	begin
  drawingPlant.free;
  RandomNumberGenerator.free;
  RandomNumberGenerator := nil;
  inherited destroy;
  end;

{ ------------------------------------------------------------------------------------------ drawing/graphics }
function GsPlant.isDisplayedAsSymbol: boolean;
  begin
  result := (Domain.menuOptions.drawSeedsAsSymbols and isSeed) or
  	(Domain.menuOptions.drawPlantsAsSymbols and isNotSeed);
  end;

function GsPlant.getHint(longHint: boolean): string;
  var
    tPha_to_g: single;
    worstStressString: string;
  begin
  result := self.getName;
  if not longHint then exit;
  if isTemplate then exit;
  if soil = nil then exit;
  if development.isDead then
    result := result + ', dead'
  else if (development.lifeHistoryStage = kSeed) then
    result := result + ', seed, heat units ' + digitValueString(development.cumHeatUnits)
  else if development.isDormant then
    result := result + ', dormant'
  else
    begin
    result := result + '. Heat unit index ' + digitValueString(development.heatUnitIndex);
    result := result + ', leaf area index ' + digitValueString(development.leafAreaIndex);
    tPha_to_g := t_to_kg * kg_to_g * soil.params.area_ha;
    result := result + '. Live biomass ' + digitValueString(biomass.totalLive_tPha * tPha_to_g) + ' g';
    result := result + ', shoot ' + digitValueString(biomass.totalShootWeight_tPha * tPha_to_g) + ' g';
    result := result + ', root ' + digitValueString(biomass.totalRootWeight_tPha * tPha_to_g) + ' g';
    if biomass.flowersAndFruits_tPha > 0 then
      result := result + ', flower/fruit ' + digitValueString(biomass.flowersAndFruits_tPha * tPha_to_g) + ' g';
    if biomass.storageOrgan_tPha > 0 then
      result := result + ', storage ' + digitValueString(biomass.storageOrgan_tPha * tPha_to_g) + ' g';
    if biomass.standingDead_tPha > 0 then
      result := result + '. Standing dead ' + digitValueString(biomass.standingDead_tPha * tPha_to_g) + ' g';
    result := result + '. Stresses: ';
    if constraints.biomassGrowthConstraint_frn = 1.0 then
      worstStressString := ''
    else
      worstStressString := ' (worst)';
    result := result + 'N ' + digitValueString(constraints.nStressFactor_frn);
    if constraints.biomassGrowthConstraint_frn = constraints.nStressFactor_frn then
      result := result + worstStressString;
    result := result + ', P ' + digitValueString(constraints.pStressFactor_frn);
    if constraints.biomassGrowthConstraint_frn = constraints.pStressFactor_frn then
      result := result + worstStressString;
    result := result + ', water ' + digitValueString(constraints.waterStressFactor_frn);
    if constraints.biomassGrowthConstraint_frn = constraints.waterStressFactor_frn then
      result := result + worstStressString;
    result := result + ', temperature ' + digitValueString(constraints.biomassTempStressFactor_frn);
    if constraints.biomassGrowthConstraint_frn = constraints.biomassTempStressFactor_frn then
      result := result + worstStressString;
    result := result + ', aeration ' + digitValueString(constraints.aerationStressFactor_frn) + '.';
    if constraints.biomassGrowthConstraint_frn = constraints.aerationStressFactor_frn then
      result := result + worstStressString;
    end;
  end;

procedure GsPlant.computeBoundsRect;
  begin
  try
  if self.isDisplayedAsSymbol then
    begin
    {extended rect by two to account for pen width}
    if self.shouldDrawFromTop then
      boundsRect := Rect(basePoint.x - (possibleSize div 2) - 2, basePoint.y - (possibleSize div 2) - 2,
        basePoint.x + (possibleSize div 2) + 2, basePoint.y + (possibleSize div 2) + 2)
    else
  	  boundsRect := Rect(basePoint.x - (possibleSize div 4) - 2, basePoint.y - possibleSize - 2,
  			basePoint.x + (possibleSize div 4) + 2, basePoint.y + 2)
    end
  else
    begin
    {the boundsRect for pretty plant is always computed when re-drawn}
    { need to compute it here only for partial redraws when plant is tiny}
  	self.enforceMinimumBoundsRect;
    end;
  except on e: Exception do errorMessage('Exception in plant computeBoundsRect: ' + e.message); end;
  end;

function GsPlant.shouldDrawFromTop: boolean;
  begin
  result := (soil.params.viewingAngle_deg >= 60.0);
  end;

function GsPlant.includesPoint(const aPoint: TPoint): boolean;
  var plantCloseness: integer;
  begin
  plantCloseness := Domain.menuOptions.plantProximityNeeded;
  with basePoint do
  	result := (aPoint.x >= x - plantCloseness) and (aPoint.x <= x + plantCloseness) and
    	(aPoint.y >= y - plantCloseness - 1) and (aPoint.y <= y + plantCloseness + 1);
  end;

function GsPlant.preciseIncludesPointTest(const aPoint: TPoint): boolean;
	begin
  Result := true;
  end;

procedure GsPlant.moveBy(delta: TPoint);
  begin
  boundsRect.left := boundsRect.left + delta.x;
  boundsRect.right := boundsRect.right + delta.x;
  boundsRect.top := boundsRect.top + delta.y;
  boundsRect.bottom := boundsRect.bottom + delta.y;
  basePoint.x := basePoint.x + delta.x;
  basePoint.y := basePoint.y + delta.y;
  end;

procedure GsPlant.moveTo(newPoint: TPoint);
  begin
  boundsRect.left := boundsRect.left + newPoint.x - basePoint.x;
  boundsRect.right := boundsRect.right + newPoint.x - basePoint.x;
  boundsRect.top := boundsRect.top + newPoint.y - basePoint.y;
  boundsRect.bottom := boundsRect.bottom + newPoint.y - basePoint.y;
  basePoint.x := newPoint.x;
  basePoint.y := newPoint.y;
  end;

procedure GsPlant.transplant(oldPoint: TPoint; oldSoilPatch: GsSoilPatch; newPoint: TPoint; newSoilPatch: GsSoilPatch);
   begin
   self.moveTo(newPoint);
   if oldSoilPatch <> newSoilPatch then
     begin
  	 oldSoilPatch.removePlant(self);
  	 newSoilPatch.addPlant(self);
     soil := newSoilPatch;
     oldSoilPatch.updatePlantMeans;
     newSoilPatch.updatePlantMeans;
     self.adjustForNewSoilPatchArea(oldSoilPatch.params.area_ha, newSoilPatch.params.area_ha); 
     oldSoilPatch.computeExtendedBoundsRect;
     end;
   newSoilPatch.computeExtendedBoundsRect;
   end;

procedure GsPlant.drawOn(destCanvas: TCanvas);
  begin
  if awaitingReseeding then exit; {ok to draw if in stasis box}
  if self.isDisplayedAsSymbol then
    self.drawOnAsSymbol(destCanvas)
  else
    self.drawOnAsPlant(destCanvas);
  end;

function GsPlant.actualSize: smallint;
  begin
  if awaitingReseeding then
    result := 0
  else
  	result := intMax(0, intMin(round(development.heatUnitIndex * self.possibleSize), self.possibleSize));
  end;

procedure GsPlant.drawOnAsSymbol(destCanvas: TCanvas);
  var
  	points : array[0..2] of TPoint;
    drawFromTop: boolean;
    halfPossible, quarterPossible: longint;
    drawColor: TColor;
	begin
  drawFromTop := self.shouldDrawFromTop;
  halfPossible := possibleSize div 2;
  quarterPossible := possibleSize div 4;
  with destCanvas do
    begin
    { draw border first }
    pen.color := clRed;
    pen.style := psSolid;
    pen.width := 1;
    brush.color := clWhite;
    brush.style := bsClear;
    { pen.mode := pmXor; }
    if drawFromTop then
      begin
      ellipse(basePoint.x - halfPossible - 0, basePoint.y - halfPossible - 0,
        basePoint.x + halfPossible + 0, basePoint.y + halfPossible + 0)
      end
    else
      begin
      points[0] := Point(basePoint.x, basePoint.y + 0);
      points[1] := Point(basePoint.x - quarterPossible - 0, basePoint.y - possibleSize - 0);
      points[2] := Point(basePoint.x + quarterPossible + 0, basePoint.y - possibleSize - 0);
      polygon(points);
      end;
   { fill with color inside border using actualSize;
      gray if dead, green if young, red if reproductive }
   if development.isDead then
     drawColor := clGray
   else if development.lifeHistoryStage = kReproductiveAllocationPeriod then
     drawColor := clRed
   else
     drawColor := clLime;
   pen.color := drawColor;
   brush.color := drawColor;
   brush.style := bsSolid;
   pen.mode := pmCopy;
   if drawFromTop then
     ellipse(basePoint.x - actualSize div 2, basePoint.y - actualSize div 2,
       basePoint.x + actualSize div 2, basePoint.y + actualSize div 2)
   else
     begin
     points[0] := Point(basePoint.x, basePoint.y);
     points[1] := Point(basePoint.x - actualSize div 4, basePoint.y - actualSize);
     points[2] := Point(basePoint.x + actualSize div 4, basePoint.y - actualSize);
     polygon(points);
     end;
   pen.color := clGray;
   end;
  { probably unneeded most times, except when first switch into symbol from plant drawing }
  if self.computeBounds then
  	self.computeBoundsRect;
  if self.isInStasisBox then
    begin
    destCanvas.brush.color := clWhite;
    destCanvas.brush.style := bsClear;
    destCanvas.pen.color := clWhite;
    with self.boundsRect do destCanvas.rectangle(left+1, top+1, right-1, bottom-1);
    end;
  end;

function GsPlant.determineScale: single;
  begin
  case soil.garden.gardenScaleEffect of
  	kGardenScaleIgnore:
  		result := soil.params.drawingScale;
  	kGardenScaleOverridesPatchScale:
  		result := soil.garden.gardenScale;
  	kGardenScaleMultipliesPatchScale:
  		result := soil.garden.gardenScale * soil.params.drawingScale;
    else
      raise Exception.create('GsPlant.determineScale: bad scale option');
    end;
  end;

procedure GsPlant.drawOnAsPlant(destCanvas: TCanvas);
  var
    turtle: KfTurtle;
    sortPolygons: boolean;
    plantRotateAngle: integer;
    plantScale: single;
    viewingAngle: smallint;
  begin
  plantRotateAngle := 0;
  if self.drawingPlant = nil then exit;
  sortPolygons := false;
  turtle := KfTurtle.defaultStartUsing;
  try
  turtle.drawingSurface.pane := destCanvas;
  turtle.drawOptions := Domain.menuOptions.mainWindowPlantDrawOptions;
  turtle.reset;  { must be after pane and draw options set }
  plantScale := self.determineScale;
  turtle.mmPerPixel(plantScale); 
  turtle.drawingSurface.foreColor := clGreen;
  turtle.drawingSurface.backColor := clRed;
  turtle.drawingSurface.lineColor := clBlue;
  self.drawingPlant.plantWideDrawingVars;
  turtle.xyz(basePoint.x, basePoint.y, 0);
  turtle.resetBoundsRect(basePoint);
  try
    viewingAngle := round(1.0 * soil.params.viewingAngle_deg * 255 / 360);
	  if turtle.drawOptions.sortPolygons then
		  begin
		  turtle.drawingSurface.recordingStart;
		  self.drawingPlant.drawWithTurtle(turtle, plantRotateAngle, viewingAngle);
		  turtle.drawingSurface.recordingStop;
		  turtle.drawingSurface.recordingDraw;
		  turtle.drawingSurface.clearTriangles;
		  end
	  else
		  begin
		  self.drawingPlant.drawWithTurtle(turtle, plantRotateAngle, viewingAngle);
	   end;
    if self.computeBounds then
      begin
      self.boundsRect := turtle.boundsRect;
      end;
    self.enforceMinimumBoundsRect;
    if self.computeBounds then
			self.expandBoundsRectForLineWidth;
   {PDF FIX - need to account for line width - added to bounds rect}
  except
	  on e: Exception do errorMessage('GsPlant.drawOnAsPlant: : ' + e.message);
  end;
  finally
  KfTurtle.defaultStopUsing;
  turtle := nil;
 { Application.ProcessMessages; - causes erratic drawing when dragging plants}
	end;
  if self.isInStasisBox then
    begin
    destCanvas.brush.color := clWhite;
    destCanvas.brush.style := bsClear;
    destCanvas.pen.color := clWhite;
    with self.boundsRect do destCanvas.rectangle(left+1, top+1, right-1, bottom-1);
    end;
  end;

procedure GsPlant.expandBoundsRectForLineWidth;
  var
  	extra: integer;
   	scale: single;
  begin
  scale := 0.5;
  extra := round(self.drawingPlant.maximumLineWidth * scale) + 1;
  dec(boundsRect.left, extra);
  inc(boundsRect.right, extra);
  dec(boundsRect.top, extra);
  inc(boundsRect.bottom, extra);
  end;

procedure GsPlant.enforceMinimumBoundsRect;
  begin
  if ((boundsRect.right - boundsRect.left) < 1) then
    begin
    boundsRect.left := basePoint.x;
    boundsRect.right := boundsRect.left + 1;
    end;
  if ((boundsRect.bottom - boundsRect.top) < 1) then
    begin
    boundsRect.bottom := basePoint.y;
    boundsRect.top := boundsRect.bottom - 1;
    end;
  end;

{ ------------------------------------------------------------------------------------------ called by next day}
function GsPlant.CheckForGerminationIfSeed(theDay: integer): integer;
  var
    randomNumber, tempToUse_degC, soilWaterToUse_mm: single;
  begin
  result := kPlantHasNotGerminated;
  try
  { change from EPIC
    if seed has overwintered (if new seed, daysGerminating < 364, else is old seed),
    check if it has survived the winter. if not, kill it, if so, set heat units to zero.
    if the seed does survive the year, but there is a significant mulch thickness, it cannot sprout.
    planted seeds are assumed to have the mulch pulled away.
    the mulch thickness threshold for 2nd year germination was chosen arbitrarily at 10 cm (0.1 m) }
  if (theDay = development.dayPlanted) and (development.daysGerminating >= 364) then
    begin
    randomNumber := Utils_RandomZeroToOne;
    if (randomNumber > params.probOfGerminationAfterFirstYear_frn) or (soil.mulch.depth_m > 0.1) then
      begin
      self.die;
      exit;
      end
    else
      development.cumHeatUnits := 0.0;
    end;
  { set temp and water to use at optimal if optimal germination is on }
  if optionIsTrue(kPlantOptionsIgnoreGerminationRequirements) then
    tempToUse_degC := params.optimalTemp_degC
  else
    tempToUse_degC := soil.surface.temp_degC;
  if optionIsTrue(kPlantOptionsIgnoreGerminationRequirements) then
    soilWaterToUse_mm := params.minSoilWaterInPlowDepthForGermination_mm
  else
    soilWaterToUse_mm := soil.totals.plowDepthSWmWP_mm;
  { change from EPIC: we added the second term here to bound the accumulation if the temperature is above optimal }
  development.cumHeatUnits := development.cumHeatUnits
      + max(0.0, tempToUse_degC - params.baseTemp_degC) - max(0.0, tempToUse_degC - params.optimalTemp_degC);
  if (development.cumHeatUnits >= params.minHeatUnitsBeforeGermination_degC)
      and (soilWaterToUse_mm >= params.minSoilWaterInPlowDepthForGermination_mm) then
    begin
    development.lifeHistoryStage := kVegetativePeriod;
    development.cumHeatUnits := 0.0;
    result := kPlantHasGerminated;
    end
  else
    inc(development.daysGerminating);
  except on e: Exception do errorMessage('Exception in GsPlant.CheckForGerminationIfSeed: ' + e.message); end;
  end;

procedure GsPlant.ZeroPlantDailyVars;
  var
    layer: integer;
  begin
  { water }
  { DO NOT ZERO potPlantEvap_mm because it is set by the soil patch before the plant NextDay function is called.
    plantWater.potPlantEvap_mm := 0.0;  }
  water.totalWaterDemand_mm := 0.0;
  water.totalWaterUse_mm := 0.0;
  for layer := 0 to kMaxPossibleLayers - 1 do
    begin
    water.waterDemandByLayer_mm[layer] := 0.0;
    water.waterUseByLayer_mm[layer] := 0.0;
    nutrients.nDemandByLayer_kgPha[layer] := 0.0;
    nutrients.nSupplyByLayer_kgPha[layer] := 0.0;
    nutrients.nUptakeByLayer_kgPha[layer] := 0.0;
    nutrients.pDemandByLayer_kgPha[layer] := 0.0;
    nutrients.pUptakeByLayer_kgPha[layer] := 0.0;
    nutrients.pSupplyByLayer_kgPha[layer] := 0.0;
    constraints.rootGrowthConstraintByLayer_frn[layer] := 0.0;
    constraints.rootTempStressFactorByLayer_frn[layer] := 0.0;
    constraints.rootAlStressFactorByLayer_frn[layer] := 0.0;
    constraints.rootSoilStrengthFactorByLayer_frn[layer] := 0.0;
    end;
  { nutrients }
  nutrients.optimalNConc_kgPkg := 0.0;
  nutrients.nDemandForOptimalGrowth_kgPha := 0.0;
  nutrients.nDemand_kgPha := 0.0;
  nutrients.nSupply_kgPha := 0.0;
  nutrients.nUptake_kgPha := 0.0;
  nutrients.nFixation_kgPha := 0.0;
  nutrients.nonFixationNUptakeOverDemand_frn := 0.0;
  nutrients.optimalPConc_kgPkg := 0.0;
  nutrients.pDemandForOptimalGrowth_kgPha := 0.0;
  nutrients.pDemand_kgPha := 0.0;
  nutrients.pSupply_kgPha := 0.0;
  nutrients.pUptake_kgPha := 0.0;
  nutrients.pUptakeOverDemand := 0.0;
  { heat units }
  development.heatUnitFactorForLAIAndHeight := 0.0;
  { constraints }
  constraints.nStressFactor_frn := 0.0;
  constraints.pStressFactor_frn := 0.0;
  constraints.waterStressFactor_frn := 0.0;
  constraints.biomassTempStressFactor_frn := 0.0;
  constraints.aerationStressFactor_frn := 0.0;
  { don't set constraints.biomassGrowthConstraint_frn because LAI uses yesterday's value }
  { don't set biomassAdjustmentIfLAIDeclining because if plant is dormant it is not calculated;
    better to use last small value }
  constraints.winterBiomassFrostReductionFactor := 0.0;
  constraints.winterBiomassDayLengthReductionFactor := 0.0;
  constraints.winterBiomassReductionFactor := 0.0;
  constraints.snowCoverFactorForFrostKill := 0.0;
  { biomass }
  biomass.potentialIncrease_tPha := 0.0;
  biomass.actualIncrease_tPha := 0.0;
  biomass.reproductiveAllocation_tPha := 0.0;
  biomass.storageOrganAllocation_tPha := 0.0;
  biomass.shootAllocation_tPha := 0.0;
  for layer := 0 to kMaxPossibleLayers - 1 do
    biomass.rootAllocationByLayer_tPha[layer] := 0.0;
  biomass.reductionFromFrostAndDaylength_tPha := 0.0;
  biomass.reductionFromFallingLeaves_tPha := 0.0;
  biomass.reductionFromDryness_tPha := 0.0;
  biomass.photoActiveRadiation_MJPm2 := 0.0;
  biomass.reductionFromReproductiveDecay_tPha := 0.0;
  biomass.reductionFromAboveGroundVegetativeDeath_tPha := 0.0;
  biomass.reductionFromReproductiveDeath_tPha := 0.0;
  biomass.reductionInStandingDeadFromTransferToResidue_tPha := 0.0;
  biomass.mobilizationOutOfShoots_tPha := 0.0;
  biomass.mobilizationOutOfRoots_tPha := 0.0;
  biomass.mobilizationOutOfStorage_tPha := 0.0;
  end;

procedure GsPlant.SetPlantDailyDefaults;
  begin
  nutrients.nonFixationNUptakeOverDemand_frn := 1.0;
  nutrients.pUptakeOverDemand := 1.0;
  constraints.nStressFactor_frn := 1.0;
  constraints.pStressFactor_frn := 1.0;
  constraints.waterStressFactor_frn := 1.0;
  constraints.biomassTempStressFactor_frn := 1.0;
  constraints.aerationStressFactor_frn := 1.0;
  end;

procedure GsPlant.AccumulateVernalizationUnits;
  var
    tempToUse_degC: single;
  begin
  try
  if development.cumVernalizationUnits >= params.thermalUnitsRequiredForVernalization then
    exit;
  if optionIsTrue(kPlantOptionsOptimalTemperature) then
    tempToUse_degC := params.optTempForVernalization_degC
  else
    tempToUse_degC := soil.garden.weather.dailyWeather.meanTempForDay_degC;
  development.cumVernalizationUnits := development.cumVernalizationUnits +
    (GS.VernalizationUnitsForDay(params.minTempForVernalization_degC, params.optTempForVernalization_degC,
    params.maxTempForVernalization_degC, tempToUse_degC));
  except
    on e: Exception do errorMessage('Exception in plant AccumulateVernalizationUnits');
  end;
  end;

procedure GsPlant.AccumulateHeatUnits;
  var
    tempToUse_degC: single;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalTemperature) then
    tempToUse_degC := params.optimalTemp_degC
  else
    tempToUse_degC := soil.garden.weather.dailyWeather.meanTempForDay_degC;
  development.cumHeatUnits := development.cumHeatUnits + EQ.HeatUnitAccumulationForDay(tempToUse_degC,
      params.baseTemp_degC, params.optimalTemp_degC);
  development.heatUnitIndex := EQ.HeatUnitIndex(development.cumHeatUnits, params.potHeatUnitsReqForMaturation);
  except on e: Exception do errorMessage('Exception in plant AccumulateHeatUnits: ' + e.message); end;
  end;

procedure GsPlant.CheckForDormancy;
  var
    lowestTempDay: single;
  begin
  { non-annual plants go into dormancy if the climate has at least one long-term min temp below 5 degrees C.
    dormancy has two effects:
    1. set heat units to zero on 15th of month with coldest long-term min temp
    2. set dormancy flag (stops new growth) if day length is below growth threshold }
  try
  if params.lifeCycleType = kAnnual then exit;
  lowestTempDay := soil.garden.weather.middayOfMonthWithLowestMeanMinTempForYearBelow5degC;
  { middayOfMonthWithLowestMeanMinTempForYearBelow5degC is set to 400 if no day is that cold }
  if lowestTempDay > 400 then exit;
  development.isDormant := not self.dayLengthIsAboveThresholdForGrowth;
  if soil.garden.julianDayToday = lowestTempDay then
    begin
    development.cumHeatUnits := 0.0;
    development.heatUnitIndex := 0.0;
    end;
  except on e: Exception do errorMessage('Exception in plant CheckForDormancy: ' + e.message); end;
  end;

procedure GsPlant.CheckForAnnualOrBiennialDeath;
  var
    lowestTempDay: single;
  begin
  try
  if isSeed then exit;
  if optionIsTrue(kPlantOptionsIgnoreLifeCycleLimits) then
    exit;
  { for annuals and biennials, attempt to kill on coldest day of year if it falls below 5 deg C,
    but if this is a warm climate and no day is that cold, kill off on the day it was planted.
    middayOfMonthWithLowestMeanMinTempForYearBelow5degC is set to 400 if no day is that cold. }
  lowestTempDay := soil.garden.weather.middayOfMonthWithLowestMeanMinTempForYearBelow5degC;
  if isAnnual then
    begin
    if lowestTempDay < 366 then {has coldest day - kill on that day}
      begin
      if soil.garden.julianDayToday = lowestTempDay then
        self.die;
      end
    else  {doesn't have coldest day - check if one year old}
      begin
      if development.yearsOld >= 1 then
        self.die;
      end;
    end
  else if isBiennial then
    begin
    if lowestTempDay < 366 then  {has coldest day - kill on that day if at least one year old}
      begin
      if (soil.garden.julianDayToday = lowestTempDay) and (development.yearsOld >= 1) then
        self.die;
      end
    else   {doesn't have coldest day - check if two years old}
      begin
      if development.yearsOld >= 2 then
        self.die;
      end;
    end;
  except on e: Exception do errorMessage('Exception in plant CheckForAnnualOrBiennialDeath: ' + e.message); end;
  end;

procedure GsPlant.BiomassTempStressFactor;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalTemperature) then
    constraints.biomassTempStressFactor_frn := 1.0
  else
    begin
    constraints.biomassTempStressFactor_frn := EQ.BiomassTempStressFactor_frn(
      soil.garden.weather.dailyWeather.meanTempForDay_degC,
      params.baseTemp_degC, params.optimalTemp_degC);
    end;
  except on e: Exception do errorMessage('Exception in plant BiomassTempStressFactor: ' + e.message); end;
  end;

procedure GsPlant.LeafAreaIndex;
  begin
  try
  development.leafAreaIndex := EP.LeafAreaIndex(params, development, soil.garden.weather.dailyWeather,
    constraints, self.minDayLengthForWinterGrowth_hr, optionIsTrue(kPlantOptionsIgnoreLeafSenescence));
  { change from EPIC: factor for amendment of LAI due to leaf loss by selectively harvesting leaves }
  development.leafAreaIndex := development.leafAreaIndex * constraints.leafLossIndex_frn;
  development.yesterdaysHeatUnitFactorForLAIAndHeight := development.heatUnitFactorForLAIAndHeight;
  except on e: Exception do errorMessage('Exception in plant LeafAreaIndex: ' + e.message); end;
  end;

procedure GsPlant.PlantHeight;
  begin
  try
  if (development.leafAreaIndex > 0.05) then
    if development.isSupported then
      biomass.height_m := EQ.PlantHeight_m(biomass.height_m, params.maxHeightSupported_m,
        development.heatUnitFactorForLAIAndHeight)
    else
      biomass.height_m := EQ.PlantHeight_m(biomass.height_m, params.maxHeightUnsupported_m,
        development.heatUnitFactorForLAIAndHeight)
  except on e: Exception do errorMessage('Exception in plant PlantHeight: ' + e.message); end;
  end;

procedure GsPlant.RootDepth;
  begin
  try
  biomass.rootDepth_m := EQ.RootDepth_m(biomass.rootDepth_m, params.maxRootDepth_m, development.heatUnitIndex,
    soil.layers[soil.state.numLayers - 1].depth_m, biomass.height_m);
  except on e: Exception do errorMessage('Exception in plant RootDepth: ' + e.message); end;
  end;

procedure GsPlant.RootSystemDiameter;
  begin
  try
  biomass.rootSystemDiameter_m := GS.RootSystemDiameter_m(biomass.rootSystemDiameter_m, params.maxRootSystemDiameter_m,
    development.heatUnitIndex);
  except on e: Exception do errorMessage('Exception in plant RootSystemDiameter: ' + e.message); end;
  end;

procedure GsPlant.FrostAndShortDaylengthDamage;
  var noTempDamage, noSunDamage: boolean;
  begin
  try
  noTempDamage := optionIsTrue(kPlantOptionsOptimalTemperature);
  noSunDamage := optionIsTrue(kPlantOptionsOptimalRadiation);
  if (not params.isTree) then
    EP.WinterFrostAndShortDaylengthDamage(soil.garden.weather.dailyWeather, development,
      params, nutrients, biomass, constraints, noTempDamage, noSunDamage,
      self.dayLengthIsAboveThresholdForGrowth, self.minDayLengthForWinterGrowth_hr);
  except on e: Exception do errorMessage('Exception in plant FrostAndShortDaylengthDamage: ' + e.message); end;
  end;

procedure GsPlant.FrostDeath;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalTemperature) then exit;
  if params.lifeCycleType = kAnnual then
    if (EQ.AnnualCropIsKilledByFrost(soil.water.snowWaterContent_mm, constraints,
      soil.garden.weather.dailyWeather.meanTempForDay_degC, params.absoluteTempForFrostKill_degC)) then
      self.die;
  except on e: Exception do errorMessage('Exception in plant FrostDeath: ' + e.message); end;
  end;

procedure GsPlant.Die;
  var
    layer: integer;
  begin
  if development.isDead then exit;
  try
  development.isDead := true;
  { place biomass to be lost in reduction vars so drawing plant can transfer them to dead biomass }
  biomass.reductionFromAboveGroundVegetativeDeath_tPha := biomass.totalShootWeight_tPha;
  biomass.reductionFromReproductiveDeath_tPha := biomass.flowersAndFruits_tPha;
  { place all living above-ground biomass into standing dead, and all root biomass into soil }
  addQuantity(biomass.standingDead_tPha, biomass.standingLive_tPha);
  biomass.standingLive_tPha := 0.0;
  { standing live includes shoots, fruits, and storage organ }
  biomass.totalShootWeight_tPha := 0.0;
  biomass.flowersAndFruits_tPha := 0.0;
  biomass.storageOrgan_tPha := 0.0;
  { add N and P accumulated by plant into standing dead N and P }
  addQuantity(nutrients.nInStandingDead_kgPha, nConcInLiveBiomass_kgPt * biomass.standingLive_tPha);
  addQuantity(nutrients.pInStandingDead_kgPha, pConcInLiveBiomass_kgPt * biomass.standingLive_tPha);
  { add root weights directly to soil residue in layers }
  { if user pulls up dead plant (and organic matter blob is created), roots will stay in soil }
  { drawing plant doesn't know about root weight }
  if biomass.numLayersWithRootsInThem > 0 then
    for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
      begin
      addQuantity(soil.layers[layer].flatCropResidue_tPha,  biomass.rootWeightByLayer_tPha[layer]);
      addQuantity(soil.layers[layer].organicNFresh_kgPha, biomass.rootWeightByLayer_tPha[layer] * nConcInLiveBiomass_kgPt);
      addQuantity(soil.layers[layer].organicPFresh_kgPha, biomass.rootWeightByLayer_tPha[layer] * pConcInLiveBiomass_kgPt);
      biomass.rootWeightByLayer_tPha[layer] := 0.0;
      end;
  biomass.totalRootWeight_tPha := 0.0;
  biomass.totalLive_tPha := 0.0;
  { don't have to tell drawing plant to die, because it will get called in the last part of the
    nextDay function (even when the plant is dead), so it will happen today }
  except
    on e: Exception do errorMessage('Exception in GsPlant.Die');
  end;
  end;

{ if the weather is warm and the plant is not dormant, do full functions that follow }
{ if the weather is warm and the plant is dormant, decrease biomass, no functions }
procedure GsPlant.RootGrowthConstraintByLayer;
  var
    layer: integer;
    soilTempOverOptimalTemp: single;
  begin
  try
  if soil.state.numLayers > 0 then for layer := 0 to soil.state.numLayers - 1 do
    begin
    soilTempOverOptimalTemp := safedivExcept(soil.layers[layer].temperature_degC, params.optimalTemp_degC, 0.0);
    { if ground is frozen, root growth constraint is zero }
    if (soilTempOverOptimalTemp <= 0.0) then
      constraints.rootGrowthConstraintByLayer_frn[layer] := 0.0
    else
      begin
      { temperature stress factor }
      if optionIsTrue(kPlantOptionsOptimalTemperature) then
         constraints.rootTempStressFactorByLayer_frn[layer] := 1.0
      else
        constraints.rootTempStressFactorByLayer_frn[layer] := EQ.RootTempStressFactorForLayer_frn(
            soil.layers[layer].temperature_degC, params.optimalTemp_degC);
      { aluminum stress factor }
      if optionIsTrue(kPlantOptionsIgnoreAluminumToxicity) then
        constraints.rootAlStressFactorByLayer_frn[layer] := 1.0
      else
        constraints.rootAlStressFactorByLayer_frn[layer] := EQ.AluminumToxicityStressFactorForLayer_frn(
          soil.layers[layer].aluminumSaturation_pct, params.aluminumTolerance_pct);
      { soil strength stress factor }
      if optionIsTrue(kPlantOptionsOptimalSoilStrengthForRootGrowth) then
        constraints.rootSoilStrengthFactorByLayer_frn[layer] := 1.0
      else
        constraints.rootSoilStrengthFactorByLayer_frn[layer] := EP.SoilStrengthRootGrowthConstraint(layer, soil.layers,
           params.rootGrowthRestrictionByRockCoeffs, params.rootGrowthRestrictionInSandySoilParam);
      end;
    { result is minimum of these three factors }
    constraints.rootGrowthConstraintByLayer_frn[layer] := max(0.0, min(1.0,
      min(constraints.rootAlStressFactorByLayer_frn[layer],
      min(constraints.rootSoilStrengthFactorByLayer_frn[layer],
      constraints.rootTempStressFactorByLayer_frn[layer]))));
    end;
  except on e: Exception do errorMessage('Exception in plant RootGrowthConstraintByLayer: ' + e.message); end;
  end;

procedure GsPlant.WaterDemand_mm;
  begin
  try
  { called by soil allocation function }
  water.totalWaterDemand_mm := EP.PlantWaterDemand_mm(water, constraints, soil.state.numLayers, soil.layers,
    soil.params.lowerLimitWaterContentInTopP5MAsFractOfWP_frn,
    soil.state.soilProfileDepth_m, biomass.rootDepth_m);
  except on e: Exception do errorMessage('Exception in plant WaterDemand_mm: ' + e.message); end;
  end;

procedure GsPlant.SetWaterUseAtOptimalIfOptionOn;
  var layer: smallint;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalWaterUptake) then
    begin
    for layer := 0 to soil.state.numLayers - 1 do
      water.waterUseByLayer_mm[layer] := water.waterDemandByLayer_mm[layer];
    water.totalWaterUse_mm := water.totalWaterDemand_mm;
    end;
  except on e: Exception do errorMessage('Exception in plant SetWaterUseAtOptimal: ' + e.message); end;
  end;

procedure GsPlant.CalcNumLayersWithRootsInThem;
  var layer: integer;
  begin
  try
  biomass.numLayersWithRootsInThem := 0;
  if soil.state.numLayers > 0 then for layer := 0 to soil.state.numLayers - 1 do
      if (biomass.rootDepth_m <= soil.layers[layer].depth_m) then
        biomass.numLayersWithRootsInThem := layer + 1;
  except on e: Exception do errorMessage('Exception in plant CalcNumLayersWithRootsInThem: ' + e.message); end;
  end;

procedure GsPlant.AerationStress;
  begin
  try
  if optionIsTrue(kPlantOptionsIgnoreAerationStress) then
    constraints.aerationStressFactor_frn := 1.0
  else
    constraints.aerationStressFactor_frn := EQ.AerationStressFactor_frn(soil.totals.top1MWaterContent_mm,
      soil.totals.top1mPorosity_mm, params.criticalAerationFactor_frn, params.rootGrowthRestrictionByAerationStress);
  except on e: Exception do errorMessage('Exception in plant AerationStress: ' + e.message); end;
  end;

procedure GsPlant.PotentialBiomassIncrease;
  var
    radiationToUse_MJPm2, shadeMultiplier_frn: single;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalRadiation) then
    radiationToUse_MJPm2 := soil.garden.weather.stationVars.maxPossibleRadiationForYear_MJPm2
  else
    begin
    radiationToUse_MJPm2 := soil.garden.weather.dailyWeather.radiationForDay_MJPm2;
    { take soil patch shade level into account }
    shadeMultiplier_frn := max(0.0, min(1.0, (100.0 - soil.params.shadeIndex_pct) / 100.0));
    radiationToUse_MJPm2 := radiationToUse_MJPm2 * shadeMultiplier_frn;
    end;
  biomass.photoActiveRadiation_MJPm2 := EQ.PhotoActiveRadiation_MJPm2(radiationToUse_MJPm2, development.leafAreaIndex);
  biomass.potentialIncrease_tPha := EQ.PotentialIncreaseInBiomass_tPha(biomass.photoActiveRadiation_MJPm2,
    soil.params.carbonDioxideInAtmosphere_ppm, params.biomassToEnergyRatioCO2Params,
    params.biomassToEnergyRatioVPDParam, soil.garden.weather.dailyWeather.vaporPressureDeficit_kPa);
  except on e: Exception do errorMessage('Exception in plant PotentialBiomassIncrease: ' + e.message); end;
  end;

function GsPlant.dayLengthIsAboveThresholdForGrowth: boolean;
  begin
	result := soil.garden.weather.dailyWeather.dayLength_hr >= self.minDayLengthForWinterGrowth_hr;
  result := result or optionIsTrue(kPlantOptionsOptimalRadiation);
  end;

function GsPlant.minDayLengthForWinterGrowth_hr: single;
  begin
  result := soil.garden.weather.stationVars.minDayLengthForYear_hr
      + params.hoursAboveMinDayLengthWhenWinterGrowthCanOccur_hr;
  result := min(24.0, max(0.0, result));
  end;

procedure GsPlant.NDemandAndFixation;
  begin
  try
  { called by soil allocation function }
  nutrients.nDemandForOptimalGrowth_kgPha := EP.NDemandForOptimalGrowth_kgPha(params, development.heatUnitIndex,
    biomass.totalLive_tPha, nutrients, biomass.potentialIncrease_tPha);
  nutrients.nSupply_kgPha := EP.NSupplyForThisPlant(soil.layers, biomass.numLayersWithRootsInThem, nutrients, water);
  nutrients.nDemand_kgPha := EQ.NDemandByLayer_kgPha(nutrients, biomass, soil.layers);
  if (params.isLegume) then
    begin
    nutrients.nFixation_kgPha := EP.NFixation(soil.state.numLayers, soil.layers, biomass.rootDepth_m,
      development.heatUnitIndex, soil.totals.top30cmFractionFieldCapacity_frn, nutrients.nDemand_kgPha,
      params.nFixationParam_frn);
    nutrients.nDemand_kgPha := nutrients.nDemand_kgPha - nutrients.nFixation_kgPha;
    end;
  except on e: Exception do errorMessage('Exception in plant NDemandAndFixation: ' + e.message); end;
  end;

procedure GsPlant.FinishNUptake;
  begin
  try
  { called by soil allocation function }
  SetNitrogenUseAtOptimalIfOptionOn; { must do this at end so soil patch doesn't know }
  nutrients.nonFixationNUptakeOverDemand_frn := safedivExcept(nutrients.nUptake_kgPha,
    nutrients.nDemandForOptimalGrowth_kgPha, 1.0);
  addQuantity(nutrients.nUptake_kgPha, nutrients.nFixation_kgPha);
  addQuantity(nutrients.nInLiveBiomass_kgPha, nutrients.nUptake_kgPha);
  except on e: Exception do errorMessage('Exception in plant FinishNUptake: ' + e.message); end;
  end;

procedure GsPlant.SetNitrogenUseAtOptimalIfOptionOn;
  var layer: integer;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalNitrogenUptake) then
    begin
    nutrients.nUptake_kgPha := 0.0;
    if biomass.numLayersWithRootsInThem > 0 then
    for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
      begin
      nutrients.nUptakeByLayer_kgPha[layer] := nutrients.nDemandForOptimalGrowth_kgPha
        * safedivExcept(biomass.rootWeightByLayer_tPha[layer], biomass.totalRootWeight_tPha, 0.0);
      nutrients.nUptake_kgPha := nutrients.nUptake_kgPha + nutrients.nUptakeByLayer_kgPha[layer];
      end;
    end;
  except on e: Exception do errorMessage('Exception in plant SetNitrogenUseAtOptimalIfOptionOn: ' + e.message); end;
  end;

procedure GsPlant.PDemand;
  var
    layer: smallint;
  begin
  try
  { called by soil allocation function }
  nutrients.pDemandForOptimalGrowth_kgPha := EP.PDemandForOptimalGrowth_kgPha(params, development.heatUnitIndex,
    biomass.totalLive_tPha, nutrients, biomass.potentialIncrease_tPha);
  nutrients.pSupply_kgPha := EP.PSupplyForThisPlant(soil.layers, params.pUptakeCoeffs,
    nutrients.pDemandForOptimalGrowth_kgPha, nutrients, biomass);
  if (nutrients.pDemandForOptimalGrowth_kgPha > nutrients.pSupply_kgPha) then
    nutrients.pDemand_kgPha := EQ.PDemandByLayer_kgPha(nutrients, biomass, soil.layers)
  else
    begin
    if biomass.numLayersWithRootsInThem > 0 then
      for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
        nutrients.pDemandByLayer_kgPha[layer] := nutrients.pSupplyByLayer_kgPha[layer];
    nutrients.pDemand_kgPha := nutrients.pSupply_kgPha;
    end;
  except on e: Exception do errorMessage('Exception in plant PDemand: ' + e.message); end;
  end;

procedure GsPlant.FinishPUptake;
  begin
  try
  { called by soil allocation function }
  SetPhosphorusUseAtOptimalIfOptionOn; { must do this at end so soil patch doesn't know }
  { cfk changed this ratio to use demand for optimal growth, which is what EPIC uses - my mistake }
  nutrients.pUptakeOverDemand := safedivExcept(nutrients.pUptake_kgPha,
    nutrients.pDemandForOptimalGrowth_kgPha, 1.0);
  addQuantity(nutrients.pInLiveBiomass_kgPha, nutrients.pUptake_kgPha);
  except on e: Exception do errorMessage('Exception in plant FinishPUptake: ' + e.message); end;
  end;

procedure GsPlant.SetPhosphorusUseAtOptimalIfOptionOn;
  var layer: integer;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalPhosphorusUptake) then
    begin
    nutrients.pUptake_kgPha := 0.0;
    if biomass.numLayersWithRootsInThem > 0 then
    for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
      begin
      nutrients.pUptakeByLayer_kgPha[layer] := nutrients.pDemandForOptimalGrowth_kgPha
        * safedivExcept(biomass.rootWeightByLayer_tPha[layer], biomass.totalRootWeight_tPha, 0.0);
      nutrients.pUptake_kgPha := nutrients.pUptake_kgPha + nutrients.pUptakeByLayer_kgPha[layer];
      end;
    end;
  except on e: Exception do errorMessage('Exception in plant SetPhosphorusUseAtOptimalIfOptionOn: ' + e.message); end;
  end;

procedure GsPlant.CombinedBiomassConstraint;
  begin
  try
  if optionIsTrue(kPlantOptionsOptimalNitrogenUptake) then
    constraints.nStressFactor_frn := 1.0
  else
    begin
    constraints.nStressFactor_frn := EP.NOrPStressFactor(nutrients.nInLiveBiomass_kgPha, nutrients.optimalNConc_kgPkg,
      biomass.totalLive_tPha + biomass.potentialIncrease_tPha,
      nutrients.nonFixationNUptakeOverDemand_frn, params.plantNAndPStressCoeffs);
    constraints.nStressFactor_frn := min(1.0, constraints.nStressFactor_frn);
    end;
  if optionIsTrue(kPlantOptionsOptimalPhosphorusUptake) then
    constraints.pStressFactor_frn := 1.0
  else
    begin
    constraints.pStressFactor_frn := EP.NOrPStressFactor(nutrients.pInLiveBiomass_kgPha, nutrients.optimalPConc_kgPkg,
      biomass.totalLive_tPha + biomass.potentialIncrease_tPha,
      nutrients.pUptakeOverDemand, params.plantNAndPStressCoeffs);
    constraints.pStressFactor_frn := min(1.0, constraints.pStressFactor_frn);
    end;
  if optionIsTrue(kPlantOptionsOptimalWaterUptake) then
    constraints.waterStressFactor_frn := 1.0
  else
    begin
    constraints.waterStressFactor_frn := EQ.WaterStressFactor_frn(water.totalWaterUse_mm, water.potPlantEvap_mm);
    constraints.waterStressFactor_frn := min(1.0, constraints.waterStressFactor_frn);
    end;
  constraints.biomassGrowthConstraint_frn := EQ.BiomassGrowthConstraintFactor_frn(constraints);
  except on e: Exception do errorMessage('Exception in plant CombinedBiomassConstraint: ' + e.message); end;
  end;

procedure GsPlant.AccumulateWaterUse;
  begin
  try
  addQuantity(water.cumWaterUse_mm, water.totalWaterUse_mm);
  addQuantity(water.cumPotPlantEvap_mm, water.potPlantEvap_mm);
  except on e: Exception do errorMessage('Exception in plant AccumulateWaterUse: ' + e.message); end;
  end;

procedure GsPlant.IncreaseBiomassWithStress;
  begin
  try
  EP.IncreaseBiomassWithStress(biomass, constraints, self.dayLengthIsAboveThresholdForGrowth);
  except on e: Exception do errorMessage('Exception in plant IncreaseBiomassWithStress: ' + e.message); end;
  end;

procedure GsPlant.AllocateNewBiomassAmongComponents;
  begin
  try
  GS.AllocateNewBiomassAmongComponents(biomass, params, development, water);
  except on e: Exception do errorMessage('Exception in plant AllocateNewBiomassAmongComponents: ' + e.message); end;
  end;

procedure GsPlant.ReproductiveBiomassDecay;
  begin
  try
  if optionIsTrue(kPlantOptionsIgnoreReproductiveDecay) then
    biomass.reductionFromReproductiveDecay_tPha := 0.0
  else
    GS.ReproductiveBiomassDecay(biomass, params, development);
  except on e: Exception do errorMessage('Exception in plant ReproductiveBiomassDecay: ' + e.message); end;
  end;

procedure GsPlant.FallingLeavesIfTree(theMonth: integer);
  begin
  try
  if (params.isTree) then
    EP.DecreaseBiomassFromFallingLeaves(theMonth, biomass, params, soil.layers, nutrients);
  except on e: Exception do errorMessage('Exception in plant FallingLeavesIfTree: ' + e.message); end;
  end;

procedure GsPlant.DecreaseBiomassFromDryness;
  begin
  try
  { change from EPIC }
  { EPIC only does this to perennials, but we want annual plants to die off }
  EP.DecreaseBiomassFromDryness(biomass, params, constraints, development, nutrients);
  except on e: Exception do errorMessage('Exception in plant DecreaseBiomassFromDryness: ' + e.message); end;
  end;

procedure GsPlant.ZeroHeatUnitsIfOldAndSmall;
  begin
  try
  { if standing live biomass of old plant gets reduced to below 0.1, zero heat units }
  { change from EPIC - we are no longer doing this }
  {if ((development.heatUnitIndex > 0.6) and (biomass.standingLive_tPha < 0.1)) then
    development.cumHeatUnits := 0.0;}
  except on e: Exception do errorMessage('Exception in plant ZeroHeatUnitsIfOldAndSmall: ' + e.message); end;
  end;

procedure GsPlant.LifeHistoryStage;
  var
    ignoreTemp, ignoreDayLength, waitingForNextYear: boolean;
  begin
  try
  with params do
  begin
  case development.lifeHistoryStage of
    kVegetativePeriod:
      begin
      { end vegetative period, but not if waiting for obligate vernalization }
      { need to check if it is next year, but use only half year to allow for fall-planted crops }
      waitingForNextYear := (vernalizationRequirement = kObligateVernalization) and (development.age < 365 div 2);
      if (development.heatUnitIndex > heatUnitIndexAtEndOfVegetativePhase) and (not waitingForNextYear) then
        development.lifeHistoryStage := kFloralInductionPeriod;
      if (vernalizationRequirement <> kNoVernalization) then
        begin
        if (vernalizationRequirement = kObligateVernalization) then
          begin
          { don't check obligate vernalization until half year has gone by }
          if (development.age >= 365 div 2) then
            if (development.cumVernalizationUnits <= thermalUnitsRequiredForVernalization) then
              development.lifeHistoryStage := kFailedVernalization;
          end
        else { vernalization is quantitative }
          begin
          if (development.cumVernalizationUnits <> 0.0) then
            development.photothermalUnitsRequiredForFloralInductionAfterVernalization :=
            		photothermalUnitsRequiredForFloralInduction
               * safediv(thermalUnitsRequiredForVernalization, development.cumVernalizationUnits)
          else
            development.lifeHistoryStage := kFailedVernalization;
          end;
        end;
      end;
    kFloralInductionPeriod:
      begin
      ignoreTemp := optionIsTrue(kPlantOptionsOptimalTemperature);
      ignoreDayLength := optionIsTrue(kPlantOptionsIgnorePhotoperiod);
      development.cumFloralInductionUnits := development.cumFloralInductionUnits +
        GS.FloralInductionUnitsForDay(
          minTempForFloralInduction_degC, optTempForFloralInduction_degC, maxTempForFloralInduction_degC,
          soil.garden.weather.dailyWeather.meanTempForDay_degC,
          floralInductionParams.c1, floralInductionParams.c2,
          soil.garden.weather.dailyWeather.dayLength_hr, ignoreTemp, ignoreDayLength);
      if (development.cumFloralInductionUnits >=
      		development.photothermalUnitsRequiredForFloralInductionAfterVernalization) then
        development.lifeHistoryStage := kFloralInitiationPeriod;
      end;
    kFloralInitiationPeriod:
      begin
      ignoreTemp := optionIsTrue(kPlantOptionsOptimalTemperature);
      { note using same temps for floral initiation as for induction }
      development.cumFloralInitiationUnits := development.cumFloralInitiationUnits +
        GS.FloralInitiationUnitsForDay(
          minTempForFloralInduction_degC, optTempForFloralInduction_degC, maxTempForFloralInduction_degC,
          soil.garden.weather.dailyWeather.meanTempForDay_degC, ignoreTemp);
      if (development.cumFloralInitiationUnits >= thermalUnitsRequiredForFloralInitiation) then
        begin
        development.lifeHistoryStage := kReproductiveAllocationPeriod;
        development.heatUnitIndexWhenReproductiveAllocationStarted := development.heatUnitIndex;
        end;
      end;
    end;
  end;
  except on e: Exception do errorMessage('Exception in plant LifeHistoryStage: ' + e.message); end;
  end;

procedure GsPlant.ZeroPlantCumulativeVars;
  var
    layer: integer;
  begin
  development.cumHeatUnits := 0.0;
  development.isDead := false;
  development.isDormant := false;
  development.age := 0;
  development.yearsOld := 0;
  development.dayPlanted := 0; 
  development.isSupported := false;
  development.cumFloralInductionUnits := 0.0;
  development.cumVernalizationUnits := 0.0;
  development.cumFloralInitiationUnits := 0.0;
  development.heatUnitIndexWhenReproductiveAllocationStarted := 0.0;
  biomass.height_m := 0.0;
  biomass.rootDepth_m := 0.0;
  biomass.numLayersWithRootsInThem := 0;
  water.cumPotPlantEvap_mm := 0.0;
  water.cumWaterUse_mm := 0.0;
  nutrients.nInLiveBiomass_kgPha := 0.0;
  nutrients.pInLiveBiomass_kgPha := 0.0;
  development.cumHeatUnits := 0.0;
  development.heatUnitIndex := 0.0;
  development.yesterdaysHeatUnitFactorForLAIAndHeight := 0.0;
  development.leafAreaIndex := 0.0;
  development.leafAreaIndexAtStartOfDecline := 0.0;
  biomass.totalLive_tPha := 0.0;
  biomass.totalShootWeight_tPha := 0.0;
  biomass.totalRootWeight_tPha := 0.0;
  for layer := 0 to kMaxPossibleLayers - 1 do biomass.rootWeightByLayer_tPha[layer] := 0.0;
  biomass.standingLive_tPha := 0.0;
  biomass.standingDead_tPha := 0.0;
  nutrients.nInStandingDead_kgPha := 0.0;
  nutrients.pInStandingDead_kgPha := 0.0;
  biomass.flowersAndFruits_tPha := 0.0;
  biomass.storageOrgan_tPha := 0.0;
  biomass.height_m := 0.0;
  biomass.diameter_m := 0.0;
  biomass.rootDepth_m := 0.0;
  biomass.rootSystemDiameter_m := 0.0;
  biomass.numLayersWithRootsInThem := 0;
  constraints.biomassGrowthConstraint_frn := 0.0;
  biomass.numLayersWithRootsInThem := 0;
  development.heatUnitIndexWhenReproductiveAllocationStarted := 0.0;
  end;

{ ------------------------------------------------------------------------------------------ utilities }
procedure GsPlant.adjustForNewSoilPatchArea(oldArea_ha, newArea_ha: single);
  var
    i: smallint;
    conversion: single;
  begin
  try
  if oldArea_ha = newArea_ha then exit;
  { convert to absolute by multiplying by old area, then convert to relative by dividing by new area }
  conversion := safedivExcept(oldArea_ha, newArea_ha, 0.0);
  with biomass do
    begin
    totalLive_tPha := totalLive_tPha * conversion;
    for i := 0 to numLayersWithRootsInThem - 1 do
      rootWeightByLayer_tPha[i] := rootWeightByLayer_tPha[i] * conversion;
    totalRootWeight_tPha := totalRootWeight_tPha * conversion;
    totalShootWeight_tPha := totalShootWeight_tPha * conversion;
    flowersAndFruits_tPha := flowersAndFruits_tPha * conversion;
    storageOrgan_tPha := storageOrgan_tPha * conversion;
    standingLive_tPha := standingLive_tPha * conversion;
    standingDead_tPha := standingDead_tPha * conversion;
    end;
  with nutrients do
    begin
    nInLiveBiomass_kgPha := nInLiveBiomass_kgPha * conversion;
    pInLiveBiomass_kgPha := pInLiveBiomass_kgPha * conversion;
    nInStandingDead_kgPha := nInStandingDead_kgPha * conversion;
    pInStandingDead_kgPha := pInStandingDead_kgPha * conversion;
    end;
  GS.RecalculatePlantBiomassRelationships(biomass);
  except on e: Exception do errorMessage('Exception in plant adjustForNewSoilPatchArea: ' + e.message); end;
  end;

function GsPlant.nConcInLiveBiomass_kgPt: single;
  begin
  result := 0.0;
  if (biomass.totalLive_tPha > 0.0) then
    result := safedivExcept(nutrients.nInLiveBiomass_kgPha, biomass.totalLive_tPha, 0.0);
  end;

function GsPlant.pConcInLiveBiomass_kgPt: single;
  begin
  result := 0.0;
  if (biomass.totalLive_tPha > 0.0) then
    result := safedivExcept(nutrients.pInLiveBiomass_kgPha, biomass.totalLive_tPha, 0.0);
  end;

function GsPlant.isSeed: boolean;
  begin
  result := (development.lifeHistoryStage = kSeed);
  end;

function GsPlant.isNotSeed: boolean;
  begin
  result := (development.lifeHistoryStage <> kSeed);
  end;

function GsPlant.isAnnual: boolean;
  begin
  result := (params.lifeCycleType = kAnnual);
  end;

function GsPlant.isBiennial: boolean;
  begin
  result := (params.lifeCycleType = kBiennial);
  end;

function GsPlant.awaitingReseedingOrInStasisBox: boolean;
  begin
  result := (self.awaitingReseeding) or (self.isInStasisBox);
  end;

function GsPlant.optionIsTrue(index: smallint): boolean;
  begin
  if (index < 0) or (index > kPlantOptionsLastOption) then
    raise Exception.create('GsPlant.optionIsTrue: index out of bounds');
  if self.isGrowingOptimally then
    result := true
  else if (soil = nil) or (soil.garden = nil) then
    result := self.options[index]
  else if soil.garden.plantOverrides[index] then
    result := soil.garden.plantOptions[index]
  else
    result := self.options[index];
  end;

{ ------------------------------------------------------------------------------------------- planting/tool actions }
procedure GsPlant.plantFromCultivar(cultivar: GsPlant);
  begin
  copyFromCultivar(cultivar, kStreamParams);
  isTemplate := false;
  randomizeFromTime;
  development.dayPlanted := domain.garden.julianDayToday;
  if development.dayPlanted = 365 then development.dayPlanted := 364;
  calculatePotentialHeatUnitsAtPlanting;
  grow(params.daysToGrowOptimallyAfterPlanting_days);
  end;

procedure GsPlant.becomeSeedNoReturns(startGrowingNow: boolean);
  var
    dummy: BiomassLostInReseedingStructure;
  begin
  { don't care about last argument in this case, so fake it out }
  self.becomeSeed(startGrowingNow, dummy);
  end;

procedure GsPlant.becomeSeed(startGrowingNow: boolean; var lostBiomass: BiomassLostInReseedingStructure);
  var
    i: integer;
    oldStandingLive_tPha, oldStandingDead_tPha: single;
    oldRootBiomassByLayer_tPha: arraySoilLayers;
    cultivar: GsPlant;
    saveName: string;
    saveInitialRotationAngle: single;
    saveSwaySeed: longint;
  begin
  oldStandingLive_tPha := 0.0;
  oldStandingDead_tPha := 0.0;
  try
  if not isTemplate then
    begin
    { set up biomass for OM blob }
    oldStandingLive_tPha := biomass.standingLive_tPha;
    oldStandingDead_tPha := biomass.standingDead_tPha;
    for i := 0 to kMaxPossibleLayers - 1 do oldRootBiomassByLayer_tPha[i] := 0.0;
    for i := 0 to biomass.numLayersWithRootsInThem - 1 do
      oldRootBiomassByLayer_tPha[i] := biomass.rootWeightByLayer_tPha[i];
    with lostBiomass do
      begin
      nConcInLiveBiomass_kgPt := safedivExcept(nutrients.nInLiveBiomass_kgPha, biomass.totalLive_tPha, 0.0);
      pConcInLiveBiomass_kgPt := safedivExcept(nutrients.pInLiveBiomass_kgPha, biomass.totalLive_tPha, 0.0);
      nConcInStandingDead_kgPt := safedivExcept(nutrients.nInStandingDead_kgPha, biomass.standingDead_tPha, 0.0);
      pConcInStandingDead_kgPt := safedivExcept(nutrients.pInStandingDead_kgPha, biomass.standingDead_tPha, 0.0);
      end;
    end;
  { reseed drawing plant }
  drawingPlant.becomeSeed;
  ZeroPlantCumulativeVars;
  if not isTemplate then
    begin
    { try to find template pointer based on name, to copy info for template if it is a seedling }
    cultivar := Domain.templateManager.findCultivarBasedOnName(self.cultivarName);
    if (cultivar <> nil) and (cultivar.development.cumHeatUnits > 0.0) then
      begin
      saveName := self.getName;
      saveInitialRotationAngle := drawingPlant.initialRotationAngle;
      saveSwaySeed := drawingPlant.swayRandomNumberGenerator.seed;
      self.copyFromCultivar(cultivar, kDontStreamParams);
      drawingPlant.initialRotationAngle := saveInitialRotationAngle;
      drawingPlant.swayRandomNumberGenerator.seed := saveSwaySeed;
      self.setName(saveName);
      self.isTemplate := false;
      end
    else
      self.SetDefaultsForSeed;
    end
  else
    self.SetDefaultsForSeed;
  { set up some things }
  awaitingReseeding := not startGrowingNow;
  if not awaitingReseeding then
    development.dayPlanted := domain.garden.julianDayToday;
  if not isTemplate then
    calculatePotentialHeatUnitsAtPlanting; 
  ZeroPlantDailyVars;
  SetPlantDailyDefaults;
  computeBoundsRect;
  constraints.biomassGrowthConstraint_frn := 1.0;
  if not isTemplate then
    begin
    { fill in lost biomass }
    with lostBiomass do
      begin
      standingLive_tPha := max(0.0, oldStandingLive_tPha - biomass.standingLive_tPha);
      standingDead_tPha := max(0.0, oldStandingDead_tPha - biomass.standingDead_tPha);
      for i := 0 to kMaxPossibleLayers - 1 do
        rootWeightByLayer_tPha[i] := max(0.0, oldRootBiomassByLayer_tPha[i] - biomass.rootWeightByLayer_tPha[i]);
      end;
    { if has a quick-growth parameter, do now }
    grow(params.daysToGrowOptimallyAfterPlanting_days);
    end;
  except
    on e: Exception do errorMessage('Exception in plant becomeSeed');
  end;
  end;

procedure GsPlant.calculatePotentialHeatUnitsAtPlanting;
  begin
  { calculate new PHUs for this climate, and adjust accumulated heat units to keep same HUI if not a seed }
  self.calculatePotentialHeatUnits(kBeingPlantedNow);
  if development.heatUnitIndex > 0.0 then
    development.cumHeatUnits := development.heatUnitIndex * params.potHeatUnitsReqForMaturation;
  end;

procedure GsPlant.calculatePotentialHeatUnits(beingPlantedNow: boolean);
  var
    dayOfLastSpringFrost, dayOfFirstFallFrost, startDay: smallint;
    dayToConsiderForPlanting, distanceFromSpringFrost, distanceFromFallFrost: smallint;
    useSpring: boolean;
  begin
  dayOfLastSpringFrost := soil.garden.weather.stationVars.julianDayOfLastSpringFrost;
  dayOfFirstFallFrost := soil.garden.weather.stationVars.julianDayOfFirstFallFrost;
  if beingPlantedNow then
    dayToConsiderForPlanting := domain.garden.julianDayToday
  else
    dayToConsiderForPlanting := development.dayPlanted;
  case params.plantingSpringFallOrBoth of
    kCultivarIsPlantedInSpring: useSpring := true;
    kCultivarIsPlantedInFall: useSpring := false;
    kCultivarCanBePlantedSpringOrFall:
      begin
      if dayOfLastSpringFrost < 0 then
        distanceFromSpringFrost := 365 {to make use fall}
      else
        distanceFromSpringFrost := dayToConsiderForPlanting - dayOfLastSpringFrost;
      if dayOfFirstFallFrost < 0 then
        distanceFromFallFrost := 365
      else
        distanceFromFallFrost := dayOfFirstFallFrost - dayToConsiderForPlanting;
      useSpring := (distanceFromSpringFrost < distanceFromFallFrost);
      end;
    else
      raise Exception.create('GsPlant.calculatePotentialHeatUnits: bad planting index');
    end;
  if useSpring then
    begin
    if dayOfLastSpringFrost >= 0 then
      startDay := dayOfLastSpringFrost + params.plantingDaysAfterLastSpringFrost_days
    else
      startDay := dayToConsiderForPlanting
    end
  else
    begin
    if dayOfFirstFallFrost >= 0 then
      startDay := dayOfFirstFallFrost - params.maturityDaysBeforeFirstFallFrost_days
          - params.plantingDaysFromGerminatedSeedToSeasonMaturity_days
          - params.plantingDaysFromSeedToGerminatedSeed_days
    else
      startDay := dayToConsiderForPlanting
    end;
  if startDay < 0 then
    startDay := startDay + 365;
  startDay := startDay mod 364;
  { germination }
  params.minHeatUnitsBeforeGermination_degC := soil.garden.weather.AccumulateHeatUnitsForNumberOfDays(
      startDay,
      params.plantingDaysFromSeedToGerminatedSeed_days, params.baseTemp_degC, params.optimalTemp_degC,
      self.minDayLengthForWinterGrowth_hr);
  { bound at parameters in case climate is way off }
  params.minHeatUnitsBeforeGermination_degC := max(params.minHeatUnitsBeforeGermination_degC,
      params.minPossibleHeatUnitsBeforeGerminationInAnyClimate_degC);
  params.minHeatUnitsBeforeGermination_degC := min(params.minHeatUnitsBeforeGermination_degC,
      params.maxPossibleHeatUnitsBeforeGerminationInAnyClimate_degC);
  { maturation after germination }
  params.potHeatUnitsReqForMaturation := soil.garden.weather.AccumulateHeatUnitsForNumberOfDays(
      startDay + params.plantingDaysFromSeedToGerminatedSeed_days,
      params.plantingDaysFromGerminatedSeedToSeasonMaturity_days, params.baseTemp_degC, params.optimalTemp_degC,
      self.minDayLengthForWinterGrowth_hr);
  { fudge - unclear why this is needed - makes it closer to EPIC's data and works better }
  params.potHeatUnitsReqForMaturation := params.potHeatUnitsReqForMaturation * 2.0;
  params.potHeatUnitsReqForMaturation := max(params.potHeatUnitsReqForMaturation,
      params.minPossibleHeatUnitsBeforeMaturityInAnyClimate_degC);
  params.potHeatUnitsReqForMaturation := min(params.potHeatUnitsReqForMaturation,
      params.maxPossibleHeatUnitsBeforeMaturityInAnyClimate_degC);
  end;

procedure GsPlant.SetDefaultsForSeed;
  begin
  try
  ZeroPlantCumulativeVars;
  with development do
    begin
    lifeHistoryStage := kSeed;
    daysGerminating := 0;
    end;
  with constraints do
    begin
    biomassAdjustmentIfLAIDeclining := 1.0;
    leafLossIndex_frn := 1.0;
    biomassGrowthConstraint_frn := 1.0;
    waterStressFactor_frn := 1.0;
    end;
  with biomass do
    begin
    biomass.height_m := 0.01;
    biomass.rootDepth_m := 0.01;
    { to get starting biomass, divide seed weight in g by soil patch size in ha }
    { and convert: g/ha * 1g/1000kg * 1kg/1000t = t/ha }
    if (soil <> nil) then
      begin
      totalLive_tPha := safedivExcept(params.seedWeight_g, soil.params.area_ha, 0.0) * g_to_kg * kg_to_t;
      params.areaOfSoilPatchInWhichPlanted_ha := soil.params.area_ha;
      end
    else
      begin
      { if no soil patch, assume soil patch of 1 m2 }
      totalLive_tPha := params.seedWeight_g / m2_to_ha * g_to_kg * kg_to_t;
      params.areaOfSoilPatchInWhichPlanted_ha := 1.0 * m2_to_ha;
      end;
    { divide by two to lose parts of seed that don't contribute to live biomass (hull, etc) }
    totalLive_tPha := totalLive_tPha * 0.5;
    rootWeightByLayer_tPha[1] :=  totalLive_tPha * 0.4;
    totalRootWeight_tPha := rootWeightByLayer_tPha[1];
    numLayersWithRootsInThem := 2;
    totalShootWeight_tPha := totalLive_tPha - totalRootWeight_tPha;
    end;
  GS.recalculatePlantBiomassRelationships(biomass);
  except on e: Exception do errorMessage('Exception in plant SetDefaultsForSeed: ' + e.message); end;
  end;

procedure GsPlant.addOrRemovePlantPartBiomass(plantPart: GsDrawingPlantPart; addOrRemoveFlag: boolean);
  var
    liveBiomass_tPha, deadBiomass_tPha: single;
    nConcInDeadBiomass_kgPt, pConcInDeadBiomass_kgPt: single;
  begin
  try
  liveBiomass_tPha := safedivExcept(plantPart.liveBiomassForHarvest_kg * kg_to_t, soil.params.area_ha, 0.0);
  deadBiomass_tPha := safedivExcept(plantPart.deadBiomassForHarvest_kg * kg_to_t, soil.params.area_ha, 0.0);
  if addOrRemoveFlag = kRemovingBiomassFromPlant then
    begin
    liveBiomass_tPha := -liveBiomass_tPha;
    deadBiomass_tPha := -deadBiomass_tPha;
    end;
  case plantPart.partType of
    kPartTypeFlowerFruit, kPartTypeInflorescence:
      addQuantity(biomass.flowersAndFruits_tPha, liveBiomass_tPha);
    kPartTypeMeristem, kPartTypePhytomer, kPartTypeLeaf:
      addQuantity(biomass.totalShootWeight_tPha, liveBiomass_tPha);
    end;
  if plantPart.partType = kPartTypeLeaf then
    { change leaf loss index before you change total biomass }
    { give negative live biomass because it wants biomass Lost } 
    constraints.leafLossIndex_frn := GS.LeafLossIndexAtLeafHarvest(constraints.leafLossIndex_frn,
      -liveBiomass_tPha, biomass.totalLive_tPha);
  { add or remove N and P in live biomass }
  { we assume that the N and P ratios are unchanged when you re-add removed biomass }
  addQuantity(nutrients.nInLiveBiomass_kgPha, nConcInLiveBiomass_kgPt * liveBiomass_tPha);
  addQuantity(nutrients.pInLiveBiomass_kgPha, pConcInLiveBiomass_kgPt * liveBiomass_tPha);
  { add or remove N and P in standing dead }
  nConcInDeadBiomass_kgPt := safedivExcept(nutrients.nInStandingDead_kgPha, biomass.standingDead_tPha, 0.0);
  pConcInDeadBiomass_kgPt := safedivExcept(nutrients.pInStandingDead_kgPha, biomass.standingDead_tPha, 0.0);
  addQuantity(nutrients.nInStandingDead_kgPha, nConcInDeadBiomass_kgPt * deadBiomass_tPha);
  addQuantity(nutrients.pInStandingDead_kgPha, pConcInDeadBiomass_kgPt * deadBiomass_tPha);
  { recalculate total biomass }
  GS.RecalculatePlantBiomassRelationships(biomass);
  { add or remove standing dead }
  addQuantity(biomass.standingDead_tPha, deadBiomass_tPha);
  except on e: Exception do errorMessage('Exception in plant addOrRemovePlantPartBiomass: ' + e.message); end;
  end;

procedure GsPlant.transferPropOfStandingDeadToFlatCropResidue(proportion: single);
  begin
  biomass.reductionInStandingDeadFromTransferToResidue_tPha := biomass.standingDead_tPha * proportion;
  { drawing plant will pick up on reductionInStandingDeadFromTransferToResidue_tPha and deal with it }
  Utils_TransferPropOfMaterial(biomass.standingDead_tPha, soil.layers[0].flatCropResidue_tPha, proportion);
  Utils_TransferPropOfMaterial(nutrients.nInStandingDead_kgPha, soil.layers[0].organicNFresh_kgPha, proportion);
  Utils_TransferPropOfMaterial(nutrients.pInStandingDead_kgPha, soil.layers[0].organicPFresh_kgPha, proportion);
  if drawingPlant <> nil then
    drawingPlant.reduceStandingDeadFromTransferToResidue;
  biomass.reductionInStandingDeadFromTransferToResidue_tPha := 0.0;
  end;

const kSmallDifference = 0.000001;

procedure GsPlant.ConvertOptimalNParams;
  var
    range, halfRange, y1, y2: single;
  begin
  try
  { keep function monotonically decreasing }
  with params do
    begin
    if nFractionAtEmergence_frn <= nFractionAtMaturity_frn then
      nFractionAtEmergence_frn := nFractionAtMaturity_frn + kSmallDifference * 2;
    if nFractionAtHalfMaturity_frn >= nFractionAtEmergence_frn then
      nFractionAtHalfMaturity_frn := nFractionAtEmergence_frn - kSmallDifference;
    if nFractionAtHalfMaturity_frn <= nFractionAtMaturity_frn then
      nFractionAtHalfMaturity_frn := nFractionAtMaturity_frn + kSmallDifference;
    end;
  {  fortran code
        BN(4,JE)=BN(1,JE)
        X1=BN(1,JE)-BN(3,JE)
        BN(1,JE)=1.-(BN(2,JE)-BN(3,JE))/X1
        BN(2,JE)=1.-.00001/X1
        CALL ASCRV(BN(1,JE),BN(2,JE),.5,1.)
   }
  { calculate s curve params for optimal N conc }
  range := params.nFractionAtEmergence_frn - params.nFractionAtMaturity_frn;
  halfRange := params.nFractionAtHalfMaturity_frn - params.nFractionAtMaturity_frn;
  y1 := 1.0 - safedivExcept(halfRange, range, 0.01);
  y2 := 1.0 - safedivExcept(0.00001, range, 0.99);
  Utils_InitSCurveParam(params.optimalNParams, 0.5, y1, 1.0, y2);
  except on e: Exception do errorMessage('Exception in plant ConvertOptimalNParams: ' + e.message); end;
  end;

procedure GsPlant.ConvertOptimalPParams;
  var
    range, halfRange, y1, y2: single;
  begin
  try
  { keep function monotonically decreasing }
  with params do
    begin
    if pFractionAtEmergence_frn <= pFractionAtMaturity_frn then
      pFractionAtEmergence_frn := pFractionAtMaturity_frn + kSmallDifference * 2;
    if pFractionAtHalfMaturity_frn >= pFractionAtEmergence_frn then
      pFractionAtHalfMaturity_frn := pFractionAtEmergence_frn - kSmallDifference;
    if pFractionAtHalfMaturity_frn <= pFractionAtMaturity_frn then
      pFractionAtHalfMaturity_frn := pFractionAtMaturity_frn + kSmallDifference;
    end;
  { calculate s curve params for optimal P conc }
  range := params.pFractionAtEmergence_frn - params.pFractionAtMaturity_frn;
  halfRange := params.pFractionAtHalfMaturity_frn - params.pFractionAtMaturity_frn;
  y1 := 1.0 - safedivExcept(halfRange, range, 0.01);
  y2 := 1.0 - safedivExcept(0.00001, range, 0.99);
  Utils_InitSCurveParam(params.optimalPParams, 0.5, y1, 1.0, y2);
  except on e: Exception do errorMessage('Exception in plant ConvertOptimalPParams: ' + e.message); end;
  end;

procedure GsPlant.ConvertLeafConductanceParams;
  begin
  try
  {In EPIC, leaf conductance declines linearly as VPD increases above VPTH.
  VPD2 is a double parameter in which the number on the left of the decimal
  is some value of VPD above VPTH (e.g. 4.0), and the number of the right of
  the decimal is the corresponding fraction of the maximum leaf conductance
  at the value of VPD (e.g., 0.7).}
  { calculate vapor pressure deficit correction factor from input }
  { we are hard-coding the Y value at 0.75 to keep from having to create a linear graph in the browser for this one var }
  params.fractionOfMaxLeafConductForHighVPD.y := 0.75;
  params.fractionOfMaxLeafConductForHighVPD.c :=
    safedivExcept(1.0 - params.fractionOfMaxLeafConductForHighVPD.y,
    params.fractionOfMaxLeafConductForHighVPD.x - params.thresholdVaporPressureDeficit_kPa, 0.0);
  except on e: Exception do errorMessage('Exception in plant ConvertLeafConductanceParams: ' + e.message); end;
  end;

procedure GsPlant.grow(numDays: longint);
  var
    i: longint;
    saveStasisStatus: boolean;
  begin
  if (numDays <= 0) or (numDays > 365) then exit; { limit number of days in case of garbling }
  isGrowingOptimally := true;
  saveStasisStatus := isInStasisBox;
  try
    isInStasisBox := false;
    for i := 0 to numDays - 1 do
      begin
      self.PlantNextDayBeforeWaterAndNutrientAllocation;
      { water demand needs potPlantEvap_mm, but since it is not zeroed it will use yesterday's }
      { but if you are growing from a seed, it is not set and is zero, so in that case we will default it }
      if water.potPlantEvap_mm = 0.0 then
        water.potPlantEvap_mm := 6.0; { better than average, but this is supposed to be optimal }
      self.WaterDemand_mm;
      self.PlantNextDayBetweenWaterAndNutrientAllocation;
      self.NDemandAndFixation;
      self.PDemand;
      self.PlantNextDayAfterWaterAndNutrientAllocation;
      end;
  finally
    isGrowingOptimally := false;
    isInStasisBox := saveStasisStatus;
  end;
  end;

{ ------------------------------------------------------------------------------------------- streaming/transfer }
procedure GsPlant.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsPlant;
  cvir.versionNumber := 6;
  cvir.additionNumber := 5;
  end;

procedure GsPlant.copyFromCultivar(cultivar: GsPlant; copyParams: boolean);
  var                     
    savedSoilPatch: GsSoilPatch;
    savedPoint: TPoint;
  begin
  savedSoilPatch := self.soil;
  savedPoint := self.basePoint;
  if cultivar <> nil then
    begin
    if copyParams then
      begin
      cultivar.copyTo(self);
      self.setCultivarName(cultivar.getName);
      end
    else
      cultivar.copyToWithoutParams(self);
    end;
  self.soil := savedSoilPatch;
  { this next line is for using a template that was saved as text; some defaults will not be set } 
  { soil pointer must be valid when setDefaultsForSeed is called }
  if (cultivar = nil) or (cultivar.development.cumHeatUnits <= 0.0) then
    self.setDefaultsForSeed;
  self.basePoint := savedPoint;
  self.computeBoundsRect;
  { update this }
  Utils_InitSCurveParam(params.biomassToEnergyRatioCO2Params,
      330.0, params.biomassToEnergyRatio_kgPhaPMJ * 0.01,
      660.0, params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ * 0.01);
  { take account of changed soil patch area from that of cultivar }
  if (soil <> nil) and (cultivar <> nil) then
    self.adjustForNewSoilPatchArea(cultivar.params.areaOfSoilPatchInWhichPlanted_ha, soil.params.area_ha);
  end;

const
  kStreamDrawingPlant = true;
  kDontStreamDrawingPlant = false;

procedure GsPlant.copyToWithoutParams(newCopy: GsPlant);
  var
    memoryStream: TMemoryStream;
    filer: GsFiler;
    cvir: GsClassAndVersionInformationRecord;
  begin
  if self.classType <> newCopy.classType then
    raise exception.create('copyTo can only be used with objects of identical class types');
  self.classAndVersionInformation(cvir);
  memoryStream := TMemoryStream.create;
  filer := GsFiler.createWithStream(memoryStream);
  filer.setToWritingMode;
  self.streamDataWithoutParamsWithFiler(filer, cvir, kStreamDrawingPlant);
  memoryStream.seek(0,0); {move to start of stream}
  filer.setToReadingMode;
  newCopy.streamDataWithoutParamsWithFiler(filer, cvir, kStreamDrawingPlant);
  filer.free;
  memoryStream.free;
  end;

procedure GsPlant.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  {don't stream soil pointer}
  { the reason not to stream the drawing plant in two pieces when streaming to and from files is
    that if you do that you cannot have a version on the drawing plant }
  self.streamDataWithoutParamsWithFiler(filer, cvir, kDontStreamDrawingPlant);
  self.streamParamsWithFiler(filer, cvir, kDontStreamDrawingPlant);
  drawingPlant.streamUsingFiler(filer);
  randomNumberGenerator.streamUsingFiler(filer);
  end;

procedure GsPlant.streamDataWithoutParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord;
    streamDrawingPlant: boolean);
	begin
  {model structures}
  filer.streamBytes(development, sizeOf(development));
  filer.streamBytes(water, sizeOf(water));
  filer.streamBytes(nutrients, sizeOf(nutrients));
  filer.streamBytes(biomass, sizeOf(biomass));
  if (filer.isReading) and (cvir.additionNumber < 3) then
    begin
    { moved n and p in standing dead from biomass to nutrients structure }
    nutrients.nInStandingDead_kgPha := biomass.unused1;
    nutrients.pInStandingDead_kgPha := biomass.unused2;
    end;
  filer.streamBytes(constraints, sizeOf(constraints));
  {graphical stuff}
  filer.streamPoint(basePoint);
  filer.streamSmallint(possibleSize);
  filer.streamBoolean(awaitingReseeding);
  if cvir.additionNumber >= 4 then
    filer.streamBoolean(isInStasisBox);
  if (filer.isReading) and (cvir.additionNumber < 5) then
  		development.photothermalUnitsRequiredForFloralInductionAfterVernalization :=
          params.photothermalUnitsRequiredForFloralInduction;
  { tell drawing plant to stream out, but not params }
  if streamDrawingPlant then
    drawingPlant.streamDataWithoutParamsWithFiler(filer, cvir);
  end;

procedure GsPlant.setCultivarName(aString: string);
  begin
  cultivarName := copy(aString, 1, kGraphicalModelNameLength);
  end;

var
  globalCultivarNameString: string;

procedure GsPlant.streamParamsWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord;
    streamDrawingPlant: boolean);
	begin
  filer.streamBytes(options, sizeOf(options));
  filer.streamBytes(params, sizeOf(params));
  { take extra care to limit cultivar name size }
  if filer.isWriting then globalCultivarNameString := cultivarName;
  filer.streamShortString(globalCultivarNameString);
  if filer.isReading then self.setCultivarName(globalCultivarNameString);
  { tell drawing plant to stream only params }
  if streamDrawingPlant then
    drawingPlant.streamParamsWithFiler(filer, cvir);
  if (filer.isReading) and (cvir.additionNumber < 1) then
    begin
    params.areaOfSoilPatchInWhichPlanted_ha := 1.0 * m2_to_ha;
    params.plantingSpringFallOrBoth := kCultivarIsPlantedInSpring;
    params.plantingDaysAfterLastSpringFrost_days := 14;
    params.maturityDaysBeforeFirstFallFrost_days := 0;
    params.plantingDaysFromSeedToGerminatedSeed_days := 14;
    params.plantingDaysFromGerminatedSeedToSeasonMaturity_days := 50;
    params.minPossibleHeatUnitsBeforeGerminationInAnyClimate_degC := 10.0;
    params.maxPossibleHeatUnitsBeforeGerminationInAnyClimate_degC := 100.0;
    params.minPossibleHeatUnitsBeforeMaturityInAnyClimate_degC := 100.0;
    params.maxPossibleHeatUnitsBeforeMaturityInAnyClimate_degC := 3000.0;
    end;
  end;

procedure GsPlant.transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  try
  self.directTransferField(d, v, fieldID, ft, index, deriveMethod, updateList);
  if (d = kGetField) then exit;
  case fieldID of
    kPlantRootGrowthRestrictionByRockCoeffs:
      Utils_CalcSCurveCoeffs(params.rootGrowthRestrictionByRockCoeffs);
    kPlantRootGrowthRestrictionByAerationStress:
      Utils_CalcSCurveCoeffs(params.rootGrowthRestrictionByAerationStress);
    kPlantPlantNAndPStressCoeffs:
      Utils_CalcSCurveCoeffs(params.plantNAndPStressCoeffs);
    kPlantPUptakeCoeffs:
      Utils_CalcSCurveCoeffs(params.pUptakeCoeffs);
    kPlantBiomassToEnergyRatio_kgPhaPMJ, kPlantBiomassToEnergyRatioAtElevatedCO2_kgPhaPMJ:
      begin
      Utils_InitSCurveParam(params.biomassToEnergyRatioCO2Params,
          330.0, params.biomassToEnergyRatio_kgPhaPMJ * 0.01,
          660.0, params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ * 0.01);
      self.addToUpdateList(kPlantBiomassToEnergyRatioCO2Params, -1{scurve is an array}, updateList);
      end;
    kPlantHeatUnitFactorParamsForLAI:
      Utils_CalcSCurveCoeffs(params.heatUnitFactorParamsForLAI);
      { heatUnitFactorParamsForLAI: X is proportion of growing season (HUI); Y is fraction of max potential LAI.
      0.15 of growing season (HUI=0.15) gives 0.05 of max LAI; 0.5 of growing season gives 0.95 of max LAI
      Note that EPIC has % and divides by 100 to get proportion for X variable (it can't be prop because is on left side).
      For this and several other s curves I have changed them so our input will be as a fraction. }
    kPlantNFractionAtEmergence_frn, kPlantNFractionAtHalfMaturity_frn, kPlantNFractionAtMaturity_frn:
      ConvertOptimalNParams;
    kPlantPFractionAtEmergence_frn, kPlantPFractionAtHalfMaturity_frn, kPlantPFractionAtMaturity_frn:
      ConvertOptimalPParams;
    kPlantFrostReductionFactorParams:
      Utils_CalcSCurveCoeffs(params.frostReductionFactorParams);
      { frostReductionFactorParams: X is min temp in NEGative deg C, Y is fraction of biomass lost each day this temp occurs.
      No frost damage is assumed to occur at a min temp over 0 deg C.
      Current parameters (-5 to -15) are likely to underestimate frost damage.
      -5 deg C gives 0.01 reduction in biomass; -15 deg C gives 0.95 reduction in biomass
      special note: the temp has to be input in POSitive deg C. transferField is handling this special case. }
    kPlantVPDAboveThresholdAtThreeQuartersMaxLeafConductance_kPa, kPlantThresholdVaporPressureDeficit_kPa:
      ConvertLeafConductanceParams;
    kPlantFloralInductionParams:
      Utils_CalcSCurveCoeffs(params.floralInductionParams);
      { floralInductionParams: X is hours of darkness (out of 24), Y is fraction of photoinductive day accumulated.
      5 hours darkness gives 0.05 photoinductive days; 12 hours darkness gives 0.95 photoinductive days
      Note that this is for a SDP (long-night plant); for a LDP (short-night plant) the 5 and 12 would be reversed.
      These are our addition. }
    kPlantOptimalTemp_degC:
      self.sideEffects_optimalTemp_degC(updateList);
    kPlantBaseTemp_degC:
      self.sideEffects_baseTemp_degC(updateList);
    end;
  except on e: Exception do errorMessage('Exception in plant transferField: ' + e.message); end;
  end;

{ sideEffects functions }
procedure GsPlant.sideEffects_optimalTemp_degC(updateList: TListCollection);
  begin
  { optimal temp cannot be lower than base temp (push base temp down) }
  if params.optimalTemp_degC < params.baseTemp_degC then
    begin
    params.baseTemp_degC := params.optimalTemp_degC;
    self.addToUpdateList(kPlantBaseTemp_degC, 0, updateList);
    self.sideEffects_baseTemp_degC(updateList);
    end;
  end;

procedure GsPlant.sideEffects_baseTemp_degC(updateList: TListCollection);
  begin
  { optimal temp cannot be lower than base temp (push optimal temp up) }
  if params.optimalTemp_degC < params.baseTemp_degC then
    begin
    params.optimalTemp_degC := params.baseTemp_degC;
    self.addToUpdateList(kPlantOptimalTemp_degC, 0, updateList);
    end;
  self.calculatePotentialHeatUnits(kNotBeingPlantedNow);
  development.heatUnitIndex := EQ.HeatUnitIndex(development.cumHeatUnits, params.potHeatUnitsReqForMaturation);
  self.addToUpdateList(kPlantMinHeatUnitsBeforeGermination_degC, 0, updateList);
  self.addToUpdateList(kPlantPotHeatUnitsReqForMaturation, 0, updateList);
  self.addToUpdateList(kPlantHeatUnitIndex, 0, updateList);
  end;

{d = direction; v = value; ft = fieldType (shortened for long lines) }
procedure GsPlant.directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  if (fieldID >= kPlantZZDrawFirstField) then
  	begin
    if (drawingPlant <> nil) then drawingPlant.directTransferField(d, v, fieldID, ft, index, deriveMethod, updateList);
    end
  else
    Plant_directTransferField(self, v, d, fieldID, ft, index, deriveMethod, updateList);
  self.addToUpdateList(fieldID, index, updateList);
  end;

{USAGE:
      id: BDConvert(accessor, v, fieldID, deriveMethod, ft, d, index, updateList);
}
procedure GsPlant.BDConvert(var modelValue: single; var value;
    fieldID, deriveMethod, fieldType, direction, index: integer; updateList: TListCollection);
  begin
  case deriveMethod of
    kDeriveTypeUndefined: self.MFD(modelValue, value, fieldType, direction);
    kDeriveTypeDepth: raise Exception.create('Depth derivation not supported by plant');
    kDeriveTypeConcentration: raise Exception.create('Concentration from mass derivation not supported by plant');
    kDeriveTypeArea: self.ConvertRelativeAndAbsolute(direction, modelValue, single(value));
    kDeriveTypeConcentrationFromPercent:
      raise Exception.create('Concentration from percent derivation not supported by plant');
    else
      raise Exception.create('Derivation method not supported by plant');
    end;
  self.addToUpdateList(fieldID, index, updateList);
  end;

procedure GsPlant.ConvertRelativeAndAbsolute(direction: integer; var modelValue: single; var value: single);
  var
    soilPatchArea_ha: single;
  begin
  try
  { if there is no soil patch associated with this plant, use a default soil patch area of 1 m2 }
  if soil <> nil then
    soilPatchArea_ha := soil.params.area_ha
  else
    soilPatchArea_ha := 1.0 * m2_to_ha;
  if (direction = kSetField) then
    begin
    modelValue := safedivExcept(value, soilPatchArea_ha, 0.0);
    end
  else
    begin
    value := modelValue * soilPatchArea_ha;
    end;
  except on e: Exception do errorMessage('Exception in plant ConvertRelativeAndAbsolute: ' + e.message); end;
  end;

class procedure GsPlant.fillEnumStringList(var list: TStringList; fieldID: Integer;
    var hasRadioButtons: boolean);
  begin
  { assumes list being given to it is empty }
  case fieldID of
    kPlantLifeCycleType:
      begin
      list.add('Annual');
      list.add('Biennial');
      list.add('Perennial');
      hasRadioButtons := true;
      end;
    kPlantVernalizationRequirement:
      begin
      list.add('No vernalization');
      list.add('Vernalization is obligate (necessary)');
      list.add('Vernalization is quantitative (speeds things up)');
      hasRadioButtons := true;
      end;
    kPlantLifeHistoryStage:  { read-only }
      begin
      list.add('Seed');
      list.add('Vegetative period');
      list.add('Floral induction period');
      list.add('Floral initiation period');
      list.add('Reproductive allocation period');
      list.add('Vernalization has failed');
      hasRadioButtons := true;
      end;
    kPlantPlantingSpringFallOrBoth:
      begin
      list.add('spring');
      list.add('fall');
      list.add('spring or fall');
      hasRadioButtons := true;
      end;
    { These last four are really for the drawing plant, but since this is a class function and
      the component doesn't know they belong to the drawing plant, we must handle them here. }
    kPlantZZDrawGeneralGender:
      begin
      list.add('Male');
      list.add('Female');
      list.add('Hermaphroditic');
      hasRadioButtons := true;
      end;
    kPlantZZDrawGeneralStorageOrganLumpedWith:
      begin
      list.add('Root');
      list.add('Stem');
      list.add('Whole plant');
      list.add('None (by itself)');
      hasRadioButtons := true;
      end;
    kPlantZZDrawGeneralHarvestItemsBundledWithWholePlant:
      begin
      list.add('seedling leaves');
      list.add('leaves');
      list.add('female inflorescences');
      list.add('male inflorescences');
      list.add('female flowers');
      list.add('female flower buds');
      list.add('male flowers');
      list.add('male flower buds');
      list.add('axillary buds');
      list.add('fruits');
      list.add('root');
      list.add('stems');
      hasRadioButtons := false;
      end;
    kPlantZZDrawLeafCompoundPinnateOrPalmate:
      begin
      list.add('pinnate (feather-like)');
      list.add('palmate (hand-like)');
      hasRadioButtons := true;
      end;
  else
    raise Exception.create('unknown field for plant string list ' + intToStr(fieldID));
  end;
  end;

function GsPlant.objectType: integer;
  begin
  result := kObjectTypePlant;
  end;

procedure GsPlant.streamUsingTextFiler(textFiler: GsTextFiler);
  begin
  if textFiler.versionString = 'v0.9' then
    Plant_streamUsingTextFilerVersion090(self, textFiler)
  else if textFiler.versionString = 'v1.0' then
    Plant_streamUsingTextFilerVersionOne(self, textFiler);
  if textFiler.isReading then
  	 development.photothermalUnitsRequiredForFloralInductionAfterVernalization :=
    		params.photothermalUnitsRequiredForFloralInduction;
  end;

end.

(*  not using, but save for reference
const
  kShortDayPlant = 1;
  kLongDayPlant = 2;
  kDayNeutralPlant = 3;

  case inductionType of
    kShortDayPlant: Utils_InitSCurveParam(params.floralInductionParams, 5.0, 0.05, 12.0, 0.95);
    kLongDayPlant:  Utils_InitSCurveParam(params.floralInductionParams, 12.0, 0.05, 5.0, 0.95);
    kDayNeutralPlant: Utils_InitSCurveParam(params.floralInductionParams, 5.0, 0.95, 12.0, 0.95);
    end;
 *)



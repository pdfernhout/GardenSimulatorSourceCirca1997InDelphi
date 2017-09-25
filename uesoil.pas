unit uesoil;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uesoil: Soil patch object. Information in the soil patch is in several record structures
as specified in uestruct. Most of the soil patch information is in the layers structure
which has all of the information that is by layer. Most functions are called in the
next day function (a few in the end year function). Actions by tools on the soil patch
were moved to uesoilop, and the soil patch is passed in (because this file was too long).
The soil patch has a TListCollection of plants and a pointer back to the garden. Read
uestruct to understand the structures here before reading this code.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses
  ExtCtrls, WinTypes, WinProcs, Graphics, ufiler, ufilertx, umodel, ucollect, uegarden, uestruct, classes,
  ueorgmat, uebag, ugroups, urandom;

type
GsSoilPatch = class(GsGraphicalModel)
  public
  extendedBoundsRect: TRect;
  garden: GsGarden;
  options: soilOptionsArray;
  params: SoilParamsStructure;
  state: StateStructure;
  surface: SurfaceStructure;
  mulch: MulchStructure;
  water: WaterStructure;
  erosion: ErosionStructure;
  layers: LayerStructureArray;
  movement: MovementStructure;
  totals: TotalsStructure;
  yearTotals: YearTotalsStructure;
  plantMeans: PlantMeansStructure;
  plantList: TListCollection;
  looseOrganicMatterList: TListCollection;
  corners: array[0..3] of TPoint;
  soilTypeName: string[kGraphicalModelNameLength];
  randomNumberGenerator: GsRandom;
  daysSinceLastAutoHarvest: integer; {don't need to stream-starting over at zero is okay}
  { create/destroy }
  constructor create; override;
  constructor createWithGardenAndPoint(aGarden: GsGarden; point: TPoint);
  destructor destroy; override;
  { graphical functions }
  function preciseIncludesPointTest(const aPoint: TPoint): boolean; override;
  procedure drawOn(destCanvas: TCanvas); override;
  procedure computeBoundsRect;
	procedure computeExtendedBoundsRect;
  function findPlant(const aPoint: TPoint) : GsModel;
  procedure resizeTo(point: TPoint);
	function enforceMinimumStartingSize: boolean;
  procedure moveCornerTo(corner: integer; point: TPoint);
  procedure moveBy(delta: TPoint);
  function findNearestCornerTo(point: TPoint): integer;
	function surfaceColor: TColorRef;
	function colorForSoilLayer(layerIndex: integer): TColorRef;
  function soilColor(baseColor: TColorRef; fractionOfFieldCapacity, orgC_pct: single): TColorRef;
  function snowOnSoilPatch: boolean;
  { streaming functions }
	function objectType: integer; override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
	procedure copyToWithoutSubobjects(newCopy: GsSoilPatch);
	procedure streamWithoutSubobjectsUsingFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  procedure copyFromSoilType(aGarden: GsGarden;  soilType: GsSoilPatch);
  procedure setSoilTypeName(aString: string);
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  { transfer functions }
  procedure directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure BDConvert(var modelValue: single; var value;
    fieldID, deriveMethod, fieldType, direction, index: integer; updateList: TListCollection);
  procedure ConvertRelativeAndAbsolute(direction: integer; var modelValue: single; var value);
  procedure convertWaterAbsoluteAndRelative(d: integer; var modelValue: single; var value; layer: integer);
  procedure convertNutrientsAbsoluteAndRelative(d: integer; var modelValue: single; var value; layer: integer);
  class procedure fillEnumStringList(var list: TStringList; fieldID: Integer; var hasRadioButtons: boolean); 
  procedure fillBasicInfoStructure(var basicInfo: SoilPatchBasicInfoStructure);
  procedure useBasicInfoStructure(basicInfo: SoilPatchBasicInfoStructure);
  function valueForLayerArray(model: GsModel; arrayShowType, fieldNumber: smallint; arraySelected: selectionArray;
      deriveMethod: smallint): single;
  { next day }
  procedure SoilNextDay;
  { functions called by next day }
  procedure IncorporateLooseOrganicMatter;
  procedure GetRainfallForDay;
  procedure PatchTotalAboveGroundBiomassAndResidue;
  procedure SoilCoverIndex;
  procedure Albedo;
  procedure SoilTemperature(theMonth: integer);
  procedure WindErosion(theMonth: integer);
  procedure Snowmelt;
  procedure RunoffAndWaterErosionFromRainfall(theMonth: integer);
  procedure Snowfall;
  procedure AdjustRidgesDikesForRainfall;
  procedure NitrateEnteringSoilInRainfall;
  procedure NitrateEnteringSoilInIrrigation;
  procedure GetTodaysIrrigationFromYesterdaysAutoAndTodaysUser;
  procedure PercolationAndLateralFlow;
  procedure PotentialSoilEvaporation;
  procedure PotentialPlantTranspiration;
  procedure ActualSoilEvaporation;
  procedure UpdateWaterHistoryArray;
  procedure WaterAllocationToPlants;
  procedure checkUptakeAgainstAvailable(var uptakeAmount: single; totalSoFar, amountAvailable: single);
  procedure WaterTableDynamics;
  procedure RebuildDikesIfNecessary;
  procedure AutoIrrigateIfNecessary;
  procedure RunoffAndWaterErosionFromIrrigation;
  procedure NutrientCycling;
  procedure NutrientAllocationToPlants;
  procedure PlantNextDayLoopBeforeWaterAndNutrientAllocation;
  procedure PlantNextDayLoopBetweenWaterAndNutrientAllocation;
  procedure PlantNextDayLoopAfterWaterAndNutrientAllocation;
  procedure AutoFertilizeIfNecessary;
  procedure NutrientLossInSediment;
  procedure SoilSettlingFromRain;
  procedure NitratesMovedUpFromEvaporation;
  procedure CheckSedimentYieldAgainstMaximum;
  procedure RemoveErodedSoilIfNecessary;
  procedure NitrateBalanceForDay;
  { zeroing, totaling and intitializing functions }
  procedure SoilEndYear;
  procedure cleanUpAfterCopyFromSoilType;
  procedure CalculatePlantMeans;
  procedure ZeroPlantMeans;
  procedure SoilPatchTotals;
  procedure RecalculatePSPandMineralPFlowCoefficient;
  procedure RecalculateAluminumSaturation;
  procedure RecalculatePorosityFromBulkDensity;
  procedure ZeroSoilTotalsVars;
  { functions called by tool functions }
   function totalSoilWeightDownToCriterionDepth(criterionDepth_m: single): single;
  procedure adjustLowerLayerDepthsForChangeToOneLayer(affectedLayer: integer; changeInDepth_m: single);
  procedure setLayerDepth(layer: integer; newDepth_m: single);
  function snowDepthFromSnowWaterContent_m: single;
  function getHint(longHint: boolean; metricUnits: boolean): string;
  { auto and other occasional functions }
  procedure AutoLime;
  procedure ChangePHFromFertilizerAdded;
  procedure MeanWeightedLimingValues(var meanWeightedOrganicC_pct: single; var meanWeightedCEC_cmolPkg: single;
    var meanWeightedSoilpH: single; var meanWeightedBaseFormingCations_cmolPkg: single; var
    totalSoilWeight_tPha: single; var layerBelowMaxTillageDepth: smallint);
  procedure SetLayerPropertiesAfterLiming(layerBelowMaxTillageDepth: integer; newBaseFormingCations_cmolPkg: single;
    newSoilpH: single; newAluminumSaturation_pct: single);
  procedure AutoFertilize(doNFert: boolean);
  procedure AutoIrrigate;
  procedure TransferStandingDeadToFlatResidueWhenTilling(tillageDepth_m: single; mixingEfficiency_frn: single);
  procedure AdjustPlantRootWeightsInLayersAfterMovingSoil(var oldDepth_m: arraySoilLayers);
  procedure AddOrRemoveSoilBasedOnLayerWeight(layer: smallint; changeInWeight_tPha: single);
  procedure AddOrRemoveSoilBasedOnLayerDepth(layer: smallint; changeInDepth_m: single);
  procedure updateSoilProfileDepthAndOtherDepths;
  function optionIsTrue(index: smallint): boolean;
  { linked list functions }
  function GetNumActivePlants: integer;
  function GetNumPlantsNotAwaitingReseeding: integer;
  procedure addPlant(thePlant: GsModel);
  procedure updatePlantMeans;
  procedure removePlant(thePlant: GsModel);
  { side effects functions }
  procedure sideEffects_slopeSteepness_mPm(updateList: TListCollection);
  procedure sideEffects_watershedArea_ha(updateList: TListCollection);
  procedure sideEffects_watershedSlopeLength_ha(updateList: TListCollection);
  procedure sideEffects_waterTableMinDepth_m(updateList: TListCollection);
  procedure sideEffects_waterTableMaxDepth_m(updateList: TListCollection);
  procedure sideEffects_waterTableDepth_m(updateList: TListCollection);
  procedure sideEffects_mulchFlatCropResidue_tPha(updateList: TListCollection);
  procedure sideEffects_mulchDepth_m(updateList: TListCollection);
  procedure sideEffects_layerWeight_tPha(index: smallint; oldWeight_tPha: single; updateList: TListCollection);
  procedure sideEffects_layerDepth_m(index: smallint; oldDepth_m: single; updateList: TListCollection);
  procedure sideEffects_wiltingPoint_mm(index: smallint; updateList: TListCollection);
  procedure sideEffects_fieldCapacity_mm(index: smallint; updateList: TListCollection);
  procedure sideEffects_porosity_mm(index: smallint; updateList: TListCollection);
  procedure sideEffects_bulkDensity_tPm3(index: smallint; updateList: TListCollection);
  procedure sideEffects_settledBulkDensity_tPm3(index: smallint; updateList: TListCollection);
  procedure sideEffects_ovenDryBulkDensity_tPm3(index: smallint; updateList: TListCollection);
  procedure sideEffects_organicMatter_tPha(index: smallint; updateList: TListCollection);
  procedure sideEffects_calciumCarbonate_pct(index: smallint; updateList: TListCollection);
  procedure sideEffects_baseFormingCations_cmolPkg(index: smallint; updateList: TListCollection);
  procedure sideEffects_soilpH(index: smallint; updateList: TListCollection);
  procedure sideEffects_cationExchangeCapacity_cmolPkg(index: smallint; updateList: TListCollection);
  procedure sideEffects_clayContent_pct(index: smallint; updateList: TListCollection);
  procedure sideEffects_sandContent_pct(index: smallint; updateList: TListCollection);
  procedure sideEffects_siltContent_pct(index: smallint; updateList: TListCollection);
  procedure sideEffects_rockContent_pct(index: smallint; updateList: TListCollection);
  end;

procedure FixUpPlant(each: TObject; data: TObject);

implementation

uses ueutils, ueq, ueqh, uep, ueg, ueplant, umconsts, SysUtils, uunits, uaspects, uturt3d,
  udate, uclasses, usupport, uesoilop, udefault, uhardcd, utstream;

{ ------------------------------------------------------------------------------------------ next day }
procedure GsSoilPatch.SoilNextDay;
  var
    theMonth: integer;
  begin
  IncorporateLooseOrganicMatter;
  theMonth := GsDate_monthFromDate(garden.date);
  GetRainfallForDay;
  CalculatePlantMeans;
  SoilPatchTotals;
  PatchTotalAboveGroundBiomassAndResidue;
  SoilCoverIndex;
  Albedo;
  SoilTemperature(theMonth);
  WindErosion(theMonth);
  if (garden.weather.dailyWeather.meanTempForDay_degC > 0.0) then
    begin
    Snowmelt;
    RunoffAndWaterErosionFromRainfall(theMonth);
    end
  else
    begin
    Snowfall;
    end;
  erosion.totalErosion_tPha := erosion.waterErosion_tPha + erosion.windErosion_tPha;
  GetTodaysIrrigationFromYesterdaysAutoAndTodaysUser;
  AdjustRidgesDikesForRainfall;
  NitrateEnteringSoilInRainfall;
  NitrateEnteringSoilInIrrigation;
  PercolationAndLateralFlow;
  PotentialSoilEvaporation;
  PotentialPlantTranspiration;
  ActualSoilEvaporation;
  UpdateWaterHistoryArray;
  WaterTableDynamics;
  RebuildDikesIfNecessary;
  AutoIrrigateIfNecessary;
  RunoffAndWaterErosionFromIrrigation;
  addQuantity(yearTotals.cumIrrigationApplied_mm, water.autoIrrigationForDay_mm + water.userIrrigationForDay_mm);
  water.userIrrigationForDay_mm := 0.0;
  NutrientCycling;
  PlantNextDayLoopBeforeWaterAndNutrientAllocation;
  WaterAllocationToPlants;
  PlantNextDayLoopBetweenWaterAndNutrientAllocation;
  NutrientAllocationToPlants; 
  PlantNextDayLoopAfterWaterAndNutrientAllocation;
  AutoFertilizeIfNecessary;
  NutrientLossInSediment;
  SoilSettlingFromRain; 
  CalculatePlantMeans;
  SoilPatchTotals;
  PatchTotalAboveGroundBiomassAndResidue;
  NitratesMovedUpFromEvaporation;
  CheckSedimentYieldAgainstMaximum;
  RemoveErodedSoilIfNecessary; 
  NitrateBalanceForDay;
  inc(daysSinceLastAutoHarvest);
  end;

{ ------------------------------------------------------------------------------------------ create/destroy }
constructor GsSoilPatch.create;
  var i: smallint;
	begin
  inherited create;
  plantList := TListCollection.create;
  looseOrganicMatterList := TListCollection.create;
  randomNumberGenerator := GsRandom.create;
  randomNumberGenerator.randomizeFromTime;
  for i := 0 to 100 do randomNumberGenerator.zeroToOne;
  params.drawingScale := 1.0;
  params.shadeIndex_pct := 10.0;
  params.viewingAngle_deg := 0.0;
  for i := 0 to kSoilOptionsLastOption do options[i] := true;
  end;

constructor GsSoilPatch.createWithGardenAndPoint(aGarden: GsGarden; point: TPoint);
	begin
  self.create;
  garden := aGarden;
  corners[0] := point;
  corners[1].x := point.x + 10;
  corners[1].y := point.y ;
  corners[2].x := point.x + 10;
  corners[2].y := point.y + 10;
  corners[3].x := point.x;
  corners[3].y := point.y + 10;
  self.computeBoundsRect;
  end;

procedure FixUpPlant(each: TObject; data: TObject);
  begin
  GsPlant(each).soil := GsSoilPatch(data);
  end;

destructor GsSoilPatch.destroy;
  begin
  plantList.free;
  plantList := nil;
  looseOrganicMatterList.free;
  looseOrganicMatterList := nil;
  RandomNumberGenerator.free;
  RandomNumberGenerator := nil;
  inherited destroy;
  end;

{ ------------------------------------------------------------------------------------------ drawing/graphics }
function GsSoilPatch.getHint(longHint: boolean; metricUnits: boolean): string;
  var
    layer: smallint;
    fcMwp_mm, swMwp_mm, soilProfileFractionOfFieldCapacity_frn, surfaceTemp: single;
    totalSolubleN_kgPha, totalOrganicN_kgPha, totalOrganicP_kgPha, totalMineralP_kgPha: single;
  begin
  result := self.getName;
  if not longHint then exit;
  {water - fraction of field capacity}
  try
  fcMwp_mm := 0.0;
  swMwp_mm := 0.0;
  for layer := 0 to state.numLayers - 1 do
    begin
    fcMwp_mm := fcMwp_mm + layers[layer].fieldCapacity_mm - layers[layer].wiltingPoint_mm;
    swMwp_mm := swMwp_mm + layers[layer].waterContent_mm - layers[layer].wiltingPoint_mm;
    end;
  if fcMwp_mm > 0 then
    soilProfileFractionOfFieldCapacity_frn := safediv(swMwp_mm, fcMwp_mm)
  else
    soilProfileFractionOfFieldCapacity_frn := 0.0;
  except
    soilProfileFractionOfFieldCapacity_frn := 0.0;
  end;
  result := result + '. Fraction of field capacity ' + digitValueString(soilProfileFractionOfFieldCapacity_frn);
  if not metricUnits then
    surfaceTemp := Convert(kTemperature, kTemperatureDegreesC, kTemperatureDegreesF, surface.temp_degC)
  else
    surfaceTemp := surface.temp_degC;
  result := result + '. Surface temp ' + intToStr(trunc(surfaceTemp));
  if not metricUnits then
    result := result + ' F'
  else
    result := result + ' C';
  {N}
  result := result + '. Nitrate ' + digitValueString(totals.patchTotalNitrate_kgPha * params.area_ha) + ' kg';
  totalOrganicN_kgPha := totals.patchTotalOrganicNFresh_kgPha + totals.patchTotalOrganicNStableHumus_kgPha
      + totals.patchTotalOrganicNActiveHumus_kgPha;
  result := result + ', organic N ' + digitValueString(totalOrganicN_kgPha * params.area_ha) + ' kg';
  {P}
  result := result + ', labile P ' + digitValueString(totals.patchTotalLabileP_kgPha * params.area_ha) + ' kg';
  totalOrganicP_kgPha := totals.patchTotalOrganicPFresh_kgPha + totals.patchTotalOrganicPHumus_kgPha;
  result := result + ', organic P ' + digitValueString(totalOrganicP_kgPha * params.area_ha) + ' kg';
  totalMineralP_kgPha := totals.patchTotalMineralPActive_kgPha + totals.patchTotalMineralPStable_kgPha;
  result := result + ', mineral P ' + digitValueString(totalMineralP_kgPha * params.area_ha) + ' kg';
  {C}
  if plantMeans.totalStandingDead_tPha > 0 then
    result := result + ', standing dead '
        + digitValueString(plantMeans.totalStandingDead_tPha * t_to_kg * params.area_ha) + ' kg';
  result := result + ', mulch '
      + digitValueString(mulch.flatCropResidue_tPha * t_to_kg * params.area_ha) + ' kg';
  result := result + ', flat residue '
      + digitValueString(totals.patchTotalFlatResidue_tPha * t_to_kg * params.area_ha) + ' kg';
  result := result + ', organic matter '
      + digitValueString(totals.patchTotalOrganicMatter_tPha * t_to_kg * params.area_ha) + ' kg.';
  end;

{ got this function from udplant. should be placed in usupport }
function blendColors(firstColor: TColorRef; secondColor: TColorRef; aStrength: single): TColorRef;
  begin
  result := 0;
  {blend first color with second color,
   weighting the second color by aStrength (0-1) and first color by (1 - aStrength).}
  if (aStrength < 0.0) or (aStrength > 1.0) then
    raise Exception.create('color blend strength outside of range');
  result := support_rgb(
    round((GetRValue(firstColor) * (1.0 - aStrength)) + (GetRValue(secondColor) * aStrength)),
    round((GetGValue(firstColor) * (1.0 - aStrength)) + (GetGValue(secondColor) * aStrength)),
    round((GetBValue(firstColor) * (1.0 - aStrength)) + (GetBValue(secondColor) * aStrength)));
  end;

function GsSoilPatch.surfaceColor: TColorRef;
  var
    mulchDepthForMulchColor_m, proportion: single;
  begin
  result := 0;
  try
  if self.snowOnSoilPatch then
    result := support_rgb(255,255,255)
  else
    begin
    result := self.colorForSoilLayer(0);
    if mulch.flatCropResidue_tPha > 0.0 then
      begin
      { set mulch depth at which pure mulch color is used (no soil shows through) at around 6 inches }
      mulchDepthForMulchColor_m := 6.0 * in_to_cm * cm_to_m;
      mulch.depth_m := Utils_MulchDepthFromMulchFlatCropResidue_m(mulch.flatCropResidue_tPha);
      proportion := min(1.0, safediv(mulch.depth_m, mulchDepthForMulchColor_m));
      result := blendColors(result, params.mulchDrawingColor, proportion);
      end;
    end;
  except on e: Exception do errorMessage('Problem in soil surfaceColor: ' + e.message); end;
  end;

function GsSoilPatch.snowOnSoilPatch: boolean;
  begin
  result := (water.snowWaterContent_mm > 0.0);
  end;

function GsSoilPatch.colorForSoilLayer(layerIndex: integer): TColorRef;
  var
    fractionOfFieldCapacity, orgC_pct: single;
    soilWater_mm: single;
  begin
  result := 0;
  try
  if (layerIndex < 0) or (layerIndex >= self.state.numLayers) then
    begin
    raise Exception.create('soil index out of range');
    result := clRed;
    exit;
    end;
  soilWater_mm := self.layers[layerIndex].waterContent_mm;
  if layerIndex = 0 then
    soilWater_mm := soilWater_mm + water.patchIrrigationForDay_mm;
  fractionOfFieldCapacity := EQH.FractionOfFieldCapacity(soilWater_mm,
      self.layers[layerIndex].wiltingPoint_mm, self.layers[layerIndex].fieldCapacity_mm);
  orgC_pct := Utils_OrganicCFromOrganicMatter_pct(self.layers[layerIndex].organicMatter_tPha,
      self.layers[layerIndex].weight_tPha);
  result := self.soilColor(self.params.baseDrawingColor, fractionOfFieldCapacity, orgC_pct);
  except on e: Exception do errorMessage('Problem in soil colorForSoilLayer: ' + e.message); end;
  end;

function GsSoilPatch.SoilColor(baseColor: TColorRef; fractionOfFieldCapacity, orgC_pct: single): TColorRef;
  var
    r, g, b: single;
    darkeningFromOM_frn, darkeningFromWater_frn, totalDarkeningFactor_frn: single;
    fractionOfMaxOrgC_frn: single;
  begin
  { convert baseColor to r, g, b }
  r := getRValue(baseColor);
  g := getGValue(baseColor);
  b := getBValue(baseColor);
  { arbitrarily set 5% organic carbon as maximum darkness (25% of normal brightness) }
  fractionOfMaxOrgC_frn := min(5.0, orgC_pct) / 5.0;
  darkeningFromOM_frn := 0.25 * fractionOfMaxOrgC_frn;
  { arbitrarily set fractionOfFieldCapacity of 1.0 as maximum darkness (75% of normal brightness) }
  darkeningFromWater_frn := 0.75 * fractionOfFieldCapacity;
  { calculate total darkening factor by adding OM and water factors }
  { this means that perfectly black soil has 20% OM and 1.0 FFC }
  totalDarkeningFactor_frn := darkeningFromOM_frn + darkeningFromWater_frn;
  r := r * (1.0 - totalDarkeningFactor_frn);
  g := g * (1.0 - totalDarkeningFactor_frn);
  b := b * (1.0 - totalDarkeningFactor_frn);
  { bound rgbs }
  r := max(0, min(255, r));
  g := max(0, min(255, g));
  b := max(0, min(255, b));
  {we decided to go with dithering for the soil - commented version is non dithered}
  {result := support_rgb(round(r), round(g), round(b));  }
  result := rgb(round(r), round(g), round(b));
  end;

function GsSoilPatch.findNearestCornerTo(point: TPoint): integer;
  var
    distances: array[0..3] of real;
    i: integer;
    nearestCorner : integer;
    minDistance : single;
  begin
  result := 0;
  for i := 0 to 3 do
  	distances[i] := (1.0 * (corners[i].x - point.x) * (corners[i].x - point.x)) +
    	(1.0 * (corners[i].y - point.y) * (corners[i].y - point.y));
  nearestCorner := 0;
  minDistance := distances[0];
  for i := 1 to 3 do
  	if distances[i] < minDistance then
    	begin
      minDistance := distances[i];
      nearestCorner := i;
      end;
  result := nearestCorner;
  end;

procedure GsSoilPatch.resizeTo(point: TPoint);
  var origin: TPoint;
	begin;
  origin := corners[0];
  corners[1].x := origin.x + (point.x - origin.x);
  corners[1].y := origin.y ;
  corners[2].x := origin.x + (point.x - origin.x);
  corners[2].y := origin.y + (point.y - origin.y);
  corners[3].x := origin.x;
  corners[3].y := origin.y + (point.y - origin.y);
  self.computeBoundsRect;
	end;

function GsSoilPatch.enforceMinimumStartingSize: boolean;
  var
   origin: TPoint;
   newFarCorner: TPoint;
	begin;
  {make sure the patch is at least 25 X 25 pixels to start }
  result := false;
  newFarCorner := corners[2];
  origin := corners[0];
  if (abs(newFarCorner.x - origin.x) < kSoilMinimumInitialSize) then
    begin
    newFarCorner.x := origin.x + kSoilMinimumInitialSize;
    result := true;
    end;
  if (abs(newFarCorner.y - origin.y) < kSoilMinimumInitialSize) then
    begin
    newFarCorner.y := origin.y + kSoilMinimumInitialSize;
    result := true;
    end;
	if result then
  	self.resizeTo(newFarCorner);
	end;

procedure GsSoilPatch.moveCornerTo(corner: integer; point: TPoint);
	begin;
  corners[corner] := point;
  self.computeBoundsRect;
	end;

procedure GsSoilPatch.moveBy(delta: TPoint);
  var i: integer;
  begin
  for i := 0 to 3 do
    begin
    corners[i].x := corners[i].x + delta.x;
    corners[i].y := corners[i].y + delta.y;
    end;
  { ok to move plants awaiting reseeding }
	if plantList.count > 0 then
  	for i := 0 to plantList.count - 1 do
      GsPlant(plantList.Items[i]).moveBy(delta);
 self.computeBoundsRect;
 end;

function GsSoilPatch.findPlant(const aPoint: TPoint) : GsModel;
	var
    i : integer;
    plant: GsPlant;
	begin
  result := nil;
	if plantList.count > 0 then
  	for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.Items[i]);
      { should not find a plant that is awaiting reseeding - but in stasis box is ok }
    	if (not plant.awaitingReseeding) and (plant.includesPoint(aPoint)) then
    		begin
     		result := plant;
      	break;
    		end;
      end;
  end;

procedure GsSoilPatch.computeBoundsRect;
	var
  i : integer;
	begin
  boundsRect.topLeft := corners[0];
  boundsRect.bottomRight := corners[0];
  with boundsRect do
  	begin
  	for i := 1 to 3 do
    	with corners[i] do
  			begin
    		if x < left then left := x;
    		if x > right then right := x;
    		if y < top then top := y;
    		if y > bottom then bottom := y;
    		end;
    end;
  self.computeExtendedBoundsRect;
  end;

procedure GsSoilPatch.computeExtendedBoundsRect;
	var
  i : integer;
  plant: GsPlant;
  newBoundsRect: TRect;
	begin
  extendedBoundsRect := boundsRect;
  if plantList.count > 0 then
  	for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      { do not consider plants awaiting reseeding - but in stasis box is ok }
      if not plant.awaitingReseeding then
        begin
        UnionRect(newBoundsRect, extendedBoundsRect, plant.boundsRect);
        extendedBoundsRect := newBoundsRect;
        end;
      end;
  end;

function GsSoilPatch.preciseIncludesPointTest(const aPoint: TPoint): boolean;
	begin
  result := PointInQuadrangle(aPoint, corners);
  end;

procedure GsSoilPatch.drawOn(destCanvas: TCanvas);
	begin
  with destCanvas do
    begin
    brush.style := bsSolid;
    brush.color := self.surfaceColor;
    pen.style := psClear;
    polygon(corners);
    pen.style := psSolid;
    end;
  end;

{ ------------------------------------------------------------------------------------------ called by next day }
procedure GsSoilPatch.IncorporateLooseOrganicMatter;
  begin
  try
  { remove any organic matter blobs from yesterday }
  self.looseOrganicMatterList.clear;
  with movement do
    begin
    flatCropResidueAddedFromDeadPlant_tPha := flatCropResidueFromOM_tPha;
    flatCropResidueFromOM_tPha := 0.0;
    organicNFreshAddedFromDeadPlant_kgPha := organicNFreshFromOM_kgPha;
    organicNFreshFromOM_kgPha := 0.0;
    organicPFreshAddedFromDeadPlant_kgPha := organicPFreshFromOM_kgPha;
    organicPFreshFromOM_kgPha := 0.0;
    end;
  except on e: Exception do errorMessage('Exception in soil IncorporateLooseOrganicMatter: ' + e.message); end;
  end;

procedure GsSoilPatch.GetRainfallForDay;
  begin
  try
    if optionIsTrue(kSoilOptionsAllowPrecipitationToReachPatch) then
      water.patchRainfallForDay_mm := garden.weather.dailyWeather.rainfallForDay_mm
    else
      water.patchRainfallForDay_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil GetRainfallForDay: ' + e.message); end;
  end;

procedure GsSoilPatch.PatchTotalAboveGroundBiomassAndResidue;
  begin
  try
  totals.patchTotalAboveGroundBiomassAndResidue_tPha := plantMeans.totalStandingLive_tPha +
    layers[0].flatCropResidue_tPha + mulch.flatCropResidue_tPha + plantMeans.totalStandingDead_tPha;
  except on e: Exception do errorMessage('Exception in soil PatchTotalAboveGroundBiomassAndResidue: ' + e.message); end;
  end;

procedure GsSoilPatch.SoilCoverIndex;
  begin
  try
    if optionIsTrue(kSoilOptionsUpdateSoilCoverIndex) then
      surface.soilCoverIndex_frn := EQH.SoilCoverIndex_frn(totals.patchTotalAboveGroundBiomassAndResidue_tPha,
        water.snowWaterContent_mm);
  except on e: Exception do errorMessage('Exception in soil SoilCoverIndex: ' + e.message); end;
  end;

procedure GsSoilPatch.Albedo;
  begin
  try
    if optionIsTrue(kSoilOptionsUpdateAlbedo) then
      surface.albedo_frn := EQH.Albedo_frn(surface.soilCoverIndex_frn, params.soilAlbedo_frn, water.snowWaterContent_mm,
          plantMeans.meanLeafAreaIndex);
  except on e: Exception do errorMessage('Exception in soil Albedo: ' + e.message); end;
  end;

procedure GsSoilPatch.SoilTemperature(theMonth: integer);
  begin
  try
    if optionIsTrue(kSoilOptionsUpdateSoilTemperature) then
      EP.SoilTemperatureByLayer(state.numLayers, layers, surface, totals,
          garden.weather.GetProbWetDayForMonth_frn(theMonth),
          garden.weather.dailyWeather, water.snowWaterContent_mm,
          garden.weather.GetMeanMonthlyMeanMeanTempForYear_degC,
          params.soilInsulationFromAirTempCoeffs, movement);
  except on e: Exception do errorMessage('Exception in soil SoilTemperature: ' + e.message); end;
  end;

procedure GsSoilPatch.WindErosion(theMonth: integer);
  begin
  try
    if optionIsTrue(kSoilOptionsAllowWindErosion) 
      and (params.windErosionAccelerationFactor > 0.0)
      and (water.snowWaterContent_mm <= 0.0)
      and (garden.weather.dailyWeather.meanWindSpeedForDay_mPsec >= 7.5) then
    begin
    erosion.windErosion_tPha := EP.WindErosionForDay_tPha(params, garden.weather.dailyWeather, surface, plantMeans,
        layers, erosion, garden.weather.GetDailyMeanWindSpeedForMonth_mPsec(theMonth));
    erosion.windErosion_tPha := erosion.windErosion_tPha * params.windErosionAccelerationFactor;
    erosion.totalErosion_tPha := erosion.totalErosion_tPha + erosion.windErosion_tPha;
    end
  else
    erosion.windErosion_tPha := 0;
  except on e: Exception do errorMessage('Exception in soil WindErosion: ' + e.message); end;
  end;

procedure GsSoilPatch.Snowmelt;
  begin
  try
    if optionIsTrue(kSoilOptionsAllowSnowMelt) and (water.snowWaterContent_mm > 0.0) then
      begin
      water.snowmeltForDay_mm := EP.Snowmelt_mm(surface.temp_degC,
          garden.weather.dailyWeather.meanTempForDay_degC, water.snowWaterContent_mm);
      { add the snowmelt to the rainfall and subtract it from the snow water content }
      if (water.snowmeltForDay_mm > 0.0) then
        begin
        addQuantity(water.patchRainfallForDay_mm, water.snowmeltForDay_mm);
        subtractQuantity(water.snowWaterContent_mm, water.snowmeltForDay_mm);
        end;
      end
    else
      water.snowmeltForDay_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil Snowmelt: ' + e.message); end;
  end;

procedure GsSoilPatch.RunoffAndWaterErosionFromRainfall(theMonth: integer);
  var allowEitherRunoffOrErosion: boolean;
  begin
  try
  { combined boolean for processes used by both }
  allowEitherRunoffOrErosion := optionIsTrue(kSoilOptionsAllowRunoff)
      or optionIsTrue(kSoilOptionsAllowWaterErosionFromRunoff);
  allowEitherRunoffOrErosion := allowEitherRunoffOrErosion and (water.patchRainfallForDay_mm > 0.0);
  { parameters used by rainfall runoff and erosion }
  if allowEitherRunoffOrErosion then
    EP.RunoffAndErosionParams(water, erosion, garden.weather.GetMeanPropRainInFirstHalfHourForMonth_frn(theMonth))
  else
    begin
    { these are the variables set in RunoffAndErosionParams }
    water.peakRainfallRate_mmPhr := 0.0;
    erosion.rainfallEnergyFactorForUSLE := 0.0;
    water.rainfallDuration_hr := 0.0;
    end;
  { runoff and rainfall don't go into the soil until percolation }
  if optionIsTrue(kSoilOptionsAllowRunoff) then
    water.runoffVolume_mm := EP.RunoffVolume_mm(state.numLayers, layers, params, water, surface,
        plantMeans.meanHeight_m)
  else
    begin
    { these are the variables set in RunoffVolume_mm }
    water.runoffVolume_mm := 0.0;
    end;
  if allowEitherRunoffOrErosion then
    water.peakRunoffRate_mmPhr := EP.PeakRunoffRate_m3Psec(params, water)
  else
    begin
    { these are the variables set in PeakRunoffRate_m3Psec }
    { do not set timeOfConc_hr to zero if there is no peak runoff rate calculated, as the value is to be held over
      until tomorrow }
    {water.timeOfConc_hr := 0.0;}
    water.peakRunoffRate_mmPhr := 0.0;
    end;
  if optionIsTrue(kSoilOptionsAllowWaterErosionFromRunoffFromIrrigation) then
    begin
   erosion.waterErosion_tPha := EP.WaterErosionForDay_tPha(params, erosion,
        plantMeans.meanMinCropManagementFactor, layers, water, params.userCoeffsForMUSI,
        totals.patchTotalAboveGroundBiomassAndResidue_tPha);
    erosion.totalErosion_tPha := erosion.totalErosion_tPha + erosion.waterErosion_tPha;
    end
  else
    begin
    { these are the variables set in WaterErosionForDay_tPha }
    erosion.waterErosion_tPha := 0.0;
    end;
  if allowEitherRunoffOrErosion then
    erosion.enrichmentRatioForNPPest := EP.EnrichmentRatioForNPPest(water, erosion)
  else
    begin
    { these are the variables set in EnrichmentRatioForNPPest }
    { this must be set to 1.0 if it is not calculated }
    erosion.enrichmentRatioForNPPest := 1.0;
    end;
  except on e: Exception do errorMessage('Exception in soil RunoffAndWaterErosionFromRainfall: ' + e.message); end;
  end;

procedure GsSoilPatch.Snowfall;
  begin
  try
    if optionIsTrue(kSoilOptionsAllowPrecipitationToReachPatch) then
      begin
      { rainfall was snowfall. add to snow on patch }
      water.snowfallForDay_mm := water.patchRainfallForDay_mm;
      addQuantity(water.snowWaterContent_mm, water.snowfallForDay_mm);
      water.patchRainfallForDay_mm := 0.0;
      end
    else
      water.snowfallForDay_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil Snowfall: ' + e.message); end;
  end;

procedure GsSoilPatch.AdjustRidgesDikesForRainfall;
  var
    rainSettlingMultiplier: single;
  begin
  try
    if optionIsTrue(kSoilOptionsAllowRidgeSettlingFromRainfall) and (water.patchRainfallForDay_mm > 0.0) then
      begin
      { adjust ridge height, dike height and random roughness for rainfall (not irrigation) }
      rainSettlingMultiplier := EQ.RainSettlingMultiplier(water.patchRainfallForDay_mm, layers);
      surface.ridgeHeight_mm := surface.ridgeHeight_mm * rainSettlingMultiplier;
      surface.dikeHeight_mm := surface.dikeHeight_mm * rainSettlingMultiplier;
      surface.randomRoughness_mm := surface.randomRoughness_mm * rainSettlingMultiplier;
      end;
  except on e: Exception do errorMessage('Exception in soil AdjustRidgesDikesForRainfall: ' + e.message); end;
  end;

procedure GsSoilPatch.NitrateEnteringSoilInRainfall;
  begin
  try
    if optionIsTrue(kSoilOptionsAllowNitrateToEnterSoilInRainfall)
        and (water.patchRainfallForDay_mm > 0.0) then
      { calculate N entering the soil in rainfall }
      { g/m3 * mm * 10 m3/mm / 1000 g/kg = g/m3 / 100 = kg/ha }
      movement.nInPatchRainfallForDay_kgPha := params.avgNConcInRainfall_gPm3 / 100.0 * water.patchRainfallForDay_mm
    else
      movement.nInPatchRainfallForDay_kgPha := 0.0;
  except on e: Exception do errorMessage('Exception in soil NitrateEnteringSoilInRainfall: ' + e.message); end;
  end;

procedure GsSoilPatch.NitrateEnteringSoilInIrrigation;
  begin
  try
    if optionIsTrue(kSoilOptionsAllowNitrateToEnterSoilInIrrigationWater)
        and (water.patchIrrigationForDay_mm > 0.0) then
      begin
      { add nitrate in irrigation water (yesterday's) to soil }
      { g/m3 * mm * 10 m3/mm / 1000 g/kg = g/m3 / 100 = kg/ha }
      movement.nInIrrigationForDay_kgPha := max(0.0,
        min(water.patchIrrigationForDay_mm * params.nitrateConcInIrrigationWater_gPm3 / 100.0,
        plantMeans.minimumMaxAnnualNFert_kgPha -
        (yearTotals.cumNFertAutoApplied_kgPha + yearTotals.cumNAddedInIrrigationWater_kgPha)));
      addQuantity(layers[0].nitrate_kgPha, movement.nInIrrigationForDay_kgPha);
      addQuantity(yearTotals.cumNAddedInIrrigationWater_kgPha, movement.nInIrrigationForDay_kgPha);
      end
    else
      begin
      movement.nInIrrigationForDay_kgPha := 0.0;
      end;
  except on e: Exception do errorMessage('Exception in soil NitrateEnteringSoilInIrrigation: ' + e.message); end;
  end;

procedure GsSoilPatch.GetTodaysIrrigationFromYesterdaysAutoAndTodaysUser;
  begin
  try
  { add yesterday's auto irrigation to today's patch irrigation to carry over, reset auto irrigation }
  water.patchIrrigationForDay_mm := water.autoIrrigationForDay_mm;
  water.autoIrrigationForDay_mm := 0.0;
  { add user irrigation to patch irrigation }
  if (water.userIrrigationForDay_mm > 0.0) then
    addQuantity(water.patchIrrigationForDay_mm, water.userIrrigationForDay_mm);
  except on e: Exception do
    errorMessage('Problem in soil GetTodaysIrrigationFromYesterdaysAutoAndTodaysUser: ' + e.message); end;
  end;

procedure GsSoilPatch.RunoffAndWaterErosionFromIrrigation;
  var
    layer: integer;
    irrigationVolume_mm, furrowVolume_mm, distance_m,
      slopeLengthFactorParam, slopeLengthAndSteepnessFactor,
      soilErodibilityFactor: single;
  begin
  try
  { runoff }
  if optionIsTrue(kSoilOptionsAllowRunoffFromIrrigation) then
    begin
    irrigationVolume_mm := water.autoIrrigationForDay_mm + water.userIrrigationForDay_mm;
    water.irrigationRunoffVolume_mm := EQ.IrrigationRunoffVolume_mm(irrigationVolume_mm,
        params.fractionOfIrrigationWaterLostToRunoff);
    end
  else
    water.irrigationRunoffVolume_mm := 0.0;
  { water erosion }
  if optionIsTrue(kSoilOptionsAllowWaterErosionFromRunoffFromIrrigation) then
    begin
    water.irrigationPeakRunoffRate_m3Psec := EQ.IrrigationPeakRunoffRate_m3Psec(params, surface,
        water.irrigationRunoffVolume_mm, furrowVolume_mm, distance_m);
    slopeLengthFactorParam := EQ.SlopeLengthFactorParam(params.watershedSlopeSteepness_mPm);
    slopeLengthAndSteepnessFactor := EQ.SlopeLengthAndSteepnessFactor(params.watershedSlopeLength_m,
        slopeLengthFactorParam, params.watershedSlopeSteepness_mPm);
    soilErodibilityFactor := EQ.SoilErodibilityFactor(layers[0].sandContent_pct, layers[0].siltContent_pct,
        layers[0].clayContent_pct, layers[0].organicMatter_tPha, layers[0].weight_tPha);
    erosion.irrigationWaterErosion_tPha := EQ.WaterErosionFromIrrigation_tPha(furrowVolume_mm,
        water.irrigationPeakRunoffRate_m3Psec, soilErodibilityFactor,
        params.erosionControlPracticeFactor, slopeLengthAndSteepnessFactor, distance_m);
    end
  else
    begin
    water.irrigationPeakRunoffRate_m3Psec := 0.0;
    erosion.irrigationWaterErosion_tPha := 0.0;
    end;
  except on e: Exception do errorMessage('Exception in soil RunoffAndWaterErosionFromIrrigation: ' + e.message); end;
  end;

procedure GsSoilPatch.PercolationAndLateralFlow;
  var
    layer, layerToUseForDrainageSystem: integer;
    inflowingWater_mm, totalRunoff_mm: single;
    allowPercolation, allowLateralFlow, completed: boolean;
  begin
  try
  completed := false;
  allowPercolation := optionIsTrue(kSoilOptionsAllowPercolation);
  allowLateralFlow := optionIsTrue(kSoilOptionsAllowLateralFlow);
  if allowPercolation or allowLateralFlow then
    begin
    inflowingWater_mm := water.patchRainfallForDay_mm + water.patchIrrigationForDay_mm;
    totalRunoff_mm := water.runoffVolume_mm + water.irrigationRunoffVolume_mm;
    if (inflowingWater_mm > totalRunoff_mm) then
      begin
      { get layer for drainage if there is a drainage system installed }
      if optionIsTrue(kSoilOptionsAutoMaintainDrainageSystem) then
        layerToUseForDrainageSystem := params.layerWithDrainageSystem
      else
        layerToUseForDrainageSystem := -1;
      { adjust return flow travel time if there is a drainage system installed }
      if layerToUseForDrainageSystem >= 0 then
        water.returnFlowTravelTime_days := max(params.timeForDrainageSystemToReducePlantStress_days,
            params.paramForReturnFlowTravelTime_days)
      else
        water.returnFlowTravelTime_days := params.paramForReturnFlowTravelTime_days;
      EP.PercAndLatFlowByLayer_mm(state.numLayers, layerToUseForDrainageSystem,
          params, water, layers, inflowingWater_mm, totalRunoff_mm, allowPercolation, allowLateralFlow);
      completed := true;
      end;
    end;
  if not completed then
    if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
      begin
      layers[layer].lateralFlow_mm := 0.0;
      layers[layer].percolation_mm := 0.0;
      end;
  except on e: Exception do errorMessage('Exception in soil PercolationAndLateralFlow: ' + e.message); end;
  end;

procedure GsSoilPatch.PotentialSoilEvaporation;
  var prevThreeDaysMeanTemp_degC: arrayThree;
  begin
  try
  if optionIsTrue(kSoilOptionsAllowSoilEvaporation) then
    begin
    garden.weather.fillPrevThreeDaysMeanTempArray_degC(prevThreeDaysMeanTemp_degC);
    totals.patchTotalPotSoilEvap_mm := EP.PotentialSoilEvap_mm(params, garden.weather.dailyWeather,
        garden.weather.stationVars, prevThreeDaysMeanTemp_degC,
        surface.albedo_frn, plantMeans.meanHeight_m, self.GetNumActivePlants);
    end
  else
    totals.patchTotalPotSoilEvap_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil PercolationAndLateralFlow: ' + e.message); end;
  end;

procedure GsSoilPatch.PotentialPlantTranspiration;
  var
    i: longint;
    plant: GsPlant;
    prevThreeDaysMeanTemp_degC: arrayThree;
  begin
  try
  if (self.GetNumActivePlants > 0) then
    begin
    if optionIsTrue(kSoilOptionsAllowPlantTranspiration) then
      begin
      garden.weather.fillPrevThreeDaysMeanTempArray_degC(prevThreeDaysMeanTemp_degC);
      totals.patchTotalPotPlantEvap_mm := EP.PotentialPlantEvap_mm(params, garden.weather.dailyWeather,
        garden.weather.stationVars, prevThreeDaysMeanTemp_degC,
        surface.albedo_frn, totals.patchTotalPotSoilEvap_mm, plantList);
      end
    else
      begin
      for i := 0 to plantList.count - 1 do
        begin
        plant := GsPlant(plantList.items[i]);
        if not plant.awaitingReseedingOrInStasisBox then
          plant.water.potPlantEvap_mm := 0.0;
        end;
      totals.patchTotalPotPlantEvap_mm := 0.0;
      end;
    end
  else  { no active plants }
    totals.patchTotalPotPlantEvap_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil PotentialPlantTranspiration: ' + e.message); end;
  end;

procedure GsSoilPatch.ActualSoilEvaporation;
  var
    layer: integer;
    potSoilEvapAdjByCoverAndPotPlantEvap_mm: single;
  begin
  try
  if totals.patchTotalPotSoilEvap_mm = 0.0 then
    begin
    totals.patchTotalSoilEvap_mm := 0.0;
    if state.numLayers > 0 then for layer := 0 to  state.numLayers - 1 do
      layers[layer].evaporation_mm := 0.0;
    water.lowestLayerInWhichSoilEvapOccured := 0;
    end
  else
    begin
    { modify potential evapotranspiration for soil cover and plant potential evap }
    potSoilEvapAdjByCoverAndPotPlantEvap_mm :=
      EP.PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm(surface.soilCoverIndex_frn, totals.patchTotalPotSoilEvap_mm,
      totals.patchTotalPotPlantEvap_mm);
    totals.patchTotalSoilEvap_mm := EP.ActualSoilEvaporation_mm(state.numLayers, layers, water.snowWaterContent_mm,
      potSoilEvapAdjByCoverAndPotPlantEvap_mm, state.plowDepth_m,
      params.lowerLimitWaterContentInTopP5MAsFractOfWP_frn, params.soilEvapByDepthCoeffs,
      water.lowestLayerInWhichSoilEvapOccured);
    end;
  except on e: Exception do errorMessage('Exception in soil ActualSoilEvaporation: ' + e.message); end;
  end;

procedure GsSoilPatch.UpdateWaterHistoryArray;
  var inflowMinusRunoff_mm: single;
  begin
  try
  { update 30-day moving windows of rainfall-runoff and pot soil evap (must be done before water table function) }
  inflowMinusRunoff_mm := (water.patchRainfallForDay_mm + water.patchIrrigationForDay_mm) -
    (water.runoffVolume_mm + water.irrigationRunoffVolume_mm);
  totals.sumRainfallMinusRunoffPrev30Days_mm := Utils_AddDayTo30DaysArray_mm(totals.rainfallMinusRunoff30DaysArray_mm,
    inflowMinusRunoff_mm);
  totals.sumPotSoilEvapPrev30Days_mm := Utils_AddDayTo30DaysArray_mm(totals.potSoilEvap30DaysArray_mm,
    totals.patchTotalPotSoilEvap_mm);
  except on e: Exception do errorMessage('Exception in soil UpdateWaterHistoryArray: ' + e.message); end;
  end;

procedure GsSoilPatch.WaterAllocationToPlants;
  var
    i, layer: integer;
    oldPatchTotalPotPlantEvap_mm, totalPotMinusActualSoil_mm, totalWaterAvailable_mm: single;
    waterDemand_mm, waterAvailable_mm, waterUsed_mm: arraySoilLayers;
    plant: GsPlant;
  begin
  try
  oldPatchTotalPotPlantEvap_mm := totals.patchTotalPotPlantEvap_mm;
  { adjust patchTotalPotPlantEvap_mm for actual soil evaporation }
  { change from EPIC: in EPIC code, this equation is
    totals.patchTotalPotPlantEvap_mm := max(0.0,min(totals.patchTotalPotSoilEvap_mm - totals.patchTotalSoilEvap_mm,
      totals.patchTotalPotPlantEvap_mm));
    This is because of an earlier statement setting patchTotalPotSoilEvap_mm to patchTotalPotPlantEvap_mm
    if patchTotalPotPlantEvap_mm is greater. We are not doing this.}
  { restrict potential plant evap to amount left after actual soil evap }
  { don't do this if plant evap is turned off }
  if optionIsTrue(kSoilOptionsAllowPlantTranspiration) then
    with totals do
      begin
      totalPotMinusActualSoil_mm := patchTotalPotSoilEvap_mm + patchTotalPotPlantEvap_mm - patchTotalSoilEvap_mm;
      patchTotalPotPlantEvap_mm := min(totalPotMinusActualSoil_mm, patchTotalPotPlantEvap_mm);
      patchTotalPotPlantEvap_mm := max(0.0, patchTotalPotPlantEvap_mm);
      end;
  if (self.GetNumActivePlants > 0) then
    begin
    { figure out how much water is available to plants in each soil layer based on patchTotalPotPlantEvap_mm }
    { should check SW - WP, not just SW, because plants can't use any water below WP }
    totalWaterAvailable_mm := 0.0;
    if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
      begin
      waterAvailable_mm[layer] := (layers[layer].waterContent_mm - layers[layer].wiltingPoint_mm) * 0.9;
      totalWaterAvailable_mm := totalWaterAvailable_mm + waterAvailable_mm[layer];
      end;
    if totals.patchTotalPotPlantEvap_mm > totalWaterAvailable_mm then
      totals.patchTotalPotPlantEvap_mm := totalWaterAvailable_mm;
    { adjust potential evap of each plant to reflect any change in patchTotalPotPlantEvap_mm }
    if (oldPatchTotalPotPlantEvap_mm <> 0)
      and (totals.patchTotalPotPlantEvap_mm <> oldPatchTotalPotPlantEvap_mm) then
      for i := 0 to plantList.count-1 do
        begin
        plant := GsPlant(plantList.items[i]);
        if plant.awaitingReseedingOrInStasisBox then continue;
        plant.water.potPlantEvap_mm := plant.water.potPlantEvap_mm
          * safediv(totals.patchTotalPotPlantEvap_mm, oldPatchTotalPotPlantEvap_mm);
        end;
    { ask each plant to calculate its water demand on each soil layer; add up all demands on each soil layer }
    if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do waterDemand_mm[layer] := 0.0;
    for i := 0 to plantList.count-1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      plant.WaterDemand_mm;
      if plant.biomass.numLayersWithRootsInThem > 0 then
        for layer := 0 to plant.biomass.numLayersWithRootsInThem - 1 do
          addQuantity(waterDemand_mm[layer], plant.water.waterDemandByLayer_mm[layer]);
      end;
    { allocate available water in each layer to plants }
    { no weighting scheme is needed, because plant water demand takes root depth, etc. into account }
    if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do waterUsed_mm[layer] := 0.0;
    for i := 0 to plantList.count-1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      plant.water.totalWaterUse_mm := 0.0;
      if plant.biomass.numLayersWithRootsInThem > 0 then
        for layer := 0 to plant.biomass.numLayersWithRootsInThem - 1 do
          if (waterDemand_mm[layer] > 0.0) and (waterAvailable_mm[layer] > 0.0) then
            begin
            { change from EPIC - pdf - added test in case available is greater than demand}
            if waterAvailable_mm[layer] < waterDemand_mm[layer] then
            	plant.water.waterUseByLayer_mm[layer] := plant.water.waterDemandByLayer_mm[layer]
              	* safediv(waterAvailable_mm[layer], waterDemand_mm[layer])
            else
              plant.water.waterUseByLayer_mm[layer] := plant.water.waterDemandByLayer_mm[layer];
            checkUptakeAgainstAvailable(plant.water.waterUseByLayer_mm[layer],
              waterUsed_mm[layer], waterAvailable_mm[layer]);
            addQuantity(plant.water.totalWaterUse_mm, plant.water.waterUseByLayer_mm[layer]);
            addQuantity(waterUsed_mm[layer], plant.water.waterUseByLayer_mm[layer]);
            end
          else { no demand or none available }
            plant.water.waterUseByLayer_mm[layer] := 0.0;
      end;
    { remove water from soil for each layer used by each plant }
    totals.patchTotalSoilEvapAndPlantWaterUse_mm := 0.0;
    for i := 0 to plantList.count-1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      if plant.biomass.numLayersWithRootsInThem > 0 then
        for layer := 0 to plant.biomass.numLayersWithRootsInThem - 1 do
          begin
          subtractQuantity(layers[layer].waterContent_mm, plant.water.waterUseByLayer_mm[layer]);
          addQuantity(totals.patchTotalSoilEvapAndPlantWaterUse_mm, plant.water.waterUseByLayer_mm[layer]);
          end;
      end;
    addQuantity(totals.patchTotalSoilEvapAndPlantWaterUse_mm, totals.patchTotalSoilEvap_mm);
    end
  else { no plants }
    begin
    totals.patchTotalSoilEvapAndPlantWaterUse_mm := totals.patchTotalSoilEvap_mm;
    end;
  except on e: Exception do errorMessage('Exception in soil WaterAllocationToPlants: : ' + e.message); end;
  end;

procedure GsSoilPatch.WaterTableDynamics;
  begin
  try
  if optionIsTrue(kSoilOptionsUpdateWaterTableDepth)
      and (params.waterTableMinDepth_m < layers[state.numLayers-1].depth_m) then
      EP.WaterTableDynamics(water, params, state.numLayers, layers, totals)
  else
    water.inflowFromWaterTable_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil WaterTableDynamics: ' + e.message); end;
  end;

procedure GsSoilPatch.RebuildDikesIfNecessary;
  begin
  try
  if optionIsTrue(kSoilOptionsAutoMaintainFurrowDikes) then
    begin
    if surface.dikesNeedRebuilt then
      begin
      surface.dikeHeight_mm := surface.dikeHeightAtCreation_mm;
      surface.dikesNeedRebuilt := false;
      end
    end
  else
    surface.dikeHeight_mm := 0.0;
  except on e: Exception do errorMessage('Exception in soil RebuildDikesIfNecessary: ' + e.message); end;
  end;

procedure GsSoilPatch.AutoIrrigateIfNecessary;
  var
    i: smallint;
    minWaterStressFactor: single;
    plant: GsPlant;
  begin
  try
  { AUTO IRRIGATE }
  inc(state.daysSinceAutoIrrApplied);
  water.autoIrrigationForDay_mm := 0.0;
  if optionIsTrue(kSoilOptionsAutoIrrigation) and (water.patchRainfallForDay_mm = 0.0)
    and (water.userIrrigationForDay_mm = 0.0) and (plantMeans.meanLeafAreaIndex > 0.0) then
    begin
    { figure out smallest water stress factor (worst) among all plants in patch }
    minWaterStressFactor := 10000;
    if plantList.count > 0 then for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      if (plant.constraints.waterStressFactor_frn < minWaterStressFactor) then
        minWaterStressFactor := plant.constraints.waterStressFactor_frn;
      end;
    if EP.AutoIrrigationIsNeeded(params.waterStressFactorForAutoIrr, minWaterStressFactor,
        state.numLayers, layers, totals.soilProfileDepthSWmWP_mm, totals.soilProfileDepthFCmWP_mm) then
      AutoIrrigate;
    end;
  except on e: Exception do errorMessage('Exception in soil AutoIrrigateIfNecessary: ' + e.message); end;
  end;

procedure GsSoilPatch.NutrientCycling;
  var
    layer: integer;
    nitrateEnteringLayer_kgPha, labilePEnteringLayer_kgPha, depthLastLayer_m: single;
    allowLabileAndMineralPFlow, allowActiveAndStableMineralPFlow,
      allowNLossInRunoff, allowPLossInRunoff, allowNLossInLateralFlow, allowNLossInPercolation, allowPLossInPercolation,
      allowNitrification, allowVolatilization, allowActiveHumusNMineralization, allowActiveHumusPMineralization,
      allowFreshNMineralization, allowFreshPMineralization: boolean;
  begin
  try
  nitrateEnteringLayer_kgPha := movement.nInPatchRainfallForDay_kgPha + movement.nInIrrigationForDay_kgPha;
  labilePEnteringLayer_kgPha := 0.0;
  depthLastLayer_m := 0.0;
  { N AND P CYCLING }
  { n entering first layer was calculated before percolation and lateral flow were calculated }
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    { N movement between forms of humus N }
    if optionIsTrue(kSoilOptionsAllowActiveAndStableHumusNFlow) then
      EP.NFluxBetweenActiveAndStableHumus(layer, layers)
    else
      layers[layer].organicNFromActiveToStableInHumus_kgPha := 0.0;
    { P movement between labile and mineral P, and between active and stable mineral P }
    allowLabileAndMineralPFlow := optionIsTrue(kSoilOptionsAllowLabileAndMineralPFlow);
    allowActiveAndStableMineralPFlow := optionIsTrue(kSoilOptionsAllowActiveAndStableMineralPFlow);
    EP.PFluxBetweenLabileAndMineral(layer, layers, allowLabileAndMineralPFlow, allowActiveAndStableMineralPFlow);
    { N and P loss in runoff, percolation, and lateral flow }
    allowNLossInRunoff := optionIsTrue(kSoilOptionsAllowNitrateLossInRunoff);
    allowPLossInRunoff := optionIsTrue(kSoilOptionsAllowLabilePLossInRunoff);
    allowNLossInLateralFlow := optionIsTrue(kSoilOptionsAllowNitrateLossInLateralFlow);
    allowNLossInPercolation := optionIsTrue(kSoilOptionsAllowNitrateLossInPercolation);
    allowPLossInPercolation := optionIsTrue(kSoilOptionsAllowLabilePLossInPercolation);
    if (layer = 0) then
      EP.NAndPLossInRunoffAndLeachingForSurfaceLayer(layers, nitrateEnteringLayer_kgPha, labilePEnteringLayer_kgPha,
        params.nitrogenLeachingParam, water, movement,
        allowNLossInRunoff, allowPLossInRunoff, allowNLossInLateralFlow,
        allowNLossInPercolation, allowPLossInPercolation)
    else
      EP.LeachNandPNonSurfaceLayer(layer, layers, nitrateEnteringLayer_kgPha, labilePEnteringLayer_kgPha,
        params.nitrogenLeachingParam, allowNLossInPercolation, allowPLossInPercolation,
        allowNLossInLateralFlow);
    { N nitrification and volatilization }
    allowNitrification := optionIsTrue(kSoilOptionsAllowNitrification);
    allowVolatilization := optionIsTrue(kSoilOptionsAllowVolatilization);
    if (layers[layer].ammonia_kgPha > 0.01) then
      EP.NitrificationAndVolatilization(layer, layers, garden.weather.dailyWeather.meanWindSpeedForDay_mPsec,
        params.nVolatilizationByDepthCoeffs, allowNitrification, allowVolatilization)
    else
      begin
      layers[layer].nitrification_kgPha := 0.0;
      layers[layer].volatilization_kgPha := 0.0;                
      end;
    { mineralization, converting standing dead to flat residue, and denitification occur only if layer is above freezing }
    if (layers[layer].temperature_degC > 0.0) then
      begin
      layers[layer].nutrientCyclingTempFactor := EQ.NutrientCyclingTempFactorForLayer(layers[layer].temperature_degC);
      if (depthLastLayer_m < state.soilProfileDepth_m) then 
        begin                                             
        allowActiveHumusNMineralization := optionIsTrue(kSoilOptionsAllowActiveHumusNMineralization);
        allowActiveHumusPMineralization := optionIsTrue(kSoilOptionsAllowActiveHumusPMineralization);
        allowFreshNMineralization := optionIsTrue(kSoilOptionsAllowFreshNMineralization);
        allowFreshPMineralization := optionIsTrue(kSoilOptionsAllowFreshPMineralization);
        EP.Mineralization(layer, layers, totals.plowDepthSettledBulkDensityAtInput_tPm3, movement,
          allowActiveHumusNMineralization, allowActiveHumusPMineralization,
          allowFreshNMineralization, allowFreshPMineralization);
        if optionIsTrue(kSoilOptionsAllowFlatCropResidueToDecay) then
          GS.DailyFlatCropResidueDecay(state.numLayers, layers, movement)
        else
          begin
          movement.flatCropResidueDecomposedToAtmosphere_tPha := 0.0;
          movement.flatCropResidueDecomposedToOM_tPha := 0.0;
          end;
        if optionIsTrue(kSoilOptionsAllowStandingDeadToDecayToResidue) then
          begin
          if (layer = 0) and (plantMeans.totalStandingDead_tPha + mulch.flatCropResidue_tPha > 0.001) then
            begin
            EP.DailyStandingDeadToFlatResidue(layers, mulch,
                (water.patchRainfallForDay_mm + water.patchIrrigationForDay_mm), plantList, plantMeans);
            end;
          end;
        end
      else
        begin
        layers[layer].activeHumusNMineralization_kgPha := 0.0;
        layers[layer].freshNMineralization_kgPha := 0.0;
        layers[layer].activeHumusPMineralization_kgPha := 0.0;
        layers[layer].freshPMineralization_kgPha := 0.0;
        end;
      end;
    if optionIsTrue(kSoilOptionsAllowDenitrification) then
      EP.Denitrification(layer, layers)
    else
      layers[layer].denitrification_kgPha := 0.0;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  { sum nutrient cycling numbers for output }
  movement.organicNFromActiveToStableInHumus_kgPha := 0.0;
  movement.pFlowFromLabileToActiveMineral_kgPha := 0.0;
  movement.nitrateLeachedFromPercolation_kgPha := 0.0;
  movement.nitrateLeachedFromLateralFlow_kgPha := 0.0;
  movement.labilePLeachedFromPercolation_kgPha := 0.0;
  movement.nitrification_kgPha := 0.0;
  movement.volatilization_kgPha := 0.0;
  movement.activeHumusNMineralization_kgPha := 0.0;
  movement.activeHumusPMineralization_kgPha := 0.0;
  movement.freshNMineralization_kgPha := 0.0;
  movement.freshPMineralization_kgPha := 0.0;
  movement.denitrification_kgPha := 0.0;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    { these could be negative }
    movement.organicNFromActiveToStableInHumus_kgPha := movement.organicNFromActiveToStableInHumus_kgPha
        + layers[layer].organicNFromActiveToStableInHumus_kgPha;
    movement.pFlowFromLabileToActiveMineral_kgPha := movement.pFlowFromLabileToActiveMineral_kgPha
        + layers[layer].pFlowFromLabileToActiveMineral_kgPha;
    movement.mineralPFlowFromActiveToStable_kgPha := movement.mineralPFlowFromActiveToStable_kgPha
        + layers[layer].mineralPFlowFromActiveToStable_kgPha;
    addQuantity(movement.nitrateLeachedFromLateralFlow_kgPha, layers[layer].nitrateLeachedFromLateralFlow_kgPha);
    addQuantity(movement.nitrification_kgPha, layers[layer].nitrification_kgPha);
    addQuantity(movement.volatilization_kgPha, layers[layer].volatilization_kgPha);
    addQuantity(movement.activeHumusNMineralization_kgPha, layers[layer].activeHumusNMineralization_kgPha);
    addQuantity(movement.activeHumusPMineralization_kgPha, layers[layer].activeHumusPMineralization_kgPha);
    addQuantity(movement.freshNMineralization_kgPha, layers[layer].freshNMineralization_kgPha);
    addQuantity(movement.freshPMineralization_kgPha, layers[layer].freshPMineralization_kgPha);
    addQuantity(movement.denitrification_kgPha, layers[layer].denitrification_kgPha);
    end;
  addQuantity(movement.nitrateLeachedFromPercolation_kgPha, layers[state.numLayers - 1].nitrateLeachedFromPercolation_kgPha);
  addQuantity(movement.labilePLeachedFromPercolation_kgPha, layers[state.numLayers - 1].labilePLeachedFromPercolation_kgPha);
  except on e: Exception do errorMessage('Exception in soil NutrientCycling: ' + e.message); end;
  end;

procedure GsSoilPatch.checkUptakeAgainstAvailable(var uptakeAmount: single; totalSoFar, amountAvailable: single);
  begin
  { check that uptake doesn't exceed available because of rounding errors while adding multiplied amounts }
  if totalSoFar + uptakeAmount > amountAvailable then
    uptakeAmount := amountAvailable - totalSoFar;
  if uptakeAmount < kLowestQuantityFloat then
    uptakeAmount := 0.0;
  end;

procedure GsSoilPatch.NutrientAllocationToPlants;
  var
    i, layer: integer;
    nRatio, pRatio: single;
    totalNDemand_kgPha, totalNUptake_kgPha, totalPDemand_kgPha, totalPUptake_kgPha: arraySoilLayers;
    plant: GsPlant;
  begin
  try
  if (plantList.count <= 0) then exit;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    totalNDemand_kgPha[layer] := 0.0;
    totalPDemand_kgPha[layer] := 0.0;
    totalNUptake_kgPha[layer] := 0.0;
    totalPUptake_kgPha[layer] := 0.0;
    end;
  { ask plants to calculate their demands for N and P for each layer }
  for i := 0 to plantList.count - 1 do
    begin
    plant := GsPlant(plantList.items[i]);
    if plant.awaitingReseedingOrInStasisBox then continue;
    {N}
    plant.NDemandAndFixation;
    if plant.biomass.numLayersWithRootsInThem > 0 then
      for layer := 0 to plant.biomass.numLayersWithRootsInThem - 1 do
        addQuantity(totalNDemand_kgPha[layer], plant.nutrients.nDemandByLayer_kgPha[layer]);
    {P}
    plant.PDemand;
    if plant.biomass.numLayersWithRootsInThem > 0 then
      for layer := 0 to plant.biomass.numLayersWithRootsInThem - 1 do
        addQuantity(totalPDemand_kgPha[layer], plant.nutrients.pDemandByLayer_kgPha[layer]);
    end;
  { allocate available N and P to plants by layer }
  for i := 0 to plantList.count - 1 do
    begin
    plant := GsPlant(plantList.items[i]);
    if plant.awaitingReseedingOrInStasisBox then continue;
    plant.nutrients.nUptake_kgPha := 0.0;
    plant.nutrients.pUptake_kgPha := 0.0;
    if plant.biomass.numLayersWithRootsInThem > 0 then
      begin
      for layer := 0 to plant.biomass.numLayersWithRootsInThem - 1 do
        begin
        {N}
        if (totalNDemand_kgPha[layer] > 0.0) and (layers[layer].nitrate_kgPha > 0.0) then
          begin
          if optionIsTrue(kSoilOptionsAllowNUptakeByPlants) then
            nRatio := min(1.0, safediv(layers[layer].nitrate_kgPha, totalNDemand_kgPha[layer]))
          else
            nRatio := 0.0;
          plant.nutrients.nUptakeByLayer_kgPha[layer] := plant.nutrients.nDemandByLayer_kgPha[layer] * nRatio;
          checkUptakeAgainstAvailable(plant.nutrients.nUptakeByLayer_kgPha[layer],
              totalNUptake_kgPha[layer], layers[layer].nitrate_kgPha);
          addQuantity(plant.nutrients.nUptake_kgPha, plant.nutrients.nUptakeByLayer_kgPha[layer]);
          totalNUptake_kgPha[layer] := totalNUptake_kgPha[layer] + plant.nutrients.nUptakeByLayer_kgPha[layer];
          end;
        {P}
        if (totalPDemand_kgPha[layer] > 0.0) and (layers[layer].labileP_kgPha > 0.0) then
          begin
          if optionIsTrue(kSoilOptionsAllowPUptakeByPlants) then
            pRatio := min(1.0, safediv(layers[layer].labileP_kgPha, totalPDemand_kgPha[layer]))
          else
            pRatio := 0.0;
          plant.nutrients.pUptakeByLayer_kgPha[layer] := plant.nutrients.pDemandByLayer_kgPha[layer] * pRatio;
          checkUptakeAgainstAvailable(plant.nutrients.pUptakeByLayer_kgPha[layer],
              totalPUptake_kgPha[layer], layers[layer].labileP_kgPha);
          addQuantity(plant.nutrients.pUptake_kgPha, plant.nutrients.pUptakeByLayer_kgPha[layer]);
          addQuantity(totalPUptake_kgPha[layer], plant.nutrients.pUptakeByLayer_kgPha[layer]);
          end;
        end;
      plant.FinishNUptake;
      plant.FinishPUptake;
      end;
    end;
  { take N and P out of soil by layer }
  movement.totalNUptakeByPlants_kgPha := 0.0;
  movement.totalPUptakeByPlants_kgPha := 0.0;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    {N}
    subtractQuantity(layers[layer].nitrate_kgPha, totalNUptake_kgPha[layer]);
    addQuantity(movement.totalNUptakeByPlants_kgPha, totalNUptake_kgPha[layer]);
    {P}
    subtractQuantity(layers[layer].labileP_kgPha, totalPUptake_kgPha[layer]);
    addQuantity(movement.totalPUptakeByPlants_kgPha, totalPUptake_kgPha[layer]);
    end;
  except on e: Exception do errorMessage('Exception in soil NutrientAllocationToPlants: ' + e.message); end;
  end;

procedure GsSoilPatch.PlantNextDayLoopBeforeWaterAndNutrientAllocation;
  var
    i: integer;
    plant: GsPlant;
  begin
  try
  if (plantList.count > 0) then
    for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      plant.PlantNextDayBeforeWaterAndNutrientAllocation;
      end;
  except on e: Exception do
    errorMessage('Exception in soil PlantNextDayLoopBeforeWaterAndNutrientAllocation: ' + e.message); end;
  end;

procedure GsSoilPatch.PlantNextDayLoopBetweenWaterAndNutrientAllocation;
  var
    i: integer;
    plant: GsPlant;
  begin
  try
  if (plantList.count > 0) then
    for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      plant.PlantNextDayBetweenWaterAndNutrientAllocation;
      end;
  except on e: Exception do
    errorMessage('Exception in soil PlantNextDayLoopBetweenWaterAndNutrientAllocation: ' + e.message); end;
  end;

procedure GsSoilPatch.PlantNextDayLoopAfterWaterAndNutrientAllocation;
  var
    i: integer;
    plant: GsPlant;
  begin
  try
  if (plantList.count > 0) then
    for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      plant.PlantNextDayAfterWaterAndNutrientAllocation;
      end;
  except on e: Exception do
    errorMessage('Exception in soil PlantNextDayLoopAfterWaterAndNutrientAllocation: ' + e.message); end;
  end;

procedure GsSoilPatch.AutoFertilizeIfNecessary;
  var
    i: integer;
    allLegumes, allOverMaxNFertForYear: boolean;
    minNStressFactor_frn: single;
    plant: GsPlant;
  begin
  try
  movement.nFertAutoApplied_kgPha := 0.0;
  movement.pFertAutoApplied_kgPha := 0.0;
  if optionIsTrue(kSoilOptionsAutoFertilization) and (plantList.count > 0)
      and (state.daysSinceAutoFertApplied >= params.minIntervalAutoFert_days) then
    begin
    allLegumes := true;
    allOverMaxNFertForYear := true;
    minNStressFactor_frn := 10.0;
    if (plantList.count > 0) then
      for i := 0 to plantList.count - 1 do
        begin
        plant := GsPlant(plantList.items[i]);
        if plant.awaitingReseedingOrInStasisBox then continue;
        { test each plant to see if requirements for fertilizing patch are met }
        { 1. don't do n fertilization if all plants in patch are legumes }
        { 2. don't fertilize if all plants in patch have had as much auto fert as allowed for year }
        { 3. figure minimum n stress factor (worst n stress) for patch }
        if (not plant.params.isLegume) then allLegumes := false;
        if (plant.constraints.nStressFactor_frn < minNStressFactor_frn) then
          minNStressFactor_frn := plant.constraints.nStressFactor_frn;
        if (yearTotals.cumNFertAutoApplied_kgPha + yearTotals.cumNAddedInIrrigationWater_kgPha <
            plant.params.maxAnnualNFert_kgPha) then
          allOverMaxNFertForYear := false;
        end;
    if (not allOverMaxNFertForYear)
        and (minNStressFactor_frn < params.nStressFactorToTriggerAutoFert) then
      AutoFertilize(not allLegumes); { if not all legumes, need N fertilization }
    end;
  except on e: Exception do errorMessage('Exception in soil AutoFertilizeIfNecessary: ' + e.message); end;
  end;

procedure GsSoilPatch.NutrientLossInSediment;
  var
    takeOrganicNActiveHumus, takeOrganicNStableHumus, takeNitrate, takeAmmonia, takeOrganicNFresh, takeOrganicMatter,
    takeFlatCropResidue, takeOrganicPHumus, takeLabileP, takeMineralPActive, takeMineralPStable, takeOrganicPFresh: boolean;
  begin
  try
  { N }
  takeOrganicNActiveHumus := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeOrganicNActiveHumus);
  takeOrganicNStableHumus := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeOrganicNStableHumus);
  takeNitrate := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeNitrate);
  takeAmmonia := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeAmmonia);
  takeOrganicNFresh := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeOrganicNFresh);
  takeOrganicMatter := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeOrganicMatter);
  takeFlatCropResidue := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeFlatCropResidue);
  EP.OrganicNAndCLossInSediment(erosion.totalErosion_tPha, erosion.enrichmentRatioForNPPest, layers, movement,
      takeOrganicNActiveHumus, takeOrganicNStableHumus, takeNitrate, takeAmmonia, takeOrganicNFresh,
      takeOrganicMatter, takeFlatCropResidue);
  { P }
  takeOrganicPHumus := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeOrganicPHumus);
  takeLabileP := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeLabileP);
  takeMineralPActive := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeMineralPActive);
  takeMineralPStable := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeMineralPStable);
  takeOrganicPFresh := optionIsTrue(kSoilOptionsAllowErodedSoilToTakeOrganicPFresh);
  EP.PLossInSediment(erosion.totalErosion_tPha, erosion.enrichmentRatioForNPPest, layers, movement,
      takeOrganicPHumus, takeLabileP, takeMineralPActive, takeMineralPStable, takeOrganicPFresh);
  except on e: Exception do errorMessage('Problem in soil NutrientLossInSediment: ' + e.message); end;
  end;

procedure GsSoilPatch.SoilSettlingFromRain;
  begin
  try
  if optionIsTrue(kSoilOptionsAllowSoilSettlingFromRain) then
    EQ.BulkDensityAfterSettlingFromRain_tPm3(state.numLayers, layers,
        (water.patchRainfallForDay_mm + water.patchIrrigationForDay_mm),
        (water.runoffVolume_mm + water.irrigationRunoffVolume_mm),
        state.maxTillageDepth_m, params.soilSettlingFromRainfallCoeffs);
  except on e: Exception do errorMessage('Exception in soil SoilSettlingFromRain: ' + e.message); end;
  end;

procedure GsSoilPatch.NitratesMovedUpFromEvaporation;
  begin
  try
  if optionIsTrue(kSoilOptionsAllowNitrateToMoveToTopSoilLayerByEvaporation) then
    movement.nitrateMovedToFirstLayerFromEvaporation_kgPha := EQ.NitrateMovedToFirstLayerFromSoilEvap_kgPha(layers,
        water.lowestLayerInWhichSoilEvapOccured)
  else
    movement.nitrateMovedToFirstLayerFromEvaporation_kgPha := 0.0;
  except on e: Exception do errorMessage('Exception in soil NitratesMovedUpFromEvaporation: ' + e.message); end;
  end;

procedure GsSoilPatch.CheckSedimentYieldAgainstMaximum;
  var
    maxErosion_tPha: single;
  begin
  try
  { total sediment yield cannot be more than 90% of soil weight in first layer }
  { if so, reduce water and wind erosion }
  { no use making option to turn this off }
  if erosion.totalErosion_tPha = 0.0 then exit;
  maxErosion_tPha := 0.9 * layers[0].weight_tPha;
  if (erosion.totalErosion_tPha > maxErosion_tPha) then 
    begin
    erosion.waterErosion_tPha := erosion.waterErosion_tPha * safediv(maxErosion_tPha, erosion.totalErosion_tPha);
    erosion.windErosion_tPha := erosion.windErosion_tPha * safediv(maxErosion_tPha, erosion.totalErosion_tPha);
    erosion.totalErosion_tPha := maxErosion_tPha;
    end;
  except on e: Exception do errorMessage('Exception in soil CheckSedimentYieldAgainstMaximum: ' + e.message); end;
  end;

procedure GsSoilPatch.RemoveErodedSoilIfNecessary;
  begin
  try
  { no need to have a switch here - if erosion turned off, this number will be zero }
  if erosion.totalErosion_tPha > 0.0 then EP.RemoveErodedSoil(self, erosion.totalErosion_tPha);
  except on e: Exception do errorMessage('Exception in soil RemoveErodedSoilIfNecessary: ' + e.message); end;
  end;

procedure GsSoilPatch.NitrateBalanceForDay;
  begin
  try
  { total up processes for end-of-day balances }
  { eventually this will be done for water, n, p, and soil weight (erosion) }
  { and the calculated change will be compared to the real change (today's minus yesterday's) to check }
  { that all things are being summed correctly }
  { n cycle, nitrate }
  movement.changeInPatchTotalNitrate_kgPha := movement.nInPatchRainfallForDay_kgPha + movement.nInIrrigationForDay_kgPha
    + movement.nFertAutoApplied_kgPha - movement.nitrateLostInRunoff_kgPha - movement.nitrateLeachedFromPercolation_kgPha -
    movement.nitrateLeachedFromLateralFlow_kgPha + movement.nitrification_kgPha + movement.activeHumusNMineralization_kgPha
    + movement.freshNMineralization_kgPha * 0.8 - movement.denitrification_kgPha - movement.totalNUptakeByPlants_kgPha;
  except on e: Exception do errorMessage('Exception in soil NitrateBalanceForDay: ' + e.message); end;
  end;

procedure GsSoilPatch.RecalculatePSPandMineralPFlowCoefficient;
  var
    layer: smallint;
    labilePConcForLayer_gPt, baseSaturationForLayer_pct: single;
  begin
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    labilePConcForLayer_gPt := safedivExcept(layers[layer].labileP_kgPha * kg_to_g, layers[layer].weight_tPha, 0.0);
    baseSaturationForLayer_pct := EQ.BaseSaturation_pct(layers[layer].baseFormingCations_cmolPkg,
        layers[layer].cationExchangeCapacity_cmolPkg);
    layers[layer].pSorptionCoeff_frn := EQ.PSorptionCoeffForLayer_frn(layers[layer].calciumCarbonate_pct,
      labilePConcForLayer_gPt, baseSaturationForLayer_pct, layers[layer].soilpH, layers[layer].clayContent_pct,
      params.soilWeatheringType, layers[layer].pSorptionCoeff_frn);
    layers[layer].mineralPFlowCoeff_Pday := EQ.MineralPFlowCoeffForLayer_Pday(layers[layer].pSorptionCoeff_frn,
      layers[layer].calciumCarbonate_pct, params.soilWeatheringType);
    end;
  end;

procedure GsSoilPatch.RecalculateAluminumSaturation;
  var
    layer: smallint;
    baseSaturationForLayer_pct, organicCForLayer_pct: single;
  begin
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    baseSaturationForLayer_pct := EQ.BaseSaturation_pct(layers[layer].baseFormingCations_cmolPkg,
        layers[layer].cationExchangeCapacity_cmolPkg);
    organicCForLayer_pct := Utils_OrganicCFromOrganicMatter_pct(layers[layer].organicMatter_tPha,
        layers[layer].weight_tPha);
    layers[layer].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturationForLayer_pct,
      organicCForLayer_pct, layers[layer].soilpH);
    end;
  end;

procedure GsSoilPatch.RecalculatePorosityFromBulkDensity;
  var
    layer: smallint;
    thickness_m: single;
  begin
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    if layer = 0 then
      thickness_m := layers[layer].depth_m
    else
      thickness_m := layers[layer].depth_m - layers[layer-1].depth_m;
    layers[layer].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
          layers[layer].bulkDensity_tPm3, layers[layer].rockContent_pct, thickness_m);
    end;
  end;

procedure GsSoilPatch.AutoFertilize(doNFert: boolean);
  { this function is completely autonomous of manual fertilization. }
  var
    layer: integer;
    potentialApplication_kgPha, finalApplication_kgPha, labilePInPlowDepth_kgPha, thickness_m, proportion,
      totalPSorptionCoeff, meanPSorptionCoeff, depthLastLayer_m: single;
  begin
  try
  if (doNFert) then
    begin
    if (params.ignoreNContentWhenAutoFertilizing) then 
      begin
      potentialApplication_kgPha := params.maxFractionMaxNFertInOneApplic_frn *
        plantMeans.minimumMaxAnnualNFert_kgPha;
      end
    else 
      begin
      potentialApplication_kgPha := params.maxFractionMaxNFertInOneApplic_frn *
        plantMeans.minimumMaxAnnualNFert_kgPha - totals.soilProfileDepthNitrate_kgPha;
      end;
    finalApplication_kgPha := 0.0;
    if (potentialApplication_kgPha > 0.0) then
      begin
      finalApplication_kgPha := min(potentialApplication_kgPha, plantMeans.minimumMaxAnnualNFert_kgPha -
        (yearTotals.cumNFertAutoApplied_kgPha + yearTotals.cumNAddedInIrrigationWater_kgPha));
      addQuantity(layers[1].nitrate_kgPha, finalApplication_kgPha);
      movement.nFertAutoApplied_kgPha := finalApplication_kgPha;
      addQuantity(yearTotals.cumNFertAutoApplied_kgPha, finalApplication_kgPha);
      end;
    end;
  { calculate mean p sorption coeff for whole plow depth and labile p in plow depth }
  { (note these are not merged with the normal patch totals because this is not often done) }
  labilePInPlowDepth_kgPha := 0.0;
  totalPSorptionCoeff := 0.0;
  depthLastLayer_m := 0.0;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    if (layers[layer].depth_m > state.plowDepth_m) then
      begin
      thickness_m := state.plowDepth_m - depthLastLayer_m;
      proportion := safediv(thickness_m, layers[layer].depth_m - depthLastLayer_m);
      end
    else 
      begin
      thickness_m := layers[layer].depth_m - depthLastLayer_m;
      proportion := 1.0;
      end;
    totalPSorptionCoeff := totalPSorptionCoeff + (layers[layer].pSorptionCoeff_frn * thickness_m);
    labilePInPlowDepth_kgPha := labilePInPlowDepth_kgPha + (layers[layer].labileP_kgPha * proportion);
    depthLastLayer_m := layers[layer].depth_m;
    if (layers[layer].depth_m > state.plowDepth_m) then break;
    end;
  meanPSorptionCoeff := safediv(totalPSorptionCoeff, state.plowDepth_m);
  { calculate amount of p added }
  finalApplication_kgPha := max(0.0, safediv(totals.plowDepthLabilePAtInput_kgPha - labilePInPlowDepth_kgPha,
    meanPSorptionCoeff));
  addQuantity(layers[1].labileP_kgPha, finalApplication_kgPha);
  movement.pFertAutoApplied_kgPha := finalApplication_kgPha;
  addQuantity(yearTotals.cumPFertAutoApplied_kgPha, finalApplication_kgPha);
  state.daysSinceAutoFertApplied := 0;
  except on e: Exception do errorMessage('Problem in soil AutoFertilize: ' + e.message); end;
  end;

procedure GsSoilPatch.AutoIrrigate;
  begin
  try
  if (yearTotals.cumIrrigationApplied_mm >= params.maxAnnualIrrVolumeAllowed_mm)
      or (state.daysSinceAutoIrrApplied < params.minIntervalAutoIrr_days) then
    exit;
  { set irrigation volume at 0 to use global value }
  SoilOp.irrigate(self, 0.0, true);
  except on e: Exception do errorMessage('Problem in soil AutoIrrigate: ' + e.message); end;
  end;

procedure GsSoilPatch.TransferStandingDeadToFlatResidueWhenTilling(tillageDepth_m: single; mixingEfficiency_frn: single);
  var
    i: integer;
    transferProportion: single;
    thePlant: GsPlant;
  begin
  try
  transferProportion := EQ.StandingCropResidueReductionFromTillageMultiplier(tillageDepth_m, mixingEfficiency_frn);
  plantMeans.totalStandingDead_tPha := 0.0;
  plantMeans.totalNInStandingDead_kgPha := 0.0;
  plantMeans.totalPInStandingDead_kgPha := 0.0;
  if (plantList.count > 0) then for i := 0 to plantList.count - 1 do
    begin
    thePlant := GsPlant(plantList.items[i]);
    if thePlant.awaitingReseedingOrInStasisBox then continue;
    thePlant.transferPropOfStandingDeadToFlatCropResidue(transferProportion);
    addQuantity(plantMeans.totalStandingDead_tPha, thePlant.biomass.standingDead_tPha);
    addQuantity(plantMeans.totalNInStandingDead_kgPha, thePlant.nutrients.nInStandingDead_kgPha);
    addQuantity(plantMeans.totalPInStandingDead_kgPha, thePlant.nutrients.pInStandingDead_kgPha);
    end;
  Utils_TransferPropOfMaterial(mulch.flatCropResidue_tPha, layers[0].flatCropResidue_tPha, transferProportion);
  Utils_TransferPropOfMaterial(mulch.organicNFresh_kgPha, layers[0].organicNFresh_kgPha, transferProportion);
  Utils_TransferPropOfMaterial(mulch.organicNFresh_kgPha, layers[0].organicPFresh_kgPha, transferProportion);
  except on e: Exception do errorMessage('Problem in soil TransferStandingDeadToFlatResidueWhenTilling: ' + e.message); end;
  end;

{ ------------------------------------------------------------------------------------------ utilities }
function GsSoilPatch.snowDepthFromSnowWaterContent_m: single;
  begin
  result := water.snowWaterContent_mm * mm_to_m;
  end;

function GsSoilPatch.optionIsTrue(index: smallint): boolean;
  begin
  result := false;
  if (index < 0) or (index > kSoilOptionsLastOption) then
    raise Exception.create('GsSoilPatch.optionIsTrue: index out of bounds');
  if garden = nil then
    result := self.options[index]
  else if garden.soilOverrides[index] then
    result := garden.soilOptions[index]
  else
    result := self.options[index];
  end;

procedure GsSoilPatch.addPlant(thePlant: GsModel);
  begin
  if (thePlant = nil) then exit;
  plantList.add(thePlant);
  self.updatePlantMeans;
  garden.updatePlantCount;
  end;

procedure GsSoilPatch.updatePlantMeans;
  begin
  { note this does not update browser - assumes caller will do so after this is called }
  CalculatePlantMeans;
  end;

procedure GsSoilPatch.removePlant(thePlant: GsModel);
  begin
  if (thePlant = nil) then exit;
  plantList.remove(thePlant);
  self.updatePlantMeans;
  garden.updatePlantCount;
  end;

function GsSoilPatch.GetNumActivePlants: integer;
  var i: longint;
  begin
  result := 0;
  if plantList = nil then exit;
  if plantList.count <= 0 then exit;
  for i := 0 to plantList.count - 1 do
    if not GsPlant(plantList.items[i]).awaitingReseedingOrInStasisBox then inc(result);
  end;

function GsSoilPatch.GetNumPlantsNotAwaitingReseeding: integer;
  var i: longint;
  begin
  result := 0;
  if plantList = nil then exit;
  if plantList.count <= 0 then exit;
  for i := 0 to plantList.count - 1 do
    if not GsPlant(plantList.items[i]).awaitingReseeding then inc(result);
  end;

function GsSoilPatch.objectType: integer;
  begin
  result := kObjectTypeSoil;
  end;

procedure GsSoilPatch.fillBasicInfoStructure(var basicInfo: SoilPatchBasicInfoStructure);
  begin
  try
  with basicInfo do
    begin
    area_ha := params.area_ha;
    slope_mPm := params.watershedSlopeSteepness_mPm;
    shade_pct := params.shadeIndex_pct;
    viewingAngle_deg := params.viewingAngle_deg;
    scale := params.drawingScale;
    orientationFromNorth_deg := Utils_RadiansToDegrees(params.watershedFieldLengthOrientationFromNorth_rad);
    end;
  except on e: Exception do errorMessage('Problem in soil fillBasicInfoStructure: ' + e.message); end;
  end;

procedure GsSoilPatch.useBasicInfoStructure(basicInfo: SoilPatchBasicInfoStructure);
  begin
  try
  with basicInfo do
    begin
    params.area_ha := area_ha;
    params.watershedslopeSteepness_mPm := slope_mPm;
    sideEffects_slopeSteepness_mPm(nil);
    params.shadeIndex_pct := shade_pct;
    params.viewingAngle_deg := viewingAngle_deg;
    params.drawingScale := scale;
    { there may be repercussions from this }
    params.watershedFieldLengthOrientationFromNorth_rad := Utils_DegreesToRadians(orientationFromNorth_deg);
    end;
  except on e: Exception do errorMessage('Problem in soil useBasicInfoStructure: ' + e.message); end;
  end;

procedure GsSoilPatch.CalculatePlantMeans;
  var
    { we are suspicious that we are not handling multiple plant means correctly here }
    i, numPlants, layer: integer;
    plant: GsPlant;
  begin
  try
  ZeroPlantMeans;
  plantMeans.minimumMaxAnnualNFert_kgPha := 10000;
  numPlants := 0;
  if (plantList.count > 0) then
    for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseedingOrInStasisBox then continue;
      inc(numPlants);
      {means}
      addQuantity(plantMeans.meanLeafAreaIndex, plant.development.leafAreaIndex);
      addQuantity(plantMeans.meanHeight_m, plant.biomass.height_m);
      addQuantity(plantMeans.meanWindErosionFactorStandingLive, plant.params.windErosionFactorStandingLive);
      addQuantity(plantMeans.meanWindErosionFactorStandingDead, plant.params.windErosionFactorStandingDead);
      addQuantity(plantMeans.meanWindErosionFactorFlatResidue, plant.params.windErosionFactorFlatResidue);
      addQuantity(plantMeans.meanMinCropManagementFactor, plant.params.minCropManagementFactor);
      {min}
      if (plant.params.maxAnnualNFert_kgPha < plantMeans.minimumMaxAnnualNFert_kgPha) then
          plantMeans.minimumMaxAnnualNFert_kgPha := plant.params.maxAnnualNFert_kgPha;
      {totals}
      addQuantity(plantMeans.totalStandingLive_tPha, plant.biomass.standingLive_tPha);
      addQuantity(plantMeans.totalStandingDead_tPha, plant.biomass.standingDead_tPha);
      addQuantity(plantMeans.totalNInStandingDead_kgPha, plant.nutrients.nInStandingDead_kgPha);
      addQuantity(plantMeans.totalPInStandingDead_kgPha, plant.nutrients.pInStandingDead_kgPha);
      addQuantity(plantMeans.totalWaterUse_mm, plant.water.totalWaterUse_mm);
      end;
  if (numPlants > 0) then
    begin
    plantMeans.meanLeafAreaIndex := plantMeans.meanLeafAreaIndex / numPlants;
    plantMeans.meanHeight_m := plantMeans.meanHeight_m / numPlants;
    plantMeans.meanWindErosionFactorStandingLive := plantMeans.meanWindErosionFactorStandingLive / numPlants;
    plantMeans.meanWindErosionFactorStandingDead := plantMeans.meanWindErosionFactorStandingDead / numPlants;
    plantMeans.meanWindErosionFactorFlatResidue := plantMeans.meanWindErosionFactorFlatResidue / numPlants;
    plantMeans.meanMinCropManagementFactor := plantMeans.meanMinCropManagementFactor / numPlants;
    end;
  { total plant root weights in each layer - moved this from SoilPatchTotals }
  for layer := 0 to state.numLayers - 1 do
    begin
    layers[layer].patchTotalRootWeight_tPha := 0.0;
    if (plantList.count > 0) then
      for i := 0 to plantList.count - 1 do
        begin
        plant := GsPlant(plantList.items[i]);
        if plant.awaitingReseedingOrInStasisBox then continue;
        addQuantity(layers[layer].patchTotalRootWeight_tPha, plant.biomass.rootWeightByLayer_tPha[layer]);
        end;
    end;
  except on e: Exception do errorMessage('Exception in soil CalculatePlantMeans: ' + e.message); end;
  end;

procedure GsSoilPatch.SoilPatchTotals;
  var
    layer, i: integer;
    layerBelowOneMHasBeenConsidered, layerUnderP3HasBeenConsidered, layerBelowSoilProfileDepthHasBeenConsidered,
      layerBelowPlowDepthHasBeenConsidered: boolean;
    proportion, depthLastLayer_m, propOfLayerInWaterTable_frn, waterContentModifiedByWaterTable_mm,
      propOfPartOfLayerInWaterTableThatIsAbove1m_frn, propOfLayerAbove1mAndBelowWaterTable_frn,
      swMinusWP_mm, fcMinusWP_mm, soilProfileDepthSoilWeight_tPha, totalBulkDensity_tPm3: single;
  begin
  try
  layerBelowOneMHasBeenConsidered := false;
  layerUnderP3HasBeenConsidered := false;
  layerBelowSoilProfileDepthHasBeenConsidered := false;
  layerBelowPlowDepthHasBeenConsidered := false;
  proportion := 0.0;
  depthLastLayer_m := 0.0;
  swMinusWP_mm := 0.0;
  fcMinusWP_mm := 0.0;
  soilProfileDepthSoilWeight_tPha := 0.0;
  totalBulkDensity_tPm3 := 0.0;
  ZeroSoilTotalsVars;
  { layer loop }
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    { whole patch totals (not depth dependent) }
    addQuantity(totals.patchTotalSoilWaterContent_mm, layers[layer].waterContent_mm);
    addQuantity(totals.patchTotalFieldCapacity_mm, layers[layer].fieldCapacity_mm);
    addQuantity(totals.patchTotalSoilEvap_mm, layers[layer].evaporation_mm);
    addQuantity(totals.patchTotalNitrate_kgPha, layers[layer].nitrate_kgPha);
    addQuantity(totals.patchTotalAmmonia_kgPha, layers[layer].ammonia_kgPha);
    addQuantity(totals.patchTotalOrganicNFresh_kgPha, layers[layer].organicNFresh_kgPha);
    addQuantity(totals.patchTotalOrganicNActiveHumus_kgPha, layers[layer].organicNActiveHumus_kgPha);
    addQuantity(totals.patchTotalOrganicNStableHumus_kgPha, layers[layer].organicNStableHumus_kgPha);
    addQuantity(totals.patchTotalLabileP_kgPha, layers[layer].labileP_kgPha);
    addQuantity(totals.patchTotalOrganicPFresh_kgPha, layers[layer].organicPFresh_kgPha);
    addQuantity(totals.patchTotalOrganicPHumus_kgPha, layers[layer].organicPHumus_kgPha);
    addQuantity(totals.patchTotalMineralPActive_kgPha, layers[layer].mineralPActive_kgPha);
    addQuantity(totals.patchTotalMineralPStable_kgPha, layers[layer].mineralPStable_kgPha);
    addQuantity(totalBulkDensity_tPm3, layers[layer].bulkDensity_tPm3);
    addQuantity(totals.patchTotalFlatResidue_tPha, layers[layer].flatCropResidue_tPha);
    addQuantity(totals.patchTotalOrganicMatter_tPha, layers[layer].organicMatter_tPha);
    { For the four following depth-dependent totals, if the layer is wholly within the depth }
    { (that is, if the depth at the bottom of the layer is less than the criterion depth) }
    { the whole layer amount is added to the total. If the layer is only partially within the }
    { depth (that is, if the depth at the bottom of the layer is greater than the criterion }
    { depth) only a proportion of that layer's amounts is added. That proportion is: }
    { (criterion depth - depth at top of layer) / thickness of the layer }
    { plow depth totals }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, state.plowDepth_m,
      layerBelowPlowDepthHasBeenConsidered);
    if (proportion > 0.0) then 
      { could be negative }
      totals.plowDepthSWmWP_mm := totals.plowDepthSWmWP_mm
        + (layers[layer].waterContent_mm - layers[layer].wiltingPoint_mm) * proportion;
    { soil profile depth totals }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m,
      state.soilProfileDepth_m, layerBelowSoilProfileDepthHasBeenConsidered);
    if (proportion > 0.0) then
      begin
      { could be negative }
      totals.soilProfileDepthSWmWP_mm := totals.soilProfileDepthSWmWP_mm
          + (layers[layer].waterContent_mm - layers[layer].wiltingPoint_mm) * proportion;
      addQuantity(totals.soilProfileDepthNitrate_kgPha, layers[layer].nitrate_kgPha * proportion);
      end;
    { 0.3 m depth totals }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, 0.3,
      layerUnderP3HasBeenConsidered);
    if (proportion > 0.0) then 
      begin
      swMinusWP_mm := swMinusWP_mm + ((layers[layer].waterContent_mm - layers[layer].wiltingPoint_mm) * proportion);
      fcMinusWP_mm := fcMinusWP_mm + ((layers[layer].fieldCapacity_mm - layers[layer].wiltingPoint_mm) * proportion);
      end;
    { 1 m depth totals (this one is special. it is affected by the water table) }
    { cases }
    { 1.  ------------ layer-1 bottom depth }
    {     ------------ layer bottom depth }
    {     ------------ 1 m }
    {     ------------ water table depth }
    {    layer is <= 1 m: whole soil water and porosity contents are added. }
    { 2.  ------------ layer-1 bottom depth }
    {     ------------ 1 m }
    {     ------------ layer bottom depth }
    {     ------------ water table depth }
    {     layer is > 1 m and above water table: proportion of soil water and porosity }
    {     is added according to the depth of the layer below 1 m }
    { 3.  layer is > 1 m and below water table: for porosity, act as if layer were above }
    {     water table. for soil water, do following cases }
    { 3a. ------------ layer-1 bottom depth }
    {     ------------ 1 m }
    {     ------------ water table depth }
    {     ------------ layer bottom depth }
    {     layer is > 1 m and below water table, and water table is below 1 m: }
    {     reduce soil water by porosity * proportion of layer in water table }
    {     then reduce further by proportion of layer that is in water table that is < 1 m }
    { 3b. ------------ layer-1 bottom depth }
    {     ------------ water table depth }
    {     ------------ 1 m }
    {     ------------ layer bottom depth }
    {     layer is > 1 m and below water table, and water table is above 1 m: }
    {     reduce soil water by porosity * proportion of layer in water table }
    {     add porosity * proportion of layer below water table and below 1 m }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, 1.0,
      layerBelowOneMHasBeenConsidered);
    if (proportion > 0.0) then 
      begin
      addQuantity(totals.top1mPorosity_mm, proportion * layers[layer].porosity_mm);
      if (water.waterTableDepth_m > layers[layer].depth_m) then 
        begin
        addQuantity(totals.top1MWaterContent_mm, proportion * layers[layer].waterContent_mm);
        end
      else 
        begin
        if layers[layer].depth_m - depthLastLayer_m <> 0 then
          propOfLayerInWaterTable_frn := safediv(layers[layer].depth_m - water.waterTableDepth_m,
              layers[layer].depth_m - depthLastLayer_m)
        else
          propOfLayerInWaterTable_frn := 0.0;
        waterContentModifiedByWaterTable_mm := layers[layer].waterContent_mm - layers[layer].porosity_mm *
          propOfLayerInWaterTable_frn;
        if (water.waterTableDepth_m > 1.0) then
          begin
          if water.waterTableDepth_m - depthLastLayer_m <> 0 then
            propOfPartOfLayerInWaterTableThatIsAbove1m_frn := safediv(1.0 - depthLastLayer_m,
                water.waterTableDepth_m - depthLastLayer_m)
          else
            propOfPartOfLayerInWaterTableThatIsAbove1m_frn := 0.0;
          addQuantity(totals.top1MWaterContent_mm, waterContentModifiedByWaterTable_mm *
              propOfPartOfLayerInWaterTableThatIsAbove1m_frn);
          end
        else
          begin
          if layers[layer].depth_m - depthLastLayer_m <> 0 then
            propOfLayerAbove1mAndBelowWaterTable_frn := safediv(1.0 - water.waterTableDepth_m,
                layers[layer].depth_m - depthLastLayer_m)
          else
            propOfLayerAbove1mAndBelowWaterTable_frn := 0.0;
          addQuantity(totals.top1MWaterContent_mm, waterContentModifiedByWaterTable_mm +
            layers[layer].porosity_mm * propOfLayerAbove1mAndBelowWaterTable_frn);
          end;
        end;
      end;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  { end of layers loop }
  { final calculations }
  totals.top30cmFractionFieldCapacity_frn := safediv(swMinusWP_mm, fcMinusWP_mm);
  totals.patchTotalSoilEvapAndPlantWaterUse_mm := plantMeans.totalWaterUse_mm + totals.patchTotalSoilEvap_mm;
  totals.patchMeanBulkDensity_tPm3 := safediv(totalBulkDensity_tPm3, state.numLayers);
  { these are calculations that used to be done only at input because the parameters that affect them
    did not change ever afterward. we moved them here to allow for changes anytime. }
  self.RecalculatePSPandMineralPFlowCoefficient;
  self.RecalculateAluminumSaturation;
  self.RecalculatePorosityFromBulkDensity;
  mulch.depth_m := Utils_MulchDepthFromMulchFlatCropResidue_m(mulch.flatCropResidue_tPha);
  except on e: Exception do errorMessage('Exception in soil SoilPatchTotals: ' + e.message); end;
  end;

procedure GsSoilPatch.ZeroSoilTotalsVars;
  begin
  totals.patchTotalSoilWaterContent_mm := 0.0;
  totals.top1MWaterContent_mm := 0.0;
  totals.plowDepthSWmWP_mm := 0.0;
  totals.soilProfileDepthSWmWP_mm := 0.0;
  totals.patchTotalFieldCapacity_mm := 0.0;
  totals.top30cmFractionFieldCapacity_frn := 0.0;
  totals.soilProfileDepthFCmWP_mm := 0.0;
  totals.top1mPorosity_mm := 0.0;
  { patchTotalPotSoilEvap_mm calc elsewhere }
  totals.patchTotalSoilEvap_mm := 0.0;
  { patchTotalPotPlantEvap_mm calc elsewhere }
  totals.patchTotalSoilEvapAndPlantWaterUse_mm := 0.0;
  totals.patchMeanBulkDensity_tPm3 := 0.0;
  { n }
  totals.patchTotalNitrate_kgPha := 0.0;
  totals.soilProfileDepthNitrate_kgPha := 0.0;
  totals.patchTotalAmmonia_kgPha := 0.0;
  totals.patchTotalOrganicNFresh_kgPha := 0.0;
  totals.patchTotalOrganicNStableHumus_kgPha := 0.0;
  totals.patchTotalOrganicNActiveHumus_kgPha := 0.0;
  { p }
  totals.patchTotalLabileP_kgPha := 0.0;
  totals.patchTotalOrganicPFresh_kgPha := 0.0;
  totals.patchTotalOrganicPHumus_kgPha := 0.0;
  totals.patchTotalMineralPActive_kgPha := 0.0;
  totals.patchTotalMineralPStable_kgPha := 0.0;
  { other materials }
  totals.patchTotalFlatResidue_tPha := 0.0;
  totals.patchTotalOrganicMatter_tPha := 0.0;
  end;

procedure GsSoilPatch.ZeroPlantMeans;
  begin
  plantMeans.meanLeafAreaIndex := 0.0;
  plantMeans.meanHeight_m := 0.0;
  plantMeans.totalStandingLive_tPha := 0.0;
  plantMeans.totalStandingDead_tPha := 0.0;
  plantMeans.totalNInStandingDead_kgPha := 0.0;
  plantMeans.totalPInStandingDead_kgPha := 0.0;
  plantMeans.totalWaterUse_mm := 0.0;
  plantMeans.meanWindErosionFactorStandingLive := 0.0;
  plantMeans.meanWindErosionFactorStandingDead := 0.0;
  plantMeans.meanWindErosionFactorFlatResidue := 0.0;
  plantMeans.meanMinCropManagementFactor := 0.0;
  plantMeans.minimumMaxAnnualNFert_kgPha := 0.0;
  end;

procedure GsSoilPatch.adjustLowerLayerDepthsForChangeToOneLayer(affectedLayer: integer; changeInDepth_m: single);
  var
    layerToStart, layer: integer;
  begin
  try
  if (affectedLayer <= 0{can't change layer 0 depth})
    or (affectedLayer >= state.numLayers - 1{no affect from last layer}) then exit;
  layerToStart := affectedLayer + 1;
  for layer := layerToStart to state.numLayers - 1 do
    self.setLayerDepth(layer, layers[layer].depth_m + changeInDepth_m);
  except on e: Exception do errorMessage('Problem in soil adjustLowerLayerDepthsForChangeToOneLayer: ' + e.message); end;
  end;

procedure GsSoilPatch.setLayerDepth(layer: integer; newDepth_m: single);
  begin
  layers[layer].depth_m := newDepth_m;
  end;

function GsSoilPatch.totalSoilWeightDownToCriterionDepth(criterionDepth_m: single): single;
  var
    layer: integer;
    depthLastLayer_m: single;
    firstLayerBelowCriterionDepthHasBeenConsidered: boolean;
  begin
  result := 0.0;
  try
  depthLastLayer_m := 0.0;
  firstLayerBelowCriterionDepthHasBeenConsidered := false;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    if (layers[layer].depth_m < criterionDepth_m) then
      result := result + layers[layer].weight_tPha
    else
      begin
      if firstLayerBelowCriterionDepthHasBeenConsidered then
        break
      else
        begin
        if layers[layer].depth_m - depthLastLayer_m <> 0 then
          result := result + layers[layer].weight_tPha
              * safediv(layers[layer].depth_m - criterionDepth_m, layers[layer].depth_m - depthLastLayer_m);
        firstLayerBelowCriterionDepthHasBeenConsidered := true;
        end;
      end;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  except on e: Exception do result := errorMessage('Problem in soil totalSoilWeightDownToCriterionDepth: ' + e.message); end;
  end;

{ ------------------------------------------------------------------------------------------ at end of year }
procedure GsSoilPatch.SoilEndYear;
  begin
  try
  with yearTotals do if (cumNFertAutoApplied_kgPha + cumNAddedInIrrigationWater_kgPha + cumNFixation_kgPha > 0.0) then
    ChangePHFromFertilizerAdded;
  if optionIsTrue(kSoilOptionsAutoPHControl) then
    AutoLime;
  { or add sulfur when ready }
  SoilPatchTotals;
  yearTotals.cumNFixation_kgPha := 0.0;
  yearTotals.cumNFertAutoApplied_kgPha := 0.0;
  yearTotals.cumPFertAutoApplied_kgPha := 0.0;
  yearTotals.cumNAddedInIrrigationWater_kgPha := 0.0;
  yearTotals.cumIrrigationApplied_mm := 0.0;
  except on e: Exception do errorMessage('Problem in soil SoilEndYear: ' + e.message); end;
  end;

procedure GsSoilPatch.AutoLime;
  var
    { this is done once at the end of each year if auto liming is on. }
    { Since it doesn't go from amount of lime added to change in pH and Al saturation }
    { but instead figures out how much lime to use to make these things meet constants, }
    { this function must be separate from the manual liming function, in which a specified }
    { amount of lime produces a change in pH and Al saturation. }
    layerBelowMaxTillageDepth: smallint;
    limeAdded_kgPha, limeNeeded_kgPha, organicC_pct, baseSaturation_pct, changeInBaseSaturation_pct, newBaseSaturation_pct,
      changeInBaseSaturationToRaisePHTo6p5_pct, changeInBaseSaturationToOffsetAlSat_pct,
      totalSoilWeightInMaxTillageDepth_tPha, meanWeightedOrganicC_pct, meanWeightedSoilpH, meanWeightedCEC_cmolPkg,
      meanWeightedBaseFormingCations_cmolPkg, aluminumSaturation_pct, newAluminumSaturation_pct, newSoilpH,
      newBaseFormingCations_cmolPkg: single;
  begin
  try
  meanWeightedOrganicC_pct := 0.0;
  meanWeightedSoilpH := 0.0;
  meanWeightedCEC_cmolPkg := 0.0;
  meanWeightedBaseFormingCations_cmolPkg := 0.0;
  totalSoilWeightInMaxTillageDepth_tPha := 0.0;
  limeAdded_kgPha := 0.0;
  baseSaturation_pct := 0.0;
  aluminumSaturation_pct := 0.0;
  changeInBaseSaturationToRaisePHTo6p5_pct := 0.0;
  changeInBaseSaturationToOffsetAlSat_pct := 0.0;
  newAluminumSaturation_pct := 0.0;
  newSoilpH := 0.0;
  newBaseFormingCations_cmolPkg := 0.0;
  { calculate mean weighted values for organic c, cec, pH, and base forming cations }
  MeanWeightedLimingValues(meanWeightedOrganicC_pct, meanWeightedCEC_cmolPkg, meanWeightedSoilpH,
    meanWeightedBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  { calculate present aluminum saturation }
  baseSaturation_pct := EQ.BaseSaturation_pct(meanWeightedBaseFormingCations_cmolPkg, meanWeightedCEC_cmolPkg);
  aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, meanWeightedOrganicC_pct, meanWeightedSoilpH);
  { if pH > 6.5 don't add lime }
  { if pH <= 6.5 and not highly weathered soil, add enough lime to raise pH to 6.5 }
  { if pH <= 6.5 and highly weathered soil, add enough lime to counteract Al toxicity }
  { because highly weathered soil is well buffered and it would take a whole lot of lime }
  { to get the pH up to 6.5, so you just go for a pH of 5.4, which is enough for Al toxicity }
  if (meanWeightedSoilpH > 6.5) then 
    begin
    { if pH over 6.5 don't add anything }
    limeAdded_kgPha := 0.0;
    newAluminumSaturation_pct := aluminumSaturation_pct;
    newSoilpH := meanWeightedSoilpH;
    newBaseFormingCations_cmolPkg := meanWeightedBaseFormingCations_cmolPkg;
    end
  else 
    begin
    { pH <= 6.5 }
    if (params.soilWeatheringType <> kHighlyWeatheredSoil) then 
      begin
      { calculate amount of lime needed to raise pH to 6.5 }
      changeInBaseSaturationToRaisePHTo6p5_pct := EQ.ChangeInBaseSaturationToRaisePHTo6p5_pct(meanWeightedSoilpH,
        baseSaturation_pct);
      limeAdded_kgPha := EQ.LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha(totalSoilWeightInMaxTillageDepth_tPha * kg_to_t,
        meanWeightedCEC_cmolPkg, changeInBaseSaturationToRaisePHTo6p5_pct);
      newAluminumSaturation_pct := 0;
      { if the amount of lime needed is < 2 kg/ha, don't bother }
      if ((limeAdded_kgPha < 2.0) and (meanWeightedBaseFormingCations_cmolPkg > 5.0)) then 
        begin
        limeAdded_kgPha := 0.0;
        newAluminumSaturation_pct := aluminumSaturation_pct;
        newSoilpH := meanWeightedSoilpH;
        newBaseFormingCations_cmolPkg := meanWeightedBaseFormingCations_cmolPkg;
        end
      else 
        begin
        { lime is added }
        newSoilpH := 6.5;
        { bsa = frn_to_pct * bfc/cec so bfc = pct_to_frn * bsa * cec }
        newBaseFormingCations_cmolPkg := pct_to_frn * (baseSaturation_pct + changeInBaseSaturationToRaisePHTo6p5_pct) *
          meanWeightedCEC_cmolPkg;
        end;
      end
    else 
      begin
      { highly weathered soil }
      { calculate amount of lime needed to counteract Al toxicity }
      changeInBaseSaturationToOffsetAlSat_pct := EQ.ChangeInBaseSaturationToOffsetAlSat_pct(aluminumSaturation_pct,
        meanWeightedCEC_cmolPkg);
      limeAdded_kgPha := EQ.LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha(changeInBaseSaturationToOffsetAlSat_pct,
        totalSoilWeightInMaxTillageDepth_tPha);
      { if the amount of lime needed is < 1 kg/ha, don't bother }
      if (limeAdded_kgPha < 1.0) then 
        begin
        limeAdded_kgPha := 0.0;
        newAluminumSaturation_pct := aluminumSaturation_pct;
        newSoilpH := meanWeightedSoilpH;
        newBaseFormingCations_cmolPkg := meanWeightedBaseFormingCations_cmolPkg;
        end
      else 
        begin
        { lime is added }
        newSoilpH := 5.4;
        newBaseFormingCations_cmolPkg := max(0.02, meanWeightedBaseFormingCations_cmolPkg +
          changeInBaseSaturationToOffsetAlSat_pct);
        baseSaturation_pct := EQ.BaseSaturation_pct(newBaseFormingCations_cmolPkg, meanWeightedCEC_cmolPkg);
        newAluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, meanWeightedOrganicC_pct, newSoilpH);
        end;
      end;
    end;
  { set the layer values to the new values (note this removes all differences between the layers) }
  SetLayerPropertiesAfterLiming(layerBelowMaxTillageDepth, newBaseFormingCations_cmolPkg, newSoilpH,
    newAluminumSaturation_pct);
  { mix in lime }
  { made guess for efficiency }
  SoilOp.mixSoil(self, params.area_ha, state.soilProfileDepth_m, 0.2, surface.randomRoughness_mm,
    surface.ridgeHeight_mm, surface.ridgeInterval_m, true);
  except on e: Exception do errorMessage('Problem in soil AutoLime: ' + e.message); end;
  end;

procedure GsSoilPatch.ChangePHFromFertilizerAdded;
  var
    layerBelowMaxTillageDepth: smallint;
    limeAdded_kgPha, organicC_pct, baseSaturation_pct, changeInBaseSaturation_pct, newBaseSaturation_pct,
      changeInBaseSaturationToRaisePHTo6p5_pct, changeInBaseSaturationToOffsetAlSat_pct,
      totalSoilWeightInMaxTillageDepth_tPha, meanWeightedOrganicC_pct, meanWeightedSoilpH, meanWeightedCEC_cmolPkg,
      meanWeightedBaseFormingCations_cmolPkg, aluminumSaturation_pct, newAluminumSaturation_pct, newSoilpH,
      newBaseFormingCations_cmolPkg, changeInBaseSaturationByNAdded_frn: single;
  begin
  try
  meanWeightedOrganicC_pct := 0.0;
  meanWeightedSoilpH := 0.0;
  meanWeightedCEC_cmolPkg := 0.0;
  meanWeightedBaseFormingCations_cmolPkg := 0.0;
  totalSoilWeightInMaxTillageDepth_tPha := 0.0;
  limeAdded_kgPha := 0.0;
  baseSaturation_pct := 0.0;
  aluminumSaturation_pct := 0.0;
  changeInBaseSaturationToRaisePHTo6p5_pct := 0.0;
  changeInBaseSaturationToOffsetAlSat_pct := 0.0;
  newAluminumSaturation_pct := 0.0;
  newSoilpH := 0.0;
  newBaseFormingCations_cmolPkg := 0.0;
  changeInBaseSaturationByNAdded_frn := 0.0;
  newBaseSaturation_pct := 0.0;
  { calculate mean weighted values for organic c, cec, pH, and base forming cations }
  MeanWeightedLimingValues(meanWeightedOrganicC_pct, meanWeightedCEC_cmolPkg, meanWeightedSoilpH,
    meanWeightedBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  { calculate changes that occur in bsa and ph due to n fertilizer added over year }
  changeInBaseSaturationByNAdded_frn := EQ.ChangeInBaseSaturationByNAdded_frn(yearTotals.cumNFertAutoApplied_kgPha +
    yearTotals.cumNAddedInIrrigationWater_kgPha, yearTotals.cumNFixation_kgPha, totalSoilWeightInMaxTillageDepth_tPha);
  newSoilpH := meanWeightedSoilpH - EQ.pHReductionFromNAdded(changeInBaseSaturationByNAdded_frn,
    meanWeightedCEC_cmolPkg);
  newBaseFormingCations_cmolPkg := max(0.02, meanWeightedBaseFormingCations_cmolPkg -
    changeInBaseSaturationByNAdded_frn);
  newBaseSaturation_pct := frn_to_pct * safediv(newBaseFormingCations_cmolPkg, meanWeightedCEC_cmolPkg);
  newAluminumSaturation_pct := EQ.AluminumSaturation_pct(newBaseSaturation_pct, meanWeightedOrganicC_pct, newSoilpH);
  { set the layer values to the new values (note this removes all differences between the layers) }
  SetLayerPropertiesAfterLiming(layerBelowMaxTillageDepth, newBaseFormingCations_cmolPkg, newSoilpH,
    newAluminumSaturation_pct);
  { record new weighted values for output }
  MeanWeightedLimingValues(totals.patchWeightedMeanOrganicC_pct,
    totals.patchWeightedMeanCationExchangeCapacity_cmolPkg, totals.patchWeightedMeanpH,
    totals.patchWeightedMeanBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  except on e: Exception do errorMessage('Problem in soil ChangePHFromFertilizerAdded: ' + e.message); end;
  end;

procedure GsSoilPatch.MeanWeightedLimingValues(var meanWeightedOrganicC_pct: single; var meanWeightedCEC_cmolPkg:
  single; var meanWeightedSoilpH: single; var meanWeightedBaseFormingCations_cmolPkg: single; var
  totalSoilWeight_tPha: single; var layerBelowMaxTillageDepth: smallint);
  var
    layer: integer;
    layerBelowMaxTillageDepthHasBeenConsidered: boolean;
    depthLastLayer_m, depthToConsider_m, organicC_pct, proportion: single;
  begin
  try
  meanWeightedOrganicC_pct := 0.0;
  meanWeightedCEC_cmolPkg := 0.0;
  meanWeightedSoilpH := 0.0;
  meanWeightedBaseFormingCations_cmolPkg := 0.0;
  totalSoilWeight_tPha := 0.0;
  layerBelowMaxTillageDepth := 0;
  depthLastLayer_m := 0.0;
  layerBelowMaxTillageDepthHasBeenConsidered := false;
  { change from EPIC - using lower bound on depth considered because in EPIC max tillage depth assumes there
    will be tillage, whereas we may not have any. Using plow depth as lower bound. }
  depthToConsider_m := max(state.maxTillageDepth_m, state.plowDepth_m);
  { calculate mean weighted organic c, cec, pH, and base forming cations for all soil layers }
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do 
    begin
    organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[layer].organicMatter_tPha, layers[layer].weight_tPha);
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m,
      depthToConsider_m, layerBelowMaxTillageDepthHasBeenConsidered);
    if proportion > 0.0 then
      begin
      meanWeightedOrganicC_pct := meanWeightedOrganicC_pct
          + (proportion * organicC_pct * layers[layer].weight_tPha);
      meanWeightedSoilpH := meanWeightedSoilpH
          + (proportion * layers[layer].soilpH * layers[layer].weight_tPha);
      meanWeightedBaseFormingCations_cmolPkg := meanWeightedBaseFormingCations_cmolPkg
          + (proportion * layers[layer].baseFormingCations_cmolPkg * layers[layer].weight_tPha);
      meanWeightedCEC_cmolPkg := meanWeightedCEC_cmolPkg
          + (proportion * layers[layer].cationExchangeCapacity_cmolPkg * layers[layer].weight_tPha);
      addQuantity(totalSoilWeight_tPha, proportion * layers[layer].weight_tPha);
      end;
    layerBelowMaxTillageDepth := layer;
    if (layers[layer].depth_m > depthToConsider_m) then break;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  { calculate weighted means from sums }
  meanWeightedCEC_cmolPkg := safediv(meanWeightedCEC_cmolPkg, totalSoilWeight_tPha);
  meanWeightedOrganicC_pct := safediv(meanWeightedOrganicC_pct, totalSoilWeight_tPha);
  meanWeightedSoilpH := safediv(meanWeightedSoilpH, totalSoilWeight_tPha);
  meanWeightedBaseFormingCations_cmolPkg := safediv(meanWeightedBaseFormingCations_cmolPkg, totalSoilWeight_tPha);
  except on e: Exception do errorMessage('Problem in soil MeanWeightedLimingValues: ' + e.message); end;
  end;

procedure GsSoilPatch.SetLayerPropertiesAfterLiming(layerBelowMaxTillageDepth: integer; newBaseFormingCations_cmolPkg:
  single; newSoilpH: single; newAluminumSaturation_pct: single);
  var
    layer: integer;
    propAbove, propBelow, depthLastLayer_m: single;
  begin
  try
  propAbove := 0.0;
  propBelow := 0.0;
  depthLastLayer_m := 0.0;
  if layerBelowMaxTillageDepth > 0 then for layer := 0 to layerBelowMaxTillageDepth - 1 do 
    begin
    { if this is the first layer below the max tillage depth, change its values only by a portion }
    if (((layer = layerBelowMaxTillageDepth) and (layerBelowMaxTillageDepth <> state.numLayers)) and
      (layerBelowMaxTillageDepth <> 1)) then 
      begin
      propAbove := safediv(state.maxTillageDepth_m - depthLastLayer_m, layers[layer].depth_m - depthLastLayer_m);
      propBelow := safediv(layers[layer].depth_m - state.maxTillageDepth_m, layers[layer].depth_m - depthLastLayer_m);
      layers[layer].baseFormingCations_cmolPkg := propBelow * layers[layer].baseFormingCations_cmolPkg + propAbove *
        layers[layer - 1].baseFormingCations_cmolPkg;
      layers[layer].soilpH := propBelow * layers[layer].soilpH + propAbove * layers[layer - 1].soilpH;
      layers[layer].aluminumSaturation_pct := propBelow * layers[layer].aluminumSaturation_pct + propAbove *
        layers[layer - 1].aluminumSaturation_pct;
      end
    else 
      begin
      layers[layer].baseFormingCations_cmolPkg := newBaseFormingCations_cmolPkg;
      layers[layer].soilpH := newSoilpH;
      layers[layer].aluminumSaturation_pct := newAluminumSaturation_pct;
      end;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  except on e: Exception do errorMessage('Problem in soil SetLayerPropertiesAfterLiming: ' + e.message); end;
  end;

{ Since plants keep track of their root weights per soil layer, and since those weights depend on the thickness of the
  different layers, when the relative sizes of the different layers changes, the plant root weights relative to those
  layers must change in response. The quantity that must be held constant is the quotient of root weight to layer
  thickness. This function keeps that ratio constant over the life of the plant.
  It uses this basic equation: newWeightProp = oldWeightProp * (newThicknessProp / oldThicknessProp) }
procedure GsSoilPatch.AdjustPlantRootWeightsInLayersAfterMovingSoil(var oldDepth_m: arraySoilLayers);
  var
    i, layer: integer;
    oldThicknessProp, oldWeightProp, newThicknessProp, newWeightProp, depthLastLayer_m, oldDepthLastLayer_m: single;
    thePlant: GsPlant;
  begin
  try
  if (plantList.count > 0) then
    for i := 0 to plantList.count - 1 do
      begin
      thePlant := GsPlant(plantList.items[i]);
      if thePlant.awaitingReseedingOrInStasisBox then continue;
      if (thePlant.biomass.totalRootWeight_tPha > 0) and (thePlant.biomass.numLayersWithRootsInThem > 0) then
        begin
        depthLastLayer_m := 0.0;
        oldDepthLastLayer_m := 0.0;
        for layer := 0 to thePlant.biomass.numLayersWithRootsInThem - 1 do
          begin
          oldThicknessProp := safediv(oldDepth_m[layer] - oldDepthLastLayer_m, oldDepth_m[state.numLayers - 1]);
          oldWeightProp := safediv(thePlant.biomass.rootWeightByLayer_tPha[layer], thePlant.biomass.totalRootWeight_tPha);
          newThicknessProp := safediv(layers[layer].depth_m - depthLastLayer_m, layers[state.numLayers - 1].depth_m);

          newWeightProp := oldWeightProp * safediv(newThicknessProp, oldThicknessProp);
          thePlant.biomass.rootWeightByLayer_tPha[layer] := newWeightProp * thePlant.biomass.totalRootWeight_tPha;
          depthLastLayer_m := layers[layer].depth_m;
          oldDepthLastLayer_m := oldDepth_m[layer];
          end;
        end;
     end;
  except on e: Exception do errorMessage('Problem in soil AdjustPlantRootWeightsInLayersAfterMovingSoil: ' + e.message); end;
  end;

{ ------------------------------------------------------------------------------------------ streaming/transfer }
procedure GsSoilPatch.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsSoilPatch;
  cvir.versionNumber := 5;
  cvir.additionNumber := 3;
  end;

procedure GsSoilPatch.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  self.streamWithoutSubobjectsUsingFiler(filer, cvir);
  {read will have corrupted garden pointer - caller will fix}
  {read will also corrupt pointer to first plant; will fix next}
  plantList.streamUsingFiler(filer, GsPlant);
  if filer.isReading then plantList.ForEach(FixUpPlant, self);
  randomNumberGenerator.streamUsingFiler(filer);
  end;

var
  globalSoilTypeNameString: string;

procedure GsSoilPatch.streamWithoutSubobjectsUsingFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
  	i, layer: integer;
	begin
  inherited streamDataWithFiler(filer, cvir);
  {model structures}
  filer.streamBytes(options, sizeOf(options));
  filer.streamBytes(params, sizeOf(params));
  filer.streamBytes(state, sizeOf(state));
  filer.streamBytes(surface, sizeOf(surface));
  filer.streamBytes(mulch, sizeOf(mulch));
  filer.streamBytes(water, sizeOf(water));
  filer.streamBytes(erosion, sizeOf(erosion));
  filer.streamBytes(layers, sizeOf(layers));
  filer.streamBytes(movement, sizeOf(movement));
  filer.streamBytes(totals, sizeOf(totals));
  filer.streamBytes(yearTotals, sizeOf(yearTotals));
  filer.streamBytes(plantMeans, sizeOf(plantMeans));
  looseOrganicMatterList.streamUsingFiler(filer, GsOrganicMatter);
  {graphical stuff}
  for i := 0 to 3 do filer.streamPoint(corners[i]);
  {take extra care to limit soil type name size}
  if filer.isWriting then globalSoilTypeNameString := soilTypeName;
  filer.streamShortString(globalSoilTypeNameString);
  if filer.isReading then self.setSoilTypeName(globalSoilTypeNameString);
  if (filer.isReading) and (cvir.additionNumber < 1) then with surface do
    begin
    dikeHeightAtCreation_mm := 100.0; {range is 0 to 200}
    dikeInterval_m := 1.0;            {range is 0 to 3}
    randomRoughness_mm := 30.0;       {from EPIC data file; range is from 0 to 75}
    ridgeHeight_mm :=  100.0;         {from EPIC data file; range is from 0 to 300}
    ridgeHeightLastOperation_mm := surface.ridgeHeight_mm;
    ridgeInterval_m := 1.0;           {from EPIC data file; range is from 0 to 2}
    ridgeIntervalLastOperation_m := surface.ridgeInterval_m;
    end;
  if (filer.isReading) and (cvir.additionNumber < 2) then
    params.avgCurveNumberUnadjustedForSlope := water.avgCurveNumber;
  if (filer.isReading) and (cvir.additionNumber < 3) then
    options[kSoilOptionsAllowFlatCropResidueToDecay] := true;
  end;

procedure GsSoilPatch.setSoilTypeName(aString: string);
  begin
  soilTypeName := copy(aString, 1, kGraphicalModelNameLength);
  end;

procedure GsSoilPatch.copyToWithoutSubobjects(newCopy: GsSoilPatch);
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
  self.streamWithoutSubobjectsUsingFiler(filer, cvir);
  memoryStream.seek(0,0); {move to start of stream}
  filer.setToReadingMode;
  newCopy.streamWithoutSubobjectsUsingFiler(filer, cvir);
  newCopy.garden := self.garden;
  filer.free;
  memoryStream.free;
  end;

class procedure GsSoilPatch.fillEnumStringList(var list: TStringList; fieldID: Integer;
    var hasRadioButtons: boolean);
  begin
  { assumes list being given to it is empty }
  case fieldID of
    kSoilPatchSoilWeatheringType:
      begin
      list.add('Calcareous soil or no weathering information');
      list.add('Slightly weathered soil');
      list.add('Highly weathered soil');
      list.add('Use inputs for phosporus sorption ratio');
      end;
    kSoilPatchPotEvapMethod:
      begin
      list.add('Penman method');
      list.add('Penman-Monteith method');
      list.add('Priestley-Taylor method');
      list.add('Hargreaves method');
      end;
    kSoilPatchPeakRateEstimationMethod:
      begin
      list.add('Modified rational equation method');
      list.add('SCS Type I method');
      list.add('SCS Type Ia method');
      list.add('SCS Type II method');
      list.add('SCS Type III method');
      end;
    kSoilPatchWaterErosionMethod:
      begin
      list.add('Universal soil loss equation (USLE)');
      list.add('Modified universal soil loss equation (MUSLE)');
      list.add('Onstad-Foster modification of USLE');
      list.add('Small watershed version of MUSLE (MUSS)');
      list.add('Theoretically derived version of MUSLE (MUST)');
      list.add('Version of MUSLE with user input (MUSI)');
      end;
  else
    raise Exception.create('unknown field for soil string list ' + intToStr(fieldID));
  end;
  { put inside case if need any check boxes for this object }
  hasRadioButtons := true;
  end;

procedure GsSoilPatch.copyFromSoilType(aGarden: GsGarden; soilType: GsSoilPatch);
  var
    savedCorners: array[0..3] of TPoint;
    i: integer;
  begin
  for i := 0 to 3 do savedCorners[i] := corners[i];
  if soilType <> nil then soilType.copyTo(self);
  self.setSoilTypeName(soilType.getName);
  self.garden := aGarden;
  for i := 0 to 3 do corners[i] := savedCorners[i];
  self.cleanUpAfterCopyFromSoilType;
  end;

procedure GsSoilPatch.cleanUpAfterCopyFromSoilType;
  var
    totalSoilWeightInMaxTillageDepth_tPha: single;
    layerBelowMaxTillageDepth: smallint;
    i: smallint;
  begin
  try
  { calculate channel length if zero - must be done here as uses garden things }
  if (params.watershedChannelLength_km = 0.0) then
    begin
    if (params.peakRateEstimationMethod <> kModifiedRationalEquationMethod) then
      begin
      { scs tr55 method }
      params.watershedChannelLength_km := power(channelGeometryConstants[2] * params.watershedArea_ha,
        channelGeometryConstants[3]);
      end
    else
      begin
      if params.watershedFieldWidth_km <> 0 then
        params.watershedChannelLength_km := 0.1 * sqrt(params.watershedFieldLength_km * km_to_m
          * safediv(params.watershedArea_ha, params.watershedFieldWidth_km * km_to_m))
      else
        params.watershedChannelLength_km := 0.0;
      end;
    end;
  { if channel depth is zero, calculate it from wathershed area - uses garden channelGeometryCoeffs }
  Utils_DefaultFloatIfZero(params.watershedChannelDepth_m,
      channelGeometryConstants[0] * power(params.watershedArea_ha, channelGeometryConstants[0]));
  { adjust curve number for slope }
  EP.AdjustCurveNumberForSlope(water, params, state.numLayers, layers);
  { starting number for curve number }
  water.curveNumber := water.avgCurveNumber;
  { set time since auto irrigation and fertilization applied at interval }
  { so these things can be done right away }
  state.daysSinceAutoIrrApplied := params.minIntervalAutoIrr_days;
  state.daysSinceAutoFertApplied := params.minIntervalAutoFert_days;
  { default soil patch area at 1 square meter }
  params.area_ha := 1.0 * m2_to_ha;
  { calculate summary things that are used later }
  { initializations for things saved from one day to the next }
  water.timeOfConc_hr := 0.85; { yesterday's timeOfConc_hr is used, so must start with something }
  erosion.windErosionAccumulatedFactor := 0.5;
  totals.sumRainfallMinusRunoffPrev30Days_mm := 0.0;
  for i := 0 to 29 do
    begin
    totals.rainfallMinusRunoff30DaysArray_mm[i] := 3.0;
    totals.sumRainfallMinusRunoffPrev30Days_mm := totals.sumRainfallMinusRunoffPrev30Days_mm
        + totals.rainfallMinusRunoff30DaysArray_mm[i];
    end;
  totals.sumPotSoilEvapPrev30Days_mm := 0.0;
  for i := 0 to 29 do
    begin
    totals.potSoilEvap30DaysArray_mm[i] := 3.0;
    totals.sumPotSoilEvapPrev30Days_mm := totals.sumPotSoilEvapPrev30Days_mm
        + totals.potSoilEvap30DaysArray_mm[i];
    end;
  Defaults_SoilPatchTotalsAtInput(self);
  SoilPatchTotals;
  MeanWeightedLimingValues(totals.patchWeightedMeanOrganicC_pct, totals.patchWeightedMeanCationExchangeCapacity_cmolPkg,
      totals.patchWeightedMeanpH, totals.patchWeightedMeanBaseFormingCations_cmolPkg,
      totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  except on e: Exception do errorMessage('Problem in soil cleanUpAfterCopyFromSoilType: ' + e.message); end;
  end;

procedure GsSoilPatch.updateSoilProfileDepthAndOtherDepths;
  begin
  with state do
    begin
    maxTillageDepth_m := min(maxTillageDepth_m, layers[state.numLayers - 1].depth_m);
    plowDepth_m := min(plowDepth_m, layers[state.numLayers - 1].depth_m);
    soilProfileDepth_m := min(soilProfileDepth_m, layers[state.numLayers - 1].depth_m);
    end;
  end;

procedure GsSoilPatch.transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
  var
    oldSingle: single;
	begin
  try
  if ft = kFieldFloat then
    directTransferField(kGetField, oldSingle, fieldID, ft, index, deriveMethod, updateList);
  self.directTransferField(d, v, fieldID, ft, index, deriveMethod, updateList);
  if d = kGetField then exit;
  case fieldID of
    kSoilPatchWatershedSlopeSteepness_mPm:
      sideEffects_slopeSteepness_mPm(updateList);
    kSoilPatchSoilEvapByDepthCoeffs:
      Utils_CalcSCurveCoeffs(params.soilEvapByDepthCoeffs);
    kSoilPatchSoilInsulationFromAirTempCoeffs:
      Utils_CalcSCurveCoeffs(params.soilInsulationFromAirTempCoeffs);
    kSoilPatchSoilSettlingFromRainfallCoeffs:
      Utils_CalcSCurveCoeffs(params.soilSettlingFromRainfallCoeffs);
    kSoilPatchNVolatilizationByDepthCoeffs:
      Utils_CalcSCurveCoeffs(params.nVolatilizationByDepthCoeffs);
    kSoilPatchWatershedSlopeLength_m:
      sideEffects_watershedSlopeLength_ha(updateList);
    kSoilPatchWatershedArea_ha:
      sideEffects_watershedArea_ha(updateList);
    kSoilPatchWaterTableMinDepth_m:
      sideEffects_waterTableMinDepth_m(updateList);
    kSoilPatchWaterTableMaxDepth_m:
      sideEffects_waterTableMaxDepth_m(updateList);
    kSoilPatchWaterTableDepth_m:
      sideEffects_waterTableDepth_m(updateList);
    kSoilPatchMulchDepth_m:
      sideEffects_mulchDepth_m(updateList);
    kSoilPatchMulchFlatCropResidue_tPha:
      sideEffects_mulchFlatCropResidue_tPha(updateList);
    kSoilPatchLayerWeight_tPha:
      sideEffects_layerWeight_tPha(index, oldSingle, updateList);
    kSoilPatchLayerDepth_m:
      sideEffects_layerDepth_m(index, oldSingle, updateList);
    kSoilPatchLayerWiltingPoint_mm:
      sideEffects_wiltingPoint_mm(index, updateList);
    kSoilPatchLayerFieldCapacity_mm:
      sideEffects_fieldCapacity_mm(index, updateList);
    kSoilPatchLayerPorosity_mm:
      sideEffects_porosity_mm(index, updateList);
    kSoilPatchLayerBulkDensity_tPm3:
      sideEffects_bulkDensity_tPm3(index, updateList);
    kSoilPatchLayerSettledBulkDensity_tPm3:
      sideEffects_settledBulkDensity_tPm3(index, updateList);
    kSoilPatchLayerBulkDensityOvenDry_tPm3:
      sideEffects_ovenDryBulkDensity_tPm3(index, updateList);
    kSoilPatchLayerOrganicMatter_tPha:
      sideEffects_organicMatter_tPha(index, updateList);
    kSoilPatchLayerSoilpH:
      sideEffects_soilpH(index, updateList);
    kSoilPatchLayerCalciumCarbonate_pct:
      sideEffects_calciumCarbonate_pct(index, updateList);
    kSoilPatchLayerBaseFormingCations_cmolPkg:
      sideEffects_baseFormingCations_cmolPkg(index, updateList);
    kSoilPatchLayerCationExchangeCapacity_cmolPkg:
      sideEffects_cationExchangeCapacity_cmolPkg(index, updateList);
    kSoilPatchLayerClayContent_pct:
      sideEffects_clayContent_pct(index, updateList);
    kSoilPatchLayerSandContent_pct:
      sideEffects_sandContent_pct(index, updateList);
    kSoilPatchLayerSiltContent_pct:
      sideEffects_siltContent_pct(index, updateList);
    kSoilPatchLayerRockContent_pct:
      sideEffects_rockContent_pct(index, updateList);
    end;
  except on e: Exception do errorMessage('Exception in soil patch transferField: ' + e.message); end;
  end;

procedure GsSoilPatch.directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  SoilPatch_directTransferField(self, v, d, fieldID, ft, index, deriveMethod, updateList);
  self.addToUpdateList(fieldID, index, updateList);
  end;

{ sideEffects functions }
procedure GsSoilPatch.sideEffects_slopeSteepness_mPm(updateList: TListCollection);
  var layer: integer;
  begin
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    layers[layer].travelTimeFactor := layers[layer].saturatedConductivity_mmPhr * params.watershedSlopeSteepness_mPm;
  if updateList <> nil then
    self.addToUpdateList(kSoilPatchLayerTravelTimeFactor, -1, updateList);
  end;

procedure GsSoilPatch.sideEffects_watershedSlopeLength_ha(updateList: TListCollection);
  var
    squareRootOfArea: single;
  begin
  squareRootOfArea := sqrt(ha_to_m2 * params.watershedArea_ha);
  if params.watershedSlopeLength_m > squareRootOfArea then
    params.watershedSlopeLength_m := squareRootOfArea;
  end;

procedure GsSoilPatch.sideEffects_watershedArea_ha(updateList: TListCollection);
  var
    squareRootOfArea: single;
  begin
  squareRootOfArea := sqrt(ha_to_m2 * params.watershedArea_ha);
  if params.watershedSlopeLength_m > squareRootOfArea then
    begin
    params.watershedSlopeLength_m := squareRootOfArea;
    self.addToUpdateList(kSoilPatchWatershedSlopeLength_m, 0, updateList);
    end;
  end;

procedure GsSoilPatch.sideEffects_waterTableMinDepth_m(updateList: TListCollection);
  begin
  if params.waterTableMinDepth_m > params.waterTableMaxDepth_m then
    params.waterTableMinDepth_m := params.waterTableMaxDepth_m;
  if water.waterTableDepth_m < params.waterTableMinDepth_m then
    begin
    water.waterTableDepth_m := params.waterTableMinDepth_m;
    self.addToUpdateList(kSoilPatchWaterTableDepth_m, 0, updateList);
    end;
  end;

procedure GsSoilPatch.sideEffects_waterTableMaxDepth_m(updateList: TListCollection);
  begin
  if params.waterTableMaxDepth_m < params.waterTableMinDepth_m then
    params.waterTableMaxDepth_m := params.waterTableMinDepth_m;
  if water.waterTableDepth_m > params.waterTableMaxDepth_m then
    begin
    water.waterTableDepth_m := params.waterTableMaxDepth_m;
    self.addToUpdateList(kSoilPatchWaterTableDepth_m, 0, updateList);
    end;
  end;

procedure GsSoilPatch.sideEffects_waterTableDepth_m(updateList: TListCollection);
  begin
  if water.waterTableDepth_m < params.waterTableMinDepth_m then
    water.waterTableDepth_m := params.waterTableMinDepth_m;
  if water.waterTableDepth_m > params.waterTableMaxDepth_m then
    water.waterTableDepth_m := params.waterTableMaxDepth_m;
  end;

procedure GsSoilPatch.sideEffects_mulchFlatCropResidue_tPha(updateList: TListCollection);
  begin
  mulch.depth_m := Utils_MulchDepthFromMulchFlatCropResidue_m(mulch.flatCropResidue_tPha);
  self.addToUpdateList(kSoilPatchMulchDepth_m, 0, updateList);
  self.PatchTotalAboveGroundBiomassAndResidue;
  self.addToUpdateList(kSoilPatchPatchTotalAboveGroundBiomassAndResidue_tPha, 0, updateList);
  end;

procedure GsSoilPatch.sideEffects_mulchDepth_m(updateList: TListCollection);
  begin
  mulch.flatCropResidue_tPha := Utils_MulchFlatCropResidueFromMulchDepth_tPha(mulch.depth_m);
  self.addToUpdateList(kSoilPatchMulchFlatCropResidue_tPha, 0, updateList);
  self.PatchTotalAboveGroundBiomassAndResidue;
  self.addToUpdateList(kSoilPatchPatchTotalAboveGroundBiomassAndResidue_tPha, 0, updateList);
  end;

procedure GsSoilPatch.sideEffects_layerWeight_tPha(index: smallint; oldWeight_tPha: single;
    updateList: TListCollection);
  var
    changeInWeight_tPha: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  changeInWeight_tPha := layers[index].weight_tPha - oldWeight_tPha;
  self.AddOrRemoveSoilBasedOnLayerWeight(index, changeInWeight_tPha);
  self.wholeModelUpdateNeeded := true;
  end;

procedure GsSoilPatch.sideEffects_layerDepth_m(index: smallint; oldDepth_m: single;
    updateList: TListCollection);
  var
    changeInDepth_m: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  changeInDepth_m := layers[index].depth_m - oldDepth_m;
  self.AddOrRemoveSoilBasedOnLayerDepth(index, changeInDepth_m);
  self.wholeModelUpdateNeeded := true;
  end;

procedure GsSoilPatch.sideEffects_wiltingPoint_mm(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  Utils_CheckRelationOfFieldCapacityToPorosity(layers[index]);
  self.addToUpdateList(kSoilPatchLayerFieldCapacity_mm, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_fieldCapacity_mm(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  Utils_CheckRelationOfFieldCapacityToPorosity(layers[index]);
  self.addToUpdateList(kSoilPatchLayerWiltingPoint_mm, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_porosity_mm(index: smallint; updateList: TListCollection);
  var
    layerThickness_m: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  if index = 0 then
    layerThickness_m := layers[index].depth_m
  else
    layerThickness_m := layers[index].depth_m - layers[index-1].depth_m;
  { recalculate bulk density and weight based on new porosity }
  layers[index].bulkDensity_tPm3 := Utils_BulkDensityFromPorosityRockContentAndThickness(
    layers[index].porosity_mm, layers[index].rockContent_pct, layerThickness_m);
  self.addToUpdateList(kSoilPatchLayerBulkDensity_tPm3, index, updateList);
  layers[index].weight_tPha := Utils_SoilWeightFromBulkDensityAndThickness_tPha(
    layers[index].bulkDensity_tPm3, layerThickness_m);
  self.addToUpdateList(kSoilPatchLayerWeight_tPha, index, updateList);
  { don't change settled bulk density }
  Utils_CheckRelationOfFieldCapacityToPorosity(layers[index]);
  self.addToUpdateList(kSoilPatchLayerWiltingPoint_mm, index, updateList);
  self.addToUpdateList(kSoilPatchLayerFieldCapacity_mm, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_bulkDensity_tPm3(index: smallint; updateList: TListCollection);
  var
    layerThickness_m: single;
  begin
  { assumption: only one layer value is changed at a time }
 if (index < 0) or (index > state.numLayers-1) then exit;
 { make sure bulk density is not higher than settled }
 if layers[index].bulkDensity_tPm3 > layers[index].settledBulkDensity_tPm3 then
   layers[index].bulkDensity_tPm3 := layers[index].settledBulkDensity_tPm3;
 if index = 0 then
   layerThickness_m := layers[index].depth_m
 else
   layerThickness_m := layers[index].depth_m - layers[index-1].depth_m;
 { recalculate porosity and weight based on new bulk density }
 layers[index].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
     layers[index].bulkDensity_tPm3, layers[index].rockContent_pct, layerThickness_m);
 self.addToUpdateList(kSoilPatchLayerPorosity_mm, index, updateList);
 layers[index].weight_tPha := Utils_SoilWeightFromBulkDensityAndThickness_tPha(
     layers[index].bulkDensity_tPm3, layerThickness_m);
 self.addToUpdateList(kSoilPatchLayerWeight_tPha, index, updateList);
 Utils_CheckRelationOfFieldCapacityToPorosity(layers[index]);
 self.addToUpdateList(kSoilPatchLayerWiltingPoint_mm, index, updateList);
 self.addToUpdateList(kSoilPatchLayerFieldCapacity_mm, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_settledBulkDensity_tPm3(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  { make sure bulk density is not higher than settled (bulk density gets pushed down) }
  if layers[index].bulkDensity_tPm3 > layers[index].settledBulkDensity_tPm3 then
    begin
    layers[index].bulkDensity_tPm3 := layers[index].settledBulkDensity_tPm3;
    self.addToUpdateList(kSoilPatchLayerBulkDensity_tPm3, index, updateList);
    end;
  end;

procedure GsSoilPatch.sideEffects_ovenDryBulkDensity_tPm3(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  { make sure oven-dry bulk density is not lower than settled (oven-dry stops) }
  if layers[index].bulkDensityOvenDry_tPm3 < layers[index].settledBulkDensity_tPm3 then
    layers[index].bulkDensityOvenDry_tPm3 := layers[index].settledBulkDensity_tPm3;
  end;

procedure GsSoilPatch.sideEffects_organicMatter_tPha(index: smallint; updateList: TListCollection);
  var
    organicC_pct, organicMatterFraction_frn, oldBulkDensity_tPm3, bulkDensityInOM_tPm3, layerThickness_m: single;
    baseSaturation_pct: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  { a ratio  of about 1:1.7 can be assumed to exist between the organic carbon and the soil humus }
  organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[index].organicMatter_tPha, layers[index].weight_tPha);
  organicMatterFraction_frn := organicC_pct * pct_to_frn * 1.72;
  { adjust aluminum saturation }
  baseSaturation_pct := frn_to_pct * safediv(layers[index].baseFormingCations_cmolPkg,
      layers[index].cationExchangeCapacity_cmolPkg);
  layers[index].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct,
      organicC_pct, layers[index].soilpH);
  self.addToUpdateList(kSoilPatchLayerAluminumSaturation_pct, index, updateList);
  { adjust bulk density for amount of organic matter }
  bulkDensityInOM_tPm3 := safediv(1.0 - organicMatterFraction_frn, safediv(1.0,
      layers[index].bulkDensity_tPm3) - organicMatterFraction_frn / 0.224);
  if (bulkDensityInOM_tPm3 >= 2.5) or (bulkDensityInOM_tPm3 <= layers[index].bulkDensity_tPm3) then
    begin
    oldBulkDensity_tPm3 := layers[index].bulkDensity_tPm3;
    layers[index].bulkDensity_tPm3 := safediv(1.0, organicMatterFraction_frn / 0.224 + (1.0 -
        organicMatterFraction_frn) / 2.0);
    { if any change made, update list and make other changes }
    if layers[index].bulkDensity_tPm3 <> oldBulkDensity_tPm3 then
      begin
      self.addToUpdateList(kSoilPatchLayerBulkDensity_tPm3, 0, updateList);
      { adjust settled bulk density by same amount }
      layers[index].settledBulkDensity_tPm3 := layers[index].settledBulkDensity_tPm3
          * safediv(layers[index].bulkDensity_tPm3, oldBulkDensity_tPm3);
      self.addToUpdateList(kSoilPatchLayerSettledBulkDensity_tPm3, 0, updateList);
      { adjust porosity for change in bulk density }
      if index = 0 then
        layerThickness_m := layers[index].depth_m
      else
        layerThickness_m := layers[index].depth_m - layers[index-1].depth_m;
      layers[index].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
          layers[index].bulkDensity_tPm3, layers[index].rockContent_pct, layerThickness_m);
      self.addToUpdateList(kSoilPatchLayerPorosity_mm, index, updateList);
      Utils_CheckRelationOfFieldCapacityToPorosity(layers[index]);
      self.addToUpdateList(kSoilPatchLayerWiltingPoint_mm, index, updateList);
      self.addToUpdateList(kSoilPatchLayerFieldCapacity_mm, index, updateList);
      end;
    end;
  end;

procedure GsSoilPatch.sideEffects_calciumCarbonate_pct(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  if (layers[index].calciumCarbonate_pct > 0.0) then
    begin
    layers[index].baseFormingCations_cmolPkg := layers[index].cationExchangeCapacity_cmolPkg;
    self.addToUpdateList(kSoilPatchLayerBaseFormingCations_cmolPkg, index, updateList);
    end;
  end;

procedure GsSoilPatch.sideEffects_baseFormingCations_cmolPkg(index: smallint; updateList: TListCollection);
  var
    baseSaturation_pct, organicC_pct: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  { base forming cations cannot be greater than CEC (bfc stops) }
  if (layers[index].baseFormingCations_cmolPkg > layers[index].cationExchangeCapacity_cmolPkg) then
    layers[index].baseFormingCations_cmolPkg := layers[index].cationExchangeCapacity_cmolPkg;
  baseSaturation_pct := frn_to_pct * safediv(layers[index].baseFormingCations_cmolPkg,
      layers[index].cationExchangeCapacity_cmolPkg);
  organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[index].organicMatter_tPha, layers[index].weight_tPha);
  layers[index].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct,
      organicC_pct, layers[index].soilpH);
  self.addToUpdateList(kSoilPatchLayerAluminumSaturation_pct, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_soilpH(index: smallint; updateList: TListCollection);
  var
    baseSaturation_pct, organicC_pct: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  baseSaturation_pct := frn_to_pct * safediv(layers[index].baseFormingCations_cmolPkg,
      layers[index].cationExchangeCapacity_cmolPkg);
  organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[index].organicMatter_tPha, layers[index].weight_tPha);
  layers[index].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct,
      organicC_pct, layers[index].soilpH);
  self.addToUpdateList(kSoilPatchLayerAluminumSaturation_pct, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_cationExchangeCapacity_cmolPkg(index: smallint; updateList: TListCollection);
  var
    baseSaturation_pct, organicC_pct: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  { base forming cations cannot be greater than CEC (bfc gets pushed down) }
  if (layers[index].baseFormingCations_cmolPkg > layers[index].cationExchangeCapacity_cmolPkg) then
    begin
    layers[index].baseFormingCations_cmolPkg := layers[index].cationExchangeCapacity_cmolPkg;
    self.addToUpdateList(kSoilPatchLayerBaseFormingCations_cmolPkg, index, updateList);
    end;
  baseSaturation_pct := frn_to_pct * safediv(layers[index].baseFormingCations_cmolPkg,
      layers[index].cationExchangeCapacity_cmolPkg);
  organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[index].organicMatter_tPha, layers[index].weight_tPha);
  layers[index].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct,
      organicC_pct, layers[index].soilpH);
  self.addToUpdateList(kSoilPatchLayerAluminumSaturation_pct, index, updateList);
  end;

function pctBound(value: single): single;
  begin
  result := min(100.0, max(0.0, value));
  end;

procedure GsSoilPatch.sideEffects_clayContent_pct(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  if layers[index].clayContent_pct + layers[index].sandContent_pct > 100.0 then
    begin
    layers[index].sandContent_pct := pctBound(100.0 - layers[index].clayContent_pct);
    self.addToUpdateList(kSoilPatchLayerSandContent_pct, index, updateList);
    end;
  layers[index].siltContent_pct := pctBound(100.0 - layers[index].clayContent_pct - layers[index].sandContent_pct);
  self.addToUpdateList(kSoilPatchLayerSiltContent_pct, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_sandContent_pct(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  if layers[index].sandContent_pct + layers[index].clayContent_pct > 100.0 then
    begin
    layers[index].clayContent_pct := pctBound(100.0 - layers[index].sandContent_pct);
    self.addToUpdateList(kSoilPatchLayerClayContent_pct, index, updateList);
    end;
  layers[index].siltContent_pct := pctBound(100.0 - layers[index].sandContent_pct - layers[index].clayContent_pct);
  self.addToUpdateList(kSoilPatchLayerSiltContent_pct, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_siltContent_pct(index: smallint; updateList: TListCollection);
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  if layers[index].siltContent_pct + layers[index].clayContent_pct > 100.0 then
    begin
    layers[index].clayContent_pct := pctBound(100.0 - layers[index].siltContent_pct);
    self.addToUpdateList(kSoilPatchLayerClayContent_pct, index, updateList);
    end;
  layers[index].sandContent_pct := pctBound(100.0 - layers[index].siltContent_pct - layers[index].clayContent_pct);
  self.addToUpdateList(kSoilPatchLayerSandContent_pct, index, updateList);
  end;

procedure GsSoilPatch.sideEffects_rockContent_pct(index: smallint; updateList: TListCollection);
  var
    layerThickness_m: single;
  begin
  { assumption: only one layer value is changed at a time }
  if (index < 0) or (index > state.numLayers-1) then exit;
  { recalculate porosity based on new rock content }
  if index = 0 then
    layerThickness_m := layers[index].depth_m
  else
    layerThickness_m := layers[index].depth_m - layers[index-1].depth_m;
  layers[index].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
      layers[index].bulkDensity_tPm3, layers[index].rockContent_pct, layerThickness_m);
  self.addToUpdateList(kSoilPatchLayerPorosity_mm, index, updateList);
  { absolute values in browser will need to be recalculated }
  self.wholeModelUpdateNeeded := true;
  end;

{ conversion functions }
{USAGE:
      id: BDConvert(accessor, v, fieldID, deriveMethod, ft, d, index, updateList);
}
procedure GsSoilPatch.BDConvert(var modelValue: single; var value;
    fieldID, deriveMethod, fieldType, direction, index: integer; updateList: TListCollection);
  begin
  case deriveMethod of
    kDeriveTypeUndefined: self.MFD(modelValue, value, fieldType, direction);
    kDeriveTypeDepth: self.convertWaterAbsoluteAndRelative(direction, modelValue, value, index);
    kDeriveTypeConcentration: self.convertNutrientsAbsoluteAndRelative(direction, modelValue, value, index);
    kDeriveTypeArea: self.ConvertRelativeAndAbsolute(direction, modelValue, value);
    kDeriveTypeConcentrationFromPercent:
      raise Exception.create('Concentration from percent derivation not supported by soil patch');
    else
      raise Exception.create('Derivation method not supported by soil patch');
    end;
  self.addToUpdateList(fieldID, index, updateList);
  end;

procedure GsSoilPatch.ConvertRelativeAndAbsolute(direction: integer; var modelValue: single; var value);
  begin
  try
  if (direction = kSetField) then
    begin
    modelValue := safediv(single(value), params.area_ha);
    end
  else
    begin
    single(value) := modelValue * params.area_ha;
    end;
  except on e: Exception do errorMessage('Problem in soil ConvertRelativeAndAbsolute: ' + e.message); end;
  end;

procedure GsSoilPatch.convertWaterAbsoluteAndRelative(d: integer; var modelValue: single; var value; layer: integer);
  var
    i: integer;
    nonRockContent_frn, thicknessThisLayer_mm: single;
  begin
  try
  if layer <> -1 then
    begin
    nonRockContent_frn := 1.0 - layers[layer].rockContent_pct * pct_to_frn;
    if layer <> 0 then
      thicknessThisLayer_mm := (layers[layer].depth_m - layers[layer-1].depth_m) * m_to_mm
    else
      thicknessThisLayer_mm := layers[layer].depth_m * m_to_mm;
    end
  else
    begin
    { if not one layer, use mean rock content of all layers }
    nonRockContent_frn := 0.0;
    for i := 0 to state.numLayers - 1 do
      nonRockContent_frn := nonRockContent_frn + 1.0 - layers[i].rockContent_pct * pct_to_frn;
    nonRockContent_frn := safediv(nonRockContent_frn, state.numLayers);
    { if not one layer, use whole depth of soil }
    thicknessThisLayer_mm := layers[state.numLayers-1].depth_m * m_to_mm;
    end;
  if d = kSetField then
    begin
    modelValue := single(value) * nonRockContent_frn * thicknessThisLayer_mm;
    end
  else
    begin
    single(value) := safediv(modelValue, nonRockContent_frn * thicknessThisLayer_mm);
    end;
  except on e: Exception do errorMessage('Problem in soil convertWaterAbsoluteAndRelative: ' + e.message); end;
  end;

procedure GsSoilPatch.convertNutrientsAbsoluteAndRelative(d: integer; var modelValue: single; var value; layer: integer);
  var
    i: integer;
    conversionFactor_tkgPgha: single;
  begin
  try
  { convert g/t to kg/ha for things entered as concentrations (g/t * t/ha = g/ha * (kg/1000g) = kg/ha) }
  if layer <> -1 then
    conversionFactor_tkgPgha := layers[layer].weight_tPha * g_to_kg
  else
    begin
    { if not one layer, use total soil weight }
    conversionFactor_tkgPgha := 0.0;
    for i := 0 to state.numLayers - 1 do
      conversionFactor_tkgPgha := conversionFactor_tkgPgha + layers[i].weight_tPha;
    conversionFactor_tkgPgha := conversionFactor_tkgPgha * g_to_kg;
    end;
  if d = kSetField then
    begin
    modelValue := single(value) * conversionFactor_tkgPgha;
    end
  else
    begin
    single(value) := safediv(modelValue, conversionFactor_tkgPgha);
    end;
  except on e: Exception do errorMessage('Problem in soil convertNutrientsAbsoluteAndRelative: ' + e.message); end;
  end;

function GsSoilPatch.valueForLayerArray(model: GsModel; arrayShowType, fieldNumber: smallint;
    arraySelected: selectionArray; deriveMethod: smallint): single;
  var
    i, arraySelectedIndex, numArrayIndexesSelected: smallint;
  	valueSingle, weightedMean: single;
    totalSoilWeight_tPha, totalSoilThickness_m, layerThickness_m, lastDepth_m, weightedValue: single;
  begin
  result := 0.0;
  try
  numArrayIndexesSelected := GsGroupItem.numberOfArrayIndexesSelected(arraySelected);
  case arrayShowType of
    kArrayShowMeanWeightedBySoilLayerThickness:
      begin
      weightedMean := 0.0;
      totalSoilThickness_m := 0.0;
      for i := 0 to state.numLayers - 1 do if arraySelected[i] then
        begin
        if i = 0 then
          lastDepth_m := 0.0
        else
          lastDepth_m := layers[i-1].depth_m;
        totalSoilThickness_m := totalSoilThickness_m + layers[i].depth_m - lastDepth_m;
        end;
      for i := 0 to state.numLayers - 1 do if arraySelected[i] then
        begin
        model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, i, deriveMethod, nil);
        if i = 0 then
          lastDepth_m := 0.0
        else
          lastDepth_m := layers[i-1].depth_m;
        layerThickness_m := layers[i].depth_m - lastDepth_m;
        if totalSoilThickness_m <> 0 then
          weightedValue := valueSingle * safediv(layerThickness_m, totalSoilThickness_m)
        else
          weightedValue := 0.0;
        weightedMean := weightedMean + weightedValue;
        end;
      result := weightedMean;
      end;
    kArrayShowMeanWeightedBySoilLayerWeight:
      begin
      weightedMean := 0.0;
      totalSoilWeight_tPha := 0.0;
      for i := 0 to state.numLayers - 1 do if arraySelected[i] then
        addQuantity(totalSoilWeight_tPha, layers[i].weight_tPha);
      for i := 0 to state.numLayers - 1 do if arraySelected[i] then
        begin
        model.TransferField(kGetField, valueSingle, fieldNumber, kFieldFloat, i, deriveMethod, nil);
        if totalSoilWeight_tPha <> 0.0 then
          weightedValue := valueSingle * safediv(layers[i].weight_tPha, totalSoilWeight_tPha)
        else
          weightedValue := 0.0;
        weightedMean := weightedMean + weightedValue;
        end;
      result := weightedMean;
      end;
    end;
  except on e: Exception do result := errorMessage('Problem in soil valueForLayerArray: ' + e.message); end;
  end;

procedure GsSoilPatch.AddOrRemoveSoilBasedOnLayerWeight(layer: smallint; changeInWeight_tPha: single);
  var
    oldThickness_m, newThickness_m, oldbulkDensity_tPm3: single;
  begin
  { assumes weight has already been set for layer in question }
  { for layer zero, cannot change thickness, so change bulk density and porosity }
  if layer = 0 then
    begin
    oldbulkDensity_tPm3 := layers[0].bulkDensity_tPm3;
    layers[0].bulkDensity_tPm3 := Utils_BulkDensityFromSoilWeightAndThickness_tPM3(
        layers[0].weight_tPha{already set}, layers[0].depth_m {same as thickness for layer 0});
    { change settled bulk density by same ratio }
    if layers[0].bulkDensity_tPm3 <> oldBulkDensity_tPm3 then
      layers[0].settledBulkDensity_tPm3 := layers[0].settledBulkDensity_tPm3
          * safedivExcept(layers[0].bulkDensity_tPm3, oldBulkDensity_tPm3, 1.0);
    { recalculate porosity }
    layers[0].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(layers[0].bulkDensity_tPm3,
        layers[0].rockContent_pct, layers[0].depth_m {same as thickness for layer 0});
    Utils_CheckRelationOfFieldCapacityToPorosity(layers[0]);
    end
  else
    { for all layers but zero, change thickness and keep bulk density and porosity the same }
    begin
    oldThickness_m := layers[layer].depth_m - layers[layer-1].depth_m;
    newThickness_m := Utils_SoilThicknessFromWeightAndBulkDensity_m(layers[layer].weight_tPha{already set},
      layers[layer].bulkDensity_tPm3);
    { function we are calling wants depth already set }
    layers[layer].depth_m := layers[layer].depth_m + (newThickness_m - oldThickness_m);
    self.AddOrRemoveSoilBasedOnLayerDepth(layer, newThickness_m - oldThickness_m);
    end;
  end;

procedure GsSoilPatch.AddOrRemoveSoilBasedOnLayerDepth(layer: smallint; changeInDepth_m: single);
  var
    layerThickness_m, oldThickness_m, newWeight_tPha, weightConversion, thicknessConversion: single;
  begin
  { assumes depth has already been set for layer in question }
  try
  if layer = 0 then
    exit; { first layer depth can't change }
  layerThickness_m := layers[layer].depth_m - layers[layer-1].depth_m;
  oldThickness_m := layerThickness_m - changeInDepth_m;
  if layerThickness_m < kFirstLayerDepth_m then
    begin
    layers[layer].depth_m := layers[layer-1].depth_m + kFirstLayerDepth_m;
    layerThickness_m := layers[layer].depth_m - layers[layer-1].depth_m;
    changeInDepth_m := oldThickness_m - kFirstLayerDepth_m;
    end;
  { calculate new layer weight from change in thickness using same bulk density }
  newWeight_tPha := Utils_SoilWeightFromBulkDensityAndThickness_tPha(
      layers[layer].bulkDensity_tPm3, layerThickness_m);
  { for each quantity, multiply by new weight / old weight to get new quantity }
  weightConversion := safedivExcept(newWeight_tPha, layers[layer].weight_tPha, 1.0);
  layers[layer].nitrate_kgPha := layers[layer].nitrate_kgPha * weightConversion;
  layers[layer].ammonia_kgPha := layers[layer].ammonia_kgPha * weightConversion;
  layers[layer].organicNFresh_kgPha := layers[layer].organicNFresh_kgPha * weightConversion;
  layers[layer].organicNActiveHumus_kgPha := layers[layer].organicNActiveHumus_kgPha * weightConversion;
  layers[layer].organicNStableHumus_kgPha := layers[layer].organicNStableHumus_kgPha * weightConversion;
  layers[layer].organicPFresh_kgPha := layers[layer].organicPFresh_kgPha * weightConversion;
  layers[layer].organicPHumus_kgPha := layers[layer].organicPHumus_kgPha * weightConversion;
  layers[layer].mineralPActive_kgPha := layers[layer].mineralPActive_kgPha * weightConversion;
  layers[layer].mineralPStable_kgPha := layers[layer].mineralPStable_kgPha * weightConversion;
  layers[layer].flatCropResidue_tPha := layers[layer].flatCropResidue_tPha * weightConversion;
  layers[layer].organicMatter_tPha := layers[layer].organicMatter_tPha * weightConversion;
  { change water content things based on thickness change, not weight change }
  thicknessConversion := safedivExcept(oldThickness_m, layerThickness_m, 1.0);
  layers[layer].waterContent_mm := max(0.0, layers[layer].waterContent_mm * thicknessConversion);
  layers[layer].fieldCapacity_mm := max(0.0, layers[layer].fieldCapacity_mm * thicknessConversion);
  layers[layer].wiltingPoint_mm := max(0.0, layers[layer].wiltingPoint_mm * thicknessConversion);
  { recalculate porosity from bulk density and re-check fc-po relationship }
  layers[layer].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
      layers[layer].bulkDensity_tPm3, layers[layer].rockContent_pct, layerThickness_m);
  Utils_CheckRelationOfFieldCapacityToPorosity(layers[layer]);
  { finally set new layer weight and deal with repercussions on depths of other layers }
  layers[layer].weight_tPha := newWeight_tPha;
  { if not last layer, drive all layers below this one down (or pull them up) keeping their same thicknesses }
  self.adjustLowerLayerDepthsForChangeToOneLayer(layer, changeInDepth_m);
  except on e: Exception do errorMessage('Problem in soil AddOrRemoveSoilBasedOnLayerThickness: ' + e.message); end;
  end;

procedure GsSoilPatch.streamUsingTextFiler(textFiler: GsTextFiler);
  begin
  if textFiler.versionString = 'v0.9' then
    SoilPatch_streamUsingTextFilerVersion090(self, textFiler)
  else if textFiler.versionString = 'v1.0' then
    SoilPatch_streamUsingTextFilerVersionOne(self, textFiler);
  end;

end.

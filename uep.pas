unit uep;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uep: Second-level EPIC functions (EP = EPIC Procedures). These are most of the functions
called by the next day functions in the soil, plants and weather. These functions in turn
call the EQ (EPIC eQuations) functions (ueq, and ueqh for hydrology functions).
Some of this code is directly as translated from EPIC; some has been modified by us.
The most major modification in this file is the change of N and P and water allocation
to model competing plants in a soil patch. Otherwise there are many small changes, some
of which are marked. Some of the pesticide transport code remains (commented out)
from when we had it working. We also made some changes in the methods used for splitting
soil layers. And we changed porosity so it maintains a closer relationship to bulk
density instead of following along with field capacity and wilting point.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses
  uestruct, classes, umodel;

type

  EP = class
  public
  { weather }
  class function RainfallForDay_mm(month: integer; itRainedYesterday: boolean;
    var weatherParams: WeatherParamsStructure; var stationVars: WeatherStationVarsStructure;
    var yesterdaysUniformRandNumForRainfall_frn: single): single;
  class procedure MinAndMaxTemperatureForDay_degC(month: integer; var weatherParams: WeatherParamsStructure; var
    dailyWeather: DailyWeatherStructure; var matrices: MatricesStructure; meanMaxTempForDay_degC: single);
  class function RadiationForDay_MJPm2(maxPossibleRadiation_MJPm2: single; meanRadiationForDay_MJPm2: single;
    radiationCorrelationMultiplier: single): single;
  class function RelativeHumidityForDay_frn(meanRelHumForDay_frn: single): single;
  class function MaxPossibleRadiation_MJPm2(julianDay: integer;
      stationLatitude_rad: single; declinationAngleOfSun_rad: single): single;
  class function EnrichmentRatioForNPPest(var water: WaterStructure; var erosion: ErosionStructure): single;
  { soil }
  class procedure SoilTemperatureByLayer(numLayers: integer; var layers: LayerStructureArray; var surface:
    SurfaceStructure; var totals: TotalsStructure; probWetDayForMonth_frn: single; var dailyWeather: DailyWeatherStructure;
    snowWaterContent_mm: single; meanMonthlyMeanMeanTempForYear_degC: single; var soilInsulationFromAirTempCoeffs:
    sCurveStructure; var movement: MovementStructure);
  class procedure RunoffAndErosionParams(var water: WaterStructure; var erosion: ErosionStructure;
    meanPropRainInFirstHalfHourForMonth_frn: single);
  class function RunoffVolume_mm(numLayers: integer; var layers: LayerStructureArray;
    var params: SoilParamsStructure; var water: WaterStructure; var surface: SurfaceStructure;
    patchMeanCropHeight_m: single): single;
  class procedure AdjustCurveNumberForSlope(var water: WaterStructure; var params: SoilParamsStructure;
    numLayers: integer; var layers: LayerStructureArray);
  class function PeakRunoffRate_m3Psec(var params: SoilParamsStructure; var water: WaterStructure): single;
  class procedure PercAndLatFlowByLayer_mm(numLayers: integer; layerWithDrainageSystem: integer;
    var params: SoilParamsStructure; var water: WaterStructure; var layers: LayerStructureArray;
    inflowingWater_mm: single; totalRunoff_mm: single; allowPercolation, allowLateralFlow: boolean);
  class procedure PercAndLatFlowForLayer_mm(layer: integer; layerWithDrainageSystem: integer; var layers:
    LayerStructureArray; slopeSteepness_mPm: single; var percolationForLayer_mm: single; var lateralFlowForLayer_mm:
    single; returnFlowTravelTime_days: single; timeForDrainageSystemToReducePlantStress_days: single;
    allowPercolation, allowLateralFlow: boolean);
  class function PotentialPlantEvap_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure; var stationVars: WeatherStationVarsStructure;
    prevThreeDaysMeanTemp_degC: arrayThree; albedo_frn: single;
    potSoilEvap_mm: single; plantList: TList): single;
  class function PotentialSoilEvap_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure; var stationVars: WeatherStationVarsStructure;
    prevThreeDaysMeanTemp_degC: arrayThree; albedo_frn: single;
    meanPlantHeight_m: single; numPlants: integer): single;
  class function PotSoilEvapByPenmanMonteith_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure;  var stationVars: WeatherStationVarsStructure;
    soilHeatFlux_MJPM2: single; netRadiationForDay_MJPm2: single;
    meanPlantHeight_m: single; numPlants: integer): single;
  class function PotPlantEvapByPenmanMonteith_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure; var stationVars: WeatherStationVarsStructure;
    soilHeatFlux_MJPM2: single; netRadiationForDay_MJPm2: single; plantProxy: GsModel): single;
  class function PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm(soilCoverIndex_frn: single; potentialSoilEvap_mm:
    single; potPlantEvap_mm: single): single;
  class function ActualSoilEvaporation_mm(numLayers: integer; var layers: LayerStructureArray; var
    snowWaterContent_mm: single; potSoilEvap_mm: single; plowDepth_m: single;
    lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single; var soilEvapByDepthCoeffs: sCurveStructure;
    var lowestLayerInWhichSoilEvapOccured: smallint): single;
  class function Snowmelt_mm(surfaceSoilTemp_degC: single; meanTempForDay_degC: single; snowWaterContent_mm: single):
    single;
  class procedure WaterTableDynamics(var water: WaterStructure; var params: SoilParamsStructure;
    numLayers: integer; var layers: LayerStructureArray; var totals: TotalsStructure);
  class function WaterErosionForDay_tPha(var params: SoilParamsStructure; var erosion: ErosionStructure;
    patchMeanMinCropManagementFactor: single; var layers: LayerStructureArray; var water: WaterStructure;
    var userCoeffsForMUSI: arrayFour; patchTotalAboveGroundBiomassAndResidue_tPha: single): single;
  class function WindErosionForDay_tPha(var params: SoilParamsStructure; var dailyWeather: DailyWeatherStructure;
    var surface: SurfaceStructure; var plantMeans: PlantMeansStructure; var layers: LayerStructureArray;
    var erosion: ErosionStructure; dailyMeanWindSpeedForMonth_mPsec: single): single;
  class function IntegratePotentialWindErosion(dailyMeanWindSpeedForMonth_mPsec: single; meanWindSpeedForDay_mPsec:
    single; thresholdFrictionVelocityWE_mPsec: single; weForFractionOfDayFactor: single; meanUnsheltDistanceFactorWE:
    single): single;
  class function WindErosionForFractionOfDay_kgPm(meanWindSpeedForDay_mPsec: single; fractionOfDayWindSpeedAtTime_frn:
    single; weForFractionOfDayFactor: single; meanUnsheltDistanceFactorWE: single): single;
  class procedure PFluxBetweenLabileAndMineral(layer: integer; var layers: LayerStructureArray;
    allowLabileAndMineralPFlow, allowActiveAndStableMineralPFlow: boolean);
  class procedure NAndPLossInRunoffAndLeachingForSurfaceLayer(var layers: LayerStructureArray; var
    nitrateEnteringLayer_kgPha: single; var labilePEnteringLayer_kgPha: single; nitrogenLeachingParam: single; var
    water: WaterStructure; var movement: MovementStructure;
    allowNLossInRunoff, allowPLossInRunoff, allowNLossInLateralFlow,
    allowNLossInPercolation, allowPLossInPercolation: boolean);
  class procedure LeachNandPNonSurfaceLayer(layer: integer; var layers: LayerStructureArray; var
    nitrateEnteringLayer_kgPha: single; var labilePEnteringLayer_kgPha: single; nitrogenLeachingParam: single;
    allowNLossInPercolation, allowPLossInPercolation, allowNLossInLateralFlow: boolean);
  class procedure NitrificationAndVolatilization(layer: integer; var layers: LayerStructureArray;
    meanWindSpeedForDay_mPsec: single; var nVolatilizationByDepthCoeffs: SCurveStructure;
    allowNitrification, allowVolatilization: boolean);
  class procedure NFluxBetweenActiveAndStableHumus(layer: integer; var layers: LayerStructureArray);
  class procedure Mineralization(layer: integer; var layers: LayerStructureArray;
    plowDepthSettledBulkDensityAtInput_tPm3: single; var movement: MovementStructure;
    allowActiveHumusNMineralization, allowActiveHumusPMineralization, allowFreshNMineralization,
    allowFreshPMineralization: boolean);
  class procedure DailyStandingDeadToFlatResidue(var layers: LayerStructureArray; var mulch: MulchStructure;
    rainfallForDay_mm: single; plantList: TList; var plantMeans: PlantMeansStructure);
  { no code for this function
  class procedure Immobilization(layer: integer; var layers: LayerStructureArray); }
  class procedure Denitrification(layer: integer; var layers: LayerStructureArray);
  class procedure OrganicNAndCLossInSediment(totalErosion_tPha: single; enrichmentRatioForNPPest: single; var layers:
    LayerStructureArray; var movement: MovementStructure;
    takeOrganicNActiveHumus, takeOrganicNStableHumus, takeNitrate, takeAmmonia, takeOrganicNFresh, takeOrganicMatter,
    takeFlatCropResidue: boolean);
  class procedure PLossInSediment(totalErosion_tPha: single; enrichmentRatioForNPPest: single; var layers:
    LayerStructureArray; var movement: MovementStructure;
    takeOrganicPHumus, takeLabileP, takeMineralPActive, takeMineralPStable, takeOrganicPFresh: boolean);
  class procedure RemoveErodedSoil(soilProxy: GsModel; amountEroded_tPha: single);
  class procedure CreateNewLayerAtInputBySplittingThickest(soilProxy: GsModel;
    firstLayerThisThickSplitAtInputIfNeeded_m: single);
  class procedure CreateNewLayerDuringErosionBySplittingThickest(soilProxy: GsModel);
  class function AutoIrrigationIsNeeded(waterStressFactorForAutoIrr: single; waterStressFactor: single; numLayers:
    integer; var layers: LayerStructureArray; swmWPInSoilProfileDepth_mm: single; fcmWPInSoilProfileDepth_mm: single):
    boolean;
  { no code for this function
  class procedure ChangePHFromFertilizerAdded(cumNFertAutoApplied_kgPha: single; cumNFixation_kgPha: single; numLayers:
    integer; var layers: LayerStructureArray; maxTillageDepth_m: single); }
  { plant }
  class function LeafAreaIndex(var plantParams: PlantParamsStructure; var development: DevelopmentStructure;
    var dailyWeather: DailyWeatherStructure; var constraints: ConstraintsStructure;
    minDayLengthForWinterGrowth_hr: single; ignoreLeafSenescence: boolean): single;
  class procedure WinterFrostAndShortDaylengthDamage(var dailyWeather: DailyWeatherStructure; 
    var development: DevelopmentStructure; var params: PlantParamsStructure; var nutrients: NutrientsStructure;
    var biomass: BiomassStructure; var constraints: ConstraintsStructure; noTempDamage: boolean; noSunDamage: boolean;
    isDayLengthIsAboveThresholdForGrowth: boolean; minDayLengthForWinterGrowth_hr: single);
  class function SoilStrengthRootGrowthConstraint(layer: integer; var layers: LayerStructureArray; var
    rootGrowthRestrictionByRockCoeffs: SCurveStructure; rootGrowthRestrictionInSandySoilParam: single): single;
  class function PlantWaterDemand_mm(var water: PlantWaterStructure; var constraints: ConstraintsStructure;
    numLayers: integer; var layers: LayerStructureArray;
    lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single; soilProfileDepth_m: single; rootDepth_m: single): single;
   class function NDemandForOptimalGrowth_kgPha(var plantParams: PlantParamsStructure; heatUnitIndex: single;
    totalPlantBiomass_tPha: single; var nutrients: NutrientsStructure; potentialIncreaseInBiomass_tPha: single): single;
  class function PDemandForOptimalGrowth_kgPha(var plantParams: PlantParamsStructure; heatUnitIndex: single;
    totalPlantBiomass_tPha: single; var nutrients: NutrientsStructure; potentialIncreaseInBiomass_tPha: single): single;
  class function NSupplyForThisPlant(var layers: LayerStructureArray; numLayersWithRootsInThem: integer;
      var nutrients: NutrientsStructure; var water: PlantWaterStructure): single;
  class function PSupplyForThisPlant(var layers: LayerStructureArray; var pUptakeCoeffs: SCurveStructure; pDemand_kgPha:
    single; var nutrients: NutrientsStructure; var biomass: BiomassStructure):
    single;
  class function NFixation(numLayers: integer; var layers: LayerStructureArray; rootDepth_m: single; heatUnitIndex: single;
    top30cmFractionFieldCapacity_frn: single; nDemand_kgPha: single; nFixationParam_frn: single): single;
  class function NOrPStressFactor(totalUptake_kgPha: single; optimalConc_kgPkg: single; totalPlantBiomassInclRoot_tPha:
    single; uptakeOverDemand_frn: single; var plantNAndPStressCoeffs: SCurveStructure): single;
  class procedure IncreaseBiomassWithStress(var biomass: BiomassStructure; var constraints: ConstraintsStructure;
    dayLengthIsAboveThresholdForGrowth: boolean);
  class procedure DecreaseBiomassFromDryness(var biomass: BiomassStructure; var plantParams: PlantParamsStructure; var
    constraints: ConstraintsStructure; var development: DevelopmentStructure; var nutrients: NutrientsStructure);
  class procedure DecreaseBiomassFromFallingLeaves(month: integer; var biomass: BiomassStructure; var plantParams:
    PlantParamsStructure; var layers: LayerStructureArray; var nutrients: NutrientsStructure);
  end;

implementation

uses
  SysUtils, ueq, ueqh, ueg, ueutils, ueplant, uesoil, uunits;

class function EP.LeafAreaIndex(var plantParams: PlantParamsStructure; var development: DevelopmentStructure;
    var dailyWeather: DailyWeatherStructure; var constraints: ConstraintsStructure;
    minDayLengthForWinterGrowth_hr: single; ignoreLeafSenescence: boolean): single;
  var
    newLeafAreaIndex: single;
  begin
  try
  newLeafAreaIndex := 0.0;
  { heat unit factor (s curve factor) }
  development.heatUnitFactorForLAIAndHeight := EQ.HeatUnitFactorForLAIAndHeight(development.heatUnitIndex,
    plantParams.heatUnitFactorParamsForLAI);
  { LAI before leaf decline }
  newLeafAreaIndex := EQ.LeafAreaIndexBeforeLeafDecline(development, plantParams.maxLeafAreaIndex, constraints);
  if (newLeafAreaIndex < 0.05) then 
    begin
    newLeafAreaIndex := 0.05;
    end
  else 
    begin
    { unresolved: unclear }
    { for a plant planted in the spring, the first test will be true in the fall, }
    { as will the second. }
    { for a plant planted in the fall, the first test will be true in the spring; }
    { the second will only be true if the day length is at least one hour above the minimum. }
    { in the epic code it says that this value (minDayLength_hr + 1.0) is the dormancy }
    { period for fall-planted crops. The only guess I have is that they are giving fall-planted }
    { plants a boost by increasing their LAI during the dormant period if they are over }
    { their potential heat units already. }
    { change from EPIC - replacing the hard-coded minDayLength + 1.0 with our plant param of hours over
      the min day length for dormancy }
    if (not ignoreLeafSenescence)
      and (development.heatUnitIndex > plantParams.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn)
      and (dailyWeather.dayLength_hr > minDayLengthForWinterGrowth_hr) then
      begin
      newLeafAreaIndex := EQ.LeafAreaIndexDuringLeafDecline(development, plantParams, constraints);
      end
    else 
      begin
      development.leafAreaIndexAtStartOfDecline := newLeafAreaIndex;
      end;
    end;
  result := newLeafAreaIndex;
  except on e: Exception do result := errorMessage('Exception in EP.LeafAreaIndex: ' + e.message); end;
  end;

class procedure EP.WinterFrostAndShortDaylengthDamage(var dailyWeather: DailyWeatherStructure; 
    var development: DevelopmentStructure; var params: PlantParamsStructure; var nutrients: NutrientsStructure;
    var biomass: BiomassStructure; var constraints: ConstraintsStructure; noTempDamage: boolean; noSunDamage: boolean;
    isDayLengthIsAboveThresholdForGrowth: boolean; minDayLengthForWinterGrowth_hr: single);
  var
    notEnoughSun, tooCold, reduceBiomass: boolean;
    nConcInPlant_kgPt, pConcInPlant_kgPt: single;
  begin
  try
  notEnoughSun := false;
  tooCold := false;
  reduceBiomass := true;
  nConcInPlant_kgPt := 0.0;
  pConcInPlant_kgPt := 0.0;
  { cold and low day length can reduce biomass independently }
  if (dailyWeather.minTempForDay_degC > -1.0) then  { warm }
    begin
    if isDayLengthIsAboveThresholdForGrowth then
      reduceBiomass := false { warm and enough sun }
    else
      notEnoughSun := true; { warm and not enough sun }
    end
  else { cold }
    begin
    if isDayLengthIsAboveThresholdForGrowth then
      tooCold := true  { cold but enough sun }
    else
      begin
      notEnoughSun := true; { cold and not enough sun }
      tooCold := true;
      end;
    end;
  { deal with ignoring temperature or sun }
  if noTempDamage then tooCold := false;
  if noSunDamage then notEnoughSun := false;
  if noTempDamage and noSunDamage then reduceBiomass := false;
  { if no damage, exit }
  if not reduceBiomass then exit;
  { if day length is low, calculate factor do to that }
  if notEnoughSun then
    constraints.winterBiomassDayLengthReductionFactor :=
      EQ.WinterBiomassDayLengthReductionFactor(dailyWeather.dayLength_hr, minDayLengthForWinterGrowth_hr)
  else
    constraints.winterBiomassDayLengthReductionFactor := 0.0;
  { if cold, calculate factor due to that }
  if tooCold then
    constraints.winterBiomassFrostReductionFactor :=
      EQ.WinterBiomassFrostReductionFactor(dailyWeather.minTempForDay_degC, params.frostReductionFactorParams)
  else
     constraints.winterBiomassFrostReductionFactor := 0.0;
  { combine factors (higher factor is worse) }
  constraints.winterBiomassReductionFactor := max(0.0, min(1.0, max(constraints.winterBiomassFrostReductionFactor,
    constraints.winterBiomassDayLengthReductionFactor)));
  if reduceBiomass and (biomass.standingLive_tPha >= 0.0001) then    { change from EPIC: constant was 0.01 }
    begin
    { calc amount of biomass lost }
    biomass.reductionFromFrostAndDaylength_tPha := EQ.WinterReductionInBiomass(biomass.totalLive_tPha,
      development.heatUnitIndex, constraints.winterBiomassReductionFactor);
    { don't let more than half of standing live biomass die at any time }
    if (biomass.reductionFromFrostAndDaylength_tPha > 0.5 * biomass.totalShootWeight_tPha) then
      biomass.reductionFromFrostAndDaylength_tPha := 0.5 * biomass.totalShootWeight_tPha;
    { subtract lost biomass from live and add to dead }
    subtractQuantity(biomass.totalShootWeight_tPha, biomass.reductionFromFrostAndDaylength_tPha);
    GS.RecalculatePlantBiomassRelationships(biomass);
    addQuantity(biomass.standingDead_tPha, biomass.reductionFromFrostAndDaylength_tPha);
    { adjust n and p in live and dead for change }
    nConcInPlant_kgPt := safedivExcept(nutrients.nInLiveBiomass_kgPha, biomass.totalLive_tPha, 0.0);
    pConcInPlant_kgPt := safedivExcept(nutrients.pInLiveBiomass_kgPha, biomass.totalLive_tPha, 0.0);
    subtractQuantity(nutrients.nInLiveBiomass_kgPha, biomass.reductionFromFrostAndDaylength_tPha * nConcInPlant_kgPt);
    subtractQuantity(nutrients.pInLiveBiomass_kgPha, biomass.reductionFromFrostAndDaylength_tPha * pConcInPlant_kgPt);
    addQuantity(nutrients.nInStandingDead_kgPha, biomass.reductionFromFrostAndDaylength_tPha * nConcInPlant_kgPt);
    addQuantity(nutrients.pInStandingDead_kgPha, biomass.reductionFromFrostAndDaylength_tPha * pConcInPlant_kgPt);
    { also reduce LAI by biomass reduction factor }
    { note that limit at half of standing live doesn't affect this }
    development.leafAreaIndex := development.leafAreaIndex * ((1.0 - constraints.winterBiomassReductionFactor));
    end;
  except on e: Exception do errorMessage('Exception in EP.WinterFrostAndShortDaylengthDamage: ' + e.message); end;
  end;

class function EP.SoilStrengthRootGrowthConstraint(layer: integer; var layers: LayerStructureArray; var
  rootGrowthRestrictionByRockCoeffs: SCurveStructure; rootGrowthRestrictionInSandySoilParam: single): single;
  var
    soilTextureParams: arrayTwo;
    bulkDensityAdjForWaterContent_tPm3: single;
    { calculate bulk densities for boundary conditions of low and high stress }
    bulkDensityForLowStress_tPm3: single;
    bulkDensityForHighStress_tPm3: single;
  begin
  try
  bulkDensityAdjForWaterContent_tPm3 := 0.0;
  bulkDensityForLowStress_tPm3 := EQ.BulkDensityForLowStress_tPm3(layers[layer].sandContent_pct,
    rootGrowthRestrictionInSandySoilParam);
  bulkDensityForHighStress_tPm3 := EQ.BulkDensityForHighStress_tPm3(layers[layer].sandContent_pct,
    rootGrowthRestrictionInSandySoilParam);
  { from these, calculate soil texture parameters }
  soilTextureParams[1] := EQ.SoilTextureParam2(bulkDensityForLowStress_tPm3, bulkDensityForHighStress_tPm3);
  soilTextureParams[0] := EQ.SoilTextureParam1(bulkDensityForLowStress_tPm3, soilTextureParams[1]);
  { calculate water-adjusted bulk density for layer }
  bulkDensityAdjForWaterContent_tPm3 := EQ.BulkDensityAdjForWaterContentForLayer_tPm3(layer, layers);
  { return soil strength factor }
  result := EQ.SoilStrengthFactor_frn(bulkDensityAdjForWaterContent_tPm3, soilTextureParams,
    layers[layer].rockContent_pct, rootGrowthRestrictionByRockCoeffs);
  except on e: Exception do result := errorMessage('Exception in EP.SoilStrengthRootGrowthConstraint: ' + e.message); end;
  end;

class function EP.PlantWaterDemand_mm(var water: PlantWaterStructure; var constraints: ConstraintsStructure;
    numLayers: integer; var layers: LayerStructureArray;
    lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single; soilProfileDepth_m: single; rootDepth_m: single): single;
  var
    layer: integer;
    maxOfDepthThisLayerOrRootDepth_m, plantWaterDeficitCompensFactor_frn, waterDemandTerm_mm,
      waterDemandTermLastLayer_mm, unconstrainedWaterDemand_mm, totalWaterDemand_mm, lowerWaterLimit_mm,
      waterDemandConstraint: single;
  begin
  try
  maxOfDepthThisLayerOrRootDepth_m := 0.0;
  plantWaterDeficitCompensFactor_frn := 0.0;
  waterDemandTerm_mm := 0.0;
  waterDemandTermLastLayer_mm := 0.0;
  unconstrainedWaterDemand_mm := 0.0;
  totalWaterDemand_mm := 0.0;
  lowerWaterLimit_mm := 0.0;
  waterDemandConstraint := 0.0;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do water.waterDemandByLayer_mm[layer] := 0.0;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do
    begin
    if (rootDepth_m > layers[layer].depth_m) then
      maxOfDepthThisLayerOrRootDepth_m := layers[layer].depth_m
    else
      maxOfDepthThisLayerOrRootDepth_m := rootDepth_m;
    { calculate unconstrained water use }
    plantWaterDeficitCompensFactor_frn := EQ.PlantWaterDeficitCompensFactorForLayer_frn(layer, constraints);
    waterDemandTerm_mm := EQ.WaterDemandTermForLayer_mm(water.potPlantEvap_mm, soilProfileDepth_m,
      maxOfDepthThisLayerOrRootDepth_m, rootDepth_m);
    unconstrainedWaterDemand_mm := EQ.UnconstrainedPlantWaterDemandForLayer_mm(waterDemandTerm_mm,
      plantWaterDeficitCompensFactor_frn, totalWaterDemand_mm, waterDemandTermLastLayer_mm);
    { calculate constraints on water use }
    { can leave this here, in water demand, because these constraints operate on limiting water demand if
        soil water content is less than 25% of the possible soil water. This limitation will not hurt the
        allocation of water to plants after this function since this has more to do with the ability of plants
        to take up water (can't get at less available water) and less to do with the actual amount of total water
        in the patch. }
    lowerWaterLimit_mm := layers[layer].wiltingPoint_mm;
    if (layers[layer].depth_m <= 0.5) then lowerWaterLimit_mm := lowerLimitWaterContentInTopP5MAsFractOfWP_frn *
      layers[layer].wiltingPoint_mm;
    waterDemandConstraint := EQ.WaterDemandConstraintForLayer_mm(layer, layers, lowerWaterLimit_mm);
    { calculate final water use }
    water.waterDemandByLayer_mm[layer] := EQ.PlantWaterDemandForLayer_mm(layer, layers, unconstrainedWaterDemand_mm,
      waterDemandConstraint, lowerWaterLimit_mm, constraints.rootGrowthConstraintByLayer_frn[layer]);
    addQuantity(totalWaterDemand_mm, water.waterDemandByLayer_mm[layer]);
    waterDemandTermLastLayer_mm := waterDemandTerm_mm;
    if maxOfDepthThisLayerOrRootDepth_m >= rootDepth_m then break;
    end;
  result := totalWaterDemand_mm;
  except on e: Exception do result := errorMessage('Exception in EP.PlantWaterDemand_mm: ' + e.message); end;
  end;

class function EP.NDemandForOptimalGrowth_kgPha(var plantParams: PlantParamsStructure; heatUnitIndex: single;
    totalPlantBiomass_tPha: single; var nutrients: NutrientsStructure; potentialIncreaseInBiomass_tPha: single): single;
  begin
  try
  nutrients.optimalNConc_kgPkg := EQ.PlantoptimalNConc_kgPkg(plantParams, heatUnitIndex);
  result := EQ.PlantNitrogenDemandForOptimalGrowth_kgPha(nutrients.optimalNConc_kgPkg, totalPlantBiomass_tPha,
    nutrients.nInLiveBiomass_kgPha, plantParams.nFractionAtMaturity_frn, potentialIncreaseInBiomass_tPha);
  except on e: Exception do result := errorMessage('Exception in EP.NDemandForOptimalGrowth_kgPha: ' + e.message); end;
  end;

class function EP.PDemandForOptimalGrowth_kgPha(var plantParams: PlantParamsStructure; heatUnitIndex: single;
    totalPlantBiomass_tPha: single; var nutrients: NutrientsStructure; potentialIncreaseInBiomass_tPha: single): single;
  begin
  try
  nutrients.optimalPConc_kgPkg := EQ.PlantoptimalPConc_kgPkg(plantParams, heatUnitIndex);
  result := EQ.PlantPhosphorusDemandForOptimalGrowth_kgPha(nutrients.optimalPConc_kgPkg, totalPlantBiomass_tPha,
    nutrients.pInLiveBiomass_kgPha, potentialIncreaseInBiomass_tPha);
  except on e: Exception do result := errorMessage('Exception in EP.PDemandForOptimalGrowth_kgPha: ' + e.message); end;
  end;

class function EP.NSupplyForThisPlant(var layers: LayerStructureArray; numLayersWithRootsInThem: integer;
    var nutrients: NutrientsStructure; var water: PlantWaterStructure): single;
  var
    layer: integer;
    nSupply_kgPha: single;
  begin
  try
  nSupply_kgPha := 0.0;
  if numLayersWithRootsInThem > 0 then for layer := 0 to numLayersWithRootsInThem - 1 do
    begin
    nutrients.nSupplyByLayer_kgPha[layer] := EQ.NSupplyForLayer_kgPha(water.waterUseByLayer_mm[layer],
      layers[layer].nitrate_kgPha, layers[layer].waterContent_mm);
    addQuantity(nSupply_kgPha, nutrients.nSupplyByLayer_kgPha[layer]);
    end;
  result := nSupply_kgPha;
  except on e: Exception do result := errorMessage('Exception in EP.NSupplyForThisPlant: ' + e.message); end;
  end;

class function EP.PSupplyForThisPlant(var layers: LayerStructureArray; var pUptakeCoeffs: SCurveStructure; pDemand_kgPha:
    single; var nutrients: NutrientsStructure; var biomass: BiomassStructure): single;
  var
    layer: integer;
    pSupply_kgPha: single;
    labilePConc_gPt: single;
    labilePUptakeFactor_frn: single;
  begin
  try
  pSupply_kgPha := 0.0;
  labilePConc_gPt := 0.0;
  labilePUptakeFactor_frn := 0.0;
  if biomass.numLayersWithRootsInThem > 0 then for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
    begin
    { kg/ha / t/ha = kg/t * kg_to_g = g/t }
    labilePConc_gPt := kg_to_g * safediv(layers[layer].labileP_kgPha, layers[layer].weight_tPha);
    labilePUptakeFactor_frn := EQ.LabilePUptakeFactorForLayer_frn(labilePConc_gPt, pUptakeCoeffs);
    nutrients.pSupplyByLayer_kgPha[layer] := EQ.PSupplyForLayer_kgPha(layer, nutrients, biomass, layers,
      labilePUptakeFactor_frn);
    addQuantity(pSupply_kgPha, nutrients.pSupplyByLayer_kgPha[layer]);
    end;
  result := pSupply_kgPha;
  except on e: Exception do result := errorMessage('Exception in EP.PSupplyForThisPlant: ' + e.message); end;
  end;

class function EP.NFixation(numLayers: integer; var layers: LayerStructureArray; rootDepth_m: single; heatUnitIndex: single;
  top30cmFractionFieldCapacity_frn: single; nDemand_kgPha: single; nFixationParam_frn: single): single;
  var
    layer: integer;
    proportion, nitrateInRootDepth_kgPha,  depthLastLayer_m, nFixationFractionNitrateFactor,
      nFixationFractionWaterFactor, nFixationFractionGrowthStageFactor, nFixationFraction_frn,
      potentialNFixation_kgPha: single;
  begin
  try
  { no n fixation if:
      1) plant is young (nodules not formed yet)
      2) plant is old (senescence)
      3) soil is too dry (bacteria can't work) }
  if (heatUnitIndex < 0.15) or (heatUnitIndex > 0.75) or (top30cmFractionFieldCapacity_frn <= 0.25) then
    begin
    result := 0.0;
    exit;
    end;
  { total nitrate in root depth for this plant }
  nitrateInRootDepth_kgPha := 0.0;
  depthLastLayer_m := 0.0;              
  if numLayers > 0 then for layer := 0 to numLayers - 1 do 
    begin
    if (layers[layer].depth_m > rootDepth_m) then
      proportion := safediv(rootDepth_m - depthLastLayer_m, layers[layer].depth_m - depthLastLayer_m)
    else
      proportion := 1.0;
    nitrateInRootDepth_kgPha := nitrateInRootDepth_kgPha + layers[layer].nitrate_kgPha * proportion;
    if (layers[layer].depth_m > rootDepth_m) then break;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  { calculate constraints on fixation based on nitrate, water, and growth stage }
  nFixationFractionNitrateFactor := EQ.NFixationFractionNitrateFactor(nitrateInRootDepth_kgPha, rootDepth_m);
  nFixationFractionWaterFactor := EQ.NFixationFractionWaterFactor(top30cmFractionFieldCapacity_frn);
  nFixationFractionGrowthStageFactor := EQ.NFixationFractionGrowthStageFactor(heatUnitIndex);
  { combine constraints to one factor }
  nFixationFraction_frn := EQ.NFixationFraction_frn(nFixationFractionWaterFactor, nFixationFractionNitrateFactor,
    nFixationFractionGrowthStageFactor);
  { calculate fixation }
  potentialNFixation_kgPha := EQ.NFixation_kgPha(nFixationFraction_frn, nDemand_kgPha);
  { adjust with parameter that reduces importance of constraints }
  result := potentialNFixation_kgPha * nFixationParam_frn + nDemand_kgPha * (1.0 - nFixationParam_frn);
  result := min(20.0, result);
  except on e: Exception do result := errorMessage('Exception in EP.NFixation: ' + e.message); end;
  end;

class function EP.NOrPStressFactor(totalUptake_kgPha: single; optimalConc_kgPkg: single; totalPlantBiomassInclRoot_tPha:
  single; uptakeOverDemand_frn: single; var plantNAndPStressCoeffs: SCurveStructure): single;
  var
    scalingFactor: single;
  begin
  try
  scalingFactor := EQ.NOrPStressFactorScalingFactor(totalUptake_kgPha, optimalConc_kgPkg, totalPlantBiomassInclRoot_tPha);
  if scalingFactor >= 1.0 then
    result := 1.0
  else
    begin
    result := EQ.NOrPStressFactor_frn(scalingFactor, plantNAndPStressCoeffs);
    result := max(uptakeOverDemand_frn, result);
    end;
  except on e: Exception do result := errorMessage('Exception in EP.NOrPStressFactor: ' + e.message); end;
  end;

class procedure EP.IncreaseBiomassWithStress(var biomass: BiomassStructure; var constraints: ConstraintsStructure;
  dayLengthIsAboveThresholdForGrowth: boolean);
  var
    newTotalPlantBiomass_tPha: single;
    changeInBiomass_tPha: single;
  begin
  try
  newTotalPlantBiomass_tPha := 0.0;
  changeInBiomass_tPha := 0.0;
  { calculate new biomass }
  changeInBiomass_tPha := EQ.PlantIncreaseInBiomass_tPha(biomass.potentialIncrease_tPha,
    constraints.biomassGrowthConstraint_frn, constraints.biomassAdjustmentIfLAIDeclining,
    dayLengthIsAboveThresholdForGrowth);
  newTotalPlantBiomass_tPha := biomass.totalLive_tPha + changeInBiomass_tPha;
  { change from EPIC }
  { epic code had the statement here: if (newTotalPlantBiomass_tPha >= 0.01) go on }
  { but we are only using zero as we are dealing with smaller amounts of biomass }
  if (newTotalPlantBiomass_tPha <= 0.0) then exit;
  { set new biomass }
  biomass.totalLive_tPha := newTotalPlantBiomass_tPha;
  biomass.actualIncrease_tPha := changeInBiomass_tPha;
  except on e: Exception do errorMessage('Exception in EP.IncreaseBiomassWithStress: ' + e.message); end;
  end;

{ change from EPIC }
{ allocation to different root layers moved to GS::AllocateBiomassAmongComponents, after allocation  }
class procedure EP.DecreaseBiomassFromDryness(var biomass: BiomassStructure; var plantParams: PlantParamsStructure; var
    constraints: ConstraintsStructure; var development: DevelopmentStructure; var nutrients: NutrientsStructure);
  var
    nConcInPlant_kgPt, pConcInPlant_kgPt, nAmountLost_kgPha, pAmountLost_kgPha: single;
  begin
  try
  { calculate amount of biomass dying from dryness - consider only shoots }
  biomass.reductionFromDryness_tPha := EQ.BiomassLossIfOldAndDry(constraints.waterStressFactor_frn,
    development.heatUnitIndex, biomass.totalShootWeight_tPha);
  if (biomass.reductionFromDryness_tPha > 0.0) then
    begin
    { figure nutrient concentrations before changing total biomass }
    nConcInPlant_kgPt := safediv(nutrients.nInLiveBiomass_kgPha, biomass.totalLive_tPha);
    pConcInPlant_kgPt := safediv(nutrients.pInLiveBiomass_kgPha, biomass.totalLive_tPha);
    { this loss goes into standing dead, not into soil }
    { loss comes completely out of shoots, not root or storage organ or fruits }
    subtractQuantity(biomass.totalShootWeight_tPha, biomass.reductionFromDryness_tPha);
    GS.RecalculatePlantBiomassRelationships(biomass);
    addQuantity(biomass.standingDead_tPha, biomass.reductionFromDryness_tPha);
    { transfer n and p to standing dead also }
    nAmountLost_kgPha := biomass.reductionFromDryness_tPha * nConcInPlant_kgPt;
    subtractQuantity(nutrients.nInLiveBiomass_kgPha, nAmountLost_kgPha);
    addQuantity(nutrients.nInStandingDead_kgPha, nAmountLost_kgPha);
    pAmountLost_kgPha := biomass.reductionFromDryness_tPha * pConcInPlant_kgPt;
    subtractQuantity(nutrients.pInLiveBiomass_kgPha, pAmountLost_kgPha);
    addQuantity(nutrients.pInStandingDead_kgPha, pAmountLost_kgPha);
    end;
  except on e: Exception do errorMessage('Exception in EP.DecreaseBiomassFromDryness: ' + e.message); end;
  end;

class procedure EP.DecreaseBiomassFromFallingLeaves(month: integer; var biomass: BiomassStructure; var plantParams:
  PlantParamsStructure; var layers: LayerStructureArray; var nutrients: NutrientsStructure);
  var
    nConcInPlant_kgPt, pConcInPlant_kgPt, nAmountLost_kgPha, pAmountLost_kgPha: single;
  begin
  try
  { calculate amount of biomass lost from falling leaves }
  biomass.reductionFromFallingLeaves_tPha := EQ.LeavesFallingInFall(biomass.totalShootWeight_tPha,
    plantParams.yearsToMaturityIfTree, month);
  if (biomass.reductionFromFallingLeaves_tPha > 0.0) then
    begin
    { figure nutrient concentrations before changing total biomass }
    nConcInPlant_kgPt := safediv(nutrients.nInLiveBiomass_kgPha, biomass.totalLive_tPha);
    pConcInPlant_kgPt := safediv(nutrients.pInLiveBiomass_kgPha, biomass.totalLive_tPha);
    { reduce biomass and add stuff lost to flat crop residue in first layer }
    subtractQuantity(biomass.totalShootWeight_tPha, biomass.reductionFromFallingLeaves_tPha);
    GS.RecalculatePlantBiomassRelationships(biomass);
    addQuantity(layers[0].flatCropResidue_tPha, biomass.reductionFromFallingLeaves_tPha);
    { reduce n and p in plant and add to organic n and p fresh in soil surface layer }
    nAmountLost_kgPha := nConcInPlant_kgPt * biomass.reductionFromFallingLeaves_tPha;
    subtractQuantity(nutrients.nInLiveBiomass_kgPha, nAmountLost_kgPha);
    addQuantity(layers[0].organicNFresh_kgPha, nAmountLost_kgPha);
    pAmountLost_kgPha := nConcInPlant_kgPt * biomass.reductionFromFallingLeaves_tPha;
    subtractQuantity(nutrients.pInLiveBiomass_kgPha, pAmountLost_kgPha);
    addQuantity(layers[0].organicPFresh_kgPha, pAmountLost_kgPha);
    end;
  except on e: Exception do errorMessage('Exception in EP.DecreaseBiomassFromFallingLeaves: ' + e.message); end;
  end;

class procedure EP.SoilTemperatureByLayer(numLayers: integer; var layers: LayerStructureArray; var surface:
  SurfaceStructure; var totals: TotalsStructure; probWetDayForMonth_frn: single; var dailyWeather: DailyWeatherStructure;
  snowWaterContent_mm: single; meanMonthlyMeanMeanTempForYear_degC: single; var soilInsulationFromAirTempCoeffs:
  SCurveStructure; var movement: MovementStructure);
  var
    layer: integer;
    maxDampingDepth_m: single;
    scalingParam: single;
    dampingDepth_m: single;
    bareSoilSurfaceTempRadiationFactor: single;
    depthLastLayer_m: single;
    soilTempDepthWtFactor: single;
  begin
  try
  depthLastLayer_m := 0.0;
  { damping depth }
  maxDampingDepth_m := EQ.MaxDampingDepthForSoilTemp_m(totals.patchMeanBulkDensity_tPm3);
  scalingParam := EQ.ScalingParamForDampingDepthForSoilTemp(totals.patchTotalSoilWaterContent_mm,
    totals.patchMeanBulkDensity_tPm3, layers[numLayers - 1].depth_m);
  dampingDepth_m := EQ.DampingDepthForSoilTemp_m(maxDampingDepth_m, scalingParam);
  { bare soil radiation factor }
  bareSoilSurfaceTempRadiationFactor := EQ.BareSoilSurfaceTempRadiationFactor(dailyWeather.radiationForDay_MJPm2,
    surface.albedo_frn, surface.soilCoverIndex_frn);
  { bare soil surface temp }
  if (dailyWeather.rainfallForDay_mm > 0.0) then 
    begin
    movement.soilSurfaceTempBare_degC := EQ.BareSoilSurfaceTempWetDay_degC(dailyWeather.meanTempForDay_degC,
      dailyWeather.minTempForDay_degC, probWetDayForMonth_frn);
    end
  else 
    begin
    movement.soilSurfaceTempBare_degC := EQ.BareSoilSurfaceTempDryDay_degC(dailyWeather.maxTempForDay_degC,
      dailyWeather.meanTempForDay_degC, bareSoilSurfaceTempRadiationFactor, probWetDayForMonth_frn);
    end;
  { soil cover lag factor }
  movement.soilSurfaceCoverLagFactor_frn :=
    EQ.SoilSurfaceTempCoverLagFactor_frn(totals.patchTotalAboveGroundBiomassAndResidue_tPha, snowWaterContent_mm,
    soilInsulationFromAirTempCoeffs);
  { soil surface temp adjusted for cover }
  movement.soilSurfaceTempWithCover_degC := EQ.SurfaceLayerSoilTemp_degC(layers[1].temperature_degC,
    movement.soilSurfaceCoverLagFactor_frn, movement.soilSurfaceTempBare_degC);
  { final soil surface temp is min of that for bare soil and cover }
  surface.temp_degC := min(movement.soilSurfaceTempBare_degC, movement.soilSurfaceTempWithCover_degC);
  { calculate temperatures for each layer based on above temperatures }
  if numLayers > 0 then for layer := 0 to numLayers - 1 do 
    begin
    soilTempDepthWtFactor := EQ.SoilTempDepthWtFactorForLayer(layers[layer].depth_m, depthLastLayer_m, dampingDepth_m);
    layers[layer].temperature_degC := EQ.SoilTempAtCenterForLayer_degC(layers[layer].temperature_degC,
      movement.soilSurfaceCoverLagFactor_frn, soilTempDepthWtFactor, meanMonthlyMeanMeanTempForYear_degC,
      movement.soilSurfaceTempWithCover_degC);
    depthLastLayer_m := layers[layer].depth_m;
    end;
  except on e: Exception do errorMessage('Exception in EP.SoilTemperatureByLayer: ' + e.message); end;
  end;

const kMinPropRainInFirstHalfHour_frn: single = 0.02083;

class procedure EP.RunoffAndErosionParams(var water: WaterStructure; var erosion: ErosionStructure;
    meanPropRainInFirstHalfHourForMonth_frn: single);
  var
    maxPropRainInFirstHalfHour_frn, propRainInFirstHalfHour_frn, propRainInFirstHalfHourModBySnow,
      patchRainfallWithoutSnowmeltForDay_mm: single;
  begin
  try
  maxPropRainInFirstHalfHour_frn := 0.0;
  propRainInFirstHalfHour_frn := 0.0;
  propRainInFirstHalfHourModBySnow := 0.0;
  { subtract snowmelt to get just rain }
  patchRainfallWithoutSnowmeltForDay_mm := water.patchRainfallForDay_mm - water.snowmeltForDay_mm;
  { calculate peak rainfall rate and rainfall energy factor for USLE water erosion }
  { if more rain than snowmelt, use one method. if more snowmelt than rain, use another }
  { first calculate alpha(0.5), the proportion of rainfall that fell in the first }
  { half hour and modify it if there was snow present }
  if (patchRainfallWithoutSnowmeltForDay_mm > 0.0) then
    begin
    { more rain than snowmelt }
    maxPropRainInFirstHalfHour_frn :=
      EQ.MaxPropTotalRainFallsInFirstHalfHourForMonth_frn(patchRainfallWithoutSnowmeltForDay_mm);
    { alpha(0.5) is calculated by a triangular distribution with the bottom at 0.02093 always, }
    { the mean at an input value, and the max at the above calculated value }
    propRainInFirstHalfHour_frn := Utils_TriangularDistribution(kMinPropRainInFirstHalfHour_frn,
      meanPropRainInFirstHalfHourForMonth_frn, maxPropRainInFirstHalfHour_frn);
    { modification if snow was present is }
    { (rain * alpha(0.5) + snowmelt * alpha(0.5min)) / rain }
    { EPIC says: "Melted snow is treated the same as rainfall for estimating runoff and percolation,
      but rainfall energy is set to 0.0 and peak runoff rate is estimated assuming uniformly
      distributed rainfall for a 24 h duration. }
    propRainInFirstHalfHourModBySnow := safediv(patchRainfallWithoutSnowmeltForDay_mm * propRainInFirstHalfHour_frn +
      water.snowmeltForDay_mm * kMinPropRainInFirstHalfHour_frn, water.patchRainfallForDay_mm);
    { calculate peak rainfall rate and rainfall energy factor for USLE }
    water.peakRainfallRate_mmPhr := EQ.PeakRainfallRate_mmPhr(patchRainfallWithoutSnowmeltForDay_mm,
      propRainInFirstHalfHourModBySnow);
    erosion.rainfallEnergyFactorForUSLE := EQ.RainfallEnergyFactorForUSLE(patchRainfallWithoutSnowmeltForDay_mm,
      water.peakRainfallRate_mmPhr, propRainInFirstHalfHourModBySnow);
    end
  else
    begin
    { more snowmelt than rain }
    { EPIC says: "Melted snow is treated the same as rainfall for estimating runoff and percolation,
      but rainfall energy is set to 0.0 and peak runoff rate is estimated assuming uniformly
      distributed rainfall for a 24 h duration. }
    if water.timeOfConc_hr = 0.0 then
      water.peakRainfallRate_mmPhr := errorMessage('Problem: time of concentration is zero')
    else
      water.peakRainfallRate_mmPhr := kMinPropRainInFirstHalfHour_frn * safediv(water.snowmeltForDay_mm,
        water.timeOfConc_hr);
    erosion.rainfallEnergyFactorForUSLE := 0.0;
    end;
  { calculate duration of rainfall }
  water.rainfallDuration_hr := EQH.RainfallDuration_hr(patchRainfallWithoutSnowmeltForDay_mm,
    propRainInFirstHalfHourModBySnow);
  except on e: Exception do errorMessage('Exception in EP.RunoffAndErosionParams: ' + e.message); end;
  end;

class function EP.RunoffVolume_mm(numLayers: integer; var layers: LayerStructureArray;
    var params: SoilParamsStructure; var water: WaterStructure; var surface: SurfaceStructure;
    patchMeanCropHeight_m: single): single;
  var
    upperLimit, lowerLimit, depthWeightedFractionOfFieldCapacity_frn: single;
    retentionParamAdjusted_mm: single;
  begin
  try
  { used to do this only at input, but now recalculating daily to deal with possible changes }
  EP.AdjustCurveNumberForSlope(water, params, numLayers, layers);
  depthWeightedFractionOfFieldCapacity_frn := EQH.DepthWeightedFractionOfFieldCapacity(numLayers, layers);
  { adjust retention parameter for current soil water conditions }
  retentionParamAdjusted_mm := EQH.SoilWaterAdjRetentionParameter_mm(water.retentionParam_mm,
    depthWeightedFractionOfFieldCapacity_frn, params.curveNumberCoeffs.c1, params.curveNumberCoeffs.c2);
  { if ground frozen, reduce retention parameter }
  if (layers[1].temperature_degC <  - 1.0) then
    retentionParamAdjusted_mm := EQH.ModifiedRetentionParamForFrozenGround(retentionParamAdjusted_mm);
  { calculate average curve number for day based on water-adjusted retention param }
  water.avgCurveNumber := safediv(25400.0, retentionParamAdjusted_mm + 254.0);
  { generate curve number for day from triangular dist if using stochastic curve numbers }
  if (params.useStochasticCurveNumberEstimation) then 
    begin
    upperLimit := min(99.5, water.avgCurveNumber + 5.0);
    lowerLimit := max(1.0, water.avgCurveNumber - 5.0);
    water.curveNumber := Utils_TriangularDistribution(lowerLimit, water.avgCurveNumber, upperLimit);
    end
  else 
    water.curveNumber := water.avgCurveNumber;
  { calculate final retention param based on final curve number for day }
  water.retentionParam_mm := safediv(25400.0, water.curveNumber) - 254.0;
  { final equation for runoff }
  result := EQH.RunoffVolume_mm(water, surface, layers[0], params, patchMeanCropHeight_m);
  except on e: Exception do result := errorMessage('Exception in EP.RunoffVolume_mm: ' + e.message); end;
  end;

class procedure EP.AdjustCurveNumberForSlope(var water: WaterStructure; var params: SoilParamsStructure;
    numLayers: integer; var layers: LayerStructureArray);
  var
     curveNumberWet, curveNumberDry, retentionParamWet_mm, retentionParamAvg_mm: single;
    depthWeightedFractionOfFieldCapacity_frn, porosityToFieldCapacityRatio: single;
  begin
  try
  depthWeightedFractionOfFieldCapacity_frn := EQH.DepthWeightedFractionOfFieldCapacity(numLayers, layers);
  porosityToFieldCapacityRatio := EQH.PorosityToFieldCapacityRatio(numLayers, layers);
  water.avgCurveNumber := params.avgCurveNumberUnadjustedForSlope;
  curveNumberWet := EQH.CurveNumberWet(water.avgCurveNumber);
  water.avgCurveNumber := EQH.CurveNumberAvgForSlope(params.watershedSlopeSteepness_mPm, water.avgCurveNumber,
    curveNumberWet);
  curveNumberDry := EQH.CurveNumberDry(water.avgCurveNumber);
  curveNumberWet := EQH.CurveNumberWet(water.avgCurveNumber);
  { retention parameters }
  water.retentionParam_mm := EQH.RetentionParameter_mm(curveNumberDry);
  retentionParamWet_mm := EQH.RetentionParameter_mm(curveNumberWet);
  retentionParamAvg_mm := EQH.RetentionParameter_mm(water.avgCurveNumber);
  { shape parameters for retention parameter under current conditions }
  EQH.ShapeParamsForRetentionParam(retentionParamAvg_mm, water.retentionParam_mm, retentionParamWet_mm,
    porosityToFieldCapacityRatio, params.curveNumberCoeffs);
  { adjust retention param if soil profile is shallow }
  if (layers[numLayers-1].depth_m < 1.0) then
    water.retentionParam_mm := water.retentionParam_mm * (sqrt(layers[numLayers-1].depth_m));
  except on e: Exception do errorMessage('Exception in EP.AdjustCurveNumberForSlope: ' + e.message); end;
  end;

class function EP.PeakRunoffRate_m3Psec(var params: SoilParamsStructure; var water: WaterStructure): single;
  var
    timeOfConcForChannelFlow_hr, timeOfConcForSurfaceFlow_hr, timeOfConcForShallowChannelFlow_hr: single;
    avgFlowRateForOneHa_mmPhr, propRainInTimeOfConc_frn: single;
    avgShallowFlowVelocity_mPhr, shallowFlowLength_m: single;
  begin
  try
  if (params.peakRateEstimationMethod = kModifiedRationalEquationMethod) then
    begin
    { calculate time of concentration for rainfall }
    avgFlowRateForOneHa_mmPhr := EQH.AvgFlowRateForOneHectare_mmPhr(water.runoffVolume_mm, water.rainfallDuration_hr);
    timeOfConcForChannelFlow_hr := EQH.TimeOfConcForChannelFlow_hr(params.watershedChannelLength_km,
        params.manningsChannelRoughnessCoeff, params.watershedArea_ha, params.watershedChannelSlope_mPm,
        avgFlowRateForOneHa_mmPhr);
    timeOfConcForSurfaceFlow_hr := EQH.TimeOfConcForSurfaceFlow_hr(params.watershedSlopeLength_m,
        params.manningsSurfaceRoughnessCoeff, params.watershedSlopeSteepness_mPm, avgFlowRateForOneHa_mmPhr);
    water.timeOfConc_hr := EQH.TimeOfConc_hr(timeOfConcForChannelFlow_hr, timeOfConcForSurfaceFlow_hr);
    { calculate proportion of rain that was in the time of concentration }
    propRainInTimeOfConc_frn := EQH.PropRainInTimeOfConc_frn(water.timeOfConc_hr, water.peakRainfallRate_mmPhr,
        water.patchRainfallForDay_mm, params.maxFractionOfRainfallInTimeOfConcParam);
    { return peak runoff rate }
    result := EQH.PeakRunoffRateByRationalEquation_m3Psec(propRainInTimeOfConc_frn, water.runoffVolume_mm,
        water.timeOfConc_hr);
    end
  else 
    begin
    { scs tr55 method }
    { calculate time of concentration for rainfall }
    avgShallowFlowVelocity_mPhr := EQH.AvgShallowFlowVelocity_mPhr(params.watershedSlopeSteepness_mPm);
    shallowFlowLength_m := EQH.ShallowFlowLength_m(params.watershedChannelLength_km);
    timeOfConcForChannelFlow_hr := EQH.TimeOfConcForChannelFlowTR55_hr(params.watershedChannelLength_km,
      params.watershedSlopeLength_m, shallowFlowLength_m, params.manningsChannelRoughnessCoeff,
      params.watershedChannelDepth_m, params.watershedChannelSlope_mPm);
    timeOfConcForSurfaceFlow_hr := EQH.TimeOfConcForSurfaceFlowTR55_hr(params.watershedSlopeLength_m,
        params.manningsSurfaceRoughnessCoeff, params.watershedSlopeSteepness_mPm,
        water.patchRainfallForDay_mm);
    timeOfConcForShallowChannelFlow_hr := EQH.TimeOfConcForShallowChannelFlow_hr(shallowFlowLength_m,
        avgShallowFlowVelocity_mPhr);
    water.timeOfConc_hr := EQH.TimeOfConcTR55_hr(timeOfConcForChannelFlow_hr, timeOfConcForSurfaceFlow_hr,
        timeOfConcForShallowChannelFlow_hr);
    { return peak runoff rate }
    result := EQH.PeakRunoffRateBySCSTR55_m3Psec(water, params.peakRateEstimationMethod);
    end;
  except on e: Exception do result := errorMessage('Exception in EP.PeakRunoffRate_m3Psec: ' + e.message); end;
  end;

class procedure EP.PercAndLatFlowByLayer_mm(numLayers: integer; layerWithDrainageSystem: integer;
    var params: SoilParamsStructure; var water: WaterStructure; var layers: LayerStructureArray;
    inflowingWater_mm: single; totalRunoff_mm: single; allowPercolation, allowLateralFlow: boolean);
  var
    layer: integer;
    percolationLastLayer_mm, lateralFlowThisLayer_mm, percolationThisLayer_mm,
      swOverFCUpperLayer, swOverFCThisLayer, movement_mm, proportion: single;
  begin
  try
  { assume this function is not called if both percolation and lateral flow are not allowed }
  percolationLastLayer_mm := inflowingWater_mm - totalRunoff_mm;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do
    begin
    { add water percolated from last layer (if first layer, add inflowing water - runoff) }
    addQuantity(layers[layer].waterContent_mm, percolationLastLayer_mm);
    { if water table is above the bottom of this soil layer, water does not move. }
    if (water.waterTableDepth_m > layers[layer].depth_m) then
      begin
      EP.PercAndLatFlowForLayer_mm(layer, layerWithDrainageSystem, layers, params.watershedSlopeSteepness_mPm,
        percolationThisLayer_mm, lateralFlowThisLayer_mm, water.returnFlowTravelTime_days,
        params.timeForDrainageSystemToReducePlantStress_days, allowPercolation, allowLateralFlow);
      { if percolation + lateral flow exceeds water in layer, reduce both proportionally }
      if (percolationThisLayer_mm + lateralFlowThisLayer_mm > layers[layer].waterContent_mm) then
        begin
        proportion := safediv(layers[layer].waterContent_mm, percolationThisLayer_mm + lateralFlowThisLayer_mm);
        if allowPercolation then percolationThisLayer_mm := percolationThisLayer_mm * (proportion);
        if allowLateralFlow then lateralFlowThisLayer_mm := lateralFlowThisLayer_mm * (proportion);
        end;
      { reduce water content in layer }
      subtractQuantity(layers[layer].waterContent_mm, percolationThisLayer_mm + lateralFlowThisLayer_mm);
      layers[layer].lateralFlow_mm := lateralFlowThisLayer_mm;
      layers[layer].percolation_mm := percolationThisLayer_mm;
      end
    else 
      begin
      layers[layer].lateralFlow_mm := 0.0;
      layers[layer].percolation_mm := 0.0;
      percolationThisLayer_mm := 0.0;
      end;
    percolationLastLayer_mm := percolationThisLayer_mm;
    end;
  { now move from the bottom to the top doing two checks: if sw > po and if sw > fc. }
  { do this only if percolation is being carried out }
  if allowPercolation and (numLayers > 1) then
    for layer := numLayers - 1 downto 1 do
      begin
      if (layers[layer].waterContent_mm > layers[layer].porosity_mm) then
        begin
        movement_mm := layers[layer].waterContent_mm - layers[layer].porosity_mm;
        addQuantity(layers[layer-1].waterContent_mm, movement_mm);
        { can allow percolation to be negative to show upward movement }
        layers[layer-1].percolation_mm := layers[layer-1].percolation_mm - movement_mm;
        layers[layer].waterContent_mm := max(0.0, layers[layer].porosity_mm);
        end;
      if (layers[layer].waterContent_mm > layers[layer].fieldCapacity_mm) then
        begin
        swOverFCUpperLayer := safediv(layers[layer-1].waterContent_mm, layers[layer-1].fieldCapacity_mm);
        swOverFCThisLayer := safediv(layers[layer].waterContent_mm, layers[layer].fieldCapacity_mm);
        if (swOverFCUpperLayer < swOverFCThisLayer) then
          begin
          movement_mm := EQH.UpwardMovementOverFieldCapacityForLayer_mm(layers[layer].waterContent_mm,
            layers[layer-1].waterContent_mm, layers[layer].fieldCapacity_mm, layers[layer-1].fieldCapacity_mm);
          addQuantity(layers[layer-1].waterContent_mm, movement_mm);
          { can allow percolation to be negative to show upward movement }
          layers[layer-1].percolation_mm := layers[layer-1].percolation_mm - movement_mm;
          subtractQuantity(layers[layer].waterContent_mm, movement_mm);
          end;
        end;
      end;
  except on e: Exception do errorMessage('Exception in EP.PercAndLatFlowByLayer_mm: ' + e.message); end;
  end;

class procedure EP.PercAndLatFlowForLayer_mm(layer: integer; layerWithDrainageSystem: integer; var layers:
  LayerStructureArray; slopeSteepness_mPm: single; var percolationForLayer_mm: single; var lateralFlowForLayer_mm:
  single; returnFlowTravelTime_days: single; timeForDrainageSystemToReducePlantStress_days: single;
  allowPercolation, allowLateralFlow: boolean);
  var
    percolationTravelTime_hr, latFlowTravelTimeFactor, lateralFlowTravelTime_hr, combinedPercAndLatFlow_mm: single;
  begin
  try
  { assume this function is not called if both percolation and lateral flow are not allowed }
  { percolation and lateral flow only occur if soil water > field capacity }
  if (layers[layer].waterContent_mm <= layers[layer].fieldCapacity_mm) then
    begin
    percolationForLayer_mm := 0.0;
    lateralFlowForLayer_mm := 0.0;
    exit;
    end;
  { calculate travel times for perc and lat flow }
  if allowPercolation then
    percolationTravelTime_hr := EQH.PercolationTravelTimeForLayer_hr(layers[layer].porosity_mm,
        layers[layer].fieldCapacity_mm, layers[layer].saturatedConductivity_mmPhr)
  else
    percolationTravelTime_hr := 10000.0;
  if allowLateralFlow then
    begin
    latFlowTravelTimeFactor := EQH.LatFlowTravelTimeFactorForLayer(layer, layerWithDrainageSystem, layers,
        timeForDrainageSystemToReducePlantStress_days, slopeSteepness_mPm);
    lateralFlowTravelTime_hr := EQH.LateralFlowTravelTimeForLayer_hr(layers[layer].porosity_mm,
        layers[layer].fieldCapacity_mm, latFlowTravelTimeFactor);
    end
  else
    begin
    latFlowTravelTimeFactor := 10000.0;
    lateralFlowTravelTime_hr := 10000.0;
    end;
  percolationTravelTime_hr := max(0.0, percolationTravelTime_hr);
  lateralFlowTravelTime_hr := max(0.0, lateralFlowTravelTime_hr);
  { calculate combined perc and lat flow (simultaneous solution) }
  combinedPercAndLatFlow_mm := EQH.PercolationAndLatFlowForLayer_mm(layers[layer].waterContent_mm,
      layers[layer].fieldCapacity_mm, percolationTravelTime_hr, lateralFlowTravelTime_hr,
      allowPercolation, allowLateralFlow);
  { split combined total into perc and lat flow portions }
  if allowLateralFlow then
    begin
    if allowPercolation then { both }
      begin
      percolationForLayer_mm := EQH.PercolationForLayer_mm(combinedPercAndLatFlow_mm, lateralFlowTravelTime_hr,
          percolationTravelTime_hr);
      lateralFlowForLayer_mm := EQH.LateralFlowForLayer_mm(combinedPercAndLatFlow_mm, percolationForLayer_mm);
      end
    else { lateral flow only }
      begin
      lateralFlowForLayer_mm := combinedPercAndLatFlow_mm;
      percolationForLayer_mm := 0.0;
      end;
    { adjust lateral flow for return flow from hillside }
    lateralFlowForLayer_mm := EQH.LatFlowAdjForReturnFlow_mm(returnFlowTravelTime_days, lateralFlowForLayer_mm);
    end
  else { percolation only or neither }
    begin
    percolationForLayer_mm := combinedPercAndLatFlow_mm;
    lateralFlowForLayer_mm := 0.0;
    end;
  except on e: Exception do errorMessage('Exception in EP.PercAndLatFlowForLayer_mm: ' + e.message); end;
  end;

class function EP.PotentialSoilEvap_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure; var stationVars: WeatherStationVarsStructure;
    prevThreeDaysMeanTemp_degC: arrayThree; albedo_frn: single;
    meanPlantHeight_m: single; numPlants: integer): single;
  var
    penmanWindSpeedFunction_mmPdaykPa, soilHeatFlux_MJPM2, netRadiationForDay_MJPm2: single;
  begin
  try
  result := 0.0;
  penmanWindSpeedFunction_mmPdaykPa := 0.0;
  { calculate variables used by all methods }
  { change from EPIC }
  { EPIC assumes soilHeatFlux_MJPM2 to be zero }
  { prevThreeDaysMeanTemp_degC goes [0] = yesterday, [1] = day before yesterday, [2] = etc.}
  soilHeatFlux_MJPM2 := EQH.SoilHeatFlux_MJPM2(dailyWeather.meanTempForDay_degC, prevThreeDaysMeanTemp_degC);
  netRadiationForDay_MJPm2 := EQH.NetRadiationForDay_MJPm2(dailyWeather.radiationForDay_MJPm2, albedo_frn,
    dailyWeather.netOutgoingRadiationIfDayIsClear_MJPm2, dailyWeather.maxPossibleRadiation_MJPm2);
  case soilParams.potEvapMethod of
    kPenmanMethod:
      begin
      { uses radiation, air temperature, wind speed, relative humidity }
      penmanWindSpeedFunction_mmPdaykPa := EQH.PenmanWindSpeedFunction_mmPdaykPa(dailyWeather.meanWindSpeedForDay_mPsec);
      result := EQH.PotentialSoilEvapByPenman_mm(dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC,
        stationVars.psychrometerConstant_kPaPdegC, netRadiationForDay_MJPm2, dailyWeather.latentHeatOfVaporization_MJPkg,
        penmanWindSpeedFunction_mmPdaykPa, dailyWeather.vaporPressureDeficit_kPa, soilHeatFlux_MJPM2);
      end;
    kPenmanMonteithMethod:
      begin
      { uses radiation, air temperature, wind speed, relative humidity, and crop cover }
      { much longer than others so it has its own function }
    result := EP.PotSoilEvapByPenmanMonteith_mm(soilParams, dailyWeather, stationVars, soilHeatFlux_MJPM2,
        netRadiationForDay_MJPm2, meanPlantHeight_m, numPlants);
      end;
    kPriestleyTaylorMethod:
      begin
      { uses radiation, air temperature, relative humidity }
      netRadiationForDay_MJPm2 := EQH.NetRadiationByPriestleyTaylor_MJPm2(dailyWeather.radiationForDay_MJPm2, albedo_frn);
      result := EQH.PotentialSoilEvapByPriestleyTaylor_mm(netRadiationForDay_MJPm2,
        dailyWeather.latentHeatOfVaporization_MJPkg, dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC,
        stationVars.psychrometerConstant_kPaPdegC);
      end;
    kHargreavesMethod:
      begin
      { uses temperature only }
      result := EQH.PotentialSoilEvapByHargreaves_mm(dailyWeather.maxPossibleRadiation_MJPm2,
        dailyWeather.latentHeatOfVaporization_MJPkg, dailyWeather.meanTempForDay_degC, dailyWeather.maxTempForDay_degC,
        dailyWeather.minTempForDay_degC);
      end;
    end;
  if result < 0.0 then result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EP.PotentialSoilEvap_mm: ' + e.message); end;
  end;

class function EP.PotSoilEvapByPenmanMonteith_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure;  var stationVars: WeatherStationVarsStructure;
    soilHeatFlux_MJPM2: single; netRadiationForDay_MJPm2: single;
    meanPlantHeight_m: single; numPlants: integer): single;
  var
    aeroResistForHeatAndVaporTransfer_secPm, surfaceRoughParam_m, displacementHeightOfCrop_m, airDensity_gPm3: single;
  begin
  try
  aeroResistForHeatAndVaporTransfer_secPm := 0.0;
  surfaceRoughParam_m := 0.0;
  displacementHeightOfCrop_m := 0.0;
  airDensity_gPm3 := EQH.AirDensity_gPm3(stationVars.barometricPressure_kPa, dailyWeather.meanTempForDay_degC);
  if (numPlants > 0) then
    begin
    surfaceRoughParam_m := EQH.SurfaceRoughnessParam_m(meanPlantHeight_m);
    displacementHeightOfCrop_m := EQH.DisplacementHeightOfCrop_m(meanPlantHeight_m);
    aeroResistForHeatAndVaporTransfer_secPm := EQH.AeroResistForHeatAndVaporTransferIfCrop_secPm(meanPlantHeight_m,
      dailyWeather.meanWindSpeedForDay_mPsec, surfaceRoughParam_m, displacementHeightOfCrop_m);
    end
  else 
    begin
    aeroResistForHeatAndVaporTransfer_secPm :=
      EQH.AeroResistForHeatAndVaporTransferNoCrop_secPm(dailyWeather.meanWindSpeedForDay_mPsec);
    end;
  result := EQH.PotentialSoilEvapByPenmanMonteith_mm(dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC,
    netRadiationForDay_MJPm2, soilHeatFlux_MJPM2, airDensity_gPm3, dailyWeather.vaporPressureDeficit_kPa,
    aeroResistForHeatAndVaporTransfer_secPm, dailyWeather.latentHeatOfVaporization_MJPkg,
    stationVars.psychrometerConstant_kPaPdegC);
  except on e: Exception do result := errorMessage('Exception in EP.PotSoilEvapByPenmanMonteith_mm: ' + e.message); end;
  end;

class function EP.PotentialPlantEvap_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure; var stationVars: WeatherStationVarsStructure;
    prevThreeDaysMeanTemp_degC: arrayThree; albedo_frn: single;
    potSoilEvap_mm: single; plantList: TList): single;
  var
    i: integer;
    soilHeatFlux_MJPM2, netRadiationForDay_MJPm2: single;
    plant: GsPlant;
  begin
  try
  result := 0.0;
  soilHeatFlux_MJPM2 := EQH.SoilHeatFlux_MJPM2(dailyWeather.meanTempForDay_degC, prevThreeDaysMeanTemp_degC);
  netRadiationForDay_MJPm2 := EQH.NetRadiationForDay_MJPm2(dailyWeather.radiationForDay_MJPm2, albedo_frn,
    dailyWeather.netOutgoingRadiationIfDayIsClear_MJPm2, dailyWeather.maxPossibleRadiation_MJPm2);
  if (plantList.count > 0) then
    begin
    for i := 0 to plantList.count - 1 do
      begin
      plant := GsPlant(plantList.items[i]);
      if plant.awaitingReseeding then continue;
      if (soilParams.potEvapMethod = kPenmanMonteithMethod) then
        plant.water.potPlantEvap_mm := EP.PotPlantEvapByPenmanMonteith_mm(soilParams, dailyWeather,
          stationVars, soilHeatFlux_MJPM2, netRadiationForDay_MJPm2, plant)
      else
        plant.water.potPlantEvap_mm := EQH.PotentialPlantEvapNotPenmanMonteith_mm(potSoilEvap_mm,
          plant.development.leafAreaIndex);
      result := result + plant.water.potPlantEvap_mm;
      end;
    end;
  except on e: Exception do result := errorMessage('Exception in EP.PotentialPlantEvap_mm: ' + e.message); end;
  end;

class function EP.PotPlantEvapByPenmanMonteith_mm(var soilParams: SoilParamsStructure;
    var dailyWeather: DailyWeatherStructure; var stationVars: WeatherStationVarsStructure;
    soilHeatFlux_MJPM2: single; netRadiationForDay_MJPm2: single; plantProxy: GsModel): single;
  var
    aeroResistForHeatAndVaporTransfer_secPm, canopyResistForVaporTransfer_secPm, surfaceRoughParam_m: single;
    displacementHeightOfCrop_m, vaporPressureDeficitCorrectionFactor, leafConductance_mPsec, airDensity_gPm3: single;
    plant: GsPlant;
  begin
  try
  plant := GsPlant(plantProxy);
  airDensity_gPm3 := EQH.AirDensity_gPm3(stationVars.barometricPressure_kPa, dailyWeather.meanTempForDay_degC);
  surfaceRoughParam_m := EQH.SurfaceRoughnessParam_m(plant.biomass.height_m);
  displacementHeightOfCrop_m := EQH.DisplacementHeightOfCrop_m(plant.biomass.height_m);
  aeroResistForHeatAndVaporTransfer_secPm := EQH.AeroResistForHeatAndVaporTransferIfCrop_secPm(plant.biomass.height_m,
    dailyWeather.meanWindSpeedForDay_mPsec, surfaceRoughParam_m, displacementHeightOfCrop_m);
  vaporPressureDeficitCorrectionFactor := EQH.VaporPressureDeficitCorrectionFactor(
    plant.params.fractionOfMaxLeafConductForHighVPD.c, dailyWeather.vaporPressureDeficit_kPa,
    plant.params.thresholdVaporPressureDeficit_kPa);
  leafConductance_mPsec := EQH.LeafConductance_mPsec(plant.params.leafResistIfVPDBelowThreshold_mPsec,
    vaporPressureDeficitCorrectionFactor);
  canopyResistForVaporTransfer_secPm := EQH.CanopyResistForVaporTransfer_secPm(plant.params.canopyResistParam,
    plant.development.leafAreaIndex, leafConductance_mPsec, soilParams.carbonDioxideInAtmosphere_ppm);
  result := EQH.PotentialPlantEvapByPenmanMonteith_mm(dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC,
    netRadiationForDay_MJPm2, soilHeatFlux_MJPM2, airDensity_gPm3, dailyWeather.vaporPressureDeficit_kPa,
    aeroResistForHeatAndVaporTransfer_secPm, dailyWeather.latentHeatOfVaporization_MJPkg,
    stationVars.psychrometerConstant_kPaPdegC, canopyResistForVaporTransfer_secPm);
  if result < 0.0 then result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EP.PotPlantEvapByPenmanMonteith_mm: ' + e.message); end;
  end;

class function EP.PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm(soilCoverIndex_frn: single; potentialSoilEvap_mm:
  single; potPlantEvap_mm: single): single;
  var
    potSoilEvapAdjByCover_mm: single;
  begin
  try
  if (potentialSoilEvap_mm = 0.0) then
    result := 0.0
  else
    begin
    potSoilEvapAdjByCover_mm := EQH.PotentialSoilEvapAdjByCover_mm(potentialSoilEvap_mm, soilCoverIndex_frn);
    result := EQH.PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm(potSoilEvapAdjByCover_mm, potentialSoilEvap_mm,
        potPlantEvap_mm);
    end;
  except on e: Exception do result := errorMessage('Exception in EP.PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm: '
      + e.message); end;
  end;

class function EP.ActualSoilEvaporation_mm(numLayers: integer; var layers: LayerStructureArray; var
  snowWaterContent_mm: single; potSoilEvap_mm: single; plowDepth_m: single;
  lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single; var soilEvapByDepthCoeffs: SCurveStructure;
  var lowestLayerInWhichSoilEvapOccured: smallint): single;
  var
    layer: integer;
    patchTotalActualSoilEvap_mm, potentialSoilEvapForBottomLastLayer_mm, potentialSoilEvapForBottomThisLayer_mm: single;
    depth_mm, potSoilEvapAdjForDrynessThisLayer_mm, proportion, depthLastLayer_m: single;
    layerBelowCriterionDepthHasBeenConsidered: boolean;
  begin
  try
  lowestLayerInWhichSoilEvapOccured := 0;
  patchTotalActualSoilEvap_mm := 0.0;
  potentialSoilEvapForBottomLastLayer_mm := 0.0;
  potentialSoilEvapForBottomThisLayer_mm := 0.0;
  depth_mm := 0.0;
  potSoilEvapAdjForDrynessThisLayer_mm := 0.0;
  proportion := 0.0;
  depthLastLayer_m := 0.0;
  layerBelowCriterionDepthHasBeenConsidered := false;
  if (potSoilEvap_mm = 0.0) then
    begin
    result := 0.0;
    exit;
    end;
  { if there is more snow water than potential evap, take wholly out of snow }
  if (snowWaterContent_mm >= potSoilEvap_mm) then 
    begin
    subtractQuantity(snowWaterContent_mm, potSoilEvap_mm);
    lowestLayerInWhichSoilEvapOccured := 1;
    result := max(0.0, potSoilEvap_mm);
    exit;
    end;
  { otherwise take what there is in snow and then take some from the soil }
  patchTotalActualSoilEvap_mm := snowWaterContent_mm;
  snowWaterContent_mm := 0.0;
  { calculate soil evap from each layer }
  if numLayers > 0 then for layer := 0 to numLayers - 1 do 
    begin
    depth_mm := layers[layer].depth_m * m_to_mm;
    { calculate potential soil evap for specific depth at bottom of layer }
    potentialSoilEvapForBottomThisLayer_mm := EQH.PotentialSoilEvapForDepth(depth_mm, potSoilEvap_mm,
      soilEvapByDepthCoeffs);
    { adjust potential soil evap for this layer based on water content of this layer }
    potSoilEvapAdjForDrynessThisLayer_mm :=
      EQH.PotentialSoilEvapForLayerAdjForDryness_mm(potentialSoilEvapForBottomThisLayer_mm -
      potentialSoilEvapForBottomLastLayer_mm, layers[layer].waterContent_mm, layers[layer].fieldCapacity_mm,
      layers[layer].wiltingPoint_mm);
    { if this is the last layer in the plow depth, reduce soil evap proportionately }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, plowDepth_m,
      layerBelowCriterionDepthHasBeenConsidered);
    if (proportion < 1.0) then lowestLayerInWhichSoilEvapOccured := layer;
    { final calculation for soil evap for layer }
    layers[layer].evaporation_mm := EQH.SoilEvaporationForLayer_mm(proportion * layers[layer].waterContent_mm,
      potSoilEvapAdjForDrynessThisLayer_mm, lowerLimitWaterContentInTopP5MAsFractOfWP_frn * proportion,
      layers[layer].wiltingPoint_mm);
    { subtract evaporated water from layer and add to total actual evap }
    { (no need to bound here, is bounded above in dryness function) }
    subtractQuantity(layers[layer].waterContent_mm, layers[layer].evaporation_mm);
    addQuantity(patchTotalActualSoilEvap_mm, layers[layer].evaporation_mm);
    potentialSoilEvapForBottomLastLayer_mm := potentialSoilEvapForBottomThisLayer_mm;
    if (lowestLayerInWhichSoilEvapOccured = layer) then break;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  result := max(0.0, patchTotalActualSoilEvap_mm);
  except on e: Exception do result := errorMessage('Exception in EP.ActualSoilEvaporation_mm: ' + e.message); end;
  end;

class function EP.Snowmelt_mm(surfaceSoilTemp_degC: single; meanTempForDay_degC: single; snowWaterContent_mm: single):
  single;
  var
    snowPackTemp_degC: single;
  begin
  try
  if (snowWaterContent_mm = 0.0) then 
    begin
    result := 0.0;
    exit;
    end;
  snowPackTemp_degC := EQH.SnowPackTemp_degC(surfaceSoilTemp_degC, meanTempForDay_degC);
  result := EQH.Snowmelt_mm(meanTempForDay_degC, snowPackTemp_degC, snowWaterContent_mm);
  except on e: Exception do result := errorMessage('Exception in EP.Snowmelt_mm: ' + e.message); end;
  end;

class procedure EP.WaterTableDynamics(var water: WaterStructure; var params: SoilParamsStructure;
    numLayers: integer; var layers: LayerStructureArray; var totals: TotalsStructure);
  var
    layer: integer;
    firstLayerInWaterTableHasBeenConsidered: boolean;
    totalSoilWaterUnchanged_mm, totalSoilWaterChanged_mm, propInWaterTable, depthLastLayer_m, drivingFunction: single;
  begin
  try
  firstLayerInWaterTableHasBeenConsidered := false;
  totalSoilWaterUnchanged_mm := 0.0;
  totalSoilWaterChanged_mm := 0.0;
  propInWaterTable := 0.0;
  depthLastLayer_m := 0.0;
  { first figure new water table depth (changed by rainfall, runoff and soil evap) }
  drivingFunction := EQH.WaterTableDepthDrivingFunction(totals.sumRainfallMinusRunoffPrev30Days_mm,
    totals.sumPotSoilEvapPrev30Days_mm);
  water.waterTableDepth_m := EQH.WaterTableDepth_m(water.waterTableDepth_m,
    params.waterTableMaxDepth_m, params.waterTableMinDepth_m, drivingFunction);
  { make adjustments to water in soil layers only if water table is above lowest layer }
  if (water.waterTableDepth_m > layers[numLayers - 1].depth_m) then exit;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do 
    begin
    totalSoilWaterUnchanged_mm := totalSoilWaterUnchanged_mm + layers[layer].waterContent_mm;
    if (layers[layer].depth_m >= water.waterTableDepth_m) then 
      begin
      { set proportion of layer affected. < 1.0 for first layer in water table. }
      { 1.0 for layers wholly submerged }
      if ( not firstLayerInWaterTableHasBeenConsidered) then 
        begin
        { this check of sw and fc is done only on the first layer in the water table }
        if (layers[layer].waterContent_mm > layers[layer].fieldCapacity_mm) then
          layers[layer].waterContent_mm := max(0.0, layers[layer].fieldCapacity_mm);
        propInWaterTable := (layers[layer].depth_m - water.waterTableDepth_m) / (layers[layer].depth_m - depthLastLayer_m);
        firstLayerInWaterTableHasBeenConsidered := true;
        end
      else 
        begin
        propInWaterTable := 1.0;
        end;
      layers[layer].waterContent_mm := max(0.0, layers[layer].waterContent_mm * (1.0 - propInWaterTable) +
        layers[layer].porosity_mm * propInWaterTable);
      end;
    totalSoilWaterChanged_mm := totalSoilWaterChanged_mm + layers[layer].waterContent_mm;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  water.inflowFromWaterTable_mm := totalSoilWaterChanged_mm - totalSoilWaterUnchanged_mm;
  except on e: Exception do errorMessage('Exception in EP.WaterTableDynamics: ' + e.message); end;
  end;

class function EP.WaterErosionForDay_tPha(var params: SoilParamsStructure; var erosion: ErosionStructure;
    patchMeanMinCropManagementFactor: single; var layers: LayerStructureArray; var water: WaterStructure;
    var userCoeffsForMUSI: arrayFour; patchTotalAboveGroundBiomassAndResidue_tPha: single): single;
  var
    energyComponent, slopeLengthFactorParam, slopeLengthAndSteepnessFactor, cropManagementFactor,
      soilErodibilityFactor, coarseFragmentFactor: single;
  begin
  try
  { if the method is USLE, erosion is zero if rainfall is < 12.7 mm }
  if (params.waterErosionMethod = kUSLE) and (water.patchRainfallForDay_mm < 12.7) then
    begin
    result := 0.0;
    exit;
    end;
  { if the method is any other than USLE, erosion is zero if runoff is < 1 mm }
  if (params.waterErosionMethod <> kUSLE) and (water.runoffVolume_mm < 1.0) then
    begin
    result := 0.0;
    exit;
    end;
  slopeLengthFactorParam := EQ.SlopeLengthFactorParam(params.watershedSlopeSteepness_mPm);
  slopeLengthAndSteepnessFactor := EQ.SlopeLengthAndSteepnessFactor(params.watershedSlopeLength_m, slopeLengthFactorParam,
    params.watershedSlopeSteepness_mPm);
  cropManagementFactor := EQ.CropManagementFactor(patchMeanMinCropManagementFactor,
    patchTotalAboveGroundBiomassAndResidue_tPha);
  soilErodibilityFactor := EQ.SoilErodibilityFactor(layers[0].sandContent_pct, layers[0].siltContent_pct,
    layers[0].clayContent_pct, layers[0].organicMatter_tPha, layers[0].weight_tPha);
  coarseFragmentFactor := EQ.CoarseFragmentFactor(layers[0].rockContent_pct);
  case params.waterErosionMethod of
    kMUST:
      begin
      energyComponent := EQ.WaterErosionEnergyComponentByMUST(water.runoffVolume_mm, water.peakRunoffRate_mmPhr);
      end;
    kOnstadFoster:
      begin
      energyComponent := EQ.WaterErosionEnergyComponentByOnstadFoster(water.runoffVolume_mm, water.peakRunoffRate_mmPhr,
        erosion.rainfallEnergyFactorForUSLE);
      end;
    kUSLE:
      begin
      energyComponent := EQ.WaterErosionEnergyComponentByUSLE(erosion.rainfallEnergyFactorForUSLE);
      end;
    kMUSS:
      begin
      energyComponent := EQ.WaterErosionEnergyComponentByMUSS(water.runoffVolume_mm, water.peakRunoffRate_mmPhr,
        params.watershedArea_ha);
      end;
    kMUSLE:
      begin
      energyComponent := EQ.WaterErosionEnergyComponentByMUSLE(water.runoffVolume_mm, water.peakRunoffRate_mmPhr,
        params.watershedArea_ha);
      end;
    kMUSI:
      begin
      energyComponent := EQ.WaterErosionEnergyComponentByMUSI(water.runoffVolume_mm, water.peakRunoffRate_mmPhr,
        params.watershedArea_ha, userCoeffsForMUSI);
      end;
    else
     energyComponent := 0.0;
    end;
  result := EQ.WaterErosion_tPha(energyComponent, soilErodibilityFactor, cropManagementFactor,
    params.erosionControlPracticeFactor, slopeLengthAndSteepnessFactor, coarseFragmentFactor);
  except on e: Exception do result := errorMessage('Exception in EP.WaterErosionForDay_tPha: ' + e.message); end;
  end;

class function EP.WindErosionForDay_tPha(var params: SoilParamsStructure; var dailyWeather: DailyWeatherStructure;
    var surface: SurfaceStructure; var plantMeans: PlantMeansStructure; var layers: LayerStructureArray;
    var erosion: ErosionStructure; dailyMeanWindSpeedForMonth_mPsec: single): single;
  var
    soilErodibilityFactorWE, angleOfWindRelativeToRidges_rad, unsheltFieldLengthAlongPrevailingWindDir_m,
      meanUnsheltDistanceFactorWE, clodRoughnessFactorWE, ridgeRoughnessFactorWE, coeffSurfaceRoughnessFactorWE,
      expSurfaceRoughnessFactorWE, surfaceRoughnessFactorWE, vegCoverEquivFactorWE, vegetativeCoverFactorWE_frn,
      coarseFragmentFactorWE, thresholdFrictionVelocityWE_mPsec, weForFractionOfDayFactor, potentialWindErosion_tPha,
      windErosion_tPha: single;
    begin
    try
    soilErodibilityFactorWE := 0.0;
    angleOfWindRelativeToRidges_rad := 0.0;
    unsheltFieldLengthAlongPrevailingWindDir_m := 0.0;
    meanUnsheltDistanceFactorWE := 0.0;
    clodRoughnessFactorWE := 0.0;
    ridgeRoughnessFactorWE := 0.0;
    coeffSurfaceRoughnessFactorWE := 0.0;
    expSurfaceRoughnessFactorWE := 0.0;
    surfaceRoughnessFactorWE := 0.0;
    vegCoverEquivFactorWE := 0.0;
    vegetativeCoverFactorWE_frn := 0.0;
    coarseFragmentFactorWE := 0.0;
    thresholdFrictionVelocityWE_mPsec := 0.0;
    weForFractionOfDayFactor := 0.0;
    potentialWindErosion_tPha := 0.0;
    windErosion_tPha := 0.0;
    { soil erodibility factor }
    soilErodibilityFactorWE := EQ.SoilErodibilityFactorWE(layers[0].sandContent_pct, layers[0].clayContent_pct,
      layers[0].siltContent_pct, layers[0].calciumCarbonate_pct);
    { unsheltered distance factor }
    angleOfWindRelativeToRidges_rad := EQ.AngleOfWindRelativeToRidges_rad(dailyWeather.windDirectionForDay_rad,
      params.watershedFieldLengthOrientationFromNorth_rad);
    unsheltFieldLengthAlongPrevailingWindDir_m := EQ.UnsheltFieldLengthAlongPrevailingWindDir_m(
      params.watershedFieldLength_km * km_to_m, params.watershedFieldWidth_km * km_to_m, angleOfWindRelativeToRidges_rad);
    meanUnsheltDistanceFactorWE := EQ.MeanUnsheltDistanceFactorWE(unsheltFieldLengthAlongPrevailingWindDir_m);
    { surface roughness factor }
    if (surface.randomRoughness_mm = 0.0) and (surface.ridgeHeight_mm = 0.0) then
      surfaceRoughnessFactorWE := 1.0
    else
      begin
      clodRoughnessFactorWE := EQ.ClodRoughnessFactorWE(surface.randomRoughness_mm);
      ridgeRoughnessFactorWE := EQ.RidgeRoughnessFactorWE(angleOfWindRelativeToRidges_rad, surface.ridgeHeight_mm);
      coeffSurfaceRoughnessFactorWE := EQ.CoeffSurfaceRoughnessFactorWE(clodRoughnessFactorWE, ridgeRoughnessFactorWE);
      expSurfaceRoughnessFactorWE := EQ.ExpSurfaceRoughnessFactorWE(surface.ridgeHeight_mm);
      surfaceRoughnessFactorWE := EQ.SurfaceRoughnessFactorWE(coeffSurfaceRoughnessFactorWE,
        expSurfaceRoughnessFactorWE);
      end;
  { vegetative cover factor }
  vegCoverEquivFactorWE := EQ.VegCoverEquivFactorWE(plantMeans.totalStandingLive_tPha,
      plantMeans.totalStandingDead_tPha, layers[0].flatCropResidue_tPha, plantMeans.meanWindErosionFactorStandingLive,
      plantMeans.meanWindErosionFactorStandingDead, plantMeans.meanWindErosionFactorFlatResidue);
  vegetativeCoverFactorWE_frn := EQ.VegetativeCoverFactorWE_frn(vegCoverEquivFactorWE);
  { coarse fragment factor }
  coarseFragmentFactorWE := EQ.CoarseFragmentFactorWE(layers[0].rockContent_pct);
  { threshold friction velocity }
  thresholdFrictionVelocityWE_mPsec := EQ.ThresholdFrictionVelocityWE_mPsec(params.soilParticleDiameter_microns);
  { factor for wind erosion for fraction of day (not in chapter) }
  weForFractionOfDayFactor := EQ.WindErosionForFractionOfDayFactor(thresholdFrictionVelocityWE_mPsec,
      safediv(layers[0].waterContent_mm, layers[0].wiltingPoint_mm), erosion.windErosionAccumulatedFactor);
  { integration of wind erosion over day }
  potentialWindErosion_tPha := EP.IntegratePotentialWindErosion(dailyMeanWindSpeedForMonth_mPsec,
      dailyWeather.meanWindSpeedForDay_mPsec, thresholdFrictionVelocityWE_mPsec, weForFractionOfDayFactor,
      meanUnsheltDistanceFactorWE);
  { final equation for wind erosion }
  windErosion_tPha := EQ.WindErosion_tPha(soilErodibilityFactorWE, surfaceRoughnessFactorWE,
      vegetativeCoverFactorWE_frn, coarseFragmentFactorWE, potentialWindErosion_tPha, params.maxWindErosionPerDay_tPha);
  { update accumulated factor }
  erosion.windErosionAccumulatedFactor := erosion.windErosionAccumulatedFactor
      * safeExp(-params.windErosionAccumCoeff * windErosion_tPha);
  result := windErosion_tPha;
  except on e: Exception do result := errorMessage('Exception in EP.WindErosionForDay_tPha: ' + e.message); end;
  end;

class function EP.IntegratePotentialWindErosion(dailyMeanWindSpeedForMonth_mPsec: single; meanWindSpeedForDay_mPsec:
  single; thresholdFrictionVelocityWE_mPsec: single; weForFractionOfDayFactor: single; meanUnsheltDistanceFactorWE:
  single): single;
    var
      { The integration of equations 141 (WindErosionRateForFractionOfDay_kgPmsec) and 107 }
      { (WindSpeedForFractionOfDay_mPsec) is accomplished numerically using variable }
      { delta-f steps. Large steps of delta-f = 0.1 are taken initially in simulating }
      { low wind speeds. The step size is reduced as f is reduced to gain better }
      { definition of the wind speed distribution at high speeds. At the end of }
      { each integration step, delta-f is cut in half if }
      { meanFractionOfDayWindSpeedThisInterval_frn >= 0.1. }
      windSpeedForFractionOfDayParam2: single;
      windSpeedForFractionOfDayParam1: single;
      fractionOfDay_frn: single;
      lastFractionOfDay_frn: single;
      integrationInterval_frn: single;
      integratedWindErosion_tPha: single;
      fractionOfDayWindSpeedAtIntervalStart_frn: single;
      fractionOfDayWindSpeedAtIntervalEnd_frn: single;
      meanFractionOfDayWindSpeedThisInterval_frn: single;
      erosionAtIntervalStart_kgPha: single;
      erosionAtIntervalEnd_kgPha: single;
      meanErosionThisInterval_kgPha: single;
    begin
    try
    windSpeedForFractionOfDayParam2 := 0.0;
    windSpeedForFractionOfDayParam1 := 0.0;
    fractionOfDay_frn := 0.0;
    lastFractionOfDay_frn := 0.0;
    integrationInterval_frn := 0.1;
    integratedWindErosion_tPha := 0.0;
    fractionOfDayWindSpeedAtIntervalStart_frn := 0.0;
    fractionOfDayWindSpeedAtIntervalEnd_frn := 0.0;
    meanFractionOfDayWindSpeedThisInterval_frn := 0.0;
    erosionAtIntervalStart_kgPha := 0.0;
    erosionAtIntervalEnd_kgPha := 0.0;
    meanErosionThisInterval_kgPha := 0.0;
    { calculate parameters used in equation to calculate wind speed for fraction of day }
    windSpeedForFractionOfDayParam2 := EQ.WindSpeedForFractionOfDayParam2(dailyMeanWindSpeedForMonth_mPsec,
      meanWindSpeedForDay_mPsec);
    windSpeedForFractionOfDayParam1 := EQ.WindSpeedForFractionOfDayParam1(windSpeedForFractionOfDayParam2);
    { set up fractions to start integration }
    fractionOfDayWindSpeedAtIntervalStart_frn := safediv(thresholdFrictionVelocityWE_mPsec / 0.0408,
      meanWindSpeedForDay_mPsec);
    lastFractionOfDay_frn := EQ.FractionOfDayToStartSimulatingWindSpeed_frn(fractionOfDayWindSpeedAtIntervalStart_frn,
      windSpeedForFractionOfDayParam2);
    { integration loop }
    while (integrationInterval_frn > 0.0001) do
      begin
      fractionOfDay_frn := lastFractionOfDay_frn - integrationInterval_frn;
      if (fractionOfDay_frn > 0.0) then 
        begin
        { wind speed at end of interval }
        fractionOfDayWindSpeedAtIntervalEnd_frn := EQ.WindSpeedForFractionOfDay_mPsec(fractionOfDay_frn,
          windSpeedForFractionOfDayParam1, windSpeedForFractionOfDayParam2);
        { erosion at end of interval }
        erosionAtIntervalEnd_kgPha := EP.WindErosionForFractionOfDay_kgPm(meanWindSpeedForDay_mPsec,
          fractionOfDayWindSpeedAtIntervalEnd_frn, weForFractionOfDayFactor, meanUnsheltDistanceFactorWE);
        { mean wind speed and erosion for interval = (start + end) * interval }
        meanFractionOfDayWindSpeedThisInterval_frn := (fractionOfDayWindSpeedAtIntervalEnd_frn +
          fractionOfDayWindSpeedAtIntervalStart_frn) * integrationInterval_frn;
        meanErosionThisInterval_kgPha := (erosionAtIntervalEnd_kgPha + erosionAtIntervalStart_kgPha) *
          integrationInterval_frn;
        { add mean erosion this interval to total }
        integratedWindErosion_tPha := integratedWindErosion_tPha + (meanErosionThisInterval_kgPha);
        { set carry-overs to use in next interval }
        lastFractionOfDay_frn := fractionOfDay_frn;
        fractionOfDayWindSpeedAtIntervalStart_frn := fractionOfDayWindSpeedAtIntervalEnd_frn;
        erosionAtIntervalStart_kgPha := erosionAtIntervalEnd_kgPha;
        { reduce interval as wind speed gets higher }
        if (meanFractionOfDayWindSpeedThisInterval_frn >= 0.1) then integrationInterval_frn := integrationInterval_frn *
          (0.5);
        end
      else 
        begin
        { if fraction gets < 0, reduce interval until it reaches the limit of integration }
        integrationInterval_frn := integrationInterval_frn * (0.5);
        end;
      end;
  result := integratedWindErosion_tPha * 0.5;
  except on e: Exception do result := errorMessage('Exception in EP.IntegratePotentialWindErosion: ' + e.message); end;
  end;

class function EP.WindErosionForFractionOfDay_kgPm(meanWindSpeedForDay_mPsec: single; fractionOfDayWindSpeedAtTime_frn:
  single; weForFractionOfDayFactor: single; meanUnsheltDistanceFactorWE: single): single;
  var
    windSpeedForTime_m3Psec: single;
    frictionVelocity_mPsec: single;
  begin
  try
  windSpeedForTime_m3Psec := meanWindSpeedForDay_mPsec * fractionOfDayWindSpeedAtTime_frn;
  frictionVelocity_mPsec := EQ.FrictionVelocityWE_mPsec(windSpeedForTime_m3Psec);
  result := EQ.WindErosionRateForFractionOfDay_kgPmsec(frictionVelocity_mPsec, weForFractionOfDayFactor,
      meanUnsheltDistanceFactorWE);
  except on e: Exception do result := errorMessage('Exception in EP.WindErosionForFractionOfDay_kgPm: ' + e.message); end;
  end;

class procedure EP.PFluxBetweenLabileAndMineral(layer: integer; var layers: LayerStructureArray;
    allowLabileAndMineralPFlow, allowActiveAndStableMineralPFlow: boolean);
  begin
  try
  { labile to active mineral }
  if allowLabileAndMineralPFlow then with layers[layer] do
    begin
    pFlowFromLabileToActiveMineral_kgPha := EQ.PFlowFromLabileToActiveMineral_kgPha(labileP_kgPha,
        mineralPActive_kgPha, pSorptionCoeff_frn);
    subtractQuantity(labileP_kgPha, pFlowFromLabileToActiveMineral_kgPha);
    { added to active mineral after next section }
    end
  else
    layers[layer].pFlowFromLabileToActiveMineral_kgPha := 0.0;
  { active mineral to stable mineral }
  if allowActiveAndStableMineralPFlow then with layers[layer] do
    begin
    mineralPFlowFromActiveToStable_kgPha := EQ.MineralPFlowFromActiveToStableForLayer_kgPha(mineralPFlowCoeff_Pday,
        mineralPActive_kgPha, mineralPStable_kgPha);
    subtractQuantity(mineralPActive_kgPha, mineralPFlowFromActiveToStable_kgPha);
    addQuantity(mineralPStable_kgPha, mineralPFlowFromActiveToStable_kgPha);
    end
  else
    layers[layer].mineralPFlowFromActiveToStable_kgPha := 0.0;
  { from above }
  with layers[layer] do
    addQuantity(mineralPActive_kgPha, pFlowFromLabileToActiveMineral_kgPha);
  except on e: Exception do errorMessage('Exception in EP.PFluxBetweenLabileAndMineral: ' + e.message); end;
  end;

class procedure EP.NAndPLossInRunoffAndLeachingForSurfaceLayer(var layers: LayerStructureArray; var
  nitrateEnteringLayer_kgPha: single; var labilePEnteringLayer_kgPha: single; nitrogenLeachingParam: single; var
  water: WaterStructure; var movement: MovementStructure;
  allowNLossInRunoff, allowPLossInRunoff, allowNLossInLateralFlow, allowNLossInPercolation, allowPLossInPercolation: boolean);
  var
    nitrateLeachedForLayer_kgPha, volumeMovingThroughLayer_mm, meanNitrateConcDuringWaterFlow_gPm3: single;
    totalRunoff_mm, meanPConcDuringWaterFlow_gPm3, labilePLossInRunoff_kgPha: single;
  begin
  try
  { add n in rainfall to first layer }
  { do this even if none of the flags are true, because it is turned off elsewhere and will be zero if turned off }
  addQuantity(layers[0].nitrate_kgPha, nitrateEnteringLayer_kgPha);
  { n leaching from percolation }
  volumeMovingThroughLayer_mm := layers[0].percolation_mm;
  nitrateLeachedForLayer_kgPha := 0.0;
  if allowNLossInPercolation and (volumeMovingThroughLayer_mm > 0.00001) then  { notation: 1.e-5 }
    begin
    nitrateLeachedForLayer_kgPha := EQ.NitrateLeachedFromLayer_kgPha(layers[0].nitrate_kgPha,
        volumeMovingThroughLayer_mm, nitrogenLeachingParam, layers[0].porosity_mm);
    subtractQuantity(layers[0].nitrate_kgPha, nitrateLeachedForLayer_kgPha);
    end;
  { nitrateEnteringLayer_kgPha moves into the next layer (pointer is used because other layers are in different fxn) }
  nitrateEnteringLayer_kgPha := nitrateLeachedForLayer_kgPha;
  layers[0].nitrateLeachedFromPercolation_kgPha := nitrateLeachedForLayer_kgPha;
  { n leaching from runoff and lateral flow }
  { this n is lost from the system }
  totalRunoff_mm := water.runoffVolume_mm + water.irrigationRunoffVolume_mm;
  volumeMovingThroughLayer_mm := 0.0;
  if allowNLossInRunoff then volumeMovingThroughLayer_mm := volumeMovingThroughLayer_mm + totalRunoff_mm;
  if allowNLossInLateralFlow then volumeMovingThroughLayer_mm := volumeMovingThroughLayer_mm + layers[0].lateralFlow_mm;
  if (volumeMovingThroughLayer_mm <= 0.0) then
    begin
    meanNitrateConcDuringWaterFlow_gPm3 := 0.0;
    nitrateLeachedForLayer_kgPha := 0.0;
    end
  else
    begin
    nitrateLeachedForLayer_kgPha := EQ.NitrateLeachedFromLayer_kgPha(layers[0].nitrate_kgPha,
        volumeMovingThroughLayer_mm, nitrogenLeachingParam, layers[0].porosity_mm);
    meanNitrateConcDuringWaterFlow_gPm3 := EQ.MeanNitrateConcInLayerDuringWaterFlow_gPm3(nitrateLeachedForLayer_kgPha,
        volumeMovingThroughLayer_mm);
    end;
  subtractQuantity(layers[0].nitrate_kgPha, nitrateLeachedForLayer_kgPha);
  if allowNLossInRunoff then
    movement.nitrateLostInRunoff_kgPha := totalRunoff_mm * meanNitrateConcDuringWaterFlow_gPm3
  else
    movement.nitrateLostInRunoff_kgPha := 0.0;
  if allowNLossInLateralFlow then
    layers[0].nitrateLeachedFromLateralFlow_kgPha := layers[0].lateralFlow_mm * meanNitrateConcDuringWaterFlow_gPm3
  else
    layers[0].nitrateLeachedFromLateralFlow_kgPha := 0.0;
  { labile p loss in runoff and percolation calculated together }
  volumeMovingThroughLayer_mm := 0.0;
  if allowPLossInRunoff then volumeMovingThroughLayer_mm := volumeMovingThroughLayer_mm + totalRunoff_mm;
  if allowPLossInPercolation then volumeMovingThroughLayer_mm := volumeMovingThroughLayer_mm + layers[0].percolation_mm;
  if (volumeMovingThroughLayer_mm <= 0.0) then
    meanPConcDuringWaterFlow_gPm3 := 0.0
  else
    meanPConcDuringWaterFlow_gPm3 := EQ.MeanPConcDuringLeaching_gPm3(layers[0].labileP_kgPha,
        volumeMovingThroughLayer_mm, layers[0].weight_tPha);
  { separate p into loss in percolation and loss in runoff }
  if allowPLossInPercolation then
    labilePEnteringLayer_kgPha := meanPConcDuringWaterFlow_gPm3 * layers[0].percolation_mm
  else
    labilePEnteringLayer_kgPha := 0.0;
  subtractQuantity(layers[0].labileP_kgPha, labilePEnteringLayer_kgPha);
  layers[0].labilePLeachedFromPercolation_kgPha := labilePEnteringLayer_kgPha;
  if allowPLossInRunoff then
    labilePLossInRunoff_kgPha := totalRunoff_mm * meanPConcDuringWaterFlow_gPm3
  else
    labilePLossInRunoff_kgPha := 0.0;
  subtractQuantity(layers[0].labileP_kgPha, labilePLossInRunoff_kgPha);
  movement.labilePLostInRunoff_kgPha := labilePLossInRunoff_kgPha;
  { only percolation part passes on to next layer }
  { also note p is not lost in lateral flow }
  except on e: Exception do errorMessage('Exception in EP.NAndPLossInRunoffAndLeachingForSurfaceLayer: ' + e.message); end;
  end;

class procedure EP.LeachNandPNonSurfaceLayer(layer: integer; var layers: LayerStructureArray; var
  nitrateEnteringLayer_kgPha: single; var labilePEnteringLayer_kgPha: single; nitrogenLeachingParam: single;
  allowNLossInPercolation, allowPLossInPercolation, allowNLossInLateralFlow: boolean);
  var
    volumeMovingThroughLayer_mm, nitrateLeachedForLayer_kgPha, meanNitrateConcDuringWaterFlow_gPm3: single;
    meanPConcDuringWaterFlow_gPm3: single;
  begin
  try
  volumeMovingThroughLayer_mm := 0.0;
  nitrateLeachedForLayer_kgPha := 0.0;
  meanNitrateConcDuringWaterFlow_gPm3 := 0.0;
  meanPConcDuringWaterFlow_gPm3 := 0.0;
  { add n and p that percolated from upper layer }
  addQuantity(layers[layer].nitrate_kgPha, nitrateEnteringLayer_kgPha);
  addQuantity(layers[layer].labileP_kgPha, labilePEnteringLayer_kgPha);
  if (layers[layer].percolation_mm + layers[layer].lateralFlow_mm <= 0.0) then
    begin
    layers[layer].labilePLeachedFromPercolation_kgPha := 0.0;
    layers[layer].nitrateLeachedFromPercolation_kgPha := 0.0;
    layers[layer].nitrateLeachedFromLateralFlow_kgPha := 0.0;
    end
  else
    begin
    { calculate n leached from percolation and lateral flow together }
    volumeMovingThroughLayer_mm := 0.0;
    if allowNLossInPercolation then
      volumeMovingThroughLayer_mm := volumeMovingThroughLayer_mm + layers[layer].percolation_mm;
    if allowNLossInLateralFlow then
      volumeMovingThroughLayer_mm := volumeMovingThroughLayer_mm + layers[layer].lateralFlow_mm;
    nitrateLeachedForLayer_kgPha := EQ.NitrateLeachedFromLayer_kgPha(layers[layer].nitrate_kgPha,
      volumeMovingThroughLayer_mm, nitrogenLeachingParam, layers[layer].porosity_mm);
    meanNitrateConcDuringWaterFlow_gPm3 := EQ.MeanNitrateConcInLayerDuringWaterFlow_gPm3(nitrateLeachedForLayer_kgPha,
        volumeMovingThroughLayer_mm);
    subtractQuantity(layers[layer].nitrate_kgPha, nitrateLeachedForLayer_kgPha);
    { separate n leached into that from percolation and that from lateral flow }
    if allowNLossInPercolation then
      layers[layer].nitrateLeachedFromPercolation_kgPha := meanNitrateConcDuringWaterFlow_gPm3 *
          layers[layer].percolation_mm
    else
      layers[layer].nitrateLeachedFromPercolation_kgPha := 0.0;
    if allowNLossInLateralFlow then
      layers[layer].nitrateLeachedFromLateralFlow_kgPha := meanNitrateConcDuringWaterFlow_gPm3 *
          layers[layer].lateralFlow_mm
    else
      layers[layer].nitrateLeachedFromLateralFlow_kgPha := 0.0;
    { p is not lost by lateral flow, only by percolation }
    if allowPLossInPercolation then
      begin
      meanPConcDuringWaterFlow_gPm3 := EQ.MeanPConcDuringLeaching_gPm3(layers[layer].labileP_kgPha,
        layers[layer].percolation_mm, layers[layer].weight_tPha);
      layers[layer].labilePLeachedFromPercolation_kgPha := meanPConcDuringWaterFlow_gPm3 * layers[layer].percolation_mm;
      subtractQuantity(layers[layer].labileP_kgPha, layers[layer].labilePLeachedFromPercolation_kgPha);
      end
    else
      layers[layer].labilePLeachedFromPercolation_kgPha := 0.0;
    end;
  { set amounts of n and p percolating down to next layer }
  nitrateEnteringLayer_kgPha := layers[layer].nitrateLeachedFromPercolation_kgPha;
  labilePEnteringLayer_kgPha := layers[layer].labilePLeachedFromPercolation_kgPha;
  except on e: Exception do errorMessage('Exception in EP.LeachNandPNonSurfaceLayer: ' + e.message); end;
  end;

class procedure EP.NitrificationAndVolatilization(layer: integer; var layers: LayerStructureArray;
  meanWindSpeedForDay_mPsec: single; var nVolatilizationByDepthCoeffs: SCurveStructure;
  allowNitrification, allowVolatilization: boolean);
  var
    combinedTempFactor, depthToMiddleOfLayer_mm,
        volatilWindSpeedFactorForSurface, volatilCECFactor, volatilDepthFactor,
        volatilRegulator, nitrifSoilWaterFactor, nitrifPHFactor, nitrifRegulator, unadjVolatil_kgPha,
        unadjNitrif_kgPha, combined_kgPha: single;
  begin
  try
  with layers[layer] do
  begin
  depthToMiddleOfLayer_mm := 0.0;
  { temperature factor }
  combinedTempFactor := EQ.NitrifAndVolatilTempFactorForLayer(temperature_degC);
  { if temperature factor is < 0, no nitrif or volatil occurs }
  if (combinedTempFactor <= 0.0) then
    begin
    nitrification_kgPha := 0.0;
    volatilization_kgPha := 0.0;
    exit;
    end;
  if allowVolatilization then
    begin
    { calculate volatilization regulator for layer }
    { for surface layer, depends on wind speed and temperature }
    { for other layers, depends on CEC, depth, and temperature }
    if (layer = 0) then
      begin
      volatilWindSpeedFactorForSurface := EQ.VolatilWindSpeedFactorForSurface(meanWindSpeedForDay_mPsec);
      volatilRegulator := EQ.VolatilRegulatorForSurfaceLayer(combinedTempFactor, volatilWindSpeedFactorForSurface);
      end
    else
      begin
      volatilCECFactor := EQ.VolatilCationExchangeCapFactorForLayer(cationExchangeCapacity_cmolPkg);
      depthToMiddleOfLayer_mm := EQ.DepthToMiddleOfSoilLayer_mm(layer, layers);
      volatilDepthFactor := EQ.VolatilDepthFactorForLayer(depthToMiddleOfLayer_mm, nVolatilizationByDepthCoeffs);
      volatilRegulator := EQ.VolatilRegulatorForNonSurfaceLayer(combinedTempFactor, volatilCECFactor, volatilDepthFactor);
      end;
    end
  else
    volatilRegulator := 0.0;
  if allowNitrification then
    begin
    { calculate nitrification regulator for layer }
    { for all layers, depends on soil water, pH, and temp }
    nitrifSoilWaterFactor := EQ.NitrifSoilWaterFactorForLayer(layer, layers);
    nitrifPHFactor := EQ.NitrifPHFactorForLayer(soilpH);
    nitrifRegulator := EQ.NitrifRegulatorForLayer(combinedTempFactor, nitrifSoilWaterFactor, nitrifPHFactor);
    end
  else
    nitrifRegulator := 0.0;
  if (volatilRegulator + nitrifRegulator <= 0.00001) then { notation: 1.e-5 }
    begin
    nitrification_kgPha := 0.0;
    volatilization_kgPha := 0.0;
    end
  else
    begin
    if allowVolatilization then
      begin
      unadjVolatil_kgPha := EQ.UnadjVolatilForLayer_kgPha(volatilRegulator);
      volatilization_kgPha := unadjVolatil_kgPha;
      end
    else
      begin
      unadjVolatil_kgPha := 0.0;
      volatilization_kgPha := 0.0;
      end;
    if allowNitrification then
      begin
      unadjNitrif_kgPha := EQ.UnadjNitrifForLayer_kgPha(nitrifRegulator);
      nitrification_kgPha := unadjNitrif_kgPha;
      end
    else
      begin
      unadjNitrif_kgPha := 0.0;
      nitrification_kgPha := 0.0;
      end;
    if allowVolatilization and allowNitrification then
      begin
      combined_kgPha := EQ.CombinedNitrifAndVolatilForLayer_kgPha(ammonia_kgPha, nitrifRegulator, volatilRegulator);
      nitrification_kgPha := EQ.NitrificationForLayer_kgPha(combined_kgPha, unadjNitrif_kgPha, unadjVolatil_kgPha);
      volatilization_kgPha := EQ.VolatilizationForLayer_kgPha(combined_kgPha, nitrification_kgPha);
      end;
    subtractQuantity(ammonia_kgPha, volatilization_kgPha + nitrification_kgPha);
    addQuantity(nitrate_kgPha, nitrification_kgPha);
    end;
  end;
  except on e: Exception do errorMessage('Exception in EP.NitrificationAndVolatilization: ' + e.message); end;
  end;

class procedure EP.NFluxBetweenActiveAndStableHumus(layer: integer; var layers: LayerStructureArray);
  begin
  try
  layers[layer].organicNFromActiveToStableInHumus_kgPha :=
      EQ.OrganicNFromActiveToStableInHumusForLayer_kgPha(layers[layer].organicNActiveHumus_kgPha,
      layers[layer].organicNStableHumus_kgPha, layers[layer].organicNActiveHumusFractionAtInput_frn);
  addQuantity(layers[layer].organicNStableHumus_kgPha, layers[layer].organicNFromActiveToStableInHumus_kgPha);
  subtractQuantity(layers[layer].organicNActiveHumus_kgPha, layers[layer].organicNFromActiveToStableInHumus_kgPha);
  except on e: Exception do errorMessage('Exception in EP.NFluxBetweenActiveAndStableHumus: ' + e.message); end;
  end;

class procedure EP.Mineralization(layer: integer; var layers: LayerStructureArray;
  plowDepthSettledBulkDensityAtInput_tPm3: single; var movement: MovementStructure;
  allowActiveHumusNMineralization, allowActiveHumusPMineralization, allowFreshNMineralization,
  allowFreshPMineralization: boolean);
  var
    cNRatioInCropResidue, cPRatioInCropResidue, cNPCompositeRatio, flatCropResidue_kgPha,
      organicNTotalHumus_kgPha: single;
  begin
  try
  with layers[layer] do
  begin
  flatCropResidue_kgPha := flatCropResidue_tPha * t_to_kg;
  organicNTotalHumus_kgPha := organicNStableHumus_kgPha + organicNActiveHumus_kgPha;
  { humus n and p mineralization }
  { don't change materials until fresh mineralization is finished }
  if allowActiveHumusNMineralization then
    begin
    activeHumusNMineralization_kgPha := EQ.ActiveHumusNMineralizationForLayer_kgPha(layer, layers,
        plowDepthSettledBulkDensityAtInput_tPm3);
    end
  else
    activeHumusNMineralization_kgPha := 0.0;
  if allowActiveHumusPMineralization then
    begin
    activeHumusPMineralization_kgPha := EQ.HumusPMineralizationForLayer_kgPha(layer, layers,
        activeHumusNMineralization_kgPha);
    end
  else
    activeHumusPMineralization_kgPha := 0.0;
  { fresh n and p mineralization. only done if residue in kg/ha is > 1.0 }
  if (flatCropResidue_kgPha > 1.0) then
    begin
    if allowFreshNMineralization or allowFreshPMineralization then
      begin
      cNRatioInCropResidue := EQ.CNRatioCropResidueForLayer(layer, layers);
      cPRatioInCropResidue := EQ.CPRatioCropResidueForLayer(layer, layers);
      cNPCompositeRatio := EQ.CNPCompositeRatioForLayer(cNRatioInCropResidue, cPRatioInCropResidue);
      decayRateConst := EQ.DecayRateConstForLayer(layer, layers, cNPCompositeRatio);
      end
    else
      decayRateConst := 0.0;
    if allowFreshNMineralization then
      begin
      freshNMineralization_kgPha := EQ.FreshNMineralizationForLayer_kgPha(decayRateConst, organicNFresh_kgPha);
      end
    else
      freshNMineralization_kgPha := 0.0;
    if allowFreshPMineralization then
      begin
      freshPMineralization_kgPha := EQ.FreshPMineralizationForLayer_kgPha(decayRateConst, organicPFresh_kgPha);
      end
    else
      freshPMineralization_kgPha := 0.0;
    end
  else
    begin
    decayRateConst := 0.0;
    freshNMineralization_kgPha := 0.0;
    freshPMineralization_kgPha := 0.0;
    end;
  if allowFreshNMineralization or allowFreshPMineralization then
    begin
    { adjust organic matter for n fresh and humus mineralization }
    movement.organicMatterChangeFromMineralization_tPha := EQ.OrganicMatterChangeFromFreshAndHumusNMineralization_tPha(
        layer, layers, organicNTotalHumus_kgPha);
    addQuantity(organicMatter_tPha, movement.organicMatterChangeFromMineralization_tPha);
    { finally, move materials as a result of fresh and humus n and p mineralization }
    { humus n mineralization }
    subtractQuantity(organicNActiveHumus_kgPha, activeHumusNMineralization_kgPha);
    addQuantity(nitrate_kgPha, activeHumusNMineralization_kgPha);
    { humus p mineralization }
    subtractQuantity(organicPHumus_kgPha, activeHumusPMineralization_kgPha);
    addQuantity(labileP_kgPha, activeHumusPMineralization_kgPha);
    { fresh n mineralization }
    if (freshNMineralization_kgPha <> 0.0) then
      begin
      subtractQuantity(organicNFresh_kgPha, freshNMineralization_kgPha);
      addQuantity(organicNActiveHumus_kgPha, 0.2 * freshNMineralization_kgPha);
      addQuantity(nitrate_kgPha, 0.8 * freshNMineralization_kgPha);
      end;
    { fresh p mineralization }
    if (freshPMineralization_kgPha > 0.0) then
      begin
      subtractQuantity(organicPFresh_kgPha, freshPMineralization_kgPha);
      addQuantity(organicPHumus_kgPha, 0.2 * freshPMineralization_kgPha);
      addQuantity(labileP_kgPha, 0.8 * freshPMineralization_kgPha);
      end;
    end;
  end;
  except on e: Exception do errorMessage('Exception in EP.Mineralization: ' + e.message); end;
  end;

class procedure EP.DailyStandingDeadToFlatResidue(var layers: LayerStructureArray; var mulch: MulchStructure;
  rainfallForDay_mm: single; plantList: TList; var plantMeans: PlantMeansStructure);
    var
      i: integer;
      transferAmount, proportion: single;
      thePlant: GsPlant;
    begin
    try
    transferAmount := 0.0;
    proportion := 0.001 * (1.0 + rainfallForDay_mm / 10.0);
    proportion := max(0.0, min(1.0, proportion));
    { remove standing dead residue from plants and put in layer 1 flat crop residue }
    { also transfer n and p in standing dead to fresh organic n and p }
    { while doing this, re-total the patch's standing dead in plants }
    plantMeans.totalStandingDead_tPha := 0.0;
    plantMeans.totalNInStandingDead_kgPha := 0.0;
    plantMeans.totalPInStandingDead_kgPha := 0.0;
    if (plantList.count > 0) then for i := 0 to plantList.count - 1 do
      begin
      thePlant := GsPlant(plantList.items[i]);
      if thePlant.awaitingReseeding then continue;
      thePlant.transferPropOfStandingDeadToFlatCropResidue(proportion);
      addQuantity(plantMeans.totalStandingDead_tPha, thePlant.biomass.standingDead_tPha);
      addQuantity(plantMeans.totalNInStandingDead_kgPha, thePlant.nutrients.nInStandingDead_kgPha);
      addQuantity(plantMeans.totalPInStandingDead_kgPha, thePlant.nutrients.pInStandingDead_kgPha);
      end;
    { do the same for the mulch layer (this decays faster than plant standing dead) }
    proportion := min(1.0, proportion * 10.0);
    Utils_TransferPropOfMaterial(mulch.flatCropResidue_tPha, layers[0].flatCropResidue_tPha, proportion);
    Utils_TransferPropOfMaterial(mulch.organicNFresh_kgPha, layers[0].organicNFresh_kgPha, proportion);
    Utils_TransferPropOfMaterial(mulch.organicPFresh_kgPha, layers[0].organicPFresh_kgPha, proportion);
  except on e: Exception do errorMessage('Exception in EP.DailyStandingDeadToFlatResidue: ' + e.message); end;
  end;

class procedure EP.Denitrification(layer: integer; var layers: LayerStructureArray);
  begin
  try
  layers[layer].denitrification_kgPha := EQ.DenitrificationForLayer_kgPha(layers[layer].waterContent_mm,
      layers[layer].fieldCapacity_mm, layers[layer].weight_tPha, layers[layer].nitrate_kgPha,
      layers[layer].nutrientCyclingTempFactor, layers[layer].organicMatter_tPha);
  subtractQuantity(layers[layer].nitrate_kgPha, layers[layer].denitrification_kgPha);
  except on e: Exception do errorMessage('Exception in EP.Denitrification: ' + e.message); end;
  end;

class procedure EP.OrganicNAndCLossInSediment(totalErosion_tPha: single; enrichmentRatioForNPPest: single; var layers:
  LayerStructureArray; var movement: MovementStructure;
  takeOrganicNActiveHumus, takeOrganicNStableHumus, takeNitrate, takeAmmonia, takeOrganicNFresh, takeOrganicMatter,
  takeFlatCropResidue: boolean);
  var
    propLost_frn: single;
  begin
  try
  with layers[0] do
  begin
  propLost_frn := EQ.OrganicNOrCOrPProportionLostInSediment_kgPha(totalErosion_tPha, enrichmentRatioForNPPest, weight_tPha);
  { remove materials from organic n and organic matter in first soil layer }
  if takeOrganicNActiveHumus and (organicNActiveHumus_kgPha > 0.0) then
    begin
    movement.organicNActiveHumusAdsorbedToSediment_kgPha := organicNActiveHumus_kgPha * propLost_frn;
    subtractQuantity(organicNActiveHumus_kgPha, movement.organicNActiveHumusAdsorbedToSediment_kgPha);
    end
  else movement.organicNActiveHumusAdsorbedToSediment_kgPha := 0.0;
  if takeOrganicNStableHumus and (organicNStableHumus_kgPha > 0.0) then
    begin
    movement.organicNStableHumusAdsorbedToSediment_kgPha := organicNStableHumus_kgPha * propLost_frn;
    subtractQuantity(organicNStableHumus_kgPha, movement.organicNStableHumusAdsorbedToSediment_kgPha);
    end
  else movement.organicNStableHumusAdsorbedToSediment_kgPha := 0.0;
  if takeOrganicMatter and (organicMatter_tPha > 0.0) then
    begin
    movement.organicMatterAdsorbedToSediment_kgPha := organicMatter_tPha * propLost_frn;
    subtractQuantity(organicMatter_tPha, movement.organicMatterAdsorbedToSediment_kgPha);
    end
  else movement.organicMatterAdsorbedToSediment_kgPha := 0.0;
  { change from EPIC - pdf - removing other n and carbon in proportion to soil loss}
  if takeNitrate and (nitrate_kgPha > 0.0) then
    begin
    movement.nitrateAdsorbedToSediment_kgPha := nitrate_kgPha * propLost_frn;
    subtractQuantity(nitrate_kgPha, movement.nitrateAdsorbedToSediment_kgPha);
    end
  else movement.nitrateAdsorbedToSediment_kgPha := 0.0;
  if takeAmmonia and (ammonia_kgPha > 0.0) then
    begin
    movement.ammoniaAdsorbedToSediment_kgPha := ammonia_kgPha * propLost_frn;
    subtractQuantity(ammonia_kgPha, movement.ammoniaAdsorbedToSediment_kgPha);
    end
  else movement.ammoniaAdsorbedToSediment_kgPha := 0.0;
  if takeOrganicNFresh and (organicNFresh_kgPha > 0.0) then
    begin
    movement.organicNFreshAdsorbedToSediment_kgPha := organicNFresh_kgPha * propLost_frn;
    subtractQuantity(organicNFresh_kgPha, movement.organicNFreshAdsorbedToSediment_kgPha);
    end
  else movement.organicNFreshAdsorbedToSediment_kgPha := 0.0;
  if takeFlatCropResidue and (flatCropResidue_tPha > 0.0) then
    begin
    movement.flatCropResidueAdsorbedToSediment_kgPha := flatCropResidue_tPha * propLost_frn;
    subtractQuantity(flatCropResidue_tPha, movement.flatCropResidueAdsorbedToSediment_kgPha);
    end
  else movement.flatCropResidueAdsorbedToSediment_kgPha := 0.0;
  end;
  except on e: Exception do errorMessage('Exception in EP.OrganicNAndCLossInSediment: ' + e.message); end;
  end;

class procedure EP.PLossInSediment(totalErosion_tPha: single; enrichmentRatioForNPPest: single; var layers:
  LayerStructureArray; var movement: MovementStructure;
  takeOrganicPHumus, takeLabileP, takeMineralPActive, takeMineralPStable, takeOrganicPFresh: boolean);
  var propLost_frn: single;
  begin
  try
  with layers[0] do
  begin
  propLost_frn := EQ.OrganicNOrCOrPProportionLostInSediment_kgPha(totalErosion_tPha, enrichmentRatioForNPPest, weight_tPha);
  { remove materials from organic, mineral and labile p in first soil layer }
  if takeOrganicPHumus then
    begin
    movement.organicPHumusAdsorbedToSediment_kgPha := organicPHumus_kgPha * propLost_frn;
    subtractQuantity(organicPHumus_kgPha, movement.organicPHumusAdsorbedToSediment_kgPha);
    end
  else movement.organicPHumusAdsorbedToSediment_kgPha := 0.0;
  if takeLabileP then
    begin
    movement.labilePAdsorbedToSediment_kgPha := labileP_kgPha * propLost_frn;
    subtractQuantity(labileP_kgPha, movement.labilePAdsorbedToSediment_kgPha);
    end
  else movement.labilePAdsorbedToSediment_kgPha := 0.0;
  if takeMineralPActive then
    begin
    movement.mineralPActiveAdsorbedToSediment_kgPha := mineralPActive_kgPha * propLost_frn;
    subtractQuantity(mineralPActive_kgPha, movement.mineralPActiveAdsorbedToSediment_kgPha);
    end
  else movement.mineralPActiveAdsorbedToSediment_kgPha := 0.0;
  {change from EPIC - pdf - removing other p in proportion to sediment loss}
  if takeMineralPStable then
    begin
    movement.mineralPStableAdsorbedToSediment_kgPha := mineralPStable_kgPha * propLost_frn;
    subtractQuantity(mineralPStable_kgPha, movement.mineralPStableAdsorbedToSediment_kgPha);
    end
  else movement.mineralPStableAdsorbedToSediment_kgPha := 0.0;
  if takeOrganicPFresh then
    begin
    movement.organicPFreshAdsorbedToSediment_kgPha :=  organicPFresh_kgPha * propLost_frn;
    subtractQuantity(organicPFresh_kgPha, movement.organicPFreshAdsorbedToSediment_kgPha);
    end
  else movement.organicPFreshAdsorbedToSediment_kgPha :=  0.0;
  end;
  except on e: Exception do errorMessage('Exception in EP.PLossInSediment: ' + e.message); end;
  end;

class procedure EP.RemoveErodedSoil(soilProxy: GsModel; amountEroded_tPha: single);
  var
    layer: integer;
    propLayer0Remaining_frn, propLayer1Removed_frn: single;
    oldDepth_m: arraySoilLayers;
    thicknessEroded_m, layer1Thickness_m, layer2Thickness_m, lost, proportionTopLayerEroded_frn,
      canBeEroded_m: single;
    soil: GsSoilPatch;
  begin
  try
  soil := GsSoilPatch(soilProxy);
  propLayer0Remaining_frn := 0.0;
  propLayer1Removed_frn := 0.0;
  thicknessEroded_m := 0.0;
  layer1Thickness_m := soil.layers[1].depth_m - soil.layers[0].depth_m;
  layer2Thickness_m := soil.layers[2].depth_m - soil.layers[1].depth_m;
  if soil.state.numLayers > 0 then
    for layer := 0 to soil.state.numLayers - 1 do
      oldDepth_m[layer] := soil.layers[layer].depth_m;
  thicknessEroded_m := Utils_SoilThicknessFromWeightAndBulkDensity_m(amountEroded_tPha,
      soil.layers[0].bulkDensity_tPm3);
  { a max of 1 cm (0.01 m) of depth can be eroded in one day. if there is more }
  { to be eroded on any one day (due to rain), it waits and is eroded slowly. }
  if (thicknessEroded_m + soil.erosion.thicknessWaitingToBeEroded_m < 0.01) then
    begin
    thicknessEroded_m := thicknessEroded_m + soil.erosion.thicknessWaitingToBeEroded_m;
    soil.erosion.thicknessWaitingToBeEroded_m := 0.0;
    end
  else
    begin
    soil.erosion.thicknessWaitingToBeEroded_m := soil.erosion.thicknessWaitingToBeEroded_m
        + (thicknessEroded_m - 0.01);
    thicknessEroded_m := 0.01;
    end;
  { if total soil depth is going to be less than a constant after erosion, don't allow erosion.
    this is for soil types that have shallow soil profiles, so that the total soil depth
    can't get below an amount that will cause numerical exceptions.
    the criterion depth is arbitrarily set at twice the constant depth of the first soil layer (0.01 m) }
  if soil.layers[soil.state.numLayers-1].depth_m - thicknessEroded_m < 0.02 then
    begin
    canBeEroded_m := max(0.0, soil.layers[soil.state.numLayers-1].depth_m - 0.02);
    soil.erosion.thicknessWaitingToBeEroded_m := soil.erosion.thicknessWaitingToBeEroded_m
        + (thicknessEroded_m - canBeEroded_m);
    thicknessEroded_m := canBeEroded_m;
    end;
  { Eroded material is removed from layer 0, but since layer 0 is always 0.01 m deep, }
  { this produces the effect that layer 1 decreases in depth as materials from layer 1 }
  { move into layer 0 to keep the first layer 0.01 m deep. At some point }
  { layer 1 may become so depleted that it is less than 0.01 m deep. when that happens }
  { what is left in layer 1 is placed in layer 2, layer 1 is cleared out, and a new layer }
  { is created by splitting the highest layer with a thickness of 0.15 m or more. }
  { This operation creates a new layer and moves layer 2 up to become the new layer 1 }
  { (unless layer 2 was split, in which case half of it becomes the new layer 1). }
  if (layer1Thickness_m - thicknessEroded_m <= 0.01) then
    begin
    Utils_MovePortionOfLayerToAnotherLayer(soil.layers[1], soil.layers[2], layer1Thickness_m,
        layer2Thickness_m, 1.0, safediv(layer2Thickness_m, layer1Thickness_m + layer2Thickness_m),
        soil.params.watershedSlopeSteepness_mPm, false);
    { now generate a new layer to replace the doomed layer 1 by splitting the layer }
    { nearest the surface with a thickness > 0.15 m in half (NOTE: we changed this to always split) }
    { if the thickest layer is layer 2, half of layer 2 becomes the new layer 1 }
    { if the thickest layer is not layer 2, layer 2 is moved into layer 1 }
    { and a new layer is created further down }
    EP.CreateNewLayerDuringErosionBySplittingThickest(soil);
    end;
  { because water constants (and water content) are stored as an absolute amount
    (mm, not m/m), they must be adjusted when the thickness of the layer changes.
    soil weight and bulk density are recalculated inside of Utils_MovePortionOfLayerToAnotherLayer }
  { change from EPIC: porosity will be recalculated in Utils_MovePortionOfLayerToAnotherLayer in a different way
    than these (not adding, just recalculating), so it is not necessary to do anything with it now }
  proportionTopLayerEroded_frn := thicknessEroded_m / 0.01;
  Utils_TransferPropOfMaterial(soil.layers[0].waterContent_mm, lost, proportionTopLayerEroded_frn);
  Utils_TransferPropOfMaterial(soil.layers[0].wiltingPoint_mm, lost, proportionTopLayerEroded_frn);
  Utils_TransferPropOfMaterial(soil.layers[0].fieldCapacity_mm, lost, proportionTopLayerEroded_frn);
  { change from EPIC: if the splitting of layers and merging of layer 1 with layer 2 STILL does not
    produce enough material in layer 1 to satisfy erosion, defer some of the eroded material until
    tomorrow, when another layer splitting can occur. the 90% cutoff is arbitrary. }
  if thicknessEroded_m > layer1Thickness_m * 0.9 then
    begin
    soil.erosion.thicknessWaitingToBeEroded_m := soil.erosion.thicknessWaitingToBeEroded_m
        + (thicknessEroded_m - layer1Thickness_m * 0.9);
    thicknessEroded_m := layer1Thickness_m * 0.9;
    end;
  { Shift materials from layer 1 to layer 0 to make up for soil eroded from layer 0. }
  { Note that (soil.layers[0].depth_m - thicknessRemoved_m) is never -ve since a max of 0.01 m }
  { can be removed at a time and the depth of layer 0 is always exactly 0.01 m. }
  propLayer0Remaining_frn := safediv(max(0.0, 0.01 - thicknessEroded_m), 0.01);
  propLayer1Removed_frn := safediv(thicknessEroded_m, layer1Thickness_m);
  Utils_MovePortionOfLayerToAnotherLayer(soil.layers[1], soil.layers[0], layer1Thickness_m, 0.01,
      propLayer1Removed_frn, propLayer0Remaining_frn, soil.params.watershedSlopeSteepness_mPm, true);
  { remove eroded depth from all layers except the first (which is always 0.01 m) }
  if soil.state.numLayers > 1 then for layer := 1 to soil.state.numLayers - 1 do
    soil.layers[layer].depth_m := soil.layers[layer].depth_m - thicknessEroded_m;
  { adjust other depths to new lowest layer depth (which may be less than before) }
  soil.updateSoilProfileDepthAndOtherDepths;
  { change from EPIC }
  soil.AdjustPlantRootWeightsInLayersAfterMovingSoil(oldDepth_m);
  except on e: Exception do errorMessage('Exception in EP.RemoveErodedSoil: ' + e.message); end;
  end;

{this function has been changed to always split the largest layer}
class procedure EP.CreateNewLayerDuringErosionBySplittingThickest(soilProxy: GsModel);
  var
    layer, layerToSplit: integer;
    atLeastOneLayerIsThickerThanP15m: boolean;
    maxThickness_m, thicknessThisLayer_m: single;
    soil: GsSoilPatch;
  begin
  try
  soil := GsSoilPatch(soilProxy);
  layerToSplit := 2;
  atLeastOneLayerIsThickerThanP15m := false;
  maxThickness_m := 0.0;
  thicknessThisLayer_m := 0.0;
  { split thickest layer (moving down)  }
  if soil.state.numLayers > 2 then for layer := 2 to soil.state.numLayers - 1 do
    begin
    thicknessThisLayer_m := soil.layers[layer].depth_m - soil.layers[layer-1].depth_m;
    if (thicknessThisLayer_m > maxThickness_m) then
      begin
      maxThickness_m := thicknessThisLayer_m;
      layerToSplit := layer;
      end;
    end;
  {move layers up until layerToSplit-1}
  if layerToSplit > 2 then for layer := 2 to layerToSplit - 1 do
    soil.layers[layer-1] := soil.layers[layer];
  { take half of what is in thickest layer and put it in new layer }
  Utils_ZeroAllLayerFields(soil.layers[layerToSplit-1]);
  Utils_MovePortionOfLayerToEmptyLayer(soil.layers[layerToSplit], soil.layers[layerToSplit-1], 0.5,
      (soil.layers[layerToSplit].depth_m + soil.layers[layerToSplit-2].depth_m) / 2.0,
      soil.layers[layerToSplit-2].depth_m{above});
  { recalculate porosity for split layer (new layer porosity is calc in Utils_MovePortionOfLayerToEmptyLayer }
  soil.layers[layerToSplit].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
    soil.layers[layerToSplit].bulkDensity_tPm3, soil.layers[layerToSplit].rockContent_pct,
    soil.layers[layerToSplit].depth_m - soil.layers[layerToSplit-1].depth_m);
  Utils_CheckRelationOfFieldCapacityToPorosity(soil.layers[layerToSplit]);
  except on e: Exception do errorMessage('Exception in EP.CreateNewLayerDuringErosionBySplittingThickest: ' + e.message); end;
  end;

(* EPIC FUNCTION goes something like this:
class procedure EP.CreateNewLayerDuringErosionBySplittingThickest(soilProxy: GsModel);
    var
      layer: integer;
      layerToSplit: integer;
      atLeastOneLayerIsThickerThanP15m: boolean;
      maxThickness_m: single;
      thicknessThisLayer_m: single;
      soil: GsSoilPatch;
    begin
    soil := GsSoilPatch(soilProxy);
    layerToSplit := 1;
    atLeastOneLayerIsThickerThanP15m := false;
    maxThickness_m := 0.0;
    thicknessThisLayer_m := 0.0;
    { split first layer (moving down) with a thickness over 0.15 m }
    { If no layer has a thickness over 0.15 m, don't split any layer but decrease the number of layers. }
    if soil.state.numLayers > 2 then for layer := 2 to soil.state.numLayers - 1 do
      begin
      thicknessThisLayer_m := soil.layers[layer].depth_m - soil.layers[layer-1].depth_m;
      if (thicknessThisLayer_m < 0.15) then 
        begin
        if (thicknessThisLayer_m > maxThickness_m + 0.01) then
          begin
          maxThickness_m := thicknessThisLayer_m;
          layerToSplit := layer;
          end;
        end
      else 
        begin
        { thicknessThisLayer_m >= 0.15 }
        layerToSplit := layer;
        maxThickness_m := thicknessThisLayer_m;
        atLeastOneLayerIsThickerThanP15m := true;
        break;
        end;
      end;
    if (atLeastOneLayerIsThickerThanP15m) and
    		(maxThickness_m > soil.soilParams.firstLayerThisThickSplitAtInputIfNeeded_m) then
      begin
      {move layers up until layerToSplit-1}
      if layerToSplit > 2 then for layer := 2 to layerToSplit - 1 do
        soil.layers[layer-1] := soil.layers[layer];
      Utils_ZeroAllLayerFields(soil.layers[layerToSplit - 1]);
      soil.MovePortionOfLayerToEmptyLayer(soil.layers[layerToSplit], soil.layers[layerToSplit - 1], 0.5,
        (soil.layers[layerToSplit].depth_m + soil.layers[layerToSplit - 2].depth_m) / 2.0, true);
      end
    else if soil.state.numLayers > 3 then
      {eliminates second layer - loses its contents}
      begin
      for layer := 1 to soil.state.numLayers - 2 do
        soil.layers[layer] := soil.layers[layer + 1];
      dec(soil.state.numLayers);
      {clear out the no longer used last layer}
      Utils_ZeroAllLayerFields(soil.layers[soil.state.numLayers]);
      end;
    end;
*)

class procedure EP.CreateNewLayerAtInputBySplittingThickest(soilProxy: GsModel;
    firstLayerThisThickSplitAtInputIfNeeded_m: single);
  var
    layer, layerToSplit: integer;
    maxThickness_m, thicknessThisLayer_m: single;
    layers: LayerStructureArray;
    soil: GsSoilPatch;
  begin
  try
    soil := GsSoilPatch(soilProxy);
    layerToSplit := 1;
    maxThickness_m := 0.0;
    thicknessThisLayer_m := 0.0;
    layers := soil.layers;
    { split first layer (moving down) with a thickness over the threshold. }
    { If no layer has a thickness over the threshold, split the thickest layer. }
    if soil.state.numLayers > 1 then for layer := 1 to soil.state.numLayers - 1 do
      begin
      thicknessThisLayer_m := soil.layers[layer].depth_m - soil.layers[layer-1].depth_m;
      if (thicknessThisLayer_m < firstLayerThisThickSplitAtInputIfNeeded_m) then
        begin
        if (thicknessThisLayer_m > maxThickness_m) then 
          begin
          maxThickness_m := thicknessThisLayer_m;
          layerToSplit := layer;
          end;
        end
      else 
        begin
        { thicknessThisLayer_m >= firstLayerThisThickSplitAtInputIfNeeded_m }
        layerToSplit := layer;
        break;
        end;
      end;
    { create new layer at bottom }
    inc(soil.state.numLayers);
    { pull all layers down one, not including thickest layer }
    if layerToSplit + 1 <= soil.state.numLayers - 2 then
    	for layer := soil.state.numLayers - 2 downto layerToSplit + 1 do
      	soil.layers[layer + 1] := soil.layers[layer];
    Utils_ZeroAllLayerFields(soil.layers[layerToSplit+1]);
    { take half of what is in thickest layer and put it in new layer }
    Utils_MovePortionOfLayerToEmptyLayer(soil.layers[layerToSplit], soil.layers[layerToSplit+1], 0.5,
        soil.layers[layerToSplit].depth_m{new},  soil.layers[layerToSplit-1].depth_m{above});
    soil.layers[layerToSplit].depth_m :=
    	(soil.layers[layerToSplit+1].depth_m + soil.layers[layerToSplit-1].depth_m) / 2.0;
    { recalculate porosity for split layer (new layer porosity is calc in Utils_MovePortionOfLayerToEmptyLayer }
    soil.layers[layerToSplit].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
        soil.layers[layerToSplit].bulkDensity_tPm3, soil.layers[layerToSplit].rockContent_pct,
        soil.layers[layerToSplit].depth_m - soil.layers[layerToSplit-1].depth_m);
    Utils_CheckRelationOfFieldCapacityToPorosity(soil.layers[layerToSplit]);
  except on e: Exception do errorMessage('Exception in EP.CreateNewLayerAtInputBySplittingThickest: ' + e.message); end;
  end;

class function EP.AutoIrrigationIsNeeded(waterStressFactorForAutoIrr: single; waterStressFactor: single; numLayers:
  integer; var layers: LayerStructureArray; swmWPInSoilProfileDepth_mm: single; fcmWPInSoilProfileDepth_mm: single):
  boolean;
    var
      { waterStressFactorForAutoIrr can be one of four things. }
      { 0:   no auto irrigation }
      { 0-1: plant water stress factor for auto irrigation }
      { > 1: soil water tension for auto irrigation }
      { < 0: plant available water deficit in root zone for auto irrigation }
      { note that in help system we only mention the plant water stress option (it is easiest to understand }
      soilWaterTension: single;
      layer: integer;
    begin
    result := false;
    try
    { if factor is zero (or near enough) no auto irrigation is done }
    if (abs(waterStressFactorForAutoIrr) < 0.00001) then 
      begin
      result := false;
      exit;
      end;
    if (waterStressFactorForAutoIrr > 0.0) then 
      begin
      if (waterStressFactorForAutoIrr < 1.0) then 
        begin
        { compare plant water stress to factor }
        if (waterStressFactor <= waterStressFactorForAutoIrr) then 
          result := true
        else
          result := false;
        end
      else 
        begin
        { compare soil water tension to factor }
        layer := 1;
        while (layer <= numLayers - 1) and (layers[layer].depth_m < 0.15) do inc(layer);
        soilWaterTension := power(10.0, 3.1761 - 1.6576 * safediv(log10(layers[layer].waterContent_mm) -
          log10(layers[layer].wiltingPoint_mm), log10(layers[layer].fieldCapacity_mm) - log10(layers[layer].wiltingPoint_mm)));
        if (soilWaterTension >= waterStressFactorForAutoIrr) then 
          result := true
        else
        	result := false;
        end;
      end
    else 
      begin
      { compare plant available water deficit [= (sm-wp) - (fc-wp)] to factor }
      { note here that since the factor is negative, the test is whether the deficit is greater }
      { (on the negative scale) than the factor }
      if (swmWPInSoilProfileDepth_mm - fcmWPInSoilProfileDepth_mm <= waterStressFactorForAutoIrr) then 
        result := true
      else
      	result := false;
      end;
  except on e: Exception do errorMessage('Exception in EP.AutoIrrigationIsNeeded: ' + e.message); end;
  end;

class function EP.RainfallForDay_mm(month: integer; itRainedYesterday: boolean;
    var weatherParams: WeatherParamsStructure; var stationVars: WeatherStationVarsStructure;
    var yesterdaysUniformRandNumForRainfall_frn: single): single;
  var
    randomNumber, probWetDayAfterYesterday, rainfall_mm, normalRandomNumber, uniformRandNumForRainfall_frn: single;
  begin
  try
  { decide if it rained today }
  randomNumber := 1.0 - Utils_RandomZeroToOne;
  if (itRainedYesterday) then 
    probWetDayAfterYesterday := weatherParams.probWetDayAfterWetDayForMonth_frn[month]
  else
    probWetDayAfterYesterday := weatherParams.probWetDayAfterDryDayForMonth_frn[month];
  if (randomNumber > probWetDayAfterYesterday) then
    begin
    result := 0.0;
    exit;
    end;
  { if it did rain, calculate amount of rainfall }
  uniformRandNumForRainfall_frn := Utils_RandomZeroToOne;
  if (weatherParams.stdDevDailyRainfallForMonth_mm[month] > 0.0) then
    begin
    normalRandomNumber := Utils_StandardNormalDeviate(yesterdaysUniformRandNumForRainfall_frn,
      uniformRandNumForRainfall_frn);
    yesterdaysUniformRandNumForRainfall_frn := uniformRandNumForRainfall_frn;
    rainfall_mm := EQ.DailyRainfallBySkewedNormal_mm(normalRandomNumber,
      weatherParams.skewCoeffForRainfallForMonth[month], weatherParams.stdDevDailyRainfallForMonth_mm[month],
      stationVars.dailyMeanRainfallForMonth_mm[month]);
    rainfall_mm := rainfall_mm * stationVars.rainfallNormalizingFactorForMonth[month];
    end
  else 
    rainfall_mm := EQ.DailyRainfallByModifiedExponential_mm(weatherParams.coeffRainfallModExpDist,
      stationVars.dailyMeanRainfallForMonth_mm[month], uniformRandNumForRainfall_frn);
  result := rainfall_mm;
  except on e: Exception do result := errorMessage('Exception in EP.RainfallForDay_mm: ' + e.message); end;
  end;

class procedure EP.MinAndMaxTemperatureForDay_degC(month: integer; var weatherParams: WeatherParamsStructure; var
  dailyWeather: DailyWeatherStructure; var matrices: MatricesStructure; meanMaxTempForDay_degC: single);
  begin
  try
  dailyWeather.maxTempForDay_degC := EQ.DailyMaxTemperature_degC(meanMaxTempForDay_degC,
    weatherParams.stdDevMaxTempForMonth_degC[month], matrices.correlationMultiplier[0]);
  dailyWeather.minTempForDay_degC := EQ.DailyMinTemperature_degC(weatherParams.dailyMeanMinTempForMonth_degC[month],
    weatherParams.stdDevMinTempForMonth_degC[month], matrices.correlationMultiplier[1]);
  if (dailyWeather.minTempForDay_degC > dailyWeather.maxTempForDay_degC) then dailyWeather.minTempForDay_degC :=
    dailyWeather.maxTempForDay_degC - 0.2 * abs(dailyWeather.maxTempForDay_degC);
  except on e: Exception do errorMessage('Exception in EP.MinAndMaxTemperatureForDay_degC: ' + e.message); end;
  end;

class function EP.RadiationForDay_MJPm2(maxPossibleRadiation_MJPm2: single; meanRadiationForDay_MJPm2: single;
  radiationCorrelationMultiplier: single): single;
  var
    stdDevRadiationForDay_MJPm2: single;
  begin
  try
  stdDevRadiationForDay_MJPm2 := EQ.StdDevRadiationFromMeanAndExtremeForDay_MJPm2(maxPossibleRadiation_MJPm2,
    meanRadiationForDay_MJPm2);
  result := EQ.DailyRadiation_MJPm2(meanRadiationForDay_MJPm2, radiationCorrelationMultiplier,
      stdDevRadiationForDay_MJPm2, maxPossibleRadiation_MJPm2);
  except on e: Exception do result := errorMessage('Exception in EP.RadiationForDay_MJPm2: ' + e.message); end;
  end;

class function EP.RelativeHumidityForDay_frn(meanRelHumForDay_frn: single): single;
  var
    upperLimit: single;
    lowerLimit: single;
  begin
  try
  upperLimit := EQ.RelHumTriangularDistUpperLimit_frn(meanRelHumForDay_frn);
  lowerLimit := EQ.RelHumTriangularDistLowerLimit_frn(meanRelHumForDay_frn);
  result := Utils_TriangularDistribution(lowerLimit, meanRelHumForDay_frn, upperLimit);
  result := result * (safediv(meanRelHumForDay_frn, (upperLimit + meanRelHumForDay_frn + lowerLimit) / 3.0));
  if (result >= 1.0) then result := 0.99;
  except on e: Exception do result := errorMessage('Exception in EP.RelativeHumidityForDay_frn: ' + e.message); end;
  end;

class function EP.MaxPossibleRadiation_MJPm2(julianDay: integer;
    stationLatitude_rad: single; declinationAngleOfSun_rad: single):
  single;
  var
    maxPossibleRadiationVariable: single;
  begin
  try
  maxPossibleRadiationVariable := EQH.MaxPossibleRadiationVariable(stationLatitude_rad, declinationAngleOfSun_rad);
  result := EQH.MaxPossibleRadiation_MJPm2(julianDay, maxPossibleRadiationVariable, stationLatitude_rad,
      declinationAngleOfSun_rad);
  except on e: Exception do result := errorMessage('Exception in EP.MaxPossibleRadiation_MJPm2: ' + e.message); end;
  end;

class function EP.EnrichmentRatioForNPPest(var water: WaterStructure; var erosion: ErosionStructure): single;
  var
    totalIrrigation_mm, combinedInflow_mm, combinedRunoff_mm,
      combinedPeakRainfallRate_mmPhr, combinedPeakRunoffRate_mmPhr, combinedWaterErosion_tPha: single;
    peakRainfallRateForIrrigation_mmPhr: single;
    peakRainfallExcessRate_mmPhr, sedimentDeliveryRatio, expForEnrichmentRatio,
      coeffForEnrichmentRatio, sedimentConc_gPm3: single;
  begin
  try
  if (water.runoffVolume_mm <= 1.0) then
    result := 1.0
  else
    begin
    totalIrrigation_mm := water.userIrrigationForDay_mm + water.autoIrrigationForDay_mm {yesterday's};
    { to calculate peak rainfall rate for irrigation, use the most distributed rainfall (gentlest),
      with an alpha(0.5) of 0.5/24 or 0.0208 (see eqn 136) }
    peakRainfallRateForIrrigation_mmPhr := EQ.PeakRainfallRate_mmPhr(totalIrrigation_mm, 0.0208);
    { add inflow, runoff, and erosion; use maximum of peak rainfall rate and peak runoff rate }
    combinedInflow_mm := water.patchRainfallForDay_mm + totalIrrigation_mm;
    combinedPeakRainfallRate_mmPhr := max(water.peakRainfallRate_mmPhr, peakRainfallRateForIrrigation_mmPhr);
    combinedRunoff_mm := water.runoffVolume_mm + water.irrigationRunoffVolume_mm;
    combinedPeakRunoffRate_mmPhr := max(water.peakRunoffRate_mmPhr, water.irrigationPeakRunoffRate_m3Psec);
    combinedWaterErosion_tPha := erosion.waterErosion_tPha + erosion.irrigationWaterErosion_tPha;
    peakRainfallExcessRate_mmPhr := EQ.PeakRainfallExcessRate_mmPhr(combinedPeakRainfallRate_mmPhr, combinedRunoff_mm,
        combinedInflow_mm);
    sedimentDeliveryRatio := EQ.SedimentDeliveryRatio(combinedPeakRunoffRate_mmPhr, peakRainfallExcessRate_mmPhr);
    expForEnrichmentRatio := EQ.ExpForEnrichmentRatio(sedimentDeliveryRatio);
    coeffForEnrichmentRatio := EQ.CoeffForEnrichmentRatio(expForEnrichmentRatio);
    sedimentConc_gPm3 := EQ.SedimentConc_gPm3(combinedWaterErosion_tPha, combinedRunoff_mm);
    result := EQ.EnrichmentRatioForNPPest(sedimentConc_gPm3, coeffForEnrichmentRatio, expForEnrichmentRatio);
    end;
  except on e: Exception do result := errorMessage('Exception in EP.EnrichmentRatioForNPPest: ' + e.message); end;
  end;

end.
(* not using
  class procedure PesticideTransportAndDegradation(numLayers: integer; var layers: LayerStructureArray; pesticideList:
    TList; leafAreaIndex: single; totalErosion_tPha: single; waterCapableOfWashingPesticideOffPlants_mm: single;
    enrichmentRatioForNPPest: single; var water: WaterStructure);
class procedure EP.PesticideTransportAndDegradation(numLayers: integer; var layers: LayerStructureArray; pesticideList:
  TList; leafAreaIndex: single; totalErosion_tPha: single; waterCapableOfWashingPesticideOffPlants_mm: single;
  enrichmentRatioForNPPest: single; var water: WaterStructure);
    var
      layer, i: integer;
      volumeMovingThroughLayer_mm: single;
      amountLost_kgPha: single;
      pesticideConc_gPm3: single;
      pesticideLeachedByPercolation_kgPha: single;
      p: GsSoilPesticide;
    begin
    volumeMovingThroughLayer_mm := 0.0;
    amountLost_kgPha := 0.0;
    pesticideConc_gPm3 := 0.0;
    pesticideLeachedByPercolation_kgPha := 0.0;
    { do whole transport and degradation for each pesticide in soil }
    if (pesticideList.count > 0) then for i := 0 to pesticideList.count - 1 do
        begin
        p := GsSoilPesticide(pesticideList.items[i]);
        { if pesticide is on plants, wash off and degrade }
        if ((leafAreaIndex > 0.0) and (p.amountOnFoliage_kgPha >= 0.1)) then 
          begin
          { pesticide washing off plants }
          if (waterCapableOfWashingPesticideOffPlants_mm > 2.54) then 
            begin
            amountLost_kgPha := EQ.PesticideWashedOffPlants_kgPha(p.washOffFraction_frn, p.amountOnFoliage_kgPha);
            p.amountOnFoliage_kgPha := p.amountOnFoliage_kgPha - amountLost_kgPha;
            p.amountInLayer_kgPha[0] := p.amountInLayer_kgPha[0] + amountLost_kgPha;
            end;
          { pesticide degrading on foliage }
          p.amountOnFoliage_kgPha := EQ.FoliarPesticideAdjForDecay(p.amountOnFoliage_kgPha, p.halfLifeOnFoliage_days);
          end;
        { compute pesticide loss from top soil layer in runoff, lateral flow, & percolation }
        if (p.amountInLayer_kgPha[0] >= 0.1) then
          begin
          { pesticide leaching from percolation in surface layer }
          volumeMovingThroughLayer_mm := layers[0].percolation_mm;
          if (volumeMovingThroughLayer_mm > 0.0) then
            begin
            pesticideLeachedByPercolation_kgPha := EQ.PesticideLeachedForLayer_kgPha(1, layers,
              volumeMovingThroughLayer_mm, p);
            p.amountInLayer_kgPha[0] := p.amountInLayer_kgPha[0] - pesticideLeachedByPercolation_kgPha;
            end;
          { pesticide leaching from runoff and lateral flow together for surface layer }
          volumeMovingThroughLayer_mm := (water.runoffVolume_mm + water.irrigationRunoffVolume_mm)
              + layers[0].lateralFlow_mm;
          if (volumeMovingThroughLayer_mm > 0.0) then 
            begin
            amountLost_kgPha := EQ.PesticideLeachedByRunoffAndLatFlow_kgPha(layers, p.amountInLayer_kgPha[0],
              volumeMovingThroughLayer_mm, p.organicCAdsorptionCoeff);
            p.amountInLayer_kgPha[0] := p.amountInLayer_kgPha[0] - amountLost_kgPha;
            end;
          { not bothering to split this up into that caused by runoff and lat flow }
          { may want to do that later, for reporting }
          { pesticide loss in sediment }
          if (volumeMovingThroughLayer_mm > 0.0) then 
            begin
            pesticideConc_gPm3 := EQ.PesticideConcInSoil_gPm3(p.organicCAdsorptionCoeff, p.amountInLayer_kgPha[0],
              layers);
            amountLost_kgPha := EQ.PesticideLostInSediment_kgPha(totalErosion_tPha, enrichmentRatioForNPPest,
              pesticideConc_gPm3, layers[0].weight_tPha);
            p.amountInLayer_kgPha[0] := p.amountInLayer_kgPha[0] - amountLost_kgPha;
            end;
          {  pesticide degradation in top soil layer }
          p.amountInLayer_kgPha[0] := EQ.GroundPesticideAdjForDecay(p.amountInLayer_kgPha[0], p.halfLifeInSoil_days);
          end;
        { leaching through other layers by percolation, lat flow, and degradation }
        if numLayers > 1 then for layer := 1 to numLayers - 1 do
          begin
          { add percolate from upper layer }
          p.amountInLayer_kgPha[layer] := p.amountInLayer_kgPha[layer] + (pesticideLeachedByPercolation_kgPha);
          pesticideLeachedByPercolation_kgPha := 0.0;
          { pesticide leached by percolation and lateral flow }
          { change from EPIC - EPIC had >= 0.1 here - we want to handle smaller amounts }
          if (p.amountInLayer_kgPha[layer] >= 0.0) then
            begin
            try
            volumeMovingThroughLayer_mm := layers[layer].percolation_mm + layers[layer].lateralFlow_mm;
            if (volumeMovingThroughLayer_mm > 0.0) then 
              begin
              amountLost_kgPha := EQ.PesticideLeachedForLayer_kgPha(layer, layers, volumeMovingThroughLayer_mm, p);
              p.amountInLayer_kgPha[layer] := p.amountInLayer_kgPha[layer] - (amountLost_kgPha);
              { figure amount coming from percolation so it can move to the next layer }
              { amount leached from lateral flow disappears }
              pesticideConc_gPm3 := min(safediv(amountLost_kgPha, volumeMovingThroughLayer_mm), p.solubility_gPm3);
              pesticideLeachedByPercolation_kgPha := pesticideConc_gPm3 * layers[layer].percolation_mm;
              end;
            { pesticide degradation in this layer }
            p.amountInLayer_kgPha[layer] := EQ.GroundPesticideAdjForDecay(p.amountInLayer_kgPha[layer],
              p.halfLifeInSoil_days);
            except
              p.amountInLayer_kgPha[layer] := 0.0;
            end;
            end;
          end;
      end;
    end;
*)

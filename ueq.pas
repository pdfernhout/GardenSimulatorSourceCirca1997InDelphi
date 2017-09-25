{ $ L-}
unit ueq;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ueq: Bottom-level EPIC functions (EQ = EPIC eQuations). Hydrology-related bottom-level EPIC
functions are found in ueqh. Into this code we have typed the entire text of a document
by JR Williams which he gave us in 1994. We call this document "the chapter" here.
The equation numbers here correspond to that chapter and are referred to in the text.
Some low-level functions that were not mentioned in the chapter are included without
equation numbers. Under the text preceding each function is a comparison of the
function as denoted in the chapter and the function as we found it in the EPIC FORTRAN code.
Just before each function is a short list of important variables in each function. We have not
tried especially hard to keep these lists of variables up to date, so some might be wrong.
Some extremely complex functions we split into two.
Please note that we started translating with EPIC3090, which was put out in 1993,
so there may be significant differences between our translation and the current EPIC code. There
may also be significant errors in translation we have not yet found.
Many of our changes to the EPIC code are noted here, but some are not.
The asterisks everywhere make it slightly easier to find specific topics in this large file
(for example, searching for '*crop' will find the crop growth section).
Note: we have left a lot of pesticide-related code here (commented out) for review. We chose not to use
EPIC's pesticide-movement code because we did not want to present such a simplistic pesticide model
in a teaching simulation. An effective pesticide movement model would include the effect of pesticides
on soil fauna, organic matter and nutrients and would model the breakdown of pesticides into
secondary compounds, which might themselves have significant impacts on the soil and groundwater.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses ueStruct, SysUtils;

type EQ = class
  public
  {                              WEATHER - *PRECIPITATION* }
  { EQN: 95 }
  class function ProbWetDayFromNumWetDays_frn(numWetDaysForMonth: single; numDaysInMonth: integer): single;
  { EQN: 96 }
  class function ProbWetDayAfterDryDayFromProbWetDay_frn(probWetDay_frn: single;
    coeffForWetDryProbsGivenNumWetDays_frn: single): single;
  { EQN: 97 }
  class function ProbWetDayAfterWetDayFromProbWetDay_frn(probWetDay_frn: single;
    coeffForWetDryProbsGivenNumWetDays_frn: single): single;
  { EQN: 98 }
  class function DailyRainfallBySkewedNormal_mm(stdNormDeviateForRainfall: single;
    skewCoeffForRainfallForMonth: single; stdDevDailyRainfallForMonth_mm: single; dailyMeanRainfallForMonth_mm:
    single): single;
  { EQN: 99 }
  class function DailyRainfallByModifiedExponential_mm(coeffRainfallModExpDist: single; dailyMeanRainfallForMonth_mm:
    single; uniformRandomNumber: single): single;
  { no equation }
  class function IsSnowing(maxTempForDay_degC: single; soilSurfaceTempWithCover_degC: single): integer;
  {              WEATHER - *AIR TEMPERATURE* AND SOLAR *RADIATION* }
  { EQN: 100 }
  class function StdDevMaxTempFromMeanAndExtremeForMonth_degC(extremeDailyMaxTempForMonth_degC: single;
    dailyMeanMaxTempForMonth_degC: single): single;
  class function StdDevMinTempFromMeanAndExtremeForMonth_degC(extremeDailyMinTempForMonth_degC: single;
    dailyMeanMinTempForMonth_degC: single): single;
  { EQN: 101 }
  class function StdDevRadiationFromMeanAndExtremeForDay_MJPm2(maxPossibleRadiation_MJPm2: single;
    meanRadiationForDay_MJPm2: single): single;
  { EQN: 102 }
  class function DailyMeanMaxTempWetDaysForMonth_degC(dailyMeanMaxTempDryDaysForMonth_degC: single;
    dailyMeanMaxTempForMonth_degC: single; dailyMeanMinTempForMonth_degC: single): single;
  { EQN: 104 }
  class function DailyMeanMaxTempDryDaysForMonth_degC(probWetDayForMonth_frn: single;
    dailyMeanMaxTempForMonth_degC: single; dailyMeanMinTempForMonth_degC: single): single;
  { EQN: 105 }
  class function DailyMeanRadiationWetDaysForMonth_MJPm2(dailyMeanRadiationDryDaysForMonth_MJPm2: single): single;
  { EQN: 106 }
  class function DailyMeanRadiationDryDaysForMonth_MJPm2(dailyMeanRadiationForMonth_MJPm2: single; probWetDayForMonth_frn:
    single): single;
  { not in chapter }
  class function DailyMaxTemperature_degC(meanMaxTempForDay_degC: single; stdDevMaxTempForMonth: single;
    maxTempCorrelationMultiplier: single): single;
  { not in chapter }
  class function DailyMinTemperature_degC(dailyMeanMinTempForMonth_degC: single; stdDevMinTempForMonth: single;
    minTempCorrelationMultiplier: single): single;
  { not in chapter }
  class function DailyRadiation_MJPm2(meanRadiationForDay_MJPm2: single; radiationCorrelationMultiplier: single;
    stdDevRadiationForDay_MJPm2: single; maxPossibleRadiation_MJPm2: single): single;
  {                                 WEATHER - *WIND* }
  { EQN: 107 }
  class function WindSpeedForFractionOfDay_mPsec(fractionOfDay: single; windSpeedForFractionOfDayParam1: single;
    windSpeedForFractionOfDayParam2: single): single;
  { not in chapter }
  class function WindSpeedForFractionOfDayParam2(dailyMeanWindSpeedForMonth_mPsec: single; meanWindSpeedForDay_mPsec:
    single): single;
  { EQN: 108 }
  class function WindSpeedForFractionOfDayParam1(windSpeedForFractionOfDayParam2: single): single;
  { EQN: 110 }
  class function FractionOfDayToStartSimulatingWindSpeed_frn(fractionOfDayWindSpeedAtIntervalStart_frn: single;
    windSpeedForFractionOfDayParam2: single): single;
  { EQN: 111 }
  class function MeanWindSpeedForDay_mPsec(dailyMeanWindSpeedForMonth_mPsec: single; paramForModifiedExpWindSpeed:
    single): single;
  { from code }
  class function WindDirectionFromNorth_rad(var windDirsForMonth_frn: windDirectionsArray; month: integer): single;
  {                           WEATHER - *RELATIVE HUMIDITY* }
  { EQN: 112 }
  class function DailyMeanRelHumWetDaysForMonth_frn(dailyMeanRelHumDryDaysForMonth_frn: single): single;
  { EQN: 113 }
  class function DailyMeanRelHumDryDaysForMonth_frn(dailyMeanRelHumForMonth_frn: single; probWetDayForMonth_frn: single):
    single;
  { EQN: 114 }
  class function RelHumTriangularDistUpperLimit_frn(meanRelHumForDay_frn: single): single;
  { EQN: 115 }
  class function RelHumTriangularDistLowerLimit_frn(meanRelHumForDay_frn: single): single;
  { EQN: 117 }
  class function DailyMeanRelHumForMonthFromTemp_frn(dailyMeanMaxTempForMonth_degC: single; dailyMeanMinTempForMonth_degC:
    single): single;
  { from code }
  class function RelHumForMonthFromDewPoint_frn(dewPoint_degC: single; meanTempForMonth_degC: single): single;
  { were in plant section }
  { EQN: 317 }
  class function DayLength_hr(stationLatitude_rad: single; sunDeclinationAngle_rad: single): single;
  { from code }
  class function MinDayLengthForYear_hr(stationLatitude_rad: single): single;
  class function HourAngleFromCosHourAngle(cosHourAngle: single): single;
  { EQN: 318 }
  class function SunDeclinationAngle_rad(julianDay: integer): single;

  {                                   *SOIL TEMPERATURE* }
  { EQN: 240 }
  class function SoilTempAtCenterForLayer_degC(yesterdaysSoilTempAtCenter_degC: single; soilSurfaceCoverLagFactor_frn:
    single; soilTempDepthWtFactor: single; meanMonthlyMeanMeanTempForYear_degC: single; soilSurfaceTempWithCover_degC:
    single): single;
  { EQN: 241 EQN: 242 }
  class function SoilTempDepthWtFactorForLayer(depth_m: single; depthUpperLayer_m: single; dampingDepthForSoilTemp_m:
    single): single;
  { EQN: 243 }
  class function MaxDampingDepthForSoilTemp_m(patchMeanBulkDensity_tPm3: single): single;
  { EQN: 244 }
  class function ScalingParamForDampingDepthForSoilTemp(patchTotalSoilWaterContent_mm: single; patchMeanBulkDensity_tPm3:
    single; depthOfLowestLayer_m: single): single;
  { EQN: 245 }
  class function DampingDepthForSoilTemp_m(maxDampingDepthForSoilTemp_m: single; scalingParam: single): single;
  { EQN: 248 }
  class function BareSoilSurfaceTempWetDay_degC(meanTempForDay_degC: single; minTempForDay_degC: single;
    probWetDayForMonth_frn: single): single;
  { EQN: 249 }
  class function BareSoilSurfaceTempDryDay_degC(maxTempForDay_degC: single; meanTempForDay_degC: single;
    bareSoilSurfaceTempRadiationFactor: single; probWetDayForMonth_frn: single): single;
  { EQN: 250 }
  class function BareSoilSurfaceTempRadiationFactor(radiationForDay_MJPm2: single; albedo_frn: single;
    soilCoverIndex_frn: single): single;
  { EQN: 251 }
  class function SurfaceLayerSoilTemp_degC(yesterdaysSoilTempInSecondLayer_degC: single; soilSurfaceCoverLagFactor_frn:
    single; soilSurfaceTempBare_degC: single): single;
  { EQN: 252 }
  class function SoilSurfaceTempCoverLagFactor_frn(aboveGroundBiomassAndResidue_tPha: single; snowWaterContent_mm:
    single; var soilInsulationFromAirTempCoeffs: SCurveStructure): single;
  {                                      *TILLAGE* }
  { EQN: 324 }
  class function BulkDensityAfterTillageForLayer_tPm3(bulkDensity_tPm3: single; bulkDensitySettled_tPm3: single;
    mixingEfficiency_frn: single): single;
  { EQN: 325 EQN: 326 }
  class procedure BulkDensityAfterSettlingFromRain_tPm3(numLayers: integer; var layers: LayerStructureArray;
    rainfallForDay_mm: single; runoffVolume_mm: single; maxTillageDepth_m: single; var soilSettlingFromRainfallCoeffs:
      sCurveStructure);
  { EQN: 327 }
  class function StandingCropResidueReductionFromTillageMultiplier(tillageDepth_m: single;
    mixingEfficiencyForOperation_frn: single): single;
  { EQN: 328 EQN: 329 }
  class function RidgeHeightAfterTillage_m(ridgeHeightForOperation_mm: single; ridgeHeightLastOperation_mm: single;
    tillageDepthForOperation_m: single; tillageDepthLastOperation_m: single): single;
  class function RidgeIntervalAfterTillage_m(ridgeIntervalForOperation_m: single; ridgeIntervalLastOperation_m:
    single; ridgeHeightForOperation_mm: single; ridgeHeightLastOperation_mm: single): single;
  { EQN: 330 EQN: 331 }
  class function AluminumSaturation_pct(baseSaturation_pct: single; organicC_pct: single; soilpH: single): single;
  class function BaseSaturation_pct(baseFormingCations_cmolPkg: single; cationExchangeCapacity_cmolPkg: single): single;
  { EQN: 332 }
  class function LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha(changeInBaseSaturationToOffsetAlSat_pct: single;
    totalSoilWeightInMaxTillageDepth_tPha: single): single;
  class function ChangeInBaseSaturationToOffsetAlSat_pct(aluminumSaturation_pct: single;
    effectiveCEC_cmolpplusPkg: single): single;
  { EQN: 333 }
  class function ChangeInBaseSaturationByNAdded_frn(nFertilizerAdded_kgPha: single; nFixation_kgPha: single;
    totalSoilWeightInMaxTillageDepth_kgPha: single): single;
  { EQN: 334 }
  class function pHReductionFromNAdded(changeInBaseSaturationByNAdded_pct: single; cationExchangeCapacity_cmolPkg: single):
    single;
  { EQN: 335 }
  class function LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha(totalSoilWeightInMaxTillageDepth_kgPha: single;
    meanWeightedCEC_cmolPkg: single; changeInBaseSaturationToRaisePHTo6p5_pct: single): single;
  { EQN: 336 }
  class function ChangeInBaseSaturationToRaisePHTo6p5_pct(soilpH: single; baseSaturation_pct: single): single;
  class function SulfurFor5PHForNonHighlyWeatheredSoil_kgPha(totalSoilWeightInMaxTillageDepth_kgPha: single;
    meanWeightedCEC_cmolPkg: single; changeInBaseSaturationToLowerPHTo5_pct: single): single;
  class function ChangeInBaseSaturationToLowerPHTo5_pct(soilpH: single; baseSaturation_pct: single): single;
  class function NewpHForLimeAdded(limeAdded_kgPha: single; effectiveCationExchangeCapacity_cmolPkg: single;
    totalSoilWeight_tPha: single; currentpH: single): single;
  class function NewpHForSulfurAdded(sulfurAdded_kgPha: single; effectiveCationExchangeCapacity_cmolPkg: single;
    totalSoilWeight_tPha: single; currentpH: single): single;
  {  Pests }
  { EQN: 337 EQN: 338 }
  class function PestPopulationIndex(pestPopulationIndex: single; minTempForDay_degC: single;
    sumRainfallMinusRunoffPrev30Days_mm: single; thresholdThirtyDayRainfallForPests_mm: single;
    aboveGroundBiomassAndResidue_tPha: single; thresholdBiomassAndResidueForPests_tPha: single;
    healthOfBeneficialsIndex: single): single;
  { EQN: 339 }
  class function PestFactorReductionFromPesticide(killFractionForPesticide_frn: single): single;
  { EQN: 340 }
  class function PestFactor_frn(pestPopulationIndex: single; minPestWeedDiseaseFactor_frn: single; maxPestPopulationIndex:
    single; biomassGrowthConstraint_frn: single; var pestDamageCoeffs: SCurveStructure): single;
  { Furrow Diking }
  { EQN: 341 }
  class function RainSettlingMultiplier(rainfallForDay_mm: single; var layers: LayerStructureArray): single;
  { EQN: 352 EQN: 353 }
  class function DikeVolume_mm(var surface: SurfaceStructure; var params: SoilParamsStructure): single;
  {                            *CROP GROWTH* }
  { EQN: 253 }
  class function HeatUnitAccumulationForDay(meanTempForDay_degC, baseTemp_degC, optTemp_degC: single): single;
  { EQN: 254 }
  class function HeatUnitIndex(cumHeatUnits: single; heatUnitsAtMaturation: single): single;
  { EQN: 255 }
  class function PhotoActiveRadiation_MJPm2(radiationForDay_MJPm2: single; leafAreaIndex: single): single;
  { EQN: 258 EQN: 257 EQN: 256 }
  class function PotentialIncreaseInBiomass_tPha(photoActiveRadiation_MJPm2: single; carbonDioxideInAtmosphere_ppm:
    single; var biomassToEnergyRatioCO2Params: sCurveStructure; biomassToEnergyRatioVPDParam: single; vaporPressureDeficit_kPa:
      single): single;
  { EQN: 259 EQN: 260 }
  class function LeafAreaIndexBeforeLeafDecline(var development: DevelopmentStructure; maxLeafAreaIndex: single;
   var constraints: ConstraintsStructure): single;
  { EQN: 261 }
  class function HeatUnitFactorForLAIAndHeight(heatUnitIndex: single; var heatUnitFactorParamsForLAI: sCurveStructure): single;
  { EQN: 262 }
  class function LeafAreaIndexDuringLeafDecline(var development: DevelopmentStructure; var plantParams: PlantParamsStructure;
    var constraints: ConstraintsStructure): single;
  { EQN: 263 }
  class function PlantHeight_m(height_m: single; maxHeight_m: single; heatUnitFactorForLAIAndHeight: single): single;
  { EQN: 264 }
  class function ChangeInTotalRootWeight_tPha(newTotalPlantBiomass_tPha: single; fractionRootWtAtEmergence_frn: single;
    fractionRootWtAtMaturity_frn: single; heatUnitIndex: single; totalRootWeight_tPha: single): single;
  { EQN: 265 }
  class function ChangeInRootWeightForLayer_tPha(changeInTotalRootWeight_tPha: single; plantWaterUseForLayer_mm: single;
    totalPlantWaterUse_mm: single; rootWeightForLayer_tPha: single; totalRootWeight_tPha: single): single;
  { EQN: 266 }
  class function RootDepth_m(rootDepth_m: single; maxRootDepth_m: single; heatUnitIndex: single; lowestSoilLayerDepth_m:
    single; cropHeight_m: single): single;
  { EQN: 273 (next two functions) }
  class function WaterDemandTermForLayer_mm(potPlantEvap_mm: single; soilProfileDepth_m: single;
    maxOfDepthThisLayerOrRootDepth_m: single; rootDepth_m: single): single;
  class function UnconstrainedPlantWaterDemandForLayer_mm(waterUseTermForLayer_mm: single; deficitCompensFactor_frn:
    single; waterUseAboveThisLayer_mm: single; waterUseTermLastLayer_mm: single): single;
  { EQN: 274 EQN: 275 }
  class function WaterDemandConstraintForLayer_mm(layer: integer; var layers: LayerStructureArray; lowerWaterLimit_mm:
    single): single;
  { EQN: 312 }
  class function PlantWaterDemandForLayer_mm(layer: integer; var layers: LayerStructureArray; unconstrainedWaterDemand_mm:
    single; waterDemandConstraint: single; lowerWaterLimit_mm: single; rootGrowthConstraint_frn: single): single;
  { EQN: 276 }
  class function PlantNitrogenDemandForOptimalGrowth_kgPha(plantOptimalNConc_kgPkg: single; totalPlantBiomass_tPha: single;
    nInLiveBiomass_kgPha: single; optimalNConcParams2: single; potentialIncreaseInBiomass_tPha: single): single;
  { EQN: 277 }
  class function PlantoptimalNConc_kgPkg(var plantParams: PlantParamsStructure; heatUnitIndex: single): single;
  { EQN: 278 }
  class function NSupplyForLayer_kgPha(plantWaterUse_mm: single; nitrate_kgPha: single; waterContent_mm: single): single;
  { EQN: 280 }
  class function NDemandByLayer_kgPha(var nutrients: NutrientsStructure; var biomass: BiomassStructure;
    var layers: LayerStructureArray): single;
  { EQN: 281 }
  class function NFixation_kgPha(nFixationFraction_frn: single; nDemand_kgPha: single): single;
  class function NFixationFraction_frn(nFixationFractionWaterFactor: single; nFixationFractionNitrateFactor:
    single; nFixationFractionGrowthStageFactor: single): single;
  { EQN: 283 EQN: 284 EQN: 285 EQN: 286 }
  class function NFixationFractionGrowthStageFactor(heatUnitIndex: single): single;
  { EQN: 287 }
  class function NFixationFractionWaterFactor(top30cmFractionFieldCapacity_frn: single): single;
  { EQN: 288 EQN: 289 EQN: 290 }
  class function NFixationFractionNitrateFactor(totalNitrateInRootZone_kgPha: single; rootDepth_m: single): single;
  { EQN: 291 }
  class function PlantPhosphorusDemandForOptimalGrowth_kgPha(optimalPConc_kgPkg: single; totalPlantBiomass_tPha: single;
    pInLiveBiomass_kgPha: single; potentialIncreaseInBiomass_tPha: single): single;
  { EQN: 292 }
  class function PlantOptimalPConc_kgPkg(var plantParams: PlantParamsStructure; heatUnitIndex: single): single;
  { EQN: 293 }
  class function PSupplyForLayer_kgPha(layer: integer; var nutrients: NutrientsStructure; var biomass: BiomassStructure;
    var layers: LayerStructureArray; labilePUptakeFactor_frn: single): single;
  class function PDemandByLayer_kgPha(var nutrients: NutrientsStructure; var biomass: BiomassStructure;
    var layers: LayerStructureArray): single;
  { EQN: 294 }
  class function LabilePUptakeFactorForLayer_frn(labilePConc_gPt: single; var pUptakeCoeffs: sCurveStructure): single;
  { EQN: 295 }
  class function PlantIncreaseInBiomass_tPha(potentialIncreaseInBiomass_tPha: single; biomassGrowthConstraint_frn: single;
    biomassAdjustmentIfLAIDeclining: single; dayLengthIsAboveThresholdForGrowth: boolean): single;
  { EQN: 296 }
  class function WaterStressFactor_frn(totalPlantWaterUse_mm: single; potPlantEvap_mm: single): single;
  class function BiomassTempStressFactor_frn(meanTempForDay_degC: single; baseTemp_degC: single; optimalTemp_degC:
    single): single;
  { EQN: 298 }
  class function NOrPStressFactorScalingFactor(totalUptake_kgPha: single; optimalConc_kgPkg: single;
    totalPlantBiomassInclRoot_tPha: single): single;
  { EQN: 299 }
  class function NOrPStressFactor_frn(scalingFactor: single; var plantNAndPStressCoeffs: SCurveStructure): single;
  { EQN: 300 EQN: 301 }
  class function AerationStressFactor_frn(top1MWaterContent_mm: single; top1mPorosity_mm: single; criticalAerationFactor:
    single; var rootGrowthRestrictionByAerationStress: SCurveStructure): single;
  class function BiomassGrowthConstraintFactor_frn(var constraints: ConstraintsStructure): single;
  { EQN: 297 }
  class function RootTempStressFactorForLayer_frn(soilTemp_degC: single; optimalTemp_degC: single): single;
  { EQN: 302 }
  class function SoilStrengthFactor_frn(bulkDensityAdjForWaterContentForLayer_tPm3: single; var soilTextureParams:
    arrayTwo; rockContent_pct: single; var rootGrowthRestrictionByRockCoeffs: SCurveStructure): single;
  { EQN: 303 }
  class function BulkDensityForLowStress_tPm3(soilSandContent_pct: single; rootGrowthRestrictionInSandySoilParam:
    single): single;
  { EQN: 304 }
  class function BulkDensityForHighStress_tPm3(soilSandContent_pct: single; rootGrowthRestrictionInSandySoilParam:
    single): single;
  { EQN: 305 }
  class function SoilTextureParam2(bulkDensityForLowStress_tPm3: single; bulkDensityForHighStress_tPm3: single): single;
  { EQN: 306 }
  class function SoilTextureParam1(bulkDensityForLowStress_tPm3: single; soilTextureParam2: single): single;
  { EQN: 307 }
  class function BulkDensityAdjForWaterContentForLayer_tPm3(layer: integer; var layers: LayerStructureArray): single;
  { EQN: 308 EQN: 309 }
  class function AluminumToxicityStressFactorForLayer_frn(aluminumSaturation_pct: single; aluminumTolerance_pct:
    single): single;
  { EQN: 310 }
  class function MaxAlSaturationForCrop_pct(alToxicitySensitivityForCrop_1to5: integer): single;
  class function RootGrowthConstraintFactorForLayer_frn(rootTempStressFactorForLayer_frn: single;
    rootSoilStrengthStressFactorForLayer_frn: single; rootAlToxicityStressFactorForLayer_frn: single): single;
  { EQN: 311 }
  class function PlantWaterDeficitCompensFactorForLayer_frn(aLayer: integer; var constraints: ConstraintsStructure): single;
  { not in chapter }
  class function LeavesFallingInFall(standingLiveBiomass_tPha: single; treePlantingToMaturity_yr: single; month: integer):
    single;
  { not in chapter }
  class function BiomassLossIfOldAndDry(waterStressFactor_frn: single; heatUnitIndex: single; standingLiveBiomass_tPha:
    single): single;
  { EQN: 313 }
  class function HarvestIndex_frn(unconstrainedHarvestIndex_frn: single; waterUseRatio: single; minHarvestIndex_frn: single;
    var harvestIndexPlantWaterUseCoeffs: SCurveStructure): single;
  { EQN: 314 }
  class function UnconstrainedHarvestIndex_frn(potentialHarvestIndex_frn: single; heatUnitIndex: single; var
    potentialHarvestIndexCoeffs: SCurveStructure): single;
  { EQN: 315 }
  class function WaterUseRatioForHarvestIndex(cumWaterUse_mm: single; cumPotPlantEvap_mm: single): single;
  { EQN: 316 }
  class function WinterBiomassDayLengthReductionFactor(dayLength_hr: single; minDayLengthForWinterGrowth_hr: single): single;
  { EQN: 319 }
  class function WinterBiomassFrostReductionFactor(minTempForDay_degC: single; var frostReductionFactorParams:
    SCurveStructure): single;
  { EQN: 320 }
  class function WinterReductionInBiomass(totalBiomassInclRoot_tPha: single; heatUnitIndex: single;
    winterBiomassFrostReductionFactor: single): single;
  { EQN: 321 EQN: 322 }
  class function AnnualCropIsKilledByFrost(snowWaterContent_mm: single; var constraints: ConstraintsStructure;
    meanTempForDay_degC: single; absoluteTempForFrostKill_degC: single): boolean;
  {                              NUTRIENTS - *NITROGEN* }
  { EQN: 161 }
  class function NitrateLeachedFromLayer_kgPha(nitrate_kgPha: single; volumeMovingThroughLayer_mm: single;
    nitrogenLeachingParam: single; porosity_mm: single): single;
  { EQN: 162 }
  class function MeanNitrateConcInLayerDuringWaterFlow_gPm3(nitrateLostFromLayer_mm: single; volumeMovingThroughLayer_mm:
    single): single;
  { EQN: 163 }
  class function NitrateMovedToFirstLayerFromSoilEvap_kgPha(var layers: LayerStructureArray;
    lowestLayerInWhichSoilEvapOccured: integer): single;
  { EQN: 164 }
  class function OrganicNOrCOrPProportionLostInSediment_kgPha(totalErosion_tPha: single; enrichmentRatioForNPPest: single;
    surfaceLayerSoilWeight_tPha: single): single;
  { EQN: 165 }
  class function SedimentDeliveryRatio(peakRunoffRate_mmPhr: single; peakRainfallExcessRate_mmPhr: single): single;
  { EQN: 166 }
  class function PeakRainfallExcessRate_mmPhr(peakRainfallRate_mmPhr: single; runoffVolume_mm: single; rainfallForDay_mm:
    single): single;
  { no equation, from code }
  class function SedimentConc_gPm3(waterErosion_tPha: single; runoffVolume_mm: single): single;
  { EQN: 167 }
  class function EnrichmentRatioForNPPest(sedimentConc_gPm3: single; coeffForEnrichmentRatio: single; expForEnrichmentRatio:
    single): single;
  { EQN: 168 }
  class function ExpForEnrichmentRatio(sedimentDeliveryRatio: single): single;
  { EQN: 169 }
  class function CoeffForEnrichmentRatio(expForEnrichmentRatio: single): single;
  { EQN: 170 }
  class function DenitrificationForLayer_kgPha(waterContent_mm: single; fieldCapacity_mm: single; weight_tPha: single;
    nitrate_kgPha: single; nutrientCyclingTempFactor: single; organicMatter_tPha: single): single;
  { EQN: 171 }
  class function NutrientCyclingTempFactorForLayer(temperature_degC: single): single;
  { EQN: 172 }
  class function SoilWaterOverFieldCapacityForLayer(waterContent_mm: single; fieldCapacity_mm: single): single;
  { EQN: 173 }
  class function FreshNMineralizationForLayer_kgPha(decayRateConst: single; organicNFresh_kgPha: single): single;
  { EQN: 174 }
  class function DecayRateConstForLayer(layer: integer; var layers: LayerStructureArray; cNPCompositeRatio: single): single;
  { EQN: 175 }
  class function CNPCompositeRatioForLayer(cNRatioCropResidue: single; cPRatioCropResidue: single): single;
  { EQN: 176 }
  class function CNRatioCropResidueForLayer(layer: integer; var layers: LayerStructureArray): single;
  { EQN: 177 }
  class function CPRatioCropResidueForLayer(layer: integer; var layers: LayerStructureArray): single;
  class function OrganicMatterChangeFromFreshAndHumusNMineralization_tPha(layer: integer; var layers: LayerStructureArray;
    organicNTotalHumus_kgPha: single): single;
  { EQN: 178 }
  class function OrganicNActiveHumusForLayer_gPt(organicNActiveHumusFraction_frn: single; organicNTotalHumus_gPt: single):
    single;
  { EQN: 179 }
  class function OrganicNActiveHumusFractionForLayer_frn(layer: integer; cultivationBeforeSimulationStarts_yr: single):
    single;
  { EQN: 180 }
  class function OrganicNFromActiveToStableInHumusForLayer_kgPha(organicNActiveHumus_kgPha: single;
    organicNStableHumus_kgPha: single; organicNActiveHumusFractionAtInput_frn: single): single;
  { EQN: 181 }
  class function ActiveHumusNMineralizationForLayer_kgPha(layer: integer; var layers: LayerStructureArray;
    plowDepthSettledBulkDensityAtInput_tPm3: single): single;
  { EQN: 183 }     { can find no implementation for this function! or call! }
  {class function AdjustedNOrPDecayRateConstForLayer(labilePart_kgPha: single; flatCropResidue_tPha: single;
    concInCropResidue_gPg: single): single;}
  { EQN: 184 }
  class function FlatCropResidueDecay_tPha(decayRateConst: single; flatCropResidue_tPha: single): single;
  { EQN: 186 }
  class function NitrifRegulatorForLayer(nitrifTempFactor: single; nitrifSoilWaterFactor: single; nitrifPHFactor: single):
    single;
  { EQN: 187 }
  class function NitrifAndVolatilTempFactorForLayer(temperature_degC: single): single;
  { EQN: 188 EQN: 189 EQN: 190 }
  class function NitrifSoilWaterFactorForLayer(layer: integer; var layers: LayerStructureArray): single;
  { EQN: 191 EQN: 192 EQN: 193 }
  class function NitrifPHFactorForLayer(soilpH: single): single;
  { EQN: 194 }
  class function VolatilRegulatorForSurfaceLayer(nitrifAndVolatilTempFactor: single; volatilWindSpeedFactorForSurface:
    single): single;
  { EQN: 195 }
  class function VolatilWindSpeedFactorForSurface(meanWindSpeedForDay_mPsec: single): single;
  { EQN: 196 }
  class function VolatilRegulatorForNonSurfaceLayer(nitrifAndVolatilTempFactor: single; volatilCationExchangeCapFactor:
    single; volatilDepthFactor: single): single;
  { EQN: 197 }
  class function VolatilCationExchangeCapFactorForLayer(cationExchangeCapacity_cmolPkg: single): single;
  { EQN: 198 }
  class function VolatilDepthFactorForLayer(depthToMiddleOfSoilLayer_mm: single; var nVolatilizationByDepthCoeffs:
    SCurveStructure): single;
  { EQN: 199 }
  class function DepthToMiddleOfSoilLayer_mm(layer: integer; var layers: LayerStructureArray): single;
  { EQN: 200 }
  class function UnadjNitrifForLayer_kgPha(nitrifRegulator: single): single;
  { EQN: 202 }
  class function UnadjVolatilForLayer_kgPha(volatilRegulator: single): single;
  { EQN: 185 }
  class function CombinedNitrifAndVolatilForLayer_kgPha(ammonia_kgPha: single; nitrifRegulator: single; volatilRegulator:
    single): single;
  { EQN: 201 }
  class function NitrificationForLayer_kgPha(combinedNitrifAndVolatil_kgPha: single; unadjNitrif_kgPha: single;
    unadjVolatil_kgPha: single): single;
  { EQN: 203 }
  class function VolatilizationForLayer_kgPha(combinedNitrifAndVolatil_kgPha: single; nitrification_kgPha: single): single;
  {                              NUTRIENTS - *PHOSPHORUS* }
  { EQN: 204 }
  class function MeanPConcDuringLeaching_gPm3(labileP_kgPha: single; volumeEnteringLayer_mm: single; weightForLayer_tPha:
    single): single;
  { EQN: 206 }
  class function FreshPMineralizationForLayer_kgPha(decayRateConst: single; organicPFresh_kgPha: single): single;
  { EQN: 207 }
  class function HumusPMineralizationForLayer_kgPha(layer: integer; var layers: LayerStructureArray;
    activeHumusNMineralization_kgPha: single): single;
  { EQN: 211 }
  class function PFlowFromLabileToActiveMineral_kgPha(labileP_kgPha: single; mineralPActive_kgPha: single;
    pSorptionCoeff_frn: single): single;
  { EQN: 212 EQN: 213 EQN: 214 EQN: 215 }
  class function PSorptionCoeffForLayer_frn(calciumCarbonate_pct: single; labilePConc_gPt: single; baseSaturation_pct:
    single; soilpH: single; clayContent_pct: single; soilType: integer; pSorptionCoeff: single): single;
  { EQN: 216 }
  class function MineralPFlowFromActiveToStableForLayer_kgPha(mineralPFlowCoeff_Pday: single; mineralPActive_kgPha:
    single; mineralPStable_kgPha: single): single;
  { EQN: 217 EQN: 218 }
  class function MineralPFlowCoeffForLayer_Pday(pSorptionCoeff_frn: single; calciumCarbonate_pct: single; soilType:
    integer): single;
  {                                          *PESTICIDE* FATE }
  (* not using this code
  { EQN: 219 }
  class function PesticideEffectiveApplication_kgPha(pesticideApplication_kgPha: single; efficiency_frn: single): single;
  { EQN: 220 }
  { from code }
  class function FractionOfGroundCoveredByPlants_frn(leafAreaIndex: single): single;
  { EQN: 221 }
  class function PesticideAppliedToPlants_kgPha(fractionOfGroundCoveredByPlants_frn: single;
    pesticideEffectiveApplication_kgPha: single): single;
  { EQN: 223 }
  class function PesticideWashedOffPlants_kgPha(washOffFractionForPesticide_frn: single; pesticideOnPlants_kgPha: single):
    single;
  { EQN: 226 }
  class function GroundPesticideAdjForDecay(amountInLayer_kgPha: single; groundHalfLifeForPesticide_days: single): single;
  { EQN: 227 }
  class function FoliarPesticideAdjForDecay(amountOnFoliage_kgPha: single; foliarHalfLifeForPesticide_days: single): single;
  { EQN: 235 EQN: 236 }
  class function PesticideLeachedForLayer_kgPha(layer: integer; var layers: LayerStructureArray; volumeMovingThroughLayer_mm:
    single; pesticide: GsSoilPesticide): single;
  { EQN: 237 }
  class function PesticideLeachedByRunoffAndLatFlow_kgPha(var layers: LayerStructureArray; pesticideInLayer_kgPha: single;
    volumeMovingThroughLayer_mm: single; partitioningCoeffForPesticide_m3Pt: single): single;
  { EQN: 238 }
  class function PesticideLostInSediment_kgPha(totalErosion_tPha: single; enrichmentRatioForNPPest: single;
    pesticideConcInSoil_gPm3: single; surfaceLayerSoilWeight_tPha: single): single;
  { EQN: 239 }
  class function PesticideConcInSoil_gPm3(partitioningCoeffForPesticide_m3Pt: single; pesticideInLayer_kgPha: single;
    var layers: LayerStructureArray): single;
  *)
  {                      EROSION - *WATER EROSION FROM RAINFALL/RUNOFF* }
  { EQN: 118 }
  class function WaterErosion_tPha(energyComponent: single; soilErodibilityFactor: single; cropManagementFactor: single;
    erosionControlPracticeFactor: single; slopeLengthAndSteepnessFactor: single; coarseFragmentFactor: single): single;
  { EQN: 119 }
  class function WaterErosionEnergyComponentByMUSLE(runoffVolume_mm: single; peakRunoffRate_mmPhr: single;
    watershedArea_ha: single): single;
  class function WaterErosionEnergyComponentByMUST(runoffVolume_mm: single; peakRunoffRate_mmPhr: single): single;
  class function WaterErosionEnergyComponentByMUSS(runoffVolume_mm: single; peakRunoffRate_mmPhr: single;
    watershedArea_ha: single): single;
  class function WaterErosionEnergyComponentByMUSI(runoffVolume_mm: single; peakRunoffRate_mmPhr: single; watershedArea_ha:
    single; var userCoeffsForMUSI: arrayFour): single;
  { EQN: 128 }
  class function RainfallEnergyFactorForUSLE(rainfallWithoutSnowmeltForDay_mm: single; peakRainfallRate_mmPhr: single;
    propRainInFirstHalfHourModBySnow: single): single;
  class function WaterErosionEnergyComponentByUSLE(rainfallEnergyFactorForUSLE: single): single;
  { EQN: 132 }
  class function PeakRainfallRate_mmPhr(rainfallMinusSnowmelt_mm: single; propRainInFirstHalfHourModBySnow: single):
    single;
  { EQN: 133 }
  class function FrequencyOfRainfallEvents_Pyr(yearsRecordMaxHalfHourRain: single): single;
  { EQN: 135 }
  class function MeanPropRainInFirstHalfHourForMonth_frn(frequencyOfRainfallEvents_Pyr: single;
    meanMaxHalfHourRainfallAmountForMonth_mm: single; meanTotalRainfallForMonth_mm: single; numWetDaysForMonth: single):
    single;
  { EQN: 136 }
  class function MaxPropTotalRainFallsInFirstHalfHourForMonth_frn(rainfallWithoutSnowmeltForDay_mm: single): single;
  class function WaterErosionEnergyComponentByOnstadFoster(runoffVolume_mm: single; peakRunoffRate_mmPhr: single;
    rainfallEnergyFactorForUSLE: single): single;
  { EQN: 120 }
  class function SlopeLengthAndSteepnessFactor(slopeLength_m: single; slopeLengthFactorParam: single; slopeSteepness_mPm:
    single): single;
  { EQN: 121 }
  class function SlopeLengthFactorParam(slopeSteepness_mPm: single): single;
  { EQN: 122 }
  class function CropManagementFactor(patchMeanMinCropManagementFactor: single;
    patchTotalAboveGroundBiomassAndResidue_tPha: single): single;
  { EQN: 123 }
  class function SoilErodibilityFactor(soilSandContent_pct: single; siltContent_pct: single; clayContent_pct: single;
    organicMatter_tPha: single; soilWeight_tPha: single): single;
  { EQN: 137 }
  class function CoarseFragmentFactor(rockContentSurfaceSoilLayer_pct: single): single;
  {                      EROSION - *WATER EROSION FROM IRRIGATION* }
  { EQN: 138 }
  class function WaterErosionFromIrrigation_tPha(furrowVolume_mm: single; peakRunoffRateForFurrow_m3Psec: single;
    soilErodibilityFactor: single; erosionControlPracticeFactor: single; slopeLengthAndSteepnessFactor: single; distance_m:
    single): single;
  class function IrrigationRunoffVolume_mm(irrigationVolume_mm: single; irrigationRunoffRatio: single): single;
  class function IrrigationPeakRunoffRate_m3Psec(var params: SoilParamsStructure;
    var surface: SurfaceStructure; irrigationRunoffVolume_mm: single; var furrowVolume_mm: single;
    var distance_m: single): single;
  {                             EROSION - *WIND EROSION* }
  { EQN: 139 }
  class function WindErosion_tPha(soilErodibilityFactorWE: single; surfaceRoughnessFactorWE: single;
    vegetativeCoverFactorWE: single; coarseFragmentFactorWE: single; integratedWindErosion_tPha: single;
    maxWindErosionPerDay_tPha: single): single;
  class function CoarseFragmentFactorWE(surfaceLayerRockContent_pct: single): single;
  { EQN: 141 }
  class function WindErosionRateForFractionOfDay_kgPmsec(frictionVelocityWE_mPsec: single;
    weForFractionOfDayFactor: single; meanUnsheltDistanceFactorWE: single): single;
  { not in chapter }
  class function WindErosionForFractionOfDayFactor(thresholdFrictionVelocityWE_mPsec: single;
    topLayerSoilWaterOverWiltingPoint: single; windErosionAccumulatedFactor: single): single;
  { EQN: 143 }
  class function FrictionVelocityWE_mPsec(windSpeedForFractionOfDay_mPsec: single): single;
  { EQN: 145 }
  class function ThresholdFrictionVelocityWE_mPsec(soilParticleDiameter_microns: single): single;
  { EQN: 146 }
  class function SoilErodibilityFactorWE(sand_pct: single; clay_pct: single; silt_pct: single; caco3_pct: single): single;
  { EQN: 147 }
  class function SurfaceRoughnessFactorWE(coeffSurfaceRoughnessFactorWE: single; expSurfaceRoughnessFactorWE: single):
    single;
  { EQN: 148 }
  class function ExpSurfaceRoughnessFactorWE(ridgeHeight_mm: single): single;
  { EQN: 149 }
  class function CoeffSurfaceRoughnessFactorWE(clodRoughnessFactorWE: single; ridgeRoughnessFactorWE: single): single;
  { EQN: 150 }
  class function RidgeRoughnessFactorWE(angleOfWindRelativeToRidges_rad: single; ridgeHeight_mm: single): single;
  { EQN: 151 }
  class function ClodRoughnessFactorWE(randomRoughnessWE_mm: single): single;
  { EQN: 152 }
  class function VegCoverEquivFactorWE(standingLiveBiomass_tPha: single; standingDeadResidue_tPha: single;
    surfaceLayerFlatCropResidue_tPha: single; windErosionFactorStandingLive: single; windErosionFactorStandingDead:
      single; windErosionFactorFlatResidue: single): single;
  { EQN: 153 }
  class function VegetativeCoverFactorWE_frn(vegCoverEquivFactorWE: single): single;
  { EQN: 154 }
  class function UnsheltFieldLengthAlongPrevailingWindDir_m(fieldLength_m: single; fieldWidth_m: single;
    angleOfWindRelativeToRidges_rad: single): single;
  class function AngleOfWindRelativeToRidges_rad(windDirectionFromNorth_rad: single;
    fieldLengthOrientationFromNorth_rad: single): single;
  { EQN: 155 }
  class function MeanUnsheltDistanceFactorWE(unsheltFieldLengthAlongPrevailingWindDir_m: single): single;
  end;

implementation

uses ueutils, ueqh, uunits, udate;

{
                                    WEATHER

The weather variables necessary for driving the EPIC model are precipitation,
air temperature, and solar radiation. If the Penman methods are used to
estimate potential evaporation, wind speed and relative humidity are also
required. Of course, wind speed is also needed when wind-induced erosion
is simulated. If daily precipitation, air temperature, and solar radiation
data are available, they can be input directly into EPIC. Rainfall and
temperature data are available for many areas of the United States, but
solar radiation, relative humidity, and wind data are scarce. Even rainfall
and temperature data are generally not adequate for the long-term EPIC
simulation (100 years+). Thus, EPIC provides options for simulating various
combinations of the five weather variables. Descriptions of the models
used for simulating precipitation, temperature, radiation, relative
humidity and wind follow.

                              WEATHER - *PRECIPITATION*

The EPIC precipitation model developed by Nicks (1974) is a first-order
Markov-chain model. Thus, input for the model must include monthly
probabilities of receiving precipitation. On any given day, the input
must include information as to whether the previous day was dry or wet.
A random number (0-1) is generated and compared with the appropriate
wet-dry probability. If the random number is less than or equal to
the wet-dry probability, precipitation occurs on that day. Random
numbers greater than the wet-dry probability give no precipitation.
Since the wet-dry state of the first day is established, the process
can be repeated for the next day and so on throughout the simulation period.

If wet-dry probabilities are not available, the average monthly
number of rainy days may be substituted. The probability of a wet day is
calculated directly from the number of wet days: [EQN: 95] where PW
is the probability of a wet day, NWD is the number of rainy days, and
ND is the number of days, in a month.
Equation:
  PW = NWD / ND
Code:
  same
Variables:
  PW = ProbWetDayFromNumWetDays_frn
  NWD = numWetDaysForMonth
  ND = numDaysInMonth
 }
{ EQN: 95 }
class function EQ.ProbWetDayFromNumWetDays_frn(numWetDaysForMonth: single; numDaysInMonth: integer): single;
  begin
  try
  result := safediv(numWetDaysForMonth, numDaysInMonth);
  except on e: Exception do result := errorMessage('Exception in EQ.ProbWetDayFromNumWetDays_frn: ' + e.message); end;
  end;

{ 
The probability of a wet day after a dry day can be estimated as a fraction
of PW [EQN: 96] where P(W/D) is the probability of a wet day following a dry
day and where beta is a fraction usually in the range of 0.6 to 0.9.
Equation:
  P(W/D) = beta * PW
Code:
  same
Variables:
  P(W/D) = EQ::ProbWetDayAfterDryDayFromProbWetDay_frn
  beta = kProbWetDayGivenDryDayCoeff_frn
  PW = probWetDayFromNumWetDays_frn
  beta = coeffForWetDryProbsGivenNumWetDays_frn
 }
{ EQN: 96 }
class function EQ.ProbWetDayAfterDryDayFromProbWetDay_frn(probWetDay_frn: single; coeffForWetDryProbsGivenNumWetDays_frn:
  single): single;
  begin
  try
  result := coeffForWetDryProbsGivenNumWetDays_frn * probWetDay_frn;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ProbWetDayAfterDryDayFromProbWetDay_frn: ' + e.message); end;
  end;

{ 
The probability of a wet day following a wet day can be calculated directly
using the equation [EQN: 97] where P(W/W) is the probability of a wet day
after a wet day.
Equation:
  P(W/W) = 1.0 - beta + P(W/D)
Code:
  same
Variables:
  P(W/W) = EQ::ProbWetDayAfterDryDayFromProbWetDay_frn
  beta =
  P(W/D) = probWetDayAfterDryDay_frn
 }
{ EQN: 97 }
class function EQ.ProbWetDayAfterWetDayFromProbWetDay_frn(probWetDay_frn: single; coeffForWetDryProbsGivenNumWetDays_frn:
  single): single;
  begin
  try
  result := 1.0 - coeffForWetDryProbsGivenNumWetDays_frn + probWetDay_frn;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ProbWetDayAfterWetDayFromProbWetDay_frn: ' + e.message); end;
  end;

{ 
When beta approaches 1.0, wet days do not affect
probability of rainfall -- P(W/D) = P(W/W) = PW. Conversely, low beta values
give strong wet day effects -- when beta approaches 0.0, P(W/W) approaches
1.0. Thus, beta controls the interval between rainfall events but has no
effect on the number of wet days. For many locations, beta = 0.75 gives
satisfactory estimates of P(W/D). Although equations 96 and 97 may give
slightly different probabilities than those estimated from rainfall
records, they do guarantee correct simulation of the number of rainfall
events.

When a precipitation event occurs, the amount is generated from a skewed
normal daily precipitation distribution [EQN: 98] where R is the amount
of rainfall for day i in mm, SND is the standard normal deviate for day i,
SCF is the skew coefficient, RSDV is the standard deviation of daily rainfall
in mm, and R(k) is the mean daily rainfall in month k.
Equation:
  R = (pow(((SND - SCF/6.0) * (SCF/6.0) + 1.0), 3) - 1.0) * RSDV / SCF + Rbar
Code:
  R = 2.0 * (pow(((SND - SCF/6.0) * (SCF/6.0) + 1.0), 3) - 1.0) * RSDV / SCF + Rbar
  also code adds lower bound of aResult at 0.01
Variables:
  R = DailyRainfallBySkewedNormal_mm
  SND = stdNormDeviateForRainfall
  SCF = skewCoeffForRainfallForMonth
  RSDV = stdDevDailyRainfallForMonth_mm
  Rbar = dailyMeanRainfallForMonth_mm
 }
{ EQN: 98 }
class function EQ.DailyRainfallBySkewedNormal_mm(stdNormDeviateForRainfall: single; skewCoeffForRainfallForMonth: single;
  stdDevDailyRainfallForMonth_mm: single; dailyMeanRainfallForMonth_mm: single): single;
  var
    skewCoeffDivSix: single;
    insideCube: single;
    insideCubeCubed: single;
    deviation: single;
    aResult: single;
  begin
  try
  if skewCoeffForRainfallForMonth = 0.0 then
    deviation := 0.0
  else
    begin
    skewCoeffDivSix := skewCoeffForRainfallForMonth / 6.0;
    insideCube := ((stdNormDeviateForRainfall - skewCoeffDivSix) * skewCoeffDivSix + 1.0);
    { added this check because power function balks at negative number }
    if (insideCube >= 0.0) then
      insideCubeCubed := power(insideCube, 3.0)
    else
      begin
      insideCubeCubed := power(-insideCube, 3.0);
      insideCubeCubed := -insideCubeCubed;
      end;
    deviation := safediv(insideCubeCubed - 1.0, skewCoeffForRainfallForMonth);
    end;
  aResult := dailyMeanRainfallForMonth_mm + 2.0 * deviation * stdDevDailyRainfallForMonth_mm;
  if (aResult < 0.01) then 
    result := 0.01
  else
    result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.DailyRainfallBySkewedNormal_mm: ' + e.message); end;
  end;

{ 
If the standard deviation and skew coefficient are not available, the model
simulates daily rainfall by using a modified exponential distribution
[EQN: 99] where mu is a uniform random number (0.0-1.0) and zeta is a
parameter usually in the range of 1.0 to 2.0. The larger the zeta value,
the more extreme the rainfall events. The denominator of equation 99 assures
that the mean long-term simulated rainfall is correct. The modified
exponential is usually a satisfactory substitute and requires only the monthly
mean rainfall as input.
Equation:
  R = power(- ln(mu), zeta) * Rbar / (integral from 0.0 to 1.0)power((- ln(chi), zeta) dx
Code:
  R = Rbar * power(- ln(mu), zeta)
  I think the integral is incorporated in the code when the dailyMeanRainfallForMonth_mm is
  multiplied at input by rainfallNormalizingFactorForModExpDist
  This is not completely understood.
Variables:
  R = DailyRainfallByModifiedExponential_mm
  mu = uniformRandomNumber
  zeta = coeffRainfallModExpDist
  Rbar = dailyMeanRainfallForMonth_mm
 }
{ EQN: 99 }
class function EQ.DailyRainfallByModifiedExponential_mm(coeffRainfallModExpDist: single; dailyMeanRainfallForMonth_mm:
  single; uniformRandomNumber: single): single;
  begin
  try
  result := dailyMeanRainfallForMonth_mm * power(-safeLn(uniformRandomNumber), coeffRainfallModExpDist);
  except on e: Exception do result := errorMessage('Exception in EQ.DailyRainfallByModifiedExponential_mm: ' + e.message); end;
  end;

{ 
Daily precipitation is partitioned between rainfall and snowfall using a
combination of maximum daily air temperature (T(mx)) and surface layer
soil temperature (T(1)). If the average of T(mx) and T(1) is zero degrees
C or below, the precipitation is snowfall, otherwise, it is rainfall.
  T(mx) = maxTempForDay_degC
  T(1) = soilSurfaceTempWithCover_degC
 }
{ no equation }
class function EQ.IsSnowing(maxTempForDay_degC: single; soilSurfaceTempWithCover_degC: single): integer;
  begin
  try
  if ((maxTempForDay_degC + soilSurfaceTempWithCover_degC) / 2 <= 0) then 
    result := 1
  else
    result := 0;
  except on e: Exception do begin errorMessage('Exception in EQ.IsSnowing: ' + e.message); result := 0; end; end;
  end;

{ 

              WEATHER - *AIR TEMPERATURE* AND SOLAR *RADIATION*

The model developed by Richardson (1981) was selected for use in EPIC
because it simulates temperature and radiation, which are mutually
correlated with rainfall. The residuals of daily maximum and minimum
air temperature and solar radiation are generated from a multivariate
normal distribution.

The multivariate generation model used implies that the residuals of
maximum temperature, minimum temperature, and solar radiation are
normally distributed and that the serial correlation of each variable
may be described by a first-order linear autogregressive model. Details of
the multivariate generation model were described by Richardson (1981).
The dependence structure of daily maximum temperature, minimum temperature,
and solar radiation were described by Richardson (1982).

The temperature model requires monthly means of maximum and minimum
temperatures and their standard deviations as inputs. If the standard
deviations are not available, the long-term observed extreme monthly
minima and maxima may be substituted. The model estimates standard
deviation as 0.25 of the difference between the extreme and the mean for
each month. For example: [EQN: 100] whre SDTMX is the standard deviation
of the daily maximum temperature, TE is the extreme daily maximum
temperature, and Tbar is the average daily maximum temperature for month k.
Equation:
  SDTMX = 0.25 * (TE - Tbar)
Code:
  equation for max temp is the same. equation for min temp is
  SDTMX = 0.25 * (Tbar - TE)
Variables:
  SDTMX = EQ::StdDevMaxTempFromMeanAndExtremeForMonth_degC or ...Min...
  TE = extremeDailyMaxTempForMonth_degC or ...Min...
  Tbar = dailyMeanMaxTempForMonth_degC or ...Min...
 }
{ EQN: 100 }
class function EQ.StdDevMaxTempFromMeanAndExtremeForMonth_degC(extremeDailyMaxTempForMonth_degC: single;
  dailyMeanMaxTempForMonth_degC: single): single;
  begin
  try
  result := 0.25 * (extremeDailyMaxTempForMonth_degC - dailyMeanMaxTempForMonth_degC);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.StdDevMaxTempFromMeanAndExtremeForMonth_degC: ' + e.message); end;
  end;

class function EQ.StdDevMinTempFromMeanAndExtremeForMonth_degC(extremeDailyMinTempForMonth_degC: single;
  dailyMeanMinTempForMonth_degC: single): single;
  begin
  try
  result := 0.25 * (dailyMeanMinTempForMonth_degC - extremeDailyMinTempForMonth_degC);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.StdDevMinTempFromMeanAndExtremeForMonth_degC: ' + e.message); end;
  end;

{ 
The solar radiation model uses the extreme approach extensively. Thus, only
the monthly means of daily solar radiation are required as inputs. The equation
for estimating standard deviation is [EQN: 101] whre SDRA is the standard
deviation of daily solar radiation i MJ/m2, RAMX is the maximum daily
solar radiation at midmonth, and RA is the mean daily solar radiation
for month k.
Equation:
  SDRA = 0.25 * (RAMX - RA)
Code:
  same
Variables:
  SDRA = EQ::StdDevRadiationFromMeanAndExtremeForDay_MJPm2
  RAMX = maxPossibleRadiation_MJPm2
  RA = meanRadiationForDay_MJPm2
 }
{ EQN: 101 }
class function EQ.StdDevRadiationFromMeanAndExtremeForDay_MJPm2(maxPossibleRadiation_MJPm2: single;
  meanRadiationForDay_MJPm2: single): single;
  begin
  try
  result := 0.25 * (maxPossibleRadiation_MJPm2 - meanRadiationForDay_MJPm2);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.StdDevRadiationFromMeanAndExtremeForDay_MJPm2: ' + e.message); end;
  end;

{ 
Maximum temperature and solar radiation tend to be lower on rainy days.
Thus, it is necessary to adjust the mean maximum temperature and solar
radiation downward for simulating rainy day conditions (note not min temp).
For T(mx) this is accomplished by assuming thta wet day values are less than
dry day values by some fraction of T(mx) - T(mn): [EQN: 102] where TW is
the daily mean maximum temperature for wet days in degrees C in month k,
TD is the daily mean maximum temperature for dry days, omega(T) is a scaling
factor ranging from 0.0 to 1.0, T(mx) is the daily mean maximum temperature,
and T(mn) is the daily mean minimum temperature. Choosing omega(T) = 1.0
provides highest deviations on wet days and omega(T) = 0.0 ignores the wet
day effect. Observed data indicate that omega(T) usually lies between
0.5 and 1.0.
Equation:
  TW(mx) = TD(mx) - omega(T) * (T(mx) - T(mn))
Code:
  same (omega(T) = 0.5)
Variables:
  TW(mx) = DailyMeanMaxTempWetDaysForMonth_degC
  TD(mx) = dailyMeanMaxTempDryDaysForMonth_degC
  omega(T) = tempResponseToWetDays_frn = 0.5
  T(mx) = dailyMeanMaxTempForMonth_degC
  T(mx) = dailyMeanMinTempForMonth_degC
 }
{ EQN: 102 }
class function EQ.DailyMeanMaxTempWetDaysForMonth_degC(dailyMeanMaxTempDryDaysForMonth_degC: single;
  dailyMeanMaxTempForMonth_degC: single; dailyMeanMinTempForMonth_degC: single): single;
  begin
  try
  result := dailyMeanMaxTempDryDaysForMonth_degC - 0.5 * (dailyMeanMaxTempForMonth_degC - dailyMeanMinTempForMonth_degC);
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMeanMaxTempWetDaysForMonth_degC: ' + e.message); end;
  end;

{ 
Since equation 102 gives lower mean maximum temperature values for wet days,
a companion equation is necessary to slightly increase mean maximum
temperature for dry days. The development is taken directly from the
continuity equation
  T(mx) * ND = TW(mx) * NWD + TD(mx) * NDD                      (Equation 103)
where ND is the number of days in a month, NWD is the number of wet days,
and NDD is the number of dry days. The desired equation is obtained by
substituing equation 102 into equation 103 and solving for TD [EQN: 104].
Use of the continuity equation guarantees that the long-term simulated value
for mean maximum temperature agrees with the input value of T(mx).
Equation:
  TD(mx) = T(mx) + (NWD / ND) * omega(T) * (T(mx) - T(mn))
Code:
  same
  probWetDayForMonth_frn = NWD / ND
  omega(T) = 0.5
Variables:
  TD(mx) = EQ::DailyMeanMaxTempDryDaysForMonth_degC
  NWD = numWetDaysForMonth
  ND = numDaysInMonth
  omega(T) = tempResponseToWetDays_frn = 0.5
  T(mx) = dailyMeanMaxTempForMonth_degC
  T(mx) = dailyMeanMinTempForMonth_degC
 }
{ EQN: 104 }
class function EQ.DailyMeanMaxTempDryDaysForMonth_degC(probWetDayForMonth_frn: single; dailyMeanMaxTempForMonth_degC:
  single; dailyMeanMinTempForMonth_degC: single): single;
  begin
  try
  result := dailyMeanMaxTempForMonth_degC + probWetDayForMonth_frn * 0.5 * (dailyMeanMaxTempForMonth_degC -
    dailyMeanMinTempForMonth_degC);
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMeanMaxTempDryDaysForMonth_degC: ' + e.message); end;
  end;

{ 
The method of adjusting solar radiation for wet and dry days is similar to
that of adjusting maximum temperature. The radiation on wet days is a fraction
of the dry day radiation [EQN: 105] where RAW is the daily mean radiation on
wet days in MJ/m2, omega(R) is a scaling factor ranging from 0.0 to 1.0, and
RAD is the daily mean solar radiation on dry days. An omega(R) value of 0.5
gives satisfactory results for many locations.
Equation:
  RAW = omega(R) * RAD
Code:
  same (omega(R) = 0.5)
Variables:
  RAW = EQ::DailyMeanRadiationWetDaysForMonth_MJPm2
  omega(R) = radiationResponseToWetDays_frn = 0.5
  RAD = dailyMeanRadiationDryDaysForMonth_MJPm2
 }
{ EQN: 105 }
class function EQ.DailyMeanRadiationWetDaysForMonth_MJPm2(dailyMeanRadiationDryDaysForMonth_MJPm2: single): single;
  begin
  try
  result := 0.5 * dailyMeanRadiationDryDaysForMonth_MJPm2;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.DailyMeanRadiationWetDaysForMonth_MJPm2: ' + e.message); end;
  end;

{ 
The dry day equation is developed by replacing temperature with radiation
in equation 103 and substituting equation 105 for RAW. Then, [EQN: 106]
where RA is the daily mean solar radiation for month k in MJ/m2.
Equation:
  RAD = (RA * ND) / (omega(R) * NWD + NDD)
Code:
  NDD = ND - NWD so
  RAD = (RA * ND) / (omega(R) * NWD + (ND - NWD))
  divide all terms by ND so
  RAD = RA / (omega(R) * (NWD/ND) + ND/ND - NWD/ND)
  probWetDayForMonth_frn = NWD / ND so
  RAD = RA / (omega(R) * probWetDayForMonth_frn + 1 - probWetDayForMonth_frn)
  omega(R) = 0.5 so
  RAD = RA / (0.5 * probWetDayForMonth_frn + 1 - probWetDayForMonth_frn)
  RAD = RA / (1.0 + (0.5 * probWetDayForMonth_frn - probWetDayForMonth_frn))
  RAD = RA / (1.0 - 0.5 * probWetDayForMonth_frn)
Variables:
  RAD = DailyMeanRadiationDryDaysForMonth_MJPm2
  RA =  dailyMeanRadiationForMonth_MJPm2
  ND = numDaysInMonth
  omega(R) = radiationResponseToWetDays_frn = 0.5
  NWD = numWetDaysForMonth
  NDD = numDryDaysInMonth (= ND - NWD)
 }
{ EQN: 106 }
class function EQ.DailyMeanRadiationDryDaysForMonth_MJPm2(dailyMeanRadiationForMonth_MJPm2: single; probWetDayForMonth_frn:
  single): single;
  begin
  try
  result := safediv(dailyMeanRadiationForMonth_MJPm2, 1.0 - 0.5 * probWetDayForMonth_frn);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.DailyMeanRadiationDryDaysForMonth_MJPm2: ' + e.message); end;
  end;

{ not in chapter }
class function EQ.DailyMaxTemperature_degC(meanMaxTempForDay_degC: single; stdDevMaxTempForMonth: single;
  maxTempCorrelationMultiplier: single): single;
  begin
  try
  result := meanMaxTempForDay_degC + stdDevMaxTempForMonth * maxTempCorrelationMultiplier;
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMaxTemperature_degC: ' + e.message); end;
  end;

{ not in chapter }
class function EQ.DailyMinTemperature_degC(dailyMeanMinTempForMonth_degC: single; stdDevMinTempForMonth: single;
  minTempCorrelationMultiplier: single): single;
  begin
  try
  result := dailyMeanMinTempForMonth_degC + stdDevMinTempForMonth * minTempCorrelationMultiplier;
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMinTemperature_degC: ' + e.message); end;
  end;

{ not in chapter }
class function EQ.DailyRadiation_MJPm2(meanRadiationForDay_MJPm2: single; radiationCorrelationMultiplier: single;
  stdDevRadiationForDay_MJPm2: single; maxPossibleRadiation_MJPm2: single): single;
  var
    aResult: single;
  begin
  try
  aResult := meanRadiationForDay_MJPm2 + stdDevRadiationForDay_MJPm2 * radiationCorrelationMultiplier;
  if (aResult <= 0.0) then 
    result := 0.05 * maxPossibleRadiation_MJPm2
  else
    result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.DailyRadiation_MJPm2: ' + e.message); end;
  end;

{ 
                                 WEATHER - *WIND*

The original EPIC wind model (Richardson and Wright, 1984) simulated mean
daily wind speed and daily direction. The new EPIC wind erosion model,
WECS (Wind Erosion Continuous Simulation) requires wind speed distribution
within the day and the dominant direction. Daily wind speed distribution is
simulated using the modified exponential equation [EQN: 107] where v(j)
is the wind speed at f fraction of the day, V(i) is the mean wind speed for
day i, and a(1) and a(2) are parameters. Equation 107 is an expression of
wind speed probabilities rather than wind speed as a function of time during
the day. The daily time distribution of wind speed is not required because
all other EPIC components operate on a daily time step. Experimental work
with data from Bushland, TX and Temple, TX indicates that 0.1 < a(2) < 0.6.
Equation:
  v(j) = a(1) * V * power(-ln(f(j)), a(2))
Code:
  v(j) = power(-ln(f(j)), a(2)) / a(1)
Variables:
  v(j) = WindSpeedForFractionOfDay_mPsec
  a(1) = windSpeedForFractionOfDayParam1
  V(i) = meanWindSpeedForDay_mPsec
  f(j) = fractionOfDay
  a(2) = windSpeedForFractionOfDayParam2
 }
{ EQN: 107 }
class function EQ.WindSpeedForFractionOfDay_mPsec(fractionOfDay: single; windSpeedForFractionOfDayParam1: single;
  windSpeedForFractionOfDayParam2: single): single;
  begin
  try
  result := safediv(power(-safeLn(fractionOfDay), windSpeedForFractionOfDayParam2), windSpeedForFractionOfDayParam1);
  except on e: Exception do result := errorMessage('Exception in EQ.WindSpeedForFractionOfDay_mPsec: ' + e.message); end;
  end;

{ Values of a(2) are generated daily from a triangular distribution with base }
{ ranging from 0.1 to 0.6 with a peak at 0.35. }
class function EQ.WindSpeedForFractionOfDayParam2(dailyMeanWindSpeedForMonth_mPsec: single; meanWindSpeedForDay_mPsec:
  single): single;
  begin
  try
  result := min(0.5, Utils_TriangularDistribution(0.1, 0.35, 0.6) * safediv(dailyMeanWindSpeedForMonth_mPsec,
    meanWindSpeedForDay_mPsec));
  except on e: Exception do result := errorMessage('Exception in EQ.WindSpeedForFractionOfDayParam2: ' + e.message); end;
  end;

{ 
The value of a(1) can be closely appointed with the equation [EQN: 108].
Equation 108 assures that
  V(i) = integral from 0.0 to 1.0 of v * df                    (Equation 109)
Equation:
  a(1) = 1.5567 * power(a(2), 0.1508) * exp(-0.4336 * a(2))
Code:
  a(1) = exp(0.4336 * a(2)) * (1/1.5567) * power(a(2), -0.1508)
  a(1) = exp(0.4336 * a(2)) / (1.5567 * power(a(2), 0.1508))
Variables:
  a(1) = EQ::WindSpeedForFractionOfDayParam1
  a(2) = windSpeedForFractionOfDayParam2
 }
{ EQN: 108 }
class function EQ.WindSpeedForFractionOfDayParam1(windSpeedForFractionOfDayParam2: single): single;
  begin
  try
  result := 0.6424 * power(windSpeedForFractionOfDayParam2,  - 0.1508) * safeExp(0.4336 * windSpeedForFractionOfDayParam2);
  except on e: Exception do result := errorMessage('Exception in EQ.WindSpeedForFractionOfDayParam1: ' + e.message); end;
  end;

{ 
Values of v(j) are simulated with f starting with the threshold windspeed
for erosion. The threshold f value (f(o)) is determined using the wind erosion
equation 141 and equation 107 in the form [EQN: 110] where v(*tau) is the
threshold friction velocity in m/sec. Larger f values produce non-erosive
wind speeds. Windspeeds greater than v(*tau) are produced as f is reduced.
Equation:
  f(o) = exp(power(v*(tau) / (0.0408 * a(1) * V), 1/a(2)))
Code:
  f(o) = exp(-power(v*(tau) / (0.0408 * V), 1/a(2))
  two differences: -power, and no a(1)
Variables:
  f(o) = FractionOfDayToStartSimulatingWindSpeed_frn
  v*(tau) = thresholdFrictionVelocityWE_mPsec
  a(1) = windSpeedForFractionOfDayParam1
  a(2) = windSpeedForFractionOfDayParam2
  V = meanWindSpeedForDay_mPsec
 }
{ EQN: 110 }
class function EQ.FractionOfDayToStartSimulatingWindSpeed_frn(fractionOfDayWindSpeedAtIntervalStart_frn: single;
  windSpeedForFractionOfDayParam2: single): single;
  begin
  try
  result := safeExp(-power(fractionOfDayWindSpeedAtIntervalStart_frn, safediv(1.0, windSpeedForFractionOfDayParam2)));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.FractionOfDayToStartSimulatingWindSpeed_frn: ' + e.message); end;
  end;

{ 
The mean daily wind speed is simulated using the modified exponential
equation [EQN: 111] where V(k) is the mean wind speed for month k,
RN is a random number b(2) is a parameter for month k, and b(1) is calculated
directly using an approach similar to that described in equation 108.
Experimental work with data from Bushland, TX and Temple, TX indicates
that b(2) ~~ 0.3.
Equation:
  V(i) = b(1) * V(k) * power(-ln(RN), b(2))
Code:
  same (b(1) is assumed to be 1.0)
Variables:
  V(i) = EQ::MeanWindSpeedForDay_mPsec
  b(1) = 1.0
  V(k) = dailyMeanWindSpeedForMonth_mPsec
  RN = randomNumber
  b(2) = paramForModifiedExpWindSpeed
 }
{ EQN: 111 }
class function EQ.MeanWindSpeedForDay_mPsec(dailyMeanWindSpeedForMonth_mPsec: single; paramForModifiedExpWindSpeed:
 single): single;
  var
    randomNumber: single;
  begin
  try
  randomNumber := Utils_RandomZeroToOne;
  result := dailyMeanWindSpeedForMonth_mPsec * power(-safeLn(randomNumber), paramForModifiedExpWindSpeed);
  except on e: Exception do result := errorMessage('Exception in EQ.MeanWindSpeedForDay_mPsec: ' + e.message); end;
  end;

{ 
Wind direction expressed as radians from north in a clockwise direction is
generated from an empirical distribution specific for each location. The
empirical distribution is simply the cumulative probability distribution
of wind direction. The "Climatic Atlas of the United States" (U.S. Department
of Commerce, 1968) gives monthly percentages of wind from each of 16
directions. Thus, to estimate wind direction for any day, the model draws
a uniformly distributed random number and locates its position on the
appropriate monthly cumulative probability distribution.
 }
{ from code }
{ In the 3rd last line the added 1.0 is because their direction array starts at 1 and ours at 0. }
  class function EQ.WindDirectionFromNorth_rad(var windDirsForMonth_frn: windDirectionsArray; month: integer): single;
  var
    thisDir, lastDir, i: integer;
    combinedTotal_frn, portionOfThisDir, aResult, randomNumber: single;
  begin
  try
  combinedTotal_frn := 0.0;
  portionOfThisDir := 0.0;
  randomNumber := Utils_RandomZeroToOne;
  { choose one of 16 compass directions to start with }
  thisDir := 0;
  for i := 0 to 15 do
    begin
    combinedTotal_frn := combinedTotal_frn + (windDirsForMonth_frn[month][thisDir]);
    thisDir := i;
    if (randomNumber < combinedTotal_frn) then break;
    end;
  { interpolate between two directions to get precise wind direction }
  lastDir := thisDir - 1;
  if lastDir < 0 then lastDir := 0;  {PDF - should this be 15? Also, why doesn't it do next is lastDir = 0?}
  if (lastDir <> 0) then if (windDirsForMonth_frn[month][thisDir] <> windDirsForMonth_frn[month][lastDir]) then
    portionOfThisDir := safediv(randomNumber - windDirsForMonth_frn[month][lastDir], windDirsForMonth_frn[month][thisDir]
     - windDirsForMonth_frn[month][lastDir]);
  { convert direction and portion to radians }
  aResult := 2.0 * kPi / 16.0 * (lastDir + portionOfThisDir - 0.5 + 1.0);
  if (aResult < 0) then aResult := aResult + (2.0 * kPi);
  result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.WindDirectionFromNorth_rad: ' + e.message); end;
  end;

{ 
                           WEATHER - *RELATIVE HUMIDITY*

The relative humidity model simulates daily average relative humidity
from the monthly average by using a triangular distribution. As with
temperature and radiation, the mean daily relative humidity is adjusted
to account for wet- and dry-day effects. The assumed relation between
relative humidity on wet and dry days is [EQN: 112] where RHW is the daily
mean relative humidity on wet days for month k, RHD is the daily mean
relative humidity on dry days, and omega(H) is a scaling factor ranging
from 0.0 to 1.0. An omega(H) value of 0.9 seems appropriate for many
locations.
Equation:
  RHW = RHD + omega(H) * (1.0 - RHD)
Code:
  omega(H) = 0.9
  RHW = RHD + 0.9 * (1.0 - RHD)
  RHW = RHD + 0.9 - 0.9 * RHD
  RHW = 0.1 * RHD + 0.9
Variables:
  RHW = DailyMeanRelHumWetDaysForMonth_frn
  RHD = dailyMeanRelHumDryDaysForMonth_frn
  omega(H) = relHumResponseToWetDays_frn = 0.9
 }
{ EQN: 112 }
class function EQ.DailyMeanRelHumWetDaysForMonth_frn(dailyMeanRelHumDryDaysForMonth_frn: single): single;
  begin
  try
  result := 0.1 * dailyMeanRelHumDryDaysForMonth_frn + 0.9;
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMeanRelHumWetDaysForMonth_frn: ' + e.message); end;
  end;

{ 
Using the continuity equation as described in the temperature
and radiation sections produces the equation [EQN: 113] where RH is the
long-term average relative humidity for month k.
Equation:
  RHD = RH - (omega(H) * (NWD/ND)) / (1.0 - omega(H) * (NWD/ND))
Code:
  same except for bounds check: if RHD < 0.05 RHD = 0.5 * RH
  omega(H) = 0.9
Variables:
  RHD = DailyMeanRelHumDryDaysForMonth_frn
  RH = dailyMeanRelHumForMonth_frn
  omega(H) = relHumResponseToWetDays_frn = 0.9
  NWD = numWetDaysForMonth
  ND = numDaysInMonth
 }
{ EQN: 113 }
class function EQ.DailyMeanRelHumDryDaysForMonth_frn(dailyMeanRelHumForMonth_frn: single; probWetDayForMonth_frn: single):
  single;
  var
    aResult: single;
  begin
  try
  aResult := safediv(dailyMeanRelHumForMonth_frn - 0.9 * probWetDayForMonth_frn, 1.0 - 0.9 * probWetDayForMonth_frn);
  if (aResult < 0.05) then 
    result := 0.5 * dailyMeanRelHumForMonth_frn
  else
    result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMeanRelHumDryDaysForMonth_frn: ' + e.message); end;
  end;

{ 
The appropriate value (RHW or RHD) is used as the peak of a triangular
distribution to generate daily relative humidity. The upper limit of the
triangular distribution is set with the equation [EQN: 114] where RHU
is the largest relative humidity value that can be generated on day i
and RHP is the peak of the triangular distribution (RHW or RHD).
Equation:
  RHU = RHP + (1.0 - RHP) * exp(RHP - 1.0)
Code:
  same (rearranged)
Variables:
  RHU = RelHumTriangularDistUpperLimit_frn
  RHP = peakOfRelHumTriangularDistr_frn
 }
{ EQN: 114 }
class function EQ.RelHumTriangularDistUpperLimit_frn(meanRelHumForDay_frn: single): single;
  begin
  try
  result := meanRelHumForDay_frn - (meanRelHumForDay_frn - 1.0) * safeExp(meanRelHumForDay_frn - 1.0);
  except on e: Exception do result := errorMessage('Exception in EQ.RelHumTriangularDistUpperLimit_frn: ' + e.message); end;
  end;

{ 
The lower limit is set with the equation [EQN: 115] where RHL is the lowest
relative humidity value that can be generated on day i.
Equation:
  RHL = RHP * (1.0 - exp(-RHP))
Code:
  same
Variables:
  RHL = RelHumTriangularDistLowerLimit_frn
  RHP = peakOfRelHumTriangularDist_frn
 }
{ EQN: 115 }
class function EQ.RelHumTriangularDistLowerLimit_frn(meanRelHumForDay_frn: single): single;
  begin
  try
  result := meanRelHumForDay_frn * (1.0 - safeExp(-meanRelHumForDay_frn));
  except on e: Exception do result := errorMessage('Exception in EQ.RelHumTriangularDistLowerLimit_frn: ' + e.message); end;
  end;

{ 
To assure that the simulated long-term value for mean relative humidity agrees
with input RH, the generated value is adjusted by using the equation [EN: 116]
where RHG* is the generated relative humidity on day i adjusted to the mean
of the triangle, RHG is the relative humidity generated from the triangle,
and RH is the mean of the triangle.
// EQN: 116 --- not used in code

If relative humidity data is not available, dew point temperature may be
substituted and EPIC estimates the monthly mean relative humidity using
equations 52 and 53. If dew point temperature is not available EPIC estimates
monthly mean relative humidity using the equation [EQN: 117] where delta-t =
T(mx) - T(mn) for month k.
Equation:
  RH = 0.9 - (0.8 * delta-t) / (delta-t + exp(5.12 - 0.127 * delta-t))
Code:
  same
Variables:
  RH = DailyMeanRelHumForMonthFromTempOnly_frn
  delta-t = maxMinusMinTemp_degC
  T(mx) = dailyMeanMaxTempForMonth_degC
  T(mn) = dailyMeanMinTempForMonth_degC
 }
{ EQN: 117 }
class function EQ.DailyMeanRelHumForMonthFromTemp_frn(dailyMeanMaxTempForMonth_degC: single; dailyMeanMinTempForMonth_degC:
  single): single;
  var
    maxMinusMinTemp_degC: single;
  begin
  try
  maxMinusMinTemp_degC := dailyMeanMaxTempForMonth_degC - dailyMeanMinTempForMonth_degC;
  result := 0.9 - 0.8 * scurve(maxMinusMinTemp_degC, 5.122, 0.1269);
  except on e: Exception do result := errorMessage('Exception in EQ.DailyMeanRelHumForMonthFromTemp_frn: ' + e.message); end;
  end;

{ EQN: 52 (use equation for saturation vapor pressure to convert dew point to rel hum) }
class function EQ.RelHumForMonthFromDewPoint_frn(dewPoint_degC: single; meanTempForMonth_degC: single): single;
  var
    satVPWithDewPoint_kPa: single;
    satVPWithTemp_kPa: single;
  begin
  try
  satVPWithDewPoint_kPa := EQH.SaturVaporPressure_kPa(dewPoint_degC);
  satVPWithTemp_kPa := EQH.SaturVaporPressure_kPa(meanTempForMonth_degC);
  result := safediv(satVPWithDewPoint_kPa, satVPWithTemp_kPa);
  except on e: Exception do result := errorMessage('Exception in EQ.RelHumForMonthFromDewPoint_frn: ' + e.message); end;
  end;
                                                                                                      
{ 
Day length is a function of the time of year and latitude
as expressed in the equation [EQN: 317] where LAT is the
latitude of the watershed in degrees and SD is the sun's
declination angle.
Equation:
  HRLT = 7.64 * acos((-sin((2pi/360) * LAT) * sin(SD) - 0.044)
         / (cos((2pi/360) * LAT) * cos(SD))
Code:
  HRLT = 7.72 * acos(-tan((2pi/360) * LAT) * tan(SD))
  since tan is sin/cos, that is the same, the only two differences are
  the 0.044 lost in the code and the change of constant from 7.64 to 7.72
Variables:
  HRLT = DayLength_hr
  LAT = stationLatitude_rad
  SD = sunDeclinationAngle_rad
 }
{ EQN: 317 }
class function EQ.DayLength_hr(stationLatitude_rad: single; sunDeclinationAngle_rad: single): single;
  var
    cosHourAngle: single;
    hourAngle: single;
  begin
  try
  cosHourAngle :=  -tan(stationLatitude_rad) * tan(sunDeclinationAngle_rad);
  hourAngle := EQ.HourAngleFromCosHourAngle(cosHourAngle);
  result := 7.72 * hourAngle;
  except on e: Exception do result := errorMessage('Exception in EQ.DayLength_hr: ' + e.message); end;
  end;

{ from code }
class function EQ.MinDayLengthForYear_hr(stationLatitude_rad: single): single;
  var
    cosHourAngle: single;
    hourAngle: single;
  begin
  try
  cosHourAngle := 0.4349 * abs(tan(stationLatitude_rad));
  hourAngle := EQ.HourAngleFromCosHourAngle(cosHourAngle);
  result := 7.72 * hourAngle;
  except on e: Exception do result := errorMessage('Exception in EQ.MinDayLengthForYear_hr: ' + e.message); end;
  end;

{ from code }
class function EQ.HourAngleFromCosHourAngle(cosHourAngle: single): single;
  begin
  try
  if (cosHourAngle >= 1.0) then 
    begin
    result := 0.0;
    end
  else if (cosHourAngle <=  - 1.0) then
    begin
    result := kPi;
    end
  else
    result := arcCos(cosHourAngle);
  except on e: Exception do result := errorMessage('Exception in EQ.HourAngleFromCosHourAngle: ' + e.message); end;
  end;

{
The sun's declination angle, SD, is defined by the equation
[EQN: 318].
Equation:
  SD = 0.4102 * sin((2pi/365) * (i - 80.25))
Code:
  same except in code they calculate this once per month
  GS will calculate it every day
Variables:
  SD = SunDeclinationAngle_rad
  i = julianDay
 }
{ EQN: 318 }
class function EQ.SunDeclinationAngle_rad(julianDay: integer): single;
  begin
  try
  result := 0.4102 * sin(2.0 * kPi / 365.0 * (julianDay - 80.25));
  except on e: Exception do result := errorMessage('Exception in EQ.SunDeclinationAngle_rad: ' + e.message); end;
  end;

{ Most comments taken verbatim from J.R. Williams paper }
{
                      EROSION - *WATER EROSION FROM RAINFALL/RUNOFF*

The EPIC component for water-induced erosion simulates erosion caused by
rainfall and runoff and by irrigation (sprinkler and furrow). To simulate
rainfall/runoff erosion, EPIC contains six equations -- the USLE (Wischmeier
and Smith, 1978), the Onstad-Foster modification of the USLE (Onstad and
Foster, 1975), the MUSLE (Williams, 1975), two recently developed
variations of MUSLE (MUST and MUSS), and a MUSLE structure that accepts
input coefficients (MUSI).  Only one of the equations (MUSI) interacts with
other EPIC components. The six equations are identical except for their
energy components. The USLE depends strictly upon rainfall as an indicator
of erosive energy. The MUSLE and its variations use only runoff variables to
simulate erosion and sediment yield. Runoff variables increase the prediction
accuracy, eliminate the need for a delivery ratio (used in the USLE to
estimate sediment yield), and enable the equation to give single storm
estimates of sediment yields. The USLE give only annual estimates. The Onstad-
Foster equation contains a combination of the USLE and MUSLE energy factors.

Thus, the water erosion model uses an equation of the form [EQN: 118] and
[EQN: 119], where Y is the sediment yield in t/ha, K is the soil erodibility
factor, CE is the crop management factor, PE is the erosion control practice
factor, LS is the slope length and steepness factor, ROKF is the coarse
fragment factor, Q is the runoff volume in mm, q(p) is the peak runoff
rate in mm/hr, A is the watershed area in ha, MUST is a new equation
theoretically developed from sediment concentration databases, MUSS is a new
equation developed by fitting small watershed data (no channel erosion), and
MUSI allows user input of four coefficients (b(y1)..b(y4)). The PE value is
determined initially by considering the conservation practices to be applied.
The runoff model supplies estimates of Q and q(p).
Equation:
  Y = chi * K * CE * PE * LS * ROKF
Code:
  same
Variables:
  Y = EQ::WaterErosion_tPha
  chi = energyComponent
  K = soilErodibilityFactor
  CE = cropManagementFactor
  PE = erosionControlPracticeFactor
  LS = slopeLengthAndSteepnessFactor
  ROKF = coarseFragmentFactor
 }
{ EQN: 118 }
class function EQ.WaterErosion_tPha(energyComponent: single; soilErodibilityFactor: single; cropManagementFactor: single;
  erosionControlPracticeFactor: single; slopeLengthAndSteepnessFactor: single; coarseFragmentFactor: single): single;
  begin
  try
  result := energyComponent * soilErodibilityFactor * cropManagementFactor * erosionControlPracticeFactor *
    slopeLengthAndSteepnessFactor * coarseFragmentFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosion_tPha: ' + e.message); end;
  end;

{ 
Now all the energy equations. We will start with the MUSLE and its derivatives
then move to the USLE and Onstad-Foster.
MUSLE
Equation:
  chi = 1.586 * power(Q * q(p), 0.65) * power(A, 0.12)
Code:
  chi = 1.586 * power(Q * q(p), 0.65) * power(A + 1.0, 0.12)
Variables:
  chi = EQ::WaterErosionEnergyComponentByMUSLE
  Q = runoffVolume_mm
  q(p) = peakRunoffRate_mmPhr
  A = watershedArea_ha
 }
{ EQN: 119 }
class function EQ.WaterErosionEnergyComponentByMUSLE(runoffVolume_mm: single; peakRunoffRate_mmPhr: single; watershedArea_ha:
  single): single;
  begin
  try
  result := 1.586 * power(runoffVolume_mm * peakRunoffRate_mmPhr, 0.56) * power(1.0 + watershedArea_ha, 0.12);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosionEnergyComponentByMUSLE: ' + e.message); end;
  end;

{  MUST
Equation:
  chi = 2.5 * sqrt(Q * q(p))
Code:
  same
Variables:
  chi = EQ::WaterErosionEnergyComponentByMUST
  Q = runoffVolume_mm
  q(p) = peakRunoffRate_mmPhr
 }
class function EQ.WaterErosionEnergyComponentByMUST(runoffVolume_mm: single; peakRunoffRate_mmPhr: single): single;
  begin
  try
  result := 2.5 * sqrt(runoffVolume_mm * peakRunoffRate_mmPhr);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosionEnergyComponentByMUST: ' + e.message); end;
  end;

{  MUSS
Equation:
  chi = 0.79 * power(Q * q(p), 0.65) * power(A, 0.009)
Code:
  chi = 0.79 * power(Q * q(p), 0.65) * power(A + 1.0, 0.009)
Variables:
  chi = EQ::WaterErosionEnergyComponentByMUSS
  Q = runoffVolume_mm
  q(p) = peakRunoffRate_mmPhr
  A = watershedArea_ha
 }
class function EQ.WaterErosionEnergyComponentByMUSS(runoffVolume_mm: single; peakRunoffRate_mmPhr: single; watershedArea_ha:
  single): single;
  begin
  try
  result := 0.79 * power(runoffVolume_mm * peakRunoffRate_mmPhr, 0.65) * power(1.0 + watershedArea_ha, 0.009);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosionEnergyComponentByMUSS: ' + e.message); end;
  end;

{  MUSI
Equation:
  chi = by(1) * power(Q, by(2)) * power(q(p), by(3)) * power(A, by(4))
Code:
  chi = by(1) * power(Q, by(2)) * power(q(p), by(3)) * power(A + 1.0, by(4))
Variables:
  chi = EQ::WaterErosionEnergyComponentByMUSS
  Q = runoffVolume_mm
  q(p) = peakRunoffRate_mmPhr
  A = watershedArea_ha
  by(1)(2)(3)(4) = userCoeffsForMUSI
 }
class function EQ.WaterErosionEnergyComponentByMUSI(runoffVolume_mm: single; peakRunoffRate_mmPhr: single; watershedArea_ha:
  single; var userCoeffsForMUSI: arrayFour): single;
  begin
  try
  result := userCoeffsForMUSI[0] * power(runoffVolume_mm, userCoeffsForMUSI[1]) * power(peakRunoffRate_mmPhr,
    userCoeffsForMUSI[2]) * power(1.0 + watershedArea_ha, userCoeffsForMUSI[3]);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosionEnergyComponentByMUSI: ' + e.message); end;
  end;

{ 
For the energy component for USLE and Onstad-Foster, we have to move forward
to equations 125-136. This is going to get a bit mucky...

To estimate the daily rainfall energy in the absence of time-distributed
rainfall, it is assumed that the rainfall rate is exponentially distributed
  r(t) = r(p) * exp(-t / kappa)                                  (Equation 124)
where r is the rainfall rate at time t in mm/hr, r(p) is the peak rainfall
rate in mm/hr and kappa is the decay constant in hrs. Equation 124 contains
no assumption about the sequence of rainfall rates (time distribution).

The USLE energy equation in metric units is
  RE = delta-r * (12.1 + 8.9 log (delta-r / delta-t))               (Equation 125)
where RE is the rainfall energy for water erosion equations and delta-r is
a rainfall amount in mm during a time interval delta-t in hours. The energy
equation can be expressed analytically as
  RE = 12.1 * 0~infinity r dt + 8.9 0~infinity log r dt          (Equation 126)
Substituting equation 124 into equation 126 and integrating gives the equation
for estimating daily rainfal energy:
  RE = R * (12.1 + 8.9 * (log r(p) - 0.434))                     (Equation 127)
where R is the daily rainfall amount in mm. The rainfall energy factor, EI,
is obtained by multiplying equation 127 by the maximum 0.5-hr rainfall
intensity (r(0.5)) and converting to the proper units: [EQN: 128].
Equation:
  EI = R * (12.1 + 8.9 * (log(r(p)) - 0.434)) * r(0.5) / 1000
Code:
  r(0.5) = 2.0 * R * alpha(0.5)
  they do not mention how to get r(0.5) in chapter
  also add lower bound at 0
Variables:
  EI =  EQ::RainfallEnergyFactorForUSLE
  R = rainfallWithoutSnowmeltForDay_mm
  r(p) = peakRainfallRate_mmPhr
  r(0.5) = maxRainfallIntensityInHalfHour_mmPhr
 }
{ EQN: 128 }
class function EQ.RainfallEnergyFactorForUSLE(rainfallWithoutSnowmeltForDay_mm: single; peakRainfallRate_mmPhr: single;
  propRainInFirstHalfHourModBySnow: single): single;
  var
    maxRainfallIntensityInHalfHour_mmPhr: single;
    result_mPhr: single;
  begin
  try
  maxRainfallIntensityInHalfHour_mmPhr := 2.0 * rainfallWithoutSnowmeltForDay_mm * propRainInFirstHalfHourModBySnow;
  result_mPhr := mm_to_m * rainfallWithoutSnowmeltForDay_mm * (12.1 + 8.9 * (log10(peakRainfallRate_mmPhr) - 0.4343)) *
    maxRainfallIntensityInHalfHour_mmPhr;
  if (result_mPhr < 0.0) then 
    result := 0.0
  else
    result := result_mPhr;
  except on e: Exception do result := errorMessage('Exception in EQ.RainfallEnergyFactorForUSLE: ' + e.message); end;
  end;

{  USLE part of EQN: 118
Equation:
  chi = EI
Code:
  chi = EI * 1.292
Variables:
  chi = EQ::WaterErosionEnergyComponentByUSLE
  EI = rainfallEnergyFactorForUSLE
 }
class function EQ.WaterErosionEnergyComponentByUSLE(rainfallEnergyFactorForUSLE: single): single;
  begin
  try
  result := 1.292 * rainfallEnergyFactorForUSLE;
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosionEnergyComponentByUSLE: ' + e.message); end;
  end;

{ 
To compute values for r(p), equation 124 is integrated to give
  R = r(p) * kappa                                             (Equation 129)
and
  R(t) = R * (1 - exp(-t / kappa))                             (Equation 130)
The value of R(0.5) can be estimated by using alpha(0.5) as mentioned in the
Hydrology section of this chapter:
  R(0.5) = alpha(0.5) * R                                      (Equation 131)
To determine the value of r(p), equations 131 and 129 are substituted into
equation 130 to give [EQN: 132].
Equation:
  r(p) = -2 * R * ln(1.0 - alpha(0.5))
Code:
  same
Variables:
  r(p) = PeakRainfallRate_mmPhr
  R = rainfallForDay_mm
  alpha(0.5) = propTotalRainFallsInFirstHalfHour_frn
 }
{ EQN: 132 }
class function EQ.PeakRainfallRate_mmPhr(rainfallMinusSnowmelt_mm: single; propRainInFirstHalfHourModBySnow: single): single;
  begin
  try
  result :=  - 2.0 * rainfallMinusSnowmelt_mm * safeLn(1.0 - propRainInFirstHalfHourModBySnow);
  except on e: Exception do result := errorMessage('Exception in EQ.PeakRainfallRate_mmPhr: ' + e.message); end;
  end;

{ 
Since rainfall rates vary seasonally, alpha(0.5) is evaluated for each
month by using Weather Service information (U.S. Department of Commerce, 1979).
The frequency with which the maximum 0.5-hr rainfall amount occurs is estimated
by using the Hazen plotting position equation (Hazen, 1930) [EQN: 133]
  F = 1 / (2 * tau)                                             (Equation 133)
where F is the frequency with which the largest of a total of tau events
occurs. The total number of events for each month is the product of the number
of years of record and the average number of rainfall events for the month.
Equation:
  F = 1 / (2 * tau)
Code:
  same
Variables:
  F = EQ::FrequencyOfRainfallEvents_Pyr
  tau = yearsRecordMaxHalfHourRain
 }
{ EQN: 133 }
class function EQ.FrequencyOfRainfallEvents_Pyr(yearsRecordMaxHalfHourRain: single): single;
  begin
  try
  result := safediv(0.5, yearsRecordMaxHalfHourRain);
  except on e: Exception do result := errorMessage('Exception in EQ.FrequencyOfRainfallEvents_Pyr: ' + e.message); end;
  end;

{ 
To estimate the mean value of alpha(0.5), it is necessary to estimate the
mean value of R(0.5). The value of R(0.5) can be computed easily if the
maximum 0.5-hr rainfall amounts are assumed to be exponentially distributed.
From the exponential distribution, the expression for the mean 0.5-hr
rainfall amount is
  R(0.5) = R(0.5F)/-log(F)                                        (Equation 134)
where R(0.5) is the mean maximum 0.5-hr rainfall amount, R(0.5F) is the maximum
0.5-hr rainfall amount for frequency F, and subscript k refers to the month.

The mean alpha(0.5) is computed with the equation [EQN: 135] where R is the
mean amount of rainfall for each event (averagy monthly rainfall/average
number of days of rainfall) and subscript k refers to the month. Daily values
of alpha(0.5) are generated from a triangular distribution.
Equation:
  alpha(0.5) = R(0.5) / R
Code:
  alpha(0.5) = -R(0.5) / log((F / numWetDaysForMonth) * R)
Variables:
  F = frequencyOfRainfallEvents
  alpha(0.5) = MeanPropTotalRainFallsInFirstHalfHourForMonth_frn
  R(0.5) = meanMaxHalfHourRainfallAmountForMonth_mm
  R = meanRainfallPerEventForMonth_mm = meanTotalRainfallForMonth_mm / numWetDaysForMonth
 }
{ EQN: 135 }
class function EQ.MeanPropRainInFirstHalfHourForMonth_frn(frequencyOfRainfallEvents_Pyr: single;
  meanMaxHalfHourRainfallAmountForMonth_mm: single; meanTotalRainfallForMonth_mm: single; numWetDaysForMonth: single):
  single;
  begin
  try
  result := safediv(-meanMaxHalfHourRainfallAmountForMonth_mm, safeLn(safediv(frequencyOfRainfallEvents_Pyr,
    numWetDaysForMonth)) * safediv(meanTotalRainfallForMonth_mm, numWetDaysForMonth));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.MeanPropRainInFirstHalfHourForMonth_frn: ' + e.message); end;
  end;

{ 
The lower limit of alpha(0.5), determined by a uniform rainfall, is
to 0.5/24 or 0.0208. The upper limit of alpha(0.5) is set by considering
a large rainfall event. In a large event, it is highly unlikely that all the
rainfall occurs in 0.5 hr (alpha(0.5) = 1.0). The upper limit of alpha(0.5)
can be estimated by substituting a high value for r(p) (250 mm/hr is generally
near the upper limit of rainfall intensity) into equation 130 [EQN: 136]
where alpha(0.5u) is the upper limit of alpha(0.5). The peak of the alpha(0.5)
triangular distribution is alpha(0.5) from equation 135.
So the lower limit is 0.0208, the upper limit is the aResult of equation 136,
and the peak is the aResult of equation 135.
Equation:
  alpha(0.5u) = 1.0 - exp(-125 / R)
  where R is mean daily rainfall for the month
Code:
  alpha(0.5u) = 1.0 - exp(-125 / (R + 5))
  where R is rainfall for the day
Variables:
  alpha(0.5u) = MaxPropTotalRainFallsInFirstHalfHourForMonth_frn
  R = meanRainfallPerEventForMonth_mm = meanTotalRainfallForMonth_mm / numWetDaysForMonth
  R = rainfallWithoutSnowmeltForDay_mm
 }
{ EQN: 136 }
class function EQ.MaxPropTotalRainFallsInFirstHalfHourForMonth_frn(rainfallWithoutSnowmeltForDay_mm: single): single;
  begin
  try
  result := 1.0 - safeExp(safediv(-125.0, rainfallWithoutSnowmeltForDay_mm + 5.0));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.MaxPropTotalRainFallsInFirstHalfHourForMonth_frn: ' + e.message);
    end;
  end;

{  Onstad-Foster part of EQN: 118
For the USLE method, X is the same as EI (equation 118), but for the
Onstad-Foster method, we need a function for that part of equation 118.
Equation:
  chi = 0.646 * EI + 0.45 * power(Q * q(p), 0.33)
Code:
  same
Variables:
  chi = EQ::WaterErosionEnergyComponentByOnstadFoster
  EI = rainfallEnergyFactorForUSLE
  Q = runoffVolume_mm
  q(p) = peakRunoffRate_mmPhr
 }
class function EQ.WaterErosionEnergyComponentByOnstadFoster(runoffVolume_mm: single; peakRunoffRate_mmPhr: single;
  rainfallEnergyFactorForUSLE: single): single;
  begin
  try
  result := 0.646 * rainfallEnergyFactorForUSLE + 0.45 * power(runoffVolume_mm * peakRunoffRate_mmPhr, 0.33);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.WaterErosionEnergyComponentByOnstadFoster: ' + e.message); end;
  end;

{ 
Now to fill in the other parts of the water erosion equation, we go back to
equations 120-123 and stick equation 137 on the end.

The value of LS is calculated with the equation (Wischmeier and Smith,
1978) [EQN: 120] where S is the land surface slope in m/m, lamda is the
slope length in m, and xi is a parameter dependent upon slope.
Equation:
  LS = pow(lambda / 22.1, xi) * (65.41 * sqr(S) + 4.56 * S + 0.065)
Code:
  same
Variables:
  LS = SlopeLengthAndSteepnessFactor
  lambda = slopeLength_m
  xi = slopeLengthFactorParam
  S = slopeSteepness_mPm
 }
{ EQN: 120 }
class function EQ.SlopeLengthAndSteepnessFactor(slopeLength_m: single; slopeLengthFactorParam: single; slopeSteepness_mPm:
  single): single;
  begin
  try
  result := power(slopeLength_m / 22.127, slopeLengthFactorParam) * (65.41 * sqr(slopeSteepness_mPm) + 4.56 *
    slopeSteepness_mPm + 0.065);
  except on e: Exception do result := errorMessage('Exception in EQ.SlopeLengthAndSteepnessFactor: ' + e.message); end;
  end;

{ 
The value of xi varies with slope and is estimated with the equation [EQN: 121].
Equation:
  xi = 0.3 * S / (S + exp(-1.47 -61.09 * S)) + 0.2
Code:
  same
Variables:
  xi = EQ::SlopeLengthFactorParam
  S = slopeSteepness_mPm
 }
{ EQN: 121 }
class function EQ.SlopeLengthFactorParam(slopeSteepness_mPm: single): single;
  begin
  try
  result := 0.3 * scurve(slopeSteepness_mPm,  - 1.47, 61.09) + 0.2;
  except on e: Exception do result := errorMessage('Exception in EQ.SlopeLengthFactorParam: ' + e.message); end;
  end;

{ 
The crop management factor is evaluated for all days when runoff occurs by
using the equation [EQN: 122] where CE(mn,j) is the minimum value of the crop
management factor for crop j and CV is the soil cover (above ground biomass
plus residue) in t/ha.
Equation:
  CE = exp[(ln(0.8) - ln(CE(mn,j))) * exp(-1.15 * CV) + ln(CE(mn,j))]
Code:
  same (ln(0.8) is -0.2231)
Variables:
  CE = EQ::CropManagementFactor
  CE(mn,j) = patchMeanMinCropManagementFactor
  CV = patchTotalAboveGroundBiomassAndResidue_tPha
 }
{ EQN: 122 }
class function EQ.CropManagementFactor(patchMeanMinCropManagementFactor: single; patchTotalAboveGroundBiomassAndResidue_tPha:
  single): single;
  var
    logMinCropManagementFactor: single;
  begin
  try
  logMinCropManagementFactor := 0.0;
  if (patchMeanMinCropManagementFactor >= kLowestFloatAboveZero) then logMinCropManagementFactor :=
    safeLn(patchMeanMinCropManagementFactor);
  result := safeExp((-0.2231 - logMinCropManagementFactor) * safeExp(-1.15 * patchTotalAboveGroundBiomassAndResidue_tPha)
    + logMinCropManagementFactor);
  except on e: Exception do result := errorMessage('Exception in EQ.CropManagementFactor: ' + e.message); end;
  end;

{ 
The soil erodibility factor, K, is evaluated for the top soil layer at the
start of each year of simulation with the equation [EQN: 123] where SAN,
SIL, CLA, and C are the sand, silt, clay, and organic carbon contents of the
soil in percent and SN1 = 1 - SAN / 100. Equation 123 allows K to vary from
about 0.1 to 0.5. The first term gives low K values for soils with high
coarse-sand contents and high values for soil with little sand. The fine
sand content is estimated as the product of sand and silt divided by
100. The expression for coarse sand in the first term is simply the difference
between sand and the estimated fine sand. The second term reduces K for soils
that have high clay to silt ratios. The third term reduces K for soils
with high organic carbon contents. The fourth term reduces K further for
soils with extremely high sand contents (SAN>70%).
Equation:
  K = [0.2 + 0.3 * exp(-0.0256 * SAN * (1 - SIL/100))]
      * pow(SIL / (CLA + SIL), 0.3)
      * [1 - 0.25 * C / (C + exp(3.72 - 2.95 * C))]
      * [1 - 0.7 * (1-SAN/100) / ((1-SAN/100) + exp(-5.51 + 22.9 * (1-SAN/100)))]
Code:
  same
Variables:
  K = soilErodibilityFactor
  SAN = soilSandContent_pct
  SIL = siltContent_pct
  CLA = clayContent_pct
  C = organicC_pct
  SN1 = nonSandContent_frn
 }
{ EQN: 123 }
class function EQ.SoilErodibilityFactor(soilSandContent_pct: single; siltContent_pct: single; clayContent_pct: single;
  organicMatter_tPha: single; soilWeight_tPha: single): single;
  var
    organicC_pct: single;
    nonSiltContent_frn: single;
    nonSandContent_frn: single;
    firstTerm: single;
    secondTerm: single;
    thirdTerm: single;
    fourthTerm: single;
  begin
  try
  organicC_pct := 0.0;
  nonSiltContent_frn := 0.0;
  nonSandContent_frn := 0.0;
  firstTerm := 0.0;
  secondTerm := 0.0;
  thirdTerm := 0.0;
  fourthTerm := 0.0;
  nonSiltContent_frn := 1.0 - siltContent_pct * pct_to_frn;
  nonSandContent_frn := 1.0 - soilSandContent_pct * pct_to_frn;
  organicC_pct := Utils_OrganicCFromOrganicMatter_pct(organicMatter_tPha, soilWeight_tPha);
  firstTerm := 0.2 + 0.3 * safeExp(-0.0256 * soilSandContent_pct * nonSiltContent_frn);
  if (clayContent_pct + siltContent_pct = 0.0) then 
    begin
    secondTerm := 0.0;
    end
  else 
    begin
    secondTerm := power(safediv(siltContent_pct, clayContent_pct + siltContent_pct), 0.3);
    end;
  thirdTerm := 1.0 - 0.25 * scurve(organicC_pct, 3.72, 2.95);
  fourthTerm := 1.0 - 0.7 * scurve(nonSandContent_frn,  - 5.51,  - 22.9);
  result := firstTerm * secondTerm * thirdTerm * fourthTerm;
  except on e: Exception do result := errorMessage('Exception in EQ.SoilErodibilityFactor: ' + e.message); end;
  end;

{ 
The coarse fragment factor is estimated with the equation (Simanton et al.
1984) [EQN: 137] where ROK is the percent of coarse fragments in the surface
soil layer.
Equation:
  ROKF = exp(-0.03 * ROK)
Code:
  same
Variables:
  ROKF = EQ::CoarseFragmentFactor
  ROK = rockContentSurfaceSoilLayer_pct
 }
{ EQN: 137 }
class function EQ.CoarseFragmentFactor(rockContentSurfaceSoilLayer_pct: single): single;
  begin
  try
  result := safeExp(-0.03 * rockContentSurfaceSoilLayer_pct);
  except on e: Exception do result := errorMessage('Exception in EQ.CoarseFragmentFactor: ' + e.message); end;
  end;

{ 
                      EROSION - *WATER EROSION FROM IRRIGATION*

Erosion caused by applying irrigation water in furrows is estimated with
MUST [EQN: 138] where CE, the crop management factor, has a constant
value of 0.5. The volume of runoff is estimated as the product of the
irrigation volume applied and the irrigation runoff ratio. The peak
runoff rate is estimated for each furrow by using Mannings equation
and assuming that the flow depth is 0.75 of the ridge height and that
the furrow is triangular. If irrigation water is applied to land without
furrows, the peak runoff rate is estimated by dividing the runoff volume
by the duration (24 hrs).
Equation:
  Y = 2.5 * power(Q * q(p), 0.5) * K * CE * PE * LS
Code:
  Y = 11.8 * 1000 * power(Q * q(p), 0.56) * K * CE * PE * LS / distance_m
Variables:
  Y = EQ::WaterErosionFromIrrigation_tPha
  Q = furrowVolume_mm
  q(p) = peakRunoffRateForFurrow_m3Psec
  K = soilErodibilityFactor
  CE = kCropManagementFactor = 0.5
  PE = erosionControlPracticeFactor
  LS = slopeLengthAndSteepnessFactor
 }
const kCropManagementFactor: single = 0.5;

{ EQN: 138 }
class function EQ.WaterErosionFromIrrigation_tPha(furrowVolume_mm: single; peakRunoffRateForFurrow_m3Psec: single;
  soilErodibilityFactor: single; erosionControlPracticeFactor: single; slopeLengthAndSteepnessFactor: single; distance_m:
  single): single;
  begin
  try
  result := 1000 * 11.8 * kCropManagementFactor * soilErodibilityFactor * erosionControlPracticeFactor *
    slopeLengthAndSteepnessFactor * safediv(power(furrowVolume_mm * peakRunoffRateForFurrow_m3Psec, 0.56), distance_m);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterErosionFromIrrigation_tPha: ' + e.message); end;
  end;

class function EQ.IrrigationRunoffVolume_mm(irrigationVolume_mm: single; irrigationRunoffRatio: single): single;
  begin
  try
  result := irrigationVolume_mm * irrigationRunoffRatio;
  except on e: Exception do result := errorMessage('Exception in EQ.IrrigationRunoffVolume_mm: ' + e.message); end;
  end;

{ was not detailed in chapter }
class function EQ.IrrigationPeakRunoffRate_m3Psec(var params: SoilParamsStructure;
    var surface: SurfaceStructure; irrigationRunoffVolume_mm: single; var furrowVolume_mm: single;
    var distance_m: single): single;
  var
    first, second, third, flowDepth_m, intervalOverHeight: single;
  begin
  try
  first := 0.0;
  second := 0.0;
  third := 0.0;
  flowDepth_m := 0.0;
  intervalOverHeight := 0.0;
  if ((not params.autoIrrigationIsByFurrow) or (surface.ridgeHeight_mm <= 0.0)) then
    begin
    furrowVolume_mm := 0.00189;
    distance_m := params.watershedFieldLength_km * km_to_m;
    end
  else 
    begin
    { using furrow irrigation and have furrows }
    flowDepth_m := 0.75 * surface.ridgeHeight_mm * mm_to_m;
    intervalOverHeight := safediv(surface.ridgeInterval_m, surface.ridgeHeight_mm);
    first := sqr(flowDepth_m) * intervalOverHeight / 2.0;
    second := 2.0 * flowDepth_m * sqrt(1.0 + sqr(intervalOverHeight) / 4.0);
    third := power(safediv(first, second), 0.6667) * safediv(sqrt(params.watershedSlopeSteepness_mPm),
      params.manningsSurfaceRoughnessCoeff);
    furrowVolume_mm := first * third;
    distance_m := params.watershedFieldLength_km * km_to_m * surface.ridgeInterval_m;
    end;
  result := (irrigationRunoffVolume_mm * mm_to_m) * distance_m;
  except on e: Exception do result := errorMessage('Exception in EQ.IrrigationPeakRunoffRate_m3Psec: ' + e.message); end;
  end;

{ 
                             EROSION - *WIND EROSION*

The original EPIC wind erosion model (WEQ) required daily mean wind speed
as a driving variable. The new EPIC wind erosion model WECS (Wind Erosion
Continuous Simulation) requires the daily distribution of wind speed to take
advantage of the more mechanistic erosion equation. The new approach
estimates potential wind erosion for a smooth bare soil by integrating
the erosion equation through a day using the wind speed distribution.
Potential erosion is adjusted using four factors based on soil properties,
surface roughness, cover, and distance across the field in the wind
direction.

Basic Equation
The basic WECS wind erosion equation is [EQN: 139] where YW is the wind
erosion in kg/m, FI is the soil erodibility factor, FR is the surface
roughness factor, FV is the vegetative cover factor, FD is the mean
unsheltered travel distance of wind across a field, DW is the duration
of wind greater than the threshold velocity in sec, and YWR is the wind
erosion rate in kg/m*sec at time t.
Equation:
  YW = FI * FR * FV * FD * (integration from 0 to DW of) YWR
Code:
  YW = 8640.0 * FI * FR * FV * ROKF * (integration from 0 to DW of) (YWR * FD)
  note that FD is multiplied into the integrated YWR (see equation 141)
  also code bounds this aResult at maxWindErosionPerDay_tPha, an input
Variables:
  YW = EQ::WindErosion_tPha
  FI = soilErodibilityFactorWE
  FR = surfaceRoughnessFactorWE
  FV = vegetativeCoverFactorWE
  FD = meanUnsheltDistanceWE
  DW = durationOfWindOverThresholdVelocityWE_sec
  YWR = windErosionRateForFractionOfDay_kgPmsec
  ROKF = coarseFragmentFactorWE
 }
{ EQN: 139 }
class function EQ.WindErosion_tPha(soilErodibilityFactorWE: single; surfaceRoughnessFactorWE: single; vegetativeCoverFactorWE:
  single; coarseFragmentFactorWE: single; integratedWindErosion_tPha: single; maxWindErosionPerDay_tPha: single): single;
  var
    aResult: single;
  begin
  try
  aResult := 8640.0 * soilErodibilityFactorWE * surfaceRoughnessFactorWE * vegetativeCoverFactorWE *
    coarseFragmentFactorWE * integratedWindErosion_tPha;
  if (aResult > maxWindErosionPerDay_tPha) then 
    result := maxWindErosionPerDay_tPha
  else
    result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.WindErosion_tPha: ' + e.message); end;
  end;

{ 
The wind erosion rate is calculated with the equation of Skidmore(1986)
  YWR = C * (rho(a) / g) * (v*(o)2 - v*(tau)2 - 0.5 * (SW/WP)2)3/2
                                                                (Equation 140)
where C is a parameter ~~2.5, rho(a) is the air density in kg/m3, g is the
acceleration of gravity in m/s2, v*(o) is the friction velocity in m/sec,
and v*(tau) is the threshold friction velocity in m/sec. SW and WP are the
actual and 1500 kPa water contents of the top soil layer (10 mm thick),
respectively. The soil water term of the equation was developed by Chepil
(1956) and Skidmore (1986). Substituting acceleration of gravity (9.8 m/sec2)
and assuming air density is 1 kg/m3 gives [EQN: 141].
Equation:
  YWR = 0.255 * power(sqr(v*(o)) - sqr(v*(tau)) - 0.5 * sqr(SW/WP), 3/2)
Code:
  ustw = sqr(v*(tau)) + 0.5 * sqr(SW/WP) + 0.5 - tlmf
  YWR = 0.255 * power(sqr(v*(o)) - ustw, 3/2)
  YWR = 0.255 * power(sqr(v*(o)) - (sqr(v*(tau)) + 0.5 * sqr(SW/WP) + 0.5 - tlmf), 3/2)
  YWR = 0.255 * power(sqr(v*(o)) - sqr(v*(tau)) - 0.5 * sqr(SW/WP) - 0.5 + tlmf, 3/2)
  so the code adds a -0.5 and a tlmf term to the part inside the power
  code also adds meanUnsheltDistanceFactorWE multiplier (FD), but does not include it
    in the final equation (so it works out, see equation 139)
  code also adds check: aResult is 0 if v*(o) - ustw < 0
Variables:
  YWR = EQ::WindErosionRateForFractionOfDay_kgPmsec
  v*(o) = frictionVelocityWE_mPsec
  ustw = weForFractionOfDayFactor
  FD = meanUnsheltDistanceFactorWE
 }
{ EQN: 141 }
class function EQ.WindErosionRateForFractionOfDay_kgPmsec(frictionVelocityWE_mPsec: single; weForFractionOfDayFactor: single;
  meanUnsheltDistanceFactorWE: single): single;
  var
    difference: single;
  begin
  try
  difference := sqr(frictionVelocityWE_mPsec) - weForFractionOfDayFactor;
  if (difference < 0.0) then 
    result := 0.0
  else
    result := 0.255 * power(difference, 1.5) * meanUnsheltDistanceFactorWE;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.WindErosionRateForFractionOfDay_kgPmsec: ' + e.message); end;
  end;

{ not in chapter }
{ 
Variables:
  ustw = EQ::WindErosionForFractionOfDayFactor
  v*(tau) = thresholdFrictionVelocityWE_mPsec
  SW/WP = topLayerSoilWaterOverWiltingPoint
  tlmf = windErosionAccumulatedFactor
 }
class function EQ.WindErosionForFractionOfDayFactor(thresholdFrictionVelocityWE_mPsec: single;
  topLayerSoilWaterOverWiltingPoint: single; windErosionAccumulatedFactor: single): single;
  begin
  try
  result := sqr(thresholdFrictionVelocityWE_mPsec) + 0.5 * sqr(topLayerSoilWaterOverWiltingPoint) + 0.5 -
    windErosionAccumulatedFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.WindErosionForFractionOfDayFactor: ' + e.message); end;
  end;

{ 
The friction velocity is estimated with the equation
  v*(o) = K * v / (log(Z/Z(o))                                  (Equation 142)
where K is Von Karmans constant ~~0.4, v is the wind speed in m/sec at
height Z (10 m for WECS input), and Z(o) is the aerodynamic roughness
(0.00055 m used in WECS). Substituting the WECS values into equation 142
gives [EQN: 143].
Equation:
  v(o) = 0.0408 * v
Code:
  same
Variables:
  v*(o) = FrictionVelocityWE_mPsec
  v = windSpeedForFractionOfDay_mPsec
 }
{ EQN: 143 }
class function EQ.FrictionVelocityWE_mPsec(windSpeedForFractionOfDay_mPsec: single): single;
  begin
  try
  result := 0.0408 * windSpeedForFractionOfDay_mPsec;
  except on e: Exception do result := errorMessage('Exception in EQ.FrictionVelocityWE_mPsec: ' + e.message); end;
  end;

{ 
The threshold friction velocity is estimated with the equation
  v*(tau) = 0.1 * sqrt(g * (delta-p/rho(a)) * D)                (Equation 144)
where delta-p is the mineral soil density (2650 kg/m3) and D is the soil
particle size in m. Substituting constants into equation 144 and expressing
D in micrometers gives [EQN: 145].
Equation:
  v*(tau) = 0.0161 * sqrt(D)
Code:
  same
Variables:
  v*(tau) = EQ::ThresholdFrictionVelocityWE_mPsec
  D = soilParticleDiameter_microns
 }
{ EQN: 145 }
class function EQ.ThresholdFrictionVelocityWE_mPsec(soilParticleDiameter_microns: single): single;
  begin
  try
  result := 0.0161 * sqrt(soilParticleDiameter_microns);
  except on e: Exception do result := errorMessage('Exception in EQ.ThresholdFrictionVelocityWE_mPsec: ' + e.message); end;
  end;

{ 
Soil Erodibility Factor
WECS uses the soil erodibility concept of WEQ expressed in dimensionless form
with the equation [EQN: 146] where I is the soil erodibility factor of the
Woodruff and Siddoway (1965) model in t/ha and FI is the dimensionless
soil erodibility factor of the new model.
Equation:
  FI = I / 695.0
Code:
  based on soil sand, silt and clay
  above equation may be the same. do not know how I is derived.
Variables:
  FI = SoilErodibilityFactorWE
  I = soilErodibilityFactorWEWoodruffAndSiddoway_tPha
 }
{ EQN: 146 }
class function EQ.SoilErodibilityFactorWE(sand_pct: single; clay_pct: single; silt_pct: single; caco3_pct: single): single;
  begin
  try
  { primarily sandy }
  if (sand_pct > 85.0 + 0.5 * clay_pct) then 
    result := 1.0
  else if (sand_pct > 70.0 + clay_pct) then
    result := 0.43
  { primarily silty }
  else if ((silt_pct > 80.0) and (clay_pct < 12.0)) then
    result := 0.12
  { calcareous soil }
  else if (caco3_pct > 0.0) then
    begin
    if (((sand_pct < 45.0) or (clay_pct < 20.0)) or (silt_pct > 28.0)) then 
      result := 0.28
    else
      result := 0.18;
    end
  { non-calcareous soil, lowest clay }
  else if (clay_pct < 7.0) then
    begin
    if (silt_pct < 50.0) then 
      result := 0.28
    else
      result := 0.18;
    end
  { non-calcareous soil, low clay }
 else if (clay_pct < 20.0) then
    begin
    if (sand_pct > 52.0) then 
      result := 0.28
    else
      result := 0.18;
    end
  { non-calcareous soil, medium clay }
  else if (clay_pct < 27.0) then
    begin
    if (silt_pct < 28.0) then 
      result := 0.18
    else
      result := 0.16;
    end
  { non-calcareous soil, high clay }
  else if (clay_pct < 35.0) then
    begin
    if (sand_pct < 20.0) then 
      result := 0.12
    else if (sand_pct < 45.0) then
      result := 0.16
    else
      result := 0.18;
    end
  { non-calcareous soil, highest clay }
  else if (sand_pct > 45.0) then
    result := 0.18
  else
    result := 0.28;
  except on e: Exception do result := errorMessage('Exception in EQ.SoilErodibilityFactorWE: ' + e.message); end;
  end;

{ 
Roughness Factor
The surface roughness factor (FR) is based upon the shelter angle developed
by Potter et al. (1990). This roughness index calculates the erodible
fraction of the soil surface by estimating the portion susceptible to
abrasion by saltating particles. The shelter angle index incorporates
both roughness due to random cloddiness and oriented roughness (ridges)
due to tillage operations. The effect of oriented roughness varies as
a function of wind direction, which is selected each day so that the
statistical distribution of wind direction approaches that of the simulation
site. FR is estimated with the equation [EQN: 147] where wn(1) is the
descent angle of saltating sand grains (about 15 degrees from horizontal).
A 15 degree impact angle has also been shown to cause maximum aggregate
abrasion (Hagen et al., 1988).
Equation:
  FR = 1.0 - exp(-power(wn(1) / RFB, RFC))
  wn(1) = 15 degrees
Code:
  same except code uses 5.0 for wn(1)
Variables:
  FR = EQ::SurfaceRoughnessFactorWE
  wn(1) = 5.0
  RFB = coeffSurfaceRoughnessFactorWE
  RFC = expSurfaceRoughnessFactorWE
 }
{ EQN: 147 }
class function EQ.SurfaceRoughnessFactorWE(coeffSurfaceRoughnessFactorWE: single; expSurfaceRoughnessFactorWE: single):
  single;
  begin
  try
  result := 1.0 - safeExp(-power(safediv(5.0, coeffSurfaceRoughnessFactorWE), expSurfaceRoughnessFactorWE));
  except on e: Exception do result := errorMessage('Exception in EQ.SurfaceRoughnessFactorWE: ' + e.message); end;
  end;

{ 
The coefficient RFC is calculated with the equation [EQN: 148] where
RHTT is the ridge height in mm.
Equation:
  RFC = 0.77 * power(1.002, RHTT)
Code:
  same
Variables:
  RFC = ExpSurfaceRoughnessFactorWE
  RHTT = ridgeHeight_mm
 }
{ EQN: 148 }
class function EQ.ExpSurfaceRoughnessFactorWE(ridgeHeight_mm: single): single;
  begin
  try
  result := 0.77 * power(1.002, ridgeHeight_mm);
  except on e: Exception do result := errorMessage('Exception in EQ.ExpSurfaceRoughnessFactorWE: ' + e.message); end;
  end;

{ 
The coefficient RFB is estimated with the equations [EQN: 149], [EQN: 150]
and [EQN: 151] where RRF is the clod roughness factor, RRUF is the random
roughness in mm, RIF is the ridge roughness factor, and wn(2) is the angle
of the wind relative to ridges. Both RRUF and RHTT are altered by wind
and water erosion and tillage.
Equation:
  RFB = RRF + RIF
Code:
  same except for lower bound at 1.0
Variables:
  RFB = EQ::CoeffSurfaceRoughnessFactorWE
  RRF = clodRoughnessFactorWE
  RIF = ridgeRoughnessFactorWE
 }
{ EQN: 149 }
class function EQ.CoeffSurfaceRoughnessFactorWE(clodRoughnessFactorWE: single; ridgeRoughnessFactorWE: single): single;
  var
    aResult: single;
  begin
  try
  aResult := clodRoughnessFactorWE + ridgeRoughnessFactorWE;
  if (aResult < 1.0) then 
    result := 1.0
  else
    result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.CoeffSurfaceRoughnessFactorWE: ' + e.message); end;
  end;

{ 
Equation:
  RRF = 11.9 * (1.0 - exp(-power(RRUF / 9.8, 1.3)))
Code:
  same
Variables:
  RRF = EQ::ClodRoughnessFactorWE
  RRUF = randomRoughnessWE_mm
 }
{ EQN: 150 }
class function EQ.ClodRoughnessFactorWE(randomRoughnessWE_mm: single): single;
  begin
  try
  result := 11.9 * (1.0 - safeExp(-power(randomRoughnessWE_mm / 9.8, 1.3)));
  except on e: Exception do result := errorMessage('Exception in EQ.ClodRoughnessFactorWE: ' + e.message); end;
  end;

{ 
Equation:
  RIF = abs(sin(wn(2) * 1.27 * power(RHTT, 0.52))
Code:
Variables:
  RIF = EQ::RidgeRoughnessFactorWE
  wn(2) = angleOfWindRelativeToRidges_rad
  RHTT = ridgeHeight_mm
 }
{ EQN: 151 }
class function EQ.RidgeRoughnessFactorWE(angleOfWindRelativeToRidges_rad: single; ridgeHeight_mm: single): single;
  begin
  try
  result := abs(sin(angleOfWindRelativeToRidges_rad) * 1.27 * power(ridgeHeight_mm, 0.52));
  except on e: Exception do result := errorMessage('Exception in EQ.RidgeRoughnessFactorWE: ' + e.message); end;
  end;

{ 
Vegetative Cover Factor
The vegetative cover factor is based on the approach used in the original
EPIC model. A vegetative cover equivalent factor is simulated daily as a
class function of standing live biomass, standing dead residue, and flat crop
residue [EQN: 152] where VE is the vegetative cover equivalent factor,
SB is the standing biomass in t/ha, SR is the standing crop residue in
t/ha, FR is the flat residue in t/ha, and w(1), w(2) and w(3) are crop
specific coefficients.
Equation:
  VE = 0.253 * power((w(1) * SB + w(2) * SR + w(3) * FR), 1.36)
Code:
  VE = w(1) * SB + w(2) * SR + w(3) * FR
Variables:
  VE = EQ::VegCoverEquivFactorWE
  SB = standingLiveBiomass_tPha
  SR = standingDeadResidue_tPha
  FR = surfaceLayerFlatCropResidue_tPha
  w(1) = windErosionFactorStandingLive
  w(2) = windErosionFactorStandingDead
  w(3) = windErosionFactorFlatResidue
 }
{ EQN: 152 }
class function EQ.VegCoverEquivFactorWE(standingLiveBiomass_tPha: single; standingDeadResidue_tPha: single;
  surfaceLayerFlatCropResidue_tPha: single; windErosionFactorStandingLive: single; windErosionFactorStandingDead: single;
  windErosionFactorFlatResidue: single): single;
  begin
  try
  result := windErosionFactorStandingLive * standingLiveBiomass_tPha + windErosionFactorStandingDead *
    standingDeadResidue_tPha + windErosionFactorFlatResidue * surfaceLayerFlatCropResidue_tPha;
  except on e: Exception do result := errorMessage('Exception in EQ.VegCoverEquivFactorWE: ' + e.message); end;
  end;

{ 
The vegetative equivalent is converted to a vegetative factor for use in the
new model [EQN: 153]. Equation 153 assures that 0.0 <= FV <= 1.0.
Equation:
  FV = VE / (VE + exp(0.48 - 1.32 * VE))
Code:
  FV = 1.0 - VE / (VE + exp(0.48 - 1.32 * VE))
Variables:
  FV = EQ::VegetativeCoverFactorWE_frn
  VE = vegCoverEquivFactorWE
 }
{ EQN: 153 }
class function EQ.VegetativeCoverFactorWE_frn(vegCoverEquivFactorWE: single): single;
  begin
  try
  result := 1.0 - scurve(vegCoverEquivFactorWE, 0.48, 1.32);
  except on e: Exception do result := errorMessage('Exception in EQ.VegetativeCoverFactorWE_frn: ' + e.message); end;
  end;

{ 
Unsheltered Distance Factor
Field length along the prevailing wind direction is calculated as in the
original model (Cole at al., 1982) by considering the field dimensions and
orientation and the wind direction. [EQN: 154] where WL is the unsheltered
field length along the prevailing wind direction in m, FL is the field
length in m, FW is the field width in m, theta is the wind direction
clockwise from north in radians, and phi is the clockwise angle between
field length and north in radians.
Equation:
  WL = FL * FW / (FL * cos(pi/2 + theta - phi) + FW * sin(pi/2 + theta - phi))
Code:
  same
Variables:
  WL = UnsheltFieldLengthAlongPrevailingWindDir_m
  FL = fieldLength_m
  FW = fieldWidth_m
  theta = windDirectionForDay_rad
  phi = fieldLengthOrientationFromNorth_rad
 }
class function EQ.UnsheltFieldLengthAlongPrevailingWindDir_m(fieldLength_m: single; fieldWidth_m: single;
  angleOfWindRelativeToRidges_rad: single): single;
  var
    cosAngle: single;
    sinAngle: single;
  begin
  try
  cosAngle := abs(cos(angleOfWindRelativeToRidges_rad));
  sinAngle := abs(sin(angleOfWindRelativeToRidges_rad));
  result := safediv(fieldLength_m * fieldWidth_m, fieldLength_m * cosAngle + fieldWidth_m * sinAngle);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.UnsheltFieldLengthAlongPrevailingWindDir_m: ' + e.message); end;
  end;

class function EQ.AngleOfWindRelativeToRidges_rad(windDirectionFromNorth_rad: single; fieldLengthOrientationFromNorth_rad:
  single): single;
  var
    aResult: single;
  begin
  try
  aResult := kPi / 2 + windDirectionFromNorth_rad - fieldLengthOrientationFromNorth_rad;
  if (aResult < 0.0) then 
    begin
    result := aResult + kPi;
    exit;
    end;
  if (aResult > kPi) then 
    begin
    result := aResult - kPi;
    exit;
    end;
  result := aResult;
  except on e: Exception do result := errorMessage('Exception in EQ.AngleOfWindRelativeToRidges_rad: ' + e.message); end;
  end;

{ 
The new model distance factor (FD) is calculated as described by Stout (1990)
using the equation [EQN: 155] where wn(3) is a parameter determined
experimentally to lie in the range 50.0 < wn(3) < 90.0. A value of 70.0
is used in EPIC.
Equation:
  FD = 1.0 - exp(-WL / wn(3))
  wn(3) = 70.0
Code:
  same except numerator is 0.07, not 70
Variables:
  FD = EQ::MeanUnsheltDistanceFactorWE
  WL = unsheltFieldLengthAlongPrevailingWindDir_m
  wn(3) = 70.0
 }
{ EQN: 155 }
class function EQ.MeanUnsheltDistanceFactorWE(unsheltFieldLengthAlongPrevailingWindDir_m: single): single;
  begin
  try
  result := 1.0 - safeExp(-unsheltFieldLengthAlongPrevailingWindDir_m / (70.0 * 0.001));
  except on e: Exception do result := errorMessage('Exception in EQ.MeanUnsheltDistanceFactorWE: ' + e.message); end;
  end;

{ according to the code, the coarse fragment factors for wind erosion and water erosion }
{ are slightly different. Here is the one for wind erosion. }
class function EQ.CoarseFragmentFactorWE(surfaceLayerRockContent_pct: single): single;
  begin
  try
  result := safeExp(-0.047 * surfaceLayerRockContent_pct);
  except on e: Exception do result := errorMessage('Exception in EQ.CoarseFragmentFactorWE: ' + e.message); end;
  end;

{ Wind speed integration explained and done in EP_Soil.cp function }
{ EP_IntegratePotentialWindErosion. }

{ was part of ieqwater.pas ========================================================================================== }

{
                                   *SOIL TEMPERATURE*

Main equation

Daily average soil temperature at the center of each soil layer is
simulated for use in nutrient cycling and hydrology. The basic soil
temperature equation is [EQN: 240] where T is the soil temperature
at the center of layer l on day i in degrees C, LAG is a coefficient
ranging from 0.0 to 1.0 that allows proper weighting of yesterday's
temperature, Tbar is the long-term average annual air temperature
at the site, TG is the soil surface temperature, and FZ is a depth
factor. Thus, given yesterday's temperature, equation 240 estimates
today's temperature as a function of soil surface temperature, depth,
and a lag coefficient. It is assumed that the temperature remains
almost constant at some depth called damping depth and is approximately
Tbar.
Equation:
  T = LAG * T(i-1) + (1.0 - LAG) * (FZ * (Tbar + TG) + TG)
Code:
  T = LAG * T(i-1) + (1.0 - LAG) * (FZ * (Tbar - TG) + TG)
  note difference of sign
Variables:
  T = EQ::SoilTempAtCenterForLayer_degC
  T(i-1) = yesterdaysSoilTempAtCenter_degC
  LAG = soilSurfaceCoverLagFactor_frn
  FZ = soilTempDepthWtFactor
  Tbar = meanMonthlyMeanMeanTempForYear_degC
  TG = soilSurfaceTempWithCover_degC
 }
{ EQN: 240 }
class function EQ.SoilTempAtCenterForLayer_degC(yesterdaysSoilTempAtCenter_degC: single; soilSurfaceCoverLagFactor_frn:
  single; soilTempDepthWtFactor: single; meanMonthlyMeanMeanTempForYear_degC: single; soilSurfaceTempWithCover_degC:
  single): single;
  begin
  try
  result := soilSurfaceCoverLagFactor_frn * yesterdaysSoilTempAtCenter_degC + (1.0 - soilSurfaceCoverLagFactor_frn) *
    (soilTempDepthWtFactor * (meanMonthlyMeanMeanTempForYear_degC - soilSurfaceTempWithCover_degC) +
    soilSurfaceTempWithCover_degC);
  except on e: Exception do result := errorMessage('Exception in EQ.SoilTempAtCenterForLayer_degC: ' + e.message); end;
  end;

{ 
Depth weighting factor

The depth weighting factor governs temperature changes between the soil
surface and the damping depth according to the equation [EQN: 241] where
[EQN: 242] where Z is soil depth from the surface in m and DD is the
damping depth in m. Obviously, equations 241 and 242 make near surface
temperatures a strong function of TG. As depth increases, Tbar has
more influence until finally at the damping depth, the temperature is
within 5% of Tbar.
Equation:
  ZD = (Z(l) + Z(l-1)) / (2.0 * DD)
  FZ = ZD / (ZD + exp(-0.867 - 2.08 * ZD))
Code:
  same
Variables:
  FZ = SoilTempDepthWtFactorForLayer
  ZD = intermediateFactor
  Z(l) = depth_m
  Z(l-1) = depthUpperLayer_m
  DD = dampingDepthForSoilTemp_m
 }
{ EQN: 241 EQN: 242 }
class function EQ.SoilTempDepthWtFactorForLayer(depth_m: single; depthUpperLayer_m: single; dampingDepthForSoilTemp_m:
  single): single;
  var
    intermediateFactor: single;
  begin
  try
  intermediateFactor := safediv(depth_m + depthUpperLayer_m, 2.0 * dampingDepthForSoilTemp_m);
  result := scurve(intermediateFactor,  - 0.867, 2.08);
  except on e: Exception do result := errorMessage('Exception in EQ.SoilTempDepthWtFactorForLayer: ' + e.message); end;
  end;

{
Damping depth

The damping depth is a function of soil bulk density and water
content as expressed in the equations [EQN: 243], [EQN: 244] and
[EQN: 245] where DP is the maximum damping depth for soil in m,
BD is the soil bulk density in t/m3, Z(M) is the distance from the
bottom of the lowest soil layer to the surface in m, and squiggle
is a scaling parameter.
Equation:
  DP = 1.0 + 2.5 * BD / (BD + exp(6.53 - 5.63 * BD))
Code:
  code has
  DP = 1.0 + 2.5 * BD / (BD + 686 * (exp-5.63 * BD))
  since exp(6.53) = 686 and e(a) * e(b) = e(a+b) this is the same
Variables:
  DP = EQ::MaxDampingDepthForSoilTemp_m
  BD = patchMeanBulkDensity_tPm3
 }
{ EQN: 243 }
class function EQ.MaxDampingDepthForSoilTemp_m(patchMeanBulkDensity_tPm3: single): single;
  begin
  try
  result := 1.0 + 2.5 * scurve(patchMeanBulkDensity_tPm3, 6.53, 5.63);
  except on e: Exception do result := errorMessage('Exception in EQ.MaxDampingDepthForSoilTemp_m: ' + e.message); end;
  end;

{ 
Equation:
  squiggle = 0.001 * SW / ((0.356 - 0.144 * BD) * Z(M))
Code:
  same
Variables:
  squiggle = EQ::ScalingParamForDampingDepthForSoilTemp
  SW = patchTotalSoilWaterContent_mm
  BD = patchMeanBulkDensity_tPm3
  Z(M) = depthOfLowestLayer_m
 }
{ EQN: 244 }
class function EQ.ScalingParamForDampingDepthForSoilTemp(patchTotalSoilWaterContent_mm: single; patchMeanBulkDensity_tPm3:
  single; depthOfLowestLayer_m: single): single;
  begin
  try
  result := 0.001 * safediv(patchTotalSoilWaterContent_mm, (0.356 - 0.144 * patchMeanBulkDensity_tPm3) *
    depthOfLowestLayer_m);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ScalingParamForDampingDepthForSoilTemp: ' + e.message); end;
  end;

{ 
Equation:
  DD = DP * exp(ln(0.5/DP) * sqr((1 - squiggle) / (1 + squiggle)))
Code:
  same
Variables:
  DD = EQ::DampingDepthForSoilTemp_m
  DP = maxDampingDepthForSoilTemp_m
  squiggle = scalingParam
 }
{ EQN: 245 }
class function EQ.DampingDepthForSoilTemp_m(maxDampingDepthForSoilTemp_m: single; scalingParam: single): single;
  begin
  try
  result := maxDampingDepthForSoilTemp_m * safeExp(safeLn(safediv(0.5, maxDampingDepthForSoilTemp_m)) * sqr(safediv(1.0 -
    scalingParam, 1.0 + scalingParam)));
  except on e: Exception do result := errorMessage('Exception in EQ.DampingDepthForSoilTemp_m: ' + e.message); end;
  end;

{ 
Soil surface temperature - bare soil

To complete the solution of equation 240, the soil surface temperature
must be estimated. The first step is to estimate the bare soil surface
temperature (TGB). Of course, TBG is usually closely related to air
temperature. Other important factors that also influence TGB are
precipitation, solar radiation, and previous soil temperature. When
precipitation occurs, the soil surface temperature usually decreases.
Thus, the appropriate air temperature for estimating TGB is near the
daily minimum
  TGBW = T(mn) + omega(s) * (T(mx) - T(mn))              (Equation 246)
where TGBW is the bare soil surface temperature on wet day i, and
omega(s) is a scaling factor to adjust for wet days. The value of
omega(s) ranges from 0.0 to 1.0, but more realistic results can
be obtained using omega(s) ~~ 0.1.

The companion equation for dry days is derived from the continuity
equation (equation 103)
  TGBD = T - PW * (T(mn) + omega(s) * (T(mx) - T(mn))) / (1.0 - PW)
                                                        (Equation 247)
where TGBD is the bare soil surface temperature on dry day i, and
PW is the probability of a wet day in month k.

The soil surface temperature is also affected by solar radiation.
Therefore, a net radiation term is added to equations 246 and
247, giving [EQN: 248] and [EQN: 249] where RAH is the radiation
factor.
Equation:
  TGBW = T(mn) + omega(s) * (T(mx) - T(mn)) + RAH
Code:
  TGBW = T + NWD/ND * (T(mx) - T(mn))
  uses probWetDayForMonth_frn (NWD/ND) as omega(s)
  uses mean temp in place of max temp
  does not add RAH at the end
Variables:
  TGBW = EQ::BareSoilSurfaceTempWetDay_degC
  omega(s) = probWetDayForMonth_frn
 }
{ EQN: 248 }
class function EQ.BareSoilSurfaceTempWetDay_degC(meanTempForDay_degC: single; minTempForDay_degC: single;
  probWetDayForMonth_frn: single): single;
  begin
  try
  result := probWetDayForMonth_frn * (meanTempForDay_degC - minTempForDay_degC) + minTempForDay_degC;
  except on e: Exception do result := errorMessage('Exception in EQ.BareSoilSurfaceTempWetDay_degC: ' + e.message); end;
  end;

{ 
Equation:
  TGBD = (T - PW * (T(mn) + omega(s) * (T(mx) - T(mn)))) / (1.0 - PW) + RAH
Code:
  TGBD = PW * (T(mx) - T) + T + RAH
  quite different. cannot explain.
Variables:
  TGBD = BareSoilSurfaceTempDryDay_degC
  T(mx) = meanMaxTempForDay_degC
  T(mn) = meanMinTempForDay_degC
  T = meanTempForDay_degC
  RAH = bareSoilSurfaceTempRadiationFactor
 }
{ EQN: 249 }
class function EQ.BareSoilSurfaceTempDryDay_degC(maxTempForDay_degC: single; meanTempForDay_degC: single;
  bareSoilSurfaceTempRadiationFactor: single; probWetDayForMonth_frn: single): single;
  begin
  try
  result := probWetDayForMonth_frn * (maxTempForDay_degC - meanTempForDay_degC) + meanTempForDay_degC +
    bareSoilSurfaceTempRadiationFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.BareSoilSurfaceTempDryDay_degC: ' + e.message); end;
  end;

{ 
Radiation factor for bare soil surface temperature

The radiation factor RAH is estimated with the equation [EQN: 250] where
solar radiation, RA, albedo, AB, and the cover factor, EA are discussed
and evaluated in the Evapotranspiration section. Equation 250 increases
surface soil temperatures when radiation is high but also makes adjustments
for albedo and cover. Low radiation can also cause up to a two degree C
drop in surface temperature.
Equation:
  RAH = 2.0 * (sqr((RA * (1.0 - AB) * EA) / 15.0) - 1.0)
Code:
  same
Variables:
  RAH = EQ::BareSoilSurfaceTempRadiationFactor
  RA = radiationForDay_MJPm2
  AB = albedo_frn
  EA = soilCoverIndex_frn
 }
{ EQN: 250 }
class function EQ.BareSoilSurfaceTempRadiationFactor(radiationForDay_MJPm2: single; albedo_frn: single; soilCoverIndex_frn:
  single): single;
  var
    numerator: single;
  begin
  try
  numerator := radiationForDay_MJPm2 * (1.0 - albedo_frn) * soilCoverIndex_frn;
  result := 2.0 * (sqr(numerator / 15.0) - 1.0);
  except on e: Exception do result := errorMessage('Exception in EQ.BareSoilSurfaceTempRadiationFactor: ' + e.message); end;
  end;

{ 
Soil surface temperature - nonbare soil (crop residue or snow)

If the soil surface is not bare, the surface temperature can be
affected considerably by the amount of cover (crop residue or snow).
This effect can be simulated by combining the estimated bare
surface temperature for the day with the previous day's temperature
in the second soil layer (the top 10 mm layer is considered too
thin for this purpose) [EQN: 251] where TG is the final estimate
of soil surface temperature in degrees C and bcv is a lagging factor
for simulating residue and snow cover effects on surface temperature.
Equation:
  TG = bcv * T(2) + (1.0 - bcv) * TGB*
Code:
  same
Variables:
  TG = EQ::SurfaceLayerSoilTemp_degC
  bcv = soilSurfaceTempCoverLagFactor_frn
  T(2) = yesterdaysSoilTempInSecondLayer_degC
  TGB* = soilSurfaceTempBare_degC
 }
{ EQN: 251 }
class function EQ.SurfaceLayerSoilTemp_degC(yesterdaysSoilTempInSecondLayer_degC: single; soilSurfaceCoverLagFactor_frn:
  single; soilSurfaceTempBare_degC: single): single;
  begin
  try
  result := soilSurfaceCoverLagFactor_frn * yesterdaysSoilTempInSecondLayer_degC + (1.0 -
    soilSurfaceCoverLagFactor_frn) * soilSurfaceTempBare_degC;
  except on e: Exception do result := errorMessage('Exception in EQ.SurfaceLayerSoilTemp_degC: ' + e.message); end;
  end;

{ 
Lag factor for nonbare soil

The value of bcv is 0 for bare soil and approaches 1.0 as cover increases,
as expressed in the equation [EQN: 252] where CV is the sum of above
ground biomass and crop residue in t/ha and SNO is the water content
of the snow cover in mm.
Equation:
  bcv = max( CV / (CV + exp(5.34 + 2.4 * CV)), SNO / (SNO + exp(2.303 - 0.2197 * SNO)) )
Code:
  bcv = max( CV / (CV + exp(5.34 - 2.4 * CV)), SNO / (SNO + exp(2.303 - 0.2197 * SNO)) )
  note sign difference
  in code, CV part is commented out and also is 0 if CV > 10
  in code, aResult is multiplied by 0.95
  Used CV part with > 0 instead of < 10, and changed sign to + as in chapter.
Variables:
  bcv = EQ::SoilSurfaceTempCoverLagFactor_frn
  CV = aboveGroundBiomassAndResidue_tPha
  SNO = snowWaterContent_mm
 }
{ EQN: 252 }
class function EQ.SoilSurfaceTempCoverLagFactor_frn(aboveGroundBiomassAndResidue_tPha: single; snowWaterContent_mm: single;
  var soilInsulationFromAirTempCoeffs: SCurveStructure): single;
  var
    forCover: single;
    forSnow: single;
  begin
  try
  forCover := 0.0;
  forSnow := 0.0;
  if (aboveGroundBiomassAndResidue_tPha < 10.0) then 
    forCover := scurve(aboveGroundBiomassAndResidue_tPha, soilInsulationFromAirTempCoeffs.c1,
      -soilInsulationFromAirTempCoeffs.c2)
  else
    forCover := 1.0;
  if (snowWaterContent_mm > 0.0) then
    forSnow := scurve(snowWaterContent_mm, 2.303, 0.2197)
  else
    forSnow := 0.0;
  result := max(forCover, forSnow) * 0.95;
  except on e: Exception do result := errorMessage('Exception in EQ.SoilSurfaceTempCoverLagFactor_frn: ' + e.message); end;
  end;

{                                      *TILLAGE*

Each tillage operation
  mixes some materials between the soil layers
  changes the bulk density
  converts standing residue to flat residue
  changes ridge height
  changes surface roughness (to input value)

Each rainfall event (day in which it rains)
  changes the bulk density due to settling

The EPIC tillage component was designed to mix nutrients and
crop residues within the plow depth, simulate the change in
bulk density, and convert standing residue to flat residue.
Other functions of the tillage component include simulating
ridge height and surface roughness.

Each tillage operation is assigned a mixing efficiency (0-1).
The tillage mixing equation is [EQN: 323] where X is the amount
of material in layer l after mixing in kg/ha, EF is the mixing
efficiency of the tillage operation, X(o) is the amount of
the material before mixing in kg/ha, and M is the number of soil
layers in the plow depth, PD, in m.
// EQN: 323
// this is done in the code, and is roughly the same

The change in bulk density in the plow layer is simulated for each
tillage operation by using the equation [EQN: 324] where BDP is
the bulk density in soil layer l after tillage, BDP(o) is the bulk
density before tillage, and BD(o) is the bulk density of the soil
when it has completely settled after tillage.
Equation:
  BDP = BDP(o) - (BDP(o) - 2/3 * BD(o)) * EF
Code:
  same
Variables:
  BDP = EQ::BulkDensityAfterTillageForLayer_tPm3
  BDP(o) = bulkDensity_tPm3
  BD(o) = bulkDensitySettled_tPm3
  EF = mixingEfficiency_frn
 }
{ EQN: 324 }
class function EQ.BulkDensityAfterTillageForLayer_tPm3(bulkDensity_tPm3: single; bulkDensitySettled_tPm3: single;
  mixingEfficiency_frn: single): single;
  begin
  try
  result := bulkDensity_tPm3 - (bulkDensity_tPm3 - (2.0 / 3.0) * bulkDensitySettled_tPm3) * mixingEfficiency_frn;
  except on e: Exception do result := errorMessage('Exception in EQ.BulkDensityAfterTillageForLayer_tPm3: ' + e.message); end;
  end;

{ 
Between tillage operations, the soil settles with each rainfall
event (if it rains on a day) according to the equations [EQN: 325]
and [EQN: 326] where SZ is a scaling factor for soil layer l, O(l-1)
is the percolation rate into the layer in mm (R-Q for the top layer)
and SAN is the percentage of sand in the layer. The exponential
parameters of equation 326 are set to give 0.10 * (BD - BDP(i-1))
when SZ = 5.0 and 0.95 * (BD-BDP(i-1)) when SZ = 100.0. Thus, near
the surface soils with little sand will almost completely settle
with 100 mm percolation. Equations 325 and 326 cause fast settling
when rainfall is large and soils are sandy and have been tilled
recently. Also, settling is much faster near the surface (this
allows simulation of long-term deep chiseling effects). Of course,
settling is relatively slow for soils low in sand content, especially
in low rainfall areas.
Equation:
  SZ = O(l-1) / power(Z, 0.6) * (1.0 + 2.0 * SAN / (SAN + exp(8.597 - 0.075 * SAN)))
  BDP = BPD(i-1) + (BD - BDP(i-1)) * (SZ / (SZ + exp(3.92 - 0.0226 * SZ)))
Code:
  SZ = O(l-1) / power(Z, 0.6) * 0.2 * (1.0 + 2.0 * SAN / (SAN + exp(8.597 - 0.075 * SAN)))
  otherwise the same
Variables:
  BDP = EQ::BulkDensityAfterSettlingFromRain_tPm3
  BDP(i-1) = bulkDensity_tPm3
  SZ = settlingScalingFactor
  R = rainfallForDay_mm
  Q = runoffVolume_mm
  O(l-1) = waterEnteringLayer_mm
  Z = depth_m
  SAN = sandContent_pct
  BD = settledBulkDensity_tPm3
 }
{ EQN: 325 EQN: 326 }
class procedure EQ.BulkDensityAfterSettlingFromRain_tPm3(numLayers: integer; var layers: LayerStructureArray;
  rainfallForDay_mm: single; runoffVolume_mm: single; maxTillageDepth_m: single; var
  soilSettlingFromRainfallCoeffs: SCurveStructure);
  var
    layer: integer;
    scalingFactor, waterEnteringLayer_mm: single;
  begin
  try
  layer := 0;
  scalingFactor := 0.0;
  waterEnteringLayer_mm := rainfallForDay_mm - runoffVolume_mm;
  { for each layer, increase bulk density due to percolation }
  while (layer <= numLayers - 1) and (layers[layer].depth_m < maxTillageDepth_m) do
    begin
    if (waterEnteringLayer_mm > 0.0) then 
      begin
      scalingFactor := safediv(waterEnteringLayer_mm, power(layers[layer].depth_m, 0.6)) * 0.2 * (1.0 + 2.0 *
        scurve(layers[layer].sandContent_pct, 8.597, 0.075));
      layers[layer].bulkDensity_tPm3 := layers[layer].bulkDensity_tPm3 + ((layers[layer].settledBulkDensity_tPm3 -
        layers[layer].bulkDensity_tPm3) * scurve(scalingFactor, soilSettlingFromRainfallCoeffs.c1,
        soilSettlingFromRainfallCoeffs.c2));
      end;
    waterEnteringLayer_mm := layers[layer].percolation_mm;
    inc(layer);
    end;
  except on e: Exception do errorMessage('Exception in EQ.BulkDensityAfterSettlingFromRain_tPm3: ' + e.message); end;
  end;

{ 
Another important function of the tillage model, converting standing
residue to flat residue, is accomplished with the equation [EQN: 327]
where SR(o) and SR are the standing residue weights before and after
tillage in t/ha and PD is the plow depth in m.
Equation:
  SR = SR(o) * exp(-56.9 * PD * EF)
Code:
  deltaSR = SR(o) - SR(o) * exp(-56.9 * PD * EF * EF)
Variables:
  SR = EQ::StandingCropResidueAfterTillageMultiplier
  PD = tillageDepth_m
  EF = mixingEfficiencyForOperation_frn
 }
{ EQN: 327 }
class function EQ.StandingCropResidueReductionFromTillageMultiplier(tillageDepth_m: single; mixingEfficiencyForOperation_frn:
  single): single;
  begin
  try
  result := 1.0 - safeExp(-56.9 * tillageDepth_m * sqr(mixingEfficiencyForOperation_frn));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.StandingCropResidueReductionFromTillageMultiplier: ' + e.message);
    end;
  end;

{ 
Other functions of the tillage component include simulating ridge
height and interval and surface roughness. These variables are
specified for each tillage implement. However, the ridge interval
and height are computed after each tillage operation to reflect the
combined effects of the current and previous operations. The
ridge height is estimated by using the equations [EQN: 328] and
[EQN: 329] where HR is the ridge height after the tillage operation
k in m, HT is the input ridge height for the tillage operation in m,
and k refers to the sequence of operations.
Equation:
  if HT(k) < HT(k-1), HR = HT(k) + (HT(K-1) - HT(K)) * exp(-PD(k) / PD(k-1))
  if HT(k) >= HT(k-1), HR = HT(k)
Code:
  same
Variables:
  HR = EQ::RidgeHeightAfterTillage_m
  HT = ridgeHeightForOperation_mm
  HT(k-1) = ridgeHeightLastOperation_mm
  PD = tillageDepthForOperation_m
  PD(k-1) = tillageDepthLastOperation_m
 }
{ EQN: 328 EQN: 329 }
class function EQ.RidgeHeightAfterTillage_m(ridgeHeightForOperation_mm: single; ridgeHeightLastOperation_mm: single;
  tillageDepthForOperation_m: single; tillageDepthLastOperation_m: single): single;
  begin
  try
  if (ridgeHeightForOperation_mm < ridgeHeightLastOperation_mm) then 
    result := ridgeHeightForOperation_mm + (ridgeHeightLastOperation_mm - ridgeHeightForOperation_mm)
      * safeExp(safediv(-tillageDepthForOperation_m, tillageDepthLastOperation_m))
  else
    result := ridgeHeightForOperation_mm;
  except on e: Exception do result := errorMessage('Exception in EQ.RidgeHeightAfterTillage_m: ' + e.message); end;
  end;

{
After each tillage operation, the ridge interval is set to the
input ridge interval of the operation with the greater HT.
Equation:
  if HT(k) > HT(k-1), RIN = RIN(k)
  if HT(k-1) > HT(k), RIN = RIN(k-1)
Code:
  same
Variables:
  EQ::RidgeIntervalAfterTillage_m
  RIN = ridgeIntervalForOperation_m
  RIN(k-1) = ridgeIntervalLastOperation_m
  HT = ridgeHeightForOperation_mm
  HT(k-1) = ridgeHeightLastOperation_mm
 }
class function EQ.RidgeIntervalAfterTillage_m(ridgeIntervalForOperation_m: single; ridgeIntervalLastOperation_m: single;
  ridgeHeightForOperation_mm: single; ridgeHeightLastOperation_mm: single): single;
  begin
  try
  if (ridgeHeightForOperation_mm > ridgeHeightLastOperation_mm) then 
    result := ridgeIntervalForOperation_m
  else
    result := ridgeIntervalLastOperation_m;
  except on e: Exception do result := errorMessage('Exception in EQ.RidgeIntervalAfterTillage_m: ' + e.message); end;
  end;

{
The user specifies the date and depth for each tillage
operation. The tillage operation is carried out on the
specified date if the soil is dry enough. If not, the
operation occurs on the next suitable day.

It is also possible to schedule operations by fraction of
heat unit accumulations. The heat unit schedule may be user
input or automatically developed by EPIC. Various combinations
of scheduling (by date, heat units input, or automatic heat
units) are also permitted.

Th harvest index and harvest efficiency provide adequate
flexibility to accommodate almost any harvest strategy. The
harvest index (HI) is input for each crop and adjusted during
each year of simulation as described in the Crop Yield section.
Normally, the adjusted HI dictates the fraction of the above
ground biomass removed from the crop. Thus, for a grain
crop like corn, about 40-50% is removed. However, if corn is
cut for silage, the input HI would be about 0.95. An option
to override HI allows single crops to be harvested in two
different ways. For example, oats could be harvested for
grain by using the model adjusted value of HI ~~ 0.4 and then
the straw could be baled by using the appropriate override
value (0.5-0.95). The harvest efficiency (HE,
efficiencyFactorHarvestOrPesticide) indicates
what portion of the harvested material actually leaves the
field. For most operations, HE may range between 0.7 and
0.95. However, it can be set as low as 0.0 to simulate the
plowing under of cover crops.

                   PLANT ENVIRONMENT *CONTROL*

The plant environment control component provides mechanisms for
applying irrigation water, fertilizer, lime, and pesticide or for
simulating grazing or drainage systems.

Drainage

Drainage via underground drainage systems is treated as a
modification of the natural lateral subsurface flow of the area.
Drainage is simulated by indicating which soil layer contains the
drainage system and the time required for the drainage system to
reduce plant stress. The drainage time in days replaces the travel
time in equation 44 for the layer containing the system.

Irrigation

The EPIC user has the option to simulate dryland or irrigated
agricultural areas. Sprinkler or furrow irrigation may be
simulated and the applications may be scheduled by the user or
automatically. As implied, the user scheduled option allows
application dates and rates to be inputted. With the automatic
option, the model decides when and how much water to apply.

Required inputs for the automatic version include a signal to
trigger applications (the three trigger choices include: plant
water stress level (0-1), plow layer soil water tension in kPa, or
root zone soil water deficit in mm), the maximum volume applied to
each crop in mm, the runoff fraction, minimum and maximum single
application volumes in mm, and the minimum time interval between
applications in days.

Two modes of application, rigid and flexible, are available.
  Rigid mode:
    1. User schedule - the exact input volumes are applied
       on specified dates.
    2. Automatic option - maximum single application volumes
       are applied when triggered.
  Flexible mode:
    1. User schedule - the application volume is the minimum
       of the specified volume, the maximum single application
       volume, and the volume required to fill the root zone
       to field capacity.
    2. Automatic option - the application volume is the minimum
       of the maximum single application volume and the volume
       required to fill the root zone to field capacity.

Also, irrigation does not occur when the application volume
derived from the appropriate mode and options (except for rigid,
user-scheduled) is less than the input minimum single application
volume.

The application mode (rigid or flexible) is fixed for the entire
crop rotation. However, the trigger value and criterion (plant
water stress level, soil water tension, or root zone water
deficit) and the runoff fraction may be changed at any time during
the operation schedule. Also, a combination of user and automatic
scheduling is permitted.

Fertilization

Fertilizer application is similar to irrigation - scheduling may
be input or automatic and rigid and flexible modes are available.
Required inputs for the automatic version include a trigger (plant
N stress level (0-1)), maximum annual N applied to a crop in
kg/ha, and minimum time between applications in days. Automatic
fertilizing at planting is also optional.

  Rigid mode:
    1. User schedule - The exact input rates of N and P are
       applied at specified depths on scheduled dates.
    2. Automatic option - a fraction of the annual maximum
       N rate for the crop is applied when triggered. The
       application fraction and the maximum rate are inputs.
       Also P is applied at a rate necessary to bring the plow
       layer (0.2 m) P concentration to a level specified at the
       start of a simulation. All automatic applications are
       placed in the second soil layer.

  Flexible mode:
    1. User schedule - the model samples N and P concentration
       in the root zone and compares with user preset N and P
       concentrations. Applications occur on schedule at specified
       depths and rates if the root zone N and P concentrations do
       not exceed user standards, otherwise, applications are
       delayed until N and P concentrations are depleted below
       user standards.
    2. Automatic option - the N application rate is the difference
       between the preset rate (application fraction times the
       maximum annual rate) and the root zone N content. The P
       application strategy is the same as in the rigid mode.

Other features and limitations include only mineral N (in NO3
form) and P may be applied automatically. Organic N and P and
ammonia are applied by user scheduling. The maximum annual N
application for a crop can be changed at planting. A combination
of user and automatic scheduling is permitted. Automatic
applications occur only when N is the active crop growth constraint
even though the trigger value is reached. Thus, the
annual N and P application rates vary according to the crop's
needs, the soil's ability to supply those needs, and the magnitude
of the N stress relative to water and temperature stresses.

Liming

EPIC simulates the use of lime to neutralize toxic levels of Al
and/or to raise soil pH to near-optimum levels. Different
algorithms are used to estimate lime requirements of "highly
weathered" soils (Oxisols, Ultisols, Quartzipsamments, Ultic
subgroups of Alfisols, and Dystric suborders of Inceptisols)
(Sharpley et al., 1985) and other soils. The highly weathered
soils have large amounts of variable-charge clays. Moderate
amounts of lime are required to increase their pH to about 5.5 and
convert extractable Al to more inactive forms. However, the pH of
these soils is highly buffered above pH 5.5, and very large
amounts of lime are required to raise the pH to near 7.0. As a
aResult, soils with variable charge clays are usually limed only to
reduce Al saturation to acceptable levels.

The Al saturation of each soil layer is estimated with
the equations (Jones, 1984) [EQN: 330] and [EQN: 331] where
ALS is the Al saturation of soil layer l in percent calculated
as KCL-extractable Al divided by effective cation exchange
capacity (ECEC), BSA is the base saturation calculated from
cation exchange capacity (CEC) determined by the NH4OAc
(pH = 7.0) method in percent, C is the organic carbon content
in percent, and PH is the soil pH.
Equation:
  ALS = 154.2 - 1.017 * BSA - 3.173 * C - 14.23 * PH, if PH <= 5.6
  ALS = 0.0, if PH > 5.6
Code:
  same but added bounds of 0-95 if ph <= 5.6
Variables:
  ALS = EQ::AluminumSaturation_pct
  BSA = baseSaturation_pct
  C = organicC_pct
  PH = soilpH
 }
{ EQN: 330 EQN: 331 }
class function EQ.AluminumSaturation_pct(baseSaturation_pct: single; organicC_pct: single; soilpH: single): single;
  begin
  try
  if (soilpH <= 5.6) then
    begin
    result := 154.2 - 1.017 * baseSaturation_pct - 3.173 * organicC_pct - 14.23 * soilpH;
    if (result < 0.0) then
      result := 0.0
    else if (result > 95.0) then
      result := 95.0;
    end
  else
    result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EQ.AluminumSaturation_pct: ' + e.message); end;
  end;

class function EQ.BaseSaturation_pct(baseFormingCations_cmolPkg: single; cationExchangeCapacity_cmolPkg: single): single;
  begin
  try
  result := frn_to_pct * safediv(baseFormingCations_cmolPkg, cationExchangeCapacity_cmolPkg);
  except on e: Exception do result := errorMessage('Exception in EQ.BaseSaturation_pct: ' + e.message); end;
  end;

{ 
For highly weathered soils, the lime required to neutralize
toxic Al in the plow layer is estimated with the equation
[EQN: 332] where RLA is the lime required to neutralize Al
in t/ha, ECEC is the effective cation exchange capcity in
cmol(p+)/kg, BD is the soil bulk density in t/m3, and PD is
the plow depth in m. (These are not by layer.)
Equation:
  RLA = 0.1 * ALS * ECEC * PD * BD
Code:
  deltaBSA = 0.1 * ALS * ECEC
  SWT = PD * BD
  RLA = deltaBSA * SWT
  so it is the same
Variables:
  RLA = EQ::LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha
  ALS = aluminumSaturation_pct
  ECEC = effectiveCEC_cmolpplusPkg
  deltaBSA = changeInBaseSaturationToOffsetAlSat_pct
  SWT = totalSoilWeightInMaxTillageDepth_tPha
  PD = plowDepth_m
  BD = bulkDensity_tPm3
 }
{ EQN: 332 }
class function EQ.LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha(changeInBaseSaturationToOffsetAlSat_pct: single;
  totalSoilWeightInMaxTillageDepth_tPha: single): single;
  begin
  try
  result := changeInBaseSaturationToOffsetAlSat_pct * totalSoilWeightInMaxTillageDepth_tPha;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha: ' + e.message); end;
  end;

class function EQ.ChangeInBaseSaturationToOffsetAlSat_pct(aluminumSaturation_pct: single; effectiveCEC_cmolpplusPkg: single):
  single;
  begin
  try
  result := 0.1 * aluminumSaturation_pct * effectiveCEC_cmolpplusPkg;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ChangeInBaseSaturationToOffsetAlSat_pct: ' + e.message); end;
  end;

{ 
ECEC is calculated as SMB/ALS (Soil Survey Staff, 1982), where
SMB in cmol/kg is the sum of the bases extracted by NH4OAc (pH = 7.0).

The constant 0.1 (in equation 332) converts cmol(p+)/kg
extractable aluminum to equivalent CaCO3 in t/ha, assuming
2 cmol(p+) CaCO3 are required to completely neutralize
1 cmol(p+) extractable Al (Kamprath, 1970). At the end of
each year, enough lime is applied to meet the lime
requirement (RLA) if RLA >= 1 t/ha. If RLA < 1 t/ha no
lime is applied. When lime is applied, the plow layer pH
is raised to 5.4 and ALS is reduced to 0.

For EPIC, soil acidification and decreasing base saturation
are caused by addition of fertilizer N and symbiotic N fixation
by legumes. All fertilizer N is assumd to derived from anhydrous
ammonia, urea, ammonium nitrate, or mixtures of these with
equivalent acidifying effects. The CaCO3 equivalent of fertilizer
or fixed N is assumed to be 1.8 kg CaCO3/kg N (Pesek et al.,
1971). This is within the range of variation reported by
Pierre et al. (1971) for fertilized corn and by Nyatsanga
and Pierre (1973) and Jarvis and Robson (1983) for legumes.

At the end of each year of simulation, the plow layer pH is
reduced to reflect the change in base saturation caused by
N fertilizer and N fixation. The change in base saturation
is computed with the equation [EQN: 333] where FN is the
amount of N fertilizer added during the year in kg/ha and
WFX is the amount of N fixation by legumes in kg/ha.
Equation:
  deltaBSA = 0.036 * (FN + WFX) / (PD * BD * CEC)
Code:
  deltaBSA = 0.036 * (FN + WFX) / SWT
  SWT = PD * BD, but CEC term is in next equation (PH)
Variables:
  deltaBSA = EQ::ChangeInBaseSaturationByNAdded_frn
  FN = nFertilizerAdded_kgPha
  WFX = nFixation_kgPha
  SWT = totalSoilWeightInMaxTillageDepth_kgPha
  PD = plowDepth_m
  BD = bulkDensity_tPm3
  CEC = cationExchangeCapacity_cmolPkg
 }
{ EQN: 333 }
class function EQ.ChangeInBaseSaturationByNAdded_frn(nFertilizerAdded_kgPha: single; nFixation_kgPha: single;
  totalSoilWeightInMaxTillageDepth_kgPha: single): single;
  begin
  try
  result := 0.036 * safediv((nFertilizerAdded_kgPha + nFixation_kgPha), totalSoilWeightInMaxTillageDepth_kgPha);
  except on e: Exception do result := errorMessage('Exception in EQ.ChangeInBaseSaturationByNAdded_frn: ' + e.message); end;
  end;

{ 
The PH value is reduced by using the equation [EQN: 334]
where the constant 0.5 approximates the slope of the relationship
between pH and deltaBSA for several soils when the values of
BSA are between 60 and 90 (Peech, 1965).
Equation:
  PH = PH(o) - 0.05 * deltaBSA
Code:
  PH = PH(o) - 0.05 * 100 * deltaBSA / CEC
  (here is the CEC term from the deltaBSA equation)
Variables:
  PH = soilpH = SoilpHAfterChangeFromNAdded
  deltaBSA = changeInBaseSaturationByNAdded_pct
  CEC = cationExchangeCapacity_cmolPkg
 }
{ EQN: 334 }
class function EQ.pHReductionFromNAdded(changeInBaseSaturationByNAdded_pct: single; cationExchangeCapacity_cmolPkg: single):
  single;
  begin
  try
  result := 0.05 * 100 * safediv(changeInBaseSaturationByNAdded_pct, cationExchangeCapacity_cmolPkg);
  except on e: Exception do result := errorMessage('Exception in EQ.pHReductionFromNAdded: ' + e.message); end;
  end;

{ 
For other soils, the lime requirement is the amount of time
needed to raise soil pH to 6.5 according to the equation [EQN: 335]
where deltaBSA is the change in base saturation needed to raise
soil pH to 6.5. The constant 0.05 converts deltaBSA in percent
to equivalent CaCO3 in t/ha, assuming that applied CaCO3 reacts
with equivalent unsaturated CEC.
Equation:
  RLA = 0.05 * PD * BD * CEC * deltaBSA
Code:
  RLA = 0.05 * SW * CEC * deltaBSA
  PD * BD = SW, so this is the same
Variables:
  RLA = EQ::LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha
  PD = plowDepth_m
  BD = bulkDensity_tPm3
  SW = totalSoilWeightInMaxTillageDepth_kgPha
  CEC = cationExchangeCapacity_cmolPkg
  deltaBSA = changeInBaseSaturationToRaisePHTo6p5_pct
 }
{ EQN: 335 }
class function EQ.LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha(totalSoilWeightInMaxTillageDepth_kgPha: single;
  meanWeightedCEC_cmolPkg: single; changeInBaseSaturationToRaisePHTo6p5_pct: single): single;
  begin
  try
  result := 5.0 * totalSoilWeightInMaxTillageDepth_kgPha * meanWeightedCEC_cmolPkg *
    changeInBaseSaturationToRaisePHTo6p5_pct * pct_to_frn;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha: ' + e.message); end;
  end;

{ CFK function }
{ CFK FIX - work out units better }
{ if the equivalent weight of CaCO3 is 50 mg/meq, and the conversion constant for lime is 5.0, then
  if the equivalent weight of S is 16 mg/meq, then the conversion constant for lime is 1.6? should work out units}
class function EQ.SulfurFor5PHForNonHighlyWeatheredSoil_kgPha(totalSoilWeightInMaxTillageDepth_kgPha: single;
  meanWeightedCEC_cmolPkg: single; changeInBaseSaturationToLowerPHTo5_pct: single): single;
  begin
  try
  result := 1.6 * totalSoilWeightInMaxTillageDepth_kgPha * meanWeightedCEC_cmolPkg *
    changeInBaseSaturationToLowerPHTo5_pct * pct_to_frn;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha: ' + e.message); end;
  end;

{
The deltaBSA is estimated with the relation [EQN: 336].
Equation:
  deltaBSA = min((6.5 - PH) / 0.023, 90 - BSA)
Code:
  same
Variables:
  deltaBSA = EQ::ChangeInBaseSaturationToRaisePHTo6p5_pct
  PH = soilpH
  BSA = baseSaturation_pct
 }
{ EQN: 336 }
class function EQ.ChangeInBaseSaturationToRaisePHTo6p5_pct(soilpH: single; baseSaturation_pct: single): single;
  begin
  try
  result := min((6.5 - soilpH) / 0.023, 90.0 - baseSaturation_pct);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ChangeInBaseSaturationToRaisePHTo6p5_pct: ' + e.message); end;
  end;

{ CFK equation }
class function EQ.ChangeInBaseSaturationToLowerPHTo5_pct(soilpH: single; baseSaturation_pct: single): single;
  begin
  try
  { choice of pH 5 and 65% BSA taken from Troeh & Thompson }
  result := min((soilpH - 5.0) / 0.023, baseSaturation_pct - 65.0);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ChangeInBaseSaturationToRaisePHTo6p5_pct: ' + e.message); end;
  end;

{
For soils that are not highly weathered, lime application
is simulated if at the end of the year, RLA > 2.0 t/ha. When lime
is applied, pH is changed to 6.5, base saturation is increased by
deltaBSA, and ALS is set to 0.

This equation, derived from equations 336 and 335, estimates the new pH
value when a given amount of lime is added. The derivation is as follows.
deltaBSA = min((6.5 - pH) / 0.023, 90 - BSA) to reach a pH of 6.5
RLA = 0.01 * SWT * ECEC * deltaBSA
so
deltaBSA = RLA / (0.05 * SWT * ECEC)
substituting equation 336 for deltaBSA and ignoring the minimum,
(newpH - pH) / 0.023  = RLA / (0.05 * SWT * ECEC)
now solving for newpH,
newpH = 0.023 * RLA / (0.05 * SWT * ECEC) + pH
 }
class function EQ.NewpHForLimeAdded(limeAdded_kgPha: single; effectiveCationExchangeCapacity_cmolPkg: single;
  totalSoilWeight_tPha: single; currentpH: single): single;
  begin
  try
  result := safediv(0.023 * limeAdded_kgPha,
    5.0 * pct_to_frn * totalSoilWeight_tPha * effectiveCationExchangeCapacity_cmolPkg) + currentpH;
  except on e: Exception do result := errorMessage('Exception in EQ.NewpHForLimeAdded: ' + e.message); end;
  end;

{ CFK function }
class function EQ.NewpHForSulfurAdded(sulfurAdded_kgPha: single; effectiveCationExchangeCapacity_cmolPkg: single;
  totalSoilWeight_tPha: single; currentpH: single): single;
  begin
  try
  result := currentpH - safediv(0.023 * sulfurAdded_kgPha,
    1.6 * pct_to_frn * totalSoilWeight_tPha * effectiveCationExchangeCapacity_cmolPkg);
  except on e: Exception do result := errorMessage('Exception in EQ.NewpHForLimeAdded: ' + e.message); end;
  end;
{
Pests

The three pests considered by EPIC are insects, weeds, and plant
diseases. The effects of all three pests are expressed in the
EPIC pest factor. Crop yields are estimated at harvest as the
product of simulated yield and pest factor. The pest factor
ranges from 0.0 to 1.0 -- 1.0 means no pest damage and 0.0
means total crop destruction by pests. The pest factor is
simulated daily as a function of temperature, moisture, and
ground cover [EQN: 337] where PSTI is the accumulated pest
index for day k, T(mn) is the minimum temperature for day i in
degrees C, RFS is the accumulated rainfall for 30 days preceding
day i in mm, RFS(T) is the threshold 30-day rainfall amount
in mm, CV is the ground cover (live biomass and crop residue)
on day i in t/ha, and CV(T) is the threshold cover value in t/ha.
When T(mn) is less than 0.0, the pest index is reduced using
the equation [EQN: 338]. Thus, the pest index grows rapidly
during warm moist periods with adequate ground cover and is
reduced by cold temperatures. This general pest index is an
attempt to account for major differences in pest problems
related to climate variability.
Equation:
  if RFS > RFS(T) and CV > CV(T) and T(mn,i) > 0.0
    PSTI = (sum with i from 1 to k of) T(mn,i) * (1.0 + RFS - RFS(T))
  if T(mn,i) < 0.0
    PSTI(k) = PSTI(k-1) + T(mn)
Code:
  if RFS > RFS(T) and CV > CV(T) and T(mn,i) > 0.0
    PSTI(k) = PSTI(k-1) + T(mn,i) * (1.0 + (RFS - RFS(T)) / 100)
  otherwise the same
Variables:
  PSTI = PestPopulationIndex
  T(mn,i) = minTempForDay_degC
  RFS = previousThirtyDaysRainfall_mm (computed every day)
  RFS(T) = thresholdThirtyDayRainfallForPests_mm
  CV = aboveGroundBiomassAndResidue_tPha
  CV(T) = thresholdBiomassAndResidueForPests_tPha
 }
{ EQN: 337 EQN: 338 }
class function EQ.PestPopulationIndex(pestPopulationIndex: single; minTempForDay_degC: single;
  sumRainfallMinusRunoffPrev30Days_mm: single; thresholdThirtyDayRainfallForPests_mm: single;
  aboveGroundBiomassAndResidue_tPha: single; thresholdBiomassAndResidueForPests_tPha: single;
  healthOfBeneficialsIndex: single): single;
  var
    pestRainfallFactor: single;
    pestCoverFactor: single;
  begin
  try
  if (minTempForDay_degC <= 0.0) then 
    begin
    result := pestPopulationIndex + minTempForDay_degC;
    exit;
    end;
  pestRainfallFactor := (sumRainfallMinusRunoffPrev30Days_mm - thresholdThirtyDayRainfallForPests_mm) / 100;
  pestCoverFactor := aboveGroundBiomassAndResidue_tPha - thresholdBiomassAndResidueForPests_tPha;
  if ((pestCoverFactor > 0.0) and (pestRainfallFactor > 0.0)) then 
    { change from EPIC: added fourth term with healthOfBeneficialsIndex }
    result := pestPopulationIndex + (minTempForDay_degC * (pestRainfallFactor + 1.0))
      * safediv(1.0, max(0.1, healthOfBeneficialsIndex))
  else
    result := pestPopulationIndex;
  except on e: Exception do result := errorMessage('Exception in EQ.PestPopulationIndex: ' + e.message); end;
  end;

{When pesticides are applied, the pest index is reduced using
the equation [EQN: 339] where PSTE is the pesticide kill fraction
ranging from near 0.0 to near 1.0. Thus, if the kill fraction
approaches 1.0, the pest index is reduced nearly 1000 units.
Equation:
  PSTI(k) = PSTI(k-1) - 1000 * PSTE
Code:
  same
Variables:
  deltaPSTI = EQ::PestFactorReductionFromPesticide
  PSTE = killFractionForPesticide_frn
 }
{ EQN: 339 }
class function EQ.PestFactorReductionFromPesticide(killFractionForPesticide_frn: single): single;
  begin
  try
  result := 1000.0 * killFractionForPesticide_frn;
  except on e: Exception do result := errorMessage('Exception in EQ.PestFactorReductionFromPesticide: ' + e.message); end;
  end;

{ 
At harvest, the pest factor is computed from the pest index using
the equation [EQN: 340] where PSTF is the pest factor used to adjust
crop yield, PSTM is the minimum pest factor value for a crop, and
k is time since last harvest in days.
Equation:
  PSTI* = PSTI / k
  PSTF = 1.0 - (1.0 - PSTM) * (PSTI* / (PSTI* + exp(2.7 - 0.499 * PSTI*)))
Code:
Variables:
  PSTF = EQ::PestFactor_frn
  PSTM = minPestWeedDiseaseFactor_frn
  PSTI = pestPopulationIndex
  PSTI* = pestPopulationIndex / daysSinceLastHarvest
  k = daysSinceLastHarvest
 }
{ EQN: 340 }
class function EQ.PestFactor_frn(pestPopulationIndex: single; minPestWeedDiseaseFactor_frn: single; maxPestPopulationIndex:
  single; biomassGrowthConstraint_frn: single; var pestDamageCoeffs: SCurveStructure): single;
  var
    pestSCurveFactor: single;
  begin
  try
  if (pestPopulationIndex <= 0.0) then
    result := 1.0
  else
    begin
    { change from EPIC: was
    result := 1.0 - (1.0 - minPestWeedDiseaseFactor_frn) * scurve(safediv(pestPopulationIndex, daysSinceLastHarvest),
      pestDamageCoeffs.c1, pestDamageCoeffs.c2);
    note: daysSinceLastHarvest was only used here, and so is no longer kept track of. }
    pestSCurveFactor := (1.0 - biomassGrowthConstraint_frn) * safediv(pestPopulationIndex, maxPestPopulationIndex);
    pestSCurveFactor := max(0.0, min(1.0 - minPestWeedDiseaseFactor_frn, pestSCurveFactor));
    result := 1.0 - scurve(pestSCurveFactor, pestDamageCoeffs.c1, pestDamageCoeffs.c2);
    end;
  except on e: Exception do result := errorMessage('Exception in EQ.PestFactor_frn: ' + e.message); end;
  end;

{
Furrow Diking

Furrow diking is the practice of building small temporary dikes
across furrows to conserve water for crop production. Since they
reduce runoff, they may also aid in erosion control. The EPIC
furrow diking model allows construction of dikes for any ridge
spacing and at any interval down the furrows. Dikes may be
constructed or destroyed mechanically on any day of the year.
If estimated runoff for a particular event (rainfall) exceeds
the dike storage volume (average for all furrows in the field),
overtopping occurs and all of the estimated runoff is lost. If
not, all of the rainfall infiltrates and is available for plant
use. When runoff destroys the dikes, the model rebuilds them
automatically. Rainstorms that do not overtop the dikes cause
settling and, thus, reduce storage volume. Settling is estimated
with the equation [EQN: 341] where H(o) is the dike height before
settling, H is the dike height after settling, and Y is the USLE
estimate of soil loss (sediment yield) in t/ha. Ridge height is
also reduced with the settling function contained in equation 341.
The dikes are automatically rebuilt when H/H(o) < 0.7.
Equation:
  H = H(o) * exp(-0.1 * Y)
Code:
  much different.
Variables:
  H = DikeHeightAfterSettlingDueToRain_mm
  Y = totalErosion_tPha
 }
{ EQN: 341 }
class function EQ.RainSettlingMultiplier(rainfallForDay_mm: single; var layers: LayerStructureArray): single;
  var
    clayFactor, insideLn, intermediate, organicC_pct: single;
  begin
  try
  organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[0].organicMatter_tPha, layers[0].weight_tPha);
  if (rainfallForDay_mm > 0.0) then 
    begin
    insideLn := organicC_pct + 15.7 * layers[0].clayContent_pct - 0.25 * sqr(layers[0].clayContent_pct);
    if insideLn <= 0.0 then
      begin
      { because this function returns a multiplier, the default should be 1.0 for no change }
      result := 1.0;
      exit;
      end;
    clayFactor := 63.0 + 62.7 * safeLn(insideLn);
    clayFactor := max(50.0, clayFactor);
    if clayFactor <> 0 then
      intermediate := power(safediv(rainfallForDay_mm, clayFactor), 0.6)
    else
      begin
      { because this function returns a multiplier, the default should be 1.0 for no change }
      result := 1.0;
      exit;
      end;
    end
  else 
    intermediate := 0.0;
  result := 0.0001 + safeExp(-intermediate);
  except on e: Exception do result := errorMessage('Exception in EQ.RainSettlingMultiplier: ' + e.message); end;
  end;

{
The dike storage volume is estimated by assuming that the furrow and
the dike are triangular and that the dike side slopes are 2:1. Given the
dike and ridge heights, the dike interval, and the slope down
the furrow, the volume can be calculated directly.

There are two possible dike configurations that require slightly
different solutions.

Normal case - lower slope

Normally, the dike interval is relatively short (1-3 m) and the
slope along the furrow is relatively flat (<1.0%). When the dike
is full, water extends from the top of the downslope dike up the
furrow to a point above the toe (bottom) of the upslope dike. The
volume is calculated by using cross-sectional areas at the toes of
the two dikes. This approach computes the volume in three parts
(1. between the top and the toe of the downslope dike, 2. between
the toes of the two dikes, and 3. between the toe and the waterline
on the upslope dike). Beginning at the centerline of the downslope
dike, the volume equations are
  DV(I) = 1/2 * H * D(2) * W(2)                             (Equation 342)
  DV(II) = 1/4 * (DI - 4 * H) * (D(2) * W(2) + D(3) * W(3)) (Equation 343)
  DV(III) = 1/4 * (XD - DI + 2 * H) * D(3) * W(3)           (Equation 344)
where DV is the dike volume between cross sections in m3,
H is the dike height in m, D is the water depth in m, W is
the water surface width in m, DI is the dike interval in m, XD
is the distance from the center of the downslope dike to the
waterline on the upslope dike in m, and subscripts 2 and 3 refer
to cross sections 2 and 3. Cross section 2 is at the toe of the
downslope dike and cross section 3 is at the toe of the upslope
dike.

Water depth is calculated with the equations
  D(2) = H - 2 * S * H                                     (Equation 345)
  D(3) = H - S * (DI - 2 * H)                              (Equation 346)
where S is the slope in m/m along the furrow (which I think
is the same as the land surface slope).

Water surface width is a function of depth and ridge spacing,
RS in mm
  W = RS * D / H                                           (Equation 347)

The distance XD is computed with the equations
  XD = DI - 2 * (H - DZ)                                   (Equation 348)
  DZ = H - S * XD                                          (Equation 349)
where DZ is the water line eleveation on the upslope dike. The constant
2 in equation 348 comes from the assumed 2:1 dike upslopes.

Simultaneous solution of equations 348 and 349 yields
  XD = DI / (1 + 2 S)                                      (Equation 350)

Substituting D, W, and XD into equations 342, 343, and 344 and
summing gives
  DV = 1/4 * RS / H * (H2 * (1-2S)2 * (DI - 2H) + (H - S(DI - 2H))2
    * (DI / (1+2S) - 2H))                                  (Equation 351)

Equation 351 is divided by the total surface area of a furrow dike
to convert volume from m3 to mm [EQN: 352].
  DV = DikeVolumeForLowSlope_mm
  DI = dikeInterval_m
  H = dikeHeight_m
  S = slopeSteepness_mPm

More unusual case - higher slope

In the simpler and more unusual dike configuration, the upslope
waterline does not extend to the toe of the upslope dike. Only
one cross section is involved and the volume is computed into
two parts. Equation 342 is used to calculated the most downslope
volume, and the upslope volume is calculated with the equation
  DV(2) = 1/4 * D(2) * W(2) * H * (1/S - 2)           (Equation 353)

Adding equations 342 and 353, substituting D and W, and converting
from m3 to mm gives [EQN: 354].
Equation:
  DV = 250 / (DI * H)
       * (sqr(H) * sqr(1 - 2 * S)
       + sqr(H - S * (DI - 2 * H))
       * (DI / (1 + 2 * S) - 2 * H))
  DV(2) = 250 * sqr(H) * sqr(1 - 2 * S) / (S * DI)
Code:
  DV = 250 * 1000 * FDSF / (DI * RH)
       * (sqr(H) * sqr(1 - 2 * S)
       * (DI - 2 * H)
       + sqr(H - S * (DI - 2 * H))
       * (0.5 * DI / (S + 0.5) - 2 * H)
       only difference here is extra term and extra factor (mentioned in chapter)
  DV(2) = 250 * 1000 * FDSF / (RH * DI) * H / S * sqr(H) * sqr(1 - 2 * S)
Variables:
  DV = DikeVolumeForHighSlope_mm
  DI = dikeInterval_m
  H = dikeHeight_mm
  RH = ridgeHeight_mm
  S = slopeSteepness_mPm
  FDSF = fractDikeVolAvailForWaterStorage_frn
 }
{ EQN: 352 EQN: 354 }
class function EQ.DikeVolume_mm(var surface: SurfaceStructure; var params: SoilParamsStructure): single;
  var
    dikeHeight_m, firstTerm_m, secondTerm_m, thirdTerm_m, fourthTerm_m, fifthTerm_m, dikeVolume_mm: single;
    slope_mPm: single;
  begin
  try
  slope_mPm := params.watershedSlopeSteepness_mPm;
  dikeHeight_m := surface.dikeHeight_mm * 0.001;
  { set ridge height to dike height if less }
  if (surface.ridgeHeight_mm < surface.dikeHeight_mm) then surface.ridgeHeight_mm :=
    surface.dikeHeight_mm;
  if (safediv(dikeHeight_m, slope_mPm) > surface.dikeInterval_m - 2.0 * dikeHeight_m) then
    begin
    { dike volume if water extends above the toe (bottom) of the upslope dike }
    {             / <- upslope dike               ~~~ = water }
    {            / }
    {           /~~~~~~~~~~~~/  <- downslope dike }
    {  toe ->  /_____       / }
    {               _______/ }
    { note to make units more clear, I have replaced 250 by 0.25 / 0.001 }
    firstTerm_m := 0.25 * safediv(params.fractDikeVolAvailForWaterStorage_frn, surface.dikeInterval_m *
      (surface.ridgeHeight_mm * mm_to_m));
    secondTerm_m := sqr(dikeHeight_m) * sqr(1.0 - 2.0 * slope_mPm);
    thirdTerm_m := surface.dikeInterval_m - 2.0 * dikeHeight_m;
    fourthTerm_m := sqr(dikeHeight_m - slope_mPm * thirdTerm_m);
    fifthTerm_m := 0.5 * safediv(surface.dikeInterval_m, slope_mPm + 0.5) - 2.0 * dikeHeight_m;
    dikeVolume_mm := m_to_mm * firstTerm_m * (secondTerm_m * thirdTerm_m + fourthTerm_m * fifthTerm_m);
    end
  else 
    begin
    { dike volume if water does not extend to the toe (bottom) of the upslope dike }
    {             / <- upslope dike               ~~~ = water }
    {            / }
    {           /            /  <- downslope dike }
    {  toe ->  /_____       / }
    {               ~~~~~~~/ }
    { note to make units more clear, I have replaced 250 by 0.25 / 0.001 }
    dikeVolume_mm := 0.25 * m_to_mm
      * safediv(params.fractDikeVolAvailForWaterStorage_frn, surface.ridgeHeight_mm * mm_to_m * surface.dikeInterval_m)
      * safediv(dikeHeight_m, slope_mPm) * sqr(dikeHeight_m) * sqr(1.0 - 2.0 * slope_mPm);
    end;
  result := max(0.0, dikeVolume_mm);
  except on e: Exception do result := errorMessage('Exception in EQ.DikeVolume_mm: ' + e.message); end;
  end;

{ 
Thus, the average dike volume of a field is estimated with
equation 352 or 354 as dictated by slope and dike height
and interval. However, no field is exactly uniform in slope;
dike and ridge heights vary, and furrow and dike side slopes
may not be triangular. Therefore, the model provides a user-
controlled dike efficiency factor to allow for varying conditions
across a field. The dike efficiency factor also provides for
conservative or optimistic dike system design.
(But they don't say where it is used.)

Grazing

Livestock grazing is simulated as a daily harvest operation.
Users specify daily grazing rate in kg/ha, minimum grazing
height in mm, harvest efficiency, and date grazing begins
and ends. Harvest efficiency is used to estimate the fraction
of grazed plant material used by animals - not returned
as manure, etc. Any number of grazing periods may occur
during a year and the grazing schedule may vary from year
to year within a rotation. Grazing ceases when forage
height is reduced to the user-specified cutoff value and
resumes automatically when new growth exceeds the cutoff
height if the grazing period has not expired.

Economics

The economic component of EPIC is more accurately represented
as a crop budget and accounting subsystem. The algorithms
keep track of the costs of producing and marketing the
crops. Costs (and income) are divided into two groups:
those costs which do not vary with yield and those that
do. These groups will be addressed in turn. All cost
registers are cleared at harvest. All operations after
harvest are charged to the next crop in the cropping
sequence.

Tillage and (preharvest) machine operation costs are
assumed to be independent of yield. These operation costs
must be calculated outside of EPIC and are inputted as
one variable into the tillage file. This cost cell
contains all costs associated with the single operation
or activity (e.g., a chiseling activity includes fuel,
labor, depreciation, repair, interest, etc., for
both the tractor and the chisel). A budget generator
program like the Micro Budge Management System (MBMS)
(McGrann et al., 1986) is convenient for making these
calculations. This is an updated interaction program
developed from the Enterprise Budget Calculator
(Kletke, 1979). The MBMS is more compatible with EPIC
in that it has output capabilities to itemize cost
by machine operation. This information (when converted
to metric units) can be input directly into the
equiment file in EPIC. Farm overhead, land rent, and
other fixed costs can be charged to the crop by
first creating null operations in the equipment file
with machine number and cost information only and
then triggering the cost in EPIC with a null
activity. Government payments can be credited by
using negative cost entries in the same way.

Costs which are yield and management dependent are
entered into EPIC in two regions of the input data.
Seed costs, seeding rates, and crop prices are entered
in the crop parameter file for each crop code. Seed
costs are calculated as the product of seeding rate
and cost per kilogram. Amendment costs are calculated
similarly. The amendments include elemental N and P,
irrigation water, and lime. Total cost per hectare
is based on the product of crop yield and net crop
price. Net crop price is the market price minus the
harvest, hauling, and other processing costs which
are yield dependent. The net price must be determined
outside EPIC.

When valid cost figures are entered into these EPIC
input cells, the model will return annual cost and
returns by crop. EPIC budget information is valuable
not only for profit analyses but also risk analyses,
since the annual distributions of profits and costs
can be captured. Risk analysis capability greatly
enhances the analytical value of EPIC for economic
studies.

The greatest value of EPIC to economic analysis is
not its internal economic accounting, but the stream of
physical outputs on daily, monthly, annual, or multi-
year periods that can be input into economic models,
budget generators, and risk analysis systems. EPIC
estimates crop yields, movement of nutrients and
pesticides, and water and sediment yields. Changes
in inputs necessary to respond to changes in managment,
soil quantity and quality, climate (i.e., global
warming), droughts etc., are also estimated. These
outputs become inputs into economic and natural
resource models facilitating comprehensive analyses
of alternative policies and programs.

                             *CROP GROWTH*


A single model is used in EPIC for simulating all the crops considered (corn,
grain sorghum, wheat, barley, oats, sunflower, soybean, alfalfa, cotton,
peanuts, potatoes, durham wheat, winter peas, faba beans, rapeseed,
sugarcane, sorghum hay, range grass, rice, casava, lentils, and pine
trees). Of course, each crop has unique values for the model parameters.
EPIC is capable of simulating growth for both annual and perennial
crops. Annual crops grow from planting to harvest date or until the
accumulated heat units equal the potential heat units for the crop.
Perennial crops maintain their root systems throughout the year, although
they may be become dormant after frost. They start growing when the
average daily air temperature exceeds their base temperature.

Heat unit accumulation

Phenological development of the crop is based on daily heat unit
accumulation. It is computed by using the equation [EQN: 253] where
HU, T(mx) and T(mn) are the values of heat units, maximum temperature,
and minimum temperature in degrees C on day k, and T(b) is the crop-
specific base temperature in degrees C (no growth occurs at or below
T(b)) of crop j.
Equation:
  HU = (T(mx) + T(mn)) / 2 - T(b)
  HU >= 0
Code:
  same
Variables:
  HU = EQ::HeatUnitAccumulationForDay
  T(mx) = maxTempForDay_degC
  T(mn) = minTempForDay_degC
  T(b) = baseTempForCrop_degC
 }
{ EQN: 253 }
class function EQ.HeatUnitAccumulationForDay(meanTempForDay_degC, baseTemp_degC, optTemp_degC: single): single;
  begin
  try
  {change from EPIC: we added the second term here to bound the accumulation if the temperature is above optimal }
  result := max(0.0, meanTempForDay_degC - baseTemp_degC) - max(0.0, meanTempForDay_degC - optTemp_degC);
  except on e: Exception do result := errorMessage('Exception in EQ.HeatUnitAccumulationForDay: ' + e.message); end;
  end;

{ 
Heat unit index

A heat unit index (HUI) ranging from 0 at planting to 1.0 at physiological
maturity is computed as follows [EQN: 254] where HUI is the heat unit
index for day i and PHU is the potential heat units required for the
maturation of crop j. The value of PHU may be inputted or calculated
by the model from normal planting and harvest dates. (But they don't
say here how to do that.) Date of harvest, leaf area growth and
senescence, optimum plant nutrient concentrations, and partition of
dry matter among roots, shoots, and economic yield are affected by HUI.
Equation:
  HUI = HU / PHU
Code:
  same
Variables:
  HUI = EQ::HeatUnitIndex
  HU = cumHeatUnits
  PHU = heatUnitsAtMaturation
 }
{ EQN: 254 }
class function EQ.HeatUnitIndex(cumHeatUnits: single; heatUnitsAtMaturation: single): single;
  begin
  try
  result := safedivExcept(cumHeatUnits, heatUnitsAtMaturation, 0.0);
  except on e: Exception do result := errorMessage('Exception in EQ.HeatUnitIndex: ' + e.message); end;
  end;

{ 
Photosynthetically active radiation

Interception of solar radiation is estimated with a Beer's law
equation (Monsi and Saeki, 1953) [EQN: 255] where PAR is
intercepted photosynthetic active radiation in MJ/m2, RA is
solar radiation in MJ/m2, LAI is the leaf area index, and
subscript i is the day of the year. The constant 0.5 is used
to convert solar radiation to photosynthetically active radiation
(Monteith, 1973). Experimental studies indicate that the extinction
coefficient varies with foliage characteristics, sun angle, row
spacing, row direction, and latitude (Thornley, 1976). The value
used in EPIC (0.65) is representative of crops with narrow row
spacings (Uchijima et al., 1968). A somewhat smaller value (0.4 - 0.6)
might be appropriate for tropical areas in which average sun
angle is higher and for wide row spacings (Begg et al., 1964.
Bonhomme et al., 1982. Muchow et al., 1982).
Equation:
  PAR = 0.5 * RA * (1.0 - exp(-0.65 * LAI))
Code:
  same
Variables:
  PAR = EQ::PhotoActiveRadiation_MJPm2
  RA = radiationForDay_MJPm2
  LAI = leafAreaIndex
 }
{ EQN: 255 }
class function EQ.PhotoActiveRadiation_MJPm2(radiationForDay_MJPm2: single; leafAreaIndex: single): single;
  begin
  try
  result := 0.5 * radiationForDay_MJPm2 * (1.0 - safeExp(-0.65 * leafAreaIndex));
  except on e: Exception do result := errorMessage('Exception in EQ.PhotoActiveRadiation_MJPm2: ' + e.message); end;
  end;

{ 
Potential increase in biomass

Using Monteith's approach (Monteith, 1977), potential increase in biomass
for a day can be estimated with the equation [EQN: 256] where deltaB(p)
is the daily potential increase in biomass in t/ha, BE is the crop
parameter for converting energy to biomass in kg/ha*MJ/m2.

Biomass energy conversion is affected by vapor pressure deficit (VPD)
and by atmospheric CO2 level. The biomass conversion factor BE is
adjusted using the equations of Stockle et al. (1992) [EQN: 257]
where CO2 is the atmospheric CO2 level in ppm and bc(1) and bc(2)
are crop parameters.

The VPD correction is accomplished in the equation [EQN: 258] where
VPD is the vapor pressure deficit in kPa and bc(3) is a crop parameter.
The value of VPD is e(a) (saturation vapor pressure at mean air
temperature) minus e(d) (vapor pressure at mean air temperature).
Finally, BE' is substituted into equation 256 to obtain the corrected
biomass estimate.
Equation:
  deltaB(p) = 0.001 * BE' * PAR
  BE* = 100 * CO2 / (CO2 + exp(bc(1) - bc(2) * CO2))
  BE' = BE* - bc(3) * (VPD - 1.0), VPD > 0.5
Code:
  same
Variables:
  deltaB(p) = EQ::PotentialIncreaseInBiomass_tPha
  BE = biomassConvFactor_kgPhaTMJPm2
  PAR = photoActiveRadiation_MJPm2
  CO2 = carbonDioxideInAtmosphere_ppm
  bc(1) = biomassToEnergyRatioCO2Params[0]
  bc(2) = biomassToEnergyRatioCO2Params[1]
  bc(3) = biomassToEnergyRatioVPDParam
  VPD = vaporPressureDeficit_kPa
 }
{ EQN: 258 EQN: 257 EQN: 256 }
class function EQ.PotentialIncreaseInBiomass_tPha(photoActiveRadiation_MJPm2: single; carbonDioxideInAtmosphere_ppm: single;
  var biomassToEnergyRatioCO2Params: SCurveStructure; biomassToEnergyRatioVPDParam: single; vaporPressureDeficit_kPa: single):
  single;
  var
    biomassConvFactor_kgPhaTMJPm2: single;
    boundedVaporPressureDeficit_kPa: single;
  begin
  try
  biomassConvFactor_kgPhaTMJPm2 := 0.0;
  boundedVaporPressureDeficit_kPa := min(vaporPressureDeficit_kPa, 0.5);
  biomassConvFactor_kgPhaTMJPm2 := 100.0 * scurve(carbonDioxideInAtmosphere_ppm, biomassToEnergyRatioCO2Params.c1,
    biomassToEnergyRatioCO2Params.c2) - biomassToEnergyRatioVPDParam * (1.0 - boundedVaporPressureDeficit_kPa);
  result := kg_to_t * biomassConvFactor_kgPhaTMJPm2 * photoActiveRadiation_MJPm2;
  except on e: Exception do result := errorMessage('Exception in EQ.PotentialIncreaseInBiomass_tPha: ' + e.message); end;
  end;

{ 
Leaf Area Index

In most crops, leaf area index (LAI) is initially zero or small. It increases
exponentially during early vegetative growth, when the rates of leaf primordia
development, leaf tip appearance, and blade expansion are linear functions of
heat unit accumulation (Tollenaar et al., 1979. Watts, 1972). In vegetative
crops such as sugarcane and some forages, LAI reaches a plateau, at which
time the rates of senescence and growth of leaf are are approximately equal.
In many crops, LAI decreases after reaching a maximum and approaches zero
at physiological maturity. In addition, leaf expansion, final LAI, and leaf
duration are reduced by stresses (Acevedo et al., 1971. Eik and Hanway, 1965).

LAI before leaf decline

LAI is simulated as a function of heat units, crop stress, and crop
development stages. From emergence to the start of leaf decline, LAI
is estimated with the equations [EQN: 259] and [EQN: 260] where LAI
is th leaf area index, HUF is the heat unit factor, and REG is the value
of the minimum (limiting) crop stress factor discussed in more detail
below. Subscript mx is the maximum value possible for the crop and delta
is the daily change. The exponential function of equation 260 prevents
LAI from exceeding LAI(mx) when HUF is adjusted for vernalization of
certain crops.
Equation:
  LAI = LAI(i-1) + deltaLAI
  deltaLAI = deltaHUF * LAI(mx) * (1.0 - exp(5.0 * (LAI(i-1) - LAI(mx)))) * sqrt(REG)
Code:
  if (deltaHUF * LAI(mx)) > 0.0
    deltaLAI = deltaHUF * LAI(mx) * dayLengthIsAboveThresholdForGrowth * sqrt(REG)
  else
    deltaLAI = 0
Variables:
  LAI(i-1) = leafAreaIndex
  LAI = EQ::LeafAreaIndexBeforeLeafDecline
  HUF = heatUnitFactorForLAIAndHeight
  HUF(i-1) = yesterdaysHeatUnitFactorForLAIAndHeight
  LAI(mx) = maxLeafAreaIndex
  REG = biomassTempStressFactor_frn
 }
{ EQN: 259 EQN: 260 }
{change from EPIC - pdf - removed test for low day length}
class function EQ.LeafAreaIndexBeforeLeafDecline(var development: DevelopmentStructure; maxLeafAreaIndex: single;
   var constraints: ConstraintsStructure): single;
  var changeInHUFTMaxLAI: single;
  begin
  try
  changeInHUFTMaxLAI := (development.heatUnitFactorForLAIAndHeight - development.yesterdaysHeatUnitFactorForLAIAndHeight)
      * maxLeafAreaIndex;
  if (changeInHUFTMaxLAI > 0.0) then 
    result := development.leafAreaIndex + changeInHUFTMaxLAI * sqrt(constraints.biomassGrowthConstraint_frn)
  else
    result := development.leafAreaIndex;
  result := min(maxLeafAreaIndex, result);
  except on e: Exception do result := errorMessage('Exception in EQ.LeafAreaIndexBeforeLeafDecline: ' + e.message); end;
  end;

{ 
Heat unit factor for LAI and for crop height

The heat unit factor is computed using the equation [EQN: 261] where
ah(1) and ah(2) are parameters of crop j, and HUI is the heat unit index.
Equation:
  HUF = HUI / (HUI + exp(ah(1) - ah(2) * HUI))
Code:
  same
Variables:
  HUF = EQ::HeatUnitFactorForLAIAndHeight
  HUI = heatUnitIndex
  ah(1) = heatUnitFactorParamsForLAI[0]
  ah(2) = heatUnitFactorParamsForLAI[1]
 }
{ EQN: 261 }
class function EQ.HeatUnitFactorForLAIAndHeight(heatUnitIndex: single; var heatUnitFactorParamsForLAI: SCurveStructure):
  single;
  begin
  try
  result := scurve(heatUnitIndex, heatUnitFactorParamsForLAI.c1, heatUnitFactorParamsForLAI.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.HeatUnitFactorForLAIAndHeight: ' + e.message); end;
  end;

{ 
LAI during leaf decline

From the start of leaf decline to the end of the growing season, LAI is estimated
with the equation [EQN: 262] where ad is a parameter that governs LAI
decline rate for crop j and subscript o is the day of the year when
LAI starts declining.
Equation:
  LAI = LAI(o) * power((1 - HUI) / (1 - HUI(o)), ad)
Code:
  LAI = LAI(o) * power(10, log10((1 - HUI) / (1 - HUI(o))) * ad)
  there is also a biomass adjustment if LAI is in decline
  also if the crop is a tree, LAI does not decline.
Variables:
  LAI = EQ::LeafAreaIndexDuringLeafDecline
  LAI(o) = leafAreaIndexAtStartOfDecline
  HUI = heatUnitIndex
  HUI(o) = fractionOfGrowingSeasonWhenLeafDeclineStarts_frn
  ad = leafAreaIndexDeclineRateFactor
 }
{ EQN: 262 }
class function EQ.LeafAreaIndexDuringLeafDecline(var development: DevelopmentStructure;
    var plantParams: PlantParamsStructure; var constraints: ConstraintsStructure): single;
  var
    exponent: single;
    { change from EPIC }
    { I added the adjustedHUI because this was producing -ve LAI after HUI > PHU }
    { in epic crops are harvested when HUI = PHU so this never occurs }
    adjustedHUI: single;
    logHUINowOverStartOfDecline: single;
  begin
  try
  exponent := 0.0;
  result := 0.0;
  adjustedHUI := min(0.999, development.heatUnitIndex);
  logHUINowOverStartOfDecline := log10(safediv(1.0 - adjustedHUI, 1.0 -
    plantParams.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn));
  { if the plant is a tree, lai does not decline, but biomass does. }
  if (not plantParams.isTree) then 
    begin
    exponent := max(-10.0, plantParams.leafAreaIndexDeclineRateFactor * logHUINowOverStartOfDecline);
    result := min(development.leafAreaIndex, development.leafAreaIndexAtStartOfDecline * power(10.0, exponent));
    end;
  { calc new biomass adjustment }
  exponent := max(-10.0, plantParams.biomassAdjustmentIfLAIDecliningFactor * logHUINowOverStartOfDecline);
  constraints.biomassAdjustmentIfLAIDeclining := power(10.0, exponent);
  except on e: Exception do result := errorMessage('Exception in EQ.LeafAreaIndexDuringLeafDecline: ' + e.message); end;
  end;

{ 
Crop height

Crop height is estimated with the relationship [EQN: 263] where CHT
is the crop height in mm and HMX is the maximum height for crop j.
Equation:
  CHT = HMX * sqrt(HUF)
Code:
  same
Variables:
  CHT = EQ::PlantHeight_m
  HMX = maxHeight_m
  HUF = heatUnitFactorForLAIAndHeight
 }
{ EQN: 263 }
class function EQ.PlantHeight_m(height_m: single; maxHeight_m: single; heatUnitFactorForLAIAndHeight: single): single;
  begin
  try
  result := max(height_m, maxHeight_m * sqrt(heatUnitFactorForLAIAndHeight));
  except on e: Exception do result := errorMessage('Exception in EQ.PlantHeight_m: ' + e.message); end;
  end;

{ 
Root biomass and depth

The fraction of total biomass partitioned to the root system
normally decreases from 0.3 to 0.5 in the seedling to 0.05
to 0.20 at maturity (Jones, 1985). The model simulates this
partitioning by decreasing the fraction linearly from emergence
to maturity. Thus, the potential daily change in root weight
is computed with the equation [EQN: 264] where deltaRWT is
the change in root weight in t/ha and ar(1) and ar(2) are crop
parameters with typical values of ~0.4 and 0.2.
Equation:
  deltaRWT = deltaB(p) * (ar(1) - ar(2) * HUI)
Code:
  deltaRWT = B * (ar(1) - ar(2) * HUI) - RWT
  if deltaRWT < 0, deltaRWT = -0.01 * RWT
Variables:
  deltaRWT = EQ::ChangeInTotalRootWeight_tPha
  deltaB(p) = potentialIncreaseInBiomass_tPha
  B = newTotalPlantBiomass_tPha
  RWT = totalRootWeight_tPha
  ar(1) = fractionRootWtAtEmergence_frn
  ar(2) = fractionRootWtAtMaturity_frn
  HUI = heatUnitIndex
 }
{ EQN: 264 }
class function EQ.ChangeInTotalRootWeight_tPha(newTotalPlantBiomass_tPha: single; fractionRootWtAtEmergence_frn: single;
  fractionRootWtAtMaturity_frn: single; heatUnitIndex: single; totalRootWeight_tPha: single): single;
  begin
  try
  result := newTotalPlantBiomass_tPha * (fractionRootWtAtEmergence_frn - fractionRootWtAtMaturity_frn * heatUnitIndex)
    - totalRootWeight_tPha;
  if (result < 0.0) then result := -0.01 * totalRootWeight_tPha;
  except on e: Exception do result := errorMessage('Exception in EQ.ChangeInTotalRootWeight_tPha: ' + e.message); end;
  end;

{ 
The potential change in root weight through the root zone is
simulated as a function of plant water use in each layer of
soil with the equation [EQN: 265] where RW is the root weight
in soil layer l in t/ha, M is the total number of soil layers,
and u is the daily water use rate in layer l in mm/day.
Equation:
  deltaRW = deltaRTW * u / (sum with l from 1 to M of) u
Code:
  if deltaRTW < 0, deltaRW = deltaRTW * RW / RWT
  otherwise same
Variables:
  deltaRW = EQ::ChangeInRootWeightForLayer_tPha
  deltaRWT = changeInTotalRootWeight_tPha
  RW = rootWeightForLayer_tPha
  RWT = totalRootWeight_tPha
  u = plantWaterUseForLayer_mm
  U = totalPlantWaterUse_mm
  M = numLayers
 }
{ EQN: 265 }
class function EQ.ChangeInRootWeightForLayer_tPha(changeInTotalRootWeight_tPha: single; plantWaterUseForLayer_mm: single;
  totalPlantWaterUse_mm: single; rootWeightForLayer_tPha: single; totalRootWeight_tPha: single): single;
  begin
  try
  if (changeInTotalRootWeight_tPha <= 0.0) or (totalPlantWaterUse_mm <= 0.0) then
    result := changeInTotalRootWeight_tPha * safedivExcept(rootWeightForLayer_tPha, totalRootWeight_tPha, 0.0)
  else
    result := changeInTotalRootWeight_tPha * safedivExcept(plantWaterUseForLayer_mm, totalPlantWaterUse_mm, 0.0);
  except on e: Exception do result := errorMessage('Exception in EQ.ChangeInRootWeightForLayer_tPha: ' + e.message); end;
  end;

{ 
Rooting depth normally increases rapidly from the seedling depth
to a crop-specific maximum. In many crops, the maximum is usually
attained well before physiological maturity (Borg and Grimes, 1986).
Rooting depth is simulated as a function of heat units and potential
root zone depth: [EQN: 266] where RD is the root depth in m, RDMX
is the maximum root depth in m for crop j in ideal soil, and RZ
is the soil profile depth in m.
Equation:
  deltaRD = 2.5 * RDMX * deltaHUF, RD <= RZ
Code:
  RD = 2.5 * RDMX * HUI
  bounded by previous root depth, soil profile depth, max root depth, and crop height
Variables:
  RD = EQ::RootDepth_m
  RDMX = maxRootDepth_m
  HUF = heatUnitFactorForLAIAndHeight
  HUF(i-1) = yesterdaysHeatUnitFactorForLAIAndHeight
  RZ = soilProfileDepth_m
 }
{ EQN: 266 }
class function EQ.RootDepth_m(rootDepth_m: single; maxRootDepth_m: single; heatUnitIndex: single; lowestSoilLayerDepth_m:
  single; cropHeight_m: single): single;
  begin
  try
  result := 2.5 * maxRootDepth_m * heatUnitIndex;
  { must be longer than previous root depth }
  result := max(rootDepth_m, result);
  { must be longer than crop height }
  result := max(cropHeight_m, result);
  { must be less deep than soil layers }
  result := min(lowestSoilLayerDepth_m, result);
  { must be shorter than max root depth for crop }
  result := min(maxRootDepth_m, result);
  except on e: Exception do result := errorMessage('Exception in EQ.RootDepth_m: ' + e.message); end;
  end;

{ 
Am leaving section on yield (eqns. 267-269) until later section also on
crop yield (eqns. 313-315).

Water Use

The potential water use, E(p), is estimated as described in the
evapotranspiration section. The potential water use from the soil
surface to any root depth is estimated with the function
  U(p) = E(p) / (1.0 - exp(-lambda) * (1.0 - exp(-lambda * (Z/RZ)))
                                                        (Equation 270)
where U(p) is the total water use rate in mm/day to depth Z in
m on day i, RZ is the root zone depth in m, and lambda is a
water use distribution parameter.

The amount used in a particular layer can be calculated by taking
the difference between U(p) values at the layer boundaries:
  U(pl) = E(p) / (1.0 - exp(-lambda) * (1.0 - exp(-lambda * (Z(l)/RZ))
   ... - (1.0 - exp(-lambda) * (1.0 - exp(-lambda * (Z(l-1)/RZ))
                                                        (Equation 271)
where U(pl) is the potential water use rate for layer l in mm/day.

Equation 271 applies to a soil that provides poor conditions for root
development when lambda is set to a high value like 10. The high
lambda value gives high water use near the surface and very low
use in the lower half of the root zone. Since there is no provision
for water deficiency compensation in any layer, considerable water
stress may be incorrectly indicated if equation 271 is used. To
overcome this problem, equation 271 was modified to allow plants
to compensate for water deficiency in a layer by using more
water from other layers. Total compensation can be accomplished
by taking the difference between U(pl) at the bottom of a layer
and the sum of water use above a layer:
  U(pl) = E(p) / (1.0 - exp(-lambda) * (1.0 - exp(-lambda * (Z(l)/RZ))
  ... - the sum from 1 to l-1 of U(k)                    (Equation 272)
where U(k) is the actual water use rate in mm/day for all layers
above layer l. Thus, any deficit can be overcome if a layer that
is encountered has adequate water storage.

Neither equation 271 (no compensation) nor equation 272 (total
compensation) is satisfactory to simulate a wide range of
soil conditions. A combination of the two equations, however,
provides a very general water use function [EQN: 273] where
UC varies over a range (0.0-1.0) and is the water deficit
compensation factor. In soils with a good rooting environment,
UC = 1.0 gives total compensation. The other extreme, poor
conditions, allow no compensation (UC = 0.0). The procedure
for estimating UC is described in the Growth Constraints
section of this chapter.
Equation:
  u(p) = E(p) * (1 - exp(-lambda * (Z(l) / RZ)) - (1 - UC) * (1 - exp(-lambda * (Z(l-1) / RZ)))
         / (1 - exp(-lambda))
         - UC * (sum with k from 1 to l-1 of) u(k)
Code:
  multiplying E(p) / (1 - exp(-lambda)) over the difference, you get
  u(p) = E(p) * (1 - exp(-lambda * Z(l) / RZ) / (1 - exp(-lambda)
         - (1 - UC) * E(p) * (1 - exp(-lambda * Z(l-1) / RZ)) / (1 - exp(-lambda)
         - UC * (sum with k from 1 to l-1 of) u(k)

  so if WUT = E(p) * (1 - exp(-lambda * Z(l) / RZ)) / (1 - exp(-lambda))
  and WUT(l-1) = E(p) * (1 - exp(-lambda * Z(l-1) / RZ)) / (1 - exp(-lambda))
  then the equation reduces to
  u(p) = WUT - (1 - UC) * WUT(l-1)
         - UC * (sum with k from 1 to l-1 of) u(k)
  so it is exactly the same.
Variables:
  WUT = EQ::WaterDemandTermForLayer_mm
  E(p) = potPlantEvap_mm
  lambda = plantWaterUseDistribParam
  Z(l) = maxOfDepthThisLayerOrRootDepth_m
  RZ = soilProfileDepth_m
 }
{ EQN: 273 }
{ change from EPIC: this was water use, but since we added a water competition component where the soil patch partitions water
  to its plants, what the plant calculates as water use is actually now a request made to the soil patch.
  Hence the name change. }
class function EQ.WaterDemandTermForLayer_mm(potPlantEvap_mm: single; soilProfileDepth_m: single;
  maxOfDepthThisLayerOrRootDepth_m: single; rootDepth_m: single): single;
  var
    plantWaterDemandDistribParam: single;
    numerator: single;
    denominator: single;
  begin
  try
  plantWaterDemandDistribParam := soilProfileDepth_m * 5.0;
  numerator := potPlantEvap_mm * (1.0 - safeExp(-plantWaterDemandDistribParam * safediv(maxOfDepthThisLayerOrRootDepth_m,
    rootDepth_m)));
  denominator := (1.0 - safeExp(-plantWaterDemandDistribParam));
  result := safediv(numerator, denominator);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterDemandTermForLayer_mm: ' + e.message); end;
  end;

{ second part of EQN: 273 (see explanation above) }
class function EQ.UnconstrainedPlantWaterDemandForLayer_mm(waterUseTermForLayer_mm: single; deficitCompensFactor_frn: single;
  waterUseAboveThisLayer_mm: single; waterUseTermLastLayer_mm: single): single;
  begin
  try
  result := max(0.0, waterUseTermForLayer_mm - (1.0 - deficitCompensFactor_frn) * waterUseTermLastLayer_mm -
    deficitCompensFactor_frn * waterUseAboveThisLayer_mm);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.UnconstrainedPlantWaterDemandForLayer_mm: ' + e.message); end;
  end;

{ 
The potential water use in each layer calculated with equation 273 is
reduced when the soil water storage is less than 25% of plant-available
soil water (Jones and Kiniry, 1986) by using the equation [EQN: 274]
and [EQN: 275] where SW is the soil water content in layer l on day i
in mm and FC and WP are the soil water contents at field capacity
and wilting point for layer l.
Equation:
  if SW < (FC - WP) / 4 + WP
    u(l) = u(p(l)) * exp(5.0 * (4.0 * (SW - WP) / (FC - WP) - 1.0))
  else
    u(l) = u(p(l))
Code:
  this code treats the part in the exp as a variable which is later
  multiplied by u(p) (in EQ::PlantWaterDemandForLayer_mm)
  calling this variable WUC,
  WUC = 4.0 * (SW - WP) / (FC - WP)
  note there is no exp, no 5.0 and no - 1.0
Variables:
  WUC = EQ::WaterDemandConstraintForLayer_mm
  SW = waterContent_mm
  WP = lowerWaterLimit_mm
  FC = fieldCapacity_mm
 }
{ EQN: 274 EQN: 275 }
class function EQ.WaterDemandConstraintForLayer_mm(layer: integer; var layers: LayerStructureArray; lowerWaterLimit_mm:
  single): single;
  begin
  try
  result := min(1.0, 4.0 * safediv(layers[layer].waterContent_mm - lowerWaterLimit_mm, layers[layer].fieldCapacity_mm
    -lowerWaterLimit_mm));
  except on e: Exception do result := errorMessage('Exception in EQ.WaterDemandConstraintForLayer_mm: ' + e.message); end;
  end;

{ 
(This is inserted from later)
The final estimates of water use for each layer are obtained by multiplying
the u(l) values in equations 274 and 275 by RGF (the root growth constraint
factor) [EQN: 312].
Equation:
  u*(l) = u(l) * RGF
Code:
  same (incorporating the part of equation 274 where the water use constraint
  is multiplied by the unconstrained water use u(p))
Variables:
  u*(l) = EQ::PlantWaterDemandForLayer_mm
  u(l) = unconstrainedWaterDemand_mm
  RGF = rootGrowthConstraint_frn
 }
{ EQN: 312 }
class function EQ.PlantWaterDemandForLayer_mm(layer: integer; var layers: LayerStructureArray; unconstrainedWaterDemand_mm:
  single; waterDemandConstraint: single; lowerWaterLimit_mm: single; rootGrowthConstraint_frn: single): single;
  var
    waterThatCanBeTakenUp_mm: single;
  begin
  try
  waterThatCanBeTakenUp_mm := layers[layer].waterContent_mm - lowerWaterLimit_mm;
  if (unconstrainedWaterDemand_mm >= waterThatCanBeTakenUp_mm) then 
    result := max(0.0, waterThatCanBeTakenUp_mm * waterDemandConstraint * rootGrowthConstraint_frn)
  else
    result := max(0.0, unconstrainedWaterDemand_mm * waterDemandConstraint * rootGrowthConstraint_frn);
  except on e: Exception do result := errorMessage('Exception in EQ.PlantWaterDemandForLayer_mm: ' + e.message); end;
  end;

{ 
Nutrient Uptake - Nitrogen

Supply and Demand

Crop use of N is estimated by using a supply and demand approach. The
daily crop N demand is the difference between the crop N content and
the ideal N content for that day. The demand is estimated with the
equation [EQN: 276] where UND is the N demand rate of the crop in
kg/ha*day, c(NB) is the optimal N concentration of the crop in
kg/t, B is the accumulated biomass in t/ha for day i, and UN is the
actual N uptake rate in kg/hg*day.
Equation:
  UND = c(NB) * B - (sum with k from 1 to i-1 of) UN
Code:
  UND = min(4 * bn(3) * deltaB(p), c(NB) * B - (sum with k from 1 to i-1 of) UN)
Variables:
  UND = PlantNitrogenDemandForOptimalGrowth_kgPha
  c(NB) = plantOptimalNConc_kgPkg
  B = totalPlantBiomass_tPha
  sum of UN(k) = nInLiveBiomass_kgPha
 }
{ EQN: 276 }
class function EQ.PlantNitrogenDemandForOptimalGrowth_kgPha(plantOptimalNConc_kgPkg: single; totalPlantBiomass_tPha:
  single; nInLiveBiomass_kgPha: single; optimalNConcParams2: single; potentialIncreaseInBiomass_tPha: single): single;
  var
    optimalNInLiveBiomass_kgPha: single;
  begin
  try
  optimalNInLiveBiomass_kgPha := plantOptimalNConc_kgPkg * (totalPlantBiomass_tPha + potentialIncreaseInBiomass_tPha) *
    t_to_kg;
  if (optimalNInLiveBiomass_kgPha < nInLiveBiomass_kgPha) then
    optimalNInLiveBiomass_kgPha := nInLiveBiomass_kgPha;
  result := min(4.0 * optimalNConcParams2 * (potentialIncreaseInBiomass_tPha * t_to_kg),
      optimalNInLiveBiomass_kgPha - nInLiveBiomass_kgPha);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.PlantNitrogenDemandForOptimalGrowth_kgPha: ' + e.message); end;
  end;

{ 
Optimal N concentration for crop

The optimal crop N concentration declines with increasing growth
stage (Jones, 1983a) and is computed as a function of growth stage
by using the equation [EQN: 277] where bn(1), bn(2) and bn(3)
are crop parameters expressing N concentration and HUI (heat unit
index) is the fraction of the growing season.
Equation:
  c(NB) = bn(1) + bn(2) * exp(-bn(3) * HUI)
Code:
  c(NB) = (bn(1) - bn(3)) * (1.0 - HUI / (HUI + exp(bn(1) - bn(2) * HUI))) + bn(3)
Variables:
  c(NB) = EQ::PlantoptimalNConc_kgPkg
  bn(1) = plantOptimalNConcParams[0]
  bn(2) = plantOptimalNConcParams[1]
  bn(3) = plantOptimalNConcParams[2]
  HUI = heatUnitIndex
 }
{ EQN: 277 }
class function EQ.PlantoptimalNConc_kgPkg(var plantParams: PlantParamsStructure; heatUnitIndex: single): single;
  var
    adjustedHeatUnitIndex: single;
  begin
  try
  adjustedHeatUnitIndex := min(1.0, heatUnitIndex);
  with plantParams do
    result := (nFractionAtEmergence_frn - nFractionAtMaturity_frn)
      * (1.0 - scurve(adjustedHeatUnitIndex, optimalNParams.c1, optimalNParams.c2)) + nFractionAtMaturity_frn;
  except on e: Exception do result := errorMessage('Exception in EQ.PlantoptimalNConc_kgPkg: ' + e.message); end;
  end;

{ 
Supply of N in the soil layers

Soil supply of N is assumed to be limited by mass flow of NO3-N to the roots
[EQN: 278] where UN is the rate of N supplied by the soil in kg/ha*day,
WNO3 is the amount of NO3-N in kg/ha, SW is the soil water content in mm,
u is water use rate in mm/day, and subscript l refers to the soil layers.
Equation:
  UN(l) = u(l) * WNO3 / SW
Code:
  same
Variables:
  UN(l) = EQ::NSupplyForLayer_kgPha
  u(l) = plantWaterUse_mm
  WNO3 = nitrate_kgPha
  SW = waterContent_mm
 }
{ EQN: 278 }
class function EQ.NSupplyForLayer_kgPha(plantWaterUse_mm: single; nitrate_kgPha: single; waterContent_mm: single): single;
  begin
  try
  if (waterContent_mm < kLowestFloatAboveZero) then 
    result := 0.0
  else
    { change from EPIC: added upper bound of 0.9 * nitrate to n supply (now is similar to P) }
    result := max(0.9 * nitrate_kgPha, safediv(plantWaterUse_mm, waterContent_mm) * nitrate_kgPha);
  except on e: Exception do result := errorMessage('Exception in EQ.NSupplyForLayer_kgPha: ' + e.message); end;
  end;

{ 
Totaling the N available in the layers

The total mass flow supply is estimated by summing the layer supplies:
[EQN: 279] where UNS is the N supply rate from soil to plants in kg/ha.
 }
{ EQN: 279 }
{ not needed. done in code. }
{ 
Adjusting the N available in the layers for high or low amounts

Since mass flow uptake can produce questionable results when N
concentrations are extremely high or low, UN values obtained from
equation 278 are adjusted: [EQN: 280]. Equation 280 assures that
actual N uptake cannot exceed the plant demand when mass flow
estimates are too large. It also provides for increased N supply
when mass flow estimates are too low despite the availability of NO3.
Equation:
  UN(a) = UN * UND / UNS, UN(a) <= WNO3
Code:
  seemingly much different.
Variables:
  UN(a) = EQ::ActualNUptakeByLayer_kgPha
  UN = nUptake_kgPha
  UND = nDemand_kgPha
  UNS = totalSupply_kgPha
  WNO3 = nitrate_kgPha
 }
{ EQN: 280 }
(*
  the function as it is in the EPIC code (translated)
float EQ::ActualNUptakeByLayer_kgPha(float demand_kgPha, float totalSupply_kgPha,
  int numLayersWithRootsInThem,  PlantLayerStructureArray* plantLayers,
  LayerStructureArray* layers)
  {
  int layer;
  float totalUptake_kgPha = 0.0;
  float remainingDemandMSupply_kgPha = 0.0, wholeDemandMWholeSupply_kgPha = 0.0;
  float demandThisLayer_kgPha = 0.0;

  if (totalSupply_kgPha == 0.0) return 0.0;

  if (demand_kgPha > totalSupply_kgPha) {

    // unresolved: unclear
    // this section is translated correctly, but I don't understand the logic of it.
    remainingDemandMSupply_kgPha = demand_kgPha - totalSupply_kgPha;
    wholeDemandMWholeSupply_kgPha = remainingDemandMSupply_kgPha;

    for (layer = 1; layer <= numLayersWithRootsInThem; layer++) {
      demandThisLayer_kgPha = plantLayers[layer].nSupply_kgPha + remainingDemandMSupply_kgPha;

      if (demandThisLayer_kgPha < layers[layer].nitrate_kgPha) {
        plantLayers[layer].nUptake_kgPha = demandThisLayer_kgPha;
        totalUptake_kgPha += wholeDemandMWholeSupply_kgPha;
        return totalUptake_kgPha;
        }
      else {
        plantLayers[layer].nUptake_kgPha = layers[layer].nitrate_kgPha;
        totalUptake_kgPha += plantLayers[layer].nUptake_kgPha;
        remainingDemandMSupply_kgPha += plantLayers[layer].nSupply_kgPha
          - layers[layer].nitrate_kgPha;
        }
      }
    }
  else {
    // if more supply than demand, figure uptake for each layer based on ratio of
    // supply for each layer to total supply
    for (layer = 1; layer <= numLayersWithRootsInThem; layer++) {
      plantLayers[layer].nUptake_kgPha = safediv(plantLayers[layer].nSupply_kgPha,
        totalSupply_kgPha) * demand_kgPha;
      totalUptake_kgPha += plantLayers[layer].nUptake_kgPha;
      }
    }
  return totalUptake_kgPha;
  }
 }
*)

{ the function as we are using it }
{ change from EPIC: this was N uptake,
  but since we added a nutrient competition component where the soil patch partitions N and P
  to its plants, what the plant calculates as uptake is actually now a request made to the soil patch.
  Hence the name change. }
class function EQ.NDemandByLayer_kgPha(var nutrients: NutrientsStructure; var biomass: BiomassStructure;
    var layers: LayerStructureArray): single;
  var
    layer: integer;
    totalDemand_kgPha, proportion: single;
  begin
  try
  totalDemand_kgPha := 0.0;
  proportion := 0.0;
  if ((nutrients.nSupply_kgPha <= 0.0) or (nutrients.nDemandForOptimalGrowth_kgPha <= 0.0)) then
    begin
    result := 0.0;
    exit;
    end;
  if biomass.numLayersWithRootsInThem > 0 then for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
    begin
    if (nutrients.nDemandForOptimalGrowth_kgPha < nutrients.nSupply_kgPha) then
      begin
      proportion := safediv(nutrients.nSupplyByLayer_kgPha[layer], nutrients.nSupply_kgPha);
      nutrients.nDemandByLayer_kgPha[layer] := proportion * nutrients.nDemandForOptimalGrowth_kgPha;
      end
    else 
      { demand >= supply }
      nutrients.nDemandByLayer_kgPha[layer] := nutrients.nSupplyByLayer_kgPha[layer];
    addQuantity(totalDemand_kgPha, nutrients.nDemandByLayer_kgPha[layer]);
    end;
  result := totalDemand_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.NDemandByLayer_kgPha: ' + e.message); end;
  end;

{ 
Nitrogen Fixation (for legumes)

Daily N fixation is estimated as a fraction of daily plant N
uptake for legumes: [EQN: 281] where WFX is the amount of N
fixation in kg/ha and FXR is the fraction of uptake for day i.
Equation:
  WFX = FXR * UN, WFX <= 6.0
Code:
  no bound here, but it is bounded at 20 kg/ha later in the code
Variables:
  WFX = EQ::NFixation_kgPha
  FXR = nFixationFraction_frn
  UN = nDemand_kgPha
 }
{ EQN: 281 }
class function EQ.NFixation_kgPha(nFixationFraction_frn: single; nDemand_kgPha: single): single;
  begin
  try
  result := nFixationFraction_frn * nDemand_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.NFixation_kgPha: ' + e.message); end;
  end;

{ 
The fraction, FXR, is estimated as a function of soil NO3 content
and water content and plant growth stage [EQN: 282] where FXG is
the plant growth stage factor, FXW is the soil water content factor,
and FXN is the soil NO3 content factor.
Equation:
  FXR = min(1.0, FXW, FXN) * FXG
Code:
  same
Variables:
  FXR = EQ::NFixationFraction_frn
  FXW = nFixationFractionWaterFactor
  FXN = nFixationFractionNitrateFactor
  FXG = nFixationFractionGrowthStageFactor
 }
{ EQN: 282 }
class function EQ.NFixationFraction_frn(nFixationFractionWaterFactor: single; nFixationFractionNitrateFactor: single;
  nFixationFractionGrowthStageFactor: single): single;
  begin
  try
  result := min(1.0, min(nFixationFractionWaterFactor, nFixationFractionNitrateFactor)) *
    nFixationFractionGrowthStageFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.NFixationFraction_frn: ' + e.message); end;
  end;

{ 
The growth stage factor inhibits N fixation in young plants prior to
development of functional nodules and in old plants with senescent
nodules (Patterson and LaRue, 1983) [EQN: 283], [EQN: 284], [EQN: 285]
and [EQN: 286] where HUI is the heat unit index for day i.
Equation:
  if HUI <= 0.15 or HUI > 0.75, FXG = 0.0
  if 0.15 < HUI <= 0.3, FXG = 6.67 * HUI - 1.0
  if 0.3 < HUI <= 0.55, FXG = 1.0
  if 0.55 < HUI <= 0.75, FXG = 3.75 - 5.0 * HUI
Code:
  FXG = min((HUI - 0.1 * 5), min(4 - 5 * HUI, 1))
  the first case (HUI < 0.15 or > 0.75) is avoided by an if statement outside of this
  the other three cases match (not exactly) the cases below
Variables:
  FXG = EQ::NFixationFractionGrowthStageFactor
  HUI = heatUnitIndex
 }
{ EQN: 283 EQN: 284 EQN: 285 EQN: 286 }
class function EQ.NFixationFractionGrowthStageFactor(heatUnitIndex: single): single;
  begin
  try
  result := min((heatUnitIndex - 0.1) * 5.0, min(4.0 - 5.0 * heatUnitIndex, 1.0));
  except on e: Exception do result := errorMessage('Exception in EQ.NFixationFractionGrowthStageFactor: ' + e.message); end;
  end;

{
The soil water content factor reduces N fixation when the water content
at the top 0.3 m of soil is less than 85% of field capacity (Albrecht
et al., 1984, Bouniols et al., 1991) using the equation [EQN: 287]
where SW3, WP3, and FC3 are the water contents in the top 0.3 m of soil
on day i, at wilting point, and at field capacity.
Equation:
  if SW3 < 0.85 * (FC3 - WP3) + WP3,
    FXW = (SW3 - WP3) / (0.85 * (FC3 - WP3))
Code:
  FXW = 1.3 * (SW3 - WP3) / (FC3 - WP3) - 0.3
Variables:
  FXW = NFixationFractionWaterFactor
  SW3 = soilWaterContentTop30cm_mm
  WP3 = wiltingPointTop30cm_mm
  FC3 = fieldCapacityTop30cm_mm
 }
{ EQN: 287 }
class function EQ.NFixationFractionWaterFactor(top30cmFractionFieldCapacity_frn: single): single;
  begin
  try
  result := 1.333 * top30cmFractionFieldCapacity_frn - 0.333;
  except on e: Exception do result := errorMessage('Exception in EQ.NFixationFractionWaterFactor: ' + e.message); end;
  end;

{ 
The amount of NO3 in the root zone can affect N fixation (Harper, 1976;
Bouniols et al., 1985) and determines the soil NO3 factor, FXN [EQN: 288],
[EQN: 289] and [EQN: 290] where WNO3 is the weight of NO3-N in the
root zone in kg/ha*m and RD is the root depth in m. This approach
reduces N fixation when the NO3-N content of the root zone is greater
than 100 kg/ha*m and prohibits N fixation at N contents greater than
300 kg/ha*m.
Equation:
  if WNO3 > 300, FXN = 0
  if 100 < WNO3 < 300, FXN = 1.5 - 0.005 * WNO3 / RD
  if WNO3 < 100, FXN = 1.0
Code:
  bounds checking is not as complex in code
Variables:
  FXN = NFixationFractionNitrateFactor
  WNO3 = totalNitrateInRootZone_kgPha
  RD = rootDepth_m
 }
{ EQN: 288 EQN: 289 EQN: 290 }
class function EQ.NFixationFractionNitrateFactor(totalNitrateInRootZone_kgPha: single; rootDepth_m: single): single;
  begin
  try
  result := max(0.0, 1.5 - 0.005 * safediv(totalNitrateInRootZone_kgPha, rootDepth_m));
  except on e: Exception do result := errorMessage('Exception in EQ.NFixationFractionNitrateFactor: ' + e.message); end;
  end;

{ 
Phosphorus

Crop use of P is estimated with the supply and demand approach
described in the N model. The daily demand is computed with equation
276 written in the form [EQN: 291] where UPD is the P demand for the
plant in kg/ha, UP is the actual P uptake in kg/ha, and c(PB) is
the optimal P concentration for the plant.
Equation:
  UPD = c(PB) * B - (sum with k from 1 to i-1 of) UP(k)
Code:
  same
Variables:
  UPD = PlantPhosphorusDemand_kgPha
  c(PB) = plantOptimalPConc_kgPkg
  B = totalPlantBiomass_tPha
  UP(k) = plantPUptake_kgPha
  sum of UP(k) = lifeTotalPlantPUptake_kgPha (summed daily)
 }
{ EQN: 291 }
class function EQ.PlantPhosphorusDemandForOptimalGrowth_kgPha(optimalPConc_kgPkg: single; totalPlantBiomass_tPha: single;
  pInLiveBiomass_kgPha: single; potentialIncreaseInBiomass_tPha: single): single;
  var
    optimalPInLiveBiomass_kgPha: single;
  begin
  try
  optimalPInLiveBiomass_kgPha := max(pInLiveBiomass_kgPha, optimalPConc_kgPkg * (totalPlantBiomass_tPha +
    potentialIncreaseInBiomass_tPha) * t_to_kg);
  result := optimalPInLiveBiomass_kgPha - pInLiveBiomass_kgPha;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.PlantPhosphorusDemandForOptimalGrowth_kgPha: ' + e.message); end;
  end;

{ 
As in the case of N, the optimal plant P concentration is computed with
equation 277 written in the form [EQN: 292] where bp(1), bp(2), and
bp(3) are parameters calculated from crop-specific optimum P concentration
at the seedling stage, halfway through the season and at maturity.
Equation:
  c(PB) = bp(1) + bp(2) * exp(-bp(3) * HUI)
Code:
  c(PB) = (bp(1) - bp(3)) * (1.0 - HUI / (HUI + exp(bp(1) - bp(2) * HUI))) + bp(3)
Variables:
  c(PB) = PlantoptimalPConc_kgPkg
  bp(1) = plantOptimalPConcParams[0]
  bp(2) = plantOptimalPConcParams[1]
  bp(3) = plantOptimalPConcParams[2]
  HUI = heatUnitIndex
 }
{ EQN: 292 }
class function EQ.PlantOptimalPConc_kgPkg(var plantParams: PlantParamsStructure; heatUnitIndex: single): single;
  var
    adjustedHeatUnitIndex: single;
  begin
  try
  adjustedHeatUnitIndex := min(1.0, heatUnitIndex);
  with plantParams do
    result := (pFractionAtEmergence_frn - pFractionAtMaturity_frn)
      * (1.0 - scurve(adjustedHeatUnitIndex, optimalPParams.c1, optimalPParams.c2)) + pFractionAtMaturity_frn;
  except on e: Exception do result := errorMessage('Exception in EQ.PlantoptimalPConc_kgPkg: ' + e.message); end;
  end;

{ 
Soil supply of P is estimated using the equation [EQN: 293] where
UPS is the amount of P supplied by the soil in kg/ha, LF(u) is the
labile P factor for uptake, RW is the root weight in layer l
in kg/ha, and RWT is the total root weight on day i in kg/ha.
The constant 1.5 allows 2/3 of the roots to meet the P demand of
the plant if labile P is not limiting. This approach is consistent
with studies suggesting that roots of P-deficient plants (or plants
whose root systems have been pruned) can absorb P faster than
the roots of normal plants (Andrews and Norman, 1970. DeJager,
1979. Jungk and Barber, 1974).
Equation:
  UPS = 1.5 * UPD * (sum with l from 1 to M of) LF(u) * RW / RWT
Code:
  LF(u) is for layer of interest only, not summed
  if UPS > labile p in layer, UPS = 0.9 * labile p in layer
Variables:
  UPS = EQ::PSupplyForLayer_kgPha
  UPD = pDemand_kgPha
  LF(u) = labilePUptakeFactor_frn
  RW = rootWeight_tPha (layer)
  RWT = totalRootWeight_tPha
 }
{ EQN: 293 }
class function EQ.PSupplyForLayer_kgPha(layer: integer; var nutrients: NutrientsStructure; var biomass: BiomassStructure;
    var layers: LayerStructureArray; labilePUptakeFactor_frn: single): single;
  begin
  try
  result := 0.0;
  if (biomass.totalRootWeight_tPha <= 0.0) then
    begin
    result := 0.0;
    exit;
    end;
  result := safediv(1.5 * nutrients.pDemandForOptimalGrowth_kgPha
    * labilePUptakeFactor_frn * biomass.rootWeightByLayer_tPha[layer], biomass.totalRootWeight_tPha);
  if (result >= layers[layer].labileP_kgPha) then
    result := 0.9 * layers[layer].labileP_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.PSupplyForLayer_kgPha: ' + e.message); end;
  end;

{ from code }
{ change from EPIC: this was P uptake,
  but since we added a nutrient competition component where the soil patch partitions N and P
  to its plants, what the plant calculates as uptake is actually now a request made to the soil patch.
  Hence the name change. }
class function EQ.PDemandByLayer_kgPha(var nutrients: NutrientsStructure; var biomass: BiomassStructure;
    var layers: LayerStructureArray): single;
  var
    layer: integer;
    totalPDemand_kgPha, remainingOptDemandMSupply_kgPha, wholeOptDemandMWholeSupply_kgPha: single;
    optDemandThisLayer_kgPha: single;
  begin
  try
  totalPDemand_kgPha := 0.0;
  { unresolved: unclear }
  { like actual n uptake, this function is correctly translated but not understood. }
  remainingOptDemandMSupply_kgPha := nutrients.pDemandForOptimalGrowth_kgPha - nutrients.pSupply_kgPha;
  wholeOptDemandMWholeSupply_kgPha := remainingOptDemandMSupply_kgPha;
  if biomass.numLayersWithRootsInThem > 0 then for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
    begin
    optDemandThisLayer_kgPha := nutrients.pSupplyByLayer_kgPha[layer] + remainingOptDemandMSupply_kgPha;
    if (optDemandThisLayer_kgPha < layers[layer].labileP_kgPha) then
      begin
      nutrients.pDemandByLayer_kgPha[layer] := optDemandThisLayer_kgPha;
      addQuantity(totalPDemand_kgPha, wholeOptDemandMWholeSupply_kgPha);
      result := totalPDemand_kgPha;
      exit;
      end;
    nutrients.pDemandByLayer_kgPha[layer] := layers[layer].labileP_kgPha;
    addQuantity(totalPDemand_kgPha, nutrients.pDemandByLayer_kgPha[layer]);
    remainingOptDemandMSupply_kgPha := remainingOptDemandMSupply_kgPha + (nutrients.pSupplyByLayer_kgPha[layer] -
      layers[layer].labileP_kgPha);
    end;
  result := totalPDemand_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.PDemandByLayer_kgPha: ' + e.message); end;
  end;

{ 
The labile P factor for uptake ranges from 0.0 to 1.0 according
to the equation [EQN: 294] where c(LP) is the labile P concentration
in soil layer l in g/t. Equation 294 allows optimum uptake rates
when c(LP) is above 20 g/t. This is consistent with critical labile
P concentrations for a range of crops and soils (Sharpley et al.,
1990). Sharpley et al. (1984, 1985) described methods of
estimating c(LP) from soil test P and other soil characteristics.
Equation:
  LF = 0.1 + 0.9 * c(LP) / (c(LP) + exp(8.01 - 0.360 * c(LP)))
Code:
  same except if c(LP) > 30, LF = 1.0
Variables:
  LF(u) = EQ::LabilePUptakeFactorForLayer_frn
  c(LP) = labilePConc_gPt
 }
{ EQN: 294 }
class function EQ.LabilePUptakeFactorForLayer_frn(labilePConc_gPt: single; var pUptakeCoeffs: SCurveStructure): single;
  begin
  try
  if (labilePConc_gPt > 30.0) then 
    result := 1.0
  else
    result := scurve(labilePConc_gPt, pUptakeCoeffs.c1, pUptakeCoeffs.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.LabilePUptakeFactorForLayer_frn: ' + e.message); end;
  end;

{ 
Growth Constraints

Potential crop growth and yield are usually not achieved because
of constratints imposed by the plant environment. The model
estimates stresses caused by water, nutrients, temperature,
aeration, and radiation. These stresses range from 0.0 to 1.0
and affect plants in several ways. In EPIC, the stresses are
considered in estimating constraints on biomass accumulation,
root growth, and yield. The biomass constraint is the minimum
of the water, nutrient, temperature, and aeration stresses.
The root growth constraint is the minimum of soil strength,
temperature, and aluminum toxicity. Though topsoil aluminum
toxicity can have a direct effect on shoot growth, EPIC
simulates only its indirect effects through its inhibition
of root growth and water use. A description of the stress
factors involved in determining each constraint follows.

So it is:
  biomass constraint - smallest of
    water
    N
    P
    temperature
    aeration
  root growth constraint - smallest of
    soil strength
    temperature
    aluminum toxicity

Biomass constraints

The potential biomass predicted with equation 256 is adjusted
daily if any of the five plant stress factors is less than 1.0
using the equation [EQN: 295] where REG is the crop growth
regulating factor (the minimum stress factor).
Equation:
  deltaB = deltaB(p) * REG
Code:
  added factors for biomassAdjustmentIfLAIDeclining, dayLengthIsAboveThresholdForGrowth
Variables:
  deltaB = EQ::PlantIncreaseInBiomass_tPha
  deltaB(p) = potentialIncreaseInBiomass_tPha
  REG = biomassGrowthConstraint_frn
 }
{ EQN: 295 }
class function EQ.PlantIncreaseInBiomass_tPha(potentialIncreaseInBiomass_tPha: single; biomassGrowthConstraint_frn: single;
  biomassAdjustmentIfLAIDeclining: single; dayLengthIsAboveThresholdForGrowth: boolean): single;
  begin
  try
  if dayLengthIsAboveThresholdForGrowth then
    result := potentialIncreaseInBiomass_tPha * biomassGrowthConstraint_frn * biomassAdjustmentIfLAIDeclining
  else
    result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EQ.PlantIncreaseInBiomass_tPha: ' + e.message); end;
  end;

{ 
Water stress factor

The water stress factor is computed by considering supply and
demand in the equation [EQN: 296] where WS is the water stress
factor, u is the water use in layer l, and E(p) is the
potential plant water use on day i. This is consistent with the
concept that drought stress biomass production is in proportion
to transpiration reduction (Hanks, 1983).
Equation:
  WS = (sum with l from 1 to M of) u / E(p)
Code:
  same (with bounds)
Variables:
  WS = EQ::WaterStressFactor_frn
  sum of u = totalPlantWaterUse_mm
  E(p) = potPlantEvap_mm
 }
{ EQN: 296 }
class function EQ.WaterStressFactor_frn(totalPlantWaterUse_mm: single; potPlantEvap_mm: single): single;
  begin
  try
  if (potPlantEvap_mm = 0.0) then 
    result := 1.0
  else
    result := max(0.0, min(1.0, safediv(totalPlantWaterUse_mm, potPlantEvap_mm)));
  except on e: Exception do result := errorMessage('Exception in EQ.WaterStressFactor_frn: ' + e.message); end;
  end;

{ 
Temperature stress factor

The plant temperature stress is estimated with the equation [EQN: 297]
where TS is the plant temperature stress factor, TG is the average
daily soil surface temperature in degrees C, T(b) is the base
temperature for crop j, and T(o) is the optimal temperature for
crop j. Equation 297 produces symmetrical plant growth stress about
the optimal temperature and is driven by average daily soil surface
temperature. This approach allows growth of small plants to respond
realistically to low soil surface temperature found in temperate
regions in the spring. The presence of soil residues can retard
simulated soil warming and reduce crop growth. As the crop canopy
develops, it shades the soil surface, and simulated average soil
surface temperature approaches average air temperature.
In addition to the temperature stress imposed by equation 297,
plant growth does not occur on days when the average daily
temperature exceeds 1.5 * T(o).
Equation:
  if T > 1.5 * T(o) TS = 0
  else TS = sin(pi / 2 * (TG - T(b)) / (T(o) - T(b)))
Code:
  if T < T(b) or T > 1.5 * T(o) TS = 0
  else PR = (T - T(b)) / (T(o) - T(b))
    if PR > 2 TS = 0
    else TS = sin(pi / 2 * PR)
Variables:
  TS = EQ::BiomassTempStressFactor_frn
  TG = soilSurfaceTemp_degC
  T = meanTempForDay_degC
  T(b) = baseTemp_degC
  T(o) = optimalTemp_degC
  PR = proportionOfRange
 }
{ EQN: 297 }
class function EQ.BiomassTempStressFactor_frn(meanTempForDay_degC: single; baseTemp_degC: single; optimalTemp_degC: single):
  single;
  var
    proportionOfRange: single;
  begin
  try
  if ((meanTempForDay_degC <= baseTemp_degC) or (meanTempForDay_degC > 1.5 * optimalTemp_degC)) then 
    begin
    result := 0.0;
    exit;
    end;
  proportionOfRange := safedivExcept(meanTempForDay_degC - baseTemp_degC, optimalTemp_degC - baseTemp_degC, 0.0);
  if (proportionOfRange > 2.0) then 
    begin
    result := 0.0;
    exit;
    end;
  result := kPi / 2.0 * proportionOfRange;
  if (result > kPi) then result := result - kPi;
  result := sin(result);
  except on e: Exception do result := errorMessage('Exception in EQ.BiomassTempStressFactor_frn: ' + e.message); end;
  end;

{ 
Nutrient Stress - nitrogen

The N and P stress factors are based on the ratio of simulated plant
N and P contents to the optimal values. The stress factors vary
non-linearly from 1.0 at optimal N and P contents to 0.0 when N or
P is half the optimal level (Jones, 1983a). In the case of N,
the scaling equation is [EQN: 298] where SN(s) is a scaling
factor for the N stress factor, c(NB) is the optimal N concentration
of the crop on day i, B is the accumulated biomass in kg/ha,
and UN is the crop N uptake on day k in kg/ha.
Equation:
  SN = 200 * ((sum with k from 1 to i of) UN / (c(NB) * B) - 0.5)
Code:
  same
  this equation is also used for p stress scaling factor
  (note changed later to remove 100 multiplier)
Variables:
  SN(s) = EQ::NOrPStressFactorScalingFactor
  c(NB) = optimalConc_kgPkg
  B = totalPlantBiomassInclRoot_tPha
  UN = totalUptake_kgPha
 }
{ EQN: 298 }
class function EQ.NOrPStressFactorScalingFactor(totalUptake_kgPha: single; optimalConc_kgPkg: single;
  totalPlantBiomassInclRoot_tPha: single): single;
  var
    optimalCumUptake_kgPha: single;
  begin
  try
  optimalCumUptake_kgPha := optimalConc_kgPkg * totalPlantBiomassInclRoot_tPha * t_to_kg;
  if totalUptake_kgPha >= optimalCumUptake_kgPha then
    result := 1.0
  else
    result := 2.0 * safediv(totalUptake_kgPha, optimalCumUptake_kgPha - 0.5);
  except on e: Exception do result := errorMessage('Exception in EQ.NOrPStressFactorScalingFactor: ' + e.message); end;
  end;

{ 
The N stress factor is computed with the equation [EQN: 299] where
SN is the N stress factor for day i.
Equation:
  SN = SN(s) / (SN(s) + exp(3.52 - 0.026 * SN(s)))
Code:
  same
  this equation is also used for p stress factor
Variables:
  SN = EQ::NOrPStressFactor_frn
  SN(s) = stressFactorScalingFactor
 }
{ EQN: 299 }
class function EQ.NOrPStressFactor_frn(scalingFactor: single; var plantNAndPStressCoeffs: SCurveStructure): single;
  begin
  try
  if (scalingFactor < 0.0) then 
    result := 0.0
  else
    result := scurve(scalingFactor, plantNAndPStressCoeffs.c1, plantNAndPStressCoeffs.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.NOrPStressFactor_frn: ' + e.message); end;
  end;

{ 
Nutrient stress factor - Phosphorus

The P stress factor, SP, is computed with equations 298 and 299
written in P terms. (Use same functions)

Aeration stress

When soil water content approaches saturation, plants may suffer
from aeration stress. The water content of the top 1 m of soil
is considered in estimating the degree of stress [EQN: 300] and
[EQN: 301] where SAT is the saturation factor, SW1 is the water
content of the top 1 m of soil in mm, PO1 is the porosity of the
top 1 m of the soil in mm, CAF is the critical aeration factor
for crop j (~~ 0.85 for many crops), and AS is the aeration stress
factor.
Equation:
  SAT = 100 * (SW1 / PO1 - CAF) / (1 - CAF)
  AS = 1.0 - SAT / (SAT + exp(5.1 - 0.082 * SAT))
Code:
  same
  (note later changed SAT so it is not * 100 because changed X portion of scurve to get rid of multiplier)
Variables:
  AS = EQ::AerationStressFactor_frn
  SAT = saturationFactor
  SW1 = top1MWaterContent_mm
  PO1 = top1mPorosity_mm
  CAF = criticalAerationFactor
 }
{ EQN: 300 EQN: 301 }
class function EQ.AerationStressFactor_frn(top1MWaterContent_mm: single; top1mPorosity_mm: single; criticalAerationFactor:
  single; var rootGrowthRestrictionByAerationStress: SCurveStructure): single;
  var
    soilWaterOverPorosity: single;
    saturationFactor: single;
  begin
  try
  soilWaterOverPorosity := safediv(top1MWaterContent_mm, top1mPorosity_mm);
  saturationFactor := safediv(soilWaterOverPorosity - criticalAerationFactor, 1.0 - criticalAerationFactor);
  if (saturationFactor < 0.0) then 
    result := 1.0
  else
    result := 1.0 - scurve(saturationFactor, rootGrowthRestrictionByAerationStress.c1,
      rootGrowthRestrictionByAerationStress.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.AerationStressFactor_frn: ' + e.message); end;
  end;

{ 
Finally, the value of REG is determined as the lowest of the stress
factors, WS, TS, SN, SP, and AS.
(no equation)
 }
class function EQ.BiomassGrowthConstraintFactor_frn(var constraints: ConstraintsStructure): single;
  begin
  try
  result := 1.0;
  result := min(result, constraints.biomassTempStressFactor_frn);
  result := min(result, constraints.aerationStressFactor_frn);
  result := min(result, constraints.waterStressFactor_frn);
  result := min(result, constraints.pStressFactor_frn);
  result := min(result, constraints.nStressFactor_frn);
  result := max(result, 0.0);
  except on e: Exception do result := errorMessage('Exception in EQ.BiomassGrowthConstraintFactor_frn: ' + e.message); end;
  end;

{ 
Root Growth Constraints

As described in equation 265, root growth is proportional to water use.
Water use from a soil layer is estimated as a function of soil depth,
water content, and a compensation factor according to equations 273
and 274. Soil strength, temperature, and aluminum toxicity stress factors
are calculated from soil properties. The lowest of these three stress
factors constrains root growth by governing the water use compensation
factor (UC).

Temperature stress on root growth

Cold soil temperatures may limit root growth, especially when subsoil
layers warm slowly in the spring (Taylor, 1983). The temperature
stress for each soil layer is computed by substituting soil temperature
at the center of the layer for soil surface temperature in equation 297.
Equation:
  there is not a separate root temp stress equation in the chapter
Code:
  RS = sin(pi/2 * ST / OT)
Variables:
  RS = EQ::RootTempStressFactorForLayer_frn
  ST = soilTemp_degC
  OT = optimalTemp_degC
 }
{ EQN: 297 }
class function EQ.RootTempStressFactorForLayer_frn(soilTemp_degC: single; optimalTemp_degC: single): single;
  var
    soilTempOverOptimalTemp: single;
  begin
  try
  soilTempOverOptimalTemp := safediv(soilTemp_degC, optimalTemp_degC);
  if (soilTempOverOptimalTemp < 1.0) then 
    result := sin(kPi / 2 * soilTempOverOptimalTemp)
  else
    result := 1.0;
  except on e: Exception do result := errorMessage('Exception in EQ.RootTempStressFactorForLayer_frn: ' + e.message); end;
  end;

{ 
Soil strength stress on root growth

Numerous studies have shown that root growth is affected by soil
strength. Three important strength determinants are bulk density,
texture, and water content (Eavis, 1972. Monteith and Bonath,
1965. Taylor et al., 1966). All three variables are considered in
estimating the EPIC soil strength stress factor by using the following
equation [EQN: 302] where SS is the soil strength factor in layer l,
BD is the bulk density in t/m3 adjusted for water content, and bt(1)
and bt(2) are parameters dependent upon soil texture.
Equation:
  SS = BD / (BD + exp(bt(1) + bt(2) + BD))
  (the + before the last BD must be a typo)
Code:
  code adds adjustment based on rock content
Variables:
  SS = EQ::SoilStrengthFactor_frn
  BD = bulkDensityAdjForWaterContentForLayer_tPm3
  bt(1) = soilTextureParams[0]
  bt(2) = soilTextureParams[1]
 }
{ EQN: 302 }
class function EQ.SoilStrengthFactor_frn(bulkDensityAdjForWaterContentForLayer_tPm3: single; var soilTextureParams:
  arrayTwo; rockContent_pct: single; var rootGrowthRestrictionByRockCoeffs: SCurveStructure): single;
  begin
  try
  result := scurve(bulkDensityAdjForWaterContentForLayer_tPm3, soilTextureParams[0], -soilTextureParams[1]);
  result := result * (1.0 - scurve(rockContent_pct, rootGrowthRestrictionByRockCoeffs.c1,
    rootGrowthRestrictionByRockCoeffs.c2));
  except on e: Exception do result := errorMessage('Exception in EQ.SoilStrengthFactor_frn: ' + e.message); end;
  end;

{ 
The values of bt(1) and bt(2) are obtained from a simultaneous
solution of equation 302 by substituting boundary conditions
for stress. The lower boundary, where essentially no stress ocurrs,
is given by the equation (Jones, 1983b) [EQN: 303] where BDL is the
bulk density near the lower boundary (SS = 1.0) for a particular
percentage of sand, SAN.
Equation:
  BDL = 1.15 + 0.00445 * SAN
Code:
  same (rootGrowthRestrictionInSandySoilParam = 1.15)
  However, in the file PARM3090.DAT this parameter is listed as 1.5. Am using 1.5.
Variables:
  BDL = BulkDensityForLowStress_tPm3
  SAN = soilSandContent_pct
 }
{ EQN: 303 }
class function EQ.BulkDensityForLowStress_tPm3(soilSandContent_pct: single; rootGrowthRestrictionInSandySoilParam: single):
  single;
  begin
  try
  result := rootGrowthRestrictionInSandySoilParam + 0.00445 * soilSandContent_pct;
  except on e: Exception do result := errorMessage('Exception in EQ.BulkDensityForLowStress_tPm3: ' + e.message); end;
  end;

{ 
The upper boundary is given by the equation (Jones, 1983b) [EQN: 304]
where BDU is the bulk density near the upper boundary (SS ~~ 0.2) for a
particular percentage of sand, SAN.
Equation:
  BDU = 1.5 + 0.005 * SAN
Code:
  same (rootGrowthRestrictionInSandySoilParam = 1.15, 1.15 + 0.35 = 1.5)
  However, in the file PARM3090.DAT this parameter is listed as 1.5. Am using 1.5.
Variables:
  BDU = BulkDensityForHighStress_tPm3
  SAN = soilSandContent_pct
 }
{ EQN: 304 }
class function EQ.BulkDensityForHighStress_tPm3(soilSandContent_pct: single; rootGrowthRestrictionInSandySoilParam: single):
  single;
  begin
  try
  result := rootGrowthRestrictionInSandySoilParam + 0.35 + 0.005 * soilSandContent_pct;
  except on e: Exception do result := errorMessage('Exception in EQ.BulkDensityForHighStress_tPm3: ' + e.message); end;
  end;

{ 
The equations for estimating bt(1) and bt(2) are [EQN: 305]
and [EQN: 306]. Equations 305 and 306 assure that equation 302 gives
SS values of 1.0 and 0.2 for BD = BDL and BD = BDU.
Equation:
  bt(2) = (log(0.0112 * BDL) - log(8.0 * BDU)) / (BDL - BDU)
Code:
  same
Variables:
  bt(2) = SoilTextureParam2
  BDL = bulkDensityForLowStress_tPm3
  BDU = bulkDensityForHighStress_tPm3
 }
{ EQN: 305 }
class function EQ.SoilTextureParam2(bulkDensityForLowStress_tPm3: single; bulkDensityForHighStress_tPm3: single): single;
  var
    denominator: single;
  begin
  try
  denominator := bulkDensityForLowStress_tPm3 - bulkDensityForHighStress_tPm3;
  result := safediv(safeLn(0.01124 * bulkDensityForLowStress_tPm3) - safeLn(8.0 * bulkDensityForHighStress_tPm3),
    denominator);
  except on e: Exception do result := errorMessage('Exception in EQ.SoilTextureParam2: ' + e.message); end;
  end;

{ 
Equation:
  bt(1) = log(0.0112 * BDL) - bt(2) * BDL
Code:
  same
Variables:
  bt(1) = SoilTextureParam1
  bt(2) = SoilTextureParam2
  BDL = bulkDensityForLowStress_tPm3
 }
{ EQN: 306 }
class function EQ.SoilTextureParam1(bulkDensityForLowStress_tPm3: single; soilTextureParam2: single): single;
  begin
  try
  result := safeLn(0.01124 * bulkDensityForLowStress_tPm3) - soilTextureParam2 * bulkDensityForLowStress_tPm3;
  except on e: Exception do result := errorMessage('Exception in EQ.SoilTextureParam1: ' + e.message); end;
  end;

{ 
The water-content-adjusted bulk density is estimated with Grossman's
equation (Grossman et al., 1985) [EQN:307] where BD is the water-
content-adjusted bulk density of day i, BD3 is the bulk density
of 33 kPa water content BDD is the bulk density of the oven dry
soil, FC is the field capacity, WP is the wilting point, and SW
is the soil water content for layer l on day i.
Equation:
  BD = BD3 + (BDD - BD3) * (FC - SW)
       / (FC - WP * (4.083  - 3.33 * pow(BDD, 1/3)))
Code:
  code uses current bulk density as BD3
  BD = BD3 + (BDD - BD3 * BDD / BDS) * (FC - SW)
       / (FC - WP * (4.083  - 3.33 * pow(BDD / BDS, 1/3)))
Variables:
  BD = EQ::BulkDensityAdjForWaterContentForLayer_tPm3
  BD3 = bulkDensity_tPm3
  BDD = bulkDensityOvenDry_tPm3
  BDS = settledBulkDensity_tPm3
  FC = fieldCapacity_mm
  SW = waterContent_mm
  WP = wiltingPoint_mm
 }
{ EQN: 307 }
class function EQ.BulkDensityAdjForWaterContentForLayer_tPm3(layer: integer; var layers: LayerStructureArray): single;
  var
    fraction: single;
    denominator: single;
    bulkDensityOvenDryOverSettled: single;
  begin
  try
  {    xx=bdp(l)*(1.+(bdd(l)-1.)*(fc(l)-st(l))/(fc(l)-s15(l)*(4.0833-3.33*bdd(l)**.333))) }
  fraction := 0.0;
  denominator := 0.0;
  bulkDensityOvenDryOverSettled := safediv(layers[layer].bulkDensityOvenDry_tPm3,
    layers[layer].settledBulkDensity_tPm3);
  denominator := layers[layer].fieldCapacity_mm - layers[layer].wiltingPoint_mm * (4.083 - 3.33 *
    power(bulkDensityOvenDryOverSettled, 0.333));
  fraction := safediv(layers[layer].fieldCapacity_mm - layers[layer].waterContent_mm, denominator);
  result := layers[layer].bulkDensity_tPm3 * (1.0 + (bulkDensityOvenDryOverSettled - 1.0)) * fraction;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.BulkDensityAdjForWaterContentForLayer_tPm3: ' + e.message); end;
  end;

{ 
Aluminum toxicity stress factor

Aluminum (Al) toxicity can limit root growth in some acid soil layers,
and Al saturation is a widely used index of its effects (Abruna et al.,
1982. Brenes and Pearson, 1973. Pavan et al., 1982). Because crops
and cultivars differ in sensitivity to Al toxicity (Foy et al., 1974.
Mugwira et al. 1980), EPIC expresses Al toxicity as a function of this
sensitivity. The Al toxicity stress factor associated with root growth
is estimated with the equations [EQN: 308] and [EQN: 309] where ATS
is the Al toxicity stress factor (0-1) for soil layer l, ALS is the
Al saturation in percent, and ALO is the maximum ALS value crop j can
tolerate without stress in percent.
Equation:
 if ALS > ALO, ATS = (100 - ALS) / (100 - ALO)
 else ATS = 1.0
Code:
Variables:
  ATS = AluminumToxicityStressFactorForLayer_frn
  ALS = aluminumSaturation_pct
  ALO = maxAlSaturationForCrop_pct
 }
{ EQN: 308 EQN: 309 }
class function EQ.AluminumToxicityStressFactorForLayer_frn(aluminumSaturation_pct: single; aluminumTolerance_pct: single):
  single;
  begin
  try
  if (aluminumTolerance_pct = 100.0) then 
    result := 1.0
  else if (aluminumSaturation_pct > aluminumTolerance_pct) then
    result := safediv(1.0 * (100 - aluminumSaturation_pct), 1.0 * (100 - aluminumTolerance_pct))
  else
    result := 1.0;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.AluminumToxicityStressFactorForLayer_frn: ' + e.message); end;
  end;

{ 
Crop specific values of ALO are determined from the equation [EQN: 310]
where ALT is the Al tolerance index number for crop j. Values of ALT
range from 1 to 5 (1 is sensitive, 5 is tolerant) for various crops.
Equation:
  ALO = 10 + 20 * (ALT - 1)
Code:
  same
Variables:
  ALO = EQ::MaxAlSaturationForCrop_pct
  ALT = alToxicitySensitivityForCrop_1to5
 }
{ EQN: 310 }
class function EQ.MaxAlSaturationForCrop_pct(alToxicitySensitivityForCrop_1to5: integer): single;
  begin
  try
  result := 10.0 + 20.0 * (alToxicitySensitivityForCrop_1to5 - 1.0);
  except on e: Exception do result := errorMessage('Exception in EQ.MaxAlSaturationForCrop_pct: ' + e.message); end;
  end;

{ 
Finally, the root growth constraint, RGF, is the lowest of the stress
factors, SS, ATS, and TS.
(no equation in chapter)
 }
class function EQ.RootGrowthConstraintFactorForLayer_frn(rootTempStressFactorForLayer_frn: single;
  rootSoilStrengthStressFactorForLayer_frn: single; rootAlToxicityStressFactorForLayer_frn: single): single;
  begin
  try
  result := min(rootAlToxicityStressFactorForLayer_frn, min(rootSoilStrengthStressFactorForLayer_frn,
    rootTempStressFactorForLayer_frn));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.RootGrowthConstraintFactorForLayer_frn: ' + e.message); end;
  end;

{ 
How root water use is affected by the stress factors (UC)

Plant water is governed by the root growth stress factor and
the water deficit compensation factor of equation 273 (UC).
Recall that the water deficit compensation factor, UC, allows
total compensation if the value is 1.0 and no compensation at 0.0.
The value of UC for any layer is estimated as the product of
the root growth stress factors for the layer and all the layers
above [EQN: 311]. Thus a low RGF(k) greatly reduces water
compensation for layer k and all layers below k.
Equation:
  UC = (product from k = 1 to (layer) of) RGF
Code:
  same
Variables:
  UC = EQ::PlantWaterDeficitCompensFactorForLayer_frn
  RGF = rootGrowthConstraint_frn
 }
{ EQN: 311 }
class function EQ.PlantWaterDeficitCompensFactorForLayer_frn(aLayer: integer; var constraints: ConstraintsStructure): single;
  var
    layer: integer;
    product: single;
  begin
  try
  product := 1.0;
  if aLayer > 0 then for layer := 0 to aLayer do
    product := product * (constraints.rootGrowthConstraintByLayer_frn[layer]);
  result := product;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.PlantWaterDeficitCompensFactorForLayer_frn: ' + e.message); end;
  end;

{ 
Loss of Senescent Plant Material For Two Reasons

Leaves falling from trees

(this section is not in the chapter but is in the epic code.)
The amount of plant material lost to senescence is a portion of standing live biomass
based on treePlantingToMaturity_yr. For any tree crop it is the number
of years until the tree is mature. For a non-tree crop it is a constant
of 2000 (default if 0 is input). So for any non-tree crop, this comes out
to 0.000005 (5x10-6) times standingLiveBiomass_tPha in December, and less
each month before that (ie very very little). So this function is fairly unimportant
for non-tree crops but handles falling of leaves for tree crops. If a tree matures
in 20 years, for example, the portion of standingLiveBiomass_tPha is 0.0004
which for a tree of say 1000 kg/ha could be 4 kg/ha lost in this way.
This loss goes directly into flat residue in the surface soil layer.
 }
{ not in chapter }
class function EQ.LeavesFallingInFall(standingLiveBiomass_tPha: single; treePlantingToMaturity_yr: single; month: integer):
  single;
  begin
  try
  result := standingLiveBiomass_tPha * (1.0 - safeExp(safediv(-0.01, treePlantingToMaturity_yr))) * month / 12.0;
  if result > standingLiveBiomass_tPha then result := standingLiveBiomass_tPha;
  except on e: Exception do result := errorMessage('Exception in EQ.LeavesFallingInFall: ' + e.message); end;
  end;

{ 
Loss of senescent material in perennials

This loss is only done in perennials and is based on water stress and age
(heat unit index). This loss goes into standing dead residue for the plant.
I have changed it so that it also occurs in annuals, since we are allowing
them to die slowly.
waterStressFactor_frn goes from 1.0 (optimal) to 0.0 (really bad), so
1.0 - waterStressFactor_frn is a multiplier based on water stress.
 }
{ not in chapter }
class function EQ.BiomassLossIfOldAndDry(waterStressFactor_frn: single; heatUnitIndex: single; standingLiveBiomass_tPha:
  single): single;
  begin
  try
  result := 0.1 * (1.0 - waterStressFactor_frn) * power(heatUnitIndex, 4.0) * standingLiveBiomass_tPha;
  if result > standingLiveBiomass_tPha then result := standingLiveBiomass_tPha;
  except on e: Exception do result := errorMessage('Exception in EQ.BiomassLossIfOldAndDry: ' + e.message); end;
  end;

{ 
Crop Yield

The economic yield of most grain, pulse, and tuber crops is
a reproductive organ. Crops have a variety of mechanisms which
ensure that their production is neither too great to be
supported by the vegetative components nor too small to ensure
survival of the species. As a result, harvest index (economic
yield / above-ground biomass) is often a relatively stable
value across a range of environmental conditions. In EPIC,
crop yield is estimated by using the harvest index concept
[EQN: 267] where YLD is the amount of the crop removed from
the field in t/ha, HI is the harvest index, and B(AG) is
the above-ground biomass in t/ha for crop j.
 }
{ EQN: 267 }
{ very simple, in code }
{ 
Water stress constraint on crop yield

Crop yield may be reduced through water-stress-induced reductions in the
harvest index. Most grain crops are particularly sensitive to water stress
from shortly before until shortly after anthesis, when major yield
components are determined (Doorenbos and Kassam, 1979). Optimum conditions
for growth may reduce harvest index slightly if dry matter accumulation
is large and economic yield is limited by sink size. The harvest index is
affected by water stress according to the equation [EQN: 313] where HIA*
is the final estimate of harvest index applied to above-ground biomass
to estimate yield, HIA is the simulated potential harvest index, HIA(o)
is the minimum harvest index for the crop, and WUR is the simulated
water use ratio.
Equation:
  HIA* = (HIA - HIA(o)) * WUR / (WUR + exp(6.13 - 0.0883 * WUR)) + HIA(o)
Code:
  same except for bound: if HIA < HIA(o) HIA* = HIA(o)
Variables:
  HIA* = EQ::HarvestIndex_frn
  HIA = unconstrainedHarvestIndex_frn
  WUR = waterUseRatio
  HIA(o) = minHarvestIndex_frn
 }
{ EQN: 313 }
class function EQ.HarvestIndex_frn(unconstrainedHarvestIndex_frn: single; waterUseRatio: single; minHarvestIndex_frn: single;
  var harvestIndexPlantWaterUseCoeffs: SCurveStructure): single;
  begin
  try
  if (unconstrainedHarvestIndex_frn < minHarvestIndex_frn) then 
    result := minHarvestIndex_frn
  else
    result := (unconstrainedHarvestIndex_frn - minHarvestIndex_frn) * scurve(waterUseRatio,
      harvestIndexPlantWaterUseCoeffs.c1, harvestIndexPlantWaterUseCoeffs.c2) + minHarvestIndex_frn;
  except on e: Exception do result := errorMessage('Exception in EQ.HarvestIndex_frn: ' + e.message); end;
  end;

{ 
The value of HIA is simulated daily using the equation [EQN: 314].
The exponential parameters are set to give HIA = 0.1 when HUI = 0.5
and HIA = 0.95 when HUI = 0.95. Thus, if the growing season is
shortened by frost or for other reasons, the potential harvest
index HI is not attained.
Equation:
  HIA = HI * (100 * HUI / (100 * HUI + exp(11.1 - 0.1 * 100 * HUI))
Code:
  same
Variables:
  HIA = EQ::UnconstrainedHarvestIndex_frn
  HI = potentialHarvestIndex_frn
  HUI = heatUnitIndex
 }
{ EQN: 314 }
class function EQ.UnconstrainedHarvestIndex_frn(potentialHarvestIndex_frn: single; heatUnitIndex: single; var
  potentialHarvestIndexCoeffs: SCurveStructure): single;
  begin
  try
  result := potentialHarvestIndex_frn * scurve(heatUnitIndex, potentialHarvestIndexCoeffs.c1,
    potentialHarvestIndexCoeffs.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.UnconstrainedHarvestIndex_frn: ' + e.message); end;
  end;

{ 
The water use ratio WUR is estimated at harvest using
the equation [EQN: 315] where U and E(p) are the actual and
potential plant water use rates in mm for day k and I is the
number of days in the growing season. The exponential parameters
in equation 313 are set to give 0.5 * (HIA-HIA(o)) when WUR = 0.10
and 0.90 * (HIA-HIA(o)) when WUR = 0.50. Thus, if the actual to
potential water use ratio is greater than 0.5 there is little
reduction to harvest index.
Equation:
  WUR = 100 * (sum of U(i)) / (sum of E(P,i))
Code:
  same
  (note later changed to remove 100 multiplier)
Variables:
  WUR = WaterUseRatioForHarvestIndex
  sum of U(i) = cumWaterUse_mm
  sum of E(P,i) = cumPotPlantEvap_mm
 }
{ EQN: 315 }
class function EQ.WaterUseRatioForHarvestIndex(cumWaterUse_mm: single; cumPotPlantEvap_mm: single): single;
  begin
  try
  result := safediv(cumWaterUse_mm, cumPotPlantEvap_mm);
  except on e: Exception do result := errorMessage('Exception in EQ.WaterUseRatioForHarvestIndex: ' + e.message); end;
  end;

{ 
Winter Dormancy

The day length growth constraint is used to simulate a winter
dormant period for fall planted crops. This constraint is only
imposed for areas that have a growing season of fewer than 12
months. A 12-month growing season is defined in the model as
having no month with mean minimum temperature of
lower than 5 degrees C. If there is a dormant winter period, it is
defined as the time when day length is within 1 hour of the location's
minimum day length.

If a crop becomes dormant in winter, the heat unit summation
is set to zero. This provides for rapid new growth when
temperatures increase in the spring. During the dormant
period, the plants are not allowed to grow. The standing live
biomass is actually reduced during this period because of
frost and short day length. The day length reduction factor
(for biomass) is estimated with the equation [EQN: 316]
where FHR is the day length reduction factor, HRLT is the
day length on day i in hours, and HRLT(mn) is the minimum
day length for the location.
Equation:
  FHR = 0.35 * (1.0 - HRLT / (HRLT(mn) + 1))
Code:
  FHR = 0.35 * (1.0 - HRLT / (HRLT(mn) + timeOverMinDayLengthForWinterGrowth_hr))
Variables:
  FHR = EQ::WinterBiomassDayLengthReductionFactor
  HRLT = dayLength_hr
  HRLT(mn) + timeOverMinDayLengthForWinterGrowth_hr = minDayLengthForWinterGrowth_hr
 }
{ EQN: 316 }
class function EQ.WinterBiomassDayLengthReductionFactor(dayLength_hr: single; minDayLengthForWinterGrowth_hr: single): single;
  begin
  try
  result := 0.35 * (1.0 - safediv(dayLength_hr, minDayLengthForWinterGrowth_hr));
  except on e: Exception do result := errorMessage('Exception in EQ.WinterBiomassDayLengthReductionFactor: ' + e.message); end;
  end;

{ calculation of day length moved to EQ::Weather.cp }
{ 
The frost reduction factor is estimated with the equation
[EQN: 319] where FRST is the frost damage factor, T(mn) is
the minimum temperature on day i in degrees C, and af(1) and
af(2) are parameters expressing the crop's frost sensitivity.
Equation:
  FRST = -T(mn) / (-T(mn) - exp(af(1) + af(2) * T(mn)), T(mn) < -1 degrees C
Code:
  same except they use abs() instead of -ve for the temperature
  (works the same because temp is always -ve when this is called)
Variables:
  FRST = WinterBiomassFrostReductionFactor
  T(mn, i) = minTempForDay_degC
  T(mn, j) = dailyMeanMinTempForMonth_degC
  af(1) = frostReductionFactorParams[0]
  af(2) = frostReductionFactorParams[1]
 }
{ EQN: 319 }
class function EQ.WinterBiomassFrostReductionFactor(minTempForDay_degC: single; var frostReductionFactorParams:
  SCurveStructure): single;
  begin
  try
  result := scurve(abs(minTempForDay_degC), frostReductionFactorParams.c1, frostReductionFactorParams.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.WinterBiomassFrostReductionFactor: ' + e.message); end;
  end;

{ 
The reduction in standing live biomass is estimated with the
equation [EQN: 320] where deltaB(AG) is the reduction in
above ground biomass in t/ha on day i, HUI is the heat unit
index, and B(AG) is the above ground biomass in t/ha on day i.
Note that frost damage is greater when plants are small (HUI
is close to zero) and approaches zero as the plants near
maturity.
Equation:
  deltaB(AG) = 0.5 * B(AG) * (1.0 - HUI) * max(FHR, FRST)
Code:
  deltaB = B * (1.0 - min(1.0, HUI)) * max(FHR, FRST)
Variables:
  deltaB = EQ::WinterReductionInBiomass
  deltaB(AG) = WinterReductionInStandingLiveBiomass
  B = totalBiomassInclRoot_tPha
  B(AG) = standingLiveBiomass_tPha
  HUI = heatUnitIndex
  FHR = winterBiomassDayLengthReductionFactor
  FRST = winterBiomassFrostReductionFactor
  max(FHR, FRST) = winterBiomassFrostReductionFactor
 }
{ EQN: 320 }
class function EQ.WinterReductionInBiomass(totalBiomassInclRoot_tPha: single; heatUnitIndex: single;
  winterBiomassFrostReductionFactor: single): single;
  var
    { change from EPIC }
    { at the end of the season, when heatUnitIndex > 1, this comes out -ve, so bound at 1. }
    adjustedHeatUnitIndex: single;
  begin
  try
  adjustedHeatUnitIndex := min(1.0, heatUnitIndex);
  result := totalBiomassInclRoot_tPha * (1.0 - adjustedHeatUnitIndex) * winterBiomassFrostReductionFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.WinterReductionInBiomass: ' + e.message); end;
  end;

{ 
There is also a provision for frost kill of annual plants. Kill
occurs if [EQN: 321] is true, where SNOF is a snow cover factor
calculated with the equation [EQN: 322] where SNO is the water
content of snow cover in mm.
Equation:
  if (FRST * (1.0 - SNOF) >= 0.9) plant dies
  SNOF = SNO / (SNO + exp(2.303 - 0.22 * SNO))
Code:
  same
Variables:
  FRST = EQ::AnnualCropIsKilledByFrost
  SNOF = snowCoverFactor
  SNO = snowWaterContent_mm
  FRST = winterBiomassFrostReductionFactor
 }
{ EQN: 321 EQN: 322 }
class function EQ.AnnualCropIsKilledByFrost(snowWaterContent_mm: single; var constraints: ConstraintsStructure;
  meanTempForDay_degC: single; absoluteTempForFrostKill_degC: single): boolean;
  begin
  try
  { change from EPIC }
  { I added this check of temperature to create an absolute lower bound for the cultivar }
  if (meanTempForDay_degC <= absoluteTempForFrostKill_degC) then 
    begin
    result := true;
    exit;
    end;
  constraints.snowCoverFactorForFrostKill := 0.0;
  if (snowWaterContent_mm > kLowestFloatAboveZero) then constraints.snowCoverFactorForFrostKill :=
    scurve(snowWaterContent_mm, 2.303, 0.2197);
  if (constraints.winterBiomassReductionFactor * (1.0 - constraints.snowCoverFactorForFrostKill) >= 0.9) then 
    result := true
  else
    result := false;
  except on e: Exception do
    begin errorMessage('Exception in EQ.AnnualCropIsKilledByFrost: ' + e.message); result :=  false; end; end;
  end;

{
                              NUTRIENTS - *NITROGEN*
The divisions of nitrogen are:
  nitrate_kgPha
  ammonia_kgPha
  organicNTotal_khPha
    organicNFresh_kgPha - in crop residue and microbial biomass
    organicNActiveHumus_kgPha - in humus
    organicNStableHumus_kgPha - in humus

Nitrogen Losses - Leaching, Surface Runoff, Lateral Subsurface Flow

The amount of NO3-N lost when water flows through a layer is estimated by
considering the change in concentration. Thus the equation
  VNO3 = QT * C(NO3)                                             (Equation 157)
where VNO3 is the amount of NO3-N lost from a soil layer and C(NO3) is
the average concentration of NO3-N in the layer during the percolation
of volume QT through the layer.

At the end of the day, the amount of NO3-N left in the layer is
  WNO3 = WNO3(o) - QT * C(NO3)                                   (Equation 158)
where WNO3(o) and WNO3 are the weights of NO3-N contained in the layer at
the beginning and ending of the day.

The NO3-N concentration can be calculated by dividing the weight of NO3-N
by the water storage volume:
  C'(NO3) = C(NO3) - C(NO3) * QT / (bl * PO)                     (Equation 159)
where C'(NO3) is the concentration of NO3-N at the end of a day, PO is soil
porosity, and bl is a fraction of the storage PO occupied by percolating
water. Equation 159 is a finite difference approximation for the exponential
equation
  C'(NO3) = C(NO3) * exp(-QT / (bl * PO))                        (Equation 160)

Thus, VNO3 can be computed for any volume, QT, by integrating equation 160
[EQN: 161].
Equation:
  VNO3 = WNO3 * (1.0 - exp(-QT / (bl * PO)))
Code:
  same
Variables:
  VNO3 = NitrateLeachedFromLayer_kgPha
  WNO3 = nitrate_kgPha
  QT = volumeMovingThroughLayer_mm
  bl = nitrogenLeachingParam
  PO = porosity_mm
 }
{ EQN: 161 }
class function EQ.NitrateLeachedFromLayer_kgPha(nitrate_kgPha: single; volumeMovingThroughLayer_mm: single;
  nitrogenLeachingParam: single; porosity_mm: single): single;
  begin
  try
  { vp=v/(po(k)*parm(4))
    vno3=wno3(k)*(1.-exp(-vp)) }
  if nitrogenLeachingParam = 0.0 then
    result := 0.0
  else
    result := nitrate_kgPha * (1.0 - safeExp(safediv(-volumeMovingThroughLayer_mm, nitrogenLeachingParam * porosity_mm)));
  except on e: Exception do result := errorMessage('Exception in EQ.NitrateLeachedFromLayer_kgPha: ' + e.message); end;
  end;

{ 
The average concentration during the percolation of QT (one day since EPIC
is a daily time step model) is [EQN: 162].
Equation:
  C(NO3) = VNO3 / QT
Code:
  same
Variables:
  C(NO3) = MeanNitrateConcInLayerDuringWaterFlow_gPm3
  VNO3 = nitrateLostFromLayer_mm
  QT = volumeMovingThroughLayer_mm
 }
{ EQN: 162 }
class function EQ.MeanNitrateConcInLayerDuringWaterFlow_gPm3(nitrateLostFromLayer_mm: single; volumeMovingThroughLayer_mm:
  single): single;
  begin
  try
  result := safediv(nitrateLostFromLayer_mm, volumeMovingThroughLayer_mm);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.MeanNitrateConcInLayerDuringWaterFlow_gPm3: ' + e.message); end;
  end;

{ 
Amounts of NO3-N contained in runoff, lateral flow, and percolation are
estimated as products of the volume of water and the concentration from
equation 162. The calculation sequence is:
  1. Soil layer 1 -- Leaching is estimated first by subsituting O
     (PercolationForLayer_mm) for QT in equation 161 and solving for VNO3.
     Then VNO3 is removed from WNO3(1) and placed into WNO3(2). Next,
     runoff and lateral flow losses are estimated simulataneously by
     substituting Q (RunoffVolume_mm) + QR (LateralFlow_mm) for QT
     in equation 161 and the resulting VNO3 into equation 162. Individual
     losses are estimated for runoff (Q*C(NO3)) and lateral flow
     (QR*C(NO3)). Finally, these losses are removed from WNO3(1).
  2. Soil layers 2-M -- Leaching and lateral flow losses are estimated
     simulataneously by substituting QR (LatFlow_mm) +
     O (PercolationForLayer_mm) for QT in equation 161 and the
     resulting VNO3 into equation 162. Individual losses are calculated
     as in (1) -- QR*C(NO3) and O*C(NO3), subtracted from WNO3 and
     added to the WNO3 of the next layer. The process is repeated
     layer by layer to the bottom of the soil profile.

NO3-N Transport by Soil Water Evaporation

When water is evaporated from the soil, NO3-N is moved upward into the top soil
layer by mass flow. The equation for estimating this NO3-N transport is [EQN: 163]
where ENO3 is the amount of NO3-N in kg/ha moved from lower layers to the top layer
by soil water evaporation E(s) in mm, subscript l refers to soil layers, and
M is the number of layers contributing to soil water evaporation (maximum depth
is 0.2 m).
Equation:
  ENO3 = (sum from l = 2 to M of) SEV* * (C(NO3))
Code:
  same
Variables:
  ENO3 = EQ::NitrateMovedToFirstLayerFromSoilEvap_kgPha
  M = numLayers
  SEV* = evaporation_mm
  C(NO3) = meanNitrateConcThisLayer_gPm3
 }
{ EQN: 163 }
class function EQ.NitrateMovedToFirstLayerFromSoilEvap_kgPha(var layers: LayerStructureArray;
  lowestLayerInWhichSoilEvapOccured: integer): single;
  var
    layer: integer;
    nitrateConcBeforeEvap_gPm3, nitrateMovingFromThisLayer_kgPha, totalNitrateMovingToSurface_kgPha: single;
  begin
  try
  layer := lowestLayerInWhichSoilEvapOccured;
  totalNitrateMovingToSurface_kgPha := 0.0;
  while (layer > 1) do
    begin
    if (layers[layer].nitrate_kgPha >= 0.001) and (layers[layer].evaporation_mm > 0.0) then
      begin
      nitrateConcBeforeEvap_gPm3 := safediv(layers[layer].nitrate_kgPha, layers[layer].waterContent_mm +
        layers[layer].evaporation_mm);
      nitrateMovingFromThisLayer_kgPha := nitrateConcBeforeEvap_gPm3 * layers[layer].evaporation_mm;
      totalNitrateMovingToSurface_kgPha := totalNitrateMovingToSurface_kgPha + nitrateMovingFromThisLayer_kgPha;
      subtractQuantity(layers[layer].nitrate_kgPha, nitrateMovingFromThisLayer_kgPha);
      end;
    dec(layer);
    end;
  addQuantity(layers[0].nitrate_kgPha, totalNitrateMovingToSurface_kgPha);
  result := totalNitrateMovingToSurface_kgPha;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.NitrateMovedToFirstLayerFromSoilEvap_kgPha: ' + e.message); end;
  end;

{ 
Organic N Transport by Sediment

A loading function developed by McElroy et al. (1976) and modified by Williams and
Hann (1978) for application to individual runoff events is used to estimate organic
N loss. The loading function is [EQN: 164] where YON is the organic N runoff loss
in kg/ha, Y is the sediment yield in t/ha, c(ON) is the concentration of organic N
in the top layer in g/t, and ER is the enrichment ratio. The enrichment ratio is
the concentration of organic N in the sediment divided by that in the soil.
Equation:
  YON = 0.001 * Y * C(ON) * ER
Code:
  this equation is used to calculate the amount of six materials lost in erosion:
  organicNActiveHumus_kgPha, organicNStableHumus_kgPha, organicMatter_tPha,
  organicPHumus_kgPha, labileP_kgPha, and mineralPActive_kgPha.
  taking the case of ON(a),
  propLost = ER * Y / SWT and YON(a) = ON(a) * propLost so
  YON(a) = ON(a) * ER * Y / SWT
  C(ON) = ON(a) / SWT so
  YON(a) = Y * C(ON) * ER
  only the 0.001 is lost, possibly because C(ON) is in different units
  also an upper bound of 0.9 is placed on propLost (ER * Y / SWT)
Variables:
  YON(a) = organicNActiveHumusAdsorbedToSediment_kgPha
  ON(a) = organicNActiveHumus_kgPha
  propLost = EQ::OrganicNOrCOrPProportionLostInSediment_kgPha
  C(ON) = organicNConcSurfaceLayer_gPt
  Y = totalErosion_tPha
  SWT = surfaceLayerSoilWeight_tPha
  ER = enrichmentRatioForNPPest
 }
{ EQN: 164 }
class function EQ.OrganicNOrCOrPProportionLostInSediment_kgPha(totalErosion_tPha: single; enrichmentRatioForNPPest: single;
  surfaceLayerSoilWeight_tPha: single): single;
  begin
  try
  result := min(0.9, enrichmentRatioForNPPest * safediv(totalErosion_tPha, surfaceLayerSoilWeight_tPha));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.OrganicNOrCOrPProportionLostInSediment_kgPha: ' + e.message); end;
  end;

{ 
Enrichment ratios are logarithmically related to sediment concentration as described
by Menzel (1980). An individual event enrichment-sediment concentration relationship
was developed for EPIC considering upper and lower bounds. The upper bound of enrichment
ratio is the inverse of the sediment delivery ratio. Exceeding the inverse of the delivery
ratio implies that more organic N leaves the watershed than is dislodged from the
soil. The delivery ratio is estimated for each runoff event by using the equation
[EQN: 165] where DR is the sediment delivery ratio (sediment yield divided by gross
sheet erosion), q(p) is the peak runoff rate in mm/hr, and r(ep) is the peak rainfall
excess rate in mm/hr. Equation 165 is based on sediment yield estimated by using
MUST.
Equation:
  DR = sqrt(q(p) / r(ep))
Code:
  same except code has power to 0.56, not 0.5
Variables:
  DR = SedimentDeliveryRatio
  q(p) = peakRunoffRate_mmPhr
  r(ep) = peakRainfallExcessRate_mmPhr
 }
{ EQN: 165 }
class function EQ.SedimentDeliveryRatio(peakRunoffRate_mmPhr: single; peakRainfallExcessRate_mmPhr: single): single;
  begin
  try
  result := power(safediv(peakRunoffRate_mmPhr, peakRainfallExcessRate_mmPhr), 0.56);
  except on e: Exception do result := errorMessage('Exception in EQ.SedimentDeliveryRatio: ' + e.message); end;
  end;

{ 
The rainfall excess rate cannot be evaluated directly because the hydrology model
predicts only the total daily runoff volume. An estimate of the rate can be
obtained, however, using the equation [EQN: 166] where r(p) is the peak rainfall rate
in mm/hr and the runoff-rainfall ratio Q/R is used to account for infiltration.
The Q/R is raised to the 0.1 power to account for intensity effects on infiltration -
Q/R is greater than average near r(p).
Equation:
  r(ep) = r(p) * power(Q / R, 0.1)
Code:
  same
Variables:
  r(ep) = EQ::PeakRainfallExcessRate_mmPhr
  r(p) = peakRainfallRate_mmPhr
  Q = runoffVolume_mm
  R = rainfallForDay_mm
 }
{ EQN: 166 }
class function EQ.PeakRainfallExcessRate_mmPhr(peakRainfallRate_mmPhr: single; runoffVolume_mm: single; rainfallForDay_mm:
  single): single;
  begin
  try
  result := peakRainfallRate_mmPhr * power(safediv(runoffVolume_mm, rainfallForDay_mm), 0.1);
  except on e: Exception do result := errorMessage('Exception in EQ.PeakRainfallExcessRate_mmPhr: ' + e.message); end;
  end;

{ no equation, from code }
class function EQ.SedimentConc_gPm3(waterErosion_tPha: single; runoffVolume_mm: single): single;
  begin
  try
  result := 0.1 * safediv(waterErosion_tPha, runoffVolume_mm);
  except on e: Exception do result := errorMessage('Exception in EQ.SedimentConc_gPm3: ' + e.message); end;
  end;

{ 
The lower limit of enrichment ratio is 1.0 - sediment particle size distribution
is the same as that of the soil. Thus, 1.0 <= ER <= 1/DR. The logarithmic equation
for estimating enrichment is [EQN: 167] where c(s) is the sediment concentration in
g/m3 and x(1) and x(2) are parameters set by the upper and lower limits.
Equation:
  ER = x(1) * power(c(s), x(2))
  1.0 <= ER <= 1/DR
Code:
  same but 1.0 <= ER <= 5.0
Variables:
  ER = EQ::EnrichmentRatioForNPPest
  c(s) = sedimentConc_gPm3
  x(1) = coeffForEnrichmentRatio
  x(2) = expForEnrichmentRatio
 }
{ EQN: 167 }
class function EQ.EnrichmentRatioForNPPest(sedimentConc_gPm3: single; coeffForEnrichmentRatio: single; expForEnrichmentRatio:
  single): single;
  begin
  try
  result := max(1.0, min(5.0, coeffForEnrichmentRatio * power(sedimentConc_gPm3, expForEnrichmentRatio)));
  except on e: Exception do result := errorMessage('Exception in EQ.EnrichmentRatioForNPPest: ' + e.message); end;
  end;

{ 
For the enrichment ratio to approach 1.0, the sediment concentration must be extremely high.
Conversely, for the enrichment ratio to approach 1/DR, the sediment concentration
must be low. The simulataneous solution of equation 167 at the boundaries assuming
that sediment concentrations range from 500 to 250,000 g/m3 gives [EQN: 168] and
[EQN: 169].
Equation:
  x(2) = -ln(1/DR) / 2.699
Code:
  same
Variables:
  x(2) = EQ::ExpForEnrichmentRatio
  DR = sedimentDeliveryRatio
 }
{ EQN: 168 }
class function EQ.ExpForEnrichmentRatio(sedimentDeliveryRatio: single): single;
  begin
  try
  result :=  - log10(safediv(1.0, sedimentDeliveryRatio)) / 2.699;
  except on e: Exception do result := errorMessage('Exception in EQ.ExpForEnrichmentRatio: ' + e.message); end;
  end;

{ 
Equation:
  x(1) = 1 / power(0.1, x(2))
Code:
  same
Variables:
  x(1) = EQ::CoeffForEnrichmentRatio
  x(2) = expForEnrichmentRatio
 }
{ EQN: 169 }
class function EQ.CoeffForEnrichmentRatio(expForEnrichmentRatio: single): single;
  begin
  try
  result := safediv(1.0, power(0.1, expForEnrichmentRatio));
  except on e: Exception do result := errorMessage('Exception in EQ.CoeffForEnrichmentRatio: ' + e.message); end;
  end;

{ 
Denitrification

As one of the microbial processes, denitrification is a function of temperature
and water content. The equation used to estimate the denitrification rate is
[EQN: 170] where DN is the denitrification rate in later l in kg/ha*day, TF(n)
is the nutrient cycling temperature factor, C is the organic carbon content in
percent, and SWF is the soil water factor.
Equation:
  SWF = SW / FC
  if SWF >= 0.95, DN = WNO3 * (1.0 - exp(-1.4 * TF(n) * C))
  else DN = 0
Code:
  if SWF >= 0.95, DN = WNO3 * (1.0 - exp(-1.4 * TF(n) * OM / SWT))
  OM / SWT does NOT = C, because in other places in the code,
  the relationship C = (OM / 1.72) / SWT * 0.01 is used.
Variables:
  DN = EQ::DenitrificationForLayer_kgPha
  SWF = soilWaterOverFieldCapacity
  WNO3 = nitrate_kgPha
  TF(n) = nutrientCyclingTempFactor
  C = organicC_pct
  OM = organicMatter_tPha
  SWT = weight_tPha
 }
{ EQN: 170 }
class function EQ.DenitrificationForLayer_kgPha(waterContent_mm: single; fieldCapacity_mm: single; weight_tPha: single;
  nitrate_kgPha: single; nutrientCyclingTempFactor: single; organicMatter_tPha: single): single;
  var
    soilWaterOverFieldCapacity: single;
  begin
  try
  soilWaterOverFieldCapacity := EQ.SoilWaterOverFieldCapacityForLayer(waterContent_mm, fieldCapacity_mm);
  if (soilWaterOverFieldCapacity >= 0.95) then 
    begin
    if (weight_tPha = 0.0) then 
      result := 0.0
    else
      result := nitrate_kgPha * (1.0 - safeExp(-1.4 * nutrientCyclingTempFactor * safediv(organicMatter_tPha,
        weight_tPha)));
    end
  else
    result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EQ.DenitrificationForLayer_kgPha: ' + e.message); end;
  end;

{ 
The temperature factor is expressed by the equation [EQN: 171] where T is soil
temperature in degrees C and subscript l refers to the layers.
Equation:
  TF(n) = max(0.1, T / (T + exp(9.93 - 0.312 * T)))
Code:
  TF(n) = T / (T + 20551.0 * exp(-0.312 * T))
  but since exp(9.93) = 20551, this is the same
  code does not have bound
Variables:
  TF(n) = NutrientCyclingTempFactorForLayer
  T = temperature_degC
 }
{ EQN: 171 }
class function EQ.NutrientCyclingTempFactorForLayer(temperature_degC: single): single;
  begin
  try
  result := scurve(temperature_degC, 9.93, 0.312);
  except on e: Exception do result := errorMessage('Exception in EQ.NutrientCyclingTempFactorForLayer: ' + e.message); end;
  end;

{ 
The soil water factor considers total soil water in the equation [EQN: 172]
where SW is the soil water content in layer l and FC is the field capacity in mm.
Equation:
  SWF = SW / FC
Code:
  same
Variables:
  SWF = EQ::SoilWaterOverFieldCapacityForLayer
  SW = waterContent_mm
  FC = fieldCapacity_mm
 }
{ EQN: 172 }
class function EQ.SoilWaterOverFieldCapacityForLayer(waterContent_mm: single; fieldCapacity_mm: single): single;
  begin
  try
  if (fieldCapacity_mm = 0.0) then 
    result := 0.0
  else
    result := max(0.0, min(1.0, safediv(waterContent_mm, fieldCapacity_mm)));
  except on e: Exception do result := errorMessage('Exception in EQ.SoilWaterOverFieldCapacityForLayer: ' + e.message); end;
  end;

{ 
Mineralization

There are three types of organic N kept track of here:
  fresh organic N (crop residue and microbial biomass) = organicNFresh_kgPha
  active organic N in the humus = organicNActiveHumus_kgPha
  stable organic N in the humus = organicNStableHumus_kgPha
They add up to the total organic N = organicNTotal_khPha

The N mineralization model is a modification of the PAPRAN mineralization
model (Seligman and van Keulen, 1981). The model considers two sources of
mineralization: fresh organic N pool, associated with crop residue and
microbial biomass, and the stable organic N pool, associated with the soil
humus. Mineralization from the fresh organic N pool is estimated with
the equation [EQN: 173] where RMN is the N mineralization rate in kg/ha*day
for fresh organic N in layer l, DCR is the decay rate constant for the fresh
organic N, and FON is the amount of fresh organic N present in kg/ha.
Equation:
  RMN = DCR * FON
Code:
  same
Variables:
  RMN = EQ::FreshNMineralizationForLayer_kgPha
  DCR = decayRateConst
  FON = organicNFresh_kgPha
 }
{ EQN: 173 }
class function EQ.FreshNMineralizationForLayer_kgPha(decayRateConst: single; organicNFresh_kgPha: single): single;
  begin
  try
  result := decayRateConst * organicNFresh_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.FreshNMineralizationForLayer_kgPha: ' + e.message); end;
  end;

{ 
The decay rate constant is a function of C:N ratio, C:P ratio, composition
of crop residue, temperature and soil water: [EQN: 174] where CNP is
a C:N and C:P ratio factor and FC is the soil water content in mm at
field capacity.
Equation:
  DCR = 0.05 * CNP * sqrt(SW / FC * TF(n))
Code:
Variables:
  DCR = DecayRateConstForLayer
  CNP = cNPCompositeRatio
  SW/FC = soilWaterOverFieldCapacity
  TF(n) = nutrientCyclingTempFactor
 }
{ EQN: 174 }
class function EQ.DecayRateConstForLayer(layer: integer; var layers: LayerStructureArray; cNPCompositeRatio: single): single;
  var
    soilWaterOverFieldCapacity: single;
  begin
  try
  soilWaterOverFieldCapacity := EQ.SoilWaterOverFieldCapacityForLayer(layers[layer].waterContent_mm,
    layers[layer].fieldCapacity_mm);
  result := 0.05 * cNPCompositeRatio * sqrt(soilWaterOverFieldCapacity * layers[layer].nutrientCyclingTempFactor);
  result := min(1.0, max(0.0, result));
  except on e: Exception do result := errorMessage('Exception in EQ.DecayRateConstForLayer: ' + e.message); end;
  end;

{ 
The value of CNP is calculated with the equation [EQN: 175] where CNR is the
C:N ratio and CNP is the C:P ratio in layer l.
Equation:
  CNP = min(exp(-0.693 * (CNR - 25) / 25), exp(-0.693 * (CPR - 200) / 200), 1.0)
Code:
  if CNR > 25 resultWithN = exp(-0.693 * (CNR - 25) / 25)
  else resultWithN = 1
  if CPR > 200 resultWithP = exp(-0.693 * (CPR - 200) / 200)
  else resultWithP = 1
  CNP = min(resultWithN, resultWithP)
  same, because in either case if the else conditions is met it will produce
  exp(-ve number) which will be > 1.
Variables:
  CNP = CNPCompositeRatioForLayer
  CNR = cNRatioCropResidue
  CPR = cPRatioCropResidue
 }
{ EQN: 175 }
class function EQ.CNPCompositeRatioForLayer(cNRatioCropResidue: single; cPRatioCropResidue: single): single;
  var
    resultWithN: single;
    resultWithP: single;
  begin
  try
  resultWithN := safeExp(-0.693 * (cNRatioCropResidue - 25.0) / 25.0);
  resultWithP := safeExp(-0.693 * (cPRatioCropResidue - 200.0) / 200.0);
  result := min(resultWithN, min(resultWithP, 1.0));
  except on e: Exception do result := errorMessage('Exception in EQ.CNPCompositeRatioForLayer: ' + e.message); end;
  end;

{ 
The C:N and C:P ratios of crop residue are computed for each soil layer with the
equations [EQN: 176] and [EQN: 177] where FON is the amount of fresh organic N
in kg/ha, FOP is the amount of fresh organic P in kg/ha, and AP is the amount
of labile P in kg/ha for layer l.
Equation:
  CNR = 0.58 * FR / (FON + WNO3)
  (the 0.58 is the inverse of 1.72, which is the ratio of organic carbon to soil humus)
Code:
  same
Variables:
  CNR = EQ::CNRatioCropResidueForLayer
  FR = flatCropResidue_tPha
  FON = organicNFresh_kgPha
  WNO3 = nitrate_kgPha
 }
{ EQN: 176 }
class function EQ.CNRatioCropResidueForLayer(layer: integer; var layers: LayerStructureArray): single;
  var
    cInFlatCropResidue_kgPha, flatCropResidue_kgPha: single;
  begin
  try
  if (layers[layer].organicNFresh_kgPha + layers[layer].nitrate_kgPha <= 0.0) then
    result := 0.0
  else
    begin
    { get flat crop residue into same units as N }
    flatCropResidue_kgPha := layers[layer].flatCropResidue_tPha * t_to_kg;
    { convert to weight of organic carbon }
    cInFlatCropResidue_kgPha := 0.58 * flatCropResidue_kgPha;
    { divide by N to get ratio }
    result := safediv(cInFlatCropResidue_kgPha, layers[layer].organicNFresh_kgPha + layers[layer].nitrate_kgPha);
    { bound result at 2500 (100 times what it is compared to) to avoid problem in exponential function
      - and if ratio is greater than this it will not matter anyway because the exponential will be a tiny number }
    if result > 2500.0 then result := 2500.0;
    end;
  except on e: Exception do result := errorMessage('Exception in EQ.CNRatioCropResidueForLayer: ' + e.message); end;
  end;

{ 
Equation:
  CPR = 0.58 * FR / (FOP + AP)
Code:
  same
Variables:
  CPR = EQ::CPRatioCropResidueForLayer
  FR = flatCropResidue_tPha
  FOP = organicPFresh_kgPha
  AP = labileP_kgPha
 }
{ EQN: 177 }
class function EQ.CPRatioCropResidueForLayer(layer: integer; var layers: LayerStructureArray): single;
  var
    cInFlatCropResidue_kgPha, flatCropResidue_kgPha: single;
  begin
  try
  if (layers[layer].organicPFresh_kgPha + layers[layer].labileP_kgPha <= 0.0) then
    result := 0.0
  else
    begin
    { get flat crop residue into same units as P }
    flatCropResidue_kgPha := layers[layer].flatCropResidue_tPha * t_to_kg;
    { convert to weight of organic carbon }
    cInFlatCropResidue_kgPha := 0.58 * flatCropResidue_kgPha;
    { divide by P to get ratio }
    result := safediv(cInFlatCropResidue_kgPha, layers[layer].organicPFresh_kgPha + layers[layer].labileP_kgPha);
    { bound result at 20000 (100 times what it is compared to) to avoid problem in exponential function
      - and if ratio is greater than this it will not matter anyway because the exponential will be a tiny number }
    if result > 20000.0 then result := 20000.0;
    end;
  except on e: Exception do result := errorMessage('Exception in EQ.CPRatioCropResidueForLayer: ' + e.message); end;
  end;

{ 
In the code, an adjustment is made to organic matter when fresh and/or humus
mineralization take place: humus changes in the same proportion that
organicNActiveHumus_kgPha is changed by these processes.
(no equation in chapter)
  ON(a) -= HMN
  ON(a) += 0.2 * RMN
So OM = OM + OM * (-HMN) / ON + OM * (0.2 * RMN) / ON
//
 }
class function EQ.OrganicMatterChangeFromFreshAndHumusNMineralization_tPha(layer: integer; var layers: LayerStructureArray;
  organicNTotalHumus_kgPha: single): single;
  begin
  try
  {   EPIC CODE:
      hum(k)=hum(k)*(1.-hmn/xx) if rmn (freshNMineralization_kgPha) is not calculated, or
      hum(k)=hum(k)*(1.+(rm2-hmn)/xx) if rmn (freshNMineralization_kgPha) is calculated.
      where xx=wn(k)+wmn(k) and wn and wmn are types of organic n,
        hmn is humus mineralization (activeHumusNMineralization_kgPha)
        and rm2 is 0.2 * rmn where rmn is fresh organic mineralization (freshNMineralization_kgPha).
      so translating this, we get
        omChange = om * (0.2 * freshMin - humusMin) / totalN
      or
        omChange = om * 0.2 * freshMin / totalN - om * humusMin / totalN
      BUT i think this is wrong, because shouldn't the new OM from freshNMineralization_kgPha be 20% of
      flat residue, not OM? it doesn't make sense to make it depend on om. so i have changed it to use
      flat residue for the fresh part. }
  with layers[layer] do
    result := flatCropResidue_tPha * safedivExcept(0.2 * freshNMineralization_kgPha, organicNTotalHumus_kgPha, 0)
        - organicMatter_tPha * safedivExcept(activeHumusNMineralization_kgPha, organicNTotalHumus_kgPha, 0);
  except on e: Exception do
    result := errorMessage('Exception in EQ.OrganicMatterChangeFromFreshAndHumusNMineralization_tPha: ' + e.message); end;
  end;

{ 
Organic N associated with humus is divided into two pools - active and stable -
by using the equation [EQN: 178] where ON(a) is the active or readily
mineralizable pool in kg/ha, RTN is the active pool fraction, ON is the
total organic N in kg/ha, and the subscript l is the soil layer number.
Equation:
  ON(a) = RTN * ON
Code:
  same
Variables:
  ON(a) = EQ::OrganicNActiveHumusForLayer_gPt
  RTN = organicNActiveHumusFraction_frn
  ON = organicNTotalHumus_gPt
 }
{ EQN: 178 }
class function EQ.OrganicNActiveHumusForLayer_gPt(organicNActiveHumusFraction_frn: single; organicNTotalHumus_gPt: single):
  single;
  begin
  try
  result := organicNActiveHumusFraction_frn * organicNTotalHumus_gPt;
  except on e: Exception do result := errorMessage('Exception in EQ.OrganicNActiveHumusForLayer_gPt: ' + e.message); end;
  end;

{ 
The active pool fraction in the plow layer depends on the number of years the soil
has been cultivated and is estimated with the equation [EQN: 179] where YC
is the period of cultivation before the simulation starts in years. The concepts
expressed in equation 179 are based on work of Hobbs and Thompson (1971).
Below the plow layer the active pool fraction is set at 40% of the plow layer
value, based on work of Cassman and Munns (1980).
Equation:
  RTN = 0.4 * exp(-0.0277 * YC) + 0.1
  layers below plow layer are 40% of upper layers
Code:
  same equation, layers below layer 2 are 40% of upper layers
Variables:
  RTN = OrganicNActiveHumusFractionForLayer_frn
  YC = cultivationBeforeSimulationStarts_yr
 }
{ EQN: 179 }
class function EQ.OrganicNActiveHumusFractionForLayer_frn(layer: integer; cultivationBeforeSimulationStarts_yr: single):
  single;
  begin
  try
  result := 0.4 * safeExp(-0.0277 * cultivationBeforeSimulationStarts_yr) + 0.1;
  if (layer > 2) then result := result * 0.4;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.OrganicNActiveHumusFractionForLayer_frn: ' + e.message); end;
  end;

{ 
Organic N flux between the active and stable pools is governed by the
equilibrium equation [EQN: 180] where RON is the flow rate in kg/ha*day
between the active and stable organic N pools, BKN is the rate constant
(~~1 x 10(5) /day), ON(s) is the stable organic N pool, and subscript
l is the soil layer number. The daily flow of humus related organic
N (RON) is added to the stable pool and subtracted from the active
pool.
Equation:
  RON = BKN * (ON(a) * (1.0 / RTN) - ON(s))
Code:
  RON = BKN * (ON(a) * (1.0 / RTN - 1.0) - ON(s))
Variables:
  RON = EQ::OrganicNFromActiveToStableInHumusForLayer_kgPha
  BKN = kRateConstantForOrganicNFlowInHumus_Pday = 0.00001
  ON(a) = organicNActiveHumus_kgPha
  RTN = organicNActiveHumusFraction_frn
  ON(s) = organicNStableHumus_kgPha
 }
{ notation: .1e-4 }
const kRateConstantForOrganicNFlowInHumus_Pday: single = 0.00001;

{ EQN: 180 }
class function EQ.OrganicNFromActiveToStableInHumusForLayer_kgPha(organicNActiveHumus_kgPha: single;
  organicNStableHumus_kgPha: single; organicNActiveHumusFractionAtInput_frn: single): single;
  begin
  try
  if (organicNActiveHumusFractionAtInput_frn = 0.0) then 
    result := 0.0
  else
    result := kRateConstantForOrganicNFlowInHumus_Pday * (organicNActiveHumus_kgPha * (safediv(1.0,
      organicNActiveHumusFractionAtInput_frn) - 1.0) - organicNStableHumus_kgPha);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.OrganicNFromActiveToStableInHumusForLayer_kgPha: ' + e.message); end;
  end;

{ 
Only the active pool of organic N is subjected to mineralization. The humus
mineralization equation is [EQN: 181] where HMN is the mineralization rate
in kg/ha*day for the active organic N pool in layer l, CMN is the humus
rate constant (~~0.0003 /day), BD is the settled bulk density of the soil
in t/m3, and BDP is the current bulk density as affected by tillage in
t/m3.
Equation:
  HMN = CMN * ON(a) * sqrt(SWF * TF(n)) * sqr(BD) / sqr(BDP)
Code:
  same
Variables:
  HMN = ActiveHumusNMineralizationForLayer_kgPha
  CMN = kHumusMineralizationConst_Pday = 0.0003
  ON(a) = organicNActiveHumus_kgPha
  SWF = soilWaterOverFieldCapacity
  TF(n) = nutrientCyclingTempFactor
  BD = plowDepthSettledBulkDensityAtInput_tPm3
  BDP = bulkDensity_tPm3
 }
const kHumusMineralizationConst_Pday: single = 0.0003;

{ EQN: 181 }
class function EQ.ActiveHumusNMineralizationForLayer_kgPha(layer: integer; var layers: LayerStructureArray;
  plowDepthSettledBulkDensityAtInput_tPm3: single): single;
  var
    { organicNActiveHumus_kgPha is used before some of it moved out into organicNStableHumus_kgPha; }
    { the reason this is done is because the movement is so slow that mineralization }
    { can occur on all the organic n active humus, not just the part left behind. }
    { adding it back on in a local variable makes it so that calculation doesn't have to be }
    { upset by waiting to subtract the active n moving until now. }
    organicNActiveHumusBeforeMovement_kgPha: single;
    soilWaterOverFieldCapacity: single;
  begin
  try
  organicNActiveHumusBeforeMovement_kgPha := layers[layer].organicNActiveHumus_kgPha +
    layers[layer].organicNFromActiveToStableInHumus_kgPha;
  soilWaterOverFieldCapacity := EQ.SoilWaterOverFieldCapacityForLayer(layers[layer].waterContent_mm,
    layers[layer].fieldCapacity_mm);
  if (layers[layer].bulkDensity_tPm3 = 0.0) then 
    result := 0.0
  else
    result := kHumusMineralizationConst_Pday * organicNActiveHumusBeforeMovement_kgPha * sqrt(soilWaterOverFieldCapacity
      * layers[layer].nutrientCyclingTempFactor) * safediv(sqr(plowDepthSettledBulkDensityAtInput_tPm3),
      sqr(layers[layer].bulkDensity_tPm3));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.ActiveHumusNMineralizationForLayer_kgPha: ' + e.message); end;
  end;

{ 
To maintain the N balance at the end of the day, the humus mineralization is
subtracted from the active organic N pool. The residue mineralization (fresh)
is subtracted from the FON pool, 20% of RMN is added to the active ON pool,
and 80% of RMN is added to WNO3 pool.
 }
{ 
The crop residue is reduced by using the equation [EQN: 184] where FR(o) and
FR are the amounts of residue in soil layer l at the start and end of a day
in kg/ha.
Equation:
  FR = FR(o) - DCR' * FR(o)
Code:
  same
Variables:
  FR = EQ::FlatCropResidueDecay_tPha
  FR(o) = flatCropResidue_tPha
  DCR' = adjustedNDecayRateConst
 }
{ EQN: 184 }
class function EQ.FlatCropResidueDecay_tPha(decayRateConst: single; flatCropResidue_tPha: single): single;
  begin
  try
  result := flatCropResidue_tPha * decayRateConst;
  except on e: Exception do result := errorMessage('Exception in EQ.FlatCropResidueAdjForMinAndImmob_tPha: ' + e.message); end;
  end;

{ 
Finally, the immobilized N is added to the FON pool and subtracted from the
WNO3 pool.

Nitrification

Nitrification, the conversion of ammonia N to nitrate N, is estimated
using a combination of the methods of Reddy et al. (1979) and Godwin
et al. (1984). The approach is based on the first-order kinetic
rate equation of Reddy et al. (1979)
  RNV = WNH3 (1.0 - exp(-AKN - AKV))                     (Equation 185)
where RNV is the combined nitrification and volatilization in kg/ha,
WNH3 is the weight of NH3 in kg/ha, AKN is the nitrification regulator,
and AKV is the volatilization regulator for soil layer l.

The nitrification regulator is a function of temperature, soil water
content, and soil pH as expressed in the equation [EQN: 186] where
TF is the temperature factor.
Equation:
  AKN = TF * SWF * PHF
Code:
  same
Variables:
  AKN = EQ::NitrifRegulatorForLayer
  TF = nitrifAndVolatilTempFactor
  SWF = nitrifSoilWaterFactor
  PHF = nitrifPHFactor
 }
{ EQN: 186 }
class function EQ.NitrifRegulatorForLayer(nitrifTempFactor: single; nitrifSoilWaterFactor: single; nitrifPHFactor: single):
  single;
  begin
  try
  result := nitrifTempFactor * nitrifSoilWaterFactor * nitrifPHFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.NitrifRegulatorForLayer: ' + e.message); end;
  end;

{ 
The temperature factor is estimated with the equation [EQN: 187] where
T(l) is the temperature of soil layer l in degrees C.
Equation:
  TF = 0.41 * (T - 5.0) / 10.0
Code:
  same
Variables:
  TF = NitrifAndVolatilTempFactorForLayer
  T = temperature_degC
 }
{ EQN: 187 }
class function EQ.NitrifAndVolatilTempFactorForLayer(temperature_degC: single): single;
  begin
  try
  result := 0.41 * (temperature_degC - 5.0) / 10.0;
  except on e: Exception do result := errorMessage('Exception in EQ.NitrifAndVolatilTempFactorForLayer: ' + e.message); end;
  end;

{ 
The soil water factor, SWF, is computed with the equations [EQN: 188],
[EQN: 189] and [EQN: 190] where SW is the soil water content, WP
is the wilting point soil water content, FC is the field capacity
soil water content, and SW25 is the water content at WP+0.25(FC-WP)
all in mm for soil layer l.
Equation:
  SW25 = WP + 0.25 * (FC - WP)
  if SW < SW25, SWF = (SW - WP) / (SW25 - WP)
  if SW25 < SW < FC, SWF = 1.0
  if SW > FC, SWF = max(0.0, 1.0 - (SW - FC) / (PO - FC))
Code:
  same, except code adds another case: if SW < WP, SWF = 0
Variables:
  SWF = NitrifSoilWaterFactorForLayer
  SW = waterContent_mm
  WP = wiltingPoint_mm
  FC = fieldCapacity_mm
  SW25 = threshold_mm
  PO = porosity_mm
 }
{ EQN: 188 EQN: 189 EQN: 190 }
class function EQ.NitrifSoilWaterFactorForLayer(layer: integer; var layers: LayerStructureArray): single;
  var
    threshold_mm: single;
  begin
  try
  threshold_mm := layers[layer].wiltingPoint_mm + 0.25 * (layers[layer].fieldCapacity_mm -
    layers[layer].wiltingPoint_mm);
  { first case: SW < WP }
  if (layers[layer].waterContent_mm <= layers[layer].wiltingPoint_mm) then 
    result := 0.0
  { second case: SW < SW25 }
  else if (layers[layer].waterContent_mm <= threshold_mm) then
    begin
    if (threshold_mm = layers[layer].wiltingPoint_mm) then 
      result := 0.0
    else
      result := safediv(layers[layer].waterContent_mm - layers[layer].wiltingPoint_mm, threshold_mm -
        layers[layer].wiltingPoint_mm);
    end
  { third case: SW25 < SW < FC }
  else if (layers[layer].waterContent_mm <= layers[layer].fieldCapacity_mm) then
    result := 1.0
  { fourth case: SW > FC }
  else if (layers[layer].porosity_mm = layers[layer].fieldCapacity_mm) then
    result := 1.0
  else
    result := max(0.0, 1.0 - safediv(layers[layer].waterContent_mm - layers[layer].fieldCapacity_mm,
      layers[layer].porosity_mm - layers[layer].fieldCapacity_mm));
  except on e: Exception do result := errorMessage('Exception in EQ.NitrifSoilWaterFactorForLayer: ' + e.message); end;
  end;

{ 
The soil pH factor PHF is computed with the equations [EQN: 191], [EQN: 192]
and [EQN: 193] where PH is the soil pH for soil layer l.
Equation:
  if PH < 7, PHF = 0.307 * PH - 1.269
  if 7 < PH < 7.4, PHF = 1.0
  if PH > 7.4, PHF = 5.367 - 0.599 * PH
Code:
  same
Variables:
  PHF = NitrifPHFactorForLayer
  PH = soilpH
 }
{ EQN: 191 EQN: 192 EQN: 193 }
class function EQ.NitrifPHFactorForLayer(soilpH: single): single;
  begin
  try
  if (soilpH < 7.0) then 
    result := 0.307 * soilpH - 1.269
  else if (soilpH < 7.4) then
    result := 1.0
  else
    result := 5.367 - 0.599 * soilpH;
  except on e: Exception do result := errorMessage('Exception in EQ.NitrifPHFactorForLayer: ' + e.message); end;
  end;

{ 
Volatilization

Volatilization, the loss of ammonia to the atmosphere, is estimated
simulataneously with nitrification. Volatilization of surface-applied
ammonia is estimated as a function of temperature and wind speed
using the equation [EQN: 194] where AKV is the volatilization
regulator from equation 185 and WNF is the wind speed factor
for surface application (soil layer 1). (What this means is that
a different set of equations is used for the first soil layer than
for the other soil layers.)
Equation:
  AKV = TF * WNF
Code:
  same
Variables:
  AKV = EQ::VolatilRegulatorForSurfaceLayer
  TF = nitrifAndVolatilTempFactor
  WNF = volatilWindSpeedFactorForSurface
 }
{ EQN: 194 }
class function EQ.VolatilRegulatorForSurfaceLayer(nitrifAndVolatilTempFactor: single; volatilWindSpeedFactorForSurface:
  single): single;
  begin
  try
  result := nitrifAndVolatilTempFactor * volatilWindSpeedFactorForSurface;
  except on e: Exception do result := errorMessage('Exception in EQ.VolatilRegulatorForSurfaceLayer: ' + e.message); end;
  end;

{ 
The wind speed factor for surface volatilization is estimated with this
equation [EQN: 195] where V is the mean wind speed in m/sec for the day.
Equation:
  WNF = 0.335 + 0.16 * ln(V)
Code:
  same
Variables:
  WNF = EQ::VolatilWindSpeedFactorForSurface
  V = meanWindSpeedForDay_mPsec
 }
{ EQN: 195 }
class function EQ.VolatilWindSpeedFactorForSurface(meanWindSpeedForDay_mPsec: single): single;
  begin
  try
  result := 0.335 + 0.16 * safeLn(meanWindSpeedForDay_mPsec);
  except on e: Exception do result := errorMessage('Exception in EQ.VolatilWindSpeedFactorForSurface: ' + e.message); end;
  end;

{ 
Depth of ammonia within the soil, cation exchange capacity of the soil,
and soil temperature are used in estimating below surface volatilization
[EQN: 196] where CECF is the cation exchange capacity factor.
Equation:
  AKV = TF * CECF * DPF
Code:
  same
Variables:
  AKV = VolatilRegulatorForNonSurfaceLayer
  TF = nitrifAndVolatilTempFactor
  CECF = volatilCationExchangeCapFactor
  DPF = volatilDepthFactor
 }
{ EQN: 196 }
class function EQ.VolatilRegulatorForNonSurfaceLayer(nitrifAndVolatilTempFactor: single; volatilCationExchangeCapFactor:
  single; volatilDepthFactor: single): single;
  begin
  try
  result := nitrifAndVolatilTempFactor * volatilCationExchangeCapFactor * volatilDepthFactor;
  except on e: Exception do result := errorMessage('Exception in EQ.VolatilRegulatorForNonSurfaceLayer: ' + e.message); end;
  end;

{ 
The cation exchange capacity factor, CECF, is estimated with the equation
[EQN: 197] where CEC is the cation exchange capacity for soil layer l.
Equation:
  CECF = 1.0 - 0.038 * CEC, CECF >= 0
Code:
  same
Variables:
  CECF = VolatilCationExchangeCapFactorForLayer
  CEC = cationExchangeCapacity_cmolPkg
 }
{ EQN: 197 }
class function EQ.VolatilCationExchangeCapFactorForLayer(cationExchangeCapacity_cmolPkg: single): single;
  begin
  try
  result := max(0.0, 1.0 - 0.038 * cationExchangeCapacity_cmolPkg);
  except on e: Exception do
  	result := errorMessage('Exception in EQ.VolatilCationExchangeCapFactorForLayer: ' + e.message); end;
  end;

{ 
The depth factor, DPF, is computed with the equation [EQN: 198] where ZZ is
the depth to the middle of soil layer l in mm. The parameters in
equation 198 are set to give DPF = 0.95 at ZZ = 5 mm and DPF = 0.05 at
ZZ = 100 mm.
Equation:
  DPF = 1.0 - ZZ / (ZZ + exp(4.706 - 0.0305 * ZZ))
Code:
  same
Variables:
  DPF = VolatilDepthFactorForLayer
  ZZ = depthToMiddleOfSoilLayer_mm
 }
{ EQN: 198 }
class function EQ.VolatilDepthFactorForLayer(depthToMiddleOfSoilLayer_mm: single; var nVolatilizationByDepthCoeffs:
  SCurveStructure): single;
  begin
  try
  result := 1.0 - scurve(depthToMiddleOfSoilLayer_mm, nVolatilizationByDepthCoeffs.c1,
    nVolatilizationByDepthCoeffs.c2);
  except on e: Exception do result := errorMessage('Exception in EQ.VolatilDepthFactorForLayer: ' + e.message); end;
  end;

{ 
The depth to the middle of the soil layer is computed with [EQN: 199]
where Z is the depth to the bottom of layer l in mm.
Equation:
  ZZ = 1000 * (Z(l) + Z(l-1)) / 2
Code:
  same
Variables:
  ZZ = EQ::DepthToMiddleOfSoilLayer_mm
  Z(l) = depth_m
  Z(l-1) = depthUpperLayer_m
 }
{ EQN: 199 }
class function EQ.DepthToMiddleOfSoilLayer_mm(layer: integer; var layers: LayerStructureArray): single;
  begin
  try
  result := m_to_mm * (layers[layer].depth_m + layers[layer - 1].depth_m) / 2.0;
  except on e: Exception do result := errorMessage('Exception in EQ.DepthToMiddleOfSoilLayer_mm: ' + e.message); end;
  end;

{ 
To partition nitrification and volatilization rates appropriately, equation
185 is solved for each process separately [EQN: 200] and [EQN: 201]
where RNIT and RVOL are the separate estimates of nitrification and
volatilization. However, to satisfy equation 185, equations 200 and 201
must be adjusted. Thus, the final equations [EQN: 202] and [EQN: 203].
Equation:
  RNIT = 1.0 - exp(-AKN)
Code:
  same
Variables:
  RNIT = EQ::UnadjNitrifForLayer_kgPha
  AKN = nitrifRegulatorForLayer
 }
{ EQN: 200 }
class function EQ.UnadjNitrifForLayer_kgPha(nitrifRegulator: single): single;
  begin
  try
  result := 1.0 - safeExp(-nitrifRegulator);
  except on e: Exception do result := errorMessage('Exception in EQ.UnadjNitrifForLayer_kgPha: ' + e.message); end;
  end;

{ 
Equation:
  RVOL = 1.0 - exp(-AKV)
Code:
  same
Variables:
  RVOL = EQ::UnadjVolatilForLayer_kgPha
  AKV = volatilRegulator
 }
{ EQN: 201 }
class function EQ.UnadjVolatilForLayer_kgPha(volatilRegulator: single): single;
  begin
  try
  result := 1.0 - safeExp(-volatilRegulator);
  except on e: Exception do result := errorMessage('Exception in EQ.UnadjVolatilForLayer_kgPha: ' + e.message); end;
  end;

{ 
Equation:
  RNV = WNH3 * (1.0 - exp(-AKN - AKV))
Code:
  same
Variables:
  RNV = EQ::CombinedNitrifAndVolatilForLayer_kgPha
  WNH3 = ammonia_kgPha
  AKN = nitrifRegulator
  AKV = volatilRegulator
 }
{ EQN: 185 }
class function EQ.CombinedNitrifAndVolatilForLayer_kgPha(ammonia_kgPha: single; nitrifRegulator: single; volatilRegulator:
  single): single;
  begin
  try
  result := ammonia_kgPha * (1.0 - safeExp(-nitrifRegulator - volatilRegulator));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.CombinedNitrifAndVolatilForLayer_kgPha: ' + e.message); end;
  end;

{ 
Equation:
  RNIT* = RNV - RVOL*
Code:
  RNIT* = RNV * RNIT / (RVOL + RNIT)
  (nitrif and volatil are switched, but it comes to the same thing)
Variables:
  RNIT* = EQ::NitrificationForLayer_kgPha
  RNV = combinedNitrifAndVolatil_kgPha
  RNIT = unadjNitrif_kgPha
  RVOL = unadjVolatil_kgPha
 }
{ EQN: 203 }
class function EQ.NitrificationForLayer_kgPha(combinedNitrifAndVolatil_kgPha: single; unadjNitrif_kgPha: single;
  unadjVolatil_kgPha: single): single;
  begin
  try
  if (unadjVolatil_kgPha + unadjNitrif_kgPha = 0.0) then 
    result := 0.0
  else
    result := combinedNitrifAndVolatil_kgPha * safediv(unadjVolatil_kgPha, unadjVolatil_kgPha + unadjNitrif_kgPha);
  except on e: Exception do result := errorMessage('Exception in EQ.NitrificationForLayer_kgPha: ' + e.message); end;
  end;

{ 
Equation:
  RVOL* = RNV * RVOL / (RVOL + RNIT)
Code:
  RVOL* = RNV - RNIT*
  (nitrif and volatil are switched, but it comes to the same thing)
Variables:
  RVOL* = EQ::VolatilizationForLayer_kgPha
  RNV = combinedNitrifAndVolatil_kgPha
  RNIT* = nitrification_kgPha
 }
{ EQN: 202 }
class function EQ.VolatilizationForLayer_kgPha(combinedNitrifAndVolatil_kgPha: single; nitrification_kgPha: single): single;
  begin
  try
  result := combinedNitrifAndVolatil_kgPha - nitrification_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.VolatilizationForLayer_kgPha: ' + e.message); end;
  end;

{ 
Rainfall

To estimate the N contribution from rainfall, EPIC uses an average rainfall
N concentration for a location for all storms. The amount of N in rainfall
is estimated as the product of rainfall amount and concentration.

                              NUTRIENTS - *PHOSPHORUS*

The divisions of P are:
  labileP_kgPha
  organicPFresh_kgPha - in crop residue and microbial biomass
  organicPHumus_kgPha - in humus
  mineralPActive_kgPha
  mineralPStable_kgPha

Soluble P Loss in Surface Runoff

The EPIC approach is based on the concept of partitioning pesticides into
the solution and sediment phases as described by Leonard and Wauchope
(Knisel, 1980). (I think they might mean phosphorus, not pesticides.)
Because P is mostly associated with the sediment phase, the soluble P
runoff equation can be expressed in the simple form [EQN: 204] where
YSP is the soluble P in kg/ha lost in runoff volume Q in mm, C(LP) is
the concentration of AP in soil layer l in g/t, and k(d) is the P
concentration of the sediment divided by that of water in m3/t. The
value of k(d) used in EPIC is 175.
Equation:
  YSP = 0.01 * C(LP) * Q / k(d)
Code:
  the equation used for runoff and percolation of P is very diffferent
  in the code. This equation computes not kg/ha of P leached, but the
  concentration of soluble p in the leached water. The equation is
  meanPConcDuringWaterFlow_gPm3 = AP * min(0.75, Q / SW) / Q where
  Q is runoff + percolation for the first layer, or percolation for others
  This is then multiplied by runoff or percolation to get kg/ha.
Variables:
  YSP = SolublePLostInRunoffForLayer_kgPha
  C(LP) = labilePConcForLayer_gPt
  Q = runoffVolume_mm
  k(d) = kPhosInSedimentOverWater_m3Pt = 175.0
 }
{#define kPhosInSedimentOverWater_m3Pt 175.0 }
{ EQN: 204 }
class function EQ.MeanPConcDuringLeaching_gPm3(labileP_kgPha: single; volumeEnteringLayer_mm: single; weightForLayer_tPha:
  single): single;
  begin
  try
  result := safediv(labileP_kgPha * min(0.75, safediv(volumeEnteringLayer_mm, weightForLayer_tPha)),
    volumeEnteringLayer_mm);
  except on e: Exception do result := errorMessage('Exception in EQ.MeanPConcDuringLeaching_gPm3: ' + e.message); end;
  end;

{ 
P Transport by Sediment

Sediment transport of P is simulated with a loading function as described
in organic N transport. The P loading function is [EQN: 205] where YP
is the sediment phase P lost in runoff in kg/ha and c(p) is the concentration
of P in the top soil layer in g/t.
 }
{ EQN: 205 }
{ use equation 164 }
{ 
Mineralization

The P mineralization model developed by Jones et al. (1984) is similar in structure
to the N mineralization model. Mineralization from the fresh organic P pool is
estimated for each soil layer with the equation [EQN: 206] where RMP is the
mineralization rate of fresh organic P in layer l in kg/ha and FOP is
the fresh organic P in crop residue in kg/ha.
Equation:
  RMP = DCR * FOP
Code:
  same
Variables:
  RMP = EQ::FreshPMineralizationForLayer_kgPha
  DCR = decayRateConst
  FOP = organicPFresh_kgPha
 }
{ EQN: 206 }
class function EQ.FreshPMineralizationForLayer_kgPha(decayRateConst: single; organicPFresh_kgPha: single): single;
  begin
  try
  result := decayRateConst * organicPFresh_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.FreshPMineralizationForLayer_kgPha: ' + e.message); end;
  end;

{ 
Mineralization of organic P associated with humus is estimated for each soil
layer by using the equation [EQN: 207] where HMP is the humus P mineralization
rate in kg/ha and OP is the organic P content of soil layer l in kg/ha.
The ratio of ON(a) to ON is used to in equation 207 to calculate the active
portion of the OP pool. (It is the same proportion as in the N pool, I guess.)
This eliminates the need for maintaining two OP pools corresponding to the
ON(a) and ON(s) pools.
Equation:
  HMP = (CMN * ON(a) * OP * sqrt(SWF * TF(n)) * sqr(BD)) / (ON * sqr(BDP))
Code:
  same except for multiplier of 1.4
  HMP = 1.4 * HMN * OP / ON
  HMN = CMN * ON(a) * sqrt(SWF * TF(n)) * sqr(BD) / sqr(BDP)
  so HMP = 1.4 * (CMN * ON(a) * OP * sqrt(SWF * TF(n)) * sqr(BD)) / (ON * sqr(BDP))
  note that this must be done after n mineralization
Variables:
  HMP = EQ::HumusPMineralizationForLayer_kgPha
  HMN = activeHumusNMineralization_kgPha
  ON = organicNTotalHumus_kgPha
  OP = organicPHumus_kgPha
 }
{ EQN: 207 }
class function EQ.HumusPMineralizationForLayer_kgPha(layer: integer; var layers: LayerStructureArray;
  activeHumusNMineralization_kgPha: single): single;
  var
    organicNTotalHumus_kgPha: single;
  begin
  try
  organicNTotalHumus_kgPha := layers[layer].organicNActiveHumus_kgPha + layers[layer].organicNStableHumus_kgPha;
  if (organicNTotalHumus_kgPha = 0.0) then 
    result := 0.0
  else
    result := 1.4 * activeHumusNMineralization_kgPha * safediv(layers[layer].organicPHumus_kgPha,
      organicNTotalHumus_kgPha);
  except on e: Exception do result := errorMessage('Exception in EQ.HumusPMineralizationForLayer_kgPha: ' + e.message); end;
  end;

{ 
To maintain the P balance at the end of a day, humus mineralization is
subtracted from the organic P pool, residue mineralization is subtracted
from the FOP pool, 20% of RMP is added to the OP pool, and 80% of RMP
is added to the labile (fresh) P pool.
 }
{ 
Mineral P Cycling

The mineral P model was developed by Jones et al. (1984). Mineral P is
transferred among three pools: labile, active mineral, and stable
mineral. Fertilizer P is labile (available for plant use) at application
but may be quickly transferred to the active mineral pool. Flow between
the labile and active mineral pools is governed by the equilibrium
equation [EQN: 211] where MPR is the mineral P flow rate for layer l
in kg/ha, MP(a) is the amount in the active mineral P pool in kg/ha,
and PSP is the P sorption coefficient defined as the fraction of
fertilizer P remaining in the labile pool after the initial rapid
phase of P sorption is complete. The daily amount of P computed with
equation 211 flows to the active mineral P pool and is, therefore,
added to that pool and subtracted from the labile pool. Obviously,
the flow reverses when labile P is less than MP(a) * PSP / (1-PSP).
Since reverse flow is much slower, equation 211 is multiplied by
0.1 when the resulting MPR is negative.
Equation:
  MPR = AP - MP(a) * (PSP / (1.0 - PSP))
  if MPR < 0, MPR *= 0.1
Code:
  same except PSP / (1.0 - PSP) is bounded at 0.8
Variables:
  MPR = PFlowFromLabileToActiveMineral_kgPha
  AP = labileP_kgPha
  MP(a) = mineralPActive_kgPha
  PSP = pSorptionCoeff_frn
 }
{ EQN: 211 }
class function EQ.PFlowFromLabileToActiveMineral_kgPha(labileP_kgPha: single; mineralPActive_kgPha: single;
  pSorptionCoeff_frn: single): single;
  begin
  try
  if (1.0 - pSorptionCoeff_frn = 0.0) then 
    result := 0.0
  else
    result := labileP_kgPha - mineralPActive_kgPha * min(0.8, safediv(pSorptionCoeff_frn, 1.0 - pSorptionCoeff_frn));
  if (result < 0.0) then result := result * 0.1;
  except on e: Exception do result := errorMessage('Exception in EQ.PFlowFromLabileToActiveMineral_kgPha: ' + e.message); end;
  end;

{ 
The P sorption coefficient is a function of chemical and physical
soil properties as described by the following equations (Jones et
al., 1984). In calcareous soils, [EQN: 212]. In noncalcareous,
slightly weathered soils, [EQN: 213]. In noncalcareous, moderately
weathered soils, [EQN: 214]. In noncalcareous, highly weathered
soils, [EQN: 215] where PSP is the P sorption coefficient for soil
layer l, CAC is the CaCO3 concentration in g/t, and BSA is the
base saturation by the ammonium acetate (NH4OAc) method in percent.
PSP is constrained within the limits of 0.05 <= PSP <= 0.75.
Equation:
  in calcareous soils,
    PSP = 0.58 - 0.0061 * CAC
  in noncalcareous, slightly weathered soils,
    PSP = 0.02 + 0.0104 * AP
  in noncalcareous, moderately weathered soils,
    PSP = 0.0054 * BSA + 0.116 * PH - 0.73
  in noncalcareous, highly weathered soils,
    PSP = 0.46 - 0.0916 * ln(CLA)
  PSP is constrained within the limits of 0.05 <= PSP <= 0.75
Code:
  in calcareous soils,
    if CAC > 0 PSP = 0.58 - 0.0061 * CAC
    if CAC <= 0 PSP = 0.5
  otherwise the same
Variables:
  PSP = PSorptionCoeffForLayer_frn
  CAC = calciumCarbonateConcForLayer_gPt
  AP = labilePConc_gPt
  BSA = baseSaturation_pct
  PH = soilpH
  CLA = clayContent_pct
 }
{ EQN: 212 EQN: 213 EQN: 214 EQN: 215 }
class function EQ.PSorptionCoeffForLayer_frn(calciumCarbonate_pct: single; labilePConc_gPt: single;
    baseSaturation_pct: single; soilpH: single; clayContent_pct: single; soilType: integer; pSorptionCoeff: single): single;
  begin
  try
  result := 0;
  case soilType of
    kCalcareousSoilOrWithoutWeatherInfo:
      if (calciumCarbonate_pct > 0.0) then
        result := 0.58 - 0.0061 * calciumCarbonate_pct
      else
        result := 0.5;
    kSlightlyWeatheredSoil:
      result := 0.02 + 0.0104 * labilePConc_gPt;
    kModWeatheredSoil:
      result := 0.0054 * baseSaturation_pct + 0.116 * soilpH - 0.73;
    kHighlyWeatheredSoil:
      result := 0.46 - 0.0916 * safeLn(clayContent_pct);
    kInputPSorptionCoeffs:
      result := pSorptionCoeff;
    end;
  if (result < 0.05) then result := 0.05;
  if (result > 0.75) then result := 0.75;
  except on e: Exception do result := errorMessage('Exception in EQ.PSorptionCoeffForLayer_frn: ' + e.message); end;
  end;

{ 
At equilibrium the stable P pool is assumed to be four times as large
as the active mineral P pool. Flow between the P pools is governed
by the equation [EQN: 216] where ASPR is the flow rate between the
active and stable mineral P pools in kg/ha for soil layer l, omega
is the flow coefficient in /day, and MP(s) is the amount of stable
mineral P in kg/ha. The daily amount of P computed with equation 216
flows into the stable pool and is subtracted from the active pool.
Obviously, the flow reverses when MP(s) > 4MP(a). Since reverse
flow is much slower, equation 216 is multiplied by 0.1 when the
resulting ASPR is negative.
Equation:
  ASPR = omega * (4 * MP(a) - MP(s))
  if ASPR < 0.0, ASPR *= 0.1
Code:
  same
Variables:
  ASPR = EQ::MineralPFlowFromActiveToStableForLayer_kgPha
  omega = mineralPFlowCoeff_Pday
  MP(a) = mineralPActive_kgPha
  MP(s) = mineralPStable_kgPha
 }
{ EQN: 216 }
class function EQ.MineralPFlowFromActiveToStableForLayer_kgPha(mineralPFlowCoeff_Pday: single; mineralPActive_kgPha: single;
  mineralPStable_kgPha: single): single;
  begin
  try
  result := mineralPFlowCoeff_Pday * (4.0 * mineralPActive_kgPha - mineralPStable_kgPha);
  if (result < 0.0) then result := result * 0.1;
  except on e: Exception do
  	result := errorMessage('Exception in EQ.MineralPFlowFromActiveToStableForLayer_kgPha: ' + e.message); end;
  end;

{ 
The flow coefficient, omega, is a function of PSP as expressed by the equations
(Jones et al., 1984) [EQN: 217] for noncalcareous soils, and [EQN: 218] for
calcareous soils.
Equation:
  in noncalcareous soils,
    omega = exp(-1.77 * PSP - 7.05)
  in calcareous soils,
    omega = 0.0076
Code:
  same
Variables:
  omega = MineralPFlowCoeffForLayer_Pday
  PSP = pSorptionCoeff_frn
 }
{ EQN: 217 EQN: 218 }
class function EQ.MineralPFlowCoeffForLayer_Pday(pSorptionCoeff_frn: single; calciumCarbonate_pct: single; soilType: integer):
  single;
  begin
  try
  { notation: 6.595e-4 }
  if ((((soilType = kCalcareousSoilOrWithoutWeatherInfo) or (soilType = kInputPSorptionCoeffs))) and
    (calciumCarbonate_pct > 0.0)) then 
    result := 0.0076
  else
    result := safeExp(-1.77 * pSorptionCoeff_frn - 7.05);
  except on e: Exception do result := errorMessage('Exception in EQ.MineralPFlowCoeffForLayer_Pday: ' + e.message); end;
  end;

(* not using this code
{ 
                                          *PESTICIDE* FATE

pesticide can be applied to the leaves or the ground. It is lost by
  evaporation during application
  moving from the leaves to the soil with rainfall (if applied to the leaves)
  "loss from the system" from the leaves (if applied to the leaves)
  "loss from the system" from the soil (evaporation?)
  runoff (first layer only)
  leaching (all layers)
  lateral flow (all layers)
  eroding out with sediment (first layer only?)

Pesticide lost during application

GLEAMS (Leonard et al., 1987) technology for simulating pesticide
transport by runoff, percolate, soil evaporation, and sediment was
added to EPIC. Pesticides may be applied at any time and rate to
plant foliage or below the soil surface at any depth. When pesticide
is applied, some is lost to the atmosphere. Thus, the amount that
reaches the ground or plants is expressed by the equation [EQN: 219]
where PAPE is the effective amount of pesticide applied in kg/ha,
PAPR is the actual amount applied in kg/ha, and PAEF is an application
efficiency factor (input).
Equation:
  PAPE = PAPR * PAEF
Code:
  same except for 1000 multiplier
  (which I can't see the necessity of in terms of units)
Variables:
  PAPE = EQ::PesticideEffectiveApplication_kgPha
  PAPR = pesticideApplication_kgPha
  PAEF = efficiency_frn
 }
{ EQN: 219 }
class function EQ.PesticideEffectiveApplication_kgPha(pesticideApplication_kgPha: single; efficiency_frn: single): single;
  begin
  try
  result := pesticideApplication_kgPha * efficiency_frn * 1000;
  except on e: Exception do result := errorMessage('Exception in EQ.PesticideEffectiveApplication_kgPha: ' + e.message); end;
  end;

{ 
Pesticide that stays on leaves vs. pesticide that falls to ground

To determine how much pesticide reaches the ground, the amount of ground cover
provided by plants is estimated with the equation [EQN: 220] where GC is
the fraction of the ground that is covered by plants, and LAI is the leaf
area index on day l.
Equation:
  GC = (1 - ERFC(1.33 * LAI - 2.0)) / 2
Code:
  same (equation for function ERFC not given in chapter)
Variables:
  GC = EQ::FractionOfGroundCoveredByPlants_frn
  LAI = leafAreaIndex
 }
{ EQN: 220 }
class function EQ.FractionOfGroundCoveredByPlants_frn(leafAreaIndex: single): single;
  begin
  try
  result := (1.0 - erfc(1.33 * leafAreaIndex - 2.0)) / 2.0;
  except on e: Exception do result := errorMessage('Exception in EQ.FractionOfGroundCoveredByPlants_frn: ' + e.message); end;
  end;

{ 
Therefore, the amount of pesticide that reaches the plants is computed with the
equation [EQN: 221] where FP is the amount of pesticide that is intercepted
by plants.
Equation:
  FP = GC * PAPE
Code:
  same
Variables:
  FP = EQ::PesticideAppliedToPlants_kgPha
  GC = fractionOfGroundCoveredByPlants_frn
  PAPE = pesticideEffectiveApplication_kgPha
 }
{ EQN: 221 }
class function EQ.PesticideAppliedToPlants_kgPha(fractionOfGroundCoveredByPlants_frn: single;
  pesticideEffectiveApplication_kgPha: single): single;
  begin
  try
  result := fractionOfGroundCoveredByPlants_frn * pesticideEffectiveApplication_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.PesticideAppliedToPlants_kgPha: ' + e.message); end;
  end;

{ 
The remaining pesticide falls to the ground and is simply the difference between
the effective amount applied and the amount intercepted by plants [EQN: 222]
where GP is the amount of pesticide that reaches the ground.
// EQN: 222 (in code, very simple)
//  pesticideAppliedToGround_kgPha = effectivePesticideApplic_kgPha
//    - pesticideAppliedToFoliage_kgPha;

Pesticide washed off leaves by rain storms

Pesticide that remains on the plant foliage can be washed off by rain
storms. It is assumed that the fraction of pesticide that is potentially
dislodgeable is washed off the plants once a threshold rainfall amount
is exceeded. The model uses a threshold value of 2.5 mm and potential
washoff fraction for various pesticides have been estimated (Leonard
et al., 1987). The appropriate equations for computing washoff and
adding to the ground amount are [EQN: 223], [EQN: 224] and [EQN: 225]
where WO is the amount of pesticide washed off the plants by a rainstorm
of R mm and WOF is the washoff fraction for the particular pesticide.
Equation:
  if R >= 2.5 mm, WO = WOF * FP
  else WO = 0
Code:
  same (if statement done outside function)
Variables:
  WO = PesticideWashedOffPlants_kgPha
  WOF = washOffFractionForPesticide_frn
  R = rainfallForDay_mm
  FP = pesticideOnPlants_kgPha
  GP = pesticideAppliedToGround_kgPha
 }
{ EQN: 223 }
class function EQ.PesticideWashedOffPlants_kgPha(washOffFractionForPesticide_frn: single; pesticideOnPlants_kgPha: single):
  single;
  begin
  try
  result := washOffFractionForPesticide_frn * pesticideOnPlants_kgPha;
  except on e: Exception do result := errorMessage('Exception in EQ.PesticideWashedOffPlants_kgPha: ' + e.message); end;
  end;

{ EQN: 224 (very simple, in code) }
{ pesticideAppliedToGround_kgPha += pesticideWashedOffPlants_kgPha; }
{ EQN: 225 (very simple, in code) }
{ pesticideAppliedToPlants_kgPha -= pesticideWashedOffPlants_kgPha; }
{ 
Pesticide "lost from the system" from the soil and leaves

Pesticide on the plants and in the soil is lost from the system
based on the decay equations [EQN: 226] and [EQN: 227] where GP(o)
and GP are the initial and final amounts of pesticide on the ground,
FP(o) and FP are the initial and final amounts of pesticide on the
plants, HLS is the half life for pesticide in the soil in days,
and HLP is the half life of the foliar residue in days. Values of
HLP and HLS have been established for various pesticides (Leonard
et al., 1987).
Equation:
 GP = GP(o) * exp(-0.693 / HLS)
Code:
Variables:
  GP = EQ::GroundPesticideAdjForDecay
  GP(o) = amountInLayer_kgPha
  HLS = groundHalfLifeForPesticide_days
 }
{ EQN: 226 }
class function EQ.GroundPesticideAdjForDecay(amountInLayer_kgPha: single; groundHalfLifeForPesticide_days: single): single;
  begin
  try
  result := amountInLayer_kgPha * safeExp(safediv(-0.693, groundHalfLifeForPesticide_days));
  except on e: Exception do result := errorMessage('Exception in EQ.GroundPesticideAdjForDecay: ' + e.message); end;
  end;

{ 
Equation:
 FP = FP(o) * exp(-0.693 / HLP)
Code:
Variables:
  FP = EQ::FoliarPesticideAdjForDecay
  FP(o) = amountOnFoliage_kgPha
  HLP = foliarHalfLifeForPesticide_days
 }
{ EQN: 227 }
class function EQ.FoliarPesticideAdjForDecay(amountOnFoliage_kgPha: single; foliarHalfLifeForPesticide_days: single): single;
  begin
  try
  result := amountOnFoliage_kgPha * safeExp(safediv(-0.693, foliarHalfLifeForPesticide_days));
  except on e: Exception do result := errorMessage('Exception in EQ.FoliarPesticideAdjForDecay: ' + e.message); end;
  end;

{ 
Pesticide leached out (percolation)

Another way that pesticide can be lost from the zone considered in
computing runoff (top 10 mm of soil) is through leaching. The GLEAMS
leaching component is used here with slight modification. The change
in the amount of pesticide contained in the top 10 mm zone is expressed
as a function of time, concentration, and amount of infiltration
using the equation
  dGP/dt = PSTC(w) * f                                  (Equation 228)
where GP is the amount of pesticide in the top 10 mm zone at time t,
PSTC(w) is the pesticide concentration in the water in g/t, and f
is the water flow rate through the zone in mm/hr.

The total amount of pesticide contained in the top 10 mm zone is the
sum of the adsorbed (in the sediment) and mobile (in the water) phases
  GP = 0.01 * PSTC(w) * SW + 0.1 * PSTC(s) * BD         (Equation 229)
where SW is the amount of water stored in the top 10 mm of soil in
mm, PSTC is the concentration of adsorbed pesticide in g/t, and BD
is the soil bulk density in t/m3.

The ratio of the concentration of pesticide adsorbed to the concentration
of pesticide in the water has been estimated for various pesticides
(Leonard et al., 1987) and is expressed by the equation
  K(d) = c(s) / c(w)                                    (Equation 230)
where K(d) is the partitioning constant.

Substituting equation 230 into equation 229 gives
  GP = 0.01 * PSTC(w) * SW + 0.1 * PSTC(w) * K(d) * BD  (Equation 231)

Solving equation 231 for PSTC(w) gives
  PSTC(w) = GP / (0.01 * SW + 0.1 * K(d) * BD)          (Equation 232)

Substituting PSTC(w) from equation 232 into equation 228 yields
  dGP/dt = GP * f / (0.01 * SW + 0.1 * K(d) * BD)       (Equation 233)

Rearranging equation 233 and integrating gives the equation expressing
the amount of pesticide as a function of the amount of water flowing
through the zone
  GP = GP(o) * exp(-f / (0.01 * SW + 0.01 * K(d) * BD)) (Equation 234)
where GP(o) is the initial amount of pesticide in the top 10 mm zone
in kg/ha, GP is the amount that remains after the amount of flow
(f in mm) passes through the zone, SW is the initial water storage
in mm, K(d) is the partitioning coefficient in m3/t, and BD is the
soil bulk density in t/m3.

To obtain the amount of pesticide leached by the amount of water f,
GP is subtracted from GP(o) using the equation [EQN: 235] where
PSTL (intermediate) is the amount of pesticide leached by f.
Pesticide concentration in percolate is computed with the equation [EQN: 236]
where PSO1 is the pesticide solubility and PSTC(L) is the pesticide
concentration in the percolate in g/m3. Finally, PSTL (final) is the product of
O and PSTC(L).
Equation:
  PSTL(interm.) = GP(o) * (1.0 - exp(-f / (0.01 * SW + 0.1 * K(d) * BD)))
  PSTC(L) = min(PSTL(interm.) / f, PSO1)
  PSTL(final) = f * PSTC(L)
Code:
  BD -> C * SWT
  0.01 * SW -> PO - WP
  PSTL(interm.) = GP(o) * (1.0 - exp(-f / (PO - WP) + 0.1 * K(d) * C * SWT))
  rest is the same
Variables:
  PSTL(final) = EQ::PesticideLeachedForLayer_kgPha
  PSTL(interm.) = pesticideLeachedByWater_kgPha
  GP(o) = amountInLayer_kgPha
  f = volumeMovingThroughLayer_mm
  SW = waterContent_mm
  PO = porosity_mm
  WP = wiltingPoint_mm
  K(d) = organicCAdsorptionCoeff
  BD = bulkDensity_tPm3
  C = organicC_frn
  SWT = weight_tPha
  PSTC(L) = pesticideConcInWaterFlow_gPm3
  PSO1 = solubility_gPm3
 }
{ EQN: 235 EQN: 236 }
class function EQ.PesticideLeachedForLayer_kgPha(layer: integer; var layers: LayerStructureArray; volumeMovingThroughLayer_mm:
  single; pesticide: GsSoilPesticide): single;
  var
    organicC_frn: single;
    denominator: single;
    pesticideLeachedByWater_kgPha: single;
    pesticideConcInWaterFlow_gPm3: single;
  begin
  try
  organicC_frn := Utils_OrganicCFromOrganicMatter_pct(layers[layer].organicMatter_tPha, layers[layer].weight_tPha) *
    pct_to_frn;
  denominator := layers[layer].porosity_mm - layers[layer].wiltingPoint_mm + 0.1 * pesticide.organicCAdsorptionCoeff *
    organicC_frn * layers[layer].weight_tPha;
  pesticideLeachedByWater_kgPha := pesticide.amountInLayer_kgPha[layer] * (1.0 - safeExp(safediv( -
    volumeMovingThroughLayer_mm, denominator)));
  pesticideConcInWaterFlow_gPm3 := min(safediv(pesticideLeachedByWater_kgPha, volumeMovingThroughLayer_mm),
    pesticide.solubility_gPm3);
  result := volumeMovingThroughLayer_mm * pesticideConcInWaterFlow_gPm3;
  except on e: Exception do result := errorMessage('Exception in EQ.PesticideLeachedForLayer_kgPha: ' + e.message); end;
  end;

{ 
Pesticide lost in surface runoff

Pesticide loss in surface runoff is estimated with a modification of
equation 235 that includes an abstraction coefficient [EQN: 237]
where PSTQ is the pesticide loss in surface runoff in kg/ha, Q
is the surface runoff volume in mm, and ab is the abstraction
coefficient.
Equation:
  PSTQ = GP(o) * (1.0 - exp(-Q * ab / (0.01 * SW + 0.1 * ab * K(d) * BD)))
Code:
  BD -> C * SWT
  0.01 * SW -> PO - WP
  PSTQ = GP(o) * (1.0 - exp(-Q * ab / ((PO-WP) + 0.1 * ab * K(d) * C * SWT)))
Variables:
  PSTQ = EQ::PesticideLeachedByRunoffAndLatFlow_kgPha
  GP(o) = pesticideInLayer_kgPha
  Q = volumeMovingThroughLayer_mm
  ab = abstractionCoeffForPesticideRunoff = 0.1
  SW = waterContent_mm
  PO = porosity_mm
  WP = wiltingPoint_mm
  K(d) = partitioningCoeffForPesticide_m3Pt
  BD = bulkDensity_tPm3
  C = organicC_frn
  SWT = weight_tPha
 }
const kAbstractionCoeff: single = 0.1;

{ EQN: 237 }
class function EQ.PesticideLeachedByRunoffAndLatFlow_kgPha(var layers: LayerStructureArray; pesticideInLayer_kgPha: single;
  volumeMovingThroughLayer_mm: single; partitioningCoeffForPesticide_m3Pt: single): single;
  var
    organicC_frn: single;
    denominator: single;
  begin
  try
  organicC_frn := Utils_OrganicCFromOrganicMatter_pct(layers[0].organicMatter_tPha, layers[0].weight_tPha) * pct_to_frn;
  denominator := layers[0].porosity_mm - layers[0].wiltingPoint_mm + kAbstractionCoeff * organicC_frn *
    layers[0].weight_tPha * 0.1 * partitioningCoeffForPesticide_m3Pt;
  result := pesticideInLayer_kgPha * (1.0 - safeExp(safediv(-volumeMovingThroughLayer_mm * kAbstractionCoeff,
    denominator)));
  except on e: Exception do
  	result := errorMessage('Exception in EQ.PesticideLeachedByRunoffAndLatFlow_kgPha: ' + e.message); end;
  end;

{ 
The calculation sequence is:
1. for first soil layer
  a. Leaching is estimated first by substituting O for f in equations 235 and
     236.
  b. PSTL is removed from layer 1 and placed into layer 2.
  c. Runoff and lateral flow losses are estimated simulataneously by
     substituting Q+QR for Q in equation 237.
  d. The average concentration PSTC(Q+QR) is obtained by dividing the rsulting PSTQ
     by Q+QR.
  e. Individual losses are estimated for runoff (Q*PSTC(Q+QR)) and lateral
     flow (QR*PSTC(Q+QR)).
  f. Finally, these losses are removed from layer 1 (to nowhere).
2.  for all the other layers
  a. Leaching and lateral flow losses are estimated simultaneously by
     substituting O+QR for f in equations 235 and 236.
  b. The average concentration PSTC(O+QR) is obtained by dividing the resulting
     PSTL by O+QR.
  c. Individual losses are calculated as in the step for the first layer:
     for lateral flow: QR*PSTC(O+QR)) and for leaching: O*PSTC(O+QR)).
  d. These amounts are subtracted from the layer in question and added
     to the next layer. This process is reapeated layer by later to
     to bottom of the soil profile. (What happens at the last layer?)

The total amount of pesticide lost in the runoff is estimated by adding
the soluble fraction computed with equation 237 to the amount that is adsorbed
to the sediment. Pesticide yield from the adsorbed phase is computed
with an enrichment ratio approach. The enrichment ratio equation is
[EQN: 238] where PSTY is the pesticide yield adsorbed to the sediment in
kg/ha, Y is the sediment yield in t/ha, and ER is the enrichment ratio
(concentration of pesticide in the sediment divided by the pesticide
concentration in the top 10 mm of soil) computed with equation 167.
Equation:
  PSTY = Y * PSTC(s) * ER * 0.001
Code:
  same except the 0.001 is lost and an upper bound of 0.9 is placed on ER * Y / SWT
Variables:
  PSTY = EQ::PesticideLostInSediment_kgPha
  Y = totalErosion_tPha
  PSTC(s) = pesticideConcInSoil_gPm3
  ER = enrichmentRatioForNPPest
  SWT = surfaceLayerSoilWeight_tPha
 }
{ EQN: 238 }
class function EQ.PesticideLostInSediment_kgPha(totalErosion_tPha: single; enrichmentRatioForNPPest: single;
  pesticideConcInSoil_gPm3: single; surfaceLayerSoilWeight_tPha: single): single;
  var
    propLost_frn: single;
  begin
  try
  propLost_frn := min(0.9, enrichmentRatioForNPPest * safediv(totalErosion_tPha, surfaceLayerSoilWeight_tPha));
  if (propLost_frn > 0.0) then 
    result := propLost_frn * pesticideConcInSoil_gPm3 * surfaceLayerSoilWeight_tPha
  else
    result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EQ.PesticideLostInSediment_kgPha: ' + e.message); end;
  end;

{ 
The pesticide concentration in the soil is calculated by substituting
equation 230 into equation 232 and solving for PSTC(s) [EQN: 239].
Equation:
  PSTC(s) = K(d) * GP / (0.01 * SW + 0.1 * K(d) * BD)
Code:
  BD -> C * SWT
  0.01 * SW -> PO - WP
  PSTC(s) = K(d) * GP * 0.001 * C / ((PO - WP) + 0.1 * K(d) * C * SWT)
Variables:
  PSTC(s) = EQ::PesticideConcInSoil_gPm3
  K(d) = partitioningCoeffForPesticide_m3Pt
  GP = pesticideInLayer_kgPha
  SW = waterContent_mm
  PO = porosity_mm
  WP = wiltingPoint_mm
  BD = bulkDensity_tPm3
  C = organicC_frn
 }
{ EQN: 239 }
class function EQ.PesticideConcInSoil_gPm3(partitioningCoeffForPesticide_m3Pt: single; pesticideInLayer_kgPha: single; var
  layers: LayerStructureArray): single;
  var
    organicC_frn: single;
    denominator: single;
  begin
  try
  organicC_frn := Utils_OrganicCFromOrganicMatter_pct(layers[0].organicMatter_tPha, layers[0].weight_tPha) * pct_to_frn;
  denominator := layers[0].porosity_mm - layers[0].wiltingPoint_mm + organicC_frn * layers[0].weight_tPha * 0.1 *
    partitioningCoeffForPesticide_m3Pt;
  result := safediv(0.1 * organicC_frn * partitioningCoeffForPesticide_m3Pt * pesticideInLayer_kgPha, denominator);
  except on e: Exception do result := errorMessage('Exception in EQ.PesticideConcInSoil_gPm3: ' + e.message); end;
  end;

{ 
Soil layers with low storage volumes have high leaching potentials
not only because percolation is greater, but also because storage
volume displacement is greater (higher concentration). Pesticides
with low K(d) values and high solubility are transported rapidly
with water. Conversely, high K(d) value pesticides are adsorbed
to soil particles and travel largely with sediment.
 }
*)

end.

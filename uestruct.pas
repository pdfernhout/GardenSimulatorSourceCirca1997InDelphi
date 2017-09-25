unit Uestruct;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uestruct: Many structures used by model objects. Structures are in this file instead of each
separate file so uep and ueq can just include this file and not the object files (avoids circularity).
Important: to add a variable to an object, add it to the end of any structure that has a buffer
at the end (usually called reservedArray), and reduce the size of the buffer by the proper amount
(single=4 bytes,smallint=2 bytes, etc, see the Delphi documentation to be sure). After a new var
is added here you must initialize it on reading from files in two ways.
1) For binary files, increment the version number of the object (in its classAndVersionInformation
function), then respond when the file is streamed in by setting the value to an initial default.
Files streamed out with the new version number will save the information. (See the streamDataWithFiler
methods in the model object files for some examples of this versioning technique.)
You can also add a var to an object at the top level (not in a structure), but then you have to pass
it more explicitly to any functions that need it (not just inside the structure). Adding a new var
to a structure works best when you don't need an aspect or to save it in the template (like something
you just need when the simulation is running).
2) For tabbed-text importing (from tabbed-text template files or from EPIC files), the problem is
harder: you cannot add a new variable to the tabbed-text data files unless you create a new version
number in the tabbed-text files and deal with it in your code (see utempman and ufilertx). But if you
create a new tabbed-text version number, your files will be unreadable by anyone else unless they have your
code. Probably it is best to just default the new value on reading the object from a tabbed-text
data file (see udefault), and that info can't be saved in tabbed-text format until there is a new
global tabbed-text version.
Note that there are some 'unused' vars in some structures that are no longer used but were left
in place to keep the binary files intact; these can be used but again cannot be put into the
tabbed-text files.
We are using only singles for float vars and only smallints for integer vars.
Comments with -> arrows are for use by the tab2asp program to compare against existing aspects to
see if any were left out (an NA on a line means the checking program will ignore the var). See the
description of the tab2asp program at the end of the programmer's guide for more information on
aspect delineation comments.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses WinTypes;

type

  arrayTwo = array[0..1] of single;
  arrayThree = array[0..2] of single;
  arrayFour = array[0..3] of single;
  arraySoilLayers = array[0..9] of single;
  arrayMonths = array[0..11] of single;
  arrayThirty = array[0..29] of single;
  windDirectionsArray = array[0..11, 0..15] of single;

SCurveStructure = record
  x1: single;
  y1: single;
  x2: single;
  y2: single;
  c1: single;
  c2: single;
  end;

SingleVarSCurveStructure = record
  x: single;
  y: single;
  c: single;
  end;

const
  channelGeometryConstants: arrayFour = (0.0208, 0.4, 0.0803, 0.6);

type
BiomassLostInReseedingStructure = record
  standingLive_tPha: single;
  rootWeightByLayer_tPha: arraySoilLayers;
  nConcInLiveBiomass_kgPt: single;
  pConcInLiveBiomass_kgPt: single;
  standingDead_tPha: single;
  nConcInStandingDead_kgPt: single;
  pConcInStandingDead_kgPt: single;
  end;

{ aspects->start }
{ ------------------------------------------------------------------------------------  Plant  }
type
{ object->plant }
{ access->params }
PlantParamsStructure = record
  { type }
  lifeCycleType: smallint{ENUM};
  isLegume: boolean;
  isTree: boolean;
  yearsToMaturityIfTree: single;
  { planting and germination }
  seedWeight_g: single;
  plantingDepth_mm: single;
  probOfGerminationAfterFirstYear_frn: single;
  minSoilWaterInPlowDepthForGermination_mm: single;
  minHeatUnitsBeforeGermination_degC: single;
  areaOfSoilPatchInWhichPlanted_ha: single;  {NA} {internal use} 
  { transpiration }
  thresholdVaporPressureDeficit_kPa: single;
  leafResistIfVPDBelowThreshold_mPsec: single;
  fractionOfMaxLeafConductForHighVPD: SingleVarSCurveStructure; {NA}{ calc from leafResistIfVPDBelowThreshold_mPsec }
  canopyResistParam: single;
  { photosynthesis and growth }
  plantingSpringFallOrBoth: smallint{ENUM};
  plantingDaysAfterLastSpringFrost_days: smallint;
  plantingDaysFromSeedToGerminatedSeed_days: smallint;
  plantingDaysFromGerminatedSeedToSeasonMaturity_days: smallint;
  potHeatUnitsReqForMaturation: single;
  biomassToEnergyRatio_kgPhaPMJ: single;
  biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ: single;
  biomassToEnergyRatioCO2Params: SCurveStructure; { calc from biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ }
  biomassToEnergyRatioVPDParam: single;
  biomassAdjustmentIfLAIDecliningFactor: single;
  { leaf area index }
  heatUnitFactorParamsForLAI: SCurveStructure;
  maxLeafAreaIndex: single;
  fractionOfGrowingSeasonWhenLeafDeclineStarts_frn: single;
  leafAreaIndexDeclineRateFactor: single;
  { stress tolerance }
  frostReductionFactorParams: SCurveStructure;
  optimalTemp_degC: single;
  baseTemp_degC: single;
  absoluteTempForFrostKill_degC: single;
  hoursAboveMinDayLengthWhenWinterGrowthCanOccur_hr: single;
  aluminumTolerance_pct: single;
  criticalAerationFactor_frn: single;
  rootGrowthRestrictionByAerationStress: SCurveStructure;
  rootGrowthRestrictionByRockCoeffs: SCurveStructure;
  rootGrowthRestrictionInSandySoilParam: single; {NA} {too sensitive to allow user to change}
  { size }
  maxHeightUnsupported_m: single;
  maxHeightSupported_m: single;
  maxDiameterUnsupported_m: single;
  maxDiameterSupported_m: single;
  maxRootDepth_m: single;
  maxRootSystemDiameter_m: single;
  { erosion }
  minCropManagementFactor: single;
  windErosionFactorStandingLive: single;
  windErosionFactorStandingDead: single;
  windErosionFactorFlatResidue: single;
  { roots and storage organ }
  fractionRootWtAtEmergence_frn: single;
  fractionRootWtAtMaturity_frn: single;
  fractionStorageOrganAllocationAtMaturity_frn: single;
  heatUnitIndexAtStartOfStorageOrganAllocation: single;
  { nutrients }
  plantNAndPStressCoeffs: SCurveStructure;
  optimalNParams: SCurveStructure; {NA}{calc from fractions}
  nFractionAtEmergence_frn: single;
  nFractionAtHalfMaturity_frn: single;
  nFractionAtMaturity_frn: single;
  nFixationParam_frn: single;
  maxAnnualNFert_kgPha: single;
  optimalPParams: SCurveStructure; {NA}{calc from fractions}
  pUptakeCoeffs: SCurveStructure;
  pFractionAtEmergence_frn: single;
  pFractionAtHalfMaturity_frn: single;
  pFractionAtMaturity_frn: single;
  { reproduction }
  heatUnitIndexAtEndOfVegetativePhase: single;
  fractionReproductiveAllocationAtMaturity_frn: single;
  reproductiveBiomassDecayAtPlantMaturity: single;
  minTempForFloralInduction_degC: single;
  optTempForFloralInduction_degC: single;
  maxTempForFloralInduction_degC: single;
  photothermalUnitsRequiredForFloralInduction: single;
  floralInductionParams: SCurveStructure;
  thermalUnitsRequiredForFloralInitiation: single;
  vernalizationRequirement: smallint{ENUM};
  minTempForVernalization_degC: single;
  optTempForVernalization_degC: single;
  maxTempForVernalization_degC: single;
  thermalUnitsRequiredForVernalization: single;
  { more planting stuff added later }
  daysToGrowOptimallyAfterPlanting_days: smallint;
  maturityDaysBeforeFirstFallFrost_days: smallint;
  minPossibleHeatUnitsBeforeGerminationInAnyClimate_degC: single;
  maxPossibleHeatUnitsBeforeGerminationInAnyClimate_degC: single;
  minPossibleHeatUnitsBeforeMaturityInAnyClimate_degC: single;
  maxPossibleHeatUnitsBeforeMaturityInAnyClimate_degC: single;
  reservedArray: array[1..20] of byte; {was 40, added 2 smallints=4 + 4 singles=16 =20, now is 20}
  end;

{ access->development }
DevelopmentStructure = record
  age: smallint;
  yearsOld: smallint;
  daysGerminating: smallint;
  dayPlanted: smallint;
  isDead: boolean;
  isDormant: boolean;
  isSupported: boolean;
  lifeHistoryStage: smallint{ENUM};
  cumHeatUnits: single;
  heatUnitIndex: single;
  heatUnitFactorForLAIAndHeight: single;
  yesterdaysHeatUnitFactorForLAIAndHeight: single;
  leafAreaIndex: single;
  leafAreaIndexAtStartOfDecline: single;
  cumFloralInductionUnits: single;
  cumVernalizationUnits: single;
  cumFloralInitiationUnits: single;
  heatUnitIndexWhenReproductiveAllocationStarted: single;
  photothermalUnitsRequiredForFloralInductionAfterVernalization: single; {NA}{added at last minute}
  reservedArray: array[1..36] of byte;{was 40, added 1 single=4, now is 36}
  end;

{ access->water }
PlantWaterStructure = record
  potPlantEvap_mm: single;
  cumPotPlantEvap_mm: single;
  waterDemandByLayer_mm: arraySoilLayers;
  totalWaterDemand_mm: single; {NA}{can use layer totaling to get}
  waterUseByLayer_mm: arraySoilLayers;
  totalWaterUse_mm: single;  {NA}{can use layer totaling to get}
  cumWaterUse_mm: single;
  reservedArray: array[1..40] of byte;
  end;

{ access->nutrients }
NutrientsStructure = record
  { n }
  optimalNConc_kgPkg: single;
  nDemandForOptimalGrowth_kgPha: single;
  nDemandByLayer_kgPha: arraySoilLayers;
  nDemand_kgPha: single;  {NA}{can use layer totaling to get}
  nSupplyByLayer_kgPha: arraySoilLayers;
  nSupply_kgPha: single;  {NA}{can use layer totaling to get}
  nUptakeByLayer_kgPha: arraySoilLayers;
  nUptake_kgPha: single;  {NA}{can use layer totaling to get}
  nFixation_kgPha: single;           
  nonFixationNUptakeOverDemand_frn: single;
  nInLiveBiomass_kgPha: single;
  { p }
  optimalPConc_kgPkg: single;
  pDemandForOptimalGrowth_kgPha: single;
  pDemandByLayer_kgPha: arraySoilLayers;
  pDemand_kgPha: single; {NA}{can use layer totaling to get}
  pSupplyByLayer_kgPha: arraySoilLayers;
  pSupply_kgPha: single;  {NA}{can use layer totaling to get}
  pUptakeByLayer_kgPha: arraySoilLayers;
  pUptake_kgPha: single;  {NA}{can use layer totaling to get}
  pUptakeOverDemand: single;
  pInLiveBiomass_kgPha: single;
  { moved n and p in standing dead from biomass }
  nInStandingDead_kgPha: single;
  pInStandingDead_kgPha: single;
  reservedArray: array[1..32] of byte;  {was 40, added 2 singles=8, now is 32 }
  end;

{ access->biomass }
BiomassStructure = record
  { cumulative }
  totalLive_tPha: single;          
  rootWeightByLayer_tPha: arraySoilLayers;
  totalRootWeight_tPha: single;  {NA}{can use layer totaling to get}
  totalShootWeight_tPha: single;
  flowersAndFruits_tPha: single;              
  storageOrgan_tPha: single;                  
  standingLive_tPha: single; 
  standingDead_tPha: single;
  unused1: single;  {NA} { was n in standing dead - moved to nutrients }
  unused2: single;  {NA} { was p in standing dead - moved to nutrients }
  { size }
  height_m: single;
  diameter_m: single;
  rootDepth_m: single;
  rootSystemDiameter_m: single; 
  numLayersWithRootsInThem: smallint;
  { daily change }
  photoActiveRadiation_MJPm2: single;
  potentialIncrease_tPha: single;
  actualIncrease_tPha: single;
  reproductiveAllocation_tPha: single;
  storageOrganAllocation_tPha: single;
  shootAllocation_tPha: single;
  rootAllocationByLayer_tPha: arraySoilLayers; 
  reductionFromFrostAndDaylength_tPha: single;
  reductionFromFallingLeaves_tPha: single;
  reductionFromDryness_tPha: single;
  reductionFromReproductiveDecay_tPha: single;
  reductionFromAboveGroundVegetativeDeath_tPha: single;  {NA} {only used when plant dies}
  reductionFromReproductiveDeath_tPha: single;  {NA} {only used when plant dies}
  reductionInStandingDeadFromTransferToResidue_tPha: single;
  mobilizationOutOfShoots_tPha: single;
  mobilizationOutOfRoots_tPha: single;
  mobilizationOutOfStorage_tPha: single;
  reservedArray: array[1..16] of byte;  {was 40, added 6 singles=24, now is 16}
  end;

{ access->constraints }
ConstraintsStructure = record
  biomassGrowthConstraint_frn: single;
  nStressFactor_frn: single;
  pStressFactor_frn: single;
  waterStressFactor_frn: single;
  biomassTempStressFactor_frn: single;
  aerationStressFactor_frn: single;
  rootGrowthConstraintByLayer_frn: arraySoilLayers;
  rootTempStressFactorByLayer_frn: arraySoilLayers;
  rootAlStressFactorByLayer_frn: arraySoilLayers;
  rootSoilStrengthFactorByLayer_frn: arraySoilLayers;
  leafLossIndex_frn: single;
  biomassAdjustmentIfLAIDeclining: single;
  winterBiomassFrostReductionFactor: single;
  winterBiomassDayLengthReductionFactor: single;
  winterBiomassReductionFactor: single;
  snowCoverFactorForFrostKill: single;
  reservedArray: array[1..40] of byte;
  end;

{ ------------------------------------------------------------------------------------  Soil  }
{ object->soil }
{ access->params }
SoilParamsStructure = record
  { site and drawing vars }
  area_ha: single;
  drawingScale: single;  {NA} {edited in change info window}
  shadeIndex_pct: single;  {NA} {edited in change info window}
  viewingAngle_deg: single;  {NA} {edited in change info window}
  baseDrawingColor: TColorRef;
  mulchDrawingColor: TColorRef; 
  unused1: single;  {NA} {meant to get rid of these, forgot, oh well}
  unused2: single;   {NA} {meant to get rid of these, forgot, oh well}
  carbonDioxideInAtmosphere_ppm: single;
  { watershed }
  watershedArea_ha: single;
  watershedChannelLength_km: single;
  watershedChannelSlope_mPm: single;
  watershedChannelDepth_m: single;
  watershedSlopeLength_m: single;
  watershedSlopeSteepness_mPm: single;
  watershedFieldLength_km: single;
  watershedFieldWidth_km: single;
  watershedFieldLengthOrientationFromNorth_rad: single;
  { water table }
  waterTableMinDepth_m: single;
  waterTableMaxDepth_m: single;
  { runoff }
  peakRateEstimationMethod: smallint{ENUM};
  curveNumberCoeffs: SCurveStructure; {NA} {calculated daily}
  useStochasticCurveNumberEstimation: boolean;
  peakRunoffRainfallEnergyAdjFactor: single;
  manningsChannelRoughnessCoeff: single;
  manningsSurfaceRoughnessCoeff: single;
  maxFractionOfRainfallInTimeOfConcParam: single;
  fractDikeVolAvailForWaterStorage_frn: single;
  { water erosion }
  waterErosionMethod: smallint{ENUM};
  userCoeffsForMUSI: arrayFour;
  erosionControlPracticeFactor: single;
  { wind erosion }
  soilParticleDiameter_microns: single;
  maxWindErosionPerDay_tPha: single;
  windErosionAccumCoeff: single;
  windErosionAccelerationFactor: single;
  { percolation and lateral flow }
  paramForReturnFlowTravelTime_days: single;
  timeForDrainageSystemToReducePlantStress_days: single;
  { evaporation }
  potEvapMethod: smallint{ENUM};
  soilEvapByDepthCoeffs: SCurveStructure;
  lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single;
  { nutrients }
  avgNConcInRainfall_gPm3: single;
  nitrateConcInIrrigationWater_gPm3: single;
  nitrogenLeachingParam: single;
  nVolatilizationByDepthCoeffs: SCurveStructure;
  { soil structure }
  soilWeatheringType: smallint{ENUM};
  soilSettlingFromRainfallCoeffs: SCurveStructure;
  { soil temperature }
  soilInsulationFromAirTempCoeffs: SCurveStructure;
  soilAlbedo_frn: single;
  { auto operations - irrigation }
  waterStressFactorForAutoIrr: single;
  maxAnnualIrrVolumeAllowed_mm: single;
  minApplicVolumeAutoIrr_mm: single;
  maxApplicVolumeAutoIrr_mm: single;
  ignoreSoilWaterContentWhenAutoIrrigating: boolean;
  autoIrrigationIsByFurrow: boolean;
  minIntervalAutoIrr_days: smallint;
  fractionOfIrrigationWaterLostToRunoff: single;
  { auto operations - fertilization }
  nStressFactorToTriggerAutoFert: single;
  minIntervalAutoFert_days: smallint;
  ignoreNContentWhenAutoFertilizing: boolean;
  maxFractionMaxNFertInOneApplic_frn: single;
  { auto operations - drainage system }
  layerWithDrainageSystem: smallint;
  { auto operations - harvest }
  minIntervalAutoHarvest_days: smallint;
  avgCurveNumberUnadjustedForSlope: single;
  reservedArray: array[1..36] of byte; { was 40, added 1 single=4, is 36 }
  end;

{ access->state }
StateStructure = record
  numLayers: smallint;
  soilProfileDepth_m: single;
  plowDepth_m: single;
  maxTillageDepth_m: single;
  daysSinceAutoFertApplied: smallint;
  daysSinceAutoIrrApplied: smallint;
  reservedArray: array[1..40] of byte;
  end;

{ access->surface }
SurfaceStructure = record
  temp_degC: single;
  albedo_frn: single;
  soilCoverIndex_frn: single;
  { ridges }
  ridgeHeight_mm: single;
  ridgeHeightLastOperation_mm: single; {NA}{comes from tool use}
  ridgeInterval_m: single;
  ridgeIntervalLastOperation_m: single; {NA}{comes from tool use}
  { ridgesDikes }
  dikeHeight_mm: single;
  dikeHeightAtCreation_mm: single;
  dikeInterval_m: single;
  dikeVolume_mm: single;
  dikesNeedRebuilt: boolean;
  randomRoughness_mm: single;
  tillageDepthLastOperation_m: single;  {NA}{comes from tool use}
  reservedArray: array[1..40] of byte;
  end;

{ access->mulch }
MulchStructure = record
  depth_m: single;  { derive from flatCropResidue_tPha and constant }
  flatCropResidue_tPha: single;         
  organicNFresh_kgPha: single;          
  organicPFresh_kgPha: single;           
  reservedArray: array[1..40] of byte;
  end;

{ access->water }
WaterStructure = record
  { snow }
  snowWaterContent_mm: single;
  snowPackTemp_degC: single;
  { water table }
  waterTableDepth_m: single;
  inflowFromWaterTable_mm: single;
  { pooled water }
  pooledWater_mm: single; {NA}{ not using yet }
  { daily water input }
  patchRainfallForDay_mm: single;      
  snowmeltForDay_mm: single;
  snowfallForDay_mm: single;          
  autoIrrigationForDay_mm: single;
  patchIrrigationForDay_mm: single;
  userIrrigationForDay_mm: single; {NA}{comes from tool use}
  peakRainfallRate_mmPhr: single;
  rainfallDuration_hr: single;
  { runoff }
  avgCurveNumber: single;
  curveNumber: single;
  retentionParam_mm: single;
  runoffVolume_mm: single;
  irrigationRunoffVolume_mm: single;
  peakRunoffRate_mmPhr: single;
  irrigationPeakRunoffRate_m3Psec: single;
  returnFlowTravelTime_days: single;
  timeOfConc_hr: single;
  unused1: single;  {NA}
  unused2: single;  {NA}
  unused3: single;  {NA}
  { evaporation }
  lowestLayerInWhichSoilEvapOccured: smallint;
  reservedArray: array[1..40] of byte;
  end;

{ access->erosion }
ErosionStructure = record
  totalErosion_tPha: single;
  { water }
  waterErosion_tPha: single;
  rainfallEnergyFactorForUSLE: single;
  irrigationWaterErosion_tPha: single;
  thicknessWaitingToBeEroded_m: single;
  enrichmentRatioForNPPest: single;
  { wind }
  windErosion_tPha: single;
  windErosionAccumulatedFactor: single;
  reservedArray: array[1..40] of byte;
  end;

{ access->layers[index] }
LayerStructure = record
  { basics }
  depth_m: single;
  temperature_degC: single;
  weight_tPha: single; 
  { water }
  waterContent_mm: single;      
  fieldCapacity_mm: single;     
  wiltingPoint_mm: single;
  porosity_mm: single;
  lateralFlow_mm: single;
  percolation_mm: single;
  evaporation_mm: single;
  travelTimeFactor: single;
  { n }
  nitrate_kgPha: single;    
  ammonia_kgPha: single;        
  organicNFresh_kgPha: single;    
  organicNActiveHumus_kgPha: single;    
  organicNStableHumus_kgPha: single;      
  organicNActiveHumusFractionAtInput_frn: single; {NA} {user does not need to see}
  { n cycling }
  decayRateConst: single;
  nutrientCyclingTempFactor: single;
  organicNFromActiveToStableInHumus_kgPha: single;
  nitrateLeachedFromPercolation_kgPha: single;
  nitrateLeachedFromLateralFlow_kgPha: single;
  nitrification_kgPha: single;
  volatilization_kgPha: single;
  activeHumusNMineralization_kgPha: single;
  freshNMineralization_kgPha: single;
  denitrification_kgPha: single;
  { p }
  labileP_kgPha: single;    
  organicPFresh_kgPha: single;  
  organicPHumus_kgPha: single;   
  mineralPActive_kgPha: single;   
  mineralPStable_kgPha: single;
  pSorptionCoeff_frn: single;
  { p cycling }
  mineralPFlowCoeff_Pday: single;
  pFlowFromLabileToActiveMineral_kgPha: single;
  mineralPFlowFromActiveToStable_kgPha: single;
  labilePLeachedFromPercolation_kgPha: single;  
  activeHumusPMineralization_kgPha: single;     
  freshPMineralization_kgPha: single;            
  { pH }
  soilpH: single;
  aluminumSaturation_pct: single;
  baseFormingCations_cmolPkg: single;
  { soil structure }
  bulkDensity_tPm3: single;
  settledBulkDensity_tPm3: single;
  bulkDensityOvenDry_tPm3: single;
  cationExchangeCapacity_cmolPkg: single;
  saturatedConductivity_mmPhr: single;
  { other materials }
  flatCropResidue_tPha: single;       
  organicMatter_tPha: single;         
  calciumCarbonate_pct: single;
  clayContent_pct: single;  
  sandContent_pct: single;  
  siltContent_pct: single;  
  rockContent_pct: single;   
  patchTotalRootWeight_tPha: single; {NA}{can use layer totaling to get}
  reservedArray: array[1..40] of byte; 
  end;
LayerStructureArray = array[0..9] of LayerStructure;

{ access->movement }
MovementStructure = record
  nInPatchRainfallForDay_kgPha: single;  
  nInIrrigationForDay_kgPha: single;     
  nitrateLostInRunoff_kgPha: single;
  nitrateMovedToFirstLayerFromEvaporation_kgPha: single;
  organicNActiveHumusAdsorbedToSediment_kgPha: single;   
  organicNStableHumusAdsorbedToSediment_kgPha: single;   
  nitrateAdsorbedToSediment_kgPha: single;   
  ammoniaAdsorbedToSediment_kgPha: single;    
  organicNFreshAdsorbedToSediment_kgPha: single;    
  labilePLostInRunoff_kgPha: single;
  organicPHumusAdsorbedToSediment_kgPha: single;         
  labilePAdsorbedToSediment_kgPha: single;
  mineralPActiveAdsorbedToSediment_kgPha: single;
  mineralPStableAdsorbedToSediment_kgPha: single; 
  organicPFreshAdsorbedToSediment_kgPha: single;  
  organicMatterAdsorbedToSediment_kgPha: single;
  flatCropResidueAdsorbedToSediment_kgPha: single;    
  soilSurfaceTempBare_degC: single;
  soilSurfaceCoverLagFactor_frn: single;
  soilSurfaceTempWithCover_degC: single;
  organicNFromActiveToStableInHumus_kgPha: single;  {NA}{can use layer totaling to get}
  pFlowFromLabileToActiveMineral_kgPha: single;     {NA}{can use layer totaling to get}
  mineralPFlowFromActiveToStable_kgPha: single;     {NA}{can use layer totaling to get}
  nitrateLeachedFromPercolation_kgPha: single;      {NA}{can use layer totaling to get}
  nitrateLeachedFromLateralFlow_kgPha: single;      {NA}{can use layer totaling to get}
  labilePLeachedFromPercolation_kgPha: single;      {NA}{can use layer totaling to get}
  nitrification_kgPha: single;                      {NA}{can use layer totaling to get}
  volatilization_kgPha: single;                     {NA}{can use layer totaling to get}
  activeHumusNMineralization_kgPha: single;         {NA}{can use layer totaling to get}
  activeHumusPMineralization_kgPha: single;         {NA}{can use layer totaling to get}
  freshNMineralization_kgPha: single;               {NA}{can use layer totaling to get}
  freshPMineralization_kgPha: single;               {NA}{can use layer totaling to get}
  organicMatterChangeFromMineralization_tPha: single;
  nImmobilization_kgPha: single;                    {NA}{can use layer totaling to get}
  pImmobilization_kgPha: single;                    {NA}{can use layer totaling to get}
  denitrification_kgPha: single;                    {NA}{can use layer totaling to get}
  changeInPatchTotalNitrate_kgPha: single;
  nFertAutoApplied_kgPha: single;
  pFertAutoApplied_kgPha: single;
  totalNUptakeByPlants_kgPha: single;
  totalPUptakeByPlants_kgPha: single;
  flatCropResidueFromOM_tPha: single;  
  flatCropResidueAddedFromDeadPlant_tPha: single;
  organicNFreshFromOM_kgPha: single;   
  organicNFreshAddedFromDeadPlant_kgPha: single;
  organicPFreshFromOM_kgPha: single;
  organicPFreshAddedFromDeadPlant_kgPha: single;
  flatCropResidueDecomposedToAtmosphere_tPha: single; { not using yet }
  flatCropResidueDecomposedToOM_tPha: single; { not using yet }
  reservedArray: array[1..40] of byte;
  end;

{ access->totals }
TotalsStructure = record      { all quantities - ones in SoilPatchTotals done - not as important }
  { whichever of these don't say "at input" are calculated daily }
  { soil water stuff }
  patchTotalSoilWaterContent_mm: single; {NA}{can use layer totaling to get}
  top1MWaterContent_mm: single;
  plowDepthSWmWP_mm: single;
  soilProfileDepthSWmWP_mm: single;
  patchTotalFieldCapacity_mm: single;  {NA}{can use layer totaling to get}
  top30cmFractionFieldCapacity_frn: single;
  soilProfileDepthFCmWP_mm: single;
  top1mPorosity_mm: single;
  { evaporation }
  patchTotalPotSoilEvap_mm: single; {NA}{can use layer totaling to get}
  patchTotalSoilEvap_mm: single;    {NA}{can use layer totaling to get}
  patchTotalPotPlantEvap_mm: single; {NA}{can use layer totaling to get}{ not calc in EP_PatchTotals }
  patchTotalSoilEvapAndPlantWaterUse_mm: single;  {NA}{can use layer totaling to get}{ not calc in EP_PatchTotals }
  { 30-day running totals of rainfall minus runoff and potential soil evap }
  { rainfall minus runoff used in pest factor; both used in water table depth }
  rainfallMinusRunoff30DaysArray_mm: arrayThirty; {NA}{can use layer totaling to get}{ not calc in EP_PatchTotals }
  sumRainfallMinusRunoffPrev30Days_mm: single; {NA}{can use layer totaling to get}{ not calc in EP_PatchTotals }
  potSoilEvap30DaysArray_mm: arrayThirty; {NA}{can use layer totaling to get}{ not calc in EP_PatchTotals }
  sumPotSoilEvapPrev30Days_mm: single; {NA}{can use layer totaling to get}{ not calc in EP_PatchTotals }
  patchMeanBulkDensity_tPm3: single;  {NA}{can use layer totaling to get}
  plowDepthSettledBulkDensityAtInput_tPm3: single;  {NA}{can use layer totaling to get}
  unused1: single; {NA} { was soilProfileDepthSettledBulkDensity_tPm3, not using anymore }
  { n }
  patchTotalNitrate_kgPha: single;      {NA}{can use layer totaling to get}
  soilProfileDepthNitrate_kgPha: single; {NA}{can use layer totaling to get}
  patchTotalAmmonia_kgPha: single;        {NA}{can use layer totaling to get}
  patchTotalOrganicNFresh_kgPha: single;    {NA}{can use layer totaling to get}
  patchTotalOrganicNStableHumus_kgPha: single; {NA}{can use layer totaling to get}
  patchTotalOrganicNActiveHumus_kgPha: single;  {NA}{can use layer totaling to get}
  { p }
  patchTotalLabileP_kgPha: single;       {NA}{can use layer totaling to get}
  plowDepthLabilePAtInput_kgPha: single;  {NA}{can use layer totaling to get}
  patchTotalOrganicPFresh_kgPha: single;   {NA}{can use layer totaling to get}
  patchTotalOrganicPHumus_kgPha: single;   {NA}{can use layer totaling to get}
  patchTotalMineralPActive_kgPha: single;   {NA}{can use layer totaling to get}
  patchTotalMineralPStable_kgPha: single;   {NA}{can use layer totaling to get}
  { other materials }
  patchTotalFlatResidue_tPha: single;      {NA}{can use layer totaling to get}
  patchTotalOrganicMatter_tPha: single;    {NA}{can use layer totaling to get}
  patchTotalAboveGroundBiomassAndResidue_tPha: single; { not calc in EP_PatchTotals }
  { other things for reporting only }
  patchWeightedMeanpH: single;      {NA}{can use layer totaling to get}
  { these three are calc only at input and after liming (manual or auto) }
  patchWeightedMeanBaseFormingCations_cmolPkg: single;  {NA}{internal use}
  patchWeightedMeanOrganicC_pct: single;                 {NA}{internal use}
  patchWeightedMeanCationExchangeCapacity_cmolPkg: single; {NA}{internal use}
  reservedArray: array[1..40] of byte;
  end;

{ access->yearTotals }
YearTotalsStructure = record
  cumNFixation_kgPha: single;     
  cumNFertAutoApplied_kgPha: single;  
  cumNAddedInIrrigationWater_kgPha: single;    
  cumPFertAutoApplied_kgPha: single;          
  cumIrrigationApplied_mm: single;            
  reservedArray: array[1..40] of byte;
  end;

{ access->plantMeans }
PlantMeansStructure = record
  { all revised daily }
  meanLeafAreaIndex: single;
  meanHeight_m: single;
  totalStandingLive_tPha: single;
  totalStandingDead_tPha: single;
  totalNInStandingDead_kgPha: single;    
  totalPInStandingDead_kgPha: single;
  totalWaterUse_mm: single;
  unused1: single;  {NA} {now calculate as needed}
  unused2: single;  {NA} {was for common transpiration, now do per plant}
  meanWindErosionFactorStandingLive: single;
  meanWindErosionFactorStandingDead: single;
  meanWindErosionFactorFlatResidue: single;
  meanMinCropManagementFactor: single;
  unused3: single; {NA} {was for common transpiration, now do per plant}
  unused4: single; {NA} {was for common transpiration, now do per plant}
  minimumMaxAnnualNFert_kgPha: single;
  reservedArray: array[1..40] of byte;
  end;

{ ------------------------------------------------------------------------------------  Weather  }
type
{ object->weather }
{ access->params }
WeatherParamsStructure = record
  climateLatitude_rad: single;
  climateElevation_m: single;
  yearsMaxMonthlyHalfHourRainfallRecord_yr: single;
  coeffForWetDryProbsGivenNumWetDays_frn: single;
  coeffRainfallModExpDist: single;
  paramForModifiedExpWindSpeed: single;
  dailyMeanMaxTempForMonth_degC: arrayMonths;
  dailyMeanMinTempForMonth_degC: arrayMonths;
  stdDevMaxTempForMonth_degC: arrayMonths;
  stdDevMinTempForMonth_degC: arrayMonths;
  meanTotalRainfallForMonth_mm: arrayMonths;
  stdDevDailyRainfallForMonth_mm: arrayMonths;
  skewCoeffForRainfallForMonth: arrayMonths;
  probWetDayAfterDryDayForMonth_frn: arrayMonths;
  probWetDayAfterWetDayForMonth_frn: arrayMonths;
  numWetDaysForMonth: arrayMonths;
  meanPropRainInFirstHalfHourForMonth_frn: arrayMonths;
  dailyMeanRadiationForMonth_MJPm2: arrayMonths;
  dailyMeanRelHumForMonth_frn: arrayMonths;
  dailyMeanWindSpeedForMonth_mPsec: arrayMonths;
  windDirectionsForMonth_frn: windDirectionsArray;
  reservedArray: array[1..40] of byte;
  end;

{ access->stationVars }
WeatherStationVarsStructure = record
  probWetDayForMonth_frn: arrayMonths;
  dailyMeanRainfallForMonth_mm: arrayMonths;
  unused1: single;
  maxPossibleRadiationForYear_MJPm2: single;
  rainfallNormalizingFactorForMonth: arrayMonths;    { from iteration at beginning of simulation }
  unused2: arrayMonths;
  barometricPressure_kPa: single;
  psychrometerConstant_kPaPdegC: single;
  minDayLengthForYear_hr: single; 
  unused3: single;
  julianDayOfLastSpringFrost: smallint;   
  julianDayOfFirstFallFrost: smallint;
  reservedArray: array[1..36] of byte; { was 40, added 2 smallints=4, now is 36 }
  end;

{ access->longTermFactors }
LongTermFactorsStructure = record
  rainfallTotalForMonth_mm: arrayMonths;
  rainfallAdjFactorForMonth: arrayMonths;
  maxTempTotalForMonth_degC: arrayMonths;
  maxTempAdjFactorForMonth: arrayMonths;
  minTempTotalForMonth_degC: arrayMonths;
  minTempAdjFactorForMonth: arrayMonths;
  yearsOfLongTermWeatherDataForAdjustment: smallint;
  daysOfLongTermWeatherDataForAdjustment: smallint;
  reservedArray: array[1..36] of byte; { was 40, added 2 smallints=4, now is 36 }
  end;

{ access->dailyFromMonthlyMeans }
DailyFromMonthlyMeansStructure = record
  maxTemp_degC: single;
  minTemp_degC: single;
  relHum_frn: single;
  radiation_MJPm2: single;
  probWetDay_frn: single;
  windSpeed_mPsec: single;
  reservedArray: array[1..40] of byte;
  end;

{ access->dailyMeans }
DailyMeansStructure = record
  maxTemp_degC: single;
  radiation_MJPm2: single;
  relHum_frn: single;
  reservedArray: array[1..40] of byte;
  end;

{ access->dailyWeather }
DailyWeatherStructure = record
  rainfallForDay_mm: single;
  radiationForDay_MJPm2: single;
  minTempForDay_degC: single;
  maxTempForDay_degC: single;
  meanTempForDay_degC: single;
  relHumForDay_frn: single;
  meanWindSpeedForDay_mPsec: single;
  windDirectionForDay_rad: single;
  dayLength_hr: single;
  itRainedYesterday: boolean;
  latentHeatOfVaporization_MJPkg: single;
  vaporPressureDeficit_kPa: single;
  slopeSaturVaporPressureCurve_kPaPdegC: single;
  declinationAngleOfSun_rad: single;
  maxPossibleRadiation_MJPm2: single;
  netOutgoingRadiationIfDayIsClear_MJPm2: single;
  reservedArray: array[1..40] of byte;
  end;

{ access->matrices }
MatricesStructure = record
  epsilonRandomNumberMatrix: arrayThree;
  matrixAResidual: arrayThree;
  matrixBResidual: arrayThree;
  correlationMultiplier: arrayThree;
  yesterdaysCorrelationMultiplier: arrayThree;
  yesterdaysUniformRandNumForRainfall_frn: single;
  lastEpsilonRandomNumber: single;
  reservedArray: array[1..40] of byte;
  end;
{ aspects->stop }

{ ------------------------------------------------------------------------------------ plant contants }
const
  kPlantHasNotDied = 0; kPlantHasDied = 1;
  kPlantHasNotGerminated = 0; kPlantHasGerminated = 1;
  kBecomeSeedAndStartGrowingNow = true; kBecomeSeedButStartGrowingLater = false;
  kStreamParams = true; kDontStreamParams = false;
  kNoVernalization = 0; kObligateVernalization = 1; kQuantitativeVernalization = 2;
  kAnnual = 0; kBiennial = 1; kPerennial = 2;
  kTerminalFemaleInflorescences = 0; kAxillaryFemaleInflorescences = 1;
  kTerminalMaleInflorescences = 0; kAxillaryMaleInflorescences = 1;
  kSeed = 0; kVegetativePeriod = 1; kFloralInductionPeriod = 2; kFloralInitiationPeriod = 3;
    kReproductiveAllocationPeriod = 4; kFailedVernalization = 5;
  kCultivarIsPlantedInSpring = 0; kCultivarIsPlantedInFall = 1; kCultivarCanBePlantedSpringOrFall = 2;
  kPlantOptionsIgnoreGerminationRequirements = 0;
  kPlantOptionsOptimalTemperature = 1;
  kPlantOptionsOptimalWaterUptake = 2;
  kPlantOptionsOptimalRadiation = 3;
  kPlantOptionsOptimalNitrogenUptake = 4;
  kPlantOptionsOptimalPhosphorusUptake = 5;
  kPlantOptionsOptimalSoilStrengthForRootGrowth = 6;
  kPlantOptionsIgnoreLeafSenescence = 7;
  kPlantOptionsIgnoreAluminumToxicity = 8;
  kPlantOptionsIgnoreAerationStress = 9;
  kPlantOptionsIgnorePhotoperiod = 10;
  kPlantOptionsIgnoreReproductiveDecay = 11;
  kPlantOptionsIgnoreLifeCycleLimits = 12;
  kPlantOptionsLastOption = 12;
type
  plantOptionsArray = array[0..kPlantOptionsLastOption + 2] of boolean; {2 extra for future use}

{ ------------------------------------------------------------------------------------ soil constants }
const
  kFirstLayerDepth_m = 0.01;
  kOrganicMatterBlobHasDecayed = true;
  kSoilMinimumInitialSize = 25;
  kMaxPossibleLayers = 10;
  kConvertArea = 1; kConvertDepth = 2; kConvertConc = 3;
  {weathering} kCalcareousSoilOrWithoutWeatherInfo = 0; kSlightlyWeatheredSoil = 1; kModWeatheredSoil = 2;
    kHighlyWeatheredSoil = 3; kInputPSorptionCoeffs = 4;
  {PET} kPenmanMethod = 0; kPenmanMonteithMethod = 1; kPriestleyTaylorMethod = 2; kHargreavesMethod = 3;
  {peak runoff} kModifiedRationalEquationMethod = 0; kSCSTypeI = 1; kSCSTypeIa = 2; kSCSTypeII = 3; kSCSTypeIII = 4;
  {water erosion} kUSLE = 0; kMUSLE = 1; kOnstadFoster = 2; kMUSS = 3; kMUST = 4; kMUSI = 5;
  { options }
  kSoilOptionsAutoIrrigation = 0;
  kSoilOptionsAutoFertilization = 1;
  kSoilOptionsAutoPHControl = 2;
  kSoilOptionsAutoHarvest = 3;
  kSoilOptionsAutoMaintainFurrowDikes = 4;
  kSoilOptionsAutoMaintainDrainageSystem = 5;
  kSoilOptionsUpdateSoilCoverIndex = 6;
  kSoilOptionsUpdateAlbedo = 7;
  kSoilOptionsUpdateSoilTemperature = 8;
  kSoilOptionsUpdateWaterTableDepth = 9;
  kSoilOptionsAllowWindErosion = 10;
  kSoilOptionsAllowSnowMelt = 11;
  kSoilOptionsAllowPrecipitationToReachPatch = 12;
  kSoilOptionsAllowRunoff = 13;
  kSoilOptionsAllowWaterErosionFromRunoff = 14;
  kSoilOptionsAllowRidgeSettlingFromRainfall = 15;
  kSoilOptionsAllowNitrateToEnterSoilInRainfall = 16;
  kSoilOptionsAllowNitrateToEnterSoilInIrrigationWater = 17;
  kSoilOptionsAllowRunoffFromIrrigation = 18;
  kSoilOptionsAllowWaterErosionFromRunoffFromIrrigation = 19;
  kSoilOptionsAllowPercolation = 20;
  kSoilOptionsAllowLateralFlow = 21;
  kSoilOptionsAllowSoilEvaporation = 22;
  kSoilOptionsAllowPlantTranspiration = 23;
  kSoilOptionsAllowRebuildingDikes = 24;  { not using }
  kSoilOptionsAllowActiveAndStableHumusNFlow = 25;
  kSoilOptionsAllowLabileAndMineralPFlow = 26;
  kSoilOptionsAllowActiveAndStableMineralPFlow = 27;
  kSoilOptionsAllowNitrateLossInRunoff = 28;
  kSoilOptionsAllowLabilePLossInRunoff = 29;
  kSoilOptionsAllowNitrateLossInLateralFlow = 30;
  kSoilOptionsAllowNitrateLossInPercolation = 31;
  kSoilOptionsAllowLabilePLossInPercolation = 32;
  kSoilOptionsAllowNitrification = 33;
  kSoilOptionsAllowVolatilization = 34;
  kSoilOptionsAllowActiveHumusNMineralization = 35;
  kSoilOptionsAllowActiveHumusPMineralization = 36;
  kSoilOptionsAllowFreshNMineralization = 37;
  kSoilOptionsAllowFreshPMineralization = 38;
  kSoilOptionsAllowStandingDeadToDecayToResidue = 39;
  kSoilOptionsAllowDenitrification = 40;
  kSoilOptionsAllowNUptakeByPlants = 41;
  kSoilOptionsAllowPUptakeByPlants = 42;
  kSoilOptionsAllowErodedSoilToTakeOrganicNActiveHumus = 43;
  kSoilOptionsAllowErodedSoilToTakeOrganicNStableHumus = 44;
  kSoilOptionsAllowErodedSoilToTakeNitrate = 45;
  kSoilOptionsAllowErodedSoilToTakeAmmonia = 46;
  kSoilOptionsAllowErodedSoilToTakeOrganicNFresh = 47;
  kSoilOptionsAllowErodedSoilToTakeOrganicPHumus = 48;
  kSoilOptionsAllowErodedSoilToTakeLabileP = 49;
  kSoilOptionsAllowErodedSoilToTakeMineralPActive = 50;
  kSoilOptionsAllowErodedSoilToTakeMineralPStable = 51;
  kSoilOptionsAllowErodedSoilToTakeOrganicPFresh = 52;
  kSoilOptionsAllowErodedSoilToTakeOrganicMatter = 53;
  kSoilOptionsAllowErodedSoilToTakeFlatCropResidue = 54;
  kSoilOptionsAllowSoilSettlingFromRain = 55;
  kSoilOptionsAllowNitrateToMoveToTopSoilLayerByEvaporation = 56;
  kSoilOptionsAllowFlatCropResidueToDecay = 57;
  kSoilOptionsLastOption = 57;

type
soilOptionsArray = array[0..kSoilOptionsLastOption + 3] of boolean; {3 extra for future use}
SoilPatchBasicInfoStructure = record
  area_ha: single;
  slope_mPm: single;
  shade_pct: single;
  viewingAngle_deg: single;
  scale: single;
  orientationFromNorth_deg: single;
  reservedArray: array[1..40] of byte;
  end;

{ ------------------------------------------------------------------------------------ weather constants }
const
  kMaxNumDays = 30;
  kWeatherOptionNormal = 0; kWeatherOptionDisabled = 1; kWeatherOptionSmoothed = 2; kWeatherOptionNoUpdate = 3;
  kWeatherOptionsTemperature = 0;
  kWeatherOptionsRadiation = 1;
  kWeatherOptionsRelHum = 2;
  kWeatherOptionsRainfall = 3;
  kWeatherOptionsWindSpeed = 4;
  kWeatherOptionsWindDirection = 5;
  kWeatherOptionsLastOption = 5;
type
  weatherOptionsArray = array[0..kWeatherOptionsLastOption + 4] of smallint; {4 extra for future use}
  HistoryDailyWeatherStructure = array[0..29] of DailyWeatherStructure;
  HistoryDailyFromMonthlyMeansStructure = array[0..29] of DailyFromMonthlyMeansStructure;
  matrixType = array [0..2,0..2] of single;

implementation

end.

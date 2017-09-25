unit Udefault;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udefault: This file contains functions that set defaults for weather, soil and plant
parameters and variables based on 1) the EPIC code, 2) the UTIL code, and 3) the
EPIC help files (.fmt) for all the parameters. It is called only by uhardcd, which
initializes some default templates (but only if the ALLOW_HARDCODING conditional define is set),
and ugetepic, which imports data from EPIC files. The defaulting here as taken from the EPIC
code may not be all correct, especially some of the more involved soil calculations. This
code was used when the current list of templates was generated. See the help file explanation
of differences between EPIC and GWI for more information on our changes.}

interface

uses uesoil, ueplant, ueweath, uestruct;

type

  SoilTypeCreationDefaultsStructure = record
    initialFractionOfFieldCapacity_frn: single;
    rootGrowthRestrictionInSandySoilParam: single;
    startingSoilTempTarget_degC: single;
    startingSoilTempMean_degC: single;
    cultivationBeforeSimulationStarts_yr: single;
    timeForDrainageSystemToReducePlantStress_days: single;
    initSnowWaterContent_mm: single;
    minThicknessOfThickestSoilLayer_m: single;
    firstLayerThisThickSplitAtInputIfNeeded_m: single;
    end;

SoilTypeLayerInitsStructure = record
  { must keep these because it is how the EPIC data is found }
  initOrganicNTotalHumus_gPt: single;
  initOrganicC_pct: single;
  initNitrate_gPt: single;
  initLabileP_gPt: single;
  initOrganicPHumus_gPt: single;
  initFieldCapacity_mPm: single;
  initWiltingPoint_mPm: single;
  initPorosity_mPm: single;
  end;
SoilTypeLayerInitsStructureArray = array[0..9] of SoilTypeLayerInitsStructure;

{ soil type }
procedure Defaults_DefaultAndCalculateSoilInputs(soilPatch: GsSoilPatch; var layerInits: SoilTypeLayerInitsStructureArray);
procedure Defaults_EnforceSoilInputRanges(soilPatch: GsSoilPatch; var layerInits: SoilTypeLayerInitsStructureArray);
procedure Defaults_ZeroLayerInits(var layerInits: SoilTypeLayerInitsStructureArray);
procedure Defaults_DefaultSoilParamsForVersionOne(soilPatch: GsSoilPatch);
procedure Defaults_SoilPatchTotalsAtInput(soilPatch: GsSoilPatch);
{ climate }
procedure Defaults_DefaultAndCalculateWeatherInputs(weather: GsWeather);
procedure Defaults_EnforceWeatherInputRanges(weather: GsWeather);
function Defaults_IterateForWeatherModExpRainfallNormalizingFactor(weather: GsWeather): single;
function Defaults_IterateForWeatherRainfallNormalizingFactor(weather: GsWeather; month: integer): single;
{ cultivar }
procedure Defaults_DefaultAndCalculatePlantInputs(plant: GsPlant);
procedure Defaults_EnforcePlantInputRanges(plant: GsPlant);
procedure Defaults_DefaultPlantParamsForVersionOne(plant: GsPlant);

implementation

uses usupport, ueutils, uunits, ueq, ueqh, uep, udate;

procedure Defaults_DefaultAndCalculateSoilInputs(soilPatch: GsSoilPatch; var layerInits: SoilTypeLayerInitsStructureArray);
  var
    defaults: SoilTypeCreationDefaultsStructure;
    layer, i: integer;
    baseSaturation_pct, bulkDensityForLowStress_tPm3, bulkDensityForHighStress_tPm3: single;
    soilTextureParam1, soilTextureParam2, soilStrengthFactorForSatCond: single;
    factor1, factor2, organicMatterFactor, threshold: single;
    nonRockContent_frn, depthLastLayer_m, thicknessThisLayer_mm, thicknessLastLayer_mm: single;
    propRemovedFromLayer1_frn, layer2Thickness_m, propLayer2WasAlreadyThere_frn: single;
    soilTempDepthWtFactor, organicMatterFractionForLayer_frn, bulkDensityInOMForLayer_tPm3: single;
    conversionFactor_tkgPgha: single;
    { these following are calculated from initLabileP_gPt or initOrganicNTotalHumus_gPt, }
    { converted to kg/ha, and never used again. }
    initMineralPActive_gPt, initMineralPStable_gPt, initOrganicNActiveHumus_gPt, initOrganicNStableHumus_gPt: single;
  begin
  try
  { set up defaults }
  with defaults do
    begin
    { these defaults based on EPIC data files - probably specifically PAIRR.DAT }
    rootGrowthRestrictionInSandySoilParam := 1.5;
    cultivationBeforeSimulationStarts_yr := 150.0;
    timeForDrainageSystemToReducePlantStress_days := 5.0;
    { EPIC had 25 mm starting snow water content but since these can be created anytime it is better to start with none }
    initSnowWaterContent_mm := 0.0;
    { this default is a reasonable guess }
    initialFractionOfFieldCapacity_frn := 0.8;
    { these defaults are based on guesses for a target temp in April in the Midwest, and a mean temp for the Midwest }
    startingSoilTempTarget_degC := Convert(kTemperature, kTemperatureDegreesF, kTemperatureDegreesC, 50.0);
    startingSoilTempMean_degC := Convert(kTemperature, kTemperatureDegreesF, kTemperatureDegreesC, 35.0);
    minThicknessOfThickestSoilLayer_m := 0.1;
    firstLayerThisThickSplitAtInputIfNeeded_m := 0.15;
    end;
  with soilPatch do
  begin
  baseSaturation_pct := 0.0;
  bulkDensityForLowStress_tPm3 := 0.0;
  bulkDensityForHighStress_tPm3 := 0.0;
  soilTextureParam1 := 0.0;
  soilTextureParam2 := 0.0;
  soilStrengthFactorForSatCond := 0.0;
  factor1 := 0.0;
  factor2 := 0.0;
  organicMatterFactor := 0.0;
  threshold := 0.0;
  nonRockContent_frn := 0.0;
  depthLastLayer_m := 0.0;
  thicknessThisLayer_mm := 0.0;
  thicknessLastLayer_mm := 0.0;
  propRemovedFromLayer1_frn := 0.0;
  layer2Thickness_m := 0.0;
  propLayer2WasAlreadyThere_frn := 0.0;
  soilTempDepthWtFactor := 0.0;
  organicMatterFractionForLayer_frn := 0.0;
  bulkDensityInOMForLayer_tPm3 := 0.0;
  conversionFactor_tkgPgha := 0.0;
  initMineralPActive_gPt := 0.0;
  initMineralPStable_gPt := 0.0;
  initOrganicNActiveHumus_gPt := 0.0;
  initOrganicNStableHumus_gPt := 0.0;
  { set up depths at starting values }
  state.plowDepth_m := 0.2;
  state.soilProfileDepth_m := 2.0;
  state.maxTillageDepth_m := 0.0;
  { defaults added by cfk because these params are not in soil type files }
  Utils_DefaultFloatIfZero(params.avgCurveNumberUnadjustedForSlope, 85.0);
  water.avgCurveNumber := params.avgCurveNumberUnadjustedForSlope;
  Utils_DefaultFloatIfZero(params.watershedChannelLength_km, 1.0);
  Utils_DefaultFloatIfZero(params.watershedChannelSlope_mPm, 0.05);
  Utils_DefaultFloatIfZero(params.watershedSlopeLength_m, 85.0);
  Utils_DefaultFloatIfZero(params.watershedSlopeSteepness_mPm, 0.06);
  Utils_DefaultFloatIfZero(params.erosionControlPracticeFactor, 0.5);
  Utils_DefaultFloatIfZero(params.watershedFieldLengthOrientationFromNorth_rad, 3.141596);
  Utils_DefaultFloatIfZero(params.soilAlbedo_frn, 0.13);
  Utils_DefaultIntIfZero(params.soilWeatheringType, kCalcareousSoilOrWithoutWeatherInfo);
  Utils_DefaultFloatIfZero(surface.ridgeHeight_mm, 0.0);
  Utils_DefaultFloatIfZero(surface.ridgeInterval_m, 0.0);
  Utils_DefaultFloatIfZero(mulch.flatCropResidue_tPha, 5.0);
  Utils_DefaultFloatIfZero(params.watershedArea_ha, 1.0);
  { EPIC defaults }
  Utils_DefaultFloatIfZero(params.manningsChannelRoughnessCoeff, 0.05);
  Utils_DefaultFloatIfZero(params.manningsSurfaceRoughnessCoeff, 0.15);
  Utils_DefaultFloatIfZero(params.watershedFieldLength_km, 0.632);
  Utils_DefaultFloatIfZero(params.watershedFieldWidth_km, 0.316);
  { calculate channel length if zero }
  (* Removing dependence - moved this to when soil patch created to use garden channel geometry coeffs
  if (watershed.channelLength_km = 0.0) then
    begin
    if (defaults.peakRateEstimationMethod <> kModifiedRationalEquationMethod) then
      begin
      { scs tr55 method }
      watershed.channelLength_km := power(defaults.channelGeometryCoeffs[2] * watershed.area_ha,
        defaults.channelGeometryCoeffs[3]);
      end
    else
      begin
      watershed.channelLength_km := 0.1 * sqrt(watershed.fieldLength_km * km_to_m * safediv(watershed.area_ha,
        watershed.fieldWidth_km * km_to_m));
      end;
    end;
    *)
  { if channel slope is zero, calculate it from slope steepness and watershed area }
  Utils_DefaultFloatIfZero(params.watershedChannelSlope_mPm, params.watershedSlopeSteepness_mPm
      * power(1.0 + params.watershedArea_ha, -0.3));
  { if channel depth is zero, calculate it from wathershed area }
  (* Removing dependence - do this when soil patch created
  Utils_DefaultFloatIfZero(watershed.channelDepth_m, defaults.channelGeometryCoeffs[0] *
    power(watershed.area_ha, defaults.channelGeometryCoeffs[0]));
  *)
  { bound slope length }
  params.watershedSlopeLength_m := min(params.watershedSlopeLength_m, sqrt(ha_to_m2 * params.watershedArea_ha));
  { defaults }
  Utils_DefaultFloatIfZero(params.soilParticleDiameter_microns, 500.0);
  { if these are not input, the water table is assumed to be too low to affect the root zone }
  if (params.waterTableMaxDepth_m = 0.0) then
    begin
    params.waterTableMinDepth_m := 50.0;
    params.waterTableMaxDepth_m := 100.0;
    water.waterTableDepth_m := 75.0;
    end;
  { if initial fraction of field capacity not filled in, calculate it from total rainfall }
  { Removing dependence - if initial FFC is zero, use constant fraction instead of considering weather }
  { defaults }
  Utils_DefaultFloatIfZero(params.paramForReturnFlowTravelTime_days, 10.0);
  { layer loop }
  depthLastLayer_m := 0;
  thicknessLastLayer_mm := 0;
  for layer := 0 to kMaxPossibleLayers - 1 do 
    begin
    { note that depth must be filled in for each layer }
    if (layers[layer].depth_m < kLowestFloatAboveZero) then break;
    thicknessThisLayer_mm := (layers[layer].depth_m - depthLastLayer_m) * m_to_mm;
    { calc clay content from sand and silt content }
    layers[layer].clayContent_pct := 100.0 - layers[layer].sandContent_pct - layers[layer].siltContent_pct;
    { estimate saturated conductivity if not entered }
    if (layers[layer].saturatedConductivity_mmPhr < kLowestFloatAboveZero) then
      begin
      { Removing dependence - use average constants for these, not the actual values in the patch }
      bulkDensityForLowStress_tPm3 := EQ.BulkDensityForLowStress_tPm3(layers[layer].sandContent_pct,
        defaults.rootGrowthRestrictionInSandySoilParam);
      bulkDensityForHighStress_tPm3 := EQ.BulkDensityForHighStress_tPm3(layers[layer].sandContent_pct,
        defaults.rootGrowthRestrictionInSandySoilParam);
      soilTextureParam2 := EQ.SoilTextureParam2(bulkDensityForLowStress_tPm3, bulkDensityForHighStress_tPm3);
      soilTextureParam1 := EQ.SoilTextureParam1(bulkDensityForLowStress_tPm3, soilTextureParam2);
      soilStrengthFactorForSatCond := scurve(layers[layer].settledBulkDensity_tPm3, soilTextureParam1,  -
        soilTextureParam2);
      layers[layer].saturatedConductivity_mmPhr := EQH.SaturatedConductivityForLayer_mmPhr(layers[layer].clayContent_pct,
        soilStrengthFactorForSatCond);
      end;
    { estimate amounts of residue, n and p if zero (function returns same amount if not zero) }
    if (layer > 1) then 
      begin
      layers[layer].flatCropResidue_tPha := Utils_EstimateMaterialForLayer(layer, layers[layer].flatCropResidue_tPha,
        layers[layer-1].flatCropResidue_tPha, thicknessThisLayer_mm, thicknessLastLayer_mm);
      layerInits[layer].initLabileP_gPt := Utils_EstimateMaterialForLayer(layer, layerInits[layer].initLabileP_gPt,
        layerInits[layer-1].initLabileP_gPt, thicknessThisLayer_mm, thicknessLastLayer_mm);
      layerInits[layer].initNitrate_gPt := Utils_EstimateMaterialForLayer(layer, layerInits[layer].initNitrate_gPt,
        layerInits[layer-1].initNitrate_gPt, thicknessThisLayer_mm, thicknessLastLayer_mm);
      end;
    { start out soil temp with a damping depth of 2.0 m and using April mean air temp (because it has avg temp) }
    { change from EPIC: EPIC always uses January mean air temp here }
    soilTempDepthWtFactor := EQ.SoilTempDepthWtFactorForLayer(layers[layer].depth_m, depthLastLayer_m, 2.0);
    { Removing dependence - replace this temperature with a reasonable constant - say 40 degrees F - do not consider weather }
    layers[layer].temperature_degC := soilTempDepthWtFactor *
      (defaults.startingSoilTempMean_degC - defaults.startingSoilTempTarget_degC) + defaults.startingSoilTempTarget_degC;
    { estimate active fraction of organic n (save because this is used later) }
    { Removing dependence - make cultivationBeforeSimulationStarts_yr a program-wide constant }
    layers[layer].organicNActiveHumusFractionAtInput_frn := EQ.OrganicNActiveHumusFractionForLayer_frn(layer,
      defaults.cultivationBeforeSimulationStarts_yr);
    { calc organic c by exponential decline starting at layer 3 if not entered }
    { the assumption is that the value for layer 1 is not zero. }
    if ((layer > 1) and (layerInits[layer].initOrganicC_pct = 0.0)) then
      begin
      if (layer > 2) then 
        layerInits[layer].initOrganicC_pct := layerInits[layer-1].initOrganicC_pct
          * safeExp(-0.001 * thicknessThisLayer_mm)
      else
        layerInits[layer].initOrganicC_pct := layerInits[0].initOrganicC_pct;
      end;
    { calc organic matter fraction for layer from organic c }
    { a ratio  of about 1:1.7 can be assumed to exist between the organic carbon and the soil humus }
    organicMatterFractionForLayer_frn := layerInits[layer].initOrganicC_pct * pct_to_frn * 1.72;
    { adjust settled bulk density for amount of organic matter }
    { (1.0 - om) / (1.0/sbd - om/0.224) }
    bulkDensityInOMForLayer_tPm3 := safediv(1.0 - organicMatterFractionForLayer_frn, safediv(1.0,
      layers[layer].settledBulkDensity_tPm3) - organicMatterFractionForLayer_frn / 0.224);
    if ((bulkDensityInOMForLayer_tPm3 >= 2.5) or (bulkDensityInOMForLayer_tPm3 <=
      layers[layer].settledBulkDensity_tPm3)) then
      begin
      { 1.0 / (om/0.224 + (1.0 - om)/2.0) }
      layers[layer].settledBulkDensity_tPm3 := safediv(1.0, organicMatterFractionForLayer_frn / 0.224 + (1.0 -
        organicMatterFractionForLayer_frn) / 2.0);
      end;
    { calc soil weight from entered settled bulk density - same as normal bd at this point }
    layers[layer].weight_tPha := Utils_SoilWeightFromBulkDensityAndThickness_tPha(
        layers[layer].settledBulkDensity_tPm3, thicknessThisLayer_mm * mm_to_m);
    { estimate porosity (2.65 is assumed for particle density) - sbd is same as bd at this point }
    layerInits[layer].initPorosity_mPm := 1.0 - layers[layer].settledBulkDensity_tPm3 / 2.65;
    { calculate wilting point and field capacity if wilting point not entered }
    { unresolved: unclear }
    { I don't understand this section very well but I think it is translated correctly. }
    if (layerInits[layer].initWiltingPoint_mPm = 0.0) then
      begin
      if (layers[layer].sandContent_pct > 75.0) then 
        begin
        factor1 := 0.19 - 0.0017 * layers[layer].sandContent_pct;
        factor2 := 0.429 - 0.00388 * layers[layer].sandContent_pct;
        end
      else 
        begin
        if (layers[layer].siltContent_pct < 70.0) then 
          begin
          factor1 := 0.0542 + 0.00409 * layers[layer].clayContent_pct;
          factor2 := 0.1079 + 0.0005004 * layers[layer].siltContent_pct;
          end
        else 
          begin
          factor1 := 0.16;
          factor2 := 0.1079 + 0.0005004 * layers[layer].siltContent_pct;
          end;
        end;
      factor1 := factor1 + (0.23 * organicMatterFractionForLayer_frn);
      factor2 := factor2 + (0.55 * organicMatterFractionForLayer_frn);
      organicMatterFactor := safediv(1.0, safediv(organicMatterFractionForLayer_frn / 0.224 + (1.0 -
        organicMatterFractionForLayer_frn), 1.75 - 0.009 * layers[layer].clayContent_pct));
      threshold := 0.9 * (1.0 - organicMatterFactor / 2.65);
      if (factor1 + factor2 >= threshold) then factor1 := threshold - factor2;
      threshold := 0.9 * layerInits[layer].initPorosity_mPm;
      if (factor1 + factor2 >= threshold) then factor1 := threshold - factor2;
      if (layers[layer].settledBulkDensity_tPm3 > organicMatterFactor) then 
        begin
        layerInits[layer].initFieldCapacity_mPm := factor1 + factor2 + (layers[layer].settledBulkDensity_tPm3 -
          organicMatterFactor) * (layerInits[layer].initPorosity_mPm - (factor1 + factor2));
        layerInits[layer].initWiltingPoint_mPm := factor1 + (layers[layer].settledBulkDensity_tPm3 - organicMatterFactor) *
          (layerInits[layer].initPorosity_mPm - factor1);
        end
      else
        begin
        layerInits[layer].initWiltingPoint_mPm := factor1 * (1.0 + (layers[layer].settledBulkDensity_tPm3 -
          organicMatterFactor));
        layerInits[layer].initFieldCapacity_mPm := layerInits[layer].initWiltingPoint_mPm + factor2 * (1.0 -
          (layers[layer].settledBulkDensity_tPm3 - organicMatterFactor));
        end;
      end;
    { adjust fc, wp, po, sw for rock content of soil and layer thickness }
    nonRockContent_frn := 1.0 - layers[layer].rockContent_pct * pct_to_frn;
    layers[layer].fieldCapacity_mm := layerInits[layer].initFieldCapacity_mPm * nonRockContent_frn * thicknessThisLayer_mm;
    layers[layer].wiltingPoint_mm := layerInits[layer].initWiltingPoint_mPm * nonRockContent_frn * thicknessThisLayer_mm;
    layers[layer].porosity_mm := layerInits[layer].initPorosity_mPm * nonRockContent_frn * thicknessThisLayer_mm;
    Utils_CheckRelationOfFieldCapacityToPorosity(layers[layer]);
    { calc starting soil water content from fraction of field capacity }
    layers[layer].waterContent_mm := max(0.0,
      defaults.initialFractionOfFieldCapacity_frn * (layers[layer].fieldCapacity_mm -
      layers[layer].wiltingPoint_mm) + layers[layer].wiltingPoint_mm);
    { set up travel time factor for lateral flow }
    layers[layer].travelTimeFactor := layers[layer].saturatedConductivity_mmPhr * params.watershedSlopeSteepness_mPm;
    { set CEC and sum of bases to pH value if not entered }
    { otherwise check and adjust }
    if (layers[layer].cationExchangeCapacity_cmolPkg = 0.0) then 
      begin
      layers[layer].cationExchangeCapacity_cmolPkg := layers[layer].soilpH;
      layers[layer].baseFormingCations_cmolPkg := layers[layer].soilpH;
      end
    else 
      begin
      { note that epic calls calciumCarbonate_pct "free soil calcium carbonate" }
      if (layers[layer].calciumCarbonate_pct > 0.0) then
        layers[layer].baseFormingCations_cmolPkg := layers[layer].cationExchangeCapacity_cmolPkg;
      if (layers[layer].baseFormingCations_cmolPkg > layers[layer].cationExchangeCapacity_cmolPkg) then
        layers[layer].baseFormingCations_cmolPkg := layers[layer].cationExchangeCapacity_cmolPkg;
      baseSaturation_pct := frn_to_pct * safediv(layers[layer].baseFormingCations_cmolPkg,
        layers[layer].cationExchangeCapacity_cmolPkg);
      layers[layer].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct,
        layerInits[layer].initOrganicC_pct, layers[layer].soilpH);
      end;
    { calc starting values of organic n fresh and organic p fresh }
    layers[layer].organicNFresh_kgPha := layers[layer].flatCropResidue_tPha * 8.0;
    layers[layer].organicPFresh_kgPha := layers[layer].flatCropResidue_tPha * 1.1;
    { estimate organic n and p if not entered }
    { assume C:N ratio of 10:1 }
    { N = 0.1C / 100 = N in parts/part x 10e6 = g/t }
    { 10e6/100 = 10e3 }
    Utils_DefaultFloatIfZero(layerInits[layer].initOrganicNTotalHumus_gPt, 1000.0 * layerInits[layer].initOrganicC_pct);
    Utils_DefaultFloatIfZero(layerInits[layer].initOrganicPHumus_gPt, 0.125 * layerInits[layer].initOrganicNTotalHumus_gPt);
    { calc p sorption coeff and mineral p flow coeff }
    layers[layer].pSorptionCoeff_frn := EQ.PSorptionCoeffForLayer_frn(layers[layer].calciumCarbonate_pct,
      layerInits[layer].initLabileP_gPt, baseSaturation_pct, layers[layer].soilpH, layers[layer].clayContent_pct,
      params.soilWeatheringType, layers[layer].pSorptionCoeff_frn);
    layers[layer].mineralPFlowCoeff_Pday := EQ.MineralPFlowCoeffForLayer_Pday(layers[layer].pSorptionCoeff_frn,
      layers[layer].calciumCarbonate_pct, params.soilWeatheringType);
    { calc mineral p active and stable, organic N active and stable in humus }
    initMineralPActive_gPt := layerInits[layer].initLabileP_gPt * safediv(1.0 - layers[layer].pSorptionCoeff_frn,
      layers[layer].pSorptionCoeff_frn);
    initMineralPStable_gPt := 4.0 * initMineralPActive_gPt;
    initOrganicNActiveHumus_gPt :=
      EQ.OrganicNActiveHumusForLayer_gPt(layers[layer].organicNActiveHumusFractionAtInput_frn,
      layerInits[layer].initOrganicNTotalHumus_gPt);
    initOrganicNStableHumus_gPt := layerInits[layer].initOrganicNTotalHumus_gPt - initOrganicNActiveHumus_gPt;
    { convert g/t to kg/ha for things entered as concentrations (g/t * t/ha = g/ha * (kg/1000g) = kg/ha) }
    conversionFactor_tkgPgha := layers[layer].weight_tPha * g_to_kg;
    layers[layer].mineralPActive_kgPha := initMineralPActive_gPt * conversionFactor_tkgPgha;
    layers[layer].mineralPStable_kgPha := initMineralPStable_gPt * conversionFactor_tkgPgha;
    layers[layer].labileP_kgPha := layerInits[layer].initLabileP_gPt * conversionFactor_tkgPgha;
    layers[layer].organicPHumus_kgPha := layerInits[layer].initOrganicPHumus_gPt * conversionFactor_tkgPgha;
    layers[layer].organicNActiveHumus_kgPha := initOrganicNActiveHumus_gPt * conversionFactor_tkgPgha;
    layers[layer].organicNStableHumus_kgPha := initOrganicNStableHumus_gPt * conversionFactor_tkgPgha;
    layers[layer].nitrate_kgPha := layerInits[layer].initNitrate_gPt * conversionFactor_tkgPgha;
    { set bulk density oven dry to settled bulk density if not input }
    Utils_DefaultFloatIfZero(layers[layer].bulkDensityOvenDry_tPm3, layers[layer].settledBulkDensity_tPm3);
    { set starting bulk density to settled bulk density }
    layers[layer].bulkDensity_tPm3 := layers[layer].settledBulkDensity_tPm3;
    { convert organic matter fraction to t/ha }
    layers[layer].organicMatter_tPha := organicMatterFractionForLayer_frn * layers[layer].weight_tPha;
    depthLastLayer_m := layers[layer].depth_m;
    thicknessLastLayer_mm := thicknessThisLayer_mm;
    end;
  { end of layer loop }
  { adjust number of layers according to how many layers were input }
  if (layer > kMaxPossibleLayers) then 
    begin
    state.numLayers := kMaxPossibleLayers;
    end
  else 
    begin
    state.numLayers := layer - 1;
    end;
  { make sure first soil layer is kFirstLayerDepth_m deep }
  { if less deep, just set depth to kFirstLayerDepth_m }
  { if deeper, move materials below kFirstLayerDepth_m out of first layer }
  if (layers[0].depth_m < kFirstLayerDepth_m) then setLayerDepth(0, kFirstLayerDepth_m);
  if (layers[0].depth_m > kFirstLayerDepth_m) then 
    begin
    if (state.numLayers < kMaxPossibleLayers) then 
      begin
      { if the number of layers isn't already at the maximum, }
      { increment the number of layers }
      inc(state.numLayers);
      { pull all layers down one including layer 1 }
      if state.numLayers > 1 then for layer := state.numLayers - 1 downto 1 do
        layers[layer] := layers[layer-1];
      { move the new layer into the position of layer 1 }
      layers[0] := layers[state.numLayers - 1];
      { take the portion of the old layer 1 that represents kFirstLayerDepth_m and move it into the new layer 1 }
      { (since all layers were moved down one, the old layer 1 is now layer 2) }
      Utils_ZeroAllLayerFields(layers[0]);
      Utils_MovePortionOfLayerToEmptyLayer(layers[1], layers[0], safediv(kFirstLayerDepth_m,
          layers[0].depth_m), kFirstLayerDepth_m, 0.0{no layer above});
      { recalculate porosity for layer 1 }
      layers[1].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
          layers[1].bulkDensity_tPm3, layers[1].rockContent_pct, layers[1].depth_m - layers[0].depth_m);
      Utils_CheckRelationOfFieldCapacityToPorosity(layers[1]);
      end
    else 
      begin
      { if there are already the maximum number of layers, take the excess from layer 1 }
      { and place it on top of what is already in layer 2. }
      propRemovedFromLayer1_frn := safediv(layers[0].depth_m - kFirstLayerDepth_m, layers[0].depth_m);
      layer2Thickness_m := layers[1].depth_m - layers[0].depth_m;
      propLayer2WasAlreadyThere_frn := safediv(layer2Thickness_m,
          layer2Thickness_m + (layers[0].depth_m - kFirstLayerDepth_m));
      Utils_MovePortionOfLayerToAnotherLayer(layers[0], layers[1], layers[0].depth_m, layer2Thickness_m,
        propRemovedFromLayer1_frn, propLayer2WasAlreadyThere_frn, params.watershedSlopeSteepness_mPm, false);
      setLayerDepth(0, kFirstLayerDepth_m);
      end;
    end;
  { if there aren't enough soil layers, split thickest ones until you get enough }
  while (state.numLayers < kMaxPossibleLayers) do EP.CreateNewLayerAtInputBySplittingThickest(soilPatch,
    defaults.firstLayerThisThickSplitAtInputIfNeeded_m);
  { calc curve numbers for slope to start simulation }
  { Removing dependence - do this when soil patch is copied from template, not here }
  (* moved to uesoil
  EP.AdjustCurveNumberForSlope(curveNumbers, params.watershedSlopeSteepness_mPm, Domain.garden.gardenParams, state.numLayers,
    layers);
  { starting number for curve number }
  curveNumbers.curveNumber := curveNumbers.avgCurveNumber;
  { adjust retention param if soil profile is shallow }
  if (layers[state.numLayers - 1].depth_m < 1.0) then curveNumbers.retentionParam_mm := curveNumbers.retentionParam_mm *
    (sqrt(layers[state.numLayers - 1].depth_m));
  { set time since auto irrigation and fertilization applied at interval }
  { so these things can be done right away }
  { Removing dependence - do this when soil patch is copied from template, not here }
  state.daysSinceAutoIrrApplied := Domain.garden.gardenParams.minIntervalAutoIrr_days;
  state.daysSinceAutoFertApplied := Domain.garden.gardenParams.minIntervalAutoFert_days;
  *)
  (* moved into soil and always default with no drainage system
  { adjust return flow travel time if there is a drainage system installed }
  if (((state.drainageSystem) and (state.layerWithDrainageSystem > 0))
      and (state.layerWithDrainageSystem <= state.numLayers)) then
    { Removing dependence - replace this with program-wide constant }
    surface.returnFlowTravelTime_days := max(defaults.timeForDrainageSystemToReducePlantStress_days,
      params.returnFlowTravelTime_days)
  else
    surface.returnFlowTravelTime_days := params.returnFlowTravelTime_days;
  *)
  { reset depths if deeper than lowest layer }
  updateSoilProfileDepthAndOtherDepths;
  { start n and p in non-plant standing dead entered at input }
  mulch.organicNFresh_kgPha := t_to_kg * 0.00829 * mulch.flatCropResidue_tPha;
  mulch.organicPFresh_kgPha := t_to_kg * 0.00104 * mulch.flatCropResidue_tPha;
  { confine data in EPIC files to EPIC hard-coded ranges taken from EPIC code }
  Defaults_EnforceSoilInputRanges(soilPatch, layerInits);
  { extra things added later }
  params.baseDrawingColor := support_rgb(100, 100, 100);
  params.mulchDrawingColor := support_rgb(200, 200, 200);
  { initializations for things saved from one day to the next }
  water.timeOfConc_hr := 0.85; { yesterday's timeOfConc_hr is used, so must start with something }
  erosion.windErosionAccumulatedFactor := 0.5;
  for i := 0 to 29 do totals.rainfallMinusRunoff30DaysArray_mm[i] := 3.0;
  totals.sumRainfallMinusRunoffPrev30Days_mm := 100.0;
  for i := 0 to 29 do totals.potSoilEvap30DaysArray_mm[i] := 3.0;
  totals.sumPotSoilEvapPrev30Days_mm := 100.0;
  water.snowWaterContent_mm := defaults.initSnowWaterContent_mm;
  Defaults_SoilPatchTotalsAtInput(soilPatch);
  SoilPatchTotals;
  Defaults_DefaultSoilParamsForVersionOne(soilPatch);
  end;  { with soilpatch }
  except
    errorMessage('Problem in Defaults_DefaultAndCalculateSoilInputs');
  end;
  end;

procedure Defaults_SoilPatchTotalsAtInput(soilPatch: GsSoilPatch);
  var
    layer, i: integer;
    layerBelowSoilProfileDepthHasBeenConsidered, layerBelowPlowDepthHasBeenConsidered: boolean;
    proportion, depthLastLayer_m, soilProfileDepthSoilWeight_tPha, plowDepthSoilWeightAtInput_tPha: single;
  begin
  with soilPatch do
  try
  layerBelowSoilProfileDepthHasBeenConsidered := false;
  layerBelowPlowDepthHasBeenConsidered := false;
  proportion := 0.0;
  depthLastLayer_m := 0.0;
  soilProfileDepthSoilWeight_tPha := 0.0;
  plowDepthSoilWeightAtInput_tPha := 0.0;
  { these two are needed later }
  totals.plowDepthSettledBulkDensityAtInput_tPm3 := 0.0;
  totals.plowDepthLabilePAtInput_kgPha := 0.0;
  { layer loop }
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    { For the following depth-dependent totals, if the layer is wholly within the depth }
    { (that is, if the depth at the bottom of the layer is less than the criterion depth) }
    { the whole layer amount is added to the total. If the layer is only partially within the }
    { depth (that is, if the depth at the bottom of the layer is greater than the criterion }
    { depth) only a proportion of that layer's amounts is added. That proportion is: }
    { (criterion depth - depth at top of layer) / thickness of the layer }
    { plow depth totals }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, state.plowDepth_m,
      layerBelowPlowDepthHasBeenConsidered);
    if (proportion > 0.0) then
      begin
      addQuantity(plowDepthSoilWeightAtInput_tPha, layers[layer].weight_tPha * proportion);
      addQuantity(totals.plowDepthLabilePAtInput_kgPha, layers[layer].labileP_kgPha * proportion);
      end;
    { soil profile depth totals }
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m,
      state.soilProfileDepth_m, layerBelowSoilProfileDepthHasBeenConsidered);
    if (proportion > 0.0) then
      addQuantity(soilProfileDepthSoilWeight_tPha, layers[layer].weight_tPha * proportion);
    end;
  totals.plowDepthSettledBulkDensityAtInput_tPm3 :=
      Utils_BulkDensityFromSoilWeightAndThickness_tPM3(plowDepthSoilWeightAtInput_tPha, state.plowDepth_m);
  except
    errorMessage('Problem in soil SoilPatchTotals'); end;
  end;

procedure Defaults_EnforceSoilInputRanges(soilPatch: GsSoilPatch; var layerInits: SoilTypeLayerInitsStructureArray);
  var
    i: integer;
  begin
  with soilPatch do
  begin
  Utils_EnforceFloatRange(params.watershedArea_ha, 0.1, 100.0);
  Utils_EnforceFloatRange(params.avgCurveNumberUnadjustedForSlope, 30.0, 95.0);
  water.avgCurveNumber := params.avgCurveNumberUnadjustedForSlope;
  Utils_EnforceFloatRange(params.manningsChannelRoughnessCoeff, 0.0, 0.2);
  Utils_EnforceFloatRange(params.manningsSurfaceRoughnessCoeff, 0.0, 0.3);
  Utils_EnforceFloatRange(params.watershedChannelDepth_m, 0.01, 5.0);
  Utils_EnforceFloatRange(params.watershedSlopeLength_m, 10.0, 150.0);
  Utils_EnforceFloatRange(params.watershedSlopeSteepness_mPm, 0.0001, 0.5);
  Utils_EnforceFloatRange(params.erosionControlPracticeFactor, 0.0, 1.0);
  Utils_EnforceFloatRange(params.watershedFieldLength_km, 0.0, 4.0);
  Utils_EnforceFloatRange(params.watershedFieldWidth_km, 0.0, 2.0);
  Utils_EnforceFloatRange(params.watershedFieldLengthOrientationFromNorth_rad, 0.0, 2.0 * kPi);
  Utils_EnforceFloatRange(mulch.flatCropResidue_tPha, 0.0, 100.0);
  Utils_EnforceFloatRange(params.soilParticleDiameter_microns, 300.0, 600.0);
  Utils_EnforceFloatRange(params.soilAlbedo_frn, 0.05, 0.2);
  Utils_EnforceFloatRange(params.waterTableMinDepth_m, 0.0, 2.0);
  Utils_EnforceFloatRange(params.waterTableMaxDepth_m, 0.0, 3.0);
  Utils_EnforceFloatRange(water.waterTableDepth_m, 0.0, 3.0);
  Utils_EnforceIntegerRange(params.soilWeatheringType, 0, 4);
  Utils_EnforceFloatRange(params.paramForReturnFlowTravelTime_days, 0.0, 2000.0);
  Utils_EnforceFloatRange(surface.ridgeHeight_mm, 0.0, 300.0);
  Utils_EnforceFloatRange(surface.ridgeInterval_m, 0.0, 2.0);
  for i := 0 to kMaxPossibleLayers - 1 do
    begin
    Utils_EnforceFloatRange(layers[i].depth_m, 0.0, 3.0);
    Utils_EnforceFloatRange(layers[i].settledBulkDensity_tPm3, 0.25, 2.3);
    Utils_EnforceFloatRange(layerInits[i].initWiltingPoint_mPm, 0.01, 0.65);
    Utils_EnforceFloatRange(layerInits[i].initFieldCapacity_mPm, 0.05, 0.8);
    Utils_EnforceFloatRange(layers[i].sandContent_pct, 0, 100);
    Utils_EnforceFloatRange(layers[i].siltContent_pct, 0, 100);
    Utils_EnforceFloatRange(layerInits[i].initOrganicNTotalHumus_gPt, 0.0, 5000.0);
    Utils_EnforceFloatRange(layers[i].soilpH, 4.0, 9.0);
    Utils_EnforceFloatRange(layers[i].baseFormingCations_cmolPkg, 0.0, 150.0);
    Utils_EnforceFloatRange(layerInits[i].initOrganicC_pct, 0.05, 5.0);
    Utils_EnforceFloatRange(layers[i].calciumCarbonate_pct, 0.0, 100.0);
    Utils_EnforceFloatRange(layers[i].cationExchangeCapacity_cmolPkg, 0.0, 150.0);
    Utils_EnforceFloatRange(layers[i].rockContent_pct, 0.0, 30.0);
    Utils_EnforceFloatRange(layerInits[i].initNitrate_gPt, 0.0, 30.0);
    Utils_EnforceFloatRange(layerInits[i].initLabileP_gPt, 0.0, 50.0);
    Utils_EnforceFloatRange(layers[i].flatCropResidue_tPha, 0.0, 15.0);
    Utils_EnforceFloatRange(layers[i].bulkDensityOvenDry_tPm3, 0.0, 2.5);
    Utils_EnforceFloatRange(layers[i].pSorptionCoeff_frn, 0.0, 0.75);
    Utils_EnforceFloatRange(layers[i].saturatedConductivity_mmPhr, 0.0, 50.0);
    Utils_EnforceFloatRange(layerInits[i].initOrganicPHumus_gPt, 0.0, 1000.0);
    end;
  end;
  end;

procedure Defaults_ZeroLayerInits(var layerInits: SoilTypeLayerInitsStructureArray);
  var i: smallint;
  begin
  for i := 0 to kMaxPossibleLayers do
    with layerInits[i] do
      begin
      initOrganicNTotalHumus_gPt := 0.0;
      initOrganicC_pct := 0.0;
      initNitrate_gPt := 0.0;
      initLabileP_gPt := 0.0;
      initOrganicPHumus_gPt := 0.0;
      initFieldCapacity_mPm := 0.0;
      initWiltingPoint_mPm := 0.0;
      initPorosity_mPm := 0.0;
      end;
  end;

procedure Defaults_DefaultSoilParamsForVersionOne(soilPatch: GsSoilPatch);
  { this function should be called when importing EPIC data AND when importing tabbed-text data version 0.90 }
  begin
  with soilPatch do
  try
  params.layerWithDrainageSystem := 3;      {wild guess}
  with surface do
    begin
    dikeHeightAtCreation_mm := 100.0; {range is 0 to 200}
    dikeInterval_m := 1.0;            {range is 0 to 3}
    randomRoughness_mm := 30.0;       {from EPIC data file; range is from 0 to 75}
    ridgeHeight_mm :=  100.0;         {from EPIC data file; range is from 0 to 300}
    ridgeHeightLastOperation_mm := surface.ridgeHeight_mm;
    ridgeInterval_m := 1.0;           {from EPIC data file; range is from 0 to 2}
    ridgeIntervalLastOperation_m := surface.ridgeInterval_m;
    end;
  params.carbonDioxideInAtmosphere_ppm := 330.0;
  params.peakRateEstimationMethod := kModifiedRationalEquationMethod;
  Utils_InitSCurveParam(params.curveNumberCoeffs, 0.5, 0.1, 0.6, 0.1);
  { curveNumberCoeffs: X is soil water content as fraction of field capacity, Y is curve number.
    0.5 FFC gives 0.1 curve number; 0.6 FFC gives 0.1 curve number
  ( NOT EDITABLE. All values are recalculated when the average curve number changes; these are so starting # is > 0.0.
    Actually, since these curve number coeffs are recalculated by the program, there is no reason to input them. }
  params.useStochasticCurveNumberEstimation := true;
  params.peakRunoffRainfallEnergyAdjFactor := 1.0;
  params.maxFractionOfRainfallInTimeOfConcParam := 2.0;
  params.fractDikeVolAvailForWaterStorage_frn := 0.9;
  params.waterErosionMethod := kOnstadFoster;
  params.userCoeffsForMUSI[0] := 0.0;
  params.userCoeffsForMUSI[1] := 0.0;
  params.userCoeffsForMUSI[2] := 0.0;
  params.userCoeffsForMUSI[3] := 0.0;
  params.maxWindErosionPerDay_tPha := 100.0;
  params.windErosionAccumCoeff := 0.01;
  params.windErosionAccelerationFactor := 1.0;
  params.timeForDrainageSystemToReducePlantStress_days := 5.0;
  params.potEvapMethod := kPenmanMonteithMethod;
  Utils_InitSCurveParam(params.soilEvapByDepthCoeffs, 10.0, 0.50, 100.0, 0.95);
  { soilEvapByDepthCoeffs: X is depth (mm), Y is fraction of soil evaporation between soil surface and specified depth.
    10 mm depth gives 0.50 of soil evaporation; 100 mm depth gives 0.95 of soil evaporation }
  { EDITABLE 0-soil last layer depth }
  params.lowerLimitWaterContentInTopP5MAsFractOfWP_frn := 0.5;
  params.avgNConcInRainfall_gPm3 := 0.8;
  params.nitrateConcInIrrigationWater_gPm3 := 10.0;
  params.nitrogenLeachingParam := 1.0;
  Utils_InitSCurveParam(params.nVolatilizationByDepthCoeffs, 5.0, 0.05, 100.0, 0.95);
  { nVolatilizationByDepthCoeffs: X is depth of nitrate in soil in mm, Y is N volatilization factor
    5 mm depth gives 0.05 N volatilization factor; 100 mm depth gives 0.95 N volatilization factor }
  { EDITABLE 0-soil last layer depth }
  Utils_InitSCurveParam(params.soilSettlingFromRainfallCoeffs, 5.0, 0.10, 100.0, 0.95);
  { soilSettlingFromRainfallCoeffs: X is rainfall - runoff adjusted for soil texture and depth in mm,
    Y is soil settling fraction caused by rainfall.
    5 mm water gives 0.1 settling fraction, 100 mm water gives 0.95 settling fraction }
  { EDITABLE 0-200 mm (guess) }
  Utils_InitSCurveParam(params.soilInsulationFromAirTempCoeffs, 1.0, 0.05, 3.0, 0.95);
  { soilInsulationFromAirTempCoeffs: X is aboveGroundBiomassAndResidue_tPha, Y is fraction of soil insulation from air temp
    1.0 t/ha residue gives 0.05 fraction of insulation; 3.0 t/ha residue gives 0.95 fraction of insulation }
  { EDITABLE 0-6 t/ha (guess) }
  params.ignoreSoilWaterContentWhenAutoIrrigating := true;
  params.autoIrrigationIsByFurrow := false;
  params.minIntervalAutoIrr_days := 3;
  params.minIntervalAutoFert_days := 45;
  params.ignoreNContentWhenAutoFertilizing := false;
  params.waterStressFactorForAutoIrr := 0.8;
  params.fractionOfIrrigationWaterLostToRunoff := 0.03;
  params.maxAnnualIrrVolumeAllowed_mm := 2000.0;
  params.minApplicVolumeAutoIrr_mm := 25.0;
  params.maxApplicVolumeAutoIrr_mm := 1000.0;
  params.nStressFactorToTriggerAutoFert := 0.8;
  params.maxFractionMaxNFertInOneApplic_frn := 0.75;
  params.minIntervalAutoHarvest_days := 5;
  params.avgCurveNumberUnadjustedForSlope := 85.0;
  { check range of vars }
  Utils_EnforceFloatRange(params.avgNConcInRainfall_gPm3, 0.5, 1.5);
  Utils_EnforceFloatRange(params.carbonDioxideInAtmosphere_ppm, 50.0, 2000.0);
  Utils_EnforceIntegerRange(params.potEvapMethod, 0, 3);
  Utils_EnforceIntegerRange(params.peakRateEstimationMethod, 0, 4);
  Utils_EnforceFloatRange(params.peakRunoffRainfallEnergyAdjFactor, 0.5, 2.0);
  Utils_EnforceIntegerRange(params.waterErosionMethod, 0, 5);
  Utils_EnforceFloatRange(params.userCoeffsForMUSI[0], 0.1, 5.0);
  Utils_EnforceFloatRange(params.userCoeffsForMUSI[1], 0.1, 0.8);
  Utils_EnforceFloatRange(params.userCoeffsForMUSI[2], 0.1, 0.8);
  Utils_EnforceFloatRange(params.userCoeffsForMUSI[3], 0.0, 0.3);
  Utils_EnforceFloatRange(params.windErosionAccelerationFactor, 0.0, 10.0);
  Utils_EnforceIntegerRange(params.minIntervalAutoIrr_days, 0, 365);
  Utils_EnforceIntegerRange(params.minIntervalAutoFert_days, 0, 1000);
  Utils_EnforceFloatRange(params.waterStressFactorForAutoIrr, -500.0, 1500.0);
  Utils_EnforceFloatRange(params.fractionOfIrrigationWaterLostToRunoff, 0.0, 0.6);
  Utils_EnforceFloatRange(params.maxAnnualIrrVolumeAllowed_mm, 0.0, 2000.0);
  Utils_EnforceFloatRange(params.minApplicVolumeAutoIrr_mm, 0.0, 500.0);
  Utils_EnforceFloatRange(params.maxApplicVolumeAutoIrr_mm, 0.0, 1000.0);
  Utils_EnforceFloatRange(params.nStressFactorToTriggerAutoFert, 0.0, 0.95);
  Utils_EnforceFloatRange(params.maxFractionMaxNFertInOneApplic_frn, 0.0, 1.0);
  Utils_EnforceFloatRange(params.timeForDrainageSystemToReducePlantStress_days, 0, 10);
  Utils_EnforceFloatRange(params.fractDikeVolAvailForWaterStorage_frn, 0.0, 0.99);
  Utils_EnforceFloatRange(params.nitrateConcInIrrigationWater_gPm3, 0.0, 100.0);
  except
    errorMessage('Problem in Defaults_DefaultSoilParamsMovedFromGarden');
  end;
  end;

procedure Defaults_DefaultAndCalculateWeatherInputs(weather: GsWeather);
  var
    i, month, monthMeanDay, daysInMonth, monthWithLowestMeanMinTemp, monthBefore: integer;
    total, declinationAngleOfSun_rad, maxPossibleRadiation_MJPm2, rainfallNormalizingFactorForModExpDist,
      yearLowMeanMinMonthTemp_degC, yearHighMeanMonthRadiation_MJPm2, yearHighMeanMonthRelHum_frn,
      yearHighMeanMonthMaxHalfHourRain_mm, sumMonthlyMeanMaxTempForYear_degC, meanMonthlyMeanMaxTempForYear_degC,
      sumMonthlyMeanMinTempForYear_degC, meanMonthlyMeanMinTempForYear_degC, sumMonthlyMeanTotalRainfallForYear_mm,
      meanMonthlyMeanTotalRainfall_mm, meanTempForMonth_degC, windSpeedModifier,
      probWetDayFromNumWetDays_frn, frequencyOfRainfallEvents_Pyr: single;
    defaultPeakRunoffRainfallEnergyAdjFactor: single;
  begin
  try
  defaultPeakRunoffRainfallEnergyAdjFactor := 1.0;
  with weather do
  begin
  Utils_DefaultFloatIfZero(params.yearsMaxMonthlyHalfHourRainfallRecord_yr, 10.0);
  Utils_DefaultFloatIfZero(params.coeffForWetDryProbsGivenNumWetDays_frn, 0.75);
  { if no std dev for rainfall is entered, calculate param for modified exp dist }
  if (params.stdDevDailyRainfallForMonth_mm[0] = 0.0) then 
    begin
    Utils_DefaultFloatIfZero(params.coeffRainfallModExpDist, 1.3);
    rainfallNormalizingFactorForModExpDist := Defaults_IterateForWeatherModExpRainfallNormalizingFactor(weather);
    end
  else
    rainfallNormalizingFactorForModExpDist := 1.0;
  { month loop A: calculates }
  { 1. std dev for max and min temp if not entered (from extremes) }
  { 2. num wet days if not entered (from probs) }
  { 3. prob of wet day if not entered (from num wet days) }
  { 4. mean radiation if not entered (from temp) }
  { 5. normalizing factor for rainfall, std dev or mod exp method }
  for month := 0 to 11 do 
    begin
    daysInMonth := GsDate_daysInMonthFromMonth_NoLeap(month);
    { if it looks like extreme temps were entered instead of std devs, }
    { calculate std devs from what was entered }
    if (params.stdDevMaxTempForMonth_degC[month] - params.stdDevMinTempForMonth_degC[month] > 10.0) then 
      begin
      params.stdDevMaxTempForMonth_degC[month] :=
        EQ.StdDevMaxTempFromMeanAndExtremeForMonth_degC(params.stdDevMaxTempForMonth_degC[month],
        params.dailyMeanMaxTempForMonth_degC[month]);
      params.stdDevMinTempForMonth_degC[month] :=
        EQ.StdDevMinTempFromMeanAndExtremeForMonth_degC(params.stdDevMinTempForMonth_degC[month],
        params.dailyMeanMinTempForMonth_degC[month]);
      end;
    { calculate probability of wet day or number of wet days if not entered }
    if (params.probWetDayAfterDryDayForMonth_frn[month] > 0.0) then 
      begin
      { if prob was entered, calculate numWetDaysForMonth (even if it was entered) }
      params.numWetDaysForMonth[month] := safediv(daysInMonth *
        params.probWetDayAfterDryDayForMonth_frn[month],
        1.0 - params.probWetDayAfterWetDayForMonth_frn[month] +
        params.probWetDayAfterDryDayForMonth_frn[month]);
      end
    else
      begin
      { if probs not entered, calculate them from numWetDaysForMonth }
      { (the assumption here is that the user either entered numWetDaysForMonth or probabilities }
      { but did not leave both at zero) }
      probWetDayFromNumWetDays_frn := EQ.ProbWetDayFromNumWetDays_frn(params.numWetDaysForMonth[month],
        daysInMonth);
      params.probWetDayAfterDryDayForMonth_frn[month] :=
        EQ.ProbWetDayAfterDryDayFromProbWetDay_frn(probWetDayFromNumWetDays_frn,
        params.coeffForWetDryProbsGivenNumWetDays_frn);
      params.probWetDayAfterWetDayForMonth_frn[month] :=
        EQ.ProbWetDayAfterWetDayFromProbWetDay_frn(probWetDayFromNumWetDays_frn,
        params.coeffForWetDryProbsGivenNumWetDays_frn);
      end;
    { calc dailyMeanRainfallForMonth_mm from total rainfall for month for later use }
    { evidently it is legitimate for both numWetDaysForMonth and probWetDayAfterDryDayForMonth_frn to be zero }
    if params.numWetDaysForMonth[month] = 0.0 then
      stationVars.dailyMeanRainfallForMonth_mm[month] := 0.0
    else
      stationVars.dailyMeanRainfallForMonth_mm[month] := safediv(params.meanTotalRainfallForMonth_mm[month],
        params.numWetDaysForMonth[month]);
    stationVars.probWetDayForMonth_frn[month] := safediv(params.numWetDaysForMonth[month], 1.0 * daysInMonth);
    { if dailyMeanRadiationForMonth_MJPm2 is not input, calculate it }
    { Removing dependence - this used garden latitude; changed to climate station latitude }
    if (params.dailyMeanRadiationForMonth_MJPm2[month] < kLowestFloatAboveZero) then 
      begin
      monthMeanDay := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, month);
      declinationAngleOfSun_rad := EQ.SunDeclinationAngle_rad(monthMeanDay);
      maxPossibleRadiation_MJPm2 := EP.MaxPossibleRadiation_MJPm2(monthMeanDay,
        params.climateLatitude_rad, declinationAngleOfSun_rad);
      params.dailyMeanRadiationForMonth_MJPm2[month] := 0.21 * maxPossibleRadiation_MJPm2 *
        sqrt(params.dailyMeanMaxTempForMonth_degC[month] - params.dailyMeanMinTempForMonth_degC[month]);
      end;
    { calculate normalizing factor for rainfall if std deviation is input, else }
    { adjust daily mean rainfall for month by normalizing factor for modified exponential }
    if (params.stdDevDailyRainfallForMonth_mm[month] > 0.0) then 
      begin
      if stationVars.dailyMeanRainfallForMonth_mm[month] = 0.0 then
        stationVars.rainfallNormalizingFactorForMonth[month] := 1.0
      else
        stationVars.rainfallNormalizingFactorForMonth[month] :=
            Defaults_IterateForWeatherRainfallNormalizingFactor(weather, month)
      end
    else 
      begin
      stationVars.dailyMeanRainfallForMonth_mm[month] := stationVars.dailyMeanRainfallForMonth_mm[month] *
          rainfallNormalizingFactorForModExpDist;
      stationVars.rainfallNormalizingFactorForMonth[month] := 1.0;
      end;
    end;
  { end of month loop A }
  { month loop B: calculates }
  { 1. mean temp for month and previous (used by soil temp function) }
  { 2. several means, maxes and mins }
  sumMonthlyMeanMaxTempForYear_degC := 0.0;
  sumMonthlyMeanMinTempForYear_degC := 0.0;
  sumMonthlyMeanTotalRainfallForYear_mm := 0.0;
  yearHighMeanMonthRadiation_MJPm2 := 0.0;
  yearHighMeanMonthRelHum_frn := 0.0;
  yearHighMeanMonthMaxHalfHourRain_mm := 0.0;
  yearLowMeanMinMonthTemp_degC := 0.0;
  monthWithLowestMeanMinTemp := 0;
  for month := 0 to 11 do 
    begin
    { calculate lowest mean monthly minimum temp for year }
    if (month = 0) then 
      begin
      monthBefore := 11;
      if (params.dailyMeanMinTempForMonth_degC[0] > params.dailyMeanMinTempForMonth_degC[11]) then 
        begin
        monthWithLowestMeanMinTemp := 11;
        end
      else 
        begin
        monthWithLowestMeanMinTemp := 0;
        end;
      yearLowMeanMinMonthTemp_degC := params.dailyMeanMinTempForMonth_degC[monthWithLowestMeanMinTemp];
      end
    else 
      begin
      monthBefore := month - 1;
      if (params.dailyMeanMinTempForMonth_degC[month] <= yearLowMeanMinMonthTemp_degC) then 
        begin
        monthWithLowestMeanMinTemp := month;
        yearLowMeanMinMonthTemp_degC := params.dailyMeanMinTempForMonth_degC[month];
        end;
      end;
    { calculate mean temp (mean of max and min) for this month and previous month }
    { for each month of year }
    { no longer need this - was used for calculating potential heat units for plants }
    {stationVars.dailyMeanMeanTempForMonthAndPrevious_degC[month] := 0.25 *
      (params.dailyMeanMaxTempForMonth_degC[month] + params.dailyMeanMinTempForMonth_degC[month] +
      params.dailyMeanMaxTempForMonth_degC[monthBefore] + params.dailyMeanMinTempForMonth_degC[monthBefore]);}
    { calculate maximum monthly radiation, rel hum, and alpha(0.5) for year }
    if (params.dailyMeanRadiationForMonth_MJPm2[month] > yearHighMeanMonthRadiation_MJPm2) then
      yearHighMeanMonthRadiation_MJPm2 := params.dailyMeanRadiationForMonth_MJPm2[month];
    if (params.dailyMeanRelHumForMonth_frn[month] > yearHighMeanMonthRelHum_frn) then
      yearHighMeanMonthRelHum_frn := params.dailyMeanRelHumForMonth_frn[month];
    if (params.meanPropRainInFirstHalfHourForMonth_frn[month] > yearHighMeanMonthMaxHalfHourRain_mm) then
      yearHighMeanMonthMaxHalfHourRain_mm := params.meanPropRainInFirstHalfHourForMonth_frn[month];
    { sum max temp, min temp and rainfall (for year averages) }
    sumMonthlyMeanMaxTempForYear_degC := sumMonthlyMeanMaxTempForYear_degC +
      (params.dailyMeanMaxTempForMonth_degC[month]);
    sumMonthlyMeanMinTempForYear_degC := sumMonthlyMeanMinTempForYear_degC +
      (params.dailyMeanMinTempForMonth_degC[month]);
    sumMonthlyMeanTotalRainfallForYear_mm := sumMonthlyMeanTotalRainfallForYear_mm +
      (params.meanTotalRainfallForMonth_mm[month]);
    end;
  { end of month loop B }
  { calculate yearly averages of max temp, min temp and rainfall }
  meanMonthlyMeanMaxTempForYear_degC := sumMonthlyMeanMaxTempForYear_degC / 12.0;
  meanMonthlyMeanMinTempForYear_degC := sumMonthlyMeanMinTempForYear_degC / 12.0;
  meanMonthlyMeanTotalRainfall_mm := sumMonthlyMeanTotalRainfallForYear_mm / 12.0;
  { month loop C: calculates }
  { 1. radiation in MJ/m2 if entered in langleys }
  { 2. rel hum if entered as dew point }
  { 3. rel hum if not entered (from temp) }
  { 4. alpha(0.5) if entered as R(0.5) }
  { 5. alpha(0.5) if not entered (from temp) }
  for month := 0 to 11 do 
    begin
    { convert radiation if in langleys (assume if > 100 is in langleys) }
    if (yearHighMeanMonthRadiation_MJPm2 > 100.0) then params.dailyMeanRadiationForMonth_MJPm2[month] :=
      params.dailyMeanRadiationForMonth_MJPm2[month] * (0.04184);
    { calculate relative humidity if dew point or not entered }
    if (yearHighMeanMonthRelHum_frn > 1.0) then 
      begin
      { if rel hum is > 1.0, assume it is dew point. convert to rel hum }
      meanTempForMonth_degC := 0.5 * (params.dailyMeanMaxTempForMonth_degC[month] +
        params.dailyMeanMinTempForMonth_degC[month]);
      params.dailyMeanRelHumForMonth_frn[month] :=
        EQ.RelHumForMonthFromDewPoint_frn(params.dailyMeanRelHumForMonth_frn[month], meanTempForMonth_degC);
      end
    else 
      begin
      { if rel hum is zero, generate from temperature only }
      if (params.dailyMeanRelHumForMonth_frn[month] < kLowestFloatAboveZero) then
        params.dailyMeanRelHumForMonth_frn[month] :=
        EQ.DailyMeanRelHumForMonthFromTemp_frn(params.dailyMeanMaxTempForMonth_degC[month],
        params.dailyMeanMinTempForMonth_degC[month]);
      end;
    { calculate alpha(0.5) if entered as R(0.5) or not entered }
    if (yearHighMeanMonthMaxHalfHourRain_mm > 1.0) then 
      begin
      { if yearHighMeanMonthMaxHalfHourRain_mm is > 1.0, it is R(0.5). convert to alpha(0.5) }
      frequencyOfRainfallEvents_Pyr :=
        EQ.FrequencyOfRainfallEvents_Pyr(params.yearsMaxMonthlyHalfHourRainfallRecord_yr);
      if params.numWetDaysForMonth[month] = 0.0 then
        params.meanPropRainInFirstHalfHourForMonth_frn[month] := 0.0
      else
        params.meanPropRainInFirstHalfHourForMonth_frn[month] :=
          EQ.MeanPropRainInFirstHalfHourForMonth_frn(frequencyOfRainfallEvents_Pyr,
          params.meanPropRainInFirstHalfHourForMonth_frn[month],
          params.meanTotalRainfallForMonth_mm[month],
          params.numWetDaysForMonth[month]);
      { Removing dependence - changed to constant }
      params.meanPropRainInFirstHalfHourForMonth_frn[month] :=
        params.meanPropRainInFirstHalfHourForMonth_frn[month] * defaultPeakRunoffRainfallEnergyAdjFactor;
      end
    else 
      begin
      { if yearHighMeanMonthMaxHalfHourRain_mm is zero, estimate alpha(0.5) from temperature }
      if (yearHighMeanMonthMaxHalfHourRain_mm < kLowestFloatAboveZero) then 
        begin
        { Removing dependence - changed to constant }
        params.meanPropRainInFirstHalfHourForMonth_frn[month] :=
          safediv(defaultPeakRunoffRainfallEnergyAdjFactor * 0.3725,
          meanMonthlyMeanMaxTempForYear_degC + 20.0) *
          (params.dailyMeanMaxTempForMonth_degC[month] + 20.0);
        end;
      end;
    { bound alpha(0.5) at 0.1 and 0.95 }
    if (params.meanPropRainInFirstHalfHourForMonth_frn[month] < 0.1) then
      params.meanPropRainInFirstHalfHourForMonth_frn[month] := 0.1;
    if (params.meanPropRainInFirstHalfHourForMonth_frn[month] > 0.95) then
      params.meanPropRainInFirstHalfHourForMonth_frn[month] := 0.95;
    end;
  { end month loop C }
  { calculate min day length for year }
  { this will never change (since it depends only on the input), so it doesn't need to be recalc every year }
  { Removing dependence - replace with climate station latitude }
  stationVars.minDayLengthForYear_hr := EQ.MinDayLengthForYear_hr(params.climateLatitude_rad);
  { set up day with lowest mean min temp < 5 deg C }
  { if middayOfMonthWithLowestMeanMinTempForYearBelow5degC is 400, this means that }
  { in no month of this year is the mean min temp below 5 degrees C. }
  { this is done with an as-needed function now, no var is needed }
  {stationVars.middayOfMonthWithLowestMeanMinTempForYearBelow5degC := 400;
  if (yearLowMeanMinMonthTemp_degC <= 5.0) then
    stationVars.middayOfMonthWithLowestMeanMinTempForYearBelow5degC :=
      GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, monthWithLowestMeanMinTemp);}
  { this mega-mean is used in the soil temperature equation }
  { this is done with an as-needed function now, no var is needed }
  {stationVars.meanMonthlyMeanMeanTempForYear_degC := (meanMonthlyMeanMaxTempForYear_degC +
    meanMonthlyMeanMinTempForYear_degC) / 2.0;}
  Utils_DefaultFloatIfZero(params.paramForModifiedExpWindSpeed, 0.5);
  { if wind directions not entered, default to 1.0 (equal distribution) }
  { because these all have to add up to 1.0 later, they get reset to 0.0625, which is 1.0 / 16 }
  for i := 0 to 15 do
    if (params.windDirectionsForMonth_frn[0][i] <= 0.0) then
      for month := 0 to 11 do
        params.windDirectionsForMonth_frn[month][i] := 1.0;
  { calc wind speed modifier from integrating mod exp equation }
  if (params.dailyMeanWindSpeedForMonth_mPsec[0] >= kLowestFloatAboveZero) then 
    windSpeedModifier := Utils_IntegrateModifiedExponentialEquation(params.paramForModifiedExpWindSpeed)
  else
    windSpeedModifier := 1.0;
  { multiply wind speed by wind speed modifier and make sure wind directions add up to 1.0 }
  for month := 0 to 11 do
    begin
    params.dailyMeanWindSpeedForMonth_mPsec[month] := params.dailyMeanWindSpeedForMonth_mPsec[month] /
      (windSpeedModifier);
    total := 0;
    for i := 0 to 15 do total := total + (params.windDirectionsForMonth_frn[month][i]);
    for i := 0 to 15 do params.windDirectionsForMonth_frn[month][i] :=
      params.windDirectionsForMonth_frn[month][i] / (total);
    end;
  { set multi-year adjustment factors at 1 and totals at zero for first year }
  for month := 0 to 11 do with longTermFactors do
    begin
    rainfallTotalForMonth_mm[month] := 0.0;
    rainfallAdjFactorForMonth[month] := 1.0;   { is multiplied }
    maxTempTotalForMonth_degC[month] := 0.0;
    maxTempAdjFactorForMonth[month] := 0.0; { is added }
    minTempTotalForMonth_degC[month] := 0.0;
    minTempAdjFactorForMonth[month] := 1.0; { is multiplied }
    end;
  Defaults_EnforceWeatherInputRanges(weather);
  weather.calculateJulianDayOfLastSpringFrost;
  weather.calculateJulianDayOfFirstFallFrost;
  end;
  except
    errorMessage('Problem in Defaults_DefaultAndCalculateWeatherInputs');
  end;
  end;

procedure Defaults_EnforceWeatherInputRanges(weather: GsWeather);
  var
    i, j: integer;
  begin
  with weather do
  begin
  Utils_EnforceFloatRange(params.yearsMaxMonthlyHalfHourRainfallRecord_yr, 0, 10);
  Utils_EnforceFloatRange(params.coeffForWetDryProbsGivenNumWetDays_frn, 0.0, 1.0);
  Utils_EnforceFloatRange(params.coeffRainfallModExpDist, 0.0, 100.0);
  for i := 0 to 11 do 
    begin
    Utils_EnforceFloatRange(params.dailyMeanMaxTempForMonth_degC[i],  - 10.0, 42.0);
    Utils_EnforceFloatRange(params.dailyMeanMinTempForMonth_degC[i],  - 30.0, 30.0);
    Utils_EnforceFloatRange(params.stdDevMaxTempForMonth_degC[i], 0.0, 15.0);
    Utils_EnforceFloatRange(params.stdDevMinTempForMonth_degC[i], 0.0, 15.0);
    Utils_EnforceFloatRange(params.meanTotalRainfallForMonth_mm[i], 0.0, 500.0);
    Utils_EnforceFloatRange(params.stdDevDailyRainfallForMonth_mm[i], 0.0, 50.0);
    { cfk change from EPIC - this was causing a problem and I am taking it out, can't find in EPIC code
      this brings up a problem: are these bounds (wherever they came from) supposed to stop at
      zero, or can the numbers also be negative? }
  {  Utils_EnforceFloatRange(params.skewCoeffForRainfallForMonth[i], 0.0, 7.0); }
    Utils_EnforceFloatRange(params.probWetDayAfterDryDayForMonth_frn[i], 0.0, 0.95);
    Utils_EnforceFloatRange(params.probWetDayAfterWetDayForMonth_frn[i], 0.0, 0.95);
    Utils_EnforceFloatRange(params.numWetDaysForMonth[i], 0.0, 31.0);
    Utils_EnforceFloatRange(params.meanPropRainInFirstHalfHourForMonth_frn[i], 0.0, 125.0);
    Utils_EnforceFloatRange(params.dailyMeanRadiationForMonth_MJPm2[i], 0.0, 750.0);
    Utils_EnforceFloatRange(params.dailyMeanRelHumForMonth_frn[i], 0.0, 1.0);
    Utils_EnforceFloatRange(params.dailyMeanWindSpeedForMonth_mPsec[i], 0.5, 10.0);
    for j := 0 to 15 do Utils_EnforceFloatRange(params.windDirectionsForMonth_frn[i][j], 0.0, 0.5);
    end;
  Utils_EnforceFloatRange(params.paramForModifiedExpWindSpeed, 0.0, 0.6);
  end;
  end;

function Defaults_IterateForWeatherModExpRainfallNormalizingFactor(weather: GsWeather): single;
  var
    i: integer;
    total, randomNumber: single;
  begin
  result := 1.0;
  try
  total := 0.0;
  for i := 0 to 10000 do
    begin
    randomNumber := Utils_RandomZeroToOne;
    total := total + power(-safeLn(randomNumber), weather.params.coeffRainfallModExpDist);
    end;
  result := safediv(10100.0, total);
  except
    errorMessage('Problem in Defaults_IterateForWeatherModExpRainfallNormalizingFactor');
  end;
  end;

function Defaults_IterateForWeatherRainfallNormalizingFactor(weather: GsWeather; month: integer): single;
  var
    i: integer;
    total, randomNumber, lastRandomNumber, normalRandomNumber, dayRain_mm: single;
  begin
  result := 1.0;
  try
  lastRandomNumber := Utils_RandomZeroToOne;
  total := 0.0;
  for i := 0 to 1000 do
    begin
    randomNumber := Utils_RandomZeroToOne;
    normalRandomNumber := Utils_StandardNormalDeviate(lastRandomNumber, randomNumber);
    dayRain_mm := EQ.DailyRainfallBySkewedNormal_mm(normalRandomNumber,
        weather.params.skewCoeffForRainfallForMonth[month],
        weather.params.stdDevDailyRainfallForMonth_mm[month],
        weather.stationVars.dailyMeanRainfallForMonth_mm[month]);
    total := total + dayRain_mm;
    lastRandomNumber := randomNumber;
    end;
  result := 1010.0 * safediv(weather.stationVars.dailyMeanRainfallForMonth_mm[month], total);
  except
    errorMessage('Problem in Defaults_IterateForWeatherRainfallNormalizingFactor');
  end;
  end;

procedure Defaults_DefaultAndCalculatePlantInputs(plant: GsPlant);
  begin
  with plant do
  try
  Utils_DefaultFloatIfZero(params.nFractionAtEmergence_frn, 0.044);
  Utils_DefaultFloatIfZero(params.nFractionAtHalfMaturity_frn, 0.0164);
  Utils_DefaultFloatIfZero(params.nFractionAtMaturity_frn, 0.0128);
  Utils_DefaultFloatIfZero(params.pFractionAtEmergence_frn, 0.0062);
  Utils_DefaultFloatIfZero(params.pFractionAtHalfMaturity_frn, 0.0023);
  Utils_DefaultFloatIfZero(params.pFractionAtMaturity_frn, 0.0018);
  { these are special s curves }
  ConvertOptimalNParams;
  ConvertOptimalPParams;
  { vpd over threshold can't be the same as vpd at threshold (0.01 is arbitrary) }
  if params.fractionOfMaxLeafConductForHighVPD.x <= params.thresholdVaporPressureDeficit_kPa then
    params.fractionOfMaxLeafConductForHighVPD.x := params.thresholdVaporPressureDeficit_kPa + 0.01;
  { fractionOfMaxLeafConductForHighVPD: this is like an S curve param, but there is only one X and one Y because it is linear.
    X is a value of vapor pressure deficit above the threshold, Y is the fraction of the max leaf conductance at that VPD
    4 kPa vapor pressure deficit gives 0.75 of max leaf conductance }
  ConvertLeafConductanceParams;
  { defaults }
  Utils_DefaultFloatIfZero(params.yearsToMaturityIfTree, 2000.0);
  Utils_DefaultFloatIfZero(params.maxAnnualNFert_kgPha, 200.0);
  Utils_DefaultFloatIfZero(params.absoluteTempForFrostKill_degC,  - 3.0);
  { new defaults added for info not in cultivars files }
  Utils_DefaultFloatIfZero(params.leafResistIfVPDBelowThreshold_mPsec, 0.007);
  Utils_DefaultFloatIfZero(params.probOfGerminationAfterFirstYear_frn, 0.2); { guess }
  Utils_DefaultFloatIfZero(params.seedWeight_g, 0.025); { guess }
  { defaults for reproductive parameters }
  Utils_DefaultFloatIfZero(params.heatUnitIndexAtEndOfVegetativePhase, 0.2);
  Utils_DefaultFloatIfZero(params.minTempForFloralInduction_degC, params.baseTemp_degC);
  Utils_DefaultFloatIfZero(params.optTempForFloralInduction_degC, params.optimalTemp_degC);
  Utils_DefaultFloatIfZero(params.maxTempForFloralInduction_degC, params.optimalTemp_degC +
    (params.optimalTemp_degC - params.baseTemp_degC));
  Utils_DefaultFloatIfZero(params.photothermalUnitsRequiredForFloralInduction, 7.0);
  Utils_DefaultFloatIfZero(params.optTempForVernalization_degC, 2.5);
  Utils_DefaultFloatIfZero(params.maxTempForVernalization_degC, 5.0);
  Utils_DefaultFloatIfZero(params.thermalUnitsRequiredForVernalization, 40.0);
  Utils_DefaultFloatIfZero(params.thermalUnitsRequiredForFloralInitiation, 13.0);
  Defaults_EnforcePlantInputRanges(plant);
  { default heights and diameters from maxHeightUnsupported and maxRootDepth }
  Utils_DefaultFloatIfZero(params.maxHeightSupported_m, params.maxHeightUnsupported_m);
  { default diameter at half height }
  Utils_DefaultFloatIfZero(params.maxDiameterUnsupported_m, params.maxHeightUnsupported_m / 2.0);
  Utils_DefaultFloatIfZero(params.maxDiameterSupported_m, params.maxHeightSupported_m / 2.0);
  Utils_DefaultFloatIfZero(params.maxRootSystemDiameter_m, params.maxRootDepth_m / 2.0);
  Defaults_DefaultPlantParamsForVersionOne(plant);
  drawingPlant.becomeGenericPlant;
  except
    errorMessage('Problem in Defaults_DefaultAndCalculatePlantInputs');
  end;
  end;

procedure Defaults_EnforcePlantInputRanges(plant: GsPlant);
  begin
  with plant do
  begin
  Utils_EnforceFloatRange(params.biomassToEnergyRatio_kgPhaPMJ, 10.0, 50.0);
  Utils_EnforceFloatRange(params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ, 10.0, 50.0);
  Utils_EnforceFloatRange(params.optimalTemp_degC, 10.0, 30.0);
  Utils_EnforceFloatRange(params.baseTemp_degC, 0.0, 12.0);
  Utils_EnforceFloatRange(params.maxLeafAreaIndex, 0.5, 10.0);
  Utils_EnforceFloatRange(params.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn, 0.40, 0.99);

  { x and y go from 0.0 to 1.0 }
  Utils_EnforceFloatRange(params.heatUnitFactorParamsForLAI.x1, 0.0, 1.0);
  Utils_EnforceFloatRange(params.heatUnitFactorParamsForLAI.y1, 0.0, 1.0);
  Utils_EnforceFloatRange(params.heatUnitFactorParamsForLAI.x2, 0.0, 1.0);
  Utils_EnforceFloatRange(params.heatUnitFactorParamsForLAI.y2, 0.0, 1.0);

  Utils_EnforceFloatRange(params.leafAreaIndexDeclineRateFactor, 0.0, 10.0);
  Utils_EnforceFloatRange(params.biomassAdjustmentIfLAIDecliningFactor, 0.0, 10.0);
  Utils_EnforceFloatRange(params.aluminumTolerance_pct, 0.0, 100.0);
  Utils_EnforceFloatRange(params.leafResistIfVPDBelowThreshold_mPsec, 0.0, 5.0);
  Utils_EnforceFloatRange(params.criticalAerationFactor_frn, 0.75, 1.0);
  Utils_EnforceFloatRange(params.seedWeight_g, 0.0, 100.0); { made up range }
  Utils_EnforceFloatRange(params.maxHeightSupported_m, 0.1, 3.0);
  Utils_EnforceFloatRange(params.maxHeightUnsupported_m, 0.1, 3.0); { same?}
  Utils_EnforceFloatRange(params.maxRootDepth_m, 0.5, 3.0);

  Utils_EnforceFloatRange(params.minCropManagementFactor, 0.001, 0.5);
  Utils_EnforceFloatRange(params.nFractionAtEmergence_frn, 0.04, 0.06);
  Utils_EnforceFloatRange(params.nFractionAtHalfMaturity_frn, 0.015, 0.03);
  Utils_EnforceFloatRange(params.nFractionAtMaturity_frn, 0.01, 0.27);
  Utils_EnforceFloatRange(params.pFractionAtEmergence_frn, 0.006, 0.009);
  Utils_EnforceFloatRange(params.pFractionAtHalfMaturity_frn, 0.002, 0.005);
  Utils_EnforceFloatRange(params.pFractionAtMaturity_frn, 0.0015, 0.0035);
  Utils_EnforceFloatRange(params.windErosionFactorStandingLive, 0.4, 3.5);
  Utils_EnforceFloatRange(params.windErosionFactorStandingDead, 0.4, 3.5);
  Utils_EnforceFloatRange(params.windErosionFactorFlatResidue, 0.2, 3.5);

  { x goes from 0 to -30; y goes from 0.0 to 0.99 }
  Utils_EnforceFloatRange(params.frostReductionFactorParams.x1,  0.0, 30.0);
  Utils_EnforceFloatRange(params.frostReductionFactorParams.y1,  0.0, 0.99);
  Utils_EnforceFloatRange(params.frostReductionFactorParams.x2,  0.0, 30.0);
  Utils_EnforceFloatRange(params.frostReductionFactorParams.y2,  0.0, 0.99);

  Utils_EnforceFloatRange(params.biomassToEnergyRatioVPDParam, 0.0, 999.0);
  Utils_EnforceFloatRange(params.thresholdVaporPressureDeficit_kPa, 0.0, 999.0);
  Utils_EnforceFloatRange(params.fractionOfMaxLeafConductForHighVPD.x, 0.0, 999.0);
  Utils_EnforceFloatRange(params.fractionRootWtAtEmergence_frn, 0.0, 0.999);
  Utils_EnforceFloatRange(params.fractionRootWtAtMaturity_frn, 0.0, 0.999);
  Utils_EnforceFloatRange(params.potHeatUnitsReqForMaturation, 0.0, 3000.0);
  Utils_EnforceFloatRange(params.maxAnnualNFert_kgPha, 0.0, 9999.0);
  Utils_EnforceFloatRange(params.yearsToMaturityIfTree, 0, 100);
  { reproductive params }
  Utils_EnforceFloatRange(params.heatUnitIndexAtEndOfVegetativePhase, 0.0, 1.0);
  Utils_EnforceFloatRange(params.minTempForFloralInduction_degC, 0.0, 30.0);
  Utils_EnforceFloatRange(params.optTempForFloralInduction_degC, 0.0, 40.0);

  { x goes from 0 to 24; y goes from 0.0 to 1.0 }
  Utils_EnforceFloatRange(params.floralInductionParams.x1, 0.0, 24.0);
  Utils_EnforceFloatRange(params.floralInductionParams.y1, 0.0, 1.0);
  Utils_EnforceFloatRange(params.floralInductionParams.x2, 0.0, 24.0);
  Utils_EnforceFloatRange(params.floralInductionParams.y2, 0.0, 1.0);

  Utils_EnforceFloatRange(params.photothermalUnitsRequiredForFloralInduction, 0.0, 50.0);
  Utils_EnforceIntegerRange(params.vernalizationRequirement, 0, 2);
  Utils_EnforceFloatRange(params.minTempForVernalization_degC,  - 10.0, 15.0);
  Utils_EnforceFloatRange(params.optTempForVernalization_degC,  - 10.0, 15.0);
  Utils_EnforceFloatRange(params.maxTempForVernalization_degC,  - 10.0, 15.0);
  Utils_EnforceFloatRange(params.thermalUnitsRequiredForVernalization, 0.0, 100.0);
  Utils_EnforceFloatRange(params.thermalUnitsRequiredForFloralInitiation, 0.0, 20.0);
  Utils_EnforceFloatRange(params.fractionReproductiveAllocationAtMaturity_frn, 0.0, 1.0);
  Utils_EnforceFloatRange(params.fractionStorageOrganAllocationAtMaturity_frn, 0.0, 1.0);
  Utils_EnforceFloatRange(params.heatUnitIndexAtStartOfStorageOrganAllocation, 0.0, 1.0);
  end;
  end;

procedure Defaults_DefaultPlantParamsForVersionOne(plant: GsPlant);
  { this function should be called when importing EPIC data AND when importing tabbed-text data version 0.90 }
  begin
  with plant do
  try
  Utils_DefaultFloatIfZero(params.minSoilWaterInPlowDepthForGermination_mm, 20.0); {cfk made up - had zero}
  Utils_DefaultFloatIfZero(params.minHeatUnitsBeforeGermination_degC, 25.0);
  Utils_DefaultFloatIfZero(params.canopyResistParam, 2.0);
  Utils_InitSCurveParam(params.rootGrowthRestrictionByAerationStress, 0.33, 0.75, 0.67, 0.99);
  { rootGrowthRestrictionByAerationStress:
    X is fraction of storage volume between critical aeration factor and saturation,
    Y is fraction of reduction in root growth caused by aeration stress.
    0.33 of storage volume gives 0.75 reduction in root growth; 0.67 of storage volume gives 0.99 reduction in root growth }
  { EDITABLE 0-1 }
  Utils_InitSCurveParam(params.rootGrowthRestrictionByRockCoeffs, 90.0, 0.05, 99.0, 0.95);
  { rootGrowthRestrictionByRockCoeffs: X is percent rock, Y is fraction reduction in root growth.
    90% rock gives 0.05 reduction; 99% rock gives 0.95 reduction }
  { note: the reason I didn't change this percent to fraction is because rock content is in percent. }
  { EDITABLE 0-100 }
  Utils_DefaultFloatIfZero(params.rootGrowthRestrictionInSandySoilParam, 1.5);
  Utils_InitSCurveParam(params.pUptakeCoeffs, 5.0, 0.01, 20.0, 0.90);
  { pUptakeCoeffs: X is soil labile P concentration in ppm, Y is P uptake factor for plants (0 to 1)
    5 ppm soil labile P gives 0.01 P uptake factor; 20 ppm soil labile P gives 0.90 P uptake factor }
  { EDITABLE 0-40 ppm (guess) }
  Utils_DefaultFloatIfZero(params.nFixationParam_frn, 0.9);
  Utils_InitSCurveParam(params.plantNAndPStressCoeffs, 0.2, 0.5, 0.8, 0.95);
  { plantNAndPStressCoeffs: X is fraction of difference between plant N content ratios (ratio of actual-potential N content),
    Y is the N stress factor.
    0.2 of difference between actual and potential gives 0.5 stress factor; 0.8 of difference gives 0.95 stress factor}
  { EDITABLE 0-1 }
  Utils_DefaultFloatIfZero(params.areaOfSoilPatchInWhichPlanted_ha, 1.0 * m2_to_ha);
  Utils_DefaultIntIfZero(params.plantingSpringFallOrBoth, kCultivarIsPlantedInSpring);
  Utils_DefaultIntIfZero(params.plantingDaysAfterLastSpringFrost_days, 14);
  Utils_DefaultIntIfZero(params.maturityDaysBeforeFirstFallFrost_days, 0);
  Utils_DefaultIntIfZero(params.plantingDaysFromSeedToGerminatedSeed_days, 14);
  Utils_DefaultIntIfZero(params.plantingDaysFromGerminatedSeedToSeasonMaturity_days, 50);
  Utils_DefaultFloatIfZero(params.minPossibleHeatUnitsBeforeGerminationInAnyClimate_degC, 10.0);
  Utils_DefaultFloatIfZero(params.maxPossibleHeatUnitsBeforeGerminationInAnyClimate_degC, 100.0);
  Utils_DefaultFloatIfZero(params.minPossibleHeatUnitsBeforeMaturityInAnyClimate_degC, 100.0);
  Utils_DefaultFloatIfZero(params.maxPossibleHeatUnitsBeforeMaturityInAnyClimate_degC, 3000.0);
  Utils_DefaultFloatIfZero(params.hoursAboveMinDayLengthWhenWinterGrowthCanOccur_hr, 1.0);
  except
    errorMessage('Problem in Defaults_DefaultPlantParamsMovedFromGarden');
  end;
  end;

end.

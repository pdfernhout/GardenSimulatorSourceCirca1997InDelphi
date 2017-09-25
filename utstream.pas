unit Utstream;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utstream: Functions to stream model templates to/from a tab-delimited text file.
These methods were removed from the object files because they made them too long.
For soil patches and plants there are separate streaming functions for version 0.90
and version 1.0. For the weather there were no changes so there is only one function.
If there were a version 1.1, each object might need a third function to stream in/out
the correct fields. A few other objects have streamUsingTextFiler methods, namely
the drawing plant (udplant), 3D objects (uturt3d), icons (uicon), harvest item
templates (uharvprt), and soil amendments (uebag). These functions are in their own files. }

interface

uses
  uesoil, ueplant, ueweath, ufilertx;

procedure Weather_streamUsingTextFiler(weather: GsWeather; textFiler: GsTextFiler);
procedure SoilPatch_streamUsingTextFilerVersion090(soil: GsSoilPatch; textFiler: GsTextFiler);
procedure SoilPatch_streamUsingTextFilerVersionOne(soil: GsSoilPatch; textFiler: GsTextFiler);
procedure Plant_streamUsingTextFilerVersion090(plant: GsPlant; textFiler: GsTextFiler);
procedure Plant_streamUsingTextFilerVersionOne(plant: GsPlant; textFiler: GsTextFiler);

implementation

uses uestruct, udefault, ueutils;

procedure Weather_streamUsingTextFiler(weather: GsWeather; textFiler: GsTextFiler);
  begin
  { weather has no change between version 0.90 and 1.0 }
  with weather, textFiler do
    begin
    { params }
    streamSingle(params.climateLatitude_rad, 'climateLatitude_rad');
    streamSingle(params.climateElevation_m, 'climateElevation_m');
    streamSingle(params.yearsMaxMonthlyHalfHourRainfallRecord_yr, 'yearsMaxMonthlyHalfHourRainfallRecord_yr');
    streamSingle(params.coeffForWetDryProbsGivenNumWetDays_frn, 'coeffForWetDryProbsGivenNumWetDays_frn');
    streamSingle(params.coeffRainfallModExpDist, 'coeffRainfallModExpDist');
    streamSingle(params.paramForModifiedExpWindSpeed, 'paramForModifiedExpWindSpeed');
    streamArrayMonths(params.dailyMeanMaxTempForMonth_degC, 'dailyMeanMaxTempForMonth_degC');
    streamArrayMonths(params.dailyMeanMinTempForMonth_degC, 'dailyMeanMinTempForMonth_degC');
    streamArrayMonths(params.stdDevMaxTempForMonth_degC, 'stdDevMaxTempForMonth_degC');
    streamArrayMonths(params.stdDevMinTempForMonth_degC, 'stdDevMinTempForMonth_degC');
    streamArrayMonths(params.meanTotalRainfallForMonth_mm, 'meanTotalRainfallForMonth_mm');
    streamArrayMonths(params.stdDevDailyRainfallForMonth_mm, 'stdDevDailyRainfallForMonth_mm');
    streamArrayMonths(params.skewCoeffForRainfallForMonth, 'skewCoeffForRainfallForMonth');
    streamArrayMonths(params.probWetDayAfterDryDayForMonth_frn, 'probWetDayAfterDryDayForMonth_frn');
    streamArrayMonths(params.probWetDayAfterWetDayForMonth_frn, 'probWetDayAfterWetDayForMonth_frn');
    streamArrayMonths(params.numWetDaysForMonth, 'numWetDaysForMonth');
    streamArrayMonths(params.meanPropRainInFirstHalfHourForMonth_frn, 'meanPropRainInFirstHalfHourForMonth_frn');
    streamArrayMonths(params.dailyMeanRadiationForMonth_MJPm2, 'dailyMeanRadiationForMonth_MJPm2');
    streamArrayMonths(params.dailyMeanRelHumForMonth_frn, 'dailyMeanRelHumForMonth_frn');
    streamArrayMonths(params.dailyMeanWindSpeedForMonth_mPsec, 'dailyMeanWindSpeedForMonth_mPsec');
    streamArrayWindDirection(params.windDirectionsForMonth_frn, 'windDirectionsForMonth_frn');
    { stationVars }
    streamArrayMonths(stationVars.probWetDayForMonth_frn, 'probWetDayForMonth_frn');
    streamArrayMonths(stationVars.dailyMeanRainfallForMonth_mm, 'dailyMeanRainfallForMonth_mm');
    streamSingle(stationVars.unused1, 'unused1');
    streamArrayMonths(stationVars.rainfallNormalizingFactorForMonth, 'rainfallNormalizingFactorForMonth');
    streamArrayMonths(stationVars.unused2, 'unused2');
    streamSingle(stationVars.minDayLengthForYear_hr, 'minDayLengthForYear_hr');
    streamSingle(stationVars.unused3, 'unused3');
    { matrices }
    streamArrayThree(matrices.matrixAResidual, 'matrixAResidual ');
    streamArrayThree(matrices.matrixBResidual, 'matrixBResidual ');
    streamArrayThree(matrices.correlationMultiplier, 'correlationMultiplier ');
    streamArrayThree(matrices.yesterdaysCorrelationMultiplier, 'yesterdaysCorrelationMultiplier ');
    streamSingle(matrices.yesterdaysUniformRandNumForRainfall_frn, 'yesterdaysUniformRandNumForRainfall_frn');
    streamSingle(matrices.lastEpsilonRandomNumber, 'lastEpsilonRandomNumber');
    streamEndOfLine;
    end;
  end;

procedure SoilPatch_streamUsingTextFilerVersion090(soil: GsSoilPatch; textFiler: GsTextFiler);
  var
    streamArray: arraySoilLayers;
    i: smallint;
    ignoreBoolean: boolean;
    ignoreFloat: single;
  begin
  with soil, textFiler do
    begin
    streamSingle(params.manningsChannelRoughnessCoeff, 'manningsChannelRoughnessCoeff');
    streamSingle(params.manningsSurfaceRoughnessCoeff, 'manningsSurfaceRoughnessCoeff');
    streamSingle(params.erosionControlPracticeFactor, 'erosionControlPracticeFactor');
    streamSingle(params.soilParticleDiameter_microns, 'soilParticleDiameter_microns');
    streamSmallint(params.soilWeatheringType, 'soilWeatheringType');
    streamSingle(params.paramForReturnFlowTravelTime_days, 'paramForReturnFlowTravelTime_days');
    streamSingle(params.soilAlbedo_frn, 'soilAlbedo_frn');
    streamColorRef(params.baseDrawingColor, 'baseDrawingColor');
    streamColorRef(params.mulchDrawingColor, 'mulchDrawingColor');
    streamSmallint(state.numLayers, 'numLayers');
    streamSingle(state.soilProfileDepth_m, 'soilProfileDepth_m');
    streamSingle(state.plowDepth_m, 'plowDepth_m');
    streamSingle(water.returnFlowTravelTime_days, 'returnFlowTravelTime_days');
    streamSingle(mulch.depth_m, 'mulch depth_m');
    streamSingle(mulch.flatCropResidue_tPha, 'mulch flatCropResidue_tPha');
    streamSingle(mulch.organicNFresh_kgPha, 'mulch organicNFresh_kgPha');
    streamSingle(mulch.organicPFresh_kgPha, 'mulch organicPFresh_kgPha');
    {depth_m}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].depth_m;
    streamArrayLayers(streamArray, 'depth_m');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].depth_m := streamArray[i];
    {temperature_degC}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].temperature_degC;
    streamArrayLayers(streamArray, 'temperature_degC');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].temperature_degC := streamArray[i];
    {weight_tPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].weight_tPha;
    streamArrayLayers(streamArray, 'weight_tPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].weight_tPha := streamArray[i];
    {waterContent_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].waterContent_mm;
    streamArrayLayers(streamArray, 'waterContent_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].waterContent_mm := streamArray[i];
    {fieldCapacity_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].fieldCapacity_mm;
    streamArrayLayers(streamArray, 'fieldCapacity_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].fieldCapacity_mm := streamArray[i];
    {wiltingPoint_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].wiltingPoint_mm;
    streamArrayLayers(streamArray, 'wiltingPoint_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].wiltingPoint_mm := streamArray[i];
    {porosity_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].porosity_mm;
    streamArrayLayers(streamArray, 'porosity_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].porosity_mm := streamArray[i];
    {travelTimeFactor}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].travelTimeFactor;
    streamArrayLayers(streamArray, 'travelTimeFactor');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].travelTimeFactor := streamArray[i];
    {nitrate_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].nitrate_kgPha;
    streamArrayLayers(streamArray, 'nitrate_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].nitrate_kgPha := streamArray[i];
    {ammonia_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].ammonia_kgPha;
    streamArrayLayers(streamArray, 'ammonia_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].ammonia_kgPha := streamArray[i];
    {organicNFresh_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicNFresh_kgPha;
    streamArrayLayers(streamArray, 'organicNFresh_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicNFresh_kgPha := streamArray[i];
    {organicNActiveHumus_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicNActiveHumus_kgPha;
    streamArrayLayers(streamArray, 'organicNActiveHumus_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicNActiveHumus_kgPha := streamArray[i];
    {organicNStableHumus_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicNStableHumus_kgPha;
    streamArrayLayers(streamArray, 'organicNStableHumus_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicNStableHumus_kgPha := streamArray[i];
    {organicNActiveHumusFractionAtInput_frn}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do
      streamArray[i] := layers[i].organicNActiveHumusFractionAtInput_frn;
    streamArrayLayers(streamArray, 'organicNActiveHumusFractionAtInput_frn');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do
      layers[i].organicNActiveHumusFractionAtInput_frn := streamArray[i];
    {labileP_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].labileP_kgPha;
    streamArrayLayers(streamArray, 'labileP_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].labileP_kgPha := streamArray[i];
    {organicPFresh_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicPFresh_kgPha;
    streamArrayLayers(streamArray, 'organicPFresh_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicPFresh_kgPha := streamArray[i];
    {organicPHumus_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicPHumus_kgPha;
    streamArrayLayers(streamArray, 'organicPHumus_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicPHumus_kgPha := streamArray[i];
    {mineralPActive_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].mineralPActive_kgPha;
    streamArrayLayers(streamArray, 'mineralPActive_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].mineralPActive_kgPha := streamArray[i];
    {mineralPStable_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].mineralPStable_kgPha;
    streamArrayLayers(streamArray, 'mineralPStable_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].mineralPStable_kgPha := streamArray[i];
    {pSorptionCoeff_frn}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].pSorptionCoeff_frn;
    streamArrayLayers(streamArray, 'pSorptionCoeff_frn');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].pSorptionCoeff_frn := streamArray[i];
    {soilpH}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].soilpH;
    streamArrayLayers(streamArray, 'soilpH');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].soilpH := streamArray[i];
    {aluminumSaturation_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].aluminumSaturation_pct;
    streamArrayLayers(streamArray, 'aluminumSaturation_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].aluminumSaturation_pct := streamArray[i];
    {baseFormingCations_cmolPkg}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].baseFormingCations_cmolPkg;
    streamArrayLayers(streamArray, 'baseFormingCations_cmolPkg');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].baseFormingCations_cmolPkg := streamArray[i];
    {bulkDensity_tPm3}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].bulkDensity_tPm3;
    streamArrayLayers(streamArray, 'bulkDensity_tPm3');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].bulkDensity_tPm3 := streamArray[i];
    {settledBulkDensity_tPm3}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].settledBulkDensity_tPm3;
    streamArrayLayers(streamArray, 'settledBulkDensity_tPm3');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].settledBulkDensity_tPm3 := streamArray[i];
    {bulkDensityOvenDry_tPm3}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].bulkDensityOvenDry_tPm3;
    streamArrayLayers(streamArray, 'bulkDensityOvenDry_tPm3');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].bulkDensityOvenDry_tPm3 := streamArray[i];
    {cationExchangeCapacity_cmolPkg}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].cationExchangeCapacity_cmolPkg;
    streamArrayLayers(streamArray, 'cationExchangeCapacity_cmolPkg');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].cationExchangeCapacity_cmolPkg := streamArray[i];
    {saturatedConductivity_mmPhr}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].saturatedConductivity_mmPhr;
    streamArrayLayers(streamArray, 'saturatedConductivity_mmPhr');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].saturatedConductivity_mmPhr := streamArray[i];
    {flatCropResidue_tPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].flatCropResidue_tPha;
    streamArrayLayers(streamArray, 'flatCropResidue_tPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].flatCropResidue_tPha := streamArray[i];
    {organicMatter_tPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicMatter_tPha;
    streamArrayLayers(streamArray, 'organicMatter_tPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicMatter_tPha := streamArray[i];
    {calciumCarbonate_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].calciumCarbonate_pct;
    streamArrayLayers(streamArray, 'calciumCarbonate_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].calciumCarbonate_pct := streamArray[i];
    {clayContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].clayContent_pct;
    streamArrayLayers(streamArray, 'clayContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].clayContent_pct := streamArray[i];
    {sandContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].sandContent_pct;
    streamArrayLayers(streamArray, 'sandContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].sandContent_pct := streamArray[i];
    {siltContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].siltContent_pct;
    streamArrayLayers(streamArray, 'siltContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].siltContent_pct := streamArray[i];
    {rockContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].rockContent_pct;
    streamArrayLayers(streamArray, 'rockContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].rockContent_pct := streamArray[i];
    { rest - not layers }
    streamSingle(water.avgCurveNumber, 'avgCurveNumber');
    ignoreBoolean := false;
    streamBoolean(ignoreBoolean, 'furrowDikes');
    streamSingle(surface.ridgeHeight_mm, 'ridgeHeight_mm');
    streamSingle(surface.ridgeHeightLastOperation_mm, 'ridgeHeightLastOperation_mm');
    streamSingle(surface.ridgeInterval_m, 'ridgeInterval_m');
    streamSingle(surface.ridgeIntervalLastOperation_m, 'ridgeIntervalLastOperation_m');
    streamSingle(surface.dikeHeight_mm, 'dikeHeight_mm');
    streamSingle(surface.dikeHeightAtCreation_mm, 'dikeHeightAtCreation_mm');
    streamSingle(surface.dikeInterval_m, 'dikeInterval_m');
    streamSingle(surface.dikeVolume_mm, 'dikeVolume_mm');
    streamSingle(params.fractDikeVolAvailForWaterStorage_frn, 'fractDikeVolAvailForWaterStorage_frn');
    streamSingle(surface.randomRoughness_mm, 'randomRoughness_mm');
    ignoreFloat := 0.0;
    streamSingle(ignoreFloat, 'plowDepthSoilWeightAtInput_tPha');
    streamSingle(totals.plowDepthSettledBulkDensityAtInput_tPm3, 'plowDepthSettledBulkDensityAtInput_tPm3');
    streamSingle(totals.unused1, 'unused1');
    streamSingle(totals.plowDepthLabilePAtInput_kgPha, 'plowDepthLabilePAtInput_kgPha');
    streamSingle(totals.patchWeightedMeanBaseFormingCations_cmolPkg, 'patchWeightedMeanBaseFormingCations_cmolPkg');
    streamSingle(totals.patchWeightedMeanOrganicC_pct, 'patchWeightedMeanOrganicC_pct');
    streamSingle(totals.patchWeightedMeanCationExchangeCapacity_cmolPkg, 'patchWeightedMeanCationExchangeCapacity_cmolPkg');
    streamSingle(water.snowWaterContent_mm, 'snowWaterContent_mm');
    streamSingle(water.snowPackTemp_degC, 'snowPackTemp_degC');
    streamSingle(water.waterTableDepth_m, 'depth_m');
    streamSingle(params.waterTableMinDepth_m, 'minDepth_m');
    streamSingle(params.waterTableMaxDepth_m, 'maxDepth_m');
    streamSingle(params.watershedArea_ha, 'area_ha');
    streamSingle(params.watershedChannelLength_km, 'channelLength_km');
    streamSingle(params.watershedChannelSlope_mPm, 'channelSlope_mPm');
    streamSingle(params.watershedChannelDepth_m, 'channelDepth_m');
    streamSingle(params.watershedSlopeLength_m, 'slopeLength_m');
    streamSingle(params.watershedSlopeSteepness_mPm, 'slopeSteepness_mPm');
    streamSingle(params.watershedFieldLength_km, 'fieldLength_km');
    streamSingle(params.watershedFieldWidth_km, 'fieldWidth_km');
    streamSingle(params.watershedFieldLengthOrientationFromNorth_rad, 'fieldLengthOrientationFromNorth_rad');
    streamSingle(params.watershedChannelLength_km, 'channelLength_km');
    { note: this was a mistake, but i have to leave it in this version now }
    streamSingle(params.watershedChannelLength_km, 'channelLength_km');
    streamSingle(params.watershedChannelLength_km, 'channelLength_km');
    Defaults_DefaultSoilParamsForVersionOne(soil);
    streamEndOfLine;
    end;
  end;

function labelForPeakRateMethod(methodType: smallint): string;
  begin
  result := '';
  case methodType of
    kModifiedRationalEquationMethod: result := 'rational equation';
    kSCSTypeI: result := 'SCS type I';
    kSCSTypeIa: result := 'SCS type Ia';
    kSCSTypeII: result := 'SCS type II';
    kSCSTypeIII: result := 'SCS type III';
    else result := '';
    end;
  end;

function labelForWaterErosionMethod(methodType: smallint): string;
  begin
  result := '';
  case methodType of
    kUSLE: result := 'USLE';
    kMUSLE: result := 'MUSLE';
    kOnstadFoster: result := 'Onstad-Foster';
    kMUSS: result := 'MUSS';
    kMUST: result := 'MUST';
    kMUSI: result := 'MUSI';
    else result := '';
    end;
  end;

function labelForPotEvapMethod(methodType: smallint): string;
  begin
  result := '';
  case methodType of
    kPenmanMethod: result := 'Penman';
    kPenmanMonteithMethod: result := 'Penman-Monteith';
    kPriestleyTaylorMethod: result := 'Priestley-Taylor';
    kHargreavesMethod: result := 'Hargreaves';
    else result := '';
    end;
  end;

function labelForWeatheringType(methodType: smallint): string;
  begin
  result := '';
  case methodType of
    kCalcareousSoilOrWithoutWeatherInfo: result := 'calcareous or no info';
    kSlightlyWeatheredSoil: result := 'slightly weathered';
    kModWeatheredSoil: result := 'moderately weathered';
    kHighlyWeatheredSoil: result := 'highly weathered';
    kInputPSorptionCoeffs: result := 'P sorption coeffs input';
    else result := '';
    end;
  end;

procedure SoilPatch_streamUsingTextFilerVersionOne(soil: GsSoilPatch; textFiler: GsTextFiler);
  var
    streamArray: arraySoilLayers;
    i: smallint;
    ignoreSingle: single;
  begin
  with soil, textFiler do
    begin
    { site and drawing vars }
    streamSingle(params.area_ha, 'area_ha');
    streamSingle(params.drawingScale, 'drawingScale');
    streamSingle(params.shadeIndex_pct, 'shadeIndex_pct');
    streamSingle(params.viewingAngle_deg, 'viewingAngle_deg');
    streamColorRef(params.baseDrawingColor, 'baseDrawingColor');
    streamColorRef(params.mulchDrawingColor, 'mulchDrawingColor');
    streamSingle(params.carbonDioxideInAtmosphere_ppm, 'carbonDioxideInAtmosphere_ppm');
    streamSingle(params.watershedArea_ha, 'watershedArea_ha');
    { watershed }
    streamSingle(params.watershedChannelLength_km, 'watershedChannelLength_km');
    streamSingle(params.watershedChannelSlope_mPm, 'watershedChannelSlope_mPm');
    streamSingle(params.watershedChannelDepth_m, 'watershedChannelDepth_m');
    streamSingle(params.watershedSlopeLength_m, 'watershedSlopeLength_m');
    streamSingle(params.watershedSlopeSteepness_mPm, 'watershedSlopeSteepness_mPm');
    streamSingle(params.watershedFieldLength_km, 'watershedFieldLength_km');
    streamSingle(params.watershedFieldWidth_km, 'watershedFieldWidth_km');
    streamSingle(params.watershedFieldLengthOrientationFromNorth_rad, 'watershedFieldLengthOrientationFromNorth_rad');
    { water table }
    streamSingle(params.waterTableMinDepth_m, 'waterTableMinDepth_m');
    streamSingle(params.waterTableMaxDepth_m, 'waterTableMaxDepth_m');
    { runoff }
    streamSmallintWithLabel(params.peakRateEstimationMethod,
      labelForPeakRateMethod(params.peakRateEstimationMethod), 'peakRateEstimationMethod');
    streamSCurve(params.curveNumberCoeffs, 'curveNumberCoeffs');
    streamBoolean(params.useStochasticCurveNumberEstimation, 'useStochasticCurveNumberEstimation');
    streamSingle(params.peakRunoffRainfallEnergyAdjFactor, 'peakRunoffRainfallEnergyAdjFactor');
    streamSingle(params.manningsChannelRoughnessCoeff, 'manningsChannelRoughnessCoeff');
    streamSingle(params.manningsSurfaceRoughnessCoeff, 'manningsSurfaceRoughnessCoeff');
    streamSingle(params.maxFractionOfRainfallInTimeOfConcParam, 'maxFractionOfRainfallInTimeOfConcParam');
    streamSingle(params.fractDikeVolAvailForWaterStorage_frn, 'fractDikeVolAvailForWaterStorage_frn');
    { water erosion }
    streamSmallintWithLabel(params.waterErosionMethod,
      labelForWaterErosionMethod(params.waterErosionMethod), 'waterErosionMethod');
    streamArrayFour(params.userCoeffsForMUSI, 'userCoeffsForMUSI');
    streamSingle(params.erosionControlPracticeFactor, 'erosionControlPracticeFactor');
    { wind erosion }
    streamSingle(params.soilParticleDiameter_microns, 'soilParticleDiameter_microns');
    streamSingle(params.maxWindErosionPerDay_tPha, 'maxWindErosionPerDay_tPha');
    streamSingle(params.windErosionAccumCoeff, 'windErosionAccumCoeff');
    streamSingle(params.windErosionAccelerationFactor, 'windErosionAccelerationFactor');
    { percolation and lateral flow }
    streamSingle(params.paramForReturnFlowTravelTime_days, 'paramForReturnFlowTravelTime_days');
    streamSingle(params.timeForDrainageSystemToReducePlantStress_days, 'timeForDrainageSystemToReducePlantStress_days');
    { evaporation }
    streamSmallintWithLabel(params.potEvapMethod,
      labelForPotEvapMethod(params.potEvapMethod), 'potEvapMethod');
    streamSCurve(params.soilEvapByDepthCoeffs, 'soilEvapByDepthCoeffs');
    streamSingle(params.lowerLimitWaterContentInTopP5MAsFractOfWP_frn, 'lowerLimitWaterContentInTopP5MAsFractOfWP_frn');
    { nutrients }
    streamSingle(params.avgNConcInRainfall_gPm3, 'avgNConcInRainfall_gPm3');
    streamSingle(params.nitrateConcInIrrigationWater_gPm3, 'nitrateConcInIrrigationWater_gPm3');
    streamSingle(params.nitrogenLeachingParam, 'nitrogenLeachingParam');
    streamSCurve(params.nVolatilizationByDepthCoeffs, 'nVolatilizationByDepthCoeffs');
    { soil structure }
    streamSmallintWithLabel(params.soilWeatheringType,
      labelForWeatheringType(params.soilWeatheringType), 'soilWeatheringType');
    streamSCurve(params.soilSettlingFromRainfallCoeffs, 'soilSettlingFromRainfallCoeffs');
    { soil temperature }
    streamSCurve(params.soilInsulationFromAirTempCoeffs, 'soilInsulationFromAirTempCoeffs');
    streamSingle(params.soilAlbedo_frn, 'soilAlbedo_frn');
    { auto operations }
    streamBoolean(params.ignoreSoilWaterContentWhenAutoIrrigating, 'ignoreSoilWaterContentWhenAutoIrrigating');
    streamBoolean(params.autoIrrigationIsByFurrow, 'autoIrrigationIsByFurrow');
    streamSmallint(params.layerWithDrainageSystem, 'layerWithDrainageSystem');
    streamSmallint(params.minIntervalAutoIrr_days, 'minIntervalAutoIrr_days');
    streamSmallint(params.minIntervalAutoFert_days, 'minIntervalAutoFert_days');
    streamBoolean(params.ignoreNContentWhenAutoFertilizing, 'ignoreNContentWhenAutoFertilizing');
    streamSingle(params.waterStressFactorForAutoIrr, 'waterStressFactorForAutoIrr');
    streamSingle(params.fractionOfIrrigationWaterLostToRunoff, 'fractionOfIrrigationWaterLostToRunoff');
    streamSingle(params.maxAnnualIrrVolumeAllowed_mm, 'maxAnnualIrrVolumeAllowed_mm');
    streamSingle(params.minApplicVolumeAutoIrr_mm, 'minApplicVolumeAutoIrr_mm');
    streamSingle(params.maxApplicVolumeAutoIrr_mm, 'maxApplicVolumeAutoIrr_mm');
    streamSingle(params.nStressFactorToTriggerAutoFert, 'nStressFactorToTriggerAutoFert');
    streamSingle(params.maxFractionMaxNFertInOneApplic_frn, 'maxFractionMaxNFertInOneApplic_frn');
    streamSmallint(params.minIntervalAutoHarvest_days, 'minIntervalAutoHarvest_days');
    streamSingle(params.avgCurveNumberUnadjustedForSlope, 'avgCurveNumberUnadjustedForSlope');
    { state }
    streamSmallint(state.numLayers, 'numLayers');
    streamSingle(state.soilProfileDepth_m, 'soilProfileDepth_m');
    streamSingle(state.plowDepth_m, 'plowDepth_m');
    { surface }
    streamSingle(surface.ridgeHeight_mm, 'ridgeHeight_mm');
    streamSingle(surface.ridgeHeightLastOperation_mm, 'ridgeHeightLastOperation_mm');
    streamSingle(surface.ridgeInterval_m, 'ridgeInterval_m');
    streamSingle(surface.ridgeIntervalLastOperation_m, 'ridgeIntervalLastOperation_m');
    streamSingle(surface.dikeHeight_mm, 'dikeHeight_mm');
    streamSingle(surface.dikeHeightAtCreation_mm, 'dikeHeightAtCreation_mm');
    streamSingle(surface.dikeInterval_m, 'dikeInterval_m');
    streamSingle(surface.dikeVolume_mm, 'dikeVolume_mm');
    streamSingle(surface.randomRoughness_mm, 'randomRoughness_mm');
    { mulch }
    streamSingle(mulch.flatCropResidue_tPha, 'mulch flatCropResidue_tPha');
    streamSingle(mulch.organicNFresh_kgPha, 'mulch organicNFresh_kgPha');
    streamSingle(mulch.organicPFresh_kgPha, 'mulch organicPFresh_kgPha');
    if isReading then
      mulch.depth_m := Utils_MulchDepthFromMulchFlatCropResidue_m(mulch.flatCropResidue_tPha);
    { water }
    streamSingle(water.snowWaterContent_mm, 'snowWaterContent_mm');
    streamSingle(water.snowPackTemp_degC, 'snowPackTemp_degC');
    streamSingle(water.waterTableDepth_m, 'depth_m');
    streamSingle(water.avgCurveNumber, 'avgCurveNumber');
    streamSingle(water.retentionParam_mm, 'retentionParam_mm');
    streamSingle(water.returnFlowTravelTime_days, 'returnFlowTravelTime_days');
    streamSingle(water.pooledWater_mm, 'pooledWater_mm');
    { erosion }
    streamSingle(erosion.rainfallEnergyFactorForUSLE, 'rainfallEnergyFactorForUSLE');
    streamSingle(erosion.windErosionAccumulatedFactor, 'windErosionAccumulatedFactor');
    { layers }
    {depth_m}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].depth_m;
    streamArrayLayers(streamArray, 'depth_m');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].depth_m := streamArray[i];
    {temperature_degC}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].temperature_degC;
    streamArrayLayers(streamArray, 'temperature_degC');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].temperature_degC := streamArray[i];
    {weight_tPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].weight_tPha;
    streamArrayLayers(streamArray, 'weight_tPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].weight_tPha := streamArray[i];
    {waterContent_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].waterContent_mm;
    streamArrayLayers(streamArray, 'waterContent_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].waterContent_mm := streamArray[i];
    {fieldCapacity_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].fieldCapacity_mm;
    streamArrayLayers(streamArray, 'fieldCapacity_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].fieldCapacity_mm := streamArray[i];
    {wiltingPoint_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].wiltingPoint_mm;
    streamArrayLayers(streamArray, 'wiltingPoint_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].wiltingPoint_mm := streamArray[i];
    {porosity_mm}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].porosity_mm;
    streamArrayLayers(streamArray, 'porosity_mm');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].porosity_mm := streamArray[i];
    {travelTimeFactor}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].travelTimeFactor;
    streamArrayLayers(streamArray, 'travelTimeFactor');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].travelTimeFactor := streamArray[i];
    {nitrate_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].nitrate_kgPha;
    streamArrayLayers(streamArray, 'nitrate_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].nitrate_kgPha := streamArray[i];
    {ammonia_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].ammonia_kgPha;
    streamArrayLayers(streamArray, 'ammonia_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].ammonia_kgPha := streamArray[i];
    {organicNFresh_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicNFresh_kgPha;
    streamArrayLayers(streamArray, 'organicNFresh_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicNFresh_kgPha := streamArray[i];
    {organicNActiveHumus_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicNActiveHumus_kgPha;
    streamArrayLayers(streamArray, 'organicNActiveHumus_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicNActiveHumus_kgPha := streamArray[i];
    {organicNStableHumus_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicNStableHumus_kgPha;
    streamArrayLayers(streamArray, 'organicNStableHumus_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicNStableHumus_kgPha := streamArray[i];
    {organicNActiveHumusFractionAtInput_frn}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do
      streamArray[i] := layers[i].organicNActiveHumusFractionAtInput_frn;
    streamArrayLayers(streamArray, 'organicNActiveHumusFractionAtInput_frn');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do
      layers[i].organicNActiveHumusFractionAtInput_frn := streamArray[i];
    {labileP_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].labileP_kgPha;
    streamArrayLayers(streamArray, 'labileP_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].labileP_kgPha := streamArray[i];
    {organicPFresh_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicPFresh_kgPha;
    streamArrayLayers(streamArray, 'organicPFresh_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicPFresh_kgPha := streamArray[i];
    {organicPHumus_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicPHumus_kgPha;
    streamArrayLayers(streamArray, 'organicPHumus_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicPHumus_kgPha := streamArray[i];
    {mineralPActive_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].mineralPActive_kgPha;
    streamArrayLayers(streamArray, 'mineralPActive_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].mineralPActive_kgPha := streamArray[i];
    {mineralPStable_kgPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].mineralPStable_kgPha;
    streamArrayLayers(streamArray, 'mineralPStable_kgPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].mineralPStable_kgPha := streamArray[i];
    {pSorptionCoeff_frn}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].pSorptionCoeff_frn;
    streamArrayLayers(streamArray, 'pSorptionCoeff_frn');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].pSorptionCoeff_frn := streamArray[i];
    {soilpH}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].soilpH;
    streamArrayLayers(streamArray, 'soilpH');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].soilpH := streamArray[i];
    {aluminumSaturation_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].aluminumSaturation_pct;
    streamArrayLayers(streamArray, 'aluminumSaturation_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].aluminumSaturation_pct := streamArray[i];
    {baseFormingCations_cmolPkg}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].baseFormingCations_cmolPkg;
    streamArrayLayers(streamArray, 'baseFormingCations_cmolPkg');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].baseFormingCations_cmolPkg := streamArray[i];
    {bulkDensity_tPm3}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].bulkDensity_tPm3;
    streamArrayLayers(streamArray, 'bulkDensity_tPm3');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].bulkDensity_tPm3 := streamArray[i];
    {settledBulkDensity_tPm3}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].settledBulkDensity_tPm3;
    streamArrayLayers(streamArray, 'settledBulkDensity_tPm3');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].settledBulkDensity_tPm3 := streamArray[i];
    {bulkDensityOvenDry_tPm3}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].bulkDensityOvenDry_tPm3;
    streamArrayLayers(streamArray, 'bulkDensityOvenDry_tPm3');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].bulkDensityOvenDry_tPm3 := streamArray[i];
    {cationExchangeCapacity_cmolPkg}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].cationExchangeCapacity_cmolPkg;
    streamArrayLayers(streamArray, 'cationExchangeCapacity_cmolPkg');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].cationExchangeCapacity_cmolPkg := streamArray[i];
    {saturatedConductivity_mmPhr}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].saturatedConductivity_mmPhr;
    streamArrayLayers(streamArray, 'saturatedConductivity_mmPhr');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].saturatedConductivity_mmPhr := streamArray[i];
    {flatCropResidue_tPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].flatCropResidue_tPha;
    streamArrayLayers(streamArray, 'flatCropResidue_tPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].flatCropResidue_tPha := streamArray[i];
    {organicMatter_tPha}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].organicMatter_tPha;
    streamArrayLayers(streamArray, 'organicMatter_tPha');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].organicMatter_tPha := streamArray[i];
    {calciumCarbonate_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].calciumCarbonate_pct;
    streamArrayLayers(streamArray, 'calciumCarbonate_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].calciumCarbonate_pct := streamArray[i];
    {clayContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].clayContent_pct;
    streamArrayLayers(streamArray, 'clayContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].clayContent_pct := streamArray[i];
    {sandContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].sandContent_pct;
    streamArrayLayers(streamArray, 'sandContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].sandContent_pct := streamArray[i];
    {siltContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].siltContent_pct;
    streamArrayLayers(streamArray, 'siltContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].siltContent_pct := streamArray[i];
    {rockContent_pct}
    if isWriting then for i := 0 to kMaxPossibleLayers - 1 do streamArray[i] := layers[i].rockContent_pct;
    streamArrayLayers(streamArray, 'rockContent_pct');
    if isReading then for i := 0 to kMaxPossibleLayers - 1 do layers[i].rockContent_pct := streamArray[i];
    { totals }
    { set only at soil patch creation }
    streamSingle(totals.plowDepthSettledBulkDensityAtInput_tPm3, 'plowDepthSettledBulkDensityAtInput_tPm3');
    streamSingle(totals.plowDepthLabilePAtInput_kgPha, 'plowDepthLabilePAtInput_kgPha');
    { was soilProfileDepthSettledBulkDensity_tPm3, no longer using }
    streamSingle(totals.unused1, 'unused1');
    { set at patch creation and when liming occurs }
    streamSingle(totals.patchWeightedMeanBaseFormingCations_cmolPkg, 'patchWeightedMeanBaseFormingCations_cmolPkg');
    streamSingle(totals.patchWeightedMeanOrganicC_pct, 'patchWeightedMeanOrganicC_pct');
    streamSingle(totals.patchWeightedMeanCationExchangeCapacity_cmolPkg, 'patchWeightedMeanCationExchangeCapacity_cmolPkg');
    streamSingle(totals.patchWeightedMeanpH, 'patchWeightedMeanpH');
    streamEndOfLine;
    end;
  end;

procedure Plant_StreamUsingTextFilerVersion090(plant: GsPlant; textFiler: GsTextFiler);
  var
    ignoreFloat: single;
    ignoreSmallint, tempSmallint: smallint;
    tempBoolean: boolean;
  begin
  with plant, textFiler do
    begin
    streamSmallint(possibleSize, 'possibleSize');
    streamSingle(params.biomassToEnergyRatio_kgPhaPMJ, 'biomassToEnergyRatio_kgPhaPMJ');
    streamSingle(params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ, 'biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ');
    streamSingle(params.optimalTemp_degC, 'optimalTemp_degC');
    streamSingle(params.baseTemp_degC, 'baseTemp_degC');
    streamSingle(params.maxLeafAreaIndex, 'maxLeafAreaIndex');
    streamSingle(params.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn,
        'fractionOfGrowingSeasonWhenLeafDeclineStarts_frn');
    streamSingle(params.leafAreaIndexDeclineRateFactor, 'leafAreaIndexDeclineRateFactor');
    streamSingle(params.biomassAdjustmentIfLAIDecliningFactor, 'biomassAdjustmentIfLAIDecliningFactor');
    streamSingle(params.plantingDepth_mm, 'plantingDepth_mm');
    streamSingle(params.aluminumTolerance_pct, 'aluminumTolerance_pct');
    streamSingle(params.leafResistIfVPDBelowThreshold_mPsec, 'leafResistIfVPDBelowThreshold_mPsec');
    streamSingle(params.criticalAerationFactor_frn, 'criticalAerationFactor_frn');
    streamSingle(params.seedWeight_g, 'seedWeight_g');
    streamSingle(params.maxHeightUnsupported_m, 'maxHeightUnsupported_m');
    streamSingle(params.maxHeightSupported_m, 'maxHeightSupported_m');
    streamSingle(params.maxDiameterUnsupported_m, 'maxDiameterUnsupported_m');
    streamSingle(params.maxDiameterSupported_m, 'maxDiameterSupported_m');
    streamSingle(params.maxRootDepth_m, 'maxRootDepth_m');
    streamSingle(params.maxRootSystemDiameter_m, 'maxRootSystemDiameter_m');
    streamSingle(params.minCropManagementFactor, 'minCropManagementFactor');
    streamSingle(params.windErosionFactorStandingLive, 'windErosionFactorStandingLive');
    streamSingle(params.windErosionFactorStandingDead, 'windErosionFactorStandingDead');
    streamSingle(params.windErosionFactorFlatResidue, 'windErosionFactorFlatResidue');
    ignoreSmallint := 0;
    streamSmallint(ignoreSmallint, 'lifeCycleClimateType');
    streamSingle(params.biomassToEnergyRatioVPDParam, 'biomassToEnergyRatioVPDParam');
    streamSingle(params.thresholdVaporPressureDeficit_kPa, 'thresholdVaporPressureDeficit_kPa');
    streamSingle(params.fractionRootWtAtEmergence_frn, 'fractionRootWtAtEmergence_frn');
    streamSingle(params.fractionRootWtAtMaturity_frn, 'fractionRootWtAtMaturity_frn');
    streamSingle(params.potHeatUnitsReqForMaturation, 'potHeatUnitsReqForMaturation');
    streamSingle(params.maxAnnualNFert_kgPha, 'maxAnnualNFert_kgPha');
    streamSingle(params.nFractionAtEmergence_frn, 'nFractionAtEmergence_frn');
    streamSingle(params.nFractionAtHalfMaturity_frn, 'nFractionAtHalfMaturity_frn');
    streamSingle(params.nFractionAtMaturity_frn, 'nFractionAtMaturity_frn');
    streamSingle(params.pFractionAtEmergence_frn, 'pFractionAtEmergence_frn');
    streamSingle(params.pFractionAtHalfMaturity_frn, 'pFractionAtHalfMaturity_frn');
    streamSingle(params.pFractionAtMaturity_frn, 'pFractionAtMaturity_frn');
    streamSingle(params.yearsToMaturityIfTree, 'yearsToMaturityIfTree');
    streamSingle(params.probOfGerminationAfterFirstYear_frn, 'probOfGerminationAfterFirstYear_frn');
    streamSingle(params.absoluteTempForFrostKill_degC, 'absoluteTempForFrostKill_degC');
    streamSingle(params.heatUnitIndexAtEndOfVegetativePhase, 'heatUnitIndexAtEndOfVegetativePhase');
    streamSingle(params.minTempForFloralInduction_degC, 'minTempForFloralInduction_degC');
    streamSingle(params.optTempForFloralInduction_degC, 'optTempForFloralInduction_degC');
    streamSingle(params.maxTempForFloralInduction_degC, 'maxTempForFloralInduction_degC');
    streamSingle(params.photothermalUnitsRequiredForFloralInduction, 'photothermalUnitsRequiredForFloralInduction');
    { mereged these two fields into one, moved obligate and quantitative up one in enum }
    if isWriting then tempBoolean := (params.vernalizationRequirement <> kNoVernalization);
    streamBoolean(tempBoolean, 'vernalizationIsRequired');
    if isWriting then
      begin
      if tempBoolean then
        tempSmallint := params.vernalizationRequirement - 1
      else
        tempSmallint := 0;
      end;
    streamSmallint(tempSmallint, 'vernalizationObligateOrQuantitative');
    if isReading then
      begin
      if tempBoolean {vernalization required} then
        params.vernalizationRequirement := tempSmallint + 1
      else
        params.vernalizationRequirement := kNoVernalization;
      end;
    streamSingle(params.minTempForVernalization_degC, 'minTempForVernalization_degC');
    streamSingle(params.optTempForVernalization_degC, 'optTempForVernalization_degC');
    streamSingle(params.maxTempForVernalization_degC, 'maxTempForVernalization_degC');
    streamSingle(params.thermalUnitsRequiredForVernalization, 'thermalUnitsRequiredForVernalization');
    streamSingle(params.thermalUnitsRequiredForFloralInitiation, 'thermalUnitsRequiredForFloralInitiation');
    streamSingle(params.fractionReproductiveAllocationAtMaturity_frn, 'fractionReproductiveAllocationAtMaturity_frn');
    streamSingle(params.fractionStorageOrganAllocationAtMaturity_frn, 'fractionStorageOrganAllocationAtMaturity_frn');
    streamSingle(params.heatUnitIndexAtStartOfStorageOrganAllocation, 'heatUnitIndexAtStartOfStorageOrganAllocation');
    ignoreFloat := 0.0;
    streamSingle(ignoreFloat, 'heatUnitIndexAtStartOfReproductiveAllocation');
    streamSingle(params.reproductiveBiomassDecayAtPlantMaturity, 'reproductiveBiomassDecayAtPlantMaturity');
    streamSingleVarSCurve(params.fractionOfMaxLeafConductForHighVPD, 'fractionOfMaxLeafConductForHighVPD');
    streamSCurve(params.optimalNParams, 'optimalNParams');
    streamSCurve(params.optimalPParams, 'optimalPParams');
    streamSCurve(params.heatUnitFactorParamsForLAI, 'heatUnitFactorParamsForLAI');
    streamSCurve(params.frostReductionFactorParams, 'frostReductionFactorParams');
    streamSCurve(params.biomassToEnergyRatioCO2Params, 'biomassToEnergyRatioCO2Params');
    streamSCurve(params.floralInductionParams, 'floralInductionParams');
    if isReading then
      Defaults_DefaultPlantParamsForVersionOne(plant);
    drawingPlant.streamUsingTextFiler(textFiler);
    streamEndOfLine;
    end;
 end;

function labelForLifeCycleType(aType: smallint): string;
  begin
  result := '';
  case aType of
    kAnnual: result := 'annual';
    kBiennial: result := 'biennial';
    kPerennial: result := 'perennial';
    else result := '';
    end;
  end;

function labelForVernReq(aType: smallint): string;
  begin
  result := '';
  case aType of
    kNoVernalization: result := 'none';
    kObligateVernalization: result := 'obligate';
    kQuantitativeVernalization: result := 'quantitative';
    else result := '';
    end;
  end;

procedure Plant_StreamUsingTextFilerVersionOne(plant: GsPlant; textFiler: GsTextFiler);
  begin
  with plant, textFiler do
    begin
    { type }
    streamSmallint(possibleSize, 'possibleSize');
    streamSmallintWithLabel(params.lifeCycleType, labelForLifeCycleType(params.lifeCycleType), 'lifeCycleType');
    streamBoolean(params.isLegume, 'isLegume');
    streamBoolean(params.isTree, 'isTree');
    streamSingle(params.yearsToMaturityIfTree, 'yearsToMaturityIfTree');
    { planting and germination }
    streamSingle(params.seedWeight_g, 'seedWeight_g');
    streamSingle(params.plantingDepth_mm, 'plantingDepth_mm');
    streamSingle(params.probOfGerminationAfterFirstYear_frn, 'probOfGerminationAfterFirstYear_frn');
    streamSingle(params.minSoilWaterInPlowDepthForGermination_mm, 'minSoilWaterInPlowDepthForGermination_mm');
    streamSingle(params.areaOfSoilPatchInWhichPlanted_ha, 'areaOfSoilPatchInWhichPlanted_ha');
    { transpiration }
    streamSingle(params.thresholdVaporPressureDeficit_kPa, 'thresholdVaporPressureDeficit_kPa');
    streamSingle(params.leafResistIfVPDBelowThreshold_mPsec, 'leafResistIfVPDBelowThreshold_mPsec');
    streamSingleVarSCurve(params.fractionOfMaxLeafConductForHighVPD, 'fractionOfMaxLeafConductForHighVPD');
    streamSingle(params.canopyResistParam, 'canopyResistParam');
    { photosynthesis and growth }
    streamSmallint(params.plantingSpringFallOrBoth, 'plantingSpringFallOrBoth');
    streamSmallint(params.plantingDaysAfterLastSpringFrost_days, 'plantingDaysAfterLastSpringFrost_days');
    streamSmallint(params.maturityDaysBeforeFirstFallFrost_days, 'maturityDaysBeforeFirstFallFrost_days');
    streamSmallint(params.plantingDaysFromSeedToGerminatedSeed_days, 'plantingDaysFromSeedToGerminatedSeed_days');
    streamSmallint(params.plantingDaysFromGerminatedSeedToSeasonMaturity_days,
        'plantingDaysFromGerminatedSeedToSeasonMaturity_days');
    streamSmallint(params.daysToGrowOptimallyAfterPlanting_days,'daysToGrowOptimallyAfterPlanting_days');
    streamSingle(params.minPossibleHeatUnitsBeforeGerminationInAnyClimate_degC,
      'minPossibleHeatUnitsBeforeGerminationInAnyClimate_degC');
    streamSingle(params.maxPossibleHeatUnitsBeforeGerminationInAnyClimate_degC,
      'maxPossibleHeatUnitsBeforeGerminationInAnyClimate_degC');
    streamSingle(params.minPossibleHeatUnitsBeforeMaturityInAnyClimate_degC,
      'minPossibleHeatUnitsBeforeMaturityInAnyClimate_degC');
    streamSingle(params.maxPossibleHeatUnitsBeforeMaturityInAnyClimate_degC,
      'maxPossibleHeatUnitsBeforeMaturityInAnyClimate_degC');
    streamSingle(params.biomassToEnergyRatio_kgPhaPMJ, 'biomassToEnergyRatio_kgPhaPMJ');
    streamSingle(params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ, 'biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ');
    streamSCurve(params.biomassToEnergyRatioCO2Params, 'biomassToEnergyRatioCO2Params');
    streamSingle(params.biomassToEnergyRatioVPDParam, 'biomassToEnergyRatioVPDParam');
    streamSingle(params.biomassAdjustmentIfLAIDecliningFactor, 'biomassAdjustmentIfLAIDecliningFactor');
    { leaf area index }
    streamSCurve(params.heatUnitFactorParamsForLAI, 'heatUnitFactorParamsForLAI');
    streamSingle(params.maxLeafAreaIndex, 'maxLeafAreaIndex');
    streamSingle(params.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn,
        'fractionOfGrowingSeasonWhenLeafDeclineStarts_frn');
    streamSingle(params.leafAreaIndexDeclineRateFactor, 'leafAreaIndexDeclineRateFactor');
    { stress tolerance }
    streamSCurve(params.frostReductionFactorParams, 'frostReductionFactorParams');
    streamSingle(params.optimalTemp_degC, 'optimalTemp_degC');
    streamSingle(params.baseTemp_degC, 'baseTemp_degC');
    streamSingle(params.absoluteTempForFrostKill_degC, 'absoluteTempForFrostKill_degC');
    streamSingle(params.hoursAboveMinDayLengthWhenWinterGrowthCanOccur_hr,
      'hoursAboveMinDayLengthWhenWinterGrowthCanOccur_hr');
    streamSingle(params.aluminumTolerance_pct, 'aluminumTolerance_pct');
    streamSingle(params.criticalAerationFactor_frn, 'criticalAerationFactor_frn');
    streamSCurve(params.rootGrowthRestrictionByAerationStress, 'rootGrowthRestrictionByAerationStress');
    streamSCurve(params.rootGrowthRestrictionByRockCoeffs, 'rootGrowthRestrictionByRockCoeffs');
    streamSingle(params.rootGrowthRestrictionInSandySoilParam, 'rootGrowthRestrictionInSandySoilParam');
    { size }
    streamSingle(params.maxHeightUnsupported_m, 'maxHeightUnsupported_m');
    streamSingle(params.maxHeightSupported_m, 'maxHeightSupported_m');
    streamSingle(params.maxDiameterUnsupported_m, 'maxDiameterUnsupported_m');
    streamSingle(params.maxDiameterSupported_m, 'maxDiameterSupported_m');
    streamSingle(params.maxRootDepth_m, 'maxRootDepth_m');
    streamSingle(params.maxRootSystemDiameter_m, 'maxRootSystemDiameter_m');
    { erosion }
    streamSingle(params.minCropManagementFactor, 'minCropManagementFactor');
    streamSingle(params.windErosionFactorStandingLive, 'windErosionFactorStandingLive');
    streamSingle(params.windErosionFactorStandingDead, 'windErosionFactorStandingDead');
    streamSingle(params.windErosionFactorFlatResidue, 'windErosionFactorFlatResidue');
    { roots and storage organ }
    streamSingle(params.fractionRootWtAtEmergence_frn, 'fractionRootWtAtEmergence_frn');
    streamSingle(params.fractionRootWtAtMaturity_frn, 'fractionRootWtAtMaturity_frn');
    streamSingle(params.fractionStorageOrganAllocationAtMaturity_frn, 'fractionStorageOrganAllocationAtMaturity_frn');
    streamSingle(params.heatUnitIndexAtStartOfStorageOrganAllocation, 'heatUnitIndexAtStartOfStorageOrganAllocation');
    { nutrients }
    streamSCurve(params.plantNAndPStressCoeffs, 'plantNAndPStressCoeffs');
    streamSCurve(params.optimalNParams, 'optimalNParams');
    streamSingle(params.nFractionAtEmergence_frn, 'nFractionAtEmergence_frn');
    streamSingle(params.nFractionAtHalfMaturity_frn, 'nFractionAtHalfMaturity_frn');
    streamSingle(params.nFractionAtMaturity_frn, 'nFractionAtMaturity_frn');
    streamSingle(params.nFixationParam_frn, 'nFixationParam_frn');
    streamSingle(params.maxAnnualNFert_kgPha, 'maxAnnualNFert_kgPha');
    streamSCurve(params.optimalPParams, 'optimalPParams');
    streamSCurve(params.pUptakeCoeffs, 'pUptakeCoeffs');
    streamSingle(params.pFractionAtEmergence_frn, 'pFractionAtEmergence_frn');
    streamSingle(params.pFractionAtHalfMaturity_frn, 'pFractionAtHalfMaturity_frn');
    streamSingle(params.pFractionAtMaturity_frn, 'pFractionAtMaturity_frn');
    { reproduction }
    streamSingle(params.heatUnitIndexAtEndOfVegetativePhase, 'heatUnitIndexAtEndOfVegetativePhase');
    streamSingle(params.fractionReproductiveAllocationAtMaturity_frn, 'fractionReproductiveAllocationAtMaturity_frn');
    streamSingle(params.reproductiveBiomassDecayAtPlantMaturity, 'reproductiveBiomassDecayAtPlantMaturity');
    streamSingle(params.minTempForFloralInduction_degC, 'minTempForFloralInduction_degC');
    streamSingle(params.optTempForFloralInduction_degC, 'optTempForFloralInduction_degC');
    streamSingle(params.maxTempForFloralInduction_degC, 'maxTempForFloralInduction_degC');
    streamSingle(params.photothermalUnitsRequiredForFloralInduction, 'photothermalUnitsRequiredForFloralInduction');
    streamSCurve(params.floralInductionParams, 'floralInductionParams');
    streamSingle(params.thermalUnitsRequiredForFloralInitiation, 'thermalUnitsRequiredForFloralInitiation');
    streamSmallintWithLabel(params.vernalizationRequirement, labelForVernReq(params.vernalizationRequirement),
        'vernalizationRequirement');
    streamSingle(params.minTempForVernalization_degC, 'minTempForVernalization_degC');
    streamSingle(params.optTempForVernalization_degC, 'optTempForVernalization_degC');
    streamSingle(params.maxTempForVernalization_degC, 'maxTempForVernalization_degC');
    streamSingle(params.thermalUnitsRequiredForVernalization, 'thermalUnitsRequiredForVernalization');
    drawingPlant.streamUsingTextFiler(textFiler);
    streamEndOfLine;
    end;
 end;

end.
 
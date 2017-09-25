unit uasp_wea;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
DO NOT EDIT THIS FILE. It was generated by the tab2asp program from aspects.tab.
uasp_wea: Aspect creation for weather.}

interface

uses uaspects;

procedure Weather_createAspects(aspectManager: GsAspectManager);

implementation

procedure Weather_createAspects(aspectManager: GsAspectManager);
  var aspect: GsAspect;
  begin
  aspect := GsAspect.createWithInfo('kWeatherBarometricPressure_kPa',
      'Barometric pressure',
      5655, 1, 1, true, 4, 
      1, 'stationVars.barometricPressure_kPa', false);
  aspect.addUnitsAndBounds(0, 14, 2, 2, 7, 0.0, 100.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherClimateElevation_m',
      'Climate station elevation',
      5656, 1, 1, false, 1, 
      1, 'params.climateElevation_m', false);
  aspect.addUnitsAndBounds(0, 2, 4, 4, 7, 0.0, 5000.0, 0.0, 5000.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherClimateLatitude_rad',
      'Climate station latitude',
      5657, 1, 1, false, 1, 
      1, 'params.climateLatitude_rad', false);
  aspect.addUnitsAndBounds(0, 4, 1, 2, 2, -1.57, 1.57, -1.57, 1.57, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherCoeffForWetDryProbsGivenNumWetDays_frn',
      'Coefficient for wet-dry probabilities given the number of wet days',
      5658, 1, 1, false, 1, 
      1, 'params.coeffForWetDryProbsGivenNumWetDays_frn', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherCoeffRainfallModExpDist',
      'Coefficient for rainfall modified exponential distribution',
      5659, 1, 1, false, 1, 
      1, 'params.coeffRainfallModExpDist', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 100.0, 0.0, 100.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherCorrelationMultiplier',
      'Correlation multiplier for weather autocorrelation today',
      5660, 1, 8, true, 4, 
      1, 'matrices.correlationMultiplier[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanFromMonthlyMeanMaxTemp_degC',
      'Smoothed mean maximum temperature today',
      5661, 1, 1, true, 4, 
      1, 'dailyFromMonthlyMeans.maxTemp_degC', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanFromMonthlyMeanMinTemp_degC',
      'Smoothed mean minimum temperature today',
      5662, 1, 1, true, 4, 
      1, 'dailyFromMonthlyMeans.minTemp_degC', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanFromMonthlyMeanProbWetDay_frn',
      'Smoothed mean probability of wet day today',
      5663, 1, 1, true, 4, 
      1, 'dailyFromMonthlyMeans.probWetDay_frn', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanFromMonthlyMeanRadiation_MJPm2',
      'Smoothed mean radiation today',
      5664, 1, 1, true, 4, 
      1, 'dailyFromMonthlyMeans.radiation_MJPm2', false);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanFromMonthlyMeanRelHum_frn',
      'Smoothed mean relative humidity today',
      5665, 1, 1, true, 4, 
      1, 'dailyFromMonthlyMeans.relHum_frn', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanFromMonthlyMeanWindSpeed_mPsec',
      'Smoothed mean wind speed today',
      5666, 1, 1, true, 4, 
      1, 'dailyFromMonthlyMeans.windSpeed_mPsec', false);
  aspect.addUnitsAndBounds(0, 13, 1, 1, 3, 0.0, 100.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanMaxTempForMonth_degC',
      'Mean daily maximum temperature by month',
      5667, 1, 3, false, 1, 
      1, 'params.dailyMeanMaxTempForMonth_degC[index]', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -10.0, 42.0, -10.0, 42.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanMinTempForMonth_degC',
      'Mean daily minimum temperature by month',
      5668, 1, 3, false, 1, 
      1, 'params.dailyMeanMinTempForMonth_degC[index]', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 30.0, -30.0, 30.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanRadiationForMonth_MJPm2',
      'Mean daily radiation by month',
      5669, 1, 3, false, 1, 
      1, 'params.dailyMeanRadiationForMonth_MJPm2[index]', false);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 750.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanRainfallForMonth_mm',
      'Mean daily precipitation by month',
      5670, 1, 3, true, 4, 
      1, 'stationVars.dailyMeanRainfallForMonth_mm[index]', false);
  aspect.addUnitsAndBounds(0, 12, 1, 1, 2, 0.0, 500.0, 0.0, 500.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanRelHumForMonth_frn',
      'Mean daily relative humidity by month',
      5671, 1, 3, false, 1, 
      1, 'params.dailyMeanRelHumForMonth_frn[index]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDailyMeanWindSpeedForMonth_mPsec',
      'Mean daily wind speed by month',
      5672, 1, 3, false, 1, 
      1, 'params.dailyMeanWindSpeedForMonth_mPsec[index]', false);
  aspect.addUnitsAndBounds(0, 13, 1, 1, 3, 0.5, 10.0, 0.5, 10.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDayLength_hr',
      'Day length today',
      5673, 1, 1, true, 4, 
      1, 'dailyWeather.dayLength_hr', false);
  aspect.addUnitsAndBounds(0, 24, 1, 1, 1, 0.0, 24.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherDeclinationAngleOfSun_rad',
      'Declination angle of the sun today',
      5674, 1, 1, true, 4, 
      1, 'dailyWeather.declinationAngleOfSun_rad', false);
  aspect.addUnitsAndBounds(0, 4, 1, 2, 2, 0.0, 6.28, 0.0, 6.28, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherEpsilonRandomNumberMatrix',
      'Epsilon random number matrix for weather autocorrelation today',
      5675, 1, 8, true, 4, 
      1, 'matrices.epsilonRandomNumberMatrix[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherItRainedYesterday',
      'It rained or snowed yesterday',
      5676, 8, 1, true, 4, 
      1, 'dailyWeather.itRainedYesterday', false);
  aspect.addUnitsAndBounds(0, 30, 1, 1, 1, 0.0, 0.0, 0.0, 0.0, 0);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherJulianDayOfFirstFallFrost',
      'Day of first fall frost (days from January 1)',
      5677, 2, 1, true, 1, 
      1, 'stationVars.julianDayOfFirstFallFrost', false);
  aspect.addUnitsAndBounds(0, 23, 1, 1, 1, 0.0, 365.0, 0.0, 365.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherJulianDayOfLastSpringFrost',
      'Day of last spring frost (days from January 1)',
      5678, 2, 1, true, 1, 
      1, 'stationVars.julianDayOfLastSpringFrost', false);
  aspect.addUnitsAndBounds(0, 23, 1, 1, 1, 0.0, 365.0, 0.0, 365.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLastEpsilonRandomNumber',
      'Last epsilon random number',
      5679, 1, 1, true, 4, 
      1, 'matrices.lastEpsilonRandomNumber', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLatentHeatOfVaporization_MJPkg',
      'Latent heat of vaporization today',
      5680, 1, 1, true, 4, 
      1, 'dailyWeather.latentHeatOfVaporization_MJPkg', false);
  aspect.addUnitsAndBounds(0, 19, 1, 1, 1, 0.0, 100.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLongTermMaxTempAdjFactor',
      'Factor to normalize maximum temperature over simulation years',
      5681, 1, 3, true, 4, 
      1, 'longTermFactors.maxTempAdjFactorForMonth[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLongTermMinTempAdjFactor',
      'Factor to normalize minimum temperature over simulation years',
      5682, 1, 3, true, 4, 
      1, 'longTermFactors.minTempAdjFactorForMonth[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLongTermRainfallAdjFactor',
      'Factor to normalize rainfall over simulation years',
      5683, 1, 3, true, 4, 
      1, 'longTermFactors.rainfallAdjFactorForMonth[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLongTermTotalMaxTempForMonth_degC',
      'Total maximum temperature since weather creation by month',
      5684, 1, 3, true, 4, 
      1, 'longTermFactors.maxTempTotalForMonth_degC[index]', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLongTermTotalMinTempForMonth_degC',
      'Total minimum temperature since weather creation by month',
      5685, 1, 3, true, 4, 
      1, 'longTermFactors.minTempTotalForMonth_degC[index]', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherLongTermTotalTotalRainfallForMonth_mm',
      'Total rainfall since weather creation by month',
      5686, 1, 3, true, 4, 
      1, 'longTermFactors.rainfallTotalForMonth_mm[index]', false);
  aspect.addUnitsAndBounds(0, 12, 1, 1, 2, 0.0, 10000.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMatrixAResidual',
      'Matrix A residual for weather autocorrelation today',
      5687, 1, 8, true, 4, 
      1, 'matrices.matrixAResidual[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMatrixBResidual',
      'Matrix B residual for weather autocorrelation today',
      5688, 1, 8, true, 4, 
      1, 'matrices.matrixBResidual[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMaxPossibleRadiation_MJPm2',
      'Maximum possible radiation (no clouds) today',
      5689, 1, 1, true, 4, 
      1, 'dailyWeather.maxPossibleRadiation_MJPm2', true);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMaxPossibleRadiationForYear_MJPm2',
      'Maximum possible radiation for year',
      5690, 1, 1, true, 4, 
      1, 'stationVars.maxPossibleRadiationForYear_MJPm2', true);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMaxTempForDay_degC',
      'Maximum temperature today',
      5691, 1, 1, true, 4, 
      1, 'dailyWeather.maxTempForDay_degC', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanMaxTempForDay_degC',
      'Mean maximum temperature today',
      5692, 1, 1, true, 4, 
      1, 'dailyMeans.maxTemp_degC', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanPropRainInFirstHalfHourForMonth_frn',
      'Mean proportion of rain falls in first half hour by month',
      5693, 1, 3, false, 1, 
      1, 'params.meanPropRainInFirstHalfHourForMonth_frn[index]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 125.0, 0.0, 125.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanRadiationForDay_MJPm2',
      'Mean radiation today',
      5694, 1, 1, true, 4, 
      1, 'dailyMeans.radiation_MJPm2', false);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanRelHumForDay_frn',
      'Mean relative humidity today',
      5695, 1, 1, true, 4, 
      1, 'dailyMeans.relHum_frn', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanTempForDay_degC',
      'Mean temperature today',
      5696, 1, 1, true, 4, 
      1, 'dailyWeather.meanTempForDay_degC', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanTotalRainfallForMonth_mm',
      'Mean total precipitation by month',
      5697, 1, 3, false, 1, 
      1, 'params.meanTotalRainfallForMonth_mm[index]', true);
  aspect.addUnitsAndBounds(0, 12, 1, 1, 2, 0.0, 500.0, 0.0, 500.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMeanWindSpeedForDay_mPsec',
      'Wind speed today',
      5698, 1, 1, true, 4, 
      1, 'dailyWeather.meanWindSpeedForDay_mPsec', false);
  aspect.addUnitsAndBounds(0, 13, 1, 1, 3, 0.0, 100.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMinDayLengthForYear_hr',
      'Minimum day length for the year',
      5699, 1, 1, true, 4, 
      1, 'stationVars.minDayLengthForYear_hr', false);
  aspect.addUnitsAndBounds(0, 24, 1, 1, 1, 0.0, 24.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherMinTempForDay_degC',
      'Minimum temperature today',
      5700, 1, 1, true, 4, 
      1, 'dailyWeather.minTempForDay_degC', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, -30.0, 40.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherNetOutgoingRadiationIfDayIsClear_MJPm2',
      'Net outgoing radiation if day is clear today',
      5701, 1, 1, true, 4, 
      1, 'dailyWeather.netOutgoingRadiationIfDayIsClear_MJPm2', false);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherNumWetDaysForMonth',
      'Number of wet days by month',
      5702, 1, 3, false, 1, 
      1, 'params.numWetDaysForMonth[index]', false);
  aspect.addUnitsAndBounds(0, 23, 1, 1, 1, 0.0, 31.0, 0.0, 31.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherParamForModifiedExpWindSpeed',
      'Parameter for modified exponential wind speed distribution',
      5703, 1, 1, false, 1, 
      1, 'params.paramForModifiedExpWindSpeed', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 0.6, 0.0, 0.6, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherProbWetDayAfterDryDayForMonth_frn',
      'Probability of wet day after dry day by month',
      5704, 1, 3, false, 1, 
      1, 'params.probWetDayAfterDryDayForMonth_frn[index]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 0.95, 0.0, 0.95, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherProbWetDayAfterWetDayForMonth_frn',
      'Probability of wet day after wet day by month',
      5705, 1, 3, false, 1, 
      1, 'params.probWetDayAfterWetDayForMonth_frn[index]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 0.95, 0.0, 0.95, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherProbWetDayForMonth_frn',
      'Probability of wet day by month',
      5706, 1, 3, true, 4, 
      1, 'stationVars.probWetDayForMonth_frn[index]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherPsychrometerConstant_kPaPdegC',
      'Psychrometric constant',
      5707, 1, 1, true, 4, 
      1, 'stationVars.psychrometerConstant_kPaPdegC', false);
  aspect.addUnitsAndBounds(0, 18, 1, 1, 1, 0.0, 100.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherRadiationForDay_MJPm2',
      'Radiation today',
      5708, 1, 1, true, 4, 
      1, 'dailyWeather.radiationForDay_MJPm2', false);
  aspect.addUnitsAndBounds(0, 10, 1, 1, 7, 0.0, 50.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherRainfallForDay_mm',
      'Precipitation (rain or snow) today',
      5709, 1, 1, true, 4, 
      1, 'dailyWeather.rainfallForDay_mm', false);
  aspect.addUnitsAndBounds(0, 12, 1, 1, 2, 0.0, 100.0, 0.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherRainfallNormalizingFactor',
      'Factor to normalize skewed normal rainfall distribution',
      5710, 1, 3, false, 4, 
      1, 'stationVars.rainfallNormalizingFactorForMonth[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherRelHumForDay_frn',
      'Relative humidity today',
      5711, 1, 1, true, 4, 
      1, 'dailyWeather.relHumForDay_frn', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherSkewCoeffForRainfallForMonth',
      'Skew coefficient for rainfall by month',
      5712, 1, 3, false, 1, 
      1, 'params.skewCoeffForRainfallForMonth[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 7.0, 0.0, 7.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherSlopeSaturVaporPressureCurve_kPaPdegC',
      'Slope of the saturation vapor pressure curve',
      5713, 1, 1, true, 4, 
      1, 'dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC', false);
  aspect.addUnitsAndBounds(0, 18, 1, 1, 1, 0.0, 100.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherStdDevDailyRainfallForMonth_mm',
      'Standard deviation of daily rainfall by month',
      5714, 1, 3, false, 1, 
      1, 'params.stdDevDailyRainfallForMonth_mm[index]', false);
  aspect.addUnitsAndBounds(0, 12, 1, 1, 2, 0.0, 50.0, 0.0, 50.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherStdDevMaxTempForMonth_degC',
      'Standard deviation of maximum temperature by month',
      5715, 1, 3, false, 1, 
      1, 'params.stdDevMaxTempForMonth_degC[index]', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, 0.0, 15.0, 0.0, 15.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherStdDevMinTempForMonth_degC',
      'Standard deviation of minimum temperature by month',
      5716, 1, 3, false, 1, 
      1, 'params.stdDevMinTempForMonth_degC[index]', false);
  aspect.addUnitsAndBounds(0, 5, 1, 1, 2, 0.0, 15.0, 0.0, 15.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherVaporPressureDeficit_kPa',
      'Vapor pressure deficit today',
      5717, 1, 1, true, 4, 
      1, 'dailyWeather.vaporPressureDeficit_kPa', false);
  aspect.addUnitsAndBounds(0, 14, 2, 2, 7, 0.0, 100.0, -1000000.0, 1000000.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionForDay_rad',
      'Wind direction today',
      5718, 1, 1, true, 4, 
      1, 'dailyWeather.windDirectionForDay_rad', false);
  aspect.addUnitsAndBounds(0, 4, 1, 2, 2, 0.0, 6.28, 0.0, 6.28, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_E',
      'Fraction of wind from E by month',
      5719, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][4]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_ENE',
      'Fraction of wind from ENE by month',
      5720, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][3]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_ESE',
      'Fraction of wind from ESE by month',
      5721, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][5]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_N',
      'Fraction of wind from N by month',
      5722, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][0]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_NE',
      'Fraction of wind from NE by month',
      5723, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][2]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_NNE',
      'Fraction of wind from NNE by month',
      5724, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][1]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_NNW',
      'Fraction of wind from NNW by month',
      5725, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][15]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_NW',
      'Fraction of wind from NW by month',
      5726, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][14]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_S',
      'Fraction of wind from S by month',
      5727, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][8]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_SE',
      'Fraction of wind from SE by month',
      5728, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][6]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_SSE',
      'Fraction of wind from SSE by month',
      5729, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][7]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_SSW',
      'Fraction of wind from SSW by month',
      5730, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][9]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_SW',
      'Fraction of wind from SW',
      5731, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][10]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_W',
      'Fraction of wind from W by month',
      5732, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][12]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_WNW',
      'Fraction of wind from WNW by month',
      5733, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][13]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherWindDirectionsForMonth_frn_WSW',
      'Fraction of wind from WSW by month',
      5734, 1, 3, false, 1, 
      1, 'params.windDirectionsForMonth_frn[index][11]', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, 0.0, 1.0, 3);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherYearsMaxMonthlyHalfHourRainfallRecord_yr',
      'Years of maximum half-hour rainfall record',
      5735, 1, 1, false, 1, 
      1, 'params.yearsMaxMonthlyHalfHourRainfallRecord_yr', false);
  aspect.addUnitsAndBounds(0, 22, 1, 1, 1, 0.0, 10.0, 0.0, 10.0, 1);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherYesterdaysCorrelationMultiplier',
      'Yesterday''s correlation multiplier',
      5736, 1, 8, true, 4, 
      1, 'matrices.yesterdaysCorrelationMultiplier[index]', false);
  aspect.addUnitsAndBounds(0, 1, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
  aspect := GsAspect.createWithInfo('kWeatherYesterdaysUniformRandNumForRainfall_frn',
      'Yesterday''s uniform random number for rainfall',
      5737, 1, 1, true, 4, 
      1, 'matrices.yesterdaysUniformRandNumForRainfall_frn', false);
  aspect.addUnitsAndBounds(0, 26, 1, 1, 1, 0.0, 1.0, -1000000.0, 1000000.0, 2);
  aspectManager.addAspect(aspect);
    end;

end.


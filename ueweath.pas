unit ueweath;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uweath: Weather object. Information in the weather is in several record structures
as specified in uestruct. Monthly parameters are used to create daily means,
and daily values are generated based on the daily means. The weather also
keeps 30 days of recent weather vars for displaying in the browser pictures
side. Read uestruct to understand the structures here before reading this code.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses ufiler, umodel, uestruct, ucollect, udate, ufilertx, urandom;

type
GsWeather = class(GsGraphicalModel)
  public
  garden: GsModel;
  options: weatherOptionsArray;
  params: WeatherParamsStructure;
  stationVars: WeatherStationVarsStructure;
  longTermFactors: LongTermFactorsStructure;
  dailyFromMonthlyMeans: DailyFromMonthlyMeansStructure;
  dailyMeans: DailyMeansStructure;
  matrices: MatricesStructure;
  dailyWeather: DailyWeatherStructure;
  randomNumberGenerator: GsRandom;
  { history }
  historyNumDays: smallint;
  historyDailyWeather: HistoryDailyWeatherStructure;
  historyDailyFromMonthlyMeans: HistoryDailyFromMonthlyMeansStructure;
  lastDayOfHistoryHasBeenFilledAtLeastOnce: boolean;
  { creation/destruction }
  constructor create; override;
  constructor createWithGarden(aGarden: GsModel);
  destructor destroy; override;
  { streaming }
  procedure copyFromClimate(climate: GsWeather);
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  { transfer }
  procedure directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  { next day }
  procedure WeatherNextDay;
  { the functions called by next day }
  procedure MakeSureWindDirectionsAddUpToOne;
  procedure updateHistory;
  procedure ZeroWeatherDailyVars;
  procedure SmoothDailyMeansFromMonthlyMeans(date: GsDate);
  procedure MaxTempRelHumAndRadMeans;
  procedure AutoCorrelatedMatrix;
  procedure RainfallForDay(month: integer);
  procedure MinAndMaxTemperaturesForDay(month: integer);
  procedure RadiationForDay(theDay: integer);
  procedure RelativeHumidityForDay;
  procedure MeanWindSpeedForDay(month: integer);
  procedure WindDirectionForDay(month: integer);
  procedure SummaryCalculationsForDay;
  procedure AccumulateLongTermRainfallAndMaxAndMinTemp(month: integer);
  procedure CalculateMaxPossibleRadiationForYear;
  function getOption(index: smallint): smallint;
  { the rest of the functions }
  procedure RecalculateLongTermAdjustmentFactors;
  function GetProbWetDayForMonth_frn(month: integer): single;
  function GetMeanMonthlyMeanMeanTempForYear_degC: single;
  function GetDailyMeanWindSpeedForMonth_mPsec(month: integer): single;
  function GetMeanPropRainInFirstHalfHourForMonth_frn(month: integer): single;
  function GetYearTotalMonthlyMeanTotalRainfall_mm: single;
  function GetMeanTempForMonth_degC(month: integer): single;
  procedure ZeroWeatherParams;
  procedure ZeroWeatherCumulativeVars;
	function objectType: integer; override;
  procedure initialize;
  procedure fillPrevThreeDaysMeanTempArray_degC(prevThreeDaysMeanTemp_degC: arrayThree);
  function middayOfMonthWithLowestMeanMinTempForYearBelow5degC: single;
  procedure calculateFrostDates;
  procedure calculateJulianDayOfLastSpringFrost;
  procedure calculateJulianDayOfFirstFallFrost;
  function monthWithWarmestMinTemp: smallint;
  function AccumulateHeatUnitsForNumberOfDays(startDay, numDays: smallint;
      baseTemp_degC, optimalTemp_degC, minDayLengthForWinterGrowth_hr: single): single;
  function getHint(longHint: boolean; metricUnits: boolean): string;
  { side effect functions }
  procedure sideEffects_climateLatitude_rad(updateList: TListCollection);
  procedure sideEffects_climateElevation_m(updateList: TListCollection);
  procedure sideEffects_paramForModifiedExpWindSpeed(oldValue: single; updateList: TListCollection);
  procedure sideEffects_dailyMeanMinTempForMonth_degC(month: smallint; updateList: TListCollection);
  procedure sideEffects_numWetDaysOrMeanTotalRainfallForMonth(month: smallint; updateList: TListCollection);
  end;

implementation

uses ueutils, ueq, ueqh, uep, uegarden, umconsts, SysUtils, udebug, uaspects, uclasses, udefault, uunits, uhardcd,
  usupport, utstream;

const
  aMatrix: matrixType = ((0.594, 0.454,  -0.004), (0.076, 0.261,  -0.037), (-0.018,  -0.129, 0.222));
  bMatrix: matrixType = ((0.767, 0.1304, 0.274),  (0.0, 0.692,  -0.33),    (0.0, 0.0, 0.873));

{ ------------------------------------------------------------------------------------------ next day }
procedure GsWeather.WeatherNextDay;
  var month, theDay: integer;
  begin
  month := GsDate_monthFromDate(GsGarden(garden).date);
  theDay := GsGarden(garden).julianDayToday;
  CalculateFrostDates;
  MakeSureWindDirectionsAddUpToOne;
  CalculateMaxPossibleRadiationForYear;
  SmoothDailyMeansFromMonthlyMeans(GsGarden(garden).date);
  MaxTempRelHumAndRadMeans;
  AutoCorrelatedMatrix;
  RainfallForDay(month);
  MinAndMaxTemperaturesForDay(month);
  RadiationForDay(theDay);
  RelativeHumidityForDay;
  MeanWindSpeedForDay(month);
  WindDirectionForDay(month);
  SummaryCalculationsForDay;
  AccumulateLongTermRainfallAndMaxAndMinTemp(month);
  UpdateHistory;
  end;

{ ------------------------------------------------------------------------------------------ create/destroy }
constructor GsWeather.create;
  var i: smallint;
	begin
  inherited create;
  RandomNumberGenerator := GsRandom.create;
  randomNumberGenerator.randomizeFromTime;
  for i := 0 to 100 do randomNumberGenerator.zeroToOne;
  end;

constructor GsWeather.createWithGarden(aGarden: GsModel);
  begin
  self.create;
  garden := aGarden;
  end;

procedure GsWeather.copyFromClimate(climate: GsWeather);
	begin
  if climate = nil then exit;
  climate.copyTo(self);
  self.initialize;
  self.isTemplate := false;
  self.setName('weather: ' + climate.getName);
  end;

procedure GsWeather.initialize;
  var month: smallint;
  begin
  historyNumDays := 0;
  { things that are calculated only once }
  stationVars.barometricPressure_kPa := EQH.BarometricPressure_kPa(params.climateElevation_m);
  stationVars.psychrometerConstant_kPaPdegC := EQH.PsychrometerConstant_kPaPdegC(stationVars.barometricPressure_kPa);
  { starting random numbers }
  matrices.lastEpsilonRandomNumber := Utils_RandomZeroToOne;
  matrices.yesterdaysUniformRandNumForRainfall_frn := Utils_RandomZeroToOne;
  lastDayOfHistoryHasBeenFilledAtLeastOnce := false;
  for month := 0 to 11 do
    longTermFactors.rainfallAdjFactorForMonth[month] := 1.0; {multiplied, so must start at 1.0}
  self.calculateFrostDates;
  longTermFactors.yearsOfLongTermWeatherDataForAdjustment := 0;
  longTermFactors.daysOfLongTermWeatherDataForAdjustment := 0;
  end;

destructor GsWeather.destroy;
  begin
  RandomNumberGenerator.free;
  RandomNumberGenerator := nil;
  inherited destroy;
  end;

{ ------------------------------------------------------------------------------------------ graphics }
function GsWeather.getHint(longHint: boolean; metricUnits: boolean): string;
  var
    maxTemp, minTemp, rainfall: single;
  begin
  result := '';
  if not longHint then exit;
  result := self.getName;
  if not metricUnits then
    begin
    minTemp := Convert(kTemperature, kTemperatureDegreesC, kTemperatureDegreesF, dailyWeather.minTempForDay_degC);
    maxTemp := Convert(kTemperature, kTemperatureDegreesC, kTemperatureDegreesF, dailyWeather.maxTempForDay_degC);
    end
  else
    begin
    minTemp := dailyWeather.minTempForDay_degC;
    maxTemp := dailyWeather.maxTempForDay_degC;
    end;
  result := result + '. Low/high temperature ' + intToStr(trunc(minTemp)) + '/' + intToStr(trunc(maxTemp));
  if not metricUnits then
    result := result + ' degrees F'
  else
    result := result + ' degrees C';
  if dailyWeather.rainfallForDay_mm > 0 then
    begin
    if not metricUnits then
      rainfall := Convert(kDepthOfWater, kDepthOfWaterMillimeters, kDepthOfWaterInches,
          dailyWeather.rainfallForDay_mm)
    else
      rainfall := dailyWeather.rainfallForDay_mm;
    if dailyWeather.meanTempForDay_degC > 0.0 then
      result := result + ', rain ' + digitValueString(rainfall)
    else
      result := result + ', snow ' + digitValueString(rainfall);
    if not metricUnits then
      result := result + ' inches'
    else
      result := result + ' mm';
    end;
  result := result + ', radiation ' + digitValueString(dailyWeather.radiationForDay_MJPm2) + ' MJ/m2';
  result := result + ', daylength ' + digitValueString(dailyWeather.dayLength_hr) + ' hours';
  result := result + ', wind speed ' + digitValueString(dailyWeather.meanWindSpeedForDay_mPsec) + ' m/sec.';
  end;

{ ------------------------------------------------------------------------------------------ called by next day }
procedure GsWeather.CalculateMaxPossibleRadiationForYear;
  var
    firstDay, secondDay: integer;
    declinationAngle_rad, firstMaxRadiation_MJPm2, secondMaxRadiation_MJPm2, latitude_rad: single;
  begin
  { really only have to do CalculateMaxPossibleRadiationForYear once for any latitude,
    but doing it every day because plants might need
    it at any time, and this weather might not have started at the beginning of a year
    and site latitude may have changed }
  try
  { take max of radiation for two days to allow for southern hemisphere }
  { can't turn this off }
  latitude_rad := params.climateLatitude_rad;
  firstDay := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(22-1, 6-1); {June 22}
  declinationAngle_rad := EQ.SunDeclinationAngle_rad(firstDay);
  firstMaxRadiation_MJPm2 := EP.MaxPossibleRadiation_MJPm2(firstDay, latitude_rad, declinationAngle_rad);
  secondDay := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(22-1, 12-1); {Dec 22}
  declinationAngle_rad := EQ.SunDeclinationAngle_rad(secondDay);
  secondMaxRadiation_MJPm2 := EP.MaxPossibleRadiation_MJPm2(secondDay, latitude_rad, declinationAngle_rad);
  stationVars.maxPossibleRadiationForYear_MJPm2 := max(firstMaxRadiation_MJPm2, secondMaxRadiation_MJPm2);
  except errorMessage('Problem in weather CalculateMaxPossibleRadiationForYear'); end;
  end;

procedure GsWeather.SmoothDailyMeansFromMonthlyMeans(date: GsDate);
  begin
  try
  { change fro EPIC }
  { calc daily means from monthly means. this smooths the jerky monthly changes in mean values }
  { and makes a smoother curve in simulated daily values }
  { can't turn this off }
  with dailyFromMonthlyMeans do
    begin
    maxTemp_degC := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanMaxTempForMonth_degC, date);
    minTemp_degC := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanMinTempForMonth_degC, date);
    relHum_frn := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanRelHumForMonth_frn, date);
    radiation_MJPm2 := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanRadiationForMonth_MJPm2, date);
    probWetDay_frn := GsDate_DailyValueFromMonthlyArray_NoLeap(stationVars.probWetDayForMonth_frn, date);
    windSpeed_mPsec := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanWindSpeedForMonth_mPsec, date);
    end;
  except errorMessage('Problem in weather SmoothDailyMeansFromMonthlyMeans'); end;
  end;

procedure GsWeather.MaxTempRelHumAndRadMeans;
  begin
  try
  { calculate means for the day for the condition of no rain yesterday, then amend if it rained yesterday }
  { don't smooth these if smoothing weather - smoothing occurs in final daily values and skips these }
  with dailyFromMonthlyMeans do
    begin
    { max temp }
    dailyMeans.maxTemp_degC := EQ.DailyMeanMaxTempDryDaysForMonth_degC(probWetDay_frn, maxTemp_degC, minTemp_degC);
    if dailyWeather.itRainedYesterday then
      dailyMeans.maxTemp_degC := EQ.DailyMeanMaxTempWetDaysForMonth_degC(dailyMeans.maxTemp_degC, maxTemp_degC, minTemp_degC);
    { rel hum }
    dailyMeans.relHum_frn := EQ.DailyMeanRelHumDryDaysForMonth_frn(relHum_frn, probWetDay_frn);
    if dailyWeather.itRainedYesterday then
      dailyMeans.relHum_frn := EQ.DailyMeanRelHumWetDaysForMonth_frn(dailyMeans.relHum_frn);
  { radiation }
    dailyMeans.radiation_MJPm2 := EQ.DailyMeanRadiationDryDaysForMonth_MJPm2(radiation_MJPm2, probWetDay_frn);
    if dailyWeather.itRainedYesterday then
      dailyMeans.radiation_MJPm2 := EQ.DailyMeanRadiationWetDaysForMonth_MJPm2(dailyMeans.radiation_MJPm2);
    end;
  except errorMessage('Problem in weather MaxTempRelHumAndRadMeans'); end;
  end;

procedure GsWeather.AutoCorrelatedMatrix;
  var
    i, j: integer;
    epsilonRandomNumber: single;
  begin
  try
  { calculate auto-correlated matrix for day }
  for i := 0 to 2 do 
    begin
    if matrices.lastEpsilonRandomNumber = 0.0 then
      matrices.lastEpsilonRandomNumber := Utils_RandomZeroToOne;
    epsilonRandomNumber := Utils_RandomZeroToOne;
    matrices.epsilonRandomNumberMatrix[i] := Utils_StandardNormalDeviate(matrices.lastEpsilonRandomNumber,
      epsilonRandomNumber);
    matrices.lastEpsilonRandomNumber := epsilonRandomNumber;
    end;
  for i := 0 to 2 do
    begin
    matrices.matrixAResidual[i] := 0.0;
    matrices.matrixBResidual[i] := 0.0;
    for j := 0 to 2 do
      begin
      matrices.matrixAResidual[i] := matrices.matrixAResidual[i] + (aMatrix[i][j] *
        matrices.yesterdaysCorrelationMultiplier[j]);
      matrices.matrixBResidual[i] := matrices.matrixBResidual[i] + (bMatrix[i][j] *
        matrices.epsilonRandomNumberMatrix[j]);
      end;
    end;
  for i := 0 to 2 do 
    begin
    matrices.correlationMultiplier[i] := matrices.matrixAResidual[i] + matrices.matrixBResidual[i];
    matrices.yesterdaysCorrelationMultiplier[i] := matrices.correlationMultiplier[i];
    end;
  except errorMessage('Problem in weather AutoCorrelatedMatrix'); end;
  end;

procedure GsWeather.RainfallForDay(month: integer);
  begin
  try
  case getOption(kWeatherOptionsRainfall) of
    kWeatherOptionNormal:
      begin
      dailyWeather.rainfallForDay_mm := EP.RainfallForDay_mm(month, dailyWeather.itRainedYesterday, params,
        stationVars, matrices.yesterdaysUniformRandNumForRainfall_frn);
      dailyWeather.itRainedYesterday := (dailyWeather.rainfallForDay_mm > 0.0);
      { adjust rainfall with long-term normalizing factors }
      { long term adjustment factors are this month's mean / mean over all the years of simulation }
      { this makes the weather more consistent over many years of simulation. }
      dailyWeather.rainfallForDay_mm := dailyWeather.rainfallForDay_mm * longTermFactors.rainfallAdjFactorForMonth[month];
      { prevent extreme outlier by simply cutting off precipitation above an arbitrary limit of 500 mm
        very large values cause all sorts of problems in runoff etc }
      dailyWeather.rainfallForDay_mm := min(500.0, dailyWeather.rainfallForDay_mm);
      end;
    kWeatherOptionDisabled: dailyWeather.rainfallForDay_mm := 0.0;
    kWeatherOptionSmoothed:
      { daily from monthly value not automatically calculated for this var, so have to do it here if smoothing }
      dailyWeather.rainfallForDay_mm := GsDate_DailyValueFromMonthlyArray_NoLeap(
          stationVars.dailyMeanRainfallForMonth_mm, GsGarden(garden).date);
    kWeatherOptionNoUpdate: ;
    end;
  except errorMessage('Problem in weather RainfallForDay'); end;
  end;

procedure GsWeather.MinAndMaxTemperaturesForDay(month: integer);
  var
    rangeOfTempForDay_degC: single;
  begin
  try
  case getOption(kWeatherOptionsTemperature) of
    kWeatherOptionNormal:
      begin
      dailyWeather.maxTempForDay_degC := EQ.DailyMaxTemperature_degC(dailyMeans.maxTemp_degC,
        params.stdDevMaxTempForMonth_degC[month], matrices.correlationMultiplier[0]);
      dailyWeather.maxTempForDay_degC := dailyWeather.maxTempForDay_degC + longTermFactors.maxTempAdjFactorForMonth[month];
      dailyWeather.minTempForDay_degC := EQ.DailyMinTemperature_degC(params.dailyMeanMinTempForMonth_degC[month],
        params.stdDevMinTempForMonth_degC[month], matrices.correlationMultiplier[1]);
      rangeOfTempForDay_degC := dailyWeather.maxTempForDay_degC - dailyWeather.minTempForDay_degC;
      dailyWeather.minTempForDay_degC := dailyWeather.maxTempForDay_degC
          - longTermFactors.minTempAdjFactorForMonth[month] * rangeOfTempForDay_degC;
      if (dailyWeather.minTempForDay_degC > dailyWeather.maxTempForDay_degC) then
        dailyWeather.minTempForDay_degC := dailyWeather.maxTempForDay_degC - 0.2 * abs(dailyWeather.maxTempForDay_degC);
      end;
    kWeatherOptionDisabled:
      begin
      dailyWeather.maxTempForDay_degC := Convert(kTemperature, kTemperatureDegreesF, kTemperatureDegreesC, 40.0);
      dailyWeather.minTempForDay_degC := Convert(kTemperature, kTemperatureDegreesF, kTemperatureDegreesC, 70.0);
      end;
    kWeatherOptionSmoothed:
      begin
      dailyWeather.maxTempForDay_degC := dailyFromMonthlyMeans.maxTemp_degC;
      dailyWeather.minTempForDay_degC := dailyFromMonthlyMeans.minTemp_degC;
      end;
    kWeatherOptionNoUpdate: ;
    end;
  { calculate mean temp }
  dailyWeather.meanTempForDay_degC := (dailyWeather.minTempForDay_degC + dailyWeather.maxTempForDay_degC) / 2.0;
  except errorMessage('Problem in weather MinAndMaxTemperaturesForDay'); end;
  end;

procedure GsWeather.RadiationForDay(theDay: integer);
  begin
  try
  { calculate these in any case }
  dailyWeather.declinationAngleOfSun_rad := EQ.SunDeclinationAngle_rad(theDay);
  dailyWeather.maxPossibleRadiation_MJPm2 := EP.MaxPossibleRadiation_MJPm2(theDay,
      params.climateLatitude_rad, dailyWeather.declinationAngleOfSun_rad);
  case getOption(kWeatherOptionsRadiation) of
    kWeatherOptionNormal:
      dailyWeather.radiationForDay_MJPm2 := EP.RadiationForDay_MJPm2(dailyWeather.maxPossibleRadiation_MJPm2,
          dailyMeans.radiation_MJPm2, matrices.correlationMultiplier[2]);
    kWeatherOptionDisabled: dailyWeather.radiationForDay_MJPm2 := 10.0;
    kWeatherOptionSmoothed: dailyWeather.radiationForDay_MJPm2 := dailyFromMonthlyMeans.radiation_MJPm2;
    kWeatherOptionNoUpdate: ;
    end;
  except errorMessage('Problem in weather RadiationForDay'); end;
  end;

procedure GsWeather.RelativeHumidityForDay;
  begin
  try
  case getOption(kWeatherOptionsRelHum) of
    kWeatherOptionNormal:
      dailyWeather.relHumForDay_frn := EP.RelativeHumidityForDay_frn(dailyMeans.relHum_frn);
    kWeatherOptionDisabled: dailyWeather.relHumForDay_frn := 0.5;
    kWeatherOptionSmoothed: dailyWeather.relHumForDay_frn := dailyFromMonthlyMeans.relHum_frn;
    kWeatherOptionNoUpdate: ;
    end;
  except errorMessage('Problem in weather RelativeHumidityForDay'); end;
  end;

procedure GsWeather.MeanWindSpeedForDay(month: integer);
  begin
  try
  case getOption(kWeatherOptionsWindSpeed) of
    kWeatherOptionNormal:
      dailyWeather.meanWindSpeedForDay_mPsec := EQ.MeanWindSpeedForDay_mPsec(dailyFromMonthlyMeans.windSpeed_mPsec,
        params.paramForModifiedExpWindSpeed);
    kWeatherOptionDisabled: dailyWeather.meanWindSpeedForDay_mPsec := 0.0;
    kWeatherOptionSmoothed: dailyWeather.meanWindSpeedForDay_mPsec := dailyFromMonthlyMeans.windSpeed_mPsec;
    kWeatherOptionNoUpdate: ;
    end;
  except errorMessage('Problem in weather MeanWindSpeedForDay'); end;
  end;

procedure GsWeather.WindDirectionForDay(month: integer);
  begin
  try
  case getOption(kWeatherOptionsWindDirection) of
    kWeatherOptionNormal:
      dailyWeather.windDirectionForDay_rad := EQ.WindDirectionFromNorth_rad(
          params.windDirectionsForMonth_frn, month);
    { to east }
    kWeatherOptionDisabled: dailyWeather.windDirectionForDay_rad := kPi / 2.0;
    { to west }
    kWeatherOptionSmoothed: dailyWeather.windDirectionForDay_rad := 3.0 * kPi / 2.0;
    kWeatherOptionNoUpdate: ;
    end;
  except errorMessage('Problem in weather WindDirectionForDay'); end;
  end;

procedure GsWeather.SummaryCalculationsForDay;
  var
    saturVaporPressureAtMeanTemp_kPa, vaporPressureAtMeanTemp_kPa: single;
  begin
  try
  dailyWeather.latentHeatOfVaporization_MJPkg := EQH.LatentHeatOfVaporization_MJPkg(dailyWeather.meanTempForDay_degC);
  saturVaporPressureAtMeanTemp_kPa := EQH.SaturVaporPressure_kPa(dailyWeather.meanTempForDay_degC);
  vaporPressureAtMeanTemp_kPa := EQH.VaporPressureAtMeanTemp_kPa(saturVaporPressureAtMeanTemp_kPa,
    dailyWeather.relHumForDay_frn);
  dailyWeather.vaporPressureDeficit_kPa := saturVaporPressureAtMeanTemp_kPa - vaporPressureAtMeanTemp_kPa;
  dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC :=
    EQH.SlopeSaturVaporPressureCurve_kPaPdegC(saturVaporPressureAtMeanTemp_kPa, dailyWeather.meanTempForDay_degC);
  dailyWeather.netOutgoingRadiationIfDayIsClear_MJPm2 :=
    EQH.NetOutgoingLongWaveRadiationClearDays_MJPm2(vaporPressureAtMeanTemp_kPa, dailyWeather.meanTempForDay_degC);
  dailyWeather.dayLength_hr := EQ.DayLength_hr(params.climateLatitude_rad, dailyWeather.declinationAngleOfSun_rad);
  except errorMessage('Problem in weather SummaryCalculationsForDay'); end;
  end;

procedure GsWeather.AccumulateLongTermRainfallAndMaxAndMinTemp(month: integer);
  begin
  try
  with longTermFactors do
    begin
    { accumulate long-term total rainfall, min and max temp for means for normalizing factors }
    rainfallTotalForMonth_mm[month] := rainfallTotalForMonth_mm[month] + dailyWeather.rainfallForDay_mm;
    maxTempTotalForMonth_degC[month] := maxTempTotalForMonth_degC[month] + dailyWeather.maxTempForDay_degC;
    minTempTotalForMonth_degC[month] := minTempTotalForMonth_degC[month] + dailyWeather.minTempForDay_degC;
    end;
  if longTermFactors.daysOfLongTermWeatherDataForAdjustment >= 365 then
    self.RecalculateLongTermAdjustmentFactors
  else
    inc(longTermFactors.daysOfLongTermWeatherDataForAdjustment);
  except errorMessage('Problem in weather AccumulateLongTermRainfallAndMaxAndMinTemp'); end;
  end;

procedure GsWeather.UpdateHistory;
  var i: integer;
  begin
  try
  inc(historyNumDays);
  if historyNumDays > kMaxNumDays then historyNumDays := kMaxNumDays;
  if historyNumDays >= kMaxNumDays then
    begin
    if lastDayOfHistoryHasBeenFilledAtLeastOnce then
      begin
      for i := 0 to historyNumDays - 2 do
        begin
        historyDailyWeather[i] := historyDailyWeather[i + 1];
        historyDailyFromMonthlyMeans[i] := historyDailyFromMonthlyMeans[i + 1];
        end;
      end
    else
      lastDayOfHistoryHasBeenFilledAtLeastOnce := true;
    end;
  historyDailyWeather[historyNumDays - 1] := dailyWeather;
  historyDailyFromMonthlyMeans[historyNumDays - 1] := dailyFromMonthlyMeans;
  except errorMessage('Problem in weather UpdateHistory'); end;
  end;

procedure GsWeather.fillPrevThreeDaysMeanTempArray_degC(prevThreeDaysMeanTemp_degC: arrayThree);
  begin
  { historyDailyWeather[historyNumDays] is today, because the weather is calculated before the
    soil patches, which call this }
  { first day is yesterday }
  if historyNumDays >= 2 then
    prevThreeDaysMeanTemp_degC[0] := historyDailyWeather[historyNumDays-2].meanTempForDay_degC
  else
    prevThreeDaysMeanTemp_degC[0] := 0.0;
  { second day is day before yesterday }
  if historyNumDays >= 3 then
    prevThreeDaysMeanTemp_degC[1] := historyDailyWeather[historyNumDays-3].meanTempForDay_degC
  else
    prevThreeDaysMeanTemp_degC[1] := 0.0;
  { third day is day before day before yesterday }
  if historyNumDays >= 4 then
    prevThreeDaysMeanTemp_degC[2] := historyDailyWeather[historyNumDays-4].meanTempForDay_degC
  else
    prevThreeDaysMeanTemp_degC[2] := 0.0;
  end;

procedure GsWeather.RecalculateLongTermAdjustmentFactors;
  var
    month, years: integer;
    longTermMeanMonthlyRainfall_mm, longTermMeanMonthlyMaxTemp_degC, longTermMeanMonthlyMinTemp_degC: single;
    yearsTDaysInMonth: single;
  begin
  try
  longTermMeanMonthlyRainfall_mm := 0.0;
  longTermMeanMonthlyMaxTemp_degC := 0.0;
  longTermMeanMonthlyMinTemp_degC := 0.0;
  yearsTDaysInMonth := 0.0;
  longTermFactors.daysOfLongTermWeatherDataForAdjustment := 0;
  inc(longTermFactors.yearsOfLongTermWeatherDataForAdjustment);
  years := longTermFactors.yearsOfLongTermWeatherDataForAdjustment;
  for month := 0 to 11 do with longTermFactors do
    begin
    { calculate long-term means from totals - ignore leap year here since no real year is used }
    longTermMeanMonthlyRainfall_mm := safediv(rainfallTotalForMonth_mm[month], years);
    yearsTDaysInMonth := years * GsDate_daysInMonthFromMonth_NoLeap(month);
    longTermMeanMonthlyMaxTemp_degC := safediv(maxTempTotalForMonth_degC[month], yearsTDaysInMonth);
    longTermMeanMonthlyMinTemp_degC := safediv(minTempTotalForMonth_degC[month], yearsTDaysInMonth);
    { calculate rainfall adjustment factor - if problem, set at 1.0 because is multiplied }
    rainfallAdjFactorForMonth[month] := safedivExcept(params.meanTotalRainfallForMonth_mm[month],
        longTermMeanMonthlyRainfall_mm, 1.0);
    { calculate min temp rainfall adjustment factor - if problem, set at 1.0 because is multiplied }
    minTempAdjFactorForMonth[month] := safedivExcept(
        longTermMeanMonthlyMaxTemp_degC - params.dailyMeanMinTempForMonth_degC[month],
        longTermMeanMonthlyMaxTemp_degC - longTermMeanMonthlyMinTemp_degC, 1.0);
    { calculate max temp rainfall adjustment factor }
    maxTempAdjFactorForMonth[month] := params.dailyMeanMaxTempForMonth_degC[month] - longTermMeanMonthlyMaxTemp_degC;
    end;
  except errorMessage('Problem in weather RecalculateLongTermAdjustmentFactors'); end;
  end;

{ ------------------------------------------------------------------------------------------ utilities }
function GsWeather.GetProbWetDayForMonth_frn(month: integer): single;
  begin
  result := stationVars.probWetDayForMonth_frn[month];
  end;

function GsWeather.GetMeanMonthlyMeanMeanTempForYear_degC: single;
  var
    month: smallint;
    meanMonthlyMeanMaxTempForYear_degC, meanMonthlyMeanMinTempForYear_degC: single;
  begin
  meanMonthlyMeanMaxTempForYear_degC := 0.0;
  meanMonthlyMeanMinTempForYear_degC := 0.0;
  for month := 0 to 11 do 
    begin
    meanMonthlyMeanMaxTempForYear_degC := meanMonthlyMeanMaxTempForYear_degC + params.dailyMeanMaxTempForMonth_degC[month];
    meanMonthlyMeanMinTempForYear_degC := meanMonthlyMeanMinTempForYear_degC + params.dailyMeanMinTempForMonth_degC[month];
    end;
  meanMonthlyMeanMaxTempForYear_degC := meanMonthlyMeanMaxTempForYear_degC / 12.0;
  meanMonthlyMeanMinTempForYear_degC := meanMonthlyMeanMinTempForYear_degC / 12.0;
  result := (meanMonthlyMeanMaxTempForYear_degC + meanMonthlyMeanMinTempForYear_degC) / 2.0;
  end;

function GsWeather.GetDailyMeanWindSpeedForMonth_mPsec(month: integer): single;
  begin
  { change from EPIC - soil calls this to use in wind erosion equations - changing to use
    smoothed daily mean instead of monthly mean, ignoring month }
  result := dailyFromMonthlyMeans.windSpeed_mPsec;
  end;

function GsWeather.GetMeanPropRainInFirstHalfHourForMonth_frn(month: integer): single;
  begin
  result := params.meanPropRainInFirstHalfHourForMonth_frn[month];
  end;

function GsWeather.GetMeanTempForMonth_degC(month: integer): single;
  begin
  result := (params.dailyMeanMaxTempForMonth_degC[month] + params.dailyMeanMinTempForMonth_degC[month]) / 2.0;
  end;

function GsWeather.GetYearTotalMonthlyMeanTotalRainfall_mm: single;
  var
    month: integer;
  begin
  result := 0.0;
  for month := 0 to 11 do
   result := result + params.meanTotalRainfallForMonth_mm[month];
  end;

procedure GsWeather.ZeroWeatherParams;
  var
    month, direction: integer;
  begin
  params.yearsMaxMonthlyHalfHourRainfallRecord_yr := 0;
  params.coeffForWetDryProbsGivenNumWetDays_frn := 0.0;
  params.coeffRainfallModExpDist := 0.0;
  params.paramForModifiedExpWindSpeed := 0.0;
  for month := 0 to 11 do
    begin
    params.dailyMeanMaxTempForMonth_degC[month] := 0.0;
    params.dailyMeanMinTempForMonth_degC[month] := 0.0;
    params.stdDevMaxTempForMonth_degC[month] := 0.0;
    params.stdDevMinTempForMonth_degC[month] := 0.0;
    params.meanTotalRainfallForMonth_mm[month] := 0.0;
    params.stdDevDailyRainfallForMonth_mm[month] := 0.0;
    params.skewCoeffForRainfallForMonth[month] := 0.0;
    params.probWetDayAfterDryDayForMonth_frn[month] := 0.0;
    params.probWetDayAfterWetDayForMonth_frn[month] := 0.0;
    params.numWetDaysForMonth[month] := 0.0;
    params.meanPropRainInFirstHalfHourForMonth_frn[month] := 0.0;
    params.dailyMeanRadiationForMonth_MJPm2[month] := 0.0;
    params.dailyMeanRelHumForMonth_frn[month] := 0.0;
    params.dailyMeanWindSpeedForMonth_mPsec[month] := 0.0;
    stationVars.probWetDayForMonth_frn[month] := 0.0;
    stationVars.dailyMeanRainfallForMonth_mm[month] := 0.0;
    stationVars.rainfallNormalizingFactorForMonth[month] := 0.0;
    for direction := 0 to 15 do
      params.windDirectionsForMonth_frn[month][direction] := 0.0;
    end;
  stationVars.minDayLengthForYear_hr := 0.0;
  end;

procedure GsWeather.ZeroWeatherCumulativeVars;
  var
    i: integer;
  begin
  for i := 0 to 2 do matrices.yesterdaysCorrelationMultiplier[i] := 0.0;
  matrices.yesterdaysUniformRandNumForRainfall_frn := 0.0;
  matrices.lastEpsilonRandomNumber := 0.0;
  dailyWeather.itRainedYesterday := false;
  stationVars.barometricPressure_kPa := 0.0;
  stationVars.psychrometerConstant_kPaPdegC := 0.0;
  end;

function GsWeather.objectType: integer;
  begin
  result := kObjectTypeWeather;
  end;

procedure GsWeather.ZeroWeatherDailyVars;
  var
    i: integer;
  begin
  for i := 0 to 2 do
    begin
    matrices.epsilonRandomNumberMatrix[i] := 0.0;
    matrices.matrixAResidual[i] := 0.0;
    matrices.matrixBResidual[i] := 0.0;
    matrices.correlationMultiplier[i] := 0.0;
    end;
  dailyMeans.maxTemp_degC := 0.0;
  dailyMeans.radiation_MJPm2 := 0.0;
  dailyMeans.relHum_frn := 0.0;
  dailyFromMonthlyMeans.maxTemp_degC := 0.0;
  dailyFromMonthlyMeans.minTemp_degC := 0.0;
  dailyFromMonthlyMeans.relHum_frn := 0.0;
  dailyFromMonthlyMeans.radiation_MJPm2 := 0.0;
  dailyFromMonthlyMeans.probWetDay_frn := 0.0;
  dailyFromMonthlyMeans.windSpeed_mPsec := 0.0;
  (* don't zero these because user can choose option of leaving them constant
  dailyWeather.rainfallForDay_mm := 0.0;
  dailyWeather.radiationForDay_MJPm2 := 0.0;
  dailyWeather.minTempForDay_degC := 0.0;
  dailyWeather.maxTempForDay_degC := 0.0;
  dailyWeather.relHumForDay_frn := 0.0;
  dailyWeather.meanWindSpeedForDay_mPsec := 0.0;
  dailyWeather.windDirectionForDay_rad := 0.0;
  *)
  dailyWeather.meanTempForDay_degC := 0.0;
  dailyWeather.dayLength_hr := 0.0;
  dailyWeather.latentHeatOfVaporization_MJPkg := 0.0;
  dailyWeather.vaporPressureDeficit_kPa := 0.0;
  dailyWeather.slopeSaturVaporPressureCurve_kPaPdegC := 0.0;
  dailyWeather.declinationAngleOfSun_rad := 0.0;
  dailyWeather.maxPossibleRadiation_MJPm2 := 0.0;
  dailyWeather.netOutgoingRadiationIfDayIsClear_MJPm2 := 0.0;
  end;

function GsWeather.getOption(index: smallint): smallint;
  begin
  result := kWeatherOptionNormal;
  if (index < 0) or (index > kWeatherOptionsLastOption) then
    raise Exception.create('GsWeather.optionIsTrue: index out of bounds');
  if GsGarden(garden).weatherOverrides[index] then
    result := GsGarden(garden).weatherOptions[index]
  else
    result := self.options[index];
  end;

procedure GsWeather.MakeSureWindDirectionsAddUpToOne;
  var
    direction, month: smallint;
    total: single;
  begin
  { make sure wind directions add up to 1.0 }
  for month := 0 to 11 do
    begin
    total := 0.0;
    for direction := 0 to 15 do
      total := total + params.windDirectionsForMonth_frn[month][direction];
    for direction := 0 to 15 do
      params.windDirectionsForMonth_frn[month][direction] :=
          params.windDirectionsForMonth_frn[month][direction] * safedivExcept(1.0, total, 1.0);
    end;
  end;

{ -------------------------------------------------------------------------------------- frost dates and heat units }
function GsWeather.monthWithWarmestMinTemp: smallint;
  var
    month: smallint;
    warmestMinTemp_degC: single;
  begin
  warmestMinTemp_degC := params.dailyMeanMinTempForMonth_degC[0];
  result := 0;
  for month := 1 to 11 do
    if params.dailyMeanMinTempForMonth_degC[month] > warmestMinTemp_degC then
      begin
      result := month;
      warmestMinTemp_degC := params.dailyMeanMinTempForMonth_degC[month];
      end;
  end;

function GsWeather.middayOfMonthWithLowestMeanMinTempForYearBelow5degC: single;
  var
    month, monthWithLowestMeanMinTemp: smallint;
    yearLowMeanMinMonthTemp_degC: single;
  begin
  result := 400; { means no month's mean min temp is below -5 deg C }
  monthWithLowestMeanMinTemp := 0;
  yearLowMeanMinMonthTemp_degC := 0.0;
  for month := 0 to 11 do
    begin
    if month = 0 then
      begin
      if params.dailyMeanMinTempForMonth_degC[0] > params.dailyMeanMinTempForMonth_degC[11] then
        monthWithLowestMeanMinTemp := 11
      else
        monthWithLowestMeanMinTemp := 0;
      end
    else
      begin
      if params.dailyMeanMinTempForMonth_degC[month] <= yearLowMeanMinMonthTemp_degC then
        monthWithLowestMeanMinTemp := month;
      end;
    yearLowMeanMinMonthTemp_degC := params.dailyMeanMinTempForMonth_degC[monthWithLowestMeanMinTemp];
    end;
  if yearLowMeanMinMonthTemp_degC <= 5.0 then
    result := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, monthWithLowestMeanMinTemp);
  end;

procedure GsWeather.calculateFrostDates;
  begin
  self.calculateJulianDayOfLastSpringFrost;
  self.calculateJulianDayOfFirstFallFrost;
  end;

procedure GsWeather.calculateJulianDayOfLastSpringFrost;
  var
    minTemp_degC, stdDevMinTemp_degC: single;
    month, dayOfMonth, day, count: smallint;
    date: GsDate;
  begin
  { use smoothed means to get day when "all danger of frost is past" - use standard deviation * 2 to reduce
    temp to consider 95% of variation. assume frost means min air temperature is <= 0 degrees C.
    to get spring frosts only, get warmest day in year and work backwards from there - handles southern hemisphere }
  stationVars.julianDayOfLastSpringFrost := 0;
  day := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, monthWithWarmestMinTemp);
  count := 182;
  while count >= 0 do
    begin
    month := GsDate_monthFromDayOfYear_NoLeap(day);
    dayOfMonth := GsDate_dayOfMonthFromDayOfYear_NoLeap(day);
    date := GsDate_dateFromYearMonthDayOfMonth(1901, month, dayOfMonth); {1900 is a leap year}
    minTemp_degC := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanMinTempForMonth_degC, date);
    stdDevMinTemp_degC := GsDate_DailyValueFromMonthlyArray_NoLeap(params.stdDevMinTempForMonth_degC, date);
    if minTemp_degC - stdDevMinTemp_degC * 2.0 <= 0.0 then
      break;
    dec(day);
    if day < 0 then
      day := 364;
    dec(count);
    end;
  if count < 0 then
    stationVars.julianDayOfLastSpringFrost := -1 {no frost}
  else
    stationVars.julianDayOfLastSpringFrost := day;
  end;

procedure GsWeather.calculateJulianDayOfFirstFallFrost;
  var
    minTemp_degC, stdDevMinTemp_degC: single;
    month, dayOfMonth, day, count: smallint;
    date: GsDate;
  begin
  stationVars.julianDayOfFirstFallFrost := 0;
  { use smoothed means to get day when first danger of frost is present - use standard deviation * 2 to reduce
    temp to consider 95% of variation. assume frost means min air temperature is <= 0 degrees C.
    to get fall frosts only, get warmest day in year and work forwards from there - handles southern hemisphere }
  day := GsDate_dayOfYearFromDayOfMonthAndMonth_NoLeap(15, monthWithWarmestMinTemp);
  count := 0;
  while count <= 182 do
    begin
    month := GsDate_monthFromDayOfYear_NoLeap(day);
    dayOfMonth := GsDate_dayOfMonthFromDayOfYear_NoLeap(day);
    date := GsDate_dateFromYearMonthDayOfMonth(1901, month, dayOfMonth);
    minTemp_degC := GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanMinTempForMonth_degC, date);
    stdDevMinTemp_degC := GsDate_DailyValueFromMonthlyArray_NoLeap(params.stdDevMinTempForMonth_degC, date);
    if minTemp_degC - stdDevMinTemp_degC * 2.0 <= 0.0 then
      break;
    inc(day);
    if day > 364 then
      day := 0;
    inc(count);
    end;
  if count > 182 then
    stationVars.julianDayOfFirstFallFrost := -1 {no frost}
  else
    stationVars.julianDayOfFirstFallFrost := day;
  end;

function GsWeather.AccumulateHeatUnitsForNumberOfDays(startDay, numDays: smallint;
    baseTemp_degC, optimalTemp_degC, minDayLengthForWinterGrowth_hr: single): single;
  var
    day, month, dayOfMonth, count: smallint;
    aveTemp_degC, declinationAngleOfSun_rad, dayLength_hr: single;
    date: GsDate;
    climateIsCold, addTemp: boolean;
  begin
  (* FORTRAN
      function cahu (j,k,base)
c     this subprogram accumulates heat units for use in cpthu.
      cahu=0.
      mo=1
      do 3 jda=j,k
         call axmon
         if (jdhu.gt.366) go to 2
         if (aralt(hrlt,hr1).lt.parm(6)) go to 3
    2    ta=aralt(tav,xx)
         tgx=ta-base
         if (tgx.gt.0.) cahu=cahu+tgx
    3 continue
      return
      end
   *)
  try
  result := 0;
  count := 1;
  day := startDay;
  climateIsCold := self.middayOfMonthWithLowestMeanMinTempForYearBelow5degC <= 366;
  while count <= numDays do
    begin
    month := GsDate_monthFromDayOfYear_NoLeap(day);
    dayOfMonth := GsDate_dayOfMonthFromDayOfYear_NoLeap(day);
    date := GsDate_dateFromYearMonthDayOfMonth(1901, month, dayOfMonth);
    addTemp := true;
    { don't count days on which plants would be in winter dormancy if the climate has a winter }
    if climateIsCold then
      begin
      declinationAngleOfSun_rad := EQ.SunDeclinationAngle_rad(day);
      dayLength_hr := EQ.DayLength_hr(params.climateLatitude_rad, declinationAngleOfSun_rad);
      if dayLength_hr < minDayLengthForWinterGrowth_hr then
        addTemp := false;
      end;
    if addTemp then
      begin
      { change from EPIC:}
      { EPIC uses dailyMeanMeanTempForMonthAndPrevious_degC here, but I can't figure out why. it depresses
        all the PHU's by dragging the colder weather along. I'm putting the regular mean temp in. }
      { got rid of dailyMeanMeanTempForMonthAndPrevious_degC to avoid updating it since it is not needed }
      aveTemp_degC := (GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanMinTempForMonth_degC, date)
        + GsDate_DailyValueFromMonthlyArray_NoLeap(params.dailyMeanMaxTempForMonth_degC, date)) / 2.0;
      {change from EPIC: we added the second term here to bound the accumulation if the temperature is above optimal }
      result := result + max(0.0, aveTemp_degC - baseTemp_degC) - max(0.0, aveTemp_degC - optimalTemp_degC);
      end;
    inc(day);
    if day > 364 then
      day := 0;
    inc(count);
    end;
  except on e: Exception do result := errorMessage('Exception in EQ.AccumulateHeatUnitsForAnyTimeSpan: ' + e.message); end;
  end;

{ ------------------------------------------------------------------------------------------ streaming/transfer }
procedure GsWeather.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsWeather;
  cvir.versionNumber := 2;
  cvir.additionNumber := 2;
  end;

procedure GsWeather.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    month: smallint;
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamBytes(params, sizeOf(params));
  filer.streamBytes(stationVars, sizeOf(stationVars));
  filer.streamBytes(longTermFactors, sizeOf(longTermFactors));
  filer.streamBytes(dailyFromMonthlyMeans, sizeOf(dailyFromMonthlyMeans));
  filer.streamBytes(dailyMeans, sizeOf(dailyMeans));
  filer.streamBytes(dailyWeather, sizeOf(dailyWeather));
  filer.streamBytes(matrices, sizeOf(matrices));
  filer.streamBytes(options, sizeOf(options));
  filer.streamSmallint(historyNumDays);
  filer.streamBytes(historyDailyWeather, sizeOf(historyDailyWeather));
  filer.streamBytes(historyDailyFromMonthlyMeans, sizeOf(historyDailyFromMonthlyMeans));
  randomNumberGenerator.streamUsingFiler(filer);
  if (filer.isReading) and (cvir.additionNumber < 1) then
    self.calculateFrostDates;
  { fix things that were supposed to be set at 1.0 }
  if (filer.isReading) then
    for month := 0 to 11 do with longTermFactors do
      begin
      if rainfallAdjFactorForMonth[month] = 0.0 then rainfallAdjFactorForMonth[month] := 1.0;
      if minTempAdjFactorForMonth[month] = 0.0 then minTempAdjFactorForMonth[month] := 1.0;
      end;
  if (filer.isReading) and (cvir.additionNumber < 2) then with longTermFactors do
    begin
    { since can't know how long long-term factors have been used, zero out }
    yearsOfLongTermWeatherDataForAdjustment := 0;
    daysOfLongTermWeatherDataForAdjustment := 0;
    for month := 0 to 11 do
      begin
      rainfallTotalForMonth_mm[month] := 0.0;
      rainfallAdjFactorForMonth[month] := 1.0;
      maxTempTotalForMonth_degC[month] := 0.0;
      maxTempAdjFactorForMonth[month] := 0.0;
      minTempTotalForMonth_degC[month] := 0.0;
      minTempAdjFactorForMonth[month] := 1.0;
      end;
    end;
  end;

procedure GsWeather.transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
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
    kWeatherClimateLatitude_rad:
      sideEffects_climateLatitude_rad(updateList);
    kWeatherClimateElevation_m:
      sideEffects_climateElevation_m(updateList);
    kWeatherParamForModifiedExpWindSpeed:
      sideEffects_paramForModifiedExpWindSpeed(oldSingle, updateList);
    kWeatherDailyMeanMinTempForMonth_degC:
      sideEffects_dailyMeanMinTempForMonth_degC(index, updateList);
    kWeatherNumWetDaysForMonth, kWeatherMeanTotalRainfallForMonth_mm:
      sideEffects_numWetDaysOrMeanTotalRainfallForMonth(index, updateList);
    end;
  except on e: Exception do errorMessage('Exception in plant transferField: ' + e.message); end;
  end;

procedure GsWeather.directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  Weather_directTransferField(self, v, d, fieldID, ft, index, deriveMethod, updateList);
  self.addToUpdateList(fieldID, index, updateList);
  end;

{ side effect functions }
procedure GsWeather.sideEffects_climateLatitude_rad(updateList: TListCollection);
  begin
  stationVars.minDayLengthForYear_hr := EQ.MinDayLengthForYear_hr(params.climateLatitude_rad);
  self.addToUpdateList(kWeatherMinDayLengthForYear_hr, 0, updateList);
  end;

procedure GsWeather.sideEffects_climateElevation_m(updateList: TListCollection);
  begin
  stationVars.barometricPressure_kPa := EQH.BarometricPressure_kPa(params.climateElevation_m);
  stationVars.psychrometerConstant_kPaPdegC := EQH.PsychrometerConstant_kPaPdegC(stationVars.barometricPressure_kPa);
  self.addToUpdateList(kWeatherBarometricPressure_kPa, 0, updateList);
  self.addToUpdateList(kWeatherPsychrometerConstant_kPaPdegC, 0, updateList);
  end;

procedure GsWeather.sideEffects_paramForModifiedExpWindSpeed(oldValue: single; updateList: TListCollection);
  var
    month: smallint;
    windSpeedModifier, oldWindSpeedModifier: single;
  begin
  windSpeedModifier := Utils_IntegrateModifiedExponentialEquation(params.paramForModifiedExpWindSpeed);
  oldWindSpeedModifier := Utils_IntegrateModifiedExponentialEquation(oldValue);
  { convert to using new value from old value }
  for month := 0 to 11 do
    params.dailyMeanWindSpeedForMonth_mPsec[month] := params.dailyMeanWindSpeedForMonth_mPsec[month] *
        safedivExcept(oldWindSpeedModifier, windSpeedModifier, 1.0);
  self.addToUpdateList(kWeatherDailyMeanWindSpeedForMonth_mPsec, -1, updateList); {-1 to update all months}
  end;

procedure GsWeather.sideEffects_dailyMeanMinTempForMonth_degC(month: smallint; updateList: TListCollection);
  begin
  self.calculateFrostDates;
  self.addToUpdateList(kWeatherJulianDayOfLastSpringFrost, -1, updateList); {-1 to update all months}
  self.addToUpdateList(kWeatherJulianDayOfFirstFallFrost, -1, updateList);
  end;

procedure GsWeather.sideEffects_numWetDaysOrMeanTotalRainfallForMonth(month: smallint; updateList: TListCollection);
  var
    daysInMonth: smallint;
  begin
  { assumption: only one month value is changed at a time }
  if (month < 0) or (month > 11) then exit;
  daysInMonth := GsDate_daysInMonthFromMonth_NoLeap(month);
  if params.numWetDaysForMonth[month] = 0.0 then
    stationVars.dailyMeanRainfallForMonth_mm[month] := 0.0
  else
    stationVars.dailyMeanRainfallForMonth_mm[month] := safediv(params.meanTotalRainfallForMonth_mm[month],
        params.numWetDaysForMonth[month]);
  stationVars.probWetDayForMonth_frn[month] := safediv(params.numWetDaysForMonth[month], 1.0 * daysInMonth);
  self.addToUpdateList(kWeatherDailyMeanRainfallForMonth_mm, month, updateList);
  self.addToUpdateList(kWeatherProbWetDayForMonth_frn, month, updateList);
  end;

procedure GsWeather.streamUsingTextFiler(textFiler: GsTextFiler);
  var
    month, i: smallint;
    total: single;
  begin
  { weather has no change between version 0.90 and 1.0 }
  Weather_streamUsingTextFiler(self, textFiler);
  if textFiler.isReading then
    begin
    { but if weather was read from a text file, some things won't be set right }
    for month := 0 to 11 do with longTermFactors do
      begin
      { set multiplied factors at 1.0 }
      rainfallAdjFactorForMonth[month] := 1.0;
      minTempAdjFactorForMonth[month] := 1.0;
      { make sure wind directions add up to 1.0 }
      total := 0;
      for i := 0 to 15 do
        total := total + (params.windDirectionsForMonth_frn[month][i]);
      for i := 0 to 15 do
        params.windDirectionsForMonth_frn[month][i] := params.windDirectionsForMonth_frn[month][i] / total;
      end;
    Defaults_EnforceWeatherInputRanges(self);
    self.calculateFrostDates;
    end;
  end;

end.

unit Uhardcd;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uhardcd: Sets up some default templates. Only called if the program is started up
with no templates file AND the ALLOW_HARDCODING conditional define is set during compiling. }

interface

uses utempman, uharvprt, umodel, uestruct, udplant, uebag, ueweath, ueplant, uesoil;

{bags}
procedure InitAsBag_Sand(bag: GsBag);
procedure InitAsBag_Lime(bag: GsBag);
procedure InitAsBag_Compost(bag: GsBag);
procedure InitAsBag_CowManureFresh(bag: GsBag);
procedure InitAsBag_WoodAshes(bag: GsBag);
procedure InitAsBag_AmmoniumSulfate(bag: GsBag);
procedure InitAsBag_Straw(bag: GsBag);
{weather}
procedure InitAsClimate_Pittsburgh_PA(weather: GsWeather);
procedure InitAsClimate_LosAngeles_CA(weather: GsWeather);
procedure InitAsClimate_Newark_NJ(weather: GsWeather);
{soil}
procedure InitAsSoilType_HagerstownB_MD(soil: GsSoilPatch);
procedure InitAsSoilType_Keyport_NJ(soil: GsSoilPatch);
procedure InitAsSoilType_Indianola_IA(soil: GsSoilPatch);
{plant}
procedure InitAsCultivar_Corn(plant: GsPlant);
procedure InitAsCultivar_Sunflower(plant: GsPlant);
procedure InitAsCultivar_EarlyPea(plant: GsPlant);
procedure InitAsCultivar_Onion(plant: GsPlant);
procedure InitAsCultivar_GreenBean(plant: GsPlant);
procedure InitAsCultivar_Lettuce(plant: GsPlant);
procedure InitAsCultivar_Spinach(plant: GsPlant);
procedure InitAsCultivar_Tomato(plant: GsPlant);
procedure InitAsCultivar_Pepper(plant: GsPlant);
procedure InitAsCultivar_Carrot(plant: GsPlant);
procedure setDefaultsForAllPlants(plant: GsPlant);
procedure setDefaultsForFloralParams(plant: GsPlant);
{drawing plant}
procedure InitAsCultivar_Corn_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Sunflower_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_EarlyPea_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Onion_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_GreenBean_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Lettuce_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Spinach_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Tomato_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Pepper_drawingPlant(drawingPlant: GsDrawingPlant);
procedure InitAsCultivar_Carrot_drawingPlant(drawingPlant: GsDrawingPlant);
{template manager}
function createHardCodedHarvestItemTemplate(aName: string; templateManager: GsTemplateManager): GsHarvestItemTemplate;

implementation

uses SysUtils, udefault, ueutils, uunits, udomain, usupport;

{ BAG }
procedure InitAsBag_Sand(bag: GsBag);
  begin
  bag.setName('Sand');
  bag.contents.sand_pct := 100.0;
  end;

procedure InitAsBag_Lime(bag: GsBag);
  begin
  bag.setName('Lime');
  bag.contents.calciumCarbonateOrEquivalent_pct := 100.0;
  end;

procedure InitAsBag_Compost(bag: GsBag);
  begin
  bag.setName('Compost');
  with bag.contents do
    begin
    { 50% OM, 50% residue - guess }
    organicMatter_pct := 50.0;
    flatCropResidue_pct :=  50.0;
    { 1% N in OM }
    organicNFresh_pct := 0.3;
    organicNActiveHumus_pct := 0.3;
    organicNStableHumus_pct := 0.4;
    { 1% P in OM }
    organicPFresh_pct := 0.5;
    organicPHumus_pct := 0.5;
    end;
  end;

procedure InitAsBag_CowManureFresh(bag: GsBag);
  begin
  bag.setName('Cow manure, fresh');
  with bag.contents do
    begin
    { 80% OM, 20% residue - guess }
    organicMatter_pct := 80.0;
    flatCropResidue_pct := 20.0;
    { 0.5% N in OM }
    organicNFresh_pct := 0.3;
    organicNActiveHumus_pct := 0.2;
    { 0.1% P in OM }
    organicPFresh_pct := 0.07;
    organicPHumus_pct := 0.03;
    { has some water }
    waterContent_mPm := 0.6;
    end;
  end;

procedure InitAsBag_WoodAshes(bag: GsBag);
  begin
  bag.setName('Wood ashes');
  with bag.contents do
    begin
    { 80% OM, 20% residue - guess }
    organicMatter_pct := 80.0;
    flatCropResidue_pct := 20.0;
    { no N }
    { 2% P in mineral and OM }
    organicPHumus_pct := 0.5;
    mineralPActive_pct := 1.5;
    { raises pH }
    calciumCarbonateOrEquivalent_pct := 1.0; { wild guess }
    end;
  end;

procedure InitAsBag_AmmoniumSulfate(bag: GsBag);
  begin
  bag.setName('Ammonium sulfate');
  with bag.contents do
    begin
    { no OM or residue }
    { N as ammonia }
    ammonia_pct := 20.0;
    { no P }
    { lowers pH }
    sulfurOrEquivalent_pct := 1.0; { wild guess }
    end;
  end;

procedure InitAsBag_Straw(bag: GsBag);
  begin
  bag.setName('Straw');
  with bag.contents do
    begin
    mulchFlatCropResidue_pct := 100.0;
    mulchOrganicNFresh_pct := 0.4;
    mulchOrganicPFresh_pct := 0.2;
    end;
  end;

{ WEATHER }
procedure InitAsClimate_Pittsburgh_PA(weather: GsWeather);
  begin
  if weather = nil then
    raise Exception.create('Init as climate: nil weather');
  with weather do
  begin
  weather.setName('Pittsburgh, PA (Airport)');
  params.climateLatitude_rad := Utils_DegreesToRadians(40.5);
  params.climateElevation_m := 350.0;
  { Mean daily maximum temperature by month_degC=
    0.92, 2.76, 8.74, 15.83, 21.49, 25.9, 27.99, 27.17, 23.63, 16.99, 10.07 3.57 }
  Utils_SetMonthValues(params.dailyMeanMaxTempForMonth_degC,
    0.92, 2.76, 8.74, 15.83, 21.49, 25.9, 27.99, 27.17, 23.63, 16.99, 10.07, 3.57);
  { Mean daily minimum temperature by month_degC=
    -7.44, -6.32, -1.52, 4.02, 9.17, 13.83, 16.4, 15.75, 11.95, 5.77, 1.02, -4.19 }
  Utils_SetMonthValues(params.dailyMeanMinTempForMonth_degC,
    -7.44, -6.32, -1.52, 4.02, 9.17, 13.83, 16.4, 15.75, 11.95, 5.77, 1.02, -4.19);
  { Standard deviation of maximum temperature by month_degC=
    6.86, 6.74, 7.1, 6.66, 5.36, 4.17, 3.19, 3.24, 4.77, 5.74, 6.43, 6.62 }
  Utils_SetMonthValues(params.stdDevMaxTempForMonth_degC,
    6.86, 6.74, 7.1, 6.66, 5.36, 4.17, 3.19, 3.24, 4.77, 5.74, 6.43, 6.62);
  { Standard deviation of minimum temperature by month_degC=
    6.64, 6.61, 5.47, 5.47, 4.82, 3.92, 3.22, 3.44, 4.58, 4.8, 5.31, 6.1 }
  Utils_SetMonthValues(params.stdDevMinTempForMonth_degC,
    6.64, 6.61, 5.47, 5.47, 4.82, 3.92, 3.22, 3.44, 4.58, 4.8, 5.31, 6.1);
  { Mean total rainfall by month_mm=
    65.3, 61.8, 87.6, 82.3, 87.1, 86.4, 92.9, 79.5, 70.3, 61.4, 58.8, 66.7 }
  Utils_SetMonthValues(params.meanTotalRainfallForMonth_mm,
    65.3, 61.8, 87.6, 82.3, 87.1, 86.4, 92.9, 79.5, 70.3, 61.4, 58.8, 66.7);
  { Standard deviation of daily rainfall by month_mm=
    5.6, 6.3, 7.1, 7.1, 9.1, 8.9, 10.9, 11.2, 9.4, 8.9, 5.8, 5.3 }
  Utils_SetMonthValues(params.stdDevDailyRainfallForMonth_mm,
    5.6, 6.3, 7.1, 7.1, 9.1, 8.9, 10.9, 11.2, 9.4, 8.9, 5.8, 5.3);
  { Skew coefficient for rainfall by month=
    1.63, 3.03, 1.48, 0.44, 1.28, 0.41, 1.31, 1.58, 0.95, 3.32, 1.07, 1.58 }
  Utils_SetMonthValues(params.skewCoeffForRainfallForMonth,
    1.63, 3.03, 1.48, 0.44, 1.28, 0.41, 1.31, 1.58, 0.95, 3.32, 1.07, 1.58);
  { Probability of wet day after dry day by month_frn=
    0.43, 0.38, 0.44, 0.35, 0.29, 0.3, 0.31, 0.26, 0.24, 0.26, 0.33, 0.45 }
  Utils_SetMonthValues(params.probWetDayAfterDryDayForMonth_frn,
    0.43, 0.38, 0.44, 0.35, 0.29, 0.3, 0.31, 0.26, 0.24, 0.26, 0.33, 0.45);
  { Probability of wet day after wet day by month_frn=
    0.6, 0.61, 0.57, 0.54, 0.53, 0.48, 0.39, 0.41, 0.46, 0.46, 0.56, 0.6 }
  Utils_SetMonthValues(params.probWetDayAfterWetDayForMonth_frn,
    0.6, 0.61, 0.57, 0.54, 0.53, 0.48, 0.39, 0.41, 0.46, 0.46, 0.56, 0.6);
  { Number of wet days by month=
    16.06, 14.31, 15.68, 12.96, 11.83, 10.98, 10.45, 9.48, 9.23, 10.07, 12.86, 16.41 }
  Utils_SetMonthValues(params.numWetDaysForMonth,
    16.06, 14.31, 15.68, 12.96, 11.83, 10.98, 10.45, 9.48, 9.23, 10.07, 12.86, 16.41);
  { Mean proportion of rain falls in first half hour by month_frn=
    8.9, 5.6, 10.7, 9.7, 17.0, 20.1, 36.1, 37.6, 20.3, 21.1, 6.3, 6.1 }
  Utils_SetMonthValues(params.meanPropRainInFirstHalfHourForMonth_frn,
    8.9, 5.6, 10.7, 9.7, 17.0, 20.1, 36.1, 37.6, 20.3, 21.1, 6.3, 6.1);
  { Mean daily radiation by month_MJPm2=
    94.0, 169.0, 216.0, 317.0, 429.0, 491.0, 497.0, 409.0, 339.0, 207.0, 118.0, 77.0 }
  Utils_SetMonthValues(params.dailyMeanRadiationForMonth_MJPm2,
    94.0, 169.0, 216.0, 317.0, 429.0, 491.0, 497.0, 409.0, 339.0, 207.0, 118.0, 77.0);
  { Mean daily relative humidity by month_frn=
    -5.28, -5.42, -2.63, 2.77, 8.19, 13.61, 15.69, 15.28, 11.95, 5.84, 0.29, -4.85 }
  Utils_SetMonthValues(params.dailyMeanRelHumForMonth_frn,
    -5.28, -5.42, -2.63, 2.77, 8.19, 13.61, 15.69, 15.28, 11.95, 5.84, 0.29, -4.85);
  { Mean daily wind speed by month_mPsec=
    4.88, 4.89, 5.04, 4.87, 4.24, 3.74, 3.44, 3.33, 3.52, 3.91, 4.62, 4.84 }
  Utils_SetMonthValues(params.dailyMeanWindSpeedForMonth_mPsec,
    4.88, 4.89, 5.04, 4.87, 4.24, 3.74, 3.44, 3.33, 3.52, 3.91, 4.62, 4.84);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 0,
    3.0, 4.0, 5.0, 6.0, 7.0, 6.0, 6.0, 7.0, 6.0, 6.0, 3.0, 3.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 1,
    2.0, 3.0, 4.0, 3.0, 4.0, 4.0, 5.0, 5.0, 6.0, 4.0, 2.0, 2.0);
    Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 2,
  2.0, 3.0, 4.0, 3.0, 3.0, 3.0, 4.0, 4.0, 5.0, 3.0, 2.0, 2.0);
   Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 3,
   4.0, 5.0, 5.0, 4.0, 4.0, 4.0, 4.0, 4.0, 6.0, 4.0, 4.0, 4.0);
   Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 4,
   4.0, 5.0, 6.0, 5.0, 4.0, 4.0, 4.0, 4.0, 6.0, 6.0, 5.0, 4.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 5,
    4.0, 4.0, 6.0, 5.0, 5.0, 4.0, 4.0, 4.0, 5.0, 6.0, 5.0, 5.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 6,
    4.0, 4.0, 4.0, 5.0, 5.0, 5.0, 4.0, 4.0, 6.0, 6.0, 6.0, 5.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 7,
    4.0, 4.0, 3.0, 4.0, 4.0, 4.0, 4.0, 4.0, 5.0, 4.0, 4.0, 5.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 8,
    5.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 6.0, 6.0, 5.0, 6.0, 6.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 9,
    6.0, 5.0, 4.0, 5.0, 6.0, 8.0, 7.0, 6.0, 6.0, 6.0, 6.0, 7.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 10,
    11.0, 8.0, 7.0, 8.0, 9.0, 11.0, 11.0, 10.0, 9.0, 10.0, 11.0, 11.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 11,
    19.0, 15.0, 15.0, 14.0, 14.0, 14.0, 15.0, 13.0, 11.0, 12.0, 17.0, 20.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 12,
    15.0, 14.0, 11.0, 9.0, 8.0, 8.0, 9.0, 9.0, 7.0, 9.0, 13.0, 12.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 13,
    9.0, 10.0, 9.0, 9.0, 8.0, 7.0, 7.0, 7.0, 6.0, 7.0, 8.0, 8.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 14,
    6.0, 7.0, 8.0, 8.0, 8.0, 6.0, 7.0, 7.0, 6.0, 6.0, 5.0, 5.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 15,
    3.0, 4.0, 5.0, 6.0, 6.0, 6.0, 5.0, 6.0, 4.0, 6.0, 3.0, 3.0);
  Defaults_DefaultAndCalculateWeatherInputs(weather);
  end;
  end;

procedure InitAsClimate_LosAngeles_CA(weather: GsWeather);
  begin
  if weather = nil then
    raise Exception.create('Init as climate: nil weather');
  with weather do
  begin
  weather.setName('Los Angeles, CA (Airport)');
  params.climateLatitude_rad := Utils_DegreesToRadians(33.93);
  params.climateElevation_m := 30.0;
  Utils_SetMonthValues(params.dailyMeanMaxTempForMonth_degC,
    17.98, 18.49, 18.33, 19.43, 20.54, 22.21, 24.08, 24.76, 24.55, 23.17, 21.23, 18.92);
  Utils_SetMonthValues(params.dailyMeanMinTempForMonth_degC,
    8.07,  9.07,  9.77, 11.28, 13.18, 15.11, 16.90, 17.52, 16.81, 14.49, 11.02,  8.62);
  Utils_SetMonthValues(params.stdDevMaxTempForMonth_degC,
    4.07,  3.69,  3.24,  3.16,  2.71,  2.70,  2.27,  2.28,  3.55,  3.91,  4.14,  3.88);
  Utils_SetMonthValues(params.stdDevMinTempForMonth_degC,
    3.04,  2.69,  2.34,  2.01,  1.77,  1.64,  1.68,  1.76,  2.08,  2.43,  2.73,  2.77);
  Utils_SetMonthValues(params.meanTotalRainfallForMonth_mm,
    68.7,  58.2,  48.8,  21.0,   3.2,    0.9,    0.3,   2.3,   4.7,   6.5,  45.5,  40.8);
  Utils_SetMonthValues(params.stdDevDailyRainfallForMonth_mm,
    14.5,  15.7,  10.9,   7.1,   6.1,   2.0,   1.0,  13.0,   8.4,   7.1,  16.3,  12.4);
  Utils_SetMonthValues(params.skewCoeffForRainfallForMonth,
    1.62,  2.06,  2.32,  -0.14,  6.01,  1.56,  2.04,  3.08,  3.42,  3.79,  3.49,  1.77);
  Utils_SetMonthValues(params.probWetDayAfterDryDayForMonth_frn,
    0.11,  0.10,  0.12,  0.07,  0.03,  0.02,  0.01,  0.01,  0.03,  0.04,  0.08,  0.09);
  Utils_SetMonthValues(params.probWetDayAfterWetDayForMonth_frn,
    0.53,  0.53,  0.50,  0.41,  0.36,  0.18,  0.09,  0.21,  0.25,  0.27,  0.49,  0.50);
  Utils_SetMonthValues(params.numWetDaysForMonth,
    5.88,  5.09,  6.00,  3.18,  1.39,   0.71,   0.34,   0.39,  1.15,  1.61,  4.07,  4.73);
  Utils_SetMonthValues(params.meanPropRainInFirstHalfHourForMonth_frn,
    10.9,   7.6,   7.6,   5.8,   6.6,   1.3,   1.3,   6.3,   5.6,  10.4,   7.6,  10.2);
  Utils_SetMonthValues(params.dailyMeanRadiationForMonth_MJPm2,
    228.0,  306.0,  426.0,  510.0,  581.0,  610.0,  605.0,  567.0,  513.0,  374.0,  263.0,  201.0);
  Utils_SetMonthValues(params.dailyMeanRelHumForMonth_frn,
    4.44,  6.11,  7.22,  9.44, 11.11, 12.78, 15.00, 15.00, 14.44, 11.67,  7.78,  5.56);
  Utils_SetMonthValues(params.dailyMeanWindSpeedForMonth_mPsec,
    2.88, 3.16, 3.47, 3.66, 3.59, 3.45, 3.36, 3.29, 3.13, 2.91, 2.86, 2.83);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 0,
    6.0, 4.0, 4.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 3.0, 5.0, 6.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 1,
    6.0, 4.0, 3.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 5.0, 7.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 2,
    9.0, 7.0, 4.0, 3.0, 2.0, 1.0, 1.0, 1.0, 2.0, 4.0, 8.0, 10.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 3,
    10.0, 7.0, 6.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 5.0, 8.0, 9.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 4,
    13.0, 11.0, 10.0, 8.0, 6.0, 5.0, 4.0, 4.0, 6.0, 9.0, 12.0, 12.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 5,
    7.0, 7.0, 6.0, 6.0, 4.0, 4.0, 3.0, 3.0, 4.0, 6.0, 7.0, 7.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 6,
   5.0, 5.0, 5.0, 5.0, 4.0, 5.0, 4.0, 4.0, 4.0, 4.0, 4.0, 5.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 7,
   3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 2.0, 3.0, 2.0, 3.0, 3.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 8,
   2.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 4.0, 3.0, 2.0, 2.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 9,
   2.0, 2.0, 3.0, 3.0, 3.0, 3.0, 2.0, 3.0, 3.0, 2.0, 2.0, 2.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 10,
    4.0, 5.0, 7.0, 8.0, 9.0, 10.0, 9.0, 8.0, 8.0, 7.0, 4.0, 3.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 11,
    10.0, 14.0, 19.0, 24.0, 30.0, 32.0, 32.0, 31.0, 28.0, 22.0, 13.0, 9.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 12,
    14.0, 18.0, 20.0, 24.0, 26.0, 25.0, 29.0, 29.0, 26.0, 23.0, 17.0, 15.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 13,
    3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 4.0, 4.0, 3.0, 4.0, 3.0, 3.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 14,
    3.0, 2.0, 2.0, 2.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 3.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 15,
    3.0, 4.0, 3.0, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, 3.0, 4.0);
  Defaults_DefaultAndCalculateWeatherInputs(weather);
  end;
  end;

procedure InitAsClimate_Newark_NJ(weather: GsWeather);
  begin
  if weather = nil then
    raise Exception.create('Init as climate: nil weather');
  with weather do
  begin
  weather.setName('Newark, NJ (Airport)');
  params.climateLatitude_rad := Utils_DegreesToRadians(40.7);
  params.climateElevation_m := 3.0;
  { Mean daily maximum temperature by month_degC=
    3.59, 4.89, 9.53, 16.31, 21.96, 27.08, 29.93, 28.84, 24.84, 18.91, 12.41, 5.88 }
  Utils_SetMonthValues(params.dailyMeanMaxTempForMonth_degC,
    3.59, 4.89, 9.53, 16.31, 21.96, 27.08, 29.93, 28.84, 24.84, 18.91, 12.41, 5.88);
  { Mean daily minimum temperature by month_degC=
    -4.28, -3.52, 0.65, 5.98, 11.61, 16.89, 20.06, 19.32, 15.1, 9.03, 3.86, -1.84 }
  Utils_SetMonthValues(params.dailyMeanMinTempForMonth_degC,
    -4.28, -3.52, 0.65, 5.98, 11.61, 16.89, 20.06, 19.32, 15.1, 9.03, 3.86, -1.84);
  { Standard deviation of maximum temperature by month_degC=
    5.67, 5.5, 5.52, 5.71, 5.19, 4.52, 3.6, 3.44, 4.47, 4.81, 5.18, 5.56 }
  Utils_SetMonthValues(params.stdDevMaxTempForMonth_degC,
    5.67, 5.5, 5.52, 5.71, 5.19, 4.52, 3.6, 3.44, 4.47, 4.81, 5.18, 5.56);
  { Standard deviation of minimum temperature by month_degC=
    5.37, 5.06, 4.28, 3.88, 3.72, 3.26, 2.66, 2.97, 4.17, 4.47, 4.59, 5.14 }
  Utils_SetMonthValues(params.stdDevMinTempForMonth_degC,
    5.37, 5.06, 4.28, 3.88, 3.72, 3.26, 2.66, 2.97, 4.17, 4.47, 4.59, 5.14);
  { Mean total rainfall by month_mm=
    80.7, 77.7, 102.2, 93.3, 87.5, 70.8, 94.1, 104.4, 86.3, 75.8, 89.9, 93.7 }
  Utils_SetMonthValues(params.meanTotalRainfallForMonth_mm,
    80.7, 77.7, 102.2, 93.3, 87.5, 70.8, 94.1, 104.4, 86.3, 75.8, 89.9, 93.7);
  { Standard deviation of daily rainfall by month_mm=
    9.9, 9.9, 11.7, 11.4, 11.4, 9.4, 13.7, 16.8, 15.7, 13.0, 13.7, 10.2 }
  Utils_SetMonthValues(params.stdDevDailyRainfallForMonth_mm,
    9.9, 9.9, 11.7, 11.4, 11.4, 9.4, 13.7, 16.8, 15.7, 13.0, 13.7, 10.2);
  { Skew coefficient for rainfall by month=
    1.93, 0.9, 1.16, 1.38, 3.17, 1.24, 1.61, 2.81, 2.72, 1.38, 5.34, 0.95 }
  Utils_SetMonthValues(params.skewCoeffForRainfallForMonth,
    1.93, 0.9, 1.16, 1.38, 3.17, 1.24, 1.61, 2.81, 2.72, 1.38, 5.34, 0.95);
  { Probability of wet day after dry day by month_frn=
    0.27, 0.29, 0.31, 0.28, 0.3, 0.26, 0.26, 0.25, 0.21, 0.2, 0.29, 0.31 }
  Utils_SetMonthValues(params.probWetDayAfterDryDayForMonth_frn,
    0.27, 0.29, 0.31, 0.28, 0.3, 0.26, 0.26, 0.25, 0.21, 0.2, 0.29, 0.31);
  { Probability of wet day after wet day by month_frn=
    0.48, 0.41, 0.45, 0.48, 0.49, 0.42, 0.39, 0.42, 0.45, 0.39, 0.43, 0.45 }
  Utils_SetMonthValues(params.probWetDayAfterWetDayForMonth_frn,
    0.48, 0.41, 0.45, 0.48, 0.49, 0.42, 0.39, 0.42, 0.45, 0.39, 0.43, 0.45);
  { Number of wet days by month=
    10.59, 9.56, 11.17, 10.5, 11.48, 9.29, 9.26, 9.34, 8.29, 7.65, 10.12, 11.17 }
  Utils_SetMonthValues(params.numWetDaysForMonth,
    10.59, 9.56, 11.17, 10.5, 11.48, 9.29, 9.26, 9.34, 8.29, 7.65, 10.12, 11.17);
  { Mean proportion of rain falls in first half hour by month_frn=
    7.9, 11.7, 7.9, 15.2, 20.6, 20.1, 22.9, 37.8, 21.6, 15.0, 25.4, 10.2 }
  Utils_SetMonthValues(params.meanPropRainInFirstHalfHourForMonth_frn,
    7.9, 11.7, 7.9, 15.2, 20.6, 20.1, 22.9, 37.8, 21.6, 15.0, 25.4, 10.2);
  { Mean daily radiation by month_MJPm2=
    130.0, 199.0, 290.0, 369.0, 432.0, 470.0, 459.0, 389.0, 331.0, 242.0, 147.0, 115.0 }
  Utils_SetMonthValues(params.dailyMeanRadiationForMonth_MJPm2,
    130.0, 199.0, 290.0, 369.0, 432.0, 470.0, 459.0, 389.0, 331.0, 242.0, 147.0, 115.0);
  { Mean daily relative humidity by month_frn=
    -5.0, -5.0, -2.78, 2.78, 8.33, 13.89, 16.67, 16.67, 13.33, 7.78, 1.11, -3.89 }
  Utils_SetMonthValues(params.dailyMeanRelHumForMonth_frn,
    -5.0, -5.0, -2.78, 2.78, 8.33, 13.89, 16.67, 16.67, 13.33, 7.78, 1.11, -3.89);
  { Mean daily wind speed by month_mPsec=
    4.83, 4.93, 5.27, 4.89, 4.42, 4.13, 3.95, 3.84, 3.97, 4.16, 4.44, 4.68 }
  Utils_SetMonthValues(params.dailyMeanWindSpeedForMonth_mPsec,
    4.83, 4.93, 5.27, 4.89, 4.42, 4.13, 3.95, 3.84, 3.97, 4.16, 4.44, 4.68);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 0,
    6.0, 6.0, 6.0, 5.0, 5.0, 4.0, 4.0, 5.0, 6.0, 6.0, 6.0, 5.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 1,
    8.0, 7.0, 7.0, 6.0, 6.0, 5.0, 5.0, 7.0, 10.0, 10.0, 8.0, 9.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 2,
    7.0, 8.0, 7.0, 6.0, 7.0, 5.0, 5.0, 6.0, 7.0, 8.0, 6.0, 7.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 3,
    3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 3.0, 3.0, 4.0, 4.0, 3.0, 2.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 4,
    2.0, 3.0, 5.0, 5.0, 6.0, 4.0, 3.0, 3.0, 4.0, 4.0, 3.0, 2.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 5,
    1.0, 3.0, 4.0, 6.0, 7.0, 6.0, 4.0, 3.0, 4.0, 3.0, 3.0, 1.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 6,
    1.0, 1.0, 3.0, 4.0, 5.0, 5.0, 4.0, 4.0, 3.0, 3.0, 2.0, 1.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 7,
    2.0, 2.0, 3.0, 4.0, 5.0, 7.0, 6.0, 6.0, 5.0, 3.0, 3.0, 2.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 8,
    3.0, 4.0, 3.0, 4.0, 5.0, 7.0, 8.0, 8.0, 6.0, 4.0, 4.0, 3.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 9,
    6.0, 5.0, 4.0, 7.0, 8.0, 10.0, 10.0, 10.0, 9.0, 8.0, 8.0, 7.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 10,
    10.0, 8.0, 6.0, 9.0, 11.0, 13.0, 15.0, 14.0, 13.0, 12.0, 12.0, 11.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 11,
    10.0, 9.0, 7.0, 7.0, 8.0, 8.0, 10.0, 8.0, 7.0, 8.0, 9.0, 11.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 12,
    10.0, 8.0, 8.0, 6.0, 5.0, 6.0, 7.0, 7.0, 7.0, 7.0, 10.0, 11.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 13,
    12.0, 12.0, 11.0, 9.0, 6.0, 7.0, 6.0, 6.0, 5.0, 7.0, 9.0, 12.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 14,
    11.0, 12.0, 12.0, 9.0, 7.0, 6.0, 6.0, 5.0, 6.0, 7.0, 9.0, 10.0);
  Utils_SetWindDirectionValues(params.windDirectionsForMonth_frn, 15,
    8.0, 8.0, 9.0, 7.0, 6.0, 5.0, 4.0, 5.0, 5.0, 6.0, 7.0, 6.0);
  Defaults_DefaultAndCalculateWeatherInputs(weather);
  end;
  end;

{ SOIL }
procedure InitAsSoilType_HagerstownB_MD(soil: GsSoilPatch);
  var
    layer: integer;
    inits: arraySoilLayers;
    layerInits: SoilTypeLayerInitsStructureArray;
  begin
  if soil = nil then
    raise Exception.create('Init as soil type: nil soil');
  with soil do
  begin
  setName('Hagerstown-B, MD');
  state.numLayers := 7; { set numlayers as number of layers there is data for }
  { this was the soil type in the PAIRR data file }
  { Soil type= HAGERSTOWN-B }
  { Depth by layer_m= 0.01, 0.15, 0.23, 0.58, 0.91, 1.31, 1.71, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0.01, 0.15, 0.23, 0.58, 0.91, 1.310, 1.710, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].depth_m := inits[layer];
  { Settled bulk density by layer_tPm3= 1.2, 1.2, 1.46, 1.42, 1.37, 1.32, 1.32, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 1.2, 1.2, 1.46, 1.42, 1.37, 1.32, 1.32, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].settledBulkDensity_tPm3 := inits[layer];
  { Wilting point by layer_mm= 0.176, 0.176, 0.202, 0.301, 0.298, 0.319, 0.319, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0.176, 0.176, 0.202, 0.301, 0.298, 0.319, 0.319, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initWiltingPoint_mPm := inits[layer];
  { Field capacity by layer_mm= 0.314, 0.314, 0.304, 0.413, 0.419, 0.448, 0.448, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0.314, 0.314, 0.304, 0.413, 0.419, 0.448, 0.448, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initFieldCapacity_mPm := inits[layer];
  { Sand content by layer_pct= 12.4, 12.4, 10.6, 7.6, 10.0, 10.9, 10.9, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 12.4, 12.4, 10.6, 7.6, 10.0, 10.9, 10.9, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].sandContent_pct := inits[layer];
  { Silt content by layer_pct= 59.3, 59.3, 51.8, 31.7, 30.4, 25.0, 25.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 59.3, 59.3, 51.8, 31.7, 30.4, 25.0, 25.0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].siltContent_pct := inits[layer];
  { Organic humus N concentration at simulation start by layer_gPt=
    2773.0, 2773.0, 830.0, 500.0, 270.0, 230.0, 230.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 2773.0, 2773.0, 830.0, 500.0, 270.0, 230.0, 230.0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicNTotalHumus_gPt := inits[layer];
  { Soil pH by layer= 6.5, 6.5, 6.6, 5.8, 5.6, 6.1, 6.1, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 6.5, 6.5, 6.6, 5.8, 5.6, 6.1, 6.1, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].soilpH := inits[layer];
  { Base forming cations by layer_cmolPkg= 20.9, 20.9, 11.3, 14.1, 15.0, 16.799999, 16.799999, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 20.9, 20.9, 11.3, 14.1, 15.0, 16.8, 16.8, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].baseFormingCations_cmolPkg := inits[layer];
  { Organic carbon at simulation start by layer_pct= 3.35, 3.35, 0.89, 0.39, 0.27, 0.23, 0.23, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 3.35, 3.35, 0.89, 0.39, 0.27, 0.23, 0.23, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicC_pct := inits[layer];
  { Calcium carbonate by layer_pct= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].calciumCarbonate_pct := inits[layer];
  { Cation exchange capacity by layer_cmolPkg= 20.9, 20.9, 11.7, 15.2, 16.4, 17.4, 17.4, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 20.9, 20.9, 11.7, 15.2, 16.4, 17.4, 17.4, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].cationExchangeCapacity_cmolPkg := inits[layer];
  { Rock content by layer_pct= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].rockContent_pct := inits[layer];
  { Nitrate by layer_kgPha= 10.0, 10.0, 5.0, 5.0, 5.0, 5.0, 5.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 10.0, 10.0, 5.0, 5.0, 5.0, 5.0, 5.0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initNitrate_gPt := inits[layer];
  { Labile P by layer_kgPha= 30.0, 30.0, 10.0, 10.0, 10.0, 10.0, 10.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 30.0, 30.0, 10.0, 10.0, 10.0, 10.0, 10.0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initLabileP_gPt := inits[layer];
  { Flat crop residue by layer_tPha= 0.034, 0.434, 0.445, 0.5, 0.165, 0.021, 0.021, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0.034, 0.434, 0.445, 0.5, 0.165, 0.021, 0.021, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].flatCropResidue_tPha := inits[layer];
  { Oven-dry bulk density by layer_tPm3= 1.28, 1.28, 1.56, 1.52, 1.47, 1.41, 1.41, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 1.28, 1.28, 1.56, 1.52, 1.47, 1.41, 1.41, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].bulkDensityOvenDry_tPm3 := inits[layer];
  { Today's P sorption coefficient by layer_frn= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].pSorptionCoeff_frn := inits[layer];
  { Saturated conductivity by layer_mmPhr= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].saturatedConductivity_mmPhr := inits[layer];
  { Organic P in humus by layer_kgPha= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicPHumus_gPt := inits[layer];
  end;
  Defaults_DefaultAndCalculateSoilInputs(soil, layerInits);
  Defaults_DefaultSoilParamsForVersionOne(soil);
  end;

procedure InitAsSoilType_Keyport_NJ(soil: GsSoilPatch);
  var
    layer: integer;
    inits: arraySoilLayers;
    layerInits: SoilTypeLayerInitsStructureArray;
  begin
  if soil = nil then
    raise Exception.create('Init as soil type: nil soil');
  with soil do
  begin
  setName('Keyport, NJ');
  state.numLayers := 10;
  { Soil type= KEYPORT }
  { Depth by layer_m= 0.01, 0.15, 0.28, 0.51, 0.69, 0.84, 1.09, 1.32, 1.68, 1.78 }
  Utils_SetLayerValues(inits, 0.01, 0.15, 0.28, 0.51, 0.69, 0.84, 1.09, 1.32, 1.68, 1.78);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].depth_m := inits[layer];
  { Settled bulk density by layer_tPm3= 1.3, 1.3, 1.3, 1.52, 1.53, 1.5, 1.35, 1.35, 1.34, 1.5 }
  Utils_SetLayerValues(inits, 1.3, 1.3, 1.3, 1.52, 1.53, 1.5, 1.35, 1.35, 1.34, 1.5);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].settledBulkDensity_tPm3 := inits[layer];
  { Wilting point by layer_mm= 0.142, 0.142, 0.142, 0.211, 0.198, 0.186, 0.249, 0.262, 0.238, 0.255 }
  Utils_SetLayerValues(inits, 0.142, 0.142, 0.142, 0.211, 0.198, 0.186, 0.249, 0.262, 0.238, 0.255);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initWiltingPoint_mPm := inits[layer];
  { Field capacity by layer_mm= 0.29, 0.29, 0.29, 0.301, 0.287, 0.281, 0.38, 0.396, 0.368, 0.351 }
  Utils_SetLayerValues(inits, 0.29, 0.29, 0.29, 0.301, 0.287, 0.281, 0.38, 0.396, 0.368, 0.351);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initFieldCapacity_mPm := inits[layer];
  { Sand content by layer_pct= 43.5, 43.5, 43.5, 23.6, 24.3, 19.5, 9.1, 7.6, 12.8, 9.5 }
  Utils_SetLayerValues(inits, 43.5, 43.5, 43.5, 23.6, 24.3, 19.5, 9.1, 7.6, 12.8, 9.5);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].sandContent_pct := inits[layer];
  { Silt content by layer_pct= 36.0, 36.0, 36.0, 36.1, 38.7, 46.6, 43.6, 42.0, 42.5, 39.8 }
  Utils_SetLayerValues(inits, 36.0, 36.0, 36.0, 36.1, 38.7, 46.6, 43.6, 42.0, 42.5, 39.8);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].siltContent_pct := inits[layer];
  { Organic humus N concentration at simulation start by layer_gPt=
    950.0, 950.0, 950.0, 380.0, 320.0, 370.0, 440.0, 440.0, 480.0, 470.0 }
  Utils_SetLayerValues(inits, 950.0, 950.0, 950.0, 380.0, 320.0, 370.0, 440.0, 440.0, 480.0, 470.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicNTotalHumus_gPt := inits[layer];
  { Soil pH by layer= 5.5, 5.5, 5.5, 4.8, 4.6, 4.6, 4.5, 4.4, 4.3, 4.3 }
  Utils_SetLayerValues(inits, 5.5, 5.5, 5.5, 4.8, 4.6, 4.6, 4.5, 4.4, 4.3, 4.3);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].soilpH := inits[layer];
  { Base forming cations by layer_cmolPkg= 5.6, 5.6, 5.6, 5.0, 3.8, 2.7, 3.3, 2.7, 2.5, 2.5 }
  Utils_SetLayerValues(inits, 5.6, 5.6, 5.6, 5.0, 3.8, 2.7, 3.3, 2.7, 2.5, 2.5);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].baseFormingCations_cmolPkg := inits[layer];
  { Organic carbon at simulation start by layer_pct= 1.26, 1.26, 1.26, 0.36, 0.35, 0.4, 0.36, 0.32, 0.31, 0.38 }
  Utils_SetLayerValues(inits, 1.26, 1.26, 1.26, 0.36, 0.35, 0.4, 0.36, 0.32, 0.31, 0.38);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicC_pct := inits[layer];
  { Calcium carbonate by layer_pct= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].calciumCarbonate_pct := inits[layer];
  { Cation exchange capacity by layer_cmolPkg= 9.9, 9.9, 9.9, 16.2, 16.8, 15.0, 21.8, 21.3, 21.1, 22.2 }
  Utils_SetLayerValues(inits, 9.9, 9.9, 9.9, 16.2, 16.8, 15.0, 21.8, 21.3, 21.1, 22.2);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].cationExchangeCapacity_cmolPkg := inits[layer];
  { Rock content by layer_pct= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].rockContent_pct := inits[layer];
  { Nitrate by layer_kgPha= 10.0, 10.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0 }
  Utils_SetLayerValues(inits, 10.0, 10.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initNitrate_gPt := inits[layer];
  { Labile P by layer_kgPha= 30.0, 30.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0 }
  Utils_SetLayerValues(inits, 30.0, 30.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0 );
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initLabileP_gPt := inits[layer];
  { Flat crop residue by layer_tPha= 0.034, 0.434, 0.518, 0.417, 0.156, 0.035, 0.005, 0.001, 0.001, 0.001 }
  Utils_SetLayerValues(inits, 0.034, 0.434, 0.518, 0.417, 0.156, 0.035, 0.005, 0.001, 0.001, 0.001);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].flatCropResidue_tPha := inits[layer];
  { Oven-dry bulk density by layer_tPm3= 1.39, 1.39, 1.39, 1.63, 1.64, 1.6, 1.44, 1.44, 1.43, 1.6 }
  Utils_SetLayerValues(inits, 1.39, 1.39, 1.39, 1.63, 1.64, 1.6, 1.44, 1.44, 1.43, 1.6);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].bulkDensityOvenDry_tPm3 := inits[layer];
  { Today's P sorption coefficient by layer_frn= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].pSorptionCoeff_frn := inits[layer];
  { Saturated conductivity by layer_mmPhr= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].saturatedConductivity_mmPhr := inits[layer];
  { Organic, P in humus by layer_kgPha= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicPHumus_gPt := inits[layer];
  end;
  Defaults_DefaultAndCalculateSoilInputs(soil, layerInits);
  Defaults_DefaultSoilParamsForVersionOne(soil);
  end;

procedure InitAsSoilType_Indianola_IA(soil: GsSoilPatch);
  var
    layer: integer;
    inits: arraySoilLayers;
    layerInits: SoilTypeLayerInitsStructureArray;
  begin
  if soil = nil then
    raise Exception.create('Init as soil type: nil soil');
  with soil do
  begin
  setName('Indianola, IA');
  state.numLayers := 9;
  {Soil type= INDIANOLA}
  { Depth by layer_m= 0.01, 0.15, 0.18, 0.3, 0.61, 0.9, 1.2, 1.4, 1.8, 0.0 }
  Utils_SetLayerValues(inits, 0.01, 0.15, 0.18, 0.3, 0.61, 0.9, 1.2, 1.4, 1.8, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].depth_m := inits[layer];
  { Settled bulk density by layer_tPm3= 0.86, 0.86, 1.03, 1.1, 1.32, 1.5, 1.5, 1.5, 1.5, 0.0  }
  Utils_SetLayerValues(inits, 0.86, 0.86, 1.03, 1.1, 1.32, 1.5, 1.5, 1.5, 1.5, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].settledBulkDensity_tPm3 := inits[layer];
  { Wilting point by layer_mm= 0.084, 0.084, 0.076, 0.074, 0.055, 0.038, 0.038, 0.041, 0.035, 0.0 }
  Utils_SetLayerValues(inits, 0.084, 0.084, 0.076, 0.074, 0.055, 0.038, 0.038, 0.041, 0.035, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initWiltingPoint_mPm := inits[layer];
  { Field capacity by layer_mm= 0.315, 0.315, 0.28, 0.278, 0.205, 0.14, 0.14, 0.147, 0.13, 0.0 }
  Utils_SetLayerValues(inits, 0.315, 0.315, 0.28, 0.278, 0.205, 0.14, 0.14, 0.147, 0.13, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initFieldCapacity_mPm := inits[layer];
  { Sand content by layer_pct= 80.6, 80.6, 81.6, 82.2, 88.3, 94.8, 94.8, 92.9, 97.2, 0.0 }
  Utils_SetLayerValues(inits, 80.6, 80.6, 81.6, 82.2, 88.3, 94.8, 94.8, 92.9, 97.2, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].sandContent_pct := inits[layer];
  { Silt content by layer_pct= 15.0, 15.0, 14.4, 14.5, 9.3, 3.9, 3.9, 5.3, 1.6, 0.0 }
  Utils_SetLayerValues(inits, 15.0, 15.0, 14.4, 14.5, 9.3, 3.9, 3.9, 5.3, 1.6, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].siltContent_pct := inits[layer];
  { Organic humus N concentration at simulation start by layer_gPt=
    1031.0, 1031.0, 780.0, 520.0, 470.0, 80.0, 80.0, 100.0, 100.0, 0.0 }
  Utils_SetLayerValues(inits, 1031.0, 1031.0, 780.0, 520.0, 470.0, 80.0, 80.0, 100.0, 100.0, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicNTotalHumus_gPt := inits[layer];
  { Base forming cations by layer_cmolPkg= 1.5, 1.5, 1.0, 1.0, 0.8, 0.8, 0.8, 0.7, 0.8, 0.0 }
  Utils_SetLayerValues(inits, 1.5, 1.5, 1.0, 1.0, 0.8, 0.8, 0.8, 0.7, 0.8, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].baseFormingCations_cmolPkg := inits[layer];
  { Soil pH by layer= 4.5, 4.5, 4.7, 5.0, 5.2, 5.3, 5.3, 5.5, 5.4, 0.0  }
  Utils_SetLayerValues(inits, 4.5, 4.5, 4.7, 5.0, 5.2, 5.3, 5.3, 5.5, 5.4, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].soilpH := inits[layer];
  { Organic carbon at simulation start by layer_pct= 2.88, 2.88, 1.96, 1.06, 0.47, 0.08, 0.08, 0.1, 0.1, 0.0 }
  Utils_SetLayerValues(inits, 2.88, 2.88, 1.96, 1.06, 0.47, 0.08, 0.08, 0.1, 0.1, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicC_pct := inits[layer];
  { Calcium carbonate by layer_pct= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].calciumCarbonate_pct := inits[layer];
  { Cation exchange capacity by layer_cmolPkg= 15.8, 15.8, 13.1, 8.8, 5.1, 1.8, 1.8, 2.5, 1.5, 0.0 }
  Utils_SetLayerValues(inits, 15.8, 15.8, 13.1, 8.8, 5.1, 1.8, 1.8, 2.5, 1.5, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].cationExchangeCapacity_cmolPkg := inits[layer];
  { Rock content by layer_pct= 2.5, 2.5, 3.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 2.5, 2.5, 3.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].rockContent_pct := inits[layer];
  { Nitrate by layer_kgPha= 10.0, 10.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 0.0 }
  Utils_SetLayerValues(inits, 10.0, 10.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initNitrate_gPt := inits[layer];
  { Labile P by layer_kgPha= 30.0, 30.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 0.0 }
  Utils_SetLayerValues(inits, 30.0, 30.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initLabileP_gPt := inits[layer];
  { Flat crop residue by layer_tPha= 0.034, 0.434, 0.366, 0.369, 0.298, 0.096, 0.096, 0.004, 0.001, 0.0 }
  Utils_SetLayerValues(inits, 0.034, 0.434, 0.366, 0.369, 0.298, 0.096, 0.096, 0.004, 0.001, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].flatCropResidue_tPha := inits[layer];
  { Oven-dry bulk density by layer_tPm3= 0.92, 0.92, 1.1, 1.18, 1.41, 1.6, 1.6, 1.6, 1.6, 0.0 }
  Utils_SetLayerValues(inits, 0.92, 0.92, 1.1, 1.18, 1.41, 1.6, 1.6, 1.6, 1.6, 0.0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].bulkDensityOvenDry_tPm3 := inits[layer];
  { Today's P sorption coefficient by layer_frn= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].pSorptionCoeff_frn := inits[layer];
  { Saturated conductivity by layer_mmPhr= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layers[layer].saturatedConductivity_mmPhr := inits[layer];
  { Organic P in humus by layer_kgPha= 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }
  Utils_SetLayerValues(inits, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  for layer := 0 to kMaxPossibleLayers - 1 do layerInits[layer].initOrganicPHumus_gPt := inits[layer];
  end;
  Defaults_DefaultAndCalculateSoilInputs(soil, layerInits);
  Defaults_DefaultSoilParamsForVersionOne(soil);
  end;

{ PLANT }
procedure SetDefaultsForAllPlants(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('SetDefaultsForAllPlants: nil plant');
  plant.possibleSize := 20;
  with plant.params do
    begin
    maxAnnualNFert_kgPha := 1000.0;
    probOfGerminationAfterFirstYear_frn := 0.2;
    vernalizationRequirement := kNoVernalization;
    minTempForVernalization_degC := 0.0;
    optTempForVernalization_degC := 0.0;
    maxTempForVernalization_degC := 0.0;
    thermalUnitsRequiredForVernalization := 0.0;
    fractionStorageOrganAllocationAtMaturity_frn := 0.0;
    heatUnitIndexAtStartOfStorageOrganAllocation := 0.0;
    end;
  end;

procedure SetDefaultsForFloralParams(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('SetDefaultsForAllPlants: nil plant');
  with plant.params do
    begin
    minTempForFloralInduction_degC := baseTemp_degC;
    optTempForFloralInduction_degC := optimalTemp_degC;
    maxTempForFloralInduction_degC := optimalTemp_degC + (optimalTemp_degC - baseTemp_degC);
    photothermalUnitsRequiredForFloralInduction := 13.0; 
    thermalUnitsRequiredForFloralInitiation := 7.0;
    end;
  end;

procedure InitAsCultivar_Corn(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Corn');
    seedWeight_g := 0.2;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 39.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 44.0;
    optimalTemp_degC := 35.0;
    baseTemp_degC := 8.0;
    maxLeafAreaIndex := 3.0;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.5;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.15, 0.05, 0.5, 0.95);
    leafAreaIndexDeclineRateFactor := 0.5;
    biomassAdjustmentIfLAIDecliningFactor := 0.5;
    leafResistIfVPDBelowThreshold_mPsec := 0.007;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 2.5;
    maxHeightUnsupported_m := 2.5;
    maxRootDepth_m := 2.0;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.047;
    nFractionAtHalfMaturity_frn := 0.0177;
    nFractionAtMaturity_frn := 0.0138;
    pFractionAtEmergence_frn := 0.0048;
    pFractionAtHalfMaturity_frn := 0.0018;
    pFractionAtMaturity_frn := 0.0014;
    windErosionFactorStandingLive := 0.433;
    windErosionFactorStandingDead := 0.4;
    windErosionFactorFlatResidue := 0.213;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.1, 15.0, 0.95);
    biomassToEnergyRatioVPDParam := 8.0;
    thresholdVaporPressureDeficit_kPa := 0.5;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1600.0;
    absoluteTempForFrostKill_degC := -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    SetDefaultsForFloralParams(plant);
    { short-day (long-night), but quantitative and weak } {misc}
    Utils_InitSCurveParam(plant.params.floralInductionParams, 5.0, 0.5, 10.0, 0.95); {10 from Coligado & Brown}
    minTempForFloralInduction_degC := baseTemp_degC;
    optTempForFloralInduction_degC := 25.0; {C&B}
    maxTempForFloralInduction_degC := optTempForFloralInduction_degC + (optTempForFloralInduction_degC - baseTemp_degC);
    photothermalUnitsRequiredForFloralInduction := 5.0; {C&B}
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Corn_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Sunflower(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Sunflower');
    seedWeight_g := 0.11;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 35.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 45.0;
    optimalTemp_degC := 25.0;
    baseTemp_degC := 6.0;
    maxLeafAreaIndex := 5.0;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.55;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.15, 0.01, 0.5, 0.95);
    leafAreaIndexDeclineRateFactor := 1.0;
    biomassAdjustmentIfLAIDecliningFactor := 1.0;
    leafResistIfVPDBelowThreshold_mPsec := 0.013;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 2.0;
    maxHeightUnsupported_m := 2.0;
    maxRootDepth_m := 2.2;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.05;
    nFractionAtHalfMaturity_frn := 0.023;
    nFractionAtMaturity_frn := 0.0146;
    pFractionAtEmergence_frn := 0.0063;
    pFractionAtHalfMaturity_frn := 0.0029;
    pFractionAtMaturity_frn := 0.0023;
    windErosionFactorStandingLive := 3.39;
    windErosionFactorStandingDead := 3.39;
    windErosionFactorFlatResidue := 1.61;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.15, 15.0, 0.95);
    biomassToEnergyRatioVPDParam := 7.0;
    thresholdVaporPressureDeficit_kPa := 0.5;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1700.0;
    absoluteTempForFrostKill_degC := -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    { day neutral } {misc}
    Utils_InitSCurveParam(plant.params.floralInductionParams, 5.0, 0.95, 12.0, 0.95);
    SetDefaultsForFloralParams(plant);
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Sunflower_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_EarlyPea(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Pea');
    seedWeight_g := 0.25;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 25.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 34.0;
    optimalTemp_degC := 27.0;
    baseTemp_degC := 7.0;
    maxLeafAreaIndex := 2.5;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.6;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.1, 0.05, 0.8, 0.95);
    leafAreaIndexDeclineRateFactor := 1.0;
    biomassAdjustmentIfLAIDecliningFactor := 1.0;
    leafResistIfVPDBelowThreshold_mPsec := 0.01;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 0.6;
    maxHeightUnsupported_m := 0.6;
    maxRootDepth_m := 1.1;
    isLegume := true;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.004;
    nFractionAtHalfMaturity_frn := 0.003;
    nFractionAtMaturity_frn := 0.0015;
    pFractionAtEmergence_frn := 0.003;
    pFractionAtHalfMaturity_frn := 0.002;
    pFractionAtMaturity_frn := 0.0015;
    windErosionFactorStandingLive := 1.27;
    windErosionFactorStandingDead := 0.6;
    windErosionFactorFlatResidue := 0.5;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.01, 15.0, 0.05);
    biomassToEnergyRatioVPDParam := 5.0;
    thresholdVaporPressureDeficit_kPa := 1.0;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1320.0;
    absoluteTempForFrostKill_degC :=  -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.1;  {short}
    { day neutral }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 5.0, 0.95, 12.0, 0.95);
    SetDefaultsForFloralParams(plant);
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_EarlyPea_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Onion(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Onion');
    seedWeight_g := 0.00328;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 30.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 20.0;
    optimalTemp_degC := 29.0;
    baseTemp_degC := 7.0;
    maxLeafAreaIndex := 1.5;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.6;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.15, 0.01, 0.5, 0.95);
    leafAreaIndexDeclineRateFactor := 2.0;
    biomassAdjustmentIfLAIDecliningFactor := 10.0;
    leafResistIfVPDBelowThreshold_mPsec := 0.007;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 0.2;
    maxHeightUnsupported_m := 0.2;
    maxRootDepth_m := 0.7;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.04;
    nFractionAtHalfMaturity_frn := 0.03;
    nFractionAtMaturity_frn := 0.002;
    pFractionAtEmergence_frn := 0.0021;
    pFractionAtHalfMaturity_frn := 0.002;
    pFractionAtMaturity_frn := 0.0019;
    windErosionFactorStandingLive := 1.138;
    windErosionFactorStandingDead := 0.603;
    windErosionFactorFlatResidue := 0.332;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.001, 15.0, 0.01);
    biomassToEnergyRatioVPDParam := 10.0;
    thresholdVaporPressureDeficit_kPa := 1.0;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1320.0; {cfk from pea}
    absoluteTempForFrostKill_degC :=  -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    { short-day, quantitative }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 8.0, 0.5, 20.0, 0.95); {Brw}
    SetDefaultsForFloralParams(plant);
    thermalUnitsRequiredForFloralInitiation := 19.0; {Brw}
    { vernalization }
    vernalizationRequirement := kObligateVernalization;
    minTempForVernalization_degC := 2.5;   {Brw}
    optTempForVernalization_degC := 9;     {Brw}
    maxTempForVernalization_degC := 16;    {Brw}
    thermalUnitsRequiredForVernalization := 60.0; {Brw}
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    fractionStorageOrganAllocationAtMaturity_frn := 0.5;
    heatUnitIndexAtStartOfStorageOrganAllocation := 0.5;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Onion_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_GreenBean(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Green bean');
    seedWeight_g := 0.2;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 25.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 34.0;
    optimalTemp_degC := 27.0;
    baseTemp_degC := 10.0;
    maxLeafAreaIndex := 1.5;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.9;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.1, 0.05, 0.8, 0.95);
    leafAreaIndexDeclineRateFactor := 1.0;
    biomassAdjustmentIfLAIDecliningFactor := 1.0;
    leafResistIfVPDBelowThreshold_mPsec := 0.01;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 0.6;
    maxHeightUnsupported_m := 0.6;
    maxRootDepth_m := 1.1;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.004;
    nFractionAtHalfMaturity_frn := 0.003;
    nFractionAtMaturity_frn := 0.0015;
    pFractionAtEmergence_frn := 0.004;
    pFractionAtHalfMaturity_frn := 0.0035;
    pFractionAtMaturity_frn := 0.0015;
    windErosionFactorStandingLive := 1.27;
    windErosionFactorStandingDead := 0.6;
    windErosionFactorFlatResidue := 0.73;
    isLegume := true;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.1, 15.0, 0.95);
    biomassToEnergyRatioVPDParam := 5.0;
    thresholdVaporPressureDeficit_kPa := 1.0;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1320.0;
    absoluteTempForFrostKill_degC :=  -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.1; {short}
    { day neutral }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 5.0, 0.95, 12.0, 0.95);
    SetDefaultsForFloralParams(plant);
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_GreenBean_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Lettuce(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Lettuce');
    seedWeight_g := 0.0011;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 23.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 25.0;
    optimalTemp_degC := 18.0;
    baseTemp_degC := 0.0;
    maxLeafAreaIndex := 4.2;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 1.0;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.25, 0.23, 0.4, 0.86);
    leafAreaIndexDeclineRateFactor := 0.1;
    biomassAdjustmentIfLAIDecliningFactor := 0.5;
    leafResistIfVPDBelowThreshold_mPsec := 0.0122;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.86;
    maxHeightSupported_m := 0.2;
    maxHeightUnsupported_m := 0.2;
    maxRootDepth_m := 0.8;
    minCropManagementFactor := 0.01;
    nFractionAtEmergence_frn := 0.036;
    nFractionAtHalfMaturity_frn := 0.025;
    nFractionAtMaturity_frn := 0.021;
    pFractionAtEmergence_frn := 0.0084;
    pFractionAtHalfMaturity_frn := 0.0032;
    pFractionAtMaturity_frn := 0.0019;
    windErosionFactorStandingLive := 2.0;
    windErosionFactorStandingDead := 1.0;
    windErosionFactorFlatResidue := 0.9;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.01, 15.0, 0.95);
    biomassToEnergyRatioVPDParam := 8.0;
    thresholdVaporPressureDeficit_kPa := 0.5;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1000.0; {cfk from pea}
    absoluteTempForFrostKill_degC :=  -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    { long-day, quantitative }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 12.0, 0.5, 5.0, 0.95);
    SetDefaultsForFloralParams(plant);
     { vernalization, quantitative }
    vernalizationRequirement := kQuantitativeVernalization;
    photothermalUnitsRequiredForFloralInduction := 40.0; {set this high to avoid first-year flowering}
    minTempForVernalization_degC := 0.0;
    optTempForVernalization_degC := 5.0;
    maxTempForVernalization_degC := 10.0;
    thermalUnitsRequiredForVernalization := 30.0;
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Lettuce_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Spinach(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Spinach');
    seedWeight_g := 0.01;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 30.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 95.0;
    optimalTemp_degC := 24.0;
    baseTemp_degC := 4.0;
    maxLeafAreaIndex := 4.2;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.95;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.1, 0.05, 0.9, 0.95);
    leafAreaIndexDeclineRateFactor := 0.1;
    biomassAdjustmentIfLAIDecliningFactor := 0.1;
    leafResistIfVPDBelowThreshold_mPsec := 0.01;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.86;
    maxHeightSupported_m := 0.3;
    maxHeightUnsupported_m := 0.3;
    maxRootDepth_m := 0.7;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.062;
    nFractionAtHalfMaturity_frn := 0.04;
    nFractionAtMaturity_frn := 0.03;
    pFractionAtEmergence_frn := 0.005;
    pFractionAtHalfMaturity_frn := 0.004;
    pFractionAtMaturity_frn := 0.0035;
    windErosionFactorStandingLive := 1.27;
    windErosionFactorStandingDead := 0.6;
    windErosionFactorFlatResidue := 0.73;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.001, 15.0, 0.01);
    biomassToEnergyRatioVPDParam := 5.0;
    thresholdVaporPressureDeficit_kPa := 1.0;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1320.0; {cfk from pea}
    absoluteTempForFrostKill_degC :=  -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    { long-day, obligate mostly }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 12.0, 0.2, 5.0, 0.95);
    SetDefaultsForFloralParams(plant);
    { vernalization, quantitative }
    vernalizationRequirement := kQuantitativeVernalization;
    photothermalUnitsRequiredForFloralInduction := 40.0; {set this high to avoid first-year flowering}
    minTempForVernalization_degC := 0.0;
    optTempForVernalization_degC := 5.0;
    maxTempForVernalization_degC := 10.0;
    thermalUnitsRequiredForVernalization := 30.0;
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Spinach_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Tomato(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Tomato');
    seedWeight_g := 0.00294;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 30.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 39.0;
    optimalTemp_degC := 27.0;
    baseTemp_degC := 10.0;
    maxLeafAreaIndex := 3.0;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.95;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.15, 0.05, 0.5, 0.95);
    leafAreaIndexDeclineRateFactor := 0.1;
    biomassAdjustmentIfLAIDecliningFactor := 0.1;
    leafResistIfVPDBelowThreshold_mPsec := 0.007;
    aluminumTolerance_pct := 30.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 0.8;
    maxHeightUnsupported_m := 0.1;
    maxRootDepth_m := 1.5;
    minCropManagementFactor := 0.03;
    nFractionAtEmergence_frn := 0.0663;
    nFractionAtHalfMaturity_frn := 0.03;
    nFractionAtMaturity_frn := 0.025;
    pFractionAtEmergence_frn := 0.0053;
    pFractionAtHalfMaturity_frn := 0.035;
    pFractionAtMaturity_frn := 0.0025;
    windErosionFactorStandingLive := 3.39;
    windErosionFactorStandingDead := 3.39;
    windErosionFactorFlatResidue := 1.61;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.1, 15.0, 0.95);
    biomassToEnergyRatioVPDParam := 8.0;
    thresholdVaporPressureDeficit_kPa := 0.5;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1500.0;
    absoluteTempForFrostKill_degC := -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.05; {juvenile phase short - Evans}
    { day neutral }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 5.0, 0.95, 12.0, 0.95);
    SetDefaultsForFloralParams(plant);
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Tomato_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Pepper(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Pepper');
    seedWeight_g := 0.00625;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 30.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 39.0;
    optimalTemp_degC := 27.0;
    baseTemp_degC := 18.0;
    maxLeafAreaIndex := 5.0;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.6;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.15, 0.05, 0.5, 0.95);
    leafAreaIndexDeclineRateFactor := 0.1;
    biomassAdjustmentIfLAIDecliningFactor := 0.1;
    leafResistIfVPDBelowThreshold_mPsec := 0.007;
    aluminumTolerance_pct := 30.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 0.8;
    maxHeightUnsupported_m := 0.8;
    maxRootDepth_m := 1.1;
    minCropManagementFactor := 0.03;
    nFractionAtEmergence_frn := 0.06;
    nFractionAtHalfMaturity_frn := 0.035;
    nFractionAtMaturity_frn := 0.025;
    pFractionAtEmergence_frn := 0.0053;
    pFractionAtHalfMaturity_frn := 0.002;
    pFractionAtMaturity_frn := 0.0012;
    windErosionFactorStandingLive := 3.39;
    windErosionFactorStandingDead := 3.39;
    windErosionFactorFlatResidue := 1.61;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.001, 15.0, 0.01);
    biomassToEnergyRatioVPDParam := 8.0;
    thresholdVaporPressureDeficit_kPa := 0.5;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1300.0; {cfk from tomato}
    absoluteTempForFrostKill_degC := -3.0;
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    { day neutral }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 5.0, 0.95, 12.0, 0.95);
    SetDefaultsForFloralParams(plant);
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Pepper_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Carrot(plant: GsPlant);
  begin
  if plant = nil then
    raise Exception.create('Init as cultivar: nil plant');
  with plant.params do
    begin
    plant.setName('Carrot');
    seedWeight_g := 0.0012;
    SetDefaultsForAllPlants(plant);
    biomassToEnergyRatio_kgPhaPMJ := 30.0;
    biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 20.0;
    optimalTemp_degC := 24.0;
    baseTemp_degC := 7.0;
    maxLeafAreaIndex := 3.5;
    fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := 0.6;
    Utils_InitSCurveParam(plant.params.heatUnitFactorParamsForLAI, 0.15, 0.01, 0.5, 0.95);
    leafAreaIndexDeclineRateFactor := 2.0;
    biomassAdjustmentIfLAIDecliningFactor := 10.0;
    leafResistIfVPDBelowThreshold_mPsec := 0.007;
    aluminumTolerance_pct := 50.0;
    criticalAerationFactor_frn := 0.85;
    maxHeightSupported_m := 0.3;
    maxHeightUnsupported_m := 0.3;
    maxRootDepth_m := 1.1;
    minCropManagementFactor := 0.2;
    nFractionAtEmergence_frn := 0.055;
    nFractionAtHalfMaturity_frn := 0.0075;
    nFractionAtMaturity_frn := 0.0012;
    pFractionAtEmergence_frn := 0.006;
    pFractionAtHalfMaturity_frn := 0.003; { lower bound; file had 0.0 }
    pFractionAtMaturity_frn := 0.002;
    windErosionFactorStandingLive := 1.138;
    windErosionFactorStandingDead := 0.603;
    windErosionFactorFlatResidue := 0.332;
    Utils_InitSCurveParam(plant.params.frostReductionFactorParams, 5.0, 0.01, 15.0, 0.1);
    biomassToEnergyRatioVPDParam := 10.0;
    thresholdVaporPressureDeficit_kPa := 1.0;
    fractionOfMaxLeafConductForHighVPD.x := 4.0;
    fractionRootWtAtEmergence_frn := 0.4;
    fractionRootWtAtMaturity_frn := 0.2;
    potHeatUnitsReqForMaturation := 1460.0;
    absoluteTempForFrostKill_degC :=  -5.0;
    { reproductive params }
    heatUnitIndexAtEndOfVegetativePhase := 0.2;
    { long-day, quantitative }
    Utils_InitSCurveParam(plant.params.floralInductionParams, 12.0, 0.5, 5.0, 0.95);
    SetDefaultsForFloralParams(plant);
    { vernalization, quantitative }
    vernalizationRequirement := kQuantitativeVernalization;
    photothermalUnitsRequiredForFloralInduction := 40.0; {set this high to avoid first-year flowering}
    minTempForVernalization_degC := 0.0;
    optTempForVernalization_degC := 5.0;
    maxTempForVernalization_degC := 10.0;
    thermalUnitsRequiredForVernalization := 30.0;
    fractionReproductiveAllocationAtMaturity_frn := 0.7;
    reproductiveBiomassDecayAtPlantMaturity := 0.05;
    end;
  Defaults_DefaultAndCalculatePlantInputs(plant);
  plant.SetDefaultsForSeed; { should only be called if not a seedling }
  initAsCultivar_Carrot_drawingPlant(plant.drawingPlant);
  end;

procedure InitAsCultivar_Corn_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 200.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 100.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 1.0 * g_to_kg;
  pFlower[kGenderMale].optimalBiomass_kg := 0.001 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 100 * g_to_kg;
  pInflor[kGenderMale].optimalBiomass_kg := 400 * g_to_kg;
  pFruit.optimalBiomass_kg := 1000.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 5.0;
    numApicalInflors := 1;
    numAxillaryInflors := 2;
    gender := kGenderHermaphroditic;
    lineDivisions := 3;
    isDicot := false;
    maleFlowersAreSeparate := true;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\sl_1.tdo');
    faceColor := support_rgb(0, 150, 0);
    backfaceColor := support_rgb(0, 220, 0);
    scale := 100;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_corn.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 50;
    maxDaysToGrow := 70;
    scaleAtOptimalBiomass := 80;
    petioleLengthAtOptimalBiomass_mm := 1;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 25;
    petioleColor := faceColor;
    compoundNumLeaflets := 1;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20);
    backfaceColor := support_rgb(0, 255, 0);
    meanderIndex := 0;
    flexibilityIndex := 0;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 20;
    maxDaysToAccumulateBiomass := 140;
    minDaysToExpand := 40;
    maxDaysToExpand := 160;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 30;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 8;
    widthMultiplierDueToBiomassAccretion := 10;
    widthMultiplierDueToExpansion := 1.5;
    lengthMultiplierDueToBolting := 1;
    minDaysToBolt := 1;
    maxDaysToBolt := 1;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_corn.tdo');
    faceColor := support_rgb(0, 200, 0);
    backfaceColor := support_rgb(0, 255, 0);
    scaleAtFullSize := 2;
    numPetals := 5;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 100;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_corn.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 2;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(200, 200, 0);
    backfaceColor := support_rgb(255, 255, 0);
    scaleAtFullSize := 0.1;
    numPetals := 1;
    petalsAreRadiallyArranged := false;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.0;
    minDaysToGrow := 0;
    maxDaysToGrowIfOverMinFraction := 0;
    daysBeforeDrop := 30;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_corn.tdo');
    budFaceColor := support_rgb(200, 200, 0);
    budBackfaceColor := support_rgb(255, 255, 0);
    budScaleAtFullSize := 0.01;
    budNumPetals := 1;
    budPetalsAreRadiallyArranged := false;
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 15;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.1;
    minDaysToGrow := 10;
    maxDaysToGrow := 20;
    peduncleLength_mm := 5;
    internodeLength_mm := 5;
    internodeWidth_mm := 5;
    pedicelLength_mm := 5;
    pedicelAngle := 0;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 1;
    daysToAllFlowersCreated := 1;
    numBranches := 0;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := false;
    end;
  with pInflor[kGenderMale] do
    begin
    stalkColor := support_rgb(180, 180, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 15;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 20;
    peduncleLength_mm := 5;
    internodeLength_mm := 5;
    internodeWidth_mm := 2;
    pedicelLength_mm := 5;
    pedicelAngle := 16;
    branchAngle := 64;
    terminalStalkLength_mm := 40;
    numFlowers := 10;
    daysToAllFlowersCreated := 5;
    numBranches := 4;
    isHead := false;
    isTerminal := true;
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
    branchingIndex := 0;
    branchingDistance := 1;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_corn.tdo');
    unripeFaceColor := support_rgb(0, 150, 0);
    unripeBackfaceColor := support_rgb(0, 100, 0);
    ripeFaceColor := support_rgb(0, 150, 0);
    ripeBackfaceColor := support_rgb(0, 100, 0);
    scaleAtFullSize := 40;
    stalkStrengthIndex := 400;
 	  daysToRipen := 20;
    numSections := 4;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit] :=
    Domain.templateManager.findHarvestItemTemplate('Corn');
  end;
  end;

procedure InitAsCultivar_Sunflower_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 100.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 80.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 0.01 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 100 * g_to_kg;
  pFruit.optimalBiomass_kg := 0.01 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 5.0;
    numApicalInflors := 1;
    numAxillaryInflors := 0;
    gender := kGenderHermaphroditic;
    lineDivisions := 3;
    isDicot := true;
    maleFlowersAreSeparate := false;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_sunfl.tdo');
    faceColor := support_rgb(0, 150, 0);
    backfaceColor := support_rgb(0, 220, 0);
    scale := 80;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_sunfl.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 50;
    maxDaysToGrow := 70;
    scaleAtOptimalBiomass := 80;
    petioleLengthAtOptimalBiomass_mm := 20;
    petioleWidthAtOptimalBiomass_mm := 3;
    petioleAngle := 45;
    petioleColor := faceColor;
    compoundNumLeaflets := 1;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20);
    backfaceColor := support_rgb(0, 255, 0);
    meanderIndex := 0;
    flexibilityIndex := 0;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 10;
    maxDaysToCreateInternodeIfOverMinFraction := 30;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 20;
    maxDaysToAccumulateBiomass := 140;
    minDaysToExpand := 40;
    maxDaysToExpand := 160;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 70;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 8;
    widthMultiplierDueToBiomassAccretion := 10;
    widthMultiplierDueToExpansion := 1.5;
    lengthMultiplierDueToBolting := 1;
    minDaysToBolt := 1;
    maxDaysToBolt := 1;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(200, 100, 0);
    backfaceColor := support_rgb(255, 128, 0);
    scaleAtFullSize := 10;
    numPetals := 1;
    petalsAreRadiallyArranged := false;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 100;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 0.005;
    budNumPetals := 1;
    budPetalsAreRadiallyArranged := false;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(168, 128, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 15;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.1;
    minDaysToGrow := 10;
    maxDaysToGrow := 20;
    peduncleLength_mm := 10;
    internodeLength_mm := 1;
    internodeWidth_mm := 5;
    pedicelLength_mm := 1;
    pedicelAngle := 0;
    branchAngle := 0;
    terminalStalkLength_mm := 60;
    numFlowers := 20;
    daysToAllFlowersCreated := 10;
    numBranches := 0;
    isHead := true;
    isTerminal := true;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := false;
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    faceColor := support_rgb(0, 200, 0);
    backfaceColor := support_rgb(0, 150, 0);
    scale := 2;
    end;
  with pMeristem do
    begin
    branchingIndex := 0;
    branchingDistance := 1;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_corn.tdo');
    unripeFaceColor := support_rgb(150, 75, 0);
    unripeBackfaceColor := support_rgb(200, 100, 0);
    ripeFaceColor := support_rgb(150, 75, 0);
    ripeBackfaceColor := support_rgb(200, 100, 0);
    scaleAtFullSize := 15;
    stalkStrengthIndex := 100;
 	  daysToRipen := 20;
    numSections := 1;
    sectionsAreRadiallyArranged := false;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeWholePlant] :=
    Domain.templateManager.findHarvestItemTemplate('Sunflower');
  pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeFruit] := true;
  pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeFemaleInflorescence] := true;
  end;
  end;

procedure InitAsCultivar_EarlyPea_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 50.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 20.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 5.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 50.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 50.0 * g_to_kg;
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
    scale := 20;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_tulip.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 2;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 12;
    petioleLengthAtOptimalBiomass_mm := 20;
    petioleWidthAtOptimalBiomass_mm := 2;
    petioleAngle := 70;
    petioleColor := faceColor;
    compoundNumLeaflets := 1;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20);
    backfaceColor := support_rgb(0, 255, 20);
    meanderIndex := 10;
    flexibilityIndex := 30;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 5;
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
    faceColor := support_rgb(220, 220, 220);
    backfaceColor := support_rgb(255, 255, 255);
    scaleAtFullSize := 8;
    numPetals := 5;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 0.05;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 3;
    maxDaysToCreateInflorescenceIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.3;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    peduncleLength_mm := 6;
    internodeLength_mm := 6;
    internodeWidth_mm := 1;
    pedicelLength_mm := 10;
    pedicelAngle := 16;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 5;
    daysToAllFlowersCreated := 40;
    numBranches := 0;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
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
    branchingIndex := 20;
    branchingDistance := 5;
    determinateProbability := 0.8;
	  branchingIsSympodial := false;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    unripeFaceColor := support_rgb(0, 200, 0);
    unripeBackfaceColor := support_rgb(0, 255, 0);
    ripeFaceColor := support_rgb(0, 150, 0);
    ripeBackfaceColor := support_rgb(0, 200, 0);
    scaleAtFullSize := 7;
    stalkStrengthIndex := 100;
 	  daysToRipen := 10;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 5;
    maxDaysToGrow := 15;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit] :=
    Domain.templateManager.findHarvestItemTemplate('Pea');
  end;
  end;

procedure InitAsCultivar_GreenBean_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 50.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 20.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 5.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 50.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 50.0 * g_to_kg;
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
    scale := 20;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_sunfl.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 2;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 20;
    petioleLengthAtOptimalBiomass_mm := 9;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 56;
    petioleColor := faceColor;
    compoundNumLeaflets := 3;
    compoundPetioleToRachisRatio := 2.0;
    compoundPinnateOrPalmate := kCompoundLeafPinnate;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20);
    backfaceColor := support_rgb(0, 255, 20);
    meanderIndex := 10;
    flexibilityIndex := 30;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 5;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 1;
    widthMultiplierDueToBiomassAccretion := 3;
    widthMultiplierDueToExpansion := 10;
    lengthMultiplierDueToBolting := 1;
    minDaysToBolt := 1;
    maxDaysToBolt := 1;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(220, 220, 220);
    backfaceColor := support_rgb(255, 255, 255);
    scaleAtFullSize := 8;
    numPetals := 5;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 0.05;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 3;
    maxDaysToCreateInflorescenceIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.3;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    peduncleLength_mm := 4;
    internodeLength_mm := 3;
    internodeWidth_mm := 1;
    pedicelLength_mm := 5;
    pedicelAngle := 30;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 5;
    daysToAllFlowersCreated := 40;
    numBranches := 1;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
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
    branchingIndex := 20;
    branchingDistance := 5;
    determinateProbability := 0.8;
	  branchingIsSympodial := false;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_carrot.tdo');
    unripeFaceColor := support_rgb(0, 200, 0);
    unripeBackfaceColor := support_rgb(0, 255, 0);
    ripeFaceColor := support_rgb(0, 150, 0);
    ripeBackfaceColor := support_rgb(0, 200, 0);
    scaleAtFullSize := 7;
    stalkStrengthIndex := 100;
 	  daysToRipen := 10;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 5;
    maxDaysToGrow := 15;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit] :=
    Domain.templateManager.findHarvestItemTemplate('Green bean');
  end;
  end;

procedure InitAsCultivar_Tomato_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 100.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 50.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 5.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 50.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 200.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 16.0;
    numApicalInflors := 0;
    numAxillaryInflors := 6;
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
    scale := 30;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\sl_4.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 40;
    petioleLengthAtOptimalBiomass_mm := 4;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 10;
    petioleColor := faceColor;
    compoundNumLeaflets := 7;
    compoundPetioleToRachisRatio := 1.3;
    compoundPinnateOrPalmate := kCompoundLeafPinnate;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20);
    backfaceColor := support_rgb(0, 255, 20);
    meanderIndex := 20;
    flexibilityIndex := 50;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 20;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 3;
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
    scaleAtFullSize := 5;
    numPetals := 5;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 1;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 3;
    maxDaysToCreateInflorescenceIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.3;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    peduncleLength_mm := 10;
    internodeLength_mm := 10;
    internodeWidth_mm := 0.3;
    pedicelLength_mm := 10;
    pedicelAngle := 32;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 5;
    daysToAllFlowersCreated := 40;
    numBranches := 0;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 0.1;
    branchingDistance := 1;
    determinateProbability := 0.5;
	  branchingIsSympodial := true;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    unripeFaceColor := support_rgb(50, 200, 50);
    unripeBackfaceColor := support_rgb(50, 255, 50);
    ripeFaceColor := support_rgb(150, 0, 0);
    ripeBackfaceColor := support_rgb(100, 0, 0);
    scaleAtFullSize := 20;
    stalkStrengthIndex := 50;
 	  daysToRipen := 20;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit] :=
    Domain.templateManager.findHarvestItemTemplate('Tomato');
  end;
  end;

procedure InitAsCultivar_Pepper_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 100.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 50.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 5.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 50.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 200.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 25.0;
    numApicalInflors := 0;
    numAxillaryInflors := 6;
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
    scale := 30;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_sunfl.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 30;
    petioleLengthAtOptimalBiomass_mm := 4;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 10;
    petioleColor := faceColor;
    compoundNumLeaflets := 7;
    compoundPetioleToRachisRatio := 1.3;
    compoundPinnateOrPalmate := kCompoundLeafPinnate;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 128, 20);
    backfaceColor := support_rgb(0, 255, 20);
    meanderIndex := 20;
    flexibilityIndex := 50;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 4;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 3;
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
    scaleAtFullSize := 5;
    numPetals := 5;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 1;
    budNumPetals := 5;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 0);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 3;
    maxDaysToCreateInflorescenceIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.3;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    peduncleLength_mm := 10;
    internodeLength_mm := 10;
    internodeWidth_mm := 0.3;
    pedicelLength_mm := 10;
    pedicelAngle := 32;
    branchAngle := 0;
    terminalStalkLength_mm := 0;
    numFlowers := 5;
    daysToAllFlowersCreated := 40;
    numBranches := 0;
    isHead := false;
    isTerminal := false;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 0.9;
    branchingDistance := 3;
    determinateProbability := 0.8;
	  branchingIsSympodial := false;
    branchingIsAlternate := true;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    unripeFaceColor := support_rgb(50, 200, 50);
    unripeBackfaceColor := support_rgb(50, 255, 50);
    ripeFaceColor := support_rgb(0, 150, 0);
    ripeBackfaceColor := support_rgb(0, 100, 0);
    scaleAtFullSize := 20;
    stalkStrengthIndex := 50;
 	  daysToRipen := 40;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit] :=
    Domain.templateManager.findHarvestItemTemplate('Pepper');
  end;
  end;

procedure InitAsCultivar_Carrot_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 30.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 5.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 1.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 200.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 1.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 5.0;
    numApicalInflors := 1;
    numAxillaryInflors := 0;
    gender := kGenderHermaphroditic;
    lineDivisions := 1;
    isDicot := true;
    maleFlowersAreSeparate := false;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_carrot.tdo');
    faceColor := support_rgb(0, 150, 50);
    backfaceColor := support_rgb(0, 220, 50);
    scale := 2;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_carrot.tdo');
    faceColor := support_rgb(0, 128, 50);
    backfaceColor := support_rgb(0, 200, 50);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 30;
    petioleLengthAtOptimalBiomass_mm := 20;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 24;
    petioleColor := faceColor;
    compoundNumLeaflets := 3;
    compoundPetioleToRachisRatio := 10.0;
    compoundPinnateOrPalmate := kCompoundLeafPinnate;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(230, 115, 0);
    backfaceColor := support_rgb(200, 100, 0);
    meanderIndex := 5;
    flexibilityIndex := 0;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 0.1;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 2;
    widthMultiplierDueToBiomassAccretion := 10;
    widthMultiplierDueToExpansion := 3;
    lengthMultiplierDueToBolting := 5;
    minDaysToBolt := 10;
    maxDaysToBolt := 30;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(150, 150, 0);
    backfaceColor := support_rgb(220, 220, 0);
    scaleAtFullSize := 5;
    numPetals := 3;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 1;
    budNumPetals := 3;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 50);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 30;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    peduncleLength_mm := 25;
    internodeLength_mm := 10;
    internodeWidth_mm := 1;
    pedicelLength_mm := 10;
    pedicelAngle := 32;
    branchAngle := 48;
    terminalStalkLength_mm := 100;
    numFlowers := 10;
    daysToAllFlowersCreated := 20;
    numBranches := 3;
    isHead := false;
    isTerminal := true;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 0;
    branchingDistance := 0;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := false;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    unripeFaceColor := support_rgb(50, 200, 80);
    unripeBackfaceColor := support_rgb(50, 255, 80);
    ripeFaceColor := support_rgb(150, 0, 80);
    ripeBackfaceColor := support_rgb(100, 0, 80);
    scaleAtFullSize := 5;
    stalkStrengthIndex := 30;
 	  daysToRipen := 20;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\r_car.tdo');
    showsAboveGround := true;
    pRoot.scaleAtFullSize := 0.00001;
    pRoot.faceColor := support_rgb(240, 120, 30);
    pRoot.backfaceColor := support_rgb(240, 120, 30);
   end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeRoot] :=
    Domain.templateManager.findHarvestItemTemplate('Carrot');
  end;
  end;

procedure InitAsCultivar_Onion_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 30.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 5.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 1.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 200.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 1.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 5.0;
    numApicalInflors := 1;
    numAxillaryInflors := 0;
    gender := kGenderHermaphroditic;
    lineDivisions := 1;
    isDicot := true;
    maleFlowersAreSeparate := false;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_carrot.tdo');
    faceColor := support_rgb(0, 150, 50);
    backfaceColor := support_rgb(0, 220, 50);
    scale := 2;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_carrot.tdo');
    faceColor := support_rgb(0, 128, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 30;
    petioleLengthAtOptimalBiomass_mm := 20;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 10;
    petioleColor := faceColor;
    compoundNumLeaflets := 1;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 115, 0);
    backfaceColor := support_rgb(0, 100, 0);
    meanderIndex := 5;
    flexibilityIndex := 0;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 0.1;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 2;
    widthMultiplierDueToBiomassAccretion := 10;
    widthMultiplierDueToExpansion := 3;
    lengthMultiplierDueToBolting := 5;
    minDaysToBolt := 10;
    maxDaysToBolt := 30;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(150, 150, 0);
    backfaceColor := support_rgb(220, 220, 0);
    scaleAtFullSize := 5;
    numPetals := 3;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 1;
    budNumPetals := 3;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 50);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 30;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    peduncleLength_mm := 25;
    internodeLength_mm := 10;
    internodeWidth_mm := 1;
    pedicelLength_mm := 10;
    pedicelAngle := 32;
    branchAngle := 48;
    terminalStalkLength_mm := 100;
    numFlowers := 10;
    daysToAllFlowersCreated := 20;
    numBranches := 3;
    isHead := false;
    isTerminal := true;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 0;
    branchingDistance := 0;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := false;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    unripeFaceColor := support_rgb(50, 200, 80);
    unripeBackfaceColor := support_rgb(50, 255, 80);
    ripeFaceColor := support_rgb(150, 0, 80);
    ripeBackfaceColor := support_rgb(100, 0, 80);
    scaleAtFullSize := 5;
    stalkStrengthIndex := 30;
 	  daysToRipen := 20;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\r_car.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeWholePlant] :=
    Domain.templateManager.findHarvestItemTemplate('Onion');
  pGeneral.storageOrganIsLumpedInWith := kHarvestPartTypeWholePlant;
  end;
  end;

procedure InitAsCultivar_Lettuce_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 60.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 5.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 1.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 200.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 1.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 5.0;
    numApicalInflors := 1;
    numAxillaryInflors := 0;
    gender := kGenderHermaphroditic;
    lineDivisions := 1;
    isDicot := true;
    maleFlowersAreSeparate := false;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_tomato.tdo');
    faceColor := support_rgb(0, 150, 50);
    backfaceColor := support_rgb(0, 220, 50);
    scale := 2;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_tomato.tdo');
    faceColor := support_rgb(192, 255, 192);
    backfaceColor := support_rgb(0, 255, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 26;
    petioleLengthAtOptimalBiomass_mm := 20;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 30;
    petioleColor := faceColor;
    compoundNumLeaflets := 3;
    compoundPetioleToRachisRatio := 10.0;
    compoundPinnateOrPalmate := kCompoundLeafPinnate;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 115, 0);
    backfaceColor := support_rgb(0, 100, 0);
    meanderIndex := 5;
    flexibilityIndex := 0;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 0.1;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 2;
    widthMultiplierDueToBiomassAccretion := 10;
    widthMultiplierDueToExpansion := 3;
    lengthMultiplierDueToBolting := 5;
    minDaysToBolt := 10;
    maxDaysToBolt := 30;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(150, 150, 0);
    backfaceColor := support_rgb(220, 220, 0);
    scaleAtFullSize := 5;
    numPetals := 3;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 1;
    budNumPetals := 3;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 50);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 30;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    peduncleLength_mm := 25;
    internodeLength_mm := 10;
    internodeWidth_mm := 1;
    pedicelLength_mm := 10;
    pedicelAngle := 32;
    branchAngle := 48;
    terminalStalkLength_mm := 100;
    numFlowers := 10;
    daysToAllFlowersCreated := 20;
    numBranches := 3;
    isHead := false;
    isTerminal := true;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 0;
    branchingDistance := 0;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := false;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    unripeFaceColor := support_rgb(50, 200, 80);
    unripeBackfaceColor := support_rgb(50, 255, 80);
    ripeFaceColor := support_rgb(150, 0, 80);
    ripeBackfaceColor := support_rgb(100, 0, 80);
    scaleAtFullSize := 5;
    stalkStrengthIndex := 30;
 	  daysToRipen := 20;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\r_car.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeLeaf] :=
    Domain.templateManager.findHarvestItemTemplate('Leaf lettuce');
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeWholePlant] :=
    Domain.templateManager.findHarvestItemTemplate('Leaf lettuce plant');
  pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeLeaf] := true;
  end;
  end;

procedure InitAsCultivar_Spinach_drawingPlant(drawingPlant: GsDrawingPlant);
  begin
  if drawingPlant = nil then
    raise Exception.create('InitAsCultivar for drawing plant: nil drawing plant');
  with drawingPlant do
  begin
  { biomass first }
  pLeaf.optimalBiomass_kg := 80.0 * g_to_kg;
  pInternode.optimalFinalBiomass_kg := 10.0 * g_to_kg;
  pFlower[kGenderFemale].optimalBiomass_kg := 1.0 * g_to_kg;
  pInflor[kGenderFemale].optimalBiomass_kg := 200.0 * g_to_kg;
  pFruit.optimalBiomass_kg := 1.0 * g_to_kg;
  with pGeneral do
    begin
    randomSway := 5.0;
    numApicalInflors := 1;
    numAxillaryInflors := 0;
    gender := kGenderHermaphroditic;
    lineDivisions := 1;
    isDicot := true;
    maleFlowersAreSeparate := false;
    end;
  with pSeedlingLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_radish.tdo');
    faceColor := support_rgb(0, 120, 0);
    backfaceColor := support_rgb(0, 220, 0);
    scale := 2;
    nodesOnStemWhenFallsOff := 3;
    end;
  with pLeaf do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\l_radish.tdo');
    faceColor := support_rgb(0, 100, 0);
    backfaceColor := support_rgb(0, 200, 0);
    optimalFractionOfOptimalBiomassAtCreation_kg := 0.1;
    minDaysToGrow := 3;
    maxDaysToGrow := 10;
    scaleAtOptimalBiomass := 30;
    petioleLengthAtOptimalBiomass_mm := 20;
    petioleWidthAtOptimalBiomass_mm := 1;
    petioleAngle := 24;
    petioleColor := faceColor;
    compoundNumLeaflets := 1;
    end;
  with pInternode do
    begin
    faceColor := support_rgb(0, 115, 0);
    backfaceColor := support_rgb(0, 100, 0);
    meanderIndex := 5;
    flexibilityIndex := 0;
    minFractionOfOptimalInitialBiomassToCreateInternode_frn := 0.8;
    minDaysToCreateInternode := 3;
    maxDaysToCreateInternodeIfOverMinFraction := 10;
    canRecoverFromStuntingDuringCreation := false;
    minDaysToAccumulateBiomass := 3;
    maxDaysToAccumulateBiomass := 10;
    minDaysToExpand := 3;
    maxDaysToExpand := 10;
    lengthAtOptimalFinalBiomassAndExpansion_mm := 2;
    lengthMultiplierDueToBiomassAccretion := 3;
    lengthMultiplierDueToExpansion := 10;
    widthAtOptimalFinalBiomassAndExpansion_mm := 2;
    widthMultiplierDueToBiomassAccretion := 10;
    widthMultiplierDueToExpansion := 3;
    lengthMultiplierDueToBolting := 5;
    minDaysToBolt := 10;
    maxDaysToBolt := 30;
    end;
  with pFlower[kGenderFemale] do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    faceColor := support_rgb(150, 150, 0);
    backfaceColor := support_rgb(220, 220, 0);
    scaleAtFullSize := 5;
    numPetals := 3;
    petalsAreRadiallyArranged := true;
    minFractionOfOptimalInitialBiomassToCreateFlower_frn := 0.1;
    minDaysToCreateFlowerBud := 3;
    maxDaysToCreateFlowerBudIfOverMinFraction := 10;
    minFractionOfOptimalBiomassToOpenFlower_frn := 0.4;
    minDaysToGrow := 3;
    maxDaysToGrowIfOverMinFraction := 10;
    daysBeforeDrop := 60;
    budObject3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    budFaceColor := support_rgb(0, 200, 0);
    budBackfaceColor := support_rgb(0, 100, 0);
    budScaleAtFullSize := 1;
    budNumPetals := 3;
    budPetalsAreRadiallyArranged := true;
    end;
  with pFlower[kGenderMale] do { doesn't have male flower }
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\p_1.tdo');
    end;
  with pInflor[kGenderFemale] do
    begin
    stalkColor := support_rgb(0, 128, 50);
    minFractionOfOptimalBiomassToCreateInflorescence_frn := 0.8;
    minDaysToCreateInflorescence := 5;
    maxDaysToCreateInflorescenceIfOverMinFraction := 30;
    minFractionOfOptimalBiomassToMakeFlowers_frn := 0.5;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    peduncleLength_mm := 25;
    internodeLength_mm := 10;
    internodeWidth_mm := 1;
    pedicelLength_mm := 10;
    pedicelAngle := 32;
    branchAngle := 48;
    terminalStalkLength_mm := 100;
    numFlowers := 10;
    daysToAllFlowersCreated := 20;
    numBranches := 3;
    isHead := false;
    isTerminal := true;
    flowersDrawTopToBottom := false;
    flowersSpiralOnStem := true;
    end;
  with pInflor[kGenderMale] do  { doesn't have male inflorescences }
    begin
    end;
  with pAxillaryBud do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_1.tdo');
    scale := 0;
    end;
  with pMeristem do
    begin
    branchingIndex := 0;
    branchingDistance := 0;
    determinateProbability := 1.0;
	  branchingIsSympodial := false;
    branchingIsAlternate := false;
   end;
  with pFruit do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\s_cuc.tdo');
    unripeFaceColor := support_rgb(50, 200, 80);
    unripeBackfaceColor := support_rgb(50, 255, 80);
    ripeFaceColor := support_rgb(50, 200, 80);
    ripeBackfaceColor := support_rgb(50, 255, 80);
    scaleAtFullSize := 5;
    stalkStrengthIndex := 30;
 	  daysToRipen := 20;
    numSections := 5;
    sectionsAreRadiallyArranged := true;
    minDaysToGrow := 10;
    maxDaysToGrow := 30;
    Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, minDaysToGrow, 0.95);
    end;
  with pRoot do
    begin
    object3D := Domain.templateManager.loadObject3D(Domain.exeDirectory + 'tdo\r_car.tdo');
    showsAboveGround := false;
    end;
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeLeaf] :=
    Domain.templateManager.findHarvestItemTemplate('Spinach leaf');
  pGeneral.harvestItemTemplatesArray[kHarvestPartTypeWholePlant] :=
    Domain.templateManager.findHarvestItemTemplate('Spinach plant');
  pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeLeaf] := true;
  end;
  end;

function createHardCodedHarvestItemTemplate(aName: string; templateManager: GsTemplateManager): GsHarvestItemTemplate;
  begin
  result := nil;
  if aName = 'Corn' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\corn.ico')
  else if aName = 'Pea' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\pea.ico')
  else if aName = 'Tomato' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\tomato.ico')
  else if aName = 'Carrot' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\carrot.ico')
  else if aName = 'Sunflower' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\sunflowr.ico')
  else if aName = 'Pepper' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\pepper.ico')
  else if aName = 'Green bean' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\bean.ico')
  else if aName = 'Leaf lettuce' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\leaflett.ico')
  else if aName = 'Leaf lettuce plant' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\headlett.ico')
  else if aName = 'Onion' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\onion.ico')
  else if aName = 'Spinach leaf' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\spinach.ico')
  else if aName = 'Spinach plant' then
    result := templateManager.createNewHarvestItemTemplate(aName, 'c:\gs_proj\gs_bmp\icons\spinach.ico')
  else
    raise Exception.create('Bad name for harvest item template');  
  end;

end.

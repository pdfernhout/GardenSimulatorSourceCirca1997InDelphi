unit ueqh;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ueqh: Bottom-level EPIC functions (EQ = EPIC eQuations) having to do with hydrology. Actually
this file was only removed from the ueq file Delphi 1.0 choked on the large file. See the comment at the
start of the ueq file for more information on the bottom-level EPIC functions.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses ueStruct, SysUtils;

type EQH = class
  public
  {                      HYDROLOGY - SURFACE *RUNOFF* }
  { EQN: 1 }
  class function RunoffVolume_mm(var water: WaterStructure; var surface: SurfaceStructure;
    var surfaceLayer: LayerStructure; var params: SoilParamsStructure; patchMeanCropHeight_m: single): single;
  { EQN: 2 }
  class function RetentionParameter_mm(curveNumber: single): single;
  { EQN: 3 }
  class function CurveNumberAvgForSlope(slopeSteepness_mPm: single; curveNumberAvg: single; curveNumberWet: single): single;
  { EQN: 4 }
  class function CurveNumberDry(curveNumberAvg: single): single;
  { EQN: 5 }
  class function CurveNumberWet(curveNumberAvg: single): single;
  { EQN: 6 }
  class function SoilWaterAdjRetentionParameter_mm(retentionParameter_mm: single; depthWeightedFractionOfFieldCapacity:
   single; shapeParamOne: single; shapeParamTwo: single): single;
  { EQN:7 }
  class function FractionOfFieldCapacity(waterContent_mm: single; wiltingPoint_mm: single; fieldCapacity_mm: single): single;
  { EQN: 8 EQN: 10 }
  class procedure ShapeParamsForRetentionParam(retentionParamAvg_mm: single; retentionParam_mm: single; retentionParamWet_mm:
    single; porosityToFieldCapacityRatio: single; var curveNumberCoeffs: sCurveStructure);
  { EQN:9 }
  class function PorosityToFieldCapacityRatio(numLayers: integer; var layers: LayerStructureArray): single;
  { EQN:11 }
  class function DepthWeightedFractionOfFieldCapacity(numLayers: integer; var layers: LayerStructureArray): single;
  { EQN:12 }
  class function ModifiedRetentionParamForFrozenGround(retentionParam: single): single;
  {                    HYDROLOGY - *PEAK RUNOFF RATE* }
  { EQN: 17 }
  class function PeakRunoffRateByRationalEquation_m3Psec(propRainInTimeOfConc_frn: single; runoffVolume_mm: single;
    timeOfConc_hr: single): single;
  { EQN: 18 }
  class function TimeOfConc_hr(timeOfConcForChannelFlow_hr: single; timeOfConcForSurfaceFlow_hr: single): single;
  { EQN: 24 }
  class function AvgFlowRateForOneHectare_mmPhr(runoffVolume_mm: single; rainfallDuration_hr: single): single;
  { EQN: 25 }
  class function RainfallDuration_hr(rainfallWithoutSnowmeltForDay_mm: single; propRainInFirstHalfHourModBySnow: single):
    single;
  { EQN: 26 }
  class function TimeOfConcForChannelFlow_hr(channelLength_km, manningsChannelRoughnessCoeff,
      watershedArea_ha, channelSlope_mPm, avgFlowRateForOneHectare_mmPhr: single): single;
  { EQN: 30 }
  class function TimeOfConcForSurfaceFlow_hr(slopeLength_m, manningsSurfaceRoughnessCoeff,
    slopeSteepness_mPm, avgFlowRateForOneHectare_mmPhr: single): single;
  { EQN: 31 }
  class function PropRainInTimeOfConc_frn(timeOfConc_hr: single; peakRainfallRate_mmPhr: single; rainfallForDay_mm:
    single; maxFractionOfRainfallInTimeOfConcParam: single): single;
  { EQN: 32 }
  class function PeakRunoffRateBySCSTR55_m3Psec(var water: WaterStructure; peakRateEstimationMethod: integer): single;
  class function PeakRunoffBySCSTR55Function(logTimeOfConc: single; scsMethod: integer; lookupValue: integer): single;
  { EQN: 33 }
  class function TimeOfConcTR55_hr(timeOfConcForChannelFlow_hr: single; timeOfConcForSurfaceFlow_hr: single;
    timeOfConcForShallowChannelFlow_hr: single): single;
  { EQN: 34 }
  class function TimeOfConcForChannelFlowTR55_hr(channelLength_km: single; slopeLength_m: single; shallowFlowLength_m:
    single; manningsChannelRoughnessCoeff: single; channelDepth_m: single; channelSlope_mPm: single): single;
  { EQN: 35 }
  class function TimeOfConcForShallowChannelFlow_hr(shallowFlowLength_km: single; avgShallowFlowVelocity_mPhr: single):
    single;
  { EQN: 36 }
  class function AvgShallowFlowVelocity_mPhr(slopeSteepness_mPm: single): single;
  { EQN: 37 }
  class function ShallowFlowLength_m(channelLength_km: single): single;
  { EQN: 38 }
  class function TimeOfConcForSurfaceFlowTR55_hr(slopeLength_m, manningsSurfaceRoughnessCoeff,
    slopeSteepness_mPm, rainfallForDay_mm: single): single;
  {               HYDROLOGY - *PERCOLATION & LATERAL SUBSURFACE FLOW* }
  { EQN: 41 }
  class function PercolationTravelTimeForLayer_hr(porosity_mm: single; fieldCapacity_mm: single; saturatedConductivity_mmPhr:
    single): single;
  { EQN: 45 }
  class function LateralFlowTravelTimeForLayer_hr(porosity_mm: single; fieldCapacity_mm: single;
    latFlowTravelTimeFactorForLayer: single): single;
  { from code, not in chapter }
  class function LatFlowTravelTimeFactorForLayer(layer: integer; layerWithDrainageSystem: integer; var layers:
    LayerStructureArray; timeForDrainageSystemToReducePlantStress_days: single; slopeSteepness_mPm: single): single;
  { EQN: 46 }
  class function PercolationAndLatFlowForLayer_mm(waterContent_mm: single; fieldCapacity_mm: single;
    percolationTravelTime_hr: single; latFlowTravelTime_hr: single; allowPercolation, allowLateralFlow: boolean): single;
  { EQN: 48 }
  class function PercolationForLayer_mm(percolationAndLatFlowForLayer_mm: single; latFlowTravelTime_hr: single;
    percolationTravelTime_hr: single): single;
  { EQN: 46 }
  class function LateralFlowForLayer_mm(percolationAndLatFlowForLayer_mm: single; percolationForLayer_mm: single): single;
  { EQN: 42 }
  class function UpwardMovementOverFieldCapacityForLayer_mm(waterContent_mm: single; soilWaterContentHigherLayer_mm: single;
    fieldCapacity_mm: single; fieldCapacityHigherLayer_mm: single): single;
  { EQN: 43 }
  class function SaturatedConductivityForLayer_mmPhr(clayContent_pct: single; soilStrengthFactorForSatCond: single): single;
  { EQN: 49 }
  class function LatFlowAdjForReturnFlow_mm(returnFlowTravelTime_days: single; unadjLateralFlow_mm: single): single;
  {                       HYDROLOGY - *EVAPOTRANSPIRATION* }
  { EQN: 50 }
  class function PotentialSoilEvapByPenman_mm(slopeSaturVaporPressureCurve_kPaPdegC: single; psychromConstant_kPaPdegC:
    single; netRadiationForDay_MJPm2: single; latentHeatOfVaporization_MJPkg: single; windSpeedFunction_mmPdaykPa:
      single; vaporPressureDeficit_kPa: single; soilHeatFlux_MJPM2: single): single;
  { EQN: 51 }
  class function LatentHeatOfVaporization_MJPkg(meanTempForDay_degC: single): single;
  { EQN: 52 }
  class function SaturVaporPressure_kPa(meanTempForDay_degC: single): single;
  { EQN: 53 }
  class function VaporPressureAtMeanTemp_kPa(saturVaporPressureAtMeanTemp_kPa: single; relHumForDay_frn: single): single;
  { EQN: 54 }
  class function SlopeSaturVaporPressureCurve_kPaPdegC(saturVaporPressureAtMeanTemp_kPa: single; meanTempForDay_degC:
    single): single;
  { EQN: 55 }
  class function PsychrometerConstant_kPaPdegC(barometricPressure_kPa: single): single;
  { EQN: 56 }
  class function BarometricPressure_kPa(siteElevation_m: single): single;
  { EQN: 57 }
  class function SoilHeatFlux_MJPM2(meanTempForDay_degC: single; prevThreeDaysMeanTemp_degC: arrayThree): single;
  { EQN: 58 }
  class function NetRadiationForDay_MJPm2(radiationForDay_MJPm2: single; albedo_frn: single;
    netOutgoingRadiationIfDayIsClear_MJPm2: single; maxPossibleRadiation_MJPm2: single): single;
  { EQN: 59 }
  class function NetOutgoingLongWaveRadiationClearDays_MJPm2(vaporPressureAtMeanTemp_kPa: single; meanTempForDay_degC:
    single): single;
  { EQN: 60 }
  class function MaxPossibleRadiation_MJPm2(julianDay: integer; maxPossibleRadiationVariable: single; stationLatitude_rad:
    single; declinationAngleOfSun_rad: single): single;
  { EQN: 61 }
  class function MaxPossibleRadiationVariable(stationLatitude_rad: single; declinationAngleOfSun_rad: single): single;
  { EQN: 63 }
  class function PenmanWindSpeedFunction_mmPdaykPa(meanWindSpeedForDay_mPsec: single): single;
  { EQN: 64 }
  class function PotentialSoilEvapByPenmanMonteith_mm(slopeSaturVaporPressureCurve_kPaPdegC: single;
    netRadiationForDay_MJPm2: single; soilHeatFlux_MJPM2: single; airDensity_gPm3: single; vaporPressureDeficit_kPa: single;
      aeroResistForHeatAndVaporTransfer_secPm: single; latentHeatOfVaporization_MJPkg: single; psychromConstant_kPaPdegC:
        single): single;
  { EQN: 65 }
  class function PotentialPlantEvapByPenmanMonteith_mm(slopeSaturVaporPressureCurve_kPaPdegC: single;
    netRadiationForDay_MJPm2: single; soilHeatFlux_MJPM2: single; airDensity_gPm3: single; vaporPressureDeficit_kPa:
      single; aeroResistForHeatAndVaporTransfer_secPm: single; latentHeatOfVaporization_MJPkg: single;
        psychromConstant_kPaPdegC: single; canopyResisForVaporTransfer_secPm: single): single;
  { numerator of preceding two fxns is identical }
  class function PotSoilOrPlantEvapPenmanMonteithNumerator(slopeSaturVaporPressureCurve_kPaPdegC: single;
    netRadiationForDay_MJPm2: single; soilHeatFlux_MJPM2: single; airDensity_gPm3: single; vaporPressureDeficit_kPa:
      single; aeroResistForHeatAndVaporTransfer_secPm: single): single;
  { EQN: 66 }
  class function AirDensity_gPm3(barometricPressure_kPa: single; meanTempForDay_degC: single): single;
  { EQN: 67 }
  class function AeroResistForHeatAndVaporTransferIfCrop_secPm(cropHeight_m: single; meanWindSpeedForDay_mPsec: single;
    surfaceRoughParam_m: single; displacementHeightOfCrop_m: single): single;
  { EQN: 68 }
  class function SurfaceRoughnessParam_m(cropHeight_m: single): single;
  { EQN: 69 }
  class function DisplacementHeightOfCrop_m(cropHeight_m: single): single;
  { EQN: 70 }
  class function AeroResistForHeatAndVaporTransferNoCrop_secPm(meanWindSpeedForDay_mPsec: single): single;
  { EQN: 71 }
  class function CanopyResistForVaporTransfer_secPm(canopyResistParam: single; leafAreaIndex: single; leafConductance_mPsec:
    single; carbonDioxideInAtmosphere_ppm: single): single;
  { EQN: 72 }
  class function LeafConductance_mPsec(leafResistIfVPDBelowThreshold_mPsec: single; vaporPressureDeficitCorrectionFactor:
    single): single;
  { EQN: 73 }
  class function VaporPressureDeficitCorrectionFactor(fractionOfMaxLeafConductForHighVPD_frn: single;
    vaporPressureDeficit_kPa: single; thresholdVaporPressureDeficit_kPa: single): single;
  { EQN: 74 }
  class function PotentialSoilEvapByPriestleyTaylor_mm(netRadiationByPriestleyTaylor_MJPm2: single;
    latentHeatOfVaporization_MJPkg: single; slopeSaturVaporPressureCurve_kPaPdegC: single; psychromConstant_kPaPdegC:
      single): single;
  { EQN: 75 }
  class function NetRadiationByPriestleyTaylor_MJPm2(radiationForDay_MJPm2: single; albedo_frn: single): single;
  { EQN: 76 }
  class function PotentialSoilEvapByHargreaves_mm(maxPossibleRadiation_MJPm2: single; latentHeatOfVaporization_MJPkg:
    single; meanTempForDay_degC: single; maxTempForDay_degC: single; minTempForDay_degC: single): single;
  { EQN: 77 }
  class function Albedo_frn(soilCoverIndex_frn: single; soilAlbedo_frn: single; snowCoverWaterContent_mm: single;
    leafAreaIndex: single): single;
  { EQN: 78 }
  class function SoilCoverIndex_frn(aboveGroundBiomassAndResidue_tPha: single; snowWaterContent_mm: single): single;
  { EQN: 79 EQN: 80 }
  class function PotentialPlantEvapNotPenmanMonteith_mm(potentialSoilEvap_mm: single; leafAreaIndex: single): single;
  { EQN: 81 }
  class function PotentialSoilEvapAdjByCover_mm(potentialSoilEvap_mm: single; soilCoverIndex_frn: single): single;
  { EQN: 82 }
  class function PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm(potentialSoilEvapAdjByCover_mm: single; potentialSoilEvap_mm:
    single; potPlantEvap_mm: single): single;
  { EQN: 83 }
  class function PotentialSoilEvapForDepth(depth_mm: single; potSoilEvap_mm: single; var
    soilEvapByDepthCoeffs: SCurveStructure): single;
  { EQN: 85 EQN: 86 }
  class function PotentialSoilEvapForLayerAdjForDryness_mm(potentialSoilEvapForLayer_mm: single; waterContent_mm: single;
    fieldCapacity_mm: single; wiltingPoint_mm: single): single;
  { EQN: 87 }
  class function SoilEvaporationForLayer_mm(waterContent_mm: single; potentialSoilEvapForLayer_mm: single;
    lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single; wiltingPoint_mm: single): single;
  {                                 HYDROLOGY - *SNOWMELT* }
  { EQN: 88 }
  class function Snowmelt_mm(meanTempForDay_degC: single; snowPackTemp_degC: single; snowWaterContent_mm: single): single;
  { EQN: 89 }
  class function SnowPackTemp_degC(soilSurfaceTemp_degC: single; meanTempForDay_degC: single): single;
  {                      HYDROLOGY - *WATER TABLE* DYNAMICS }
  { EQN: 90 EQN: 91 EQN: 93 EQN: 94 }
  class function WaterTableDepth_m(waterTableDepthYesterday_m: single; maxWaterTableDepth_m: single;
    minWaterTableDepth_m: single; drivingFunction: single): single;
  { EQN: 92 }
  class function WaterTableDepthDrivingFunction(sumRainfallMinusRunoffPrev30Days_mm: single;
    sumPotSoilEvapPrev30Days_mm: single): single;
  end;


implementation

uses ueutils, ueq, uunits, utr55;

{
                      HYDROLOGY - SURFACE *RUNOFF*
The runoff model simulates surface runoff volumes and peak runoff rates,
given daily rainfall amounts.  Runoff volume is estimated by using a
modification of the Soil Conservation Service (SCS) curve number
technique (U.S. Department of Agriculture, Soil COnservation Service 1972).
The technique was selected for use because
(a) it is reliable and has been used for many years in the United States,
(b) it is computationally efficient,
(c) the required inputs are generally available, and
(d) it relates runoff to soil type, land use, and management practices.
The use of readily available daily rainfall data is a particularly important
attribute of the curve number technique because for many locations, rainfall
data with time increments of less than one day are not available.  Also,
rainfall data manipulations and runoff computations are more efficient for data
taken daily than at shorter intervals.

There are two options for estimating the peak runoff rate -- the modified
Rational formula and the SCS TR-55 method (USDA-SCS, 1986).
A stochastic element is included in the Rational equation to allow realistic
simulation of peak runoff rates, given only daily rainfall and monthly
rainfall intensity.

Runoff Volume

Surface runoff is predicted for daily rainfall by using the SCS curve number
equation (U.S. Department of Agriculture, Soil Conservation Service 1972)[EQN:1]
where Q is daily runoff in mm, R is daily rainfall in mm, and s is a
retention parameter. The retention parameter, s, varies (a) among watersheds
because soils, land use, management, and slope all vary, and (b) with time
because of changes in soil water content.
Equation:
  if R > 0.2 * s
    Q = sqr(R - 0.2 * s) / (R + 0.8 * s)
  else
    Q = 0.0
Code:
  same, modifications are made for impact of dikes
Variables:
  Q = RunoffVolume_mm
  R = rainfallForDay_mm
  s = retentionParameter_mm
 }
{ EQN: 1 }
class function EQH.RunoffVolume_mm(var water: WaterStructure; var surface: SurfaceStructure;
    var surfaceLayer: LayerStructure; var params: SoilParamsStructure; patchMeanCropHeight_m: single): single;
  var
    runoffVolume_mm: single;
    runoffVolumeDikesCanHold_mm: single;
    runoffHasWashedOverDikes: boolean;
  begin
  try
  runoffVolume_mm := 0.0;
  runoffVolumeDikesCanHold_mm := 0.0;
  { basic runoff volume }
  if (water.patchRainfallForDay_mm > 0.2 * water.retentionParam_mm) then
    runoffVolume_mm := safediv(sqr(water.patchRainfallForDay_mm - 0.2 * water.retentionParam_mm),
      water.patchRainfallForDay_mm + 0.8 * water.retentionParam_mm)
  else
    runoffVolume_mm := 0.0;
  { if dikes exist, adjust runoff volume for absorption by dikes }
  runoffHasWashedOverDikes := false;
  if (runoffVolume_mm > 0.0) then
    begin
    if (surface.dikeHeight_mm < 0.001) then
      runoffHasWashedOverDikes := true
    else
      begin
      surface.dikeVolume_mm := EQ.DikeVolume_mm(surface, params);
      runoffVolumeDikesCanHold_mm := surface.dikeVolume_mm - max(0.0, surfaceLayer.waterContent_mm -
        surfaceLayer.porosity_mm);
      if (runoffVolume_mm > runoffVolumeDikesCanHold_mm) then
        runoffHasWashedOverDikes := true
      else
        begin
        runoffHasWashedOverDikes := false;
        runoffVolume_mm := 0.0;
        { change from EPIC - in EPIC they appear to compare this ratio to 7.0, not 0.7, but it
          seems wrong, because you will be rebuilding dikes even when they are higher than
          the original height }
        if safedivExcept(surface.dikeHeight_mm, surface.dikeHeightAtCreation_mm, 0.0) < 0.7 then
          surface.dikesNeedRebuilt := true;
        end;
      end;
    end;
  { if dikes have been washed out by the runoff, decide if they will be rebuilt }
  if (runoffHasWashedOverDikes) then 
    begin
    surface.dikeHeight_mm := 0.0;
    if (patchMeanCropHeight_m <= 1.0) then
      surface.dikesNeedRebuilt := true;
    end;
  result := runoffVolume_mm;
  except on e: Exception do begin errorMessage('Exception in EQH.RunoffVolume_mm: ' + e.message); result := 0.0; end; end;
  end;

{ 
The parameter s is related to curve number (CN) by the SCS equation
(U.S. Dept. of Ag., Soil Conservation Service 1972) [EQN:2].  The constant,
254, in equation 2 gives s in mm.  Thus, R and Q are also expressed in mm.
Equation:
  S = 254 * (100 / CN - 1)
Code:
  same
Variables:
  CN = curveNumber
 }
{ EQN: 2 }
class function EQH.RetentionParameter_mm(curveNumber: single): single;
  begin
  try
  result := 254.0 * (safediv(100.0, curveNumber) - 1.0);
  except on e: Exception do result := errorMessage('Exception in EQH.RetentionParameter_mm: ' + e.message); end;
  end;

{ 
CN2- the curve number for moisture condition 2, or average curve number --
can be obtained easily for any area by using the SCS hydrology handbook (U.S.
Dept. Ag., SCS 1972). The handbook tables consider soils, land use, and
management.  Asuuming the handbook CN2 value is appropriate for a 5% slope,
we developed the following equation for adjusting that value for other slopes.
[EQN: 3] where CN2s is the handbook CN2 value adjusted for slope, CN3 is the
curve number for moisture condition 3 (wet), and S is the average slope of the
watershed.  Values of CN1, the curve number for moisture condition 1 (dry), and
CN3 corresponding to CN2 are also tabulated in the handbook.  For computing
purposes, CN1 and CN3 were related to CN2 with the equations [EQN: 4] [EQN: 5]
Equation:
  CN(2s) = 1/3 * (CN(3) - CN(2)) * (1 - 2 * exp(-13.86 * S)) + CN(2)
Code:
  same
Variables:
  CN(2s) = EQ::CurveNumberAvgForSlope
  CN(2) = curveNumberAvg
  CN(3) = curveNumberWet
  S = slopeSteepness_mPm
 }
{ EQN: 3 }
class function EQH.CurveNumberAvgForSlope(slopeSteepness_mPm: single; curveNumberAvg: single; curveNumberWet: single): single;
  begin
  try
  result := 1.0 / 3.0 * (curveNumberWet - curveNumberAvg) * (1.0 - 2.0 * safeExp(-13.86 * slopeSteepness_mPm)) +
    curveNumberAvg;
  except on e: Exception do result := errorMessage('Exception in EQH.CurveNumberAvgForSlope: ' + e.message); end;
  end;

{ 
Equation:
  CN(1) = CN(2) - 20 * (100 - CN(2)) / (100 - CN(2) + exp(2.533 - 0.0636 * (100 - CN(2))))
Code:
  same except for added upper bound at 0.4 * CN(2)
Variables:
  CN(1) = EQ::CurveNumberDry
  CN(2) = curveNumberAvg
 }
{ EQN: 4 }
class function EQH.CurveNumberDry(curveNumberAvg: single): single;
  begin
  try
  result := curveNumberAvg - 20.0 * scurve(100.0 - curveNumberAvg, 2.533, 0.0636);
  result := max(0.4 * curveNumberAvg, result);
  except on e: Exception do begin errorMessage('Exception in EQH.CurveNumberDry: ' + e.message); result := 0.0; end; end;
  end;

{ 
Equation:
  CN(3) = CN(2) * exp(0.00673 * (100 - CN(2)))
Code:
  same
Variables:
  CN(3) = EQ::CurveNumberWet
  CN(2) = curveNumberAvg
 }
{ EQN: 5 }
class function EQH.CurveNumberWet(curveNumberAvg: single): single;
  begin
  try
  result := curveNumberAvg * safeExp(0.00673 * (100.0 - curveNumberAvg));
  except on e: Exception do begin errorMessage('Exception in EQH.CurveNumberWet: ' + e.message); result := 0.0; end; end;
  end;

{ 
Fluctuations in soil water content cause the retention parameter to change
according to the equation [EQN:6] where s1 is the value of s associated with
CN1, FFC is the fraction of field capacity, and w1 and w2 are shape parameters.
Equation:
  s = s1 * (1 - FFC / (FFC + exp(w1 - w2 * FFC)))
Code:
  if FFC > 0
    s = s1 * (1 - FFC * 100 / (FFC * 100 + exp(w1 - w2 * FFC * 100)))
  else
    s = s1 * (1 - FFC)
Variables:
  s1 = retentionParameter_mm
  FFC = fractionOfFieldCapacity
  w1 = shapeParamOne
  w2 = shapeParamTwo
 }
{ EQN: 6 }
class function EQH.SoilWaterAdjRetentionParameter_mm(retentionParameter_mm: single; depthWeightedFractionOfFieldCapacity:
  single; shapeParamOne: single; shapeParamTwo: single): single;
  begin
  try
  if (depthWeightedFractionOfFieldCapacity > 0.0) then 
    result := retentionParameter_mm * (1.0 - scurve(depthWeightedFractionOfFieldCapacity, shapeParamOne, shapeParamTwo))
  else
    result := retentionParameter_mm * (1.0 - depthWeightedFractionOfFieldCapacity);
  except on e: Exception do
    begin errorMessage('Exception in EQH.SoilWaterAdjRetentionParameter_mm: ' + e.message); result := 0.0; end; end;
  end;

{ 
FFC is computed with the equation [EQN:7] where SW is the soil water content
in the root zone, WP is the wilting point water content (1,500 kPa for many
soils) and FC is the field capacity water content (33 kPa for many soils).
Equation:
  FFC = (SW - WP) / (FC - WP)
Code:
  same
Variables:
  FFC = EQ::FractionOfFieldCapacity
  SW = waterContent_mm
  WP = wiltingPoint_mm
  FC = fieldCapacity_mm
 }
{ EQN: 7 }
class function EQH.FractionOfFieldCapacity(waterContent_mm: single; wiltingPoint_mm: single; fieldCapacity_mm: single): single;
  begin
  try
  if waterContent_mm >= fieldCapacity_mm then
    result := 1.0
  else if waterContent_mm < wiltingPoint_mm then
    result := 0.0
  else if fieldCapacity_mm < wiltingPoint_mm then
    result := 0.0
  else
    result := safediv(waterContent_mm - wiltingPoint_mm, fieldCapacity_mm - wiltingPoint_mm);
  except on e: Exception do result := errorMessage('Exception in EQH.FractionOfFieldCapacity: ' + e.message); end;
  end;

{ 
Values for w1 and w2 are obtained from a simultaneous solution of equation 6
according to the assumptions that s=s2 when FFC=0.6 and s=s3 when
(SW-FC)/(PO-FC) = 0.5 [EQN:8] [EQN:9] where s3 is the CN3 retention parameter
and the porosity-field capacity ratio POFC is computed with the equation
[EQN:10] where PO is the porosity of the soil layer l. Equations 8 and 9 assure
that CN1 corresponds with the wilting point and that the curve number cannot
exceed 100.
Equation:
  w1 = ln(60 / (1 - s2/s1) - 60) + 60 * w2
  w2 = (ln(60 / (1 - s2/s1) - 60) - ln(POFC / (1 - s3/s1) - POFC)) / (POFC - 60)
Code:
  w1 = 1 - s2/smx + 60
  w2 = 1 - s3/smx + POFC
  x1 = leftside(w1) or 60
  w1 = rightside(w1) or 1 - s2/smx
  x2 = leftside(w2) or POFC
  w2 = rightside(w2) or 1 - s3/s2
  w1 = ln(x1/w1 - x1) + x1 * w2
     = ln(60 / (1 - s2/smx) - 60) + 60 * w2
     = same except for s2/s1 is replaced by s2/smx
  w2 = (ln(x1/w1 - x1) - ln(x2/w2 - x2)) / (x2 - x1)
     = (ln(60 / (1 - s2/smx) - 60) - ln(POFC / (1 - s3/smx) - POFC)) / (POFC - 60)
     = same except for s2/s1 is replaced by s2/smx and s3/s1 by s3/smx
     (if you assume s1 is the same as smx here, they are the same)
     ( Note: 60 was changed to 0.6 and POFC changed from 0-100 to 0-1 later )
Variables:
  w1 = curveNumberCoeffs[0]
  w2 = curveNumberCoeffs[1]
  s2 = retentionParamAvg_mm
  s3 = retentionParamWet_mm
  smx = retentionParamFinal_mm
  POFC = porosityToFieldCapacityRatio
 }
{ EQN: 8 EQN: 10 }
class procedure EQH.ShapeParamsForRetentionParam(retentionParamAvg_mm: single; retentionParam_mm: single; retentionParamWet_mm:
  single; porosityToFieldCapacityRatio: single; var curveNumberCoeffs: SCurveStructure);
  var
    x1, y1, x2, y2: single;
  begin
  try
  x1 := 0.6;
  y1 := 1.0 - safediv(retentionParamAvg_mm, retentionParam_mm);
  x2 := porosityToFieldCapacityRatio;
  y2 := 1.0 - safediv(retentionParamWet_mm, retentionParam_mm);
  Utils_InitSCurveParam(curveNumberCoeffs, x1, y1, x2, y2);
  except on e: Exception do errorMessage('Exception in EQH.ShapeParamsForRetentionParam: ' + e.message); end;
  end;

{ 
Equation:
  POFC = 100 + 50 * (sum(1-M)(PO - FC) / sum(1-M)(FC - WP))
Code:
  POFC = 100 + 50 * ((sum(1-M)(PO - WP) / sum(1-m)(FC - WP)) - 1.0) + 0.5
  (later amended to POFC = 1.0 + 0.5 * ((sum(1-M)(PO - WP) / sum(1-m)(FC - WP)) - 1.0) + 0.005 to make 0-1 instead of 0-100 )
Variables:
  POFC = EQ::PorosityToFieldCapacityRatio
  PO = porosity_mm
  FC = fieldCapacity_mm
  WP = wiltingPoint_mm
 }
{ EQN: 9 }
class function EQH.PorosityToFieldCapacityRatio(numLayers: integer; var layers: LayerStructureArray): single;
  var
    layer: integer;
    proportion: single;
    depthLastLayer_m: single;
    porosityMWiltingPointInTopM_mm: single;
    fieldCapacityMWiltingPointInTopM_mm: single;
    layerBelowCriterionDepthHasBeenConsidered: boolean;
  begin
  try
  proportion := 1.0;
  depthLastLayer_m := 0.0;
  porosityMWiltingPointInTopM_mm := 0.0;
  fieldCapacityMWiltingPointInTopM_mm := 0.0;
  layerBelowCriterionDepthHasBeenConsidered := false;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do
    begin
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, 1.0,
      layerBelowCriterionDepthHasBeenConsidered);  { do we want to do anything with this? }
    porosityMWiltingPointInTopM_mm := porosityMWiltingPointInTopM_mm + (proportion * (layers[layer].porosity_mm -
      layers[layer].wiltingPoint_mm));
    fieldCapacityMWiltingPointInTopM_mm := fieldCapacityMWiltingPointInTopM_mm + (proportion *
      (layers[layer].fieldCapacity_mm - layers[layer].wiltingPoint_mm));
    if (layers[layer].depth_m > 1.0) then break;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  result := 1.0 + 0.5 * (safediv(porosityMWiltingPointInTopM_mm, fieldCapacityMWiltingPointInTopM_mm) - 1.0) + 0.005;
  except on e: Exception do result := errorMessage('Exception in EQH.PorosityToFieldCapacityRatio: ' + e.message); end;
  end;

{ 
The FFC value obtained in equation 7 represents soil water uniformly
distributed through the top 1.0 m of soil.  Runoff estimates can be improved
if the depth distribution of soil water is known.  For example, water
distributed near the soil surface results in more runoff than the same volume
of water uniformly distributed through the top meter of soil.  Also, a soil
surface associated with a uniform distribution of soil water results in more
runoff than a soil surface that is dry.  Since EPIC estimates the water
content of each soil layer daily, the depth distribution is available.  The
effect of depth distribution on runoff is expressed in the depth weighting
class function [EQN:11] where FFC* is the depth weighted FFC value for use in
equation 6, Z is the depth in m to the bottom of soil layer l, and M is the
number of soil layers.  Equation 11 performs two functions:  (1) it reduces
the influence of lower layers because FFCl is divided by Zl and (2) it gives
proper weight to thick layers relative to thin layers because FFC is multiplied
by the layer thickness.
Equation:
  FFC* = sum(1-M)(FFC * (Z(l) - Z(l-1)) / Z(l)) / sum(1-M)((Z(l) - Z(l-1))/Z(l))
  Z(l) <= 1.0 m
Code:
  same
Variables:
  FFC* = EQ::DepthWeightedFractionOfFieldCapacity
  FFC = fractionOfFieldCapacity_frn
  Z = depth_m
  M = numLayers
 }
{ EQN:11 }
class function EQH.DepthWeightedFractionOfFieldCapacity(numLayers: integer; var layers: LayerStructureArray): single;
  var
    layer: integer;
    upperLayerDepth_m: single;
    depthWeightForLayer: single;
    fractionOfFieldCapacity_frn: single;
    sumDepthWeights: single;
    sumFractionFieldCapacityTDepthWeight: single;
  begin
  try
  upperLayerDepth_m := 0.0;
  depthWeightForLayer := 0.0;
  fractionOfFieldCapacity_frn := 0.0;
  sumDepthWeights := 0.0;
  sumFractionFieldCapacityTDepthWeight := 0.0;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do
    begin
    if (layers[layer].depth_m <= 1.0) then 
      depthWeightForLayer := safediv(layers[layer].depth_m - upperLayerDepth_m, layers[layer].depth_m)
    else
      depthWeightForLayer := 1.0 - upperLayerDepth_m;
    fractionOfFieldCapacity_frn := EQH.FractionOfFieldCapacity(layers[layer].waterContent_mm,
      layers[layer].wiltingPoint_mm, layers[layer].fieldCapacity_mm);
    sumFractionFieldCapacityTDepthWeight := sumFractionFieldCapacityTDepthWeight + (fractionOfFieldCapacity_frn *
      depthWeightForLayer);
    sumDepthWeights := sumDepthWeights + (depthWeightForLayer);
    upperLayerDepth_m := layers[layer].depth_m;
    if (layers[layer].depth_m > 1.0) then break;
    end;
  result := safediv(sumFractionFieldCapacityTDepthWeight, sumDepthWeights);
  except on e: Exception do
    begin errorMessage('Exception in EQH.DepthWeightedFractionOfFieldCapacity: ' + e.message); result := 0.0; end; end;
  end;

{ 
There is also a provision for estimating runoff from frozen soil.  If the
temperature in the second soil layer is less than 0 degrees C, the retention
parameter is reduced using the equation [EQN:12], where sf is the retention
parameter for frozen ground.  Equation 12 increased runoff for frozen soils
but allows significant infiltration when soils are dry.
Equation:
  s(f) = 0.1 s
Code:
Variables:
  s(f) = EQ::ModifiedRetentionParamForFrozenGround
  s = retentionParam
 }
{ EQN:12 }
class function EQH.ModifiedRetentionParamForFrozenGround(retentionParam: single): single;
  begin
  try
  result := 0.1 * retentionParam;
  except on e: Exception do
    begin errorMessage('Exception in EQH.ModifiedRetentionParamForFrozenGround: ' + e.message); result := 0.0; end; end;
  end;

{ 
The final step in estimating the runoff volume is an attempt to account for
uncertainty.  The retention parameter or curve number estimate is based on land
use, management, hydrologic soil group, land slope, and soil water content and
distribution and is adjusted for frozen soil.  However, many complex natural
processes and artificial diversions that affect runoff are not+ accounted for in
the model.  Thus, the final curve number estimate is generated from a triangular
distribution to account for this uncertain variation.  The mean of the triangle
is the best estimate of the curve number based on using equations 2, 3, 6, 7,
11, and 12.  The extremes as +-5 curve numbers from the mean.  The generated
curve number is substituted into equation 2 to estimate runoff with equation 1.

                    HYDROLOGY - *PEAK RUNOFF RATE*

EPIC contains two methods for estimating peak runoff rate -- the modified
Rational formula and the SCS TR-55 method (USDA-SCS, 1986).

Rational Equation Method
The rational equation can be written in the form
  q(rho) = rho * r * A / 360                                     (Equation 13)
where q(rho) is the peak runoff rate in meters cubed per second, rho
is a runoff coefficient expressing the watershed infiltration characteristics,
r is the rainfall intensity in mm per hour for the watershed's time of
concentration, and A is the drainage area in ha.

The runoff coefficient can be calculated for each storm if the amount of
rainfall and runoff are known in this equation
  tho = Q / R                                                   (Equation 14)
Since R is input and Q is computed with [EQN: 1], rho can be calculated directly.

Rainfall intensity r can be expressed with the relationship
  r = R(tc) / t(c)                                              (Equation 15)
where R(tc) is the amount of rainfall in mm during the watershed's time of
concentration, t(c) in h.

The value of R(tc) can be estimated by developing a relationship with total
R. The Weather Service's TP-40 (Hershfield 1961) provides accumulated rainfall
amounts for various durations and frequencies. Generally, R(tc) and R(24)
(24-h duration is appropriate for the daily time step model) are proportional
for various frequencies. Thus,
  R(tc) = alpha * R(24)                                         (Equation 16)
where alpha is a dimensionless parameter that expresses the proportion of
total rainfall the occurs during t(c).

The peak runoff equation [EQN: 17] is obtained by substituting equations
14, 15 and 16 into equation 13.
Equation:
  q(p) = alpha * Q * A / (360 * t(c))
Code:
  q(p) = alpha * Q / t(c)
Variables:
  q(p) = PeakRunoffRateByRationalEquation_m3Psec
  alpha =  propRainInTimeOfConc_frn
  Q = runoffVolume_mm
  A = watershedArea_ha
  t(c) = timeOfConc_hr
 }
{ EQN: 17 }
class function EQH.PeakRunoffRateByRationalEquation_m3Psec(propRainInTimeOfConc_frn: single; runoffVolume_mm: single;
  timeOfConc_hr: single): single;
  begin
  try
  result := propRainInTimeOfConc_frn * safediv(runoffVolume_mm, timeOfConc_hr);
  except on e: Exception do
    begin errorMessage('Exception in EQH.PeakRunoffRateByRationalEquation_m3Psec: ' + e.message); result := 0.0; end; end;
  end;

{ 
The time of concentration can be estimated by adding the surface and channel
flow times [EQN: 18] where t(cc) is the time of concentration for channel
flow and t(cs) is the time of concentration for surface flow in h.
Equation:
  t(c) = t(cc) + t(cs)
Code:
  same
Variables:
  t(c) = TimeOfConc_hr
  t(cc) = timeOfConcForChannelFlow_hr
  t(cs) = timeOfConcForSurfaceFlow_hr
 }
{ EQN: 18 }
class function EQH.TimeOfConc_hr(timeOfConcForChannelFlow_hr: single; timeOfConcForSurfaceFlow_hr: single): single;
  begin
  try
  result := timeOfConcForChannelFlow_hr + timeOfConcForSurfaceFlow_hr;
  except on e: Exception do begin errorMessage('Exception in EQH.TimeOfConc_hr: ' + e.message); result := 0.0; end; end;
  end;

{ 
The t(cc) can be computed using the equation
  t(cc) = L(c)/v(c)                                             (Equation 19)
where L(c) is the average channel flow length for the watershed in km and
v(c) is the average channel velocity in meters per second.

The average channel flow length can be estimated by using the equation
  L(c) = sqrt(L * L(ca))                                        (Equation 20)
where L is the channel length from the most distant point to the watershed
outlet in km and L(ca) is the distance from the outlet along the channel
to the watershed centroid in km.

Average velocity can be estimated by using Manning's equation and assuming
a trapezoidal channel with 2:1 side slopes and a 10:1 bottom width/depth ratio.
Substitution of these estimated and assumed values gives
  t(cc) = (sqrt(L * L(ca)) * n^0.75) / 0.489 * q(c)^0.25 * sigma^0.375
                                                               (Equation 21)
where n is Manning's n, q(c) is the average flow rate in meters cubed per
second, and sigma is the average channel slope in meters/meter.

Assuming that L(ca) = 0.5L and converting units (L from m to km,
t(cc) from sec to hr, and q(c) from meters cubed per second to mm/hr)
gives the equation
  t(cc) = (1.75 * L* * n^0.75) / (q*(c) * A)^0.25 * sigma^0.375
                                                                (Equation 22)
where L* is the channel length in km and q*(c) is the average flow rate
in mm/hr.

The average flow rate is obtained from the estimated average flow rate from
a unit source in the watershed (one hectare area) and the relationship
  q*(c) = q*(o) * A^-0.5                                       (Equation 23)
where q*(o) is the average flow rate from a one hectare area in mm/hr.

The unit source flow rate is estimated with the equation [EQN: 24]
where DUR, the rainfall duration in hours, is calculated using the equation
[EQN: 25] where alpha(0.5) is computed with equation 16 using R(0.5)
instead of R(tc). Equation 25 is derived assuming rainfall intensity is
exponentially distributed. (Details of the procedure for estimating
alpha(0.5) and DUR are given in the water erosion section of this chapter.)
Equation:
  q*(o) = Q / DUR
Code:
  if (Q > DUR) q*(o) = Q / DUR else q*(o) = 1.0
Variables:
  q*(o) = AvgFlowRateForOneHectare_mmPhr
  Q = runoffVolume_mm
  DUR = rainfallDuration_hr
  alpha(0.5) = meanPropTotalRainFallsInFirstHalfHourForMonth_frn
 }
{ EQN: 24 }
class function EQH.AvgFlowRateForOneHectare_mmPhr(runoffVolume_mm: single; rainfallDuration_hr: single): single;
  begin
  try
  if (runoffVolume_mm > rainfallDuration_hr) then 
    result := safediv(runoffVolume_mm, rainfallDuration_hr)
  else
    result := 1.0;
  except on e: Exception do
    begin errorMessage('Exception in EQH.AvgFlowRateForOneHectare_mmPhr: ' + e.message); result := 0.0; end; end;
  end;

{ 
Equation:
  DUR = 4.605 / (-2.0 * ln(1.0 - alpha(0.5)))
Code:
  same except if R <= 0 DUR = 24
  also added upper bound check at 24 if R > 0
Variables:
  DUR = EQ::RainfallDuration_hr
  alpha(0.5) = propRainInFirstHalfHourModBySnow
  R = rainfallWithoutSnowmeltForDay_mm
 }
{ EQN: 25 }
class function EQH.RainfallDuration_hr(rainfallWithoutSnowmeltForDay_mm: single; propRainInFirstHalfHourModBySnow: single):
	 single;
  begin
  try
  if (rainfallWithoutSnowmeltForDay_mm > 0.0) then 
    result := min(24.0, safediv(4.605,  - 2.0 * safeLn(1.0 - propRainInFirstHalfHourModBySnow)))
  else
    result := 24.0;
  except on e: Exception do result := errorMessage('Exception in EQH.RainfallDuration_hr: ' + e.message); end;
  end;

{ 
Substituting equation 24 into equation 22 gives the final equation
for t(cc), [EQN: 26].
Equation:
  t(cc) = 1.75 * L* * pow(n, 0.75) / (pow(q*(o), 0.25) * pow(A, 0.125) * pow(sigma, 0.375))
Code:
  same, broken into two equations, one calculated at input (unchanging), one daily (changing)
  at input:
    tccf = 1.75 * L* * pow(n, 0.75) / (pow(A, 0.125) * pow(sigma, 0.375))
  daily:
    t(cc) = tccf / pow(q*(o), 0.25)
Variables:
  t(cc) = EQ::TimeOfConcForChannelFlow_hr
  tccf = EQ::TimeOfConcForChannelFlowFactor
  L* = channelLength_km
  n = manningsChannelRoughnessCoeff
  q*(o) = avgFlowRateForOneHectare_mmPhr
  A = watershedArea_ha
  sigma = channelSlope_mPm
 }
{ EQN: 26 }
class function EQH.TimeOfConcForChannelFlow_hr(channelLength_km, manningsChannelRoughnessCoeff,
      watershedArea_ha, channelSlope_mPm, avgFlowRateForOneHectare_mmPhr: single): single;
  begin
  try
  result := safedivExcept(1.75 * channelLength_km * power(manningsChannelRoughnessCoeff, 0.75),
    power(watershedArea_ha, 0.125) * power(channelSlope_mPm, 0.375), 0.0);
  result := safedivExcept(result, power(avgFlowRateForOneHectare_mmPhr, 0.25), 0.0);
  except on e: Exception do result := errorMessage('Exception in EQH.TimeOfConcForChannelFlow_hr: ' + e.message); end;
  end;

{
A similar approach is used to estimate t(cs):
  t(cs) = lambda / v(s)                                         (Equation 27)
where lambda is the surface slope length in m and v(s) is the surface flow
velocity in m/sec.

Applying Manning's equation to a strip 1 m wide down the slope length, assuming
flow is concentrated into a small trapezoidal channel with 1:1 side slopes
and 5:1 bottom width-depth ratio gives the velocity equation
  v(s) = (0.8375 * d^2/3 * s^0.5) / n                           (Equation 28)
where v(s) is the flow velocity in meters cubed per sec, d is flow depth
in m, s is the land surface slope in m/m and n is Manning's roughness
Coeff for the surface.

The average flow depth, d, can be calculated from Manning's equation as a
class function of flow rate
  d = ((q(o) * n) / (5.025 * s^0.5))^0.375                      (Equation 29)
where q(o) is the average flow rate in meters cubed per sec.

Substituting equations 28 and 29 into equation 27 gives [EQN: 30]
Equation:
  t(cs) = 0.0216 * pow(lambda * n, 0.75) / (pow(q*(o), 0.25) * pow(s, 0.375))
Code:
  same, broken into two equations, one calculated at input (unchanging), one daily (changing)
  at input:
    tcsf =  0.0216 * pow(lambda * n, 0.75) / pow(s, 0.375)
  daily:
    t(cs) = tcsf / pow(q*(o), 0.25)
Variables:
  t(cs) = EQ::TimeOfConcForSurfaceFlow_hr
  tcsf = EQ::TimeOfConcForSurfaceFlowFactor
  lambda = slopeLength_m
  n = manningsSurfaceRoughnessCoeff
  q*(o) = avgFlowRateForOneHectare_mmPhr
  s = slopeSteepness_mPm
 }
{ EQN: 30 }
class function EQH.TimeOfConcForSurfaceFlow_hr(slopeLength_m, manningsSurfaceRoughnessCoeff,
    slopeSteepness_mPm, avgFlowRateForOneHectare_mmPhr: single): single;
  begin
  try
  result := safedivExcept(0.0216 * power(slopeLength_m * manningsSurfaceRoughnessCoeff, 0.75),
    power(slopeSteepness_mPm, 0.375), 0.0);
  { if avgFlowRateForOneHectare_mmPhr is zero, don't want to do this step }
  if avgFlowRateForOneHectare_mmPhr <> 0.0 then
    result := safedivExcept(result, power(avgFlowRateForOneHectare_mmPhr, 0.25), 0.0);
  except on e: Exception do result := errorMessage('Exception in EQH.TimeOfConcForSurfaceFlow_hr: ' + e.message); end;
  end;

{ 
To properly evaluate alpha, variation in rainfall patterns must be considered.
For some short duration storms, most or all the rain occurs during t(c)
causing alpha to approach its upper limit of 1.0. Other storms of uniform
intensity cause alpha to approach a minimum value. All other patterns
cause higher alpha values than the uniform pattern, because r(tc) is greater
than r(24) for all patterns except the uniform. By substituting the products
of intensity and time into Equation 16, an expression for the minimum value
of alpha, alpha(mn), is obtained [EQN: 31].
Equation:
  alpha = triangular distribution with t(c)/24 < alpha < 1.0
Code:
  the code does not use a triangular distribution. It uses this equation:
  alpha = 1 - exp(-maxFractionOfRainfallInTimeOfConcParam * t(c) * r(p) / R)
Variables:
  alpha = EQ::PropRainInTimeOfConc_frn
  t(c) = timeOfConc_hr
  r(p) = peakRainfallRate_mmPhr
  R = rainfallForDay_mm
 }
{ EQN: 31 }
class function EQH.PropRainInTimeOfConc_frn(timeOfConc_hr: single; peakRainfallRate_mmPhr: single; rainfallForDay_mm: single;
  maxFractionOfRainfallInTimeOfConcParam: single): single;
  begin
  try
  result := 1.0 - safeExp(-maxFractionOfRainfallInTimeOfConcParam * timeOfConc_hr * safediv(peakRainfallRate_mmPhr,
    rainfallForDay_mm));
  except on e: Exception do result := errorMessage('Exception in EQH.PropRainInTimeOfConc_frn: ' + e.message); end;
  end;

{ 
Thus, alpha ranges within the limits of t(c)/24 <= alpha <= 1.0. Although
confined between limits, the value of alpha is assigned with considerable
uncertainty when only daily rainfall and simulated runoff amounts are given.
Thus, alpha is generated from a triangular distribution with the base
ranging from t(c)/24 to 1.0.

The peak of the alpha distribution changes monthly because of seasonal
differences in rainfall intensities. The Weather Service (US Department of
Commerce, 1979) provides information on monthly maximum rainfall intensities
that can be used to estimate the peak alpha for each month.
(Peak alpha is an input for each month. If not input, it is estimated.)

TR-55 Method.
The SCS TR-55 method for estimating peak runoff rate has been described in
detail (USDA-SCS, 1986). Only the essential equations are presented here.
The peak runoff rate is dependent on the rainfall distribution and amount,
the runoff curve number, and the time of concentration as expressed in the
equation [EQN: 32]
Equation:
  q(p) = q*(p) * R
Code:
  same (no calculation of q*(p) given in chapter)
Variables:
  q(p) = EQ::PeakRunoffRateBySCSTR55_m3Psec
  q*(p) = peakRunoffRatePerUnitRainfall_mmPhrmm
  R = rainfallForDay_mm
 }
{ EQN: 32 }
class function EQH.PeakRunoffRateBySCSTR55_m3Psec(var water: WaterStructure; peakRateEstimationMethod: integer): single;
  var
    i, scsMethod: integer;
    curve: array[0..17] of single;
    peakRunoffRate_mmPhr, functionOfPeakRunoffRate, proportion, result1, result2: single;
    peakRunoffRatePerUnitRainfall_mmPhrmm: single;
    intFunctionOfPeakRunoffRate: integer;
  begin
  try
  { must subtract one from peakRateEstimationMethod because first method is rational equation
    and PeakRunoffBySCSTR55Function function only wants to know what SCS method is being used starting at zero }
  scsMethod := intMax(0, intMin(3, peakRateEstimationMethod - 1));
  peakRunoffRate_mmPhr := 0.0;
  functionOfPeakRunoffRate := 0.0;
  proportion := 0.0;
  result1 := 0.0;
  result2 := 0.0;
  peakRunoffRatePerUnitRainfall_mmPhrmm := 0.0;
  intFunctionOfPeakRunoffRate := 0;
  { unresolved: unclear }
  { I don't fully understand this function. }
  { set up curve }
  curve[0] := 0.0;
  for i := 1 to 3 do curve[i] := curve[i - 1] + 0.1;
  for i := 4 to 17 do curve[i] := curve[i - 1] + 0.05;
  { calculate first estimate }
  peakRunoffRate_mmPhr := 0.2 * safediv(water.retentionParam_mm, water.patchRainfallForDay_mm);
  { choose scs values for this method based on initial estimate of peak runoff rate }
  if (peakRunoffRate_mmPhr > 0.35) then 
    functionOfPeakRunoffRate := (peakRunoffRate_mmPhr - 0.35) / 0.05 + 5.0
  else
    functionOfPeakRunoffRate := peakRunoffRate_mmPhr / 0.1 + 1.0;
  { must subtract one in doing single->integer conversion because our array starts at zero }
  { also, because we add one to this truncated index,
    and because tr55 constants array goes from 0 to 16 (not 17), we will never allow the last array
    index of the "curve" array to be used }
  intFunctionOfPeakRunoffRate := intMax(0, intMin(16, trunc(functionOfPeakRunoffRate) - 1));
  proportion := safediv(peakRunoffRate_mmPhr - curve[intFunctionOfPeakRunoffRate],
    curve[intFunctionOfPeakRunoffRate + 1] - curve[intFunctionOfPeakRunoffRate]);
  result1 := EQH.PeakRunoffBySCSTR55Function(safeLn(water.timeOfConc_hr), scsMethod, intFunctionOfPeakRunoffRate);
  { calculate peak runoff rate per unit rainfall from scs equation - subtract one for starting at zero }
  if (intFunctionOfPeakRunoffRate >= 16) then
    peakRunoffRatePerUnitRainfall_mmPhrmm := safeExp(result1) * (1.0 - proportion)
  else
    begin
    result2 := EQH.PeakRunoffBySCSTR55Function(safeLn(water.timeOfConc_hr), scsMethod, intFunctionOfPeakRunoffRate + 1);
    peakRunoffRatePerUnitRainfall_mmPhrmm := safeExp(result1 + (result2 - result1) * proportion);
    end;
  { calculate final peak runoff rate }
  peakRunoffRate_mmPhr := peakRunoffRatePerUnitRainfall_mmPhrmm * water.patchRainfallForDay_mm;
  result := peakRunoffRate_mmPhr;
  except on e: Exception do result := errorMessage('Exception in EQH.PeakRunoffRateBySCSTR55_m3Psec: ' + e.message); end;
  end;

{ 
A set of curves is available (USDA-SCA, 1986) for estimating q*(p) given
rainfall distribution (SCS Type I, IA, II, or III), the runoff curve number,
and the watershed time of concentration. A map of the U.S. is given
(USDA-SCS, 1986) to determine appropriate storm type. The curves relating
q*(p), t(c), and CN have been fitted with a seventh degree polynomial
for use in computer solutions. (These numbers are in the TR55EPIC....DAT file.)
 }
class function EQH.PeakRunoffBySCSTR55Function(logTimeOfConc: single; scsMethod: integer; lookupValue: integer): single;
  begin
  try
  if (scsMethod < 0) or (scsMethod > 3) then
    raise Exception.create('Problem in EQH.PeakRunoffBySCSTR55Function: SCS method index out of bounds ');
  if (lookupValue < 0) or (lookupValue > 16) then
    raise Exception.create('Problem in EQH.PeakRunoffBySCSTR55Function: lookupValue index out of bounds ');
  result := tr55Constants[scsMethod][lookupValue][0]
      + logTimeOfConc * (tr55Constants[scsMethod][lookupValue][1]
      + logTimeOfConc * (tr55Constants[scsMethod][lookupValue][2]
      + logTimeOfConc * (tr55Constants[scsMethod][lookupValue][3]
      + logTimeOfConc * (tr55Constants[scsMethod][lookupValue][4]
      + logTimeOfConc * (tr55Constants[scsMethod][lookupValue][5]
      + logTimeOfConc * (tr55Constants[scsMethod][lookupValue][6]
      + logTimeOfConc * tr55Constants[scsMethod][lookupValue][7]))))));
  except on e: Exception do result := errorMessage('Exception in EQH.PeakRunoffBySCSTR55Function: ' + e.message); end;
  end;

{ 
Time of concentration is estimated with the equation [EQN: 33] where
t(c) is the watershed time of concentration in hr and t(cc), t(cs) and
t(sf) are the t(c) components attributed to channel flow, surface flow, and
shallow channel flow.
Equation:
  t(c) = t(cc) + t(cs) + t(sf)
Code:
  same
Variables:
  t(c) = EQ::TimeOfConcTR55_hr
  t(cc) = timeOfConcForChannelFlow_hr
  t(cs) = timeOfConcForSurfaceFlow_hr
  t(sf) = timeOfConcForShallowChannelFlow_hr
 }
{ EQN: 33 }
class function EQH.TimeOfConcTR55_hr(timeOfConcForChannelFlow_hr: single; timeOfConcForSurfaceFlow_hr: single;
  timeOfConcForShallowChannelFlow_hr: single): single;
  begin
  try
  result := timeOfConcForChannelFlow_hr + timeOfConcForSurfaceFlow_hr + timeOfConcForShallowChannelFlow_hr;
  except on e: Exception do result := errorMessage('Exception in EQH.TimeOfConcTR55_hr: ' + e.message); end;
  end;

{ 
The channel component is computed with the equation [EQN: 34] where t(cc) is
the channel time of concentration in hr, L is the channel lengh from the
most distant point to the watershed outlet in km, lambda is the surface slope
length in km, L(sf) is the shallow flow length in km, n is Manning's roughness
Coeff, d is the average channel flow depth in m, and sigma is the
channel slope in m/m.
Equation:
  t(cc) = (L - lambda - L(sf)) * n / (3.6 * pow(d, 0.67) * pow(sigma, 0.5))
Code:
  same except first group (L - lambda - L(sf)) cannot be below zero
Variables:
  t(cc) = TimeOfConcForChannelFlow_hr
  L = channelLength_km
  lambda = slopeLength_km
  L(sf) = shallowFlowLength_km
  n = manningsChannelRoughnessCoeff
  d = channelDepth_m
  sigma = channelSlope_mPm
 }
{ EQN: 34 }
class function EQH.TimeOfConcForChannelFlowTR55_hr(channelLength_km: single; slopeLength_m: single; shallowFlowLength_m:
  single; manningsChannelRoughnessCoeff: single; channelDepth_m: single; channelSlope_mPm: single): single;
  var
    slopeLength_km: single;
    shallowFlowLength_km: single;
    difference: single;
  begin
  try
  slopeLength_km := slopeLength_m * m_to_km;
  shallowFlowLength_km := shallowFlowLength_m * m_to_km;
  difference := max(channelLength_km - slopeLength_km - shallowFlowLength_km, 0.0);
  result := difference * safediv(manningsChannelRoughnessCoeff, 3.6 * power(channelDepth_m, 0.67) *
    power(channelSlope_mPm, 0.5));
  except on e: Exception do result := errorMessage('Exception in EQH.TimeOfConcForChannelFlowTR55_hr: ' + e.message); end;
  end;

{ 
The shallow flow component of t(c) is estimated with the equation [EQN: 35]
where V(sf), the average shallow flow velocity in km/h, is estimated with
[EQN: 36] where S is surface flow in m/m.
Equation:
  t(sf) = L(sf) / V(sf)
Code:
  same
Variables:
  t(sf) =  TimeOfConcForShallowChannelFlow_hr
  L(sf) =  shallowFlowLength_km
  V(sf) = avgShallowFlowVelocity_mPhr
  S = slopeSteepness_mPm
 }
{ EQN: 35 }
class function EQH.TimeOfConcForShallowChannelFlow_hr(shallowFlowLength_km: single; avgShallowFlowVelocity_mPhr: single):
  single;
  begin
  try
  result := safediv(shallowFlowLength_km, avgShallowFlowVelocity_mPhr);
  except on e: Exception do result := errorMessage('Exception in EQH.TimeOfConcForShallowChannelFlow_hr: ' + e.message); end;
  end;

{ 
Equation:
  v(sf) = 17.7 * sqrt(S), <= 2.19 km/h
Code:
  same except this is in m/hr
Variables:
  v(sf) = EQ::AvgShallowFlowVelocity_mPhr
  S = slopeSteepness_mPm
 }
{ EQN: 36 }
class function EQH.AvgShallowFlowVelocity_mPhr(slopeSteepness_mPm: single): single;
  begin
  try
  result := min(2.16 * km_to_m, 17.712 * km_to_m * sqrt(slopeSteepness_mPm));
  except on e: Exception do result := errorMessage('Exception in EQH.AvgShallowFlowVelocity_mPhr: ' + e.message); end;
  end;

{ 
The length of shallow flow, L(sf), is estimated with [EQN: 37]. Note that
t(sf) is constrained to less than 0.1 hr even with S = 0.001. Thus, t(sf)
is generally insignificant.
Equation:
  if L > 0.1 km L(sf) = 0.05
  if 0.05 < L < 0.1 km L(sf) = L - 0.05
  if L < 0.05 km L(sf) = 0.0
Code:
  same except in middle case L(sf) is multiplied by 0.1, and aResult is in m, not km
Variables:
  L(sf) = EQ::ShallowFlowLength_m
  L = channelLength_km
 }
{ EQN: 37 }
class function EQH.ShallowFlowLength_m(channelLength_km: single): single;
  begin
  try
  if (channelLength_km > 0.1) then 
    result := 50.0
  else if (channelLength_km > 0.05) then
    result := 1000.0 * 0.1 * (channelLength_km - 0.05)
  else
    result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EQH.ShallowFlowLength_m: ' + e.message); end;
  end;

{ 
The surface flow component of t(c) is estimated with the equation [EQN: 38].
Equation:
  t(cs) = 0.0913 * pow(lambda * n, 0.8) / (pow(S, 0.4) * pow(R, 0.5))
Code:
  same, broken into two equations, one calculated at input (unchanging), one daily (changing)
  at input:
    tcsf = 0.0913 * pow(lambda * n, 0.8) / pow(S, 0.4)
  daily:
    t(cs) = tcsf / pow(R, 0.5)
Variables:
  t(cs) = EQ::TimeOfConcForSurfaceFlowTR55_hr
  tcsf = EQ::TimeOfConcForSurfaceFlowTR55Factor
  lambda = slopeLength_m
  n = manningsSurfaceRoughnessCoeff
  S = slopeSteepness_mPm
  R = rainfallForDay_mm
 }
{ EQN: 38 }
class function EQH.TimeOfConcForSurfaceFlowTR55_hr(slopeLength_m, manningsSurfaceRoughnessCoeff,
    slopeSteepness_mPm, rainfallForDay_mm: single): single;
  begin
  try
  result := safedivExcept(0.0913 * power((slopeLength_m * manningsSurfaceRoughnessCoeff), 0.8),
    power(slopeSteepness_mPm, 0.4), 0.0);
  result := safedivExcept(result, power(rainfallForDay_mm, 0.5), 0.0);
  except on e: Exception do result := errorMessage('Exception in EQH.TimeOfConcForSurfaceFlowTR55_hr: ' + e.message); end;
  end;

{               HYDROLOGY - *PERCOLATION & LATERAL SUBSURFACE FLOW*

The EPIC percolation component uses a storage routing technique to simulate
flow through soil layers. Flow from a soil layer occurs when soil water
content exceeds field capacity. Water drains from the layer until the storage
returns to field capacity. The reduction in soil water is simulated with
the following equation
  SW(l) = (SW(0l) - FC(l)) * exp(-24 / TT(l)) + FC(l)       (Equation 39)
where SW and SW(0) are the soil water contents at the end and the start of
time interval delta-t (24 hr) and TT is travel time through layer l in hr.

Thus, daily percolation can be computed by taking the difference between
SW and SW(0)
  O(l) = (SW(0l) - FC(l)) * (1.0 - exp(-24 / TT(l)))        (Equation 40)
where O is the percolation rate for layer l in mm/day.

Percolation travel time through a layer is computed with the linear storage
equation [EQN: 41] where PO is the porosity in mm, FC is field capacity in mm,
and SC is saturated conductivity in mm/hr.
Equation:
  TT = (PO - FC) / SC
Code:
 same
Variables:
  TT =  PercolationTravelTimeForLayer_hr
  PO = porosity_mm
  FC = fieldCapacity_mm
  SC = saturatedConductivity_mmPhr
 }
{ EQN: 41 }
class function EQH.PercolationTravelTimeForLayer_hr(porosity_mm: single; fieldCapacity_mm: single; saturatedConductivity_mmPhr:
  single): single;
  begin
  try
  result := safediv(porosity_mm - fieldCapacity_mm, saturatedConductivity_mmPhr);
  except on e: Exception do result := errorMessage('Exception in EQH.PercolationTravelTimeForLayer_hr: ' + e.message); end;
  end;

{ 
Equations 42 and 43 are discussed later (after equation 48).

Lateral Subsurface Flow is calculated simultaneously with percolation. The
lateral flow function (similar to equation 40) is expressed in the equation
  QH(l) = (SW(0l) - FC(l)) * (1.0 - exp(-24 / TT(Hl)))     (Equation 44)
where QH is the lateral flow rate for soil layer l in mm/day and TT(Hl) is
the lateral flow travel time in days.

The lateral flow travel time is estimated for each soil layer using the
equation [EQN: 45] where S is the land surface slope in m/m.
Equation:
  TT(H) = TT / S
Code:
  TT(H) = (po-fc) / (SC * S)
    unless the layer has a drainage system, when TT(H) is
    (po-fc) / ((po-wp) / (24 * timeForDrainageSystemToReducePlantStress_days))
  TT = (po-fc) / SC
  TT(H) = TT / S
Variables:
  TT(H) = lateralFlowTravelTime_hr
  TT = percolationTravelTime_hr
  S = slopeSteepness_mPm
 }
{ EQN: 45 }
class function EQH.LateralFlowTravelTimeForLayer_hr(porosity_mm: single; fieldCapacity_mm: single;
  latFlowTravelTimeFactorForLayer: single): single;
  begin
  try
  result := safediv(porosity_mm - fieldCapacity_mm, latFlowTravelTimeFactorForLayer);
  except on e: Exception do result := errorMessage('Exception in EQH.LateralFlowTravelTimeForLayer_hr: ' + e.message); end;
  end;

class function EQH.LatFlowTravelTimeFactorForLayer(layer: integer; layerWithDrainageSystem: integer; var layers:
  LayerStructureArray; timeForDrainageSystemToReducePlantStress_days: single; slopeSteepness_mPm: single): single;
  begin
  try
  if (layer = layerWithDrainageSystem) then 
    result := safediv(layers[layer].porosity_mm - layers[layer].wiltingPoint_mm, 24.0 *
      timeForDrainageSystemToReducePlantStress_days)
  else
    result := layers[layer].saturatedConductivity_mmPhr * slopeSteepness_mPm;
  except on e: Exception do result := errorMessage('Exception in EQH.LatFlowTravelTimeFactorForLayer: ' + e.message); end;
 end;

{ 
Equations 40 and 44 must be solved simultaneously to avoid one process
dominating the other, simply because the solution occurs first. Thus,
an equation for the sum of percolation and lateral flow is written as [EQN: 46].
Equation:
  O + QH = (SW - FC) * (1.0 - exp(-24 / TT) * exp(-24 / TT(H)))
Code:
  if 24 / TT + 24 / TT(H) > 20.0
    O + QH = SW - FC
  else
    O + QH = (SW - FC) * (1.0 - exp(-(24 / TT + 24 / TT(H))))
Variables:
  O + QH = EQ::PercolationAndLatFlowForLayer_mm
  SW(0) = waterContent_mm
  FC = fieldCapacity_mm
  TT = percolationTravelTime_hr
  TT(H) = lateralFlowTravelTime_hr
 }
{ EQN: 46 }
class function EQH.PercolationAndLatFlowForLayer_mm(waterContent_mm: single; fieldCapacity_mm: single;
  percolationTravelTime_hr: single; latFlowTravelTime_hr: single; allowPercolation, allowLateralFlow: boolean): single;
  var
    combinedFactor: single;
  begin
  try
  { assume this function is not called if both percolation and lateral flow are not allowed }
  combinedFactor := 0.0;
  if allowPercolation then combinedFactor := combinedFactor + safedivExcept(24.0, percolationTravelTime_hr, 0.0);
  if allowLateralFlow then combinedFactor := combinedFactor + safedivExcept(24.0, latFlowTravelTime_hr, 0.0);
  if (combinedFactor > 20.0) then
    result := waterContent_mm - fieldCapacity_mm
  else
    result := (waterContent_mm - fieldCapacity_mm) * (1.0 - safeExp(-combinedFactor));
  except on e: Exception do result := errorMessage('Exception in EQH.PercolationAndLatFlowForLayer_mm: ' + e.message); end;
  end;

{ 
Taking the ratio of QH/O and substituting the resulting QH into equation 46
leads to the equation
  O + O * (1.0 - exp(-24/TT(Hl))/(1.0 - exp(-deltat-t/TT(l)))) =
  (SW(0l) - FC(l)) * (1.0 - exp(-24/TT(l) * exp(-24/TT(Hl))))    (Equation 47)

Solving for O gives the final percolation equation [EQN: 48].
Equation:
  O = ((SW - FC) * (1.0 - exp(-24 / TT) * exp(-24 / TT(H))))
      / (1.0 + (1.0 - exp(-24 / TT(H))) / (1.0 - exp(-24 / TT)))
Code:
  O + QH = (SW - FC) * (1.0 - exp(-(24 / TT + 24 / TT(H))))
  O = (O + QH) / (1.0 + (24 / TT(H)) / (24 / TT))
Variables:
  O = EQ::PercolationForLayer_mm
  SW(0) = waterContent_mm
  FC = fieldCapacity_mm
  TT = percolationTravelTime_hr
  TT(H) = lateralFlowTravelTime_hr
 }
{ EQN: 48 }
class function EQH.PercolationForLayer_mm(percolationAndLatFlowForLayer_mm: single; latFlowTravelTime_hr: single;
  percolationTravelTime_hr: single): single;
  var
    ratio: single;
  begin
  try
  ratio := safedivExcept(safedivExcept(24, latFlowTravelTime_hr, 0.0), safedivExcept(24, percolationTravelTime_hr, 0.0), 0);
  result := safediv(percolationAndLatFlowForLayer_mm, 1.0 + ratio);
  except on e: Exception do result := errorMessage('Exception in EQH.PercolationForLayer_mm: ' + e.message); end;
  end;

{ 
The calculated O value is substituted into [EQN: 46] to obtain the final
estimate of QH.
Equation:
  O + QH = (SW - FC) * (1.0 - exp(-(24 / TT + 24 / TT(H)))
  QH = (SW - FC) * (1.0 - exp(-(24 / TT + 24 / TT(H))) - O
  QH = (O + QH) - O
Code:
  same
Variables:
  QH = EQ::LateralFlowForLayer_mm
  O + QH = percolationAndLatFlowForLayer_mm
  O = percolationForLayer_mm
 }
{ EQN: 46 }
class function EQH.LateralFlowForLayer_mm(percolationAndLatFlowForLayer_mm: single; percolationForLayer_mm: single): single;
  begin
  try
  result := percolationAndLatFlowForLayer_mm - percolationForLayer_mm;
  except on e: Exception do result := errorMessage('Exception in EQH.LateralFlowForLayer_mm: ' + e.message); end;
  end;

{ 
Going back to equations 42 and 43...
There is also a provision for upward movement when a lower layer exceeds field
capacity. Movement from a lower layer to an adjoining layer is regulated
by soil water to field capacity ratios using the equation [EQN: 42].
Equation:
  UF = (SW - FC) * (1.0 - exp(0.05 * (SW(l-1) / FC(l-1) - SW / FC)))
Code:
  same
Variables:
  UF = EQ::UpwardMovementOverFieldCapacityForLayer_mm
  SW = waterContent_mm
  SW(l-1) = soilWaterContentHigherLayer_mm
  FC = fieldCapacity_mm
  FC(l-1) = fieldCapacityHigherLayer_mm
 }
{ EQN: 42 }
class function EQH.UpwardMovementOverFieldCapacityForLayer_mm(waterContent_mm: single; soilWaterContentHigherLayer_mm: single;
  fieldCapacity_mm: single; fieldCapacityHigherLayer_mm: single): single;
  begin
  try
  result := (waterContent_mm - fieldCapacity_mm) * (1.0 - safeExp(0.05 * (safediv(soilWaterContentHigherLayer_mm,
    fieldCapacityHigherLayer_mm) - safediv(waterContent_mm, fieldCapacity_mm))));
  if result < 0.0 then result := 0.0;
  except on e: Exception do
  	result := errorMessage('Exception in EQH.UpwardMovementOverFieldCapacityForLayer_mm: ' + e.message); end;
  end;

{ 
Saturated conductivity may be input or estimated for each soil layer by
using the equation [EQN: 43] where CLA is the percentage of clay in soil layer
l and SS is the soil strength factor (described in the Growth Constraints
section of this chapter).
Equation:
  SC = 12.7 * (100 - CLA) * SS / ((100 - CLA) + (11.45 - 0.097 * (100 - CLA)))
Code:
  SC = 1.0 + 12.7 * (100 - CLA) * SS / ((100 - CLA) + exp(11.45 - 0.097 * (100 - CLA)))
Variables:
  SC = EQ::SaturatedConductivityForLayer_mmPhr
  CLA = clayContent_pct
  SS = soilStrengthFactorForSatCond
 }
{ EQN: 43 }
class function EQH.SaturatedConductivityForLayer_mmPhr(clayContent_pct: single; soilStrengthFactorForSatCond: single): single;
  var
    pctNotClay: single;
  begin
  try
  pctNotClay := 100 - clayContent_pct;
  result := 1.0 + 12.7 * pctNotClay * safediv(soilStrengthFactorForSatCond, pctNotClay + safeExp(11.45 - 0.097 *
    pctNotClay));
  except on e: Exception do result := errorMessage('Exception in EQH.SaturatedConductivityForLayer_mmPhr: ' + e.message); end;
  end;

{ 
Now skipping to equation 49...
Equations 46 and 48 are used to partition flow between vertical and
lateral components. The lateral flow component is estimated by considering
subsurface flow from a hillside. This is generally adequate for small
areas composed of a hillside landscape. However, for larger more complex areas,
there is need to lag lateral flow to simulate return flow. EPIC uses an
additional routing equation to accomplish the proper lag: [EQN: 49] where
QE is the return flow volume in mm and TT(R) is the return flow travel time
in days (input to EPIC).
Equation:
  QR = (1.0 - exp(-1.0 / TT(R))) * (sum from 1 to M of) QH
Code:
  QR = (1.0 - exp(-1.0 / TT(R))) * QH
  same except QH is for layer
Variables:
  QR = EQ::LatFlowAdjForReturnFlow_mm
  TT(R) = returnFlowTravelTime_days
  QH = unadjLateralFlow_mm
 }
{ EQN: 49 }
class function EQH.LatFlowAdjForReturnFlow_mm(returnFlowTravelTime_days: single; unadjLateralFlow_mm: single): single;
  begin
  try
  result := (1.0 - safeExp(safediv(-1.0, returnFlowTravelTime_days))) * unadjLateralFlow_mm;
  except on e: Exception do result := errorMessage('Exception in EQH.LatFlowAdjForReturnFlow_mm: ' + e.message); end;
  end;


{                       HYDROLOGY - *EVAPOTRANSPIRATION*

The model offers four options for estimating potential evaporation:
Hargreaves and Samani (1985), Penman (1948), Priestly-Taylor (1972), and
Penman-Monteith (Monteith, 1965). The Penman and Penman-Monteith methods
require solar radiation, air temperatue, wind speed, and relative humidity
as input. If wind speed, relative humidity, and solar radiation data are
not available, the Hargreaves or Priestley-Taylor methods provide options
that give realistic results in most cases.

The model computes evaporation from soils and plants separately, as described
by Ritchie (1972). Potential soil water evaporation is estimated as a
class function of potential evaporation and leaf area index (LAI, area of plant
leaves relative to the soil surface area). Actual soil water evaporation
is simulated as a linear function of potential evaporation and leaf area
index.

Potential evaporation - Penman method

The Penman (1948) option for estimating potential evaporation is based on
the equation [EQN: 50] where E(0) is the potential evaporation in mm,
delta is the slope of the saturation vapor pressure curvin kPa/degreesC, gamma
is a psychrometer constant in kPa/degressC, h(0) is the net radiation in
MJ/m2, G is the soil heat flux in MJ/m2, HV is the latent heat of
vaporization in MJ/kg, f(V) is a wind speed function in mm/day*kPa,
e(a) is the saturation vapor pressure at mean air temperature in kPa,
and e(d) is the vapor pressure at mean air temperature in kPa.
Equation:
  E(o) = (delta / (delta + gamma)) * (h(o) - G) / HV
         + (gamma / (delta + gamma)) * f(V) * (e(a) - e(d))
Code:
  same
Variables:
  E(o) = EQ::PotentialSoilEvapByPenman_mm
  delta = slopeSaturVaporPressureCurve_kPaPdegC
  gamma = psychromConstant_kPaPdegC
  h(0) = netRadiationForDay_MJPm2
  G = soilHeatFlux_MJPM2
  HV = latentHeatOfVaporization_MJPkg
  f(V) = windSpeedFunction_mmPdaykPa
  e(a) - e(d) = vaporPressureDeficit_kPa
 }
{ EQN: 50 }
class function EQH.PotentialSoilEvapByPenman_mm(slopeSaturVaporPressureCurve_kPaPdegC: single; psychromConstant_kPaPdegC:
  single; netRadiationForDay_MJPm2: single; latentHeatOfVaporization_MJPkg: single; windSpeedFunction_mmPdaykPa: single;
  vaporPressureDeficit_kPa: single; soilHeatFlux_MJPM2: single): single;
  var
    firstNumerator: single;
    secondNumerator: single;
    commonDenominator: single;
  begin
  try
  firstNumerator := slopeSaturVaporPressureCurve_kPaPdegC * safediv(netRadiationForDay_MJPm2 - soilHeatFlux_MJPM2,
    latentHeatOfVaporization_MJPkg);
  secondNumerator := psychromConstant_kPaPdegC * windSpeedFunction_mmPdaykPa * vaporPressureDeficit_kPa;
  commonDenominator := slopeSaturVaporPressureCurve_kPaPdegC + psychromConstant_kPaPdegC;
  result := safediv(firstNumerator + secondNumerator, commonDenominator);
  except on e: Exception do result := errorMessage('Exception in EQH.PotentialSoilEvapByPenman_mm: ' + e.message); end;
  end;

{ 
The latent heat of vaporization is estimated with the temperature function
[EQN: 51] where T is the mean daily temperature in degrees C.
Equation:
  HV = 2.5 - 0.0022 * T
Code:
  same
Variables:
  HV =  EQ::LatentHeatOfVaporization_MJPkg
  T = meanTempForDay_degC
 }
{ EQN: 51 }
class function EQH.LatentHeatOfVaporization_MJPkg(meanTempForDay_degC: single): single;
  begin
  try
  { notation: 2.2e-3 }
  result := 2.501 - 0.0022 * meanTempForDay_degC;
  except on e: Exception do result := errorMessage('Exception in EQH.LatentHeatOfVaporization_MJPkg: ' + e.message); end;
  end;

{ 
The saturation vapor pressure is also estimated as a function of temperature
by using the equation [EQN: 52].
Equation:
  e(a) = 0.1 * exp(54.88 - 5.03 * ln(T + 273) - 6791 / (T + 273))
Code:
  same
Variables:
  e(a) = saturVaporPressure_kPa
  T = meanTempForDay_degC
 }
{ EQN: 52 }
class function EQH.SaturVaporPressure_kPa(meanTempForDay_degC: single): single;
  var
    meanTempForDay_degK: single;
  begin
  try
  meanTempForDay_degK := meanTempForDay_degC + 273.0;
  result := 0.1 * safeExp(54.879 - 5.029 * safeLn(meanTempForDay_degK) - safediv(6790.5, meanTempForDay_degK));
  except on e: Exception do result := errorMessage('Exception in EQH.SaturVaporPressure_kPa: ' + e.message); end;
  end;

{ 
The vapor pressure is simulated as a function of the saturation value and
the relative humidity [EQN: 53] where RH is the relative humidity expressed
as a fraction.
Equation:
  e(d) = e(a) * RH
Code:
  same
Variables:
  e(d) = vaporPressureAtMeanTemp_kPa
  e(a) = saturVaporPressureAtMeanTemp_kPa
  RH = relHumForDay_frn
 }
{ EQN: 53 }
class function EQH.VaporPressureAtMeanTemp_kPa(saturVaporPressureAtMeanTemp_kPa: single; relHumForDay_frn: single): single;
  begin
  try
  result := saturVaporPressureAtMeanTemp_kPa * relHumForDay_frn;
  except on e: Exception do result := errorMessage('Exception in EQH.VaporPressureAtMeanTemp_kPa: ' + e.message); end;
  end;

{ 
The slope of the saturation vapor pressure curve is estimated with the equation
[EQN: 54].
Equation:
  delta = (e(a) / (T + 273)) * (6791 / (T + 273) -5.03)
Code:
  same
Variables:
  delta = EQ::SlopeSaturVaporPressureCurve_kPaPdegC
  e(a) = saturVaporPressureAtMeanTemp_kPa
  T = meanTempForDay_degC
 }
{ EQN: 54 }
class function EQH.SlopeSaturVaporPressureCurve_kPaPdegC(saturVaporPressureAtMeanTemp_kPa: single; meanTempForDay_degC:
  single): single;
  begin
  try
  result := safediv(saturVaporPressureAtMeanTemp_kPa, meanTempForDay_degC + 273) * (safediv(6790.5,
    meanTempForDay_degC + 273) - 5.029);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.SlopeSaturVaporPressureCurve_kPaPdegC: ' + e.message); end;
  end;

{ 
The psychrometer constant is computed with the equation [EQN: 55] where PB
is the barometric pressure (kPa).
Equation:
  gamma = 6.6e10-4 * PB
Code:
  same
Variables:
  gamma = psychromConstant_kPaPdegC
  PB = barometricPressure_kPa
 }
{ EQN: 55 }
class function EQH.PsychrometerConstant_kPaPdegC(barometricPressure_kPa: single): single;
  begin
  try
  { notation: 6.595e-4 }
  result := 0.0006595 * barometricPressure_kPa;
  except on e: Exception do result := errorMessage('Exception in EQH.PsychrometerConstant_kPaPdegC: ' + e.message); end;
  end;

{ 
The barometric pressure is estimated as a function of elevation by using the
equation [EQN: 56] where ELEV is the elevation of the site in m.
Equation:
  PB = 101 - 0.0115 * ELEV + 5.44e10-7 * sqr(ELEV)
Code:
  same
Variables:
  PB = barometricPressure_kPa
  ELEV = siteElevation_m
 }
{ EQN: 56 }
class function EQH.BarometricPressure_kPa(siteElevation_m: single): single;
  begin
  try
  { notation: 5.44e-7 }
  result := 101.3 - siteElevation_m * (0.01152 - 0.000000544 * siteElevation_m);
  except on e: Exception do result := errorMessage('Exception in EQH.BarometricPressure_kPa: ' + e.message); end;
  end;

{ 
The soil heat flux is estimated by using air temperature on the day of interest
plus three days prior: [EQN: 57] where T is the mean daily air temperature
on day i in degrees C. Since G is usually relatively small, it is assumed
zero in EPIC.
Equation:
  G = 0.12 * (T(i) - (T(i-1) + T(i-2) + T(i-3)) / 3)
Code:
  G is assumed to be zero in the code
  I am implementing it because in a microclimate the heat flux might be more important.
Variables:
  G = EQ::SoilHeatFlux_MJPM2
  T(i) = meanTempForDay_degC
  T(i-1) = dailyMeanTempMinus1Day_degC
  T(i-2) = dailyMeanTempMinus2Days_degC
  T(i-3) = dailyMeanTempMinus3Days_degC
 }
{ EQN: 57 }
class function EQH.SoilHeatFlux_MJPM2(meanTempForDay_degC: single; prevThreeDaysMeanTemp_degC: arrayThree): single;
  var
    average: single;
  begin
  try
  average := (prevThreeDaysMeanTemp_degC[0] + prevThreeDaysMeanTemp_degC[1] + prevThreeDaysMeanTemp_degC[2]) / 3.0;
  result := 0.12 * (meanTempForDay_degC - average);
  except on e: Exception do result := errorMessage('Exception in EQH.SoilHeatFlux_MJPM2: ' + e.message); end;
  end;

{ 
Solar radiation is adjusted to obtain net radiation by using the equation
[EQN: 58] where RA is the solar radiation in MJ/m2, AB is albedo, RAB is
the net outgoing long wave radiation in MJ/m2 for clear days, and RAMX is the
maximum solar radiation possible in MJ/m2 for the location on day i.
Equation:
  h(o) = RA * (1.0 - AB) - RAB * ((0.9 * RA) / RAMX + 0.1)
Code:
  same
Variables:
  h(o) = EQ::NetRadiationForDay_MJPm2
  RA = radiationForDay_MJPm2
  AB = albedo_frn
  RAB = netOutgoingRadiationIfDayIsClear_MJPm2
  RAMX = maxPossibleRadiation_MJPm2
 }
{ EQN: 58 }
class function EQH.NetRadiationForDay_MJPm2(radiationForDay_MJPm2: single; albedo_frn: single;
  netOutgoingRadiationIfDayIsClear_MJPm2: single; maxPossibleRadiation_MJPm2: single): single;
  begin
  try
  if maxPossibleRadiation_MJPm2 = 0.0 then
    result := 0.0
  else
    begin
    result := radiationForDay_MJPm2 * (1.0 - albedo_frn)
      - netOutgoingRadiationIfDayIsClear_MJPm2 * (safediv(0.9 * radiationForDay_MJPm2, maxPossibleRadiation_MJPm2) + 0.1);
    if result < 0.0 then result := 0.0;
    end;
  except on e: Exception do result := errorMessage('Exception in EQH.NetRadiationForDay_MJPm2: ' + e.message); end;
  end;

{ 
The RAB value is estimated with the equation [EQN: 59].
Equation:
  RAB = 4.9x10-9 * (0.34 - 0.14 * sqrt(e(d))) * power(T + 273, 4)
Code:
  same
Variables:
  RAB = netOutgoingRadiationIfDayIsClear_MJPm2
  e(d) = vaporPressureAtMeanTemp_kPa
  T = meanTempForDay_degC
 }
{ EQN: 59 }
class function EQH.NetOutgoingLongWaveRadiationClearDays_MJPm2(vaporPressureAtMeanTemp_kPa: single; meanTempForDay_degC:
  single): single;
  begin
  try
  { notation: 4.9e-9 }
  result := 0.0000000049 * (0.34 - 0.14 * sqrt(vaporPressureAtMeanTemp_kPa)) * power(meanTempForDay_degC + 273, 4);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.NetOutgoingLongWaveRadiationClearDays_MJPm2: ' + e.message); end;
  end;

{ 
The maximum possible solar radiation is computed with the equations
[EQN: 60] and [EQN: 61] where LAT is the latitude of the site in degrees,
SD is the sun's declination angle (radians), and i is the day of the year.
Equation:
  RAMX = 30 * (1.0 + 0.0335 * sin((2pi/365) * (i + 88.2)) * (XT * sin((2pi/360) * LAT)
         * sin(SD) + cos((2pi/360) * LAT) * cos(SD) * sin(XT)))
Code:
  same except in the code this is calculated only once per month
  GS will calculate it daily
Variables:
  RAMX = EQ::MaxPossibleRadiation_MJPm2
  i = julianDay
  XT = maxPossibleRadiationVariable
  LAT = stationLatitude_rad
  SD = declinationAngleOfSun_rad
 }
{ EQN: 60 }
class function EQH.MaxPossibleRadiation_MJPm2(julianDay: integer; maxPossibleRadiationVariable: single; stationLatitude_rad:
  single; declinationAngleOfSun_rad: single): single;
  var
    firstProduct: single;
    secondProduct: single;
  begin
  try
  firstProduct := 1.0 + 0.0335 * sin((2.0 * kPi / 365) * (julianDay + 88.2));
  secondProduct := maxPossibleRadiationVariable * sin(stationLatitude_rad) * sin(declinationAngleOfSun_rad) +
    cos(stationLatitude_rad) * cos(declinationAngleOfSun_rad) * sin(maxPossibleRadiationVariable);
  result := 30.0 * firstProduct * secondProduct;
  except on e: Exception do result := errorMessage('Exception in EQH.MaxPossibleRadiation_MJPm2: ' + e.message); end;
  end;

{ 
Equation:
  XT = acos(-tan((2pi/360) * LAT) * tan(SD)), 0 <= XT <= pi
Code:
  same
Variables:
  XT = EQ::MaxPossibleRadiationVariable
  LAT = stationLatitude_rad
  SD = declinationAngleOfSun_rad
 }
{ EQN: 61 }
class function EQH.MaxPossibleRadiationVariable(stationLatitude_rad: single; declinationAngleOfSun_rad: single): single;
  begin
  try
  result := -tan(stationLatitude_rad) * tan(declinationAngleOfSun_rad);
  if (result >= 1.0) then
    result := 0.0
  else if (result <=  - 1.0) then
    result := kPi
  else
    result := arcCos(result);
  except on e: Exception do result := errorMessage('Exception in EQH.MaxPossibleRadiationVariable: ' + e.message); end;
  end;

{ 
Finally, the wind function of the Penman equation is approximated with the
relationship [EQN: 63] where V is the mean daily wind speed (at a 10 meter
height) in meters/second.
Equation:
  f(V) = 2.7 + 1.63 * V
Code:
  same
Variables:
  f(V) = PenmanWindSpeedFunction_mmPdaykPa
  V = meanWindSpeedForDay_mPsec
 }
{ EQN: 63 }
class function EQH.PenmanWindSpeedFunction_mmPdaykPa(meanWindSpeedForDay_mPsec: single): single;
  begin
  try
  result := 2.7 + 1.63 * meanWindSpeedForDay_mPsec;
  except on e: Exception do result := errorMessage('Exception in EQH.PenmanWindSpeedFunction_mmPdaykPa: ' + e.message); end;
  end;

{ 
Potential evaporation - Penman-Monteith method

The Penman-Monteith method (Monteith, 1965) was recently added to EPIC to
provide a means for estimating the effects of CO2 changes (Stockle et al.,
1992). The Penman-Monteith equation is expressed as [EQN: 64] where AD is the
air density in g/m3 and AR is the aerodynamic resistance for heat and vapor
transfer in s/m.
Equation:
  E(o) = (delta * (h(o) - G) + 86.7 * AD * (e(a) - e(d)) / AR)
         / (HV * (delta + gamma))
Code:
  same
Variables:
  E(o) = PotentialSoilEvapByPenmanMonteith_mm
  delta = slopeSaturVaporPressureCurve_kPaPdegC
  h(0) = netRadiationForDay_MJPm2
  G = soilHeatFlux_MJPM2
  AD = airDensity_gPm3
  e(a) - e(d) = vaporPressureDeficit_kPa
  AR = aeroResistForHeatAndVaporTransfer_secPm
  HV = latentHeatOfVaporization_MJPkg
  gamma = psychromConstant_kPaPdegC
 }
{ EQN: 64 }
class function EQH.PotentialSoilEvapByPenmanMonteith_mm(slopeSaturVaporPressureCurve_kPaPdegC: single;
  netRadiationForDay_MJPm2: single; soilHeatFlux_MJPM2: single; airDensity_gPm3: single; vaporPressureDeficit_kPa: single;
  aeroResistForHeatAndVaporTransfer_secPm: single; latentHeatOfVaporization_MJPkg: single; psychromConstant_kPaPdegC:
  single): single;
  var
    numerator: single;
    denominator: single;
  begin
  try
  numerator := EQH.PotSoilOrPlantEvapPenmanMonteithNumerator(slopeSaturVaporPressureCurve_kPaPdegC,
    netRadiationForDay_MJPm2, soilHeatFlux_MJPM2, airDensity_gPm3, vaporPressureDeficit_kPa,
    aeroResistForHeatAndVaporTransfer_secPm);
  denominator := latentHeatOfVaporization_MJPkg * (slopeSaturVaporPressureCurve_kPaPdegC + psychromConstant_kPaPdegC);
  result := safediv(numerator, denominator);
  except on e: Exception do result := errorMessage('Exception in EQH.PotentialSoilEvapByPenmanMonteith_mm: ' + e.message); end;
  end;

{ 
The Penman-Monteith method also estimates plant water evaporation (all other
methods use a common equation which will come later), which is [EQN: 65]
where CR is the canopy resistance for vapor transfer in s/m.
Equation:
  E(p) = (delta * (h(o) - G) + 86.7 * AD * (e(a) - e(d)) / AR)
         / (HV * (delta + gamma * (1.0 + CR / AR)))
Code:
  same
Variables:
  E(p) = EQ::PotentialPlantEvapByPenmanMonteith_mm
  delta = slopeSaturVaporPressureCurve_kPaPdegC
  h(0) = netRadiationForDay_MJPm2
  G = soilHeatFlux_MJPM2
  AD = airDensity_gPm3
  e(a) - e(d) = vaporPressureDeficit_kPa
  AR = aeroResistForHeatAndVaporTransfer_secPm
  HV = latentHeatOfVaporization_MJPkg
  gamma = psychromConstant_kPaPdegC
  CR = canopyResisForVaporTransfer_secPm
 }
{ EQN: 65 }
class function EQH.PotentialPlantEvapByPenmanMonteith_mm(slopeSaturVaporPressureCurve_kPaPdegC: single;
  netRadiationForDay_MJPm2: single; soilHeatFlux_MJPM2: single; airDensity_gPm3: single; vaporPressureDeficit_kPa: single;
  aeroResistForHeatAndVaporTransfer_secPm: single; latentHeatOfVaporization_MJPkg: single; psychromConstant_kPaPdegC:
  single; canopyResisForVaporTransfer_secPm: single): single;
  var
    numerator: single;
    canopyResisOverAerodynamicResis_secPm: single;
    denominator: single;
  begin
  try
  numerator := EQH.PotSoilOrPlantEvapPenmanMonteithNumerator(slopeSaturVaporPressureCurve_kPaPdegC,
    netRadiationForDay_MJPm2, soilHeatFlux_MJPM2, airDensity_gPm3, vaporPressureDeficit_kPa,
    aeroResistForHeatAndVaporTransfer_secPm);
  canopyResisOverAerodynamicResis_secPm := safediv(canopyResisForVaporTransfer_secPm,
    aeroResistForHeatAndVaporTransfer_secPm);
  denominator := latentHeatOfVaporization_MJPkg * (slopeSaturVaporPressureCurve_kPaPdegC + psychromConstant_kPaPdegC *
    (1.0 + canopyResisOverAerodynamicResis_secPm));
  result := safediv(numerator, denominator);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.PotentialPlantEvapByPenmanMonteith_mm: ' + e.message); end;
  end;

{ the preceding two equations have identical numerators, so the code is combined here }
class function EQH.PotSoilOrPlantEvapPenmanMonteithNumerator(slopeSaturVaporPressureCurve_kPaPdegC: single;
  netRadiationForDay_MJPm2: single; soilHeatFlux_MJPM2: single; airDensity_gPm3: single; vaporPressureDeficit_kPa: single;
  aeroResistForHeatAndVaporTransfer_secPm: single): single;
  begin
  try
  result := slopeSaturVaporPressureCurve_kPaPdegC * (netRadiationForDay_MJPm2 - soilHeatFlux_MJPM2) + 86.66 *
    airDensity_gPm3 * safediv(vaporPressureDeficit_kPa, aeroResistForHeatAndVaporTransfer_secPm);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.PotSoilOrPlantEvapPenmanMonteithNumerator: ' + e.message); end;
  end;

{ 
Air density is estimated with the equation [EQN: 66].
Equation:
  AD = 0.01276 * PB / (1.0 + 0.0367 * T)
Code:
  same
Variables:
  AD = AirDensity_gPm3
  PB = barometricPressure_kPa
  T = meanTempForDay_degC
 }
{ EQN: 66 }
class function EQH.AirDensity_gPm3(barometricPressure_kPa: single; meanTempForDay_degC: single): single;
  begin
  try
  result := 0.01276 * safediv(barometricPressure_kPa, 1.0 + 0.0367 * meanTempForDay_degC);
  except on e: Exception do result := errorMessage('Exception in EQH.AirDensity_gPm3: ' + e.message); end;
  end;

{ 
The aerodynamic resistance is computed with the equation [EQN: 67] where ZD
is the displacement height of the crop in m, ZO is the surface roughness
parameter in m, and V is the daily mean wind speed in m/sec.
Equation:
  AR = 6.25 * sqr(ln((10.0 - ZD) / ZO)) / V
Code:
  same, but if cropHeight_m > 8.0,
    the 10.0 is replaced with cropHeight_m + 2
    and V is replaced with V * ln((cropHeight_m + 2) / 0.0005) / 9.9035
Variables:
  AR = AeroResistForHeatAndVaporTransfer_secPm
  ZD = displacementHeightOfCrop_m
  ZO = surfaceRoughnessParam_m
  V = meanWindSpeedForDay_mPsec
 }
{ EQN: 67 }
class function EQH.AeroResistForHeatAndVaporTransferIfCrop_secPm(cropHeight_m: single; meanWindSpeedForDay_mPsec: single;
  surfaceRoughParam_m: single; displacementHeightOfCrop_m: single): single;
  var
    heightToCompareTo: single;
    windSpeedToUse_mPsec: single;
  begin
  try
  if (cropHeight_m > 8.0) then
    begin
    heightToCompareTo := cropHeight_m + 2.0;
    windSpeedToUse_mPsec := meanWindSpeedForDay_mPsec * safeLn(heightToCompareTo / 0.0005) / 9.9035;
    end
  else
    begin
    heightToCompareTo := 10.0;
    windSpeedToUse_mPsec := meanWindSpeedForDay_mPsec;
    end;
  result := 6.25 * safediv(sqr(safeLn(safediv(heightToCompareTo - displacementHeightOfCrop_m, surfaceRoughParam_m))),
    windSpeedToUse_mPsec);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.AeroResistForHeatAndVaporTransferIfCrop_secPm: ' + e.message); end;
  end;

{ 
The surface roughness is estimated with the equation [EQN: 68] and the crop
displacement height is estimated with the equation [EQN: 69] where CHT
is crop height in m.
Equation:
  ZO = 0.131 * power(CHT, 0.997)
Code:
  ZO = power(10.0, 0.979 * log10(CHT) - 0.883)
Variables:
  ZO = EQ::SurfaceRoughnessParam_m
  CHT = cropHeight_m
 }
{ EQN: 68 }
class function EQH.SurfaceRoughnessParam_m(cropHeight_m: single): single;
  begin
  try
  result := power(10.0, 0.997 * log10(cropHeight_m) - 0.883);
  except on e: Exception do result := errorMessage('Exception in EQH.SurfaceRoughnessParam_m: ' + e.message); end;
  end;

{ 
Equation:
  ZD = 0.702 * power(CHT, 0.979)
Code:
  ZD = power(10.0, 0.979 * log10(CHT) - 0.154)
Variables:
  ZD = DisplacementHeightOfCrop_m
  CHT = cropHeight_m
 }
{ EQN: 69 }
class function EQH.DisplacementHeightOfCrop_m(cropHeight_m: single): single;
  begin
  try
  result := power(10.0, 0.979 * log10(cropHeight_m) - 0.154);
  except on e: Exception do result := errorMessage('Exception in EQH.DisplacementHeightOfCrop_m: ' + e.message); end;
  end;

{ 
When no crop is growing the aerodynamic resistance is estimated with the
equation [EQN: 70].
Equation:
  AR = 350.0 / V
Code:
  same
Variables:
  AR = EQ::AeroResistForHeatAndVaporTransferNoCrop_secPm
  V = meanWindSpeedForDay_mPsec
 }
{ EQN: 70 }
class function EQH.AeroResistForHeatAndVaporTransferNoCrop_secPm(meanWindSpeedForDay_mPsec: single): single;
  begin
  {change from EPIC - pdf - put in bound for this function to avoid divide by zero error if no wind today}
  if meanWindSpeedForDay_mPsec <= 0.1 then
    begin
    result := 3500.0;
    exit;
    end;
  try
  result := safediv(350.0, meanWindSpeedForDay_mPsec);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.AeroResistForHeatAndVaporTransferNoCrop_secPm: ' + e.message); end;
  end;

{ 
The canopy resistance is computed with the equation [EQN: 71] where p(1)
is a parameter ranging from 1.0 to 2.0, LAI is the leaf area index of the
crop, g*(o) is the leaf conductance in m/sec, and CO2 is the carbon dioxide
level in the atmosphere in ppm.
Equation:
  CR = p(1) / (LAI * g*(o) * (1.4 - 0.00121 * CO2))
Code:
  same (0.4 / 330.0 = 0.00121)
Variables:
  CR = CanopyResistForVaporTransfer_secPm
  p(1) = canopyResistParam
  LAI = leafAreaIndex
  g*(o) = leafConductance_mPsec
  CO2 = carbonDioxideInAtmosphere_ppm
 }
{ EQN: 71 }
class function EQH.CanopyResistForVaporTransfer_secPm(canopyResistParam: single; leafAreaIndex: single; leafConductance_mPsec:
  single; carbonDioxideInAtmosphere_ppm: single): single;
  begin
  try
  if (leafAreaIndex = 0.0) or (leafConductance_mPsec = 0.0) then
    result := 0.0
  else
    result := safediv(canopyResistParam, leafAreaIndex * leafConductance_mPsec * (1.4 - 0.4 *
      carbonDioxideInAtmosphere_ppm / 330.0));
  except on e: Exception do result := errorMessage('Exception in EQH.CanopyResistForVaporTransfer_secPm: ' + e.message); end;
  end;

{ 
Leaf conductance is estimated from the crop input rate adjusted for vapor
pressure deficit (VPD) [EQN: 72] where g(o) is the crop's leaf resistance
when the VPD is less than the crop's threshold VPD, and FV is the VPD
correction factor.
Equation:
  g*(o) = g(o) * FV
Code:
  same
Variables:
  g*(o) = LeafConductance_mPsec
  g(o) = leafResistIfVaporPressureDeficitBelowThresholdForCrop_mPsec
  FV = vaporPressureDeficitCorrectionFactor
 }
{ EQN: 72 }
class function EQH.LeafConductance_mPsec(leafResistIfVPDBelowThreshold_mPsec: single; vaporPressureDeficitCorrectionFactor:
  single): single;
  begin
  try
  result := leafResistIfVPDBelowThreshold_mPsec * vaporPressureDeficitCorrectionFactor;
  except on e: Exception do result := errorMessage('Exception in EQH.LeafConductance_mPsec: ' + e.message); end;
  end;

{ 
FV is calculated by [EQN: 73] where b(v) is a crop coefficient and VPD(t) is
threshold VPD for the crop.
Equation:
  FV = 1.0 - b(v) * (VPD - VPD(t)), >= 0.1
Code:
  same, except if VPD < VPD(t), FV = 1.0
Variables:
  FV = EQ::VaporPressureDeficitCorrectionFactor
  b(v) = fractionOfMaxLeafConductForHighVPD_frn
  VPD = vaporPressureDeficit_kPa
  VPD(t) = thresholdVaporPressureDeficit_kPa
 }
{ EQN: 73 }
class function EQH.VaporPressureDeficitCorrectionFactor(fractionOfMaxLeafConductForHighVPD_frn: single;
  vaporPressureDeficit_kPa: single; thresholdVaporPressureDeficit_kPa: single): single;
  var
    vpdOverThreshold_kPa: single;
  begin
  try
  vpdOverThreshold_kPa := vaporPressureDeficit_kPa - thresholdVaporPressureDeficit_kPa;
  if (vpdOverThreshold_kPa < 0.0) then 
    result := 1.0
  else
    result := max(0.1, 1.0 - fractionOfMaxLeafConductForHighVPD_frn * vpdOverThreshold_kPa);
  except on e: Exception do result := errorMessage('Exception in EQH.VaporPressureDeficitCorrectionFactor: ' + e.message); end;
  end;

{ 
Potential evaporation - Priestley-Taylor method

The Priestley-Taylor (1972) method provides estimates of potential evaporation
without wind and relative humidity inputs. The simplified equation based only
on temperature and radiation is [EQN: 74].
Equation:
  E(o) = 1.28 * (h(o) / HV) * (delta / (delta + gamma))
Code:
  same
Variables:
  E(o) = EQ::PotentialSoilEvapByPriestleyTaylor_mm
  h(o) = netRadiationByPriestleyTaylor_MJPm2
  HV = latentHeatOfVaporization_MJPkg
  delta = slopeSaturVaporPressureCurve_kPaPdegC
  gamma = psychromConstant_kPaPdegC
 }
{ EQN: 74 }
class function EQH.PotentialSoilEvapByPriestleyTaylor_mm(netRadiationByPriestleyTaylor_MJPm2: single;
  latentHeatOfVaporization_MJPkg: single; slopeSaturVaporPressureCurve_kPaPdegC: single; psychromConstant_kPaPdegC:
  single): single;
  begin
  try
  result := 1.28 * safediv(netRadiationByPriestleyTaylor_MJPm2, latentHeatOfVaporization_MJPkg) *
    safediv(slopeSaturVaporPressureCurve_kPaPdegC, slopeSaturVaporPressureCurve_kPaPdegC + psychromConstant_kPaPdegC);
  except on e: Exception do
  	result := errorMessage('Exception in EQH.PotentialSoilEvapByPriestleyTaylor_mm: ' + e.message); end;
  end;

{ 
The net radiation (for the Priestley-Taylor method) is estimated with the
equation [EQN: 75] instead of equation 58 which is used in the Penman method.
Equation:
  h(o) = RA * (1.0 - AB)
Code:
  same
Variables:
  h(o) = EQ::NetRadiationByPriestleyTaylor_MJPm2
  RA = radiationForDay_MJPm2
  AB = albedo_frn
 }
{ EQN: 75 }
class function EQH.NetRadiationByPriestleyTaylor_MJPm2(radiationForDay_MJPm2: single; albedo_frn: single): single;
  begin
  try
  result := radiationForDay_MJPm2 * (1.0 - albedo_frn);
  except on e: Exception do result := errorMessage('Exception in EQH.NetRadiationByPriestleyTaylor_MJPm2: ' + e.message); end;
  end;

{ 
Potential evaporation - Hargreaves method

The Hargreaves method (Hargreaves and Samani, 1985) estimates potential
evapotranspiration as a function of extraterrestrial radiation and air
temperature. Hargreaves' method was modified for use in EPIC by
increasing the temperature difference exponent from 0.5 to 0.6. Also,
extraterrestrial radiation is replaced by RAMX (maximum possible solar
radiation at the earth's surface) and the coefficient is adjusted from
0.0023 to 0.0032 for proper conversion. The modified equation is [EQN: 76]
where T(mx) and T(mn) are the daily maximum and minimum air temperatures
in degrees C.
Equation:
  E(o) = 0.0032 * (RAMX / HV) * (T + 17.8) * power(T(mx) - T(mn), 0.6)
Code:
  same
Variables:
  E(o) = PotentialSoilEvapByHargreaves_mm
  RAMX = maxPossibleRadiation_MJPm2
  HV = latentHeatOfVaporization_MJPkg
  T = meanTempForDay_degC
  T(mx) = maxTempForDay_degC
  T(mn) = minTempForDay_degC
 }
{ EQN: 76 }
class function EQH.PotentialSoilEvapByHargreaves_mm(maxPossibleRadiation_MJPm2: single; latentHeatOfVaporization_MJPkg: single;
  meanTempForDay_degC: single; maxTempForDay_degC: single; minTempForDay_degC: single): single;
  begin
  try
  result := 0.0032 * safediv(maxPossibleRadiation_MJPm2, latentHeatOfVaporization_MJPkg) * (meanTempForDay_degC +
    17.8) * power(maxTempForDay_degC - minTempForDay_degC, 0.6);
  except on e: Exception do result := errorMessage('Exception in EQH.PotentialSoilEvapByHargreaves_mm: ' + e.message); end;
  end;

{ 
All four methods estimate albedo by considering the soil, crop, and snow
cover. If a snow cover exists with 5 mm or greater water content, the value
of albedo is set to 0.6. If the snow cover is less than 5 mm and no crop
is growing, the soil albedo is the appropriate value. When crops are growing,
albedo is determined by using the equation [EQN: 77] where 0.23 is the albedo
for plants, AB(s) is the soil albedo, and EA is a soil cover index.
Equation:
  if snowWaterContent_mm > 5 AB = 0.6
  if snowWaterContent_mm < 5 and no crop is growing, AB = AB(s)
  if crops are growing AB = 0.23 * (1.0 - EA) + AB(s) * EA
Code:
  same
Variables:
  AB = EQ::Albedo_frn
  EA = soilCoverIndex_frn
  AB(s) = soilAlbedo_frn
 }
{ EQN: 77 }
class function EQH.Albedo_frn(soilCoverIndex_frn: single; soilAlbedo_frn: single; snowCoverWaterContent_mm: single;
  leafAreaIndex: single): single;
  begin
  try
  if (snowCoverWaterContent_mm >= 5.0) then 
    result := 0.6
  else if (leafAreaIndex <= 0.0) then
    result := soilAlbedo_frn
  else
    result := 0.23 * (1.0 - soilCoverIndex_frn) + soilAlbedo_frn * soilCoverIndex_frn;
  except on e: Exception do result := errorMessage('Exception in EQH.Albedo_frn: ' + e.message); end;
  end;

{ 
The value of EA ranges from 0.0 to 1.0 according to the equation [EQN: 78]
where CV is the sum of the above ground biomass and crop residue in t/ha.
Equation:
  EA = exp(-0.05 * CV)
Code:
  same except if snowWaterContent_mm > 5 EA = 1.0
Variables:
  EA = EQ::SoilCoverIndex_frn
  CV = aboveGroundBiomassAndResidue_tPha
 }
{ EQN: 78 }
class function EQH.SoilCoverIndex_frn(aboveGroundBiomassAndResidue_tPha: single; snowWaterContent_mm: single): single;
  begin
  try
  if (snowWaterContent_mm >= 5.0) then 
    result := 1.0
  else
    result := safeExp(-0.05 * aboveGroundBiomassAndResidue_tPha);
  except on e: Exception do result := errorMessage('Exception in EQH.SoilCoverIndex_frn: ' + e.message); end;
  end;

{ 
Soil and Plant Evaporation

The model computes evaporation from soils and plants separately by an approach
similar to that of Ritchie (1972). For all methods except Penman-Monteith,
potential plant water evaporation is computed with the equations [EQN: 79]
and [EQN: 80] where E(p) is the predicted plant water evaporation rate
in mm/day. If soil water is limited, plant water evaporation will be reduced
as described in the plant growth section of this chapter.
Equation:
  E(p) = (E(o) * LAI) / 3.0 if 0.0 < LAI < 3.0
  E(p) = E(o) if LAI > 3.0
Code:
  E(p) = min((E(o) * LAI) / 3.0, E(o))
  this comes out the same because if LAI > 3, E(o) * LAI / 3 > E(o)
Variables:
  E(p) = potPlantEvap_mm
  E(o) = potentialSoilEvap_mm
  LAI =  leafAreaIndex
 }
{ EQN: 79 EQN: 80 }
class function EQH.PotentialPlantEvapNotPenmanMonteith_mm(potentialSoilEvap_mm: single; leafAreaIndex: single): single;
  begin
  try
  result := min(potentialSoilEvap_mm * leafAreaIndex / 3.0, potentialSoilEvap_mm);
  if result < 0.0 then result := 0.0;
  except on e: Exception do
  	result := errorMessage('Exception in EQH.PotentialPlantEvapNotPenmanMonteith_mm: ' + e.message); end;
  end;

{ 
Potential soil water evaporation is simulated by considering soil cover
according to the following equation [EQN: 81] where E(s) is the potential
soil water evaporation rate in mm/day.
Equation:
  E(s) = E(o) * EA
Code:
  same
Variables:
  E(s) = PotentialSoilEvapAdjByCover_mm
  E(o) = potentialSoilEvap_mm
  EA = soilCoverIndex_frn
 }
{ EQN: 81 }
class function EQH.PotentialSoilEvapAdjByCover_mm(potentialSoilEvap_mm: single; soilCoverIndex_frn: single): single;
  begin
  try
  result := potentialSoilEvap_mm * soilCoverIndex_frn;
  except on e: Exception do result := errorMessage('Exception in EQH.PotentialSoilEvapAdjByCover_mm: ' + e.message); end;
  end;

{ 
Potential soil water evaporation is reduced during periods of high plant
water use with the equation [EQN: 82]. When E(p) is low E*(s) approaches
E(s), but as E(p) approaches E(o), E*(s) approaches E(s)/(1.0 + EA).
Equation:
  E*(s) = min(ES, E(s) * E(o) / (E(s) + E(p)))
Code:
  same
Variables:
  E*(s) = EQ::PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm
  E(s) = potentialSoilEvapAdjByCover_mm
  E(o) = potentialSoilEvap_mm
  E(p) = potPlantEvap_mm
 }
{ EQN: 82 }
class function EQH.PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm(potentialSoilEvapAdjByCover_mm: single; potentialSoilEvap_mm:
  single; potPlantEvap_mm: single): single;
  begin
  try
  if potentialSoilEvapAdjByCover_mm <= 0.0 then
    result := 0.0
  else
    result := min(potentialSoilEvapAdjByCover_mm, safediv(potentialSoilEvapAdjByCover_mm * potentialSoilEvap_mm,
      potentialSoilEvapAdjByCover_mm + potPlantEvap_mm));
  except on e: Exception do
  	result := errorMessage('Exception in EQH.PotentialSoilEvapAdjByCoverAndPotPlantEvap_mm: ' + e.message); end;
  end;

{ 
Actual soil water evaporation is estimated on the basis of the top 0.2 m
of soil and snow cover, if any. If 5 mm or more (water content) of snow is
present albedo is set to 0.6 and EA to 0.5 for estimating E(o) and snow
is evaporated at that rate. When all snow is evaporated, soil water evaporation
begins. Such evaporation is governed by soil depth and water content according
to the equation [EQN: 83] where EV is the total soil water evaporation in
mm from soil of depth Z in mm. The coefficients of equation 83 are set to give
EV = 0.5 E*(s) when Z = 10 mm and EV = 0.95 E*(s) when Z = 100 mm.
Equation:
  EV(Z) = E*(s) * (Z / (Z + exp(2.374 - 0.00713 * Z)))
Code:
  same
Variables:
  EV(Z) = PotentialSoilEvapForDepth_mm
  E*(s) = potentialSoilEvapAdjByCoverAndPotPlantEvap_mm
  Z = depth_mm
 }
{ EQN: 83 }
class function EQH.PotentialSoilEvapForDepth(depth_mm: single; potSoilEvap_mm: single; var soilEvapByDepthCoeffs:
  SCurveStructure): single;
  begin
  try
  result := potSoilEvap_mm * scurve(depth_mm, soilEvapByDepthCoeffs.c1, soilEvapByDepthCoeffs.c2);
  except on e: Exception do result := errorMessage('Exception in EQH.PotentialSoilEvapForDepth: ' + e.message); end;
  end;

{ 
Potential soil water evaporation for a layer is estimated by taking the
difference between EV's at the layer boundaries [EQN: 84] where SEV
is the potential soil evaporation for layer l in mm.
Equation:
  SEV = EV(Z(l)) - EV(Z(l-1))
Code:
  same
Variables:
  SEV = PotentialSoilEvapForLayer_mm
 }
{ EQN: 84 }
{ in code }
{ 
The depth distributed estimate of soil water evaporation may be reduced
according to the following equation if soil water is limited in a layer
[EQN: 85] and [EQN: 86] where SEV*(l) is the adjusted soil water evaporation
estimate in mm.
Equation:
  SEV* = SEV * exp(2.5 * (SW - FC) / (FC - WP)) if SW < FC
  SEV* = SEV if SW >= FC
Code:
  same
Variables:
  SEV* = EQ::PotentialSoilEvapForLayerAdjForDryness_mm
  SEV = potentialSoilEvapForLayer_mm
  SW = waterContent_mm
  FC = fieldCapacity_mm
  WP = wiltingPoint_mm
 }
{ EQN: 85 EQN: 86 }
class function EQH.PotentialSoilEvapForLayerAdjForDryness_mm(potentialSoilEvapForLayer_mm: single; waterContent_mm: single;
  fieldCapacity_mm: single; wiltingPoint_mm: single): single;
  begin
  try
  if (waterContent_mm >= fieldCapacity_mm) then 
    result := potentialSoilEvapForLayer_mm
  else
    result := potentialSoilEvapForLayer_mm * safeExp(2.5 * safediv(waterContent_mm - fieldCapacity_mm, fieldCapacity_mm -
      wiltingPoint_mm));
  except on e: Exception do
  	result := errorMessage('Exception in EQH.PotentialSoilEvapForLayerAdjForDryness_mm: ' + e.message); end;
  end;

{ 
The final step in adjusting the evaporation estimate is to assure that the
soil water supply is adequate to meet the demand [EQN: 87] where b(w) may
range from 0.0 to 1.0 in the top 0.5 m of soil and is set to 1.0 below
0.5 m. Thus, EPIC can be adjusted to allow the top 0.5 m to dry down to
any fraction of wilting point.
Equation:
  SEV* = min(SEV*, SW - b(w) * WP)
Code:
  same
  code has
  if (SW - SEV* < b(w) * WP) SEV* = SW - b(W) * WP otherwise SEV* = SEV*
  if SW - SEV* - b(w) * WP < 0
  if SW - b(w) * WP - SEV* < 0
  if SEV* > SW - b(w) * WP
  which is the same as min(SEV*, SW - b(w) * WP)
Variables:
  SEV* = EQ::SoilEvaporationForLayer_mm
  SEV* = potentialSoilEvapForLayer_mm
  SW = waterContent_mm
  b(w) = lowerLimitWaterContentInTopP5MAsFractOfWP_frn
  WP = wiltingPoint_mm
 }
{ EQN: 87 }
class function EQH.SoilEvaporationForLayer_mm(waterContent_mm: single; potentialSoilEvapForLayer_mm: single;
  lowerLimitWaterContentInTopP5MAsFractOfWP_frn: single; wiltingPoint_mm: single): single;
  begin
  try
  result := min(potentialSoilEvapForLayer_mm, waterContent_mm - lowerLimitWaterContentInTopP5MAsFractOfWP_frn *
    wiltingPoint_mm);
  if result < 0.0 then result := 0.0;
  except on e: Exception do result := errorMessage('Exception in EQH.SoilEvaporationForLayer_mm: ' + e.message); end;
  end;

{ 
                                 HYDROLOGY - *SNOWMELT*

If snow is present, it may be melted on days when the second soil layer
temperature exceeds 0 degrees C. Snow is melted as a function of the snow
pack temperature using the equation [EQN: 88] where SML is the snowmelt
rate in mm/day, SNO is the snow present in mm of water, T is the mean
daily air temperature in degrees C, and SPT is the snow pack temperature
in degrees C.
Equation:
  SML = T * (1.52 + 0.54 * SPT), 0 <= SML <= SNO
Code:
  same
Variables:
  SML = EQ::Snowmelt_mm
  T = meanTempForDay_degC
  SPT = snowPackTemp_degC
  SNO = snowWaterContent_mm
 }
{ EQN: 88 }
class function EQH.Snowmelt_mm(meanTempForDay_degC: single; snowPackTemp_degC: single; snowWaterContent_mm: single): single;
  begin
  try
  result := meanTempForDay_degC * (1.52 + 0.54 * snowPackTemp_degC);
  if (result < 0.0) then result := 0.0;
  if (result > snowWaterContent_mm) then result := snowWaterContent_mm;
  except on e: Exception do result := errorMessage('Exception in EQH.Snowmelt_mm: ' + e.message); end;
  end;

{ 
The snow pack temperature is estimated with the equation [EQN: 89] where
T(s) is the temperature at the top of the snow pack and T(2) is the
temperature at the center of soil layer 2. The equations for estimating
T(s) and T(2) are presented in the soil temperature section. Melted
snow is treated the same as rainfall for estimating runoff volume and
percolation, but rainfall energy is set to 0.0 and peak runoff rate
is estimated by assuming uniformly distributed rainfall for a 24-hr
duration.
Equation:
  SPT = min(T(s), T(2))
Code:
  SPT = (2.0 * T(surf) + T) / 3.0
Variables:
  SPT = EQ::SnowPackTemp_degC
  T(s) = tempAtTopOfSnowPack_degC
  T(2) = tempAtCenterOfSoilLayerTwo_degC
  T(surf) = soilSurfaceTemp_degC
  T = meanTempForDay_degC
 }
{ EQN: 89 }
class function EQH.SnowPackTemp_degC(soilSurfaceTemp_degC: single; meanTempForDay_degC: single): single;
  begin
  try
  result := 0.3333 * (2.0 * soilSurfaceTemp_degC + meanTempForDay_degC);
  except on e: Exception do result := errorMessage('Exception in EQH.SnowPackTemp_degC: ' + e.message); end;
  end;

{ 
                      HYDROLOGY - *WATER TABLE* DYNAMICS

The water table height is simulated without direct linkage to other soil water
processes in the root zone to allow for offsite water effects. The model
drives the water table up and down between input values of maximum and
minimum depths from the surface. The driving mechanism is a function of
rainfall, surface runoff, and potential evaporation, as given in the
equation [EQN: 90] where WTBL is the depth in m from the surface to the
water table on day i, W1 is the driving function, and WTL is the appropriate
limit.

The driving equations are [EQN: 91] and [EQN: 92] where RFS, QS, and EOS
are the sums of rainfall, runoff, and potential evaporation for 30 days
before day i and W2 is a scaling factor. Equation 92 causes the water table
to rise faste than it falls because the denominator is larger during
recession.

The maximum wtaer table depth, WTMX, is substituted into equation 90 for
WTL when the water table is falling (when W2 is negative). Conversely,
WTL is set to the minimum water table depth, WTMN, on the rising side [EQN: 93]
and [EQN: 94].

Obviously, equation 90 gives highest rates when W2 is large and when WTBL ~~
WTMN the rate of rise approaches zero. The reverse is true on the falling
side.
Equation:
  WTBL(i) = WTBL(i-1) - W1 * (WTBL(i-1) - WTL)
  W1 = min(0.1, abs(W2))
  WTL = WTMX if W2 <= 0.0, WTL = WTMN if W2 > 0.0
Code:
  same
Variables:
  WTBL(i) = WaterTableDepth_m
  WTBL(i-1) = waterTableDepthYesterday_m
  W1 = boundedDrivingFunction
  W2 = drivingFunction
  WTL = limitToWaterTable
  WTMX = maxWaterTableDepth_m
  WTMN = minWaterTableDepth_m
 }
{ EQN: 90 EQN: 91 EQN: 93 EQN: 94 }
class function EQH.WaterTableDepth_m(waterTableDepthYesterday_m: single; maxWaterTableDepth_m: single; minWaterTableDepth_m:
  single; drivingFunction: single): single;
  var
    boundedDrivingFunction: single;
    limitToWaterTable: single;
  begin
  try
  boundedDrivingFunction := 0.0;
  limitToWaterTable := 0.0;
  if (drivingFunction <= 0) then 
    limitToWaterTable := maxWaterTableDepth_m
  else
    limitToWaterTable := minWaterTableDepth_m;
  boundedDrivingFunction := min(0.1, abs(drivingFunction));
  result := waterTableDepthYesterday_m - boundedDrivingFunction * (waterTableDepthYesterday_m - limitToWaterTable);
  except on e: Exception do result := errorMessage('Exception in EQH.WaterTableDepth_m: ' + e.message); end;
  end;

{ 
Equation:
  W2 = (RFS - QS - EOS) / EOS)
Code:
  same
Variables:
  W2 = EQ::WaterTableDepthDrivingFunction
  RFS - QS = sumRainfallMinusRunoffPrev30Days_mm
  EOS = sumPotentialEvapPrev30Days_mm
 }
{ EQN: 92 }
class function EQH.WaterTableDepthDrivingFunction(sumRainfallMinusRunoffPrev30Days_mm: single; sumPotSoilEvapPrev30Days_mm:
  single): single;
  begin
  try
  result := safediv(sumRainfallMinusRunoffPrev30Days_mm - sumPotSoilEvapPrev30Days_mm, sumPotSoilEvapPrev30Days_mm);
  except on e: Exception do result := errorMessage('Exception in EQH.WaterTableDepthDrivingFunction: ' + e.message); end;
  end;

end.

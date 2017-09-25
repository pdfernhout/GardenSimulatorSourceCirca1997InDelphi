unit Ueutils;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ueutils: Assorted math and other functions used by the model objects. The soil layer manipulation
functions are here because they didn't fit in the soil file under Delphi 1.0.
NOTE: The words RE-RAISES EXCEPTION mean that any function that calls the function
SHOULD have a try/except handler somewhere around the function call, because
the function re-throws the exception. This is for reporting, so we know in which
function the function was called when it went wrong. This whole exception handling scheme
was to deal with a bug in Delphi 1.0 in which floating point exceptions (overflow, underflow,
divide by zero, etc) cause the development environment to crash. If you are using the source
code under Delphi 1.0 be aware of this bug. If you have the compiler directive 'GS_DEBUG'
on, the numerical exceptions window (udebug) will show an extra 'break on exception'
check box. If you check this box, the program will stop execution with a 'debugBreak' call.
The 'debugBreak' call doesn't work very well and Delphi doesn't know where the
execution point is, but if you press F7 or F8 at this point the system (usually) comes to
the correct line without crashing. Note that we're not sure the 'debugBreak' call works
under Delphi 2.0.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses uestruct;

const
  kPi =  3.141596;
  kLowestFloatAboveZero = 10e-12; {used to use FLT_MIN in C++; EPIC uses this number}
  kLowestQuantityFloat = 10e-6;
  gRandomSeed: longint = 1023;

{ message function }
function ErrorMessage(errorString: string): single;  {returns zero for convenience}
{ math functions }
function sqr(x: single): single;
function min(a: single; b: single): single;
function max(a: single; b: single): single;
function intMin(a: integer; b: integer): integer;
function intMax(a: integer; b: integer): integer;
procedure addQuantity(var value: single; changeAmount: single);
procedure subtractQuantity(var value: single; changeAmount: single);
function pow(number: extended; exponent: extended): extended;
function power(number: extended; exponent: extended): extended;
function tan(x: extended): extended;
function arcCos(x: extended): extended;
function arcSin(x: extended): extended;
function log10(x: extended): extended;
function safeLn(x: extended): extended;
function safeLnWithResult(x: extended; var failed: boolean): extended;
function safeExp(x: extended): extended;
function safeExpWithResult(x: extended; var failed: boolean): extended;
function safediv(x: single; y: single): single;
function safedivExcept(x: single; y: single; exceptionResult: single): single;
function safedivWithResult(x: single; y: single; var failed: boolean): single;
function erfc(xx: single): single;
{ s curves }
function scurve(x: extended; c1: extended; c2: extended): extended;
function scurveWithResult(x: extended; c1: extended; c2: extended; var failed: boolean): extended;
{ s curve params }
procedure Utils_InitSCurveParam(var sCurve: SCurveStructure; x1, y1, x2, y2: single);
procedure Utils_CalcSCurveCoeffs(var sCurve: SCurveStructure);
procedure Utils_CalcSCurveCoeffsWithResult(var sCurve: SCurveStructure; var failed: boolean);
procedure Utils_GetSetSCurveValue(direction: integer; var param: sCurveStructure; index: integer; var value);
{ distributions }
function Utils_RandomZeroToOne: single;
function Utils_StandardNormalDeviate(one: single; two: single): single;
function Utils_NormalByAdding: single;
function Utils_TriangularDistribution(lowerXLimit: single; xAtPeakOfDist: single; upperXLimit: single): single;
{ defaults and ranges for inputs }
procedure Utils_DefaultFloatIfZero(var value: single; theDefault: single);
procedure Utils_DefaultIntIfZero(var value: smallint; theDefault: smallint);
procedure Utils_EnforceFloatRange(var value: single; theMin: single; theMax: single);
procedure Utils_EnforceIntegerRange(var value: smallint; theMin: smallint; theMax: smallint);
{ setting arrays }
function Utils_AddDayTo30DaysArray_mm(var theArray: arrayThirty; theValue: single): single;
procedure Utils_SetMonthValues(var theArray: arrayMonths; x1: single; x2: single; x3: single; x4: single; x5: single;
  x6: single; x7: single; x8: single; x9: single; x10: single; x11: single; x12: single);
procedure Utils_SetLayerValues(var theArray: arraySoilLayers; x1: single; x2: single; x3: single; x4: single; x5: single; x6:
  single; x7: single; x8: single; x9: single; x10: single);
procedure Utils_SetWindDirectionValues(var theArray: windDirectionsArray; direction: integer; x1: single; x2: single; x3:
  single; x4: single; x5: single; x6: single; x7: single; x8: single; x9: single; x10: single; x11: single; x12: single);
{ unit conversion functions }
procedure Utils_ConvertRadiansAndDegrees(direction: integer; var radians: single; var degrees);
function Utils_DegreesToRadians(degrees: single): single;
function Utils_RadiansToDegrees(radians: single): single;
function Utils_SoilWeightFromBulkDensityAndThickness_tPha(bulkDensity_tPm3: single; thickness_m: single): single;
function Utils_BulkDensityFromSoilWeightAndThickness_tPM3(weight_tPha: single; thickness_m: single): single;
function Utils_SoilThicknessFromWeightAndBulkDensity_m(weight_tPha: single; bulkDensity_tPm3: single): single;
function Utils_OrganicMatterFromOrganicC_tPha(organicC_pct: integer; soilWeight_tPha: single): single;
function Utils_OrganicCFromOrganicMatter_pct(organicMatter_tPha: single; soilWeight_tPha: single): single;
function Utils_PorosityFromBulkDensityRockContentAndThickness(bulkDensity_tPm3, rockContent_pct, thickness_m: single): single;
function Utils_BulkDensityFromPorosityRockContentAndThickness(porosity_mm, rockContent_pct, thickness_m: single): single;
function Utils_MulchDepthFromMulchFlatCropResidue_m(flatCropResidue_tPha: single): single;
function Utils_MulchFlatCropResidueFromMulchDepth_tPha(mulchDepth_m: single): single;
{ specific functions }
function Utils_SaturVaporPressureAtMeanTemp(temp_degK: single): single;
function Utils_IntegrateModifiedExponentialEquation(exponent: single): single;
function Utils_EstimateMaterialForLayer(layer: integer; materialThisLayer: single; materialUpperLayer: single;
  thicknessThisLayer_mm: single; thicknessUpperLayer_mm: single): single;
{ layer functions }
procedure Utils_CheckRelationOfFieldCapacityToPorosity(var layer: LayerStructure);
procedure Utils_TransferPropOfMaterial(var amountFrom: single; var amountTo: single; proportion: single);
procedure Utils_TransferPropOfNutrients(var layerFrom: LayerStructure; var layerTo: LayerStructure; proportion: single);
procedure Utils_AddPropOfMaterial(var amountFrom: single; var amountTo: single; proportion: single);
procedure Utils_AddPropOfNutrients(var layerFrom: LayerStructure; var layerTo: LayerStructure; proportion: single);
procedure Utils_ZeroAllLayerFields(var layer: LayerStructure);
function Utils_LayerPropAboveCriterionDepth_frn(layerDepth_m: single; depthLastLayer_m: single; criterionDepth_m: single;
  var layerBelowCriterionDepthHasBeenConsidered: boolean): single;
procedure Utils_MovePortionOfLayerToEmptyLayer(var layerFrom: LayerStructure; var newLayer: LayerStructure;
  proportion, newDepth_m, depthLayerAbove_m: single);
procedure Utils_MovePortionOfLayerToAnotherLayer(var layerFrom: LayerStructure;
    var layerTo: LayerStructure; thicknessLayerFrom_m, thicknessLayerTo_m,
    propOfLayerFromMoving, propOfLayerToThatWasThereBefore, slopeSteepness_mPm: single;
    layerToIsSurfaceLayer: boolean);

implementation

uses
  Dialogs, SysUtils, uunits, udebug, udomain;

{these consts are really defined in umodel.pas but we don't want to include it - keep current}
const
	kGetField = 0;
	kSetField = 1;

{ error message - returns zero for convenience}
function ErrorMessage(errorString: string): single;
  begin
  result := 0.0;
  DebugPrint(errorString);
  if Domain.menuOptions.stopSimulationAtEndOfDayOnException then
    Domain.stopSimulationAfterTodayBecauseOfException := true;
  end;

{ ---------------------------------------------------------------------------------------------- math functions }
function sqr(x: single): single;
  begin
  result := x * x;
  end;

function min(a: single; b: single): single;
  begin
  if (a < b) then result := a else result := b;
  end;

function max(a: single; b: single): single;
  begin
  if (a > b) then result := a else result := b;
  end;

function intMin(a: integer; b: integer): integer;
  begin
  if (a < b) then result := a else result := b;
  end;

function intMax(a: integer; b: integer): integer;
  begin
  if (a > b) then result := a else result := b;
  end;

{ RE-RAISES EXCEPTION }
procedure addQuantity(var value: single; changeAmount: single);
  var
    oldValue: single;
  begin
  oldValue := value;
{  if (changeAmount > -10e-30) and (changeAmount < 10e-30) then exit; }
  try
    value := value + changeAmount;
  except
    { deal with underflow or overflow }
    on e: exception do
    begin
    value := 0.0;
    ErrorMessage('Error on change of quantity: ' + floatToStr(oldValue) + ' plus ' + floatToStr(changeAmount)
        + ' (' + e.message + ')');
    raise;
    end;
  end;
  { deal with value going negative, which signals a problem in any quantity variable }
  if value < 0.0 then
    begin
    { if below zero but a small number (such as might come about from rounding error), don't create an exception }
    if value > -kLowestQuantityFloat then
      value := 0.0
    { PDF - added this possibility in case rounding problem comes about due to large amounts
    	using up precision on left side of decimal point}
    else if value > -kLowestQuantityFloat * abs(changeAmount) then
      value := 0.0
    else
      begin
      value := 0.0;
      raise Exception.create('Quantity below zero: ' + floatToStr(oldValue) + ' plus ' + floatToStr(changeAmount));
      end;
    end;
  end;

{ RE-RAISES EXCEPTION }
procedure subtractQuantity(var value: single; changeAmount: single);
  var
    oldValue: single;
  begin
  oldValue := value;
 { if (changeAmount > -10e-30) and (changeAmount < 10e-30) then exit; }
  try
    value := value - changeAmount;
  except
    { deal with underflow or overflow }
    on e: exception do
    begin
    value := 0.0;
    ErrorMessage('Error on change of quantity: ' + floatToStr(oldValue) + ' minus ' + floatToStr(changeAmount)
        + ' (' + e.message + ')');
    raise;
    end;
  end;
  { deal with value going negative, which signals a problem in any quantity variable }
  if value < 0.0 then
    begin
    { if below zero but a small number (such as might come about from rounding error), don't create an exception }
    if value > -kLowestQuantityFloat then
      value := 0.0
    { PDF - added this possibility in case rounding problem comes about due to large amounts
    	using up precision on left side of decimal point}
    else if value > -kLowestQuantityFloat * abs(changeAmount) then
      value := 0.0
    else
      begin
      value := 0.0;
      raise Exception.create('Quantity below zero: ' + floatToStr(oldValue) + ' minus ' + floatToStr(changeAmount));
      end; 
    end;
  end;

{ RE-RAISES EXCEPTION }
function pow(number: extended; exponent: extended): extended;
	begin
  try
  	result := safeExp(exponent * safeLn(number));
  except
    ErrorMessage('Invalid number for power function: ' + floatToStr(number) + ' to the power ' + floatToStr(exponent));
    result := 1.0;
    raise;
  end;
	end;

{ this function is needed to get around a bug in the development environment: if a negative
number is raised to a non-integer power, an exception should be raised but the environment
instead crashes. }
{ RE-RAISES EXCEPTION }
function power(number: extended; exponent: extended): extended;
  begin
  try
  if (number > 0.0) then
    result := pow(number, exponent)
  else if (number = 0.0) then
    begin
    if exponent = 0.0 then
      begin
      result := 1.0;
      raise Exception.create('Zero raised to zero power');
      end
    else
      result := 0.0;
    end
  else
    begin
    result := 0.0;
    raise Exception.create('Negative number to non-integer power: ' + floatToStr(number) + ' to ' + floatToStr(exponent));
    end;
  except
    result := errorMessage('Uncaught error in power function: ' + floatToStr(number) + ' to ' + floatToStr(exponent)); 
    raise;
  end;
  end;

{ RE-RAISES EXCEPTION }
function tan(x: extended): extended;
  begin
  try
  result := sin(x) / cos(x);
  except
    result := errorMessage('Uncaught error in tangent function: ' + floatToStr(x));
    raise;
  end;
  end;

{ RE-RAISES EXCEPTION }
function arcCos(x: extended): extended;
  begin
  { this was in the Delphi help but doesn't seem to work }
  { result := arcTan(sqrt(1.0 - sqr(x)) / x); }
  try
  result := kPi / 2.0 - arcSin(x);  { from Smalltalk/V }
  except
    result := ErrorMessage('Invalid number for arcCos: ' + floatToStr(x));
    raise;
  end;
  end;

{ RE-RAISES EXCEPTION }
function arcSin(x: extended): extended;
  begin
  try
  result := arcTan(x / sqrt(1.0 - sqr(x)));
  except
    result := ErrorMessage('Invalid number for arcSin: ' + floatToStr(x));
    raise;
  end;
  end;

const
  oneOverNaturalLogOfTen: extended = 0.434294481;

{ RE-RAISES EXCEPTION }
{ uses notion that logN(x) = ln(x) / ln(N) }
function log10(x: extended): extended;
  begin
  try
	if x > 0.0 then
		result := oneOverNaturalLogOfTen * safeLn(x)
  else
    begin
    result := -1;
    raise Exception.create('Tried to take log (base 10) of zero or negative number');
    end;
  except
    result := ErrorMessage('Invalid number for log (base 10): ' + floatToStr(x));
    raise;
  end;
  end;

{ RE-RAISES EXCEPTION }
function safeLn(x: extended): extended;
  begin
  if x > 0.0 then
    try
    result := ln(x)
  	except
    	ErrorMessage('Invalid number for natural log: ' + floatToStr(x));
    	result := 1.0;
      raise;
  	end
  else
    begin
    raise Exception.create('Tried to take ln of zero or negative number: ' + floatToStr(x));
    end;
  end;

{ same as safeLn but does not show an errorMessage or re-raise the exception; just returns whether it failed;
  used inside paint methods where you don't want error dialogs popping up }
function safeLnWithResult(x: extended; var failed: boolean): extended;
  var temp: extended;
  begin
  failed := true;
  result := 0.0;
  temp := 0.0;
  if x <= 0.0 then exit;
  try
    temp := ln(x);
  except
    result := 1.0;
    exit;
  end;
  result := temp;
  failed := false;
  end;

{ RE-RAISES EXCEPTION }
function safeExp(x: extended): extended;
  begin
  try
    result := exp(x)
  except
    result := ErrorMessage('Invalid number for exponential function: ' + floatToStr(x));
    raise;
  end;
  end;

{ same as safeExp but does not show an errorMessage or re-raise the exception; just returns whether it failed;
  used inside paint methods where you don't want error dialogs popping up }
function safeExpWithResult(x: extended; var failed: boolean): extended;
  var temp: extended;
  begin
  failed := true;
  result := 0.0;
  temp := 0.0;
  try
    temp := exp(x);
  except
    exit;
  end;
  result := temp;
  failed := false;
  end;

{this function catches a divide by zero before it produces a not-a-number.
this can happen because of an underflow, an error in the code, or an unexpected condition.
this is a different approach than EPIC's which is basically to divide by the number plus a tiny float
for all cases (like 1e-20 or 1e-10)}
{ RE-RAISES EXCEPTION }
function safediv(x: single; y: single): single;
  begin
  try
  if (y <> 0.0) then
    result := x / y
  else
    begin
    result := 0.0;
    raise Exception.create('Divide by zero');
    end;
  except
    result := ErrorMessage('Problem in dividing ' + floatToStr(x) + ' by ' + floatToStr(y));
    raise;
  end;
  end;

{ RE-RAISES EXCEPTION }
{ same as safediv except that the user specifies what to return if the denominator is zero }
function safedivExcept(x: single; y: single; exceptionResult: single): single;
  begin
  try
  if (y <> 0.0) then
    result := x / y
  else
    result := exceptionResult;
  except
    result := ErrorMessage('Problem in dividing ' + floatToStr(x) + ' by ' + floatToStr(y));
    raise;
  end;
  end;

{ same as safediv but does not show an errorMessage or re-raise the exception; just returns whether it failed;
  used inside paint methods where you don't want error dialogs popping up }
function safedivWithResult(x: single; y: single; var failed: boolean): single;
  var temp: extended;
  begin
  failed := true;
  result := 0.0;
  temp := 0.0;
  if y = 0.0 then exit;
  try
    temp := x / y;
  except
    exit;
  end;
  result := temp;
  failed := false;
  end;

{ RE-RAISES EXCEPTION }
{ erfc stands for "error function" }
function erfc(xx: single): single;
  var
    x: single;
    erf: single;
  begin
  try
  x := abs(1.4142 * xx);
  erf := 1.0 - power(1.0 + 0.19684 * x + 0.115194 * sqr(x) + 0.00034 * power(x, 3.0) + 0.019527 * power(x, 4.0),  - 4.0);
  if (xx < 0) then erf :=  - erf;
    result := 1.0 - erf;
  except
    on e: Exception do
      begin
      result := errorMessage('Problem in erfc' + e.message);
      raise;
      end;
  end;
  end;

{ RE-RAISES EXCEPTION }
{ this function carries out a structure found in many EPIC equations. }
function scurve(x: extended; c1: extended; c2: extended): extended;
  var temp: extended;
  begin
  try
  temp := c1 - c2 * x;
  if temp > 85.0 then
    begin
    temp := 85.0;
    raise exception.create('Exponent of number would have gone out of float range');
    end;
  result := safediv(x, x + safeExp(temp));
  except
    result := errorMessage('Problem in s curve: numbers are ' + floatToStr(x)
        + ', ' + floatToStr(c1) + ', and ' + floatToStr(c2));
    raise;
  end;
  end;

{ same as scurve but does not show an errorMessage or re-raise the exception; just returns whether it failed;
  used inside paint methods where you don't want error dialogs popping up }
function scurveWithResult(x: extended; c1: extended; c2: extended; var failed: boolean): extended;
  var temp: extended;
  begin
  failed := true;
  result := 0.0;
  temp := 0.0;
  try
    temp := c1 - c2 * x;
    if temp > 85.0 then exit;
    temp := safeExpWithResult(temp, failed);
    if failed then exit;
    temp := safedivWithResult(x, x + temp, failed);
    if failed then exit;
  except
    exit;
  end;
  failed := false;
  result := temp;
  end;

{ s curve params }
procedure Utils_InitSCurveParam(var sCurve: SCurveStructure; x1, y1, x2, y2: single);
  begin
  if (x1 = 0) or (y1 = 0) or (x2 = 0) or (y2 = 0) then
    raise Exception.create('Zero value for s curve not allowed')
  else
    begin
    sCurve.x1 := x1;
    sCurve.y1 := y1;
    sCurve.x2 := x2;
    sCurve.y2 := y2;
    Utils_CalcSCurveCoeffs(sCurve);
    end;
  end;

{ RE-RAISES EXCEPTION }
procedure Utils_CalcSCurveCoeffs(var sCurve: SCurveStructure);
  var xx: extended;
  begin
  try
  if (sCurve.x1 = 0) or (sCurve.y1 = 0) or (sCurve.x2 = 0) or (sCurve.y2 = 0) then
    raise Exception.create('Zero value for s curve not allowed')
  else
    with sCurve do
      begin
      xx := safeLn(safediv(x1, y1) - x1);
      c2 := safediv((xx - safeLn(safediv(x2, y2) - x2)), x2 - x1);
      c1 := xx + x1 * c2;
      end;
  except
    on e: Exception do
      begin
      errorMessage('Problem in Utils_CalcSCurveCoeffs: ' + e.message);
      raise;
      end;
  end;
  end;

{ same as Utils_CalcSCurveCoeffs but does not show an errorMessage or re-raise the exception;
  just returns whether it failed; used inside paint methods where you don't want error dialogs popping up }
procedure Utils_CalcSCurveCoeffsWithResult(var sCurve: SCurveStructure; var failed: boolean);
  var
    xx, quotientX1Y1, lnQuotientX1Y1MinusX1, quotientX2Y2, lnQuotientX2Y2MinusX2, c1temp, c2temp: extended;
  begin
  failed := true;
  try
  with sCurve do
    begin
    quotientX1Y1 := safedivWithResult(x1, y1, failed);
    if failed then exit;
    lnQuotientX1Y1MinusX1 := safeLnWithResult(quotientX1Y1 - x1, failed);
    if failed then exit;
    quotientX2Y2 := safedivWithResult(x2, y2, failed);
    if failed then exit;
    lnQuotientX2Y2MinusX2 := safeLnWithResult(quotientX2Y2 - x2, failed);
    if failed then exit;
    c2temp := safedivWithResult(lnQuotientX1Y1MinusX1 - lnQuotientX2Y2MinusX2, x2 - x1, failed);
    if failed then exit;
    c1temp := lnQuotientX1Y1MinusX1 + x1 * c2temp;
    end;
  except
    exit;
  end;
  sCurve.c1 := c1temp;
  sCurve.c2 := c2temp;
  failed := false;
  end;

procedure Utils_GetSetSCurveValue(direction: integer; var param: sCurveStructure; index: integer; var value);
  begin
  if (direction = kGetField) then
    begin
    case index of
      0: single(value) := param.x1;
      1: single(value) := param.y1;
      2: single(value) := param.x2;
      3: single(value) := param.y2;
      end;
    end
  else
    begin
    { direction == kSetField }
    case index of
      0: param.x1 := single(value);
      1: param.y1 := single(value);
      2: param.x2 := single(value);
      3: param.y2 := single(value);
    end;
  end;
end;

{ ---------------------------------------------------------------------------------------------- distributions }
function Utils_RandomZeroToOne: single;
  var k: longint;
  begin
  { RAMAS/age uniform random number generator:
    x := (7e5 * x)(mod 2e31 - 1)
    gRandomSeed = (16807 * gRandomSeed) % (2147483648 - 1); }
  k := gRandomSeed div 127773;
  gRandomSeed := 16807 * (gRandomSeed - k * 127773) - k * 2836;
  if (gRandomSeed < 0) then gRandomSeed := gRandomSeed + 2147483647;
  result := gRandomSeed * 0.0000000004656612875; { 4.656612875d-10 }
  end;

function Utils_StandardNormalDeviate(one: single; two: single): single;
  begin
  { EPIC code:  ADSTN=SQRT(-2.*ALOG(RN1))*COS(6.283185*RN2) }
  result := sqrt(-2.0 * safeLn(one)) * cos(2.0 * kPi * two);
  end;

{ this was for testing against the polar coordinate normal function - not using }
function Utils_NormalByAdding: single;
  var
    i: integer;
    total: single;
  begin
  total := 0.0;
  for i := 0 to 11 do total := total + Utils_RandomZeroToOne;
  {result := ((total - 6.0) + 0.5) * 1.5;}
  result := total / 12.0;
  end;

function Utils_TriangularDistribution(lowerXLimit: single; xAtPeakOfDist: single; upperXLimit: single): single;
  var
    upSide, downSide, propXAtPeakIsOfRange, randomNumber, randomNumberTHalfOfRange: single;
  begin
  { upSide is the part of the distribution before the peak. downSide is the part after the peak }
  upSide := xAtPeakOfDist - lowerXLimit;
  downSide := upperXLimit - xAtPeakOfDist;
  propXAtPeakIsOfRange := upSide / (upperXLimit - lowerXLimit);
  randomNumber := Utils_RandomZeroToOne;
  randomNumberTHalfOfRange := randomNumber * (upperXLimit - lowerXLimit) / 2.0;
  if (randomNumber > propXAtPeakIsOfRange) then { randomNumber is on downSide }
    result := upperXLimit - sqrt(sqr(downSide) - 2.0 * downSide * (randomNumberTHalfOfRange - 0.5 * upSide))
  else
    result := sqrt(2.0 * randomNumberTHalfOfRange * upSide) + lowerXLimit;
  end;

{ --------------------------------------------------------------------------------- defaults and ranges for inputs }
procedure Utils_DefaultFloatIfZero(var value: single; theDefault: single);
  begin
  if (value < kLowestFloatAboveZero) then value := theDefault;
  end;

procedure Utils_DefaultIntIfZero(var value: smallint; theDefault: smallint);
  begin
  if (value = 0) then value := theDefault;
  end;

procedure Utils_EnforceFloatRange(var value: single; theMin: single; theMax: single);
  begin
  if (value < theMin) then value := theMin;
  if (value > theMax) then value := theMax;
  end;

procedure Utils_EnforceIntegerRange(var value: smallint; theMin: smallint; theMax: smallint);
  begin
  if (value < theMin) then value := theMin;
  if (value > theMax) then value := theMax;
  end;

function Utils_AddDayTo30DaysArray_mm(var theArray: arrayThirty; theValue: single): single;
  var
    day: integer;
    total: single;
  begin
  total := 0.0;
  for day := 0 to 28 do
    begin
    theArray[day] := theArray[day + 1];
    total := total + (theArray[day]);
    end;
  theArray[29] := theValue;
  total := total + (theValue);
  result := total;
  end;

{ ---------------------------------------------------------------------------------------------- setting arrays }
procedure Utils_SetMonthValues(var theArray: arrayMonths; x1: single; x2: single; x3: single; x4: single; x5:
  single; x6: single; x7: single; x8: single; x9: single; x10: single; x11: single; x12: single);
  begin
  theArray[0] := x1;
  theArray[1] := x2;
  theArray[2] := x3;
  theArray[3] := x4;
  theArray[4] := x5;
  theArray[5] := x6;
  theArray[6] := x7;
  theArray[7] := x8;
  theArray[8] := x9;
  theArray[9] := x10;
  theArray[10] := x11;
  theArray[11] := x12;
  end;

procedure Utils_SetLayerValues(var theArray: arraySoilLayers; x1: single; x2: single; x3: single; x4: single; x5: single; x6:
  single; x7: single; x8: single; x9: single; x10: single);
  begin
  theArray[0] := x1;
  theArray[1] := x2;
  theArray[2] := x3;
  theArray[3] := x4;
  theArray[4] := x5;
  theArray[5] := x6;
  theArray[6] := x7;
  theArray[7] := x8;
  theArray[8] := x9;
  theArray[9] := x10;
  end;

procedure Utils_SetWindDirectionValues(var theArray: windDirectionsArray; direction: integer; x1: single; x2: single; x3:
  single; x4: single; x5: single; x6: single; x7: single; x8: single; x9: single; x10: single; x11: single; x12: single);
  begin
  theArray[0][direction] := x1;
  theArray[1][direction] := x2;
  theArray[2][direction] := x3;
  theArray[3][direction] := x4;
  theArray[4][direction] := x5;
  theArray[5][direction] := x6;
  theArray[6][direction] := x7;
  theArray[7][direction] := x8;
  theArray[8][direction] := x9;
  theArray[9][direction] := x10;
  theArray[10][direction] := x11;
  theArray[11][direction] := x12;
  end;

{ -------------------------------------------------------------------------------------- unit conversion functions }
function Utils_DegreesToRadians(degrees: single): single;
  begin
  result := 2.0 * kPi * degrees / 360.0;
  end;

function Utils_RadiansToDegrees(radians: single): single;
  begin
  result := (radians / (2.0 * kPi)) * 360.0;
  end;

procedure Utils_ConvertRadiansAndDegrees(direction: integer; var radians: single; var degrees);
  begin
  try
  if (direction = kSetField) then
    radians := Utils_DegreesToRadians(single(degrees))
  else
    single(degrees) := Utils_RadiansToDegrees(radians);
  except
    on e: Exception do errorMessage('Problem in Utils_ConvertRadiansAndDegrees: ' + e.message);
  end;
  end;

function Utils_SoilWeightFromBulkDensityAndThickness_tPha(bulkDensity_tPm3: single; thickness_m: single): single;
  var
    weight_tPm2: single;
  begin
  try
    weight_tPm2 := bulkDensity_tPm3 * thickness_m;
    result := weight_tPm2 / m2_to_ha;
  except
    on e: Exception do result := errorMessage('Problem in Utils_SoilWeightFromBulkDensityAndThickness_tPha: ' + e.message);
  end;
  end;

function Utils_BulkDensityFromSoilWeightAndThickness_tPM3(weight_tPha: single; thickness_m: single): single;
  var
    weight_tPm2: single;
  begin
  try
    weight_tPm2 := weight_tPha / ha_to_m2;
    result := safediv(weight_tPm2, thickness_m);
  except
    on e: Exception do result := errorMessage('Problem in Utils_SettledBulkDensityFromSoilWeight_tPM3: ' + e.message);
  end;
  end;

function Utils_SoilThicknessFromWeightAndBulkDensity_m(weight_tPha: single; bulkDensity_tPm3: single): single;
  var
    weight_tPm2: single;
  begin
  try
    weight_tPm2 := weight_tPha / ha_to_m2;
    result := safediv(weight_tPm2, bulkDensity_tPm3);
  except
    on e: Exception do result := errorMessage('Problem in Utils_SoilThicknessFromWeightAndBulkDensity_m: ' + e.message);
  end;
  end;

function Utils_OrganicMatterFromOrganicC_tPha(organicC_pct: integer; soilWeight_tPha: single): single;
  begin
  try
    { a ratio  of about 1:1.7 can be assumed to exist between the organic carbon and the soil humus }
    result := organicC_pct * pct_to_frn * 1.72 * soilWeight_tPha;
  except
    on e: Exception do result := errorMessage('Problem in Utils_OrganicMatterFromOrganicC_tPha: ' + e.message);
  end;
  end;

function Utils_OrganicCFromOrganicMatter_pct(organicMatter_tPha: single; soilWeight_tPha: single): single;
  begin
  try
    { a ratio  of about 1:1.7 can be assumed to exist between the organic carbon and the soil humus }
    result := min(100.0, max(0.0, safediv(organicMatter_tPha / 1.72, soilWeight_tPha) * frn_to_pct));
  except
    on e: Exception do result := errorMessage('Problem in Utils_OrganicCFromOrganicMatter_pct: ' + e.message); 
  end;
  end;

function Utils_PorosityFromBulkDensityRockContentAndThickness(bulkDensity_tPm3, rockContent_pct, thickness_m: single): single;
  var
    porosity_mPm, nonRockContent_frn: single;
  begin
  try
    { 2.65 is assumed for particle density }
    porosity_mPm := 1.0 - bulkDensity_tPm3 / 2.65;
    nonRockContent_frn := 1.0 - rockContent_pct * pct_to_frn;
    result := porosity_mPm * nonRockContent_frn * thickness_m * m_to_mm;
  except
    on e: Exception do
        result := errorMessage('Problem in Utils_PorosityFromBulkDensityRockContentAndThickness: ' + e.message);
  end;
  end;

function Utils_BulkDensityFromPorosityRockContentAndThickness(porosity_mm, rockContent_pct, thickness_m: single): single;
  var
    porosity_mPm, nonRockContent_frn: single;
  begin
  try
    nonRockContent_frn := 1.0 - rockContent_pct * pct_to_frn;
    porosity_mPm := safedivExcept(porosity_mm, nonRockContent_frn * thickness_m * m_to_mm, 0.0);
    { 2.65 is assumed for particle density }
    result := (1.0 - porosity_mPm) * 2.65;
  except
    on e: Exception do
        result := errorMessage('Problem in Utils_BulkDensityFromPorosityRockContentAndThickness: ' + e.message);
  end;
  end;

function Utils_MulchDepthFromMulchFlatCropResidue_m(flatCropResidue_tPha: single): single;
  var
    bulkDensity_tPm3: single;
  begin
  result := 0.0;
  try
    { use constant for mulch bulk density of 1.0 g/cm3, average for 'loose soil' (Troeh & Thompson 1993) }
    { g/cm3 = t/m3 }
    bulkDensity_tPm3 := 1.0;
    result := Utils_SoilThicknessFromWeightAndBulkDensity_m(flatCropResidue_tPha, bulkDensity_tPm3);
  except
    on e: Exception do result := errorMessage('Problem in Utils_MulchDepthFromMulchFlatCropResidue_m: ' + e.message);
  end;
  end;

function Utils_MulchFlatCropResidueFromMulchDepth_tPha(mulchDepth_m: single): single;
  var bulkDensity_tPm3: single;
  begin
  result := 0.0;
  try
    { use constant for mulch bulk density of 1.0 g/cm3, average for 'loose soil' (Troeh & Thompson 1993) }
    { g/cm3 = t/m3 }
    bulkDensity_tPm3 := 1.0;
    result := Utils_SoilWeightFromBulkDensityAndThickness_tPha(bulkDensity_tPm3, mulchDepth_m);
  except on e: Exception do
    result := errorMessage('Problem in soil mulchFlatCropResidueFromMulchDepth_tPha: ' + e.message);
  end;
  end;

{ ---------------------------------------------------------------------------------------------- specific functions }
function Utils_SaturVaporPressureAtMeanTemp(temp_degK: single): single;
  begin
  try
    result := 0.1 * safeExp(54.879 - 5.029 * safeLn(temp_degK) - safediv(6790.5, temp_degK));
  except
    on e: Exception do result := errorMessage('Problem in Utils_SaturVaporPressureAtMeanTemp: ' + e.message);
  end;
  end;

function Utils_EstimateMaterialForLayer(layer: integer; materialThisLayer: single; materialUpperLayer: single;
  thicknessThisLayer_mm: single; thicknessUpperLayer_mm: single): single;
  begin
  try
  if (materialUpperLayer = 0.0) then
    result := materialThisLayer
  else { materialUpperLayer <> 0 }
    begin
    if (materialThisLayer = 0.0) then
      begin
      if (layer > 1) then
        result := materialUpperLayer * safeExp(-0.01 * thicknessThisLayer_mm)
            * safediv(thicknessThisLayer_mm, thicknessUpperLayer_mm)
      else
        result := 1.0;
      end
    else { materialThisLayer <> 0 }
      result := materialThisLayer;
    end;
  except
    on e: Exception do result := errorMessage('Problem in Utils_EstimateMaterialForLayer: ' + e.message);
  end;
  end;

function Utils_IntegrateModifiedExponentialEquation(exponent: single): single;
  var
    doAgain, halveInterval: boolean;
    integrationInterval_frn, fractionAtIntervalStart_frn, fractionAtIntervalEnd_frn,
      valueAtIntervalStart, valueAtIntervalEnd, meanValueForInterval: single;
  begin
  try
  result := 0.0;
  fractionAtIntervalStart_frn := 1.0;
  integrationInterval_frn := 0.1;
  valueAtIntervalStart := 0.0;
  doAgain := true;
  halveInterval := false;
  while doAgain do
    begin
    fractionAtIntervalEnd_frn := fractionAtIntervalStart_frn - integrationInterval_frn;
    if fractionAtIntervalEnd_frn > 0.0 then
      begin
      valueAtIntervalEnd := power(-safeLn(fractionAtIntervalEnd_frn), exponent);
      meanValueForInterval := (valueAtIntervalEnd + valueAtIntervalStart) * integrationInterval_frn;
      result := result + meanValueForInterval;
      fractionAtIntervalStart_frn := fractionAtIntervalEnd_frn;
      valueAtIntervalStart := valueAtIntervalEnd;
      if meanValueForInterval < 0.1 then
        begin
        halveInterval := false;
        doAgain := true;
        end
      else
        begin
        halveInterval := true;
        end;
      end
    else
      begin
      halveInterval := true;
      end;
    if (halveInterval) then
      begin
      integrationInterval_frn := integrationInterval_frn * 0.5;
      { notation: 1.e-4 }
      if (integrationInterval_frn > 0.0001) then 
        doAgain := true
      else
        doAgain := false;
      end;
    end;
  result := result * 0.5;
  except
    on e: Exception do result := errorMessage('Problem in Utils_IntegrateModifiedExponentialEquation: ' + e.message); 
  end;
  end;

{ ---------------------------------------------------------------------------------------------- layer functions
  these layer functions are here because they are used in places that don't have a soil patch pointer
  and because the soil patch file was too big to compile }
procedure Utils_TransferPropOfMaterial(var amountFrom: single; var amountTo: single; proportion: single);
  var
    transferAmount: single;
  begin
  { this function assumes that neither of the transferring quantities should ever be less than zero }
  transferAmount := proportion * amountFrom;
  subtractQuantity(amountFrom, transferAmount);
  addQuantity(amountTo, transferAmount);
  end;

procedure Utils_TransferPropOfNutrients(var layerFrom: LayerStructure; var layerTo: LayerStructure; proportion: single);
  begin
  { n }
  Utils_TransferPropOfMaterial(layerFrom.nitrate_kgPha, layerTo.nitrate_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.ammonia_kgPha, layerTo.ammonia_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.organicNFresh_kgPha, layerTo.organicNFresh_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.organicNStableHumus_kgPha, layerTo.organicNStableHumus_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.organicNActiveHumus_kgPha, layerTo.organicNActiveHumus_kgPha, proportion);
  { p }
  Utils_TransferPropOfMaterial(layerFrom.labileP_kgPha, layerTo.labileP_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.organicPFresh_kgPha, layerTo.organicPFresh_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.organicPHumus_kgPha, layerTo.organicPHumus_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.mineralPActive_kgPha, layerTo.mineralPActive_kgPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.mineralPStable_kgPha, layerTo.mineralPStable_kgPha, proportion);
  { organic matter }
  Utils_TransferPropOfMaterial(layerFrom.flatCropResidue_tPha, layerTo.flatCropResidue_tPha, proportion);
  Utils_TransferPropOfMaterial(layerFrom.organicMatter_tPha, layerTo.organicMatter_tPha, proportion);
  end;

procedure Utils_AddPropOfMaterial(var amountFrom: single; var amountTo: single; proportion: single);
  begin
  amountTo := amountTo + proportion * amountFrom;
  end;

procedure Utils_AddPropOfNutrients(var layerFrom: LayerStructure; var layerTo: LayerStructure; proportion: single);
  begin
  { n }
  Utils_AddPropOfMaterial(layerFrom.nitrate_kgPha, layerTo.nitrate_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.ammonia_kgPha, layerTo.ammonia_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.organicNFresh_kgPha, layerTo.organicNFresh_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.organicNStableHumus_kgPha, layerTo.organicNStableHumus_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.organicNActiveHumus_kgPha, layerTo.organicNActiveHumus_kgPha, proportion);
  { p }
  Utils_AddPropOfMaterial(layerFrom.labileP_kgPha, layerTo.labileP_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.organicPFresh_kgPha, layerTo.organicPFresh_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.organicPHumus_kgPha, layerTo.organicPHumus_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.mineralPActive_kgPha, layerTo.mineralPActive_kgPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.mineralPStable_kgPha, layerTo.mineralPStable_kgPha, proportion);
  { organic matter }
  Utils_AddPropOfMaterial(layerFrom.flatCropResidue_tPha, layerTo.flatCropResidue_tPha, proportion);
  Utils_AddPropOfMaterial(layerFrom.organicMatter_tPha, layerTo.organicMatter_tPha, proportion);
  end;

procedure Utils_CheckRelationOfFieldCapacityToPorosity(var layer: LayerStructure);
  var
    { Field capacity should never get above 90% of porosity, so if that happens, this check }
    { sets things right. Wilting point is also adjusted on the basis of two things: }
    { the previous difference between it and field capacity, and if that is less than zero, }
    { it is set at 1% of field capacity. }
    oldFCmWP_mm: single;
  begin
  if (layer.fieldCapacity_mm >= 0.9 * layer.porosity_mm) then
    begin
    oldFCmWP_mm := layer.fieldCapacity_mm - layer.wiltingPoint_mm;
    layer.fieldCapacity_mm := 0.9 * layer.porosity_mm;
    layer.wiltingPoint_mm := layer.fieldCapacity_mm - oldFCmWP_mm;
    if (layer.wiltingPoint_mm <= 0.0) then
      layer.wiltingPoint_mm := 0.01 * layer.fieldCapacity_mm;
    end;
  end;

procedure Utils_MovePortionOfLayerToEmptyLayer(var layerFrom: LayerStructure; var newLayer: LayerStructure;
    proportion, newDepth_m, depthLayerAbove_m: single);
  begin
  { assume newLayer fields have been zeroed before this function called }
  newLayer.pSorptionCoeff_frn := layerFrom.pSorptionCoeff_frn;
  newLayer.settledBulkDensity_tPm3 := layerFrom.settledBulkDensity_tPm3;
  newLayer.bulkDensityOvenDry_tPm3 := layerFrom.bulkDensityOvenDry_tPm3;
  newLayer.bulkDensity_tPm3 := layerFrom.bulkDensity_tPm3;
  newLayer.clayContent_pct := layerFrom.clayContent_pct;
  newLayer.siltContent_pct := layerFrom.siltContent_pct;
  newLayer.sandContent_pct := layerFrom.sandContent_pct;
  { extra check for sand silt clay }
  newLayer.sandContent_pct := max(0.0, 100.0 - newLayer.clayContent_pct - newLayer.siltContent_pct);
  newLayer.rockContent_pct := layerFrom.rockContent_pct;
  newLayer.saturatedConductivity_mmPhr := layerFrom.saturatedConductivity_mmPhr;
  newLayer.travelTimeFactor := layerFrom.travelTimeFactor;
  newLayer.soilpH := layerFrom.soilpH;
  newLayer.temperature_degC := layerFrom.temperature_degC;
  newLayer.mineralPFlowCoeff_Pday := layerFrom.mineralPFlowCoeff_Pday;
  newLayer.baseFormingCations_cmolPkg := layerFrom.baseFormingCations_cmolPkg;
  newLayer.cationExchangeCapacity_cmolPkg := layerFrom.cationExchangeCapacity_cmolPkg;
  newLayer.calciumCarbonate_pct := layerFrom.calciumCarbonate_pct;
  newLayer.aluminumSaturation_pct := layerFrom.aluminumSaturation_pct;
  newLayer.organicNActiveHumusFractionAtInput_frn := layerFrom.organicNActiveHumusFractionAtInput_frn;
  Utils_TransferPropOfMaterial(layerFrom.weight_tPha, newLayer.weight_tPha, proportion);
  Utils_TransferPropOfNutrients(layerFrom, newLayer, proportion);
  { because water constants (and water content) are stored as an absolute amount
    (mm, not m/m), they must be adjusted when the thickness of the layer changes. }
  Utils_TransferPropOfMaterial(layerFrom.waterContent_mm, newLayer.waterContent_mm, proportion);
  Utils_TransferPropOfMaterial(layerFrom.wiltingPoint_mm, newLayer.wiltingPoint_mm, proportion);
  Utils_TransferPropOfMaterial(layerFrom.fieldCapacity_mm, newLayer.fieldCapacity_mm, proportion);
  newLayer.depth_m := newDepth_m;
  { calculation of porosity must be after setting layer depth, bulk density, and rock content }
  newLayer.porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
      newLayer.bulkDensity_tPm3, newLayer.rockContent_pct, max(0.0, newDepth_m - depthLayerAbove_m));
  Utils_CheckRelationOfFieldCapacityToPorosity(newLayer);
  end;

procedure Utils_MovePortionOfLayerToAnotherLayer(var layerFrom: LayerStructure;
    var layerTo: LayerStructure; thicknessLayerFrom_m, thicknessLayerTo_m,
    propOfLayerFromMoving, propOfLayerToThatWasThereBefore, slopeSteepness_mPm: single;
    layerToIsSurfaceLayer: boolean);
  var
    newThicknessLayerTo_m, newThicknessLayerFrom_m, oldBulkDensity_tPm3: single;
    propOfLayerToThatWasntThereBefore: single;
  begin
  try
  newThicknessLayerTo_m := thicknessLayerTo_m + propOfLayerFromMoving * thicknessLayerFrom_m;
  newThicknessLayerFrom_m :=  thicknessLayerFrom_m * (1.0 - propOfLayerFromMoving);
  Utils_TransferPropOfNutrients(layerFrom, layerTo, propOfLayerFromMoving);
  { because water constants (and water content) are stored as an absolute amount
    (mm, not m/m), they must be adjusted when the thickness of the layer changes. }
  Utils_TransferPropOfMaterial(layerFrom.waterContent_mm, layerTo.waterContent_mm, propOfLayerFromMoving);
  Utils_TransferPropOfMaterial(layerFrom.fieldCapacity_mm, layerTo.fieldCapacity_mm, propOfLayerFromMoving);
  Utils_TransferPropOfMaterial(layerFrom.wiltingPoint_mm, layerTo.wiltingPoint_mm, propOfLayerFromMoving);
  { recalculate weight and bulk density because of movement }
  if (layerToIsSurfaceLayer) then
    begin
    layerFrom.weight_tPha := layerFrom.weight_tPha * (1.0 - propOfLayerFromMoving);
    layerTo.weight_tPha := Utils_SoilWeightFromBulkDensityAndThickness_tPha(
        layerTo.bulkDensity_tPm3, thicknessLayerTo_m); {because thickness of surface layer never changes}
    { bulk density is not recalculated for the top soil layer because since thickness stays the
      same and weight is recalculated from bulk density, the bulk density remains the same }
    end
  else
    begin
    Utils_TransferPropOfMaterial(layerFrom.weight_tPha, layerTo.weight_tPha, propOfLayerFromMoving);
    oldBulkDensity_tPm3 := layerTo.bulkDensity_tPm3;
    layerTo.bulkDensity_tPm3 := Utils_BulkDensityFromSoilWeightAndThickness_tPM3(
        layerTo.weight_tPha, newThicknessLayerTo_m);
    { change settled bulk density by same proportion }
    if layerTo.bulkDensity_tPm3 <> oldBulkDensity_tPm3 then
      layerTo.settledBulkDensity_tPm3 := layerTo.settledBulkDensity_tPm3
          * safediv(layerTo.bulkDensity_tPm3, oldBulkDensity_tPm3);
    end;
  layerTo.porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(layerTo.bulkDensity_tPm3,
    layerTo.rockContent_pct, newThicknessLayerTo_m);
  Utils_CheckRelationOfFieldCapacityToPorosity(layerTo);
  layerFrom.porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(layerFrom.bulkDensity_tPm3,
    layerFrom.rockContent_pct, newThicknessLayerFrom_m);
  Utils_CheckRelationOfFieldCapacityToPorosity(layerFrom);
  { properties }
  propOfLayerToThatWasntThereBefore := 1.0 - propOfLayerToThatWasThereBefore;
    {
      before:
      mmmmmmmmmmmmmmmm        mmmmmmmmmmmmmmmmmmmmmmmmmmm
      ---layerFrom----        --------layerTo------------

      during:
      mmmmmmmmmmmmm MMM -->   mmmmmmmmmmmmmmmmmmmmmmmmmmm
      ---layerFrom----        --------layerTo------------

      after:
      mmmmmmmmmmmmm        MMMmmmmmmmmmmmmmmmmmmmmmmmmmmm
      --layerFrom--        ----------layerTo-------------

      MMM / (layerFrom + MMM) = propOfLayerFromRemoved
      (layerTo - MMM) / layerTo = propOfLayerToThatWasThereBefore
      MMM / layerTo = propOfLayerToThatWasntThereBefore
       }
  { don't have to change these properties for layerFrom }
  { change layerTo properties to layerFrom property * what was there before over what is there now }
  { + layerTo property * what is new there over what is there now }
  layerTo.pSorptionCoeff_frn := layerFrom.pSorptionCoeff_frn * propOfLayerToThatWasntThereBefore
      + layerTo.pSorptionCoeff_frn * propOfLayerToThatWasThereBefore;
  layerTo.saturatedConductivity_mmPhr := layerFrom.saturatedConductivity_mmPhr * propOfLayerToThatWasntThereBefore
      + layerTo.saturatedConductivity_mmPhr * propOfLayerToThatWasThereBefore;
  layerTo.soilpH := layerFrom.soilpH * propOfLayerToThatWasntThereBefore
      + layerTo.soilpH * propOfLayerToThatWasThereBefore;
  layerTo.clayContent_pct := layerFrom.clayContent_pct * propOfLayerToThatWasntThereBefore
      + layerTo.clayContent_pct * propOfLayerToThatWasThereBefore;
  layerTo.siltContent_pct := layerFrom.siltContent_pct * propOfLayerToThatWasntThereBefore
      + layerTo.siltContent_pct * propOfLayerToThatWasThereBefore;
  layerTo.rockContent_pct := layerFrom.rockContent_pct * propOfLayerToThatWasntThereBefore
      + layerTo.rockContent_pct * propOfLayerToThatWasThereBefore;
  { calculate from other properties }
  layerTo.sandContent_pct := 100.0 - layerTo.clayContent_pct - layerTo.siltContent_pct;
  layerTo.travelTimeFactor := layerTo.saturatedConductivity_mmPhr * slopeSteepness_mPm;
  except
    on e: Exception do errorMessage('Problem in Utils_MovePortionOfLayerQuantitiesToAnotherLayer' + e.message); 
  end;
  end;

procedure Utils_ZeroAllLayerFields(var layer: LayerStructure);
  begin
  layer.depth_m := 0.0;
  layer.temperature_degC := 0.0;
  layer.weight_tPha := 0.0;
  layer.waterContent_mm := 0.0;
  layer.fieldCapacity_mm := 0.0;
  layer.wiltingPoint_mm := 0.0;
  layer.porosity_mm := 0.0;
  layer.lateralFlow_mm := 0.0;
  layer.percolation_mm := 0.0;
  layer.evaporation_mm := 0.0;
  layer.travelTimeFactor := 0.0;
  layer.nitrate_kgPha := 0.0;
  layer.ammonia_kgPha := 0.0;
  layer.organicNFresh_kgPha := 0.0;
  layer.organicNActiveHumus_kgPha := 0.0;
  layer.organicNStableHumus_kgPha := 0.0;
  layer.organicNActiveHumusFractionAtInput_frn := 0.0;
  layer.decayRateConst := 0.0;
  layer.nutrientCyclingTempFactor := 0.0;
  layer.organicNFromActiveToStableInHumus_kgPha := 0.0;
  layer.nitrateLeachedFromPercolation_kgPha := 0.0;
  layer.nitrateLeachedFromLateralFlow_kgPha := 0.0;
  layer.nitrification_kgPha := 0.0;
  layer.volatilization_kgPha := 0.0;
  layer.activeHumusNMineralization_kgPha := 0.0;
  layer.freshNMineralization_kgPha := 0.0;
  layer.denitrification_kgPha := 0.0;
  layer.labileP_kgPha := 0.0;
  layer.organicPFresh_kgPha := 0.0;
  layer.organicPHumus_kgPha := 0.0;
  layer.mineralPActive_kgPha := 0.0;
  layer.mineralPStable_kgPha := 0.0;
  layer.pSorptionCoeff_frn := 0.0;
  layer.mineralPFlowCoeff_Pday := 0.0;
  layer.pFlowFromLabileToActiveMineral_kgPha := 0.0;
  layer.mineralPFlowFromActiveToStable_kgPha := 0.0;
  layer.labilePLeachedFromPercolation_kgPha := 0.0;
  layer.activeHumusPMineralization_kgPha := 0.0;
  layer.freshPMineralization_kgPha := 0.0;
  layer.soilpH := 0.0;
  layer.aluminumSaturation_pct := 0.0;
  layer.baseFormingCations_cmolPkg := 0.0;
  layer.bulkDensity_tPm3 := 0.0;
  layer.settledBulkDensity_tPm3 := 0.0;
  layer.bulkDensityOvenDry_tPm3 := 0.0;
  layer.cationExchangeCapacity_cmolPkg := 0.0;
  layer.saturatedConductivity_mmPhr := 0.0;
  layer.flatCropResidue_tPha := 0.0;
  layer.organicMatter_tPha := 0.0;
  layer.calciumCarbonate_pct := 0.0;
  layer.clayContent_pct := 0.0;
  layer.sandContent_pct := 0.0;
  layer.siltContent_pct := 0.0;
  layer.rockContent_pct := 0.0;
  layer.patchTotalRootWeight_tPha := 0.0;
  end;

  { the function that called this could be keeping track of whether the first layer below }
  { the criterion depth has been considered, (in which case this pointer will point to the }
  { flag for if the layer has been considered), or it could just break out of the loop }
  { after that first layer has been considered (in which case this pointer would be nil) }
function Utils_LayerPropAboveCriterionDepth_frn(layerDepth_m: single; depthLastLayer_m: single; criterionDepth_m: single;
  	var layerBelowCriterionDepthHasBeenConsidered: boolean): single;
  begin
  try
  if (layerDepth_m <= criterionDepth_m) then 
    result := 1.0
  else
    begin
    if (not layerBelowCriterionDepthHasBeenConsidered) then
      begin
      layerBelowCriterionDepthHasBeenConsidered := true;
      result := safediv(criterionDepth_m - depthLastLayer_m, layerDepth_m - depthLastLayer_m);
      end
  	else
    	result := 0.0;
    end;
  except
    on e: Exception do result := errorMessage('Problem in Utils_LayerPropAboveCriterionDepth_frn: ' + e.message); 
  end;
  end;

end.

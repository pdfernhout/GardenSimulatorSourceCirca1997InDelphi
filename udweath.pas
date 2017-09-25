unit udweath;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udweath: Browser pictures side component for displaying weather values for the past month.
The weather object holds on to an array of 30 structures that hold daily weather for this
display. See ubrowtri for more information on right-side browser components.}

interface

uses WinTypes, WinProcs, Controls, ExtCtrls, Graphics, Forms, Classes, ubrowcom, ueweath, uestruct, umodel, udate;

const
  kDisplayTemperature = 0;
  kDisplayPrecipitation = 1;
  kDisplayRadiation = 2;
  kDisplayWindSpeed = 3;
  kDisplayRelativeHumidity = 4;
  kDisplayDayLength = 5;

  kItemDisplayUnit = 1;

type

KfWeatherDrawComponent = class(KfBrowserComponent)
	public
  profile: TBitmap;
  weather: GsWeather;
  displayMode: integer;
  numDays: integer;
  profileRect: TRect;
  labelRect: TRect;
  labelText: string[60];
  legendColorRects: array[0..5] of TRect;
  displayMaxValue: single;
  displayMinValue: single;
  displayUnitSet: integer;
  displayUnitModel: integer;
  displayUnitCurrent: integer;
  displayUnitRect: TRect;
  drawMeans: boolean;
  rescale: boolean;
  graphBars: boolean;
  constructor create(anOwner: TComponent); override;
  destructor destroy; override;
  procedure initialize; override;
  procedure updateWeather(newWeather: GsWeather);
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateModel(newModel: GsModel); override;
  procedure updateDisplay; override;
  procedure paint; override;
  procedure resizeElements; override;
  procedure drawLegend;
  procedure drawProfile;
  procedure drawLinePointOrBar(i: integer; value: single);
  procedure drawLinePoint(i: integer; value: single);
  procedure drawBar(i: integer; value: single);
  function varDistanceForGraph(value: single): integer;
  function maxSelectedItemIndex: integer; override;
  function nextSelectedItemIndex(goForward: boolean): integer; override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  procedure collapseOrExpand(y: integer); override;
  function toModelUnit(value: single): single;
  function toCurrentUnit(value: single): single;
  procedure setDefaultOverlayUnit;
  procedure changeOverlayUnit(shift: boolean);
  procedure drawTextLabel(text: string; drawRect: TRect; drawLine, drawBox: boolean);
  function displayModeName: string;
  procedure changeLegendOrLineColor(index: integer);
  function dateStringDaysAndMonthsBeforePresent(daysBefore, monthsBefore: integer): string;
  function collapsedHeight: integer; override;
  end;

implementation

uses SysUtils, Dialogs, uunits, ueutils, ueqh, udomain, ubrowser, usupport, ugsim, ugscom,
  umconsts;

constructor KfWeatherDrawComponent.create(anOwner: TComponent);
  begin
  inherited create(anOwner);
  self.model := nil;
  self.weather := nil;
  profile := TBitmap.create;
  self.parentFont := true;
  self.onMouseUp := self.doMouseUp;
  self.onMouseMove := self.doMouseMove;
  self.onKeyDown := self.doKeyDown;
  self.tabStop := true;
  self.enabled := true;
  self.initialize;
  self.selectedItemIndex := kItemNone;
  drawMeans := TBrowserForm(owner).drawWeatherMeans.down;
  graphBars := TBrowserForm(owner).graphWeatherBars.down;
  self.draggable := false;
  end;

destructor KfWeatherDrawComponent.destroy;
  begin
  profile.free;
  inherited destroy;
  end;

procedure KfWeatherDrawComponent.initialize;
  begin
  self.numDays := 0;
  displayMode := kDisplayTemperature;
  labelText := 'Weather: ' + self.displayModeName;
  self.setDefaultOverlayUnit;
  end;

function KfWeatherDrawComponent.maxSelectedItemIndex: integer;
  begin
  result := kItemDisplayUnit;
  end;

function KfWeatherDrawComponent.nextSelectedItemIndex(goForward: boolean): integer;
  begin
  if goForward then
    result := kItemDisplayUnit + 1
  else
    result := kItemNone;
  end;

procedure KfWeatherDrawComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    i: integer;
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  thePoint := Point(x, y);
  if ptInRect(displayUnitRect, thePoint) then
    begin
    self.selectedItemIndex := kItemDisplayUnit;
    self.changeOverlayUnit(ssShift in shift);
    end
  else for i := 0 to 5 do if ptInRect(legendColorRects[i], thePoint) then
    begin
    self.changeLegendOrLineColor(i);
    end;
  end;

procedure KfWeatherDrawComponent.doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
    daysBefore, profileWidth, profileHeight: integer;
    dateString, unitString, valueString: string[30];
    value: single;
  begin
  try
  if weather = nil then exit;
  if numDays = 0 then exit;
  thePoint := point(x,y);
  if not ptInRect(profileRect, thePoint) then
    labelText := 'Weather: ' + displayModeName
  else
    begin
    { figure out date and value at point }
    profileWidth := profileRect.right - profileRect.left;
    profileHeight := profileRect.bottom - profileRect.top;
    daysBefore := round(safediv(profileWidth - (x - profileRect.left), profileWidth) * kMaxNumDays);
    dateString := self.dateStringDaysAndMonthsBeforePresent(daysBefore, 0);
    value := toCurrentUnit(displayMinValue + safediv(profileRect.bottom - thePoint.y, profileHeight)
      * (displayMaxValue - displayMinValue));
    valueString := digitValueString(value);
    unitString := UnitStringForEnum(displayUnitSet, displayUnitCurrent);
    labelText := 'Weather: ' + displayModeName + '   ' + dateString
      + ' (' + valueString + ' ' + unitString + ')';
    end;
  except
    labelText := 'Weather: ' + displayModeName;
  end;
  self.invalidate; { should really only invalidate top label area }
  self.repaint;
  end;

function KfWeatherDrawComponent.dateStringDaysAndMonthsBeforePresent(daysBefore, monthsBefore: integer): string;
  var
    date: GsDate;
  begin
  date := Domain.garden.date;
  GsDate_addMonths(date, -monthsBefore);
  GsDate_addDays(date, -daysBefore);
  result := GsDate_dateString(date);
  end;

function KfWeatherDrawComponent.displayModeName: string;
  begin
  result := 'no mode';
  case displayMode of
    kDisplayTemperature: result := 'temperature';
    kDisplayPrecipitation: result := 'precipitation';
    kDisplayRadiation: result := 'radiation';
    kDisplayWindSpeed: result := 'wind speed';
    kDisplayRelativeHumidity: result := 'relative humidity';
    kDisplayDayLength: result := 'day length';
    end;
  end;

procedure KfWeatherDrawComponent.changeOverlayUnit(shift: boolean);
  begin
  if shift then
    displayUnitCurrent := GetPreviousUnitEnumInUnitSet(displayUnitSet, displayUnitCurrent)
  else
    displayUnitCurrent := GetNextUnitEnumInUnitSet(displayUnitSet, displayUnitCurrent);
  self.resizeElements;
  self.invalidate;
  end;

procedure KfWeatherDrawComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) and (self.selectedItemIndex = kItemDisplayUnit) then
    self.changeOverlayUnit(ssShift in shift);
  end;

procedure KfWeatherDrawComponent.changeLegendOrLineColor(index: integer);
  var colorDialog: TColorDialog;
  begin
  colorDialog := TColorDialog.create(owner);
  try
  case displayMode of
    kDisplayTemperature: colorDialog.color := GsDomain.default.weatherDrawColors.temperatureColors[index];
    kDisplayPrecipitation: colorDialog.color := GsDomain.default.weatherDrawColors.precipitationColor;
    kDisplayRadiation: colorDialog.color := GsDomain.default.weatherDrawColors.radiationColor;
    kDisplayWindSpeed: colorDialog.color := GsDomain.default.weatherDrawColors.windSpeedColor;
    kDisplayRelativeHumidity: colorDialog.color := GsDomain.default.weatherDrawColors.relativeHumidityColor;
    kDisplayDayLength: colorDialog.color := GsDomain.default.weatherDrawColors.dayLengthColor;
    end;
  if colorDialog.execute then
    begin
    case displayMode of
      kDisplayTemperature: GsDomain.default.weatherDrawColors.temperatureColors[index] := colorDialog.color;
      kDisplayPrecipitation: GsDomain.default.weatherDrawColors.precipitationColor := colorDialog.color;
      kDisplayRadiation: GsDomain.default.weatherDrawColors.radiationColor := colorDialog.color;
      kDisplayWindSpeed: GsDomain.default.weatherDrawColors.windSpeedColor := colorDialog.color;
      kDisplayRelativeHumidity: GsDomain.default.weatherDrawColors.relativeHumidityColor := colorDialog.color;
      kDisplayDayLength: GsDomain.default.weatherDrawColors.dayLengthColor := colorDialog.color;
      end;
    self.invalidate;
    self.repaint;
    end;
  finally
    colorDialog.free;
    colorDialog := nil;
  end;
  end;

function KfWeatherDrawComponent.collapsedHeight: integer;
  begin
  result := self.textHeight + kTopBottomGap;
  end;

procedure KfWeatherDrawComponent.collapseOrExpand(y: integer);
  begin
  { do nothing }
  end;

procedure KfWeatherDrawComponent.updateWeather(newWeather: GsWeather);
  begin
  if weather <> newWeather then
  	begin
    weather := newWeather;
    updateCurrentValue(0);
    end;
  end;

procedure KfWeatherDrawComponent.updateModelValues;
  begin
  updateCurrentValue(0);
  updateDisplay;
  repaint; { NOT refresh }
  end;

procedure KfWeatherDrawComponent.updateCurrentValue;
  begin
  if weather = nil then exit;
  numDays := weather.historyNumDays;
  end;

procedure KfWeatherDrawComponent.updateModel(newModel: GsModel);
	begin
  { ignore newModel in this case because weather is set differently }
  updateCurrentValue(0);
  updateDisplay;
  repaint; { NOT refresh }
  end;

procedure KfWeatherDrawComponent.updateDisplay;
  var
    i: integer;
    value, maxValue: single;
  begin
  if weather = nil then exit;
  if numDays <= 0 then exit;
  value := 0.0;
  maxValue := 0.0;
  if numDays > 0 then for i := 0 to numDays - 1 do
    begin
    case displayMode of
      kDisplayTemperature: value := weather.historyDailyWeather[i].minTempForDay_degC;
      kDisplayPrecipitation: value := weather.historyDailyWeather[i].rainfallForDay_mm;
      kDisplayRadiation: value := weather.historyDailyWeather[i].radiationForDay_MJPm2;
      kDisplayWindSpeed: value := weather.historyDailyWeather[i].meanWindSpeedForDay_mPsec;
      kDisplayRelativeHumidity: value := weather.historyDailyWeather[i].relHumForDay_frn;
      kDisplayDayLength: value := weather.historyDailyWeather[i].dayLength_hr;
      end;
    checkBoundsWithMargin(value, displayMinValue, maxValue);
    { if temperature, also consider max temp }
    if displayMode = kDisplayTemperature then
      checkBoundsWithMargin(weather.historyDailyWeather[i].maxTempForDay_degC, displayMinValue, maxValue);
    end;
  if rescale then
    displayMaxValue := maxValue
  else
    displayMaxValue := max(maxValue, displayMaxValue);
  rescale := false;
  self.invalidate;
  end;

procedure KfWeatherDrawComponent.setDefaultOverlayUnit;
  begin
  case displayMode of
    kDisplayTemperature:
      begin
      displayUnitSet := kTemperature;
      displayUnitModel := kTemperatureDegreesC;
      if GsDomain.default.menuOptions.showMetricUnits then
        displayUnitCurrent := kTemperatureDegreesC
      else
        displayUnitCurrent := kTemperatureDegreesF;
      displayMinValue := -30.0;
      displayMaxValue := 40.0;
      end;
    kDisplayPrecipitation:
      begin
      displayUnitSet := kDepthOfWater;
      displayUnitModel := kDepthOfWaterMillimeters;
      if GsDomain.default.menuOptions.showMetricUnits then
        displayUnitCurrent := kDepthOfWaterMillimeters
      else
        displayUnitCurrent := kDepthOfWaterInches;
      displayMinValue := 0.0;
      displayMaxValue := 100.0;
      end;
    kDisplayRadiation:
      begin
      displayUnitSet := kRadiation;
      displayUnitModel := kRadiationMegaJoulesPSquareMeter;
      if GsDomain.default.menuOptions.showMetricUnits then
        displayUnitCurrent := kRadiationCaloriesPerSquareMeter
      else
        displayUnitCurrent := kRadiationCaloriesPerSquareFoot;
      displayMinValue := 0.0;
      displayMaxValue := 0.0;
      end;
    kDisplayWindSpeed:
      begin
      displayUnitSet := kVelocity;
      displayUnitModel := kVelocityMetersPSecond;
      if GsDomain.default.menuOptions.showMetricUnits then
        displayUnitCurrent := kVelocityMetersPSecond { CFK FIX - there should be a unit km/hr }
      else
        displayUnitCurrent := kVelocityMilesPHour;
      displayMinValue := 0.0;
      displayMaxValue := 10.0;
      end;
    kDisplayRelativeHumidity:
      begin
      displayUnitSet := kNonChangingUnitFraction;
      displayUnitModel := kDimensionless;
      displayUnitCurrent := kDimensionless;
      displayMinValue := 0.0;
      displayMaxValue := 1.0;
      end;
    kDisplayDayLength:
      begin
      displayUnitSet := kNonChangingUnitHours;
      displayUnitModel := kDimensionless;
      displayUnitCurrent := kDimensionless;
      displayMinValue := 6.0;
      displayMaxValue := 18.0;
      end;
    end;
  labelText := 'Weather: ' + displayModeName;
  self.updateDisplay;
  end;

procedure KfWeatherDrawComponent.resizeElements;
  var
    fullRect: TRect;
  begin
  fullRect := getClientRect;
  with profile do
    begin
    width := fullRect.right - fullRect.left;
    height := fullRect.bottom - fullRect.top;
    end;
  self.updateDisplay;
  end;

procedure KfWeatherDrawComponent.paint;
  var
    fullRect, minOverlayRect, maxOverlayRect, labelRect: TRect;
    minOverlayText, maxOverlayText, displayUnitText: string[30];
    firstDateText, todayDateText: string[30];
    firstDateRect, todayDateRect: TRect;
    maxWidth: integer;
  begin
  fullRect := getClientRect;
  self.canvas.font := self.font;
  if self.textHeight = 0 then self.calculateTextDimensions;
  minOverlayText := digitValueString(toCurrentUnit(displayMinValue));
  maxOverlayText := digitValueString(toCurrentUnit(displayMaxValue));
  displayUnitText := UnitStringForEnum(displayUnitSet, displayUnitCurrent);
  with profileRect do
    begin
    maxWidth := intMax(self.canvas.textWidth(minOverlayText),
      intMax(self.canvas.textWidth(maxOverlayText), self.canvas.textWidth(displayUnitText)));
    left := fullRect.left + kLeftRightGap + maxWidth + kLeftRightGap;
    right := fullRect.right - kLeftRightGap;
    top := fullRect.top + self.collapsedHeight + kTopBottomGap;
    bottom := fullRect.bottom - kTopBottomGap - self.textHeight - kTopBottomGap;
    if displayMode = kDisplayTemperature then
      bottom := bottom - self.textHeight * 3 - kTopBottomGap * 3
    else
      bottom := bottom - self.textHeight - kTopBottomGap;
    end;
  with labelRect do
    begin
    left := profileRect.left; right := left + self.canvas.textWidth(labelText);
    bottom := profileRect.top - kTopBottomGap; top := bottom - self.textHeight;
    end;
  with minOverlayRect do
    begin
    left := fullRect.left + kLeftRightGap; right := left + self.canvas.textWidth(minOverlayText);
    bottom := profileRect.bottom - kTopBottomGap;
    top := bottom - self.textHeight;
    end;
  with maxOverlayRect do
    begin
    left := fullRect.left + kLeftRightGap; right := left + self.canvas.textWidth(maxOverlayText);
    top := profileRect.top + kTopBottomGap; bottom := top + self.textHeight;
    end;
  with displayUnitRect do
    begin
    left := fullRect.left + kLeftRightGap; right := left + self.canvas.textWidth(displayUnitText);
    top := profileRect.top + (profileRect.bottom - profileRect.top) div 2
      - self.canvas.textWidth(displayUnitText) div 2;
    bottom := top + self.textHeight;
    end;
  firstDateText := self.dateStringDaysAndMonthsBeforePresent(0, 1);
  todayDateText := self.dateStringDaysAndMonthsBeforePresent(0, 0);
  with firstDateRect do
    begin
    left := profileRect.left; right := left + self.canvas.textWidth(firstDateText);
    top := profileRect.bottom + kTopBottomGap; bottom := top + self.textHeight;
    end;
  with todayDateRect do
    begin
    right := profileRect.right; left := right - self.canvas.textWidth(todayDateText);
    top := profileRect.bottom + kTopBottomGap; bottom := top + self.textHeight;
    end;
  profile.canvas.brush.color := clBtnFace;
  profile.canvas.rectangle(0, 0, profile.width, profile.height);
  self.drawTextLabel(labelText, labelRect, false, false);
  self.drawProfile;
  profile.canvas.brush.style := bsClear;
  profile.canvas.pen.color := clBlack;
  with profileRect do
    begin
    profile.canvas.moveTo(left, top);
    profile.canvas.lineTo(left, bottom);
    profile.canvas.lineTo(right, bottom);
    end;
  self.drawLegend;
  self.drawTextLabel(firstDateText, firstDateRect, false, false);
  self.drawTextLabel(todayDateText, todayDateRect, false, false);
  self.drawTextLabel(minOverlayText, minOverlayRect, false, false);
  self.drawTextLabel(maxOverlayText, maxOverlayRect, false, false);
  self.drawTextLabel(displayUnitText, displayUnitRect, true, (self.selectedItemIndex = kItemDisplayUnit));
  self.canvas.draw(0, 0, profile);
  end;

const kLegendColorSize = 15;

procedure KfWeatherDrawComponent.drawTextLabel(text: string; drawRect: TRect; drawLine, drawBox: boolean);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with profile.canvas do
    begin
    font := self.font;
    font.color := clBtnText;
    strPCopy(cText, text);
    winprocs.drawText(handle, cText, strLen(cText), drawRect, DT_LEFT);
    end;
  if drawLine then with profile.canvas do
    begin
    pen.color := clBtnShadow;
    moveTo(drawRect.left, drawRect.bottom - 2);
    lineTo(drawRect.right, drawRect.bottom - 2);
    end;
  if drawBox then with profile.canvas do
    begin
    pen.color := clBtnShadow;
    moveTo(drawRect.right + 2, drawRect.top);
    lineTo(drawRect.left - 2, drawRect.top);
    lineTo(drawRect.left - 2, drawRect.bottom - 2);
    lineTo(drawRect.right + 2, drawRect.bottom - 2);
    lineTo(drawRect.right + 2, drawRect.top);
    end;
  end;

procedure KfWeatherDrawComponent.drawLegend;
  var
    i, pos, numVars: integer;
    stringRects: array[0..2] of TRect;
    legendStrings: array[0..2] of string[50];
    legendColors: array[0..2] of TColorRef;
  begin
  self.canvas.pen.color := clBlack;
  numVars := 1;
  case displayMode of
    kDisplayTemperature:
      begin
      numVars := 3;
      legendStrings[0] := 'Lowest temperature for day';
      legendStrings[1] := 'Average temperature for day';
      legendStrings[2] := 'Highest temperature for day';
      for i := 0 to numVars - 1 do legendColors[i] := GsDomain.default.weatherDrawColors.temperatureColors[i];
      end;
    kDisplayPrecipitation:
      begin
      numVars := 1;
      legendStrings[0] := 'Precipitation (rain or snow)';
      legendColors[0] := GsDomain.default.weatherDrawColors.precipitationColor;
      end;
    kDisplayRadiation:
      begin
      numVars := 1;
      legendStrings[0] := 'Radiation for day';
      legendColors[0] := GsDomain.default.weatherDrawColors.radiationColor;
      end;
    kDisplayWindSpeed:
      begin
      numVars := 1;
      legendStrings[0] := 'Wind speed for day';
      legendColors[0] := GsDomain.default.weatherDrawColors.windSpeedColor;
      end;
    kDisplayRelativeHumidity:
      begin
      numVars := 1;
      legendStrings[0] := 'Relative humidity for day';
      legendColors[0] := GsDomain.default.weatherDrawColors.relativeHumidityColor;
      end;
    kDisplayDayLength:
      begin
      numVars := 1;
      legendStrings[0] := 'Day length';
      legendColors[0] := GsDomain.default.weatherDrawColors.dayLengthColor;
      end;
    end;
  pos := profileRect.bottom + kTopBottomGap * 2 + self.textHeight;
  for i := 0 to 2 do legendColorRects[i] := rect(0, 0, 0, 0);
  for i := 0 to numVars - 1 do
    begin
    with legendColorRects[i] do
      begin
      left := profileRect.left;
      right := left + kLegendColorSize;
      top := pos;
      bottom := top + self.textHeight;
      end;
    with stringRects[i] do
      begin
      left := legendColorRects[i].right + kBetweenGap;
      right := left + self.canvas.textWidth(legendStrings[i]);
      top := legendColorRects[i].top; bottom := top + self.textHeight;
      end;
    pos := pos + (stringRects[i].bottom - stringRects[i].top) + kTopBottomGap;
    with profile.canvas do
      begin
      brush.color := legendColors[i];
      rectangle(legendColorRects[i].left, legendColorRects[i].top, legendColorRects[i].right, legendColorRects[i].bottom);
      brush.style := bsClear;
      end;
    self.drawTextLabel(legendStrings[i], stringRects[i], false, false);
    end;
  end;

procedure KfWeatherDrawComponent.drawProfile;
  var i: integer;
  begin
  if (self.numDays <= 0) then exit;
  if weather = nil then exit;
  profile.canvas.pen.color := clBlack;
  case displayMode of
    kDisplayTemperature:
      begin
      { graphing these correctly as bars depends on them not crossing over }
      { max temp first }
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.temperatureColors[2];
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].maxTempForDay_degC);
      { mean temp }
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.temperatureColors[1];
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].meanTempForDay_degC);
      { min temp }
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.temperatureColors[0];
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].minTempForDay_degC);
      if drawMeans then
        begin
        profile.canvas.pen.color := GsDomain.default.weatherDrawColors.temperatureColors[0];
        for i := 0 to self.numDays - 1 do
          drawLinePoint(i, weather.historyDailyFromMonthlyMeans[i].minTemp_degC);
        profile.canvas.pen.color := GsDomain.default.weatherDrawColors.temperatureColors[2];
        for i := 0 to self.numDays - 1 do
          drawLinePoint(i, weather.historyDailyFromMonthlyMeans[i].maxTemp_degC);
        end;
      end;
    kDisplayPrecipitation:
      begin
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.precipitationColor;
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].rainfallForDay_mm);
      end;
    kDisplayRadiation:
      begin
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.radiationColor;
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].radiationForDay_MJPm2);
      if drawMeans then
        for i := 0 to self.numDays - 1 do
          drawLinePoint(i, weather.historyDailyFromMonthlyMeans[i].radiation_MJPm2);
      end;
    kDisplayWindSpeed:
      begin
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.windSpeedColor;
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].meanWindSpeedForDay_mPsec);
      if drawMeans then
        for i := 0 to self.numDays - 1 do
          drawLinePoint(i, weather.historyDailyFromMonthlyMeans[i].windSpeed_mPsec);
      end;
    kDisplayRelativeHumidity:
      begin
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.relativeHumidityColor;
      for i := 0 to self.numDays - 1 do
        drawLinePointOrBar(i, weather.historyDailyWeather[i].relHumForDay_frn);
      if drawMeans then
        for i := 0 to self.numDays - 1 do
          drawLinePoint(i, weather.historyDailyFromMonthlyMeans[i].relHum_frn);
      end;
    kDisplayDayLength:
      begin
      profile.canvas.pen.color := GsDomain.default.weatherDrawColors.dayLengthColor;
      for i := 0 to self.numDays - 1 do drawLinePointOrBar(i, weather.historyDailyWeather[i].dayLength_hr);
      end;
    end;
  profile.canvas.pen.color := clBlack;
  end;

procedure KfWeatherDrawComponent.drawLinePointOrBar(i: integer; value: single);
  begin
  if self.graphBars then
    self.drawBar(i, value)
  else
    self.drawLinePoint(i, value);
  end;

procedure KfWeatherDrawComponent.drawLinePoint(i: integer; value: single);
  var
    dayDistance, varDistance: integer;
    proportion: single;
    thePoint: TPoint;
  begin
  varDistance := self.varDistanceForGraph(value);
  if i = numDays - 1 then
    dayDistance := profileRect.right - profileRect.left
  else
    begin
    proportion := ((kMaxNumDays - numDays) + i) / (kMaxNumDays - 1);
    dayDistance := round(proportion * (profileRect.right - profileRect.left));
    end;
  thePoint := point(profileRect.left + dayDistance, profileRect.top + profileRect.bottom - varDistance);
  if i = 0 then
    profile.canvas.moveTo(thePoint.x, thePoint.y)
  else
    profile.canvas.lineTo(thePoint.x, thePoint.y);
  end;

procedure KfWeatherDrawComponent.drawBar(i: integer; value: single);
  var
    drawRect: TRect;
    proportion: single;
    varDistance, dayDistance, barWidth: integer;
  begin
  varDistance := self.varDistanceForGraph(value);
  proportion := ((kMaxNumDays - numDays) + i) / kMaxNumDays;
  dayDistance := round(proportion * (profileRect.right - profileRect.left));
  barWidth := (profileRect.right - profileRect.left) div kMaxNumDays - 2;
  { make sure lines at edge of profileRect are not drawn over }
  if dayDistance <= 0 then dayDistance := 1;
  if varDistance <= 0 then varDistance := 1;
  drawRect := Rect(profileRect.left + dayDistance, profileRect.bottom - 1,
    profileRect.left + dayDistance + barWidth, profileRect.top + profileRect.bottom - varDistance);
  profile.canvas.brush.color := profile.canvas.pen.color;
  profile.canvas.fillRect(drawRect);
  end;

function KfWeatherDrawComponent.varDistanceForGraph(value: single): integer;
  var
    proportion: single;
  begin
  try
  if (displayMaxValue - displayMinValue <> 0.0) then
    proportion := safediv(value - displayMinValue, displayMaxValue - displayMinValue)
  else
    proportion := 0.0;
  result := profileRect.top + round(proportion * (profileRect.bottom - profileRect.top));
  except
    result := 0;
  end;
  end;

function KfWeatherDrawComponent.toModelUnit(value: single): single;
  begin
  result := Convert(displayUnitSet, displayUnitCurrent, displayUnitModel, value)
  end;

function KfWeatherDrawComponent.toCurrentUnit(value: single): single;
  begin
  result := Convert(displayUnitSet, displayUnitModel, displayUnitCurrent, value)
  end;

end.

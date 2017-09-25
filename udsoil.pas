unit Udsoil;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udsoil: Browser pictures side component for displaying soil color, texture, materials,
temperature, pH, N and P. Uses bitmap to double-buffer picture drawn. Descended from
browser component superclass (ubrowcom) but overrides many functions to change behaviors.
Has no aspect or groupItem, but has a soil patch. See ubrowtri for more information
on right-side browser components.}

interface

uses WinTypes, WinProcs, Controls, ExtCtrls, Graphics, Forms, Classes, ubrowcom, uesoil, umodel;

const
  kDisplayColor = 0;
  kDisplayTexture = 1;
  kDisplayContents = 2;
  kDisplayTemperature = 3;
  kDisplayRootWeight = 4;
  kDisplayPH = 5;
  kDisplayNConc = 6;
  kDisplayPConc = 7;

  kItemDisplayUnit = 1;
  kItemDepthUnit = 2;

type

polygonPointsArray = array[0..14] of TPoint;

SoilLayerInfo = record
  { color & thickness }
  thickness_m: single;
  color: TColorRef;
  { percents for content graph (add up to 100) }
  percentAir: single;
  percentWater: single;
  percentClay: single;
  percentSilt: single;
  percentSand: single;
  percentOM: single;
  percentRocks: single;
  { other vars }
  temperature_degC: single;
  totalRootWeight_kg: single;
  totalRootWeightPerUnitSoilThickness_kg: single;
  pH: single;
  { N }
  nitrateN_gPt: single;
  ammoniaN_gPt: single;
  organicNFresh_gPt: single; { in crop residue and microbial biomass }
  organicNActiveHumus_gPt: single;
  organicNStableHumus_gPt: single;
  { P }
  labileP_gPt: single;
  organicPFresh_gPt: single;
  organicPHumus_gPt: single;
  mineralPActive_gPt: single;
  mineralPStable_gPt: single;
  reservedArray: array [1..160] of byte;
  end;

KfSoilDrawComponent = class(KfBrowserComponent)
	public
  profile: TBitmap;
  soil: GsSoilPatch;
  baseColor: TColorRef; { get from soil patch }
  mulchColor: TColorRef; { get from soil patch }
  displayMode: integer;
  drawLayers: array[0..9] of SoilLayerInfo;
  numLayers: integer;
  profileRect: TRect;
  layerRects: array[0..9] of TRect;
  mulchRect: TRect;
  snowRect: TRect;
  labelRect: TRect;
  labelText: string[60];
  legendColorRects: array[0..7] of TRect;
  displayMaxValue: single;
  displayMinValue: single;
  displayUnitSet: integer;
  displayUnitModel: integer;
  displayUnitCurrent: integer;
  displayUnitRect: TRect;
  depthUnitModel: integer;
  depthUnitCurrent: integer;
  depthUnitRect: TRect;
  drawLines: boolean;
  rescale: boolean;
  graphBars: boolean;
  constructor create(anOwner: TComponent); override;
  destructor destroy; override;
  procedure initialize; override;
  procedure updateSoilPatch(newPatch: GsSoilPatch);
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateColors;
  procedure updateModel(newModel: GsModel); override;
  procedure updateDisplay; override;
  procedure paint; override;
  procedure resizeElements; override;
  procedure drawLegend;
  procedure drawProfile;
  procedure drawProfileWithNitrogenConcentration;
  procedure drawProfileWithPhosphorusConcentration;
  procedure drawProfileWithRootWeight;
  procedure drawProfileWithTemperatureOrPH;
  procedure drawProfileWithContents;
  procedure placePolygonPoint(i: integer; distance: integer; var polygonPoints: polygonPointsArray);
  procedure drawLinePoint(i, distance: integer);
  procedure drawBar(layerIndex, distance: integer);
  function maxSelectedItemIndex: integer; override;
  function nextSelectedItemIndex(goForward: boolean): integer; override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  procedure collapseOrExpand(y: integer); override;
  function toModelUnit(item: integer; value: single): single;
  function toCurrentUnit(item: integer; value: single): single;
  procedure setDefaultOverlayUnit;
  procedure changeOverlayUnit(shift: boolean);
  procedure changeDepthUnit(shift: boolean);
  procedure zeroLayerFields(i: integer);
  procedure drawTextLabel(text: string; drawRect: TRect; drawLine, drawBox: boolean);
  function displayModeName: string;
  procedure changeLegendOrLineColor(index: integer);
  function collapsedHeight: integer; override;
  procedure calculateRootWeightsPerCurrentUnitOfSoil;
  end;

implementation

uses SysUtils, Dialogs, uunits, ueutils, ueqh, udomain, ubrowser, usupport, ugsim, ugscom,
  umconsts;

constructor KfSoilDrawComponent.create(anOwner: TComponent);
  begin
  inherited create(anOwner);
  self.model := nil;
  self.soil := nil;
  profile := TBitmap.create;
  self.parentFont := true;
  self.onMouseUp := self.doMouseUp;
  self.onMouseMove := self.doMouseMove;
  self.onKeyDown := self.doKeyDown;
  self.tabStop := true;
  self.enabled := true;
  self.initialize;
  self.selectedItemIndex := kItemNone;
  drawLines := TBrowserForm(owner).drawSoilLines.down;
  graphBars := TBrowserForm(owner).graphSoilBars.down;
  self.draggable := false;
  end;

destructor KfSoilDrawComponent.destroy;
  begin
  profile.free;
  inherited destroy;
  end;

procedure KfSoilDrawComponent.initialize;
  begin
  baseColor := support_rgb(200, 200, 200);
  mulchColor := support_rgb(200, 200, 200);
  depthUnitModel := kLengthMeters;
  if Domain.menuOptions.showMetricUnits then
    depthUnitCurrent := kLengthMeters
  else
    depthUnitCurrent := kLengthFeet;
  displayMode := kDisplayColor;
  labelText := 'Soil Profile: ' + self.displayModeName;
  end;

function KfSoilDrawComponent.maxSelectedItemIndex: integer;
  begin
  result := kItemDepthUnit;
  end;

function KfSoilDrawComponent.nextSelectedItemIndex(goForward: boolean): integer;
  begin
  result := kItemNone;
  case self.selectedItemIndex of
    kItemLabel:
      begin
      if goForward then
        begin
        if displayMode <> kDisplayColor then
          result := kItemDisplayUnit
        else
          result := kItemDepthUnit;
        end
      else
        result := kItemNone;
      end;
    kItemDisplayUnit:
      if goForward then
        result := kItemDepthUnit
      else
        result := kItemLabel;
    kItemDepthUnit:
      if goForward then
        result := kItemDepthUnit + 1
      else
        begin
        if displayMode <> kDisplayColor then
          result := kItemDisplayUnit
        else
          result := kItemLabel;
        end;
    end;
  end;

procedure KfSoilDrawComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
  else if ptInRect(depthUnitRect, thePoint) then
    begin
    self.selectedItemIndex := kItemDepthUnit;
    self.changeDepthUnit(ssShift in shift);
    end
  else for i := 0 to 7 do if ptInRect(legendColorRects[i], thePoint) then
    begin
    self.changeLegendOrLineColor(i);
    end;
  end;

procedure KfSoilDrawComponent.doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    i: integer;
    thePoint: TPoint;
    unitString, depthString, layerEtcString: string[20];
    depth_m, mulchDepth_m, snowDepth_m, totalDepth_m: single;
    drawDepth: boolean;
    soilRect: TRect;
  begin
  labelText := 'Soil Profile: ' + self.displayModeName;
  drawDepth := false;
  depth_m := 0.0;
  mulchDepth_m := 0.0;
  snowDepth_m := 0.0;
  totalDepth_m := 0.0;
  try
  if soil <> nil then
    begin
    mulchDepth_m := soil.mulch.depth_m;
    snowDepth_m := soil.snowDepthFromSnowWaterContent_m;
    totalDepth_m := soil.layers[numLayers-1].depth_m;
    thePoint := point(x,y);
    drawDepth := false;
    depth_m := 0.0;
    if not ptInRect(profileRect, thePoint) then
      labelText := 'Soil Profile: ' + displayModeName
    else if ptInRect(snowRect, thePoint) then
      begin
      drawDepth := true;
      layerEtcString := '[Snow]';
      if snowRect.bottom - snowRect.top <> 0 then
        depth_m := safediv(thePoint.y - snowRect.top, snowRect.bottom - snowRect.top) * snowDepth_m
      else
        depth_m := 0.0;
      end
    else if ptInRect(mulchRect, thePoint) then
      begin
      drawDepth := true;
      layerEtcString := '[Mulch]';
      if mulchRect.bottom - mulchRect.top <> 0 then
        depth_m := safediv(thePoint.y - mulchRect.top, mulchRect.bottom - mulchRect.top) * mulchDepth_m
      else
        depth_m := 0.0;
      end
    else for i := 0 to numLayers - 1 do
      if ptInRect(layerRects[i], thePoint) then
        begin
        drawDepth := true;
        layerEtcString := '[Layer ' + intToStr(i+1);
        soilRect := profileRect;
        soilRect.top := mulchRect.bottom;
        if soilRect.bottom - soilRect.top <> 0 then
          depth_m := safediv(thePoint.y - soilRect.top, soilRect.bottom - soilRect.top) * totalDepth_m
        else
          depth_m := 0.0;
        break;
        end;
    end;
  if drawDepth then
    begin
    unitString := UnitStringForEnum(kLength, depthUnitCurrent);
    depthString := digitValueString(toCurrentUnit(kItemDepthUnit, depth_m));
    labelText := 'Soil Profile: ' + displayModeName + '   ' + layerEtcString
      + ' (' + depthString + ' ' + unitString + ')]';
   end;
  except
    labelText := 'Soil Profile: ' + self.displayModeName;
  end;
  self.invalidate; { should really only invalidate top label area }
  self.repaint;
  end;

function KfSoilDrawComponent.displayModeName: string;
  begin
  result := 'no mode';
  case displayMode of
    kDisplayColor: result := 'color';
    kDisplayContents: result := 'materials';
    kDisplayTemperature: result := 'temperature';
    kDisplayRootWeight: result := 'root weight per ' + UnitStringForEnum(kLength, depthUnitCurrent)
      + ' soil';
    kDisplayPH: result := 'pH';
    kDisplayNConc: result := 'nitrogen';
    kDisplayPConc: result := 'phosphorus';
    end;
  end;

procedure KfSoilDrawComponent.changeOverlayUnit(shift: boolean);
  begin
  if shift then
    displayUnitCurrent := GetPreviousUnitEnumInUnitSet(displayUnitSet, displayUnitCurrent)
  else
    displayUnitCurrent := GetNextUnitEnumInUnitSet(displayUnitSet, displayUnitCurrent);
  self.resizeElements;
  self.invalidate;
  end;

procedure KfSoilDrawComponent.changeDepthUnit(shift: boolean);
  begin
  if shift then
    depthUnitCurrent := GetPreviousUnitEnumInUnitSet(kLength, depthUnitCurrent)
  else
    depthUnitCurrent := GetNextUnitEnumInUnitSet(kLength, depthUnitCurrent);
  self.calculateRootWeightsPerCurrentUnitOfSoil;
  self.resizeElements;
  self.invalidate;
  end;

procedure KfSoilDrawComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) then
    begin
    case self.selectedItemIndex of
      kItemDisplayUnit: self.changeOverlayUnit(ssShift in shift);
      kItemDepthUnit: self.changeDepthUnit(ssShift in shift);
      end;
    end;
  end;

procedure KfSoilDrawComponent.changeLegendOrLineColor(index: integer);
  var colorDialog: TColorDialog;
  begin
  colorDialog := TColorDialog.create(owner);
  try
  case displayMode of
    kDisplayColor:
      if index = 0 then
        colorDialog.color := self.baseColor
      else
        colorDialog.color := self.mulchColor;
    kDisplayContents:
      if index > 0 then
        colorDialog.color := Domain.soilDrawColors.materialsColors[index-1]
      else
        { root color is not in array because it was added later (overwrote pesticide color) }
        colorDialog.color := Domain.soilDrawColors.rootMaterialColor;
    kDisplayTemperature: colorDialog.color := Domain.soilDrawColors.temperatureColor;
    kDisplayRootWeight: colorDialog.color := Domain.soilDrawColors.rootWeightColor;
    kDisplayPH: colorDialog.color := Domain.soilDrawColors.pHColor;
    kDisplayNConc: colorDialog.color := Domain.soilDrawColors.nitrogenColors[index];
    kDisplayPConc: colorDialog.color := Domain.soilDrawColors.phosphorusColors[index];
    end;
  if colorDialog.execute then
    begin
    case displayMode of
      kDisplayColor:
        begin
        if index = 0 then
          begin
          self.baseColor := colorDialog.color;
          { this will call browser.updateModelField, which will update the base color if it is in the components }
          GardenForm.doCommand(
            GsChangeDomainColorCommand.createCommand(soil, colorDialog.color, kSoilPatchBaseDrawingColor));
          end
        else
          begin
          self.mulchColor := colorDialog.color;
          { this will call browser.updateModelField, which will update the base color if it is in the components }
          GardenForm.doCommand(
            GsChangeDomainColorCommand.createCommand(soil, colorDialog.color, kSoilPatchMulchDrawingColor));
          end;
        end;
      kDisplayContents:
        if index > 0 then
          Domain.soilDrawColors.materialsColors[index-1] := colorDialog.color
        else
          { root color is not in array because it was added later (overwrote pesticide color) }
          Domain.soilDrawColors.rootMaterialColor := colorDialog.color;
      kDisplayTemperature: Domain.soilDrawColors.temperatureColor := colorDialog.color;
      kDisplayRootWeight: Domain.soilDrawColors.rootWeightColor := colorDialog.color;
      kDisplayPH: Domain.soilDrawColors.pHColor := colorDialog.color;
      kDisplayNConc: Domain.soilDrawColors.nitrogenColors[index] := colorDialog.color;
      kDisplayPConc: Domain.soilDrawColors.phosphorusColors[index] := colorDialog.color;
      end;
    self.invalidate;
    self.repaint;
    end;
  finally
    colorDialog.free;
    colorDialog := nil;
  end;
  end;

function KfSoilDrawComponent.collapsedHeight: integer;
  begin
  result := self.textHeight + kTopBottomGap;
  end;

procedure KfSoilDrawComponent.collapseOrExpand(y: integer);
  begin
  { do nothing }
  end;

procedure KfSoilDrawComponent.updateSoilPatch(newPatch: GsSoilPatch);
  begin
  if soil <> newPatch then
  	begin
    soil := newPatch;
    updateCurrentValue(0);
    end;
  end;

procedure KfSoilDrawComponent.updateModelValues;
  begin
  updateCurrentValue(0);
  updateDisplay;
  repaint; { NOT refresh }
  end;

procedure KfSoilDrawComponent.updateCurrentValue;
  var
    i: integer;
    orgC_pct, depthLastLayer: single;
    percentPoreSpace, percentSolids: single;
    totalPercent, correction, totalRootWeight_t: single;
    totalSandSiltClay_pct: single;
  begin
  depthLastLayer := 0.0;
  for i := 0 to 9 do self.zeroLayerFields(i);
  try
  if soil = nil then exit;
  self.numLayers := soil.state.numLayers;
  self.updateColors;
  if numLayers > 0 then for i := 0 to numLayers - 1 do with drawLayers[i] do
    begin
    { thickness }
    thickness_m := soil.layers[i].depth_m - depthLastLayer;
    if thickness_m = 0 then
      raise Exception.create('Soil layer thickness is zero');
    depthLastLayer := soil.layers[i].depth_m;
    { percents for content graph }
    percentPoreSpace := 100.0 * safediv(soil.layers[i].porosity_mm, thickness_m * m_to_mm);
    percentPoreSpace := max(0.0, min(100.0, percentPoreSpace * (1.0 - soil.layers[i].rockContent_pct * pct_to_frn)));
    percentWater := safediv(soil.layers[i].waterContent_mm, soil.layers[i].porosity_mm) * percentPoreSpace;
    percentWater := min(percentPoreSpace, max(0.0, percentWater));
    percentAir := percentPoreSpace - percentWater;
    percentAir := min(percentPoreSpace, max(0.0, percentAir));
    percentSolids := 100.0 - percentPoreSpace;
    orgC_pct := Utils_OrganicCFromOrganicMatter_pct(soil.layers[i].organicMatter_tPha, soil.layers[i].weight_tPha);
    totalSandSiltClay_pct := max(0.0, min(100.0, 100.0 - (orgC_pct + soil.layers[i].rockContent_pct)));
    percentRocks := soil.layers[i].rockContent_pct * percentSolids * pct_to_frn;
    percentOM := orgC_pct* percentSolids * pct_to_frn;
    percentClay := soil.layers[i].clayContent_pct * totalSandSiltClay_pct * pct_to_frn * percentSolids * pct_to_frn;
    percentSilt := soil.layers[i].siltContent_pct * totalSandSiltClay_pct * pct_to_frn * percentSolids * pct_to_frn;
    percentSand := soil.layers[i].sandContent_pct * totalSandSiltClay_pct * pct_to_frn * percentSolids * pct_to_frn;
    { check that all percents add up to exactly 100.0 }
    totalPercent := percentOM + percentClay + percentSilt + percentSand + percentRocks + percentWater + percentAir;
    if totalPercent <> 100.0 then
      begin
      correction := safediv(100.0, totalPercent);
      percentOM := percentOM * correction;
      percentClay := percentClay * correction;
      percentSilt := percentSilt * correction;
      percentSand := percentSand * correction;
      percentRocks := percentRocks * correction;
      percentWater := percentWater * correction;
      percentAir := percentAir * correction;
      end;
    { other vars }
    temperature_degC := soil.layers[i].temperature_degC;
    soil.ConvertRelativeAndAbsolute(kGetField, soil.layers[i].patchTotalRootWeight_tPha, totalRootWeight_t);
    totalRootWeight_kg := totalRootWeight_t * t_to_kg;
    pH := soil.layers[i].soilpH;
    { N }
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].nitrate_kgPha, nitrateN_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].ammonia_kgPha, ammoniaN_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].organicNFresh_kgPha, organicNFresh_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].organicNActiveHumus_kgPha, organicNActiveHumus_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].organicNStableHumus_kgPha, organicNStableHumus_gPt, i);
    { P }
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].labileP_kgPha, labileP_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].organicPFresh_kgPha, organicPFresh_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].organicPHumus_kgPha, organicPHumus_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].mineralPActive_kgPha, mineralPActive_gPt, i);
    soil.convertNutrientsAbsoluteAndRelative(kGetField, soil.layers[i].mineralPStable_kgPha, mineralPStable_gPt, i);
    end;
  self.calculateRootWeightsPerCurrentUnitOfSoil;
  except
    for i := 0 to 9 do self.zeroLayerFields(i);
    errorMessage('Problem in KfSoilDrawComponent.updateCurrentValue');
  end;
  end;

procedure KfSoilDrawComponent.calculateRootWeightsPerCurrentUnitOfSoil;
  var i: smallint;
  begin
  try
  if numLayers > 0 then for i := 0 to numLayers - 1 do
    drawLayers[i].totalRootWeightPerUnitSoilThickness_kg :=
      safediv(drawLayers[i].totalRootWeight_kg, toCurrentUnit(kItemDepthUnit, drawLayers[i].thickness_m));
  except
    for i := 0 to numLayers - 1 do drawLayers[i].totalRootWeightPerUnitSoilThickness_kg := 0.0;
    errorMessage('Problem in KfSoilDrawComponent.calculateRootWeightsPerCurrentUnitOfSoil');
  end;
  end;

procedure KfSoilDrawComponent.updateColors;
  var
    i: integer;
  begin
  if soil = nil then exit;
  self.baseColor := soil.params.baseDrawingColor;
  self.mulchColor := soil.params.mulchDrawingColor;
  if numLayers > 0 then for i := 0 to numLayers - 1 do
    drawLayers[i].color := soil.colorForSoilLayer(i);
  end;

procedure KfSoilDrawComponent.updateModel(newModel: GsModel);
	begin
  { ignore newModel in this case because soil patch is set differently }
  updateCurrentValue(-1);
  updateDisplay;
  repaint; { NOT refresh }
  end;

procedure KfSoilDrawComponent.updateDisplay;
  var
    i: integer;
    total, maxValue: single;
  begin
  if soil = nil then exit;
  if displayMode = kDisplayPH then exit;
  if displayMode = kDisplayTemperature then
    maxValue := -30.0
  else
    maxValue := 0.0;
  case displayMode of
    kDisplayTemperature:
      for i := 0 to self.numLayers - 1 do
        checkBoundsWithMargin(drawLayers[i].temperature_degC, displayMinValue, maxValue);
    kDisplayRootWeight:
      for i := 0 to self.numLayers - 1 do
        checkBoundsWithMargin(drawLayers[i].totalRootWeightPerUnitSoilThickness_kg, displayMinValue, maxValue);
    kDisplayNConc:
      for i := 0 to self.numLayers - 1 do
        begin
        with drawLayers[i] do
          total := nitrateN_gPt + ammoniaN_gPt + organicNFresh_gPt + organicNActiveHumus_gPt
            + organicNStableHumus_gPt;
        checkBoundsWithMargin(total, displayMinValue, maxValue);
        end;
    kDisplayPConc:
      for i := 0 to self.numLayers - 1 do
        begin
        with drawLayers[i] do
          total := labileP_gPt + organicPFresh_gPt + organicPHumus_gPt + mineralPActive_gPt
            + mineralPStable_gPt;
        checkBoundsWithMargin(total, displayMinValue, maxValue);
        end;
    end;
  if rescale then displayMaxValue := maxValue;
  displayMaxValue := max(maxValue, displayMaxValue);
  self.updateColors;
  rescale := false;
  self.invalidate;
  end;

procedure KfSoilDrawComponent.setDefaultOverlayUnit;
  begin
  case displayMode of
    kDisplayContents:
      begin
      displayUnitSet := kNonChangingUnitPercent;
      displayUnitModel := kDimensionless;
      displayUnitCurrent := kDimensionless;
      displayMinValue := 0.0;
      displayMaxValue := 100.0;
      end;
    kDisplayTemperature:
      begin
      displayUnitSet := kTemperature;
      displayUnitModel := kTemperatureDegreesC;
      if Domain.menuOptions.showMetricUnits then
        displayUnitCurrent := kTemperatureDegreesC
      else
        displayUnitCurrent := kTemperatureDegreesF;
      displayMinValue := 30.0;
      displayMaxValue := -30.0;
      end;
    kDisplayRootWeight:
      begin
      displayUnitSet := kMass;
      displayUnitModel := kMassKilograms;
      if Domain.menuOptions.showMetricUnits then
        displayUnitCurrent := kMassKilograms
      else
        displayUnitCurrent := kMassPounds;
      displayMinValue := 0.0;
      displayMaxValue := 0.0;
      end;
    kDisplayPH:
      begin
      displayUnitSet := kDimensionless;
      displayUnitModel := kDimensionless;
      displayUnitCurrent := kDimensionless;
      displayMinValue := 4.0;
      displayMaxValue := 9.0;  { check }
      end;
    kDisplayNConc, kDisplayPConc:
      begin
      displayUnitSet := kConcentration;
      displayUnitModel := kConcentrationGramsPMetricTon;
      if Domain.menuOptions.showMetricUnits then
        displayUnitCurrent := kConcentrationGramsPMetricTon
      else
        displayUnitCurrent := kConcentrationOuncesPEnglishTon;
      displayMinValue := 0.0;
      displayMaxValue := 0.0;
      end;
    end;
  labelText := 'Soil Profile: ' + displayModeName;
  self.updateDisplay;
  end;

procedure KfSoilDrawComponent.resizeElements;
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

const kMinSnowDisplayDepth_m = 0.05; { start with max of 50 mm snow }

procedure KfSoilDrawComponent.paint;
  var
    fullRect, minDepthRect, maxDepthRect, minOverlayRect, maxOverlayRect, labelRect: TRect;
    minDepthText, maxDepthText, minOverlayText, maxOverlayText: string[30];
    displayUnitText, depthUnitText: string[30];
    propHeightThisLayer, mulchDepth_m, snowDepth_m, totalDepth_m,
    minValueInCurrentUnit, maxValueInCurrentUnit, propHeightPossibleForSnow: single;
    i, pos, maxWidth: integer;
  begin
  if displayMode = kDisplayTexture then exit;
  fullRect := getClientRect;
  self.canvas.font := self.font;
  if self.textHeight = 0 then self.calculateTextDimensions;
  minDepthText := '0.0';
  if soil <> nil then
    maxDepthText := digitValueString(toCurrentUnit(kItemDepthUnit, soil.layers[soil.state.numLayers-1].depth_m))
  else
    maxDepthText := '2.0';
  depthUnitText := UnitStringForEnum(kLength, depthUnitCurrent);
  with profileRect do
    begin
    left := fullRect.left + kLeftRightGap;
    maxWidth := intMax(self.canvas.textWidth(minDepthText),
      intMax(self.canvas.textWidth(maxDepthText), self.canvas.textWidth(depthUnitText)));
    right := fullRect.right - kLeftRightGap - maxWidth - kLeftRightGap;
    top := fullRect.top + self.collapsedHeight + kTopBottomGap;
    bottom := fullRect.bottom - kTopBottomGap;
    { room for min, unit, max on bottom }
    if displayMode <> kDisplayColor then
      bottom := bottom - self.textHeight - kTopBottomGap;
    { room for legends }
    case displayMode of
      kDisplayColor: 
        bottom := bottom - self.textHeight * 2 - kTopBottomGap * 2;
      kDisplayContents:
        bottom := bottom - self.textHeight * 4 - kTopBottomGap * 4;
      kDisplayNConc, kDisplayPConc:
        bottom := bottom - self.textHeight * 3 - kTopBottomGap * 3;
      kDisplayTemperature, kDisplayPH, kDisplayRootWeight:
        bottom := bottom - self.textHeight - kTopBottomGap;
      end;
    end;
  with labelRect do
    begin
    left := profileRect.left; right := left + self.canvas.textWidth(labelText);
    bottom := profileRect.top - kTopBottomGap; top := bottom - self.textHeight;
    end;
  pos := profileRect.top;
  mulchDepth_m := soil.mulch.depth_m;
  snowDepth_m := soil.snowDepthFromSnowWaterContent_m;
  if displayMode = kDisplayColor then
    begin
    totalDepth_m := max(kMinSnowDisplayDepth_m, snowDepth_m) + mulchDepth_m + soil.layers[soil.state.numLayers-1].depth_m;
    snowRect := profileRect;
    with snowRect do
      begin
      { possible size first }
      try
        propHeightPossibleForSnow := safediv(max(kMinSnowDisplayDepth_m, snowDepth_m), totalDepth_m);
      except
        propHeightPossibleForSnow := 0.0;
      end;
      top := pos;
      bottom := top + round(propHeightPossibleForSnow * (profileRect.bottom - profileRect.top));
      pos := pos + (bottom - top);
      { now real size }
      try
        propHeightThisLayer := safediv(snowDepth_m, totalDepth_m);
      except
        propHeightThisLayer := 0.0;
      end;
      top := bottom - round(propHeightThisLayer * (profileRect.bottom - profileRect.top));
      end;
    mulchRect := profileRect;
    with mulchRect do
      begin
      try
        propHeightThisLayer := safediv(mulchDepth_m, totalDepth_m);
      except
        propHeightThisLayer := 0.0;
      end;
      top := pos;
      bottom := top + round(propHeightThisLayer * (profileRect.bottom - profileRect.top));
      if bottom > profileRect.bottom then bottom := profileRect.bottom;
      pos := pos + (bottom - top);
      end;
    end
  else
    begin
    totalDepth_m := soil.layers[soil.state.numLayers-1].depth_m;
    snowRect := rect(0, 0, 0, 0);
    mulchRect := rect(0, 0, 0, 0);
    end;
  for i := 0 to self.numLayers - 1 do
    begin
    layerRects[i] := profileRect;
    with layerRects[i] do
      begin
      try
        propHeightThisLayer := safediv(drawLayers[i].thickness_m, totalDepth_m);
      except
        propHeightThisLayer := 0.0;
      end;
      top := pos;
      bottom := top + round(propHeightThisLayer * (profileRect.bottom - profileRect.top));
      if i = self.numLayers - 1 then bottom := profileRect.bottom;
      if bottom > profileRect.bottom then bottom := profileRect.bottom;
      pos := pos + (bottom - top);
      end;
    end;
  with minDepthRect do
    begin
    left := profileRect.right + kLeftRightGap; right := left + self.canvas.textWidth(minDepthText);
    top := layerRects[0].top; { was profileRect.top; moved to put below mulch }
    bottom := top + self.textHeight;
    end;
  with maxDepthRect do
    begin
    left := profileRect.right + kLeftRightGap; right := left + self.canvas.textWidth(maxDepthText);
    bottom := profileRect.bottom; top := bottom - self.textHeight;
    end;
  with depthUnitRect do
    begin
    left := profileRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth(depthUnitText);
    top := profileRect.top + (profileRect.bottom - profileRect.top) div 2 - self.textHeight div 2;
    bottom := top + self.textHeight;
    end;
  if displayMode <> kDisplayColor then
    begin
    minValueInCurrentUnit := toCurrentUnit(kItemDisplayUnit, displayMinValue);
    maxValueInCurrentUnit := toCurrentUnit(kItemDisplayUnit, displayMaxValue);
    minOverlayText := digitValueString(minValueInCurrentUnit);
    maxOverlayText := digitValueString(maxValueInCurrentUnit);
    displayUnitText := UnitStringForEnum(displayUnitSet, displayUnitCurrent);
    with minOverlayRect do
      begin
      left := profileRect.left; right := left + self.canvas.textWidth(minOverlayText);
      top := profileRect.bottom + kTopBottomGap; bottom := top + self.textHeight;
      end;
    with maxOverlayRect do
      begin
      right := profileRect.right; left := right - self.canvas.textWidth(maxOverlayText);
      top := profileRect.bottom + kTopBottomGap; bottom := top + self.textHeight;
      end;
    with displayUnitRect do
      begin
      left := profileRect.left + (profileRect.right - profileRect.left) div 2
        - self.canvas.textWidth(displayUnitText) div 2;
      right := left + self.canvas.textWidth(displayUnitText);
      top := profileRect.bottom + kTopBottomGap;
      bottom := top + self.textHeight;
      end;
    end
  else
    displayUnitRect := rect(0, 0, 0, 0);
  profile.canvas.brush.color := clBtnFace;
  profile.canvas.rectangle(0, 0, profile.width, profile.height);
  self.drawTextLabel(labelText, labelRect, false, false);
  self.drawProfile;
  profile.canvas.brush.style := bsClear;
  profile.canvas.pen.color := clBlack;
  if displayMode <> kDisplayColor then with profileRect do
    begin
    profile.canvas.moveTo(left, top);
    profile.canvas.lineTo(right, top);
    profile.canvas.lineTo(right, bottom);
    profile.canvas.lineTo(left, bottom);
    profile.canvas.lineTo(left, top);
    end;
  self.drawLegend;
  self.drawTextLabel(minDepthText, minDepthRect, false, false);
  self.drawTextLabel(maxDepthText, maxDepthRect, false, false);
  self.drawTextLabel(depthUnitText, depthUnitRect, true, (self.selectedItemIndex = kItemDepthUnit));
  if displayMode <> kDisplayColor then
    begin
    self.drawTextLabel(minOverlayText, minOverlayRect, false, false);
    self.drawTextLabel(maxOverlayText, maxOverlayRect, false, false);
    self.drawTextLabel(displayUnitText, displayUnitRect, true, (self.selectedItemIndex = kItemDisplayUnit));
    end;
  self.canvas.draw(0, 0, profile);
  end;

const kLegendColorSize = 15;

procedure KfSoilDrawComponent.drawTextLabel(text: string; drawRect: TRect; drawLine, drawBox: boolean);
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

procedure KfSoilDrawComponent.drawLegend;
  var
    i, pos, numVars, squaresInColumn: integer;
    stringRects: array[0..7] of TRect;
    legendStrings: array[0..7] of string[50];
    legendColors: array[0..7] of TColorRef;
  begin
  self.canvas.pen.color := clBlack;
  case displayMode of
    kDisplayColor:
      begin
      numVars := 2;
      legendStrings[0] := 'Soil base color (dry, low organic matter soil)';
      legendColors[0] := self.baseColor;
      legendStrings[1] := 'Mulch color';
      legendColors[1] := self.mulchColor;
      end;
    kDisplayTemperature:
      begin
      numVars := 1;
      legendStrings[0] := 'Temperature';
      legendColors[0] := Domain.soilDrawColors.temperatureColor;
      end;
    kDisplayRootWeight:
      begin
      numVars := 1;
      legendStrings[0] := 'Root weight';
      legendColors[0] := Domain.soilDrawColors.rootWeightColor;
      end;
    kDisplayPH:
      begin
      numVars := 1;
      legendStrings[0] := 'pH';
      legendColors[0] := Domain.soilDrawColors.pHColor;
      end;
    kDisplayContents:
      begin
      numVars := 7;
      legendStrings[0] := 'Rocks';
      legendStrings[1] := 'Organic matter';
      legendStrings[2] := 'Sand';
      legendStrings[3] := 'Silt';
      legendStrings[4] := 'Clay';
      legendStrings[5] := 'Water';
      legendStrings[6] := 'Air';
      { root color is not in array because it was added later (overwrote pesticide color) }
      legendColors[0] := Domain.soilDrawColors.rootMaterialColor;
      for i := 1 to numVars - 1 do legendColors[i] := Domain.soilDrawColors.materialsColors[i-1];
      end;
    kDisplayNConc:
      begin
      numVars := 5;
      legendStrings[0] := 'Nitrate N';
      legendStrings[1] := 'Ammonia N';
      legendStrings[2] := 'Residue/microbial N';
      legendStrings[3] := 'Active humus N';
      legendStrings[4] := 'Stable humus N'; 
      for i := 0 to numVars - 1 do legendColors[i] := Domain.soilDrawColors.nitrogenColors[i];
      end;
    kDisplayPConc:
      begin
      numVars := 5;
      legendStrings[0] := 'Soluble P';
      legendStrings[1] := 'Residue/microbial P';
      legendStrings[2] := 'Humus P';
      legendStrings[3] := 'Active mineral P';
      legendStrings[4] := 'Stable mineral P';
      for i := 0 to numVars - 1 do legendColors[i] := Domain.soilDrawColors.phosphorusColors[i];
      end;
    else
      begin {default these if programming error just in case}
      numVars := 1;
      legendStrings[0] := '';
      legendColors[0] := Domain.soilDrawColors.temperatureColor;
      end;
    end;
  { leave space for min, unit, max }
  if displayMode = kDisplayColor then
    pos := profileRect.bottom + kTopBottomGap
  else
    pos := profileRect.bottom + self.textHeight + kTopBottomGap * 2;
  for i := 0 to 7 do legendColorRects[i] := rect(0, 0, 0, 0);
  squaresInColumn := 3;
  if numVars = 7 then squaresInColumn := 4;
  for i := 0 to numVars - 1 do
    begin
    with legendColorRects[i] do
      begin
      if i < squaresInColumn then
        left := profileRect.left
      else
        left := profileRect.left + (profileRect.right - profileRect.left) div 2;
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
    if i = squaresInColumn - 1 then pos := profileRect.bottom + kTopBottomGap * 2 + self.textHeight;
    with profile.canvas do
      begin
      brush.color := legendColors[i];
      rectangle(legendColorRects[i].left, legendColorRects[i].top, legendColorRects[i].right, legendColorRects[i].bottom);
      brush.style := bsClear;
      end;
    self.drawTextLabel(legendStrings[i], stringRects[i], false, false);
    end;
  end;

procedure KfSoilDrawComponent.drawProfile;
  var i: integer;
  begin
  if (self.numLayers <= 0) then exit;
  profile.canvas.pen.color := clBlack;
  case displayMode of
    kDisplayColor:
      begin
      with profile.canvas do
        begin
        brush.color := clWhite;
        fillRect(snowRect);
        brush.color := mulchColor;
        fillRect(mulchRect);
        for i := 0 to numLayers - 1 do
          begin
          brush.color := drawLayers[i].color;
          fillRect(layerRects[i]);
          brush.style := bsClear;
          end;
        end;
      end;
    kDisplayContents: drawProfileWithContents;
    kDisplayTemperature, kDisplayPH: drawProfileWithTemperatureOrPH;
    kDisplayRootWeight: drawProfileWithRootWeight;
    kDisplayNConc: drawProfileWithNitrogenConcentration;
    kDisplayPConc: drawProfileWithPhosphorusConcentration;
    end;
  if drawLines then with profile.canvas do
    begin
    moveTo(snowRect.left, snowRect.bottom);
    lineTo(snowRect.right, snowRect.bottom);
    moveTo(mulchRect.left, mulchRect.bottom);
    lineTo(mulchRect.right, mulchRect.bottom);
    { don't draw line at bottom of last layer - is end of profile rect }
    for i := 0 to numLayers - 2 do
      begin
      moveTo(layerRects[i].left, layerRects[i].bottom);
      lineTo(layerRects[i].right, layerRects[i].bottom);
      end;
    end;
  end;

procedure KfSoilDrawComponent.drawProfileWithContents;
  var
    i, variable, distance: integer;
    cumTotals: array[0..9] of single;
    polygonPoints: polygonPointsArray;
  begin
  for variable := 6 downto 0 do
    begin
    if variable > 0 then
      profile.canvas.brush.color := Domain.soilDrawColors.materialsColors[variable-1]
    else
      { root color is not in array because it was added later (overwrote pesticide color) }
      profile.canvas.brush.color := Domain.soilDrawColors.rootMaterialColor;
    for i := 0 to 14 do polygonPoints[i] := point(profileRect.left, profileRect.top);
    for i := 0 to self.numLayers - 1 do
      begin
      case variable of
        { last var is what is left over }
        0: cumTotals[i] := cumTotals[i] - drawLayers[i].percentOM;
        1: cumTotals[i] := cumTotals[i] - drawLayers[i].percentSand;
        2: cumTotals[i] := cumTotals[i] - drawLayers[i].percentSilt;
        3: cumTotals[i] := cumTotals[i] - drawLayers[i].percentClay;
        4: cumTotals[i] := cumTotals[i] - drawLayers[i].percentWater;
        5: cumTotals[i] := cumTotals[i] - drawLayers[i].percentAir;
        6: cumTotals[i] := 100.0;
        end;
      distance := profileRect.left + round(cumTotals[i] / 100.0 * (profileRect.right - profileRect.left));
      if graphBars then
        drawBar(i, distance)
      else
        placePolygonPoint(i, distance, polygonPoints);
      end;
    if not graphBars then profile.canvas.polygon(polygonPoints);
    profile.canvas.brush.style := bsClear;
    profile.canvas.brush.color := clBtnFace;
    end;
  end;

procedure KfSoilDrawComponent.drawProfileWithTemperatureOrPH;
  var
    i, distance: integer;
    value, proportion: single;
  begin
  case displayMode of
    kDisplayTemperature: profile.canvas.pen.color := Domain.soilDrawColors.temperatureColor;
    kDisplayPH: profile.canvas.pen.color := Domain.soilDrawColors.pHColor;
    end;
  profile.canvas.brush.color := profile.canvas.pen.color;
  for i := 0 to self.numLayers - 1 do
    begin
    case displayMode of
      kDisplayTemperature: value := drawLayers[i].temperature_degC;
      kDisplayPH: value := drawLayers[i].pH;
      else value := 0.0;
      end;
    if displayMaxValue - displayMinValue <> 0.0 then
      proportion := safediv(value - displayMinValue, displayMaxValue - displayMinValue)
    else
      proportion := 0.0;
    distance := profileRect.left + round(proportion * (profileRect.right - profileRect.left));
    if graphBars then
      drawBar(i, distance)
    else
      drawLinePoint(i, distance);
    end;
  profile.canvas.pen.color := clBlack;
  end;

procedure KfSoilDrawComponent.drawProfileWithRootWeight;
  var
    i, distance: integer;
    polygonPoints: polygonPointsArray;
    proportion: single;
  begin
  profile.canvas.pen.color := Domain.soilDrawColors.rootWeightColor;
  profile.canvas.brush.color := profile.canvas.pen.color;
  for i := 0 to 14 do polygonPoints[i] := point(profileRect.left, profileRect.top);
  for i := 0 to self.numLayers - 1 do
    begin
    if displayMaxValue - displayMinValue <> 0.0 then
      proportion := safediv(drawLayers[i].totalRootWeightPerUnitSoilThickness_kg - displayMinValue,
        displayMaxValue - displayMinValue)
    else
      proportion := 0.0;
    distance := profileRect.left + round(proportion * (profileRect.right - profileRect.left));
    if graphBars then
      drawBar(i, distance)
    else
      drawLinePoint(i, distance);
    end;
  profile.canvas.pen.color := clBlack;
  end;

procedure KfSoilDrawComponent.drawProfileWithNitrogenConcentration;
  var
    i, variable, distance: integer;
    polygonPoints: polygonPointsArray;
    amount, proportion: single;
  begin
  for variable := 4 downto 0 do
    begin
    profile.canvas.brush.color := Domain.soilDrawColors.nitrogenColors[variable];
    for i := 0 to 14 do polygonPoints[i] := point(profileRect.left, profileRect.top);
    for i := 0 to self.numLayers - 1 do
      begin
      with drawLayers[i] do
        case variable of
          0: amount := nitrateN_gPt;
          1: amount := nitrateN_gPt + ammoniaN_gPt;
          2: amount := nitrateN_gPt + ammoniaN_gPt + organicNFresh_gPt;
          3: amount := nitrateN_gPt + ammoniaN_gPt + organicNFresh_gPt + organicNActiveHumus_gPt;
          4: amount := nitrateN_gPt + ammoniaN_gPt + organicNFresh_gPt + organicNActiveHumus_gPt
            + organicNStableHumus_gPt;
          else amount := 0.0;
          end;
      if displayMaxValue - displayMinValue <> 0.0 then
        proportion := safediv(amount - displayMinValue, displayMaxValue - displayMinValue)
      else
        proportion := 0.0;
      distance := profileRect.left + round(proportion * (profileRect.right - profileRect.left));
      if graphBars then
        drawBar(i, distance)
      else
        placePolygonPoint(i, distance, polygonPoints);
      end;
    if not graphBars then profile.canvas.polygon(polygonPoints);
    profile.canvas.brush.style := bsClear;
    profile.canvas.brush.color := clBtnFace;
    end;
  end;

procedure KfSoilDrawComponent.drawProfileWithPhosphorusConcentration;
  var
    i, variable, distance: integer;
    polygonPoints: polygonPointsArray;
    amount, proportion: single;
  begin
  for variable := 4 downto 0 do
    begin
    profile.canvas.brush.color := Domain.soilDrawColors.phosphorusColors[variable];
    for i := 0 to 14 do polygonPoints[i] := point(profileRect.left, profileRect.top);
    for i := 0 to self.numLayers - 1 do
      begin
      with drawLayers[i] do
        case variable of
          0: amount := labileP_gPt;
          1: amount := labileP_gPt + organicPFresh_gPt;
          2: amount := labileP_gPt + organicPFresh_gPt + organicPHumus_gPt;
          3: amount := labileP_gPt + organicPFresh_gPt + organicPHumus_gPt + mineralPActive_gPt;
          4: amount := labileP_gPt + organicPFresh_gPt + organicPHumus_gPt + mineralPActive_gPt
            + mineralPStable_gPt;
          else amount := 0.0;
          end;
      if displayMaxValue - displayMinValue <> 0.0 then
        proportion := safediv(amount - displayMinValue, displayMaxValue - displayMinValue)
      else
        proportion := 0.0;
      distance := profileRect.left + round(proportion * (profileRect.right - profileRect.left));
      if graphBars then
        drawBar(i, distance)
      else
        placePolygonPoint(i, distance, polygonPoints);
      end;
    if not graphBars then profile.canvas.polygon(polygonPoints);
    profile.canvas.brush.style := bsClear;
    profile.canvas.brush.color := clBtnFace;
    end;
  end;

procedure KfSoilDrawComponent.placePolygonPoint(i: integer; distance: integer; var polygonPoints: polygonPointsArray);
  begin
  if i = 0 then polygonPoints[i+1] := point(distance, profileRect.top);
  polygonPoints[i+2] := point(distance, (layerRects[i].top + layerRects[i].bottom) div 2);
  if i = numLayers - 1 then
     begin
     polygonPoints[i+3] := point(distance, profileRect.bottom);
     polygonPoints[i+4] := point(profileRect.left, profileRect.bottom);
     end;
 end;

procedure KfSoilDrawComponent.drawLinePoint(i, distance: integer);
  begin
  if i = 0 then profile.canvas.moveTo(distance, profileRect.top);
  profile.canvas.lineTo(distance, (layerRects[i].top + layerRects[i].bottom) div 2);
  if i = numLayers - 1 then profile.canvas.lineTo(distance, profileRect.bottom);
  end;

procedure KfSoilDrawComponent.drawBar(layerIndex, distance: integer);
  var
    drawRect: TRect;
  begin
  { assumes multiple things draw from right to left, overlaying }
  drawRect := Rect(profileRect.left,
    layerRects[layerIndex].top, distance, layerRects[layerIndex].bottom);
  with profile.canvas do
    begin
    fillRect(drawRect);
    pen.color := clBlack;
    pen.width := 1;
    pen.style := psSolid;
    moveTo(profileRect.left, layerRects[layerIndex].top);
    lineTo(profileRect.left, layerRects[layerIndex].bottom);
    moveTo(distance, layerRects[layerIndex].top);
    lineTo(distance, layerRects[layerIndex].bottom);
    end;
  end;

procedure KfSoilDrawComponent.zeroLayerFields(i: integer);
  begin
  with drawLayers[i] do
    begin
    thickness_m := 0.0;
    color := self.baseColor;
    percentAir := 0.0;
    percentWater := 0.0;
    percentClay := 0.0;
    percentSilt := 0.0;
    percentSand := 0.0;
    percentOM := 0.0;
    temperature_degC := 0.0;
    totalRootWeight_kg := 0.0;
    totalRootWeightPerUnitSoilThickness_kg := 0.0;
    pH := 0.0;
    nitrateN_gPt := 0.0;
    ammoniaN_gPt := 0.0;
    organicNFresh_gPt := 0.0;
    organicNActiveHumus_gPt := 0.0;
    organicNStableHumus_gPt := 0.0;
    labileP_gPt := 0.0;
    organicPFresh_gPt := 0.0;
    organicPHumus_gPt := 0.0;
    mineralPActive_gPt := 0.0;
    mineralPStable_gPt := 0.0;
    end;
  end;

function KfSoilDrawComponent.toModelUnit(item: integer; value: single): single;
  begin
  if item = kItemDisplayUnit then
    result := Convert(displayUnitSet, displayUnitCurrent, displayUnitModel, value)
  else
    result := Convert(kLength, depthUnitCurrent, depthUnitModel, value);
  end;

function KfSoilDrawComponent.toCurrentUnit(item: integer; value: single): single;
  begin
  if item = kItemDisplayUnit then
    result := Convert(displayUnitSet, displayUnitModel, displayUnitCurrent, value)
  else
    result := Convert(kLength, depthUnitModel, depthUnitCurrent, value);
  end;

end.
 
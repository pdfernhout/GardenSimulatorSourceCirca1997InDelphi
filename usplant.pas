unit Usplant;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
usplant: Plant statistics drawing component. Used on browser right (pictures) side.
This is used to draw the plant biomasses (called 'statistics' here) and the plant
growth stresses. Uses a bitmap to double-buffer. Keeps pointer to plant; otherwise
is like other right-side descendants of browser components (see ubrowtri for explanation
of these descendants). Plant statistics are gathered by the drawing plant using the
function getDrawingPlantStatistics which fills the statistics record with information
by traversing the drawing plant (in a statistics-gathering traversal mode). Plant stress
information is gotten directly from the model plant. Note that in the browser the
plant display options are 'drawing', 'biomass' or 'stresses'. This panel is visible when
either 'biomass' or 'stresses' is selected, and hidden when 'drawing' is selected.}

interface

uses WinTypes, WinProcs, Controls, ExtCtrls, Graphics, Forms, Classes, ubrowcom, uesoil, umodel,
  ueplant, udplant, uharvprt;

const
  kDisplayStatistics = 0;
  kDisplayStresses = 1;
  kItemPercent = 1;
  kItemUnit = 2;

type

KfPlantStatisticsDrawComponent = class(KfBrowserComponent)
	public
  drawBitmap: TBitmap;
  plant: GsPlant;
  statistics: GsDrawingPlantStatistics;
  displayMode: integer;
  maxValue: single;
  minValue: single;
  unitSet: integer;
  unitModel: integer;
  unitCurrent: integer;
  unitRect, graphRect: TRect;
  labelRects: array[0..kStatisticsPartTypeLast] of TRect;
  biomassGrowthConstraint, tempStress, waterStress, aerationStress, nStress, pStress: single;
  percentRect: TRect;
  showPercents: boolean;
  constructor create(anOwner: TComponent); override;
  destructor destroy; override;
  procedure initialize; override;
  procedure updatePlant(newPlant: GsPlant);
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateModel(newModel: GsModel); override;
  procedure updateDisplay; override;
  procedure paint; override;
  procedure resizeElements; override;
  procedure drawGraph;
  procedure drawBar(index: integer; amount_kg: single);
  function maxSelectedItemIndex: integer; override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  procedure collapseOrExpand(y: integer); override;
  function toModelUnit(value: single): single;
  function toCurrentUnit(value: single): single;
  procedure changeUnit(shift: boolean);
  procedure drawTextLabel(text: string; drawRect: TRect; active, drawLine, drawBox: boolean);
  function collapsedHeight: integer; override;
  function nameForPartType(partType: smallint; isSingular: boolean): string;
  function partTypeForIndex(partType: smallint): smallint;
  procedure drawStresses;
  procedure drawStressGraph;
  function nameForIndexOfLowestStress: string;
  function valueForStress(index: integer): single;
  function nameForStress(index: integer): string;
  end;

implementation

uses SysUtils, Dialogs, uunits, ueutils, ueqh, udomain, ubrowser, usupport, ugsim, ugscom,
  umconsts, udebug, uestruct;

constructor KfPlantStatisticsDrawComponent.create(anOwner: TComponent);
  begin
  inherited create(anOwner);
  self.model := nil;
  self.plant := nil;
  drawBitmap := TBitmap.create;
  statistics := GsDrawingPlantStatistics.create;
  self.parentFont := true;
  self.onMouseUp := self.doMouseUp;
  self.onKeyDown := self.doKeyDown;
  self.tabStop := true;
  self.enabled := true;
  self.initialize;
  self.selectedItemIndex := kItemNone;
  self.draggable := false;
  self.showPercents := false;
  end;

destructor KfPlantStatisticsDrawComponent.destroy;
  begin
  drawBitmap.free;
  drawBitmap := nil;
  statistics.free;
  statistics := nil;
  inherited destroy;
  end;

procedure KfPlantStatisticsDrawComponent.initialize;
  begin
  unitSet := kMass;
  unitModel := kMassKilograms;
  if GsDomain.default.menuOptions.showMetricUnits then
    unitCurrent := kMassGrams
  else
    unitCurrent := kMassOunces;
   displayMode := kDisplayStatistics;
   end;

function KfPlantStatisticsDrawComponent.maxSelectedItemIndex: integer;
  begin
  result := kItemUnit;
  end;

procedure KfPlantStatisticsDrawComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseUp(sender, button, shift, x, y);
  thePoint := Point(x, y);
  if ptInRect(unitRect, thePoint) then
    begin
    self.selectedItemIndex := kItemUnit;
    self.changeUnit(ssShift in shift);
    end
  else if ptInRect(percentRect, thePoint) and (displayMode = kDisplayStatistics) then
    begin
    self.selectedItemIndex := kItemPercent;
    showPercents := not showPercents;
    self.invalidate;
    end;
  end;

procedure KfPlantStatisticsDrawComponent.changeUnit(shift: boolean);
  begin
  if shift then
    unitCurrent := GetPreviousUnitEnumInUnitSet(unitSet, unitCurrent)
  else
    unitCurrent := GetNextUnitEnumInUnitSet(unitSet, unitCurrent);
  self.resizeElements;
  self.invalidate;
  end;

procedure KfPlantStatisticsDrawComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) then
    begin
    if (selectedItemIndex = kItemUnit) then
      self.changeUnit(ssShift in shift)
    else if (displayMode = kDisplayStatistics) then
      begin
      showPercents := not showPercents;
      self.invalidate;
      end;
    end;
  end;

function KfPlantStatisticsDrawComponent.collapsedHeight: integer;
  begin
  result := self.textHeight + kTopBottomGap;
  end;

procedure KfPlantStatisticsDrawComponent.collapseOrExpand(y: integer);
  begin
  { do nothing }
  end;

procedure KfPlantStatisticsDrawComponent.updatePlant(newPlant: GsPlant);
  begin
  if plant <> newPlant then
  	begin
    plant := newPlant;
    updateCurrentValue(0);
    end;
  end;

procedure KfPlantStatisticsDrawComponent.updateModelValues;
  begin
  updateCurrentValue(0);
  repaint; { NOT refresh }
  end;

procedure KfPlantStatisticsDrawComponent.updateCurrentValue;
  var
    tPha_to_kg: single;
  begin
  statistics.zeroAllFields;
  self.enabled := false;
  if plant = nil then exit;
  if plant.drawingPlant = nil then
    raise Exception.create('KfPlantStatisticsDrawComponent.updateCurrentValue: nil drawing plant');
  if (not plant.isTemplate) and (plant.soil = nil) then
    raise Exception.create('KfPlantStatisticsDrawComponent.updateCurrentValue: nil soil patch');
  plant.drawingPlant.getDrawingPlantStatistics(statistics);
  if not plant.isTemplate then
    tPha_to_kg := t_to_kg * plant.soil.params.area_ha
  else
    tPha_to_kg := t_to_kg * 1.0 * m2_to_ha;
  with statistics do
    begin
    liveBiomass_kg[kHarvestPartTypeWholePlant] := plant.biomass.totalLive_tPha * tPha_to_kg;
    deadBiomass_kg[kHarvestPartTypeWholePlant] := plant.biomass.standingDead_tPha * tPha_to_kg;
    liveBiomass_kg[kHarvestPartTypeRoot] := plant.biomass.totalRootWeight_tPha * tPha_to_kg;
    liveBiomass_kg[kHarvestPartTypeStorageOrgan] := plant.biomass.storageOrgan_tPha * tPha_to_kg;
    end;
  { above-ground stresses }
  tempStress := plant.constraints.biomassTempStressFactor_frn;
  aerationStress := plant.constraints.aerationStressFactor_frn;
  waterStress := plant.constraints.waterStressFactor_frn;
  nStress := plant.constraints.nStressFactor_frn;
  pStress := plant.constraints.pStressFactor_frn;
  biomassGrowthConstraint := plant.constraints.biomassGrowthConstraint_frn;
  self.enabled := true;
  self.updateDisplay;
  end;

procedure KfPlantStatisticsDrawComponent.updateModel(newModel: GsModel);
	begin
  { ignore newModel in this case because plant is set differently }
  updateCurrentValue(-1);
  repaint; { NOT refresh }
  end;

procedure KfPlantStatisticsDrawComponent.updateDisplay;
  var
    i, partType: integer;
  begin
  if plant = nil then exit;
  maxValue := 0.0;
  minValue := 100000.0;
  for i := 0 to kStatisticsPartTypeLast do
    with statistics do
      begin
      partType := self.partTypeForIndex(i);
      checkBoundsWithMargin(liveBiomass_kg[partType] + deadBiomass_kg[partType], minValue, maxValue);
      end;
  self.invalidate;
  end;

procedure KfPlantStatisticsDrawComponent.resizeElements;
  var
    fullRect: TRect;
  begin
  fullRect := getClientRect;
  with drawBitmap do
    begin
    width := fullRect.right - fullRect.left;
    height := fullRect.bottom - fullRect.top;
    end;
  self.updateDisplay;
  end;

procedure KfPlantStatisticsDrawComponent.paint;
  var
    fullRect, minRect, maxRect: TRect;
    minText, maxText, unitText, percentText: string[30];
    i, partType, topPos, maxWidth: integer;
    totalBiomassInCurrentUnit, liveAndDeadInWholePlant_kg, percentOfTotal: single;
    totalOfPercents: single;
{$ifdef WIN32}
    labelTexts: array[0..kStatisticsPartTypeLast] of ansistring;
{$else}
    labelTexts: array[0..kStatisticsPartTypeLast] of string;
{$endif}
    hasStuff: boolean;
  begin
  fullRect := getClientRect;
  self.canvas.font := self.font;
  if self.textHeight = 0 then self.calculateTextDimensions;
  if (displayMode = kDisplayStresses) then
    begin
    self.drawStresses;
    exit;
    end;
  minText := digitValueString(toCurrentUnit(minValue));
  maxText := digitValueString(toCurrentUnit(maxValue));
  unitText := UnitStringForEnum(unitSet, unitCurrent);
  if showPercents then percentText := 'percent' else percentText := 'biomass';
  totalOfPercents := 0.0;
  for i := 0 to kStatisticsPartTypeLast do
    with statistics do
      begin
      partType := self.partTypeForIndex(i);
      labelTexts[i] := '';
      { count }
      if count[partType] > 0 then
        labelTexts[i] := intToStr(count[partType]) + ' ';
      { name }
      appendStr(labelTexts[i], self.nameForPartType(partType, count[partType] = 1));
      totalBiomassInCurrentUnit := toCurrentUnit(liveBiomass_kg[partType] + deadBiomass_kg[partType]);
      liveAndDeadInWholePlant_kg :=
        liveBiomass_kg[kHarvestPartTypeWholePlant] + deadBiomass_kg[kHarvestPartTypeWholePlant];
      if (totalBiomassInCurrentUnit > 0) and (liveAndDeadInWholePlant_kg > 0) then
        begin
        { percent }
        if showPercents then
          begin
          percentOfTotal := 100.0 * (liveBiomass_kg[partType] + deadBiomass_kg[partType]) / liveAndDeadInWholePlant_kg;
          totalOfPercents := totalOfPercents + percentOfTotal;
          appendStr(labelTexts[i], ': ' + digitValueString(percentOfTotal) + '%');
          end
        else
          begin
          { biomass }
          appendStr(labelTexts[i], ': ' + digitValueString(totalBiomassInCurrentUnit) + ' ' + unitText);
          end;
        end;
    end;
  topPos := fullRect.top + kTopBottomGap;
  maxWidth := 0;
  for i := 0 to kStatisticsPartTypeLast do labelRects[i] := rect(0, 0, 0, 0);
  for i := 0 to kStatisticsPartTypeLast do with labelRects[i] do
      begin
      left := kLeftRightGap;
      right := left + self.canvas.textWidth(labelTexts[i]);
      if right - left > maxWidth then maxWidth := right - left;
      top := topPos;
      bottom := top + self.textHeight;
      topPos := topPos + (bottom - top);
      if right - left > maxWidth then maxWidth := right - left;
      end;
  { set all label widths equal to largest, so can right-justify them }
  for i := 0 to kStatisticsPartTypeLast do labelRects[i].right := labelRects[i].left + maxWidth;
  with graphRect do
    begin
    left := kLeftRightGap + maxWidth + kBetweenGap;
    right := fullRect.right - kLeftRightGap;
    top := fullRect.top + kTopBottomGap;
    bottom := fullRect.bottom - kTopBottomGap - self.textHeight - kBetweenGap;
    end;
  with minRect do
    begin
    left := graphRect.left;
    right := left + self.canvas.textWidth(minText);
    top := graphRect.bottom;
    bottom := top + self.textHeight;
    end;
  maxRect := minRect;
  with maxRect do
    begin
    right := graphRect.right;
    left := right - self.canvas.textWidth(maxText);
    end;
  unitRect := minRect;
  with unitRect do
    begin
    left := graphRect.left + (graphRect.right - graphRect.left) div 2 - self.canvas.textWidth(unitText) div 2;
    right := left + self.canvas.textWidth(unitText);
    end;
  percentRect := minRect;
  with percentRect do
    begin
    left := kLeftRightGap;
    right := left + self.canvas.textWidth(percentText);
    end;
  with drawBitmap.canvas do
    begin
    brush.color := clBtnFace;
    rectangle(0, 0, drawBitmap.width, drawBitmap.height);
    for i := 0 to kStatisticsPartTypeLast do
      if labelRects[i].bottom < graphRect.bottom then
        with statistics do
        begin
        partType := self.partTypeForIndex(i);
        hasStuff := (liveBiomass_kg[partType] + deadBiomass_kg[partType] > 0) or (count[partType] > 0);
        self.drawTextLabel(labelTexts[i], labelRects[i], hasStuff, false, false);
        end;
    self.drawGraph;
    brush.style := bsClear;
    pen.color := clBlack;
    rectangle(graphRect.left, graphRect.top, graphRect.right, graphRect.bottom);
    self.drawTextLabel(minText, minRect, true, false, false);
    self.drawTextLabel(maxText, maxRect, true, false, false);
    self.drawTextLabel(unitText, unitRect, true, true, (self.selectedItemIndex = kItemUnit));
    self.drawTextLabel(percentText, percentRect, true, true, (self.selectedItemIndex = kItemPercent));
    end;
  self.canvas.draw(0, 0, drawBitmap);
  end;

const
  kStressTemp = 0;
  kStressWater = 1;
  kStressAeration = 2;
  kStressN = 3;
  kStressP = 4;
  kStressLast = 4;

procedure KfPlantStatisticsDrawComponent.drawStresses;
  var
    fullRect, topRect, minRect, maxRect: TRect;
    topText: string[30];
    i, pos, maxWidth: integer;
{$ifdef WIN32}
    labelTexts: array[0..kStressLast] of ansistring;
{$else}
    labelTexts: array[0..kStressLast] of string;
{$endif}
    hasStuff: boolean;
  begin
  { set up strings }
  if plant.development.isDead then
    topText := 'Plant is dead'
  else if plant.development.lifeHistoryStage = kSeed then
    topText := 'Plant is seed'
  else
    topText := 'Limiting stress:  ' + self.nameForIndexOfLowestStress;
  for i := 0 to kStressLast do labelTexts[i] := self.nameForStress(i);
  { set up rectangles }
  fullRect := getClientRect;
  with topRect do
    begin
    left := kLeftRightGap;
    right := left + self.canvas.textWidth(topText);
    top := fullRect.top + kTopBottomGap;
    bottom := top + self.textHeight;
    end;
  maxWidth := 0;
  pos := fullRect.top + kTopBottomGap + self.textHeight + kBetweenGap;
  for i := 0 to kStatisticsPartTypeLast do labelRects[i] := rect(0, 0, 0, 0);
  for i := 0 to kStressLast do with labelRects[i] do
    begin
    left := kLeftRightGap;
    right := left + self.canvas.textWidth(labelTexts[i]);
    if right - left > maxWidth then maxWidth := right - left;
    top := pos;
    bottom := top + self.textHeight + kBetweenGap;
    pos := pos + (bottom - top);
    end;
  { set all label widths equal to largest, so can right-justify them }
  for i := 0 to kStressLast do labelRects[i].right := labelRects[i].left + maxWidth;
  with graphRect do
    begin
    left := kLeftRightGap + maxWidth + kBetweenGap;
    right := fullRect.right - kLeftRightGap;
    top := fullRect.top + kTopBottomGap + self.textHeight + kBetweenGap;
    bottom := fullRect.bottom - kTopBottomGap - self.textHeight - kBetweenGap;
    end;
  with minRect do
    begin
    left := graphRect.left;
    right := left + self.canvas.textWidth('0.0');
    top := graphRect.bottom;
    bottom := top + self.textHeight;
    end;
  with maxRect do
    begin
    right := graphRect.right;
    left := right - self.canvas.textWidth('1.0');
    top := graphRect.bottom;
    bottom := top + self.textHeight;
    end;
  with drawBitmap.canvas do
    begin
    brush.color := clBtnFace;
    rectangle(0, 0, drawBitmap.width, drawBitmap.height);
    self.drawTextLabel(topText, topRect, true, false, false);
    for i := 0 to kStressLast do
      if labelRects[i].bottom <= graphRect.bottom then
        self.drawTextLabel(labelTexts[i], labelRects[i], true, false, false);
    if (not plant.development.isDead) and (plant.development.lifeHistoryStage <> kSeed) then
      self.drawStressGraph;
    self.drawTextLabel('0.0', minRect, true, false, false);
    self.drawTextLabel('1.0', maxRect, true, false, false);
    brush.style := bsClear;
    pen.color := clBlack;
    with graphRect do rectangle(left, top, right, bottom);
    end;
  self.canvas.draw(0, 0, drawBitmap);
  end;

function KfPlantStatisticsDrawComponent.nameForStress(index: integer): string;
  begin
  result := '';
  case index of
    kStressTemp: result := 'temperature';
    kStressWater: result := 'water';
    kStressAeration: result := 'aeration';
    kStressN: result := 'nitrogen';
    kStressP: result := 'phosphorus';
    end;
  end;

function KfPlantStatisticsDrawComponent.valueForStress(index: integer): single;
  begin
  result := 0.0;
  case index of
    kStressTemp: result := tempStress;
    kStressWater: result := waterStress;
    kStressAeration: result := aerationStress;
    kStressN: result := nStress;
    kStressP: result := pStress;
    end;
  end;

function KfPlantStatisticsDrawComponent.nameForIndexOfLowestStress: string;
  var i: integer;
  begin
  result := 'error';
  if biomassGrowthConstraint >= 1.0 then
    result := 'none'
  else
    for i := 0 to kStressLast do
      if valueForStress(i) <= biomassGrowthConstraint then
        begin
        result := nameForStress(i);
        exit;
        end;
  end;

procedure KfPlantStatisticsDrawComponent.drawStressGraph;
  var
    i, distance: longint;
    stressRect, otherRect: TRect;
  begin
  with drawBitmap.canvas do
    begin
    brush.style := bsSolid;
    pen.color := clBlack;
    pen.width := 1;
    pen.style := psSolid;
    for i := 0 to kStressLast do
      if labelRects[i].bottom < graphRect.bottom then
        begin
        distance := graphRect.left + round(self.valueForStress(i) * (graphRect.right - graphRect.left));
        stressRect := rect(graphRect.left, labelRects[i].top, distance, labelRects[i].bottom);
        otherRect := rect(distance, labelRects[i].top, graphRect.right, labelRects[i].bottom);
        brush.color := clLime;
        with stressRect do drawBitmap.canvas.rectangle(left, top, right, bottom);
        brush.color := clRed;
        with otherRect do drawBitmap.canvas.rectangle(left, top, right, bottom);
       end;
    brush.style := bsClear;
    end;
  end;

procedure KfPlantStatisticsDrawComponent.drawTextLabel(text: string; drawRect: TRect; active, drawLine, drawBox: boolean);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with drawBitmap.canvas do
    begin
    font := self.font;
    if active then
      font.color := clBtnText
    else
      font.color := clBtnShadow;
    strPCopy(cText, text);
    { changed DT_LEFT to DT_RIGHT to right-justify labels; ok because nothing else matters here }
    winprocs.drawText(handle, cText, strLen(cText), drawRect, DT_RIGHT);
    end;
  if drawLine then with drawBitmap.canvas do
    begin
    pen.color := clBtnShadow;
    moveTo(drawRect.left, drawRect.bottom - 2);
    lineTo(drawRect.right, drawRect.bottom - 2);
    end;
  if drawBox then with drawBitmap.canvas do
    begin
    pen.color := clBtnShadow;
    moveTo(drawRect.right + 2, drawRect.top);
    lineTo(drawRect.left - 2, drawRect.top);
    lineTo(drawRect.left - 2, drawRect.bottom - 2);
    lineTo(drawRect.right + 2, drawRect.bottom - 2);
    lineTo(drawRect.right + 2, drawRect.top);
    end;
  end;

procedure KfPlantStatisticsDrawComponent.drawGraph;
  var
    i, partType: longint;
  begin
  with drawBitmap.canvas do
    begin
    brush.style := bsSolid;
    pen.color := clBlack;
    pen.width := 1;
    pen.style := psSolid;
    for i := 0 to kStatisticsPartTypeLast do
      if labelRects[i].bottom < graphRect.bottom then
        begin
        partType := self.partTypeForIndex(i);
        { first draw all biomass }
        brush.color := clBlack;
        self.drawBar(i, statistics.liveBiomass_kg[partType] + statistics.deadBiomass_kg[partType]);
        { next draw only live biomass }
        brush.color := clLime;
        self.drawBar(i, statistics.liveBiomass_kg[partType]);
        end;
    brush.style := bsClear;
    end;
  end;

procedure KfPlantStatisticsDrawComponent.drawBar(index: integer; amount_kg: single);
  var
    proportion: single;
    distance: longint;
    drawRect: TRect;
  begin
  if maxValue - minValue <> 0 then
    proportion := safediv(amount_kg - minValue, maxValue - minValue)
  else
    proportion := 0.0;
  distance := graphRect.left + round(proportion * (graphRect.right - graphRect.left));
  drawRect := rect(graphRect.left, labelRects[index].top, distance, labelRects[index].bottom);
  with drawRect do drawBitmap.canvas.rectangle(left, top, right, bottom);
  end;

function KfPlantStatisticsDrawComponent.nameForPartType(partType: smallint; isSingular: boolean): string;
  begin
  result := '';
  case partType of
    kHarvestPartTypeWholePlant: result := 'whole plant';
    kHarvestPartTypeSeedlingLeaf:
      if isSingular then result := 'seedling leaf' else result := 'seedling leaves';
    kHarvestPartTypeLeaf:
      if isSingular then result := 'leaf' else result := 'leaves';
    kHarvestPartTypeFemaleInflorescence:
      if isSingular then result := 'female inflorescence' else result := 'female inflorescences';
    kHarvestPartTypeMaleInflorescence:
      if isSingular then result := 'male inflorescence' else result := 'male inflorescences';
    kHarvestPartTypeFemaleFlower:
      if isSingular then result := 'female flower' else result := 'female flowers';
    kHarvestPartTypeFemaleFlowerBud:
      if isSingular then result := 'female flower bud' else result := 'female flower buds';
    kHarvestPartTypeMaleFlower:
      if isSingular then result := 'male flower' else result := 'male flowers';
    kHarvestPartTypeMaleFlowerBud:
      if isSingular then result := 'male flower bud' else result := 'male flower buds';
    kHarvestPartTypeAxillaryBud:
      if isSingular then result := 'meristem' else result := 'meristems';
    kHarvestPartTypeFruit:
      if isSingular then result := 'ripe fruit' else result := 'ripe fruits';
    kHarvestPartTypeRoot: result := 'root';
    kHarvestPartTypeStem:
      if isSingular then result := 'stem internode' else result := 'stem internodes';
    kHarvestPartTypeStorageOrgan: result := 'storage organ';
    kStatisticsPartTypeUnripeFruit:
      if isSingular then result := 'unripe fruit' else result := 'unripe fruits';
    kStatisticsPartTypeFallenFruit:
      if isSingular then result := 'fallen fruit' else result := 'fallen fruits';
    kStatisticsPartTypeUnallocatedNewVegetativeBiomass_kg: result := 'unallocated vegetative';
    kStatisticsPartTypeUnremovedDeadVegetativeBiomass_kg: result := 'unremoved vegetative';
    kStatisticsPartTypeUnallocatedNewReproductiveBiomass_kg: result := 'unallocated reproductive';
    kStatisticsPartTypeUnremovedDeadReproductiveBiomass_kg: result := 'unremoved reproductive';
    kStatisticsPartTypeFallenFlower:
      if isSingular then result := 'fallen flower' else result := 'fallen flowers';
    kStatisticsPartTypeAllVegetative: result := 'vegetative parts';
    kStatisticsPartTypeAllReproductive: result := 'reproductive parts';
    kStatisticsPartTypeUnremovedStandingDeadBiomass_kg: result := 'unremoved standing dead';
  else
    result := '';
  end;
  end;

function KfPlantStatisticsDrawComponent.partTypeForIndex(partType: smallint): smallint;
  begin
  result := -1;
  case partType of
    0: result := kHarvestPartTypeWholePlant;
    1: result := kHarvestPartTypeRoot;
    2: result := kHarvestPartTypeStorageOrgan;
    3: result := kStatisticsPartTypeAllVegetative;
    4: result := kStatisticsPartTypeAllReproductive;
    5: result := kHarvestPartTypeAxillaryBud;
    6: result := kHarvestPartTypeStem;
    7: result := kHarvestPartTypeSeedlingLeaf;
    8: result := kHarvestPartTypeLeaf;
    9: result := kHarvestPartTypeFemaleInflorescence;
    10: result := kHarvestPartTypeFemaleFlowerBud;
    11: result := kHarvestPartTypeFemaleFlower;
    12: result := kHarvestPartTypeMaleInflorescence;
    13: result := kHarvestPartTypeMaleFlowerBud;
    14: result := kHarvestPartTypeMaleFlower;
    15: result := kStatisticsPartTypeFallenFlower;
    16: result := kStatisticsPartTypeUnripeFruit;
    17: result := kHarvestPartTypeFruit;
    18: result := kStatisticsPartTypeFallenFruit;
    19: result := kStatisticsPartTypeUnallocatedNewVegetativeBiomass_kg;
    20: result := kStatisticsPartTypeUnremovedDeadVegetativeBiomass_kg;
    21: result := kStatisticsPartTypeUnallocatedNewReproductiveBiomass_kg;
    22: result := kStatisticsPartTypeUnremovedDeadReproductiveBiomass_kg;
    23: result := kStatisticsPartTypeUnremovedStandingDeadBiomass_kg;
  else
    { called by paint method, so do not raise exception }
    result := 0;
  end;
  end;

function KfPlantStatisticsDrawComponent.toModelUnit(value: single): single;
  begin
  result := Convert(unitSet, unitCurrent, unitModel, value);
  end;

function KfPlantStatisticsDrawComponent.toCurrentUnit(value: single): single;
  begin
  result := Convert(unitSet, unitModel, unitCurrent, value);
  end;

end.

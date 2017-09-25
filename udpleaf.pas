unit Udpleaf;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udpleaf: Leaf object. Created by a meristem (when its internode is created). Has no
pointers to any plant parts (pointed to by internode). Fairly simple. Grows by
s curve. Draws using 3d object with scale based on biomass over optimal biomass.
If is first leaf on plant (cotyledon), draws using seedling leaf 3d object. Can
draw compound leaves (pinnate or palmate) using recursive method. Wilts based
on plant water stress for day.}

interface

uses WinProcs, WinTypes, Graphics, udppart, udplant, ufiler, uestruct, uharvprt;

type

GsLeaf = class(GsDrawingPlantPart)
  public
  sCurveParams: SCurveStructure;
  faceColor: TColorRef;
  backfaceColor: TColorRef;
  petioleColor: TColorRef;
  unused1: single;
  unused2: single;
  propFullSize: single;
  biomassAtCreation_kg: single;
  isSeedlingLeaf: boolean;
  class function NewWithPlantFractionOfOptimalSize(aPlant: GsDrawingPlant; aFraction: single): GsLeaf;
  procedure traverseActivity(mode: integer; traverser: GsTraverser); override;
  procedure checkIfHasAbscissed;
  destructor destroy; override;
  function isPhytomer: boolean; override;
  procedure blendColorsStrength(aColor: TColorRef; aStrength: single); override;
  procedure setColorsToParameters; override;
  procedure drawWithDirection(direction: single);
  procedure DrawLeafOrLeaflet(aScale: single);
  procedure drawCompoundLeafPinnate;
  procedure drawCompoundLeafInternode;
  procedure DrawCompoundLeafPetioletCount(scale: single; aCount: integer);
  procedure drawCompoundLeafPalmate;
  procedure initializeFractionOfOptimalSize(thePlant: GsDrawingPlant; aFraction: single);
  procedure nextDay; override;
  procedure report;
  function partType: integer; override;
  function harvestItemTemplate: GsHarvestItemTemplate; override;
  function harvestItemIncludeInWholePlantHarvestBiomass: boolean; override;
  procedure wiltLeaf;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  class function optimalInitialBiomass_kg(drawingPlant: GsDrawingPlant): single;
  end;

implementation

uses SysUtils, Dialogs, udebug, uturt3d, ueutils, ueplant, uclasses;

{ ---------------------------------------------------------------------------------------- leaf }
procedure GsLeaf.initializeFractionOfOptimalSize(thePlant: GsDrawingPlant; aFraction: single);
  begin
  try
  initialize(thePlant);
  isSeedlingLeaf := false; {Plant sets this from outside on first phytomer.}
  liveBiomass_kg := aFraction * GsLeaf.optimalInitialBiomass_kg(plant);
  deadBiomass_kg := 0.0;
  propFullSize := safedivExcept(liveBiomass_kg, plant.pLeaf.optimalBiomass_kg, 1.0);
  faceColor := plant.pLeaf.faceColor;
  backfaceColor := plant.pLeaf.backfaceColor;
  Utils_InitSCurveParam(sCurveParams, 1.0, 0.05, plant.pLeaf.minDaysToGrow, 0.95);
  except errorMessage('Problem in GsLeaf.initializeFractionOfOptimalSize'); end;
  end;

class function GsLeaf.optimalInitialBiomass_kg(drawingPlant: GsDrawingPlant): single;
  begin
  with drawingPlant.pLeaf do
    result := optimalFractionOfOptimalBiomassAtCreation_kg * optimalBiomass_kg;
  end;

procedure GsLeaf.nextDay;
  begin
  try
  inherited nextDay;
  self.checkIfHasAbscissed;
  except
    ErrorMessage('GsLeaf.nextDay: Problem in GsLeaf next day');
  end;
  end;

procedure GsLeaf.traverseActivity(mode: integer; traverser: GsTraverser);
  var
    propFullSizeWanted, biomassNeeded_kg, newBiomass_kg, biomassToRemove_kg: single;
    maxNewLeafBiomass_kg: single;
  begin
  if self.isRemoved and (mode <> kActivityStream) and (mode <> kActivityFree) then exit;
  with plant.pLeaf do
  begin
  try
  case mode of
    kActivityNone: ;
    kActivityNextDay: self.nextDay;
    kActivityDemandVegetative_kg:
      begin
      if self.age > maxDaysToGrow then
        begin
        self.biomassDemand_kg := 0.0;
        exit;
        end;
      propFullSizeWanted := min(1.0, scurve(age + 1, sCurveParams.c1, sCurveParams.c2));
      self.biomassDemand_kg := linearGrowthResult(self.liveBiomass_kg,
        propFullSizeWanted * optimalBiomass_kg, plant.pLeaf.minDaysToGrow);
      traverser.total := traverser.total + self.biomassDemand_kg;
      end;
    kActivityDemandReproductive_kg:;
      { no repro. demand}
    kActivityGrowVegetative: 
      begin
      if self.age > maxDaysToGrow then exit;
      newBiomass_kg := self.biomassDemand_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg + newBiomass_kg;
      self.propFullSize := Min(1.0, safediv(self.totalBiomass_kg, optimalBiomass_kg));
      end;
    kActivityGrowReproductive: ;
	    { no repro. growth}
    kActivityStartReproduction: ;
     { no response }
    kActivityFindPlantPartAtPosition:
      begin
      if pointsAreCloseEnough(traverser.point, position) then
        begin
        traverser.foundPlantPart := self;
        traverser.finished := true;
        end;
      end;
    kActivityFindAgeOfYoungestPhytomer: ;
      { not a phytomer }
    kActivityDraw: ;
      { phytomer will control drawing }
    kActivityReport:
      self.report;
    kActivityStream: ;
      {streaming will be done by internode}
    kActivityFree:
      { free will be called by phytomer };
    kActivityVegetativeBiomassThatCanBeRemoved_kg:
      traverser.total := traverser.total + self.liveBiomass_kg;
    kActivityRemoveVegetativeBiomass:
      begin
      biomassToRemove_kg := self.liveBiomass_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg - biomassToRemove_kg;
      self.deadBiomass_kg := self.deadBiomass_kg + biomassToRemove_kg;
      end;
    kActivityReproductiveBiomassThatCanBeRemoved_kg: { none };
    kActivityRemoveReproductiveBiomass: { none };
    kActivityAddMatureHarvestedPartsToList:
      begin
      if self.canBeHarvested then traverser.foundList.add(self);
      end;
    kActivityAddBundledPartsToList:
      begin
      if self.harvestItemIncludeInWholePlantHarvestBiomass then traverser.foundList.add(self);
      end;
    kActivityGatherStatistics:
      begin
      if self.isSeedlingLeaf then
        self.addToStatistics(traverser.statistics, kHarvestPartTypeSeedlingLeaf)
      else
        self.addToStatistics(traverser.statistics, kHarvestPartTypeLeaf);
      self.addToStatistics(traverser.statistics, kStatisticsPartTypeAllVegetative);
      end;
    kActivityStandingDeadBiomassThatCanBeRemoved_kg:
      inherited traverseActivity(mode, traverser);
    kActivityRemoveStandingDeadBiomass:
      inherited traverseActivity(mode, traverser);
    else
      raise Exception.create('unhandled mode for GsLeaf activity');
    end;
  except
    ErrorMessage('GsLeaf.traverseActivity: Problem in leaf traverseActivity');
  end;
  end;
  end;

procedure GsLeaf.checkIfHasAbscissed;
  begin
  { if enough biomass removed (parameter), absciss leaf or leaves }
  if (self.fractionLive < plant.pLeaf.fractionOfLiveBiomassWhenAbscisses_frn) and (not self.isRemoved) then
    self.isRemoved := true;
  end;

destructor GsLeaf.destroy;
	begin
  inherited destroy;
  end;

function GsLeaf.isPhytomer: boolean;
  begin
  result := false;
  end;

class function GsLeaf.NewWithPlantFractionOfOptimalSize(aPlant: GsDrawingPlant; aFraction: single): GsLeaf;
  begin
  result := create;
  result.initializeFractionOfOptimalSize(aPlant, aFraction);
  end;

procedure GsLeaf.BlendColorsStrength(aColor: TColorRef; aStrength: single);
  begin
  if aStrength <= 0.0 then exit;
  faceColor := blendColors(faceColor, aColor, aStrength);
  backfaceColor := blendColors(backfaceColor, aColor, aStrength);
  petioleColor := blendColors(petioleColor, aColor, aStrength);
  end;

procedure GsLeaf.setColorsToParameters;
  begin
  {Initialize leaf colors at those in plant parameters, before stresses are considered.}
  faceColor := plant.pLeaf.faceColor;
  backFaceColor := plant.pLeaf.backFaceColor;
  petioleColor := plant.pLeaf.petioleColor;
  end;

procedure GsLeaf.drawWithDirection(direction: single);
  var
    fractionOfOptimalBiomass, scale, length: single;
    width: single;
    angle: single;
  begin
try
  {Draw petiole, then leaf. If compound leaf, call drawCompoundLeaf instead.}
  if (plant.turtle = nil) then exit;
  if self.isRemoved then exit;
  if plant.needToRecalculateColors then self.calculateColors;
  try
  self.recordPositionFromTurtleIfDesired;
  except
  end;
  {First, draw petiole.}
  plant.turtle.push;
  if (direction = kDirectionRight) then
    plant.turtle.RotateX(128);   
  length := plant.pLeaf.petioleLengthAtOptimalBiomass_mm * propFullSize;
  if (isSeedlingLeaf) then length := length / 2;
  width := plant.pLeaf.petioleWidthAtOptimalBiomass_mm * propFullSize;
  {wiltPetiole;}
  angle := plant.pLeaf.petioleAngle;
  {Give petiole angle a little random sway.}
  if not GsPlant(plant.model).development.isDead then
    angle := angle + ((plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway);
  DrawLengthInPiecesWidthAngleColor(length, width, angle, petioleColor);
  {Second, draw leaf.}
  if isSeedlingLeaf then
    begin
    scale := (propFullSize * (plant.pSeedlingLeaf.scale / 100.0)) * 1.0;
    DrawLeafOrLeaflet(scale);
    end
  else
    begin
    if (plant.pLeaf.compoundNumLeaflets <= 1) or plant.turtle.drawOptions.simpleLeavesOnly then
      begin
      scale := self.propFullSize * plant.pLeaf.scaleAtOptimalBiomass / 100.0;
      DrawLeafOrLeaflet(scale);
      end
    else if (plant.pLeaf.compoundPinnateOrPalmate = kCompoundLeafPinnate) then
      drawCompoundLeafPinnate
    else
      drawCompoundLeafPalmate;
    end;
  plant.turtle.pop;
except
	on EDivByZero do ErrorMessage('GsLeaf.drawWithDirection:drawPetioleAndLeaf: EDivByZero');
	on EInvalidOp do ErrorMessage('GsLeaf.drawWithDirection:drawPetioleAndLeaf: EInvalidOp');
	on EOverFlow do ErrorMessage('GsLeaf.drawWithDirection:drawPetioleAndLeaf: EOverFlow');
	on EUnderFlow do ErrorMessage('GsLeaf.drawWithDirection:drawPetioleAndLeaf: EUnderFlow');
end;
  end;

procedure GsLeaf.wiltLeaf;
  var
    angle: integer;
  begin
  if (plant.turtle = nil) then exit;
  angle := round(abs(plant.turtle.angleX + 32) * plant.wiltingPercent / 100.0);
  if plant.turtle.angleX > -32 then
    angle := -angle;
  plant.turtle.rotateX(angle);
  end;

procedure GsLeaf.DrawLeafOrLeaflet(aScale: single);
  var
    turtle: KfTurtle;
  begin
  {Draw leaf only. If seedling leaf (on first phytomer), draw seedling leaf 3D object and colors instead.
    Wilt leaf according to water stress and age.}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  if isSeedlingLeaf then
    begin
    turtle.setLineWidth(1.0);
    turtle.drawingSurface.ForeColor := plant.pSeedlingLeaf.faceColor;
    turtle.drawingSurface.BackColor := plant.pSeedlingLeaf.backfaceColor;
    turtle.drawingSurface.LineColor := darkerColor(plant.pSeedlingLeaf.faceColor);
    end
  else
    begin
    turtle.setLineWidth(1.0);
    turtle.drawingSurface.ForeColor := faceColor;
    turtle.drawingSurface.BackColor := backfaceColor;
    turtle.drawingSurface.LineColor := darkerColor(faceColor);
    end;
  {this aligns the 3D object as stored in the file to the way it should draw on the plant}
  turtle.RotateX(64);   {flip over; in 3D designer you design the leaf from the underside}
  turtle.RotateZ(-64); {pull leaf up to plane of petiole (is perpendicular)}
  self.wiltLeaf;
  if isSeedlingLeaf then
    begin
    if plant.pSeedlingLeaf.object3D <> nil then
    	plant.pSeedlingLeaf.object3D.DrawWithTurtleScale(turtle, aScale);
    end
  else
    begin
    if plant.pLeaf.object3D <> nil then
    	plant.pLeaf.object3D.DrawWithTurtleScale(turtle, aScale);
    end;
  end;

procedure GsLeaf.drawCompoundLeafPinnate;
  var
    turtle: KfTurtle;
    scale: single;
  var i: integer;
  begin
  {Draw compound leaf. Use recursion structure we used to use for whole plant, with no branching.
    Leaflets decrease in size as you move up the leaf, simulating a gradual appearance of leaflets.
    Note that seedling leaves are never compound.}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  if plant.pLeaf.compoundNumLeaflets > 0 then
    for i := plant.pLeaf.compoundNumLeaflets downto 1 do
      begin
      if (i <> 1) then drawCompoundLeafInternode;
      turtle.push;
      scale := self.propFullSize * plant.pLeaf.scaleAtOptimalBiomass / 100.0
        * (1.0  - safediv(i * 1.0, plant.pLeaf.compoundNumLeaflets));
      DrawCompoundLeafPetioletCount(scale, i);
      DrawLeafOrLeaflet(scale);
      turtle.pop;
      end;
  end;

procedure GsLeaf.drawCompoundLeafInternode;
  var
    length, width, angleZ, angleY: single;
  begin
  {Draw internode of leaflet (portion of rachis). This is almost identical to drawing the petiole, etc,
   but a bit of random drift is included to make the compound leaf look more single.}
  length := plant.pLeaf.petioleLengthAtOptimalBiomass_mm * propFullSize;
  length := safediv(length, plant.pLeaf.compoundPetioleToRachisRatio);
  width := plant.pLeaf.petioleWidthAtOptimalBiomass_mm * propFullSize;
  angleZ := (plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway;
  angleY := (plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway;
  DrawLengthInPiecesWidthAngleZAngleYColor(length, width, angleZ, angleY, petioleColor);
  end;

procedure GsLeaf.DrawCompoundLeafPetioletCount(scale: single; aCount: integer);
  var
    length, width, angle: single;
  begin
  {Draw petiolet, which is the leaflet stem coming off the compound leaf rachis.}
  length := scale * plant.pLeaf.petioleLengthAtOptimalBiomass_mm * propFullSize;
  width := scale * plant.pLeaf.petioleWidthAtOptimalBiomass_mm * propFullSize;
  if (aCount = 1) then
    angle := 0
  else
    begin
    if (odd(aCount)) then
      angle := 32
    else
      angle := -32;
    end;
  {Give angle a little random sway.}
  if not GsPlant(plant.model).development.isDead then
    angle := angle + ((plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway);
  DrawLengthInPiecesWidthAngleZAngleYColor(length, width, 0, angle, petioleColor);
  end;

procedure GsLeaf.drawCompoundLeafPalmate;
  var
    turtle: KfTurtle;
    scale: single;
    angle: single;
    angleOne: single;
    length: single;
    width: single;
  var i: integer;
  begin
  {Draw palmate compound leaf. Use recursion structure we used to use for whole plant, with no branching.
    In a palmate leaf, leaflets increase in size as you move toward the middle of the leaf.
    Note that seedling leaves are never compound.}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  angleOne := safediv(64, plant.pLeaf.compoundNumLeaflets);
  if plant.pLeaf.compoundNumLeaflets > 0 then
    for i := plant.pLeaf.compoundNumLeaflets downto 1 do
	    begin
		  turtle.push;
      scale := self.propFullSize * plant.pLeaf.scaleAtOptimalBiomass / 100.0
        * (1.0  - safediv(i * 1.0, plant.pLeaf.compoundNumLeaflets));
      if (i <> 1) then
        begin
        length := scale * plant.pLeaf.petioleLengthAtOptimalBiomass_mm * propFullSize;
        width := scale * plant.pLeaf.petioleWidthAtOptimalBiomass_mm * propFullSize;
        if (odd(i)) then
          angle := angleOne * i * -1
        else
          angle := angleOne * i * 1;
        DrawLengthInPiecesWidthAngleZAngleYColor(length, width, 0, angle, petioleColor);
        end;
      DrawLeafOrLeaflet(scale);
      turtle.pop;
      end;
  end;

procedure GsLeaf.report;
  begin
  debugPrint('leaf, age ' + IntToStr(age) + ' biomass ' + floatToStr(liveBiomass_kg));
  {DebugForm.printNested(plant.turtle.stackSize, 'leaf, age ' + IntToStr(age));}
  end;

function GsLeaf.partType: integer;
  begin
  result := kPartTypeLeaf;
  end;

function GsLeaf.harvestItemTemplate: GsHarvestItemTemplate;
  begin
  if self.isSeedlingLeaf then
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeSeedlingLeaf]
  else
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeLeaf];
  end;

function GsLeaf.harvestItemIncludeInWholePlantHarvestBiomass: boolean;
  begin
  if self.isSeedlingLeaf then
    result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeSeedlingLeaf]
  else
    result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeLeaf];
  end;

procedure GsLeaf.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsLeaf;
  cvir.versionNumber := 1;
  cvir.additionNumber := 0;
  end;

procedure GsLeaf.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamBytes(sCurveParams, sizeof(sCurveParams));
  filer.streamColorRef(faceColor);
  filer.streamColorRef(backfaceColor);
  filer.streamColorRef(petioleColor);
  filer.streamSingle(unused1);
  filer.streamSingle(unused2);
  filer.streamSingle(propFullSize);
  filer.streamSingle(biomassAtCreation_kg);
  filer.streamBoolean(isSeedlingLeaf);
  end;

end.


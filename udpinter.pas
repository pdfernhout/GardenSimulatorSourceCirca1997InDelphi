unit Udpinter;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udpinter: Internode object. Skeleton of plant. Plant starts with one internode with
an apical meristem and one (monocot) or two (dicot) axillary meristems. Meristems
produce new internodes. Each internode points forward to up to three forward plant
parts (meristems, internodes, or inflorescences) and backward to one plant part
(usually an internode). Leaves are separate pointers. Internodes grow in size
both by biomass accumulation and water uptake. The bottom internode
draws the root top. A 'phytomer' is an internode and its associated leaves. At the
correct time internodes can also expand by bolting (defined by parameters). The
internode used to be combined with the leaves into a phytomer and it was recently
separated, so some things in this file refer to the phytomer instead of internode.}

interface

uses WinProcs, WinTypes, udppart, udplant, ufiler, udpleaf, uharvprt;

type

GsInternode = class(GsDrawingPlantPart)
  public
  leftBranchPlantPart: GsDrawingPlantPart;
  rightBranchPlantPart: GsDrawingPlantPart;
  nextPlantPart: GsDrawingPlantPart;
  phytomerAttachedTo: GsInternode;
  leftLeaf: GsLeaf;
  rightLeaf: GsLeaf;
  internodeColor: TColorRef;
  unused1: single;
  unused2: single;
  internodeAngle: single;  
  lengthExpansion: single;
  widthExpansion: single;
  boltingExpansion: single;
  fractionOfOptimalInitialBiomassAtCreation_frn: single;
  traversingDirection: byte;
  isFirstPhytomer: boolean;
  newBiomassForDay_kg: single;
  class function NewWithPlantFractionOfInitialOptimalSize(aPlant: GsDrawingPlant; aFraction: single): GsInternode;
  procedure blendColorsStrength(aColor: TColorRef; aStrength: single); override;
  procedure setColorsToParameters; override;
	procedure calculateInternodeAngle;
	function distanceFromApicalMeristem: longint;
	procedure draw; override;
	procedure drawInternode;
	procedure drawRootTop;
	procedure InitializeFractionOfInitialOptimalSize(thePlant: GsDrawingPlant; aFraction: single); {???}
	procedure SetInternodeAngle(anObject: single);
	procedure nextDay;  override;
	procedure report;
	function partType: integer; override;
  function harvestItemTemplate: GsHarvestItemTemplate; override;
  destructor destroy; override;
  procedure traverseActivity(mode: integer; traverser: GsTraverser); override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure streamDrawingPlantPart(filer: GsFiler; var drawingPlantPart: GsDrawingPlantPart);
  function isPhytomer: boolean; override;
  procedure checkIfSeedlingLeavesHaveAbscissed;
  function propFullLength: single;
  function propFullWidth: single;
  class function optimalInitialBiomass_kg(drawingPlant: GsDrawingPlant): single;
  procedure setAsFirstPhytomer;
  function harvestItemIncludeInWholePlantHarvestBiomass: boolean; override;
  end;

implementation

uses SysUtils, Dialogs, udebug, uturt3d, ueutils, ueplant, udpmeris, udpinflo, uclasses;

{ ------------------------------------------------------------------------------------------- phytomer }
class function GsInternode.NewWithPlantFractionOfInitialOptimalSize(aPlant: GsDrawingPlant;
  aFraction: single): GsInternode;
  begin
  result := create;
  result.InitializeFractionOfInitialOptimalSize(aPlant, aFraction);
  end;

procedure GsInternode.initializeFractionOfInitialOptimalSize(thePlant: GsDrawingPlant; aFraction: single);
  begin
  Initialize(thePlant);
  IsFirstPhytomer := false; {Plant sets this from outside on first phytomer.}
  calculateInternodeAngle;
  self.lengthExpansion := 1.0;
  self.widthExpansion := 1.0;
  self.boltingExpansion := 1.0;
  self.fractionOfOptimalInitialBiomassAtCreation_frn := aFraction;
  self.liveBiomass_kg := aFraction * GsInternode.optimalInitialBiomass_kg(plant);
  self.deadBiomass_kg := 0.0;
  internodeColor := plant.pInternode.faceColor;
  leftLeaf := GsLeaf.NewWithPlantFractionOfOptimalSize(plant, aFraction);
  if not plant.pMeristem.branchingIsAlternate then
    rightLeaf := GsLeaf.NewWithPlantFractionOfOptimalSize(plant, aFraction);
  end;

destructor GsInternode.destroy;
	begin
  {note that if branch parts were phytomers they will have been
  freed and set to nil by the traverser}
  nextPlantPart.free;
  nextPlantPart := nil;
  leftBranchPlantPart.free;
  leftBranchPlantPart := nil;
  rightBranchPlantPart.free;
  rightBranchPlantPart := nil;
  leftLeaf.free;
  leftLeaf := nil;
  rightLeaf.free;
  rightLeaf := nil;
  inherited destroy;
  end;

procedure GsInternode.setAsFirstPhytomer;
  begin
  self.isFirstPhytomer := true;
  if leftLeaf <> nil then leftLeaf.isSeedlingLeaf := true;
  if rightLeaf <> nil then rightLeaf.isSeedlingLeaf := true;
  self.calculateInternodeAngle;
  end;

procedure GsInternode.nextDay;
  var
    waterStressFactor_frn: single;
    tryExpansion: single;
  begin
  try
  inherited nextDay;
  { length and width expansion adjustment from new biomass (always decreases because new biomass is compact) }
  if liveBiomass_kg > 0 then
    begin
    { if liveBiomass_kg is extremely small, these divisions may produce an overflow }
    { must bound these because some accounting error is causing problems that should be fixed later }
    try
      tryExpansion :=
            max(0.0, min(50.0, safediv(liveBiomass_kg - newBiomassForDay_kg, liveBiomass_kg) * lengthExpansion
          + safediv(newBiomassForDay_kg, liveBiomass_kg) * 1.0));
      lengthExpansion := tryExpansion;
    except
    end;
    try
      tryExpansion :=
            max(0.0, min(50.0, safediv(liveBiomass_kg - newBiomassForDay_kg, liveBiomass_kg) * widthExpansion
          + safediv(newBiomassForDay_kg, liveBiomass_kg) * 1.0));
      widthExpansion := tryExpansion;
    except
    end;
    { length and width expansion increase due to water uptake }
    waterStressFactor_frn := GsPlant(plant.model).constraints.waterStressFactor_frn;
    with plant.pInternode do
      if self.age <= plant.pInternode.maxDaysToExpand then
        begin
        linearGrowthWithFactor(self.lengthExpansion,
            lengthMultiplierDueToExpansion, minDaysToExpand, waterStressFactor_frn);
        linearGrowthWithFactor(self.widthExpansion,
            widthMultiplierDueToExpansion, minDaysToExpand, waterStressFactor_frn);
        end;
    if plant.floweringHasStarted and
      (GsPlant(plant.model).development.age - plant.ageAtWhichFloweringStarted <= plant.pInternode.maxDaysToBolt) then
      with plant.pInternode do
        linearGrowthWithFactor(self.boltingExpansion, lengthMultiplierDueToBolting, minDaysToBolt, waterStressFactor_frn);
    end;
  self.checkIfSeedlingLeavesHaveAbscissed;
  except
    ErrorMessage('GsInternode.nextDay: Problem');
  end;
  end;

class function GsInternode.optimalInitialBiomass_kg(drawingPlant: GsDrawingPlant): single;
  begin
  try
  with drawingPlant.pInternode do
    if lengthMultiplierDueToBiomassAccretion * widthMultiplierDueToBiomassAccretion <> 0 then
      result := safediv(optimalFinalBiomass_kg,
          lengthMultiplierDueToBiomassAccretion * widthMultiplierDueToBiomassAccretion)
    else
      result := 0.0;
  except
    result := errorMessage('GsInternode.optimalInitialBiomass_kg: problem');
  end;
  end;

function GsInternode.propFullLength: single;
  begin
  try
  with plant.pInternode do
    if optimalFinalBiomass_kg * lengthMultiplierDueToExpansion <> 0 then
      result := safediv(self.totalBiomass_kg * self.lengthExpansion * self.boltingExpansion,
          optimalFinalBiomass_kg * lengthMultiplierDueToExpansion)
    else
      result := 0.0;
  except
    result := errorMessage('GsInternode.propFullLength: problem');
  end;
  end;

function GsInternode.propFullWidth: single;
  begin
  try
  with plant.pInternode do
    if optimalFinalBiomass_kg *  widthMultiplierDueToExpansion <> 0 then
      result := safediv(self.totalBiomass_kg * self.widthExpansion,
          optimalFinalBiomass_kg *  widthMultiplierDueToExpansion)
    else
      result := 0.0;
  except
    result := errorMessage('GsInternode.propFullWidth: problem');
  end;
  end;

procedure GsInternode.traverseActivity(mode: integer; traverser: GsTraverser);
  var
    biomassToRemove_kg: single;
    targetBiomass_kg: single;
  begin
  if self.isRemoved and (mode <> kActivityStream) and (mode <> kActivityFree) then exit;
  with plant.pInternode do
  begin
  try
  if (mode <> kActivityDraw) then
    begin
    if leftLeaf <> nil then leftLeaf.traverseActivity(mode, traverser);
    if rightLeaf <> nil then rightLeaf.traverseActivity(mode, traverser);
    end;
  case mode of
    kActivityNone: ;
    kActivityNextDay: self.nextDay;
    kActivityDemandVegetative_kg:
      begin
      if self.age > maxDaysToAccumulateBiomass then
        begin
        self.biomassDemand_kg := 0.0;
        exit;
        end;
      try
      if canRecoverFromStuntingDuringCreation then
        targetBiomass_kg := optimalFinalBiomass_kg
      else
        targetBiomass_kg := optimalFinalBiomass_kg * fractionOfOptimalInitialBiomassAtCreation_frn;
      self.biomassDemand_kg := linearGrowthResult(self.liveBiomass_kg,
        targetBiomass_kg, minDaysToAccumulateBiomass);
      traverser.total := traverser.total + self.biomassDemand_kg;
      except
        ErrorMessage('GsInternode.traverseActivity: Problem in internode demandVegetative');
      end
      end;
    kActivityDemandReproductive_kg:;
	    {Return reproductive demand recursively from all reproductive meristems and fruits connected to
      this phytomer. Phytomers, inflorescences, and flowers themselves have no demands.}
    kActivityGrowVegetative:
	    begin
      if self.age > maxDaysToAccumulateBiomass then exit;
      try
      self.newBiomassForDay_kg := max(0.0, self.biomassDemand_kg * traverser.fractionOfPotentialBiomass);
      self.liveBiomass_kg := self.liveBiomass_kg + self.newBiomassForDay_kg;
      except
        ErrorMessage('GsInternode.traverseActivity: Problem in internode GrowVegetative');
      end;
      end;
    kActivityGrowReproductive: ;
	    {Recurse available photosynthate allocated to reproductive growth to all plant parts.
      Only meristems and fruits will incorporate it.}
    kActivityStartReproduction: ;
     {Send signal to consider reproductive mode to all meristems on plant.}
    kActivityFindPlantPartAtPosition:
      begin
      if pointsAreCloseEnough(traverser.point, position) then
        begin
        { CFK UNFINISHED - should clip off everything above it }
        { traverser.finished := true; }
        end;
      end;
    kActivityAddMatureHarvestedPartsToList:  ;
    kActivityFindAgeOfYoungestPhytomer:
      begin
      {Return age of youngest phytomer (not inflorescence or meristem) on plant. This is used to create
      a relative age index for all phytomers to distribute stress and age colors.}
      if self.age < traverser.count then traverser.count := self.age;
      end;
    kActivityDraw: self.draw;
    kActivityReport: self.report;
    kActivityStream: self.streamUsingFiler(traverser.filer);
    kActivityFree: { free called by traverser };
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
    kActivityAddBundledPartsToList:
      begin
      if self.harvestItemIncludeInWholePlantHarvestBiomass then traverser.foundList.add(self);
      end;
    kActivityGatherStatistics:
      begin
      self.addToStatistics(traverser.statistics, kHarvestPartTypeStem);
      self.addToStatistics(traverser.statistics, kStatisticsPartTypeAllVegetative);
      end;
    kActivityStandingDeadBiomassThatCanBeRemoved_kg:
      inherited traverseActivity(mode, traverser);
    kActivityRemoveStandingDeadBiomass:
      inherited traverseActivity(mode, traverser);
   else
      raise Exception.create('unhandled mode for GsInternode activity');
    end;
  except
    on GsExceptionFiler do
      raise;
    else
    	ErrorMessage('GsInternode.traverseActivity: Problem in phytomer traverseActivity');
  end;
  end;
  end;

function GsInternode.harvestItemIncludeInWholePlantHarvestBiomass: boolean;
  begin
  result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeStem];
  end;

function GsInternode.isPhytomer: boolean;
  begin
  result := true;
  end;

procedure GsInternode.BlendColorsStrength(aColor: TColorRef; aStrength: single);
  begin
  if aStrength <= 0.0 then exit;
  internodeColor := blendColors(internodeColor, aColor, aStrength);
  end;

procedure GsInternode.setColorsToParameters;
  begin
  {Initialize phytomer colors at those in plant parameters, before stresses are considered.}
  internodeColor := plant.pInternode.faceColor;
  end;

procedure GsInternode.calculateInternodeAngle;
  begin
  {Calculate internode angle from previous internode at creation of phytomer. No longer using
  flexibility index to calculate this as it is redundant with weight-bearing algorithm.
  Created a second parameter (meander index) to make the stems completely straight or wandering. }
  if (plant.pInternode.meanderIndex = 0) then
    InternodeAngle := 0
  else
    InternodeAngle := 64.0  / 100.0  *
      (plant.randomNumberGenerator.RandomNormalPercent(plant.pInternode.meanderIndex));
  end;

function GsInternode.distanceFromApicalMeristem: longint;
  var
    aPhytomer: GsInternode;
  begin
  {Count phytomers along this apex until you reach an apical meristem or inflorescence.}
  result := 0;
  if (self.nextPlantPart.isPhytomer) then
    aPhytomer := GsInternode(self.nextPlantPart)
  else
    aPhytomer := nil;
  while aPhytomer <> nil do
    begin
    inc(result);
    if (aPhytomer.nextPlantPart.isPhytomer) then
      aPhytomer := GsInternode(aPhytomer.nextPlantPart)
    else
      aPhytomer := nil;
    end;
  end;

procedure GsInternode.draw;
  var
    turtle: KfTurtle;
  begin
  {Draw all parts of phytomer. Consider if the phytomer is the first (has the seedling leaves) and whether
    the leaves attached to this phytomer have abscissed (and are not drawn).}
  try
    turtle := plant.turtle;
    if (turtle = nil) then exit;
    if plant.needToRecalculateColors then self.calculateColors;
    if (isFirstPhytomer) and (plant.pRoot.showsAboveGround) then drawRootTop;
    drawInternode;
    try
    self.recordPositionFromTurtleIfDesired;
    except
    end;
    if leftLeaf <> nil then leftLeaf.drawWithDirection(kDirectionLeft);
    if rightLeaf <> nil then rightLeaf.drawWithDirection(kDirectionRight);
  except
	  on EDivByZero do ErrorMessage('GsInternode.draw EDivByZero');
	  on EInvalidOp do ErrorMessage('GsInternode.draw EInvalidOp');
	  on EOverFlow do ErrorMessage('GsInternode.draw EOverFlow');
	  on EUnderFlow do ErrorMessage('GsInternode.draw EUnderFlow');
  end;
  end;

procedure GsInternode.drawInternode;
  var
    fractionOfOptimalBiomass: single;
    length, width: single;
  begin
  if (plant.turtle = nil) then exit;
  length := max(0.0, propFullLength * plant.pInternode.lengthAtOptimalFinalBiomassAndExpansion_mm);
  width := max(0.0, propFullWidth * plant.pInternode.widthAtOptimalFinalBiomassAndExpansion_mm);
  DrawLengthInPiecesWidthAngleZAngleYColor(length, width, internodeAngle, 0, internodeColor);
  end;

procedure GsInternode.drawRootTop;
  var
    turtle: KfTurtle;
    scale: single;
    numParts: integer;
  var i: integer;
  begin
  {Draw top of root above ground, if it can be seen. Adjust size for heat unit index of plant. }
  numParts := 8; {constant}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  turtle.push;
  turtle.setLineWidth(1.0);
  turtle.drawingSurface.ForeColor := plant.pRoot.faceColor;
  turtle.drawingSurface.BackColor := plant.pRoot.backfaceColor;
  turtle.setLineColor(darkerColor(plant.pRoot.faceColor));
  scale := GsPlant(plant.model).development.heatUnitIndex * plant.pRoot.scaleAtFullSize / 100.0;
  if numParts > 0 then
    for i := 0 to numParts  - 1 do
      begin
		  turtle.RotateX(256 div numParts);
      turtle.push;
      turtle.RotateZ(64);
      turtle.RotateY(-64);
      if plant.pRoot.object3D <> nil then
      	plant.pRoot.object3D.DrawWithTurtleScale(turtle, scale);
      turtle.pop;
      end;
  turtle.pop;
  end;

procedure GsInternode.report;
  begin
  debugPrint('internode, age ' + IntToStr(age) + ' biomass ' + floatToStr(liveBiomass_kg));
  {DebugForm.printNested(plant.turtle.stackSize, 'phytomer, age ' + IntToStr(age));}
  end;

procedure GsInternode.checkIfSeedlingLeavesHaveAbscissed;
  begin
  {If first phytomer, only want to draw seedling leaves for some time after emergence.
  For monopodial plant, stop drawing seedling leaves some number of nodes after emergence (parameter).
  For sympodial plant, this doesn't work; use age of meristem instead; age is set as constant.}
  if (not isFirstPhytomer) then exit;
  if (plant.pMeristem.branchingIsSympodial) then
    begin
    if (age < 10) then exit;
    end
  else
    begin
    if (self.distanceFromApicalMeristem <= plant.pSeedlingLeaf.nodesOnStemWhenFallsOff) then exit;
    end;
  { absolute cut-off }
  if (GsPlant(plant.model).development.heatUnitIndex < 0.25) then exit;
  { CFK FIX - should really have removed biomass in seedling leaves from model plant }
  if leftLeaf <> nil then leftLeaf.isRemoved := true;
  if rightLeaf <> nil then rightLeaf.isRemoved := true;
  end;

function GsInternode.partType: integer;
  begin
  result := kPartTypePhytomer;
  end;

function GsInternode.harvestItemTemplate: GsHarvestItemTemplate;
  begin
  result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeStem];
  end;

{PDF PORT think this is wrong}
procedure GsInternode.SetInternodeAngle(anObject: single);
  begin
  {did this because if internode angle was large, division of angle produced curlique}
  if (anObject < 128) then
    internodeAngle := anObject
  else
    internodeAngle := 256 - anObject;
  end;

procedure GsInternode.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsInternode;
  cvir.versionNumber := 2;
  cvir.additionNumber := 0;
  end;

procedure GsInternode.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamColorRef(internodeColor);
  filer.streamSingle(unused1);
  filer.streamSingle(unused2);
  filer.streamSingle(internodeAngle);
  filer.streamSingle(lengthExpansion);
  filer.streamSingle(widthExpansion);
  filer.streamSingle(boltingExpansion);
  filer.streamSingle(fractionOfOptimalInitialBiomassAtCreation_frn);
  filer.streamByte(traversingDirection);
  filer.streamBoolean(isFirstPhytomer);
  filer.streamSingle(newBiomassForDay_kg);
  {reading or writing the plant part subobject phytomers will be done by traverse}
  {for now, just need to create these objects if needed and set plant and phytomerAttachedTo}
  {if it is an inflorescence - read it now}
  self.streamDrawingPlantPart(filer, leftBranchPlantPart);
  self.streamDrawingPlantPart(filer, rightBranchPlantPart);
  self.streamDrawingPlantPart(filer, nextPlantPart);
  self.streamDrawingPlantPart(filer, GsDrawingPlantPart(leftLeaf));
  self.streamDrawingPlantPart(filer, GsDrawingPlantPart(rightLeaf));
  end;

procedure GsInternode.streamDrawingPlantPart(filer: GsFiler; var drawingPlantPart: GsDrawingPlantPart);
  var
   partType: smallint;
  begin
  if filer.isWriting then
    begin
    if drawingPlantPart = nil then
      partType := kPartTypeNone
    else partType := drawingPlantPart.partType;
    filer.streamSmallint(partType);
    case partType of
      kPartTypeMeristem, kPartTypeInflorescence, kPartTypeLeaf:
        drawingPlantPart.streamUsingFiler(filer);
      end;
    end
  else if filer.isReading then
    begin
    filer.streamSmallint(partType);
    case partType of
  		kPartTypeNone:
        drawingPlantPart := nil;
  		kPartTypeMeristem:
        begin
        drawingPlantPart := GsMeristem.create;
        drawingPlantPart.plant := self.plant;
        GsMeristem(drawingPlantPart).phytomerAttachedTo := self;
        drawingPlantPart.streamUsingFiler(filer);
        end;
  		kPartTypeInflorescence:
        begin
        drawingPlantPart := GsInflorescence.create;
        drawingPlantPart.plant := self.plant;
        drawingPlantPart.streamUsingFiler(filer);
        end;
  		kPartTypePhytomer:
        begin
        drawingPlantPart := GsInternode.create;
        drawingPlantPart.plant := self.plant;
        GsInternode(drawingPlantPart).phytomerAttachedTo := self;
        {will be streamed in by traverser}
        end;
  		kPartTypeLeaf:
        begin
        drawingPlantPart := GsLeaf.create;
        drawingPlantPart.plant := self.plant;
        drawingPlantPart.streamUsingFiler(filer);
        end
      else
        Exception.create('GsInternode: unknown plant part type ' + inttostr(partType));
    end;
    end;
  end;

end.

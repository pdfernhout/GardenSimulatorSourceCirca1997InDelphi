unit Udpmeris;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udpmeris: Meristem (bud) object. Creates new internodes (with leaves) if in vegetative
mode, or inflorescences (with flower/fruits) if in reproductive mode. Switches to repro
mode at start of flowering if random number is below repro. probability. Can be apical
(at end of stem) or axillary (in leaf axil). If axillary, decides whether to branch
every day based on branching index of plant (branchiness) and branching distance
(distance from stem apex when axillary buds can branch).}

interface

uses WinProcs, WinTypes, udppart, udpinter, udplant, ufiler, uharvprt;

type

GsMeristem = class(GsDrawingPlantPart)
  public
  phytomerAttachedTo: GsInternode;
  daysCreatingThisPlantPart: longint;
  isActive: boolean;
  isApical: boolean;
  isReproductive: boolean;
  genderIndex: smallint;
	constructor newWithPlant(aPlant: GsDrawingPlant);
	procedure initialize(aPlant: GsDrawingPlant);
  procedure setIfActive(active: boolean);
  procedure setIfApical(apical: boolean);
  procedure setIfReproductive(reproductive: boolean);
	function contemplateBranching: boolean;
	function createAxillaryMeristem(direction: integer): GsMeristem;
	function createFirstPhytomer: GsInternode;
	procedure createInflorescence(fractionOfOptimalSize: single);
	procedure createPhytomer(fractionOfFullSize: single);
	function decideIfActiveFemale: boolean;
	function decideIfActiveHermaphroditic: boolean;
	function decideIfActiveMale: boolean;
	procedure draw; override;
	procedure nextDay;  override;
	procedure startReproduction;
	function partType: integer; override;
  function harvestItemTemplate: GsHarvestItemTemplate; override;
	function willCreateInflorescence: boolean;
  procedure traverseActivity(mode: integer; traverser: GsTraverser); override;
  destructor destroy; override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure accumulateOrCreatePhytomer;
  procedure accumulateOrCreateInflorescence;
  function harvestItemIncludeInWholePlantHarvestBiomass: boolean; override;
  function optimalInitialPhytomerBiomass_kg: single;
  end;

implementation

uses SysUtils, Dialogs, udebug, uturt3d, ueutils, ueplant, udpinflo, udpleaf, uclasses, uunits;

{ -------------------------------------------------------------------------------------------- meristem }
constructor GsMeristem.NewWithPlant(aPlant: GsDrawingPlant);
  begin
  self.create;
  Initialize(aPlant);
  end;

destructor GsMeristem.destroy;
  begin
  if isReproductive then
    begin
    if isActive then
      begin
      if isApical then
        dec(plant.numApicalActiveReproductiveMeristemsOrInflorescences)
      else {not isApical}
        dec(plant.numAxillaryActiveReproductiveMeristemsOrInflorescences);
      end
    else {not isActive}
      begin
      if isApical then
        dec(plant.numApicalInactiveReproductiveMeristems)
      else {not isApical}
        dec(plant.numAxillaryInactiveReproductiveMeristems);
      end;
    end;
  inherited destroy;
  end;

procedure GsMeristem.Initialize(aPlant: GsDrawingPlant);
  begin
  inherited Initialize(aPlant);
  { don't need to call any setIf... functions here because lists don't need to be changed yet }
  isApical := true;
  liveBiomass_kg := 0.0;
  deadBiomass_kg := 0.0;
  biomassDemand_kg := 0.0;
  daysCreatingThisPlantPart := 0;
  isActive := false;
  isReproductive := false;
  genderIndex := kGenderFemale;
  if plant.floweringHasStarted and
    (plant.randomNumberGenerator.zeroToOne <= plant.pMeristem.determinateProbability) then
    self.setIfReproductive(true);
  end;

procedure GsMeristem.nextDay;
  begin
  try
  inherited nextDay;
  if isActive then
    begin
    inc(self.daysCreatingThisPlantPart);
    if not isReproductive then
      self.accumulateOrCreatePhytomer
    else
      self.accumulateOrCreateInflorescence;
    end
  else
    begin
    if not isApical and not isReproductive and contemplateBranching then self.setIfActive(true);
    end;
  except
    ErrorMessage('GsMeristem.nextDay: Problem in meristem next day');
  end;
  end;

function GsMeristem.contemplateBranching: boolean;
  var
    distanceModifier: single;
    decisionFraction: single;
    randomNumber: single;
  begin
  try
  {Decide if this meristem is going to become active and branch (start demanding photosynthate).
  This method is called once per day to create a large pool of random tests, each with
  very small probability, leading to a small number of occurrences with small variation.}
  if (plant.pMeristem.branchingIndex = 100) then
    begin
    result := true;
    exit;
    end;
  if (plant.pMeristem.branchingDistance = 0) or (plant.pMeristem.branchingIndex = 0) then
    begin
    result := false;
    exit;
    end;
  { stop the branching within the branching distance }
  if phytomerAttachedTo.distanceFromApicalMeristem < plant.pMeristem.branchingDistance then
    begin
    result := false;
    exit;
    end;
  { was working on method to decrease branching smoothly away from apex, but abandoned it }
  {distanceModifier := safediv(1.0 * phytomerAttachedTo.distanceFromApicalMeristem, plant.pMeristem.branchingDistance); }
  {distanceModifier := Max(0.0, Min(1.0, distanceModifier)); }
  distanceModifier := 1.0;
  decisionFraction := distanceModifier * plant.pMeristem.branchingIndex / 100.0;
  randomNumber := plant.randomNumberGenerator.randomPercent;
  result := randomNumber < decisionFraction;
  except
    result := false;
    errorMessage('GsMeristem.contemplateBranching: problem');
  end;
  end;

function GsMeristem.optimalInitialPhytomerBiomass_kg: single;
  begin
  result := GsInternode.optimalInitialBiomass_kg(plant) + GsLeaf.optimalInitialBiomass_kg(plant);
  if not plant.pMeristem.branchingIsAlternate then
    result := result + GsLeaf.optimalInitialBiomass_kg(plant);
  end;

procedure GsMeristem.accumulateOrCreatePhytomer;
  var
    optimalInitialBiomass_kg, minBiomassNeeded_kg: single;
    fractionMultiplier, fractionOfOptimalSize: single;
    shouldCreatePhytomer: boolean;
  begin
  try
  shouldCreatePhytomer := false;
  optimalInitialBiomass_kg := self.optimalInitialPhytomerBiomass_kg;
  if (self.liveBiomass_kg >= optimalInitialBiomass_kg) then
    shouldCreatePhytomer := true
  else
    begin
    minBiomassNeeded_kg := optimalInitialBiomass_kg * plant.pInternode.minFractionOfOptimalInitialBiomassToCreateInternode_frn;
    if (self.liveBiomass_kg >= minBiomassNeeded_kg)
      and (daysCreatingThisPlantPart >= plant.pInternode.maxDaysToCreateInternodeIfOverMinFraction) then
      shouldCreatePhytomer := true;
    end;
  if shouldCreatePhytomer then
    begin
    if (GsPlant(plant.model).constraints.pStressFactor_frn < 0.5) then
      begin
      { p stress affects size of new internodes being formed at time of stress }
      fractionMultiplier := min(1.0, GsPlant(plant.model).constraints.pStressFactor_frn * 2.0);
      plant.unallocatedNewVegetativeBiomass_kg := plant.unallocatedNewVegetativeBiomass_kg
        + liveBiomass_kg * (1.0 - fractionMultiplier);
      liveBiomass_kg := liveBiomass_kg * fractionMultiplier;
      end;
    fractionOfOptimalSize := safediv(liveBiomass_kg, optimalInitialBiomass_kg);
    self.createPhytomer(fractionOfOptimalSize);
    self.liveBiomass_kg := 0.0;
    self.daysCreatingThisPlantPart := 0;
    end;
  except errorMessage('GsMeristem.accumulateOrCreatePhytomer: problem'); end;
  end;

procedure GsMeristem.accumulateOrCreateInflorescence;
  var
    optimalInitialBiomass_kg, minBiomassNeeded_kg, fractionOfOptimalSize: single;
    shouldCreateInflorescence: boolean;
    waterStressFactor_frn: single;
  begin
  try
  if phytomerAttachedTo.isFirstPhytomer or not isReproductive or not isActive then exit;
  shouldCreateInflorescence := false;
  optimalInitialBiomass_kg := GsInflorescence.optimalInitialBiomass_kg(plant, genderIndex);
  if (self.liveBiomass_kg >= optimalInitialBiomass_kg) then
    shouldCreateInflorescence := true
  else
    begin
    minBiomassNeeded_kg := optimalInitialBiomass_kg * plant.pInternode.minFractionOfOptimalInitialBiomassToCreateInternode_frn;
    if (self.liveBiomass_kg >= minBiomassNeeded_kg)
      and (daysCreatingThisPlantPart >= plant.pInflor[genderIndex].maxDaysToCreateInflorescenceIfOverMinFraction) then
      shouldCreateInflorescence := true;
    end;
  if shouldCreateInflorescence then
    begin
    fractionOfOptimalSize := safediv(liveBiomass_kg, optimalInitialBiomass_kg);
    { CFK FIX - quick fix for inflorescences being too long when plant is water stressed,
      but not complete solution - should expand the same way internodes do }
    waterStressFactor_frn := GsPlant(plant.model).constraints.waterStressFactor_frn;
    fractionOfOptimalSize := fractionOfOptimalSize * waterStressFactor_frn;
    self.createInflorescence(fractionOfOptimalSize);
    self.liveBiomass_kg := 0.0;
    self.daysCreatingThisPlantPart := 0;
    end;
  except errorMessage('GsMeristem.accumulateOrCreateInflorescence: problem'); end;
  end;

procedure GsMeristem.traverseActivity(mode: integer; traverser: GsTraverser);
  var
    optimalBiomass_kg, biomassToRemove_kg: single;
  begin
  if self.isRemoved and (mode <> kActivityStream) and (mode <> kActivityFree) then exit;
  try
  case mode of
    kActivityNone: ;
    kActivityNextDay: self.nextDay;
    kActivityDemandVegetative_kg:
      begin
      {Return vegetative demand to create a phytomer.
      No demand if
      meristem is inactive,
      meristem is in reproductive mode, or if
      meristem is axillary and attached to the first phytomer and there is NOT sympodial branching.}
      if (not isActive) or (isReproductive) then exit;
      if ((not isApical) and (phytomerAttachedTo.isFirstPhytomer) and
        (not plant.pMeristem.branchingIsSympodial)) then exit;
      try
      optimalBiomass_kg := GsInternode.optimalInitialBiomass_kg(plant) + GsLeaf.optimalInitialBiomass_kg(plant);
      if not plant.pMeristem.branchingIsAlternate then
        optimalBiomass_kg := optimalBiomass_kg + GsLeaf.optimalInitialBiomass_kg(plant);
      self.biomassDemand_kg := linearGrowthResult(self.liveBiomass_kg,
        optimalBiomass_kg, plant.pInternode.minDaysToCreateInternode);
      traverser.total := traverser.total + self.biomassDemand_kg;
      except
        ErrorMessage('GsMeristem.traverseActivity: Problem in meristem demandVegetative');
      end
      end;
   kActivityDemandReproductive_kg:
      begin
      {Return reproductive demand to create an inflorescence. No demand if meristem is inactive,
      if it is in vegetative mode, or if it is attached to the first phytomer.}
      if (not isActive) or (not isReproductive) then exit;
      if (phytomerAttachedTo.isFirstPhytomer) and (not isApical) then exit;
      try
      with plant.pInflor[genderIndex] do
        self.biomassDemand_kg := linearGrowthResult(self.liveBiomass_kg,
          optimalBiomass_kg, minDaysToCreateInflorescence);
      traverser.total := traverser.total + self.biomassDemand_kg;
      except
        ErrorMessage('GsMeristem.traverseActivity: Problem in meristem demandReproductive');
      end
      end;
    kActivityGrowVegetative:
      begin
      {Allocate new biomass by portion of demand. A phytomer cannot be made before the minimum number
      of days has passed to make one phytomer.}
      if not (isActive) then exit;
      if isReproductive then exit;
      try
      self.liveBiomass_kg := self.liveBiomass_kg + self.biomassDemand_kg * traverser.fractionOfPotentialBiomass;
      except
        ErrorMessage('GsMeristem.traverseActivityProblem in meristem GrowVegetative');
      end;
      end;
    kActivityGrowReproductive:
      begin
      if (not isActive) or (not isReproductive) then exit;
      try
      self.liveBiomass_kg := self.liveBiomass_kg + self.biomassDemand_kg * traverser.fractionOfPotentialBiomass;
      except
        ErrorMessage('GsMeristem.traverseActivityProblem in meristem kActivityGrowReproductive');
      end;
      end;
    kActivityStartReproduction: self.startReproduction;
    kActivityFindPlantPartAtPosition:
      begin
      if pointsAreCloseEnough(traverser.point, position) then
        begin
        traverser.foundPlantPart := self;
        traverser.finished := true;
        end;
      end;
    kActivityAddMatureHarvestedPartsToList:
      begin
      if self.canBeHarvested then traverser.foundList.add(self);
      end;
    kActivityFindAgeOfYoungestPhytomer: { not a phytomer };
    kActivityDraw:  self.draw;
    kActivityReport:
      begin
      debugPrint('meristem, age ' + IntToStr(age) + ' biomass ' + floatToStr(liveBiomass_kg));
      {DebugForm.printNested(plant.turtle.stackSize, 'meristem, age ' + IntToStr(age) + ' biomass '
        + floatToStr(liveBiomass_kg));}
      end;
    kActivityStream: {called by phytomer} ;
    kActivityFree: { free called by phytomer or inflorescence };
    kActivityVegetativeBiomassThatCanBeRemoved_kg, kActivityReproductiveBiomassThatCanBeRemoved_kg:
      begin
      if not isActive then exit;
      if (mode = kActivityVegetativeBiomassThatCanBeRemoved_kg) and (isReproductive) then exit;
      if (mode = kActivityReproductiveBiomassThatCanBeRemoved_kg) and (not isReproductive) then exit;
      try
      { a meristem can lose all of its biomass }
      traverser.total := traverser.total + self.liveBiomass_kg;
      except
        ErrorMessage('GsMeristem.traverseActivityProblem in meristem kActivityVegetativeBiomassThatCanBeRemoved_kg');
      end;
      end;
    kActivityRemoveVegetativeBiomass, kActivityRemoveReproductiveBiomass:
      begin
      if not isActive then exit;
      if (mode = kActivityRemoveVegetativeBiomass) and (isReproductive) then exit;
      if (mode = kActivityRemoveReproductiveBiomass) and (not isReproductive) then exit;
      try
      biomassToRemove_kg := self.liveBiomass_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg - biomassToRemove_kg;
      self.deadBiomass_kg := self.deadBiomass_kg + biomassToRemove_kg;
      except
        ErrorMessage('GsMeristem.traverseActivityProblem in meristem kActivityRemoveVegetativeBiomass');
      end;
      end;
    kActivityAddBundledPartsToList:
      begin
      if self.harvestItemIncludeInWholePlantHarvestBiomass then traverser.foundList.add(self);
      end;
    kActivityGatherStatistics:
      begin
      self.addToStatistics(traverser.statistics, kHarvestPartTypeAxillaryBud);
      if isReproductive then
        self.addToStatistics(traverser.statistics, kStatisticsPartTypeAllReproductive)
      else
        self.addToStatistics(traverser.statistics, kStatisticsPartTypeAllVegetative);
      end;
    kActivityStandingDeadBiomassThatCanBeRemoved_kg:
      inherited traverseActivity(mode, traverser);
    kActivityRemoveStandingDeadBiomass:
      inherited traverseActivity(mode, traverser);
   else
      raise Exception.create('unhandled mode for GsMeristem activity');
    end;
  except
    ErrorMessage('GsMeristem.traverseActivity: Problem in meristem traverseActivity');
  end;
  end;

function GsMeristem.harvestItemIncludeInWholePlantHarvestBiomass: boolean;
  begin
  { NOTE: we have decided you can't harvest (eat) an apical bud. }
  result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeAxillaryBud];
  end;

function GsMeristem.CreateAxillaryMeristem(direction: integer): GsMeristem;
  var
    newMeristem: GsMeristem;
  begin
  result := nil;
  {Create new axillary meristem attached to the same phytomer as this apical meristem is attached to.}
  newMeristem := GsMeristem.create;
  newMeristem.Initialize(plant);
  newMeristem.setIfApical(false);
  newMeristem.PhytomerAttachedTo := phytomerAttachedTo;
  if (direction = kDirectionLeft) then
    phytomerAttachedTo.LeftBranchPlantPart := newMeristem
  else
    phytomerAttachedTo.RightBranchPlantPart := newMeristem;
  result := newMeristem;
  end;

procedure GsMeristem.setIfActive(active: boolean);
  begin
  if isActive <> active then
    begin
    isActive := active;
    if isReproductive then
      if isApical then
        begin
        if isActive then
          begin
          inc(plant.numApicalActiveReproductiveMeristemsOrInflorescences);
          dec(plant.numApicalInactiveReproductiveMeristems);
          end
        else {not isActive}
          begin
          inc(plant.numApicalInactiveReproductiveMeristems);
          dec(plant.numApicalActiveReproductiveMeristemsOrInflorescences);
          end;
       end
     else {not isApical}
       begin
       if isActive then
         begin
         inc(plant.numAxillaryActiveReproductiveMeristemsOrInflorescences);
         dec(plant.numAxillaryInactiveReproductiveMeristems);
         end
       else {not isActive}
         begin
         inc(plant.numAxillaryInactiveReproductiveMeristems);
         dec(plant.numAxillaryActiveReproductiveMeristemsOrInflorescences);
         end;
       end;
     end;
  end;

procedure GsMeristem.setIfApical(apical: boolean);
  begin
  if isApical <> apical then
    begin
    isApical := apical;
    if isReproductive then
      if isActive then
        begin
        if isApical then
          begin
          inc(plant.numApicalActiveReproductiveMeristemsOrInflorescences);
          dec(plant.numAxillaryActiveReproductiveMeristemsOrInflorescences);
          end
        else {not isApical}
          begin
          inc(plant.numAxillaryActiveReproductiveMeristemsOrInflorescences);
          dec(plant.numApicalActiveReproductiveMeristemsOrInflorescences);
          end;
       end
     else {not isActive}
       begin
        if isApical then
          begin
          inc(plant.numApicalInactiveReproductiveMeristems);
          dec(plant.numAxillaryInactiveReproductiveMeristems);
          end
        else {not isApical}
          begin
          inc(plant.numAxillaryInactiveReproductiveMeristems);
          dec(plant.numApicalInactiveReproductiveMeristems);
          end;
       end;
     end;
  end;

procedure GsMeristem.setIfReproductive(reproductive: boolean);
  begin
  {assume can only become reproductive and not go other way}
  if isReproductive <> reproductive then
    begin
    isReproductive := reproductive;
    if isActive then
      begin
      if isApical then
        inc(plant.numApicalActiveReproductiveMeristemsOrInflorescences)
      else {not isApical}
        inc(plant.numAxillaryActiveReproductiveMeristemsOrInflorescences);
      end
    else {not isActive}
      begin
      if isApical then
        inc(plant.numApicalInactiveReproductiveMeristems)
      else {not isApical}
        inc(plant.numAxillaryInactiveReproductiveMeristems);
      end;
    end;
  end;

function GsMeristem.createFirstPhytomer: GsInternode;
  var
    optimalInitialBiomass_kg, biomassInStandingLive_kg, fractionOfOptimalBiomass_frn: single;
  begin
  result := nil;
  try
  {Create first phytomer of plant, based on initial standing live biomass, assuming whole above-ground biomass goes into
   first phytomer. Return phytomer created so plant can hold on to it.}
  optimalInitialBiomass_kg := self.optimalInitialPhytomerBiomass_kg;
  biomassInStandingLive_kg := plant.modelPlantStandingLive_kg;
  fractionOfOptimalBiomass_frn := safediv(biomassInStandingLive_kg, optimalInitialBiomass_kg);
  if fractionOfOptimalBiomass_frn > 1.0 then
    begin
    plant.unallocatedNewVegetativeBiomass_kg := plant.unallocatedNewVegetativeBiomass_kg
      + (biomassInStandingLive_kg - optimalInitialBiomass_kg);
    fractionOfOptimalBiomass_frn := 1.0;
    end;
  CreatePhytomer(fractionOfOptimalBiomass_frn); 
  result := phytomerAttachedTo;
  except errorMessage('GsMeristem.createFirstPhytomer: problem'); end;
  end;

procedure GsMeristem.createInflorescence(fractionOfOptimalSize: single);
  var
    newInflorescence: GsInflorescence;
  begin
  {Create new inflorescence. Attach new inflorescence to phytomer this meristem is attached to,
    then unattach this meristem from the phytomer and tell inflorescence pointer to self
    so inflorescence can free self when it is freed.}
  newInflorescence := GsInflorescence.create;
  newInflorescence.InitializeGenderApicalOrAxillary(plant, gender, isApical, fractionOfOptimalSize);
  newInflorescence.meristemThatCreatedMe := self;
  if isApical then
    phytomerAttachedTo.NextPlantPart := newInflorescence
  else if phytomerAttachedTo.LeftBranchPlantPart = self then
    phytomerAttachedTo.LeftBranchPlantPart := newInflorescence
  else if phytomerAttachedTo.RightBranchPlantPart = self then
    phytomerAttachedTo.RightBranchPlantPart := newInflorescence
  else
    raise Exception.create('problem in GsMeristem.createInflorescence');
  PhytomerAttachedTo := nil;
  end;

procedure GsMeristem.createPhytomer(fractionOfFullSize: single);
  var
    newPhytomer: GsInternode;
    leftMeristem, rightMeristem: GsMeristem;
  begin
  {Create new phytomer. Fraction of full size is the amount of biomass the meristem accumulated
     (in the number of days it had to create a phytomer) divided by the optimal biomass of a phytomer.}
  newPhytomer := GsInternode.NewWithPlantFractionOfInitialOptimalSize(plant, fractionOfFullSize);
  newPhytomer.phytomerAttachedTo := self.phytomerAttachedTo;
  if (phytomerAttachedTo <> nil) then
    begin
    if isApical then
      begin
      phytomerAttachedTo.NextPlantPart := newPhytomer;
      if (plant.pMeristem.branchingIsSympodial) then self.setIfActive(false);
      end
    else { axillary }
      begin
      if phytomerAttachedTo.LeftBranchPlantPart = self then
        phytomerAttachedTo.LeftBranchPlantPart := newPhytomer
      else if phytomerAttachedTo.RightBranchPlantPart = self then
        phytomerAttachedTo.RightBranchPlantPart := newPhytomer
      else
        raise Exception.create('problem in GsMeristem.CreatePhytomer');
      self.setIfApical(true);
      end;
    end
  else { phytomerAttachedTo = nil (means this is first phytomer) }
    begin
    self.setIfApical(true);
    self.setIfActive(not plant.pMeristem.branchingIsSympodial);
    end;
  self.phytomerAttachedTo := newPhytomer;
  self.phytomerAttachedTo.NextPlantPart := self;
  if plant.pMeristem.branchingIsAlternate then
    begin
    leftMeristem := CreateAxillaryMeristem(kDirectionLeft);
    if plant.pMeristem.branchingIsSympodial then leftMeristem.setIfActive(true);
    end
  else
    begin
    leftMeristem := CreateAxillaryMeristem(kDirectionLeft);
    rightMeristem := CreateAxillaryMeristem(kDirectionRight);
    if plant.pMeristem.branchingIsSympodial then
      if (plant.randomNumberGenerator.zeroToOne < 0.5) then
        leftMeristem.setIfActive(true)
      else
        rightMeristem.setIfActive(true);
    end;
  end;

procedure GsMeristem.draw;
  var i: integer;
  var
    turtle: KfTurtle;
    scale: single;
    daysToFullSize: single;
    numParts: integer;
  begin
  {Draw meristem (only if buds are enlarged as in Brussels sprouts). Since this only very rarely happens,
  have these buds increase to maximum size in a constant number of days (10). Number of sections drawn
  (of the 3D object being rotated) is also set to a constant (8).}
try
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  try
  self.recordPositionFromTurtleIfDesired;
  except
  end;
  if isApical then exit;
  if (plant.paxillaryBud.scale = 0) then exit;
  daysToFullSize := 10;
  numParts := 8;
  if phytomerAttachedTo <> nil then
    begin
    turtle.drawingSurface.ForeColor := phytomerAttachedTo.internodeColor;
    turtle.drawingSurface.BackColor := phytomerAttachedTo.internodeColor;
    end
  else
    begin
    turtle.drawingSurface.ForeColor := plant.pAxillaryBud.faceColor;
    turtle.drawingSurface.BackColor := plant.pAxillaryBud.backfaceColor;
    end;
  turtle.setLineColor (darkerColor(plant.pAxillaryBud.faceColor));
  turtle.setLineWidth(1.0);
  scale := (plant.pAxillaryBud.scale / 100.0) * (Min(1.0, (age / daysToFullSize)));
  if numParts > 0 then
    for i := 0 to numParts - 1 do
    	begin
			turtle.RotateX(256  div numParts);
    	turtle.push;
    	turtle.RotateZ(-64);
      if plant.pAxillaryBud.object3D <> nil then
    		plant.pAxillaryBud.object3D.DrawWithTurtleScale(turtle, scale);
    	turtle.pop;
    	end;
except
	on EDivByZero do ErrorMessage('GsMeristem.draw: EDivByZero');
	on EInvalidOp do ErrorMessage('GsMeristem.draw: EInvalidOp');
	on EOverFlow do ErrorMessage('GsMeristem.draw: EOverFlow');
	on EUnderFlow do ErrorMessage('GsMeristem.draw: EUnderFlow');
end;
  end;

procedure GsMeristem.startReproduction;
  begin
  {Decide gender and activity based on whether the plant has hermaphroditic or separate flowers, and if
    hermaphroditic, female and/or male flowers are located terminally or axially.}
  if (plant.randomNumberGenerator.zeroToOne <= plant.pMeristem.determinateProbability) then
    self.setIfReproductive(true);
  if not isReproductive then exit;
  self.setIfActive(false);
  Gender := kGenderUnassigned;
  if not plant.pGeneral.maleFlowersAreSeparate then
    begin
    if self.decideIfActiveHermaphroditic then Gender := kGenderHermaphroditic;
    end
  else
    begin
    if self.decideIfActiveMale then Gender := kGenderMale;
    if self.decideIfActiveFemale then Gender := kGenderFemale;
    end;
  if not (gender = kGenderUnassigned) and self.willCreateInflorescence then
    self.setIfActive(true);
  if (gender = kGenderFemale) or (gender = kGenderHermaphroditic) then
    genderIndex := kGenderFemale
  else
    genderIndex := kGenderMale;
  end;

function GsMeristem.decideIfActiveFemale: boolean;
  begin
  {For the case of separate male and female flowers, decide if this meristem will be able to create
  a female inflorescence. Called by decideReproductiveGenderAndActivity.}
  {If meristem is already male, then both male and female flowers are apical (or axillary).
  in that case, override with female only half the time.}
  result := (isApical = plant.pInflor[kGenderFemale].isTerminal);
  if result and (gender = kGenderMale) then
    result := (plant.randomNumberGenerator.zeroToOne < 0.5);
  end;

function GsMeristem.decideIfActiveHermaphroditic: boolean;
  begin
  {For the case of hermaphroditic flowers, decide if this meristem will be able to create
  a hermaphroditic inflorescence (if flowers are hermaphroditic, female parameters are used).
  Called by decideReproductiveGenderAndActivity.}
  result := (isApical = plant.pInflor[kGenderFemale].isTerminal);
  end;

function GsMeristem.decideIfActiveMale: boolean;
  begin
  {For the case of separate male and female flowers, decide if this meristem will be able to create
  a male inflorescence. Called by decideReproductiveGenderAndActivity.}
  result := (isApical = plant.pInflor[kGenderMale].isTerminal);
  end;

function GsMeristem.partType: integer;
  begin
  result := kPartTypeMeristem;
  end;

function GsMeristem.harvestItemTemplate: GsHarvestItemTemplate;
  begin
  { NOTE: we have decided you can't harvest (eat) an apical bud. }
  if (not self.isApical) then
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeAxillaryBud]
  else
    result := nil;
  end;

function GsMeristem.willCreateInflorescence: boolean;
  var
    inflorProb: single;
    numExpected: single;
    numAlready: single;
  begin
  try
  {Determine probability that this meristem will produce an inflorescence, which is
  number of inflorescences left to be placed on plant / number of meristems open to develop
  (apical meristems if flowering is apical, or axillary meristems if flowering is axillary).
  First phytomer on plant is for seedling leaves and cannot produce inflorescences.}
  if (phytomerAttachedTo.isFirstPhytomer) and (not isApical) then
    begin
    result := false;
    exit;
    end;
  if isApical then
    begin
    numExpected := plant.pGeneral.numApicalInflors;
    numAlready := plant.numApicalActiveReproductiveMeristemsOrInflorescences;
    inflorProb := safediv(numExpected - numAlready, plant.numApicalInactiveReproductiveMeristems);
    end
  else
    begin
    numExpected := plant.pGeneral.numAxillaryInflors;
    numAlready := plant.numAxillaryActiveReproductiveMeristemsOrInflorescences;
    inflorProb := safediv(numExpected - numAlready, plant.numAxillaryInactiveReproductiveMeristems);
    end;
  {If there are only a few inflorescences on the plant, don't mess with probability; place on
  first few meristems.}
  if (numExpected <= 3) then inflorProb := 1.0;
  if (plant.randomNumberGenerator.zeroToOne < inflorProb) then
    {Check that the expected number of inflorescences hasn't been created already.}
     result := (numAlready < numExpected)
  else
    result := false;
  except
    result := false;
    errorMessage('GsMeristem.willCreateInflorescence: problem');
  end;
  end;

procedure GsMeristem.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsMeristem;
  cvir.versionNumber := 1;
  cvir.additionNumber := 0;
  end;

procedure GsMeristem.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamLongint(daysCreatingThisPlantPart);
  filer.streamBoolean(isActive);
  filer.streamBoolean(isApical);
  filer.streamBoolean(isReproductive);
  filer.streamSmallint(genderIndex);
  end;

end.

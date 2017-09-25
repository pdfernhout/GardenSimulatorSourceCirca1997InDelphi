unit Udpinflo;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udpinflo: Inflorescence object. Connected to phytomer at base, one or more flower/fruit
objects at end. Handles creation of flowers (according to timetable), draws itself and
places flowers (they draw themselves). Can be harvested whole with all flowers/fruits.
Uses recursion to draw either as tiny little plant (with or w/o branches) or as head.
The 'head' shape doesn't have ray flowers and center flowers but draws flowers in a circle,
each having one long petal, so it looks like ray flowers. Grows in size linearly with
some influence from water stress only on the day it is created.}

interface

uses WinProcs, WinTypes, Classes, udppart, ucollect, udpfruit, udplant, ufiler, uharvprt;

type

GsInflorescence = class(GsDrawingPlantPart)
  public
  flowers: TListCollection;
  unused1: single;
  daysSinceStartedMakingFlowers: longint;
  numFlowersEachDay: longint;
  daysBetweenFlowerAppearances: longint;
  daysSinceLastFlowerAppeared: longint;
  isApical: boolean;
  meristemThatCreatedMe: GsDrawingPlantPart;
  fractionOfOptimalSizeWhenCreated: single;
  drawColor: TColorRef;
  constructor create; override;
	function allFlowersHaveBeenDrawn: boolean;
	procedure createFlower;
	procedure deleteFlower(theFlower: GsFlowerFruit);
	procedure draw; override;
  function shouldDraw: boolean;
	procedure drawApex(internodeCount: integer);
	procedure drawAxillaryBud(internodeCount: integer);
	procedure drawFlower(internodeCount: integer);
	procedure drawHead;
	procedure drawInternode(internodeCount: integer);
	procedure drawPeduncle;
  function lengthOrWidthAtAgeForFraction(starting, fraction: single): single;
	procedure initializeGenderApicalOrAxillary(aPlant: GsDrawingPlant; aGender: integer; initAsApical: boolean;
    fractionOfOptimalSize: single);
	procedure nextDay;  override;
	function pullAngleForFlower(aFlower: GsFlowerFruit): single;
	function pullAngleForWholeInflorescence: single;
	function partType: integer; override;
  function harvestItemTemplate: GsHarvestItemTemplate; override;
  function flower(index: integer): GsFlowerFruit;
  destructor destroy; override;
  procedure traverseActivity(mode: integer; traverser: GsTraverser); override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  class function optimalInitialBiomass_kg(drawingPlant: GsDrawingPlant; genderIndex: integer): single;
  procedure addDependentPartsToList(aList: TList); override;
  procedure addOrRemove(addOrRemoveFlag: boolean); override;
  function liveBiomassForHarvest_kg: single; override;
  function deadBiomassForHarvest_kg: single; override;
  function harvestItemIncludeInWholePlantHarvestBiomass: boolean; override;
  procedure blendColorsStrength(aColor: TColorRef; aStrength: single); override;
  procedure setColorsToParameters; override;
  end;

implementation

uses SysUtils, Dialogs, udebug, uturt3d, ueutils, ueplant, uclasses;

{ ----------------------------------------------------------------------------------------- inflorescence }
constructor GsInflorescence.create;
  begin
  inherited create;
  Flowers := TListCollection.create;
  meristemThatCreatedMe := nil;
  end;

destructor GsInflorescence.destroy;
	begin
  flowers.free;
  flowers := nil;
  meristemThatCreatedMe.free;
  meristemThatCreatedMe := nil;
  inherited destroy;
  end;

procedure GsInflorescence.InitializeGenderApicalOrAxillary(aPlant: GsDrawingPlant; aGender: integer;
	initAsApical: boolean; fractionOfOptimalSize: single);
  var
    daysToAllFlowers: single;
    numFlowers: single;
  begin
  try
  Initialize(aPlant);
  gender := aGender;
  isApical := initAsApical;
  daysSinceLastFlowerAppeared := 0;
  daysSinceStartedMakingFlowers := 0;
  if (gender = kGenderMale) then
    genderIndex := kGenderMale
  else
    genderIndex := kGenderFemale;
  self.fractionOfOptimalSizeWhenCreated := min(1.0, fractionOfOptimalSize);
  {The inflorescence must know whether it produces flowers slowly (over a greater number of days than flowers)
  or produces many flowers in a few days.}
  daysToAllFlowers := plant.pInflor[genderIndex].daysToAllFlowersCreated;
  numFlowers := round(min(1.0, max(0.0, fractionOfOptimalSize)) * plant.pInflor[genderIndex].numFlowers);
  daysBetweenFlowerAppearances := 0;
  numFlowersEachDay := 0;
  if numFlowers > 0 then
    begin
    if numFlowers = 1 then
      numFlowersEachDay := 1
    else if numFlowers = daysToAllFlowers then
      numFlowersEachDay := 1
    else if numFlowers > daysToAllFlowers then
      numFlowersEachDay := round(safedivExcept(1.0 * numFlowers, 1.0 * daysToAllFlowers, 0))
    else
      daysBetweenFlowerAppearances := round(safedivExcept(1.0 * daysToAllFlowers, 1.0 * numFlowers, 0))
    end;
  except
    errorMessage('Problem in InitializeGenderApicalOrAxillary');
  end;
  end;

class function GsInflorescence.optimalInitialBiomass_kg(drawingPlant: GsDrawingPlant;
  genderIndex: integer): single;
  begin
  if (genderIndex < 0) or (genderIndex > 1) then
    result := 0.0
  else
    with drawingPlant.pInflor[genderIndex] do
      result := optimalBiomass_kg * minFractionOfOptimalBiomassToCreateInflorescence_frn;
  end;

procedure GsInflorescence.nextDay;
  var
    i, numFlowersToCreateToday: integer;
    biomassToMakeFlowers_kg: single;
  begin
  {The inflorescence is very simple; it creates a specified number of flowers over a specified period
  of days. Each inflorescence on the plant has the same number of flowers. Since an inflorescence is
  created by a meristem which accumulates biomass, nothing stands in the way of the inflorescence
  producing the flowers according to schedule.
  This method must act differently for inflorescences that produce flowers slowly (over a greater
  number of days than flowers) than for those that produce many flowers in a few days.
  The inflorescence can create no flowers until it reaches a specified fraction of its optimal
  biomass.}
  try
  if self.age < plant.pInflor[genderIndex].maxDaysToGrow then
    begin
    with plant.pInflor[genderIndex] do
      biomassToMakeFlowers_kg := minFractionOfOptimalBiomassToMakeFlowers_frn * optimalBiomass_kg;
    if self.liveBiomass_kg < biomassToMakeFlowers_kg then exit;
    end;
  inc(daysSinceStartedMakingFlowers);
  if (flowers.count < plant.pInflor[genderIndex].numFlowers) then
    begin
    if (daysBetweenFlowerAppearances > 0) then
      begin
      if (daysSinceLastFlowerAppeared >= daysBetweenFlowerAppearances) then
        createFlower
      else
        inc(daysSinceLastFlowerAppeared);
      end
    else
      begin
      numFlowersToCreateToday := intMin(numFlowersEachDay, plant.pInflor[genderIndex].numFlowers - flowers.count);
      if numFlowersToCreateToday > 0 then for i := 0 to numFlowersToCreateToday - 1 do createFlower;
      daysSinceLastFlowerAppeared := 0;
      end;
    end;
  if flowers.count > 0 then for i := 0 to flowers.count - 1 do GsFlowerFruit(flowers.items[i]).nextDay;
  inherited nextDay;
  except
    ErrorMessage('GsInflorescence.nextDay: Problem in inflorescence next day');
  end;
  end;

procedure GsInflorescence.traverseActivity(mode: integer; traverser: GsTraverser);
  var
    originalTotal, newBiomass_kg, biomassToRemove_kg: single;
    i: integer;
  begin
  if self.isRemoved and (mode <> kActivityStream) and (mode <> kActivityFree) then exit;
  try
  originalTotal := traverser.total;
  if (mode <> kActivityDraw) and (mode <> kActivityAddMatureHarvestedPartsToList)
    then if flowers.count > 0 then for i := 0 to flowers.count - 1 do
    (GsFlowerFruit(flowers.items[i])).traverseActivity(mode, traverser);
  with plant.pInflor[genderIndex] do
  case mode of
    kActivityNone: ;
    kActivityNextDay: self.nextDay;
    kActivityDemandVegetative_kg: { no vegetative demand };
    kActivityDemandReproductive_kg:
      begin
      if (self.age > maxDaysToGrow) then
        begin
        self.biomassDemand_kg := 0.0;
        exit;
        end;
      try
      self.biomassDemand_kg := linearGrowthResult(self.liveBiomass_kg,
        optimalBiomass_kg, minDaysToGrow);
      traverser.total := traverser.total + self.biomassDemand_kg;
      except
        ErrorMessage('GsInflorescence.traverseActivity: Problem in inflorescence demandReproductive');
      end
      end;
    kActivityGrowVegetative: { no vegetative growth };
    kActivityGrowReproductive: 
      begin
      if self.age > maxDaysToGrow then exit;
      newBiomass_kg := self.biomassDemand_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg + newBiomass_kg;
      end;
    kActivityStartReproduction: { cannot switch };
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
      if self.canBeHarvested then
        begin
        traverser.foundList.add(self);
        {add all flower/fruits whether ripe or not - if they could not be harvested otherwise}
        if flowers.count > 0 then
          for i := 0 to flowers.count - 1 do
            if not GsFlowerFruit(flowers.items[i]).isRemoved then
              traverser.foundList.add(flowers.items[i]);
        end
      else if flowers.count > 0 then for i := 0 to flowers.count - 1 do
        (GsFlowerFruit(flowers.items[i])).traverseActivity(mode, traverser);
      end;
    kActivityFindAgeOfYoungestPhytomer: { not a phytomer };
    kActivityDraw: self.draw;
    kActivityReport: 
      begin
      { note flowers will print first }
      debugPrint('inflorescence, age '  + IntToStr(age));
      {DebugForm.printNested(plant.turtle.stackSize, 'inflorescence, age '  + IntToStr(age));}
      end;
    kActivityStream: {streaming called by phytomer};
    kActivityFree: { free called by phytomer };
    kActivityVegetativeBiomassThatCanBeRemoved_kg: { none };
    kActivityRemoveVegetativeBiomass: { none };
    kActivityReproductiveBiomassThatCanBeRemoved_kg:
      begin
      traverser.total := traverser.total + self.liveBiomass_kg;
      end;
    kActivityRemoveReproductiveBiomass:
      begin
      biomassToRemove_kg := self.liveBiomass_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg - biomassToRemove_kg;
      self.deadBiomass_kg := self.deadBiomass_kg + biomassToRemove_kg;
      end;
    kActivityAddBundledPartsToList:
      begin
      if self.harvestItemIncludeInWholePlantHarvestBiomass then traverser.foundList.add(self);
      end;
    kActivityGatherStatistics:
      begin
      if gender = kGenderMale then
        self.addToStatistics(traverser.statistics, kHarvestPartTypeMaleInflorescence)
      else
        self.addToStatistics(traverser.statistics, kHarvestPartTypeFemaleInflorescence);
      self.addToStatistics(traverser.statistics, kStatisticsPartTypeAllReproductive);
      end;
    kActivityStandingDeadBiomassThatCanBeRemoved_kg:
      inherited traverseActivity(mode, traverser);
    kActivityRemoveStandingDeadBiomass:
      inherited traverseActivity(mode, traverser);
    else
      raise Exception.create('unhandled mode for GsInflorescence activity');
    end;
  except
    ErrorMessage('GsInflorescence.traverseActivity: Problem in inflorescence traverseActivity');
  end;
  end;

function GsInflorescence.harvestItemIncludeInWholePlantHarvestBiomass: boolean;
  begin
  if gender = kGenderMale then
    result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeMaleInflorescence]
  else
    result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeFemaleInflorescence];
  end;

procedure GsInflorescence.addDependentPartsToList(aList: TList);
  var i: longint;
  begin
  if flowers.count > 0 then
    for i := 0 to flowers.count - 1 do
      aList.add(flowers.items[i]);
  end;

function GsInflorescence.allFlowersHaveBeenDrawn: boolean;
  var
    allFlowersHaveBeenDrawn: boolean;
    i: integer;
  begin
  {Survey flowers and return true if all flowers have been drawn (they know), false if any have not been
   drawn. This is mostly so a branched inflorescence can know if there are any flowers left to place
   on its branched structure.}
  allFlowersHaveBeenDrawn := true;
  if flowers.count > 0 then
    for i := 0 to flowers.count - 1 do
      if not ((GsFlowerFruit(flowers.items[i])).hasBeenDrawn) then allFlowersHaveBeenDrawn := false;
  result := allFlowersHaveBeenDrawn;
  end;

procedure GsInflorescence.createFlower;
	var
	aFlowerFruit: GsFlowerFruit;
  begin
  { create new flower/fruit object }
  aFlowerFruit := GsFlowerFruit.create;
  aFlowerFruit.InitializeGender(plant, gender);
  flowers.Add(aFlowerFruit);
  end;

procedure GsInflorescence.deleteFlower(theFlower: GsFlowerFruit);
  begin
  { remove flower object from list }
  flowers.Remove(theFlower);
  end;

procedure GsInflorescence.draw;
  var i: integer;
  begin
  if not self.shouldDraw then exit;
  try
   self.recordPositionFromTurtleIfDesired;
  except
  end;
  try
    self.calculateColors;
    if flowers.count > 0 then
      for i := 0 to flowers.count - 1 do
    	  GsFlowerFruit(flowers.items[i]).HasBeenDrawn := false;
    if (flowers.count > 0) then
      begin
      if (plant.pInflor[genderIndex].isHead) then
        self.drawHead
      else
        self.DrawApex(flowers.count);
      end;
  except
	  on EDivByZero do ErrorMessage('GsInflorescence.draw EDivByZero');
	  on EInvalidOp do ErrorMessage('GsInflorescence.draw EInvalidOp');
	  on EOverFlow do ErrorMessage('GsInflorescence.draw EOverFlow');
	  on EUnderFlow do ErrorMessage('GsInflorescence.draw EUnderFlow');
  end;
  end;

function GsInflorescence.shouldDraw: boolean;
  var
    i: longint;
    flowerFruit: GsFlowerFruit;
  begin
  { if inflorescence has at least one flower or non-fallen fruit, should draw }
  result := true;
  if flowers.count > 0 then
    for i := 0 to flowers.count - 1 do
      begin
      flowerFruit := GsFlowerFruit(flowers.items[i]);
      if flowerFruit = nil then continue;
      if (flowerFruit.stage <> kStageFallenFruit) and (flowerFruit.stage <> kStageFallenFlower) then
        exit;
      end;
  result := false;
  end;

procedure GsInflorescence.DrawApex(internodeCount: integer);
  var
    turtle: KfTurtle;
    var i: integer;
  begin
  {Draw inflorescence in raceme, panicle, or umbel form. This method uses the recursive algorithm
     we first developed to draw the entire plant.}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  if (internodeCount = flowers.count) then drawPeduncle;
  if internodeCount > 0 then
    for i := internodeCount downto 1 do
      if (i = 1) then
        begin
        DrawInternode(i);
        turtle.push;
        DrawFlower(i);
        turtle.pop;
        end
      else
        begin
        if (plant.pInflor[genderIndex].flowersSpiralOnStem) then turtle.RotateX(98); {param}
        DrawInternode(i);
        turtle.push;
        DrawFlower(i);
        turtle.pop;
        {The decision to create branches to the inflorescence is not probabilistic; it is
        based purely on a parameter of how many branches there are on the inflorescence.}
        if (internodeCount = flowers.count) and
        	(i > Max(0, flowers.count - plant.pInflor[genderIndex].numBranches)) then
            begin
            DrawAxillaryBud(i);
            if not plant.pInflor[genderIndex].branchesAreAlternate then
              begin
              turtle.push;
              turtle.rotateX(128);
              DrawAxillaryBud(i);
              turtle.pop;
              end;
            end;
        end;
  end;

procedure GsInflorescence.BlendColorsStrength(aColor: TColorRef; aStrength: single);
  begin
  if aStrength <= 0.0 then exit;
  drawColor := blendColors(drawColor, aColor, aStrength);
  end;

procedure GsInflorescence.setColorsToParameters;
  begin
  drawColor := plant.pInflor[genderIndex].stalkColor;
  end;

procedure GsInflorescence.drawPeduncle;
  var
    length, width: single;
  begin
  try
  {Draw peduncle, which is the primary inflorescence stalk. If the inflorescence has only one flower,
     this is the only part drawn. If the inflorescence is apical, the stalk may be longer (e.g. in bolting
     plants) and is specified by a different parameter.}
  with plant.pInflor[genderIndex] do
    begin
    if isApical then
      length := lengthOrWidthAtAgeForFraction(terminalStalkLength_mm, fractionOfOptimalSizeWhenCreated)
    else
      length := lengthOrWidthAtAgeForFraction(peduncleLength_mm, fractionOfOptimalSizeWhenCreated);
    width := lengthOrWidthAtAgeForFraction(internodeWidth_mm, fractionOfOptimalSizeWhenCreated);
    {Use no angle here because phytomer makes the rotation before the inflorescence is drawn.}
    DrawLengthInPiecesWidthAngleZAngleYColor(length, width, 0, 0, drawColor);
    end;
  except
    errorMessage('Problem in GsInflorescence.drawPeduncle');
  end;
  end;

procedure GsInflorescence.DrawAxillaryBud(internodeCount: integer);
  var
    turtle: KfTurtle;
    angle: single;
  begin
  {This message is sent when the inflorescence is branched. The decision to create a branch is
     made before the message is sent. Note the check if all flowers have been drawn; this prevents
     scrawly lines with no flowers.}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  if (allFlowersHaveBeenDrawn) then exit;
  angle := plant.pInflor[genderIndex].branchAngle;
  { Give angle a little random sway }
  if not GsPlant(plant.model).development.isDead then
    angle := angle + ((plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway);
  turtle.push;
  turtle.RotateZ(angle);
  DrawApex(internodeCount div 2);
  turtle.pop;
  end;

procedure GsInflorescence.DrawFlower(internodeCount: integer);
  var
    length, width, angle: single;
    flowerIndex: integer;
  begin
  try
  {Draw one flower, remembering that internodeCount goes from flowers size down to 1,
  but flowers are in the order formed.
  If the oldest flowers on the inflorescence are at the bottom (acropetal),
  draw the flowers in reverse order from internodeCount (which is in the order they were formed).
  If the oldest flowers on the inflorescence are at the top (basipetal),
  draw the flowers in the order presented by internodeCount
     which is in reverse order to how they were formed).
  In most plants the older flowers are lower.}
  if (allFlowersHaveBeenDrawn) then exit;
  with plant.pInflor[genderIndex] do
    begin
    length := lengthOrWidthAtAgeForFraction(pedicelLength_mm, fractionOfOptimalSizeWhenCreated);
    width := lengthOrWidthAtAgeForFraction(internodeWidth_mm, fractionOfOptimalSizeWhenCreated);
    if (internodeCount = 1) then
      angle := 0
    else
      angle := pedicelAngle;
    {Give angle a little random sway.}
    if not GsPlant(plant.model).development.isDead then
      angle := angle + ((plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway);
    if (plant.pInflor[genderIndex].flowersDrawTopToBottom) then
      flowerIndex := internodeCount
    else
      flowerIndex := flowers.count - internodeCount + 1;
    DrawLengthInPiecesWidthAngleZAngleYColor(length, width, angle, 0, drawColor);
    end;
  if (flowerIndex - 1 >= 0) and (flowerIndex - 1 <= flowers.count - 1) then
    GsFlowerFruit(flowers.items[flowerIndex-1]).draw;
  except
    errorMessage('Problem in GsInflorescence.DrawFlower');
  end;
  end;

procedure GsInflorescence.drawHead;
  var
    turtle: KfTurtle;
    var i: integer;
  begin
  {Draw the inflorescences in a radial pattern; this is for a head such as a sunflower.}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  drawPeduncle;
  turtle.RotateY(64);
  turtle.RotateZ(64);
  { give a little angle down to make it look more natural }
  turtle.RotateY(32);
  if flowers.count > 0 then
    for i := flowers.count - 1 downto 0 do
    	begin
			turtle.RotateZ(256.0  / flowers.count);
    	turtle.push;
    	DrawFlower(i);
    	turtle.pop;
   	 end;
  end;

procedure GsInflorescence.DrawInternode(internodeCount: integer);
  var
    length, width, angle: single;
  begin
  try
  {Draw the inflorescence internode, which is the portion of inflorescence stem between where successive
  flower pedicels come off. Note that this is not drawn if all flowers have been drawn; this prevents
  straggly lines in branched inflorescences.}
  if (allFlowersHaveBeenDrawn) then exit;
  with plant.pInflor[genderIndex] do
    begin
    if (internodeLength_mm = 0.0) then exit;
    length := lengthOrWidthAtAgeForFraction(internodeLength_mm, fractionOfOptimalSizeWhenCreated);
    width :=  lengthOrWidthAtAgeForFraction(internodeWidth_mm, fractionOfOptimalSizeWhenCreated);
    angle := 0;
    {Give angle a little random sway.}
    if not GsPlant(plant.model).development.isDead then
      angle := angle + ((plant.swayRandomNumberGenerator.zeroToOne - 0.5) * plant.pGeneral.randomSway);
    DrawLengthInPiecesWidthAngleZAngleYColor(length, width, angle, 0, drawColor);
    end;
  except
    errorMessage('Problem in GsInflorescence.DrawInternode');
  end;
  end;

function GsInflorescence.lengthOrWidthAtAgeForFraction(starting, fraction: single): single;
  var ageBounded: single;
  begin
  result := 0.0;
  try
  with plant.pInflor[genderIndex] do
    begin
    ageBounded := min(daysToAllFlowersCreated, daysSinceStartedMakingFlowers);
    if daysToAllFlowersCreated <> 0 then
      result := safediv(starting * ageBounded, daysToAllFlowersCreated) * fraction
    else
      result := starting * ageBounded * fraction;
    end;
  except errorMessage('Problem in GsInflorescence.lengthOrWidthAtAgeForFraction'); end;
  end;

function GsInflorescence.flower(index: integer): GsFlowerFruit;
	begin
  result := flowers.items[index];
  end;

function GsInflorescence.partType: integer;
  begin
  result := kPartTypeInflorescence;
  end;

function GsInflorescence.harvestItemTemplate: GsHarvestItemTemplate;
  begin
  if self.gender = kGenderMale then
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeMaleInflorescence]
  else
    { female or hermaphroditic }
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFemaleInflorescence];
  end;

function GsInflorescence.liveBiomassForHarvest_kg: single;
  var
    flowerFruit: GsFlowerFruit;
    i: longint;
  begin
  result := self.liveBiomass_kg;
  if flowers.count > 0 then
    if not GsFlowerFruit(flowers.items[0]).canEverBeHarvested then
      for i := 0 to flowers.count - 1 do
        begin
        flowerFruit := GsFlowerFruit(flowers.items[i]);
        if not flowerFruit.isRemoved then
          result := result + flowerFruit.liveBiomass_kg;
        end;
  end;

function GsInflorescence.deadBiomassForHarvest_kg: single;
  var
    flowerFruit: GsFlowerFruit;
    i: longint;
  begin
  result := self.deadBiomass_kg;
  if flowers.count > 0 then
    if not GsFlowerFruit(flowers.items[0]).canEverBeHarvested then
      for i := 0 to flowers.count - 1 do
        begin
        flowerFruit := GsFlowerFruit(flowers.items[i]);
        if not flowerFruit.isRemoved then
          result := result + flowerFruit.deadBiomass_kg;
        end;
  end;

procedure GsInflorescence.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsInflorescence;
  cvir.versionNumber := 2;
  cvir.additionNumber := 2;
  end;

procedure GsInflorescence.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
   i: longint;
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamSingle(unused1);
  filer.streamLongint(daysSinceStartedMakingFlowers);
  filer.streamLongint(numFlowersEachDay);
  filer.streamLongint(daysBetweenFlowerAppearances);
  filer.streamLongint(daysSinceLastFlowerAppeared);
  filer.streamBoolean(isApical);
  if filer.isReading then meristemThatCreatedMe := nil;
  flowers.streamUsingFiler(filer, GsFlowerFruit);
  {fixup plant in flowers if needed}
  if filer.isReading and (flowers.count > 0) then
    for i := 0 to flowers.count - 1 do
      (GsFlowerFruit(flowers.items[i])).plant := self.plant;
  if cvir.additionNumber >= 1 then
    filer.streamSingle(fractionOfOptimalSizeWhenCreated)
  else
    if filer.isReading then fractionOfOptimalSizeWhenCreated := 1.0;
  if cvir.additionNumber >= 2 then
    filer.streamColorRef(drawColor)
  else
    if filer.isReading then drawColor := plant.pInflor[genderIndex].stalkColor;
  end;

{ ---------------------------------------------------------------------- Wilting and falling functions }
function GsInflorescence.PullAngleForFlower(aFlower: GsFlowerFruit): single;
  var
    angle: single;
    proportion: single;
  begin
  {Return the downward angle generated by the pull of the associated flower/fruit, which comes from
  its weight. The angle is calculated so that a fruit of optimal biomass will pull directly downward.}
  {Problem: sometimes falls too far, sometimes not far enough.}
  result := 0;
  if (aFlower = nil) then exit;
  proportion := 0;
  proportion := aFlower.totalBiomass_kg /
  	(plant.pFruit.stalkStrengthIndex / 100  * plant.pFruit.optimalBiomass_kg);
  angle := proportion * 64;
  {self plant turtle angleY. }
  if (angle < 0) then angle := angle * -1;
  result := angle;
  end;

function GsInflorescence.pullAngleForWholeInflorescence: single;
  var
    angle: single;
  var i: integer;
  begin
  {Return the downward angle generated by the pull of all flower/fruit objects associated with this
  inflorescence.}
  result := 0;
  if (flowers = nil) then exit;
  angle := 0;
  if flowers.count > 0 then for i := 0 to flowers.count - 1 do
    angle := angle + PullAngleForFlower(flower(i));
  result := angle;
  end;

procedure GsInflorescence.addOrRemove(addOrRemoveFlag: boolean);
  var i: longint;
  begin
  inherited addOrRemove(addOrRemoveFlag);
  if flowers.count > 0 then
    if not GsFlowerFruit(flowers.items[0]).canEverBeHarvested then
      for i := 0 to flowers.count - 1 do
        begin
        GsFlowerFruit(flowers.items[i]).hasBeenHarvestedWithInflorescence :=
          (addOrRemoveFlag = kRemovingBiomassFromPlant);
        end;
  end;

end.
 
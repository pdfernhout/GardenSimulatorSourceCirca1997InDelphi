unit Udpfruit;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udpfruit: Flower/fruit object. Connected to inflorescence. Stages are bud, open flower,
unripe fruit, ripe fruit, fallen flower (if flower drops), fallen fruit (if fruit drops).
Flower grows linearly, fruit grows by s curve. }

interface

uses WinProcs, WinTypes, udppart, uestruct, udplant, ufiler, uturt3d, uharvprt;

type

GsFlowerFruit = class(GsDrawingPlantPart)
  public
  propFullSize: single;
  stage: smallint;
  hasBeenDrawn: boolean;
  daysAccumulatingFruitBiomass: longint;
  hasBeenHarvestedWithInflorescence: boolean;
	procedure draw; override;
  procedure drawTDO(tdo: KfObject3D; faceColor, backfaceColor: TColorRef;
    scale: single; numParts: integer; partsArranged: boolean; open: boolean);
  procedure dragDownFromWeight;
	procedure initializeGender(aPlant: GsDrawingPlant; aGender: integer);
	procedure nextDay; override;
	function partType: integer; override;
  procedure traverseActivity(mode: integer; traverser: GsTraverser); override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  function harvestItemTemplate: GsHarvestItemTemplate; override;
  function canEverBeHarvested: boolean;
  function liveBiomassForHarvest_kg: single; override;
  function deadBiomassForHarvest_kg: single; override;
  function harvestItemIncludeInWholePlantHarvestBiomass: boolean; override;
  end;

implementation

uses SysUtils, Dialogs, udebug, ueutils, ueplant, uclasses;

{ ---------------------------------------------------------------------------------------- flower-fruit }
procedure GsFlowerFruit.InitializeGender(aPlant: GsDrawingPlant; aGender: integer);
  begin
  initialize(aPlant);
  gender := aGender;
  propFullSize := 0.0;
  liveBiomass_kg := 0.0;
  deadBiomass_kg := 0.0;
  biomassDemand_kg := 0.0;
  stage := kStageFlowerBud;
  hasBeenDrawn := false;
  if (gender = kGenderMale) then
    genderIndex := kGenderMale
  else
    genderIndex := kGenderFemale; { could be hermaphroditic }
  hasBeenHarvestedWithInflorescence := false;
  end;

procedure GsFlowerFruit.nextDay;
  var
    anthesisLoss_kg: single;
  begin
  try
  case self.stage of
    kStageFlowerBud: {if over required fraction of optimal or over max days to grow, open bud}
      begin
      with plant.pFlower[genderIndex] do
        if (liveBiomass_kg >= minFractionOfOptimalBiomassToOpenFlower_frn * optimalBiomass_kg)
           or (age > maxDaysToGrowIfOverMinFraction) then
           stage := kStageOpenFlower;
      end;
    kStageOpenFlower: {if over optimal or over min fraction to create fruit and over max days to grow, set fruit}
      begin
      if age > plant.pFlower[genderIndex].daysBeforeDrop then
        stage := kStageFallenFlower
      else if gender <> kGenderMale then
        begin
        with plant.pFlower[genderIndex] do
          if (liveBiomass_kg >= optimalBiomass_kg)
            or ((liveBiomass_kg >= minFractionOfOptimalBiomassToCreateFruit_frn * optimalBiomass_kg)
              and (age > maxDaysToGrowIfOverMinFraction)) then
              begin
              stage := kStageUnripeFruit;
              daysAccumulatingFruitBiomass := 0;
              { flower biomass drops off, 50% goes into developing fruit (ovary) }
              { choice of 50% is arbitrary - could be parameter in future depending on size of flower parts/ovary }
              anthesisLoss_kg := self.liveBiomass_kg * 0.5;
              liveBiomass_kg := liveBiomass_kg - anthesisLoss_kg;
              deadBiomass_kg := deadBiomass_kg + anthesisLoss_kg;
              propFullSize := (Min(1.0, safediv(self.totalBiomass_kg, plant.pFruit.optimalBiomass_kg)));
              end;
        end;
      end;
    kStageUnripeFruit, kStageRipeFruit:
      begin
      if (stage = kStageUnripeFruit) and (daysAccumulatingFruitBiomass >= plant.pFruit.daysToRipen) then
        stage := kStageRipeFruit;
      inc(daysAccumulatingFruitBiomass);
      end;
    kStageFallenFruit:  ;
    kStageFallenFlower: ;
    end;
  inherited nextDay;
  except
    ErrorMessage('GsFlowerFruit.nextDay: Problem in flower/fruit next day');
  end;
  end;

procedure GsFlowerFruit.traverseActivity(mode: integer; traverser: GsTraverser);
  var
    newPropFullSize, newBiomass_kg, newOptimalBiomass_kg, biomassToRemove_kg: single;
  begin
  if self.isRemoved and (mode <> kActivityStream) and (mode <> kActivityFree) then exit;
  try
  case mode of
    kActivityNone: ;
    kActivityNextDay: self.nextDay;
    kActivityDemandVegetative_kg: { has no vegetative demand };
    kActivityDemandReproductive_kg:
      begin
      case self.stage of
        kStageFlowerBud, kStageOpenFlower: { accum. biomass for flower }
          begin
          with plant.pFlower[genderIndex] do
            self.biomassDemand_kg := linearGrowthResult(liveBiomass_kg, optimalBiomass_kg, minDaysToGrow);
          traverser.total := traverser.total + self.biomassDemand_kg;
          end;
        kStageUnripeFruit, kStageRipeFruit: { accum. biomass for fruit }
          begin
          if self.daysAccumulatingFruitBiomass > plant.pFruit.maxDaysToGrow then
            self.biomassDemand_kg := 0.0
          else
            begin
            newPropFullSize := Max(0.0, Min (1.0, scurve(daysAccumulatingFruitBiomass + 1,
              plant.pFruit.sCurveParams.c1, plant.pFruit.sCurveParams.c2)));
            newOptimalBiomass_kg := newPropFullSize * plant.pFruit.optimalBiomass_kg;
            with plant.pFruit do
              self.biomassDemand_kg := linearGrowthResult(liveBiomass_kg, newOptimalBiomass_kg, 1);
            traverser.total := traverser.total + self.biomassDemand_kg;
            end;
          end;
        kStageFallenFruit: { no demand }
          self.biomassDemand_kg := 0.0;
        kStageFallenFlower: { no demand }
          self.biomassDemand_kg := 0.0;
        end;
      end;
    kActivityGrowVegetative: { cannot grow vegetatively };
    kActivityGrowReproductive:
      begin
      {Allocate portion of total new biomass based on this demand over total demand.}
      if self.stage = kStageFallenFruit then exit;
      if self.stage = kStageFallenFlower then exit;
      newBiomass_kg := self.biomassDemand_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg + newBiomass_kg;
      case self.stage of
        kStageFlowerBud, kStageOpenFlower:
          propFullSize := (Min(1.0, safediv(self.totalBiomass_kg, plant.pFlower[genderIndex].optimalBiomass_kg)));
        kStageUnripeFruit, kStageRipeFruit:
          propFullSize := (Min(1.0, safediv(self.totalBiomass_kg, plant.pFruit.optimalBiomass_kg)));
        end;
      end;
    kActivityStartReproduction: { can't switch because has no vegetative mode };
    kActivityFindPlantPartAtPosition:
      begin
      if stage = kStageFallenFruit then exit;
      if stage = kStageFallenFlower then exit;
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
    kActivityDraw: { inflorescence should handle telling flowers to draw };
    kActivityReport:
      begin
      DebugPrint(' flower/fruit, age '  + IntToStr(age) + ' biomass '  + floatToStr(self.liveBiomass_kg));
      end;
    kActivityStream: {streaming called by inflorescence};
    kActivityFree: { free called by inflorescence };
    kActivityVegetativeBiomassThatCanBeRemoved_kg: { none };
    kActivityRemoveVegetativeBiomass: { do nothing };
    kActivityReproductiveBiomassThatCanBeRemoved_kg:
      begin
      if (self.stage = kStageFallenFruit) then exit;
      if (self.stage = kStageFallenFlower) then exit;
      traverser.total := traverser.total + self.liveBiomass_kg;
      end;
    kActivityRemoveReproductiveBiomass:
      begin
      if self.liveBiomass_kg <= 0.0 then exit;
      biomassToRemove_kg := self.liveBiomass_kg * traverser.fractionOfPotentialBiomass;
      self.liveBiomass_kg := self.liveBiomass_kg - biomassToRemove_kg;
      self.deadBiomass_kg := self.deadBiomass_kg + biomassToRemove_kg;
      if self.liveBiomass_kg <= 0.0 then
        begin
        if (stage = kStageUnripeFruit) or (stage = kStageRipeFruit) then
          stage := kStageFallenFruit
        else
          stage := kStageFallenFlower;
        end;
      end;
    kActivityAddBundledPartsToList:
      begin
      if self.harvestItemIncludeInWholePlantHarvestBiomass then traverser.foundList.add(self);
      end;
    kActivityGatherStatistics:
      begin
      case stage of
        kStageFlowerBud:
          if gender = kGenderMale then
            self.addToStatistics(traverser.statistics, kHarvestPartTypeMaleFlowerBud)
          else
            self.addToStatistics(traverser.statistics, kHarvestPartTypeFemaleFlowerBud);
        kStageOpenFlower:
          if gender = kGenderMale then
            self.addToStatistics(traverser.statistics, kHarvestPartTypeMaleFlower)
          else
            self.addToStatistics(traverser.statistics, kHarvestPartTypeFemaleFlower);
        kStageUnripeFruit: self.addToStatistics(traverser.statistics, kStatisticsPartTypeUnripeFruit);
        kStageRipeFruit: self.addToStatistics(traverser.statistics, kHarvestPartTypeFruit);
        kStageFallenFruit: self.addToStatistics(traverser.statistics, kStatisticsPartTypeFallenFruit);
        kStageFallenFlower: self.addToStatistics(traverser.statistics, kStatisticsPartTypeFallenFlower);
        else
          raise Exception.create('GsFlowerFruit.traverseActivity: invalid fruit stage');
        end;
      self.addToStatistics(traverser.statistics, kStatisticsPartTypeAllReproductive);
      end;
    kActivityStandingDeadBiomassThatCanBeRemoved_kg:
      inherited traverseActivity(mode, traverser);
    kActivityRemoveStandingDeadBiomass:
      inherited traverseActivity(mode, traverser);
    else
      raise Exception.create('unhandled mode for plant draw activity');
    end;
  except
    ErrorMessage('GsFlowerFruit.traverseActivity: Problem in flower/fruit traverseActivity');
  end;
  end;

function GsFlowerFruit.harvestItemIncludeInWholePlantHarvestBiomass: boolean;
  begin
  result := false;
  case stage of
    kStageFlowerBud:
      if gender = kGenderMale then
        result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeMaleFlowerBud]
      else
        result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeFemaleFlowerBud];
    kStageOpenFlower:
      if gender = kGenderMale then
        result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeMaleFlower]
      else
        result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeFemaleFlower];
    kStageUnripeFruit:
      result := false;
    kStageRipeFruit:
      result := plant.pGeneral.harvestItemIncludeInWholePlantHarvestBiomass[kHarvestPartTypeFruit];
    kStageFallenFruit:
      result := false;
    kStageFallenFlower:
      result := false;
    else
      raise Exception.create('GsMeristem.harvestItemIncludeInWholePlantHarvestBiomass: invalid fruit stage');
    end;
  end;

const
  kDrawTDOOpen = true;
  kDrawTDOClosed = false;

procedure GsFlowerFruit.draw;
  var scale: single;
  begin
  if (plant.turtle = nil) then exit;
  if self.isRemoved then exit;
  try
  self.recordPositionFromTurtleIfDesired;
  except
  end;
  self.dragDownFromWeight;
  case stage of
    kStageFlowerBud:
      with plant.pFlower[genderIndex] do
       begin
        scale := ((budScaleAtFullSize / 100.0) * propFullSize);
        drawTDO(budObject3D, budFaceColor, budBackfaceColor, scale, budNumPetals,
          budPetalsAreRadiallyArranged, kDrawTDOClosed);
        end;
    kStageOpenFlower:
      with plant.pFlower[genderIndex] do
        begin
        scale := ((scaleAtFullSize / 100.0) * propFullSize);
        drawTDO(object3D, faceColor, backfaceColor, scale, numPetals,
          petalsAreRadiallyArranged, kDrawTDOOpen);
        end;
    kStageUnripeFruit:
      begin
      {draw flower and fruit together}
      with plant.pFlower[genderIndex] do
        begin
        scale := ((scaleAtFullSize / 100.0) * 1.0 {draw flower at full size in this stage});
        drawTDO(object3D, faceColor, backfaceColor, scale, numPetals,
          petalsAreRadiallyArranged, kDrawTDOOpen);
        end;
      with plant.pFruit do
        begin
        scale := ((scaleAtFullSize / 100.0) * propFullSize);
        drawTDO(object3D, unripeFaceColor, unripeBackfaceColor, scale, numSections,
          sectionsAreRadiallyArranged, kDrawTDOClosed);
        end;
      end;
    kStageRipeFruit:
     with plant.pFruit do
        begin
        scale := ((scaleAtFullSize / 100.0) * propFullSize);
        drawTDO(object3D, ripeFaceColor, ripeBackfaceColor, scale, numSections,
          sectionsAreRadiallyArranged, kDrawTDOClosed);
        end;
    kStageFallenFruit: ;
    kStageFallenFlower: ;
    end;
  hasBeenDrawn := true;
  end;

procedure GsFlowerFruit.drawTDO(tdo: KfObject3D; faceColor, backfaceColor: TColorRef;
  scale: single; numParts: integer; partsArranged: boolean; open: boolean);
  var
    turtle: KfTurtle;
    i: integer;
  begin
  try
  if (scale <= 0.0) then exit;
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  turtle.push;
  turtle.setLineWidth(1.0);
  turtle.drawingSurface.foreColor := faceColor;
  turtle.drawingSurface.BackColor := backfaceColor;
  turtle.setLineColor(darkerColor(faceColor));
  if partsArranged then
    begin
    for i:= 1 to numParts do
      begin
			turtle.RotateX(256 div numParts);
      turtle.push;
      turtle.RotateZ(-64); {aligns object as stored in the file to way should draw on plant}
      if open then turtle.RotateY(32); {pulls petal up to plane of stalk (is perpendicular)}
      if tdo <> nil then tdo.drawWithTurtleScale(turtle, scale);
      turtle.pop;
      end;
    end
  else
    begin
    turtle.push;
    if open then
      turtle.RotateZ(-32) {pulls petal up to plane of stalk (is perpendicular)}
    else
      turtle.RotateZ(-64);
    if tdo <> nil then tdo.drawWithTurtleScale(turtle, scale);
    turtle.pop;
    end;
  turtle.pop;
except
	on EDivByZero do ErrorMessage('GsFlowerFruit.drawTDO EDivByZero');
	on EInvalidOp do ErrorMessage('GsFlowerFruit.drawTDO EInvalidOp');
	on EOverFlow do ErrorMessage('GsFlowerFruit.drawTDO EOverFlow');
	on EUnderFlow do ErrorMessage('GsFlowerFruit.drawTDO EUnderFlow');
end;
  end;

procedure GsFlowerFruit.dragDownFromWeight;
  var
    angle: integer;
    fractionOfOptimalFruitWeight_frn: single;
  begin
  if (plant.turtle = nil) then exit;
  fractionOfOptimalFruitWeight_frn := safedivExcept(liveBiomass_kg, plant.pFruit.optimalBiomass_kg, 0.0);
  angle := round(abs(plant.turtle.angleZ + 32) * fractionOfOptimalFruitWeight_frn
      * min(1.0, max(0.0, 100 - plant.pFruit.stalkStrengthIndex) / 100.0));
  angle := -angle;
  if plant.turtle.angleZ > -32 then
    angle := -angle;
  plant.turtle.rotateZ(angle);
  end;

function GsFlowerFruit.partType: integer;
  begin
  result := kPartTypeFlowerFruit;
  end;

function GsFlowerFruit.canEverBeHarvested: boolean;
  begin
  result := false;
  if self.gender = kGenderMale then
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeMaleFlowerBud] <> nil
  else
    result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFemaleFlowerBud] <> nil;
  if self.gender = kGenderMale then
    result := result or (plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeMaleFlower] <> nil)
  else
    result := result or (plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFemaleFlower] <> nil);
  result := result or (plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit] <> nil);
  end;

function GsFlowerFruit.liveBiomassForHarvest_kg: single;
  begin
  if not self.hasBeenHarvestedWithInflorescence then
    result := self.liveBiomass_kg
  else
    result := 0.0;
  end;

function GsFlowerFruit.deadBiomassForHarvest_kg: single;
  begin
  if not self.hasBeenHarvestedWithInflorescence then
    result := self.deadBiomass_kg
  else
    result := 0.0;
  end;

function GsFlowerFruit.harvestItemTemplate: GsHarvestItemTemplate;
  begin
  case stage of
    kStageFlowerBud:
      begin
      if self.gender = kGenderMale then
        result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeMaleFlowerBud]
      else
        result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFemaleFlowerBud];
      end;
    kStageOpenFlower:
      begin
      if self.gender = kGenderMale then
        result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeMaleFlower]
      else
        result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFemaleFlower];
      end;
    kStageUnripeFruit: result := nil;
    kStageRipeFruit: result := plant.pGeneral.harvestItemTemplatesArray[kHarvestPartTypeFruit];
    kStageFallenFruit: result := nil;
    kStageFallenFlower: result := nil;
    else
      raise Exception.create('GsFlowerFruit.harvestItemTemplate: unsupported case');
    end;
  end;

procedure GsFlowerFruit.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsFlowerFruit;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsFlowerFruit.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamSingle(propFullSize);
  filer.streamSmallint(stage);
  filer.streamBoolean(hasBeenDrawn);
  filer.streamLongint(daysAccumulatingFruitBiomass);
  filer.streamBoolean(hasBeenHarvestedWithInflorescence);
  end;

end.

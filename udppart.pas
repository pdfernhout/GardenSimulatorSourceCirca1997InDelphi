unit Udppart;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udppart: Drawing plant part. Subclasses are: internode, leaf (including seedling leaf),
meristem (apical or axillary), inflorescence, and flower/fruit. A phytomer is one
unit of an internode and one or two leaves (whorled leaves are not simulated). The
superclass just has a few drawing and stream methods the other classes have in
common. Possibly some of the demand and growth methods could have been placed here.}

interface

uses WinProcs, WinTypes, Classes, udplant, ufiler, uharvprt, uicon;

const
  kAddingBiomassToPlant = true;
  kRemovingBiomassFromPlant = false;

type
  GsDrawingPlantPart = class(GsStreamableObject)
  public
  plant: GsDrawingPlant;
  position: SinglePoint;
  liveBiomass_kg: single;
  deadBiomass_kg: single;
  biomassDemand_kg: single;
  gender: smallint;
  genderIndex: smallint; { CFK FIX - not used, redundant }
  age: longint;
  isRemoved: boolean;
  isBundled: boolean;
	procedure draw; virtual;
	procedure drawLengthInPiecesWidthAngleColor(length: single; width: single; angle: single; color: TColorRef);
	procedure drawLengthInPiecesWidthAngleZAngleYColor(length: single; width: single; angleZ: single; angleY:
    single; color: TColorRef);
	procedure initialize(thePlant: GsDrawingPlant);
	procedure nextDay; virtual;
  procedure recordPositionFromTurtleIfDesired;
  function partType: integer; virtual;
  procedure traverseActivity(mode: integer; traverser: GsTraverser); virtual;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  function isPhytomer: boolean; virtual;
  function totalBiomass_kg: single;
  function fractionLive: single;
  procedure addOrRemove(addOrRemoveFlag: boolean); virtual;
  function harvestItemTemplate: GsHarvestItemTemplate; virtual;
  function harvestItemIcon: GsIcon;
  function canBeHarvested: boolean; virtual;
  procedure addDependentPartsToList(aList: TList); virtual;
  function liveBiomassForHarvest_kg: single; virtual;
  function deadBiomassForHarvest_kg: single; virtual;
  function totalBiomassForHarvest_kg: single; virtual;
  function harvestItemIncludeInWholePlantHarvestBiomass: boolean; virtual;
  procedure addToStatistics(statistics: GsDrawingPlantStatistics; partType: smallint);
  procedure calculateColors;
  procedure blendColorsStrength(aColor: TColorRef; aStrength: single); virtual;
  procedure setColorsToParameters; virtual;
  end;

implementation

uses SysUtils, udebug, uturt3d, ueutils, ueplant, uclasses;

{ ---------------------------------------------------------------------------------- plant part ancestor }
procedure GsDrawingPlantPart.Initialize(thePlant: GsDrawingPlant);
  begin
  {initialize generic plant part}
  plant := thePlant;
  age := 0;
  gender := kGenderFemale;
  isRemoved := false;
  end;

procedure GsDrawingPlantPart.nextDay;
  begin
  {next day procedure for generic plant part}
  age := age + 1;
  end;

procedure GsDrawingPlantPart.traverseActivity(mode: integer; traverser: GsTraverser);
  var
    biomassToRemove_kg: single;
  begin
  case mode of
    kActivityNone: ;
    kActivityNextDay: ;
    kActivityDemandVegetative_kg:;
    kActivityDemandReproductive_kg:;
    kActivityGrowVegetative: ;
    kActivityGrowReproductive: ;
    kActivityStartReproduction: ;
    kActivityFindPlantPartAtPosition: ;
    kActivityFindAgeOfYoungestPhytomer: ;
    kActivityDraw:  ;
    kActivityReport: ;
    kActivityStream: ;
    kActivityFree: ;
    kActivityVegetativeBiomassThatCanBeRemoved_kg: ;
    kActivityRemoveVegetativeBiomass: ;
    kActivityReproductiveBiomassThatCanBeRemoved_kg: ;
    kActivityRemoveReproductiveBiomass: ;
    kActivityAddMatureHarvestedPartsToList: ;
    kActivityStandingDeadBiomassThatCanBeRemoved_kg:
      traverser.total := traverser.total + self.deadBiomass_kg;
    kActivityRemoveStandingDeadBiomass:
      begin
      biomassToRemove_kg := self.deadBiomass_kg * traverser.fractionOfPotentialBiomass;
      self.deadBiomass_kg := self.deadBiomass_kg - biomassToRemove_kg;
      end;
    else
      raise Exception.create('unhandled mode for GsDrawingPlantPart activity');
    end;
  end;

procedure GsDrawingPlantPart.addToStatistics(statistics: GsDrawingPlantStatistics; partType: smallint);
  begin
  if isRemoved or isBundled then exit;
  with statistics do
    begin
    count[partType] := count[partType] + 1;
    liveBiomass_kg[partType] := liveBiomass_kg[partType] + self.liveBiomass_kg;
    deadBiomass_kg[partType] := deadBiomass_kg[partType] + self.deadBiomass_kg;
    end;
  end;

function GsDrawingPlantPart.liveBiomassForHarvest_kg: single;
  begin
  result := self.liveBiomass_kg;
  end;

function GsDrawingPlantPart.deadBiomassForHarvest_kg: single;
  begin
  result := self.deadBiomass_kg;
  end;

function GsDrawingPlantPart.totalBiomassForHarvest_kg: single;
  begin
  result := self.liveBiomassForHarvest_kg + self.deadBiomassForHarvest_kg;
  end;

function GsDrawingPlantPart.totalBiomass_kg: single;
  begin
  result := liveBiomass_kg + deadBiomass_kg;
  end;

function GsDrawingPlantPart.fractionLive: single;
  begin
  result := 0.0;
  try
  if self.totalBiomass_kg > 0.0 then
    result := safediv(self.liveBiomass_kg, self.totalBiomass_kg)
  else
    result := 0.0;
  except errorMessage('Problem in GsDrawingPlantPart.fractionLive'); end;
  end;

procedure GsDrawingPlantPart.draw;
  begin
  DebugPrint('implementedBySubclass');
  end;

function GsDrawingPlantPart.isPhytomer: boolean;
  begin
  result := false;
  end;

procedure GsDrawingPlantPart.addOrRemove(addOrRemoveFlag: boolean);
  begin
  if addOrRemoveFlag = kAddingBiomassToPlant then
    isRemoved := false
  else
    isRemoved := true;
  GsPlant(plant.model).addOrRemovePlantPartBiomass(self, addOrRemoveFlag);
  end;
                              
procedure GsDrawingPlantPart.addDependentPartsToList(aList: TList);
  begin
  { subclasses can override }
  end;

function GsDrawingPlantPart.canBeHarvested: boolean;
  begin
  result := (self.harvestItemTemplate <> nil) and (not isBundled);
  end;

function GsDrawingPlantPart.harvestItemTemplate: GsHarvestItemTemplate;
  begin
  result := nil;
  end;

function GsDrawingPlantPart.harvestItemIncludeInWholePlantHarvestBiomass: boolean;
  begin
  result := false;
  end;

function GsDrawingPlantPart.harvestItemIcon: GsIcon;
  begin
  if self.harvestItemTemplate <> nil then
    result := self.harvestItemTemplate.icon
  else
    result := nil;
  end;

procedure GsDrawingPlantPart.calculateColors;
  var
    deadFactor, nStressFactor, pStressFactor, relativeAgeFactor: single;
  begin
  { leaf, internode and inflorescence color is based on:
        1. live/dead portion - deadest is most brown (worstDeadColor)
        2. N stress (plant-wide factor) - oldest is most yellow (worstNStressColor)
        3. P stress (plant-wide factor) - youngest is most dark green (worstPStressColor)
    The parameters specify colors for the perfect condition; each factor brings the color
    closer to the worst-case color (for each condition) separately. The results are additive. }
  setColorsToParameters;
  blendColorsStrength(worstDeadColor, 1.0 - self.fractionLive);
  relativeAgeFactor := min(1.0, max(0.0, safedivExcept((self.age - plant.ageOfYoungestPhytomer) * 1.0,
      (GsPlant(plant.model).development.age - plant.ageOfYoungestPhytomer) * 1.0, 1.0)));
  blendColorsStrength(worstNStressColor,
      (1.0 - GsPlant(plant.model).constraints.nStressFactor_frn) * relativeAgeFactor);
  blendColorsStrength(worstPStressColor,
      (1.0  - GsPlant(plant.model).constraints.pStressFactor_frn) * (1.0 - relativeAgeFactor));
  end;

procedure GsDrawingPlantPart.BlendColorsStrength(aColor: TColorRef; aStrength: single);
  begin
  {subclasses can override}
  end;

procedure GsDrawingPlantPart.setColorsToParameters;
  begin
  {subclasses can override}
  end;

procedure GsDrawingPlantPart.DrawLengthInPiecesWidthAngleColor(length: single; width: single; angle: single; color: TColorRef);
  var
    turtle: KfTurtle;
    turnPortion: single;
    drawPortion: single;
    lineDivisions: integer;
    i: integer;
  begin
  {draw length of line for internode or petiole or peduncle in sections to make curve}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  if turtle.drawOptions.straightLinesOnly then
    lineDivisions := 1
  else
    lineDivisions := plant.pGeneral.lineDivisions;
  turtle.setLineWidth(width);
  turtle.setLineColor(color);
  if (lineDivisions > 1) then
    begin
    turnPortion := angle / lineDivisions;
    drawPortion := length / lineDivisions;
    end
  else
    begin
    turnPortion := angle;
    drawPortion := length;
    end;
  if lineDivisions > 1 then
  	for i := 1 to lineDivisions - 1 do
   	 	begin
    	turtle.RotateZ(turnPortion);
    	turtle.DrawInMillimeters(drawPortion);
    	end;
  turtle.RotateZ((angle - (turnPortion * (lineDivisions - 1))));
  turtle.DrawInMillimeters((length - (drawPortion * (lineDivisions - 1))));
  end;

procedure GsDrawingPlantPart.DrawLengthInPiecesWidthAngleZAngleYColor(length: single; width: single; angleZ: single;
	angleY: single; color: TColorRef);
  var
    turtle: KfTurtle;
    turnPortionZ: single;
    turnPortionY: single;
    drawPortion: single;
    lineDivisions: integer;
    i: integer;
  begin
  {draw length of line for internode or petiole or peduncle in sections to make curve}
  turtle := plant.turtle;
  if (turtle = nil) then exit;
  if turtle.drawOptions.straightLinesOnly then
    lineDivisions := 1
  else
    lineDivisions := plant.pGeneral.lineDivisions;
  turtle.setLineWidth(width); 
  turtle.setLineColor(color);
  if (lineDivisions > 1) then
    begin
    turnPortionZ := angleZ / lineDivisions;
    turnPortionY := angleY / lineDivisions;
    drawPortion := length / lineDivisions;
    end
  else
    begin
    turnPortionZ := angleZ;
    turnPortionY := angleY;
    drawPortion := length;
    end;
  if lineDivisions > 1 then
  	for i := 1 to lineDivisions - 1 do
  		begin
			turtle.RotateY(turnPortionY);
    	turtle.RotateZ(turnPortionZ);
    	turtle.DrawInMillimeters(drawPortion);
    	end;
  turtle.RotateY((angleY - (turnPortionY * (lineDivisions - 1))));
  turtle.RotateZ((angleZ - (turnPortionZ * (lineDivisions - 1))));
  turtle.DrawInMillimeters((length - (drawPortion * (lineDivisions - 1))));
  end;

procedure GsDrawingPlantPart.recordPositionFromTurtleIfDesired;
  begin
  if plant.turtle.userWantsToRecordPosition then
    begin
    self.position.x := plant.turtle.position.x;
    self.position.y := plant.turtle.position.y;
    end;
  end;

function GsDrawingPlantPart.partType: integer;
  begin
  DebugPrint('implementedBySubclass');
  result := 0;
  end;

{this will stream entire the entire object -
but the included object references need to be fixed up afterwards
or the objects streamed out separately afterwards - subclasses overrides
need to call inherited to get this behavior}
procedure GsDrawingPlantPart.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsDrawingPlantPart;
  cvir.versionNumber := 1;
  cvir.additionNumber := 0;
  end;

procedure GsDrawingPlantPart.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamBytes(position, sizeof(position));
  filer.streamSingle(liveBiomass_kg);
  filer.streamSingle(deadBiomass_kg);
  filer.streamSingle(biomassDemand_kg);
  filer.streamSmallint(gender);
  filer.streamSmallint(genderIndex);
  filer.streamLongint(age);
  filer.streamBoolean(isRemoved);
  filer.streamBoolean(isBundled);
  end;

end.
 
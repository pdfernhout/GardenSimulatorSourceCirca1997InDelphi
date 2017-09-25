unit ueg;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ueg: Our additions to the EPIC model. The division makes little sense at this point
since we have made major changes all over the model code. These are mainly functions
that we made entirely from scratch. These functions deal mainly with plant biomass
allocation and plant reproduction. }

interface

uses
  uestruct;

type GS = class
  public
  class procedure DailyFlatCropResidueDecay(numLayers: smallint; var layers: LayerStructureArray;
    var movement: MovementStructure);
  class procedure RecalculatePlantBiomassRelationships(var biomass: BiomassStructure);
  class function RootDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAlreadyTaken_tPha: single): single;
  class function ShootDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAlreadyTaken_tPha: single): single;
  class function StorageOrganDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAlreadyTaken_tPha: single): single;
  class function ReproductiveDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAvailable_tPha: single): single;
  class function FloralInductionUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
    meanTempForDay_degC: single; photoInductionParams1: single; photoInductionParams2: single; dayLength_hr: single;
    ignoreTemp: boolean; ignoreDaylength: boolean): single;
  class function FloralInitiationUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
    meanTempForDay_degC: single; ignoreTemp: boolean): single;
  class function VernalizationUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
    meanTempForDay_degC: single): single;
  class function ThermalUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
    meanTempForDay_degC: single): single;
  class procedure AllocateNewBiomassAmongComponents(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; var water: PlantWaterStructure);
  class function RootSystemDiameter_m(rootDepth_m: single; maxRootSystemDiameter_m: single; heatUnitIndex: single): single;
  class function LeafLossIndexAtLeafHarvest(leafLossIndex: single; biomassLost_tPha, totalBiomass_tPha: single): single;
  class procedure ReproductiveBiomassDecay(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure);
  end;

implementation

uses
  ueutils, ueq;

class procedure GS.DailyFlatCropResidueDecay(numLayers: smallint; var layers: LayerStructureArray;
    var movement: MovementStructure);
  var
    layer: smallint;
    flatCropResidueDecayForLayer_tPha: single;
  begin
  movement.flatCropResidueDecomposedToAtmosphere_tPha := 0.0;
  movement.flatCropResidueDecomposedToOM_tPha := 0.0;
  if numLayers > 0 then for layer := 0 to numLayers - 1 do
    begin
    flatCropResidueDecayForLayer_tPha := EQ.FlatCropResidueDecay_tPha(layers[layer].decayRateConst,
        layers[layer].flatCropResidue_tPha);
    { the organic matter is increased by a multiplier based on 20% of the ratio of mineralized N to
      total organic N. i take this to mean that 20% of the flat residue decay is considered to enter
      the organic matter, and 80% is lost to the atmosphere. }
    addQuantity(movement.flatCropResidueDecomposedToAtmosphere_tPha, flatCropResidueDecayForLayer_tPha * 0.8);
    addQuantity(movement.flatCropResidueDecomposedToOM_tPha, flatCropResidueDecayForLayer_tPha * 0.2);
    subtractQuantity(layers[layer].flatCropResidue_tPha, flatCropResidueDecayForLayer_tPha);
    { don't subtract from OM here because it is done elsewhere }
    end;
  end;

class procedure GS.AllocateNewBiomassAmongComponents(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; var water: PlantWaterStructure);
  var
    layer: integer;
    rootDemand_tPha, shootDemand_tPha, storageOrganDemand_tPha, reproductiveDemand_tPha,
      totalAvailable_tPha, availAfterReproDemand_tPha, shootAvailable_tPha,
      totalDemand_tPha, storageOrganAvailable_tPha, rootAvailable_tPha, rootAllocation_tPha: single;
  begin
  try
  if biomass.actualIncrease_tPha <= 0.0 then exit;
  totalAvailable_tPha := biomass.actualIncrease_tPha;
  { if reproductive phase has started, reproductive demand }
  reproductiveDemand_tPha := GS.ReproductiveDemand_tPha(biomass, plantParams, development, totalAvailable_tPha);
  { repro demand has priority over other demands, but they do try to get a fraction based on the whole plant biomass }
  { root, shoot and storage organ make demands on whole increase also }
  rootDemand_tPha := GS.RootDemand_tPha(biomass, plantParams, development, reproductiveDemand_tPha);
  if rootDemand_tPha < 0.0 then
    begin
    rootAvailable_tPha := -rootDemand_tPha;
    biomass.mobilizationOutOfRoots_tPha := rootAvailable_tPha;
    totalAvailable_tPha := totalAvailable_tPha + rootAvailable_tPha;
    if biomass.numLayersWithRootsInThem > 0 then
      for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
        begin
        biomass.rootAllocationByLayer_tPha[layer] := 0.0;
        subtractQuantity(biomass.rootWeightByLayer_tPha[layer],
          rootAvailable_tPha * safediv(biomass.rootWeightByLayer_tPha[layer], biomass.totalRootWeight_tPha));
        end;
    rootDemand_tPha := 0.0;
    rootAllocation_tPha := 0.0;
    end;
  shootDemand_tPha := GS.ShootDemand_tPha(biomass, plantParams, development, reproductiveDemand_tPha);
  if shootDemand_tPha < 0.0 then
    begin
    shootAvailable_tPha := -shootDemand_tPha;
    biomass.mobilizationOutOfShoots_tPha := shootAvailable_tPha;
    totalAvailable_tPha := totalAvailable_tPha + shootAvailable_tPha;
    subtractQuantity(biomass.totalShootWeight_tPha, shootAvailable_tPha);
    shootDemand_tPha := 0.0;
    biomass.shootAllocation_tPha := 0.0;
    end;
  storageOrganDemand_tPha := GS.StorageOrganDemand_tPha(biomass, plantParams, development, reproductiveDemand_tPha);
  if storageOrganDemand_tPha < 0.0 then
    begin
    storageOrganAvailable_tPha := -storageOrganDemand_tPha;
    biomass.mobilizationOutOfStorage_tPha := storageOrganAvailable_tPha;
    totalAvailable_tPha := totalAvailable_tPha + storageOrganAvailable_tPha;
    subtractQuantity(biomass.storageOrgan_tPha, storageOrganAvailable_tPha);
    storageOrganDemand_tPha := 0.0;
    biomass.storageOrganAllocation_tPha := 0.0;
    end;
  { if other plant parts have returned some biomass (redistribution), recalculate repro demand }
  if totalAvailable_tPha > biomass.actualIncrease_tPha then
    reproductiveDemand_tPha := GS.ReproductiveDemand_tPha(biomass, plantParams, development, totalAvailable_tPha);
  { now add up totals and calculate allocations }
  totalDemand_tPha := rootDemand_tPha + storageOrganDemand_tPha + reproductiveDemand_tPha + shootDemand_tPha;
  if totalDemand_tPha <= 0.0 then
    exit;
  if (totalDemand_tPha > totalAvailable_tPha) then
    begin
    if storageOrganDemand_tPha > 0 then
      biomass.storageOrganAllocation_tPha := totalAvailable_tPha * safediv(storageOrganDemand_tPha, totalDemand_tPha);
    rootAllocation_tPha := totalAvailable_tPha * safediv(rootDemand_tPha, totalDemand_tPha);
    biomass.shootAllocation_tPha := totalAvailable_tPha * safediv(shootDemand_tPha, totalDemand_tPha);
    biomass.reproductiveAllocation_tPha := totalAvailable_tPha * safediv(reproductiveDemand_tPha, totalDemand_tPha);
    end
  else
    { if less demand than supply, the excess goes into the storage organ where it can be used later }
    { the storage organ does not have to be a separate entity - it represents storage all over the plant }
    begin
    biomass.storageOrganAllocation_tPha := storageOrganDemand_tPha + (totalAvailable_tPha - totalDemand_tPha);
    rootAllocation_tPha := rootDemand_tPha;
    biomass.shootAllocation_tPha := shootDemand_tPha;
    biomass.reproductiveAllocation_tPha := reproductiveDemand_tPha;
    end;
  { allocate photosynthate to sinks }
  addQuantity(biomass.totalShootWeight_tPha, biomass.shootAllocation_tPha);
  addQuantity(biomass.storageOrgan_tPha, biomass.storageOrganAllocation_tPha);
  addQuantity(biomass.flowersAndFruits_tPha, biomass.reproductiveAllocation_tPha);
  { figure new root weight for each soil layer }
  if biomass.numLayersWithRootsInThem > 0 then
    for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
      begin
      biomass.rootAllocationByLayer_tPha[layer] := EQ.ChangeInRootWeightForLayer_tPha(rootAllocation_tPha,
          water.waterUseByLayer_mm[layer], water.totalWaterUse_mm,
          biomass.rootWeightByLayer_tPha[layer], biomass.totalRootWeight_tPha);
      addQuantity(biomass.rootWeightByLayer_tPha[layer], biomass.rootAllocationByLayer_tPha[layer]);
      end;
  GS.RecalculatePlantBiomassRelationships(biomass);
  except
    errorMessage('Problem in GS.AllocateNewBiomassAmongComponents'); end;
  end;

class procedure GS.RecalculatePlantBiomassRelationships(var biomass: BiomassStructure);
  var layer: smallint;
  begin
  { recalc total root weight }
  biomass.totalRootWeight_tPha := 0.0;
  if biomass.numLayersWithRootsInThem > 0 then
    for layer := 0 to biomass.numLayersWithRootsInThem - 1 do
      biomass.totalRootWeight_tPha := biomass.totalRootWeight_tPha + biomass.rootWeightByLayer_tPha[layer];
  biomass.totalLive_tPha := biomass.totalShootWeight_tPha + biomass.totalRootWeight_tPha
      + biomass.storageOrgan_tPha + biomass.flowersAndFruits_tPha;
  biomass.standingLive_tPha := biomass.totalLive_tPha - biomass.totalRootWeight_tPha;
  end;

class function GS.RootDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAlreadyTaken_tPha: single): single;
  var
    newPlantBiomass_tPha, fractionBasedOnHUI: single;
  begin
  result := 0.0;
  fractionBasedOnHUI := plantParams.fractionRootWtAtEmergence_frn
    - plantParams.fractionRootWtAtMaturity_frn * min(1.0, development.heatUnitIndex);
  newPlantBiomass_tPha := biomass.totalLive_tPha + biomass.actualIncrease_tPha;
  result := newPlantBiomass_tPha * fractionBasedOnHUI - biomass.totalRootWeight_tPha - biomassAlreadyTaken_tPha;
  { if want to suck biomass out of the root to maintain proper ratio, put daily bound of 1% of root mass on it }
  if (result < 0.0) then
    result := max(result, -0.01 * biomass.totalRootWeight_tPha);
  end;

class function GS.ShootDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAlreadyTaken_tPha: single): single;
  var
    newPlantBiomass_tPha, fractionBasedOnHUI: single;
  begin
  result := 0.0;
  { shoot demand is based on 1.0 - root demand  }
  fractionBasedOnHUI := plantParams.fractionRootWtAtEmergence_frn
    - plantParams.fractionRootWtAtMaturity_frn * min(1.0, development.heatUnitIndex);
  newPlantBiomass_tPha := biomass.totalLive_tPha + biomass.actualIncrease_tPha;
  result := newPlantBiomass_tPha * (1.0 - fractionBasedOnHUI) - biomass.totalShootWeight_tPha - biomassAlreadyTaken_tPha;
  { if want to suck biomass out of the shoot to maintain proper ratio, put daily bound of 1% of shoot mass on it }
  if (result < 0.0) then
    result := max(result, -0.01 * biomass.totalShootWeight_tPha);
  end;

class function GS.StorageOrganDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAlreadyTaken_tPha: single): single;
  var
    newPlantBiomass_tPha, fractionBasedOnHUI: single;
  begin
  result := 0.0;
  if (development.heatUnitIndex < plantParams.heatUnitIndexAtStartOfStorageOrganAllocation)
      or (plantParams.heatUnitIndexAtStartOfStorageOrganAllocation >= 1.0)
      or (plantParams.fractionStorageOrganAllocationAtMaturity_frn <= 0.0) then
    { if not collecting and have some, means it was put there because it was extra - try to take back out }
    result := -biomass.storageOrgan_tPha
  else
    begin
    fractionBasedOnHUI := min(1.0, development.heatUnitIndex)
        * safediv(plantParams.fractionStorageOrganAllocationAtMaturity_frn,
        1.0 - plantParams.heatUnitIndexAtStartOfStorageOrganAllocation);
    newPlantBiomass_tPha := biomass.totalLive_tPha + biomass.actualIncrease_tPha;
    result := newPlantBiomass_tPha * (1.0 - fractionBasedOnHUI) - biomass.storageOrgan_tPha - biomassAlreadyTaken_tPha;
     { if need to suck biomass out of storage to maintain proper ratio, put daily bound of 20% of storage mass on it }
    if (result < 0.0) then
      result := -0.2 * biomass.storageOrgan_tPha;
    end;
  end;

class function GS.ReproductiveDemand_tPha(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure; biomassAvailable_tPha: single): single;
  var
    newPlantBiomass_tPha, fractionBasedOnHUI: single;
  begin
  result := 0.0;
  if (development.heatUnitIndexWhenReproductiveAllocationStarted >= 1.0)
    or (development.lifeHistoryStage <> kReproductiveAllocationPeriod) then
    exit;
  fractionBasedOnHUI := min(1.0, development.heatUnitIndex)
      * safediv(plantParams.fractionReproductiveAllocationAtMaturity_frn,
      1.0 - development.heatUnitIndexWhenReproductiveAllocationStarted);
  newPlantBiomass_tPha := biomass.totalLive_tPha + biomassAvailable_tPha;
  result := max(0.0, newPlantBiomass_tPha * fractionBasedOnHUI - biomass.flowersAndFruits_tPha);
  end;

class function GS.FloralInductionUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
  meanTempForDay_degC: single; photoInductionParams1: single; photoInductionParams2: single; dayLength_hr: single;
  ignoreTemp: boolean; ignoreDaylength: boolean): single;
  var thermalUnits, photoUnits: single;
  begin
  try
  if ignoreTemp then
    thermalUnits := 1.0
  else
    thermalUnits := ThermalUnitsForDay(minTemp_degC, optTemp_degC, maxTemp_degC, meanTempForDay_degC);
  if ignoreDaylength then
    photoUnits := 1.0
  else
    photoUnits := scurve(24.0 - dayLength_hr, photoInductionParams1, photoInductionParams2);
  result := thermalUnits * photoUnits;
  except result := errorMessage('Problem in GS.FloralInductionUnitsForDay'); end;
  end;

class function GS.FloralInitiationUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
  meanTempForDay_degC: single; ignoreTemp: boolean): single;
  begin
  if ignoreTemp then
    result := 1.0
  else
    result := ThermalUnitsForDay(minTemp_degC, optTemp_degC, maxTemp_degC, meanTempForDay_degC);
  end;

class function GS.VernalizationUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single;
  meanTempForDay_degC: single): single;
  begin
  
    begin
    result := ThermalUnitsForDay(minTemp_degC, optTemp_degC, maxTemp_degC, meanTempForDay_degC);
    exit;
    end;
  end;

class function GS.ThermalUnitsForDay(minTemp_degC: single; optTemp_degC: single; maxTemp_degC: single; meanTempForDay_degC:
  single): single;
  var
    proportionOfRange: single;
  begin
  try
  if (meanTempForDay_degC <= minTemp_degC) or (meanTempForDay_degC > maxTemp_degC) then
    result := 0.0
  else if meanTempForDay_degC = optTemp_degC then
    result := 1.0
  else
    begin
    { lower half of sine curve (0 to pi/2) }
    if (meanTempForDay_degC < optTemp_degC) then
      begin
      proportionOfRange := safediv(meanTempForDay_degC - minTemp_degC, optTemp_degC - minTemp_degC);
      result := sin(kPi / 2.0 * proportionOfRange);
      end
    else
      begin
      { upper half of sine curve (pi/2 to pi) }
      proportionOfRange := safediv(maxTemp_degC - meanTempForDay_degC, maxTemp_degC - optTemp_degC);
      result := sin(kPi / 2.0 + kPi / 2.0 * proportionOfRange);
      end;
    end;
  except result := errorMessage('Problem in GS.ThermalUnitsForDay'); end;
  end;

class function GS.RootSystemDiameter_m(rootDepth_m: single; maxRootSystemDiameter_m: single; heatUnitIndex: single): single;
  begin
  { this function is identical to the one for root depth }
  result := min(maxRootSystemDiameter_m, 2.5 * maxRootSystemDiameter_m * min(1.0, heatUnitIndex));
  end;

class function GS.LeafLossIndexAtLeafHarvest(leafLossIndex: single; biomassLost_tPha, totalBiomass_tPha: single): single;
  begin
  try
  if biomassLost_tPha > 0 then
    result := leafLossIndex * (1.0 - safediv(biomassLost_tPha, totalBiomass_tPha))
  else if biomassLost_tPha < 0 then
    result := safediv(leafLossIndex,
      (1.0 - safediv(-biomassLost_tPha, totalBiomass_tPha - biomassLost_tPha)))
  else
    result := leafLossIndex;
  except result := errorMessage('Problem in GS.LeafLossIndexAtLeafHarvest'); end;
  end;

class procedure GS.ReproductiveBiomassDecay(var biomass: BiomassStructure; var plantParams: PlantParamsStructure;
    var development: DevelopmentStructure);
  var
    decayMultiplier, startHeatUnitIndex: single;
  begin
  try
  if development.heatUnitIndexWhenReproductiveAllocationStarted <= 0.0 then exit;
  startHeatUnitIndex := min(1.0, max(0.0, (1.0 + development.heatUnitIndexWhenReproductiveAllocationStarted) / 2.0));
  if development.heatUnitIndex < startHeatUnitIndex then exit;
  decayMultiplier := min(1.0, development.heatUnitIndex)
      * safedivExcept(plantParams.reproductiveBiomassDecayAtPlantMaturity - 0.0,
      1.0 - startHeatUnitIndex, 0.0);
  biomass.reductionFromReproductiveDecay_tPha := biomass.flowersAndFruits_tPha * decayMultiplier;
  if (biomass.reductionFromReproductiveDecay_tPha > 0.0) then
    begin
    if (biomass.reductionFromReproductiveDecay_tPha > biomass.flowersAndFruits_tPha) then
      biomass.reductionFromReproductiveDecay_tPha := biomass.flowersAndFruits_tPha;
    subtractQuantity(biomass.flowersAndFruits_tPha, biomass.reductionFromReproductiveDecay_tPha);
    GS.RecalculatePlantBiomassRelationships(biomass);
    addQuantity(biomass.standingDead_tPha, biomass.reductionFromReproductiveDecay_tPha);
    end;
  except errorMessage('Problem in GS.ReproductiveBiomassDecay'); end;
  end;


end.
{
	Copyright (c) 1996 Paul D. Fernhout and Cynthia F. Kurtz
  								 All rights reserved

  Notice: This program contains trade secrets that are the property of
  Paul D. Fernhout and Cynthia F. Kurtz. The contents may not be used or disclosed in part or
  in whole without express written permission of the owners.
  Internet: pdfernhout @ bix.com, cfkurtz @ igc.apc.org, kfsoft @ netins.net
}

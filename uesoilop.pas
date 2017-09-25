unit uesoilop;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uesoilop: Tool actions on the soil patch object. These were moved out of uesoil
(because the file was too big for Delphi 1.0), and the soil patch is now passed in.
'mixSoil' is the only function here that is largely unchanged from the EPIC. The
liming function is pretty shaky because it was derived from autoliming. (But then
the entire pH model is extremely simple and should not be relied upon.) The sulfur
application function is ridiculously shaky. The bag function is theoretically okay
but could use more work. The simpler functions (add/remove soil, add/remove mulch,
aerate) are probably fine since they don't do that much. Read uestruct to understand
the structures used here before reading this code.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses uesoil, uestruct, uebag;

type SoilOp = class
  public
  class procedure mixSoil(soil: GsSoilPatch;
    area_ha, depth_m, maximumEfficiency, randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m: single;
  	liming: boolean);
  class procedure addSoil(soil: GsSoilPatch; amount_tPha: single; var carriedSoil: LayerStructure);
  class procedure removeSoil(soil: GsSoilPatch; amount_tPha: single; var carriedSoil: LayerStructure);
  class procedure irrigate(soil: GsSoilPatch; amount_mm: single; isAutoIrrigation: boolean);
  class procedure aerateSoil(soil: GsSoilPatch; depth_m, area_ha: single);
  class procedure addMulch(soil: GsSoilPatch; var carriedMulch: MulchStructure);
  class procedure removeMulch(soil: GsSoilPatch; amount_tPha: single; var carriedMulch: MulchStructure);
  class procedure applyBag(soil: GsSoilPatch; amount_tPha: single; var bag: BagStructure);
  class procedure applyLime(soil: GsSoilPatch; amount_tPha: single);
  class procedure applyLimeToFirstLayerOnly(soil: GsSoilPatch; amount_tPha: single);
  class procedure applySulfurToFirstLayerOnly(soil: GsSoilPatch; amount_tPha: single);
  end;

implementation

uses ueutils, uunits, ueq;

class procedure SoilOp.mixSoil(soil: GsSoilPatch;
    area_ha, depth_m, maximumEfficiency, randomRoughness_mm, ridgeHeight_mm, ridgeInterval_m: single;
  	liming: boolean);
  var
    quitAfterThisLayer: boolean;
    layer, i, lastLayer, lastLayerInTillageDepth: integer;
    formerRockContent_pct, pspTSolubleP_kgPha, solublePMpspTSolubleP_kgPha: arraySoilLayers;
    layerThickness_m, solubleP_kgPha, depthLastLayer_m, holdPspTSolubleP_kgPha, holdSolublePMpspTSolubleP_kgPha,
      depthBeingAdded_m, propNewToOldNonRockContent, proportion, propTMixingEffic,
      boundedMixingEfficiency_frn, boundedTillageDepth_m, boundedRandomRoughness_mm: single;
    holdLayer: LayerStructure;
  begin
  if soil = nil then exit;
  with soil do
  try
  solubleP_kgPha := 0.0;
  holdPspTSolubleP_kgPha := 0.0;
  holdSolublePMpspTSolubleP_kgPha := 0.0;
  depthBeingAdded_m := 0.0;
  propNewToOldNonRockContent := 0.0;
  proportion := 0.0;
  propTMixingEffic := 0.0;
  boundedMixingEfficiency_frn := safediv(area_ha, params.area_ha) * maximumEfficiency;
  boundedTillageDepth_m := depth_m;
  boundedRandomRoughness_mm := randomRoughness_mm; 
  { zero out soil holding layer }
  Utils_ZeroAllLayerFields(holdLayer);
  if (boundedTillageDepth_m < 0.0) then exit;
  Utils_EnforceFloatRange(boundedMixingEfficiency_frn, 0.0, 1.0);
  Utils_EnforceFloatRange(boundedTillageDepth_m,  - 300.0 * cm_to_m, 750.0 * cm_to_m);
  Utils_EnforceFloatRange(boundedRandomRoughness_mm, 0.0, 75.0); {from EPIC data file; range is from 0 to 75}
  { if tillage depth not deeper than first layer depth, no mixing occurs }
  if (layers[0].depth_m >= boundedTillageDepth_m) then exit;
  if (not liming) then
    begin
    { calculate new ridge height and interval }
    surface.ridgeHeight_mm := EQ.RidgeHeightAfterTillage_m(ridgeHeight_mm, surface.ridgeHeightLastOperation_mm,
      boundedTillageDepth_m, surface.tillageDepthLastOperation_m);
    surface.ridgeInterval_m := EQ.RidgeIntervalAfterTillage_m(ridgeInterval_m,
      surface.ridgeIntervalLastOperation_m, ridgeHeight_mm, surface.ridgeHeightLastOperation_mm);
    { transfer standing dead to flat crop residue }
    TransferStandingDeadToFlatResidueWhenTilling(boundedTillageDepth_m, boundedMixingEfficiency_frn);
    { reset random roughness and wind erosion accumulated factor }
    surface.randomRoughness_mm := boundedRandomRoughness_mm;
    erosion.windErosionAccumulatedFactor := 0.5;
    end; 
  depthLastLayer_m := 0.0;
  quitAfterThisLayer := false;
  lastLayerInTillageDepth := 0;
	if state.numLayers > 0 then
  	for layer := 0 to state.numLayers - 1 do
    begin
    if (quitAfterThisLayer) then break;
    lastLayerInTillageDepth := layer;
    layerThickness_m := layers[layer].depth_m - depthLastLayer_m;
    formerRockContent_pct[layer] := layers[layer].rockContent_pct;
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, boundedTillageDepth_m,
      quitAfterThisLayer);
    propTMixingEffic := proportion * boundedMixingEfficiency_frn;
    { change bulk density due to tillage }
    layers[layer].bulkDensity_tPm3 := EQ.BulkDensityAfterTillageForLayer_tPm3(layers[layer].bulkDensity_tPm3,
      layers[layer].settledBulkDensity_tPm3, propTMixingEffic);
    { now move proportion of materials from layer to holding layer }
    Utils_TransferPropOfNutrients(layers[layer], holdLayer, propTMixingEffic);
    { temporarily bring percentages to absolute amounts so they can be shuffled as amounts }
    layers[layer].clayContent_pct := layers[layer].clayContent_pct * layerThickness_m;
    layers[layer].siltContent_pct := layers[layer].siltContent_pct * layerThickness_m;
    layers[layer].rockContent_pct := layers[layer].rockContent_pct * layerThickness_m;
    { transfer amounts }
    Utils_TransferPropOfMaterial(layers[layer].clayContent_pct, holdLayer.clayContent_pct, propTMixingEffic);
    Utils_TransferPropOfMaterial(layers[layer].siltContent_pct, holdLayer.siltContent_pct, propTMixingEffic);
    Utils_TransferPropOfMaterial(layers[layer].rockContent_pct, holdLayer.rockContent_pct, propTMixingEffic);
    { calculate two vars having to do with labile/mineral P, which will be used to recalculate
      the PSP after mixing, then transfer them }
    solubleP_kgPha := layers[layer].mineralPActive_kgPha + layers[layer].labileP_kgPha;
    pspTSolubleP_kgPha[layer] := layers[layer].pSorptionCoeff_frn * solubleP_kgPha;
    solublePMpspTSolubleP_kgPha[layer] := solubleP_kgPha - pspTSolubleP_kgPha[layer];
    Utils_TransferPropOfMaterial(pspTSolubleP_kgPha[layer], holdPspTSolubleP_kgPha, propTMixingEffic);
    Utils_TransferPropOfMaterial(solublePMpspTSolubleP_kgPha[layer], holdSolublePMpspTSolubleP_kgPha, propTMixingEffic);
    depthLastLayer_m := layers[layer].depth_m;
    end;
  if (not quitAfterThisLayer) then 
    begin
    lastLayerInTillageDepth := state.numLayers - 1;
    boundedTillageDepth_m := layers[state.numLayers - 1].depth_m;
    end;
  { divide each var in the holding layer by the new tillage depth to get
   an amount of material _per m_ which was removed from the layers (temporarily)
   and can now be re-distributed among the layers }
  { things moved in TransferPropOfNutrients }
  holdLayer.nitrate_kgPha := safediv(holdLayer.nitrate_kgPha, boundedTillageDepth_m);
  holdLayer.ammonia_kgPha := safediv(holdLayer.ammonia_kgPha, boundedTillageDepth_m);
  holdLayer.organicNFresh_kgPha := safediv(holdLayer.organicNFresh_kgPha, boundedTillageDepth_m);
  holdLayer.organicNActiveHumus_kgPha := safediv(holdLayer.organicNActiveHumus_kgPha, boundedTillageDepth_m);
  holdLayer.organicNStableHumus_kgPha := safediv(holdLayer.organicNStableHumus_kgPha, boundedTillageDepth_m);
  holdLayer.labileP_kgPha := safediv(holdLayer.labileP_kgPha, boundedTillageDepth_m);
  holdLayer.organicPHumus_kgPha := safediv(holdLayer.organicPHumus_kgPha, boundedTillageDepth_m);
  holdLayer.organicPFresh_kgPha := safediv(holdLayer.organicPFresh_kgPha, boundedTillageDepth_m);
  holdLayer.mineralPActive_kgPha := safediv(holdLayer.mineralPActive_kgPha, boundedTillageDepth_m);
  holdLayer.mineralPStable_kgPha := safediv(holdLayer.mineralPStable_kgPha, boundedTillageDepth_m);
  holdLayer.flatCropResidue_tPha := safediv(holdLayer.flatCropResidue_tPha, boundedTillageDepth_m);
  holdLayer.organicMatter_tPha := safediv(holdLayer.organicMatter_tPha, boundedTillageDepth_m);
  { clay, silt, rock }
  holdLayer.clayContent_pct := safediv(holdLayer.clayContent_pct, boundedTillageDepth_m);
  holdLayer.siltContent_pct := safediv(holdLayer.siltContent_pct, boundedTillageDepth_m);
  holdLayer.rockContent_pct := safediv(holdLayer.rockContent_pct, boundedTillageDepth_m);
  { PSP intermediate vars }
  holdPspTSolubleP_kgPha := safediv(holdPspTSolubleP_kgPha, boundedTillageDepth_m);
  holdSolublePMpspTSolubleP_kgPha := safediv(holdSolublePMpspTSolubleP_kgPha, boundedTillageDepth_m);
  { redistribute holding layer material into all layers wholly or partly in the tillage depth }
  depthLastLayer_m := 0.0;
  if lastLayerInTillageDepth > 0 then
  	for layer := 0 to lastLayerInTillageDepth do
    begin
    layerThickness_m := layers[layer].depth_m - depthLastLayer_m;
    if (layer < lastLayerInTillageDepth) then
      depthBeingAdded_m := layerThickness_m
    else
      depthBeingAdded_m := boundedTillageDepth_m - depthLastLayer_m;
    { return materials to layers based on depth of layer }
    { note the use of Add, not Transfer, functions here }
    { because the holdLayer amount is an amount per m and we don't want to deplete it. }
    Utils_AddPropOfNutrients(holdLayer, layers[layer], depthBeingAdded_m);
    { add clay silt rocks to layer, still in non-percent form }
    Utils_AddPropOfMaterial(holdLayer.clayContent_pct, layers[layer].clayContent_pct, depthBeingAdded_m);
    Utils_AddPropOfMaterial(holdLayer.siltContent_pct, layers[layer].siltContent_pct, depthBeingAdded_m);
    Utils_AddPropOfMaterial(holdLayer.rockContent_pct, layers[layer].rockContent_pct, depthBeingAdded_m);
    { return clay, silt and rocks to percentages  }
    layers[layer].clayContent_pct := layers[layer].clayContent_pct / layerThickness_m;
    layers[layer].siltContent_pct := layers[layer].siltContent_pct / layerThickness_m;
    layers[layer].rockContent_pct := layers[layer].rockContent_pct / layerThickness_m;
    { recalculate sand to keep adding up to 100 }
    layers[layer].sandContent_pct := 100.0 - layers[layer].clayContent_pct - layers[layer].siltContent_pct;
    { transfer intermediate PSP vars and recalculate PSP from saved values }
    Utils_AddPropOfMaterial(holdPspTSolubleP_kgPha, pspTSolubleP_kgPha[layer], depthBeingAdded_m);
    Utils_AddPropOfMaterial(holdSolublePMpspTSolubleP_kgPha, solublePMpspTSolubleP_kgPha[layer], depthBeingAdded_m);
    layers[layer].pSorptionCoeff_frn := safediv(pspTSolubleP_kgPha[layer],
        solublePMpspTSolubleP_kgPha[layer] + pspTSolubleP_kgPha[layer]);
    { to calculate the change in fc and wp, you use the change in non-rock content }
    propNewToOldNonRockContent := safediv(100.0 - layers[layer].rockContent_pct, 100.0 - formerRockContent_pct[layer]);
    layers[layer].fieldCapacity_mm := layers[layer].fieldCapacity_mm * (propNewToOldNonRockContent);
    layers[layer].wiltingPoint_mm := layers[layer].wiltingPoint_mm * (propNewToOldNonRockContent);
    { recalculate porosity from bulk density and thickness }
    layers[layer].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
        layers[layer].bulkDensity_tPm3, layers[layer].rockContent_pct, layerThickness_m);
    Utils_CheckRelationOfFieldCapacityToPorosity(layers[layer]);
    { recalculate soil weight }
    layers[layer].weight_tPha := Utils_SoilWeightFromBulkDensityAndThickness_tPha(layers[layer].bulkDensity_tPm3,
      layerThickness_m);
    depthLastLayer_m := layers[layer].depth_m;
    end;
  surface.tillageDepthLastOperation_m := boundedTillageDepth_m;
  if (boundedTillageDepth_m > state.maxTillageDepth_m) then state.maxTillageDepth_m := boundedTillageDepth_m;
  SoilPatchTotals;
  except
    errorMessage('SoilOp.MixSoil: problem');
  end;
  end;

class procedure SoilOp.addSoil(soil: GsSoilPatch; amount_tPha: single; var carriedSoil: LayerStructure);
  var
    layer: integer;
    propOfNewLayer1AlreadyThere_frn, propLayer1Moving_frn, propOfNewLayer2AlreadyThere_frn,
      layer2Thickness_m, thicknessToMove_m: single;
    oldDepth_m: arraySoilLayers;
  begin
  if soil = nil then exit;
  with soil do
  try
  layer2Thickness_m := layers[1].depth_m - layers[0].depth_m;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    oldDepth_m[layer] := layers[layer].depth_m;
  { put all of tool layer depth into layer 0 }
  propOfNewLayer1AlreadyThere_frn := safediv(kFirstLayerDepth_m, kFirstLayerDepth_m + carriedSoil.depth_m);
  Utils_MovePortionOfLayerToAnotherLayer(carriedSoil, layers[0], carriedSoil.depth_m, kFirstLayerDepth_m,
      1.0, propOfNewLayer1AlreadyThere_frn, params.watershedSlopeSteepness_mPm, true);
  setLayerDepth(0, layers[0].depth_m + carriedSoil.depth_m);
  { put layer 0 new depth - kFirstLayerDepth_m into layer 1 }
  thicknessToMove_m := layers[0].depth_m - kFirstLayerDepth_m;
  propLayer1Moving_frn := safediv(thicknessToMove_m, layers[0].depth_m);
  propOfNewLayer2AlreadyThere_frn := safediv(layer2Thickness_m, layer2Thickness_m + thicknessToMove_m);
  Utils_MovePortionOfLayerToAnotherLayer(layers[0], layers[1], layers[0].depth_m, layer2Thickness_m,
      propLayer1Moving_frn, propOfNewLayer2AlreadyThere_frn, params.watershedSlopeSteepness_mPm, false);
  setLayerDepth(1, layers[1].depth_m + thicknessToMove_m);
  adjustLowerLayerDepthsForChangeToOneLayer(1, thicknessToMove_m);
  setLayerDepth(0, kFirstLayerDepth_m);
  { clean up }
  updateSoilProfileDepthAndOtherDepths;
  adjustPlantRootWeightsInLayersAfterMovingSoil(oldDepth_m);
  SoilPatchTotals;
  except
    errorMessage('GsSoilPatch.AddSoil: problem');
  end;
  end;

class procedure SoilOp.removeSoil(soil: GsSoilPatch; amount_tPha: single; var carriedSoil: LayerStructure);
  var
    firstLayerBelowToolDepthHasBeenConsidered: boolean;
    layer, layerDown: integer;
    toolAmount_tPha, toolDepth_m, soilWeightInToolDepth_tPha, depthLastLayer_m, layerThickness_m,
      thicknessRemovedFromThisLayer_m, propInToolDepth_frn, propOfLayerRemoved_frn,
      propOfCarriedSoilDepthAlreadyThere_frn: single;
    oldDepth_m: arraySoilLayers;
  begin
  if soil = nil then exit;
  with soil do
  try
  if state.numLayers <= 0 then exit;
  toolAmount_tPha := amount_tPha;
  toolDepth_m := carriedSoil.depth_m;
  { assume carriedSoil has no contents to start with }
  Utils_ZeroAllLayerFields(carriedSoil);
  { save starting layer depths to recalculate plant root weights at end }
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    oldDepth_m[layer] := layers[layer].depth_m;
  { bound tool amount at total soil weight in tool depth }
  soilWeightInToolDepth_tPha := totalSoilWeightDownToCriterionDepth(toolDepth_m);
  toolAmount_tPha := min(toolAmount_tPha, soilWeightInToolDepth_tPha);
  { pull materials out of each layer and put them into tool layer }
  depthLastLayer_m := 0.0;
  propInToolDepth_frn := 0.0;
  propOfLayerRemoved_frn := 0.0;
  firstLayerBelowToolDepthHasBeenConsidered := false;
  for layer := 0 to state.numLayers - 1 do 
    begin
    layerThickness_m := layers[layer].depth_m - depthLastLayer_m;
    propInToolDepth_frn := 1.0;
    if (layers[layer].depth_m > toolDepth_m) then
      if firstLayerBelowToolDepthHasBeenConsidered then
        break
      else
        begin
        propInToolDepth_frn := safediv(layers[layer].depth_m - toolDepth_m, layerThickness_m);
        firstLayerBelowToolDepthHasBeenConsidered := true;
        end;
    propOfLayerRemoved_frn := propInToolDepth_frn * safediv(toolAmount_tPha, soilWeightInToolDepth_tPha);
    { assuming a linear relation between weight and thickness within a layer }
    thicknessRemovedFromThisLayer_m := layerThickness_m * propOfLayerRemoved_frn;
    if carriedSoil.depth_m = 0.0 then
      begin
      Utils_MovePortionOfLayerToEmptyLayer(layers[layer], carriedSoil,
          propOfLayerRemoved_frn, thicknessRemovedFromThisLayer_m, 0.0 {no layer above})
      end
    else
      begin
      propOfCarriedSoilDepthAlreadyThere_frn := safediv(carriedSoil.depth_m,
          carriedSoil.depth_m + thicknessRemovedFromThisLayer_m);
      Utils_MovePortionOfLayerToAnotherLayer(layers[layer], carriedSoil, layerThickness_m, carriedSoil.depth_m,
          propOfLayerRemoved_frn, propOfCarriedSoilDepthAlreadyThere_frn, params.watershedSlopeSteepness_mPm, false);
      end;
    carriedSoil.depth_m := carriedSoil.depth_m + thicknessRemovedFromThisLayer_m;
    setLayerDepth(layer, layers[layer].depth_m - thicknessRemovedFromThisLayer_m);
    adjustLowerLayerDepthsForChangeToOneLayer(layer, -thicknessRemovedFromThisLayer_m);
    { recalculate porosity for this layer if depth changed }
    if thicknessRemovedFromThisLayer_m > 0.0 then
      begin
      layers[layer].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
          layers[layer].bulkDensity_tPm3, layers[layer].rockContent_pct,
          layerThickness_m - thicknessRemovedFromThisLayer_m);
      Utils_CheckRelationOfFieldCapacityToPorosity(layers[layer]);
      end;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  { should split layers to replace any layers with no weight, but not doing that }
  { it is a bit complicated and should be much like the erosion function }
  updateSoilProfileDepthAndOtherDepths;
  adjustPlantRootWeightsInLayersAfterMovingSoil(oldDepth_m);
  SoilPatchTotals;
  except
    errorMessage('GsSoilPatch.RemoveSoil: problem');
  end;
  end;

class procedure SoilOp.irrigate(soil: GsSoilPatch; amount_mm: single; isAutoIrrigation: boolean);
  var
    layer: integer;
    maxIrrigationAllowed_mm, waterSoilCouldTake_mm, finalIrrigationVolume_mm, boundedIrrigationVolume_mm,
      irrLeftInYear_mm: single;
  begin
  if soil = nil then exit;
  with soil do
  try
  boundedIrrigationVolume_mm := amount_mm;
  irrLeftInYear_mm := params.maxAnnualIrrVolumeAllowed_mm - yearTotals.cumIrrigationApplied_mm;
  Utils_EnforceFloatRange(boundedIrrigationVolume_mm, 0.0, 3000.0);
  { if paying attention to soil water content, calc amount soil can take (fc - sw) }
  { if amount_mm = 0 assume auto irrigation }
  if ((amount_mm <> 0) and (params.ignoreSoilWaterContentWhenAutoIrrigating)) then
    waterSoilCouldTake_mm := 10000.0
  else
    begin
    waterSoilCouldTake_mm := 0.0;
    if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do 
      waterSoilCouldTake_mm := waterSoilCouldTake_mm + (layers[layer].fieldCapacity_mm - layers[layer].waterContent_mm);
    waterSoilCouldTake_mm := waterSoilCouldTake_mm / ((1.0 - params.fractionOfIrrigationWaterLostToRunoff));
    end;
  { set max allowed (boundedIrrigationVolume_mm should only be zero if auto irrigating) }
  maxIrrigationAllowed_mm := boundedIrrigationVolume_mm;
  Utils_DefaultFloatIfZero(maxIrrigationAllowed_mm, 10000.0);
  { figure irrigation amount }
  if isAutoIrrigation then
    begin
    finalIrrigationVolume_mm := min(maxIrrigationAllowed_mm, min(irrLeftInYear_mm, min(waterSoilCouldTake_mm,
      params.maxApplicVolumeAutoIrr_mm)));
    if (finalIrrigationVolume_mm < params.minApplicVolumeAutoIrr_mm) then
      finalIrrigationVolume_mm := 0.0;
    end
  else
    finalIrrigationVolume_mm := boundedIrrigationVolume_mm;
  if isAutoIrrigation then
    { can only auto irrigate once in a day }
    water.autoIrrigationForDay_mm := finalIrrigationVolume_mm
  else
    { don't overwrite - user could irrigate more than once in a day }
    addQuantity(water.userIrrigationForDay_mm, finalIrrigationVolume_mm);
  if isAutoIrrigation then state.daysSinceAutoIrrApplied := 0;
  SoilPatchTotals;
  except errorMessage('SoilOp.irrigate: problem'); end;
  end;

class procedure SoilOp.aerateSoil(soil: GsSoilPatch; depth_m, area_ha: single);
  var
    { from looking at books, the most reasonable thing to do to simulate aeration }
    { is to decrease bulk density (which increases porosity) in each soil layer aerated. }
    layer: integer;
    proportion, depthLastLayer_m, aerationProportion: single;
    depthBounded_m: single;
    layerBelowCriterionDepthHasBeenConsidered: boolean;
  begin
  if soil = nil then exit;
  with soil do
  try
  aerationProportion := safediv(area_ha, params.area_ha);
  Utils_EnforceFloatRange(aerationProportion, 0.0, 1.0);
  depthBounded_m := depth_m;
  Utils_EnforceFloatRange(depthBounded_m, 0.0, layers[state.numLayers - 1].depth_m);
  proportion := 0.0;
  depthLastLayer_m := 0.0;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    begin
    layerBelowCriterionDepthHasBeenConsidered := false;
    proportion := Utils_LayerPropAboveCriterionDepth_frn(layers[layer].depth_m, depthLastLayer_m, depthBounded_m,
      layerBelowCriterionDepthHasBeenConsidered);
    { bound bulk density at 1.0 which is a reasonable assumption for 'loose soil' (Troeh & Thompson 1993) }
    layers[layer].bulkDensity_tPm3 := max(1.0,
        layers[layer].bulkDensity_tPm3 * ((1.0 - aerationProportion) * proportion));
    if (layers[layer].depth_m > depthBounded_m) then break;
    depthLastLayer_m := layers[layer].depth_m;
    end;
  soil.RecalculatePorosityFromBulkDensity;
  if state.numLayers > 0 then for layer := 0 to state.numLayers - 1 do
    Utils_CheckRelationOfFieldCapacityToPorosity(layers[layer]);
  SoilPatchTotals;
  except
    errorMessage('SoilOp.aerateSoil: problem');
  end;
  end;

class procedure SoilOp.addMulch(soil: GsSoilPatch; var carriedMulch: MulchStructure);
  begin
  if soil = nil then exit;
  with soil do
  try
  addQuantity(mulch.flatCropResidue_tPha, carriedMulch.flatCropResidue_tPha);
  addQuantity(mulch.organicNFresh_kgPha, carriedMulch.organicNFresh_kgPha);
  addQuantity(mulch.organicPFresh_kgPha, carriedMulch.organicPFresh_kgPha);
  except
    errorMessage('SoilOp.addMulch: problem');
  end;
  end;

class procedure SoilOp.removeMulch(soil: GsSoilPatch; amount_tPha: single; var carriedMulch: MulchStructure);
  var
    residueToTake_tPha: single;
    nConc_kgPt, pConc_kgPt, organicNFreshToTake_kgPha, organicPFreshToTake_kgPha: single;
  begin
  if amount_tPha <= 0 then exit;
  if soil = nil then exit;
  with soil do
  try
  { figure amounts to take }
  if mulch.flatCropResidue_tPha <= 0 then exit;
  residueToTake_tPha := min(amount_tPha, mulch.flatCropResidue_tPha);
  nConc_kgPt := safediv(mulch.organicNFresh_kgPha, mulch.flatCropResidue_tPha);
  organicNFreshToTake_kgPha := min(mulch.organicNFresh_kgPha, residueToTake_tPha * nConc_kgPt);
  pConc_kgPt := safediv(mulch.organicPFresh_kgPha, mulch.flatCropResidue_tPha);
  organicPFreshToTake_kgPha := min(mulch.organicPFresh_kgPha, residueToTake_tPha * pConc_kgPt);
  { transfer amounts }
  addQuantity(carriedMulch.flatCropResidue_tPha, residueToTake_tPha);
  subtractQuantity(mulch.flatCropResidue_tPha, residueToTake_tPha);
  addQuantity(carriedMulch.organicNFresh_kgPha, organicNFreshToTake_kgPha);
  subtractQuantity(mulch.organicNFresh_kgPha, organicNFreshToTake_kgPha);
  addQuantity(carriedMulch.organicPFresh_kgPha, organicPFreshToTake_kgPha);
  subtractQuantity(mulch.organicPFresh_kgPha, organicPFreshToTake_kgPha);
  except
    errorMessage('SoilOp.removeMulch: problem');
  end;
  end;

class procedure SoilOp.applyBag(soil: GsSoilPatch; amount_tPha: single; var bag: BagStructure);
  var
    organicC_pct, organicMatterFraction_frn, bulkDensityInOM_tPm3,
      oldBulkDensity_tPm3, fractionInLayer_frn, waterAdded_mm, bagBulkDensity_tPm3,
      bagThickness_m, bagSand_tPha, bagSilt_tPha, bagClay_tPha, bagRocks_tPha, soilSand_tPha, soilSilt_tPha,
      soilClay_tPha, soilRocks_tPha, sandSiltClayInBag_tPha, sandSiltClayInSoil_tPha, soilOrganicC_pct: single;
  begin
  if soil = nil then exit;
  with soil do
  try
  if amount_tPha <= 0 then exit;
  { 1. if bag has any mulch, add to soil mulch }
  addQuantity(mulch.flatCropResidue_tPha, bag.mulchFlatCropResidue_pct * pct_to_frn * amount_tPha);
  addQuantity(mulch.organicNFresh_kgPha, bag.mulchOrganicNFresh_pct * pct_to_frn * amount_tPha * t_to_kg);
  addQuantity(mulch.organicPFresh_kgPha, bag.mulchOrganicPFresh_pct * pct_to_frn * amount_tPha * t_to_kg);
  { 2. if bag has organic matter, add OM to top layer and adjust bulk density and porosity of top layer }
  if bag.organicMatter_pct > 0 then
    begin
    addQuantity(layers[0].organicMatter_tPha, bag.organicMatter_pct * pct_to_frn * amount_tPha);
    { adjust bulk density for new OM }
    organicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[0].organicMatter_tPha,
        layers[0].weight_tPha + amount_tPha);
    { a ratio  of about 1:1.7 can be assumed to exist between the organic carbon and the soil humus }
    organicMatterFraction_frn := organicC_pct * pct_to_frn * 1.72;
    { adjust bulk density for amount of organic matter }
    bulkDensityInOM_tPm3 := safediv(1.0 - organicMatterFraction_frn, safediv(1.0,
      layers[0].bulkDensity_tPm3) - organicMatterFraction_frn / 0.224);
    if (bulkDensityInOM_tPm3 >= 2.5) or (bulkDensityInOM_tPm3 <= layers[0].bulkDensity_tPm3) then
      begin
      oldBulkDensity_tPm3 := layers[0].bulkDensity_tPm3;
      layers[0].bulkDensity_tPm3 := safediv(1.0, organicMatterFraction_frn / 0.224 + (1.0 -
        organicMatterFraction_frn) / 2.0);
      { adjust settled bulk density by same amount }
      if layers[0].bulkDensity_tPm3 <> oldBulkDensity_tPm3 then
        layers[0].settledBulkDensity_tPm3 := layers[0].settledBulkDensity_tPm3
          * safediv(layers[0].bulkDensity_tPm3, oldBulkDensity_tPm3);
      end;
    layers[0].porosity_mm := Utils_PorosityFromBulkDensityRockContentAndThickness(
      layers[0].bulkDensity_tPm3, layers[0].rockContent_pct, layers[0].depth_m {same as thickness});
    Utils_CheckRelationOfFieldCapacityToPorosity(layers[0]);
    end;
  {  3. if bag has any other quantitites > 0, add them }
  with layers[0] do
    begin
    addQuantity(flatCropResidue_tPha, bag.flatCropResidue_pct * pct_to_frn * amount_tPha);
    addQuantity(nitrate_kgPha, bag.nitrate_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(ammonia_kgPha, bag.ammonia_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(organicNFresh_kgPha, bag.organicNFresh_pct * pct_to_frn * amount_tPha);
    addQuantity(organicNActiveHumus_kgPha, bag.organicNActiveHumus_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(organicNStableHumus_kgPha, bag.organicNStableHumus_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(labileP_kgPha, bag.labileP_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(organicPFresh_kgPha, bag.organicPFresh_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(organicPHumus_kgPha, bag.organicPHumus_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(mineralPActive_kgPha, bag.mineralPActive_pct * pct_to_frn * amount_tPha * t_to_kg);
    addQuantity(mineralPStable_kgPha, bag.mineralPStable_pct * pct_to_frn * amount_tPha * t_to_kg);
    end;
  { 4. mix in sand, silt, clay and rocks - convert percents to weights, mix in weights, convert to % }
  { convert percents in bag to amounts - consider rocks and OM as outside the sand-silt-clay breakdown }
  bagRocks_tPha := bag.rocks_pct * pct_to_frn * amount_tPha;
  sandSiltClayInBag_tPha := amount_tPha * (100.0 - bag.organicMatter_pct - bag.rocks_pct) * pct_to_frn;
  bagSand_tPha := bag.sand_pct * pct_to_frn * sandSiltClayInBag_tPha;
  bagSilt_tPha := bag.silt_pct * pct_to_frn * sandSiltClayInBag_tPha;
  bagClay_tPha := bag.clay_pct * pct_to_frn * sandSiltClayInBag_tPha;
  { convert percents in soil to amounts in same way }
  soilRocks_tPha := layers[0].rockContent_pct * pct_to_frn * layers[0].weight_tPha;
  soilOrganicC_pct := Utils_OrganicCFromOrganicMatter_pct(layers[0].organicMatter_tPha, layers[0].weight_tPha);
  sandSiltClayInSoil_tPha := layers[0].weight_tPha * (100.0 - soilOrganicC_pct - layers[0].rockContent_pct) * pct_to_frn;
  soilSand_tPha := layers[0].sandContent_pct * pct_to_frn * sandSiltClayInSoil_tPha;
  soilSilt_tPha := layers[0].siltContent_pct * pct_to_frn * sandSiltClayInSoil_tPha;
  soilClay_tPha := layers[0].clayContent_pct * pct_to_frn * sandSiltClayInSoil_tPha;
  { add bag amounts to soil amounts }
  soilRocks_tPha := soilRocks_tPha + bagRocks_tPha;
  soilSand_tPha := soilSand_tPha + bagSand_tPha;
  soilSilt_tPha := soilSilt_tPha + bagSilt_tPha;
  soilClay_tPha := soilClay_tPha + bagClay_tPha;
  { convert amounts back to percents }
  layers[0].rockContent_pct := safediv(soilRocks_tPha, layers[0].weight_tPha) * frn_to_pct;
  sandSiltClayInSoil_tPha := soilSand_tPha + soilSilt_tPha + soilClay_tPha;
  layers[0].sandContent_pct := safediv(soilSand_tPha, sandSiltClayInSoil_tPha) * frn_to_pct;
  layers[0].siltContent_pct := safediv(soilSilt_tPha, sandSiltClayInSoil_tPha) * frn_to_pct;
  layers[0].clayContent_pct := safediv(soilClay_tPha, sandSiltClayInSoil_tPha) * frn_to_pct;
  { extra check for sand silt clay }
  layers[0].sandContent_pct := max(0.0, 100.0 - layers[0].clayContent_pct - layers[0].siltContent_pct);
  { 5. add water to first layer if there is water }
  if bag.waterContent_mPm > 0 then
    begin
    { assume bag bulk density is an average value of 1.33 }
    bagBulkDensity_tPm3 := 1.33;
    bagThickness_m := Utils_SoilThicknessFromWeightAndBulkDensity_m(amount_tPha, bagBulkDensity_tPm3);
    waterAdded_mm := bag.waterContent_mPm * bagThickness_m * m_to_mm;
    addQuantity(layers[0].waterContent_mm, waterAdded_mm);
    end;
  { 6. add weight to soil }
  addQuantity(layers[0].weight_tPha, amount_tPha);
  { 7. if bag has calcium carbonate or equivalent, change pH }
  if bag.calciumCarbonateOrEquivalent_pct > 0 then
    applyLimeToFirstLayerOnly(soil, bag.calciumCarbonateOrEquivalent_pct * pct_to_frn * amount_tPha);
  { 8. if bag had sulfur or equivalent, change pH }
  if bag.sulfurOrEquivalent_pct > 0 then
    applySulfurToFirstLayerOnly(soil, bag.sulfurOrEquivalent_pct * pct_to_frn * amount_tPha);
  SoilPatchTotals;
  except
    errorMessage('SoilOp.addBag: problem');
  end;
  end;

class procedure SoilOp.applyLimeToFirstLayerOnly(soil: GsSoilPatch; amount_tPha: single);
  var
    layerBelowMaxTillageDepth: smallint;
    limeAdded_kgPha, limeNeeded_kgPha, organicC_pct, baseSaturation_pct, changeInBaseSaturation_pct, newBaseSaturation_pct,
      changeInBaseSaturationToRaisePHTo6p5_pct, changeInBaseSaturationToOffsetAlSat_pct,
      totalSoilWeightInMaxTillageDepth_tPha: single;
  { this function is tentative and untested }
  begin
  if soil = nil then exit;
  with soil do
  try
  { if pH over 6.5 don't change anything }
  if (layers[0].soilpH > 6.5) then exit;
  with layers[0] do
    begin
    organicC_pct := Utils_OrganicCFromOrganicMatter_pct(organicMatter_tPha, weight_tPha);
    baseSaturation_pct := EQ.BaseSaturation_pct(baseFormingCations_cmolPkg, cationExchangeCapacity_cmolPkg);
    end;
  limeAdded_kgPha := amount_tPha * t_to_kg;
  if (params.soilWeatheringType <> kHighlyWeatheredSoil) then
    begin
    { calculate amount of lime needed to raise pH to 6.5 }
    changeInBaseSaturationToRaisePHTo6p5_pct := EQ.ChangeInBaseSaturationToRaisePHTo6p5_pct(layers[0].soilpH,
      baseSaturation_pct);
    limeNeeded_kgPha := EQ.LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha(layers[0].weight_tPha * t_to_kg,
      layers[0].cationExchangeCapacity_cmolPkg, changeInBaseSaturationToRaisePHTo6p5_pct);
    { calculate real change in base saturation by proportion of ideal amount of lime }
    if (limeAdded_kgPha > limeNeeded_kgPha) then limeAdded_kgPha := limeNeeded_kgPha;
    changeInBaseSaturation_pct := changeInBaseSaturationToRaisePHTo6p5_pct * safediv(limeAdded_kgPha,
      limeNeeded_kgPha);
    { calculate new pH }
    layers[0].soilpH := EQ.NewpHForLimeAdded(limeAdded_kgPha, layers[0].cationExchangeCapacity_cmolPkg,
      layers[0].weight_tPha, layers[0].soilpH);
    { calculate other new values }
    layers[0].baseFormingCations_cmolPkg := pct_to_frn * (baseSaturation_pct + changeInBaseSaturation_pct) *
      layers[0].cationExchangeCapacity_cmolPkg;
    layers[0].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, organicC_pct, layers[0].soilpH);
    end
  else
    begin { highly weathered }
    { calculate amount of lime needed to counteract Al toxicity }
    changeInBaseSaturationToOffsetAlSat_pct := EQ.ChangeInBaseSaturationToOffsetAlSat_pct(layers[0].aluminumSaturation_pct,
      layers[0].cationExchangeCapacity_cmolPkg);
    limeNeeded_kgPha := EQ.LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha(changeInBaseSaturationToOffsetAlSat_pct,
      layers[0].weight_tPha);
    { calculate real change in base saturation by proportion of ideal amount of lime }
    if (limeAdded_kgPha > limeNeeded_kgPha) then limeAdded_kgPha := limeNeeded_kgPha;
    changeInBaseSaturation_pct := changeInBaseSaturationToOffsetAlSat_pct * safediv(limeAdded_kgPha, limeNeeded_kgPha);
    { set pH to 5.4 because these soils are strongly buffered, and also because this equation is not easily derived }
    layers[0].soilpH := 5.4;
    { calculate new values }
    layers[0].baseFormingCations_cmolPkg := max(0.02, layers[0].baseFormingCations_cmolPkg + changeInBaseSaturation_pct);
    newBaseSaturation_pct := EQ.BaseSaturation_pct(layers[0].baseFormingCations_cmolPkg,
        layers[0].cationExchangeCapacity_cmolPkg);
    layers[0].aluminumSaturation_pct := EQ.AluminumSaturation_pct(newBaseSaturation_pct, organicC_pct, layers[0].soilpH);
    end;
  { record new values for output }
  MeanWeightedLimingValues(totals.patchWeightedMeanOrganicC_pct,
    totals.patchWeightedMeanCationExchangeCapacity_cmolPkg, totals.patchWeightedMeanpH,
    totals.patchWeightedMeanBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  except
    errorMessage('SoilOp.applyLimeToFirstLayerOnly: problem');
  end;
  end;

class procedure SoilOp.applySulfurToFirstLayerOnly(soil: GsSoilPatch; amount_tPha: single);
  var
    sulfurAdded_kgPha, sulfurNeeded_kgPha: single;
    organicC_pct, baseSaturation_pct, changeInBaseSaturation_pct: single;
    changeInBaseSaturationToLowerPHTo5_pct: single;
    totalSoilWeightInMaxTillageDepth_tPha: single;
    layerBelowMaxTillageDepth: smallint;
  { this function is VERY VERY tentative and untested }
  begin
  if soil = nil then exit;
  with soil do
  try
  { if pH below 5.0 don't change anything }
  if (layers[0].soilpH < 5.0) then exit;
  { highly weathered soils are rarely alkiline }
  if (params.soilWeatheringType = kHighlyWeatheredSoil) then exit;
  with layers[0] do
    begin
    organicC_pct := Utils_OrganicCFromOrganicMatter_pct(organicMatter_tPha, weight_tPha);
    baseSaturation_pct := EQ.BaseSaturation_pct(baseFormingCations_cmolPkg, cationExchangeCapacity_cmolPkg);
    end;
  sulfurAdded_kgPha := amount_tPha * t_to_kg;
  { calculate amount of sulfur needed to lower pH to 5.0 }
  changeInBaseSaturationToLowerPHTo5_pct := EQ.ChangeInBaseSaturationToLowerPHTo5_pct(layers[0].soilpH, baseSaturation_pct);
  sulfurNeeded_kgPha := EQ.SulfurFor5PHForNonHighlyWeatheredSoil_kgPha(layers[0].weight_tPha * t_to_kg,
      layers[0].cationExchangeCapacity_cmolPkg, changeInBaseSaturationToLowerPHTo5_pct);
  { calculate real change in base saturation by proportion of ideal amount of sulfur }
  if (sulfurAdded_kgPha > sulfurNeeded_kgPha) then sulfurAdded_kgPha := sulfurNeeded_kgPha;
  changeInBaseSaturation_pct := changeInBaseSaturationToLowerPHTo5_pct * safediv(sulfurAdded_kgPha, sulfurNeeded_kgPha);
  { calculate new pH }
  layers[0].soilpH := EQ.NewpHForSulfurAdded(sulfurAdded_kgPha, layers[0].cationExchangeCapacity_cmolPkg,
      layers[0].weight_tPha, layers[0].soilpH);
  { calculate other new values }
  layers[0].baseFormingCations_cmolPkg := pct_to_frn * (baseSaturation_pct + changeInBaseSaturation_pct) *
      layers[0].cationExchangeCapacity_cmolPkg;
  layers[0].aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, organicC_pct, layers[0].soilpH);
  { record new values for output }
  MeanWeightedLimingValues(totals.patchWeightedMeanOrganicC_pct,
    totals.patchWeightedMeanCationExchangeCapacity_cmolPkg, totals.patchWeightedMeanpH,
    totals.patchWeightedMeanBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  except
    errorMessage('SoilOp.applySulfurToFirstLayerOnly: problem');
  end;
  end;

class procedure SoilOp.applyLime(soil: GsSoilPatch; amount_tPha: single);
  var
    layerBelowMaxTillageDepth: smallint;
    limeAdded_kgPha, limeNeeded_kgPha, organicC_pct, baseSaturation_pct, changeInBaseSaturation_pct, newBaseSaturation_pct,
      changeInBaseSaturationToRaisePHTo6p5_pct, changeInBaseSaturationToOffsetAlSat_pct,
      totalSoilWeightInMaxTillageDepth_tPha, meanWeightedOrganicC_pct, meanWeightedSoilpH, meanWeightedCEC_cmolPkg,
      meanWeightedBaseFormingCations_cmolPkg, aluminumSaturation_pct, newAluminumSaturation_pct, newSoilpH,
      newBaseFormingCations_cmolPkg: single;
  begin
  if soil = nil then exit;
  with soil do
  try
  meanWeightedOrganicC_pct := 0.0;
  meanWeightedSoilpH := 0.0;
  meanWeightedCEC_cmolPkg := 0.0;
  meanWeightedBaseFormingCations_cmolPkg := 0.0;
  totalSoilWeightInMaxTillageDepth_tPha := 0.0;
  limeAdded_kgPha := 0.0;
  baseSaturation_pct := 0.0;
  aluminumSaturation_pct := 0.0;
  changeInBaseSaturationToRaisePHTo6p5_pct := 0.0;
  changeInBaseSaturationToOffsetAlSat_pct := 0.0;
  newAluminumSaturation_pct := 0.0;
  newSoilpH := 0.0;
  newBaseFormingCations_cmolPkg := 0.0;
  limeNeeded_kgPha := 0.0;
  changeInBaseSaturation_pct := 0.0;
  { calculate mean weighted values for organic c, cec, pH, and base forming cations }
  MeanWeightedLimingValues(meanWeightedOrganicC_pct, meanWeightedCEC_cmolPkg, meanWeightedSoilpH,
    meanWeightedBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  { calculate present aluminum saturation }
  baseSaturation_pct := EQ.BaseSaturation_pct(meanWeightedBaseFormingCations_cmolPkg, meanWeightedCEC_cmolPkg);
  aluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, meanWeightedOrganicC_pct, meanWeightedSoilpH);
  limeAdded_kgPha := amount_tPha * t_to_kg;
  if (meanWeightedSoilpH > 6.5) then
    begin
    { if pH over 6.5 don't change anything }
    limeAdded_kgPha := 0.0;
    newAluminumSaturation_pct := aluminumSaturation_pct;
    newSoilpH := meanWeightedSoilpH;
    newBaseFormingCations_cmolPkg := meanWeightedBaseFormingCations_cmolPkg;
    end
  else
    begin
    { pH <= 6.5 }
    if (params.soilWeatheringType <> kHighlyWeatheredSoil) then 
      begin
      { calculate amount of lime needed to raise pH to 6.5 }
      changeInBaseSaturationToRaisePHTo6p5_pct := EQ.ChangeInBaseSaturationToRaisePHTo6p5_pct(meanWeightedSoilpH,
        baseSaturation_pct);
      limeNeeded_kgPha := EQ.LimeFor6p5PHForNonHighlyWeatheredSoil_kgPha(totalSoilWeightInMaxTillageDepth_tPha *
        kg_to_t, meanWeightedCEC_cmolPkg, changeInBaseSaturationToRaisePHTo6p5_pct);
      { calculate real change in base saturation by proportion of ideal amount of lime }
      if (limeAdded_kgPha > limeNeeded_kgPha) then limeAdded_kgPha := limeNeeded_kgPha;
      changeInBaseSaturation_pct := changeInBaseSaturationToRaisePHTo6p5_pct * safediv(limeAdded_kgPha,
        limeNeeded_kgPha);
      { calculate new pH }
      newSoilpH := EQ.NewpHForLimeAdded(limeAdded_kgPha, meanWeightedCEC_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha,
        meanWeightedSoilpH);
      { calculate other new values }
      newBaseFormingCations_cmolPkg := pct_to_frn * (baseSaturation_pct + changeInBaseSaturation_pct) *
        meanWeightedCEC_cmolPkg;
      newAluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, meanWeightedOrganicC_pct, newSoilpH);
      end
    else 
      begin
      { highly weathered }
      { calculate amount of lime needed to counteract Al toxicity }
      changeInBaseSaturationToOffsetAlSat_pct := EQ.ChangeInBaseSaturationToOffsetAlSat_pct(aluminumSaturation_pct,
        meanWeightedCEC_cmolPkg);
      limeNeeded_kgPha := EQ.LimeToNeutralizeAlForHighlyWeatheredSoil_kgPha(changeInBaseSaturationToOffsetAlSat_pct,
        totalSoilWeightInMaxTillageDepth_tPha);
      { calculate real change in base saturation by proportion of ideal amount of lime }
      if (limeAdded_kgPha > limeNeeded_kgPha) then limeAdded_kgPha := limeNeeded_kgPha;
      changeInBaseSaturation_pct := changeInBaseSaturationToRaisePHTo6p5_pct * safediv(limeAdded_kgPha,
        limeNeeded_kgPha);
      { set pH to 5.4 because these soils are strongly buffered }
      { and also because this equation is not easily derived }
      newSoilpH := 5.4;
      { calculate new values }
      newBaseFormingCations_cmolPkg := max(0.02, meanWeightedBaseFormingCations_cmolPkg + changeInBaseSaturation_pct);
      baseSaturation_pct := EQ.BaseSaturation_pct(newBaseFormingCations_cmolPkg, meanWeightedCEC_cmolPkg);
      newAluminumSaturation_pct := EQ.AluminumSaturation_pct(baseSaturation_pct, meanWeightedOrganicC_pct, newSoilpH);
      end;
    end;
  { set the layer values to the new values (note this removes all differences between the layers) }
  SetLayerPropertiesAfterLiming(layerBelowMaxTillageDepth, newBaseFormingCations_cmolPkg, newSoilpH,
    newAluminumSaturation_pct);
  { record new values for output }
  MeanWeightedLimingValues(totals.patchWeightedMeanOrganicC_pct,
    totals.patchWeightedMeanCationExchangeCapacity_cmolPkg, totals.patchWeightedMeanpH,
    totals.patchWeightedMeanBaseFormingCations_cmolPkg, totalSoilWeightInMaxTillageDepth_tPha, layerBelowMaxTillageDepth);
  { don't need last two }
  except
    errorMessage('SoilOp.applyLime: problem');
  end;
  end;

end.
 
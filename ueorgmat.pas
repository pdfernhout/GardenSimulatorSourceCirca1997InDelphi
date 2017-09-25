unit ueorgmat;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ueorgmat: Organic matter blob object. Holding place for dead plant biomass before it
decays into soil patch. Whole reason for this object's existence is so user can drag
organic matter around and see it as a useful and important resource. Each soil patch
keeps a list of OM blobs and clears the list out on each day of simulation. Important
to understand that the OM blob biomass is added to the soil patch as well as the OM blob
when the OM blob is created, so nothing has to be added when the OM blob "decays".
This double-accounting is done so that the user can see the amount of OM on the patch
increase as they drag OM blobs to it and harvest plants (otherwise it looks like OM is missing).
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses
  WinTypes, WinProcs, umodel, uestruct, ufiler, ucollect, graphics;

type

GsOrganicMatter = class(GsGraphicalModel)
  public
  basePoint: TPoint;
  numRootLayers: smallint;
  standingDead_tPha: single;
  nInStandingDead_kgPha: single;
  pInStandingDead_kgPha: single;
  rootWeight_tPha: arraySoilLayers;
  organicNFresh_kgPha: arraySoilLayers;
  organicPFresh_kgPha: arraySoilLayers;
	procedure addPlantPartBiomass(plantPartProxy: GsStreamableObject;
			transferLiveBiomass: boolean);
	procedure addWholePlant(plantProxy: GsModel; amountHarvested_g: single;
    rootsHaveBeenHarvested: boolean);
  procedure addReseededPlantBiomass(biomassLost: BiomassLostInReseedingStructure);
	procedure transferToOrFromSoil(soilProxy: GsModel; toSoil: boolean);
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
	function objectType: integer; override;
  procedure drawOn(destCanvas: TCanvas); override;
	procedure setPosition(aPoint: TPoint);
  function hasContents: boolean;
	function preciseIncludesPointTest(const aPoint: TPoint): boolean;  override;
  end;

const
	kToSoil  = true;
	kFromSoil = false;

implementation

uses
  ueplant, uesoil, ueutils, umconsts, SysUtils, uaspects, udppart, uclasses, ugsim, uunits;

procedure GsOrganicMatter.addPlantPartBiomass(plantPartProxy: GsStreamableObject;
		transferLiveBiomass: boolean);
  var
    plantPart: GsDrawingPlantPart;
    plant: GsPlant;
     transferBiomass_kg, transferBiomass_tPha, transferN_kgPha, transferP_kgPha: single;
  begin
  try
  plantPart := plantPartProxy as GsDrawingPlantPart;
  if plantPart = nil then raise Exception.create('GsOrganicMatter.addPlant: nil plant part');
  plant := plantPart.plant.model as GsPlant;
  if plant = nil then raise Exception.create('GsOrganicMatter.addPlant: nil plant');
  if transferLiveBiomass then
    transferBiomass_kg := plantPart.totalBiomassForHarvest_kg
  else
    transferBiomass_kg := plantPart.deadBiomassForHarvest_kg;
  transferBiomass_tPha := safediv(transferBiomass_kg * kg_to_t, GsPlant(plantPart.plant.model).soil.params.area_ha);
  transferN_kgPha := transferBiomass_tPha * plant.nConcInLiveBiomass_kgPt;
  transferP_kgPha := transferBiomass_tPha * plant.pConcInLiveBiomass_kgPt;
  addQuantity(standingDead_tPha, transferBiomass_tPha);
  addQuantity(nInStandingDead_kgPha, transferN_kgPha);
  addQuantity(pInStandingDead_kgPha, transferP_kgPha);
  except on e: Exception do errorMessage('Problem in GsOrganicMatter.addPlantPartBiomass' + e.message); end;
  end;

procedure GsOrganicMatter.addWholePlant(plantProxy: GsModel; amountHarvested_g: single;
    rootsHaveBeenHarvested: boolean);
  var
    plant: GsPlant;
    soilPatch: GsSoilPatch;
    layer: integer;
    transferBiomass_tPha, transferN_kgPha, transferP_kgPha: single;
    g_to_tPha: single;
    i: longint;
  begin
  try
  plant := plantProxy as GsPlant;
  if plant = nil then
    raise Exception.create('GsOrganicMatter.addWholePlant: nil plant');
  soilPatch := plant.soil;
  if soilPatch = nil then
    raise Exception.create('GsOrganicMatter.addWholePlant: nil soil patch');
  g_to_tPha := safediv(g_to_kg * kg_to_t, soilPatch.params.area_ha);
  transferBiomass_tPha := plant.biomass.standingDead_tPha + plant.biomass.standingLive_tPha
      - amountHarvested_g * g_to_tPha;
  transferN_kgPha := plant.biomass.standingLive_tPha * plant.nConcInLiveBiomass_kgPt + plant.nutrients.nInStandingDead_kgPha;
  transferP_kgPha := plant.biomass.standingLive_tPha * plant.pConcInLiveBiomass_kgPt + plant.nutrients.pInStandingDead_kgPha;
  addQuantity(standingDead_tPha, transferBiomass_tPha);
  addQuantity(nInStandingDead_kgPha, transferN_kgPha);
  addQuantity(pInStandingDead_kgPha, transferP_kgPha);
  if not rootsHaveBeenHarvested then
    begin
    numRootLayers := plant.biomass.numLayersWithRootsInThem;
    if numRootLayers > 0 then
      for i := 0 to numRootLayers - 1 do
        begin
        rootWeight_tPha[i] := plant.biomass.rootWeightByLayer_tPha[i];
        { use same N and P conc as rest of plant }
        organicNFresh_kgPha[i] := rootWeight_tPha[i] * plant.nConcInLiveBiomass_kgPt;
        organicPFresh_kgPha[i] := rootWeight_tPha[i] * plant.pConcInLiveBiomass_kgPt;
        end;
    end;
  except on e: Exception do errorMessage('Problem in GsOrganicMatter.addWholePlant' + e.message); end;
  end;

procedure GsOrganicMatter.addReseededPlantBiomass(biomassLost: BiomassLostInReseedingStructure);
  var
    i: smallint;
  begin
  addQuantity(standingDead_tPha,
      biomassLost.standingDead_tPha + biomassLost.standingLive_tPha);
  addQuantity(nInStandingDead_kgPha,
      biomassLost.standingLive_tPha * biomassLost.nConcInLiveBiomass_kgPt
      + biomassLost.standingDead_tPha * biomassLost.nConcInStandingDead_kgPt);
  addQuantity(pInStandingDead_kgPha,
      biomassLost.standingLive_tPha * biomassLost.pConcInLiveBiomass_kgPt
      + biomassLost.standingDead_tPha * biomassLost.pConcInStandingDead_kgPt);
  for i := 0 to kMaxPossibleLayers - 1 do
    begin
    rootWeight_tPha[i] := biomassLost.rootWeightByLayer_tPha[i];
    { use same N and P conc as rest of plant }
    organicNFresh_kgPha[i] := rootWeight_tPha[i] * biomassLost.nConcInLiveBiomass_kgPt;
    organicPFresh_kgPha[i] := rootWeight_tPha[i] * biomassLost.pConcInLiveBiomass_kgPt;
    end;
  end;

procedure GsOrganicMatter.transferToOrFromSoil(soilProxy: GsModel; toSoil: boolean);
  var
    soil: GsSoilPatch;
    layer: integer;
  begin
  try
  soil := soilProxy as GsSoilPatch;
  with soil do
  begin
  if toSoil = kToSoil then
    begin
    addQuantity(mulch.flatCropResidue_tPha, standingDead_tPha);
    addQuantity(movement.flatCropResidueFromOM_tPha, standingDead_tPha);
  	addQuantity(mulch.organicNFresh_kgPha, nInStandingDead_kgPha);
    addQuantity(movement.organicNFreshFromOM_kgPha, nInStandingDead_kgPha);
  	addQuantity(mulch.organicPFresh_kgPha, pInStandingDead_kgPha);
    addQuantity(movement.organicPFreshFromOM_kgPha, pInStandingDead_kgPha);
  	if numRootLayers > 0 then for layer := 0 to numRootLayers - 1 do
    	begin
    	addQuantity(layers[layer].flatCropResidue_tPha, rootWeight_tPha[layer]);
      addQuantity(movement.flatCropResidueFromOM_tPha, rootWeight_tPha[layer]);
    	addQuantity(layers[layer].organicNFresh_kgPha, organicNFresh_kgPha[layer]);
      addQuantity(movement.organicNFreshFromOM_kgPha, organicNFresh_kgPha[layer]);
    	addQuantity(layers[layer].organicPFresh_kgPha, organicPFresh_kgPha[layer]);
      addQuantity(movement.organicPFreshFromOM_kgPha, organicPFresh_kgPha[layer]);
    	end;
    end
  else
    begin
    subtractQuantity(mulch.flatCropResidue_tPha, standingDead_tPha);
    subtractQuantity(movement.flatCropResidueFromOM_tPha, standingDead_tPha);
 	  subtractQuantity(mulch.organicNFresh_kgPha, nInStandingDead_kgPha);
    subtractQuantity(movement.organicNFreshFromOM_kgPha, nInStandingDead_kgPha);
  	subtractQuantity(mulch.organicPFresh_kgPha, pInStandingDead_kgPha);
    subtractQuantity(movement.organicPFreshFromOM_kgPha, pInStandingDead_kgPha);
   	if numRootLayers > 0 then for layer := 0 to numRootLayers - 1 do
    	begin
      { since this may be the last layer after several OM blobs were added and then subtracted,
        there could be a rounding error that would drive the amounts below zero, so check
        subtraction amounts for that possibility }
      { root weight }
      rootWeight_tPha[layer] := min(movement.flatCropResidueFromOM_tPha, min(layers[layer].flatCropResidue_tPha,
          rootWeight_tPha[layer]));
    	subtractQuantity(layers[layer].flatCropResidue_tPha, rootWeight_tPha[layer]);
      subtractQuantity(movement.flatCropResidueFromOM_tPha, rootWeight_tPha[layer]);
      { organic N fresh }
      organicNFresh_kgPha[layer] := min(movement.organicNFreshFromOM_kgPha, min(layers[layer].organicNFresh_kgPha,
          organicNFresh_kgPha[layer]));
    	subtractQuantity(layers[layer].organicNFresh_kgPha, organicNFresh_kgPha[layer]);
      subtractQuantity(movement.organicNFreshFromOM_kgPha, organicNFresh_kgPha[layer]);
      { organic P fresh }
      organicPFresh_kgPha[layer] := min(movement.organicPFreshFromOM_kgPha, min(layers[layer].organicPFresh_kgPha,
          organicPFresh_kgPha[layer])); 
    	subtractQuantity(layers[layer].organicPFresh_kgPha, organicPFresh_kgPha[layer]);
      subtractQuantity(movement.organicPFreshFromOM_kgPha, organicPFresh_kgPha[layer]);
    	end;
    end;
    SoilPatchTotals;
    end;
  except on e: Exception do errorMessage('Problem in GsOrganicMatter.transferToOrFromSoil' + e.message); end;
  end;

procedure GsOrganicMatter.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsOrganicMatter;
  cvir.versionNumber := 2;
  cvir.additionNumber := 0;
  end;

procedure GsOrganicMatter.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamPoint(basePoint);
  filer.streamSmallint(numRootLayers);
  filer.streamSingle(standingDead_tPha);
  filer.streamSingle(nInStandingDead_kgPha);
  filer.streamSingle(pInStandingDead_kgPha);
  filer.streamSingleArray(rootWeight_tPha);
  filer.streamSingleArray(organicNFresh_kgPha);
  filer.streamSingleArray(organicPFresh_kgPha);
  end;

function GsOrganicMatter.objectType: integer;
  begin
  result := kObjectTypeBlob;
  end;

procedure GsOrganicMatter.drawOn(destCanvas: TCanvas);
  begin
  {width and height should be relative to icon size}
  destCanvas.draw(basePoint.x - 16, basePoint.y - 16, GardenForm.harvestIconDefault.picture.icon);
  end;

procedure GsOrganicMatter.setPosition(aPoint: TPoint);
  begin
  basePoint := aPoint;
  boundsRect.left := aPoint.x - 16;
  boundsRect.top := aPoint.y - 16;
  boundsRect.right := aPoint.x + 16;
  boundsRect.bottom := aPoint.y + 16;
  end;

{this function checks if point on icon bitmap}
function GsOrganicMatter.preciseIncludesPointTest(const aPoint: TPoint): boolean;
	var
  testPixel, backgroundPixel: TColor;
  xPixel, yPixel: longint;
  begin
  xPixel := aPoint.x - boundsRect.left;
  yPixel := aPoint.y - boundsRect.top;
  backgroundPixel := GardenForm.harvestIconDefaultBitmap.canvas.Pixels[0, 0];
  testPixel := GardenForm.harvestIconDefaultBitmap.canvas.Pixels[xPixel, yPixel];
  result := backgroundPixel <> testPixel;
	end;

function GsOrganicMatter.hasContents: boolean;
  var i: longint;
  begin
  result := false;
  if standingDead_tPha > 0.0 then result := true;
  if result or (numRootLayers = 0) then exit;
  for i := 0 to numRootLayers - 1 do
    if rootWeight_tPha[i] > 0.0 then
      result := true;
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


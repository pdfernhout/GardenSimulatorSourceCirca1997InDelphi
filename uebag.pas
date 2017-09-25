unit Uebag;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uebag: Bag model object. Not very complicated; just holds materials. Functions in soil
patch operations file (uesoilop) handle using the bag. This file mainly just has
methods for data transfer and streaming.}

interface

uses umodel, ucollect, ufiler, ufilertx;

type

BagStructure = record
  { aspects->start }
  { object->bag }
  { access->contents }
  mulchFlatCropResidue_pct: single;
  mulchOrganicNFresh_pct: single;
  mulchOrganicPFresh_pct: single;
  organicMatter_pct: single;
  flatCropResidue_pct: single;
  nitrate_pct: single;
  ammonia_pct: single;
  organicNFresh_pct: single;
  organicNActiveHumus_pct: single;
  organicNStableHumus_pct: single;
  labileP_pct: single;
  organicPFresh_pct: single;
  organicPHumus_pct: single;
  mineralPActive_pct: single;
  mineralPStable_pct: single;
  waterContent_mPm: single;
  sand_pct: single;
  silt_pct: single;
  clay_pct: single;
  rocks_pct: single;
  calciumCarbonateOrEquivalent_pct: single;
  sulfurOrEquivalent_pct: single;
  reservedArray: array [1..160] of byte;
  end;

GsBag = class(GsGraphicalModel)
  public
  { access-> } {none}
  materialType: smallint;
  { aspects->stop }
  contents: BagStructure;
	function objectType: integer; override;
  procedure directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection); override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure BDConvert(var modelValue: single; var value;
    fieldID, deriveMethod, fieldType, direction, index: integer; updateList: TListCollection);
  procedure convertPercentAndConcentration(d: integer; var modelValue: single; var value; layer: integer);
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  end;

implementation

uses SysUtils, uaspects, umconsts, uclasses, uunits, ueutils;

function GsBag.objectType: integer;
  begin
  result := kObjectTypeBag;
  end;

const
  kConvertConc = 1;

procedure GsBag.directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  Bag_directTransferField(self, v, d, fieldID, ft, index, deriveMethod, updateList);
  self.addToUpdateList(fieldID, index, updateList);
  end;

{USAGE:
      id: BDConvert(accessor, v, fieldID, deriveMethod, ft, d, index, updateList);
}
procedure GsBag.BDConvert(var modelValue: single; var value;
    fieldID, deriveMethod, fieldType, direction, index: integer; updateList: TListCollection);
  begin
  case deriveMethod of
    kDeriveTypeUndefined: self.MFD(modelValue, value, fieldType, direction);
    kDeriveTypeDepth:  raise Exception.create('Depth derivation method not supported by bag');
    kDeriveTypeConcentration:  raise Exception.create('Concentration from mass derivation method not supported by bag');
    kDeriveTypeArea: raise Exception.create('Area derivation method not supported by bag');
    kDeriveTypeConcentrationFromPercent: self.convertPercentAndConcentration(direction, modelValue, value, index);
    else
      raise Exception.create('Derivation method not supported by bag');
    end;
  self.addToUpdateList(fieldID, index, updateList);
  end;

procedure GsBag.convertPercentAndConcentration(d: integer; var modelValue: single; var value; layer: integer);
  var
    gPt_to_pct: single;
  begin
  gPt_to_pct := g_to_kg * kg_to_t * frn_to_pct;
  if d = kSetField then
    begin
    modelValue := single(value) * gPt_to_pct;
    end
  else
    begin
    try
    single(value) := modelValue / gPt_to_pct;
    except
      single(value) := 0.0;
      errorMessage('Problem dividing ' + floatToStr(modelValue) + ' by ' + floatToStr(gPt_to_pct));
    end;
    end;
  end;

procedure GsBag.streamUsingTextFiler(textFiler: GsTextFiler);
  begin
  { don't read or write name }
  with textFiler, contents do
    begin
    streamSingle(mulchFlatCropResidue_pct, 'mulchFlatCropResidue_pct');
    streamSingle(mulchOrganicNFresh_pct, 'mulchOrganicNFresh_pct');
    streamSingle(mulchOrganicPFresh_pct, 'mulchOrganicPFresh_pct');
    streamSingle(organicMatter_pct, 'organicMatter_pct');
    streamSingle(flatCropResidue_pct, 'flatCropResidue_pct');
    streamSingle(nitrate_pct, 'nitrate_pct');
    streamSingle(ammonia_pct, 'ammonia_pct');
    streamSingle(organicNFresh_pct, 'organicNFresh_pct');
    streamSingle(organicNActiveHumus_pct, 'organicNActiveHumus_pct');
    streamSingle(organicNStableHumus_pct, 'organicNStableHumus_pct');
    streamSingle(mulchFlatCropResidue_pct, 'mulchFlatCropResidue_pct');
    streamSingle(labileP_pct, 'labileP_pct');
    streamSingle(organicPFresh_pct, 'organicPFresh_pct');
    streamSingle(organicPHumus_pct, 'organicPHumus_pct');
    streamSingle(mineralPActive_pct, 'mineralPActive_pct');
    streamSingle(mineralPStable_pct, 'mineralPStable_pct');
    streamSingle(waterContent_mPm, 'waterContent_mPm');
    streamSingle(sand_pct, 'sand_pct');
    streamSingle(silt_pct, 'silt_pct');
    streamSingle(clay_pct, 'clay_pct');
    streamSingle(rocks_pct, 'rocks_pct');
    streamSingle(calciumCarbonateOrEquivalent_pct, 'calciumCarbonateOrEquivalent_pct');
    streamSingle(sulfurOrEquivalent_pct, 'sulfurOrEquivalent_pct');
    streamEndOfLine;
    end;
  end;

procedure GsBag.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsBag;
  cvir.versionNumber := 1;
  cvir.additionNumber := 0;
  end;

procedure GsBag.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamBytes(contents, sizeOf(contents));
  filer.streamSmallint(materialType);
  end;

end.

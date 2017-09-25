unit Uclasses;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uclasses: Sets up class IDs for streaming, method to create streamable classes.}

interface

uses ufiler;

const
	KTNil = 14501;
	KGsStreamableObject = 14502;
  {GWI specific classes}
	KGsAspect = 14503;
	KGsAspectManager = 14504;
	{KGsCategory = 14505; - obsolete}
	KGsDrawingPlantPart = 14506;
	KGsFlowerFruit = 14507;
	KGsInflorescence = 14508;
	KGsInternode = 14509;
	KGsLeaf = 14510;
	KGsMeristem = 14511;
	KGsGroup = 14512;
	KGsGroupItem = 14513;
	KGsGroupManager = 14514;
	KGsHarvestItem = 14515;
	KGsHarvestItemTemplate = 14516;
	KGsHarvestManager = 14517;
	KGsHarvestReport = 14518;
	KGsHarvestItemSortingGroup = 14519;
	KGsIcon = 14520;
	KGsLoggedVar = 14521;
	KGsModel = 14522;
	KGsDomain = 14523;
	KGsDrawingPlant = 14524;
	KGsGraphicalModel = 14525;
	KGsBag = 14526;
	{KGsEdibleItem = 14527; - obsolete}
	KGsGarden = 14528;
	KGsOrganicMatter = 14529;
	KGsPlant = 14530;
	KGsSoilPatch = 14531;
	{KGsSoilPesticide = 14532; - obsolete}
	KGsWeather = 14533;
	{KGsPropertyValue = 14534; - obsolete}
	{KGsRealPropertyValue = 14535; - obsolete}
	{KGsToolActionPropertyValue = 14536; - obsolete}
	KGsRandom = 14537;
	KGsSound = 14538;
	KGsTemplateManager = 14539;
	KGsTool = 14540;
	KGsToolManager = 14541;
	KGsToolProperty = 14542; { obsolete }
	KKfIndexTriangle = 14543;
	KKfObject3D = 14544;
  {out of initial sequence from browser - added later}
  KGsToolSettings = 14545;
  KGsToolAction = 14546;
  KGsToolRateOrAmountValue = 14547;
  KGsToolParamListManager = 14548;
  KGsToolContents = 14549;
  KGsWindowManager = 14550;


function CreateStreamableObjectFromClassAndVersionInformation(
	const cvir: GsClassAndVersionInformationRecord): GsStreamableObject;

implementation

uses sysutils, utools, ulogvar;

function CreateStreamableObjectFromClassAndVersionInformation(
		const cvir: GsClassAndVersionInformationRecord): GsStreamableObject;
  begin
  case cvir.classNumber of
  	KTNil: result := nil;
    KGsLoggedVar: result := GsLoggedVar.create;
  	KGsStreamableObject:
      raise Exception.create('Should not stream GsStreamableObject abstract class ' + IntToStr(cvir.classNumber));
  	else
      begin
      raise Exception.create('Unknown class ' + IntToStr(cvir.classNumber));
      result := nil;
      end;
    end;
  end;
end.

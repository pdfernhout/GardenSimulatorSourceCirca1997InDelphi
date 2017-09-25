unit Utempman;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utempman: Template manager. Templates are objects used to create garden objects by making
copies of them. Climate templates are used to make weather objects; soil type templates
are used to make soil patches; cultivar templates are used to make plants. Soil amendment
templates are used differently - they are used when applying fertilizers with the bag
tool (the 'apply' tool action) and have no corresponding garden objects. Note that the
template manager has a pesticide list; this is left over from when we had implemented
pesticides and taking it out would make the garden files unreadable. The icons and
harvest item templates (HITs) are a little complicated: plants just have pointers (references)
to them so that memory is not used redundantly. This means the methods for assigning
and streaming them are a little complicated.}

interface

uses StdCtrls, Classes, ufiler, ufilertx, ucollect, uebag, ueplant, uesoil, ueweath, 
	umodel, uicon, uturt3d, uharvprt;

type

GsTemplateManager = class(GsStreamableObject)
  public
  bagList: TListCollection;
  pesticideTypeList: TListCollection;
  cultivarList: TListCollection;
  soilTypeList: TListCollection;
  climateList: TListCollection;
  defaultSoilTypeIndex: longint;
  defaultClimateIndex: longint;
  tdoList: TListCollection;
  iconList: TListCollection;
  harvestItemTemplateList: TListCollection;
  streamOnlyResources: boolean;
  { create / destroy }
  constructor create; override;
  destructor destroy; override;
  procedure initializeHardCodedTemplates;
  { streaming }
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure streamReferenceToObject3d(filer: GsFiler; var object3D: KfObject3d);
  procedure streamReferenceToIcon(filer: GsFiler; var icon: GsIcon);
  procedure streamReferenceToHarvestItemTemplate(filer: GsFiler; var harvestItemTemplate: GsHarvestItemTemplate);
  { creating new templates }
  function copyFromWeather(weather: GsWeather; isTemplate: boolean): GsWeather;
  function copyFromPlant(plant: GsPlant; isTemplate: boolean): GsPlant;
  function copyFromSoilPatch(soilPatch: GsSoilPatch; isTemplate: boolean): GsSoilPatch;
  function copyFromBag(bag: GsBag): GsBag;
  function loadObject3D(fileName: string): KfObject3d;
	function loadGsIcon(fileName: string): GsIcon;
  function createNewHarvestItemTemplate(aName, iconName: string): GsHarvestItemTemplate;
  { removing templates }
  function removeTemplate(model: GsModel): boolean;
  procedure removeAllItems;
  { defaults }
  function defaultSoilType: GsSoilPatch;
  function defaultClimate: GsWeather;
  procedure fixUpDefaultIndexes;
  { interaction with dialogs }
  procedure loadObjectNamesIntoTStrings(stringList: TStrings);
  procedure loadListObjectNamesIntoTStrings(stringList: TStrings; list: TListCollection);
  procedure setInUseFlagsForHarvestItemTemplateList(includeGarden: boolean);
  procedure setAllHarvestItemTemplateInUseFlagsToFalse;
  procedure setAllIconInUseFlagsToFalse;
  procedure setInUseFlagsForIconList(includeGarden: boolean);
  procedure setAllTdoInUseFlagsToFalse;
  procedure setInUseFlagsForTdoList(includeGarden: boolean);
  procedure fillComboBoxWithBagsForTools(comboBox: TComboBox);
  procedure fillComboBoxWithCultivarsForTools(comboBox: TComboBox);
  { finding matches }
	function findMatchForObject3d(original: KfObject3D): KfObject3d;
	function findMatchForObject3dByName(name: string): KfObject3d;
  function findMatchForHarvestItemTemplate(original: GsHarvestItemTemplate): GsHarvestItemTemplate;
	function findMatchForIcon(original: GsIcon): GsIcon;
  function findMatchForIconByName(name: string): GsIcon;
  function findHarvestItemTemplate(aName: string): GsHarvestItemTemplate;
  function findCultivarBasedOnName(aName: string): GsPlant;
  function findObjectInListBasedOnName(objectType: smallint; objectName: string): GsStreamableObject;
  { fixing up }
	procedure fixupCopiedCultivar(newCultivar: GsPlant; original: GsPlant);
	procedure fixupPlantResources(plant: GsPlant);
	procedure fixupHarvestItemResources(harvestItem: GsHarvestItem);
  { garbage collecting }
  procedure garbageCollectIconList(includeGarden, showMessages: boolean);
  procedure garbageCollectTdoList(includeGarden, showMessages: boolean);
  procedure garbageCollectHarvestItemTemplateList(includeGarden, showMessages: boolean);
  procedure garbageCollect(includeGarden, showMessages: boolean);
  { tabbed text file i/o }
  procedure saveAllTemplatesToFileName(fileName: string);
  procedure saveListOfTemplatesWithTextFiler(textFiler: GsTextFiler; aList: TListCollection; objectType: smallint);
  procedure readAllTemplatesFromFileName(fileName: string);
  procedure createNewObjectWithTextFiler(objectType: smallint; textFiler: GsTextFiler; objectName: string);
  end;

implementation

uses Sysutils, Dialogs, WinTypes, WinProcs, Messages, Graphics, Controls, Forms,
  udomain, uaspects, usstream, udebug, udplant, ucommand, ugsim, ugscom, uclasses, uhardcd;

{ --------------------------------------------------------------------------------------------- create/destroy }
constructor GsTemplateManager.create;
  begin
  inherited create;
  bagList := TListCollection.create;
  pesticideTypeList := TListCollection.create;
  cultivarList := TListCollection.create;
  soilTypeList := TListCollection.create;
  climateList := TListCollection.create;
  defaultSoilTypeIndex := 0;
  defaultClimateIndex := 0;
  tdoList := TListCollection.create;
  iconList := TListCollection.create;
  harvestItemTemplateList := TListCollection.create;
  streamOnlyResources := false;
  end;

destructor GsTemplateManager.destroy;
  begin
  bagList.free;
  bagList := nil;
  pesticideTypeList.free;
  pesticideTypeList := nil;
  cultivarList.free;
  cultivarList := nil;
  soilTypeList.free;
  soilTypeList := nil;
  climateList.free;
  climateList := nil;
  {free shared tdos}
  tdoList.free;
  tdoList := nil;
  {free shared icons}
  iconList.free;
  iconList := nil;
  {free shared harvest item templates}
  harvestItemTemplateList.free;
  harvestItemTemplateList := nil;
  inherited destroy;
  end;

{ ---------------------------------------------------------------------------------------- interaction with dialogs }
procedure GsTemplateManager.loadObjectNamesIntoTStrings(stringList: TStrings);
	begin
  stringList.addObject('--- Templates ---', nil);
  stringList.addObject('Climates', nil);
  self.loadListObjectNamesIntoTStrings(stringList, climateList);
  stringList.addObject('Soil types', nil);
  self.loadListObjectNamesIntoTStrings(stringList, soilTypeList);
  stringList.addObject('Cultivars', nil);
  self.loadListObjectNamesIntoTStrings(stringList, cultivarList);
  stringList.addObject('Bags', nil);
  self.loadListObjectNamesIntoTStrings(stringList, bagList);
  end;

procedure GsTemplateManager.loadListObjectNamesIntoTStrings(stringList: TStrings; list: TListCollection);
  var
    i: integer;
    model: GsModel;
	begin
  if list.count > 0 then
    begin
    for i := 0 to list.count - 1 do
      begin
      model := GsModel(list.items[i]);
      if length(model.getName) = 0 then model.setName('unnamed');
  	  stringList.addObject('  T: ' + model.getName, model);
      end;
    end
  else
    stringList.addObject('  {none}', nil);
  end;

procedure GsTemplateManager.fillComboBoxWithBagsForTools(comboBox: TComboBox);
  var
    i: integer;
    model: GsModel;
	begin
  comboBox.sorted := true;
  if bagList.count > 0 then for i := 0 to bagList.count - 1 do
    begin
    model := GsModel(bagList.items[i]);
    if length(model.getName) = 0 then model.setName('unnamed');
    { CFK FIX - later append seed description string after name - e.g., seeds, tubers, bulbs }
  	comboBox.items.addObject(model.getName, model);
    end;
  end;

procedure GsTemplateManager.fillComboBoxWithCultivarsForTools(comboBox: TComboBox);
  var
    i: integer;
    model: GsModel;
	begin
  comboBox.sorted := true;
  if cultivarList.count > 0 then for i := 0 to cultivarList.count - 1 do
    begin
    model := GsModel(cultivarList.items[i]);
    if length(model.getName) = 0 then model.setName('unnamed');
    { should later append seed description string after name - e.g., seeds, tubers, bulbs }
  	comboBox.items.addObject(model.getName, model);
    end;
  end;

{ --------------------------------------------------------------------------------------------- finding objects }
function GsTemplateManager.findCultivarBasedOnName(aName: string): GsPlant;
  var
    i: longint;
    cultivar: GsPlant;
  begin
  result := nil;
  if cultivarList.count > 0 then
    for i := 0 to cultivarList.count - 1 do
      begin
      cultivar := GsPlant(cultivarList.items[i]);
      if cultivar.getName = aName then
        begin
        result := cultivar;
        exit;
        end;
      end;
  end;

function GsTemplateManager.findObjectInListBasedOnName(objectType: smallint; objectName: string): GsStreamableObject;
  var
    i: longint;
    anObject: GsStreamableObject;
    theList: TListCollection;
  begin
  result := nil;
  theList := nil;
  case objectType of
    kObjectTypeSoil: theList := soilTypeList;
    kObjectTypePlant: theList := cultivarList;
    kObjectTypeWeather: theList := climateList;
    kObjectTypeBag: theList := bagList;
    kObjectTypeHarvestItemTemplate: theList := harvestItemTemplateList;
    kObjectType3DObject: theList := tdoList;
    kObjectTypeIcon: theList := iconList;
    else
      raise Exception.create('GsTextFiler.findObjectInListBasedOnName: objects of this type do not have a name');
    end;
  if theList = nil then exit;
  if theList.count > 0 then
    for i := 0 to theList.count - 1 do
      begin
      anObject := GsStreamableObject(theList.items[i]);
      if anObject.getName = objectName then
        begin
        result := anObject;
        exit;
        end;
      end;
  end;

function GsTemplateManager.findMatchForObject3d(original: KfObject3D): KfObject3d;
  var
  newObject3D: KfObject3d;
  i: longint;
  possibleMatch: KfObject3d;
  begin
  { see if already loaded - use latest names first}
  if tdoList.count > 0 then
    for i := tdoList.count - 1 downto 0 do
      begin
      possibleMatch := KfObject3d(tdoList.items[i]);
      if possibleMatch.name = original.name then
        begin
        if possibleMatch.isSameAs(original) then
          begin
        	result := possibleMatch;
        	exit;
          end;
        end;
      end;
  newObject3D := KfObject3d.create;
  original.copyTo(newObject3D);
  tdoList.add(newObject3D);
  result := newObject3D;
  end;

function GsTemplateManager.findMatchForObject3dByName(name: string): KfObject3d;
  var
    i: longint;
    possibleMatch: KfObject3d;
  begin
  result := nil;
  { see if already loaded - use latest names first}
  if tdoList.count > 0 then
    for i := tdoList.count - 1 downto 0 do
      begin
      possibleMatch := KfObject3d(tdoList.items[i]);
      if possibleMatch.name = name then
        begin
        result := possibleMatch;
        exit;
        end;
      end;
  end;

function GsTemplateManager.findMatchForIcon(original: GsIcon): GsIcon;
  var
    newIcon: GsIcon;
    i: longint;
    possibleMatch: GsIcon;
  begin
  { see if already loaded - use latest names first}
  if iconList.count > 0 then
    for i := iconList.count - 1 downto 0 do
      begin
      possibleMatch := GsIcon(iconList.items[i]);
      if possibleMatch.name = original.name then
        begin
        if possibleMatch.isSameAs(original) then
          begin
        	result := possibleMatch;
        	exit;
          end;
        end;
      end;
  newIcon := GsIcon.create;
  { CFK changed this because copyTo had problems in icon chooser; this is not used yet }
  {original.copyTo(newIcon);}
  newIcon.copyFrom(original);
  iconList.add(newIcon);
  result := newIcon;
  end;

function GsTemplateManager.findMatchForIconByName(name: string): GsIcon;
  var
    i: longint;
    possibleMatch: GsIcon;
  begin
  result := nil;
  { see if already loaded - use latest names first}
  if iconList.count > 0 then
    for i := iconList.count - 1 downto 0 do
      begin
      possibleMatch := GsIcon(iconList.items[i]);
      if possibleMatch.name = name then
        begin
        result := possibleMatch;
        exit;
        end;
      end;
  end;

function GsTemplateManager.findMatchForHarvestItemTemplate(original: GsHarvestItemTemplate): GsHarvestItemTemplate;
  var
  newHarvestItemTemplate: GsHarvestItemTemplate;
  i: longint;
  possibleMatch: GsHarvestItemTemplate;
  originalIcon: GsIcon;
  begin
  result := nil;
  if original = nil then exit;
  originalIcon := original.icon;
  { see if already loaded - use latest names first}
  if harvestItemTemplateList.count > 0 then
    for i := harvestItemTemplateList.count - 1 downto 0 do
      begin
      possibleMatch := GsHarvestItemTemplate(harvestItemTemplateList.items[i]);
      if possibleMatch.name = original.name then
        begin
        if possibleMatch.isSameAs(original) then
          begin
        	result := possibleMatch;
        	exit;
          end;
        end;
      end;
  newHarvestItemTemplate := GsHarvestItemTemplate.create;
  original.copyTo(newHarvestItemTemplate);

  {at this point, the icon in the harvest item template may be wrong, so fix it up}
  {this is because when the original wrote itself on the stream, it probably wrote a reference
  to an icon not in the template manager, which was reconstructed as a nil}
  if newHarvestItemTemplate.icon = nil then
    newHarvestItemTemplate.icon := self.findMatchForIcon(originalIcon);

  harvestItemTemplateList.add(newHarvestItemTemplate);
  result := newHarvestItemTemplate;
  end;

function GsTemplateManager.findHarvestItemTemplate(aName: string): GsHarvestItemTemplate;
  var
    i: longint;
    theTemplate: GsHarvestItemTemplate;
  begin
  result := nil;
  if harvestItemTemplateList.count > 0 then
    for i := 0 to harvestItemTemplateList.count - 1 do
      begin
      theTemplate := GsHarvestItemTemplate(harvestItemTemplateList.items[i]);
      if theTemplate.name = aName then
        begin;
        result := theTemplate;
        exit;
        end;
      end;
  {$IFDEF ALLOW_HARDCODING}
  result := createHardCodedHarvestItemTemplate(aName, self);
  {$ENDIF}
  end;                                               

{ --------------------------------------------------------------------------------------------- streaming }
procedure GsTemplateManager.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsTemplateManager;
  cvir.versionNumber := 4;
  cvir.additionNumber := 0;
  end;

procedure GsTemplateManager.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  {first set up self as manager of shared resources - icons and  tdos}
  filer.resourceProvider := self;
  {stream out icons and tdos first so available to rest when streaming in}
  iconList.streamUsingFiler(filer, GsIcon);
  tdoList.streamUsingFiler(filer, KfObject3D);
  {haverst items should be streamed before cultivars}
  harvestItemTemplateList.streamUsingFiler(filer, GsHarvestItemTemplate);
  if streamOnlyResources then exit;
  climateList.streamUsingFiler(filer, GsWeather);
  bagList.streamUsingFiler(filer, GsBag);
  soilTypeList.streamUsingFiler(filer, GsSoilPatch);
  cultivarList.streamUsingFiler(filer, GsPlant);
  filer.streamLongint(defaultSoilTypeIndex);
  filer.streamLongint(defaultClimateIndex);
  end;

{these can fail if copying object from library template manager to current domain one}
{PDF FIX - could make more write lookup more efficient by storing index in object}
procedure GsTemplateManager.streamReferenceToObject3d(filer: GsFiler; var object3D: KfObject3d);
  var reference: longint;
  begin
  if filer.isReading then
    begin
    filer.streamLongint(reference);
    if reference = -1 then
    	object3d := nil
    else
      begin
      if reference < self.tdoList.count then
    		object3d := KfObject3d(self.tdoList.items[reference])
      else
        begin
        object3d := nil;
        {raise Exception.create('TDO ref outside range of template manager');}
        end;
      end;
    end
  else if filer.isWriting then
    begin
    if object3D = nil then
    	reference := -1
    else
      begin
      reference := self.tdoList.IndexOf(object3d);
      {if reference = -1 then
        raise Exception.create('TDO not found in template manager'); }
      end;
    filer.streamLongint(reference);
    end;
  end;

procedure GsTemplateManager.streamReferenceToIcon(filer: GsFiler; var icon: GsIcon);
  var reference: longint;
  begin
  if filer.isReading then
    begin
    filer.streamLongint(reference);
    if reference = -1 then
    	icon := nil
    else
      begin
      if reference < self.iconList.count then
    		icon := GsIcon(self.iconList.items[reference])
      else
        begin
        icon := nil;
        {raise Exception.create('Icon ref outside range of template manager');}
        end;
      end;
    end
  else if filer.isWriting then
    begin
    if icon = nil then
    	reference := -1
    else
      begin
      reference := self.iconList.IndexOf(icon);
      {if reference = -1 then
        raise Exception.create('Icon not found in template manager');}
      end;
    filer.streamLongint(reference);
    end;
  end;

procedure GsTemplateManager.streamReferenceToHarvestItemTemplate(filer: GsFiler;
  var harvestItemTemplate: GsHarvestItemTemplate);
  var reference: longint;
  begin
  if filer.isReading then
    begin
    filer.streamLongint(reference);
    if reference = -1 then
    	harvestItemTemplate := nil
    else
      begin
      if reference < self.harvestItemTemplateList.count then
    		harvestItemTemplate := GsHarvestItemTemplate(self.harvestItemTemplateList.items[reference])
      else
        begin
        harvestItemTemplate := nil;
        {raise Exception.create('harvestItemTemplate ref outside range of template manager');}
        end;
      end;
    end
  else if filer.isWriting then
    begin
    if harvestItemTemplate = nil then
    	reference := -1
    else
      begin
      reference := self.harvestItemTemplateList.IndexOf(harvestItemTemplate);
      {if reference = -1 then
        raise Exception.create('harvestItemTemplate not found in template manager');}
      end;
    filer.streamLongint(reference);
    end;
  end;

function GsTemplateManager.loadObject3D(fileName: string): KfObject3d;
  var
  newObject3D: KfObject3d;
  i: longint;
  begin
  { see if already loaded - use latest names first}
  if tdoList.count > 0 then
    for i := tdoList.count - 1 downto 0 do
      begin
      if KfObject3d(tdoList.items[i]).name = fileName then
        begin
        result := KfObject3d(tdoList.items[i]);
        exit;
        end;
      end;
  newObject3D := KfObject3d.fromFile(fileName);
  newObject3D.name := fileName;
  tdoList.add(newObject3D);
  result := newObject3D;
  end;

function GsTemplateManager.loadGsIcon(fileName: string): GsIcon;
  var
  newIcon: GsIcon;
  i: longint;
  begin
  { see if already loaded - use latest names first}
  if iconList.count > 0 then
    for i := iconList.count - 1 downto 0 do
      begin
      if GsIcon(iconList.items[i]).name = fileName then
        begin
        result := GsIcon(iconList.items[i]);
        exit;
        end;
      end;
  newIcon := GsIcon.create;       
  newIcon.icon.loadFromFile(fileName);
  newIcon.setName(extractFileName(fileName));
  iconList.add(newIcon);
  result := newIcon;
  end;            

procedure GsTemplateManager.fixupCopiedCultivar(newCultivar: GsPlant; original: GsPlant);
  var i: smallint;
	begin
  {clear out incorrect information}
  with newCultivar.drawingPlant do
    begin
    pSeedlingLeaf.object3D := nil;
 		pLeaf.object3D := nil;
  	pFlower[kGenderFemale].object3D := nil;
  	pFlower[kGenderMale].object3D := nil;
  	pAxillaryBud.object3D := nil;
  	pFruit.object3D := nil;
  	pRoot.object3D := nil;
    for i := 0 to kHarvestPartTypeLast do
      pGeneral.harvestItemTemplatesArray[i] := nil;
    end;
  with original.drawingPlant do
    begin
    if pSeedlingLeaf.object3D <> nil then
      newCultivar.drawingPlant.pSeedlingLeaf.object3D := self.findMatchForObject3D(pSeedlingLeaf.object3D);
 		if pLeaf.object3D <> nil then
    	newCultivar.drawingPlant.pLeaf.object3D := self.findMatchForObject3D(pLeaf.object3D);
  	if pFlower[kGenderFemale].object3D <> nil then
    	newCultivar.drawingPlant.pFlower[kGenderFemale].object3D := self.findMatchForObject3D(pFlower[kGenderFemale].object3D);
  	if pFlower[kGenderFemale].budObject3D <> nil then
    	newCultivar.drawingPlant.pFlower[kGenderFemale].budObject3D :=
      	self.findMatchForObject3D(pFlower[kGenderFemale].budObject3D);
  	if pFlower[kGenderMale].object3D <> nil then
    	newCultivar.drawingPlant.pFlower[kGenderMale].object3D := self.findMatchForObject3D(pFlower[kGenderMale].object3D);
  	if pFlower[kGenderMale].budObject3D <> nil then
    	newCultivar.drawingPlant.pFlower[kGenderMale].budObject3D :=
      	self.findMatchForObject3D(pFlower[kGenderMale].budObject3D);
  	if pAxillaryBud.object3D <> nil then
    	newCultivar.drawingPlant.pAxillaryBud.object3D := self.findMatchForObject3D(pAxillaryBud.object3D);
  	if pFruit.object3D <> nil then
    	newCultivar.drawingPlant.pFruit.object3D := self.findMatchForObject3D(pFruit.object3D);
  	if pRoot.object3D <> nil then
    	newCultivar.drawingPlant.pRoot.object3D := self.findMatchForObject3D(pRoot.object3D);
    for i := 0 to kHarvestPartTypeLast do
       newCultivar.drawingPlant.pGeneral.harvestItemTemplatesArray[i] :=
         self.findMatchForHarvestItemTemplate(pGeneral.harvestItemTemplatesArray[i]);
    end;
  end;

procedure GsTemplateManager.fixupPlantResources(plant: GsPlant);
  var
  	i: smallint;
	begin
  with plant.drawingPlant do
    begin
    if pSeedlingLeaf.object3D <> nil then
      pSeedlingLeaf.object3D := self.findMatchForObject3D(pSeedlingLeaf.object3D);
 		if pLeaf.object3D <> nil then
    	pLeaf.object3D := self.findMatchForObject3D(pLeaf.object3D);
  	if pFlower[kGenderFemale].object3D <> nil then
    	pFlower[kGenderFemale].object3D := self.findMatchForObject3D(pFlower[kGenderFemale].object3D);
  	if pFlower[kGenderFemale].budObject3D <> nil then
    	pFlower[kGenderFemale].budObject3D := self.findMatchForObject3D(pFlower[kGenderFemale].budObject3D);
  	if pFlower[kGenderMale].object3D <> nil then
    	pFlower[kGenderMale].object3D := self.findMatchForObject3D(pFlower[kGenderMale].object3D);
  	if pFlower[kGenderMale].budObject3D <> nil then
    	pFlower[kGenderMale].budObject3D := self.findMatchForObject3D(pFlower[kGenderMale].budObject3D);
  	if pAxillaryBud.object3D <> nil then
    	pAxillaryBud.object3D := self.findMatchForObject3D(pAxillaryBud.object3D);
  	if pFruit.object3D <> nil then
    	pFruit.object3D := self.findMatchForObject3D(pFruit.object3D);
  	if pRoot.object3D <> nil then
    	pRoot.object3D := self.findMatchForObject3D(pRoot.object3D);
    for i := 0 to kHarvestPartTypeLast do
       pGeneral.harvestItemTemplatesArray[i] :=
         self.findMatchForHarvestItemTemplate(pGeneral.harvestItemTemplatesArray[i]);
    end;
  end;

procedure GsTemplateManager.fixupHarvestItemResources(harvestItem: GsHarvestItem);
	begin
  harvestItem.harvestItemTemplate := self.findMatchForHarvestItemTemplate(harvestItem.harvestItemTemplate);
  end;

{ --------------------------------------------------------------------------------------------- object creation }
function GsTemplateManager.copyFromWeather(weather: GsWeather; isTemplate: boolean): GsWeather;
  var
    newClimate: GsWeather;
	begin
  newClimate := GsWeather.create;
  newClimate.garden := weather.garden;
  newClimate.becomeTemplateBasedOn(weather);
  climateList.add(newClimate);
  result := newClimate;
	end;

function GsTemplateManager.copyFromPlant(plant: GsPlant; isTemplate: boolean): GsPlant;
  var
    newCultivar: GsPlant;
	begin
  newCultivar := GsPlant.create;
  newCultivar.soil := plant.soil; {something for now - may be needed when becomes template}
  newCultivar.becomeTemplateBasedOn(plant);
  cultivarList.add(newCultivar);
  result := newCultivar;
	end;

function GsTemplateManager.copyFromSoilPatch(soilPatch: GsSoilPatch; isTemplate: boolean): GsSoilPatch;
  var
    newSoilType: GsSoilPatch;
	begin
  newSoilType := GsSoilPatch.create;
  newSoilType.garden := soilPatch.garden;
  newSoilType.becomeTemplateBasedOn(soilPatch);
  soilTypeList.add(newSoilType);
  result := newSoilType;
	end;

function GsTemplateManager.copyFromBag(bag: GsBag): GsBag;
  var
    newBag: GsBag;
	begin
  newBag := GsBag.create;
  newBag.becomeTemplateBasedOn(bag);
  bagList.add(newBag);
  result := newBag;
	end;

function GsTemplateManager.createNewHarvestItemTemplate(aName, iconName: string): GsHarvestItemTemplate;
  begin
  result := GsHarvestItemTemplate.create;
  result.icon := self.loadGsIcon(iconName);
  result.setName(aName);
  self.harvestItemTemplateList.add(result);
  end;

{ --------------------------------------------------------------------------------------------- garbage collection }
{Garbage collection is inefficient - but probably fast enough for small lists}
{Could improve speed by setting flags in objects (icons, tdos, harvest items)}
procedure GsTemplateManager.garbageCollect(includeGarden, showMessages: boolean);
  begin
  {do harvest item tempaltes first because they reference icons}
  self.garbageCollectHarvestItemTemplateList(includeGarden, showMessages);
  self.garbageCollectIconList(includeGarden, showMessages);
  self.garbageCollectTdoList(includeGarden, showMessages);
  end;

procedure setHarvestItemTemplateInUseFlagsForPlant(aPlant: GsPlant);
  var i: smallint;
  begin
  if aPlant = nil then exit;
  if aPlant.drawingPlant = nil then exit;
  for i := 0 to kHarvestPartTypeLast do
    if aPlant.drawingPlant.pGeneral.harvestItemTemplatesArray[i] <> nil then
      aPlant.drawingPlant.pGeneral.harvestItemTemplatesArray[i].inUse := true;
  end;

procedure GsTemplateManager.setAllHarvestItemTemplateInUseFlagsToFalse;
  var i: longint;
  begin
  if harvestItemTemplateList.count <= 0 then exit;
  for i := 0 to harvestItemTemplateList.count - 1 do
    GsHarvestItemTemplate(harvestItemTemplateList.items[i]).inUse := false;
  end;

procedure GsTemplateManager.setInUseFlagsForHarvestItemTemplateList(includeGarden: boolean);
  var
    i, j: longint;
    plant: GsPlant;
    soilPatch: GsSoilPatch;
    harvestItemTemplate: GsHarvestItemTemplate;
    harvestItem: GsHarvestItem;
    command: KfCommand;
    otherPlants: TList;
  begin
  if harvestItemTemplateList.count <= 0 then exit;
  { first set all flags to false }
  self.setAllHarvestItemTemplateInUseFlagsToFalse;
  { set flag to true for all HITs pointed to by cultivars, plants, or undo commands }
  if cultivarList.count > 0 then
    for i := 0 to cultivarList.count - 1 do
      setHarvestItemTemplateInUseFlagsForPlant(GsPlant(cultivarList.items[i]));
  if includeGarden then
    begin
    otherPlants := TList.create;
    try
    if Domain.garden.soilPatchList.count > 0 then
      for i := 0 to Domain.garden.soilPatchList.count - 1 do
        begin
        soilPatch := GsSoilPatch(Domain.garden.soilPatchList.items[i]);
        if soilPatch.plantList.count > 0 then
          for j := 0 to soilPatch.plantList.count - 1 do
            setHarvestItemTemplateInUseFlagsForPlant(GsPlant(soilPatch.plantList.items[j]));
        end;
    if gardenForm.commandList.commandList.count > 0 then
      for i := 0 to gardenForm.commandList.commandList.count - 1 do
        begin
        command := KfCommand(gardenForm.commandList.commandList.items[i]);
        command.addPlantsInUseNotInGarden(otherPlants);
        if command is GsChangeDomainHarvestItemTemplateCommand then
          begin
          if GsChangeDomainHarvestItemTemplateCommand(command).newHarvestItemTemplate <> nil then
            GsChangeDomainHarvestItemTemplateCommand(command).newHarvestItemTemplate.inUse := true;
          if GsChangeDomainHarvestItemTemplateCommand(command).oldHarvestItemTemplate <> nil then
            GsChangeDomainHarvestItemTemplateCommand(command).oldHarvestItemTemplate.inUse := true;
          end;
        end;
    if Domain.harvestManager.harvestItemList.count > 0 then
      for i := 0 to Domain.harvestManager.harvestItemList.count - 1 do
        begin
        harvestItem := GsHarvestItem(Domain.harvestManager.harvestItemList.items[i]);
        if (harvestItem <> nil) and (harvestItem.harvestItemTemplate <> nil) then
          harvestItem.harvestItemTemplate.inUse := true;
        end;
    {now handle possibly deleted plants to ensure their harvest item templates are marked as in use}
    if otherPlants.count > 0 then
      for i := 0 to otherPlants.count - 1 do
        setHarvestItemTemplateInUseFlagsForPlant(GsPlant(otherPlants[i]));
    finally
    otherPlants.free;
    end;
    end;
  end;

{remove unreferenced harvest item templates}
procedure GsTemplateManager.garbageCollectHarvestItemTemplateList(includeGarden, showMessages: boolean);
  var
    i: longint;
    harvestItemTemplate: GsHarvestItemTemplate;
    harvestItemTemplatesDeletedCount: longint;
  begin
  self.setInUseFlagsForHarvestItemTemplateList(includeGarden);
  harvestItemTemplatesDeletedCount := 0;
  if self.harvestItemTemplateList.count > 0 then
    for i := 0 to harvestItemTemplateList.count - 1 do
      begin
      harvestItemTemplate := GsHarvestItemTemplate(harvestItemTemplateList.items[i]);
      if not harvestItemTemplate.inUse then
        begin
        harvestItemTemplate.free;
        harvestItemTemplateList.items[i] := nil;
        inc(harvestItemTemplatesDeletedCount);
        end;
      end;
  {removes nil objects in iconList}
  harvestItemTemplateList.pack;
  if showMessages then
    ShowMessage(IntToStr(harvestItemTemplatesDeletedCount) + ' unused harvest item(s) were deleted.');
  end;

procedure GsTemplateManager.setAllIconInUseFlagsToFalse;
  var i: longint;
  begin
  if iconList.count <= 0 then exit;
  for i := 0 to iconList.count - 1 do
    GsIcon(iconList.items[i]).inUse := false;
  end;

procedure setInUseFlagsForIconsForPlant(plant: GsPlant);
  var
  	i: longint;
    harvestItemTemplate: GsHarvestItemTemplate;
  begin
  for i := 0 to kHarvestPartTypeLast do
    begin
    harvestItemTemplate := plant.drawingPlant.pGeneral.harvestItemTemplatesArray[i];
    if harvestItemTemplate <> nil then
      harvestItemTemplate.icon.inUse := true;
    end;
  end;

{icons are in harvest templates or commands to change icon}
procedure GsTemplateManager.setInUseFlagsForIconList(includeGarden: boolean);
  var
    i: longint;
    icon: GsIcon;
    command: KfCommand;
    harvestItemTemplate: GsHarvestItemTemplate;
    otherPlants: TList;
  begin
  self.setAllIconInUseFlagsToFalse;
  if harvestItemTemplateList.count > 0 then
    for i := 0 to harvestItemTemplateList.count - 1 do
      begin
      harvestItemTemplate := GsHarvestItemTemplate(harvestItemTemplateList.items[i]);
      if harvestItemTemplate.icon <> nil then harvestItemTemplate.icon.inUse := true;
      end;
  if includeGarden then
    begin
    otherPlants := TList.create;
    try
    {need to keep undone icons while still in command list}
    if gardenForm.commandList.commandList.count > 0 then
      for i := 0 to gardenForm.commandList.commandList.count - 1 do
        begin
        command := KfCommand(gardenForm.commandList.commandList.items[i]);
        command.addPlantsInUseNotInGarden(otherPlants);
        if command is GsChangeDomainHarvestItemTemplateCommand then
          begin
          icon := GsChangeDomainHarvestItemTemplateCommand(command).newHarvestItemTemplate.icon;
          if icon <> nil then icon.inUse := true;
          icon := GsChangeDomainHarvestItemTemplateCommand(command).oldHarvestItemTemplate.icon;
          if icon <> nil then icon.inUse := true;
          end;
        end;
  	{now check harvest manager for any in use}
    if Domain.harvestManager.harvestItemList.count > 0 then
      for i := 0 to Domain.harvestManager.harvestItemList.count - 1 do
        begin
        harvestItemTemplate := GsHarvestItem(Domain.harvestManager.harvestItemList.items[i]).harvestItemTemplate;
        if (harvestItemTemplate <> nil) and (harvestItemTemplate.icon <> nil) then
          harvestItemTemplate.icon.inUse := true;
        end;
    {now handle possibly deleted plants to ensure their icons are marked as in use}
    if otherPlants.count > 0 then
      for i := 0 to otherPlants.count - 1 do
        setInUseFlagsForIconsForPlant(otherPlants[i]);
    finally
    otherPlants.free;
    end;
    end;
  end;

{remove unreferenced icons}
procedure GsTemplateManager.garbageCollectIconList(includeGarden, showMessages: boolean);
  var
    i: longint;
    icon: GsIcon;
    iconsDeletedCount: longint;
  begin
  self.setInUseFlagsForIconList(includeGarden);
  iconsDeletedCount := 0;
  if self.iconList.count > 0 then
    for i := 0 to iconList.count - 1 do
      begin
      icon := GsIcon(iconList.items[i]) ;
      if not icon.inUse then
        begin
        icon.free;
        iconList.items[i] := nil;
        inc(iconsDeletedCount);
        end;
      end;
  {removes nil objects in iconList}
  iconList.pack;
  if showMessages then
    ShowMessage(IntToStr(iconsDeletedCount) + ' unused icon(s) were deleted.');
  end;

procedure GsTemplateManager.setAllTdoInUseFlagsToFalse;
  var i: longint;
  begin
  if tdoList.count <= 0 then exit;
  for i := 0 to tdoList.count - 1 do
  	KfObject3d(tdoList.items[i]).inUse := false;
  end;

procedure GsTemplateManager.setInUseFlagsForTdoList(includeGarden: boolean);
  var
    i, j: longint;
    plant: GsPlant;
    soilPatch: GsSoilPatch;
    tdo: KfObject3d;
    command: KfCommand;
    otherPlants: TList;
  begin
  if tdoList.count <= 0 then exit;
  { first set all flags to false }
  self.setAllTdoInUseFlagsToFalse;
  { set flag to true for all tdos pointed to by cultivars, plants, or undo commands }
  if cultivarList.count > 0 then
    for i := 0 to cultivarList.count - 1 do
      begin
      plant := GsPlant(cultivarList.items[i]);
      if (plant <> nil) and (plant.drawingPlant <> nil) then
        plant.drawingPlant.setTdoInUseFlags;
      end;
  if includeGarden then
    begin
    otherPlants := TList.create;
    try
    if Domain.garden.soilPatchList.count > 0 then
      for i := 0 to Domain.garden.soilPatchList.count - 1 do
        begin
        soilPatch := GsSoilPatch(Domain.garden.soilPatchList.items[i]);
        if soilPatch.plantList.count > 0 then
          for j := 0 to soilPatch.plantList.count - 1 do
            begin
            plant := GsPlant(soilPatch.plantList.items[j]);
            if (plant <> nil) and (plant.drawingPlant <> nil) then
              plant.drawingPlant.setTdoInUseFlags;
            end;
        end;
    if gardenForm.commandList.commandList.count > 0 then
      for i := 0 to gardenForm.commandList.commandList.count - 1 do
        begin
        command := KfCommand(gardenForm.commandList.commandList.items[i]);
        command.addPlantsInUseNotInGarden(otherPlants);
        if command is GsChangeDomainObject3DCommand then
          begin
          if GsChangeDomainObject3DCommand(command).newTdo <> nil then
            GsChangeDomainObject3DCommand(command).newTdo.inUse := true;
          if GsChangeDomainObject3DCommand(command).oldTdo <> nil then
            GsChangeDomainObject3DCommand(command).oldTdo.inUse := true;
          end;
        end;
    {now handle possibly deleted plants to ensure their tdos are marked as in use}
    if otherPlants.count > 0 then
      for i := 0 to otherPlants.count - 1 do
        GsPlant(otherPlants[i]).drawingPlant.setTdoInUseFlags;
    finally
    otherPlants.free;
    end;
    end;
  end;

{remove unreferenced tdos}
procedure GsTemplateManager.garbageCollectTdoList(includeGarden, showMessages: boolean);
  var
    i: longint;
    tdo: KfObject3D;
    tdosDeletedCount: longint;
  begin
  self.setInUseFlagsForTdoList(includeGarden);
  tdosDeletedCount := 0;
  if self.tdoList.count > 0 then
    for i := 0 to tdoList.count - 1 do
      begin
      tdo := KfObject3D(tdoList.items[i]) ;
      if not tdo.inUse then
        begin
        tdo.free;
        tdoList.items[i] := nil;
        inc(tdosDeletedCount);
        end;
      end;
  {removes nil objects in iconList}
  tdoList.pack;
  if showMessages then
    ShowMessage(IntToStr(tdosDeletedCount) + ' unused tdo(s) were deleted.');
  end;

{return true/false if removed something - called should then delete}
function GsTemplateManager.removeTemplate(model: GsModel): boolean;
  begin
  result := false;
  if bagList.remove(model) < 0 then
    if cultivarList.remove(model) < 0 then
      if soilTypeList.remove(model) < 0 then
        if climateList.remove(model) < 0 then
          exit;
  result := true;
  end;

{ ----------------------------------------------------------------------------------------- general list maintenance }
procedure GsTemplateManager.removeAllItems;
  begin
  { bags }
  bagList.free;
  bagList := nil;
  bagList := TListCollection.create;
  { cultivars }
  cultivarList.free;
  cultivarList := nil;
  cultivarList := TListCollection.create;
  { soil types }
  soilTypeList.free;
  soilTypeList := nil;
  soilTypeList := TListCollection.create;
  { climates }
  climateList.free;
  climateList := nil;
  climateList := TListCollection.create;
  { tdos }
  tdoList.free;
  tdoList := nil;
  tdoList := TListCollection.create;
  { icons }
  iconList.free;
  iconList := nil;
  iconList := TListCollection.create;
  { harvest item templates }
  harvestItemTemplateList.free;
  harvestItemTemplateList := nil;
  harvestItemTemplateList := TListCollection.create;
  defaultSoilTypeIndex := 0;
  defaultClimateIndex := 0;
  end;

function GsTemplateManager.defaultClimate: GsWeather;
  begin
  if (defaultClimateIndex < 0) or (defaultClimateIndex >= climateList.count) then
    begin
    result := nil;
  {  raise Exception.create('Invalid default climate index'); }
    end
  else
    result := climateList.items[defaultClimateIndex];
  end;

function GsTemplateManager.defaultSoilType: GsSoilPatch;
  begin
  if (defaultSoilTypeIndex < 0) or (defaultSoilTypeIndex >= soilTypeList.count) then
    begin
    result := nil;
  {  raise Exception.create('Invalid default soil type index');  }
    end
  else
    result := soilTypeList.items[defaultSoilTypeIndex];
  end;

procedure GsTemplateManager.fixUpDefaultIndexes;
  begin
  { this is to reset the defaults to the zero index if a new set of templates has been imported
    (and the others lost) and the default indexes no longer point to reasonable items. this makes it
    so the user doesn't have to re-choose a default soil type before making a patch }
  if (defaultSoilTypeIndex < 0) or (defaultSoilTypeIndex >= soilTypeList.count) then
    defaultSoilTypeIndex := 0;
  if (defaultClimateIndex < 0) or (defaultClimateIndex >= climateList.count) then
    defaultClimateIndex := 0;
  end;

procedure GsTemplateManager.initializeHardCodedTemplates;
  var
    bag: GsBag;
    climate: GsWeather;
    soilType: GsSoilPatch;
    cultivar: GsPlant;
  begin
  { bags }
  bag := GsBag.create; InitAsBag_Sand(bag); bag.isTemplate := true; bagList.add(bag);
  bag := GsBag.create; InitAsBag_Lime(bag); bag.isTemplate := true; bagList.add(bag);
  bag := GsBag.create; InitAsBag_Compost(bag); bag.isTemplate := true; bagList.add(bag);
  bag := GsBag.create; InitAsBag_CowManureFresh(bag); bag.isTemplate := true; bagList.add(bag);
  bag := GsBag.create; InitAsBag_WoodAshes(bag); bag.isTemplate := true; bagList.add(bag);
  bag := GsBag.create; InitAsBag_AmmoniumSulfate(bag); bag.isTemplate := true; bagList.add(bag);
  bag := GsBag.create; InitAsBag_Straw(bag); bag.isTemplate := true; bagList.add(bag);
  { climates }
  climate := GsWeather.create; InitAsClimate_Pittsburgh_PA(climate); climate.isTemplate := true; climateList.add(climate);
  climate := GsWeather.create; InitAsClimate_Newark_NJ(climate); climate.isTemplate := true; climateList.add(climate);
  climate := GsWeather.create; InitAsClimate_LosAngeles_CA(climate); climate.isTemplate := true; climateList.add(climate);
  { soil types }
  soilType := GsSoilPatch.createWithGardenAndPoint(Domain.garden, point(0,0));
  InitAsSoilType_Keyport_NJ(soilType); soilType.isTemplate := true; soilTypeList.add(soilType);
  soilType := GsSoilPatch.createWithGardenAndPoint(Domain.garden, point(0,0));
  InitAsSoilType_HagerstownB_MD(soilType); soilType.isTemplate := true; soilTypeList.add(soilType);
  soilType := GsSoilPatch.createWithGardenAndPoint(Domain.garden, point(0,0));
  InitAsSoilType_Indianola_IA(soilType); soilType.isTemplate := true; soilTypeList.add(soilType);
  { cultivars }
  cultivar := GsPlant.create; InitAsCultivar_Tomato(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_EarlyPea(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Corn(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Carrot(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Sunflower(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_GreenBean(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Pepper(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Onion(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Lettuce(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  cultivar := GsPlant.create; InitAsCultivar_Spinach(cultivar); cultivar.isTemplate := true; cultivarList.add(cultivar);
  end;

{ --------------------------------------------------------------------------------------------- tabbed text file i/o }
procedure GsTemplateManager.saveAllTemplatesToFileName(fileName: string);
  var
    textFiler: GsTextFiler;
  begin
  textFiler := GsTextFiler.createWithFileNameForWriting(fileName);
  try
    { save icons before harvest items }
    self.saveListOfTemplatesWithTextFiler(textFiler, iconList, kObjectTypeIcon);
    { save harvest items and tdos before cultivars }
    self.saveListOfTemplatesWithTextFiler(textFiler, harvestItemTemplateList, kObjectTypeHarvestItemTemplate);
    self.saveListOfTemplatesWithTextFiler(textFiler, tdoList, kObjectType3DObject);
    self.saveListOfTemplatesWithTextFiler(textFiler, cultivarList, kObjectTypePlant);
    self.saveListOfTemplatesWithTextFiler(textFiler, soilTypeList, kObjectTypeSoil);
    self.saveListOfTemplatesWithTextFiler(textFiler, climateList, kObjectTypeWeather);
    self.saveListOfTemplatesWithTextFiler(textFiler, bagList, kObjectTypeBag);
  finally
    textFiler.free;
  end;
  end;

procedure GsTemplateManager.saveListOfTemplatesWithTextFiler(textFiler: GsTextFiler;
    aList: TListCollection; objectType: smallint);
  var
    anObject: GsStreamableObject;
    i: longint;
    aString: string;
  begin
  if aList.count > 0 then
    for i := 0 to aList.count - 1 do
      begin
      anObject := GsStreamableObject(aList.items[i]);
      if anObject = nil then continue;
      if i = 0 then
        begin
        textFiler.writingHeader := true;
        textFiler.writeLiteralString('objectType');
        textFiler.writeLiteralString('version');
        textFiler.writeLiteralString('name');
        anObject.streamUsingTextFiler(textFiler);
        textFiler.writingHeader := false;
        end;
      textFiler.writeLiteralString(textFiler.objectStringForType(objectType)); {object type}
      textFiler.writeLiteralString(textFiler.versionString);  {version}
      textFiler.writeLiteralString(anObject.getName); {name}
      anObject.streamUsingTextFiler(textFiler);
      end;
  end;

procedure GsTemplateManager.readAllTemplatesFromFileName(fileName: string);
  var
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
    textFiler: GsTextFiler;
    objectString, versionString, objectName, skipObjectString: string;
    objectType: smallint;
    existingObject: GsStreamableObject;
    whatToDo: smallint;
    goAhead: boolean;
    response: word;
  begin
  skipObjectString := '12345';
  textFiler := GsTextFiler.createWithFileNameForReading(fileName);
  try
  textFiler.resourceProvider := self;
  while not textFiler.atEndOfFile do
    begin
    { read first two columns for header or object type and version }
    textFiler.streamString(objectString, '');
    if (objectString = 'objectType') or (objectString = skipObjectString) then
      begin
      textFiler.skipRestOfLine;
      continue;
      end
    else
      skipObjectString := '12345';
    textFiler.streamString(versionString, '');
    if not textFiler.canStreamVersion(versionString) then
      begin
      response := messageDlg('This ' + objectString + ' cannot be read because it is version '
           + versionString + '.' + chr(13) +
          'Do you want to read the rest of the items?', mtError, [mbYes, mbNo], 0);
      case response of
        mrYes:
          begin
          textFiler.skipRestOfLine;
          continue;
          end;
        mrNo:
          break;
        end;
      end;
    textFiler.streamString(objectName, '');
    objectType := textFiler.objectTypeForString(objectString);
    {look for object in list of that type with same name}
    existingObject := self.findObjectInListBasedOnName(objectType, objectName);
    if existingObject = nil then
      begin
      self.createNewObjectWithTextFiler(objectType, textFiler, objectName);
      continue;
      end;
    {duplicate}
    case textFiler.whatToDoWithDupicates of
      kOverwriteAllDuplicates:
        begin
        case objectType of
          kObjectTypeHarvestItemTemplate, kObjectType3DObject, kObjectTypeIcon:
            whatToDo := textFiler.askWhatToDoWithDuplicates(objectType, objectName);
          else
            whatToDo := kOverwriteThisExistingObject;
          end;
        end;
      kSkipAllDuplicates: whatToDo := kSkipThisNewObject;
      kAskOnEachDuplicate: whatToDo := textFiler.askWhatToDoWithDuplicates(objectType, objectName);
      else whatToDo := textFiler.askWhatToDoWithDuplicates(objectType, objectName);
      end;
    case whatToDo of
      kOverwriteThisExistingObject:
        begin
        case objectType of
          kObjectTypeSoil: soilTypeList.remove(existingObject);
          kObjectTypePlant: cultivarList.remove(existingObject);
          kObjectTypeWeather: climateList.remove(existingObject);
          kObjectTypeBag: bagList.remove(existingObject);
          else
            raise Exception.create('readAllTemplatesFromFileName: should not be deleting object of this type');
          end;
        existingObject.free;
        existingObject := nil;
        self.createNewObjectWithTextFiler(objectType, textFiler, objectName);
        end;
      kSkipThisNewObject:
        begin
        textFiler.skipRestOfLine;
        continue;
        end;
      kRenameThisNewObject:
        begin
        newName := objectName;
        if not inputQuery('Enter new name', 'Enter a new unique name for this ' + objectString + '.', newName) then
          continue;
        goAhead := true;
        while self.findObjectInListBasedOnName(objectType, newName) <> nil do
          if not inputQuery('Enter new name', 'That name is taken. Enter a unique name.', newName) then
            begin
            goAhead := false;
            break;
            end;
        if goAhead then
          self.createNewObjectWithTextFiler(objectType, textFiler, newName);
        end;
      kSkipRemainingObjectsOfThisType:
        begin
        textFiler.skipRestOfLine;
        skipObjectString := objectString;
        continue;
        end;
      kStopImporting:
        break;
      end;
    end;
  finally
    textFiler.free;
  end;
  end;

procedure GsTemplateManager.createNewObjectWithTextFiler(objectType: smallint; textFiler: GsTextFiler; objectName: string);
  var
    soilType: GsSoilPatch;
    cultivar: GsPlant;
    climate: GsWeather;
    bag: GsBag;
    harvestItemTemplate: GsHarvestItemTemplate;
    tdo: KfObject3d;
    icon: GsIcon;
  begin
  case objectType of
    kObjectTypeSoil:
      begin
      soilType := GsSoilPatch.create;
      soilType.setName(objectName);
      soilType.streamUsingTextFiler(textFiler);
      soilType.isTemplate := true;
      soilTypeList.add(soilType);
      end;
    kObjectTypePlant:
      begin
      cultivar := GsPlant.create;
      cultivar.setName(objectName);
      cultivar.streamUsingTextFiler(textFiler);
      cultivar.isTemplate := true;
      cultivarList.add(cultivar);
      end;
    kObjectTypeWeather:
      begin
      climate := GsWeather.create;
      climate.setName(objectName);
      climate.streamUsingTextFiler(textFiler);
      climate.isTemplate := true;
      climateList.add(climate);
      end;
    kObjectTypeBag:
      begin
      bag := GsBag.create;
      bag.setName(objectName);
      bag.streamUsingTextFiler(textFiler);
      bag.isTemplate := true;
      bagList.add(bag);
      end;
    kObjectTypeHarvestItemTemplate:
      begin
      harvestItemTemplate := GsHarvestItemTemplate.create;
      harvestItemTemplate.setName(objectName);
      harvestItemTemplate.streamUsingTextFiler(textFiler);
      harvestItemTemplateList.add(harvestItemTemplate);
      end;
    kObjectType3DObject:
      begin
      tdo := KfObject3d.create;
      tdo.setName(objectName);
      tdo.streamUsingTextFiler(textFiler);
      tdoList.add(tdo);
      end;
    kObjectTypeIcon:
      begin
      icon := GsIcon.create;
      icon.setName(objectName);
      icon.streamUsingTextFiler(textFiler);
      iconList.add(icon);
      end;
    else
      raise Exception.create('GsTextFiler.createNewObjectWithTextFiler: Unsupported object type');
    end;
  end;

end.

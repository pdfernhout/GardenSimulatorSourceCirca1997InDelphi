unit uegarden;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uegarden: Barden object. The garden object has very little to do as most of the parameters
have been moved into the soil patch. In EPIC, the garden and soil patch are the same thing
since the simulation models only one field. In GWI, the garden is a container for any number
of soil patches. At this point, the garden is just a container. With improvement the garden
could manage communication between soil patches and model the surrounding areas to more
completely simulate water flow between areas. Simulation options are stored here -- both
whether each option is to be overridden from individual choices, and what the overridden
option is set to. The garden also handles a few garden-wide tasks such as drawing and
reseeding all plants.
All model code is based in part on EPIC3090 in FORTRAN by J.R. Williams et. al., USDA ARS. }

interface

uses ExtCtrls, WinTypes, ufiler, ucollect, umodel, uestruct, ueweath, classes, Graphics,
  StdCtrls, udate;

const
  kRedrawNone = 0; kRedrawSoil = 1; kRedrawGarden = 2;
  kGardenScaleIgnore = 0; kGardenScaleOverridesPatchScale = 1; kGardenScaleMultipliesPatchScale = 2;

type
  weatherOverridesArray = array[0..kWeatherOptionsLastOption + 4] of boolean; {4 extra for future use}

GsGarden = class(GsGraphicalModel)
  public
  weather: GsWeather;
  soilPatchList: TListCollection;
  weatherOverrides: weatherOverridesArray;
  weatherOptions: weatherOptionsArray;
  soilOverrides: soilOptionsArray;
  soilOptions: soilOptionsArray;
  plantOverrides: plantOptionsArray;
  plantOptions: plantOptionsArray;
  date: GsDate;
  numSoilPatchesCreated: longint;
  numPlantsCreated: longint;
  yearsOfSimulation: longint;
  gardenScale: single;
  gardenScaleEffect: smallint;
  plantCount: smallint; {don't stream - for hints}
  { constructor/destructor }
  destructor destroy; override;
  constructor create; override;
  { list management }
  procedure clearGarden;
	procedure fixupAllResourcesInPlants;
  procedure addSoilPatch(theSoilPatch: GsModel);
  procedure removeSoilPatch(theSoilPatch: GsModel);
  procedure loadObjectNamesIntoTStrings(stringList: TStrings);
  procedure loadObjectNamesIntoComboBoxForObjectType(objectChoice: TComboBox; objectType: integer);
  { graphical functions }
  function findSoilPatch(const aPoint: TPoint) : GsModel;
  procedure drawOn(destCanvas: TCanvas); override;
  procedure drawOnInvalidRect(destCanvas: TCanvas; invalidRect: TRect);
	procedure drawOrganicMatterOnInvalidRect(destCanvas: TCanvas; invalidRect: TRect);
  function findMulchAtXY(x, y: smallint): TObject;
  function findSoilPatchForMulchAtXY(x, y: smallint): TObject;
  function snowIsOnGround: boolean;
  { streaming/transfer }
	function objectType: integer; override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure updatePlantCount;
  class procedure fillEnumStringList(var list: TStringList; fieldID: Integer; var hasRadioButtons: boolean);
  function modelForName(name: string): GsModel;
  function soilPatchPrefixIsInUseByAnotherPatch(soilPatchProxy: GsModel): boolean;
  function plantPrefixIsInUseByAnotherPlant(plantProxy: GsModel): boolean;
  { model functions }
  procedure IGarden;
  function NextDay: boolean;
  function julianDayToday: smallint;
	function handleReseedingIfNeeded: boolean;
  function reseedPlantsWhosePlantingDateIsToday(startGrowingNow: boolean): boolean;
  function reseedAllPlantsNow(startGrowingNow: boolean): boolean;
	procedure handleAutoHarvestIfNeeded;
  procedure EndYear;
  procedure defaultOverrides;
  end;

procedure FixUpSoilPatch(each: TObject; data: TObject);

implementation

uses uesoil, ueplant, ueutils, umconsts, SysUtils, uaspects, uunits, winprocs,
	udomain, uclasses, ugsim, ucursor, ueorgmat, ugscom, usupport;

procedure FixUpSoilPatch(each: TObject; data: TObject);
  begin
  GsSoilPatch(each).garden := GsGarden(data);
  end;

constructor GsGarden.create;
	begin
  inherited create;
  { create objects under garden }
  weather := GsWeather.createWithGarden(self);
  soilPatchList := TListCollection.create;
  {edible items and organic matter blobs}
  numSoilPatchesCreated := 0;
  numPlantsCreated := 0;
  plantCount := 0;
  {any defaulting init code should be done by domain, so don't do unneeded work when stream in}
  end;

destructor GsGarden.destroy;
  begin
  weather.free;
  soilPatchList.free;
  inherited destroy;
  end;

procedure GsGarden.clearGarden;
  var
  	newList: TListCollection;
  begin
  newList := TListCollection.create;
  soilPatchList.free;
  soilPatchList := newList;
  plantCount := 0;
  end;

function GsGarden.julianDayToday: smallint;
  begin
  result := GsDate_dayOfYearFromDate(self.date);
  end;

function GsGarden.snowIsOnGround: boolean;
  var
    i: integer;
    theSoilPatch: GsSoilPatch;
	begin
  result := false;
  if soilPatchList.count > 0 then for i:= 0 to soilPatchList.count - 1 do
    begin
    theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
    if theSoilPatch.snowOnSoilPatch then
      begin
      result := true;
      exit;
      end;
    end;
  end;

procedure GsGarden.loadObjectNamesIntoTStrings(stringList: TStrings);
  var
    i, j: integer;
    theSoilPatch: GsSoilPatch;
    thePlant: GsPlant;
    plantList: TListCollection;
	begin
  if self.weather.getName = '' then self.weather.setName('Weather');
  stringList.addObject(self.weather.getName, self.weather);
  if soilPatchList.count > 0 then for i:= 0 to soilPatchList.count - 1 do
    begin
    theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
    if length(theSoilPatch.getName) = 0 then theSoilPatch.setName('unnamed soil patch');
  	stringList.addObject(theSoilPatch.getName, theSoilPatch);
    plantList := theSoilPatch.plantList;
  	if plantList.count > 0 then for j := 0 to plantList.count - 1 do
    	begin
    	thePlant := GsPlant(plantList.items[j]);
      if not thePlant.awaitingReseeding then
        begin
      	if length(thePlant.getName) = 0 then thePlant.setName('unnamed plant');
  			stringList.addObject('    ' + thePlant.getName, thePlant);
        end;
      end;
    end;
  end;

procedure GsGarden.loadObjectNamesIntoComboBoxForObjectType(objectChoice: TComboBox; objectType: integer);
  var
    i, j: integer;
    theSoilPatch: GsSoilPatch;
    thePlant: GsPlant;
	begin
  { this is used to create a combo box to choose among objects of one type for a graph loggedVar }
  { you cannot graph vars of a template or of the garden }
  case objectType of
    kObjectTypeWeather:
      begin
      if self.weather.getName = '' then self.weather.setName('Weather');
      objectChoice.items.addObject(self.weather.getName, self.weather);
      end;
    kObjectTypeSoil:
      begin
      if soilPatchList.count > 0 then for i := 0 to soilPatchList.count - 1 do
        begin
        theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
        if length(theSoilPatch.getName) = 0 then theSoilPatch.setName('unnamed soil patch');
  	    objectChoice.items.addObject(theSoilPatch.getName, theSoilPatch);
        end;
      end;
    kObjectTypePlant, KObjectTypeDrawingPlant:
      begin
      if soilPatchList.count > 0 then for i := 0 to soilPatchList.count - 1 do
        begin
        theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
  	    if theSoilPatch.plantList.count > 0 then for j:= 0 to theSoilPatch.plantList.count - 1 do
          begin
    	    thePlant := GsPlant(theSoilPatch.plantList.items[j]);
      		if not thePlant.awaitingReseeding then
        		begin
            if length(thePlant.getName) = 0 then thePlant.setName('unnamed plant');
            objectChoice.items.addObject(thePlant.getName, thePlant);
            end;
          end;
        end;
      end;
    end;
  end;

{garden has just been loaded and plants point to resources in another resource provider}
{the plants must be made to point to resources in template manager}
procedure GsGarden.fixupAllResourcesInPlants;
  var
    i, j: integer;
    theSoilPatch: GsSoilPatch;
    thePlant: GsPlant;
    plantList: TListCollection;
	begin
  if soilPatchList.count > 0 then for i:= 0 to soilPatchList.count - 1 do
    begin
    theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
    plantList := theSoilPatch.plantList;
  	if plantList.count > 0 then for j := 0 to plantList.count - 1 do
    	begin
    	thePlant := GsPlant(plantList.items[j]);
      Domain.templateManager.fixupPlantResources(thePlant);
      end;
    end;
  end;

function GsGarden.findSoilPatch(const aPoint: TPoint) : GsModel;
  var
    i: integer;
    patch: GsSoilPatch;
  begin
  result := nil;
  if soilPatchList.count > 0 then
  	for i := 0 to soilPatchList.count - 1 do
      begin
      patch := GsSoilPatch(soilPatchList.items[i]);
    	if patch.includesPoint(aPoint) then
    		begin
      	result := patch;
      	break;
    		end;
      end;
  end;

function GsGarden.soilPatchPrefixIsInUseByAnotherPatch(soilPatchProxy: GsModel): boolean;
  var
    i: longint;
    patch, thePatch: GsSoilPatch;
    match: boolean;
  begin
  result := true;
  if soilPatchProxy = nil then exit;
  thePatch := soilPatchProxy as GsSoilPatch;
  if soilPatchList.count > 0 then
  	for i := 0 to soilPatchList.count - 1 do
      begin
      patch := GsSoilPatch(soilPatchList.items[i]);
      if (patch <> nil) and (patch <> thePatch)
          and (prefixFromObjectName(patch.getName) = prefixFromObjectName(thePatch.getName)) then
        exit;
      end;
  result := false;
  end;

function GsGarden.plantPrefixIsInUseByAnotherPlant(plantProxy: GsModel): boolean;
  var
    i, j: longint;
    patch: GsSoilPatch;
    plant, thePlant: GsPlant;
    match: boolean;
  begin
  result := true;
  if plantProxy = nil then exit;
  thePlant := plantProxy as GsPlant;
  if soilPatchList.count > 0 then
  	for i := 0 to soilPatchList.count - 1 do
      begin
      patch := GsSoilPatch(soilPatchList.items[i]);
      if (patch <> nil) and (patch.plantList.count > 0) then
        for j := 0 to patch.plantList.count - 1 do
          begin
          plant := GsPlant(patch.plantList.items[j]);
          if (plant <> nil) and (plant <> thePlant)
            and (prefixFromObjectName(plant.getName) = prefixFromObjectName(thePlant.getName)) then
            exit;
          end;
      end;
  result := false;
  end;

function GsGarden.modelForName(name: string): GsModel;
  var
    i, j: longint;
    patch: GsSoilPatch;
    plant: GsPlant;
  begin
  result := nil;
  if (weather <> nil) and (weather.getName = name) then
    begin
    result := weather;
    exit;
    end;
  if soilPatchList.count > 0 then
  	for i := 0 to soilPatchList.count - 1 do
      begin
      patch := GsSoilPatch(soilPatchList.items[i]);
      if patch <> nil then
        begin
        if patch.getName = name then
          begin
          result := patch;
          exit;
          end
        else
          begin
	        if patch.plantList.count > 0 then
  	        for j := 0 to patch.plantList.count - 1 do
              begin
              plant := GsPlant(patch.plantList.Items[j]);
              if (plant <> nil) and (plant.getName = name) then
    		        begin
     		        result := plant;
      	        exit;
    		        end;
              end;
          end;
        end;
      end;
  end;

procedure GsGarden.drawOn(destCanvas: TCanvas);
  begin
  raise Exception.create('use drawOnInvalidRect instead');
  end;

procedure GsGarden.drawOnInvalidRect(destCanvas: TCanvas; invalidRect: TRect);
	var
  	patchIndex: integer;
    plantIndex: integer;
    patch: GsSoilPatch;
    plant: GsPlant;
    intersection: TRect;
{$IFDEF WINDOWS}
    intersectResult: integer;
{$ELSE}
    intersectResult: longbool;
{$ENDIF}
	begin
  if soilPatchList.count <= 0 then exit;
  if snowIsOnGround then
    begin
    destCanvas.brush.color := support_rgb(255, 255, 255);
    destCanvas.brush.style := bsDiagCross;
    {destCanvas.fillRect(invalidRect);}
    end;
  {draw soil patches}
  for patchIndex := 0 to soilPatchList.count - 1 do
  	begin
    patch := GsSoilPatch(soilPatchList.items[patchIndex]);
    intersectResult := IntersectRect(intersection, patch.boundsRect, invalidRect);
{$IFDEF WINDOWS}
    if intersectResult <> 0 then
{$ELSE}
    if intersectResult then
{$ENDIF}
    	patch.drawOn(destCanvas);
    end;
  { draw plants in each soil patch after draw all patches first }
  for patchIndex := 0 to soilPatchList.count - 1 do
  	begin
    patch := GsSoilPatch(soilPatchList.items[patchIndex]);
    if patch.plantList.count > 0 then
  		for plantIndex := 0 to patch.plantList.count - 1 do
      	begin
      	plant := GsPlant(patch.plantList.items[plantIndex]);
      	try
      	plant.computeBounds := true;
   			intersectResult := IntersectRect(intersection, plant.boundsRect, invalidRect);
{$IFDEF WINDOWS}
      	if intersectResult <> 0 then
{$ELSE}
      	if intersectResult then
{$ENDIF}
          begin
          if not plant.isDisplayedAsSymbol then
            begin
          	cursor_startWaitIfNotWaiting;
         	 	GardenForm.statusUpdateLabel('drawing ' + plant.getName);
            end
          else
         	 	GardenForm.statusUpdateLabel('drawing...');
    			plant.drawOn(destCanvas);
          end;
      	finally
      	plant.computeBounds := false;
      	end;
      	end;
    patch.computeExtendedBoundsRect; {update rect for all plants}
    end;
  { if drawing symbols over plants, do loop drawing symbols only - no need to update bounds rect of patches }
  { must do this after all soil patches and plants drawn so symbols will be on top }
  if Domain.menuOptions.drawSymbolsOverPlants then
    for patchIndex := 0 to soilPatchList.count - 1 do
  	  begin
      patch := GsSoilPatch(soilPatchList.items[patchIndex]);
      if patch.plantList.count > 0 then
  		  for plantIndex := 0 to patch.plantList.count - 1 do
      	  begin
      	  plant := GsPlant(patch.plantList.items[plantIndex]);
   			  intersectResult := IntersectRect(intersection, plant.boundsRect, invalidRect);
{$IFDEF WINDOWS}
      	  if intersectResult <> 0 then
{$ELSE}
      	  if intersectResult then
{$ENDIF}
            begin
            GardenForm.statusUpdateLabel('drawing...');
    			  if not plant.awaitingReseeding then plant.drawOnAsSymbol(destCanvas);
            end;
      	  end;
      end;
  end;

procedure GsGarden.drawOrganicMatterOnInvalidRect(destCanvas: TCanvas; invalidRect: TRect);
	var
  	patchIndex: integer;
    organicMatterIndex: integer;
    patch: GsSoilPatch;
    organicMatter: GsOrganicMatter;
    intersection: TRect;
{$IFDEF WINDOWS}
    intersectResult: integer;
{$ELSE}
    intersectResult: longbool;
{$ENDIF}
	begin
  if soilPatchList.count <= 0 then exit;
  {draw organic matter in each soil patch}
  for patchIndex := 0 to soilPatchList.count - 1 do
  	begin
    patch := GsSoilPatch(soilPatchList.items[patchIndex]);
    if patch.looseOrganicMatterList.count > 0 then
  		for organicMatterIndex := 0 to patch.looseOrganicMatterList.count - 1 do
      	begin
      	organicMatter := GsOrganicMatter(patch.looseOrganicMatterList.items[organicMatterIndex]);
   			intersectResult := IntersectRect(intersection, organicMatter.boundsRect, invalidRect);
{$IFDEF WINDOWS}
      	if intersectResult <> 0 then
{$ELSE}
      	if intersectResult then
{$ENDIF}
          begin
          organicMatter.drawOn(destCanvas);
          end;
      	end;
    end;
  end;

function GsGarden.findMulchAtXY(x, y: smallint): TObject;
	var
  	patchIndex, organicMatterIndex: integer;
    patch: GsSoilPatch;
    organicMatter: GsOrganicMatter;
    intersection: TRect;
    thePoint: TPoint;
	begin
  result := nil;
  if soilPatchList.count <= 0 then exit;
  thePoint := Point(x, y);
  {draw organic matter in each soil patch}
  {loop backwards because want to select higher drawn ones first}
  for patchIndex := soilPatchList.count - 1 downto 0 do
  	begin
    patch := GsSoilPatch(soilPatchList.items[patchIndex]);
    if patch.looseOrganicMatterList.count > 0 then
  		for organicMatterIndex := patch.looseOrganicMatterList.count - 1 downTo 0 do
      	begin
      	organicMatter := GsOrganicMatter(patch.looseOrganicMatterList.items[organicMatterIndex]);
        if organicMatter.includesPoint(thePoint) then
          begin
          result := organicMatter;
          exit;
          end;
      	end;
    end;
  end;

function GsGarden.findSoilPatchForMulchAtXY(x, y: smallint): TObject;
	var
  	patchIndex, organicMatterIndex: integer;
    patch: GsSoilPatch;
    organicMatter: GsOrganicMatter;
    intersection: TRect;
    thePoint: TPoint;
	begin
  result := nil;
  if soilPatchList.count <= 0 then exit;
  thePoint := Point(x, y);
  {draw organic matter in each soil patch}
  {loop backwards because want to select higher drawn ones first}
  for patchIndex := soilPatchList.count - 1 downto 0 do
  	begin
    patch := GsSoilPatch(soilPatchList.items[patchIndex]);
    if patch.looseOrganicMatterList.count > 0 then
  		for organicMatterIndex := patch.looseOrganicMatterList.count - 1 downTo 0 do
      	begin
      	organicMatter := GsOrganicMatter(patch.looseOrganicMatterList.items[organicMatterIndex]);
        if organicMatter.includesPoint(thePoint) then
          begin
          result := patch;
          exit;
          end;
      	end;
    end;
  end;

procedure GsGarden.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsGarden;
  cvir.versionNumber := 6;
  cvir.additionNumber := 1;
  end;

procedure GsGarden.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  {model structures}
  filer.streamBytes(weatherOverrides, sizeOf(weatherOverrides));
  filer.streamBytes(weatherOptions, sizeOf(weatherOptions));
  filer.streamBytes(soilOverrides, sizeOf(soilOverrides));
  filer.streamBytes(soilOptions, sizeOf(soilOptions));
  filer.streamBytes(plantOverrides, sizeOf(plantOverrides));
  filer.streamBytes(plantOptions, sizeOf(plantOptions));
  filer.streamDate(date);
  filer.streamSingle(gardenScale);
  filer.streamSmallint(gardenScaleEffect);
  filer.streamLongint(numSoilPatchesCreated);
  filer.streamLongint(numPlantsCreated);
  filer.streamLongint(yearsOfSimulation);
  {read will corrupt pointers to weather and soil patches}
  weather.streamUsingFiler(filer);
  {should change this to pointer}
  if filer.isReading then
    begin
    weather.garden := self;
    { weather needs to calculate this on streaming in so plants can use it to grow with magic wand,
      and weather needs garden pointer to do it, so must do after pointer is set }
    weather.CalculateMaxPossibleRadiationForYear;
    end;
  soilPatchList.streamUsingFiler(filer, GsSoilPatch);
  if filer.isReading then soilPatchList.forEach(FixUpSoilPatch, self);
  if filer.isReading then
    self.updatePlantCount;
  if (filer.isReading) and (cvir.additionNumber < 1) then
    begin
    soilOptions[kSoilOptionsAllowFlatCropResidueToDecay] := true;
    soilOverrides[kSoilOptionsAllowFlatCropResidueToDecay] := true;
    end;
 end;

procedure GsGarden.updatePlantCount;
  var
    patch: GsSoilPatch;
    plant: GsPlant;
    i, j: smallint;
  begin
  plantCount := 0;
  if soilPatchList.count > 0 then
    for i := 0 to soilPatchList.count - 1 do
      begin
      patch := GsSoilPatch(soilPatchList.items[i]);
      if patch = nil then continue;
      if patch.plantList.count > 0 then
        for j := 0 to patch.plantList.count - 1 do
          begin
          plant := GsPlant(patch.plantList.items[j]);
          if plant = nil then continue;
          { plants in stasis boxes are ok, but plants awaiting reseeding are not }
          if not plant.awaitingReseeding then
            inc(plantCount);
          end;
      end;
  end;

class procedure GsGarden.fillEnumStringList(var list: TStringList; fieldID: Integer;
    var hasRadioButtons: boolean);
  begin
  { garden has none }
  end;

function GsGarden.NextDay: boolean;
  { returns true if browser needs to be updated because of events such as reseeding }
  var
    i: longint;
    theSoilPatch: GsSoilPatch;
  begin
  result := false;
  { next day functions }
  weather.WeatherNextDay;
  if soilPatchList.count > 0 then for i:= 0 to soilPatchList.count - 1 do
    begin
    theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
    theSoilPatch.SoilNextDay;
    end;
  self.updatePlantCount;
  { date change }
  GsDate_addDays(date, 1);
  self.handleAutoHarvestIfNeeded;
  result := self.handleReseedingIfNeeded;
  if self.julianDayToday = 1 then
    begin
    inc(yearsOfSimulation);  {used by weather for long-term normalizing}
   	self.EndYear;
    end;
  end;

function GsGarden.handleReseedingIfNeeded: boolean;
  { returns true if any plants were reseeded }
  begin
  result := false;
  with Domain.menuOptions do
    begin
  	if reseedingOption = kReseedingOnMonth then
      begin
    	{ reseedingMonth is zero based }
  		if (GsDate_monthFromDate(date) = reseedingMonth) and (GsDate_dayOfMonthFromDate(date) = 0) then
    		result := self.reseedAllPlantsNow(kBecomeSeedAndStartGrowingNow);
    	end
    else if reseedingOption = kReseedingWhenPlanted then
      result := self.reseedPlantsWhosePlantingDateIsToday(kBecomeSeedAndStartGrowingNow);
    end;
  end;

 function GsGarden.reseedPlantsWhosePlantingDateIsToday(startGrowingNow: boolean): boolean;
  { returns true if any plants were reseeded }
	var
  	patchIndex, plantIndex: longint;
    patch: GsSoilPatch;
    plant: GsPlant;
    dayOfYear: integer;
    patchChanged: boolean;
    reseedPlantCommand: GsReseedPlantCommand;
	begin
  result := false;
  {reseed plants in each soil patch}
  dayOfYear := self.julianDayToday;
  if soilPatchList.count > 0 then
  	for patchIndex := 0 to soilPatchList.count - 1 do
  		begin
    	patch := GsSoilPatch(soilPatchList.items[patchIndex]);
      patchChanged := false;
    	if patch.plantList.count > 0 then
  			for plantIndex := 0 to patch.plantList.count - 1 do
      		begin
      		plant := GsPlant(patch.plantList.items[plantIndex]);
          if dayOfYear = plant.development.dayPlanted then
            begin
            { changed to go through command so OM blobs are created, but we don't want to put the command in the list }
            reseedPlantCommand := GsReseedPlantCommand.createWithPlant(plant, startGrowingNow);
            reseedPlantCommand.doCommand;
            reseedPlantCommand.free;
            reseedPlantCommand := nil;
            patchChanged := true;
            result := true;
            end;
      		end;
    	if patchChanged then
        begin
      	patch.computeExtendedBoundsRect; { update rect for all plants }
        end;
      end;
   end;

function GsGarden.reseedAllPlantsNow(startGrowingNow: boolean): boolean;
  { returns true if any plants were reseeded }
	var
  	patchIndex, plantIndex: longint;
    patch: GsSoilPatch;
    plant: GsPlant;
    reseedPlantCommand: GsReseedPlantCommand;
	begin
  result := false;
  { reseed plants in each soil patch }
  if soilPatchList.count > 0 then
  	for patchIndex := 0 to soilPatchList.count - 1 do
  		begin
    	patch := GsSoilPatch(soilPatchList.items[patchIndex]);
    	if patch.plantList.count > 0 then
  			for plantIndex := 0 to patch.plantList.count - 1 do
      		begin
          { changed to go through command so OM blobs are created }
      		plant := GsPlant(patch.plantList.items[plantIndex]);
          reseedPlantCommand := GsReseedPlantCommand.createWithPlant(plant, startGrowingNow);
          reseedPlantCommand.doCommand;
          reseedPlantCommand.free;
          reseedPlantCommand := nil;
          result := true;
      		end;
    	patch.computeExtendedBoundsRect; { update rect for all plants }
      end;
   end;

procedure GsGarden.handleAutoHarvestIfNeeded;
  var
    i, j: longint;
    theSoilPatch: GsSoilPatch;
    thePlant: GsPlant;
    harvestCommand: GsHarvestPlantCommand;
    commandsToProcess: TListCollection;
    statusHasBeenShown: boolean;
    harvestedFromThisSoilPatch: boolean;
  begin
  statusHasBeenShown := false;
  commandsToProcess := TListCollection.create;
  try
  if soilPatchList.count > 0 then
  	for i:= 0 to soilPatchList.count - 1 do
    	begin
    	theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
      harvestedFromThisSoilPatch := false;
      if not theSoilPatch.optionIsTrue(kSoilOptionsAutoHarvest) then
        continue;
      if theSoilPatch.daysSinceLastAutoHarvest < theSoilPatch.params.minIntervalAutoHarvest_days then
        continue;
    	if theSoilPatch.plantList.count > 0 then
      	for j := 0 to theSoilPatch.plantList.count - 1 do
          begin
          thePlant := GsPlant(theSoilPatch.plantList.items[j]);
          if not thePlant.awaitingReseeding and
          		(thePlant.development.heatUnitIndex >= 1.0) then
            begin
            { only show on garden form if at least one plant has been harvested }
            if not statusHasBeenShown then
              begin
              if GardenForm.statusPanel.visible then
              	GardenForm.statusUpdateLabel('auto harvesting...');
              statusHasBeenShown := true;
              end;
            if thePlant.drawingPlant.iconForHarvesting(false) <> nil then
              begin
              {may need to do something special to command to supress GUI updates}
            	harvestCommand := GsHarvestPlantCommand.create;
            	harvestCommand.userWantsToPullPlant := false;
            	harvestCommand.soilPatch := theSoilPatch;
            	harvestCommand.plant := thePlant;
              commandsToProcess.add(harvestCommand);
              harvestedFromThisSoilPatch := true;
              end;
            end;
          end;
      if harvestedFromThisSoilPatch then
        theSoilPatch.daysSinceLastAutoHarvest := 0;
    	end;
  {need to do commands after iterate through list in case some of them change the list}
  if commandsToProcess.count > 0 then
    for i := 0 to commandsToProcess.count - 1 do
      begin
      harvestCommand := GsHarvestPlantCommand(commandsToProcess.items[i]);
      harvestCommand.doCommand;
      end;
  finally
  commandsToProcess.free;
  end;
  end;

procedure GsGarden.EndYear;
  var
    i: integer;
    theSoilPatch: GsSoilPatch;
  begin
  if soilPatchList.count > 0 then for i := 0 to soilPatchList.count - 1 do
    begin
    theSoilPatch := GsSoilPatch(soilPatchList.items[i]);
    theSoilPatch.SoilEndYear;
    end;
  end;

procedure GsGarden.addSoilPatch(theSoilPatch: GsModel);
  begin
  if (theSoilPatch = nil) then exit;
  soilPatchList.add(theSoilPatch);
  end;

procedure GsGarden.removeSoilPatch(theSoilPatch: GsModel);
  begin
  if (theSoilPatch = nil) then exit;
  soilPatchList.remove(theSoilPatch);
  end;

procedure GsGarden.IGarden;
  begin
  { set up starting date }
  { start simulation at the first day of the year they really start garden; they can change year later, but not day }
  date := GsDate_currentDate;
  date := GsDate_dateFromYearMonthDayOfMonth(GsDate_yearFromDate(date), 0, 0);
  yearsOfSimulation := 0;
  gardenScale := 0.5; { default garden scale }
  gardenScaleEffect := kGardenScaleMultipliesPatchScale;
  weather.copyFromClimate(Domain.templateManager.defaultClimate);
  self.defaultOverrides;
  end;

procedure GsGarden.defaultOverrides;
  var i: smallint;
  begin
  { default to all overridden and all optimal }
  for i := 0 to kWeatherOptionsLastOption do
    weatherOverrides[i] := true;
  for i := 0 to kSoilOptionsLastOption do
    begin
    soilOverrides[i] := true;
    soilOptions[i] := true;
    end;
  for i := 0 to kPlantOptionsLastOption do
    begin
    plantOverrides[i] := true;
    plantOptions[i] := true;
    end;
  end;

function GsGarden.objectType: integer;
  begin
  result := kObjectTypeGarden;
  end;

end.

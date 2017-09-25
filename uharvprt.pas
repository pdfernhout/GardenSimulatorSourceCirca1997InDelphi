unit uharvprt;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uharvprt: Defines several objects.
* The harvest item template (HIT) contains information about types of things
that can be harvested, including nutritional information about a typical item
of that type (calories, energy, vitamins, minerals), and which icon is used to
show the item in the garden window and harvest panels.
* The harvest item models a specific fruit, leaf or other object that was harvested
from a particular plant. It keeps track of which plant it was harvested from,
which cultivar the plant belonged to, which soil patch it was in, what date
it was harvested, and what kind of item it is (which leads to a HIT).
* The harvest report specifies what information to show about harvested items
in the harvest window, and how to sort that information. see the constant lists
below for what the 'show' and 'sort by' possibilities are.
* The harvest item sorting group is an object used when sorting and totaling the
harvest item nutritional information. For example, if the harvest report
says (in effect) 'show calories per week', the harvest manager sorts the
harvested items by week, and the harvest item sorting group keeps the total amount
of calories in each week so it can display that total in the harvest window.
* The harvest manager handles all the harvest information. It resides in the
domain, and the harvest window (uharvest), HIT editor (uharved), and
harvest report editor (uhvrpted) access and change it.}

interface

uses SysUtils, Classes, ufiler, ufilertx, umodel, uicon, uturt3d, udate, ucollect;

const
  kHarvestPartTypeWholePlant = 0;
  kHarvestPartTypeSeedlingLeaf = 1;
  kHarvestPartTypeLeaf = 2;
  kHarvestPartTypeFemaleInflorescence = 3;
  kHarvestPartTypeMaleInflorescence = 4;
  kHarvestPartTypeFemaleFlower = 5;
  kHarvestPartTypeFemaleFlowerBud = 6;
  kHarvestPartTypeMaleFlower = 7;
  kHarvestPartTypeMaleFlowerBud = 8;
  kHarvestPartTypeAxillaryBud = 9;
  kHarvestPartTypeFruit = 10;
  kHarvestPartTypeRoot = 11;
  kHarvestPartTypeStem = 12;
  kHarvestPartTypeStorageOrgan = 13;
  kHarvestPartTypeLast = 13;
  kStatisticsPartTypeUnripeFruit = 14;
  kStatisticsPartTypeFallenFruit = 15;
  kStatisticsPartTypeUnallocatedNewVegetativeBiomass_kg = 16;
  kStatisticsPartTypeUnremovedDeadVegetativeBiomass_kg = 17;
  kStatisticsPartTypeUnallocatedNewReproductiveBiomass_kg = 18;
  kStatisticsPartTypeUnremovedDeadReproductiveBiomass_kg = 19;
  kStatisticsPartTypeFallenFlower = 20;
  kStatisticsPartTypeAllVegetative = 21;
  kStatisticsPartTypeAllReproductive = 22;
  kStatisticsPartTypeUnremovedStandingDeadBiomass_kg = 23;
  kStatisticsPartTypeLast = 23;

const
  kHarvestReportSortEndOfList = -1;
  kHarvestReportSortByYear = 0;
  kHarvestReportSortBySoilPatch = 1;
  kHarvestReportSortByMonth = 2;
  kHarvestReportSortByWeek = 3;
  kHarvestReportSortByType = 4;
  kHarvestReportSortByPlant = 5;
  kHarvestReportSortByLastChoice = 5;

  kHarvestReportShowEndOfList = -1;
  kHarvestReportShowCount = 0;
  { if not totaling, all other properties pertain to this unit amount of biomass }
  kHarvestReportShowBiomassWet_g = 1;
  kHarvestReportShowBiomassDry_g = 2;
  kHarvestReportShowRefuse_pct = 3;
  { these are all exactly as found in the USDA book on nutritional information }
  kHarvestReportShowEnergy_cal = 4;
  kHarvestReportShowWater_pct = 5;
  kHarvestReportShowProtein_g = 6;
  kHarvestReportShowFat_g = 7;
  kHarvestReportShowCarbohydrate_g = 8;
  kHarvestReportShowCalcium_mg = 9;
  kHarvestReportShowPhosphorus_mg = 10;
  kHarvestReportShowIron_mg = 11;
  kHarvestReportShowSodium_mg = 12;
  kHarvestReportShowPotassium_mg = 13;
  kHarvestReportShowVitaminAValue_IU = 14;
  kHarvestReportShowThiamin_mg = 15;
  kHarvestReportShowRiboflavin_mg = 16;
  kHarvestReportShowNiacin_mg = 17;
  kHarvestReportShowAscorbicAcid_mg = 18;
  kHarvestReportShowUserVariable1 = 19;
  kHarvestReportShowUserVariable2 = 20;
  kHarvestReportShowLastChoice = 20;

type

GsHarvestItemTemplate = class(GsStreamableObject)
  public
  name: string[kGraphicalModelNameLength];
  note: Pchar;
  icon: GsIcon;
  itemType: smallint;
  infoArray: array[0..kHarvestReportShowLastChoice] of single;
  inUse: boolean;
  originalIfCopy: GsStreamableObject;
  constructor create; override;
  destructor destroy; override;
  function getName: string; override;
  procedure setName(newName: string); override;
  function isSameAs(other: GsHarvestItemTemplate): boolean;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  end;

GsHarvestItem = class(GsStreamableObject)
  public
  count: longint;
  biomass_g: single;
  dateHarvested: GsDate;
  plantName: string[kGraphicalModelNameLength];
  soilPatchName: string[kGraphicalModelNameLength];
  harvestItemTemplate: GsHarvestItemTemplate;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  function infoForIndex(index: smallint): single;
  function hint(wantLongHint: boolean): string;
  end;

GsHarvestReport = class(GsStreamableObject)
  public
  name: string;
  sortByChoicesArray: array[0..kHarvestReportSortByLastChoice] of smallint;
  showChoicesArray: array[0..kHarvestReportShowLastChoice] of smallint;
  originalIfCopy: GsHarvestReport;
  inUse: boolean;
  constructor create; override;
	function verifyClassAndVersionInformation(
		filer: GsFiler; size: longint; const cvirRead, cvirClass: GsClassAndVersionInformationRecord): boolean; override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  end;

GsHarvestItemSortingGroup = class(GsStreamableObject)
  public
  firstHarvestItem: GsHarvestItem;
  firstHarvestItemIndex: longint;
  numHarvestItems: longint;
  height: longint;
  totals: array[0..kHarvestReportShowLastChoice] of single;
  constructor createWithReportAndHarvestItem(aReport: GsHarvestReport;
    aHarvestItem: GsHarvestItem; index: longint);
  function addHarvestItem(aReport: GsHarvestReport;
    aHarvestItem: GsHarvestItem): boolean;
  function textForSortIndex(index: smallint): string;
  function textForShowIndex(index: smallint): string;
  end;

GsHarvestManager = class(GsStreamableObject)
  public
  harvestItemList: TListCollection;
  harvestReportList: TListCollection;
  currentReport: GsHarvestReport;
  currentSortList: TList;
  monthToResetAllReports: smallint;
  afterHowManyYearsToResetAllReports: longint;
  lastReportResetDate: GsDate;
  constructor create; override;
  destructor destroy; override;
  procedure fixupAllResourcesInHarvestItems;
  procedure setLastReportResetDate(aDate: GsDate);
  procedure nextDay(aDate: GsDate);
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure fillHarvestItemListForPlant(listToFill: TList; plantProxy: GsGraphicalModel);
  function harvestItemTemplateInfoArrayDescription(index: integer): string;
  function harvestItemTypeDescription(index: integer): string;
  function harvestItemTemplateInfoArrayName(index: integer): string;
  function harvestItemTemplateInfoArrayUnit(index: integer): string;
  function newHarvestReport(aName: string): GsHarvestReport;
  procedure fillReportGroupList(reportGroupList: TListCollection;
    sortedHarvestItemList: TList; aReport: GsHarvestReport);
  procedure sortHarvestListUsingHarvestReport(sortedList: TList; aReport: GsHarvestReport);
  procedure sortHarvestListByOneSortIndex(left: integer; right: integer);
  function equalSortFieldInfo(
    firstItem, secondItem: GsHarvestItem; index: smallint): boolean;
  function firstLessThanSecondSortFieldInfo(
    firstItem, secondItem: GsHarvestItem; index: smallint): boolean;
  function firstGreaterThanSecondSortFieldInfo(
    firstItem, secondItem: GsHarvestItem; index: smallint): boolean;
  function sortByChoicesArrayDescription(index: integer): string;
  function firstLessThanSecondForAllSortFields(firstItem, secondItem: GsHarvestItem): boolean;
  function firstGreaterThanSecondForAllSortFields(firstItem, secondItem: GsHarvestItem): boolean;
  end;

implementation

uses utempman, udomain, usupport, ueutils, uharvest, ugscom, ugsim, ubrowser, uclasses;

{GsHarvestItemTemplate}
constructor GsHarvestItemTemplate.create;
  begin
  inherited create;
  { default }
  infoArray[kHarvestReportShowCount] := 1;
  end;

destructor GsHarvestItemTemplate.destroy;
  begin
  if note <> nil then
    begin
    StrDispose(note);
    note := nil;
    end;
  end;

function GsHarvestItemTemplate.isSameAs(other: GsHarvestItemTemplate): boolean;
  var i: smallint;
  begin
  result := false;
  if itemType <> other.itemType then exit;
  if self.name <> other.name then exit;
  for i := 0 to kHarvestPartTypeLast do
    if infoArray[i] <> other.infoArray[i] then exit;
  if not icon.isSameAs(other.icon) then exit;
  result := true;
  end;

procedure GsHarvestItemTemplate.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsHarvestItemTemplate;
  cvir.versionNumber := 0;
  cvir.additionNumber := 1;
  end;

procedure GsHarvestItemTemplate.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var templateManager: GsTemplateManager;
  begin
  filer.streamShortString(name);
  filer.streamSmallInt(itemType);
  filer.streamSingleArray(infoArray);
  filer.streamBoolean(inUse); {do this so editor can use it}
  templateManager := filer.resourceProvider as GsTemplateManager;
  {copyTo doesn't set the resource system}
  {if it is used on objects in another template manager, the results for these field
  will be wrong - the template manager dialog adjusts for this after the copy}
  if templateManager = nil then
    templateManager := Domain.templateManager;
  if templateManager <> nil then
  	{ tell icon to stream itself as reference}
  	templateManager.streamReferenceToIcon(filer, icon);
  if cvir.additionNumber >= 1 then
    filer.streamPChar(note);
  end;

function GsHarvestItemTemplate.getName: string;
  begin
  result := self.name;
  end;

procedure GsHarvestItemTemplate.setName(newName: string);
  begin
  name := copy(newName, 1, kGraphicalModelNameLength);
  end;

procedure GsHarvestItemTemplate.streamUsingTextFiler(textFiler: GsTextFiler);
  var
    iconName: string;
    i: smallint;
    provider: GsTemplateManager;
  begin
  provider := nil;
  with textFiler do
    begin
    if isReading then
      begin
      provider := GsTemplateManager(textFiler.resourceProvider);
      if provider = nil then provider := Domain.templateManager;
      if provider = nil then exit;
      end;
    if isWriting then iconName := icon.getName;
    streamString(iconName, 'icon');
    if isReading and (provider <> nil) then self.icon := provider.findMatchForIconByName(iconName);
    streamSmallintWithLabel(itemType, Domain.harvestManager.harvestItemTypeDescription(itemType), 'type');
    if textFiler.versionString = 'v0.9' then
      begin
      streamSingle(infoArray[0], Domain.harvestManager.harvestItemTemplateInfoArrayDescription(0));
      streamSingle(infoArray[1], Domain.harvestManager.harvestItemTemplateInfoArrayDescription(1));
      streamSingle(infoArray[2], Domain.harvestManager.harvestItemTemplateInfoArrayDescription(2));
      { skip kHarvestReportShowRefuse_pct (3) which is new one }
      infoArray[kHarvestReportShowRefuse_pct] := 0.0;
      for i := 4 to kHarvestReportShowLastChoice do
        streamSingle(infoArray[i], Domain.harvestManager.harvestItemTemplateInfoArrayDescription(i));
      end
    else if textFiler.versionString = 'v1.0' then
      begin
      for i := 0 to kHarvestReportShowLastChoice do
        streamSingle(infoArray[i], Domain.harvestManager.harvestItemTemplateInfoArrayDescription(i));
      end;
    streamPChar(note, 'note');
    streamEndOfLine;
    end;
  end;

{ GsHarvestItem }
procedure GsHarvestItem.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsHarvestItem;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsHarvestItem.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var templateManager: GsTemplateManager;
  begin
  filer.streamLongint(count);
  filer.streamSingle(biomass_g);
  filer.streamBytes(dateHarvested, sizeOf(dateHarvested));
  filer.streamShortString(plantName);
  filer.streamShortString(soilPatchName);
  templateManager := filer.resourceProvider as GsTemplateManager;
  {copyTo doesn't set the resource system}
  {if it is used on objects in another template manager, the results for these field
  will be wrong - the template manager dialog adjusts for this after the copy}
  if templateManager = nil then
    templateManager := Domain.templateManager;
  if templateManager <> nil then
  	{ tell icon to stream itself as reference}
  	templateManager.streamReferenceToHarvestItemTemplate(filer, harvestItemTemplate);
  end;

function GsHarvestItem.infoForIndex(index: smallint): single;
  var
    thisPortionOfStandard_frn: single;
  begin
  result := 0.0;
  if harvestItemTemplate = nil then
    raise Exception.create('GsHarvestItem.infoForIndex nil harvestItemTemplate');
  if (index < 0) or (index > kHarvestReportShowLastChoice) then
    raise Exception.create('GsHarvestItem.infoForIndex index out of range');
  if (index = kHarvestReportShowCount)
    or (index = kHarvestReportShowWater_pct)
    or (index = kHarvestReportShowRefuse_pct) then
    result := harvestItemTemplate.infoArray[index]
  else
    begin
    if harvestItemTemplate.infoArray[kHarvestReportShowBiomassDry_g] = 0.0 then
      result := 0.0
    else
      begin
      thisPortionOfStandard_frn := biomass_g / harvestItemTemplate.infoArray[kHarvestReportShowBiomassDry_g]
        * (1.0 - harvestItemTemplate.infoArray[kHarvestReportShowRefuse_pct] / 100.0);
      result := thisPortionOfStandard_frn * harvestItemTemplate.infoArray[index];
      end;
    end;
  end;

function lessPreciseString(value: single): string;
  begin
  if value > 10 then
    result := intToStr(round(value))
  else
    result := digitValueString(value);
  end;

function GsHarvestItem.hint(wantLongHint: boolean): string;
  var
    i, index: smallint;
    harvestReport: GsHarvestReport;
    unitString: string;
  begin
  result := harvestItemTemplate.name;
  if not wantLongHint then exit;
  { basic info }
  result := result + ' from plant [' + plantName + '] in soil patch [' + soilPatchName + '].';
  result := result + ' Harvested ' + GsDate_dateString(dateHarvested) + '.';
  { biomass (wet) }
  result := result + ' Wet biomass: ' + lessPreciseString(self.infoForIndex(kHarvestReportShowBiomassWet_g)) + ' g.';
  { get current report }
  if HarvestForm = nil then exit;
  harvestReport := nil;
  harvestReport := HarvestForm.currentReport;
  if harvestReport = nil then exit;
  for i := 0 to kHarvestReportShowLastChoice do
    begin
    index := harvestReport.showChoicesArray[i];
    if index = kHarvestReportShowEndOfList then break;
    if (index = kHarvestReportShowCount) or (index = kHarvestReportShowBiomassWet_g) then continue;
    result := result + ' ' + Domain.harvestManager.harvestItemTemplateInfoArrayName(index) + ': '
        + lessPreciseString(self.infoForIndex(index));
    unitString := Domain.harvestManager.harvestItemTemplateInfoArrayUnit(index);
    if unitString <> '' then
      result := result + ' ' + unitString;
    result := result + '.';
    end;
  { if cut off, put ellipse }
  if length(result) >= 250 then
    result := copy(result, 1, 252) + '...';
  end;

{ GsHarvestReport }
constructor GsHarvestReport.create;
  var i: smallint;
  begin
  inherited create;
  for i := 0 to kHarvestReportShowLastChoice do showChoicesArray[i] := kHarvestReportShowEndOfList;
  for i := 0 to kHarvestReportSortByLastChoice do sortByChoicesArray[i] := kHarvestReportSortEndOfList;
  end;

{handle reading older version with fixed size strings}
function GsHarvestReport.verifyClassAndVersionInformation(
	filer: GsFiler; size: longint; const cvirRead, cvirClass: GsClassAndVersionInformationRecord): boolean;
  begin
  if cvirRead.classNumber <> cvirClass.classNumber then
    begin
    raise GsExceptionFilerUnexpectedClassNumber.createWithSizeAndMessage(size,
    	'Problem reading file.  This file may be corrupt. (Expected class ' +
      IntToStr(cvirClass.classNumber) + ' read class ' +
      IntToStr(cvirRead.classNumber) + ')');
    end;
  if cvirRead.versionNumber > cvirClass.versionNumber then
    begin
    raise GsExceptionFilerUnexpectedVersionNumber.createWithSizeAndMessage(size,
    	'Problem reading file.  This file is of a different version than the program. (Class ' + IntToStr(cvirClass.classNumber) +
    	' version ' + IntToStr(cvirClass.versionNumber) +
    	' cannot read version ' + IntToStr(cvirRead.versionNumber) + ')');
    end;
  result := true;
  end;

procedure GsHarvestReport.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsHarvestReport;
  cvir.versionNumber := 1; {this is handled specially - see above verifyClassAndVersionInformation}
  cvir.additionNumber := 0;
  end;

procedure GsHarvestReport.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  if cvir.versionNumber = 0 then
    filer.streamBytes(self.name, self.instanceSize - 4)
  else
  begin
  filer.streamShortstring(name);
  filer.streamBytes(sortByChoicesArray, sizeof(sortByChoicesArray));
  filer.streamBytes(showChoicesArray, sizeof(showChoicesArray));
  {don't stream - originalIfCopy: GsHarvestReport; }
  if filer.isReading then originalIfCopy := nil;
  filer.streamBoolean(inUse);
  end;
  end;

{ GsHarvestManager }
constructor GsHarvestManager.create;
  var
    newReport: GsHarvestReport;
  begin
  harvestItemList := TListCollection.create;
  harvestReportList := TListCollection.create;
  { default to resetting all reports every other year on Jan 1 }
  monthToResetAllReports := 0;
  afterHowManyYearsToResetAllReports := 2;
  end;

destructor GsHarvestManager.destroy;
  begin
  harvestItemList.free;
  harvestItemList := nil;
  harvestReportList.free;
  harvestReportList := nil;
  inherited destroy;
  end;

function GsHarvestManager.newHarvestReport(aName: string): GsHarvestReport;
  begin
  result := GsHarvestReport.create;
  result.name := aName;
  harvestReportList.add(result);
  end;

procedure GsHarvestManager.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsHarvestManager;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsHarvestManager.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  harvestItemList.streamUsingFiler(filer, GsHarvestItem);
  harvestReportList.streamUsingFiler(filer, GsHarvestReport);
  filer.streamSmallint(monthToResetAllReports);
  filer.streamLongint(afterHowManyYearsToResetAllReports);
  filer.streamBytes(lastReportResetDate, sizeOf(lastReportResetDate));
  end;

{harvest manager has just been loaded and harvest items point to resources in another resource provider}
{the harvest items must be made to point to resources in template manager}
procedure GsHarvestManager.fixupAllResourcesInHarvestItems;
  var i: longint;
  begin
  if harvestItemList.count > 0 then
    for i := 0 to harvestItemList.count - 1 do
      Domain.templateManager.fixupHarvestItemResources(GsHarvestItem(harvestItemList.items[i]));
  end;

procedure GsHarvestManager.setLastReportResetDate(aDate: GsDate);
  begin
  { this is called by the domain after a new garden is created }
  { it must be done after IGarden is called because that is where the garden date is set }
  { may be better way to do this }
  { also done by command when user resets reports by clicking button in report editor }
  lastReportResetDate := aDate;
  end;

procedure GsHarvestManager.nextDay(aDate: GsDate);
  var
    yearsDifferent: longint;
  begin
  { if "never" selected, do not reset }
  if monthToResetAllReports = -1 then exit;
  { if not first of month (zero-based), do not reset }
  if GsDate_dayOfMonthFromDate(aDate) <> 0 then exit;
  { if not correct month, do not reset }
  if GsDate_monthFromDate(aDate) <> monthToResetAllReports then exit;
  { if correct number of years has not gone by since last reset (ignore leap year), do not reset }
  { if afterHowManyYearsToResetAllReports is 1, it means EACH January 1 (or whatever month) }
  if afterHowManyYearsToResetAllReports > 1 then
    begin
    yearsDifferent := GsDate_yearFromDate(aDate) - GsDate_yearFromDate(lastReportResetDate);
    if GsDate_monthFromDate(lastReportResetDate) > GsDate_monthFromDate(aDate) then
      dec(yearsDifferent);
    if yearsDifferent < afterHowManyYearsToResetAllReports then exit;
    end;
  { don't use undo command for this because user is not doing it }
  harvestItemList.free;
  harvestItemList := nil;
  harvestItemList := TListCollection.create;
  lastReportResetDate := aDate;
  GardenForm.updateHarvestPanel := true;
  GardenForm.handlePossibleGardenChange;
  BrowserForm.drawPlantHarvestItems;
  HarvestForm.handlePossibleHarvestListChange;
  end;

function GsHarvestManager.equalSortFieldInfo(
    firstItem, secondItem: GsHarvestItem; index: smallint): boolean;
  begin
  result := false;
  case index of
    kHarvestReportSortByYear:
      result :=
        GsDate_yearFromDate(firstItem.dateHarvested) = GsDate_yearFromDate(secondItem.dateHarvested);
    kHarvestReportSortBySoilPatch:
      result := (firstItem.soilPatchName = secondItem.soilPatchName);
    kHarvestReportSortByMonth:
      result :=
        GsDate_monthFromDate(firstItem.dateHarvested) = GsDate_monthFromDate(secondItem.dateHarvested);
    kHarvestReportSortByWeek:
      result :=
        GsDate_weekOfYearFromDate(firstItem.dateHarvested) = GsDate_weekOfYearFromDate(secondItem.dateHarvested);
    kHarvestReportSortByType:
      result := (firstItem.harvestItemTemplate.name = secondItem.harvestItemTemplate.name);
    kHarvestReportSortByPlant:
      result := (firstItem.plantName = secondItem.plantName);
    end;
  end;

function GsHarvestManager.firstLessThanSecondSortFieldInfo(
    firstItem, secondItem: GsHarvestItem; index: smallint): boolean;
  begin
  result := false;
  case index of
    kHarvestReportSortByYear:
      result :=
        GsDate_yearFromDate(firstItem.dateHarvested) < GsDate_yearFromDate(secondItem.dateHarvested);
    kHarvestReportSortBySoilPatch:
      result := (firstItem.soilPatchName < secondItem.soilPatchName);
    kHarvestReportSortByMonth:
      result :=
        GsDate_monthFromDate(firstItem.dateHarvested) < GsDate_monthFromDate(secondItem.dateHarvested);
    kHarvestReportSortByWeek:
      result :=
        GsDate_weekOfYearFromDate(firstItem.dateHarvested) < GsDate_weekOfYearFromDate(secondItem.dateHarvested);
    kHarvestReportSortByType:
      result := (firstItem.harvestItemTemplate.name < secondItem.harvestItemTemplate.name);
    kHarvestReportSortByPlant:
      result := (firstItem.plantName < secondItem.plantName);
    end;
  end;

function GsHarvestManager.firstLessThanSecondForAllSortFields(firstItem, secondItem: GsHarvestItem): boolean;
  var
    i: smallint;
  begin
  result := true;
  for i := 0 to kHarvestReportSortByLastChoice do
    begin
    if self.currentReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then
      break;
    if firstLessThanSecondSortFieldInfo(firstItem, secondItem, self.currentReport.sortByChoicesArray[i]) then
      exit;
    if firstGreaterThanSecondSortFieldInfo(firstItem, secondItem, self.currentReport.sortByChoicesArray[i]) then
      break;
    end;
  result := false;
  end;

function GsHarvestManager.firstGreaterThanSecondSortFieldInfo(
    firstItem, secondItem: GsHarvestItem; index: smallint): boolean;
  begin
  result := false;
  case index of
    kHarvestReportSortByYear:
      result :=
        GsDate_yearFromDate(firstItem.dateHarvested) > GsDate_yearFromDate(secondItem.dateHarvested);
    kHarvestReportSortBySoilPatch:
      result := (firstItem.soilPatchName > secondItem.soilPatchName);
    kHarvestReportSortByMonth:
      result :=
        GsDate_monthFromDate(firstItem.dateHarvested) > GsDate_monthFromDate(secondItem.dateHarvested);
    kHarvestReportSortByWeek:
      result :=
        GsDate_weekOfYearFromDate(firstItem.dateHarvested) > GsDate_weekOfYearFromDate(secondItem.dateHarvested);
    kHarvestReportSortByType:
      result := (firstItem.harvestItemTemplate.name > secondItem.harvestItemTemplate.name);
    kHarvestReportSortByPlant:
      result := (firstItem.plantName > secondItem.plantName);
    end;
  end;

function GsHarvestManager.firstGreaterThanSecondForAllSortFields(firstItem, secondItem: GsHarvestItem): boolean;
  var
    i: smallint;
  begin
  result := true;
  for i := 0 to kHarvestReportSortByLastChoice do
    begin
    if self.currentReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then
      break;
    if firstGreaterThanSecondSortFieldInfo(firstItem, secondItem, self.currentReport.sortByChoicesArray[i]) then
      exit;
    if firstLessThanSecondSortFieldInfo(firstItem, secondItem, self.currentReport.sortByChoicesArray[i]) then
      break;
    end;
  result := false;
  end;

procedure GsHarvestManager.fillHarvestItemListForPlant(listToFill: TList; plantProxy: GsGraphicalModel);
  var
    i: longint;
    plantName: string;
    harvestItem: GsHarvestItem;
  begin
  listToFill.clear;
  plantName := plantProxy.getName;
  if harvestItemList.count > 0 then
    for i := 0 to harvestItemList.count - 1 do
      begin
      harvestItem := GsHarvestItem(harvestItemList.items[i]);
      if plantName = harvestItem.plantName then
        listToFill.add(harvestItem);
      end;
  end;

procedure GsHarvestManager.sortHarvestListUsingHarvestReport(sortedList: TList; aReport: GsHarvestReport);
  var
    i: longint;
  begin
  sortedList.clear;
  sortedList.capacity := harvestItemList.count;
  if harvestItemList.count > 0 then
    for i := 0 to harvestItemList.count - 1 do
      sortedList.add(harvestItemList.items[i]);
  { sort new list }
  self.currentReport := aReport;
  self.currentSortList := sortedList;
  self.sortHarvestListByOneSortIndex(0, sortedList.count - 1);
  end;

procedure GsHarvestManager.sortHarvestListByOneSortIndex(left: integer; right: integer);
  var
    i, j: integer;
    lastItem: GsHarvestItem;
  begin
  if right > left then
  	begin
    i := left - 1;
    j := right;
    lastItem := GsHarvestItem(currentSortList.items[right]);
    while true do
      begin
      repeat
        i := i + 1;
      until not self.firstLessThanSecondForAllSortFields(
        GsHarvestItem(currentSortList.items[i]), lastItem);
      repeat
        j := j - 1;
      until (j < i) or (not self.firstGreaterThanSecondForAllSortFields(
        GsHarvestItem(currentSortList.items[j]), lastItem));
      if i >= j then break;
      currentSortList.exchange(i,j);
      end;
    currentSortList.exchange(i,right);
    self.sortHarvestListByOneSortIndex(left, j);
    self.sortHarvestListByOneSortIndex(i + 1, right);
    end;
	end;

procedure GsHarvestManager.fillReportGroupList(reportGroupList: TListCollection;
    sortedHarvestItemList: TList; aReport: GsHarvestReport);
  var
    i: longint;
    reportGroup: GsHarvestItemSortingGroup;
    harvestItem: GsHarvestItem;
    inGroup: boolean;
  begin
  reportGroupList.clear;  { frees contents }
  if sortedHarvestItemList.count > 0 then
    begin
    reportGroup := GsHarvestItemSortingGroup.createWithReportAndHarvestItem(
      aReport, GsHarvestItem(sortedHarvestItemList.items[0]), 0);
    reportGroupList.add(reportGroup);
    if sortedHarvestItemList.count > 1 then
      for i := 1 to sortedHarvestItemList.count - 1 do
        begin
        harvestItem := GsHarvestItem(sortedHarvestItemList.items[i]);
        inGroup := reportGroup.addHarvestItem(aReport, harvestItem);
        if not inGroup then
          begin
          reportGroup := GsHarvestItemSortingGroup.createWithReportAndHarvestItem(aReport, harvestItem, i);
          reportGroupList.add(reportGroup);
          end;
        end;
    end;
  end;

function GsHarvestManager.harvestItemTemplateInfoArrayDescription(index: integer): string;
  var unitString: string;
  begin
  result := self.harvestItemTemplateInfoArrayName(index);
  unitString := self.harvestItemTemplateInfoArrayUnit(index);
  if unitString <> '' then
    result := result + ' (' + unitString + ')';
  end;

function GsHarvestManager.harvestItemTemplateInfoArrayName(index: integer): string;
  begin
  result := '';
  case index of
    kHarvestReportShowCount:            result := 'Count';
    kHarvestReportShowBiomassWet_g:     result := 'Wet biomass';
    kHarvestReportShowBiomassDry_g:     result := 'Dry biomass';
    kHarvestReportShowRefuse_pct:       result := 'Refuse';
    kHarvestReportShowEnergy_cal:       result := 'Energy';
    kHarvestReportShowWater_pct:        result := 'Water content';
    kHarvestReportShowProtein_g:        result := 'Protein';
    kHarvestReportShowFat_g:            result := 'Fat';
    kHarvestReportShowCarbohydrate_g:   result := 'Carbohydrate';
    kHarvestReportShowCalcium_mg:       result := 'Calcium';
    kHarvestReportShowPhosphorus_mg:    result := 'Phosphorus';
    kHarvestReportShowIron_mg:          result := 'Iron';
    kHarvestReportShowSodium_mg:        result := 'Sodium';
    kHarvestReportShowPotassium_mg:     result := 'Potassium';
    kHarvestReportShowVitaminAValue_IU: result := 'Vitamin A value';
    kHarvestReportShowThiamin_mg:       result := 'Thiamin (B1)';
    kHarvestReportShowRiboflavin_mg:    result := 'Riboflavin (B2)';
    kHarvestReportShowNiacin_mg:        result := 'Niacin';
    kHarvestReportShowAscorbicAcid_mg:  result := 'Ascorbic acid (C)';
    kHarvestReportShowUserVariable1:    result := 'Custom 1';
    kHarvestReportShowUserVariable2:    result := 'Custom 2';
    end;
  end;

function GsHarvestManager.harvestItemTemplateInfoArrayUnit(index: integer): string;
  begin
  result := '';
  case index of
    kHarvestReportShowCount:            result := '';
    kHarvestReportShowBiomassWet_g:     result := 'g';
    kHarvestReportShowBiomassDry_g:     result := 'g';
    kHarvestReportShowRefuse_pct:       result := '%';
    kHarvestReportShowEnergy_cal:       result := 'cal';
    kHarvestReportShowWater_pct:        result := '%';
    kHarvestReportShowProtein_g:        result := 'g';
    kHarvestReportShowFat_g:            result := 'g';
    kHarvestReportShowCarbohydrate_g:   result := 'g';
    kHarvestReportShowCalcium_mg:       result := 'mg';
    kHarvestReportShowPhosphorus_mg:    result := 'mg';
    kHarvestReportShowIron_mg:          result := 'mg';
    kHarvestReportShowSodium_mg:        result := 'mg';
    kHarvestReportShowPotassium_mg:     result := 'mg';
    kHarvestReportShowVitaminAValue_IU: result := 'IU';
    kHarvestReportShowThiamin_mg:       result := 'mg';
    kHarvestReportShowRiboflavin_mg:    result := 'mg';
    kHarvestReportShowNiacin_mg:        result := 'mg';
    kHarvestReportShowAscorbicAcid_mg:  result := 'mg';
    kHarvestReportShowUserVariable1:    result := '';
    kHarvestReportShowUserVariable2:    result := '';
    end;
  end;

function GsHarvestManager.harvestItemTypeDescription(index: integer): string;
  begin
  result := '';
  case index of
    kHarvestPartTypeWholePlant: result := 'whole plant';
    kHarvestPartTypeSeedlingLeaf: result := 'seedling leaf';
    kHarvestPartTypeLeaf: result := 'leaf';
    kHarvestPartTypeFemaleInflorescence: result := 'female inflorescence';
    kHarvestPartTypeMaleInflorescence: result := 'male inflorescence';
    kHarvestPartTypeFemaleFlower: result := 'female flower';
    kHarvestPartTypeFemaleFlowerBud: result := 'female flower bud';
    kHarvestPartTypeMaleFlower: result := 'male flower';
    kHarvestPartTypeMaleFlowerBud: result := 'male flower bud';
    kHarvestPartTypeAxillaryBud: result := 'axillary bud';
    kHarvestPartTypeFruit: result := 'fruit';
    kHarvestPartTypeRoot: result := 'root';
    kHarvestPartTypeStem: result := 'stem';
    kHarvestPartTypeStorageOrgan: result := 'storage organ';
    end;
  end;

function GsHarvestManager.sortByChoicesArrayDescription(index: integer): string;
  begin
  result := '';
  case index of
    kHarvestReportSortByYear:           result := 'Year';
    kHarvestReportSortBySoilPatch:      result := 'Soil patch';
    kHarvestReportSortByMonth:          result := 'Month';
    kHarvestReportSortByWeek:           result := 'Week';
    kHarvestReportSortByType:           result := 'Type';
    kHarvestReportSortByPlant:          result := 'Plant';
    end;
  end;

{ GsHarvestItemSortingGroup }
constructor GsHarvestItemSortingGroup.createWithReportAndHarvestItem(aReport: GsHarvestReport;
    aHarvestItem: GsHarvestItem; index: longint);
  begin
  self.create;
  firstHarvestItem := aHarvestItem;
  firstHarvestItemIndex := index;
  numHarvestItems := 0;
  self.addHarvestItem(aReport, firstHarvestItem);
  end;

function GsHarvestItemSortingGroup.addHarvestItem(aReport: GsHarvestReport;
    aHarvestItem: GsHarvestItem): boolean;
  var
    i: smallint;
    newTotalBiomassDry_g: single;
  begin
  result := false;
  if aHarvestItem = nil then exit;
  { test that object is in group }
  for i := 0 to kHarvestReportSortByLastChoice do
    begin
    if aReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then break;
    if not Domain.harvestManager.equalSortFieldInfo(
      firstHarvestItem, aHarvestItem, aReport.sortByChoicesArray[i]) then
      exit;
    end;
  { add to totals }
  for i := 0 to kHarvestReportShowLastChoice do
    begin
   { if aReport.showChoicesArray[i] = kHarvestReportShowEndOfList then break; }
    if i <> kHarvestReportShowWater_pct then
      totals[i] := totals[i] + aHarvestItem.infoForIndex(i)
    else
      begin
      { calculate running mean of water percent based on amounts of dry biomass
        in the new harvest item relative to the dry biomass already accumulated }
      newTotalBiomassDry_g := totals[kHarvestReportShowBiomassDry_g] + aHarvestItem.biomass_g;
      if newTotalBiomassDry_g <> 0.0 then
        totals[i] := (totals[kHarvestReportShowWater_pct] * totals[kHarvestReportShowBiomassDry_g]
        + aHarvestItem.infoForIndex(kHarvestReportShowWater_pct) * aHarvestItem.biomass_g)
        / newTotalBiomassDry_g;
      end;
    end;
  result := true;
  inc(numHarvestItems);
  end;

function GsHarvestItemSortingGroup.textForSortIndex(index: smallint): string;
  begin
  result := '';
  case index of
    kHarvestReportSortByYear: result := GsDate_yearString(firstHarvestItem.dateHarvested);
    kHarvestReportSortBySoilPatch: result := firstHarvestItem.soilPatchName;
    kHarvestReportSortByMonth: result := GsDate_monthShortString(firstHarvestItem.dateHarvested);
    kHarvestReportSortByWeek: result := intToStr(GsDate_weekOfYearFromDate(firstHarvestItem.dateHarvested) + 1);
    kHarvestReportSortByType: result := firstHarvestItem.harvestItemTemplate.name;
    kHarvestReportSortByPlant: result := firstHarvestItem.plantName;
    end;
  end;

function GsHarvestItemSortingGroup.textForShowIndex(index: smallint): string;
  begin
  if index = kHarvestReportShowCount then
    { show count as integer }
    result := floatToStrF(totals[index], ffFixed, 7, 0)
  else
    result := digitValueString(totals[index]);
  end;

end.

{PDF FIX - could make more write lookup more efficient by storing index in object}
(* may not need this
procedure GsHarvestManager.streamReferenceToHarvestItem(filer: GsFiler; var harvestItem: GsHarvestItem);
  var reference: longint;
  begin
  if filer.isReading then
    begin
    filer.streamLongint(reference);
    if reference = -1 then
    	harvestItem := nil
    else
      begin
      if reference < self.harvestItemList.count then
    		harvestItem := GsHarvestItem(self.harvestItemList.items[reference])
      else
        begin
        harvestItem := nil;
        {raise Exception.create('harvest item ref outside range of harvest manager');}
        end;
      end;
    end
  else if filer.isWriting then
    begin
    if harvestItem = nil then
    	reference := -1
    else
      begin
      reference := self.harvestItemList.IndexOf(harvestItem);
      {if reference = -1 then
        raise Exception.create('harvest item not found in harvest manager'); }
      end;
    filer.streamLongint(reference);
    end;
  end;
*)


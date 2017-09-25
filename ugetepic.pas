unit Ugetepic;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugetepic: Epic import form. This form is kind of secret, because you can only access
it by opening the templates window then pressing Ctrl-Shift-E, at which point a button
appears. This is so the general user doesn't mess with it as it is not clean. It was
created mainly to aid us in using the EPIC weather and soil files and is not as
user-friendly as the rest of the program. For example, it does not recover well
when there is anything (at all) wrong during file input. The help system has a
description of the window in the EPIC section, under Auto Operations. We have
left the form around in the program for any soil scientists who have special
EPIC files they want to import. It does import plant data, but we have added
so many parameters to the plants that it's not advisable. It does not know
how to read the soils5 data files at this point. We have tried to put in defaults
for all non-EPIC parameters, but some may have slipped by as it was not a priority.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,
  uesoil, ueplant, ueweath, ucollect, utempman, ugsform;

const
  kEpicFileNone = 0;
  kEpicFileClimates = 1;
  kEpicFileSoilTypes = 2;
  kEpicFileCultivars = 3;
  kEpicFileSoils5SoilTypes = 4;

  kClimateNameLength = 60;
  kClimateLines = 15;
  kClimateWindLines = 19;
  kSoilTypeNumberLength = 16;
  kSoilTypeNameLength = 17;
  kSoilTypeLines = 22;
  kCultivarNumberLength = 2;
  kCultivarNameLength = 6;
  kCultivarLines = 5;

  kDefaultAlbedo = 0.13;

{ EPIC plant life cycle type (epic starts at 1) }
const
  kWarmSeasonAnnualLegume = 1;
  kColdSeasonAnnualLegume = 2;
  kPerennialLegume = 3;
  kWarmSeasonAnnual = 4;
  kColdSeasonAnnual = 5;
  kPerennial = 6;
  kTree = 7;

type
  TEpicImportForm = class(GsForm)
    fileItems: TListBox;
    closeWindow: TButton;
    importSelectedItems: TButton;
    topPanel: TPanel;
    fileTypeLabel: TLabel;
    dataFileName: TEdit;
    openFile: TButton;
    selectAll: TButton;
    importWholeDirectory: TButton;
    helpButton: TBitBtn;
    importingLabel: TLabel;
    importingLabel2: TLabel;
    procedure importSelectedItemsClick(Sender: TObject);
    procedure openFileClick(Sender: TObject);
    procedure topPanelResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure closeWindowClick(Sender: TObject);
    procedure selectAllClick(Sender: TObject);
    procedure importWholeDirectoryClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    fileType: smallint;
    fullFileName: string;
    currentTemplateManager: GsTemplateManager;
    procedure CreateSoilTypeTemplatesFromEpicFile;
    procedure CreateClimateTemplatesFromEpicFile;
    procedure CreateCultivarTemplatesFromEpicFile;
    function fileTypeFromFileName(fileName: string): smallint;
    function nameForFileType(fileType: smallint): string;
    procedure readTemplateNamesFromCurrentFile;
    function SoilTypeNameAndAlbedoFromFirstLine(var inputFile: TextFile; var albedo: single): string;
    function openFileFromName(fileNameWithPath: string): boolean;
  end;

implementation

{$R *.DFM}

uses usupport, udefault, ucursor, ueutils, uestruct, udebug;

var
  readOn: boolean;
  showMessageAtEnd: boolean;
  stopImporting: boolean;
  layerInits: SoilTypeLayerInitsStructureArray;


procedure TEpicImportForm.FormCreate(Sender: TObject);
  begin
  importWholeDirectory.enabled := false;
  showMessageAtEnd := true;
  end;

function readNumChars(var inputFile: TextFile; numChars: integer): string;
  var
    i: longint;
    newChar: char;
  begin
  result := '';
  for i := 1 to numChars do
    if not eof(inputFile) then
      begin
      newChar := chr(0);
      read(inputFile, newChar);
      if (newChar <> chr(13)) and (newChar <> chr(10)) then
        begin
        result := result + newChar;
        readOn := true;
        end
      else
        begin
        if (newChar = chr(13)) and not eof(inputFile) then
          begin
          read(inputFile, newChar);
          if newChar <> chr(10) then
            raise Exception.create('Problem in file input');
          readOn := false;
          end;
        break;
        end;
    end;
  end;

function floatFromChars(var inputFile: TextFile; numChars: integer): single;
  var
    theString: string;
  begin
  theString := readNumChars(inputFile, numChars);
  if length(trimLeftAndRight(theString)) = 0 then
    result := 0.0
  else
    begin
    try
      result := strToFloat(theString);
    except
      result := 0.0;
    end;
    end;
  end;

procedure TEpicImportForm.openFileClick(Sender: TObject);
  var
    fileNameWithPath: string;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeEpicDataFile, '');
  importWholeDirectory.enabled := self.openFileFromName(fileNameWithPath);
  end;

function TEpicImportForm.openFileFromName(fileNameWithPath: string): boolean;
  var
    newFileType: integer;
  begin
  result := false;
  if fileNameWithPath = '' then exit;
  newFileType := self.fileTypeFromFileName(fileNameWithPath);
  if newFileType = kEpicFileNone then
    begin
    { tell user cannot read file }
    showMessage('Could not read ' + lowerCase(extractFileName(fileNameWithPath)) + '; it is not a valid EPIC data file. '
      + chr(13) + 'Click the Help button on the window for details.');
    exit;
    end;
  try
    cursor_startWait;
    fileType := newFileType;
    fullFileName := fileNameWithPath;
    dataFileName.text := lowerCase(extractFileName(fullFileName));
    fileTypeLabel.caption := 'File (' + self.nameForFileType(fileType) + ')';
    self.topPanelResize(self);
    self.readTemplateNamesFromCurrentFile;
    result := true;
  finally
    cursor_stopWait;
  end;
  end;

function TEpicImportForm.fileTypeFromFileName(fileName: string): smallint;
  var
    firstLine, secondLine: string;
    inputFile: TextFile;
  begin
  result := kEpicFileNone;
  assignFile(inputFile, fileName);
  try
    reset(inputFile);
    if eof(inputFile) then exit;
    readln(inputFile, firstLine);
    { see if plant number in first 2 chars - if so, is cultivar file }
    if strToIntDef(copy(firstLine, 1, 2), 0) > 0 then
      result := kEpicFileCultivars
    else
      begin
      { see if soil number in first 16 chars - if so, is soil type file }
      if strToIntDef(copy(firstLine, 1, 16), 0) > 0 then
        result := kEpicFileSoilTypes
      else
        begin
        readln(inputFile, secondLine);
        if pos('LATT', upperCase(secondLine)) <> 0 then
          result := kEpicFileClimates;
        end;
      end;
  { not reading soils5 yet, so if not plant or soil or weather, return none }
  finally
    closeFile(inputFile);
  end;
  end;

function TEpicImportForm.nameForFileType(fileType: smallint): string;
  begin
  result := '';
  case fileType of
    kEpicFileNone: result := 'no type';
    kEpicFileClimates: result := 'climates';
    kEpicFileSoilTypes: result := 'soil types';
    kEpicFileCultivars: result := 'cultivars';
    kEpicFileSoils5SoilTypes: result := 'soils 5 soil types';
    end;
  end;

procedure skipLines(numLines: smallint; var inputFile: TextFile);
  var i: smallint;
  begin
  for i := 0 to numLines - 1 do
    begin
    readln(inputFile);
    if eof(inputFile) then exit;
    end;
  end;

function cleanUpName(name: string): string;
  begin
  result := lowerCase(trimLeftAndRight(name));
  result := upperCase(copy(result, 1, 1)) + copy(result, 2, length(result));
  end;

function cleanUpClimateName(name: string): string;
  begin
  result := lowerCase(trimLeftAndRight(name));
  { capitalize first two letters (state), add comma, and capitalize first name of town after state }
  result := upperCase(copy(result, 1, 2)) + ', ' + upperCase(copy(result, 4, 1)) + copy(result, 5, length(result));
  end;

procedure TEpicImportForm.readTemplateNamesFromCurrentFile;
  var
    ignore, stateAbbrev: string;
    inputFile: TextFile;
    albedo: single;
    climateName: string;
  begin
  assignFile(inputFile, fullFileName);
  fileItems.clear;
  try
    reset(inputFile);
    while not eof(inputFile) do
      begin
        case fileType of
          kEpicFileClimates:
            begin
            { kClimateNameLength takes whole first line }
            readln(inputFile, climateName);
            climateName := cleanUpClimateName(climateName);
            fileItems.items.add(climateName);
            { skip over rest of climate }
            skipLines(kClimateLines - 1, inputFile);
            end;
          kEpicFileSoilTypes:
            begin
            ignore := readNumChars(inputFile, kSoilTypeNumberLength);
            if eof(inputFile) then break;
            { don't care about albedo here, but function sets it - this line is to stop a warning in Delphi 2.0 }
            albedo := kDefaultAlbedo;
            fileItems.items.add(self.SoilTypeNameAndAlbedoFromFirstLine(inputFile, albedo));
            { skip over rest of soil type }
            skipLines(kSoilTypeLines - 1, inputFile);
           end;
          kEpicFileCultivars:
            begin
            ignore := readNumChars(inputFile, kCultivarNumberLength);
            if eof(inputFile) then break;
            fileItems.items.add(lowerCase(trimLeftAndRight(readNumChars(inputFile, kCultivarNameLength))));
            { skip over rest of cultivar }
            readln(inputFile);
            skipLines(kCultivarLines - 1, inputFile);
            end;
          kEpicFileSoils5SoilTypes: ;
          end;
      end;
  finally
    closeFile(inputFile);
  end;
  end;

procedure TEpicImportForm.importSelectedItemsClick(Sender: TObject);
  begin
  cursor_startWait;
  try
    case fileType of
      kEpicFileClimates: self.CreateClimateTemplatesFromEpicFile;
      kEpicFileSoilTypes: self.CreateSoilTypeTemplatesFromEpicFile;
      kEpicFileCultivars: self.CreateCultivarTemplatesFromEpicFile;
      kEpicFileSoils5SoilTypes:  ;
      end;
  finally
    cursor_stopWait;
  end;
  end;

function TEpicImportForm.SoilTypeNameAndAlbedoFromFirstLine(var inputFile: TextFile; var albedo: single): string;
  var
    name, letter, ignore, stateAbbrev: string;
  begin
  result := '';
  readOn := true;
  name := lowerCase((readNumChars(inputFile, kSoilTypeNameLength - 1)));
  letter := '';
  if readOn then letter := readNumChars(inputFile, 1);
  { in some cases, last two chars of name are used by a letter and a slash before the last letter }
  if pos('/', name) <> 0 then
    begin
    letter := copy(name, length(name) - 1, 2) + letter;
    name := copy(name, 1, length(name) - 2);
    end;
  name := cleanUpName(name);
  letter := lowerCase(letter);
  albedo := kDefaultAlbedo; { default in case not read correctly }
  albedo := floatFromChars(inputFile, 6);
  if albedo = 0.0 then albedo := kDefaultAlbedo;
  { five floats }
  if readOn then ignore := readNumChars(inputFile, 6 * 5);
  { state abbreviation - add to soil type name }
  stateAbbrev := '';
  if readOn then stateAbbrev := trimLeftAndRight(readNumChars(inputFile, 4));
  result := name;
  { not using letter in name - don't know what it means }
  if (letter <> '') and (letter <> ' ') then result := result + '-' + letter;
  if stateAbbrev <> '' then result := result + ', ' + stateAbbrev;
  if readOn then readln(inputFile);
  end;

procedure TEpicImportForm.CreateSoilTypeTemplatesFromEpicFile;
  var
    ignore, stateAbbrev, soilTypeName: string;
    soilPatch: GsSoilPatch;
    inputFile: TextFile;
    i, index: longint;
    albedo: single;
  begin
  assignFile(inputFile, fullFileName);
  try
    reset(inputFile);
    while not eof(inputFile) do
      begin
      { 16 chars - soil type number - we don't need }
      ignore := readNumChars(inputFile, kSoilTypeNumberLength);
      soilTypeName := '';
      albedo := kDefaultAlbedo; { default if not read in }
      soilTypeName := self.SoilTypeNameAndAlbedoFromFirstLine(inputFile, albedo);
      index := fileItems.items.indexOf(soilTypeName);
      if (index = -1) or (not fileItems.selected[index]) then
        begin
        { skip over rest of soil type }
        skipLines(kSoilTypeLines - 1, inputFile);
        continue;
        end;
      soilPatch := GsSoilPatch.create;
      currentTemplateManager.soilTypeList.add(soilPatch);
      soilPatch.soilTypeName := soilTypeName;
      soilPatch.setName(soilPatch.soilTypeName);
      soilPatch.params.soilAlbedo_frn := albedo;
      { read rest of lines for soil type }
      readOn := true;
      Defaults_ZeroLayerInits(layerInits);
      for i := 0 to 9 do if readOn then soilPatch.layers[i].depth_m := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].settledBulkDensity_tPm3 := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then layerInits[i].initWiltingPoint_mPm := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then layerInits[i].initFieldCapacity_mPm := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].sandContent_pct := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].siltContent_pct := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then layerInits[i].initOrganicNTotalHumus_gPt := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].soilpH := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].baseFormingCations_cmolPkg := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then layerInits[i].initOrganicC_pct := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].calciumCarbonate_pct := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].cationExchangeCapacity_cmolPkg := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].rockContent_pct := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then layerInits[i].initNitrate_gPt := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then layerInits[i].initLabileP_gPt := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].flatCropResidue_tPha := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].bulkDensityOvenDry_tPm3 := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].pSorptionCoeff_frn := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 9 do if readOn then soilPatch.layers[i].saturatedConductivity_mmPhr := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      readln(inputFile); { line not used by epic }
      for i := 0 to 9 do if readOn then layerInits[i].initOrganicPHumus_gPt := floatFromChars(inputFile, 8);
      if readOn then readln(inputFile); readOn := true;
      Defaults_DefaultAndCalculateSoilInputs(soilPatch, layerInits);
      end;
  finally
    closeFile(inputFile);
    if showMessageAtEnd then showMessage('File read.');
  end;
  end;

procedure TEpicImportForm.CreateClimateTemplatesFromEpicFile;
  var
    ignore, climateName, fileName, windFileName, prompt: string;
    climate: GsWeather;
    inputFile, windFile: TextFile;
    i, j, index: longint;
    readWindData: boolean;
    latitude_deg, longitude_deg: single;
  begin
  { look for wind file - if not found, ask user if they want to go on anyway }
  fileName := extractFileName(fullFileName);
  windFileName := extractFilePath(fullFileName) + 'wind' + copy(fileName, 5, length(fileName));
  if not fileExists(windFileName) then
    begin
    readWindData := false;
    prompt := 'The wind file for ' + lowerCase(fileName) + ' ('
      + lowerCase(extractFileName(windFileName)) + ') could not be found. ' + chr(13)
      + 'It should be in the directory ' + lowerCase(extractFilePath(windFileName)) + '. ' + chr(13)
      + ' Do you want to use the climate data in ' + lowerCase(fileName) + ' without wind data?';
    if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
    end
  else
    readWindData := true;
  assignFile(inputFile, fullFileName);
  if readWindData then assignFile(windFile, windFileName);
  try
    reset(inputFile);
    if readWindData then reset(windFile);
    while not eof(inputFile) do
      begin
      { first line is name only }
      readln(inputFile, climateName);
      climateName := cleanUpClimateName(climateName);
      index := fileItems.items.indexOf(climateName);
      if (index = -1) or (not fileItems.selected[index]) then
        begin
        { skip over rest of climate }
        skipLines(kClimateLines - 1, inputFile);
        { skip whole climate in wind file }
        if readWindData then skipLines(kClimateWindLines, windFile);
        continue;
        end;
      climate := GsWeather.create;
      currentTemplateManager.climateList.add(climate);
      climate.setName(climateName);
      { read second line - latitude, longitude, elevation of weather station }
      readOn := true;
      ignore := readNumChars(inputFile, 9); { latitude label }
      if readOn then latitude_deg := floatFromChars(inputFile, 6) else latitude_deg := 0;
      climate.params.climateLatitude_rad := Utils_DegreesToRadians(latitude_deg);
      if readOn then ignore := readNumChars(inputFile, 7); { longitude label }
      if readOn then longitude_deg := floatFromChars(inputFile, 6); { not using longitude }
      if readOn then ignore := readNumChars(inputFile, 8); { elevation label }
      if readOn then climate.params.climateElevation_m := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile);
      { read rest of lines in main weather file }
      readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.dailyMeanMaxTempForMonth_degC[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.dailyMeanMinTempForMonth_degC[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.stdDevMaxTempForMonth_degC[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.stdDevMinTempForMonth_degC[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.meanTotalRainfallForMonth_mm[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.stdDevDailyRainfallForMonth_mm[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.skewCoeffForRainfallForMonth[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.probWetDayAfterDryDayForMonth_frn[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.probWetDayAfterWetDayForMonth_frn[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.numWetDaysForMonth[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.meanPropRainInFirstHalfHourForMonth_frn[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.dailyMeanRadiationForMonth_MJPm2[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      for i := 0 to 11 do if readOn then
        climate.params.dailyMeanRelHumForMonth_frn[i] := floatFromChars(inputFile, 6);
      if readOn then readln(inputFile); readOn := true;
      if readWindData then
        begin
        { read wind data from wind file }
        { note: wind and normal data must be exactly matching in the files }
        { skip over first two lines in wind file }
        readln(windFile);
        readln(windFile);
        { read wind speed by month }
        readOn := true;
        for i := 0 to 11 do if readOn then
          climate.params.dailyMeanWindSpeedForMonth_mPsec[i] := floatFromChars(windFile, 6);
        if readOn then readln(windFile);
        { read wind directions (16) by month }
        for j := 0 to 15 do
          begin
          readOn := true;
          for i := 0 to 11 do if readOn then
            climate.params.windDirectionsForMonth_frn[i][j] := floatFromChars(windFile, 6);
          if readOn then readln(windFile);
          end;
        end;
      try
        Defaults_DefaultAndCalculateWeatherInputs(climate);
      except
        showMessage('Problem in defaulting climate.');
      end;
      end;
  finally
    closeFile(inputFile);
    closeFile(windFile);
    if showMessageAtEnd then showMessage('File read.');
  end;
  end;

function leftPartOfSplitVar(splitVar: single): single;
  begin
  try
    result := trunc(splitVar);
  except
    result := 0;
  end;
  end;

function rightPartOfSplitVar(splitVar: single): single;
  var
    leftPart: longint;
    leftPartAsFloat: single;
  begin
  leftPart := trunc(leftPartOfSplitVar(splitVar));
  leftPartAsFloat := leftPart;
  result := splitVar - leftPartAsFloat;
  end;

procedure splitUpVars(var splitVarArray: arrayFour);
  var temp: array[0..1] of single;
  begin
  temp[0] := splitVarArray[0];
  temp[1] := splitVarArray[1];
  splitVarArray[0] := leftPartOfSplitVar(temp[0]);
  splitVarArray[1] := rightPartOfSplitVar(temp[0]);
  splitVarArray[2] := leftPartOfSplitVar(temp[1]);
  splitVarArray[3] := rightPartOfSplitVar(temp[1]);
  end;

procedure writePlantReportLabels(var reportFile: TextFile);
  begin
  write(reportFile, 'name' + chr(9));
  write(reportFile, 'biomassToEnergyRatio_kgPhaPMJ' + chr(9));
  write(reportFile, 'potentialHarvestIndex_frn' + chr(9));
  write(reportFile, 'optimalTemp_degC' + chr(9));
  write(reportFile, 'baseTemp_degC' + chr(9));
  write(reportFile, 'maxLeafAreaIndex' + chr(9));
  write(reportFile, 'fractionOfGrowingSeasonWhenLeafDeclineStarts_frn' + chr(9));
  write(reportFile, 'heatUnitFactorParamsForLAI.x1' + chr(9));
  write(reportFile, 'heatUnitFactorParamsForLAI.y1' + chr(9));
  write(reportFile, 'heatUnitFactorParamsForLAI.x2' + chr(9));
  write(reportFile, 'heatUnitFactorParamsForLAI.y2' + chr(9));
  write(reportFile, 'leafAreaIndexDeclineRateFactor' + chr(9));
  write(reportFile, 'biomassAdjustmentIfLAIDecliningFactor' + chr(9));
  write(reportFile, 'aluminumTolerance_pct' + chr(9));
  write(reportFile, 'leafResistIfVPDBelowThreshold_mPsec' + chr(9));
  write(reportFile, 'criticalAerationFactor_frn' + chr(9));
  write(reportFile, 'maxHeightUnsupported_m' + chr(9));
  write(reportFile, 'maxRootDepth_m' + chr(9));
  write(reportFile, 'biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ' + chr(9));
  write(reportFile, 'minCropManagementFactor' + chr(9));
  write(reportFile, 'nInYield_kgPkg' + chr(9));
  write(reportFile, 'pInYield_kgPkg' + chr(9));
  write(reportFile, 'minHarvestIndex_frn' + chr(9));
  write(reportFile, 'minPestWeedDiseaseFactor_frn' + chr(9));
  write(reportFile, 'waterInYield_frn' + chr(9));
  write(reportFile, 'nFractionAtEmergence_frn' + chr(9));
  write(reportFile, 'nFractionAtHalfMaturity_frn' + chr(9));
  write(reportFile, 'nFractionAtMaturity_frn' + chr(9));
  write(reportFile, 'pFractionAtEmergence_frn' + chr(9));
  write(reportFile, 'pFractionAtHalfMaturity_frn' + chr(9));
  write(reportFile, 'pFractionAtMaturity_frn' + chr(9));
  write(reportFile, 'windErosionFactorStandingLive' + chr(9));
  write(reportFile, 'windErosionFactorStandingDead' + chr(9));
  write(reportFile, 'windErosionFactorFlatResidue' + chr(9));
  write(reportFile, 'lifeCycleClimateType' + chr(9));
  write(reportFile, 'frostReductionFactorParams.x1' + chr(9));
  write(reportFile, 'frostReductionFactorParams.y1' + chr(9));
  write(reportFile, 'frostReductionFactorParams.x2' + chr(9));
  write(reportFile, 'frostReductionFactorParams.y2' + chr(9));
  write(reportFile, 'biomassToEnergyRatioVPDParam' + chr(9));
  write(reportFile, 'thresholdVaporPressureDeficit_kPa' + chr(9));
  write(reportFile, 'fractionOfMaxLeafConductForHighVPD.x' + chr(9));
  write(reportFile, 'fractionOfMaxLeafConductForHighVPD.y' + chr(9));
  write(reportFile, 'fractionRootWtAtEmergence_frn' + chr(9));
  write(reportFile, 'fractionRootWtAtMaturity_frn' + chr(9));
  writeln(reportFile);
  end;
  
procedure writePlantReportForCultivar(cultivar: GsPlant; var reportFile: TextFile);
  begin
  with cultivar do
  begin
  write(reportFile, getName + chr(9));
  write(reportFile, floatToStr(params.biomassToEnergyRatio_kgPhaPMJ) + chr(9));
  write(reportFile, floatToStr(params.optimalTemp_degC) + chr(9));
  write(reportFile, floatToStr(params.baseTemp_degC) + chr(9));
  write(reportFile, floatToStr(params.maxLeafAreaIndex) + chr(9));
  write(reportFile, floatToStr(params.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn) + chr(9));
  write(reportFile, floatToStr(params.heatUnitFactorParamsForLAI.x1) + chr(9));
  write(reportFile, floatToStr(params.heatUnitFactorParamsForLAI.y1) + chr(9));
  write(reportFile, floatToStr(params.heatUnitFactorParamsForLAI.x2) + chr(9));
  write(reportFile, floatToStr(params.heatUnitFactorParamsForLAI.y2) + chr(9));
  write(reportFile, floatToStr(params.leafAreaIndexDeclineRateFactor) + chr(9));
  write(reportFile, floatToStr(params.biomassAdjustmentIfLAIDecliningFactor) + chr(9));
  write(reportFile, floatToStr(params.aluminumTolerance_pct) + chr(9));
  write(reportFile, floatToStr(params.leafResistIfVPDBelowThreshold_mPsec) + chr(9));
  write(reportFile, floatToStr(params.criticalAerationFactor_frn) + chr(9));
  write(reportFile, floatToStr(params.maxHeightUnsupported_m) + chr(9));
  write(reportFile, floatToStr(params.maxRootDepth_m) + chr(9));
  write(reportFile, floatToStr(params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ) + chr(9));
  write(reportFile, floatToStr(params.minCropManagementFactor) + chr(9));
  write(reportFile, floatToStr(params.nFractionAtEmergence_frn) + chr(9));
  write(reportFile, floatToStr(params.nFractionAtHalfMaturity_frn) + chr(9));
  write(reportFile, floatToStr(params.nFractionAtMaturity_frn) + chr(9));
  write(reportFile, floatToStr(params.pFractionAtEmergence_frn) + chr(9));
  write(reportFile, floatToStr(params.pFractionAtHalfMaturity_frn) + chr(9));
  write(reportFile, floatToStr(params.pFractionAtMaturity_frn) + chr(9));
  write(reportFile, floatToStr(params.windErosionFactorStandingLive) + chr(9));
  write(reportFile, floatToStr(params.windErosionFactorStandingDead) + chr(9));
  write(reportFile, floatToStr(params.windErosionFactorFlatResidue) + chr(9));
  write(reportFile, floatToStr(params.frostReductionFactorParams.x1) + chr(9));
  write(reportFile, floatToStr(params.frostReductionFactorParams.y1) + chr(9));
  write(reportFile, floatToStr(params.frostReductionFactorParams.x2) + chr(9));
  write(reportFile, floatToStr(params.frostReductionFactorParams.y2) + chr(9));
  write(reportFile, floatToStr(params.biomassToEnergyRatioVPDParam) + chr(9));
  write(reportFile, floatToStr(params.thresholdVaporPressureDeficit_kPa) + chr(9));
  write(reportFile, floatToStr(params.fractionOfMaxLeafConductForHighVPD.x) + chr(9));
  write(reportFile, floatToStr(params.fractionOfMaxLeafConductForHighVPD.y) + chr(9));
  write(reportFile, floatToStr(params.fractionRootWtAtEmergence_frn) + chr(9));
  write(reportFile, floatToStr(params.fractionRootWtAtMaturity_frn) + chr(9));
  writeln(reportFile);
  end;
  end;
  
procedure TEpicImportForm.CreateCultivarTemplatesFromEpicFile;
  var
    ignore, cultivarName: string;
    cultivar: GsPlant;
    inputFile, reportFile: TextFile;
    i, index: longint;
    anArray: arrayFour;
    splitVar, leftSide, ignoreFloat, alToxicitySensitivity_1to5: single;
    reporting: boolean;
    lifeCycleClimateType: smallint;
  begin
  assignFile(inputFile, fullFileName);
  { IMPORTANT - change this line to use the reporting feature }
  reporting := false;
  { IMPORTANT - change this line to use the reporting feature }
  if reporting then assignFile(reportFile, 'plants.tab');
  try
    reset(inputFile);
    if reporting then
      begin
      rewrite(reportFile);
      writePlantReportLabels(reportFile);
      end;
    while not eof(inputFile) do
      begin
      { LINE 1 }
      { 2 chars - cultivar number - we don't need }
      readOn := true;
      ignore := readNumChars(inputFile, kCultivarNumberLength);
      cultivarName := '';
      if readOn then cultivarName := lowerCase(trimLeftAndRight(readNumChars(inputFile, kCultivarNameLength)));
      index := fileItems.items.indexOf(cultivarName);
      if (index = -1) or (not fileItems.selected[index]) then
        begin
        { skip over rest of cultivar }
        if readOn then
          skipLines(kCultivarLines, inputFile)
        else
          skipLines(kCultivarLines - 1, inputFile);
        continue;
        end;
      cultivar := GsPlant.create;
      currentTemplateManager.cultivarList.add(cultivar);
      cultivar.setName(cultivarName);
      with cultivar do
        begin
        { line 1 after name }
        if readOn then params.biomassToEnergyRatio_kgPhaPMJ := floatFromChars(inputFile, 8);
        if readOn then { potential harvest index - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then params.optimalTemp_degC := floatFromChars(inputFile, 8);
        if readOn then params.baseTemp_degC := floatFromChars(inputFile, 8);
        if readOn then params.maxLeafAreaIndex := floatFromChars(inputFile, 8);
        if readOn then params.fractionOfGrowingSeasonWhenLeafDeclineStarts_frn := floatFromChars(inputFile, 8);
        { params for this s curve are split vars - read, then split up }
        if readOn then anArray[0] := floatFromChars(inputFile, 8) else anArray[0] := 0;
        if readOn then anArray[1] := floatFromChars(inputFile, 8) else anArray[1] := 0;
        anArray[2] := 0;
        anArray[3] := 0;
        splitUpVars(anArray);
        { extra thing because left side goes from 0 to 1 }
        anArray[0] := anArray[0] * 0.01;
        anArray[2] := anArray[2] * 0.01;
        { defaults if zero - CFK fix - these defaults are from corn }
        if anArray[0] = 0 then anArray[0] := 0.15;
        if anArray[1] = 0 then anArray[1] := 0.05;
        if anArray[2] = 0 then anArray[2] := 0.5;
        if anArray[3] = 0 then anArray[3] := 0.95;
        Utils_InitSCurveParam(params.heatUnitFactorParamsForLAI, anArray[0], anArray[1], anArray[2], anArray[3]);
        { heatUnitFactorParamsForLAI: X is proportion of growing season (HUI); Y is fraction of max potential LAI.
        0.15 of growing season (HUI=0.15) gives 0.05 of max LAI; 0.5 of growing season gives 0.95 of max LAI
        Note that EPIC has % and divides by 100 to get proportion for X variable (it can't be prop because is on left side).
        For this and several other s curves I have changed them so our input will be as a fraction. }
        { rest of line 1 }
        if readOn then params.leafAreaIndexDeclineRateFactor := floatFromChars(inputFile, 8);
        if readOn then readln(inputFile);
        { LINE 2 }
        readOn := true;
        params.biomassAdjustmentIfLAIDecliningFactor := floatFromChars(inputFile, 8);
        alToxicitySensitivity_1to5 := 5;
        if readOn then alToxicitySensitivity_1to5 := floatFromChars(inputFile, 8);
        params.aluminumTolerance_pct := 10.0 + 20.0 * (alToxicitySensitivity_1to5 - 1.0);
        if readOn then params.leafResistIfVPDBelowThreshold_mPsec := floatFromChars(inputFile, 8);
        if readOn then params.criticalAerationFactor_frn := floatFromChars(inputFile, 8);
        if readOn then { seeding rate - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then params.maxHeightUnsupported_m := floatFromChars(inputFile, 8);
        if readOn then params.maxRootDepth_m := floatFromChars(inputFile, 8);
        { this s curve comes from biomassToEnergyRatio_kgPhaPMJ, a 330 constant, and one split var }
        if readOn then splitVar := floatFromChars(inputFile, 8) else splitVar := 0;
        { right part of split var must be multiplied by 100 (since it is on the right side) }
        params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := rightPartOfSplitVar(splitVar) * 100.0;
        leftSide := leftPartOfSplitVar(splitVar);
        if params.biomassToEnergyRatio_kgPhaPMJ = 0 then
          params.biomassToEnergyRatio_kgPhaPMJ := 40.0;
        if params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ = 0 then
          params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ := 44.0; { cfk fix - from corn }
        if leftSide = 0 then leftSide := 660.0;
        Utils_InitSCurveParam(params.biomassToEnergyRatioCO2Params,
          330.0, params.biomassToEnergyRatio_kgPhaPMJ * 0.01,
          leftSide, params.biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ * 0.01);
        { biomassToEnergyRatioCO2Params: X is CO2 concentration, Y is radiation use efficiency / 100.
        330 ul/l CO2 (hard-coded) gives biomassToEnergyRatio_kgPhaPMJ / 100;
        660 ul/l CO2 (hard-coded) gives biomassToEnergyRatioAtElevatedCO2_kgPhaPMJ / 100 (these are both parameters).
        Dividing by 100 moves the equation into the normal form for s curves where Y goes from 0 to 1;
        the result of the scurve function is multiplied by 100 to get the biomass conversion factor for each day.
        Note: we changed this from inputting an s curve and ignoring the x1 value to inputting only one parameter,
        biomassToEnergyRatio at 660 ul/l. This hard-codes the 660, but is more understandable to the user than a partial
        s curve. }
        if readOn then params.minCropManagementFactor := floatFromChars(inputFile, 8);
        if readOn then { N in yield - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then readln(inputFile);
        { LINE 3 }
        readOn := true;
        { P in yield - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then { min harvest index - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then { min pest factor - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then { seed cost - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then { price for yield - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then { water in yield - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then params.nFractionAtEmergence_frn := floatFromChars(inputFile, 8);
        if readOn then params.nFractionAtHalfMaturity_frn := floatFromChars(inputFile, 8);
        if readOn then params.nFractionAtMaturity_frn := floatFromChars(inputFile, 8);
        if readOn then params.pFractionAtEmergence_frn := floatFromChars(inputFile, 8);
        if readOn then readln(inputFile);
        { LINE 4 }
        readOn := true;
        params.pFractionAtHalfMaturity_frn := floatFromChars(inputFile, 8);
        if readOn then params.pFractionAtMaturity_frn := floatFromChars(inputFile, 8);
        if readOn then params.windErosionFactorStandingLive := floatFromChars(inputFile, 8);
        if readOn then params.windErosionFactorStandingDead := floatFromChars(inputFile, 8);
        if readOn then params.windErosionFactorFlatResidue := floatFromChars(inputFile, 8);
        if readOn then
          begin
          lifeCycleClimateType := round(floatFromChars(inputFile, 8));
          case lifeCycleClimateType of
            kWarmSeasonAnnualLegume, kColdSeasonAnnualLegume:
              begin
              params.lifeCycleType := kAnnual;
              params.isLegume := true;
              end;
            kPerennialLegume:
              begin
              params.lifeCycleType := kPerennial;
              params.isLegume := true;
              end;
            kWarmSeasonAnnual, kColdSeasonAnnual: params.lifeCycleType := kAnnual;
            kPerennial: params.lifeCycleType := kPerennial;
            kTree:
              begin
              params.lifeCycleType := kPerennial;
              params.isTree := true;
              end;
            end;
          end;
        { s curve }
        if readOn then anArray[0] := floatFromChars(inputFile, 8) else anArray[0] := 0;
        if readOn then anArray[1] := floatFromChars(inputFile, 8) else anArray[1] := 0;
        anArray[2] := 0;
        anArray[3] := 0;
        splitUpVars(anArray);
        { these defaults are from corn }
        if anArray[0] = 0 then anArray[0] := 5.0; { CFK fix - these defaults are from corn }
        if anArray[1] = 0 then anArray[1] := 0.01;
        if anArray[2] = 0 then anArray[2] := 15.0;
        if anArray[3] = 0 then anArray[3] := 0.95;
        Utils_InitSCurveParam(params.frostReductionFactorParams, anArray[0], anArray[1], anArray[2], anArray[3]);
        { frostReductionFactorParams: X is min temp in NEGative deg C, Y is fraction of biomass lost each day this temp occurs.
        No frost damage is assumed to occur at a min temp over 0 deg C.
        Current parameters (-5 to -15) are likely to underestimate frost damage.
        -5 deg C gives 0.01 reduction in biomass; -15 deg C gives 0.95 reduction in biomass }
        if readOn then params.biomassToEnergyRatioVPDParam := floatFromChars(inputFile, 8);
        if readOn then params.thresholdVaporPressureDeficit_kPa := floatFromChars(inputFile, 8);
        if readOn then readln(inputFile);
        { LINE 5 }
        readOn := true;
        { one split var }
        splitVar := floatFromChars(inputFile, 8);
        params.fractionOfMaxLeafConductForHighVPD.x := leftPartOfSplitVar(splitVar);
        params.fractionOfMaxLeafConductForHighVPD.y := rightPartOfSplitVar(splitVar);
        if params.fractionOfMaxLeafConductForHighVPD.x = 0 then
          params.fractionOfMaxLeafConductForHighVPD.x := 4.0;
        if params.fractionOfMaxLeafConductForHighVPD.y = 0 then
          params.fractionOfMaxLeafConductForHighVPD.y := 0.75;
        if readOn then { crop id number - not using } ignoreFloat := floatFromChars(inputFile, 8);
        if readOn then params.fractionRootWtAtEmergence_frn := floatFromChars(inputFile, 8);
        if readOn then params.fractionRootWtAtMaturity_frn := floatFromChars(inputFile, 8);
        if readOn then readln(inputFile);
        end;
      if reporting then writePlantReportForCultivar(cultivar, reportFile);
      Defaults_DefaultAndCalculatePlantInputs(cultivar);
      end;
  finally
    closeFile(inputFile);
    if reporting then closeFile(reportFile);
    if showMessageAtEnd then showMessage('File read.');
  end;
  end;

const
  kBetweenGap = 4;

procedure TEpicImportForm.FormResize(Sender: TObject);
  var newWidth, newTop: longint;
  begin
  closeWindow.left := self.clientWidth - closeWindow.width - kBetweenGap;
  importSelectedItems.left := closeWindow.left;
  selectAll.left := closeWindow.left;
  importWholeDirectory.left := closeWindow.left;
  helpButton.left := closeWindow.left;
  newWidth := self.clientWidth - closeWindow.width - kBetweenGap * 3;
  with topPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, dataFileName.height + kBetweenGap * 2);
  newTop := topPanel.top + topPanel.height + kBetweenGap;
  with fileItems do setBounds(kBetweenGap, newTop, newWidth, self.clientHeight - newTop - kBetweenGap);
  end;

procedure TEpicImportForm.topPanelResize(Sender: TObject);
  begin
  with fileTypeLabel do setBounds(kBetweenGap, topPanel.height div 2 - height div 2, width, height);
  with openFile do setBounds(topPanel.width - openFile.width - kBetweenGap, kBetweenGap, width, height);
  with dataFileName do setBounds(fileTypeLabel.left + fileTypeLabel.width + kBetweenGap,
    kBetweenGap, topPanel.width - fileTypeLabel.width - openFile.width - kBetweenGap * 4, height);
  end;

procedure TEpicImportForm.closeWindowClick(Sender: TObject);
  begin
  modalResult := mrOK;
  end;

procedure TEpicImportForm.selectAllClick(Sender: TObject);
  var i: longint;
  begin
  if fileItems.items.count > 0 then
    for i := 0 to fileItems.items.count - 1 do
      fileItems.selected[i] := true;
  end;

procedure TEpicImportForm.importWholeDirectoryClick(Sender: TObject);
  var
    searchRec: TSearchRec;
    directoryName: string;
    returnCode: integer;
  begin
  directoryName := extractFilePath(fullFileName);
  if messageDlg('About to import all files in ' + directoryName + '. Proceed?',
      mtConfirmation, [mbOK, mbCancel], 0) <> idOk then
    exit;
  stopImporting := false;
  showMessageAtEnd := false;
  importingLabel.visible := true;
  importingLabel2.visible := true;
  self.invalidate;
  Application.processMessages;
  returnCode := SysUtils.findFirst(directoryName + '*.*', 0, searchRec);
  if returnCode = 0 then          
    begin
    if self.openFileFromName(directoryName + searchRec.name) then
      begin
      self.invalidate;
      Application.processMessages;
      self.selectAllClick(self);
      self.importSelectedItemsClick(self);
      end;
    end
  else
    begin
    showMessage('No valid files in directory.');
    exit;
    end;
  returnCode := 0;
  while returnCode = 0 do
    begin
    Application.processMessages;
    if stopImporting then break;
    returnCode := SysUtils.findNext(searchRec);
    if returnCode = 0 then
      begin
      if self.openFileFromName(directoryName + searchRec.name) then
        begin
        self.invalidate;
        Application.processMessages;
        self.selectAllClick(self);
        self.importSelectedItemsClick(self);
        end;
      end;
    end;
  SysUtils.findClose(searchRec);
  importingLabel.visible := false;
  importingLabel2.visible := false;
  showMessageAtEnd := true;
  end;

procedure TEpicImportForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('epic_EPIC_import')
  end;

procedure TEpicImportForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  if (ssCtrl in shift) and (lowerCase(chr(key)) = 's') then
    stopImporting := true;
  end;

end.

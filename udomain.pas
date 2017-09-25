unit UDomain;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udomain: Important file. The domain works somewhat like a document, keeping track
of everything in the garden file. At this point there can only be one domain at
a time (therefore the global var 'domain'). Several menu options and other options
set in dialogs (such as the display dialog) are kept here as well as a pointer to
the garden. The domain handles saving and loading of the garden file, and the loading
of all ancillary files (palette, hints, aspect hints, templates, groups) at startup.
It also does aspect creation. The domain is used all over the program to access things
in the garden (e.g., domain.garden.weather)}

interface

uses SysUtils, WinTypes, ucollect, WinProcs, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
      ufiler, uaspects, umconsts, umodel, uegarden,
      ugroups, utools, utempman, uharvprt, uturt3d, ubitmap, uwindman, uhints;
const
	kGetField = 0;
	kSetField = 1;

const
  kReseedingOptionNever = 0;
  kReseedingWhenPlanted = 1;
  kReseedingOnMonth = 2;
  kDefaultPlantProximityNeeded = 4;

const
  kUnsavedGardenName = 'noname';

type

  menuOptionsStructure = record
    showPlantsFromSide: boolean;
    showMetricUnits: boolean;
    showTools: boolean;
    showBackdrop: boolean;
    playToolSounds: boolean;
    drawSeedsAsSymbols: boolean;
    drawPlantsAsSymbols: boolean;
    drawSymbolsOverPlants: boolean;
    doNotSimulatePlantStructure: boolean;
    plantProximityNeeded: smallint;
    reseedingOption: smallint;
    reseedingMonth: smallint;
    mainWindowPlantDrawOptions: PlantDrawOptionsStructure;
    browserPlantDrawOptions: PlantDrawOptionsStructure;
    undoLimit: longint;
    backgroundColor: TColor;
    showNumericalExceptionsWindowOnException: boolean;
    bringUpGraphItemOptionsDialogWhenAspectGraphed: boolean;
    showLongHints: boolean;
    showAspectHints: boolean;
    stopSimulationAtEndOfDayOnException: boolean;
    showToolHowToHints: boolean;
    reservedArray: array [1..996] of byte;
    end;

  fileOptionsStructure = record
    gardenFileName: string;
    paletteSaved: boolean;
    paletteFileName: string;
    backdropUsed: boolean;
    backdropSaved: boolean;
    backdropFileName: string;
    backdropChangeCount: longint;
    toolsSaved: boolean;
    toolsFileName: string;
    toolsChangedCount: longint;
    templatesSaved: boolean;
    templatesFileName: string;
    templatesChangedCount: longint;
    aspectsSaved: boolean;
    aspectsFileName: string;
    aspectsChangedCount: longint;
    groupsSaved: boolean;
    groupsFileName: string;
    groupsChangedCount: longint;
    reservedArray: array [1..1000] of byte;
    end;

  soilDrawColorsStructure = record
    materialsColors: array[0..5] of TColorRef;
    temperatureColor: TColorRef;
    rootWeightColor: TColorRef;
    pHColor: TColorRef;
    nitrogenColors: array[0..4] of TColorRef;
    phosphorusColors: array[0..4] of TColorRef;
    rootMaterialColor: TColorRef;
    reservedArray: array [1..1000] of byte;
    end;

  weatherDrawColorsStructure = record
    temperatureColors: array[0..2] of TColorRef;
    precipitationColor: TColorRef;
    radiationColor: TColorRef;
    windSpeedColor: TColorRef;
    relativeHumidityColor: TColorRef;
    dayLengthColor: TColorRef;
    reservedArray: array [1..1000] of byte;
    end;

 GsDomain = class(GsModel)
    public
    gardenSize: TPoint;
    garden : GsGarden;
    menuOptions: menuOptionsStructure;
    fileOptions: fileOptionsStructure;
    soilDrawColors: soilDrawColorsStructure;
    weatherDrawColors: weatherDrawColorsStructure;
    aspectManager: GsAspectManager;
    aspectHintsFileName: string;
    groupManager: GsGroupManager;
    toolManager: GsToolManager;
    templateManager: GsTemplateManager;
    harvestManager: GsHarvestManager;
    toolParamListManager: GsToolParamListManager;
    windowManager: GsWindowManager;
    hintManager: GsHintManager;
    hintsFileName: string;
    gardenBackdropBitmap: GsBitmap;
    plantBitmap: GsBitmap;
    gardenBitmap: GsBitmap;
    paletteBitmap: GsBitmap;
    iniFileName: string;
    paletteBitmapLoaded: boolean;
    useIniFile: boolean;
    criticalGDIFreeResourcePercent: smallint;
    criticalUserFreeResourcePercent: smallint;
    startupMusicFileName: string;
    stopSimulationAfterTodayBecauseOfException: boolean;
    destructor destroy; override;
    constructor create; override;
    class function createDefault: boolean;
    class function default : GsDomain;
    class procedure destroyDefault;
    procedure defaultEverything;
    procedure defaultSoilColors;
    procedure defaultWeatherColors;
    function startupLoading: boolean;
    procedure copyBackdropToGardenBitmap;
  	procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
		procedure readDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  	procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    procedure modelTransferField(model: GsModel; var value;
		  direction, fieldID, fieldType, fieldIndex, deriveMethod: smallint);
    function createFileStream(const fileName: string; mode: word): TFileStream;
    function isFileInPath(const fileName: string): boolean;
    function exeDirectory: string;
    function windowsDirectory: string;
    procedure save(fileName: string);
    procedure load(fileName: string);
    procedure storeProfileInformation;
    function loadMostRecentFileName: string;
    procedure storeMostRecentFileName(fileName: string);
		procedure setPalette(newPalette: GsBitmap);
		procedure setBackdropGardenPlantBitmaps(newBackdropBitmap: GsBitmap; newGardenBitmap: GsBitmap; newPlantBitmap: GsBitmap);
		procedure setToolManager(newTools: GsToolManager);
		procedure setTemplateManager(newTemplates: GsTemplateManager);
		procedure setAspectManager(newAspects: GsAspectManager);
		procedure setGroupManager(newGroups: GsGroupManager);
    procedure loadObjectNamesIntoTStrings(stringList: TStrings);
		function loadPreference(const theField: string; const theDefault: string): string;
		procedure storePreference(const theField: string; const theValue: string);
    function loadResourceCutoffPercent(resourceType: smallint): string;
    procedure storeResourceCutoffPercent(resourceType: smallint);
		procedure constrainToolsToGarden;
		function nameIsAbsolute(const fileName: string): boolean;
		function buildFileNameInPath(const fileName: string): string;
		function loadObject(objectToLoad: TObject; const defaultFileName: string; useIniFile: boolean;
 			fileType: smallint; const section: string; var fileNameLoaded: string): boolean;
    function loadHintsFile(manager: TObject;
      const defaultFileName: string; useIniFile: boolean; const section: string; var fileNameLoaded: string): boolean;
		procedure resizeSupportingBitmaps;
    function playStartupMusic: boolean;
    function musicFileNameForMenu: string;
    function firstUnusedUnsavedGardenName: string;
    function unsavedGardenNameForNumber(aNumber: smallint): string;
    end;

var
	domain: GsDomain;

implementation

{browser violates gui/domain separation but in lieu of dependencies}
uses Forms, Dialogs, FileCtrl, uexcept, ubrowser, ugsim, ucursor, uprofile,
		usound, uclasses, usupport, usplash, ugraph, umusic;

{GsDomain}

class function GsDomain.createDefault: boolean;
	begin
  domain := GsDomain.create;
  result := domain.startupLoading;
  end;

class function GsDomain.default : GsDomain;
	begin
  Result := domain;
  end;

class procedure GsDomain.destroyDefault;
	begin
  domain.free;
  domain := nil;
  end;

constructor GsDomain.create;
	begin
  inherited create;
  if splashForm <> nil then splashForm.loadNextAnimationPicture;
  gardenBackdropBitmap := GsBitmap.create;
  plantBitmap := GsBitmap.create;
  gardenBitmap := GsBitmap.create;
  paletteBitmap := GsBitmap.create;
  aspectManager := GsAspectManager.create;
  groupManager := GsGroupManager.create;
  templateManager := GsTemplateManager.create;
  toolManager := GsToolManager.create;
  toolParamListManager := GsToolParamListManager.create;
  harvestManager := GsHarvestManager.create;
  windowManager := GsWindowManager.create;
  hintManager := GsHintManager.create;
  garden := GsGarden.create;
  { default menu options }
  with menuOptions do
    begin
    drawSeedsAsSymbols := true;
    showBackdrop := true;
    showTools := true;
    plantProximityNeeded := kDefaultPlantProximityNeeded;
    undoLimit := 20;
    backgroundColor := clWhite;
    showNumericalExceptionsWindowOnException := true;
    bringUpGraphItemOptionsDialogWhenAspectGraphed := true;
    showLongHints := true;
    showAspectHints := true;
    stopSimulationAtEndOfDayOnException := true;
    showToolHowToHints := true;
    end;
  iniFileName := 'gwi_kfs.ini';
  paletteBitmapLoaded := false;
  stopSimulationAfterTodayBecauseOfException := false;
  end;

destructor GsDomain.destroy;
  begin
  GsSound.stopAny;
  toolManager.Free;
  toolManager := nil;
  toolParamListManager.free;
  toolParamListManager := nil;
  gardenBackdropBitmap.free;
  gardenBackdropBitmap := nil; 
  plantBitmap.free;
  plantBitmap := nil;
  gardenBitmap.free;
  gardenBitmap := nil;
  paletteBitmap.free;
  paletteBitmap := nil;
  aspectManager.free;
  aspectManager := nil;
  groupManager.free;
  groupManager := nil;
  templateManager.free;
  templateManager := nil;
  harvestManager.free;
  harvestManager := nil;
  windowManager.free;
  windowManager := nil;
  hintManager.free;
  hintManager := nil;
  garden.free;
  garden := nil;
  inherited destroy;
  end;

function GsDomain.nameIsAbsolute(const fileName: string): boolean;
  begin
  result := ((pos('\', fileName) = 1) or (pos(':', fileName) = 2));
  end;

function GsDomain.isFileInPath(const fileName: string): boolean;
  var
    qualifiedName: string;
  begin
  result := true;
  {see if file exists}
  qualifiedName := fileName;
  if not FileExists(qualifiedName) then
    begin
    {this merging process could be more sophisticated in case
     file name has leading drive or leading slash - just not merging for now}
    if not self.nameIsAbsolute(fileName) then
      qualifiedName := self.exeDirectory + fileName;
    if not FileExists(qualifiedName) then
      begin
      result := false;
      end;
    end;
  end;

{searches for file according to name, and then in exe directory, and then gives up}
function GsDomain.buildFileNameInPath(const fileName: string): string;
  begin
  result := ExpandFileName(fileName);
  if FileExists(result) then exit;
  result := self.exeDirectory + ExtractFileName(fileName);
  if FileExists(result) then exit;
  result := fileName;
  end;

{need to add failure handling, and trying multiple times...}
function GsDomain.loadObject(objectToLoad: TObject; const defaultFileName: string; useIniFile: boolean;
 		fileType: smallint; const section: string; var fileNameLoaded: string): boolean;
 var
 	fileName: string;
  fileLoadedOK: boolean;
  buttonPressed: Word;
 begin
  result := false;
  fileLoadedOK := false;
  fileName := self.buildFileNameInPath(defaultFileName);
  if useIniFile then fileName := self.loadPreference(section, fileName);
  while not fileLoadedOK do
    begin
  	if not FileExists(fileName) then
    	begin
      if fileName <> '' then
    	  buttonPressed := MessageDlg('The ' + section + ' file ' + chr(13) + lowerCase(fileName) + chr(13)
          + 'could not be found.' + chr(13) + chr(13)
          + 'Would you like to look for it?', mtConfirmation, [mbYes, mbNo], 0)
      else
    	  buttonPressed := MessageDlg('No file is selected for the ' + section + ' file.' + chr(13)
          + 'Would you like to look for one?', mtConfirmation, [mbYes, mbNo], 0);
      if buttonPressed = mrNo then exit;
    	end
    else
      begin
      try
  		if objectToLoad is GsStreamableObject then
 				GsFiler.load(fileName, objectToLoad as GsStreamableObject)
  		else {must be bitmap}
        begin
      	if self.paletteBitmapLoaded then   {test lets palette load first, then backdrop is normalize}
    			LoadDibFromFileAndPutInBitmap(fileName, (objectToLoad as GsBitmap), self.paletteBitmap)
      	else
        	(objectToLoad as GsBitmap).loadFromFile(fileName);
        end;
  		fileNameLoaded := fileName;
      if fileNameLoaded <> defaultFileName then
        iniFileMightHaveChanged := true;
  		result := true;
      exit;
  		except
    		on E: Exception do
      		begin
      		ShowMessage(E.message);
      		ShowMessage('Could not load ' + section + ' from ' + chr(13) + fileName);
      		end;
      end;
    	buttonPressed := MessageDlg('The ' + section + ' file ' + chr(13) + fileName + chr(13)
        + 'could not be opened.' + chr(13) + chr(13)
        + 'Would you like to choose another file to use instead?',
      	mtConfirmation, [mbYes, mbNo], 0);
      if buttonPressed = mrNo then exit;
      end;
    fileName := GsFile_GetFileOpenInfo(fileType, fileName);
    if fileName = '' then exit;
    end;
  end;

{ this function is very similar to loadObject above }
function GsDomain.loadHintsFile(manager: TObject; const defaultFileName: string; useIniFile: boolean;
 		const section: string; var fileNameLoaded: string): boolean;
 var
 	fileName: string;
  fileLoadedOK: boolean;
  buttonPressed: Word;
  begin
  if manager = nil then
    raise Exception.create('GsDomain.loadHintsFile: no manager');
  result := false;
  fileLoadedOK := false;
  fileName := self.buildFileNameInPath(defaultFileName);
  if useIniFile then fileName := self.loadPreference(section, fileName);
  while not fileLoadedOK do
    begin
  	if not FileExists(fileName) then
    	begin
      if fileName <> '' then
    	  buttonPressed := MessageDlg('The ' + section + ' file ' + chr(13) + lowerCase(fileName) + chr(13)
          + 'could not be found.' + chr(13) + chr(13)
          + 'Would you like to look for it?', mtConfirmation, [mbYes, mbNo], 0)
      else
    	  buttonPressed := MessageDlg('No file is selected for the ' + section + ' file.' + chr(13)
          + 'Would you like to look for one?', mtConfirmation, [mbYes, mbNo], 0);
      if buttonPressed = mrNo then exit;
    	end
    else
      begin
      try
      if manager is GsAspectManager then
        GsAspectManager(manager).readHintsFromFile(fileName)
      else if manager is GsHintManager then
        GsHintManager(manager).readHintsFromFile(fileName)
      else
        raise Exception.create('GsDomain.loadHintsFile: wrong type of manager');
  		fileNameLoaded := fileName;
      if fileNameLoaded <> defaultFileName then
        iniFileMightHaveChanged := true;
  		result := true;
      exit;
  		except
    		on E: Exception do
      		begin
      		ShowMessage(E.message);
      		ShowMessage('Could not load ' + section + ' from ' + chr(13) + fileName);
      		end;
      end;
    	buttonPressed := MessageDlg('The ' + section + ' file ' + chr(13) + fileName + chr(13)
        + 'could not be opened.' + chr(13) + chr(13)
        + 'Would you like to choose another file to use instead?',
      	mtConfirmation, [mbYes, mbNo], 0);
      if buttonPressed = mrNo then exit;
      end;
    fileName := GsFile_GetFileOpenInfo(kFileTypeTabbedText, fileName);
    if fileName = '' then exit;
    end;
  end;

{ may want more exception handling around INI stuff - tell what is wrong? }
function GsDomain.startupLoading;
  var
    gardenFileName: string;
    i: integer;
    initialized, fileLoaded, fileNameValid, overridePlayingMusic: boolean;
    kludgeCanvas: TCanvas;
   begin
  result := true;
  useIniFile := true;
  overridePlayingMusic := false;
  gardenFileName := '';
  initialized := false;
  startupMusicFileName := '';
  { ============= parse command line options }
  if ParamCount > 0 then
    begin
    for i := 1 to ParamCount do
      if uppercase(ParamStr(i)) = '-I=' then
        useIniFile := false
      else if uppercase(ParamStr(i)) = '-I' then
        useIniFile := false
      else if pos('-I=', uppercase(ParamStr(i))) = 1 then
        iniFileName := copy(ParamStr(i), 4, length(ParamStr(i)))
      else if pos('-I', uppercase(ParamStr(i))) = 1 then
        iniFileName := copy(ParamStr(i), 3, length(ParamStr(i)))
      else if (gardenFileName = '') and (pos('-', uppercase(ParamStr(i))) <> 1) then
        gardenFileName := ParamStr(i)
      else if pos('-M', uppercase(ParamStr(i))) = 1 then
        overridePlayingMusic := true
      else
        ShowMessage('Improper parameter string ' + ParamStr(i));
    end;
  { find out if ini file exists now, because if it doesn't we should save the profile info when leaving
    even if it did not change. if iniFileName does not have a directory, use windows directory }
  if extractFilePath(iniFileName) <> '' then
    iniFileMightHaveChanged := not fileExists(iniFileName)
  else
    iniFileMightHaveChanged := not fileExists(self.windowsDirectory + '\' + iniFileName);
  { ============= load startup music file name and play music }
  if useIniFile and not overridePlayingMusic then
    begin
    startupMusicFileName := self.loadPreference('startup music', startupMusicFileName);
    { if option is 'none', user does not want to play startup music - skip rest }
    if (lowerCase(startupMusicFileName) <> 'none') then
      begin
      { if no startup file specified, try default in exe directory }
      if length(startupMusicFileName) = 0 then
        startupMusicFileName := self.exeDirectory + 'startup.mid';
      { if file does not exist, ask user to find }
      if not fileExists(startupMusicFileName) then
        begin
        fileNameValid := false;
        while not fileNameValid do
          begin
          if messageDlg('The startup music file could not be found.' + chr(13)
            + 'Would you like to look for it?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then break;
          startupMusicFileName := GsFile_GetFileOpenInfo(kFileTypeMidiFile, startupMusicFileName);
          if startupMusicFileName = '' then break;
          fileNameValid := fileExists(startupMusicFileName);
          end;
        end;
      { if correct file found, play }
      if fileExists(startupMusicFileName) then
        musicForm.playMusic(startupMusicFileName);
      end;
    end;
  { ============= read resource limits from ini file }
  if useIniFile then
    begin
    self.criticalGDIFreeResourcePercent := strToIntDef(self.loadResourceCutoffPercent(kGDIResources),
        kDefaultCriticalGDIFreeResourcePercent);
    self.criticalUserFreeResourcePercent := strToIntDef(self.loadResourceCutoffPercent(kUserResources),
        kDefaultCriticalUserFreeResourcePercent);
    end
  else
    begin
    self.criticalGDIFreeResourcePercent := kDefaultCriticalGDIFreeResourcePercent;
    self.criticalUserFreeResourcePercent := kDefaultCriticalUserFreeResourcePercent;
    end;
  { ============= create aspects }
  if splashForm <> nil then splashForm.loadNextAnimationPicture;
  CreateAllAspects(aspectManager);
  { ============= load groups }
  showLoadingString('Loading groups...');
  fileLoaded := self.loadObject(groupManager, 'groups.grp', useIniFile, kFileTypeGroups, 'groups', fileOptions.groupsFileName);
  if not fileLoaded then
    begin
    ShowMessage('The browser will be difficult to use' + chr(13) + 'without the groups file (default: groups.grp)');
    {cleanup for failed loading - object may be in corrupt state}
    groupManager.free;
    groupManager := GsGroupManager.create;
    end;
  { ============= load non-aspect hints }
  { if option is 'none', user does not want to use hints - skip rest }
  hintsFileName := self.loadPreference('hints', hintsFileName);
  if (lowerCase(hintsFileName) <> 'none') then
    begin
    showLoadingString('Loading hints...');
    if splashForm <> nil then splashForm.loadNextAnimationPicture;
    fileLoaded := self.loadHintsFile(hintManager, 'hints.tab', useIniFile, 'hints', hintsFileName);
    end;
  if not fileLoaded then
    begin
    hintManager.free;
    hintManager := nil;
    { garden window will check if hintManager is nil when deciding whether to override form hints with hints system }
    end;
  { ============= load aspect hints }
  { if option is 'none', user does not want to use aspect hints - skip rest }
  aspectHintsFileName := self.loadPreference('aspect hints', aspectHintsFileName);
  if (lowerCase(aspectHintsFileName) <> 'none') then
    begin
    showLoadingString('Loading aspect hints...');
    if splashForm <> nil then splashForm.loadNextAnimationPicture;
    fileLoaded := self.loadHintsFile(aspectManager, 'asphints.tab', useIniFile, 'aspect hints', aspectHintsFileName);
    end;
  { ============= load palette }
  showLoadingString('Loading palette...');
  if splashForm <> nil then splashForm.loadNextAnimationPicture;
  fileLoaded := self.loadObject(paletteBitmap, 'palette.bmp', useIniFile, kFileTypeBitmap, 'palette',
  		fileOptions.paletteFileName);
  if not fileLoaded then
    begin
    ShowMessage('The colors may be incorrect' + chr(13) + 'without the palette file (default: palette.bmp)');
    {cleanup for failed loading - object may be in corrupt state}
    paletteBitmap.free;
    paletteBitmap := GsBitmap.create;
    end;
  paletteBitmapLoaded := fileLoaded;
  if paletteBitmapLoaded then
    begin
  	kludgeCanvas := paletteBitmap.canvas; {force it to make handles}
    {RealizeGlobalPalette(paletteBitmap.palette); }
    end;
  { ============= load template manager }
  showLoadingString('Loading templates...');
  if splashForm <> nil then splashForm.loadNextAnimationPicture;
  fileLoaded := self.loadObject(templateManager, 'library.tpl', useIniFile, kFileTypeLibrary, 'library',
  		fileOptions.templatesFileName);
  if not fileLoaded then
    begin
    ShowMessage('There will not be any templates to choose from' + chr(13)
      + 'without the library file (default: library.tpl)');
    {cleanup for failed loading - object may be in corrupt state}
    templateManager.free;
    templateManager := GsTemplateManager.create;
    {$IFDEF ALLOW_HARDCODING}
  	ShowMessage('Using hard coded templates');
 	 	templateManager.initializeHardCodedTemplates; 
  	self.fileOptions.templatesFileName := self.exeDirectory + 'library.tpl';
    {$ENDIF}
    end;
  { ============= load tools}
  showLoadingString('Loading tools...');
  if splashForm <> nil then splashForm.loadNextAnimationPicture;
  fileLoaded := self.loadObject(toolManager, 'tools.too', useIniFile, kFileTypeTools, 'tools', fileOptions.toolsFileName);
  if not fileLoaded then
    begin
    ShowMessage('The program will not be very useable' + chr(13) + 'without the tools file (default: tools.too)');
    {cleanup for failed loading - object may be in corrupt state}
    toolManager.free;
    toolManager := GsToolManager.create;
    {$IFDEF ALLOW_HARDCODING}
  	ShowMessage('Using hard coded tools');
  	toolManager.loadDefaultTools;
 		self.fileOptions.toolsFileName := self.exeDirectory + 'tools.too';
    {$ELSE}
  	toolManager.createDefaultGlove;
  	ShowMessage('Creating all powerful invisible mega-glove');
    {$ENDIF}
    end;
  { ============= now try to load garden }
  { ------------- if filename on command line - try it }
  if gardenFileName <> '' then
    try
      showLoadingString('Loading command line garden...');
      if splashForm <> nil then splashForm.loadNextAnimationPicture;
      gardenFileName := buildFileNameInPath(gardenFileName);
      self.load(gardenFileName);
      initialized := true;
      self.storeMostRecentFileName(gardenFileName);
    except on E: Exception do
      begin
      ShowMessage(E.message);
      ShowMessage('Could not open garden file from command line ' + gardenFileName);
      end;
    end;
  { kept exit out of try block out of caution }
  if initialized then begin fileOptions.gardenFileName := gardenFileName; exit; end;
  { ------------- if saved game in INI try it unless -I option }
  if useIniFile then
    begin
    gardenFileName := self.loadMostRecentFileName;
    gardenFileName := buildFileNameInPath(gardenFileName);
    if (gardenFileName <> '') and self.isFileInPath(gardenFileName) then
      try
        showLoadingString('Loading most recent garden...');
        if splashForm <> nil then splashForm.loadNextAnimationPicture;
        self.load(gardenFileName);
        initialized := true;
      except on E: Exception do
        begin
        ShowMessage(E.message);
        ShowMessage('Could not load most recently saved garden ' + gardenFileName);
        end;
      end;
    end;
  if initialized then begin fileOptions.gardenFileName := gardenFileName; exit; end;
  { ------------- try default file }
  gardenFileName := 'default.gdn';
  gardenFileName := buildFileNameInPath(gardenFileName);
  if useIniFile then
    begin
  	gardenFileName := self.loadPreference('default', gardenFileName);
  	gardenFileName := buildFileNameInPath(gardenFileName);
    end;
  if self.isFileInPath(gardenFileName) then
  	try
      showLoadingString('Loading default garden...');
      if splashForm <> nil then splashForm.loadNextAnimationPicture;
  	  self.load(gardenFileName);
  	  initialized := true;
  	except on E: Exception do
      begin
      ShowMessage(E.message);
      ShowMessage('Could not load default garden ' + gardenFileName);
      end;
  	end;
  if initialized then begin fileOptions.gardenFileName := gardenFileName; exit; end;
  { ------------- default garden could not be found - give user a chance to find it }
  fileLoaded := false;
  while not fileLoaded do
    begin
    if messageDlg('The default garden file could not be found.' + chr(13)
      + 'Would you like to look for it?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then break;
    gardenFileName := GsFile_GetFileOpenInfo(kFileTypeGarden, gardenFileName);
    if gardenFileName = '' then break;
    if fileExists(gardenFileName) then
      try
        showLoadingString('Loading default garden...');
        if splashForm <> nil then splashForm.loadNextAnimationPicture;
        self.load(gardenFileName);
        fileLoaded := true;
        initialized := true;
      except on E: Exception do
        begin
        ShowMessage(E.message);
        ShowMessage('Could not load default garden ' + gardenFileName);
        end;
      end;
    end;
  if splashForm <> nil then splashForm.loadNextAnimationPicture;
  if initialized then begin fileOptions.gardenFileName := gardenFileName; exit; end;
  { ------------- every attempt to load a garden file has failed - just default stuff }
  showMessage('Could not load a garden file.' + chr(13) + chr(13)
    + 'Defaulting garden.' + chr(13)
    + 'You should choose a climate before you run the simulation.' + chr(13)
    + 'If you have not loaded a template file,' + chr(13)
    + 'you should load one before choosing a climate.');
  self.defaultEverything;
  end;

function GsDomain.firstUnusedUnsavedGardenName: string;
  var
    fileNumber: smallint;
  begin
  result := '';
  fileNumber := 1;
  while fileNumber < 100 do
    begin
    result := self.exeDirectory + unsavedGardenNameForNumber(fileNumber);
    if not fileExists(result) then
      exit;
    inc(fileNumber);
    end;
  showMessage('Too many unnamed garden files. Delete some to reuse names.');
  end;

function GsDomain.unsavedGardenNameForNumber(aNumber: smallint): string;
  var
    numberString: string;
  begin
  numberString := intToStr(aNumber);
  if length(numberString) = 1 then
    numberString := '0' + numberString;
  result := kUnsavedGardenName + numberString + '.gdn';
  end;

procedure GsDomain.defaultEverything;
  var
    fileLoaded: boolean;
  begin
  fileOptions.gardenFileName := self.firstUnusedUnsavedGardenName;
  fileOptions.backdropUsed := false;
  { drawing options for main window default to wire frame, straight lines, simple leaves (for best speed) }
  menuOptions.mainWindowPlantDrawOptions.wireFrame := true;
  menuOptions.mainWindowPlantDrawOptions.straightLinesOnly := true;
  menuOptions.mainWindowPlantDrawOptions.simpleLeavesOnly := true;
  { browser drawing options default to lines between (for best look) - but not sorting because it is demanding }
  menuOptions.browserPlantDrawOptions.drawLines := true;
  toolParamListManager.hardCodeLists;
  self.defaultSoilColors;
  self.defaultWeatherColors;
  garden.IGarden; {this defaults the weather}
  { set up starting date for harvest manager so it knows when to clear out harvest list }
  { this only needs done when a new garden is created }
  harvestManager.setLastReportResetDate(garden.date);
  {default the size in case can't load bitmap}
  gardenSize.x := 400;
  gardenSize.y := 300;
  fileOptions.backdropUsed := true;
  {load default backdrop}
  fileLoaded := self.loadObject(gardenBackdropBitmap, fileOptions.backdropFileName, false,
  		kFileTypeBitmap, 'backdrop', fileOptions.backdropFileName);
  if not fileLoaded then
    begin
    ShowMessage('The default backdrop file ' + chr(13) + fileOptions.backdropFileName + chr(13) + ' could not be loaded.');
    fileOptions.backdropUsed := false;
    {clean up for possible corrupted object}
    gardenBackdropBitmap.free;
    gardenBackdropBitmap := GsBitmap.create;
    end;
  self.resizeSupportingBitmaps;
  plantBitmap.palette := CopyPalette(gardenBackdropBitmap.palette);
  gardenBitmap.palette := CopyPalette(gardenBackdropBitmap.palette);
  self.constrainToolsToGarden;
  end;

procedure GsDomain.defaultSoilColors;
  begin
  with soilDrawColors do
    begin
      materialsColors[0] := clBlack;
      materialsColors[1] := clMaroon;
      materialsColors[2] := clOlive;
      materialsColors[3] := clGray;
      materialsColors[4] := clBlue;
      materialsColors[5] := clWhite;
      temperatureColor := clBlue;
      rootWeightColor := clOlive;
      pHColor := clTeal;
      nitrogenColors[0] := clLime;
      nitrogenColors[1] := clBlue;
      nitrogenColors[2] := clRed;
      nitrogenColors[3] := clAqua;
      nitrogenColors[4] := clTeal;
      phosphorusColors[0] := clLime;
      phosphorusColors[1] := clBlue;
      phosphorusColors[2] := clRed;
      phosphorusColors[3] := clAqua;
      phosphorusColors[4] := clTeal;
      rootMaterialColor := clNavy;
    end;
  end;

procedure GsDomain.defaultWeatherColors;
  begin
  with weatherDrawColors do
    begin
    temperatureColors[0] := clBlue;  {min}
    temperatureColors[1] := clGreen;  {mean}
    temperatureColors[2] := clRed;  {max}
    precipitationColor := clAqua; 
    radiationColor := clLime;
    windSpeedColor := clFuchsia;
    relativeHumidityColor := clNavy;
    dayLengthColor := clPurple;
    end;
  end;

function GsDomain.playStartupMusic: boolean;
  begin
  result := (lowerCase(self.startupMusicFileName) <> 'none') and (self.startupMusicFileName <> '');
  end;

function GsDomain.musicFileNameForMenu: string;
  begin
  if self.playStartupMusic then
    result := ' (' + lowerCase(extractFileName(self.startupMusicFileName)) + ')'
  else
    result := '';
  end;

procedure GsDomain.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsDomain;
  cvir.versionNumber := 10;
  cvir.additionNumber := 2;
  end;

procedure GsDomain.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    toolSettingsList: TListCollection;
	begin
  cursor_startWait;
  try
  inherited streamDataWithFiler(filer, cvir);
  if filer.isReading then
    begin
    { needs to be done separately so can back out if read fails }
    self.readDataWithFiler(filer, cvir);
    if cvir.additionNumber < 1 then
      begin
      menuOptions.showLongHints := true;
      menuOptions.showAspectHints := true;
      menuOptions.stopSimulationAtEndOfDayOnException := true;
      end;
    if cvir.additionNumber < 2 then
      menuOptions.showToolHowToHints := true;
    end
  else
    begin
    toolSettingsList := toolManager.newListCollectionOfToolSettings;
    try
      toolSettingsList.streamUsingFiler(filer, GsToolSettings);
    finally
      toolSettingsList.free;
    end;
  	filer.streamPoint(gardenSize);
    { options }
  	filer.streamBoolean(fileOptions.backdropUsed);
  	filer.streamShortString(fileOptions.backdropFileName);
  	filer.streamBytes(menuOptions, sizeOf(menuOptions));
  	filer.streamBytes(soilDrawColors, sizeOf(soilDrawColors));
  	filer.streamBytes(weatherDrawColors, sizeOf(weatherDrawColors));

    toolParamListManager.streamUsingFiler(filer);
  	{need to stream all resources used by garden in case template manager doesn't have them}
  	{need to stream only resources of template manager, so set a special flag}
  	{the finally ensures it will always be set back}
 		try
  	  templateManager.streamOnlyResources := true;
  	  templateManager.streamUsingFiler(filer);
  	finally
  	  templateManager.streamOnlyResources := false;
 	 	end;
  	harvestManager.streamUsingFiler(filer);
  	garden.streamUsingFiler(filer);
    windowManager.streamUsingFiler(filer);
    end;
  finally
  cursor_stopWait;
  end;
  end;

{maybe would be cleaner if made new domain and only copied a few things to it?}
procedure GsDomain.readDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    newGardenSize: TPoint;
    newBackdropUsed: boolean;
    newBackdropFileName: string;
    newMenuOptions: menuOptionsStructure;
    newSoilDrawColors: soilDrawColorsStructure;
    newWeatherDrawColors: weatherDrawColorsStructure;
    newHarvestManager: GsHarvestManager;
    newGarden: GsGarden;
    newBackdropBitmap: GsBitmap;
    newPlantBitmap: GsBitmap;
    newGardenBitmap: GsBitmap;
    fileLoaded: boolean;
    newTemplateManagerForResourcesOnly: GsTemplateManager;
    newToolSettingsList: TListCollection;
    newToolParamListManager: GsToolParamListManager;
	begin
  {nil objects to be created in case they fail}
  newHarvestManager := nil;
  newGarden := nil;
  newBackdropBitmap := nil;
  newPlantBitmap := nil;
  newGardenBitmap := nil;
  newTemplateManagerForResourcesOnly := nil;
  newToolSettingsList := nil;
  newToolParamListManager := nil;
  try
  {create all objects}
  newToolSettingsList := TListCollection.create;
  newHarvestManager := GsHarvestManager.create;
  newGarden := GsGarden.create;
  newBackdropBitmap := GsBitmap.create;
  newPlantBitmap := GsBitmap.create;
  newGardenBitmap := GsBitmap.create;
  newTemplateManagerForResourcesOnly := GsTemplateManager.create;
  newToolParamListManager := GsToolParamListManager.create;
  {stream stuff to new parallel structure}
  newToolSettingsList.streamUsingFiler(filer, GsToolSettings);
  filer.streamPoint(newGardenSize);
  {options}
  filer.streamBoolean(newBackdropUsed);
  filer.streamShortString(newBackdropFileName);
  filer.streamBytes(newMenuOptions, sizeOf(menuOptions));
  filer.streamBytes(newSoilDrawColors, sizeOf(soilDrawColors));
  filer.streamBytes(newWeatherDrawColors, sizeOf(weatherDrawColors));

  newToolParamListManager.streamUsingFiler(filer);
  {need to stream all resources used by garden in case template manager doesn't have them}
  {need to stream only resources of template manager, so set a special flag - doesn't need to be reset}
  newTemplateManagerForResourcesOnly.streamOnlyResources := true;
  newTemplateManagerForResourcesOnly.streamUsingFiler(filer);

  {set the filer to use this resource provider instead of domain template manager}
  filer.resourceProvider := newTemplateManagerForResourcesOnly;

  {harvest manager refers to harvest templates in template manager and will need to be fixed up}
  newHarvestManager.streamUsingFiler(filer);

  {garden plants refer to tdos and harvest templates in template manager and will need to be fixed up}
  newGarden.streamUsingFiler(filer);
  if newBackdropUsed then
    begin
    { if don't find directory for location provided in name,
      change name to the same file in the exe directory, but do not ask user to look for it;
      this is primarily for the first time after installation }
    if extractFilePath(newBackdropFileName) = '' then
      newBackdropFileName := self.exeDirectory + extractFileName(newBackdropFileName);
  	fileLoaded := self.loadObject(newBackdropBitmap, newBackdropFileName, false, kFileTypeBitmap, 'backdrop',
      newBackdropFileName);
  	if not fileLoaded then
    	begin
    	ShowMessage('The backdrop file ' + chr(13) + newBackdropFileName + chr(13) + ' could not be loaded.');
    	newBackdropUsed := false;
    	{clean up for possible corrupted object}
    	newBackdropBitmap.free;
    	newBackdropBitmap := GsBitmap.create;
    	end;
    if newBackdropUsed then
      begin
      newGardenSize.x := newBackdropBitmap.width;
      newGardenSize.y := newBackdropBitmap.height;
      end;
    newPlantBitmap.width := newGardenSize.x;
    newPlantBitmap.height := newGardenSize.y;
    newGardenBitmap.width := newGardenSize.x;
    newGardenBitmap.height := newGardenSize.y;
    if newBackdropUsed then
      begin
			newPlantBitmap.Assign(newBackdropBitmap);
      newPlantBitmap.palette := CopyPalette(newBackdropBitmap.palette);
			newGardenBitmap.Assign(newBackdropBitmap);
      newGardenBitmap.palette := CopyPalette(newBackdropBitmap.palette);
     end;
    end
  else
    begin
    { if not using backdrop, still must set bitmap sizes at streamed size }
    newPlantBitmap.width := newGardenSize.x;
    newPlantBitmap.height := newGardenSize.y;
    newGardenBitmap.width := newGardenSize.x;
    newGardenBitmap.height := newGardenSize.y;
    end;
  {now try to fix up harvest manager and garden}
  {this may add icon, tdo, and harvest resources to the template manager}
  {if this fails, the domain template manager may be left with extra resources}
  {these resources could be garbage collected and should be OK to use}
  {alternative would be to make a copy of template manager before doing this...}
  {not doing alternative because assuming infrequent failures at this point and
  prefer to avoid overhead of copy}
  newHarvestManager.fixupAllResourcesInHarvestItems;
  newGarden.fixupAllResourcesInPlants;
  {the moment of truth - the above should no longer point to resources in this manager}
  {otherwise, they will be pointing to freed objects}
  newTemplateManagerForResourcesOnly.free;
  newTemplateManagerForResourcesOnly := nil;
  except
  {free all allocated objects}
  newHarvestManager.free;
  newGarden.free;
  newBackdropBitmap.free;
  newPlantBitmap.free;
  newGardenBitmap.free;
  newTemplateManagerForResourcesOnly.free;
  newToolSettingsList.free;
  newToolParamListManager.free;
  {let someone else handle this exception}
  raise;
  end;

  {all data loaded succesfully (except windows) - so put everything in place}
  gardenSize := newGardenSize;
  {options}
  fileOptions.backdropUsed := newBackdropUsed;
  fileOptions.backdropFileName := newBackdropFileName;
  menuOptions := newMenuOptions;
  soilDrawColors := newSoilDrawColors;
  weatherDrawColors := newWeatherDrawColors;
  harvestManager.free;
  harvestManager := newHarvestManager;
  garden.free;
  garden := newGarden;
  gardenBackdropBitmap.free;
  gardenBackdropBitmap := newBackdropBitmap;
  plantBitmap.free;
  plantBitmap := newPlantBitmap;
  gardenBitmap.free;
  gardenBitmap := newGardenBitmap;
  toolParamListManager.free;
  toolParamListManager := newToolParamListManager;
  { setting tool positions can't be undone }
  try
    toolManager.updateToolSettings(newToolSettingsList);
  finally
    newToolSettingsList.free;
  end;
  { window streaming can't be undone }
  windowManager.streamUsingFiler(filer);
  cursor_stopWait;
  end;

procedure GsDomain.resizeSupportingBitmaps;
	begin
  if fileOptions.backdropUsed then
     self.copyBackdropToGardenBitmap
  else
    begin
    self.plantBitmap.width := gardenSize.x;
    self.plantBitmap.height := gardenSize.y;
    self.gardenBitmap.width := gardenSize.x;
    self.gardenBitmap.height := gardenSize.y;
    end;
  end;

function GsDomain.createFileStream(const fileName: string; mode: word): TFileStream;
  var
    qualifiedName: string;
  begin
  if mode = fmOpenRead then
    begin
    {see if file exists}
    qualifiedName := fileName;
    if not FileExists(qualifiedName) then
      begin
      {this merging process could be more sophisticated in case
       file name has leading drive or leading slash - just not merging for now}
      if not((pos('\', fileName) = 1) or (pos(':', fileName) = 2)) then
        qualifiedName := self.exeDirectory + fileName;
      if not FileExists(qualifiedName) then
        begin
        {could exit here or try something else}
        result := nil;
        end;
      end;
  	result := TFileStream.create(qualifiedName, mode);
    end
  else
  	result := TFileStream.create(fileName, mode);
  end;

function GsDomain.exeDirectory: string;
  begin
  result := ExtractFilePath(Application.exeName);
  end;

function GsDomain.windowsDirectory: string;
  var
    cString: array[0..255] of char;
  begin
  result := '';
  getWindowsDirectory(cString, 256);
  result := strPas(cString);
  end;

procedure GsDomain.save(fileName: string);
	var
    fileStream: TFileStream;
	  filer: GSFiler;
	begin
  fileStream := nil;
  filer := nil;
	fileStream := self.createFileStream(fileName, fmOpenWrite or fmCreate);
	try
		filer := GsFiler.createWithStream(fileStream);
    filer.setToWritingMode;
    try
    self.streamUsingFiler(filer);
    finally
		filer.free;
    end;
	finally
		fileStream.free;
	end;
  end;

procedure GsDomain.load(fileName: string);
	var
  	fileStream: TFileStream;
	  filer: GsFiler;
	begin
  fileStream := nil;
  filer := nil;
	fileStream := self.createFileStream(fileName, fmOpenRead);
	try
		filer := GsFiler.createWithStream(fileStream);
    filer.setToReadingMode;
    try
    Domain.streamUsingFiler(filer);
    finally
		filer.free;
    end;
	finally
		fileStream.free;
	end;
  end;

procedure GsDomain.loadObjectNamesIntoTStrings(stringList: TStrings);
  begin
  if garden <> nil then garden.loadObjectNamesIntoTStrings(stringList);
  if templateManager <> nil then templateManager.loadObjectNamesIntoTStrings(stringList);
  end;

procedure GsDomain.copyBackdropToGardenBitmap;
  begin
  plantBitmap.width := gardenBackdropBitmap.width;
  plantBitmap.height := gardenBackdropBitmap.height;
	plantBitmap.Assign(gardenBackdropBitmap);
  gardenBitmap.width := gardenBackdropBitmap.width;
  gardenBitmap.height := gardenBackdropBitmap.height;
	gardenBitmap.Assign(gardenBackdropBitmap);
  gardenSize.x := gardenBackdropBitmap.width;
  gardenSize.y := gardenBackdropBitmap.height;
  end;

{provide place to intercept all model transfers for dependency updating}
procedure GsDomain.modelTransferField(model: GsModel; var value;
		direction, fieldID, fieldType, fieldIndex, deriveMethod: smallint);
  var
    updateList: TListCollection;
	begin
  if (direction = kGetField) then
    model.transferField(direction, value, fieldID, fieldType, fieldIndex, deriveMethod, nil)
  else
    begin
    updateList := TListCollection.create;
    model.transferField(direction, value, fieldID, fieldType, fieldIndex, deriveMethod, updateList);
    if updateList.count > 0 then BrowserForm.updateModelList(updateList);
    updateList.free;
    updateList := nil;
    end;
  end;

{PDF FIX - need to update views and combo boxes for all these}
procedure GsDomain.setPalette(newPalette: GsBitmap);
  begin
  paletteBitmap.free;
  paletteBitmap := newPalette;
  raise Exception.create('Palette changing while running not allowed.');
  {PDF FIX - update colors in tools and backdrop}
  end;

procedure GsDomain.setBackdropGardenPlantBitmaps(newBackdropBitmap: GsBitmap;
		newGardenBitmap: GsBitmap; newPlantBitmap: GsBitmap);
  begin
  if (newBackdropBitmap = nil) or (newGardenBitmap = nil) or (newPlantBitmap = nil) then exit;
  gardenBackdropBitmap.free;
  gardenBackdropBitmap := newBackdropBitmap;
  gardenBitmap.free;
  gardenBitmap := newGardenBitmap;
  plantBitmap.free;
  plantBitmap := newPlantBitmap;
  gardenSize.x := newPlantBitmap.width;
  gardenSize.y := newPlantBitmap.height;
  end;

procedure GsDomain.setToolManager(newTools: GsToolManager);
  begin
  toolManager.free;
  toolManager := newTools;
  end;

procedure GsDomain.setTemplateManager(newTemplates: GsTemplateManager);
  begin
  templateManager.free;
  templateManager := newTemplates;
  end;

procedure GsDomain.setAspectManager(newAspects: GsAspectManager);
  begin
  aspectManager.free;
  aspectManager := newAspects;
  end;

procedure GsDomain.setGroupManager(newGroups: GsGroupManager);
  begin
  groupManager.free;
  groupManager := newGroups;
  end;

{profile stuff}
procedure GsDomain.storeProfileInformation;
  var
    fileInfo: SaveFileNamesStructure;
    fileIsOKForSaving: boolean;
    response: integer;
  begin
  fileIsOKForSaving := false;
  while not fileIsOKForSaving do
    begin
    try
      Domain.storePreference('groups', lowerCase(Domain.fileOptions.groupsFileName));
      fileIsOKForSaving := true;
    except
      response := messageDlg('Could not save the initialization file ' + iniFileName + chr(13)
        + 'in your Windows directory.' + chr(13)
        + 'Would you like to save to another ini file?', mtError, [mbYes, mbRetry, mbNo], 0);
      case response of
        mrYes:
          if not GsFile_GetFileSaveInfo(kFileTypeIni, kAskForFileName, iniFileName, fileInfo) then
            exit
          else
            iniFileName := fileInfo.newFile;
        mrRetry:
          continue;
        mrNo:
          exit;
      end;
    end;
    end;
  { assume if you got through this with no exits, ok to save rest of items }
  Domain.storeMostRecentFileName(lowerCase(Domain.fileOptions.gardenFileName));
  { we want to save default file name if it is not in the file at all (new file) }
  if self.loadPreference('default', '') = '' then
    Domain.storePreference('default', lowerCase(self.exeDirectory) + 'default.gdn');
  {a bit kludgy - maybe should do this elsewhere? as in startup?}
  Domain.storePreference('groups', lowerCase(Domain.fileOptions.groupsFileName));
  Domain.storePreference('library', lowerCase(Domain.fileOptions.templatesFileName));
  Domain.storePreference('tools', lowerCase(Domain.fileOptions.toolsFileName));
  {more questionable - in case changed at startup?}
  Domain.storePreference('palette', lowerCase(Domain.fileOptions.paletteFileName));
  Domain.storePreference('startup music', lowerCase(Domain.startupMusicFileName));
  Domain.storePreference('hints', lowerCase(Domain.hintsFileName));
  Domain.storePreference('aspect hints', lowerCase(Domain.aspectHintsFileName));
  { resources }
  Domain.storeResourceCutoffPercent(kGDIResources);
  Domain.storeResourceCutoffPercent(kUserResources);
  iniFileMightHaveChanged := false;
  end;

function GsDomain.loadMostRecentFileName: string;
  var
    iniFile: GsProfile;
  begin
  iniFile := GsProfile.create(iniFileName);
  try
    result := iniFile.ReadString('Recent', 'file', '');
  finally
    iniFile.free;
  end;
  end;

procedure GsDomain.storeMostRecentFileName(fileName: string);
  var
    iniFile: GsProfile;
  begin
  iniFile := GsProfile.create(iniFileName);
  try
    iniFile.WriteString('Recent', 'file', fileName);
  finally
    iniFile.free;
  end;
  end;

function GsDomain.loadPreference(const theField: string; const theDefault: string): string;
  var
    iniFile: GsProfile;
  begin
  iniFile := GsProfile.create(iniFileName);
  try
    result := iniFile.ReadString('Preferences', theField, theDefault);
  finally
    iniFile.free;
  end;
  end;

procedure GsDomain.storePreference(const theField: string; const theValue: string);
  var
    iniFile: GsProfile;
  begin
  iniFile := GsProfile.create(iniFileName);
  try
    iniFile.WriteString('Preferences', theField, theValue);
  finally
    iniFile.free;
  end;
  end;

const
  kResourcesSectionString = 'Critical resource percents for Win 3.1';

function GsDomain.loadResourceCutoffPercent(resourceType: smallint): string;
  var
    iniFile: GsProfile;
  begin
  iniFile := GsProfile.create(iniFileName);
  try
    if resourceType = kGDIResources then
      result := iniFile.ReadString(kResourcesSectionString, 'GDI', intToStr(kDefaultCriticalGDIFreeResourcePercent))
    else if resourceType = kUserResources then
      result := iniFile.ReadString(kResourcesSectionString, 'User', intToStr(kDefaultCriticalUserFreeResourcePercent));
  finally
    iniFile.free;
  end;
  end;

procedure GsDomain.storeResourceCutoffPercent(resourceType: smallint);
  var
    iniFile: GsProfile;
  begin
  iniFile := GsProfile.create(iniFileName);
  try
    if resourceType = kGDIResources then
      iniFile.WriteString(kResourcesSectionString, 'GDI', intToStr(self.criticalGDIFreeResourcePercent))
    else if resourceType = kUserResources then
      iniFile.WriteString(kResourcesSectionString, 'User', intToStr(self.criticalUserFreeResourcePercent));
  finally
    iniFile.free;
  end;
  end;

procedure GsDomain.constrainToolsToGarden;
  var
    aRect: TRect;
	begin
  aRect := Rect(0, 0, gardenBitmap.width, gardenBitmap.height);
  toolManager.constrainToolsToRectangle(aRect);
  end;

end.

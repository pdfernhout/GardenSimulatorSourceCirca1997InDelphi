unit UGsim;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugsim: The main form for the application. It is called ugsim because the old name
was 'Garden Simulator'. This form file is long and does MANY things. Handles tool
actions (with mouse down, move, up); opens and saves files; opens other windows
from menu; changes menu options; manages transfer of model data; draws garden
and manages bitmaps to layer picture (bitmaps are in domain). You should look over
the domain file (udomain) before reading this as you need to understand some of
the things in the domain.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Menus, Gauges, Buttons, MPlayer,
  ubrowser, utools, ucollect, umodel, udomain, uesoil, ugraph,
  usimop, utempl, ufiler, uharvest, ucommand, uaspects, ugroups, ucursor, uestruct, utempman,
  udate, uharvpnl, ueplant, uicon, utimelin, ugsform;

type
  GsPaintBoxWithPalette = class(TPaintBox)
    public
		function GetPalette: HPALETTE; override;
    end;

  TGardenForm = class(GsForm)
    topPanel: TPanel;
    ToolChoices: TComboBox;
    DayButton: TButton;
    YearButton: TButton;
    toolActionChoices: TComboBox;
    toolParam1Choices: TComboBox;
    MainMenu: TMainMenu;
    EditMenu: TMenuItem;
    MenuUndo: TMenuItem;
    N10: TMenuItem;
    MenuCut: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    RunMenu: TMenuItem;
    MenuGo: TMenuItem;
    MenuStop: TMenuItem;
    N1: TMenuItem;
    MenuGoForDay: TMenuItem;
    MenuGoForWeek: TMenuItem;
    MenuGoForMonth: TMenuItem;
    MenuGoForYear: TMenuItem;
    MenuGoForMonths: TMenuItem;
    OptionsMenu: TMenuItem;
    MenuShowTools: TMenuItem;
    MenuShowBackdrop: TMenuItem;
    N4: TMenuItem;
    WindowMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MenuHelpContents: TMenuItem;
    MenuHelpSearchFor: TMenuItem;
    MenuHelpQuickStart: TMenuItem;
    MenuAboutGS: TMenuItem;
    MenuRedo: TMenuItem;
    MenuDailyRedraw: TMenuItem;
    MenuWeeklyRedraw: TMenuItem;
    MenuMonthlyRedraw: TMenuItem;
    MenuAtStopRedraw: TMenuItem;
    N9: TMenuItem;
    Timer: TTimer;
    MenuDrawPlantsAsSymbols: TMenuItem;
    GardenImageScrollBox: TScrollBox;
    expandGlyph: TImage;
    collapseGlyph: TImage;
    MenuBrowser: TMenuItem;
    MenuGraph: TMenuItem;
    N3: TMenuItem;
    MenuHarvestList: TMenuItem;
    statusPanel: TPanel;
    statusLabel: TLabel;
    MenuWeatherForecast: TMenuItem;
    weekButton: TButton;
    N2: TMenuItem;
    MenuSimulation: TMenuItem;
    MenuSoilTypeFinder: TMenuItem;
    MenuPlantEstimator: TMenuItem;
    MenuReseedingOptions: TMenuItem;
    MenuDrawSeedsAsSymbols: TMenuItem;
    FileMenu: TMenuItem;
    MenuExit: TMenuItem;
    N6: TMenuItem;
    MenuSaveAs: TMenuItem;
    MenuSave: TMenuItem;
    N8: TMenuItem;
    MenuOpen: TMenuItem;
    MenuNew: TMenuItem;
    MenuPlayMusic: TMenuItem;
    N11: TMenuItem;
    MenuPlayToolSounds: TMenuItem;
    MenuDisplayOptions: TMenuItem;
    harvestPanelUpGlyph: TImage;
    harvestPanelDownGlyph: TImage;
    harvestIconDefault: TImage;
    MenuChooseBackdrop: TMenuItem;
    toolParam2Choices: TComboBox;
    MenuHelpTutorial: TMenuItem;
    harvestIconDefaultBitmap: TImage;
    MenuNumericalExceptions: TMenuItem;
    MenuHelpHowto: TMenuItem;
    N5: TMenuItem;
    MenuHelpMenus: TMenuItem;
    MenuHelpModels: TMenuItem;
    MenuDrawSymbolsOverPlants: TMenuItem;
    N13: TMenuItem;
    MenuMetricUnits: TMenuItem;
    MenuEnglishUnits: TMenuItem;
    collapseExpand: TBitBtn;
    MonthButton: TButton;
    N7: TMenuItem;
    MenuCopyForm: TMenuItem;
    MenuPrintForm: TMenuItem;
    PrintDialog: TPrintDialog;
    MenuHelpWindows: TMenuItem;
    MenuGroupEditor: TMenuItem;
    MenuHarvestItemEditor: TMenuItem;
    MenuTemplatesEditor: TMenuItem;
    MenuToolEditor: TMenuItem;
    MenuToolParameterListEditor: TMenuItem;
    MenuHarvestReportEditor: TMenuItem;
    MenuStopMusic: TMenuItem;
    MenuPlayStartupMusic: TMenuItem;
    menuShowLongHints: TMenuItem;
    menuShowHintsForAspects: TMenuItem;
    N12: TMenuItem;
    menuShowtoolhowtohints: TMenuItem;
    procedure Quit1Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolChoicesChange(Sender: TObject);
    procedure toolActionChoicesChange(Sender: TObject);
    procedure toolParam1ChoicesChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure testDragIconMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
		procedure MenuSimulationClick(Sender: TObject);
    procedure MenuHarvestListClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure GardenPaintBoxPaint(Sender: TObject);
 		procedure terminateRun;
   	procedure MenuGoForYearClick(Sender: TObject);
    procedure MenuGoForDayClick(Sender: TObject);
    procedure MenuDailyRedrawClick(Sender: TObject);
    procedure MenuWeeklyRedrawClick(Sender: TObject);
    procedure MenuMonthlyRedrawClick(Sender: TObject);
    procedure MenuAtStopRedrawClick(Sender: TObject);
    procedure MenuGoForWeekClick(Sender: TObject);
    procedure MenuGoForMonthClick(Sender: TObject);
    procedure MenuGoForMonthsClick(Sender: TObject);
    procedure MenuGoClick(Sender: TObject);
    procedure MenuStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure MenuShowToolsClick(Sender: TObject);
    procedure MenuShowBackdropClick(Sender: TObject);
    procedure MenuPlayToolSoundsClick(Sender: TObject);
    procedure MenuToolEditorClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuAboutGSClick(Sender: TObject);
    procedure collapseExpandClick(Sender: TObject);
    procedure MenuBrowserClick(Sender: TObject);
    procedure MenuGraphClick(Sender: TObject);
    procedure MenuReseedingOptionsClick(Sender: TObject);
    procedure MenuDisplayOptionsClick(Sender: TObject);
    procedure MenuDrawSeedsAsSymbolsClick(Sender: TObject);
    procedure MenuDrawPlantsAsSymbolsClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuPlayMusicClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuChooseBackdropClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure toolParam2ChoicesChange(Sender: TObject);
    procedure MenuHelpContentsClick(Sender: TObject);
    procedure MenuHelpSearchForClick(Sender: TObject);
    procedure MenuHelpQuickStartClick(Sender: TObject);
    procedure MenuHelpTutorialClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure MenuNumericalExceptionsClick(Sender: TObject);
    procedure MenuHarvestItemEditorClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuHelpHowtoClick(Sender: TObject);
    procedure MenuHelpMenusClick(Sender: TObject);
    procedure MenuHelpModelsClick(Sender: TObject);
    procedure MenuDrawSymbolsOverPlantsClick(Sender: TObject);
    procedure MenuMetricUnitsClick(Sender: TObject);
    procedure MenuEnglishUnitsClick(Sender: TObject);
    procedure statusPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuCopyFormClick(Sender: TObject);
    procedure MenuPrintFormClick(Sender: TObject);
    procedure MenuHelpWindowsClick(Sender: TObject);
    procedure MenuTemplatesEditorClick(Sender: TObject);
    procedure MenuToolParameterListEditorClick(Sender: TObject);
    procedure MenuGroupEditorClick(Sender: TObject);
    procedure MenuHarvestReportEditorClick(Sender: TObject);
    procedure MenuStopMusicClick(Sender: TObject);
    procedure MenuPlayStartupMusicClick(Sender: TObject);
    procedure menuShowLongHintsClick(Sender: TObject);
    procedure menuShowHintsForAspectsClick(Sender: TObject);
    procedure MenuTimelineClick(Sender: TObject);
    procedure menuShowtoolhowtohintsClick(Sender: TObject);
  public  {our stuff}
    currentTool: GsTool;
    invalidRectForGarden : TRect;
    invalidRectForTools : TRect;
    offsetPoint: TPoint;
    actionInProgress : boolean;
    commandList: KfCommandList;
    redrawFrequency: integer;
    runUntil: GsDate;
    lastDraw: GsDate;
    running: boolean;
    runUntilUserStop: boolean;
    copyrightString: string;
    expanded: boolean;
    collapsedSize: TPoint;
    expandedSize: TPoint;
		hintX, hintY: integer;
    hintActive: boolean;
    lastHintString: string;
    isModelAddedOrRemoved: boolean;
    gardenShouldBeInvalidatedOnUpdate: boolean;
    harvestPanel: GsHarvestItemPanel;
    updateHarvestPanel: boolean;
    toolNowOver: GsTool;
    soilPatchNowOver: GsSoilPatch;
    plantNowOver: GsPlant;
    plantLastOverRect: TRect;
    toolLineIsSplit: boolean;
    GardenPaintBox: GsPaintBoxWithPalette;
    rightButtonPressed: boolean;
    lastSaveProceeded: boolean;
    procedure modelAddedOrRemoved;
    procedure populateTools;
    procedure drawCurrentTool;
		procedure drawToolsExceptCurrent;
		procedure invalidateHarvestIconRectIfNeeded;
    procedure invalidateCurrentToolRect;
    procedure invalidateGardenRect(rect: TRect);
		procedure invalidateToolRect(rect: TRect);
    procedure selectTool(x : integer; y: integer);
    procedure setCurrentTool(tool: GSTool; returnToStart: boolean);
		procedure constrainCurrentToolPositionToWindow;
    destructor Destroy; override;
    procedure paintGardenImage;
    procedure modelDeleted(model: GsModel);
    procedure modelUndeleted(model: GsModel);
    procedure updateRedrawFrequencyMenu;
    procedure redrawGarden;
		function simulateAnotherDay: boolean;
    procedure doUpdating;
    procedure startRunning;
		procedure cursorJumpToCurrentTool;
    procedure updateMenu;
    procedure updateOptionsMenu;
    procedure doCommand(command: KfCommand);
    procedure undoLast;
    function makeMouseCommand(var point: TPoint): KfCommand;
		procedure updateObjectsNowOver(point: TPoint);
		function isDrawingHarvestIcons: boolean;
		function hintForObjectsNowOver: string;
		procedure setCursorForObjectsNowOver;
    procedure DoHint(Sender: TObject);
{$IFDEF WINDOWS}
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
{$ELSE}
    procedure DoShowHint(var HintStr: ansistring; var CanShow: Boolean; var HintInfo: THintInfo);
{$ENDIF}
		function iconForPlantNowOver(actionType: longint): TIcon;
    function copyrightCheck(var aString: string): longint;
    procedure invalidateEntireGarden;
    procedure invalidateEntireToolRect;
    procedure statusUpdateLabel(newLabel: string);
    procedure statusClear;
		procedure updateDateButtons;
    procedure handlePossibleGardenChange;
		procedure resizeForGardenBitmapChange;
		procedure clearCommandList;
    function minTopPanelWidthFirstLine: integer;
    function minTopPanelWidthSecondLine: integer;
    function topPanelHeightAfterResize(firstLineSplit, secondLineSplit: boolean): longint;
    function resizeTopPanel(inFormResize: boolean): boolean;
    procedure resizeHarvestPanel;
		procedure initializeStuff;
		procedure updateFormCaption;
    procedure WMGetMinMaxInfo(var MSG: Tmessage);  message WM_GetMinMaxInfo;
    function currentToolActionObject: GsToolAction;
    function currentToolParam1Object: TObject;
    function currentToolParam2Object: TObject;
    procedure updateToolChoicesForCurrentTool;
    procedure updateToolActionChoicesForCurrentTool;
    procedure updateToolParamChoicesForCurrentToolAction;
    procedure updateToolDisplayForCurrentToolAction;
    function comboBoxMinWidth(comboBox: TComboBox): smallint;
		function GetPalette: HPALETTE; override;
		function PaletteChanged(Foreground: Boolean): Boolean; override;
		procedure handleUpdateHarvestPanel;
    procedure updateForNewDomain;
		function askForSaveAndProceed: boolean;
    function askForSaveAtExitAndProceed: boolean;
    procedure saveCurrentGroupsFile;
    procedure saveCurrentTemplatesFile;
    procedure saveCurrentToolsFile;
    procedure GlobalExceptionHandler(Sender: TObject; E: Exception);
    procedure switchUnitSystem;
    procedure streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
    procedure redrawPlantToUpdateBoundsRectForPossibleChange(plant: GsPlant);
    procedure respondToSimulationRunningStatus(isRunning: boolean);
    procedure updateDeletedLoggedVars;
  end;

{ gdi resources are bitmaps, handles, brushes, pens, etc; user resources are window and menu handles }
const
  kDefaultCriticalGDIFreeResourcePercent = 30;
  kDefaultCriticalUserFreeResourcePercent = 20;
  kGDIResources = 1;
  kUserResources = 2;
  kGDIAndUserResources = 3;

function resourcesAreLowInWin31(checkType: smallint; windowName: string): boolean;
procedure addToolToToolChoices(each: TObject; data: TObject);
procedure drawToolExceptCurrent(each: TObject; data: TObject);

var
  GardenForm: TGardenForm;
  globalFirstUpdateForNewDomain: boolean;

const
	kRedrawDaily = 0;
  kRedrawWeekly = 1;
  kRedrawMonthly = 2;
  kRedrawAtStop = 3;

  kHarvestPanelHeight = 32;

implementation

uses Clipbrd, Printers, umusic, uegarden, ueutils, ueweath, utooledt, ugrped,
	udebug, uebag, uturt3d, usound, ureseed, udisplay, ubackdrp, usupport, ubitmap,
  utlpmed, usplash, uharved, uabout, uexit, uwait, ugscom;
{, unotes; }

{$R *.DFM}

{ ------------------------------------------------------------------------------------------------- functions }
function resourcesAreLowInWin31(checkType: smallint; windowName: string): boolean;
  var
    freeGDIResources, freeUserResources: word;
    prompt: string;
  begin
{$IFDEF WINDOWS}
  result := true;
  case checkType of
     kGDIResources:
       begin
       freeGDIResources := GetFreeSystemResources(GFSR_GDIRESOURCES);
       if (freeGDIResources <= Domain.criticalGDIFreeResourcePercent) then
         begin
         prompt := 'The ' + windowName + ' uses a lot of GDI resources. You have '
            + intToStr(freeGDIResources) + '% of GDI resources free ('
            + intToStr(Domain.criticalGDIFreeResourcePercent) + '% needed). '
            + 'You must close one or more Windows applications and try again.';
          messageDlg(prompt, mtError, [mbOK], 0);
          exit;
         end;
       end;
     kUserResources:
       begin
       freeUserResources := GetFreeSystemResources(GFSR_USERRESOURCES);
       if (freeUserResources <= Domain.criticalUserFreeResourcePercent) then
         begin
         prompt := 'The ' + windowName + ' uses a lot of user resources. You have '
            + intToStr(freeUserResources) + '% of user resources free ('
            + intToStr(Domain.criticalUserFreeResourcePercent) + '% needed). '
            + 'You must close one or more Windows applications and try again.';
         messageDlg(prompt, mtError, [mbOK], 0);
         exit;
         end;
       end;
     kGDIAndUserResources:
       begin
       freeGDIResources := GetFreeSystemResources(GFSR_GDIRESOURCES);
       freeUserResources := GetFreeSystemResources(GFSR_USERRESOURCES);
       if (freeGDIResources <= Domain.criticalGDIFreeResourcePercent)
          or (freeUserResources <= Domain.criticalUserFreeResourcePercent) then
         begin
         prompt := 'The ' + windowName + ' uses a lot of resources. You have '
            + intToStr(freeGDIResources) + '% of GDI resources free ('
            + intToSTr(Domain.criticalGDIFreeResourcePercent) + '% needed) and '
            + intToStr(freeUserResources) + '% of user resources free ('
            + intToStr(Domain.criticalUserFreeResourcePercent) + '% needed). '
            + 'You must close one or more Windows applications and try again.';
         messageDlg(prompt, mtError, [mbOK], 0);
         exit;
         end;
       end;
     end;
  result := false;
{$ELSE}
  result := false;
{$ENDIF}
  end;

{ ------------------------------------------------------------------------------------------------- create/destroy }
procedure TGardenForm.FormCreate(Sender: TObject);
	begin
  Application.OnException := self.GlobalExceptionHandler;
  { this statement makes all the Delphi message dialog buttons (with messageDlg) not have glyphs }
{$IFDEF WINDOWS}
  MsgDlgGlyphs := false;
{$ENDIF}
	self.initializeStuff;
	end;

destructor TGardenForm.Destroy;
  begin
  commandList.free;
  commandList := nil;
  { do not free harvestPanel because owner (self) will free it }
  harvestPanel := nil;
  inherited destroy;
  end;

procedure TGardenForm.initializeStuff;
	begin
  GardenPaintBox := GsPaintBoxWithPalette.create(self);
  GardenPaintBox.parent := GardenImageScrollBox;
  GardenPaintBox.onPaint := self.GardenPaintBoxPaint;
  GardenPaintBox.onMouseDown := self.FormMouseDown;
  GardenPaintBox.onMouseMove := self.FormMouseMove;
  GardenPaintBox.onMouseUp := self.FormMouseUp;
  Application.helpFile := Domain.exeDirectory + 'gwi.hlp';
  gardenPaintBox.cursor := crArrowGarden;
	gardenShouldBeInvalidatedOnUpdate := false;
  isModelAddedOrRemoved := false;
  hintActive := false;
  lastHintString := '';
  application.hintPause := 1500;
  expanded := true;
  expandedSize := point(self.width, self.height);
  collapsedSize.x := statusPanel.left + statusPanel.width;
  collapsedSize.y := 0; {will be ignored}
  copyrightString := 'Copyright 1997 Paul D. Fernhout and Cynthia F. Kurtz All Rights Reserved';
  if copyrightCheck(copyrightString) <> 6307 then
    begin
    ShowMessage('The copyright string has been tampered with.' + chr(13) + chr(13)
      + 'Unauthorized users and distributors of this program' + chr(13)
      + 'are subject to criminal prosecution' + chr(13)
      + 'to the fullest extent possible under the law.');
    Halt(1);
    end;
  running := false;
  MenuStop.enabled := running;
  redrawFrequency := kRedrawWeekly;
  self.updateRedrawFrequencyMenu;
  actionInProgress := false;
  { update play startup music menu item }
  MenuPlayStartupMusic.checked := Domain.playStartupMusic;
  MenuPlayStartupMusic.caption := 'Play startup music' + Domain.musicFileNameForMenu;
  { update unit options }
  MenuMetricUnits.checked := Domain.menuOptions.showMetricUnits;
  MenuEnglishUnits.checked := not Domain.menuOptions.showMetricUnits;
  {create bitmap for harvest icon for use in testing pixels}
  self.harvestIconDefaultBitmap.picture.bitmap.width := self.harvestIconDefault.picture.icon.width;
  self.harvestIconDefaultBitmap.picture.bitmap.height := self.harvestIconDefault.picture.icon.height;
  self.harvestIconDefaultBitmap.picture.bitmap.canvas.draw(0, 0, self.harvestIconDefault.picture.icon);
  { plant harvest panel }
  self.harvestPanel := GsHarvestItemPanel.create(self);
  self.harvestPanel.parent := self;
  self.resizeHarvestPanel;
  gardenPaintBox.left := 0;
  gardenPaintBox.top := 0;
  gardenPaintBox.width := Domain.gardenSize.x;
  gardenPaintBox.height :=  Domain.gardenSize.y;
  commandList := KfCommandList.create;
  commandList.setNewUndoLimit(Domain.menuOptions.undoLimit);
  currentTool := Domain.toolManager.gloveTool;
	self.populateTools;
  self.updateToolChoicesForCurrentTool;
  self.updateMenu;
	self.invalidateEntireGarden;
  self.paintGardenImage;
  self.updateOptionsMenu;
  Application.OnHint := DoHint;
  Application.OnShowHint := DoShowHint;
  Application.showHint := true;
  self.statusClear;
  self.updateDateButtons;
  self.resizeForGardenBitmapChange;
  self.updateFormCaption;
  self.handleUpdateHarvestPanel;
	end;

procedure TGardenForm.MenuPlayStartupMusicClick(Sender: TObject);
  var
    fileNameWithPath: string;
  begin
  { switch from checked to none, or from unchecked to choosing file }
  if MenuPlayStartupMusic.checked then
    Domain.startupMusicFileName := 'none'
  else
    begin
    fileNameWithPath := '';
    fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeMidiFile, Domain.startupMusicFileName);
    if fileNameWithPath <> '' then
      begin
      Domain.startupMusicFileName := fileNameWithPath;
      iniFileMightHaveChanged := true;
      end;
    end;
  MenuPlayStartupMusic.checked := Domain.playStartupMusic;
  MenuPlayStartupMusic.caption := 'Play startup music' + Domain.musicFileNameForMenu;
  end;

procedure TGardenForm.GlobalExceptionHandler(Sender: TObject; E: Exception);
  begin
  if e is EMathError then
    errorMessage('Unhandled math exception: ' + e.message)
  else
    Application.ShowException(e);
  end;

function TGardenForm.copyrightCheck(var aString: string): longint;
  var
   	sum: longint;
   	i: integer;
  begin
  sum := 0;
  for i := 1 to length(aString) do
    sum := sum + ord(aString[i]);
  result := sum;
  end;

{ -------------------------------------------------------------------------------------------- streaming }
procedure TGardenForm.streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var oldExpanded, newExpanded: boolean;
  begin
  { the cvir passed in is the windowManager cvir, because this object is not a streamable object }
  { stream expanded flag first because it is used in resizing the form }
  if filer.isReading then
    begin
    { reset harvest panel so if it gets a paint message while streaming is taking place,
      it will not have a nonsense pointer }
    oldExpanded := expanded;
    filer.streamBoolean(newExpanded);
    if oldExpanded <> newExpanded then
      self.collapseExpandClick(self);
    end
  else
    filer.streamBoolean(expanded);
  StreamFormPositionInfoWithFiler(filer, cvir, self);
  end;

procedure TGardenForm.updateForNewDomain;
  begin
  { when the garden file is created while streaming for some reason it cannot add objects to the
    toolChoices combobox (perhaps because it is not set up yet), so do this later if it was not done while streaming.
    I don't like this workaround and it seems strange, but could fix it better later. }
  if toolChoices.items.count <= 0 then
    begin
    self.populateTools;
    self.updateToolChoicesForCurrentTool;
    end;
  { don't redraw if first time loading domain, unless form is collapsed }
	if (not globalFirstUpdateForNewDomain) then self.redrawGarden;
  browserForm.updateForNewDomain;
  graphForm.updateForNewDomain;
  harvestForm.updateForNewDomain;
  self.updateDateButtons;
  self.updateFormCaption;
  self.handleUpdateHarvestPanel;
  self.updateOptionsMenu;
  end;

{ -------------------------------------------------------------------------------------------- movement related }
const
  hintRange = 1;
  maxHintRangeForLargeAreas = 50; {in each direction, so box is 100 x 100}

procedure TGardenForm.updateObjectsNowOver(point: TPoint);
	begin
  toolNowOver := nil;
  soilPatchNowOver := nil;
  plantNowOver := nil;
  if (Domain.menuOptions.showTools) and (currentTool = Domain.toolManager.gloveTool) then
  	toolNowOver := Domain.toolManager.findToolAtXY(point.x, point.y, nil);
  soilPatchNowOver := Domain.garden.findSoilPatch(point) as GsSoilPatch;
  if soilPatchNowOver <> nil then
  	plantNowOver := soilPatchNowOver.findPlant(point) as GsPlant;
  end;

function TGardenForm.hintForObjectsNowOver: string;
  var
    overWhat: smallint;
    gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants: boolean;
	begin
  result := '';
  if plantNowOver <> nil then
    overWhat := kOverPlant
  else if soilPatchNowOver <> nil then
    overWhat := kOverPatch
  else
    overWhat := kOverWeather;
  if (toolNowOver <> nil) then
    result := toolNowOver.getHintIfOver(menuShowLongHints.checked)
  else if (menuShowtoolhowtohints.checked) then
    begin
    gardenHasPatches := Domain.garden.soilPatchList.count > 0;
    atLeastTwoPatches := Domain.garden.soilPatchList.count > 1;
    gardenHasPlants := Domain.garden.plantCount > 0;
    if soilPatchNowOver <> nil then
      patchHasPlants := soilPatchNowOver.GetNumPlantsNotAwaitingReseeding > 0
    else
      patchHasPlants := false;
    if currentTool <> nil then
      result := currentTool.getHowToHintIfUsing(overWhat,
          gardenHasPatches, atLeastTwoPatches, gardenHasPlants, patchHasPlants);
    end
  else if overWhat = kOverPlant then
    result := plantNowOver.getHint(menuShowLongHints.checked)
  else if overWhat = kOverPatch then
    result := soilPatchNowOver.getHint(menuShowLongHints.checked, Domain.menuOptions.showMetricUnits)
  else if (domain <> nil) and (domain.garden <> nil) and (domain.garden.weather <> nil) then
    result := domain.garden.weather.getHint(menuShowLongHints.checked, Domain.menuOptions.showMetricUnits);
  end;

procedure TGardenForm.setCursorForObjectsNowOver;
	begin
  if toolNowOver <> nil then
    gardenPaintBox.cursor := crArrowGarden
  else if plantNowOver <> nil then
  	gardenPaintBox.cursor := crArrowPlant
  else if soilPatchNowOver <> nil then
    gardenPaintBox.cursor := crArrowSoil
  else
    gardenPaintBox.cursor := crArrowGarden;
  end;

procedure TGardenForm.DoHint(Sender: TObject);
  begin
  end;

{$IFDEF WINDOWS}
    procedure TGardenForm.DoShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
{$ELSE}
    procedure TGardenForm.DoShowHint(var HintStr: ansistring; var CanShow: Boolean; var HintInfo: THintInfo);
{$ENDIF}
  begin
  if self.running then
    begin
    HintStr := '';
    exit;
    end;
  hintInfo.hintPos := hintInfo.hintControl.clientToScreen(Point(HintInfo.CursorPos.x + 20, HintInfo.CursorPos.y + 20));
  hintInfo.hintMaxWidth := 200;
  if HintInfo.HintControl = GardenPaintBox then
    begin
  	HintStr := self.hintForObjectsNowOver;
    lastHintString := HintStr;
    hintActive := true;
    hintX := HintInfo.CursorPos.x;
    hintY := HintInfo.CursorPos.y;
    end
  else
    begin
    if (Domain <> nil) and (Domain.hintManager <> nil) then
      hintStr := Domain.hintManager.hintForComponentName(hintInfo, menuShowLongHints.checked,
          menuShowHintsForAspects.checked);
    if (hintStr = '') and (hintInfo.hintControl.showHint) then
      hintStr := hintInfo.hintControl.hint;
    end;
  end;

function TGardenForm.iconForPlantNowOver(actionType: longint): TIcon;
  begin
  result := nil;
  if plantNowOver <> nil then
    begin
    result := plantNowOver.drawingPlant.iconForHarvesting(actionType = kToolActionHarvestWholePlant);
    end;
  end;

function TGardenForm.currentToolActionObject: GsToolAction;
  begin
  result := currentObjectInComboBox(toolActionChoices) as GsToolAction;
  end;

function TGardenForm.currentToolParam1Object: TObject;
  begin
  result := currentObjectInComboBox(toolParam1Choices);
  end;

function TGardenForm.currentToolParam2Object: TObject;
  begin
  result := currentObjectInComboBox(toolParam2Choices);
  end;

procedure TGardenForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    newCommand: KfCommand;
    anchorPoint: TPoint;
  begin
  if actionInProgress then
    try    {error somewhere - user did weird things with mouse buttons - teminate action...}
    self.FormMouseUp(sender, button, shift, x, y);
    finally
    actionInProgress := false;
    end;
  rightButtonPressed := false;
  if self.running then
  	begin
    self.terminateRun;
    end;
  if (Button = mbRight) or ((Button = mbLeft) and (ssShift in Shift)) then
    begin
  	rightButtonPressed := true;
    if Domain.menuOptions.showTools then
    	self.selectTool(x, y);
    end
  else
    begin
    {if Domain.menuOptions.playToolSounds then
      currentTool.soundUsing.playLoopIfDone; - will get turned off anyway when action stops}
    anchorPoint := Point(x, y);
    actionInProgress := true;
    newCommand := self.makeMouseCommand(anchorPoint);
    if newCommand <> nil then
      begin
    	actionInProgress := commandList.mouseDown(newCommand, anchorPoint);
      if actionInProgress and Domain.menuOptions.playToolSounds then
    	  currentTool.soundDown.play;
      end
    else
      actionInProgress := false;
    end;
	self.handlePossibleGardenChange;
	end;

procedure TGardenForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var shouldCancelHint: boolean;
  begin
  if self.running then exit;
  if rightButtonPressed then exit;
  if not self.running then
    begin
    self.updateObjectsNowOver(point(x, y));
    shouldCancelHint := false;
    if hintActive and ((x < hintX - hintRange) or (x > hintX + hintRange) or
  	  (y < hintY - hintRange) or (y > hintY + hintRange)) then
      begin
      if self.hintForObjectsNowOver <> lastHintString then
        shouldCancelHint := true;
      if ((x < hintX - maxHintRangeForLargeAreas) or (x > hintX + maxHintRangeForLargeAreas) or
  	    (y < hintY - maxHintRangeForLargeAreas) or (y > hintY + maxHintRangeForLargeAreas)) then
        shouldCancelHint := true;
      if shouldCancelHint then
        begin
  		  Application.cancelHint;
    	  hintActive := false;
        end;  
      end;
    self.setCursorForObjectsNowOver;
    end;
  self.invalidateCurrentToolRect;
  self.invalidateHarvestIconRectIfNeeded;
  currentTool.x := x;
  currentTool.y := y;
  {if (not (ssRight in shift)) then}
  	begin
  	if actionInProgress then
    	begin
  		commandList.mouseMove(Point(x, y));
  		if Domain.menuOptions.playToolSounds then
    		currentTool.soundUsing.playLoopIfDone;
    	end
  	else
   	 begin
  		currentTool.moveOver(Point(x, y));
  		if Domain.menuOptions.playToolSounds then
    		currentTool.soundMoving.playLoopIfDone;
    	end;
    end;
  self.invalidateCurrentToolRect;
	self.handlePossibleGardenChange;
	end;

procedure TGardenForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
	begin
  try
  if not rightButtonPressed then
  	begin
  	if actionInProgress then
  		begin
    	if currentTool.soundUsing.isUsed then
      	GsSound.stopAny;
    	commandList.mouseUp(Point(x, y));
    	actionInProgress := false;
  		if Domain.menuOptions.playToolSounds then
    		currentTool.soundUp.play;
    	end
  	else
    	begin
    	if currentTool.soundMoving.isUsed then
      	GsSound.stopAny;
    	end;
	  self.handlePossibleGardenChange;
    end;
  finally
  rightButtonPressed := false;
  actionInProgress := false;  {make sure this is cleared if there is a problem}
  end;
	end;

{testing icon dragging}
procedure TGardenForm.testDragIconMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  var thePoint: TPoint;
	begin
	thePoint := (sender as TImage).clientToScreen(Point(x,y));
	thePoint := (sender as TImage).parent.screenToClient(thePoint);
	(sender as TImage).top := thePoint.y - 16;
	(sender as TImage).left := thePoint.x - 16;
	end;

{ -------------------------------------------------------------------------------------------- tool handling }
procedure TGardenForm.selectTool(x: integer; y: integer);
  var
  tool: GsTool;
  begin
  if currentTool = Domain.toolManager.gloveTool then
  	tool := Domain.toolManager.findToolAtXY(x, y, nil)
  else
    tool := nil;
  if (tool = nil) then
    tool := Domain.toolManager.gloveTool
  else
  	begin
    offsetPoint.x := Domain.toolManager.gloveTool.x - tool.x;
    offsetPoint.y := Domain.toolManager.gloveTool.y - tool.y;
  	end;
  self.setCurrentTool(tool, false);
  end;

procedure TGardenForm.setCurrentTool(tool: GSTool; returnToStart: boolean);
  var
  	oldTool: GsTool;
  begin
  if (tool <> nil) and (tool <> currentTool) then
  	begin
    { temporarily set current tool to update combo boxes, then set back to what it was }
    oldTool := currentTool;
    currentTool := tool;
    self.updateToolChoicesForCurrentTool;
    currentTool := oldTool;
    {PDF FIX - second sound will cut off first}
  	if Domain.menuOptions.playToolSounds then
      begin
      if tool.soundPickUp.isUsed then
      	currentTool.soundPutDown.playWait
      else
      	currentTool.soundPutDown.play;
    	tool.soundPickUp.play;
      end;
  	self.invalidateCurrentToolRect;
    Domain.toolManager.bringToolToFront(tool);
  	currentTool.setState(kToolStateDown);
    if (currentTool <> Domain.toolManager.gloveTool) then
    	begin
      if returnToStart then
      	begin
        currentTool.x := Domain.toolManager.gloveTool.x;
        currentTool.y := Domain.toolManager.gloveTool.y;
        end;
      self.invalidateCurrentToolRect;
      end;
  	currentTool := tool;
  	if (currentTool <> Domain.toolManager.gloveTool) then self.invalidateCurrentToolRect;
  	currentTool.setState(kToolStateUp);
    if (currentTool = Domain.toolManager.gloveTool) then
    	begin
      Domain.toolManager.gloveTool.x := oldTool.x + offsetPoint.x;
      Domain.toolManager.gloveTool.y := oldTool.y + offsetPoint.y;
      end;
  	self.invalidateCurrentToolRect;
    if (tool <> Domain.toolManager.gloveTool) then
    	begin  {store old position}
      Domain.toolManager.gloveTool.x := tool.x;
      Domain.toolManager.gloveTool.y := tool.y;
      end;
    self.constrainCurrentToolPositionToWindow;
  	self.invalidateCurrentToolRect;
    self.cursorJumpToCurrentTool;
	  self.handlePossibleGardenChange;
    self.repaint;
    end;
  end;

procedure TGardenForm.constrainCurrentToolPositionToWindow;
  begin
  if currentTool = nil then exit;
  with currentTool do
    begin
    if x >= gardenImageScrollBox.HorzScrollBar.position + gardenImageScrollBox.clientWidth then
      x := gardenImageScrollBox.HorzScrollBar.position + gardenImageScrollBox.clientWidth - 1;
    if x < gardenImageScrollBox.HorzScrollBar.position then
    	x := gardenImageScrollBox.HorzScrollBar.position;
    if y >= gardenImageScrollBox.VertScrollBar.position + gardenImageScrollBox.clientHeight then
      y := gardenImageScrollBox.VertScrollBar.position + gardenImageScrollBox.clientHeight - 1;
    if y < gardenImageScrollBox.VertScrollBar.position then
    	y := gardenImageScrollBox.VertScrollBar.position;
    end;
 end;

procedure TGardenForm.cursorJumpToCurrentTool;
  var	jumpPoint : TPoint;
  begin
  if not Domain.menuOptions.showTools then exit;
  {constrain current tool point to paint box}
	jumpPoint := GardenPaintBox.ClientToScreen(Point(currentTool.x, currentTool.y));
	SetCursorPos(jumpPoint.x, jumpPoint.y);
  end;

{ -------------------------------------------------------------------------------------------- drawing }
procedure TGardenForm.drawCurrentTool;
	begin
  Domain.gardenBitmap.canvas.brush.color := clWhite;
  if Domain.menuOptions.showTools then
  	currentTool.drawOn(Domain.gardenBitmap);
  end;

procedure drawToolExceptCurrent(each: TObject; data: TObject);
  var
  tool : GsTool;
  intersect: TRect;
{$IFDEF WINDOWS}
  intersectResult: integer;
{$ELSE}
  intersectResult: longbool;
{$ENDIF}
  begin
  Domain.gardenBitmap.canvas.brush.color := clWhite;
  tool := each as GsTool;
  if (tool <> GardenForm.currentTool) and (tool <> Domain.toolManager.gloveTool) then
    begin
    intersectResult := IntersectRect(intersect, tool.boundsRect, GardenForm.invalidRectForTools);
{$IFDEF WINDOWS}
  	if (intersectResult <> 0) then
{$ELSE}
  	if intersectResult then
{$ENDIF}
    	begin
      {if debugForm <> nil then DebugPrint(tool.name);  }
    	if Domain.menuOptions.showTools then
				tool.drawOn(Domain.gardenBitmap);
      end;
    end;
  end;

procedure TGardenForm.drawToolsExceptCurrent;
  begin
  Domain.toolManager.tools.forEach(drawToolExceptCurrent, nil);
  end;

{note that calls to update status and set wait cursor arranged so only occur if
a non-iconic plant is being drawn}
procedure TGardenForm.paintGardenImage;
  var
    icon: TIcon;
    {oldPalette: HPALETTE; }
    {newPalette: HPALETTE; }
	begin
  if not expanded then exit;
  {newPalette := 0;}
  if (invalidRectForGarden.top <> 0) or (invalidRectForGarden.left <> 0) or
    (invalidRectForGarden.bottom <> 0) or (invalidRectForGarden.right <> 0) then
    begin
  	{cursor_startWait;  this will be done conditionally only if plant to draw and not icon}
  	{statusUpdateLabel('drawing...'); done by plants}
  (*	if Domain.paletteBitmapLoaded then
    	begin
      {newPalette := CopyPalette(Domain.paletteBitmap.palette); }
    	{oldPalette := SelectPalette(Domain.plantBitmap.canvas.handle, newPalette, false);}
    	oldPalette := SelectPalette(Domain.plantBitmap.canvas.handle, Domain.paletteBitmap.palette, false);
    	RealizePalette(Domain.plantBitmap.canvas.handle);
    	end; *)
    try
  	IntersectClipRect(Domain.plantBitmap.canvas.handle, invalidRectForGarden.left, invalidRectForGarden.top,
    	invalidRectForGarden.right,invalidRectForGarden.bottom);
    if Domain.fileOptions.backdropUsed and (Domain.gardenBackdropBitmap <> nil) and
    	Domain.menuOptions.showBackdrop and (Domain.gardenBackdropBitmap.width > 0) then
      begin
      Domain.plantBitmap.canvas.copyMode := cmSrcCopy;
			Domain.plantBitmap.canvas.copyRect(self.invalidRectForGarden, Domain.gardenBackdropBitmap.canvas,
      	self.invalidRectForGarden);
      end
    else
      begin
      Domain.plantBitmap.canvas.brush.color := Domain.menuOptions.backgroundColor;
      Domain.plantBitmap.canvas.brush.style := bsSolid;
			Domain.plantBitmap.canvas.fillRect(self.invalidRectForGarden);
      end;
    {put plants and soil onto plant bitmap}
  	if self.statusPanel.visible then statusUpdateLabel('drawing...');
  	Domain.garden.drawOnInvalidRect(Domain.plantBitmap.canvas, invalidRectForGarden);
  	invalidRectForGarden := Bounds(0,0,0,0);
    finally
  (*  if Domain.paletteBitmapLoaded then
      begin
    	SelectPalette(Domain.plantBitmap.canvas.handle, oldPalette, true);
      RealizePalette(Domain.plantBitmap.canvas.handle);
      {DeleteObject(newPalette);}
      end;    *)
 		statusClear;
  	cursor_stopWait;
    end;
    end;      
  {if garden needs to be redrawn, tool area will always include that area}
	if (invalidRectForTools.top <> 0) or (invalidRectForTools.left <> 0) or
  	(invalidRectForTools.bottom <> 0) or (invalidRectForTools.right <> 0) then
    begin
    try
  	IntersectClipRect(Domain.gardenBitmap.canvas.handle, invalidRectForTools.left, invalidRectForTools.top,
  		invalidRectForTools.right,invalidRectForTools.bottom);
    { copy plant bitmap to garden bitmap}
    Domain.gardenBitmap.canvas.copyMode := cmSrcCopy;
		Domain.gardenBitmap.canvas.copyRect(self.invalidRectForTools, Domain.plantBitmap.canvas,
    	self.invalidRectForTools);
  	Domain.garden.drawOrganicMatterOnInvalidRect(Domain.gardenBitmap.canvas, invalidRectForTools);
		self.drawToolsExceptCurrent;
		self.drawCurrentTool;
    if isDrawingHarvestIcons then
      begin
    	icon := self.iconForPlantNowOver(currentTool.currentAction.actionType);
    	if icon <> nil then
      	begin
      	drawIcon(Domain.gardenBitmap.canvas.handle, plantNowOver.basePoint.x, plantNowOver.basePoint.y - 32,
        	icon.handle);
      	end;
      end;
    {force immediate redrawing of affected area if got this far}
    gardenPaintBox.canvas.copyRect(invalidRectForTools, Domain.gardenBitmap.canvas, invalidRectForTools);
  	invalidRectForTools := Bounds(0,0,0,0);
    finally
    end;
    end;
	end;

procedure TGardenForm.GardenPaintBoxPaint(Sender: TObject);
	var
  aRect: TRect;
  {redrawNeeded: boolean; }
  oldPalette: HPALETTE;
	begin
  {self.paintGardenImage; }
  {redrawNeeded := GetUpdateRect(self.handle, aRect, false);}
  {paint validates update rect - so always empty - need to assume clipping rect set correctly}
  aRect := Rect(0, 0, Domain.gardenBitmap.width, Domain.gardenBitmap.height);
	{if redrawNeeded then }
  {if Domain.paletteBitmapLoaded then
    begin
    oldPalette := SelectPalette(gardenPaintBox.canvas.handle, Domain.paletteBitmap.palette, false);
    RealizePalette(gardenPaintBox.canvas.handle);
  	gardenPaintBox.canvas.copyRect(aRect, Domain.gardenBitmap.canvas, aRect);
    SelectPalette(gardenPaintBox.canvas.handle, oldPalette, true);
    end
  else }
  	gardenPaintBox.canvas.copyRect(aRect, Domain.gardenBitmap.canvas, aRect);
	end;

procedure TGardenForm.redrawGarden;
	begin
  self.invalidateEntireGarden;
  self.paintGardenImage;
  self.repaint;
  end;

{ -------------------------------------------------------------------------------------------- invalidating }
function TGardenForm.isDrawingHarvestIcons: boolean;
  begin
  result := false;
  if self.running then exit;
  if currentTool = nil then exit;
  if currentTool.currentAction = nil then exit;
  result :=
  	(currentTool.currentAction.actionType = kToolActionHarvestWholePlant) or
    	(currentTool.currentAction.actionType = kToolActionHarvestMatureParts);
  end;

procedure TGardenForm.invalidateHarvestIconRectIfNeeded;
  begin
  if not isRectEmpty(plantLastOverRect) then
    self.invalidateToolRect(plantLastOverRect);
  if (plantNowOver <> nil) and self.isDrawingHarvestIcons then
    begin
    plantLastOverRect := Rect(plantNowOver.basePoint.x, plantNowOver.basePoint.y - 32,
    	plantNowOver.basePoint.x + 32, plantNowOver.basePoint.y);
    self.invalidateToolRect(plantLastOverRect);
    end
  else
    plantLastOverRect := Rect(0,0,0,0);
  end;

procedure TGardenForm.invalidateCurrentToolRect;
  var
    toolRect : TRect;
	begin
  toolRect := currentTool.boundsRect;
  if Domain.menuOptions.showTools then
		self.invalidateToolRect(toolRect);
  end;

procedure TGardenForm.redrawPlantToUpdateBoundsRectForPossibleChange(plant: GsPlant);
  begin
  if (plant <> nil) and (Domain <> nil) and (Domain.plantBitmap <> nil) and (Domain.plantBitmap.canvas <> nil) then
    begin
    plant.computeBounds := true;
    plant.drawOn(Domain.plantBitmap.canvas);
    plant.computeBounds := false;
    end;
  end;

procedure TGardenForm.invalidateGardenRect(rect: TRect);
  var
    newRect : TRect;
	begin
  (*
  {for now - forcing entire rect to be invalid - since not computing plant bounding rect}
  rect.top := 0;
  rect.left := 0;
  rect.right := Domain.gardenBitmap.width;
  rect.bottom := Domain.gardenBitmap.height;
  *)
  {adjust rect for scroll}
  {rect.top := rect.top + gardenImageScrollBox.VertScrollBar.position;
  rect.bottom := rect.bottom + gardenImageScrollBox.VertScrollBar.position;
  rect.left := rect.left + gardenImageScrollBox.HorzScrollBar.position;
  rect.right := rect.right + gardenImageScrollBox.HorzScrollBar.position;}
  UnionRect(newRect, rect, invalidRectForGarden);
  invalidRectForGarden := newRect;
  {newRect.top := newRect.top - gardenImageScrollBox.VertScrollBar.position;
  newRect.bottom := newRect.bottom - gardenImageScrollBox.VertScrollBar.position;
  newRect.left := newRect.left - gardenImageScrollBox.HorzScrollBar.position;
  newRect.right := newRect.right - gardenImageScrollBox.HorzScrollBar.position;
  InvalidateRect(gardenPaintBox.parent.Handle, @newRect, false); }
  {everything invalid in garden effects tools}
  self.invalidateToolRect(rect);
  {InvalidateRect(gardenPaintBox.Handle, nil, false);}
  {gardenPaintBox.invalidate;}
  end;

procedure TGardenForm.invalidateToolRect(rect: TRect);
  var
    newRect : TRect;
	begin
  UnionRect(newRect, rect, invalidRectForTools);
  invalidRectForTools := newRect;
 { newRect.top := newRect.top - gardenImageScrollBox.VertScrollBar.position;
  newRect.bottom := newRect.bottom - gardenImageScrollBox.VertScrollBar.position;
  newRect.left := newRect.left - gardenImageScrollBox.HorzScrollBar.position;
  newRect.right := newRect.right - gardenImageScrollBox.HorzScrollBar.position;
  InvalidateRect(gardenPaintBox.parent.Handle, @newRect, false);  }
  {InvalidateRect(gardenPaintBox.Handle, nil, false);}
  {gardenPaintBox.invalidate;}
  end;

procedure TGardenForm.invalidateEntireGarden;
	var
	aRect: TRect;
	begin
  aRect := Rect(0, 0, Domain.gardenBitmap.width, Domain.gardenBitmap.height);
  self.invalidateGardenRect(aRect);
	end;

procedure TGardenForm.invalidateEntireToolRect;
	var
	aRect: TRect;
	begin
  aRect := Rect(0, 0, Domain.gardenBitmap.width, Domain.gardenBitmap.height);
  self.invalidateToolRect(aRect);
	end;

{ -------------------------------------------------------------------------------------------- resizing }
procedure TGardenForm.FormResize(Sender: TObject);
  var
    maxHeight: longint;
  begin
  if application.terminated then exit;
  if harvestPanel = nil then
    exit;
  if not expanded then
    self.clientHeight := statusPanel.top + statusPanel.height
  else
    begin
    self.resizeTopPanel(true);
    self.resizeHarvestPanel;
    maxHeight := topPanel.Height + harvestPanel.height + Domain.gardenSize.y;
    if self.clientHeight > maxHeight then self.clientHeight := maxHeight;
    if self.clientWidth > Domain.gardenSize.x then self.clientWidth := Domain.gardenSize.x;
    with GardenImageScrollBox do setBounds(0, topPanel.height,
      self.clientWidth, harvestPanel.top - topPanel.height);
    end;
  end;

const
  kStatusPanelMinWidth = 50;
  kToolAndActionChoicesMinWidth = 250;
  kToolParamsChoicesMinWidth = 250;

function TGardenForm.minTopPanelWidthFirstLine: integer;
  begin
  result := collapseExpand.width + weekButton.width + monthButton.width + dayButton.width
    + yearButton.width + kStatusPanelMinWidth;
  end;

function TGardenForm.comboBoxMinWidth(comboBox: TComboBox): smallint;
  var
    i: longint;
    lineWidth: smallint;
  begin
  result := 0;
  if comboBox = nil then exit;
  if comboBox.items.count <= 0 then exit;
  if not comboBox.visible then exit; { if invisible, don't count }
  for i := 0 to comboBox.items.count - 1 do
    begin
    { assumption is that combo box has same font as form }
    lineWidth := self.canvas.textWidth(comboBox.items[i]);
    if lineWidth > result then result := lineWidth;
    end;
  result := result + 25;  { arrow button }
  end;

function TGardenForm.minTopPanelWidthSecondLine: integer;
  begin
  result := comboBoxMinWidth(toolChoices) + comboBoxMinWidth(toolActionChoices)
    + comboBoxMinWidth(toolParam1Choices) + comboBoxMinWidth(toolParam2Choices);
  end;

function TGardenForm.resizeTopPanel(inFormResize: boolean): boolean;
  var
    topStatus, leftStatus,
    topToolAndAction, widthTool, widthAction,
    topParam1, leftParam1, widthParam1, widthParam2,
    minWidthTool, minWidthAction, minWidthParam1, minWidthParam2: longint;
    propMinWidthTool, propMinWidthParam1: single;
    firstLineSplit, secondLineSplit: boolean;
  begin
  result := false;
  with topPanel do setBounds(0, 0, clientRect.right - clientRect.left, height);
  firstLineSplit := topPanel.width < self.minTopPanelWidthFirstLine;
  if firstLineSplit then
    begin
    topStatus := collapseExpand.top + collapseExpand.height;
    leftStatus := 0;
    end
  else
    begin
    topStatus := 0;
    leftStatus := yearButton.left + yearButton.width;
    end;
  topToolAndAction := topStatus + statusPanel.height;
  secondLineSplit := topPanel.width < self.minTopPanelWidthSecondLine;
  if inFormResize then
    self.toolLineIsSplit := secondLineSplit
  else
    begin
    if (secondLineSplit <> self.toolLineIsSplit) and (not self.toolLineIsSplit) then result := true;
    secondLineSplit := self.toolLineIsSplit or secondLineSplit;
    end;
  if secondLineSplit then
    begin
    minWidthTool := comboBoxMinWidth(toolChoices);
    minWidthAction := comboBoxMinWidth(toolActionChoices);
    propMinWidthTool := 1.0 * minWidthTool / (minWidthTool + minWidthAction);
    widthTool := intMin(round(topPanel.width * propMinWidthTool), minWidthTool);
    widthAction := intMin(topPanel.width - widthTool, minWidthAction);
    topParam1 := topToolAndAction + toolChoices.height;
    leftParam1 := 0;
    minWidthParam1 := comboBoxMinWidth(toolParam1Choices);
    minWidthParam2 := comboBoxMinWidth(toolParam2Choices);
    if minWidthParam1 + minWidthParam2 <> 0 then
      propMinWidthParam1 := 1.0 * minWidthParam1 / (minWidthParam1 + minWidthParam2)
    else
      propMinWidthParam1 := 1.0;
    widthParam1 := intMin(round(topPanel.width * propMinWidthParam1), minWidthParam1);
    widthParam2 := intMin(topPanel.width - widthParam1, minWidthParam2);
    end
  else
    begin
    widthTool := comboBoxMinWidth(toolChoices);
    widthAction := comboBoxMinWidth(toolActionChoices);
    topParam1 := topToolAndAction;
    leftParam1 := widthTool + widthAction;
    widthParam1 := comboBoxMinWidth(toolParam1Choices);
    widthParam2 := comboBoxMinWidth(toolParam2Choices);
    end;
  with statusPanel do setBounds(leftStatus, topStatus, topPanel.width - leftStatus, height);
  with toolChoices do setBounds(0, topToolAndAction, widthTool, height);
  with toolActionChoices do setBounds(toolChoices.width, topToolAndAction, widthAction, height);
  with toolParam1Choices do setBounds(leftParam1, topParam1, widthParam1, height);
  with toolParam2Choices do setBounds(leftParam1 + toolParam1Choices.width, topParam1, widthParam2, height);
  topPanel.height := self.topPanelHeightAfterResize(firstLineSplit, secondLineSplit);
  end;

function TGardenForm.topPanelHeightAfterResize(firstLineSplit, secondLineSplit: boolean): longint;
  begin
  result := weekButton.height + toolChoices.height;
  if firstLineSplit then
    result := result + statusPanel.height;
  if secondLineSplit then
    result := result + toolChoices.height;
  end;

procedure TGardenForm.resizeHarvestPanel;
  begin
  harvestPanel.height := kHarvestPanelHeight;
  harvestPanel.top := self.clientHeight - harvestPanel.height;
  harvestPanel.left := 0;
  harvestPanel.width := self.clientWidth;
  harvestPanel.doResize;
  end;

procedure TGardenForm.resizeForGardenBitmapChange;
  var maxClientWidth, maxClientHeight: integer;
  begin
  GardenPaintBox.width := Domain.gardenSize.x;
  GardenPaintBox.height := Domain.gardenSize.y;
  maxClientHeight := topPanel.height + Domain.gardenSize.y + self.harvestPanel.height;
  maxClientWidth := Domain.gardenSize.x;
  self.clientHeight := maxClientHeight;
  self.clientWidth := maxClientWidth;
 { if self.clientHeight > maxClientHeight then
  	self.clientHeight := maxClientHeight;
  if self.clientWidth > maxClientWidth then
  	self.clientWidth := maxClientWidth;}
  {self.formResize(self);  }
  end;

const
  kSmallestPictureHeight = 100;

procedure TGardenForm.WMGetMinMaxInfo(var MSG: Tmessage);
  var
    topPanelHeight, maxTopPanelHeight: longint;
  begin
  inherited;
  with PMinMaxInfo(MSG.lparam)^ do
    begin
    { min width is size of buttons on top panel - we are setting width here, not client width }
    ptMinTrackSize.X := topPanel.left + yearButton.left + yearButton.width
      + (self.width - self.clientWidth);
    if not expanded then
      ptMinTrackSize.Y := collapseExpand.top + collapseExpand.height + (self.height - self.clientHeight)
    else
      begin
      { min expanded height is top panel height (after resize) + harvest panel height + arbitrary number }
      { cannot use real harvest panel height here because this function is called before the
        harvest panel is made, because it is not a design-time component and therefore is not
        created until the FormCreate method which is after this is called for the first time. }
      maxTopPanelHeight := self.topPanelHeightAfterResize(true, true);
      ptMinTrackSize.Y := maxTopPanelHeight + intMin(Domain.gardenSize.y, kSmallestPictureHeight)
        + kHarvestPanelHeight;
      end;
    { max width is garden size }
    ptMaxTrackSize.X := Domain.gardenSize.x + (self.width - self.clientWidth);
    if not expanded then
      ptMaxTrackSize.Y := collapseExpand.top + collapseExpand.height + (self.height - self.clientHeight)
    else
      begin
      { min height is height of top panel (after resize) + harvest panel height + garden size }
      { cannot get max height correct because calculating topPanelHeight using last width,
        not width as user is resizing window }
      topPanelHeight := self.topPanelHeightAfterResize(self.clientWidth < self.minTopPanelWidthFirstLine,
          self.clientWidth < self.minTopPanelWidthSecondLine);
      ptMaxTrackSize.Y := topPanelHeight + Domain.gardenSize.y + kHarvestPanelHeight
        + (self.height - self.clientHeight);
      end;
    end;
  end;

{ -------------------------------------------------------------------------------------------- save/load }
procedure TGardenForm.MenuSaveClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if pos(kUnsavedGardenName, UpperCase(ExtractFileName(Domain.fileOptions.gardenFileName))) > 0 then
    begin
    self.MenuSaveAsClick(self);
    exit;
    end;
  lastSaveProceeded := GsFile_GetFileSaveInfo(kFileTypeGarden, kDontAskForFileName,
  		Domain.fileOptions.gardenFileName, fileInfo);
  if not lastSaveProceeded then exit;
  try
    Domain.save(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  Domain.fileOptions.gardenFileName := fileInfo.newFile;
  self.updateFormCaption;
  Domain.storeProfileInformation;
	end;

procedure TGardenForm.MenuSaveAsClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
	begin
  lastSaveProceeded := GsFile_GetFileSaveInfo(kFileTypeGarden, kAskForFileName, Domain.fileOptions.gardenFileName, fileInfo);
  if not lastSaveProceeded then exit;
  try
    Domain.save(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  Domain.fileOptions.gardenFileName := fileInfo.newFile;
  self.updateFormCaption;
  Domain.storeProfileInformation;
	end;

procedure TGardenForm.MenuNewClick(Sender: TObject);
  var
    gardenFileName: string;
    initialized: boolean;
	begin
	if not self.askForSaveAndProceed then exit;
  {should prompt to save existing garden...}
  {could also make undoable command?}
  self.clearCommandList;
  { tell harvest panel not to redraw until it has the new harvest manager pointer }
  harvestPanel.hidden := true;
  { tell browser harvest panel not to redraw until it has the new harvest manager pointer }
  BrowserForm.plantharvestPanel.hidden := true;

  {try to load default garden}
  initialized := false;
  gardenFileName := 'default.gdn';
  gardenFileName := Domain.buildFileNameInPath(gardenFileName);
  if Domain.useIniFile then
    begin
  	gardenFileName := Domain.loadPreference('default', gardenFileName);
  	gardenFileName := Domain.buildFileNameInPath(gardenFileName);
    end;
  if Domain.isFileInPath(gardenFileName) then
  	try
    showLoadingString('Loading default garden...');
  	Domain.load(gardenFileName);
  	initialized := true;
  	except
      on E: Exception do
          begin
          ShowMessage(E.message);
      		ShowMessage('Could not load default garden ' + gardenFileName);
          end;
  	end
  else
    ShowMessage('Could not find the default garden ' + gardenFileName);
  if initialized then
    begin
    {similar to open}
  	self.commandList.setNewUndoLimit(Domain.menuOptions.undoLimit);
  	Domain.fileOptions.gardenFileName := Domain.firstUnusedUnsavedGardenName;
  	currentTool := Domain.toolManager.gloveTool;
		self.populateTools;
 	 	self.updateToolChoicesForCurrentTool;
  	Domain.constrainToolsToGarden;
  	self.updateForNewDomain;
    harvestPanel.hidden := false; { safe to redraw now because has new pointer }
    BrowserForm.plantHarvestPanel.hidden := false; { safe to redraw now because has new pointer }
  	self.resizeForGardenBitmapChange;
    exit;
    end;

  {if failed to load default...}
  Domain.fileOptions.gardenFileName := Domain.firstUnusedUnsavedGardenName;
  Domain.garden.clearGarden;
  Domain.garden.date := GsDate_currentDate;
  Domain.garden.date := GsDate_dateFromYearMonthDayOfMonth(GsDate_yearFromDate(Domain.garden.date), 0, 0);
  Domain.harvestManager.harvestItemList.clear;
  self.updateHarvestPanel := true;
  Domain.harvestManager.lastReportResetDate := Domain.garden.date;
  self.updateForNewDomain;
  harvestPanel.hidden := false; { safe to redraw now because has new pointer }
  BrowserForm.plantHarvestPanel.hidden := false; { safe to redraw now because has new pointer }
	end;

{may need to make sure that tool isn't operating on object about to be replaced - like soil patch dragging}
procedure TGardenForm.MenuOpenClick(Sender: TObject);
	var
    fileNameWithPath: string;
	begin
	if not self.askForSaveAndProceed then exit;
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeGarden, Domain.fileOptions.gardenFileName);
  if fileNameWithPath = '' then exit;
  self.clearCommandList;
  { tell harvest panel not to redraw until it has the new harvest manager pointer }
  harvestPanel.hidden := true;
  { tell browser harvest panel not to redraw until it has the new harvest manager pointer }
  BrowserForm.plantharvestPanel.hidden := true;
  try
  	Domain.load(fileNameWithPath);
  except
    on E: Exception do
      begin
  		ShowMessage(E.message);
  		ShowMessage('Could not load file ' + fileNameWithPath);
    	exit;
      end;
	end;
  self.commandList.setNewUndoLimit(Domain.menuOptions.undoLimit);
  Domain.fileOptions.gardenFileName := fileNameWithPath;
  currentTool := Domain.toolManager.gloveTool;
	self.populateTools;
  self.updateToolChoicesForCurrentTool;
  Domain.constrainToolsToGarden;
  self.updateForNewDomain;
  harvestPanel.hidden := false; { safe to redraw now because has new pointer }
  BrowserForm.plantHarvestPanel.hidden := false; { safe to redraw now because has new pointer }
  self.resizeForGardenBitmapChange;
  Domain.storeMostRecentFileName(Domain.fileOptions.gardenFileName);
  iniFileMightHaveChanged := true;
	end;

{ -------------------------------------------------------------------------------------------- menu commands }
procedure TGardenForm.MenuChooseBackdropClick(Sender: TObject);
  var
  	backdropChooserForm: TBackdropChooserForm;
    redrawNeeded: boolean;
    theResult: integer;
  begin
  backdropChooserForm := TBackdropChooserForm.create(self);
  if backdropChooserForm = nil then
    raise Exception.create('Could not create backdrop chooser window');
  try
    theResult := backdropChooserForm.showModal;
    if backdropChooserForm.resizedBitmap then
      begin
      Domain.constrainToolsToGarden;
  		GardenForm.resizeForGardenBitmapChange;
      end;
    redrawNeeded := backdropChooserForm.redrawNeeded;
  finally
    backdropChooserForm.free;
    backdropChooserForm := nil;
  end;
  if redrawNeeded then
    begin
  	self.redrawGarden;
  	GardenImageScrollBox.invalidate; {need to force a cleanup?}
  	{may also want to constrain tools to new garden}
    end;
  end;

procedure TGardenForm.MenuSimulationClick(Sender: TObject);
  var simulationOptionsForm: TSimulationOptionsForm;
  begin
  if resourcesAreLowInWin31(kUserResources, 'simulation options window') then exit;
  simulationOptionsForm := TSimulationOptionsForm.create(self);
  if simulationOptionsForm = nil then
    raise Exception.create('Could not create simulation options window');
  try
    simulationOptionsForm.showModal;
  finally
    simulationOptionsForm.free;
    simulationOptionsForm := nil;
  end;
  end;

procedure TGardenForm.MenuTemplatesEditorClick(Sender: TObject);
  var
  	templatesForm: TTemplatesForm;
    theResult: integer;
    oldFileName: string;
  begin
  oldFileName := Domain.fileOptions.templatesFileName;
  templatesForm := TTemplatesForm.create(self);
  if templatesForm = nil then
    raise Exception.create('Could not create templates window');
  try
    theResult := templatesForm.showModal;
    if theResult = mrOK then
      begin
      self.updateToolParamChoicesForCurrentToolAction;
      BrowserForm.updateObjectChoices;
      end
    else
      Domain.fileOptions.templatesFileName := oldFileName;
  finally
    templatesForm.free;
    templatesForm := nil;
  end;
  end;

procedure TGardenForm.updateMenu;
	begin
  {handle undo / redo}
  if CommandList.isUndoEnabled then
  	begin
    MenuUndo.enabled := true;
    MenuUndo.caption := 'Undo ' + CommandList.undoDescription;
    end
  else
  	begin
    MenuUndo.enabled := false;
    MenuUndo.caption := 'Undo';
    end;
  if CommandList.isRedoEnabled then
  	begin
    MenuRedo.enabled := true;
    MenuRedo.caption := 'Redo ' + CommandList.redoDescription;
    end
  else
  	begin
    MenuRedo.enabled := false;
    MenuRedo.caption := 'Redo';
    end;
  end;

procedure TGardenForm.MenuUndoClick(Sender: TObject);
	begin
	CommandList.undoLast;
	self.handlePossibleGardenChange;
	end;

procedure TGardenForm.MenuRedoClick(Sender: TObject);
	begin
	CommandList.redoLast;
	self.handlePossibleGardenChange;
	end;

procedure TGardenForm.Quit1Click(Sender: TObject);
	begin
	Halt;
	end;

procedure TGardenForm.MenuStopClick(Sender: TObject);
	begin
	self.running := false;
  self.doUpdating;
	end;

procedure TGardenForm.MenuGoClick(Sender: TObject);
	begin
  if self.running then
  	begin self.terminateRun;
    exit;
    end;
	self.running := true;
	self.lastDraw := Domain.garden.date;
  self.runUntilUserStop := true;
  self.startRunning;
	end;

procedure TGardenForm.terminateRun;
  begin
  self.running := false;
  self.doUpdating;
  end;

procedure TGardenForm.MenuGoForDayClick(Sender: TObject);
	begin
  if self.running then
  	begin
    self.terminateRun;
    exit;
    end;
	self.running := true;
	self.lastDraw := Domain.garden.date;
	self.runUntil := Domain.garden.date;
	GsDate_addDays(self.runUntil, 1);
  self.runUntilUserStop := false;
  self.startRunning;
	end;

procedure TGardenForm.MenuGoForWeekClick(Sender: TObject);
	begin
  if self.running then
  	begin
    self.terminateRun;
    exit;
    end;
	self.running := true;
	self.lastDraw := Domain.garden.date;
	self.runUntil := Domain.garden.date;
  GsDate_addDays(self.runUntil, 7);
  self.runUntilUserStop := false;
  self.startRunning;
	end;

procedure TGardenForm.MenuGoForMonthClick(Sender: TObject);
	begin
  if self.running then
  	begin
    self.terminateRun;
    exit;
    end;
	self.running := true;
	self.lastDraw := Domain.garden.date;
	self.runUntil := Domain.garden.date;
	GsDate_addMonths(self.runUntil, 1);
  self.runUntilUserStop := false;
  self.startRunning;
	end;

procedure TGardenForm.MenuGoForYearClick(Sender: TObject);
	begin
  if self.running then
  	begin
    self.terminateRun;
    exit;
    end;
	self.running := true;
	self.lastDraw := Domain.garden.date;
	self.runUntil := Domain.garden.date;
  GsDate_addYears(self.runUntil, 1);
  self.runUntilUserStop := false;
  self.startRunning;
	end;

procedure TGardenForm.MenuGoForMonthsClick(Sender: TObject);
  var
  	monthsToRun: longint;
{$IFDEF WINDOWS}
    monthsToRunString: string;
{$ELSE}
    monthsToRunString: ansistring;
{$ENDIF}
	begin
  if self.running then
  	begin
    self.terminateRun;
    exit;
    end;
  {this runs for a year - should prompt user for date}
  monthsToRunString := '4';
  if not InputQuery('Run for months', 'Enter number of months to run.', monthsToRunString) then
  	exit;
  try
    monthsToRun := StrToInt(monthsToRunString);
    if monthsToRun <= 0 then
      begin
    	ShowMessage('You should enter a number of months to run greater than zero.');
      exit;
      end;
    if monthsToRun > 32000 then
      begin
    	ShowMessage('You should enter a number of months to run less than or equal to 32000.');
      exit;
      end;
  except
    ShowMessage('You should enter a number of months to run from 1 to 32000.');
    exit;
  end;
	self.running := true;
	self.lastDraw := Domain.garden.date;
	self.runUntil := Domain.garden.date;
  GsDate_addMonths(self.runUntil, monthsToRun);
	{self.runUntil := ?????  }
  self.runUntilUserStop := false;
  self.startRunning;
	end;

procedure TGardenForm.MenuReseedingOptionsClick(Sender: TObject);
  var
  	reseedingOptionsForm: TReseedingOptionsForm;
    theResult: integer;
    reseedNow: boolean;
    startGrowingNow: boolean;
  begin
  reseedNow := false;
  startGrowingNow := false;
  reseedingOptionsForm := TReseedingOptionsForm.create(self);
  if reseedingOptionsForm = nil then
    raise Exception.create('Could not create reseeding options window');
  try
    theResult := reseedingOptionsForm.showModal;
    reseedNow := reseedingOptionsForm.reseedNow.checked;
    startGrowingNow := not reseedingOptionsForm.keepPlantsDormantUntilNextReseeding.checked;
  finally
    reseedingOptionsForm.free;
    reseedingOptionsForm := nil;
  end;
  if theResult <> mrOK then exit;
  if reseedNow then
    begin
    if Domain.garden.reseedAllPlantsNow(startGrowingNow) then
      { returns whether any plants were reseeded }
      begin
  	  self.redrawGarden;
      { have to tell browser to update list of objects because plants awaiting reseeding were not shown }
      BrowserForm.updateObjectChoices;
      BrowserForm.updateModelValues;
      end;
    end;
  end;

 procedure TGardenForm.MenuDisplayOptionsClick(Sender: TObject);
  var
  	displayOptionsForm: TDisplayOptionsForm;
    dialogResult: integer;
    redrawNeeded, redrawBrowserPlantNeeded: boolean;
  begin
  displayOptionsForm := TDisplayOptionsForm.create(self);
  if displayOptionsForm = nil then
    raise Exception.create('Could not create display options window');
  try
    dialogResult := displayOptionsForm.showModal;
    redrawNeeded := displayOptionsForm.redrawNeeded;
    redrawBrowserPlantNeeded := displayOptionsForm.redrawBrowserPlantNeeded;
  finally
    displayOptionsForm.free;
    displayOptionsForm := nil;
  end;
  if (dialogResult = mrOK) then
    begin
    if redrawNeeded then self.redrawGarden;
    if redrawBrowserPlantNeeded then BrowserForm.plantDrawClick(BrowserForm);
    end;
  end;

procedure TGardenForm.MenuDrawSeedsAsSymbolsClick(Sender: TObject);
	begin
  Domain.menuOptions.drawSeedsAsSymbols := not Domain.menuOptions.drawSeedsAsSymbols;
  self.MenuDrawSeedsAsSymbols.checked := Domain.menuOptions.drawSeedsAsSymbols;
  self.redrawGarden;
	end;

procedure TGardenForm.MenuDrawPlantsAsSymbolsClick(Sender: TObject);
	begin
  Domain.menuOptions.drawPlantsAsSymbols := not Domain.menuOptions.drawPlantsAsSymbols;
  self.MenuDrawPlantsAsSymbols.checked := Domain.menuOptions.drawPlantsAsSymbols;
  self.MenuDrawSymbolsOverPlants.enabled := not Domain.menuOptions.drawPlantsAsSymbols;
  self.redrawGarden;
	end;

procedure TGardenForm.MenuDrawSymbolsOverPlantsClick(Sender: TObject);
  begin
  Domain.menuOptions.drawSymbolsOverPlants := not Domain.menuOptions.drawSymbolsOverPlants;
  self.MenuDrawSymbolsOverPlants.checked := Domain.menuOptions.drawSymbolsOverPlants;
  self.redrawGarden;
  end;

procedure TGardenForm.MenuExitClick(Sender: TObject);
	begin
	if self.askForSaveAtExitAndProceed then
    begin
    Application.helpCommand(HELP_QUIT, 0);
  	Application.terminate;
    end;
	end;

procedure TGardenForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  Application.helpCommand(HELP_QUIT, 0);
  end;

procedure TGardenForm.MenuPlayMusicClick(Sender: TObject);
  var
    fileNameWithPath: string;
	begin
  fileNameWithPath := '';
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeMidiFile, '');
  if fileNameWithPath <> '' then
    if musicForm <> nil then
      musicForm.playMusic(fileNameWithPath);
	end;

procedure TGardenForm.MenuStopMusicClick(Sender: TObject);
  begin
  if musicForm <> nil then
    musicForm.stopMusic;
  end;

procedure TGardenForm.collapseExpandClick(Sender: TObject);
begin
expanded := not expanded;
if expanded then
  begin
  collapseExpand.glyph := collapseGlyph.picture.bitmap;
  collapseExpand.hint := 'Collapse window';
  cursor_startWait;
  self.paintGardenImage; {repaint if has been invalidated}
  cursor_stopWait;
  {self.formStyle := fsNormal;}
  collapsedSize.x := self.width;
  collapsedSize.y := self.height;
  self.width := expandedSize.x;
  self.height := expandedSize.y;
  GardenImageScrollBox.visible := true;
  self.updateToolChoicesForCurrentTool; {compensates for some sort of bug w/ comboboxes maintaning selection}
  end
else
  begin
  collapseExpand.glyph := expandGlyph.picture.bitmap;   
  collapseExpand.hint := 'Expand window';
  expandedSize.x := self.width;
  expandedSize.y := self.height;
  self.width := collapsedSize.x;
  {self.height := collapsedSize.y; ignored}
  GardenImageScrollBox.visible := false;
  self.clientHeight := collapseExpand.top + collapseExpand.height;
  {if Domain.menuOptions.keepCollapsedGardenWindowOnTop then
  	self.formStyle := fsStayOnTop;}
  end;
end;

procedure TGardenForm.MenuBrowserClick(Sender: TObject);
	begin
  if BrowserForm = nil then
    raise Exception.create('Browser not found');
	BrowserForm.Show;
  if BrowserForm.windowState = wsMinimized then
    BrowserForm.windowState := wsNormal;
	end;

procedure TGardenForm.MenuGraphClick(Sender: TObject);
	begin
  if GraphForm = nil then
    raise Exception.create('Graph window not found');
  GraphForm.show;
  if GraphForm.windowState = wsMinimized then
    GraphForm.windowState := wsNormal;
	end;

procedure TGardenForm.MenuHarvestListClick(Sender: TObject);
  begin
  if HarvestForm = nil then
    raise Exception.create('Harvest window not found');
  HarvestForm.show;
  if HarvestForm.windowState = wsMinimized then
    HarvestForm.windowState := wsNormal;
  end;

procedure TGardenForm.MenuDailyRedrawClick(Sender: TObject);
	begin
	redrawFrequency := kRedrawDaily;
	self.updateRedrawFrequencyMenu;
	end;

procedure TGardenForm.MenuWeeklyRedrawClick(Sender: TObject);
	begin
	redrawFrequency := kRedrawWeekly;
	self.updateRedrawFrequencyMenu;
	end;

procedure TGardenForm.MenuMonthlyRedrawClick(Sender: TObject);
	begin
	redrawFrequency := kRedrawMonthly;
	self.updateRedrawFrequencyMenu;
	end;

procedure TGardenForm.MenuAtStopRedrawClick(Sender: TObject);
	begin
	redrawFrequency := kRedrawAtStop;
	self.updateRedrawFrequencyMenu;
	end;

procedure TGardenForm.updateRedrawFrequencyMenu;
	begin
  if redrawFrequency = kRedrawDaily then
  	MenuDailyRedraw.checked := true
  else
  	MenuDailyRedraw.checked := false;

  if redrawFrequency = kRedrawWeekly then
  	MenuWeeklyRedraw.checked := true
  else
  	MenuWeeklyRedraw.checked := false;

  if redrawFrequency = kRedrawMonthly then
  	MenuMonthlyRedraw.checked := true
  else
  	MenuMonthlyRedraw.checked := false;

  if redrawFrequency = kRedrawAtStop then
  	MenuAtStopRedraw.checked := true
  else
  	MenuAtStopRedraw.checked := false;
  end;

{run menu}
{need to improve this to handle a list of graphs}
{may want to change when data is sampled to before next day}
function TGardenForm.simulateAnotherDay: boolean;
  var
    browserUpdateNeeded: boolean;
	begin
  {$IFDEF DEBUGREPORT } errorMessage('starting simulateAnotherDay'); {$ENDIF }
  result := false;
  browserUpdateNeeded := false;
  if (not self.running) then exit;
  statusUpdateLabel('simulating...');
  {$IFDEF DEBUGREPORT } errorMessage('about to do garden NextDay'); {$ENDIF }
	browserUpdateNeeded := Domain.garden.nextDay;
  {$IFDEF DEBUGREPORT } errorMessage('about to do harvest manager nextDay'); {$ENDIF }
  Domain.harvestManager.nextDay(Domain.garden.date);
  gardenShouldBeInvalidatedOnUpdate := true;
  statusClear;
  GraphForm.updateLog;
  self.updateDeletedLoggedVars;
  if ((not self.runUntilUserStop) and (GsDate_daysBetween(Domain.garden.date, self.runUntil) = 0))
      or Domain.stopSimulationAfterTodayBecauseOfException then
  	begin
    self.running := false;
    self.doUpdating;
    end
  else if Domain.stopSimulationAfterTodayBecauseOfException then
    begin
    self.running := false;
    self.doUpdating;
    end
  else
    case redrawFrequency of
  	kRedrawDaily:
    	begin
      self.doUpdating;
      self.lastDraw := Domain.garden.date;
      end;
  	kRedrawWeekly:
      if GsDate_daysBetween(self.lastDraw, Domain.garden.date) >= 7 then
    		begin
        self.doUpdating;
      	self.lastDraw := Domain.garden.date;
      	end;
  	kRedrawMonthly:
      if GsDate_daysBetween(self.lastDraw, Domain.garden.date) >=
      		GsDate_daysInMonthFromDate(self.lastDraw) then
    		begin
        self.doUpdating;
      	self.lastDraw := Domain.garden.date;
      	end;                      
  	end;
  if browserUpdateNeeded then
    begin
    BrowserForm.updateObjectChoices;
    BrowserForm.updateModelValues;
    end;
  result := true;
  end;

procedure TGardenForm.updateDeletedLoggedVars;
  var
    i: longint;
    command: KfCommand;
  begin
  if commandList = nil then exit;
  if commandList.commandList = nil then exit;
  if commandList.commandList.count > 0 then
    for i := 0 to commandList.commandList.count - 1 do
      begin
      command := KfCommand(commandList.commandList.items[i]);
      if command is GsRemoveGraphLoggedVarCommand then
        if (command.done) and ((command as GsRemoveGraphLoggedVarCommand).loggedVar <> nil) then
          (command as GsRemoveGraphLoggedVarCommand).loggedVar.addToLog;
      end;
  end;

procedure TGardenForm.doUpdating;
  var gardenChanged: boolean;
  begin
  Domain.stopSimulationAfterTodayBecauseOfException := false;
  self.respondToSimulationRunningStatus(self.running);
  {ensure updating flag reset if exception by storing it in temp and resetting right away}
  gardenChanged := gardenShouldBeInvalidatedOnUpdate;
  gardenShouldBeInvalidatedOnUpdate := false;
	if gardenChanged then
  	self.invalidateEntireGarden;
  if self.expanded then
  	self.handlePossibleGardenChange;
	if gardenChanged then
    begin
  	GraphForm.updateGraphWithTracking;
  	BrowserForm.updateModelValues;
    end;
  end;

procedure TGardenForm.updateDateButtons;
  begin
	MonthButton.caption := GsDate_monthShortString(Domain.garden.date);
  WeekButton.caption := GsDate_dayOfWeekShortString(Domain.garden.date);
  DayButton.caption := GsDate_dayOfMonthString(Domain.garden.date);
	YearButton.caption := GsDate_yearString(Domain.garden.date);
  end;

procedure TGardenForm.TimerTimer(Sender: TObject);
	begin
	timer.enabled := false;
	self.simulateAnotherDay;
  self.updateDateButtons;
	timer.enabled := self.running;
	end;

procedure TGardenForm.startRunning;
	begin
  self.clearCommandList;
  self.respondToSimulationRunningStatus(self.running);
  { set cursor to arrow for running - does not change while running }
  gardenPaintBox.cursor := crArrowGarden;
  { now start the timer }
  timer.enabled := true;
  end;

procedure TGardenForm.respondToSimulationRunningStatus(isRunning: boolean);
  begin
  { toolbar }
  ToolChoices.enabled := not isRunning;
  toolActionChoices.enabled := not isRunning;
  toolParam1Choices.enabled := not isRunning;
  toolParam2Choices.enabled := not isRunning;
  { file menu }
  MenuNew.enabled := not isRunning;
  MenuOpen.enabled := not isRunning;
  MenuSaveAs.enabled := not isRunning;
  MenuSave.enabled := not isRunning;
  MenuPrintForm.enabled := not isRunning;
  MenuPlayMusic.enabled := not isRunning;
  { edit menu }
  MenuCut.enabled := not isRunning;
  MenuCopy.enabled := not isRunning;
  MenuPaste.enabled := not isRunning;
  MenuCopyForm.enabled := not isRunning;
  MenuGroupEditor.enabled := not isRunning;
  MenuHarvestItemEditor.enabled := not isRunning;
  MenuTemplatesEditor.enabled := not isRunning;
  MenuToolEditor.enabled := not isRunning;
  MenuToolParameterListEditor.enabled := not isRunning;
  MenuHarvestReportEditor.enabled := not isRunning;
  { run menu }
  MenuGo.enabled := not isRunning;
  MenuStop.enabled := isRunning;
  MenuGoForDay.enabled := not isRunning;
  MenuGoForWeek.enabled := not isRunning;
  MenuGoForMonth.enabled := not isRunning;
  MenuGoForYear.enabled := not isRunning;
  MenuGoForMonths.enabled := not isRunning;
  { options menu }
  MenuChooseBackdrop.enabled := not isRunning;
  MenuDisplayOptions.enabled := not isRunning;
  MenuSimulation.enabled := not isRunning;
  MenuReseedingOptions.enabled := not isRunning;
  MenuMetricUnits.enabled := not isRunning;
  MenuEnglishUnits.enabled := not isRunning;
  { tell all non-modal windows }
  BrowserForm.respondToSimulationRunningStatus(self.running);
  GraphForm.respondToSimulationRunningStatus(self.running);
  HarvestForm.respondToSimulationRunningStatus(self.running);
  DebugForm.respondToSimulationRunningStatus(self.running);
  end;

procedure TGardenForm.MenuShowToolsClick(Sender: TObject);
  begin
  Domain.menuOptions.showTools := not Domain.menuOptions.showTools;
  self.menuShowTools.checked := Domain.menuOptions.showTools;
  self.invalidateEntireToolRect;
	self.handlePossibleGardenChange;
  end;

procedure TGardenForm.MenuShowBackdropClick(Sender: TObject);
  begin
  Domain.menuOptions.showBackdrop := not Domain.menuOptions.showBackdrop;
  self.menuShowBackdrop.checked := Domain.menuOptions.showBackdrop;
  self.redrawGarden;
  end;

procedure TGardenForm.MenuPlayToolSoundsClick(Sender: TObject);
  begin
  Domain.menuOptions.playToolSounds := not Domain.menuOptions.playToolSounds;
  self.menuPlayToolSounds.checked := Domain.menuOptions.playToolSounds;
  end;

procedure TGardenForm.updateOptionsMenu;
  begin
  self.menuDrawSeedsAsSymbols.checked := Domain.menuOptions.drawSeedsAsSymbols;
  self.menuDrawPlantsAsSymbols.checked := Domain.menuOptions.drawPlantsAsSymbols;
  self.menuDrawSymbolsOverPlants.checked := Domain.menuOptions.drawSymbolsOverPlants;
  self.menuDrawSymbolsOverPlants.enabled := not Domain.menuOptions.drawPlantsAsSymbols;
  self.menuShowTools.checked := Domain.menuOptions.showTools;
  self.menuShowBackdrop.checked := Domain.menuOptions.showBackdrop;
  self.menuPlayToolSounds.checked := Domain.menuOptions.playToolSounds;
  self.menuShowLongHints.checked := Domain.menuOptions.showLongHints;
  self.menuShowHintsForAspects.checked := Domain.menuOptions.showAspectHints;
  self.menuShowtoolhowtohints.checked := Domain.menuOptions.showToolHowToHints;
  end;

procedure TGardenForm.MenuAboutGSClick(Sender: TObject);
  begin
  AboutBoxForm.showModal;
  end;

{ -------------------------------------------------------------------------------------------- commands / model change }
procedure TGardenForm.clearCommandList;
	begin
  {assumes no commands in progress - if so - problem}
  {clear out undo list}
  commandList.free;
  commandList := nil;
  commandList := KfCommandList.create;
  self.updateMenu;
  end;

procedure TGardenForm.doCommand(command: KfCommand);
	begin
  CommandList.doCommand(command);
	self.handlePossibleGardenChange;
  end;

procedure TGardenForm.undoLast;
	begin
  CommandList.undoLast;
	self.handlePossibleGardenChange;
  end;

function TGardenForm.makeMouseCommand(var point: TPoint): KfCommand;
	begin
  result := currentTool.createNewCommand(point);
  end;

procedure TGardenForm.modelDeleted(model: GsModel);
	var i: longint;
	begin
  {need to change if more than one browser to, tell all}
  BrowserForm.modelDeleted(model);
  GraphForm.modelDeleted(model);
  if model is GsSoilPatch then
  	begin
    if (model as GsSoilPatch).plantList.count > 0 then
      for i := 0 to (model as GsSoilPatch).plantList.count - 1 do
      	self.modelDeleted((model as GsSoilPatch).plantList.items[i]);
    end;
  self.modelAddedOrRemoved;
  end;

procedure TGardenForm.modelUndeleted(model: GsModel);
	var i: longint;
	begin
  {need to change if more than one browser to, tell all}
  BrowserForm.modelUndeleted(model);
  GraphForm.modelUndeleted(model);
  if model is GsSoilPatch then
  	begin
    if (model as GsSoilPatch).plantList.count > 0 then
      for i := 0 to (model as GsSoilPatch).plantList.count - 1 do
      	self.modelUndeleted((model as GsSoilPatch).plantList.items[i]);
    end;
  self.modelAddedOrRemoved;
  end;

procedure TGardenForm.handleUpdateHarvestPanel;
  begin
  harvestPanel.useListAsHarvestList(Domain.harvestManager.harvestItemList);
  harvestPanel.enableOrDisableButtonsIfNeeded;
  harvestPanel.invalidate;
  updateHarvestPanel := false;
  end;

procedure TGardenForm.handlePossibleGardenChange;
	begin
  self.updateMenu;
  self.paintGardenImage;
  { tell harvest panel to use domain's harvest list to save time }
  { we do this repeatedly because user might have loaded new file since last time }
  if updateHarvestPanel then
    self.handleUpdateHarvestPanel;
  {self.gardenPaintBox.repaint;}
  {force this potentially long process to be done after repainting}
  if isModelAddedOrRemoved then
    BrowserForm.updateObjectChoices;
  isModelAddedOrRemoved := false;
	end;

procedure TGardenForm.modelAddedOrRemoved;
  begin
  isModelAddedOrRemoved := true;
  end;

{ -------------------------------------------------------------------------------------------- tool related functions }
procedure TGardenForm.MenuToolEditorClick(Sender: TObject);
  var
    toolEditorForm: TToolEditorForm;
    response: integer;
	begin
  if resourcesAreLowInWin31(kGDIAndUserResources, 'tool editor') then exit;
  {put down current tool if not glove}
  self.setCurrentTool(Domain.toolmanager.gloveTool, true);
  toolEditorForm := TToolEditorForm.create(self);
  if toolEditorForm = nil then
    raise Exception.create('Could not create tool editor');
  try
	  response := toolEditorForm.showModal;
  finally
    toolEditorForm.free;           
    toolEditorForm := nil;
  end;
  if response = mrOK then
    begin
    self.currentTool := Domain.toolmanager.gloveTool;
		self.populateTools;
    if toolChoices.Items.indexOfObject(currentTool) < 0 then
      toolChoices.itemIndex := toolChoices.Items.IndexOfObject(Domain.toolManager.gloveTool)
    else
 			toolChoices.itemIndex := toolChoices.Items.IndexOfObject(currentTool);
    self.updateToolChoicesForCurrentTool;
  	self.invalidateEntireToolRect;
	  self.handlePossibleGardenChange;
    end;
	end;

procedure addToolToToolChoices(each: TObject; data: TObject);
  var
  tool : GsTool;
  begin
  tool := each as GsTool;
  GardenForm.toolChoices.items.addObject(tool.name, tool);
  end;

 {put the tool strings and objects in the tools combo box}
procedure TGardenForm.populateTools;
  begin
  toolChoices.clear;
  Domain.toolManager.tools.forEach(addToolToToolChoices, nil);
  end;

procedure TGardenForm.ToolChoicesChange(Sender: TObject);
  var
    newTool: GsTool;
	begin
  if toolChoices.itemIndex <> -1 then
  	begin
  	newTool := toolChoices.items.objects[toolChoices.itemIndex] as GsTool;
    if newTool <> currentTool then self.setCurrentTool(newTool, true);
    offsetPoint.x := 0;
    offsetPoint.y := 0;
    self.updateToolActionChoicesForCurrentTool;
		end;
  end;

procedure TGardenForm.updateToolChoicesForCurrentTool;
	begin
  toolChoices.itemIndex := toolChoices.Items.IndexOfObject(currentTool);
  self.updateToolActionChoicesForCurrentTool;
  end;

procedure TGardenForm.updateToolActionChoicesForCurrentTool;
	begin
  toolActionChoices.clear;
  if currentTool <> nil then
    begin
    currentTool.fillComboBoxWithActions(toolActionChoices);
    self.updateToolParamChoicesForCurrentToolAction;
    end;
  end;

procedure TGardenForm.updateToolParamChoicesForCurrentToolAction;
  begin
  if self.currentToolActionObject <> nil then
    self.currentToolActionObject.fillComboBoxesWithParams(toolParam1Choices, toolParam2Choices);
  self.updateToolDisplayForCurrentToolAction;
  end;

procedure TGardenForm.updateToolDisplayForCurrentToolAction;
  var
    theAction: GsToolAction;
  begin
  theAction := self.currentToolActionObject;
  if theAction = nil then
    begin
    toolParam1Choices.visible := false;
    toolParam2Choices.visible := false;
    end
  else
    begin
    toolParam1Choices.visible := theAction.paramCount > 0;
    toolParam2Choices.visible := theAction.paramCount > 1;
    end;
  toolChoices.droppedDown := false;
  toolActionChoices.droppedDown := false;
  toolParam1Choices.droppedDown := false;
  toolParam2Choices.droppedDown := false;
  if self.resizeTopPanel(false) then self.formResize(self);
  end;

procedure TGardenForm.toolActionChoicesChange(Sender: TObject);
	begin
  self.updateToolParamChoicesForCurrentToolAction;
  if self.currentTool = nil then exit;
  if self.currentToolActionObject <> nil then
    self.currentTool.currentActionName := self.currentToolActionObject.name
  else
    self.currentTool.currentActionName := '';
	end;

procedure TGardenForm.toolParam1ChoicesChange(Sender: TObject);
	begin
  if self.currentTool = nil then exit;
  if self.currentToolActionObject = nil then exit;
  if self.currentToolParam1Object <> nil then
    begin
    if self.currentToolParam1Object is GsToolRateOrAmountParam then
      self.currentToolActionObject.param1 :=
        (self.currentToolParam1Object as GsToolRateOrAmountParam).name(true)
    else
      self.currentToolActionObject.param1 := (self.currentToolParam1Object as GsModel).getName;
    end
  else
    self.currentToolActionObject.param1 := '';
  self.currentToolActionObject.fillSecondComboBoxWithParams(toolParam1Choices, toolParam2Choices);
	end;

procedure TGardenForm.toolParam2ChoicesChange(Sender: TObject);
	begin
  if self.currentTool = nil then exit;
  if self.currentToolActionObject = nil then exit;
  if self.currentToolParam2Object <> nil then
    self.currentToolActionObject.param2 :=
      (self.currentToolParam2Object as GsToolRateOrAmountParam).name(true)
  else
    self.currentToolActionObject.param2 := '';
	end;

{status}
procedure TGardenForm.statusUpdateLabel(newLabel: string);
  begin
  if statusPanel.visible and (statusLabel.caption = newLabel) then
  	exit;
  statusLabel.left := 2;
  statusLabel.width := statusPanel.width - 4;
  statusLabel.caption := newLabel;
  statusLabel.visible := true;
  statusLabel.repaint;
  end;

procedure TGardenForm.statusClear;
  begin
  statusLabel.visible := false;
  statusLabel.caption := '';
  end;

procedure TGardenForm.updateFormCaption;
  begin
  self.caption := 'Garden with Insight - ' + lowerCase(extractFileName(Domain.fileOptions.gardenFileName));
  end;

function TGardenForm.GetPalette: HPALETTE;
  begin
  if Application.terminated or (Domain = nil) or (not Domain.paletteBitmapLoaded) then
    result := inherited GetPalette
  else
  	result := Domain.paletteBitmap.Palette
  end;

{overriden because paint box will not update correctly}
{makes window take first priority for palettes}
function TGardenForm.PaletteChanged(Foreground: Boolean): Boolean;
	var
  	OldPalette, Palette: HPALETTE;
  	WindowHandle: HWnd;
  	DC: HDC;
	begin
  Palette := GetPalette;
  if Palette <> 0 then
  	begin
    DC := GetDeviceContext(WindowHandle);
    OldPalette := SelectPalette(DC, Palette, not Foreground);
    {if anything changed - repaint the garden}
    if (RealizePalette(DC) <> 0) and not Application.terminated then
      begin
    	gardenPaintBox.invalidate;
      end;
    SelectPalette(DC, OldPalette, True);
    RealizePalette(DC);
    ReleaseDC(WindowHandle, DC);
  	end;
  Result := inherited PaletteChanged(Foreground);
	end;

{GsPaintBoxWithPalette}
{this code did not seem to make a difference in update problem}
function GsPaintBoxWithPalette.GetPalette: HPALETTE;
  begin
  if Application.terminated or (not Domain.paletteBitmapLoaded) or (Domain = nil) then
    result := inherited GetPalette
  else
  	result := Domain.paletteBitmap.Palette
  end;

procedure TGardenForm.MenuHelpContentsClick(Sender: TObject);
  begin
  application.helpJump('IDH_CONTENTS');
  end;

procedure TGardenForm.MenuHelpSearchForClick(Sender: TObject);
  begin
  { trick help system by asking for a key that doesn't exist - makes search dialog come up. }
  application.helpCommand(HELP_PARTIALKEY, 0);
  end;

procedure TGardenForm.MenuHelpQuickStartClick(Sender: TObject);
  begin
  application.helpJump('sections_Quick_Start');
  end;

procedure TGardenForm.MenuHelpTutorialClick(Sender: TObject);
  begin
  application.helpJump('sections_Tutorial');
  end;

procedure TGardenForm.MenuToolParameterListEditorClick(Sender: TObject);
  var
    listEditorForm: TToolParamListEditorForm;
	begin
  if resourcesAreLowInWin31(kUserResources, 'tool parameter list editor') then exit;
  listEditorForm := TToolParamListEditorForm.create(self);
  if listEditorForm = nil then
    raise Exception.create('Could not create tool param list editor window');
  try
    listEditorForm.showModal;
    self.updateToolActionChoicesForCurrentTool;
  finally
    listEditorForm.free;
    listEditorForm := nil;
  end;
	end;

procedure TGardenForm.FormKeyPress(Sender: TObject; var Key: Char);
	begin
  if key = char(VK_Escape) then
    self.terminateRun;
	end;

procedure TGardenForm.MenuNumericalExceptionsClick(Sender: TObject);
  begin
  debugForm.show;
  end;

procedure TGardenForm.MenuHarvestItemEditorClick(Sender: TObject);
  var
    harvestItemTemplateEditorForm: THarvestItemTemplateEditorForm;
	begin
  harvestItemTemplateEditorForm := THarvestItemTemplateEditorForm.create(self);
  if harvestItemTemplateEditorForm = nil then
    raise Exception.create('Could not create harvest item editor window');
  try
    harvestItemTemplateEditorForm.showModal;
    BrowserForm.updateModelValues;
  finally
    harvestItemTemplateEditorForm.free;
    harvestItemTemplateEditorForm := nil;
  end;
	end;

procedure TGardenForm.MenuHelpHowtoClick(Sender: TObject);
  begin
  application.helpJump('sections_How_to');
  end;

procedure TGardenForm.MenuHelpMenusClick(Sender: TObject);
  begin
  application.helpJump('sections_Menus_reference');
  end;

procedure TGardenForm.MenuHelpWindowsClick(Sender: TObject);
  begin
  application.helpJump('sections_Windows_reference');
  end;

procedure TGardenForm.MenuHelpModelsClick(Sender: TObject);
  begin
  application.helpJump('sections_Garden_with_Insight_model_documentation');
  end;

procedure TGardenForm.MenuMetricUnitsClick(Sender: TObject);
  begin
  self.switchUnitSystem;
  end;

procedure TGardenForm.MenuEnglishUnitsClick(Sender: TObject);
  begin
  self.switchUnitSystem;
  end;

procedure TGardenForm.switchUnitSystem;
  var
    fromSystem, toSystem, prompt: string;
  begin
  { warn user }
  if Domain.menuOptions.showMetricUnits then fromSystem := 'metric' else fromSystem := 'English';
  if not Domain.menuOptions.showMetricUnits then toSystem := 'metric' else toSystem := 'English';
  prompt := 'You have chosen to switch the unit system from ' + fromSystem + ' to ' + toSystem + '. ' + chr(13)
    + 'Doing this will reset all units in the browser and graph window' + chr(13)
    + 'to defaults for the ' + toSystem + ' system. You cannot undo this action. '  + chr(13) + chr(13)
    + 'Do you want to go ahead?';
  if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;
  { change domain option and menu checkmarks }
  Domain.menuOptions.showMetricUnits := not Domain.menuOptions.showMetricUnits;
  MenuMetricUnits.checked := Domain.menuOptions.showMetricUnits;
  MenuEnglishUnits.checked := not Domain.menuOptions.showMetricUnits;
  { repercussions : }
  { group manager }
  Domain.groupManager.setAllGroupItemsToDefaultUnitsForUnitSystem(Domain.menuOptions.showMetricUnits);
  { browser }
  BrowserForm.groupChange(BrowserForm);
  { graph window }
  GraphForm.setAllCurrentUnitsToDefaultUnitsForUnitSystem(Domain.menuOptions.showMetricUnits);
  { tool parameters in garden window }
  if (Domain.toolParamListManager <> nil) and
    (not Domain.toolParamListManager.showParamsWithUnselectedUnitSystem) then
      self.updateToolParamChoicesForCurrentToolAction;
  end;

{ ------------------------------------------------------------------------------------------------- exiting }
procedure TGardenForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
	begin
  CanClose := self.askForSaveAtExitAndProceed;
	end;

function TGardenForm.askForSaveAndProceed: boolean;
  var
    messageBoxResult: integer;
  begin
  {cfk fix - put help context in}
  messageBoxResult := MessageDlg('Save current garden?', mtConfirmation, mbYesNoCancel, 0);
  case messageBoxResult of
    IDCANCEL: result := false;
    IDYES:
    	begin
      self.MenuSaveClick(Self);
      result := self.lastSaveProceeded;
      end;
    IDNO: result := true;
    else
      begin
      ShowMessage('error with save request dialog');
      result := true;
      end;
    end;
  end;

function TGardenForm.askForSaveAtExitAndProceed: boolean;
  { returns true if should exit program }
  var
    response: integer;
    saveGardenFile, saveGroupsFile, saveTemplatesFile, saveToolsFile: boolean;
    shortGardenFile, shortGroupsFile, shortTemplatesFile, shortToolsFile: string;
  begin
  result := false;
  if iniFileMightHaveChanged then
    try
      Domain.storeProfileInformation;
    except
      result := true;
    end;
  if exitForm = nil then
    raise Exception.create('Could not create exit window');
  shortGardenFile := lowerCase(extractFileName(Domain.fileOptions.gardenFileName));
  { if were using default garden, force to save as unnamed garden - can still save default.gdn in
    normal file menu, but not here }
  if shortGardenFile = 'default.gdn' then
    begin
    Domain.fileOptions.gardenFileName := Domain.firstUnusedUnsavedGardenName;
    shortGardenFile := lowerCase(extractFileName(Domain.fileOptions.gardenFileName));
    end;
  shortGroupsFile := lowerCase(extractFileName(Domain.fileOptions.groupsFileName));
  shortTemplatesFile := lowerCase(extractFileName(Domain.fileOptions.templatesFileName));
  shortToolsFile := lowerCase(extractFileName(Domain.fileOptions.toolsFileName));
  with exitForm do
    begin
    saveGardenFile.caption := 'garden file (' + shortGardenFile + ')';
    saveGardenFile.checked := true;
    saveGroupsFile.caption := 'groups file (' + shortGroupsFile + ')';
    saveGroupsFile.checked := groupsFileMightHaveChanged;
    saveTemplatesFile.caption := 'templates file (' + shortTemplatesFile + ')';
    saveTemplatesFile.checked := templatesFileMightHaveChanged;
    saveToolsFile.caption := 'tools file (' + shortToolsFile + ')';
    saveToolsFile.checked := toolsFileMightHaveChanged;
    end;
  response := exitForm.showModal;
  saveGardenFile := exitForm.saveGardenFile.checked;
  saveGroupsFile := exitForm.saveGroupsFile.checked;
  saveTemplatesFile := exitForm.saveTemplatesFile.checked;
  saveToolsFile := exitForm.saveToolsFile.checked;
  case response of
    mrCancel: result := false;
    mrYes:
    	begin
      savingFilesAtProgramExit := true;
      if saveGardenFile then
        begin
        startWaitMessage('Saving garden file (' + shortGardenFile + ')...');
        self.menuSaveClick(self);
        end;
      if saveGroupsFile then
        begin
        startWaitMessage('Saving groups file (' + shortGroupsFile + ')...');
        self.saveCurrentGroupsFile;
        end;
      if saveTemplatesFile then
        begin
        startWaitMessage('Saving templates file (' + shortTemplatesFile + ')...');
        self.saveCurrentTemplatesFile;
        end;
      if saveToolsFile then
        begin
        startWaitMessage('Saving tools file (' + shortToolsFile + ')...');
        self.saveCurrentToolsFile;
        end;
      stopWaitMessage;
      result := true;
      end;
    mrNo: result := true;
    else
      begin
      showMessage('error with exit dialog');
      result := true;
      end;
    end;
  end;

procedure TGardenForm.saveCurrentGroupsFile;
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeGroups, kDontAskForFileName, Domain.fileOptions.groupsFileName, fileInfo) then exit;
  try
  	cursor_startWait;
    GsFiler.save(fileInfo.tempFile, Domain.groupManager);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
    Domain.fileOptions.groupsFileName := lowerCase(fileInfo.newFile);
  end;
  end;

procedure TGardenForm.saveCurrentTemplatesFile;
  var
    fileInfo: SaveFileNamesStructure;
	begin
  if not GsFile_GetFileSaveInfo(kFileTypeLibrary, kDontAskForFileName, Domain.fileOptions.templatesFileName, fileInfo) then
    exit;
  try
  	cursor_startWait;
    GsFiler.save(fileInfo.tempFile, Domain.templateManager);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
    Domain.fileOptions.templatesFileName := lowerCase(fileInfo.newFile);
  end;
	end;

procedure TGardenForm.saveCurrentToolsFile;
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTools, kDontAskForFileName, Domain.fileOptions.toolsFileName, fileInfo) then exit;
  try
  	cursor_startWait;
    GsFiler.save(fileInfo.tempFile, Domain.toolManager);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
    Domain.fileOptions.toolsFileName := lowerCase(fileInfo.newFile);
  end;
  end;

procedure TGardenForm.statusPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  { can click here to stop simulation }
  if self.running then self.terminateRun;
  end;

{$R-}  {added because of FreeMem range check bug in its design - it expects a word}
{copied from TForm.print and added palette handling}
procedure Support_PrintFormWithPalette(aForm: TForm);
var
  FormImage: TBitmap;
  Info: PBitmapInfo;
  InfoSize: Integer;
  Image: Pointer;
{$IFDEF WINDOWS}
  ImageSize: Longint;
{$ELSE}
  ImageSize: DWORD;
{$ENDIF}
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
  oldPalette: HPALETTE;
begin
  if aForm = nil then exit;
  Printer.BeginDoc;
 { if (Domain <> nil) and (Domain.paletteBitmapLoaded) and (Domain.paletteBitmap <> nil) then
    begin
    oldPalette := SelectPalette(Printer.Canvas.Handle, Domain.paletteBitmap.palette, false);
    RealizePalette(Printer.Canvas.Handle);
    end;  }
  with aForm do
  try
    FormImage := GetFormImage;
    if (Domain <> nil) and (Domain.paletteBitmapLoaded) and (Domain.paletteBitmap <> nil) then
      FormImage.palette := CopyPalette(Domain.paletteBitmap.palette);
    try
      { Paint bitmap to the printer }
      with Printer, Canvas do
        begin
        Bits := FormImage.Handle;
        GetDIBSizes(Bits, InfoSize, ImageSize);
        Info := MemAlloc(InfoSize);
        try
          Image := MemAlloc(ImageSize);
          try
            GetDIB(Bits, FormImage.palette, Info^, Image^);
            with Info^.bmiHeader do
              begin
              DIBWidth := biWidth;
              DIBHeight := biHeight;
              end;
            case PrintScale of
              poProportional:
                begin
                PrintWidth := MulDiv(DIBWidth, GetDeviceCaps(Handle, LOGPIXELSX), PixelsPerInch);
                PrintHeight := MulDiv(DIBHeight, GetDeviceCaps(Handle, LOGPIXELSY), PixelsPerInch);
                end;
              poPrintToFit:
                begin
                PrintWidth := MulDiv(DIBWidth, PageHeight, DIBHeight);
                if PrintWidth < PageWidth then
                  PrintHeight := PageHeight
                else
                  begin
                  PrintWidth := PageWidth;
                  PrintHeight := MulDiv(DIBHeight, PageWidth, DIBWidth);
                  end;
                end;
              else
              PrintWidth := DIBWidth;
              PrintHeight := DIBHeight;
              end;
            StretchDIBits(Canvas.Handle, 0, 0, PrintWidth, PrintHeight, 0, 0,
                DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
          finally
          FreeMem(Image, ImageSize);
          end;
        finally
        FreeMem(Info, InfoSize);
        end;
      end;
    finally
    FormImage.Free;
    end;
  finally
{  if (Domain <> nil) and (Domain.paletteBitmapLoaded) and (Domain.paletteBitmap <> nil) then
    begin
    SelectPalette(Printer.Canvas.Handle, oldPalette, true);
    RealizePalette(Printer.Canvas.Handle);
    end;   }
  Printer.EndDoc;
  end;
end;

procedure TGardenForm.MenuPrintFormClick(Sender: TObject);
  var
    FormImage: TBitmap;
  begin
  if Screen.ActiveForm = nil then exit;
  if not printDialog.execute then exit;
  Support_PrintFormWithPalette(Screen.ActiveForm);
  (*
  FormImage := nil;
  try
    if Screen.ActiveForm = nil then exit;
    Screen.ActiveForm.printScale := poProportional;
    FormImage := Screen.ActiveForm.GetFormImage;
    if (Domain <> nil) and (Domain.paletteBitmapLoaded) and (Domain.paletteBitmap <> nil) then
      FormImage.palette := CopyPalette(Domain.paletteBitmap.palette);
    with Printer do
      begin
      BeginDoc;
      Canvas.Draw(1, 1, TGraphic(FormImage));
      EndDoc;
      end;
  finally
    FormImage.free;
    FormImage := nil;
  end; *)
  end;

procedure TGardenForm.MenuCopyFormClick(Sender: TObject);
  var
    FormImage: TBitmap;
  begin
  { this does not work perfectly; write and paintbrush don't get the palette correctly,
    but Word and Paintshop Pro do - should be fixed later }
  FormImage := nil;
  try
    if Screen.ActiveForm = nil then exit;
    FormImage := Screen.ActiveForm.GetFormImage;
    if (Domain <> nil) and (Domain.paletteBitmapLoaded) and (Domain.paletteBitmap <> nil) then
      FormImage.palette := CopyPalette(Domain.paletteBitmap.palette);
    Clipboard.Assign(FormImage);
  finally
    FormImage.free;
    FormImage := nil;
  end;
  end;

procedure TGardenForm.MenuGroupEditorClick(Sender: TObject);
  begin
  if BrowserForm <> nil then
    BrowserForm.EditGroupClick(BrowserForm);
  end;

procedure TGardenForm.MenuHarvestReportEditorClick(Sender: TObject);
  begin
  if HarvestForm <> nil then
    HarvestForm.editHarvestReportsClick(HarvestForm);
  end;

procedure TGardenForm.menuShowLongHintsClick(Sender: TObject);
  begin
  Domain.menuOptions.showLongHints := not Domain.menuOptions.showLongHints;
  menuShowLongHints.checked := Domain.menuOptions.showLongHints;
  end;

procedure TGardenForm.menuShowHintsForAspectsClick(Sender: TObject);
  begin
  Domain.menuOptions.showAspectHints := not Domain.menuOptions.showAspectHints;
  menuShowHintsForAspects.checked := Domain.menuOptions.showAspectHints;
  end;

procedure TGardenForm.menuShowtoolhowtohintsClick(Sender: TObject);
  begin
  Domain.menuOptions.showToolHowToHints := not Domain.menuOptions.showToolHowToHints;
  menuShowtoolhowtohints.checked := Domain.menuOptions.showToolHowToHints;
  end;

procedure TGardenForm.MenuTimelineClick(Sender: TObject);
  var timelineForm: TtimelineForm;
  begin
  timelineForm := TtimelineForm.create(self);
  if timelineForm = nil then
    raise Exception.create('Could not create time line window');
  try
    timelineForm.showModal;
  finally
    timelineForm.free;
    timelineForm := nil;
  end;
  end;

end.

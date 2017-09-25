unit Ubrowser;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowser: The browser is one of the few non-modal windows. It does a gazillion things.
The browser is divided into the left side (numbers side in the help system), which shows
the browser components, and the right side (pictures side) which shows any one of several
displays for weather, soil or plants. The browser file is a little messy and too large. You
should look at the help system on the browser thoroughly before reading this file.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, Menus, 
  ugrped, ugraph, umodel, uegarden, uesoil, ueplant, ueweath, ubrowcom, udsoil, udweath, ucollect, uebag, udppart,
  udplant, uharvpnl, ubrowtri, usplant, ufiler, ugsform;

const
  {which part of browser to show }         
  kBrowserShowLeft = 0;
  kBrowserShowRight = 1;
  kBrowserShowBoth = 2;
                                                                                                               
type
  TBrowserForm = class(GsForm)
    buttonsPanel: TPanel;
    ObjectChoice: TComboBox;
    rightPanel: TPanel;
    plantDrawControls: TPanel;
    soilDrawControls: TPanel;
    soilDrawDisplayMode: TComboBox;
    weatherDrawControls: TPanel;
    weatherDrawDisplayMode: TComboBox;
    plantDrawImage: TImage;
    FontDialog1: TFontDialog;
    leftPanel: TPanel;
    Group: TComboBox;
    ControlScrollBox: TScrollBox;
    ControlPanel: TPanel;
    splitter: TPanel;
    harvestIconBackup: TImage;
    showLeft: TSpeedButton;
    showBoth: TSpeedButton;
    showRight: TSpeedButton;
    graph: TSpeedButton;
    templates: TSpeedButton;
    notesButton: TSpeedButton;
    chooseFont: TSpeedButton;
    helpButton: TSpeedButton;
    plantCenter: TSpeedButton;
    plantLeft: TSpeedButton;
    plantRight: TSpeedButton;
    plantDrawFromSide: TSpeedButton;
    plantDrawFromTop: TSpeedButton;
    plantZoomPlus: TSpeedButton;
    plantZoomMinus: TSpeedButton;
    plantDragMode: TSpeedButton;
    plantHarvestMode: TSpeedButton;
    becomeSeed: TSpeedButton;
    editGroup: TSpeedButton;
    expandAll: TSpeedButton;
    collapseAll: TSpeedButton;
    drawSoilLines: TSpeedButton;
    rescaleSoilWhenLow: TSpeedButton;
    drawWeatherMeans: TSpeedButton;
    rescaleWeatherWhenLow: TSpeedButton;
    graphSoilLines: TSpeedButton;
    graphSoilBars: TSpeedButton;
    graphWeatherLines: TSpeedButton;
    graphWeatherBars: TSpeedButton;
    deleteObjectButton: TSpeedButton;
    BrowserMenu: TMainMenu;
    ShowMenu: TMenuItem;
    MenuLeftSide: TMenuItem;
    MenuRightSide: TMenuItem;
    MenuBothSides: TMenuItem;
    AspectMenu: TMenuItem;
    MenuCollapseAll: TMenuItem;
    MenuExpandAll: TMenuItem;
    N1: TMenuItem;
    MenuGraphAspect: TMenuItem;
    MenuChangeFont: TMenuItem;
    N7: TMenuItem;
    ObjectMenu: TMenuItem;
    MenuObjectMakeTemplate: TMenuItem;
    N8: TMenuItem;
    MenuEditGroups: TMenuItem;
    MenuObjectDelete: TMenuItem;
    MenuObjectNotes: TMenuItem;
    MenuWeatherStartSeparator: TMenuItem;
    MenuWeatherLineGraph: TMenuItem;
    MenuWeatherBarGraph: TMenuItem;
    MenuWeatherSeparator1: TMenuItem;
    MenuWeatherShowMeans: TMenuItem;
    MenuWeatherSeparator2: TMenuItem;
    MenuWeatherRescale: TMenuItem;
    MenuSoilShowLayers: TMenuItem;
    MenuSoilConnectLayerValues: TMenuItem;
    MenuSoilSeparator: TMenuItem;
    MenuSoilRescale: TMenuItem;
    MenuPlantTurnLeft: TMenuItem;
    MenuPlantTurnRight: TMenuItem;
    MenuPlantZoomIn: TMenuItem;
    MenuPlantZoomOut: TMenuItem;
    MenuPlantCenter: TMenuItem;
    MenuPlantSeparator1: TMenuItem;
    MenuPlantViewFromTop: TMenuItem;
    MenuPlantViewFromSide: TMenuItem;
    MenuPlantSeparator2: TMenuItem;
    MenuPlantDragMode: TMenuItem;
    MenuPlantHarvestMode: TMenuItem;
    MenuPlantSeparator3: TMenuItem;
    MenuPlantReseed: TMenuItem;
    MenuSoilStartSeparator: TMenuItem;
    MenuPlantStartSeparator: TMenuItem;
    growPlant: TSpeedButton;
    MenuPlantGrow: TMenuItem;
    plantDisplayMode: TComboBox;
    MenuShowHelp: TMenuItem;
    procedure EditGroupClick(Sender: TObject);
    procedure GraphClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure expandAllClick(Sender: TObject);
    procedure collapseAllClick(Sender: TObject);
    procedure chooseFontClick(Sender: TObject);
    procedure ObjectChoiceChange(Sender: TObject);
    procedure plantCenterClick(Sender: TObject);
    procedure plantLeftClick(Sender: TObject);
    procedure plantRightClick(Sender: TObject);
    procedure plantZoomMinusClick(Sender: TObject);
    procedure PlantZoomPlusClick(Sender: TObject);
    procedure plantDrawImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure plantDrawImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure becomeSeedClick(Sender: TObject);
    procedure reportClick(Sender: TObject);
    procedure soilDrawDisplayModeChange(Sender: TObject);
    procedure drawSoilLinesClick(Sender: TObject);
    procedure showLeftClick(Sender: TObject);
    procedure showRightClick(Sender: TObject);
    procedure notesButtonClick(Sender: TObject);
    procedure showBothClick(Sender: TObject);
    procedure plantDrawControlsResize(Sender: TObject);
    procedure soilDrawControlsResize(Sender: TObject);
    procedure weatherDrawControlsResize(Sender: TObject);
    procedure templatesClick(Sender: TObject);
    procedure rescaleSoilWhenLowClick(Sender: TObject);
    procedure weatherDrawDisplayModeChange(Sender: TObject);
    procedure drawWeatherMeansClick(Sender: TObject);
    procedure rescaleWeatherWhenLowClick(Sender: TObject);
    procedure splitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure splitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure splitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure plantHarvestModeClick(Sender: TObject);
    procedure plantDrawImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure plantDragModeClick(Sender: TObject);
    procedure plantDrawFromSideClick(Sender: TObject);
    procedure plantDrawFromTopClick(Sender: TObject);
    procedure graphSoilLinesClick(Sender: TObject);
    procedure graphSoilBarsClick(Sender: TObject);
    procedure graphWeatherLinesClick(Sender: TObject);
    procedure graphWeatherBarsClick(Sender: TObject);
    procedure deleteObjectButtonClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure plantDisplayModeChange(Sender: TObject);
    procedure ControlPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ControlPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ControlPanelDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ControlPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure MenuLeftSideClick(Sender: TObject);
    procedure MenuRightSideClick(Sender: TObject);
    procedure MenuBothSidesClick(Sender: TObject);
    procedure MenuChangeFontClick(Sender: TObject);
    procedure MenuObjectNotesClick(Sender: TObject);
    procedure MenuObjectMakeTemplateClick(Sender: TObject);
    procedure MenuObjectDeleteClick(Sender: TObject);
    procedure MenuGraphAspectClick(Sender: TObject);
    procedure MenuExpandAllClick(Sender: TObject);
    procedure MenuCollapseAllClick(Sender: TObject);
    procedure MenuEditGroupsClick(Sender: TObject);
    procedure MenuWeatherLineGraphClick(Sender: TObject);
    procedure MenuWeatherBarGraphClick(Sender: TObject);
    procedure MenuWeatherShowMeansClick(Sender: TObject);
    procedure MenuWeatherRescaleClick(Sender: TObject);
    procedure MenuSoilShowLayersClick(Sender: TObject);
    procedure MenuSoilRescaleClick(Sender: TObject);
    procedure MenuPlantTurnLeftClick(Sender: TObject);
    procedure MenuPlantTurnRightClick(Sender: TObject);
    procedure MenuPlantZoomInClick(Sender: TObject);
    procedure MenuPlantZoomOutClick(Sender: TObject);
    procedure MenuPlantCenterClick(Sender: TObject);
    procedure MenuPlantViewFromTopClick(Sender: TObject);
    procedure MenuPlantViewFromSideClick(Sender: TObject);
    procedure MenuPlantDragModeClick(Sender: TObject);
    procedure MenuPlantHarvestModeClick(Sender: TObject);
    procedure MenuPlantReseedClick(Sender: TObject);
    procedure MenuSoilConnectLayerValuesClick(Sender: TObject);
    procedure growPlantClick(Sender: TObject);
    procedure MenuPlantGrowClick(Sender: TObject);
    procedure MenuShowHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    backTabbing: boolean;
    selectedComponent: KfBrowserComponent;
    deletedModel: GsModel;
    plantRotateAngle: single;
    plantDrawPosition: SinglePoint;
    plantScale: single;
    plantIsTopView: boolean;
    plantDragFrom: TPoint;
    soilDrawPanel: KfSoilDrawComponent;
    soilTextureTriangleDrawPanel: KfSoilTextureTriangleDrawComponent;
    weatherDrawPanel: KfWeatherDrawComponent;
    plantStatisticsPanel: KfPlantStatisticsDrawComponent;
    plantHarvestPanel: GsHarvestItemPanel;
    currentShowMode: smallint;
    splitterDragging : boolean;
    splitterNeedToRedraw : boolean;
    splitterStartPos : integer;
    splitterLastDrawPos : integer;
    lastWidth: integer;
    harvestMode: boolean;
    harvestIconBackupIsValid: boolean;
    harvestIconLastPlantPart: GsDrawingPlantPart;
    componentsWereExpandedLast: boolean;
    dragComponentY: integer;
    streamedObjectIndex: longint;
    streamedGroupIndex: longint;
    procedure createCustomPanels;
    procedure setStartingSettings;
		procedure resizePanelsToSplitter(topOfPanels: integer);
    function drawSplitterLine(pos: integer): integer;
    procedure undrawSplitterLine;
    procedure setSelectedModel(model: GsModel);
    function selectedModel: GsModel;
    function currentGarden: GsGarden;
    function currentWeather: GsWeather;
    function currentSoilPatch: GsSoilPatch;
    function currentPlant: GsPlant;
    function currentBag: GsBag;
    procedure updateForNewDomain;
		procedure updateGroups;
		procedure updateComponents;
    procedure updateObjectChoices;
    procedure updateObjectChoicesWithPointer;
    procedure updateObjectChoicesForDeletedObject;
    procedure updateComponentModels(updateRightSide: boolean);
 		procedure updateModelValuesForModel(aModel: GsModel);
    procedure updateModelValues;
		procedure modelDeleted(model: GsModel);
		procedure modelUndeleted(model: GsModel);
    procedure repositionComponents;
    function browserComponentForHandle(handle: HWnd): KfBrowserComponent;
    procedure updateAspectInfo;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    function isSoilPatchSelected: boolean;
    function isPlantSelected: boolean;
    function isWeatherSelected: boolean;
    function isBagSelected: boolean;
    procedure updateModelList(updateList: TListCollection);
    procedure resizeLeftPanelContents;
    procedure resizeRightPanelContents;
    procedure updatePlantDraw(plantIsSelected: boolean);
    procedure updateSoilDraw(patchIsSelected: boolean);
    procedure resizeSoilDrawPanel;
    procedure resizeSoilTextureTriangleDrawPanel;
		procedure showMode(showMode: integer);
    procedure setMode(showMode: integer);
    procedure updateWeatherDraw(weatherIsSelected: boolean);
    procedure resizeWeatherDrawPanel;
    procedure resizePlantStatisticsPanel;
    procedure resizePlantHarvestPanel;
    procedure resizePlantDrawBitmap;
    procedure resetPlantDrawBitmap;
    procedure adjustPlantDrawPosition(scaleChange: single);
    procedure plantDrawClick(sender: TObject);
		procedure addSelectionToGraph;
    procedure drawPlantHarvestItems;
    procedure selectTemplate(model: GsModel);
		function GetPalette: HPALETTE; override;
		function PaletteChanged(Foreground: Boolean): Boolean; override;
    function browserComponentForControlPanelY(y: integer): KfBrowserComponent;
    procedure updateComponentOrder;
    procedure updateMenusForSelectedModel;
    procedure enteringComponent(component: KfBrowserComponent);
    procedure leavingComponent(component: KfBrowserComponent);
    procedure updatePlantMenus(plantIsSelected: boolean);
    procedure enableOrDisablePlantDrawingMenus(drawingIsSelected: boolean);
    procedure updateSoilMenus(patchIsSelected: boolean);
    procedure updateWeatherMenus(weatherIsSelected: boolean);
    procedure streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
    procedure useStreamedChoicesAfterUpdateForNewDomain;
    procedure setMenuEnabling;
    procedure respondToSimulationRunningStatus(isRunning: boolean);
  end;

var
  BrowserForm: TBrowserForm;

implementation

uses uaspects, umconsts, ugroups, ugsim, udomain, ueutils, uunits,
  ubrowrl, ubrowrla, ubrowint, ubrowboo, ubrowscv, ubrowcol, uturt3d,
  ubrowtdo, ubrowlis, ubrowhar, ugscom, unotes, utempl, ucursor,
  udpfruit, udpleaf, udpinter, udpinflo, udpmeris, uicon, uharvprt, ubitmap, usupport;

{$R *.DFM}

const
  kBitButtonWidth = 22;
  kBetweenButtonGroupsGap = 4;
  kRotateIncrement = 16;
  kBetweenGap = 4;
  kMinObjectComboBoxWidth = 150;
  kMinGroupComboBoxWidth = 100;
  kMinSoilDrawDisplayComboBoxWidth = 80;
  kMinPlantDrawDisplayComboBoxWidth = 70;
  kSplitterDragMinPixels = 50;
  kDisplayPlantDrawing = 0;  { first }
  kDisplayPlantBiomass = 1;
  kDisplayPlantStresses = 2;

{ ----------------------------------------------------------------------------------------------- LOCAL PROCS }
procedure toggleTwoSpeedButtons(button1, button2: TSpeedButton);
  begin
  { push one down (can't pull up), other goes up }
  if not button1.down then
    button1.down := true
  else if not button2.down then
    button2.down := true;
  end;

{ ---------------------------------------------------------------------------------------- CREATION/DESTRUCTION }
constructor TBrowserForm.Create(AOwner: TComponent);
  begin
  inherited create(AOwner);
  end;

procedure TBrowserForm.FormCreate(Sender: TObject);
  begin
  plantDrawImage.visible := false;
  if Domain.paletteBitmapLoaded then
  	plantDrawImage.picture.bitmap.palette := CopyPalette(Domain.paletteBitmap.palette);
  self.createCustomPanels;
  self.setStartingSettings;
  end;

procedure TBrowserForm.createCustomPanels;
  begin
  { soil draw panel }
  self.soilDrawPanel := KfSoilDrawComponent.create(self);
  self.soilDrawPanel.parent := self.soilDrawControls.parent;
  self.soilDrawPanel.visible := false;
  self.soilDrawPanel.graphBars := true;
  self.soilDrawPanel.drawLines := true;
  self.resizeSoilDrawPanel;
  if Domain.paletteBitmapLoaded then
  	self.soilDrawPanel.profile.palette := CopyPalette(Domain.paletteBitmap.palette);
  { soil texture triangle draw panel }
  self.soilTextureTriangleDrawPanel := KfSoilTextureTriangleDrawComponent.create(self);
  self.soilTextureTriangleDrawPanel.parent := self.soilDrawControls.parent;
  self.soilTextureTriangleDrawPanel.visible := false;
  self.resizeSoilTextureTriangleDrawPanel;
  { weather draw panel }
  self.weatherDrawPanel := KfWeatherDrawComponent.create(self);
  self.weatherDrawPanel.parent := self.weatherDrawControls.parent;
  self.weatherDrawPanel.visible := false;
  self.weatherDrawPanel.graphBars := true;
  self.weatherDrawPanel.caption := 'Weather draw panel (unfinished)';
  self.resizeWeatherDrawPanel;
  { plant statistics panel }
  self.plantStatisticsPanel := KfPlantStatisticsDrawComponent.create(self);
  self.plantStatisticsPanel.parent := self.plantDrawControls.parent;
  self.plantStatisticsPanel.visible := false;
  self.resizePlantStatisticsPanel;
  { plant harvest panel }
  self.plantHarvestPanel := GsHarvestItemPanel.create(self);
  self.plantHarvestPanel.parent := self.plantDrawControls.parent;
  self.plantHarvestPanel.visible := false;
  self.resizePlantHarvestPanel;
  end;

procedure TBrowserForm.setStartingSettings;
  begin
  streamedObjectIndex := -1;
  streamedGroupIndex := -1;
  { plant drawing }
  plantRotateAngle := 0.0;
  plantDrawPosition.x := 100;
  plantDrawPosition.y := 200;
  plantScale := 1.0;
  plantIsTopView := false;
  plantDragMode.down := true;
  plantDrawFromSide.down := true;
  harvestMode := false;
  plantDrawImage.cursor := crSize;
  self.resetPlantDrawBitmap;
  harvestIconBackupIsValid := false;
  { right side displays }
  soilDrawDisplayMode.itemIndex := 0;
  graphSoilLines.enabled := false;
  graphSoilBars.enabled := false;
  rescaleSoilWhenLow.enabled := false;
  graphSoilBars.down := true;
  drawSoilLines.down := true;
  weatherDrawDisplayMode.itemIndex := 0;
  graphWeatherBars.down := true;
  plantDisplayMode.itemIndex := 0;
  { show mode and sizing }
  currentShowMode := kBrowserShowRight;
  self.menuLeftSide.checked := (self.currentShowMode = kBrowserShowLeft);
  self.MenuRightSide.checked := (self.currentShowMode = kBrowserShowRight);
  self.MenuBothSides.checked := (self.currentShowMode = kBrowserShowBoth);
  showRight.down := true;
  self.clientWidth := self.clientWidth div 2;
  lastWidth := self.clientWidth;
  splitter.left := self.clientWidth div 2 - splitter.width div 2;
  componentsWereExpandedLast := true;
  self.setMenuEnabling;
  end;

procedure TBrowserForm.setMenuEnabling;
  begin
  { set menu checkmarks based on down state of buttons }
  self.menuWeatherLineGraph.checked := self.graphWeatherLines.down;
  self.menuWeatherBarGraph.checked := not self.graphWeatherLines.down;
  self.menuWeatherShowMeans.checked := self.drawWeatherMeans.down;
  self.MenuSoilShowLayers.checked := self.drawSoilLines.down;
  self.MenuSoilConnectLayerValues.checked := self.graphSoilLines.down;
  self.MenuPlantViewFromTop.checked := self.plantIsTopView;
  self.MenuPlantViewFromSide.checked := not self.plantIsTopView;
  self.MenuPlantHarvestMode.checked := self.harvestMode;
  self.MenuPlantDragMode.checked := not self.harvestMode;
  self.MenuCollapseAll.enabled := (self.currentShowMode <> kBrowserShowRight);
  self.MenuExpandAll.enabled := (self.currentShowMode <> kBrowserShowRight);
  self.MenuGraphAspect.enabled := (self.currentShowMode <> kBrowserShowRight);
  end;

destructor TBrowserForm.destroy;
  begin
  { do not free soilDrawPanel because owner (self) will free it }
  soilDrawPanel := nil;
  { do not free soilDrawTextureTrianglePanel because owner (self) will free it }
  soilTextureTriangleDrawPanel := nil;
  { do not free weatherDrawPanel because owner (self) will free it }
  weatherDrawPanel := nil;
  plantStatisticsPanel := nil;
  { do not free plantHarvestPanel because owner (self) will free it }
  plantHarvestPanel := nil;
  inherited destroy;
  end;

procedure TBrowserForm.streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    newShowMode: smallint;
    tempSplitterLeft: longint;
  begin
  { the cvir passed in is the windowManager cvir, because this object is not a streamable object }
  if filer.isReading then self.setSelectedModel(nil);
  filer.streamSingle(plantRotateAngle);
  filer.streamBytes(plantDrawPosition, sizeOf(SinglePoint));
  filer.streamSingle(plantScale);
  filer.streamBoolean(plantIsTopView);
  if filer.isReading then
    begin
    filer.streamSmallint(newShowMode);
    if newShowMode <> currentShowMode then
      self.setMode(newShowMode);
    end
  else
    filer.streamSmallint(currentShowMode);
  filer.streamBoolean(harvestMode);
  filer.streamBoolean(componentsWereExpandedLast);
  self.setMenuEnabling;
  StreamFormPositionInfoWithFiler(filer, cvir, self);
  StreamComboBoxItemIndexWithFiler(filer, cvir, weatherDrawDisplayMode);
  StreamComboBoxItemIndexWithFiler(filer, cvir, soilDrawDisplayMode);
  StreamComboBoxItemIndexWithFiler(filer, cvir, plantDisplayMode);
  StreamComboBoxItemIndexToTempVarWithFiler(filer, cvir, group, streamedGroupIndex);
  StreamComboBoxItemIndexToTempVarWithFiler(filer, cvir, objectChoice, streamedObjectIndex);
  if filer.isReading then
    begin
    filer.streamLongint(tempSplitterLeft);
    if (currentShowMode = kBrowserShowBoth)
        and (tempSplitterLeft <> splitter.left)
        and (tempSplitterLeft >= kSplitterDragMinPixels)
        and (tempSplitterLeft < self.clientWidth - kSplitterDragMinPixels) then
      begin
      splitter.left := tempSplitterLeft;
      self.resizePanelsToSplitter(objectChoice.top + objectChoice.height + kBetweenGap);
      self.resizeLeftPanelContents;
      self.resizeRightPanelContents;
      end;
    end
  else
    begin
    tempSplitterLeft := splitter.left;
    filer.streamLongint(tempSplitterLeft);
    end;
  end;

{ ----------------------------------------------------------------------------------------------- ACTIONS }
procedure TBrowserForm.plantCenterClick(Sender: TObject);
  begin
  plantDrawPosition.x := plantDrawImage.width div 2;
  if self.plantIsTopView then
    plantDrawPosition.y := plantDrawImage.height div 2
  else
    plantDrawPosition.y := round(plantDrawImage.height * 0.9);
  plantScale := 1.0;
  plantDrawClick(self);
  end;

procedure TBrowserForm.plantDrawClick(Sender: TObject);
  var
    turtle: KfTurtle;
    viewingAngle: smallint;
   { oldPalette: HPALETTE; }
  begin
  if self.visible = false then exit;
  if self.currentPlant = nil then exit;
  if self.currentPlant.drawingPlant = nil then exit;
  harvestIconBackupIsValid := false;
  turtle := KfTurtle.defaultStartUsing;
  try
  if turtle <> nil then
    turtle.userWantsToRecordPosition := true;
  cursor_startWait;
  GardenForm.statusUpdateLabel('drawing ' + self.currentPlant.getName);
  { set up turtle and drawing surface }
  turtle.drawingSurface.pane := self.plantDrawImage.canvas;
  turtle.drawOptions := Domain.menuOptions.browserPlantDrawOptions;
  turtle.reset; { must be after pane and draw options set }
  turtle.mmPerPixel(plantScale);
  turtle.drawingSurface.foreColor := clGreen;
  turtle.drawingSurface.backColor := clRed;
  turtle.drawingSurface.lineColor := clBlue;
  {erase plantDrawImage}
  with plantDrawImage.canvas do
    begin
    brush.color := clWhite;
    pen.style := psClear;
    rectangle(0, 0, plantDrawImage.width, plantDrawImage.height);
    pen.style := psSolid;
    end;
  self.currentPlant.drawingPlant.plantWideDrawingVars;
  if plantIsTopView then
    viewingAngle := 64
  else
    viewingAngle := 0;
  turtle.xyz(plantDrawPosition.x, plantDrawPosition.y, 0);
  try
	  turtle.push;
   { try
    if Domain.paletteBitmapLoaded then
    	begin
    	oldPalette := SelectPalette(plantDrawImage.canvas.handle, Domain.paletteBitmap.palette, false);
    	RealizePalette(plantDrawImage.canvas.handle);
    	end; }
	  if turtle.drawOptions.sortPolygons then
		  begin
		  turtle.drawingSurface.recordingStart;
		  self.currentPlant.drawingPlant.drawWithTurtle(turtle, plantRotateAngle, viewingAngle);
		  turtle.drawingSurface.recordingStop;
		  turtle.drawingSurface.recordingDraw;
		  turtle.drawingSurface.clearTriangles;
		  end
	  else
		  begin
		  self.currentPlant.drawingPlant.drawWithTurtle(turtle, plantRotateAngle, viewingAngle);
	   end;
   { finally
    if Domain.paletteBitmapLoaded then
      begin
    	SelectPalette(plantDrawImage.canvas.handle, oldPalette, true);
      RealizePalette(plantDrawImage.canvas.handle);
      end;
		end; }
	  turtle.pop;
  except
	  on EDivByZero do ErrorMessage('TBrowserForm.plantDrawClick: EDivByZero');
	  on EInvalidOp do ErrorMessage('TBrowserForm.plantDrawClick: EInvalidOp');
	  on EOverFlow do ErrorMessage('TBrowserForm.plantDrawClick: EOverFlow');
	  on EUnderFlow do ErrorMessage('TBrowserForm.plantDrawClick: EUnderFlow');
  end;
  finally
  if turtle <> nil then
    turtle.userWantsToRecordPosition := false;
  KfTurtle.defaultStopUsing;
  turtle := nil;
 	GardenForm.statusClear;
  cursor_stopWait;
  end;
  self.drawPlantHarvestItems;
  end;

procedure TBrowserForm.drawPlantHarvestItems;
  begin
  if self.currentPlant = nil then exit;
  Domain.harvestManager.fillHarvestItemListForPlant(plantHarvestPanel.harvestItemList, self.currentPlant);
  plantHarvestPanel.enableOrDisableButtonsIfNeeded;
  plantHarvestPanel.invalidate;
  end;

procedure TBrowserForm.plantLeftClick(Sender: TObject);
	begin
	plantRotateAngle := (round(plantRotateAngle) + kRotateIncrement) mod 256;
  self.plantDrawClick(self);
	end;

procedure TBrowserForm.plantRightClick(Sender: TObject);
	begin
	plantRotateAngle := (round(plantRotateAngle) - kRotateIncrement) mod 256;
  self.plantDrawClick(self);
	end;

procedure TBrowserForm.adjustPlantDrawPosition(scaleChange: single);
  var
    newX, newY: single;
  begin
  if scaleChange = 0 then exit;
  try
  with plantDrawPosition, plantDrawImage do
    begin
    x := width * 0.5 + (x - 0.5 * width) * scaleChange;
    y := height * 0.5 + (y - 0.5 * height) * scaleChange;
    (*{limit the amount the plant can move around}
    if (newX < 32767) and (newX > -32767) then x := round(newX);
    if (newY < 32767) and (newY > -32767) then y := round(newY);
    *)
   end;
  except
  {do nothing}
  end;
  end;

procedure TBrowserForm.plantZoomMinusClick(Sender: TObject);
  var multiplier: single;
	begin
  multiplier := 0.75;
	plantScale := plantScale * multiplier;
  {put in arbitrary limit}
	if plantScale < 1.0E-16 then
    plantScale := 1.0E-16
  else
    self.adjustPlantDrawPosition(multiplier);
  self.plantDrawClick(self);
	end;

procedure TBrowserForm.PlantZoomPlusClick(Sender: TObject);
 var multiplier: single;
	begin
  multiplier := 1.5;
	plantScale := plantScale * multiplier;
  {put in arbitrary limit}
	if plantScale > 1.0E16 then
    plantScale := 1.0E16
  else
    self.adjustPlantDrawPosition(multiplier);
  self.plantDrawClick(self);
	end;

procedure TBrowserForm.plantDrawFromTopClick(Sender: TObject);
	begin
  plantIsTopView := true;
  self.MenuPlantViewFromTop.checked := self.plantIsTopView;
  self.MenuPlantViewFromSide.checked := not self.plantIsTopView;
  self.plantDrawClick(self);
	end;

procedure TBrowserForm.plantDrawFromSideClick(Sender: TObject);
  begin
  plantIsTopView := false;
  self.MenuPlantViewFromTop.checked := self.plantIsTopView;
  self.MenuPlantViewFromSide.checked := not self.plantIsTopView;
  self.plantDrawClick(self);
	end;

procedure TBrowserForm.plantDrawImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    foundPlantPart: GsDrawingPlantPart;
    removePlantPartsCommand: GsRemovePlantPartsCommand;
  begin
  if not harvestMode then
    begin
    plantDragFrom.x := x;
    plantDragFrom.y := y;
    end
  else
    begin
    if self.currentPlant <> nil then
      begin
      foundPlantPart := self.currentPlant.drawingPlant.findPlantPartAtPosition(point(x,y))
        as GsDrawingPlantPart;
      if foundPlantPart <> nil then
        begin
        removePlantPartsCommand := GsRemovePlantPartsCommand.create;
        removePlantPartsCommand.plant := self.currentPlant;
        removePlantPartsCommand.soilPatch := self.currentPlant.soil;
        removePlantPartsCommand.removedPlantPartsList.add(foundPlantPart);
        foundPlantPart.addDependentPartsToList(removePlantPartsCommand.removedPlantPartsList);
    	  GardenForm.doCommand(removePlantPartsCommand);
        end;
      end;
    end;
  end;

procedure TBrowserForm.plantDrawImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if not harvestMode then
    begin
    plantDrawPosition.x := plantDrawPosition.x + x - plantDragFrom.x;
    plantDrawPosition.y := plantDrawPosition.y + y - plantDragFrom.y;
    self.plantDrawClick(self);
    end
  else
    begin
    end;
  end;

procedure TBrowserForm.plantDrawImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  var
    foundPlantPart: GsDrawingPlantPart;
    icon: GsIcon;
    sourceRect: TRect;
  begin
  if not harvestMode then exit;
  if self.currentPlant <> nil then
    begin
    foundPlantPart := self.currentPlant.drawingPlant.findPlantPartAtPosition(point(x,y))
      as GsDrawingPlantPart;
    if foundPlantPart <> harvestIconLastPlantPart then
      begin
      if harvestIconBackupIsValid then
        begin
        harvestIconBackupIsValid := false;
        plantDrawImage.canvas.draw(
          round(harvestIconLastPlantPart.position.x),
          round(harvestIconLastPlantPart.position.y) - 32,
          harvestIconBackup.picture.bitmap);
        end;
      harvestIconLastPlantPart := foundPlantPart;
      if foundPlantPart = nil then exit;
      {copy plant bitmap to harvest backup}
      harvestIconBackupIsValid := true;
      sourceRect := Rect(
          round(harvestIconLastPlantPart.position.x),
          round(harvestIconLastPlantPart.position.y) - 32,
          round(harvestIconLastPlantPart.position.x) + 32,
          round(harvestIconLastPlantPart.position.y));
      harvestIconBackup.canvas.copyRect( harvestIconBackup.clientRect, plantDrawImage.canvas, sourceRect);
      icon := foundPlantPart.harvestItemIcon;
      if icon <> nil then
        plantDrawImage.canvas.draw(
          round(harvestIconLastPlantPart.position.x),
          round(harvestIconLastPlantPart.position.y) - 32,
          icon.icon)
      else
        plantDrawImage.canvas.draw(
          round(harvestIconLastPlantPart.position.x),
          round(harvestIconLastPlantPart.position.y) - 32,
          GardenForm.harvestIconDefault.picture.icon);
      end;
    end;
  end;

procedure TBrowserForm.becomeSeedClick(Sender: TObject);
  var
    reseedPlantCommand: GsReseedPlantCommand;
  begin
  if (self.currentPlant <> nil) and (not self.currentPlant.isTemplate) then
    begin
    reseedPlantCommand := GsReseedPlantCommand.createWithPlant(self.currentPlant, true); {kBecomeSeedAndStartGrowingNow}
    GardenForm.doCommand(reseedPlantCommand);
    end;
  end;

procedure TBrowserForm.growPlantClick(Sender: TObject);
  var
    growPlantCommand: GsGrowPlantCommand;
  begin
  if (self.currentPlant <> nil) and (not self.currentPlant.isTemplate) then
    begin
    growPlantCommand := GsGrowPlantCommand.createWithPlant(self.currentPlant);
    growPlantCommand.setDaysToGrowWithItemIndex(0); { 5 days }
    GardenForm.doCommand(growPlantCommand);
    end;
  end;

procedure TBrowserForm.reportClick(Sender: TObject);
  begin
  if (self.selectedModel <> nil) and (self.selectedModel is GsPlant) then
    begin
    self.currentPlant.drawingPlant.report;
    end;
  end;

procedure TBrowserForm.drawSoilLinesClick(Sender: TObject);
  begin
  soilDrawPanel.drawLines := drawSoilLines.down;
  soilDrawPanel.invalidate;
  self.MenuSoilShowLayers.checked := self.drawSoilLines.down;
  end;

procedure TBrowserForm.rescaleSoilWhenLowClick(Sender: TObject);
  begin
  soilDrawPanel.rescale := true;
  soilDrawPanel.updateDisplay;
  soilDrawPanel.repaint;
  end;

procedure TBrowserForm.drawWeatherMeansClick(Sender: TObject);
  begin
  weatherDrawPanel.drawMeans := drawWeatherMeans.down;
  weatherDrawPanel.invalidate;
  self.menuWeatherShowMeans.checked := self.drawWeatherMeans.down;
  end;

procedure TBrowserForm.rescaleWeatherWhenLowClick(Sender: TObject);
  begin
  weatherDrawPanel.rescale := true;
  weatherDrawPanel.updateDisplay;
  weatherDrawPanel.repaint;
  end;

procedure TBrowserForm.showLeftClick(Sender: TObject);
  begin
  self.showMode(kBrowserShowLeft);
  end;

procedure TBrowserForm.showRightClick(Sender: TObject);
  begin
  self.showMode(kBrowserShowRight);
  end;

procedure TBrowserForm.showBothClick(Sender: TObject);
  begin
  self.showMode(kBrowserShowBoth);
  end;

procedure TBrowserForm.notesButtonClick(Sender: TObject);
  var
    notesForm: TNotesForm;
    response: integer;
	begin
  if self.selectedModel <> nil then
    begin
    notesForm := TNotesForm.create(self);
    if notesForm = nil then
      raise Exception.create('Could not create notes window');
    try
      notesForm.initialize(kModelNote, nil, self.selectedModel, objectChoice.items);
		  response := notesForm.showModal;
    finally
      notesForm.free;
      notesForm := nil;
    end;
    if response = mrOK then
      begin
    	self.updateObjectChoicesWithPointer;
      if self.selectedModel.isTemplate then templatesFileMightHaveChanged := true;
      end;
    end;
	end;

procedure TBrowserForm.deleteObjectButtonClick(Sender: TObject);
  var
    theModel: GsModel;
    removeSoilPatchCommand: GsRemoveSoilPatchCommand;
    zapPlantCommand: GsZapPlantCommand;
    deleteTemplateCommand: GsDeleteTemplateCommand;
	begin
  theModel := self.selectedModel;
  if theModel = nil then exit;
  { cannot delete weather (but can delete climate template) }
  if (theModel = nil) or ((theModel is GsWeather) and (not theModel.isTemplate)) then
    begin
    MessageDlg('You cannot delete the Garden or Weather objects.', mtError, [mbOK], 0);
    exit;
    end;
  if MessageDlg('Delete ' + theModel.getName + '?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
  if theModel.isTemplate then
    begin
    deleteTemplateCommand := GsDeleteTemplateCommand.createCommand(theModel);
    GardenForm.doCommand(deleteTemplateCommand);
    templatesFileMightHaveChanged := true;
    end
  else
    begin { not a template }
    if theModel is GsSoilPatch then
      begin
      removeSoilPatchCommand := GsRemoveSoilPatchCommand.createCommand(theModel as GsSoilPatch);
      GardenForm.doCommand(removeSoilPatchCommand);
      end
    else if theModel is GsPlant then
      begin
      zapPlantCommand := GsZapPlantCommand.createCommand(theModel as GsPlant);
      GardenForm.doCommand(zapPlantCommand);
      end;
    end;
	end;

procedure TBrowserForm.templatesClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
    newModel: GsModel;
    copyOfSelectedPlant: GsPlant;
    response: integer;
    copied: boolean;
  begin
  if (self.selectedModel = nil) then exit;
  if Domain.templateManager = nil then exit;
  { restrict size of new name }
  newName := copy('Template from ' + self.selectedModel.getName, 1, kGraphicalModelNameLength);
  if not inputQuery('Enter new name', 'Enter a name for this new template.', newName) then exit;
  newModel := nil;
  case self.selectedModel.objectType of
    kObjectTypeWeather:
      newModel := Domain.templateManager.copyFromWeather(self.selectedModel as GsWeather, false);
    kObjectTypeSoil:
      newModel := Domain.templateManager.copyFromSoilPatch(self.selectedModel as GsSoilPatch, false);
    kObjectTypePlant:
      begin
      copied := false;
      if not (self.selectedModel as GsPlant).isTemplate then
        begin
        {ask if they want to make into seed or seedling template}
        response := MessageDlg('Would you like to save plant drawing information with the new cultivar?' + chr(13)
            + '(If you click No, the cultivar will be saved as a seed.)',
            mtConfirmation, [mbYes, mbNo, mbCancel], 0);
        if response = mrCancel then exit;
        if response = mrNo then
          begin
          copyOfSelectedPlant := nil;
          try
            copyOfSelectedPlant := GsPlant.create;
            copyOfSelectedPlant.soil := (self.selectedModel as GsPlant).soil;
            copyOfSelectedPlant.becomeTemplateBasedOn(self.selectedModel as GsPlant);
            copyOfSelectedPlant.becomeSeedNoReturns(true); {kBecomeSeedAndStartGrowingNow}
            newModel := Domain.templateManager.copyFromPlant(copyOfSelectedPlant, false);
            copied := true;
          finally
            copyOfSelectedPlant.free;
            copyOfSelectedPlant := nil;
          end;
          end;
        end;
      if not copied then
        newModel := Domain.templateManager.copyFromPlant(self.selectedModel as GsPlant, false);
      end;
    kObjectTypeBag:
      newModel := Domain.templateManager.copyFromBag(self.selectedModel as GsBag);
  	else
    	raise Exception.create('TBrowserForm.templatesClick: unknown object type ' + IntToStr(self.selectedModel.objectType));
  	end;
  if newModel = nil then
  	exit;
  newModel.setName(newName);
  { for patches and plants, update template name field }
  if (newModel is GsSoilPatch) then
    (newModel as GsSoilPatch).setSoilTypeName(newName)
  else if (newModel is GsPlant) then
    (newModel as GsPlant).setCultivarName(newName);
  self.updateObjectChoices;
	self.setSelectedModel(newModel);
  templatesFileMightHaveChanged := true;
  end;

procedure TBrowserForm.expandAllClick(Sender: TObject);
  var
    i: integer;
    control: KfBrowserComponent;
  begin
  if controlPanel.controlCount > 0 then
    begin
    for i := 0 to controlPanel.controlCount - 1 do
      begin
      control := KfBrowserComponent(controlPanel.controls[i]);
      control.expand;
      end;
    self.repositionComponents;
    end;
  componentsWereExpandedLast := true;
  end;

procedure TBrowserForm.collapseAllClick(Sender: TObject);
  var
    i: integer;
    control: KfBrowserComponent;
  begin
  if controlPanel.controlCount > 0 then
    begin
    for i := 0 to controlPanel.controlCount - 1 do
      begin
      control := KfBrowserComponent(controlPanel.controls[i]);
      control.collapse;
      end;
    self.repositionComponents;
    end;
  componentsWereExpandedLast := false;
  end;

procedure TBrowserForm.chooseFontClick(Sender: TObject);
  var i: integer;
  begin
  FontDialog1.Font := controlPanel.Font;
  if FontDialog1.Execute then
    begin
    self.Font := FontDialog1.Font;
    if controlPanel.controlCount > 0 then for i := 0 to controlPanel.controlCount - 1 do
      KfBrowserComponent(controlPanel.controls[i]).calculateTextDimensions;
    self.repositionComponents;
    end;
  end;

procedure TBrowserForm.showMode(showMode: integer);
  begin
  self.setMode(showMode);
  self.menuLeftSide.checked := (self.currentShowMode = kBrowserShowLeft);
  self.MenuRightSide.checked := (self.currentShowMode = kBrowserShowRight);
  self.MenuBothSides.checked := (self.currentShowMode = kBrowserShowBoth);
  self.selectedComponent := nil;
  controlPanel.invalidate;
  graph.enabled := false;
  MenuGraphAspect.enabled := false;
  self.show;
  end;

procedure TBrowserForm.setMode(showMode: integer);
  var
    oldShowMode: integer;
    newWidth: longint;
  begin
  if showMode <> currentShowMode then
    begin
    {need to set show mode before do resize because form resize will reference it}
    oldShowMode := currentShowMode;
    currentShowMode := showMode;
    case showMode of
     	kBrowserShowBoth:
        begin
        newWidth := intMin(600, self.width * 2);
        if self.left + newWidth > screen.width then
          if screen.width - newWidth > 0 then
            self.left := screen.width - newWidth;
        self.width := newWidth;
  			showBoth.down := true;
        end;
  	  kBrowserShowLeft:
        begin
        if oldShowMode = kBrowserShowBoth then
          self.width := intMax(250, self.width div 2)
        else
          self.FormResize(self); {this will move panels around}
  			showLeft.down := true;
        end;
     	kBrowserShowRight:
        begin
        if oldShowMode = kBrowserShowBoth then
          self.width := intMax(250, self.width div 2)
        else
          self.FormResize(self); {this will move panels around}
  			showRight.down := true;
        end;
      else
        raise Exception.create('Error in showMode');
      end;
    end;
  self.MenuCollapseAll.enabled := (self.currentShowMode <> kBrowserShowRight);
  self.MenuExpandAll.enabled := (self.currentShowMode <> kBrowserShowRight);
  self.MenuGraphAspect.enabled := (self.currentShowMode <> kBrowserShowRight);
  self.updateMenusForSelectedModel;
  end;

procedure TBrowserForm.EditGroupClick(Sender: TObject);
  var
    groupEditor: TGroupEditorForm;
    saveOrCancel, newItemIndex: integer;
    currentGroupName: string;
    currentGroup: GsGroup;
  begin
  groupEditor := TGroupEditorForm.create(self);
  if groupEditor = nil then
    raise Exception.create('Could not create group editor');
  try
    { Note we are using the current group's name to find it again after the user closes the
      group editor, because pointers will not work because the group editor replaces the
      entire group manager. This means that if a user goes into the group editor and
      renames the current group, it will not be found on their return. Not the end of the world. }
    {only do if group selected}
    if group.itemIndex >= 0 then
      begin
    	currentGroupName := group.items[group.itemIndex];
    	currentGroup := group.items.objects[group.itemIndex] as GsGroup;
    	groupEditor.selectGroup(currentGroup);
    	groupEditor.setObjectTypeForModel(selectedModel);
      end;
    saveOrCancel := groupEditor.showModal;
  finally
    groupEditor.free;
    groupEditor := nil;
  end;
  if saveOrCancel = idOK then
    begin
    self.updateGroups;
    { reset to group selected before clicking button }
    newItemIndex := group.items.indexOf(currentGroupName);
    if newItemIndex <> -1 then
    	group.itemIndex := newItemIndex
    else
      begin
      if group.items.count >= 0 then
      	group.itemIndex := 0
      else
        group.itemIndex := -1;
      end;
    self.updateComponents;
    self.updateComponentModels(false);
    end;
  end;

procedure TBrowserForm.GraphClick(Sender: TObject);
  var
    aspect: GsAspect;
    groupItem: GsGroupItem;
    model: GsModel;
  begin
  self.addSelectionToGraph;
  GraphForm.show;
  end;

procedure TBrowserForm.enteringComponent(component: KfBrowserComponent);
  begin
  self.selectedComponent := component;
  graph.enabled := component.canBeGraphed;
  self.MenuGraphAspect.enabled := graph.enabled;
  end;

procedure TBrowserForm.leavingComponent(component: KfBrowserComponent);
  begin
  { not using component pointer yet but might later, so passing it }
  self.selectedComponent := nil;
  graph.enabled := false;
  self.MenuGraphAspect.enabled := false;
  end;

procedure TBrowserForm.addSelectionToGraph;
  var
    aspect: GsAspect;
    groupItem: GsGroupItem;
    model: GsModel;
  begin
  GraphForm.show;
  if self.selectedModel = nil then exit;
  if self.selectedModel.isTemplate then exit;
  if self.selectedComponent = nil then exit;
  if not (self.selectedComponent is KfBrowserComponent) then exit;
  if not (self.selectedComponent as KfBrowserComponent).canBeGraphed then exit;
  aspect := (self.selectedComponent as KfBrowserComponent).aspect;
  groupItem := (self.selectedComponent as KfBrowserComponent).groupItem;
  if groupItem = nil then exit;
  model := nil;
  case aspect.objectType of
    kObjectTypeGarden: model := self.currentGarden;
    kObjectTypeWeather: model := self.currentWeather; 
    kObjectTypeSoil: model := self.currentSoilPatch;
    kObjectTypePlant, kObjectTypeDrawingPlant: model := self.currentPlant;
    end;
  if model = nil then exit;
  GraphForm.addPlotAspect(self.selectedModel, aspect, groupItem);
  end;

procedure TBrowserForm.plantHarvestModeClick(Sender: TObject);
  begin
  harvestMode := true;
  plantDrawImage.cursor := crGloveHarvest;
  self.MenuPlantHarvestMode.checked := self.harvestMode;
  self.MenuPlantDragMode.checked := not self.harvestMode;
  end;

procedure TBrowserForm.plantDragModeClick(Sender: TObject);
  begin
  harvestMode := false;
  plantDrawImage.cursor := crSize;
  self.MenuPlantHarvestMode.checked := self.harvestMode;
  self.MenuPlantDragMode.checked := not self.harvestMode;
  end;

procedure TBrowserForm.graphSoilLinesClick(Sender: TObject);
  begin
  soilDrawPanel.graphBars := not graphSoilLines.down;
  soilDrawPanel.invalidate;
  self.MenuSoilConnectLayerValues.checked := self.graphSoilLines.down;
  end;

procedure TBrowserForm.graphSoilBarsClick(Sender: TObject);
  begin
  soilDrawPanel.graphBars := graphSoilBars.down;
  soilDrawPanel.invalidate;
  end;

procedure TBrowserForm.graphWeatherLinesClick(Sender: TObject);
  begin
  weatherDrawPanel.graphBars := not graphWeatherLines.down;
  weatherDrawPanel.invalidate;
  self.menuWeatherLineGraph.checked := self.graphWeatherLines.down;
  self.menuWeatherBarGraph.checked := not self.graphWeatherLines.down;
  end;

procedure TBrowserForm.graphWeatherBarsClick(Sender: TObject);
  begin
  weatherDrawPanel.graphBars := graphWeatherBars.down;
  weatherDrawPanel.invalidate;
  self.menuWeatherLineGraph.checked := self.graphWeatherLines.down;
  self.menuWeatherBarGraph.checked := not self.graphWeatherLines.down;
  end;

procedure TBrowserForm.helpButtonClick(Sender: TObject);
  var
    aspect: GsAspect;
  begin
  aspect := nil;
  if (self.selectedModel <> nil) and (self.selectedComponent <> nil)
    and (self.selectedComponent is KfBrowserComponent) then
    aspect := (self.selectedComponent as KfBrowserComponent).aspect;
  if aspect = nil then
    application.helpJump('windows_Browser')
  else if aspect.hasHelp then
    application.helpJump(aspect.fieldID)
  else
    application.helpJump('sections_Default_aspect_groups');
  end;

{ ----------------------------------------------------------------------------------------------- UPDATING }
procedure TBrowserForm.updateGroups;
  var savedIndex: longint;
  begin
  savedIndex := group.itemIndex;
  Domain.groupManager.loadGroupsIntoComboBox(group);
  if (savedIndex >= 0) and (savedIndex <= group.items.count - 1) then
    group.itemIndex := savedIndex;
  end;

procedure TBrowserForm.updateForNewDomain;
  begin
  deletedModel := nil;
  self.updateObjectChoices;
  self.updateGroups;
  self.useStreamedChoicesAfterUpdateForNewDomain;
  self.updateComponents;
  self.updateComponentModels(true);
  self.backTabbing := false;
  end;

procedure TBrowserForm.useStreamedChoicesAfterUpdateForNewDomain;
  begin
  if streamedObjectIndex >= 0 then
    begin
    if (streamedObjectIndex <= objectChoice.items.count - 1) then
      objectChoice.itemIndex := streamedObjectIndex;
    streamedObjectIndex := -1;
    end
  else
    objectChoice.itemIndex := 0;
  if streamedGroupIndex >= 0 then
    begin
    if (streamedGroupIndex <= group.items.count - 1) then
      group.itemIndex := streamedGroupIndex;
    streamedGroupIndex := -1;
    end
  else
    group.itemIndex := 0;
  end;

procedure TBrowserForm.respondToSimulationRunningStatus(isRunning: boolean);
  begin
  { can't edit groups if running }
  editGroup.enabled := not isRunning;
  MenuEditGroups.enabled := editGroup.enabled;
  { can't make template if running or no object selected}
  templates.enabled := (not isRunning) and (self.selectedModel <> nil);
  MenuObjectMakeTemplate.enabled := templates.enabled;
  { can't change notes if running or no object selected }
  notesButton.enabled := (not isRunning) and (self.selectedModel <> nil);
  MenuObjectNotes.enabled := notesButton.enabled;
  { can't delete an object if running or no object selected }
  deleteObjectButton.enabled := (not isRunning) and (self.selectedModel <> nil);
  MenuObjectDelete.enabled := deleteObjectButton.enabled;
  { can't go into harvest mode if not running or no plant selected or plant is template or not drawing mode }
  plantHarvestMode.enabled := (not isRunning) and (self.currentPlant <> nil) and (not self.currentPlant.isTemplate)
    and (plantDisplayMode.itemIndex = kDisplayPlantDrawing);
  MenuPlantHarvestMode.enabled := plantHarvestMode.enabled;
  { can't reseed }
  becomeSeed.enabled := (not isRunning) and (self.currentPlant <> nil) and (not self.currentPlant.isTemplate);
  MenuPlantReseed.enabled := becomeSeed.enabled;
  { can't grow plant }
  growPlant.enabled := (not isRunning) and (self.currentPlant <> nil) and (not self.currentPlant.isTemplate);
  MenuPlantGrow.enabled := growPlant.enabled;
  end;

procedure TBrowserForm.updateComponents;
  var
	  component, oldComponent: KfBrowserComponent;
	  currentGroup: GsGroup;
    groupItem: GsGroupItem;
    aspect: GsAspect;
	  i: integer;
  begin
  {clear out any earlier components}
  self.selectedComponent := nil;
  graph.enabled := false;
  self.MenuGraphAspect.enabled := false;
  if controlPanel.controlCount > 0 then
	  while controlPanel.controlCount > 0 do
  	  begin
      oldComponent := KfBrowserComponent(controlPanel.controls[0]);
      oldComponent.parent := nil;
      self.removeComponent(oldComponent);
      { this is the only valid time to free a component owned by something - after it has
        been removed from the component list (owned object) of the owner }
      oldComponent.free;
      end;
  if group.itemIndex >= 0 then
	  begin
    currentGroup := group.items.objects[group.itemIndex] as GsGroup;
    if currentGroup.groupItems.count > 0 then
  	  for i := 0 to currentGroup.groupItems.count - 1 do
  		  begin
        groupItem := currentGroup.groupItemForIndex(i);
        try
          aspect := currentGroup.aspectForIndex(i);
        except
          showMessage('Aspect ' + groupItem.fieldID + chr(13)
            + ' not found; not showing group item in group "'
            + currentGroup.name + '".');
          aspect := nil;
        end;
        if aspect = nil then continue;
        case aspect.fieldType of
          kFieldHarvestItemTemplate:
            component := KfHarvestItemTemplateBrowserComponent.createField(self, aspect, groupItem);
          kFieldEnumeratedList:
            component := KfEnumChoiceBrowserComponent.createField(self, aspect, groupItem);
          kFieldThreeDObject:
            component := KfObject3DBrowserComponent.createField(self, aspect, groupItem);
          kFieldColor:
            component := KfColorBrowserComponent.createField(self, aspect, groupItem);
          kFieldInt:
            component := KfIntegerSliderBrowserComponent.createField(self, aspect, groupItem);
          kFieldFloat:
            begin
            if aspect.indexCount <= 1 then
				      component := KfRealSliderBrowserComponent.createField(self, aspect, groupItem)
            else
              case aspect.indexType of
                kIndexTypeSCurve:
                  component := KfSCurveBrowserComponent.createField(self, aspect, groupItem);
                kIndexTypeLayer:
                  begin
                  if (groupItem.arrayShowType = kArrayShowAllValues) then
                    component := KfRealSliderArrayBrowserComponent.createField(self, aspect, groupItem)
                  else
                    begin
 				            component := KfRealSliderBrowserComponent.createField(self, aspect, groupItem);
                    if groupItem.arrayShowType = kArrayShowOneIndex then
                      KfRealSliderBrowserComponent(component).arrayIndex :=
                        GsGroupItem.firstArrayIndexSelected(groupItem.arraySelected)
                    else
                      component.readOnly := true;
                    end;
                 end;
              else
                component := KfRealSliderArrayBrowserComponent.createField(self, aspect, groupItem);
              end;
            end;
          kFieldBoolean:
            component := KfBooleanRadioBrowserComponent.createField(self, aspect, groupItem);
        else    {default to float for now}
          component := KfRealSliderBrowserComponent.createField(self, aspect, groupItem);
        end;
			  component.parent := ControlPanel;
        component.tabOrder := 9 + i;
        component.collapse;
			  component.calculateTextDimensions;
        component.calculateHeight;
    	  end;
    if componentsWereExpandedLast then
      self.expandAllClick(self) { calls repositionComponents }
    else
      self.repositionComponents;
	  end;
  end;

{usually means soilPatch or plant has changed}
procedure TBrowserForm.updateComponentModels(updateRightSide: boolean);
var
  i: integer;
  component: KfBrowserComponent;
begin
self.updateMenusForSelectedModel;
if ControlPanel.ControlCount > 0 then
  	for i := 0 to ControlPanel.ControlCount - 1 do
      if ControlPanel.Controls[i] is KfBrowserComponent then
     		begin
      	component := ControlPanel.Controls[i] as KfBrowserComponent;
      	{components check if the new model is different from current one}
      	case component.aspect.objectType of
  				kObjectTypeGarden: component.updateModel(self.currentGarden);
  				kObjectTypeWeather: component.updateModel(self.currentWeather);
  				kObjectTypeSoil: component.updateModel(self.currentSoilPatch);
  				kObjectTypePlant, kObjectTypeDrawingPlant: component.updateModel(self.currentPlant);
          kObjectTypeBag: component.updateModel(self.currentBag);
  			else
   				raise Exception.create('unsupported browser object type ' + IntToStr(component.aspect.objectType));
				end;
      	end;
  self.repositionComponents;
  if updateRightSide then
    begin
    self.updatePlantDraw(self.isPlantSelected);
    self.updateSoilDraw(self.isSoilPatchSelected);
    self.updateWeatherDraw(self.isWeatherSelected and not self.selectedModel.isTemplate);
    end;
  end;

procedure TBrowserForm.updateMenusForSelectedModel;
  var objectIsSelected, rightSideShowing: boolean;
  begin
  { can make notes on any object including garden }
  self.MenuObjectNotes.enabled := (self.selectedModel <> nil);
  objectIsSelected := (self.selectedModel <> nil);
  { can make template from any object not the garden (including a template) }
  self.MenuObjectMakeTemplate.enabled := objectIsSelected;
  { can delete any object not the garden (including a template) }
  self.MenuObjectDelete.enabled := objectIsSelected;
  rightSideShowing := ((currentShowMode = kBrowserShowRight) or (currentShowMode = kBrowserShowBoth));
  self.updatePlantMenus(rightSideShowing and self.isPlantSelected);
  self.updateSoilMenus(rightSideShowing and self.isSoilPatchSelected);
  self.updateWeatherMenus(rightSideShowing and self.isWeatherSelected and not self.selectedModel.isTemplate);
  end;

procedure TBrowserForm.updateModelValuesForModel(aModel: GsModel);
	var
  	i: integer;
  	component: KfBrowserComponent;
	begin
  if aModel = nil then exit;
  if ControlPanel = nil then exit;
  { update components }
  if ControlPanel.ControlCount > 0 then
  	for i := 0 to ControlPanel.ControlCount - 1 do
      if ControlPanel.Controls[i] is KfBrowserComponent then
     		begin
      	component := ControlPanel.Controls[i] as KfBrowserComponent;
      	{ components refresh only if any of their values have changed }
        if component.model = aModel then
  				component.updateModelValues;
      	end;
  { update right side of browser }
  if aModel <> self.selectedModel then exit;
  case aModel.objectType of
  	kObjectTypeGarden: ;
  	kObjectTypeWeather: self.updateWeatherDraw(self.isWeatherSelected and not self.selectedModel.isTemplate);
  	kObjectTypeSoil: self.updateSoilDraw(self.isSoilPatchSelected);
  	kObjectTypePlant, kObjectTypeDrawingPlant: self.updatePlantDraw(self.isPlantSelected);
    kObjectTypeBag: ;
    end;
  end;

procedure TBrowserForm.updateModelValues;
var
  i: integer;
  component: KfBrowserComponent;
begin
  if ControlPanel = nil then exit;
  { update components }
  if ControlPanel.ControlCount > 0 then
  	for i := 0 to ControlPanel.ControlCount - 1 do
      if ControlPanel.Controls[i] is KfBrowserComponent then
     		begin
      	component := ControlPanel.Controls[i] as KfBrowserComponent;
      	{ components refresh only if any of their values have changed }
  			component.updateModelValues;
      	end;
  { update right side of browser }
  self.updatePlantDraw(self.isPlantSelected);
  self.updateSoilDraw(self.isSoilPatchSelected);
  self.updateWeatherDraw(self.isWeatherSelected and not self.selectedModel.isTemplate);
  end;

procedure TBrowserForm.updatePlantDraw(plantIsSelected: boolean);
  var
    plantIsEnabled: boolean;
  begin
  plantIsEnabled := plantIsSelected and (currentShowMode <> kBrowserShowLeft);
  plantHarvestPanel.visible := plantIsEnabled;
  plantDrawControls.visible := plantIsEnabled;
  if plantIsEnabled then
    begin
    plantDrawControls.top := 0;
    plantDrawControls.width := rightPanel.width;
    self.resizePlantHarvestPanel;
    plantStatisticsPanel.updatePlant(self.currentPlant);
    self.drawPlantHarvestItems;
    self.plantDisplayModeChange(self); {determines whether stats or drawing is visible}
    end
  else
    begin
    plantDrawImage.visible := false;
    plantStatisticsPanel.visible := false;
    plantStatisticsPanel.updatePlant(nil);
    self.resetPlantDrawBitmap;
    end;
  self.updatePlantMenus(plantIsEnabled);
  end;

procedure TBrowserForm.plantDisplayModeChange(Sender: TObject);
  var
    isDrawing: boolean;
  begin
  isDrawing := self.isPlantSelected and (plantDisplayMode.itemIndex = kDisplayPlantDrawing);
  plantLeft.enabled := isDrawing;
  plantRight.enabled := isDrawing;
  plantZoomMinus.enabled := isDrawing;
  plantZoomPlus.enabled := isDrawing;
  plantCenter.enabled := isDrawing;
  plantDrawFromSide.enabled := isDrawing;
  plantDrawFromTop.enabled := isDrawing;
  plantDragMode.enabled := isDrawing;
  plantHarvestMode.enabled := isDrawing;
  plantDrawImage.visible := isDrawing;
  plantStatisticsPanel.visible := not isDrawing;
  if isDrawing then
    begin
    self.resizePlantDrawBitmap;
    self.plantDrawClick(self);
    end
  else
    begin
    if plantDisplayMode.itemIndex = kDisplayPlantStresses then
      plantStatisticsPanel.displayMode := kDisplayStresses
    else
      plantStatisticsPanel.displayMode := kDisplayStatistics;
    self.resizePlantStatisticsPanel;
    plantStatisticsPanel.updateCurrentValue(0);
    plantStatisticsPanel.resizeElements;
    self.resetPlantDrawBitmap;
    end;
  self.enableOrDisablePlantDrawingMenus(isDrawing);
  end;

procedure TBrowserForm.updatePlantMenus(plantIsSelected: boolean);
  var currentPlantIsNotATemplate: boolean;
  begin
  { don't reset menus if they are already set the right way, because they are all the same }
  if self.MenuPlantStartSeparator.visible <> plantIsSelected then
    begin
    self.MenuPlantStartSeparator.visible := plantIsSelected;
    self.MenuPlantSeparator1.visible := plantIsSelected;
    self.MenuPlantSeparator2.visible := plantIsSelected;
    self.MenuPlantSeparator3.visible := plantIsSelected;
    self.MenuPlantTurnLeft.visible := plantIsSelected;
    self.MenuPlantTurnRight.visible := plantIsSelected;
    self.MenuPlantZoomIn.visible := plantIsSelected;
    self.MenuPlantZoomOut.visible := plantIsSelected;
    self.MenuPlantCenter.visible := plantIsSelected;
    self.MenuPlantViewFromTop.visible := plantIsSelected;
    self.MenuPlantViewFromSide.visible := plantIsSelected;
    self.MenuPlantDragMode.visible := plantIsSelected;
    self.MenuPlantHarvestMode.visible := plantIsSelected;
    self.MenuPlantReseed.visible := plantIsSelected;
    self.MenuPlantGrow.visible := plantIsSelected;
    end;
  if plantIsSelected then
    begin
    { only enable grow, reseed, harvest if plant is NOT a template }
    currentPlantIsNotATemplate := not self.currentPlant.isTemplate;
    self.growPlant.enabled := currentPlantIsNotATemplate;
    self.becomeSeed.enabled := currentPlantIsNotATemplate;
    self.plantHarvestMode.enabled := currentPlantIsNotATemplate
        and (plantDisplayMode.itemIndex = kDisplayPlantDrawing);
    self.MenuPlantHarvestMode.enabled := currentPlantIsNotATemplate
        and (plantDisplayMode.itemIndex = kDisplayPlantDrawing);
    self.MenuPlantReseed.enabled := currentPlantIsNotATemplate;
    self.MenuPlantGrow.enabled := currentPlantIsNotATemplate;
    end;
  end;

procedure TBrowserForm.enableOrDisablePlantDrawingMenus(drawingIsSelected: boolean);
  begin
  { don't reset menus if they are already set the right way, because they are all the same }
  if self.MenuPlantTurnLeft.enabled <> drawingIsSelected then
    begin
    self.MenuPlantTurnLeft.enabled := drawingIsSelected;
    self.MenuPlantTurnRight.enabled := drawingIsSelected;
    self.MenuPlantZoomIn.enabled := drawingIsSelected;
    self.MenuPlantZoomOut.enabled := drawingIsSelected;
    self.MenuPlantCenter.enabled := drawingIsSelected;
    self.MenuPlantViewFromTop.enabled := drawingIsSelected;
    self.MenuPlantViewFromSide.enabled := drawingIsSelected;
    self.MenuPlantDragMode.enabled := drawingIsSelected;
    self.MenuPlantHarvestMode.enabled := drawingIsSelected;
    end;
  end;

procedure TBrowserForm.updateSoilDraw(patchIsSelected: boolean);
  begin
  if patchIsSelected then
    begin
    with soilDrawControls do setBounds(0, 0, rightPanel.width, height);
    soilDrawControls.visible := true;
    self.soilDrawDisplayModeChange(self);
    if (soilDrawPanel <> nil) then
      begin
      soilDrawPanel.updateSoilPatch(self.currentSoilPatch);
      soilDrawPanel.updateCurrentValue(0);
      self.resizeSoilDrawPanel;
      soilDrawPanel.resizeElements;
      end;
    if (soilTextureTriangleDrawPanel <> nil) then
      begin
      soilTextureTriangleDrawPanel.updateSoilPatch(self.currentSoilPatch);
      soilTextureTriangleDrawPanel.updateCurrentValue(0);
      self.resizeSoilTextureTriangleDrawPanel;
      soilTextureTriangleDrawPanel.resizeElements;
      end;
    end
  else
    begin
    soilDrawPanel.visible := false;
    soilTextureTriangleDrawPanel.visible := false;
    soilDrawControls.visible := false;
    soilDrawPanel.updateSoilPatch(nil);
    soilTextureTriangleDrawPanel.updateSoilPatch(nil);
    end;
  self.updateSoilMenus(patchIsSelected and (self.currentShowMode <> kBrowserShowLeft));
  end;

procedure TBrowserForm.soilDrawDisplayModeChange(Sender: TObject);
  var index: longint;
  begin
  index := soilDrawDisplayMode.itemIndex;
  { enable/disable buttons for different display modes (menus follow buttons in updateSoilMenus) }
  { enabled layers button for all modes except texture }
  drawSoilLines.enabled := (index <> kDisplayTexture);
  { enable lines/bars buttons for all modes except color and texture }
  graphSoilLines.enabled := (index <> kDisplayColor) and (index <> kDisplayTexture);
  graphSoilBars.enabled := (index <> kDisplayColor) and (index <> kDisplayTexture);
  { enable rescale button for all modes except color, texture, contents, and pH }
  rescaleSoilWhenLow.enabled := (index <> kDisplayColor) and (index <> kDisplayTexture)
      and (index <> kDisplayContents) and (index <> kDisplayPH);
  { make soil texture triangle panel visible if texture chosen, otherwise make soil draw panel visible }
  if soilDrawDisplayMode.itemIndex = kDisplayTexture then
    begin
    soilDrawPanel.visible := false;
    soilTextureTriangleDrawPanel.visible := true;
    end
  else
    begin
    soilTextureTriangleDrawPanel.visible := false;
    soilDrawPanel.visible := true;
    if soilDrawPanel.displayMode <> soilDrawDisplayMode.itemIndex then
      begin
      soilDrawPanel.displayMode := soilDrawDisplayMode.itemIndex;
      soilDrawPanel.setDefaultOverlayUnit;
      end;
    end;
  self.updateSoilMenus(self.isSoilPatchSelected and (self.currentShowMode <> kBrowserShowLeft));
  end;

procedure TBrowserForm.updateSoilMenus(patchIsSelected: boolean);
  begin
  if self.MenuSoilStartSeparator.visible <> patchIsSelected then
    begin
    self.MenuSoilStartSeparator.visible := patchIsSelected;
    self.MenuSoilShowLayers.visible := patchIsSelected;
    self.MenuSoilConnectLayerValues.visible := patchIsSelected;
    self.MenuSoilSeparator.visible := patchIsSelected;
    self.MenuSoilRescale.visible := patchIsSelected;
    end;
  if patchIsSelected then
    begin
    { because of this part this function should be called after soilDrawDisplayModeChange }
    menuSoilShowLayers.enabled := drawSoilLines.enabled;
    menuSoilConnectLayerValues.enabled := graphSoilLines.enabled;
    menuSoilRescale.enabled := rescaleSoilWhenLow.enabled;
    end;
  end;

procedure TBrowserForm.updateWeatherDraw(weatherIsSelected: boolean);
  begin
  if weatherIsSelected then
    begin
    with weatherDrawControls do setBounds(0, 0, rightPanel.width, height);
  	if (weatherDrawPanel <> nil) then self.resizeWeatherDrawPanel;
    self.weatherDrawPanel.visible := true;
    self.weatherDrawControls.visible := true;
    self.weatherDrawPanel.updateWeather(self.currentWeather); {a climate has no right side}
    self.weatherDrawPanel.updateCurrentValue(0);
    self.weatherDrawPanel.resizeElements;
    self.weatherDrawDisplayModeChange(self);
    end
  else
    begin
    self.weatherDrawPanel.visible := false;
    self.weatherDrawControls.visible := false;
    self.weatherDrawPanel.updateWeather(nil);
    end;
  self.updateWeatherMenus(weatherIsSelected and (self.currentShowMode <> kBrowserShowLeft));
  end;

procedure TBrowserForm.weatherDrawDisplayModeChange(Sender: TObject);
  begin
  if weatherDrawPanel.displayMode <> weatherDrawDisplayMode.itemIndex then
    begin
    weatherDrawPanel.displayMode := weatherDrawDisplayMode.itemIndex;
    weatherDrawPanel.setDefaultOverlayUnit;
    weatherDrawPanel.repaint;
    end;
  end;

procedure TBrowserForm.updateWeatherMenus(weatherIsSelected: boolean);
  begin
  if self.MenuWeatherStartSeparator.visible <> weatherIsSelected then
    begin
    self.MenuWeatherStartSeparator.visible := weatherIsSelected;
    self.MenuWeatherLineGraph.visible := weatherIsSelected;
    self.MenuWeatherBarGraph.visible := weatherIsSelected;
    self.MenuWeatherSeparator1.visible := weatherIsSelected;
    self.MenuWeatherShowMeans.visible := weatherIsSelected;
    self.MenuWeatherSeparator2.visible := weatherIsSelected;
    self.MenuWeatherRescale.visible := weatherIsSelected;
    end;
  end;

procedure TBrowserForm.updateAspectInfo;
var
  i: integer;
  component: KfBrowserComponent;
begin
  if ControlPanel.ControlCount > 0 then
  	for i := 0 to ControlPanel.ControlCount - 1 do
      if ControlPanel.Controls[i] is KfBrowserComponent then
     		begin
      	component := ControlPanel.Controls[i] as KfBrowserComponent;
      	{components reload information from their aspect}
        component.updateAspectInfo;
      	end;
  end;

procedure TBrowserForm.updateObjectChoices;
  var
    savedObjectName: string;
	begin
  { cannot use index, because a new object may have been added;
    if pointers may have been changed by template editor, cannot use pointer;
    but if name has been changed, cannot use name so should use pointer (if notes dialog was used)
    - see updateObjectChoicesWithPointer }
  savedObjectName := '';
  if (objectChoice.items.count > 0) and (objectChoice.itemIndex >= 0) then
    savedObjectName := objectChoice.items[objectChoice.itemIndex];
  objectChoice.clear;
  Domain.loadObjectNamesIntoTStrings(objectChoice.items);
  if savedObjectName <> '' then
    objectChoice.itemIndex := objectChoice.items.indexOf(savedObjectName)
  else if objectChoice.items.count > 0 then
    objectChoice.itemIndex := 0;
  self.ObjectChoiceChange(self);
  end;

procedure TBrowserForm.updateObjectChoicesWithPointer;
  var
    savedObject: TObject;
	begin
  { if name has been changed, cannot use name so should use pointer (if notes dialog was used) }
  savedObject := nil;
  if (objectChoice.items.count > 0) and (objectChoice.itemIndex >= 0) then
    savedObject := objectChoice.items.objects[objectChoice.itemIndex];
  objectChoice.clear;
  Domain.loadObjectNamesIntoTStrings(objectChoice.items);
  if (savedObject <> nil) then
    objectChoice.itemIndex := objectChoice.items.indexOfObject(savedObject)
  else if objectChoice.items.count > 0 then
    objectChoice.itemIndex := 0;
  self.ObjectChoiceChange(self);
  end;

procedure TBrowserForm.updateObjectChoicesForDeletedObject;
  var
    savedItemIndex: longint;
	begin
  { only use if object deleted - go to next in list which will be at same index }
  savedItemIndex := objectChoice.itemIndex;
  objectChoice.clear;
  Domain.loadObjectNamesIntoTStrings(objectChoice.items);
  if (savedItemIndex >= 0) and (savedItemIndex <= objectChoice.items.count - 1) then
    objectChoice.itemIndex := savedItemIndex
  else if objectChoice.items.count > 0 then
    objectChoice.itemIndex := 0;
  self.ObjectChoiceChange(self);
  end;

procedure TBrowserForm.selectTemplate(model: GsModel);
  var index: longint;
  begin
  index := objectChoice.items.indexOfObject(model);
  if (index > 0) and (index < objectChoice.items.count) then
    objectChoice.itemIndex := index;
  end;

procedure TBrowserForm.updateModelList(updateList: TListCollection);
  var
    i: longint;
    component: KfBrowserComponent;
    updateEvent: GsModelUpdateEvent;
    aspect: GsAspect;
    needToUpdateModels, updateDrawingPlant, updatePlant, updateSoilPatch, updateWeather: boolean;
	begin
  if updateList.count <= 0 then exit;
  { first look to see if any whole models need updated - if any do, update all (simpler) }
  needToUpdateModels := false;
  for i := 0 to updateList.count - 1 do
    begin
    updateEvent := nil;
    updateEvent := GsModelUpdateEvent(updateList.items[i]);
    if updateEvent = nil then continue;
    if (updateEvent.model <> nil) and (updateEvent.model.wholeModelUpdateNeeded) then
      begin
      needToUpdateModels := true;
      updateEvent.model.wholeModelUpdateNeeded := false;
      end;
    end;
  if needToUpdateModels then
    begin
    self.updateModelValues;
    exit;
    end;
  { if not updating everything, update based on list }
  { tell all components to look at list to see if they need updating }
  if ControlPanel.ControlCount > 0 then
  	for i := 0 to ControlPanel.ControlCount - 1 do
      if ControlPanel.Controls[i] is KfBrowserComponent then
     		begin
      	component := ControlPanel.Controls[i] as KfBrowserComponent;
      	{ components check if use models and fields in list }
        component.updateFromUpdateEventList(updateList);
      	end;
  { check if any of right-side panels should update }
  updateDrawingPlant := false;
  updatePlant := false;
  updateSoilPatch := false;
  updateWeather := false;
  for i := 0 to updateList.count - 1 do
    begin
    updateEvent := nil;
    updateEvent := GsModelUpdateEvent(updateList.items[i]);
    if updateEvent = nil then continue;
    if updateEvent.model = nil then continue;
    { must get aspect because want to act differently based on whether it is a plant or drawing plant aspect,
      and the model collected by the update event doesn't know that (since the plant handles transfer for
      the drawing plant). }
    aspect := nil;
    aspect := Domain.aspectManager.aspectForFieldNumber(updateEvent.fieldID);
    if aspect = nil then continue;
    case aspect.objectType of
		  kObjectTypeWeather: updateWeather := true;
		  kObjectTypeSoil: updateSoilPatch := true;
		  kObjectTypePlant: updatePlant := true;
      kObjectTypeDrawingPlant: updateDrawingPlant := true;
      end;
    end;
  { update any right-side panels that need updating }
  if plantDrawImage.visible and updateDrawingPlant then self.plantDrawClick(self);
  if plantStatisticsPanel.visible and updatePlant then plantStatisticsPanel.updateModelValues;
  if soilDrawPanel.visible and updateSoilPatch then soilDrawPanel.updateModelValues;
  if soilTextureTriangleDrawPanel.visible and updateSoilPatch then { tracks specific fields }
    soilTextureTriangleDrawPanel.updateFromUpdateEventList(updateList);
  if weatherDrawPanel.visible and updateWeather then weatherDrawPanel.updateModelValues;
  end;

procedure TBrowserForm.modelDeleted(model: GsModel);
	begin
  if self.selectedModel = model then
    begin
    deletedModel := model;
    objectChoice.items.delete(objectChoice.items.indexOfObject(model));
    self.objectChoice.itemIndex := -1;
    updateComponentModels(true);
    end;
  end;

procedure TBrowserForm.setSelectedModel(model: GsModel);
	begin
  if model = nil then
    objectChoice.itemIndex := -1
  else if objectChoice.items.indexOfObject(model) >= 0 then
    begin
    objectChoice.itemIndex := objectChoice.items.indexOfObject(model);
    self.ObjectChoiceChange(self);
    end
  else
    objectChoice.itemIndex := -1;
  end;

procedure TBrowserForm.modelUndeleted(model: GsModel);
	begin
  self.updateObjectChoices;
  if deletedModel = model then
    begin
  	self.setSelectedModel(model);
    end;
  end;

procedure TBrowserForm.GroupChange(Sender: TObject);
  begin
  self.updateComponents;
  self.updateComponentModels(false);
  end;

procedure TBrowserForm.ObjectChoiceChange(Sender: TObject);
  begin
  if self.selectedModel = nil then
    self.caption := 'Browser'
  else
    self.caption := 'Browser on ' + self.selectedModel.getName;
  self.updateComponentModels(true);
  end;

{ ----------------------------------------------------------------------------------------------- MENU ITEMS }
procedure TBrowserForm.MenuLeftSideClick(Sender: TObject);
  begin
  self.showLeftClick(self);
  end;

procedure TBrowserForm.MenuRightSideClick(Sender: TObject);
  begin
  self.showRightClick(self);
  end;

procedure TBrowserForm.MenuBothSidesClick(Sender: TObject);
  begin
  self.showBothClick(self);
  end;

procedure TBrowserForm.MenuChangeFontClick(Sender: TObject);
  begin
  self.chooseFontClick(self);
  end;

procedure TBrowserForm.MenuObjectNotesClick(Sender: TObject);
  begin
  self.notesButtonClick(self);
  end;

procedure TBrowserForm.MenuObjectMakeTemplateClick(Sender: TObject);
  begin
  self.templatesClick(self);
  end;

procedure TBrowserForm.MenuObjectDeleteClick(Sender: TObject);
  begin
  self.deleteObjectButtonClick(self);
  end;

procedure TBrowserForm.MenuGraphAspectClick(Sender: TObject);
  begin
  self.graphClick(self);
  end;

procedure TBrowserForm.MenuExpandAllClick(Sender: TObject);
  begin
  self.expandAllClick(self);
  end;

procedure TBrowserForm.MenuCollapseAllClick(Sender: TObject);
  begin
  self.collapseAllClick(self);
  end;

procedure TBrowserForm.MenuEditGroupsClick(Sender: TObject);
  begin
  self.EditGroupClick(self);
  end;

procedure TBrowserForm.MenuWeatherLineGraphClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(graphWeatherLines, graphWeatherBars);
  self.graphWeatherLinesClick(self);
  end;

procedure TBrowserForm.MenuWeatherBarGraphClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(graphWeatherLines, graphWeatherBars);
  self.graphWeatherBarsClick(self);
  end;

procedure TBrowserForm.MenuWeatherShowMeansClick(Sender: TObject);
  begin
  self.drawWeatherMeans.down := not self.drawWeatherMeans.down;
  self.drawWeatherMeansClick(self);
  end;

procedure TBrowserForm.MenuWeatherRescaleClick(Sender: TObject);
  begin
  self.rescaleWeatherWhenLowClick(self);
  end;

procedure TBrowserForm.MenuSoilShowLayersClick(Sender: TObject);
  begin
  self.drawSoilLines.down := not self.drawSoilLines.down;
  self.drawSoilLinesClick(self);
  end;

procedure TBrowserForm.MenuSoilConnectLayerValuesClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(graphSoilBars, graphSoilLines);
  self.graphSoilLinesClick(self);
  end;

procedure TBrowserForm.MenuSoilRescaleClick(Sender: TObject);
  begin
  self.rescaleSoilWhenLowClick(self);
  end;

procedure TBrowserForm.MenuPlantTurnLeftClick(Sender: TObject);
  begin
  self.plantLeftClick(self);
  end;

procedure TBrowserForm.MenuPlantTurnRightClick(Sender: TObject);
  begin
  self.plantRightClick(self);
  end;

procedure TBrowserForm.MenuPlantZoomInClick(Sender: TObject);
  begin
  self.PlantZoomPlusClick(self);
  end;

procedure TBrowserForm.MenuPlantZoomOutClick(Sender: TObject);
  begin
  self.PlantZoomMinusClick(self);
  end;

procedure TBrowserForm.MenuPlantCenterClick(Sender: TObject);
  begin
  self.plantCenterClick(self);
  end;

procedure TBrowserForm.MenuPlantViewFromTopClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(plantDrawFromTop, plantDrawFromSide);
  self.plantDrawFromTopClick(self);
  end;

procedure TBrowserForm.MenuPlantViewFromSideClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(plantDrawFromTop, plantDrawFromSide);
  self.plantDrawFromSideClick(self);
  end;

procedure TBrowserForm.MenuPlantDragModeClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(plantDragMode, plantHarvestMode);
  self.plantDragModeClick(self);
  end;

procedure TBrowserForm.MenuPlantHarvestModeClick(Sender: TObject);
  begin
  toggleTwoSpeedButtons(plantDragMode, plantHarvestMode);
  self.plantHarvestModeClick(self);
  end;

procedure TBrowserForm.MenuPlantReseedClick(Sender: TObject);
  begin
  self.becomeSeedClick(self);
  end;

procedure TBrowserForm.MenuPlantGrowClick(Sender: TObject);
  begin
  self.growPlantClick(self);
  end;

{ --------------------------------------------------------------------------------------- FUNCTIONS FOR SELECTED OBJECTS }
function TBrowserForm.browserComponentForHandle(handle: HWnd): KfBrowserComponent;
  var
    i: integer;
    control: KfBrowserComponent;
  begin
  result := nil;
  if controlPanel.controlCount > 0 then
    for i := 0 to controlPanel.controlCount - 1 do
      begin
      control := KfBrowserComponent(controlPanel.controls[i]);
      if control.handle = handle then
        begin
        result := control;
        exit;
        end;
      end;
  end;

function TBrowserForm.selectedModel: GsModel;
  begin
  if (objectChoice.items.count <= 0) or (objectChoice.itemIndex < 0) then
    result := nil
  else
  	result := objectChoice.items.objects[objectChoice.itemIndex] as GsModel;
  end;

function TBrowserForm.currentGarden: GsGarden;
  var
    theModel: GsModel;
  begin
  { no matter what is selected, if it is not a template, return the garden }
  theModel := self.selectedModel;
  if (theModel <> nil) and (not theModel.isTemplate) then
    result := Domain.garden
  else
    result := nil;
  end;

function TBrowserForm.isWeatherSelected: boolean;
  var
    theModel: GsModel;
  begin
  theModel := self.selectedModel;
  result := (theModel <> nil) and (theModel is GsWeather);
  end;

function TBrowserForm.currentWeather: GsWeather;
  var
    theModel: GsModel;
  begin
  result := nil;
  theModel := self.selectedModel;
  if theModel = nil then exit;
  { no matter what is selected, if it is not a template, return the weather }
  if (not theModel.isTemplate) then
    result := Domain.garden.weather
  { but if the selected model is a climate, return that }
  else if theModel is GsWeather then
    result := theModel as GsWeather;
  end;

function TBrowserForm.isSoilPatchSelected: boolean;
  var
    theModel: GsModel;
  begin
  theModel := self.selectedModel;
  result := (theModel <> nil) and (theModel is GsSoilPatch);
  end;

function TBrowserForm.currentSoilPatch: GsSoilPatch;
  var
    theModel: GsModel;
  begin
  result := nil;
  theModel := self.selectedModel;
  if (theModel = nil) then exit;
  { a soil patch can be selected by choosing the patch or a plant in it }
  if (theModel is GsSoilPatch) then
    result := theModel as GsSoilPatch
  else if (theModel is GsPlant) and (not theModel.isTemplate) then
    result := (theModel as GsPlant).soil;
  end;

function TBrowserForm.isPlantSelected: boolean;
  var
    theModel: GsModel;
  begin
  theModel := self.selectedModel;
  result := (theModel <> nil) and (theModel is GsPlant);
  end;

function TBrowserForm.currentPlant: GsPlant;
  var
    theModel: GsModel;
  begin
  result := nil;
  theModel := self.selectedModel;
  if theModel = nil then exit;
  if theModel is GsPlant then
    result := theModel as GsPlant;
  end;

function TBrowserForm.isBagSelected: boolean;
  var
    theModel: GsModel;
  begin
  theModel := self.selectedModel;
  result := (theModel <> nil) and (theModel is GsBag);
  end;

function TBrowserForm.currentBag: GsBag;
  var
    theModel: GsModel;
  begin
  result := nil;
  theModel := self.selectedModel;
  if theModel = nil then exit;
  if theModel is GsBag then
    result := theModel as GsBag;
  end;

{ ----------------------------------------------------------------------------------------------- SPLITTER/DRAGGING }
procedure TBrowserForm.splitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.splitterDragging := true;
  self.splitterStartPos := x;
  self.splitterLastDrawPos := -1;
  self.splitterNeedToRedraw := true;
  inherited mouseDown(button, shift, x, y);
  end;

procedure TBrowserForm.splitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  begin
  if self.splitterDragging and
    (splitter.left + x >= kSplitterDragMinPixels)
      and (splitter.left + x < self.clientWidth - kSplitterDragMinPixels) then
      begin
      self.undrawSplitterLine;
      self.splitterLastDrawPos := self.drawSplitterLine(x);
      end;
  inherited mouseMove(shift, x, y);
  end;

procedure TBrowserForm.splitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if self.splitterDragging then
    begin
    self.undrawSplitterLine;
    splitter.left := splitter.left - (splitterStartPos - x);
  	self.resizePanelsToSplitter(objectChoice.top + objectChoice.height + kBetweenGap);
    self.resizeLeftPanelContents;
    self.resizeRightPanelContents;
    self.splitterDragging := false;
    end;
  inherited mouseUp(button, shift, x, y);
  end;

function TBrowserForm.drawSplitterLine(pos: integer): integer;
  var
    theDC: HDC;
  begin
  theDC := getDC(0);
  result := self.clientOrigin.x + splitter.left + pos + 2;
  patBlt(theDC, result, self.clientOrigin.y + splitter.top, 1, splitter.height, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := true;
  end;

procedure TBrowserForm.undrawSplitterLine;
  var theDC: HDC;
  begin
  if not self.splitterNeedToRedraw then exit;
  theDC := getDC(0);
  patBlt(theDC, self.splitterLastDrawPos,
    self.clientOrigin.y + splitter.top, 1, splitter.height, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := false;
  end;

procedure TBrowserForm.ControlPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.dragComponentY := y;
  { start dragging }
  if not controlPanel.dragging then
   controlPanel.beginDrag(true)
  else
   controlPanel.endDrag(true);
  end;

procedure TBrowserForm.ControlPanelDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
  begin
  accept := (source = controlPanel);
  end;

procedure TBrowserForm.ControlPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
  var
    currentGroup: GsGroup;
    selectedIndex, draggedToIndex: integer;
    componentStartedAt, componentDraggedTo, newComponent: KfBrowserComponent;
    i: integer;
  begin
  { y given to this from browser component is from top of control panel, not from top of component }
  if group.itemIndex >= 0 then
    currentGroup := group.items.objects[group.itemIndex] as GsGroup
  else
    exit;
  componentStartedAt := self.browserComponentForControlPanelY(self.dragComponentY);
  { do these two things in every case }
  self.dragComponentY := 0;
  controlPanel.endDrag(false);
  if componentStartedAt = nil then exit;
  { need to force mouse up event onto browser component because for some reason drag-drop operation eats
    mouse up event; browser components need the mouse up event to work correctly
    since y has been amended to be relative to the controlPanel top, you need to recalculate the y
    from the top of the browser component }
  componentStartedAt.forceMouseUpAfterDrag(x, y - componentStartedAt.top);
  selectedIndex := currentGroup.groupItems.indexOf(componentStartedAt.groupItem);
  if selectedIndex < 0 then exit;
  componentDraggedTo := self.browserComponentForControlPanelY(y);
  if componentDraggedTo = nil then exit;
  draggedToIndex := currentGroup.groupItems.indexOf(componentDraggedTo.groupItem);
  if draggedToIndex < 0 then exit;
  if (selectedIndex <> draggedToIndex) then
    begin
    { moving the selectedIndex to the position of the draggedToIndex puts the dragged component right on top
      of the dragged to one, moving that one down (or up if it is the last) }
    currentGroup.groupItems.move(selectedIndex, draggedToIndex);
    groupsFileMightHaveChanged := true;
    self.updateComponentOrder;
    { retain focus on selected component, even though it has moved; getting a new pointer is probably not
      necessary since the updateComponentOrder function is used instead of updateComponents, but am leaving this
      anyway as it is safer }
    newComponent := KfBrowserComponent(controlPanel.controls[draggedToIndex]);
    if newComponent <> nil then newComponent.setFocus;
    end;
  end;

function TBrowserForm.browserComponentForControlPanelY(y: integer): KfBrowserComponent;
  var
    i: longint;
    control: KfBrowserComponent;
  begin
  result := nil;
  if controlPanel.controlCount > 0 then
    for i := 0 to controlPanel.controlCount - 1 do
      begin
      control := KfBrowserComponent(controlPanel.controls[i]);
      if (control.top < y) and (control.top + control.height >= y) then
        begin
        result := control;
        exit;
        end;
      end;
  end;

procedure TBrowserForm.updateComponentOrder;
  var
	  component: KfBrowserComponent;
	  currentGroup: GsGroup;
    groupItem: GsGroupItem;
	  i, j: integer;
    savedComponents: TList;
  begin
  { respond to a change in the order of group items in the group created by dragging a component.
    note that components are not deleted here, just removed from the controls and put back again.
    this messy thing must be done because the controls array under the panel is private, so you can't just
    change the order - you must take them all out and put them back again in the new order. }
  if controlPanel.controlCount <= 0 then exit;
  if group.items.count <= 0 then exit;
  if group.itemIndex < 0 then exit;
  currentGroup := group.items.objects[group.itemIndex] as GsGroup;
  if currentGroup = nil then exit;
  if currentGroup.groupItems.count < 0 then exit;
  savedComponents := TList.create;
  { copy component pointers to list }
  for i := 0 to controlPanel.controlCount - 1 do
    savedComponents.add(KfBrowserComponent(controlPanel.controls[i]));
  { clear out control panel controls list }
  while controlPanel.controlCount > 0 do
    begin
    component := KfBrowserComponent(controlPanel.controls[0]);
    controlPanel.removeControl(component); { don't free component - just forget about }
    end;
  if savedComponents.count <= 0 then exit;
  { add components back to control panel controls array in new group item order (in group) }
  for i := 0 to currentGroup.groupItems.count - 1 do
    begin
    groupItem := currentGroup.groupItemForIndex(i);
    for j := 0 to savedComponents.count - 1 do
      begin
      component := KfBrowserComponent(savedComponents.items[j]);
      if component.groupItem = nil then
        raise Exception.create('Problem: component has nil group item');
      if component.groupItem = groupItem then
        begin
        controlPanel.insertControl(component);
        break;
        end;
      end;
    end;
  savedComponents.free;
  self.repositionComponents;
  end;

procedure TBrowserForm.ControlPanelEndDrag(Sender, Target: TObject; X,
  Y: Integer);
  begin
  { this does dragging to the graph window list box only }
  if (target <> nil) and (Target is TListBox) and ((target as TListBox).name = 'loggedVars') then
    begin
    self.graphClick(self);
    exit;
    end;
  end;

{ ----------------------------------------------------------------------------------------------- RESIZING }
procedure TBrowserForm.FormResize(Sender: TObject);
  var
    moveComboBox: boolean;
    newTop: integer;
  begin
  if Application.terminated then exit;
  { safety check for WIN32 in case not fully created yet }
  if self.WindowState = wsMinimized then exit;
  if soilDrawPanel = nil then exit;
  splitter.visible := false;
  leftPanel.visible := false;
  rightPanel.visible := false;
  moveComboBox := (self.clientWidth <= buttonsPanel.width + kMinObjectComboBoxWidth + kBetweenGap * 2);
  with buttonsPanel do setBounds(kBetweenGap, kBetweenGap, width, height);
  with objectChoice do
    begin
    if moveComboBox then
      setBounds(kBetweenGap, buttonsPanel.top + buttonsPanel.height + kBetweenGap,
          self.clientWidth - kBetweenGap * 2, height)
    else
      setBounds(buttonsPanel.left + buttonsPanel.width + kBetweenGap, kBetweenGap,
          self.clientWidth - buttonsPanel.width - kBetweenGap * 3, height);
    end;
  newTop := objectChoice.top + objectChoice.height + kBetweenGap;
  case currentShowMode of
    kBrowserShowLeft:
      begin
      with leftPanel do setBounds(0, newTop, self.clientWidth, self.clientHeight - newTop);
  		self.resizeLeftPanelContents;
      leftPanel.visible := true;
      end;
    kBrowserShowRight:
      begin
      with rightPanel do setBounds(0, newTop, self.clientWidth, self.clientHeight - newTop);
  		self.resizeRightPanelContents;
      rightPanel.visible := true;
      end;
    kBrowserShowBoth:
      begin
      splitter.left := round(1.0 * splitter.left * clientWidth / lastWidth);
      if splitter.left >= self.clientWidth then splitter.left := self.clientWidth - 1;
      if splitter.left < 0 then splitter.left := 0;
      self.resizePanelsToSplitter(newTop);
  		self.resizeLeftPanelContents;
  		self.resizeRightPanelContents;
      leftPanel.visible := true;
      splitter.visible := true;
      rightPanel.visible := true;
  		lastWidth := self.clientWidth;
      end;
    end;
  end;

procedure TBrowserForm.resizePanelsToSplitter(topOfPanels: integer);
  var newWidth: longint;
	begin
  with splitter do setBounds(left, topOfPanels, width, self.clientHeight - topOfPanels);
  with leftPanel do setBounds(0, topOfPanels, splitter.left, self.clientHeight - topOfPanels);
  newWidth := self.clientWidth - splitter.width - splitter.left;
  with rightPanel do setBounds(self.clientWidth - newWidth, topOfPanels, newWidth, self.clientHeight - topOfPanels);
  end;

procedure TBrowserForm.resizeLeftPanelContents;
  var
    moveButtons: boolean;
    newTop: longint;
  begin
  if self.currentShowMode = kBrowserShowRight then exit;
  moveButtons := (leftPanel.width <= kBitButtonWidth * 3 + kMinGroupComboBoxWidth + kBetweenGap * 3);
  editGroup.left := leftPanel.width - kBitButtonWidth * 3 - kBetweenGap;
  expandAll.left := editGroup.left + editGroup.width;
  collapseAll.left := expandAll.left + expandAll.width;
  with group do setBounds(kBetweenGap, 0, width, height);
  if moveButtons then
    begin
    editGroup.top := group.top + group.height + kBetweenGap;
    expandAll.top := editGroup.top;
    collapseAll.top := editGroup.top;
    group.width := leftPanel.width - kBetweenGap * 2;
    end
  else
    begin
    editGroup.top := 0;
    expandAll.top := 0;
    collapseAll.top := 0;
    group.width := editGroup.left - kBetweenGap * 2;
    end;
  if moveButtons then
    newTop := editGroup.top + editGroup.height + kBetweenGap
  else
    newTop := group.top + group.height + kBetweenGap;
  with controlScrollBox do setBounds(0, newTop, leftPanel.width, leftPanel.height - newTop);
  with controlPanel do setBounds(0, 0, width, height);
  self.repositionComponents;
  end;

procedure TBrowserForm.resizeRightPanelContents;
  begin
  if self.currentShowMode = kBrowserShowLeft then exit;
  self.updatePlantDraw(self.isPlantSelected);
  self.updateSoilDraw(self.isSoilPatchSelected);
  self.updateWeatherDraw(self.isWeatherSelected and not self.selectedModel.isTemplate);
  end;

procedure TBrowserForm.resizeSoilDrawPanel;
  var
    newTop: longint;
  begin
  newTop := soilDrawControls.top + soilDrawControls.height;
  with soilDrawPanel do setBounds(0, newTop, rightPanel.width, rightPanel.height - newTop);
  soilDrawPanel.resizeElements;
  end;

procedure TBrowserForm.resizeSoilTextureTriangleDrawPanel;
  var
    newTop: longint;
  begin
  newTop := soilDrawControls.top + soilDrawControls.height;
  with soilTextureTriangleDrawPanel do setBounds(0, newTop, rightPanel.width, rightPanel.height - newTop);
  soilTextureTriangleDrawPanel.resizeElements;
  end;

procedure TBrowserForm.resizeWeatherDrawPanel;
  var
    newTop: longint;
  begin
  newTop := weatherDrawControls.top + weatherDrawControls.height;
  with weatherDrawPanel do setBounds(0, newTop, rightPanel.width, rightPanel.height - newTop);
  weatherDrawPanel.resizeElements;
  end;

procedure TBrowserForm.resizePlantStatisticsPanel;
  var
    newTop, newHeight: longint;
  begin
  newTop := plantDrawControls.top + plantDrawControls.height;
  { plantHarvestPanel may not have been created yet }
  if plantHarvestPanel = nil then
    newHeight := rightPanel.height - newTop - 34
  else
    newHeight := rightPanel.height - newTop - plantHarvestPanel.height;
  with plantStatisticsPanel do setBounds(0, newTop, rightPanel.width, newHeight);
  plantStatisticsPanel.resizeElements;
  end;

procedure TBrowserForm.resizePlantHarvestPanel;
  begin
  plantHarvestPanel.clientHeight := 32;
  plantHarvestPanel.top := rightPanel.height - plantHarvestPanel.height;
  plantHarvestPanel.left := 0;
  plantHarvestPanel.width := rightPanel.width;
  plantHarvestPanel.doResize;
  end;

procedure TBrowserForm.plantDrawControlsResize(Sender: TObject);
  var
    newButtonTop, leftPos, maxWidth, newHeight: integer;
    wrapping: boolean;
  begin
  maxWidth := kBitButtonWidth * 11 + kBetweenGap * 3 + kBetweenButtonGroupsGap * 3 + kMinPlantDrawDisplayComboBoxWidth;
  if plantDrawControls.width < maxWidth then
    begin
    wrapping := true;
    newButtonTop := plantCenter.top + plantCenter.height + kBetweenGap;
    end
  else
    begin
    wrapping := false;
    newButtonTop := 0;
    end;
  with plantDisplayMode do setBounds(kBetweenGap, 0, width, height);
  leftPos := plantDisplayMode.left + plantDisplayMode.width + kBetweenGap;
  with plantLeft do setBounds(leftPos, 0, width, height);
  inc(leftPos, kBitButtonWidth);
  with plantRight do setBounds(leftPos, 0, width, height);
  inc(leftPos, kBitButtonWidth);
  with plantZoomMinus do setBounds(leftPos, 0, width, height);
  inc(leftPos, kBitButtonWidth);
  with plantZoomPlus do setBounds(leftPos, 0, width, height);
  inc(leftPos, kBitButtonWidth);
  with plantCenter do setBounds(leftPos, 0, width, height);
  if wrapping then
    leftPos := kBetweenGap
  else
    inc(leftPos, kBitButtonWidth + kBetweenButtonGroupsGap);
  with plantDrawFromSide do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with plantDrawFromTop do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth + kBetweenButtonGroupsGap);
  with plantDragMode do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with plantHarvestMode do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth + kBetweenButtonGroupsGap);
  with growPlant do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with becomeSeed do setBounds(leftPos, newButtonTop, width, height);
  newHeight := plantDrawFromSide.top + plantDrawFromSide.height + kBetweenGap;
  { this stops it from recursing }
  if newHeight <> plantDrawControls.height then
    plantDrawControls.height := newHeight;          
  end;

procedure TBrowserForm.soilDrawControlsResize(Sender: TObject);
  var
    newButtonTop, leftPos: integer;
    wrapping: boolean;
  begin
  if soilDrawControls.width < kMinSoilDrawDisplayComboBoxWidth
    + kBitButtonWidth * 4 + kBetweenButtonGroupsGap + kBetweenGap * 3 then
    begin
    wrapping := true;
    newButtonTop := soilDrawDisplayMode.height + kBetweenGap;
    end
  else
    begin
    wrapping := false;
    newButtonTop := 0;
    end;
  if wrapping then
    leftPos := kBetweenGap
  else
    leftPos := soilDrawControls.width - kBetweenGap - kBitButtonWidth * 4 - kBetweenButtonGroupsGap;
  with graphSoilLines do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with graphSoilBars do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth + kBetweenButtonGroupsGap);
  with drawSoilLines do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with rescaleSoilWhenLow do setBounds(leftPos, newButtonTop, width, height);
  with soilDrawDisplayMode do
    if wrapping then
      setBounds(kBetweenGap, 0, soilDrawControls.width - kBetweenGap * 2, height)
    else
      setBounds(kBetweenGap, 0, graphSoilLines.left - kBetweenGap * 2, height);
  soilDrawControls.height := graphSoilLines.top + graphSoilLines.height + kBetweenGap;
  self.resizeSoilDrawPanel; { must do this explicitly because it is my own component }
  self.resizeSoilTextureTriangleDrawPanel;
  end;

procedure TBrowserForm.weatherDrawControlsResize(Sender: TObject);
  var
    newButtonTop, leftPos: integer;
    wrapping: boolean;
  begin
  if weatherDrawControls.width < kMinSoilDrawDisplayComboBoxWidth
    + kBitButtonWidth * 4 + kBetweenButtonGroupsGap + kBetweenGap * 3 then
    begin
    wrapping := true;
    newButtonTop := weatherDrawDisplayMode.height + kBetweenGap;
    end
  else
    begin
    wrapping := false;
    newButtonTop := 0;
    end;
  if wrapping then
    leftPos := kBetweenGap
  else
    leftPos := weatherDrawControls.width - kBetweenGap - kBitButtonWidth * 4 - kBetweenButtonGroupsGap;
  with graphWeatherLines do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with graphWeatherBars do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth + kBetweenButtonGroupsGap);
  with drawWeatherMeans do setBounds(leftPos, newButtonTop, width, height);
  inc(leftPos, kBitButtonWidth);
  with rescaleWeatherWhenLow do setBounds(leftPos, newButtonTop, width, height);
  with weatherDrawDisplayMode do
    if wrapping then
      setBounds(kBetweenGap, 0, weatherDrawControls.width - kBetweenGap * 2, height)
    else
      setBounds(kBetweenGap, 0, graphWeatherLines.left - kBetweenGap * 2, height);
  weatherDrawControls.height := graphWeatherLines.top + graphWeatherLines.height + kBetweenGap;
  self.resizeWeatherDrawPanel; { must do this explicitly because it is my own component }
  end;

procedure TBrowserForm.resizePlantDrawBitmap;
  var
    newTop, newHeight: longint;
  begin
  { PDF CFK FIX - needs more work }
  try
  newTop := plantDrawControls.top + plantDrawControls.height;
  newHeight := rightPanel.height - newTop - plantHarvestPanel.height;
  with plantDrawImage do setBounds(0, newTop, rightPanel.width, newHeight);
  with plantDrawImage.picture.bitmap do
    begin
    height := plantDrawImage.height;
    width := plantDrawImage.width;
    end;
  except
  ShowMessage('Not enough memory to resize the plant bitmap');
  end;
  end;

procedure TBrowserForm.resetPlantDrawBitmap;
  begin
  { PDF CFK FIX - needs more work }
  try
  with plantDrawImage.picture.bitmap do
    begin
    height := 0;
    width := 0;
    end;
  except
  ShowMessage('Not enough memory to reset the plant bitmap');
  end;
  end;

procedure TBrowserForm.repositionComponents;
  var
    i, lastPos, totalMaxWidth, componentMaxWidth, componentMinWidth, requestedWidth: integer;
    windowLock: boolean;
    component: KfBrowserComponent;
    oldPosition: integer;
  begin
  if self.currentShowMode = kBrowserShowRight then exit;
  if controlPanel.controlCount > 0 then
    begin
    totalMaxWidth := 0;
    lastPos := 0;
    oldPosition := ControlScrollBox.VertScrollBar.position;
    ControlScrollBox.VertScrollBar.position := 0;
    windowLock := lockWindowUpdate(self.handle);
    controlPanel.height := controlScrollBox.height + 10;
    controlPanel.top := 0;
    requestedWidth := controlScrollBox.clientWidth;
    for i := 0 to controlPanel.controlCount - 1 do
      begin
      component := KfBrowserComponent(controlPanel.controls[i]);
      component.left := 0;
      component.top := lastPos;
      component.calculateTextDimensions;
      componentMaxWidth := component.maxWidth;
      componentMinWidth := component.minWidth(requestedWidth);
      if componentMaxWidth <= requestedWidth then
        component.width := componentMaxWidth
      else
        begin
        if componentMinWidth = -1 then
          component.width := requestedWidth
        else
          component.width := componentMinWidth;
        end;
      component.calculateHeight;
      lastPos := lastPos + component.height;
      if component.width > totalMaxWidth then totalMaxWidth := component.width;
      end;
    controlPanel.height := lastPos;
    controlPanel.width := totalMaxWidth;
    lastPos := 0;
    for i := 0 to controlPanel.controlCount - 1 do
      begin
      component := KfBrowserComponent(controlPanel.controls[i]);
      component.top := lastPos;
      component.width := totalMaxWidth;
      component.calculateHeight;
      component.resizeElements;
      lastPos := lastPos + component.height;
      end;
    ControlScrollBox.VertScrollBar.position := oldPosition;
    if windowLock then lockWindowUpdate(0);
    end;
  end;

{ ----------------------------------------------------------------------------------------------- PALETTE }
function TBrowserForm.GetPalette: HPALETTE;
  begin
  if Application.terminated or (Domain = nil) or (not Domain.paletteBitmapLoaded) then
    result := inherited GetPalette
  else
  	result := Domain.paletteBitmap.Palette
  end;

{overriden because paint box does not have palette and will not update correctly}
{makes window take first priority for palettes}
function TBrowserForm.PaletteChanged(Foreground: Boolean): Boolean;
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
    if (RealizePalette(DC) <> 0) and not Application.terminated then
      begin
    	self.invalidate;
      GardenForm.gardenPaintBox.invalidate;
      end;
    SelectPalette(DC, OldPalette, True);
    RealizePalette(DC);
    ReleaseDC(WindowHandle, DC);
  	end;
  Result := inherited PaletteChanged(Foreground);
	end;

procedure TBrowserForm.MenuShowHelpClick(Sender: TObject);
  begin
  self.helpButtonClick(self);
  end;

end.

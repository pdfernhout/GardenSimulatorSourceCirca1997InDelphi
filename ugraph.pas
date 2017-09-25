unit Ugraph;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugraph: Graph form. Simulation aspects are graphed using the browser, placed
under the graph in a legend, drawn on the graph. The list box below the graph
has logged var objects (ulogvar) that contain the information for each graphed
aspect. The graph is drawing using a special TImage descendant (ugrafcom) that
deals with the graphing and the special date-related data. User can click on
different columns of the list box (owner-draw) to change different things
about the graphed aspects. Each data point has a date, and the user can
select what dates to look at.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  Buttons, UGrped, ExtCtrls, uaspects, umodel, ugrafcom,
  ugroups, uestruct, udate, ulogvar, ucollect, ufiler, ugsform;

const
  kSectionPlot = 0;
  kSectionLine = 1;
  kSectionAxis = 2;
  kSectionName = 3;
  kSectionUnit = 4;
  kSectionScale = 5;
  kSectionModel = 6;
  kSectionList = 7;

type
  TGraphForm = class(GsForm)
    checkBitmap: TImage;
    FontDialog1: TFontDialog;
    topPanel: TPanel;
    timeControlsPanel: TPanel;
    showLabel: TLabel;
    saveLabel: TLabel;
    clearGraph: TButton;
    timeUnitsToShow: TComboBox;
    timeUnitsToSave: TComboBox;
    graphPanel: TPanel;
    backOneYear: TBitBtn;
    backOneMonth: TBitBtn;
    forwardOneMonth: TBitBtn;
    forwardOneYear: TBitBtn;
    splitter: TPanel;
    bottomPanel: TPanel;
    header: THeader;
    loggedVars: TListBox;
    buttonsPanel: TPanel;
    help: TBitBtn;
    chooseFont: TBitBtn;
    graphOptions: TBitBtn;
    logGroupBox: TPanel;
    logStartOrStop: TBitBtn;
    logFileName: TEdit;
    logFileNameBrowse: TButton;
    logFileLabel: TLabel;
    toDataStart: TBitBtn;
    toDataEnd: TBitBtn;
    moveLoggedVarDown: TBitBtn;
    moveLoggedVarUp: TBitBtn;
    deleteLoggedVar: TBitBtn;
    timeSeriesGraph: TSpeedButton;
    xyGraph: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure loggedVarsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure plotOptionsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure clearGraphClick(Sender: TObject);
    procedure loggedVarsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure headerSized(Sender: TObject; ASection, AWidth: Integer);
    procedure bringUpBrowserClick(Sender: TObject);
    procedure chooseFontClick(Sender: TObject);
    procedure timeUnitsToSaveChange(Sender: TObject);
    procedure timeUnitsToShowChange(Sender: TObject);
    procedure backOneYearClick(Sender: TObject);
    procedure backOneMonthClick(Sender: TObject);
    procedure forwardOneMonthClick(Sender: TObject);
    procedure forwardOneYearClick(Sender: TObject);
    procedure graphOptionsClick(Sender: TObject);
    procedure splitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure splitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure splitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure toDataStartClick(Sender: TObject);
    procedure toDataEndClick(Sender: TObject);
    procedure helpClick(Sender: TObject);
    procedure loggedVarsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure loggedVarsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure loggedVarsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure moveLoggedVarDownClick(Sender: TObject);
    procedure moveLoggedVarUpClick(Sender: TObject);
    procedure deleteLoggedVarClick(Sender: TObject);
    procedure timeSeriesGraphClick(Sender: TObject);
    procedure xyGraphClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    graph: GsGraph;
    startDate: GsDate;
    monthsToShow, monthsToSave: longint;
    loggingToFile: boolean;
    selectedHeaderSection: integer;
    deletedLoggedVarList: TListCollection;
    splitterDragging : boolean;
    splitterNeedToRedraw : boolean;
    splitterStartPos : integer;
    splitterLastDrawPos : integer;
    lastHeight: integer;
    correlateFlag: boolean;
    graphHasBeenDrawnOnce: boolean;
    dragItemsY: longint;
    loggedVarsWereJustStreamedIn: boolean;
    function drawSplitterLine(pos: integer): integer;
    procedure undrawSplitterLine;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure clearLoggedVars;
    procedure updateLog;
		procedure updateGraph;
    procedure drawGraph;
		procedure updateGraphWithTracking;
    procedure removeSelectedPlotAspect;
    procedure removeLoggedVarFromList(anIndex: longint);
    procedure addLoggedVarToList(loggedVar: GsLoggedVar; anIndex: longint);
    procedure addPlotAspect(model: GsModel; aspect: GsAspect; groupItem: GsGroupItem);
    procedure autoScaleRangeForAxis(axis: integer; var start, stop: single);
    procedure sizeTopAndBottomPanelsToSplitter;
    procedure resizeTopPanelContents;
    procedure resizeBottomPanelContents;
    procedure resizeTimeControlsPanel;
    procedure resizeLogGroupBox;
    procedure drawTextInLoggedVars(text: string; drawRect: TRect; isSelected: boolean);
    procedure drawLineInLoggedVars(loggedVar: GsLoggedVar; drawRect: TRect);
    function nextUnusedLineColor: TColorRef;
    function colorIsInUse(color: TColorRef): boolean;
    function monthsForIndex(index: integer): integer;
    function indexForMonths(months: integer): integer;
    function earliestStartDate: GsDate;
    procedure fillComboBoxWithTimeUnits(comboBox: TComboBox; lastIndex: integer);
    function computeDaysToShow: longint;
		procedure modelDeleted(model: GsModel);
		procedure modelUndeleted(model: GsModel);
		procedure resetDeletedLoggedVarList;
    procedure updateForNewDomain;
    function firstLoggedVarOnAxis(axisToCheck: smallint): GsLoggedVar;
    procedure setAllCurrentUnitsToDefaultUnitsForUnitSystem(showMetricUnits: boolean);
    procedure layerOptions;
    procedure updateAxisTitles;
    procedure generateAxisTitles(var leftTitle: string; var rightTitle: string);
    procedure streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
    procedure respondToSimulationRunningStatus(isRunning: boolean);
    function hintForAspectInListBox(component: TComponent; cursorPos: TPoint): string;
   end;

var GraphForm: TGraphForm;

implementation


uses ugsim, udomain, uegarden, uesoil, ueplant, usupport, uplotopt, ubrowser, ueutils,
  utitlech, ubrowcom, ulayers, ugscom;

{$R *.DFM}

const
  kBetweenGap = 4;
  kNumDefaultColors = 13;
  defaultColors: array[0..kNumDefaultColors-1] of TColorRef =
    (clBlue, clRed, clGreen, clNavy, clMaroon, clOlive, clAqua, clFuchsia, clTeal,
     clPurple, clGray, clLime, clBlack);
  kNumComboBoxItems = 14;
  kSplitterDragMinPixels = 100;

{ ------------------------------------------------------------------------------------------------- create/destroy }
constructor TGraphForm.Create(AOwner: TComponent);
  begin
  inherited create(AOwner);
  graphHasBeenDrawnOnce := false;
  loggedVarsWereJustStreamedIn := false;
  end;

procedure TGraphForm.FormCreate(Sender: TObject);
  begin
  {this method is called before rest of create so need to create graph in here}
  graph := GsGraph.create(self);
  graph.parent := graphPanel;
  self.fillComboBoxWithTimeUnits(timeUnitsToSave, kNumComboBoxItems - 1);
  timeUnitsToSave.itemIndex := 4; { 1 year }
  monthsToSave := monthsForIndex(timeUnitsToSave.itemIndex);
  self.fillComboBoxWithTimeUnits(timeUnitsToShow, kNumComboBoxItems - 1);
  timeUnitsToShow.itemIndex := timeUnitsToSave.itemIndex;
  monthsToShow := monthsForIndex(timeUnitsToShow.itemIndex);
  startDate := GsDate_dateForFirstDayOfMonth(Domain.garden.date);
  selectedHeaderSection := kSectionName;
  deletedLoggedVarList := TListCollection.create;
  lastHeight := self.clientHeight;
  correlateFlag := false;
  timeSeriesGraph.down := not correlateFlag;
  end;

destructor TGraphForm.destroy;
	begin
  self.clearLoggedVars;
  deletedLoggedVarList.free;
  deletedLoggedVarList := nil;
  { do not free graph because owner (self) will free it }
  graph := nil;
  inherited destroy;
  end;

{ ------------------------------------------------------------------------------------------------- updating }
procedure TGraphForm.updateForNewDomain;
  begin
  if not loggedVarsWereJustStreamedIn then self.clearLoggedVars;
  loggedVarsWereJustStreamedIn := false;
  { this does not work while streaming because for some reason it can't get at the items in the combo box
    (same as for garden form) }
  timeUnitsToShow.itemIndex := self.indexForMonths(monthsToShow);
  timeUnitsToSave.itemIndex := self.indexForMonths(monthsToSave);
  self.resetDeletedLoggedVarList;
  self.updateGraph;
  end;

procedure TGraphForm.respondToSimulationRunningStatus(isRunning: boolean);
  begin
  { everything on the graph can be enabled during running }
  end;

procedure TGraphForm.modelDeleted(model: GsModel);
  var
  	i: longint;
	  loggedVar: GsLoggedVar;
    index: longint;
    changed: boolean;
	begin
  changed := false;
  if loggedVars.items.count > 0 then
    begin
    {break into two seperate loops so don't modify loggedVars list while traversing it}
    for i:= 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := GsLoggedVar(loggedVars.items.objects[i]);
      if loggedVar.model = model then
        deletedLoggedVarList.add(loggedVar);
      end;
    if deletedLoggedVarList.count > 0 then
    	for i:= 0 to deletedLoggedVarList.count - 1 do
      	begin
      	loggedVar := GsLoggedVar(deletedLoggedVarList.items[i]);
        index := loggedVars.items.indexOfObject(loggedVar);
        if index >= 0 then
          begin
          loggedVars.items.delete(index);
          changed := true;
          end;
      	end;
    end;
  if changed then
    self.updateGraph;
  end;

procedure TGraphForm.modelUndeleted(model: GsModel);
  var
  	i: longint;
	  loggedVar: GsLoggedVar;
    index, savedItemIndex: longint;
    changed: boolean;
	begin
  changed := false;
  savedItemIndex := loggedVars.itemIndex;
  if deletedLoggedVarList.count > 0 then
    begin
    {break into two seperate loops so don't modify loggedVars list while traversing it}
    for i:= 0 to deletedLoggedVarList.count - 1 do
      begin
      loggedVar := GsLoggedVar(deletedLoggedVarList.items[i]);
      if loggedVar.model = model then
        begin
        loggedVars.items.addObject(loggedVar.displayName(kDontShowLayerInfo), loggedVar);
        changed := true;
        end;
      end;
    if loggedVars.items.count > 0 then
    	for i:= 0 to loggedVars.items.count - 1 do
      	begin
      	loggedVar := GsLoggedVar(loggedVars.items.objects[i]);
        index := deletedLoggedVarList.indexOf(loggedVar);
        if index >= 0 then
          deletedLoggedVarList.delete(index);
      	end;
    end;
  if changed then
    begin
    self.updateGraph;
    loggedVars.itemIndex := savedItemIndex;
    end;
  end;

procedure TGraphForm.updateGraphWithTracking;
  var
    endDate: GsDate;
    newStartDate: GsDate;
  begin
  if loggedVars = nil then exit;
  if loggedVars.items.count <= 0 then exit;
  endDate := startDate;
  GsDate_addMonths(endDate, monthsToShow);
  if GsDate_daysBetween(Domain.garden.date, endDate) <= 0 then
    begin
    startDate := GsDate_dateForFirstDayOfMonth(Domain.garden.date);
    GsDate_addMonths(startDate, -monthsToShow + 1);
    end;
  self.updateGraph;
  end;

procedure TGraphForm.updateGraph;
  var startOffset: longint;
	begin
  if loggedVars = nil then exit;
  graph.setStartDateAndDaysToShow(startDate, self.computeDaysToShow);
  self.updateAxisTitles;
  self.drawGraph;
  end;

procedure TGraphForm.updateLog;
	var
  	i: longint;
	  loggedVar: GsLoggedVar;
	begin
  if loggedVars = nil then exit;
  self.resetDeletedLoggedVarList;
  if loggedVars.items.count <= 0 then exit;
  if loggedVars.items.count > 0 then
  	for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      loggedVar.addToLog;
      end;
  end;

{ ------------------------------------------------------------------------------------------------- logged vars }
procedure TGraphForm.addPlotAspect(model: GsModel; aspect: GsAspect; groupItem: GsGroupItem);
  var
    loggedVar: GsLoggedVar;
    i: longint;
  begin
  case aspect.objectType of
    kObjectTypeGarden: model := Domain.garden;
    kObjectTypeWeather: model := Domain.garden.weather;
    kObjectTypeSoil:
      begin
      if model is GsSoilPatch then
        model := model
      else if model is GsPlant then
        model := (model as GsPlant).soil
      else model := nil;
      end;
    kObjectTypePlant, kObjectTypeDrawingPlant:
      begin
      if model is GsPlant then
        model := model
      else model := nil;
      end;
    else
      model := nil;
    end;
    if model <> nil then
     	begin
     	loggedVar := GsLoggedVar.createWithInfo(model, aspect, groupItem, Domain.garden.date, monthsToSave * 31);
      { default color is chosen based on colors already used }
      loggedVar.lineColor := self.nextUnusedLineColor;
  	 	loggedVars.items.addObject(loggedVar.displayName(kDontShowLayerInfo), loggedVar);
      loggedVars.itemIndex := loggedVars.items.count - 1;
      if Domain.menuOptions.bringUpGraphItemOptionsDialogWhenAspectGraphed then self.plotOptionsClick(self);
      self.updateGraph;
		 	end;
  end;

procedure TGraphForm.removeSelectedPlotAspect;
	var
	  loggedVar: GsLoggedVar;
	begin
  if (loggedVars.items.count > 0) and (loggedVars.itemIndex >= 0) then
  	begin
    loggedVar := loggedVars.items.objects[loggedVars.itemIndex] as GsLoggedVar;
    if loggedVar <> nil then
      begin
      if messageDlg('Delete graph aspect: ' + loggedVar.displayName(kDontShowLayerInfo) + '?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        GardenForm.doCommand(GsRemoveGraphLoggedVarCommand.createCommand(loggedVar, loggedVars.itemIndex));
      end;
		end;
	end;

procedure TGraphForm.removeLoggedVarFromList(anIndex: longint);
  begin
  if (anIndex >= 0) and (anIndex <= loggedVars.items.count - 1) then
    loggedVars.items.delete(anIndex);
  { set itemIndex after delete }
  if anIndex <= loggedVars.items.count - 1 then
    loggedVars.itemIndex := anIndex
  else if loggedVars.items.count = 1 then
    loggedVars.itemIndex := 0
  else
    loggedVars.itemIndex := -1;
  self.updateGraph;
  end;

procedure TGraphForm.addLoggedVarToList(loggedVar: GsLoggedVar; anIndex: longint);
  begin
  if loggedVar = nil then exit;
  if (anIndex >= 0) then
    loggedVars.items.insertObject(anIndex, loggedVar.displayName(kDontShowLayerInfo), loggedVar);
  { set itemIndex after addition }
  loggedVars.itemIndex := loggedVars.items.indexOfObject(loggedVar);
  self.updateGraph;
  end;

procedure TGraphForm.resetDeletedLoggedVarList;
  begin
  if deletedLoggedVarList.count <= 0 then exit;
  deletedLoggedVarList.free;
  deletedLoggedVarList := nil;
  deletedLoggedVarList := TListCollection.create;
  end;

procedure TGraphForm.clearLoggedVars;
	var
  	i: longint;
	  loggedVar: GsLoggedVar;
  begin
	if loggedVars.items.count > 0 then
    begin
  	for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      loggedVar.free;
      loggedVar := nil;
      end;
    loggedVars.items.clear;
    end;
  end;

procedure TGraphForm.loggedVarsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
	  loggedVar: GsLoggedVar;
    i, leftPos: integer;
    drawRects: array[kSectionPlot..kSectionList] of TRect;
    selected: boolean;
  begin
  if Application.terminated then exit;
  if (loggedVars.items.count <= 0) or (index < 0) or (index > loggedVars.items.count - 1) then exit;
  selected := (odSelected in state);
  { set up loggedVar pointer }
  loggedVar := loggedVars.items.objects[index] as GsLoggedVar;
  if loggedVar = nil then exit;
  { set up drawing rects using header }
  leftPos := 0;
  for i := kSectionPlot to kSectionList do
    begin
    drawRects[i].top := rect.top;
    drawRects[i].bottom := rect.bottom;
    drawRects[i].left := leftPos;
    drawRects[i].right := leftPos + header.sectionWidth[i];
    leftPos := drawRects[i].right;
    end;
  { fill checkmark and line rects with white, others with clHighlight if selected }
  for i := kSectionPlot to kSectionList do with loggedVars.canvas do
    begin
    brush.style := bsSolid;
    if selected then
      begin
      if (i = kSectionPlot) or (i = kSectionLine) then
        brush.color := clWindow
      else
        brush.color := clHighlight;
      end
    else
      brush.color := clWindow;
    fillRect(drawRects[i]);
    brush.style := bsClear;
    if selected and ((i = kSectionPlot) or (i = kSectionLine)) then drawFocusRect(drawRects[i]);
    end;
  { draw checkmark }
  if loggedVar.plotFlag then
    loggedVars.canvas.draw(
      drawRects[kSectionPlot].left + (drawRects[kSectionPlot].right - drawRects[kSectionPlot].left) div 2
        - checkBitmap.picture.bitmap.width div 2,
      rect.top, checkBitmap.picture.bitmap);
  self.drawTextInLoggedVars('', drawRects[kSectionPlot],  { no string draws only lines }
    selected and (selectedHeaderSection = kSectionPlot));
  { draw line }
  self.drawLineInLoggedVars(loggedVar, drawRects[kSectionLine]);
  self.drawTextInLoggedVars('', drawRects[kSectionLine], { no string draws only lines }
    selected and (selectedHeaderSection = kSectionLine));
  { draw axis field }
  self.drawTextInLoggedVars(loggedVar.axisString, drawRects[kSectionAxis],
    selected and (selectedHeaderSection = kSectionAxis));
  { draw name field }
  self.drawTextInLoggedVars(loggedVar.displayName(kDontShowLayerInfo), drawRects[kSectionName],
    selected and (selectedHeaderSection = kSectionName));
  { draw unit field }
  self.drawTextInLoggedVars(loggedVar.unitString, drawRects[kSectionUnit],
    selected and (selectedHeaderSection = kSectionUnit));
  { draw scale field }
  self.drawTextInLoggedVars(loggedVar.scaleString, drawRects[kSectionScale],
    selected and (selectedHeaderSection = kSectionScale));
  { draw model field }
  self.drawTextInLoggedVars(loggedVar.modelString, drawRects[kSectionModel],
    selected and (selectedHeaderSection = kSectionModel));
  { draw list field }
  self.drawTextInLoggedVars(loggedVar.listString, drawRects[kSectionList],
    selected and (selectedHeaderSection = kSectionList));
  end;

procedure TGraphForm.drawLineInLoggedVars(loggedVar: GsLoggedVar; drawRect: TRect);
  var rectWidth, rectHeight: integer;
  begin
  with loggedVars.canvas do
    begin
    rectWidth := drawRect.right - drawRect.left;
    rectHeight := drawRect.bottom - drawRect.top;
    pen.color := loggedVar.lineColor;
    if loggedVar.drawBars then
      begin
      pen.width := 5;
      pen.style := psSolid;
      moveTo(drawRect.left + rectWidth div 3, drawRect.bottom - 2);
      lineTo(drawRect.left + rectWidth div 3, drawRect.top + rectHeight div 3);
      moveTo(drawRect.left + 2 * rectWidth div 3, drawRect.bottom - 2);
      lineTo(drawRect.left + 2 * rectWidth div 3, drawRect.top + 3 * rectHeight div 4);
      end
    else
      begin
      pen.width := loggedVar.lineWidth;
      pen.style := TPenStyle(loggedVar.lineStyle);
      moveTo(drawRect.left, drawRect.bottom);
      lineTo(drawRect.left + rectWidth div 3, drawRect.top + rectHeight div 3);
      lineTo(drawRect.left + 2 * rectWidth div 3, drawRect.top + 3 * rectHeight div 4);
      lineTo(drawRect.right, drawRect.top);
      end;
    brush.style := bsClear;
    pen.width := 1;
    pen.color := clBtnShadow;
    pen.style := psSolid;
    end;
  end;

procedure TGraphForm.drawTextInLoggedVars(text: string; drawRect: TRect; isSelected: boolean);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with loggedVars.canvas do
    begin
    font := loggedVars.font;
    font.color := clBtnText;
    if length(text) > 0 then
      begin
      strPCopy(cText, text);
      drawRect.left := drawRect.left + 5; { margin for text }
      winprocs.drawText(handle, cText, strLen(cText), drawRect, DT_LEFT);
      drawRect.left := drawRect.left - 5;
      end;
    if (isSelected) then
      begin
      pen.color := clNavy;
      moveTo(drawRect.right-1, drawRect.top+1);
      lineTo(drawRect.left+1, drawRect.top+1);
      lineTo(drawRect.left+1, drawRect.bottom-1);
      lineTo(drawRect.right-1, drawRect.bottom-1);
      lineTo(drawRect.right-1, drawRect.top+1);
      end
    else
      begin
      pen.color := clBtnShadow;
      moveTo(drawRect.right, drawRect.top);
      lineTo(drawRect.right, drawRect.bottom);
      end;
    pen.color := clBtnText;
    end;
  end;

procedure TGraphForm.loggedVarsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  if (key = VK_RETURN) then
    self.plotOptionsClick(self)
  else if (key = VK_DELETE) then
    self.removeSelectedPlotAspect;
  end;

procedure TGraphForm.loggedVarsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	var
	  loggedVar: GsLoggedVar;
    found: boolean;
    i, leftPos, rightPos: integer;
	begin
  if loggedVars.itemIndex < 0 then exit;
  found := false;
  leftPos := 0;
  for i := kSectionPlot to kSectionList do
    begin
    rightPos := leftPos + header.sectionWidth[i];
    if (x > leftPos) and (x <= rightPos) then
      begin
      selectedHeaderSection := i;
      found := true;
      end;
    leftPos := rightPos;
    end;
  loggedVar := loggedVars.items.objects[loggedVars.itemIndex] as GsLoggedVar;
  if found then case selectedHeaderSection of
    kSectionPlot:
      begin
      loggedVar.plotFlag := not loggedVar.plotFlag;
      self.updateGraph;
 	 		loggedVars.invalidate;
      end;
    kSectionAxis:
      begin
      loggedVar.switchAxisToPlotOn;
      self.updateGraph;
  		loggedVars.invalidate;
      end;
    kSectionName:
      { drag }
      begin
      self.dragItemsY := y;
      loggedVars.beginDrag(false); { drag mode should be manual }
      end;
    kSectionUnit:
      begin
      loggedVar.switchToNextUnitInSet(ssShift in shift, ssCtrl in shift);
      self.updateGraph;
  		loggedVars.invalidate;
      end;
    kSectionLine, kSectionScale, kSectionModel:
      self.plotOptionsClick(self);
    kSectionList:
      self.layerOptions;
    end;
	end;

{ dragging in loggedVars - from browser and dragging items up and down }
{ beginDrag is in loggedVarsMouseDown - dragging must be done from NAME area only }
procedure TGraphForm.loggedVarsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
  begin
  accept := (source <> nil) and ((source is TPanel) or (source = sender));
  end;

procedure TGraphForm.loggedVarsEndDrag(Sender, Target: TObject; X, Y: Integer);
  var
    i, rows: smallint;
  begin
  if target = nil then exit;
  if not (sender = target) then exit; { must be dragged to itself }
  { move selected item up or down }
  if self.dragItemsY <> 0 then
    begin
    rows := abs(y - self.dragItemsY) div loggedVars.itemHeight;
    if rows > 0 then
      begin
      if (y - self.dragItemsY) > 0 then { move down }
        for i := 0 to rows - 1 do self.moveLoggedVarDownClick(self)
      else
        for i := 0 to rows - 1 do self.moveLoggedVarUpClick(self);
      end;
    self.dragItemsY := 0;
    end;
  end;

{ ------------------------------------------------------------------------------------------------- streaming/transfer }
var globalTitle: string[kTitleLength];

procedure TGraphForm.streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    tempStringList: TStringList;
    tempSplitterTop: longint;
  begin
  { the cvir passed in is the windowManager cvir, because this object is not a streamable object }
  tempStringList := TStringList.create;
  try
    if filer.isReading then
      begin
      filer.streamStringList(tempStringList);
      self.clearLoggedVars;
      loggedVars.items.assign(tempStringList);
      self.loggedVarsWereJustStreamedIn := true;
      end
    else
      begin
      tempStringList.assign(loggedVars.items);
      filer.streamStringList(tempStringList);
      end;
    filer.streamDate(startDate);
    filer.streamLongint(monthsToShow);
    filer.streamLongint(monthsToSave);
    if filer.isReading then
      begin
      filer.streamShortString(globalTitle);
      graph.setTitle(globalTitle);
      filer.streamShortString(globalTitle);
      graph.scales[kLeftAxis].setTitle(globalTitle);
      filer.streamShortString(globalTitle);
      graph.scales[kRightAxis].setTitle(globalTitle);
      end
    else
      begin
      globalTitle := graph.getTitle;
      filer.streamShortString(globalTitle);
      globalTitle := graph.scales[kLeftAxis].getTitle;
      filer.streamShortString(globalTitle);
      globalTitle := graph.scales[kRightAxis].getTitle;
      filer.streamShortString(globalTitle);
      end;
    filer.streamBoolean(graph.scales[kLeftAxis].autoTitle);
    filer.streamBoolean(graph.scales[kLeftAxis].autoScale);
    filer.streamBoolean(graph.scales[kLeftAxis].showZeroLine);
    filer.streamBoolean(graph.scales[kRightAxis].autoTitle);
    filer.streamBoolean(graph.scales[kRightAxis].autoScale);
    filer.streamBoolean(graph.scales[kRightAxis].showZeroLine);
    StreamFormPositionInfoWithFiler(filer, cvir, self);
    if filer.isReading then
      begin
      filer.streamLongint(tempSplitterTop);
      if (tempSplitterTop <> splitter.top) and (tempSplitterTop >= kSplitterDragMinPixels)
          and (tempSplitterTop < self.clientHeight - kSplitterDragMinPixels) then
        begin
        splitter.top := tempSplitterTop;
        self.formResize(self);
        end;
      end
    else
      begin
      tempSplitterTop := splitter.top;
      filer.streamLongint(tempSplitterTop);
      end;
  finally
    tempStringList.free;
    tempStringList := nil;
  end;
  end;

{ ------------------------------------------------------------------------------------------------- drawing }
function TGraphForm.hintForAspectInListBox(component: TComponent; cursorPos: TPoint): string;
  var
    aspect: GsAspect;
    listBox: TListBox;
    itemIndex: longint;
  begin
  result := '';
  if not (component is TListBox) then exit;
  listBox := component as TListBox;
  if (listBox <> loggedVars) then exit;
  itemIndex := listBox.itemAtPos(cursorPos, true); {true = don't show anything if beyond last item}
  if itemIndex >= 0 then
    begin
    aspect := nil;
    aspect := GsLoggedVar(listBox.items.objects[itemIndex]).aspect;
    if (aspect <> nil) and (aspect.hint <> nil) then
      result := strPas(aspect.hint);
    end;
  end;

procedure TGraphForm.drawGraph;
  begin
  if Application.terminated then exit;
  graph.plotData(loggedVars, correlateFlag);
  end;

procedure TGraphForm.updateAxisTitles;
  var
    leftAxisTitle, rightAxisTitle: string;
  begin
  self.generateAxisTitles(leftAxisTitle, rightAxisTitle);
  if graph.scales[kLeftAxis].autoTitle then
    graph.scales[kLeftAxis].setTitle(leftAxisTitle);
  if graph.scales[kRightAxis].autoTitle then
    graph.scales[kRightAxis].setTitle(rightAxisTitle);
  end;

procedure TGraphForm.generateAxisTitles(var leftTitle: string; var rightTitle: string);
  var
    i: longint;
    loggedVar, firstLeft, firstRight: GsLoggedVar;
    title: string;
  begin
  leftTitle := '';
  rightTitle := '';
  if loggedVars.items.count > 0 then
    begin
    firstLeft := self.firstLoggedVarOnAxis(kLeftAxis);
    firstRight := self.firstLoggedVarOnAxis(kRightAxis);
    for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      if loggedVar = nil then
        raise Exception.create('TGraphForm.updateAxisTitles: nil loggedVar');
      title := trimLeftAndRight(loggedVar.displayName(kDontShowLayerInfo)) + ' (' + loggedVar.unitString + ')';
      if loggedVar = firstLeft then
        leftTitle := title
      else if loggedVar = firstRight then
        rightTitle := title;
      if (leftTitle <> '') and (rightTitle <> '') then break;
      end;
    end;
  end;

function TGraphForm.earliestStartDate: GsDate;
  var i: integer;
  begin
  result := Domain.garden.date;
  if loggedVars.items.count > 0 then
    for i := 0 to loggedVars.items.count - 1 do
      if loggedVars.items.objects[i] <> nil then
        if (loggedVars.items.objects[i] as GsLoggedVar).hasEarlierStartDateThan(result) then
          result := (loggedVars.items.objects[i] as GsLoggedVar).computeStartDate;
  result := GsDate_dateForFirstDayOfMonth(result);
  end;

procedure TGraphForm.setAllCurrentUnitsToDefaultUnitsForUnitSystem(showMetricUnits: boolean);
	var
  	i: longint;
	  loggedVar: GsLoggedVar;
	begin
  if loggedVars = nil then exit;
  if loggedVars.items.count > 0 then
    begin
  	for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      if loggedVar = nil then
        raise Exception.create('TGraphForm.setAllCurrentUnitsToDefaultUnitsForUnitSystem: nil loggedVar');
      if loggedVar.aspect = nil then continue;
      if showMetricUnits then
        loggedVar.currentUnit := loggedVar.aspect.unitDefaultMetric(loggedVar.groupItemDerivedIndex)
      else
        loggedVar.currentUnit := loggedVar.aspect.unitDefaultEnglish(loggedVar.groupItemDerivedIndex);
      end;
    loggedVars.invalidate;
    self.updateGraph;
    end;
  end;

{ ------------------------------------------------------------------------------------------------- buttons etc }
procedure TGraphForm.timeUnitsToSaveChange(Sender: TObject);
  var
    i: integer;
    loggedVar: GsLoggedVar;
    newDate: GsDate;
    newMonthsToSave: longint;
  begin
  newMonthsToSave := monthsForIndex(timeUnitsToSave.itemIndex);
  if newMonthsToSave = monthsToSave then exit;
  monthsToSave := newMonthsToSave;
  if loggedVars.items.count > 0 then
  	for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      loggedVar.changeDaysToLog(monthsToSave * 31, Domain.garden.date);
      end;
  self.updateGraph;
  end;

procedure TGraphForm.timeUnitsToShowChange(Sender: TObject);
  begin
  monthsToShow := monthsForIndex(timeUnitsToShow.itemIndex);
  self.updateGraph;
  end;

procedure TGraphForm.fillComboBoxWithTimeUnits(comboBox: TComboBox; lastIndex: integer);
  var
    i, saveIndex: integer;
  begin
  saveIndex := comboBox.itemIndex;
  comboBox.items.clear;
  if lastIndex >= 0 then
    for i := 0 to lastIndex do
      case i of
        0: comboBox.items.add('1 month');
        1: comboBox.items.add('3 months');
        2: comboBox.items.add('6 months');
        3: comboBox.items.add('9 months');
        4: comboBox.items.add('1 year');
        5..13: comboBox.items.add(intToStr(i - 3) + ' years');
        end;
  comboBox.itemIndex := intMin(saveIndex, comboBox.items.count - 1);
  end;

procedure TGraphForm.backOneYearClick(Sender: TObject);
  begin
  GsDate_addYears(startDate, -1);
  self.updateGraph;
  end;

procedure TGraphForm.backOneMonthClick(Sender: TObject);
  begin
  GsDate_addMonths(startDate, -1);
  self.updateGraph;
  end;

procedure TGraphForm.forwardOneMonthClick(Sender: TObject);
  begin
  GsDate_addMonths(startDate, 1);
  self.updateGraph;
  end;

procedure TGraphForm.forwardOneYearClick(Sender: TObject);
  begin
  GsDate_addYears(startDate, 1);
  self.updateGraph;
  end;

procedure TGraphForm.toDataStartClick(Sender: TObject);
  begin
  self.startDate := self.earliestStartDate;
  self.updateGraph;
  end;

procedure TGraphForm.toDataEndClick(Sender: TObject);
  var
    aDate: GsDate;
  begin
  { get current date for end of graph }
  aDate := Domain.garden.date;
  { advance to first day of next month so this whole month is graphed }
  GsDate_addMonths(aDate, 1);
  aDate := GsDate_dateForFirstDayOfMonth(aDate);
  { calculate start date from new end date using months to show }
  GsDate_addMonths(aDate, -monthsToShow);
  self.startDate := aDate;
  self.updateGraph;
  end;

procedure TGraphForm.plotOptionsClick(Sender: TObject);
	var
    plotOptionsForm: TPlotOptionsForm;
	  loggedVar: GsLoggedVar;
    dialogResult: longint;
	begin
  if loggedVars.itemIndex < 0 then exit;
  loggedVar := loggedVars.items.objects[loggedVars.itemIndex] as GsLoggedVar;
  if loggedVar = nil then exit;
  dialogResult := 0;
  plotOptionsForm := TPlotOptionsForm.create(application);
  if plotOptionsForm = nil then
    raise Exception.create('Could not create plot options window');
  try
    plotOptionsForm.initialize(loggedVar, loggedVar.aspect,
      Domain.menuOptions.bringUpGraphItemOptionsDialogWhenAspectGraphed);
    dialogResult := plotOptionsForm.showModal;
    Domain.menuOptions.bringUpGraphItemOptionsDialogWhenAspectGraphed := plotOptionsForm.showDialogAtChoice.checked;
  finally
    plotOptionsForm.free;
    plotOptionsForm := nil;
  end;
  if dialogResult > 0 then
    begin
  	self.updateGraph;
    loggedVars.invalidate;
		end;
	end;

procedure TGraphForm.layerOptions;
  var
    layersForm: TlayerOptionsForm;
    loggedVar: GsLoggedVar;
    response: longint;
	begin
  if loggedVars.itemIndex < 0 then exit;
  loggedVar := loggedVars.items.objects[loggedVars.itemIndex] as GsLoggedVar;
  if loggedVar = nil then exit;
  if not loggedVar.isLayerArray then exit;
  layersForm := TlayerOptionsForm.create(self);
  if layersForm = nil then
    raise Exception.create('Could not create layer options window');
  try
    layersForm.initialize(loggedVar.displayName(kShowLayerInfo), loggedVar.arrayShowType, loggedVar.arraySelected,
        kLayersFormIsBeingCalledFromGraphForm);
    response := layersForm.showModal;
    if response = mrOK then layersForm.getResult(loggedVar.arrayShowType, loggedVar.arraySelected);
  finally
    layersForm.free;
    layersForm := nil;
  end;
  if response = mrOK then
    begin
  	self.updateGraph;
    loggedVars.invalidate;
		end;
  end;

procedure TGraphForm.graphOptionsClick(Sender: TObject);
	var
    titlesForm: TGraphTitleForm;
    dialogResult: longint;
    saved, start, stop: single;
    leftGeneratedTitle, rightGeneratedTitle: string;
	begin
  dialogResult := 0;
  titlesForm := TGraphTitleForm.create(self);
  if titlesForm = nil then
    raise Exception.create('Could not create graph options window');
  try
    { must give titles form generated titles because if user picks auto after user was selected,
      must have something to put into box }
    self.generateAxisTitles(leftGeneratedTitle, rightGeneratedTitle);
    with titlesForm do
      begin
      graphTitleEdit.text := self.graph.getTitle;
      self.autoScaleRangeForAxis(kLeftAxis, start, stop);
      titlesForm.initializeForAxis(kLeftAxis, self.graph.scales[kLeftAxis], start, stop, leftGeneratedTitle);
      self.autoScaleRangeForAxis(kRightAxis, start, stop);
      titlesForm.initializeForAxis(kRightAxis, self.graph.scales[kRightAxis], start, stop, rightGeneratedTitle);
      end;
    dialogResult := titlesForm.showModal;
    if dialogResult <> idCancel then with titlesForm do
      begin
      self.graph.setTitle(graphTitleEdit.text);
      with self.graph.scales[kLeftAxis] do
        begin
        autoTitle := leftAxisAutoTitle.checked;
        if not autoTitle then setTitle(leftAxisTitleEdit.text);
        autoScale := leftAxisAutoScale.checked;
        showZeroLine := leftAxisShowZero.checked;
        if not autoScale then
          begin
          saved := yStart;
          try yStart := strToFloat(leftAxisMinimum.text); except yStart := saved; end;
          saved := yRange; 
          try yRange := strToFloat(leftAxisMaximum.text) - yStart; except yRange := saved; end;
          end;
        end;
      with self.graph.scales[kRightAxis] do
        begin
        autoTitle := rightAxisAutoTitle.checked;
        if not autoTitle then setTitle(rightAxisTitleEdit.text);
        autoScale := rightAxisAutoScale.checked;
        showZeroLine := rightAxisShowZero.checked;
        if not autoScale then
          begin
          saved := yStart;
          try yStart := strToFloat(rightAxisMinimum.text); except yStart := saved; end;
          saved := yRange; 
          try yRange := strToFloat(rightAxisMaximum.text) - yStart; except yRange := saved; end;
          end;
        end;
      end;
  finally
    titlesForm.free;
    titlesForm := nil;
  end;
  if dialogResult > 0 then self.updateGraph;
	end;

procedure TGraphForm.clearGraphClick(Sender: TObject);
	var
  	i: longint;
	  loggedVar: GsLoggedVar;
	begin
  if loggedVars.items.count > 0 then
  	for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      loggedVar.reset(Domain.garden.date);
      end;
  self.updateGraph;
  end;

procedure TGraphForm.bringUpBrowserClick(Sender: TObject);
  begin
  BrowserForm.show;
  end;

procedure TGraphForm.chooseFontClick(Sender: TObject);
  begin
  FontDialog1.Font := loggedVars.Font;
  if FontDialog1.Execute then
    begin
    loggedVars.Font := FontDialog1.Font;
    header.font := FontDialog1.Font;
    graphPanel.font := FontDialog1.Font;
    self.formResize(self);
    end;
  end;

procedure TGraphForm.helpClick(Sender: TObject);
  begin
  application.helpJump('windows_Graph_window')
  end;

procedure TGraphForm.moveLoggedVarDownClick(Sender: TObject);
  var
    index: longint;
  begin
  if loggedVars.items.count <= 1 then exit;
  index := loggedVars.itemIndex;
  if (index >= 0) and (index <= loggedVars.items.count - 2) then
    begin
    loggedVars.items.move(index, index + 1);
    loggedVars.itemIndex := index + 1;
    self.updateGraph;
    end;
  end;

procedure TGraphForm.moveLoggedVarUpClick(Sender: TObject);
  var
    index: longint;
  begin
  if loggedVars.items.count <= 1 then exit;
  index := loggedVars.itemIndex;
  if (index >= 1) and (index <= loggedVars.items.count - 1) then
    begin
    loggedVars.items.move(index, index - 1);
    loggedVars.itemIndex := index - 1;
    self.updateGraph;
    end;
  end;

procedure TGraphForm.deleteLoggedVarClick(Sender: TObject);
  begin
  self.removeSelectedPlotAspect;
  end;

procedure TGraphForm.timeSeriesGraphClick(Sender: TObject);
  begin
  correlateFlag := false;
  self.updateGraph;
  end;

procedure TGraphForm.xyGraphClick(Sender: TObject);
  begin
  correlateFlag := true;
  self.updateGraph;
  end;

{ ------------------------------------------------------------------------------------------------- utilities}
function TGraphForm.monthsForIndex(index: integer): integer;
  begin
  result := 12;
  case index of
    0: result := 1;
    1: result := 3;
    2: result := 6;
    3: result := 9;
    4..13: result := (index - 3) * 12;
    end;
  end;

function TGraphForm.indexForMonths(months: integer): integer;
  begin
  result := 12;
  case months of
    1: result := 0;
    3: result := 1;
    6: result := 2;
    9: result := 3;
    else
      result := months div 12 + 3;
    end;
  end;

function TGraphForm.nextUnusedLineColor: TColorRef;
  var i: integer;
  begin
  result := defaultColors[kNumDefaultColors-1];
  for i := 0 to kNumDefaultColors - 1 do
    if not self.colorIsInUse(defaultColors[i]) then
      begin
      result := defaultColors[i];
      exit;
      end;
  end;

function TGraphForm.colorIsInUse(color: TColorRef): boolean;
  var i: integer;
  begin
  result := false;
  if loggedVars.items.count > 0 then
    for i := 0 to loggedVars.items.count - 1 do
      if loggedVars.items.objects[i] <> nil then
        if (loggedVars.items.objects[i] as GsLoggedVar).lineColor = color then
          begin
          result := true;
          exit;
          end;
  end;

function TGraphForm.firstLoggedVarOnAxis(axisToCheck: smallint): GsLoggedVar;
  var
    i: longint;
    loggedVar: GsLoggedVar;
  begin
  result := nil;
  if loggedVars.items.count > 0 then
    for i := 0 to loggedVars.items.count - 1 do
      begin
      loggedVar := loggedVars.items.objects[i] as GsLoggedVar;
      if loggedVar.plotFlag and (loggedVar.axisToPlotOn = axisToCheck) then
        begin
        result := loggedVar;
        exit;
        end;
      end;
  end;

procedure TGraphForm.autoScaleRangeForAxis(axis: integer; var start, stop: single);
  var
    saveYStart, saveYRange: single;
    loggedVar: GsLoggedVar;
    i: integer;
  begin
  if (axis < kLeftAxis) or (axis > kRightAxis) then exit;
  with self.graph.scales[axis] do
    begin
    if autoScale then
      begin
      start := yStart;
      stop := yStart + yRange;
      end
    else
      begin
      saveYStart := yStart;
      saveYRange := yRange;
      if loggedVars.items.count > 0 then for i := 0 to loggedVars.items.count - 1 do
        begin
        loggedVar := (loggedVars.items.objects[i] as GsLoggedVar);
        if (loggedVar.plotFlag) and (loggedVar.axisToPlotOn = axis) then
          computeScaleForLoggedVar(loggedVar, 0, self.graph.daysToShow - 1);
          end;
      prettifyBounds(true, 3);
      start := yStart;
      stop := yStart + yRange;
      yStart := saveYStart;
      yRange := saveYRange;
      end;
    end;
  end;

function TGraphForm.computeDaysToShow: longint;
  begin
  result := GsDate_daysInXMonthsForwardFromDate(startDate, monthsToShow) + 1;
  end;

{ ------------------------------------------------------------------------------------------------- resizing }
procedure TGraphForm.splitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.splitterDragging := true;
  self.splitterStartPos := y;
  self.splitterLastDrawPos := -1;
  self.splitterNeedToRedraw := true;
  inherited mouseDown(button, shift, x, y);
  end;

procedure TGraphForm.splitterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  begin
  if self.splitterDragging and
    (splitter.top + y >= kSplitterDragMinPixels) and (splitter.top + y < self.clientHeight - kSplitterDragMinPixels) then
      begin
      self.undrawSplitterLine;
      self.splitterLastDrawPos := self.drawSplitterLine(y);
      end;
  inherited mouseMove(shift, x, y);
  end;

procedure TGraphForm.splitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if self.splitterDragging then
    begin
    self.undrawSplitterLine;
    if (splitter.top + y < kSplitterDragMinPixels) then
      splitter.top := kSplitterDragMinPixels
    else if (splitter.top + y > self.clientHeight - kSplitterDragMinPixels) then
      splitter.top := self.clientHeight - kSplitterDragMinPixels
    else
      splitter.top := splitter.top - (splitterStartPos - y);
    self.formResize(self);
    self.splitterDragging := false;
    end;
  inherited mouseUp(button, shift, x, y);
  end;

procedure TGraphForm.FormResize(Sender: TObject);
  var newSplitterTop:integer;
  begin
  if Application.terminated then exit;
  splitter.visible := false;
  topPanel.visible := false;
  bottomPanel.visible := false;
  with topPanel do setBounds(0, 0, self.clientWidth, height);
  with bottomPanel do setBounds(0, top, self.clientWidth, height);
  { keep splitter at same proportion it was before }
  newSplitterTop := round(1.0 * splitter.top * clientHeight / lastHeight);
  if newSplitterTop >= self.clientHeight then newSplitterTop := self.clientHeight - 1;
  if newSplitterTop < 0 then newSplitterTop := 0;
  with splitter do setBounds(0, newSplitterTop, self.clientWidth, height);
  self.sizeTopAndBottomPanelsToSplitter;
  self.resizeTopPanelContents;
  self.resizeBottomPanelContents;
  splitter.visible := true;
  topPanel.visible := true;
  bottomPanel.visible := true;
  lastHeight := self.clientHeight;
  end;

procedure TGraphForm.sizeTopAndBottomPanelsToSplitter;
  begin
  {topPanel.visible := false;
  bottomPanel.visible := false;}
  topPanel.height := splitter.top;
  bottomPanel.height := self.clientHeight - splitter.top - splitter.height;
  bottomPanel.top := splitter.top + splitter.height;
  {topPanel.visible := true;
  bottomPanel.visible := true;}
  end;

procedure TGraphForm.resizeTopPanelContents;
  begin
  timeControlsPanel.left := 0;
  timeControlsPanel.width := topPanel.width;
  self.resizeTimeControlsPanel;
  timeControlsPanel.top := topPanel.height - timeControlsPanel.height;
  graphPanel.left := 0;
  graphPanel.width := topPanel.width;
  graphPanel.top := 0;
  graphPanel.height := topPanel.height - timeControlsPanel.height;
  if graph <> nil then
    begin
    graph.setBounds(0, 0, graphPanel.width, graphPanel.height);
    graph.picture.bitmap.width := graphPanel.width;
    graph.picture.bitmap.height := graphPanel.height;
    if not graphHasBeenDrawnOnce then
      begin
      self.updateGraph;
      graphHasBeenDrawnOnce := true;
      end
    else
      self.drawGraph;
    end;
  end;

procedure TGraphForm.resizeBottomPanelContents;
  begin
  with header do setBounds(kBetweenGap, kBetweenGap, bottomPanel.width - kBetweenGap * 2, height);
  with buttonsPanel do setBounds(kBetweenGap, bottomPanel.height - buttonsPanel.height - kBetweenGap, width, height);
  if logGroupBox.visible then
    begin
    with logGroupBox do setBounds(bottomPanel.width - logGroupBox.width - kBetweenGap,
        bottomPanel.height - logGroupBox.height - kBetweenGap,
        bottomPanel.width - buttonsPanel.width - kBetweenGap * 3,
        height);
    self.resizeLogGroupBox;
    end;
  with loggedVars do setBounds(kBetweenGap,
      header.top + header.height,
      bottomPanel.width - kBetweenGap * 2,
      bottomPanel.height - buttonsPanel.height - header.height - kBetweenGap * 3);
  end;

const kMinTimeUnitsComboBoxWidth = 80;

procedure TGraphForm.resizeTimeControlsPanel;
  var
    minWidth, newButtonTop: integer;
  begin
  minWidth := saveLabel.width + timeUnitsToSave.width + showLabel.width + timeUnitsToShow.width
    + backOneYear.width * 6 + clearGraph.width + kBetweenGap * 10;
  { combo boxes from left }
  saveLabel.left := kBetweenGap;
  timeUnitsToSave.left := saveLabel.left + saveLabel.width + kBetweenGap;
  showLabel.left := timeUnitsToSave.left + timeUnitsToSave.width + kBetweenGap;
  timeUnitsToShow.left := showLabel.left + showLabel.width + kBetweenGap;
  { buttons from right }
  clearGraph.left := topPanel.width - clearGraph.width - kBetweenGap;
  toDataEnd.left := clearGraph.left - toDataEnd.width - kBetweenGap;
  forwardOneYear.left := toDataEnd.left - forwardOneYear.width;
  forwardOneMonth.left := forwardOneYear.left - forwardOneMonth.width;
  backOneMonth.left := forwardOneMonth.left - backOneMonth.width;
  backOneYear.left := backOneMonth.left - backOneYear.width;
  toDataStart.left := backOneYear.left - toDataStart.width;
  if timeControlsPanel.width < minWidth then
    begin
    newButtonTop := timeUnitsToSave.top + timeUnitsToSave.height + kBetweenGap;
    timeControlsPanel.height := newButtonTop + clearGraph.height + kBetweenGap * 2;
    end
  else
    begin
    newButtonTop := kBetweenGap;
    timeControlsPanel.height := newButtonTop + timeUnitsToSave.height + kBetweenGap;
    end;
  clearGraph.top := newButtonTop;
  toDataEnd.top := newButtonTop;
  forwardOneYear.top := newButtonTop;
  forwardOneMonth.top := newButtonTop;
  backOneMonth.top := newButtonTop;
  backOneYear.top := newButtonTop;
  toDataStart.top := newButtonTop;
  end;

procedure TGraphForm.resizeLogGroupBox;
  begin
  if logGroupBox.width < 120 then
    logGroupBox.visible := false
  else
    begin
    if not logGroupBox.visible then logGroupBox.visible := true;
    logFileLabel.left := kBetweenGap;
    logStartOrStop.left := logFileLabel.left + logFileLabel.width + kBetweenGap;
    logFileNameBrowse.left := logGroupBox.width - logFileNameBrowse.width - kBetweenGap;
    logFileName.left := logStartOrStop.left + logStartOrStop.width + kBetweenGap;
    logFileName.width := logGroupBox.width - logStartOrStop.width - logFileNameBrowse.width - logFileLabel.width
      - kBetweenGap * 5;
    end;
  end;

procedure TGraphForm.headerSized(Sender: TObject; ASection,
  AWidth: Integer);
  begin
  loggedVars.invalidate;
  end;

function TGraphForm.drawSplitterLine(pos: integer): integer;
  var
    theDC: HDC;
  begin
  theDC := getDC(0);
  result := self.clientOrigin.y + splitter.top + pos + 2;
  patBlt(theDC, self.clientOrigin.x + splitter.left, result, splitter.width, 1, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := true;
  end;

procedure TGraphForm.undrawSplitterLine;
  var theDC: HDC;
  begin
  if not self.splitterNeedToRedraw then exit;
  theDC := getDC(0);
  patBlt(theDC, self.clientOrigin.x + splitter.left, self.splitterLastDrawPos, splitter.width, 1, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := false;
  end;

end.

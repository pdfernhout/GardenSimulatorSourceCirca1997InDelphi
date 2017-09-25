unit Ugrafcom;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugrafcom: Graphing component, a subclass of TImage. Handles graphing with data from
logged vars (ulogvar) in graph window (ugraph). Also a graph scale object which keeps
data on graph scales and recalculates scales as needed. The graph has two scale
objects, left and right; vars are graphed on one of the scales. Graph can draw in either
normal time-series mode (where each Y point is graphed against X of the simulation
date) or xy mode (where each Y point is from one var and each X point is from another
var). Graph uses a TBitmap to double-buffer itself. Whole system works with data points
for each var associated with simulation dates.}

interface
uses WinProcs, WinTypes, ExtCtrls, Classes, StdCtrls, graphics, udate, ulogvar;

const
  kTitleLength = 64;
  kBetweenGap = 4;
  kOutsideGap = 8;
  kGraphTickHeight = 4;
  kMaxDateStringLength = 13;

type
GsGraphScale = class
  public
  computed: boolean;
  xStart, xRange, yStart, yRange: single;
  graphRect: TRect;
  axisTitle: string[kTitleLength];
  autoTitle: boolean;
  autoScale: boolean;
  showZeroLine: boolean;
  constructor create;
  procedure resetY;
	function scalePoint(x, y: single): TPoint;
  procedure computeScaleForLoggedVar(loggedVar: GsLoggedVar; startIndex, endIndex: longint);
	procedure prettifyBounds(includeZero: boolean; significantDigits: integer);
  function getTitle: string;
  procedure setTitle(newTitle: string);
  procedure setXStartAndRange(start: single; range: single);
  function includesZero: boolean;
  end;

GsGraph = class(TImage)
	public
  graphType: integer;
  scales: array[0..1] of GsGraphScale;
  graphTitle: string[kTitleLength];
  startDate: GsDate;
  daysToShow: longint;
  graphSurface: TBitmap;
  graphRect: TRect;
	constructor create(AOwner: TComponent); override;
	destructor destroy; override;
  procedure setStartDateAndDaysToShow(aStartDate: GsDate; newDaysToShow: longint);
  procedure plotData(listBox: TListBox; correlate: boolean);
	procedure plotVar(scale: GsGraphScale; loggedVar: GsLoggedVar);
  function getTitle: string;
  procedure setTitle(newTitle: string);
  function textHeight: integer;
  procedure drawGraphBackground(listBox: TListBox; correlate: boolean; leftAxisVarsToPlot, rightAxisVarsToPlot: integer);
  procedure drawTextLabel(text: string; drawRect: TRect);
  procedure drawTextBottomToTop(text: string; x, y: integer);
  procedure setTextAngle(angle: Word);
  procedure setSize(newLeft, newTop, newWidth, newHeight: integer);
  function computeEndDate: GsDate;
	procedure drawMonthTicks;
  procedure drawLineAtZeroForScale(scaleIndex: integer);
  end;

implementation

uses Forms, ugraph, ueutils, sysutils, usupport;

{ ----------------------------------- GsGraph ------------------------------------------------ }
{ -------------------------------------------------------------------------------------------- }
constructor GsGraph.create(AOwner: TComponent);
	begin
  inherited create(AOwner);
  scales[kLeftAxis] := GsGraphScale.create;
  scales[kRightAxis] := GsGraphScale.create;
  graphSurface := TBitmap.create;
  graphTitle := '';
  autoSize := true;
  end;

destructor GsGraph.destroy;
  begin
  scales[kLeftAxis].free;
  scales[kRightAxis].free;
  graphSurface.free;
  inherited destroy;
  end;

function GsGraph.getTitle: string;
  begin
  result := graphTitle;
  end;

procedure GsGraph.setTitle(newTitle: string);
  begin
  graphTitle := copy(newTitle, 1, kTitleLength);
  end;

procedure GsGraph.setSize(newLeft, newTop, newWidth, newHeight: integer);
  begin
  self.left := newLeft;
  self.top := newTop;
  self.width := newWidth;
  self.height := newHeight;
  end;

procedure GsGraph.setStartDateAndDaysToShow(aStartDate: GsDate; newDaysToShow: longint);
  begin
  startDate := aStartDate;
  daysToShow := newDaysToShow;
  end;

procedure GsGraph.plotData(listBox: TListBox; correlate: boolean);
	var
  	i, varsToPlot, leftAxisVarsToPlot, rightAxisVarsToPlot: integer;
	  loggedVar: GsLoggedVar;
    x, y: single;
    plotPoint: integer;
    date: GsDate;
    latestDate: GsDate;
    aPoint: TPoint;
    leftLoggedVar, rightLoggedVar: GsLoggedVar;
	begin
  { dynamically compute scale for now each time - later consider only new data if no options changed }
  try
    for i := kLeftAxis to kRightAxis do scales[i].computed := false;
    varsToPlot := 0;
    leftAxisVarsToPlot := 0;
    rightAxisVarsToPlot := 0;
    if listBox.items.count > 0 then for i := 0 to listBox.items.count - 1 do
      begin
      loggedVar := (listBox.items.objects[i] as GsLoggedVar);
      if loggedVar.plotFlag then
        begin
        if loggedVar.axisToPlotOn = kLeftAxis then
          begin
          { if correlating, only want to consider ONE var on left axis in figuring scale }
          if correlate and (leftAxisVarsToPlot > 0) then
            continue
          else
            inc(leftAxisVarsToPlot);
          end
        else { right axis }
          begin
          { if correlating, only want to consider ONE var on right axis in figuring scale }
          if correlate and (rightAxisVarsToPlot > 0) then
            continue
          else
            inc(rightAxisVarsToPlot);
          end;
        inc(varsToPlot);
        if scales[loggedVar.axisToPlotOn].autoScale then
          scales[loggedVar.axisToPlotOn].computeScaleForLoggedVar(loggedVar, 0, daysToShow - 1);
        end;
      { if correlating, only want to consider ONE var on each axis in figuring scale }
      if correlate and (leftAxisVarsToPlot > 0) and (rightAxisVarsToPlot > 0) then break;
      end;
    for i := kLeftAxis to kRightAxis do with scales[i] do if autoScale and not computed then resetY;
    for i := kLeftAxis to kRightAxis do if scales[i].autoScale then scales[i].prettifyBounds(true, 3);
    self.drawGraphBackGround(listBox, correlate, leftAxisVarsToPlot, rightAxisVarsToPlot);
    if not correlate then
      begin
    	if (leftAxisVarsToPlot > 0) and (scales[kLeftAxis].showZeroLine) and (scales[kLeftAxis].includesZero) then
      	self.drawLineAtZeroForScale(kLeftAxis);
    	if (rightAxisVarsToPlot > 0) and (scales[kRightAxis].showZeroLine) and (scales[kRightAxis].includesZero) then
      	self.drawLineAtZeroForScale(kRightAxis);
    	{ draw lines }
    	for i := 0 to listBox.items.count - 1 do
      	begin
      	loggedVar := listBox.items.objects[i] as GsLoggedVar;
      	if loggedVar.plotFlag then
        	self.plotVar(scales[loggedVar.axisToPlotOn], loggedVar);
      	end;
      end
    else
      begin
      { making correlation go for only selected time instead of all data - better parallels other view }
      date := self.startDate;
      latestDate := self.startDate;
      GsDate_addDays(latestDate, self.daysToShow);
      with graphSurface.canvas do
        begin
        pen.color := clBlack;
        pen.width := 1;
        pen.style := psSolid;
        brush.style := bsSolid;
        end;
      leftLoggedVar := nil;
      rightLoggedVar := nil;
      for i := 0 to listBox.items.count - 1 do
        begin
        loggedVar := listBox.items.objects[i] as GsLoggedVar;
        if loggedVar.plotFlag and (loggedVar.axisToPlotOn = kLeftAxis) then
          begin
          leftLoggedVar := loggedVar;
          break;
          end;
        end;
      for i := 0 to listBox.items.count - 1 do
        begin
        loggedVar := listBox.items.objects[i] as GsLoggedVar;
        if loggedVar.plotFlag and (loggedVar.axisToPlotOn = kRightAxis) then
          begin
          rightLoggedVar := loggedVar;
          break;
          end;
        end;
      if (leftLoggedVar <> nil) and (rightLoggedVar <> nil) then
      	while GsDate_daysBetween(date, latestDate) >= 0 do
        	begin
        	plotPoint := 0;
        	if leftLoggedVar.dataForDate(date, y) then inc(plotPoint);
        	if rightLoggedVar.dataForDate(date, x) then inc(plotPoint);
        	if plotPoint = 2 then
          	begin
          	aPoint := scales[kLeftAxis].scalePoint(x, y);
          	with graphSurface.canvas do
              rectangle(aPoint.x - 1, aPoint.y - 1, aPoint.x + 1, aPoint.y + 1);
          	end;
      		GsDate_addDays(date, 1);
        	end;
      end;
    self.canvas.draw(graphRect.left, graphRect.top, graphSurface);
  except
    errorMessage('Problem in plot data');
  end;
	end;

procedure GsGraph.drawGraphBackground(listBox: TListBox; correlate: boolean; leftAxisVarsToPlot, rightAxisVarsToPlot: integer);
  var
    haveLeftVars, haveRightVars, haveTitle, haveLeftAxisTitle, haveRightAxisTitle: boolean;
    theTextHeight, centerWidth, centerHeight: integer;
    leftAxisTitleRect, rightAxisTitleRect, graphTitleRect,
      leftMinRect, leftMaxRect, rightMinRect, rightMaxRect,
      dateMinRect, dateMaxRect, timeRect: TRect;
    leftMinString, leftMaxString, rightMinString, rightMaxString,
      bottomMinString, bottomMaxString: string[30];
    endDate: GsDate;
    bottomLabel, leftLabel, rightLabel, graphTitle: string;
    i: longint;
	  loggedVar: GsLoggedVar;
  begin
  self.canvas.font := (parent as TPanel).font;
  theTextHeight := self.textHeight;
  haveLeftVars := leftAxisVarsToPlot > 0;
  haveRightVars := rightAxisVarsToPlot > 0;
  { in testing for titles, could trim spaces, but user could have only spaces to allocate space in the graph }
  haveTitle := (length(self.getTitle) > 0);
  haveLeftAxisTitle := (length(scales[kLeftAxis].getTitle) > 0);
  haveRightAxisTitle := (length(scales[kRightAxis].getTitle) > 0);
  centerWidth := self.width div 2;
  bottomLabel := 'time';
  if haveTitle then
    centerHeight := (self.height - theTextHeight * 2 - kBetweenGap * 2 - kOutsideGap * 2) div 2
        + (theTextHeight + kBetweenGap + kOutsideGap)
  else
    centerHeight := (self.height - theTextHeight - kBetweenGap - kOutsideGap * 2) div 2
        + kOutsideGap;
  { set up all strings to draw on graph }
  leftMinString := digitValueString(scales[kLeftAxis].yStart);
  leftMaxString := digitValueString(scales[kLeftAxis].yStart + scales[kLeftAxis].yRange);
  rightMinString := digitValueString(scales[kRightAxis].yStart);
  rightMaxString := digitValueString(scales[kRightAxis].yStart + scales[kRightAxis].yRange);
  bottomMinString := GsDate_monthShortString(startDate) + ' '
    + GsDate_dayOfMonthString(startDate) + ', ' + GsDate_yearString(startDate);
  endDate := self.computeEndDate;
  bottomMaxString := GsDate_monthShortString(endDate) + ' '
    + GsDate_dayOfMonthString(endDate) + ', ' + GsDate_yearString(endDate);
  graphTitle := self.getTitle;
  leftLabel := scales[kLeftAxis].getTitle;
  rightLabel := scales[kRightAxis].getTitle;

  { if this is a correlation graph, use right axis title on bottom (X), and warn user if two aspects not selected
    on left and right }
  if correlate then
    begin
    if haveLeftAxisTitle and haveRightAxisTitle then
      begin
  	  bottomLabel := rightLabel + ' [' + GsDate_monthShortString(startDate) + ' ' + GsDate_yearString(startDate)
        + ' - ' + GsDate_monthShortString(endDate) + ' ' + GsDate_yearString(endDate) + ']';
  	  bottomMinString := rightMinString;
  	  bottomMaxString := rightMaxString;
      end
    else
      begin
  	  bottomLabel := '(select left and right axis aspects for xy mode)';
  	  bottomMinString := '';
  	  bottomMaxString := '';
      end;
    haveRightVars := false;
    haveRightAxisTitle := false;
    end;

  { figure all rectangles }
  if haveTitle then with graphTitleRect do  { graph title }
    begin
    left := centerWidth - self.canvas.textWidth(graphTitle) div 2;
    right := left + self.canvas.textWidth(graphTitle);
    top := kOutsideGap;
    bottom := top + theTextHeight;
    end;
  if haveLeftVars then with leftMinRect do  { left min }
    begin
    left := kOutsideGap;
    right := left + self.canvas.textWidth(leftMinString);
    bottom := self.height -  kOutsideGap - theTextHeight - kBetweenGap;
    top := bottom - theTextHeight;
    end;
  if haveLeftVars then with leftMaxRect do { left max }
    begin
    left := kOutsideGap;
    right := left + self.canvas.textWidth(leftMaxString);
    if haveTitle then
      top := graphTitleRect.bottom + kBetweenGap
    else
      top := kOutsideGap;
    bottom := top + theTextHeight;
    end;
  if haveLeftAxisTitle then with leftAxisTitleRect do { left axis title }
    begin
    left := kOutsideGap;
    right := left + theTextHeight;
    top := centerHeight - self.canvas.textWidth(leftLabel) div 2;
    bottom := top + self.canvas.textWidth(leftLabel);
    end;
  if haveRightVars then with rightMinRect do  { right min }
    begin
    right := self.width - kOutsideGap;
    left := right - self.canvas.textWidth(rightMinString);
    bottom := self.height -  kOutsideGap - theTextHeight - kBetweenGap;
    top := bottom - theTextHeight;
    end;
  if haveRightVars then with rightMaxRect do  { right max }
    begin
    right := self.width - kOutsideGap;
    left := right - self.canvas.textWidth(rightMaxString);
    if haveTitle then
      top := graphTitleRect.bottom + kBetweenGap
    else
      top := kOutsideGap;
    bottom := top + theTextHeight;
    end;
  if haveRightAxisTitle then with rightAxisTitleRect do { right axis title }
    begin
    right := self.width - kOutsideGap;
    left := right - theTextHeight;
    top := centerHeight - self.canvas.textWidth(rightLabel) div 2;
    bottom := top + self.canvas.textWidth(rightLabel);
    end;
  with dateMinRect do  { date min }
    begin
    if haveLeftVars then
      left := intMax(kOutsideGap, intMax(leftMinRect.right, leftMaxRect.right) + kBetweenGap
        - self.canvas.textWidth(bottomMinString) div 2)
    else
      left := kOutsideGap;
    right := left + self.canvas.textWidth(bottomMinString);
    bottom := self.height - kOutsideGap;
    top := bottom - theTextHeight;
    end;
  with dateMaxRect do  { date max }
    begin
    if haveRightVars then
      right := intMin(self.width - kOutsideGap - kBetweenGap, intMin(rightMinRect.left, rightMaxRect.left)
        - kBetweenGap + self.canvas.textWidth(bottomMinString) div 2)
    else
      right := self.width - kOutsideGap;
    left := right - self.canvas.textWidth(bottomMaxString);
    bottom := self.height - kOutsideGap;
    top := bottom - theTextHeight;
    end;
  with timeRect do  { time }
    begin
    left := (dateMaxRect.right - dateMinRect.left) div 2 - self.canvas.textWidth(bottomLabel) div 2;
    right := left + self.canvas.textWidth(bottomLabel);
    top := dateMinRect.top;
    bottom := top + theTextHeight;
    end;
  with graphRect do  { graph rect }
    begin
    left := dateMinRect.left + rWidth(dateMinRect) div 2;
    right := dateMaxRect.right - rWidth(dateMaxRect) div 2;
    if haveTitle then
      top := graphTitleRect.bottom + kBetweenGap + theTextHeight div 2
    else
      top := kOutsideGap + theTextHeight div 2;
    bottom := dateMinRect.top - kBetweenGap - theTextHeight div 2;
    end;
  if graphSurface.width <> graphRect.right - graphRect.left then
    graphSurface.width := graphRect.right - graphRect.left;
  if graphSurface.height <> graphRect.bottom - graphRect.top then
    graphSurface.height := graphRect.bottom - graphRect.top;
  with graphSurface.canvas do 
    begin
    brush.color := clWhite;
    fillRect(rect(0, 0, graphSurface.width, graphSurface.height));
    brush.style := bsClear;
    end;
  with self.canvas do
    begin
    brush.color := clWhite;
    brush.style := bsSolid;
    fillRect(self.boundsRect);
    pen.color := clBlack;
    { draw bottom line and main title }
    moveTo(graphRect.left - 1, graphRect.bottom);
    lineTo(graphRect.right, graphRect.bottom);
    if haveTitle then self.drawTextLabel(graphTitle, graphTitleRect);
    { draw left axis line and title }
    if haveLeftVars or not haveRightVars then
      begin
      moveTo(graphRect.left - 1, graphRect.bottom);
    	lineTo(graphRect.left - 1, graphRect.top - 1);
      if haveLeftAxisTitle then self.drawTextBottomToTop(leftLabel,
          leftAxisTitleRect.left, leftAxisTitleRect.bottom);
      end;
    { draw right axis line and title }
    if haveRightVars then
      begin
      moveTo(graphRect.right, graphRect.top - 1);
    	lineTo(graphRect.right, graphRect.bottom);
      if haveRightAxisTitle then self.drawTextBottomToTop(rightLabel,
          rightAxisTitleRect.left, rightAxisTitleRect.bottom);
      end;
    { draw min and max value and date strings }
    if haveLeftVars then
      begin
      self.drawTextLabel(leftMinString, leftMinRect);
      self.drawTextLabel(leftMaxString, leftMaxRect);
      end;
    if haveRightVars then
      begin
      self.drawTextLabel(rightMinString, rightMinRect);
      self.drawTextLabel(rightMaxString, rightMaxRect);
      end;
    self.drawTextLabel(bottomMinString, dateMinRect);
    self.drawTextLabel(bottomMaxString, dateMaxRect);
    self.drawTextLabel(bottomLabel, timeRect);
    end;
  { tell scales what the graphRect is and what the x range is }
  scales[kLeftAxis].graphRect := rect(0, 0, graphSurface.width, graphSurface.height);
  scales[kRightAxis].graphRect := rect(0, 0, graphSurface.width, graphSurface.height);
  scales[kLeftAxis].setXStartAndRange(0, daysToShow - 1);
  scales[kRightAxis].setXStartAndRange(0, daysToShow - 1);
  if not correlate then
    begin
    scales[kLeftAxis].setXStartAndRange(0, daysToShow - 1);
    scales[kRightAxis].setXStartAndRange(0, daysToShow - 1);
  	self.drawMonthTicks;
    end
  else
  	scales[kLeftAxis].setXStartAndRange(scales[kRightAxis].yStart, scales[kRightAxis].yStart + scales[kRightAxis].yRange);
  end;

procedure GsGraph.drawMonthTicks;
  var
  	dateToPlot: GsDate;
    dayFromStart: longint;
    plotPoint: TPoint;
  begin
  dateToPlot := startDate;
  dayFromStart := 0;
  while dayFromStart < daysToShow - 1 do
    begin
    plotPoint := scales[kLeftAxis].scalePoint(dayFromStart, 0.0);
    plotPoint.x := plotPoint.x + graphRect.left;
    self.canvas.moveTo(plotPoint.x, graphRect.bottom);
    if GsDate_monthFromDate(dateToPlot) = kJanuary then
    	self.canvas.lineTo(plotPoint.x, graphRect.bottom + kGraphTickHeight * 2)
    else
    	self.canvas.lineTo(plotPoint.x, graphRect.bottom + kGraphTickHeight);
  	dayFromStart := dayFromStart + GsDate_daysInMonthFromDate(dateToPlot);
    GsDate_addMonths(dateToPlot, 1);
    end;
  end;

procedure GsGraph.drawLineAtZeroForScale(scaleIndex: integer);
  var
    plotPoint: TPoint;
  begin
  with graphSurface.canvas do
    begin
    pen.color := clBtnFace;
    pen.style := psSolid;
    pen.width := 1;
    plotPoint := scales[scaleIndex].scalePoint(0, 0.0);
    moveTo(0, plotPoint.y);
    lineTo(graphSurface.width, plotPoint.y);
    brush.style := bsSolid;
    { draw arrow pointing to scale this zero line pertains to }
    if scaleIndex = kLeftAxis then
      begin
      moveTo(8, plotPoint.y - 4);
      lineTo(0, plotPoint.y);
      lineTo(8, plotPoint.y + 5);
      end
    else if scaleIndex = kRightAxis then
      begin
      moveTo(graphSurface.width - 8, plotPoint.y - 4);
      lineTo(graphSurface.width, plotPoint.y);
      lineTo(graphSurface.width - 8, plotPoint.y + 5);
      end;
    brush.style := bsClear;
    end;
  end;

procedure GsGraph.drawTextLabel(text: string; drawRect: TRect);
  var
    cText: array[0..255] of Char;
  begin
  strPCopy(cText, '');
  with self.canvas do
    begin
    font := (parent as TPanel).font;
    font.color := clBtnText;
    strPCopy(cText, text);
    winprocs.drawText(handle, cText, strLen(cText), drawRect, DT_LEFT);
    end;
  end;

procedure GsGraph.drawTextBottomToTop(text: string; x, y: integer);
  begin
  self.setTextAngle(900); { 90 degrees }
  self.canvas.textOut(x, y, text);
  self.setTextAngle(0);
  end;

procedure GsGraph.setTextAngle(angle: Word);
{ got this function from Lloyd's help file. not sure if Delphi will really deallocate old font handle }
{ c - The canvas on which to output the text. The font for the canvas must be a scaleable font.
  d - The angle in tenths of degrees (450 would be 45 degrees). To reset the text back to normal, make d = 0. }
  var
    LogRec: TLOGFONT;
  begin
  { Get the current font information. We only want to modify the angle }
  GetObject(self.canvas.Font.Handle, SizeOf(LogRec), Addr(LogRec));
  { Modify the angle. lfEscapement is "The angle, in tenths of a degrees, between the base
    line of a character and the x-axis." (Windows API Help file.) }
  LogRec.lfEscapement := angle;
  { Delphi will handle deallocation of the old font handle }
  self.canvas.Font.Handle := CreateFontIndirect(LogRec);
  end;

function GsGraph.textHeight: integer;
  begin
  result := TGraphForm(owner).canvas.textHeight('W') + 2; { 2 is for button border }
  end;

procedure GsGraph.plotVar(scale: GsGraphScale; loggedVar: GsLoggedVar);
	var
  	i: longint;
    value: single;
    date: GsDate;
    previousPointValid: boolean;
    previousPoint, currentPoint, zeroPoint: TPoint;
    startIndex: longint;
    barWidth: integer;
	begin
  if daysToShow <= 0 then exit;
  barWidth := 0;
  with graphSurface.canvas do
    begin
    if loggedVar.drawBars then
      begin
      pen.color := loggedVar.lineColor;
      pen.style := psSolid;
      pen.width := 1;
      barWidth := intMin(100, intMax(1, rWidth(graphRect) div (TGraphForm(owner).monthsToShow * 31)));
      brush.color := loggedvar.lineColor;
      brush.style := bsSolid;
      end
    else
      begin
      pen.color := loggedVar.lineColor;
      pen.width := loggedVar.lineWidth;
      pen.style := loggedVar.lineStyle;
      brush.style := bsClear;
      end;
    previousPointValid := false;
    startIndex := loggedVar.dataPointIndexFromCreationForDate(startDate);
    if daysToShow > 0 then
    	for i := 0 to daysToShow - 1 do
      	begin
      	if loggedVar.dataForIndexFromCreation(startIndex + i, value) then
    	  	begin
        	currentPoint := scale.scalePoint(i, value);
          if loggedVar.drawBars then
            begin
            if value <> scale.yStart then
              begin
              zeroPoint := scale.scalePoint(i, 0.0);
              if barWidth <= 2 then
                rectangle(currentPoint.x, zeroPoint.y,
                  currentPoint.x + barWidth, currentPoint.y)
              else
                rectangle(currentPoint.x - barWidth div 2, zeroPoint.y,
                  currentPoint.x + barWidth div 2, currentPoint.y)
              end;
            end
          else
            begin
       	    if previousPointValid then moveTo(previousPoint.x, previousPoint.y);
            if previousPointValid then lineTo(currentPoint.x, currentPoint.y);
            end;
        	previousPoint := currentPoint;
        	previousPointValid := true;
        	end
      	else
    	  	previousPointValid := false;
      	GsDate_addDays(date, 1);
      	end;
    end;
  end;

function GsGraph.computeEndDate: GsDate;
  begin
  result := startDate;
  GsDate_addDays(result, daysToShow - 1);
  end;

{ ----------------------------------- GsGraphScale ------------------------------------------------ }
{ ------------------------------------------------------------------------------------------------- }
constructor GsGraphScale.create;
  begin
  axisTitle := '';
  autoScale := true;
  showZeroLine := true;
  autoTitle := true;
  end;

function GsGraphScale.scalePoint(x, y: single): TPoint;
	begin
  if xRange = 0 then xRange := 1;
  if yRange = 0 then yRange := 1;
  result.x := graphRect.left + round((x - xStart) / xRange * rWidth(graphRect));
  result.y := graphRect.top + round(rHeight(graphRect) - ((y - yStart) / yRange * rHeight(graphRect)));
  end;

procedure GsGraphScale.setXStartAndRange(start: single; range: single);
  begin
  xStart := start;
  xRange := range;
  end;

function GsGraphScale.getTitle: string;
  begin
  result := axisTitle;
  end;

procedure GsGraphScale.setTitle(newTitle: string);
  begin
  axisTitle := copy(newTitle, 1, kTitleLength);
  end;

procedure GsGraphScale.resetY;
  begin
  yStart := 0.0;
  yRange := 0.0;
  end;

function GsGraphScale.includesZero: boolean;
  begin
  result := (yStart < 0.0) and (yRange > 0.0);
  end;

procedure GsGraphScale.computeScaleForLoggedVar(loggedVar: GsLoggedVar; startIndex, endIndex: longint);
	var
  	minValue, maxValue: single;
    index: longint;
    value: single;
	begin
    minValue := 0.0;
    maxValue := 0.0;
    if computed then
    	begin
    	minValue := yStart;
      maxValue := yStart + yRange;
      end;
    if loggedVar.isAnyDataAvailable then
    	for index := loggedVar.firstAvailableDataPointIndex to loggedVar.lastAvailableDataPointIndex do
        begin
    		if loggedVar.dataForIndexFromCreation(index, value) then
      		begin
        	if not computed then
        		begin
          	minValue := value;
          	maxValue := value;
          	computed := true;
          	end
        	else
        		begin
    				if value < minValue then minValue := value;
        		if value > maxValue then maxValue := value;
          	end;
        	end;
        end;
    if computed then
    	begin
    	if minValue = maxValue then
        begin
        if minValue < 0.0 then
          begin
          maxValue := 0.0;
          end
        else if minValue = 0.0 then
          begin
          maxValue := 1.0;
          end
        else {minValue > 0.0}
          begin
          minValue := 0.0;
          end;
        end;
    	yStart := minValue;
    	yRange := maxValue - minValue;
    	xStart := startIndex;
    	xRange := endIndex - startIndex;
    	end;
  end;

{make bounds something rounded}
procedure prettifyLowerBoundAndRange(var lowerBoundVar: single; var rangeVar: single;
		significantDigits: integer; includeZero: boolean);
  var
    firstRangeDigit: integer;
    rangeMultiplier: extended;
    upperBound: extended;
    lowerBound: extended;
    range: extended;
	begin
  try
  range := rangeVar;
  lowerBound := lowerBoundVar;
  upperBound := lowerBound + range;
  {adjust lower to zero if requested}
  if includeZero then
  	if (lowerBound >= 0.0) then
    	begin
      range := range + lowerBound;
      lowerBound := 0.0;
      end
  	else if (upperBound < 0.0) then
    	begin
      range := range - upperBound;
      upperBound := 0.0;
      end;
  firstRangeDigit := firstSignificantDigitPosition(range);
  {include the significant digits in range multiplier}
  {use of either power or loop leads to small 9th place rounding errors}
  rangeMultiplier := power(10, firstRangeDigit - significantDigits + 1);
  {rangeMultiplier := 1;
  firstRangeDigit := firstRangeDigit - significantDigits + 1;
  if firstRangeDigit > 0 then
  	for i := 1 to firstRangeDigit do
      rangeMultiplier := rangeMultiplier * 10.0
  else if firstRangeDigit < 0 then
  	for i := -1 downto firstRangeDigit do
      rangeMultiplier := rangeMultiplier / 10.0; }
  try
  { trunc will fail if the number is outside the longint range; in that case, don't prettify bounds }
  if lowerBound <> 0 then
  	begin
    lowerBound := lowerBound / rangeMultiplier;
    {trunc rounds toward zero}
    if lowerBound >= 0.0 then
    	lowerBound := trunc(lowerBound) * rangeMultiplier
    else
    	lowerBound := trunc(lowerBound - 1.0) * rangeMultiplier;
    end;
  {singe range may no longer be correct for new lowerBound,
  	use previously recorded upper bound and then recalculate range}
  upperBound := upperBound / rangeMultiplier;
  {round upperBound up}
  if trunc(upperBound) <> upperBound then
  	upperBound := (trunc(upperBound) + 1.0) * rangeMultiplier
  else
    upperBound := trunc(upperBound) * rangeMultiplier;
  except
  end;
  {recompute range}
  rangeVar := upperBound - lowerBound;
  lowerBoundVar := lowerBound;
  except errorMessage('Problem in prettifyLowerBoundAndRange'); end;
  end;

procedure GsGraphScale.prettifyBounds(includeZero: boolean; significantDigits: integer);
	begin
  prettifyLowerBoundAndRange(yStart, yRange, significantDigits, includeZero);
  end;

end.

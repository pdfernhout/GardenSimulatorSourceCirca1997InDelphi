unit Uharvest;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uharvest: Harvest form. This form is fairly simple. All it has to do is draw the list
of harvested items, with or without pictures. It uses a TImage to draw the report
instead of a list box, because we wanted to handle wrapping the icons around ourselves
and we don't know the height of each line until it is drawn. An owner-draw variable-height
list box would also allow this, but we tried it and it was flickery and had
size/resource limits.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  uharvprt, ucollect, ufiler, ugsform;

type
  THarvestForm = class(GsForm)
    header: THeader;
    Close: TButton;
    helpButton: TBitBtn;
    editHarvestReports: TButton;
    harvestReports: TComboBox;
    reportLabel: TLabel;
    chooseFont: TBitBtn;
    reportScrollBar: TScrollBar;
    reportImage: TImage;
    showPictures: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure harvestReportsChange(Sender: TObject);
    procedure editHarvestReportsClick(Sender: TObject);
    procedure headerSized(Sender: TObject; ASection, AWidth: Integer);
    procedure FormResize(Sender: TObject);
    procedure chooseFontClick(Sender: TObject);
    procedure reportScrollBarChange(Sender: TObject);
    procedure showPicturesClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    streamingIn: boolean;
    { Public declarations }
    sortedHarvestItemList: TList; 
    reportGroupList: TListCollection;
    reportImageYScrollPosition: longint;
    reportImageEntireSize: longint;
    streamedHarvestReportIndex: longint;
    destructor destroy; override;
    function currentReport: GsHarvestReport;
    procedure resetHeaderForNewReport;
    procedure writeColumnInReportDrawList(section: smallint; const useText: string;
      var leftPos: smallint; rect: TRect);
    procedure calculateGroupHeights;
    procedure handlePossibleHarvestListChange;
    procedure resizeGardenImageBitmap;
		function groupHeight(Index: Integer): longint;
    procedure reportImageRepaint;
    procedure reportChanged;
		procedure drawReportGroup(thisGroup: GsHarvestItemSortingGroup;
			yStartOfGroupInImage: longint; selected: boolean);
		procedure drawIconsInReportDrawList(group: GsHarvestItemSortingGroup; drawRect: TRect; clippingAdjustment: longint);
    procedure fillReportComboBoxFromHarvestManager;
		procedure updateForNewDomain;
    procedure streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
    procedure respondToSimulationRunningStatus(isRunning: boolean);
  end;

var
  HarvestForm: THarvestForm;

implementation

{$R *.DFM}

uses udomain, usupport, ueutils, uhvrpted;

procedure THarvestForm.FormCreate(Sender: TObject);
  begin
  self.streamingIn := false;
  streamedHarvestReportIndex := -1;
  sortedHarvestItemList := TList.create;
  reportGroupList := TListCollection.create;
  self.resizeGardenImageBitmap;
  self.fillReportComboBoxFromHarvestManager;
  showPictures.checked := false;
  self.reportChanged;
  end;

destructor THarvestForm.destroy;
  begin
  sortedHarvestItemList.free;
  sortedHarvestItemList := nil;
  reportGroupList.free;
  reportGroupList := nil;
  inherited destroy;
  end;

procedure THarvestForm.streamInfoWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  var
    tempPicturesChecked: boolean;
  begin
  if filer.isReading then self.streamingIn := true;
  { the cvir passed in is the windowManager cvir, because this object is not a streamable object }
  StreamFormPositionInfoWithFiler(filer, cvir, self);
  if filer.isReading then
    begin
    filer.streamBoolean(tempPicturesChecked);
    showPictures.checked := tempPicturesChecked;
    end
  else
    begin
    tempPicturesChecked := showPictures.checked;
    filer.streamBoolean(tempPicturesChecked);
    end;
  StreamComboBoxItemIndexToTempVarWithFiler(filer, cvir, harvestReports, streamedHarvestReportIndex);
  self.streamingIn := false;
  end;

procedure THarvestForm.updateForNewDomain;
  begin
  sortedHarvestItemList.clear;
  reportGroupList.clear;
  self.fillReportComboBoxFromHarvestManager;
  if streamedHarvestReportIndex >= 0 then
    begin
    if (streamedHarvestReportIndex <= harvestReports.items.count - 1) then
      harvestReports.itemIndex := streamedHarvestReportIndex;
    streamedHarvestReportIndex := -1;
    end
  else
    harvestReports.itemIndex := 0;
  self.reportChanged;
  end;

procedure THarvestForm.respondToSimulationRunningStatus(isRunning: boolean);
  begin
  editHarvestReports.enabled := not isRunning;
  end;

procedure THarvestForm.fillReportComboBoxFromHarvestManager;
  var
    i, choice: longint;
    report: GsHarvestReport;
  begin
  if harvestReports.itemIndex <> -1 then
    choice := harvestReports.itemIndex
  else
    choice := 0;
  harvestReports.clear;
  if Domain.harvestManager.harvestReportList.count > 0 then
    begin
    for i := 0 to Domain.harvestManager.harvestReportList.count - 1 do
      begin
      report := GsHarvestReport(Domain.harvestManager.harvestReportList.items[i]);
      harvestReports.items.addObject(report.name, report);
      end;
    harvestReports.itemIndex := choice;
    self.harvestReportsChange(self);
    end;
  end;

procedure THarvestForm.reportChanged;
  begin
  if self.streamingIn then exit;
  reportImageYScrollPosition := 0;
  reportScrollBar.position := 0;
  self.calculateGroupHeights;
  if reportImageEntireSize > self.reportImage.height then
    begin
    reportScrollBar.visible := true;
  	reportScrollBar.min := 0;
  	if reportImageEntireSize < 32000 - self.reportImage.height then
  		reportScrollBar.max := reportImageEntireSize - self.reportImage.height
  	else
    	reportScrollBar.max := 32000;
    reportScrollBar.largeChange := self.reportImage.height;
    end
  else
    reportScrollBar.visible := false;
  self.reportImageRepaint;
  end;

procedure THarvestForm.resizeGardenImageBitmap;
  begin
  reportImage.picture.bitmap.width := reportImage.width;
  reportImage.picture.bitmap.height := reportImage.height;
  end;

function THarvestForm.currentReport: GsHarvestReport;
  begin
  if harvestReports.itemIndex <> -1 then
    result := GsHarvestReport(harvestReports.items.objects[harvestReports.itemIndex])
  else
    result := nil;
  end;

procedure THarvestForm.CloseClick(Sender: TObject);
  begin
  self.visible := false;
  end;

procedure THarvestForm.showPicturesClick(Sender: TObject);
  begin
  { to force list box to create measure item function call }
  self.reportChanged;
  end;

procedure THarvestForm.calculateGroupHeights;
  var
    i: longint;
    theHeight: longint;
  begin
  reportImageEntireSize := 0;
  if reportGroupList.count > 0 then
    for i := 0 to reportGroupList.count - 1 do
      begin
      theHeight := self.groupHeight(i);
      GsHarvestItemSortingGroup(reportGroupList.items[i]).height := theHeight;
      reportImageEntireSize := reportImageEntireSize + theHeight;
      end;
  end;

function THarvestForm.groupHeight(Index: Integer): longint;
  var
    iconsInOneRow, iconsInGroup, rows, tail: longint;
  begin
  result := reportImage.canvas.textHeight('W') + 2;
  if showPictures.checked then
    begin
    iconsInOneRow := reportImage.width div 32;
    if iconsInOneRow <= 0 then iconsInOneRow := 1;
    iconsInGroup := GsHarvestItemSortingGroup(reportGroupList.items[index]).numHarvestItems;
    tail := iconsInGroup mod iconsInOneRow;
    rows := iconsInGroup div iconsInOneRow;
    if (tail > 0) and (tail < iconsInOneRow) then inc(rows);
    if rows > 1000 then rows := 1000;
    result := result + rows * 32;
    end;
  end;

procedure THarvestForm.handlePossibleHarvestListChange;
  begin
  { same as harvestReportsChange, but don't redo header }
  if self.currentReport = nil then exit;
  Domain.harvestManager.sortHarvestListUsingHarvestReport(sortedHarvestItemList, self.currentReport);
  Domain.harvestManager.fillReportGroupList(reportGroupList, sortedHarvestItemList, self.currentReport);
  self.reportChanged;
  end;

procedure THarvestForm.harvestReportsChange(Sender: TObject);
  begin
  if self.currentReport = nil then exit;
  Domain.harvestManager.sortHarvestListUsingHarvestReport(sortedHarvestItemList, self.currentReport);
  Domain.harvestManager.fillReportGroupList(reportGroupList, sortedHarvestItemList, self.currentReport);
  self.resetHeaderForNewReport;
  self.reportChanged;
  end;

procedure THarvestForm.resetHeaderForNewReport;
  var
    i, section, showArrayIndex, sortByArrayIndex: smallint;
    headerString: string;
  begin
  if self.currentReport = nil then exit;
  header.sections.clear;
  section := 0;
  for i := 0 to kHarvestReportSortByLastChoice do
    begin
    if self.currentReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then break;
    sortByArrayIndex := self.currentReport.sortByChoicesArray[i];
    headerString := Domain.harvestManager.sortByChoicesArrayDescription(sortByArrayIndex);
    header.sections.add(headerString);
    { set section width based on length of text }
    header.sectionWidth[section] := reportImage.canvas.textWidth(headerString) + 6;
    inc(section);
    end;
  for i := 0 to kHarvestReportShowLastChoice do
    begin
    if self.currentReport.showChoicesArray[i] = kHarvestReportShowEndOfList then break;
    showArrayIndex := self.currentReport.showChoicesArray[i];
    headerString := Domain.harvestManager.harvestItemTemplateInfoArrayDescription(showArrayIndex);
    header.sections.add(headerString);
    { set section width based on length of text }
    header.sectionWidth[section] := reportImage.canvas.textWidth(headerString) + 6;
    inc(section);
    end;
  end;

procedure THarvestForm.editHarvestReportsClick(Sender: TObject);
  var
    harvestReportEditorForm: THarvestReportEditorForm;
  begin
  harvestReportEditorForm := THarvestReportEditorForm.create(self);
  if harvestReportEditorForm = nil then
    raise Exception.create('Could not create harvest report editor');
  try
    harvestReportEditorForm.setSelectedReport(self.currentReport);
    if harvestReportEditorForm.showModal = mrOK then
      begin
      self.fillReportComboBoxFromHarvestManager;
  		self.reportChanged;
      end;
  finally
    harvestReportEditorForm.free;
    harvestReportEditorForm := nil;
  end;
  end;

procedure THarvestForm.headerSized(Sender: TObject; ASection,
  AWidth: Integer);
  begin
  self.reportImageRepaint;
  end;

const
  kBetweenGap = 4;

procedure THarvestForm.FormResize(Sender: TObject);
  begin
  if Application.terminated then exit;
  reportLabel.left := kBetweenGap;
  reportLabel.top := kBetweenGap;
  close.left := self.clientWidth - close.width - kBetweenGap;
  editHarvestReports.left := close.left;
  chooseFont.left := close.left;
  helpButton.left := close.left;
  showPictures.left := close.left;
  harvestReports.left := reportLabel.left + reportLabel.width + kBetweenGap;
  harvestReports.top := kBetweenGap;
  harvestReports.width := close.left - harvestReports.left - kBetweenGap;
  header.top := harvestReports.top + harvestReports.height + kBetweenGap;
  header.left := kBetweenGap;
  reportScrollBar.left := close.left - kBetweenGap - reportScrollBar.width;
  header.width := reportScrollBar.left - header.left - kBetweenGap;
  header.height := reportImage.canvas.textHeight('W') + 4;
  reportImage.top := header.top + header.height;
  reportImage.left := header.left;
  reportImage.width := header.width;
  reportImage.height := self.clientHeight - reportImage.top - kBetweenGap;
  reportScrollBar.top := reportImage.top;
  reportScrollBar.height := reportImage.height;
  self.resizeGardenImageBitmap;
  self.reportChanged;
  end;

procedure THarvestForm.chooseFontClick(Sender: TObject);
  var fontDialog: TFontDialog;
  begin
  fontDialog := TFontDialog.create(self);
  try
  fontDialog.font := header.font;
  if fontDialog.Execute then
    begin
    header.font := fontDialog.font;
    reportImage.canvas.font := fontDialog.font;
    self.FormResize(self);
    end;
  finally
  fontDialog.free;
  end;
  end;

procedure THarvestForm.reportImageRepaint;
  var
    firstOverlappingGroup, lastOverlappingGroup: longint;
    i, cumulativeHeight, scrolledReportImageBottom, yStartOfGroup: longint;
    thisGroup: GsHarvestItemSortingGroup;
  begin
  if Application.terminated then exit;
  {fill image with white}
  with reportImage.canvas do
    begin
    font := header.font;
    brush.style := bsSolid;
    brush.color := clWindow;
    pen.style := psSolid;
    pen.color := clBtnText;
    fillRect(reportImage.clientRect);
    end;
  if self.currentReport = nil then exit;
  if header.sections.count = 0 then exit;
  if reportGroupList.count <= 0 then exit;
  {find first group overlapping}
  firstOverlappingGroup := -1;
  cumulativeHeight := 0;
  yStartOfGroup := 0;
  for i := 0 to reportGroupList.count - 1 do
    begin
  	thisGroup := GsHarvestItemSortingGroup(reportGroupList.items[i]);
    cumulativeHeight := cumulativeHeight + thisGroup.height;
    if cumulativeHeight >= reportImageYScrollPosition then
      begin
    	firstOverlappingGroup := i;
      yStartOfGroup := cumulativeHeight - thisGroup.height;
      break;
      end;
    end;
  if firstOverlappingGroup = -1 then exit;
  {find last group overlapping}
  scrolledReportImageBottom := reportImage.height + reportImageYScrollPosition;
  if cumulativeHeight >= scrolledReportImageBottom then
    lastOverlappingGroup := firstOverlappingGroup
  else
    begin
  	lastOverlappingGroup := reportGroupList.count - 1; {default if go beyond end}
  	if firstOverlappingGroup >= reportGroupList.count - 1 then
  		for i := firstOverlappingGroup + 1 to reportGroupList.count - 1 do
    		begin
  			thisGroup := GsHarvestItemSortingGroup(reportGroupList.items[i]);
    		cumulativeHeight := cumulativeHeight + thisGroup.height;
    		if cumulativeHeight >= scrolledReportImageBottom then
      		begin
    			lastOverlappingGroup := i;
      		break;
      		end;
    		end;
     end;
  {draw all these groups}
  for i := firstOverlappingGroup to lastOverlappingGroup do
    begin
  	thisGroup := GsHarvestItemSortingGroup(reportGroupList.items[i]);
    self.drawReportGroup(thisGroup, yStartOfGroup - reportImageYScrollPosition, false);
    yStartOfGroup := yStartOfGroup + thisGroup.height;
    end;
  end;

procedure THarvestForm.drawReportGroup(thisGroup: GsHarvestItemSortingGroup;
		yStartOfGroupInImage: longint; selected: boolean);
  var
    i: smallint;
    section: smallint;
    leftPos: smallint;
    useText: string;
    rect: TRect;
    clippingAdjustment: smallint;
    textHeight: smallint;
  begin
  leftPos := 0;
  section := 0;
  textHeight := reportImage.canvas.textHeight('W') + 4;
  {clip rect to image}
  rect.left := reportImage.left;
  rect.right := reportImage.left + reportImage.width;
  if yStartOfGroupInImage < -textHeight then
    begin
  	clippingAdjustment := -yStartOfGroupInImage;
    rect.top := 0;
    end
  else
    begin
  	clippingAdjustment := 0;
    rect.top := yStartOfGroupInImage;
    end;
  if yStartOfGroupInImage + thisGroup.height > reportImage.height then
    rect.bottom := reportImage.height
  else
    rect.bottom := yStartOfGroupInImage + thisGroup.height;

  with reportImage.canvas do
    begin
    font := reportImage.canvas.font;
    font.color := clBtnText;
    brush.style := bsSolid;
    if selected then
      brush.color := clHighlight
    else
      brush.color := clWindow;
    fillRect(rect);
    brush.style := bsClear;
    if clippingAdjustment = 0 then
      begin
     	for i := 0 to kHarvestReportSortByLastChoice do
      	begin
      	if self.currentReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then break;
      	useText := thisGroup.textForSortIndex(self.currentReport.sortByChoicesArray[i]);
      	writeColumnInReportDrawList(section, useText, leftPos, rect);
      	inc(section);
      	end;
    	for i := 0 to kHarvestReportShowLastChoice do
      	begin
      	if self.currentReport.showChoicesArray[i] = kHarvestReportShowEndOfList then break;
      	useText := thisGroup.textForShowIndex(self.currentReport.showChoicesArray[i]);
      	writeColumnInReportDrawList(section, useText, leftPos, rect);
      	inc(section);
      	end;
      end;
    if showPictures.checked then
      begin
      self.drawIconsInReportDrawList(thisGroup, rect, clippingAdjustment);
      if rect.top > 0 then
        begin
        moveTo(rect.left, rect.top);
        lineTo(rect.right, rect.top);
        end;
      end;
    if selected then drawFocusRect(rect);
    end;
  end;

procedure THarvestForm.writeColumnInReportDrawList(section: smallint; const useText: string;
    var leftPos: smallint; rect: TRect);
  var
    drawRect: TRect;
    cText: array[0..255] of Char;
  begin
  with reportImage.canvas do
    begin
    drawRect.top := rect.top;
    drawRect.bottom := drawRect.top + textHeight('W');
    drawRect.left := leftPos;
    drawRect.right := leftPos + header.sectionWidth[section];
    leftPos := drawRect.right;
    { cut off with margin before line }
    drawRect.right := drawRect.right - 10;
    strPCopy(cText, useText);
    drawRect.left := drawRect.left + 3; { margin for text }
    drawRect.right := drawRect.right + 3; { margin for text }
    drawText(handle, cText, strLen(cText), drawRect, DT_LEFT);
    drawRect.left := drawRect.left - 3;
    drawRect.right := drawRect.right - 3;
    drawRect.right := drawRect.right + 10;
    if section <> header.sections.count - 1 then
      begin
      pen.color := clBtnShadow;
      pen.style := psSolid;
      moveTo(drawRect.right - 1, drawRect.top);
      lineTo(drawRect.right - 1, drawRect.bottom);
      end;
    end;
  end;

procedure THarvestForm.drawIconsInReportDrawList(group: GsHarvestItemSortingGroup;
    drawRect: TRect; clippingAdjustment: longint);
  var
    i, endIndex: longint;
    harvestItem: GsHarvestItem;
    drawX, drawY: longint;
    iconsInOneRow, row, column: longint;
    headerHeight: integer;
  begin
  iconsInOneRow := (drawRect.right - drawRect.left) div 32;
  row := 1;
  column := 1;
  endIndex := intMin(group.firstHarvestItemIndex + group.numHarvestItems - 1,
    sortedHarvestItemList.count - 1);
  with reportImage.canvas do
    begin
  	headerHeight := textHeight('W');
  	for i := group.firstHarvestItemIndex to endIndex do
    	begin
    	harvestItem := GsHarvestItem(sortedHarvestItemList.items[i]);
    	if (harvestItem = nil)
      	or (harvestItem.harvestItemTemplate = nil)
      	or (harvestItem.harvestItemTemplate.icon = nil)
      	or (harvestItem.harvestItemTemplate.icon.icon = nil) then continue;
    	drawX := (column - 1) * 32;
      drawY := headerHeight + (row - 1) * 32;
    	drawX := drawX + drawRect.left;
    	drawY := drawY + drawRect.top - clippingAdjustment;
      {test if would draw below image}
    	if drawY >= drawRect.bottom then
    		exit;
      {test if not drawing above image}
    	if drawY > drawRect.top - 32 then
    		draw(drawX, drawY, harvestItem.harvestItemTemplate.icon.icon);
    	if column >= iconsInOneRow then
      	begin
      	inc(row);
      	column := 1;
      	end
    	else
      	inc(column);
      end;
    end;
  end;

procedure THarvestForm.reportScrollBarChange(Sender: TObject);
	begin
  if reportScrollBar.max < 32000 then
    reportImageYScrollPosition := reportScrollBar.position
  else
    reportImageYScrollPosition := round((reportScrollBar.position / 32000.0) *
    	(reportImageEntireSize- self.reportImage.height));
  self.reportImageRepaint;
	end;

procedure THarvestForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Harvest_window')
  end;

end.

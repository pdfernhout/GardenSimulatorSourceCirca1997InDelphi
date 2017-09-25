unit Uhvrpted;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uhvrpted: Harvest report editor form. Edits harvest reports (read uharvprt first
before reading this). Fairly simple; two arrays ('show' and 'sort by') are edited
by moving items into and out of them from the list of available options. User
can also make new reports, copy, delete, export, import, etc. Because there
is no discrete harvest report manager to make a copy of for canceling, the
form copies to a list of harvest reports (in the list box) and checks the list
against the original list (in the harvest manager) when OK is clicked. This is
the same scheme as in the harvest item template editor. There are some fancy
things here to deal with the fact that the information is kept in arrays
(show and sort by) and not in objects.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, uharvprt, ucommand, ExtCtrls, ugsform;

type
  THarvestReportEditorForm = class(GsForm)
    OK: TButton;
    Cancel: TButton;
    copyReport: TButton;
    deleteReport: TButton;
    importReports: TButton;
    exportReports: TButton;
    helpButton: TBitBtn;
    Label1: TLabel;
    sortByGroupBox: TGroupBox;
    sortByChoicesListBox: TListBox;
    showGroupBox: TGroupBox;
    showPossibleChoicesListBox: TListBox;
    showChoicesListBox: TListBox;
    sortByPossibleChoicesListBox: TListBox;
    sortByMoveUp: TBitBtn;
    sortByMoveDown: TBitBtn;
    sortByAdd: TBitBtn;
    sortByRemove: TBitBtn;
    showAdd: TBitBtn;
    showRemove: TBitBtn;
    showMoveUp: TBitBtn;
    showMoveDown: TBitBtn;
    reportsListBox: TListBox;
    renameReport: TButton;
    newReport: TButton;
    resetPanel: TPanel;
    Label4: TLabel;
    everyLabel: TLabel;
    resetReportsWhatMonth: TComboBox;
    resetReportsHowManyYears: TComboBox;
    resetAllReportsNow: TButton;
    procedure CancelClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure copyReportClick(Sender: TObject);
    procedure deleteReportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure showAddClick(Sender: TObject);
    procedure showRemoveClick(Sender: TObject);
    procedure showMoveUpClick(Sender: TObject);
    procedure showMoveDownClick(Sender: TObject);
    procedure sortByAddClick(Sender: TObject);
    procedure sortByRemoveClick(Sender: TObject);
    procedure sortByMoveUpClick(Sender: TObject);
    procedure sortByMoveDownClick(Sender: TObject);
    procedure importReportsClick(Sender: TObject);
    procedure exportReportsClick(Sender: TObject);
    procedure reportsListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure reportsListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure renameReportClick(Sender: TObject);
    procedure reportsListBoxDblClick(Sender: TObject);
    procedure newReportClick(Sender: TObject);
    procedure resetReportsWhatMonthChange(Sender: TObject);
    procedure resetAllReportsNowClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    showPossibleChoicesListBoxIndexArray: array[0..kHarvestReportShowLastChoice] of smallint;
    sortByPossibleChoicesListBoxIndexArray: array[0..kHarvestReportSortByLastChoice] of smallint;
    resetHarvestListCommand: KfCommand;
    harvestListHasBeenReset: boolean;
    destructor destroy; override;
    procedure loadHarvestReportsFromHarvestManager;
    procedure saveHarvestReportsToHarvestManager;
    function currentReport: GsHarvestReport;
    procedure updateInfoForCurrentHarvestReport;
    procedure setSelectedReport(aReport: GsHarvestReport);
    procedure fillPossibleChoiceListBoxes;
    procedure undoResetAllReportsCommand;
  end;

implementation

{$R *.DFM}

uses udomain, usupport, ucollect, uharvest, ugscom, ugsim;

{ ------------------------------------------------------------------------------------------------- local functions }
function firstUnusedShowIndexForReport(aReport: GsHarvestReport): smallint;
  var i: smallint;
  begin
  result := -1;
  for i := 0 to kHarvestReportShowLastChoice do
    if aReport.showChoicesArray[i] = kHarvestReportShowEndOfList then
      begin
      result := i;
      break;
      end;
  end;

function reportShowsIndex(aReport: GsHarvestReport; index: smallint): boolean;
  var i: smallint;
  begin
  result := false;
  for i := 0 to kHarvestReportShowLastChoice do
    begin
    if aReport.showChoicesArray[i] = kHarvestReportShowEndOfList then exit;
    if aReport.showChoicesArray[i] = index then
      begin
      result := true;
      exit;
      end;
    end;
  end;

function firstUnusedSortByIndexForReport(aReport: GsHarvestReport): smallint;
  var i: smallint;
  begin
  result := -1;
  for i := 0 to kHarvestReportSortByLastChoice do
    if aReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then
      begin
      result := i;
      break;
      end;
  end;

function reportSortsByIndex(aReport: GsHarvestReport; index: smallint): boolean;
  var i: smallint;
  begin
  result := false;
  for i := 0 to kHarvestReportSortByLastChoice do
    begin
    if aReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then exit;
    if aReport.sortByChoicesArray[i] = index then
      begin
      result := true;
      exit;
      end;
    end;
  end;

procedure switchShowArrayValuesInReport(aReport: GsHarvestReport; firstIndex, secondIndex: smallint);
  var
    saveValue: smallint;
  begin
  saveValue := aReport.showChoicesArray[firstIndex];
  aReport.showChoicesArray[firstIndex] := aReport.showChoicesArray[secondIndex];
  aReport.showChoicesArray[secondIndex] := saveValue;
  end;

procedure switchSortByArrayValuesInReport(aReport: GsHarvestReport; firstIndex, secondIndex: smallint);
  var
    saveValue: smallint;
  begin
  saveValue := aReport.sortByChoicesArray[firstIndex];
  aReport.sortByChoicesArray[firstIndex] := aReport.sortByChoicesArray[secondIndex];
  aReport.sortByChoicesArray[secondIndex] := saveValue;
  end;

procedure cleanUpRemovedItemsInReportShowList(aReport: GsHarvestReport);
  var
    i, j, lastUnusedItem: smallint;
    packedArray: array[0..kHarvestReportShowLastChoice] of smallint;
  begin
  { start packed array at all -1 }
  for i := 0 to kHarvestReportShowLastChoice do packedArray[i] := kHarvestReportShowEndOfList;
  { make sure report array items after the LAST -1 are all -1 }
  lastUnusedItem := -1;
  for i := 0 to kHarvestReportShowLastChoice do
    if aReport.showChoicesArray[i] = kHarvestReportShowEndOfList then
       lastUnusedItem := i;
  if lastUnusedItem = -1 then
    raise Exception.create('No end-of-list marker in report show list');
  if lastUnusedItem < kHarvestReportShowLastChoice then
    for i := lastUnusedItem + 1 to kHarvestReportShowLastChoice do
      aReport.showChoicesArray[i] := kHarvestReportShowEndOfList;
  { copy all items that are not -1 to packed array }
  j := 0;
  for i := 0 to kHarvestReportShowLastChoice do
    if aReport.showChoicesArray[i] <> kHarvestReportShowEndOfList then
      begin
      packedArray[j] := aReport.showChoicesArray[i];
      inc(j);
      end;
  { copy packed array back to report array }
  for i := 0 to kHarvestReportShowLastChoice do
    aReport.showChoicesArray[i] := packedArray[i];
  end;

procedure cleanUpRemovedItemsInReportSortByList(aReport: GsHarvestReport);
  var
    i, j, lastUnusedItem: smallint;
    packedArray: array[0..kHarvestReportSortByLastChoice] of smallint;
  begin
  { start packed array at all -1 }
  for i := 0 to kHarvestReportSortByLastChoice do packedArray[i] := kHarvestReportSortEndOfList;
  { make sure report array items after the LAST -1 are all -1 }
  lastUnusedItem := -1;
  for i := 0 to kHarvestReportSortByLastChoice do
    if aReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then
       lastUnusedItem := i;
  if lastUnusedItem = -1 then
    raise Exception.create('No end-of-list marker in report sort by list');
  if lastUnusedItem < kHarvestReportSortByLastChoice then
    for i := lastUnusedItem + 1 to kHarvestReportSortByLastChoice do
      aReport.sortByChoicesArray[i] := kHarvestReportSortEndOfList;
  { copy all items that are not -1 to packed array }
  j := 0;
  for i := 0 to kHarvestReportSortByLastChoice do
    if aReport.sortByChoicesArray[i] <> kHarvestReportSortEndOfList then
      begin
      packedArray[j] := aReport.sortByChoicesArray[i];
      inc(j);
      end;
  { copy packed array back to report array }
  for i := 0 to kHarvestReportSortByLastChoice do
    aReport.sortByChoicesArray[i] := packedArray[i];
  end;

{ ------------------------------------------------------------------------------------------------- create/destroy }
procedure THarvestReportEditorForm.FormCreate(Sender: TObject);
  begin
  self.fillPossibleChoiceListBoxes;
  self.loadHarvestReportsFromHarvestManager;
  self.updateInfoForCurrentHarvestReport;
  end;

destructor THarvestReportEditorForm.destroy;
  var
    i: longint;
    reportInList: GsHarvestReport;
  begin
  { delete all copies of reports in list box }
  if reportsListBox.items.count > 0 then
    for i := 0 to reportsListBox.items.count - 1 do
    begin
    reportInList := GsHarvestReport(reportsListBox.items.objects[i]);
    reportInList.free;
    reportInList := nil;
    end;
  inherited destroy;
  end;

procedure THarvestReportEditorForm.fillPossibleChoiceListBoxes;
  var
    i: smallint;
  begin
  showPossibleChoicesListBox.clear;
  for i := 0 to kHarvestReportShowLastChoice do
    showPossibleChoicesListBox.items.add(Domain.harvestManager.harvestItemTemplateInfoArrayDescription(i));
  sortByPossibleChoicesListBox.clear;
  for i := 0 to kHarvestReportSortByLastChoice do
    sortByPossibleChoicesListBox.items.add(Domain.harvestManager.sortByChoicesArrayDescription(i));
  end;

procedure THarvestReportEditorForm.loadHarvestReportsFromHarvestManager;
  var
    i: longint;
    report: GsHarvestReport;
    newReport: GsHarvestReport;
  begin
  reportsListBox.clear;
  if Domain.harvestManager.harvestReportList.count > 0 then
    begin
    for i := 0 to Domain.harvestManager.harvestReportList.count - 1 do
      begin
      newReport := GsHarvestReport.create;
      report := GsHarvestReport(Domain.harvestManager.harvestReportList.items[i]);
      report.copyTo(newReport);
      newReport.originalIfCopy := report;
      reportsListBox.items.addObject(newReport.name, newReport);
      end;
    reportsListBox.itemIndex := 0;
    end;
  if Domain.harvestManager.monthToResetAllReports = -1 then
    resetReportsWhatMonth.itemIndex := resetReportsWhatMonth.items.count - 1
  else
    resetReportsWhatMonth.itemIndex := Domain.harvestManager.monthToResetAllReports;
  resetReportsHowManyYears.itemIndex := Domain.harvestManager.afterHowManyYearsToResetAllReports - 1;
  end;

{ ------------------------------------------------------------------------------------------------- saving }
procedure THarvestReportEditorForm.saveHarvestReportsToHarvestManager;
  var
    i: longint;
    toRemoveList: TList;
    realList: TListCollection;
    reportInList, reportInManager: GsHarvestReport;
  begin
  { check if any reports have been deleted from list box;
    if so, delete them from harvest manager. To do this,
    use report inUse flags to say they are present in list box;
    those whose flags are not set should be removed }
  realList := Domain.harvestManager.harvestReportList;
  if realList = nil then exit;
  if realList.count > 0 then
    begin
    toRemoveList := TList.create;
    for i := 0 to realList.count - 1 do
      GsHarvestReport(realList.items[i]).inUse := false;
    if reportsListBox.items.count > 0 then
      for i := 0 to reportsListBox.items.count - 1 do
        begin
        reportInList := GsHarvestReport(reportsListBox.items.objects[i]);
        if reportInList = nil then continue;
        reportInManager := GsHarvestReport(reportInList.originalIfCopy);
        if reportInManager = nil then continue;
        reportInManager.inUse := true;
        end;
    { two step process to avoid changing list as traverse it }
    for i := 0 to realList.count - 1 do
      begin
      reportInManager := GsHarvestReport(realList.items[i]);
      if not reportInManager.inUse then toRemoveList.add(reportInManager);
      end;
    { now remove unused (deleted) reports from harvest manager }
    if toRemoveList.count > 0 then
      for i := 0 to toRemoveList.count - 1 do
        realList.remove(toRemoveList.items[i]);
    toRemoveList.free;
    toRemoveList := nil;
    end;
  { now copy the contents of all reports in list box to harvest manager versions }
  if reportsListBox.items.count > 0 then
    for i := 0 to reportsListBox.items.count - 1 do
      begin
      reportInList := GsHarvestReport(reportsListBox.items.objects[i]);
      if reportInList = nil then continue;
      reportInManager := GsHarvestReport(reportInList.originalIfCopy);
      { if new report, make new one in harvest manager }
      if reportInManager = nil then
        begin
        reportInManager := GsHarvestReport.create;
        reportInList.originalIfCopy := reportInManager;
        realList.add(reportInManager);
        end;
      reportInList.copyTo(reportInManager);
      end;
  if resetReportsWhatMonth.itemIndex = resetReportsWhatMonth.items.count - 1 then
    Domain.harvestManager.monthToResetAllReports := -1
  else
    Domain.harvestManager.monthToResetAllReports := resetReportsWhatMonth.itemIndex;
  Domain.harvestManager.afterHowManyYearsToResetAllReports := resetReportsHowManyYears.itemIndex + 1;
  end;

{ ------------------------------------------------------------------------------------------------- updating }
function THarvestReportEditorForm.currentReport: GsHarvestReport;
  begin
  { CFK FIX - other dialogs might not be checking if there is anything in list in similar functions }
  if reportsListBox.items.count = 0 then
    result := nil
  else if reportsListBox.itemIndex <> -1 then
    result := GsHarvestReport(reportsListBox.items.objects[reportsListBox.itemIndex])
  else
    result := nil;
  end;

procedure THarvestReportEditorForm.updateInfoForCurrentHarvestReport;
  var
    i: longint;
    theReport: GsHarvestReport;
  begin
  theReport := self.currentReport;
  if theReport = nil then exit;
  { show choices }
  showChoicesListBox.clear;
  for i := 0 to kHarvestReportShowLastChoice do
    begin
    if theReport.showChoicesArray[i] = kHarvestReportShowEndOfList then break;
    showChoicesListBox.items.add(
      Domain.harvestManager.harvestItemTemplateInfoArrayDescription(theReport.showChoicesArray[i]));
    end;
  { show possible choices }
  showPossibleChoicesListBox.clear;
  for i := 0 to kHarvestReportShowLastChoice do
    if not reportShowsIndex(theReport, i) then
      begin
      showPossibleChoicesListBox.items.add(
        Domain.harvestManager.harvestItemTemplateInfoArrayDescription(i));
      showPossibleChoicesListBoxIndexArray[showPossibleChoicesListBox.items.count - 1] := i;
      end;
  { sort by choices }
  sortByChoicesListBox.clear;
  for i := 0 to kHarvestReportSortByLastChoice do
    begin
    if theReport.sortByChoicesArray[i] = kHarvestReportSortEndOfList then break;
    sortByChoicesListBox.items.add(
      Domain.harvestManager.sortByChoicesArrayDescription(theReport.sortByChoicesArray[i]));
    end;
  { sort by possible choices }
  sortByPossibleChoicesListBox.clear;
  for i := 0 to kHarvestReportSortByLastChoice do
    if not reportSortsByIndex(theReport, i) then
      begin
      sortByPossibleChoicesListBox.items.add(
        Domain.harvestManager.sortByChoicesArrayDescription(i));
      sortByPossibleChoicesListBoxIndexArray[sortByPossibleChoicesListBox.items.count - 1] := i;
      end;
  end;

{ ------------------------------------------------------------------------------------------------- actions }
{ note we don't bother to add/remove items from list boxes; just redo them after finished.
  this is so the order in the possible list box is not changed. }
procedure THarvestReportEditorForm.showAddClick(Sender: TObject);
  var
    i, firstUnusedIndex: smallint;
    theReport: GsHarvestReport;
  begin
  if (self.currentReport = nil) or (showPossibleChoicesListBox.itemIndex < 0)
    or (showPossibleChoicesListBox.items.count = 0) then exit;
  theReport := self.currentReport;
  for i := 0 to showPossibleChoicesListBox.items.count - 1 do
    begin
    if showPossibleChoicesListBox.selected[i] then
      if showChoicesListBox.items.indexOf(showPossibleChoicesListBox.items[i]) = -1 then
      begin
      firstUnusedIndex := firstUnusedShowIndexForReport(theReport);
      if (firstUnusedIndex < 0) or (firstUnusedIndex > kHarvestReportShowLastChoice) then
        raise Exception.create('THarvestReportEditorForm.showAddClick index out of bounds');
      theReport.showChoicesArray[firstUnusedIndex] := showPossibleChoicesListBoxIndexArray[i];
      if firstUnusedIndex < kHarvestReportShowLastChoice then
        theReport.showChoicesArray[firstUnusedIndex + 1] := kHarvestReportShowEndOfList;
      end;
    end;
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.showRemoveClick(Sender: TObject);
  var
    i: smallint;
  begin
  if (self.currentReport = nil) or (showChoicesListBox.itemIndex < 0)
    or (showChoicesListBox.items.count = 0) then exit;
  for i := 0 to showChoicesListBox.items.count - 1 do
    if showChoicesListBox.selected[i] then
      self.currentReport.showChoicesArray[i] := kHarvestReportShowEndOfList;
  cleanUpRemovedItemsInReportShowList(self.currentReport);
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.showMoveUpClick(Sender: TObject);
  var
    i: smallint;
  begin
  if showChoicesListBox.itemIndex < 0 then exit;
  if showChoicesListBox.items.count > 1 then
    begin
    { i starts at 1 because you can't move first one up any more }
    i := 1;
    while i <= showChoicesListBox.items.count - 1 do
      begin
      if showChoicesListBox.selected[i] then
        begin
        showChoicesListBox.items.move(i, i - 1);
        showChoicesListBox.selected[i - 1] := true;
        switchShowArrayValuesInReport(self.currentReport, i, i - 1);
        end;
      inc(i);
      end;
    end;
  end;

procedure THarvestReportEditorForm.showMoveDownClick(Sender: TObject);
  var
    i: smallint;
  begin
  if showChoicesListBox.itemIndex < 0 then exit;
  if showChoicesListBox.items.count > 1 then
    begin
    { i starts at next to last one because you can't move last one down any more }
    i := showChoicesListBox.items.count - 2;
    while i >= 0 do
      begin
      if showChoicesListBox.selected[i] then
        begin
        showChoicesListBox.items.move(i, i + 1);
        showChoicesListBox.selected[i+1] := true;
        switchShowArrayValuesInReport(self.currentReport, i, i + 1);
        end;
      dec(i);
      end;
    end;
  end;

procedure THarvestReportEditorForm.sortByAddClick(Sender: TObject);
  var
    i, firstUnusedIndex: smallint;
    theReport: GsHarvestReport;
  begin
  if (self.currentReport = nil) or (sortByPossibleChoicesListBox.itemIndex < 0)
    or (sortByPossibleChoicesListBox.items.count = 0) then exit;
  theReport := self.currentReport;
  for i := 0 to sortByPossibleChoicesListBox.items.count - 1 do
    begin
    if sortByPossibleChoicesListBox.selected[i] then
      if sortByChoicesListBox.items.indexOf(sortByPossibleChoicesListBox.items[i]) = -1 then
      begin
      firstUnusedIndex := firstUnusedSortByIndexForReport(theReport);
      if (firstUnusedIndex < 0) or (firstUnusedIndex > kHarvestReportSortByLastChoice) then
        raise Exception.create('THarvestReportEditorForm.sortByAddClick index out of bounds');
      theReport.sortByChoicesArray[firstUnusedIndex] := sortByPossibleChoicesListBoxIndexArray[i];
      if firstUnusedIndex < kHarvestReportSortByLastChoice then
        theReport.sortByChoicesArray[firstUnusedIndex + 1] := kHarvestReportSortEndOfList;
      end;
    end;
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.sortByRemoveClick(Sender: TObject);
  var
    i: smallint;
  begin
  if (self.currentReport = nil) or (sortByChoicesListBox.itemIndex < 0)
    or (sortByChoicesListBox.items.count = 0) then exit;
  for i := 0 to sortByChoicesListBox.items.count - 1 do
    if sortByChoicesListBox.selected[i] then
      self.currentReport.sortByChoicesArray[i] := kHarvestReportSortEndOfList;
  cleanUpRemovedItemsInReportSortByList(self.currentReport);
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.sortByMoveUpClick(Sender: TObject);
  var
    i: smallint;
  begin
  if sortByChoicesListBox.itemIndex < 0 then exit;
  if sortByChoicesListBox.items.count > 1 then
    begin
    { i starts at 1 because you can't move first one up any more }
    i := 1;
    while i <= sortByChoicesListBox.items.count - 1 do
      begin
      if sortByChoicesListBox.selected[i] then
        begin
        sortByChoicesListBox.items.move(i, i - 1);
        sortByChoicesListBox.selected[i-1] := true;
        switchSortByArrayValuesInReport(self.currentReport, i, i - 1);
        end;
      inc(i);
      end;
    end;
  end;

procedure THarvestReportEditorForm.sortByMoveDownClick(Sender: TObject);
  var
    i: smallint;
  begin
  if sortByChoicesListBox.itemIndex < 0 then exit;
  if sortByChoicesListBox.items.count > 1 then
    begin
    { i starts at next to last one because you can't move last one down any more }
    i := sortByChoicesListBox.items.count - 2;
    while i >= 0 do
      begin
      if sortByChoicesListBox.selected[i] then
        begin
        sortByChoicesListBox.items.move(i, i + 1);
        sortByChoicesListBox.selected[i+1] := true;
        switchSortByArrayValuesInReport(self.currentReport, i, i + 1);
        end;
      dec(i);
      end;
    end;
  end;

procedure THarvestReportEditorForm.setSelectedReport(aReport: GsHarvestReport);
  var
    i: longint;
    reportInList, reportInManager: GsHarvestReport;
  begin
  if aReport = nil then exit;
  if reportsListBox.items.count <= 0 then exit;
  for i := 0 to reportsListBox.items.count - 1 do
    begin
    reportInList := GsHarvestReport(reportsListBox.items.objects[i]);
    if reportInList = nil then continue;
    reportInManager := GsHarvestReport(reportInList.originalIfCopy);
    if reportInManager = nil then continue;
    if reportInManager = aReport then
      reportsListBox.itemIndex := reportsListBox.items.indexOfObject(reportInList);
    end;
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.newReportClick(Sender: TObject);
  var
    newReport: GsHarvestReport;
{$IFDEF WINDOWS}
    nameString: string;
{$ELSE}
    nameString: ansistring;
{$ENDIF}
  begin
  nameString := 'New report';
  if inputQuery('New harvest report', 'Enter a name for the new report', nameString) then
    begin
    newReport := GsHarvestReport.create;
    newReport.originalIfCopy := nil;
    newReport.name := nameString;
    newReport.inUse := false;
    reportsListBox.items.addObject(newReport.name, newReport);
    reportsListBox.itemIndex := reportsListBox.items.count - 1;
    reportsListBox.invalidate;
    self.updateInfoForCurrentHarvestReport;
    end;
  end;

procedure THarvestReportEditorForm.copyReportClick(Sender: TObject);
  var
    newReport: GsHarvestReport;
{$IFDEF WINDOWS}
    nameString: string;
{$ELSE}
    nameString: ansistring;
{$ENDIF}
  begin
  if self.currentReport = nil then exit;
  nameString := self.currentReport.name + ' copy';
  if inputQuery('Copy harvest report', 'Enter a name for the report copy', nameString) then
    begin
    newReport := GsHarvestReport.create;
    self.currentReport.copyTo(newReport);
    newReport.originalIfCopy := nil;
    newReport.name := nameString;
    newReport.inUse := false;
    reportsListBox.items.addObject(newReport.name, newReport);
    reportsListBox.itemIndex := reportsListBox.items.count - 1;
    reportsListBox.invalidate;
    self.updateInfoForCurrentHarvestReport;
    end;
  end;

procedure THarvestReportEditorForm.deleteReportClick(Sender: TObject);
  var
    savedItemIndex: longint;
  begin
  if (reportsListBox.itemIndex < 0) or (reportsListBox.items.count < 0) then exit;
  if messageDlg('Delete the selected report?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
  savedItemIndex := reportsListBox.itemIndex;
  reportsListBox.items.delete(reportsListBox.itemIndex);
  if reportsListBox.items.count > 0 then
    begin
    if savedItemIndex > reportsListBox.items.count - 1 then
      reportsListBox.itemIndex := reportsListBox.items.count - 1
    else
      reportsListBox.itemIndex := savedItemIndex;
    end;
  end;

procedure THarvestReportEditorForm.CancelClick(Sender: TObject);
  begin
  if harvestListHasBeenReset then
    self.undoResetAllReportsCommand;
  modalResult := mrCancel;
  end;

procedure THarvestReportEditorForm.OKClick(Sender: TObject);
  begin
  self.saveHarvestReportsToHarvestManager;
  modalResult := mrOK;
  end;

procedure THarvestReportEditorForm.importReportsClick(Sender: TObject);
  var
    i: smallint;
    fileNameWithPath, ignore: string;
    inputFile: TextFile;
    report: GsHarvestReport;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTabbedText, 'reports.tab');
  if fileNameWithPath <> '' then
    begin
    try
	    assignFile(inputFile, fileNameWithPath);
      reset(inputFile);
      readln(inputFile, ignore); { skip over labels line }
      while not eof(inputFile) do
        begin
        report := GsHarvestReport.create;
        report.name := readUntilTab(inputFile);
        for i := 0 to kHarvestReportShowLastChoice do
          begin
          read(inputFile, report.showChoicesArray[i]);
          ignore := readUntilTab(inputFile);
          end;
        for i := 0 to kHarvestReportSortByLastChoice do
          begin
          read(inputFile, report.sortByChoicesArray[i]);
          ignore := readUntilTab(inputFile);
          end;
        reportsListBox.items.addObject(report.name, report);
        end;
    finally
      closeFile(inputFile);
    end;
    end;
  end;

procedure THarvestReportEditorForm.exportReportsClick(Sender: TObject);
  var
    i, j, index: smallint;
    fileInfo: SaveFileNamesStructure;
    outputFile: TextFile;
    report: GsHarvestReport;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, 'reports.tab', fileInfo) then exit;
  try
    assignFile(outputFile, fileInfo.tempFile);
    rewrite(outputFile);
    { write labels at top }
    write(outputFile, 'report name' + chr(9));
    for i := 0 to kHarvestReportShowLastChoice do write(outputFile, 'Show ' + intToStr(i) + chr(9));
    for i := 0 to kHarvestReportSortByLastChoice do
      begin
      write(outputFile, 'Sort by ' + intToStr(i));
      if i <> kHarvestReportSortByLastChoice then write(outputFile, chr(9));
      end;
    writeln(outputFile);
    if reportsListBox.items.count > 0 then
      for i := 0 to reportsListBox.items.count - 1 do
        begin
        report := GsHarvestReport(reportsListBox.items.objects[i]);
        if report = nil then continue;
        write(outputFile, report.name);
        write(outputFile, chr(9));
        for j := 0 to kHarvestReportShowLastChoice do
          begin
          index := report.showChoicesArray[j];
          write(outputFile,
            intToStr(index) + ' ' + Domain.harvestManager.harvestItemTemplateInfoArrayDescription(index));
          write(outputFile, chr(9));
          end;
        for j := 0 to kHarvestReportSortByLastChoice do
          begin
          index := report.sortByChoicesArray[j];
          write(outputFile,
            intToStr(index) + ' ' + Domain.harvestManager.sortByChoicesArrayDescription(index));
          if j <> kHarvestReportSortByLastChoice then write(outputFile, chr(9));
          end;
        writeln(outputFile);
        end;
      fileInfo.writingWasSuccessful := true;
  finally
    closeFile(outputFile);
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure THarvestReportEditorForm.reportsListBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.reportsListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.updateInfoForCurrentHarvestReport;
  end;

procedure THarvestReportEditorForm.renameReportClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
  newString: string;
{$ELSE}
  newString: ansistring;
{$ENDIF}
    clickedOK: Boolean;
  begin
  if self.currentReport <> nil then
  	begin
    newString := self.currentReport.name;
    clickedOK := InputQuery('Rename harvest report ',
      'Enter new name for harvest report', newString);
    if clickedOK then
     	begin
		  self.currentReport.name := newString;
      reportsListBox.items[reportsListBox.itemIndex] := self.currentReport.name;
      self.updateInfoForCurrentHarvestReport;
      end;
    end;
  end;

procedure THarvestReportEditorForm.reportsListBoxDblClick(Sender: TObject);
  begin
  self.renameReportClick(self);
  end;

procedure THarvestReportEditorForm.resetReportsWhatMonthChange(
  Sender: TObject);
  begin
  if resetReportsWhatMonth.itemIndex = resetReportsWhatMonth.items.count - 1 then
    begin
    everyLabel.visible := false;
    resetReportsHowManyYears.visible := false;
    end
  else
    begin
    everyLabel.visible := true;
    resetReportsHowManyYears.visible := true;
    end;
  end;

procedure THarvestReportEditorForm.resetAllReportsNowClick(
  Sender: TObject);
  begin
  if not harvestListHasBeenReset then
    begin
    resetHarvestListCommand := GsResetHarvestListCommand.createCommand(
      Domain.harvestManager.harvestItemList);
    GardenForm.doCommand(resetHarvestListCommand);
    resetAllReportsNow.caption := 'Undo reset';
    harvestListHasBeenReset := true;
    end
  else
    begin
    self.undoResetAllReportsCommand;
    resetAllReportsNow.caption := 'Reset now';
    harvestListHasBeenReset := false;
    end;
  end;

procedure THarvestReportEditorForm.undoResetAllReportsCommand;
  begin
  if resetHarvestListCommand = nil then exit;
  resetHarvestListCommand.undoCommand;
  { this next line relies on the window being modal, so no other items can be added to
    the command list while the window is up (so this command has to be the last one) }
  GardenForm.undoLast;
  { delete command from the command list }
  GardenForm.commandList.removeCommand(resetHarvestListCommand);
  resetHarvestListCommand.free;
  resetHarvestListCommand := nil;
  end;

procedure THarvestReportEditorForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Harvest_report_editor')
  end;

end.

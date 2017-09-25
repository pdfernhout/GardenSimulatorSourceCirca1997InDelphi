unit Uharvpnl;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uharvpnl: Harvest item panel. Used both in the garden window (at the bottom) and in
the browser right side (under the plant drawing). Note that items harvested from one
plant show up in both the browser and the garden window. This is a fairly simple
panel descendant that draws a quantity of icons in one row with up and down buttons
to show the other rows. The blue bar on the right side of the panel shows how many
rows of icons there are and acts like a little 'progress bar'. The two buttons
are created at run-time as this component is not available at design time. The panel
can have its own harvest list; at this point the garden window lets its harvest
panel use the domain harvest list (since it needs the whole thing), and the browser
fills a harvest list in the harvest panel with only the items for the plant in
question. When another plant is selected the harvest list is cleared and a new
one is made.}

interface

uses WinTypes, WinProcs, Forms, Graphics, SysUtils, ExtCtrls, Classes, Dialogs, Buttons;

type GsHarvestItemPanel = class(TPanel)
	public
  hidden: boolean;
  rowsFromBottom: longint;
  lastHarvestListCount: longint;
  hasOwnList: boolean;
  harvestItemList: TList;
  upButton: TBitBtn;
  downButton: TBitBtn;
  indexToStartInList: smallint;
  iconsInOneRow: smallint;
  constructor create(anOwner: TComponent); override;
  destructor destroy; override;
  procedure doResize;
  procedure paint; override;
  procedure doUpButtonClick(sender: TObject);
  procedure doDownButtonClick(sender: TObject);
  procedure enableOrDisableButtonsIfNeeded;
  procedure useListAsHarvestList(newList: TList);
  function hintForHarvestItemAtCursor(var hintInfo: THintInfo; wantLongHint: boolean): string;
  end;

implementation

uses uharvprt, ugsim, ueutils;

const
  kBetweenGap = 4;

constructor GsHarvestItemPanel.create(anOwner: TComponent);
  begin
  inherited create(anOwner);
  hidden := false;
  rowsFromBottom := 0;
  lastHarvestListCount := 0;
  indexToStartInList := 0;
  iconsInOneRow := 0;
  hasOwnList := true;
  harvestItemList := TList.create;
  upButton := TBitBtn.create(self);
  with upButton do
    begin
    parent := self;
    glyph := GardenForm.harvestPanelUpGlyph.picture.bitmap;
    numGlyphs := 2;
    width := 22;
    height := 14;
    enabled := false;
    onClick := self.doUpButtonClick;
    showHint := true;
    hint := 'Scroll up one row';
    end;
  downButton := TBitBtn.create(self);
  with downButton do
    begin
    parent := self;
    glyph := GardenForm.harvestPanelDownGlyph.picture.bitmap;
    numGlyphs := 2;
    width := 22;
    height := 14;
    enabled := false;
    onClick := self.doDownButtonClick;
    showHint := true;
    hint := 'Scroll down one row';
    end;
  self.showHint := true;
  end;

destructor GsHarvestItemPanel.destroy;
  begin
  if hasOwnList then
    begin
    harvestItemList.free;
    harvestItemList := nil;
    end;
  inherited destroy;
  end;

procedure GsHarvestItemPanel.doUpButtonClick(sender: TObject);
  begin
  inc(self.rowsFromBottom);
  self.enableOrDisableButtonsIfNeeded;
  self.invalidate;
  end;

procedure GsHarvestItemPanel.doDownButtonClick(sender: TObject);
  begin
  dec(self.rowsFromBottom);
  self.enableOrDisableButtonsIfNeeded;
  self.invalidate;
  end;

procedure GsHarvestItemPanel.useListAsHarvestList(newList: TList);
  begin
  if hasOwnList then
    begin
    harvestItemList.free;
    harvestItemList := nil;
    hasOwnList := false;
    end;
  harvestItemList := newList;
  end;

procedure GsHarvestItemPanel.enableOrDisableButtonsIfNeeded;
  var
    iconsInOneRow, tail, numRows: longint;
  begin
  if (harvestItemList = nil) or (self.hidden) then
    begin
    upButton.enabled := false;
    downButton.enabled := false;
    exit;
    end;
  if harvestItemList.count <> lastHarvestListCount then
    rowsFromBottom := 0;
  if harvestItemList.count > 0 then
    begin
    downButton.enabled := rowsFromBottom > 0;
    iconsInOneRow := (upButton.left - kBetweenGap) div 32;
    if iconsInOneRow <= 0 then iconsInOneRow := 1;
    tail := harvestItemList.count mod iconsInOneRow;
    numRows := harvestItemList.count div iconsInOneRow;
    if (tail > 0) and (tail < iconsInOneRow) then inc(numRows);
    if rowsFromBottom > numRows - 1 then
      rowsFromBottom := numRows - 1;
    upButton.enabled := (rowsFromBottom < (numRows - 1));
    end
  else
    begin
    downButton.enabled := false;
    upButton.enabled := false;
    end;
  lastHarvestListCount := harvestItemList.count;
  end;

procedure GsHarvestItemPanel.doResize;
  { this is called explicitly by the user of the panel }
  begin
  upButton.left := self.clientWidth - 8 - kBetweenGap * 2 - upButton.width;
  upButton.top := 2;
  downButton.left := upButton.left;
  downButton.top := self.clientHeight - downButton.height - 2;
  self.enableOrDisableButtonsIfNeeded;
  end;

function GsHarvestItemPanel.hintForHarvestItemAtCursor(var hintInfo: THintInfo; wantLongHint: boolean): string;
  var
    i: smallint;
    harvestItem: GsHarvestItem;
    iconRect: TRect;
  begin
  result := '';
  if iconsInOneRow < 1 then exit;
  for i := 0 to iconsInOneRow - 1 do
    begin
    iconRect := rect(i * 32, 0, i * 32 + 32, 32);
    if ptInRect(iconRect, hintInfo.cursorPos) then
      begin
      if (i + indexToStartInList < 0) or (i + indexToStartInList > harvestItemList.count - 1) then
        exit;
      harvestItem := GsHarvestItem(harvestItemList.items[i + indexToStartInList]);
      if harvestItem <> nil then
        begin
        result := harvestItem.hint(wantLongHint);
        hintInfo.cursorRect := iconRect;
        exit;
        end;
      end;
    end;
  result := 'Harvest panel';
  if wantLongHint then
    result := result + ' with harvested plant parts. To change the information shown in the long hints here, '
      + ' choose another harvest report in the harvest window.';
  end;

procedure GsHarvestItemPanel.paint;
  var
    i, tail, numRows: longint;
    harvestItem: GsHarvestItem;
    drawRect, selectedRect: TRect;
    totalHeight, markerHeight, markerPosition: single;
  begin
  inherited paint;
  if harvestItemList = nil then exit;
  if Application.terminated then exit;
  if self.hidden then exit;
  try
  if harvestItemList.count > 0 then
    begin
    { figure out how many will fit in panel }
    iconsInOneRow := (upButton.left - kBetweenGap) div 32;
    if iconsInOneRow <= 0 then iconsInOneRow := 1;
    tail := harvestItemList.count mod iconsInOneRow;
    indexToStartInList := harvestItemList.count - rowsFromBottom * iconsInOneRow - tail;
    if indexToStartInList = harvestItemList.count then
      indexToStartInList := indexToStartInList - iconsInOneRow;
    if indexToStartInList < 0 then indexToStartInList := 0;
    numRows := harvestItemList.count div iconsInOneRow;
    if (tail > 0) and (tail < iconsInOneRow) then inc(numRows);
    for i := 0 to iconsInOneRow - 1 do
      begin
      if i + indexToStartInList >= harvestItemList.count then break;
      harvestItem := GsHarvestItem(harvestItemList.items[i + indexToStartInList]);
      self.canvas.draw(i * 32, 0, harvestItem.harvestItemTemplate.icon.icon);
      with drawRect do
        begin
        left := upButton.left + upButton.width + kBetweenGap;
        right := self.clientWidth - kBetweenGap;
        top := upButton.top;
        bottom := downButton.top + downButton.height;
        end;
      selectedRect := drawRect;
      totalHeight := selectedRect.bottom - selectedRect.top;
      markerHeight := totalHeight / numRows;
      markerPosition := (numRows - rowsFromBottom - 1) * markerHeight;
      with selectedRect do
        begin
        top := top + round(markerPosition);
        bottom := top + round(markerHeight);
        if bottom > drawRect.bottom then bottom := drawRect.bottom;
        if bottom - top < 1 then top := bottom - 1;
        if top < drawRect.top then top := drawRect.top;
        end;
      with self.canvas do
        begin
        pen.color := clBtnText;
        brush.color := clBtnText;
        brush.style := bsClear;
        pen.style := psSolid;
        rectangle(drawRect.left, drawRect.top, drawRect.right, drawRect.bottom);
        brush.style := bsSolid;
        brush.color := clBlue;
        pen.color := clBlue;
        fillRect(selectedRect);
        end;
      end;
    end;
  except
    ErrorMessage('GsHarvestItemPanel.paint: Problem');
  end;
  end;

end.

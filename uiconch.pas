unit Uiconch;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uiconch: Icon chooser form. Used to set the icon for a harvest item template (HIT).
Called only from the HIT editor. Like the HIT editor and the harvest report editor,
makes a copy of the list of icons (in the template manager) at startup and checks
the copy against the original list when OK is pressed. This allows the user to
cancel all changes in the form. Otherwise fairly simple; user can pick a different
icon, or import or export to an icon file (.ico). There is no icon editor.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, uicon, ugsform;

type
  TIconChooserForm = class(GsForm)
    Save: TButton;
    Cancel: TButton;
    deleteIcon: TButton;
    helpButton: TBitBtn;
    iconsList: TListBox;
    importIcon: TButton;
    exportIcon: TButton;
    renameIcon: TButton;
    procedure copyIconClick(Sender: TObject);
    procedure iconsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SaveClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure deleteIconClick(Sender: TObject);
    procedure importIconClick(Sender: TObject);
    procedure exportIconClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure iconsListClick(Sender: TObject);
    procedure renameIconClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    destructor destroy; override;
    procedure loadIconsFromTemplateManager;
    procedure saveIconsToTemplateManager;
    procedure updateInfoForCurrentIcon;
    function currentIcon: GsIcon;
    procedure setSelectedIcon(anIcon: GsIcon);
  end;

implementation

{$R *.DFM}

uses udomain, uharved, usupport;

procedure TIconChooserForm.FormCreate(Sender: TObject);
  begin
  self.loadIconsFromTemplateManager;
  end;

destructor TIconChooserForm.destroy;
  var
    i: longint;
    iconInList: GsIcon;
  begin
  { delete all copies of icons in list box }
  if iconsList.items.count > 0 then
    for i := 0 to iconsList.items.count - 1 do
    begin
    iconInList := GsIcon(iconsList.items.objects[i]);
    iconInList.free;
    iconInList := nil;
    end;
  inherited destroy;
  end;

procedure TIconChooserForm.loadIconsFromTemplateManager;
  var
    i: longint;
    icon, newIcon: GsIcon;
  begin
  { ask template manager to set in use flags to use in deletion }
  Domain.templateManager.setInUseFlagsForIconList(true);
  iconsList.clear;
  if Domain.templateManager.iconList.count > 0 then
    begin
    for i := 0 to Domain.templateManager.iconList.count - 1 do
      begin
      icon := GsIcon(Domain.templateManager.iconList.items[i]);
      newIcon := GsIcon.create;
      newIcon.copyFrom(icon);
      newIcon.originalIfCopy := icon;
      iconsList.items.addObject(newIcon.getName, newIcon);
      end;
    iconsList.itemIndex := 0;
    self.updateInfoForCurrentIcon;
    end;
  end;

procedure TIconChooserForm.saveIconsToTemplateManager;
  var
    i: longint;
    toRemoveList: TList;
    iconInList, iconInManager: GsIcon;
  begin
  { check if any icons have been deleted from list box;
    if so, delete them from template manager. To do this,
    use icon inUse flags to say they are present in list box;
    those whose flags are not set should be removed }
  toRemoveList := TList.create;
  Domain.templateManager.setAllIconInUseFlagsToFalse;
  if iconsList.items.count > 0 then
    for i := 0 to iconsList.items.count - 1 do
      begin
      iconInList := GsIcon(iconsList.items.objects[i]);
      if iconInList = nil then continue;
      iconInManager := GsIcon(iconInList.originalIfCopy);
      if iconInManager = nil then continue;
      iconInManager.inUse := true;
      end;
  { two step process to avoid changing list as traverse it }
  if Domain.templateManager.iconList.count > 0 then
    for i := 0 to Domain.templateManager.iconList.count - 1 do
      begin
      iconInManager := GsIcon(Domain.templateManager.iconList.items[i]);
      if not iconInManager.inUse then toRemoveList.add(iconInManager);
      end;
  { now remove unused (deleted) harvest item templates from template manager }
  if toRemoveList.count > 0 then
    for i := 0 to toRemoveList.count - 1 do
      Domain.templateManager.iconList.remove(toRemoveList.items[i]);
  toRemoveList.free;
  toRemoveList := nil;
  { now copy the contents of all HITs in list box to template manager versions }
  if iconsList.items.count > 0 then
    for i := 0 to iconsList.items.count - 1 do
      begin
      iconInList := GsIcon(iconsList.items.objects[i]);
      if iconInList = nil then continue;
      iconInManager := GsIcon(iconInList.originalIfCopy);
      { if new template, make new one in template manager }
      if iconInManager = nil then
        begin
        iconInManager := GsIcon.create;
        iconInList.originalIfCopy := iconInManager;
        Domain.templateManager.iconList.add(iconInManager);
        end;
      iconInManager.copyFrom(iconInList);
      end;
  { tell owner (which is assumed to be harvest item template editor form) what
    selected pointer is in template manager. Note that this must be done last so we know what the
    icon's counterpart is in the template manager even if it was created just now. }
  (owner as THarvestItemTemplateEditorForm).currentHarvestItemTemplate.icon :=
    GsIcon(self.currentIcon.originalIfCopy);
  end;

procedure TIconChooserForm.updateInfoForCurrentIcon;
  begin
  deleteIcon.enabled := (self.currentIcon <> nil) and (not self.currentIcon.inUse);
  iconsList.invalidate;
  end;

function TIconChooserForm.currentIcon: GsIcon;
  begin
  if iconsList.itemIndex <> -1 then
    result := GsIcon(iconsList.items.objects[iconsList.itemIndex])
  else
    result := nil;
  end;

procedure TIconChooserForm.setSelectedIcon(anIcon: GsIcon);
  var
    i: longint;
    iconInList, iconInManager: GsIcon;
  begin
  if anIcon = nil then exit;
  if iconsList.items.count <= 0 then exit;
  for i := 0 to iconsList.items.count - 1 do
    begin
    iconInList := GsIcon(iconsList.items.objects[i]);
    if iconInList = nil then continue;
    iconInManager := GsIcon(iconInList.originalIfCopy);
    if iconInManager = nil then continue;
    if iconInManager = anIcon then
      iconsList.itemIndex := iconsList.items.indexOfObject(iconInList);
    self.updateInfoForCurrentIcon;
    end;
  end;

procedure TIconChooserForm.copyIconClick(Sender: TObject);
  var
    newIcon: GsIcon;
{$IFDEF WINDOWS}
    nameString: string;
{$ELSE}
    nameString: ansistring;
{$ENDIF}
  begin
  if self.currentIcon = nil then exit;
  nameString := self.currentIcon.getName;
  nameString[1] := 'c';
  nameString[2] := '_';
  if inputQuery('New icon', 'Enter a name for the new icon', nameString) then
    begin
    newIcon := GsIcon.create;
    newIcon.copyFrom(self.currentIcon);
    newIcon.originalIfCopy := nil;
    newIcon.setName(nameString);
    newIcon.inUse := false;
    iconsList.items.addObject(newIcon.getName, newIcon);
    iconsList.itemIndex := iconsList.items.count - 1;
    self.updateInfoForCurrentIcon;
    end;
  end;

procedure TIconChooserForm.deleteIconClick(Sender: TObject);
  var
    theIcon: GsIcon;
    savedItemIndex: longint;
  begin
  if messageDlg('Delete the selected icon?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
  { can only delete icon if it has no references }
  { this button should be disabled if an icon has any references (is in use) }
  { free, but delete only from list box; when close dialog with OK, will update template manager }
  if (iconsList.itemIndex >= 0) and (iconsList.itemIndex <= iconsList.items.count - 1) then
    begin
    theIcon := GsIcon(iconsList.items.objects[iconsList.itemIndex]);
    savedItemIndex := iconsList.itemIndex;
    iconsList.items.delete(iconsList.itemIndex);
    theIcon.free;
    theIcon := nil;
    if iconsList.items.count > 0 then
      begin
      if savedItemIndex > iconsList.items.count - 1 then
        iconsList.itemIndex := iconsList.items.count - 1
      else
        iconsList.itemIndex := savedItemIndex;
      end;
    self.updateInfoForCurrentIcon;
    end;
  end;

procedure TIconChooserForm.importIconClick(Sender: TObject);
  var
    fileNameWithPath: string;
    newIcon: GsIcon;
    notInList: boolean;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
  begin
  newIcon := nil;
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeIcon, kNoSuggestedFile);
  if fileNameWithPath <> '' then
    begin
    newIcon := GsIcon.create;
    newIcon.setName(lowerCase(extractFileName(fileNameWithPath)));
    try
      newIcon.icon.loadFromFile(fileNameWithPath);
    except
      showMessage('Problem loading icon');
      newIcon.free;
      newIcon := nil;
      exit;
    end;
    { this doesn't seem to be necessary as TIcon converts every icon to 32x32, for some reason,
      but I'll leave it in just in case anyway }
    if (newIcon.icon.width <> 32) or (newIcon.icon.height <> 32) then
      begin
      messageDlg('The icon you have chosen is not a 32x32 pixel icon. No other size icon can be imported.',
        mtError, [mbOK], 0);
      newIcon.free;
      newIcon := nil;
      exit;
      end;
    newName := newIcon.getName;
    notInList := iconsList.items.indexOf(newName) < 0;
    while not notInList do
      begin
      if inputQuery('Rename duplicate',
            'The name of this icon is the same' + chr(13)
          + 'as one already in the list. Please' + chr(13)
          + 'rename it or click Cancel to stop the' + chr(13)
          + 'import.', newName) then
        begin
        newIcon.setName(newName);
        notInList := iconsList.items.indexOf(newName) < 0;
        end
      else
        begin
        newIcon.free;
        newIcon := nil;
        exit;
        end;
      end;
    newIcon.originalIfCopy := nil;
    newIcon.inUse := false;
    iconsList.items.addObject(newIcon.getName, newIcon);
    iconsList.itemIndex := iconsList.items.count - 1;
    self.updateInfoForCurrentIcon;
    end;
  end;

procedure TIconChooserForm.renameIconClick(Sender: TObject);
  var
    icon: GsIcon;
    savedItemIndex: longint;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
  begin
  icon := self.currentIcon;
  if icon = nil then exit;
  newName := icon.getName;
  if not inputQuery('Enter new name', 'Enter a new name for this icon.', newName) then exit;
  icon.setName(newName);
  savedItemIndex := iconsList.itemIndex;
  iconsList.items[savedItemIndex] := newName;
  iconsList.itemIndex := savedItemIndex;
  self.updateInfoForCurrentIcon;
  end;

procedure TIconChooserForm.exportIconClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if self.currentIcon <> nil then
    begin
    if not GsFile_GetFileSaveInfo(kFileTypeIcon, kAskForFileName,
        self.currentIcon.getName, fileInfo) then exit;
    try
      self.currentIcon.icon.saveToFile(fileInfo.tempFile);
      fileInfo.writingWasSuccessful := true;
    finally
      GsFile_CleanUpAfterFileSave(fileInfo);
    end;
    end;
  end;

procedure TIconChooserForm.iconsListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    selected: boolean;
    useText: string;
    cText: array[0..255] of Char;
  begin
  if Application.terminated then exit;
  if (iconsList.items.count <= 0) or (index < 0) or (index > iconsList.items.count - 1) then exit;
  selected := odSelected in state;
  with iconsList.canvas do
    begin
    brush.style := bsSolid;
    if selected then
      brush.color := clHighlight
    else
      brush.color := clWindow;
    fillRect(rect);
    brush.style := bsClear;
    font := iconsList.font;
    font.color := clBtnText;
    if GsIcon(iconsList.items.objects[index]).inUse then
      useText := iconsList.items[index]
    else
      useText := iconsList.items[index] + ' (unused)';
    strPCopy(cText, useText);
    draw(rect.left, rect.top, GsIcon(iconsList.items.objects[index]).icon);
    rect.left := rect.left + 3 { margin for text } + 32 + 3;
    drawText(handle, cText, strLen(cText), rect, DT_LEFT);
    rect.left := rect.left - 3 - 32 - 3;
    if selected then drawFocusRect(rect);
    end;
  end;

procedure TIconChooserForm.SaveClick(Sender: TObject);
  begin
  self.saveIconsToTemplateManager;
  modalResult := mrOK;
  end;

procedure TIconChooserForm.CancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

const
  kBetweenGap = 4;

procedure TIconChooserForm.FormResize(Sender: TObject);
  begin
  iconsList.setBounds(kBetweenGap, kBetweenGap,
    self.clientWidth - save.width - kBetweenGap * 3, self.clientHeight - kBetweenGap * 2);
  save.left := self.clientWidth - save.width - kBetweenGap;
  cancel.left := save.left;
  deleteIcon.left := save.left;
  importIcon.left := save.left;
  exportIcon.left := save.left;
  renameIcon.left := save.left;
  helpButton.left := save.left;
  end;

procedure TIconChooserForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Icon_chooser')
  end;

procedure TIconChooserForm.iconsListClick(Sender: TObject);
  begin
  self.updateInfoForCurrentIcon;
  end;

end.

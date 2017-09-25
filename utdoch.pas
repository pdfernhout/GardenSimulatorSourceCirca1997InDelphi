unit Utdoch;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utdoch: 3D object (tdo) chooser form. Opened from browser component for 3D object (ubrowtdo).
Fairly simple, just has owner-draw list box with drawings of tdos, and set of buttons
for import/export, edit, etc. User can choose a tdo from the list for the model
aspect (a drawing plant part tdo). Like harvest item template editor and harvest
report editor, makes copy of list of tdos (from template manager), then when user
clicks OK compares the copy to the original.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, uturt3d, ugsform;

type
  TThreeDObjectChooserForm = class(GsForm)
    Save: TButton;
    Cancel: TButton;
    deleteTdo: TButton;
    helpButton: TBitBtn;
    tdosList: TListBox;
    importTdo: TButton;
    exportTdo: TButton;
    editTdo: TButton;
    renameTdo: TButton;
    procedure tdosListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SaveClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure deleteTdoClick(Sender: TObject);
    procedure importTdoClick(Sender: TObject);
    procedure exportTdoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure editTdoClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure tdosListClick(Sender: TObject);
    procedure renameTdoClick(Sender: TObject);
    procedure tdosListDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    destructor destroy; override;
    procedure loadTdosFromTemplateManager;
    procedure saveTdosToTemplateManager;
    procedure updateInfoForCurrentTdo;
    function currentTdo: KfObject3d;
    procedure setSelectedTdo(aTdo: KfObject3d);
    procedure draw3DObject(drawRect: TRect; tdo: KfObject3d);
  end;

implementation

{$R *.DFM}

uses udomain, ubrowtdo, usupport, ueutils, utdoed;

const
  kTdoSize = 64;

procedure TThreeDObjectChooserForm.FormCreate(Sender: TObject);
  begin
  self.loadTdosFromTemplateManager;
  end;

destructor TThreeDObjectChooserForm.destroy;
  var
    i: longint;
    tdoInList: KfObject3d;
  begin
  { delete all copies of icons in list box }
  if tdosList.items.count > 0 then
    for i := 0 to tdosList.items.count - 1 do
    begin
    tdoInList := KfObject3d(tdosList.items.objects[i]);
    tdoInList.free;
    tdoInList := nil;
    end;
  inherited destroy;
  end;

procedure TThreeDObjectChooserForm.loadTdosFromTemplateManager;
  var
    i: longint;
    tdo, newTdo: KfObject3d;
  begin
  { ask template manager to set in use flags to use in deletion }
  Domain.templateManager.setInUseFlagsForTdoList(true);
  tdosList.clear;
  if Domain.templateManager.tdoList.count > 0 then
    begin
    for i := 0 to Domain.templateManager.tdoList.count - 1 do
      begin
      tdo := KfObject3d(Domain.templateManager.tdoList.items[i]);
      newTdo := KfObject3d.create;
      newTdo.copyFrom(tdo);
      newTdo.originalIfCopy := tdo;
      tdosList.items.addObject(extractFileName(newTdo.getName), newTdo);
      end;
    tdosList.itemIndex := 0;
    self.updateInfoForCurrentTdo;
    end;
  end;

procedure TThreeDObjectChooserForm.saveTdosToTemplateManager;
  var
    i: longint;
    toRemoveList: TList;
    tdoInList, tdoInManager: KfObject3d;
  begin
  { check if any tdos have been deleted from list box;
    if so, delete them from template manager. To do this,
    use tdo inUse flags to say they are present in list box;
    those whose flags are not set should be removed }
  toRemoveList := TList.create;
  Domain.templateManager.setAllTdoInUseFlagsToFalse;
  if tdosList.items.count > 0 then
    for i := 0 to tdosList.items.count - 1 do
      begin
      tdoInList := KfObject3d(tdosList.items.objects[i]);
      if tdoInList = nil then continue;
      tdoInManager := KfObject3d(tdoInList.originalIfCopy);
      if tdoInManager = nil then continue;
      tdoInManager.inUse := true;
      end;
  { two step process to avoid changing list as traverse it }
  if Domain.templateManager.tdoList.count > 0 then
    for i := 0 to Domain.templateManager.tdoList.count - 1 do
      begin
      tdoInManager := KfObject3d(Domain.templateManager.tdoList.items[i]);
      if not tdoInManager.inUse then toRemoveList.add(tdoInManager);
      end;
  { now remove unused (deleted) tdos from template manager }
  if toRemoveList.count > 0 then
    for i := 0 to toRemoveList.count - 1 do
      Domain.templateManager.tdoList.remove(toRemoveList.items[i]);
  toRemoveList.free;
  toRemoveList := nil;
  { now copy the contents of all tdos in list box to template manager versions }
  if tdosList.items.count > 0 then
    for i := 0 to tdosList.items.count - 1 do
      begin
      tdoInList := KfObject3d(tdosList.items.objects[i]);
      if tdoInList = nil then continue;
      tdoInManager := KfObject3d(tdoInList.originalIfCopy);
      { if new tdo, make new one in template manager }
      if tdoInManager = nil then
        begin
        tdoInManager := KfObject3d.create;
        tdoInList.originalIfCopy := tdoInManager;
        Domain.templateManager.tdoList.add(tdoInManager);
        end;
      tdoInManager.copyFrom(tdoInList);
      end;
  { tell owner (which is assumed to be browser form) what
    selected pointer is in template manager. Note that this must be done last so we know what the
    tdo's counterpart is in the template manager even if it was created just now. }
  (owner as KfObject3DBrowserComponent).tdo := KfObject3d(self.currentTdo.originalIfCopy);
  end;

procedure TThreeDObjectChooserForm.updateInfoForCurrentTdo;
  begin
  deleteTdo.enabled := (self.currentTdo <> nil) and (not self.currentTdo.inUse);
  tdosList.invalidate;
  end;

function TThreeDObjectChooserForm.currentTdo: KfObject3d;
  begin
  if tdosList.itemIndex <> -1 then
    result := KfObject3d(tdosList.items.objects[tdosList.itemIndex])
  else
    result := nil;
  end;

procedure TThreeDObjectChooserForm.setSelectedTdo(aTdo: KfObject3d);
  var
    i: longint;
    tdoInList, tdoInManager: KfObject3d;
  begin
  if aTdo = nil then exit;
  if tdosList.items.count <= 0 then exit;
  for i := 0 to tdosList.items.count - 1 do
    begin
    tdoInList := KfObject3d(tdosList.items.objects[i]);
    if tdoInList = nil then continue;
    tdoInManager := KfObject3d(tdoInList.originalIfCopy);
    if tdoInManager = nil then continue;
    if tdoInManager = aTdo then
      tdosList.itemIndex := tdosList.items.indexOfObject(tdoInList);
    self.updateInfoForCurrentTdo;
    end;
  end;

procedure TThreeDObjectChooserForm.deleteTdoClick(Sender: TObject);
  var tdo: KfObject3d;
  begin
  if messageDlg('Delete the selected 3D object?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
  { can only delete tdo if it has no references }
  { this button should be disabled if a tdo has any references (is in use) }
  { free, but delete only from list box; when close dialog with OK, will update template manager }
  if (tdosList.itemIndex >= 0) and (tdosList.itemIndex <= tdosList.items.count - 1) then
    begin
    tdo := KfObject3d(tdosList.items.objects[tdosList.itemIndex]);
    tdosList.items.delete(tdosList.itemIndex);
    tdo.free;
    tdo := nil;
    self.updateInfoForCurrentTdo;
    end;
  end;

procedure TThreeDObjectChooserForm.importTdoClick(Sender: TObject);
  var
    fileNameWithPath: string;
    newTdo: KfObject3d;
    notInList: boolean;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
  begin
  newTdo := nil;
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTdo, kNoSuggestedFile);
  if fileNameWithPath <> '' then
    begin
    try
      newTdo := KfObject3d.fromFile(fileNameWithPath);
    except
      showMessage('Problem loading 3D object');
      newTdo.free;
      newTdo := nil;
      exit;
    end;
    newTdo.setName(lowerCase(extractFileName(fileNameWithPath)));
    newTdo.originalIfCopy := nil;
    newTdo.inUse := false;
    newName := newTdo.getName;
    notInList := tdosList.items.indexOf(newName) < 0;
    while not notInList do
      begin
      if inputQuery('Rename duplicate',
            'The name of this 3D object is the same' + chr(13)
          + 'as one already in the list. Please' + chr(13)
          + 'rename it or click Cancel to stop the' + chr(13)
          + 'import.', newName) then
        begin
        newTdo.setName(newName);
        notInList := tdosList.items.indexOf(newName) < 0;
        end
      else
        begin
        newTdo.free;
        newTdo := nil;
        exit;
        end;
      end;
    tdosList.items.addObject(newTdo.getName, newTdo);
    tdosList.itemIndex := tdosList.items.count - 1;
    self.updateInfoForCurrentTdo;
    end;
  end;

procedure TThreeDObjectChooserForm.renameTdoClick(Sender: TObject);
  var
    tdo: KfObject3d;
    savedItemIndex: longint;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
  begin
  tdo := self.currentTdo;
  if tdo = nil then exit;
  newName := tdo.getName;
  if not inputQuery('Enter new name', 'Enter a new name for this 3D object.', newName) then exit;
  tdo.setName(newName);
  savedItemIndex := tdosList.itemIndex;
  tdosList.items[savedItemIndex] := newName;
  tdosList.itemIndex := savedItemIndex;
  self.updateInfoForCurrentTdo;
  end;

procedure TThreeDObjectChooserForm.exportTdoClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if self.currentTdo = nil then exit;
  if not GsFile_GetFileSaveInfo(kFileTypeTdo, kAskForFileName,
      self.currentTdo.getName, fileInfo) then exit;
  try
    self.currentTdo.saveToFile(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure TThreeDObjectChooserForm.tdosListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    selected: boolean;
    useText: string;
    cText: array[0..255] of Char;
  begin
  if Application.terminated then exit;
  if (tdosList.items.count <= 0) or (index < 0) or (index > tdosList.items.count - 1) then exit;
  selected := odSelected in state;
  with tdosList.canvas do
    begin
    brush.style := bsSolid;
    if selected then
      brush.color := clHighlight
    else
      brush.color := clWindow;
    fillRect(rect);
    brush.style := bsClear;
    font := tdosList.font;
    font.color := clBtnText;
    if KfObject3D(tdosList.items.objects[index]).inUse then
      useText := tdosList.items[index]
    else
      useText := tdosList.items[index] + ' (unused)';
    strPCopy(cText, useText);
    self.draw3DObject(rect, KfObject3d(tdosList.items.objects[index]));
    rect.left := rect.left + 3 { margin for text } + kTdoSize + 3;
    drawText(handle, cText, strLen(cText), rect, DT_LEFT);
    rect.left := rect.left - 3 - kTdoSize - 3;
    if selected then drawFocusRect(rect);
    end;
  end;

procedure TThreeDObjectChooserForm.draw3DObject(drawRect: TRect; tdo: KfObject3d);
  var
    turtle: KfTurtle;
    bitmap: TBitmap;
  begin
  if tdo = nil then exit;
  { set up clipping bitmap }
  bitmap := TBitmap.create;
  bitmap.width := kTdoSize;
  bitmap.height := kTdoSize;
  bitmap.canvas.brush.color := clWhite;
  bitmap.canvas.rectangle(0, 0, bitmap.width, bitmap.height);
  { set up turtle }
  turtle := KfTurtle.defaultStartUsing;
  try
  turtle.drawingSurface.pane := bitmap.canvas;
  with turtle.drawOptions do
    begin
    sortPolygons := true;
    drawLines := true;
    wireFrame := false;
    end;
  turtle.reset; { must be after pane and draw options set }
  turtle.mmPerPixel(1);  
  turtle.drawingSurface.foreColor := clGreen;
  turtle.drawingSurface.backColor := clLime;
  turtle.drawingSurface.lineColor := clBlack;
  turtle.xyz(bitmap.width div 2, bitmap.height, 0);
  try
	  turtle.push;
    tdo.drawWithTurtleScale(turtle, 0.3 { guess });
	  turtle.pop;
    except
	    on EMathError do ErrorMessage('TThreeDObjectChooserForm.draw3DObject: Turtle EMathError');
    end;
  { copy from bitmap to self }
  tdosList.canvas.draw(drawRect.left, drawRect.top, bitmap);
  bitmap.free;
  finally
  KfTurtle.defaultStopUsing;
  turtle := nil;
  end;
  end;

procedure TThreeDObjectChooserForm.SaveClick(Sender: TObject);
  begin
  self.saveTdosToTemplateManager;
  modalResult := mrOK;
  end;

procedure TThreeDObjectChooserForm.CancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

const
  kBetweenGap = 4;

procedure TThreeDObjectChooserForm.FormResize(Sender: TObject);
  begin
  tdosList.setBounds(kBetweenGap, kBetweenGap,
    self.clientWidth - save.width - kBetweenGap * 3, self.clientHeight - kBetweenGap * 2);
  save.left := self.clientWidth - save.width - kBetweenGap;
  cancel.left := save.left;
  deleteTdo.left := save.left;
  importTdo.left := save.left;
  exportTdo.left := save.left;
  editTdo.left := save.left;
  renameTdo.left := save.left;
  helpButton.left := save.left;
  end;

procedure TThreeDObjectChooserForm.editTdoClick(Sender: TObject);
  var
    tdoEditorForm: TtdoEditorForm;
  begin
  if self.currentTdo = nil then exit;
  tdoEditorForm := TtdoEditorForm.create(self);
  if tdoEditorForm = nil then
    raise Exception.create('Could not create 3d object editor window');
  try
    tdoEditorForm.initialize(self.currentTdo);
    if tdoEditorForm.showModal = mrOK then
      begin
      self.invalidate;
      templatesFileMightHaveChanged := true;
      end;
  finally
    tdoEditorForm.free;
    tdoEditorForm := nil;
  end;
  end;

procedure TThreeDObjectChooserForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_3D_object_chooser')
  end;

procedure TThreeDObjectChooserForm.tdosListClick(Sender: TObject);
  begin
  self.updateInfoForCurrentTdo;
  end;

procedure TThreeDObjectChooserForm.tdosListDblClick(Sender: TObject);
  begin
  self.editTdoClick(self);
  end;

end.

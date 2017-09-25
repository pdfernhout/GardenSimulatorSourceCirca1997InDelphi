unit Utooledt;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utooledt: Tool editor. Edits tools and their actions, pictures and sounds. Pictures
are from bitmaps (separate for down, up, and full states); sounds are from wav
files (separate for pick up, holding (loop), put down, starting use, using (loop),
finishing use); actions are chosen from a list of hard-coded possibilities. Each
tool can have any set of actions (including all of them). See utools for a
description of how the tools and tool manager work. The tool editor works
by making a copy of the entire tool manager at startup (from the one in the
domain), then replacing the domain version when OK is pressed. Note that
this uses a lot of memory and resources and could not work correctly if either
is stressed.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, MPlayer, StdCtrls, Menus, ExtCtrls, usound, utools, ubitmap,
  Buttons, ugsform;

type
  TToolEditorForm = class(GsForm)
    OK: TButton;
    Cancel: TButton;
    helpButton: TBitBtn;
    filePanel: TPanel;
    Label3: TLabel;
    toolFile: TEdit;
    toolFileLoad: TButton;
    toolFileSaveAs: TButton;
    toolFileSave: TButton;
    toolChoice: TComboBox;
    toolNew: TButton;
    toolRename: TButton;
    toolCopy: TButton;
    toolDelete: TButton;
    Label4: TLabel;
    toolNotes: TButton;
    actionsGroupBox: TGroupBox;
    actionChoices: TListBox;
    picturesGroupBox: TGroupBox;
    pictureScroll: TScrollBox;
    picture: TImage;
    pictureImport: TButton;
    pictureExport: TButton;
    pictureClear: TButton;
    soundsGroupBox: TGroupBox;
    soundPlay: TBitBtn;
    soundImport: TButton;
    soundExport: TButton;
    soundClear: TButton;
    soundChoice: TListBox;
    Label1: TLabel;
    Label11: TLabel;
    pictureHotX: TEdit;
    Label2: TLabel;
    pictureHotY: TEdit;
    onGroundChoice: TRadioButton;
    inUseChoice: TRadioButton;
    fullWhileUsingChoice: TRadioButton;
    procedure soundImportClick(Sender: TObject);
    procedure soundPlayClick(Sender: TObject);
    procedure toolChoiceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure soundExportClick(Sender: TObject);
    procedure soundClearClick(Sender: TObject);
    procedure pictureImportClick(Sender: TObject);
    procedure pictureExportClick(Sender: TObject);
    procedure pictureClearClick(Sender: TObject);
    procedure pictureHotXChange(Sender: TObject);
    procedure pictureHotYChange(Sender: TObject);
    procedure pictureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pictureMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pictureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OKClick(Sender: TObject);
    procedure toolFileLoadClick(Sender: TObject);
    procedure toolFileSaveAsClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure toolNewClick(Sender: TObject);
    procedure toolCopyClick(Sender: TObject);
    procedure toolDeleteClick(Sender: TObject);
    procedure toolRenameClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure toolFileSaveClick(Sender: TObject);
    procedure actionChoicesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure actionChoicesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure toolNotesClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure actionChoicesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure soundChoiceDblClick(Sender: TObject);
    procedure soundChoiceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure soundChoiceClick(Sender: TObject);
    procedure pictureHotXKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pictureHotYKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure onGroundChoiceClick(Sender: TObject);
    procedure inUseChoiceClick(Sender: TObject);
    procedure fullWhileUsingChoiceClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    newToolManager: GsToolManager;
    currentToolFileName: string;
		function initializeToolDialog(aString: string): longint;  {really copyright check}
    destructor destroy; override;
    function currentTool: GsTool;
    function currentBitmap: GsBitmap;
    function currentSound: GsSound;
    function currentHotSpot: TPoint;
    procedure setCurrentHotSpot(newHotSpot: TPoint);
		procedure updateToolChoice;
		procedure updateToolActions;
		procedure updateSounds;
    procedure addSoundToList(soundString: string; sound: GsSound);
		procedure updatePictures;
		procedure drawHotSpot;
		procedure moveHotSpot(x, y: longint);
    procedure constrainHotSpot;
    procedure toolFileSaveOrSaveAs(askForFileName: boolean);
    procedure fillActionChoicesWithStrings;
  end;

implementation

uses udomain, unotes, usupport, ufiler, ucursor;

{$R *.DFM}

{ ----------------------------------------------------------------------------- create/destroy }
procedure TToolEditorForm.FormCreate(Sender: TObject);
  var copySucessful: boolean;
	begin
	soundChoice.itemIndex := 0;
  onGroundChoice.checked := true;
  {the following string was rotated up one character here - (. to >, z to A, - to +)}
  {'Copyright 1997 Paul D. Fernhout and Cynthia F. Kurtz All Rights Reserved;}
	if self.initializeToolDialog('Dpqzsjhiu 2008 Qbvm E> Ghsoipvu boe Dzouijb G> LvsuA Bmm Sjhiut Sftfswfe')
  	<> -12389 then
    	Halt;
  self.fillActionChoicesWithStrings;
  self.update;
  newToolManager := GsToolManager.create;
  cursor_startWait;
  copySucessful := false;
  try
    copySucessful := Domain.toolManager.specialCopyTo(newToolManager);
  finally
    cursor_stopWait;
  end;
  if not copySucessful then
    begin
    showMessage('Not enough resources to create copies of tools.' + chr(13)
      + 'Close applications to make more resources available.');
    modalResult := mrCancel;
    exit;
    end;
  currentToolFileName := Domain.fileOptions.toolsFileName;
  toolFile.text := lowerCase(currentToolFileName);
 	self.updateToolChoice;
  end;

procedure TToolEditorForm.FormDeactivate(Sender: TObject);
	begin
	newToolManager.free;
	newToolManager := nil;
	end;

destructor TToolEditorForm.destroy;
  begin
  newToolManager.free;
  newToolManager := nil;
  inherited destroy;
  end;

{really a copyright check}
function TToolEditorForm.initializeToolDialog(aString: string): longint;
  var
   	sum: longint;
   	i: integer;
  begin
  sum := 255;
  for i := 1 to length(aString) do
    sum := sum - ord(aString[i]) * 2;
  result := sum;
  end;

procedure TToolEditorForm.fillActionChoicesWithStrings;
  var
    anAction: GsToolAction;
    i: longint;
  begin
  actionChoices.clear;
  anAction := nil;
  anAction := GsToolAction.create;
  if anAction = nil then exit;
  for i := kToolActionFirstAction to kToolActionLastAction do
    begin
    anAction.actionType := i;
    actionChoices.items.add(anAction.name);
    end;
  anAction.free;
  anAction := nil;
  end;

{ ----------------------------------------------------------------------------- ok/cancel }
procedure TToolEditorForm.OKClick(Sender: TObject);
	begin
  toolsFileMightHaveChanged := true;
  try
    newToolManager.gloveTool := nil;
    newToolManager.gloveTool := newToolManager.findGlove;
  except
    showMessage('No glove tool was found. You need to add one to the tools.');
  end;
  if newToolManager.gloveTool = nil then
    begin
    self.modalResult := 0; {keep going}
    exit;
    end;
  Domain.toolManager.free;
  Domain.toolManager := newToolManager;
  Domain.fileOptions.toolsFileName := currentToolFileName;
  newToolManager := nil;
  {need to redraw garden and reload tool options}
  self.modalResult := mrOK;
	end;

procedure TToolEditorForm.CancelClick(Sender: TObject);
	begin
  newToolManager.free;
  newToolManager := nil;
	end;

procedure TToolEditorForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Tool_editor')
  end;

{ ----------------------------------------------------------------------------- updating/current }
procedure TToolEditorForm.updateToolChoice;
  var
    i: longint;
    theTool: GsTool;
  begin
  toolChoice.items.clear;
  if newToolManager.tools.count > 0 then
    begin
    for i := 0 to newToolManager.tools.count - 1 do
      begin
      theTool := GsTool(newToolManager.tools.items[i]);
      toolChoice.items.addObject(theTool.name, theTool);
      end;
    toolChoice.itemIndex := 0;
 		end;
  self.updateToolActions;
  self.updateSounds;
  self.updatePictures;
  end;

procedure TToolEditorForm.updateToolActions;
  begin
  actionChoices.invalidate;
  end;

procedure TToolEditorForm.updateSounds;
  var savedItemIndex: smallint;
  begin
  savedItemIndex := soundChoice.itemIndex;
  soundChoice.items.clear;
  if self.currentTool <> nil then
    begin
    self.addSoundToList('pick up', self.currentTool.soundPickUp);
    self.addSoundToList('holding', self.currentTool.soundMoving);
    self.addSoundToList('put down', self.currentTool.soundPutDown);
    self.addSoundToList('starting use', self.currentTool.soundDown);
    self.addSoundToList('using', self.currentTool.soundUsing);
    self.addSoundToList('finishing use', self.currentTool.soundUp);
    end;
  if (soundChoice.items.count > 0) and (savedItemIndex >= 0) and (savedItemIndex <= soundChoice.items.count - 1) then
    soundChoice.itemIndex := savedItemIndex;
  self.soundChoiceClick(self);
  end;

procedure TToolEditorForm.updatePictures;
  begin
  if (self.currentBitmap <> nil) then
    begin
    picture.width := self.currentBitmap.width;
    picture.height := self.currentBitmap.height;
    picture.picture.assign(self.currentBitmap);
    pictureHotX.text := IntToStr(self.currentHotSpot.x);
    pictureHotY.text := IntToStr(self.currentHotSpot.y);
    self.drawHotSpot;
    { this makes the scroller scroll to where the picture image is in view }
    pictureScroll.scrollInView(picture);
    end
  else
    begin
    picture.width := 0;
    picture.height := 0;
    pictureHotX.text := '';
    pictureHotY.text := '';
    end;
  end;

function TToolEditorForm.currentTool: GsTool;
  begin
  result := nil;
  if toolChoice.itemIndex >= 0 then
    result := GsTool(toolChoice.items.objects[toolChoice.itemIndex]);
  end;

function TToolEditorForm.currentBitmap: GsBitmap;
  begin
  result := nil;
  if (self.currentTool <> nil) then
    begin
    if onGroundChoice.checked then
      result := self.currentTool.downBitmap
    else if inUseChoice.checked then
      result := self.currentTool.upBitmap
    else if fullWhileUsingChoice.checked then
      result := self.currentTool.fullBitmap;
    end;
  end;

function TToolEditorForm.currentSound: GsSound;
  begin
  result := nil;
  if (self.currentTool <> nil) and (soundChoice.itemIndex >= 0) then
    case soundChoice.itemIndex of
      0: result := self.currentTool.soundPickUp;
      1: result := self.currentTool.soundMoving;
      2: result := self.currentTool.soundPutDown;
      3: result := self.currentTool.soundDown;
      4: result := self.currentTool.soundUsing;
      5: result := self.currentTool.soundUp;
    	end;
  end;

function TToolEditorForm.currentHotSpot: TPoint;
  begin
  result := Point(0,0);
  if (self.currentTool <> nil) then
    begin
    if onGroundChoice.checked then
      result := self.currentTool.downHotSpot
    else if inUseChoice.checked then
      result := self.currentTool.upHotSpot
    else if fullWhileUsingChoice.checked then
      result := self.currentTool.fullHotSpot;
    end;
  end;

procedure TToolEditorForm.setCurrentHotSpot(newHotSpot: TPoint);
  begin
  if (self.currentTool <> nil) then
    begin
    if onGroundChoice.checked then
      self.currentTool.downHotSpot := newHotSpot
    else if inUseChoice.checked then
      self.currentTool.upHotSpot := newHotSpot
    else if fullWhileUsingChoice.checked then
      self.currentTool.fullHotSpot := newHotSpot;
    end;
  end;

{ ----------------------------------------------------------------------------- buttons at top }
procedure TToolEditorForm.toolChoiceChange(Sender: TObject);
	begin
  {need to save changed values if changed for previous tool
   note is only one not already changed}
  self.updateToolActions;
  self.updateSounds;
  { when change tool, always to back to down picture - less confusing }
  onGroundChoice.checked := true;
  self.updatePictures;
	end;

procedure TToolEditorForm.toolFileLoadClick(Sender: TObject);
  var
    fileNameWithPath: string;
    tempToolManager: GsToolManager;
    fileNameIsDifferent: boolean;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTools, currentToolFileName);
  if fileNameWithPath <> '' then
    begin
    fileNameIsDifferent := (fileNameWithPath <> currentToolFileName);
    tempToolManager := GsToolManager.create;
    try
  	try
  		cursor_startWait;
    	GsFiler.load(fileNameWithPath, tempToolManager);
  	finally
  		cursor_stopWait;
    end;
    except
    tempToolManager.free;
    raise;
    end;
    newToolManager.free;
    newToolManager := tempToolManager;
    currentToolFileName := fileNameWithPath;
    toolFile.text := lowerCase(currentToolFileName);
    self.updateToolChoice;
    if fileNameIsDifferent then iniFileMightHaveChanged := true;
    end;
  end;

procedure TToolEditorForm.toolFileSaveClick(Sender: TObject);
  begin
  toolFileSaveOrSaveAs(kDontAskForFileName);
  { can turn off this flag if save tool file, because you can ONLY change the tool file in the tool editor }
  toolsFileMightHaveChanged := false;
  end;

procedure TToolEditorForm.toolFileSaveAsClick(Sender: TObject);
  begin
  toolFileSaveOrSaveAs(kAskForFileName);
  { can turn off this flag if save tool file, because you can ONLY change the tool file in the tool editor }
  toolsFileMightHaveChanged := false;
  end;

procedure TToolEditorForm.toolFileSaveOrSaveAs(askForFileName: boolean);
  var
    fileInfo: SaveFileNamesStructure;
    fileNameIsDifferent: boolean;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTools, askForFileName, currentToolFileName, fileInfo) then exit;
  fileNameIsDifferent := fileInfo.newFile <> currentToolFileName;
  try
  	cursor_startWait;
    GsFiler.save(fileInfo.tempFile, newToolManager);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
    currentToolFileName := fileInfo.newFile;
    toolFile.text := lowerCase(currentToolFileName);
  end;
  if fileNameIsDifferent then iniFileMightHaveChanged := true;
  toolsFileMightHaveChanged := false;
  end;

procedure TToolEditorForm.toolNewClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
  	newToolName: string;
{$ELSE}
  	newToolName: ansistring;
{$ENDIF}
    newTool: GsTool;
	begin
  newToolName := 'Unnamed tool';
  if InputQuery('New tool', 'Please enter name for new tool', newToolName) then
    begin
    newTool := GsTool.create;
    newTool.name := newToolName;
   {	newTool.toolType := self.typeChoice.itemIndex + 1;   }
   { CFK FIX }
    newToolManager.tools.add(newTool);
    self.toolChoice.items.addObject(newToolName, newTool);
    self.toolChoice.itemIndex := self.toolChoice.items.count - 1;
    self.toolChoiceChange(self);
    end;
	end;

procedure TToolEditorForm.toolCopyClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
  	newToolName: string;
{$ELSE}
  	newToolName: ansistring;
{$ENDIF}
    newTool: GsTool;
	begin
  if self.currentTool = nil then exit;
  newToolName := self.currentTool.name + ' copy';
  if InputQuery('Copy tool', 'Please enter name for copied tool', newToolName) then
    begin
    newTool := GsTool.create;
    self.currentTool.copyTo(newTool);
    newTool.name := newToolName;
    newToolManager.tools.add(newTool);
    self.toolChoice.items.addObject(newToolName, newTool);
    self.toolChoice.itemIndex := self.toolChoice.items.count - 1;
    self.toolChoiceChange(self);
    end;
	end;

procedure TToolEditorForm.toolDeleteClick(Sender: TObject);
	begin
  if self.currentTool = nil then exit;
  if MessageDlg('Are you sure you want to delete the ' + self.currentTool.name,  mtConfirmation, mbOKCancel, 0) = mrOK then
  	begin
		newToolManager.tools.remove(self.currentTool);
  	self.toolChoice.items.delete(self.toolChoice.itemIndex);
    self.toolChoiceChange(self);
    end;
  end;

procedure TToolEditorForm.toolRenameClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
  	newToolName: string;
{$ELSE}
  	newToolName: ansistring;
{$ENDIF}
    selectedIndex: longint;
	begin
  if self.currentTool = nil then exit;
	newToolName := self.currentTool.name;
  if InputQuery('Rename tool', 'Please enter new name for ' + self.currentTool.name, newToolName) then
    begin
    self.currentTool.name := newToolName;
    selectedIndex := self.toolChoice.itemIndex;
    self.toolChoice.items.strings[self.toolChoice.itemIndex] := newToolName;
    self.toolChoice.itemIndex := selectedIndex;
    end;
	end;

procedure TToolEditorForm.toolNotesClick(Sender: TObject);
  var
    notesForm: TNotesForm;
	begin
  if self.currentTool <> nil then
    begin
    notesForm := TNotesForm.create(self);
    if notesForm = nil then
      raise Exception.create('Could not create notes window');
    try
      notesForm.initialize(kToolNote, self.currentTool, nil, toolChoice.items);
		  notesForm.showModal;
    finally
      notesForm.free;
      notesForm := nil;
    end;
    end;
	end;

{ ----------------------------------------------------------------------------- pictures }
procedure TToolEditorForm.onGroundChoiceClick(Sender: TObject);
  begin
	self.updatePictures;
  end;

procedure TToolEditorForm.inUseChoiceClick(Sender: TObject);
  begin
	self.updatePictures;
  end;

procedure TToolEditorForm.fullWhileUsingChoiceClick(Sender: TObject);
  begin
	self.updatePictures;
  end;

procedure TToolEditorForm.pictureImportClick(Sender: TObject);
  var
    fileNameWithPath: string;
	begin
  if self.currentBitmap <> nil then
    begin
    fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeBitmap, kNoSuggestedFile);
    if fileNameWithPath <> '' then
      begin
      LoadDibFromFileAndPutInBitmap(fileNameWithPath, self.currentBitmap, Domain.paletteBitmap);
      self.currentTool.makeMasks; {wasteful}
      self.constrainHotSpot;
      self.updatePictures;
      end;
    end;
	end;

procedure TToolEditorForm.pictureExportClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if self.currentBitmap <> nil then
    begin
    if not GsFile_GetFileSaveInfo(kFileTypeBitmap, kAskForFileName, 'export.bmp', fileInfo) then exit;
    try
      self.currentBitmap.saveToFile(fileInfo.tempFile);
      fileInfo.writingWasSuccessful := true;
    finally
      GsFile_CleanUpAfterFileSave(fileInfo);
    end;
    end;
  end;

procedure TToolEditorForm.pictureClearClick(Sender: TObject);
	begin
  if self.currentBitmap <> nil then
    begin
    self.currentBitmap.Height := 0;
    self.currentBitmap.Width := 0;
    self.picture.Height := 0;
    self.picture.Width := 0;
    self.constrainHotSpot;
    self.updatePictures;
    end;
	end;

procedure TToolEditorForm.pictureHotXChange(Sender: TObject);
	begin
  if self.currentBitmap <> nil then
    begin
    self.drawHotSpot;
    self.setCurrentHotSpot(Point(StrToIntDef(pictureHotX.text, 0), self.currentHotSpot.y));
    {picture.picture.assign(self.currentBitmap);}
    self.drawHotSpot;
    end;
	end;

procedure TToolEditorForm.pictureHotYChange(Sender: TObject);
	begin
  if self.currentBitmap <> nil then
    begin
    self.drawHotSpot;
    self.setCurrentHotSpot(Point(self.currentHotSpot.x, StrToIntDef(pictureHotY.text, 0)));
    {picture.picture.assign(self.currentBitmap); }
    self.drawHotSpot;
    end;
	end;

procedure TToolEditorForm.pictureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
  self.moveHotSpot(x,y);
	end;

procedure TToolEditorForm.pictureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
	begin
  if ssLeft in shift then
  	self.moveHotSpot(x,y);
	end;

procedure TToolEditorForm.pictureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	begin
  self.moveHotSpot(x,y);
	end;

procedure TToolEditorForm.moveHotSpot(x, y: longint);
  begin
  if self.currentBitmap <> nil then
    begin
    {picture.picture.assign(self.currentBitmap);}
    self.drawHotSpot;
    self.setCurrentHotSpot(Point(x, y));
    self.drawHotSpot;
    pictureHotX.text := IntToStr(self.currentHotSpot.x);
    pictureHotY.text := IntToStr(self.currentHotSpot.y);
    end;
end;

procedure TToolEditorForm.constrainHotSpot;
  var newHotSpot: TPoint;
  begin
  newHotSpot := self.currentHotSpot;
  if newHotSpot.x > self.currentBitmap.width then
    newHotSpot.x := self.currentBitmap.width;
  if newHotSpot.y > self.currentBitmap.height then
    newHotSpot.y := self.currentBitmap.height;
  self.setCurrentHotSpot(newHotSpot);
  end;

const
  kHotspotPenWidth = 1;
  kHotspotLineLength = 6;

procedure TToolEditorForm.drawHotSpot;
  begin
  with picture.canvas do
    begin
    pen.color := clRed;
    pen.mode := pmNot;
    pen.width := kHotspotPenWidth;
    moveTo(self.currentHotSpot.x - kHotspotLineLength, self.currentHotSpot.y);
    lineTo(self.currentHotSpot.x + kHotspotLineLength, self.currentHotSpot.y);
    moveTo(self.currentHotSpot.x, self.currentHotSpot.y - kHotspotLineLength);
    lineTo(self.currentHotSpot.x, self.currentHotSpot.y + kHotspotLineLength);
    end;
  end;

procedure TToolEditorForm.pictureHotXKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  var
    newX, newY: longint;
  begin
  if not (ssCtrl in shift) then exit;
  if (key = VK_LEFT) or (key = VK_RIGHT) then
    begin
    newX := StrToIntDef(pictureHotX.text, 0);
    if key = VK_LEFT then dec(newX) else inc(newX);
    if newX < 0 then newX := 0;
    if newX > self.currentBitmap.width then newX := self.currentBitmap.width;
    pictureHotX.text := intToStr(newX);
    end
  else if (key = VK_UP) or (key = VK_DOWN) then
    begin
    newY := StrToIntDef(pictureHotY.text, 0);
    if key = VK_UP then dec(newY) else inc(newY);
    if newY < 0 then newY := 0;
    if newY > self.currentBitmap.height then newY := self.currentBitmap.height;
    pictureHotY.text := intToStr(newY);
    end;
  end;

procedure TToolEditorForm.pictureHotYKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
  self.pictureHotXKeyUp(sender, key, shift);
  end;

{ ----------------------------------------------------------------------------- sounds }
procedure TToolEditorForm.addSoundToList(soundString: string; sound: GsSound);
  var
    showString: string;
  begin
  showString := soundString;
  if (sound <> nil) and (sound.soundSize > 0) then
    showString := showString + ': ' + lowerCase(extractFileName(sound.fileName));
  soundChoice.items.add(showString);
  end;

procedure TToolEditorForm.soundChoiceClick(Sender: TObject);
  begin
  soundPlay.enabled := (self.currentSound <> nil) and (self.currentSound.soundSize > 0);
  end;

procedure TToolEditorForm.soundChoiceMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if (button = mbRight) or (ssShift in shift) then
    self.soundImportClick(self);
  end;

procedure TToolEditorForm.soundChoiceDblClick(Sender: TObject);
  begin
  if soundPlay.enabled then self.soundPlayClick(self);
  end;

procedure TToolEditorForm.soundImportClick(Sender: TObject);
  var
    fileNameWithPath: string;
  begin
  if self.currentSound <> nil then
    begin
    fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeWave, self.currentSound.fileName);
    if fileNameWithPath <> '' then
      begin
  		self.currentSound.loadFromFile(fileNameWithPath);
      self.updateSounds;
      end;
    end;
	end;

procedure TToolEditorForm.soundExportClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
	begin
  if self.currentSound <> nil then
    begin
    if not GsFile_GetFileSaveInfo(kFileTypeWave, kAskForFileName,
        self.currentSound.fileName, fileInfo) then exit;
    try
      self.currentSound.saveToFile(fileInfo.tempFile);
      fileInfo.writingWasSuccessful := true;
    finally
      GsFile_CleanUpAfterFileSave(fileInfo);
    end;
    end;
	end;

procedure TToolEditorForm.soundPlayClick(Sender: TObject);
	begin
  if (self.currentSound <> nil) then
  	self.currentSound.play;
	end;

procedure TToolEditorForm.soundClearClick(Sender: TObject);
	begin
  if (self.currentSound <> nil) then
    begin
  	self.currentSound.freeSoundMem;
    self.updateSounds;
    end;
	end;

{ ----------------------------------------------------------------------------- tool actions }
procedure TToolEditorForm.actionChoicesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  var
    selected: boolean;
    cText: array[0..255] of Char;
    checkBoxRect, stringRect: TRect;
  begin
  if Application.terminated then exit;
  if (actionChoices.items.count <= 0) or (index < 0) or (index > actionChoices.items.count - 1) then exit;
  selected := odSelected in state;
  with actionChoices.canvas do
    begin
    brush.style := bsSolid;
    if selected then
      brush.color := clHighlight
    else
      brush.color := clWindow;
    fillRect(rect);
    brush.style := bsClear;
    font := actionChoices.font;
    font.color := clBtnText;
    strPCopy(cText, actionChoices.items[index]);
    checkBoxRect := rect;
    inc(checkBoxRect.left);
    checkBoxRect.right := rect.left + (rect.bottom - rect.top) + 1;
    inflateRect(checkBoxRect, -1, -1);
    with checkBoxRect do
      begin
      rectangle(left, top, right, bottom);
      if (self.currentTool <> nil)
          and (self.currentTool.actionForActionType(index + kToolActionFirstAction) <> nil) then
        begin
        moveTo(left, top);
        lineTo(right, bottom);
        moveTo(left, bottom);
        lineTo(right, top);
        end;
      end;
    stringRect := rect;
    stringRect.left := checkBoxRect.right + 2;
    drawText(handle, cText, strLen(cText), stringRect, DT_LEFT);
    if selected then drawFocusRect(rect);
    end;
  end;

procedure TToolEditorForm.actionChoicesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
  { pretend x is in check box }
  if (key = VK_SPACE) then
    self.actionChoicesMouseUp(sender, mbLeft, shift, 0, 0);
  end;

procedure TToolEditorForm.actionChoicesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    anAction: GsToolAction;
  begin
  if x - actionChoices.left > actionChoices.itemHeight then exit;
  if actionChoices.itemIndex < 0 then exit;
  if self.currentTool = nil then exit;
  anAction := self.currentTool.actionForActionType(actionChoices.itemIndex + kToolActionFirstAction);
  if anAction = nil then
    begin
    self.currentTool.actionList.add(
      GsToolAction.createWithActionType(actionChoices.itemIndex + kToolActionFirstAction));
    end
  else
    begin
    self.currentTool.actionList.remove(anAction);
    anAction.free;
    anAction := nil;
    end;
  actionChoices.invalidate;
  end;

end.

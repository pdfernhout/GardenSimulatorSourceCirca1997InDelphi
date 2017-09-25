unit Unotes;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
unotes: Notes editor. Each graphical model (see umodel) descendant has a note which
is a PChar. This includes garden, weather, soil, plant, amendment, and OM blob objects.
Also tools have notes (same type). The notes editor can be opened from the browser
or from the tool editor. If opened from the browser, it shows the notes for all
objects in the garden and templates in the current templates file. If opened from the
tool editor, it shows the notes for all tools in the current tools file. The
form has no cancel ability mainly because we just didn't have time and the notes
aren't especially important. If tool notes are being edited, the Rename button
is hidden because there is already a rename button in the tool editor. Either
set of notes can be exported to a tab-delimited file for use in other programs,
but they cannot be imported (again for lack of time and importance).}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, umodel, Buttons, utools, ExtCtrls, ugsform;

const
  kModelNote = 1;
  kToolNote = 2;

type
  TNotesForm = class(GsForm)
    Notes: TMemo;
    DoneButton: TButton;
    helpButton: TBitBtn;
    objectsList: TListBox;
    ObjectsLabel: TLabel;
    notesLabel: TLabel;
    renameButton: TButton;
    splitter: TPanel;
    exportButton: TButton;
    procedure FormResize(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure objectsListClick(Sender: TObject);
    procedure NotesChange(Sender: TObject);
    procedure NotesExit(Sender: TObject);
    procedure splitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure splitterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure splitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure renameButtonClick(Sender: TObject);
    procedure exportButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    modelOrToolNote: smallint;
    noteHasChanged: boolean;
    splitterDragging : boolean;
    splitterNeedToRedraw : boolean;
    splitterStartPos : integer;
    splitterLastDrawPos : integer;
    lastWidth: integer;
    procedure initialize(aModelOrToolNote: smallint; aTool: GsTool; aModel: GsModel; stringList: TStrings);
    function currentModel: GsGraphicalModel;
    function currentTool: GsTool;
    function drawSplitterLine(pos: integer): integer;
    procedure undrawSplitterLine;
    procedure resizeContentsToSplitter;
  end;

implementation

uses uexcept, usupport, ugsim, ugscom, ucursor, udomain, uesoil, ueplant;

{$R *.DFM}

procedure TNotesForm.initialize(aModelOrToolNote: smallint; aTool: GsTool; aModel: GsModel; stringList: TStrings);
  begin
  modelOrToolNote := aModelOrToolNote;
  if stringList <> nil then objectsList.items.assign(stringList);
  if modelOrToolNote = kModelNote then
    objectsList.itemIndex := objectsList.items.indexOfObject(aModel)
  else
    objectsList.itemIndex := objectsList.items.indexOfObject(aTool);
  self.objectsListClick(self);
  noteHasChanged := false;
  splitter.left := self.clientWidth div 2 - splitter.width div 2;
  lastWidth := self.clientWidth;
  { can't rename tool here }
  renameButton.visible := (modelOrToolNote = kModelNote);
  end;

procedure TNotesForm.objectsListClick(Sender: TObject);
  begin
  if modelOrToolNote = kModelNote then
    begin
    if self.currentModel <> nil then
      transferNoteAndMemo(self.currentModel.note, notes, kGetField);
    end
  else
    begin
    if self.currentTool <> nil then
      transferNoteAndMemo(self.currentTool.note, notes, kGetField);
    end;
  end;

function TNotesForm.currentModel: GsGraphicalModel;
  begin
  result := nil;
  if modelOrToolNote <> kModelNote then exit;
  if (objectsList.items.count <= 0) or (objectsList.itemIndex < 0) then exit;
  result := GsGraphicalModel(objectsList.items.objects[objectsList.itemIndex]);
  end;

function TNotesForm.currentTool: GsTool;
  begin
  result := nil;
  if modelOrToolNote <> kToolNote then exit;
  if (objectsList.items.count <= 0) or (objectsList.itemIndex < 0) then exit;
  result := GsTool(objectsList.items.objects[objectsList.itemIndex]);
  end;

procedure TNotesForm.renameButtonClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
    afterPrefixAnsii, newNameAnsii: string;
{$ELSE}
    afterPrefixAnsii, newNameAnsii: ansistring;
{$ENDIF}
		newName, prefix, afterPrefix: string;
    savedIndex: longint;
    response, isPatchOrPlant: boolean;
    changeNameCommand: GsChangeModelNameCommand;
  begin
  if (modelOrToolNote <> kModelNote) or (self.currentModel = nil) then exit;
  newName := self.currentModel.getName;
  isPatchOrPlant := ((currentModel is GsSoilPatch) or (currentModel is GsPlant)) and (not currentModel.isTemplate);
  response := false;
  if isPatchOrPlant then
    begin
    separateNameIntoPrefixAndAfterPrefix(newName, prefix, afterPrefix);
    afterPrefixAnsii := afterPrefix;
    if currentModel is GsSoilPatch then
      begin
      response := inputQuery('Rename soil patch', 'Please enter a new name for soil patch ' + prefix, afterPrefixAnsii);
      newName := 's' + prefix + ': ' + afterPrefixAnsii;
      end
    else
      begin
      response := inputQuery('Rename plant', 'Please enter a new name for plant ' + prefix, afterPrefixAnsii);
      newName := 'p' + prefix + ': ' + afterPrefixAnsii;
      end;
    afterPrefix := afterPrefixAnsii;
    end
  else
    begin
    newNameAnsii := newName;
    response := inputQuery('Rename object', 'Please enter a new name for '
    		+ chr(13) + currentModel.getName, newNameAnsii);
    newName := newNameAnsii;
    if currentModel.isTemplate then
      begin
      { for patches and plants, update template name field }
      if (currentModel is GsSoilPatch) then
        (currentModel as GsSoilPatch).setSoilTypeName(newName)
      else if (currentModel is GsPlant) then
        (currentModel as GsPlant).setCultivarName(newName);
      end;
    end;
  if response then
    begin
    changeNameCommand := GsChangeModelNameCommand.createCommand(currentModel, newName);
    GardenForm.doCommand(changeNameCommand);
    savedIndex := objectsList.itemIndex;
    { make string to go into list - may be slightly different from name }
    if currentModel.isTemplate then
      newName := '  T: ' + newName
    else if currentModel is GsPlant then
      newName := '  ' + newName;
    objectsList.items[objectsList.itemIndex] := newName;
    objectsList.itemIndex := savedIndex;
    end;
  end;

procedure TNotesForm.NotesChange(Sender: TObject);
  begin
  noteHasChanged := true;
  end;

procedure TNotesForm.NotesExit(Sender: TObject);
  begin
  if not noteHasChanged then exit;
  if modelOrToolNote = kModelNote then
    begin
    if self.currentModel <> nil then
      transferNoteAndMemo(self.currentModel.note, notes, kSetField);
    end
  else
    begin
    if self.currentTool <> nil then
      transferNoteAndMemo(self.currentTool.note, notes, kSetField);
    end;
  noteHasChanged := false;
  end;

const
  kBetweenGap = 4;
  kMinWidth = 24;

procedure TNotesForm.FormResize(Sender: TObject);
  begin
  doneButton.left := self.clientWidth - doneButton.width - kBetweenGap;
  renameButton.left := doneButton.left;
  exportButton.left := doneButton.left;
  helpButton.left := DoneButton.left;
  with objectsLabel do setBounds(kBetweenGap, kBetweenGap, width, height);
  self.resizeContentsToSplitter;
  end;

procedure TNotesForm.resizeContentsToSplitter;
  var newTop, newWidth: integer;
  begin
  splitter.visible := false;
  newTop := objectsLabel.top + objectsLabel.height + kBetweenGap;
  splitter.left := round(1.0 * splitter.left * clientWidth / lastWidth);
  if splitter.left > doneButton.left - kMinWidth then splitter.left := doneButton.left - kMinWidth;
  if splitter.left < kMinWidth then splitter.left := kMinWidth;
  with splitter do setBounds(left, newTop, width, self.clientHeight - newTop - kBetweenGap);
  with objectsList do setBounds(kBetweenGap, newTop, splitter.left - kBetweenGap, self.clientHeight - newTop - kBetweenGap);
  newWidth := helpButton.left - splitter.left - splitter.width - kBetweenGap;
  with notesLabel do setBounds(helpButton.left - newWidth, kBetweenGap, width, height);
  with notes do setBounds(helpButton.left - newWidth - kBetweenGap, newTop, newWidth,
    self.clientHeight - newTop - kBetweenGap);
  lastWidth := self.clientWidth;
  splitter.visible := true;
  end;

procedure TNotesForm.exportButtonClick(Sender: TObject);
  var
    i: integer;
    model: GsGraphicalModel;
    tool: GsTool;
    fileInfo: SaveFileNamesStructure;
    outputFile: TextFile;
    suggestedFileName: string;
  begin
  if (modelOrToolNote = kModelNote) then
    suggestedFileName := 'notesmod.tab'
  else
    suggestedFileName := 'notestoo.tab';
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, suggestedFileName, fileInfo) then exit;
  try
  	cursor_startWait;
    assignFile(outputFile, fileInfo.tempFile);
    rewrite(outputFile);
    { labels }
    write(outputFile, 'name'); write(outputFile, chr(9));
    write(outputFile, 'notes'); write(outputFile, chr(9));
    writeln(outputFile);
    if objectsList.items.count > 0 then
      for i := 0 to objectsList.items.count - 1 do
        begin
        if (modelOrToolNote = kModelNote) then
          begin
          model := GsGraphicalModel(objectsList.items.objects[i]);
          if model = nil then continue;
          write(outputFile, model.getName); write(outputFile, chr(9));
          write(outputFile, model.note); write(outputFile, chr(9));
          writeln(outputFile);
          end
        else
          begin
          tool := GsTool(objectsList.items.objects[i]);
          if tool = nil then continue;
          write(outputFile, tool.name); write(outputFile, chr(9));
          write(outputFile, tool.note); write(outputFile, chr(9));
          writeln(outputFile);
          end;
        end;
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    closeFile(outputFile);
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure TNotesForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Notes_editor')
  end;

procedure TNotesForm.splitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.splitterDragging := true;
  self.splitterStartPos := x;
  self.splitterLastDrawPos := -1;
  self.splitterNeedToRedraw := true;
  inherited mouseDown(button, shift, x, y);
  end;

procedure TNotesForm.splitterMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
  begin
  if self.splitterDragging and
    (splitter.left + x >= kMinWidth)
      and (splitter.left + x < self.clientWidth - kMinWidth) then
      begin
      self.undrawSplitterLine;
      self.splitterLastDrawPos := self.drawSplitterLine(x);
      end;
  inherited mouseMove(shift, x, y);
  end;

procedure TNotesForm.splitterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  begin
  if self.splitterDragging then
    begin
    self.undrawSplitterLine;
    splitter.left := splitter.left - (splitterStartPos - x);
    self.resizeContentsToSplitter;
    self.splitterDragging := false;
    end;
  inherited mouseUp(button, shift, x, y);
  end;

function TNotesForm.drawSplitterLine(pos: integer): integer;
  var
    theDC: HDC;
  begin
  theDC := getDC(0);
  result := self.clientOrigin.x + splitter.left + pos + 2;
  patBlt(theDC, result, self.clientOrigin.y + splitter.top, 1, splitter.height, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := true;
  end;

procedure TNotesForm.undrawSplitterLine;
  var theDC: HDC;
  begin
  if not self.splitterNeedToRedraw then exit;
  theDC := getDC(0);
  patBlt(theDC, self.splitterLastDrawPos,
    self.clientOrigin.y + splitter.top, 1, splitter.height, dstInvert);
  releaseDC(0, theDC);
  self.splitterNeedToRedraw := false;
  end;

end.

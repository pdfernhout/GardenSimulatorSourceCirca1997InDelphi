unit Umovtemp;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
umovtemp: Template mover form. Used to copy templates from one template file to another.
Called from the templates window (utempl). Standard mover-type dialog.
Current templates library opens by default on left side; on right side user opens another
template file. But user can open another file on the left side also. Also here you can
import tab-delimited templates singly from a template file by opening the file on the
right using Import and choosing templates to copy over. (Much easier than using the
whole-file import in the templates window.) A little messiness with keeping track of
the two list boxes with accompanying objects (templates) and the file names. Some
redundancy between the two sides. Had meant to make group, tool, etc movers like this
one but never got around to it.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, utempman, umodel, Buttons, ExtCtrls, ugsform;

type
  TTemplateMoverForm = class(GsForm)
    leftTemplatesList: TListBox;
    rightTemplatesList: TListBox;
    Done: TButton;
    CopyTemplate: TButton;
    DeleteTemplate: TButton;
    ObjectTypeToShow: TComboBox;
    helpButton: TBitBtn;
    renameTemplate: TButton;
    Import: TButton;
    Export: TButton;
    copyLeftToRight: TBitBtn;
    copyRightToLeft: TBitBtn;
    chooseFont: TBitBtn;
    FontDialog1: TFontDialog;
    leftTemplatesFileNameEdit: TEdit;
    leftTemplatesFileOpen: TButton;
    leftTemplatesFileNew: TButton;
    leftTemplatesFileSave: TButton;
    rightTemplatesFileNameEdit: TEdit;
    rightTemplatesFileOpen: TButton;
    rightTemplatesFileNew: TButton;
    rightTemplatesFileSave: TButton;
    leftTemplatesFileSaveAs: TButton;
    rightTemplatesFileSaveAs: TButton;
    exportText: TButton;
    importText: TButton;
    procedure copyLeftToRightClick(Sender: TObject);
    procedure copyRightToLeftClick(Sender: TObject);
    procedure CopyTemplateClick(Sender: TObject);
    procedure renameTemplateClick(Sender: TObject);
    procedure DeleteTemplateClick(Sender: TObject);
    procedure leftTemplatesListClick(Sender: TObject);
    procedure rightTemplatesListClick(Sender: TObject);
    procedure chooseFontClick(Sender: TObject);
    procedure leftTemplatesFileSaveClick(Sender: TObject);
    procedure leftTemplatesFileNewClick(Sender: TObject);
    procedure leftTemplatesFileOpenClick(Sender: TObject);
    procedure rightTemplatesFileOpenClick(Sender: TObject);
    procedure rightTemplatesFileNewClick(Sender: TObject);
    procedure rightTemplatesFileSaveClick(Sender: TObject);
    procedure DoneClick(Sender: TObject);
    procedure leftTemplatesFileSaveAsClick(Sender: TObject);
    procedure rightTemplatesFileSaveAsClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure exportTextClick(Sender: TObject);
    procedure importTextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    leftTemplateManager: GsTemplateManager;
    leftTemplatesFileChanged: boolean;
    leftTemplatesFile: string;
    rightTemplateManager: GsTemplateManager;
    rightTemplatesFileChanged: boolean;
    rightTemplatesFile: string;
    changedDefaultLibrary: boolean;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure setFileNameForManager(newName: string; templateManager: GsTemplateManager);
    function getFileNameForManager(templateManager: GsTemplateManager): string;
    function getFileNameForOtherManager(templateManager: GsTemplateManager): string;
    function listBoxForManager(templateManager: GsTemplateManager): TListBox;
    procedure setChangedFlagForTemplateManager(value: boolean; templateManager: GsTemplateManager);
    function managerHasChanged(templateManager: GsTemplateManager): boolean;
    procedure updateTemplateListForManager(templateManager: GsTemplateManager);
    procedure updateTemplateList(listBox: TListBox; templateManager: GsTemplateManager);
    procedure updateTemplateListWithModelsNoClear(listBox: TListBox; list: TList);
	 	function copyModelToTemplateManager(model: GsModel; toTemplateManager: GsTemplateManager): GsModel;
    procedure selectedModelAndTemplateManager(var model: GsModel;
      var templateManager: GsTemplateManager);
    function saveManagerIfChanged(templateManager: GsTemplateManager): boolean;
    procedure openTemplateFileForManager(templateManager: GsTemplateManager);
    procedure newTemplateFileForManager(templateManager: GsTemplateManager);
    procedure templateManagerSaveOrSaveAs(templateManager: GsTemplateManager;
      askForFileName: boolean);
  end;

implementation

uses udomain, uesoil, ueweath, ueplant, uebag, ufiler, ubrowser, ucursor, ugsim,
  usupport, uaspects;

{$R *.DFM}

{ -------------------------------------------------------------------------------------------- create/destroy }
constructor TTemplateMoverForm.create(AOwner: TComponent);
  begin
  inherited create(AOwner);
  leftTemplateManager := GsTemplateManager.create;
  { load default library into left side to start }
  if length(Domain.fileOptions.templatesFileName) > 0 then
    begin
    try
      cursor_startWait;
      GsFiler.load(Domain.fileOptions.templatesFileName, leftTemplateManager);
      self.setFileNameForManager(Domain.fileOptions.templatesFileName, leftTemplateManager);
    finally
      cursor_stopWait;
    end;
    end
  else
    self.setFileNameForManager('', leftTemplateManager);
  self.updateTemplateListForManager(leftTemplateManager);
  self.setChangedFlagForTemplateManager(false, leftTemplateManager);
  { right side is empty to start }
  rightTemplateManager := GsTemplateManager.create;
  setChangedFlagForTemplateManager(false, rightTemplateManager);
  self.setFileNameForManager('', rightTemplateManager);
  changedDefaultLibrary := false;
  end;

destructor TTemplateMoverForm.destroy;
  begin
  leftTemplateManager.free;
  leftTemplateManager := nil;
  rightTemplateManager.free;
  rightTemplateManager := nil;
  inherited destroy;
  end;

{ -------------------------------------------------------------------------------------------- updating }
procedure TTemplateMoverForm.updateTemplateList(listBox: TListBox; templateManager: GsTemplateManager);
	begin
{ CFK FIX - should really have this function in template manager - move later }
{ use this later - templateManager.loadObjectNamesIntoComboBox(listBox); }
{ also make other function for browser show this none flag }
  listBox.clear;
  listBox.items.addObject('Climates', nil);
  if templateManager.climateList.count = 0 then listBox.items.addObject('  {none}', nil);
  self.updateTemplateListWithModelsNoClear(listBox, templateManager.climateList);
  listBox.items.add('Soil types');
  if templateManager.soilTypeList.count = 0 then listBox.items.addObject('  {none}', nil);
  self.updateTemplateListWithModelsNoClear(listBox, templateManager.soilTypeList);
  listBox.items.add('Cultivars');
  if templateManager.cultivarList.count = 0 then listBox.items.addObject('  {none}', nil);
  self.updateTemplateListWithModelsNoClear(listBox, templateManager.cultivarList);
  listBox.items.add('Bags');
  if templateManager.bagList.count = 0 then listBox.items.addObject('  {none}', nil);
  self.updateTemplateListWithModelsNoClear(listBox, templateManager.bagList);
  end;

procedure TTemplateMoverForm.updateTemplateListWithModelsNoClear(listBox: TListBox; list: TList);
  var
  	i: longint;
  	model: GsModel;
  begin
  if list.count > 0 then
  	for i := 0 to list.count - 1 do
      begin
      model := GsModel(list.items[i]);
      if length(model.getName) = 0 then model.setName('unnamed');
  	  listBox.items.addObject('  T: ' + model.getName, model);
      end;
  end;

{ -------------------------------------------------------------------------------------------- button functions }
procedure TTemplateMoverForm.copyLeftToRightClick(Sender: TObject);
  var
    model: GsModel;
    theIndex: longint;
    theObject: TObject;
    theItems: TStrings;
	begin
  if length(rightTemplatesFile) = 0 then exit;
  model := nil;
  theItems := nil;
  theIndex := -1;
  if (leftTemplatesList.itemIndex >= 0) and (leftTemplatesList.items.count > 0) then
    begin
    theIndex := leftTemplatesList.itemIndex;
    theItems := leftTemplatesList.items;
    theObject := theItems.objects[theIndex];
    model := theObject as GsModel;
    end
  else
    exit;
  if model = nil then exit;
  self.copyModelToTemplateManager(model, rightTemplateManager);
	self.updateTemplateListForManager(rightTemplateManager);
  self.setChangedFlagForTemplateManager(true, rightTemplateManager);
	end;

procedure TTemplateMoverForm.copyRightToLeftClick(Sender: TObject);
  var
    model: GsModel;
	begin
  if length(leftTemplatesFile) = 0 then exit;
  model := nil;
  if (rightTemplatesList.itemIndex >= 0) and (rightTemplatesList.items.count > 0) then
    model := rightTemplatesList.items.objects[rightTemplatesList.itemIndex] as GsModel
  else
    exit;
  if model = nil then exit;
  self.copyModelToTemplateManager(model, leftTemplateManager);
	self.updateTemplateListForManager(leftTemplateManager);
  self.setChangedFlagForTemplateManager(true, leftTemplateManager);
	end;

function TTemplateMoverForm.copyModelToTemplateManager(model: GsModel; toTemplateManager: GsTemplateManager): GsModel;
   begin
   if model = nil then
     result := nil
   else
     case model.objectType of
  	   kObjectTypeBag: result := toTemplateManager.copyFromBag(model as GsBag);
		   kObjectTypeSoil: result := toTemplateManager.copyFromSoilPatch(model as GsSoilPatch, true);
		   kObjectTypeWeather: result := toTemplateManager.copyFromWeather(model as GsWeather, true);
		   kObjectTypePlant:
         begin
    	   result := toTemplateManager.copyFromPlant(model as GsPlant, true);
    	   {need to fixup resulting plant resources and icons}
    	   toTemplateManager.fixupCopiedCultivar(GsPlant(toTemplateManager.cultivarList.last), model as GsPlant);
         end;
       else result := nil;
       end;
  end;

procedure TTemplateMoverForm.selectedModelAndTemplateManager(var model: GsModel;
    var templateManager: GsTemplateManager);
  begin
  model := nil;
  templateManager := nil;
  if (length(leftTemplatesFile) <> 0) and (leftTemplatesList.itemIndex >= 0)
    and (leftTemplatesList.items.count > 0) then
    begin
    model := leftTemplatesList.items.objects[leftTemplatesList.itemIndex] as GsModel;
    templateManager := leftTemplateManager;
    end
  else if (length(rightTemplatesFile) <> 0) and (rightTemplatesList.itemIndex >= 0)
    and (rightTemplatesList.items.count > 0) then
    begin
    model := rightTemplatesList.items.objects[rightTemplatesList.itemIndex] as GsModel;
    templateManager := rightTemplateManager;
    end;
  end;

procedure TTemplateMoverForm.CopyTemplateClick(Sender: TObject);
  var
    model: GsModel;
    templateManager: GsTemplateManager;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
	begin
  self.selectedModelAndTemplateManager(model, templateManager);
  if (model = nil) or (templateManager = nil) then exit;
  model := self.copyModelToTemplateManager(model, templateManager);
  newName := copy('Copy of ' + model.getName, 1, kGraphicalModelNameLength);
  if not inputQuery('Make copy of template', 'Enter a name for this template copy.', newName) then
  	begin
    templateManager.removeTemplate(model);
    model.free;
    model := nil;
    end
  else
    begin
 	  model.setName(newName);
    self.updateTemplateListForManager(templateManager);
    self.setChangedFlagForTemplateManager(true, templateManager);
    end;
	end;

procedure TTemplateMoverForm.renameTemplateClick(Sender: TObject);
  var
    model: GsModel;
    templateManager: GsTemplateManager;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
	begin
  self.selectedModelAndTemplateManager(model, templateManager);
  if (model = nil) or (templateManager = nil) then exit;
  { restrict size of new name }
  newName := model.getName;
  if not inputQuery('Enter new name', 'Enter a new name for this template.', newName) then exit;
  model.setName(newName);
  self.updateTemplateListForManager(templateManager);
  self.setChangedFlagForTemplateManager(true, templateManager);
	end;

procedure TTemplateMoverForm.DeleteTemplateClick(Sender: TObject);
  var
    model: GsModel;
    templateManager: GsTemplateManager;
    listBox: TListBox;
    savedItemIndex: longint;
	begin
  if messageDlg('Delete this template?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;
  self.selectedModelAndTemplateManager(model, templateManager);
  if (model = nil) or (templateManager = nil) then exit;
  listBox := self.listBoxForManager(templateManager);
  if listBox = nil then
    raise Exception.create('DeleteTemplateClick: nil list box');
  savedItemIndex := listBox.itemIndex;
  if not templateManager.removeTemplate(model) then
    showMessage('DeleteTemplateClick: Error');
  model.free;
  model := nil;
  self.updateTemplateListForManager(templateManager);
  self.setChangedFlagForTemplateManager(true, templateManager);
  if listBox.items.count > 0 then
    begin
    if savedItemIndex > listBox.items.count - 1 then
      listBox.itemIndex := listBox.items.count - 1
    else
      listBox.itemIndex := savedItemIndex;
    end;
	end;

procedure TTemplateMoverForm.templateManagerSaveOrSaveAs(templateManager: GsTemplateManager;
    askForFileName: boolean);
  var
    fileInfo: SaveFileNamesStructure;
    fileName: string;
    askForFileNameConsideringUntitled: boolean;
  begin
  { clear out unused icons, tdos, harvest items }
  templateManager.garbageCollect(false, false);
  { if file name is 'untitled.tpl' then force saveAs }
  fileName := self.getFileNameForManager(templateManager);
  if fileName = 'untitled.tpl' then
    askForFileNameConsideringUntitled := true
  else
    askForFileNameConsideringUntitled := askForFileName;
  if not GsFile_GetFileSaveInfo(kFileTypeLibrary, askForFileNameConsideringUntitled, fileName, fileInfo) then exit;
  try
    cursor_startWait;
    GsFiler.save(fileInfo.tempFile, templateManager);
    fileInfo.writingWasSuccessful := true;
  finally
    cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
    self.setFileNameForManager(fileInfo.newFile, templateManager);
    self.setChangedFlagForTemplateManager(false, templateManager);
    if fileInfo.newFile = Domain.fileOptions.templatesFileName then changedDefaultLibrary := true;
  end;
  end;

procedure TTemplateMoverForm.leftTemplatesFileSaveClick(Sender: TObject);
  begin
  self.templateManagerSaveOrSaveAs(leftTemplateManager, kDontAskForFileName);
  end;

procedure TTemplateMoverForm.rightTemplatesFileSaveClick(Sender: TObject);
  begin
  self.templateManagerSaveOrSaveAs(rightTemplateManager, kDontAskForFileName);
  end;

procedure TTemplateMoverForm.leftTemplatesFileSaveAsClick(Sender: TObject);
  begin
  self.templateManagerSaveOrSaveAs(leftTemplateManager, kAskForFileName);
  end;

procedure TTemplateMoverForm.rightTemplatesFileSaveAsClick(Sender: TObject);
  begin
  self.templateManagerSaveOrSaveAs(rightTemplateManager, kAskForFileName);
  end;

function TTemplateMoverForm.saveManagerIfChanged(templateManager: GsTemplateManager): boolean;
  { returns false if user cancelled action }
  var
    response: word;
    prompt: string;
  begin
  result := true;
  if not self.managerHasChanged(templateManager) then exit;
  prompt := 'Save changes to ' + lowerCase(extractFileName(self.getFileNameForManager(templateManager))) + '?';
  response := messageDlg(prompt, mtConfirmation, mbYesNoCancel, 0);
  case response of
    mrYes: self.templateManagerSaveOrSaveAs(templateManager, kDontAskForFileName);
    mrNo: result := true;
    mrCancel: result := false;
    end;
  end;

procedure TTemplateMoverForm.openTemplateFileForManager(templateManager: GsTemplateManager);
  var
    fileNameWithPath: string;
  begin
  if not self.saveManagerIfChanged(templateManager) then exit;
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeLibrary, kNoSuggestedFile);
  if fileNameWithPath <> '' then
  	begin
  	if fileNameWithPath = self.getFileNameForOtherManager(templateManager) then
    	begin
      showMessage('The file you chose is already open on the other side.');
    	exit;
    	end;
    try
      cursor_startWait;
      GsFiler.load(fileNameWithPath, templateManager);
      self.setFileNameForManager(fileNameWithPath, templateManager);
		  self.updateTemplateListForManager(templateManager);
      self.setChangedFlagForTemplateManager(false, templateManager);
    finally
      cursor_stopWait;
    end;
    end;
	end;

procedure TTemplateMoverForm.leftTemplatesFileOpenClick(Sender: TObject);
  begin
  self.openTemplateFileForManager(leftTemplateManager);
  end;

procedure TTemplateMoverForm.rightTemplatesFileOpenClick(Sender: TObject);
  begin
  self.openTemplateFileForManager(rightTemplateManager);
  end;

procedure TTemplateMoverForm.newTemplateFileForManager(templateManager: GsTemplateManager);
  begin
  if not self.saveManagerIfChanged(templateManager) then exit;
  self.setFileNameForManager('untitled.tpl', templateManager);
  { clear out }
  templateManager.removeAllItems;
	self.updateTemplateListForManager(templateManager);
  self.setChangedFlagForTemplateManager(false, templateManager);
  end;

procedure TTemplateMoverForm.leftTemplatesFileNewClick(Sender: TObject);
  begin
  self.newTemplateFileForManager(leftTemplateManager);
  end;

procedure TTemplateMoverForm.rightTemplatesFileNewClick(Sender: TObject);
  begin
  self.newTemplateFileForManager(rightTemplateManager);
  end;

procedure TTemplateMoverForm.DoneClick(Sender: TObject);
  begin
  if not self.saveManagerIfChanged(leftTemplateManager) then exit;
  if not self.saveManagerIfChanged(rightTemplateManager) then exit;
  modalResult := mrOk; { cannot cancel }
  end;

procedure TTemplateMoverForm.leftTemplatesListClick(Sender: TObject);
	begin
  rightTemplatesList.itemIndex := -1;
	end;

procedure TTemplateMoverForm.rightTemplatesListClick(Sender: TObject);
	begin
  leftTemplatesList.itemIndex := -1;
	end;

procedure TTemplateMoverForm.chooseFontClick(Sender: TObject);
  begin
  fontDialog1.font := leftTemplatesList.font;
  if fontDialog1.Execute then
    begin
    leftTemplatesList.font := fontDialog1.font;
    rightTemplatesList.font := fontDialog1.font;
    application.processMessages;
    end;
  end;

{ get/set for left/right template manager }
procedure TTemplateMoverForm.setFileNameForManager(newName: string; templateManager: GsTemplateManager);
  begin
  if templateManager = leftTemplateManager then
    begin
    leftTemplatesFile := newName;
    leftTemplatesFileNameEdit.text := lowerCase(extractFileName(newName));
    if length(newName) > 0 then
      begin
      leftTemplatesFileSave.enabled := true;
      leftTemplatesFileSaveAs.enabled := true;
      end
    else
      begin
      leftTemplatesFileSave.enabled := false;
      leftTemplatesFileSaveAs.enabled := false;
      end;
    end
  else
    begin
    rightTemplatesFile := newName;
    rightTemplatesFileNameEdit.text := lowerCase(extractFileName(newName));
    if length(newName) > 0 then
      begin
      rightTemplatesFileSave.enabled := true;
      rightTemplatesFileSaveAs.enabled := true;
      end
    else
      begin
      rightTemplatesFileSave.enabled := false;
      rightTemplatesFileSaveAs.enabled := false;
      end;
    end;
  end;

function TTemplateMoverForm.getFileNameForManager(templateManager: GsTemplateManager): string;
  begin
  if templateManager = leftTemplateManager then
    result := leftTemplatesFile
  else
    result := rightTemplatesFile;
  end;

function TTemplateMoverForm.listBoxForManager(templateManager: GsTemplateManager): TListBox;
  begin
  result := nil;
  if templateManager = leftTemplateManager then
    result := leftTemplatesList
  else if templateManager = rightTemplateManager then
    result := rightTemplatesList;
  end;

{return name for othe file manager than one passed in}
function TTemplateMoverForm.getFileNameForOtherManager(templateManager: GsTemplateManager): string;
  begin
  if templateManager = rightTemplateManager then
    result := leftTemplatesFile
  else
    result := rightTemplatesFile;
  end;

procedure TTemplateMoverForm.setChangedFlagForTemplateManager(value: boolean; templateManager: GsTemplateManager);
  begin
  if templateManager = leftTemplateManager then
    begin
    leftTemplatesFileChanged := value;
    if leftTemplatesFileChanged then
      leftTemplatesFileSave.enabled := true
    else
      leftTemplatesFileSave.enabled := false;
    end
  else
    begin
    rightTemplatesFileChanged := value;
    if rightTemplatesFileChanged then
      rightTemplatesFileSave.enabled := true
    else
      rightTemplatesFileSave.enabled := false;
    end;
  end;

function TTemplateMoverForm.managerHasChanged(templateManager: GsTemplateManager): boolean;
  begin
  if templateManager = leftTemplateManager then
    result := leftTemplatesFileChanged
  else
    result := rightTemplatesFileChanged;
  end;

procedure TTemplateMoverForm.updateTemplateListForManager(templateManager: GsTemplateManager);
  begin
  if templateManager = leftTemplateManager then
    self.updateTemplateList(leftTemplatesList, templateManager)
  else if templateManager = rightTemplateManager then
    self.updateTemplateList(rightTemplatesList, templateManager)
  else
    raise Exception.create('TTemplateMoverForm.updateTemplateListForManager: bad manager');
  end;

procedure TTemplateMoverForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Template_mover')
  end;

procedure TTemplateMoverForm.exportTextClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, 'library.tab', fileInfo) then exit;
  try
  	cursor_startWait;
    { clear out unused icons, tdos, harvest items }
    rightTemplateManager.garbageCollect(false, false);
    rightTemplateManager.saveAllTemplatesToFileName(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure TTemplateMoverForm.importTextClick(Sender: TObject);
  var
    fileNameWithPath: string;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTabbedText, 'library.tab');
  if fileNameWithPath <> '' then
  	begin
    try
      cursor_startWait;
      rightTemplateManager.readAllTemplatesFromFileName(fileNameWithPath);
		  self.updateTemplateListForManager(rightTemplateManager);
      self.setChangedFlagForTemplateManager(true, rightTemplateManager);
    finally
      cursor_stopWait;
    end;
    end;
	end;

end.

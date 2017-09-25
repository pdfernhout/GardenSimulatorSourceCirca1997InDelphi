unit Utempl;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utempl: Templates form. See template manager (utempman) for description of
templates. When any garden file is open, the program needs an open templates
file to use to make new garden objects. This window shows the templates in
the open templates file (in a list box) and allows the user to change the
templates file, import templates into it from another file, import/export tab-delimited
templates files, save changes to the current templates file, and do various
things with individual templates in the list. The form makes a copy of the
current template manager (in the domain) at creation and replaces the original
with the copy if the user clicks OK. If the user clicks Cancel, the copy is
removed. Sometimes under the 16-bit app, making a copy of the whole
template manager makes the program run out of resources; if this happens
the window shows with no templates and the user has to go out and
close something and try again (but this doesn't happen often and
possibly not at all under 32-bit). It would be better if the templates
window didn't make a copy of the whole template manager because if you
have a large templates file it could be too memory-consuming
(never mind resources). This is the same problem as with the tools editor.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, utempman, umodel, Buttons, ExtCtrls, ugsform;

type
  TTemplatesForm = class(GsForm)
    StaticText4: TLabel;
    GardenTemplateList: TListBox;
    ok: TButton;
    CopyTemplate: TButton;
    DeleteTemplate: TButton;
    helpButton: TBitBtn;
    renameTemplate: TButton;
    filePanel: TPanel;
    libraryFileName: TEdit;
    LoadLibrary: TButton;
    saveLibraryAs: TButton;
    Label1: TLabel;
    chooseFont: TBitBtn;
    FontDialog1: TFontDialog;
    templateMoverButton: TButton;
    saveLibrary: TButton;
    garbageCollectIconsAndTdos: TButton;
    Cancel: TButton;
    epicImport: TButton;
    exportText: TButton;
    importText: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LoadLibraryClick(Sender: TObject);
    procedure saveLibraryAsClick(Sender: TObject);
    procedure garbageCollectIconsAndTdosClick(Sender: TObject);
    procedure CopyTemplateClick(Sender: TObject);
    procedure renameTemplateClick(Sender: TObject);
    procedure DeleteTemplateClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure chooseFontClick(Sender: TObject);
    procedure templateMoverButtonClick(Sender: TObject);
    procedure saveLibraryClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure okClick(Sender: TObject);
    procedure epicImportClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure exportTextClick(Sender: TObject);
    procedure importTextClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    currentTemplateManager: GsTemplateManager;
    currentFileName: string;
    procedure updateGardenTemplateList;
		procedure updateTemplateList(listBox: TListBox; templateManager: GsTemplateManager);
	 	function copyModelToTemplateManager(model: GsModel; toTemplateManager: GsTemplateManager): GsModel;
    procedure librarySaveOrSaveAs(askForFileName: boolean);
  end;

implementation

uses udomain, uesoil, ueweath, ueplant, uebag, ufiler, ubrowser, ucursor, ugsim,
  usupport, uaspects, umovtemp, ugetepic;

{$R *.DFM}

procedure TTemplatesForm.FormCreate(Sender: TObject);
	begin
  epicImport.visible := false;
  currentTemplateManager := GsTemplateManager.create;
  Domain.templateManager.copyTo(currentTemplateManager);
  currentFileName := Domain.fileOptions.templatesFileName;
  LibraryFileName.text := lowerCase(currentFileName);
	end;

procedure TTemplatesForm.FormActivate(Sender: TObject);
	begin
	self.updateGardenTemplateList;
	end;

procedure TTemplatesForm.FormDeactivate(Sender: TObject);
  begin
  currentTemplateManager := nil;
  end;

procedure TTemplatesForm.okClick(Sender: TObject);
  var
    oldTemplateManager: GsTemplateManager;
    oldFileName: string;
  begin
  templatesFileMightHaveChanged := true;
  self.currentTemplateManager.fixUpDefaultIndexes;
  oldTemplateManager := Domain.templateManager;
  Domain.templateManager := self.currentTemplateManager;
  { fix up all current plants and harvest items.
    if something fails here, will have left system in undetermined state,
    so try to revert back to old manager - which should be OK }
  try
    try
      oldFileName := Domain.fileOptions.templatesFileName;
      Domain.harvestManager.fixupAllResourcesInHarvestItems;
      Domain.garden.fixupAllResourcesInPlants;
      Domain.fileOptions.templatesFileName := self.currentFileName;
    except on E: Exception do
      begin
      ShowMessage('Problem: ' + E.message + '; The original library is being restored.');
      { put old template manager back }
      try
        Domain.templateManager := oldTemplateManager;
        oldTemplateManager := self.currentTemplateManager;
        Domain.harvestManager.fixupAllResourcesInHarvestItems;
        Domain.garden.fixupAllResourcesInPlants;
        Domain.fileOptions.templatesFileName := oldFileName;
      except
        ShowMessage('The original library could not be restored. Exiting the program immediately is recommended. '
          + 'Any garden files you save before exiting may be corrupt.');
        { don't free the other manager... }
        oldTemplateManager := nil;
        raise;
      end;
      end;
    end;
  finally
    oldTemplateManager.free;
  end;
  modalResult := mrOK;
  end;
 
procedure TTemplatesForm.updateGardenTemplateList;
	begin
  self.updateTemplateList(gardenTemplateList, currentTemplateManager)
  end;

procedure TTemplatesForm.updateTemplateList(listBox: TListBox; templateManager: GsTemplateManager);
	begin
  listBox.clear;
  if templateManager <> nil then
    templateManager.loadObjectNamesIntoTStrings(listBox.items);
  end;

procedure TTemplatesForm.LoadLibraryClick(Sender: TObject);
  var
    fileNameWithPath: string;
    fileNameIsDifferent: boolean;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeLibrary, currentFileName);
  fileNameIsDifferent := (fileNameWithPath <> currentFileName);
  if fileNameWithPath <> '' then
  	begin
    try
      cursor_startWait;
  	  currentFileName := fileNameWithPath;
      LibraryFileName.text := lowerCase(currentFileName);
		  GsFiler.load(fileNameWithPath, currentTemplateManager);
		  self.updateGardenTemplateList;
    finally
      cursor_stopWait;
    end;
    end;
  if fileNameIsDifferent then iniFileMightHaveChanged := true;
	end;

procedure TTemplatesForm.saveLibraryClick(Sender: TObject);
	begin
  self.librarySaveOrSaveAs(kDontAskForFileName);
	end;

procedure TTemplatesForm.saveLibraryAsClick(Sender: TObject);
	begin
  self.librarySaveOrSaveAs(kAskForFileName);
	end;

procedure TTemplatesForm.librarySaveOrSaveAs(askForFileName: boolean);
  var
    fileInfo: SaveFileNamesStructure;
    fileNameIsDifferent: boolean;
	begin
  if not GsFile_GetFileSaveInfo(kFileTypeLibrary, askForFileName, currentFileName, fileInfo) then exit;
  try
  	cursor_startWait;
    GsFiler.save(fileInfo.tempFile, currentTemplateManager);
    fileNameIsDifferent := fileInfo.newFile <> currentFileName;
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
    currentFileName := fileInfo.newFile;
    LibraryFileName.text := lowerCase(currentFileName);
  end;
  if fileNameIsDifferent then iniFileMightHaveChanged := true;
  templatesFileMightHaveChanged := false;
	end;

procedure TTemplatesForm.garbageCollectIconsAndTdosClick(Sender: TObject);
	begin
	cursor_startWait;
  currentTemplateManager.garbageCollect(true, true);
  cursor_stopWait;
	end;

procedure TTemplatesForm.CopyTemplateClick(Sender: TObject);
  var
    model: GsModel;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
	begin
  model := nil;
  if (gardenTemplateList.itemIndex >= 0)  and (gardenTemplateList.items.count > 0) then
    model := gardenTemplateList.items.objects[gardenTemplateList.itemIndex] as GsModel
  else
    exit;
  if model = nil then exit;
  model := self.copyModelToTemplateManager(model, currentTemplateManager);
  newName := copy('Copy of ' + model.getName, 1, kGraphicalModelNameLength);
  if not inputQuery('Make copy of template', 'Enter a name for this template copy.', newName) then
  	begin
    currentTemplateManager.removeTemplate(model);
    model.free;
    model := nil;
    end
  else
    begin
 	  model.setName(newName);
    { for patches and plants, update template name field }
    if (model is GsSoilPatch) then
      (model as GsSoilPatch).setSoilTypeName(newName)
    else if (model is GsPlant) then
      (model as GsPlant).setCultivarName(newName);
		self.updateGardenTemplateList;
    end;
	end;

function TTemplatesForm.copyModelToTemplateManager(model: GsModel; toTemplateManager: GsTemplateManager): GsModel;
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
    	   {need to fix up resulting plant resources and icons}
    	   toTemplateManager.fixupCopiedCultivar(GsPlant(toTemplateManager.cultivarList.last), model as GsPlant);
         end;
       else result := nil;
       end;
  end;

procedure TTemplatesForm.renameTemplateClick(Sender: TObject);
  var
    model: GsModel;
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
	begin
  model := nil;
  if (gardenTemplateList.itemIndex >= 0)  and (gardenTemplateList.items.count > 0) then
    model := gardenTemplateList.items.objects[gardenTemplateList.itemIndex] as GsModel
  else
    exit;
  { restrict size of new name }
  newName := model.getName;
  if not inputQuery('Enter new name', 'Enter a new name for this template.', newName) then exit;
  model.setName(newName);
  self.updateGardenTemplateList;
	end;

procedure TTemplatesForm.DeleteTemplateClick(Sender: TObject);
  var
    model: GsModel;
    savedItemIndex: longint;
	begin
  if messageDlg('Delete this template?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;
  model := nil;
  if (gardenTemplateList.itemIndex >= 0)  and (gardenTemplateList.items.count > 0) then
    model := gardenTemplateList.items.objects[gardenTemplateList.itemIndex] as GsModel
  else
    exit;
  savedItemIndex := gardenTemplateList.itemIndex;
  if not currentTemplateManager.removeTemplate(model) then
    ShowMessage('DeleteTemplateClick: Error');
  model.free;
  model := nil;
  self.updateGardenTemplateList;
  if gardenTemplateList.items.count > 0 then
    begin
    if savedItemIndex > gardenTemplateList.items.count - 1 then
      gardenTemplateList.itemIndex := gardenTemplateList.items.count - 1
    else
      gardenTemplateList.itemIndex := savedItemIndex;
    end;
	end;

procedure TTemplatesForm.chooseFontClick(Sender: TObject);
  begin
  fontDialog1.font := gardenTemplateList.font;
  if fontDialog1.Execute then
    begin
    gardenTemplateList.font := fontDialog1.font;
    application.processMessages;
    end;
  end;

procedure TTemplatesForm.templateMoverButtonClick(Sender: TObject);
  var
    templateMoverForm: TTemplateMoverForm;
  begin
  { CFK FIX - note that bringing up this form 'inside' the modal templates form makes
    the 'cancel' button on the templates form lose its meaning. it would be
    better to bring the template mover up from the garden form or some other way. }
  Domain.fileOptions.templatesFileName := currentFileName;
  templateMoverForm := TTemplateMoverForm.create(self);
  if templateMoverForm = nil then
    raise Exception.create('Could not create template mover window');
  try
    {set this to new name - will be rest by caller if cancel}
    templateMoverForm.showModal;
    if templateMoverForm.changedDefaultLibrary then
      begin
      ShowMessage('The default library has changed and will be reloaded');
      { reload default library ? }
      try
        cursor_startWait;
        GsFiler.load(currentFileName, currentTemplateManager);
			  self.updateGardenTemplateList;
      finally
        cursor_stopWait;
      end;
      end;
  finally
    templateMoverForm.free;
    templateMoverForm := nil;
  end;
  end;

procedure TTemplatesForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Templates_window')
  end;

procedure TTemplatesForm.epicImportClick(Sender: TObject);
  var
    epicImportForm: TEpicImportForm;
  begin
  epicImportForm := TEpicImportForm.create(self);
  if epicImportForm = nil then
    raise Exception.create('Could not create epic import window');
  try
    epicImportForm.currentTemplateManager := currentTemplateManager;
    epicImportForm.showModal;
    self.updateGardenTemplateList;
  finally
    epicImportForm.free;
    epicImportForm := nil;
  end;
  end;

procedure TTemplatesForm.exportTextClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
  begin
  if not GsFile_GetFileSaveInfo(kFileTypeTabbedText, kAskForFileName, 'library.tab', fileInfo) then exit;
  try
  	cursor_startWait;
    currentTemplateManager.saveAllTemplatesToFileName(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
  	cursor_stopWait;
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
  end;

procedure TTemplatesForm.importTextClick(Sender: TObject);
  var
    fileNameWithPath: string;
  begin
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeTabbedText, 'library.tab');
  if fileNameWithPath <> '' then
  	begin
    try
      cursor_startWait;
      currentTemplateManager.readAllTemplatesFromFileName(fileNameWithPath);
		  self.updateGardenTemplateList;
    finally
      cursor_stopWait;
    end;
    end;
	end;

procedure TTemplatesForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
  if (ssShift in shift) and (ssCtrl in shift) and (lowerCase(chr(key)) = 'e') then
    epicImport.visible := true;
  end;

end.

unit usupport;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
usupport: Many commonly used functions, mostly dealing with text manipulation.
Functions for opening and saving any kinds of files are also here. The file save
function makes a temporary file and a backup file and tries to handle problems.
Also this is where global vars are for whether ancillary files opened by the program
might have been changed during a session (templates, tools, groups, ini file).}

interface

uses WinTypes, WinProcs, StdCtrls, Classes, umodel;

{ don't like using constants for these, but can't find any functions that provide them }
const
  kMinSingle = -3.4e38;
  kMaxSingle = 3.4e38;

{ for suppressing message at end of save when exiting program }
var savingFilesAtProgramExit: boolean;

{ first simple try at knowing whether these files need to be saved or not }
{ put globals here because most files include this file }
var
  toolsFileMightHaveChanged: boolean;
  templatesFileMightHaveChanged: boolean;
  groupsFileMightHaveChanged: boolean;
  iniFileMightHaveChanged: boolean;

type
  SaveFileNamesStructure = record
    tempFile: string;
    newFile: string;
    backupFile: string;
    writingWasSuccessful: boolean;
    end;

const
  kFileTypeAny = 0;
  kFileTypeText = 1;
  kFileTypeTabbedText = 2;
  kFileTypeGarden = 3;
  kFileTypeTools = 4;
  kFileTypeLibrary = 5;
  kFileTypeAspects = 6;
  kFileTypeGroups = 7;
  kFileTypeTdo = 8;
  kFileTypeBitmap = 9;
  kFileTypeIcon = 10;
  kFileTypeWave = 11;
  kFileTypeExceptionList = 12;
  kFileTypeEpicDataFile = 13;
  kFileTypeMidiFile = 14;
  kFileTypeIni = 15;

  kWritingWasSuccessful = true;
  kWritingFailed = false;

  kNoSuggestedFile = '';

  kAskForFileName = true;
  kDontAskForFileName = false;

function readUntilTab(var theFile: textFile): string;
procedure readBooleanToTab(var fileRef: TextFile; var aBoolean: boolean);
function prefixFromObjectName(aName: string): string;
procedure separateNameIntoPrefixAndAfterPrefix(name: string; var prefix: string; var afterPrefix: string);
function firstLetterForObjectTypeIndex(objectTypeIndex: integer): string;
procedure checkBoundsWithMargin(value: single; var lowerBound: single; var upperBound: single);
function clientRectToScreenRect(handle: HWnd; clientRect: TRect): TRect;
function removeUnitSuffix(aString: string): string;
function boundForString(boundString: string; fieldType: integer; var number): boolean;
function addReturnsAfter30Chars(theString: string): string;
function trimLeft(theString: string): string;
function trimRight(theString: string): string;
function trimLeftAndRight(theString: string): string;
function removeTrailingZeros(theString: string): string;
function firstSignificantDigitPosition(range: extended): integer;
function digitValueString(value: single): string;
function shortenLongNonZeroDigitsAfterDecimal(theString: string): string;
function rWidth(rect: TRect): integer;
function rHeight(rect: TRect): integer;
function boolToStr(value: boolean): string;
procedure transferNoteAndMemo(var note: Pchar; memo: TMemo; direction: integer);
function currentObjectInComboBox(comboBox: TComboBox): TObject;
function currentObjectInListBox(listBox: TListBox): TObject;
function GsFile_GetFileOpenInfo(fileType: smallint; const suggestedFile: string): string;
function GsFile_FileNameIsOkayForSaving(suggestedFile: string): boolean;
function GsFile_GetFileSaveInfo(fileType: smallint; askForFileName: boolean;
    const suggestedFile: string; var fileInfo: SaveFileNamesStructure): boolean;
procedure GsFile_CleanUpAfterFileSave(var fileInfo: SaveFileNamesStructure);
function support_rgb(R: Byte; G: Byte; B: Byte): LongInt;

implementation

uses Forms, Dialogs, Controls, SysUtils, FileCtrl, uaspects, ueutils, uexcept, ugsim;

function readUntilTab(var theFile: textFile): string;
  var
    aChar: char;
  begin
  aChar := chr(0);
  result := '';
  while not eof(theFile) do
    begin
    read(theFile, aChar);
    if (aChar <> chr(9)) and (aChar <> chr(10)) and (aChar <> chr(13)) then
      result := result + aChar
    else
      begin
      if (aChar = chr(13)) and not eof(theFile) then
        begin
        read(theFile, aChar);
        if aChar <> chr(10) then
          raise Exception.create('Problem in file input');
        end;
      break;
      end;
    end;
  { remove quotes placed by spreadsheet around string if it has a comma in it }
  if (result[1] = '"') and (result[length(result)] = '"') then
    result := copy(result, 2, length(result) - 2);
  end;

procedure readBooleanToTab(var fileRef: TextFile; var aBoolean: boolean);
  var
    booleanString: string;
  begin
  booleanString := readUntilTab(fileRef);
  if (upperCase(booleanString) = 'TRUE') then
    aBoolean := true
  else if (upperCase(booleanString) = 'FALSE') then
    aBoolean := false
  else
    raise Exception.create('Improper file format for boolean, got: ' + booleanString);
  end;

function prefixFromObjectName(aName: string): string;
  begin
  result := copy(aName, 1, pos(':', aName) - 1);
  end;

procedure separateNameIntoPrefixAndAfterPrefix(name: string; var prefix: string; var afterPrefix: string);
  begin
  { get prefix }
  prefix := prefixFromObjectName(name);
  { get part user can edit }
  afterPrefix := copy(name, length(prefix) + 3, length(name));
  { remove first letter from prefix to get number }
  prefix := copy(prefix, 2, length(prefix));
  end;

function firstLetterForObjectTypeIndex(objectTypeIndex: integer): string;
  begin
  case objectTypeIndex of
    kObjectTypeGarden: result := 'g';
    kObjectTypeWeather: result := 'w';
    kObjectTypeSoil: result := 's';
    kObjectTypePlant, kObjectTypeDrawingPlant: result := 'p';
    kObjectTypePesticide: result := 'pe';
    kObjectTypeBag: result := 'b';
    { these will probably never be used }
    kObjectTypeBlob: result := 'om';
    kObjectTypeFruit: result := 'f';
  else
    result := '?';
  end;
  end;

procedure checkBoundsWithMargin(value: single; var lowerBound: single; var upperBound: single);
  var newBound: single;
  begin
  if value < lowerBound then
    begin
    try
      newBound := value {- abs(value / 100.0)};
    except
      newBound := value;
    end;
    { don't push bound below zero just for margin }
    if (value >= 0) and (newBound < 0) then newBound := 0.0;
    lowerBound := newBound;
    end;
  if value > upperBound then
    begin
    try
      newBound := value {+ abs(value / 100.0)};
    except
      newBound := value;
    end;
    upperBound := newBound;
    end;
  end;

function clientRectToScreenRect(handle: HWnd; clientRect: TRect): TRect;
  var
    p1, p2: TPoint;
  begin
  p1 := point(clientRect.left, clientRect.top);
  clientToScreen(handle, p1);
  p2 := point(clientRect.right, clientRect.bottom);
  clientToScreen(handle, p2);
  result := rect(p1.x, p1.y, p2.x, p2.y);
  end;

function removeUnitSuffix(aString: string): string;
  var underscore: integer;
  begin
  underscore := pos('_', aString);
  if underscore = 0 then
    result := aString
  else
    result := copy(aString, 1, underscore - 1);
  end;

function boundForString(boundString: string; fieldType: integer; var number): boolean;
{returns whether conversion was successful}
  var
    piTimesTwo: single;
  begin
  result := true;
  if (boundString = 'MIN') or (boundString = 'FLT_MIN') then
    begin
    if fieldType = kFieldFloat then
      single(number) := kMinSingle
    else
      smallint(number) := -32768
    end
  else if (boundString = 'MAX') or (boundString = 'FLT_MAX')  then
    begin
    if fieldType = kFieldFloat then
      single(number) := kMaxSingle
    else
      smallint(number) := 32767;
    end
  else if (boundString = 'PI*2') and (fieldType = kFieldFloat) then
    begin
    piTimesTwo := pi * 2.0;
    single(number) := piTimesTwo;
    end
  else if (boundString = '')  then
    begin
    if fieldType = kFieldFloat then
      single(number) := 0.0
    else
      smallint(number) := 0;
    end
  else
    try
      if fieldType = kFieldFloat then
        single(number) := StrToFloat(boundString)
      else
        smallint(number) := StrToInt(boundString);
    except
      on EConvertError do
        begin
        result := false;
        if fieldType = kFieldFloat then
          single(number) := 0.0
        else
          smallint(number) := 0;
        end;
    end;
  end;

function addReturnsAfter30Chars(theString: string): string;
{not using anymore - still had bug where string longer than i got garbled}
  var
    i, origLength: integer;
  begin
  i := 30;
  result := '';
  origLength := length(theString);
  while i < 241 do
    begin
    result := result + copy(theString, i - 29, i);
    if origLength > i then
      begin
      if theString[i] <> ' ' then result := result + '-';
      result := result + chr(13);
      end
    else
      exit;
    i := i + 30;
    end;
  end;

function trimLeft(theString: string): string;
  var
    i: integer;
  begin
  i := 1;
  while theString[i] = ' ' do inc(i);
  result := copy(theString, i, length(theString) - i + 1);
  end;

function trimRight(theString: string): string;
  begin
  while theString[length(theString)] = ' ' do dec(theString[0]);
  result := theString;
  end;

function trimLeftAndRight(theString: string): string;
  begin
  result := trimRight(trimLeft(theString));
  end;

function removeTrailingZeros(theString: string): string;
  begin
  while (length(theString) > 1)
    and (theString[length(theString)] = '0') and (theString[length(theString) - 1] <> '.')
    do dec(theString[0]);
  result := theString;
  end;

const
  kMaxDigitsAfterFirstNonZeroDigit = 2;

function shortenLongNonZeroDigitsAfterDecimal(theString: string): string;
  var
    pointPos, firstNonZeroDigit, digitsToGetRidOf: longint;
  begin
  result := theString;
  pointPos := pos('.', result);
  if pointPos <> 0 then
    begin
    firstNonZeroDigit := pointPos + 1;
    while (firstNonZeroDigit <= length(theString)) and (theString[firstNonZeroDigit] = '0') do
      inc(firstNonZeroDigit);
    if firstNonZeroDigit < length(theString) then
      begin
      digitsToGetRidOf := length(theString) - firstNonZeroDigit + 1 - kMaxDigitsAfterFirstNonZeroDigit;
      if digitsToGetRidOf > 0 then
        result := copy(result, 1, length(theString) - digitsToGetRidOf);
      end;
    end;
  end;

{returns place in number where first significant digit
Examples:
4 -> 0
12300 -> 4
0.034 -> -2
}
{ this is something we don't really want to flag the errorMessage system because it is called from paint methods
  and could cause a recursive paint }
function firstSignificantDigitPosition(range: extended): integer;
	var
   log10Result : single;
	begin
  try
  if range = 0.0 then
    result := 0
  else
    begin
  	log10Result := log10(range);
    if log10Result > 0.0 then
    	result := trunc(log10Result)
    else
    	result := trunc(log10Result - 1.0);
    end;
  except
    result := 0; {errorMessage('Problem in firstSignificantDigitPosition');}
  end;
  end;

function digitValueString(value: single): string;
  begin
  result := removeTrailingZeros(shortenLongNonZeroDigitsAfterDecimal(floatToStrF(value, ffFixed, 7, 8)));
  end;

function combineRects(rect1, rect2: TRect): TRect;
  begin
  with result do
    begin
    left := intMin(rect1.left, rect2.left);
    right := intMax(rect1.right, rect2.right);
    top := intMin(rect1.top, rect2.top);
    bottom := intMax(rect1.bottom, rect2.bottom);
    end;
  end;

function rWidth(rect: TRect): integer;
  begin
  result := rect.right - rect.left;
  end;

function rHeight(rect: TRect): integer;
  begin
  result := rect.bottom - rect.top;
  end;

function boolToStr(value: boolean): string;
  begin
  if value then
    result := 'true'
  else
    result := 'false';
  end;

procedure transferNoteAndMemo(var note: Pchar; memo: TMemo; direction: integer);
  var
  	length: longint;
	begin
  if direction = kGetField then
    begin
		if note <> nil then
  		memo.setTextBuf(note)
		else
  		memo.setTextBuf('');
    end
  else if direction = kSetField then
    begin
 		length := memo.getTextLen;
  	if length = 0 then
    	begin
    	if note <> nil then
      	begin
      	StrDispose(note);
      	note := nil;
      	end;
    	end
  	else
    	begin
    	if note = nil then
      	begin
      	note := StrAlloc(length+1);
      	FailNilPtr(note);
      	end;
    	{PDF FIX - account for nil}
    	if length >= StrBufSize(note) then
      	begin
    		StrDispose(note);
    		note := nil;
    		note := StrAlloc(length+1);
    		FailNilPtr(note);
    		end;
    	memo.getTextBuf(note, StrBufSize(note));
    	end;
 	  end;
  end;

function currentObjectInComboBox(comboBox: TComboBox): TObject;
  begin
  if comboBox.items.count = 0 then
    result := nil
  else if comboBox.itemIndex <> -1 then
    result := comboBox.items.objects[comboBox.itemIndex]
  else
    result := nil;
  end;

function currentObjectInListBox(listBox: TListBox): TObject;
  begin
  if listBox.items.count = 0 then
    result := nil
  else if listBox.itemIndex <> -1 then
    result := listBox.items.objects[listBox.itemIndex]
  else
    result := nil;
  end;


{ FILE OPEN/SAVE FUNCTIONS }
function nameStringForFileType(fileType: smallint): string;
  begin
  case fileType of
    kFileTypeAny: result := '';
    kFileTypeText: result := 'Text';
    kFileTypeTabbedText: result := 'Tabbed text';
    kFileTypeGarden: result := 'Garden';
    kFileTypeTools: result := 'Tool';
    kFileTypeLibrary: result := 'Library';
    kFileTypeAspects: result := 'Aspect';
    kFileTypeGroups: result := 'Group';
    kFileTypeTdo: result := '3D object';
    kFileTypeBitmap: result := 'Bitmap';
    kFileTypeIcon: result := 'Icon';
    kFileTypeWave: result := 'Wave';
    kFileTypeExceptionList: result := 'Exception';
    kFileTypeEpicDataFile: result := 'EPIC data';
    kFileTypeMidiFile: result := 'MIDI';
    kFileTypeIni: result := 'Ini';
    end;
  end;

function extensionForFileType(fileType: smallint): string;
  begin
  result := '';
  case fileType of
    kFileTypeAny: result := '*';
    kFileTypeText: result := 'txt';
    kFileTypeTabbedText: result := 'tab';
    kFileTypeGarden: result := 'gdn';
    kFileTypeTools: result := 'too';
    kFileTypeLibrary: result := 'tpl';
    kFileTypeAspects: result := 'asp';
    kFileTypeGroups: result := 'grp';
    kFileTypeTdo: result := 'tdo';
    kFileTypeBitmap: result := 'bmp';
    kFileTypeIcon: result := 'ico';
    kFileTypeWave: result := 'wav';
    kFileTypeExceptionList: result := 'nex';
    kFileTypeEpicDataFile: result := 'dat';
    kFileTypeMidiFile: result := 'mid';
    kFileTypeIni: result := 'ini';
    end;
  end;

function filterStringForFileType(fileType: smallint): string;
  var extension: string;
  begin
  extension := extensionForFileType(fileType);
  if fileType = kFileTypeAny then
    result := 'All files (*.*)|*.*'
  else
    result := nameStringForFileType(fileType) + ' files (*.' + extension + ')|*.' + extension +
      '|All files (*.*)|*.*';
  end;

function GsFile_GetFileOpenInfo(fileType: smallint; const suggestedFile: string): string;
  var
    fullSuggestedFileName: string;
    openDialog: TOpenDialog;
    nameString: string;
  begin
  result := '';
  openDialog := TOpenDialog.create(application);
  try
  with openDialog do
    begin
    if suggestedFile = '' then
      begin
      fileName := '*.' + extensionForFileType(fileType);
      end
    else
      begin
      fullSuggestedFileName := expandFileName(suggestedFile);
      { if directory does not exist, will leave as it was }
      initialDir := extractFilePath(fullSuggestedFileName);
      if fileExists(fullSuggestedFileName) then fileName := extractFileName(fullSuggestedFileName);
      end;
    nameString := nameStringForFileType(fileType);
    if nameString[1] in ['A', 'E', 'I', 'O', 'U'] then
      title := 'Choose an ' + nameString + ' file'
    else
      title := 'Choose a ' + nameString + ' file';
    filter := filterStringForFileType(fileType);
    defaultExt := extensionForFileType(fileType);
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
   end;
  if openDialog.execute then
    begin
    if (ofExtensionDifferent in openDialog.options) and (fileType <> kFileTypeAny) then
      begin
      showMessage('The file (' + lowerCase(openDialog.fileName)
        + ') does not have the correct extension (' + lowerCase(openDialog.defaultExt) + ').');
      exit;
      end
    else
      result := openDialog.fileName;
    end;
  finally
  openDialog.free;
  end;
  end;

function GsFile_FileNameIsOkayForSaving(suggestedFile: string): boolean;
  var
    fullSuggestedFileName: string;
  begin
  result := false;
  if length(suggestedFile) = 0 then exit;
  fullSuggestedFileName := expandFileName(suggestedFile);
  { check if directory exists }
  if not directoryExists(extractFilePath(fullSuggestedFileName)) then
    begin
    showMessage('The directory ' + lowerCase(extractFilePath(fullSuggestedFileName)) + ' does not exist.');
    exit;
    end;
  { if file exists and is writable, it's ok because Save (not Save As) should not ask to rewrite }
  { if file exists but is read-only, quit  }
  if fileExists(fullSuggestedFileName)
      and Boolean(FileGetAttr(fullSuggestedFileName) and faReadOnly) then
    begin
    showMessage('The file ' + lowerCase(fullSuggestedFileName) + ' exists and is read-only.');
    exit;
    end;
  result := true;
  end;

function GsFile_GetFileSaveInfo(fileType: smallint; askForFileName: boolean;
    const suggestedFile: string; var fileInfo: SaveFileNamesStructure): boolean;
  var
    saveDialog: TSaveDialog;
    tryBackupName, tryTempName, fullSuggestedFileName, prompt, extension: string;
    index: smallint;
    tempFileHandle: longint;
 begin
  result := false;
  saveDialog := TSaveDialog.create(application);
  try
  { default info }
  with fileInfo do
    begin
    tempFile := '';
    newFile := '';
    backupFile := '';
    writingWasSuccessful := false;
    end;
  { if this is a Save, try to set the file name from the suggestedFile given; if file name
    is invalid, set flag to move into Save As instead }
  if not askForFileName then
    begin
    askForFileName := not GsFile_FileNameIsOkayForSaving(suggestedFile);
    if not askForFileName then fileInfo.newFile := expandFileName(suggestedFile);
    end;
  { if this is a Save As, or if this is a Save and the file in suggestedFile is invalid,
    ask user for a file name }
  if askForFileName then
    begin
    with saveDialog do
      begin
      if length(suggestedFile) > 0 then
        begin
        fullSuggestedFileName := expandFileName(suggestedFile);
        { if directory does not exist, will leave as it was }
        initialDir := extractFilePath(fullSuggestedFileName);
        { don't check if file exists (because saving); check if dir exists }
        if directoryExists(extractFilePath(fullSuggestedFileName)) then
          fileName := extractFileName(fullSuggestedFileName);
        end;
      filter := filterStringForFileType(fileType);
      defaultExt := extensionForFileType(fileType);
      options := options + [ofPathMustExist, ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn];
      end;
    if not saveDialog.execute then
      begin
      saveDialog.free;
      saveDialog := nil;
      exit;
      end;
    fileInfo.newFile := saveDialog.fileName;
    end;
  { set backup file name, check if read-only }
  { for backup file extension, use first two chars of newFile, then tilde, so they sort together and
    you can tell what type the backup file is }
  try
    extension := extractFileExt(fileInfo.newFile);   { includes dot }
    extension := copy(extension, 1, 3) + '~';
  except
    extension := '.BAK';
  end;
  tryBackupName := changeFileExt(fileInfo.newFile, extension); 
  if fileExists(tryBackupName) then
    begin
    if (boolean(fileGetAttr(tryBackupName) and faReadOnly)) then
      begin
      prompt := 'The backup file ' + lowerCase(tryBackupName) + ' is read-only. Continue?';
      if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        exit;
      end;
    end
  else
    fileInfo.backupFile := tryBackupName;
  { set temp file name }
  for index := 100 to 999 do
    begin
    tryTempName := changeFileExt(fileInfo.newFile, '.' + intToStr(index));
    if not fileExists(tryTempName) then
      begin
      fileInfo.tempFile := tryTempName;
      break;
      end;
    end;
  { if can't find unused temp file, quit }
  if fileInfo.tempFile = '' then
    begin
    showMessage('Could not create temporary file ' + lowerCase(tryTempName) + '.');
    exit;
    end;
  { test whether temp file can be created }
  tempFileHandle := fileCreate(fileInfo.tempFile);
  if tempFileHandle > 0 then
    begin
    fileClose(tempFileHandle);
    if not deleteFile(fileInfo.tempFile) then
      begin
      showMessage('Problem with temporary file ' + lowerCase(fileInfo.tempFile) + '.');
      exit;
      end;
    end
  else
    begin
    showMessage('Could not write to temporary file ' + lowerCase(fileInfo.tempFile) + '.');
    exit;
    end;
  result := true;
  finally
  saveDialog.free;
  saveDialog := nil;
  end;
  end;

procedure GsFile_CleanUpAfterFileSave(var fileInfo: SaveFileNamesStructure);
  var
    useBackup, renamingFailed, deletingFailed: boolean;
    prompt: string;
  begin
  useBackup := true;
  {if couldn't write, then remove temp file and exit without warning}
  if not fileInfo.writingWasSuccessful then
    begin
    deleteFile(fileInfo.tempFile);
    exit;
    end;
  {remove backup file if exists from prior backup}
  if fileInfo.backupFile <> '' then
    begin
    if fileExists(fileInfo.backupFile) then
      begin
      {try to delete backup file}
      deletingFailed := not deleteFile(fileInfo.backupFile);
      if deletingFailed then
        begin
        {couldn't delete backup file}
        prompt := 'Could not write backup file ' + lowerCase(fileInfo.backupFile) + '. Continue?';
        if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
          {user doesn't want to proceed - so cleanup temp file}
          deleteFile(fileInfo.tempFile);
          exit;
          end
        else
          useBackup := false;
        end;
      end
    end
  else
    useBackup := false;
  {if original file exists make backup if requested...}
  if fileExists(fileInfo.newFile) then
    begin
    if useBackup then
      begin
      {rename old copy of new file to make backup}
      renamingFailed := not renameFile(fileInfo.newFile, fileInfo.backupFile);
      if renamingFailed then
        begin
        prompt := 'Could not rename old file to backup file ' + lowerCase(fileInfo.backupFile) + '. Continue?';
        if messageDlg(prompt, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
          {user doesn't want to proceed - so cleanup temp file}
          deleteFile(fileInfo.tempFile);
          exit;
          end
        else
          useBackup := false;
        end;
      end;
    {could not create backup file - so just delete old file instead of renaming}
    if not useBackup then
      begin
      deletingFailed := not deleteFile(fileInfo.newFile);
      if deletingFailed then
        begin
        ShowMessage('Could not write file ' + lowerCase(fileInfo.newFile));
        exit;
        end;
      end;
    end;
  {rename temp file to newFile name}
  renamingFailed := not renameFile(fileInfo.tempFile, fileInfo.newFile);
  if renamingFailed then
    begin
    {clean up by removing temp file}
    ShowMessage('Could not write file ' + lowerCase(fileInfo.newFile) + ' from '
        + lowerCase(fileInfo.tempFile));
    DeleteFile(fileInfo.tempFile);
    exit;
    end;
  if not savingFilesAtProgramExit then
    messageDlg('File ' + lowerCase(fileInfo.newFile) + ' saved.', mtInformation, [mbOK], 0);  
  end;

function support_rgb(R: Byte; G: Byte; B: Byte): LongInt;
  begin
  {result := RGB(r, g, b);  }
  result := PALETTERGB(r, g, b);
  end;

begin
{ these are set up when the application starts, because we are concerned with saving this for the session
  and not the garden file }
toolsFileMightHaveChanged := false;
templatesFileMightHaveChanged := false;
groupsFileMightHaveChanged := false;
iniFileMightHaveChanged := false;
savingFilesAtProgramExit := false;
end.

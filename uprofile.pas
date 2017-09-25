unit Uprofile;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uprofile: Code for reading an ini file. Used only by domain. We used to use this
for more complex things at a time when we had data files stored in profile format,
but now most of this code is unused. }
          
interface

uses
	SysUtils, WinTypes, IniFiles, classes;

type
	GsProfile = class(TIniFile)
  public
  strings: TStringList;
  {for incremental loading or saving}
  importFile: TextFile;
  exportFile: TextFile;
  currentSection: string;
  nextSection: string;
  {conventional features}
  function readPoint(name: string; section: string) : TPoint;
  class function readPointFromString(const aString: string): TPoint;
  destructor destroy; override;
  constructor Create(const FileName: string);
  {advanced features}
	procedure resetStrings;
  procedure loadSection(const fileName: string; const sectionName: string);
  procedure saveSection(const fileName: string; const sectionName: string);
  function stringForName(const name: string): string;
	procedure transferNote(name: string; var note: PChar; direction: integer);
  {for incremental operations}
  procedure openImportFile(const fileName: string);
  function  importDone: boolean;
  procedure openExportFile(const fileName: string);
  procedure importNextSection;
  procedure exportNextSection(const sectionName: string);
  procedure closeImportFile;
  procedure closeExportFile;
 end;


implementation

uses uexcept;

constructor GsProfile.Create(const FileName: string);
  begin
  inherited Create(FileName);
  strings := TStringList.create;
  end;

destructor GsProfile.destroy;
  begin
  strings.free;
  inherited destroy;
  end;

procedure GsProfile.resetStrings;
  begin
  strings.clear
  end;

function GsProfile.readPoint(name: string; section: string) : TPoint;
  var
  pointString: string;
  point : TPoint;
	begin
  pointString := self.readString(name, section, '0 0');
  point := GsProfile.readPointFromString(pointString);
  Result := point;
  end;

class function GsProfile.readPointFromString(const aString: string): TPoint;
	var
  subString : string;
  index: integer;
  aPoint: TPoint;
	begin
  {assume no leading space}
  index := pos(' ', aString);
  subString := copy(aString, 1, index - 1);
  aPoint.x := StrToInt(subString);
  subString := copy(aString, index + 1, length(aString) - index);
  aPoint.y := StrToInt(subString);
  Result := aPoint;
  end;

{advanced features}
{sectionName should be given in brackets and must be first thing on line}
{lines must be less than 256 characters}
{remember to deal with possible blank lines at end - insert before if
adding new things}
procedure GsProfile.loadSection(const fileName: string; const sectionName: string);
  var
    inputFile: TextFile;
    inputString: string;
    foundSection: boolean;
  begin
  self.resetStrings;
  assignFile(inputFile, fileName);
  try
  reset(inputFile);
  foundSection := false;
  while not eof(inputFile) do
    begin
    readln(inputFile, inputString);
    if pos(sectionName, inputString) = 1 then
      begin
      foundSection := true;
      break;
      end;
    end;
  if not foundSection then exit;
  while not eof(inputFile) do
    begin
    readln(inputFile, inputString);
    if pos('[', inputString) = 1 then break;
    strings.add(inputString);
    end;
  finally
    CloseFile(inputFile);
  end;
  end;

procedure GsProfile.saveSection(const fileName: string; const sectionName: string);
  var
    inputFile: TextFile;
    outputFile: TextFile;
    inputString: string;
    foundSection: boolean;
    tempFileName: string;
{$IFDEF WINDOWS}
    backupFileName: string;
{$ELSE}
    backupFileName: ansistring;
{$ENDIF}
    i: longint;
  begin
  foundSection := false;
  {maybe should put temp file in windows directory? - GetWindowsDirectory}
  tempFileName := ExtractFilePath(fileName) + '$gwiini$.tmp';
  backupFileName := ExtractFileName(fileName);
  backupFileName := copy(backupFileName, 1, length(backupFileName) - 3);
  backupFileName := ExtractFilePath(fileName) + backupFileName + 'bak';
  assignFile(inputFile, fileName);
  reset(inputFile);
  try
  assignFile(outputFile, tempFileName);
  rewrite(outputFile);
  try
  {copy while looking for section}
  while not eof(inputFile) do
    begin
    readln(inputFile, inputString);
    writeln(outputFile, inputString);
    if pos(sectionName, inputString) = 1 then
      begin
      foundSection := true;
      break;
      end;
    end;
  if foundSection then
    begin
    {skip input file section}
  	while not eof(inputFile) do
    	begin
    	readln(inputFile, inputString);
    	if pos('[', inputString) = 1 then
        break;
    	end;
    {write out strings}
    if strings.count > 0 then
      for i := 0 to strings.count - 1 do
        writeln(outputFile, strings.strings[i]);
    {write out next section name if one caused the break}
    if pos('[', inputString) = 1 then
    	writeln(outputFile, inputString);
    end;
  {write rest of input file}
  while not eof(inputFile) do
    begin
    readln(inputFile, inputString);
    writeln(outputFile, inputString);
    end;
  {otherise write section now}
  if not foundSection then
    begin
    writeln(outputFile, sectionName);
    {write out strings}
    if strings.count > 0 then
      for i := 0 to strings.count - 1 do
        writeln(outputFile, strings.strings[i]);
    end;
  finally
    CloseFile(outputFile);
  end;
  finally
    CloseFile(inputFile);
  end;
  {now replace ini file with temp file}
  if foundSection then
    begin
{$IFDEF WINDOWS}
    deleteFile(backupFileName);
{$ELSE}
    deleteFile(PChar(backupFileName));
{$ENDIF}
    renameFile(fileName, backupFileName);
    renameFile(tempFileName, fileName);
    end;

  end;

{returns string after name and =}
function GsProfile.stringForName(const name: string): string;
  var
    i: longint;
    searchForString: string;
  begin
  result := '';
  if (strings = nil) or (strings.count = 0) then exit;
  searchForString := name + '=';
  if strings.count > 0 then
    for i := 0 to strings.count - 1 do
      begin
      if pos(searchForString, strings.strings[i]) = 1 then
        begin
        result := copy(strings.strings[i], length(searchForString) + 1, 256);
        exit;
        end;
      end;
  end;

{copied from umodel}
const
  kGetField = 0;
  kSetField = 1;

{break not into 80 char strings, or recombine such strings}
procedure GsProfile.transferNote(name: string; var note: PChar; direction: integer);
  var
    i: longint;
    searchForString: string;
    stringToAdd: array [0..256] of char;
    newNote: PChar;
    stringForProfile: string;
    withinNote: PChar;
  begin
	searchForString := name + '=';
	if direction = kGetField then
    begin
    if note <> nil then
      begin
      StrDispose(note);
      note := nil;
      end;
    {allocate maximum size}
    newNote := StrAlloc(65526);
    FailNilPtr(newNote);
    try
    newNote[0] := chr(0);
    {loop through all strings and concatenate all matches to name =}
  	if strings.count > 0 then
    	for i := 0 to strings.count - 1 do
      	begin
      	if pos(searchForString, strings.strings[i]) = 1 then
        	begin
        	StrPCopy(stringToAdd, copy(strings.strings[i], length(searchForString) + 1, 256));
          StrLCat(newNote, stringToAdd, 65526 - 1); {inefficient but safe}
	        end;
      	end;
    {now alloc just enough space for string}
    note := StrAlloc(StrLen(newNote) + 1);
    FailNilPtr(note);
    StrCopy(note, newNote);
    finally
    StrDispose(newNote);
    end;
    end
  else if direction = kSetField then
    begin
    if note = nil then exit;
    withinNote := note;
    while withinNote^ <> char(0) do
      begin
    	StrLCopy(stringToAdd, note, 80);
    	stringForProfile := searchForString + StrPas(stringToAdd);
      withinNote := @(withinNote[StrLen(stringToAdd)]); {does this pointer arithmetic really work in pascal?}
      self.strings.add(stringForProfile);
      end;
    end;
  end;

{import export functions}
procedure GsProfile.openImportFile(const fileName: string);
  begin
  assignFile(importFile, fileName);
  reset(importFile);
  nextSection := '';
  end;

procedure GsProfile.openExportFile(const fileName: string);
  begin
  assignFile(exportFile, fileName);
  rewrite(exportFile);
  end;

procedure GsProfile.importNextSection;
  var
    inputString: string;
  begin
  strings.clear;
  currentSection := '';
  if nextSection = '' then
    {skip till find section}
  	while not eof(importFile) do
    	begin
    	readln(importFile, inputString);
    	if pos('[', inputString) = 1 then
        begin
        currentSection := inputString;
      	break;
        end;
    end
  else
    currentSection := nextSection;
  nextSection := 'DONE';
  if currentSection = '' then exit;
  while not eof(importFile) do
    begin
    readln(importFile, inputString);
    if pos('[', inputString) = 1 then
      begin
      nextSection := inputString;
      break;
      end;
    strings.add(inputString);
    end;
  end;

function GsProfile.importDone: boolean;
  begin
  result := eof(importFile);
  end;

procedure GsProfile.exportNextSection(const sectionName: string);
  var i: longint;
  begin
  writeln(exportFile, sectionName);
  {write out strings}
  if strings.count > 0 then
    for i := 0 to strings.count - 1 do
      writeln(exportFile, strings.strings[i]);
  end;

procedure GsProfile.closeImportFile;
  begin
  CloseFile(importFile);
  end;

procedure GsProfile.closeExportFile;
  begin
  CloseFile(exportFile);
  end;


end.
{
	Copyright (c) 1996 Paul D. Fernhout and Cynthia F. Kurtz
  								 All rights reserved

  Notice: This program contains trade secrets that are the property of
  Paul D. Fernhout and Cynthia F. Kurtz. The contents may not be used or disclosed in part or
  in whole without express written permission of the owners.
  Internet: pdfernhout @ bix.com, cfkurtz @ igc.apc.org, kfsoft @ netins.net
}


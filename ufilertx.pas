unit Ufilertx;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ufilertx: A filer for tab-delimited text files. Used only for text versions of templates.
Some assumptions about tab-delimited text files:
1) each object takes up one line;
2) each object has a version number and type;
3) the number of columns is constant as is the order (if the number of columns is wrong it will choke); and
4) objects are identified by their names and therefore cannot have duplicate names (if they do,
the duplicate options dialog appears - uduplic).
Another complication is that Excel (and possibly other spreadsheets) puts double quotes around any cell
in a tab-delimited file that contains a comma (,) to make sure the comma isn't misunderstood as a secondary
delimiter. This means if we write out a file with some cells containing commas, edit the file in Excel,
save it from Excel as a tab-delimited file, then read the file again, the cells containing commas will
'magically' have double quotes as their first and last characters. To get around this problem, for text
data stored in Delphi as a string variable, the text filer strips off double quotes in the first and
last positions (only) when they occur. For text data stored in Delphi as a PChar (notes) the text filer strips
out ANY double quotes that are not preceded by a slash.
}

interface

uses WinTypes, Graphics, uestruct;

type

  GsTextFilerMode = (textFilerModeReading, textFilerModeWriting);

const
  kOverwriteAllDuplicates = 0;
  kSkipAllDuplicates = 1;
  kAskOnEachDuplicate = 2;

  kSkipThisNewObject = 0;
  kRenameThisNewObject = 1;
  kSkipRemainingObjectsOfThisType = 2;
  kOverwriteThisExistingObject = 3;
  kRenameRemainingObjectsOfThisType = 4;
  kStopImporting = 5;

  kTextFilerCurrentVersionString = 'v1.0';
  kTextFilerDelimiter = '|';

type

	GsTextFiler = class(TObject)
    public
    theTextFile: TextFile;
    fileIsValid: boolean;
  	mode: GsTextFilerMode;
    atEndOfFile: boolean;
    writingHeader: boolean;
    whatToDoWithDupicates: smallint;
    lastWhatToDoChoice: smallint;
    resourceProvider: TObject; {an object that can be used to look up shared resource references}
    versionString: string;
    constructor createWithFileNameForReading(aFileName: string);
    constructor createWithFileNameForWriting(aFileName: string);
    destructor destroy; override;
    function readUntilTab: string;
    procedure skipRestOfLine;
    function isReading: boolean;
    function isWriting: boolean;
    procedure writeLiteralString(aString: string);
    function writeHeader(header: string): boolean;
		procedure streamString(var aString: string; header: string);
    procedure streamPChar(var aPChar: PChar; header: string);
    procedure streamBoolean(var aBoolean: boolean; header: string);
    procedure streamSmallint(var aSmallint: smallint; header: string);
    procedure streamSmallintWithLabel(var aSmallint: smallint; aLabel, header: string);
    procedure streamLongint(var aLongint: longint; header: string);
    procedure streamSingle(var aSingle: single; header: string);
    procedure streamColorRef(var aColorRef: TColorRef; header: string);
    procedure streamPoint3d(var x: single; var y: single; var z: single; header: string);
    procedure streamTriangle(aTriangleProxy: TObject; header: string);
    procedure streamArrayThree(var anArray: arrayThree; header: string);
    procedure streamArrayFour(var anArray: arrayFour; header: string);
    procedure streamArrayMonths(var anArray: arrayMonths; header: string);
    procedure streamArrayLayers(var anArray: arraySoilLayers; header: string);
    procedure streamArrayWindDirection(var anArray: windDirectionsArray; header: string);
    procedure streamIcon(anIcon: TIcon);
    procedure streamAlphaCodeByte(var oneByte: byte);
    procedure streamSingleVarSCurve(var aSingleVarSCurve: SingleVarSCurveStructure; header: string);
    procedure streamSCurve(var sCurve: SCurveStructure; header: string);
    procedure streamStringWithNonTabDelimiter(var aString: string; header: string);
    procedure streamBooleanWithNonTabDelimiter(var aBoolean: boolean; header: string);
    procedure streamTab;
    function readUntilNonTabDelimiter: string;
    procedure streamEndOfLine;
    function objectStringForType(objectType: smallint): string;
    function objectTypeForString(objectString: string): smallint;
    function askWhatToDoWithDuplicates(objectType: smallint; objectName: string): smallint;
    function canStreamVersion(versionStringRead: string): boolean;
		end;

implementation

uses SysUtils, WinProcs, Dialogs, Forms, Controls, Classes, uaspects, uduplic, usupport, usstream, uturt3d;

var ignore: string;

constructor GsTextFiler.createWithFileNameForReading(aFileName: string);
  begin
  self.create;
  self.mode := textFilerModeReading;
  whatToDoWithDupicates := kAskOnEachDuplicate;
  lastWhatToDoChoice := kOverwriteThisExistingObject;
  assignFile(theTextFile, aFileName);
  try
    reset(theTextFile);
    fileIsValid := true;
  except
    fileIsValid := false;
    messageDlg('Could not open file ' + aFileName + ' for reading.', mtError, [mbOK], 0);
    raise; {caller should have try-finally loop}
  end;
  { caller must set version string before each object read if they need it - set in canStreamVersion }
  versionString := kTextFilerCurrentVersionString;
  end;

constructor GsTextFiler.createWithFileNameForWriting(aFileName: string);
  begin
  self.create;
  self.mode := textFilerModeWriting;
  assignFile(theTextFile, aFileName);
  try
    rewrite(theTextFile);
    fileIsValid := true;
  except
    fileIsValid := false;
    messageDlg('Could not open file ' + aFileName + ' for writing.', mtError, [mbOK], 0);
    raise; {caller should have try-finally loop}
  end;
  { if writing, versionString is alway current version string }
  versionString := kTextFilerCurrentVersionString;
  end;

destructor GsTextFiler.destroy;
  begin
  try
    closeFile(theTextFile);
  except
    fileIsValid := false;
    {nothing else?}
  end;
  end;

function GsTextFiler.canStreamVersion(versionStringRead: string): boolean;
  begin
  result := false;
  { add new versions here as they are made }
  if (versionStringRead = 'v0.9')
    or (versionStringRead = kTextFilerCurrentVersionString) then
    begin
    self.versionString := versionStringRead;
    result := true;
    end;
  end;

function GsTextFiler.objectStringForType(objectType: smallint): string;
  begin
  result := '';
  case objectType of
    kObjectTypeSoil: result := 'soil type';
    kObjectTypePlant: result := 'cultivar';
    kObjectTypeWeather: result := 'climate';
    kObjectTypeBag: result := 'bag';
    kObjectTypeHarvestItemTemplate: result := 'harvest item';
    kObjectType3DObject: result := '3D object';
    kObjectTypeIcon: result := 'icon';
  else
    raise Exception.create('GsTextFiler.objectStringForType: Unsupported object type');
  end;
  end;

function GsTextFiler.objectTypeForString(objectString: string): smallint;
  begin
  if lowerCase(objectString) = 'soil type' then
    result := kObjectTypeSoil
  else if lowerCase(objectString) = 'cultivar' then
    result := kObjectTypePlant
  else if lowerCase(objectString) = 'climate' then
    result := kObjectTypeWeather
  else if lowerCase(objectString) = 'bag' then
    result := kObjectTypeBag
  else if lowerCase(objectString) = 'harvest item' then
    result := kObjectTypeHarvestItemTemplate
  else if lowerCase(objectString) = '3d object' then
    result := kObjectType3DObject
  else if lowerCase(objectString) = 'icon' then
    result := kObjectTypeIcon
  else raise Exception.create('GsTextFiler.objectTypeForString: Bad object type string');
  end;

function GsTextFiler.isReading : boolean;
	begin
  result := (mode = textFilerModeReading);
  end;

function GsTextFiler.isWriting: boolean;
	begin
  result := (mode = textFilerModeWriting);
  end;

procedure GsTextFiler.streamEndOfLine;
  var
    endString: string;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    writeln(theTextFile, 'end')
  else
    skipRestOfLine;
  end;

procedure GsTextFiler.streamString(var aString: string; header: string);
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if writingHeader then
      write(theTextFile, header)
    else
      write(theTextFile, aString);
    write(theTextFile, chr(9));
    end
  else
    begin
    aString := self.readUntilTab;
    { remove quotes placed by spreadsheet around string if it has a comma in it }
    if (aString[1] = '"') and (aString[length(aString)] = '"') then
      aString := copy(aString, 2, length(aString) - 2);
    end;
  end;

procedure GsTextFiler.streamStringWithNonTabDelimiter(var aString: string; header: string);
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if writingHeader then
      write(theTextFile, header)
    else
      write(theTextFile, aString);
    write(theTextFile, kTextFilerDelimiter);
    end
  else
    begin
    aString := self.readUntilNonTabDelimiter;
    end;
  end;

procedure GsTextFiler.streamBooleanWithNonTabDelimiter(var aBoolean: boolean; header: string);
  var booleanString: string;
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if writingHeader then
      write(theTextFile, header)
    else
      write(theTextFile, boolToStr(aBoolean));
    write(theTextFile, kTextFilerDelimiter);
    end
  else
    begin
    booleanString := self.readUntilNonTabDelimiter;
    if (lowerCase(booleanString) = 'true') or (lowerCase(booleanString) = 'yes') then
      aBoolean := true
    else if (lowerCase(booleanString) = 'false') or (lowerCase(booleanString) = 'no') then
      aBoolean := false
    else
      raise Exception.create('Improper file format for boolean, got: ' + booleanString);
    end;
  end;

procedure GsTextFiler.streamTab;
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    write(theTextFile, chr(9))
  else if self.isReading then
    ignore := readUntilTab;
  end;

procedure GsTextFiler.writeLiteralString(aString: string);
  begin
  if not fileIsValid then exit;
  if self.isReading then exit;
  write(theTextFile, aString);
  write(theTextFile, chr(9));
  end;

function GsTextFiler.writeHeader(header: string): boolean;
  begin
  result := false;
  if not self.writingHeader then exit;
  write(theTextFile, header + chr(9));
  result := true;
  end;

procedure GsTextFiler.streamPChar(var aPChar: PChar; header: string);
  var
    streamingChar, tabOrCRCodeChar, tempChar: char;
    stringLength, streamSize: longint;
    i: longint;
    memoryStream: TMemoryStream;
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    if aPChar = nil then
      stringLength := 0
    else
      stringLength := StrLen(aPChar);
    if stringLength > 0 then
      begin
      i := 0;
      while i <= stringLength - 1 do
      	begin
      	streamingChar := aPChar[i];
      	if streamingChar = chr(13) then
        	begin
        	write(theTextFile, '\r');
        	{ if line-feed after carriage-return, skip over }
        	if (i < stringLength - 1) and (aPChar[i+1] = chr(10)) then
          	inc(i);
        	end
      	else if streamingChar = chr(9) then
        	write(theTextFile, '\t')
      	else
        	write(theTextFile, streamingChar);
      	inc(i);
      	end;
      end;
    write(theTextFile, chr(9));
    end
  else {reading}
    begin
    memoryStream := TMemoryStream.create;
    try
    if aPChar <> nil then
      begin
      StrDispose(aPChar);
      aPChar := nil;
      end;
    streamingChar := chr(0);
    while not eof(theTextFile) do
      begin
      read(theTextFile, streamingChar);
      if streamingChar = chr(9) then
        break;
      if (streamingChar = chr(13)) then
        begin
        if not eof(theTextFile) then
          begin
          read(theTextFile, streamingChar);
          if streamingChar <> chr(10) then
            raise Exception.create('Problem in file input');
          end;
        break;
        end;
      { ignore double quote if not preceded by slash }
      if (streamingChar = '"') then
        continue;
      if streamingChar = '\' then
        begin
        read(theTextFile, tabOrCRCodeChar);
        if (tabOrCRCodeChar = 't') then
          begin
          tempChar := chr(9);
          memoryStream.write(tempChar, 1);
          end
        else if (tabOrCRCodeChar = 'r') then
          begin
          tempChar := chr(13);
          memoryStream.write(tempChar, 1);
          end
        { allow double quote (unchanged) if preceded by slash }
        else if (tabOrCRCodeChar = '"') then
          begin
          tempChar := '"';
          memoryStream.write(tempChar, 1);
          end
        else
          begin
          tempChar := streamingChar;
          memoryStream.write(tempChar, 1);
          tempChar := tabOrCRCodeChar;
          memoryStream.write(tempChar, 1);
          end;
        end
      else
        begin
        tempChar := streamingChar;
        memoryStream.write(tempChar, 1);
        end;
      end;
    memoryStream.position := 0;
    streamSize := memoryStream.size - memoryStream.position;
    { extra byte for null terminator }
    aPChar := StrAlloc(streamSize + 1);
    if aPChar = nil then raise Exception.create('Out of memory');
    if streamSize > 0 then
      begin
      for i := 0 to streamSize - 1 do
        memoryStream.read(aPChar[i], 1);
      { add null terminator }
      aPChar[streamSize] := #0;
      end;
    finally
    memoryStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamBoolean(var aBoolean: boolean; header: string);
  var booleanString: string;
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    write(theTextFile, boolToStr(aBoolean));
    write(theTextFile, chr(9));
    end
  else
    begin
    booleanString := readUntilTab;
    if (lowerCase(booleanString) = 'true') or (lowerCase(booleanString) = 'yes') then
      aBoolean := true
    else if (lowerCase(booleanString) = 'false') or (lowerCase(booleanString) = 'no') then
      aBoolean := false
    else
      raise Exception.create('Improper file format for boolean, got: ' + booleanString);
    end;
  end;

procedure GsTextFiler.streamSmallint(var aSmallint: smallint; header: string);
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    write(theTextFile, intToStr(aSmallint));
    write(theTextFile, chr(9));
    end
  else
    begin
    read(theTextFile, aSmallint);
    ignore := readUntilTab;
    end;
  end;

procedure GsTextFiler.streamSmallintWithLabel(var aSmallint: smallint; aLabel, header: string);
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    write(theTextFile, intToStr(aSmallint));
    write(theTextFile, ' ' + aLabel);
    write(theTextFile, chr(9));
    end
  else
    begin
    read(theTextFile, aSmallint);
    ignore := readUntilTab;
    end;
  end;

procedure GsTextFiler.streamLongint(var aLongint: longint; header: string);
 	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    write(theTextFile, intToStr(aLongint));
    write(theTextFile, chr(9));
    end
  else
    begin
    read(theTextFile, aLongint);
    ignore := readUntilTab;
    end;
  end;

procedure GsTextFiler.streamSingle(var aSingle: single; header: string);
  var valueString: string;
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    write(theTextFile, digitValueString(aSingle));
    write(theTextFile, chr(9));
    end
  else
    begin
    valueString := readUntilTab;
    boundForString(valueString, kFieldFloat, aSingle);
    end;
  end;

procedure GsTextFiler.streamColorRef(var aColorRef: TColorRef; header: string);
  var
    colorString: string;
    r, g, b: smallint;
	begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header) then exit;
    colorString := intToStr(getRValue(aColorRef)) + ' ' + intToStr(getGValue(aColorRef))
        + ' ' + intToStr(getBValue(aColorRef));
    write(theTextFile, colorString);
    write(theTextFile, chr(9));
    end
  else
    begin
    read(theTextFile, r, g, b);
    ignore := readUntilTab;
    aColorRef := rgb(r, g, b);
    end;
  end;

procedure GsTextFiler.streamPoint3d(var x: single; var y: single; var z: single; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header + 'X' + kTextFilerDelimiter + header + 'Y' + kTextFilerDelimiter + header + 'Z') then exit;
    write(theTextFile, intToStr(round(x)) + kTextFilerDelimiter + intToStr(round(y))
        + kTextFilerDelimiter + intToStr(round(z)));
    write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      x := stringStream.nextInteger;
      y := stringStream.nextInteger;
      z := stringStream.nextInteger;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamTriangle(aTriangleProxy: TObject; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
    aTriangle: KfIndexTriangle;
  begin
  if not fileIsValid then exit;
  aTriangle := aTriangleProxy as KfIndexTriangle;
  if self.isWriting then
    begin
    if self.writeHeader(header + '1' + kTextFilerDelimiter + header + '2' + kTextFilerDelimiter + header + '3') then exit;
    write(theTextFile, intToStr(aTriangle.points[0]) + kTextFilerDelimiter + intToStr(aTriangle.points[1])
        + kTextFilerDelimiter + intToStr(aTriangle.points[2]));
    write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      aTriangle.points[0] := stringStream.nextInteger;
      aTriangle.points[1] := stringStream.nextInteger;
      aTriangle.points[2] := stringStream.nextInteger;
    finally
      stringStream.free;
    end;
    end;
  end;

const
  kMaxArrayMonths = 11;
  kMaxArrayLayers = 9;
  kMaxArrayDirections = 15;
  kMaxArrayThree = 2;
  kMaxArrayFour = 3;

procedure GsTextFiler.streamArrayThree(var anArray: arrayThree; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
    i: smallint;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    for i := 0 to kMaxArrayThree do
      begin
      if self.writingHeader then
        write(theTextFile, header + intToStr(i+1))
      else
        write(theTextFile, digitValueString(anArray[i]));
      if i < kMaxArrayThree then write(theTextFile, kTextFilerDelimiter);
      end;
     write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      for i := 0 to kMaxArrayThree do
        anArray[i] := stringStream.nextSingle;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamArrayFour(var anArray: arrayFour; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
    i: smallint;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    for i := 0 to kMaxArrayFour do
      begin
      if self.writingHeader then
        write(theTextFile, header + intToStr(i+1))
      else
        write(theTextFile, digitValueString(anArray[i]));
      if i < kMaxArrayFour then write(theTextFile, kTextFilerDelimiter);
      end;
     write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      for i := 0 to kMaxArrayFour do
        anArray[i] := stringStream.nextSingle;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamArrayMonths(var anArray: arrayMonths; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
    i: smallint;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    for i := 0 to kMaxArrayMonths do
      begin
      if self.writingHeader then
        write(theTextFile, header + intToStr(i+1))
      else
        write(theTextFile, digitValueString(anArray[i]));
      if i < kMaxArrayMonths then write(theTextFile, kTextFilerDelimiter);
      end;
     write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      for i := 0 to kMaxArrayMonths do
        anArray[i] := stringStream.nextSingle;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamArrayLayers(var anArray: arraySoilLayers; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
    i: smallint;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    for i := 0 to kMaxArrayLayers do
      begin
      if self.writingHeader then
        write(theTextFile, header + intToStr(i+1))
      else
        write(theTextFile, digitValueString(anArray[i]));
      if i < kMaxArrayLayers then write(theTextFile, kTextFilerDelimiter);
      end;
     write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      for i := 0 to kMaxArrayLayers do
        anArray[i] := stringStream.nextSingle;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamArrayWindDirection(var anArray: windDirectionsArray; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
    month, direction: smallint;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    for month := 0 to kMaxArrayMonths do
      begin
      for direction := 0 to kMaxArrayDirections do
        begin
        if self.writingHeader then
          write(theTextFile, header + intToStr(month+1) + ' dir ' + intToStr(direction+1))
        else
          write(theTextFile, digitValueString(anArray[month][direction]));
        if direction < kMaxArrayDirections then write(theTextFile, kTextFilerDelimiter);
        end;
       write(theTextFile, chr(9));
       end;
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
    for month := 0 to kMaxArrayMonths do
      begin
      inputString := readUntilTab;
      stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
      for direction := 0 to kMaxArrayDirections do
        anArray[month][direction] := stringStream.nextSingle;
      end;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamIcon(anIcon: TIcon);
  var
    memoryStream: TMemoryStream;
    iconSize: longint;
    oneByte: byte;
    i: longint;
  begin
  memoryStream := TMemoryStream.create;
  try
  if self.isWriting then
    begin
    if self.writeHeader('iconSize') and self.writeHeader('icon') then exit;
    anIcon.SaveToStream(memoryStream);
    memoryStream.position := 0;
    iconSize := memoryStream.size;
    self.streamLongint(iconSize, 'iconSize');
    for i := 0 to iconSize - 1 do
      begin
      memoryStream.read(oneByte, 1);
      self.streamAlphaCodeByte(oneByte);
      if (i > 0) and (i mod 32 = 0) then
        write(theTextFile, chr(9));
      end;
    write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    self.streamLongint(iconSize, 'iconSize');
    for i := 0 to iconSize - 1 do
      begin
      self.streamAlphaCodeByte(oneByte);
      memoryStream.write(oneByte, 1);
      if (i > 0) and (i mod 32 = 0) then
        ignore := readUntilTab;
      end;
    memoryStream.position := 0;
    anIcon.loadFromStream(memoryStream);
    ignore := readUntilTab;
    end
  finally
  memoryStream.free;
  end;
  end;

procedure GsTextFiler.streamAlphaCodeByte(var oneByte: byte);
  var
    firstChar, secondChar: char;
  begin
  if self.isWriting then
    begin
    firstChar := chr(65 + oneByte div 16);    {65 is letter A}
    secondChar := chr(65 + oneByte mod 16);
    write(theTextFile, firstChar);
    write(theTextFile, secondChar);
    end
  else if self.isReading then
    begin
    read(theTextFile, firstChar);
    read(theTextFile, secondChar);
    oneByte := (ord(firstChar) - 65) * 16 + (ord(secondChar) - 65); 
    end;
  end;

procedure GsTextFiler.streamSingleVarSCurve(var aSingleVarSCurve: SingleVarSCurveStructure; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(header + 'X' + kTextFilerDelimiter + header + 'Y' + kTextFilerDelimiter
        + header + 'C') then exit;
     write(theTextFile, digitValueString(aSingleVarSCurve.x) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(aSingleVarSCurve.y) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(aSingleVarSCurve.c) + kTextFilerDelimiter);
     write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
    inputString := readUntilTab;
    stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
    aSingleVarSCurve.x := stringStream.nextSingle;
    aSingleVarSCurve.y := stringStream.nextSingle;
    aSingleVarSCurve.c := stringStream.nextSingle;
    finally
      stringStream.free;
    end;
    end;
  end;

procedure GsTextFiler.streamSCurve(var sCurve: SCurveStructure; header: string);
  var
    stringStream: KfStringStream;
    inputString: string;
  begin
  if not fileIsValid then exit;
  if self.isWriting then
    begin
    if self.writeHeader(
          header + 'X1' + kTextFilerDelimiter + header + 'Y1' + kTextFilerDelimiter
        + header + 'X2' + kTextFilerDelimiter + header + 'Y2' + kTextFilerDelimiter
        + header + 'C1' + kTextFilerDelimiter + header + 'C2') then exit;
     write(theTextFile, digitValueString(sCurve.x1) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(sCurve.y1) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(sCurve.x2) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(sCurve.y2) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(sCurve.c1) + kTextFilerDelimiter);
     write(theTextFile, digitValueString(sCurve.c2));
     write(theTextFile, chr(9));
    end
  else if self.isReading then
    begin
    stringStream := KfStringStream.create;
    try
    inputString := readUntilTab;
    stringStream.onStringSeparator(inputString, kTextFilerDelimiter);
    sCurve.x1 := stringStream.nextSingle;
    sCurve.y1 := stringStream.nextSingle;
    sCurve.x2 := stringStream.nextSingle;
    sCurve.y2 := stringStream.nextSingle;
    sCurve.c1 := stringStream.nextSingle;
    sCurve.c2 := stringStream.nextSingle;
    finally
      stringStream.free;
    end;
    end;
  end;

function GsTextFiler.readUntilTab: string;
  begin
  result := usupport.readUntilTab(theTextFile);
  atEndOfFile := eof(theTextFile);
  end;

function GsTextFiler.readUntilNonTabDelimiter: string;
  var aChar: char;
  begin
  aChar := chr(0);
  result := '';
  while not eof(theTextFile) do
    begin
    read(theTextFile, aChar);
    if (aChar <> kTextFilerDelimiter) and (aChar <> chr(9)) and (aChar <> chr(10)) and (aChar <> chr(13)) then
      result := result + aChar
    else
      begin
      if (aChar = chr(13)) and not eof(theTextFile) then
        begin
        read(theTextFile, aChar);
        if aChar <> chr(10) then
          raise Exception.create('Problem in file input');
        end;
      break;
      end;
    end;
  atEndOfFile := eof(theTextFile);
  end;

procedure GsTextFiler.skipRestOfLine;
  begin
  readln(theTextFile);
  atEndOfFile := eof(theTextFile);
  end;

function nameForOptionString(index: smallint): string;
  begin
  case index of
    kSkipThisNewObject: result := 'skip new ';
    kRenameThisNewObject: result := 'rename new ';
    kSkipRemainingObjectsOfThisType: result := 'skip all duplicate ';
    kOverwriteThisExistingObject: result := 'overwrite existing ';
    else
      raise Exception.create('nameForOptionString: option not supported');
    end;
  end;

function GsTextFiler.askWhatToDoWithDuplicates(objectType: smallint; objectName: string): smallint;
  var
    duplicateObjectForm: TDuplicateObjectForm;
    response: integer;
    doWithThisObject: smallint;
    objectString: string;
	begin
  objectString := self.objectStringForType(objectType);
  result := kStopImporting;
  doWithThisObject := kStopImporting;
  response := mrCancel;
  duplicateObjectForm := TDuplicateObjectForm.create(Application);
  if duplicateObjectForm = nil then
    raise Exception.create('Could not create duplicate object window');
  try
    with duplicateObjectForm.optionsForThisObject do
      begin
      items.clear;
      items.add(nameForOptionString(kSkipThisNewObject) + objectString);
      items.add(nameForOptionString(kRenameThisNewObject) + objectString);
      items.add(nameForOptionString(kSkipRemainingObjectsOfThisType) + objectString + 's');
      if (objectType <> kObjectTypeHarvestItemTemplate) and (objectType <> kObjectType3DObject)
        and (objectType <> kObjectTypeIcon) then
        items.add(nameForOptionString(kOverwriteThisExistingObject) + objectString);
      itemIndex := self.lastWhatToDoChoice;
      end;
    duplicateObjectForm.objectName.text := objectName;
    duplicateObjectForm.optionsForAllObjects.itemIndex := self.whatToDoWithDupicates;
    duplicateObjectForm.duplicateTypeLabel.caption := 'There is already one ' + objectString + ' with this name.';
    response := duplicateObjectForm.showModal;
    doWithThisObject := duplicateObjectForm.optionsForThisObject.itemIndex;
    self.whatToDoWithDupicates := duplicateObjectForm.optionsForAllObjects.itemIndex;
    self.lastWhatToDoChoice := doWithThisObject;
  finally
    duplicateObjectForm.free;
    duplicateObjectForm := nil;
  end;
  if response = mrOK then
    result := doWithThisObject
  else if response = mrCancel then
    result := kStopImporting;
	end;

end.

unit Utab2asp;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utab2asp: This separate program (with its own project files) is used primarily to generate
several files of code that create the 800 or so aspects when the program starts up.
Code generation works from a data file (aspects.tab) with all information for the aspects
in tab-delimited format. The tab2asp program also does some other housekeeping
and comparison functions for general maintenance of the code. By the buttons on the
form, these are as follows.
* Generate aspect code: The function discussed above (the program's primary function).
* Generate aspect report: Compares list of existing aspects (as read from aspects.tab)
with model variables marked in the code files uestruct, udplant and uebag.
The report file (asp_rep.txt) tells 1) what variables are in the aspects file but not in the
Pascal files; 2) what variables are in the Pascal files but not in the aspects file;
and 3) what variables have conflicting data types in the Pascal and aspects files.
In uestruct, udplant and uebag, aspect variables are designated by special
codes. The special codes are:
  aspects->start  Start reading variables here.
  object->plant   Start reading variables for the 'plant' object here.
  access->params  Start reading variables for the 'params' data structure here.
  ENUM            Consider this variable an enumerated string variable, not just an integer.
  NA              Don't look for an aspect for this variable. After each NA variable there
                  is usually an explanation of why there is no aspect for it.
  aspects->stop   Stop reading variables here.
* Generate group aspect report: Creates a report file (asp_grp.txt) showing discrepancies
between the aspects file (aspects.tab) and a groups tab-delimited file (created in the
group editor by exporting the groups). The report shows 1) which aspects are in aspects.tab
but not in groups.tab; 2) which aspects are in groups.tab but not in aspects.tab; and
3) which aspects are in groups.tab twice. The report always produces some discrepancies
that are not real, mainly for aspects in aspects.tab that have hard-coded array indexes.
* Generate button hints file from code: The long hints used for buttons, list boxes, etc
come from a tab-delimited file called hints.tab. If you add new windows or substantially
change existing ones, you must update hints.tab to reflect the changes. This function reads
all the Pascal files in the directory you choose, pulls out the names and types of all
the 'important' items on each form, and writes out a new hints.tab file. It also asks
you to choose an existing hints.tab file, and writes the hints for any existing items
in that file into the new file. If you created a new form, for example, then ran this,
you would end up with a new hints file (newhints.tab) with all the old hints plus
lines with blank hint cells for the items on your new form. Which items are 'important'
is hard-coded below with a list of enumerated item types (find the 'componentConstants'
list below).
* Merge aspects file with existing hints: Hints for each aspect are read when the program
starts from a tab-delimited text file, usually asphints.tab. If you add new aspects or
delete aspects, you can run this function to create a new aspect hints file (newhints.tab)
from the aspects file (aspects.tab) and your existing aspect hints file. For example,
if you add two new aspects then run this function, you will end up with a newhints.tab
with all the existing hints and blank cells on the lines for your two new aspects.
Generate aspects help file: Reads an aspects file (aspects.tab), an aspect hints file
(asphints.tab), and a groups file (groups.tab, exported from the group editor), and creates
an RTF (rich-text format) file with aspect hints for all the aspects in the groups,
ordered by group and the order they are in each group. The RTF file can be read
by Microsoft Word (and possibly other word processors) to format a document for
printing or for use in the help system.

We also include here a description of what is in each column of the aspects.tab file. Keeping
the aspects information in the tab file and generating Pascal code from it makes it easier
to compare aspects (by simply sorting the file) and make changes to many aspects at once. When
you sort, make sure you keep the row of labels at the top of the file.
* FieldID: A text string (no spaces, only alphanumeric) that uniquely identifies the aspect.
Often taken from the variable name in the object, but not always, and sometimes the variable
name changed later. The aspects.tab file should always be sorted by the FieldID column when
you are finished with it and when you run the tab2asp program. The fieldIDs are named
so that the objects come out sorted when the FieldIDs are sorted.
* name: What shows in the browser (etc) when the aspect is shown. Actually, this column
is not used anymore because the aspect hints file (asphints.tab) has a 'name' column and
we use that column instead to show in the browser, etc. So this is really obsolete. If
you want to change how an aspect shows in the program, change the 'name' column in
asphints.tab (that does not require recompiling, either).
* fieldType: The data type of the aspect. The types are: float, integer, unsigned long,
unsigned char, string, file string (unused), color, boolean, icon, 3D object, enumerated
list (shown in the browser as a radio button or check box group), and harvest item template.
This list of constants is in uaspects.pas. You could add a new type by adding it to this
list and checking for any case statements you need to add it to. NOTE that in each of the
columns here that start with a number, only the initial number is read by the tab2asp program,
but the text is for your reference. It's best never to type these cells but to copy and
paste from another cell that contains the same thing, so you don't get the wrong number for
what you want. 
* IndexType: The type of array this represents. The types are: none, soil layer (array of 10),
month (for weather, array of 12), s curve (array of 4 singles), soil texture triangle (array of 3),
male/female (array of 2), harvest item types (array of 12), weather autocorrelation matrix (array of 3),
MUSI coefficients (array of 4). This list of constants is in uaspects.pas. You could add a new
type by adding it to this list and checking for any case statements you need to add it to.
* unitSet: The unit set. The list of 32 unit sets is in uunits. See that file for more
on unit conversions.
* unitModel: The type of unit (within its unit set) in which the model variable is stored. This
is very important because if this unit is wrong the browser will report the wrong number.
* unitMetric: The type of unit (within its unit set) to show when the user chooses to set
all units to metric. Usually these are determined by the unit set (concentration, weight, etc).
* unitEnglish. The type of unit (within its unit set) to show when the user chooses to set
all uints to English.
* SLower: The 'soft' lower bound. Soft bounds are our recommended bounds for displaying values.
For input values, the user can override the soft bounds to input a value outside of them.
We made these up mostly by looking at the unit and variable types and thinking of the normal garden scale
(biomass, for example, will usually be in grams or kilograms, not tons).
* SUpper: The 'soft' upper bound.
* HLower: The 'hard' lower bound. For input variables, the user cannot input a value outside
of them. Most of the hard bounds came from EPIC (see the Source column below); some of them
came from our own guesses as to what extreme values might be. In some cases we might have
set the hard bounds too restrictingly. We tried to stop values from reaching extremes that might
crash the simulation. Of course read-only values need no bounds so they have mostly just
initial display meanings for output variables.
* HUpper: The 'hard' upper bound.
* Source: Where the hard or absolute bounds came from. Usually these are from EPIC,
from the unit set, or from our guesses.
* Type: Whether the variable is an input parameter, a state variable, or an output variable.
These distinctions are not extremely important or accurate. The 'parameter' versus everything
else distinction is used in the group editor to choose what to look at, but otherwise this
column is not used by anything.
* ReadOnly: Whether the user can change the value for this aspect in the browser. We used
to have more changeable aspects, but in our run-through trying to make the code a little
less vulnerable to messy side effects, we removed a lot of these. In most cases variables
the user can change affect other variables either 1) right away (in which case there
will be a side effect function in the object for it), 2) during the next day of simulation
(in which case there will be no effect until you run the simulation), or 3) not at all.
It is not advisable to make read-only variables changeable without at least perusing the
code to look for potentially damaging side effects.
* HasHelp: Whether this aspect has a help topic in the help system. This column is not
used anymore because we decided not to implement separate help topics for all 800 aspects.
* Access: A VERY IMPORTANT COLUMN. This is how the data for this aspect is retrieved from its model
object. Any changes to model code which change variable names MUST update this field, or
the program will choke when it attempts to fetch data for the aspect (when drawing the
browser, group editor, or graph window).
* Transfer: Also important - HOW the data for this aspect is collected.
- MFD: The transfer type is MFD (MoveFieldData), a simple
one-for-one data transfer using the Access column string.
- S Curve, 3D object, harvest item: These is nearly the same as one-for-one transfer,
but a little fancy work is done in maintaining the special structures.
- BDConvert: Here the transfer type is a basic-derived (BD) conversion. In
basic-derived conversion, the next three sets of ten columns (D1, D2, D3) each come into play. They
specify alternate ways in which the aspect can be derived from the simulation variable
described in the Access column. For example, the aspect named 'Actual increase in biomass today'
can be shown in its 'basic' derivation as kg/m2 (under the 'unitMetric' column), OR it can
be shown in its 'first derived' derivation as grams (under the 'D1 metric' column). The
D1, D2, and D3 sets of columns determine how each alternative derivation (up to three)
is calculated from the simulation variable. Note that so far we have no 'D3' derivations.
The remaining columns in the file are as follows and repeat similar columns for
the derived representations.
D1 type	D1 set	D1 model	D1 metric	D1 English	D1 BSL	D1 BSU	D1 BHL	D1 BHU	D1 source
D2 type	D2 set	D2 model	D2 metric	D2 English	D2 BSL	D2 BSU	D2 BHL	D2 BHU	D2 source
D3 type	D3 set	D3 model	D3 metric	D3 English	D3 BSL	D3 BSU	D3 BHL	D3 BHU	D3 source}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

const

  kHeaderDelimiter = '->';
  kNoAspectCode = '{NA}';
  kEnumCode = '{ENUM}';
  kObjectHeader = 'object';
  kStructureHeader = 'access';

	{ ObjectType }
  kObjectUndefined = 0;
  kObjectTypeSoil = 1;
  kObjectTypePlant = 2;
  kObjectTypeWeather = 3;
  kObjectTypeGarden = 4;
  kObjectTypeBlob = 5;
  kObjectTypeFruit = 6;
  kObjectTypePesticide = 7;
  kObjectTypeBag = 8;
  kObjectTypeDrawingPlant = 9;
  kObjectTypeLast = 9;

  { transferType }
  kTransferTypeUndefined = 0;
  kTransferTypeMFD = 1;
  kTransferTypeBDConvert = 2;
  kTransferTypeGetSetSCurve = 3;
  kTransferTypeHarvestItemTemplate = 4;
  kTransferTypeObject3D = 5;
  kTransferTypeStorageOrganLumping = 6;
  kTransferTypeHarvestBundling = 7;
  kTransferTypeDerived = 100;

  { deriveType }
  kNotDerived = 0;
  kDeriveTypeDepth = 1;
  kDeriveTypeConcentration = 2;
  kDeriveTypeArea = 3;
  kDeriveTypeConcentrationFromPercent = 4;

  kMaxNumDerivations = 3;

  { FieldType }
  kFieldUndefined = 0;
  kFieldFloat = 1;
  kFieldInt = 2;
  kFieldUnsignedLong = 3;
  kFieldUnsignedChar = 4;
  kFieldString = 5;
  kFieldFileString = 6;
  kFieldColor = 7;
  kFieldBoolean = 8;
  kFieldIcon = 9;
  kFieldThreeDObject = 10;
  kFieldEnumeratedList = 11;
  kFieldHarvestItemTemplate = 12;

	{ IndexType }
  kIndexTypeUndefined = 0;
  kIndexTypeNone = 1;
  kIndexTypeLayer = 2;
  kIndexTypeMonth = 3;
  kIndexTypeSCurve = 4;
  kIndexTypeSoilTextureTriangle = 5;
  kIndexTypeMaleFemale = 6;
  kIndexTypeHarvestItemTypes = 7;
  kIndexTypeWeatherMatrix = 8;
  kIndexTypeMUSICoefficients = 9;

  kNotSplit = 0;
  kFirstPart = 1;
  kSecondPart = 2;
  kThirdPart = 3;

type

unitsAndBoundsInfoType = record
  deriveMethod: smallint; {ignored for the normal units and bounds}
  unitSet: smallint;
  unitModel: smallint;
  unitDefaultMetric: smallint;
  unitDefaultEnglish: smallint;
  boundSoftLower: single;
  boundSoftUpper: single;
  boundHardLower: single;
  boundHardUpper: single;
  boundSource: smallint;
  end;
unitsAndBoundsInfoArrayType = array[0..kMaxNumDerivations] of unitsAndBoundsInfoType;

{ the version of GsAspect used here is simpler than that in the main project. be careful
  not to confuse the two. }
GsAspect = class(TObject)
  public
  fieldID: string[80];
  fieldNumber: longint;
  aspectName: string[80];
  fieldType: smallint;
  indexType: smallint;
  readOnly: boolean;
  aspectType: smallint;
  transferType: smallint;
  accessName: string[80];
  hasHelp: boolean;
  unitsAndBounds: unitsAndBoundsInfoArrayType;
  numUnitsAndBounds: smallint;
  function objectType: smallint;
  end;

type
  TTab2AspForm = class(TForm)
    OpenDialog: TOpenDialog;
    messageLabel: TLabel;
    generateReport: TButton;
    generateCode: TButton;
    generateGroupAspectsReport: TButton;
    quit: TButton;
    generateComponentsList: TButton;
    generateAspectsHelpFile: TButton;
    mergeTwoAspectHintFiles: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure generateCodeClick(Sender: TObject);
    procedure generateReportClick(Sender: TObject);
    procedure generateGroupAspectsReportClick(Sender: TObject);
    procedure quitClick(Sender: TObject);
    procedure generateComponentsListClick(Sender: TObject);
    procedure generateAspectsHelpFileClick(Sender: TObject);
    procedure mergeTwoAspectHintFilesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    outputFile: TextFile;
    procedure say(what: string);
    procedure readOneAspectFromFile(aspect: GsAspect; var inputFile: textFile; aFieldNumber: longint);
    function readAllAspectsFromTabFile: boolean;
    function readAllAspectsFromPascalFile(inputFileName: string): boolean;
    function readAllAspectsFromGroupsFile: boolean;
    procedure writeInterfaceStartToFile(var outputFile: TextFile; unitName: string);
    procedure writeConstantsToFile(var outputFile: TextFile);
    procedure writeTransferFieldFunctionsToFile(var outputFile: TextFile);
    procedure writeFunctionHeaderForObjectType(objectType: smallint; var outputFile: TextFile; isFirst: boolean);
    procedure writeTransferFieldLineForAspect(aspect: GsAspect; var outputFile: TextFile);
    procedure writeCreateAspectsFunctionToFile(var outputFile: TextFile; objectType, part: smallint);
    procedure writeConstantsFile;
    procedure writeWeatherAspectsFile;
    procedure writePlantAspectsFile;
    procedure writeDrawingPlantAspectsFile;
    procedure writeSoilPatchAspectsFiles;
 end;

var
  Tab2AspForm: TTab2AspForm;

implementation

{$R *.DFM}

var
  aspectsFromTabFile: TList;
  aspectsFromPascalFile: TList;
  aspectsInTabAndPascal: TList;
  aspectsFromGroupsFile: TList;

{ ------------------------------------------------------------------------------------------------- form functions }
procedure TTab2AspForm.FormCreate(Sender: TObject);
  begin
  aspectsFromTabFile := TList.create;
  aspectsFromPascalFile := TList.create;
  aspectsInTabAndPascal := TList.create;
  aspectsFromGroupsFile := TList.create;
  end;

procedure TTab2AspForm.FormDestroy(Sender: TObject);
  var i: longint;
  begin
  with aspectsFromTabFile do if count > 0 then for i := 0 to count - 1 do GsAspect(items[i]).free;
  aspectsFromTabFile.free;
  with aspectsFromPascalFile do if count > 0 then for i := 0 to count - 1 do GsAspect(items[i]).free;
  aspectsFromPascalFile.free;
  with aspectsInTabAndPascal do if count > 0 then for i := 0 to count - 1 do GsAspect(items[i]).free;
  aspectsInTabAndPascal.free;
  with aspectsFromGroupsFile do if count > 0 then for i := 0 to count - 1 do GsAspect(items[i]).free;
  aspectsFromGroupsFile.free;
  end;

procedure TTab2AspForm.FormActivate(Sender: TObject);
  begin
  say('Click on a button to start.');
  end;

procedure TTab2AspForm.quitClick(Sender: TObject);
  begin
  self.close;
  end;

procedure TTab2AspForm.say(what: string);
  begin
  messageLabel.caption := what;
  messageLabel.invalidate;
  application.processMessages;
  end;

function TTab2AspForm.readAllAspectsFromTabFile: boolean;
  { returns false if user canceled }
  var
    inputFileName, ignore: string;
    inputFile: TextFile;
    aspect: GsAspect;
    count: longint;
  begin
  result := false;
  say('Starting...');
  inputFileName := extractFilePath(application.exeName) + '\aspects.tab';
  if not fileExists(inputFileName) then
    begin
    with openDialog do
      begin
      fileName := 'aspects.tab';
      title := 'Choose an aspects tab file to read the aspects from';
      filter := 'Tab files (*.tab)|*.tab';
      options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
      end;
    if not openDialog.execute then exit;
    inputFileName := openDialog.fileName;
    end;
  try
    say('Loading aspects from tab file...');
    assignFile(inputFile, inputFileName);
    reset(inputFile);
    aspectsFromTabFile.clear;
    { read in }
    readln(inputFile, ignore); { skip over labels line }
    count := 5000;
    while not eof(inputFile) do
      begin
      aspect := GsAspect.create;
      if aspect <> nil then self.readOneAspectFromFile(aspect, inputFile, count);
      aspectsFromTabFile.add(aspect);
      inc(count);
      end;
  finally
    closeFile(inputFile);
    result := true;
  end;
  end;

procedure TTab2AspForm.generateCodeClick(Sender: TObject);
  begin
  if not self.readAllAspectsFromTabFile then exit;
  say('Writing constants and transfer fields...');
  self.writeConstantsFile;
  say('Writing weather aspects...');
  self.writeWeatherAspectsFile;
  say('Writing plant aspects...');
  self.writePlantAspectsFile;
  say('Writing drawing plant aspects...');
  self.writeDrawingPlantAspectsFile;
  say('Writing soil aspects...');
  self.writeSoilPatchAspectsFiles;
  say('Finished. Close window to quit.');
  end;

{ -------------------------------------------------------------------------------------- text formatting functions }
function found(substring, fullString: string): boolean;
  begin
  result := pos(substring, fullString) > 0;
  end;

procedure fieldTypeAndIndexTypeFromName(varTypeName: string; var fieldType: smallint; var indexType: smallint);
  begin
  { default type is single, not an array }
  indexType := kIndexTypeNone;
  fieldType := kFieldFloat;
  if pos('smallint', varTypeName) > 0 then
    begin
    if pos(kEnumCode, varTypeName) > 0 then
      fieldType :=  kFieldEnumeratedList
    else
      fieldType := kFieldInt;
    end
  else if varTypeName = 'boolean' then
    fieldType := kFieldBoolean
  else if varTypeName = 'TColorRef' then
    fieldType := kFieldColor
  else if varTypeName = 'KfObject3D' then
    fieldType := kFieldThreeDObject
  else if varTypeName = 'SCurveStructure' then
    indexType := kIndexTypeSCurve
  else if varTypeName = 'arraySoilLayers' then
    indexType := kIndexTypeLayer
  else if varTypeName = 'arrayFour' then
    indexType := kIndexTypeMUSICoefficients
  else if varTypeName = 'arrayMonths' then
    indexType := kIndexTypeMonth
  else if varTypeName = 'windDirectionsArray' then
    indexType := kIndexTypeMonth
  else if varTypeName = 'arrayThree' then
    indexType := kIndexTypeWeatherMatrix;
  end;

procedure writeCopyrightComment(var outputFile: textFile; unitName: string);
  begin
  writeln(outputFile, '{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved');
  writeln(outputFile, 'http://www.gardenwithinsight.com. See the license file for details on redistribution.');
  writeln(outputFile, '=====================================================================================');
  writeln(outputFile, 'DO NOT EDIT THIS FILE. It was generated by the tab2asp program from aspects.tab.');
  if unitName = 'uasp_dpl' then
    writeln(outputFile, unitName + ': Aspect creation for drawing plant.}')
  else if unitName = 'uasp_pla' then
    writeln(outputFile, unitName + ': Aspect creation for plant.}')
  else if unitName = 'uasp_s1' then
    writeln(outputFile, unitName + ': Aspect creation for soil patch (part 1).}')
  else if unitName = 'uasp_s2' then
    writeln(outputFile, unitName + ': Aspect creation for soil patch (part 2).}')
  else if unitName = 'uasp_s3' then
    writeln(outputFile, unitName + ': Aspect creation for soil patch (part 3).}')
  else if unitName = 'uasp_wea' then
    writeln(outputFile, unitName + ': Aspect creation for weather.}')
  else if unitName = 'umconsts' then
    writeln(outputFile, unitName + ': Aspect creation constants and transferField functions.}');
  end;

function fieldTypeName(fieldType: integer): string;
  begin
  case fieldType of
    kFieldUndefined: result := 'Undefined';
    kFieldFloat: result := 'Single';
    kFieldInt: result := 'Integer';
    kFieldUnsignedLong: result := 'Longint';
    kFieldUnsignedChar: result := 'Char';
    kFieldString: result := 'String';
    kFieldFileString: result := 'File String';
    kFieldColor: result := 'Color';
    kFieldBoolean: result := 'Boolean';
    kFieldIcon: result := 'Icon';
    kFieldThreeDObject: result := '3D object';
    kFieldEnumeratedList: result := 'List of choices';
    kFieldHarvestItemTemplate: result := 'Harvest item template';
    else result := 'not in list';
    end;
  end;

function indexTypeName(indexType: integer): string;
  begin
  case indexType of
    kIndexTypeUndefined: result := 'Undefined';
    kIndexTypeNone: result := 'None';
    kIndexTypeLayer: result := 'Layer';
    kIndexTypeMonth: result := 'Month';
    kIndexTypeSCurve: result := 'S curve';
    kIndexTypeSoilTextureTriangle: result := 'Soil texture triangle';
    kIndexTypeMaleFemale: result := 'Male/female';
    kIndexTypeHarvestItemTypes: result := 'Harvest item types';
    else result := 'not in list';
    end;
  end;

function boolToStr(value: boolean): string;
  begin
  if value then
    result := 'true'
  else
    result := 'false';
  end;

function createName(objectType: smallint): string;
  begin
  case objectType of
  	kObjectTypeGarden: result := 'Garden_createAspects';
		kObjectTypeWeather: result := 'Weather_createAspects';
		kObjectTypeSoil: result := 'SoilPatch_createAspects';
		kObjectTypePlant: result :='Plant_createAspects';
    kObjectTypeBag: result := 'Bag_createAspects';
    kObjectTypeDrawingPlant: result := 'DrawingPlant_createAspects';
  else
  	result := '<ERROR>';
  end;
  end;

const
  kAspectsInFirstPart = 150;
  kAspectsInSecondPart = 150;

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

function digitValueString(value: single): string;
  begin
  result := removeTrailingZeros(shortenLongNonZeroDigitsAfterDecimal(floatToStrF(value, ffFixed, 7, 8)));
  end;

function GsAspect.objectType: smallint;
  begin
  if pos('kSoilPatch', fieldID) <> 0 then
    result := kObjectTypeSoil
  else if pos('kPlantZZDraw', fieldID) <> 0 then
    result := kObjectTypeDrawingPlant
  else if pos('kPlant', fieldID) <> 0 then
    result := kObjectTypePlant
  else if pos('kWeather', fieldID) <> 0 then
    result := kObjectTypeWeather
  else if pos('kGarden', fieldID) <> 0 then
    result := kObjectTypeGarden
  else if pos('kBag', fieldID) <> 0 then
    result := kObjectTypeBag
  else
    result := 0;
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

function readUntilTab(var theFile: textFile): string;
  var aChar: char;
  begin
  result := '';
  while not eof(theFile) do
    begin
    read(theFile, aChar);
    if (aChar <> chr(9)) and (aChar <> chr(10)) and (aChar <> chr(13)) then
      begin
      { ignore quotes put in by Excel - does not allow double quotes in our text }
      if (aChar <> '"') then
        result := result + aChar;
      end
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
  end;

procedure readBooleanToTab(var fileRef: TextFile; var aBoolean: boolean; aspectName: string);
  var booleanString: string;
  begin
  booleanString := readUntilTab(fileRef);
  if (booleanString = 'TRUE') then
    aBoolean := true
  else if (booleanString = 'FALSE') then
    aBoolean := false
  else
    raise Exception.create('Improper file format for boolean in ' + aspectName + '; got: ' + booleanString);
  end;

function trimQuotes(theString: string): string;
  var
    i: integer;
  begin
  result := theString;
  i := 1;
  while theString[i] = '"' do inc(i);
  result := copy(result, i, length(result) - i + 1);
  while result[length(result)] = '"' do dec(result[0]);
  end;

function fixApostrophes(theString: string): string;
  var
    i: integer;
  begin
  result := theString;
  i := 1;
  while i <= length(result) do
    begin
    if result[i] = '''' then
      begin
      result := copy(result, 1, i-1) + '''' + copy(result, i, length(result) - i + 1);
      inc(i);
      end;
    inc(i);
    end;
  end;

const
  kMinSingle = -3.4e38;
  kMaxSingle = 3.4e38;
  kMinSmallint = -32768;
  kMaxSmallint = 32767;

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
      smallint(number) := kMinSmallint;
    end
  else if (boundString = 'MAX') or (boundString = 'FLT_MAX')  then
    begin
    if fieldType = kFieldFloat then
      single(number) := kMaxSingle
    else
      smallint(number) := kMaxSmallint;
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

{ copied these from uunits.pas - must keep them up to date }
const
  kNonChangingUnitMetersPMeter = 21;
  kConcentration = 31;
  kMass = 6;

function unitSetMatchesConvertType(unitSet, convertType: smallint): boolean;
  begin
  case convertType of
    kDeriveTypeDepth:
      result := (unitSet = kNonChangingUnitMetersPMeter);
    kDeriveTypeConcentration:
      result := (unitSet = kConcentration);
    kDeriveTypeArea:
      result := (unitSet = kMass);
    else
      result := false;
    end;
  end;

function partBeforeUnderscore(aString: string): string;
  var
    underscorePos: integer;
  begin
  underscorePos := pos('_', aString);
  if underscorePos <= 0 then
    result := aString
  else
   begin
   { if has two underscores, is a wind direction, so give whole string back }
   if pos('_', copy(aString, underscorePos + 1, length(aString))) > 0 then
     result := aString
   else
     result := copy(aString, 1, underscorePos - 1);
   end;
  end;

function objectTypeName(objectType: smallint): string;
	begin
  case objectType of
		kObjectUndefined: result := 'Undefined';
  	kObjectTypeGarden: result := 'Garden';
		kObjectTypeWeather: result := 'Weather';
		kObjectTypeSoil: result := 'Soil Patch';
		kObjectTypePlant: result :='Plant';
		kObjectTypeBlob: result := 'Blob';
		kObjectTypeFruit: result := 'Fruit';
		kObjectTypePesticide: result := 'Pesticide';
    kObjectTypeBag: result := 'Bag';
    { this line added here cfk }
    kObjectTypeDrawingPlant: result := 'Drawing plant';
  else
  	result := '<ERROR>';
  end;
  end;

{ ---------------------------------------------------------------------------------------------------------- reading }
procedure TTab2AspForm.readOneAspectFromFile(aspect: GsAspect; var inputFile: textFile; aFieldNumber: longint);
  var
    ignore: string;
    tempString: string;
    derivedIndex: smallint;
  begin
  with aspect do
    begin
    fieldNumber := aFieldNumber;
    { FieldID Name fieldType IndexType unitSet unitModel unitMetric unitEnglish
      SLower SUpper HLower HUpper Source Type ReadOnly HasHelp Access Transfer }
    fieldID := readUntilTab(inputFile);
    aspectName := readUntilTab(inputFile);
    aspectName := trimQuotes(aspectName);
    aspectName := fixApostrophes(aspectName);
    read(inputFile, fieldType); ignore := readUntilTab(inputFile);
    read(inputFile, indexType); ignore := readUntilTab(inputFile);
    read(inputFile, unitsAndBounds[0].unitSet); ignore := readUntilTab(inputFile);
    read(inputFile, unitsAndBounds[0].unitModel); ignore := readUntilTab(inputFile);
    read(inputFile, unitsAndBounds[0].unitDefaultMetric); ignore := readUntilTab(inputFile);
    read(inputFile, unitsAndBounds[0].unitDefaultEnglish); ignore := readUntilTab(inputFile);
    tempString := readUntilTab(inputFile); boundForString(tempString, kFieldFloat, unitsAndBounds[0].boundSoftLower);
    tempString := readUntilTab(inputFile); boundForString(tempString, kFieldFloat, unitsAndBounds[0].boundSoftUpper);
    tempString := readUntilTab(inputFile); boundForString(tempString, kFieldFloat, unitsAndBounds[0].boundHardLower);
    tempString := readUntilTab(inputFile); boundForString(tempString, kFieldFloat, unitsAndBounds[0].boundHardUpper);
    read(inputFile, unitsAndBounds[0].boundSource); ignore := readUntilTab(inputFile);
    read(inputFile, aspectType); ignore := readUntilTab(inputFile);
    readBooleanToTab(inputFile, readOnly, aspectName);
    readBooleanToTab(inputFile, hasHelp, aspectName);
    accessName := readUntilTab(inputFile);
    read(inputFile, transferType); ignore := readUntilTab(inputFile);
    { D1 type D1 set D1 model D1 metric D1 English D1 BSL D1 BSU D1 BHL D1 BHU D1 source }
    derivedIndex := 1;
    while derivedIndex <= kMaxNumDerivations do
      begin
      read(inputFile, unitsAndBounds[derivedIndex].deriveMethod);
      if unitsAndBounds[derivedIndex].deriveMethod = kNotDerived then
        begin
        numUnitsAndBounds := derivedIndex;
        derivedIndex := kMaxNumDerivations + 1;
        readln(inputFile);
        end
      else with aspect.unitsAndBounds[derivedIndex] do
        begin
        ignore := readUntilTab(inputFile);
        read(inputFile, unitSet); ignore := readUntilTab(inputFile);
        read(inputFile, unitModel); ignore := readUntilTab(inputFile);
        read(inputFile, unitDefaultMetric); ignore := readUntilTab(inputFile);
        read(inputFile, unitDefaultEnglish); ignore := readUntilTab(inputFile);
        read(inputFile, boundSoftLower); ignore := readUntilTab(inputFile);
        read(inputFile, boundSoftUpper); ignore := readUntilTab(inputFile);
        read(inputFile, boundHardLower); ignore := readUntilTab(inputFile);
        read(inputFile, boundHardUpper); ignore := readUntilTab(inputFile);
        read(inputFile, boundSource); ignore := readUntilTab(inputFile);
        aspect.numUnitsAndBounds := derivedIndex;
        inc(derivedIndex);
        end;
      end;
    end;
  end;

{ ---------------------------------------------------------------------------------------------------------- writing }
procedure TTab2AspForm.writeConstantsFile;
  var
    fileName: string;
  begin
  fileName := 'umconsts.pas';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
  try
    assignFile(outputFile, fileName);
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'umconsts');
    self.writeConstantsToFile(outputFile);
    writeln(outputFile, 'procedure Bag_directTransferField(bagProxy: GsStreamableObject; var v;');
    writeln(outputFile, '  d, fieldID, ft, index, dM: smallint; updateList: TListCollection);');
    writeln(outputFile, 'procedure Plant_directTransferField(plantProxy: GsStreamableObject; var v;');
    writeln(outputFile, '  d, fieldID, ft, index, dM: smallint; updateList: TListCollection);');
    writeln(outputFile, 'procedure DrawingPlant_directTransferField(drawingPlantProxy: GsStreamableObject; var v;');
    writeln(outputFile, '  d, fieldID, ft, index, dM: smallint; updateList: TListCollection);');
    writeln(outputFile, 'procedure SoilPatch_directTransferField(soilpatchProxy: GsStreamableObject; var v;');
    writeln(outputFile, '  d, fieldID, ft, index, dM: smallint; updateList: TListCollection);');
    writeln(outputFile, 'procedure Weather_directTransferField(weatherProxy: GsStreamableObject; var v;');
    writeln(outputFile, '  d, fieldID, ft, index, dM: smallint; updateList: TListCollection);');
    writeln(outputFile, 'procedure CreateAllAspects(aspectManager: GsAspectManager);');
    writeln(outputFile, 'procedure Bag_createAspects(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    writeln(outputFile, 'uses SysUtils, uegarden, ueweath, uesoil, uestruct, '
        + 'ueplant, udplant, uebag, ueutils, uharvprt, uturt3d,');
    writeln(outputFile, '  uasp_wea, uasp_pla, uasp_dpl, uasp_s1, uasp_s2, uasp_s3;');
    writeln(outputFile);
    self.writeTransferFieldFunctionsToFile(outputFile);
    writeln(outputFile, 'procedure CreateAllAspects(aspectManager: GsAspectManager);');
    writeln(outputFile, '  begin');
    writeln(outputFile, '  if aspectManager = nil then');
    writeln(outputFile, '    raise Exception.create(''CreateAllAspects: nil aspect manager'');');
    writeln(outputFile, '  Bag_createAspects(aspectManager);');
    writeln(outputFile, '  Weather_createAspects(aspectManager);');
    writeln(outputFile, '  Plant_createAspects1(aspectManager);');
    writeln(outputFile, '  Plant_createAspects2(aspectManager);');
    writeln(outputFile, '  DrawingPlant_createAspects(aspectManager);');
    writeln(outputFile, '  SoilPatch_createAspects1(aspectManager);');
    writeln(outputFile, '  SoilPatch_createAspects2(aspectManager);');
    writeln(outputFile, '  SoilPatch_createAspects3(aspectManager);');
    writeln(outputFile, '  end;');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypeBag, kNotSplit);
    writeln(outputFile, 'end.');
    writeln(outputFile);
  finally
    closeFile(outputFile);
  end;
  end;

procedure TTab2AspForm.writeWeatherAspectsFile;
   var
    fileName: string;
  begin
  fileName := 'uasp_wea.pas';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
 try
    assignFile(outputFile, fileName);
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'uasp_wea');
    writeln(outputFile, 'procedure Weather_createAspects(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypeWeather, kNotSplit);
    writeln(outputFile, 'end.');
    writeln(outputFile);
  finally
    closeFile(outputFile);
  end;
 end;

procedure TTab2AspForm.writePlantAspectsFile;
   var
    fileName: string;
  begin
  fileName := 'uasp_pla.pas';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
  try
    assignFile(outputFile, fileName);
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'uasp_pla');
    writeln(outputFile, 'procedure Plant_createAspects1(aspectManager: GsAspectManager);');
    writeln(outputFile, 'procedure Plant_createAspects2(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypePlant, kFirstPart);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypePlant, kSecondPart);
    writeln(outputFile, 'end.');
  finally
    closeFile(outputFile);
  end;
 end;

procedure TTab2AspForm.writeDrawingPlantAspectsFile;
   var
    fileName: string;
  begin
  fileName := 'uasp_dpl.pas';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
  try
    assignFile(outputFile, fileName);
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'uasp_dpl');
    writeln(outputFile, 'procedure DrawingPlant_createAspects(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypeDrawingPlant, kNotSplit);
    writeln(outputFile, 'end.');
 finally
    closeFile(outputFile);
  end;
 end;

procedure TTab2AspForm.writeSoilPatchAspectsFiles;
   var
    fileName: string;
  begin
  fileName := 'uasp_s1.pas';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
  try
    { first file }
    assignFile(outputFile, fileName);
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'uasp_s1');
    writeln(outputFile, 'procedure SoilPatch_createAspects1(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypeSoil, kFirstPart);
    writeln(outputFile, 'end.');
    closeFile(outputFile);
    { second file - assume has same readonly status as first }
    assignFile(outputFile, 'uasp_s2.pas');
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'uasp_s2');
    writeln(outputFile, 'procedure SoilPatch_createAspects2(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypeSoil, kSecondPart);
    writeln(outputFile, 'end.');
    closeFile(outputFile);
    { third file - assume has same readonly status as first }
    assignFile(outputFile, 'uasp_s3.pas');
    rewrite(outputFile);
    self.writeInterfaceStartToFile(outputFile, 'uasp_s3');
    writeln(outputFile, 'procedure SoilPatch_createAspects3(aspectManager: GsAspectManager);');
    writeln(outputFile);
    writeln(outputFile, 'implementation');
    writeln(outputFile);
    self.writeCreateAspectsFunctionToFile(outputFile, kObjectTypeSoil, kThirdPart);
    writeln(outputFile, 'end.');
  finally
    closeFile(outputFile);
  end;
 end;

procedure TTab2AspForm.writeInterfaceStartToFile(var outputFile: TextFile; unitName: string);
  begin
  writeln(outputFile, 'unit ' + unitName + ';');
  writeCopyrightComment(outputFile, unitName);
  writeln(outputFile);
  writeln(outputFile, 'interface');
  writeln(outputFile);
  if pos('const', unitName) <> 0 then
    writeln(outputFile, 'uses ufiler, ucollect, uaspects;')
  else
    writeln(outputFile, 'uses uaspects;');
  writeln(outputFile);
  end;

procedure TTab2AspForm.writeConstantsToFile(var outputFile: TextFile);
  var
    i: integer;
    aspect: GsAspect;
    objectType: smallint;
  begin
  try
    writeln(outputFile, 'const');
    objectType := 0;
    if aspectsFromTabFile.count > 0 then
      for i := 0 to aspectsFromTabFile.count - 1 do
        begin
        aspect := GsAspect(aspectsFromTabFile.items[i]);
        if objectType <> aspect.objectType then
          begin
          objectType := aspect.objectType;
          writeln(outputFile);
          writeln(outputFile, '{ ' + objectTypeName(objectType) + ' }');
          if objectType = kObjectTypeDrawingPlant then
            writeln(outputFile, 'kPlantZZDrawFirstField = ' + intToStr(aspect.fieldNumber) + ';');
          end;
        writeln(outputFile, aspect.fieldID + ' = ' + intToStr(aspect.fieldNumber) + ';');
        end;
    writeln(outputFile);
  except
    on e: exception do raise Exception.create('Error in writeConstantsToFile: ' + e.message);
  end;
  end;

procedure TTab2AspForm.writeTransferFieldFunctionsToFile(var outputFile: TextFile);
  var
    i: integer;
    aspect: GsAspect;
    objectType: smallint;
  begin
  try
    objectType := 0;
    if aspectsFromTabFile.count > 0 then
      for i := 0 to aspectsFromTabFile.count - 1 do
        begin
        aspect := GsAspect(aspectsFromTabFile.items[i]);
        if objectType <> aspect.objectType then
          begin
          self.writeFunctionHeaderForObjectType(aspect.objectType, outputFile, (objectType = 0));
          objectType := aspect.objectType;
          end;
        self.writeTransferFieldLineForAspect(aspect, outputFile);
        end;
    writeln(outputFile, '    end;');
    writeln(outputFile, '  end;');
    writeln(outputFile);
  except
    on e: exception do raise Exception.create('Error in writeTransferFieldFunctionsToFile: ' + e.message);
  end;
  end;

procedure TTab2AspForm.writeFunctionHeaderForObjectType(objectType: smallint; var outputFile: TextFile; isFirst: boolean);
  var
    wordLower, wordUpper: string;
  begin
  if not isFirst then
    begin
    writeln(outputFile, '    end;');
    writeln(outputFile, '  end;');
    writeln(outputFile);
    end;
  case objectType of
  	kObjectTypeGarden: wordUpper := 'Garden';
		kObjectTypeWeather: wordUpper := 'Weather';
		kObjectTypeSoil: wordUpper := 'SoilPatch';
		kObjectTypePlant: wordUpper := 'Plant';
    kObjectTypeBag: wordUpper := 'Bag';
    kObjectTypeDrawingPlant: wordUpper := 'DrawingPlant';
  else
  	raise Exception.create('writeFunctionHeaderForObjectType: no object type');
  end;
  wordLower := lowerCase(wordUpper);
  writeln(outputFile, 'procedure ' + wordUpper + '_directTransferField(' + wordLower + 'Proxy: GsStreamableObject; var v;');
  writeln(outputFile, '  d, fieldID, ft, index, dM: smallint; updateList: TListCollection);');
  writeln(outputFile, '  var ' + wordLower + ': Gs' + wordUpper + ';');
  writeln(outputFile, '  begin');
  writeln(outputFile, '  if ' + wordLower + 'Proxy = nil then');
  writeln(outputFile, '    raise Exception.create(''' + wordUpper + '_directTransferField: nil ' + wordLower + ''');');
  writeln(outputFile, '  ' + wordLower + ' := ' + wordLower + 'Proxy as Gs' + wordUpper + ';');
  writeln(outputFile, '  with ' + wordLower + ' do case fieldID of');
  end;

procedure TTab2AspForm.writeTransferFieldLineForAspect(aspect: GsAspect; var outputFile: TextFile);
  begin
  with aspect do
  case transferType of
    kTransferTypeMFD:
      begin
      writeln(outputFile, '    ' + fieldID + ': ');
      writeln(outputFile, '      MFD(' + accessName + ', v, ft, d);');
      end;
    kTransferTypeBDConvert:
      begin
      writeln(outputFile, '    ' + fieldID + ': ');
      writeln(outputFile, '      BDConvert(' + accessName + ',');
      writeln(outputFile, '          v, ' + fieldID + ', dM, ft, d, index, updateList);');
      end;
   kTransferTypeGetSetSCurve:
      begin
      writeln(outputFile, '    ' + fieldID + ': ');
      writeln(outputFile, '      Utils_GetSetSCurveValue(d, ' + accessName + ', index, v);');
      end;
    kTransferTypeHarvestItemTemplate:
      begin
      writeln(outputFile, '    ' + fieldID + ': ');
      writeln(outputFile, '      transferHarvestItemTemplate(d, ' + accessName + ',');
      writeln(outputFile, '          GsHarvestItemTemplate(v));');
      end;
    kTransferTypeObject3D:
      begin
      writeln(outputFile, '    ' + fieldID + ': ');
      writeln(outputFile, '      transferObject3D(d, ' + accessName + ', KfObject3D(v));');
      end;
    kTransferTypeStorageOrganLumping:
      begin
      writeln(outputFile, '    ' + fieldID + ': transferStorageOrganLumping(d, smallint(v));');
      end;
    kTransferTypeHarvestBundling:
      begin
      writeln(outputFile, '    ' + fieldID + ': transferHarvestBundling(d, boolean(v), index);');
      end;
    kTransferTypeDerived: { do nothing };
    else
  	  raise Exception.create('writeTransferFieldLineForAspect: transfer type zero');
    end;
  end;

procedure TTab2AspForm.writeCreateAspectsFunctionToFile(var outputFile: TextFile; objectType, part: smallint);
  var
    i, j, objectStart: longint;
    aspect: GsAspect;
    functionName: string;
  begin
  try
    functionName := createName(objectType);
    if part <> kNotSplit then functionName := functionName + intToStr(part);
    writeln(outputFile, 'procedure ' + functionName + '(aspectManager: GsAspectManager);');
    writeln(outputFile, '  var aspect: GsAspect;');
    writeln(outputFile, '  begin');
    objectStart := 0;
    if aspectsFromTabFile.count > 0 then
      for i := 0 to aspectsFromTabFile.count - 1 do
        begin
        aspect := GsAspect(aspectsFromTabFile.items[i]);
        if aspect.objectType <> objectType then continue;
        { special code to split plant or soil into two functions }
        if objectStart = 0 then objectStart := i;
        case part of
          kNotSplit: ;
          kFirstPart: if i > objectStart + kAspectsInFirstPart then break;
          kSecondPart:
            begin
            if i <= objectStart + kAspectsInFirstPart then continue;
            if i > objectStart + kAspectsInFirstPart + kAspectsInSecondPart then break;
            end;
          kThirdPart: if i <= objectStart + kAspectsInFirstPart + kAspectsInSecondPart then continue;
          end;
        with aspect do
          begin
          writeln(outputFile, '  aspect := GsAspect.createWithInfo('''
              + fieldID + ''',');
          writeln(outputFile, '      '''
              + aspectName + ''',');
          writeln(outputFile, '      '
              + intToStr(fieldNumber) + ', '
              + intToStr(fieldType) + ', '
              + intToStr(indexType) + ', '
              + boolToStr(readOnly) + ', '
              + intToStr(aspectType) + ', ');
          writeln(outputFile, '      '
              + intToStr(transferType) + ', '''
              + accessName + ''', '
              + boolToStr(hasHelp) + ');');
          for j := 0 to numUnitsAndBounds - 1 do
            begin
            writeln(outputFile, '  aspect.addUnitsAndBounds('
              + intToStr(unitsAndBounds[j].deriveMethod) + ', '
              + intToStr(unitsAndBounds[j].unitSet) + ', '
              + intToStr(unitsAndBounds[j].unitModel) + ', '
              + intToStr(unitsAndBounds[j].unitDefaultMetric) + ', '
              + intToStr(unitsAndBounds[j].unitDefaultEnglish) + ', '
              + digitValueString(unitsAndBounds[j].boundSoftLower) + ', '
              + digitValueString(unitsAndBounds[j].boundSoftUpper) + ', '
              + digitValueString(unitsAndBounds[j].boundHardLower) + ', '
              + digitValueString(unitsAndBounds[j].boundHardUpper) + ', '
              + intToStr(unitsAndBounds[j].boundSource) + ');');
            end;
          writeln(outputFile, '  aspectManager.addAspect(aspect);');
          end;
        end;
    writeln(outputFile, '    end;');
    writeln(outputFile);
  except
    on e: exception do raise Exception.create('Error in writeCreateAspectsFunctionToFile: ' + e.message);
  end;
  end;

{ ---------------------------------------------------------------------------------------------------------- reports }
procedure TTab2AspForm.generateReportClick(Sender: TObject);
  var
    outputFile: TextFile;
    i, j, matchingIndex, totalNum: longint;
    aspectFromUestruct, aspectFromTab, aspect: GsAspect;
    fileName: string;
  begin
  fileName := 'asp_rep.txt';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
  { read all aspects into tab list }
  if not self.readAllAspectsFromTabFile then exit;
  { read all info in uestruct into uestruct list }
  aspectsFromPascalFile.clear;
  if not self.readAllAspectsFromPascalFile('uestruct.pas') then exit;
  if not self.readAllAspectsFromPascalFile('udplant.pas') then exit;
  if not self.readAllAspectsFromPascalFile('uebag.pas') then exit;
  try
  assignFile(outputFile, fileName);
  rewrite(outputFile);
  say('Comparing...');
  aspectsInTabAndPascal.clear;
  { go through uestruct list looking for aspects in tab list - if found, remove from both lists and add to "both" list }
  totalNum := aspectsFromPascalFile.count;
  if aspectsFromPascalFile.count > 0 then
     for i := 0 to aspectsFromPascalFile.count - 1 do
        begin
       say('Comparing ' + intToStr(i) + '/' + intToStr(totalNum) + '...');
       aspectFromUestruct := GsAspect(aspectsFromPascalFile.items[i]);
        if aspectsFromTabFile.count > 0 then
          for j := 0 to aspectsFromTabFile.count - 1 do
            begin
            aspectFromTab := GsAspect(aspectsFromTabFile.items[j]);
            if lowerCase(trimLeftAndRight(aspectFromTab.accessName)) =
                lowerCase(trimLeftAndRight(aspectFromUestruct.accessName)) then
              begin
              aspectsInTabAndPascal.add(aspectFromTab);
              aspectsInTabAndPascal.add(aspectFromUestruct);
              if aspectFromTab.fieldType <> aspectFromUestruct.fieldType then
                writeln(outputFile, 'Aspect ' + aspectFromTab.fieldID + ' has field type '
                  + intToStr(aspectFromTab.fieldType) + ' (' + fieldTypeName(aspectFromTab.fieldType)
                  + ') in aspects.tab and field type '
                  + intToStr(aspectFromUestruct.fieldType) + ' (' + fieldTypeName(aspectFromUestruct.fieldType)
                  + ') in the Pascal file.');
              if aspectFromTab.indexType <> aspectFromUestruct.indexType then
                writeln(outputFile, 'Aspect ' + aspectFromTab.fieldID + ' has index type '
                  + intToStr(aspectFromTab.indexType) + ' (' + indexTypeName(aspectFromTab.indexType)
                  + ') in aspects.tab and index type '
                  + intToStr(aspectFromUestruct.indexType) + ' (' + indexTypeName(aspectFromUestruct.indexType)
                  + ') in the Pascal file.');
              end;
            end;
        end;
  { now that we have a list in both, remove all those from both other lists }
  if aspectsInTabAndPascal.count > 0 then
    for i := 0 to aspectsInTabAndPascal.count - 1 do
      begin
      matchingIndex := aspectsFromTabFile.indexOf(aspectsInTabAndPascal.items[i]);
      if matchingIndex >= 0 then aspectsFromTabFile.delete(matchingIndex);
      matchingIndex := aspectsFromPascalFile.indexOf(aspectsInTabAndPascal.items[i]);
      if matchingIndex >= 0 then aspectsFromPascalFile.delete(matchingIndex);
     end;
   say('Writing...');
   { now write out each list }
    writeln(outputFile);
    writeln(outputFile, 'in uestruct.pas or udplant.pas or uebag.pas but not in aspects.tab');
    if aspectsFromPascalFile.count > 0 then
      for i := 0 to aspectsFromPascalFile.count - 1 do
        begin
        aspect := GsAspect(aspectsFromPascalFile.items[i]);
        write(outputFile, aspect.fieldID); {object}
        write(outputFile, chr(9));
        write(outputFile, intToStr(aspect.fieldType) + ' ' + fieldTypeName(aspect.fieldType));
        write(outputFile, chr(9));
        write(outputFile, intToStr(aspect.indexType) + ' ' + indexTypeName(aspect.indexType));
        write(outputFile, chr(9));
        write(outputFile, aspect.accessName);
        write(outputFile, chr(9));
        writeln(outputFile);
        end;
    writeln(outputFile);
    writeln(outputFile,  'in aspects.tab but not in uestruct.pas or udplant.pas or uebag.pas');
    if aspectsFromTabFile.count > 0 then
      for i := 0 to aspectsFromTabFile.count - 1 do
        begin
        aspect := GsAspect(aspectsFromTabFile.items[i]);
        write(outputFile, aspect.fieldID); 
        write(outputFile, chr(9));
        write(outputFile, aspect.accessName);
        write(outputFile, chr(9));
        writeln(outputFile);
        end;
      say('File ' + fileName + ' written.');
    finally
      closeFile(outputFile);
    end;
  end;

function TTab2AspForm.readAllAspectsFromPascalFile(inputFileName: string): boolean;
  var
    inputFile: TextFile;
    aspect, otherAspect: GsAspect;
    oneLine, objectName, accessName, varName, varTypeName: string;
    started: boolean;
    fieldType, indexType: smallint;
  begin
  result := false;
  if not fileExists(inputFileName) then
    begin
    with openDialog do
      begin
      fileName := inputFileName;
      title := 'Choose Pascal file to read variables (uestruct, udplant, uebag)';
      filter := 'Pascal files (*.pas)|*.pas';
      options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
      end;
    if not openDialog.execute then exit;
    inputFileName := openDialog.fileName;
    end;
  try
    say('Loading ' + lowerCase(extractFileName(inputFileName)) + '...');
    assignFile(inputFile, inputFileName);
    reset(inputFile);
     started := false;
    objectName := '';
    accessName := '';
    varName := '';
    varTypeName := '';
    while not eof(inputFile) do
      begin
      { read line }
      readln(inputFile, oneLine);
      { if not up to aspects.start line, continue }
      if not started then
        begin
        if found('aspects' + kHeaderDelimiter + 'start', oneLine) then started := true;
        continue;
        end;
      { if line has "aspects.stop" then quit reading }
      if found('aspects' + kHeaderDelimiter + 'stop', oneLine) then
        break;
      { if code that means don't make an aspect for this var found, go to next line }
      if found(kNoAspectCode, oneLine) then
        continue;
      { if line has kObjectHeader, start new object }
      if found(kObjectHeader + kHeaderDelimiter, oneLine) then
        begin
        objectName := copy(oneLine, pos(kHeaderDelimiter, oneLine) + 2, length(oneLine));
        objectName := copy(objectName, 1, pos(' }', objectName) - 1);
        continue;
        end;
      { if line has kStructureHeader, start new structure for accessName }
      if found(kStructureHeader + kHeaderDelimiter, oneLine) then
        begin
        accessName := copy(oneLine, pos(kHeaderDelimiter, oneLine) + 2, length(oneLine));
        accessName := copy(accessName, 1, pos(' }', accessName) - 1);
        { don't continue, because it could be on same line as var }
        end;
      { if line is reserved array, skip }
      if found('reservedArray', oneLine) then
        continue;
      { if line has ":" and ";", parse var name and type, then complete accessName and make aspect for it }
      if found(':', oneLine) and found(';', oneLine) then
        begin
        varName := copy(oneLine, 1, pos(':', oneLine) - 1);
        varName := trimLeftAndRight(varName);
        varTypeName := copy(oneLine, pos(':', oneLine) + 2, length(oneLine));
        varTypeName := copy(varTypeName, 1, pos(';', varTypeName) - 1);
        varTypeName := trimLeftAndRight(varTypeName);
        fieldTypeAndIndexTypeFromName(varTypeName, fieldType, indexType);
        { if layer, override index type}
        if found('[index]', accessName) then
          indexType := kIndexTypeLayer;
        { in new aspect, fill accessName and fieldType, and in fieldID put objectName }
        aspect := GsAspect.create;
        aspectsFromPascalFile.add(aspect);
        aspect.fieldId := objectName;
        aspect.fieldType := fieldType;
        aspect.indexType := indexType;
        if found('array', varTypeName) then
          aspect.accessName := accessName + '.' + varName + '[index]'
        else if found('pFlower', accessName) or found('pInflor', accessName) then
          begin
          aspect.accessName := accessName + '[kGenderFemale].' + varName;
          otherAspect := GsAspect.create;
          aspectsFromPascalFile.add(otherAspect);
          otherAspect.fieldId := objectName;
          otherAspect.fieldType := fieldType;
          otherAspect.indexType := indexType;
          otherAspect.accessName := accessName + '[kGenderMale].' + varName;
          end
        else
          aspect.accessName := accessName + '.' + varName;
        end;
     end;
  finally
    closeFile(inputFile);
    result := true;
  end;
  end;

procedure TTab2AspForm.generateGroupAspectsReportClick(Sender: TObject);
  var
    outputFile: TextFile;
    i, j, totalNum: longint;
    aspectFromTab, aspectFromGroups, aspect: GsAspect;
    fileName, groupFieldID, tabFieldID: string;
    found: boolean;
  begin
  fileName := 'asp_grp.txt';
  if fileExists(fileName) and Boolean(FileGetAttr(fileName) and faReadOnly) then
    begin
    showMessage('File ' + fileName + ' is read-only.');
    exit;
    end;
  { read all aspects into tab list }
  if not self.readAllAspectsFromTabFile then exit;
  if not self.readAllAspectsFromGroupsFile then exit;
  try
  assignFile(outputFile, fileName);
  rewrite(outputFile);
  say('Checking...');
  totalNum := aspectsFromTabFile.count;
  writeln(outputFile, 'Aspects in aspects.tab but not in groups.tab:');
  writeln(outputFile);
  if aspectsFromTabFile.count > 0 then for i := 0 to aspectsFromTabFile.count - 1 do
    begin
    say('Checking ' + intToStr(i) + '/' + intToStr(totalNum) + '...');
    aspectFromTab := GsAspect(aspectsFromTabFile.items[i]);
    found := false;
    if aspectsFromGroupsFile.count > 0 then for j := 0 to aspectsFromGroupsFile.count - 1 do
      begin
      aspectFromGroups := GsAspect(aspectsFromGroupsFile.items[j]);
      { check only part before unit because that will only report if both members of a basic-derived
        pair are missing }
      groupFieldID := partBeforeUnderscore(aspectFromGroups.fieldID);
      tabFieldID := partBeforeUnderscore(aspectFromTab.fieldID);
      if groupFieldID = tabFieldID then
        begin
        found := true;
        { use readOnly field of groups aspects to save whether aspect was found - use later to make report }
        { use hasHelp field to store whether aspect is in more than one group }
        if aspectFromGroups.readOnly then
          aspectFromGroups.hasHelp := true;
        aspectFromGroups.readOnly := true;
        end;
      end;
      if not found then
        writeln(outputFile, aspectFromTab.fieldID);
     end;
    { now go through groups aspects saying which were not found in the aspects file }
    writeln(outputFile);
    writeln(outputFile, 'Aspects in groups.tab but not in aspects.tab:');
    writeln(outputFile);
    if aspectsFromGroupsFile.count > 0 then for j := 0 to aspectsFromGroupsFile.count - 1 do
      begin
      aspect := GsAspect(aspectsFromGroupsFile.items[j]);
      if not aspect.readOnly then
        writeln(outputFile, aspect.fieldID);
      end;
    { now go through groups aspects saying which are in more than one group }
    writeln(outputFile);
    writeln(outputFile, 'Aspects in groups.tab twice:');
    writeln(outputFile);
    if aspectsFromGroupsFile.count > 0 then for j := 0 to aspectsFromGroupsFile.count - 1 do
      begin
      aspect := GsAspect(aspectsFromGroupsFile.items[j]);
      if aspect.hasHelp then
        writeln(outputFile, aspect.fieldID);
      end;
    say('File ' + fileName + ' written.');
  finally
    closeFile(outputFile);
  end;
  end;

function TTab2AspForm.readAllAspectsFromGroupsFile: boolean;
  var
    inputFileName, ignore: string;
    inputFile: TextFile;
    aspect: GsAspect;
  begin
  result := false;
  say('Loading aspects from groups file...');
  inputFileName := extractFilePath(application.exeName) + '\output\groups.tab';
  if not fileExists(inputFileName) then
    begin
    with openDialog do
      begin
      fileName := 'groups.tab';
      title := 'Choose a groups tab file to read groups from';
      filter := 'Tab files (*.tab)|*.tab';
      options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly, ofNoChangeDir];
      end;
    if not openDialog.execute then exit;
    inputFileName := openDialog.fileName;
    end;
  try
    say('Loading aspects from groups file...');
    assignFile(inputFile, inputFileName);
    reset(inputFile);
    aspectsFromGroupsFile.clear;
    { read in }
    readln(inputFile, ignore); { skip over labels line }
    while not eof(inputFile) do
      begin
      aspect := GsAspect.create;
      if aspect <> nil then
        begin
        ignore := readUntilTab(inputFile);
        aspect.fieldID := readUntilTab(inputFile);
        if not eof(inputFile) then readln(inputFile);
        end;
      aspectsFromGroupsFile.add(aspect);
      end;
  finally
    closeFile(inputFile);
    result := true;
  end;
  end;

type
componentConstants = (TButton, TBitBtn, TSpeedButton,
  TCheckBox, TRadioButton, TRadioGroup,
  TListBox, TComboBox,
  TEdit, TMemo,
  TPanel, THeader, TGroupBox, TScrollBox,
  TImage, TShape, TPaintBox,
  TSpinEdit);

function stringForComponentType(aType: smallint): string;
  begin
  result := '';
  case aType of
    ord(TButton): result := 'TButton';
    ord(TBitBtn): result := 'TBitBtn';
    ord(TSpeedButton): result := 'TSpeedButton';

    ord(TCheckBox): result := 'TCheckBox';
    ord(TRadioButton): result := 'TRadioButton';
    ord(TRadioGroup): result := 'TRadioGroup';

    ord(TListBox): result := 'TListBox';
    ord(TComboBox): result := 'TComboBox';

    ord(TEdit): result := 'TEdit';
    ord(TMemo): result := 'TMemo';

    ord(TPanel): result := 'TPanel';
    ord(THeader): result := 'THeader';
    ord(TGroupBox): result := 'TGroupBox';
    ord(TScrollBox): result := 'TScrollBox';

    ord(TImage): result := 'TImage';
    ord(TShape): result := 'TShape';
    ord(TPaintBox): result := 'TPaintBox';

    ord(TSpinEdit): result := 'TSpinEdit';
    end;
  end;

type
THint = class
  public
  formClassName: string;
  componentName: string;
  shortHint: string;
  longHint: string;
  end;

procedure TTab2AspForm.generateComponentsListClick(Sender: TObject);
  var
    inputFile, hintsFile: TextFile;
    outputFile: TextFile;
    aFileName, hintsFileName, mask, aLine, formName, typeName, componentName, filesDir: string;
    shortHint, longHint, ignore: string;
    searchRec: TSearchRec;
    i: longint;
    done, formLineFound: boolean;
    hints: TList;
    hint: THint;
  begin
  hints := TList.create;
  with openDialog do
    begin
    filter := 'Pascal files (*.pas)|*.pas';
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    title := 'Choose a Pascal file to read all PAS files in directory';
    end;
  if not openDialog.execute then exit;
  aFileName := openDialog.fileName;
  with openDialog do
    begin
    filter := 'Tab files (*.tab)|*.tab';
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    title := 'Choose an existing hints file to merge with the new file';
    end;
  if not openDialog.execute then exit;
  hintsFileName := openDialog.fileName;
  assignFile(hintsFile, hintsFileName);
  try
    reset(hintsFile);
    while not eof(hintsFile) do
      begin
      hint := THint.create;
      hint.formClassName := readUntilTab(hintsFile);
      ignore := readUntilTab(hintsFile); {component type}
      hint.componentName := readUntilTab(hintsFile);
      hint.shortHint := readUntilTab(hintsFile);
      hint.longHint := readUntilTab(hintsFile);
      hints.add(hint);
      end;
  finally
    closeFile(hintsFile);
  end;
  try
  assignFile(outputFile, extractFilePath(aFileName) + 'controls.tab');
  rewrite(outputFile);
  writeln(outputFile, 'form' + chr(9) + 'type' + chr(9) + 'name' + chr(9) + 'shortHint' + chr(9) + 'longHint');
  { get directory }
  filesDir := extractFilePath(aFileName);
  mask := filesDir + '*.pas';
  FindFirst(mask, faAnyFile, SearchRec);
  done := false;
  while not done do
    begin
    aFileName := filesDir + searchRec.name;
    try
    say('Looking at ' + lowerCase(aFileName));
    assignFile(inputFile, aFileName);
    reset(inputFile);
    formLineFound := false;
    while not eof(inputFile) do
      begin
      readln(inputFile, aLine);
      if (pos('(GsForm)', aLine) > 0) {or (pos('(TForm)', aLine) > 0)} then
        begin
        formName := copy(aLine, 1, pos(' =', aLine));
        formName := trimLeftAndRight(formName);
        formLineFound := true;
        end;
      if pos('implementation', aLine) > 0 then
        break;
      if not formLineFound then
        continue;
      if (pos('procedure', aLine) > 0) or (pos('function', aLine) > 0) then
        break;
      if pos(':', aLine) = 0 then
        continue;
      typeName := '';
      for i := 0 to ord(high(componentConstants)) do
        if pos(stringForComponentType(i), aLine) > 0 then
          begin
          typeName := stringForComponentType(i);
          componentName := copy(aLine, 1, pos(':', aLine) - 1);
          componentName := trimLeftAndRight(componentName);
          end;
      if typeName = '' then
        continue;
      { match up componentName with existing hints }
      shortHint := '(none)';
      longHint := '(none)';
      for i := 0 to hints.count - 1 do
        begin
        hint := THint(hints.items[i]);
        if (hint.componentName = componentName) and (hint.formClassName = formName) then
          begin
          shortHint := hint.shortHint;
          longHint := hint.longHint;
          end;
        end;
      write(outputFile, formName + chr(9) + typeName + chr(9) + componentName + chr(9) + shortHint + chr(9));
      writeln(outputFile, longHint);
      end;
    finally
      flush(outputFile);
      closeFile(inputFile);
      application.processMessages;
    end;
    done := FindNext(SearchRec) < 0;
    end;
  finally
    closeFile(outputFile);
    with hints do if count > 0 then for i := 0 to count - 1 do THint(items[i]).free;
    hints.free;
  end;
  say('File controls.tab written.');
  end;

const writingForHelpFile = false;

procedure TTab2AspForm.generateAspectsHelpFileClick(Sender: TObject);
  var
    groupsFileName, hintsFileName, outputFileName: string;
    groupsFile, hintsFile, outputFile: TextFile;
    lastGroupName, groupName, fieldID: string;
    hints: TList;
    hint: THint;
    i: longint;
  begin
  hints := TList.create;
  { open groups file and aspect hints file and output file }
  with openDialog do
    begin
    filter := 'Tab files (*.tab)|*.tab';
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    title := 'Choose a Groups file to read groups from';
    end;
  if not openDialog.execute then exit;
  groupsFileName := openDialog.fileName;
  with openDialog do
    begin
    filter := 'Tab files (*.tab)|*.tab';
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    title := 'Choose an aspect hints file for the aspect help';
    end;
  if not openDialog.execute then exit;
  say('Reading groups and hints . . . ');
  hintsFileName := openDialog.fileName;
  outputFileName := extractFilePath(hintsFileName) + '\asphelp.rtf';
  assignFile(groupsFile, groupsFileName);
  assignFile(hintsFile, hintsFileName);
  assignFile(outputFile, outputFileName);
  try
    reset(hintsFile);
    { skip header line }
    readln(hintsFile);
    { read hints into list }
    while not eof(hintsFile) do
      begin
      hint := THint.create;
      hint.componentName := readUntilTab(hintsFile);   {fieldID}
      hint.shortHint := readUntilTab(hintsFile); {name}
      hint.longHint := readUntilTab(hintsFile); {hint}
      hints.add(hint);
      end;
    reset(groupsFile);
    rewrite(outputFile);
    writeln(outputFile, '{\rtf1\ansi ');
    writeln(outputFile, '{\stylesheet{\f5\fs20 \snext0 Normal;}'
      + '{\s2\keepn \b\f5\fs20 \sbasedon0\snext0 heading 2;}'
      + '{\s3\keepn \b\f5 \sbasedon0\snext0 heading 3;}}');
    { go through groups file }
    lastGroupName := '';
    { skip header line }
    readln(groupsFile);
    say('Writing rtf file . . . ');
    while not eof(groupsFile) do
      begin
      groupName := readUntilTab(groupsFile);
      { when group changes, write out group name with something for header 2 }
      if groupName <> lastGroupName then
        begin
        if lastGroupName <> '' then
          write(outputFile, '\par \par ');
        writeln(outputFile, '\pard\plain \s3\keepn \b\f5 ' + groupName);
        end;
      { read the aspect field ID }
      fieldID := readUntilTab(groupsFile);
      { find that hint (assume no duplicates }
      hint := nil;
      if hints.count > 0 then
        for i := 0 to hints.count - 1 do
          begin
          hint := THint(hints.items[i]);
          if hint.componentName{fieldID} = fieldID then
            break;
          end;
      if hint <> nil then
        begin
        if writingForHelpFile then
          begin
          { if the aspect is found, write out the aspect name as header 3 and then the hint as just text }
          writeln(outputFile, '\par \par \pard\plain \s2\keepn \b\f5\fs20 ' + trimQuotes(hint.shortHint){name});
          write(outputFile, '\par \pard\plain \f5\fs20 ');
          writeln(outputFile, hint.longHint{hint});
          end
        else
          begin
          writeln(outputFile, '\par \pard\plain \f5\fs20 {\b ' + trimQuotes(hint.shortHint){name} + '}: ');
          write(outputFile, '\f5\fs20 ');
          writeln(outputFile, hint.longHint{hint});
          end;
        end;
      readln(groupsFile);
      lastGroupName := groupName;
      application.processMessages;
      end;
  finally
  writeln(outputFile, '}');
  with hints do if count > 0 then for i := 0 to count - 1 do THint(items[i]).free;
  hints.free;
  closeFile(groupsFile);
  closeFile(hintsFile);
  closeFile(outputFile);
  say('File ' + outputFileName + ' written.');
  end;
  end;


procedure TTab2AspForm.mergeTwoAspectHintFilesClick(Sender: TObject);
  var
    aspectsFile, existingHintsFile, outputFile: TextFile;
    aspectsFileName, existingHintsFileName: string;
    i, j: longint;
    newHints, existingHints: TList;
    newHint, existingHint: THint;
  begin
  newHints := TList.create;
  existingHints := TList.create;
  { get aspect and hints files }
  with openDialog do
    begin
    fileName := 'aspects.tab';
    filter := 'Tab files (*.tab)|*.tab';
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    title := 'Choose an aspects file to use the IDs and names';
    end;
  if not openDialog.execute then exit;
  aspectsFileName := openDialog.fileName;
  assignFile(aspectsFile, aspectsFileName);
  with openDialog do
    begin
    fileName := 'asphints.tab';
    filter := 'Tab files (*.tab)|*.tab';
    options := options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    title := 'Choose an existing aspect hints file to use the hints';
    end;
  if not openDialog.execute then exit;
  existingHintsFileName := openDialog.fileName;
  assignFile(existingHintsFile, existingHintsFileName);
  { read both files }
  say('Reading existing hints . . .');
  try
    reset(existingHintsFile);
    readln(existingHintsFile); {skip labels}
    while not eof(existingHintsFile) do
      begin
      existingHint := THint.create;
      existingHint.componentName := readUntilTab(existingHintsFile);   {fieldID}
      existingHint.shortHint := readUntilTab(existingHintsFile); {name}
      existingHint.longHint := readUntilTab(existingHintsFile); {hint}
      existingHints.add(existingHint);
      end;
  finally
    closeFile(existingHintsFile);
  end;
  say('Reading aspects . . .');
  try
    reset(aspectsFile);
    readln(aspectsFile); {skip labels}
    while not eof(aspectsFile) do
      begin
      newHint := THint.create;
      newHint.componentName := readUntilTab(aspectsFile);   {fieldID}
      newHint.shortHint := readUntilTab(aspectsFile); {name}
      { since we are assuming we are going from the aspects.tab file we ignore the rest of the line }
      readln(aspectsFile);
      newHints.add(newHint);
      end;
  finally
    closeFile(aspectsFile);
  end;
  { write out to new file }
  try
    say('Comparing and writing out . . .');
    assignFile(outputFile, extractFilePath(existingHintsFileName) + 'newhints.tab');
    rewrite(outputFile);
    writeln(outputFile, 'FieldID' + chr(9) + 'name' + chr(9) + 'hint');
    { match up fieldID with existing hints }
    for i := 0 to newHints.count - 1 do
      begin
      newHint := THint(newHints.items[i]);
      for j := 0 to existingHints.count - 1 do
        begin
        existingHint := THint(existingHints.items[j]);
        if (newHint.componentName = existingHint.componentName) {fieldIDs match} then
          begin
          newHint.shortHint := existingHint.shortHint; {name}
          newHint.longHint := existingHint.longHint; {hint}
          end;
        end;
      write(outputFile, newHint.componentName + chr(9) + newHint.shortHint + chr(9));
      writeln(outputFile, newHint.longHint);
      end;
  finally
    closeFile(outputFile);
    with newHints do if count > 0 then for i := 0 to count - 1 do THint(items[i]).free;
    newHints.free;
    with existingHints do if count > 0 then for i := 0 to count - 1 do THint(items[i]).free;
    existingHints.free;
  end;
  say('File newhints.tab written.');
  end;

end.


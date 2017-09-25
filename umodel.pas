unit Umodel;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
umodel: GsModel is the base class for model objects. Based on streamable object (in ufiler). Encapsulates
some transfer functions for browser/grapher access to models and for streaming and creating.
The graphical model subclass adds some basic drawing code (keeping a bounds rectangle,
checking if a point is in it), a name, and a note. All the model objects (garden,
weather, plant, soil, amendment, OM blob) are descended from the graphical model.}

interface

uses Graphics, WinTypes, ExtCtrls, SysUtils, WinProcs, Classes, 
		ufiler, uaspects, umconsts, ucollect;

const
  kUnitSystemEnglish = 0;
  kUnitSystemMetric = 1;

const
  kGetField = 0;
  kSetField = 1;

const
  kEdibleItem = 0;
  kGarden = 1;
  kOrganicMatterBlob = 2;
  kPlant = 3;
  kSoilPatch = 4;
  kTool = 5;
  kWeather = 6;

const
  kGraphicalModelNameLength = 80;

type

  GsModel = class(GsStreamableObject)
  	public
    isTemplate: boolean;
    wholeModelUpdateNeeded: boolean;
    procedure directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
      updateList: TListCollection); virtual;
    procedure transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
      updateList: TListCollection); virtual;
    procedure defaultField(fieldID: integer; fieldIndex: integer; updateList: TListCollection); virtual;
    procedure defaultAllFields; virtual;
    procedure addToUpdateList(fieldID: integer; fieldIndex: integer; updateList: TListCollection);
		procedure MFD(var objectValue; var value; fieldType: integer; direction: integer);
		procedure MFDSA(var objectValueArray: array of single;
      var valueArray: array of single; fieldType: integer; fieldIndex: integer; direction: integer);
		procedure convertValueOrString(var valueString: string; var value; fieldType: integer; direction: integer);
    function stringForColor(color: TColorRef): string;
    function colorForString(colorString: string): TColorRef;
    procedure becomeTemplateBasedOn(model: GsModel); virtual;
    function objectType: integer; virtual;
  	procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
 	 	procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    function getName: string; override;
    procedure setName(newName: string); override;
    end;

  GsModelUpdateEvent = class
    public
    model: GsModel;
    fieldID: integer;
    fieldIndex: integer;
    end;

  GsGraphicalModel = class(GsModel)
  	public
    boundsRect : TRect;
    privateName: string[kGraphicalModelNameLength];
    note: Pchar;
    destructor destroy; override;
    function includesPoint(const aPoint: TPoint): boolean; virtual;
    function preciseIncludesPointTest(const aPoint: TPoint): boolean; virtual;
    procedure drawOn(destCanvas: TCanvas); virtual;
 		procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
 	 	procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    function getName: string; override;
    procedure setName(newName: string); override;
  	end;

implementation

uses uclasses, usupport;

{GsModel}
function GsModel.getName: string;
  begin
  result := 'not nameable';
  raise Exception.create('this object can''t have a name');
  end;

procedure GsModel.setName(newName: string);
  begin
  raise Exception.create('this object can''t have a name');
  end;

procedure GsModel.transferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
  {var
    oldSingle: single;
    oldInteger: integer; }
	begin
  { MOST SIMPLE CASE - JUST CALL directTransferField }
  {oldSingle := 0.0;
  oldInteger := 0;
  if d = kSetField then
    begin
    if ft = kFieldFloat then
      directTransferField(kGetField, oldSingle, fieldID, ft, index, deriveMethod, updateList)
    else if ft = kFieldInt then
      directTransferField(kGetField, oldInteger, fieldID, ft, index, deriveMethod, updateList);
    end; }
  self.directTransferField(d, v, fieldID, ft, index, deriveMethod, updateList);
  if (self.isTemplate) and (d = kSetField) then templatesFileMightHaveChanged := true;
  {case fieldID of
     add special cases here - special cases may use oldSingle or oldInteger values
    end;}
  end;

procedure GsModel.directTransferField(d: integer; var v; fieldID, ft, index, deriveMethod: smallint;
    updateList: TListCollection);
	begin
  {do nothing - subclass can override}
  end;

procedure GsModel.defaultField(fieldID: integer; fieldIndex: integer; updateList: TListCollection);
  begin
  {do nothing - subclass can override}
  end;

procedure GsModel.defaultAllFields;
  begin
  {do nothing - subclass can override}
  end;

procedure GsModel.addToUpdateList(fieldID: integer; fieldIndex: integer; updateList: TListCollection);
  var updateEvent: GsModelUpdateEvent;
	begin
  if updateList = nil then exit;
  updateEvent := GsModelUpdateEvent.create;
  updateEvent.model := self;
  updateEvent.fieldID := fieldID;
  updateEvent.fieldIndex := fieldIndex;
  updateList.add(updateEvent);
  end;

{may need to limit length of string copy?}
{MFD = MoveFieldData}
procedure GsModel.MFD(var objectValue; var value; fieldType: integer; direction: integer);
	begin
  if direction = kGetField then
    case fieldType of
  		kFieldFloat: 					single(value) := single(objectValue);
  		kFieldInt:   					smallint(value) := smallint(objectValue);
  		kFieldUnsignedLong:   longint(value) := longint(objectValue); {unsigned?}
  		kFieldUnsignedChar:   byte(value) := byte(objectValue);
  		kFieldString:         string(value) := string(objectValue);
  		kFieldFileString:     string(value) := string(objectValue);
  		kFieldColor:          TColorRef(value) := TColorRef(objectValue);
  		kFieldBoolean:        boolean(value) := boolean(objectValue);
      kFieldEnumeratedList: smallint(value) := smallint(objectValue);
    else
      raise Exception.create('Unsupported model transfer to field ' + IntToStr(fieldType));
    end
  else if direction = kSetField then
    case fieldType of
  		kFieldFloat: 					single(objectValue) := single(value);
  		kFieldInt:   					smallint(objectValue) := smallint(value);
  		kFieldUnsignedLong:   longint(objectValue) := longint(value); {unsigned?}
  		kFieldUnsignedChar:   byte(objectValue) := byte(value);
  		kFieldString:         string(objectValue) := string(value);
  		kFieldFileString:     string(objectValue) := string(value);
  		kFieldColor:          TColorRef(objectValue) := TColorRef(value);
  		kFieldBoolean:        boolean(objectValue) := boolean(value);
      kFieldEnumeratedList: smallint(objectValue) := smallint(value);
    else
      raise Exception.create('Unsupported model transfer to field ' + IntToStr(fieldType));
    end
	end;

(*
{include an update for a single derived aspect - commonest case}
procedure GsModel.MFDU(var objectValue; var value; fieldType: integer; direction: integer; fieldID: integer;
  fieldIndex: integer; updateList: TListCollection);
  begin
  self.MFD(objectValue, value, fieldType, direction);
  self.addToUpdateList(fieldID, fieldIndex, updateList);
  end;
*)

{MFDSA = MoveFieldDataSingleArray}
procedure GsModel.MFDSA(var objectValueArray: array of single;
  var valueArray: array of single; fieldType: integer; fieldIndex: integer; direction: integer);
	begin
  self.MFD(objectValueArray[fieldIndex], valueArray[fieldIndex], fieldType, direction);
  end;

procedure GsModel.convertValueOrString(var valueString: string; var value; fieldType: integer; direction: integer);
	begin
  { this should handle all field types }
  if direction = kGetField then
    begin
    case fieldType of
  		kFieldFloat: 					valueString := FloatToStr(single(value));
  		kFieldInt:   					valueString := IntToStr(smallint(value));
  		kFieldUnsignedLong:   valueString := IntToStr(longint(value)); {unsigned?}
    { kFieldUnsignedChar: }
  		kFieldString:         valueString := string(value);
  		kFieldFileString:     valueString := string(value);
  		kFieldColor:          valueString := self.stringForColor(TColorRef(value));
  		kFieldBoolean:        if boolean(value) then valueString := 'TRUE' else valueString := 'FALSE';
     { kFieldIcon: ;  }
    {  kFieldThreeDObject: ; }
      kFieldEnumeratedList: valueString := IntToStr(smallint(value));
    else
      raise Exception.create('Unsupported model transferUsingString ' + IntToStr(fieldType));
    end;
    end
  else if direction = kSetField then
    begin
    case fieldType of
  		kFieldFloat: 					try single(value) := StrToFloat(valueString); except on EConvertError do single(value) := 0.0; end;
  		kFieldInt:   					smallint(value) := StrToIntDef(valueString, 0);
  		kFieldUnsignedLong:   longint(value) := StrToIntDef(valueString, 0); {unsigned?}
    { kFieldUnsignedChar: }
  		kFieldString:         string(value) := valueString;
  		kFieldFileString:     string(value) := valueString;
  		kFieldColor:          TColorRef(value) := self.colorForString(valueString);
  		kFieldBoolean:        boolean(value) := ((valueString[1] = 'T') or (valueString[1] = 't'));
    {  kFieldIcon: ;  }
    {  kFieldThreeDObject: ; }
  		kFieldEnumeratedList: smallint(value) := StrToIntDef(valueString, 0);
    else
      raise Exception.create('Unsupported model transferUsingString ' + IntToStr(fieldType));
    end;
    end
  else raise Exception.create('unsupported direction');
  end;

function GsModel.stringForColor(color: TColorRef): string;
  begin
  result := intToStr(getRValue(color)) + ' ' + intToStr(getGValue(color)) + ' '
    + intToStr(getBValue(color));
  end;

function GsModel.colorForString(colorString: string): TColorRef;
{ PDF - not tested }
  var
    shortString: string;
    i, j, lastSpace, r, g, b: integer;
  begin
  i := 1;
  r := -1;
  g := -1;
  b := -1;
  lastSpace := 0;
  while i <= length(colorString) do
    begin
    if (colorString[i] = ' ') or (i = length(colorString)) then
      begin
      shortString := '';
      for j := lastSpace + 1 to i - 1 do shortString[j] := colorString[j];
      if r = -1 then
        r := strToIntDef(shortString, 0)
      else if g = -1 then
        g := strToIntDef(shortString, 0)
      else if b = -1 then
        b := strToIntDef(shortString, 0);
      lastSpace := i;
      end;
    i := i + 1;
    end;
  if r = -1 then r := 0;
  if g = -1 then g := 0;
  if b = -1 then b := 0;
  result := support_rgb(r, g, b);
  end;

procedure GsModel.becomeTemplateBasedOn(model: GsModel);
  begin
  model.copyTo(self);
  self.isTemplate := true;
  end;

function GsModel.objectType: integer;
  begin
  result := kObjectUndefined;
  end;

procedure GsModel.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsModel;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsModel.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamBoolean(isTemplate);
  end;

{GsGraphicalmodel}
destructor GsGraphicalModel.destroy;
  begin
  if note <> nil then
    begin
    StrDispose(note);
    note := nil;
    end;
  inherited destroy;
  end;

function GsGraphicalModel.getName: string;
  begin
  result := self.privateName;
  end;

procedure GsGraphicalModel.setName(newName: string);
  begin
  privateName := copy(newName, 1, kGraphicalModelNameLength);
  end;

function GsGraphicalModel.includesPoint(const aPoint: TPoint): boolean;
	begin
  if PtInRect(self.boundsRect, aPoint) then
    Result := self.preciseIncludesPointTest(aPoint)
  else
    Result := false;
  end;

function GsGraphicalModel.preciseIncludesPointTest(const aPoint: TPoint): boolean;
	begin
  Result := true;
  end;

procedure GsGraphicalModel.drawOn(destCanvas: TCanvas);
	begin
  end;

var
  globalString: string;

procedure GsGraphicalModel.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsGraphicalModel;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsGraphicalModel.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
	begin
  inherited streamDataWithFiler(filer, cvir);
  filer.streamRect(boundsRect);
  {take extra care to limit name size}
  if filer.isWriting then globalString := privateName;
  filer.streamShortString(globalString);
  if filer.isReading then self.setName(globalString);
  filer.streamPChar(note);
  end;

end.

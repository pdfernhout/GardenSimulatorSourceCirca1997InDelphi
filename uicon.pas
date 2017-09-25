unit Uicon;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uicon: Just an icon that is subclassed to be streamable with our system (see ufiler).
It also knows its name. Not very complicated. }

interface

uses graphics, ufiler, ufilertx, umodel;

type
GsIcon = class(GsStreamableObject)
  public
  name: string;
  icon: TIcon;
  originalIfCopy: GsIcon;
  inUse: boolean;
  constructor create; override;
  destructor destroy; override;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  procedure copyFrom(original: GsIcon);
  procedure clear;
	function isSameAs(other: GsIcon): boolean;
  function getName: string; override;
  procedure setName(newName: string); override;
  procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
  end;

implementation

uses classes, uclasses;

constructor GsIcon.create;
  begin
  inherited create;
  name := '';
  icon := TIcon.create;
  end;

destructor GsIcon.destroy;
  begin
  icon.free;
  icon := nil;
  inherited destroy;
  end;

function GsIcon.getName: string;
  begin
  result := self.name;
  end;

procedure GsIcon.setName(newName: string);
  begin
  name := newName;
  end;

procedure GsIcon.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsIcon;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsIcon.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  filer.streamIcon(icon);
  filer.streamShortString(name);
  filer.streamBoolean(inUse);
  end;

procedure GsIcon.copyFrom(original: GsIcon);
  begin
  if original = nil then exit;
  name := original.name;
  inUse := original.inUse;
  icon.assign(original.icon);
  end;

function GsIcon.isSameAs(other: GsIcon): boolean;
  var
    selfStream, otherStream: TMemoryStream;
    i: longint;
    selfBuffer: array[0..16] of char;
    otherBuffer: array[0..16] of char;
  begin
  result := false;
  if self.name <> other.name then exit;
  if (self.icon.width <> other.icon.width) or
  		(self.icon.height <> other.icon.height) or
    	(self.icon.empty <> other.icon.empty) then exit;
  {need to check if icon looks the same}
  selfStream := nil;
  otherStream := nil;
  try
  selfStream := TMemoryStream.create;
  otherStream := TMemoryStream.create;
  self.icon.saveToStream(selfStream);
  other.icon.saveToStream(selfStream);
   result := true;
  if selfStream.size = otherStream.size then
    begin
    {see if streams are the same - move both to start}
    selfStream.seek(0,0);
    otherStream.seek(0,0);
    if selfStream.size > 0 then
    	for i := 0 to selfStream.size - 1 do
      	begin
        selfStream.read(selfBuffer, 1);
        otherStream.read(otherBuffer, 1);
        if selfBuffer[0] <> otherBuffer[0] then
          begin
          result := false;
          break;
          end;
      	end;
    end;
  finally
  selfStream.free;
  otherStream.free;
  end;
  end;

procedure GsIcon.clear;
  begin
  name := '';
  icon.free;
  icon := TIcon.create;
  end;

procedure GsIcon.streamUsingTextFiler(textFiler: GsTextFiler);
  begin
  with textFiler do
    begin
    streamIcon(icon);
    streamEndOfLine;
    end;
  end;

end.

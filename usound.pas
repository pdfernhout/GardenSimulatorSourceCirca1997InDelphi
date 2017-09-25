unit Usound;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
usound: Sound object for tool sounds. Uses Delphi function
sndPlaySound (in the MMSYSTEM system included in WinTypes). Sounds are played
when tool actions are done in garden window (and 'play tool sounds' option is on),
and sounds are played, imported and exported in the tool editor. Because the
sounds are streamed with the tools, this is a streamable object.}

interface

uses SysUtils, Wintypes, WinProcs, Classes, ufiler;

type

  GsSound = class(GsStreamableObject)
    public
    soundMem: PChar;
    soundSize: longint;
    fileName: string;
    constructor create; override;
  	procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  	procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    procedure loadFromFile(fileName: string);
    procedure saveToFile(fileName: string);
    procedure play;
    procedure playWait;
    procedure playLoop;
    procedure stop;
		class procedure stopAny;
    function playIfDone: boolean;  {returns true if played sound}
		function playLoopIfDone: boolean;
    function isUsed: boolean;
    destructor destroy; override;
    procedure allocateSoundMem(theSoundSize: longint);
    procedure freeSoundMem;
    end;

implementation

uses mmsystem, uclasses;

{not really necessary as should default to nil and zero}
constructor GsSound.create;
  begin
  inherited create;
  soundMem := nil;
  soundSize := 0;
  end;

procedure GsSound.freeSoundMem;
  begin
  if soundMem <> nil then
    begin
  	GlobalFreePtr(soundMem);
  	soundMem := nil;
  	end;
  soundSize := 0;
  fileName := '';
  end;

destructor GsSound.destroy;
  begin
  self.freeSoundMem;
  inherited destroy;
  end;

procedure GsSound.loadFromFile(fileName: string);
	var
  	memoryStream: TMemoryStream;
	begin
	{free old sound}
	self.freeSoundMem;
  memoryStream := TMemoryStream.create;
  try
  memoryStream.loadFromFile(fileName);
  soundSize := memoryStream.seek(0,2);
  {allocate shared memory}
  self.allocateSoundMem(soundSize);
  {move to start of mem stream and copy sound to new memory}
  memoryStream.seek(0,0);
  {need caret because readBuffer expects actual thing}
  memoryStream.readBuffer(soundMem^, soundSize);
  self.fileName := lowerCase(fileName);
  finally
  memoryStream.free;
  memoryStream := nil;
  end;
  end;

procedure GsSound.saveToFile(fileName: string);
	var
  	memoryStream: TMemoryStream;
	begin
  memoryStream := TMemoryStream.create;
  try
  {copy sound to memoryStream memory}
  if soundMem <> nil then
  	memoryStream.writeBuffer(soundMem^, soundSize);
  memoryStream.saveToFile(fileName);
  finally
  memoryStream.free;
  memoryStream := nil;
  end;
  end;

procedure GsSound.play;
  begin
  if soundMem <> nil then sndPlaySound(soundMem, SND_MEMORY or SND_ASYNC);
  end;

procedure GsSound.playWait;
  begin
  if soundMem <> nil then sndPlaySound(soundMem, SND_MEMORY or SND_SYNC);
  end;

{returns true if played sound}
function GsSound.playIfDone: boolean;
  begin
  result := false;
  if soundMem <> nil then
  	result := sndPlaySound(soundMem, SND_MEMORY or SND_ASYNC or SND_NOSTOP)
  else
    result := true; {act like payed it so no one waits around for it}
  end;

function GsSound.playLoopIfDone: boolean;
  begin
  result := false;
  if soundMem <> nil then
  	result := sndPlaySound(soundMem, SND_MEMORY or SND_ASYNC or SND_NOSTOP or SND_LOOP)
  else
    result := true; {act like played it so no one waits around for it}
  end;

procedure GsSound.playLoop;
  begin
  if soundMem <> nil then
  	sndPlaySound(soundMem, SND_MEMORY or SND_ASYNC or SND_LOOP);
  end;

procedure GsSound.stop;
  begin
  GsSound.stopAny;
  end;

class procedure GsSound.stopAny;
  begin
	sndPlaySound(nil, SND_ASYNC);
  end;

function GsSound.isUsed: boolean;
  begin
  result := soundMem <> nil;
  end;

procedure GsSound.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsSound;
  cvir.versionNumber := 0;
  cvir.additionNumber := 1;
  end;

procedure GsSound.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  if filer.isReading then
    begin
    self.freeSoundMem;
    filer.streamLongint(soundSize);
    if soundSize > 0 then
      begin
      self.allocateSoundMem(soundSize);
      filer.stream.readBuffer(soundMem^, soundSize)
      end;
    end
  else if filer.isWriting then
    begin
 	 	if soundMem <> nil then
      begin
      filer.streamLongint(soundSize);
      {caret needed as writeBuffer expects actual thing, not pointer}
  		filer.stream.writeBuffer(soundMem^, soundSize);
      end
    else
      begin
      soundSize := 0;
      filer.streamLongint(soundSize);
      end;
    end;
  if cvir.additionNumber >= 1 then
    filer.streamShortString(fileName);
  end;

procedure GsSound.allocateSoundMem(theSoundSize: longint);
  begin
  if soundMem <> nil then
    raise Exception.create('GsSound.allocateSoundMem: soundMem should be nil');
  soundMem := GlobalAllocPtr(GMEM_MOVEABLE or GMEM_SHARE, theSoundSize);
  if soundMem = nil then
    raise Exception.create('GsSound.allocateSoundMem: out of memory');
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


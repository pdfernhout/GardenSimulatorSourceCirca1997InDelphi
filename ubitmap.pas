unit Ubitmap;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubitmap: Mainly handles bitmap reading from files. GsBitmap is subclassed to make it
streamable with our system. }

{$R-} {for MemFree which has incorrect second param when long size}

interface

uses WinProcs, WinTypes, SysUtils, Graphics, Classes, ufiler;

const
typePM1 = 1;
typeWin3 = 2;

type

	TDib = class
  public
  dibType: integer;
  Bmf: TBitmapFileHeader;
  dibStreamSize: longint;
  {for win 3}
  wBitsMem: Pointer;
  wImageSize: longint;
  wSize: longint;
  wBitmapHeader: TBitmapInfoHeader;
  wBitmapInfo: PBitmapInfo;
  {for pm}
  pmBitsMem: Pointer;
  pmSize: longint;
  pmMaxSize: longint;
  pmBitmapHeader: TBitmapCoreHeader;
  pmBitmapInfo: PBitmapCoreInfo;
  destructor destroy; override;
	procedure LoadFromStream(aStream: TStream);
	procedure ReadDIB(Stream: TStream);
	procedure ReadWin3DIB(Stream: TStream);
	procedure ReadPM1DIB(Stream: TStream);
	procedure storeInBitmap(aBitmap: TBitmap);
  end;

  {this class is subclassed to get at superclass streaming functions -
   they are not complete streamable objects - so they can't be stored in collections
   unless they are embedded inside true streamable objects}
  GsBitmap = class(TBitmap)
    public
  	procedure streamUsingFiler(filer: GsFiler); {not overriden because not based on streamable object}
    end;

procedure RealizeGlobalPalette(Pal: HPALETTE);
procedure LoadDibFromStreamAndPutInBitmap(aStream: TStream; destBitmap: TBitmap);
procedure LoadDibFromStreamAndPutInBitmapWithPalette(aStream: TStream; destBitmap: TBitmap; paletteBitmap: TBitmap);
procedure LoadDibFromFileAndPutInBitmap(fileName: string; destBitmap: TBitmap; paletteBitmap: TBitmap);
function CopyPalette(Palette: HPALETTE): HPALETTE;

implementation

uses udomain;

destructor TDib.destroy;
  begin
  if dibType = typeWin3 then
    begin
    if wBitsMem <> nil then
      FreeMem(wBitsMem, wImageSize);
    if wBitmapInfo <> nil then
    	FreeMem(wBitmapInfo, wSize + SizeOf(TBitmapInfoHeader));
    end
  else if dibType = typeWin3 then
    begin
    if pmBitsMem <> nil then
    	FreeMem(pmBitsMem, pmMaxSize);
    if pmBitmapInfo <> nil then
    	FreeMem(pmBitmapInfo, pmSize + SizeOf(TBitmapCoreInfo));
    end;
  inherited destroy;
  end;

procedure InvalidBitmap; near;
begin
  raise Exception.create('invalid bitmap');
end;

procedure OutOfResources; near;
begin
  raise Exception.create('out of resources');
end;

procedure TDib.LoadFromStream(aStream: TStream);
	begin
  if dibStreamSize = 0 then raise Exception.create('Tdib: Empty stream');
  aStream.Read(Bmf, SizeOf(Bmf));
  wImageSize := dibStreamSize - SizeOf(Bmf);
  self.ReadDIB(aStream);
	end;

procedure TDib.ReadDIB(Stream: TStream);
	var
  	HeaderSize: Longint;
	begin
  Stream.Read(HeaderSize, SizeOf(HeaderSize));
  if HeaderSize = SizeOf(TBitmapInfoHeader) then
    begin
    dibType := typeWin3;
    ReadWin3DIB(Stream);
    end
  else if HeaderSize = SizeOf(TBitmapCoreHeader) then
    begin
    dibType := typePM1;
    ReadPM1DIB(Stream)
    end
  else
    InvalidBitmap;
	end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;

procedure TDib.ReadWin3DIB(Stream: TStream);
	begin
  Stream.Read(Pointer(Longint(@wBitmapHeader) + SizeOf(Longint))^,
    SizeOf(TBitmapInfoHeader) - SizeOf(Longint));
  wBitmapHeader.biSize := sizeof(TBitmapInfoHeader);

  { check number of planes. Windows 3.x supports only 1 plane DIBS }
  if wBitmapHeader.biPlanes <> 1 then InvalidBitmap;

  with wBitmapHeader do
  	begin
    if biClrUsed = 0 then
      biClrUsed := GetDInColors(biBitCount);
    wSize := biClrUsed * SizeOf(TRgbQuad);
  	end;

  wBitmapInfo := MemAlloc(wSize + SizeOf(TBitmapInfoHeader));
  if wBitmapInfo = nil then
    raise Exception.create('TDib.ReadWin3DIB: out of memory (wBitmapInfo)');
  with wBitmapInfo^ do
    begin
    bmiHeader := wBitmapHeader;
    Stream.Read(bmiColors, wSize);

    { some applications do not fill in the SizeImage field in the header.
    (Actually the truth is more likely that some drivers do not fill the field
    in and the apps do not compensate for these buggy drivers.) Therefore, if
    this field is 0, we will compute the size. }
    with bmiHeader do
    	begin
      Dec(wImageSize, SizeOf(TBitmapInfoHeader) + wSize);
      if biSizeImage <> 0 then
        if biSizeImage < wImageSize then
          wImageSize := biSizeImage;
      wBitsMem := MemAlloc(wImageSize);
      if wBitsMem = nil then
        begin
        if wBitmapInfo <> nil then
    	    FreeMem(wBitmapInfo, wSize + SizeOf(TBitmapInfoHeader));
        raise Exception.create('TDib.ReadWin3DIB: out of memory (wBitsMem)');
        end;
      Stream.Read(wBitsMem^, wImageSize);
      end;
    end;
  end;

{ Read a PM 1.x device independent bitmap. }
procedure TDib.ReadPM1DIB(Stream: TStream);
	begin
  Stream.Read(Pointer(Longint(@pmBitmapHeader) + SizeOf(longint))^,
    SizeOf(TBitmapCoreHeader) - SizeOf(Longint));
  pmBitmapHeader.bcSize := sizeof(TBitmapCoreHeader);
  if pmBitmapHeader.bcPlanes <> 1 then InvalidBitmap;

  pmSize := GetDInColors(pmBitmapHeader.bcBitCount) * SizeOf(TRGBTriple);
  pmBitmapInfo := MemAlloc(pmSize + SizeOf(TBitmapCoreInfo));
  if pmBitmapInfo = nil then
    raise Exception.create('TDib.ReadPM1DIB: out of memory (pmBitmapInfo)');
  with pmBitmapInfo^ do
    begin
    bmciHeader := pmBitmapHeader;
    Stream.Read(bmciColors, pmSize);

    { size of image = Width of a scan line * number of scan lines Width = Pixel
    Width * bits per pixel rounded to a DWORD boundary }
    with bmciHeader do
      pmMaxSize := ((((bcWidth * bcBitCount) + 31) div 32) * 4) * bcHeight;

    pmBitsMem := MemAlloc(pmMaxSize);
    if pmBitsMem = nil then
      begin
      if pmBitmapInfo <> nil then
        FreeMem(pmBitmapInfo, pmSize + SizeOf(TBitmapCoreInfo));
      raise Exception.create('TDib.ReadPM1DIB: out of memory (pmBitsMem)');
      end;
    Stream.Read(pmBitsMem^, pmMaxSize);
		end;
	end;

procedure TDib.storeInBitmap(aBitmap: TBitmap);
  begin
  if dibType = typeWin3 then
    begin
    {free up bitmap first}
    aBitmap.width := 0;
    aBitmap.height := 0;
    aBitmap.width := wBitmapHeader.biWidth;
    aBitmap.height := wBitmapHeader.biHeight;
    {assuming correct palette loaded into bitmap}
  	StretchDIBits(aBitmap.canvas.handle, 0, 0, wBitmapHeader.biWidth, wBitmapHeader.biHeight,
    	0, 0, wBitmapHeader.biWidth, wBitmapHeader.biHeight, wBitsMem, wBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);
    end
  else if dibType = typeWin3 then
    begin
    {free up bitmap first}
    aBitmap.width := 0;
    aBitmap.height := 0;
    aBitmap.width := pmBitmapHeader.bcWidth;
    aBitmap.height := pmBitmapHeader.bcHeight;
    {cast for pmBitmapInfo is same as in graphics}
  	StretchDIBits(aBitmap.canvas.handle, 0, 0, pmBitmapHeader.bcWidth, pmBitmapHeader.bcHeight,
    	0, 0, pmBitmapHeader.bcWidth, pmBitmapHeader.bcHeight, pmBitsMem, PBitmapInfo(pmBitmapInfo)^, DIB_RGB_COLORS, SRCCOPY);
    end;
  end;

{assumes size is not on stream}
procedure LoadDibFromStreamAndPutInBitmap(aStream: TStream; destBitmap: TBitmap);
  var
    aDib: TDib;
  begin
  aDib := TDib.create;
  try
  aDib.dibStreamSize := aStream.size;
  if aDib.dibStreamSize > 0 then
    begin
  	aDib.LoadFromStream(aStream);
  	aDib.storeInBitmap(destBitmap);
    end
  else {empty dib}
    begin
    destBitmap.width := 0;
    destBitmap.height := 0;
    end;
  finally
  aDib.free;
  end;
  end;

{assumes size is on stream}
procedure LoadDibFromStreamAndPutInBitmapWithPalette(aStream: TStream; destBitmap: TBitmap; paletteBitmap: TBitmap);
  var
    aDib: TDib;
    dibStreamSize: Longint;
	begin
  destBitmap.palette := CopyPalette(paletteBitmap.palette);
  aDib := TDib.create;
  try
  aStream.read(dibStreamSize, SizeOf(dibStreamSize));
  aDib.dibStreamSize := dibStreamSize;
  if dibStreamSize > 0 then
    begin
  	aDib.LoadFromStream(aStream);
  	aDib.storeInBitmap(destBitmap);
    end
  else {empty dib}
    begin
    destBitmap.width := 0;
    destBitmap.height := 0;
    end;
  finally
  aDib.free;
  end;
  end;

procedure LoadDibFromFileAndPutInBitmap(fileName: string; destBitmap: TBitmap; paletteBitmap: TBitmap);
	var
  	fileStream: TFileStream;
	begin
  destBitmap.palette := CopyPalette(paletteBitmap.palette);
	fileStream := TFileStream.create(fileName, fmOpenRead);
	try
    LoadDibFromStreamAndPutInBitmap(fileStream, destBitmap);
	finally
		fileStream.free;
	end;
	end;

procedure RealizeGlobalPalette(Pal: HPALETTE);
var
  Focus: HWND;
  DC: HDC;
  OldPal: HPALETTE;
	begin
	{ we use the handle of the window with the focus (the splash screen) in order to guarantee
	that the realized palette will have first priority on the system palette }
	Focus := GetFocus;
	DC := GetDC(Focus);
  if DC = 0 then
  	raise Exception.create('Out of resources');
  try
  if Pal <> 0 then
    begin
    { select and realize our palette we have gotten the DC of the focus
    window just to make sure that all our colors are mapped }
    OldPal := SelectPalette(DC, Pal, False);
    RealizePalette(DC);
    if OldPal <> 0 then
      SelectPalette(DC, OldPal, true);
    end;
  finally
  ReleaseDC(Focus, DC);
  end;
end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogSize: Integer;
  LogPalette: PLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  GetObject(Palette, SizeOf(PaletteSize), @PaletteSize);
  LogSize := SizeOf(TLogPalette) + (PaletteSize - 1) * SizeOf(TPaletteEntry);
  GetMem(LogPalette, LogSize);
  try
    with LogPalette^ do
    begin
      palVersion := $0300;
      palNumEntries := PaletteSize;
      GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
    end;
    Result := CreatePalette(LogPalette^);
  finally
    FreeMem(LogPalette, LogSize);
  end;
end;

{GsBitmap}
{subclassed to get at protected streaming functions!}
procedure GsBitmap.streamUsingFiler(filer: GsFiler);
  begin
  if filer.isReading then
    begin
  	if (Domain <> nil) and Domain.paletteBitmapLoaded then
      begin
    	LoadDibFromStreamAndPutInBitmapWithPalette(filer.stream, self, Domain.paletteBitmap);
      end
    else
      self.ReadData(filer.stream);
    end
  else if filer.isWriting then
  	self.WriteData(filer.stream);
  end;


end.

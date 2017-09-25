unit Ubackdrp;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubackdrp: Backdrop chooser window. Displays current backdrop in picture, allows user to
choose new one or choose not to use one. Allows user to change background color even
if still using backdrop (because can turn off display of backdrop in options menu).
Deals with loading new bitmap into domain.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, ubitmap, ugsform;

type
  TBackdropChooserForm = class(GsForm)
    useBackdropFromBitmapFile: TRadioButton;
    dontUseBitmap: TRadioButton;
    dontUseBackdropPanel: TPanel;
    widthEdit: TEdit;
    label23: TLabel;
    Label11: TLabel;          
    heightEdit: TEdit;
    useBackdropPanel: TPanel;
    backdropFileName: TEdit;
    backdropChoose: TButton;
    OK: TButton;
    Cancel: TButton;
    helpButton: TBitBtn;
    gardenBitmapPaintBox: TPaintBox;
    ColorDialog: TColorDialog;
    Label2: TLabel;
    Label1: TLabel;
    backgroundColorPanel: TPanel;
    removeFilePath: TButton;
    procedure backdropChooseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure useBackdropFromBitmapFileClick(Sender: TObject);
    procedure dontUseBitmapClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure gardenBitmapPaintBoxPaint(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure backgroundColorPanelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure removeFilePathClick(Sender: TObject);
  private
    { Private declarations }
  public
    loading: boolean;
    newBackdropBitmap: GsBitmap;
    resizedBitmap: boolean;  {used by caller so see if changed size}
    redrawNeeded: boolean;
    truncateNameWhenFinished: boolean;
		procedure transferXY(transferDirection: integer; xEdit: TEdit; yEdit: tEdit; var point: TPoint);
		procedure backdropUsedChanged;
		function GetPalette: HPALETTE; override;
		function PaletteChanged(Foreground: Boolean): Boolean; override;
  end;

implementation

{$R *.DFM}

uses udomain, ucursor, ugsim, udebug, usupport;

const
  kLoadIntoForm = 1;
  kLoadFromForm = 2;

procedure TBackdropChooserForm.FormCreate(Sender: TObject);
	begin
  resizedBitmap := false;
  redrawNeeded := false;
  truncateNameWhenFinished := false;
  loading := true;
  newBackdropBitmap := nil;
  useBackdropFromBitmapFile.checked := Domain.fileOptions.backdropUsed;
  backgroundColorPanel.color := Domain.menuOptions.backgroundColor;
  dontUseBitmap.checked := not Domain.fileOptions.backdropUsed;
  backdropFileName.text := lowerCase(Domain.fileOptions.backdropFileName);
  self.transferXY(kLoadIntoForm, widthEdit, heightEdit, Domain.gardenSize);
  gardenBitmapPaintBox.invalidate;
  loading := false;
	end;

function TBackdropChooserForm.GetPalette: HPALETTE;
  begin
  if Application.terminated or (Domain = nil) or (not Domain.paletteBitmapLoaded) then
    result := inherited GetPalette
  else
  	result := Domain.paletteBitmap.Palette
  end;

procedure TBackdropChooserForm.OKClick(Sender: TObject);
  var
  	newSize: TPoint;
    newGardenBitmap: GsBitmap;
    newPlantBitmap: GsBitmap;
	begin
  newSize := Point(0,0);
  newGardenBitmap := nil;
  newPlantBitmap := nil;
  { because the background color can show even if there is a backdrop (if the user unchecks "Show backdrop" in
    the options menu), it should be editable even if there is a backdrop used. }
  if Domain.menuOptions.backgroundColor <> backgroundColorPanel.color then
    begin
    Domain.menuOptions.backgroundColor := backgroundColorPanel.color;
    redrawNeeded := true;
    end;
  if self.useBackdropFromBitmapFile.checked and
  	((self.backdropFileName.text = '') or
    ((not Domain.fileOptions.backdropUsed) and (self.newBackdropBitmap = nil))) then
    begin
    showMessage('A backdrop file must be chosen if "Use backdrop from bitmap file" is selected.');
    modalResult := mrNone;
    exit;
    end;
  if self.useBackdropFromBitmapFile.checked then
    begin
    { exit if didn't change bitmap -- but save new name if truncating }
    if (self.newBackdropBitmap = nil) then
      begin
      if truncateNameWhenFinished then
        Domain.fileOptions.backdropFileName := extractFileName(backdropFileName.text);
      exit;
      end;
    {see if can allocate new plant and tool bitmaps}
    if ((Domain.gardenSize.x = newBackdropBitmap.width) and
      (Domain.gardenSize.y = newBackdropBitmap.height)) then
      {size of supporting bitmaps does not need to change}
      begin
      Domain.gardenBackdropBitmap.free;
      Domain.gardenBackdropBitmap := newBackdropBitmap;
     	newBackdropBitmap := nil;
     	end
    else
      begin
    	try
    	  newGardenBitmap := GsBitmap.create;
    	  newPlantBitmap := GsBitmap.create;
  		  newGardenBitmap.width := newBackdropBitmap.width;
  		  newGardenBitmap.height := newBackdropBitmap.height;
        newGardenBitmap.palette := CopyPalette(newBackdropBitmap.palette);
  		  newPlantBitmap.width := newBackdropBitmap.width;
  		  newPlantBitmap.height := newBackdropBitmap.height;
        newPlantBitmap.palette := CopyPalette(newBackdropBitmap.palette);
    	except
    	  showMessage('There was a problem allocating memory for the new bitmap');
    	  newGardenBitmap.free;
    	  newGardenBitmap := nil;
    	  newPlantBitmap.free;
    	  newPlantBitmap := nil;
    	  modalResult := mrNone;
    	  exit;
    	end;
    	{put new bitmaps into domain}
  		Domain.setBackdropGardenPlantBitmaps(newBackdropBitmap, newGardenBitmap, newPlantBitmap);
    	{the domain now owns these bitmaps, so let go of them}
    	newBackdropBitmap := nil;
    	newGardenBitmap := nil;
  		newPlantBitmap := nil;
      resizedBitmap := true;
      end;
    if truncateNameWhenFinished then
      Domain.fileOptions.backdropFileName := extractFileName(backdropFileName.text)
    else
      Domain.fileOptions.backdropFileName := backdropFileName.text;
    Domain.fileOptions.backdropUsed := true;
    redrawNeeded := true;
    exit;
    end
  else
    begin
  	transferXY(kLoadFromForm, widthEdit, heightEdit, newSize);
  	if (newSize.x <= 0) or (newSize.x <= 0) then
    	begin
    	showMessage('Width and height must each be greater than zero');
    	modalResult := mrNone;
    	exit;
    	end;
    {free this first - in case allocated and need memory later}
    newBackdropBitmap.free;
    newBackdropBitmap := nil;
    {don't resize plant or garden bitmap if size if still the same}
    if (newSize.x = Domain.gardenSize.x) and (newSize.y = Domain.gardenSize.y) then
      begin
      Domain.gardenBackdropBitmap.free;
      Domain.gardenBackdropBitmap := nil;
      if Domain.fileOptions.backdropUsed then
        begin
        Domain.fileOptions.backdropUsed := false;
        Domain.fileOptions.backdropFileName := '';
        redrawNeeded := true;
        end;
      exit;
      end;
    {need to resize those other bitmaps}
    {see if can allocate new plant and tool bitmaps}
    try
    	newBackdropBitmap := GsBitmap.create;;
    	newGardenBitmap := GsBitmap.create;
    	newPlantBitmap := GsBitmap.create;
      newBackdropBitmap.width := 0;
      newBackdropBitmap.height := 0;
  		newGardenBitmap.width := newSize.x;
  		newGardenBitmap.height := newSize.y;
  		newPlantBitmap.width := newSize.x;
  		newPlantBitmap.height := newSize.y;
    except
    	showMessage('There was a problem allocating memory for the new bitmap.');
     	{change text in case of error - previously may have freed this bitmap}
  		backdropFileName.text := lowerCase(Domain.fileOptions.backdropFileName);
      newBackdropBitmap.free;
      newBackdropBitmap := nil;
    	newGardenBitmap.free;
    	newGardenBitmap := nil;
    	newPlantBitmap.free;
    	newPlantBitmap := nil;
    	modalResult := mrNone;
   		exit;
    end;
    {put new bitmaps into domain}
  	Domain.setBackdropGardenPlantBitmaps(newBackdropBitmap, newGardenBitmap, newPlantBitmap);
    {the domain now owns these bitmaps, so let go of them}
    newBackdropBitmap := nil;
    newGardenBitmap := nil;
  	newPlantBitmap := nil;
    resizedBitmap := true;
    Domain.fileOptions.backdropFileName := '';
    Domain.fileOptions.backdropUsed := false;
    redrawNeeded := true;
    exit;
    end;
	end;

procedure TBackdropChooserForm.CancelClick(Sender: TObject);
	begin
  {remove new bitmap if it was allocated}
	newBackdropBitmap.free;
  newBackdropBitmap := nil;
	end;

procedure TBackdropChooserForm.transferXY(transferDirection: integer; xEdit: TEdit; yEdit: tEdit; var point: TPoint);
  var numberString: string;
  begin
  if transferDirection = kLoadIntoForm then
    begin
    numberString := IntToStr(point.x);
    xEdit.text := numberString;
    numberString := IntToStr(point.y);
    yEdit.text := numberString;
    end
  else if transferDirection = kLoadFromForm then
    begin
    point.x := StrToIntDef(xEdit.text, 0);
    point.y := StrToIntDef(yEdit.text, 0);
    end
  else
  	raise exception.create('TStartupOptionsForm: invalid transfer option');
  end;

procedure TBackdropChooserForm.useBackdropFromBitmapFileClick(
  Sender: TObject);
	begin
	self.backdropUsedChanged;
	end;

procedure TBackdropChooserForm.dontUseBitmapClick(Sender: TObject);
	begin
	self.backdropUsedChanged;
	end;

procedure TBackdropChooserForm.backdropUsedChanged;
  begin
  backdropFileName.enabled := useBackdropFromBitmapFile.checked;
  backdropChoose.enabled := useBackdropFromBitmapFile.checked;
  widthEdit.enabled := not useBackdropFromBitmapFile.checked;
  heightEdit.enabled := not useBackdropFromBitmapFile.checked;
  { backgroundColorPanel is always enabled because it can be changed whether or not a backdrop is used }
  gardenBitmapPaintBox.invalidate;
  end;

procedure TBackdropChooserForm.backdropChooseClick(Sender: TObject);
  var
    newSize: TPoint;
    fileNameWithPath: string;
	begin
  fileNameWithPath := '';
  fileNameWithPath := GsFile_GetFileOpenInfo(kFileTypeBitmap, backdropFileName.text);
  if fileNameWithPath = '' then exit;
  newBackdropBitmap.free;
  newBackdropBitmap := nil;
  newBackdropBitmap := GsBitmap.create;
  cursor_startWait;
  try
    try
      if Domain.paletteBitmapLoaded then
        LoadDibFromFileAndPutInBitmap(fileNameWithPath, newBackdropBitmap, Domain.paletteBitmap)
      else
        begin
  	    newBackdropBitmap.loadFromFile(fileNameWithPath);
  	    GardenForm.invalidate; {because borland VCL damages color integrity of other windows}
        end;
    except
      newBackdropBitmap.free;
      newBackdropBitmap := nil;
      backdropFileName.text := '';
      raise;
    end;
  finally
    cursor_stopWait;
	end;
  newSize.x := newBackdropBitmap.width;
  newSize.y := newBackdropBitmap.height;
  self.transferXY(kLoadIntoForm, widthEdit, heightEdit, newSize);
  backdropFileName.text := lowerCase(fileNameWithPath);
	gardenBitmapPaintBox.invalidate;
	end;

procedure TBackdropChooserForm.gardenBitmapPaintBoxPaint(Sender: TObject);
  var
    theRect: TRect;
    theBitmap: GsBitmap;
    theWidth, theHeight, previousHeight: longint;
    oldPalette: HPALETTE;
	begin
  if useBackdropFromBitmapFile.checked then
    begin
    if newBackdropBitmap <> nil then
    	theBitmap := newBackdropBitmap
    else
    	theBitmap := Domain.gardenBackdropBitmap;
    if theBitmap = nil then exit;
    with theRect do
      begin
      theWidth := theBitmap.width;
      theHeight := theBitmap.height;
      if theWidth > gardenBitmapPaintBox.clientWidth then
        begin
        theWidth := gardenBitmapPaintBox.clientWidth;
        theHeight := round(1.0 * theWidth / theBitmap.width * theHeight);
        end;
      if theHeight > gardenBitmapPaintBox.clientHeight then
        begin
        previousHeight := theHeight;
        theHeight := gardenBitmapPaintBox.clientHeight;
        theWidth := round(1.0 * theHeight / previousHeight * theWidth);
        end;
      { center }
      left := gardenBitmapPaintBox.clientWidth div 2 - theWidth div 2;
      top := gardenBitmapPaintBox.clientHeight div 2 - theHeight div 2;
      right := left + theWidth;
      bottom := top + theHeight;
      end;
    if Domain.paletteBitmapLoaded then
      begin
    	oldPalette := SelectPalette(gardenBitmapPaintBox.canvas.handle, Domain.paletteBitmap.palette, false);
    	RealizePalette(gardenBitmapPaintBox.canvas.handle);
    	gardenBitmapPaintBox.canvas.stretchDraw(theRect, theBitmap);
    	SelectPalette(gardenBitmapPaintBox.canvas.handle, oldPalette, true);
      end
    else
    	gardenBitmapPaintBox.canvas.stretchDraw(theRect, theBitmap);
    end
  else
    begin
    theRect := gardenBitmapPaintBox.clientRect;
    winprocs.drawText(gardenBitmapPaintBox.canvas.handle,
    	'Enter width and height below',
    	strLen('Enter width and height below'), theRect, DT_CENTER);
    end;
	end;

{overriden because paint box does not have palette and will not update correctly}
{makes window take first priority for palettes}
function TBackdropChooserForm.PaletteChanged(Foreground: Boolean): Boolean;
	var
  	OldPalette, Palette: HPALETTE;
  	WindowHandle: HWnd;
  	DC: HDC;
	begin
  Palette := GetPalette;
  if Palette <> 0 then
  	begin
    DC := GetDeviceContext(WindowHandle);
    OldPalette := SelectPalette(DC, Palette, not Foreground);
    {if anything changed - repaint the paintBox}
    if (RealizePalette(DC) <> 0) and not Application.terminated then
      begin
    	gardenBitmapPaintBox.invalidate;
      GardenForm.gardenPaintBox.invalidate;
      end;
    SelectPalette(DC, OldPalette, True);
    RealizePalette(DC);
    ReleaseDC(WindowHandle, DC);
  	end;
  Result := inherited PaletteChanged(Foreground);
	end;

procedure TBackdropChooserForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Choose_backdrop')
  end;

procedure TBackdropChooserForm.backgroundColorPanelClick(Sender: TObject);
  begin
  ColorDialog.color := backgroundColorPanel.color;
  if ColorDialog.execute then
	  begin
    backgroundColorPanel.color := ColorDialog.color;
    backgroundColorPanel.invalidate;
    end;
  end;

procedure TBackdropChooserForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  if (ssAlt in shift) and (chr(key) in ['B', 'b']) then
    self.backgroundColorPanelClick(self)
  else if (ssShift in shift) and (ssCtrl in shift) and (lowerCase(chr(key)) = 'e') then
    removeFilePath.visible := true;
  end;

procedure TBackdropChooserForm.removeFilePathClick(Sender: TObject);
  begin
  { the "remove path" button is normally invisible. right before creating a new garden file
  for redistribution, we stripped out the path name of the file so the path name on our computer
  is not saved in the file. }
  truncateNameWhenFinished := true;
  end;

end.

unit Ubrowtdo;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ubrowtdo: Browser component for 3D object (tdo). Displays name of tdo and drawing of it.
Under drawing are text characters user can click to turn left, right, enlarge and reduce.
User can drag tdo inside box to move around. Clicking on name opens tdo chooser window.
Clicking on picture of tdo does NOT open tdo chooser because clicking is used (with drag)
to move the tdo around in the drawing.}

interface

uses ExtCtrls, Classes, StdCtrls, Controls, WinTypes, WinProcs, Graphics, Messages,
  umodel, uaspects, ubrowcom, uturt3d;

const
  kItemFileName = 1;
  kItemTdo = 2;
  kItemTurnLeft = 3;  {<}
  kItemTurnRight = 4; {>}
  kItemEnlarge = 5;   {+}
  kItemReduce = 6;    {-}
  kTdoSize = 64;

type KfObject3DBrowserComponent = class(KfBrowserComponent)
	public
  tdo: KfObject3D;
  fileNameRect: TRect;
  tdoRect: TRect;
  turnLeftRect: TRect;
  turnRightRect: TRect;
  enlargeRect: TRect;
  reduceRect: TRect;
  rotateAngle: integer;
  scale: single;
  position: TPoint;
  editEnabled: boolean;
  movingTdo: boolean;
  moveStart: TPoint;
  procedure initialize; override;
	procedure updateEnabling; override;
  procedure updateModelValues; override;
  procedure updateCurrentValue(aFieldIndex: integer); override;
  procedure updateDisplay; override;
  function minWidth(requestedWidth: integer): integer; override;
  function maxWidth: integer; override;
  function uncollapsedHeight: integer; override;
  procedure resizeElements; override;
  procedure paint; override;
  procedure doMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure doKeyDown(sender: TObject; var key: word; shift: TShiftState); override;
  function maxSelectedItemIndex: integer; override;
  procedure editOrChooseObject3D;
  procedure draw3DObject;
  end;

implementation

uses SysUtils, Dialogs, Forms,
  ugsim, ugscom, umconsts, ueutils, uunits, usupport, udomain, udebug, ubrowser, uestruct,
  utdoch;

procedure KfObject3DBrowserComponent.initialize;
	begin
  tdo := nil;
  rotateAngle := 0;
  scale := 0.1;
  position.x := kTdoSize div 2;
  position.y := kTdoSize;
  movingTdo := false;
  end;

procedure KfObject3DBrowserComponent.updateEnabling;
	begin
  inherited updateEnabling;
  if (model = nil) or (self.readOnly) then
    editEnabled := false
  else
    editEnabled := true;
  end;

procedure KfObject3DBrowserComponent.updateModelValues;
  begin
  { this won't ever be changed by the simulation, so don't respond }
  end;

procedure KfObject3DBrowserComponent.updateCurrentValue(aFieldIndex: integer);
	begin
  if (model <> nil) and (aspect.fieldType = kFieldThreeDObject) then
    model.transferField(kGetField, tdo, aspect.fieldNumber, kFieldThreeDObject, kNotArray,
      aspect.deriveMethod(groupItem.derivedIndex), nil)
  else
    editEnabled := false;
  end;

procedure KfObject3DBrowserComponent.updateDisplay;
	begin
  if (model <> nil) and (aspect.fieldType = kFieldThreeDObject) then
    self.invalidate
  else
    editEnabled := false;
  end;

procedure KfObject3DBrowserComponent.doMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    thePoint: TPoint;
  begin
  { must always call this first because it sets the focus }
  inherited doMouseDown(sender, button, shift, x, y);
  if (editEnabled) then
    begin
    thePoint := Point(x, y);
    if ptInRect(tdoRect, thePoint) then
      begin
      movingTdo := true;
      moveStart := thePoint;
      end;
    end;
  end;

procedure KfObject3DBrowserComponent.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
  var
    thePoint: TPoint;
  begin
  inherited doMouseUp(sender, button, shift, x, y);
  if (editEnabled) then
    begin
    thePoint := Point(x, y);
    if ptInRect(fileNameRect, thePoint) then
      begin
      self.selectedItemIndex := kItemFileName;
      self.editOrChooseObject3D;
      end
    else if ptInRect(tdoRect, thePoint) then
      begin
      self.selectedItemIndex := kItemTdo;
      if movingTdo then
        begin
        movingTdo := false;
        self.position.x := self.position.x + (x - moveStart.x);
        self.position.y := self.position.y + (y - moveStart.y);
        moveStart := point(0,0);
        self.invalidate;
        end;
      end
    else if ptInRect(turnLeftRect, thePoint) then
      begin
      self.selectedItemIndex := kItemTurnLeft;
      rotateAngle := rotateAngle + 10;
      end
    else if ptInRect(turnRightRect, thePoint) then
      begin
      self.selectedItemIndex := kItemTurnRight;
      rotateAngle := rotateAngle - 10;
      end
    else if ptInRect(enlargeRect, thePoint) then
      begin
      self.selectedItemIndex := kItemEnlarge;
      scale := scale + 0.1;
      end
    else if ptInRect(reduceRect, thePoint) then
      begin
      self.selectedItemIndex := kItemReduce;
      scale := scale - 0.1;
      end;
    self.invalidate;
    end;
  end;

procedure KfObject3DBrowserComponent.doKeyDown(sender: TObject; var key: word; shift: TShiftState);
  begin
  inherited doKeyDown(sender, key, shift);
  if (key = VK_RETURN) and (editEnabled) then
    begin
    case self.selectedItemIndex of
      kItemFileName: self.editOrChooseObject3D;
      kItemTdo: self.editOrChooseObject3D;
      end;
    end;
  if (key = VK_DELETE) and (editEnabled) then
    if (self.selectedItemIndex = kItemFileName) or (self.selectedItemIndex = kItemTdo) then
      begin
      tdo := nil;
      GardenForm.doCommand(
        GsChangeDomainObject3DCommand.createCommand(model, tdo, aspect.fieldNumber));
      self.updateDisplay;
      self.invalidate;
      end;
  end;

procedure KfObject3DBrowserComponent.editOrChooseObject3D;
  var
    tdoChooserForm: TThreeDObjectChooserForm;
  begin
  tdoChooserForm := TThreeDObjectChooserForm.create(self);
  if tdoChooserForm = nil then
    raise Exception.create('Could not create 3d object chooser window');
  try
    tdoChooserForm.setSelectedTdo(self.tdo);
    if tdoChooserForm.showModal = mrOK then
      begin
      { when user clicks OK on form, it copies correct pointer to tdo }
      { we just deal with the change by making an undo command }
      GardenForm.doCommand(GsChangeDomainObject3DCommand.createCommand(model, tdo,aspect.fieldNumber));
      self.invalidate;
      end;
  finally
    tdoChooserForm.free;
    tdoChooserForm := nil;
  end;
  end;

function KfObject3DBrowserComponent.maxWidth: integer;
  begin
  result := intMax(kLeftRightGap * 2 + self.labelWidth,
    kLeftRightGap + self.formTextWidth('WWWWWWWW.WWW') + kLeftRightGap + kTdoSize
      + kLeftRightGap);
  end;

function KfObject3DBrowserComponent.minWidth(requestedWidth: integer): integer;
  begin
  result := intMax(kLeftRightGap * 2 + self.longestLabelWordWidth,
    kLeftRightGap + self.formTextWidth('WWWWWWWW.WWW') + kLeftRightGap + kTdoSize
      + kLeftRightGap);
  end;

function KfObject3DBrowserComponent.uncollapsedHeight: integer;
  begin
  result := self.collapsedHeight + kTdoSize + + kBetweenGap + self.textHeight + kTopBottomGap * 2;
  end;

function KfObject3DBrowserComponent.maxSelectedItemIndex: integer;
  begin
  if (not self.collapsed) and (self.editEnabled) then
    result := kItemReduce
  else
    result := kItemLabel;
  end;

procedure KfObject3DBrowserComponent.resizeElements;
  begin
  { do nothing }
  end;

procedure KfObject3DBrowserComponent.paint;
  var
    rect: TRect;
    showText: string;
  begin
  inherited paint;
  if self.collapsed then exit;
  rect := GetClientRect;
  if (tdo <> nil) and (length(tdo.name) > 0) then
    showText := lowerCase(extractFileName(tdo.name))
  else
    showText := '(no tdo selected)';
  with fileNameRect do
    begin
    left := rect.left + kLeftRightGap;
    right := left + self.canvas.textWidth(showText);
    top := self.collapsedHeight + kTopBottomGap;
    bottom := top + self.textHeight;
    end;
  with tdoRect do
    begin
    right := rect.right - kLeftRightGap;
    left := right - kTdoSize;
    top := fileNameRect.top;
    bottom := top + kTdoSize;
    end;
  with turnLeftRect do
    begin
    left := tdoRect.left + (tdoRect.right - tdoRect.left) div 2
      - (self.canvas.textWidth('>') * 4 + kLeftRightGap * 3) div 2;
    right := left + self.canvas.textWidth('<');
    top := tdoRect.bottom + kBetweenGap;
    bottom := top + self.textHeight;
    end;
  with turnRightRect do
    begin
    left := turnLeftRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth('>');
    top := turnLeftRect.top;
    bottom := top + self.textHeight;
    end;
  with enlargeRect do
    begin
    left := turnRightRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth('+');
    top := turnLeftRect.top;
    bottom := top + self.textHeight;
    end;
  with reduceRect do
    begin
    left := enlargeRect.right + kLeftRightGap;
    right := left + self.canvas.textWidth('-');
    top := turnLeftRect.top;
    bottom := top + self.textHeight;
    end;
  with Canvas do
    begin
    self.drawText(showText, fileNameRect, true, (self.selectedItemIndex = kItemFileName) and (self.editEnabled));
    self.drawText('<', turnLeftRect, true, self.selectedItemIndex = kItemTurnLeft);
    self.drawText('>', turnRightRect, true, self.selectedItemIndex = kItemTurnRight);
    self.drawText('+', enlargeRect, true, self.selectedItemIndex = kItemEnlarge);
    self.drawText('-', reduceRect, true, self.selectedItemIndex = kItemReduce);
    self.draw3DObject;
    if (self.selectedItemIndex <> kItemTdo) then
      pen.color := clBtnShadow
    else
      pen.color := clBtnText;
    brush.style := bsClear;
    pen.width := 1;
    rectangle(tdoRect.left - 1, tdoRect.top - 1, tdoRect.right + 1, tdoRect.bottom + 1);
    end;
end;

procedure KfObject3DBrowserComponent.draw3DObject;
  var
    turtle: KfTurtle;
    bitmap: TBitmap;
  begin
  if model = nil then exit;
  if tdo = nil then exit;
  { set up clipping bitmap }
  bitmap := TBitmap.create;
  bitmap.width := kTdoSize;
  bitmap.height := kTdoSize;
  bitmap.canvas.brush.color := clWhite;
  bitmap.canvas.rectangle(0, 0, bitmap.width, bitmap.height);
  { set up turtle }
  turtle := KfTurtle.defaultStartUsing;
  try
  turtle.drawingSurface.pane := bitmap.canvas;
  with turtle.drawOptions do
    begin
    sortPolygons := true;
    drawLines := true;
    wireFrame := false;
    end;
  turtle.reset; { must be after pane and draw options set }
  turtle.mmPerPixel(1);  
  turtle.drawingSurface.foreColor := clGreen;
  turtle.drawingSurface.backColor := clLime;
  turtle.drawingSurface.lineColor := clBlack;
  turtle.xyz(position.x, position.y, 0);
  try
	  turtle.push;
	  turtle.rotateY(rotateAngle);
    tdo.drawWithTurtleScale(turtle, scale);
	  turtle.pop;
    except
	    on EMathError do
        begin
        ErrorMessage('KfObject3DBrowserComponent.draw3DObject: EMathError');
        turtle.free;
        bitmap.free;
        end;
    end;
  { copy from bitmap to self }
  self.canvas.draw(tdoRect.left, tdoRect.top, bitmap);
  bitmap.free;
  finally
  KfTurtle.defaultStopUsing;
  turtle := nil;
  end;
  end;

end.
 
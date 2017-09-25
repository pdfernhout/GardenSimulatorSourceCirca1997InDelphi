unit Utdoed;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utdoed: 3D object (tdo) editor. This window was made at the last minute and is
not very easy to use. It just acts as an intepreter for the tdo specification.
Probably making a new tdo would be very difficult in this window (the original
tdos were made in a separate tdo editor written in Visual Basic some time ago).
Eventually one would want to add the ability to move the points in the tdo
by clicking on them and dragging them, and to make new points graphically.
The way it is now, it is no different from exporting a tdo to a file, editing
the file in a text editor, then importing the file again. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, uturt3d, ugsform, Spin;

type
  TtdoEditorForm = class(GsForm)
    tdoImage: TImage;
    tdoEditMemo: TMemo;
    nameLabel: TLabel;
    OK: TButton;
    Cancel: TButton;
    help: TBitBtn;
    rename: TButton;
    reset: TButton;
    controlPanel: TPanel;
    Label1: TLabel;
    xRotationEdit: TSpinEdit;
    Label2: TLabel;
    yRotationEdit: TSpinEdit;
    Label3: TLabel;
    zRotationEdit: TSpinEdit;
    scaleEdit: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    center: TButton;
    readSpec: TBitBtn;
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure tdoEditMemoChange(Sender: TObject);
    procedure centerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure xRotationEditChange(Sender: TObject);
    procedure yRotationEditChange(Sender: TObject);
    procedure zRotationEditChange(Sender: TObject);
    procedure tdoImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tdoImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scaleEditChange(Sender: TObject);
    procedure renameClick(Sender: TObject);
    procedure resetClick(Sender: TObject);
    procedure readSpecClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    startPoint, dragFrom: TPoint;
    workingTdo, originalTdo: KfObject3d;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure initialize(aTdo: KfObject3d);
    procedure draw3DObject;
    procedure dumpCurrent3DObjectToMemo;
    procedure updateCurrent3DObjectFromMemo;
  end;

implementation

{$R *.DFM}

uses ueutils;

var initializing: boolean;
const kDefaultScale = 25;

constructor TtdoEditorForm.Create(AOwner: TComponent);
  begin
  inherited create(AOwner);
  workingTdo := KfObject3d.create;
  initializing := false;
  end;

destructor TtdoEditorForm.destroy;
  begin
  workingTdo.free;
  workingTdo := nil;
  inherited destroy;
  end;

procedure TtdoEditorForm.initialize(aTdo: KfObject3d);
  begin
  initializing := true;
  originalTdo := aTdo;
  if originalTdo = nil then
    raise Exception.create('TtdoEditorForm.initialize: nil tdo');
  workingTdo.copyFrom(originalTdo);
  self.dumpCurrent3DObjectToMemo;
  self.draw3DObject;
  nameLabel.caption := '3D object:  ' + workingTdo.getName;
  startPoint := Point(tdoImage.width div 2, tdoImage.height);
  initializing := false;
  end;

procedure TtdoEditorForm.OKClick(Sender: TObject);
  begin
  if originalTdo <> nil then
    originalTdo.copyFrom(workingTdo);
  modalResult := mrOk;
  end;

procedure TtdoEditorForm.CancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure TtdoEditorForm.dumpCurrent3DObjectToMemo;
  begin
  if workingTdo = nil then exit;
  workingTdo.toTStrings(tdoEditMemo.lines);
  end;

procedure TtdoEditorForm.updateCurrent3DObjectFromMemo;
  begin
  if workingTdo = nil then exit;
  workingTdo.fromTStrings(tdoEditMemo.lines);
  self.draw3DObject;
  end;

procedure TtdoEditorForm.draw3DObject;
  var
    turtle: KfTurtle;
    scale: single;
  begin
  if workingTdo = nil then exit;
  tdoImage.canvas.brush.color := clWhite;
  tdoImage.canvas.rectangle(0, 0, tdoImage.width, tdoImage.height);
  { set up turtle }
  turtle := KfTurtle.defaultStartUsing;
  try
    turtle.drawingSurface.pane := tdoImage.canvas;
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
    try
      turtle.xyz(startPoint.x, startPoint.y, 0);
      if xRotationEdit.value > 0 then
        turtle.rotateX(xRotationEdit.value * 1.0);
      if yRotationEdit.value > 0 then
        turtle.rotateY(yRotationEdit.value * 1.0);
      if zRotationEdit.value > 0 then
        turtle.rotateZ(zRotationEdit.value * 1.0);
      scale := scaleEdit.value * 2.0 {to make 50 same as 100} / 50.0 {to make 100 into 2.0};
	    turtle.push;
      workingTdo.drawWithTurtleScale(turtle, scale);
	    turtle.pop;
    except
      on EMathError do ErrorMessage('TtdoEditorForm.draw3DObject: Turtle EMathError');
    end;
  finally
    KfTurtle.defaultStopUsing;
    turtle := nil;
  end;
  tdoImage.invalidate;
  end;

procedure TtdoEditorForm.tdoEditMemoChange(Sender: TObject);
  begin
  if not initializing then
    readSpec.caption := 'Read (changed)';
  end;

procedure TtdoEditorForm.xRotationEditChange(Sender: TObject);
  begin
  self.draw3DObject;
  end;

procedure TtdoEditorForm.yRotationEditChange(Sender: TObject);
  begin
  self.draw3DObject;
  end;

procedure TtdoEditorForm.zRotationEditChange(Sender: TObject);
  begin
  self.draw3DObject;
  end;

procedure TtdoEditorForm.centerClick(Sender: TObject);
  begin
  scaleEdit.value := kDefaultScale;
  startPoint := Point(tdoImage.width div 2, tdoImage.height);
  xRotationEdit.value := 0;
  yRotationEdit.value := 0;
  zRotationEdit.value := 0;
  self.draw3DObject;
  end;

const kBetweenGap = 4;

procedure TtdoEditorForm.FormResize(Sender: TObject);
  begin
  with nameLabel do setBounds(kBetweenGap, kBetweenGap, width, height);
  with tdoEditMemo do setBounds(kBetweenGap, nameLabel.top + nameLabel.height + kBetweenGap, width,
      self.clientHeight - nameLabel.height - readSpec.height - kBetweenGap * 4);
  with readSpec do setBounds(kBetweenGap, tdoEditMemo.top + tdoEditMemo.height + kBetweenGap,
      tdoEditMemo.width, height);
  with controlPanel do setBounds(tdoImage.left, self.clientHeight - height - kBetweenGap,
      width, height);
  OK.left := self.clientWidth - OK.width - kBetweenGap;
  cancel.left := OK.left;
  rename.left := OK.left;
  reset.left := OK.left;
  help.left := OK.left;
  try
  with tdoImage do setBounds(tdoEditMemo.width + kBetweenGap * 2, nameLabel.top + nameLabel.height + kBetweenGap,
      self.clientWidth - tdoEditMemo.width - OK.width - kBetweenGap * 4,
      self.clientHeight - controlPanel.height - nameLabel.height - kBetweenGap * 4);
  with tdoImage.picture.bitmap do
    begin
    height := tdoImage.height;
    width := tdoImage.width;
    end;
  except
    messageDlg('Not enough memory to resize the 3D object bitmap', mtError, [mbOK], 0);
  end;
  self.draw3DObject;
  end;

procedure TtdoEditorForm.tdoImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  dragFrom.x := x;
  dragFrom.y := y;
  end;

procedure TtdoEditorForm.tdoImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  startPoint.x := startPoint.x + x - dragFrom.x;
  startPoint.y := startPoint.y + y - dragFrom.y;
  self.draw3DObject;
  end;

procedure TtdoEditorForm.scaleEditChange(Sender: TObject);
  begin
  self.draw3DObject;
  end;

procedure TtdoEditorForm.renameClick(Sender: TObject);
  var
{$IFDEF WINDOWS}
    newName: string;
{$ELSE}
    newName: ansistring;
{$ENDIF}
  begin
  if workingTdo = nil then exit;
  newName := workingTdo.getName;
  if not inputQuery('Enter new name', 'Enter a new name for this 3D object.', newName) then exit;
  workingTdo.setName(newName);
  nameLabel.caption := '3D object:  ' + workingTdo.getName;
  end;

procedure TtdoEditorForm.resetClick(Sender: TObject);
  begin
  initializing := true;
  if originalTdo = nil then
    raise Exception.create('TtdoEditorForm.initialize: nil tdo');
  workingTdo.copyFrom(originalTdo);
  self.dumpCurrent3DObjectToMemo;
  self.draw3DObject;
  nameLabel.caption := '3D object:  ' + workingTdo.getName;
  initializing := false;
  self.centerClick(self);
  end;

procedure TtdoEditorForm.readSpecClick(Sender: TObject);
  begin
  self.updateCurrent3DObjectFromMemo;
  readSpec.caption := 'Read';
  end;

end.

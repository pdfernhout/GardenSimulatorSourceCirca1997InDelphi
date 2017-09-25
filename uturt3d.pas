unit Uturt3d;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uturt3D: Defines several objects important in 3D plant drawing.
* KfMatrix: A matrix used to transform 3D points to produce new 3D points
  based on current turtle rotations along X, Y, and Z axes.
* KfDrawingSurface:A device-independent representation of a canvas on which to draw lines and triangles.
* KfIndexTriangle: A representation of a triangle as indexes into an array of points.
* KfObject3D: An object to be drawn in 3D - represented as an array of triangles which refer to 3D points.
* KfTurtle: A turtle object that can be moved in 3D space and has an X, Y,and Z rotation for drawing lines.}

interface
uses Graphics, Dialogs, WinTypes, SysUtils, Classes, ucollect, usstream, ufiler, ufilertx;

{if this is defined - turtle will draw axis marks for each line segment}
{put spaces before and after the $ to disable}
{ $ DEFINE DEBUG_TURTLE}

const
kDirectionClockwise = 1;
kDirectionCounterClockwise = -1;
kDirectionUnknown = 0;

kInitialTurtleMatrixStackDepth = 250;
kInitialDrawingSurfaceTriangles = 1000;
kMaximumRecordedPoints = 200;

function PointInQuadrangle(const point: TPoint; const quadrangle: array of TPoint): boolean;
function Clockwise (const point0: TPoint; const point1: TPoint; const point2: TPoint): integer;

type
	KfPoint3D = record
    x: single;
    y: single;
    z: single;
    end;

  PlantDrawOptionsStructure = record
    { general options }
    sortPolygons: boolean;
    drawLines: boolean;
    { speed optimizations }
    wireFrame: boolean;
    straightLinesOnly: boolean;
    simpleLeavesOnly: boolean;
    { extra, for adding more speed optimizations }
    unassigned1: boolean;
    unassigned2: boolean;
    unassigned3: boolean;
    unassignedLongint: longint;
    end;

procedure KfPoint3D_setXYZ(var thePoint: KfPoint3D; aX: single; aY: single; aZ: single);
procedure KfPoint3D_addXYZ(var thePoint: KfPoint3D; xOffset: single; yOffset: single; zOffset: single);
procedure KfPoint3D_scaleBy(var thePoint: KfPoint3D; aScale: single);
procedure KfPoint3D_subtract(var thePoint: KfPoint3D; const aPoint: KfPoint3D);

const
	kFastTrigArraySize = 256; {if you change this - you need to change angleX etc. functions}

procedure FastTrigInitialize;
{these functions are no longer called here but are available for other uses}
{they were bundled directly into rotate}
function FastTrigCos(angle: single): single;
function FastTrigSin(angle: single): single;

type
	KfMatrix = class(TObject)
  	public
    a0, a1, a2, b0, b1, b2, c0, c1, c2: single;
    position: KfPoint3D;
	  {class SinCache CosCache}
		function angleX: byte;
		function angleY: byte;
		function angleZ: byte;
		procedure initializeAsUnitMatrix;
		procedure move(distance: single);
		procedure rotateX(angle: single);
		procedure rotateY(angle: single);
		procedure rotateZ(angle: single);
		procedure transform(var aPoint3D: KfPoint3D);
    function deepCopy: KfMatrix;
    procedure copyTo(otherMatrix: KfMatrix);
  	end;

  KfDrawingSurface = class;

	KfTriangle = class(TObject) {these can only be triangular}
  	public
    foreColor: TColor;
    backColor: TColor;
    lineColor: TColor;
    zForSorting: single;
    backFacing: boolean;
    lineWidth: single;
    isLine: boolean;
    points: array[0..2] of KfPoint3D;
    constructor create;
		{constructor createFromPoints(thePoints: TListCollection); }
    destructor destroy; override;
    function visibleSurfaceColor: TColor;
    function invisibleSurfaceColor: TColor;
		procedure computeBackFacing;
		procedure computeZ;
    procedure updateGeometry;
		procedure initializeDetails(drawingSurface: KfDrawingSurface);
    end;

	KfDrawingSurface = class(TObject)
    public
    pane: TCanvas;
    drawingLines: boolean;
    fillingTriangles: boolean;
    triangles: TListCollection;
    numTrianglesUsed: longint;
    recording: boolean;
    foreColor: TColor;
    backColor: TColor;
    lineColor: TColor;
    lineWidth: single;
    constructor create;
    destructor destroy; override;
    procedure clearTriangles;
		function allocateTriangle: KfTriangle;
		procedure sortTriangles(left: integer; right: integer);
		procedure trianglesSort;
		procedure recordingStart;
		procedure recordingStop;
		procedure trianglesDraw;
 		procedure recordingDraw;
		procedure drawLineFromTo(const startPoint: KfPoint3D; const endPoint: KfPoint3D);                           
		procedure basicDrawLineFromTo(const startPoint: KfPoint3D; const endPoint: KfPoint3D);
		{procedure drawTriangleFromPoints(points: TListCollection); }
		procedure drawLastTriangle;
		procedure basicDrawTriangle(triangle: KfTriangle);
		procedure initialize;
  	end;

	KfIndexTriangle = class(GsStreamableObject)
    public
    points: array[0..2] of longint;
    constructor createABC(a: longint; b: longint; c: longint);
  	procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
 	 	procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    end;

  KfTurtle = class;

  {PDF FIX - should allocate array according to need - or do some bounds check...}
	KfObject3D = class(GsStreamableObject)
  	public
		points: array[0..kMaximumRecordedPoints] of KfPoint3d;
    pointsInUse: longint;
    triangles: TListCollection;
    name: string;
    originalIfCopy: KfObject3D;
    inUse: boolean;
    constructor create; override;
    destructor destroy; override;
		procedure addPoint(const aPoint: KfPoint3d);
		function pointForIndex(anIndex: longint): KfPoint3D;
		procedure addTriangle(aTriangle: KfIndexTriangle);
		function triangleForIndex(anIndex: longint): KfIndexTriangle;
		procedure adjustForOrigin;
		procedure drawWithTurtleScale(turtle: KfTurtle; scale: single);
		procedure addTriangleWithVerticesABC(a: longint; b: longint; c: longint);
		procedure addPointString(stream: KfStringStream);
		procedure addTriangleString(stream: KfStringStream);
		procedure fromFileStream(var inputFile: TextFile);
		class function fromFile(aFileName: string): KfObject3D;
    procedure saveToFile(aFileName: string);
  	procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  	procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
    procedure copyFrom(original: KfObject3D);
    function isSameAs(other: KfObject3D): boolean;
    function getName: string; override;
    procedure setName(newName: string); override;
    procedure streamUsingTextFiler(textFiler: GsTextFiler); override;
    procedure fromTStrings(stringList: TStrings);
    procedure toTStrings(stringList: TStrings);
  	end;

  TRealRect = record
    top, left, bottom, right: single;
    end;

  {PDF FIX - may want to think about size of recorded points}
	KfTurtle = class(TObject)
  	public
    matrixStack: TListCollection;
    numMatrixesUsed: longint;
    currentMatrix: KfMatrix;
    recordedPoints: array[0..kMaximumRecordedPoints] of KfPoint3d;
    recordedPointsInUse: longint;
    scale: single;
    drawingSurface: KfDrawingSurface;
    tempPosition: KfPoint3d;
    realBoundsRect: TRealRect;
    userWantsToRecordPosition: boolean;
    drawOptions: PlantDrawOptionsStructure;
    class function defaultStartUsing: KfTurtle;
    class procedure defaultStopUsing;
    class procedure defaultAllocate;
    class procedure defaultFree;
    function boundsRect: TRect;
    procedure resetBoundsRect(basePoint: TPoint);
    procedure addToBoundsRect(const aPoint: KfPoint3d);
		function angleX: byte;
		function angleY: byte;
		function angleZ: byte;
		procedure clearRecording;
		procedure recordPosition;
		procedure recordPositionPoint(const aPoint: KfPoint3D);
		function scaleMillimeters(mm:single): single;
		procedure mmPerPixel(mmPerPixel: single);
		procedure moveAndRecordScale(const originalPoint3D: KfPoint3D; theScale: single);
		procedure moveInMillimeters(mm: single);
		procedure moveInMillimetersAndRecord(mm: single);
		procedure moveInPixels(pixels: single);
		procedure moveInPixelsAndRecord(pixels: single);
		function positionCopy: KfPoint3D;
		procedure pop;
		procedure push;
		procedure rotateX(angle: single);
		procedure rotateY(angle: single);
		procedure rotateZ(angle: single);
		function stackSize: longint;
		procedure startRecording;
		procedure transformAndRecord(const originalPoint3D: KfPoint3D; theScale: single);
		function newTriangleTriangle(aPointIndex: longint; bPointIndex: longint; cPointIndex: longint): KfTriangle;
		procedure xyz(x: single; y: single; z: single);
		constructor create;
		destructor destroy; override;
		procedure drawInMillimeters(mm: single);
		procedure drawInPixels(pixels: single);
		procedure drawTriangle;
		procedure setForeColorBackColor(frontColor: TColor; backColor: TColor);
	 {	procedure drawTriangleFromPoints(aPoint: KfPoint3D; bPoint: KfPoint3D; cPoint: KfPoint3D); }
		procedure drawTriangleFromIndexes(aPointIndex: longint; bPointIndex: longint; cPointIndex: longint);
		function lineColor: TColor;
		procedure setLineColor(aColor: TColor);
		function lineWidth: single;
		procedure setLineWidth(aWidth: single);
    procedure reset;
    function position: KfPoint3d;
  	end;

var
	SinCache: array [0..kFastTrigArraySize-1] of single;
  CosCache: array [0..kFastTrigArraySize-1] of single;

implementation

uses uclasses;

{ ------------------------------------------------------------------- FastTrig }
{Approach: Sin and cosine arrays are used for speed.
	Angles are 0 - 255 instead of 0 to 359}
{PDF - maybe want to expand this to 0 - 1023? - would impact rotate callers}
procedure FastTrigInitialize;
	var i: integer;
	begin
	for i := 0 to kFastTrigArraySize - 1 do
  	begin
    SinCache[i] := sin(i * 2 * 3.141592654 / kFastTrigArraySize);
    CosCache[i] := cos(i * 2 * 3.141592654 / kFastTrigArraySize);
    end;
	end;

{moved bounding angle into cache range into functions to decrease function overhead}
{copied these functions into rotate to decrease overhead there - both of
function call and of bounding angle twice}
function FastTrigCos(angle: single): single;
  var boundedAngle: integer;
  begin
  try
    boundedAngle := round(angle) mod kFastTrigArraySize;
  except
    boundedAngle := 0;
  end;
  if (boundedAngle  < 0) then
  	boundedAngle := kFastTrigArraySize + boundedAngle;
  result := CosCache[boundedAngle];
  end;

function FastTrigSin(angle: single): single;
  var boundedAngle: integer;
  begin
  try
    boundedAngle := round(angle) mod kFastTrigArraySize;
  except
    boundedAngle := 0;
  end;
  if (boundedAngle  < 0) then
  	boundedAngle := kFastTrigArraySize + boundedAngle;
  result := SinCache[boundedAngle];
  end;


{ ---------------------------------------------------------------------------------- KfPoint3D }
procedure KfPoint3D_setXYZ(var thePoint: KfPoint3D; aX: single; aY: single; aZ: single);
	begin
  with thePoint do
    begin
		x := aX;
  	y := aY;
  	z := aZ;
    end;
  end;

procedure KfPoint3D_addXYZ(var thePoint: KfPoint3D; xOffset: single; yOffset: single; zOffset: single);
	begin
  {pdf - shift point by x y and z.}
  with thePoint do
    begin
  	x := x + xOffset;
  	y := y + yOffset;
  	z := z + zOffset;
    end;
  end;

procedure KfPoint3D_scaleBy(var thePoint: KfPoint3D; aScale: single);
  begin
  {pdf - multiply point by scale.}
  with thePoint do
    begin
  	x := x * aScale;
  	y := y * aScale;
  	z := z * aScale;
    end;
  end;

procedure KfPoint3D_subtract(var thePoint: KfPoint3D; const aPoint: KfPoint3D);
  begin
    {pdf - subtract point from this point.}
  with thePoint do
    begin
    x := x - aPoint.x;
    y := y - aPoint.y;
    z := z - aPoint.z;
    end;
  end;

{ ---------------------------------------------------------------------------------- KfMatrix }
{PDF FIX - potential bug}
{ these do not take in account kFastTrigArraySize could be different from 256}
function KfMatrix.angleX: byte;
	var temp: single;
  begin
    try 
    result := 0;
    temp := 0.0;

    temp := (a2 * a2) + (c2 * c2);
    if (temp < 0.0) then temp := 0.0;

    temp := sqrt(temp);
    if (temp = 0.0)then
    	begin
      if (b2 < 0) then result := 64
      else result := 256-64;
			end
    else
      begin
      temp := b2 / temp;
      temp := arcTan(temp);
      result := byte(round(-temp * 256 / (2 * 3.1415926)));
      end;
    except
    result := 0;
    end;
	end;

function KfMatrix.angleY: byte;
	var temp: single;
  begin
    try
    result := 0;
    temp := 0.0;

    temp := (a0 * a0) + (c0 * c0);
    if (temp < 0.0) then temp := 0.0;

    temp := sqrt(temp);
    if (temp = 0.0)then
    	begin
      if (b0 < 0) then result := 64
      else result := 256-64;
			end
    else
      begin
      temp := b0 / temp;
      temp := arcTan(temp);
      result := byte(round(-temp * 256 / (2 * 3.1415926)));
      end;
    except
    result := 0;
    end;
	end;

function KfMatrix.angleZ: byte;
	var temp: single;
  begin
    try
    result := 0;
    temp := 0.0;

    temp := (a1 * a1) + (c1 * c1);
    if (temp < 0.0) then temp := 0.0;

    temp := sqrt(temp);
    if (temp = 0.0)then
    	begin
      if (b1 < 0) then result := 64
      else result := 256-64;
			end
    else
      begin
      temp := b1 / temp;
      temp := arcTan(temp);
      result := byte(round(-temp * 256 / (2 * 3.1415926)));
      end;
    except
    result := 0;
    end;
	end;

procedure KfMatrix.initializeAsUnitMatrix;
	begin
	a0 := 1.0;
	a1 := 0;
	a2 := 0;
	b0 := 0;
	b1 := 1.0;
	b2 := 0;
	c0 := 0;
	c1 := 0;
	c2 := 1.0;
	position.x := 0;
	position.y := 0;
	position.z := 0;
	end;

procedure KfMatrix.move(distance: single);
  begin
  {pdf - move a distance by multiplying matrix values
   movement is along x axis (d, 0, 0, 1);}
  position.x := position.x + distance * a0;
  position.y := position.y + distance * b0;
  position.z := position.z + distance * c0;
  end;

procedure KfMatrix.rotateX(angle: single);
  var
  cosAngle, sinAngle, temp1: single;
  boundedAngleIndex: integer;
  begin
  {bound angle and convert to index}
	{not doing try except around round for speed here - could fail ...}
  boundedAngleIndex := round(angle) mod kFastTrigArraySize;
  if (boundedAngleIndex  < 0) then
  	boundedAngleIndex := kFastTrigArraySize + boundedAngleIndex;
	cosAngle := CosCache[boundedAngleIndex];
  sinAngle := SinCache[boundedAngleIndex];

  {moved minuses to middle to optimize}
  a0 := a0;
  temp1 := (a1 * cosAngle) - (a2 * sinAngle);
  a2 := (a1 * sinAngle) + (a2 * cosAngle);
  a1 := temp1;

  b0 := b0;
  temp1 := (b1 * cosAngle) - (b2 * sinAngle);
  b2 := (b1 * sinAngle) + (b2 * cosAngle);
  b1 := temp1;

  c0 := c0;
  temp1 := (c1 * cosAngle) - (c2 * sinAngle) ;
  c2 := (c1 * sinAngle) + (c2 * cosAngle);
  c1 := temp1;
	end;

procedure KfMatrix.rotateY(angle: single);
  var
  cosAngle, sinAngle, temp0: single;
  boundedAngleIndex: integer;
  begin
  {bound angle and convert to index}
	{not doing try except around round for speed here - could fail ...}
  boundedAngleIndex := round(angle) mod kFastTrigArraySize;
  if (boundedAngleIndex  < 0) then
  	boundedAngleIndex := kFastTrigArraySize + boundedAngleIndex;
	cosAngle := CosCache[boundedAngleIndex];
  sinAngle := SinCache[boundedAngleIndex];

  temp0 := (a0 * cosAngle) + (a2 * sinAngle);
  a1 := a1;
  a2 := (a2 * cosAngle) - (a0 * sinAngle); {flipped to put minus in middle}
  a0 := temp0;

  temp0 := (b0 * cosAngle) + (b2 * sinAngle);
  b1 := b1;
  b2 := (b2 * cosAngle) - (b0 * sinAngle);  {flipped to put minus in middle}
  b0 := temp0;

  temp0 := (c0 * cosAngle) + (c2 * sinAngle);
  c1 := c1;
  c2 := (c2 * cosAngle) - (c0 * sinAngle); {flipped to put minus in middle}
  c0 := temp0;
  end;

procedure KfMatrix.rotateZ(angle: single);
  var
  cosAngle, sinAngle, temp0: single;
  boundedAngleIndex: integer;
  begin
  {bound angle and convert to index}
	{not doing try except around round for speed here - could fail ...}
  boundedAngleIndex := round(angle) mod kFastTrigArraySize;
  if (boundedAngleIndex  < 0) then
  	boundedAngleIndex := kFastTrigArraySize + boundedAngleIndex;
	cosAngle := CosCache[boundedAngleIndex];
  sinAngle := SinCache[boundedAngleIndex];

  {minuses moved to middle to optimize}
  temp0  :=(a0 * cosAngle) - (a1 * sinAngle);
  a1 := (a0 * sinAngle) + (a1 * cosAngle);
  a2 := a2;
  a0 := temp0;

  temp0 := (b0 * cosAngle) - (b1 * sinAngle);
  b1 := (b0 * sinAngle) + (b1 * cosAngle);
  b2 := b2;
  b0 := temp0;

  temp0 := (c0 * cosAngle) - (c1 * sinAngle);
  c1 :=  (c0 * sinAngle) + (c1 * cosAngle);
  c2 := c2;
  c0 := temp0;
  end;

{pdf - transform the point, including offsetting it by the current position}
{Alters the point's contents}
procedure KfMatrix.transform(var aPoint3D: KfPoint3D);
  var
  	x, y, z : single;
	begin
    x := aPoint3D.x;
    y := aPoint3D.y;
    z := aPoint3D.z;
    aPoint3D.x := (x * a0) + (y * a1) + (z * a2) + position.x;
    aPoint3D.y := (x * b0) + (y * b1) + (z * b2) + position.y;
    aPoint3D.z := (x * c0) + (y * c1) + (z * c2) + position.z;
  end;

function KfMatrix.deepCopy: KfMatrix;
	begin
  result := nil;
  result := KfMatrix.create;
  result.position := position;
  result.a0 := a0;
  result.a1 := a1;
  result.a2 := a2;
  result.b0 := b0;
  result.b1 := b1;
  result.b2 := b2;
  result.c0 := c0;
  result.c1 := c1;
  result.c2 := c2;
  end;

procedure KfMatrix.copyTo(otherMatrix: KfMatrix);
	begin
  otherMatrix.position := position;
  otherMatrix.a0 := a0;
  otherMatrix.a1 := a1;
  otherMatrix.a2 := a2;
  otherMatrix.b0 := b0;
  otherMatrix.b1 := b1;
  otherMatrix.b2 := b2;
  otherMatrix.c0 := c0;
  otherMatrix.c1 := c1;
  otherMatrix.c2 := c2;
  end;

{ ---------------------------------------------------------------------------------- KfTriangle }

constructor KfTriangle.create;
	begin
  inherited create;
  end;

{  for streaming
filer.streamColor(foreColor);
  filer.streamColor(backColor);
  filer.streamColor(lineColor);
  filer.streamSingle(zForSorting);
  filer.streamBoolean(backFacing);
  filer.streamSingle(lineWidth);
  filer.streamBoolean(isLine);
  filer.streamBytes(points, sizeof(points));
}
(*
constructor KfTriangle.createFromPoints(thePoints: TListCollection);
	begin
  self.create;
  {points := thePoints; PDF FIX LATER - BUG BUG BUG}
  self.updateGeometry;
  end;
*)

destructor KfTriangle.destroy;
	begin
  inherited destroy;
  end;

procedure KfTriangle.computeBackFacing;
  var
    point0, point1, point2: KfPoint3d;
    backfacingResult: single;
  begin
  {determine if back facing or not and set flag}

  self.backFacing := false;

  if self.isLine then exit;

  point0 := self.points[0];
  point1 := self.points[1];
  point2 := self.points[2];

  backfacingResult := ((point1.x - point0.x) * (point2.y - point0.y)) -
  		((point1.y - point0.y) * (point2.x - point0.x));

  self.backFacing := (backfacingResult < 0);
  end;

{compute some representative z value}
procedure KfTriangle.computeZ;
  var
  	minZ: single;
    aPoint: KfPoint3d;
	begin
  aPoint := points[0];
  minZ := aPoint.z;
  aPoint := points[1];
  if aPoint.z < minZ then minZ := aPoint.z;
  if not self.isLine then
    begin
  	aPoint := points[2];
 	 	if aPoint.z < minZ then minZ := aPoint.z;
    end;
  self.zForSorting := minZ;
  end;

procedure KfTriangle.updateGeometry;
	begin
  self.computeBackFacing;
  self.computeZ;
  end;

procedure KfTriangle.initializeDetails(drawingSurface: KfDrawingSurface);
	begin
  {load up colors, line width}
  backColor := drawingSurface.backColor;
  foreColor := drawingSurface.foreColor;
  lineColor := drawingSurface.lineColor;
  lineWidth := drawingSurface.lineWidth; {PDF FIX - maybe should scale this?}
  end;

function KfTriangle.visibleSurfaceColor: TColor;
	begin
  {return the visible color dependening on backFacing}
  if backFacing then
  	result := backColor
  else
    result := foreColor;
  end;

function KfTriangle.invisibleSurfaceColor: TColor;
	begin
  {return the invisible color dependening on backFacing}
  if backFacing then
  	result := foreColor
  else
    result := backColor;
  end;

{ ---------------------------------------------------------------------------------- KfDrawingSurface }
constructor KfDrawingSurface.create;
  var i: longint;
	begin
  inherited create;
  triangles := TListCollection.create;
  numTrianglesUsed := 0;
  fillingTriangles := true;
  drawingLines := false;
  self.initialize;
  { start with a bunch of triangles }
  for i := 0 to kInitialDrawingSurfaceTriangles do
    triangles.add(KfTriangle.create);
  end;

destructor KfDrawingSurface.destroy;
	begin
  triangles.free;
  inherited destroy;
  end;

procedure KfDrawingSurface.clearTriangles;
  begin
  numTrianglesUsed := 0;
  end;

function KfDrawingSurface.allocateTriangle: KfTriangle;
  begin
  result := nil;
  if numTrianglesUsed < triangles.count then
    result := triangles.items[numTrianglesUsed]
  else
    begin
{$IFDEF WIN32 }
    if true then  
{$ELSE}
    if triangles.count > 8000 then
{$ENDIF}
      begin
      { don't allocate any more triangles if in danger of overflowing stack partition }
      { in 16-bit, raises integer overflow if number * 4 is > 32767 - may want to find work around for this later }
      dec(numTrianglesUsed);
      result := triangles.items[numTrianglesUsed];
      end
    else
      begin
      result := KfTriangle.create;
      triangles.add(result);
      end;
    end;
  inc(numTrianglesUsed);
  end;

procedure KfDrawingSurface.sortTriangles(left: integer; right: integer);
  var
  i: integer;
  j: integer;
  z: single;
  begin
  if right > left then
  	begin
    z := KfTriangle(triangles.items[right]).zForSorting;
    i := left - 1;
    j := right;

    while True do
      begin
      repeat
        i := i + 1;
      until not (KfTriangle(triangles.items[i]).zForSorting < z);
      repeat
        j := j - 1;
      until (j < i) or (not (KfTriangle(triangles.items[j]).zForSorting > z));
      if i >= j then break;
      triangles.exchange(i,j);
      end;
    triangles.exchange(i, right);
    SortTriangles(left, j);
    SortTriangles(i + 1, right);
    end;
	end;

procedure KfDrawingSurface.trianglesSort;
	begin
  if numTrianglesUsed > 1 then
  	self.sortTriangles(0, numTrianglesUsed - 1);
  end;

procedure KfDrawingSurface.recordingStart;
	begin
  recording := true;
  end;

procedure KfDrawingSurface.recordingStop;
	begin
  recording := false;
  end;

procedure KfDrawingSurface.trianglesDraw;
	var
  i: integer;
	begin
  if numTrianglesUsed > 0 then
  	for i := 0 to numTrianglesUsed - 1 do
  		begin
    	self.basicDrawTriangle(triangles.items[i]);
    	end;
  end;

procedure KfDrawingSurface.recordingDraw;
	begin
  self.trianglesSort;
  self.trianglesDraw;
  end;

{pdf - draw a line from start to end.}
{distance used to be passed to avoid calculating it in Zbuffer system}
procedure KfDrawingSurface.drawLineFromTo(const startPoint: KfPoint3D; const endPoint: KfPoint3D);
  var
  triangle: KfTriangle;
  begin
  if self.recording then
  	begin
    {store triangle}
    triangle := self.allocateTriangle;
    triangle.points[0] := startPoint;
    triangle.points[1] := endPoint;
    triangle.isLine := true;
    {need to handle drawing surface colors}
    triangle.foreColor := foreColor;
    triangle.backColor := backColor;
    triangle.lineColor := lineColor;
    triangle.lineWidth := lineWidth; 
    end
  else
    begin
    self.basicDrawLineFromTo(startPoint, endPoint);
    end;

  end;

procedure KfDrawingSurface.basicDrawLineFromTo(const startPoint: KfPoint3D; const endPoint: KfPoint3D);
  begin
  {pdf - draw a line from start to end.}
  {assuming pen color and width is saved and restored elsewhere}
  try
  self.pane.pen.width := round(self.lineWidth);
  self.pane.pen.color := self.lineColor;
  self.pane.pen.style := psSolid;
  self.pane.moveTo(round(startPoint.x) mod 32767, round(startPoint.y) mod 32767);
  self.pane.lineTo(round(endPoint.x) mod 32767, round(endPoint.y) mod 32767);
  except
  {do nothing if rounding fails}
  end;
  end;

(*
{drawing surface takes over ownership of points}
procedure KfDrawingSurface.drawTriangleFromPoints(points: TListCollection);
  var triangle: KfTriangle;
  begin
  triangle := KfTriangle.createFromPoints(points);
  triangle.lineWidth := lineWidth; {PDF FIX - maybe should scale this?}
  triangle.lineColor := lineColor;
  triangle.foreColor := foreColor;
  triangle.backColor := backColor;
  if self.recording then
  	begin
    {"store triangle"}
    self.addTriangle(triangle);
    end
  else
  	begin
    self.basicDrawTriangle(triangle);
    triangle.free;
    end;
  end;
*)

{"terminology is different: drawing surface frontColor is pen line color }
{ and drawing surface backColor (which should be back-facing triangle) is fill color"}
{drawing surface draws the last triangle allocated - and deallocates it if needed}
procedure KfDrawingSurface.drawLastTriangle;
  var triangle: KfTriangle;
  begin
  triangle := triangles.items[numTrianglesUsed - 1]; {last allocated triangle}
  triangle.isLine := false;
  triangle.updateGeometry;
  triangle.lineWidth := lineWidth; {PDF FIX - maybe should scale this?}
  triangle.lineColor := lineColor;
  triangle.foreColor := foreColor;
  triangle.backColor := backColor;
  if self.recording then
  	begin
    {"store triangle"}
    {do nothing as already stored}
    end
  else
  	begin
    self.basicDrawTriangle(triangle);
    {deallocate it}
    dec(numTrianglesUsed);
    end;
  end;

{pdf - draw the triangle object - assume it is reasonably flat"
{can only draw triangles for now}
{needs fixed size for array - would need to allocate for variable sizes}
{or could use direct windows triangle call and larger array but pass number of points}
{terminology is different: drawing surface frontColor is pen line color}
{and drawing surface backColor (which should be back-facing triangle) is fill color"}
{restricting range of coordingates to 32767 to prevent possible out of range errors when drawing}
procedure KfDrawingSurface.basicDrawTriangle(triangle: KfTriangle);
  var
  startPoint: KfPoint3D;
  endPoint: KfPoint3D;
  aPoint: KfPoint3D;
  pointArray: array[0..2] of TPoint;
  i: integer;
  begin
  if (triangle.isLine) then
    begin
    {draw line}
    try
    self.pane.pen.width := round(triangle.lineWidth);
    self.pane.pen.color := triangle.lineColor;
    self.pane.pen.style := psSolid;
    startPoint := triangle.points[0];
    endPoint := triangle.points[1];
    self.pane.moveTo(round(startPoint.x) mod 32767, round(startPoint.y) mod 32767);
    self.pane.lineTo(round(endPoint.x) mod 32767, round(endPoint.y) mod 32767);
    except
    {do nothing if round fails or assign to integer fails}
    end;
    end
  else
  	begin
  {  if fillingTriangles then
      self.pane.brush.style := bsSolid
    else
      self.pane.brush.style := bsClear;}

    self.pane.brush.color := triangle.visibleSurfaceColor;
    if self.drawingLines then
    	self.pane.pen.color := triangle.invisibleSurfaceColor
    else
    	self.pane.pen.color := triangle.visibleSurfaceColor;
    	for i := 0 to 2 do
    		begin
      	aPoint := triangle.points[i];
        try
    		pointArray[i].x := round(aPoint.x) mod 32767;
    		pointArray[i].y := round(aPoint.y) mod 32767;
        except
        {maybe could test sign to put in negative big numbers? watch out for NaNs!}
    		pointArray[i].x := 32767;
    		pointArray[i].y := 32767;
      	end;
        end;
    self.pane.pen.width := 1;
    if fillingTriangles then
      self.pane.polygon(pointArray)
    else
      self.pane.polyLine(pointArray);
 		end;
	end;

procedure KfDrawingSurface.initialize;
  begin
  backColor := clLtGray;
  foreColor := clLtGray;
  lineColor := clLtGray;
  lineWidth := 1.0;
  recording := false;
  clearTriangles;
  if self.pane <> nil then
    begin
    if fillingTriangles then
      self.pane.brush.style := bsSolid
    else
      self.pane.brush.style := bsClear;
    end;
  end;

{ ---------------------------------------------------------------------------------- KfIndexTriangle }
constructor KfIndexTriangle.createABC(a: longint; b: longint; c: longint);
	begin
  self.create;
  points[0] := a;
  points[1] := b;
  points[2] := c;
  end;

procedure KfIndexTriangle.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KKfIndexTriangle;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure KfIndexTriangle.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  filer.streamBytes(points, sizeof(points));
  end;

{ ---------------------------------------------------------------------------------- KfObject3D }
constructor KfObject3D.create;
	begin
  inherited create;
  triangles := TListCollection.create;
  end;

destructor KfObject3D.destroy;
	begin
  triangles.free;
  inherited destroy;
  end;

procedure KfObject3D.copyFrom(original: KfObject3D);
  var
    i: integer;
    theTriangle: KfIndexTriangle;
  begin
  if original = nil then exit;
  name := original.name;
  if original.pointsInUse > 0 then
    for i := 0 to original.pointsInUse - 1 do
      points[i] := original.points[i];
  self.pointsInUse := original.pointsInUse;
  triangles.clear;
  if original.triangles.count > 0 then
    for i := 0 to original.triangles.count - 1 do
      begin
      theTriangle := original.triangles.items[i];
      self.addTriangle(KfIndexTriangle.createABC(theTriangle.points[0], theTriangle.points[1], theTriangle.points[2]));
      end;
  inUse := original.inUse;
  end;

function KfObject3D.isSameAs(other: KfObject3D): boolean;
  var
    i: longint;
    j: longint;
    thePoint: KfPoint3d;
    theTriangle: KfIndexTriangle;
    otherPoint: KfPoint3d;
    otherTriangle: KfIndexTriangle;
  begin
  {false if fails any test}
  result := false;
  if self.name <> other.name then exit;
  if self.pointsInUse <> other.pointsInUse then exit;
  if self.triangles.count <> other.triangles.count then exit;
  if self.pointsInUse > 0 then
    for i := 0 to self.pointsInUse - 1 do
      begin
      thePoint := self.points[i];
      otherPoint := other.points[i];
      if (thePoint.x <> otherPoint.x) or (thePoint.y <> otherPoint.y) or (thePoint.z <> otherPoint.z) then
      	exit;
      end;
  if self.triangles.count > 0 then
    for i := 0 to self.triangles.count - 1 do
      begin
      theTriangle := self.triangles.items[i];
      otherTriangle := other.triangles.items[i];
      for j := 0 to 2 do
        if theTriangle.points[j] <> otherTriangle.points[j] then exit;
      end;
  {passed all the tests}
  result := true;
  end;

function KfObject3D.getName: string;
  begin
  result := self.name;
  end;

procedure KfObject3D.setName(newName: string);
  begin
  name := newName;
  end;

procedure KfObject3D.addPoint(const aPoint: KfPoint3d);
  begin
  if pointsInUse >= kMaximumRecordedPoints then
    raise Exception.create('too many points in three-D object');
  self.points[self.pointsInUse] := aPoint;
  inc(self.pointsInUse);
  end;

{PDF FIX - may want to optimize this to use var to pass around result rather than assign}
function KfObject3D.pointForIndex(anIndex: longint): KfPoint3D;
	begin
	if (anIndex < 1) or (anIndex > self.pointsInUse) then
  	raise Exception.create('point index out of range');
  result := self.points[anIndex - 1];
  end;

procedure KfObject3D.addTriangle(aTriangle: KfIndexTriangle);
  begin
  self.triangles.add(aTriangle);
  end;

function KfObject3D.triangleForIndex(anIndex: longint): KfIndexTriangle;
	begin
	if (anIndex < 1) or (anIndex > self.triangles.count)then
  	raise Exception.create('triangle index out of range');
  result := self.triangles.items[anIndex - 1];
  end;

{adjust all points for origin, which is assumed to be at the first point}
{in terms of plant organs, this means the first point is
where the organ is attached to the plant}
procedure KfObject3D.adjustForOrigin;
  var
  	firstPoint: KfPoint3D;
    i: longint;
  begin
    if pointsInUse < 1 then exit;
    firstPoint := self.points[0];
    if pointsInUse > 1 then
    	for i := 1 to pointsInUse - 1 do
    		KfPoint3D_subtract(self.points[i], firstPoint);
    {makes the first point zero}
    KfPoint3D_setXYZ(self.points[0], 0.0, 0.0, 0.0);
	end;

procedure KfObject3D.drawWithTurtleScale(turtle: KfTurtle; scale: single);
	var
  	i: longint;
    triangle: KfIndexTriangle;
   { saveDrawLines: boolean; }
  begin
  {draw 3D object using turtle to scale}
{ saveDrawLines := turtle.drawingSurface.drawingLines;     }
{  if (scale * turtle.scale < 0.5) and turtle.drawingSurface.drawingLines then
    turtle.drawingSurface.drawingLines := false; }
  turtle.clearRecording;
  if pointsInUse > 0 then
  	for i := 0 to pointsInUse - 1 do
    	turtle.transformAndRecord(self.points[i], scale);
  if triangles.count > 0 then
  	for i := 1 to triangles.count do
      begin
      triangle := triangleForIndex(i);
    	turtle.drawTriangleFromIndexes(triangle.points[0], triangle.points[1], triangle.points[2]);
      end;
{  turtle.drawingSurface.drawingLines := saveDrawLines;  }
  end;

procedure KfObject3D.addTriangleWithVerticesABC(a: longint; b: longint; c: longint);
	begin
  self.addTriangle(KfIndexTriangle.createABC(a, b, c));
  end;

{parse string into xyz positions and add point to collection}
procedure KfObject3D.addPointString(stream: KfStringStream);
  var
    aPoint3D: KfPoint3D;
    x,y,z: integer;
	begin
  x := 0; y := 0; z := 0;
  x := stream.nextInteger;
  y := stream.nextInteger;
  z := stream.nextInteger;
  KfPoint3d_setXYZ(aPoint3D, x, y, z);
  self.addPoint(aPoint3D);
  end;

{parse string into three point indexes and add triangle to collection}
procedure KfObject3D.addTriangleString(stream: KfStringStream);
  var
    p1, p2, p3: integer;
  begin
  p1 := 0; p2 := 0; p3 := 0;
  p1 := stream.nextInteger;
  p2 := stream.nextInteger;
  p3 := stream.nextInteger;
  if (p1 = 0) or (p2 = 0) or (p3 = 0)
    or (p1 > pointsInUse) or (p2 > pointsInUse) or (p3 > pointsInUse) then
    messageDlg('Bad triangle: ' + intToStr(p1) + ' ' + intToStr(p2) + ' ' + intToStr(p3)
        + '. Point indexes must be between 1 and ' + intToStr(pointsInUse) + '.', mtError, [mbOK], 0)
  else
    self.addTriangle(KfIndexTriangle.createABC(p1, p2, p3));
  end;

procedure KfObject3D.fromFileStream(var inputFile: TextFile);
	var
  	inputLine: string;
    fieldType: string;
    stream: KfStringStream;
  begin
  {read info for 3D object from file at current position}
  stream := KfStringStream.create;
	while not eof(inputFile) do
    begin
    readln(inputFile, inputLine);
    stream.onStringSeparator(inputLine, '=');
    fieldType := stream.nextToken;
    stream.spaceSeparator;
    if fieldType = 'Name' then
    	self.name := stream.remainder
    else if fieldType = 'Point' then
      self.addPointString(stream)
    else if fieldType = 'Triangle' then
      self.addTriangleString(stream)
    else if fieldType[1] = '[' then
      break;
    end;
  self.adjustForOrigin;
  stream.free;
  end;

{reads 3D object from file
could make it so there can be more than one 3D object per file;
right now there can only be one.}
class function KfObject3D.fromFile(aFileName: string): KfObject3D;
	var
  	inputFile: TextFile;
  begin
	assignFile(inputFile, aFileName);
	reset(inputFile);
  if skipToProfileSection(inputFile, 'Three-dimensional object') then
  	begin
		result := KfObject3D.create;
		result.fromFileStream(inputFile);
    end
  else
    raise Exception.create('This file ' + aFileName + ' is not a valid 3D object file.');
  close(inputFile);
	end;

procedure KfObject3d.saveToFile(aFileName: string);
	var
  	outputFile: TextFile;
    i: longint;
    triangle: KfIndexTriangle;
  begin
  {[Three-dimensional object]
  Name=Carrot leaf
  Point=129 236 150
  Triangle=2 4 3  }
  assignFile(outputFile, aFileName);
  try
    rewrite(outputFile);
    writeln(outputFile, '[Three-dimensional object]');
    writeln(outputFile, 'Name=' + name);
    if self.pointsInUse > 0 then for i := 0 to self.pointsInUse - 1 do
      writeln(outputFile, 'Point='
        + intToStr(round(points[i].x)) + ' ' + intToStr(round(points[i].y)) + ' '
        + intToStr(round(points[i].z)));
    if triangles.count > 0 then for i := 0 to triangles.count - 1 do
      begin
      triangle := KfIndexTriangle(triangles[i]);
      writeln(outputFile, 'Triangle='
        + intToStr(triangle.points[0]) + ' ' + intToStr(triangle.points[1]) + ' '
        + intToStr(triangle.points[2]));
      end;
  finally
    closeFile(outputFile);
  end;
  end;

{ for first messy 3d editor }
procedure KfObject3D.fromTStrings(stringList: TStrings);
	var
    i: longint;
  	inputLine: string;
    fieldType: string;
    stream: KfStringStream;
  begin
  { clear out}
  pointsInUse := 0;
  triangles.clear;
  {read info for 3D object from file at current position}
  stream := KfStringStream.create;
  if stringList.count > 0 then for i := 0 to stringList.count - 1 do
    begin
    inputLine := stringList.strings[i];
    stream.onStringSeparator(inputLine, '=');
    fieldType := stream.nextToken;
    stream.spaceSeparator;
    if fieldType = 'Point' then
      self.addPointString(stream)
    else if fieldType = 'Triangle' then
      self.addTriangleString(stream)
    else if fieldType[1] = '[' then
      break;
    end;
  self.adjustForOrigin;
  stream.free;
  end;

procedure KfObject3d.toTStrings(stringList: TStrings);
	var
    i: longint;
    triangle: KfIndexTriangle;
  begin
  {[Three-dimensional object]
  Name=Carrot leaf
  Point=129 236 150
  Triangle=2 4 3  }
  stringList.clear;
  if self.pointsInUse > 0 then for i := 0 to self.pointsInUse - 1 do
    stringList.add('Point=' + intToStr(round(points[i].x)) + ' ' + intToStr(round(points[i].y)) + ' '
        + intToStr(round(points[i].z)));
  if triangles.count > 0 then for i := 0 to triangles.count - 1 do
    begin
    triangle := KfIndexTriangle(triangles[i]);
    stringList.add('Triangle='+ intToStr(triangle.points[0]) + ' ' + intToStr(triangle.points[1]) + ' '
        + intToStr(triangle.points[2]));
    end;
  end;

procedure KfObject3D.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KKfObject3D;
  cvir.versionNumber := 1;
  cvir.additionNumber := 0;
  end;

procedure KfObject3D.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  filer.streamLongint(pointsInUse);
  filer.streamBytes(points, sizeof(points));
  triangles.streamUsingFiler(filer, KfIndexTriangle);
  filer.streamShortString(name);
  filer.streamBoolean(inUse);
  end;

procedure KfObject3D.streamUsingTextFiler(textFiler: GsTextFiler);
  var
    newTriangle: KfIndexTriangle;
    numTriangles, i: longint;
  begin
  with textFiler do
    begin
    textFiler.streamLongint(pointsInUse, 'numPoints');
    if pointsInUse > 0 then
      for i := 0 to pointsInUse - 1 do
        textFiler.streamPoint3d(points[i].x, points[i].y, points[i].z, 'point');
    if textFiler.isWriting then
      begin
      numTriangles := triangles.count;
      textFiler.streamLongint(numTriangles, 'numTriangles');
      if triangles.count > 0 then
        for i := 0 to triangles.count - 1 do
          textFiler.streamTriangle(TObject(triangles[i]), 'triangle');
      end
    else if textFiler.isReading then
      begin
      numTriangles := 0;
      textFiler.streamLongint(numTriangles, 'numTriangles');
      if numTriangles > 0 then
        for i := 0 to numTriangles - 1 do
          begin
          newTriangle := KfIndexTriangle.create;
          triangles.add(newTriangle);
          textFiler.streamTriangle(newTriangle, 'triangle');
          end;
      end;
    streamEndOfLine;
    end;
  end;

{ ---------------------------------------------------------------------------------- KfTurtle }
var
gDefaultTurtle: KfTurtle;
gDefaultTurtleInUse: boolean;

{KfTurtle methods }
{where the default turtle is used need a try..finally
to ensure defaultStopUsing is called no matter what}
class function KfTurtle.defaultStartUsing: KfTurtle;
  begin
  if gDefaultTurtleInUse then
    raise Exception.create('Development - fix - Turtle already in use');
	gDefaultTurtleInUse := true;
  result := gDefaultTurtle;
  end;

class procedure KfTurtle.defaultStopUsing;
  begin
	gDefaultTurtleInUse := false;
  end;

class procedure KfTurtle.defaultAllocate;
  begin
	gDefaultTurtle := nil;
	gDefaultTurtle := KfTurtle.create;
	gDefaultTurtleInUse := false;
  end;

class procedure KfTurtle.defaultFree;
  begin
	gDefaultTurtle.free;
	gDefaultTurtle := nil;
  end;

procedure KfTurtle.resetBoundsRect(basePoint: TPoint);
  begin
  realBoundsRect.left := basePoint.x;
  realBoundsRect.right := basePoint.x;
  realBoundsRect.top := basePoint.y;
  realBoundsRect.bottom := basePoint.y;
  end;

procedure KfTurtle.addToBoundsRect(const aPoint: KfPoint3d);
	begin
  with aPoint do
    with realBoundsRect do
  		begin
  		if x < left then
  			left := x
    	else if x > right then
      	right := x;
    	if y < top then
      	top := y
    	else if y > bottom then
      	bottom := y;
    	end;
  end;

function KfTurtle.angleX: byte;
	begin
  result := currentMatrix.angleX;
  end;

function KfTurtle.angleY: byte;
	begin
  result := currentMatrix.angleY;
  end;

function KfTurtle.angleZ: byte;
	begin
  result := currentMatrix.angleZ;
  end;

procedure KfTurtle.clearRecording;
	begin
 	recordedPointsInUse := 0;
  end;

procedure KfTurtle.recordPosition;
  var newPoint: KfPoint3D;
  begin
  newPoint := currentMatrix.position;
  recordedPoints[recordedPointsInUse] := newPoint;
  inc(recordedPointsInUse);
  self.addToBoundsRect(newPoint);
  end;

{takes over ownership of point - so pass it a copy if needed}
procedure KfTurtle.recordPositionPoint(const aPoint: KfPoint3D);
  begin
  recordedPoints[recordedPointsInUse] := aPoint;
  inc(recordedPointsInUse);
  self.addToBoundsRect(aPoint);
  end;

function KfTurtle.scaleMillimeters(mm: single): single;
  begin
  result := mm * scale;
  end;

procedure KfTurtle.mmPerPixel(mmPerPixel: single);
  begin
  scale := mmPerPixel ;  
  end;

{move some distance specified by point after scaling and transforming point}
procedure KfTurtle.moveAndRecordScale(const originalPoint3D: KfPoint3D; theScale: single);
 	var aPoint3d: KfPoint3D;
  begin
  aPoint3D := originalPoint3D;
  KfPoint3d_scaleBy(aPoint3d, theScale * scale);
  currentMatrix.transform(aPoint3D);
  currentMatrix.position := aPoint3D;
  self.recordPositionPoint(aPoint3D);
  end;

procedure KfTurtle.moveInMillimeters(mm: single);
  begin
  currentMatrix.move(self.scaleMillimeters(mm));
  end;

procedure KfTurtle.moveInMillimetersAndRecord(mm: single);
  begin
  currentMatrix.move(self.scaleMillimeters(mm));
  self.recordPosition;
  end;

procedure KfTurtle.moveInPixels(pixels: single);
  begin
  currentMatrix.move(pixels);
  end;

procedure KfTurtle.moveInPixelsAndRecord(pixels: single);
  begin
  currentMatrix.move(pixels);
  self.recordPosition;
  end;

{suspicious things that call this may not clean up copy}
function KfTurtle.positionCopy: KfPoint3D;
  begin
  result := currentMatrix.position;
  end;

procedure KfTurtle.pop;
  begin
  {pdf - pop the matrix on the stack to the current matrix}
  if numMatrixesUsed < 1 then exit;
  dec(numMatrixesUsed);
  currentMatrix := matrixStack.items[numMatrixesUsed-1];
  end;

procedure KfTurtle.push;
  begin
  {pdf - push the matrix on the stack}
  if numMatrixesUsed >= matrixStack.count then
  	matrixStack.add(currentMatrix.deepCopy)
  else
    currentMatrix.copyTo(KfMatrix(matrixStack.items[numMatrixesUsed]));
  inc(numMatrixesUsed);
  currentMatrix := matrixStack.items[numMatrixesUsed-1];
  end;

{procedure KfTurtle.quadrangle: aPoint with: bPoint with: cPoint  with:  dPoint

    ^Array
        with: (recordedPoints at: aPoint)
        with: (recordedPoints at: bPoint)
        with: (recordedPoints at: cPoint)
        with: (recordedPoints at: dPoint)!
}

{procedure KfTurtle.reset

    self initialize;
 }

procedure KfTurtle.rotateX(angle: single);
	begin
  currentMatrix.rotateX(angle);
  end;

procedure KfTurtle.rotateY(angle: single);
	begin
  currentMatrix.rotateY(angle);
  end;

procedure KfTurtle.rotateZ(angle: single);
	begin
  currentMatrix.rotateZ(angle);
  end;

function KfTurtle.stackSize: longint;
 	begin
  result := numMatrixesUsed;
  end;

procedure KfTurtle.startRecording;
  begin
  recordedPointsInUse := 0;
  self.recordPosition;
  end;

procedure KfTurtle.transformAndRecord(const originalPoint3D: KfPoint3D; theScale: single);
  var aPoint3D: KfPoint3d;
	begin
  {transform and scale point and record the new location without moving}
  aPoint3D := originalPoint3D;
  KfPoint3d_scaleBy(aPoint3D, scale * theScale);
  currentMatrix.transform(aPoint3D);
  self.recordPositionPoint(aPoint3D);
  end;

function KfTurtle.newTriangleTriangle(aPointIndex: longint; bPointIndex: longint; cPointIndex: longint): KfTriangle;
	begin
	result := KfTriangle.create;
  result.points[0] := recordedPoints[aPointIndex];
  result.points[1] := recordedPoints[bPointIndex];
  result.points[2] := recordedPoints[cPointIndex];
  end;

{pdf - set x y z position in the current matrix.  Adjust for fast math.}
procedure KfTurtle.xyz(x: single; y: single; z: single);
	begin
  currentMatrix.position.x := x;
  currentMatrix.position.y := y;
  currentMatrix.position.z := z;
  end;

constructor KfTurtle.create;
  var i: longint;
  begin
  inherited create;
  userWantsToRecordPosition := false;
  matrixStack := TListCollection.create;
  currentMatrix := KfMatrix.create;
  currentMatrix.initializeAsUnitMatrix;
  matrixStack.add(currentMatrix);
  numMatrixesUsed := 1;
  {start with a bunch of matrixes}
  for i := 0 to kInitialTurtleMatrixStackDepth do
    matrixStack.add(KfMatrix.create);
  recordedPointsInUse := 0;
	{may want to break KfDrawingSurface out later - so can have turtles share?}
  drawingSurface := KfDrawingSurface.create;
  scale := 1.0;
  end;

destructor KfTurtle.destroy;
	begin
  matrixStack.free;
  matrixStack := nil;
  drawingSurface.free;
  drawingSurface := nil;
  inherited destroy;
  end;

{if the DEBUG_TURTLE flag is defined then draw axis information}
{$IFDEF DEBUG_TURTLE}

procedure debugDrawInMillimeters(turtle: KfTurtle; mm: single);
 	var
  	oldPosition: KfPoint3d;
    newPosition: KfPoint3d;
  begin
    oldPosition := turtle.currentMatrix.position;
    turtle.currentMatrix.move(turtle.scaleMillimeters(mm));
    newPosition := turtle.currentMatrix.position;
    turtle.drawingSurface.drawLineFromTo(oldPosition, newPosition);
    turtle.addToBoundsRect(newPosition);
  end;

procedure debugDrawAxis(turtle: KfTurtle; pixels: single);
 	var
  	oldPosition: KfPoint3d;
    newPosition: KfPoint3d;
  begin
    oldPosition := turtle.currentMatrix.position;
    turtle.currentMatrix.move(pixels);
    newPosition := turtle.currentMatrix.position;
    turtle.drawingSurface.drawLineFromTo(oldPosition, newPosition);
  end;

procedure KfTurtle.drawInMillimeters(mm: single);
 	var
    oldColor: TColorRef;
    oldWidth: single;
  begin
  oldColor := drawingSurface.lineColor;
  oldWidth := drawingSurface.lineWidth;
  drawingSurface.lineWidth := 2;
	drawingSurface.lineColor := clRed;
	self.push;
	self.rotateZ(64); {will flip so draw Y (sic)}
	debugDrawAxis(self, 100.0);
	self.pop;
  drawingSurface.lineWidth := 1;
	drawingSurface.lineColor := clBlue;
	self.push;
	self.rotateY(64); {will flip so draw Z (sic)}
	debugDrawAxis(self, 100.0);
	self.pop;
  drawingSurface.lineColor := oldColor;
  drawingSurface.lineWidth := oldWidth;
  debugDrawInMillimeters(self, mm);
  end;

{$ELSE}

procedure KfTurtle.drawInMillimeters(mm: single);
 	var
  	oldPosition: KfPoint3d;
    newPosition: KfPoint3d;
  begin
    oldPosition := currentMatrix.position;
    currentMatrix.move(self.scaleMillimeters(mm));
    newPosition := currentMatrix.position;
    drawingSurface.drawLineFromTo(oldPosition, newPosition);
    self.addToBoundsRect(newPosition);
  end;

{$ENDIF}

procedure KfTurtle.drawInPixels(pixels: single);
 	var
  	oldPosition: KfPoint3d;
    newPosition: KfPoint3d;
  begin
    oldPosition := currentMatrix.position;
    currentMatrix.move(pixels);
    newPosition := currentMatrix.position;
    drawingSurface.drawLineFromTo(oldPosition, newPosition);
    self.addToBoundsRect(newPosition);
    {distance (pixels) }
	end;

procedure KfTurtle.drawTriangle;
  var triangle: KfTriangle;
  begin
  if recordedPointsInUse <> 3 then
    raise Exception.create('triangle made without three points');
  {drawing surface owns triangle}
  triangle := self.drawingSurface.allocateTriangle;
  triangle.points[0] := recordedPoints[0];
  triangle.points[1] := recordedPoints[1];
  triangle.points[2] := recordedPoints[2];
  self.drawingSurface.drawLastTriangle;
  recordedPointsInUse := 0;
  end;

procedure KfTurtle.setForeColorBackColor(frontColor: TColor; backColor: TColor);
	begin
  drawingSurface.foreColor := frontColor;
  drawingSurface.backColor := backColor;
  end;

{procedure KfTurtle.drawQuadrangle: aPoint with: bPoint with: cPoint  with:  dPoint

    drawingSurface drawTriangle:
        Array
            with: (recordedPoints at: aPoint)
            with: (recordedPoints at: bPoint)
            with: (recordedPoints at: cPoint)
            with: (recordedPoints at: dPoint)!
}

(*
procedure KfTurtle.drawTriangleFromPoints(aPoint: KfPoint3D; bPoint: KfPoint3D; cPoint: KfPoint3D);
  var triangle: KfTriangle;
	begin
	triangle := KfTriangle.create;
  triangle.points.add(aPoint.copy);
  triangle.points.add(bPoint.copy);
  triangle.points.add(cPoint.copy);

  drawingSurface.drawTriangle(triangle); {drawing surface takes over ownership of triangle}
  end;
*)

procedure KfTurtle.drawTriangleFromIndexes(aPointIndex: longint; bPointIndex: longint; cPointIndex: longint);
  var triangle: KfTriangle;
	begin
	{drawing surface takes over ownership of triangle}
  triangle := self.drawingSurface.allocateTriangle;
  drawingSurface.lineWidth := 1.0;
  triangle.points[0] := recordedPoints[aPointIndex-1];
  triangle.points[1] := recordedPoints[bPointIndex-1];
  triangle.points[2] := recordedPoints[cPointIndex-1];
  drawingSurface.drawLastTriangle;
  end;

function KfTurtle.lineColor: TColor;
  begin
  result := drawingSurface.lineColor;
  end;

procedure KfTurtle.setLineColor(aColor: TColor);
  begin
  drawingSurface.lineColor := aColor;
  end;

function KfTurtle.lineWidth: single;
  begin
  result := drawingSurface.lineWidth;
  end;

procedure KfTurtle.setLineWidth(aWidth: single);
  begin
  drawingSurface.lineWidth := aWidth * scale;
  if drawingSurface.lineWidth > 16000 then drawingSurface.lineWidth := 16000;
  end;

procedure KfTurtle.reset;
	begin
  numMatrixesUsed := 1;
  currentMatrix := matrixStack.items[0];
  currentMatrix.initializeAsUnitMatrix;
  recordedPointsInUse := 0;
  drawingSurface.fillingTriangles := not self.drawOptions.wireFrame;
  drawingSurface.drawingLines := self.drawOptions.drawLines;
  drawingSurface.initialize;
  scale := 1.0;
  end;

function KfTurtle.position: KfPoint3d;
  begin
  tempPosition := currentMatrix.position;
  result := tempPosition;
  end;

function KfTurtle.boundsRect: TRect;
  begin
  try
  result.top := round(realBoundsRect.top);
  result.left := round(realBoundsRect.left);
  result.bottom := round(realBoundsRect.bottom);
  result.right := round(realBoundsRect.right);
  except
  {might want to be more precise in assigning a rect}
  result.top := -32767;
  result.left := -32767;
  result.bottom := 32767;
  result.right := 32767;
  end;
  end;

(*
procedure KfTurtle.moveXYZScaleAndRecord

"(long xu, long yu, long zu, float scale)
  {
  long x = xu * scale * gTurtleScale;
  long y = yu * scale * gTurtleScale;
  long z = zu * scale * gTurtleScale;

  if (FM_CurrentMatrix)
	 {
	 FM_x += x * MB_a0;
	 FM_y += x * MB_b0;
	 FM_z += x * MB_c0;

	 FM_x += y * MB_a1;
	 FM_y += y * MB_b1;
	 FM_z += y * MB_c1;

	 FM_x += z * MB_a2;
	 FM_y += z * MB_b2;
	 FM_z += z * MB_c2;
	 }
  else
	 {
	 FM_x += x * MA_a0;
	 FM_y += x * MA_b0;
	 FM_z += x * MA_c0;

	 FM_x += y * MA_a1;
	 FM_y += y * MA_b1;
	 FM_z += y * MA_c1;

	 FM_x += z * MA_a2;
	 FM_y += z * MA_b2;
	 FM_z += z * MA_c2;
	 }
  gXTurtlePointArray[gTurtleRecordedPoints] = FM_x PixelLower;
  gYTurtlePointArray[gTurtleRecordedPoints] = FM_y PixelLower;
  gZTurtlePointArray[gTurtleRecordedPoints] = FM_z PixelLower;
  gTurtleRecordedPoints++;
  }
"!

transformXYZScaleAndRecord

"(long xu, long yu, long zu, float scale)
  {
  long x = xu * scale * gTurtleScale;
  long y = yu * scale * gTurtleScale;
  long z = zu * scale * gTurtleScale;
  long theX = 0;
  long theY = 0;
  long theZ = 0;

  if (FM_CurrentMatrix)
	 {
	 theX = x * MB_a0 + y * MB_a1 + z * MB_a2;

	 theY = x * MB_b0 + y * MB_b1 + z * MB_b2;

	 theZ = x * MB_c0 + y * MB_c1 + z * MB_c2;
	 }
  else
	 {
	 theX = x * MA_a0 + y * MA_a1 + z * MA_a2;

	 theY = x * MA_b0 + y * MA_b1 + z * MA_b2;

	 theZ = x * MA_c0 + y * MA_c1 + z * MA_c2;
	 }
  gXTurtlePointArray[gTurtleRecordedPoints] = (FM_x PixelLower) + (theX PixelLower);
  gYTurtlePointArray[gTurtleRecordedPoints] = (FM_y PixelLower) + (theY PixelLower);
  gZTurtlePointArray[gTurtleRecordedPoints] = (FM_z PixelLower) + (theZ PixelLower);
  gTurtleRecordedPoints++;
  }
}
*)

function Clockwise (const point0: TPoint; const point1: TPoint; const point2: TPoint): integer;
  var dx1, dy1, dx2, dy2: single;
  begin
  dx1 := point1.x - point0.x;
  dy1 := point1.y - point0.y;
  dx2 := point2.x - point0.x;
  dy2 := point2.y - point0.y;

  if (dx1 * dy2) > (dy1 * dx2) then
    result := kDirectionClockwise
  else if (dx1 * dy2) < (dy1 * dx2) then
    result := kDirectionCounterClockwise
  else if ((dx1 * dx2) < 0) Or ((dy1 * dy2) < 0) then
    result := kDirectionCounterClockwise
  else if ((dx1 * dx1) + (dy1 * dy1)) < ((dx2 * dx2) + (dy2 * dy2)) then
    result := kDirectionClockwise
  else
    result := kDirectionUnknown;
	end;

{since not sure what way quandrangle goes - need to just see if they are all the same}
{not 100% sure this will work}
function PointInQuadrangle(const point: TPoint; const quadrangle: array of TPoint): boolean;
  var
   first, second, third: integer;
   fourth, fifth, sixth: integer;
   inFirst, inSecond: boolean;
  begin
  result := false;
  inFirst := false;
  inSecond := false;
  {do first triangle}
  first := clockwise(point, quadrangle[0], quadrangle[1]);
  second := clockwise(point, quadrangle[1], quadrangle[3]);
  third := clockwise(point, quadrangle[3], quadrangle[0]);
  if (first = second) and (second = third) then
  	inFirst := true;
  {do second triangle}
  fourth := clockwise(point, quadrangle[1], quadrangle[2]);
  fifth := clockwise(point, quadrangle[2], quadrangle[3]);
  sixth := clockwise(point, quadrangle[3], quadrangle[1]);
  if (fourth = fifth) and (fifth = sixth) then
  	inSecond := true;
  if inFirst and inSecond then
    {check if it is in concave section }
    begin
    if first = fourth then
    	result := true
    end
  else
    result := inFirst or inSecond;
 (* {do other two triangles to be safe}
  {do third triangle}
  first := clockwise(point, quadrangle[2], quadrangle[3]);
  second := clockwise(point, quadrangle[3], quadrangle[0]);
  third := clockwise(point, quadrangle[0], quadrangle[2]);
  if (first = second) and (second = third) then
  	exit;
  {do fourth triangle}
  first := clockwise(point, quadrangle[3], quadrangle[0]);
  second := clockwise(point, quadrangle[0], quadrangle[1]);
  third := clockwise(point, quadrangle[1], quadrangle[2]);
  if (first = second) and (second = third) then
  	exit;       *)
  end;

(*
function intersect (line1, line2: LineType): boolean;
  var
   firstCase, secondCase: boolean;
  begin
  {was <= 0; changed to < 0 to remove case of being on the line from classification as intersection}
  if (Clockwise(line1.point1, line1.point2, line2.point1) * Clockwise(line1.point1, line1.point2, line2.point2)) < 0 then
    firstCase := true
  else
    firstCase := false;

  if (Clockwise(line2.point1, line2.point2, line1.point1) * Clockwise(line2.point1, line2.point2, line1.point2)) < 0 then
    secondCase := true
  else
    secondCase := false;

  if firstCase and secondCase then
    result := true
  else
    result := false;
	end;
*)

(*
function inside(testPoint: TPoint; triangle): boolean;
  begin
  i, count, j: integer;
  lt, lp: LineType;
  count := 0;
  j := 0;
  lt.p1 := testPoint;
  lt.p2 := testPoint;
  lt.p2.x := 32767;
  for i := 1 to poygon.points do
    begin
    lp.p1 := triangle.points[i];
    lp.p2 := triangle.points[i];
    if not intersect(lp, lt) then
      begin
      lp.p2 := triangle.points[j];
      j := i;
      if intersect(lp, lt) then
        inc(count);
      end;
    end;
  result := count and 1;
  end;

 *)

(*



Function IsConcave (points() As pointType) As Integer
  Dim clockwiseAhead As Integer, clockwiseBack As Integer, clockwiseCenter As Integer
  Dim concaveTestAhead As Integer, concaveTestBack As Integer, concaveTestCenter As Integer
  
  clockwiseAhead = Clockwise(points(3), points(4), points(5))
  clockwiseBack = Clockwise(points(3), points(2), points(1))
  clockwiseCenter = Clockwise(points(3), points(4), points(2))
  
  concaveTestAhead = (clockwiseAhead = DIRECTION_COUNTERCLOCKWISE)
  concaveTestBack = (clockwiseBack = DIRECTION_CLOCKWISE)
  concaveTestCenter = (clockwiseCenter = DIRECTION_COUNTERCLOCKWISE)

  IsConcave = (concaveTestAhead Or concaveTestBack Or concaveTestCenter)

End Function
Function PointInTriangle (x As Integer, y As Integer, theTriangleArray() As intPointType, numPoints As Integer)
  Dim result As Integer
  Dim hRgn As Integer
  
  hRgn = CreateTriangleRgn(theTriangleArray(0), numPoints, ALTERNATE)
  If hRgn <> 0 Then
    PointInTriangle = PtInRegion(hRgn, x, y)
    result = DeleteObject(hRgn)
  Else
    PointInTriangle = 0
  End If

End Function
Function CalcAngle (dx As Single, dy As Single) As Single
  Dim angle As Single

  If dx = 0 Then
    If dy > 0 Then
      angle = PI / 2
    Else
      angle = 3 * PI / 2
    End If
  Else
    If dx > 0 Then
      angle = Atn(dy / dx)
    Else
      angle = PI + Atn(dy / dx)   ' ok; dx <> 0
    End If
  End If
  angle = angle + PI / 2
  CalcAngle = angle

End Function


*)

{do special initialization here}
begin
FastTrigInitialize;
KfTurtle.defaultAllocate;
{need to default free somewhere in project}
end.

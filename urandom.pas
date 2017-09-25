unit Urandom;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
urandom: An object that generates random numbers. All the model objects have one
(weather, soil, plant, drawing plant), but only the drawing plant uses the one it
has (actually it has two). Meant to use these in calculations but did not get around to it.
Most of the model code instead uses the random functions in ueutils (mainly Utils_RandomZeroToOne).
So this code is used right now only by the drawing plant, though the objects are created
and maintained by several classes. The idea is to move into a situation where each
model object has its own generator that operates independently of others, which is
more reliable. The generation algorithm for zeroToOne is almost identical to the function in
ueutils. }

interface

uses ufiler;

type
GsRandom = class(GsStreamableObject)
  public
  seed: longint;
  constructor createFromTime;
  procedure randomizeFromTime;
	procedure initialize(aLong: longint);
	function randomNormal(mean: single): single;
	function randomNormalBoundedZeroToOne(mean: single): single;
	function randomNormalPercent(mean: single): single;
	function randomPercent: single;
	function zeroToOne: single;
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  end;

implementation

uses SysUtils, ueutils, uclasses;

constructor GsRandom.createFromTime;
	begin
  self.create;
  self.randomizeFromTime;
  end;

procedure GsRandom.randomizeFromTime;
	var
    Present: TDateTime;
    Year, Month, Day, Hour, Min, Sec, MSec: Word;
    longintVar: longint;
	begin
  Present:= Now;
  DecodeDate(Present, Year, Month, Day);
  DecodeTime(Present, Hour, Min, Sec, MSec);
  { was (Hour * 24 * 60 * 60 * 100) + }
  longintVar := 1;
  initialize(longintVar * Hour * 24 * 60 * 60 + longintVar * Min * 60 * 60 * 100
    + longintVar * Sec * 100 + longintVar * MSec);
  end;

procedure GsRandom.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsRandom;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsRandom.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  filer.streamLongint(seed);
  end;

procedure GsRandom.Initialize(aLong: longint);
  begin
  seed := aLong;
  end;

function GsRandom.RandomNormal(mean: single): single;
  var
    randomNumber: single;
  	i: integer;
  begin
  {return normal random number based on mean (and std dev of half mean)}
  randomNumber := 0.0;
  for i := 1 to 12 do
    begin
		randomNumber := randomNumber + zeroToOne;
    end;
  result := (randomNumber - 6.0) * (mean / 2.0) + mean;
  end;

function GsRandom.RandomNormalBoundedZeroToOne(mean: single): single;
  begin
  {return normal random number based on mean (and std dev of half mean)}
  result := max(0.0, (min(1.0, (randomNormal(mean)))));
  end;

function GsRandom.RandomNormalPercent(mean: single): single;
  begin
  {return normal random number based on mean (and std dev of half mean) bounded at 0 and 100}
  result := max(0, (min(100, round(randomNormal(mean / 100.0) * 100.0))));
  end;

function GsRandom.randomPercent: single;
  var
    k: single;
  begin
  k := round(seed / 127773);
  seed := round((16807  * (seed - (k * 127773))) - (k * 2846));
  if (seed < 0.0) then seed := seed + 2147483647;
  result := round(max(0, (min(100, (seed * 0.0000000004656612875  * 100.0)))));
  end;

function GsRandom.zeroToOne: single;
  var
    k: single;
  begin
  k := round (seed / 127773);
  seed := round((16807  * (seed - (k * 127773))) - (k * 2846));
  if (seed < 0.0) then seed := seed + 2147483647;
  result := (max(0.0, (min(1.0, (seed * 0.0000000004656612875))))) * 1.0;
  end;

end.

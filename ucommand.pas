unit Ucommand;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ucommand: Base class for all undoable commands, and command list that manages the commands
to create and maintain undoable/redoable commands in the menu. See ugscom for all the
undoable command subclasses. }

interface

uses Classes, ucollect, WinTypes;

type
TrackPhase = (trackPress, trackMove, trackRelease);

KfCommand = class(TObject)
	public
  canUndo: boolean;
  done: boolean;
  procedure doCommand; virtual;
  procedure undoCommand; virtual;
  procedure redoCommand; virtual;
  function description: string; virtual;
  function TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand; virtual;
  constructor create;
  destructor destroy; override;
  procedure addPlantsInUseNotInGarden(inUseList: TList); virtual;
  end;

KfCommandList = class(TObject)
	public
	commandList: TListCollection;
	lastDoneCommandIndex: longint;
  undoLimit: longint;
  mouseCommand: KfCommand;
  anchorPoint: TPoint;
  previousPoint: TPoint;
  function command(index: longint): KfCommand;
  procedure setNewUndoLimit(newLimit: longint);
	procedure freeCommandsAboveLimit(theLimit: longint);
  procedure doCommand(newCommand: KfCommand);
  function mouseDown(newCommand: KfCommand; point: TPoint): boolean;
  procedure mouseMove(point: TPoint);
  procedure mouseUp(point: TPoint);
  function isUndoEnabled: boolean;
  function isRedoEnabled: boolean;
  function undoDescription: string;
  function redoDescription: string;
  procedure undoLast;
  procedure removeCommand(aCommand: KfCommand);
  procedure redoLast;
  constructor create;
  destructor destroy; override;
  end;

implementation

uses SysUtils;

{KfCommand}
procedure KfCommand.doCommand;
	begin
  self.done := true;
  {subclass should override and call inherited}
  end;

procedure KfCommand.undoCommand;
	begin
  self.done := false;
  {sublass should override and call inherited}
  end;

procedure KfCommand.redoCommand;
	begin
  self.doCommand;
  {sublass may override and call inherited doCommand}
  end;

constructor KfCommand.create;
	begin
  inherited create;
  canUndo := true;
  done := false;
  end;

destructor KfCommand.destroy;
	begin
  {sublass could override}
  inherited destroy;
  end;

function KfCommand.description: string;
	begin
  result := '*command description*';
  end;

function KfCommand.TrackMouse(aTrackPhase: TrackPhase; var anchorPoint, previousPoint, nextPoint: TPoint;
  	mouseDidMove: boolean): KfCommand;
  begin
  {sublasses should override if needed}
  result := self;
  end;

procedure KfCommand.addPlantsInUseNotInGarden(inUseList: TList);
  begin
  {do nothing - sublasses may override}
  end;

{KfCommandList}
constructor KfCommandList.create;
  begin
  inherited create;
  commandList := TListCollection.create;
	lastDoneCommandIndex := -1;
  undoLimit := 10;
  end;

destructor KfCommandList.destroy;
	begin
  commandList.free;
  {if mouseCoOmmand <> nil then error condition - ignoring for now - not released}
  {could only happend if quitting somehow in middle of action}
  inherited destroy;
  end;

function KfCommandList.command(index: longint): KfCommand;
	begin
  result := KfCommand(commandList.items[index]);
  end;

procedure KfCommandList.setNewUndoLimit(newLimit: longint);
  begin
  undoLimit := newLimit;
  self.freeCommandsAboveLimit(undoLimit);
  end;

{free any command more than the number passed in}
procedure KfCommandList.freeCommandsAboveLimit(theLimit: longint);
  var
    theCommand: KfCommand;
  begin
  while (commandList.count > theLimit) and (commandList.count > 0) do
  	begin
    theCommand := command(0);
    commandList.delete(0);
    theCommand.free;
    dec(lastDoneCommandIndex);
    if lastDoneCommandIndex < -1 then lastDoneCommandIndex := -1;
    end;
  end;

procedure KfCommandList.doCommand(newCommand: KfCommand);
	var i: longint;
    theCommand: KfCommand;
	begin
  {remove any extra commands after the current}
  {do this first to free memory for command}
  if isRedoEnabled then
  	for i := commandList.count - 1 downto lastDoneCommandIndex + 1 do
    	begin
      theCommand := command(i);
      commandList.delete(i);
      theCommand.free;
      end;
  {see if too many commands are stored and if so, scroll them}
  self.freeCommandsAboveLimit(undoLimit - 1);
  {now do this command}
  newCommand.doCommand; {may fail in which case won't add}
  commandList.add(newCommand);
  inc(lastDoneCommandIndex);
  end;

{added nextMouseCommand in these three functions to deal with unhandled exceptions occurring
during mouse commands.  This way, the command will not be further processed.
This may occasionally leak - the mouse command should be the one responsible for freeing
itself and returning nil if a problem occurs}
{returns whether the command finished tracking without freeing itself}
function KfCommandList.mouseDown(newCommand: KfCommand; point: Tpoint): boolean;
  var
    nextMouseCommand: KfCommand;
	begin
  result := false;
  {check if need to clear mouse command}
  if mouseCommand <> nil then
    self.mouseUp(point);
  mouseCommand := nil;
  {save mouse command and start it}
  if newCommand <> nil then
    begin
    anchorPoint := point;
    previousPoint := point;
  	nextMouseCommand := newCommand;
  	mouseCommand := nextMouseCommand.trackMouse(TrackPress, anchorPoint, previousPoint, point, false);
    result := (mouseCommand <> nil);
    end;
  end;

procedure KfCommandList.mouseMove(point: TPoint);
  var
  	mouseDidMove: boolean;
    nextMouseCommand: KfCommand;
	begin
  nextMouseCommand := mouseCommand;
  mouseCommand := nil;
  if nextMouseCommand <> nil then
    begin
    mouseDidMove := (previousPoint.x <> point.x) or (previousPoint.y <> point.y);
  	mouseCommand := nextMouseCommand.trackMouse(TrackMove, anchorPoint, previousPoint, point, mouseDidMove);
    end;
  previousPoint := point;
  end;

procedure KfCommandList.mouseUp(point: TPoint);
  var
  	mouseDidMove: boolean;
    nextMouseCommand: KfCommand;
	begin
  nextMouseCommand := mouseCommand;
  mouseCommand := nil;
  if nextMouseCommand <> nil then
    begin
    mouseDidMove := (previousPoint.x <> point.x) or (previousPoint.y <> point.y);
  	nextMouseCommand := nextMouseCommand.trackMouse(TrackRelease, anchorPoint, previousPoint, point, mouseDidMove);
  	if nextMouseCommand <> nil then
    	doCommand(nextMouseCommand);
    end;
  end;

function KfCommandList.isUndoEnabled: boolean;
	begin
  result := lastDoneCommandIndex >= 0;
  end;

function KfCommandList.isRedoEnabled: boolean;
	begin
  result := lastDoneCommandIndex < (commandList.count - 1);
  end;

function KfCommandList.undoDescription: string;
	begin
  if lastDoneCommandIndex >= 0 then
  	result := command(lastDoneCommandIndex).description
  else
    result := '';
  end;

function KfCommandList.redoDescription: string;
	begin
  if lastDoneCommandIndex < (commandList.count - 1) then
  	result := command(lastDoneCommandIndex+1).description
  else
    result := '';
  end;

procedure KfCommandList.undoLast;
	begin
  if lastDoneCommandIndex >= 0 then
  	begin
  	command(lastDoneCommandIndex).undoCommand;
    dec(lastDoneCommandIndex);
    end;
  end;

procedure KfCommandList.redoLast;
	begin
  if lastDoneCommandIndex < (commandList.count - 1) then
  	begin
  	command(lastDoneCommandIndex+1).redoCommand;
    inc(lastDoneCommandIndex);
    end;
  end;

procedure KfCommandList.removeCommand(aCommand: KfCommand);
	begin
  { assume this command has been undone previously }
  if aCommand.done then
    raise Exception.create('KfCommandList.removeCommand: command not undone');
  commandList.remove(aCommand);
  end;


end.

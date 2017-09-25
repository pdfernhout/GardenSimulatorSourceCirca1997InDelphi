unit ucursor;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ucursor: Wrapper functions for showing wait cursor and loading GWI custom cursors. }

interface

uses WinProcs, WinTypes;

const
  crArrowGarden = 1;
  crArrowSoil = 2;
  crArrowPlant = 3;
  crGloveHarvest = 4;

procedure cursor_initializeWait;
procedure cursor_startWait;
procedure cursor_startWaitIfNotWaiting;
procedure cursor_stopWait;
procedure cursor_loadCustomCursors;

var
  oldCursor: HCursor;
  waitState: integer;

implementation

{$IFDEF WINDOWS}
	{$R CURSORS}
{$ELSE}
	{$R CURSOR32}
{$ENDIF}

uses Forms;

procedure cursor_initializeWait;
begin
  waitState := 0;
end;

procedure cursor_startWait;
begin
  waitState := waitState + 1;
  oldCursor := SetCursor(LoadCursor(0, IDC_WAIT));
end;

procedure cursor_startWaitIfNotWaiting;
begin
	if waitState = 0 then cursor_startWait;
end;

procedure cursor_stopWait;
begin
  if waitState > 0 then
    begin
  	waitState := waitState - 1;
  	if waitState = 0 then SetCursor(oldCursor);
    end;
end;

{Note:	You don't need to call the WinAPI function DestroyCursor when you are finished using the custom
cursor; Delphi does this automatically. }
procedure cursor_loadCustomCursors;
	begin
  Screen.Cursors[crArrowGarden] := LoadCursor(HInstance, 'CRARROWGARDEN');
  Screen.Cursors[crArrowSoil] := LoadCursor(HInstance, 'CRARROWSOIL');
  Screen.Cursors[crArrowPlant] := LoadCursor(HInstance, 'CRARROWPLANT');
  Screen.Cursors[crGloveHarvest] := LoadCursor(HInstance, 'CRGLOVEHARVEST');
	end;

begin
cursor_initializeWait;
cursor_loadCustomCursors;
end.

unit Ucollect;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ucollect: TListCollection, same as TList but it frees its contents when it is freed.
From chap 19 Delphi Developer's Guide, with empty clearing bug fixed, and extended for GsFiler.}

interface

uses Classes, UFiler;

type
  TForEachProc = procedure(each: TObject; data: TObject);
  TFirstLastFunc = function(each: TObject; data: TObject): Boolean;

  TListCollection = class(TList)
  public
    destructor Destroy; override;
    procedure ForEach(Proc: TForEachProc; data: TObject);
    function FirstThat(TestFunc: TFirstLastFunc; data: TObject): TObject;
    function LastThat(TestFunc: TFirstLastFunc; data: TObject): TObject;
    procedure streamUsingFiler(filer: GsFiler; classForCreating: GsStreamableObjectClass);
    procedure clear; {note this should be virtual in TList but it isn't}
    procedure clearPointersWithoutDeletingObjects;
  end;

procedure addToListIfAbsent(list: TList; item: TObject);

implementation

procedure addToListIfAbsent(list: TList; item: TObject);
  begin
  if item = nil then exit;
  if list.indexOf(item) < 0 then
    list.add(item);
  end;

procedure TListCollection.clear;
  var
    i: integer;
    temp: TObject;
  begin
  if count > 0 then
  	for i:= 0 to count - 1 do
    	begin
    	temp := items[i];
      items[i] := nil;
    	temp.free;
    	end;
  inherited clear;
  end;

procedure TListCollection.clearPointersWithoutDeletingObjects;
  var
    i: integer;
    temp: TObject;
  begin
  if count > 0 then
  	for i:= 0 to count - 1 do
    	begin
    	temp := items[i];
      items[i] := nil;
    	end;
  inherited clear;
  end;

destructor TListCollection.Destroy;
  begin
  {pdf - clear will be called twice since inherited destroy also clears - but minor problem}
	self.clear;
  inherited Destroy;
  end;

procedure TListCollection.ForEach(proc: TForEachProc; data: TObject);
  var
    i: integer;
  begin
  if count > 0 then
 	 	for i := 0 to count - 1 do
    	proc(items[i], data);
  end;

function TListCollection.FirstThat(testFunc: TFirstLastFunc; data: TObject): TObject;
  var
    i: integer;
  begin
  result := nil;
  if count > 0 then
  	for i := 0 to count - 1 do
    	if testFunc(items[i], data) then
        begin
      	result := items[i];
      	break;
    	  end;
  end;

function TListCollection.LastThat(testFunc: TFirstLastFunc; data: TObject): TObject;
  var
    i: integer;
  begin
  result := nil;
  if count > 0 then
  	for i := count - 1 downto 0 do
    	if testFunc(Items[i], data) then
        begin
      	result := items[i];
      	break;
    	  end;
  end;

procedure TListCollection.streamUsingFiler(filer: GsFiler; classForCreating: GsStreamableObjectClass);
  var
  	countOnStream: longint;
  	i: longint;
    streamableObject : GsStreamableObject;
	begin
  if filer.isReading then
  	begin
    self.clear;
    filer.streamLongint(countOnStream);
    if countOnStream > 0 then
      begin
      self.setCapacity(countOnStream);
			for i := 0 to countOnStream - 1 do
    		begin
      	streamableObject := classForCreating.create;
      	streamableObject.streamUsingFiler(filer);
    		self.add(streamableObject);
      	end;
      end;
    end
  else {filer is writing or counting}
    begin
    countOnStream := count;
    filer.streamLongint(countOnStream);
    if countOnStream > 0 then
    	for i := 0 to countOnStream - 1 do
    		begin
      	streamableObject := GsStreamableObject(self.items[i]);
      	streamableObject.streamUsingFiler(filer);
      	end;
    end;
  end;

end.

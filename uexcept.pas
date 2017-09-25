unit UExcept;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uexcept: Exception-handling code for pointers, wrapping around Delphi exception-handling
system. }

interface

procedure FailNil(anObject: TObject);
procedure FailNilPtr(aPointer: Pointer);
procedure FailNilPChar(aPointer: PChar);

implementation
uses SysUtils;

procedure FailNil(anObject: TObject);
	begin
  if anObject = nil then raise Exception.create('Note enough memory to do that operation.');
  end;

procedure FailNilPtr(aPointer: Pointer);
	begin
  if aPointer = nil then raise Exception.create('Note enough memory to do that operation.');
  end;

procedure FailNilPChar(aPointer: PChar);
	begin
  if aPointer = nil then raise Exception.create('Note enough memory to do that operation.');
  end;

end.

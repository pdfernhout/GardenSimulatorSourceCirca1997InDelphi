unit Ugsform;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ugsform: GWI form superclass. This class exists solely to allow extra hint handling
in uhints and ugsim for the long and aspect hints, which are read in from tab-delimited
files at the start of program operation. Most of the forms in the project
are descended from this class (only a few small forms aren't).
See uhints for a list of what types of interface elements have hints in
our long hint system. }

interface

uses Forms, Classes;

type

GsForm = class(TForm)
  public
  constructor create(anOwner: TComponent); override;
  end;

implementation

uses uhints;

constructor GsForm.create(anOwner: TComponent);
  begin
  inherited create(anOwner);
  MakeAllFormComponentsHaveHints(self);
  end;

end.
 